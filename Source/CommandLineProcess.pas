(**

  This module contains a class for processing the command line and synchronising the
  files associated with the information.

  @Version 1.5
  @Author  David Hoyle
  @Date    22 Dec 2012

**)
Unit CommandLineProcess;

Interface

Uses
  SysUtils,
  Classes,
  Graphics,
  SyncModule;

Type
  (** This enumerate describe the available command line options switches that can be
      applied to the application. **)
  TCommandLineOption = (cloPause, cloCheckForUpdates, cloHelp, cloQuiet);
  (** This is a set of the above command line option switches. **)
  TCommandLineOptions = Set Of TCommandLineOption;

  (** An arrray of Ansi Characters - used for console input selection. **)
  TCharArray = Set Of AnsiChar;

  (** A class to process the command line information and synchronise files. **)
  TCommandLineProcessing = Class
  Strict Private
    FINIFileName         : String;
    FParams              : TStringList;
    FStd, FErr           : THandle;
    FCommandLineOptions  : TCommandLineOptions;
    FTitleColour         : TColor;
    FPathColour          : TColor;
    FSuccessColour       : TColor;
    FExistsColour        : TColor;
    FNotExistsColour     : TColor;
    FSourceDir           : String;
    FDestDir             : String;
    FFilePatterns        : String;
    FExclusions          : String;
    FLastFolder          : String;
    FSyncOptions         : TSyncOptions;
    FTotalFiles          : Int64;
    FTotalSize           : Int64;
    FOutputUpdateInterval: Integer;
    FInputColour         : TColor;
    FReadOnlyColour      : TColor;
    FExceptionColour     : TColor;
    FHeaderColour        : TColor;
  Strict Protected
    Procedure ExceptionProc(strMsg: String);
    Function GetPause: Boolean;
    Procedure LoadSettings;
    Procedure SaveSettings;
    Procedure ProcessCommandLine;
    Procedure DisplayHelp;
    Procedure SearchStartProc(strFolder: String);
    Procedure SearchProc(strFolder, strFileName: String; iCount: Integer);
    Procedure SearchEndProc(iFileCount: Integer; iTotalSize: Int64);
    Procedure CompareStartProc(strLeftFldr, strRightFldr: String);
    Procedure CompareProc(strLeftFldr, strRightFldr, strFileName: String;
      iPosition, iMaxItems: Integer);
    Procedure CompareEndProc;
    Procedure MatchListStartProc;
    Procedure MatchListProc(iPosition, iMaxItems: Integer);
    Procedure MatchListEndProc;
    Procedure ClearLine;
    Procedure DeleteStartProc(iFileCount: Integer; iTotalSize: Int64);
    Procedure DeletingProc(iFile : Integer; strFileName: String);
    Procedure DeletedProc(iFile: Integer; iSize: Int64; boolSuccess: Boolean;
      strErrMsg: String);
    Procedure DeleteQueryProc(strFileName: String; Var Option: TFileAction);
    Procedure DeleteReadOnlyQueryProc(strFileName: String; Var Option: TFileAction);
    Procedure DeleteEndProc(iDeleted, iSkipped, iErrors: Integer);
    Procedure CopyStartProc(iTotalCount: Integer; iTotalSize: Int64);
    Procedure CopyContentsProc(iCopiedSize, iTotalSize: Int64);
    Procedure CopyingProc(iFile : Integer; strSource, strDest, strFileName: String);
    Procedure CopiedProc(iCopiedFiles: Integer; iCopiedFileTotalSize,
      iCopiedTotalSize: Int64; boolSuccess: Boolean; strErrMsg: String);
    Procedure CopyQueryProc(strSourceFile, strDestFile: String; Var Option: TFileAction);
    Procedure CopyReadOnlyQueryProc(strSourceFile, strDestFile: String;
      Var Option: TFileAction);
    Procedure CopyEndProc(iCopied, iSkipped, iError: Integer);
    Function GetConsoleCharacter(Characters: TCharArray): Char;
    Procedure DeleteQuery(strMsg, strFileName: String; Var Option: TFileAction);
    Procedure CopyQuery(strMsg, strFileName: String; Var Option: TFileAction);
    Procedure DiffSizeStart(iFileCount: Integer);
    Procedure DiffSize(strLPath, strRPath, strFileName: String);
    Procedure DiffSizeEnd();
    Procedure NothingToDoStart(iFileCount: Integer);
    Procedure NothingToDo(strLPath, strRPath, strFileName: String);
    Procedure NothingToDoEnd();
    Procedure OutputStats(CFC : TCompareFoldersCollection);
  Public
    Constructor Create;
    Destructor Destroy; Override;
    Procedure Execute(iStd, iErr: THandle);
    (**
      This property returns whether the pause switch was present on the command line.
      @precon  None.
      @postcon Returns whether the pause switch was present on the command line.
      @return  a Boolean
    **)
    Property Pause: Boolean Read GetPause;
  End;

Implementation

Uses
  Windows,
  DGHLibrary,
  INIFiles,
  checkforupdates,
  ShellAPI;

(**

  This method returns true if the passed file is read only.

  @precon  None.
  @postcon Returns true if the passed file is read only.

  @param   strFileName as a String
  @return  a Boolean

**)
Function IsRO(strFileName: String): Boolean;

Begin
  IsRO := GetFileAttributes(PChar(Expand(strFileName))) And FILE_ATTRIBUTE_READONLY > 0;
End;

{ TCommandLineProcessing }

(**

  This method clears the current command line from the current cursor position to the
  end of the buffer on the same line.

  @precon  None.
  @postcon Clears the current command line from the current cursor position to the
           end of the buffer on the same line.

**)
Procedure TCommandLineProcessing.ClearLine;

Var
  ConsoleInfo: _CONSOLE_SCREEN_BUFFER_INFO;

Begin
  GetConsoleScreenBufferInfo(FStd, ConsoleInfo);
  OutputToConsole(FStd, StringOfChar(#32, ConsoleInfo.dwSize.X -
        ConsoleInfo.dwCursorPosition.X - 1), clNone, clNone, False);
End;

(**

  This method is called at the end of the comparison process and output that that part of
  the process is complete.

  @precon  None.
  @postcon Outputs that the compare process is complete.

**)
Procedure TCommandLineProcessing.CompareEndProc;

Begin
  ClearLine;
  OutputToConsoleLn(FStd, 'Done.', FSuccessColour);
End;

(**

  This method is called during the comparison process for each file.

  @precon  None.
  @postcon Outputs the percetnage completion of the comparison process.

  @param   strLeftFldr  as a String
  @param   strRightFldr as a String
  @param   strFileName  as a String
  @param   iPosition    as an Integer
  @param   iMaxItems    as an Integer

**)
Procedure TCommandLineProcessing.CompareProc(strLeftFldr, strRightFldr,
  strFileName: String; iPosition, iMaxItems: Integer);

Begin
  If Not(cloQuiet In FCommandLineOptions) Then
    Begin
      ClearLine;
      OutputToConsole(FStd, Format('%1.1n%%: %s',
          [Int(iPosition) / Int(iMaxItems) * 100.0, strFileName]), clNone, clNone, False);
    End;
End;

(**

  This method is called at the start of each pair of folders comparison process.

  @precon  None.
  @postcon Outputs that the comparison process is starting.

  @param   strLeftFldr  as a String
  @param   strRightFldr as a String

**)
Procedure TCommandLineProcessing.CompareStartProc(strLeftFldr, strRightFldr: String);

Begin
  OutputToConsole(FStd, 'Comparing: ', FHeaderColour);
End;

(**

  This method is called at the end of each file is Ccpied.

  @precon  None.
  @postcon Outputs the percentage completion of the overall copying process.

  @param   iCopiedFiles         as an Integer
  @param   iCopiedFileTotalSize as an Int64
  @param   iCopiedTotalSize     as an Int64
  @param   boolSuccess          as a Boolean
  @param   strErrMsg            as a String

**)
Procedure TCommandLineProcessing.CopiedProc(iCopiedFiles: Integer; iCopiedFileTotalSize,
  iCopiedTotalSize: Int64; boolSuccess: Boolean; strErrMsg: String);

Begin
  If boolSuccess Then
    OutputToConsoleLn(FStd, Format(' %1.1n%% Complete',
        [Int64(iCopiedTotalSize) / Int64(FTotalSize) * 100.0]), FSuccessColour)
  Else
    Begin
      OutputToConsoleLn(FStd);
      OutputToConsoleLn(FErr, #32#32#32#32 + strErrMsg, FExceptionColour);
    End;
End;

(**

  This method is called during the copying process for each file to provide a progress
  update to the amount if the file copying completed.

  @precon  None.
  @postcon Outputs the percentage completion of the individual file copying.

  @param   iCopiedSize as an Int64
  @param   iTotalSize  as an Int64

**)
Procedure TCommandLineProcessing.CopyContentsProc(iCopiedSize, iTotalSize: Int64);

Begin
  OutputToConsole(FStd, Format(' %1.1f%%', [Int(iCopiedSize) / Int(iTotalSize) * 100]),
    clNone, clNone, False);
End;

(**

  This method is called at the end of the file copying process.

  @precon  None.
  @postcon Outputs how many files were copied and how many were skipped.

  @param   iCopied  as an Integer
  @param   iSkipped as an Integer
  @param   iError   as an Integer

**)
Procedure TCommandLineProcessing.CopyEndProc(iCopied, iSkipped, iError: Integer);

Begin
  OutputToConsole(FStd, Format('  Copied %1.0n file(s) (Skipped %1.0n file(s)',
      [Int(iCopied), Int(iSkipped)]), FSuccessColour);
  If iError > 0 Then
    Begin
      OutputToConsole(FErr, ', ', FSuccessColour);
      OutputToConsole(FErr, Format('Errored %1.0n file(s)', [Int(iError)]),
        FExceptionColour);
    End;
  OutputToConsoleLn(FStd, ').', FSuccessColour);
End;

(**

  This method is called at the start of the copying of a single file.

  @precon  None.
  @postcon Outputs the file information for the file being copied.

  @param   iFile       as an Integer
  @param   strSource   as a String
  @param   strDest     as a String
  @param   strFileName as a String

**)
Procedure TCommandLineProcessing.CopyingProc(iFile : Integer; strSource, strDest,
  strFileName: String);

Var
  iColour: TColor;

Begin
  If FileExists(strDest + strFileName) Then
    iColour := FExistsColour
  Else
    iColour := FNotExistsColour;
  If Not IsRO(strSource + strFileName) Then
    OutputToConsole(FStd, #32#32 + strSource, FPathColour)
  Else
    OutputToConsole(FStd, #32#32 + strSource, FReadOnlyColour);
  OutputToConsole(FStd, ' => ');
  If Not IsRO(strSource + strFileName) Then
    Begin
      OutputToConsole(FStd, strDest, FPathColour);
      OutputToConsole(FStd, strFileName, iColour);
    End
  Else
    Begin
      OutputToConsole(FStd, strDest, FReadOnlyColour);
      OutputToConsole(FStd, strFileName, FReadOnlyColour);
    End;
End;

(**

  This method is determines via input from the keyboard whether a file should be
  overwritten.

  @precon  None.
  @postcon If the var option is not set to faAll, queries the user via the console as to
           the action to be take.

  @param   strMsg      as a String
  @param   strFileName as a String
  @param   Option      as a TFileAction as a reference

**)
Procedure TCommandLineProcessing.CopyQuery(strMsg, strFileName: String;
  Var Option: TFileAction);

Var
  C: Char;
Begin
  If Option <> faAll Then
    Begin
      OutputToConsole(FStd, strMsg, FInputColour);
      C := GetConsoleCharacter(['a', 'A', 'y', 'Y', 'n', 'N', 'c', 'C']);
      Case C Of
        'y', 'Y':
          Option := faYes;
        'n', 'N':
          Option := faNo;
        'a', 'A':
          Option := faAll;
        'c', 'C':
          Option := faCancel;
      Else
        Option := faUnknown;
      End;
      OutputToConsole(FStd, strFileOptions[Option], FInputColour);
    End;
End;

(**

  This method is called for each file that requires to be overwritten.

  @precon  None.
  @postcon Queries the user for the action to be taken

  @param   strSourceFile as a String
  @param   strDestFile   as a String
  @param   Option        as a TFileAction as a reference

**)
Procedure TCommandLineProcessing.CopyQueryProc(strSourceFile, strDestFile: String;
  Var Option: TFileAction);

Begin
  CopyQuery(' Overwrite (Y/N/A/C)? ', strDestFile, Option)
End;

(**

  This method is called for each readonly file that requires to be overwritten.

  @precon  None.
  @postcon Queries the user for the action to be taken

  @param   strSourceFile as a String
  @param   strDestFile   as a String
  @param   Option        as a TFileAction as a reference

**)
Procedure TCommandLineProcessing.CopyReadOnlyQueryProc(strSourceFile, strDestFile: String;
  Var Option: TFileAction);

Begin
  CopyQuery(' Overwrite READONLY (Y/N/A/C)? ', strDestFile, Option)
End;

(**

  This method is called at the start of the copying process.

  @precon  None.
  @postcon Outputs the number of files to be copied.

  @param   iTotalCount as an Integer
  @param   iTotalSize  as an Int64

**)
Procedure TCommandLineProcessing.CopyStartProc(iTotalCount: Integer; iTotalSize: Int64);

Begin
  FTotalFiles := iTotalCount;
  FTotalSize  := iTotalSize;
  OutputToConsoleLn(FStd, Format('Copying %1.0n files...', [Int(iTotalCount)]),
    FHeaderColour);
End;

(**

  A constructor for the TCommandLineProcessing class.

  @precon  None.
  @postcon Initialises the class memory and loads the applications settings from the INI
           file.

**)
Constructor TCommandLineProcessing.Create;

Begin
  FParams      := TStringList.Create;
  FINIFileName := BuildRootKey(FParams, ExceptionProc);
  Include(FSyncOptions, soEnabled);
  LoadSettings;
End;

(**

  This method is called at the end of the deletion of each file.

  @precon  None.
  @postcon Output the percentage completion of the deletion process.

  @param   iFile       as an Integer
  @param   iSize       as an int64
  @param   boolSuccess as a Boolean
  @param   strErrMsg   as a String

**)
Procedure TCommandLineProcessing.DeletedProc(iFile: Integer; iSize: Int64;
  boolSuccess: Boolean; strErrMsg: String);

Begin
  OutputToConsoleLn(FStd, Format(' %1.1n%% Complete',
      [Int(iFile) / Int(FTotalFiles) * 100.0]), FSuccessColour);
End;

(**

  This method is called at the end of the file deletion process.

  @precon  None.
  @postcon Outputs how many files were deleted and how many were skipped.

  @param   iDeleted as an Integer
  @param   iSkipped as an Integer
  @param   iErrors  as an Integer

**)
Procedure TCommandLineProcessing.DeleteEndProc(iDeleted, iSkipped, iErrors: Integer);

Begin
  OutputToConsole(FStd, Format('  Deleted %1.0n file(s) (Skipped %1.0n file(s)',
      [Int(iDeleted), Int(iSkipped)]), FSuccessColour);
  If iErrors > 0 Then
    Begin
      OutputToConsole(FErr, ', ', FSuccessColour);
      OutputToConsole(FErr, Format('Errored %1.0n file(s)', [Int(iErrors)]),
        FExceptionColour);
    End;
  OutputToConsoleLn(FStd, ').');
End;

(**

  This method is called at the start of the deletion of each individual file.

  @precon  None.
  @postcon Outputs the name of the file being deleted.

  @param   iFile       as an Integer
  @param   strFileName as a String

**)
Procedure TCommandLineProcessing.DeletingProc(iFile : Integer; strFileName: String);

Begin
  If Not IsRO(strFileName) Then
    OutputToConsole(FStd, #32#32 + strFileName, FExistsColour)
  ELse
    OutputToConsole(FStd, #32#32 + strFileName, FReadOnlyColour);
End;

(**

  This method queries the user via the command line as to the action to be take for the
  deletion of a file.

  @precon  None.
  @postcon Prompts the user for action is the var parameter is not set to faAll.

  @param   strMsg      as a String
  @param   strFileName as a String
  @param   Option      as a TFileAction as a reference

**)
Procedure TCommandLineProcessing.DeleteQuery(strMsg, strFileName: String;
  Var Option: TFileAction);

Var
  C: Char;

Begin
  If Option <> faAll Then
    Begin
      OutputToConsole(FStd, strMsg, FInputColour);
      C := GetConsoleCharacter(['a', 'A', 'y', 'Y', 'n', 'N', 'c', 'C']);
      Case C Of
        'y', 'Y':
          Option := faYes;
        'n', 'N':
          Option := faNo;
        'a', 'A':
          Option := faAll;
        'c', 'C':
          Option := faCancel;
      Else
        Option := faUnknown;
      End;
      OutputToConsole(FStd, strFileOptions[Option], FInputColour);
    End;
End;

(**

  This method is called for each file that requires deletion.

  @precon  None.
  @postcon Queries the user for action.

  @param   strFileName as a String
  @param   Option      as a TFileAction as a reference

**)
Procedure TCommandLineProcessing.DeleteQueryProc(strFileName: String;
  Var Option: TFileAction);

Begin
  DeleteQuery(' Delete (Y/N/A/C)? ', strFileName, Option)
End;

(**

  This method is called for each readonly file that requires deletion.

  @precon  None.
  @postcon Queries the user for action.

  @param   strFileName as a String
  @param   Option      as a TFileAction as a reference

**)
Procedure TCommandLineProcessing.DeleteReadOnlyQueryProc(strFileName: String;
  Var Option: TFileAction);

Begin
  DeleteQuery(' Delete READONLY (Y/N/A/C)? ', strFileName, Option);
End;

(**

  This is an OnDeleteStart event handler for the synchronisation process.

  @precon  None.
  @postcon Displays the number of files to be deleted.

  @param   iFileCount as an Integer
  @param   iTotalSize as an Int64

**)
Procedure TCommandLineProcessing.DeleteStartProc(iFileCount: Integer; iTotalSize: Int64);

Begin
  FTotalFiles := iFileCount;
  OutputToConsoleLn(FStd, Format('Deleting %1.0n files (%1.0n bytes)...',
      [Int(iFileCount), Int(iTotalSize)]), FHeaderColour);
End;

(**

  A destructor for the TCommandLineProcessing class.

  @precon  None.
  @postcon Frees the memory used by the application.

**)
Destructor TCommandLineProcessing.Destroy;

Begin
  SaveSettings;
  FParams.Free;
  Inherited;
End;

(**

  This method is called for each files that has a size difference only.

  @precon  None.
  @postcon Outputs the name of the files which has a size difference only.

  @param   strLPath    as a String
  @param   strRPath    as a String
  @param   strFileName as a String

**)
Procedure TCommandLineProcessing.DiffSize(strLPath, strRPath, strFileName: String);

Begin
  If Not IsRO(strLPath + strFileName) Then
    OutputToConsole(FStd, #32#32 + strLPath, FPathColour)
  Else
    OutputToConsole(FStd, #32#32 + strLPath, FReadOnlyColour);
  OutputToConsole(FStd, ' => ');
  If Not IsRO(strRPath + strFileName) Then
    Begin
      OutputToConsole(FStd, strRPath, FPathColour);
      OutputToConsoleLn(FStd, strFileName, FExistsColour);
    End
  Else
    Begin
      OutputToConsole(FStd, strRPath, FReadOnlyColour);
      OutputToConsoleLn(FStd, strFileName, FReadOnlyColour);
    End;
End;

(**

  This method is called at the end of the size different process.

  @precon  None.
  @postcon Does nothing.

**)
Procedure TCommandLineProcessing.DiffSizeEnd;

Begin
  // Do nothing
End;

(**

  This method is called at the start of the size difference processing.

  @precon  None.
  @postcon Outputs the number of files which have a siuze difference only.

  @param   iFileCount as an Integer

**)
Procedure TCommandLineProcessing.DiffSizeStart(iFileCount: Integer);

Begin
  FTotalFiles := iFileCount;
  If iFileCount > 0 Then
    OutputToConsoleLn(FStd, Format('%1.0n Files with Differing Sizes...',
        [Int(iFileCount)]), FHeaderColour);
End;

(**

  This method displays the HTML Help file for the command line options for this
  application.

  @precon  None.
  @postcon Displays the HTML Help file for the command line options for this
           application.

**)
Procedure TCommandLineProcessing.DisplayHelp;

Const
  strPage = '::/CommandLine.html';

Var
  strHelpFile: String;
  strEXEName : String;

Begin
  strHelpFile := ExtractFilePath(ParamStr(0)) + 'FldrSync.chm';
  If FileExists(strHelpFile) Then
    Begin
      OutputToConsoleLn(FStd, 'Opening HTML Help...');
      strEXEName := ExtractEXEFromExt('.chm');
      ShellExecute(0, 'Open', PChar(strEXEName), PChar('"' + strHelpFile + strPage + '"'),
        PChar(ExtractFilePath(strHelpFile)), SW_NORMAL);
    End
  Else
    OutputToConsoleLn(FErr, Format('Can not find the HTML Help file "%s".', [strHelpFile]
        ), FExceptionColour);
End;

(**

  This is an on exception handler for the BuildRootKey method.

  @precon  None.
  @postcon Outputs an exception message to the console.

  @param   strMsg as a String

**)
Procedure TCommandLineProcessing.ExceptionProc(strMsg: String);

Begin
  OutputToConsoleLn(FStd, 'Exception: ' + strMsg, clRed);
End;

(**

  This method starts the processing of the information from the command line.

  @precon  iSrd and iErr must be valid console handles for Standard and Error.
  @postcon Starts the processing of the information from the command line.

  @param   iStd as a THandle
  @param   iErr as a THandle

**)
Procedure TCommandLineProcessing.Execute(iStd, iErr: THandle);

Const
  strTitle =
    'Folder Sync %d.%d%s (Build %s) [%s] A command line tool to synchronise directories.';
  strSoftwareID = 'FldrSync';

Var
  CFC      : TCompareFoldersCollection;
  slFolders: TStringList;
  FOA      : TFolderOptionsAdapter;

Begin
  FStd := iStd;
  FErr := iErr;
  OutputToConsoleLn(FStd, GetConsoleTitle(strTitle), FTitleColour);
  OutputToConsoleLn(FStd);
  ProcessCommandLine;
  TCheckForUpdates.Execute(strSoftwareID, FINIFileName, cloCheckForUpdates
      In FCommandLineOptions);
  If cloHelp In FCommandLineOptions Then
    Begin
      DisplayHelp;
      Exit;
    End;
  OutputToConsoleLn(FStd, 'Synchronising folders:', FHeaderColour);
  If Not DirectoryExists(FSourceDir) Then
    Raise EFldrSyncException.CreateFmt('The source directory "%s" does not exists.',
      [FSourceDir]);
  OutputToConsoleLn(FStd, #32#32 + 'Source:   ' + FSourceDir);
  If Not DirectoryExists(FDestDir) Then
    Raise EFldrSyncException.CreateFmt('The destination directory "%s" does not exists.',
      [FDestDir]);
  OutputToConsoleLn(FStd, #32#32 + 'Dest:     ' + FDestDir);
  If FFilePatterns = '' Then
    FFilePatterns := '*.*';
  OutputToConsoleLn(FStd, #32#32 + 'Patterns: ' + FFilePatterns);
  OutputToConsoleLn(FStd);
  CFC := TCompareFoldersCollection.Create;
  Try
    slFolders := TStringList.Create;
    Try
      FOA.FRAWData     := 0;
      FOA.FSyncOptions := FSyncOptions;
      slFolders.AddObject(FSourceDir + FFilePatterns + '=' + FDestDir + FFilePatterns,
        FOA.FObjData);
      CFC.OnSearchStart         := SearchStartProc;
      CFC.OnSearch              := SearchProc;
      CFC.OnSearchEnd           := SearchEndProc;
      CFC.OnCompareStart        := CompareStartProc;
      CFC.OnCompare             := CompareProc;
      CFC.OnCompareEnd          := CompareEndProc;
      CFC.OnMatchListStart      := MatchListStartProc;
      CFC.OnMatchList           := MatchListProc;
      CFC.OnMatchListEnd        := MatchListEndProc;
      CFC.OnDeleteStart         := DeleteStartProc;
      CFC.OnDeleting            := DeletingProc;
      CFC.OnDeleted             := DeletedProc;
      CFC.OnDeleteQuery         := DeleteQueryProc;
      CFC.OnDeleteReadOnlyQuery := DeleteReadOnlyQueryProc;
      CFC.OnDeleteEnd           := DeleteEndProc;
      CFC.OnCopyStart           := CopyStartProc;
      CFC.OnCopying             := CopyingProc;
      CFC.OnCopyContents        := CopyContentsProc;
      CFC.OnCopied              := CopiedProc;
      CFC.OnCopyQuery           := CopyQueryProc;
      CFC.OnCopyReadOnlyQuery   := CopyReadOnlyQueryProc;
      CFC.OnCopyEnd             := CopyEndProc;
      CFC.OnDiffSizeStart       := DiffSizeStart;
      CFC.OnDiffSize            := DiffSize;
      CFC.OnDiffSizeEnd         := DiffSizeEnd;
      CFC.OnNothingToDoStart    := NothingToDoStart;
      CFC.OnNothingToDo         := NothingToDo;
      CFC.OnNothingToDoEnd      := NothingToDoEnd;
      CFC.ProcessFolders(slFolders, FExclusions);
      OutputStats(CFC);
      Try
        CFC.ProcessFiles;
      Except
        On E : EAbort Do
          {Do nothing};
      End;
    Finally
      slFolders.Free;
    End;
  Finally
    CFC.Free;
  End;
End;

(**

  This method queries the console input buffer for key presses and returns the character
  pressed if its in a predefined list.

  @precon  Characters is an array of chars that are valid inputs.
  @postcon Queries the console input buffer for key presses and returns the character
           pressed if its in a predefined list.

  @param   Characters as a TCharArray
  @return  a Char

**)
Function TCommandLineProcessing.GetConsoleCharacter(Characters: TCharArray): Char;

Var
  Buffer  : TInputRecord;
  iCount  : Cardinal;
  hndInput: THandle;

Begin
  hndInput := GetStdHandle(STD_INPUT_HANDLE);
  Repeat
    Result := #0;
    If ReadConsoleInput(hndInput, Buffer, 1, iCount) Then
      If iCount > 0 Then
        If Buffer.EventType = KEY_EVENT Then
          If Buffer.Event.KeyEvent.bKeyDown Then
            Result := Buffer.Event.KeyEvent.UnicodeChar;
    Sleep(50);
  Until CharInSet(Result, Characters);
End;

(**

  This is a getter method for the Pause property.

  @precon  None
  @postcon Returns whether the program should Pause after completion.

  @return  a Boolean

**)
Function TCommandLineProcessing.GetPause: Boolean;

Begin
  Result := cloPause In FCommandLineOptions;
End;

(**

  This method loads the applications settings from its INI file.

  @precon  None.
  @postcon The applications settings are loaded from its INI file.

**)
Procedure TCommandLineProcessing.LoadSettings;

Begin
  With TMemINIFile.Create(FINIFileName) Do
    Try
      FTitleColour     := StringToColor(ReadString('Colours', 'Title', 'clWhite'));
      FPathColour      := StringToColor(ReadString('Colours', 'Path', 'clYellow'));
      FSuccessColour   := StringToColor(ReadString('Colours', 'Success', 'clLime'));
      FExistsColour    := StringToColor(ReadString('Colours', 'Exists', 'clWhite'));
      FNotExistsColour := StringToColor(ReadString('Colours', 'NotExists', 'clGray'));
      FInputColour     := StringToColor(ReadString('Colours', 'Input', 'clFuchsia'));
      FReadOnlyColour  := StringToColor(ReadString('Colours', 'ReadOnly', 'clMaroon'));
      FExceptionColour := StringToColor(ReadString('Colours', 'Exception', 'clRed'));
      FHeaderColour    := StringToColor(ReadString('Colours', 'Header', 'clWhite'));
      FOutputUpdateInterval := ReadInteger('Setup', 'OutputUpdateInterval', 10);
    Finally
      Free;
    End;
End;

(**

  This method is call at the end of the matching process.

  @precon  None.
  @postcon Outputs that the matching process has completed.

**)
Procedure TCommandLineProcessing.MatchListEndProc;

Begin
  ClearLine;
  OutputToConsoleLn(FStd, 'Done.', FSuccessColour);
End;

(**

  This method is called for each files that is matched.

  @precon  None.
  @postcon Outputs the progress through the matching progress.

  @param   iPosition as an Integer
  @param   iMaxItems as an Integer

**)
Procedure TCommandLineProcessing.MatchListProc(iPosition, iMaxItems: Integer);

Begin
  If Not(cloQuiet In FCommandLineOptions) Then
    Begin
      OutputToConsole(FStd, Format('Processing Item %1.0n of %1.0n (%1.0n%%)...',
          [Int(iPosition), Int(iMaxItems), Int(iPosition) / Int(iMaxItems) * 100.0]),
        clNone, clNone, False);
    End;
End;

(**

  This method is called at the start of the matching process.

  @precon  None.
  @postcon Outputs that the matching process is starting.

**)
Procedure TCommandLineProcessing.MatchListStartProc;

Begin
  OutputToConsole(FStd, 'Match Lists: ', FHeaderColour);
End;

(**

  This method is called for each file that is marked as Nothing To Do.

  @precon  None.
  @postcon Outputs the name and path(s) of the file.

  @param   strLPath    as a String
  @param   strRPath    as a String
  @param   strFileName as a String

**)
Procedure TCommandLineProcessing.NothingToDo(strLPath, strRPath, strFileName: String);

Begin
  If Not IsRO(strLPath + strFileName) Then
    OutputToConsole(FStd, #32#32 + strLPath, FPathColour)
  Else
    OutputToConsole(FStd, #32#32 + strLPath, FReadOnlyColour);
  OutputToConsole(FStd, ' => ');
  If Not IsRO(strRPath + strFileName) Then
    Begin
      OutputToConsole(FStd, strRPath, FPathColour);
      OutputToConsoleLn(FStd, strFileName, FExistsColour);
    End
  Else
    Begin
      OutputToConsole(FStd, strRPath, FReadOnlyColour);
      OutputToConsoleLn(FStd, strFileName, FReadOnlyColour);
    End;
End;

(**

  This method is called at the end of the nothing to do process.

  @precon  None.
  @postcon Does not do anything.

**)
Procedure TCommandLineProcessing.NothingToDoEnd;

Begin
  // Do nothing
End;

(**

  This method is called at the start of the nothing to do process.

  @precon  None.
  @postcon Outputs the number of files marked as nothing to do.

  @param   iFileCount as an Integer

**)
Procedure TCommandLineProcessing.NothingToDoStart(iFileCount: Integer);

Begin
  FTotalFiles := iFileCount;
  If iFileCount > 0 Then
    OutputToConsoleLn(FStd, Format('%1.0n Files with Nothing to do...', [Int(iFileCount)]
        ), FHeaderColour);
End;

(**

  This method outputs the statistic for the synchronisation job.

  @precon  CFC must be a valid instance.
  @postcon Outputs the statistics to the console.

  @param   CFC as a TCompareFoldersCollection

**)
Procedure TCommandLineProcessing.OutputStats(CFC : TCompareFoldersCollection);

Var
  i : Integer;
  iPos : Integer;

Begin
  OutputToConsoleLn(FStd, 'Statistics:', FHeaderColour);
  For i := 0 To CFC.Statistics.Count - 1 Do
    Begin
      iPos := Pos(':', CFC.Statistics[i]);
      OutputToConsoleLn(FStd, Format('  %-20s: %40s', [
        Copy(CFC.Statistics[i], 1, iPos - 1),
        Copy(CFC.Statistics[i], iPos + 1, Length(CFC.Statistics[i]) - iPos)
      ]));
    End;
End;

(**

  This method processes the command line parameters of the console application and sets up
  the applications internal variables based on those settings.

  @precon  None.
  @postcon Processes the command line parameters of the console application and sets up
           the applications internal variables based on those settings.

**)
Procedure TCommandLineProcessing.ProcessCommandLine;

Var
  strOption: String;
  i        : Integer;

Begin
  FCommandLineOptions := [];
  For i := 0 To FParams.Count - 1 Do
    Begin
      strOption := FParams[i];
      If CharInSet(strOption[1], ['-', '/']) Then
        Begin
          strOption := Copy(strOption, 2, Length(strOption) - 1);
          If CompareText(strOption, 'Quiet') = 0 Then
            Include(FCommandLineOptions, cloQuiet)
          Else If strOption = '!' Then
            Include(FCommandLineOptions, cloPause)
          Else If strOption = '?' Then
            Include(FCommandLineOptions, cloHelp)
          Else If CompareText(strOption, 'Updates') = 0 Then
            Include(FCommandLineOptions, cloCheckForUpdates)
          Else If CompareText(strOption, 'PrimaryLeft') = 0 Then
            Include(FSyncOptions, soPrimaryLeft)
          Else If CompareText(strOption, 'PrimaryRight') = 0 Then
            Include(FSyncOptions, soPrimaryRight)
          Else If CompareText(Copy(strOption, 1, 1), 'E') = 0 Then
            FExclusions := Copy(strOption, 2, Length(strOption) - 1)
          Else If CompareText(strOption, 'OverwriteReadOnly') = 0 Then
            Include(FSyncOptions, soOverwriteReadOnlyFiles)
          Else If CompareText(strOption, 'ConfirmNo') = 0 Then
            Begin
              Include(FSyncOptions, soConfirmNo);
              Exclude(FSyncOptions, soConfirmYes);
            End
          Else If CompareText(strOption, 'ConfirmYes') = 0 Then
            Begin
              Include(FSyncOptions, soConfirmYes);
              Exclude(FSyncOptions, soConfirmNo);
            End
          Else If CompareText(strOption, 'NoRecursion') = 0 Then
            Include(FSyncOptions, soNoRecursion)
          Else
            Raise EFldrSyncException.CreateFmt('Invalid command line option "%s".',
              [FParams[i]]);
        End
      Else
        Begin
          If FSourceDir = '' Then
            Begin
              FFilePatterns := ExtractFileName(FParams[i]);
              FSourceDir    := ExtractFilePath(FParams[i]);
            End
          Else
            FDestDir := ExtractFilePath(FParams[i]);
        End;
    End;
End;

(**

  This method saves the applications settings to the INI file.

  @precon  None.
  @postcon The applications settings are saved to the INI file.

**)
Procedure TCommandLineProcessing.SaveSettings;

Begin
  With TMemINIFile.Create(FINIFileName) Do
    Try
      WriteString('Colours', 'Title', ColorToString(FTitleColour));
      WriteString('Colours', 'Path', ColorToString(FPathColour));
      WriteString('Colours', 'Success', ColorToString(FSuccessColour));
      WriteString('Colours', 'Exists', ColorToString(FExistsColour));
      WriteString('Colours', 'NotExists', ColorToString(FNotExistsColour));
      WriteString('Colours', 'Input', ColorToString(FInputColour));
      WriteString('Colours', 'ReadOnly', ColorToString(FReadOnlyColour));
      WriteString('Colours', 'Exception', ColorToString(FExceptionColour));
      WriteString('Colours', 'Header', ColorToString(FHeaderColour));
      WriteInteger('Setup', 'OutputUpdateInterval', FOutputUpdateInterval);
    Finally
      Free;
    End;
End;

(**

  This method is called for each file found in the search folder.

  @precon  None.
  @postcon Output the number of files and their size found in the search.

  @param   iFileCount as an Integer
  @param   iTotalSize as an int64

**)
Procedure TCommandLineProcessing.SearchEndProc(iFileCount: Integer; iTotalSize: Int64);

Begin
  ClearLine;
  OutputToConsoleLn(FStd, Format('%1.0n files in %1.0n bytes.',
      [Int(iFileCount), Int(iTotalSize)]), FSuccessColour);
End;

(**

  This methid is called for each file found in the search.

  @precon  None.
  @postcon Outputs periodically the number and name of the file found.

  @param   strFolder   as a String
  @param   strFileName as a String
  @param   iCount      as an Integer

**)
Procedure TCommandLineProcessing.SearchProc(strFolder, strFileName: String;
  iCount: Integer);

Begin
  If strFolder = FLastFolder Then
    Begin
      If Not(cloQuiet In FCommandLineOptions) Then
        If iCount Mod FOutputUpdateInterval = 0 Then
          Begin
            ClearLine;
            OutputToConsole(FStd, Format('%1.0n\%s', [Int(iCount), strFileName]), clNone,
              clNone, False);
          End;
    End
  Else
    FLastFolder := strFolder;
End;

(**

  This method is called at the start of the file searching process.

  @precon  None.
  @postcon Outputs the folder that is being searched.

  @param   strFolder as a String

**)
Procedure TCommandLineProcessing.SearchStartProc(strFolder: String);

Begin
  OutputToConsole(FStd, 'Searching: ', FHeaderColour);
  OutputToConsole(FStd, Format('%s', [strFolder]), FPathColour);
End;

End.
