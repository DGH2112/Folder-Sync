(**

  This module contains a class for processing the command line and synchronising the
  files associated with the information.

  @Version 2.0
  @Author  David Hoyle
  @Date    21 Nov 2013

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
    FMaxFileSize         : Int64;
  Strict Protected
    Procedure ExceptionProc(strMsg: String);
    Function GetPause: Boolean;
    Procedure LoadSettings;
    Procedure SaveSettings;
    Procedure ProcessCommandLine;
    Procedure DisplayHelp;
    Procedure SearchStartProc(strFolder: String);
    Procedure SearchProc(strFolder, strFileName: String; iCount: Integer;
      Update : TUpdateType);
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
    Procedure DeletedProc(iFile: Integer; iSize: Int64; iSuccess: TProcessSuccess);
    Procedure DeleteQueryProc(strFilePath: String; DeleteFile : TFileRecord;
      Var Option: TFileAction);
    Procedure DeleteReadOnlyQueryProc(strFilePath: String; DeleteFile : TFileRecord;
      Var Option: TFileAction);
    Procedure DeleteEndProc(iDeleted, iSkipped, iErrors: Integer);
    Procedure CopyStartProc(iTotalCount: Integer; iTotalSize: Int64);
    Procedure CopyContentsProc(iCopiedSize, iTotalSize: Int64);
    Procedure CopyingProc(iFile : Integer; strSource, strDest, strFileName: String);
    Procedure CopiedProc(iCopiedFiles: Integer; iCopiedFileTotalSize,
      iCopiedTotalSize: Int64; iSuccess: TProcessSuccess);
    Procedure CopyQueryProc(strSourcePath, strDestPath: String; SourceFile,
      DestFile : TFileRecord; Var Option: TFileAction);
    Procedure CopyReadOnlyQueryProc(strSourcePath, strDestPath: String; SourceFile,
      DestFile : TFileRecord; Var Option: TFileAction);
    Procedure CopyEndProc(iCopied, iSkipped, iError: Integer);
    Function GetConsoleCharacter(Characters: TCharArray): Char;
    Procedure FileQuery(strMsg, strFilePath: String; DeleteFile : TFileRecord;
      Var Option: TFileAction; boolReadOnly : Boolean);
    Procedure DiffSizeStart(iFileCount: Integer);
    Procedure DiffSize(strLPath, strRPath, strFileName: String);
    Procedure DiffSizeEnd();
    Procedure NothingToDoStart(iFileCount: Integer);
    Procedure NothingToDo(strLPath, strRPath, strFileName: String);
    Procedure NothingToDoEnd();
    Procedure OutputStats(CFC : TCompareFoldersCollection);
    Procedure ParseSizeLimit(strOption : String);
    Procedure ExceedsSizeLimitStart(iFileCount: Integer);
    Procedure ExceedsSizeLimit(strLPath, strRPath, strFileName: String);
    Procedure ExceedsSizeLimitEnd();
    Procedure ErrorMsgsStart(iErrorCount: Integer);
    Procedure ErrorMsgs(strErrorMsg : String);
    Procedure ErrorMsgsEnd();
    Procedure DeleteFoldersEnd();
    Procedure DeleteFoldersStart(iFolderCount: Integer);
    Procedure DeleteFolders(iFolder, iFolders : Integer; strFolder: String);
    Procedure CopyError(strSource, strDest, strErrorMsg : String;
      iLastError : Cardinal; var iResult : TDGHErrorResult);
    Procedure DeleteError(strSource, strErrorMsg : String;
      iLastError : Cardinal; var iResult : TDGHErrorResult);
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
    If iPosition Mod 100 = 0 Then
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
  @param   iSuccess             as a TProcessSuccess

**)
Procedure TCommandLineProcessing.CopiedProc(iCopiedFiles: Integer; iCopiedFileTotalSize,
  iCopiedTotalSize: Int64; iSuccess: TProcessSuccess);

Begin
  Case iSuccess Of
    psSuccessed: OutputToConsoleLn(FStd, Format(' %1.1n%% Complete',
        [Int64(iCopiedTotalSize) / Int64(FTotalSize) * 100.0]), FSuccessColour);
    psFailed: OutputToConsoleLn(FStd, ' Error Copying file (error type ignored).',
      FExceptionColour);
    //psIgnored: OutputToConsoleLn(FStd, ' Ignored Copying file.', FExceptionColour);
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

  This method displays an error message on the screen and ask the user to ignore or stop 
  the processing.

  @precon  None.
  @postcon The iResult value is change depending upon the users answer to the question.

  @param   strSource   as a String
  @param   strDest     as a String
  @param   strErrorMsg as a String
  @param   iLastError  as a Cardinal
  @param   iResult     as a TDGHErrorResult as a reference

**)
Procedure TCommandLineProcessing.CopyError(strSource, strDest, strErrorMsg: String;
  iLastError : Cardinal; Var iResult: TDGHErrorResult);

Var
  Ch: Char;

Begin
  OutputToConsoleLn(FStd);
  OutputToConsoleLn(FStd, '    An error has occurred during the copying of files:',
    FExceptionColour);
  OutputToConsoleLn(FStd, Format('      Source     : %s', [strSource]));
  OutputToConsoleLn(FStd, Format('      Destination: %s', [strDest]));
  OutputToConsoleLn(FStd, Format('      OS Error   : (%d) %s', [iLastError, strErrorMsg]),
    FExceptionColour);
  OutputToConsole(FStd, '    Do you want to [I]gnore once, Ignore [A]ll the errors or [S]top processing? ',
    FInputColour);
  Ch := GetConsoleCharacter(['i', 'I', 'a', 'A', 'S', 's']);
  Case Ch Of
    'i', 'I': iResult := derIgnoreOnce;
    'a', 'A': iResult := derIgnoreAll;
    's', 'S': iResult := derStop;
  End;
  OutputToConsoleLn(FStd, Ch, FInputColour);
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
  OutputToConsole(FStd, #32#32 + strSource, FPathColour);
  OutputToConsole(FStd, ' => ');
  OutputToConsole(FStd, strDest, FPathColour);
  If FileExists(strDest + strFileName) Then
    iColour := FExistsColour
  Else
    iColour := FNotExistsColour;
  If Not IsRO(strSource + strFileName) Then
    OutputToConsole(FStd, strFileName, iColour)
  Else
    OutputToConsole(FStd, strFileName, FReadOnlyColour);
End;

(**

  This method is called for each file that requires to be overwritten.

  @precon  None.
  @postcon Queries the user for the action to be taken

  @param   strSourcePath as a String
  @param   strDestPath   as a String
  @param   SourceFile    as a TFileRecord
  @param   DestFile      as a TFileRecord
  @param   Option        as a TFileAction as a reference

**)
Procedure TCommandLineProcessing.CopyQueryProc(strSourcePath, strDestPath: String;
  SourceFile, DestFile : TFileRecord; Var Option: TFileAction);

Begin
  FileQuery(' Overwrite (Y/N/A/O/C)? ', strDestPath + DestFile.FileName, SourceFile, Option,
    False)
End;

(**

  This method is called for each readonly file that requires to be overwritten.

  @precon  None.
  @postcon Queries the user for the action to be taken

  @param   strSourcePath as a String
  @param   strDestPath   as a String
  @param   SourceFile    as a TFileRecord
  @param   DestFile      as a TFileRecord
  @param   Option        as a TFileAction as a reference

**)
Procedure TCommandLineProcessing.CopyReadOnlyQueryProc(strSourcePath, strDestPath: String;
  SourceFile, DestFile : TFileRecord; Var Option: TFileAction);

Begin
  FileQuery(' Overwrite READONLY (Y/N/A/O/C)? ', strDestPath + DestFile.FileName,
    SourceFile, Option, True)
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
  FMaxFileSize := 0;
  LoadSettings;
End;

(**

  This method is called at the end of the deletion of each file.

  @precon  None.
  @postcon Output the percentage completion of the deletion process.

  @param   iFile    as an Integer
  @param   iSize    as an Int64
  @param   iSuccess as a TProcessSuccess

**)
Procedure TCommandLineProcessing.DeletedProc(iFile: Integer; iSize: Int64;
  iSuccess: TProcessSuccess);

Begin
  Case iSuccess Of
    psSuccessed: OutputToConsoleLn(FStd, Format(' %1.1n%% Complete',
      [Int(iFile) / Int(FTotalFiles) * 100.0]), FSuccessColour);
    psFailed: OutputToConsoleLn(FStd, ' Error Deleting file (error type ignored).',
      FExceptionColour);
    //psIgnored: OutputToConsoleLn(FStd, ' Ignored Copying file.', FExceptionColour);
  End;
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

  This method is displayed upon a deletion error and prompts the user as to whether they 
  want to ignore the error or stop processing.

  @precon  None.
  @postcon The iResult value is changed depending upon the users answer.

  @param   strSource   as a String
  @param   strErrorMsg as a String
  @param   iLastError  as a Cardinal
  @param   iResult     as a TDGHErrorResult as a reference

**)
Procedure TCommandLineProcessing.DeleteError(strSource, strErrorMsg: String;
  iLastError : Cardinal; Var iResult: TDGHErrorResult);

Var
  Ch : Char;
  
Begin
  OutputToConsoleLn(FStd);
  OutputToConsoleLn(FStd, '    An error has occurred during the deletion of files:',
    FExceptionColour);
  OutputToConsoleLn(FStd, Format('      Source     : %s', [strSource]));
  OutputToConsoleLn(FStd, Format('      OS Error   : (%d) %s', [iLastError, strErrorMsg]),
    FExceptionColour);
  OutputToConsole(FStd, '    Do you want to [I]gnore Once, Ignore [A]ll the errors or [S]top processing? ',
    FInputColour);
  Ch := GetConsoleCharacter(['i', 'I', 'a', 'A', 'S', 's']);
  Case Ch Of
    'i', 'I': iResult := derIgnoreOnce;
    'a', 'A': iResult := derIgnoreAll;
    's', 'S': iResult := derStop;
  End;
  OutputToConsoleLn(FStd, Ch, FInputColour);
End;

(**

  This is an on delete folders event handler.

  @precon  None.
  @postcon Outputs the folder being deleted.

  @param   iFolder   as an Integer
  @param   iFolders  as an Integer
  @param   strFolder as a String

**)
Procedure TCommandLineProcessing.DeleteFolders(iFolder, iFolders: Integer;
  strFolder: String);

Begin
  OutputToConsoleLn(FStd, #32#32 + strFolder);
End;

(**

  This is an on Delete Folders End event handler.

  @precon  None.
  @postcon Do nothing.

**)
Procedure TCommandLineProcessing.DeleteFoldersEnd;

Begin
End;

(**

  This is an on delete folder start event handler.

  @precon  None.
  @postcon Outputs a header in the console for the list of deleted files.

  @param   iFolderCount as an Integer

**)
Procedure TCommandLineProcessing.DeleteFoldersStart(iFolderCount: Integer);

Begin
  FTotalFiles := iFolderCount;
  If iFolderCount > 0 Then
    OutputToConsoleLn(FStd, 'Deleting empty folders...', FHeaderColour);
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

  This method is called for each file that requires deletion.

  @precon  None.
  @postcon Queries the user for action.

  @param   strFilePath as a String
  @param   DeleteFile  as a TFileRecord
  @param   Option      as a TFileAction as a reference

**)
Procedure TCommandLineProcessing.DeleteQueryProc(strFilePath: String;
  DeleteFile : TFileRecord; Var Option: TFileAction);

Begin
  FileQuery(' Delete (Y/N/A/O/C)? ', strFilePath, DeleteFile, Option, False);
End;

(**

  This method is called for each readonly file that requires deletion.

  @precon  None.
  @postcon Queries the user for action.

  @param   strFilePath as a String
  @param   DeleteFile  as a TFileRecord
  @param   Option      as a TFileAction as a reference

**)
Procedure TCommandLineProcessing.DeleteReadOnlyQueryProc(strFilePath: String;
  DeleteFile : TFileRecord; Var Option: TFileAction);

Begin
  FileQuery(' Delete READONLY (Y/N/A/O/C)? ', strFilePath, DeleteFile, Option, True);
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
  OutputToConsole(FStd, #32#32 + strLPath, FPathColour);
  OutputToConsole(FStd, ' => ');
  OutputToConsole(FStd, strRPath, FPathColour);
  If Not IsRO(strRPath + strFileName) Then
    OutputToConsoleLn(FStd, strFileName, FExistsColour)
  Else
    OutputToConsoleLn(FStd, strFileName, FReadOnlyColour);
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

  This is an on error message event handler for outputting errors messages from the delete
  and copying process.

  @precon  None.
  @postcon Outputs an error message to the log file.

  @param   strErrorMsg as a String

**)
Procedure TCommandLineProcessing.ErrorMsgs(strErrorMsg: String);

Begin
  OutputToConsoleLn(FStd, #32#32 + strErrorMsg, FExceptionColour);
End;

(**

  This is an on error message end event handler for outputting the error messages.

  @precon  None.
  @postcon Does nothing.

**)
Procedure TCommandLineProcessing.ErrorMsgsEnd;

Begin
End;

(**

  This is an on error message start event handler for the outputting of error messaeges
  from the deleting and copying process.

  @precon  None.
  @postcon Outputs a header for error messages to the log file if there are any errors
           reported.

  @param   iErrorCount as an Integer

**)
Procedure TCommandLineProcessing.ErrorMsgsStart(iErrorCount: Integer);

Begin
  FTotalFiles := iErrorCount;
  If iErrorCount > 0 Then
    OutputToConsoleLn(FStd, Format('%1.0n Files had errors...', [Int(iErrorCount)]
        ), FHeaderColour);
End;

(**

  This is an on Exceeds Size Limit event handler.

  @precon  None.
  @postcon Outputs a line of information for each file considered as too large for
           copying.

  @param   strLPath    as a String
  @param   strRPath    as a String
  @param   strFileName as a String

**)
Procedure TCommandLineProcessing.ExceedsSizeLimit(strLPath, strRPath,
  strFileName: String);

Begin
  OutputToConsole(FStd, #32#32 + strLPath, FPathColour);
  OutputToConsole(FStd, ' => ');
  OutputToConsole(FStd, strRPath, FPathColour);
  If Not IsRO(strRPath + strFileName) Then
    OutputToConsoleLn(FStd, strFileName, FExistsColour)
  Else
    OutputToConsoleLn(FStd, strFileName, FReadOnlyColour);
End;

(**

  This is an on Exceeds Size Limirt End Event handler.

  @precon  None.
  @postcon Does nothing.

**)
Procedure TCommandLineProcessing.ExceedsSizeLimitEnd;

Begin
  // Do Nothing
End;

(**

  This is an on exceeds size limit start event handler.

  @precon  None.
  @postcon Outputs a headers to the list of files that are considered too large for
           copying.

  @param   iFileCount as an Integer

**)
Procedure TCommandLineProcessing.ExceedsSizeLimitStart(iFileCount: Integer);

Begin
  FTotalFiles := iFileCount;
  If iFileCount > 0 Then
    OutputToConsoleLn(FStd, Format('%1.0n Files exceeding Size Limit...', [Int(iFileCount)]
        ), FHeaderColour);
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
  strMsg = 'Drive "%s" is running low on disk space. Do you want to continue (Y/N)? ';

Var
  CFC      : TCompareFoldersCollection;
  Folders  : TFolders;
  iDrive : Integer;
  D : TDriveTotal;
  C : Char;

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
    If Not ForceDirectories(FSourceDir) Then
      Raise EFldrSyncException.CreateFmt('Could not create the directory "%s".',
        [FSourceDir]);
  OutputToConsoleLn(FStd, #32#32 + 'Source:   ' + FSourceDir);
  If Not DirectoryExists(FDestDir) Then
    If Not ForceDirectories(FDestDir) Then
      Raise EFldrSyncException.CreateFmt('Could not create the directory "%s".',
        [FDestDir]);
  OutputToConsoleLn(FStd, #32#32 + 'Dest:     ' + FDestDir);
  If FFilePatterns = '' Then
    FFilePatterns := '*.*';
  OutputToConsoleLn(FStd, #32#32 + 'Patterns: ' + FFilePatterns);
  OutputToConsoleLn(FStd);
  CFC := TCompareFoldersCollection.Create;
  Try
    Folders := TFolders.Create;
    Try
      Folders.Add(TFolder.Create(FSourceDir, FDestDir, FFilePatterns, FSyncOptions,
        FMaxFileSize));
      CFC.OnSearchStart           := SearchStartProc;
      CFC.OnSearch                := SearchProc;
      CFC.OnSearchEnd             := SearchEndProc;
      CFC.OnCompareStart          := CompareStartProc;
      CFC.OnCompare               := CompareProc;
      CFC.OnCompareEnd            := CompareEndProc;
      CFC.OnMatchListStart        := MatchListStartProc;
      CFC.OnMatchList             := MatchListProc;
      CFC.OnMatchListEnd          := MatchListEndProc;
      CFC.OnDeleteStart           := DeleteStartProc;
      CFC.OnDeleting              := DeletingProc;
      CFC.OnDeleted               := DeletedProc;
      CFC.OnDeleteQuery           := DeleteQueryProc;
      CFC.OnDeleteReadOnlyQuery   := DeleteReadOnlyQueryProc;
      CFC.OnDeleteEnd             := DeleteEndProc;
      CFC.OnCopyStart             := CopyStartProc;
      CFC.OnCopying               := CopyingProc;
      CFC.OnCopyContents          := CopyContentsProc;
      CFC.OnCopied                := CopiedProc;
      CFC.OnCopyQuery             := CopyQueryProc;
      CFC.OnCopyReadOnlyQuery     := CopyReadOnlyQueryProc;
      CFC.OnCopyEnd               := CopyEndProc;
      CFC.OnDiffSizeStart         := DiffSizeStart;
      CFC.OnDiffSize              := DiffSize;
      CFC.OnDiffSizeEnd           := DiffSizeEnd;
      CFC.OnNothingToDoStart      := NothingToDoStart;
      CFC.OnNothingToDo           := NothingToDo;
      CFC.OnNothingToDoEnd        := NothingToDoEnd;
      CFC.OnExceedsSizeLimitStart := ExceedsSizeLimitStart;
      CFC.OnExceedsSizeLimit      := ExceedsSizeLimit;
      CFC.OnExceedsSizeLimitEnd   := ExceedsSizeLimitEnd;
      CFC.OnErrorMsgsStart        := ErrorMsgsStart;
      CFC.OnErrorMsgs             := ErrorMsgs;
      CFC.OnErrorMsgsEnd          := ErrorMsgsEnd;
      CFC.OnDeleteFoldersStart    := DeleteFoldersStart;
      CFC.OnDeleteFolders         := DeleteFolders;
      CFC.OnDeleteFoldersEnd      := DeleteFoldersEnd;
      CFC.OnCopyError             := CopyError;
      CFC.OnDeleteError           := DeleteError;
      CFC.ProcessFolders(Folders, FExclusions);
      OutputStats(CFC);
      Try
        For iDrive := 0 To CFC.Drives.Count - 1 Do
          Begin
            D := CFC.Drives.Drive[iDrive];
            If D.FreeAtFinish / D.Total < dblDriveSpaceCheckPercentage Then
              Begin
                OutputToConsole(FStd, Format(strMsg, [D.Drive]), FInputColour);
                C := GetConsoleCharacter(['y', 'Y', 'n', 'N']);
                OutputToConsoleLn(FStd, C, FInputColour);
                Case C Of
                  'n', 'N': Exit;
                End;
              End;
          End;
        CFC.ProcessFiles;
      Except
        On E : EAbort Do {Do nothing};
        On E : Exception Do
          Begin
            OutputToConsoleLn(FStd);
            OutputToConsoleLn(FStd, E.Message, FExceptionColour);
          End;
      End;
    Finally
      Folders.Free;
    End;
  Finally
    CFC.Free;
  End;
End;

(**

  This method queries the user via the command line as to the action to be take for the 
  deletion of a file.

  @precon  None.
  @postcon Prompts the user for action is the var parameter is not set to faAll.

  @param   strMsg       as a String
  @param   strFilePath  as a String
  @param   DeleteFile   as a TFileRecord
  @param   Option       as a TFileAction as a reference
  @param   boolReadOnly as a Boolean

**)
Procedure TCommandLineProcessing.FileQuery(strMsg, strFilePath: String;
  DeleteFile : TFileRecord; Var Option: TFileAction; boolReadOnly : Boolean);

Var
  C: Char;

Begin
  If (Not boolReadOnly And Not (Option In [faYesToAll, faNoToAll])) Or
     (    boolReadOnly And Not (Option In [faYesToAllRO, faNoToAllRO])) Then
    Begin
      OutputToConsole(FStd, strMsg, FInputColour);
      C := GetConsoleCharacter(['a', 'A', 'y', 'Y', 'n', 'N', 'o', 'O', 'c', 'C']);
      Case C Of
        'y', 'Y':
          Option := faYes;
        'n', 'N':
          Option := faNo;
        'a', 'A':
          If Not boolReadOnly Then
            Option := faYesToAll
          Else
            Option := faYesToAllRO;
        'o', 'O':
          If Not boolReadOnly Then
            Option := faNoToAll
          Else
            Option := faNoToAllRO;
        'c', 'C':
          Option := faCancel;
      Else
        Option := faUnknown;
      End;
      OutputToConsole(FStd, strFileOptions[Option], FInputColour);
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
      If iPosition Mod 100 = 0 Then
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
  OutputToConsole(FStd, #32#32 + strLPath, FPathColour);
  OutputToConsole(FStd, ' => ');
  OutputToConsole(FStd, strRPath, FPathColour);
  If Not IsRO(strRPath + strFileName) Then
    OutputToConsoleLn(FStd, strFileName, FExistsColour)
  Else
    OutputToConsoleLn(FStd, strFileName, FReadOnlyColour);
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
  i : TFileOpStat;
  iDrive : Integer;
  strHeader : String;

Begin
  OutputToConsoleLn(FStd, 'Statistics:', FHeaderColour);
  For i := Low(TFileOpStat) To High(TFileOpStat) Do
    Begin
      OutputToConsoleLn(FStd, Format('  %-20s: %1.0n files in %1.1n kbytes', [
        CFC.Statistics[i].FName,
        Int(CFC.Statistics[i].FCount),
        Int(CFC.Statistics[i].FSize / 1024.0)
      ]));
    End;
  OutputToConsoleLn(FStd, 'Drive Space (in kbytes):',
    FHeaderColour);
  strHeader := Format(' %-25s | %16s | %16s | %16s | %16s | %16s ', [
    'Drive',
    'Total',
    'Free at Start',
    'Total Deletes',
    'Total Adds',
    'Free at Finish'
  ]);
  OutputToConsoleLn(FStd, strHeader);
  OutputToConsoleLn(FStd, StringOfChar('-', Length(strHeader)));
  For iDrive := 0 To CFC.Drives.Count -  1 Do
    Begin
      OutputToConsoleLn(FStd, Format(' %-25s | %16.1n | %16.1n | %16.1n | %16.1n | %16.1n', [
        CFC.Drives.Drive[iDrive].Drive,
        Int(CFC.Drives.Drive[iDrive].Total) / 1024,
        Int(CFC.Drives.Drive[iDrive].FreeAtStart) / 1024,
        Int(CFC.Drives.Drive[iDrive].TotalDeletes) / 1024,
        Int(CFC.Drives.Drive[iDrive].TotalAdds) / 1024,
        Int(CFC.Drives.Drive[iDrive].FreeAtFinish) / 1024
      ]));
    End;
End;

(**

  This method parses the size limit command line argument. If an incorrect specification
  an exception is raised.

  @precon  None.
  @postcon Sets the size limit for copying files else raises an exception.

  @param   strOption as a String

**)
Procedure TCommandLineProcessing.ParseSizeLimit(strOption: String);

Const
  strMultipliers = ['k', 'K', 'm', 'M', 'g', 'G', 't', 'T'];

Var
  strValue   : String;
  cM       : Char;
  iSizeLimit : Int64;
  iErrorCode : Integer;
  
Begin
  strValue := strOption;
  Delete(strValue, 1, 9);
  If Copy(strValue, 1, 1) = '=' Then
    Begin
      Delete(strValue, 1, 1);
      If (Length(strValue) > 0) Then
        Begin
          If (Not CharInSet(strValue[Length(strValue)], ['0'..'0']))
            And (CharInSet(strValue[Length(strValue)], strMultipliers)) Then
            Begin
              cM := strValue[Length(strValue)];
              Delete(strValue, Length(strValue), 1);
              Val(strValue, iSizeLimit, iErrorCode);
              If iErrorCode > 0 Then
                Raise EFldrSyncException.Create('Invalid SizeLimit Number');
              Case cM Of
                'k', 'K': FMaxFileSize := iSizeLimit * 1024;
                'm', 'M': FMaxFileSize := iSizeLimit * 1024 * 1024;
                'g', 'G': FMaxFileSize := iSizeLimit * 1024 * 1024 * 1024;
                't', 'T': FMaxFileSize := iSizeLimit * 1024 * 1024 * 1024 * 1024;
              Else  
                FMaxFileSize := iSizeLimit;
              End;
            End Else
              Raise EFldrSyncException.Create('Invalid SizeLimit Multiplier');
        End Else
          Raise EFldrSyncException.Create('Missing SizeLimit Specification');
    End Else
      Raise EFldrSyncException.Create('= expected after SizeLimit');
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
          Else If CompareText(Copy(strOption, 1, 9), 'SizeLimit') = 0 Then
            ParseSizeLimit(strOption)
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
  @param   Update      as a TUpdateType

**)
Procedure TCommandLineProcessing.SearchProc(strFolder, strFileName: String;
  iCount: Integer; Update : TUpdateType);

Begin
  If strFolder = FLastFolder Then
    Begin
      If Not(cloQuiet In FCommandLineOptions) Then
        If (iCount Mod FOutputUpdateInterval = 0) Or (Update = utImmediate) Then
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
