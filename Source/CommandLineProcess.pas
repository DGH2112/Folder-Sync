(**

  This module contains a class for processing the command line and synchronising the
  files associated with the information.

  @Version 2.009
  @Author  David Hoyle
  @Date    29 Apr 2021

  @nocheck HardCodedInteger HardCodedNumber HardCodedString

**)
Unit CommandLineProcess;

Interface

Uses
  SysUtils,
  Classes,
  Graphics,
  SyncModule,
  ApplicationFunctions,
  FldrSync.Console;

Type
  (** An array of ANSI Characters - used for console input selection. **)
  TCharArray = Set Of AnsiChar;

  (** A class to process the command line information and synchronise files. **)
  TCommandLineProcessing = Class
  Strict Private
    FINIFileName         : String;
    FParams              : TStringList;
    FCFC                 : TCompareFoldersCollection;
    FCommandLineOptions  : TCommandLineOptionsRec;
    FTitleColour         : TColor;
    FPathColour          : TColor;
    FSuccessColour       : TColor;
    FExistsColour        : TColor;
    FNotExistsColour     : TColor;
    FInputColour         : TColor;
    FReadOnlyColour      : TColor;
    FExceptionColour     : TColor;
    FHeaderColour        : TColor;
    FLastFolder          : String;
    FOutputUpdateInterval: Integer;
    FFldrSyncOptions     : TFldrSyncOptions;
    FStartTime           : TDateTime;
    FConsole             : TFSConsole;
  Strict Protected
    Procedure ExceptionProc(Const strMsg: String);
    Function GetPause: Boolean;
    Procedure LoadSettings;
    Procedure SaveSettings;
    Procedure DisplayHelp;
    Procedure SearchStartProc(Const strFolder: String);
    Procedure SearchProc(Const strFolder, strFileName: String; Const iCount: Integer;
      Const Update : TUpdateType);
    Procedure SearchEndProc(Const iFileCount: Integer; Const iTotalSize: Int64);
    Procedure CompareStartProc(Const strLeftFldr, strRightFldr: String);
    Procedure CompareProc(Const strLeftFldr, strRightFldr, strFileName: String;
      Const iPosition, iMaxItems: Integer);
    Procedure CompareEndProc;
    Procedure MatchListStartProc;
    Procedure MatchListProc(Const iPosition, iMaxItems: Integer);
    Procedure MatchListEndProc;
    Procedure ClearLine;
    Procedure DeleteStartProc(Const iTotalFileCount: Integer; Const iTotalFileSize: Int64);
    Procedure DeletingProc(Const iCurrentFileToDelete, iTotalFilesToDelete : Integer;
      Const iCumulativeFileSizeBeforeDelete, iTotalFileSizeToDelete: Int64;
      Const strDeletePath, strFileNameToDelete: String);
    Procedure DeletedProc(Const iCurrentFileDeleted, iTotalFilesToDelete: Integer;
      Const iCumulativeFileSizeAfterDelete, iTotalFileSizeToDelete: Int64;
      Const iSuccess: TProcessSuccess);
    Procedure DeleteQueryProc(Const strFilePath: String; Const DeleteFile : TFileRecord;
      Var Option: TFileAction);
    Procedure DeleteReadOnlyQueryProc(Const strFilePath: String; Const DeleteFile : TFileRecord;
      Var Option: TFileAction);
    Procedure DeleteEndProc(Const iTotalDeletedFileCount, iTotalSkippedFileCount,
      iTotalErrorsFileCount: Integer);
    Procedure CopyStartProc(Const iTotalCount: Integer; Const iTotalSize: Int64);
    Procedure CopyContentsProc(Const iCurrentFileToCopy, iTotalFilesToCopy : Integer;
      Const iCumulativeFileSizeBeforeCopy, iTotalFileSizeToCopy,
      iCurrentFileCopiedSizeSoFar, iTotalCurrentFileSize: Int64);
    Procedure CopyingProc(Const iCurrentFileToCopy, iTotalFilesToCopy : Integer;
      Const iCumulativeFileSizeBeforeCopy, iTotalFileSizeToCopy: Int64; Const strSource, strDest,
      strFileName: String);
    Procedure CopiedProc(Const iCurrentFileToCopy, iTotalFilesToCopy: Integer;
      Const iCumulativeFileSizeAfterCopy, iTotalFileSizeToCopy: Int64;
      Const iSuccess : TProcessSuccess);
    Procedure CopyQueryProc(Const strSourcePath, strDestPath: String; Const SourceFile,
      DestFile : TFileRecord; Var Option: TFileAction);
    Procedure CopyReadOnlyQueryProc(Const strSourcePath, strDestPath: String; Const SourceFile,
      DestFile : TFileRecord; Var Option: TFileAction);
    Procedure CopyEndProc(Const iCopied, iSkipped, iError: Integer);
    Function GetConsoleCharacter(Const Characters: TCharArray): Char;
    Procedure FileQuery(Const strMsg, strFilePath: String; Const DeleteFile : TFileRecord;
      Var Option: TFileAction; Const boolReadOnly : Boolean);
    Procedure DiffSizeStart(Const iFileCount: Integer);
    Procedure DiffSize(Const iFile, iFileCount : Integer; Const strLPath, strRPath,
      strFileName: String);
    Procedure DiffSizeEnd();
    Procedure NothingToDoStart(Const iFileCount: Integer);
    Procedure NothingToDo(Const iFile, iFileCount : Integer; Const strLPath, strRPath,
      strFileName: String);
    Procedure NothingToDoEnd();
    Procedure OutputStats(Const CFC : TCompareFoldersCollection);
    Procedure ExceedsSizeLimitStart(Const iFileCount: Integer);
    Procedure ExceedsSizeLimit(Const iFile, iFileCount : integer; Const strLPath, strRPath,
      strFileName: String);
    Procedure ExceedsSizeLimitEnd();
    Procedure ErrorMsgsStart(Const iErrorCount: Integer);
    Procedure ErrorMsgs(Const strErrorMsg : String);
    Procedure ErrorMsgsEnd();
    Procedure DeleteFoldersEnd();
    Procedure DeleteFoldersStart(Const iFolderCount: Integer);
    Procedure DeleteFolders(Const iFolder, iFolders : Integer; Const strFolder: String);
    Procedure CopyError(Const strSource, strDest, strErrorMsg : String;
      Const iLastError : Cardinal; var iResult : TDGHErrorResult);
    Procedure DeleteError(Const strSource, strErrorMsg : String;
      Const iLastError : Cardinal; var iResult : TDGHErrorResult);
    Procedure CheckForEscape;
    Function  CheckPath(Const strOutput : String) : String;
    Procedure OutputFileNumber(Const iCurrentFile, iTotal : Integer);
  Public
    Constructor Create;
    Destructor Destroy; Override;
    Procedure Execute;
    Procedure RaiseFldrSyncException(Const E : Exception);
    (**
      This property returns whether the pause switch was present on the command line.
      @precon  None.
      @postcon Returns whether the pause switch was present on the command line.
      @return  a Boolean
    **)
    Property Pause: Boolean Read GetPause;
  End;


Const
  (** This is a template for the output of an exception message. **)
  strExpMsg = #13#10 +
    'An exception has occurred in the application!'#13#10#13#10 +
    'Class  : %s'#13#10 +
    'Message: %s';

Implementation

Uses
  {$IFDEF DEBUG}
  CodeSiteLogging,
  {$ENDIF}
  System.INIFiles,
  WinAPI.Windows,
  WinAPI.ShellAPI,
  {$IFDEF EUREKALOG_VER7}
  ExceptionLog7,
  EExceptionManager,
  ECallStack,
  EBaseClasses,
  {$ENDIF}
  FldrSync.Functions;

(**

  This method returns true if the passed file is read only.

  @precon  None.
  @postcon Returns true if the passed file is read only.

  @param   strFileName as a String as a constant
  @return  a Boolean

**)
Function IsRO(Const strFileName: String): Boolean;

Begin
  IsRO := GetFileAttributes(PChar(Expand(strFileName))) And FILE_ATTRIBUTE_READONLY > 0;
End;

{ TCommandLineProcessing }

(**

  This method checks for the Escape having been pressed at the command line and if true
  prompts the user as to whether they wish to terminate the process.

  @precon  None.
  @postcon If Escape is pressed the user is prompted to stop the processing.

**)
Procedure TCommandLineProcessing.CheckForEscape;

ResourceString
  strMsg = '    Are you sure you want to stop processing? (Y/N): ';

Const
  wBufferLength : DWord = 1024;

Var
  KBBuffer : Array Of TInputRecord;
  wCharsRead : DWord;
  wEvents : DWord;
  Hnd : THandle;
  i : Integer;
  C: Char;
  ConsoleInfo : TConsoleScreenBufferInfo;
  OldPos : TCoord;

Begin
  Hnd := GetStdHandle(STD_INPUT_HANDLE);
  Win32Check(GetNumberOfConsoleInputEvents(Hnd, wEvents));
  If wEvents > 0 Then
    Begin
      SetLength(KBBuffer, wBufferLength);
      Win32Check(PeekConsoleInput(Hnd, KBBuffer[0], wBufferLength, wCharsRead));
      If wCharsRead > 0 Then
        For i := 0 To wEvents - 1 Do
          If KBBuffer[i].EventType = KEY_EVENT Then
            Begin
              If Boolean(KBBuffer[i].Event.KeyEvent.bKeyDown) Then
                Case KBBuffer[i].Event.KeyEvent.wVirtualKeyCode Of
                  VK_ESCAPE:
                    Begin
                      Win32Check(GetConsoleScreenBufferInfo(FConsole.StdHnd, ConsoleInfo));
                      OldPos := ConsoleInfo.dwCursorPosition;
                      ClearLine;
                      FConsole.OutputToConsole(coStd, strMsg, FInputColour);
                      C := GetConsoleCharacter(['y', 'Y', 'n', 'N']);
                      Case C Of
                        'y', 'Y':
                          Begin
                            FConsole.OutputToConsoleLn(coStd, 'Yes', FInputColour);
                            Abort
                          End;
                        'n', 'N':
                          Begin
                            Win32Check(SetConsoleCursorPosition(FConsole.StdHnd, OldPos));
                            FConsole.OutputToConsole(coStd, StringOfChar(#32, Length(strMsg)));
                            Win32Check(SetConsoleCursorPosition(FConsole.StdHnd, OldPos));
                          End;
                      End;
                    End;
                End;
              FlushConsoleInputBuffer(Hnd);
            End;
      FlushConsoleInputBuffer(Hnd);
    End;
End;

(**

  This method checks the length of the given path text an ensures it will fit on the width of the console
  screen from the cursor position.

  @precon  None.
  @postcon Shortens the path to fit on the console screen.

  @param   strOutput as a String as a constant
  @return  a String

**)
Function TCommandLineProcessing.CheckPath(Const strOutput: String): String;

Var
  ConsoleInfo: _CONSOLE_SCREEN_BUFFER_INFO;
  iMaxWidth : Integer;
  i: Integer;
  j: Integer;

Begin
  If (cloQuiet In FCommandLineOptions.iCommandLineOptions) Then
    Exit;
  GetConsoleScreenBufferInfo(FConsole.StdHnd, ConsoleInfo);
  iMaxWidth := ConsoleInfo.dwSize.X - ConsoleInfo.dwCursorPosition.X - 1;
  Result := strOutput;
  While Length(Result) > iMaxWidth Do
    Begin
      If Pos('...', Result) = 0 Then
        Begin
          i := TFSFunctions.PosOfNthChar(Result, '\', 1) + 1;
          j := TFSFunctions.PosOfNthChar(Result, '\', 2);
          If (i > 0) And (j > 0) Then
            Result := StringReplace(Result, Copy(Result, i, j - i), '...', [])
          Else
            Result := Copy(Result, 1, j - 1) + Copy(Result, j + 1, Length(Result) - j - 1);
        End
      Else
        Begin
          i := TFSFunctions.PosOfNthChar(Result, '\', 2);
          j := TFSFunctions.PosOfNthChar(Result, '\', 3);
          If (i > 0) And (j > 0) Then
            Result := StringReplace(Result, Copy(Result, i, j - i), '', [])
          Else
            Result := Copy(Result, Length(Result) - iMaxWidth, iMaxWidth);
        End;
    End;
End;

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
  GetConsoleScreenBufferInfo(FConsole.StdHnd, ConsoleInfo);
  FConsole.OutputToConsole(coStd, StringOfChar(#32, ConsoleInfo.dwSize.X -
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
  FConsole.OutputToConsoleLn(coStd, 'Done.', FSuccessColour);
End;

(**

  This method is called during the comparison process for each file.

  @precon  None.
  @postcon Outputs the percentage completion of the comparison process.

  @nohint  strLeftFldr strRightFldr

  @param   strLeftFldr  as a String as a constant
  @param   strRightFldr as a String as a constant
  @param   strFileName  as a String as a constant
  @param   iPosition    as an Integer as a constant
  @param   iMaxItems    as an Integer as a constant

**)
Procedure TCommandLineProcessing.CompareProc(Const strLeftFldr, strRightFldr,
  strFileName: String; Const iPosition, iMaxItems: Integer);

Begin
  If Not(cloQuiet In FCommandLineOptions.iCommandLineOptions) Then
    If iPosition Mod 100 = 0 Then
      Begin
        ClearLine;
        FConsole.OutputToConsole(coStd, Format('%1.1n%%: %s',
          [Int(iPosition) / Int(iMaxItems) * 100.0, strFileName]), clNone, clNone, False);
      End;
End;

(**

  This method is called at the start of each pair of folders comparison process.

  @precon  None.
  @postcon Outputs that the comparison process is starting.

  @nohint  strLeftFldr strRightFldr

  @param   strLeftFldr  as a String as a constant
  @param   strRightFldr as a String as a constant

**)
Procedure TCommandLineProcessing.CompareStartProc(Const strLeftFldr, strRightFldr: String);

Begin
  FConsole.OutputToConsole(coStd, 'Comparing: ', FHeaderColour);
End;

(**

  This method is called at the end of each file is Copied.

  @precon  None.
  @postcon Outputs the percentage completion of the overall copying process.

  @nohint  iCurrentFiletoCopy iTotalFilesToCopy iCumulativeFileSizeAfterCopy iTotalFileSizeToCopy

  @param   iCurrentFileToCopy           as an Integer as a constant
  @param   iTotalFilesToCopy            as an Integer as a constant
  @param   iCumulativeFileSizeAfterCopy as an Int64 as a constant
  @param   iTotalFileSizeToCopy         as an Int64 as a constant
  @param   iSuccess                     as a TProcessSuccess as a constant

**)
Procedure TCommandLineProcessing.CopiedProc(Const iCurrentFileToCopy, iTotalFilesToCopy: Integer;
    Const iCumulativeFileSizeAfterCopy, iTotalFileSizeToCopy: Int64;
    Const iSuccess : TProcessSuccess);

Begin
  ClearLine;
  Case iSuccess Of
    //psSuccessed: FConsole.OutputToConsole(coStd, Format(' %1.1n%% Complete',
    //  [Int64(iCumulativeFileSizeAfterCopy) / Int64(iTotalFileSizeToCopy) * 100.0]),
    //  FSuccessColour);
    psFailed: FConsole.OutputToConsoleLn(coStd,
      Format(' Error Copying file (error type ignored [%s]).', [FCFC.LastError]),
      FExceptionColour);
    //psIgnoreOnce, psIgnoreAll: FConsole.OutputToConsoleLn(coStd, ' Ignored Copying file.',
    //  FExceptionColour);
  End;
  CheckForEscape;
End;

(**

  This method is called during the copying process for each file to provide a progress update to the 
  amount if the file copying completed.

  @precon  None.
  @postcon Outputs the percentage completion of the individual file copying.

  @nohint  iCurrentfiletoCopy iTotalFilesToCopy 

  @param   iCurrentFileToCopy            as an Integer as a constant
  @param   iTotalFilesToCopy             as an Integer as a constant
  @param   iCumulativeFileSizeBeforeCopy as an Int64 as a constant
  @param   iTotalFileSizeToCopy          as an Int64 as a constant
  @param   iCurrentFileCopiedSizeSoFar   as an Int64 as a constant
  @param   iTotalCurrentFileSize         as an Int64 as a constant

**)
Procedure TCommandLineProcessing.CopyContentsProc(Const iCurrentFileToCopy,
  iTotalFilesToCopy : Integer; Const iCumulativeFileSizeBeforeCopy, iTotalFileSizeToCopy,
  iCurrentFileCopiedSizeSoFar, iTotalCurrentFileSize: Int64);

Var
  strOutput : String;
  strFileSize : String;
  iLength : Integer;

Begin
  ClearLine;
  If iTotalFileSizeToCopy <> 0 Then
    Begin
      strOutput := Format('    Copying %5.1f%% (%1.2f%%)',
        [Int64(iCurrentFileCopiedSizeSoFar) / Int64(iTotalCurrentFileSize) * 100.0,
         Int64(iCumulativeFileSizeBeforeCopy + iCurrentFileCopiedSizeSoFar) /
           Int64(iTotalFileSizeToCopy) * 100.0]);
      strFileSize := Format('%1.1n', [Int(iTotalCurrentFileSize) / 1024.0]);
      iLength := Length(strFileSize);
      strOutput := strOutput + Format(', %*.1n of %*.1n kbytes copied', [iLength,
        Int(iCurrentFileCopiedSizeSoFar) / 1024.0, iLength,
        Int(iTotalCurrentFileSize) / 1024.0]);
      strOutput := strOutput + Format(', %s', [UpdateRemainingTime(FStartTime,
           Int64(iCumulativeFileSizeBeforeCopy + iCurrentFileCopiedSizeSoFar) /
           Int64(iTotalFileSizeToCopy))]);
    End Else
      strOutput := '    Copying...';
  FConsole.OutputToConsole(coStd, strOutput, clNone, clNone, False);
End;

(**

  This method is called at the end of the file copying process.

  @precon  None.
  @postcon Outputs how many files were copied and how many were skipped.

  @param   iCopied  as an Integer as a constant
  @param   iSkipped as an Integer as a constant
  @param   iError   as an Integer as a constant

**)
Procedure TCommandLineProcessing.CopyEndProc(Const iCopied, iSkipped, iError: Integer);

Begin
  ClearLine;
  FConsole.OutputToConsole(coStd, Format('  Copied %1.0n file(s) (Skipped %1.0n file(s)',
      [Int(iCopied), Int(iSkipped)]), FSuccessColour);
  If iError > 0 Then
    Begin
      FConsole.OutputToConsole(coErr, ', ', FSuccessColour);
      FConsole.OutputToConsole(coErr, Format('Errored %1.0n file(s)', [Int(iError)]),
        FExceptionColour);
    End;
  FConsole.OutputToConsoleLn(coStd, ').', FSuccessColour);
End;

(**

  This method displays an error message on the screen and ask the user to ignore or stop the processing.

  @precon  None.
  @postcon The iResult value is change depending upon the users answer to the question.

  @param   strSource   as a String as a constant
  @param   strDest     as a String as a constant
  @param   strErrorMsg as a String as a constant
  @param   iLastError  as a Cardinal as a constant
  @param   iResult     as a TDGHErrorResult as a reference

**)
Procedure TCommandLineProcessing.CopyError(Const strSource, strDest, strErrorMsg: String;
  Const iLastError : Cardinal; Var iResult: TDGHErrorResult);

Var
  Ch: Char;

Begin
  ClearLine;
  FConsole.OutputToConsoleLn(coStd, '    An error has occurred during the copying of files:',
    FExceptionColour);
  FConsole.OutputToConsoleLn(coStd, Format('      Source     : %s', [strSource]));
  FConsole.OutputToConsoleLn(coStd, Format('      Destination: %s', [strDest]));
  FConsole.OutputToConsoleLn(coStd, Format('      OS Error   : (%d) %s', [iLastError, strErrorMsg]),
    FExceptionColour);
  FConsole.OutputToConsole(coStd, '    Do you want to [I]gnore once, Ignore [A]ll the errors or [S]top processing? ',
    FInputColour);
  Ch := GetConsoleCharacter(['i', 'I', 'a', 'A', 'S', 's']);
  Case Ch Of
    'i', 'I': iResult := derIgnoreOnce;
    'a', 'A': iResult := derIgnoreAll;
    's', 'S': iResult := derStop;
  End;
  FConsole.OutputToConsoleLn(coStd, Ch, FInputColour);
End;

(**

  This method is called at the start of the copying of a single file.

  @precon  None.
  @postcon Outputs the file information for the file being copied.

  @nohint  iCumulativeFileSizeBeforeCopy iTotalFileSizeToCopy

  @param   iCurrentFileToCopy            as an Integer as a constant
  @param   iTotalFilesToCopy             as an Integer as a constant
  @param   iCumulativeFileSizeBeforeCopy as an Int64 as a constant
  @param   iTotalFileSizeToCopy          as an Int64 as a constant
  @param   strSource                     as a String as a constant
  @param   strDest                       as a String as a constant
  @param   strFileName                   as a String as a constant

**)
Procedure TCommandLineProcessing.CopyingProc(Const iCurrentFileToCopy,
  iTotalFilesToCopy : Integer; Const iCumulativeFileSizeBeforeCopy, iTotalFileSizeToCopy: Int64;
  Const strSource, strDest, strFileName: String);

Var
  iColour: TColor;

Begin
  ClearLine;
  OutputFileNumber(iCurrentFileToCopy, iTotalFilesToCopy);
  FConsole.OutputToConsole(coStd, strSource, FPathColour);
  FConsole.OutputToConsole(coStd, ' => ');
  FConsole.OutputToConsole(coStd, strDest, FPathColour);
  If FileExists(strDest + strFileName) Then
    iColour := FExistsColour
  Else
    iColour := FNotExistsColour;
  If Not IsRO(strSource + strFileName) Then
    FConsole.OutputToConsoleLn(coStd, CheckPath(strFileName), iColour)
  Else
    FConsole.OutputToConsoleLn(coStd, CheckPath(strFileName), FReadOnlyColour);
End;

(**

  This method is called for each file that requires to be overwritten.

  @precon  None.
  @postcon Queries the user for the action to be taken

  @nohint  strSourcePath

  @param   strSourcePath as a String as a constant
  @param   strDestPath   as a String as a constant
  @param   SourceFile    as a TFileRecord as a constant
  @param   DestFile      as a TFileRecord as a constant
  @param   Option        as a TFileAction as a reference

**)
Procedure TCommandLineProcessing.CopyQueryProc(Const strSourcePath, strDestPath: String;
  Const SourceFile, DestFile : TFileRecord; Var Option: TFileAction);

Begin
  ClearLine;
  FileQuery('    Overwrite (Y/N/A/O/C)? ', strDestPath + DestFile.FileName, SourceFile, Option,
    False)
End;

(**

  This method is called for each read-only file that requires to be overwritten.

  @precon  None.
  @postcon Queries the user for the action to be taken

  @nohint  strSourcePath

  @param   strSourcePath as a String as a constant
  @param   strDestPath   as a String as a constant
  @param   SourceFile    as a TFileRecord as a constant
  @param   DestFile      as a TFileRecord as a constant
  @param   Option        as a TFileAction as a reference

**)
Procedure TCommandLineProcessing.CopyReadOnlyQueryProc(Const strSourcePath, strDestPath: String;
  Const SourceFile, DestFile : TFileRecord; Var Option: TFileAction);

Begin
  ClearLine;
  FileQuery('    Overwrite READONLY (Y/N/A/O/C)? ', strDestPath + DestFile.FileName,
    SourceFile, Option, True)
End;

(**

  This method is called at the start of the copying process.

  @precon  None.
  @postcon Outputs the number of files to be copied.

  @nohint  iTotalSize
  
  @param   iTotalCount as an Integer as a constant
  @param   iTotalSize  as an Int64 as a constant

**)
Procedure TCommandLineProcessing.CopyStartProc(Const iTotalCount: Integer; Const iTotalSize: Int64);

Begin
  FStartTime  := Now();
  FConsole.OutputToConsoleLn(coStd, Format('Copying %1.0n files...', [Int(iTotalCount)]),
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
  FINIFileName := TFSFunctions.BuildRootKey(FParams, ExceptionProc);
  Include(FCommandLineOptions.iSyncOptions, soEnabled);
  FCommandLineOptions.iMaxFileSize := 0;
  FConsole := TFSConsole.Create;
  LoadSettings;
End;

(**

  This method is called at the end of the deletion of each file.

  @precon  None.
  @postcon Output the percentage completion of the deletion process.

  @nohint  iCurrentFileDeleted iTotalFilesToDelete

  @param   iCurrentFileDeleted            as an Integer as a constant
  @param   iTotalFilesToDelete            as an Integer as a constant
  @param   iCumulativeFileSizeAfterDelete as an Int64 as a constant
  @param   iTotalFileSizeToDelete         as an Int64 as a constant
  @param   iSuccess                       as a TProcessSuccess as a constant

**)
Procedure TCommandLineProcessing.DeletedProc(Const iCurrentFileDeleted,
  iTotalFilesToDelete: Integer; Const iCumulativeFileSizeAfterDelete,
  iTotalFileSizeToDelete: Int64; Const iSuccess: TProcessSuccess);

Var
  dblPercent: Double;

Begin
  ClearLine;
  Case iSuccess Of
    //psSuccessed: FConsole.OutputToConsoleLn(coStd, Format(' %1.1n%% Complete',
    //  [Int(iCumulativeFileSizeAfterDelete) / Int(iTotalFileSizeToDelete) * 100.0]),
    //  FSuccessColour);
    psFailed: FConsole.OutputToConsoleLn(coStd,
      Format(' Error Deleting file (error type ignored [%s]).', [FCFC.LastError]),
      FExceptionColour);
    //psIgnoreOnce, psIgnoreAll: FConsole.OutputToConsoleLn(coStd, ' Ignored Copying file.',
    //  FExceptionColour);
  End;
  CheckForEscape;
  dblPercent := 0;
  If iTotalFileSizeToDelete <> 0 Then
    dblPercent := Int(iCumulativeFileSizeAfterDelete) / Int(iTotalFileSizeToDelete);
  FConsole.OutputToConsole(coStd, Format('    Deleting %1.2f%%, %s', [dblPercent * 100.0,
     UpdateRemainingTime(FStartTime, dblPercent)]), clNone, clNone, False);
End;

(**

  This method is called at the end of the file deletion process.

  @precon  None.
  @postcon Outputs how many files were deleted and how many were skipped.

  @param   iTotalDeletedFileCount as an Integer as a constant
  @param   iTotalSkippedFileCount as an Integer as a constant
  @param   iTotalErrorsFileCount  as an Integer as a constant

**)
Procedure TCommandLineProcessing.DeleteEndProc(Const iTotalDeletedFileCount,
  iTotalSkippedFileCount, iTotalErrorsFileCount: Integer);

Begin
  ClearLine;
  FConsole.OutputToConsole(coStd, Format('  Deleted %1.0n file(s) (Skipped %1.0n file(s)',
      [Int(iTotalDeletedFileCount), Int(iTotalSkippedFileCount)]), FSuccessColour);
  If iTotalErrorsFileCount > 0 Then
    Begin
      FConsole.OutputToConsole(coErr, ', ', FSuccessColour);
      FConsole.OutputToConsole(coErr, Format('Errored %1.0n file(s)', [Int(iTotalErrorsFileCount)]),
        FExceptionColour);
    End;
  FConsole.OutputToConsoleLn(coStd, ').');
End;

(**

  This method is displayed upon a deletion error and prompts the user as to whether they want to ignore 
  the error or stop processing.

  @precon  None.
  @postcon The iResult value is changed depending upon the users answer.

  @param   strSource   as a String as a constant
  @param   strErrorMsg as a String as a constant
  @param   iLastError  as a Cardinal as a constant
  @param   iResult     as a TDGHErrorResult as a reference

**)
Procedure TCommandLineProcessing.DeleteError(Const strSource, strErrorMsg: String;
  Const iLastError : Cardinal; Var iResult: TDGHErrorResult);

Var
  Ch : Char;

Begin
  ClearLine;
  FConsole.OutputToConsoleLn(coStd, '    An error has occurred during the deletion of files:',
    FExceptionColour);
  FConsole.OutputToConsoleLn(coStd, Format('      Source     : %s', [strSource]));
  FConsole.OutputToConsoleLn(coStd, Format('      OS Error   : (%d) %s', [iLastError, strErrorMsg]),
    FExceptionColour);
  FConsole.OutputToConsole(coStd, '    Do you want to [I]gnore Once, Ignore [A]ll the errors or [S]top processing? ',
    FInputColour);
  Ch := GetConsoleCharacter(['i', 'I', 'a', 'A', 'S', 's']);
  Case Ch Of
    'i', 'I': iResult := derIgnoreOnce;
    'a', 'A': iResult := derIgnoreAll;
    's', 'S': iResult := derStop;
  End;
  FConsole.OutputToConsoleLn(coStd, Ch, FInputColour);
End;

(**

  This is an on delete folders event handler.

  @precon  None.
  @postcon Outputs the folder being deleted.

  @param   iFolder   as an Integer as a constant
  @param   iFolders  as an Integer as a constant
  @param   strFolder as a String as a constant

**)
Procedure TCommandLineProcessing.DeleteFolders(Const iFolder, iFolders: Integer;
  Const strFolder: String);

Begin
  ClearLine;
  OutputFileNumber(iFolder, iFolders);
  FConsole.OutputToConsoleLn(coStd, CheckPath(strFolder));
End;

(**

  This is an on Delete Folders End event handler.

  @precon  None.
  @postcon Do nothing.

  @nocheck EmptyMethod

**)
Procedure TCommandLineProcessing.DeleteFoldersEnd;

Begin
End;

(**

  This is an on delete folder start event handler.

  @precon  None.
  @postcon Outputs a header in the console for the list of deleted files.

  @param   iFolderCount as an Integer as a constant

**)
Procedure TCommandLineProcessing.DeleteFoldersStart(Const iFolderCount: Integer);

Begin
  ClearLine;
  If iFolderCount > 0 Then
    FConsole.OutputToConsoleLn(coStd, 'Deleting empty folders...', FHeaderColour);
End;

(**

  This method is called for each file that requires deletion.

  @precon  None.
  @postcon Queries the user for action.

  @param   strFilePath as a String as a constant
  @param   DeleteFile  as a TFileRecord as a constant
  @param   Option      as a TFileAction as a reference

**)
Procedure TCommandLineProcessing.DeleteQueryProc(Const strFilePath: String;
  Const DeleteFile : TFileRecord; Var Option: TFileAction);

Begin
  ClearLine;
  FileQuery('    Delete (Y/N/A/O/C)? ', strFilePath, DeleteFile, Option, False);
End;

(**

  This method is called for each read-only file that requires deletion.

  @precon  None.
  @postcon Queries the user for action.

  @param   strFilePath as a String as a constant
  @param   DeleteFile  as a TFileRecord as a constant
  @param   Option      as a TFileAction as a reference

**)
Procedure TCommandLineProcessing.DeleteReadOnlyQueryProc(Const strFilePath: String;
  Const DeleteFile : TFileRecord; Var Option: TFileAction);

Begin
  ClearLine;
  FileQuery('    Delete READONLY (Y/N/A/O/C)? ', strFilePath, DeleteFile, Option, True);
End;

(**

  This is an On Delete Start event handler for the synchronisation process.

  @precon  None.
  @postcon Displays the number of files to be deleted.

  @param   iTotalFileCount as an Integer as a constant
  @param   iTotalFileSize  as an Int64 as a constant

**)
Procedure TCommandLineProcessing.DeleteStartProc(Const iTotalFileCount: Integer;
  Const iTotalFileSize: Int64);

Begin
  ClearLine;
  FStartTime  := Now();
  FConsole.OutputToConsoleLn(coStd, Format('Deleting %1.0n files (%1.0n bytes)...',
      [Int(iTotalFileCount), Int(iTotalFileSize)]), FHeaderColour);
End;

(**

  This method is called at the start of the deletion of each individual file.

  @precon  None.
  @postcon Outputs the name of the file being deleted.

  @param   iCurrentFileToDelete            as an Integer as a constant
  @param   iTotalFilesToDelete             as an Integer as a constant
  @param   iCumulativeFileSizeBeforeDelete as an Int64 as a constant
  @param   iTotalFileSizeToDelete          as an Int64 as a constant
  @param   strDeletePath                   as a String as a constant
  @param   strFileNameToDelete             as a String as a constant

**)
Procedure TCommandLineProcessing.DeletingProc(Const iCurrentFileToDelete,
  iTotalFilesToDelete : Integer; Const iCumulativeFileSizeBeforeDelete,
  iTotalFileSizeToDelete: Int64; Const strDeletePath, strFileNameToDelete: String);

Var
  dblPercent : Double;

Begin
  ClearLine;
  OutputFileNumber(iCurrentFileToDelete, iTotalFilesToDelete);
  FConsole.OutputToConsole(coStd, strDeletePath, FPathColour);
  If Not IsRO(strFileNameToDelete) Then
    FConsole.OutputToConsoleLn(coStd, CheckPath(strFileNameToDelete), FExistsColour)
  ELse
    FConsole.OutputToConsoleLn(coStd, CheckPath(strFileNameToDelete), FReadOnlyColour);
  dblPercent := 0;
  If iTotalFileSizeToDelete <> 0 Then
    dblPercent := Int(iCumulativeFileSizeBeforeDelete) / Int(iTotalFileSizeToDelete);
  FConsole.OutputToConsole(coStd, Format('    Deleting %1.2f%%, %s', [dblPercent * 100.0,
     UpdateRemainingTime(FStartTime, dblPercent)]), clNone, clNone, False);
End;

(**

  A destructor for the TCommandLineProcessing class.

  @precon  None.
  @postcon Frees the memory used by the application.

**)
Destructor TCommandLineProcessing.Destroy;

Begin
  SaveSettings;
  FConsole.Free;
  FParams.Free;
  Inherited;
End;

(**

  This method is called for each files that has a size difference only.

  @precon  None.
  @postcon Outputs the name of the files which has a size difference only.

  @param   iFile       as an Integer as a constant
  @param   iFileCount  as an Integer as a constant
  @param   strLPath    as a String as a constant
  @param   strRPath    as a String as a constant
  @param   strFileName as a String as a constant

**)
Procedure TCommandLineProcessing.DiffSize(Const iFile, iFileCount : Integer; Const strLPath, strRPath,
  strFileName: String);

Begin
  OutputFileNumber(iFile, iFileCount);
  FConsole.OutputToConsole(coStd, strLPath, FPathColour);
  FConsole.OutputToConsole(coStd, ' => ');
  FConsole.OutputToConsole(coStd, strRPath, FPathColour);
  If Not IsRO(strRPath + strFileName) Then
    FConsole.OutputToConsoleLn(coStd, strFileName, FExistsColour)
  Else
    FConsole.OutputToConsoleLn(coStd, strFileName, FReadOnlyColour);
End;

(**

  This method is called at the end of the size different process.

  @precon  None.
  @postcon Does nothing.

  @nocheck EmptyMethod

**)
Procedure TCommandLineProcessing.DiffSizeEnd;

Begin
  // Do nothing
End;

(**

  This method is called at the start of the size difference processing.

  @precon  None.
  @postcon Outputs the number of files which have a size difference only.

  @param   iFileCount as an Integer as a constant

**)
Procedure TCommandLineProcessing.DiffSizeStart(Const iFileCount: Integer);

Begin
  If iFileCount > 0 Then
    FConsole.OutputToConsoleLn(coStd, Format('%1.0n Files with Differing Sizes...',
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
      FConsole.OutputToConsoleLn(coStd, 'Opening HTML Help...');
      strEXEName := TFSFunctions.ExtractEXEFromExt('.chm');
      ShellExecute(0, 'Open', PChar(strEXEName), PChar('"' + strHelpFile + strPage + '"'),
        PChar(ExtractFilePath(strHelpFile)), SW_NORMAL);
    End
  Else
    FConsole.OutputToConsoleLn(coErr, Format('Can not find the HTML Help file "%s".', [strHelpFile]
        ), FExceptionColour);
End;

(**

  This is an on error message event handler for outputting errors messages from the delete and copying 
  process.

  @precon  None.
  @postcon Outputs an error message to the log file.

  @param   strErrorMsg as a String as a constant

**)
Procedure TCommandLineProcessing.ErrorMsgs(Const strErrorMsg: String);

Begin
  FConsole.OutputToConsoleLn(coStd, #32#32 + strErrorMsg, FExceptionColour);
End;

(**

  This is an on error message end event handler for outputting the error messages.

  @precon  None.
  @postcon Does nothing.

  @nocheck EmptyMethod

**)
Procedure TCommandLineProcessing.ErrorMsgsEnd;

Begin
End;

(**

  This is an on error message start event handler for the outputting of error messages from the deleting
  and copying process.

  @precon  None.
  @postcon Outputs a header for error messages to the log file if there are any errors reported.

  @param   iErrorCount as an Integer as a constant

**)
Procedure TCommandLineProcessing.ErrorMsgsStart(Const iErrorCount: Integer);

Begin
  If iErrorCount > 0 Then
    FConsole.OutputToConsoleLn(coStd, Format('%1.0n Files had errors...', [Int(iErrorCount)]
        ), FHeaderColour);
End;

(**

  This is an on Exceeds Size Limit event handler.

  @precon  None.
  @postcon Outputs a line of information for each file considered as too large for copying.

  @param   iFile       as an integer as a constant
  @param   iFileCount  as an integer as a constant
  @param   strLPath    as a String as a constant
  @param   strRPath    as a String as a constant
  @param   strFileName as a String as a constant

**)
Procedure TCommandLineProcessing.ExceedsSizeLimit(Const iFile, iFileCount : integer; Const strLPath,
  strRPath, strFileName: String);

Begin
  OutputFileNumber(iFile, iFileCount);
  FConsole.OutputToConsole(coStd, strLPath, FPathColour);
  FConsole.OutputToConsole(coStd, ' => ');
  FConsole.OutputToConsole(coStd, strRPath, FPathColour);
  If Not IsRO(strRPath + strFileName) Then
    FConsole.OutputToConsoleLn(coStd, CheckPath(strFileName), FExistsColour)
  Else
    FConsole.OutputToConsoleLn(coStd, CheckPath(strFileName), FReadOnlyColour);
End;

(**

  This is an on Exceeds Size Limit End Event handler.

  @precon  None.
  @postcon Does nothing.

  @nocheck EmptyMethod

**)
Procedure TCommandLineProcessing.ExceedsSizeLimitEnd;

Begin
  // Do Nothing
End;

(**

  This is an on exceeds size limit start event handler.

  @precon  None.
  @postcon Outputs a headers to the list of files that are considered too large for copying.

  @param   iFileCount as an Integer as a constant

**)
Procedure TCommandLineProcessing.ExceedsSizeLimitStart(Const iFileCount: Integer);

Begin
  If iFileCount > 0 Then
    FConsole.OutputToConsoleLn(coStd, Format('%1.0n Files exceeding Size Limit...', [Int(iFileCount)]
        ), FHeaderColour);
End;

(**

  This is an on exception handler for the Build Root Key method.

  @precon  None.
  @postcon Outputs an exception message to the console.

  @param   strMsg as a String as a constant

**)
Procedure TCommandLineProcessing.ExceptionProc(Const strMsg: String);

Begin
  FConsole.OutputToConsoleLn(coStd, 'Exception: ' + strMsg, clRed);
End;

(**

  This method starts the processing of the information from the command line.

  @precon  None.
  @postcon Starts the processing of the information from the command line.

**)
Procedure TCommandLineProcessing.Execute();

ResourceString
  strTitle =
    'Folder Sync %d.%d%s (Build %s) [%s] A command line tool to synchronise directories.';
  strMsg = 'Drive "%s" does not have enough disk space. Do you want to continue (Y/N)? ';

Var
  Folders  : TFolders;
  iDrive : Integer;
  D : TDriveTotal;
  C : Char;

Begin
  FConsole.OutputToConsoleLn(coStd, TFSFunctions.GetConsoleTitle(strTitle), FTitleColour);
  FConsole.OutputToConsoleLn(coStd);
  ProcessCommandLine(FParams, FCommandLineOptions);
  If cloHelp In FCommandLineOptions.iCommandLineOptions Then
    Begin
      DisplayHelp;
      Exit;
    End;
  FConsole.OutputToConsoleLn(coStd, 'Synchronising folders:', FHeaderColour);
  If Not DirectoryExists(FCommandLineOptions.strSourceFldr) Then
    If Not ForceDirectories(FCommandLineOptions.strSourceFldr) Then
      Raise EFldrSyncException.CreateFmt('Could not create the directory "%s".',
        [FCommandLineOptions.strSourceFldr]);
  FConsole.OutputToConsoleLn(coStd, #32#32 + 'Source:   ' + ExpandFileName(FCommandLineOptions.strSourceFldr));
  If Not DirectoryExists(FCommandLineOptions.strDestFldr) Then
    If Not ForceDirectories(FCommandLineOptions.strDestFldr) Then
      Raise EFldrSyncException.CreateFmt('Could not create the directory "%s".',
        [FCommandLineOptions.strDestFldr]);
  FConsole.OutputToConsoleLn(coStd, #32#32 + 'Dest:     ' + ExpandFileName(FCommandLineOptions.strDestFldr));
  If FCommandLineOptions.strFilePatterns = '' Then
    FCommandLineOptions.strFilePatterns := '*.*';
  FConsole.OutputToConsoleLn(coStd, #32#32 + 'Patterns: ' + FCommandLineOptions.strFilePatterns);
  FConsole.OutputToConsoleLn(coStd);
  FCFC := TCompareFoldersCollection.Create(0);
  Try
    Folders := TFolders.Create;
    Try
      Folders.Add(TFolder.Create(FCommandLineOptions.strSourceFldr,
        FCommandLineOptions.strDestFldr, FCommandLineOptions.strFilePatterns,
        FCommandLineOptions.iSyncOptions,
        FCommandLineOptions.iMaxFileSize));
      FCFC.OnSearchStart           := SearchStartProc;
      FCFC.OnSearch                := SearchProc;
      FCFC.OnSearchEnd             := SearchEndProc;
      FCFC.OnCompareStart          := CompareStartProc;
      FCFC.OnCompare               := CompareProc;
      FCFC.OnCompareEnd            := CompareEndProc;
      FCFC.OnMatchListStart        := MatchListStartProc;
      FCFC.OnMatchList             := MatchListProc;
      FCFC.OnMatchListEnd          := MatchListEndProc;
      FCFC.OnDeleteStart           := DeleteStartProc;
      FCFC.OnDeleting              := DeletingProc;
      FCFC.OnDeleted               := DeletedProc;
      FCFC.OnDeleteQuery           := DeleteQueryProc;
      FCFC.OnDeleteReadOnlyQuery   := DeleteReadOnlyQueryProc;
      FCFC.OnDeleteEnd             := DeleteEndProc;
      FCFC.OnCopyStart             := CopyStartProc;
      FCFC.OnCopying               := CopyingProc;
      FCFC.OnCopyContents          := CopyContentsProc;
      FCFC.OnCopied                := CopiedProc;
      FCFC.OnCopyQuery             := CopyQueryProc;
      FCFC.OnCopyReadOnlyQuery     := CopyReadOnlyQueryProc;
      FCFC.OnCopyEnd               := CopyEndProc;
      FCFC.OnDiffSizeStart         := DiffSizeStart;
      FCFC.OnDiffSize              := DiffSize;
      FCFC.OnDiffSizeEnd           := DiffSizeEnd;
      FCFC.OnNothingToDoStart      := NothingToDoStart;
      FCFC.OnNothingToDo           := NothingToDo;
      FCFC.OnNothingToDoEnd        := NothingToDoEnd;
      FCFC.OnExceedsSizeLimitStart := ExceedsSizeLimitStart;
      FCFC.OnExceedsSizeLimit      := ExceedsSizeLimit;
      FCFC.OnExceedsSizeLimitEnd   := ExceedsSizeLimitEnd;
      FCFC.OnErrorMsgsStart        := ErrorMsgsStart;
      FCFC.OnErrorMsgs             := ErrorMsgs;
      FCFC.OnErrorMsgsEnd          := ErrorMsgsEnd;
      FCFC.OnDeleteFoldersStart    := DeleteFoldersStart;
      FCFC.OnDeleteFolders         := DeleteFolders;
      FCFC.OnDeleteFoldersEnd      := DeleteFoldersEnd;
      FCFC.OnCopyError             := CopyError;
      FCFC.OnDeleteError           := DeleteError;
      FCFC.ProcessFolders(Folders, FCommandLineOptions.strExclusions);
      Try
        OutputStats(FCFC);
        For iDrive := 0 To FCFC.Drives.Count - 1 Do
          Begin
            D := FCFC.Drives.Drive[iDrive];
            If D.FreeAtFinish <= 0 Then
              Begin
                FConsole.OutputToConsole(coStd, Format(strMsg, [D.Drive]), FInputColour);
                C := GetConsoleCharacter(['y', 'Y', 'n', 'N']);
                FConsole.OutputToConsoleLn(coStd, C, FInputColour);
                Case C Of
                  'n', 'N': Exit;
                End;
              End;
          End;
        FFldrSyncOptions := [];
        If clsDeletePermentently In FCommandLineOptions.iCommandLineOptions Then
          Include(FFldrSyncOptions, fsoPermanentlyDeleteFiles);
        If clsBatchRecycleFiles In FCommandLineOptions.iCommandLineOptions Then
          Include(FFldrSyncOptions, fsoBatchRecycleFiles);
        FCFC.ProcessFiles(FFldrSyncOptions);
      Except
        On E : EAbort Do {Do nothing};
        On E : EFldrSyncException Do
          RaiseFldrSyncException(E);
      End;
    Finally
      Folders.Free;
    End;
  Finally
    FCFC.Free;
  End;
End;

(**

  This method queries the user via the command line as to the action to be take for the deletion of a 
  file.

  @precon  None.
  @postcon Prompts the user for action if the var parameter is not set to All.

  @nohint  strFilePath DeleteFile

  @param   strMsg       as a String as a constant
  @param   strFilePath  as a String as a constant
  @param   DeleteFile   as a TFileRecord as a constant
  @param   Option       as a TFileAction as a reference
  @param   boolReadOnly as a Boolean as a constant

**)
Procedure TCommandLineProcessing.FileQuery(Const strMsg, strFilePath: String;
  Const DeleteFile : TFileRecord; Var Option: TFileAction; Const boolReadOnly : Boolean);

Var
  C: Char;
  ConsoleInfo : TConsoleScreenBufferInfo;
  OldPos : TCoord;

Begin
  Win32Check(GetConsoleScreenBufferInfo(FConsole.StdHnd, ConsoleInfo));
  OldPos := ConsoleInfo.dwCursorPosition;
  If (Not boolReadOnly And Not (Option In [faYesToAll, faNoToAll])) Or
     (    boolReadOnly And Not (Option In [faYesToAllRO, faNoToAllRO])) Then
    Begin
      FConsole.OutputToConsole(coStd, strMsg, FInputColour);
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
      FConsole.OutputToConsole(coStd, strFileOptions[Option], FInputColour);
    End;
  Win32Check(SetConsoleCursorPosition(FConsole.StdHnd, OldPos));
  FConsole.OutputToConsole(coStd, StringOfChar(#32, Length(strMsg + strFileOptions[Option])));
  Win32Check(SetConsoleCursorPosition(FConsole.StdHnd, OldPos));
End;

(**

  This method queries the console input buffer for key presses and returns the character pressed if its 
  in a predefined list.

  @precon  Characters is an array of chars that are valid inputs.
  @postcon Queries the console input buffer for key presses and returns the character pressed if its in 
           a predefined list.

  @param   Characters as a TCharArray as a constant
  @return  a Char

**)
Function TCommandLineProcessing.GetConsoleCharacter(Const Characters: TCharArray): Char;

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
  Result := cloPause In FCommandLineOptions.iCommandLineOptions;
End;

(**

  This method loads the applications settings from its INI file.

  @precon  None.
  @postcon The applications settings are loaded from its INI file.

**)
Procedure TCommandLineProcessing.LoadSettings;

Var
  iniMemFile: TMemIniFile;

Begin
  iniMemFile := TMemINIFile.Create(FINIFileName);
  Try
    FTitleColour     := StringToColor(iniMemFile.ReadString('Colours', 'Title', 'clWhite'));
    FPathColour      := StringToColor(iniMemFile.ReadString('Colours', 'Path', 'clYellow'));
    FSuccessColour   := StringToColor(iniMemFile.ReadString('Colours', 'Success', 'clLime'));
    FExistsColour    := StringToColor(iniMemFile.ReadString('Colours', 'Exists', 'clWhite'));
    FNotExistsColour := StringToColor(iniMemFile.ReadString('Colours', 'NotExists', 'clGray'));
    FInputColour     := StringToColor(iniMemFile.ReadString('Colours', 'Input', 'clFuchsia'));
    FReadOnlyColour  := StringToColor(iniMemFile.ReadString('Colours', 'ReadOnly', 'clMaroon'));
    FExceptionColour := StringToColor(iniMemFile.ReadString('Colours', 'Exception', 'clRed'));
    FHeaderColour    := StringToColor(iniMemFile.ReadString('Colours', 'Header', 'clWhite'));
    FOutputUpdateInterval := iniMemFile.ReadInteger('Setup', 'OutputUpdateInterval', 10);
  Finally
    iniMemFile.Free;
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
  FConsole.OutputToConsoleLn(coStd, 'Done.', FSuccessColour);
End;

(**

  This method is called for each files that is matched.

  @precon  None.
  @postcon Outputs the progress through the matching progress.

  @param   iPosition as an Integer as a constant
  @param   iMaxItems as an Integer as a constant

**)
Procedure TCommandLineProcessing.MatchListProc(Const iPosition, iMaxItems: Integer);

Begin
  If Not(cloQuiet In FCommandLineOptions.iCommandLineOptions) Then
    Begin
      If iPosition Mod 100 = 0 Then
        FConsole.OutputToConsole(coStd, Format('Processing Item %1.0n of %1.0n (%1.0n%%)...',
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
  FConsole.OutputToConsole(coStd, 'Match Lists: ', FHeaderColour);
End;

(**

  This method is called for each file that is marked as Nothing To Do.

  @precon  None.
  @postcon Outputs the name and path(s) of the file.

  @param   iFile       as an Integer as a constant
  @param   iFileCount  as an Integer as a constant
  @param   strLPath    as a String as a constant
  @param   strRPath    as a String as a constant
  @param   strFileName as a String as a constant

**)
Procedure TCommandLineProcessing.NothingToDo(Const iFile, iFileCount : Integer; Const strLPath,
  strRPath, strFileName: String);

Begin
  OutputFileNumber(iFile, iFileCount);
  FConsole.OutputToConsole(coStd, strLPath, FPathColour);
  FConsole.OutputToConsole(coStd, ' => ');
  FConsole.OutputToConsole(coStd, strRPath, FPathColour);
  If Not IsRO(strRPath + strFileName) Then
    FConsole.OutputToConsoleLn(coStd, CheckPath(strFileName), FExistsColour)
  Else
    FConsole.OutputToConsoleLn(coStd, CheckPath(strFileName), FReadOnlyColour);
End;

(**

  This method is called at the end of the nothing to do process.

  @precon  None.
  @postcon Does not do anything.

  @nocheck EmptyMethod

**)
Procedure TCommandLineProcessing.NothingToDoEnd;

Begin
  // Do nothing
End;

(**

  This method is called at the start of the nothing to do process.

  @precon  None.
  @postcon Outputs the number of files marked as nothing to do.

  @param   iFileCount as an Integer as a constant

**)
Procedure TCommandLineProcessing.NothingToDoStart(Const iFileCount: Integer);

Begin
  If iFileCount > 0 Then
    FConsole.OutputToConsoleLn(coStd, Format('%1.0n Files with Nothing to do...', [Int(iFileCount)]
        ), FHeaderColour);
End;

(**

  This method outputs a consistent file number / total number format to the console.

  @precon  None.
  @postcon Outputs a consistent file number / total number format to the console.

  @param   iCurrentFile as an Integer as a constant
  @param   iTotal       as an Integer as a constant

**)
Procedure TCommandLineProcessing.OutputFileNumber(Const iCurrentFile, iTotal : Integer);

Var
  strTotal : String;
  iSize : Integer;

Begin
  strTotal := Format('%1.0n', [Int(iTotal)]);
  iSize    := Length(strTotal);
  FConsole.OutputToConsole(coStd, #32#32 + Format('(%*.0n/%*.0n) ',
      [iSize, Int(iCurrentFile), iSize, Int(iTotal)]));
End;

(**

  This method outputs the statistic for the synchronisation job.

  @precon  CFC must be a valid instance.
  @postcon Outputs the statistics to the console.

  @param   CFC as a TCompareFoldersCollection as a constant

**)
Procedure TCommandLineProcessing.OutputStats(Const CFC : TCompareFoldersCollection);

Var
  i : TFileOpStat;
  iDrive : Integer;
  strHeader : String;
  C : Char;
  iMaxDriveWidth : Integer;

Begin
  FConsole.OutputToConsoleLn(coStd, 'Statistics:', FHeaderColour);
  For i := Low(TFileOpStat) To High(TFileOpStat) Do
    Begin
      FConsole.OutputToConsoleLn(coStd, Format('  %-20s: %1.0n files in %1.1n kbytes', [
        CFC.Statistics[i].FName,
        Int(CFC.Statistics[i].FCount),
        Int(CFC.Statistics[i].FSize / 1024.0)
      ]));
    End;
  FConsole.OutputToConsoleLn(coStd, 'Drive Space (in kbytes):',
    FHeaderColour);
  iMaxDriveWidth := 0;
  For iDrive := 0 To CFC.Drives.Count -  1 Do
    If Length(CFC.Drives.Drive[iDrive].Drive) > iMaxDriveWidth Then
      iMaxDriveWidth := Length(CFC.Drives.Drive[iDrive].Drive);
  strHeader := Format(' %-*s | %16s | %16s | %16s | %16s | %16s ', [
    iMaxDriveWidth + 2,
    'Drive',
    'Total',
    'Free at Start',
    'Total Deletes',
    'Total Adds',
    'Free at Finish'
  ]);
  FConsole.OutputToConsoleLn(coStd, strHeader);
  FConsole.OutputToConsoleLn(coStd, StringOfChar('-', Length(strHeader)));
  For iDrive := 0 To CFC.Drives.Count -  1 Do
    Begin
      FConsole.OutputToConsoleLn(coStd, Format(' %-*s | %16.1n | %16.1n | %16.1n | %16.1n | %16.1n', [
        iMaxDriveWidth + 2,
        CFC.Drives.Drive[iDrive].Drive,
        Int(CFC.Drives.Drive[iDrive].Total) / 1024,
        Int(CFC.Drives.Drive[iDrive].FreeAtStart) / 1024,
        Int(CFC.Drives.Drive[iDrive].TotalDeletes) / 1024,
        Int(CFC.Drives.Drive[iDrive].TotalAdds) / 1024,
        Int(CFC.Drives.Drive[iDrive].FreeAtFinish) / 1024
      ]));
    End;
  FConsole.OutputToConsoleLn(coStd);
  If Not (clsProceedAutomatically In FCommandLineOptions.iCommandLineOptions) Then
    Begin
      FConsole.OutputToConsole(coStd, 'Do you want to proceed (Y/N)? ', FInputColour);
      C := GetConsoleCharacter(['y', 'Y', 'n', 'N']);
      FConsole.OutputToConsole(coStd, C, FInputColour);
      FConsole.OutputToConsoleLn(coStd);
      FConsole.OutputToConsoleLn(coStd);
      Case C Of
        'n', 'N': Abort;
      End;
    End;
End;

(**

  This method outputs an exception message to the console along with its call stack.

  @precon  None.
  @postcon An exception message and call stack are output to the console.

  @param   E as an Exception as a constant

**)
Procedure TCommandLineProcessing.RaiseFldrSyncException(Const E : Exception);

Var
  CS: TEurekaBaseStackList;
  i : Integer;
  L:  TELLocationInfo;

Begin
  FConsole.OutputToConsoleLn(coErr, #13#10 + E.ClassName + ': ' + E.Message, clRed);
  CS := ExceptionManager.LastThreadException.CallStack;
  FConsole.OutputToConsoleLn(coErr, 'CALL STACK', clRed);
  FConsole.OutputToConsoleLn(coErr, '  -------------------------------------------------------------------------------------------------------', clRed);
  FConsole.OutputToConsoleLn(coErr, '   Line Col  Unit Name                 Class Name                Method Name', clRed);
  FConsole.OutputToConsoleLn(coErr, '  -------------------------------------------------------------------------------------------------------', clRed);
  For i := 0 To CS.Count - 1 Do
    Begin
      L := CS.Item[i].Location;
      If CS.Item[i].ThreadName = 'Main thread' Then
        FConsole.OutputToConsoleLn(coErr, Format('  %5d[%3d] %-25s %-25s %-25s', [
          L.LineNumber,
          L.OffsetFromLineNumber,
          UnitName,
          ClassName,
          L.ProcedureName
        ]), clRed);
    End;
  FConsole.OutputToConsoleLn(coErr, '', clRed);
End;

(**

  This method saves the applications settings to the INI file.

  @precon  None.
  @postcon The applications settings are saved to the INI file.

**)
Procedure TCommandLineProcessing.SaveSettings;

Var
  iniMemFile: TMemIniFile;

Begin
  iniMemFile := TMemINIFile.Create(FINIFileName);
  Try
    iniMemFile.WriteString('Colours', 'Title', ColorToString(FTitleColour));
    iniMemFile.WriteString('Colours', 'Path', ColorToString(FPathColour));
    iniMemFile.WriteString('Colours', 'Success', ColorToString(FSuccessColour));
    iniMemFile.WriteString('Colours', 'Exists', ColorToString(FExistsColour));
    iniMemFile.WriteString('Colours', 'NotExists', ColorToString(FNotExistsColour));
    iniMemFile.WriteString('Colours', 'Input', ColorToString(FInputColour));
    iniMemFile.WriteString('Colours', 'ReadOnly', ColorToString(FReadOnlyColour));
    iniMemFile.WriteString('Colours', 'Exception', ColorToString(FExceptionColour));
    iniMemFile.WriteString('Colours', 'Header', ColorToString(FHeaderColour));
    iniMemFile.WriteInteger('Setup', 'OutputUpdateInterval', FOutputUpdateInterval);
  Finally
    iniMemFile.Free;
  End;
End;

(**

  This method is called for each file found in the search folder.

  @precon  None.
  @postcon Output the number of files and their size found in the search.

  @param   iFileCount as an Integer as a constant
  @param   iTotalSize as an Int64 as a constant

**)
Procedure TCommandLineProcessing.SearchEndProc(Const iFileCount: Integer; Const iTotalSize: Int64);

Begin
  ClearLine;
  FConsole.OutputToConsoleLn(coStd, Format('%1.0n files in %1.0n bytes.',
      [Int(iFileCount), Int(iTotalSize)]), FSuccessColour);
End;

(**

  This method is called for each file found in the search.

  @precon  None.
  @postcon Outputs periodically the number and name of the file found.

  @param   strFolder   as a String as a constant
  @param   strFileName as a String as a constant
  @param   iCount      as an Integer as a constant
  @param   Update      as a TUpdateType as a constant

**)
Procedure TCommandLineProcessing.SearchProc(Const strFolder, strFileName: String;
  Const iCount: Integer; Const Update : TUpdateType);
var
  strOutput: String;

Begin
  If strFolder = FLastFolder Then
    Begin
      If Not(cloQuiet In FCommandLineOptions.iCommandLineOptions) Then
        If (iCount Mod FOutputUpdateInterval = 0) Or (Update = utImmediate) Then
          Begin
            ClearLine;
            strOutput := Format('%1.0n\%s', [Int(iCount), strFileName]);
            FConsole.OutputToConsole(coStd, CheckPath(strOutput), clNone, clNone, False);
          End;
    End
  Else
    FLastFolder := strFolder;
End;

(**

  This method is called at the start of the file searching process.

  @precon  None.
  @postcon Outputs the folder that is being searched.

  @param   strFolder as a String as a constant

**)
Procedure TCommandLineProcessing.SearchStartProc(Const strFolder: String);

Begin
  FConsole.OutputToConsole(coStd, 'Searching: ', FHeaderColour);
  FConsole.OutputToConsole(coStd, Format('%s', [strFolder]), FPathColour);
End;

End.
