(**

  This module contains a command line application to synchronsise 2 folders files.

  @Version 1.5
  @Author  David Hoyle
  @Date    01 Aug 2012

**)
Program FldrSyncCmd;

{$APPTYPE CONSOLE}

{$R *.RES}

{$R 'ITHelperVersionInfoCmd.res' 'ITHelperVersionInfoCmd.RC'}

uses
  ExceptionLog,
  SysUtils,
  Windows,
  Graphics,
  CommandLineProcess in 'Source\CommandLineProcess.pas',
  dghlibrary in '..\..\LIBRARY\dghlibrary.pas',
  checkforupdates in '..\..\LIBRARY\checkforupdates.pas',
  SyncModule in 'Source\SyncModule.pas';

Var
  (** iStd and iErr are handles for the console. **)
  iStd, iErr : THandle;
  (** A variable to hold whether the console should pause before exiting. **)
  boolPause : Boolean;
  (** A variable to old an instance of the indexing engine. **)
  CommandLineProcessing : TCommandLineProcessing;

Begin
  {$IFDEF EUREKALOG}
  SetEurekaLogState(Not IsDebuggerPresent);
  {$ENDIF}
  ReportMemoryLeaksOnShutdown := IsDebuggerPresent;
  boolPause := True;
  iStd := GetStdHandle(STD_OUTPUT_HANDLE);
  iErr := GetStdHandle(STD_ERROR_HANDLE);
  Try
    CommandLineProcessing := TCommandLineProcessing.Create;
    Try
      CommandLineProcessing.Execute(iErr, iStd);
      boolPause := CommandLineProcessing.Pause;
    Finally
      CommandLineProcessing.Free;
    End;
  Except
    On E: EFldrSyncException Do
      OutputToConsoleLn(iErr, #13#10 + E.ClassName + ': ' + E.Message, clRed);
  End;
  If boolPause Then
    Begin
      OutputToConsoleLn(iErr, #13#10'Please press <Enter> to finish...');
      ReadLn;
    End;
End.
