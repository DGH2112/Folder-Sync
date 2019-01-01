(**

  This module contains a command line application to synchronsise 2 folders files.

  @Version 2.0
  @Author  David Hoyle
  @Date    01 Jan 2019

**)
Program FldrSyncCmd;

{$APPTYPE CONSOLE}

{$R *.RES}

{$R 'ITHelperVersionInfoCmd.res' 'ITHelperVersionInfoCmd.RC'}

uses
  {$IFDEF EurekaLog}
  EMemLeaks,
  EResLeaks,
  ESendMailMAPI,
  ESendMailSMAPI,
  EDialogConsole,
  EDebugExports,
  EDebugJCL,
  EMapWin32,
  EAppConsole,
  ExceptionLog7,
  {$ENDIF EurekaLog}
  EExceptionManager,
  SysUtils,
  Windows,
  Graphics,
  CommandLineProcess in 'Source\CommandLineProcess.pas',
  dghlibrary in 'Externals\dghlibrary.pas',
  SyncModule in 'Source\SyncModule.pas',
  ApplicationFunctions in 'Source\ApplicationFunctions.pas';

ResourceString
  (** A resource string for the pause before finishing **)
  strPleasePressEnter = 'Please press <Enter> to finish...';

Var
  (** iStd and iErr are handles for the console. **)
  iStd, iErr : THandle;
  (** A variable to hold whether the console should pause before exiting. **)
  boolPause : Boolean;
  (** A variable to old an instance of the indexing engine. **)
  CommandLineProcessing : TCommandLineProcessing;

Begin
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
      RaiseFldrSyncException(iErr, E);
  End;
  If boolPause Then
    Begin
      OutputToConsoleLn(iErr, #13#10 + strPleasePressEnter);
      ReadLn;
    End;
End.
