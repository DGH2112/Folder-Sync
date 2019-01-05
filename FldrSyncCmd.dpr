(**

  This module contains a command line application to synchronsise 2 folders files.

  @Version 2.0
  @Author  David Hoyle
  @Date    02 Jan 2019

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
  SyncModule in 'Source\SyncModule.pas',
  ApplicationFunctions in 'Source\ApplicationFunctions.pas',
  FldrSync.Functions in 'Source\FldrSync.Functions.pas',
  FldrSync.Console in 'Source\FldrSync.Console.pas';

ResourceString
  (** A resource string for the pause before finishing **)
  strPleasePressEnter = 'Please press <Enter> to finish...';

Var
  (** A variable to hold whether the console should pause before exiting. **)
  boolPause : Boolean;
  (** A variable to old an instance of the indexing engine. **)
  CommandLineProcessing : TCommandLineProcessing;

Begin
  boolPause := True;
  CommandLineProcessing := TCommandLineProcessing.Create;
  Try
    Try
      CommandLineProcessing.Execute;
      boolPause := CommandLineProcessing.Pause;
    Except
      On E: EFldrSyncException Do
        CommandLineProcessing.RaiseFldrSyncException(E);
    End;
  Finally
    CommandLineProcessing.Free;
  End;
  If boolPause Then
    Begin
      WriteLn(#13#10 + strPleasePressEnter);
      ReadLn;
    End;
End.
