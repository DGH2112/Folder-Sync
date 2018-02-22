{:  @stopdocumentation }
program FldrSyncTests;
{

  Delphi DUnit Test Project
  -------------------------
  This project contains the DUnit test framework and the GUI/Console test runners.
  Add "CONSOLE_TESTRUNNER" to the conditional defines entry in the project options
  to use the console test runner.  Otherwise the GUI test runner will be used by
  default.

}

{$IFDEF CONSOLE_TESTRUNNER}
{$APPTYPE CONSOLE}
{$ENDIF}

{$R 'ITHelperVersionInfo.res' 'ITHelperVersionInfo.RC'}

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
  //ExceptionLog,
  Forms,
  TestFramework,
  GUITestRunner,
  TextTestRunner,
  //JclDebug,
  TestFileComparision in 'Source\TestFileComparision.pas',
  SyncModule in '..\Source\SyncModule.pas',
  TestDGHLibrary in '..\Externals\TestDGHLibrary.pas',
  dghlibrary in '..\Externals\dghlibrary.pas',
  ProgressForm in '..\Source\ProgressForm.pas' {frmProgress},
  DGHEllipsisLabel in '..\Externals\DGHEllipsisLabel.pas',
  FolderPathsForm in '..\Source\FolderPathsForm.pas' {frmFolderPaths},
  CheckForUpdatesOptionsForm in '..\Externals\CheckForUpdatesOptionsForm.pas' {frmCheckForUpdatesOptions};

{$R *.RES}

Var
  T : TTestResult;
  iErrors : Integer;

begin
  iErrors := 0;
  Application.Initialize;
  if IsConsole then
    Begin
      T := TextTestRunner.RunRegisteredTests;
      Try
        iErrors := T.FailureCount + T.ErrorCount;
      Finally
        T.Free;
      End;
    End else
      GUITestRunner.RunRegisteredTests;
  If IsDebuggerPresent Then
    ReadLn;
  If iErrors > 0 Then
    Halt(iErrors);
end.
