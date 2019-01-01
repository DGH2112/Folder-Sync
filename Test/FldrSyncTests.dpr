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
  TestInsight.DUnit,
  TestFileComparision in 'Source\TestFileComparision.pas',
  SyncModule in '..\Source\SyncModule.pas',
  TestDGHLibrary in '..\Externals\TestDGHLibrary.pas',
  dghlibrary in '..\Externals\dghlibrary.pas',
  ProgressForm in '..\Source\ProgressForm.pas' {frmProgress},
  DGHEllipsisLabel in '..\Externals\DGHEllipsisLabel.pas',
  FolderPathsForm in '..\Source\FolderPathsForm.pas' {frmFolderPaths},
  CheckForUpdatesOptionsForm in '..\Externals\CheckForUpdatesOptionsForm.pas' {frmCheckForUpdatesOptions};

{$R *.RES}

begin
  RunRegisteredTests;
end.
