{:  @stopdocumentation }
program FldrSyncTests;

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
  ProgressForm in '..\Source\ProgressForm.pas' {frmProgress},
  FolderPathsForm in '..\Source\FolderPathsForm.pas' {frmFolderPaths},
  FldrSync.Functions in '..\Source\FldrSync.Functions.pas';

{$R *.RES}

begin
  RunRegisteredTests;
end.
