(**
  
  The main delphi module for the Folder Synchronisation application.

  @Author  David Hoyle
  @Date    22 Feb 2018
  @Version 2.0

**)
program FldrSync;

{$R 'ITHelperVersionInfoGUI.res' 'ITHelperVersionInfoGUI.RC'}
{$R 'ApplicationSplashScreen.res' 'ApplicationSplashScreen.rc'}

uses
  {$IFDEF EurekaLog}
  EMemLeaks,
  EResLeaks,
  ESendMailMAPI,
  ESendMailSMAPI,
  EDialogWinAPIMSClassic,
  EDialogWinAPIEurekaLogDetailed,
  EDialogWinAPIStepsToReproduce,
  EDebugExports,
  EDebugJCL,
  EMapWin32,
  EAppVCL,
  ExceptionLog7,
  {$ENDIF EurekaLog}
  Forms,
  SysUtils,
  HTMLHelpViewer,
  MainForm in 'Source\MainForm.pas' {frmMainForm},
  SyncModule in 'Source\SyncModule.pas',
  OptionsForm in 'Source\OptionsForm.pas' {frmOptions},
  ProgressForm in 'Source\ProgressForm.pas' {frmProgress},
  FolderPathsForm in 'Source\FolderPathsForm.pas' {frmFolderPaths},
  dghlibrary in 'Externals\dghlibrary.pas',
  checkforupdates in 'Externals\checkforupdates.pas',
  CheckForUpdatesForm in 'Externals\CheckForUpdatesForm.pas' {frmCheckForUpdates},
  CheckForUpdatesOptionsForm in 'Externals\CheckForUpdatesOptionsForm.pas' {frmCheckForUpdatesOptions},
  FileCopyProgressForm in 'Source\FileCopyProgressForm.pas' {frmCopyProgress},
  DGHMemoryMonitorControl in 'Externals\DGHMemoryMonitorControl.pas',
  DGHCustomGraphicsControl in 'Externals\DGHCustomGraphicsControl.pas',
  FileDeleteProgressForm in 'Source\FileDeleteProgressForm.pas' {frmDeleteProgress},
  ConfirmationDlg in 'Source\ConfirmationDlg.pas' {frmConfirmationDlg},
  Vcl.Themes,
  Vcl.Styles,
  AboutForm in 'Source\AboutForm.pas' {frmAboutDialogue},
  DGHCustomLabel in 'Externals\DGHCustomLabel.pas',
  MemoryMonitorOptionsForm in 'Externals\MemoryMonitorOptionsForm.pas' {frmBatteryMonitorOptions},
  DGHNumericEdit in 'Externals\DGHNumericEdit.pas',
  InterfaceFontForm in 'Source\InterfaceFontForm.pas' {frmInterfaceFonts},
  Profiler in 'Externals\Profiler.pas',
  OperationsFontForm in 'Source\OperationsFontForm.pas' {frmOperationsFonts},
  DiskSpaceForm in 'Source\DiskSpaceForm.pas' {frmDiskSpace},
  ProcessingErrorForm in 'Source\ProcessingErrorForm.pas' {frmErrorDlg},
  ApplicationFunctions in 'Source\ApplicationFunctions.pas';

{$R *.RES}

begin
  Application.Initialize;
  Application.Title := 'Folder Sync';
  Application.CreateForm(TfrmMainForm, frmMainForm);
  Application.Run;
end.
