(**
  
  The main delphi module for the Folder Synchronisation application.

  @Author  David Hoyle
  @Date    01 Jan 2019
  @Version 2.0

  @nocheck HardCodedString

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
  FileCopyProgressForm in 'Source\FileCopyProgressForm.pas' {frmCopyProgress},
  DGHMemoryMonitorControl in 'Externals\DGHMemoryMonitorControl.pas',
  DGHCustomGraphicsControl in 'Externals\DGHCustomGraphicsControl.pas',
  FileDeleteProgressForm in 'Source\FileDeleteProgressForm.pas' {frmDeleteProgress},
  ConfirmationDlg in 'Source\ConfirmationDlg.pas' {frmConfirmationDlg},
  Vcl.Themes,
  Vcl.Styles,
  AboutForm in 'Source\AboutForm.pas' {frmAboutDialogue},
  MemoryMonitorOptionsForm in 'Externals\MemoryMonitorOptionsForm.pas' {frmBatteryMonitorOptions},
  DGHNumericEdit in 'Externals\DGHNumericEdit.pas',
  InterfaceFontForm in 'Source\InterfaceFontForm.pas' {frmInterfaceFonts},
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
