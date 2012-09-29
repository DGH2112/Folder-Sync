(**
  
  The main delphi module for the Folder Synchronisation application.

  @Author  David Hoyle
  @Date    17 Aug 2012
  @Version 1.0

**)
program FldrSync;

{$R 'ITHelperVersionInfoGUI.res' 'ITHelperVersionInfoGUI.RC'}

uses
  ExceptionLog,
  Forms,
  SysUtils,
  HTMLHelpViewer,
  MainForm in 'Source\MainForm.pas' {frmMainForm},
  SyncModule in 'Source\SyncModule.pas',
  OptionsForm in 'Source\OptionsForm.pas' {frmOptions},
  ProgressForm in 'Source\ProgressForm.pas' {frmProgress},
  FolderPathsForm in 'Source\FolderPathsForm.pas' {frmFolderPaths},
  About in '..\..\LIBRARY\About.pas' {frmAbout},
  dghlibrary in '..\..\LIBRARY\dghlibrary.pas',
  checkforupdates in '..\..\LIBRARY\checkforupdates.pas',
  CheckForUpdatesForm in '..\..\LIBRARY\CheckForUpdatesForm.pas' {frmCheckForUpdates},
  DGHSpectrum in '..\..\Components\Source\DGHSpectrum.pas',
  DGHEllipsisLabel in '..\..\components\source\DGHEllipsisLabel.pas',
  CheckForUpdatesOptionsForm in '..\..\Library\CheckForUpdatesOptionsForm.pas' {frmCheckForUpdatesOptions},
  FileCopyProgressForm in 'Source\FileCopyProgressForm.pas' {frmCopyProgress},
  DGHMemoryMonitorControl in '..\..\Components\Source\DGHMemoryMonitorControl.pas',
  DGHCustomGraphicsControl in '..\..\Components\Source\DGHCustomGraphicsControl.pas',
  FileDeleteProgressForm in 'Source\FileDeleteProgressForm.pas' {frmDeleteProgress},
  ConfirmationDlg in 'Source\ConfirmationDlg.pas' {frmConfirmationDlg};

{$R *.RES}

begin
  SetEurekaLogState(IsDebuggerPresent);
  Application.Initialize;
  Application.Title := 'Folder Sync';
  Application.CreateForm(TfrmMainForm, frmMainForm);
  Application.Run;
end.
