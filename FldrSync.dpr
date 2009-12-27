(**
  
  The main delphi module for the Folder Synchronisation application.

  @Author  David Hoyle
  @Date    27 Dec 2009
  @Version 1.0

**)
program FldrSync;



uses
  ExceptionLog,
  Forms,
  MainForm in 'Source\MainForm.pas' {frmMainForm},
  FileComparision in 'Source\FileComparision.pas',
  OptionsForm in 'Source\OptionsForm.pas' {frmOptions},
  ProgressForm in 'Source\ProgressForm.pas' {frmProgress},
  FolderPathsForm in 'Source\FolderPathsForm.pas' {frmFolderPaths},
  About in '..\..\LIBRARY\About.pas' {frmAbout},
  dghlibrary in '..\..\LIBRARY\dghlibrary.pas',
  checkforupdates in '..\..\LIBRARY\checkforupdates.pas',
  CheckForUpdatesForm in '..\..\LIBRARY\CheckForUpdatesForm.pas' {frmCheckForUpdates},
  DGHSpectrum in '..\..\Components\Source\DGHSpectrum.pas',
  MSXML2_TLB in '..\..\LIBRARY\MSXML2_TLB.pas',
  DGHEllipsisLabel in '..\..\components\source\DGHEllipsisLabel.pas';

{$R *.RES}

begin
  Application.Initialize;
  ReportMemoryLeaksOnShutdown := DebugHook <> 0;
  SetEurekaLogState(DebugHook = 0);
  Application.Title := 'Folder Sync';
  Application.CreateForm(TfrmMainForm, frmMainForm);
  Application.Run;
end.
