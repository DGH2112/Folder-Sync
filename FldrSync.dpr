(**
  
  The main delphi module for the Folder Synchronisation application.

  @Author  David Hoyle
  @Date    02 Oct 2004
  @Version 1.0
  
**)
program FldrSync;

uses
  Forms,
  MainForm in 'MainForm.pas' {frmMainForm},
  FileComparision in 'FileComparision.pas',
  OptionsForm in 'OptionsForm.pas' {frmOptions},
  ProgressForm in 'ProgressForm.pas' {frmProgress},
  FolderPathsForm in 'FolderPathsForm.pas' {frmFolderPaths};

{$R *.RES}

begin
  Application.Initialize;
  Application.Title := 'Folder Sync';
  Application.CreateForm(TfrmMainForm, frmMainForm);
  Application.Run;
end.
