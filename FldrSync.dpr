(**
  
  The main delphi module for the Folder Synchronisation application.

  @Author  David Hoyle
  @Date    30 Dec 2005
  @Version 1.0
  
**)
program FldrSync;

uses
  Forms,
  MainForm in 'Source\MainForm.pas' {frmMainForm},
  FileComparision in 'Source\FileComparision.pas',
  OptionsForm in 'Source\OptionsForm.pas' {frmOptions},
  ProgressForm in 'Source\ProgressForm.pas' {frmProgress},
  FolderPathsForm in 'Source\FolderPathsForm.pas' {frmFolderPaths},
  About in '..\..\LIBRARY\About.pas' {frmAbout};

{$R *.RES}

begin
  Application.Initialize;
  Application.Title := 'Folder Sync';
  Application.CreateForm(TfrmMainForm, frmMainForm);
  Application.Run;
end.
