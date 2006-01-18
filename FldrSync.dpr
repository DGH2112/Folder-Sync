(**
  
  The main delphi module for the Folder Synchronisation application.

  @Author  David Hoyle
  @Date    30 Dec 2005
  @Version 1.0
  
**)
program FldrSync;

{%TogetherDiagram 'ModelSupport_FldrSync\default.txaPackage'}
{%TogetherDiagram 'ModelSupport_FldrSync\MainForm\default.txaPackage'}
{%TogetherDiagram 'ModelSupport_FldrSync\About\default.txaPackage'}
{%TogetherDiagram 'ModelSupport_FldrSync\FileComparision\default.txaPackage'}
{%TogetherDiagram 'ModelSupport_FldrSync\ProgressForm\default.txaPackage'}
{%TogetherDiagram 'ModelSupport_FldrSync\FolderPathsForm\default.txaPackage'}
{%TogetherDiagram 'ModelSupport_FldrSync\FldrSync\default.txaPackage'}
{%TogetherDiagram 'ModelSupport_FldrSync\OptionsForm\default.txaPackage'}
{%TogetherDiagram 'ModelSupport_FldrSync\default.txvpck'}
{%TogetherDiagram 'ModelSupport_FldrSync\FolderPathsForm\default.txvpck'}
{%TogetherDiagram 'ModelSupport_FldrSync\MainForm\default.txvpck'}
{%TogetherDiagram 'ModelSupport_FldrSync\FldrSync\default.txvpck'}
{%TogetherDiagram 'ModelSupport_FldrSync\FileComparision\default.txvpck'}
{%TogetherDiagram 'ModelSupport_FldrSync\About\default.txvpck'}
{%TogetherDiagram 'ModelSupport_FldrSync\ProgressForm\default.txvpck'}
{%TogetherDiagram 'ModelSupport_FldrSync\OptionsForm\default.txvpck'}

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
