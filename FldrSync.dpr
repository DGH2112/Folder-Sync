program FldrSync;

uses
  Forms,
  MainForm in 'MainForm.pas' {frmMainForm},
  ProgressForm in 'ProgressForm.pas' {frmProgress},
  FileComparision in 'FileComparision.pas',
  OptionsForm in 'OptionsForm.pas' {frmOptions};

{$R *.RES}

begin
  Application.Initialize;
  Application.Title := 'Folder Sync';
  Application.CreateForm(TfrmMainForm, frmMainForm);
  Application.Run;
end.
