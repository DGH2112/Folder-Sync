(**

  This module defines a class to represent a progress dialogue for use
  throughout this application.

  @Date    08 Apr 2004
  @Version 1.0
  @Author  David Hoyle

**)
unit ProgressForm;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  DGHProgress, ExtCtrls, StdCtrls;

type
  (** The class to define the progress form **)
  TfrmProgress = class(TForm)
    lblPath: TLabel;
    lblCount: TLabel;
    lblFile: TLabel;
  private
    { Private declarations }
  public
    { Public declarations }
    Procedure Progress(boolShow : Boolean; strPath, strFile : String;
      iCount : Integer);
  end;

implementation

{$R *.DFM}

{ TfrmProgress }

(**

  This method updates the form interface with the progress passed.

  @precon  None.
  @postcon Updates the form interface with the progress passed.

  @param   boolShow as a Boolean
  @param   strPath  as a String
  @param   strFile  as a String
  @param   iCount   as an Integer

**)
procedure TfrmProgress.Progress(boolShow: Boolean; strPath,
  strFile: String; iCount: Integer);
begin
  If boolShow <> Visible Then Visible := boolShow;
  If strPath <> lblPath.Caption Then lblPath.Caption := strPath;
  If strFile <> lblFile.Caption Then lblFile.Caption := strFile;
  lblCount.Caption := Format('%1.0n', [iCount + 0.1]);
  Application.ProcessMessages;
end;

end.
