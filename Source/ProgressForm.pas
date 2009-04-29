(**
  
  This module represents a form for displaying progress.

  @Version 1.0
  @Date    29 Apr 2009
  @Author  David Hoyle

**)
unit ProgressForm;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls, StdCtrls, DGHEllipsisLabel;

type
  (** A class to represents a form for displaying progress. **)
  TfrmProgress = class(TForm)
    pnlInfo: TPanel;
    lblMessage: TDGHEllipsisLabel;
    lblFileName: TDGHEllipsisLabel;
  private
    { Private declarations }
  public
    { Public declarations }
    Procedure Progress(strMessage : String; iCount : Integer; strFileName : String);
  end;

implementation

{$R *.DFM}

{ TfrmProgress }

(**

  This is the forms progress method that update the progress within the dialogue
  .

  @precon  None.
  @postcon Update the dialogues progress.

  @param   strMessage  as a String
  @param   iCount      as an Integer
  @param   strFileName as a String

**)
procedure TfrmProgress.Progress(strMessage : String; iCount : Integer; strFileName : String);

begin
  If iCount Mod 100 = 0 Then
    Begin
      lblMessage.Caption := Format('%s... (%d)', [strMessage, iCount]);
      lblFileName.Caption := Format('%s', [strFileName]);
      Application.ProcessMessages;
    End;
end;

end.
