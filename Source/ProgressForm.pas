(**
  
  This module represents a form for displaying progress.

  @Version 1.0
  @Date    02 Oct 2004
  @Author  David Hoyle
  
**)
unit ProgressForm;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls, StdCtrls;

type
  (** A class to represents a form for displaying progress. **)
  TfrmProgress = class(TForm)
    pnlInfo: TPanel;
    lblInfo: TLabel;
  private
    { Private declarations }
  public
    { Public declarations }
    Procedure Progress(iCount : Integer; strPath, strFileName : String);
  end;

implementation

{$R *.DFM}

{ TfrmProgress }

(**

  This is the forms progress method that update the progress within the
  dialogue.

  @precon  None.
  @postcon Update the dialogues progress.

  @param   iCount      as an Integer
  @param   strPath     as a String
  @param   strFileName as a String

**)
procedure TfrmProgress.Progress(iCount : Integer; strPath, strFileName: String);

Const
  strMask = 'Scanning file (%d):'#13'%s'#13'%s';

begin
  lblInfo.Caption := Format(strMask, [iCount, strPath, strFileName]);
  If iCount Mod 100 = 0 Then Application.ProcessMessages;
end;

end.
