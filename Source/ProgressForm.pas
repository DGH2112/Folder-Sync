(**

  This module represents a form for displaying progress.

  @Version 1.0
  @Date    12 Jul 2009
  @Author  David Hoyle

**)
unit ProgressForm;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls, StdCtrls, DGHEllipsisLabel, ComCtrls;

type
  (** A class to represents a form for displaying progress. **)
  TfrmProgress = class(TForm)
    pnlInfo: TPanel;
    lblMessage: TDGHEllipsisLabel;
    lblFileName: TDGHEllipsisLabel;
    pbrProgress: TProgressBar;
  private
    { Private declarations }
  public
    { Public declarations }
    Procedure Progress(strMessage : String; iCount : Integer;
      strFileName : String); Overload;
    Procedure Progress(iPosition : Integer); Overload;
    Procedure RegisterSections(iSections : Integer); Virtual;
  end;

  (** A event method for feeding back progress messages. **)
  TProgressMsgProc = Procedure(strMessage : String; iCount : Integer;
    strFile : String) Of Object;
  (** A event method for feeding back progress position. **)
  TProgressPosProc = Procedure(iPosition : Integer) Of Object;

implementation

{$R *.DFM}

{ TfrmProgress }

(**

  This method updates the position of the progress bar.

  @precon  Position must be between 0 and Max inclusive.
  @postcon Updates the position of the progress bar.

  @param   iPosition as an Integer

**)
procedure TfrmProgress.Progress(iPosition: Integer);

begin
  pbrprogress.Position := iPosition;
  Application.ProcessMessages;
end;

(**

  This method registers the number of sections in the progres dialogue, i.e.
  folder pairs in the search.

  @precon  None.
  @postcon Registers the number of sections in the progres dialogue, i.e.
           folder pairs in the search.

  @param   iSections as an Integer

**)
Procedure TfrmProgress.RegisterSections(iSections : Integer);
begin
  pbrProgress.Position := 0;
  pbrProgress.Max := iSections;
end;

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
