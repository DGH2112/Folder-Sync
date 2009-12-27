(**

  This module represents a form for displaying progress.

  @Version 1.0
  @Date    27 Dec 2009
  @Author  David Hoyle

**)
unit ProgressForm;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls, StdCtrls, DGHEllipsisLabel, ComCtrls, Buttons;

type
  (** A class to represents a form for displaying progress. **)
  TfrmProgress = class(TForm)
    pnlInfo: TPanel;
    lblMessage: TDGHEllipsisLabel;
    lblFileName: TDGHEllipsisLabel;
    pbrProgress: TProgressBar;
    btnCancel: TBitBtn;
    procedure btnCancelClick(Sender: TObject);
  private
    { Private declarations }
    FAbort: Boolean;
    Procedure CheckForCancel;
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
  TProgressPosProc = Procedure(iPosition, iIndex : Integer) Of Object;

implementation

{$R *.DFM}

{ TfrmProgress }

(**

  This method checks for the cancellation of the process and confirms that is
  what is required.

  @precon  None.
  @postcon Checks for the cancellation of the process and confirms that is
           what is required.

**)
procedure TfrmProgress.CheckForCancel;

Const
  strMsg = 'Are you sure you want to cancel this operation?';
begin
  If FAbort Then
    Case MessageDlg(strMsg, mtConfirmation, [mbYes, mbNo], 0) Of
      mrYes: Abort;
      mrNo: FAbort := False;
    End;
end;

(**

  This method updates the position of the progress bar.

  @precon  Position must be between 0 and Max inclusive.
  @postcon Updates the position of the progress bar.

  @param   iPosition as an Integer

**)
procedure TfrmProgress.Progress(iPosition: Integer);

begin
  SetFocus;
  pbrprogress.Position := iPosition;
  Application.ProcessMessages;
  CheckForCancel;
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
  FAbort := False;
end;

(**

  This is an on click event handler for the Cancel button.

  @precon  None.
  @postcon Sets the cancellation of the processing.

  @param   Sender as a TObject

**)
procedure TfrmProgress.btnCancelClick(Sender: TObject);
begin
  FAbort := True;
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
  If iCount Mod 10 = 0 Then
    Begin
      SetFocus;
      lblMessage.Caption := Format('%s... (%d)', [strMessage, iCount]);
      lblFileName.Caption := Format('%s', [strFileName]);
      Application.ProcessMessages;
      CheckForCancel;
    End;
end;

end.
