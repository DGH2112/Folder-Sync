(**

  This module contains a clas which represents a form for displaying software
  update information using a Singleton Form.

  @Version 1.0
  @Author  David Hoyle
  @Date    11 Jul 2009

**)
unit CheckForUpdatesForm;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Buttons, ExtCtrls;

type
  (** This class represents for the form interface. **)
  TfrmCheckForUpdates = class(TForm)
    lbInformation: TListBox;
    btnOK: TBitBtn;
    tmFinish: TTimer;
    lblWebSite: TLabel;
    procedure lbInformationDrawItem(Control: TWinControl; Index: Integer;
      Rect: TRect; State: TOwnerDrawState);
    procedure lbInformationMeasureItem(Control: TWinControl; Index: Integer;
      var Height: Integer);
    procedure tmFinishTimer(Sender: TObject);
    procedure btnOKClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure lblWebSiteClick(Sender: TObject);
    procedure lblWebSiteMouseEnter(Sender: TObject);
    procedure lblWebSiteMouseLeave(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    Class Procedure ShowUpdates(strMsg, strURL : String; iColour : TColor);
    Class Procedure Finish(iSeconds : Integer);
    Class Procedure HideUpdates;
  end;

implementation

{$R *.dfm}

Uses
  ShellAPI;

Var
  (** This is an private variable for the Singleton Form reference. **)
  frm : TfrmCheckForUpdates;

{ TfrmCheckForUpdatesForm }

(**

  This method frees the nils the form variable if the form is valid.

  @precon  None.
  @postcon Frees and Nils the form.

**)
class procedure TfrmCheckForUpdates.HideUpdates;
begin
  If Assigned(frm) Then
    FreeAndNil(frm);
end;

(**

  This is an on Draw Item event handler for the list box control.

  @precon  None.
  @postcon Renders the text with the assigned colour (in .Objects[]).

  @param   Control as a TWinControl
  @param   Index   as an Integer
  @param   Rect    as a TRect
  @param   State   as a TOwnerDrawState

**)
procedure TfrmCheckForUpdates.lbInformationDrawItem(Control: TWinControl;
  Index: Integer; Rect: TRect; State: TOwnerDrawState);

Var
  iColour : TColor;
  strText : String;
  R : TRect;

begin
  With (Control As TListBox).Canvas Do
    Begin
      iColour := Integer(lbInformation.Items.Objects[Index]);
      Brush.Color := clBlack;
      If iColour <> clNone Then
        Font.Color := iColour;
      FillRect(Rect);
      strText := Trim((Control As TListBox).Items[Index]);
      R := (Control As TListBox).ItemRect(Index);
      Inc(R.Left, 4);
      Dec(R.Right, 4);
      DrawText((Control As TListBox).Canvas.Handle, PChar(strText), Length(strText), R,
        DT_LEFT Or DT_NOCLIP Or DT_NOPREFIX Or DT_WORDBREAK);
    End;
end;

(**

  This is an on Measure Item event handler for the list box control.

  @precon  None.
  @postcon Adjusts the height of the item if the text needs multiple lines.

  @param   Control as a TWinControl
  @param   Index   as an Integer
  @param   Height  as an Integer as a reference

**)
procedure TfrmCheckForUpdates.lbInformationMeasureItem(Control: TWinControl;
  Index: Integer; var Height: Integer);

Var
  strText : String;
  R : TRect;

begin
  strText := Trim((Control As TListBox).Items[Index]);
  R := (Control As TListBox).ItemRect(Index);
  Inc(R.Left, 4);
  Dec(R.Right, 4);
  DrawText((Control As TListBox).Canvas.Handle, PChar(strText), Length(strText), R,
    DT_CALCRECT Or DT_LEFT Or DT_NOCLIP Or DT_NOPREFIX Or DT_WORDBREAK);
  If R.Bottom - R.Top > Height Then
    Height := R.Bottom - R.Top + 3;
end;

(**

  This is an on click event handler for the WebSite label.

  @precon  None.
  @postcon Opens the URL on the label.

  @param   Sender as a TObject

**)
procedure TfrmCheckForUpdates.lblWebSiteClick(Sender: TObject);
begin
  ShellExecute(Application.Handle, 'OPEN', PChar(lblWebSite.Caption),
    '', '', SW_SHOWNORMAL);
end;

(**

  This is an on mouse enter event handler for the web site label.

  @precon  None.
  @postcon Adds the underlining to the label.

  @param   Sender as a TObject

**)
procedure TfrmCheckForUpdates.lblWebSiteMouseEnter(Sender: TObject);
begin
  lblWebSite.Font.Style := lblWebSite.Font.Style + [fsUnderline];
end;

(**

  This is an on mouse leave event handler for the web site label.

  @precon  None.
  @postcon Remove the underlining from the label.

  @param   Sender as a TObject

**)
procedure TfrmCheckForUpdates.lblWebSiteMouseLeave(Sender: TObject);
begin
  lblWebSite.Font.Style := lblWebSite.Font.Style - [fsUnderline];
end;

(**

  This method displays the message window and adds the message to the listbox.

  @precon  None.
  @postcon Displays the message window and adds the message to the listbox.

  @param   strMsg  as a String
  @param   strURL  as a String
  @param   iColour as a TColor

**)
class procedure TfrmCheckForUpdates.ShowUpdates(strMsg, strURL: String;
  iColour: TColor);

Var
  sl : TStringList;
  i : Integer;

begin
  If frm = Nil Then
    frm := TfrmCheckForUpdates.Create(Nil);
  With frm Do
    Begin
      Show;
      sl := TStringList.Create;
      Try
        sl.Text := strMsg;
        For i := 0 To Sl.Count - 1 Do
          If sl[i] <> '' Then
            lbInformation.Items.AddObject(sl[i], TObject(iColour));
      Finally
        sl.Free;
      End;
      lbInformation.ItemIndex := lbInformation.Items.Count - 1;
      lblWebSite.Caption := strURL;
    End;
  Application.ProcessMessages;
end;

(**

  This is an on timer event handler for the Timer control.

  @precon  None.
  @postcon Closes the dialogue.

  @param   Sender as a TObject

**)
procedure TfrmCheckForUpdates.tmFinishTimer(Sender: TObject);
begin
  Close;
end;

(**

  This is an on click event handler for the OK button.

  @precon  None.
  @postcon Closes the dialogue.

  @param   Sender as a TObject

**)
procedure TfrmCheckForUpdates.btnOKClick(Sender: TObject);
begin
  Close;
end;

(**

  This method shows the dialogue in a modal form so the user can see that there
  is information to be read.

  @precon  None.
  @postcon Shows the dialogue in a modal form so the user can see that there
           is information to be read.

  @param   iSeconds as an Integer

**)
class procedure TfrmCheckForUpdates.Finish(iSeconds : Integer);
begin
  frm.btnOK.Enabled := True;
  frm.tmFinish.Interval := iSeconds * 1000;
  frm.tmFinish.Enabled := True;
end;

(**

  This is an on close event handler for the form.

  @precon  None.
  @postcon Disables the timer.

  @param   Sender as a TObject
  @param   Action as a TCloseAction as a reference

**)
procedure TfrmCheckForUpdates.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
  tmFinish.Enabled := False;
end;

(** Nothing the intialise. **)
Initialization
(** Make sure the form is freed is its been created. **)
Finalization
  If frm <> Nil Then
    frm.Free;
end.

