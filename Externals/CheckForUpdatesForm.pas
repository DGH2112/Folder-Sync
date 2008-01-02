(**
  
  This module contains a clas which represents a form for displaying software
  update information using a Singleton Form.

  @Version 1.0
  @Author  David Hoyle
  @Date    02 Jan 2008

**)
unit CheckForUpdatesForm;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Buttons;

type
  (** This class represents for the form interface. **)
  TfrmCheckForUpdates = class(TForm)
    lbInformation: TListBox;
    btnOK: TBitBtn;
    procedure lbInformationDrawItem(Control: TWinControl; Index: Integer;
      Rect: TRect; State: TOwnerDrawState);
  private
    { Private declarations }
  public
    { Public declarations }
    Class Procedure ShowUpdates(strMsg : String; iColour : TColor);
    Class Procedure Stop;
    Class Procedure HideUpdates;
  end;

implementation

{$R *.dfm}

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

begin
  With (Control As TListBox).Canvas Do
    Begin
      FillRect(Rect);
      iColour := Integer(lbInformation.Items.Objects[Index]);
      If iColour <> clNone Then
        Font.Color := iColour;
      TextOut(Rect.Left + 2, Rect.Top, (Control as TListBox).Items[Index]);
    End;
end;

(**

  This method displays the message window and adds the message to the listbox.

  @precon  None.
  @postcon Displays the message window and adds the message to the listbox.

  @param   strMsg  as a String
  @param   iColour as a TColor

**)
class procedure TfrmCheckForUpdates.ShowUpdates(strMsg: String; iColour: TColor);
begin
  If frm = Nil Then
    frm := TfrmCheckForUpdates.Create(Nil);
  With frm Do
    Begin
      Show;
      lbInformation.Items.AddObject(strMsg, TObject(iColour));
      lbInformation.ItemIndex := lbInformation.Items.Count - 1;
    End;
  Application.ProcessMessages;
end;

(**

  This method shows the dialogue in a modal form so the user can see that there
  is information to be read.

  @precon  None.
  @postcon Shows the dialogue in a modal form so the user can see that there
           is information to be read.

**)
class procedure TfrmCheckForUpdates.Stop;
begin
  frm.btnOK.Enabled := True;
  frm.Hide;
  frm.ShowModal;
end;

end.
