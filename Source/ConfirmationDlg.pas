(**

  This module contains a form for confirming a file action.

  @Author  David Hoyle
  @Version 1.0
  @Date    02 Aug 2012

**)
Unit ConfirmationDlg;

Interface

Uses
  Winapi.Windows,
  Winapi.Messages,
  System.SysUtils,
  System.Variants,
  System.Classes,
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.Dialogs,
  Vcl.StdCtrls,
  Vcl.Buttons;

Type
  (** A class to represent the form interface. **)
  TfrmConfirmationDlg = Class(TForm)
    lblMessage: TLabel;
    lblSourceLabel: TLabel;
    lblSource: TLabel;
    lblDestLabel: TLabel;
    lblDest: TLabel;
    lblFilenameLabel: TLabel;
    lblFilename: TLabel;
    btnYes: TBitBtn;
    btnNo: TBitBtn;
    btnAll: TBitBtn;
    btnCancel: TBitBtn;
  Private
    { Private declarations }
  Public
    { Public declarations }
    Class Function Execute(strMsg, strSource, strDest, strFilename: String): TModalResult;
  End;

Implementation

{$R *.dfm}
{ TForm1 }

(**

  This is the forms main interface method for invoking the dialogue.

  @precon  None.
  @postcon Returns the modal result of the dialogue.

  @param   strMsg      as a String
  @param   strSource   as a String
  @param   strDest     as a String
  @param   strFilename as a String
  @return  a TModalResult

**)
Class Function TfrmConfirmationDlg.Execute(strMsg, strSource, strDest,
  strFilename: String) : TModalResult;

Begin
  With TfrmConfirmationDlg.Create(Nil) Do
    Try
      lblMessage.Caption := strMsg;
      lblSourceLabel.Caption := 'Source:';
      lblSource.Caption := strSource;
      lblDest.Caption := strDest;
      If strDest <> '' Then
        lblDestLabel.Caption := 'Destination:'
      Else
        lblDestLabel.Caption := '';
      lblFilenameLabel.Caption := 'Filename:';
      lblFilename.Caption := strFilename;
      Result := ShowModal;
    Finally
      Free;
    End;
End;

End.
