(**

  This module contains a form for confirming a file action.

  @Author  David Hoyle
  @Version 1.0
  @Date    03 Aug 2012

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
    lblLabelLine1: TLabel;
    lblInformation1: TLabel;
    lblLabelLine2: TLabel;
    lblInformation2: TLabel;
    lblLabelLine3: TLabel;
    lblInformation3: TLabel;
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
      lblLabelLine1.Caption := 'Source:';
      lblInformation1.Caption := strSource;
      If strDest <> '' Then
        Begin
          lblLabelLine2.Caption := 'Destination:';
          lblInformation2.Caption := strDest;
          lblLabelLine3.Caption := 'Filename:';
          lblInformation3.Caption := strFilename;
        End Else
        Begin
          lblLabelLine2.Caption := 'Filename:';
          lblInformation2.Caption := strFilename;
          lblLabelLine3.Caption := '';
          lblInformation3.Caption := '';
          Height := Height - (lblInformation3.Top - lblInformation2.Top);
        End;
      Result := ShowModal;
    Finally
      Free;
    End;
End;

End.
