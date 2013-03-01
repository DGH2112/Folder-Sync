(**

  This module contains a form for confirming a file action.

  @Author  David Hoyle
  @Version 1.0
  @Date    01 Mar 2013

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
  Vcl.Buttons,
  SyncModule;

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
    lblSourceInfo: TLabel;
    lblDestInfo: TLabel;
  Private
    { Private declarations }
  Public
    { Public declarations }
    Class Function Execute(ParentForm : TForm; strMsg, strSrcPath, strDestPath : String;
      Source, Dest : TFileRecord; iTop : Integer): TModalResult;
  End;

Implementation

{$R *.dfm}
{ TForm1 }

(**

  This is the forms main interface method for invoking the dialogue.

  @precon  None.
  @postcon Returns the modal result of the dialogue.

  @param   ParentForm  as a TForm
  @param   strMsg      as a String
  @param   strSrcPath  as a String
  @param   strDestPath as a String
  @param   Source      as a TFileRecord
  @param   Dest        as a TFileRecord
  @param   iTop        as an Integer
  @return  a TModalResult

**)
Class Function TfrmConfirmationDlg.Execute(ParentForm : TForm; strMsg, strSrcPath,
  strDestPath : String; Source, Dest : TFileRecord; iTop : Integer) : TModalResult;

Begin
  With TfrmConfirmationDlg.Create(Nil) Do
    Try
      Left := ParentForm.Left + (ParentForm.Width - Width) Div 2;
      Top := iTop;
      lblMessage.Caption := strMsg;
      lblLabelLine1.Caption := 'Source:';
      lblInformation1.Caption := strSrcPath + Source.FileName;
      lblSourceInfo.Caption := 'Source: ' + Format('%1.0n bytes, %s', [
        Int(Source.Size),
        FormatDateTime('ddd dd/mmm/yyyy @ hh:nn:ss', FileDateToDateTime(Source.DateTime))
      ]);
      If strDestPath <> '' Then
        Begin
          lblLabelLine2.Caption := 'Destination:';
          lblInformation2.Caption := strDestPath;
          lblLabelLine3.Caption := 'Filename:';
          lblInformation3.Caption := Dest.FileName;
          lblDestInfo.Caption := 'Destination: ' + Format('%1.0n bytes, %s', [
            Int(Dest.Size),
            FormatDateTime('ddd dd/mmm/yyyy @ hh:nn:ss', FileDateToDateTime(Dest.DateTime))
          ]);
        End Else
        Begin
          lblLabelLine2.Caption := 'Filename:';
          lblInformation2.Caption := Source.FileName;
          lblLabelLine3.Caption := '';
          lblInformation3.Caption := '';
          lblDestInfo.Caption := '';
          Height := Height - (lblInformation3.Top - lblInformation2.Top);
        End;
      Result := ShowModal;
    Finally
      Free;
    End;
End;

End.
