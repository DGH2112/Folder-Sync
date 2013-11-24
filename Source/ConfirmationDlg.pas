(**

  This module contains a form for confirming a file action.

  @Author  David Hoyle
  @Version 1.0
  @Date    24 Nov 2013

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
  SyncModule, Vcl.ExtCtrls;

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
    btnYesToAll: TBitBtn;
    btnCancel: TBitBtn;
    lblSourceInfo: TLabel;
    lblDestInfo: TLabel;
    imgIcon: TImage;
    btnNoToAll: TBitBtn;
  Private
    { Private declarations }
  Public
    { Public declarations }
    Class Function Execute(ParentForm : TForm; strMsg, strSrcPath, strDestPath : String;
      Source, Dest : TFileRecord; iTop : Integer; var iWidth : Integer): TModalResult;
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
  @param   iWidth      as an Integer as a reference
  @return  a TModalResult

**)
Class Function TfrmConfirmationDlg.Execute(ParentForm : TForm; strMsg, strSrcPath,
  strDestPath : String; Source, Dest : TFileRecord; iTop : Integer;
  var iWidth : Integer) : TModalResult;

Var
  AIcon : TIcon;
  
Begin
  With TfrmConfirmationDlg.Create(Nil) Do
    Try
      If iWidth > 0 Then
        Width := iWidth;
      Left := ParentForm.Left + (ParentForm.Width - Width) Div 2;
      Top := iTop;
      lblMessage.Caption := strMsg;
      lblLabelLine1.Caption := 'Source:';
      lblInformation1.Caption := ExtractFilepath(strSrcPath + Source.FileName);
      lblSourceInfo.Caption := 'Source: ' + Format('%1.0n bytes, %s', [
        Int(Source.Size),
        FormatDateTime('ddd dd/mmm/yyyy @ hh:nn:ss', FileDateToDateTime(Source.DateTime))
      ]);
      If strDestPath <> '' Then
        Begin
          AIcon := TIcon.Create;
          Try
            If (Dest <> Nil) And (
              (Source.DateTime < Dest.DateTime) Or (Dest.Attributes And faReadOnly <> 0)) Then
              AIcon.Handle := LoadIcon(AIcon.Handle, IDI_WARNING)
             Else
              AIcon.Handle := LoadIcon(AIcon.Handle, IDI_QUESTION);
            imgIcon.Picture.Icon := AIcon;
          Finally
            AIcon.Free;
          End;
          lblLabelLine2.Caption := 'Destination:';
          lblInformation2.Caption := ExtractFilePath(strDestPath + Dest.FileName);
          lblLabelLine3.Caption := 'Filename:';
          lblInformation3.Caption := ExtractFileName(Dest.FileName);
          lblDestInfo.Caption := 'Destination: ' + Format('%1.0n bytes, %s', [
            Int(Dest.Size),
            FormatDateTime('ddd dd/mmm/yyyy @ hh:nn:ss', FileDateToDateTime(Dest.DateTime))
          ]);
        End Else
        Begin
          AIcon := TIcon.Create;
          Try
            If (Source <> Nil) And (Source.Attributes And faReadOnly <> 0) Then
              AIcon.Handle := LoadIcon(AIcon.Handle, IDI_WARNING)
             Else
              AIcon.Handle := LoadIcon(AIcon.Handle, IDI_QUESTION);
            imgIcon.Picture.Icon := AIcon;
          Finally
            AIcon.Free;
          End;
          lblLabelLine2.Caption := 'Filename:';
          lblInformation2.Caption := ExtractFileName(Source.FileName);
          lblLabelLine3.Caption := '';
          lblInformation3.Caption := '';
          lblDestInfo.Caption := '';
          Height := Height - (lblInformation3.Top - lblInformation2.Top);
        End;
      Result := ShowModal;
      iWidth := Width;
    Finally
      Free;
    End;
End;

End.
