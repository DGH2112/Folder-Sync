(**
  
  This module defines a class to represent a form for displaying an error message while
  processing.

  @Author  David Hoyle
  @Version 1.0
  @Date    09 Nov 2013
  
**)
Unit ProcessingErrorForm;

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
  Vcl.ExtCtrls;

Type
  (** A class to represent the user form for displaying the error message. **)
  TfrmErrorDlg = Class(TForm)
    lblMessage: TLabel;
    lblSourceLabel: TLabel;
    lblSourceFile: TLabel;
    lblDestFile: TLabel;
    lblDestLabel: TLabel;
    lblOSError: TLabel;
    lblOSErrorLabel: TLabel;
    btnIgnore: TBitBtn;
    btnStop: TBitBtn;
    imgIcon: TImage;
    lblWhatToDo: TLabel;
    Procedure FormCreate(Sender: TObject);
  Private
    { Private declarations }
  Public
    { Public declarations }
    Class Function Execute(strErrorMsg, strSource, strDest, strOSError: String)
      : TModalResult;
  End;

Implementation

{$R *.dfm}

(**

  This method is the forms main interface method for displaying the form.

  @precon  None.
  @postcon Displays the form with the error message and asks the user to Ignore or Stop.

  @param   strErrorMsg as a String
  @param   strSource   as a String
  @param   strDest     as a String
  @param   strOSError  as a String
  @return  a TModalResult

**)
Class Function TfrmErrorDlg.Execute(strErrorMsg, strSource, strDest, strOSError: String)
  : TModalResult;
Begin
  With TfrmErrorDlg.Create(Nil) Do
    Try
      lblMessage.Caption := strErrorMsg;
      lblSourceFile.Caption := strSource;
      lblDestFile.Caption := strDest;
      lblOSError.Caption := strOSError;
      Result := ShowModal;
    Finally
      Free;
    End;
End;

(**

  This is an OnFormCreate Event Handler for the TfrmErrorDlg class.

  @precon  None.
  @postcon Loads an error icon into the image control.

  @param   Sender as a TObject

**)
Procedure TfrmErrorDlg.FormCreate(Sender: TObject);

Var
  AIcon: TIcon;

Begin
  AIcon := TIcon.Create;
  Try
    AIcon.Handle := LoadIcon(AIcon.Handle, IDI_ERROR);
    imgIcon.Picture.Icon := AIcon;
  Finally
    AIcon.Free;
  End;
End;

End.
