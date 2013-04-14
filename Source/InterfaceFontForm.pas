(**
  
  This module contains a form to edit the interface fonts.

  @Author  David Hoyle
  @Version 1.0
  @Date    14 Apr 2013
  
**)
Unit InterfaceFontForm;

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
  Vcl.ComCtrls;

Type
  (** A form for editing thr interface fonts. **)
  TfrmInterfaceFonts = Class(TForm)
    lblFontName: TLabel;
    cbxFontName: TComboBox;
    lblFontSize: TLabel;
    edtFontSize: TEdit;
    udFontSize: TUpDown;
    btnHelp: TBitBtn;
    btnOK: TBitBtn;
    btnCancel: TBitBtn;
    Procedure FormCreate(Sender: TObject);
  Private
    { Private declarations }
  Public
    { Public declarations }
    Class Function Execute(Var strFontName: String; Var iFontSize: Integer): Boolean;
  End;

Implementation

{$R *.dfm}


(**

  This is the forms main interface method of invoking the form and getting font
  information.

  @precon  None.
  @postcon Displays the form and retrieves the required font settings.

  @param   strFontName as a String as a reference
  @param   iFontSize   as an Integer as a reference
  @return  a Boolean

**)
Class Function TfrmInterfaceFonts.Execute(Var strFontName: String;
  Var iFontSize: Integer): Boolean;

Begin
  Result := False;
  With TfrmInterfaceFonts.Create(Nil) Do
    Try
      cbxFontName.ItemIndex := cbxFontName.Items.IndexOf(strFontName);
      udFontSize.Position := iFontSize;
      If ShowModal = mrOK Then
        Begin
          strFontName := cbxFontName.Items[cbxFontName.ItemIndex];
          iFontSize := udFontSize.Position;
          Result := True;
        End;
    Finally
      Free;
    End;
End;

(**

  This is an OnFormCreate Event Handler for the TfrmInterfaceFonts class.

  @precon  None.
  @postcon Loads the systems fonts into the drop down list.

  @param   Sender as a TObject

**)
Procedure TfrmInterfaceFonts.FormCreate(Sender: TObject);

Begin
  cbxFontName.Items.Assign(Screen.Fonts);
End;

End.
