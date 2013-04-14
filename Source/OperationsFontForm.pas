(**
  
  This module contains a form for editing the file operations fonts.

  @Version 1.0
  @Author  David Hoyle
  @Date    14 Apr 2013
  
**)
Unit OperationsFontForm;

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
  Vcl.ExtCtrls,
  Vcl.StdCtrls,
  Vcl.Buttons;

Type
  (** A form for editing the file operations fonts. **)
  TfrmOperationsFonts = Class(TForm)
    lblFontColour: TLabel;
    ccbxFontColour: TColorBox;
    lblBackgroundColour: TLabel;
    ccbxBackgroundColour: TColorBox;
    gbxFontStyles: TGroupBox;
    chkBold: TCheckBox;
    chkItalic: TCheckBox;
    chkStrikeout: TCheckBox;
    chkUnderline: TCheckBox;
    btnHelp: TBitBtn;
    btnOK: TBitBtn;
    btnCancel: TBitBtn;
    GridPanel: TGridPanel;
  Private
    { Private declarations }
  Public
    { Public declarations }
    Class Function Execute(Var iFontColour, iBackColour: TColor;
      var iStyles: TFontStyles): Boolean;
  End;

Implementation

{$R *.dfm}
{ TfrmOperationsFonts }

(**

  This is the forms main interface method for imnvoking the form and editing the fonts.

  @precon  None.
  @postcon Displays the form for editing operations fonts.

  @param   iFontColour as a TColor as a reference
  @param   iBackColour as a TColor as a reference
  @param   iStyles     as a TFontStyles as a reference
  @return  a Boolean

**)
Class Function TfrmOperationsFonts.Execute(Var iFontColour, iBackColour: TColor;
  var iStyles: TFontStyles): Boolean;

Begin
  Result := False;
  With TfrmOperationsFonts.Create(Nil) Do
    Try
      ccbxFontColour.Selected := iFontColour;
      ccbxBackgroundColour.Selected := iBackColour;
      chkBold.Checked := fsBold In iStyles;
      chkItalic.Checked := fsItalic In iStyles;
      chkUnderline.Checked := fsUnderline In iStyles;
      chkStrikeout.Checked := fsStrikeOut In iStyles;
      If ShowModal = mrOK Then
        Begin
          iFontColour := ccbxFontColour.Selected;
          iBackColour := ccbxBackgroundColour.Selected;
          iStyles := [];
          If chkBold.Checked Then Include(iStyles, fsBold);          
          If chkItalic.Checked Then Include(iStyles, fsItalic);          
          If chkUnderline.Checked Then Include(iStyles, fsUnderline);          
          If chkStrikeout.Checked Then Include(iStyles, fsStrikeOut);          
          Result := True;
        End;
    Finally
      Free;
    End;
End;

End.
