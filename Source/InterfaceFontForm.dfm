object frmInterfaceFonts: TfrmInterfaceFonts
  Left = 0
  Top = 0
  BorderIcons = []
  Caption = 'Interface Fonts'
  ClientHeight = 92
  ClientWidth = 244
  Color = clBtnFace
  Constraints.MaxHeight = 130
  Constraints.MaxWidth = 260
  Constraints.MinHeight = 130
  Constraints.MinWidth = 260
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poOwnerFormCenter
  OnCreate = FormCreate
  DesignSize = (
    244
    92)
  PixelsPerInch = 96
  TextHeight = 13
  object lblFontName: TLabel
    Left = 8
    Top = 11
    Width = 52
    Height = 13
    Caption = 'Font &Name'
    FocusControl = cbxFontName
  end
  object lblFontSize: TLabel
    Left = 8
    Top = 38
    Width = 44
    Height = 13
    Caption = 'Font &Size'
    FocusControl = edtFontSize
  end
  object cbxFontName: TComboBox
    Left = 73
    Top = 8
    Width = 163
    Height = 21
    Style = csDropDownList
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 0
  end
  object edtFontSize: TEdit
    Left = 73
    Top = 35
    Width = 141
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    ReadOnly = True
    TabOrder = 1
    Text = '8'
  end
  object udFontSize: TUpDown
    Left = 214
    Top = 35
    Width = 16
    Height = 21
    Anchors = [akTop, akRight]
    Associate = edtFontSize
    Min = 8
    Max = 32
    Position = 8
    TabOrder = 2
  end
  object btnHelp: TBitBtn
    Left = -1
    Top = 59
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Kind = bkHelp
    NumGlyphs = 2
    TabOrder = 3
  end
  object btnOK: TBitBtn
    Left = 80
    Top = 59
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Kind = bkOK
    NumGlyphs = 2
    TabOrder = 4
  end
  object btnCancel: TBitBtn
    Left = 161
    Top = 59
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Kind = bkCancel
    NumGlyphs = 2
    TabOrder = 5
  end
end
