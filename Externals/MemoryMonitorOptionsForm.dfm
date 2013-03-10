object frmMemoryMonitorOptions: TfrmMemoryMonitorOptions
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  Caption = 'Memory Monitor Options'
  ClientHeight = 476
  ClientWidth = 378
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object lblUpdateInterval: TLabel
    Left = 8
    Top = 11
    Width = 76
    Height = 13
    Caption = '&Update Interval'
  end
  object lblBackColour: TLabel
    Left = 8
    Top = 38
    Width = 56
    Height = 13
    Caption = 'Back &Colour'
    FocusControl = cbxBackColour
  end
  object lblBackFontColour: TLabel
    Left = 8
    Top = 66
    Width = 81
    Height = 13
    Caption = 'Back &Font Colour'
    FocusControl = cbxBackFontColour
  end
  object lblHighColour: TLabel
    Left = 8
    Top = 94
    Width = 68
    Height = 13
    Caption = '&1) High Colour'
    FocusControl = cbxHighColour
  end
  object lblHighFontColour: TLabel
    Left = 8
    Top = 122
    Width = 93
    Height = 13
    Caption = '&2) High Font Colour'
    FocusControl = cbxHighFontColour
  end
  object lblHighPoint: TLabel
    Left = 8
    Top = 150
    Width = 61
    Height = 13
    Caption = '&3) High Point'
    FocusControl = edtHighPoint
  end
  object lblMidPointColour: TLabel
    Left = 8
    Top = 177
    Width = 90
    Height = 13
    Caption = '&4) Mid Point Colour'
    FocusControl = cbxMidPointColour
  end
  object lblMidPointFontColour: TLabel
    Left = 8
    Top = 205
    Width = 115
    Height = 13
    Caption = '&5) Mid Point Font Colour'
    FocusControl = cbxMidPointFontColour
  end
  object lblMidPointPoint: TLabel
    Left = 8
    Top = 233
    Width = 80
    Height = 13
    Caption = '&6) MidPoint Point'
    FocusControl = edtMidPointPoint
  end
  object lblLowColour: TLabel
    Left = 8
    Top = 260
    Width = 66
    Height = 13
    Caption = '&7) Low Colour'
    FocusControl = cbxLowColour
  end
  object lblLowFontColour: TLabel
    Left = 8
    Top = 288
    Width = 91
    Height = 13
    Caption = '&8) Low Font Colour'
    FocusControl = cbxLowFontColour
  end
  object lblLowPoint: TLabel
    Left = 8
    Top = 316
    Width = 59
    Height = 13
    Caption = '&9) Low Point'
    FocusControl = edtLowPoint
  end
  object lblFontName: TLabel
    Left = 8
    Top = 343
    Width = 52
    Height = 13
    Caption = 'Font &Name'
  end
  object lblFontSize: TLabel
    Left = 8
    Top = 370
    Width = 44
    Height = 13
    Caption = 'Font &Size'
    FocusControl = edtFontSize
  end
  object udUpdateInterval: TUpDown
    Left = 273
    Top = 8
    Width = 16
    Height = 21
    Associate = edtUpdateInterval
    Min = 1
    Max = 3600
    Position = 1
    TabOrder = 1
  end
  object edtUpdateInterval: TDGHEdit
    Left = 128
    Top = 8
    Width = 145
    Height = 21
    Alignment = taRightJustify
    ReadOnly = True
    TabOrder = 0
    Text = '1'
    OnKeyPress = edtUpdateIntervalKeyPress
  end
  object cbxBackColour: TColorBox
    Left = 128
    Top = 35
    Width = 161
    Height = 22
    Style = [cbStandardColors, cbExtendedColors, cbSystemColors, cbCustomColor, cbPrettyNames, cbCustomColors]
    TabOrder = 2
  end
  object cbxBackFontColour: TColorBox
    Left = 128
    Top = 63
    Width = 161
    Height = 22
    Style = [cbStandardColors, cbExtendedColors, cbSystemColors, cbCustomColor, cbPrettyNames, cbCustomColors]
    TabOrder = 3
  end
  object cbxHighColour: TColorBox
    Left = 128
    Top = 91
    Width = 161
    Height = 22
    Style = [cbStandardColors, cbExtendedColors, cbSystemColors, cbCustomColor, cbPrettyNames, cbCustomColors]
    TabOrder = 4
  end
  object cbxHighFontColour: TColorBox
    Left = 128
    Top = 119
    Width = 161
    Height = 22
    Style = [cbStandardColors, cbExtendedColors, cbSystemColors, cbCustomColor, cbPrettyNames, cbCustomColors]
    TabOrder = 5
  end
  object cbxMidPointColour: TColorBox
    Left = 128
    Top = 174
    Width = 161
    Height = 22
    Style = [cbStandardColors, cbExtendedColors, cbSystemColors, cbCustomColor, cbPrettyNames, cbCustomColors]
    TabOrder = 8
  end
  object cbxMidPointFontColour: TColorBox
    Left = 128
    Top = 202
    Width = 161
    Height = 22
    Style = [cbStandardColors, cbExtendedColors, cbSystemColors, cbCustomColor, cbPrettyNames, cbCustomColors]
    TabOrder = 9
  end
  object cbxLowColour: TColorBox
    Left = 128
    Top = 257
    Width = 161
    Height = 22
    Style = [cbStandardColors, cbExtendedColors, cbSystemColors, cbCustomColor, cbPrettyNames, cbCustomColors]
    TabOrder = 12
  end
  object cbxLowFontColour: TColorBox
    Left = 128
    Top = 285
    Width = 161
    Height = 22
    Style = [cbStandardColors, cbExtendedColors, cbSystemColors, cbCustomColor, cbPrettyNames, cbCustomColors]
    TabOrder = 13
  end
  object btnOK: TBitBtn
    Left = 295
    Top = 8
    Width = 75
    Height = 25
    Kind = bkOK
    NumGlyphs = 2
    TabOrder = 20
  end
  object btnCancel: TBitBtn
    Left = 295
    Top = 39
    Width = 75
    Height = 25
    Kind = bkCancel
    NumGlyphs = 2
    TabOrder = 21
  end
  object edtHighPoint: TDGHEdit
    Left = 128
    Top = 147
    Width = 145
    Height = 21
    Alignment = taRightJustify
    ReadOnly = True
    TabOrder = 6
    Text = '1'
    OnKeyPress = edtUpdateIntervalKeyPress
  end
  object udHighPoint: TUpDown
    Left = 273
    Top = 147
    Width = 16
    Height = 21
    Associate = edtHighPoint
    Min = 1
    Position = 1
    TabOrder = 7
  end
  object edtMidPointPoint: TDGHEdit
    Left = 128
    Top = 230
    Width = 145
    Height = 21
    Alignment = taRightJustify
    ReadOnly = True
    TabOrder = 10
    Text = '1'
    OnKeyPress = edtUpdateIntervalKeyPress
  end
  object udMidPointPoint: TUpDown
    Left = 273
    Top = 230
    Width = 16
    Height = 21
    Associate = edtMidPointPoint
    Min = 1
    Position = 1
    TabOrder = 11
  end
  object edtLowPoint: TDGHEdit
    Left = 128
    Top = 313
    Width = 145
    Height = 21
    Alignment = taRightJustify
    ReadOnly = True
    TabOrder = 14
    Text = '1'
    OnKeyPress = edtUpdateIntervalKeyPress
  end
  object udLowPoint: TUpDown
    Left = 273
    Top = 313
    Width = 16
    Height = 21
    Associate = edtLowPoint
    Min = 1
    Position = 1
    TabOrder = 15
  end
  object cbxFontName: TComboBox
    Left = 128
    Top = 340
    Width = 161
    Height = 21
    TabOrder = 16
    Text = 'cbxFontName'
  end
  object edtFontSize: TDGHEdit
    Left = 128
    Top = 367
    Width = 145
    Height = 21
    Alignment = taRightJustify
    ReadOnly = True
    TabOrder = 17
    Text = '6'
    OnKeyPress = edtUpdateIntervalKeyPress
  end
  object udFontSize: TUpDown
    Left = 273
    Top = 367
    Width = 16
    Height = 21
    Associate = edtFontSize
    Min = 6
    Max = 32
    Position = 6
    TabOrder = 18
  end
  object grpFontStyles: TGroupBox
    Left = 8
    Top = 394
    Width = 281
    Height = 75
    Caption = 'Font Styles'
    TabOrder = 19
    object cbxBold: TCheckBox
      Left = 19
      Top = 23
      Width = 97
      Height = 17
      Caption = '&Bold'
      TabOrder = 0
    end
    object cbxItalic: TCheckBox
      Left = 19
      Top = 48
      Width = 97
      Height = 17
      Caption = '&Italic'
      TabOrder = 1
    end
    object cbxUnderline: TCheckBox
      Left = 139
      Top = 23
      Width = 97
      Height = 17
      Caption = 'Un&derline'
      TabOrder = 2
    end
    object cbxStrikeout: TCheckBox
      Left = 139
      Top = 46
      Width = 97
      Height = 17
      Caption = 'Stri&keout'
      TabOrder = 3
    end
  end
end
