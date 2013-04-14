object frmOperationsFonts: TfrmOperationsFonts
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu]
  Caption = 'Operation Fonts'
  ClientHeight = 147
  ClientWidth = 323
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poMainFormCenter
  DesignSize = (
    323
    147)
  PixelsPerInch = 96
  TextHeight = 13
  object lblFontColour: TLabel
    Left = 8
    Top = 11
    Width = 56
    Height = 13
    Caption = '&Font Colour'
    FocusControl = ccbxFontColour
  end
  object lblBackgroundColour: TLabel
    Left = 8
    Top = 39
    Width = 90
    Height = 13
    Caption = 'Back&ground Colour'
    FocusControl = ccbxBackgroundColour
  end
  object ccbxFontColour: TColorBox
    Left = 126
    Top = 8
    Width = 189
    Height = 22
    Style = [cbStandardColors, cbExtendedColors, cbSystemColors, cbCustomColor, cbPrettyNames, cbCustomColors]
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 0
    ExplicitWidth = 468
  end
  object ccbxBackgroundColour: TColorBox
    Left = 126
    Top = 36
    Width = 189
    Height = 22
    Style = [cbStandardColors, cbExtendedColors, cbSystemColors, cbCustomColor, cbPrettyNames, cbCustomColors]
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 1
    ExplicitWidth = 468
  end
  object gbxFontStyles: TGroupBox
    Left = 8
    Top = 64
    Width = 307
    Height = 44
    Anchors = [akLeft, akTop, akRight, akBottom]
    Caption = 'Font Styles'
    Padding.Left = 5
    Padding.Top = 5
    Padding.Right = 5
    Padding.Bottom = 5
    TabOrder = 2
    ExplicitWidth = 586
    ExplicitHeight = 301
    object GridPanel: TGridPanel
      Left = 7
      Top = 20
      Width = 293
      Height = 17
      Align = alClient
      BevelOuter = bvNone
      ColumnCollection = <
        item
          Value = 25.000000000000010000
        end
        item
          Value = 24.999999999999970000
        end
        item
          Value = 25.000000000000000000
        end
        item
          Value = 25.000000000000010000
        end>
      ControlCollection = <
        item
          Column = 0
          Control = chkBold
          Row = 0
        end
        item
          Column = 1
          Control = chkItalic
          Row = 0
        end
        item
          Column = 2
          Control = chkUnderline
          Row = 0
        end
        item
          Column = 3
          Control = chkStrikeout
          Row = 0
        end>
      RowCollection = <
        item
          Value = 100.000000000000000000
        end>
      TabOrder = 0
      ExplicitLeft = 20
      ExplicitTop = 33
      ExplicitWidth = 605
      ExplicitHeight = 41
      DesignSize = (
        293
        17)
      object chkBold: TCheckBox
        Left = 0
        Top = 0
        Width = 73
        Height = 17
        Anchors = []
        Caption = '&Bold'
        TabOrder = 0
        ExplicitLeft = 34
        ExplicitTop = 73
      end
      object chkItalic: TCheckBox
        Left = 73
        Top = 0
        Width = 73
        Height = 17
        Anchors = []
        Caption = '&Italic'
        TabOrder = 1
        ExplicitLeft = 165
        ExplicitTop = 80
      end
      object chkUnderline: TCheckBox
        Left = 146
        Top = 0
        Width = 73
        Height = 17
        Anchors = []
        Caption = '&Underline'
        TabOrder = 2
        ExplicitLeft = 466
        ExplicitTop = 80
      end
      object chkStrikeout: TCheckBox
        Left = 219
        Top = 0
        Width = 74
        Height = 17
        Anchors = []
        Caption = '&Strikeout'
        TabOrder = 3
        ExplicitLeft = 294
        ExplicitTop = 76
      end
    end
  end
  object btnHelp: TBitBtn
    Left = 78
    Top = 114
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Kind = bkHelp
    NumGlyphs = 2
    TabOrder = 3
    ExplicitLeft = 357
    ExplicitTop = 371
  end
  object btnOK: TBitBtn
    Left = 159
    Top = 114
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Kind = bkOK
    NumGlyphs = 2
    TabOrder = 4
    ExplicitLeft = 438
    ExplicitTop = 371
  end
  object btnCancel: TBitBtn
    Left = 240
    Top = 114
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Kind = bkCancel
    NumGlyphs = 2
    TabOrder = 5
    ExplicitLeft = 519
    ExplicitTop = 371
  end
end
