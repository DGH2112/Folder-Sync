object frmDeleteProgress: TfrmDeleteProgress
  Left = 0
  Top = 20
  BorderIcons = []
  Caption = 'Deleting Files'
  ClientHeight = 122
  ClientWidth = 441
  Color = clBtnFace
  Constraints.MaxHeight = 160
  Constraints.MinHeight = 160
  Constraints.MinWidth = 457
  DoubleBuffered = True
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  FormStyle = fsStayOnTop
  OldCreateOrder = False
  Position = poMainFormCenter
  OnResize = FormResize
  OnShow = FormShow
  DesignSize = (
    441
    122)
  PixelsPerInch = 96
  TextHeight = 13
  object lblFromLabel: TLabel
    Left = 8
    Top = 8
    Width = 29
    Height = 13
    Caption = 'From'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object lblFrom: TLabel
    Left = 72
    Top = 8
    Width = 361
    Height = 13
    Anchors = [akLeft, akTop, akRight]
    AutoSize = False
    EllipsisPosition = epPathEllipsis
  end
  object lblFileLabel: TLabel
    Left = 8
    Top = 27
    Width = 51
    Height = 13
    Caption = 'Filename'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object lblFilename: TLabel
    Left = 72
    Top = 27
    Width = 361
    Height = 13
    Anchors = [akLeft, akTop, akRight]
    AutoSize = False
    EllipsisPosition = epPathEllipsis
  end
  object lblDeleteStatus: TLabel
    Left = 8
    Top = 43
    Width = 425
    Height = 13
    Alignment = taRightJustify
    Anchors = [akLeft, akTop, akRight]
    AutoSize = False
  end
  object pbrOverall: TProgressBar
    Left = 8
    Top = 59
    Width = 425
    Height = 17
    Anchors = [akLeft, akTop, akRight]
    Max = 1000000
    MarqueeInterval = 0
    TabOrder = 0
  end
  object GridPanel: TGridPanel
    Left = 8
    Top = 82
    Width = 425
    Height = 32
    Anchors = [akLeft, akTop, akRight, akBottom]
    BevelOuter = bvNone
    ColumnCollection = <
      item
        Value = 50.000000000000000000
      end
      item
        SizeStyle = ssAbsolute
        Value = 75.000000000000000000
      end
      item
        Value = 50.000000000000000000
      end>
    ControlCollection = <
      item
        Column = 2
        Control = lblProgress
        Row = 0
      end
      item
        Column = 1
        Control = btnCancel
        Row = 0
      end>
    RowCollection = <
      item
        Value = 100.000000000000000000
      end>
    TabOrder = 1
    DesignSize = (
      425
      32)
    object lblProgress: TLabel
      Left = 249
      Top = 0
      Width = 174
      Height = 30
      Align = alClient
      Alignment = taRightJustify
      AutoSize = False
    end
    object btnCancel: TBitBtn
      Left = 179
      Top = 2
      Width = 65
      Height = 25
      Anchors = []
      Kind = bkCancel
      NumGlyphs = 2
      TabOrder = 0
      OnClick = btnCancelClick
    end
  end
end
