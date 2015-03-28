object frmCopyProgress: TfrmCopyProgress
  Left = 0
  Top = 20
  BorderIcons = []
  Caption = 'Copying Files'
  ClientHeight = 212
  ClientWidth = 544
  Color = clBtnFace
  Constraints.MaxHeight = 250
  Constraints.MinHeight = 250
  Constraints.MinWidth = 560
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
    544
    212)
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
    Width = 464
    Height = 13
    Anchors = [akLeft, akTop, akRight]
    AutoSize = False
    EllipsisPosition = epPathEllipsis
  end
  object lblToLabel: TLabel
    Left = 8
    Top = 27
    Width = 14
    Height = 13
    Caption = 'To'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object lblTo: TLabel
    Left = 72
    Top = 27
    Width = 464
    Height = 13
    Anchors = [akLeft, akTop, akRight]
    AutoSize = False
    EllipsisPosition = epPathEllipsis
  end
  object lblFileLabel: TLabel
    Left = 8
    Top = 46
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
    Top = 46
    Width = 464
    Height = 13
    Anchors = [akLeft, akTop, akRight]
    AutoSize = False
    EllipsisPosition = epPathEllipsis
  end
  object lblOverall: TLabel
    Left = 8
    Top = 127
    Width = 40
    Height = 13
    Caption = 'Overall'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object lblFile: TLabel
    Left = 8
    Top = 85
    Width = 19
    Height = 13
    Caption = 'File'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object lblBytesCopied: TLabel
    Left = 72
    Top = 85
    Width = 464
    Height = 13
    Alignment = taRightJustify
    Anchors = [akLeft, akTop, akRight]
    AutoSize = False
    EllipsisPosition = epPathEllipsis
  end
  object lblBytesOverallCopied: TLabel
    Left = 72
    Top = 127
    Width = 464
    Height = 13
    Alignment = taRightJustify
    Anchors = [akLeft, akTop, akRight]
    AutoSize = False
    EllipsisPosition = epPathEllipsis
  end
  object pbrOverall: TProgressBar
    Left = 8
    Top = 146
    Width = 528
    Height = 17
    Anchors = [akLeft, akTop, akRight]
    Max = 1000000
    MarqueeInterval = 0
    TabOrder = 1
  end
  object pbrFile: TProgressBar
    Left = 8
    Top = 104
    Width = 528
    Height = 17
    Anchors = [akLeft, akTop, akRight]
    Max = 1000000
    MarqueeInterval = 0
    TabOrder = 0
  end
  object GridPanel: TGridPanel
    Left = 8
    Top = 169
    Width = 528
    Height = 35
    Anchors = [akLeft, akTop, akRight, akBottom]
    BevelOuter = bvNone
    ColumnCollection = <
      item
        Value = 50.000000745058070000
      end
      item
        SizeStyle = ssAbsolute
        Value = 75.000000000000000000
      end
      item
        Value = 49.999999254941930000
      end>
    ControlCollection = <
      item
        Column = 1
        Control = btnCancel
        Row = 0
      end
      item
        Column = 2
        Control = lblRemainingTime
        Row = 0
      end>
    RowCollection = <
      item
        Value = 100.000000000000000000
      end>
    TabOrder = 2
    DesignSize = (
      528
      35)
    object btnCancel: TBitBtn
      Left = 225
      Top = 4
      Width = 75
      Height = 25
      Anchors = []
      Kind = bkCancel
      NumGlyphs = 2
      TabOrder = 0
      OnClick = btnCancelClick
    end
    object lblRemainingTime: TLabel
      Left = 300
      Top = 0
      Width = 226
      Height = 33
      Align = alClient
      Alignment = taRightJustify
      AutoSize = False
      EllipsisPosition = epPathEllipsis
    end
  end
end
