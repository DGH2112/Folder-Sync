object frmCopyProgress: TfrmCopyProgress
  Left = 0
  Top = 20
  BorderIcons = []
  Caption = 'Copying Files'
  ClientHeight = 212
  ClientWidth = 544
  Color = clBtnFace
  Constraints.MaxHeight = 250
  Constraints.MaxWidth = 560
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
    ExplicitWidth = 371
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
    ExplicitWidth = 371
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
    ExplicitWidth = 371
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
    ExplicitWidth = 371
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
    ExplicitWidth = 414
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
  object btnCancel: TBitBtn
    Left = 230
    Top = 179
    Width = 75
    Height = 25
    Anchors = [akLeft, akBottom]
    Kind = bkCancel
    NumGlyphs = 2
    TabOrder = 2
    OnClick = btnCancelClick
  end
end
