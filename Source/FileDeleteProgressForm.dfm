object frmDeleteProgress: TfrmDeleteProgress
  Left = 0
  Top = 20
  BorderIcons = []
  Caption = 'Deleting Files'
  ClientHeight = 93
  ClientWidth = 441
  Color = clBtnFace
  Constraints.MaxHeight = 131
  Constraints.MaxWidth = 457
  Constraints.MinHeight = 131
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
    93)
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
    ExplicitWidth = 371
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
    ExplicitWidth = 371
  end
  object pbrOverall: TProgressBar
    Left = 8
    Top = 46
    Width = 425
    Height = 17
    Anchors = [akLeft, akTop, akRight]
    Max = 1000000
    MarqueeInterval = 0
    TabOrder = 0
    ExplicitWidth = 435
  end
  object btnCancel: TBitBtn
    Left = 192
    Top = 60
    Width = 65
    Height = 25
    Anchors = [akLeft, akRight, akBottom]
    Kind = bkCancel
    NumGlyphs = 2
    TabOrder = 1
    OnClick = btnCancelClick
    ExplicitTop = 70
    ExplicitWidth = 75
  end
end
