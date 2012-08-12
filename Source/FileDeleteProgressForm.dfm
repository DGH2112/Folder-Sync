object frmDeleteProgress: TfrmDeleteProgress
  Left = 0
  Top = 20
  BorderIcons = []
  BorderStyle = bsDialog
  Caption = 'Deleting Files'
  ClientHeight = 103
  ClientWidth = 451
  Color = clBtnFace
  DoubleBuffered = True
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  FormStyle = fsStayOnTop
  OldCreateOrder = False
  Position = poMainFormCenter
  DesignSize = (
    451
    103)
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
    Width = 371
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
    Width = 371
    Height = 13
    Anchors = [akLeft, akTop, akRight]
    AutoSize = False
    EllipsisPosition = epPathEllipsis
  end
  object pbrOverall: TProgressBar
    Left = 8
    Top = 46
    Width = 435
    Height = 17
    Anchors = [akLeft, akTop, akRight]
    Max = 1000000
    MarqueeInterval = 0
    TabOrder = 0
  end
  object btnCancel: TBitBtn
    Left = 192
    Top = 70
    Width = 75
    Height = 25
    Anchors = [akLeft, akRight, akBottom]
    Kind = bkCancel
    NumGlyphs = 2
    TabOrder = 1
    OnClick = btnCancelClick
  end
end
