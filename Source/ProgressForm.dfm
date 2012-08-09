object frmProgress: TfrmProgress
  Left = 543
  Top = 507
  BorderStyle = bsNone
  Caption = 'Progress'
  ClientHeight = 107
  ClientWidth = 500
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  FormStyle = fsStayOnTop
  OldCreateOrder = False
  Position = poOwnerFormCenter
  PixelsPerInch = 96
  TextHeight = 13
  object pnlInfo: TPanel
    Left = 0
    Top = 0
    Width = 500
    Height = 107
    Align = alClient
    BevelWidth = 2
    DoubleBuffered = True
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlue
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    Padding.Left = 5
    Padding.Top = 5
    Padding.Right = 5
    Padding.Bottom = 5
    ParentDoubleBuffered = False
    ParentFont = False
    TabOrder = 0
    DesignSize = (
      500
      107)
    object lblMessage: TLabel
      Left = 11
      Top = 34
      Width = 475
      Height = 13
      Alignment = taCenter
      AutoSize = False
      Caption = 'Processing...'
      EllipsisPosition = epPathEllipsis
    end
    object lblFileName: TLabel
      Left = 11
      Top = 53
      Width = 475
      Height = 13
      Alignment = taCenter
      Anchors = [akLeft, akTop, akRight]
      AutoSize = False
      Caption = 'Please wait...'
      EllipsisPosition = epPathEllipsis
    end
    object pbrProgress: TProgressBar
      Left = 11
      Top = 11
      Width = 475
      Height = 17
      Anchors = [akLeft, akTop, akRight]
      Max = 1000000
      Smooth = True
      TabOrder = 0
    end
    object btnCancel: TBitBtn
      Left = 208
      Top = 72
      Width = 75
      Height = 25
      Kind = bkCancel
      NumGlyphs = 2
      TabOrder = 1
      OnClick = btnCancelClick
    end
  end
end
