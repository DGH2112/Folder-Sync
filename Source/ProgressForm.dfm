object frmProgress: TfrmProgress
  Left = 543
  Top = 507
  BorderStyle = bsNone
  Caption = 'Progress'
  ClientHeight = 93
  ClientWidth = 632
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
    Width = 632
    Height = 93
    Align = alClient
    BevelInner = bvLowered
    BevelWidth = 2
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlue
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
    TabOrder = 0
    object lblInfo: TLabel
      Left = 8
      Top = 8
      Width = 617
      Height = 77
      Anchors = [akLeft, akTop, akRight, akBottom]
      AutoSize = False
      Caption = 'lblInfo'
      Layout = tlCenter
      WordWrap = True
    end
  end
end
