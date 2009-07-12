object frmProgress: TfrmProgress
  Left = 543
  Top = 507
  BorderStyle = bsNone
  Caption = 'Progress'
  ClientHeight = 79
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
    Height = 79
    Align = alClient
    BevelInner = bvLowered
    BevelWidth = 2
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlue
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    Padding.Left = 5
    Padding.Top = 2
    Padding.Right = 5
    Padding.Bottom = 2
    ParentFont = False
    TabOrder = 0
    object lblMessage: TDGHEllipsisLabel
      Left = 9
      Top = 23
      Width = 482
      Height = 22
      Align = alTop
      ExplicitLeft = 4
      ExplicitTop = 4
      ExplicitWidth = 624
    end
    object lblFileName: TDGHEllipsisLabel
      Left = 9
      Top = 45
      Width = 482
      Height = 28
      Align = alClient
      ExplicitTop = 112
      ExplicitHeight = 60
    end
    object pbrProgress: TProgressBar
      Left = 9
      Top = 6
      Width = 482
      Height = 17
      Align = alTop
      TabOrder = 0
    end
  end
end
