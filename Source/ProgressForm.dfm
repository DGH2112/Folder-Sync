object frmProgress: TfrmProgress
  Left = 543
  Top = 507
  BorderStyle = bsNone
  Caption = 'Progress'
  ClientHeight = 74
  ClientWidth = 492
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
    Width = 492
    Height = 74
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
    ExplicitWidth = 632
    ExplicitHeight = 93
    object lblMessage: TDGHEllipsisLabel
      Left = 4
      Top = 4
      Width = 484
      Height = 22
      Align = alTop
      ExplicitWidth = 624
    end
    object lblFileName: TDGHEllipsisLabel
      Left = 4
      Top = 48
      Width = 484
      Height = 22
      Align = alBottom
      ExplicitTop = 59
      ExplicitWidth = 624
    end
    object lblPath: TDGHEllipsisLabel
      Left = 4
      Top = 26
      Width = 484
      Height = 22
      Align = alClient
      ExplicitLeft = 320
      ExplicitTop = 48
      ExplicitWidth = 100
      ExplicitHeight = 40
    end
  end
end
