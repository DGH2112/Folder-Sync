object frmProgress: TfrmProgress
  Left = 543
  Top = 507
  BorderStyle = bsNone
  Caption = 'Progress'
  ClientHeight = 106
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
    Height = 106
    Align = alClient
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
      Left = 7
      Top = 21
      Width = 486
      Height = 22
      Align = alTop
      ExplicitLeft = 9
      ExplicitTop = 23
      ExplicitWidth = 482
    end
    object lblFileName: TDGHEllipsisLabel
      Left = 9
      Top = 45
      Width = 482
      Height = 22
    end
    object pbrProgress: TProgressBar
      Left = 7
      Top = 4
      Width = 486
      Height = 17
      Align = alTop
      TabOrder = 0
    end
    object btnCancel: TBitBtn
      Left = 216
      Top = 73
      Width = 75
      Height = 25
      DoubleBuffered = True
      Kind = bkCancel
      ParentDoubleBuffered = False
      TabOrder = 1
      OnClick = btnCancelClick
    end
  end
end
