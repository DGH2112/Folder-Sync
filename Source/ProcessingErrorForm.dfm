object frmErrorDlg: TfrmErrorDlg
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu]
  Caption = 'Processing Error'
  ClientHeight = 187
  ClientWidth = 534
  Color = clBtnFace
  Constraints.MinHeight = 225
  Constraints.MinWidth = 550
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poMainFormCenter
  OnCreate = FormCreate
  DesignSize = (
    534
    187)
  PixelsPerInch = 96
  TextHeight = 13
  object lblMessage: TLabel
    Left = 8
    Top = 8
    Width = 518
    Height = 13
    Anchors = [akLeft, akTop, akRight]
    AutoSize = False
    Caption = 'lblMessage'
    ExplicitWidth = 619
  end
  object lblSourceLabel: TLabel
    Left = 18
    Top = 27
    Width = 68
    Height = 13
    AutoSize = False
    Caption = '&Source:'
  end
  object lblSourceFile: TLabel
    Left = 92
    Top = 27
    Width = 434
    Height = 13
    Anchors = [akLeft, akTop, akRight]
    AutoSize = False
    Caption = 'lblSourceFile'
    EllipsisPosition = epPathEllipsis
    ExplicitWidth = 535
  end
  object lblDestFile: TLabel
    Left = 92
    Top = 46
    Width = 434
    Height = 13
    Anchors = [akLeft, akTop, akRight]
    AutoSize = False
    Caption = 'lblDestFile'
    EllipsisPosition = epPathEllipsis
    ExplicitWidth = 411
  end
  object lblDestLabel: TLabel
    Left = 18
    Top = 46
    Width = 68
    Height = 13
    AutoSize = False
    Caption = '&Destination:'
  end
  object lblOSError: TLabel
    Left = 92
    Top = 65
    Width = 434
    Height = 64
    Anchors = [akLeft, akTop, akRight, akBottom]
    AutoSize = False
    Caption = 'lblOSError'
    WordWrap = True
    ExplicitWidth = 411
    ExplicitHeight = 104
  end
  object lblOSErrorLabel: TLabel
    Left = 18
    Top = 65
    Width = 68
    Height = 13
    AutoSize = False
    Caption = '&OS Error:'
  end
  object imgIcon: TImage
    Left = 8
    Top = 131
    Width = 48
    Height = 48
    Anchors = [akLeft, akBottom]
    Center = True
    Stretch = True
    ExplicitTop = 171
  end
  object lblWhatToDo: TLabel
    Left = 62
    Top = 135
    Width = 464
    Height = 13
    Anchors = [akLeft, akRight, akBottom]
    AutoSize = False
    Caption = 'Do you want to [I]gnore the error or [S]top processing?'
    ExplicitTop = 175
    ExplicitWidth = 441
  end
  object btnIgnore: TBitBtn
    Left = 370
    Top = 154
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Kind = bkIgnore
    NumGlyphs = 2
    TabOrder = 0
    ExplicitLeft = 347
    ExplicitTop = 143
  end
  object btnStop: TBitBtn
    Left = 451
    Top = 154
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Kind = bkAbort
    NumGlyphs = 2
    TabOrder = 1
    ExplicitLeft = 428
    ExplicitTop = 143
  end
end
