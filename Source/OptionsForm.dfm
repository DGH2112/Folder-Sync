object frmOptions: TfrmOptions
  Left = 548
  Top = 415
  BorderStyle = bsDialog
  Caption = 'Folder Sync Options'
  ClientHeight = 297
  ClientWidth = 483
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poMainFormCenter
  PixelsPerInch = 96
  TextHeight = 13
  object lblTimeDiff: TLabel
    Left = 4
    Top = 272
    Width = 117
    Height = 13
    Caption = '&Time Different Tolerance'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
  end
  object Label1: TLabel
    Left = 4
    Top = 4
    Width = 50
    Height = 13
    Caption = '&Exclusions'
    FocusControl = edtExclusions
  end
  object edtExclusions: TMemo
    Left = 4
    Top = 24
    Width = 473
    Height = 237
    ScrollBars = ssBoth
    TabOrder = 0
    WordWrap = False
  end
  object edtTimeDiff: TEdit
    Left = 136
    Top = 268
    Width = 37
    Height = 21
    ReadOnly = True
    TabOrder = 1
    Text = '0'
  end
  object udTolerance: TUpDown
    Left = 173
    Top = 268
    Width = 16
    Height = 21
    Associate = edtTimeDiff
    Min = 0
    Max = 60
    Position = 0
    TabOrder = 2
    Wrap = False
  end
  object btnOK: TBitBtn
    Left = 324
    Top = 268
    Width = 75
    Height = 25
    TabOrder = 3
    Kind = bkOK
  end
  object btnCancel: TBitBtn
    Left = 404
    Top = 268
    Width = 75
    Height = 25
    TabOrder = 4
    Kind = bkCancel
  end
end
