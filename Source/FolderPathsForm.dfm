object frmFolderPaths: TfrmFolderPaths
  Left = 415
  Top = 386
  BorderStyle = bsDialog
  Caption = 'Folder Paths'
  ClientHeight = 124
  ClientWidth = 632
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object lblLeftFolder: TLabel
    Left = 4
    Top = 8
    Width = 50
    Height = 13
    Caption = '&Left Folder'
    FocusControl = edtLeftFolder
  end
  object lblRightFolder: TLabel
    Left = 4
    Top = 52
    Width = 57
    Height = 13
    Caption = '&Right Folder'
    FocusControl = edtRightFolder
  end
  object edtLeftFolder: TEdit
    Left = 4
    Top = 24
    Width = 581
    Height = 21
    TabOrder = 0
  end
  object edtRightFolder: TEdit
    Left = 4
    Top = 68
    Width = 581
    Height = 21
    TabOrder = 1
  end
  object btnBrowseLeft: TButton
    Left = 592
    Top = 20
    Width = 33
    Height = 25
    Caption = '...'
    TabOrder = 2
    OnClick = btnBrowseLeftClick
  end
  object btnBrowseRight: TButton
    Left = 592
    Top = 64
    Width = 33
    Height = 25
    Caption = '...'
    TabOrder = 3
    OnClick = btnBrowseRightClick
  end
  object BitBtn1: TBitBtn
    Left = 468
    Top = 96
    Width = 75
    Height = 25
    TabOrder = 4
    Kind = bkOK
  end
  object BitBtn2: TBitBtn
    Left = 552
    Top = 96
    Width = 75
    Height = 25
    TabOrder = 5
    Kind = bkCancel
  end
end
