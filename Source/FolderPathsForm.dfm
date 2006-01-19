object frmFolderPaths: TfrmFolderPaths
  Left = 499
  Top = 388
  Caption = 'Folder Paths'
  ClientHeight = 126
  ClientWidth = 574
  Color = clBtnFace
  Constraints.MaxHeight = 160
  Constraints.MinHeight = 160
  Constraints.MinWidth = 300
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poMainFormCenter
  DesignSize = (
    574
    126)
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
    Left = 5
    Top = 25
    Width = 524
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 0
    OnChange = FolderPathChange
    ExplicitWidth = 563
  end
  object edtRightFolder: TEdit
    Left = 5
    Top = 69
    Width = 524
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 1
    OnChange = FolderPathChange
    ExplicitWidth = 563
  end
  object btnBrowseLeft: TButton
    Left = 535
    Top = 20
    Width = 33
    Height = 25
    Anchors = [akTop, akRight]
    Caption = '...'
    TabOrder = 2
    OnClick = btnBrowseLeftClick
    ExplicitLeft = 592
  end
  object btnBrowseRight: TButton
    Left = 535
    Top = 64
    Width = 33
    Height = 25
    Anchors = [akTop, akRight]
    Caption = '...'
    TabOrder = 3
    OnClick = btnBrowseRightClick
    ExplicitLeft = 592
  end
  object BitBtn1: TBitBtn
    Left = 411
    Top = 96
    Width = 75
    Height = 25
    Anchors = [akTop, akRight]
    TabOrder = 4
    Kind = bkOK
    ExplicitLeft = 468
  end
  object BitBtn2: TBitBtn
    Left = 495
    Top = 96
    Width = 75
    Height = 25
    Anchors = [akTop, akRight]
    TabOrder = 5
    Kind = bkCancel
    ExplicitLeft = 552
  end
end
