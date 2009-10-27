object frmFolderPaths: TfrmFolderPaths
  Left = 499
  Top = 388
  Caption = 'Folder Paths'
  ClientHeight = 126
  ClientWidth = 632
  Color = clBtnFace
  Constraints.MaxHeight = 160
  Constraints.MinHeight = 160
  Constraints.MinWidth = 640
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poMainFormCenter
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  DesignSize = (
    632
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
  object lblSyncOption: TLabel
    Left = 8
    Top = 101
    Width = 58
    Height = 13
    Caption = 'Sync &Option'
    FocusControl = cbxSyncOption
  end
  object edtLeftFolder: TEdit
    Left = 5
    Top = 25
    Width = 582
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 0
    OnChange = FolderPathChange
    ExplicitWidth = 524
  end
  object edtRightFolder: TEdit
    Left = 5
    Top = 69
    Width = 582
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 2
    OnChange = FolderPathChange
    ExplicitWidth = 524
  end
  object btnBrowseLeft: TButton
    Left = 593
    Top = 20
    Width = 33
    Height = 25
    Anchors = [akTop, akRight]
    Caption = '...'
    TabOrder = 1
    OnClick = btnBrowseLeftClick
    ExplicitLeft = 535
  end
  object btnBrowseRight: TButton
    Left = 593
    Top = 64
    Width = 33
    Height = 25
    Anchors = [akTop, akRight]
    Caption = '...'
    TabOrder = 3
    OnClick = btnBrowseRightClick
    ExplicitLeft = 535
  end
  object btnOK: TBitBtn
    Left = 469
    Top = 96
    Width = 75
    Height = 25
    Anchors = [akTop, akRight]
    DoubleBuffered = True
    Kind = bkOK
    ParentDoubleBuffered = False
    TabOrder = 5
    ExplicitLeft = 411
  end
  object btnCancel: TBitBtn
    Left = 553
    Top = 96
    Width = 75
    Height = 25
    Anchors = [akTop, akRight]
    DoubleBuffered = True
    Kind = bkCancel
    ParentDoubleBuffered = False
    TabOrder = 6
    ExplicitLeft = 495
  end
  object cbxSyncOption: TComboBox
    Left = 104
    Top = 98
    Width = 185
    Height = 21
    Style = csDropDownList
    ItemHeight = 13
    TabOrder = 4
    Items.Strings = (
      'Synchronise'
      'Primary Left'
      'Primary Right')
  end
end
