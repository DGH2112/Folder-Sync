object frmFolderPaths: TfrmFolderPaths
  Left = 499
  Top = 388
  Caption = 'Folder Paths'
  ClientHeight = 216
  ClientWidth = 632
  Color = clBtnFace
  Constraints.MaxHeight = 308
  Constraints.MinHeight = 250
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
    216)
  PixelsPerInch = 96
  TextHeight = 13
  object lblLeftFolder: TLabel
    Left = 8
    Top = 8
    Width = 50
    Height = 13
    Caption = '&Left Folder'
    FocusControl = edtLeftFolder
  end
  object lblRightFolder: TLabel
    Left = 8
    Top = 52
    Width = 57
    Height = 13
    Caption = '&Right Folder'
    FocusControl = edtRightFolder
  end
  object lblFldrSyncOptions: TLabel
    Left = 8
    Top = 101
    Width = 146
    Height = 13
    Caption = 'Folder Synchronization Options'
  end
  object edtLeftFolder: TEdit
    Left = 8
    Top = 25
    Width = 577
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 0
    OnChange = FolderPathChange
  end
  object edtRightFolder: TEdit
    Left = 8
    Top = 69
    Width = 577
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 2
    OnChange = FolderPathChange
  end
  object btnBrowseLeft: TButton
    Left = 591
    Top = 23
    Width = 33
    Height = 25
    Anchors = [akTop, akRight]
    Caption = '...'
    TabOrder = 1
    OnClick = btnBrowseLeftClick
  end
  object btnBrowseRight: TButton
    Left = 591
    Top = 67
    Width = 33
    Height = 25
    Anchors = [akTop, akRight]
    Caption = '...'
    TabOrder = 3
    OnClick = btnBrowseRightClick
  end
  object btnOK: TBitBtn
    Left = 468
    Top = 183
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Kind = bkOK
    NumGlyphs = 2
    TabOrder = 6
    ExplicitTop = 186
  end
  object btnCancel: TBitBtn
    Left = 549
    Top = 183
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Kind = bkCancel
    NumGlyphs = 2
    TabOrder = 7
    ExplicitTop = 186
  end
  object lbxSyncOptions: TCheckListBox
    Left = 8
    Top = 120
    Width = 616
    Height = 57
    Anchors = [akLeft, akTop, akRight, akBottom]
    Columns = 2
    ItemHeight = 13
    TabOrder = 4
    OnClick = lbxSyncOptionsClick
    ExplicitHeight = 60
  end
  object btnHelp: TBitBtn
    Left = 387
    Top = 183
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Kind = bkHelp
    NumGlyphs = 2
    TabOrder = 5
    OnClick = btnHelpClick
    ExplicitTop = 186
  end
end
