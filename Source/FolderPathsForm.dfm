object frmFolderPaths: TfrmFolderPaths
  Left = 499
  Top = 388
  Caption = 'Folder Paths'
  ClientHeight = 270
  ClientWidth = 778
  Color = clBtnFace
  Constraints.MaxHeight = 379
  Constraints.MinHeight = 308
  Constraints.MinWidth = 788
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
    778
    270)
  PixelsPerInch = 96
  TextHeight = 16
  object lblLeftFolder: TLabel
    Left = 10
    Top = 10
    Width = 63
    Height = 16
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    Caption = '&Left Folder'
    FocusControl = edtLeftFolder
  end
  object lblRightFolder: TLabel
    Left = 10
    Top = 64
    Width = 73
    Height = 16
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    Caption = '&Right Folder'
    FocusControl = edtRightFolder
  end
  object lblFldrSyncOptions: TLabel
    Left = 10
    Top = 124
    Width = 185
    Height = 16
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    Caption = 'Folder Synchronization Options'
  end
  object edtLeftFolder: TEdit
    Left = 10
    Top = 31
    Width = 710
    Height = 24
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 0
    OnChange = FolderPathChange
  end
  object edtRightFolder: TEdit
    Left = 10
    Top = 85
    Width = 710
    Height = 24
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 2
    OnChange = FolderPathChange
  end
  object btnBrowseLeft: TButton
    Left = 727
    Top = 28
    Width = 41
    Height = 31
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    Anchors = [akTop, akRight]
    Caption = '...'
    TabOrder = 1
    OnClick = btnBrowseLeftClick
  end
  object btnBrowseRight: TButton
    Left = 727
    Top = 82
    Width = 41
    Height = 31
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    Anchors = [akTop, akRight]
    Caption = '...'
    TabOrder = 3
    OnClick = btnBrowseRightClick
  end
  object btnOK: TBitBtn
    Left = 576
    Top = 229
    Width = 92
    Height = 31
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    Anchors = [akRight, akBottom]
    Kind = bkOK
    NumGlyphs = 2
    TabOrder = 5
  end
  object btnCancel: TBitBtn
    Left = 676
    Top = 229
    Width = 92
    Height = 31
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    Anchors = [akRight, akBottom]
    Kind = bkCancel
    NumGlyphs = 2
    TabOrder = 6
  end
  object lbxSyncOptions: TCheckListBox
    Left = 10
    Top = 148
    Width = 758
    Height = 74
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    Anchors = [akLeft, akTop, akRight, akBottom]
    Columns = 2
    TabOrder = 4
    OnClick = lbxSyncOptionsClick
  end
end
