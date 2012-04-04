object frmFolderPaths: TfrmFolderPaths
  Left = 499
  Top = 388
  Caption = 'Folder Paths'
  ClientHeight = 159
  ClientWidth = 778
  Color = clBtnFace
  Constraints.MaxHeight = 242
  Constraints.MinHeight = 197
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
    159)
  PixelsPerInch = 96
  TextHeight = 16
  object lblLeftFolder: TLabel
    Left = 5
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
    Left = 5
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
  object lblSyncOption: TLabel
    Left = 10
    Top = 124
    Width = 72
    Height = 16
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    Caption = 'Sync &Option'
    FocusControl = cbxSyncOption
  end
  object edtLeftFolder: TEdit
    Left = 6
    Top = 31
    Width = 716
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
    Left = 6
    Top = 85
    Width = 716
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
    Left = 730
    Top = 25
    Width = 40
    Height = 30
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
    Left = 730
    Top = 79
    Width = 40
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
    Left = 577
    Top = 118
    Width = 93
    Height = 31
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    Anchors = [akTop, akRight]
    DoubleBuffered = True
    Kind = bkOK
    ParentDoubleBuffered = False
    TabOrder = 5
  end
  object btnCancel: TBitBtn
    Left = 681
    Top = 118
    Width = 92
    Height = 31
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    Anchors = [akTop, akRight]
    DoubleBuffered = True
    Kind = bkCancel
    ParentDoubleBuffered = False
    TabOrder = 6
  end
  object cbxSyncOption: TComboBox
    Left = 128
    Top = 121
    Width = 228
    Height = 24
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    Style = csDropDownList
    TabOrder = 4
    Items.Strings = (
      'Synchronise'
      'Primary Left'
      'Primary Right')
  end
end
