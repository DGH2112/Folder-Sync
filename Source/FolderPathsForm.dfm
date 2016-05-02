object frmFolderPaths: TfrmFolderPaths
  Left = 499
  Top = 388
  Caption = 'Folder Paths'
  ClientHeight = 356
  ClientWidth = 772
  Color = clBtnFace
  Constraints.MaxHeight = 485
  Constraints.MinHeight = 394
  Constraints.MinWidth = 788
  DoubleBuffered = True
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
    772
    356)
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
  object lblMaxFileSize: TLabel
    Left = 10
    Top = 322
    Width = 79
    Height = 16
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    Anchors = [akLeft, akBottom]
    Caption = 'Ma&x File Size'
    FocusControl = edtMaxFileSize
    ExplicitTop = 318
  end
  object edtLeftFolder: TEdit
    Left = 10
    Top = 31
    Width = 704
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
    Width = 704
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
    Left = 721
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
    Left = 721
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
    Left = 570
    Top = 315
    Width = 92
    Height = 31
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    Anchors = [akRight, akBottom]
    Kind = bkOK
    NumGlyphs = 2
    TabOrder = 8
  end
  object btnCancel: TBitBtn
    Left = 670
    Top = 315
    Width = 92
    Height = 31
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    Anchors = [akRight, akBottom]
    Kind = bkCancel
    NumGlyphs = 2
    TabOrder = 9
  end
  object btnHelp: TBitBtn
    Left = 470
    Top = 315
    Width = 93
    Height = 31
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    Anchors = [akRight, akBottom]
    Kind = bkHelp
    NumGlyphs = 2
    TabOrder = 7
    OnClick = btnHelpClick
  end
  object edtMaxFileSize: TEdit
    Left = 105
    Top = 318
    Width = 123
    Height = 24
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    Anchors = [akLeft, akBottom]
    NumbersOnly = True
    TabOrder = 5
    Text = 'edtMaxFileSize'
  end
  object cbxMaxFileSize: TComboBox
    Left = 235
    Top = 318
    Width = 101
    Height = 24
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    Style = csDropDownList
    Anchors = [akLeft, akBottom]
    TabOrder = 6
    Items.Strings = (
      'Bytes'
      'KBytes'
      'MBytes'
      'GBytes'
      'TBytes')
  end
  object lbxSyncOptions: TListView
    Left = 11
    Top = 117
    Width = 752
    Height = 190
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    Anchors = [akLeft, akTop, akRight, akBottom]
    Checkboxes = True
    Columns = <
      item
        Caption = 'Folder Synchronization Options'
        Width = 725
      end>
    HideSelection = False
    ReadOnly = True
    RowSelect = True
    TabOrder = 4
    ViewStyle = vsReport
    OnChange = lbxSyncOptionsChange
  end
end
