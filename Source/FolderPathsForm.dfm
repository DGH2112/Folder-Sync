object frmFolderPaths: TfrmFolderPaths
  Left = 499
  Top = 388
  Caption = 'Folder Paths'
  ClientHeight = 286
  ClientWidth = 632
  Color = clBtnFace
  Constraints.MaxHeight = 320
  Constraints.MinHeight = 320
  Constraints.MinWidth = 640
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
    632
    286)
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
  object lblMaxFileSize: TLabel
    Left = 8
    Top = 258
    Width = 62
    Height = 13
    Anchors = [akLeft, akBottom]
    Caption = 'Ma&x File Size'
    FocusControl = edtMaxFileSize
    ExplicitTop = 246
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
    Top = 253
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Kind = bkOK
    NumGlyphs = 2
    TabOrder = 8
    ExplicitTop = 241
  end
  object btnCancel: TBitBtn
    Left = 549
    Top = 253
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Kind = bkCancel
    NumGlyphs = 2
    TabOrder = 9
    ExplicitTop = 241
  end
  object btnHelp: TBitBtn
    Left = 387
    Top = 253
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Kind = bkHelp
    NumGlyphs = 2
    TabOrder = 7
    OnClick = btnHelpClick
    ExplicitTop = 241
  end
  object edtMaxFileSize: TEdit
    Left = 85
    Top = 255
    Width = 100
    Height = 21
    Anchors = [akLeft, akBottom]
    NumbersOnly = True
    TabOrder = 5
    Text = 'edtMaxFileSize'
    ExplicitTop = 243
  end
  object cbxMaxFileSize: TComboBox
    Left = 191
    Top = 255
    Width = 82
    Height = 21
    Style = csDropDownList
    Anchors = [akLeft, akBottom]
    TabOrder = 6
    Items.Strings = (
      'Bytes'
      'KBytes'
      'MBytes'
      'GBytes'
      'TBytes')
    ExplicitTop = 243
  end
  object lbxSyncOptions: TListView
    Left = 8
    Top = 96
    Width = 616
    Height = 151
    Anchors = [akLeft, akTop, akRight, akBottom]
    Checkboxes = True
    Columns = <
      item
        AutoSize = True
        Caption = 'Folder Synchronization Options'
      end>
    HideSelection = False
    ReadOnly = True
    RowSelect = True
    TabOrder = 4
    ViewStyle = vsReport
    OnClick = lbxSyncOptionsClick
    ExplicitHeight = 139
  end
end
