object frmOptions: TfrmOptions
  Left = 502
  Top = 310
  Caption = 'Folder Sync Options'
  ClientHeight = 446
  ClientWidth = 632
  Color = clBtnFace
  Constraints.MinHeight = 480
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
    446)
  PixelsPerInch = 96
  TextHeight = 13
  object btnOK: TBitBtn
    Left = 473
    Top = 418
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    DoubleBuffered = True
    Kind = bkOK
    ParentDoubleBuffered = False
    TabOrder = 0
  end
  object btnCancel: TBitBtn
    Left = 553
    Top = 418
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    DoubleBuffered = True
    Kind = bkCancel
    ParentDoubleBuffered = False
    TabOrder = 1
  end
  object PageControl1: TPageControl
    Left = 8
    Top = 8
    Width = 618
    Height = 407
    ActivePage = pgFolderList
    Anchors = [akLeft, akTop, akRight, akBottom]
    TabOrder = 2
    object pgFolderList: TTabSheet
      Caption = 'Folder List'
      DesignSize = (
        610
        379)
      object lvFolders: TListView
        Left = 4
        Top = 4
        Width = 602
        Height = 339
        Anchors = [akLeft, akTop, akRight, akBottom]
        Checkboxes = True
        Columns = <
          item
            Caption = 'Left Folder'
            Width = 300
          end
          item
            Caption = 'Right Folder'
            Width = 300
          end
          item
            Caption = 'Sync Option'
          end>
        ReadOnly = True
        RowSelect = True
        TabOrder = 0
        ViewStyle = vsReport
        OnDblClick = lvFoldersDblClick
        OnResize = lvFoldersResize
      end
      object btnAdd: TBitBtn
        Left = 365
        Top = 350
        Width = 75
        Height = 25
        Anchors = [akRight, akBottom]
        Caption = '&Add'
        DoubleBuffered = True
        ParentDoubleBuffered = False
        TabOrder = 1
        OnClick = btnAddClick
      end
      object btnEdit: TBitBtn
        Left = 449
        Top = 350
        Width = 75
        Height = 25
        Anchors = [akRight, akBottom]
        Caption = '&Edit'
        DoubleBuffered = True
        ParentDoubleBuffered = False
        TabOrder = 2
        OnClick = btnEditClick
      end
      object btnDelete: TBitBtn
        Left = 533
        Top = 350
        Width = 75
        Height = 25
        Anchors = [akRight, akBottom]
        Caption = '&Delete'
        DoubleBuffered = True
        ParentDoubleBuffered = False
        TabOrder = 3
        OnClick = btnDeleteClick
      end
    end
    object pgExclusions: TTabSheet
      Caption = 'E&xclusions'
      ImageIndex = 1
      DesignSize = (
        610
        379)
      object edtExclusions: TMemo
        Left = 4
        Top = 8
        Width = 602
        Height = 367
        Anchors = [akLeft, akTop, akRight, akBottom]
        ScrollBars = ssBoth
        TabOrder = 0
        WordWrap = False
      end
    end
    object tabCompareFiles: TTabSheet
      Caption = '&Compare'
      ImageIndex = 2
      DesignSize = (
        610
        379)
      object lblCompareFiles: TLabel
        Left = 3
        Top = 3
        Width = 66
        Height = 13
        Caption = 'Compare &EXE'
      end
      object edtCompareEXE: TEdit
        Left = 3
        Top = 22
        Width = 523
        Height = 21
        Anchors = [akLeft, akTop, akRight]
        TabOrder = 0
        Text = 'edtCompareEXE'
      end
      object btnBrowse: TButton
        Left = 532
        Top = 20
        Width = 75
        Height = 25
        Anchors = [akTop, akRight]
        Caption = '&Browse'
        TabOrder = 1
        OnClick = btnBrowseClick
      end
    end
    object tabAdvancedOptions: TTabSheet
      Caption = '&Advanced Options'
      ImageIndex = 3
      DesignSize = (
        610
        379)
      object lbxAdvancedOptions: TCheckListBox
        Left = 3
        Top = 3
        Width = 604
        Height = 373
        Anchors = [akLeft, akTop, akRight, akBottom]
        ItemHeight = 13
        TabOrder = 0
      end
    end
  end
  object dlgOpen: TOpenDialog
    Filter = 'Executables (*.exe)|*.exe'
    Title = 'Select Compare EXE'
    Left = 53
    Top = 108
  end
end
