object frmOptions: TfrmOptions
  Left = 502
  Top = 310
  Caption = 'Folder Sync Options'
  ClientHeight = 553
  ClientWidth = 772
  Color = clBtnFace
  Constraints.MinHeight = 591
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
    772
    553)
  PixelsPerInch = 96
  TextHeight = 16
  object btnOK: TBitBtn
    Left = 576
    Top = 518
    Width = 92
    Height = 31
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    Anchors = [akRight, akBottom]
    DoubleBuffered = True
    Kind = bkOK
    ParentDoubleBuffered = False
    TabOrder = 2
  end
  object btnCancel: TBitBtn
    Left = 675
    Top = 518
    Width = 92
    Height = 31
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    Anchors = [akRight, akBottom]
    DoubleBuffered = True
    Kind = bkCancel
    ParentDoubleBuffered = False
    TabOrder = 3
  end
  object PageControl1: TPageControl
    Left = 10
    Top = 10
    Width = 754
    Height = 505
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    ActivePage = pgFolderList
    Anchors = [akLeft, akTop, akRight, akBottom]
    TabOrder = 0
    object pgFolderList: TTabSheet
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      Caption = 'Folder List'
      DesignSize = (
        746
        474)
      object lvFolders: TListView
        Left = 5
        Top = 5
        Width = 735
        Height = 421
        Margins.Left = 4
        Margins.Top = 4
        Margins.Right = 4
        Margins.Bottom = 4
        Anchors = [akLeft, akTop, akRight, akBottom]
        Checkboxes = True
        Columns = <
          item
            Caption = 'Left Folder'
            Width = 369
          end
          item
            Caption = 'Right Folder'
            Width = 369
          end
          item
            Caption = 'Sync Option'
            Width = 62
          end>
        ReadOnly = True
        RowSelect = True
        TabOrder = 0
        ViewStyle = vsReport
        OnDblClick = lvFoldersDblClick
        OnResize = lvFoldersResize
      end
      object btnAdd: TBitBtn
        Left = 443
        Top = 435
        Width = 93
        Height = 31
        Margins.Left = 4
        Margins.Top = 4
        Margins.Right = 4
        Margins.Bottom = 4
        Anchors = [akRight, akBottom]
        Caption = '&Add'
        DoubleBuffered = True
        ParentDoubleBuffered = False
        TabOrder = 1
        OnClick = btnAddClick
      end
      object btnEdit: TBitBtn
        Left = 547
        Top = 435
        Width = 92
        Height = 31
        Margins.Left = 4
        Margins.Top = 4
        Margins.Right = 4
        Margins.Bottom = 4
        Anchors = [akRight, akBottom]
        Caption = '&Edit'
        DoubleBuffered = True
        ParentDoubleBuffered = False
        TabOrder = 2
        OnClick = btnEditClick
      end
      object btnDelete: TBitBtn
        Left = 650
        Top = 435
        Width = 92
        Height = 31
        Margins.Left = 4
        Margins.Top = 4
        Margins.Right = 4
        Margins.Bottom = 4
        Anchors = [akRight, akBottom]
        Caption = '&Delete'
        DoubleBuffered = True
        ParentDoubleBuffered = False
        TabOrder = 3
        OnClick = btnDeleteClick
      end
    end
    object pgExclusions: TTabSheet
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      Caption = 'E&xclusions'
      ImageIndex = 1
      DesignSize = (
        746
        474)
      object edtExclusions: TMemo
        Left = 5
        Top = 10
        Width = 735
        Height = 456
        Margins.Left = 4
        Margins.Top = 4
        Margins.Right = 4
        Margins.Bottom = 4
        Anchors = [akLeft, akTop, akRight, akBottom]
        ScrollBars = ssBoth
        TabOrder = 0
        WordWrap = False
      end
    end
    object tabCompareFiles: TTabSheet
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      Caption = '&Options'
      ImageIndex = 2
      DesignSize = (
        746
        474)
      object lblCompareFiles: TLabel
        Left = 4
        Top = 4
        Width = 85
        Height = 16
        Margins.Left = 4
        Margins.Top = 4
        Margins.Right = 4
        Margins.Bottom = 4
        Caption = 'Compare &EXE'
      end
      object lblAdvancedOptions: TLabel
        Left = 4
        Top = 58
        Width = 111
        Height = 16
        Caption = '&Advanced Options'
        FocusControl = lbxAdvancedOptions
      end
      object lblFontName: TLabel
        Left = 3
        Top = 425
        Width = 66
        Height = 16
        Anchors = [akLeft, akRight, akBottom]
        Caption = '&Font Name'
        FocusControl = cbxFontName
      end
      object lblFontSize: TLabel
        Left = 450
        Top = 425
        Width = 55
        Height = 16
        Anchors = [akRight, akBottom]
        Caption = 'Font &Size'
        FocusControl = cbxFontSize
        ExplicitLeft = 456
        ExplicitTop = 427
      end
      object lblFontStyle: TLabel
        Left = 562
        Top = 425
        Width = 59
        Height = 16
        Anchors = [akRight, akBottom]
        Caption = 'Font St&yle'
        FocusControl = cbxFontStyle
        ExplicitLeft = 568
        ExplicitTop = 427
      end
      object edtCompareEXE: TEdit
        Left = 4
        Top = 27
        Width = 637
        Height = 24
        Margins.Left = 4
        Margins.Top = 4
        Margins.Right = 4
        Margins.Bottom = 4
        Anchors = [akLeft, akTop, akRight]
        TabOrder = 0
        Text = 'edtCompareEXE'
      end
      object btnBrowse: TButton
        Left = 649
        Top = 25
        Width = 92
        Height = 30
        Margins.Left = 4
        Margins.Top = 4
        Margins.Right = 4
        Margins.Bottom = 4
        Anchors = [akTop, akRight]
        Caption = '&Browse'
        TabOrder = 1
        OnClick = btnBrowseClick
      end
      object lbxAdvancedOptions: TCheckListBox
        Left = 4
        Top = 81
        Width = 738
        Height = 337
        Margins.Left = 4
        Margins.Top = 4
        Margins.Right = 4
        Margins.Bottom = 4
        Anchors = [akLeft, akTop, akRight, akBottom]
        TabOrder = 2
      end
      object cbxFontName: TComboBox
        Left = 3
        Top = 447
        Width = 441
        Height = 24
        Style = csDropDownList
        Anchors = [akLeft, akRight, akBottom]
        TabOrder = 3
      end
      object cbxFontSize: TComboBox
        Left = 450
        Top = 447
        Width = 106
        Height = 24
        Style = csDropDownList
        Anchors = [akRight, akBottom]
        TabOrder = 4
      end
      object cbxFontStyle: TComboBox
        Left = 562
        Top = 447
        Width = 181
        Height = 24
        Style = csDropDownList
        Anchors = [akRight, akBottom]
        TabOrder = 5
      end
    end
  end
  object btnCheckforUpdates: TBitBtn
    Left = 8
    Top = 518
    Width = 185
    Height = 31
    Anchors = [akLeft, akTop, akBottom]
    Caption = 'Check for &Updates...'
    DoubleBuffered = True
    Glyph.Data = {
      F6000000424DF600000000000000760000002800000010000000100000000100
      0400000000008000000000000000000000001000000000000000000000000000
      8000008000000080800080000000800080008080000080808000C0C0C0000000
      FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00DDDDDDDDDDDD
      DDDDDDDDDDDDDDDDDDDDDCDCDCDDCDCDDDDDDCDCDCDDCDCDDDCDDCCCDCDDCDCC
      CDDDDCDC1CDDCDCDCDCDDCD9CCCCCCCCCDDDDDDD1DDDDDDDDDDDDDD91DDDDDA2
      DDDDDDD91DDDDDAA2DDDDDDD91DDDAAAA2DDDDDDD91DDA2DAA2DDDDDD91DAADD
      DAA2D91119DDADDDDDAADD999DDDDDDDDDDADDDDDDDDDDDDDDDD}
    ParentDoubleBuffered = False
    TabOrder = 1
    OnClick = btnCheckforUpdatesClick
  end
  object dlgOpen: TOpenDialog
    Filter = 'Executables (*.exe)|*.exe'
    Title = 'Select Compare EXE'
    Left = 701
    Top = 108
  end
end
