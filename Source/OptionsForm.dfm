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
  object lblTimeDiff: TLabel
    Left = 8
    Top = 422
    Width = 117
    Height = 13
    Anchors = [akLeft, akBottom]
    Caption = '&Time Different Tolerance'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
    ExplicitTop = 420
  end
  object edtTimeDiff: TEdit
    Left = 140
    Top = 418
    Width = 37
    Height = 21
    Anchors = [akLeft, akBottom]
    ReadOnly = True
    TabOrder = 0
    Text = '0'
  end
  object udTolerance: TUpDown
    Left = 177
    Top = 418
    Width = 16
    Height = 21
    Anchors = [akLeft, akBottom]
    Associate = edtTimeDiff
    Max = 60
    TabOrder = 1
  end
  object btnOK: TBitBtn
    Left = 473
    Top = 418
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    TabOrder = 2
    Kind = bkOK
  end
  object btnCancel: TBitBtn
    Left = 553
    Top = 418
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    TabOrder = 3
    Kind = bkCancel
  end
  object PageControl1: TPageControl
    Left = 8
    Top = 8
    Width = 618
    Height = 407
    ActivePage = pgFolderList
    Anchors = [akLeft, akTop, akRight, akBottom]
    TabOrder = 4
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
        Columns = <
          item
            Caption = 'Left Folder'
            Width = 300
          end
          item
            Caption = 'Right Folder'
            Width = 300
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
        TabOrder = 3
        OnClick = btnDeleteClick
      end
    end
    object pgExclusions: TTabSheet
      Caption = '&Exclusions'
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
  end
end
