object frmOptions: TfrmOptions
  Left = 502
  Top = 310
  Caption = 'Folder Sync Options'
  ClientHeight = 441
  ClientWidth = 670
  Color = clBtnFace
  Constraints.MinHeight = 250
  Constraints.MinWidth = 400
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poMainFormCenter
  DesignSize = (
    670
    441)
  PixelsPerInch = 96
  TextHeight = 13
  object lblTimeDiff: TLabel
    Left = 8
    Top = 417
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
    Top = 413
    Width = 37
    Height = 21
    Anchors = [akLeft, akBottom]
    ReadOnly = True
    TabOrder = 0
    Text = '0'
    ExplicitTop = 416
  end
  object udTolerance: TUpDown
    Left = 177
    Top = 413
    Width = 16
    Height = 21
    Anchors = [akLeft, akBottom]
    Associate = edtTimeDiff
    Max = 60
    TabOrder = 1
    ExplicitTop = 416
  end
  object btnOK: TBitBtn
    Left = 511
    Top = 413
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    TabOrder = 2
    Kind = bkOK
    ExplicitLeft = 500
    ExplicitTop = 416
  end
  object btnCancel: TBitBtn
    Left = 591
    Top = 413
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    TabOrder = 3
    Kind = bkCancel
    ExplicitLeft = 580
    ExplicitTop = 416
  end
  object PageControl1: TPageControl
    Left = 8
    Top = 8
    Width = 656
    Height = 402
    ActivePage = pgFolderList
    Anchors = [akLeft, akTop, akRight, akBottom]
    TabOrder = 4
    ExplicitWidth = 645
    ExplicitHeight = 405
    object pgFolderList: TTabSheet
      Caption = 'Folder List'
      ExplicitLeft = 0
      ExplicitTop = 25
      ExplicitWidth = 637
      ExplicitHeight = 377
      DesignSize = (
        648
        374)
      object lvFolders: TListView
        Left = 4
        Top = 4
        Width = 640
        Height = 334
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
        ExplicitWidth = 629
        ExplicitHeight = 337
      end
      object btnAdd: TBitBtn
        Left = 403
        Top = 345
        Width = 75
        Height = 25
        Anchors = [akRight, akBottom]
        Caption = '&Add'
        TabOrder = 1
        OnClick = btnAddClick
        ExplicitLeft = 392
        ExplicitTop = 348
      end
      object btnEdit: TBitBtn
        Left = 487
        Top = 345
        Width = 75
        Height = 25
        Anchors = [akRight, akBottom]
        Caption = '&Edit'
        TabOrder = 2
        OnClick = btnEditClick
        ExplicitLeft = 476
        ExplicitTop = 348
      end
      object btnDelete: TBitBtn
        Left = 571
        Top = 345
        Width = 75
        Height = 25
        Anchors = [akRight, akBottom]
        Caption = '&Delete'
        TabOrder = 3
        OnClick = btnDeleteClick
        ExplicitLeft = 560
        ExplicitTop = 348
      end
    end
    object pgExclusions: TTabSheet
      Caption = '&Exclusions'
      ImageIndex = 1
      ExplicitWidth = 637
      ExplicitHeight = 377
      DesignSize = (
        648
        374)
      object edtExclusions: TMemo
        Left = 4
        Top = 8
        Width = 640
        Height = 362
        Anchors = [akLeft, akTop, akRight, akBottom]
        ScrollBars = ssBoth
        TabOrder = 0
        WordWrap = False
        ExplicitWidth = 629
        ExplicitHeight = 365
      end
    end
  end
end
