object frmOptions: TfrmOptions
  Left = 502
  Top = 310
  BorderStyle = bsDialog
  Caption = 'Folder Sync Options'
  ClientHeight = 444
  ClientWidth = 659
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poMainFormCenter
  PixelsPerInch = 96
  TextHeight = 13
  object lblTimeDiff: TLabel
    Left = 8
    Top = 420
    Width = 117
    Height = 13
    Caption = '&Time Different Tolerance'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
  end
  object edtTimeDiff: TEdit
    Left = 140
    Top = 416
    Width = 37
    Height = 21
    ReadOnly = True
    TabOrder = 0
    Text = '0'
  end
  object udTolerance: TUpDown
    Left = 177
    Top = 416
    Width = 16
    Height = 21
    Associate = edtTimeDiff
    Min = 0
    Max = 60
    Position = 0
    TabOrder = 1
    Wrap = False
  end
  object btnOK: TBitBtn
    Left = 500
    Top = 416
    Width = 75
    Height = 25
    TabOrder = 2
    Kind = bkOK
  end
  object btnCancel: TBitBtn
    Left = 580
    Top = 416
    Width = 75
    Height = 25
    TabOrder = 3
    Kind = bkCancel
  end
  object PageControl1: TPageControl
    Left = 8
    Top = 8
    Width = 645
    Height = 405
    ActivePage = pgFolderList
    TabOrder = 4
    object pgFolderList: TTabSheet
      Caption = 'Folder List'
      object lvFolders: TListView
        Left = 4
        Top = 4
        Width = 629
        Height = 337
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
      end
      object btnAdd: TBitBtn
        Left = 392
        Top = 348
        Width = 75
        Height = 25
        Caption = '&Add'
        TabOrder = 1
        OnClick = btnAddClick
      end
      object btnEdit: TBitBtn
        Left = 476
        Top = 348
        Width = 75
        Height = 25
        Caption = '&Edit'
        TabOrder = 2
        OnClick = btnEditClick
      end
      object btnDelete: TBitBtn
        Left = 560
        Top = 348
        Width = 75
        Height = 25
        Caption = '&Delete'
        TabOrder = 3
        OnClick = btnDeleteClick
      end
    end
    object pgExclusions: TTabSheet
      Caption = '&Exclusions'
      ImageIndex = 1
      object edtExclusions: TMemo
        Left = 4
        Top = 8
        Width = 629
        Height = 365
        ScrollBars = ssBoth
        TabOrder = 0
        WordWrap = False
      end
    end
  end
end
