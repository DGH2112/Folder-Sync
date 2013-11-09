object frmDiskSpace: TfrmDiskSpace
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu]
  Caption = 'Process Files'
  ClientHeight = 187
  ClientWidth = 624
  Color = clBtnFace
  Constraints.MinHeight = 225
  Constraints.MinWidth = 640
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poMainFormCenter
  DesignSize = (
    624
    187)
  PixelsPerInch = 96
  TextHeight = 13
  object lblDiskSpace: TLabel
    Left = 8
    Top = 8
    Width = 608
    Height = 13
    Anchors = [akLeft, akTop, akRight]
    AutoSize = False
    Caption = '&Disk Space (in kbytes):'
    ExplicitWidth = 619
  end
  object lvDiskSpace: TListView
    Left = 8
    Top = 27
    Width = 608
    Height = 121
    Anchors = [akLeft, akTop, akRight, akBottom]
    Columns = <
      item
        Caption = 'Drive'
        Width = 100
      end
      item
        Alignment = taRightJustify
        Caption = 'Total'
        Width = 100
      end
      item
        Alignment = taRightJustify
        Caption = 'Free at Start'
        Width = 100
      end
      item
        Alignment = taRightJustify
        Caption = 'Deletes'
        Width = 100
      end
      item
        Alignment = taRightJustify
        Caption = 'Adds'
        Width = 100
      end
      item
        Alignment = taRightJustify
        Caption = 'Free at Finish'
        Width = 100
      end>
    GridLines = True
    HideSelection = False
    ReadOnly = True
    RowSelect = True
    TabOrder = 0
    ViewStyle = vsReport
    OnResize = lvDiskSpaceResize
    ExplicitWidth = 626
    ExplicitHeight = 128
  end
  object btnOK: TBitBtn
    Left = 460
    Top = 154
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Kind = bkOK
    NumGlyphs = 2
    TabOrder = 1
    ExplicitLeft = 478
    ExplicitTop = 161
  end
  object btnCancel: TBitBtn
    Left = 541
    Top = 154
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Kind = bkCancel
    NumGlyphs = 2
    TabOrder = 2
    ExplicitLeft = 559
    ExplicitTop = 161
  end
end
