object frmDiskSpace: TfrmDiskSpace
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu]
  Caption = 'Process Files'
  ClientHeight = 187
  ClientWidth = 635
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
    635
    187)
  PixelsPerInch = 96
  TextHeight = 13
  object lblDiskSpace: TLabel
    Left = 8
    Top = 8
    Width = 619
    Height = 13
    Anchors = [akLeft, akTop, akRight]
    AutoSize = False
    Caption = '&Disk Space (in kbytes):'
  end
  object btnOK: TBitBtn
    Left = 471
    Top = 154
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Kind = bkOK
    NumGlyphs = 2
    TabOrder = 0
  end
  object btnCancel: TBitBtn
    Left = 552
    Top = 154
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Kind = bkCancel
    NumGlyphs = 2
    TabOrder = 1
  end
  object vstDiskSpace: TVirtualStringTree
    Left = 8
    Top = 24
    Width = 619
    Height = 124
    Anchors = [akLeft, akTop, akRight, akBottom]
    Header.AutoSizeIndex = 0
    Header.Font.Charset = DEFAULT_CHARSET
    Header.Font.Color = clWindowText
    Header.Font.Height = -11
    Header.Font.Name = 'Tahoma'
    Header.Font.Style = []
    Header.Options = [hoAutoResize, hoColumnResize, hoDrag, hoShowSortGlyphs, hoVisible]
    TabOrder = 2
    TreeOptions.PaintOptions = [toShowButtons, toShowDropmark, toShowHorzGridLines, toShowTreeLines, toShowVertGridLines, toThemeAware, toUseBlendedImages, toFullVertGridLines, toAlwaysHideSelection]
    OnAfterItemErase = vstDiskSpaceAfterItemErase
    OnGetText = vstDiskSpaceGetText
    Columns = <
      item
        Position = 0
        Width = 115
        WideText = 'Drive'
      end
      item
        Alignment = taRightJustify
        Position = 1
        Width = 100
        WideText = 'Total'
      end
      item
        Alignment = taRightJustify
        Position = 2
        Width = 100
        WideText = 'Free at Start'
      end
      item
        Alignment = taRightJustify
        Position = 3
        Width = 100
        WideText = 'Deletes'
      end
      item
        Alignment = taRightJustify
        Position = 4
        Width = 100
        WideText = 'Adds'
      end
      item
        Alignment = taRightJustify
        Position = 5
        Width = 100
        WideText = 'Free at Finish'
      end>
  end
end
