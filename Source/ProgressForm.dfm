object frmProgress: TfrmProgress
  Left = 435
  Top = 463
  BorderStyle = bsDialog
  Caption = 'Progress'
  ClientHeight = 87
  ClientWidth = 592
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  FormStyle = fsStayOnTop
  OldCreateOrder = False
  Position = poMainFormCenter
  PixelsPerInch = 96
  TextHeight = 13
  object lblPath: TLabel
    Left = 4
    Top = 4
    Width = 585
    Height = 13
    AutoSize = False
    Caption = 'lblPath'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clNavy
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold, fsUnderline]
    ParentFont = False
    Layout = tlCenter
  end
  object lblCount: TLabel
    Left = 4
    Top = 24
    Width = 589
    Height = 13
    AutoSize = False
    Caption = 'lblCount'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clMaroon
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentFont = False
    Layout = tlCenter
  end
  object lblFile: TLabel
    Left = 4
    Top = 44
    Width = 585
    Height = 41
    AutoSize = False
    Caption = 'lblFile'
    Layout = tlCenter
    WordWrap = True
  end
end
