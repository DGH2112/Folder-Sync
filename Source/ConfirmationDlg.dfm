object frmConfirmationDlg: TfrmConfirmationDlg
  Left = 0
  Top = 0
  BorderIcons = []
  Caption = 'Please confirm the action'
  ClientHeight = 181
  ClientWidth = 492
  Color = clBtnFace
  Constraints.MaxHeight = 215
  Constraints.MaxWidth = 500
  Constraints.MinHeight = 215
  Constraints.MinWidth = 500
  DoubleBuffered = True
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  FormStyle = fsStayOnTop
  OldCreateOrder = False
  Position = poDesigned
  DesignSize = (
    492
    181)
  PixelsPerInch = 96
  TextHeight = 13
  object lblMessage: TLabel
    Left = 8
    Top = 8
    Width = 476
    Height = 29
    Anchors = [akLeft, akTop, akRight]
    AutoSize = False
    Caption = 'lblMessage'
    Color = clBtnFace
    ParentColor = False
    Transparent = True
    WordWrap = True
    ExplicitWidth = 454
  end
  object lblLabelLine1: TLabel
    Left = 8
    Top = 43
    Width = 71
    Height = 13
    AutoSize = False
    Caption = 'lblLabelLine1'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object lblInformation1: TLabel
    Left = 85
    Top = 43
    Width = 399
    Height = 13
    Anchors = [akLeft, akTop, akRight]
    AutoSize = False
    Caption = 'lblInformation1'
    EllipsisPosition = epPathEllipsis
    ExplicitWidth = 377
  end
  object lblLabelLine2: TLabel
    Left = 8
    Top = 62
    Width = 71
    Height = 13
    AutoSize = False
    Caption = 'lblLabelLine2'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object lblInformation2: TLabel
    Left = 85
    Top = 62
    Width = 399
    Height = 13
    Anchors = [akLeft, akTop, akRight]
    AutoSize = False
    Caption = 'lblInformation2'
    EllipsisPosition = epPathEllipsis
    ExplicitWidth = 377
  end
  object lblLabelLine3: TLabel
    Left = 8
    Top = 81
    Width = 71
    Height = 13
    AutoSize = False
    Caption = 'lblLabelLine3'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object lblInformation3: TLabel
    Left = 85
    Top = 81
    Width = 399
    Height = 13
    Anchors = [akLeft, akTop, akRight]
    AutoSize = False
    Caption = 'lblInformation3'
    EllipsisPosition = epPathEllipsis
    ExplicitWidth = 377
  end
  object lblSourceInfo: TLabel
    Left = 85
    Top = 100
    Width = 399
    Height = 13
    Alignment = taRightJustify
    Anchors = [akLeft, akTop, akRight]
    AutoSize = False
    Caption = 'lblSourceInfo'
    ExplicitWidth = 391
  end
  object lblDestInfo: TLabel
    Left = 85
    Top = 119
    Width = 399
    Height = 13
    Alignment = taRightJustify
    Anchors = [akLeft, akTop, akRight]
    AutoSize = False
    Caption = 'lblSourceInfo'
    ExplicitWidth = 391
  end
  object imgIcon: TImage
    Left = 8
    Top = 125
    Width = 48
    Height = 48
    Anchors = [akLeft, akBottom]
    Center = True
    Stretch = True
    ExplicitTop = 121
  end
  object btnYes: TBitBtn
    Left = 65
    Top = 148
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = '&Yes'
    Glyph.Data = {
      DE010000424DDE01000000000000760000002800000024000000120000000100
      0400000000006801000000000000000000001000000000000000000000000000
      80000080000000808000800000008000800080800000C0C0C000808080000000
      FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00333333333333
      3333333333333333333333330000333333333333333333333333F33333333333
      00003333344333333333333333388F3333333333000033334224333333333333
      338338F3333333330000333422224333333333333833338F3333333300003342
      222224333333333383333338F3333333000034222A22224333333338F338F333
      8F33333300003222A3A2224333333338F3838F338F33333300003A2A333A2224
      33333338F83338F338F33333000033A33333A222433333338333338F338F3333
      0000333333333A222433333333333338F338F33300003333333333A222433333
      333333338F338F33000033333333333A222433333333333338F338F300003333
      33333333A222433333333333338F338F00003333333333333A22433333333333
      3338F38F000033333333333333A223333333333333338F830000333333333333
      333A333333333333333338330000333333333333333333333333333333333333
      0000}
    ModalResult = 6
    NumGlyphs = 2
    TabOrder = 0
  end
  object btnNo: TBitBtn
    Left = 146
    Top = 148
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Kind = bkNo
    NumGlyphs = 2
    TabOrder = 1
  end
  object btnYesToAll: TBitBtn
    Left = 227
    Top = 148
    Width = 85
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'Yes to &All'
    Kind = bkAll
    NumGlyphs = 2
    TabOrder = 2
  end
  object btnCancel: TBitBtn
    Left = 409
    Top = 148
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Kind = bkCancel
    NumGlyphs = 2
    TabOrder = 4
  end
  object btnNoToAll: TBitBtn
    Left = 318
    Top = 148
    Width = 85
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'N&o to All'
    Glyph.Data = {
      4E010000424D4E01000000000000760000002800000012000000120000000100
      040000000000D8000000130B0000130B00001000000010000000000000000000
      80000080000000808000800000008000800080800000C0C0C000808080000000
      FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00333333388888
      8333330000003333388111111883330000003333888888891118330000003338
      8111111881118300000033811199991118118300000039119933331111811800
      0000391133333811118918000000911833338111911818000000918333381119
      3918180000009183338111933918180000009183381119333918180000009183
      8111933339188300000091181119333391188300000039111193333391883300
      0000391118333381118333000000339111888811183333000000333991111119
      933333000000333339999993333333000000}
    ModalResult = 5
    TabOrder = 3
  end
end
