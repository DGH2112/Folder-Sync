object frmErrorDlg: TfrmErrorDlg
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu]
  Caption = 'Processing Error'
  ClientHeight = 187
  ClientWidth = 534
  Color = clBtnFace
  Constraints.MinHeight = 225
  Constraints.MinWidth = 550
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poMainFormCenter
  OnCreate = FormCreate
  DesignSize = (
    534
    187)
  PixelsPerInch = 96
  TextHeight = 13
  object lblMessage: TLabel
    Left = 8
    Top = 8
    Width = 518
    Height = 13
    Anchors = [akLeft, akTop, akRight]
    AutoSize = False
    Caption = 'lblMessage'
    ExplicitWidth = 619
  end
  object lblSourceLabel: TLabel
    Left = 18
    Top = 27
    Width = 68
    Height = 13
    AutoSize = False
    Caption = '&Source:'
  end
  object lblSourceFile: TLabel
    Left = 92
    Top = 27
    Width = 434
    Height = 13
    Anchors = [akLeft, akTop, akRight]
    AutoSize = False
    Caption = 'lblSourceFile'
    EllipsisPosition = epPathEllipsis
    ExplicitWidth = 535
  end
  object lblDestFile: TLabel
    Left = 92
    Top = 46
    Width = 434
    Height = 13
    Anchors = [akLeft, akTop, akRight]
    AutoSize = False
    Caption = 'lblDestFile'
    EllipsisPosition = epPathEllipsis
    ExplicitWidth = 411
  end
  object lblDestLabel: TLabel
    Left = 18
    Top = 46
    Width = 68
    Height = 13
    AutoSize = False
    Caption = '&Destination:'
  end
  object lblOSError: TLabel
    Left = 92
    Top = 65
    Width = 434
    Height = 64
    Anchors = [akLeft, akTop, akRight, akBottom]
    AutoSize = False
    Caption = 'lblOSError'
    WordWrap = True
    ExplicitWidth = 411
    ExplicitHeight = 104
  end
  object lblOSErrorLabel: TLabel
    Left = 18
    Top = 65
    Width = 68
    Height = 13
    AutoSize = False
    Caption = '&OS Error:'
  end
  object imgIcon: TImage
    Left = 8
    Top = 131
    Width = 48
    Height = 48
    Anchors = [akLeft, akBottom]
    Center = True
    Stretch = True
    ExplicitTop = 171
  end
  object lblWhatToDo: TLabel
    Left = 62
    Top = 135
    Width = 464
    Height = 13
    Anchors = [akLeft, akRight, akBottom]
    AutoSize = False
    Caption = 'Do you want to [I]gnore the error or [S]top processing?'
    ExplicitTop = 175
    ExplicitWidth = 441
  end
  object btnIgnoreAll: TBitBtn
    Left = 345
    Top = 154
    Width = 100
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = '&Ignore ALL'
    Glyph.Data = {
      DE010000424DDE01000000000000760000002800000024000000120000000100
      0400000000006801000000000000000000001000000000000000000000000000
      80000080000000808000800000008000800080800000C0C0C000808080000000
      FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00344446333334
      44433333FFFF333333FFFF33000033AAA43333332A4333338833F33333883F33
      00003332A46333332A4333333383F33333383F3300003332A2433336A6633333
      33833F333383F33300003333AA463362A433333333383F333833F33300003333
      6AA4462A46333333333833FF833F33330000333332AA22246333333333338333
      33F3333300003333336AAA22646333333333383333F8FF33000033444466AA43
      6A43333338FFF8833F383F330000336AA246A2436A43333338833F833F383F33
      000033336A24AA442A433333333833F33FF83F330000333333A2AA2AA4333333
      333383333333F3330000333333322AAA4333333333333833333F333300003333
      333322A4333333333333338333F333330000333333344A433333333333333338
      3F333333000033333336A24333333333333333833F333333000033333336AA43
      33333333333333833F3333330000333333336663333333333333333888333333
      0000}
    ModalResult = 14
    NumGlyphs = 2
    TabOrder = 2
  end
  object btnStop: TBitBtn
    Left = 451
    Top = 154
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Cancel = True
    Caption = 'Abort'
    Default = True
    Glyph.Data = {
      DE010000424DDE01000000000000760000002800000024000000120000000100
      0400000000006801000000000000000000001000000000000000000000000000
      80000080000000808000800000008000800080800000C0C0C000808080000000
      FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00333333333333
      333333333333333333333333000033338833333333333333333F333333333333
      0000333911833333983333333388F333333F3333000033391118333911833333
      38F38F333F88F33300003339111183911118333338F338F3F8338F3300003333
      911118111118333338F3338F833338F3000033333911111111833333338F3338
      3333F8330000333333911111183333333338F333333F83330000333333311111
      8333333333338F3333383333000033333339111183333333333338F333833333
      00003333339111118333333333333833338F3333000033333911181118333333
      33338333338F333300003333911183911183333333383338F338F33300003333
      9118333911183333338F33838F338F33000033333913333391113333338FF833
      38F338F300003333333333333919333333388333338FFF830000333333333333
      3333333333333333333888330000333333333333333333333333333333333333
      0000}
    ModalResult = 3
    NumGlyphs = 2
    TabOrder = 0
  end
  object btnIgnoreOnce: TBitBtn
    Left = 239
    Top = 154
    Width = 100
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = '&Ignore Once'
    Kind = bkIgnore
    NumGlyphs = 2
    TabOrder = 1
  end
end
