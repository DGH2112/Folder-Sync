object frmAboutDialogue: TfrmAboutDialogue
  Left = 0
  Top = 0
  BorderIcons = []
  BorderStyle = bsNone
  ClientHeight = 161
  ClientWidth = 293
  Color = clBtnFace
  DoubleBuffered = True
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  FormStyle = fsStayOnTop
  OldCreateOrder = False
  Position = poOwnerFormCenter
  OnClick = AboutClick
  OnCreate = FormCreate
  DesignSize = (
    293
    161)
  PixelsPerInch = 96
  TextHeight = 13
  object imgSplashImage: TImage
    Left = 0
    Top = 0
    Width = 293
    Height = 161
    Align = alClient
    Transparent = True
    OnClick = AboutClick
    ExplicitLeft = 29
    ExplicitTop = 29
    ExplicitWidth = 495
    ExplicitHeight = 247
  end
  object lblAppName: TDGHCustomLabel
    Left = 20
    Top = 20
    Width = 253
    Height = 29
    Anchors = [akLeft, akTop, akRight]
    AutoSize = False
    Caption = 'lblAppName'
    Color = clBtnFace
    Font.Charset = DEFAULT_CHARSET
    Font.Color = 3158064
    Font.Height = -24
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentColor = False
    ParentFont = False
    ShowAccelChar = False
    Transparent = True
    OnClick = AboutClick
  end
  object lblBuild: TDGHCustomLabel
    Left = 40
    Top = 55
    Width = 233
    Height = 16
    Anchors = [akLeft, akTop, akRight]
    AutoSize = False
    Caption = 'lblBuild'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = 3158064
    Font.Height = -13
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    Transparent = True
    OnClick = AboutClick
  end
  object lblCopyright: TDGHCustomLabel
    Left = 20
    Top = 125
    Width = 253
    Height = 16
    Anchors = [akLeft, akRight, akBottom]
    AutoSize = False
    Caption = 'lblCopyright'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = 3158064
    Font.Height = -13
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    Transparent = True
  end
  object lblPlatform: TDGHCustomLabel
    Left = 40
    Top = 77
    Width = 233
    Height = 16
    Anchors = [akLeft, akTop, akRight]
    AutoSize = False
    Caption = 'lblPlatform'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = 3158064
    Font.Height = -13
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    Transparent = True
  end
  object tmTimer: TTimer
    Enabled = False
    Interval = 10000
    OnTimer = AboutClick
    Left = 171
    Top = 35
  end
end
