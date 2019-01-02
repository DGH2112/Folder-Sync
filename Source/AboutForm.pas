(**
  
  This module contains a new about dialogue that contains some simple information about
  the application as well as a image derived from a resource in the application.

  @Author  David Hoyle
  @Version 1.0
  @Date    02 Jan 2019
  
**)
Unit AboutForm;

Interface

Uses
  Winapi.Windows,
  Winapi.Messages,
  System.SysUtils,
  System.Variants,
  System.Classes,
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.Dialogs,
  Vcl.StdCtrls,
  Vcl.ExtCtrls;

Type
  (** A class to represent a form interface for the About dialogue. **)
  TfrmAboutDialogue = Class(TForm)
    imgSplashImage: TImage;
    tmTimer: TTimer;
    lblAppName: TLabel;
    lblBuild: TLabel;
    lblCopyright: TLabel;
    lblPlatform: TLabel;
    Procedure AboutClick(Sender: TObject);
    Procedure FormCreate(Sender: TObject);
  Private
    { Private declarations }
  Public
    { Public declarations }
    Class Procedure ShowAbout(Const Parent: TForm);
  End;

Implementation

{$R *.dfm}

Uses
  FldrSync.Functions;

Var
  (** This is an internal variable to hold the singleton reference to the about form. **)
  frm: TfrmAboutDialogue;

(**

  This is an on click event handler for the form, image and labels.

  @precon  None.
  @postcon Closes the form.

  @param   Sender as a TObject

**)
Procedure TfrmAboutDialogue.AboutClick(Sender: TObject);

Begin
  Close;
  tmTimer.Enabled := False;
End;

(**

  This is an OnFormCreate Event Handler for the TfrmAboutDialogue class.

  @precon  None.
  @postcon Loads an image into the image control (resizing the dialogue) if found in the
           applications resources and updates the labels with application title, build
           platforms, etc.

  @param   Sender as a TObject

**)
Procedure TfrmAboutDialogue.FormCreate(Sender: TObject);

Const
  strRevisions = ' abcdefghijklmnopqrstuvwxyz';
  strTitle     = '%s %d.%d%s';
  strSplashScreen = 'ApplicationSplashScreen';
  strDateFmt = 'dd/mmm/yyyy';
  {$IFDEF WIN32}
  str32Bit = '32-bit';
  {$ELSE}
  str64Bit = '64-bit';
  {$ENDIF}

ResourceString
  strBuild = 'Build %d.%d.%d.%d';
  strWrittenBy = 'Written by David Hoyle - Copyright ';

Var
  iMajor, iMinor, iBugFix, iBuild: Integer;
  dtDate                         : TDateTime;
  BM                             : TBitMap;

Begin
  If FindResource(hInstance, strSplashScreen, RT_BITMAP) > 0 Then
    Begin
      BM := TBitMap.Create;
      Try
        BM.LoadFromResourceName(hInstance, strSplashScreen);
        Width := BM.Width;
        Height := BM.Height;
        imgSplashImage.Picture.Assign(BM);
      Finally
        BM.Free;
      End;
    End;
  TFSFunctions.GetBuildNumber(ParamStr(0), iMajor, iMinor, iBugFix, iBuild);
  lblAppName.Caption := Format(strTitle, [Application.Title, iMajor, iMinor,
      strRevisions[iBugFix + 1]]);
  lblBuild.Caption := Format(strBuild, [iMajor, iMinor, iBugFix, iBuild]);
  FileAge(ParamStr(0), dtDate);
  lblCopyright.Caption := strWrittenBy +
    FormatDateTime(strDateFmt, dtDate);
  {$IFDEF WIN32}
  lblPlatform.Caption := str32Bit;
  {$ELSE}
  lblPlatform.Caption := str64Bit;
  {$ENDIF}
End;

(**

  This is the class method to invoke the form.

  @precon  None.
  @postcon Displays (creates if not existing) the form.

  @param   Parent as a TForm as a constant

**)
Class Procedure TfrmAboutDialogue.ShowAbout(Const Parent: TForm);

Begin
  If frm = Nil Then
    frm := TfrmAboutDialogue.Create(Parent);
  frm.tmTimer.Enabled := True;
  frm.Show;
End;

(** Ensures that the internal form variable is initialised to NIL. **)
Initialization
  frm := Nil;
End.
