(**

  This module represents a form for displaying the applications About
  information.

  @Author  David Hoyle
  @Version 1.0
  @Date    09 Jul 2012

**)
Unit About;

Interface

Uses
  SysUtils,
  WinTypes,
  WinProcs,
  Messages,
  Classes,
  Graphics,
  Controls,
  Forms,
  Dialogs,
  ExtCtrls,
  StdCtrls,
  DGHSpectrum;

{$I '..\..\..\Library\CompilerDefinitions.inc'}

Type
  (** This is a class to represent the About form which displays the
      applications information. **)
  TfrmAbout = Class(TForm)
    MainPanel: TPanel;
    TitleLabel: TLabel;
    AboutTimer: TTimer;
    AnthenaImage: TImage;
    ClientNameLabel: TLabel;
    ClientCompanyLabel: TLabel;
    PhysMemLabel: TLabel;
    LicensedToLabel: TLabel;
    WinLabel: TLabel;
    BuildLabel: TLabel;
    lblBy: TLabel;
    DGHSpectrum1: TDGHSpectrum;
    Procedure AboutTimerTimer(Sender: TObject);
    Procedure FormKeyPress(Sender: TObject; Var Key: Char);
    Procedure FormClose(Sender: TObject; Var Action: TCloseAction);
    Procedure FormCreate(Sender: TObject);
    Procedure FormShow(Sender: TObject);
  Private
    { Private declarations }
    FModuleFileName: String;
    Procedure BuildNumber(Var iMajor, iMinor, iBugFix, iBuild: Integer);
    Procedure VersionInformation;
  Public
    { Public declarations }
    Class Procedure ShowAbout;
  End;

Implementation

{$R *.DFM}

Uses
  Registry;

Var
  (** This is a private variable for an instance of the form used to implement
      this forms as a singleton class. **)
  frm: TfrmAbout;

(**

  This is an on timer event handler for the timer control.

  @precon  None.
  @postcon Disabled the timer and Closes the form and destroys its instance.

  @param   Sender as a TObject

**)
Procedure TfrmAbout.AboutTimerTimer(Sender: TObject);

Begin
  AboutTimer.Enabled := False;
  Close;
End;

(**

  This is an on click event handler for all controls on the form.

  @precon  None.
  @postcon Closes the form and destroy the instance when an item is clicked.

  @param   Sender as a TObject
  @param   Key    as a Char as a reference

**)
Procedure TfrmAbout.FormKeyPress(Sender: TObject; Var Key: Char);

Begin
  Close;
End;

(**

  This is a form on close event handler.

  @precon  None.
  @postcon Makes sure that then timer is disbled and the form is freed from
           memory when the form is closed.

  @param   Sender as a TObject
  @param   Action as a TCloseAction as a reference

**)
Procedure TfrmAbout.FormClose(Sender: TObject; Var Action: TCloseAction);
Begin
  AboutTimer.Enabled := False;
  Action             := caFree;
  frm                := Nil;
End;

(**

  This is a method which obtains information about the application from is
  version information with the applications resources.

  @precon  None.
  @postcon Extracts and display the applications version number present within
           the EXE file.

  @param   iMajor  as an Integer as a reference
  @param   iMinor  as an Integer as a reference
  @param   iBugFix as an Integer as a reference
  @param   iBuild  as an Integer as a reference

**)
Procedure TfrmAbout.BuildNumber(Var iMajor, iMinor, iBugFix, iBuild: Integer);

Var
  VerInfoSize : DWORD;
  VerInfo     : Pointer;
  VerValueSize: DWORD;
  VerValue    : PVSFixedFileInfo;
  Dummy       : DWORD;

Begin
  { Build Number }
  VerInfoSize := GetFileVersionInfoSize(PChar(FModuleFileName), Dummy);
  If VerInfoSize <> 0 Then
    Begin
      GetMem(VerInfo, VerInfoSize);
      GetFileVersionInfo(PChar(FModuleFileName), 0, VerInfoSize, VerInfo);
      VerQueryValue(VerInfo, '\', Pointer(VerValue), VerValueSize);
      With VerValue^ Do
        Begin
          iMajor  := dwFileVersionMS Shr 16;
          iMinor  := dwFileVersionMS And $FFFF;
          iBugFix := dwFileVersionLS Shr 16;
          iBuild  := dwFileVersionLS And $FFFF;
        End;
      BuildLabel.Caption := Format('Build %d.%d.%d.%d',
        [iMajor, iMinor, iBugFix, iBuild]);
      FreeMem(VerInfo, VerInfoSize);
    End
  Else
    Begin
      MessageDlg('This executable does not contain any version information.', mtWarning,
        [mbOK], 0);
      BuildLabel.Caption := '';
    End;
End;

(**

  This is a method which gets and displays the OS version information on the
  dialogue.

  @precon  None.
  @postcon Displays the OS version number within the dialogue.

**)
Procedure TfrmAbout.VersionInformation;

Var
  MS          : TMemoryStatus;
  pVersionInfo: TOSVersionInfo;
  strTemp     : String;

Begin
  GlobalMemoryStatus(MS);
  PhysMemLabel.Caption := FormatFloat('Memory Available #,###" KB"',
    MS.dwTotalPhys / 1024);
  { Get Version Information }
  pVersionInfo.dwOSVersionInfoSize := SizeOf(pVersionInfo);
  GetVersionEx(pVersionInfo);
  With pVersionInfo Do
    Begin
      Case dwPlatformID Of
        VER_PLATFORM_WIN32s:
          strTemp := 'Win32s on Windows 3.1';
        VER_PLATFORM_WIN32_WINDOWS:
          strTemp := 'Windows 95';
        VER_PLATFORM_WIN32_NT:
          strTemp := 'Windows NT';
      Else
        strTemp := 'Unknown Windows Version';
      End;
      WinLabel.Caption := strTemp + ' ' + IntToStr(dwMajorVersion) + '.' +
        IntToStr(dwMinorVersion) + #13'Build: ' + IntToStr(dwBuildNumber) + ' - ' +
        szCSDVersion;
    End;
End;

(**

  This is an on form create event handler for the About form.

  @precon  None.
  @postcon Tries to get the users name and company from the registry. If not
           found it prompts for the information and then stores it.

  @param   Sender as a TObject

**)
Procedure TfrmAbout.FormCreate(Sender: TObject);

Const
  strBugfixes = ' abcdefghijklmnopqrstuvwxyz';

Var
  Reg                            : TRegistry;
  iMajor, iMinor, iBugFix, iBuild: Integer;
  Buffer                         : Array [0 .. MAX_PATH] Of Char;

Begin
  GetModuleFilename(hInstance, Buffer, MAX_PATH);
  FModuleFileName := StrPas(Buffer);
  Reg             := TRegistry.Create;
  Try
    Reg.RootKey := HKEY_LOCAL_MACHINE;
    Reg.OpenKeyReadOnly('Software\Microsoft\Windows NT\CurrentVersion');
    iMajor  := 0;
    iMinor  := 0;
    iBugFix := 0;
    iBuild  := 0;
    BuildNumber(iMajor, iMinor, iBugFix, iBuild);
    TitleLabel.Caption := Format('%s %d.%d%s', [Application.Title, iMajor, iMinor,
        strBugfixes[iBugFix + 1]]);
    ClientCompanyLabel.Caption := Reg.ReadString('RegisteredOrganization');
    ClientNameLabel.Caption    := Reg.ReadString('RegisteredOwner');
    VersionInformation;
  Finally
    Reg.Free;
  End;
  AboutTimer.Enabled := True;
End;

(**

  This is a form on show event handler for the About form.

  @precon  None.
  @postcon Displays the authors name and copyright + the current memory useage.

  @param   Sender as a TObject

**)
Procedure TfrmAbout.FormShow(Sender: TObject);

Var
  MS    : TMemoryStatus;
  dtDate: TDateTime;

Begin
  {$IFNDEF D2005}
  dtDate := FileDateToDateTime(FileAge(FModuleFileName));
  {$ELSE}
  FileAge(FModuleFileName, dtDate);
  {$ENDIF}
  lblBy.Caption := 'Written by David Hoyle - Copyright ' +
    FormatDateTime('dd mmmm yyyy', dtDate);
  AboutTimer.Enabled := True;
  GlobalMemoryStatus(MS);
  PhysMemLabel.Caption := FormatFloat('Memory Available #,###" KB"',
    MS.dwTotalPhys / 1024);
  Application.ProcessMessages;
End;

(**

  This is a class method for invoking and a single instance of this about
  dialogue.

  @precon  None.
  @postcon Displays a single instance of this about dialogue.

**)
Class Procedure TfrmAbout.ShowAbout;

Begin
  If frm = Nil Then
    frm := TfrmAbout.Create(Application);
  frm.Show;
  Application.ProcessMessages;
End;

End.
