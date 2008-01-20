(**

  This module represents a form for displaying the applications About
  information.

  @Author  David Hoyle
  @Version 1.0
  @Date    20 Jan 2008

**)
unit About;

interface

uses
  SysUtils, WinTypes, WinProcs, Messages, Classes, Graphics, Controls,
  Forms, Dialogs, ExtCtrls, StdCtrls, DGHSpectrum;

type
  (** This is a class to represent the About form which displays the
      applications information. **)
  TfrmAbout = class(TForm)
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
    procedure AboutTimerTimer(Sender: TObject);
    procedure FormKeyPress(Sender: TObject; var Key: Char);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    { Private declarations }
    Procedure BuildNumber(var iMajor, iMinor, iBugFix, iBuild : Integer);
    procedure VersionInformation;
  public
    { Public declarations }
    class procedure ShowAbout(strAppINIFile : String);
  end;

implementation

{$R *.DFM}

Uses
  IniFiles;

Var
  (** This is a private variable for an instance of the form used to implement
      this forms as a singleton class. **)
  frm : TfrmAbout;
  (** A private variable to hold the Apps INI File. **)
  strRootKey : String;

(**

  This is an on timer event handler for the timer control.

  @precon  None.
  @postcon Disabled the timer and Closes the form and destroys its instance.

  @param   Sender as a TObject

**)
procedure TfrmAbout.AboutTimerTimer(Sender: TObject);

begin
  AboutTimer.Enabled := False;
  Close;
end;

(**

  This is an on click event handler for all controls on the form.

  @precon  None.
  @postcon Closes the form and destroy the instance when an item is clicked.

  @param   Sender as a TObject
  @param   Key    as a Char as a reference

**)
procedure TfrmAbout.FormKeyPress(Sender: TObject; var Key: Char);

begin
  Close;
end;

(**

  This is a form on close event handler.

  @precon  None.
  @postcon Makes sure that then timer is disbled and the form is freed from
           memory when the form is closed.

  @param   Sender as a TObject
  @param   Action as a TCloseAction as a reference

**)
procedure TfrmAbout.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  AboutTimer.Enabled := False;
  Action := caFree;
  frm := Nil;
end;

(**

  This is a method which obtains information about the application from is
  version information with the applications resources.

  @precon  None.
  @postcon Extracts and display the applications version number present within
           the EXE file.

  @param   iMajor  as an Integer
  @param   iMinor  as an Integer
  @param   iBugFix as an Integer
  @param   iBuild  as an Integer

**)
Procedure TfrmAbout.BuildNumber(var iMajor, iMinor, iBugFix, iBuild : Integer);

Var
  VerInfoSize: DWORD;
  VerInfo: Pointer;
  VerValueSize: DWORD;
  VerValue: PVSFixedFileInfo;
  Dummy: DWORD;

Begin
  { Build Number }
  VerInfoSize := GetFileVersionInfoSize(PChar(ParamStr(0)), Dummy);
  If VerInfoSize <> 0 Then
    Begin
      GetMem(VerInfo, VerInfoSize);
      GetFileVersionInfo(PChar(ParamStr(0)), 0, VerInfoSize, VerInfo);
      VerQueryValue(VerInfo, '\', Pointer(VerValue), VerValueSize);
      With VerValue^ Do
        Begin
          iMajor := dwFileVersionMS shr 16;
          iMinor := dwFileVersionMS and $FFFF;
          iBugFix := dwFileVersionLS shr 16;
          iBuild := dwFileVersionLS and $FFFF;
        End;
      BuildLabel.Caption := Format('Build %d.%d.%d.%d', [iMajor, iMinor,
        iBugfix, iBuild]);
      FreeMem(VerInfo, VerInfoSize);
    End Else
    Begin
      MessageDlg('This executable does not contain any version information.', mtWarning, [mbOK], 0);
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
  MS : TMemoryStatus;
  pVersionInfo : TOSVersionInfo;
  strTemp : String;

Begin
  GlobalMemoryStatus(MS);
  PhysMemLabel.Caption := FormatFloat('Memory Available #,###" KB"', MS.dwTotalPhys /1024);
  { Get Version Information }
  pVersionInfo.dwOSVersionInfoSize := SizeOf(pVersionInfo);
  GetVersionEx(pVersionInfo);
  With pVersionInfo Do
    Begin
      Case dwPlatformID Of
        VER_PLATFORM_WIN32s : strTemp :=  'Win32s on Windows 3.1';
        VER_PLATFORM_WIN32_WINDOWS  : strTemp :=  'Windows 95';
        VER_PLATFORM_WIN32_NT	 : strTemp :=  'Windows NT';
      Else
        strTemp :=  'Unknown Windows Version';
      End;
      WinLabel.Caption := strTemp + ' ' + IntToStr(dwMajorVersion) + '.' +
        IntToStr(dwMinorVersion) + #13'Build: ' + IntToStr(dwBuildNumber) +
        ' - ' + szCSDVersion;
    End;
End;

(**

  This is an on form create event handler for the About form.

  @precon  None.
  @postcon Tries to get the users name and company from the registry. If not
           found it prompts for the information and then stores it.

  @param   Sender as a TObject

**)
procedure TfrmAbout.FormCreate(Sender: TObject);

Const
  strBugfixes = ' abcedfghijklmnopqrstuvwxyz';

Var
  strName : String;
  strCompany : String;
  Reg : TIniFile;
  iMajor, iMinor, iBugfix, iBuild : Integer;

begin
  Reg := TIniFile.Create(strRootKey);
  Try
    strName := Reg.Readstring('License', 'Name', '');
    strCompany := Reg.Readstring('License', 'Company', '');
    iMajor := 0;
    iMinor := 0;
    iBugfix := 0;
    iBuild := 0;
    BuildNumber(iMajor, iMinor, iBugfix, iBuild);
    TitleLabel.Caption := Format('%s %d.%d%s', [Application.Title, iMajor,
      iMinor, strBugfixes[iBugfix + 1]]);
    ClientCompanyLabel.Caption := strCompany;
    ClientNameLabel.Caption := strName;
    VersionInformation;
    If strName + strCompany = '' Then
      Begin
        If Not InputQuery(Application.Title + ' License Registration',
          'Please enter your Name:', strName) Then
          Exit;
        If Not InputQuery(Application.Title + ' License Registration',
          'Please enter your Company:', strCompany) Then
          Exit;
        Reg.WriteString('License', 'Name', strName);
        Reg.WriteString('License', 'Company', strCompany);
        ClientCompanyLabel.Caption := strCompany;
        ClientNameLabel.Caption := strName;
      End;
  Finally
    Reg.Free;
  End;
  AboutTimer.Enabled := True;
end;

(**

  This is a form on show event handler for the About form.

  @precon  None.
  @postcon Displays the authors name and copyright + the current memory useage.

  @param   Sender as a TObject

**)
procedure TfrmAbout.FormShow(Sender: TObject);

Var
  MS : TMemoryStatus;
  dtDate: TDateTime;

begin
  FileAge(ParamStr(0), dtDate);
  lblBy.Caption := 'Written by David Hoyle - Copyright ' +
    FormatDateTime('mmmm yyyy', dtDate);
  AboutTimer.Enabled := True;
  GlobalMemoryStatus(MS);
  PhysMemLabel.Caption := FormatFloat('Memory Available #,###" KB"', MS.dwTotalPhys /1024);
  Application.ProcessMessages;
end;

(**

  This is a class method for invoking and a single instance of this about
  dialogue.

  @precon  None.
  @postcon Displays a single instance of this about dialogue.

  @param   strAppINIFile as a String

**)
Class Procedure TfrmAbout.ShowAbout(strAppINIFile : String);

Begin
  strRootKey := strAppINIFile;
  If frm = Nil Then
    frm := TfrmAbout.Create(Application);
  frm.Show;
  Application.ProcessMessages;
End;

end.


