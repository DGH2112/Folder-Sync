unit About;

interface

uses
  SysUtils, WinTypes, WinProcs, Messages, Classes, Graphics, Controls,
  Forms, Dialogs, ExtCtrls, StdCtrls, IniFiles, DGHSpectrum;

type
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
    Procedure BuildNumber;
    procedure VersionInformation;
  public
    { Public declarations }
  end;

implementation

{$R *.DFM}

Uses
  Functs;

{ -------------------------------------------------------------------------

   TTimer event. This disables the timer and closes the about form.

  -------------------------------------------------------------------------- }

procedure TfrmAbout.AboutTimerTimer(Sender: TObject);

begin
  AboutTimer.Enabled := False;
  Close;
end;

{ -------------------------------------------------------------------------

   Form KeyPress event. If a key is pressed close the about form.

  -------------------------------------------------------------------------- }

procedure TfrmAbout.FormKeyPress(Sender: TObject; var Key: Char);

begin
  Close;
end;

{ -------------------------------------------------------------------------

   Form Close event. Make sure the the timer is disabled on closing.

  -------------------------------------------------------------------------- }

procedure TfrmAbout.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  AboutTimer.Enabled := False;
end;

{ -------------------------------------------------------------------------

   This routine encrypts a string of text.

   TransposeUp(
     Str    // Text to encrypt as a string
   );       // Returns encrypted text as a string

  -------------------------------------------------------------------------- }

Function TransposeUp(Str : String) : String;

Var
  sTempStr : String;
  iIndex : Integer;

Begin
  sTempStr := Str;
  For iIndex := 1 To Length(Str) Do
    sTempStr[iIndex] := Char(Byte(sTempStr[iIndex]) + (Length(Str) - iIndex + 1));
  Result := sTempStr;
End;

{ -------------------------------------------------------------------------

   This routine decrypts a string of text.

   TransposeUp(
     Str    // Text to decrypt as a string
   );       // Returns decrypted text as a string

  -------------------------------------------------------------------------- }

Function TransposeDown(Str : String) : String;

Var
  sTempStr : String;
  iIndex : Integer;

Begin
  sTempStr := Str;
  For iIndex := 1 To Length(Str) Do
    sTempStr[iIndex] := Char(Byte(sTempStr[iIndex]) - (Length(Str) - iIndex + 1));
  Result := sTempStr;
End;

{ -------------------------------------------------------------------------

   This routine gets the build number of the application and displays it.

   BuildNumber(
   );

  -------------------------------------------------------------------------- }

Procedure TfrmAbout.BuildNumber;

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
      with VerValue^ do
      begin
        BuildLabel.Caption := 'Build ' + IntToStr(dwFileVersionMS shr 16) +
          '.' + IntToStr(dwFileVersionMS and $FFFF) +
          '.' + IntToStr(dwFileVersionLS shr 16) +
          '.' + IntToStr(dwFileVersionLS and $FFFF);
      end;
      FreeMem(VerInfo, VerInfoSize);
    End Else
    Begin
      MessageDlg('This executable does not contain any version information.', mtWarning, [mbOK], 0);
      BuildLabel.Caption := '';
    End;

End;

{ -------------------------------------------------------------------------

   This routine gets the current windows version number and displays it.

   VersionInformation(
   );

  -------------------------------------------------------------------------- }

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

{ -------------------------------------------------------------------------

   Form Create event. Tries to get the users name and company from the INI
   file. If not found it prompts for the information and then stores it.

  -------------------------------------------------------------------------- }

procedure TfrmAbout.FormCreate(Sender: TObject);

Type
  TCharArray = Array[0..MAXCHAR] Of Char;
  PVSFixedFileInfo = ^TVSFIXEDFILEINFO;

Var
  strName : String;
  strCompany : String;
  strIniFileName : String;
  IniFile : TIniFile;

begin
  strIniFileName := ChangeFileExt(ExpandFileName(Application.EXEName), '.INI');
  IniFile := TIniFile.Create(strIniFileName);
  Try
    strName := TransposeDown(IniFile.Readstring('License', 'Name', ''));
    strCompany := TransposeDown(IniFile.Readstring('License', 'Company', ''));
    TitleLabel.Caption := Application.Title;
    ClientCompanyLabel.Caption := strCompany;
    ClientNameLabel.Caption := strName;
    AboutTimer.Enabled := True;
    BuildNumber;
    VersionInformation;
    If strName + strCompany = '' Then
      Begin
        If Not InputQuery(Application.Title + ' License Registration',
          'Please enter your Name:', strName) Then
          Exit;
        If Not InputQuery(Application.Title + ' License Registration',
          'Please enter your Company:', strCompany) Then
          Exit;
        IniFile.WriteString('License', 'Name', TransposeUp(strName));
        IniFile.WriteString('License', 'Company', TransposeUp(strCompany));
        ClientCompanyLabel.Caption := strCompany;
        ClientNameLabel.Caption := strName;
      End;
  Finally
    IniFile.Free;
  End;
end;

{ -------------------------------------------------------------------------

   Form OnShow event. Starts the timer, and displays the available memory.

  -------------------------------------------------------------------------- }

procedure TfrmAbout.FormShow(Sender: TObject);

Var
  MS : TMemoryStatus;

begin
  lblBy.Caption := 'Written by David Hoyle - Copyright ' +
    FormatDateTime('mmmm yyyy', FileDateToDateTime(FileAge(ParamStr(0))));
  AboutTimer.Enabled := True;
  GlobalMemoryStatus(MS);
  PhysMemLabel.Caption := FormatFloat('Memory Available #,###" KB"', MS.dwTotalPhys /1024);
end;

end.
