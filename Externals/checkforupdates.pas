(**

  This module contains a class which handles the checking of updates from the
  Internet for console applications.

  @Version 1.0
  @Author  David Hoyle
  @Date    01 Aug 2012

**)
Unit CheckForUpdates;

Interface

Uses
  SysUtils,
  XMLDoc,
  XMLIntf,
  Graphics,
  Windows;

{$INCLUDE 'CompilerDefinitions.inc'}

Type
    (** An enumerate type to describe the internet connection status. **)
  TINetConnection = (icUnknown, icMissingDLL, icMissingFunction, icNotConnected,
    icConnected);

    (** This class handles the checking of software vesions against the
        internet. **)
  TCheckForUpdates = Class
    {$IFDEF D2005} Strict {$ENDIF} Private
    {$IFDEF CONSOLE}
    FConHnd: THandle;
    {$ENDIF}
    FSoftwareID              : String;
    FLastUpdateDate          : TDateTime;
    FRegRoot                 : String;
    FMessageTextColour       : TColor;
    FMessageConfirmColour    : TColor;
    FMessageWarningColour    : TColor;
    FMessageNoteColour       : TColor;
    FMessageDescriptionColour: TColor;
    FMessageHeaderColour     : TColor;
    FUpdateInterval          : Integer;
    FEnabled                 : Boolean;
    FDLLHnd                  : THandle;
    {$IFDEF D2005} Strict {$ENDIF} Protected
    Function CheckForUpdates(strURL, strXMLFile: String): Boolean;
    Function FindPackage(xmlDocumentElement: IXMLNode): IXMLNode;
    Function GetNamedNodeText(P: IXMLNode; strName: String): String;
    Function CheckVersionNumber(iMajor, iMinor, iBugFix, iBuild: Integer;
      Var strAppVerNum: String): Integer;
    Procedure OutputMsg(strText, strURL: String; iColour: TColor = clNone);
    Procedure ReadLastUpdateDate;
    Procedure WriteLastUpdateDate;
    Function BuildVersionNumber(iMajor, iMinor, iBugFix, iBuild: Integer): String;
    Procedure LoadSettings;
    Procedure SaveSettings;
    Function CheckForConnection: Boolean;
    Function InternetAvailable(Var strConnection: String): TINetConnection;
    Function DownloadFile(strURL, strOutputFile: String): Boolean;
  Public
    Constructor Create(strSoftwareID, strRegRoot: String; boolForceUpdate: Boolean);
    Destructor Destroy; Override;
    Class Procedure Execute(strSoftwareID, strRegRoot: String; boolForceUpdate: Boolean);
  End;

Implementation

Uses
  DGHLibrary,
  IniFiles {$IFNDEF CONSOLE},
  CheckForUpdatesForm {$ENDIF};

ResourceString
    (** A resource string of the start of the checking process. **)
  strCheckingForUpdates = 'Checking Updates for ''%s''...';
    (** A resource string to note the loading of MS XML. **)
  strLoadingMSXML = '  Loading XML Parser...';
    (** A resource string to note the lading of the URL **)
  strLoadingURL = '  Loading "%s"...';
    (** A resource string to note the searching for packages. **)
  strGettingPackages = '  Getting packages...';
    (** A resource string to note the package was found. **)
  strFoundPackage = '  Found "%s"...';
    (** A resource string to note the checking of the software version. **)
  strCheckingSoftwareVerNum = '  Checking software version number...';
    (** A resource string to note that the software is up to date. **)
  strYourSoftwareIsUpToDate = '  Your software is up to date!';
    (** A resource string to note that there is an update available. **)
  strThereIsASoftwareUpdate = '  There is a software update available ' +
    '[New %s, Old %s] @ %s! See the description below for update details...';
    (** A resource string to define that the package was not found. **)
  strPackageNotFound = '  Package "%s" not found!';
    (** A resource string for an exception message. **)
  strExceptionS = '  Exception: %s';
  {$IFDEF CONSOLE}
    (** A resource string to prompt to continue. **)
  strPressEnterToContinue = 'Press <Enter> to continue...';
  {$ENDIF}
    (** A resource string to prompt that you are using a newer version of software. **)
  strYouAreUsingANewerVersion = '  You are using a newer version of the ' +
    'software [%s] than is available from the internet [%s] @ %s.';
    (** A resource strin to define the INI file section for update information. **)
  strCheckForUpdatesSection = 'CheckForUpdates';
    (** An error message for failing to start MS XML. **)
  strFailedMSXML = '  Failed to start or find an XML Parser on your system (%s).';

(**

  This is the main interface method for checking the software version.

  @precon  None.
  @postcon Main interface method for checking the software version.

  @param   strSoftwareID   as a String
  @param   strRegRoot      as a String
  @param   boolForceUpdate as a Boolean

**)
Class Procedure TCheckForUpdates.Execute(strSoftwareID, strRegRoot: String;
  boolForceUpdate: Boolean);

Begin
  TCheckForUpdates.Create(strSoftwareID, strRegRoot, boolForceUpdate).Free;
End;

(**

  This method builds a string representation of the build information.

  @precon  None.
  @postcon Builds a string representation of the build information.

  @param   iMajor  as an Integer
  @param   iMinor  as an Integer
  @param   iBugFix as an Integer
  @param   iBuild  as an Integer
  @return  a String

**)
Function TCheckForUpdates.BuildVersionNumber(iMajor, iMinor, iBugFix,
  iBuild: Integer): String;

Const
  strBuild = '%d.%d%s (Build %d.%d.%d.%d)';

Begin
  Result := Format(strBuild, [iMajor, iMinor, strBugFix[Succ(iBugFix)], iMajor, iMinor,
    iBugFix, iBuild]);
End;

(**

  This method determines if there is an internet connection and returns true
  if this is the case else returns false.

  @precon  None.
  @postcon Determines if there is an internet connection and returns true
           if this is the case else returns false.

  @return  a Boolean

**)
Function TCheckForUpdates.CheckForConnection: Boolean;

Var
  strConnection: String;

Begin
  Result := False;
  Case InternetAvailable(strConnection) Of
    icMissingDLL:
      OutputMsg('  Internet Connection: Missing DLL.', '', FMessageWarningColour);
    icMissingFunction:
      OutputMsg('  Internet Connection: Missing Function.', '', FMessageWarningColour);
    icNotConnected:
      OutputMsg('  Internet Connection: Not Connected.', '', FMessageWarningColour);
    icConnected:
      Begin
        OutputMsg(Format('  Internet Connection: Connected (%s).', [strConnection]), '',
          FMessageTextColour);
        Result := True;
      End;
  Else
    OutputMsg('  Internet Connection: Unknown.', '', FMessageWarningColour);
  End;
End;

(**

  This procedure checks the build number of the software against the XML file at the given
  URL to see if there are any updates available.

  @precon  strXMLFile must be a valid XMLfile.
  @postcon Displays messages to the console regarding the versioning of the software.

  @param   strURL     as a String
  @param   strXMLFile as a String
  @return  a Boolean

**)
Function TCheckForUpdates.CheckForUpdates(strURL, strXMLFile: String): Boolean;

Var
  xmlDoc                         : IXMLDocument;
  P                              : IXMLNode;
  iMajor, iMinor, iBugFix, iBuild: Integer;
  iResult                        : Integer;
  strInternet, strApplication    : String;

Begin
  Result := False;
  OutputMsg(strLoadingMSXML, strURL, FMessageTextColour);
  Try
    xmlDoc := TXMLDocument.Create(Nil);
    Try
      Try
        xmlDoc.ParseOptions := [poValidateOnParse, poAsyncLoad];
        OutputMsg(Format(strLoadingURL, [strURL + 'HoylD.xml']), strURL,
          FMessageTextColour);
        xmlDoc.LoadFromFile(strXMLFile);
        If xmlDoc.Active Then
          Begin
            OutputMsg(strGettingPackages, strURL, FMessageTextColour);
            P           := FindPackage(xmlDoc.DocumentElement);
            If P <> Nil Then
              Begin
                OutputMsg(Format(strFoundPackage, [FSoftwareID]), strURL,
                  FMessageTextColour);
                OutputMsg(strCheckingSoftwareVerNum, strURL, FMessageTextColour);
                iMajor      := StrToInt(GetNamedNodeText(P, 'Major'));
                iMinor      := StrToInt(GetNamedNodeText(P, 'Minor'));
                iBugFix     := StrToInt(GetNamedNodeText(P, 'BugFix'));
                iBuild      := StrToInt(GetNamedNodeText(P, 'Build'));
                strInternet := BuildVersionNumber(iMajor, iMinor, iBugFix, iBuild);
                iResult     := CheckVersionNumber(iMajor, iMinor, iBugFix, iBuild,
                  strApplication);
                If iResult = 0 Then
                  Begin
                    OutputMsg(strYourSoftwareIsUpToDate, strURL, FMessageConfirmColour);
                    {$IFNDEF CONSOLE}
                    TfrmCheckForUpdates.Finish(5);
                    {$ENDIF}
                  End
                Else If iResult > 0 Then
                  Begin
                    OutputMsg(Format(strYouAreUsingANewerVersion,
                      [strApplication, strInternet, strURL]), strURL,
                      FMessageWarningColour);
                    {$IFDEF CONSOLE}
                    OutputMsg('', strURL);
                    If CheckConsoleMode(FConHnd) Then
                      Begin
                        OutputMsg(strPressEnterToContinue, strURL, FMessageTextColour);
                        SysUtils.Beep;
                        ReadLn;
                      End;
                    {$ELSE}
                    SysUtils.Beep;
                    TfrmCheckForUpdates.Finish(60);
                    {$ENDIF}
                  End
                Else
                  Begin
                    OutputMsg(Format(strThereIsASoftwareUpdate,
                      [strInternet, strApplication, strURL]), strURL, FMessageNoteColour);
                    OutputMsg('', strURL);
                    OutputMsg('  ' + StringReplace(GetNamedNodeText(P, 'Description'),
                      #13#10, #32#32#13#10, [rfReplaceAll]), strURL,
                      FMessageDescriptionColour);
                    {$IFDEF CONSOLE}
                    OutputMsg('', strURL);
                    OutputMsg(strPressEnterToContinue, strURL, FMessageTextColour);
                    SysUtils.Beep;
                    ReadLn;
                    {$ELSE}
                    SysUtils.Beep;
                    TfrmCheckForUpdates.Finish(60);
                    {$ENDIF}
                  End;
                Result := True;
              End
            Else
              Begin
                OutputMsg(Format(strPackageNotFound, [FSoftwareID]), strURL,
                  FMessageNoteColour);
                SysUtils.Beep;
                {$IFNDEF CONSOLE}
                TfrmCheckForUpdates.Finish(60);
                {$ENDIF}
              End;
          End
        Else
          Begin
            OutputMsg(Format('  Failed to Parse the XML file ("%s")', [strURL]), strURL,
              FMessageWarningColour);
            {$IFNDEF CONSOLE}
            SysUtils.Beep;
            TfrmCheckForUpdates.Finish(60);
            {$ENDIF}
          End;
      Except
        On E: Exception Do
          OutputMsg(Format(strExceptionS, [E.Message]), strURL, FMessageWarningColour);
      End;
      OutputMsg('', strURL);
    Finally
      xmlDoc := Nil;
    End;
  Except
    On E: Exception Do
      OutputMsg(Format(strFailedMSXML, [E.Message]), '', FMessageWarningColour);
  End;
End;

(**

  This is the constructor method for the TConsoleCheckForupdates class.

  @precon  None.
  @postcon Initialises the class.

  @param   strSoftwareID   as a String
  @param   strRegRoot      as a String
  @param   boolForceUpdate as a Boolean

**)
Constructor TCheckForUpdates.Create(strSoftwareID, strRegRoot: String;
  boolForceUpdate: Boolean);

ResourceString
  strURL = 'http://www.davidghoyle.co.uk/';

Var  
  strTempFile : String;
  strTempPath : String;
  iSize : Integer;

Begin
  FMessageWarningColour := clRed;
  LoadSettings;
  FSoftwareID := strSoftwareID;
  FRegRoot    := strRegRoot;
  {$IFDEF CONSOLE}
  FConHnd := GetStdHandle(STD_OUTPUT_HANDLE);
  {$ENDIF}
  ReadLastUpdateDate;
  FDLLHnd := LoadLibrary('WININET.DLL');
  If boolForceUpdate Or (FEnabled And (FLastUpdateDate + FUpdateInterval < Now)) Then
    Begin
      OutputMsg(Format(strCheckingForUpdates, [FSoftwareID]), '', FMessageHeaderColour);
      If CheckForConnection Then
        Begin
          SetLength(strTempPath, 1024);
          iSize := GetTempPath(1024, PChar(strTempPath));
          SetLength(StrTempPath, iSize);
          SetLength(strTempFile, 1024);
          GetTempFileName(PChar(strTempPath), PChar(strSoftwareID), Random(123456789),
            PChar(strTempFile));
          iSize := StrLen(PChar(strTempFile));
          SetLength(strTempFile, iSize);
          If DownloadFile(strURL + '/HoylD.xml', strTempFile) Then
            Try
              CheckForUpdates(strURL, strTempFile);
            Finally
              DeleteFile(PChar(strTempFile));
            End;
        End
        {$IFNDEF CONSOLE}
      Else
        Begin
          SysUtils.Beep;
          TfrmCheckForUpdates.Finish(60)
        End
        {$ENDIF};
      WriteLastUpdateDate;
    End;
End;

(**

  This is the destructor method for the TCheckForUpdates class.

  @precon  None.
  @postcon Saves the INI settings for Colours.

**)
Destructor TCheckForUpdates.Destroy;

Begin
  SaveSettings;
  FreeLibrary(FDLLHnd);
  Inherited Destroy;
End;

(**

  This method use raw WinINet to download an XML file from the web and put the contents
  in a temporary file so that the XML parser can open the file and search for the
  relative information.

  @precon  strURL and strOutputFile must be valid URL and filename respectively.
  @postcon The XML file is download to the specified file.

  @param   strURL        as a String
  @param   strOutputFile as a String
  @return  a Boolean

**)
Function TCheckForUpdates.DownloadFile(strURL, strOutputFile: String): Boolean;

Type
  HINTERNET = Pointer;

Const
  INTERNET_OPEN_TYPE_PREFONFIG = 0;
  BufferSize = 1024;

Var
  InternetOpen: Function(lpszAgent: PWideChar; dwAccessType: DWORD;
    lpszProxy, lpszProxyBypass: PWideChar; dwFlags: DWORD): HINTERNET; stdcall;
  InternetOpenURL: Function(hInet: HINTERNET; lpszUrl: PWideChar;
    lpszHeaders: PWideChar; dwHeadersLength: DWORD; dwFlags: DWORD;
    dwContext: DWORD_PTR): HINTERNET; stdcall;
  InternetCloseHandle: Function(hInet: HINTERNET): BOOL; stdcall;
  InternetReadFile: Function(hFile: HINTERNET; lpBuffer: Pointer;
    dwNumberOfBytesToRead: DWORD; var lpdwNumberOfBytesRead: DWORD): BOOL; stdcall;
  hndSession : HINTERNET;
  hndURL : HINTERNET;
  F : File;
  Buffer: Array[1..BufferSize] Of Byte;
  BufferLen : DWORD;

Begin
  Result := False;
  If FDLLHnd <> 0 Then
    Begin
      @InternetOpen := GetProcAddress(FDLLHnd, 'InternetOpenW');
      If @InternetOpen <> Nil Then
        Begin
          @InternetCloseHandle := GetProcAddress(FDLLHnd, 'InternetCloseHandle');
          If @InternetCloseHandle <> Nil Then
            Begin
              @InternetOpenURL := GetProcAddress(FDLLHnd, 'InternetOpenUrlW');
              If @InternetOpenURL <> Nil Then
                Begin
                  @InternetReadFile := GetProcAddress(FDLLHnd, 'InternetReadFile');
                  If @InternetReadFile <> Nil Then
                    Begin
                      hndSession := InternetOpen('Check for Updates',
                        INTERNET_OPEN_TYPE_PREFONFIG, Nil, Nil, 0);
                      Try
                        hndURL := InternetOpenURL(hndSession, PChar(strURL), Nil, 0, 0,
                          0);
                        Try
                          OutputMsg('  Downloading file ' + strURL + '...', '');
                          AssignFile(F, strOutputFile);
                          ReWrite(F, 1);
                          Repeat
                            InternetReadFile(hndURL, @Buffer, SizeOf(Buffer), BufferLen);
                            BlockWrite(F, Buffer, BufferLen);
                          Until BufferLen = 0;
                          CloseFile(F);
                          Result := True;
                          OutputMsg('  File downloaded...', '');
                        Finally
                          InternetCloseHandle(hndURL);
                        End;
                      Finally
                        InternetCloseHandle(hndSession);
                      End;
                    End Else
                      OutputMsg('  Missing function InternetReadFile!', '',
                        FMessageWarningColour);
                End Else
                  OutputMsg('  Missing function InternetOpenURLW!', '',
                    FMessageWarningColour);
            End Else
              OutputMsg('  Missing function InternetCloseHandle!', '',
                FMessageWarningColour);
        End Else
          OutputMsg('  Missing function InternetOpenW!', '', FMessageWarningColour);
    End;
End;

(**

  This function searches through the packages looking for the packages with the correct ID
  .

  @precon  xmlDocumentElement must be a valid list of nodes to search.
  @postcon Searches through the folders and packages looking for the packages with the
           correct ID.

  @param   xmlDocumentElement as an IXMLNode
  @return  an IXMLNode

**)
Function TCheckForUpdates.FindPackage(xmlDocumentElement: IXMLNode): IXMLNode;

Var
  i, j   : Integer;
  Fldr, S: IXMLNode;

Begin
  Result  := Nil;
  For i   := 0 To xmlDocumentElement.childNodes.Count - 1 Do
    Begin
      Fldr := xmlDocumentElement.ChildNodes[i];
      For j := 0 To Fldr.ChildNodes.Count - 1 Do
        Begin
          S := Fldr.ChildNodes[j];
          If S.Attributes['ID'] = FSoftwareID Then
            Begin
              Result := S;
              Exit;
            End;
        End;
    End;
End;

(**

  This function returns the text associated with the named node.

  @precon  P must be a valid IXMLDOMNode.
  @postcon Returns the text associated with the named node.

  @param   P       as an IXMLNode
  @param   strName as a String
  @return  a String

**)
Function TCheckForUpdates.GetNamedNodeText(P: IXMLNode; strName: String): String;

Var
  i: Integer;

Begin
  Result := '';
  For i  := 0 To P.childNodes.Count - 1 Do
    If CompareText(P.childNodes[i].nodeName, strName) = 0 Then
      Begin
        Result := P.childNodes[i].text;
        Break;
      End;
End;

(**

  This method determines the status of the internet connection and returns
  an enumerate informing the caller of the internet status.

  @precon  None.
  @postcon Determines the status of the internet connection and returns
           an enumerate informing the caller of the internet status.

  @param   strConnection as a String as a Reference
  @return  a TINetConnection

**)
Function TCheckForUpdates.InternetAvailable(Var strConnection: String): TINetConnection;

Type
  TConnectionInfo = Record
    FState: Integer;
    FDescription: String;
  End;

Const
  Connections: Array [1 .. 6] Of TConnectionInfo = ((FState: $01; FDescription: 'Modem'),
    (FState: $02; FDescription: 'Lan'), (FState: $04; FDescription: 'Proxy'),
    (FState: $08; FDescription: 'Modem Busy'), (FState: $20; FDescription: 'Off Line'),
    (FState: $49; FDescription: 'Connection Configured'));

Var
  InternetGetConnectedStateEx: Function(lpdwFlags: LPDWORD; lpszConnectionname: PChar;
    dwSize: DWORD; dwReserved: DWORD): BOOL; StdCall;
  lpdwFlags : DWORD;
  iSize     : DWORD;
  strOptions: String;
  i         : Integer;

Begin
  If FDLLHnd <> 0 Then
    Begin
      @InternetGetConnectedStateEx := GetProcAddress(FDLLHnd,
        'InternetGetConnectedStateExW');
      If @InternetGetConnectedStateEx <> Nil Then
        Begin
          lpdwFlags := 0;
          iSize     := 1024;
          SetLength(strConnection, iSize);
          If InternetGetConnectedStateEx(@lpdwFlags, PChar(strConnection), iSize, 0) Then
            Result := icConnected
          Else
            Result      := icNotConnected;
          strConnection := StrPas(PChar(strConnection)) + '(%s)';
          strOptions    := '';
          For i         := Low(Connections) To High(Connections) Do
            If lpdwFlags And Connections[i].FState <> 0 Then
              Begin
                If strOptions <> '' Then
                  strOptions := strOptions + ', ';
                strOptions   := strOptions + Connections[i].FDescription;
              End;
          strConnection := Format(strConnection, [strOptions]);
        End
      Else
        Result := icMissingFunction;
    End
  Else
    Result := icMissingDLL;
End;

(**

  This method loads the Colours settings from the INI file.

  @precon  None.
  @postcon Loads the Colours settings from the INI file.

**)
Procedure TCheckForUpdates.LoadSettings;

Begin
  With TMeminiFile.Create(FRegRoot) Do
    Try
      FMessageTextColour := StringToColor(ReadString('Colours', 'MessageText', 'clNone'));
      FMessageConfirmColour := StringToColor(ReadString('Colours', 'MessageConfirm',
        'clWhite'));
      FMessageWarningColour := StringToColor(ReadString('Colours', 'MessageWarning',
        'clRed'));
      FMessageNoteColour := StringToColor(ReadString('Colours', 'MessageNote',
        'clYellow'));
      FMessageDescriptionColour :=
        StringToColor(ReadString('Colours', 'MessageDescription', 'clLime'));
      FMessageHeaderColour := StringToColor(ReadString('Colours', 'MessageHeader',
        'clWhite'));
    Finally
      Free;
    End;
End;

(**

  This method returns -ve if the software of older than the internet version,
  0 if the same or +ve if it is newer than the internet version.

  @precon  None.
  @postcon Returns -ve if the software of older than the internet version,
           0 if the same or +ve if it is..

  @param   iMajor       as an Integer
  @param   iMinor       as an Integer
  @param   iBugFix      as an Integer
  @param   iBuild       as an Integer
  @param   strAppVerNum as a String as a Reference
  @return  a Integer

**)
Function TCheckForUpdates.CheckVersionNumber(iMajor, iMinor, iBugFix, iBuild: Integer;
  Var strAppVerNum: String): Integer;

Var
  iMaj, iMin, iBug, iBui: Integer;
  Buffer                : Array [0 .. MAX_PATH] Of Char;
  strModuleFileName     : String;

Begin
  GetModuleFilename(hInstance, Buffer, MAX_PATH);
  strModuleFileName := StrPas(Buffer);
  GetBuildNumber(strModuleFileName, iMaj, iMin, iBug, iBui);
  Result := iMaj - iMajor;
  If Result = 0 Then
    Result := iMin - iMinor;
  If Result = 0 Then
    Result := iBug - iBugFix;
  If Result = 0 Then
    Result     := iBui - iBuild;
  strAppVerNum := BuildVersionNumber(iMaj, iMin, iBug, iBui);
End;

(**

  This procedure outputs a message to the console IF boolOutputMsgs is true.

  @precon  None.
  @postcon Outputs a message to the console IF boolOutputMsgs is true.

  @param   strText as a String
  @param   strURL  as a String
  @param   iColour as a TColor

**)
Procedure TCheckForUpdates.OutputMsg(strText, strURL: String; iColour: TColor = clNone);

Begin
  {$IFDEF CONSOLE}
  OutputToConsoleLn(FConHnd, StringReplace(strText, #13#10, #32, [rfReplaceAll]),
    iColour);
  {$ELSE}
  If strText <> '' Then
    TfrmCheckForUpdates.ShowUpdates(StringReplace(strText, #13#10, #32, [rfReplaceAll]),
      strURL, iColour);
  {$ENDIF}
End;

(**

  This method reads the last update date from the INI File.

  @precon  None.
  @postcon Reads the last update date from the INI File.

**)
Procedure TCheckForUpdates.ReadLastUpdateDate;

Var
  iDay, iMonth, iYear: Word;

Begin
  With TMeminiFile.Create(FRegRoot) Do
    Try
      iDay            := ReadInteger(strCheckForUpdatesSection, 'Day', 01);
      iMonth          := ReadInteger(strCheckForUpdatesSection, 'Month', 01);
      iYear           := ReadInteger(strCheckForUpdatesSection, 'Year', 1900);
      FLastUpdateDate := EncodeDate(iYear, iMonth, iDay);
      FUpdateInterval := ReadInteger(strCheckForUpdatesSection, 'Interval', 28);
      FEnabled        := ReadBool(strCheckForUpdatesSection, 'Enabled', False);
    Finally
      Free;
    End;
End;

(**

  This method saves the Colour settings to the INI file.

  @precon  None.
  @postcon Saves the Colour settings to the INI file.

**)
Procedure TCheckForUpdates.SaveSettings;
Begin
  With TMeminiFile.Create(FRegRoot) Do
    Try
      WriteString('Colours', 'MessageText', ColorToString(FMessageTextColour));
      WriteString('Colours', 'MessageConfirm', ColorToString(FMessageConfirmColour));
      WriteString('Colours', 'MessageWarning', ColorToString(FMessageWarningColour));
      WriteString('Colours', 'MessageNote', ColorToString(FMessageNoteColour));
      WriteString('Colours', 'MessageDescription',
        ColorToString(FMessageDescriptionColour));
      WriteString('Colours', 'MessageHeader', ColorToString(FMessageHeaderColour));
      UpdateFile;
    Finally
      Free;
    End;
End;

(**

  This method writes the last update date to the INI File.

  @precon  None.
  @postcon Writes the last update date to the INI File.

**)
Procedure TCheckForUpdates.WriteLastUpdateDate;

Var
  iDay, iMonth, iYear: Word;

Begin
  With TMeminiFile.Create(FRegRoot) Do
    Try
      WriteInteger(strCheckForUpdatesSection, 'Interval', FUpdateInterval);
      DecodeDate(Now, iYear, iMonth, iDay);
      WriteInteger(strCheckForUpdatesSection, 'Day', iDay);
      WriteInteger(strCheckForUpdatesSection, 'Month', iMonth);
      WriteInteger(strCheckForUpdatesSection, 'Year', iYear);
      WriteBool(strCheckForUpdatesSection, 'Enabled', FEnabled);
      UpdateFile;
    Finally
      Free;
    End;
End;

End.
