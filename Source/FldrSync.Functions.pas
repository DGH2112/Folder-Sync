(**
  
  This module contains useful function for use throughout the application.

  @Author  David Hoyle
  @Version 1.0
  @Date    02 Jan 2019
  
**)
Unit FldrSync.Functions;

Interface

Uses
  System.Classes,
  VCL.Graphics;

Type
  (** This is a procedure type for handling Exception messages in ParseMacro. **)
  TExceptionProcedure = Procedure(Const strExceptionMsg : String) Of Object;

  (** A record to encapsulate function for use throughout the application. **)
  TFSFunctions = Record
  Strict Private
    Class Function ComputerName : String; Static;
    Class Function UserName : String; Static;
  Public
    Class Function  Like(Const strPattern, strText : String) : Boolean; Static;
    Class Function  PosOfNthChar(Const strText : String; Const Ch : Char; Const iIndex : Integer;
      Const boolIgnoreQuotes : Boolean = True): Integer; Static;
    Class Function BuildRootKey(Const slParams : TStringList;
      Const ExceptionProc : TExceptionProcedure) : String; Static;
    Class Function GetConsoleTitle(Const strTitle: String) : String; Static;
    Class Function GetBuildNumber(Const strFileName : String; var iMajor, iMinor, iBugfix,
      iBuild : Integer) : String; Static;
    Class Function CalcColour(Const dblValue, dblLowCriteria, dblMiddleCriteria,
      dblUpperCriteria : Double; Const iLowColour, iMiddleColour, iHighColour : TColor) : TColor; Static;
    Class Function ExtractEXEFromExt(Const strExt : String) : String; Static;
  End;

Implementation

Uses
  System.SysUtils,
  System.Win.ComObj,
  WinAPI.Windows,
  WinAPI.ShlObj;

Const
  (** A list of bug fix letters as a string array. **)
  strBugFix = ' abcdefghijklmnopqrstuvwxyz';

(**

  This method builds the root key INI filename for the loading and saving of settings from the instance 
  handle for the module.

  @precon  slParams must be a valid instance of a TStringList class.
  @postcon Builds the root key INI filename for the loading and saving of settings from the instance 
           handle for the module.

  @param   slParams      as a TStringList as a constant
  @param   ExceptionProc as a TExceptionProcedure as a constant
  @return  a String

**)
Class Function TFSFunctions.BuildRootKey(Const slParams: TStringList;
  Const ExceptionProc: TExceptionProcedure): String;


ResourceString
  strExpectedSquare = 'Expected "[" at position 3 in alternate INI file parameter.';
  strExpectedClosingSquare = 'Expected a closing "]" in alternate INI file parameter.';
  strPathDoesNotExist = 'The path "%s" does not exist for the alternate INI file.';
  strINIPattern = '%s Settings for %s on %s.INI';
  strSeasonsFall = '\Season''s Fall\';

  (**

    This function parses the alternate INI filename from the parameter.

    @precon  None.
    @postcon Parses the alternate INI filename from the parameter.

    @param   strDefaultINI as a String as a constant
    @param   strParam      as a String as a constant
    @return  a String

  **)
  Function ParseAlternateINIFile(Const strDefaultINI, strParam : String) : String;

  Const
    iThirdChar = 3;

  Var
    i : Integer;
    strFileName : String;

  Begin
    Result := strDefaultINI;
    i := iThirdChar;
    If strParam[i] <> '[' Then
      If Assigned(ExceptionProc) Then
        Begin
          ExceptionProc(strExpectedSquare);
          Exit;
        End;
    Inc(i);
    strFileName := '';
    While (i <= Length(strParam)) And (strParam[i] <> ']') Do
      Begin
        strFileName := strFileName + strParam[i];
        Inc(i);
        If i > Length(strParam) Then
          If Assigned(ExceptionProc) Then
            Begin
              ExceptionProc(strExpectedClosingSquare);
              Exit;
            End;
      End;
    strFileName := ExpandUNCFileName(strFileName);
    If DirectoryExists(ExtractFilePath(strFileName)) Then
      Result := strFileName
    Else
      If Assigned(ExceptionProc) Then
        ExceptionProc(Format(strPathDoesNotExist, [ExtractFilePath(strFileName)]));
  End;

Const
  strExe = '*.exe';
  iSecChar = 2;
  iFirstChar = 1;

var
  strModuleName : String;
  strINIFileName : String;
  strUserAppDataPath : String;
  strBuffer : String;
  iParam : Integer;
  iSize : Integer;                                                              

begin
  SetLength(strBuffer, MAX_PATH);
  iSize := GetModuleFileName(hInstance, PChar(strBuffer), MAX_PATH);
  SetLength(strBuffer, iSize);
  strModuleName := strBuffer;
  strINIFileName := ChangeFileExt(ExtractFileName(strBuffer), '');
  While (Length(strIniFilename) > 0) And
    (CharInSet(strIniFileName[Length(strIniFilename)], ['0'..'9'])) Do
    strIniFileName := Copy(strIniFileName, 1, Length(strIniFileName) - 1);
  strINIFileName :=  Format(strINIPattern, [strIniFileName, UserName, ComputerName]);
  SetLength(strBuffer, MAX_PATH);
  SHGetFolderPath(0, CSIDL_APPDATA Or CSIDL_FLAG_CREATE, 0, SHGFP_TYPE_CURRENT,
    PChar(strBuffer));
  strBuffer := StrPas(PChar(strBuffer));
  strUserAppDataPath := strBuffer + strSeasonsFall;
  If Not DirectoryExists(strUserAppDataPath) Then
    ForceDirectories(strUserAppDataPath);
  Result := strUserAppDataPath + strINIFileName;
  If Like(strExe, ExtractFileExt(strModuleName)) Then
    If slParams <> Nil Then
      For iParam := 1 To ParamCount Do
        Begin
          If Length(ParamStr(iParam)) > 0 Then
            If CharInSet(ParamStr(iParam)[iFirstChar], ['-', '/']) Then
              If Length(ParamStr(iParam)) > iFirstChar Then
                If CharInSet(ParamStr(iParam)[iSecChar], ['@']) Then
                  Begin
                    Result := ParseAlternateINIFile(Result, ParamStr(iParam));
                    Continue;
                  End;
          slParams.Add(ParamStr(iParam));
        End;
End;

(**

  This method interpolates a colour for the specified percentage position within the colour and position 
  information passed.

  @precon  None.
  @postcon Interpolates a colour for the specified percentage position within the colour and position 
           information passed..

  @param   dblValue          as a Double as a constant
  @param   dblLowCriteria    as a Double as a constant
  @param   dblMiddleCriteria as a Double as a constant
  @param   dblUpperCriteria  as a Double as a constant
  @param   iLowColour        as a TColor as a constant
  @param   iMiddleColour     as a TColor as a constant
  @param   iHighColour       as a TColor as a constant
  @return  a TColor

**)
Class Function TFSFunctions.CalcColour(Const dblValue, dblLowCriteria, dblMiddleCriteria,
  dblUpperCriteria : Double; Const iLowColour, iMiddleColour, iHighColour : TColor) : TColor;

  (**

    This function calculate the intepolation of a single colour value between 2 colours based on value 
    for those colour positions.

    @precon  None.
    @postcon Returns the colour which is an interpolation between the input colours.

    @param   iLow     as a TColor as a constant
    @param   iHigh    as a TColor as a constant
    @param   iMask    as a TColor as a constant
    @param   dblLow   as a Double as a constant
    @param   dblValue as a Double as a constant
    @param   dblHigh  as a Double as a constant
    @return  a TColor

  **)
  Function InterpolateColour(Const iLow, iHigh, iMask : TColor; Const dblLow,
    dblValue, dblHigh : Double) : TColor;

  Var
    iColourDiff : TColor;

  Begin
    iColourDiff := iHigh And iMask - iLow And iMask;
    Result := Round(iLow And iMask + iColourDiff * (dblValue - dblLow) /
      (dblHigh - dblLow)) And iMask;
  End;

  (**

    This function calculate the intepolation of a colour value between 2 colours based on value for those
    colour positions.

    @precon  None.
    @postcon Returns the colour which is an interpolation between the input colours.

    @param   iLow     as a TColor as a constant
    @param   iHigh    as a TColor as a constant
    @param   dblLow   as a Double as a constant
    @param   dblValue as a Double as a constant
    @param   dblHigh  as a Double as a constant
    @return  a TColor

  **)
  Function InterpolateColours(Const iLow, iHigh : TColor; Const dblLow,
    dblValue, dblHigh : Double) : TColor;

  Const
    iBlueMask = $FF0000;
    iGreenMask = $00FF00;
    iRedMask = $0000FF;

  Begin
    Result :=
      InterpolateColour(iLow, iHigh, iBlueMask, dblLow, dblValue, dblHigh) +
      InterpolateColour(iLow, iHigh, iGreenMask, dblLow, dblValue, dblHigh) +
      InterpolateColour(iLow, iHigh, iRedMask, dblLow, dblValue, dblHigh);
  End;

Begin
  If dblValue <= dblLowCriteria Then
    Result := iLowColour
  Else If dblValue <= dblMiddleCriteria Then
    Result := InterpolateColours(
      ColorToRGB(iLowColour),
      ColorToRGB(iMiddleColour),
      dblLowCriteria,
      dblValue,
      dblMiddleCriteria)
  Else If dblValue <= dblUpperCriteria then
    Result := InterpolateColours(
      ColorToRGB(iMiddleColour),
      ColorToRGB(iHighColour),
      dblMiddleCriteria,
      dblValue,
      dblUpperCriteria)
  Else
    Result := iHighColour;
End;

(**

  This function returns the users computer name as a String.

  @precon  None.
  @postcon Returns the users computer name as a String.

  @return  a String

**)
Class Function TFSFunctions.ComputerName : String;

Const
  iMaxBufLen = 1024;

Var
  i : Cardinal;

Begin
  i := iMaxBufLen;
  SetLength(Result, i);
  GetComputerName(@Result[1], i);
  Win32Check(LongBool(i));
  SetLength(Result, i);
End;

(**

  This function extracts from the registry the EXE name that is associated with the passed file extension
  .

  @precon  None.
  @postcon Returns the executable file associated with the file extension.

  @param   strExt as a String as a constant
  @return  a String

**)
Class Function TFSFunctions.ExtractEXEFromExt(Const strExt : String) : String;

Const
  iSize = 1024;
  iSecChar = 2;
  strShellOpenCommand = '\shell\open\command';

Var
  strFileClass : String;
  strExpanded : String;
  iOutputSize : Integer;

Begin
  strFileClass := GetRegStringValue(strExt, '');
  If strFileClass <> '' Then
    Result := GetRegStringValue(strFileClass + strShellOpenCommand, '');
  SetLength(strExpanded, iSize);
  iOutputSize := ExpandEnvironmentStrings(PChar(Result), PChar(strExpanded), iSize);
  SetLength(strExpanded, iOutputSize - 1);
  strExpanded := Trim(StringReplace(strExpanded, '%1', '', [rfReplaceAll]));
  If (Length(strExpanded) > 0) And (strExpanded[1] = '"') And
    (strExpanded[Length(strExpanded)] = '"') Then
    strExpanded := Copy(strExpanded, iSecChar, Length(strExpanded) - iSecChar);
  Result := strExpanded;
End;

(**

  This routine extract the build number from the EXE resources for display in the app title.

  @precon  None.
  @postcon Extract the build number from the EXE resources for display in the app title.

  @param   strFileName as a String as a constant
  @param   iMajor      as an Integer as a reference
  @param   iMinor      as an Integer as a reference
  @param   iBugfix     as an Integer as a reference
  @param   iBuild      as an Integer as a reference
  @return  a String

**)
Class Function TFSFunctions.GetBuildNumber(Const strFileName : String; Var iMajor, iMinor, iBugfix,
  iBuild : Integer) : String;

Const
  strBuild = '%d.%d.%d.%d';
  iDWordMask = $FFFF;
  iDWordShift = 16;

ResourceString
  strExeDoesNotContainAnyVerInfo = 'The executable "%s" does not contain any version information.';

Var
  VerInfoSize: DWORD;
  VerInfo: Pointer;
  VerValueSize: DWORD;
  VerValue: PVSFixedFileInfo;
  Dummy: DWORD;

Begin
  VerInfoSize := GetFileVersionInfoSize(PChar(strFileName), Dummy);
  If VerInfoSize <> 0 Then
    Begin
      GetMem(VerInfo, VerInfoSize);
      GetFileVersionInfo(PChar(strFileName), 0, VerInfoSize, VerInfo);
      VerQueryValue(VerInfo, '\', Pointer(VerValue), VerValueSize);
      iMajor := VerValue^.dwFileVersionMS shr iDWordShift;
      iMinor := VerValue^.dwFileVersionMS and iDWordMask;
      iBugfix := VerValue^.dwFileVersionLS shr iDWordShift;
      iBuild := VerValue^.dwFileVersionLS and iDWordMask;
      Result := Format(strBuild, [iMajor, iMinor, iBugfix, iBuild]);
      FreeMem(VerInfo, VerInfoSize);
    End Else
      Raise Exception.CreateFmt(strExeDoesNotContainAnyVerInfo, [strFileName]);
End;

(**

  This method returns a string for the title of a console application containing the version number and 
  build number.

  @precon  Expects a string to contains %d.%d%s for the version number followed by %s for the build 
           number.
  @postcon Returns a string for the title of a console application containing the version number and 
           build number.

  @param   strTitle as a String as a constant
  @return  a String

**)
Class Function TFSFunctions.GetConsoleTitle(Const strTitle: String) : String;

ResourceString
  strWrittenByDavidHoyle = 'Written by David Hoyle (c) %s';

Const
  strDateFmt = 'ddd dd/mmm/yyyy';
  {$IFDEF WIN64}
  str64Bit = '64-bit';
  {$ELSE}
  str32Bit = '32-bit';
  {$ENDIF}

Var
  iMajor, iMinor, iBugfix, iBuild : Integer;
  strBuildNumber  : String;
  dtDate : TDateTime;
  strPlatform : String;

Begin
  strBuildNumber := GetBuildNumber(ParamStr(0), iMajor, iMinor, iBugFix, iBuild);
  {$IFDEF WIN64}
  strPlatform := str64Bit;
  {$ELSE}
  strPlatform := str32Bit;
  {$ENDIF}
  Result := Format(strTitle, [iMajor, iMinor, strBugFix[iBugFix + 1],
    strBuildNumber, strPlatform]);
  FileAge(ParamStr(0), dtDate);
  Result := Result + #13#10 +
    Format(strWrittenByDavidHoyle, [FormatDateTime(strDateFmt, dtDate)]);
End;

(**

  This function returns true if the pattern matches the text.

  @precon  None.
  @postcon Returns true if the pattern matches the text.

  @param   strPattern as a String as a constant
  @param   strText    as a String as a constant
  @return  a Boolean

**)
Class Function TFSFunctions.Like(Const strPattern, strText : String) : Boolean;

Type
  TMatchType = (mtStart, mtEnd);
  TMatchTypes = Set Of TMatchType;

  (**

    This function checks that the last part of the pattern matches the end of the input string and
    returns true if so.

    @precon  strParts must be a valid array.
    @postcon Returns true if the last pattern matches the end of the input string.

    @param   strParts   as a TArray<String> as a constant
    @param   MatchTypes as a TMatchTypes as a constant
    @return  a Boolean

  **)
  Function CheckEnd(Const strParts : TArray<String>; Const MatchTypes : TMatchTypes) : Boolean;

  Begin
    Result := False;
    If Length(strParts) > 0 Then
      If mtEnd In MatchTypes Then
        If CompareText(strParts[Length(strParts) - 1], Copy(strText, Length(strText) -
          Length(strParts[Length(strParts) - 1]) + 1, Length(strParts[Length(strParts) - 1]))) <> 0 Then
          Result := True;
  End;

  (**

    This function checks that the middle parts of the pattern match the middle of the input string and 
    returns true if so.

    @precon  strParts must be a valid array.
    @postcon Returns true if the middle patterns match the middle of the input string.

    @param   strParts    as a TArray<String> as a constant
    @param   MatchTypes  as a TMatchTypes as a constant
    @param   iStartIndex as an Integer as a reference
    @return  a Boolean

  **)
  Function CheckInBetween(Const strParts : TArray<String>; Const MatchTypes : TMatchTypes;
    Var iStartIndex : Integer) : Boolean;

  Var
    iPos: Integer;
    i: Integer;
  
  Begin
    Result := False;
    For i := Integer(mtStart In MatchTypes) To Length(strParts) - 1 - Integer(mtEnd In MatchTypes) Do
      Begin
        iPos := Pos(strParts[i], LowerCase(strText));
        If (iPos = 0) Or (iPos < iStartIndex) Then
          Result := True;
        Inc(iStartIndex, Length(strParts[i]));
      End;
  End;

  (**

    This function checks that the first part of the pattern matches the start of the input string and 
    returns true if so.

    @precon  strParts must be a valid array.
    @postcon Returns true if the first pattern matches the startof the input string.

    @param   strParts    as a TArray<String> as a constant
    @param   MatchTypes  as a TMatchTypes as a constant
    @param   iStartIndex as an Integer as a reference
    @return  a Boolean

  **)
  Function CheckStart(Const strParts : TArray<String>; Const MatchTypes : TMatchTypes;
    Var iStartIndex : Integer) : Boolean;

  Begin
    Result := False;
    iStartIndex := 1;
    If Length(strParts) > 0 Then
      If mtStart In MatchTypes Then
        If CompareText(strParts[0], Copy(strText, 1, Length(strParts[0]))) <> 0 Then
          Result := True
        Else
          Inc(iStartIndex, Length(strParts[0]));
  End;

  (**

    This method processes the parts of the pattern for matches.

    @precon  None.
    @postcon Returns True if the parts are confirmed as matching the input else returns false.

    @param   strPattern as a String as a constant
    @param   MatchTypes as a TMatchTypes as a constant
    @return  a Boolean

  **)
  Function ProcessParts(Const strPattern : String; Const MatchTypes : TMatchTypes) : Boolean;

  Var
    strParts : TArray<String>;
    iStartIndex : Integer;
    
  Begin
    strParts := strPattern.Split(['*']);
    Result := Not(
      CheckStart(strParts, MatchTypes, iStartIndex) Or
      CheckInBetween(strParts, MatchTypes, iStartIndex) Or
      CheckEnd(strParts, MatchTypes));
  End;

Var
  MatchTypes : TMatchTypes;
  strLPattern : String;

Begin
  Result := False;
  MatchTypes := [];
  strLPattern := strPattern;
  If Length(strLPattern) = 0 Then
    Exit;
  If strLPattern = '*' Then
    Begin
      Result := True;
      Exit;
    End;
  If strLPattern[1] <> '*' Then
    Include(MatchTypes, mtStart)
  Else
    Delete(strLPattern, 1, 1);
  If Length(strLPattern) > 0 Then
    If strLPattern[Length(strLPattern)] <> '*' Then
      Include(MatchTypes, mtEnd)
    Else
      Delete(strLPattern, Length(strLPattern), 1);
  Result := ProcessParts(strLPattern, MatchTypes);
End;

(**

  This routine returns the position of the Nth occurrance of the character in the text.

  @precon  None.
  @postcon Returns the position of the Nth occurrance of the character in the text.

  @param   strText          as a String as a constant
  @param   Ch               as a Char as a constant
  @param   iIndex           as an Integer as a constant
  @param   boolIgnoreQuotes as a Boolean as a constant
  @return  an Integer

**)
Class Function TFSFunctions.PosOfNthChar(Const strText : String; Const Ch : Char;
  Const iIndex : Integer; Const boolIgnoreQuotes : Boolean = True): Integer;

Var
  i : Integer;
  iCount : Integer;
  boolInQuotes : Boolean;

Begin
  Result := 0;
  iCount := 0;
  boolInQuotes := False;
  For i := 1 To Length(strText) Do
    Begin
      If Not boolIgnoreQuotes Then
        If strText[i] = '"' Then
          boolInQuotes := Not boolInQuotes;
      If strText[i] = Ch Then
        If Not boolInQuotes Then
          Inc(iCount);
      If iIndex = iCount Then
        Begin
          Result := i;
          Exit;
        End;
    End;
End;

(**

  This function returns the users logon name as a String.

  @precon  None.
  @postcon Returns the users logon name as a String.

  @return  a String

**)
Class Function TFSFunctions.UserName : String;

Const
  iMaxBufLen = 1024;

Var
  i : Cardinal;

Begin
  i := iMaxBufLen;
  SetLength(Result, i);
  GetUserName(@Result[1], i);
  Win32Check(LongBool(i));
  SetLength(Result, i - 1);
End;

End.
