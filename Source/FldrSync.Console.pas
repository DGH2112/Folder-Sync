(**
  
  This module contains a class that encapsulates the console output for the application.

  @Author  David Hoyle
  @Version 1.0
  @Date    02 Jan 2019
  
**)
Unit FldrSync.Console;

Interface

Uses
  System.SysUtils, 
  Vcl.Graphics, 
  Winapi.Windows;

Type
  (** An enumerate to define the type of console output to be used. **)
  TConsoleHnd = (coStd, coErr);

  (** A class to implement the ISearchConsole interface for providing coloured output capabilities to
      the windows console. **)
  TFSConsole = Class
  Strict Private
    FConsoleHnd : Array[Low(TConsoleHnd)..High(TConsoleHnd)] Of THandle;
    FWidth      : Integer;
    FHeight     : Integer;
  Strict Protected
    Function  GetErrHnd: NativeUInt;
    Function  GetStdHnd: NativeUInt;
    Function  GetWidth : Integer;
    Function  CheckConsoleMode(Const eConsoleOutput : TConsoleHnd) : Boolean;
    Procedure CheckForEscape(Const iColour : TColor);
    // General Methods
    Procedure UpdateCursor(Const eConsoleHnd : TConsoleHnd; Const OldPos, NewPos : TCoord;
      Var wChars : DWORD; Const boolUpdateCursor : Boolean); InLine;
    Procedure UpdateConsoleColours(Const eConsoleHnd : TConsoleHnd; Const iTextColour : TColor;
      Const iBackColour : TColor; Const ConsoleInfo : TConsoleScreenBufferInfo; Var wChars : DWORD;
      Const strTABText : String); InLine;
    Procedure UpdateNewPos(Const ConsoleInfo : TConsoleScreenBufferInfo; Var NewPos : TCoord); InLine;
    Procedure ClearLine; InLine;
    Function  GetConsoleCharacter(Const Characters: TSysCharSet): Char;
    Function  BackGroundColour(Const iColour, iNone : TColor) : Integer;
    Function  ForeGroundColour(Const iColour, iNone : TColor): Integer;
    Procedure ProcessEscapeKey(Const iColour : TColor);
  Public
    Constructor Create;
    Procedure OutputToConsole(Const eConsoleHnd : TConsoleHnd; Const strText: String = '';
      Const iForeColour: TColor = clNone; Const iBackColour: TColor = clNone;
      Const boolUpdateCursor: Boolean = True);
    Procedure OutputToConsoleLn(Const eConsoleHnd : TConsoleHnd; Const strText: String = '';
      Const iForeColour: TColor = clNone; Const iBackColour: TColor = clNone;
      Const boolUpdateCursor: Boolean = True);
    (**
      This property returns the handle of the standard console output.
      @precon  None.
      @postcon Returns the handle of the standard console output.
      @return  a NativeUInt
    **)
    Property StdHnd : NativeUInt Read GetStdHnd;
    (**
      This property returns the handle of the error console output.
      @precon  None.
      @postcon Returns the handle of the error console output.
      @return  a NativeUInt
    **)
    Property ErrHnd : NativeUInt Read GetErrHnd;
  End;

Implementation

Uses
  System.Classes;

Type
  (** An enumerate to define the current console output mode **)
  TConsoleMode = (cmUnknown, cmStandard, cmRedirected);

Var
  (** A private variable to hold the console output mode **)
  ConsoleMode : TConsoleMode;

(**

  This function returns the background colour attribute for the console associated with the given cl#### 
  colour.

  @precon  CSBI must be a valid Console Screen Buffer Info structure.
  @postcon Returns the background colour attribute for the console associated with the given cl#### 
           colour.

  @param   iColour as a TColor as a constant
  @param   iNone   as a TColor as a constant
  @return  an Integer

**)
Function TFSConsole.BackGroundColour(Const iColour, iNone : TColor) : Integer;

ResourceString
  strInvalidConsoleColour = 'Invalid console colour.';

Begin
  Case iColour Of
    clBlack  : Result := 0;
    clMaroon  :Result := BACKGROUND_RED;
    clGreen   :Result := BACKGROUND_GREEN;
    clNavy    :Result := BACKGROUND_BLUE;
    clOlive   :Result := BACKGROUND_RED Or BACKGROUND_GREEN;
    clPurple  :Result := BACKGROUND_RED or BACKGROUND_GREEN;
    clTeal    :Result := BACKGROUND_BLUE Or BACKGROUND_GREEN;
    clGray    :Result := BACKGROUND_BLUE Or BACKGROUND_GREEN Or BACKGROUND_RED;
    clRed     :Result := BACKGROUND_RED   Or BACKGROUND_INTENSITY;
    clLime    :Result := BACKGROUND_GREEN Or BACKGROUND_INTENSITY;
    clBlue    :Result := BACKGROUND_BLUE  Or BACKGROUND_INTENSITY;
    clYellow  :Result := BACKGROUND_RED Or BACKGROUND_GREEN Or BACKGROUND_INTENSITY;
    clFuchsia :Result := BACKGROUND_BLUE Or BACKGROUND_RED  Or BACKGROUND_INTENSITY;
    clAqua    :Result := BACKGROUND_BLUE Or BACKGROUND_GREEN  Or BACKGROUND_INTENSITY;
    clWhite   :Result := BACKGROUND_BLUE Or BACKGROUND_GREEN Or BACKGROUND_RED Or BACKGROUND_INTENSITY;
    clNone    :Result  := iNone;
  Else
    Raise Exception.Create(strInvalidConsoleColour);
  End;
End;

(**

  This function returns the current mode of output of the console functions. The first time its gets the 
  console mode from the Win32 API.

  @precon  hndConsole must be a valid console handle.
  @postcon Returns the current mode of output of the console functions.

  @param   eConsoleOutput as a TConsoleHnd as a constant
  @return  a Boolean

**)
Function TFSConsole.CheckConsoleMode(Const eConsoleOutput : TConsoleHnd) : Boolean;

Var
  lpMode : Cardinal;

Begin
  If ConsoleMode = cmUnknown Then
    If Not GetConsoleMode(FConsoleHnd[eConsoleOutput], lpMode) Then
      ConsoleMode := cmRedirected
    Else
      ConsoleMode := cmStandard;
  Result := ConsoleMode = cmStandard;
End;

(**

  This method checks for the Escape having been pressed at the command line and if true prompts the user 
  as to whether they wish to terminate the process.

  @precon  None.
  @postcon If Esacape is pressed the user is prompted to stop the processing.

  @param   iColour as a TColor as a constant

**)
Procedure TFSConsole.CheckForEscape(Const iColour : TColor);

Const
  wBufferLength :  DWORD = 1024;

Var
  KBBuffer : Array Of TInputRecord;
  wCharsRead : DWord;
  wEvents : DWord;
  Hnd : THandle;
  i : Integer;

Begin
  Hnd := GetStdHandle(STD_INPUT_HANDLE);
  Win32Check(GetNumberOfConsoleInputEvents(Hnd, wEvents));
  If wEvents > 0 Then
    Begin
      SetLength(KBBuffer, wBufferLength);
      Win32Check(PeekConsoleInput(Hnd, KBBuffer[0], wBufferLength, wCharsRead));
      If wCharsRead > 0 Then
        For i := 0 To wEvents - 1 Do
          If KBBuffer[i].EventType = KEY_EVENT Then
            Begin
              If Boolean(KBBuffer[i].Event.KeyEvent.bKeyDown) Then
                Case KBBuffer[i].Event.KeyEvent.wVirtualKeyCode Of
                  VK_ESCAPE: ProcessEscapeKey(iColour);
                End;
              FlushConsoleInputBuffer(Hnd);
            End;
      FlushConsoleInputBuffer(Hnd);
    End;
End;

(**

  This method clears the current command line from the current cursor position to the
  end of the buffer on the same line.

  @precon  None.
  @postcon Clears the current command line from the current cursor position to the
           end of the buffer on the same line.

**)
Procedure TFSConsole.ClearLine;

Var
  ConsoleInfo: _CONSOLE_SCREEN_BUFFER_INFO;

Begin
  GetConsoleScreenBufferInfo(FConsoleHnd[coStd], ConsoleInfo);
  OutputToConsole(coStd, StringOfChar(#32, ConsoleInfo.dwSize.X -
    ConsoleInfo.dwCursorPosition.X - 1), clNone, clNone, False);
End;

(**

  A constructor for the TSearchConsole class.

  @precon  None.
  @postcon Initialises the console.

**)
Constructor TFSConsole.Create;

Var
  ConsoleInfo: TConsoleScreenBufferInfo;

Begin
  FConsoleHnd[coStd] := GetStdHandle(STD_OUTPUT_HANDLE);
  FConsoleHnd[coErr] := GetStdHandle(STD_ERROR_HANDLE);
  GetConsoleScreenBufferInfo(FConsoleHnd[coStd], ConsoleInfo);
  FWidth := ConsoleInfo.dwSize.X - 1;
  FHeight := ConsoleInfo.dwSize.Y;
End;

(**

  This function returns the background colour attribute for the console associated with the given cl#### 
  colour. Colour Matrix: Red(Maroon) Yellow(Olive) Lime(Green) White(Gray) Aqua(Teal) Blue(Navy)

  @precon  CSBI must be a valid Console Screen Buffer Info structure.
  @postcon Returns the background colour attribute for the console associated with the given cl#### 
           colour.

  @param   iColour as a TColor as a constant
  @param   iNone   as a TColor as a constant
  @return  an Integer

**)
Function TFSConsole.ForeGroundColour(Const iColour, iNone : TColor): Integer;

ResourceString
  strInvalidConsoleColour = 'Invalid console colour.';

Begin
  Case iColour Of
    clBlack  : Result := 0;
    clMaroon  :Result := FOREGROUND_RED;
    clGreen   :Result := FOREGROUND_GREEN;
    clNavy    :Result := FOREGROUND_BLUE;
    clOlive   :Result := FOREGROUND_RED Or FOREGROUND_GREEN;
    clPurple  :Result := FOREGROUND_RED Or FOREGROUND_BLUE;
    clTeal    :Result := FOREGROUND_BLUE Or FOREGROUND_GREEN;
    clGray    :Result := FOREGROUND_BLUE Or FOREGROUND_GREEN Or FOREGROUND_RED;
    clRed     :Result := FOREGROUND_RED   Or FOREGROUND_INTENSITY;
    clLime    :Result := FOREGROUND_GREEN Or FOREGROUND_INTENSITY;
    clBlue    :Result := FOREGROUND_BLUE  Or FOREGROUND_INTENSITY;
    clFuchsia :Result := FOREGROUND_RED Or FOREGROUND_BLUE Or FOREGROUND_INTENSITY;
    clYellow  :Result := FOREGROUND_RED Or FOREGROUND_GREEN Or FOREGROUND_INTENSITY;
    clAqua    :Result := FOREGROUND_BLUE Or FOREGROUND_GREEN  Or FOREGROUND_INTENSITY;
    clWhite   :Result := FOREGROUND_BLUE Or FOREGROUND_GREEN Or FOREGROUND_RED Or FOREGROUND_INTENSITY;
    clNone  : Result  := iNone;
  Else
    Raise Exception.Create(strInvalidConsoleColour);
  End;
End;

(**

  This method queries the console input buffer for key presses and returns the character pressed if its 
  in a predefined list.

  @precon  Characters is an array of chars that are valid inputs.
  @postcon Queries the console input buffer for key presses and returns the character pressed if its in 
           a predefined list.

  @param   Characters as a TSysCharSet as a constant
  @return  a Char

**)
Function TFSConsole.GetConsoleCharacter(Const Characters: TSysCharSet): Char;

Var
  Buffer  : TInputRecord;
  iCount  : Cardinal;
  hndInput: THandle;

Begin
  hndInput := GetStdHandle(STD_INPUT_HANDLE);
  Repeat
    Result := #0;
    If ReadConsoleInput(hndInput, Buffer, 1, iCount) Then
      If iCount > 0 Then
        If Buffer.EventType = KEY_EVENT Then
          If Buffer.Event.KeyEvent.bKeyDown Then
            Result := Buffer.Event.KeyEvent.UnicodeChar;
  Until CharInSet(Result, Characters);
End;

(**

  This is a getter method for the ErrHnd property.

  @precon  None.
  @postcon Returns the console error output handle.

  @return  a NativeUInt

**)
Function TFSConsole.GetErrHnd: NativeUInt;

Begin
  Result := FConsoleHnd[coErr];
End;

(**

  This is a getter method for the StdHnd property.

  @precon  None.
  @postcon Returns the console standard console output handle.

  @return  a NativeUInt

**)
Function TFSConsole.GetStdHnd: NativeUInt;

Begin
  Result := FConsoleHnd[coStd];
End;

(**

  This is a getter method for the Width property.

  @precon  None.
  @postcon Returns the width of the console.

  @return  an Integer

**)
Function TFSConsole.GetWidth: Integer;

Begin
  Result := FWidth;
End;

(**

  This function outputs the given text to the console references by the given handle using the text and 
  background colours provided. If xlNone is used for the colours then the consoles default colours are 
  used. DOES NOT add a carraige return at the end of each line.

  @precon  hndConsole must be a valid console handle.
  @postcon Outputs the given text to the console references by the given handle using the text and 
           background colours provided.

  @param   eConsoleHnd      as a TConsoleHnd as a constant
  @param   strText          as a String as a constant
  @param   iForeColour      as a TColor as a constant
  @param   iBackColour      as a TColor as a constant
  @param   boolUpdateCursor as a Boolean as a constant

**)
Procedure TFSConsole.OutputToConsole(Const eConsoleHnd: TConsoleHnd; Const strText: String;
  Const iForeColour, iBackColour: TColor; Const boolUpdateCursor: Boolean);

Var
  ConsoleInfo: TConsoleScreenBufferInfo;
  wChars: DWORD;
  OldPos: TCoord;
  NewPos: TCoord;
  strTABText: String;

Begin
  strTABText := StringReplace(strText, #9, #175, [rfReplaceAll]);
  If CheckConsoleMode(eConsoleHnd) Then
    Begin
      Repeat
        Win32Check(GetConsoleScreenBufferInfo(FConsoleHnd[eConsoleHnd], ConsoleInfo));
        OldPos := ConsoleInfo.dwCursorPosition;
        NewPos := OldPos;
        Win32Check(WriteConsoleOutputCharacter(FConsoleHnd[eConsoleHnd], PChar(strTABText),
          Length(strTABText),
          ConsoleInfo.dwCursorPosition, wChars));
        UpdateConsoleColours(eConsoleHnd, iForeColour, iBackColour, ConsoleInfo, wChars, strTABText);
        If wChars > 0 Then
          Delete(strTABText, 1, wChars);
        Inc(NewPos.X, wChars);
        UpdateNewPos(ConsoleInfo, NewPos);
        If strTABText <> '' Then
          Begin
            Win32Check(WriteConsole(FConsoleHnd[eConsoleHnd], PChar(#13#10), Length(#13#10), wChars, Nil));
            Inc(NewPos.Y);
            NewPos.X := 0;
          End;
      Until strTABText = '';
      UpdateCursor(eConsoleHnd, OldPos, NewPos, wChars, boolUpdateCursor);
    End Else
      Write(strTABText);
End;

(**

  This function outputs the given text to the console references by the given handle using the text and 
  background colours provided. If xlNone is used for the colours then the consoles default colours are 
  used. Adds a Carriage Return at the end of each line.

  @precon  hndConsole must be a valid console handle.
  @postcon Outputs the given text to the console references by the given handle using the text and 
           background colours provided.

  @param   eConsoleHnd      as a TConsoleHnd as a constant
  @param   strText          as a String as a constant
  @param   iForeColour      as a TColor as a constant
  @param   iBackColour      as a TColor as a constant
  @param   boolUpdateCursor as a Boolean as a constant

**)
Procedure TFSConsole.OutputToConsoleLn(Const eConsoleHnd: TConsoleHnd; Const strText: String;
  Const iForeColour, iBackColour: TColor; Const boolUpdateCursor: Boolean);

Var
  sl : TStringList;
  i : Integer;
  wChars : Cardinal;

Begin
  sl := TStringList.Create;
  Try
    sl.Text := strText;
    If sl.Text = '' Then
      sl.Text := #13#10;
    For i := 0 to sl.Count - 1 Do
      Begin
        If CheckConsoleMode(eConsoleHnd) Then
          Begin
            OutputToConsole(eConsoleHnd, sl[i], iForeColour, iBackColour, boolUpdateCursor);
            Win32Check(WriteConsole(FConsoleHnd[eConsoleHnd], PChar(#13#10), Length(#13#10), wChars, Nil));
          End Else
            WriteLn(sl[i]);
      End;
  Finally
    sl.Free;
  End;
End;

(**

  This method outputs a message on the screen asking the user if they wish to stop processing. If the 
  user pressews Y then the application is terminated through an Abort exception.

  @precon  None.
  @postcon If the user presses Y Abort is raised to stop the application.

  @param   iColour as a TColor as a constant

**)
Procedure TFSConsole.ProcessEscapeKey(Const iColour : TColor);

ResourceString
  strMsg = 'Are you sure you want to stop the search? (Y/N): ';
  strYes = 'Yes';

Var
  C: Char;
  ConsoleInfo : TConsoleScreenBufferInfo;
  OldPos : TCoord;

Begin
  Win32Check(GetConsoleScreenBufferInfo(FConsoleHnd[coStd], ConsoleInfo));
  OldPos := ConsoleInfo.dwCursorPosition;
  ClearLine;
  OutputToConsole(coStd, strMsg, iColour);
  C := GetConsoleCharacter(['y', 'Y', 'n', 'N']);
  Case C Of
    'y', 'Y':
      Begin
        OutputToConsoleLn(coStd, strYes, iColour);
        Abort
      End;
    'n', 'N':
      Begin
        Win32Check(SetConsoleCursorPosition(FConsoleHnd[coStd], OldPos));
        OutputToConsole(coStd, StringOfChar(#32, Length(strMsg)));
        Win32Check(SetConsoleCursorPosition(FConsoleHnd[coStd], OldPos));
      End;
  End;
End;

(**

  This method sets the fore and backgroudn colours for the console output.

  @precon  None.
  @postcon The console colour attributes are updated with the required colours.

  @param   eConsoleHnd as a TConsoleHnd as a constant
  @param   iTextColour as a TColor as a constant
  @param   iBackColour as a TColor as a constant
  @param   ConsoleInfo as a TConsoleScreenBufferInfo as a constant
  @param   wChars      as a DWORD as a reference
  @param   strTABText  as a String as a constant

**)
Procedure TFSConsole.UpdateConsoleColours(Const eConsoleHnd : TConsoleHnd;
  Const iTextColour : TColor; Const iBackColour : TColor; Const ConsoleInfo : TConsoleScreenBufferInfo;
  Var wChars : DWORD; Const strTABText : String);

Const
  iForegroundMask = $0F;
  iBackgroundMask = $F0;

Var
  iForeAttrColour, iBackAttrColour : Integer;
  iChar : Integer;
  Attrs : Array of Word;
  
Begin
  SetLength(Attrs, wChars);
  iForeAttrColour := ForeGroundColour(iTextColour, ConsoleInfo.wAttributes And iForegroundMask);
  iBackAttrColour := BackGroundColour(iBackColour, ConsoleInfo.wAttributes And iBackgroundMask);
  If wChars > 0 Then
    For iChar := 0 To wChars - 1 Do
      Attrs[iChar] := iForeAttrColour Or iBackAttrColour;
  Win32Check(WriteConsoleOutputAttribute(FConsoleHnd[eConsoleHnd], Attrs,
    Length(strTABText), ConsoleInfo.dwCursorPosition, wChars));
End;
  
(**

  This method sets the position of the cursor after the output of the string to the console.

  @precon  None.
  @postcon The position of the console cursor is updated.

  @param   eConsoleHnd      as a TConsoleHnd as a constant
  @param   OldPos           as a TCoord as a constant
  @param   NewPos           as a TCoord as a constant
  @param   wChars           as a DWORD as a reference
  @param   boolUpdateCursor as a Boolean as a constant

**)
Procedure TFSConsole.UpdateCursor(Const eConsoleHnd : TConsoleHnd; Const OldPos, NewPos : TCoord;
  Var wChars : DWORD; Const boolUpdateCursor : Boolean);

Begin
  If boolUpdateCursor Then
    Begin
      // The only time the below fails is at the end of the buffer and a new
      // line is required, hence the new line on failure.
      If Not SetConsoleCursorPosition(FConsoleHnd[eConsoleHnd], NewPos) Then
        Win32Check(WriteConsole(FConsoleHnd[eConsoleHnd], PChar(#13#10), Length(#13#10), wChars, Nil));
    End Else
      Win32Check(SetConsoleCursorPosition(FConsoleHnd[eConsoleHnd], OldPos));
End;
  
(**

  This procedure update the NewPos record based on the console information.

  @precon  None.
  @postcon The NewPos record is updated.

  @param   ConsoleInfo as a TConsoleScreenBufferInfo as a constant
  @param   NewPos      as a TCoord as a reference

**)
Procedure TFSConsole.UpdateNewPos(Const ConsoleInfo : TConsoleScreenBufferInfo; Var NewPos : TCoord);

Begin
  While NewPos.X >= ConsoleInfo.dwSize.X Do
    Begin
      Inc(NewPos.Y);
      Dec(NewPos.X, ConsoleInfo.dwSize.X);
    End;
End;

(** Initialises the console more to Unknown to force a call to the Win32 API **)
Initialization
  ConsoleMode := cmUnknown;
End.
