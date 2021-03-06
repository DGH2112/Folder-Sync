(**

  This module contains graphics control which provide information on the
  status of the battery in the computer.

  @Author  David Hoyle
  @Version 1.0
  @Date    02 Jan 2019

**)
Unit DGHMemoryMonitorControl;

Interface

Uses
  SysUtils,
  Classes,
  ExtCtrls,
  Windows,
  Messages,
  Graphics,
  DGHCustomGraphicsControl;

Type
  (** An enumerate to define the different memory block types. **)
  TDGHMemType = (dmtLarge, dmtMedium, dmtSmall);

  (** A class to represent the Battery Monitor Grasphics control. **)
  TDGHMemoryMonitor = Class(TDGHCustomGraphicControl)
  Private
    { Private declarations }
    FTimer         : TTimer;
    FUpdateInterval: Integer;
    FUsed          : Cardinal;
    FReserved      : Cardinal;
    FPercentage    : TPercentage;
    FHighColour    : TColor;
    FHalfColour    : TColor;
    FLowColour     : TColor;
    FHighFontColour: TColor;
    FHalfFontColour: TColor;
    FLowFontColour : TColor;
    FBackColour    : TColor;
    FHighPoint     : TPercentage;
    FLowPoint      : TPercentage;
    FHalfPoint     : TPercentage;
    FBackFontColour: TColor;
    FMemBlocks     : Array[Low(TDGHMemType)..High(TDGHMemType)] Of Cardinal;
  Protected
    { Protected declarations }
    Procedure Paint; Override;
    Procedure SetUpdateInterval(Const Value: Integer); Virtual;
    Procedure SetLowPoint(Const Value: TPercentage); Virtual;
    Procedure SetHighPoint(Const Value: TPercentage); Virtual;
    Procedure SetHalfPoint(Const Value: TPercentage); Virtual;
    Procedure UpdateControl(Sender: TObject); Virtual;
    Procedure GetFFMemoryManagerState; Virtual;
  Public
    { Public declarations }
    Constructor Create(AOwner: TComponent); Override;
    Destructor Destroy; Override;
  Published
    { Published declarations }
    (**
      This property gets or sets the timing interval between information updates.
      @precon  None.
      @postcon Gets or sets the timing interval between information updates.
      @return  an Integer
    **)
    Property UpdateInterval: Integer Read FUpdateInterval Write SetUpdateInterval
      Default 5000;
    (**
      This property gets or set the back ground colour of the control.
      @precon  None.
      @postcon Gets or set the back ground colour of the control.
      @return  a TColor
    **)
    Property BackColour: TColor Read FBackColour Write FBackColour Default clGray;
    (**
      This property gets or set the back ground font colour of the control.
      @precon  None.
      @postcon Gets or set the back ground font colour of the control.
      @return  a TColor
    **)
    Property BackFontColour: TColor Read FBackFontColour Write FBackFontColour
      Default clBlack;
    (**
      This property gets or set the High Battery colour of the control.
      @precon  None.
      @postcon Gets or set the High Battery colour of the control.
      @return  a TColor
    **)
    Property HighColour: TColor Read FHighColour Write FHighColour Default clRed;
    (**
      This property gets or set the Half Empty Battery colour of the control.
      @precon  None.
      @postcon Gets or set the Half Empty Battery colour of the control.
      @return  a TColor
     **)
    Property HalfColour: TColor Read FHalfColour Write FHalfColour Default $00CCFF;
    (**
      This property gets or set the Low Battery colour of the control.
      @precon  None.
      @postcon Gets or set the Low Battery colour of the control.
      @return  a TColor
     **)
    Property LowColour: TColor Read FLowColour Write FLowColour Default clLime;
    (**
      This property gets or set the High Battery Font colour of the control.
      @precon  None.
      @postcon Gets or set the High Battery Font colour of the control.
      @return  a TColor
    **)
    Property HighFontColour: TColor Read FHighFontColour Write FHighFontColour
      Default clBlack;
    (**
      This property gets or set the Half Empty Battery Font colour of the control.
      @precon  None.
      @postcon Gets or set the Half Empty Battery Font colour of the control.
      @return  a TColor
    **)
    Property HalfFontColour: TColor Read FHalfFontColour Write FHalfFontColour
      Default clBlack;
    (**
      This property gets or set the Low Battery Font colour of the control.
      @precon  None.
      @postcon Gets or set the Low Battery Font colour of the control.
      @return  a TColor
    **)
    Property LowFontColour: TColor Read FLowFontColour Write FLowFontColour
      Default clBlack;
    (**
      This property gets or set the Low Battery Point (Percentage) of the control.
      @precon  None.
      @postcon Gets or set the Low Battery Point (Percentage) of the control.
      @return  a TPercentage
    **)
    Property LowPoint: TPercentage Read FLowPoint Write SetLowPoint Default 10;
    (**
      This property gets or set the Half Empty Battery Point (Percentage) of the control.
      @precon  None.
      @postcon Gets or set the Half Empty Battery Point (Percentage) of the control.
      @return  a TPercentage
    **)
    Property HalfPoint: TPercentage Read FHalfPoint Write SetHalfPoint Default 50;
    (**
      This property gets or set the High Battery Point (Percentage) of the control.
      @precon  None.
      @postcon Gets or set the High Battery Point (Percentage) of the control.
      @return  a TPercentage
    **)
    Property HighPoint: TPercentage Read FHighPoint Write SetHighPoint Default 90;
    (** Publish this existing property. @precon  None. @postcon None. **)
    Property Align;
    (** Publish this existing property. @precon  None. @postcon None. **)
    Property BevelInner;
    (** Publish this existing property. @precon  None. @postcon None. **)
    Property BevelOuter;
    (** Publish this existing property. @precon  None. @postcon None. **)
    Property BevelWidth;
    (** Publish this existing property. @precon  None. @postcon None. **)
    Property BorderWidth;
    (** Publish this existing property. @precon  None. @postcon None. **)
    Property Font;
  End;

Procedure Register;

Implementation

Uses
  Types,
  Themes;

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
Function CalcColour(Const dblValue, dblLowCriteria, dblMiddleCriteria,
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

  Begin
    Result :=
      InterpolateColour(iLow, iHigh, $FF0000, dblLow, dblValue, dblHigh) +
      InterpolateColour(iLow, iHigh, $00FF00, dblLow, dblValue, dblHigh) +
      InterpolateColour(iLow, iHigh, $0000FF, dblLow, dblValue, dblHigh);
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

  This function calculates a shorter representation of a memory size and returns a string representing 
  the value.

  @precon  None.
  @postcon Returns a string representation of the given size.

  @param   iSize as a Cardinal as a constant
  @return  a String

**)
Function CalcSize(Const iSize: Cardinal): String;

ResourceString
  strUnknown = 'Unknown';

Const
  dblKILOBYTE: Double = 1024.0;
  dblMEGABYTE: Double = 1024.0 * 1024.0;
  strKileByteFmt = '%1.2nK';
  strMegaByteFmt = '%1.2nM';

Begin
  Result := strUnknown;
  If iSize < dblKILOBYTE Then
    Result := Format('%1.0n', [Int(iSize)])
  Else If iSize < dblMEGABYTE Then
    Result := Format(strKileByteFmt, [Int(iSize) / dblKILOBYTE])
  Else
    Result := Format(strMegaByteFmt, [Int(iSize) / dblMEGABYTE])
End;

(**

  This procedure registers the component with the BDS IDE.

  @precon  None.
  @postcon Registers the component with the BDS IDE.

**)
Procedure Register;

ResourceString
  strDGHControls = 'DGH Controls';

Begin
  RegisterComponents(strDGHControls, [TDGHMemoryMonitor]);
End;

{ TDGHBatteryMonitor }

(**

  This is the constructor method for the TDGHBatteryMonitor class.

  @precon  None.
  @postcon Creates and initialise the component including the timer.

  @nocheck MissingCONSTInParam

  @param   AOwner as a TComponent

**)
Constructor TDGHMemoryMonitor.Create(AOwner: TComponent);

Begin
  Inherited Create(AOwner);
  FUpdateInterval := 5000;
  FBackColour     := clGray;
  FBackFontColour := clBlack;
  FHighColour     := clRed;
  FHalfColour     := $00CCFF;
  FLowColour      := clLime;
  FHighFontColour := clBlack;
  FHalfFontColour := clBlack;
  FLowFontColour  := clBlack;
  FLowPoint       := 10;
  FHalfPoint      := 50;
  FHighPoint      := 90;
  Width           := 125;
  Height          := 22;
  FTimer          := TTimer.Create(Nil);
  FTimer.Interval := FUpdateInterval;
  FTimer.OnTimer  := UpdateControl;
  FTimer.Enabled  := True;
  UpdateControl(Nil);
End;

(**

  This is the destructor method for the TDGHBatteryMonitor class.

  @precon  None.
  @postcon Disabled the timer and free's its mmemory.

**)
Destructor TDGHMemoryMonitor.Destroy;

Begin
  FTimer.Enabled := False;
  FTimer.Free;
  Inherited Destroy;
End;

(**

  This method sets the interval variables with information about the systems
  power status.

  @precon  None.
  @postcon FBatteryStatus, FPercentage and FRemainingTime are set with the
           battery status.

**)
Procedure TDGHMemoryMonitor.GetFFMemoryManagerState;

Var
  MMS : TMemoryManagerState;
  i   : Integer;
  SBTS: TSmallBlockTypeState;

Begin
  GetMemoryManagerState(MMS);
  FUsed     := MMS.TotalAllocatedLargeBlockSize + MMS.TotalAllocatedMediumBlockSize;
  FMemBlocks[dmtLarge]  := MMS.AllocatedLargeBlockCount;
  FMemBlocks[dmtMedium] := MMS.AllocatedMediumBlockCount;
  FMemBlocks[dmtSmall]  := 0;
  FReserved := MMS.ReservedLargeBlockAddressSpace + MMS.ReservedMediumBlockAddressSpace;
  For i     := Low(MMS.SmallBlockTypeStates) To High(MMS.SmallBlockTypeStates) Do
    Begin
      SBTS := MMS.SmallBlockTypeStates[i];
      Inc(FMemBlocks[dmtSmall], SBTS.AllocatedBlockCount);
      Inc(FUsed, SBTS.UseableBlockSize * SBTS.AllocatedBlockCount);
      Inc(FReserved, SBTS.ReservedAddressSpace);
    End;
  FPercentage := 0;
  If FReserved > 0 Then
    FPercentage := Trunc(Int(FUsed) / Int(FReserved) * 100);
End;

(**

  This is the controls overridden paint method.

  @precon  None.
  @postcon Draw the display of battery information.

**)
Procedure TDGHMemoryMonitor.Paint;

Const
  iMargin: Integer = 8;
  strLongOutput = 'Used %s in %s bytes (%1.1n%%) [Blocks: %dL, %dM, %dS]';
  strMediumOutput = 'Used %s in %s bytes (%1.1n%%)';
  strShortOutput = '%s in %s (%1.1n%%)';
  strVeryShortOutput = '%s (%1.1n%%)';

Var
  strStatus : String;
  R, T      : TRect;
  strU, strR: String;

Begin
  Inherited Paint;
  Canvas.Font.Assign(Font);
  R := ClientRect;
    // Render Background
  Canvas.Brush.Color := FBackColour;
  Canvas.FillRect(R);
  Canvas.Font.Color := FBackFontColour;
    // Write Background text
  strU      := CalcSize(FUsed);
  strR      := CalcSize(FReserved);
  strStatus := Format(strLongOutput,
    [strU, strR, Int(FPercentage), FMemBlocks[dmtLarge], FMemBlocks[dmtMedium],
    FMemBlocks[dmtSmall]]);
  If Canvas.TextWidth(strStatus) > R.Right - R.Left - iMargin Then
    strStatus := Format(strMediumOutput, [strU, strR, Int(FPercentage)]);
  If Canvas.TextWidth(strStatus) > R.Right - R.Left - iMargin Then
    strStatus := Format(strShortOutput, [strU, strR, Int(FPercentage)]);
  If Canvas.TextWidth(strStatus) > R.Right - R.Left - iMargin Then
    strStatus := Format(strVeryShortOutput, [strU, Int(FPercentage)]);
  T.Left      := R.Left + ((R.Right - R.Left) - Canvas.TextWidth(strStatus)) Div 2;
  T.Right     := T.Left + Canvas.TextWidth(strStatus);
  T.Top       := R.Top;
  T.Bottom    := R.Bottom;
  DrawText(Canvas.Handle, PChar(strStatus), Length(strStatus), T, DT_SINGLELINE Or
    DT_VCENTER);
  // Render % of memory High in custom colour
  If FPercentage In [0 .. 100] Then
    Begin
      Canvas.Brush.Color := CalcColour(FPercentage, FLowPoint, FHalfPoint, FHighPoint,
        FLowColour, FHalfColour, FHighColour);
      R.Right := R.Left + (R.Right - R.Left) * FPercentage Div 100;
      Canvas.FillRect(R);
    End
  Else
    Begin
      Canvas.Brush.Color := FLowColour;
      Canvas.FillRect(R);
    End;
  // Render % of memory text.
  T.Right           := R.Right;
  Canvas.Font.Color := CalcColour(FPercentage, FLowPoint, FHalfPoint, FHighPoint,
    FLowFontColour, FHalfFontColour, FHighFontColour);
  If T.Right >= T.Left Then
    DrawText(Canvas.Handle, PChar(strStatus), Length(strStatus), T, DT_SINGLELINE Or
        DT_VCENTER);
End;

(**

  This is a setter method for the HalfPoint property.

  @precon  None.
  @postcon Sets the HalfPoint Property

  @param   Value as a TPercentage as a constant

**)
Procedure TDGHMemoryMonitor.SetHalfPoint(Const Value: TPercentage);

Begin
  //If (Value > FLowPoint) And (Value < FHighPoint) Then
    FHalfPoint := Value;
End;

(**

  This is a setter method for the HighPoint property.

  @precon  None
  @postcon Sets the HighPoint property.

  @param   Value as a TPercentage as a constant

**)
Procedure TDGHMemoryMonitor.SetHighPoint(Const Value: TPercentage);

Begin
  //If (Value > FHalfPoint) And (Value <= 100) Then
    FHighPoint := Value;
End;

(**

  This is a setter method for the LowPoint property.

  @precon  None.
  @postcon Sets the LowPoitn property.

  @param   Value as a TPercentage as a constant

**)
Procedure TDGHMemoryMonitor.SetLowPoint(Const Value: TPercentage);

Begin
  //If (Value < FHalfPoint) And (Value > 0) Then
    FLowPoint := Value;
End;

(**

  This is a setter method for the UpdateInterval property.

  @precon  None.
  @postcon Set the update interval of the componenot and the timer control.

  @param   Value as an Integer as a constant

**)
Procedure TDGHMemoryMonitor.SetUpdateInterval(Const Value: Integer);

Begin
  FUpdateInterval := Value;
  FTimer.Interval := Value;
End;

(**

  This method is an on timer event handler for the timer control.

  @precon  None.
  @postcon Gets the battery status and then forces the control to re-paint.

  @param   Sender as a TObject

**)
Procedure TDGHMemoryMonitor.UpdateControl(Sender: TObject);

Begin
  GetFFMemoryManagerState;
  If CanInvalidate Then
    Invalidate;
End;

End.
