(**

  This module contains a class that defines a conponent which draws a spectrum
  of colours across the component.

  @Author  Dave Jewel
  @Version 1.0
  @Date    21 Jun 2009

  @Note    The source for this was published in I think The Delphi Magazine
           quite a long time ago, unfortundately I can't find the reference
           anymore.

**)
Unit DGHSpectrum;

Interface

Uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs;

Const
  (** A constant to describe the minimum Nano Metre Value. *)
  NanoMetresMinimum = 380;
  (** A constant to describe the maximum Nano Metre Value. *)
  NanoMetresMaximum = 780;

Type
  (** A class to represent a control for drawing spectrums. **)
  TDGHSpectrum = Class(TGraphicControl)
  Private
    { Private declarations }
    fBitmap : TBitmap;
    fReverse : Boolean;
    fWaveMin : Integer;
    fWaveMax : Integer;
    fBrightness : Integer;
    Procedure SetReverse(Value : Boolean);
    Procedure SetWaveMin(Value : Integer);
    Procedure SetWaveMax(Value : Integer);
    Procedure SetBrightness(Value : Integer);
    Procedure InitBitmap;
  Protected
    { Protected declarations }
  Public
    { Public declarations }
    Constructor Create(AOwner : TComponent); Override;
    Destructor Destroy; Override;
    Procedure Paint; Override;
  Published
    { Published declarations }
    (**
      This property just publishes the hidden Align property of its parent.
      @precon  None.
      @postcon None.
    **)
    Property Align;
    (**
      This property just publishes the hidden Font property of its parent.
      @precon  None.
      @postcon None.
    **)
    Property Font;
    (**
      This property just publishes the hidden Hint property of its parent.
      @precon  None.
      @postcon None.
    **)
    Property Hint;
    (**
      This property just publishes the hidden ShowHint property of its parent.
      @precon  None.
      @postcon None.
    **)
    Property ShowHint;
    (**
      This property just publishes the hidden OnMouseDown property of its parent.
      @precon  None.
      @postcon None.
    **)
    Property OnMouseDown;
    (**
      This property just publishes the hidden OnMouseUp property of its parent.
      @precon  None.
      @postcon None.
    **)
    Property OnMouseUp;
    (**
      This property just publishes the hidden OnClick property of its parent.
      @precon  None.
      @postcon None.
    **)
    Property OnClick;
    (**
      This property just publishes the hidden OnDblClick property of its parent.
      @precon  None.
      @postcon None.
    **)
    Property OnDblClick;
    (**
      This property just publishes the hidden OnMouseMove property of its parent.
      @precon  None.
      @postcon None.
    **)
    Property OnMouseMove;
    (**
      This property sets and gets the brightness of the spectrum.
      @precon  None.
      @postcon Returns the brightness of the spectrum rendered.
      @return  an Integer
    **)
    Property Brightness : Integer Read fBrightness Write SetBrightness Default 100;
    (**
      This property gets and sets the minimum waves length rendered in the
      control.
      @precon  None.
      @postcon Returns the minimum waves length rendered in the control.
      @return  an Integer
    **)
    Property WaveLengthMin : Integer Read fWaveMin Write SetWaveMin Default NanoMetresMinimum;
    (**
      This property gets and sets the minimum waves length rendered in the
      control.
      @precon  None.
      @postcon Returns the maximum waves length rendered in the control.
      @return  an Integer
    **)
    Property WaveLengthMax : Integer Read fWaveMax Write SetWaveMax Default NanoMetresMaximum;
    (**
      This method gets and sets whether the spectrum is rendered left to right
      or right to left.
      @precon  None.
      @postcon Return whether the spectrum is rendered left to right or right to
               left.
      @return  a Boolean
    **)
    Property Reverse : Boolean Read fReverse Write SetReverse Default False;
  End;

Procedure Register;

Implementation

{$R *.DCR}

Uses
  Math;

(**

  This method converts the nanometres and brightness to an RGB colours.

  @precon  None.
  @postcon Returns a the nanometres and brightness converted to an RGB colour.

  @param   NanoMetres as an Integer
  @param   Brightness as a Double
  @return  a ColorRef

**)
Function NanoMetresToRGB(NanoMetres : Integer; Brightness : Double) : ColorRef;

Var
  Red, Green, Blue, Factor: Double;

  (**

    This function adjusts the given colour by the given factor.

    @precon  None.
    @postcon Returns an adjusted given colour by a given factor.

    @param   Color  as a Double
    @param   Factor as a Double
    @return  an Integer

  **)
  Function Adjust(Color, Factor : Double) : Integer;

  Const
    Gamma = 0.80;

  Begin
    If Color = 0.0 Then
      Result := 0
    Else
      Result := Round(255 * Power(Color * Factor * Brightness, Gamma));
  End;

Begin
  Red := 0.0; Green := 0.0; Blue := 0.0;
  If Brightness > 1.0 Then
    Brightness := 1.0;
  If Brightness < 0.0 Then
    Brightness := 0.0;
  // Calculate relative RGB values
  Case Trunc (NanoMetres) Of
    380..439 :
      Begin
        // Ultra Violet to Blue
        Red := -(NanoMetres - 440) / (440 - 380);
        Blue := 1.0;
      End;
    440..489:
      Begin
        // Blue to Cyan
        Green := (NanoMetres - 440) / (490 - 440);
        Blue := 1.0;
      End;
    490..509:
      Begin
        // Cyan to Green
        Green := 1.0;
        Blue := -(NanoMetres - 510) / (510 - 490);
      End;
    510..579:
      Begin
        //Green to Yellow-Orange
        Red := (NanoMetres - 510) / (580 - 510);
        Green := 1.0;
      End;
    580..644:
      Begin
        // Yellow-Orange to Full Red
        Red := 1.0;
        Green := -(NanoMetres - 645) / (645 - 580);
      End;
    645..780:
      Begin
        // Full Red to hereon...
        Red := 1.0;
        Green := 0.0;
      End;
  End;
  // Condition RGB according to limits of vision
  Case Trunc(NanoMetres) Of
    380..419: Factor := 0.3 + 0.7 * (NanoMetres - 380) / (420 - 380);
    420..700: Factor := 1.0;
    701..780: Factor := 0.3 + 0.7 * (780 - NanoMetres) / (780 - 700);
    Else Factor := 0.0;
  End;
  Result := RGB(Adjust(Red, Factor), Adjust(Green, Factor), Adjust(Blue, Factor));
End;

{ TDGHSpectrum }

(**

  This is a constructor for the TDGHSpectrum class.

  @precon  None.
  @postcon Initialises the component.

  @param   AOwner as a TComponent

**)
Constructor TDGHSpectrum.Create(AOwner : TComponent);

Begin
  Inherited Create(AOwner);
  width := 200; Height := 30;
  fBrightness := 100;
  fWaveMin := NanoMetresMinimum;
  fWaveMax := NanoMetresMaximum;
  fBitmap := TBitmap.Create;
  fBitmap.PixelFormat := pf24bit;
End;

(**

  This is a destructor for the TDGHSpectrum class.

  @precon  None.
  @postcon Fress memory used by the class.

**)
Destructor TDGHSpectrum.Destroy;

Begin
  fBitmap.Free;
  Inherited Destroy;
End;

(**

  This method initialises the internal bitmap used for rendering the colours.

  @precon  None.
  @postcon Initialises the internal bitmap used for rendering the colours.

**)
Procedure TDGHSpectrum.InitBitmap;

Type
  TRGBArray = Array [0..32767] Of TRGBTriple;
  pRGBArray = ^ TRGBArray;

Var
  Row : pRGBArray;
  CurColor : TColor;
  WaveLength : Double;
  X, Y, Index : Integer;

Begin
  fBitmap.Width := Width;
  fBitmap.Height := Height;
  For X := 0 To Width - 1 Do
    Begin
      If fReverse Then
        Index := Width - 1 - X
      Else
        Index := X;
      WaveLength := fWaveMin + (fwaveMax - fWaveMin) * X / Width;
      CurColor := NanoMetresToRGB(Trunc(WaveLength), fBrightness / 100);
        For y := 0 To Height - 1 Do
          Begin
            Row := fBitmap.Scanline[Y];
            With Row[Index] Do
              Begin
                rgbtRed := GetRValue(CurColor);
                rgbtGreen := GetGValue(CurColor);
                rgbtBlue := GetBValue(CurColor);
              End;
          End;
    End;
End;

(**

  This method paints the spectrum image on the control canvas.

  @precon  None.
  @postcon Paints the spectrum image on the control canvas.

**)
Procedure TDGHSpectrum.Paint;

Begin
  If (fBitmap.Width <> Width) Or (fBitmap.Height <> Height) Then
    InitBitMap;
  canvas.Draw (0, 0, fBitmap);
End;

(**

  This is a setter method for the Reverse property.

  @precon  None.
  @postcon Sets the Reverse property.

  @param   Value as a Boolean

**)
Procedure TDGHSpectrum.SetReverse(Value : Boolean);

Begin
  If fReverse <> Value Then
    Begin
      fReverse := Value;
      InitBitMap;
      Refresh;
    End;
End;

(**

  This is a setter method for the Brightness property.

  @precon  None.
  @postcon Sets the brightness property.

  @param   Value as an Integer

**)
Procedure TDGHSpectrum.SetBrightness(Value : Integer);

Begin
  If (fBrightness <> Value) And (Value In [0..100]) Then
    Begin
      fBrightness := Value;
      InitBitMap;
      Refresh;
    End;
End;

(**

  This is a setter method for the WaveMin property.

  @precon  None.
  @postcon Sets the WaveMin property.

  @param   Value as an Integer

**)
Procedure TDGHSpectrum.SetWaveMin(Value : Integer);

Begin
  If (fWaveMin <> Value) And (Value >= NanoMetresMinimum) And (Value < fWaveMax) Then
    Begin
      fWaveMin := Value;
      InitBitMap;
      Refresh;
    End;
End;

(**

  This is a setter method for the WaveMax property.

  @precon  None.
  @postcon Sets the WaveMax property.

  @param   Value as an Integer

**)
Procedure TDGHSpectrum.SetWaveMax(Value : Integer);

Begin
  If (fWaveMax <> Value) And (Value <= NanoMetresMaximum) And (Value > fWaveMin) Then
    Begin
      fWaveMax := Value;
      InitBitMap;
      Refresh;
    End;
End;

(**

  This procedure registers the component in the Delphi component palette.

  @precon  None.
  @postcon Registers the component in the Delphi component palette.

**)
Procedure Register;

Begin
  RegisterComponents('DGH Controls', [TDGHSpectrum]);
End;

End.
