Unit DGHSpectrum;

Interface

Uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs;

Const
  NanoMetresMinimum = 380;
  NanoMetresMaximum = 780;

Type
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
    Property Align;
    Property Font;
    Property Hint;
    Property ShowHint;
    Property OnMouseDown;
    Property OnMouseUp;
    Property OnClick;
    Property OnDblClick;
    Property OnMouseMove;
    Property Brightness : Integer Read fBrightness Write SetBrightness Default 100;
    Property WaveLengthMin : Integer Read fWaveMin Write SetWaveMin Default NanoMetresMinimum;
    Property WaveLengthMax : Integer Read fWaveMax Write SetWaveMax Default NanoMetresMaximum;
    Property Reverse : Boolean Read fReverse Write SetReverse Default False;
  End;

Procedure Register;

Implementation

{$R *.DCR}

Uses
  Math;

Function NanoMetresToRGB(NanoMetres : Integer; Brightness : Double) : ColorRef;

Var
  Red, Green, Blue, Factor: Double;

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

Destructor TDGHSpectrum.Destroy;

Begin
  fBitmap.Free;
  Inherited Destroy;
End;

Procedure TDGHSpectrum.InitBitmap;

Type
  pRGBArray = ^ TRGBArray;
  TRGBArray = Array [0..32767] Of TRGBTriple;

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

Procedure TDGHSpectrum.Paint;

Begin
  If (fBitmap.Width <> Width) Or (fBitmap.Height <> Height) Then
    InitBitMap;
  canvas.Draw (0, 0, fBitmap);
End;

Procedure TDGHSpectrum.SetReverse(Value : Boolean);

Begin
  If fReverse <> Value Then
    Begin
      fReverse := Value;
      InitBitMap;
      Refresh;
    End;
End;

Procedure TDGHSpectrum.SetBrightness(Value : Integer);

Begin
  If (fBrightness <> Value) And (Value In [0..100]) Then
    Begin
      fBrightness := Value;
      InitBitMap;
      Refresh;
    End;
End;

Procedure TDGHSpectrum.SetWaveMin(Value : Integer);

Begin
  If (fWaveMin <> Value) And (Value >= NanoMetresMinimum) And (Value < fWaveMax) Then
    Begin
      fWaveMin := Value;
      InitBitMap;
      Refresh;
    End;
End;

Procedure TDGHSpectrum.SetWaveMax(Value : Integer);

Begin
  If (fWaveMax <> Value) And (Value <= NanoMetresMaximum) And (Value > fWaveMin) Then
    Begin
      fWaveMax := Value;
      InitBitMap;
      Refresh;
    End;
End;

Procedure Register;

Begin
  RegisterComponents('DGH Controls', [TDGHSpectrum]);
End;

End.
