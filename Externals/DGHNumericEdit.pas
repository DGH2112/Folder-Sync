unit DGHNumericEdit;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls;

{ -------------------------------------------------------------------------

   This module defines a descendant of TEdit that allows for alignment
   justment and the evauation of numerical inputs but does not evaluate the
   input if there is an OnExit event handler assigned.

   New Properties

     Align         : TAlignment
     DecimalPlaces : Integer
     Value         : Double

  -------------------------------------------------------------------------- }

type
  TNumericEdit = class(TEdit)
  private
    { Private declarations }
    FAlignment : TAlignment;
    FDecimalPlaces : Integer;
    FValue : Double;
    FEnterAsTab : Boolean;
    procedure SetDecimalPlaces(const Value: Integer);
    function GetValue: Double;
    procedure SetValue(const Value: Double);
  protected
    { Protected declarations }
    Procedure SetAlignment(Value : TAlignment);
    Procedure DoExit; Override;
    Procedure KeyPress(var Key: Char); Override;
  public
    { Public declarations }
    Constructor Create(AOwner : TComponent); Override;
    Procedure CreateParams(var Params: TCreateParams); Override;
    Procedure EvaluateInput; Virtual;
    Property Value : Double Read GetValue Write SetValue;
  published
    { Published declarations }
    Property Alignment : TAlignment Read FAlignment Write SetAlignment;
    Property DecimalPlaces : Integer read FDecimalPlaces
      write SetDecimalPlaces Default 3;
    Property EnterAsTab : Boolean Read FEnterAsTab Write FEnterAsTab Default False;
  end;

procedure Register;

implementation

Uses
  DGHLibrary;

{ -------------------------------------------------------------------------

   Standard component Register procedure.

  -------------------------------------------------------------------------- }

procedure Register;
begin
  RegisterComponents('DGH Controls', [TNumericEdit]);
end;

{ -------------------------------------------------------------------------

   This is an overridden constructor for the component setting the number
   of decimal places to 3 as default.

   Create(
     AOwner   // Owner Component as TComponent
   );

  -------------------------------------------------------------------------- }

Constructor TNumericEdit.Create(AOwner: TComponent);
begin
  Inherited Create(AOwner);
  FDecimalPlaces := 3;
  FValue := 0;
end;

{ -------------------------------------------------------------------------

   This is an overridden CreateParams procedure to allow for the edit
   control to be aligned left, centre or right.

   CreateParams(
     Params    // TCreateParams structure to update
   );

  -------------------------------------------------------------------------- }

Procedure TNumericEdit.CreateParams(var Params: TCreateParams);

Var
  X : DWORD;

Begin
  Inherited CreateParams(Params);
  Case FAlignment Of
    taRightJustify: X := ES_RIGHT;
    taLeftJustify: X := ES_LEFT;
    taCenter: X := ES_CENTER;
  Else
    X := ES_LEFT;
  End;
  Params.Style := Params.Style Or X;
End;

{ -------------------------------------------------------------------------

   Overridden DoExit procedure that evaluate the input of the edit control
   on the focus leaving. This does not evaluate the input if there is an
   OnExit event handler assigned.

   DoExit(
   );

  -------------------------------------------------------------------------- }

procedure TNumericEdit.DoExit;
begin
  inherited;
  If Not Assigned(OnExit) Then
    EvaluateInput;
end;

{ -------------------------------------------------------------------------

   This routine evalaute the text and returns the result.

   EvaluateInput(
   );

  -------------------------------------------------------------------------- }

procedure TNumericEdit.EvaluateInput;
begin
  Try
    Value := EvaluateEquation(Text);
  Except
    On E: Exception Do
      Begin
        Self.SetFocus;
        Raise Exception.Create(E.Message);
      End;
  End;
end;

{ -------------------------------------------------------------------------

   Getter method for the Value property.

   GetValue(
   );    // Returns the edit controls value as a double.

  -------------------------------------------------------------------------- }

function TNumericEdit.GetValue: Double;
begin
  (*
    Check the text to be a number, if not then evaluate it - ensures that
    FValue is always correct.
  *)
  Try
    FValue := StrToFloat(Text)
  Except
    Try
      FValue := EvaluateEquation(Text);
    Except
      On E: Exception Do
        Begin
          Self.SetFocus;
          Raise Exception.Create(E.Message);
        End;
    End;
  End;
  Result := FValue;
end;

procedure TNumericEdit.KeyPress(var Key : Char);
begin
  If FEnterAsTab Then
    If Key = #13 Then
      Begin
        Key := #0;
        keybd_event(VK_TAB, MapVirtualKey(VK_TAB, 0), 0, 0);
        keybd_event(VK_TAB, MapVirtualKey(VK_TAB, 0), KEYEVENTF_KEYUP, 0);
      End;
  inherited;
end;

{ -------------------------------------------------------------------------

   Setter method for the controls Align property.

   SetAlignment(
     Value    // A TAlignment value
   );

  -------------------------------------------------------------------------- }

Procedure TNumericEdit.SetAlignment(Value : TAlignment);

Begin
  If Value <> FAlignment Then
    Begin
      FAlignment := Value;
      ReCreateWnd;
    End;
End;

{ -------------------------------------------------------------------------

   Setter method for the DecimalPlaces property.

   SetDecimals(
     Value   // Number of decimal places as an integer between 0..9
   );

  -------------------------------------------------------------------------- }

procedure TNumericEdit.SetDecimalPlaces(const Value: Integer);
begin
  If Value <> FDecimalPLaces Then
    Case Value Of
      0..9: FDecimalPlaces := Value;
    Else
      Raise Exception.Create('Then number of decimal places needs to ' +
        'be in the range 0 to 9.');
    End;
end;

{ -------------------------------------------------------------------------

   Setter method for the Value property.

   SetValue(
     Value   // Value as a double
   );

  -------------------------------------------------------------------------- }

procedure TNumericEdit.SetValue(const Value: Double);
begin
  FValue := Value;
  Text := Format('%*.*f', [1, FDecimalPlaces, Value]);
end;

end.
