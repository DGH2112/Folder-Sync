(**

  This module contains 2 controls, the first a modified edit control with
  the ability to tab forward to the next control if the <Enter> key is pressed
  and allows the alignment of the text and a second control which inherited from
  the first and allows the evaluation of numeric expressions.

  @Version 1.0
  @Author  David Hoyle
  @Date    06 Oct 2007

**)
unit DGHNumericEdit;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls;

type
  (** A new edit control with Alignment and EnterAsTab properties. **)
  TDGHEdit = class(TEdit)
  Private
    FAlignment : TAlignment;
    FEnterAsTab : Boolean;
  Protected
    Procedure SetAlignment(Value : TAlignment);
    Procedure KeyPress(var Key: Char); Override;
  Public
    Procedure CreateParams(var Params: TCreateParams); Override;
  Published
    (**
      This property sets the alignment of the text.
      @precon  None.
      @postcon Sets the alignment of the text.
      @return  a TAlignment
    **)
    Property Alignment : TAlignment Read FAlignment Write SetAlignment;
    (**
      This property sets whether the Enter key behaves as a Tab key.
      @precon  None.
      @postcon Sets whether the Enter key behaves as a Tab key.
      @return  a Boolean
    **)
    Property EnterAsTab : Boolean Read FEnterAsTab Write FEnterAsTab Default False;
  end;

  (** A new edit control with evaluation of numeric expressions. **)
  TNumericEdit = class(TDGHEdit)
  private
    { Private declarations }
    FDecimalPlaces : Integer;
    FValue : Double;
    procedure SetDecimalPlaces(const Value: Integer);
    function GetValue: Double;
    procedure SetValue(const Value: Double);
  protected
    { Protected declarations }
    Procedure DoExit; Override;
  public
    { Public declarations }
    Constructor Create(AOwner : TComponent); Override;
    Procedure EvaluateInput; Virtual;
    (**
      This property gets and sets the value of the expression.
      @precon  None.
      @postcon Gets and sets the value of the expression.
      @return  a Double
    **)
    Property Value : Double Read GetValue Write SetValue;
  published
    { Published declarations }
    (**
      This property determines the number of decimal places of the expression
      result.
      @precon  None.
      @postcon Determines the number of decimal places of the expression
               result.
      @return  an Integer
    **)
    Property DecimalPlaces : Integer read FDecimalPlaces
      write SetDecimalPlaces Default 3;
  end;

procedure Register;

implementation

Uses
  DGHLibrary;

(**

  The standard Delphi register procedure..

  @precon  None
  @postcon Registers the component with the Delphi Palette.

**)
procedure Register;
begin
  RegisterComponents('DGH Controls', [TNumericEdit]);
end;

(**

  This is an overridden CreateParams method to allow the edit control's text
  to be aligned.

  @precon  None.
  @postcon Creates the control so that the style contains the alignment
           information.

  @param   Params as a TCreateParams as a reference

**)
Procedure TDGHEdit.CreateParams(var Params: TCreateParams);

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

(**

  This is an overridden method to allow the enter key to be used as a Tab key to
  move the focus to the next control on the form.

  @precon  None.
  @postcon If the property EnterAsTab is true then a tab keybaord event is
           generated.

  @param   Key as a Char as a reference

**)
procedure TDGHEdit.KeyPress(var Key : Char);
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

(**

  This is a setter method for the Alignment property.

  @precon  None.
  @postcon Sets the alignment of the edit controls text.

  @param   Value as a TAlignment

**)
Procedure TDGHEdit.SetAlignment(Value : TAlignment);

Begin
  If Value <> FAlignment Then
    Begin
      FAlignment := Value;
      ReCreateWnd;
    End;
End;

(**

  This is the constructor method for the TNumericEdit class.

  @precon  None;
  @postcon Sets the default values of the component.

  @param   AOwner as a TComponent

**)
Constructor TNumericEdit.Create(AOwner: TComponent);
begin
  Inherited Create(AOwner);
  FDecimalPlaces := 3;
  FValue := 0;
end;

(**

  This is an overidden DoExit method to evaluate the numeric expression is an
  on exit event handler is NOT assigned.

  @precon  None.
  @postcon Overidden DoExit method to evaluate the numeric expression is an
           on exit event handler is NOT assigned.

**)
procedure TNumericEdit.DoExit;
begin
  inherited;
  If Not Assigned(OnExit) Then
    EvaluateInput;
end;

(**

  This method attempts to evaluate the controls expression, if an exception
  occurs it is handled and the focus remains in the control.

  @precon  None.
  @postcon Attempts to evaluate the controls expression, if an exception
           occurs it is handled and the focus remains in the control.

**)
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

(**

  This is a getter method for the Value property.

  @precon  None.
  @postcon Returns the value of the Value property.

  @return  a Double

**)
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

(**

  This is a setter method for the DecimalPlaces property.

  @precon  None.
  @postcon Sets the decimal places of the result.

  @param   Value as an Integer constant

**)
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

(**

  This is a setter method for the Value property.

  @precon  None.
  @postcon Formats the Value using the FValue variable and the number of
           decimal places.

  @param   Value as a Double constant

**)
procedure TNumericEdit.SetValue(const Value: Double);
begin
  FValue := Value;
  Text := Format('%*.*f', [1, FDecimalPlaces, Value]);
end;

end.
