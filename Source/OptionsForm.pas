unit OptionsForm;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Buttons, ComCtrls;

type
  TfrmOptions = class(TForm)
    edtExclusions: TMemo;
    lblTimeDiff: TLabel;
    Label1: TLabel;
    edtTimeDiff: TEdit;
    udTolerance: TUpDown;
    btnOK: TBitBtn;
    btnCancel: TBitBtn;
  private
    { Private declarations }
  public
    { Public declarations }
    Class Function Execute(var strExclusions :String;
      var iTolerance : Integer) : Boolean;
  end;

implementation

{$R *.DFM}

{ TfrmOptions }

class function TfrmOptions.Execute(var strExclusions: String;
  var iTolerance: Integer): Boolean;
begin
  Result := False;
  With TfrmOptions.Create(Nil) Do
    Try
      edtExclusions.Text := strExclusions;
      udTolerance.Position := iTolerance;
      If ShowModal = mrOK Then
        Begin
          strExclusions := edtExclusions.Text;
          iTolerance := udTolerance.Position;
          Result := True;
        End;
    Finally
      Free;
    End;
end;

end.
