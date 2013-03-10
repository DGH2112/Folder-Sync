(**
  
  This module contains a class which represents an options for for configuring
  the memory monitor component.

  @Version 1.0
  @Date    10 Mar 2013
  @Author  David Hoyle

**)
unit MemoryMonitorOptionsForm;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, DGHNumericEdit, ComCtrls, Buttons, ExtCtrls,
  DGHMemoryMonitorControl;

type
  (** A class to represent the form interface. **)
  TfrmMemoryMonitorOptions = class(TForm)
    lblUpdateInterval: TLabel;
    udUpdateInterval: TUpDown;
    edtUpdateInterval: TDGHEdit;
    cbxBackColour: TColorBox;
    cbxBackFontColour: TColorBox;
    cbxHighColour: TColorBox;
    cbxHighFontColour: TColorBox;
    cbxMidPointColour: TColorBox;
    cbxMidPointFontColour: TColorBox;
    cbxLowColour: TColorBox;
    cbxLowFontColour: TColorBox;
    btnOK: TBitBtn;
    btnCancel: TBitBtn;
    edtHighPoint: TDGHEdit;
    udHighPoint: TUpDown;
    edtMidPointPoint: TDGHEdit;
    udMidPointPoint: TUpDown;
    edtLowPoint: TDGHEdit;
    udLowPoint: TUpDown;
    lblBackColour: TLabel;
    lblBackFontColour: TLabel;
    lblHighColour: TLabel;
    lblHighFontColour: TLabel;
    lblHighPoint: TLabel;
    lblMidPointColour: TLabel;
    lblMidPointFontColour: TLabel;
    lblMidPointPoint: TLabel;
    lblLowColour: TLabel;
    lblLowFontColour: TLabel;
    lblLowPoint: TLabel;
    cbxFontName: TComboBox;
    lblFontName: TLabel;
    lblFontSize: TLabel;
    edtFontSize: TDGHEdit;
    udFontSize: TUpDown;
    grpFontStyles: TGroupBox;
    cbxBold: TCheckBox;
    cbxItalic: TCheckBox;
    cbxUnderline: TCheckBox;
    cbxStrikeout: TCheckBox;
    procedure FormCreate(Sender: TObject);
    procedure udHighPointChangingEx(Sender: TObject; var AllowChange: Boolean;
      NewValue: Smallint; Direction: TUpDownDirection);
    procedure udMidPointPointChangingEx(Sender: TObject; var AllowChange: Boolean;
      NewValue: Smallint; Direction: TUpDownDirection);
    procedure udLowPointChangingEx(Sender: TObject;
      var AllowChange: Boolean; NewValue: Smallint;
      Direction: TUpDownDirection);
    procedure edtUpdateIntervalKeyPress(Sender: TObject; var Key: Char);
  private
    { Private declarations }
  public
    { Public declarations }
    Class Function Execute(MemMon : TDGHMemoryMonitor) : Boolean;
  end;

implementation

{$R *.dfm}

{ TfrmMemoryMonitorOptions }

(**

  This is a KeyPress event handler for the Update Interval control.

  @precon  None.
  @postcon Only allows numeric values to be entered.

  @param   Sender as a TObject
  @param   Key    as a Char as a reference

**)
procedure TfrmMemoryMonitorOptions.edtUpdateIntervalKeyPress(Sender: TObject;
  var Key: Char);

begin
  If Not CharInSet(Key, ['0'..'9']) Then
    Key := #0;
end;

(**

  This is the classes / forms main interface method which display the form
  inorder to configure the component.

  @precon  MemMon must be a valid intance of a TBatteryMonitor component.
  @postcon If OK is pressed the properties of the component are updated.

  @param   MemMon as a TDGHMemoryMonitor
  @return  a Boolean

**)
class Function TfrmMemoryMonitorOptions.Execute(MemMon : TDGHMemoryMonitor) : Boolean;

Var
  FS : TFontStyles;

begin
  Result := False;
  With TfrmMemoryMonitorOptions.Create(Nil) Do
    Try
      udUpdateInterval.Position := MemMon.UpdateInterval;
      cbxBackColour.Selected := MemMon.BackColour;
      cbxBackFontColour.Selected := MemMon.BackFontColour;
      cbxHighColour.Selected := MemMon.HighColour;
      cbxHighFontColour.Selected := MemMon.HighFontColour;
      udHighPoint.Position := MemMon.HighPoint;
      cbxMidPointColour.Selected := MemMon.HalfColour;
      cbxMidPointFontColour.Selected := MemMon.HalfFontColour;
      udMidPointPoint.Position := MemMon.HalfPoint;
      cbxLowColour.Selected := MemMon.LowColour;
      cbxLowFontColour.Selected := MemMon.LowFontColour;
      udLowPoint.Position := MemMon.LowPoint;
      cbxFontName.Text := MemMon.Font.Name;
      udFontSize.Position := MemMon.Font.Size;
      cbxBold.Checked := fsBold In MemMon.Font.Style;
      cbxItalic.Checked := fsItalic In MemMon.Font.Style;
      cbxUnderline.Checked := fsUnderline In MemMon.Font.Style;
      cbxStrikeout.Checked := fsStrikeOut In MemMon.Font.Style;
      udHighPoint.OnChangingEx := udHighPointChangingEx;
      udMidPointPoint.OnChangingEx := udMidPointPointChangingEx;
      udLowPoint.OnChangingEx := udLowPointChangingEx;
      If ShowModal = mrOK Then
        Begin
          MemMon.BeginUpdate;
          Try
            MemMon.UpdateInterval := udUpdateInterval.Position;
            MemMon.BackColour := cbxBackColour.Selected;
            MemMon.BackFontColour := cbxBackFontColour.Selected;
            MemMon.HighColour := cbxHighColour.Selected;
            MemMon.HighFontColour := cbxHighFontColour.Selected;
            MemMon.HighPoint := udHighPoint.Position;
            MemMon.HalfColour := cbxMidPointColour.Selected;
            MemMon.HalfFontColour := cbxMidPointFontColour.Selected;
            MemMon.HalfPoint := udMidPointPoint.Position;
            MemMon.LowColour := cbxLowColour.Selected;
            MemMon.LowFontColour := cbxLowFontColour.Selected;
            MemMon.LowPoint := udLowPoint.Position;
            MemMon.Font.Name := cbxFontName.Text;
            MemMon.Font.Size := udFontSize.Position;
            FS := [];
            If cbxBold.Checked Then
              Include(FS, fsBold);
            If cbxItalic.Checked Then
              Include(FS, fsItalic);
            If cbxUnderline.Checked Then
              Include(FS, fsUnderline);
            If cbxStrikeout.Checked Then
              Include(FS, fsStrikeOut);
            MemMon.Font.Style := FS;
          Finally
            MemMon.EndUpdate;
          End;
          Result := True;
        End;
    Finally
      Free;
    End;
end;

(**

  This is the form's on create event handler.

  @precon  None.
  @postcon Initialises the list of Font Names.

  @param   Sender as a TObject

**)
(**

  This is an on form create event handler for the form.

  @precon  None.
  @postcon Populates the Font Name list box with system fonts.

  @param   Sender as a TObject

**)
procedure TfrmMemoryMonitorOptions.FormCreate(Sender: TObject);
begin
  cbxFontName.Items.Assign(Screen.Fonts);
end;

(**

  This is an On ChangingEx event handler for the Low Point control.

  @precon  None.
  @postcon Ensures that the Low point can not be larger than the MidPoint Point
           value.

  @param   Sender      as a TObject
  @param   AllowChange as a Boolean as a reference
  @param   NewValue    as a Smallint
  @param   Direction   as a TUpDownDirection

**)
procedure TfrmMemoryMonitorOptions.udLowPointChangingEx(Sender: TObject;
  var AllowChange: Boolean; NewValue: Smallint; Direction: TUpDownDirection);
begin
  AllowChange := (NewValue < udMidPointPoint.Position) And (NewValue > 0);
end;

(**

  This is an On ChangingEx event handler for the High Point control.

  @precon  None.
  @postcon Ensures that the High point can not be smaller than the MidPoint Point
           value.

  @param   Sender      as a TObject
  @param   AllowChange as a Boolean as a reference
  @param   NewValue    as a Smallint
  @param   Direction   as a TUpDownDirection

**)
procedure TfrmMemoryMonitorOptions.udHighPointChangingEx(Sender: TObject;
  var AllowChange: Boolean; NewValue: Smallint; Direction: TUpDownDirection);
begin
  AllowChange := (NewValue < 100) And (NewValue > udMidPointPoint.Position);
end;

(**

  This is an On ChangingEx event handler for the MidPoint Point control.

  @precon  None.
  @postcon Ensures that the MidPoint point can not be smaller than the Low
           Point value or greater than the High point value.

  @param   Sender      as a TObject
  @param   AllowChange as a Boolean as a reference
  @param   NewValue    as a Smallint
  @param   Direction   as a TUpDownDirection

**)
procedure TfrmMemoryMonitorOptions.udMidPointPointChangingEx(Sender: TObject;
  var AllowChange: Boolean; NewValue: Smallint; Direction: TUpDownDirection);
begin
  AllowChange := (NewValue < udHighPoint.Position) And
    (NewValue > udLowPoint.Position);
end;

end.

