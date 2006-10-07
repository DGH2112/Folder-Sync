(**

  This module defines the options dialogue.

  @Date    07 Oct 2006
  @Version 1.0
  @Author  David Hoyle

**)
unit OptionsForm;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Buttons, ComCtrls;

type
  (** This is a class to represent **)
  TfrmOptions = class(TForm)
    lblTimeDiff: TLabel;
    edtTimeDiff: TEdit;
    udTolerance: TUpDown;
    btnOK: TBitBtn;
    btnCancel: TBitBtn;
    PageControl1: TPageControl;
    pgFolderList: TTabSheet;
    pgExclusions: TTabSheet;
    edtExclusions: TMemo;
    lvFolders: TListView;
    btnAdd: TBitBtn;
    btnEdit: TBitBtn;
    btnDelete: TBitBtn;
    procedure lvFoldersResize(Sender: TObject);
    procedure btnAddClick(Sender: TObject);
    procedure btnEditClick(Sender: TObject);
    procedure btnDeleteClick(Sender: TObject);
    procedure lvFoldersDblClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
    FRightWidth: Integer;
    FLeftWidth: Integer;
    procedure SetLeftWidth(const Value: Integer);
    procedure SetRightWidth(const Value: Integer);
    Procedure AddFolders(strLeft, strRight : String);
    (**
      This property holds the maximum width of the Right Folder Text.
      @precon  None.
      @postcon Gets or sets the maximum width of the Right Folder Text.
      @return  a Integer
    **)
    Property RightWidth : Integer Read FRightWidth Write SetRightWidth;
    (**
      This property holds the maximum width of the Left Folder Text.
      @precon  None.
      @postcon Gets or sets the maximum width of the Left Folder Text.
      @return  a Integer
    **)
    Property LeftWidth : Integer Read FLeftWidth Write SetLeftWidth;
  public
    { Public declarations }
    Class Function Execute(var slFolders : TStringList; var strExclusions :String;
      var iTolerance : Integer) : Boolean;
  end;

implementation

uses FolderPathsForm;

{$R *.DFM}

{ TfrmOptions }

(**

  This is the classes main interface method for editing the applications options.

  @precon  None.
  @postcon Returns true with the updated options in the var variables else
           returns false if the dialogue is cancelled.

  @param   slFolders     as a TStringList as a reference
  @param   strExclusions as a String as a reference
  @param   iTolerance    as an Integer as a reference
  @return  a Boolean

**)
class function TfrmOptions.Execute(var slFolders : TStringList;
  var strExclusions: String; var iTolerance: Integer): Boolean;

Var
  i : Integer;

begin
  Result := False;
  With TfrmOptions.Create(Nil) Do
    Try
      For i := 0 to slFolders.Count - 1 Do
        AddFolders(slFolders.Names[i], slFolders.Values[slFolders.Names[i]]);
      edtExclusions.Text := strExclusions;
      udTolerance.Position := iTolerance;
      If ShowModal = mrOK Then
        Begin
          slFolders.Clear;
          For i := 0 To lvFolders.Items.Count - 1 Do
            slFolders.Add(lvFolders.Items[i].Caption + '=' +
              lvFolders.Items[i].SubItems[0]);
          strExclusions := edtExclusions.Text;
          iTolerance := udTolerance.Position;
          Result := True;
        End;
    Finally
      Free;
    End;
end;

(**

  This is an on create event handler for the form.

  @precon  None.
  @postcon Initialises the Right and Left width properties for the form
           resizing.

  @param   Sender as a TObject

**)
procedure TfrmOptions.FormCreate(Sender: TObject);
begin
  FRightWidth := 1;
  FLeftWidth := 1;
end;

(**

  This method adds folder paths to the list view and updates the width
  properties for calculating the resize width of the list view.

  @precon  None.
  @postcon Adds folder paths to the list view and updates the width properties
           for calculating the resize width of the list view.

  @param   strLeft  as a String
  @param   strRight as a String

**)
procedure TfrmOptions.AddFolders(strLeft, strRight: String);

Var
  Item : TListItem;

begin
  Item := lvFolders.Items.Add;
  Item.Caption := strLeft;
  LeftWidth := Length(strLeft);
  Item.SubItems.Add(strRight);
  RightWidth := Length(strRight);
  lvFoldersResize(Self);
end;

(**

  This is an on click event handler for the Add button.

  @precon  None.
  @postcon Adds the folder paths returned from the Folder Paths dialogue to the
           list view.

  @param   Sender as a TObject

**)
procedure TfrmOptions.btnAddClick(Sender: TObject);

Var
  strLeft, strRight : String;

begin
  If TfrmFolderPaths.Execute(strLeft, strRight) Then
    AddFolders(strLeft, strRight);
end;

(**

  This is an on click event handler for the Edit button.

  @precon  None.
  @postcon Allows the selected item to be edited in the Folder Paths dialogue.

  @param   Sender as a TObject

**)
procedure TfrmOptions.btnEditClick(Sender: TObject);

Var
  strLeft, strRight : String;

begin
  If lvFolders.Selected <> Nil Then
    Begin
      strLeft := lvFolders.Selected.Caption;
      strRight := lvFolders.Selected.SubItems[0];
      If TfrmFolderPaths.Execute(strLeft, strRight) Then
        Begin
          lvFolders.Selected.Caption := strLeft;
          lvFolders.Selected.SubItems[0] := strRight;
          lvFoldersResize(Sender);
        End;
    End;
end;

(**

  This is an on click event handler for the delete button.

  @precon  None.
  @postcon Deletes the selected item from the folders list view.

  @param   Sender as a TObject

**)
procedure TfrmOptions.btnDeleteClick(Sender: TObject);

begin
  If lvFolders.Selected <> Nil Then
    lvFolders.Selected.Delete;
end;

(**

  This is an on double event handler for the list view.

  @precon  None.
  @postcon Edits the selected item in the list.

  @param   Sender as a TObject

**)
procedure TfrmOptions.lvFoldersDblClick(Sender: TObject);
begin
  btnEditClick(Self);
end;

(**

  This is an on Resize event handler for the list view.

  @precon  None.
  @postcon Resizes the list views columns to match the size of the window.

  @param   Sender as a TObject

**)
procedure TfrmOptions.lvFoldersResize(Sender: TObject);

Var
  i : Integer;

begin
  i := lvFolders.ClientWidth - 22;
  lvFolders.Column[0].Width := Trunc(i * Int(LeftWidth) / Int(LeftWidth + RightWidth));
  lvFolders.Column[1].Width := Trunc(i * Int(RightWidth) / Int(LeftWidth + RightWidth));
end;

(**

  This is a setter method for the LeftWidth property.

  @precon  None.
  @postcon Sets the LeftWidth porperty.

  @param   Value as an Integer constant

**)
procedure TfrmOptions.SetLeftWidth(const Value: Integer);
begin
  If Value > FLeftWidth Then
    FLeftWidth := Value;
end;

(**

  This is a setter method for the RightWidth property.

  @precon  None.
  @postcon Sets the RightWidth property.

  @param   Value as an Integer constant

**)
procedure TfrmOptions.SetRightWidth(const Value: Integer);
begin
  If Value > FRightWidth Then
    FRightWidth := Value;
end;

end.
