(**
  
  This module defines the options dialogue.

  @date    02 Oct 2004
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
    procedure btnAddClick(Sender: TObject);
    procedure btnEditClick(Sender: TObject);
    procedure btnDeleteClick(Sender: TObject);
    procedure lvFoldersDblClick(Sender: TObject);
  private
    { Private declarations }
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
  Item : TListItem;

begin
  Result := False;
  With TfrmOptions.Create(Nil) Do
    Try
      For i := 0 to slFolders.Count - 1 Do
        Begin
          Item := lvFolders.Items.Add;
          Item.Caption := slFolders.Names[i];
          Item.SubItems.Add(slFolders.Values[slFolders.Names[i]]);
        End;
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

  This is an on click event handler for the Add button.

  @precon  None.
  @postcon Adds the folder paths returned from the Folder Paths dialogue to the
           list view.

  @param   Sender as a TObject

**)
procedure TfrmOptions.btnAddClick(Sender: TObject);

Var
  strLeft, strRight : String;
  Item : TListItem;

begin
  If TfrmFolderPaths.Execute(strLeft, strRight) Then
    Begin
      Item := lvFolders.Items.Add;
      Item.Caption := strLeft;
      Item.SubItems.Add(strRight);
    End;
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

end.
