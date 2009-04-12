(**

  This module defines the options dialogue.

  @Date    12 Apr 2009
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
    tabCompareFiles: TTabSheet;
    edtCompareEXE: TEdit;
    lblCompareFiles: TLabel;
    btnBrowse: TButton;
    dlgOpen: TOpenDialog;
    procedure lvFoldersResize(Sender: TObject);
    procedure btnAddClick(Sender: TObject);
    procedure btnEditClick(Sender: TObject);
    procedure btnDeleteClick(Sender: TObject);
    procedure lvFoldersDblClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure btnBrowseClick(Sender: TObject);
  private
    { Private declarations }
    FRightWidth: Integer;
    FLeftWidth: Integer;
    FRootKey : String;
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
    Class Function Execute(var slFolders : TStringList; var strExclusions,
      strCompareEXE :String; strRootKey : String) : Boolean;
    Constructor CreateWithRootKey(AOwner : TComponent; strRootKey : String); Virtual;
  end;

implementation

uses
  FolderPathsForm, IniFiles;

{$R *.DFM}

{ TfrmOptions }

(**

  This is the classes main interface method for editing the applications options
  .

  @precon  None.
  @postcon Returns true with the updated options in the var variables else
           returns false if the dialogue is cancelled.

  @param   slFolders     as a TStringList as a reference
  @param   strExclusions as a String as a reference
  @param   strCompareEXE as a String as a reference
  @param   strRootKey    as a String
  @return  a Boolean

**)
class function TfrmOptions.Execute(var slFolders : TStringList;
  var strExclusions, strCompareEXE : String; strRootKey : String): Boolean;

Var
  i : Integer;

begin
  Result := False;
  With TfrmOptions.CreateWithRootKey(Nil, strRootKey) Do
    Try
      For i := 0 to slFolders.Count - 1 Do
        AddFolders(slFolders.Names[i], slFolders.Values[slFolders.Names[i]]);
      edtExclusions.Text := strExclusions;
      edtCompareEXE.Text := strCompareEXE;
      If ShowModal = mrOK Then
        Begin
          slFolders.Clear;
          For i := 0 To lvFolders.Items.Count - 1 Do
            slFolders.Add(lvFolders.Items[i].Caption + '=' +
              lvFolders.Items[i].SubItems[0]);
          strExclusions := edtExclusions.Text;
          strCompareEXE := edtCompareEXE.Text;
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
           resizing. Also loads the forms size and position from the registry.

  @param   Sender as a TObject

**)
procedure TfrmOptions.FormCreate(Sender: TObject);
begin
  FRightWidth := 1;
  FLeftWidth := 1;
  With TIniFile.Create(FRootKey) Do
    Try
      Top := ReadInteger('Options', 'Top', Top);
      Left := ReadInteger('Options', 'Left', Left);
      Height := ReadInteger('Options', 'Height', Height);
      Width := ReadInteger('Options', 'Width', Width);
    Finally
      Free;
    End;
end;

(**

  This method is the forms on Destroy event handler.

  @precon  None.
  @postcon Saves the forms position and size to the registry.

  @param   Sender as a TObject

**)
procedure TfrmOptions.FormDestroy(Sender: TObject);
begin
  With TIniFile.Create(FRootKey) Do
    Try
      WriteInteger('Options', 'Top', Top);
      WriteInteger('Options', 'Left', Left);
      WriteInteger('Options', 'Height', Height);
      WriteInteger('Options', 'Width', Width);
    Finally
      Free;
    End;
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
  If TfrmFolderPaths.Execute(strLeft, strRight, FRootKey) Then
    AddFolders(strLeft, strRight);
end;

(**

  This is an on click event handler for the Browse button.

  @precon  None.
  @postcon Updates the edit control with the selected exe files.

  @param   Sender as a TObject

**)
procedure TfrmOptions.btnBrowseClick(Sender: TObject);
begin
  If DlgOpen.Execute Then
    edtCompareEXE.Text := dlgOpen.FileName;
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
      If TfrmFolderPaths.Execute(strLeft, strRight, FRootKey) Then
        Begin
          lvFolders.Selected.Caption := strLeft;
          lvFolders.Selected.SubItems[0] := strRight;
          lvFoldersResize(Sender);
        End;
    End;
end;

(**

  This is the constructor method for the TfrmOptions class.

  @precon  None.
  @postcon Sets the FRootKey variable for loading an saving settings to the
           registry.

  @param   AOwner     as a TComponent
  @param   strRootKey as a String

**)
constructor TfrmOptions.CreateWithRootKey(AOwner: TComponent; strRootKey: String);
begin
  Inherited Create(AOwner);
  FRootKey := strRootKey;
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

  @param   Value as an Integer as a constant

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

  @param   Value as an Integer as a constant

**)
procedure TfrmOptions.SetRightWidth(const Value: Integer);
begin
  If Value > FRightWidth Then
    FRightWidth := Value;
end;

end.
