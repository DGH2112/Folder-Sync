(**

  A class to define a form for editing the Folder Paths.

  @Version 1.0
  @date    06 Jan 2008
  @Author  David Hoyle.

**)
unit FolderPathsForm;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  Buttons, StdCtrls;

type
  (** A class to represent a dialogue for editing the folder paths. **)
  TfrmFolderPaths = class(TForm)
    lblLeftFolder: TLabel;
    edtLeftFolder: TEdit;
    lblRightFolder: TLabel;
    edtRightFolder: TEdit;
    btnBrowseLeft: TButton;
    btnBrowseRight: TButton;
    btnOK: TBitBtn;
    btnCancel: TBitBtn;
    procedure FolderPathChange(Sender: TObject);
    procedure btnBrowseLeftClick(Sender: TObject);
    procedure btnBrowseRightClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
    FRootKey : String;
  public
    { Public declarations }
    Class Function Execute(var strLeftFolder, strRightFolder : String;
      strRootKey : String) : Boolean;
    Constructor CreateWithRootKey(AOwner : TComponent; strRootKey : String);
  end;

implementation

Uses
  FileCtrl, Registry;

{$R *.DFM}

{ TfrmFolderPaths }

(**

  This is a class method for invoking this dialogue and editing the folder paths.

  @precon  None.
  @postcon The function returns true is the dialogue was confirmed with the
           altered fodler paths in the var variables else returns false.

  @param   strLeftFolder  as a String as a reference
  @param   strRightFolder as a String as a reference
  @param   strRootKey     as a String
  @return  a Boolean

**)
Class function TfrmFolderPaths.Execute(var strLeftFolder,
  strRightFolder: String; strRootKey : String): Boolean;

begin
  With TfrmFolderPaths.CreateWithRootKey(Nil, strRootKey) Do
    Try
      Result := False;
      edtLeftFolder.Text := strLeftFolder;
      edtRightFolder.Text := strRightFolder;
      If ShowModal = mrOK Then
        Begin
          strLeftFolder := edtLeftFolder.Text;
          strRightFolder := edtRightFolder.Text;
          Result := True;
        End;
    Finally
      Free;
    End;
end;

(**

  This method makes sure that changes to the file filter on ther first path are
  replicated on the second path and visa versa.

  @precon  None.
  @postcon Makes sure that changes to the file filter on ther first path are
           replicated on the second path and visa versa.

  @param   Sender as a TObject

**)
procedure TfrmFolderPaths.FolderPathChange(Sender: TObject);

Var
  src, dest : TEdit;
  strFileFilter : String;

begin
  If Sender Is TEdit Then
    Begin
      src := Sender As TEdit;
      If src = edtLeftFolder Then
        dest := edtRightFolder
      Else
        dest := edtLeftFolder;
      strFileFilter := ExtractFileName(src.Text);
      dest.Text := ExtractFilePath(dest.Text) + strFileFilter;
    End;
end;

(**

  This is the forms on Create event handler.

  @precon  None.
  @postcon Loads the forms position and sise from the registry.

  @param   Sender as a TObject

**)
procedure TfrmFolderPaths.FormCreate(Sender: TObject);
begin
  With TRegIniFile.Create(FRootKey) Do
    Try
      Top := ReadInteger('FolderPathForm', 'Top', Top);
      Left := ReadInteger('FolderPathForm', 'Left', Left);
      Height := ReadInteger('FolderPathForm', 'Height', Height);
      Width := ReadInteger('FolderPathForm', 'Width', Width);
    Finally
      Free;
    End;
End;

(**

  This is the forms on Destroy event handler.

  @precon  None.
  @postcon Saves the forms position and sise to the registry.

  @param   Sender as a TObject

**)
procedure TfrmFolderPaths.FormDestroy(Sender: TObject);
begin
  With TRegIniFile.Create(FRootKey) Do
    Try
      WriteInteger('FolderPathForm', 'Top', Top);
      WriteInteger('FolderPathForm', 'Left', Left);
      WriteInteger('FolderPathForm', 'Height', Height);
      WriteInteger('FolderPathForm', 'Width', Width);
    Finally
      Free;
    End;
end;

(**

  This is an on click event handler for the left browse button.

  @precon  None.
  @postcon Allows the user to browse for the left folder.

  @param   Sender as a TObject

**)
procedure TfrmFolderPaths.btnBrowseLeftClick(Sender: TObject);

Var
  strFolder : String;

begin
  If SelectDirectory('Left Folder', '', strFolder) Then
    edtLeftFolder.Text := strFolder + '\';
end;

(**

  This is an on click event handler for the right browse button.

  @precon  None.
  @postcon Allows the user to browse for the right folder.

  @param   Sender as a TObject

**)
procedure TfrmFolderPaths.btnBrowseRightClick(Sender: TObject);

Var
  strFolder : String;

begin
  If SelectDirectory('Right Folder', '', strFolder) Then
    edtRightFolder.Text := strFolder + '\';
end;

(**

  This is the constructor method for the TfrmFolderPaths class.

  @precon  None.
  @postcon Sets the FRootKey variable to the registry location where the
           settings are to be loaded and saved.

  @param   AOwner     as a TComponent
  @param   strRootKey as a String

**)
constructor TfrmFolderPaths.CreateWithRootKey(AOwner: TComponent;
  strRootKey: String);
begin
  Inherited Create(AOwner);
  FRootKey := strRootKey;
end;

end.
