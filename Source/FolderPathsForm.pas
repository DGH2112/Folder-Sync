(**

  A class to define a form for editing the Folder Paths.

  @Version 1.0
  @date    19 Jan 2006
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
    BitBtn1: TBitBtn;
    BitBtn2: TBitBtn;
    procedure FolderPathChange(Sender: TObject);
    procedure btnBrowseLeftClick(Sender: TObject);
    procedure btnBrowseRightClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    Class Function Execute(var strLeftFolder, strRightFolder : String) : Boolean;
  end;

implementation

Uses
  FileCtrl;

{$R *.DFM}

{ TfrmFolderPaths }

(**

  This is a class method for invoking this dialogue and editing the folder paths.

  @precon  None.
  @postcon The function returns true is the dialogue was confirmed with the
           altered fodler paths in the var variables else returns false.

  @param   strLeftFolder  as a String as a reference
  @param   strRightFolder as a String as a reference
  @return  a Boolean

**)
Class function TfrmFolderPaths.Execute(var strLeftFolder,
  strRightFolder: String): Boolean;

begin
  With TfrmFolderPaths.Create(Nil) Do
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

end.
