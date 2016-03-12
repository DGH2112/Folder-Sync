(**

  A class to define a form for editing the Folder Paths.

  @Version 1.0
  @date    12 Mar 2016
  @Author  David Hoyle.

**)
Unit FolderPathsForm;

Interface

Uses
  Windows,
  Messages,
  SysUtils,
  Classes,
  Graphics,
  Controls,
  Forms,
  Dialogs,
  Buttons,
  StdCtrls,
  SyncModule,
  CheckLst, Vcl.ComCtrls;

Type
  (** A class to represent a dialogue for editing the folder paths. **)
  TfrmFolderPaths = Class(TForm)
    lblLeftFolder: TLabel;
    edtLeftFolder: TEdit;
    lblRightFolder: TLabel;
    edtRightFolder: TEdit;
    btnBrowseLeft: TButton;
    btnBrowseRight: TButton;
    btnOK: TBitBtn;
    btnCancel: TBitBtn;
    btnHelp: TBitBtn;
    lblMaxFileSize: TLabel;
    edtMaxFileSize: TEdit;
    cbxMaxFileSize: TComboBox;
    lbxSyncOptions: TListView;
    Procedure FolderPathChange(Sender: TObject);
    Procedure btnBrowseLeftClick(Sender: TObject);
    Procedure btnBrowseRightClick(Sender: TObject);
    Procedure FormDestroy(Sender: TObject);
    Procedure FormCreate(Sender: TObject);
    procedure btnHelpClick(Sender: TObject);
    procedure lbxSyncOptionsChange(Sender: TObject; Item: TListItem; Change: TItemChange);
  Private
    { Private declarations }
    FRootKey: String;
  Public
    { Public declarations }
    Class Function Execute(Folder : TFolder; strRootKey: String): Boolean;
    Constructor CreateWithRootKey(AOwner: TComponent; strRootKey: String);
  End;

Implementation

Uses
  FileCtrl,
  IniFiles;

{$R *.DFM}

Const
  (** A constant array to provide string representations of the sync options. **)
  SyncOps: Array [Low(TSyncOption)..High(TSyncOption)] Of String = (
    'Enable Folder Comparison',
    'Primary Left Folder',
    'Primary Right Folder',
    'Overwrite Read Only Files',
    'Confirm Copy Yes',
    'Confirm Delete Yes',
    'Confirm Copy No',
    'Confirm Delete No',
    'No Recursion',
    'Temporarily Disabled'
  );

{ TfrmFolderPaths }

(**

  This is a class method for invoking this dialogue and editing the folder paths
  .

  @precon  None.
  @postcon The function returns true is the dialogue was confirmed with the
           altered fodler paths in the var variables else returns false.

  @param   Folder     as a TFolder
  @param   strRootKey as a String
  @return  a Boolean

**)
Class Function TfrmFolderPaths.Execute(Folder : TFolder; strRootKey: String): Boolean;

Var
  i     : TSyncOption;
  iMaxFileSize: Int64;
  Item : TListItem;

Begin
  With TfrmFolderPaths.CreateWithRootKey(Nil, strRootKey) Do
    Try
      Result              := False;
      edtLeftFolder.Text  := Folder.LeftFldr + Folder.Patterns;
      edtRightFolder.Text := Folder.RightFldr + Folder.Patterns;
      lbxSyncOptions.OnChange := Nil;
      For i               := Low(TSyncOption) To High(TSyncOption) Do
        Begin
          Item := lbxSyncOptions.Items.Add;
          Item.Caption := SyncOps[i];
          Item.Checked := i In Folder.SyncOptions;
        End;
      lbxSyncOptions.OnChange := lbxSyncOptionsChange;
      iMaxFileSize := Folder.MaxFileSize;
      cbxMaxFileSize.ItemIndex := 0;
      While iMaxFileSize Mod 1024 = 0 Do
        Begin
          iMaxFileSize := iMaxFileSize Div 1024;
          cbxMaxFileSize.ItemIndex := cbxMaxFileSize.ItemIndex + 1;
          If cbxMaxFileSize.ItemIndex >= cbxMaxFileSize.Items.Count - 1 Then
            Break;
        End;
      edtMaxFileSize.Text := IntToStr(iMaxFileSize);
      If ShowModal = mrOK Then
        Begin
          Folder.LeftFldr  := ExtractFilePath(edtLeftFolder.Text);
          Folder.RightFldr := ExtractFilePath(edtRightFolder.Text);
          Folder.Patterns := ExtractFileName(edtLeftFolder.Text);
          Folder.SyncOptions := [];
          For i          := Low(TSyncOption) To High(TSyncOption) Do
            If lbxSyncOptions.Items[Integer(i)].Checked Then
              Folder.SyncOptions := Folder.SyncOptions + [i];
          Folder.MaxFileSize := StrToInt64(edtMaxFileSize.Text);
          Case cbxMaxFileSize.ItemIndex Of
            1: Folder.MaxFileSize := Folder.MaxFileSize * 1024;
            2: Folder.MaxFileSize := Folder.MaxFileSize * 1024 * 1024;
            3: Folder.MaxFileSize := Folder.MaxFileSize * 1024 * 1024 * 1024;
            4: Folder.MaxFileSize := Folder.MaxFileSize * 1024 * 1024 * 1024 * 1024;
          End;
          Result := True;
        End;
    Finally
      Free;
    End;
End;

(**

  This method makes sure that changes to the file filter on ther first path are
  replicated on the second path and visa versa.

  @precon  None.
  @postcon Makes sure that changes to the file filter on ther first path are
           replicated on the second path and visa versa.

  @param   Sender as a TObject

**)
Procedure TfrmFolderPaths.FolderPathChange(Sender: TObject);

Var
  src, dest    : TEdit;
  strFileFilter: String;

Begin
  If Sender Is TEdit Then
    Begin
      src := Sender As TEdit;
      If src = edtLeftFolder Then
        dest := edtRightFolder
      Else
        dest        := edtLeftFolder;
      strFileFilter := ExtractFileName(src.Text);
      dest.Text     := ExtractFilePath(dest.Text) + strFileFilter;
    End;
End;

(**

  This is the forms on Create event handler.

  @precon  None.
  @postcon Loads the forms position and sise from the registry.

  @param   Sender as a TObject

**)
Procedure TfrmFolderPaths.FormCreate(Sender: TObject);
Begin
  With TMemIniFile.Create(FRootKey) Do
    Try
      Top    := ReadInteger('FolderPathForm', 'Top', Top);
      Left   := ReadInteger('FolderPathForm', 'Left', Left);
      Height := ReadInteger('FolderPathForm', 'Height', Height);
      Width  := ReadInteger('FolderPathForm', 'Width', Width);
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
Procedure TfrmFolderPaths.FormDestroy(Sender: TObject);
Begin
  With TMemIniFile.Create(FRootKey) Do
    Try
      WriteInteger('FolderPathForm', 'Top', Top);
      WriteInteger('FolderPathForm', 'Left', Left);
      WriteInteger('FolderPathForm', 'Height', Height);
      WriteInteger('FolderPathForm', 'Width', Width);
      UpdateFile;
    Finally
      Free;
    End;
End;

(**

  This is an on change event handler for the check list box.

  @precon  None.
  @postcon Ensures that Primary Left and Right are not checked at the same time and that
           Confirm Yes and No are not checked at the same time.

  @param   Sender as a TObject
  @param   Item   as a TListItem
  @param   Change as a TItemChange

**)
procedure TfrmFolderPaths.lbxSyncOptionsChange(Sender: TObject; Item: TListItem;
  Change: TItemChange);

Const
  SyncOpInts: Array [Low(TSyncOption)..High(TSyncOption)] Of Integer =
    (0, 1, 2, 3, 4, 5, 6, 7, 8, 9);

Var
  i : TSyncOption;

begin
  i := TSyncOption(Item.Index);
  If (i In [soPrimaryLeft]) And lbxSyncOptions.Items[SyncOpInts[i]].Checked Then
    lbxSyncOptions.Items[SyncOpInts[soPrimaryRight]].Checked := False;
  If (i In [soPrimaryRight]) And lbxSyncOptions.Items[SyncOpInts[i]].Checked Then
    lbxSyncOptions.Items[SyncOpInts[soPrimaryLeft]].Checked := False;
  If (i In [soConfirmCopyYes]) And lbxSyncOptions.Items[SyncOpInts[i]].Checked Then
    lbxSyncOptions.Items[SyncOpInts[soConfirmCopyNo]].Checked := False;
  If (i In [soConfirmDeleteYes]) And lbxSyncOptions.Items[SyncOpInts[i]].Checked Then
    lbxSyncOptions.Items[SyncOpInts[soConfirmDeleteNo]].Checked := False;
  If (i In [soConfirmCopyNo]) And lbxSyncOptions.Items[SyncOpInts[i]].Checked Then
    lbxSyncOptions.Items[SyncOpInts[soConfirmCopyYes]].Checked := False;
  If (i In [soConfirmDeleteNo]) And lbxSyncOptions.Items[SyncOpInts[i]].Checked Then
    lbxSyncOptions.Items[SyncOpInts[soConfirmDeleteYes]].Checked := False;
end;

(**

  This is an on click event handler for the left browse button.

  @precon  None.
  @postcon Allows the user to browse for the left folder.

  @param   Sender as a TObject

**)
Procedure TfrmFolderPaths.btnBrowseLeftClick(Sender: TObject);

Var
  strFolder: String;

Begin
  strFolder := ExtractFilePath(edtLeftFolder.Text);
  If SelectDirectory('Left Folder', '', strFolder) Then
    edtLeftFolder.Text := strFolder + '\';
End;

(**

  This is an on click event handler for the right browse button.

  @precon  None.
  @postcon Allows the user to browse for the right folder.

  @param   Sender as a TObject

**)
Procedure TfrmFolderPaths.btnBrowseRightClick(Sender: TObject);

Var
  strFolder: String;

Begin
  strFolder := ExtractFilePath(edtRightFolder.Text);
  If SelectDirectory('Right Folder', '', strFolder) Then
    edtRightFolder.Text := strFolder + '\';
End;

(**

  This is an on click event handler for the Help button.

  @precon  None.
  @postcon Displays the Options topic of the HTML Help.

  @param   Sender as a TObject

**)
procedure TfrmFolderPaths.btnHelpClick(Sender: TObject);

begin
  Application.HelpContext(1);
end;

(**

  This is the constructor method for the TfrmFolderPaths class.

  @precon  None.
  @postcon Sets the FRootKey variable to the registry location where the
           settings are to be loaded and saved.

  @param   AOwner     as a TComponent
  @param   strRootKey as a String

**)
Constructor TfrmFolderPaths.CreateWithRootKey(AOwner: TComponent; strRootKey: String);
Begin
  Inherited Create(AOwner);
  FRootKey := strRootKey;
End;

End.
