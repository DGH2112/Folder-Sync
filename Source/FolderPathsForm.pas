(**

  A class to define a form for editing the Folder Paths.

  @Version 1.0
  @date    01 Aug 2012
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
  CheckLst;

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
    lbxSyncOptions: TCheckListBox;
    lblFldrSyncOptions: TLabel;
    Procedure FolderPathChange(Sender: TObject);
    Procedure btnBrowseLeftClick(Sender: TObject);
    Procedure btnBrowseRightClick(Sender: TObject);
    Procedure FormDestroy(Sender: TObject);
    Procedure FormCreate(Sender: TObject);
    Procedure lbxSyncOptionsClick(Sender: TObject);
  Private
    { Private declarations }
    FRootKey: String;
  Public
    { Public declarations }
    Class Function Execute(Var strLeftFolder, strRightFolder: String; strRootKey: String;
      Var SyncOptions: TSyncOptions): Boolean;
    Constructor CreateWithRootKey(AOwner: TComponent; strRootKey: String);
  End;

Implementation

Uses
  FileCtrl,
  IniFiles;

{$R *.DFM}

Const
  (** A constant array to provide string representations of the sync options. **)
  SyncOps: Array [Low(TSyncOption) .. High(TSyncOption)
    ] Of String = ('Enable Folder Comparison', 'Primary Left Folder',
    'Primary Right Folder', 'Overwrite Read Only Files', 'Confirm Yes', 'Confirm No');

{ TfrmFolderPaths }

(**

  This is a class method for invoking this dialogue and editing the folder paths
  .

  @precon  None.
  @postcon The function returns true is the dialogue was confirmed with the
           altered fodler paths in the var variables else returns false.

  @param   strLeftFolder  as a String as a reference
  @param   strRightFolder as a String as a reference
  @param   strRootKey     as a String
  @param   SyncOptions    as a TSyncOptions as a reference
  @return  a Boolean

**)
Class Function TfrmFolderPaths.Execute(Var strLeftFolder, strRightFolder: String;
  strRootKey: String; Var SyncOptions: TSyncOptions): Boolean;

Var
  i     : TSyncOption;
  iIndex: Integer;

Begin
  With TfrmFolderPaths.CreateWithRootKey(Nil, strRootKey) Do
    Try
      Result              := False;
      edtLeftFolder.Text  := strLeftFolder;
      edtRightFolder.Text := strRightFolder;
      For i               := Low(TSyncOption) To High(TSyncOption) Do
        Begin
          iIndex                         := lbxSyncOptions.Items.Add(SyncOps[i]);
          lbxSyncOptions.Checked[iIndex] := i In SyncOptions;
        End;
      If ShowModal = mrOK Then
        Begin
          strLeftFolder  := edtLeftFolder.Text;
          strRightFolder := edtRightFolder.Text;
          SyncOptions    := [];
          For i          := Low(TSyncOption) To High(TSyncOption) Do
            If lbxSyncOptions.Checked[Integer(i)] Then
              Include(SyncOptions, i);
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

  This is an on click event handler for the check list box.

  @precon  None.
  @postcon Ensures that Primary Left and Right are not checked at the same time and that
           Confirm Yes and No are not checked at the same time.

  @param   Sender as a TObject

**)
Procedure TfrmFolderPaths.lbxSyncOptionsClick(Sender: TObject);

Const
  SyncOpInts: Array [Low(TSyncOption) .. High(TSyncOption)
    ] Of Integer = (0, 1, 2, 3, 4, 5);

Var
  i: TSyncOption;

Begin
  i := TSyncOption(lbxSyncOptions.ItemIndex);
  If (i In [soPrimaryLeft]) And lbxSyncOptions.Checked[SyncOpInts[i]] Then
    lbxSyncOptions.Checked[SyncOpInts[soPrimaryRight]] := False;
  If (i In [soPrimaryRight]) And lbxSyncOptions.Checked[SyncOpInts[i]] Then
    lbxSyncOptions.Checked[SyncOpInts[soPrimaryLeft]] := False;
  If (i In [soConfirmYes]) And lbxSyncOptions.Checked[SyncOpInts[i]] Then
    lbxSyncOptions.Checked[SyncOpInts[soConfirmNo]] := False;
  If (i In [soConfirmNo]) And lbxSyncOptions.Checked[SyncOpInts[i]] Then
    lbxSyncOptions.Checked[SyncOpInts[soConfirmYes]] := False;
End;

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
