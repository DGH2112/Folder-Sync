(**
  
  This module if a class to represent the applications main form interface.
  This form provide the display of differences between two folders.

  @Version 1.0
  @Date    08 Apr 2004
  @Author  David Hoyle

**)
unit MainForm;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  Menus, ActnList, ImgList, ComCtrls, ExtCtrls, ToolWin, StdCtrls, Buttons,
  AppEvnts, ProgressForm;

type
  (** This is the class that actually describes for the form interface. **)
  TfrmMainForm = class(TForm)
    tbrToolBar: TToolBar;
    stbrStatusBar: TStatusBar;
    pnlLeft: TPanel;
    pnlRight: TPanel;
    lvLeft: TListView;
    lvRight: TListView;
    tbtnFileExit: TToolButton;
    mmMainMenu: TMainMenu;
    ilImages: TImageList;
    alActions: TActionList;
    actFileExit: TAction;
    mmiFile: TMenuItem;
    mmiFileExit: TMenuItem;
    edtLeftFolder: TEdit;
    btnLeftBrowse: TSpeedButton;
    edtRightFolder: TEdit;
    btnRightBrowse: TSpeedButton;
    actFileLeftBrowse: TAction;
    actFileRightBrowse: TAction;
    mmiFileLeftBrowse: TMenuItem;
    mmiFileRightBrowse: TMenuItem;
    mmiSep1: TMenuItem;
    mmiHelp: TMenuItem;
    mmiHelpAbout: TMenuItem;
    tbtnLeftBrowse: TToolButton;
    tbtnRightBrowse: TToolButton;
    ToolButton5: TToolButton;
    actFileCompare: TAction;
    mmiSep2: TMenuItem;
    mmiFileCompare: TMenuItem;
    ToolButton1: TToolButton;
    tbtnFileCompare: TToolButton;
    ilFileIcons: TImageList;
    actEditDelete: TAction;
    pmFiles: TPopupMenu;
    mmiEditDelete: TMenuItem;
    mmiEdit: TMenuItem;
    mmiEditDellete: TMenuItem;
    ToolButton2: TToolButton;
    tbtnEditDelete: TToolButton;
    actToolsOptions: TAction;
    mmiTools: TMenuItem;
    mmiToolsOptions: TMenuItem;
    ToolButton3: TToolButton;
    btnToolsOptions: TToolButton;
    actEditCopyRightToLeft: TAction;
    actEditCopyLeftToRight: TAction;
    mmiEditCopyLefttoRight: TMenuItem;
    mmiEditCopyRighttoLeft: TMenuItem;
    mmiSep3: TMenuItem;
    mmiSep4: TMenuItem;
    mmiCopyLefttoRight: TMenuItem;
    mmiCopyRighttoLeft: TMenuItem;
    tbtnRightToLeft: TToolButton;
    tbtnLefttoRight: TToolButton;
    procedure actFileExitExecute(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure btnLeftBrowseClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure btnRightBrowseClick(Sender: TObject);
    procedure actFileCompareExecute(Sender: TObject);
    procedure actEditDeleteExecute(Sender: TObject);
    procedure lvDragOver(Sender, Source: TObject; X, Y: Integer;
      State: TDragState; var Accept: Boolean);
    procedure lvDragDrop(Sender, Source: TObject; X, Y: Integer);
    procedure actToolsOptionsExecute(Sender: TObject);
    procedure actEditCopyLeftToRightExecute(Sender: TObject);
    procedure actEditCopyRightToLeftExecute(Sender: TObject);
  private
    { Private declarations }
    FProgressForm : TfrmProgress;
    FExclusions : String;
    FTolerance : Integer;
    Procedure LoadSettings();
    Procedure SaveSettings();
    Procedure ApplicationHint(Sender  : TObject);
    Procedure Progress(Sender : TObject; boolShow : Boolean;
      strPath, strFile : String; iCount : Integer);
  public
    { Public declarations }
  end;

var
  (** A global variable used by Delphis auto create form process. **)
  frmMainForm: TfrmMainForm;

implementation

Uses
  FileCtrl, Registry, FileComparision, ShellAPI, Math, OptionsForm;

Const
  (** This is the registry root key for storing the applications persistence
      data **)
  strRootKey : String = 'Season''s Fall\Folder Sync\';

{$R *.DFM}

(**

  This method is an Action List Execute method.

  @precon  None.
  @postcon Closes the application and returns to the OS.

  @param   Sender as a TObject

**)
procedure TfrmMainForm.actFileExitExecute(Sender: TObject);
begin
  Close();
end;

(**

  This is a Form on Resize event handler.

  @precon  None.
  @postcon Makes sure that the two sides of the interface are evenly spaced.

  @param   Sender as a TObject

**)
procedure TfrmMainForm.FormResize(Sender: TObject);
begin
  pnlLeft.Width := ClientWidth Div 2;
  With lvLeft Do
    Column[0].Width := Width - Column[1].Width - Column[2].Width - 22;
  With lvRight Do
    Column[0].Width := Width - Column[1].Width - Column[2].Width - 22;
end;

procedure TfrmMainForm.btnLeftBrowseClick(Sender: TObject);

Var
  strFldr : String;

begin
  If DirectoryExists(edtLeftFolder.Text) Then strFldr := edtLeftFolder.Text;
  If SelectDirectory(strFldr, [sdAllowCreate, sdPerformCreate, sdPrompt], 0) Then
    edtLeftFolder.Text := strFldr;
end;

procedure TfrmMainForm.LoadSettings;

begin
  With TRegIniFile.Create(strRootKey) Do
    Begin
      Top := ReadInteger('Setup', 'Top', 100);
      Left := ReadInteger('Setup', 'Left', 100);
      Height := ReadInteger('Setup', 'Height', 300);
      Width := ReadInteger('Setup', 'Width', 450);
      edtLeftFolder.Text := ReadString('Setup', 'Left Folder', '');
      edtRightFolder.Text := ReadString('Setup', 'Right Folder', '');
      FExclusions := ReadString('Setup', 'Exclusions', '');
      FTolerance := ReadInteger('Setup', 'Tolerance', 0);
    End;
end;

procedure TfrmMainForm.SaveSettings;

begin
  With TRegIniFile.Create(strRootKey) Do
    Begin
      WriteInteger('Setup', 'Top', Top);
      WriteInteger('Setup', 'Left', Left);
      WriteInteger('Setup', 'Height', Height);
      WriteInteger('Setup', 'Width', Width);
      WriteString('Setup', 'Left Folder', edtLeftFolder.Text);
      WriteString('Setup', 'Right Folder', edtRightFolder.Text);
      WriteString('Setup', 'Exclusions', FExclusions);
      WriteInteger('Setup', 'Tolerance', FTolerance);
    End;
end;

procedure TfrmMainForm.FormCreate(Sender: TObject);

begin
  LoadSettings();
  Application.OnHint := ApplicationHint;
  FProgressForm := TfrmProgress.Create(Self);
  actFileCompareExecute(Self);
end;

procedure TfrmMainForm.FormDestroy(Sender: TObject);

begin
  SaveSettings();
  FProgressForm.Free;
end;

procedure TfrmMainForm.btnRightBrowseClick(Sender: TObject);

Var
  strFldr : String;

begin
  If DirectoryExists(edtRightFolder.Text) Then strFldr := edtRightFolder.Text;
  If SelectDirectory(strFldr, [sdAllowCreate, sdPerformCreate, sdPrompt], 0) Then
    edtRightFolder.Text := strFldr;
end;

procedure TfrmMainForm.ApplicationHint(Sender: TObject);
begin
  stbrStatusBar.SimplePanel := Application.Hint <> '';
  If Application.Hint <> '' Then
    stbrStatusBar.SimpleText := GetLongHint(Application.Hint);
end;

procedure TfrmMainForm.actFileCompareExecute(Sender: TObject);

Var
  i : Integer;
  Item : TListItem;

begin
  If Not DirectoryExists(edtLeftFolder.Text) Then Exit;
  If Not DirectoryExists(edtRightFolder.Text) Then Exit;
  With TCompareFolders.Create(edtLeftFolder.Text, edtRightFolder.Text,
    Progress, FExclusions, FTolerance) Do
    Try
      lvLeft.Items.BeginUpdate;
      Try
        lvLeft.Items.Clear;
        For i := 0 To LeftFldr.Count - 1 Do
          Begin
            If LeftFldr.Status[i] In [stNewer, stOlder] Then
              Begin
                item := lvLeft.Items.Add;
                item.Caption := LeftFldr.Filename[i];
                item.SubItems.Add(Format('%1.0n', [LeftFldr.Size[i] + 0.1]));
                item.SubItems.Add(FormatDateTime('ddd dd/mmm/yyyy hh:mm:ss',
                  FileDateToDateTime(LeftFldr.DateTime[i])));
                Case LeftFldr.Status[i] Of
                  stNewer : item.ImageIndex := 0;
                  stOlder : item.ImageIndex := 3;
                Else
                  item.ImageIndex := -1;
                End;
              End;
          End;
      Finally
        lvLeft.Items.EndUpdate;
      End;
      lvRight.Items.BeginUpdate;
      Try
        lvRight.Items.Clear;
        For i := 0 To RightFldr.Count - 1 Do
          Begin
            If RightFldr.Status[i] In [stNewer, stOlder] Then
              Begin
                item := lvRight.Items.Add;
                item.Caption := RightFldr.Filename[i];
                item.SubItems.Add(Format('%1.0n', [RightFldr.Size[i] + 0.1]));
                item.SubItems.Add(FormatDateTime('ddd dd/mmm/yyyy hh:mm:ss',
                  FileDateToDateTime(RightFldr.DateTime[i])));
                Case RightFldr.Status[i] Of
                  stNewer : item.ImageIndex := 1;
                  stOlder : item.ImageIndex := 3;
                Else
                  item.ImageIndex := -1;
                End;
              End;
          End;
      Finally
        lvRight.Items.EndUpdate;
      End;
    Finally
      Free;
    End;
  i := 0;
  While i < Max(lvLeft.Items.Count, lvRight.Items.Count) Do
    Begin
      If lvLeft.Items.Count <= i Then
        Begin
          Item := lvLeft.Items.Add;
          Item.ImageIndex := -1;
        End;
      If lvRight.Items.Count <= i Then
        Begin
          Item := lvRight.Items.Add;
          Item.ImageIndex := -1;
        End;
      If lvLeft.Items[i].Caption <> '' Then
        If lvRight.Items[i].Caption <> '' Then
          Begin
          If AnsiCompareFileName(lvLeft.Items[i].Caption,
            lvRight.Items[i].Caption) > 0 Then
            Begin
              Item := lvLeft.Items.Insert(i);
              Item.ImageIndex := -1;
            End Else
          If AnsiCompareFileName(lvLeft.Items[i].Caption,
            lvRight.Items[i].Caption) < 0 Then
            Begin
              Item := lvRight.Items.Insert(i);
              Item.ImageIndex := -1;
            End;
        End;
      Inc(i);
    End;
end;

procedure TfrmMainForm.Progress(Sender: TObject; boolShow: Boolean;
  strPath, strFile: String; iCount: Integer);
begin
  FProgressForm.Progress(boolShow, strPath, strFile, iCount);
end;

procedure TfrmMainForm.actEditDeleteExecute(Sender: TObject);

Var
  i : Integer;
  strFolderPath : String;
  recFileOp : SHFILEOPSTRUCT;
  strFileList : String;

begin
  If ActiveControl Is TListView Then
    With ActiveControl As TListView Do
      Begin
        If ActiveControl = lvLeft Then
          strFolderPath := edtLeftFolder.Text
        Else
          strFolderPath := edtRightFolder.Text;
        If Length(strFolderPath) > 0 Then
          If strFolderPath[Length(strFolderPath)] In ['\', '/'] Then
            Delete(strFolderPath, Length(strFolderPath), 1);
        For i := 0 to Items.Count - 1 Do
          If Items[i].Selected Then
            strFileList := strFileList + strFolderPath + Items[i].Caption + #0;
        strFileList := strFileList + #0;
        // Set file operation to delete files with recycle bin
        recFileOp.Wnd := Self.Handle;
        recFileOp.wFunc := FO_DELETE;
        recFileOp.pFrom := PChar(strFileList);
        recFileOp.pTo := '';
        recFileOp.fFlags := FOF_ALLOWUNDO;
        recFileOp.lpszProgressTitle := PChar('Deleting files from ' + strFolderPath);
        SHFileOperation(recFileOp);
        actFileCompareExecute(Self);
      End;
end;

procedure TfrmMainForm.lvDragOver(Sender, Source: TObject; X,
  Y: Integer; State: TDragState; var Accept: Boolean);
begin
  Accept := (Source = lvLeft) And (Sender = lvRight) Or
    (Source = lvRight) And (Sender = lvLeft);
end;

procedure TfrmMainForm.lvDragDrop(Sender, Source: TObject; X,
  Y: Integer);

Var
  i : Integer;
  strSrcPath, strDestPath : String;
  strSrcFileList, strDestFileList : String;
  recFileOp : SHFILEOPSTRUCT;

begin
  If Source = lvLeft Then
    Begin
      strSrcPath := edtLeftFolder.Text;
      strDestPath := edtRightFolder.Text;
    End Else
    Begin
      strSrcPath := edtRightFolder.Text;
      strDestPath := edtLeftFolder.Text;
    End;
  If Length(strSrcPath) > 0 Then
    If strSrcPath[Length(strSrcPath)] In ['/', '\'] Then
      Delete(strSrcPath, Length(strSrcPath), 1);
  If Length(strDestPath) > 0 Then
    If strDestPath[Length(strDestPath)] In ['/', '\'] Then
      Delete(strDestPath, Length(strDestPath), 1);
  With Source As TListView Do
    For i := 0 To Items.Count - 1 Do
      If (Items[i].Selected) And (Items[i].Caption <> '') Then
        Begin
          strSrcFileList := strSrcFileList + strSrcPath + Items[i].Caption + #0;
          strDestFileList := strDestFileList + strDestPath + Items[i].Caption + #0;
        End;
  strSrcFileList := strSrcFileList + #0;
  strDestFileList := strDestFileList + #0;
  // Set file operation to delete files with recycle bin
  recFileOp.Wnd := Self.Handle;
  recFileOp.wFunc := FO_COPY;
  recFileOp.pFrom := PChar(strSrcFileList);
  recFileOp.pTo := PChar(strDestFileList);
  recFileOp.fFlags := FOF_ALLOWUNDO Or FOF_MULTIDESTFILES;
  recFileOp.lpszProgressTitle := PChar('Copying files from ' + strSrcPath +
    ' to ' + strDestPath);
  SHFileOperation(recFileOp);
  actFileCompareExecute(Self);
end;

procedure TfrmMainForm.actToolsOptionsExecute(Sender: TObject);
begin
  If TfrmOptions.Execute(FExclusions, FTolerance) Then
    actFileCompareExecute(Self);
end;

procedure TfrmMainForm.actEditCopyLeftToRightExecute(Sender: TObject);
begin
  lvLeft.SetFocus;
  lvDragDrop(Self, lvLeft, 0, 0);
end;

procedure TfrmMainForm.actEditCopyRightToLeftExecute(Sender: TObject);
begin
  lvRight.SetFocus;
  lvDragDrop(Self, lvRight, 0, 0);
end;

end.
