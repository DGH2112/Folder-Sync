(**

  This module if a class to represent the applications main form interface.
  This form provide the display of differences between two folders.

  @Version 1.0
  @Date    02 Oct 2004
  @Author  David Hoyle

**)
unit MainForm;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  Menus, ActnList, ImgList, ComCtrls, ExtCtrls, ToolWin, StdCtrls, Buttons,
  AppEvnts, Registry, FileComparision, ProgressForm;

type
  (** A list of enumerate values for the different types of file operation that
      can be undertaken. **)
  TFileOp = (foNothing, foLeftToRight, foRightToLeft, foDelete);

  (** This is the class that actually describes for the form interface. **)
  TfrmMainForm = class(TForm)
    tbrToolBar: TToolBar;
    stbrStatusBar: TStatusBar;
    tbtnFileExit: TToolButton;
    mmMainMenu: TMainMenu;
    ilActionImages: TImageList;
    alActions: TActionList;
    actFileExit: TAction;
    mmiFile: TMenuItem;
    mmiFileExit: TMenuItem;
    mmiSep1: TMenuItem;
    mmiHelp: TMenuItem;
    mmiHelpAbout: TMenuItem;
    ToolButton5: TToolButton;
    actFileCompare: TAction;
    mmiFileCompare: TMenuItem;
    tbtnFileCompare: TToolButton;
    pmFiles: TPopupMenu;
    mmiEdit: TMenuItem;
    ToolButton2: TToolButton;
    actToolsOptions: TAction;
    mmiTools: TMenuItem;
    mmiToolsOptions: TMenuItem;
    ToolButton3: TToolButton;
    btnToolsOptions: TToolButton;
    actEditCopyRightToLeft: TAction;
    actEditCopyLeftToRight: TAction;
    tbtnRightToLeft: TToolButton;
    tbtnLefttoRight: TToolButton;
    ilFileTypeIcons: TImageList;
    lvFileList: TListView;
    actEditDelete: TAction;
    actEditDoNothing: TAction;
    CopyLefttoRight1: TMenuItem;
    CopyRighttoLeft1: TMenuItem;
    N1: TMenuItem;
    DeleteBoth1: TMenuItem;
    N2: TMenuItem;
    DoNothing1: TMenuItem;
    CopyLefttoRight2: TMenuItem;
    CopyRighttoLeft2: TMenuItem;
    N3: TMenuItem;
    DeleteBoth2: TMenuItem;
    N4: TMenuItem;
    DoNothing2: TMenuItem;
    ToolButton6: TToolButton;
    ToolButton7: TToolButton;
    actFileProcessFiles: TAction;
    N5: TMenuItem;
    ProcessFiles1: TMenuItem;
    procedure actFileExitExecute(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure actFileCompareExecute(Sender: TObject);
    procedure actToolsOptionsExecute(Sender: TObject);
    procedure actEditCopyLeftToRightExecute(Sender: TObject);
    procedure actEditCopyRightToLeftExecute(Sender: TObject);
    procedure actEditDeleteExecute(Sender: TObject);
    procedure actEditDoNothingExecute(Sender: TObject);
    procedure actFileProcessFilesExecute(Sender: TObject);
  private
    { Private declarations }
    FFolders : TStringList;
    FExclusions : String;
    FTolerance : Integer;
    FIconFiles : TStringList;
    frmProgress : TfrmProgress;
    Procedure LoadSettings();
    Procedure SaveSettings();
    Procedure ApplicationHint(Sender  : TObject);
    Procedure Progress(Sender : TObject; boolShow : Boolean;
      strPath, strFile : String; iCount : Integer);
    function GetImageIndex(strFileName: String): Integer;
    function GetRegInfo(Reg: TRegistry; strKey, strName: String): String;
    procedure FixUpPanes(fileCompColl : TCompareFoldersCollection);
    procedure InsertListItem(strLPath, strLFileName, strRPath,
      strRFileName: String; iLSize, iRSize, iLDateTime, iRDateTime: Integer);
    procedure FindNextNonSame(Lst: TFileList; var iIndex: Integer);
    procedure SetFileOperation(FileOp: TFileOp);
    procedure DeleteFiles;
    procedure CopyFiles;
  public
    { Public declarations }
  end;

Const
  (** Constant to represent the Left File Name position in the list view **)
  iLFileCol = 0;
  (** Constant to represent the Left File Size position in the list view **)
  iLSizeCol = 1;
  (** Constant to represent the Left File Date position in the list view **)
  iLDateCol = 2;
  (** Constant to represent the Right File Name position in the list view **)
  iRFileCol = 3;
  (** Constant to represent the Right File Size position in the list view **)
  iRSizeCol = 4;
  (** Constant to represent the Right File Date position in the list view **)
  iRDateCol = 5;
  (** Constant to represent the Left Folder position in the list view **)
  iLPathCol = 6;
  (** Constant to represent the Right Folder position in the list view **)
  iRPathCol = 7;

var
  (** A global variable used by Delphis auto create form process. **)
  frmMainForm: TfrmMainForm;

implementation

Uses
  FileCtrl, ShellAPI, OptionsForm;

Const
  (** This is the registry root key for storing the applications persistence
      data **)
  strRootKey : String = 'Software\Season''s Fall\Folder Sync\';
  (** This is a mask for displaying the number of files and total size of
      files. **)
  strStatus = '%1.0n File(s), %1.0n Byte(s)';


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

Var
  i : Integer;

begin
  With lvFileList Do
    Begin
      i := Width Div 2 - Column[iLSizeCol + 1].Width - Column[iLDateCol + 1].Width - 11 -
        Column[0].Width Div 2;
      If i > 0 Then
        Begin
          Column[iLFileCol + 1].Width := i;
          Column[iRFileCol + 1].Width := i;
        End;
    End;
  stbrStatusBar.Panels[0].Width := stbrStatusBar.ClientWidth Div 2;
end;

(**

  This method loads the applications seetings from the registry.

  @precon  None.
  @postcon Loads the applications settings from the registry.

**)
procedure TfrmMainForm.LoadSettings;

Var
  sl : TStringList;
  i : Integer;

begin
  With TRegIniFile.Create(strRootKey) Do
    Begin
      Top := ReadInteger('Setup', 'Top', 100);
      Left := ReadInteger('Setup', 'Left', 100);
      Height := ReadInteger('Setup', 'Height', 300);
      Width := ReadInteger('Setup', 'Width', 450);
      sl := TStringList.Create;
      Try
        ReadSection('Folders', sl);
        For i := 0 To sl.Count - 1 Do
          FFolders.Add(sl[i] + '=' + ReadString('Folders', sl[i], ''));
      Finally
        sl.Free;
      End;
      FExclusions := ReadString('Setup', 'Exclusions', '');
      FTolerance := ReadInteger('Setup', 'Tolerance', 0);
    End;
end;

(**

  This method saves the applications seetings to the registry.

  @precon  None.
  @postcon Save the applications settings to the registry.

**)
procedure TfrmMainForm.SaveSettings;

Var
  i : Integer;

begin
  With TRegIniFile.Create(strRootKey) Do
    Begin
      WriteInteger('Setup', 'Top', Top);
      WriteInteger('Setup', 'Left', Left);
      WriteInteger('Setup', 'Height', Height);
      WriteInteger('Setup', 'Width', Width);
      EraseSection('Folders');
      For i := 0 To FFolders.Count - 1 Do
        WriteString('Folders', FFolders.Names[i],
          FFolders.Values[FFolders.Names[i]]);
      WriteString('Setup', 'Exclusions', FExclusions);
      WriteInteger('Setup', 'Tolerance', FTolerance);
    End;
end;

(**

  This is the forms on create event handler.

  @precon  None.
  @postcon Creates various objects for the life time of the application and
           loads the applications settings from the registry.

  @param   Sender as a TObject

**)
procedure TfrmMainForm.FormCreate(Sender: TObject);

begin
  FFolders := TStringList.Create;
  frmProgress := TfrmProgress.Create(Self);
  LoadSettings();
  Application.OnHint := ApplicationHint;
  FIconFiles := TStringList.Create;
  actFileCompareExecute(Self);
end;

(**

  This is the forms on destroy event handler.

  @precon  None.
  @postcon Save the applications settings and free memory used by various
           objects.

  @param   Sender as a TObject

**)
procedure TfrmMainForm.FormDestroy(Sender: TObject);

begin
  SaveSettings();
  FIconFiles.Free;
  frmProgress.Free;
  FFolders.Free;
end;

(**

  This is the applications on hint event handler.

  @precon  None.
  @postcon Updates the application status bar when menu to toolbar items are
           used.

  @param   Sender as a TObject

**)
procedure TfrmMainForm.ApplicationHint(Sender: TObject);
begin
  stbrStatusBar.SimplePanel := Application.Hint <> '';
  If Application.Hint <> '' Then
    stbrStatusBar.SimpleText := GetLongHint(Application.Hint);
end;

(**

  This is an on execute method for the File Compare action.

  @precon  None.
  @postcon Start the process of comparing files and folders and output the
           results in the list view.

  @param   Sender as a TObject

**)
procedure TfrmMainForm.actFileCompareExecute(Sender: TObject);

Var
  fileCompColl : TCompareFoldersCollection;

begin
  fileCompColl := TCompareFoldersCollection.Create(FFolders,
    Progress, FExclusions, FTolerance);
  Try
    FixUpPanes(fileCompColl);
  Finally
    fileCompColl.Free;
  End;
end;

(**

  This method outputs the results of the various folder comparisons into the
  list view for actioning.

  @precon  fileCompColl must be a valid TCompareFoldersCollection.
  @postcon Outputs the results of the various folder comparisons into the
           list view for actioning.

  @param   fileCompColl as a TCompareFoldersCollection

**)
Procedure TfrmMainForm.FixUpPanes(fileCompColl : TCompareFoldersCollection);

  (**

    This is a local function that returns the maximum of the two given numbers.

    @precon  None.
    @postcon Returns the maximum of the two given numbers.

    @param   i as an Integer
    @param   j as an Integer
    @return  an Integer

  **)
  Function Max(i, j : Integer) : Integer;

  Begin
    If i > j Then
      Result := i
    Else
      Result := j;
  End;

Var
  iRight, iLeft : Integer;
  iCollection : Integer;

Begin
  lvFileList.Items.BeginUpdate;
  Try
    lvFileList.Items.Clear;
    For iCollection := 0 To fileCompColl.Count - 1 Do
      With fileCompColl.CompareFolders[iCollection] Do
        Begin
          iRight := -1;
          iLeft := -1;
          FindNextNonSame(LeftFldr, iLeft);
          FindNextNonSame(RightFldr, iRight);
          While (iLeft < LeftFldr.Count) Or (iRight < RightFldr.Count) Do
            Begin
              Progress(Self, True,
                Format('Updating Collection %d''s List...', [iCollection + 1]),
                'Please wait...', Max(iLeft, iRight));
              If (LeftFldr.Count > iLeft) And (RightFldr.Count > iRight) Then
                Begin
                  If AnsiCompareFileName(LeftFldr.FileName[iLeft], RightFldr.Filename[iRight]) = 0 Then
                    Begin
                      InsertListItem(
                        LeftFldr.FolderPath,
                        LeftFldr.FileName[iLeft],
                        RightFldr.FolderPath,
                        RightFldr.FileName[iRight],
                        LeftFldr.Size[iLeft],
                        RightFldr.Size[iRight],
                        LeftFldr.DateTime[iLeft],
                        RightFldr.DateTime[iRight]
                      );
                      FindNextNonSame(LeftFldr, iLeft);
                      FindNextNonSame(RightFldr, iRight);
                    End Else
                  If AnsiCompareFileName(LeftFldr.FileName[iLeft], RightFldr.Filename[iRight]) < 0 Then
                    Begin
                      InsertListItem(
                        LeftFldr.FolderPath,
                        LeftFldr.FileName[iLeft],
                        RightFldr.FolderPath,
                        '',
                        LeftFldr.Size[iLeft],
                        -1,
                        LeftFldr.DateTime[iLeft],
                        -1
                      );
                      FindNextNonSame(LeftFldr, iLeft);
                    End Else
                  If AnsiCompareFileName(LeftFldr.FileName[iLeft], RightFldr.Filename[iRight]) > 0 Then
                    Begin
                      InsertListItem(
                        LeftFldr.FolderPath,
                        '',
                        RightFldr.FolderPath,
                        RightFldr.FileName[iRight],
                        -1,
                        RightFldr.Size[iRight],
                        -1,
                        RightFldr.DateTime[iRight]
                      );
                      FindNextNonSame(RightFldr, iRight);
                    End;
                End Else
              If (LeftFldr.Count > iLeft) Then
                Begin
                  InsertListItem(
                    LeftFldr.FolderPath,
                    LeftFldr.FileName[iLeft],
                    RightFldr.FolderPath,
                    '',
                    LeftFldr.Size[iLeft],
                    -1,
                    LeftFldr.DateTime[iLeft],
                    -1
                  );
                  FindNextNonSame(LeftFldr, iLeft);
                End Else
              If (RightFldr.Count > iRight) Then
                Begin
                  InsertListItem(
                    LeftFldr.FolderPath,
                    '',
                    RightFldr.FolderPath,
                    RightFldr.FileName[iRight],
                    -1,
                    RightFldr.Size[iRight],
                    -1,
                    RightFldr.DateTime[iRight]
                  );
                  FindNextNonSame(RightFldr, iRight);
                End;
            End;
        End;
  Finally
    lvFileList.Items.EndUpdate;
  End;
End;

(**

  This method finds the next item in the file list that has not got the status
  of stSame and moved the iIndex value to the new position else return Count.

  @precon  None.
  @postcon Finds the next item in the file list that has not got the status
           of stSame and moved the iIndex value to the new position else return
           Count.

  @param   Lst    as a TFileList
  @param   iIndex as an Integer as a reference

**)
Procedure TfrmMainForm.FindNextNonSame(Lst : TFileList; var iIndex : Integer);

Var
  i : Integer;

Begin
  Inc(iIndex);
  For i := iIndex To Lst.Count Do
    If i < Lst.Count Then
      Begin
        If Lst.Status[i] <> stSame Then
          Begin
            iIndex := i;
            Break;
          End;
      End Else
        iIndex := Lst.Count;
End;

(**

  This method adds a pair of filenames with sizes and dates to the list view.

  @precon  None.
  @postcon Adds a pair of filenames with sizes and dates to the list view.

  @param   strLPath     as a String
  @param   strLFileName as a String
  @param   strRPath     as a String
  @param   strRFileName as a String
  @param   iLSize       as an Integer
  @param   iRSize       as an Integer
  @param   iLDateTime   as an Integer
  @param   iRDateTime   as an Integer

**)
Procedure TfrmMainForm.InsertListItem(strLPath, strLFileName, strRPath,
  strRFileName : String; iLSize, iRSize, iLDateTime, iRDateTime : Integer);

Const
  strSize = '%1.0n';
Var
  Item : TListItem;

Begin
  Item := lvFileList.Items.Add;
  Item.ImageIndex := -1;
  // Action
  Item.Caption := '';
  // Left File
  Item.SubItems.Add(strLFileName);
  If iLSize > -1 Then
    Item.SubItems.Add(Format(strSize, [iLSize + 0.1]))
  Else
    Item.SubItems.Add('');
  If iLDateTime > -1 Then
    Item.SubItems.Add(FormatDateTime('ddd dd/mmm/yyyy hh:mm:ss',
      FileDateToDateTime(iLDateTime)))
  Else
    Item.SubItems.Add('');
  // Right File
  Item.SubItems.Add(strRFileName);
  If iRSize > -1 Then
    Item.SubItems.Add(Format(strSize, [iRSize + 0.1]))
  Else
    Item.SubItems.Add('');
  If iRDateTime > -1 Then
    Item.SubItems.Add(FormatDateTime('ddd dd/mmm/yyyy hh:mm:ss',
      FileDateToDateTime(iRDateTime)))
  Else
    Item.SubItems.Add('');
  // Lefta nd Right Paths
  Item.SubItems.Add(strLPath);
  Item.SubItems.Add(strRPath);
  // Operation Status
  If iLDateTime < iRDateTime Then
    Item.StateIndex := Integer(foRightToLeft)
  Else
    Item.StateIndex := Integer(foLeftToRight);
  // Image Indexes
  If strLFileName <> '' Then
    Begin
      If strLFileName <> '' Then
        Item.SubItemImages[iLFileCol] := GetImageIndex(strLPath + strLFileName)
      Else
        Item.SubItemImages[iLFileCol] := -1;
      If strRFileName <> '' Then
        Item.SubItemImages[iRFileCol] := GetImageIndex(strLPath + strLFileName)
      Else
        Item.SubItemImages[iRFileCol] := -1;
    End Else
    Begin
      If strLFileName <> '' Then
        Item.SubItemImages[iLFileCol] := GetImageIndex(strRPath + strRFileName)
      Else
        Item.SubItemImages[iLFileCol] := -1;
      If strRFileName <> '' Then
        Item.SubItemImages[iRFileCol] := GetImageIndex(strRPath + strRFileName)
      Else
        Item.SubItemImages[iRFileCol] := -1;
    End;
End;

(**

  This method retrieves from the system the icon for the specified file, places
  it in a list and returns the index of the index in the list.

  @precon  None.
  @postcon Retrieves from the system the icon for the specified file, places
           it in a list and returns the index of the index in the list
  @param   strFileName as a String
  @return  an Integer    

**)
Function TfrmMainForm.GetImageIndex(strFileName : String) : Integer;

Var
  Reg : TRegistry;
  strExt : String;
  strClassName : String;
  strFName : String;
  iPos : Integer;
  iIndex : Integer;
  strWinSys : String;
  iIcon : Word;
  szIconFile : Array[0..MAX_PATH + 1] Of Char;
  Icon : TIcon;
  iLIcon, iSIcon : HICON;

Begin
  Reg := TRegistry.Create;
  Try
    Reg.RootKey := HKEY_CLASSES_ROOT;
    strExt := ExtractFileExt(strFileName);
    iSIcon := 0;
    strClassName := GetRegInfo(Reg, strExt, '');
    (*
      Try and find the Default Icon in the registry. This way we can
      find the correct icon index and then extract a small icon
      instead of extracting the large icon and scaling.
    *)
    If strClassName <> '' Then
      Begin
        strFName := GetRegInfo(Reg, strExt + '\DefaultIcon', '');
        If strFName = '' Then
          strFName := GetRegInfo(Reg, strClassName + '\DefaultIcon', '');
      End;
    Result := FIconFiles.IndexOf(strFName);
    If Result > -1 Then Exit;
    FIconFiles.Add(strFName);
    iPos := Pos(',', strFName);
    If iPos <> 0 Then
      Begin
        iIndex := StrToInt(Copy(strFName, iPos + 1, Length(strFName) - iPos));
        strFName := Copy(strFName, 1, iPos - 1);
        If Not FileExists(strFName) Then
          Begin
            SetLength(strWinSys, MAX_PATH + 1);
            SetLength(strWinSys, GetSystemDirectory(@strWinSys[1], MAX_PATH));
            strFName := strWinSys + '\' + strFName;
          End;
        ExtractIconEx(PChar(strFName), iIndex, iLIcon, iSIcon, 1);
      End;
    (*
      If the registry technique fail to get an icon use the
      ExtractAssociatedIcon function instead
    *)
    If iSIcon = 0 Then
      Begin
        iIcon := 0;
        StrCopy(szIconFile, PChar(strFileName));
        iSIcon := ExtractAssociatedIcon(Handle, szIconFile, iIcon);
      End;
    Icon := TIcon.Create;
    Try
      Icon.Handle := iSIcon;
      Result := ilFileTypeIcons.AddIcon(Icon);
    Finally
      Icon.Free;
    End;
  Finally
    Reg.Free;
  End;
End;


(**

  This method gets a values from a particular registration key.

  @precon  None.
  @postcon return a registration key value.

  @param   Reg     as a TRegistry
  @param   strKey  as a String
  @param   strName as a String
  @return  a String 

**)
Function TfrmMainForm.GetRegInfo(Reg : TRegistry; strKey, strName : String) : String;

Begin
  If Reg.OpenKeyReadOnly(strKey) Then
    Begin
      Result := Reg.ReadString(strName);
      Reg.CloseKey;
    End Else
      Result := '';
End;

(**

  This is an event handler for the progress of the searching and comparisons.

  @precon  None.
  @postcon Either show the progress dialogue with information or hides the
           dialogue.

  @param   Sender   as a TObject
  @param   boolShow as a Boolean
  @param   strPath  as a String
  @param   strFile  as a String
  @param   iCount   as an Integer

**)
procedure TfrmMainForm.Progress(Sender: TObject; boolShow: Boolean;
  strPath, strFile: String; iCount: Integer);

begin
  If frmProgress.Visible <> boolShow Then
    frmProgress.Visible := boolShow;
  frmProgress.Progress(iCount, strPath, strFile);
end;

(**

  This is an on execute event handler for the Tools Options action.

  @precon  None.
  @postcon Displays the options dialogue for the application.

  @param   Sender as a TObject

**)
procedure TfrmMainForm.actToolsOptionsExecute(Sender: TObject);
begin
  If TfrmOptions.Execute(FFolders, FExclusions, FTolerance) Then
    actFileCompareExecute(Self);
end;

(**

  This is an on execute event handler for the CopyLeftToRight Action.

  @precon  None.
  @postcon Sets the selected items in the list view to be Copied from Left to
           Right.

  @param   Sender as a TObject

**)
procedure TfrmMainForm.actEditCopyLeftToRightExecute(Sender: TObject);
begin
  SetFileOperation(foLeftToRight);
end;

(**

  This method sets the file operations for the selected list view items.

  @precon  None.
  @postcon Sets the selected items in the list view to the File op state passed
           while also checking that the operation is valid for that item.

  @param   FileOp as a TFileOp

**)
Procedure TfrmMainForm.SetFileOperation(FileOp : TFileOp);

Var
  i : Integer;

Begin
  For i := 0 To lvFileList.Items.Count - 1 Do
    With lvFileList Do
      If Items[i].Selected Then
        Begin
          Case FileOp Of
            foLeftToRight: If Items[i].SubItems[iLFileCol] = '' Then Continue;
            foRightToLeft: If Items[i].SubItems[iRFileCol] = '' Then Continue;
          End;
          lvFileList.Items[i].StateIndex := Integer(FileOp);
        End;
End;

(**

  This is an on execute event handler for the CopyRightToLeft action.

  @precon  None.
  @postcon Sets the selected items in the list view to be Copies from Right
           to Left.

  @param   Sender as a TObject

**)
procedure TfrmMainForm.actEditCopyRightToLeftExecute(Sender: TObject);
begin
  SetFileOperation(foRightToLeft);
end;

(**

  This is an on execute event handler for the Delete action.

  @precon  None.
  @postcon Sets the selected items in the list view to be Deleted. 

  @param   Sender as a TObject

**)
procedure TfrmMainForm.actEditDeleteExecute(Sender: TObject);
begin
  SetFileOperation(foDelete);
end;

(**

  This is an on execute event handler for the DoNothing action.

  @precon  None.
  @postcon Sets the selected items in the listview to DoNothing.

  @param   Sender as a TObject

**)
procedure TfrmMainForm.actEditDoNothingExecute(Sender: TObject);
begin
  SetFileOperation(foNothing);
end;

(**

  This method uses the information in the list view and the state index to
  determine which files need to be deleted and then deletes them.

  @precon  None.
  @postcon Deletes the required files specified within the listview.

**)
Procedure TfrmMainForm.DeleteFiles;

Var
  i : Integer;
  recFileOp : SHFILEOPSTRUCT;
  strFileList : String;

Begin
  For i := 0 to lvFileList. Items.Count - 1 Do
    With lvFileList.Items[i] Do
      Begin
        If TFileOp(StateIndex) = foDelete Then
          Begin
            If SubItems[iRFileCol] <> '' Then
              strFileList := strFileList + SubItems[iRPathCol] + SubItems[iRFileCol] + #0;
            If SubItems[iLFileCol] <> '' Then
              strFileList := strFileList + SubItems[iLPathCol] + SubItems[iLFileCol] + #0;
          End;
      End;
  If strFileList <> '' Then
    Begin
      strFileList := strFileList + #0;
      // Set file operation to delete files with recycle bin
      recFileOp.Wnd := Self.Handle;
      recFileOp.wFunc := FO_DELETE;
      recFileOp.pFrom := PChar(strFileList);
      recFileOp.pTo := '';
      recFileOp.fFlags := FOF_ALLOWUNDO;
      recFileOp.lpszProgressTitle := PChar('Deleting files...');
      SHFileOperation(recFileOp);
    End;
End;


(**

  This a an on execute event handler for the Process Files action.

  @precon  None.
  @postcon Firstly, it deletes any files need to be deleted from the list, then
           copies any files that need to be copied and finally refreshes the
           information in the list view.

  @param   Sender as a TObject

**)
procedure TfrmMainForm.actFileProcessFilesExecute(Sender: TObject);
begin
  DeleteFiles;
  CopyFiles;
  actFileCompareExecute(Self);
end;

(**

  This method uses the information in the list view along with the state index
  to determine the files that need to be copied.

  @precon  None.
  @postcon Undertakes all the file copying that need to be done.

**)
procedure TfrmMainForm.CopyFiles;

Var
  i : Integer;
  strSrcFileList, strDestFileList : String;
  recFileOp : SHFILEOPSTRUCT;

Begin
  With lvFileList Do
    For i := 0 To Items.Count - 1 Do
      Case TFileOp(Items[i].StateIndex) Of
        foLeftToRight:
          Begin
            strSrcFileList := strSrcFileList + Items[i].SubItems[iLPathCol] +
              Items[i].SubItems[iLFileCol] + #0;
            strDestFileList := strDestFileList + Items[i].SubItems[iRPathCol] +
              Items[i].SubItems[iLFileCol] + #0;
          End;
        foRightToLeft:
          Begin
            strSrcFileList := strSrcFileList + Items[i].SubItems[iRPathCol] +
              Items[i].SubItems[iRFileCol] + #0;
            strDestFileList := strDestFileList + Items[i].SubItems[iLPathCol] +
              Items[i].SubItems[iRFileCol] + #0;
          End;
      End;
  If (strSrcFileList <> '') And (strDestFileList <> '') Then
    Begin
      strSrcFileList := strSrcFileList + #0;
      strDestFileList := strDestFileList + #0;
      // Set file operation to delete files with recycle bin
      recFileOp.Wnd := Self.Handle;
      recFileOp.wFunc := FO_COPY;
      recFileOp.pFrom := PChar(strSrcFileList);
      recFileOp.pTo := PChar(strDestFileList);
      recFileOp.fFlags := FOF_ALLOWUNDO Or FOF_MULTIDESTFILES Or FOF_NOCONFIRMMKDIR;
      recFileOp.lpszProgressTitle := PChar('Copying files...');
      SHFileOperation(recFileOp);
    End;
End;

end.
