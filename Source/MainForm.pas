(**

  This module if a class to represent the applications main form interface.
  This form provide the display of differences between two folders.

  @Version 1.0
  @Date    12 Apr 2009
  @Author  David Hoyle

**)
unit MainForm;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  Menus, ActnList, ImgList, ComCtrls, ExtCtrls, ToolWin, StdCtrls, Buttons,
  AppEvnts, IniFiles, Registry, FileComparision, ProgressForm;

type
  (** A list of enumerate values for the different types of file operation that
      can be undertaken. **)
  TFileOp = (foNothing, foLeftToRight, foRightToLeft, foDelete, foSizeDiff);

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
    mmiFileSep2: TMenuItem;
    mmiHelp: TMenuItem;
    mmiHelpAbout: TMenuItem;
    tbtnSep1: TToolButton;
    actFileCompare: TAction;
    mmiFileCompare: TMenuItem;
    tbtnFileCompare: TToolButton;
    pmFiles: TPopupMenu;
    mmiEdit: TMenuItem;
    tbtnSep3: TToolButton;
    actToolsOptions: TAction;
    mmiTools: TMenuItem;
    mmiToolsOptions: TMenuItem;
    tbtnSep2: TToolButton;
    btnToolsOptions: TToolButton;
    actEditCopyRightToLeft: TAction;
    actEditCopyLeftToRight: TAction;
    tbtnRightToLeft: TToolButton;
    tbtnLefttoRight: TToolButton;
    lvFileList: TListView;
    actEditDelete: TAction;
    actEditDoNothing: TAction;
    mmiEditCopyLefttoRight: TMenuItem;
    mmiEditCopyRighttoLeft: TMenuItem;
    mmiEditSep1: TMenuItem;
    mmiEditDeleteBoth: TMenuItem;
    mmiEditSep2: TMenuItem;
    mmiEditDoNothing: TMenuItem;
    mmiCopyLefttoRight: TMenuItem;
    mmiCopyRighttoLeft: TMenuItem;
    mmiSep1: TMenuItem;
    mmiDeleteBoth: TMenuItem;
    mmiSep2: TMenuItem;
    mmiDoNothing: TMenuItem;
    tbtnEditDelete: TToolButton;
    tbtnEditDoNothing: TToolButton;
    actFileProcessFiles: TAction;
    mmiFileSep1: TMenuItem;
    mmiFileProcessFiles: TMenuItem;
    actEditSelectAll: TAction;
    tbtnEditSelectAll: TToolButton;
    tbtnSep4: TToolButton;
    mmiEditSep3: TMenuItem;
    mmiEditSelectAll: TMenuItem;
    mmiSep3: TMenuItem;
    mmiSelectAll: TMenuItem;
    appEvents: TApplicationEvents;
    actHelpAbout: TAction;
    actHelpCheckForUpdates: TAction;
    N1: TMenuItem;
    CheckforUpdates1: TMenuItem;
    ilFileTypeIcons: TImageList;
    actToolsCompare: TAction;
    tbtnSep5: TToolButton;
    tbtnCompareFiles: TToolButton;
    N2: TMenuItem;
    Compare1: TMenuItem;
    procedure actHelpAboutExecute(Sender: TObject);
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
    procedure actEditSelectAllExecute(Sender: TObject);
    procedure appEventsException(Sender: TObject; E: Exception);
    procedure lvFileListCustomDrawItem(Sender: TCustomListView; Item: TListItem;
      State: TCustomDrawState; var DefaultDraw: Boolean);
    procedure actHelpCheckForUpdatesExecute(Sender: TObject);
    procedure actToolsCompareExecute(Sender: TObject);
    procedure actToolsCompareUpdate(Sender: TObject);
  private
    { Private declarations }
    FFolders : TStringList;
    FExclusions : String;
    FIconFiles : TStringList;
    frmProgress : TfrmProgress;
    FRootKey : String;
    FParams : TStringList;
    FCompareEXE : String;
    Procedure LoadSettings();
    Procedure SaveSettings();
    Procedure ApplicationHint(Sender  : TObject);
    Procedure Progress(Sender : TObject; boolShow : Boolean;
      strPath, strFile : String; iCount : Integer);
    function GetImageIndex(strFileName: String): Integer;
    function GetRegInfo(Reg: TRegistry; strKey, strName: String): String;
    procedure FixUpPanes(fileCompColl : TCompareFoldersCollection);
    procedure InsertListItem(strLPath, strRPath : String; LeftFile,
      RightFile : TFileRecord);
    procedure FindNextNonSame(Lst: TFileList; var iIndex: Integer);
    procedure SetFileOperation(FileOp: TFileOp);
    procedure DeleteFiles;
    procedure CopyFiles;
    Function CheckFolders : Boolean;
    Procedure OperationStatus(LeftFile, RightFile : TFileRecord;
      Var Item: TListItem);
    procedure ImageIndexes(strLPath, strRPath : String; LeftFile,
      RightFile : TFileRecord; Item: TListItem);
    Procedure ExceptionProc(strExceptionMsg : String);
  end;

  (** This is a custon exception for folders not found or created. **)
  TFolderNotFoundException = Class(Exception);

ResourceString
  (** A resource string for the Update URLs **)
  strUpdateURLs = 'http://www.hoyld.freeserve.co.uk/HoylD.xml|' +
    'C:\Documents and Settings\HoylD\My Documents\Web Page\HoylD.xml';
  (** A resource string for the Update Software ID **)
  strSoftwareID = 'FldrSync';

Const
  (** Constant to represent the Left File Name position in the list view **)
  iLDisplayCol = 1;
  (** Constant to represent the Left File Attributes position in the list view **)
  iLAttrCol = 2;
  (** Constant to represent the Left File Size position in the list view **)
  iLSizeCol = 3;
  (** Constant to represent the Left File Date position in the list view **)
  iLDateCol = 4;
  (** Constant to represent the Right File Name position in the list view **)
  iRDisplayCol = 5;
  (** Constant to represent the Right File Attributes position in the list view **)
  iRAttrCol = 6;
  (** Constant to represent the Right File Size position in the list view **)
  iRSizeCol = 7;
  (** Constant to represent the Right File Date position in the list view **)
  iRDateCol = 8;
  (** Constant to represent the Left Folder position in the list view **)
  iLFullFileNameCol = 9;
  (** Constant to represent the Right Folder position in the list view **)
  iRFullFileNameCol = 10;

var
  (** A global variable used by Delphis auto create form process. **)
  frmMainForm: TfrmMainForm;

implementation

Uses
  FileCtrl, ShellAPI, OptionsForm, About, CheckForUpdates, DGHLibrary;

Const
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
      i := Width Div 2 - Column[0].Width Div 2 - Column[iLAttrCol].Width -
        Column[iLSizeCol].Width - Column[iLDateCol].Width - 11;
      If i > 0 Then
        Begin
          Column[iLDisplayCol].Width := i;
          Column[iRDisplayCol].Width := i;
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
  With TIniFile.Create(FRootKey) Do
    Begin
      Top := ReadInteger('Setup', 'Top', 100);
      Left := ReadInteger('Setup', 'Left', 100);
      Height := ReadInteger('Setup', 'Height', 300);
      Width := ReadInteger('Setup', 'Width', 450);
      FCompareEXE := ReadString('Setup', 'CompareEXE', '');
      sl := TStringList.Create;
      Try
        ReadSection('Folders', sl);
        For i := 0 To sl.Count - 1 Do
          FFolders.Add(sl[i] + '=' + ReadString('Folders', sl[i], ''));
      Finally
        sl.Free;
      End;
      FExclusions := StringReplace(ReadString('Setup', 'Exclusions', ''), '|',
        #13#10, [rfReplaceAll]);
      Free;
    End;
end;

(**

  This is an CustomDrawItem event for the list view.

  @precon  None.
  @postcon Draw readonly items in the list with a light red background.

  @param   Sender      as a TCustomListView
  @param   Item        as a TListItem
  @param   State       as a TCustomDrawState
  @param   DefaultDraw as a Boolean as a reference

**)
procedure TfrmMainForm.lvFileListCustomDrawItem(Sender: TCustomListView;
  Item: TListItem; State: TCustomDrawState; var DefaultDraw: Boolean);

var
  i: Integer;
  R, ItemR : TRect;
  Buffer : Array[0..2048] Of Char;
  Ops : Integer;

  (**

    This function returns display rectangle for the given indexed sub item.

    @precon  iIndex must be a valid SubItem index..
    @postcon Returns display rectangle for the given indexed sub item.

    @param   iIndex as an Integer
    @return  a TRect

  **)
  Function GetSubItemRect(iIndex : Integer) : TRect;

  Var
    j : Integer;

  Begin
    Result := Item.DisplayRect(drBounds);
    For j := 0 To iIndex Do
      Begin
        Inc(Result.Left, Sender.Column[j].Width);
        Result.Right := Result.Left + Sender.Column[j + 1].Width;
      End;
    Inc(Result.Top, 2);   // Padding / Margin
    Inc(Result.Bottom, 2);// Padding / Margin
    Inc(Result.Left, 6);  // Padding / Margin
    Dec(Result.Right, 6); // Padding / Margin
  End;

begin
  DefaultDraw := False;
  // Set Left Background
  Sender.Canvas.Brush.Color := clWindow;
  If Item.Selected Then
    Sender.Canvas.Brush.Color := clHighlight
  Else
    If (Pos('R', Item.SubItems[iLAttrCol - 1]) > 0) Then
      Sender.Canvas.Brush.Color := $BBBBFF;
  R := GetSubItemRect(iLDateCol - 1);
  ItemR := Item.DisplayRect(drBounds);
  ItemR.Right := R.Right + 6;
  Sender.Canvas.FillRect(ItemR);
  // Set Right Background
  Sender.Canvas.Brush.Color := clWindow;
  If Item.Selected Then
    Sender.Canvas.Brush.Color := clHighlight
  Else
    If (Pos('R', Item.SubItems[iRAttrCol - 1]) > 0) Then
      Sender.Canvas.Brush.Color := $BBBBFF;
  R := GetSubItemRect(iRDisplayCol - 1);
  ItemR := Item.DisplayRect(drBounds);
  ItemR.Left := R.Left - 6;
  Sender.Canvas.FillRect(ItemR);
  // Set Text Attributes
  If TFileOp(Item.StateIndex) = foNothing Then
    Sender.Canvas.Font.Color := clGray;
  If TFileOp(Item.StateIndex) = foDelete Then
    Sender.Canvas.Font.Style := Sender.Canvas.Font.Style + [fsStrikeout];
  If Item.Selected Then
    Sender.Canvas.Font.Color := clHighlightText;
  R := Item.DisplayRect(drBounds);
  ilActionImages.Draw(Sender.Canvas, R.Left + 2, R.Top, Item.StateIndex, True);
  For i := 0 To iRDateCol - 1 Do
    Begin
      Case i Of
        0, 4 : Ops := DT_LEFT Or DT_MODIFYSTRING Or DT_PATH_ELLIPSIS Or DT_NOPREFIX;
        2, 3, 6, 7: Ops := DT_RIGHT;
      Else
        Ops := DT_LEFT;
      End;
      R := GetSubItemRect(i);
      If Item.SubItemImages[i] > -1 Then
        If i In [0, 4] Then
          Begin
            Dec(R.Left, 3);
            ilFileTypeIcons.Draw(Sender.Canvas, R.Left, R.Top,
              Item.SubItemImages[i], True);
            Inc(R.Left, 16 + 3);
          End;
      StrPCopy(Buffer, Item.SubItems[i]);
      Sender.Canvas.Brush.Color := clWindow;
      If Item.Selected Then
        Sender.Canvas.Brush.Color := clHighlight
      Else
        If i In [iLDisplayCol - 1..iLDateCol - 1] Then
          Begin
            If (Pos('R', Item.SubItems[iLAttrCol - 1]) > 0) Then
              Sender.Canvas.Brush.Color := $BBBBFF
          End Else
          Begin
            If (Pos('R', Item.SubItems[iRAttrCol - 1]) > 0) Then
              Sender.Canvas.Brush.Color := $BBBBFF
          End;
      Sender.Canvas.Refresh;
      DrawText(Sender.Canvas.Handle, Buffer, Length(Item.SubItems[i]), R, Ops);
      R.Left := R.Right;
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
  With TIniFile.Create(FRootKey) Do
    Begin
      WriteInteger('Setup', 'Top', Top);
      WriteInteger('Setup', 'Left', Left);
      WriteInteger('Setup', 'Height', Height);
      WriteInteger('Setup', 'Width', Width);
      WriteString('Setup', 'CompareEXE', FCompareEXE);
      EraseSection('Folders');
      For i := 0 To FFolders.Count - 1 Do
        WriteString('Folders', FFolders.Names[i],
          FFolders.Values[FFolders.Names[i]]);
      WriteString('Setup', 'Exclusions',
        StringReplace(FExclusions, #13#10, '|', [rfReplaceAll]));
      Free;
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
  FParams := TStringList.Create;
  FRootKey := BuildRootKey(FParams, ExceptionProc);
  TfrmAbout.ShowAbout(FRootKey);
  actHelpCheckForUpdatesExecute(Nil);
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
  FParams.Free;
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
  If Not CheckFolders Then Exit;
  fileCompColl := TCompareFoldersCollection.Create(FFolders,
    Progress, FExclusions);
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
  dblLSize, dblRSize, dblLCount, dblRCount : Double;
  iCollection : Integer;

Begin
  dblLSize := 0;
  dblRSize := 0;
  dblLCount := 0;
  dblRCount := 0;
  lvFileList.Items.BeginUpdate;
  Try
    lvFileList.Items.Clear;
    For iCollection := 0 To fileCompColl.Count - 1 Do
      With fileCompColl.CompareFolders[iCollection] Do
        Begin
          If LeftFldr = Nil Then Continue;
          If RightFldr = Nil Then Continue;
          dblLSize := dblLSize + LeftFldr.TotalSize;
          dblRSize := dblRSize + RightFldr.TotalSize;
          dblLCount := dblLCount + LeftFldr.Count;
          dblRCount := dblRCount + RightFldr.Count;
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
                  If AnsiCompareFileName(LeftFldr[iLeft].FileName,
                    RightFldr[iRight].Filename) = 0 Then
                    Begin
                      InsertListItem(LeftFldr.FolderPath, RightFldr.FolderPath,
                        LeftFldr[iLeft], RightFldr[iRight]);
                      FindNextNonSame(LeftFldr, iLeft);
                      FindNextNonSame(RightFldr, iRight);
                    End Else
                  If AnsiCompareFileName(LeftFldr[iLeft].FileName,
                    RightFldr[iRight].Filename) < 0 Then
                    Begin
                      InsertListItem(LeftFldr.FolderPath, RightFldr.FolderPath,
                        LeftFldr[iLeft], Nil);
                      FindNextNonSame(LeftFldr, iLeft);
                    End Else
                  If AnsiCompareFileName(LeftFldr[iLeft].FileName,
                    RightFldr[iRight].Filename) > 0 Then
                    Begin
                      InsertListItem(LeftFldr.FolderPath, RightFldr.FolderPath,
                        Nil, RightFldr[iRight]);
                      FindNextNonSame(RightFldr, iRight);
                    End;
                End Else
              If (LeftFldr.Count > iLeft) Then
                Begin
                  InsertListItem(LeftFldr.FolderPath, RightFldr.FolderPath,
                    LeftFldr[iLeft], Nil);
                  FindNextNonSame(LeftFldr, iLeft);
                End Else
              If (RightFldr.Count > iRight) Then
                Begin
                  InsertListItem(LeftFldr.FolderPath, RightFldr.FolderPath,
                    Nil, RightFldr[iRight]);
                  FindNextNonSame(RightFldr, iRight);
                End;
            End;
        End;
  Finally
    lvFileList.Items.EndUpdate;
  End;
  stbrStatusBar.Panels[0].Text := Format(strStatus, [dblLCount, dblLSize]);
  stbrStatusBar.Panels[1].Text := Format(strStatus, [dblRCount, dblRSize]);
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
        If Lst[i].Status <> stSame Then
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
  @param   strRPath     as a String
  @param   LeftFile     as a TFileRecord
  @param   RightFile    as a TFileRecord

**)
Procedure TfrmMainForm.InsertListItem(strLPath, strRPath : String; LeftFile,
  RightFile : TFileRecord);

  (**

    This method returns a string representation of a files attrivutes [RASH].

    @precon  None.
    @postcon Returns a string representation of a files attrivutes [RASH].

    @param   iAttr as an Integer
    @return  a String

  **)
  Function GetAttributeString(iAttr : Integer) : String;

  Begin
    Result := '....';
    If iAttr And faReadOnly > 0 Then
      Result[1] := 'R';
    If iAttr And faArchive > 0 Then
      Result[2] := 'A';
    If iAttr And faSysFile > 0 Then
      Result[3] := 'S';
    If iAttr And faHidden > 0 Then
      Result[4] := 'H';
  End;

Const
  strSize = '%1.0n';
Var
  Item : TListItem;
  strFileName : String;

Begin
  Item := lvFileList.Items.Add;
  Item.ImageIndex := -1;
  // Action
  Item.Caption := '';
  // Left File
  If LeftFile <> Nil Then
    Begin
      Item.SubItems.Add(strLPath + LeftFile.FileName);
      Item.SubItems.Add(GetAttributeString(LeftFile.Attributes));
      Item.SubItems.Add(Format(strSize, [LeftFile.Size + 0.1]));
      Item.SubItems.Add(FormatDateTime('ddd dd/mmm/yyyy hh:mm:ss',
        FileDateToDateTime(LeftFile.DateTime)));
      strFileName := LeftFile.FileName;
    End Else
    Begin
      Item.SubItems.Add('');
      Item.SubItems.Add('');
      Item.SubItems.Add('');
      Item.SubItems.Add('');
    End;
  // Right File
  If RightFile <> Nil Then
    Begin
      Item.SubItems.Add(strRPath + RightFile.FileName);
      Item.SubItems.Add(GetAttributeString(RightFile.Attributes));
      Item.SubItems.Add(Format(strSize, [RightFile.Size + 0.1]));
      Item.SubItems.Add(FormatDateTime('ddd dd/mmm/yyyy hh:mm:ss',
        FileDateToDateTime(RightFile.DateTime)));
      strFileName := RightFile.FileName;
    End Else
    Begin
      Item.SubItems.Add('');
      Item.SubItems.Add('');
      Item.SubItems.Add('');
      Item.SubItems.Add('');
    End;
  Item.SubItems.Add(strLPath + strFileName);
  Item.SubItems.Add(strRPath + strFileName);
  OperationStatus(LeftFile, RightFile, Item);
  ImageIndexes(strLPath, strRPath, LeftFile, RightFile, Item);
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
  objIcon : TIcon;
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
    objIcon := TIcon.Create;
    Try
      objIcon.Handle := iSIcon;
      Result := ilFileTypeIcons.AddIcon(objIcon);
    Finally
      objIcon.Free;
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

  This is an on execute event handler for the Tools Compare action.

  @precon  None.
  @postcon Opens the 2 selected files in the comparison tool.

  @param   Sender as a TObject

**)
procedure TfrmMainForm.actToolsCompareExecute(Sender: TObject);

Var
  S: TListItem;

begin
  S := lvFileList.Selected;
  ShellExecute(Application.Handle, 'Open', PChar(FCompareEXE),
    PChar(Format('"%s" "%s"', [S.SubItems[iLDisplayCol - 1],
      S.SubItems[iRDisplayCol - 1]])),
    PChar(ExtractFilePath(FCompareEXE)), SW_SHOWNORMAL)
end;

(**

  This is an on update event handler for the Tools Compare action.

  @precon  None.
  @postcon Enables the option only if the EXE exists and the 2 files to compare
           also exist.

  @param   Sender as a TObject

**)
procedure TfrmMainForm.actToolsCompareUpdate(Sender: TObject);

Var
  S: TListItem;

begin
  S := lvFileList.Selected;
  (Sender As TAction).Enabled :=
    (S <> Nil) And
    FileExists(FCompareEXE) And
    FileExists(S.SubItems[iLDisplayCol - 1]) And
    FileExists(S.SubItems[iRDisplayCol - 1]);
end;

(**

  This is an on execute event handler for the Tools Options action.

  @precon  None.
  @postcon Displays the options dialogue for the application.

  @param   Sender as a TObject

**)
procedure TfrmMainForm.actToolsOptionsExecute(Sender: TObject);
begin
  If TfrmOptions.Execute(FFolders, FExclusions, FCompareEXE, FRootKey) Then
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
            foLeftToRight: If Items[i].SubItems[iLDisplayCol - 1] = '' Then Continue;
            foRightToLeft: If Items[i].SubItems[iRDisplayCol - 1] = '' Then Continue;
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
            If SubItems[iRDisplayCol - 1] <> '' Then
              strFileList := strFileList + SubItems[iRFullFileNameCol - 1] + #0;
            If SubItems[iLDisplayCol - 1] <> '' Then
              strFileList := strFileList + SubItems[iLFullFileNameCol - 1] + #0;
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

  This is an exception handling message for the BuildRootKey method.

  @precon  None.
  @postcon Displays the exception message in a dialogue.

  @param   strExceptionMsg as a String

**)
procedure TfrmMainForm.ExceptionProc(strExceptionMsg: String);
begin
  MessageDlg(strExceptionMsg, mtError, [mbOK], 0);
end;

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

  This is an on execute event hanlder for the Help About action.

  @precon  None.
  @postcon Displays the about dialogue.

  @param   Sender as a TObject

**)
procedure TfrmMainForm.actHelpAboutExecute(Sender: TObject);
begin
  TfrmAbout.ShowAbout(FRootKey);
end;

(**

  This is an on execute event handler for the Chekc for updates action.

  @precon  None.
  @postcon Checks for updates on the Internet.

  @param   Sender as a TObject

**)
procedure TfrmMainForm.actHelpCheckForUpdatesExecute(Sender: TObject);
begin
  TCheckForUpdates.Execute(strSoftwareID, FRootKey, Sender = actHelpCheckForUpdates);
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
            strSrcFileList := strSrcFileList + Items[i].SubItems[iLFullFileNameCol - 1] + #0;
            strDestFileList := strDestFileList + Items[i].SubItems[iRFullFileNameCol - 1] + #0;
          End;
        foRightToLeft:
          Begin
            strSrcFileList := strSrcFileList + Items[i].SubItems[iRFullFileNameCol - 1] + #0;
            strDestFileList := strDestFileList + Items[i].SubItems[iLFullFileNameCol - 1] + #0;
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

(**

  This is an on execute event handler for the Select All action.

  @precon  None.
  @postcon Selects all the items in the list.

  @param   Sender as a TObject

**)
procedure TfrmMainForm.actEditSelectAllExecute(Sender: TObject);

Var
  i : Integer;

begin
  With lvFileList Do
    Begin
      Items.BeginUpdate;
      Try
        For i := 0 To Items.Count - 1 Do
          Items[i].Selected := True;
      Finally
        Items.EndUpdate;
      End;
    End;
end;

(**

  This method checks that all the folders exist and provides an option to create
  them if they don't. If it can not find a directory and can not create the
  directory the function returns false else success is indicated by returning
  True.

  @precon  None.
  @postcon Check the existence of the directories in the folders string list.
           Returns true if they exist or are created else returns false.

  @return  a Boolean

**)
function TfrmMainForm.CheckFolders: Boolean;

  (**

    This is a local method to check and confirm the creation of a folder. It
    raises an exception if the folder could not be created.

    @precon  None.
    @postcon Checks and confirms the creation of a folder. It raises an
             exception if the folder could not be created.

    @param   strFolder as a String

  **)
  Procedure CheckAndCreateFolder(strFolder : String);

  Const
    strExcepMsg = 'Could not create the folder "%s".';
    strCreateMsg = 'The folder "%s" does not exist. Would you like to create ' +
      'this folder?';

  Var
    strPath : String;

  Begin
    strPath := ExtractFilePath(strFolder);
    If Not DirectoryExists(strPath) Then
      Case MessageDlg(Format(strCreateMsg, [strPath]), mtConfirmation,
        [mbYes, mbNo, mbCancel], 0) Of
        mrYes:
          If Not ForceDirectories(strPath) Then
            Raise TFolderNotFoundException.CreateFmt(strExcepMsg, [strPath]);
        mrCancel: Abort;
      End;
  End;

Var
  i : Integer;

begin
  For i := 0 To FFolders.Count - 1 Do
    Begin
      CheckAndCreateFolder(FFolders.Names[i]);
      CheckAndCreateFolder(FFolders.Values[FFolders.Names[i]]);
    End;
  Result := True;
end;

(**

  This method updates the operational status of the files based on their date
  and time and also whether they are reasd only or not.

  @precon  None.
  @postcon Updates the operational status of the files based on their date and
           time and also whether they are reasd only or not.

  @param   LeftFile   as a TFileRecord
  @param   RightFile  as a TFileRecord
  @param   Item       as a TListItem as a reference

**)
procedure TfrmMainForm.OperationStatus(LeftFile, RightFile : TFileRecord;
  var Item: TListItem);

begin
  If RightFile <> Nil Then
    Case RightFile.Status Of
      stNewer    : Item.StateIndex := Integer(foRightToLeft);
      stOlder    : Item.StateIndex := Integer(foLeftToRight);
      stSame     : Item.StateIndex := Integer(foNothing);
      stDiffSize : Item.StateIndex := Integer(foNothing);
    End
  Else
    Case LeftFile.Status Of
      stNewer    : Item.StateIndex := Integer(foLeftToRight);
      stOlder    : Item.StateIndex := Integer(foRightToLeft);
      stSame     : Item.StateIndex := Integer(foNothing);
      stDiffSize : Item.StateIndex := Integer(foNothing);
    End;
  Case Item.StateIndex Of
    Integer(foLeftToRight):
      If (Pos('R', Item.SubItems[iLAttrCol - 1]) = 0) And
        (Pos('R', Item.SubItems[iRAttrCol - 1]) > 0) Then
        Item.StateIndex := Integer(foNothing);
    Integer(foRightToLeft):
      If (Pos('R', Item.SubItems[iLAttrCol - 1]) > 0) And
        (Pos('R', Item.SubItems[iRAttrCol - 1]) = 0) Then
        Item.StateIndex := Integer(foNothing);
  End;
end;

(**

  This method updates the status images of the list view based on the filename
  / extension of the file.

  @precon  None.
  @postcon Updates the status images of the list view based on the filename
           / extension of the file.

  @param   strLPath     as a String
  @param   strRPath     as a String
  @param   LeftFile     as a TFileRecord
  @param   RightFile    as a TFileRecord
  @param   Item         as a TListItem

**)
procedure TfrmMainForm.ImageIndexes(strLPath, strRPath : String; LeftFile,
  RightFile : TFileRecord; Item: TListItem);

begin
  if LeftFile <> Nil then
  begin
    if LeftFile <> Nil then
      Item.SubItemImages[iLDisplayCol - 1] := GetImageIndex(strLPath +
        LeftFile.FileName)
    else
      Item.SubItemImages[iLDisplayCol - 1] := -1;
    if RightFile <> Nil then
      Item.SubItemImages[iRDisplayCol - 1] := GetImageIndex(strLPath +
        RightFile.FileName)
    else
      Item.SubItemImages[iRDisplayCol - 1] := -1;
  end
  else
  begin
    if LeftFile <> Nil then
      Item.SubItemImages[iLDisplayCol - 1] := GetImageIndex(strRPath +
        LeftFile.FileName)
    else
      Item.SubItemImages[iLDisplayCol - 1] := -1;
    if RightFile <> Nil then
      Item.SubItemImages[iRDisplayCol - 1] := GetImageIndex(strRPath +
        RightFile.FileName)
    else
      Item.SubItemImages[iRDisplayCol - 1] := -1;
  end;
end;

(**

  This is an application on exception event handler. It displays a dialogue box
  containing the message of the exception.

  @precon  None.
  @postcon Displays the exception message.

  @param   Sender as a TObject
  @param   E      as an Exception

**)
procedure TfrmMainForm.appEventsException(Sender: TObject;
  E: Exception);
begin
  MessageDlg('Exception: ' + E.Message, mtError, [mbOK], 0);
end;

end.
