(**

  This module if a class to represent the applications main form interface.
  This form provide the display of differences between two folders.

  @Version 1.0
  @Date    03 Aug 2012
  @Author  David Hoyle

**)
Unit MainForm;

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
  Menus,
  ActnList,
  ImgList,
  ComCtrls,
  ExtCtrls,
  ToolWin,
  StdCtrls,
  Buttons,
  AppEvnts,
  IniFiles,
  Registry,
  SyncModule,
  ProgressForm,
  OptionsForm,
  XPMan,
  PlatformDefaultStyleActnCtrls,
  ActnMan,
  ActnCtrls,
  ActnMenus,
  DGHCustomGraphicsControl,
  DGHMemoryMonitorControl,
  ComObj,
  ShlObj,
  FileDeleteProgressForm,
  FileCopyProgressForm;

Type
  (** This enumerate determines the type of progress to be shown in the taskbar in
      windows 7. **)
  TTaskbarProgressType = (ptNone, ptNormal, ptError);

  (** This is the class that actually describes for the form interface. **)
  TfrmMainForm = Class(TForm)
    stbrStatusBar: TStatusBar;
    ilActionImages: TImageList;
    actFileExit: TAction;
    actFileCompare: TAction;
    pmFiles: TPopupMenu;
    actToolsOptions: TAction;
    actEditCopyRightToLeft: TAction;
    actEditCopyLeftToRight: TAction;
    lvFileList: TListView;
    actEditDelete: TAction;
    actEditDoNothing: TAction;
    mmiCopyLefttoRight: TMenuItem;
    mmiCopyRighttoLeft: TMenuItem;
    mmiSep1: TMenuItem;
    mmiDeleteBoth: TMenuItem;
    mmiSep2: TMenuItem;
    mmiDoNothing: TMenuItem;
    actFileProcessFiles: TAction;
    actEditSelectAll: TAction;
    mmiSep3: TMenuItem;
    mmiSelectAll: TMenuItem;
    appEvents: TApplicationEvents;
    actHelpAbout: TAction;
    actHelpCheckForUpdates: TAction;
    ilFileTypeIcons: TImageList;
    actToolsCompare: TAction;
    N2: TMenuItem;
    Compare1: TMenuItem;
    XPManifest: TXPManifest;
    amActions: TActionManager;
    ambMenuBar: TActionMainMenuBar;
    atbToolbar: TActionToolBar;
    pnlTop: TPanel;
    DGHMemoryMonitor: TDGHMemoryMonitor;
    splOutputResults: TSplitter;
    redtOutputResults: TRichEdit;
    pnlMainArea: TPanel;
    actEditClearLog: TAction;
    actHelpContents: TAction;
    Procedure actHelpAboutExecute(Sender: TObject);
    Procedure actFileExitExecute(Sender: TObject);
    Procedure FormResize(Sender: TObject);
    Procedure FormCreate(Sender: TObject);
    Procedure FormDestroy(Sender: TObject);
    Procedure actFileCompareExecute(Sender: TObject);
    Procedure actToolsOptionsExecute(Sender: TObject);
    Procedure actEditCopyLeftToRightExecute(Sender: TObject);
    Procedure actEditCopyRightToLeftExecute(Sender: TObject);
    Procedure actEditDeleteExecute(Sender: TObject);
    Procedure actEditDoNothingExecute(Sender: TObject);
    Procedure actFileProcessFilesExecute(Sender: TObject);
    Procedure actEditSelectAllExecute(Sender: TObject);
    Procedure appEventsException(Sender: TObject; E: Exception);
    Procedure lvFileListCustomDrawItem(Sender: TCustomListView; Item: TListItem;
      State: TCustomDrawState; Var DefaultDraw: Boolean);
    Procedure actHelpCheckForUpdatesExecute(Sender: TObject);
    Procedure actToolsCompareExecute(Sender: TObject);
    Procedure actToolsCompareUpdate(Sender: TObject);
    Procedure FormShow(Sender: TObject);
    procedure actEditClearLogExecute(Sender: TObject);
    procedure actHelpContentsExecute(Sender: TObject);
    procedure stbrStatusBarDrawPanel(StatusBar: TStatusBar; Panel: TStatusPanel;
      const Rect: TRect);
  Strict Private
    { Private declarations }
    FFolders        : TStringList;
    FExclusions     : String;
    FIconFiles      : TStringList;
    FProgressForm   : TfrmProgress;
    FRootKey        : String;
    FParams         : TStringList;
    FCompareEXE     : String;
    FFldrSyncOptions: TFldrSyncOptions;
    FAutoProcessing : Boolean;
    FCloseTimer     : TTimer;
    FSyncModule     : TCompareFoldersCollection;
    FProgressSection: Integer;
    FTaskbarList    : ITaskbarList;
    FTaskbarList3   : ITaskbarList3;
    FStartTimer     : TTimer;
    FDeleteForm     : TfrmDeleteProgress;
    FCopyForm       : TfrmCopyProgress;
    Procedure LoadSettings();
    Procedure SaveSettings();
    Procedure ApplicationHint(Sender: TObject);
    Function GetImageIndex(strFileName: String): Integer;
    Procedure FixUpPanes;
    Procedure InsertListItem(strLPath, strRPath: String; LeftFile, RightFile: TFileRecord;
      SyncOptions: TSyncOptions; FileOp: TFileOp);
    Procedure SetFileOperation(FileOp: TFileOp);
    Function CheckFolders: Boolean;
    Procedure ImageIndexes(strLPath, strRPath: String; LeftFile, RightFile: TFileRecord;
      Item: TListItem);
    Procedure ExceptionProc(strExceptionMsg: String);
    Procedure CloseTimerEvent(Sender: TObject);
    Procedure SearchStartProc(strFolder: String);
    Procedure SearchProc(strFolder, strFileName: String; iCount: Integer);
    Procedure SearchEndProc(iFileCount: Integer; iTotalSize: int64);
    Procedure CompareStartProc(strLeftFldr, strRightFldr: String);
    Procedure CompareProc(strLeftFldr, strRightFldr, strFileName: String;
      iPosition, iMaxItems: Integer);
    Procedure CompareEndProc;
    Procedure MatchListStartProc;
    Procedure MatchListProc(iPosition, iMaxItems: Integer);
    Procedure MatchListEndProc;
    Procedure DeleteStartProc(iFileCount: Integer; iTotalSize: int64);
    Procedure DeletingProc(strFileName: String);
    Procedure DeletedProc(iFile: Integer; iSize: int64; boolSuccess: Boolean;
      strErrMsg: String);
    Procedure DeleteQueryProc(strFileName: String; Var Option: TFileAction);
    Procedure DeleteReadOnlyQueryProc(strFileName: String; Var Option: TFileAction);
    Procedure DeleteEndProc(iDeleted, iSkipped, iErrors: Integer);
    Procedure CopyStartProc(iFileCount: Integer; iSize: int64);
    Procedure CopyContentsProc(iCopiedSize, iTotalSize: int64);
    Procedure CopyingProc(strSource, strDest, strFileName: String);
    Procedure CopiedProc(iFile: Integer; iSize: int64; boolSuccess: Boolean;
      strErrMsg: String);
    Procedure CopyQueryProc(strSourceFile, strDestFile: String; Var Option: TFileAction);
    Procedure CopyReadOnlyQueryProc(strSourceFile, strDestFile: String;
      Var Option: TFileAction);
    Procedure CopyEndProc(iCopied, iSkipped, iError: Integer);
    Procedure FileQuery(strMsg, strSource, strDest, strFilename: String;
      Var Option: TFileAction);
    Procedure DiffSizeStart(iFileCount : Integer);
    Procedure DiffSize(strLPath, strRPath, strFileName : String);
    Procedure DiffSizeEnd();
    Procedure NothingToDoStart(iFileCount : Integer);
    Procedure NothingToDo(strLPath, strRPath, strFileName : String);
    Procedure NothingToDoEnd();
    Procedure UpdateTaskBar(ProgressType: TTaskbarProgressType; iPosition: Integer = 0;
      iMaxPosition: Integer = 100);
    Procedure UpdateProgress(iPosition, iMaxPosition: Integer);
    Procedure StartTimerEvent(Sender: TObject);
    Procedure OutputResult(strMsg: String = ''; iColour: TColor = clBlack;
      FontStyles : TFontStyles = []);
    Procedure OutputResultLn(strMsg: String = ''; iColour: TColor = clBlack;
      FontStyles : TFontStyles = []);
    Procedure DisableActions;
    Procedure EnableActions(boolSuccess: Boolean);
    Procedure LogSize;
    Procedure OutputStats;
  End;

  (** This is a custon exception for folders not found or created. **)
  TFolderNotFoundException = Class(Exception);

ResourceString
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

Var
    (** A global variable used by Delphis auto create form process. **)
  frmMainForm: TfrmMainForm;

Implementation

Uses
  FileCtrl,
  ShellAPI,
  About,
  CheckForUpdates,
  DGHLibrary, ConfirmationDlg;

{$R *.DFM}

(**

  This method is an Action List Execute method.

  @precon  None.
  @postcon Closes the application and returns to the OS.

  @param   Sender as a TObject

**)
Procedure TfrmMainForm.actFileExitExecute(Sender: TObject);

Begin
  Close();
End;

(**

  This is a Form on Resize event handler.

  @precon  None.
  @postcon Makes sure that the two sides of the interface are evenly spaced.

  @param   Sender as a TObject

**)
Procedure TfrmMainForm.FormResize(Sender: TObject);

Var
  i: Integer;

Begin
  With lvFileList Do
    Begin
      i := ClientWidth Div 2 - Column[0].Width Div 2 - Column[iLAttrCol].Width -
        Column[iLSizeCol].Width - Column[iLDateCol].Width - 2;
      If i > 0 Then
        Begin
          Column[iLDisplayCol].Width := i;
          Column[iRDisplayCol].Width := i;
        End;
    End;
End;

(**

  This is an on show event handler for the form.

  @precon  None.
  @postcon Starts the start timer to allow the interface 500ms to appear and then start
           processing folder.

  @param   Sender as a TObject

**)
Procedure TfrmMainForm.FormShow(Sender: TObject);

Begin
  FStartTimer.Enabled := True;
End;

(**

  This method loads the applications seetings from the registry.

  @precon  None.
  @postcon Loads the applications settings from the registry.

**)
Procedure TfrmMainForm.LoadSettings;

Var
  sl : TStringList;
  i  : Integer;
  FOA: TFolderOptionsAdapter;
  j  : TFldrSyncOption;
  strLog : String;

Begin
  With TMemIniFile.Create(FRootKey) Do
    Try
      Top                                := ReadInteger('Setup', 'Top', 100);
      Left                               := ReadInteger('Setup', 'Left', 100);
      Height                             := ReadInteger('Setup', 'Height', 300);
      Width                              := ReadInteger('Setup', 'Width', 450);
      lvFileList.Column[iLAttrCol].Width := ReadInteger('ColumnWidths', 'LAttr', 50);
      lvFileList.Column[iLSizeCol].Width := ReadInteger('ColumnWidths', 'LSize', 85);
      lvFileList.Column[iLDateCol].Width := ReadInteger('ColumnWidths', 'LDAte', 150);
      lvFileList.Column[iRAttrCol].Width := ReadInteger('ColumnWidths', 'RAttr', 50);
      lvFileList.Column[iRSizeCol].Width := ReadInteger('ColumnWidths', 'RSize', 85);
      lvFileList.Column[iRDateCol].Width := ReadInteger('ColumnWidths', 'RDate', 150);
      lvFileList.Font.Name               := ReadString('ListViewFont', 'Name', 'Tahoma');
      lvFileList.Font.Size               := ReadInteger('ListViewFont', 'Size', 9);
      lvFileList.Font.Style := TFontStyles(Byte(ReadInteger('ListViewFont', 'Style', 0)));
      redtOutputResults.Font.Name        := ReadString('LogFont', 'Name', 'Courier New');
      redtOutputResults.Font.Size        := ReadInteger('LogFont', 'Size', 10);
      strLog := ChangeFileExt(FRootKey, '_log.rtf');
      If FileExists(strLog) Then
        redtOutputResults.Lines.LoadFromFile(strLog);
      LogSize;
      WindowState := TWindowState(ReadInteger('Setup', 'WindowState', Byte(wsNormal)));
      redtOutputResults.Height := ReadInteger('Setup', 'OutputResultsHeight', 100);
      FCompareEXE      := ReadString('Setup', 'CompareEXE', '');
      FFldrSyncOptions := [];
      For j            := Low(TFldrSyncOption) To High(TFldrSyncOption) Do
        If ReadBool(strFldrSyncOptions[j].FINISection, strFldrSyncOptions[j].FINIKey,
          strFldrSyncOptions[j].fDefault) Then
          Include(FFldrSyncOptions, j);
      sl := TStringList.Create;
      Try
        ReadSection('Folders', sl);
        For i := 0 To sl.Count - 1 Do
          Begin
            FOA.FRAWData     := 0;
            FOA.FSyncOptions := [soEnabled];
            FOA.FSyncOptions :=
              TSyncOptions(Byte(ReadInteger('FolderStatus', sl[i],
                  Byte(FOA.FSyncOptions))));
            FFolders.AddObject(sl[i] + '=' + ReadString('Folders', sl[i], ''),
              FOA.FOBjData);
          End;
      Finally
        sl.Free;
      End;
      FExclusions := StringReplace(ReadString('Setup', 'Exclusions', ''), '|', #13#10,
        [rfReplaceAll]);
    Finally
      Free;
    End;
End;

(**

  This methiohd updates the status bar with the current log size.

  @precon  None.
  @postcon The status bar is updated with the current log size.

**)
Procedure TfrmMainForm.LogSize;

Begin
  stbrStatusBar.Panels[0].Text := Format('Log size: %1.1n kbytes',
    [Int(Length(redtOutputResults.Text)) / 1024.0]);
  With stbrStatusBar Do
    Panels[0].Width := Canvas.TextWidth(Panels[0].Text) + 25;
End;

(**

  This is an CustomDrawItem event for the list view.

  @precon  None.
  @postcon Draw readonly items in the list with a light red background.

  @param   Sender      as a TCustomListView
  @param   Item        as a TListItem
  @param   State       as a TCustomDrawState
  @param   DefaultDraw as a Boolean as a reference

**)
Procedure TfrmMainForm.lvFileListCustomDrawItem(Sender: TCustomListView; Item: TListItem;
  State: TCustomDrawState; Var DefaultDraw: Boolean);

Type
  TBackground = (bgLeft, bgRight);

Var
  iSubItem  : Integer;
  R, ItemR  : TRect;
  Buffer    : Array [0 .. MAX_PATH * 2] Of Char;
  Ops       : Integer;
  iBufferLen: Integer;

  (**

    This function returns display rectangle for the given indexed sub item.

    @precon  iIndex must be a valid SubItem index..
    @postcon Returns display rectangle for the given indexed sub item.

    @param   iIndex as an Integer
    @return  a TRect

  **)
  Function GetSubItemRect(iIndex: Integer): TRect;

  Var
    j: Integer;

  Begin
    Result := Item.DisplayRect(drBounds);
    For j  := 0 To iIndex Do
      Begin
        Inc(Result.Left, Sender.Column[j].Width);
        Result.Right := Result.Left + Sender.Column[j + 1].Width;
      End;
    Inc(Result.Top, 2);    // Padding / Margin
    Inc(Result.Bottom, 2); // Padding / Margin
    Inc(Result.Left, 6);   // Padding / Margin
    Dec(Result.Right, 6);  // Padding / Margin
  End;

  (**

    This method draws the background for the left and right displays of file
    informations.

    @precon  None.
    @postcon Draws the background for the left and right displays of file
             informations.

    @param   iColumn    as an Integer
    @param   Background as a TBackground

  **)
  Procedure DrawBackground(iColumn: Integer; Background: TBackground);

  Begin
    Sender.Canvas.Brush.Color := clWindow;
    If Item.Selected Then
      Sender.Canvas.Brush.Color := clHighlight
    Else
      Begin
        If (Pos('R', Item.SubItems[iColumn - 1]) > 0) Then
          Sender.Canvas.Brush.Color := $BBBBFF
        Else If TFileOp(Item.StateIndex) = foNothing Then
          Sender.Canvas.Brush.Color := clSilver;
      End;
    If Background = bgLeft Then
      R := GetSubItemRect(iLDateCol - 1)
    Else
      R   := GetSubItemRect(iRDisplayCol - 1);
    ItemR := Item.DisplayRect(drBounds);
    If Background = bgLeft Then
      ItemR.Right := R.Right + 6
    Else
      ItemR.Left := R.Left - 6;
    Sender.Canvas.FillRect(ItemR);
  End;

  (**

    This procedure sets the font colour and style attributes depending on the
    status of the list item.

    @precon  None.
    @postcon Sets the font colour and style attributes depending on the
    status of the list item.

  **)
  Procedure SetTextAttributes;

  Begin
    Sender.Canvas.Font.Color := clWindowText;
    If TFileOp(Item.StateIndex) = foNothing Then
      Sender.Canvas.Font.Color := clGray;
    If TFileOp(Item.StateIndex) = foDelete Then
      Sender.Canvas.Font.Style := Sender.Canvas.Font.Style + [fsStrikeout];
    If Item.Selected Then
      Sender.Canvas.Font.Color := clHighlightText;
  End;

  (**

    This procedure sets the text drawing options (alignment, etc) for the
    different columns to be displayed.

    @precon  None.
    @postcon Sets the text drawing options (alignment, etc) for the
    different columns to be displayed.

  **)
  Procedure SetTextDrawingOptions;

  Begin
    Case iSubItem Of
      0, 4:
        Ops := DT_LEFT Or DT_MODIFYSTRING Or DT_PATH_ELLIPSIS Or DT_NOPREFIX;
      2, 3, 6, 7:
        Ops := DT_RIGHT;
    Else
      Ops := DT_LEFT;
    End;
  End;

  (**

    This procedure returns the display rectangle (adjusted for file icon  image)
    for the text to be displayed in the report column.

    @precon  None.
    @postcon Returns the display rectangle (adjusted for file icon  image)
    for the text to be displayed in the report column.

    @return  a TRect

  **)
  Function DrawFileIcon: TRect;

  Var
    iImage     : Integer;
    boolEnabled: Boolean;

  Begin
    Result := GetSubItemRect(iSubItem);
    If iSubItem In [0, 4] Then
      Begin
        Dec(Result.Left, 3);
        iImage      := Item.SubItemImages[iSubItem];
        boolEnabled := (iImage > -1);
        If Not boolEnabled Then
          If iSubItem = 0 Then
            iImage := Item.SubItemImages[4]
          Else
            iImage := Item.SubItemImages[0];
        ilFileTypeIcons.Draw(Sender.Canvas, Result.Left, Result.Top, iImage, boolEnabled);
        Inc(Result.Left, 16 + 3);
      End;
  End;

  (**

    This procedure sets the text background for the columns of information.

    @precon  None.
    @postcon Sets the text background for the columns of information.

  **)
  Procedure SetTextFontBackground;

  Begin
    Sender.Canvas.Brush.Color := clWindow;
    If Item.Selected Then
      Sender.Canvas.Brush.Color := clHighlight
    Else
      Begin
        If iSubItem In [iLDisplayCol - 1 .. iLDateCol - 1] Then
          Begin
            If (Pos('R', Item.SubItems[iLAttrCol - 1]) > 0) Then
              Sender.Canvas.Brush.Color := $BBBBFF
            Else If TFileOp(Item.StateIndex) = foNothing Then
              Sender.Canvas.Brush.Color := clSilver;
          End
        Else
          Begin
            If (Pos('R', Item.SubItems[iRAttrCol - 1]) > 0) Then
              Sender.Canvas.Brush.Color := $BBBBFF
            Else If TFileOp(Item.StateIndex) = foNothing Then
              Sender.Canvas.Brush.Color := clSilver;
          End;
      End;
  End;

  (**

    This procedure displays a grayed out destination path for files which don`t
    have a destination file, else sets up the font colours and style attributes
    for displaying the text.

    @precon  None.
    @postcon Displays a grayed out destination path for files which don`t
    have a destination file, else sets up the font colours and style
    attributes for displaying the text.

  **)
  Procedure FixUpEmptyFilePaths;

  Begin
    If (iSubItem = 0) And (Item.SubItems[iSubItem] = '') Then
      Begin
        StrPCopy(Buffer, Item.SubItems[iLFullFileNameCol - 1]);
        iBufferLen               := Length(Item.SubItems[iLFullFileNameCol - 1]);
        Sender.Canvas.Font.Color := clGray;
      End
    Else If (iSubItem = 4) And (Item.SubItems[iSubItem] = '') Then
      Begin
        StrPCopy(Buffer, Item.SubItems[iRFullFileNameCol - 1]);
        iBufferLen               := Length(Item.SubItems[iRFullFileNameCol - 1]);
        Sender.Canvas.Font.Color := clGray;
      End
    Else
      SetTextAttributes;
  End;

Begin
  DefaultDraw := False;
  DrawBackground(iLAttrCol, bgLeft);
  DrawBackground(iRAttrCol, bgRight);
  R := Item.DisplayRect(drBounds);
  ilActionImages.Draw(Sender.Canvas, R.Left + lvFileList.Column[0].Width Div 2 - 8, R.Top,
    Item.StateIndex, True);
  For iSubItem := 0 To iRDateCol - 1 Do
    Begin
      SetTextDrawingOptions;
      R := DrawFileIcon;
      SetTextFontBackground;
      StrPCopy(Buffer, Item.SubItems[iSubItem]);
      iBufferLen := Length(Item.SubItems[iSubItem]);
      FixUpEmptyFilePaths;
      Sender.Canvas.Refresh;
      DrawText(Sender.Canvas.Handle, Buffer, iBufferLen, R, Ops);
    End;
End;

(**

  This is an on match list end event handler for the sync module.

  @precon  None.
  @postcon None.

**)
Procedure TfrmMainForm.MatchListEndProc;

Begin
  OutputResultLn(' Done!', clGreen);
  FProgressForm.Progress(FProgressSection, 100, 'Done!', '');
End;

(**

  This is an on match list event handler for the sync module.

  @precon  None.
  @postcon Updates the progress dialogue with the progress through the matching process.

  @param   iPosition as an Integer
  @param   iMaxItems as an Integer

**)
Procedure TfrmMainForm.MatchListProc(iPosition, iMaxItems: Integer);

Begin
  If iPosition Mod 10 = 0 Then
    FProgressForm.Progress(FProgressSection, Trunc(iPosition / iMaxItems * 100.0),
      Format('Matching items... %1.0n in %1.0n', [Int(iPosition), Int(iMaxItems)]), '');
End;

(**

  This is an on match list start event handler for the sync module.

  @precon  None.
  @postcon Increments the progress sections and initialises the dialogue for progress.

**)
Procedure TfrmMainForm.MatchListStartProc;

Begin
  OutputResult('Matching files...');
  Inc(FProgressSection);
  FProgressForm.InitialiseSection(FProgressSection, 1, 100);
End;

(**

  This is an on Nothing To Do event handler for the sync module.

  @precon  None.
  @postcon Outputs the name of the files that have a difference size but the same date
           and time.

  @param   strLPath    as a String
  @param   strRPath    as a String
  @param   strFileName as a String

**)
procedure TfrmMainForm.NothingToDo(strLPath, strRPath, strFileName: String);

begin
  OutputResultLn(Format('  %s => %s%s', [strLPath, strRPath, strFileName]));
end;

(**

  This is an on nothingToDo end event handler for the sync module.

  @precon  None.
  @postcon None.

**)
procedure TfrmMainForm.NothingToDoEnd;

begin
  // Do nothing.
end;

(**

  This is a Nothing To Do Start event handler for the sync module.

  @precon  None.
  @postcon Displays a header is there are files to output.

  @param   iFileCount as an Integer

**)
procedure TfrmMainForm.NothingToDoStart(iFileCount: Integer);

begin
  If iFileCount > 0 Then
    OutputResultLn(Format('There are %1.0n files marked as Do Nothing...',
      [Int(iFileCount)]), clMaroon);
end;

(**

  This method outputs the given message to the output window with the optional colour but
  without a line feed and carraige return.

  @precon  None.
  @postcon The messages is output to the end of output window.

  @param   strMsg     as a String
  @param   iColour    as a TColor
  @param   FontStyles as a TFontStyles

**)
Procedure TfrmMainForm.OutputResult(strMsg: String = ''; iColour: TColor = clBlack;
  FontStyles : TFontStyles = []);

Begin
  redtOutputResults.SelLength           := 0;
  redtOutputResults.SelStart            := Length(redtOutputResults.Text);
  redtOutputResults.SelAttributes.Color := iColour;
  redtOutputResults.SelAttributes.Style := FontStyles;
  redtOutputResults.SelText             := strMsg;
  LogSize;
End;

(**

  This method outputs the given message to the output window with the optional colour but
  WITH a line feed and carraige return.

  @precon  None.
  @postcon The messages is output to the end of output window.

  @param   strMsg     as a String
  @param   iColour    as a TColor
  @param   FontStyles as a TFontStyles

**)
Procedure TfrmMainForm.OutputResultLn(strMsg: String = ''; iColour: TColor = clBlack;
  FontStyles : TFontStyles = []);

Begin
  OutputResult(strMsg + #13#10, iColour, FontStyles);
End;

(**

  This method adds panels to the status bar for each of the statistics returned from the
  sync module.

  @precon  None.
  @postcon Panels are added to the status bar for each available statistic.

**)
Procedure TfrmMainForm.OutputStats;

Var
  i: Integer;
  P: TStatusPanel;

Begin
  stbrStatusBar.Panels.BeginUpdate;
  Try
    While stbrStatusBar.Panels.Count > 1 Do
      stbrStatusBar.Panels.Delete(stbrStatusBar.Panels.Count - 1);
    FSyncModule.BuildStats;
    For i := 0 To FSyncModule.Statistics.Count - 1 Do
      Begin
        P := stbrStatusBar.Panels.Add;
        P.Text := FSyncModule.Statistics[i];
        P.Width := stbrStatusBar.Canvas.TextWidth(P.Text) + 25;
        P.Style := psOwnerDraw;
      End;
  Finally
    stbrStatusBar.Panels.EndUpdate;
  End;
End;

(**

  This method saves the applications seetings to the registry.

  @precon  None.
  @postcon Save the applications settings to the registry.

**)
Procedure TfrmMainForm.SaveSettings;

Var
  i         : Integer;
  recWndPlmt: TWindowPlacement;
  j         : TFldrSyncOption;
  FOA       : TFolderOptionsAdapter;

Begin
  With TMemIniFile.Create(FRootKey) Do
    Try
      recWndPlmt.Length := SizeOf(TWindowPlacement);
      GetWindowPlacement(Handle, @recWndPlmt);
      WriteInteger('Setup', 'Top', recWndPlmt.rcNormalPosition.Top);
      WriteInteger('Setup', 'Left', recWndPlmt.rcNormalPosition.Left);
      WriteInteger('Setup', 'Height', recWndPlmt.rcNormalPosition.Bottom -
          recWndPlmt.rcNormalPosition.Top);
      WriteInteger('Setup', 'Width', recWndPlmt.rcNormalPosition.Right -
          recWndPlmt.rcNormalPosition.Left);
      WriteInteger('ColumnWidths', 'LAttr', lvFileList.Column[iLAttrCol].Width);
      WriteInteger('ColumnWidths', 'LSize', lvFileList.Column[iLSizeCol].Width);
      WriteInteger('ColumnWidths', 'LDAte', lvFileList.Column[iLDateCol].Width);
      WriteInteger('ColumnWidths', 'RAttr', lvFileList.Column[iRAttrCol].Width);
      WriteInteger('ColumnWidths', 'RSize', lvFileList.Column[iRSizeCol].Width);
      WriteInteger('ColumnWidths', 'RDate', lvFileList.Column[iRDateCol].Width);
      WriteString('ListViewFont', 'Name', lvFileList.Font.Name);
      WriteInteger('ListViewFont', 'Size', lvFileList.Font.Size);
      WriteInteger('ListViewFont', 'Style', Byte(lvFileList.Font.Style));
      WriteString('LogFont', 'Name', redtOutputResults.Font.Name);
      WriteInteger('LogFont', 'Size', redtOutputResults.Font.Size);
      redtOutputResults.Lines.SaveToFile(ChangeFileExt(FRootKey, '_log.rtf'));
      WriteInteger('Setup', 'WindowState', Byte(WindowState));
      WriteInteger('Setup', 'OutputResultsHeight', redtOutputResults.Height);
      WriteString('Setup', 'CompareEXE', FCompareEXE);
      For j := Low(TFldrSyncOption) To High(TFldrSyncOption) Do
        WriteBool(strFldrSyncOptions[j].FINISection, strFldrSyncOptions[j].FINIKey,
          j In FFldrSyncOptions);
      EraseSection('Folders');
      For i := 0 To FFolders.Count - 1 Do
        Begin
          WriteString('Folders', FFolders.Names[i], FFolders.ValueFromIndex[i]);
          FOA.FOBjData := FFolders.Objects[i];
          WriteInteger('FolderStatus', FFolders.Names[i], Byte(FOA.FSyncOptions));
        End;
      WriteString('Setup', 'Exclusions', StringReplace(FExclusions, #13#10, '|',
          [rfReplaceAll]));
      UpdateFile;
    Finally
      Free;
    End;
End;

(**

  This is the forms on create event handler.

  @precon  None.
  @postcon Creates various objects for the life time of the application and
           loads the applications settings from the registry.

  @param   Sender as a TObject

**)
Procedure TfrmMainForm.FormCreate(Sender: TObject);

Begin
  FParams                         := TStringList.Create;
  FCloseTimer                     := TTimer.Create(Nil);
  FCloseTimer.Enabled             := False;
  FCloseTimer.Interval            := 2000;
  FCloseTimer.OnTimer             := CloseTimerEvent;
  FStartTimer                     := TTimer.Create(Nil);
  FStartTimer.Enabled             := False;
  FStartTimer.Interval            := 1000;
  FStartTimer.OnTimer             := StartTimerEvent;
  DGHMemoryMonitor.HighPoint      := 100;
  DGHMemoryMonitor.HalfPoint      := 90;
  DGHMemoryMonitor.LowPoint       := 80;
  DGHMemoryMonitor.UpdateInterval := 500;
  FRootKey                        := BuildRootKey(FParams, ExceptionProc);
  TfrmAbout.ShowAbout;
  actHelpCheckForUpdatesExecute(Nil);
  FFolders                       := TStringList.Create;
  FProgressForm                  := TfrmProgress.Create(Self);
  FProgressForm.OnUpdateProgress := UpdateProgress;
  Application.HelpFile := ChangeFileExt(ParamStr(0), '.chm');
  LoadSettings();
  Application.OnHint                := ApplicationHint;
  FIconFiles                        := TStringList.Create;
  FIconFiles.Sorted                 := True;
  Caption                           := Format('%s: %s', [Caption, FRootKey]);
  FSyncModule                       := TCompareFoldersCollection.Create;
  FSyncModule.OnSearchStart         := SearchStartProc;
  FSyncModule.OnSearch              := SearchProc;
  FSyncModule.OnSearchEnd           := SearchEndProc;
  FSyncModule.OnCompareStart        := CompareStartProc;
  FSyncModule.OnCompare             := CompareProc;
  FSyncModule.OnCompareEnd          := CompareEndProc;
  FSyncModule.OnMatchListStart      := MatchListStartProc;
  FSyncModule.OnMatchList           := MatchListProc;
  FSyncModule.OnMatchListEnd        := MatchListEndProc;
  FSyncModule.OnDeleteStart         := DeleteStartProc;
  FSyncModule.OnDeleting            := DeletingProc;
  FSyncModule.OnDeleted             := DeletedProc;
  FSyncModule.OnDeleteQuery         := DeleteQueryProc;
  FSyncModule.OnDeleteReadOnlyQuery := DeleteReadOnlyQueryProc;
  FSyncModule.OnDeleteEnd           := DeleteEndProc;
  FSyncModule.OnCopyStart           := CopyStartProc;
  FSyncModule.OnCopying             := CopyingProc;
  FSyncModule.OnCopyContents        := CopyContentsProc;
  FSyncModule.OnCopied              := CopiedProc;
  FSyncModule.OnCopyQuery           := CopyQueryProc;
  FSyncModule.OnCopyReadOnlyQuery   := CopyReadOnlyQueryProc;
  FSyncModule.OnCopyEnd             := CopyEndProc;
  FSyncModule.OnDiffSizeStart := DiffSizeStart;
  FSyncModule.OnDiffSize := DiffSize;
  FSyncModule.OnDiffSizeEnd := DiffSizeEnd;
  FSyncModule.OnNothingToDoStart := NothingToDoStart;
  FSyncModule.OnNothingToDo := NothingToDo;
  FSyncModule.OnNothingToDoEnd := NothingToDoEnd;
  FTaskbarList  := Nil;
  FTaskbarList3 := Nil;
  If CheckWin32Version(6, 1) Then
    Begin
      FTaskbarList := CreateComObject(CLSID_TaskbarList) As ITaskbarList;
      FTaskbarList.HrInit;
      Supports(FTaskbarList, IID_ITaskbarList3, FTaskbarList3);
    End;
End;

(**

  This is the forms on destroy event handler.

  @precon  None.
  @postcon Save the applications settings and free memory used by various
           objects.

  @param   Sender as a TObject

**)
Procedure TfrmMainForm.FormDestroy(Sender: TObject);

Begin
  SaveSettings();
  FSyncModule.Free;
  FIconFiles.Free;
  FProgressForm.Free;
  FFolders.Free;
  FCloseTimer.Free;
  FParams.Free;
End;

(**

  This is the applications on hint event handler.

  @precon  None.
  @postcon Updates the application status bar when menu to toolbar items are
           used.

  @param   Sender as a TObject

**)
Procedure TfrmMainForm.ApplicationHint(Sender: TObject);

Begin
  stbrStatusBar.SimplePanel := Application.Hint <> '';
  If Application.Hint <> '' Then
    stbrStatusBar.SimpleText := GetLongHint(Application.Hint);
End;

(**

  This is an on execute method for the File Compare action.

  @precon  None.
  @postcon Start the process of comparing files and folders and output the
           results in the list view.

  @param   Sender as a TObject

**)
Procedure TfrmMainForm.actFileCompareExecute(Sender: TObject);

Var
  iEnabledFolders: Integer;
  i              : Integer;
  FOA            : TFolderOptionsAdapter;
  boolSuccess    : Boolean;

Begin
  OutputResultLn('Comparison started @ ' + FormatDateTime('dddd dd mmmm yyyy hh:mm:ss',
    Now()), clBlue, [fsBold, fsUnderline]);
  boolSuccess := False;
  If Not CheckFolders Then
    Exit;
  Try
    DisableActions;
    Try
      iEnabledFolders := 0;
      For i           := 0 To FFolders.Count - 1 Do
        Begin
          FOA.FOBjData := FFolders.Objects[i];
          If soEnabled In FOA.FSyncOptions Then
            Inc(iEnabledFolders);
        End;
      FProgressForm.RegisterSections(iEnabledFolders * 3 + 2);
      FProgressSection := -1;
      lvFileList.Items.BeginUpdate;
      Try
        lvFileList.Clear;
      Finally
        lvFileList.Items.EndUpdate;
      End;
      Try
        FSyncModule.Clear;
        boolSuccess := FSyncModule.ProcessFolders(FFolders, FExclusions);
        FixUpPanes;
        FormResize(Sender);
        If (fsoCloseIFNoFilesAfterComparison In FFldrSyncOptions) And
          (lvFileList.Items.Count = 0) Then
          Begin
            FProgressForm.Progress(FProgressSection, 0, 'Auto-exiting application...',
              'as there are no more files to process...');
            FCloseTimer.Enabled := True;
          End;
        If (fsoStartProcessingAutomatically In FFldrSyncOptions) And
          Not FAutoProcessing Then
          Try
            FAutoProcessing := True;
            actFileProcessFilesExecute(Sender);
          Finally
            FAutoProcessing := False;
          End;
      Finally
        UpdateTaskBar(ptNone);
        If Not FCloseTimer.Enabled Then
          FProgressForm.Hide;
        OutputStats;
      End;
    Finally
      EnableActions(boolSuccess);
    End;
  Finally
    OutputResultLn();
  End;
End;

(**

  This method outputs the results of the various folder comparisons into the list view for
  actioning.

  @precon  None.
  @postcon Outputs the results of the various folder comparisons into the list view for
           actioning.

**)
Procedure TfrmMainForm.FixUpPanes;

Var
  iProcessItem: Integer;
  P           : TProcessItem;

Begin
  Inc(FProgressSection);
  FProgressForm.InitialiseSection(FProgressSection, 0, FSyncModule.ProcessCount);
  lvFileList.Items.BeginUpdate;
  Try
    For iProcessItem := 0 To FSyncModule.ProcessCount - 1 Do
      Begin
        P := FSyncModule.Process[iProcessItem];
        InsertListItem(P.LPath, P.RPath, P.LeftFile, P.RightFile, P.SyncOptions,
          P.FileOp);
        If iProcessItem Mod 10 = 0 Then
          FProgressForm.Progress(FProgressSection, iProcessItem,
            Format('Building ListView... %1.0n in %1.0n', [Int(iProcessItem),
                Int(FSyncModule.ProcessCount)]), '');
      End;
  Finally
    lvFileList.Items.EndUpdate;
  End;
End;

(**

  This method adds a pair of filenames with sizes and dates to the list view.

  @precon  None.
  @postcon Adds a pair of filenames with sizes and dates to the list view.

  @param   strLPath    as a String
  @param   strRPath    as a String
  @param   LeftFile    as a TFileRecord
  @param   RightFile   as a TFileRecord
  @param   SyncOptions as a TSyncOptions
  @param   FileOp      as a TFileOp

**)
Procedure TfrmMainForm.InsertListItem(strLPath, strRPath: String;
  LeftFile, RightFile: TFileRecord; SyncOptions: TSyncOptions; FileOp: TFileOp);

  (**

    This method returns a string representation of a files attrivutes [RASH].

    @precon  None.
    @postcon Returns a string representation of a files attrivutes [RASH].

    @param   iAttr as an Integer
    @return  a String

  **)
  Function GetAttributeString(iAttr: Integer): String;

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
  Item       : TListItem;
  strFileName: String;

Begin
  Item            := lvFileList.Items.Add;
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
    End
  Else
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
    End
  Else
    Begin
      Item.SubItems.Add('');
      Item.SubItems.Add('');
      Item.SubItems.Add('');
      Item.SubItems.Add('');
    End;
  Item.SubItems.Add(strLPath + strFileName);
  Item.SubItems.Add(strRPath + strFileName);
  Item.StateIndex := Integer(FileOp);
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
Function TfrmMainForm.GetImageIndex(strFileName: String): Integer;

Var
  strExt        : String;
  strClassName  : String;
  strFName      : String;
  iPos          : Integer;
  iIndex        : Integer;
  iIcon         : Word;
  szIconFile    : PChar;
  objIcon       : TIcon;
  iLIcon, iSIcon: HICON;
  strIndex      : String;
  strExpanded   : String;
  iOutputSize   : Integer;

Begin
  strExt := LowerCase(ExtractFileExt(strFileName));
  // Get Icon information from Registry
  strClassName := GetRegStringValue(strExt, '');
  If FIconFiles.Find(strClassName, iIndex) Then
    Begin
      Result := Integer(FIconFiles.Objects[iIndex]);
      Exit;
    End;
  If strClassName <> '' Then
    Begin
      strFName := GetRegStringValue(strExt + '\DefaultIcon', '');
      If strFName = '' Then
        strFName := GetRegStringValue(strClassName + '\DefaultIcon', '');
      If strFName = '' Then
        Begin
          strFName := GetRegStringValue(strClassName + '\shell\open\command', '');
          strFName := StringReplace(strFName, ' "%1"', '', [rfReplaceAll]);
          strFName := StringReplace(strFName, ' %1', '', [rfReplaceAll]);
          strFName := strFName + ',0';
        End;
      SetLength(strExpanded, 1024);
      iOutputSize := ExpandEnvironmentStrings(PChar(strFName), PChar(strExpanded), 1024);
      SetLength(strExpanded, iOutputSize - 1);
      strFName := strExpanded;
    End;
  iPos := Pos(',', strFName);
  If iPos <> 0 Then
    Begin
      strIndex := Copy(strFName, iPos + 1, Length(strFName) - iPos);
      If strIndex <> '' Then
        If strIndex[Length(strIndex)] = '"' Then
          strIndex := Copy(strIndex, 1, Length(strIndex) - 1);
      iIndex       := StrToInt(strIndex);
      strFName     := Copy(strFName, 1, iPos - 1);
      ExtractIconEx(PChar(strFName), iIndex, iLIcon, iSIcon, 1);
    End;
  // If the registry technique fail to get an icon use the ExtractAssociatedIcon
  // function instead
  If iSIcon = 0 Then
    Begin
      iIcon      := 0;
      szIconFile := StrAlloc(1024);
      Try
        StrCopy(szIconFile, PChar(strFileName));
        iSIcon := ExtractAssociatedIcon(Handle, szIconFile, iIcon);
      Finally
        StrDispose(szIconFile);
      End;
    End;
  objIcon := TIcon.Create;
  Try
    objIcon.Handle := iSIcon;
    Result         := ilFileTypeIcons.AddIcon(objIcon);
  Finally
    objIcon.Free;
  End;
  If (strClassName <> '') And (strExt <> '.exe') Then
    FIconFiles.AddObject(strClassName, TObject(Result));
End;

(**

  This is an on execute event handler for the Tools Compare action.

  @precon  None.
  @postcon Opens the 2 selected files in the comparison tool.

  @param   Sender as a TObject

**)
Procedure TfrmMainForm.actToolsCompareExecute(Sender: TObject);

Var
  S: TListItem;

Begin
  S := lvFileList.Selected;
  ShellExecute(Application.Handle, 'Open', PChar(FCompareEXE),
    PChar(Format('"%s" "%s"', [S.SubItems[iLDisplayCol - 1], S.SubItems[iRDisplayCol - 1]]
        )), PChar(ExtractFilePath(FCompareEXE)), SW_SHOWNORMAL)
End;

(**

  This is an on update event handler for the Tools Compare action.

  @precon  None.
  @postcon Enables the option only if the EXE exists and the 2 files to compare
           also exist.

  @param   Sender as a TObject

**)
Procedure TfrmMainForm.actToolsCompareUpdate(Sender: TObject);

Var
  S: TListItem;

Begin
  S                           := lvFileList.Selected;
  (Sender As TAction).Enabled := (S <> Nil) And FileExists(FCompareEXE) And
    FileExists(S.SubItems[iLDisplayCol - 1]) And FileExists(S.SubItems[iRDisplayCol - 1]);
End;

(**

  This is an on execute event handler for the Tools Options action.

  @precon  None.
  @postcon Displays the options dialogue for the application.

  @param   Sender as a TObject

**)
Procedure TfrmMainForm.actToolsOptionsExecute(Sender: TObject);

Begin
  If TfrmOptions.Execute(FFolders, FExclusions, FCompareEXE, FRootKey, lvFileList.Font,
    redtOutputResults.Font, FFldrSyncOptions) Then
    actFileCompareExecute(Self);
End;

(**

  This is an on execute event handler for the Clear Log action.

  @precon  None.
  @postcon Clears the log.

  @param   Sender as a TObject

**)
procedure TfrmMainForm.actEditClearLogExecute(Sender: TObject);

begin
  redtOutputResults.Clear;
end;

(**

  This is an on execute event handler for the CopyLeftToRight Action.

  @precon  None.
  @postcon Sets the selected items in the list view to be Copied from Left to
           Right.

  @param   Sender as a TObject

**)
Procedure TfrmMainForm.actEditCopyLeftToRightExecute(Sender: TObject);

Begin
  SetFileOperation(foLeftToRight);
End;

(**

  This is an on search end event handler for the sync module.

  @precon  None.
  @postcon Outputs the results of the search.

  @param   iFileCount as an Integer
  @param   iTotalSize as an int64

**)
Procedure TfrmMainForm.SearchEndProc(iFileCount: Integer; iTotalSize: int64);

Begin
  FProgressForm.Progress(FProgressSection, 1, 'Done!', '');
  OutputResultLn(Format('... Found %1.0n files (%1.0n bytes)',
      [Int(iFileCount), Int(iTotalSize)]), clGreen);
End;

(**

  This is an on search event handler for the sync module.

  @precon  None.
  @postcon Updates the progress dialogue with the current number of files found and
           the current folder and filename being searched.

  @param   strFolder   as a String
  @param   strFileName as a String
  @param   iCount      as an Integer

**)
Procedure TfrmMainForm.SearchProc(strFolder, strFileName: String; iCount: Integer);

Begin
  If iCount Mod 10 = 0 Then
    FProgressForm.Progress(FProgressSection, 0, Format('Searching: %s... %1.0n',
        [strFolder, Int(iCount)]), strFileName);
End;

(**

  This is an on search start event handler for the sync module.

  @precon  None.
  @postcon Increments the progress section and initialises the progress bar.

  @param   strFolder as a String

**)
Procedure TfrmMainForm.SearchStartProc(strFolder: String);

Begin
  OutputResult(Format('Search: %s...', [strFolder]));
  Inc(FProgressSection);
  FProgressForm.InitialiseSection(FProgressSection, 0, 1);
End;

(**

  This method sets the file operations for the selected list view items.

  @precon  None.
  @postcon Sets the selected items in the list view to the File op state passed
           while also checking that the operation is valid for that item.

  @param   FileOp as a TFileOp

**)
Procedure TfrmMainForm.SetFileOperation(FileOp: TFileOp);

Var
  i: Integer;

Begin
  For i := 0 To lvFileList.Items.Count - 1 Do
    Begin
      If lvFileList.Items[i].Selected Then
        Begin
          Case FileOp Of
            foLeftToRight:
              If lvFileList.Items[i].SubItems[iLDisplayCol - 1] = '' Then
                Continue;
            foRightToLeft:
              If lvFileList.Items[i].SubItems[iRDisplayCol - 1] = '' Then
                Continue;
          End;
          lvFileList.Items[i].StateIndex := Integer(FileOp);
          {$IFDEF DEBUG}
          Assert(FSyncModule.Process[i].FileOp = FileOp,
            'Synchronisation Error in Process List.');
          {$ENDIF}
          FSyncModule.Process[i].FileOp := FileOp;
        End;
    End;
  OutputStats;
End;

(**

  This is an on timer event handler for the start timer of the application.

  @precon  None.
  @postcon The start timer is stopped and the comparison of the folders is started.

  @param   Sender as a TObject

**)
Procedure TfrmMainForm.StartTimerEvent(Sender: TObject);

Begin
  FStartTimer.Enabled := False;
  actFileCompareExecute(Self);
End;

(**

  This is an on draw panel event handler for the status bar.

  @precon  None.
  @postcon Renders the Panels with bold text for the title and the rest as panel text.

  @param   StatusBar as a TStatusBar
  @param   Panel     as a TStatusPanel
  @param   Rect      as a TRect as a constant

**)
Procedure TfrmMainForm.stbrStatusBarDrawPanel(StatusBar: TStatusBar; Panel: TStatusPanel;
  Const Rect: TRect);

Var
  R : TRect;
  iPos:  Integer;
  strText : String;
  iColour: TColor;

Begin
  iPos := Pos(':', Panel.Text);
  strText := Copy(Panel.Text, 1, iPos);
  R := Rect;
  iColour := StatusBar.Canvas.Font.Color;
  StatusBar.Canvas.Font.Color := clMaroon;
  StatusBar.Canvas.Font.Style := [fsBold];
  StatusBar.Canvas.TextRect(R, strText, [tfLeft, tfVerticalCenter]);
  Inc(R.Left, StatusBar.Canvas.TextWidth(strText) + 2);
  StatusBar.Canvas.Font.Style := [];
  strText := Copy(Panel.Text, iPos + 1, Length(Panel.Text) - iPos);
  StatusBar.Canvas.Font.Color := iColour;
  StatusBar.Canvas.TextRect(R, strText, [tfLeft, tfVerticalCenter]);
End;

(**

  This is an on update progress event handler for the application.

  @precon  None.
  @postcon Updates the applications task bar with the current progress position.

  @param   iPosition    as an Integer
  @param   iMaxPosition as an Integer

**)
Procedure TfrmMainForm.UpdateProgress(iPosition, iMaxPosition: Integer);

Begin
  UpdateTaskBar(ptNormal, iPosition, iMaxPosition);
End;

(**

  This method updates the windows taskbar progress in windows 7 and above. The progress
  of the bar is only updated IF the progress type is normal.

  @precon  None.
  @postcon The progress bar on the applications taskbar is updated.

  @param   ProgressType as a TTaskbarProgressType
  @param   iPosition    as an Integer
  @param   iMaxPosition as an Integer

**)
Procedure TfrmMainForm.UpdateTaskBar(ProgressType: TTaskbarProgressType;
  iPosition: Integer = 0; iMaxPosition: Integer = 100);

Const
  ProgressTypeValues: Array [Low(TTaskbarProgressType) .. High(TTaskbarProgressType)
    ] Of Cardinal = (TBPF_NOPROGRESS, TBPF_NORMAL, TBPF_ERROR);

Begin
  If Assigned(FTaskbarList3) Then
    Begin
      FTaskbarList3.SetProgressState(Application.Handle,
        ProgressTypeValues[ProgressType]);
      If ProgressType In [ptNormal] Then
        FTaskbarList3.SetProgressValue(Application.Handle, iPosition, iMaxPosition);
    End;
End;

(**

  This is an on execute event handler for the CopyRightToLeft action.

  @precon  None.
  @postcon Sets the selected items in the list view to be Copies from Right
           to Left.

  @param   Sender as a TObject

**)
Procedure TfrmMainForm.actEditCopyRightToLeftExecute(Sender: TObject);

Begin
  SetFileOperation(foRightToLeft);
End;

(**

  This is an on execute event handler for the Delete action.

  @precon  None.
  @postcon Sets the selected items in the list view to be Deleted.

  @param   Sender as a TObject

**)
Procedure TfrmMainForm.actEditDeleteExecute(Sender: TObject);

Begin
  SetFileOperation(foDelete);
End;

(**

  This is an on execute event handler for the DoNothing action.

  @precon  None.
  @postcon Sets the selected items in the listview to DoNothing.

  @param   Sender as a TObject

**)
Procedure TfrmMainForm.actEditDoNothingExecute(Sender: TObject);

Begin
  SetFileOperation(foNothing);
End;

(**

  This method enables all actions.

  @precon  None.
  @postcon All actions are enabled.

  @param   boolSuccess as a Boolean

**)
Procedure TfrmMainForm.EnableActions(boolSuccess: Boolean);

Var
  i: Integer;
  A: TAction;

Begin
  For i := 0 To ComponentCount - 1 Do
    If Components[i] Is TAction Then
      Begin
        A := Components[i] As TAction;
        If boolSuccess Then
          A.Enabled := True
        Else If A.Name <> 'actFileProcessFiles' Then
          A.Enabled := True;
      End;
End;

(**

  This is an exception handling message for the BuildRootKey method.

  @precon  None.
  @postcon Displays the exception message in a dialogue.

  @param   strExceptionMsg as a String

**)
Procedure TfrmMainForm.ExceptionProc(strExceptionMsg: String);

Begin
  MessageDlg(strExceptionMsg, mtError, [mbOK], 0);
End;

(**

  This a an on execute event handler for the Process Files action.

  @precon  None.
  @postcon Firstly, it deletes any files need to be deleted from the list, then
           copies any files that need to be copied and finally refreshes the
           information in the list view.

  @param   Sender as a TObject

**)
Procedure TfrmMainForm.actFileProcessFilesExecute(Sender: TObject);

Begin
  OutputResultLn('Processing started @ ' + FormatDateTime('dddd dd mmmm yyyy hh:mm:ss',
    Now()), clBlue, [fsBold, fsUnderline]);
  Try
    DisableActions;
    Try
      FSyncModule.ProcessFiles;
    Finally
      EnableActions(True);
    End;
  Finally
    OutputResultLn();
  End;
  actFileCompareExecute(Self);
End;

(**

  This is an on execute event hanlder for the Help About action.

  @precon  None.
  @postcon Displays the about dialogue.

  @param   Sender as a TObject

**)
Procedure TfrmMainForm.actHelpAboutExecute(Sender: TObject);

Begin
  TfrmAbout.ShowAbout;
End;

(**

  This is an on execute event handler for the Chekc for updates action.

  @precon  None.
  @postcon Checks for updates on the Internet.

  @param   Sender as a TObject

**)
Procedure TfrmMainForm.actHelpCheckForUpdatesExecute(Sender: TObject);

Begin
  TCheckForUpdates.Execute(strSoftwareID, FRootKey, Sender = actHelpCheckForUpdates);
End;

(**

  This is an on execute event handler for the Help Contents action.

  @precon  None.
  @postcon Displays the Contents and Welcome page of the HTML Help.

  @param   Sender as a TObject

**)
Procedure TfrmMainForm.actHelpContentsExecute(Sender: TObject);

Begin
  Application.HelpShowTableOfContents;
End;

(**

  This is an on execute event handler for the Select All action.

  @precon  None.
  @postcon Selects all the items in the list.

  @param   Sender as a TObject

**)
Procedure TfrmMainForm.actEditSelectAllExecute(Sender: TObject);

Var
  i: Integer;

Begin
  With lvFileList Do
    Begin
      Items.BeginUpdate;
      Try
        For i               := 0 To Items.Count - 1 Do
          Items[i].Selected := True;
      Finally
        Items.EndUpdate;
      End;
    End;
End;

(**

  This method checks that all the folders exist and provides an option to create
  them if they don`t. If it can not find a directory and can not create the
  directory the function returns false else success is indicated by returning
  True.

  @precon  None.
  @postcon Check the existence of the directories in the folders string list.
           Returns true if they exist or are created else returns false.

  @return  a Boolean

**)
Function TfrmMainForm.CheckFolders: Boolean;

  (**

    This is a local method to check and confirm the creation of a folder. It
    raises an exception if the folder could not be created.

    @precon  None.
    @postcon Checks and confirms the creation of a folder. It raises an
             exception if the folder could not be created.

    @param   strFolder as a String

  **)
  Procedure CheckAndCreateFolder(strFolder: String);

  Const
    strExcepMsg  = 'Could not create the folder "%s".';
    strCreateMsg = 'The folder "%s" does not exist. Would you like to create ' +
      'this folder?';

  Var
    strPath: String;

  Begin
    strPath := ExtractFilePath(strFolder);
    If Not SysUtils.DirectoryExists(strPath) Then
      Case MessageDlg(Format(strCreateMsg, [strPath]), mtConfirmation,
        [mbYes, mbNo, mbCancel], 0) Of
        mrYes:
          If Not SysUtils.ForceDirectories(strPath) Then
            Raise TFolderNotFoundException.CreateFmt(strExcepMsg, [strPath]);
        mrCancel:
          Abort;
      End;
  End;

Var
  i: Integer;

Begin
  For i := 0 To FFolders.Count - 1 Do
    If Boolean(FFolders.Objects[i]) Then
      Begin
        CheckAndCreateFolder(FFolders.Names[i]);
        CheckAndCreateFolder(FFolders.ValueFromIndex[i]);
      End;
  Result := True;
End;

(**

  This is an on timer event handler.

  @precon  None.
  @postcon Closes the application.

  @param   Sender as a TObject

**)
Procedure TfrmMainForm.CloseTimerEvent(Sender: TObject);

Begin
  Close;
End;

(**

  This is an on compare end event handler for the sync module.

  @precon  None.
  @postcon None.

**)
Procedure TfrmMainForm.CompareEndProc;

Begin
  FProgressForm.Progress(FProgressSection, 100, 'Done!', '');
  OutputResultLn('Done!', clGreen);
End;

(**

  This is an on compare event handler for the sync module.

  @precon  None.
  @postcon Updates the progress dialogue with the current progress through the comparison
           process.

  @param   strLeftFldr  as a String
  @param   strRightFldr as a String
  @param   strFileName  as a String
  @param   iPosition    as an Integer
  @param   iMaxItems    as an Integer

**)
Procedure TfrmMainForm.CompareProc(strLeftFldr, strRightFldr, strFileName: String;
  iPosition, iMaxItems: Integer);

Begin
  If iPosition Mod 10 = 0 Then
    FProgressForm.Progress(FProgressSection, Trunc(iPosition / iMaxItems * 100.0),
      Format('Comparing... %1.0n in %1.0n', [Int(iPosition), Int(iMaxItems)]),
      strFileName);
End;

(**

  This is an on compare start event handler for the sync module.

  @precon  None.
  @postcon Increments the progress section and initialises the progress dialogue.

  @param   strLeftFldr  as a String
  @param   strRightFldr as a String

**)
Procedure TfrmMainForm.CompareStartProc(strLeftFldr, strRightFldr: String);

Begin
  OutputResult(Format('Comparing %s to %s...', [strLeftFldr, strRightFldr]));
  Inc(FProgressSection);
  FProgressForm.InitialiseSection(FProgressSection, 0, 100);
End;

(**

  This is an on copied event handler for the sync module.

  @precon  None.
  @postcon Updates the copy progress or if an error outputs the error message.

  @param   iFile       as an Integer
  @param   iSize       as an int64
  @param   boolSuccess as a Boolean
  @param   strErrMsg   as a String

**)
Procedure TfrmMainForm.CopiedProc(iFile: Integer; iSize: int64; boolSuccess: Boolean;
  strErrMsg: String);

Begin
  If boolSuccess Then
    Begin
      FCopyForm.Progress(iSize, iSize);
      FCopyForm.ProgressOverall(iSize);
      OutputResultLn();
    End
  Else
    OutputResultLn(#13#10#32#32 + strErrMsg, clRed, [fsBold]);
End;

(**

  This is an on copy contents event handler for the sync module.

  @precon  None.
  @postcon Updates the progress of the individual file copy.

  @param   iCopiedSize as an Int64
  @param   iTotalSize  as an Int64

**)
Procedure TfrmMainForm.CopyContentsProc(iCopiedSize, iTotalSize: int64);

Begin
  FCopyForm.Progress(iCopiedSize, iTotalSize);
End;

(**

  This is an on copy end event handler for the sync Module.

  @precon  None.
  @postcon Outputs the number of files copied, skipped, etc and frees the copy dialogue.

  @param   iCopied  as an Integer
  @param   iSkipped as an Integer
  @param   iError   as an Integer

**)
Procedure TfrmMainForm.CopyEndProc(iCopied, iSkipped, iError: Integer);

Begin
  OutputResult(Format('...Copied %1.0n (Skipped %1.0n', [Int(iCopied), Int(iSkipped)]),
    clGreen);
  If iError > 0 Then
    OutputResult(Format(', Errored %1.0n', [Int(iError)]), clRed);
  OutputResultLn(')', clGreen);
  FreeAndNil(FCopyForm);
End;

(**

  This is an on copying event handler for the sync module.

  @precon  None.
  @postcon Updates the copy dialogue.

  @param   strSource   as a String
  @param   strDest     as a String
  @param   strFileName as a String

**)
Procedure TfrmMainForm.CopyingProc(strSource, strDest, strFileName: String);

Begin
  FCopyForm.Progress(ExtractFilePath(strSource + strFileName),
    ExtractFilePath(strDest + strFileName), ExtractFileName(strFileName));
  OutputResult(Format('  %s => %s%s', [strSource, strDest, strFilename]));
End;

(**

  This is an on copy query event handler for the sync module.

  @precon  None.
  @postcon Displays a confirmation dialogue to the user to ask whether the file should be
           overwritten.

  @param   strSourceFile as a String
  @param   strDestFile   as a String
  @param   Option        as a TFileAction as a reference

**)
Procedure TfrmMainForm.CopyQueryProc(strSourceFile, strDestFile: String;
  Var Option: TFileAction);

Const
  strMsg = 'Are you sure you want to overwrite the following file?';

Begin
  FileQuery(strMsg, ExtractFilePath(strSourceFile), ExtractFilePath(strDestFile),
    ExtractFileName(strDestFile), Option);
End;

(**

  This is an on copy read only query event handler for the sync module.

  @precon  None.
  @postcon Displays a confirmation dialogue to the user to ask whether the read only file
           should be overwritten.

  @param   strSourceFile as a String
  @param   strDestFile   as a String
  @param   Option        as a TFileAction as a reference

**)
Procedure TfrmMainForm.CopyReadOnlyQueryProc(strSourceFile, strDestFile: String;
  Var Option: TFileAction);

Const
  strMsg = 'Are you sure you want to overwrite the following READ-ONLY file?';

Begin
  FileQuery(strMsg, ExtractFilePath(strSourceFile), ExtractFilePath(strDestFile),
        ExtractFileName(strDestFile), Option);
End;

(**

  This is an on copy start event handler for the sync module.

  @precon  None.
  @postcon Initialises the copy progress dialogue.

  @param   iFileCount as an Integer
  @param   iSize      as an int64

**)
Procedure TfrmMainForm.CopyStartProc(iFileCount: Integer; iSize: int64);

Begin
  OutputResultLn(Format('Copying %1.0n files (%1.0n bytes)...',
      [Int(iFileCount), Int(iSize)]));
  If iFileCount > 0 Then
    Begin
      FCopyForm                  := TfrmCopyProgress.Create(Nil);
      FCopyForm.OnUpdateProgress := UpdateProgress;
      FCopyForm.Initialise(iSize);
    End;
End;

(**

  This method displays a dialogue asking the user to confirm the file action they wish to
  take based on the message displayed. The answer to the dialogue is returned to the sync
  module.

  @precon  None.
  @postcon The users response to the query is passed back to the sync module.

  @param   strMsg      as a String
  @param   strSource   as a String
  @param   strDest     as a String
  @param   strFilename as a String
  @param   Option      as a TFileAction as a reference

**)
Procedure TfrmMainForm.FileQuery(strMsg, strSource, strDest, strFilename: String;
  Var Option: TFileAction);

Begin
  If Option <> faAll Then
    Begin
      UpdateTaskBar(ptError, 0, 1);
      Case TfrmConfirmationDlg.Execute(strMsg, strSource, strDest, strFilename) Of
        mrYes:
          Option := faYes;
        mrNo:
          Option := faNo;
        mrAll:
          Option := faAll;
      Else
        Option := faCancel;
      End;
    End;
End;

(**

  This is an on deleted event handler for the sync module.

  @precon  None.
  @postcon Updates the progress dialogue is successful or outputs an error message.

  @param   iFile       as an Integer
  @param   iSize       as an int64
  @param   boolSuccess as a Boolean
  @param   strErrMsg   as a String

**)
Procedure TfrmMainForm.DeletedProc(iFile: Integer; iSize: int64; boolSuccess: Boolean;
  strErrMsg: String);

Begin
  If boolSuccess Then
    Begin
      FDeleteForm.Progress(iSize);
      OutputResultLn();
    End Else
      OutputResultLn(#13#10#32#32 + strErrMsg, clRed, [fsBold]);
End;

(**

  This is an on delete end event handler for the sync module.

  @precon  None.
  @postcon Frees the deletion progress form.

  @param   iDeleted as an Integer
  @param   iSkipped as an Integer
  @param   iErrors  as an Integer

**)
Procedure TfrmMainForm.DeleteEndProc(iDeleted, iSkipped, iErrors: Integer);

Begin
  OutputResult(Format('...Deleted %1.0n (Skipped %1.0n', [Int(iDeleted), Int(iSkipped)]),
    clGreen);
  If iErrors > 0 Then
    OutputResult(Format(', Errored %1.0n', [Int(iErrors)]), clRed);
  OutputResultLn(')', clGreen);
  FreeAndNil(FDeleteForm);
End;

(**

  This method is an on delete query event handler for the sync module.

  @precon  None.
  @postcon Displays a confirmation dialogue to the user asking whether the file should be
           deleted.

  @param   strFileName as a String
  @param   Option      as a TFileAction as a reference

**)
Procedure TfrmMainForm.DeleteQueryProc(strFileName: String; Var Option: TFileAction);

Const
  strMsg = 'Are you sure you want to delete the file?';

Begin
  FileQuery(strMsg, ExtractFilePath(strFileName), '', ExtractFileName(strFileName),
    Option);
End;

(**

  This method is an on delete readonly query event handler for the sync module.

  @precon  None.
  @postcon Displays a confirmation dialogue to the user asking whether the read only file
           should be deleted.

  @param   strFileName as a String
  @param   Option      as a TFileAction as a reference

**)
Procedure TfrmMainForm.DeleteReadOnlyQueryProc(strFileName: String;
  Var Option: TFileAction);

Const
  strMsg = 'Are you sure you want to delete the READ-ONLY file?';

Begin
  FileQuery(strMsg, ExtractFilePath(strFileName), '', ExtractFileName(strFileName),
    Option);
End;

(**

  This is an on delete start event handler for the sync module.

  @precon  None.
  @postcon Creates an instance of the deletion progress form.

  @param   iFileCount as an Integer
  @param   iTotalSize as an Int64

**)
Procedure TfrmMainForm.DeleteStartProc(iFileCount: Integer; iTotalSize: int64);

Begin
  OutputResultLn(Format('Deleting %1.0n files (%1.0n bytes)...',
      [Int(iFileCount), Int(iTotalSize)]));
  If iFileCount > 0 Then
    Begin
      FDeleteForm                  := TfrmDeleteProgress.Create(Nil);
      FDeleteForm.OnUpdateProgress := UpdateProgress;
      FDeleteForm.Initialise(iTotalSize);
    End;
End;

(**

  This is an on delete event handler for the sync module.

  @precon  None.
  @postcon Output the file to be deleted to the deletion progress form.

  @param   strFileName as a String

**)
Procedure TfrmMainForm.DeletingProc(strFileName: String);

Begin
  FDeleteForm.FileName := strFileName;
  OutputResult(#32#32 + strFileName);
End;

(**

  This is an on diff size event handler for the sync module.

  @precon  None.
  @postcon Outputs the name of the files that have a difference size but the same date
           and time.

  @param   strLPath    as a String
  @param   strRPath    as a String
  @param   strFileName as a String

**)
procedure TfrmMainForm.DiffSize(strLPath, strRPath, strFileName: String);

begin
  OutputResultLn(Format('  %s => %s%s', [strLPath, strRPath, strFileName]));
end;

(**

  This is an on diff size end event handler for the sync module.

  @precon  None.
  @postcon None.

**)
procedure TfrmMainForm.DiffSizeEnd;

begin
  // Do nothing
end;

(**

  This is a Diff Size Start event handler for the sync module.

  @precon  None.
  @postcon Displays a header is there are files to output.

  @param   iFileCount as an Integer

**)
procedure TfrmMainForm.DiffSizeStart(iFileCount: Integer);

begin
  If iFileCount > 0 Then
    OutputResultLn(Format('There are %1.0n file(s) with a size difference (same date)...',
      [Int(iFileCount)]), clMaroon);
end;

(**

  This method disables all actions.

  @precon  None.
  @postcon All actions are disabled.

**)
Procedure TfrmMainForm.DisableActions;

Var
  i: Integer;

Begin
  For i := 0 To ComponentCount - 1 Do
    If Components[i] Is TAction Then
      (Components[i] As TAction).Enabled := False;
End;

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
Procedure TfrmMainForm.ImageIndexes(strLPath, strRPath: String;
  LeftFile, RightFile: TFileRecord; Item: TListItem);

Begin
  If LeftFile <> Nil Then
    Begin
      If LeftFile <> Nil Then
        Item.SubItemImages[iLDisplayCol - 1] :=
          GetImageIndex(strLPath + LeftFile.FileName)
      Else
        Item.SubItemImages[iLDisplayCol - 1] := -1;
      If RightFile <> Nil Then
        Item.SubItemImages[iRDisplayCol - 1] :=
          GetImageIndex(strLPath + RightFile.FileName)
      Else
        Item.SubItemImages[iRDisplayCol - 1] := -1;
    End
  Else
    Begin
      If LeftFile <> Nil Then
        Item.SubItemImages[iLDisplayCol - 1] :=
          GetImageIndex(strRPath + LeftFile.FileName)
      Else
        Item.SubItemImages[iLDisplayCol - 1] := -1;
      If RightFile <> Nil Then
        Item.SubItemImages[iRDisplayCol - 1] :=
          GetImageIndex(strRPath + RightFile.FileName)
      Else
        Item.SubItemImages[iRDisplayCol - 1] := -1;
    End;
End;

(**

  This is an application on exception event handler. It displays a dialogue box
  containing the message of the exception.

  @precon  None.
  @postcon Displays the exception message.

  @param   Sender as a TObject
  @param   E      as an Exception

**)
Procedure TfrmMainForm.appEventsException(Sender: TObject; E: Exception);

Begin
  MessageDlg('Exception: ' + E.Message, mtError, [mbOK], 0);
End;

End.
