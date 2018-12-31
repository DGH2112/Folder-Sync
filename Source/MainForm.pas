(**

  This module if a class to represent the applications main form interface.
  This form provide the display of differences between two folders.

  @Version 2.0
  @Date    31 Dec 2018
  @Author  David Hoyle

  @nocheck HardCodedInteger HardCodedString HardCodedNumber UnsortedModule
  @nometrics

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
  FileCopyProgressForm,
  DiskSpaceForm,
  System.Actions, System.ImageList, VirtualTrees;

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
    amActions: TActionManager;
    ambMenuBar: TActionMainMenuBar;
    atbToolbar: TActionToolBar;
    pnlTop: TPanel;
    DGHMemoryMonitor: TDGHMemoryMonitor;
    splOutputResults: TSplitter;
    pnlMainArea: TPanel;
    actEditClearLog: TAction;
    actHelpContents: TAction;
    redtOutputResults: TMemo;
    actToolsConfigMemMon: TAction;
    Splitter: TSplitter;
    vstFileList: TVirtualStringTree;
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
    procedure actEditClearLogExecute(Sender: TObject);
    procedure actHelpContentsExecute(Sender: TObject);
    procedure stbrStatusBarDrawPanel(StatusBar: TStatusBar; Panel: TStatusPanel;
      const Rect: TRect);
    procedure actEditFileOperationsUpdate(Sender: TObject);
    procedure actToolsConfigMemMonExecute(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure vstFileListFreeNode(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure FormShow(Sender: TObject);
    procedure vstFileListGetText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex;
      TextType: TVSTTextType; var CellText: string);
    procedure vstFileListGetImageIndex(Sender: TBaseVirtualTree; Node: PVirtualNode; Kind: TVTImageKind;
      Column: TColumnIndex; var Ghosted: Boolean; var ImageIndex: TImageIndex);
  Strict Private
    Type
      (** A record to describe the data stored in the File List nodes of the VTV control. **)
      TFSFileListNode = Record
        FProcessIndex      : Integer;   
        FState             : TFileOp;
        FLeftFilename      : String;
        FLeftAttr          : Cardinal;
        FLeftSize          : Int64;
        FLeftDate          : TDateTime;
        FLeftFileIndex     : INteger;
        FRightFilename     : String;
        FRightAttr         : Cardinal;
        FRightSize         : Int64;
        FRightDate         : TDateTime;
        FRightFileIndex     : INteger;
        FLeftFullFileName  : String;
        FRightFullFileName : String;
      End;
      (** A poiner to be above file list node structure **)
      PFSFileListNode = ^TFSFileListNode;
  Strict Private
    FFolders         : TFolders;
    FExclusions      : String;
    FIconFiles       : TStringList;
    FProgressForm    : TfrmProgress;
    FRootKey         : String;
    FParams          : TStringList;
    FCompareEXE      : String;
    FFldrSyncOptions : TFldrSyncOptions;
    FAutoProcessing  : Boolean;
    FCloseTimer      : TTimer;
    FSyncModule      : TCompareFoldersCollection;
    FProgressSection : Integer;
    FTaskbarList     : ITaskbarList;
    FTaskbarList3    : ITaskbarList3;
    FStartTimer      : TTimer;
    FDeleteForm      : TfrmDeleteProgress;
    FCopyForm        : TfrmCopyProgress;
    FDialogueBottom  : Integer;
    FInterfaceFonts  : TInterfaceFontInfo;
    FFileOpFonts     : TFileOperationFontInfo;
    FLastFolder      : String;
    FTotalSize       : Int64;
    FFileOpStats     : TFileOpStats;
    FCopyDlgWidth   : Integer;
    FDeleteDlgWidth  : Integer;
    FConfirmDlgWidth : Integer;
    Procedure LoadSettings();
    Procedure SaveSettings();
    Procedure UpgradeINIFolderOptions(iniMemFile : TMemIniFile);
    Procedure ApplicationHint(Sender: TObject);
    Function GetImageIndex(ProcessItem : TProcessItem; strFileName: String): NativeInt;
    Procedure FixUpPanes;
    Procedure InsertListItem(Const iProcessItem : Integer; Const ProcessItem : TProcessItem);
    Procedure SetFileOperation(Const FileOp: TFileOp);
    Function CheckFolders: Boolean;
    Procedure ImageIndexes(Const ProcessItem : TProcessItem; Const NodeData : PFSFileListNode);
    Procedure ExceptionProc(strExceptionMsg: String);
    Procedure CloseTimerEvent(Sender: TObject);
    Procedure SearchStartProc(strFolder: String);
    Procedure SearchProc(strFolder, strFileName: String; iCount: Integer;
      Update : TUpdateType);
    Procedure SearchEndProc(iFileCount: Integer; iTotalSize: int64);
    Procedure CompareStartProc(strLeftFldr, strRightFldr: String);
    Procedure CompareProc(strLeftFldr, strRightFldr, strFileName: String;
      iPosition, iMaxItems: Integer);
    Procedure CompareEndProc;
    Procedure MatchListStartProc;
    Procedure MatchListProc(iPosition, iMaxItems: Integer);
    Procedure MatchListEndProc;
    Procedure DeleteStartProc(iFileCount: Integer; iTotalSize: int64);
    Procedure DeletingProc(iCurrentFileToDelete, iTotalFilesToDelete : Integer;
      iCumulativeFileSizeBeforeDelete, iTotalFileSizeToDelete: Int64;
      strDeletePath, strFileNameToDelete: String);
    Procedure DeletedProc(iCurrentFileToDeleted, iTotalFilesToDelete: Integer;
      iCumulativeFileSizeAfterDelete, iTotalFileSizeToDelete: Int64;
      iSuccess : TProcessSuccess);
    Procedure DeleteQueryProc(strFilePath: String; DeleteFile : TFileRecord;
      Var Option: TFileAction);
    Procedure DeleteReadOnlyQueryProc(strFilePath: String; DeleteFile : TFileRecord;
      Var Option: TFileAction);
    Procedure DeleteEndProc(iDeleted, iSkipped, iErrors: Integer);
    Procedure CopyStartProc(iTotalCount: Integer; iTotalSize: int64);
    Procedure CopyContentsProc(iCurrentFileToCopy, iTotalFilesToCopy : Integer;
      iCumulativeFileSizeBeforeCopy, iTotalFileSizeToCopy, iCurrentFileCopiedSizeSoFar,
      iTotalCurrentFileSize: Int64);
    Procedure CopyingProc(iCurrentFileToCopy, iTotalFilesToCopy : Integer;
      iCumulativeFileSizeBeforeCopy, iTotalFileSizeToCopy: Int64; strSource, strDest,
      strFileName: String);
    Procedure CopiedProc(iCurrentFileToCopy, iTotalFilesToCopy: Integer;
      iCumulativeFileSizeAfterCopy, iTotalFileSizeToCopy: Int64;
      iSuccess : TProcessSuccess);
    Procedure CopyQueryProc(strSourcePath, strDestPath: String; SourceFile,
      DestFile : TFileRecord; Var Option: TFileAction);
    Procedure CopyReadOnlyQueryProc(strSourcePath, strDestPath: String; SourceFile,
      DestFile : TFileRecord;  Var Option: TFileAction);
    Procedure CopyEndProc(iCopied, iSkipped, iError: Integer);
    Procedure FileQuery(strMsg, strConsoleMsg, strSourcePath, strDestPath: String;
      SourceFile, DestFile : TFileRecord; Var Option: TFileAction; boolReadOnly : Boolean);
    Procedure DiffSizeStart(iFileCount : Integer);
    Procedure DiffSize(iFile, iFileCount: Integer; strLPath, strRPath,
      strFileName: String);
    Procedure DiffSizeEnd();
    Procedure NothingToDoStart(iFileCount : Integer);
    Procedure NothingToDo(iFile, iFileCount: Integer; strLPath, strRPath,
    strFileName: String);
    Procedure NothingToDoEnd();
    Procedure ExceedsSizeLimitStart(iFileCount : Integer);
    Procedure ExceedsSizeLimit(iFile, iFileCount: Integer; strLPath, strRPath,
      strFileName: String);
    Procedure ExceedsSizeLimitEnd();
    Procedure ErrorMessageStart(iFileCount : Integer);
    Procedure ErrorMessage(strErrorMsg : String);
    Procedure ErrorMessageEnd();
    Procedure DeleteFoldersStart(iFolderCount : Integer);
    Procedure DeleteFolders(iFolder, iFolders : Integer; strFolder : String);
    Procedure DeleteFoldersEnd();
    Procedure CopyError(strSource, strDest, strErrorMsg : String;
      iLastError : Cardinal; var iResult : TDGHErrorResult);
    Procedure DeleteError(strSource, strErrorMsg : String;
      iLastError : Cardinal; var iResult : TDGHErrorResult);
    Procedure UpdateTaskBar(ProgressType: TTaskbarProgressType; iPosition: Integer = 0;
      iMaxPosition: Integer = 100);
    Procedure UpdateProgress(iPosition, iMaxPosition: Integer);
    Procedure StartTimerEvent(Sender: TObject);
    Procedure OutputResult(strMsg: String = '');
    Procedure OutputResultLn(strMsg: String = '');
    Procedure DisableActions;
    Procedure EnableActions(boolSuccess: Boolean);
    Procedure LogSize;
    Procedure OutputStats;
    Procedure MemoryPopupMenu(Sender: TObject; MousePos: TPoint; var Handled: Boolean);
    Procedure OutputFileNumber(iCurrentFile, iTotal : Integer);
  End;

  (** This is a custon exception for folders not found or created. **)
  TFolderNotFoundException = Class(Exception);

  (** An enumerate to define the Virtual string tree fields of data. **)
  TFSVTVFields = (
    vfState,
    vfLeftFileName,
    vfLeftAttr,
    vfLeftSize,
    vfLeftDate,
    vfRightFilename,
    vfRightAttr,
    vfRightSize,
    vfRightDate
  );

ResourceString
    (** A resource string for the Update Software ID **)
  strSoftwareID = 'FldrSync';

Var
    (** A global variable used by Delphis auto create form process. **)
  frmMainForm: TfrmMainForm;

Implementation

Uses
  CodeSiteLogging,
  {$IFDEF PROFILECODE}
  Profiler,
  {$ENDIF}
  FileCtrl,
  ShellAPI,
  AboutForm,
  CheckForUpdates,
  DGHLibrary,
  ConfirmationDlg,
  Themes,
  MemoryMonitorOptionsForm,
  ProcessingErrorForm,
  {$IFDEF EUREKALOG_VER7}
  ExceptionLog7,
  EExceptionManager,
  {$ENDIF}
  UITypes,
  Types,
  ApplicationFunctions,
  TypInfo,
  StrUtils;

{$R *.DFM}

Type
  (** A record for convert an Int64 into Word portions for storage in the INI file
      (INI file can only store signed DWORDS) **)
  TInt64Ex = Record 
    Case Integer Of
      1: (Value : Int64);
      2: (
        iFirst  : Word;
        iSecond : Word;
        iThird  : Word;
        iFourth : Word;
      );
  End;

(**

  This method returns a string representation of a files attrivutes [RASH].

  @precon  None.
  @postcon Returns a string representation of a files attrivutes [RASH].

  @param   iAttr as an Cardinal
  @return  a String

**)
Function GetAttributeString(iAttr: Cardinal): String;

Begin
  Result := '....'; //: @todo Add full set of attributes
  If iAttr And faReadOnly > 0 Then
    Result[1] := 'R';
  If iAttr And faArchive > 0 Then
    Result[2] := 'A';
  If iAttr And faSysFile > 0 Then
    Result[3] := 'S';
  If iAttr And faHidden > 0 Then
    Result[4] := 'H';
End;

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
  H: TVTHeader;
  i: Integer;

Begin
  H := vstFileList.Header;
  i := vstFileList.ClientWidth Div 2
     - H.Columns[0].Width Div 2
     - H.Columns[Integer(vfLeftAttr)].Width
     - H.Columns[Integer(vfLeftSize)].Width
     - H.Columns[Integer(vfLeftDate)].Width
     - 2;
  If i > 0 Then
    Begin
      H.Columns[Integer(vfLeftFileName)].Width := i;
      H.Columns[Integer(vfRightFilename)].Width := i;
    End;
End;

(**

  This is an on show event handler for the form.

  @precon  None.
  @postcon Starts the start time.

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
  j  : TFldrSyncOption;
  k : TFileOpFont;
  strLog : String;
  iSyncOptions : TSyncOptions;
  strMaxValue : String;
  iMaxValue : TInt64Ex;
  strLeftFldr, strRightFldr : String;
  iniMemFile : TMemIniFile;
  iDefaultOps : TFileOpStats;
  ComOps: TCommandLineOptionsRec;

Begin
  iniMemFile := TMemIniFile.Create(FRootKey);
  Try
    TStyleManager.SetStyle(iniMemFile.ReadString('Setup', 'Theme',
      TStyleManager.ActiveStyle.Name));
    Top := iniMemFile.ReadInteger('Setup', 'Top', 100);
    Left := iniMemFile.ReadInteger('Setup', 'Left', 100);
    Height := iniMemFile.ReadInteger('Setup', 'Height', 300);
    Width := iniMemFile.ReadInteger('Setup', 'Width', 450);
    vstFileList.Header.Columns[Integer(vfLeftAttr)].Width := iniMemFile.ReadInteger('ColumnWidths', 'LAttr', 50);
    vstFileList.Header.Columns[Integer(vfLeftSize)].Width := iniMemFile.ReadInteger('ColumnWidths', 'LSize', 85);
    vstFileList.Header.Columns[Integer(vfLeftDate)].Width := iniMemFile.ReadInteger('ColumnWidths', 'LDAte', 150);
    vstFileList.Header.Columns[Integer(vfRightAttr)].Width := iniMemFile.ReadInteger('ColumnWidths', 'RAttr', 50);
    vstFileList.Header.Columns[Integer(vfRightSize)].Width := iniMemFile.ReadInteger('ColumnWidths', 'RSize', 85);
    vstFileList.Header.Columns[Integer(vfRightDate)].Width := iniMemFile.ReadInteger('ColumnWidths', 'RDate', 150);
    FInterfaceFonts[ifTableFont].FFontName :=
      iniMemFile.ReadString('ListViewFont', 'Name', InterfaceFontDefaults[ifTableFont].FFontName);
    vstFileList.Font.Name               := FInterfaceFonts[ifTableFont].FFontName;
    FInterfaceFonts[ifTableFont].FFontSize :=
      iniMemFile.ReadInteger('ListViewFont', 'Size', InterfaceFontDefaults[ifTableFont].FFontSize);
    vstFileList.Font.Size               := FInterfaceFonts[ifTableFont].FFontSize;
    FInterfaceFonts[ifLogFont].FFontName :=
      iniMemFile.ReadString('LogFont', 'Name', InterfaceFontDefaults[ifLogFont].FFontName);
    redtOutputResults.Font.Name        := FInterfaceFonts[ifLogFont].FFontName;
    FInterfaceFonts[ifLogFont].FFontSize :=
      iniMemFile.ReadInteger('LogFont', 'Size', InterfaceFontDefaults[ifLogFont].FFontSize);
    redtOutputResults.Font.Size        := FInterfaceFonts[ifLogFont].FFontSize;
    For k := Low(TFileOpFont) To High(TFileOpFont) Do
      Begin
        FFileOpFonts[k].FFontColour := StringToColor(
          iniMemFile.ReadString('FileOpFonts', FileOperationFontDefaults[k].FININame + 'Colour',
            ColorToString(FileOperationFontDefaults[k].FFontColour)));
        FFileOpFonts[k].FBGFontColour := StringToColor(
          iniMemFile.ReadString('FileOpFonts', FileOperationFontDefaults[k].FININame + 'BGColour',
            ColorToString(FileOperationFontDefaults[k].FBGFontColour)));
        FFileOpFonts[k].FFontStyle := TFontStyles(Byte(
          iniMemFile.ReadInteger('FileOpFonts', FileOperationFontDefaults[k].FININame + 'Style',
            Byte(FileOperationFontDefaults[k].FFontStyle))));
      End;
    strLog := ChangeFileExt(FRootKey, '_log.txt');
    If FileExists(strLog) Then
      redtOutputResults.Lines.LoadFromFile(strLog);
    LogSize;
    WindowState := TWindowState(iniMemFile.ReadInteger('Setup', 'WindowState', Byte(wsNormal)));
    redtOutputResults.Height := iniMemFile.ReadInteger('Setup', 'OutputResultsHeight', 100);
    FCompareEXE := iniMemFile.ReadString('Setup', 'CompareEXE', '');
    FCopyDlgWidth := iniMemFile.ReadInteger('DlgWidths', 'CopyDlg', 0);
    FDeleteDlgWidth := iniMemFile.ReadInteger('DlgWidths', 'DeleteDlg', 0);
    FConfirmDlgWidth := iniMemFile.ReadInteger('DlgWidths', 'ConfirmDlg', 0);
    iDefaultOps := [fosDelete..fosDifference];
    FFileOpStats := TFileOpStats(Byte(iniMemFile.ReadInteger('Setup', 'FileOpStats',
      Byte(iDefaultOps))));
    UpgradeINIFolderOptions(iniMemFile);
    If FParams.Count = 0 Then
      Begin
        FFldrSyncOptions := [];
        For j := Low(TFldrSyncOption) To High(TFldrSyncOption) Do
          If iniMemFile.ReadBool(strFldrSyncOptions[j].FINISection, strFldrSyncOptions[j].FINIKey,
            strFldrSyncOptions[j].fDefault) Then
            Include(FFldrSyncOptions, j);
        sl := TStringList.Create;
        Try
          iniMemFile.ReadSection('NewFolders', sl);
          For i := 0 To sl.Count - 1 Do
            Begin
              iSyncOptions := TSyncOptions(SmallInt(iniMemFile.ReadInteger('NewFolderStatus', sl[i],
                SmallInt(iSyncOptions))));
              strMaxValue := iniMemFile.ReadString('NewFolderMaxFileSize', sl[i], '0,0,0,0');
              iMaxValue.Value := 0;
              iMaxValue.iFirst := StrToInt(GetField(strMaxValue, ',', 1));
              iMaxValue.iSecond := StrToInt(GetField(strMaxValue, ',', 2));
              iMaxValue.iThird := StrToInt(GetField(strMaxValue, ',', 3));
              iMaxValue.iFourth := StrToInt(GetField(strMaxValue, ',', 4));
              strLeftFldr := GetShortHint(iniMemFile.ReadString('NewFolders', sl[i], ''));
              strRightFldr := GetLongHint(iniMemFile.ReadString('NewFolders', sl[i], ''));
              FFolders.Add(
                TFolder.Create(
                  ExtractFilePath(strLeftFldr),
                  ExtractFilePath(strRightFldr),
                  ExtractFileName(strLeftFldr),
                  iSyncOptions,
                  iMaxValue.Value
                )
              );
            End;
        Finally
          sl.Free;
        End;
        FExclusions := StringReplace(iniMemFile.ReadString('Setup', 'Exclusions', ''), '|', #13#10,
          [rfReplaceAll]);
      End Else
      Begin
        ComOps.iSyncOptions := [soEnabled];
        ProcessCommandLine(FParams, ComOps);
        FFolders.Add(
          TFolder.Create(
            ComOps.strSourceFldr,
            ComOps.strDestFldr,
            ComOps.strFilePatterns,
            ComOps.iSyncOptions,
            ComOps.iMaxFileSize
          ));
        If clsDeletePermentently In ComOps.iCommandLineOptions Then
          Include(FFldrSyncOptions, fsoPermanentlyDeleteFiles)
        Else
          Exclude(FFldrSyncOptions, fsoPermanentlyDeleteFiles);
        If clsBatchRecycleFiles In ComOps.iCommandLineOptions Then
          Include(FFldrSyncOptions, fsoBatchRecycleFiles)
        Else
          Exclude(FFldrSyncOptions, fsoBatchRecycleFiles);
        If clsProceedAutomatically In ComOps.iCommandLineOptions Then
          Include(FFldrSyncOptions, fsoStartProcessingAutomatically)
        Else
          Exclude(FFldrSyncOptions, fsoStartProcessingAutomatically);
      End;
    DGHMemoryMonitor.UpdateInterval := iniMemFile.ReadInteger('MemoryMonitor', 'UpdateInterval',
      DGHMemoryMonitor.UpdateInterval);
    DGHMemoryMonitor.BackColour := StringToColor(iniMemFile.ReadString('MemoryMonitor',
      'BackColour', ColorToString(DGHMemoryMonitor.BackColour)));
    DGHMemoryMonitor.BackFontColour := StringToColor(iniMemFile.ReadString('MemoryMonitor',
      'BackFontColour', ColorToString(DGHMemoryMonitor.BackFontColour)));
    DGHMemoryMonitor.HighColour := StringToColor(iniMemFile.ReadString('MemoryMonitor',
      'HighColour', ColorToString(DGHMemoryMonitor.HighColour)));
    DGHMemoryMonitor.HalfColour := StringToColor(iniMemFile.ReadString('MemoryMonitor',
      'HalfColour', ColorToString(DGHMemoryMonitor.HalfColour)));
    DGHMemoryMonitor.LowColour := StringToColor(iniMemFile.ReadString('MemoryMonitor',
      'LowColour', ColorToString(DGHMemoryMonitor.LowColour)));
    DGHMemoryMonitor.HighFontColour := StringToColor(iniMemFile.ReadString('MemoryMonitor',
      'HighFontColour', ColorToString(DGHMemoryMonitor.HighFontColour)));
    DGHMemoryMonitor.HalfFontColour := StringToColor(iniMemFile.ReadString('MemoryMonitor',
      'HalfFontColour', ColorToString(DGHMemoryMonitor.HalfFontColour)));
    DGHMemoryMonitor.LowFontColour := StringToColor(iniMemFile.ReadString('MemoryMonitor',
      'LowFontColour', ColorToString(DGHMemoryMonitor.LowFontColour)));
    DGHMemoryMonitor.LowPoint := iniMemFile.ReadInteger('MemoryMonitor', 'LowPoint',
      DGHMemoryMonitor.LowPoint);
    DGHMemoryMonitor.HalfPoint := iniMemFile.ReadInteger('MemoryMonitor', 'HalfPoint',
      DGHMemoryMonitor.HalfPoint);
    DGHMemoryMonitor.HighPoint := iniMemFile.ReadInteger('MemoryMonitor', 'HighPoint',
      DGHMemoryMonitor.HighPoint);
    DGHMemoryMonitor.Font.Name := iniMemFile.ReadString('MemoryMonitor', 'FontName',
      DGHMemoryMonitor.Font.Name);
    DGHMemoryMonitor.Font.Size := iniMemFile.ReadInteger('MemoryMonitor', 'FontSize',
      DGHMemoryMonitor.Font.Size);
    DGHMemoryMonitor.Font.Style := TFontStyles(Byte(iniMemFile.ReadInteger('MemoryMonitor', 'FontStyle',
      Byte(DGHMemoryMonitor.Font.Style))));
    DGHMemoryMonitor.Width := iniMemFile.ReadInteger('MemoryMonitor', 'Width',
      DGHMemoryMonitor.Width);
  Finally
    iniMemFile.Free;
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
    [Int(Length(redtOutputResults.Lines.Text)) / 1024.0]);
  stbrStatusBar.Panels[0].Width := stbrStatusBar.Canvas.TextWidth(stbrStatusBar.Panels[0].Text) + 25;
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

//Type
//  TBackground = (bgLeft, bgRight);

//Var
//  R, ItemR     : TRect;
//  Buffer       : Array [0 .. MAX_PATH * 2] Of Char;
//  Ops          : Integer;
//  iBufferLen   : Integer;
//  iFileOp      : TFileOp;
//  SubItemRects : Array Of TRect;

//  (**
//
//    This function returns display rectangle for the given indexed sub item.
//
//    @precon  iIndex must be a valid SubItem index..
//    @postcon Returns display rectangle for the given indexed sub item.
//
//  **)
//  Procedure GetSubItemRects;
//
//  Var
//    LR : TRect;
//    j: Integer;
//
//  Begin
//    {$IFDEF PROFILECODE}
//    CodeProfiler.Start('GetSubItemRect');
//    {$ENDIF}
//    SetLength(SubItemRects, vstFileList.Header.Columns.Count);
//    LR := Item.DisplayRect(drBounds);
//    Inc(LR.Top, 2);    // Padding / Margin
//    Inc(LR.Bottom, 2); // Padding / Margin
//    For j  := 0 To vstFileList.Header.Columns.Count - 1 - 1 Do
//      Begin
//        Inc(LR.Left, Sender.Column[j].Width);
//        LR.Right := LR.Left + Sender.Column[j + 1].Width;
//        SubItemRects[j] := LR;
//        Inc(SubItemRects[j].Left, 6);   // Padding / Margin
//        Dec(SubItemRects[j].Right, 6);  // Padding / Margin
//      End;
//    {$IFDEF PROFILECODE}
//    CodeProfiler.Stop;
//    {$ENDIF}
//  End;

//  (**
//
//    This method draws the background for the left and right displays of file informations.
//
//    @precon  None.
//    @postcon Draws the background for the left and right displays of file informations.
//
//    @param   iColumn    as an Integer as a constant
//    @param   Background as a TBackground as a constant
//
//  **)
//  Procedure DrawBackground(Const iColumn: Integer; Const Background: TBackground);
//
//  Begin
//    {$IFDEF PROFILECODE}
//    CodeProfiler.Start('DrawBackground');
//    {$ENDIF}
//    If Item.Selected Then
//      Sender.Canvas.Brush.Color := StyleServices.GetSystemColor(clHighlight)
//    Else If (Pos('R', Item.SubItems[iColumn - 1]) > 0) Then
//      Sender.Canvas.Brush.Color :=
//        StyleServices.GetSystemColor(FFileOpFonts[fofReadOnly].FBGFontColour)
//    Else
//      Sender.Canvas.Brush.Color :=
//        StyleServices.GetSystemColor(FFileOpFonts[TFileOpFont(iFileOp)].FBGFontColour);
//    //Sender.Canvas.Brush.Color := StyleServices.GetSystemColor(clWindow);
//    If Background = bgLeft Then
//      R := SubItemRects[iLDateCol - 1]
//    Else
//      R := SubItemRects[iRDisplayCol - 1];
//    ItemR := Item.DisplayRect(drBounds);
//    If Background = bgLeft Then
//      ItemR.Right := R.Right + 6
//    Else
//      ItemR.Left := R.Left - 6;
//    Sender.Canvas.FillRect(ItemR);
//    {$IFDEF PROFILECODE}
//    CodeProfiler.Stop;
//    {$ENDIF}
//  End;

//  (**
//
//    This procedure sets the font colour and style attributes depending on the
//    status of the list item.
//
//    @precon  None.
//    @postcon Sets the font colour and style attributes depending on the
//    status of the list item.
//
//  **)
//  Procedure SetTextAttributes;
//
//  Begin
//    {$IFDEF PROFILECODE}
//    CodeProfiler.Start('SetTextAttributes');
//    {$ENDIF}
//    Sender.Canvas.Font.Color :=
//      StyleServices.GetSystemColor(FFileOpFonts[TFileOpFont(iFileOp)].FFontColour);
//    Sender.Canvas.Font.Style := FFileOpFonts[TFileOpFont(iFileOp)].FFontStyle;
//    If Item.Selected Then
//      Sender.Canvas.Font.Color := StyleServices.GetSystemColor(clHighlightText);
//    {$IFDEF PROFILECODE}
//    CodeProfiler.Stop;
//    {$ENDIF}
//  End;

//  (**
//
//    This procedure sets the text drawing options (alignment, etc) for the different columns to be 
//    displayed.
//
//    @precon  None.
//    @postcon Sets the text drawing options (alignment, etc) for the different columns to be displayed.
//
//    @param   iSubItem as an Integer as a constant
//
//  **)
//  Procedure SetTextDrawingOptions(Const iSubItem : Integer);
//
//  Begin
//    {$IFDEF PROFILECODE}
//    CodeProfiler.Start('SetTextDrawingOptions');
//    {$ENDIF}
//    Case iSubItem Of
//      0, 4:
//        Ops := DT_LEFT Or DT_MODIFYSTRING Or DT_PATH_ELLIPSIS Or DT_NOPREFIX;
//      2, 3, 6, 7:
//        Ops := DT_RIGHT;
//    Else
//      Ops := DT_LEFT;
//    End;
//    {$IFDEF PROFILECODE}
//    CodeProfiler.Stop;
//    {$ENDIF}
//  End;

//  (**
//
//    This procedure returns the display rectangle (adjusted for file icon image) for the text to be 
//    displayed in the report column.
//
//    @precon  None.
//    @postcon Returns the display rectangle (adjusted for file icon image) for the text to be displayed 
//             in the report column.
//
//    @param   iSubItem as an Integer as a constant
//    @return  a TRect
//
//  **)
//  Function DrawFileIcon(Const iSubItem : Integer): TRect;
//
//  Var
//    iImage     : Integer;
//    boolEnabled: Boolean;
//
//  Begin
//    {$IFDEF PROFILECODE}
//    CodeProfiler.Start('DrawFileIcon');
//    {$ENDIF}
//    Result := SubItemRects[iSubItem];
//    If iSubItem In [0, 4] Then
//      Begin
//        Dec(Result.Left, 3);
//        iImage      := Item.SubItemImages[iSubItem];
//        boolEnabled := (iImage > -1);
//        If Not boolEnabled Then
//          Begin
//            If iSubItem = 0 Then
//              iImage := Item.SubItemImages[4]
//            Else
//              iImage := Item.SubItemImages[0];
//            ilFileTypeIcons.Draw(Sender.Canvas, Result.Left, Result.Top, iImage, boolEnabled);
//          End;
//        Inc(Result.Left, 16 + 3);
//      End;
//    {$IFDEF PROFILECODE}
//    CodeProfiler.Stop;
//    {$ENDIF}
//  End;

//  (**
//
//    This procedure sets the text background for the columns of information.
//
//    @precon  None.
//    @postcon Sets the text background for the columns of information.
//
//    @param   FileSide as a TBackground as a constant
//
//  **)
//  Procedure SetTextFontBackground(Const FileSide : TBackground);
//
//  Begin
//    {$IFDEF PROFILECODE}
//    CodeProfiler.Start('SetTextFontBackground');
//    {$ENDIF}
//    Sender.Canvas.Brush.Color :=
//      StyleServices.GetSystemColor(FFileOpFonts[TFileOpFont(iFileOp)].FBGFontColour);
//    Sender.Canvas.Font.Color :=
//      StyleServices.GetSystemColor(FFileOpFonts[TFileOpFont(iFileOp)].FFontColour);
//    If Item.Selected Then
//      Begin
//        Sender.Canvas.Brush.Color := StyleServices.GetSystemColor(clHighlight);
//        Sender.Canvas.Font.Color := StyleServices.GetSystemColor(clHighlightText);
//      End
//    Else If FileSide = bgLeft Then
//      Begin
//        If (Pos('R', Item.SubItems[iLAttrCol - 1]) > 0) Then
//          Begin
//            Sender.Canvas.Brush.Color :=
//              StyleServices.GetSystemColor(FFileOpFonts[fofReadOnly].FBGFontColour);
//            Sender.Canvas.Font.Color := 
//              StyleServices.GetSystemColor(FFileOpFonts[fofReadOnly].FFontColour);
//          End;
//      End  Else
//      Begin
//        If (Pos('R', Item.SubItems[iRAttrCol - 1]) > 0) Then
//          Begin
//            Sender.Canvas.Brush.Color := 
//              StyleServices.GetSystemColor(FFileOpFonts[fofReadOnly].FBGFontColour);
//            Sender.Canvas.Font.Color := 
//              StyleServices.GetSystemColor(FFileOpFonts[fofReadOnly].FFontColour);
//          End;
//      End;
//    {$IFDEF PROFILECODE}
//    CodeProfiler.Stop;
//    {$ENDIF}
//  End;

//  (**
//
//    This procedure displays a grayed out destination path for files which don`t have a destination file, 
//    else sets up the font colours and style attributes for displaying the text.
//
//    @precon  None.
//    @postcon Displays a grayed out destination path for files which don`t have a destination file, else 
//             sets up the font colours and style attributes for displaying the text.
//
//    @param   iSubItem as an Integer as a constant
//    @param   iFileOp  as a TFileOp as a constant
//
//  **)
//  Procedure FixUpEmptyFilePaths(Const iSubItem : Integer; Const iFileOp : TFileOp);
//
//  Begin
//    {$IFDEF PROFILECODE}
//    CodeProfiler.Start('FixUpEmptyFilePaths');
//    {$ENDIF}
//    If (iSubItem = 0) And (Item.SubItems[iSubItem] = '') Then
//      Begin
//        StrPCopy(Buffer, Item.SubItems[iLFullFileNameCol - 1]);
//        iBufferLen               := Length(Item.SubItems[iLFullFileNameCol - 1]);
//        If Item.Selected Then
//          Begin
//            Sender.Canvas.Font.Color := StyleServices.GetSystemColor(clHighlightText);
//            Sender.Canvas.Brush.Color := StyleServices.GetSystemColor(clHighlight);
//          End Else
//          Begin
//            Sender.Canvas.Font.Color := 
//              StyleServices.GetSystemColor(FFileOpFonts[fofMissingFile].FFontColour);
//            Sender.Canvas.Brush.Color := 
//              StyleServices.GetSystemColor(FFileOpFonts[TFileOpFont(iFileOp)].FBGFontColour);
//          End;
//      End
//    Else If (iSubItem = 4) And (Item.SubItems[iSubItem] = '') Then
//      Begin
//        StrPCopy(Buffer, Item.SubItems[iRFullFileNameCol - 1]);
//        iBufferLen               := Length(Item.SubItems[iRFullFileNameCol - 1]);
//        If Item.Selected Then
//          Begin
//            Sender.Canvas.Font.Color := StyleServices.GetSystemColor(clHighlightText);
//            Sender.Canvas.Brush.Color := StyleServices.GetSystemColor(clHighlight);
//          End Else
//          Begin
//            Sender.Canvas.Font.Color := 
//              StyleServices.GetSystemColor(FFileOpFonts[fofMissingFile].FFontColour);
//            Sender.Canvas.Brush.Color := 
//              StyleServices.GetSystemColor(FFileOpFonts[TFileOpFont(iFileOp)].FBGFontColour);
//          End;
//      End
//    Else
//      SetTextAttributes;
//    {$IFDEF PROFILECODE}
//    CodeProfiler.Stop;
//    {$ENDIF}
//  End;

//Var
//  iSubItem  : Integer;

Begin
  {$IFDEF PROFILECODE}
  CodeProfiler.Start('TfrmMainForm.lvFileListCustomDrawItem');
  {$ENDIF}
  DefaultDraw := False;
//  iFileOp := TFileOp(Item.StateIndex);
//  GetSubItemRects;
//  DrawBackground(iLAttrCol, bgLeft);
//  DrawBackground(iRAttrCol, bgRight);
//  R := Item.DisplayRect(drBounds);
//  ilActionImages.Draw(Sender.Canvas, R.Left + vstFileList.Header.Columns[0].Width Div 2 - 8, R.Top,
//    Item.StateIndex, True);
//  // Draw Left File
//  SetTextFontBackground(bgLeft);
//  For iSubItem := iLDisplayCol - 1 To iLDateCol - 1 Do
//    Begin
//      SetTextDrawingOptions(iSubItem);
//      R := DrawFileIcon(iSubItem);
//      StrPCopy(Buffer, Item.SubItems[iSubItem]);
//      iBufferLen := Length(Item.SubItems[iSubItem]);
//      FixUpEmptyFilePaths(iSubItem, iFileOp);
//      Sender.Canvas.Refresh;
//      DrawText(Sender.Canvas.Handle, Buffer, iBufferLen, R, Ops);
//    End;
//  // Draw Left File
//  SetTextFontBackground(bgRight);
//  For iSubItem := iRDisplayCol - 1 To iRDateCol - 1 Do
//    Begin
//      SetTextDrawingOptions(iSubItem);
//      R := DrawFileIcon(iSubItem);
//      StrPCopy(Buffer, Item.SubItems[iSubItem]);
//      iBufferLen := Length(Item.SubItems[iSubItem]);
//      FixUpEmptyFilePaths(iSubItem, iFileOp);
//      Sender.Canvas.Refresh;
//      DrawText(Sender.Canvas.Handle, Buffer, iBufferLen, R, Ops);
//    End;
  {$IFDEF PROFILECODE}
  CodeProfiler.Stop;
  {$ENDIF}
End;

(**

  This is an on match list end event handler for the sync module.

  @precon  None.
  @postcon None.

**)
Procedure TfrmMainForm.MatchListEndProc;

Begin
  OutputResultLn('Done.');
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
  OutputResult('Match Lists: ');
  Inc(FProgressSection);
  FProgressForm.InitialiseSection(FProgressSection, 1, 100);
End;

(**

  This is an on context popup event handler for the memory monitor graphics control.

  @precon  None.
  @postcon Invokes the dialogue to editing the configuration of the Memory Monitor.

  @param   Sender   as a TObject
  @param   MousePos as a TPoint
  @param   Handled  as a Boolean as a reference

**)
Procedure TfrmMainForm.MemoryPopupMenu(Sender: TObject; MousePos: TPoint;
  Var Handled: Boolean);

Begin
  actToolsConfigMemMonExecute(Sender);
End;

(**

  This is an on Nothing To Do event handler for the sync module.

  @precon  None.
  @postcon Outputs the name of the files that have a difference size but the same date
           and time.

  @param   iFile       as an Integer
  @param   iFileCount  as an Integer
  @param   strLPath    as a String
  @param   strRPath    as a String
  @param   strFileName as a String

**)
procedure TfrmMainForm.NothingToDo(iFile, iFileCount: Integer; strLPath, strRPath,
    strFileName: String);

begin
  OutputFileNumber(iFile, iFileCount);
  OutputResultLn(Format('%s => %s%s', [strLPath, strRPath, strFileName]));
end;

(**

  This is an on nothingToDo end event handler for the sync module.

  @precon  None.
  @postcon None.

  @nocheck EmptyMethod

**)
Procedure TfrmMainForm.NothingToDoEnd;

Begin
End;

(**

  This is a Nothing To Do Start event handler for the sync module.

  @precon  None.
  @postcon Displays a header is there are files to output.

  @param   iFileCount as an Integer

**)
procedure TfrmMainForm.NothingToDoStart(iFileCount: Integer);

begin
  If iFileCount > 0 Then
    OutputResultLn(Format('%1.0n Files with Nothing to do...',
      [Int(iFileCount)]));
end;

(**

  This method outputs the given message to the output window with the optional colour but
  without a line feed and carraige return.

  @precon  None.
  @postcon The messages is output to the end of output window.

  @param   strMsg     as a String

**)
Procedure TfrmMainForm.OutputResult(strMsg: String = '');

Begin
  redtOutputResults.SelLength           := 0;
  redtOutputResults.SelStart            := Length(redtOutputResults.Text);
  redtOutputResults.SelText             := strMsg;
  LogSize;
End;

(**

  This method outputs the given message to the output window with the optional colour but
  WITH a line feed and carraige return.

  @precon  None.
  @postcon The messages is output to the end of output window.

  @param   strMsg     as a String

**)
Procedure TfrmMainForm.OutputResultLn(strMsg: String = '');

Begin
  OutputResult(strMsg + #13#10);
End;

(**

  This method adds panels to the status bar for each of the statistics returned from the
  sync module.

  @precon  None.
  @postcon Panels are added to the status bar for each available statistic.

**)
Procedure TfrmMainForm.OutputStats;

Const
  strTemplate = '%s: %1.0n files in %1.1n kbytes';

Var
  i: TFileOpStat;
  P: TStatusPanel;

Begin
  stbrStatusBar.Panels.BeginUpdate;
  Try
    While stbrStatusBar.Panels.Count > 1 Do
      stbrStatusBar.Panels.Delete(stbrStatusBar.Panels.Count - 1);
    FSyncModule.BuildStats;
    For i := Low(TFileOpStat) To High(TFileOpStat) Do
      If i In FFileOpStats Then
        Begin
          P := stbrStatusBar.Panels.Add;
          P.Text := Format(strTemplate, [
            FSyncModule.Statistics[i].FName,
            Int(FSyncModule.Statistics[i].FCount),
            Int(FSyncModule.Statistics[i].FSize / 1024.0)
          ]);
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
  iMaxValue : TInt64Ex;
  k: TFileOpFont;
  Ops: TSyncOptions;
  iniMemFile: TMemIniFile;

Begin
  iniMemFile := TMemIniFile.Create(FRootKey);
  Try
    recWndPlmt.Length := SizeOf(TWindowPlacement);
    GetWindowPlacement(Handle, @recWndPlmt);
    iniMemFile.WriteInteger('Setup', 'Top', recWndPlmt.rcNormalPosition.Top);
    iniMemFile.WriteInteger('Setup', 'Left', recWndPlmt.rcNormalPosition.Left);
    iniMemFile.WriteInteger('Setup', 'Height', recWndPlmt.rcNormalPosition.Bottom -
        recWndPlmt.rcNormalPosition.Top);
    iniMemFile.WriteInteger('Setup', 'Width', recWndPlmt.rcNormalPosition.Right -
        recWndPlmt.rcNormalPosition.Left);
    iniMemFile.WriteInteger('ColumnWidths', 'LAttr', vstFileList.Header.Columns[Integer(vfLeftAttr)].Width);
    iniMemFile.WriteInteger('ColumnWidths', 'LSize', vstFileList.Header.Columns[Integer(vfLeftSize)].Width);
    iniMemFile.WriteInteger('ColumnWidths', 'LDAte', vstFileList.Header.Columns[Integer(vfLeftDate)].Width);
    iniMemFile.WriteInteger('ColumnWidths', 'RAttr', vstFileList.Header.Columns[Integer(vfRightAttr)].Width);
    iniMemFile.WriteInteger('ColumnWidths', 'RSize', vstFileList.Header.Columns[Integer(vfRightSize)].Width);
    iniMemFile.WriteInteger('ColumnWidths', 'RDate', vstFileList.Header.Columns[Integer(vfRightDate)].Width);
    iniMemFile.WriteString('ListViewFont', 'Name', FInterfaceFonts[ifTableFont].FFontName);
    iniMemFile.WriteInteger('ListViewFont', 'Size', FInterfaceFonts[ifTableFont].FFontSize);
    iniMemFile.WriteString('LogFont', 'Name', FInterfaceFonts[ifLogFont].FFontName);
    iniMemFile.WriteInteger('LogFont', 'Size', FInterfaceFonts[ifLogFont].FFontSize);
    For k := Low(TFileOpFont) To High(TFileOpFont) Do
      Begin
        iniMemFile.WriteString('FileOpFonts', FileOperationFontDefaults[k].FININame + 'Colour',
          ColorToString(FFileOpFonts[k].FFontColour));
        iniMemFile.WriteString('FileOpFonts', FileOperationFontDefaults[k].FININame + 'BGColour',
          ColorToString(FFileOpFonts[k].FBGFontColour));
        iniMemFile.WriteInteger('FileOpFonts', FileOperationFontDefaults[k].FININame + 'Style',
          Byte(FFileOpFonts[k].FFontStyle));
      End;
    redtOutputResults.Lines.SaveToFile(ChangeFileExt(FRootKey, '_log.txt'));
    iniMemFile.WriteInteger('Setup', 'WindowState', Byte(WindowState));
    iniMemFile.WriteInteger('Setup', 'OutputResultsHeight', redtOutputResults.Height);
    iniMemFile.WriteString('Setup', 'CompareEXE', FCompareEXE);
    iniMemFile.WriteInteger('DlgWidths', 'CopyDlg', FCopyDlgWidth);
    iniMemFile.WriteInteger('DlgWidths', 'DeleteDlg', FDeleteDlgWidth);
    iniMemFile.WriteInteger('DlgWidths', 'ConfirmDlg', FConfirmDlgWidth);
    iniMemFile.WriteInteger('Setup', 'FileOpStats', Byte(FFileOpStats));
    iniMemFile.WriteString('Setup', 'Theme', TStyleManager.ActiveStyle.Name);
    If FParams.Count = 0 Then
      Begin
        For j := Low(TFldrSyncOption) To High(TFldrSyncOption) Do
          iniMemFile.WriteBool(strFldrSyncOptions[j].FINISection, strFldrSyncOptions[j].FINIKey,
            j In FFldrSyncOptions);
        iniMemFile.EraseSection('Folders');
        iniMemFile.EraseSection('FolderStatus');
        iniMemFile.EraseSection('FolderMaxFileSize');
        iniMemFile.EraseSection('NewFolders');
        iniMemFile.EraseSection('NewFolderStatus');
        iniMemFile.EraseSection('NewFolderMaxFileSize');
        For i := 0 To FFolders.Count - 1 Do
          Begin
            iniMemFile.WriteString('NewFolders', Format('Folder%2.2d', [i]),
              FFolders.Folder[i].LeftFldr + FFolders.Folder[i].Patterns + '|' +
              FFolders.Folder[i].RightFldr + FFolders.Folder[i].Patterns);
            Ops := FFolders.Folder[i].SyncOptions;
            Exclude(Ops, soTempDisabled);
            iniMemFile.WriteInteger('NewFolderStatus', Format('Folder%2.2d', [i]), SmallInt(Ops));
            iMaxValue.Value := FFolders.Folder[i].MaxFileSize;
            iniMemFile.WriteString('NewFolderMaxFileSize', Format('Folder%2.2d', [i]),
              Format('%d,%d,%d,%d', [iMaxValue.iFirst, iMaxValue.iSecond,
                iMaxValue.iThird, iMaxValue.iFourth]));
          End;
        iniMemFile.WriteString('Setup', 'Exclusions', StringReplace(FExclusions, #13#10, '|',
            [rfReplaceAll]));
        //FExclusions := StringReplace(ReadString('Setup', 'Exclusions', ''), '|', #13#10,
        //  [rfReplaceAll]);
      End;
    iniMemFile.WriteInteger('MemoryMonitor', 'UpdateInterval', DGHMemoryMonitor.UpdateInterval);
    iniMemFile.WriteString('MemoryMonitor', 'BackColour', ColorToString(DGHMemoryMonitor.BackColour));
    iniMemFile.WriteString('MemoryMonitor', 'BackFontColour', ColorToString(DGHMemoryMonitor.BackFontColour));
    iniMemFile.WriteString('MemoryMonitor', 'HighColour', ColorToString(DGHMemoryMonitor.HighColour));
    iniMemFile.WriteString('MemoryMonitor', 'HalfColour', ColorToString(DGHMemoryMonitor.HalfColour));
    iniMemFile.WriteString('MemoryMonitor', 'LowColour', ColorToString(DGHMemoryMonitor.LowColour));
    iniMemFile.WriteString('MemoryMonitor', 'HighFontColour', ColorToString(DGHMemoryMonitor.HighFontColour));
    iniMemFile.WriteString('MemoryMonitor', 'HalfFontColour', ColorToString(DGHMemoryMonitor.HalfFontColour));
    iniMemFile.WriteString('MemoryMonitor', 'LowFontColour', ColorToString(DGHMemoryMonitor.LowFontColour));
    iniMemFile.WriteInteger('MemoryMonitor', 'LowPoint', DGHMemoryMonitor.LowPoint);
    iniMemFile.WriteInteger('MemoryMonitor', 'HalfPoint', DGHMemoryMonitor.HalfPoint);
    iniMemFile.WriteInteger('MemoryMonitor', 'HighPoint', DGHMemoryMonitor.HighPoint);
    iniMemFile.WriteString('MemoryMonitor', 'FontName', DGHMemoryMonitor.Font.Name);
    iniMemFile.WriteInteger('MemoryMonitor', 'FontSize', DGHMemoryMonitor.Font.Size);
    iniMemFile.WriteInteger('MemoryMonitor', 'FontStyle', Byte(DGHMemoryMonitor.Font.Style));
    iniMemFile.WriteInteger('MemoryMonitor', 'Width', DGHMemoryMonitor.Width);
    iniMemFile.UpdateFile;
  Finally
    iniMemFile.Free;
  End;
End;

(**

  This is an on form activate event handler for the main form.

  @precon  None.
  @postcon Ensures that any child forms that are visible are brought to the front.

  @param   Sender as a TObject

**)
Procedure TfrmMainForm.FormActivate(Sender: TObject);

Var
  iForm: Integer;

Begin
  For iForm := 0 To ComponentCount - 1 Do
    If Components[iForm] Is TForm Then
      If (Components[iForm] As TForm).Visible  Then
        (Components[iForm] As TForm).BringToFront;
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
  vstFileList.NodeDataSize := SizeOf(TFSFileListNode);
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
  FFolders                       := TFolders.Create;
  FProgressForm                  := TfrmProgress.Create(Self);
  FProgressForm.OnUpdateProgress := UpdateProgress;
  Application.HelpFile := ExtractFilePath(ParamStr(0)) + 'FldrSync.chm';
  LoadSettings();
  actHelpCheckForUpdatesExecute(Nil);
  Application.OnHint                := ApplicationHint;
  FIconFiles                        := TStringList.Create;
  FIconFiles.Sorted                 := True;
  Caption := Caption + StringReplace(GetConsoleTitle(' %d.%d%s (Build %s)'), #13#10,
    ' - ', []);
  If IsDebuggerPresent Then
    Caption := Format('%s [DEBUGGING]', [Caption]);
  {$IFDEF WIN64}
  Caption := Caption + ' [64-bit]';
  {$ELSE}
  Caption := Caption + ' [32-bit]';
  {$ENDIF}
  Caption                             := Format('%s: %s', [Caption, FRootKey]);
  FDialogueBottom                     := 0;
  FSyncModule                         := TCompareFoldersCollection.Create(Self.Handle);
  FSyncModule.OnSearchStart           := SearchStartProc;
  FSyncModule.OnSearch                := SearchProc;
  FSyncModule.OnSearchEnd             := SearchEndProc;
  FSyncModule.OnCompareStart          := CompareStartProc;
  FSyncModule.OnCompare               := CompareProc;
  FSyncModule.OnCompareEnd            := CompareEndProc;
  FSyncModule.OnMatchListStart        := MatchListStartProc;
  FSyncModule.OnMatchList             := MatchListProc;
  FSyncModule.OnMatchListEnd          := MatchListEndProc;
  FSyncModule.OnDeleteStart           := DeleteStartProc;
  FSyncModule.OnDeleting              := DeletingProc;
  FSyncModule.OnDeleted               := DeletedProc;
  FSyncModule.OnDeleteQuery           := DeleteQueryProc;
  FSyncModule.OnDeleteReadOnlyQuery   := DeleteReadOnlyQueryProc;
  FSyncModule.OnDeleteEnd             := DeleteEndProc;
  FSyncModule.OnCopyStart             := CopyStartProc;
  FSyncModule.OnCopying               := CopyingProc;
  FSyncModule.OnCopyContents          := CopyContentsProc;
  FSyncModule.OnCopied                := CopiedProc;
  FSyncModule.OnCopyQuery             := CopyQueryProc;
  FSyncModule.OnCopyReadOnlyQuery     := CopyReadOnlyQueryProc;
  FSyncModule.OnCopyEnd               := CopyEndProc;
  FSyncModule.OnDiffSizeStart         := DiffSizeStart;
  FSyncModule.OnDiffSize              := DiffSize;
  FSyncModule.OnDiffSizeEnd           := DiffSizeEnd;
  FSyncModule.OnNothingToDoStart      := NothingToDoStart;
  FSyncModule.OnNothingToDo           := NothingToDo;
  FSyncModule.OnNothingToDoEnd        := NothingToDoEnd;
  FSyncModule.OnExceedsSizeLimitStart := ExceedsSizeLimitStart;
  FSyncModule.OnExceedsSizeLimit      := ExceedsSizeLimit;
  FSyncModule.OnExceedsSizeLimitEnd   := ExceedsSizeLimitEnd;
  FSyncModule.OnErrorMsgsStart        := ErrorMessageStart;
  FSyncModule.OnErrorMsgs             := ErrorMessage;
  FSyncModule.OnErrorMsgsEnd          := ErrorMessageEnd;
  FSyncModule.OnDeleteFoldersStart    := DeleteFoldersStart;
  FSyncModule.OnDeleteFolders         := DeleteFolders;
  FSyncModule.OnDeleteFoldersEnd      := DeleteFoldersEnd;
  FSyncModule.OnCopyError             := CopyError;
  FSyncModule.OnDeleteError           := DeleteError;
  FTaskbarList  := Nil;
  FTaskbarList3 := Nil;
  If CheckWin32Version(6, 1) Then
    Begin
      FTaskbarList := CreateComObject(CLSID_TaskbarList) As ITaskbarList;
      FTaskbarList.HrInit;
      Supports(FTaskbarList, IID_ITaskbarList3, FTaskbarList3);
    End;
  actEditCopyRightToLeft.Tag := Integer(foRightToLeft);
  actEditCopyLeftToRight.Tag := Integer(foLeftToRight);
  actEditDelete.Tag := Integer(foDelete);
  actEditDoNothing.Tag := Integer(foNothing);
  DGHMemoryMonitor.OnContextPopup := MemoryPopupMenu;
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
  boolSuccess    : Boolean;
  iCount         : Integer;
  NodeData       : PFSFileListNode;
  Node: PVirtualNode;

Begin
  OutputResultLn('Comparison started @ ' + FormatDateTime('dddd dd mmmm yyyy hh:mm:ss',
    Now()));
  boolSuccess := False;
  If Not CheckFolders Then
    Exit;
  Try
    DisableActions;
    Try
      iEnabledFolders := 0;
      For i := 0 To FFolders.Count - 1 Do
        Begin
          If [soEnabled, soTempDisabled] * FFolders.Folder[i].SyncOptions = [soEnabled] Then
            Inc(iEnabledFolders);
        End;
      FProgressForm.RegisterSections(iEnabledFolders * 3 + 2);
      FProgressSection := -1;
      vstFileList.BeginUpdate;
      Try
        vstFileList.Clear;
      Finally
        vstFileList.EndUpdate;
      End;
      Try
        FSyncModule.Clear;
        boolSuccess := FSyncModule.ProcessFolders(FFolders, FExclusions);
        FixUpPanes;
        FormResize(Sender);
        iCount := 0;
        Node := vstFileList.GetFirst;
        While Assigned(Node) Do
          Begin
            NodeData := vstFileList.GetNodeData(Node);
            If Integer(NodeData.FState) <> Integer(fofExceedsSizeLimit) Then
              Inc(iCount);
            Node := vstFileList.GetNext(Node);
          End;
        If (fsoCloseIFNoFilesAfterComparison In FFldrSyncOptions) And (iCount = 0) Then
          Begin
            FProgressForm.InitialiseSection(FProgressSection, 0, 1);
            FProgressForm.Progress(FProgressSection, 1, 'Auto-exiting application...',
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
        If Not boolSuccess Then
          Begin
            OutputResultLn;
            OutputResultLn('Comparison cancelled by user!');
          End;
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
  vstFileList.BeginUpdate;
  Try
    For iProcessItem := 0 To FSyncModule.ProcessCount - 1 Do
      Begin
        P := FSyncModule.Process[iProcessItem];
        InsertListItem(iProcessItem, P);
        If iProcessItem Mod 10 = 0 Then
          FProgressForm.Progress(FProgressSection, iProcessItem,
            Format('Building ListView... %1.0n in %1.0n', [Int(iProcessItem),
                Int(FSyncModule.ProcessCount)]), '');
      End;
  Finally
    vstFileList.EndUpdate;
  End;
End;

(**

  This method adds a pair of filenames with sizes and dates to the list view.

  @precon  None.
  @postcon Adds a pair of filenames with sizes and dates to the list view.

  @param   iProcessItem as an Integer as a constant
  @param   ProcessItem  as a TProcessItem as a constant

**)
Procedure TfrmMainForm.InsertListItem(Const iProcessItem : Integer; Const ProcessItem : TProcessItem);

Var
  Node : PVirtualNode;
  NodeData : PFSFileListNode;
  strFileName: String;

Begin
  Node := vstFileList.AddChild(Nil);
  NodeData := vstFileList.GetNodeData(Node);
  NodeData.FProcessIndex := iProcessItem;
  // Left File
  NodeData.FLeftFileIndex := -1;
  NodeData.FRightFileIndex := -1;
  If Assigned(ProcessItem.LeftFile) Then
    Begin
      NodeData.FLeftFilename := ProcessItem.LPath + ProcessItem.LeftFile.FileName;
      NodeData.FLeftAttr := ProcessItem.LeftFile.Attributes;
      NodeData.FLeftSize := ProcessItem.LeftFile.Size;
      NodeData.FLeftDate := FileDateToDateTime(ProcessItem.LeftFile.DateTime);
      strFileName := ProcessItem.LeftFile.FileName;
    End Else
    Begin
      NodeData.FLeftFilename := '';
      NodeData.FLeftAttr := 0;
      NodeData.FLeftSize := 0;
      NodeData.FLeftDate := 0;
    End;
  // Right File
  If Assigned(ProcessItem.RightFile) Then
    Begin
      NodeData.FRightFilename := ProcessItem.RPath + ProcessItem.RightFile.FileName;
      NodeData.FRightAttr := ProcessItem.RightFile.Attributes;
      NodeData.FRightSize := ProcessItem.RightFile.Size;
      NodeData.FRightDate := FileDateToDateTime(ProcessItem.RightFile.DateTime);
      strFileName := ProcessItem.RightFile.FileName;
    End Else
    Begin
      NodeData.FRightFilename := '';
      NodeData.FRightAttr := 00;
      NodeData.FRightSize := 0;
      NodeData.FRightDate := 0;
    End;
  NodeData.FLeftFullFileName := ProcessItem.LPath + strFileName;
  NodeData.FRightFullFileName := ProcessItem.RPath + strFileName;
  NodeData.FState := ProcessItem.FileOp;
End;

{$HINTS OFF}
(**

  This method retrieves from the system the icon for the specified file, places it in a 
  list and returns the index of the index in the list.

  @precon  None.
  @postcon Retrieves from the system the icon for the specified file, places it in a list
           and returns the index of the index in the list

  @param   ProcessItem as a TProcessItem
  @param   strFileName as a String
  @return  a NativeInt

**)
Function TfrmMainForm.GetImageIndex(ProcessItem : TProcessItem; strFileName: String): NativeInt;

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
  Result := -1;
  strExt := LowerCase(ExtractFileExt(strFileName));
  If FIconFiles.Find(strExt, iIndex) Then
    Begin
      Result := NativeInt(FIconFiles.Objects[iIndex]);
      Exit;
    End;
  // Get Icon information from Registry
  strClassName := GetRegStringValue(strExt, '');
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
  If strExt <> '.exe' Then
    FIconFiles.AddObject(strExt, TObject(Result));
  //-------------------------------------------------------------------------------------
  If Result < 0 Then
    Begin
      CodeSite.Send('Ext', strExt);
      CodeSite.Send('Extension List', FIconFiles);
      CodeSite.Send('Class', strClassName);
      CodeSite.Send(strFileName, Result);
      CodeSite.Send('LPath', ProcessItem.LPath);
      CodeSite.Send('RPath', ProcessItem.RPath);
      CodeSite.Send('LeftFile', ProcessItem.LeftFile);
      CodeSite.Send('RightFile', ProcessItem.RightFile);
      CodeSite.SendEnum('FileOp', TypeInfo(TFileOp), Ord(ProcessItem.FileOp));
      CodeSite.SendSet('SynvOptions', TypeInfo(TSyncOptions), ProcessItem.SyncOptions);
      CodeSite.AddSeparator;
    End;
End;
{$HINTS ON}

(**

  This is an on execute event handler for the Tools Compare action.

  @precon  None.
  @postcon Opens the 2 selected files in the comparison tool.

  @param   Sender as a TObject

**)
Procedure TfrmMainForm.actToolsCompareExecute(Sender: TObject);

Var
  NodeData : PFSFileListNode;

Begin
  NodeData := vstFileList.GetNodeData(vstFileList.FocusedNode);
  ShellExecute(Application.Handle, 'Open', PChar(FCompareEXE), PChar(Format('"%s" "%s"', [
    NodeData.FLeftFullFileName,
    NodeData.FRightFullFileName
  ])), PChar(ExtractFilePath(FCompareEXE)), SW_SHOWNORMAL)
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
  boolEnabled: Boolean;
  NodeData : PFSFileListNode;

Begin
  boolEnabled := Assigned(vstFileList.FocusedNode);
  If boolEnabled Then
    Begin
      NodeData := vstFileList.GetNodeData(vstFileList.FocusedNode);
    boolEnabled := boolEnabled
      And FileExists(NodeData.FLeftFullFileName)
      And FileExists(NodeData.FRightFullFileName);
    End;
  (Sender As TAction).Enabled := boolEnabled;
End;

(**

  This is an on execute event handler for the ConfigMemMon action.

  @precon  None.
  @postcon Displays a configuration form that allows you to edit the colours, fonts and
           positions of the memory monitor information.

  @param   Sender as a TObject

**)
Procedure TfrmMainForm.actToolsConfigMemMonExecute(Sender: TObject);

Begin
  TfrmMemoryMonitorOptions.Execute(DGHMemoryMonitor);
End;

(**

  This is an on execute event handler for the Tools Options action.

  @precon  None.
  @postcon Displays the options dialogue for the application.

  @param   Sender as a TObject

**)
Procedure TfrmMainForm.actToolsOptionsExecute(Sender: TObject);

Var
  strTheme : String;
  
Begin
  FInterfaceFonts[ifTableFont].FFontName := vstFileList.Font.Name;
  FInterfaceFonts[ifTableFont].FFontSize :=vstFileList.Font.Size;
  FInterfaceFonts[ifLogFont].FFontName := redtOutputResults.Font.Name;
  FInterfaceFonts[ifLogFont].FFontSize := redtOutputResults.Font.Size;
  If TfrmOptions.Execute(FFolders, FExclusions, FCompareEXE, FRootKey, FInterfaceFonts,
    FFileOpFonts, FFldrSyncOptions, strTheme, FFileOpStats) Then
    Begin
      vstFileList.Font.Name := FInterfaceFonts[ifTableFont].FFontName;
      vstFileList.Font.Size:= FInterfaceFonts[ifTableFont].FFontSize;
      redtOutputResults.Font.Name := FInterfaceFonts[ifLogFont].FFontName;
      redtOutputResults.Font.Size := FInterfaceFonts[ifLogFont].FFontSize;
      If CompareText(strTheme, TStyleManager.ActiveStyle.Name) <> 0 Then
        TStyleManager.SetStyle(strTheme);
      OutputStats;
      actFileCompareExecute(Self);
    End;
End;

(**

  This is an on execute event handler for the Clear Log action.

  @precon  None.
  @postcon Clears the log.

  @param   Sender as a TObject

**)
procedure TfrmMainForm.actEditClearLogExecute(Sender: TObject);

begin
  If redtOutputResults.SelLength > 0 Then
    redtOutputResults.SelText := ''
  Else
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

  This is an on update event handler for the Fiole Operation actions.

  @precon  None.
  @postcon Enables / disables the file operations based on the opertation and the selected
           files.

  @param   Sender as a TObject

**)
Procedure TfrmMainForm.actEditFileOperationsUpdate(Sender: TObject);

Var
  boolEnabled : Boolean;
  Node: PVirtualNode;
  NodeData : PFSFileListNode;
  Action: TAction;
  
Begin
  If Sender Is TAction Then
    Begin
      Action := Sender As TAction;
      boolEnabled := vstFileList.SelectedCount > 0;
      Node := vstFileList.GetFirstSelected;
      While Assigned(Node) Do
        Begin
          NodeData := vstFileList.GetNodeData(Node);
          boolEnabled := boolEnabled And (Integer(NodeData.FState) <> Action.Tag);
          Case TFileOp(Tag) Of
            foLeftToRight:
              If NodeData.FLeftFilename = '' Then
                boolEnabled := False;
            foRightToLeft:
              If NodeData.FRightFilename = '' Then
                boolEnabled := False;
          End;
          Node := vstFileList.GetNextSelected(Node);
        End; 
      Action.Enabled := boolEnabled;
    End;
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
  FProgressForm.Progress(FProgressSection, 1, ' Done!', '');
  OutputResultLn(Format('%1.0n files in %1.0n bytes.',
      [Int(iFileCount), Int(iTotalSize)]));
End;

(**

  This is an on search event handler for the sync module.

  @precon  None.
  @postcon Updates the progress dialogue with the current number of files found and the 
           current folder and filename being searched.

  @param   strFolder   as a String
  @param   strFileName as a String
  @param   iCount      as an Integer
  @param   Update      as a TUpdateType

**)
Procedure TfrmMainForm.SearchProc(strFolder, strFileName: String; iCount: Integer;
  Update : TUpdateType);

Begin
  If strFolder = FLastFolder Then
    Begin
      If (iCount Mod 10 = 0) Or (Update = utImmediate) Then
        FProgressForm.Progress(FProgressSection, 0, Format('Searching: %s... %1.0n',
            [strFolder, Int(iCount)]), strFileName);
    End Else
      FLastFolder := strFolder;
End;

(**

  This is an on search start event handler for the sync module.

  @precon  None.
  @postcon Increments the progress section and initialises the progress bar.

  @param   strFolder as a String

**)
Procedure TfrmMainForm.SearchStartProc(strFolder: String);

Begin
  OutputResult(Format('Searching: %s', [strFolder]));
  Inc(FProgressSection);
  FProgressForm.InitialiseSection(FProgressSection, 0, 1);
End;

(**

  This method sets the file operations for the selected list view items.

  @precon  None.
  @postcon Sets the selected items in the list view to the File op state passed while also checking that
           the operation is valid for that item.

  @param   FileOp as a TFileOp as a constant

**)
Procedure TfrmMainForm.SetFileOperation(Const FileOp: TFileOp);

Var
  Node : PVirtualNode;
  NodeData : PFSFileListNode;

Begin
  Node := vstFileList.GetFirstSelected();
  While Assigned(Node) Do
    Begin
      NodeData := vstFileList.GetNodeData(Node);
      Case FileOp Of
        foLeftToRight:
          If NodeData.FLeftFilename = '' Then
            Continue;
        foRightToLeft:
          If NodeData.FRightFilename = '' Then
            Continue;
      End;
      NodeData.FState := FileOp;
      FSyncModule.Process[NodeData.FProcessIndex].FileOp := FileOp;
      Node := vstFileList.GetNextSelected(Node);
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

Begin
  iPos := Pos(':', Panel.Text);
  strText := Copy(Panel.Text, 1, iPos);
  R := Rect;
  StatusBar.Canvas.FillRect(R);
  Inc(R.Left, 4);
  StatusBar.Canvas.Font.Color := StyleServices.GetSystemColor(clWindowText);
  StatusBar.Canvas.Font.Style := [fsBold];
  StatusBar.Canvas.TextRect(R, strText, [tfLeft, tfVerticalCenter]);
  Inc(R.Left, StatusBar.Canvas.TextWidth(strText) + 2);
  StatusBar.Canvas.Font.Style := [];
  strText := Copy(Panel.Text, iPos + 1, Length(Panel.Text) - iPos);
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

  This method upgrades the old limited Folder options in the INI file to more flexible 
  options.

  @precon  iniMemFile must be a valid instance.
  @postcon Upgrades the old limited Folder options in the INI file to more flexible 
           options.

  @param   iniMemFile as a TMemIniFile

**)
Procedure TfrmMainForm.UpgradeINIFolderOptions(iniMemFile: TMemIniFile);

Var
  sl : TStringList;
  i : Integer;
  iSyncOptions : TSyncOptions;
  
Begin
  If Not iniMemFile.SectionExists('Folders') Then
    Exit;
  sl := TStringList.Create;
  Try
    iniMemFile.ReadSection('Folders', sl);
    For i := 0 To sl.Count - 1 Do
      Begin
        iniMemFile.WriteInteger('NewFolderStatus', Format('Folder%2.2d', [i]), 
          iniMemFile.ReadInteger('FolderStatus', sl[i], SmallInt(iSyncOptions)));
        iniMemFile.WriteString('NewFolderMaxFileSize', Format('Folder%2.2d', [i]), 
          iniMemFile.ReadString('FolderMaxFileSize', sl[i], '0,0,0,0'));
        iniMemFile.WriteString('NewFolders', Format('Folder%2.2d', [i]), 
          sl[i] + '|' + iniMemFile.ReadString('Folders', sl[i], ''));
      End;
  Finally
    sl.Free;
  End;
  iniMemFile.UpdateFile;
End;

(**

  This is an on free event handler for the virtual treeview.

  @precon  None.
  @postcon Ensures that managed types are freed from the node data.

  @param   Sender as a TBaseVirtualTree
  @param   Node   as a PVirtualNode

**)
Procedure TfrmMainForm.vstFileListFreeNode(Sender: TBaseVirtualTree; Node: PVirtualNode);

Var
  NodeData : PFSFileListNode;
  
Begin
  NodeData := vstFileList.GetNodeData(Node);
  Finalize(NodeData^);
End;

(**

  This is an on get image index event handler for the tree view.

  @precon  None.
  @postcon Returns the image index for the appropriate column.

  @param   Sender     as a TBaseVirtualTree
  @param   Node       as a PVirtualNode
  @param   Kind       as a TVTImageKind
  @param   Column     as a TColumnIndex
  @param   Ghosted    as a Boolean as a reference
  @param   ImageIndex as a TImageIndex as a reference

**)
Procedure TfrmMainForm.vstFileListGetImageIndex(Sender: TBaseVirtualTree; Node: PVirtualNode;
  Kind: TVTImageKind; Column: TColumnIndex; Var Ghosted: Boolean; Var ImageIndex: TImageIndex);

Var
  NodeData : PFSFileListNode;
  
Begin
  NodeData := Sender.GetNodeData(Node);
  If Kind In [ikNormal, ikSelected] Then
    Case Column Of
      0: ImageIndex := Integer(NodeData.FState);
    End;
End;

(**

  This is an on get text event handler for the vstTreeView.

  @precon  None.
  @postcon Returns the apporpriate text for the specified node and column.

  @param   Sender   as a TBaseVirtualTree
  @param   Node     as a PVirtualNode
  @param   Column   as a TColumnIndex
  @param   TextType as a TVSTTextType
  @param   CellText as a String as a reference

**)
Procedure TfrmMainForm.vstFileListGetText(Sender: TBaseVirtualTree; Node: PVirtualNode;
  Column: TColumnIndex; TextType: TVSTTextType; Var CellText: String);

Const
  strDateFmt = 'dd/mmm/yyyy hh:nn';

Var
  NodeData : PFSFileListNode;
  boolHasLeftFile: Boolean;
  boolHasRightFile: Boolean;
  
Begin
  NodeData := Sender.GetNodeData(Node);
  boolHasLeftFile := Length(NodeData.FLeftFilename) > 0;
  boolHasRightFile := Length(NodeData.FRightFilename) > 0;
  Case TFSVTVFields(Column) Of
    vfState:         CellText := '';
    vfLeftFileName:  CellText := NodeData.FLeftFilename;
    vfLeftAttr:      CellText := IfThen(boolHasLeftFile, GetAttributeString(NodeData.FLeftAttr), '');
    vfLeftSize:      CellText := IfThen(boolHasLeftFile, Format('%1.0n', [Int(NodeData.FLeftSize)]), '');
    vfLeftDate:      CellText := IfThen(boolHasLeftFile, FormatDateTime(strDateFmt, NodeData.FLeftDate), '');
    vfRightFileName: CellText := NodeData.FRightFilename;
    vfRightAttr:     CellText := IfThen(boolHasRightFile, GetAttributeString(NodeData.FRightAttr), '');
    vfRightSize:     CellText := IfThen(boolHasRightFile, Format('%1.0n', [Int(NodeData.FRightSize)]), '');
    vfRightDate:     CellText := IfThen(boolHasRightFile, FormatDateTime(strDateFmt, NodeData.FRightDate), '');
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

  This is an on error message event handler for outputting error message from the deleting
  ans copying process.

  @precon  None.
  @postcon Outputs an error message to the log file.

  @param   strErrorMsg as a String

**)
Procedure TfrmMainForm.ErrorMessage(strErrorMsg : String);

Begin
  OutputResultLn(Format('  %s', [strErrorMsg]));
End;

(**

  This is an on error message end event handler for the outputting of error messages from
  the deleting and copying processes.

  @precon  None.
  @postcon Does nothing.

  @nocheck EmptyMethod

**)
Procedure TfrmMainForm.ErrorMessageEnd;

Begin
End;

(**

  This is an on error message start event handler for the outputting of error messages
  from the deleting and copyin processes.

  @precon  None.
  @postcon Ouputs an error message header to the log file if there are errors to output.

  @param   iFileCount as an Integer

**)
Procedure TfrmMainForm.ErrorMessageStart(iFileCount: Integer);

Begin
  If iFileCount > 0 Then
    OutputResultLn(Format('%1.0n Files had errors...', [Int(iFileCount)]));
End;

(**

  This is an on exceeeds size limit event handler.

  @precon  None.
  @postcon Outputs the given file to the log.

  @param   iFile       as an Integer
  @param   iFileCount  as an Integer
  @param   strLPath    as a String
  @param   strRPath    as a String
  @param   strFileName as a String

**)
Procedure TfrmMainForm.ExceedsSizeLimit(iFile, iFileCount: Integer; strLPath, strRPath,
    strFileName: String);

Begin
  OutputFileNumber(iFile, iFileCount);
  OutputResultLn(Format('%s => %s%s', [strLPath, strRPath, strFileName]));
End;

(**

  This is an on exceeds size limit end event handler.

  @precon  None.
  @postcon Does nothing.

  @nocheck EmptyMethod

**)
Procedure TfrmMainForm.ExceedsSizeLimitEnd;

Begin
End;

(**

  This is an on exceeds size limit start event handler.

  @precon  None.
  @postcon Outputs a header for the list of over sizeed files in the log.

  @param   iFileCount as an Integer

**)
Procedure TfrmMainForm.ExceedsSizeLimitStart(iFileCount: Integer);

Begin
  If iFileCount > 0 Then
    OutputResultLn(Format('%1.0n Files exceeding Size Limit...', [Int(iFileCount)]));
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

Var
  boolAbort : Boolean;
  
Begin
  If TfrmDiskSpace.Execute(FSyncModule) Then
    Begin
      OutputResultLn('Processing started @ ' + FormatDateTime('dddd dd mmmm yyyy hh:mm:ss',
        Now()));
      Try
        boolAbort := False;
        DisableActions;
        Try
          Try
            FSyncModule.ProcessFiles(FFldrSyncOptions);
          Except
            On E : EAbort Do
              boolAbort := True;
          End;
        Finally
          EnableActions(True);
        End;
        actFileProcessFiles.Enabled := Not boolAbort
      Finally
        OutputResultLn();
      End;
      If Not boolAbort Then
        actFileCompareExecute(Self);
    End;
End;

(**

  This is an on execute event hanlder for the Help About action.

  @precon  None.
  @postcon Displays the about dialogue.

  @param   Sender as a TObject

**)
Procedure TfrmMainForm.actHelpAboutExecute(Sender: TObject);

Begin
  TfrmAboutDialogue.ShowAbout(Self);
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

Begin
  vstFileList.SelectAll(True);
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

    This is a local method to check and confirm the creation of a folder. It raises an 
    exception if the folder could not be created. The drive of the folder mapping is
    returned in the strDrive var parameter.

    @precon  None.
    @postcon Checks and confirms the creation of a folder. It raises an exception if the 
             folder could not be created.

    @param   strFolder as a String
    @param   strDrive  as a String as a reference
    @return  a Boolean

  **)
  Function CheckAndCreateFolder(strFolder: String;  var strDrive : String) : Boolean;

  Const
    strExcepMsg  = 'Could not create the folder "%s".';
    strCreateMsg = 'The folder "%s" does not exist. Would you like to create ' +
      'this folder?';
    strDriveMsg = 'The drive "%s" for directory "%s" does not exist. Would you like ' +
      'to ignore or cancel?';

  Var
    strPath: String;

  Begin
    Result := False;
    strDrive := ExtractFileDrive(strFolder);
    strPath := ExtractFilePath(strFolder);
    If Not SysUtils.DirectoryExists(strDrive + '\') Then
      Case MessageDlg(Format(strDriveMsg, [strDrive, strPath]), mtConfirmation,
        [mbIgnore, mbCancel], 0) Of
        mrIgnore:
          Begin
            Result := True;
            Exit;
          End;
        mrCancel: Abort;
      End;
    If Not SysUtils.DirectoryExists(strPath) Then
      Case MessageDlg(Format(strCreateMsg, [strPath]), mtConfirmation,
        [mbYes, mbNo, mbCancel], 0) Of
        mrYes:
          If Not SysUtils.ForceDirectories(strPath) Then
            Raise TFolderNotFoundException.CreateFmt(strExcepMsg, [strPath]);
        mrIgnore: Result := True;
        mrCancel: Abort;
      End;
  End;

  (**

    This method disabled all folder pairs with the given drive mapping.

    @precon  None.
    @postcon Disabled all folder pairs with the given drive mapping.

    @param   strDrive as a String

  **)
  Procedure DisableDrive(strDrive : String);

  Var
    i : Integer;
    F : TFolder;
    
  Begin
    For i := 0 To FFolders.Count - 1 Do
      Begin
        F := FFolders.Folder[i];
        If (CompareText(Copy(F.LeftFldr, 1, Length(strDrive)), strDrive) = 0) Or
           (CompareText(Copy(F.RightFldr, 1, Length(strDrive)), strDrive) = 0) Then
          F.SyncOptions := F.SyncOptions + [soTempDisabled];
      End;
  End;

Var
  i: Integer;
  strDrive: String;

Begin
  For i := 0 To FFolders.Count - 1 Do
    If [soEnabled, soTempDisabled] * FFolders.Folder[i].SyncOptions = [soEnabled] Then
      Begin
        If CheckAndCreateFolder(FFolders.Folder[i].LeftFldr, strDrive) Then
          Begin
            DisableDrive(strDrive);
            Continue;
          End;
        If CheckAndCreateFolder(FFolders.Folder[i].RightFldr, strDrive) Then
          DisableDrive(strDrive);
      End Else
      Begin
        If soTempDisabled In FFolders.Folder[i].SyncOptions Then
          Begin
            If SysUtils.DirectoryExists(ExtractFileDrive(FFolders.Folder[i].LeftFldr) + '\') And
             SysUtils.DirectoryExists(ExtractFileDrive(FFolders.Folder[i].RightFldr) + '\') Then
             FFolders.Folder[i].SyncOptions := FFolders.Folder[i].SyncOptions - [soTempDisabled];
          End;
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
  FProgressForm.Progress(FProgressSection, 100, 'Done.', '');
  OutputResultLn('Done.');
End;

(**

  This is an on compare event handler for the sync module.

  @precon  None.
  @postcon Updates the progress dialogue with the current progress through the comparison
           process.

  @nohint  strLeftFldr strRightFldr

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
  OutputResult(Format('Comparing: ', [strLeftFldr, strRightFldr]));
  Inc(FProgressSection);
  FProgressForm.InitialiseSection(FProgressSection, 0, 100);
End;

(**

  This is an on copied event handler for the sync module.

  @precon  None.
  @postcon Updates the copy progress or if an error outputs the error message.

  @nohint  iCurrentFileToCopy iTotalFilesToCopy

  @param   iCurrentFileToCopy           as an Integer
  @param   iTotalFilesToCopy            as an Integer
  @param   iCumulativeFileSizeAfterCopy as an Int64
  @param   iTotalFileSizeToCopy         as an Int64
  @param   iSuccess                     as a TProcessSuccess

**)
Procedure TfrmMainForm.CopiedProc(iCurrentFileToCopy, iTotalFilesToCopy: Integer;
    iCumulativeFileSizeAfterCopy, iTotalFileSizeToCopy: Int64;
    iSuccess : TProcessSuccess);

Begin
  FCopyForm.IndividualProgress(0, 0 ,iCumulativeFileSizeAfterCopy, iTotalFileSizeToCopy);
  If FTotalSize = 0 Then
    Inc(FTotalSize);
  Case iSuccess Of
    psSuccessed: OutputResultLn();
    psFailed: OutputResultLn(
      Format(' Error Copying file (error type ignored [%s]).', [FSyncModule.LastError]));
    //psIgnored: OutputResultLn(' Ignored Copying file.');
  End;
End;

(**

  This is an on copy contents event handler for the sync module.

  @precon  None.
  @postcon Updates the progress of the individual file copy.

  @nohint  iCurrentFileToCopy iTotalFilesToCopy

  @param   iCurrentFileToCopy            as an Integer
  @param   iTotalFilesToCopy             as an Integer
  @param   iCumulativeFileSizeBeforeCopy as an Int64
  @param   iTotalFileSizeToCopy          as an Int64
  @param   iCurrentFileCopiedSizeSoFar   as an Int64
  @param   iTotalCurrentFileSize         as an Int64

**)
Procedure TfrmMainForm.CopyContentsProc(iCurrentFileToCopy, iTotalFilesToCopy : Integer;
    iCumulativeFileSizeBeforeCopy, iTotalFileSizeToCopy, iCurrentFileCopiedSizeSoFar,
    iTotalCurrentFileSize: Int64);

Begin
  FCopyForm.IndividualProgress(iCurrentFileCopiedSizeSoFar, iTotalCurrentFileSize,
    iCumulativeFileSizeBeforeCopy, iTotalFileSizeToCopy);
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
  OutputResult(Format('  Copied %1.0n (Skipped %1.0n file(s)',
    [Int(iCopied), Int(iSkipped)]));
  If iError > 0 Then
    OutputResult(Format(', Errored %1.0n file(s)', [Int(iError)]));
  OutputResultLn(')');
  If FCopyForm <> Nil Then
    FCopyDlgWidth := FCopyForm.Width;
  FreeAndNil(FCopyForm);
End;

(**

  This is an on error copting event handler for the processing of the files.

  @precon  None.
  @postcon Displays the error message on the screen and asks the user what to do.

  @param   strSource   as a String
  @param   strDest     as a String
  @param   strErrorMsg as a String
  @param   iLastError  as a Cardinal
  @param   iResult     as a TDGHErrorResult as a reference

**)
Procedure TfrmMainForm.CopyError(strSource, strDest, strErrorMsg: String;
  iLastError: Cardinal; Var iResult: TDGHErrorResult);

Begin
  UpdateTaskBar(ptError, 0, 1);
  OutputResultLn();
  OutputResultLn('    An error has occurred during the copying of files:');
  OutputResultLn(Format('      Source     : %s', [strSource]));
  OutputResultLn(Format('      Destination: %s', [strDest]));
  OutputResultLn(Format('      OS Error   : (%d) %s', [iLastError, strErrorMsg]));
  OutputResult('    Do you want to [I]gnore Once, Ignore [A]ll the errors or [S]top processing? ');
  Case TfrmErrorDlg.Execute('An error has occurred during the copying of files', 
    strSource, strDest, Format('(%d) %s', [iLastError, strErrorMsg])) Of
    mrIgnore:
      Begin
        iResult := derIgnoreOnce;
        OutputResultLn('I');
      End;
    mrYesToAll:
      Begin
        iResult := derIgnoreAll;
        OutputResultLn('A');
      End;
    mrAbort:
      Begin
        iResult := derStop;
        OutputResultLn('S');
      End;
  End;
End;

(**

  This is an on copying event handler for the sync module.

  @precon  None.
  @postcon Updates the copy dialogue.

  @nohint  iCumulativeFileSizeBeforeCopy iTotalFileSizeToCopy

  @param   iCurrentFileToCopy            as an Integer
  @param   iTotalFilesToCopy             as an Integer
  @param   iCumulativeFileSizeBeforeCopy as an Int64
  @param   iTotalFileSizeToCopy          as an Int64
  @param   strSource                     as a String
  @param   strDest                       as a String
  @param   strFileName                   as a String

**)
Procedure TfrmMainForm.CopyingProc(iCurrentFileToCopy, iTotalFilesToCopy : Integer;
    iCumulativeFileSizeBeforeCopy, iTotalFileSizeToCopy: Int64; strSource, strDest,
    strFileName: String);

Begin
  FCopyForm.Progress(
    iCurrentFileToCopy,
    iTotalFilesToCopy,
    ExtractFilePath(strSource + strFileName),
    ExtractFilePath(strDest + strFileName),
    ExtractFileName(strFileName)
  );
  OutputFileNumber(iCurrentFileToCopy, iTotalFilesToCopy);
  OutputResult(Format('%s => %s%s', [strSource, strDest, strFilename]));
End;

(**

  This is an on copy query event handler for the sync module.

  @precon  None.
  @postcon Displays a confirmation dialogue to the user to ask whether the file should be
           overwritten.

  @param   strSourcePath as a String
  @param   strDestPath   as a String
  @param   SourceFile    as a TFileRecord
  @param   DestFile      as a TFileRecord
  @param   Option        as a TFileAction as a reference

**)
Procedure TfrmMainForm.CopyQueryProc(strSourcePath, strDestPath: String;
  SourceFile, DestFile : TFileRecord; Var Option: TFileAction);

Const
  strMsg = 'Are you sure you want to overwrite the following file?';
  strConsoleMsg = ' Overwrite (Y/N/A/O/C)? ';

Begin
  FileQuery(strMsg, strConsoleMsg,strSourcePath, strDestPath, SourceFile, DestFile,
    Option, False);
End;

(**

  This is an on copy read only query event handler for the sync module.

  @precon  None.
  @postcon Displays a confirmation dialogue to the user to ask whether the read only file
           should be overwritten.

  @param   strSourcePath as a String
  @param   strDestPath   as a String
  @param   SourceFile    as a TFileRecord
  @param   DestFile      as a TFileRecord
  @param   Option        as a TFileAction as a reference

**)
Procedure TfrmMainForm.CopyReadOnlyQueryProc(strSourcePath, strDestPath: String;
  SourceFile, DestFile : TFileRecord; Var Option: TFileAction);

Const
  strMsg = 'Are you sure you want to overwrite the following READ-ONLY file?';
  strConsoleMsg = ' Overwrite READONLY (Y/N/A/O/C)? ';

Begin
  FileQuery(strMsg, strConsoleMsg, strSourcePath, strDestPath, SourceFile, DestFile,
    Option, True);
End;

(**

  This is an on copy start event handler for the sync module.

  @precon  None.
  @postcon Initialises the copy progress dialogue.

  @param   iTotalCount as an Integer
  @param   iTotalSize  as an int64

**)
Procedure TfrmMainForm.CopyStartProc(iTotalCount: Integer; iTotalSize: int64);

Begin
  FTotalSize := iTotalSize;
  OutputResultLn(Format('Copying %1.0n files (%1.0n bytes)',
      [Int(iTotalCount), Int(iTotalSize)]));
  If iTotalCount > 0 Then
    Begin
      FCopyForm                  := TfrmCopyProgress.Create(Self);
      FCopyForm.OnUpdateProgress := UpdateProgress;
      FCopyForm.Initialise(iTotalCount, iTotalSize, FCopyDlgWidth);
      FDialogueBottom := FCopyForm.Top + FCopyForm.Height;
    End;
End;

(**

  This method displays a dialogue asking the user to confirm the file action they wish to 
  take based on the message displayed. The answer to the dialogue is returned to the sync 
  module.

  @precon  None.
  @postcon The users response to the query is passed back to the sync module.

  @param   strMsg        as a String
  @param   strConsoleMsg as a String
  @param   strSourcePath as a String
  @param   strDestPath   as a String
  @param   SourceFile    as a TFileRecord
  @param   DestFile      as a TFileRecord
  @param   Option        as a TFileAction as a reference
  @param   boolReadOnly  as a Boolean

**)
Procedure TfrmMainForm.FileQuery(strMsg, strConsoleMsg, strSourcePath,
  strDestPath: String; SourceFile, DestFile : TFileRecord; Var Option: TFileAction;
  boolReadOnly : Boolean);

Begin
  If (Not (Option In [faYesToAll, faNoToAll])     And Not boolReadOnly) Or
     (Not (Option In [faYesToAllRO, faNoToAllRO]) And     boolReadOnly) Then
    Begin
      OutputResult(strConsoleMsg);
      UpdateTaskBar(ptError, 0, 1);
      Case TfrmConfirmationDlg.Execute(Self, strMsg, strSourcePath, strDestPath,
        SourceFile, DestFile, FDialogueBottom, FConfirmDlgWidth) Of
        mrYes:
          Option := faYes;
        mrNo:
          Option := faNo;
        mrAll:
          If Not boolReadOnly Then
            Option := faYesToAll
          Else
            Option := faYesToAllRO;
        mrIgnore:
          If Not boolReadOnly Then
            Option := faNoToAll
          Else
            Option := faNoToAllRO;
      Else
        Option := faCancel;
      End;
      OutputResult(strFileOptions[Option]);
    End;
End;

(**

  This is an on deleted event handler for the sync module.

  @precon  None.
  @postcon Updates the progress dialogue is successful or outputs an error message.

  @nohint  iCurrentFileToDeleted iTotalFilesToDelete 

  @param   iCurrentFileToDeleted          as an Integer
  @param   iTotalFilesToDelete            as an Integer
  @param   iCumulativeFileSizeAfterDelete as an Int64
  @param   iTotalFileSizeToDelete         as an Int64
  @param   iSuccess                       as a TProcessSuccess

**)
Procedure TfrmMainForm.DeletedProc(iCurrentFileToDeleted, iTotalFilesToDelete: Integer;
    iCumulativeFileSizeAfterDelete, iTotalFileSizeToDelete: Int64;
    iSuccess : TProcessSuccess);

Begin
  FDeleteForm.Progress(iCumulativeFileSizeAfterDelete, iTotalFileSizeToDelete);
  If FTotalSize = 0 Then
    Inc(FTotalSize);
  Case iSuccess Of
    psSuccessed: OutputResultLn();
    psFailed: OutputResultLn(
      Format(' Error Deleting file (error type ignored [%s]).', [FSyncModule.LastError]));
    //psIgnored: OutputResultLn(' Ignored Copying file.');
  End;
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
  OutputResult(Format('  Deleted %1.0n file(s) (Skipped %1.0n file(s)', [Int(iDeleted),
    Int(iSkipped)]));
  If iErrors > 0 Then
    OutputResult(Format(', Errored %1.0n file(s)', [Int(iErrors)]));
  OutputResultLn(').');
  If FDeleteForm <> Nil Then
    FDeleteDlgWidth := FDeleteForm.Width;
  FreeAndNil(FDeleteForm);
End;

(**

  This is an on delete error event handler for the Sync Module.

  @precon  None.
  @postcon Displays the error message on the screen and asks the user what to do.

  @param   strSource   as a String
  @param   strErrorMsg as a String
  @param   iLastError  as a Cardinal
  @param   iResult     as a TDGHErrorResult as a reference

**)
Procedure TfrmMainForm.DeleteError(strSource, strErrorMsg: String; iLastError: Cardinal;
  Var iResult: TDGHErrorResult);

Begin
  UpdateTaskBar(ptError, 0, 1);
  OutputResultLn();
  OutputResultLn('    An error has occurred during the deleting of files:');
  OutputResultLn(Format('      Source     : %s', [strSource]));
  OutputResultLn(Format('      OS Error   : (%d) %s', [iLastError, strErrorMsg]));
  OutputResult('    Do you want to [I]gnore Once, Ignore [A]ll the errors or [S]top processing? ');
  Case TfrmErrorDlg.Execute('An error has occurred during the deleting of files',
    strSource, '', Format('(%d) %s', [iLastError, strErrorMsg])) Of
    mrIgnore:
      Begin
        iResult := derIgnoreOnce;
        OutputResultLn('I');
      End;
    mrYesToAll:
      Begin
        iResult := derIgnoreAll;
        OutputResultLn('A');
      End;
    mrAbort:
      Begin
        iResult := derStop;
        OutputResultLn('S');
      End;
  End;
End;

(**

  This is an on delete folders event handler.

  @precon  None.
  @postcon Outputs the folder to be deleted to the log and updates the progress.

  @param   iFolder   as an Integer
  @param   iFolders  as an Integer
  @param   strFolder as a String

**)
Procedure TfrmMainForm.DeleteFolders(iFolder, iFolders : Integer; strFolder: String);

Begin
  FDeleteForm.InitialiseFileName(dtFiles, iFolder, iFolders, strFolder);
  FDeleteForm.Progress(iFolder, iFolders);
  OutputFileNumber(iFolder, iFolders);
  OutputResultLn(strFolder);
End;

(**

  This is an on delete folders end event handler.

  @precon  None.
  @postcon Frees the memory used by the form.

**)
Procedure TfrmMainForm.DeleteFoldersEnd;

Begin
  If FDeleteForm <> Nil Then  
    FDeleteDlgWidth := FDeleteForm.Width;
  FreeAndNil(FDeleteForm);
End;

(**

  This is an on delete folders start event handler.

  @precon  None.
  @postcon Outputs the number of folders to be deleted to the log and initialises the
           delete form.

  @param   iFolderCount as an Integer

**)
Procedure TfrmMainForm.DeleteFoldersStart(iFolderCount: Integer);

Begin
  If iFolderCount > 0 Then
    Begin
      OutputResultLn('Deleting empty folders...');
      FDeleteForm                  := TfrmDeleteProgress.Create(Self);
      FDeleteForm.OnUpdateProgress := UpdateProgress;
      FDeleteForm.Initialise(dtFolders, iFolderCount, iFolderCount, FDeleteDlgWidth);
      FDialogueBottom := FDeleteForm.Top + FDeleteForm.Height;
    End;
End;

(**

  This method is an on delete query event handler for the sync module.

  @precon  None.
  @postcon Displays a confirmation dialogue to the user asking whether the file should be
           deleted.

  @param   strFilePath as a String
  @param   DeleteFile  as a TFileRecord
  @param   Option      as a TFileAction as a reference

**)
Procedure TfrmMainForm.DeleteQueryProc(strFilePath: String; DeleteFile : TFileRecord;
  Var Option: TFileAction);

Const
  strMsg = 'Are you sure you want to delete the file?';
  strConsoleMsg = ' Delete (Y/N/A/O/C)? ';

Begin
  FileQuery(strMsg, strConsoleMsg, strFilePath, '', DeleteFile, Nil, Option, False);
End;

(**

  This method is an on delete readonly query event handler for the sync module.

  @precon  None.
  @postcon Displays a confirmation dialogue to the user asking whether the read only file
           should be deleted.

  @param   strFilePath as a String
  @param   DeleteFile  as a TFileRecord
  @param   Option      as a TFileAction as a reference

**)
Procedure TfrmMainForm.DeleteReadOnlyQueryProc(strFilePath: String;
  DeleteFile : TFileRecord; Var Option: TFileAction);

Const
  strMsg = 'Are you sure you want to delete the READ-ONLY file?';
  strConsoleMsg = ' Delete READONLY (Y/N/A/O/C)? ';

Begin
  FileQuery(strMsg, strConsoleMsg, strFilePath, '', DeleteFile, Nil, Option, True);
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
  FTotalSize := iTotalSize;
  OutputResultLn(Format('Deleting %1.0n files (%1.0n bytes)...',
      [Int(iFileCount), Int(iTotalSize)]));
  If iFileCount > 0 Then
    Begin
      FDeleteForm                  := TfrmDeleteProgress.Create(Self);
      FDeleteForm.OnUpdateProgress := UpdateProgress;
      FDeleteForm.Initialise(dtFiles, iFileCount, iTotalSize, FDeleteDlgWidth);
      FDialogueBottom := FDeleteForm.Top + FDeleteForm.Height;
    End;
End;

(**

  This is an on delete event handler for the sync module.

  @precon  None.
  @postcon Output the file to be deleted to the deletion progress form.

  @nohint  iCumulativeFileSizeBeforeDelete iTotalFileSizeToDelete

  @param   iCurrentFileToDelete            as an Integer
  @param   iTotalFilesToDelete             as an Integer
  @param   iCumulativeFileSizeBeforeDelete as an Int64
  @param   iTotalFileSizeToDelete          as an Int64
  @param   strDeletePath                   as a String
  @param   strFileNameToDelete             as a String

**)
Procedure TfrmMainForm.DeletingProc(iCurrentFileToDelete, iTotalFilesToDelete : Integer;
    iCumulativeFileSizeBeforeDelete, iTotalFileSizeToDelete: Int64;
    strDeletePath, strFileNameToDelete: String);

Begin
  FDeleteForm.InitialiseFileName(dtFiles, iCurrentFileToDelete, iTotalFilesToDelete,
    strDeletePath + strFileNameToDelete);
  OutputFileNumber(iCurrentFileToDelete, iTotalFilesToDelete);
  OutputResult(strDeletePath + strFileNameToDelete);
End;

(**

  This is an on diff size event handler for the sync module.

  @precon  None.
  @postcon Outputs the name of the files that have a difference size but the same date
           and time.

  @param   iFile       as an Integer
  @param   iFileCount  as an Integer
  @param   strLPath    as a String
  @param   strRPath    as a String
  @param   strFileName as a String

**)
procedure TfrmMainForm.DiffSize(iFile, iFileCount: Integer; strLPath, strRPath,
    strFileName: String);

begin
  OutputFileNumber(iFile, iFileCount);
  OutputResultLn(Format('%s => %s%s', [strLPath, strRPath, strFileName]));
end;

(**

  This is an on diff size end event handler for the sync module.

  @precon  None.
  @postcon None.

  @nocheck EmptyMethod

**)
procedure TfrmMainForm.DiffSizeEnd;

begin
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
    OutputResultLn(Format('%1.0n File(s) with Differing Size...', [Int(iFileCount)]));
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

  This method updates the status images of the list view based on the filename / extension of the file.

  @precon  None.
  @postcon Updates the status images of the list view based on the filename / extension of the file.

  @param   ProcessItem as a TProcessItem as a constant
  @param   NodeData    as a PFSFileListNode as a constant

**)
Procedure TfrmMainForm.ImageIndexes(Const ProcessItem : TProcessItem; Const NodeData : PFSFileListNode);

Begin //: @todo Defer until the display of text!
  If Assigned(ProcessItem.LeftFile) Then
    Begin
      If Assigned(ProcessItem.LeftFile) Then
        NodeData.FLeftFileIndex := GetImageIndex(ProcessItem,
          ProcessItem.LPath + ProcessItem.LeftFile.FileName)
      Else
        NodeData.FLeftFileIndex := -1;
      If Assigned(ProcessItem.RightFile) Then
        NodeData.FRightFileIndex := GetImageIndex(ProcessItem,
          ProcessItem.LPath + ProcessItem.RightFile.FileName)
      Else
        NodeData.FRightFileIndex := -1;
    End Else
    Begin
      If Assigned(ProcessItem.LeftFile) Then
        NodeData.FLeftFileIndex := GetImageIndex(ProcessItem,
          ProcessItem.RPath + ProcessItem.LeftFile.FileName)
      Else
        NodeData.FLeftFileIndex := -1;
      If Assigned(ProcessItem.RightFile) Then
        NodeData.FRightFileIndex := GetImageIndex(ProcessItem,
          ProcessItem.RPath + ProcessItem.RightFile.FileName)
      Else
        NodeData.FRightFileIndex := -1;
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

(**

  This method outputs the file number and the total number of files to the output window.

  @precon  None.
  @postcon Outputs the file number and the total number of files to the output window.

  @param   iCurrentFile as an Integer
  @param   iTotal       as an Integer

**)
Procedure TfrmMainForm.OutputFileNumber(iCurrentFile, iTotal : Integer);

Var
  strTotal : String;
  iSize : Integer;

Begin
  strTotal := Format('%1.0n', [Int(iTotal)]);
  iSize    := Length(strTotal);
  OutputResult(#32#32 + Format('(%*.0n/%*.0n) ',
      [iSize, Int(iCurrentFile), iSize, Int(iTotal)]));
End;

End.
