(**

  This module defines the options dialogue.

  @Date    11 Mar 2016
  @Version 1.0
  @Author  David Hoyle

**)
Unit OptionsForm;

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
  StdCtrls,
  Buttons,
  ComCtrls,
  CheckLst,
  SyncModule,
  ImgList, System.ImageList;

Type
  (** An enumerate to describe the Interface fonts that can be modified. **)
  TInterfaceFont = (ifTableFont, ifLogFont);
  
  (** A record to describe the attributes of the interface fonts that can be modified. **)
  TInterfaceFontRecord = Record
    FFontName : String;
    FFontSize : Integer;
  End;

  (** A record to describe the file operation font attributes that can be modified. **)
  TFileOperationFontRecord = Record
    FININame      : String;
    FFontColour   : TColor;
    FBGFontColour : TColor;
    FFontStyle    : TFontStyles;
  End;

  (** A type to define the Interface font data that can be editied by this dialogue. **)
  TInterfaceFontInfo = Array[Low(TInterfaceFont)..High(TInterfaceFont)] Of
    TInterfaceFontRecord;

  (** An enumerate to define the File Operation font that can be editied (more than the
      file operations alone). **)
  TFileOpFont = (fofNothing, fofLeftToRight, fofRightToLeft, fofDelete, fofSizeDiff,
    fofExceedsSizeLimit, fofReadOnly, fofMissingFile);

  (** A type to define the File Operation font data that can be editied by this
      dialogue. **)
  TFileOperationFontInfo = Array[Low(TFileOpFont)..High(TFileOpFont)] Of
    TFileOperationFontRecord;

  (** This is a class to represent **)
  TfrmOptions = Class(TForm)
    btnOK: TBitBtn;
    btnCancel: TBitBtn;
    edtExclusions: TMemo;
    lvFolders: TListView;
    btnAdd: TBitBtn;
    btnEdit: TBitBtn;
    btnDelete: TBitBtn;
    edtCompareEXE: TEdit;
    lblCompareFiles: TLabel;
    btnBrowse: TButton;
    dlgOpen: TOpenDialog;
    btnCheckforUpdates: TBitBtn;
    lblExclusions: TLabel;
    ilStatus: TImageList;
    btnHelp: TBitBtn;
    cbxThemes: TComboBox;
    lblThemes: TLabel;
    lbxFldrSyncOps: TListView;
    pagPages: TPageControl;
    tabFolders: TTabSheet;
    tabFonts: TTabSheet;
    tabGlobalOptions: TTabSheet;
    tabExclusions: TTabSheet;
    lblInterfaceFonts: TLabel;
    lbxInterfaceFonts: TListBox;
    lblFileOperationFonts: TLabel;
    lbxFileOperationFonts: TListBox;
    btnInterfaceFontEdit: TBitBtn;
    btnFileOperationFontEdit: TBitBtn;
    btnUp: TBitBtn;
    btnDown: TBitBtn;
    btnCopy: TBitBtn;
    lvFileOpStats: TListView;
    Procedure lvFoldersResize(Sender: TObject);
    Procedure btnAddClick(Sender: TObject);
    Procedure btnEditClick(Sender: TObject);
    Procedure btnDeleteClick(Sender: TObject);
    Procedure lvFoldersDblClick(Sender: TObject);
    Procedure FormCreate(Sender: TObject);
    Procedure FormDestroy(Sender: TObject);
    Procedure btnBrowseClick(Sender: TObject);
    Procedure btnCheckforUpdatesClick(Sender: TObject);
    Procedure lvFoldersSelectItem(Sender: TObject; Item: TListItem; Selected: Boolean);
    Procedure lvFoldersChange(Sender: TObject; Item: TListItem; Change: TItemChange);
    Procedure lvFoldersCustomDrawItem(Sender: TCustomListView; Item: TListItem;
      State: TCustomDrawState; Var DefaultDraw: Boolean);
    Procedure btnHelpClick(Sender: TObject);
    procedure lbxInterfaceFontsDrawItem(Control: TWinControl; Index: Integer; Rect: TRect;
      State: TOwnerDrawState);
    procedure lbxFileOperationFontsDrawItem(Control: TWinControl; Index: Integer;
      Rect: TRect; State: TOwnerDrawState);
    procedure btnInterfaceFontEditClick(Sender: TObject);
    procedure tabFontsResize(Sender: TObject);
    procedure btnFileOperationFontEditClick(Sender: TObject);
    procedure btnUpClick(Sender: TObject);
    procedure btnDownClick(Sender: TObject);
    procedure btnCopyClick(Sender: TObject);
  Private
    { Private declarations }
    FRightWidth     : Integer;
    FLeftWidth      : Integer;
    FINIFileName    : String;
    FFolders        : TFolders;
    FInterfaceFonts : TInterfaceFontInfo;
    FFileOpFonts    : TFileOperationFontInfo;
    Procedure SetLeftWidth(Const Value: Integer);
    Procedure SetRightWidth(Const Value: Integer);
    Procedure PopulateFolderList;
    (**
      This property holds the maximum width of the Right Folder Text.
      @precon  None.
      @postcon Gets or sets the maximum width of the Right Folder Text.
      @return  a Integer
    **)
    Property RightWidth: Integer Read FRightWidth Write SetRightWidth;
    (**
      This property holds the maximum width of the Left Folder Text.
      @precon  None.
      @postcon Gets or sets the maximum width of the Left Folder Text.
      @return  a Integer
    **)
    Property LeftWidth: Integer Read FLeftWidth Write SetLeftWidth;
  Public
    { Public declarations }
    Class Function Execute(Folders: TFolders; Var strExclusions, strCompareEXE: String;
      strINIFileName: String; var InterfaceFonts : TInterfaceFontInfo;
      var FileOpFonts : TFileOperationFontInfo; Var FldrSyncOps: TFldrSyncOptions;
      var strTheme : String; var FileOpStats : TFileOpStats): Boolean;
    Constructor CreateWithRootKey(AOwner: TComponent; strRootKey: String); Virtual;
  End;

  (** This is a record to describe the INI file parameters for storage of the
      Application options. **)
  TOptionsInfo = Record
    FINISection: String;
    FINIKey: String;
    FDescription: String;
    FDefault: Boolean;
  End;

Const
  (** A constant array of strings corresponding to the OLD folder sync options. **)
  strOLDFldrSyncOptions: Array [Low(TOLDFldrSyncOption) .. High(TOLDFldrSyncOption)] Of TOptionsInfo =
    (
      (FINISection: 'AppOptions';
       FINIKey: 'CloseOnNoFiles';
       FDescription: 'Close Folder Sync IF there are no files to processes after comparison.';
       FDefault: False),
      (FINISection: 'AppOptions';
       FINIKey: 'RespondWithYes';
       FDescription: 'Respond with "Yes to All" for any dialogue boxes that are displayed.';
       FDefault: False),
      (FINISection: 'AppOptions';
       FINIKey: 'DoNotConfirmDir';
       FDescription: 'Do not confirm the creation of a new directories if the operation requires one to be created.';
       FDefault: True),
      (FINISection: 'AppOptions';
       FINIKey: 'DisplayProgress';
       FDescription: 'Display a progress dialogue box but do not show the file names.';
       FDefault: False),
      (FINISection: 'AppOptions';
       FINIKey: 'StartAutomatically';
       FDescription: 'Start processing the files after comparison automatically (DANGEROUS!).';
       FDefault: False),
      (FINISection: 'AppOptions';
       FINIKey: 'HideLongFiles';
       FDescription: 'Hide file that are too long to copy or delete.';
       FDefault: False)
    );
  (** A constant array of strings corresponding to the NEW folder sync options. **)
  strFldrSyncOptions: Array [Low(TFldrSyncOption) .. High(TFldrSyncOption)] Of TOptionsInfo =
    (
      (FINISection: 'AppOptions';
       FINIKey: 'CloseOnNoFiles';
       FDescription: 'Close Folder Sync IF there are no files to processes after comparison.';
       FDefault: False),
      (FINISection: 'AppOptions';
       FINIKey: 'StartAutomatically';
       FDescription: 'Start processing the files after comparison automatically (DANGEROUS!).';
       FDefault: False),
      (FINISection: 'AppOptions';
       FINIKey: 'PermanentlyDeleteFiles';
       FDescription: 'Permanently delete files rather than delete them to the recycle bin.';
       FDefault: False),
      (FINISection: 'AppOptions';
       FINIKey: 'BatchDeleteFiles';
       FDescription: 'Batch delete files rather than delete them individually to the recycle bin.';
       FDefault: False)
    );
  (** A constant array of strings that name of the Interface fonts. **)
  strInterfaceFonts : Array[Low(TInterfaceFont)..High(TInterfaceFont)] Of String = (
    'Table Font', 'Log Font');
  (** A constant array of default options for the interface fonts. **)
  InterfaceFontDefaults : Array[Low(TInterfaceFont)..High(TInterfaceFont)] Of TInterfaceFontRecord = (
    (FFontName: 'Tahoma'; FFontSize: 9),
    (FFontName: 'Courier New'; FFontSize: 9)
  );
  (** A constant array of strings that name of the File Operation fonts. **)
  strFileOperationFonts : Array[Low(TFileOpFont)..High(TFileOpFont)] Of String = (
    'Do Nothing', 'Copy Left to Right', 'Copy Right to Left', 'Delete', 'Size Different',
    'Exceeds Maximum Size', 'Read Only', 'Missing File');
  (** A constant array of default options for the file operation fonts. **)
  FileOperationFontDefaults : Array[Low(TFileOpFont)..High(TFileOpFont)] Of
    TFileOperationFontRecord = (
    (FININame: 'DoNothingFont';   FFontColour: clGray;       FBGFontColour: clSilver; FFontStyle: []),
    (FININame: 'LeftRightFont';   FFontColour: clWindowText; FBGFontColour: clWindow; FFontStyle: []),
    (FININame: 'RightLeftFont';   FFontColour: clWindowText; FBGFontColour: clWindow; FFontStyle: []),
    (FININame: 'DeleteFont';      FFontColour: clMaroon;     FBGFontColour: clWindow; FFontStyle: [fsStrikeOut]),
    (FININame: 'SizeDiffFont';    FFontColour: clWindowText; FBGFontColour: clWindow; FFontStyle: [fsItalic]),
    (FININame: 'ExceedMaxFont';   FFontColour: clGray;       FBGFontColour: clWindow; FFontStyle: []),
    (FININame: 'ReadOnlyFont';    FFontColour: clBlack;      FBGFontColour: $BBBBFF;  FFontStyle: []),
    (FININame: 'MissingFileFont'; FFontColour: clSilver;     FBGFontColour: clWindow; FFontStyle: [])
  );
  (** A constant array of strings to represent the File Operation Statistics. **)
  strFileOpStatDesc : Array[Low(TFileOpStat)..High(TFileOpStat)] Of String = (
    'Show number of files and size of Deletes',
    'Show number of files and size of Copies.',
    'Show number of files and size of file with Differing Size',
    'Show number of files and size of file with Nothing to Do',
    'Show number of files and size of file in Total on the Left',
    'Show number of files and size of file in Total on the Right',
    'Show number of files and size of file in Total Difference'
  );

Implementation

Uses
  CodeSiteLogging,
  FolderPathsForm,
  IniFiles,
  CheckForUpdatesOptionsForm,
  Themes,
  InterfaceFontForm,
  OperationsFontForm,
  Types;
  
{$R *.DFM}

{ TfrmOptions }

(**

  This is the classes main interface method for editing the applications options .

  @precon  None.
  @postcon Returns true with the updated options in the var variables else returns false 
           if the dialogue is cancelled.

  @param   Folders        as a TFolders
  @param   strExclusions  as a String as a reference
  @param   strCompareEXE  as a String as a reference
  @param   strINIFileName as a String
  @param   InterfaceFonts as a TInterfaceFontInfo as a reference
  @param   FileOpFonts    as a TFileOperationFontInfo as a reference
  @param   FldrSyncOps    as a TFldrSyncOptions as a reference
  @param   strTheme       as a String as a reference
  @param   FileOpStats    as a TFileOpStats as a reference
  @return  a Boolean

**)
Class Function TfrmOptions.Execute(Folders: TFolders;
  Var strExclusions, strCompareEXE: String; strINIFileName: String;
  var InterfaceFonts : TInterfaceFontInfo; var FileOpFonts : TFileOperationFontInfo;
  Var FldrSyncOps: TFldrSyncOptions; var strTheme : String;
  var FileOpStats : TFileOpStats): Boolean;

Var
  i     : TFldrSyncOption;
  j : TFileOpStat;
  Item: TListItem;

Begin
  Result := False;
  With TfrmOptions.CreateWithRootKey(Nil, strINIFileName) Do
    Try
      FInterfaceFonts := InterfaceFonts;
      FFileOpFonts := FileOpFonts;
      FFolders.Assign(Folders);
      PopulateFolderList;
      lvFoldersResize(Nil);
      lvFoldersSelectItem(Nil, Nil, False);
      edtExclusions.Text := strExclusions;
      edtCompareEXE.Text := strCompareEXE;
      For i              := Low(TFldrSyncOption) To High(TFldrSyncOption) Do
        Begin
          Item := lbxFldrSyncOps.Items.Add;
          Item.Caption := strFldrSyncOptions[i].FDescription;
          lbxFldrSyncOps.Items[Integer(i)].Checked := i In FldrSyncOps;
        End;
      For j              := Low(TFileOpStat) To High(TFileOpStat) Do
        Begin
          Item := lvFileOpStats.Items.Add;
          Item.Caption := strFileOpStatDesc[j];
          lvFileOpStats.Items[Integer(j)].Checked := j In FileOpStats;
        End;
      If ShowModal = mrOK Then
        Begin
          Folders.Assign(FFolders);
          strExclusions := edtExclusions.Text;
          strCompareEXE := edtCompareEXE.Text;
          FldrSyncOps   := [];
          For i         := Low(TFldrSyncOption) To High(TFldrSyncOption) Do
            If lbxFldrSyncOps.Items[Integer(i)].Checked Then
              Include(FldrSyncOps, i);
          FileOpStats   := [];
          For j         := Low(TFileOpStat) To High(TFileOpStat) Do
            If lvFileOpStats.Items[Integer(j)].Checked Then
              Include(FileOpStats, j);
          InterfaceFonts := FInterfaceFonts;
          FileOpFonts := FFileOpFonts;
          strTheme := cbxThemes.Text;
          Result := True;
        End;
    Finally
      Free;
    End;
End;

(**

  This is an on create event handler for the form.

  @precon  None.
  @postcon Initialises the Right and Left width properties for the form
  resizing. Also loads the forms size and position from the registry.

  @param   Sender as a TObject

**)
Procedure TfrmOptions.FormCreate(Sender: TObject);

Var
  i : Integer;
  j : TInterfaceFont;
  k : TFileOpFont;
  
Begin
  FRightWidth := 1;
  FLeftWidth  := 1;
  With TMemIniFile.Create(FINIFileName) Do
    Try
      Top    := ReadInteger('Options', 'Top', Top);
      Left   := ReadInteger('Options', 'Left', Left);
      Height := ReadInteger('Options', 'Height', Height);
      Width  := ReadInteger('Options', 'Width', Width);
    Finally
      Free;
    End;
  FFolders    := TFolders.Create;
  For i := Low(TStyleManager.StyleNames) To High(TStyleManager.StyleNames) Do
    cbxThemes.Items.Add(TStyleManager.StyleNames[i]);
  cbxThemes.ItemIndex := cbxThemes.Items.IndexOf(TStyleManager.ActiveStyle.Name);
  For j := Low(TInterfaceFont) To High(TinterfaceFont) Do
    lbxInterfaceFonts.Items.Add(strInterfaceFonts[j]);
  For k := Low(TFileOpFont) To High(TFileOpFont) Do
    lbxFileOperationFonts.Items.Add(strFileOperationFonts[k]);
  pagPages.ActivePageIndex := 0;
End;

(**

  This method is the forms on Destroy event handler.

  @precon  None.
  @postcon Saves the forms position and size to the registry.

  @param   Sender as a TObject

**)
Procedure TfrmOptions.FormDestroy(Sender: TObject);

Begin
  With TMemIniFile.Create(FINIFileName) Do
    Try
      WriteInteger('Options', 'Top', Top);
      WriteInteger('Options', 'Left', Left);
      WriteInteger('Options', 'Height', Height);
      WriteInteger('Options', 'Width', Width);
      UpdateFile;
    Finally
      Free;
    End;
  FFolders.Free;
End;

(**

  This is an on click event handler for the Add button.

  @precon  None.
  @postcon Adds the folder paths returned from the Folder Paths dialogue to the
  list view.

  @param   Sender as a TObject

**)
Procedure TfrmOptions.btnAddClick(Sender: TObject);

Var
  Folder, F        : TFolder;

Begin
  Folder := TFolder.Create('', '', '', [soEnabled], 0);
  Try
    If TfrmFolderPaths.Execute(Folder, FINIFileName) Then
      Begin
        F := TFolder.Create('', '', '', [], 0);
        F.Assign(Folder);
        FFolders.Add(F);
        PopulateFolderList;
      End;
  Finally
    Folder.Free;
  End;
End;

(**

  This is an on click event handler for the Browse button.

  @precon  None.
  @postcon Updates the edit control with the selected exe files.

  @param   Sender as a TObject

**)
Procedure TfrmOptions.btnBrowseClick(Sender: TObject);

Begin
  If dlgOpen.Execute Then
    edtCompareEXE.Text := dlgOpen.FileName;
End;

(**

  This is an on click event handler for the Check for Updates button.

  @precon  None.
  @postcon Displays a dialogue in which the Check for Updates functionality can be
           configured.

  @param   Sender as a TObject

**)
Procedure TfrmOptions.btnCheckforUpdatesClick(Sender: TObject);

Begin
  TfrmCheckForUpdatesOptions.Execute(FINIFileName);
End;

(**

  This is an on click event handler for the Copy button.

  @precon  None.
  @postcon This method copies the selected folder pairing and adds it as a new item to the
           end of the folder list.

  @param   Sender as a TObject

**)
Procedure TfrmOptions.btnCopyClick(Sender: TObject);

Var
  F : TFolder;
  iIndex: Integer;
  
Begin
  F := FFolders.Folder[lvFolders.ItemIndex];
  iIndex := FFolders.Add(TFolder.Create(F.LeftFldr, F.RightFldr, F.Patterns,
    F.SyncOptions, F.MaxFileSize));
  PopulateFolderList;
  lvFolders.ItemIndex := iIndex;
End;

(**

  This is an on click event handler for the Edit button.

  @precon  None.
  @postcon Allows the selected item to be edited in the Folder Paths dialogue.

  @param   Sender as a TObject

**)
Procedure TfrmOptions.btnEditClick(Sender: TObject);

Var
  Folder           : TFolder;

Begin
  Folder := TFolder.Create('', '', '', [], 0);
  Try
    Folder.Assign(FFolders.Folder[lvFolders.ItemIndex]);
    If TfrmFolderPaths.Execute(Folder, FINIFileName) Then
      Begin
        FFolders.Folder[lvFolders.ItemIndex].Assign(Folder);
        PopulateFolderList;
        lvFoldersResize(Sender);
      End;
  Finally
    Folder.Free;
  End;
End;

(**

  This is an on click event handler for the File operation Font button.

  @precon  None.
  @postcon Displays the File Operations Font editing dialogue.

  @param   Sender as a TObject

**)
Procedure TfrmOptions.btnFileOperationFontEditClick(Sender: TObject);

Var
  iFontColour, iBackColour : TColor;
  iStyles : TFontStyles;
  iIndex : TFileOpFont;
  
Begin
  If lbxFileOperationFonts.ItemIndex > -1 Then
    Begin
      iIndex := TFileOpFont(lbxFileOperationFonts.ItemIndex);
      iFontColour := FFileOpFonts[iIndex].FFontColour;
      iBackColour := FFileOpFonts[iIndex].FBGFontColour;
      iStyles := FFileOpFonts[iIndex].FFontStyle;
      If TfrmOperationsFonts.Execute(iFontColour, iBackColour, iStyles) Then
        Begin
          FFileOpFonts[iIndex].FFontColour := iFontColour;
          FFileOpFonts[iIndex].FBGFontColour := iBackColour;
          FFileOpFonts[iIndex].FFontStyle := iStyles;
          lbxFileOperationFonts.Invalidate;
        End;
    End;
End;

(**

  This is an on click event handler for the Help button.

  @precon  None.
  @postcon Displays the Options topic of the HTML Help.

  @param   Sender as a TObject

**)
Procedure TfrmOptions.btnHelpClick(Sender: TObject);

Begin
  Application.HelpContext(1);
End;

(**

  This is an on click event handler for the Interface Font Edit button.

  @precon  None.
  @postcon Displays the Interface Font dialogue for editing the fonts of the interface.

  @param   Sender as a TObject

**)
Procedure TfrmOptions.btnInterfaceFontEditClick(Sender: TObject);

Var
  strFontName : string;
  iFontSize   : Integer;
  
Begin
  If lbxInterfaceFonts.ItemIndex > -1 Then
    Begin
      strFontName := FInterfaceFonts[TInterfaceFont(lbxInterfaceFonts.ItemIndex)].FFontName;
      iFontSize := FInterfaceFonts[TInterfaceFont(lbxInterfaceFonts.ItemIndex)].FFontSize;
      If TfrmInterfaceFonts.Execute(strFontName, iFontSize) Then
        Begin
          FInterfaceFonts[TInterfaceFont(lbxInterfaceFonts.ItemIndex)].FFontName := strFontName;
          FInterfaceFonts[TInterfaceFont(lbxInterfaceFonts.ItemIndex)].FFontSize := iFontSize;
          lbxInterfaceFonts.Invalidate;
        End;
    End;
End;

(**

  This is an on click event handler for the Up button.

  @precon  None.
  @postcon Moves the selected folder up the list.

  @param   Sender as a TObject

**)
Procedure TfrmOptions.btnUpClick(Sender: TObject);

Begin
  lvFolders.OnChange := Nil;
  Try
    FFolders.Exchange(lvFolders.ItemIndex, lvFolders.ItemIndex - 1);
    lvFolders.ItemIndex := lvFolders.ItemIndex - 1;
    PopulateFolderList;
  Finally
    lvFolders.OnChange := lvFoldersChange;
  End;
End;

(**

  This is the constructor method for the TfrmOptions class.

  @precon  None.
  @postcon Sets the FRootKey variable for loading an saving settings to the
  registry.

  @param   AOwner     as a TComponent
  @param   strRootKey as a String

**)
Constructor TfrmOptions.CreateWithRootKey(AOwner: TComponent; strRootKey: String);

Begin
  Inherited Create(AOwner);
  FINIFileName := strRootKey;
End;

(**

  This is an on click event handler for the delete button.

  @precon  None.
  @postcon Deletes the selected item from the folders list view.

  @param   Sender as a TObject

**)
Procedure TfrmOptions.btnDeleteClick(Sender: TObject);

Begin
  FFolders.Delete(lvFolders.ItemIndex);
  PopulateFolderList;
End;

(**

  This is an on click event handler for the Down button.

  @precon  None.
  @postcon Moves the selected folder down the list.

  @param   Sender as a TObject

**)
Procedure TfrmOptions.btnDownClick(Sender: TObject);

Begin
  lvFolders.OnChange := Nil;
  Try
    FFolders.Exchange(lvFolders.ItemIndex, lvFolders.ItemIndex + 1);
    lvFolders.ItemIndex := lvFolders.ItemIndex + 1;
    PopulateFolderList;
  Finally
    lvFolders.OnChange := lvFoldersChange;
  End;
End;

(**

  This is an on draw items event handler for the File Operation listbox.

  @precon  None.
  @postcon Draws the file operations items in their specific font colours and styles.

  @param   Control as a TWinControl
  @param   Index   as an Integer
  @param   Rect    as a TRect
  @param   State   as a TOwnerDrawState

**)
Procedure TfrmOptions.lbxFileOperationFontsDrawItem(Control: TWinControl; Index: Integer;
  Rect: TRect; State: TOwnerDrawState);

Var
  lbx: TListBox;

Begin
  lbx                  := Control As TListBox;
  lbx.Canvas.Font.Name := FInterfaceFonts[ifTableFont].FFontName;
  lbx.Canvas.Font.Size := FInterfaceFonts[ifTableFont].FFontSize;
  If odSelected In State Then
    Begin
      lbx.Canvas.Font.Color  := StyleServices.GetSystemColor(clHighlightText);
      lbx.Canvas.Brush.Color := StyleServices.GetSystemColor(clHighlight);
    End
  Else
    Begin
      lbx.Canvas.Font.Color  :=
        StyleServices.GetSystemColor(FFileOpFonts[TFileOpFont(Index)].FFontColour);
      lbx.Canvas.Brush.Color :=
        StyleServices.GetSystemColor(FFileOpFonts[TFileOpFont(Index)].FBGFontColour);
    End;
  lbx.Canvas.Font.Style := FFileOpFonts[TFileOpFont(Index)].FFontStyle;
  lbx.Canvas.FillRect(Rect);
  lbx.Canvas.Refresh;
  DrawText(lbx.Canvas.Handle, PChar(lbx.Items[Index]), Length(lbx.Items[Index]), Rect,
    DT_CENTER Or DT_VCENTER Or DT_SINGLELINE);
End;

(**

  This is an on draw item event handler for the Interface Font list box.

  @precon  None.
  @postcon Draws the interface fonts in their font name and size.

  @param   Control as a TWinControl
  @param   Index   as an Integer
  @param   Rect    as a TRect
  @param   State   as a TOwnerDrawState

**)
Procedure TfrmOptions.lbxInterfaceFontsDrawItem(Control: TWinControl; Index: Integer;
  Rect: TRect; State: TOwnerDrawState);

Var
  lbx : TListbox;
  
Begin
  lbx := Control As TListbox;
  lbx.Canvas.Font.Name := FInterfaceFonts[TInterfaceFont(Index)].FFontName;
  lbx.Canvas.Font.Size := FInterfaceFonts[TInterfaceFont(Index)].FFontSize;
  If odSelected In State Then
    Begin
      lbx.Canvas.Font.Color := StyleServices.GetSystemColor(clHighlightText);
      lbx.Canvas.Brush.Color := StyleServices.GetSystemColor(clHighlight);
    End Else
    Begin
      lbx.Canvas.Font.Color := StyleServices.GetSystemColor(clWindowText);
      lbx.Canvas.Brush.Color := StyleServices.GetSystemColor(clWindow);
    End;
  lbx.Canvas.Refresh;
  lbx.Canvas.FillRect(Rect);
  DrawText(lbx.Canvas.Handle, PChar(lbx.Items[Index]), Length(lbx.Items[Index]), Rect,
    DT_CENTER Or DT_VCENTER Or DT_SINGLELINE);
End;

(**

  This is an on chnage event handler for the folders list view control.

  @precon  None.
  @postcon Updates the SyncOptions for the folder IF the check box for an item is changed.

  @param   Sender as a TObject
  @param   Item   as a TListItem
  @param   Change as a TItemChange

**)
Procedure TfrmOptions.lvFoldersChange(Sender: TObject; Item: TListItem;
  Change: TItemChange);

Begin
  If Item.Checked Then
    FFolders.Folder[Item.Index].SyncOptions :=
      FFolders.Folder[Item.Index].SyncOptions + [soEnabled]
  Else
    FFolders.Folder[Item.Index].SyncOptions :=
      FFolders.Folder[Item.Index].SyncOptions - [soEnabled];
End;

(**

  This method is an on custom draw item event handler for the folder list view.

  @precon  None.
  @postcon Renders the folders with a PATH ellipsis if the dialogue is too narrow.

  @param   Sender      as a TCustomListView
  @param   Item        as a TListItem
  @param   State       as a TCustomDrawState
  @param   DefaultDraw as a Boolean as a reference

**)
Procedure TfrmOptions.lvFoldersCustomDrawItem(Sender: TCustomListView; Item: TListItem;
  State: TCustomDrawState; Var DefaultDraw: Boolean);

Var
  i       : Integer;
  R, ItemR: TRect;
  Buffer  : Array [0 .. 2048] Of Char;
  Ops     : Integer;

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

Begin
  DefaultDraw := False;
  // Set Left Background
  Sender.Canvas.Font.Color := StyleServices.GetSystemColor(clWindowText);
  Sender.Canvas.Brush.Color := StyleServices.GetSystemColor(clWindow);
  If Item.Selected Then
    Begin
      Sender.Canvas.Brush.Color := StyleServices.GetSystemColor(clHighlight);
      Sender.Canvas.Font.Color  := StyleServices.GetSystemColor(clHighlightText);
    End;
  ItemR := Item.DisplayRect(drBounds);
  Sender.Canvas.FillRect(ItemR);
  // Draw Status Icon
  R := Item.DisplayRect(drBounds);
  Sender.Canvas.FillRect(R);
  ilStatus.Draw(Sender.Canvas, R.Left + 2, R.Top + 1, Integer(Item.Checked), True);
  // Draw Caption
  R := Item.DisplayRect(drLabel);
  Inc(R.Top, 2);    // Padding / Margin
  Inc(R.Bottom, 2); // Padding / Margin
  Inc(R.Left, 2);   // Padding / Margin
  Dec(R.Right, 2);  // Padding / Margin
  Ops := DT_LEFT Or DT_MODIFYSTRING Or DT_PATH_ELLIPSIS Or DT_NOPREFIX;
  StrPCopy(Buffer, Item.Caption);
  DrawText(Sender.Canvas.Handle, Buffer, Length(Item.Caption), R, Ops);
  // Draw Sub Items
  For i := 0 To 1 Do
    Begin
      Case i Of
        0:
          Ops := DT_LEFT Or DT_MODIFYSTRING Or DT_PATH_ELLIPSIS Or DT_NOPREFIX;
        1:
          Ops := DT_LEFT Or DT_MODIFYSTRING Or DT_END_ELLIPSIS Or DT_NOPREFIX;
      Else
        Ops := DT_LEFT;
      End;
      R := GetSubItemRect(i);
      StrPCopy(Buffer, Item.SubItems[i]);
      Sender.Canvas.Brush.Color := StyleServices.GetSystemColor(clWindow);
      If Item.Selected Then
        Sender.Canvas.Brush.Color := StyleServices.GetSystemColor(clHighlight);
      Sender.Canvas.Refresh;
      DrawText(Sender.Canvas.Handle, Buffer, Length(Item.SubItems[i]), R, Ops);
      R.Left := R.Right;
    End;
End;

(**

  This is an on double event handler for the list view.

  @precon  None.
  @postcon Edits the selected item in the list.

  @param   Sender as a TObject

**)
Procedure TfrmOptions.lvFoldersDblClick(Sender: TObject);

Begin
  btnEditClick(Self);
End;

(**

  This is an on Resize event handler for the list view.

  @precon  None.
  @postcon Resizes the list views columns to match the size of the window.

  @param   Sender as a TObject

**)
Procedure TfrmOptions.lvFoldersResize(Sender: TObject);

Var
  i: Integer;

Begin
  lvFolders.Column[2].Width := 150;
  i                         := lvFolders.ClientWidth - 22 - lvFolders.Column[2].Width;
  lvFolders.Column[0].Width := Trunc(i * Int(LeftWidth) / Int(LeftWidth + RightWidth));
  lvFolders.Column[1].Width := Trunc(i * Int(RightWidth) / Int(LeftWidth + RightWidth));
End;

(**

  This is an on select item event handler for the Folder List View.

  @precon  None.
  @postcon Updates the enabled property of the Edit and Delete buttons depending upon
           whether there is a selected item in the list.

  @param   Sender   as a TObject
  @param   Item     as a TListItem
  @param   Selected as a Boolean

**)
Procedure TfrmOptions.lvFoldersSelectItem(Sender: TObject; Item: TListItem;
  Selected: Boolean);

Begin
  btnEdit.Enabled   := Selected;
  btnDelete.Enabled := Selected;
  btnCopy.Enabled   := Selected;
  btnUp.Enabled     := (Item <> Nil) And (Item.Index > 0) And (lvFolders.Items.Count > 1);
  btnDown.Enabled   := (Item <> Nil) And (Item.Index < lvFolders.Items.Count - 1) And
    (lvFolders.Items.Count > 1);
End;

(**

  This method populates the list view with the folder information and options.

  @precon  None.
  @postcon The list view is populates with the folder information and options.

**)
Procedure TfrmOptions.PopulateFolderList;

Const
  SyncOps: Array [Succ(Low(TSyncOption))..High(TSyncOption)] Of String = (
    'Left',
    'Right',
    'Overwrite',
    'Copy Yes',
    'Delete Yes',
    'Copy No',
    'Delete No',
    'No Recursion',
    'Temp Disabled'
    );
  strMultiplers : Array[0..5] of String = ('B', 'KB', 'MB', 'GB', 'TB', 'PB');

Var
  i        : Integer;
  Item     : TListItem;
  j        : TSyncOption;
  strOps   : String;
  iSelected: Integer;
  iMax     : Int64;
  iIndex: Integer;

Begin
  Try
    Try
      lvFolders.OnChange := Nil;
      lvFolders.Items.EndUpdate;
      iSelected := lvFolders.ItemIndex;
      lvFolders.Clear;
      For i := 0 To FFolders.Count - 1 Do
        Begin
          Item         := lvFolders.Items.Add;
          Item.Caption := FFolders.Folder[i].LeftFldr + FFolders.Folder[i].Patterns;
          Item.SubItems.Add(FFolders.Folder[i].RightFldr + FFolders.Folder[i].Patterns);
          Item.Checked := soEnabled In FFolders.Folder[i].SyncOptions;
          strOps       := '';
          For j        := Succ(Low(TSyncOption)) To High(TSyncOption) Do
            Begin
              If j In FFolders.Folder[i].SyncOptions Then
                Begin
                  If strOps <> '' Then
                    strOps := strOps + ', ';
                  strOps   := strOps + SyncOps[j];
                End;
              If (j = soPrimaryRight) And (strOps = '') Then
                strOps := 'Synchronise';
            End;
          iMax := FFolders.Folder[i].MaxFileSize;
          If iMax > 0 Then
            Begin
              iIndex := 0;
              While iMax Mod 1024 = 0 Do
                Begin
                  iMax := iMax Div 1024;
                  iIndex := iIndex + 1;
                  If iIndex >= High(strMultiplers) Then
                    Break;
                End;
              If strOps <> '' Then
                strOps := strOps + ', ';
              strOps := strOps + 'Max ' + IntToStr(iMax) + strMultiplers[iIndex];            
            End;
          Item.SubItems.Add(strOps);
        End;
      If iSelected >= FFolders.Count Then
        Dec(iSelected);
      lvFolders.ItemIndex := iSelected;
    Finally
      lvFolders.OnChange := lvFoldersChange;
    End;
  Finally
    lvFolders.Items.EndUpdate;
  End;
End;

(**

  This is a setter method for the LeftWidth property.

  @precon  None.
  @postcon Sets the LeftWidth porperty.

  @param   Value as an Integer as a constant

**)
Procedure TfrmOptions.SetLeftWidth(Const Value: Integer);

Begin
  If Value > FLeftWidth Then
    FLeftWidth := Value;
End;

(**

  This is a setter method for the RightWidth property.

  @precon  None.
  @postcon Sets the RightWidth property.

  @param   Value as an Integer as a constant

**)
Procedure TfrmOptions.SetRightWidth(Const Value: Integer);

Begin
  If Value > FRightWidth Then
    FRightWidth := Value;
End;

(**

  Force the Font list boxes to redraw when the dialogue is resized.

  @precon  None.
  @postcon Re-draws the list boxes containing the interface fonts.

  @param   Sender as a TObject

**)
Procedure TfrmOptions.tabFontsResize(Sender: TObject);

Begin
  lbxInterfaceFonts.Invalidate;
  lbxFileOperationFonts.Invalidate;
End;

End.
