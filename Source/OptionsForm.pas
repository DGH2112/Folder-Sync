(**

  This module defines the options dialogue.

  @Date    03 Aug 2012
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
  ImgList;

Type
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
    btnTableFont: TBitBtn;
    dlgFont: TFontDialog;
    lbxFldrSyncOps: TCheckListBox;
    lblExclusions: TLabel;
    ilStatus: TImageList;
    btnHelp: TBitBtn;
    btnLogFont: TBitBtn;
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
    Procedure btnTableFontClick(Sender: TObject);
    Procedure lvFoldersCustomDrawItem(Sender: TCustomListView; Item: TListItem;
      State: TCustomDrawState; Var DefaultDraw: Boolean);
    Procedure btnHelpClick(Sender: TObject);
    Procedure btnLogFontClick(Sender: TObject);
  Private
    { Private declarations }
    FRightWidth : Integer;
    FLeftWidth  : Integer;
    FINIFileName: String;
    FFolderList : TStringList;
    FTableFont  : TFont;
    FLogFont    : TFont;
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
    Class Function Execute(Var slFolders: TStringList;
      Var strExclusions, strCompareEXE: String; strINIFileName: String;
      TableFont, LogFont: TFont; Var FldrSyncOps: TFldrSyncOptions): Boolean;
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
  strOLDFldrSyncOptions: Array [Low(TOLDFldrSyncOption) .. High(TOLDFldrSyncOption)
    ] Of TOptionsInfo = ((FINISection: 'AppOptions'; FINIKey: 'CloseOnNoFiles';
    FDescription
    : 'Close Folder Sync IF there are no files to processes after comparison.';
    FDefault: False), (FINISection: 'AppOptions'; FINIKey: 'RespondWithYes';
    FDescription: 'Respond with "Yes to All" for any dialogue boxes that are displayed.';
    FDefault: False), (FINISection: 'AppOptions'; FINIKey: 'DoNotConfirmDir';
    FDescription
    : 'Do not confirm the creation of a new directories if the operation requires one to be created.';
    FDefault: True), (FINISection: 'AppOptions'; FINIKey: 'DisplayProgress';
    FDescription: 'Display a progress dialogue box but do not show the file names.';
    FDefault: False), (FINISection: 'AppOptions'; FINIKey: 'StartAutomatically';
    FDescription
    : 'Start processing the files after comparison automatically (DANGEROUS!).';
    FDefault: False), (FINISection: 'AppOptions'; FINIKey: 'HideLongFiles';
    FDescription: 'Hide file that are too long to copy or delete.'; FDefault: False));
  (** A constant array of strings corresponding to the NEW folder sync options. **)
  strFldrSyncOptions: Array [Low(TFldrSyncOption) .. High(TFldrSyncOption)
    ] Of TOptionsInfo = ((FINISection: 'AppOptions'; FINIKey: 'CloseOnNoFiles';
    FDescription
    : 'Close Folder Sync IF there are no files to processes after comparison.';
    FDefault: False), (FINISection: 'AppOptions'; FINIKey: 'StartAutomatically';
    FDescription
    : 'Start processing the files after comparison automatically (DANGEROUS!).';
    FDefault: False));

Implementation

Uses
  FolderPathsForm,
  IniFiles,
  CheckForUpdatesOptionsForm;
{$R *.DFM}
{ TfrmOptions }

(**

  This is the classes main interface method for editing the applications options .

  @precon  None.
  @postcon Returns true with the updated options in the var variables else returns false
           if the dialogue is cancelled.

  @param   slFolders      as a TStringList as a reference
  @param   strExclusions  as a String as a reference
  @param   strCompareEXE  as a String as a reference
  @param   strINIFileName as a String
  @param   TableFont      as a TFont
  @param   LogFont        as a TFont
  @param   FldrSyncOps    as a TFldrSyncOptions as a reference
  @return  a Boolean

**)
Class Function TfrmOptions.Execute(Var slFolders: TStringList;
  Var strExclusions, strCompareEXE: String; strINIFileName: String;
  TableFont, LogFont: TFont; Var FldrSyncOps: TFldrSyncOptions): Boolean;

Var
  i     : TFldrSyncOption;
  iIndex: Integer;

Begin
  Result := False;
  With TfrmOptions.CreateWithRootKey(Nil, strINIFileName) Do
    Try
      FTableFont.Assign(TableFont);
      FLogFont.Assign(LogFont);
      FFolderList.Assign(slFolders);
      PopulateFolderList;
      lvFoldersSelectItem(Nil, Nil, False);
      edtExclusions.Text := strExclusions;
      edtCompareEXE.Text := strCompareEXE;
      For i              := Low(TFldrSyncOption) To High(TFldrSyncOption) Do
        Begin
          iIndex := lbxFldrSyncOps.Items.Add(strFldrSyncOptions[i].FDescription);
          lbxFldrSyncOps.Checked[iIndex] := i In FldrSyncOps;
        End;
      If ShowModal = mrOK Then
        Begin
          slFolders.Assign(FFolderList);
          strExclusions := edtExclusions.Text;
          strCompareEXE := edtCompareEXE.Text;
          FldrSyncOps   := [];
          For i         := Low(TFldrSyncOption) To High(TFldrSyncOption) Do
            If lbxFldrSyncOps.Checked[iIndex] Then
              Include(FldrSyncOps, i);
          TableFont.Assign(FTableFont);
          LogFont.Assign(FLogFont);
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
  FTableFont  := TFont.Create;
  FLogFont    := TFont.Create;
  FFolderList := TStringList.Create;
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
  FFolderList.Free;
  FTableFont.Free;
  FLogFont.Free;
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
  strLeft, strRight: String;
  FOA              : TFolderOptionsAdapter;

Begin
  FOA.FRAWData := 0;
  Include(FOA.FSyncOptions, soEnabled);
  If TfrmFolderPaths.Execute(strLeft, strRight, FINIFileName, FOA.FSyncOptions) Then
    Begin
      FFolderList.AddObject(Format('%s=%s', [strLeft, strRight]), FOA.FOBjData);
      PopulateFolderList;
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

  This is an on click event handler for the Edit button.

  @precon  None.
  @postcon Allows the selected item to be edited in the Folder Paths dialogue.

  @param   Sender as a TObject

**)
Procedure TfrmOptions.btnEditClick(Sender: TObject);

Var
  strLeft, strRight: String;
  FOA              : TFolderOptionsAdapter;
  iIndex           : Integer;

Begin
  iIndex       := lvFolders.ItemIndex;
  strLeft      := lvFolders.Selected.Caption;
  strRight     := lvFolders.Selected.SubItems[0];
  FOA.FOBjData := FFolderList.Objects[iIndex];
  If TfrmFolderPaths.Execute(strLeft, strRight, FINIFileName, FOA.FSyncOptions) Then
    Begin
      FFolderList[iIndex]         := Format('%s=%s', [strLeft, strRight]);
      FFolderList.Objects[iIndex] := FOA.FOBjData;
      PopulateFolderList;
      lvFoldersResize(Sender);
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

  This is an on click event handler for the Table Font button.

  @precon  None.
  @postcon Displays a dialogue from which the user can change the font anme, size and
           style to be used in the table.

  @param   Sender as a TObject

**)
Procedure TfrmOptions.btnLogFontClick(Sender: TObject);

Begin
  dlgFont.Font.Assign(FLogFont);
  dlgFont.Options := [fdForceFontExist, fdNoStyleSel];
  If dlgFont.Execute(Application.Handle) Then
    FLogFont.Assign(dlgFont.Font);
End;

(**

  This is an on click event handler for the Table Font button.

  @precon  None.
  @postcon Displays a dialogue from which the user can change the font anme, size and
           style to be used in the table.

  @param   Sender as a TObject

**)
Procedure TfrmOptions.btnTableFontClick(Sender: TObject);

Begin
  dlgFont.Font.Assign(FTableFont);
  dlgFont.Options := [fdForceFontExist];
  If dlgFont.Execute(Application.Handle) Then
    FTableFont.Assign(dlgFont.Font);
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
  FFolderList.Delete(lvFolders.ItemIndex);
  PopulateFolderList;
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

Var
  FOA: TFolderOptionsAdapter;

Begin
  FOA.FOBjData := FFolderList.Objects[Item.Index];
  If Item.Checked Then
    Include(FOA.FSyncOptions, soEnabled)
  Else
    Exclude(FOA.FSyncOptions, soEnabled);
  FFolderList.Objects[Item.Index] := FOA.FOBjData;
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
  Sender.Canvas.Brush.Color := clWindow;
  If Item.Selected Then
    Begin
      Sender.Canvas.Brush.Color := clHighlight;
      Sender.Canvas.Font.Color  := clHighlightText;
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
      Sender.Canvas.Brush.Color := clWindow;
      If Item.Selected Then
        Sender.Canvas.Brush.Color := clHighlight;
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
End;

(**

  This method populates the list view with the folder information and options.

  @precon  None.
  @postcon The list view is populates with the folder information and options.

**)
Procedure TfrmOptions.PopulateFolderList;

Const
  SyncOps: Array [Succ(Low(TSyncOption)) .. High(TSyncOption)
    ] Of String = ('Left', 'Right', 'Overwrite', 'Yes', 'No');

Var
  i        : Integer;
  Item     : TListItem;
  FOA      : TFolderOptionsAdapter;
  j        : TSyncOption;
  strOps   : String;
  iSelected: Integer;

Begin
  Try
    Try
      lvFolders.OnChange := Nil;
      lvFolders.Items.EndUpdate;
      iSelected := lvFolders.ItemIndex;
      lvFolders.Clear;
      For i := 0 To FFolderList.Count - 1 Do
        Begin
          Item         := lvFolders.Items.Add;
          Item.Caption := FFolderList.Names[i];
          Item.SubItems.Add(FFolderList.ValueFromIndex[i]);
          FOA.FOBjData := FFolderList.Objects[i];
          Item.Checked := soEnabled In FOA.FSyncOptions;
          strOps       := '';
          For j        := Succ(Low(TSyncOption)) To High(TSyncOption) Do
            Begin
              If j In FOA.FSyncOptions Then
                Begin
                  If strOps <> '' Then
                    strOps := strOps + ', ';
                  strOps   := strOps + SyncOps[j];
                End;
              If (j = soPrimaryRight) And (strOps = '') Then
                strOps := 'Synchronise';
            End;
          Item.SubItems.Add(strOps);
        End;
      If iSelected >= FFolderList.Count Then
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

End.
