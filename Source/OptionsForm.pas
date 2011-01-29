(**

  This module defines the options dialogue.

  @Date    29 Jan 2011
  @Version 1.0
  @Author  David Hoyle

**)
Unit OptionsForm;

Interface

  Uses
    Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
    StdCtrls, Buttons, ComCtrls, CheckLst;

  Type
    (** An enumerate of Folder Sync Options **)
    TFldrSyncOption = (fsoCloseIFNoFilesAfterComparison, fsoNoConfirmation,
      fsoDoNotConfirmMkDir, fsoShowSimpleProgress,
      fsoStartProcessingAutomatically, fsoHideLongFileNames,
      fsoCheckForUpdatesOnStartup);

    (** A set of folder sync options. **)
    TFldrSyncOptions = Set Of TFldrSyncOption;

    (** This is an enumerate of synchronisation options. **)
    TSyncOption = (soEnabled, soPrimaryLeft, soPrimaryRight);

    (** A set of sync options. **)
    TSyncOptions = Set Of TSyncOption;

    (** This is a class to represent **)
    TfrmOptions = Class(TForm)
      btnOK: TBitBtn;
      btnCancel: TBitBtn;
      PageControl1: TPageControl;
      pgFolderList: TTabSheet;
      pgExclusions: TTabSheet;
      edtExclusions: TMemo;
      lvFolders: TListView;
      btnAdd: TBitBtn;
      btnEdit: TBitBtn;
      btnDelete: TBitBtn;
      tabCompareFiles: TTabSheet;
      edtCompareEXE: TEdit;
      lblCompareFiles: TLabel;
      btnBrowse: TButton;
      dlgOpen: TOpenDialog;
      lbxAdvancedOptions: TCheckListBox;
      lblAdvancedOptions: TLabel;
      lblFontName: TLabel;
      cbxFontName: TComboBox;
      lblFontSize: TLabel;
      cbxFontSize: TComboBox;
      cbxFontStyle: TComboBox;
      lblFontStyle: TLabel;
      Procedure lvFoldersResize(Sender: TObject);
      Procedure btnAddClick(Sender: TObject);
      Procedure btnEditClick(Sender: TObject);
      Procedure btnDeleteClick(Sender: TObject);
      Procedure lvFoldersDblClick(Sender: TObject);
      Procedure FormCreate(Sender: TObject);
      Procedure FormDestroy(Sender: TObject);
      Procedure btnBrowseClick(Sender: TObject);
    Private
      { Private declarations }
      FRightWidth: Integer;
      FLeftWidth: Integer;
      FRootKey: String;
      Procedure SetLeftWidth(Const Value: Integer);
      Procedure SetRightWidth(Const Value: Integer);
      Procedure AddFolders(strLeft, strRight: String;
        SyncOptions: TSyncOptions);
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
      Class Function Execute(Var slFolders: TStringList; Var strExclusions,
        strCompareEXE: String; strRootKey: String;
        Var FldrSyncOps: TFldrSyncOptions; AFont: TFont): Boolean;
      Constructor CreateWithRootKey(AOwner: TComponent; strRootKey: String);
        Virtual;
    End;

    (** This is a record to describe the INI file parameters for storage of the
        Application options. **)
    TOptionsInfo = Record
      FINISection  : String;
      FINIKey      : String;
      FDescription : String;
      FDefault     : Boolean;
    End;

  Const
    (** A constant array of strings corresponding to the folder sync options. **)
    strFldrSyncOptions: Array [ Low(TFldrSyncOption) .. High(TFldrSyncOption)]
      Of TOptionsInfo = (
      ( FINISection:  'AppOptions';
        FINIKey:      'CloseOnNoFiles';
        FDescription: 'Close Folder Sync IF there are no files to processes after comparison.';
        FDefault:     False),
      ( FINISection:  'AppOptions';
        FINIKey:      'RespondWithYes';
        FDescription: 'Respond with "Yes to All" for any dialogue boxes that are displayed.';
        FDefault:     False),
      ( FINISection:  'AppOptions';
        FINIKey:      'DoNotConfirmDir';
        FDescription: 'Do not confirm the creation of a new directories if the operation requires one to be created.';
        FDefault:     True),
      ( FINISection:  'AppOptions';
        FINIKey:      'DisplayProgress';
        FDescription: 'Display a progress dialogue box but do not show the file names.';
        FDefault:     False),
      ( FINISection:  'AppOptions';
        FINIKey:      'StartAutomatically';
        FDescription: 'Start processing the files after comparison automatically (DANGEROUS!).';
        FDefault:     False),
      ( FINISection:  'AppOptions';
        FINIKey:      'HideLongFiles';
        FDescription: 'Hide file that are too long to copy or delete.';
        FDefault:     False),
      ( FINISection:  'CheckForUpdates';
        FINIKey:      'Enabled';
        FDescription: 'Check for software Updates on Application Startup.';
        FDefault:     False)
    );

Implementation

  Uses
    FolderPathsForm, IniFiles;
{$R *.DFM}

    { TfrmOptions }

    (**

      This is the classes main interface method for editing the applications options
      .

      @precon  None.
      @postcon Returns true with the updated options in the var variables else
      returns false if the dialogue is cancelled.

      @param   slFolders     as a TStringList as a reference
      @param   strExclusions as a String as a reference
      @param   strCompareEXE as a String as a reference
      @param   strRootKey    as a String
      @param   FldrSyncOps   as a TFldrSyncOptions as a reference
      @param   AFont         as a TFont
      @return  a Boolean

    **)
  Class Function TfrmOptions.Execute(Var slFolders: TStringList;
    Var strExclusions, strCompareEXE: String; strRootKey: String;
    Var FldrSyncOps: TFldrSyncOptions; AFont: TFont): Boolean;

  Var
    i: Integer;
    iOption: TFldrSyncOption;
    iIndex: Integer;
    SyncOptions: TSyncOptions;

  Begin
    Result := False;
    With TfrmOptions.CreateWithRootKey(Nil, strRootKey) Do
      Try
        For i := 0 To slFolders.Count - 1 Do
          AddFolders(slFolders.Names[i], slFolders.ValueFromIndex[i],
            TSyncOptions(Byte(slFolders.Objects[i])));
        edtExclusions.Text := strExclusions;
        edtCompareEXE.Text := strCompareEXE;
        For iOption := Low(TFldrSyncOption) To High(TFldrSyncOption) Do
          Begin
            iIndex := lbxAdvancedOptions.Items.Add(strFldrSyncOptions[iOption].FDescription);
            lbxAdvancedOptions.Checked[iIndex] := iOption In FldrSyncOps;
          End;
        cbxFontName.ItemIndex := cbxFontName.Items.IndexOf(AFont.Name);
        cbxFontSize.ItemIndex := cbxFontSize.Items.IndexOf
          (IntToStr(AFont.Size));
        cbxFontStyle.ItemIndex := 0;
        If AFont.Style = [fsItalic] Then
          cbxFontStyle.ItemIndex := 1
        Else If AFont.Style = [fsBold] Then
          cbxFontStyle.ItemIndex := 2
        Else If AFont.Style = [fsItalic, fsBold] Then
          cbxFontStyle.ItemIndex := 3;
        If ShowModal = mrOK Then
          Begin
            slFolders.Clear;
            For i := 0 To lvFolders.Items.Count - 1 Do
              Begin
                SyncOptions := [];
                If lvFolders.Items[i].Checked Then
                  Include(SyncOptions, soEnabled);
                If lvFolders.Items[i].SubItems[1] = 'Primary Left' Then
                  Include(SyncOptions, soPrimaryLeft);
                If lvFolders.Items[i].SubItems[1] = 'Primary Right' Then
                  Include(SyncOptions, soPrimaryRight);
                slFolders.AddObject
                  (lvFolders.Items[i].Caption + '=' + lvFolders.Items
                    [i].SubItems[0], TObject(Byte(SyncOptions)));
              End;
            strExclusions := edtExclusions.Text;
            strCompareEXE := edtCompareEXE.Text;
            For i := 0 To lbxAdvancedOptions.Items.Count - 1 Do
              If lbxAdvancedOptions.Checked[i] Then
                Include(FldrSyncOps, TFldrSyncOption(i))
              Else
                Exclude(FldrSyncOps, TFldrSyncOption(i));
            AFont.Name := cbxFontName.Items[cbxFontName.ItemIndex];
            AFont.Size := StrToInt(cbxFontSize.Items[cbxFontSize.ItemIndex]);
            Case cbxFontStyle.ItemIndex Of
              1:
                AFont.Style := [fsItalic];
              2:
                AFont.Style := [fsBold];
              3:
                AFont.Style := [fsItalic, fsBold];
              Else
                AFont.Style := [];
              End;
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
    i: Integer;

  Begin
    FRightWidth := 1;
    FLeftWidth := 1;
    With TIniFile.Create(FRootKey) Do
      Try
        Top := ReadInteger('Options', 'Top', Top);
        Left := ReadInteger('Options', 'Left', Left);
        Height := ReadInteger('Options', 'Height', Height);
        Width := ReadInteger('Options', 'Width', Width);
      Finally
        Free;
      End;
    For i := 0 To Screen.Fonts.Count - 1 Do
      cbxFontName.Items.Add(Screen.Fonts[i]);
    For i := 6 To 32 Do
      cbxFontSize.Items.Add(IntToStr(i));
    cbxFontStyle.Items.Add('Normal');
    cbxFontStyle.Items.Add('Italic');
    cbxFontStyle.Items.Add('Bold');
    cbxFontStyle.Items.Add('Bold Italic');
  End;

  (**

    This method is the forms on Destroy event handler.

    @precon  None.
    @postcon Saves the forms position and size to the registry.

    @param   Sender as a TObject

  **)
  Procedure TfrmOptions.FormDestroy(Sender: TObject);
  Begin
    With TIniFile.Create(FRootKey) Do
      Try
        WriteInteger('Options', 'Top', Top);
        WriteInteger('Options', 'Left', Left);
        WriteInteger('Options', 'Height', Height);
        WriteInteger('Options', 'Width', Width);
      Finally
        Free;
      End;
  End;

  (**

    This method adds folder paths to the list view and updates the width
    properties for calculating the resize width of the list view.

    @precon  None.
    @postcon Adds folder paths to the list view and updates the width properties
    for calculating the resize width of the list view.

    @param   strLeft     as a String
    @param   strRight    as a String
    @param   SyncOptions as a TSyncOptions

  **)
  Procedure TfrmOptions.AddFolders(strLeft, strRight: String;
    SyncOptions: TSyncOptions);

  Var
    Item: TListItem;

  Begin
    Item := lvFolders.Items.Add;
    Item.Caption := strLeft;
    Item.Checked := soEnabled In SyncOptions;
    LeftWidth := Length(strLeft);
    Item.SubItems.Add(strRight);
    RightWidth := Length(strRight);
    Item.SubItems.Add('Synchronise');
    If soPrimaryLeft In SyncOptions Then
      Item.SubItems[1] := 'Primary Left';
    If soPrimaryRight In SyncOptions Then
      Item.SubItems[1] := 'Primary Right';
    lvFoldersResize(Self);
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
    SyncOptions: TSyncOptions;

  Begin
    Include(SyncOptions, soEnabled);
    If TfrmFolderPaths.Execute(strLeft, strRight, FRootKey, SyncOptions) Then
      AddFolders(strLeft, strRight, SyncOptions);
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

    This is an on click event handler for the Edit button.

    @precon  None.
    @postcon Allows the selected item to be edited in the Folder Paths dialogue.

    @param   Sender as a TObject

  **)
  Procedure TfrmOptions.btnEditClick(Sender: TObject);

  Var
    strLeft, strRight: String;
    SyncOptions: TSyncOptions;

  Begin
    If lvFolders.Selected <> Nil Then
      Begin
        strLeft := lvFolders.Selected.Caption;
        strRight := lvFolders.Selected.SubItems[0];
        SyncOptions := [];
        If lvFolders.Selected.SubItems[1] = 'Primary Left' Then
          Include(SyncOptions, soPrimaryLeft);
        If lvFolders.Selected.SubItems[1] = 'Primary Right' Then
          Include(SyncOptions, soPrimaryRight);
        If TfrmFolderPaths.Execute(strLeft, strRight, FRootKey, SyncOptions)
          Then
          Begin
            lvFolders.Selected.Caption := strLeft;
            lvFolders.Selected.SubItems[0] := strRight;
            lvFolders.Selected.SubItems[1] := 'Synchronise';
            If soPrimaryLeft In SyncOptions Then
              lvFolders.Selected.SubItems[1] := 'Primary Left';
            If soPrimaryRight In SyncOptions Then
              lvFolders.Selected.SubItems[1] := 'Primary Right';
            lvFoldersResize(Sender);
          End;
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
  Constructor TfrmOptions.CreateWithRootKey(AOwner: TComponent;
    strRootKey: String);
  Begin
    Inherited Create(AOwner);
    FRootKey := strRootKey;
  End;

  (**

    This is an on click event handler for the delete button.

    @precon  None.
    @postcon Deletes the selected item from the folders list view.

    @param   Sender as a TObject

  **)
  Procedure TfrmOptions.btnDeleteClick(Sender: TObject);

  Begin
    If lvFolders.Selected <> Nil Then
      lvFolders.Selected.Delete;
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
    lvFolders.Column[2].Width := 100;
    i := lvFolders.ClientWidth - 22 - lvFolders.Column[2].Width;
    lvFolders.Column[0].Width := Trunc
      (i * Int(LeftWidth) / Int(LeftWidth + RightWidth));
    lvFolders.Column[1].Width := Trunc
      (i * Int(RightWidth) / Int(LeftWidth + RightWidth));
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
