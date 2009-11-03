(**

  This module defines classes for handling and comparing two directories of
  files.

  @Version 1.0
  @Date    03 Nov 2009
  @Author  David Hoyle

**)
Unit FileComparision;

Interface

Uses
  SysUtils, Classes, Contnrs, Windows, ProgressForm, OptionsForm;

Type
  (** A type to define the status of a file **)
  TStatus = (stNewer, stOlder, stSame, stDiffSize, stTooLong);

  (** A type to define whether the CheckDifference method should check for
      Older or Newer differences. **)
  TCheckDifference = (cdNewer, cdOlder);

  (** A record to describe a single file. **)
  TFileRecord = Class
  Strict Private
    FFileName : String;
    FSize : Integer;
    FAttributes : Integer;
    FDateTime : Integer;
    FStatus : TStatus;
  Strict Protected
    Procedure SetStatus(Status : TStatus);
  Public
    Constructor Create(strName : String; iSize, iAttributes : Integer;
      dtDateTime : Integer; Status : TStatus); Virtual;
    (**
      A property to return the filename of the file in this class.
      @precon  None.
      @postcon Returns the filename of the file in the file record.
      @return  a String
    **)
    Property FileName : String Read FFileName;
    (**
      A property to return the size of the file in this class.
      @precon  None.
      @postcon Returns the size of the file in the file record.
      @return  an Integer
    **)
    Property Size : Integer Read FSize;
    (**
      A property to return the attributes of the file in this class.
      @precon  None.
      @postcon Returns the attrbutes of the file in the file record.
      @return  an Integer
    **)
    Property Attributes : Integer Read FAttributes;
    (**
      A property to return the date and time of the file in this class.
      @precon  None.
      @postcon Returns the date and time as an age integer for the file record.
      @return  an Integer
    **)
    Property DateTime : Integer Read FDateTime;
    (**
      A property to return the status of the file in this class.
      @precon  None.
      @postcon Returns the status of the file record.
      @return  a TStatus
    **)
    Property Status : TStatus Read FStatus Write SetStatus;
  End;

  (** This class defines a list of files from a single directory. **)
  TFileList = Class
  Strict Private
    FFolderPath : String;
    FFiles : TObjectList;
    FProgressProc: TProgressMsgProc;
    FExclusions : TStringList;
    FTotalSize : Int64;
    FFileFilters : TStringList;
    function InExclusions(strFileName: String): Boolean;
    function GetCount: Integer;
    function GetFiles(iIndex: Integer): TFileRecord;
  Strict Protected
    Procedure RecurseFolder(strFolderPath : String); Virtual;
    Procedure DoProgress(strMessage : String; iCount : Integer; strFile : String);
    (**
      Provides a indexable type access to the FileRecords.
      @precon  iIndex must be a valid index in the list.
      @postcon Returns the indexed TFileRecord.
      @param   iIndex as       an Integer
      @return  a TFileRecord
    **)
  Public
    Constructor Create(strFolderPath, strFileFilter : String;
      ProgressProc : TProgressMsgProc; strExclusions : String);
      Virtual;
    Destructor Destroy; Override;
    Function Find(strFileName : String) : Integer; Virtual;
    (**
      Aa property to return the folder path for the class.
      @precon  None.
      @postcon Returns the folder path for the class.
      @return  a String
    **)
    Property FolderPath : String Read FFolderPath;
    (**
      A property to return the number of files in the list.
      @precon  None.
      @postcon Returns the current number of items in the classes collection.
      @return  an Integer
    **)
    Property Count : Integer Read GetCount;
    (**
      A property to return the relative path of the indexed filename.
      @precon  iIndex must be from 0 to FCount - 1
      @postcon Returns the relative path of the indexed filename.
      @param   iIndex as       an Integer
      @return  a TFileRecord
    **)
    Property Files[iIndex : Integer] : TFileRecord Read GetFiles; Default;
    (**
      A property to get the total size of the file list.
      @precon  None.
      @postcon Returns an integer representing the total size.
      @return  an Int64
    **)
    Property TotalSize : Int64 Read FTotalSize;
  End;

  (** A class to compare to list of folder file. **)
  TCompareFolders = Class
  Strict Private
    FLeftFldr : TFileList;
    FRightFldr : TFileList;
    FProgressProc : TProgressMsgProc;
    FSyncOptions : TSyncOptions;
  Strict Protected
    Procedure CompareFolders;
  Public
    Constructor Create(strLeftFldr, strLeftFilter, strRightFldr,
      strRightFilter : String; ProgressProc : TProgressMsgProc;
      ProgressPosProc : TProgressPosProc; strExclusions : String;
      iSection : Integer; SyncOps : TSyncOptions); Virtual;
    function CheckDifference(iTimeDifference, iSizeDifference : Integer;
      Check : TCheckDifference): Boolean;
    Destructor Destroy; Override;
    (**
      A property to reference the Left Folder file list.
      @precon  None.
      @postcon Returns a reference to the left file folder list.
      @return  a TFileList
    **)
    Property LeftFldr : TFileList Read FLeftFldr;
    (**
      A property to reference the Right Folder file list.
      @precon  None.
      @postcon Returns a reference to the right file folder list.

      @return  a TFileList
    **)
    Property RightFldr : TFileList Read FRightFldr;
    (**
      This property returns the sync options that should be applied to the
      lists of files.
      @precon  None.
      @postcon Returns the sync options that should be applied to the
               lists of files.
      @return  a TSyncOptions
    **)
    Property SyncOptions : TSyncOptions Read FSyncOptions;
  End;

  (** A class to represent a collection of TCompareFolders classes. **)
  TCompareFoldersCollection = Class
  Strict Private
    FCompareFolders : TObjectList;
    Function GetCount : Integer;
    function GetCompareFolders(iIndex: Integer): TCompareFolders;
  Strict Protected
  Public
    Constructor Create(slFolders : TStringList; ProgressProc : TProgressMsgProc;
      ProgressPosProc : TProgressPosProc; strExclusions : String); Virtual;
    Destructor Destroy; Override;
    (**
      This property returns an indexed CompareFolders class.
      @precon  The index but be between 0 and Count - 1.
      @postcon Returns the indexed CompareFolders class.
      @param   iIndex as       an Integer
      @return  a TCompareFolders
    **)
    Property CompareFolders[iIndex : Integer] : TCompareFolders
      Read GetCompareFolders;
    (**
      This property represents the number of CompareFolder classes in the
      collection.
      @precon  None.
      @postcon Returns the number of CompareFolder classes in the collection.
      @return  an Integer
    **)
    Property Count : Integer Read GetCount;
  End;

Implementation

Uses
  FileCtrl, DGHLibrary;

{ TFileRecord }

(**

  This is the constructor method for the TFileRecord class.

  @precon  None.
  @postcon Initialises the file record with a filename, size, date and time and
           a status.

  @param   strName     as a String
  @param   iSize       as an Integer
  @param   iAttributes as an Integer
  @param   dtDateTime  as an Integer
  @param   Status      as a TStatus

**)
constructor TFileRecord.Create(strName : String; iSize, iAttributes : Integer;
  dtDateTime : Integer; Status : TStatus);
begin
  FFileName := strName;
  FSize := iSize;
  FAttributes := iAttributes;
  FDateTime := dtDateTime;
  FStatus := Status;
end;

(**

  This is a seter method for the Status property.

  @precon  None.
  @postcon Sets the status of the file record.

  @param   Status     as a TStatus

**)
procedure TFileRecord.SetStatus(Status: TStatus);
begin
  If FStatus <> Status Then
    FStatus := Status;
end;

{ TFileList }

(**

  This is the constructor method for the TFileList class.

  @precon  None.
  @postcon Creates the class instance and attempts to recurse the passed folder
           for files.

  @param   strFolderPath as a String
  @param   strFileFilter as a String
  @param   ProgressProc  as a TProgressMsgProc
  @param   strExclusions as a String

**)                                          
constructor TFileList.Create(strFolderPath, strFileFilter: String;
  ProgressProc : TProgressMsgProc; strExclusions : String);

Var
  iFilter : Integer;

begin
  inherited Create;
  FTotalSize := 0;
  FFiles := TObjectList.Create(True);
  FFileFilters := TStringList.Create;
  FFolderPath := strFolderPath;
  If strFileFilter = '' Then
    FFileFilters.Add('*.*')
  Else
    Begin
      For iFilter := 1 To CharCount(';', strFileFilter) + 1 Do
        FFileFilters.Add(GetField(strFileFilter, ';', iFilter));
    End;
  FProgressProc := ProgressProc;
  FExclusions := TStringList.Create;
  FExclusions.Text := LowerCase(strExclusions);
  DoProgress(Format('Buidling list for "%s"', [strFolderPath + strFileFilter]),
    0, 'Please wait...');
  If Length(FolderPath) = 0 Then
    Exit;
  RecurseFolder(FFolderPath);
end;

(**

  This is the destructor method for the TFileList class.

  @precon  None.
  @postcon Deallocated the  array file records and destroys the class instance.


**)
destructor TFileList.Destroy;
begin
  FFileFilters.Free;
  FFiles.Free;
  FExclusions.Free;
  inherited;
end;

(**

  This method updates the progress event method hooked with the current progress
  .

  @precon  None.
  @postcon Fires the progress event if the event handler is hooked.

  @param   strMessage as a String
  @param   iCount     as an Integer
  @param   strFile    as a String

**)
procedure TFileList.DoProgress(strMessage : String; iCount  : Integer; strFile: String);
begin
  If Assigned(FProgressProc) Then
    FProgressProc(strMessage, iCount, strFile);
end;

(**

  This method uses a binary search to find the given filename in the collection
  and return the index of the filename in the collection.

  @precon  None.
  @postcon Returns the index of the filename if the file is found else returns
           -1.

  @param   strFileName as a String
  @return  an Integer

**)
function TFileList.Find(strFileName: String): Integer;

Var
  iFirst, iLast, iMid : Integer;

begin
  Result := -1;
  iFirst := 0;
  iLast := Count - 1;
  While iLast >= iFirst Do
    Begin
      iMid := (iFirst + iLast) Div 2;
      If AnsiCompareFileName(strFileName, Files[iMid].FileName) = 0 Then
        Begin
          Result := iMid;
          Exit;
        End;
      If AnsiCompareFileName(strFileName, Files[iMid].FileName) < 0 Then
        iLast := iMid - 1
      Else
        iFirst := iMid + 1;
    End;
end;

(**

  This is a getter method for the Count property.

  @precon  None.
  @postcon Returns the number of files in the list.

  @return  an Integer

**)
function TFileList.GetCount: Integer;
begin
  Result := FFiles.Count;
end;

(**

  This is a getter method for the FileRecord property.

  @precon  iIndex must be a valid index between 0 and Count -1.
  @postcon Returns the indexed TFileRecord.

  @param   iIndex as an Integer
  @return  a TFileRecord

**)
function TFileList.GetFiles(iIndex: Integer): TFileRecord;
begin
  Result := FFiles.Items[iIndex] As TFileRecord;
end;

(**

  This method recurses the passed folder for file name and adds them to the file
  collection.

  @precon  None.
  @postcon Recurses the passed folder for file name and adds them to the file
           collection.

  @param   strFolderPath as a String

**)
procedure TFileList.RecurseFolder(strFolderPath: String);

Var
  rec : TSearchRec;
  iRes : Integer;
  strFileName, strFCName : String;
  iFirst, iLast, iMid : Integer;
  iFilter: Integer;

begin
  If Not InExclusions(strFolderPath) Then
    Begin
      // Search for files
      For iFilter := 0 To FFileFilters.Count - 1 Do
        Begin
          iRes := FindFirst(strFolderPath + FFileFilters[iFilter], faAnyFile, rec);
            Try
              While iRes = 0 Do
                Begin
                  If rec.Attr And faDirectory = 0 Then
                    Begin
                      strFileName := strFolderPath + rec.Name;
                      If Not InExclusions(strFileName) Then
                        Begin
                          strFCName := Copy(strFileName, Length(FFolderPath) + 1,
                            Length(strFileName));
                          iFirst := 0;
                          iLast := Count - 1;
                          While iLast >= iFirst Do
                            Begin
                              iMid := (iFirst + iLast) Div 2;
                              If AnsiCompareFileName(strFCName, Files[iMid].FileName) = 0 Then
                                Break;
                              If AnsiCompareFileName(strFCName, Files[iMid].FileName) < 0 Then
                                iLast := iMid - 1
                              Else
                                iFirst := iMid + 1;
                            End;
                          If Length(strFileName) < MAX_PATH Then
                            FFiles.Insert(iFirst,
                              TFileRecord.Create(strFCName, rec.Size, rec.Attr,
                              rec.Time, stNewer))
                          Else
                            FFiles.Insert(iFirst,
                              TFileRecord.Create(strFCName, rec.Size, rec.Attr,
                              rec.Time, stTooLong));
                          Inc(FTotalSize, rec.Size);
                          DoProgress(Format('Searching for files in "%s"', [FFolderPath]),
                            Count, FFolderPath + Files[iFirst].FileName);
                        End;
                    End;
                  iRes := FindNext(rec);
                End;
            Finally
              SysUtils.FindClose(rec);
            End;
          End;
      // Search directories
      iRes := FindFirst(strFolderPath + '*.*', faAnyFile, rec);
      Try
        While iRes = 0 Do
          Begin
            If rec.Attr And faDirectory <> 0 Then
              If (rec.Name <> '.') And (rec.Name <> '..') Then
                RecurseFolder(strFolderPath + rec.Name + '\');
            iRes := FindNext(rec);
          End;
      Finally
        SysUtils.FindClose(rec);
      End;
    End;
end;

(**

  This method check that a filename does not contain any of the list of
  exclusion strings.

  @precon  None.
  @postcon Returns true if the filename contains one of the exclusion strings
           else returns false.

  @param   strFileName as a String
  @return  a Boolean

**)
Function TFileList.InExclusions(strFileName : String) : Boolean;

Var
  i : Integer;

Begin
  strFileName := LowerCase(strFileName);
  Result := False;
  For i := 0 To FExclusions.Count - 1 Do
    Result := Result Or (Pos(FExclusions[i], strFileName) > 0);
End;

{ TCompareFolders }

(**

  This function checks the different between the file dates accounting for day
  light saving (i.e. exactly 1 hour on files of the same size).

  @precon  None.
  @postcon Checks the different between the file dates accounting for day
           light saving (i.e. exactly 1 hour on files of the same size).
           Returns true if different.

  @param   iTimeDifference as an Integer
  @param   iSizeDifference as an Integer
  @param   Check           as a TCheckDifference
  @return  a Boolean

**)
Function TCompareFolders.CheckDifference(iTimeDifference, iSizeDifference : Integer;
  Check : TCheckDifference) : Boolean;

Const
  Direction : Array[cdNewer..cdOlder] Of Integer = (1, -1);

Begin
  Result := (Direction[Check] * iTimeDifference > 0);
  Result := Result And Not ((iTimeDifference = Direction[Check] * 2048){ And (iSizeDifference = 0)});
  Result := Result And Not ((iTimeDifference = Direction[Check] * 18432){ And (iSizeDifference = 0)});
End;

(**

  This method compares the two folders of information and marks the
  corresponding files as either Old, New or the Same.

  @precon  None.
  @postcon The two lists of files are correctly martked up based on matching
           filenames and comparing date and time stamps.

**)
procedure TCompareFolders.CompareFolders;

Var
  iLeft, iRight : Integer;
  iTimeDifference : Integer;
  iSizeDifference : Integer;

begin
  For iLeft := 0 To LeftFldr.Count - 1 Do
    Begin
      If Assigned(FProgressProc) Then
        FProgressProc(Format('Comparing Folders "%s" to "%s"', [LeftFldr.FolderPath,
          RightFldr.FolderPath]), iLeft, LeftFldr[iLeft].FileName);
      iRight := RightFldr.Find(LeftFldr[iLeft].FileName);
      If iRight > -1 Then
        Begin
          If (LeftFldr[iLeft].Status <> stTooLong) And (RightFldr[iRight].Status <> stTooLong) Then
            Begin
              iTimeDifference := LeftFldr[iLeft].DateTime - RightFldr[iRight].DateTime;
              iSizeDifference := LeftFldr[iLeft].Size - RightFldr[iRight].Size;
              If CheckDifference(iTimeDifference, iSizeDifference, cdNewer) Then
                Begin
                  LeftFldr[iLeft].Status := stNewer;
                  RightFldr[iRight].Status := stOlder;
                End Else
              If CheckDifference(iTimeDifference, iSizeDifference, cdOlder) Then
                Begin
                  LeftFldr[iLeft].Status := stOlder;
                  RightFldr[iRight].Status := stNewer;
                End Else
              If iSizeDifference = 0 Then
                Begin
                  LeftFldr[iLeft].Status := stSame;
                  RightFldr[iRight].Status := stSame;
                End Else
                Begin
                  LeftFldr[iLeft].Status := stDiffSize;
                  RightFldr[iRight].Status := stDiffSize;
                End;
            End;
        End;
    End;
end;

(**

  This is the constructor method for the TCompareFolders class.

  @precon  None.
  @postcon Creates an instance of TFileList for ther left and right folder.

  @param   strLeftFldr     as a String
  @param   strLeftFilter   as a String
  @param   strRightFldr    as a String
  @param   strRightFilter  as a String
  @param   ProgressProc    as a TProgressMsgProc
  @param   ProgressPosProc as a TProgressPosProc
  @param   strExclusions   as a String
  @param   iSection        as an Integer
  @param   SyncOps         as a TSyncOptions

**)
constructor TCompareFolders.Create(strLeftFldr, strLeftFilter, strRightFldr,
  strRightFilter: String; ProgressProc: TProgressMsgProc;
  ProgressPosProc : TProgressPosProc; strExclusions : String; iSection : Integer;
  SyncOps : TSyncOptions);

begin
  FSyncOptions := SyncOps;
  If Not DirectoryExists(strLeftFldr) Then Exit;
  If Not DirectoryExists(strRightFldr) Then Exit;
  If Assigned(ProgressPosProc) Then
    ProgressPosProc(iSection, 0);
  FLeftFldr := TFileList.Create(strLeftFldr, strLeftFilter, ProgressProc,
    strExclusions);
  If Assigned(ProgressPosProc) Then
    ProgressPosProc(iSection, 1);
  FRightFldr := TFileList.Create(strRightFldr, strRightFilter, ProgressProc,
    strExclusions);
  ProgressProc := ProgressProc;
  If Assigned(ProgressPosProc) Then
    ProgressPosProc(iSection, 2);
  CompareFolders;
  If Assigned(ProgressPosProc) Then
    ProgressPosProc(iSection, 3);
end;

(**

  This is the destructor method for the TCompareFolders class.

  @precon  None.
  @postcon Destroys the left and right folder lists.

**)
destructor TCompareFolders.Destroy;
begin
  FLeftFldr.Free;
  FRightFldr.Free;
  inherited;
end;

{ TCompareFolderCollection }

(**

  This is the constructor method for the TCompareFolderCollection class.

  @precon  slFolders must contain pairs folders fld1=fld2 etc.
  @postcon Creates an instance of the compare folder clases for each pairing.

  @param   slFolders       as a TStringList
  @param   ProgressProc    as a TProgressMsgProc
  @param   ProgressPosProc as a TProgressPosProc
  @param   strExclusions   as a String

**)
constructor TCompareFoldersCollection.Create(slFolders: TStringList;
  ProgressProc : TProgressMsgProc; ProgressPosProc : TProgressPosProc;
  strExclusions : String);

Var
  i : Integer;

begin
  FCompareFolders := TObjectList.Create(True);
  For i := 0 To slFolders.Count - 1 Do
    Begin
      If soEnabled In TSyncOptions(Byte(slFolders.Objects[i])) Then
        FCompareFolders.Add(
          TCompareFolders.Create(
            ExtractFilePath(slFolders.Names[i]),
            ExtractFileName(slFolders.Names[i]),
            ExtractFilePath(slFolders.Values[slFolders.Names[i]]),
            ExtractFileName(slFolders.Values[slFolders.Names[i]]),
            ProgressProc,
            ProgressPosProc,
            strExclusions,
            i,
            TSyncOptions(Byte(slFolders.Objects[i]))
          )
        );
    End;
end;

(**

  This is the destructor method for the TCompareFolderCollection class.

  @precon  None.
  @postcon Destroy the class and all the CompareFolders classes it owns.

**)
destructor TCompareFoldersCollection.Destroy;
begin
  FCompareFolders.Free;
  inherited;
end;

(**

  This is a getter method for the CompareFolders property.

  @precon  The index value must be between 0 and Count - 1 to be valid.
  @postcon Returns the indexed object cast as a TCompareFolders class.

  @param   iIndex as an Integer
  @return  a TCompareFolders

**)
function TCompareFoldersCollection.GetCompareFolders(
  iIndex: Integer): TCompareFolders;
begin
  Result := FCompareFolders[iIndex] As TCompareFolders;
end;

(**

  This is a getter method for the Count property.

  @precon  None.
  @postcon Returns the number of CompareFolders classes within the collection.

  @return  an Integer

**)
Function TCompareFoldersCollection.GetCount : Integer;
begin
  Result := FCompareFolders.Count;
end;

End.
