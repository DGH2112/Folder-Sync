(**

  This module defines classes for handling and comparing two directories of
  files.

  @Version 1.0
  @Date    20 Oct 2004
  @Author  David Hoyle

**)
Unit FileComparision;

Interface

Uses
  SysUtils, Classes, Contnrs;

Type
  (** A type to define the status of a file **)
  TStatus = (stNewer, stOlder, stSame);

  (** A record to describe a single file. **)
  TFileRecord = Class
  Private
    FFileName : String;
    FSize : Integer;
    FDateTime : Integer;
    FStatus : TStatus;
  Protected
    Procedure SetStatus(Status : TStatus);
  Public
    Constructor Create(strName : String; iSize : Integer; dtDateTime : Integer;
      Status : TStatus); Virtual;
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

  (** A event method for feeding back progress. **)
  TProgressProc = Procedure(Sender : TObject; boolShow : Boolean;
    strPath, strFile : String; iCount : Integer) Of Object;

  (** This class defines a list of files from a single directory. **)
  TFileList = Class
  Private
    FFolderPath : String;
    FFiles : TObjectList;
    FProgressProc: TProgressProc;
    FExclusions : TStringList;
    FTotalSize : Int64;
    function GetDate(iIndex: Integer): Integer;
    function GetFileName(iIndex: Integer): String;
    function GetSize(iIndex: Integer): Integer;
    function GetStatus(iIndex: Integer): TStatus;
    procedure SetStatus(iIndex: Integer; const Value: TStatus);
    function InExclusions(strFileName: String): Boolean;
    function GetCount: Integer;
    function GetFileRecord(iIndex: Integer): TFileRecord;
  Protected
    Procedure RecurseFolder(strFolderPath : String); Virtual;
    Procedure DoProgress(boolShow : Boolean; strPath, strFile : String;
      iCount : Integer);
    (**
      Provides a indexable type access to the FileRecords.
      @precon  iIndex must be a valid index in the list.
      @postcon Returns the indexed TFileRecord.
      @param   iIndex as       an Integer
      @return  a TFileRecord
    **)
    Property Files[iIndex : Integer] : TFileRecord Read GetFileRecord;
  Public
    Constructor Create(strFolderPath : String;
      ProgressProc : TProgressProc; strExclusions : String); Virtual;
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
      @return  a String
    **)
    Property FileName[iIndex : Integer] : String Read GetFileName;
    (**
      A property to return the size of the indexed filename.
      @precon  iIndex must be from 0 to FCount - 1
      @postcon Returns the size of the indexed filename.
      @param   iIndex as       an Integer
      @return  an Integer
    **)
    Property Size[iIndex : Integer] : Integer Read GetSize;
    (**
      A property to return the date and time of the indexed filename.
      @precon  iIndex must be from 0 to FCount - 1
      @postcon Returns the date and time of the indexed filename.
      @param   iIndex as       an Integer
      @return  a Integer
    **)
    Property DateTime[iIndex : Integer] : Integer Read GetDate;
    (**
      A property to get and set the status of a file.
      @precon  iIndex must be from 0 to FCount - 1
      @postcon Gets or Sets the status of the indexed filename.
      @param   iIndex as       an Integer
      @return  a TStatus
    **)
    Property Status[iIndex : Integer] : TStatus Read GetStatus Write SetStatus;
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
  Private
    FLeftFldr : TFileList;
    FRightFldr : TFileList;
    FProgressProc : TProgressProc;
    FTolerance : Integer;
  Protected
    Procedure CompareFolders;
  Public
    Constructor Create(strLeftFldr, strRightFldr : String;
      ProgressProc : TProgressProc; strExclusions : String;
      iTolerance : Integer); Virtual;
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
  End;

  (** A class to represent a collection of TCompareFolders classes. **)
  TCompareFoldersCollection = Class
  Private
    FCompareFolders : TObjectList;
    Function GetCount : Integer;
    function GetCompareFolders(iIndex: Integer): TCompareFolders;
  Protected
  Public
    Constructor Create(slFolders : TStringList; ProgressProc : TProgressProc;
      strExclusions : String; iTolerance : Integer); Virtual;
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
  FileCtrl;

{ TFileRecord }

(**

  This is the constructor method for the TFileRecord class.

  @precon  None.
  @postcon Initialises the file record with a filename, size, date and time and
           a status.

  @param   strName    as a String
  @param   iSize      as an Integer
  @param   dtDateTime as an Integer
  @param   Status     as a TStatus

**)
constructor TFileRecord.Create(strName : String; iSize : Integer; dtDateTime : Integer;
  Status : TStatus);
begin
  FFileName := strName;
  FSize := iSize;
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
  @param   ProgressProc  as a TProgressProc
  @param   strExclusions as a String

**)
constructor TFileList.Create(strFolderPath: String;
  ProgressProc : TProgressProc; strExclusions : String);
begin
  inherited Create;
  FTotalSize := 0;
  FFiles := TObjectList.Create(True);
  FFolderPath := strFolderPath;
  FProgressProc := ProgressProc;
  FExclusions := TStringList.Create;
  FExclusions.Text := LowerCase(strExclusions);
  DoProgress(True, '', '', 0);
  If Length(strFolderPath) = 0 Then Exit;
  If strFolderPath[Length(strFolderPath)] In ['\', '/'] Then
    Delete(strFolderPath, Length(strFolderPath), 1);
  RecurseFolder(FFolderPath);
end;

(**

  This is the destructor method for the TFileList class.

  @precon  None.
  @postcon Deallocated the  array file records and destroys the class instance.


**)
destructor TFileList.Destroy;
begin
  DoProgress(False, '', '', 0);
  FFiles.Free;
  FExclusions.Free;
  inherited;
end;

(**

  This method updates the progress event method hooked with the current
  progress.

  @precon  None.
  @postcon Fires the progress event if the event handler is hooked.

  @param   boolShow as a Boolean
  @param   strPath as a String
  @param   strFile  as a String
  @param   iCount as an Integer

**)
procedure TFileList.DoProgress(boolShow: Boolean; strPath, strFile: String;
  iCount : Integer);
begin
  If Assigned(FProgressProc) Then
    FProgressProc(Self, boolShow, strPath, strFile, iCount);
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

  This is a getter method for the Date property.

  @precon  iIndex must between 0 and FCount - 1
  @postcon Returns the date of the indexed filename

  @param   iIndex as an Integer
  @return  an Integer

**)
function TFileList.GetDate(iIndex: Integer): Integer;
begin
  If (iIndex < 0) Or (iIndex > Count - 1) Then
    Raise Exception.Create('TFileList index error.');
  Result := Files[iIndex].DateTime;
end;

(**

  This is a getter method for the FileName property.

  @precon  iIndex must between 0 and FCount - 1
  @postcon Returns the filename of the indexed filename

  @param   iIndex as an Integer
  @return  a String

**)
function TFileList.GetFileName(iIndex: Integer): String;
begin
  If (iIndex < 0) Or (iIndex > Count - 1) Then
    Raise Exception.Create('TFileList index error.');
  Result := Files[iIndex].FileName;
end;

(**

  This is a getter method for the FileRecord property.

  @precon  iIndex must be a valid index between 0 and Count -1.
  @postcon Returns the indexed TFileRecord.

  @param   iIndex as an Integer
  @return  a TFileRecord

**)
function TFileList.GetFileRecord(iIndex: Integer): TFileRecord;
begin
  Result := FFiles.Items[iIndex] As TFileRecord;
end;

(**

  This is a getter method for the Size property.

  @precon  iIndex must between 0 and FCount - 1
  @postcon Returns the size of the indexed filename

  @param   iIndex as an Integer
  @return  an Integer

**)
function TFileList.GetSize(iIndex: Integer): Integer;
begin
  If (iIndex < 0) Or (iIndex > Count - 1) Then
    Raise Exception.Create('TFileList index error.');
  Result := Files[iIndex].Size;
end;

(**

  This is a getter method for the Status property.

  @precon  iIndex must be between 0 and FCount - 1
  @postcon Gets the status of the indexed filename.

  @param   iIndex as an Integer
  @return  a TStatus

**)
function TFileList.GetStatus(iIndex: Integer): TStatus;
begin
  If (iIndex < 0) Or (iIndex > Count - 1) Then
    Raise Exception.Create('TFileList index error.');
  Result := Files[iIndex].Status;
end;

(**

  This method recurses the passed folder for file name and adds them to the
  file collection.

  @precon  None.
  @postcon Recurses the passed folder for file name and adds them to the
           file collection.

  @param   strFolderPath as a String

**)
procedure TFileList.RecurseFolder(strFolderPath: String);

Var
  rec : TSearchRec;
  iRes : Integer;
  strFileName, strFCName : String;
  iFirst, iLast, iMid : Integer;

begin
  If Not InExclusions(strFolderPath) Then
    Try
      iRes := FindFirst(strFolderPath + '\*.*', faAnyFile, rec);
      While iRes = 0 Do
        Begin
          If rec.Attr And faDirectory = 0 Then
            Begin
              strFileName := strFolderPath + '\' + rec.Name;
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
                  FFiles.Insert(iFirst, TFileRecord.Create(strFCName, rec.Size, rec.Time,
                    stNewer));
                  Inc(FTotalSize, rec.Size);
                  DoProgress(True, FFolderPath, Files[iFirst].FileName, Count);
                End;
            End Else
              If (rec.Name <> '.') And (rec.Name <> '..') Then
                RecurseFolder(strFolderPath + '\' + rec.Name);
          iRes := FindNext(rec);
        End;
    Finally
      FindClose(rec);
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

(**

  This is a setter method for the Status property.

  @precon  iIndex must be between 0 and FCount - 1
  @postcon Sets the status of the indexed filename.

  @param   iIndex as an Integer
  @param   Value  as a TStatus constant

**)
procedure TFileList.SetStatus(iIndex: Integer; const Value: TStatus);
begin
  If (iIndex < 0) Or (iIndex > Count - 1) Then
    Raise Exception.Create('TFileList index error.');
  Files[iIndex].Status := Value;
end;

{ TCompareFolders }

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

begin
  For iLeft := 0 To LeftFldr.Count - 1 Do
    Begin
      If Assigned(FProgressProc) Then
        FProgressProc(Self, True, 'Comparing Folders... Please wait...',
          LeftFldr.FileName[iLeft], iLeft);
      iRight := RightFldr.Find(LeftFldr.FileName[iLeft]);
      If iRight > -1 Then
        Begin
          If LeftFldr.DateTime[iLeft] - RightFldr.DateTime[iRight] > FTolerance Then
            Begin
              LeftFldr.Status[iLeft] := stNewer;
              RightFldr.Status[iRight] := stOlder;
            End Else
          If LeftFldr.DateTime[iLeft] < RightFldr.DateTime[iRight] Then
            Begin
              LeftFldr.Status[iLeft] := stOlder;
              RightFldr.Status[iRight] := stNewer;
            End Else
            Begin
              LeftFldr.Status[iLeft] := stSame;
              RightFldr.Status[iRight] := stSame;
            End;
        End;
    End;
end;

(**

  This is the constructor method for the TCompareFolders class.

  @precon  None.
  @postcon Creates an instance of TFileList for ther left and right folder.

  @param   strLeftFldr  as a String
  @param   strRightFldr as a String
  @param   ProgressProc as a TProgressProc
  @param   strExclusions as a String
  @param   iTolerance as an Integer

**)
constructor TCompareFolders.Create(strLeftFldr, strRightFldr: String;
  ProgressProc: TProgressProc; strExclusions : String; iTolerance : Integer);

begin
  If Not DirectoryExists(strLeftFldr) Then Exit;
  If Not DirectoryExists(strRightFldr) Then Exit;
  FLeftFldr := TFileList.Create(strLeftFldr, ProgressProc, strExclusions);
  FRightFldr := TFileList.Create(strRightFldr, ProgressProc, strExclusions);
  FProgressProc := ProgressProc;
  FTolerance := iTolerance;
  CompareFolders;
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

  @param   slFolders     as a TStringList
  @param   ProgressProc  as a TProgressProc
  @param   strExclusions as a String
  @param   iTolerance    as an Integer

**)
constructor TCompareFoldersCollection.Create(slFolders: TStringList;
  ProgressProc : TProgressProc; strExclusions : String; iTolerance : Integer);

Var
  i : Integer;

begin
  FCompareFolders := TObjectList.Create(True);
  For i := 0 To slFolders.Count - 1 Do
    FCompareFolders.Add(
      TCompareFolders.Create(
        slFolders.Names[i],
        slFolders.Values[slFolders.Names[i]],
        ProgressProc,
        strExclusions,
        iTolerance
      )
    );
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
