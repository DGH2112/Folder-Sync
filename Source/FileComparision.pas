(**

  This module defines classes for handling and comparing two directories of
  files.

  @Version 1.0
  @Date    08 Apr 2004
  @Author  David Hoyle

**)
Unit FileComparision;

Interface

Uses
  SysUtils, Classes;

Type
  (** A type to define the status of a file **)
  TStatus = (stNewer, stOlder, stSame);

  (** A record to describe a single file. **)
  TFileRecord = Record
    strFileName : String;
    iSize : Integer;
    iDateTime : Integer;
    Status : TStatus;
  End;

  (** A to describe an array of TFileRecords **)
  TFileRecords = Array of TFileRecord;

  (** A event method for feeding back progress. **)
  TProgressProc = Procedure(Sender : TObject; boolShow : Boolean;
    strPath, strFile : String; iCount : Integer) Of Object;

  (** This class defines a list of files from a single directory. **)
  TFileList = Class
  Private
    FFolderPath : String;
    FCount : Integer;
    FFiles : TFileRecords;
    FProgressProc: TProgressProc;
    FExclusions : TStringList;
    function GetDate(iIndex: Integer): Integer;
    function GetFileName(iIndex: Integer): String;
    function GetSize(iIndex: Integer): Integer;
    function GetStatus(iIndex: Integer): TStatus;
    procedure SetStatus(iIndex: Integer; const Value: TStatus);
    function InExclusions(strFileName: String): Boolean;
  Protected
    Procedure RecurseFolder(strFolderPath : String); Virtual;
    Procedure Sort; Virtual;
    Procedure DoProgress(boolShow : Boolean; strPath, strFile : String;
      iCount : Integer);
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
    Property Count : Integer Read FCount;
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

Implementation

Const
  (** The growth capacity of the FileList collections. **)
  iCAPACITY : Integer = 10;

{ TFileList }

(**

  This is the constructor method for the TFileList class.

  @precon  None.
  @postcon Creates the class instance and attempts to recurse the passed folder
           for files.

  @param   strFolderPath as a String
  @param   ProgressProc  as a TProgressProc

**)
constructor TFileList.Create(strFolderPath: String;
  ProgressProc : TProgressProc; strExclusions : String);
begin
  inherited Create;
  FCount := 0;
  SetLength(FFiles, iCAPACITY);
  FFolderPath := strFolderPath;
  FProgressProc := ProgressProc;
  FExclusions := TStringList.Create;
  FExclusions.Text := LowerCase(strExclusions);
  DoProgress(True, '', '', 0);
  If Length(strFolderPath) = 0 Then Exit;
  If strFolderPath[Length(strFolderPath)] In ['\', '/'] Then
    Delete(strFolderPath, Length(strFolderPath), 1);
  RecurseFolder(FFolderPath);
  Sort;
end;

(**

  This is the destructor method for the TFileList class.

  @precon  None.
  @postcon Deallocated the  array file records and destroys the class instance.


**)
destructor TFileList.Destroy;
begin
  DoProgress(False, '', '', 0);
  FFiles := Nil;
  FExclusions.Free;
  inherited;
end;

(**

  This method updates the progress event method hooked with the current
  progress.

  @precon  None.
  @postcon Fires the progress event if the event handler is hooked.

  @param   boolShow as a Boolean
  @param   strFile  as a String

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
  iLast := FCount - 1;
  While iLast >= iFirst Do
    Begin
      iMid := (iFirst + iLast) Div 2;
      If AnsiCompareFileName(strFileName, FFiles[iMid].strFileName) = 0 Then
        Begin
          Result := iMid;
          Exit;
        End;
      If AnsiCompareFileName(strFileName, FFiles[iMid].strFileName) < 0 Then
        iLast := iMid - 1
      Else
        iFirst := iMid + 1;
    End;
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
  If (iIndex < 0) Or (iIndex > FCount - 1) Then
    Raise Exception.Create('TFileList index error.');
  Result := FFiles[iIndex].iDateTime;
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
  If (iIndex < 0) Or (iIndex > FCount - 1) Then
    Raise Exception.Create('TFileList index error.');
  Result := FFiles[iIndex].strFileName;
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
  If (iIndex < 0) Or (iIndex > FCount - 1) Then
    Raise Exception.Create('TFileList index error.');
  Result := FFiles[iIndex].iSize;
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
  If (iIndex < 0) Or (iIndex > FCount - 1) Then
    Raise Exception.Create('TFileList index error.');
  Result := FFiles[iIndex].Status;
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
  tmpFiles : TFileRecords;
  i : Integer;
  strFileName : String;

begin
  tmpFiles := Nil;
  Try
    iRes := FindFirst(strFolderPath + '\*.*', faAnyFile, rec);
    While iRes = 0 Do
      Begin
        If rec.Attr And faDirectory = 0 Then
          Begin
            strFileName := strFolderPath + '\' + rec.Name;
            If Not InExclusions(strFileName) Then
              Begin
                Inc(FCount);
                If FCount > High(FFiles) + 1 Then
                  Begin
                    tmpFiles := Copy(FFiles, 0, High(FFiles));
                    SetLength(FFiles, High(FFiles) + iCAPACITY);
                    For i := Low(tmpFiles) To High(tmpFiles) Do
                      FFiles[i] := tmpFiles[i];
                  End;
                FFiles[FCount - 1].strFileName := strFileName;
                Delete(FFiles[FCount - 1].strFileName, 1, Length(FFolderPath));
                FFiles[FCount - 1].iSize := rec.Size;
                FFiles[FCount - 1].iDateTime := rec.Time;
                FFiles[FCount - 1].Status := stNewer;
                DoProgress(True, FFolderPath, FFiles[FCount - 1].strFileName,
                  FCount);
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
  If (iIndex < 0) Or (iIndex > FCount - 1) Then
    Raise Exception.Create('TFileList index error.');
  FFiles[iIndex].Status := Value;
end;

(**

  This method sorts the list of files in the collection in ascending alpha-
  numeric order.

  @precon  None.
  @postcon Sorts the list of files in the collection in ascending alpha-
           numeric order

**)
procedure TFileList.Sort;

Var
  i, j : Integer;
  tmpRec : TFileRecord;

begin
  For i := Low(FFiles) To FCount - 1 Do
    Begin
      DoProgress(True, FFolderPath + ', Sorting list...', FFiles[i].strFileName, i);
      For j := i + 1 To FCount - 1 Do
        If AnsiCompareFileName(FFiles[i].strFileName, FFiles[j].strFileName) > 0 Then
          Begin
            tmpRec := FFiles[i];
            FFiles[i] := FFiles[j];
            FFiles[j] := tmpRec;
          End;
    End;
end;

{ TCompareFolders }

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

**)
constructor TCompareFolders.Create(strLeftFldr, strRightFldr: String;
  ProgressProc: TProgressProc; strExclusions : String; iTolerance : Integer);
begin
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

End.
