unit TestFileComparision;
{

  Delphi DUnit Test Case
  ----------------------
  This unit contains a skeleton test case class generated by the Test Case Wizard.
  Modify the generated code to correctly setup and call the methods from the unit
  being tested.

}

interface

uses
  TestFramework, Windows, SyncModule, SysUtils, Contnrs, Classes;
type

  TestTFileRecord = class(TTestCase)
  private
    FFileRecord: TFileRecord;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestFileName;
    procedure TestSize;
    procedure TestAttribute;
    Procedure TestDateTime;
    Procedure TestStatus;
  end;

  TestTFileList = class(TTestCase)
  private
    FFileList: TFileList;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    Procedure TestCount;
    Procedure TestFolderPath;
    Procedure TestTotalSize;
    Procedure TestFiles;
  end;

  TestTCompareFolders = class(TTestCase)
  private
    FCompareFolders: TCompareFolders;
    function CheckDates(strLeftDate, strRightDate: String; iSize: Integer;
      Check : TCheckDifference): Boolean;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestCheckDifference;
    Procedure TestCheckFolders;
  end;

implementation

procedure TestTFileRecord.SetUp;
begin
  FFileRecord := TFileRecord.Create('Daves File.Txt', 123456, faArchive,
    DateTimeToFileDate(StrToDateTime('07/01/2007 10:11:12')), stNewer);
end;

procedure TestTFileRecord.TearDown;
begin
  FFileRecord.Free;
  FFileRecord := nil;
end;

procedure TestTFileRecord.TestAttribute;
begin
  Check(FFileRecord.Attributes = faArchive);
end;

procedure TestTFileRecord.TestDateTime;
begin
  CheckEquals(StrToDateTime('07/01/2007 10:11:12'), FileDateToDateTime(FFileRecord.DateTime));
end;

procedure TestTFileRecord.TestFileName;

begin
  CheckEquals('Daves File.Txt', FFileRecord.FileName);
end;

procedure TestTFileRecord.TestSize;
begin
  CheckEquals(123456, FFileRecord.Size);
end;

procedure TestTFileRecord.TestStatus;
begin
  Check(FFileRecord.Status = stNewer);
  FFileRecord.Status := stOlder;
  Check(FFileRecord.Status = stOlder);
end;

procedure TestTFileList.SetUp;
begin
  FFileList := TFileList.Create;
  FFileList.SearchFolder('.\Test Compare Folders\Left Folder\', '*.*', '', []);
end;

procedure TestTFileList.TearDown;
begin
  FFileList.Free;
  FFileList := nil;
end;

procedure TestTFileList.TestCount;
begin
  CheckEquals(4, FFileList.Count);
end;

procedure TestTFileList.TestFiles;
begin
  CheckEquals('20 Bytes.Txt', FFileList[1].FileName);
  CheckEquals(20, FFileList[1].Size);
end;

procedure TestTFileList.TestFolderPath;
begin
  CheckEquals('.\Test Compare Folders\Left Folder\', FFileList.FolderPath);
end;

procedure TestTFileList.TestTotalSize;
begin
  CheckEquals(100, FFileList.TotalSize);
end;

procedure TestTCompareFolders.SetUp;
begin
  FCompareFolders := TCompareFolders.Create;
  FCompareFolders.SearchFolders('.\Test Compare Folders\Left Folder\',
    '.\Test Compare Folders\Right Folder\', '*.*', '', 0, []);
end;

procedure TestTCompareFolders.TearDown;
begin
  FCompareFolders.Free;
  FCompareFolders := nil;
end;

Function TestTCompareFolders.CheckDates(strLeftDate, strRightDate : String;
  iSize : Integer; Check : TCheckDifference) : Boolean;

Var
  dtLeftDate, dtRightDate : TDateTime;
  iLeftDate, iRightDate : Integer;

Begin
  dtLeftDate := StrToDateTime(strLeftDate);
  dtRightDate := StrToDateTime(strRightDate);
  iLeftDate := DateTimeToFileDate(dtLeftDate);
  iRightDate := DateTimeToFileDate(dtRightDate);
  Result := FCompareFolders.CheckDifference(iLeftDate - iRightDate, iSize, Check);
End;

procedure TestTCompareFolders.TestCheckDifference;

begin
  Check(Not CheckDates('07/01/2007 11:11:12', '07/01/2007 10:11:12', 0, cdNewer), 'Same Day, 1 Hour After');
  Check(Not CheckDates('07/01/2007 09:11:12', '07/01/2007 10:11:12', 0, cdOlder), 'Same Day, 1 Hour Before');
  Check(CheckDates('07/01/2007 09:41:12', '07/01/2007 10:11:12', 0, cdOlder), 'Same Day, 1/2 Hour Before');
  Check(CheckDates('07/01/2007 08:41:12', '07/01/2007 10:11:12', 0, cdOlder), 'Same Day, 1 1/2 Hours Before');
  Check(CheckDates('07/01/2007 10:41:12', '07/01/2007 10:11:12', 0, cdNewer), 'Same Day, 1/2 hour After');
  Check(CheckDates('07/01/2007 11:41:12', '07/01/2007 10:11:12', 0, cdNewer), 'Same Day, 1 1/2 Hours After');
  Check(Not CheckDates('07/01/2007 10:11:12', '07/01/2007 10:11:12', 0, cdOlder), 'Same Day, Same Time');
  Check(Not CheckDates('08/01/2007 00:11:12', '07/01/2007 23:11:12', 0, cdNewer), 'Diff Day, 1 Hour After');
  Check(Not CheckDates('07/01/2007 23:11:12', '08/01/2007 00:11:12', 0, cdOlder), 'Diff Day, 1 Hour Before');
  Check(CheckDates('07/01/2007 23:41:12', '08/01/2007 00:11:12', 0, cdOlder), 'Diff Day, 1/2 Hour Before');
  Check(CheckDates('07/01/2007 22:41:12', '08/01/2007 00:11:12', 0, cdOlder), 'Diff Day, 1 1/2 Hours Before');
  Check(CheckDates('08/01/2007 00:11:12', '07/01/2007 23:41:12', 0, cdNewer), 'Diff Day, 1/2 Hour Before');
  Check(CheckDates('08/01/2007 00:41:12', '07/01/2007 23:11:12', 0, cdNewer), 'Diff Day, 1 1/2 Hours Before');
end;


procedure TestTCompareFolders.TestCheckFolders;
begin
  CheckEquals(4, FCompareFolders.LeftFldr.Count);
  CheckEquals('20 Bytes.Txt', FCompareFolders.LeftFldr[1].FileName);
  CheckEquals(20, FCompareFolders.LeftFldr[1].Size);
  CheckEquals('.\Test Compare Folders\Left Folder\', FCompareFolders.LeftFldr.FolderPath);
  CheckEquals(100, FCompareFolders.LeftFldr.TotalSize);
  CheckEquals(4, FCompareFolders.RightFldr.Count);
  CheckEquals('30 Bytes.Txt', FCompareFolders.RightFldr[1].FileName);
  CheckEquals(30, FCompareFolders.RightFldr[1].Size);
  CheckEquals('.\Test Compare Folders\Right Folder\', FCompareFolders.RightFldr.FolderPath);
  CheckEquals(140, FCompareFolders.RightFldr.TotalSize);
end;

initialization
  // Register any test cases with the test runner
  RegisterTest('File Comparison Tests', TestTFileRecord.Suite);
  RegisterTest('File Comparison Tests', TestTFileList.Suite);
  RegisterTest('File Comparison Tests', TestTCompareFolders.Suite);
end.


