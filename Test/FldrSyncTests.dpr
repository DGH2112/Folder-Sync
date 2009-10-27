program FldrSyncTests;
{

  Delphi DUnit Test Project
  -------------------------
  This project contains the DUnit test framework and the GUI/Console test runners.
  Add "CONSOLE_TESTRUNNER" to the conditional defines entry in the project options 
  to use the console test runner.  Otherwise the GUI test runner will be used by 
  default.

}

{$IFDEF CONSOLE_TESTRUNNER}
{$APPTYPE CONSOLE}
{$ENDIF}

uses
  ExceptionLog,
  Forms,
  TestFramework,
  GUITestRunner,
  TextTestRunner,
  TestFileComparision in 'Source\TestFileComparision.pas',
  FileComparision in '..\Source\FileComparision.pas',
  TestDGHLibrary in '..\..\..\LIBRARY\Test\Source\TestDGHLibrary.pas',
  dghlibrary in '..\..\..\LIBRARY\dghlibrary.pas',
  ProgressForm in '..\source\ProgressForm.pas' {frmProgress},
  DGHEllipsisLabel in '..\..\..\Components\Source\DGHEllipsisLabel.pas',
  OptionsForm in '..\Source\OptionsForm.pas' {frmOptions},
  FolderPathsForm in '..\Source\FolderPathsForm.pas' {frmFolderPaths};

{$R *.RES}

Var
  T : TTestResult;
  iErrors : Integer;

begin
  iErrors := 0;
  Application.Initialize;
  if IsConsole then
    Begin
      T := TextTestRunner.RunRegisteredTests;
      Try
        iErrors := T.FailureCount + T.ErrorCount;
      Finally
        T.Free;
      End;
    End else
      GUITestRunner.RunRegisteredTests;
  If iErrors > 0 Then
    Halt(iErrors);
end.
