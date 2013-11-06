(**
  
  This module contains a class to represent a form for displaying the disk space
  before and after the processing of the deletes and copies on all the drives
  referenced.

  @Author  Daviid Hoyle
  @Version 1.0
  @Date    06 Nov 2013
  
**)
Unit DiskSpaceForm;

Interface

Uses
  Winapi.Windows,
  Winapi.Messages,
  System.SysUtils,
  System.Variants,
  System.Classes,
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.Dialogs,
  Vcl.StdCtrls,
  Vcl.Buttons,
  Vcl.ComCtrls,
  SyncModule;

Type
  (** A class to represent a form for displaying the disk space. **)
  TfrmDiskSpace = Class(TForm)
    lblDiskSpace: TLabel;
    lvDiskSpace: TListView;
    btnOK: TBitBtn;
    btnCancel: TBitBtn;
    Procedure lvDiskSpaceResize(Sender: TObject);
  Private
    { Private declarations }
  Public
    { Public declarations }
    Class Function Execute(CFC: TCompareFoldersCollection): Boolean;
  End;

Implementation

{$R *.dfm}

(**

  This is the forms main interface method for interacting with the form..

  @precon  CFC must be a valid instance.
  @postcon Returns true if the dialogue is confirmed.

  @param   CFC as a TCompareFoldersCollection
  @return  a Boolean

**)
Class Function TfrmDiskSpace.Execute(CFC: TCompareFoldersCollection): Boolean;

Var
  iDrive : Integer;
  Item: TListitem;
  i: Integer;
  
Begin
  With TfrmDiskSpace.Create(Nil) Do
    Try
      Result := False;
      For iDrive := 0 To CFC.Drives.Count - 1 Do
        Begin
          Item := lvDiskSpace.Items.Add;
          Item.Caption := CFC.Drives.Drive[iDrive].Drive;
          Item.SubItems.Add(Format('%1.1n', [Int(CFC.Drives.Drive[iDrive].Total) / 1024]));
          Item.SubItems.Add(Format('%1.1n', [Int(CFC.Drives.Drive[iDrive].FreeAtStart) / 1024]));
          Item.SubItems.Add(Format('%1.1n', [Int(CFC.Drives.Drive[iDrive].TotalDeletes) / 1024]));
          Item.SubItems.Add(Format('%1.1n', [Int(CFC.Drives.Drive[iDrive].TotalAdds) / 1024]));
          Item.SubItems.Add(Format('%1.1n', [Int(CFC.Drives.Drive[iDrive].FreeAtFinish) / 1024]));
        End;
      For i := 1 To lvDiskSpace.Columns.Count - 1 Do
        lvDiskSpace.Column[i].Width := 100;
      lvDiskSpaceResize(Nil);
      If ShowModal = mrOK Then
        Result := True;
    Finally
      Free;
    End;
End;

(**

  This is an on resize event handler for the list view.

  @precon  None.
  @postcon Resizes the drive column depending on the width of the dialogue.

  @param   Sender as a TObject

**)
Procedure TfrmDiskSpace.lvDiskSpaceResize(Sender: TObject);

Var
  iWidth : Integer;
  iColumn: Integer;

Begin
  iWidth      := lvDiskSpace.ClientWidth;
  For iColumn := 1 To lvDiskSpace.Columns.Count - 1 Do
    iWidth    := iWidth - lvDiskSpace.Column[iColumn].Width;
  If iWidth < 50 Then
    iWidth := 50;
  lvDiskSpace.Column[0].Width := iWidth;
End;

End.
