(**
  
  This module contains a class to represent a form for displaying the disk space
  before and after the processing of the deletes and copies on all the drives
  referenced.

  @Author  Daviid Hoyle
  @Version 1.0
  @Date    16 Nov 2013
  
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
    procedure lvDiskSpaceCustomDrawItem(Sender: TCustomListView; Item: TListItem;
      State: TCustomDrawState; var DefaultDraw: Boolean);
  Private
    { Private declarations }
    FCFC : TCompareFoldersCollection;
  Public
    { Public declarations }
    Class Function Execute(CFC: TCompareFoldersCollection): Boolean;
  End;

Implementation

{$R *.dfm}

Uses
  CodeSiteLogging,
  dghlibrary;

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
      FCFC := CFC;
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

  This is an on custom draw item for the list view.

  @precon  None.
  @postcon Colours the items on a scale from clWindow, through Amber to red if the drive
           space is between 5 and 0%.

  @param   Sender      as a TCustomListView
  @param   Item        as a TListItem
  @param   State       as a TCustomDrawState
  @param   DefaultDraw as a Boolean as a reference

**)
procedure TfrmDiskSpace.lvDiskSpaceCustomDrawItem(Sender: TCustomListView;
  Item: TListItem; State: TCustomDrawState; var DefaultDraw: Boolean);

Var
  R : Trect;
  dblTotal, dblValue : Double;
  D : TDriveTotal;
  strText: String;
  iSubItem: Integer;
  i : Integer;
  
begin
  DefaultDraw := False;
  D := FCFC.Drives.Drive[Item.Index];
  dblTotal := D.Total;
  dblValue := D.FreeAtFinish;
  R := Item.DisplayRect(drBounds);
  If dblTotal <> 0 Then
    dblValue := dblValue / dblTotal
  Else
    dblValue := 0;
  Sender.Canvas.Brush.Color := CalcColour(dblValue, 0.0, 0.025, 0.05, clRed, $00CCFF,
    clWindow);
  Sender.Canvas.Font.Color := CalcColour(dblValue, 0.0, 0.025, 0.05, clBlack, clBlack,
    clWindowText);
  Sender.Canvas.FillRect(R);
  R := Item.DisplayRect(drLabel);
  Inc(R.Left, 2);
  Dec(R.Right, 2);
  strText := D.Drive;
  DrawText(Sender.Canvas.Handle, PChar(strText), Length(strText), R,
    DT_LEFT Or DT_VCENTER Or DT_END_ELLIPSIS);
  For iSubItem := 0 To Item.SubItems.Count - 1 Do
    Begin
      R := Item.DisplayRect(drBounds);
      For i := 0 To iSubItem Do
        Inc(R.Left, Sender.Column[i].Width);
      R.Right := R.Left + Sender.Column[iSubItem + 1].Width; 
      Inc(R.Left, 2);
      Dec(R.Right, 4);
      strText := Item.SubItems[iSubItem];
      DrawText(Sender.Canvas.Handle, PChar(strText), Length(strText), R,
        DT_RIGHT Or DT_VCENTER Or DT_END_ELLIPSIS);
    End;
end;

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
