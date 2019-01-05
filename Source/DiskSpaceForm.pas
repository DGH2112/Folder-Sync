(**

  This module contains a class to represent a form for displaying the disk space
  before and after the processing of the deletes and copies on all the drives
  referenced.

  @Author  Daviid Hoyle
  @Version 1.0
  @Date    02 Jan 2019

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
  SyncModule,
  VirtualTrees;

Type
  (** A class to represent a form for displaying the disk space. **)
  TfrmDiskSpace = Class(TForm)
    lblDiskSpace: TLabel;
    btnOK: TBitBtn;
    btnCancel: TBitBtn;
    vstDiskSpace: TVirtualStringTree;
    procedure vstDiskSpaceGetText(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; TextType: TVSTTextType; var CellText: string);
    procedure vstDiskSpaceAfterItemErase(Sender: TBaseVirtualTree; TargetCanvas: TCanvas;
      Node: PVirtualNode; ItemRect: TRect);
  Private
    { Private declarations }
    FCFC : TCompareFoldersCollection;
  Public
    { Public declarations }
    Class Function Execute(Const CFC: TCompareFoldersCollection): Boolean;
  End;

Implementation

{$R *.dfm}

Uses
  {$IFDEF DEBUG}
  CodeSiteLogging,
  {$ENDIF}
  VCL.Themes,
  FldrSync.Functions;

Type
  (** This is a record to describe the node data for each virtual tree node. **)
  TTreeData = Record
    Drive : TDriveTotal;
  End;

(**

  This is the forms main interface method for interacting with the form..

  @precon  CFC must be a valid instance.
  @postcon Returns true if the dialogue is confirmed.

  @param   CFC as a TCompareFoldersCollection as a constant
  @return  a Boolean

**)
Class Function TfrmDiskSpace.Execute(Const CFC: TCompareFoldersCollection): Boolean;

Var
  iDrive : Integer;
  N : PVirtualNode;
  ND : ^TTreeData;
  F: TfrmDiskSpace;

Begin
  F := TfrmDiskSpace.Create(Nil);
  Try
    Result := False;
    F.vstDiskSpace.NodeDataSize := SizeOf(TTreeData);
    F.FCFC := CFC;
    For iDrive := 0 To CFC.Drives.Count - 1 Do
      Begin
        N := F.vstDiskSpace.AddChild(Nil);
        ND := F.vstDiskSpace.GetNodeData(N);
        ND.Drive := CFC.Drives.Drive[iDrive];
      End;
    If F.ShowModal = mrOK Then
      Result := True;
  Finally
    F.Free;
  End;
End;

(**

  This is an on after item erase event handler for the virtual tree view.

  @precon  None.
  @postcon Colours items with low disk space with a coloured background.

  @param   Sender       as a TBaseVirtualTree
  @param   TargetCanvas as a TCanvas
  @param   Node         as a PVirtualNode
  @param   ItemRect     as a TRect

**)
Procedure TfrmDiskSpace.vstDiskSpaceAfterItemErase(Sender: TBaseVirtualTree;
  TargetCanvas: TCanvas; Node: PVirtualNode; ItemRect: TRect);

Const
  iAmber = $00CCFF;
  dbl2_5PerCent = 0.025;
  dbl5PerCent = 0.05;

Var
  ND : ^TTreeData;
  dblTotal: Double;
  dblValue: Double;

Begin
  ND := Sender.GetNodeData(Node);
  dblTotal := ND.Drive.Total;
  dblValue := ND.Drive.FreeAtFinish;
  If dblTotal <> 0 Then
    dblValue := dblValue / dblTotal
  Else
    dblValue := 0;
  TargetCanvas.Brush.Color := TFSFunctions.CalcColour(dblValue, 0.0, dbl2_5PerCent, dbl5PerCent,
    StyleServices.GetSystemColor(clRed),
    StyleServices.GetSystemColor(iAmber),
    StyleServices.GetSystemColor(clWindow));
  TargetCanvas.Font.Color := TFSFunctions.CalcColour(dblValue, 0.0, dbl2_5PerCent, dbl5PerCent,
    StyleServices.GetSystemColor(clBlack),
    StyleServices.GetSystemColor(clBlack),
    StyleServices.GetSystemColor(clWindowText));
  TargetCanvas.FillRect(ItemRect);
End;

(**

  This is an on get tetx event handler for the virtual tree view.

  @precon  None.
  @postcon Returns the appropriate column of data for each field.

  @param   Sender   as a TBaseVirtualTree
  @param   Node     as a PVirtualNode
  @param   Column   as a TColumnIndex
  @param   TextType as a TVSTTextType
  @param   CellText as a String as a reference

**)
Procedure TfrmDiskSpace.vstDiskSpaceGetText(Sender: TBaseVirtualTree; Node: PVirtualNode;
  Column: TColumnIndex; TextType: TVSTTextType; Var CellText: String);

Type
  TFSDiskSpaceField = (dsfDrive, dsfTotal, dsfFreeAtStart, dsfTotalDeletes, dsfTotalAdds,
    dsfFreeAtFinish);

Const
  dblKiloByte = 1024.0;

Var
  ND : ^TTreeData;

Begin
  ND := Sender.GetNodeData(Node);
  Case TFSDiskSpaceField(Column) Of
    dsfDrive:        CellText := ND.Drive.Drive;
    dsfTotal:        CellText := Format('%1.1n', [Int(ND.Drive.Total) / dblKiloByte]);
    dsfFreeAtStart:  CellText := Format('%1.1n', [Int(ND.Drive.FreeAtStart) / dblKiloByte]);
    dsfTotalDeletes: CellText := Format('%1.1n', [Int(ND.Drive.TotalDeletes) / dblKiloByte]);
    dsfTotalAdds:    CellText := Format('%1.1n', [Int(ND.Drive.TotalAdds) / dblKiloByte]);
    dsfFreeAtFinish: CellText := Format('%1.1n', [Int(ND.Drive.FreeAtFinish) / dblKiloByte]);
  End;
End;

End.
