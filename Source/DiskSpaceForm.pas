(**

  This module contains a class to represent a form for displaying the disk space
  before and after the processing of the deletes and copies on all the drives
  referenced.

  @Author  Daviid Hoyle
  @Version 1.0
  @Date    25 Mar 2016

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
    Class Function Execute(CFC: TCompareFoldersCollection): Boolean;
  End;

Implementation

{$R *.dfm}

Uses
  CodeSiteLogging,
  dghlibrary,
  Themes;

Type
  (** This is a record to describe the node data for each virtual tree node. **)
  TTreeData = Record
    Drive : TDriveTotal;
  End;

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
  N : PVirtualNode;
  ND : ^TTreeData;

Begin
  With TfrmDiskSpace.Create(Nil) Do
    Try
      Result := False;
      vstDiskSpace.NodeDataSize := SizeOf(TTreeData);
      FCFC := CFC;
      For iDrive := 0 To CFC.Drives.Count - 1 Do
        Begin
          N := vstDiskSpace.AddChild(Nil);
          ND := vstDiskSpace.GetNodeData(N);
          ND.Drive := CFC.Drives.Drive[iDrive];
        End;
      If ShowModal = mrOK Then
        Result := True;
    Finally
      Free;
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
  TargetCanvas.Brush.Color := CalcColour(dblValue, 0.0, 0.025, 0.05,
    StyleServices.GetSystemColor(clRed),
    StyleServices.GetSystemColor($00CCFF),
    StyleServices.GetSystemColor(clWindow));
  TargetCanvas.Font.Color := CalcColour(dblValue, 0.0, 0.025, 0.05,
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

Var
  ND : ^TTreeData;

Begin
  ND := Sender.GetNodeData(Node);
  Case Column Of
    0: CellText := ND.Drive.Drive;
    1: CellText := Format('%1.1n', [Int(ND.Drive.Total) / 1024]);
    2: CellText := Format('%1.1n', [Int(ND.Drive.FreeAtStart) / 1024]);
    3: CellText := Format('%1.1n', [Int(ND.Drive.TotalDeletes) / 1024]);
    4: CellText := Format('%1.1n', [Int(ND.Drive.TotalAdds) / 1024]);
    5: CellText := Format('%1.1n', [Int(ND.Drive.FreeAtFinish) / 1024]);
  End;
End;

End.
