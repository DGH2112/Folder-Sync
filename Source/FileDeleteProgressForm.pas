(**

  This module contains a form for displaying the progress of deleting files.

  @Version 1.0
  @Author  David Hoyle
  @Date    16 Oct 2015

**)
Unit FileDeleteProgressForm;

Interface

Uses
  Windows,
  Messages,
  SysUtils,
  Variants,
  Classes,
  Graphics,
  Controls,
  Forms,
  Dialogs,
  StdCtrls,
  Buttons,
  ComCtrls,
  SyncModule,
  UITypes,
  Vcl.ExtCtrls;

Type
  (** An enumerate to define the deletion type to be displayed **)
  TDeleteType = (dtFiles, dtFolders);
  (** This class represents a form interface for displaying the progress of deleting
      files. **)
  TfrmDeleteProgress = Class(TForm)
    lblFromLabel: TLabel;
    lblFrom: TLabel;
    lblFileLabel: TLabel;
    lblFilename: TLabel;
    pbrOverall: TProgressBar;
    btnCancel: TBitBtn;
    lblDeleteStatus: TLabel;
    lblProgress: TLabel;
    GridPanel: TGridPanel;
    Procedure btnCancelClick(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure FormShow(Sender: TObject);
  Private
    { Private declarations }
    FCaption       : String;
    FAbort         : Boolean;
    FUpdateProgress: TUpdateProgress;
    FStartTime     : Double;
    Procedure CheckForCancel;
    Procedure DoUpdateProgress(iPosition, iMaxPosition: Integer);
  Public
    { Public declarations }
    Procedure Initialise(iType : TDeleteType; iTotalFileCount : Integer;
      iTotalFileSize : Int64; iWidth : Integer);
    Procedure InitialiseFileName(iType : TDeleteType; iCurrentFileToDelete,
      iTotalFilesToDelete : Integer; strFileName: String);
    Procedure Progress(iCumulativeFileSizeAfterDelete, iTotalFileSizeToDelete: Int64);
    (**
      This property defines a call back event handler for getting the current progress
      position from the dialogue.
      @precon  None.
      @postcon Provides progress information.
      @return  a TUpdateProgress
    **)
    Property OnUpdateProgress: TUpdateProgress Read FUpdateProgress Write FUpdateProgress;
  End;

Implementation

uses
  ApplicationFunctions;

{$R *.dfm}
{ TfrmDeleteProgress }

(**

  This is an on click event handler for the Cancel button.

  @precon  None.
  @postcon The internal variable FAbort is changed to true.

  @param   Sender as a TObject

**)
Procedure TfrmDeleteProgress.btnCancelClick(Sender: TObject);

Begin
  FAbort  := True;
  Caption := Caption + ' (Cancel Pending)';
End;

(**

  This method checks whether the process should be aborted and prompts the user as to
  whether they really do want to cancel. If yes, an Abort exception is raise.

  @precon  None.
  @postcon Aborts the current process if the process is confirmed as wanting to be
           cancelled.

**)
Procedure TfrmDeleteProgress.CheckForCancel;

Const
  strMsg = 'Are you sure you want to cancel this operation?';

Begin
  If FAbort Then
    Case MessageDlg(strMsg, mtConfirmation, [mbYes, mbNo], 0) Of
      mrYes:
        Abort;
      mrNo:
        FAbort := False;
    End;
End;

(**

  This method fires the Update Progress event if assigned.

  @precon  None.
  @postcon The Update Progress event is fired.

  @param   iPosition    as an Integer
  @param   iMaxPosition as an Integer

**)
Procedure TfrmDeleteProgress.DoUpdateProgress(iPosition, iMaxPosition: Integer);

Begin
  If Assigned(FUpdateProgress) Then
    FUpdateProgress(iPosition, iMaxPosition);
End;

(**

  This is an on resize event handler for the form.

  @precon  None.
  @postcon Ensures that the cancel button is centred on the form.

  @param   Sender as a TObject

**)
Procedure TfrmDeleteProgress.FormResize(Sender: TObject);

Begin
  btnCancel.Left := (ClientWidth - btnCancel.Width) Div 2;
End;

(**

  This is an on show event handler for the form.

  @precon  None.
  @postcon Ensures that the cancel buttons is centred on the form.

  @param   Sender as a TObject

**)
Procedure TfrmDeleteProgress.FormShow(Sender: TObject);

Begin
  FormResize(Sender);
End;

(**

  This method intialises the progress dialogue.

  @precon  None.
  @postcon The progress dialogue is initialised.

  @param   iType           as a TDeleteType
  @param   iTotalFileCount as an Integer
  @param   iTotalFileSize  as an Int64
  @param   iWidth          as an Integer

**)
Procedure TfrmDeleteProgress.Initialise(iType : TDeleteType; iTotalFileCount : Integer;
  iTotalFileSize : Int64; iWidth : Integer);

Begin
  FStartTime := Now();
  If iType = dtFiles Then
    FCaption := 'Deleting Files'
  Else
    FCaption := 'Deleting Folders';
  If iType = dtFiles Then
    lblFileLabel.Caption := 'Filename'
  Else
    lblFileLabel.Caption := 'Foldername';
  FCaption := Caption;
  If iWidth > 0 Then
    Width := iWidth;
  Show;
  Application.ProcessMessages;
  CheckForCancel;
End;

(**

  This method sets the from and filename captions of the dialogue.

  @precon  None.
  @postcon The filename and from labels are updated.

  @param   iType                as a TDeleteType
  @param   iCurrentFileToDelete as an Integer
  @param   iTotalFilesToDelete  as an Integer
  @param   strFileName          as a String

**)
Procedure TfrmDeleteProgress.InitialiseFileName(iType : TDeleteType; iCurrentFileToDelete,
  iTotalFilesToDelete : Integer; strFileName: String);

Begin
  Caption := Format('%s (%1.0n of %1.0n)', [FCaption, Int(iCurrentFileToDelete),
    Int(iTotalFilesToDelete)]);
  If iType = dtFiles Then
    Begin
      lblFrom.Caption     := ExtractFilePath(strFileName);
      lblFilename.Caption := ExtractFileName(strFileName);
    End Else
    Begin
      lblFrom.Caption     := ExtractFilePath(Copy(strFileName, 1, Length(strFileName) - 1));
      lblFilename.Caption := ExtractFileName(Copy(strFileName, 1, Length(strFileName) - 1));
    End;
  Application.ProcessMessages;
  CheckForCancel;
End;

(**

  This method updates the progress bar on the dialogue.

  @precon  None.
  @postcon The progress bar on the dialogue is updated.

  @param   iCumulativeFileSizeAfterDelete as an Int64
  @param   iTotalFileSizeToDelete         as an Int64

**)
Procedure TfrmDeleteProgress.Progress(iCumulativeFileSizeAfterDelete,
  iTotalFileSizeToDelete: Int64);

Const
  dblFactor : Double = 1024.0;

Begin
  If iTotalFileSizeToDelete = 0 Then
    Inc(iTotalFileSizeToDelete);
  lblDeleteStatus.Caption := Format('Deleted %1.0n kbytes in %1.0n kbytes [%1.1f%%]',
    [Int(iCumulativeFileSizeAfterDelete) /
     dblFactor, Int(iTotalFileSizeToDelete) / dblFactor,
    Int(iCumulativeFileSizeAfterDelete) / Int(iTotalFileSizeToDelete) * 100.0]);
  pbrOverall.Position := Trunc(Int(iCumulativeFileSizeAfterDelete) /
    Int(iTotalFileSizeToDelete) * pbrOverall.Max);
  pbrOverall.Position := pbrOverall.Position - 1;
  pbrOverall.Position := pbrOverall.Position + 1;
  lblProgress.Caption := UpdateRemainingTime(FStartTime,
    Int(iCumulativeFileSizeAfterDelete) / Int(iTotalFileSizeToDelete));
  DoUpdateProgress(pbrOverall.Position, pbrOverall.Max);
  Application.ProcessMessages;
  CheckForCancel;
End;

End.
