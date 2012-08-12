(**

  This module contains a form for displaying the progress of deleting files.

  @Version 1.0
  @Author  David Hoyle
  @Date    12 Aug 2012

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
  SyncModule;

Type
  (** This class represents a form interface for displaying the progress of deleting
      files. **)
  TfrmDeleteProgress = Class(TForm)
    lblFromLabel: TLabel;
    lblFrom: TLabel;
    lblFileLabel: TLabel;
    lblFilename: TLabel;
    pbrOverall: TProgressBar;
    btnCancel: TBitBtn;
    Procedure btnCancelClick(Sender: TObject);
  Private
    { Private declarations }
    FFileCount     : Integer;
    FTotalSize     : Int64;
    FCaption       : String;
    FAbort         : Boolean;
    FUpdateProgress: TUpdateProgress;
    Procedure CheckForCancel;
    Procedure DoUpdateProgress(iPosition, iMaxPosition: Integer);
  Public
    { Public declarations }
    Procedure Initialise(iFileCount : Integer; iTotalSize: Int64);
    Procedure InitialiseFileName(iFile : Integer; strFileName: String);
    Procedure Progress(iFile : Integer; iSize: Int64);
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

  This method intialises the progress dialogue.

  @precon  None.
  @postcon The progress dialogue is initialised.

  @param   iFileCount as an Integer
  @param   iTotalSize as an Int64

**)
Procedure TfrmDeleteProgress.Initialise(iFileCount : Integer; iTotalSize: Int64);

Begin
  FFileCount := iFileCount;
  FTotalSize := iTotalSize;
  FCaption := Caption;
  Show;
  Application.ProcessMessages;
  CheckForCancel;
End;

(**

  This method sets the from and filename captions of the dialogue.

  @precon  None.
  @postcon The filename and from labels are updated.

  @param   iFile       as an Integer
  @param   strFileName as a String

**)
Procedure TfrmDeleteProgress.InitialiseFileName(iFile : Integer; strFileName: String);

Begin
  Caption := Format('%s (%1.0n of %1.0n)', [FCaption, Int(iFile), Int(FFileCount)]);
  lblFrom.Caption     := ExtractFilePath(strFileName);
  lblFilename.Caption := ExtractFileName(strFileName);
  Application.ProcessMessages;
  CheckForCancel;
End;

(**

  This method updates the progress bar on the dialogue.

  @precon  None.
  @postcon The progress bar on the dialogue is updated.

  @param   iFile as an Integer
  @param   iSize as an Int64

**)
Procedure TfrmDeleteProgress.Progress(iFile : Integer; iSize: Int64);

Begin
  pbrOverall.Position := Trunc(Int(iSize) / Int(FTotalSize) * pbrOverall.Max);
  DoUpdateProgress(pbrOverall.Position, pbrOverall.Max);
  Application.ProcessMessages;
  CheckForCancel;
End;

End.
