(**

  This module contains a form to display the progress of both an individual file copy
  and the overall file copy process.

  @Author  David Hoyle
  @Version 1.0
  @Date    13 Dec 2014

**)
Unit FileCopyProgressForm;

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
  Vcl.Samples.Gauges,
  UITypes;

Type
  (** This is a class to represent the form interface for displaying progress. **)
  TfrmCopyProgress = Class(TForm)
    lblFromLabel: TLabel;
    lblFrom: TLabel;
    lblToLabel: TLabel;
    lblTo: TLabel;
    lblFileLabel: TLabel;
    lblFilename: TLabel;
    lblOverall: TLabel;
    pbrOverall: TProgressBar;
    pbrFile: TProgressBar;
    lblFile: TLabel;
    lblBytesCopied: TLabel;
    lblBytesOverallCopied: TLabel;
    btnCancel: TBitBtn;
    lblRemainingTime: TLabel;
    Procedure btnCancelClick(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure FormShow(Sender: TObject);
  Strict Private
    { Private declarations }
    FFileCount     : Integer;
    FTotalSize     : Int64;
    FCaption       : String;
    FSize          : Int64;
    FStartTick     : Int64;
    FLastTick      : Int64;
    FAbort         : Boolean;
    FStartTime: TDateTime;
    FUpdateProgress: TUpdateProgress;
    Procedure CheckForCancel;
    Procedure DoUpdateProgress(iPosition, iMaxPosition: Integer);
    Procedure UpdateOverallProgress(iSize : Int64);
    Procedure UpdateRemainingTime(dblProgress : Double);
  Public
    { Public declarations }
    Procedure Initialise(iFileCount : Integer; iTotalSize: Int64; iWidth : Integer);
    Procedure Progress(iFile : Integer; strSource, strDest,
      strFileName: String); Overload;
    Procedure Progress(iCopiedSize, iTotalSize: Int64); Overload;
    Procedure ProgressOverall(iSize: Int64);
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

Const
  (** This is a division factor for the output display (kilobytes). **)
  dblFactor : Double = 1024.0;
{ TfrmCopyProgress }

(**

  This is an on click event handler for the Cancel button.

  @precon  None.
  @postcon Changes the FAbort variable to true.

  @param   Sender as a TObject

**)
Procedure TfrmCopyProgress.btnCancelClick(Sender: TObject);

Begin
  FAbort  := True;
  Caption := Caption + ' (Cancel Pending)';
End;

(**

  This method checks the FAbort variable for true and if so asks the user if they really
  do want to cancel the process and if so raises an Abort exception.

  @precon  None.
  @postcon If FAbort is true and the user confirms the cancelation, an Abort exception
           is raised.

**)
Procedure TfrmCopyProgress.CheckForCancel;

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

  This metohd fires the Update Progress event handler is assigned.

  @precon  None.
  @postcon The update progress event handler is fired.

  @param   iPosition    as an Integer
  @param   iMaxPosition as an Integer

**)
Procedure TfrmCopyProgress.DoUpdateProgress(iPosition, iMaxPosition: Integer);

Begin
  If Assigned(FUpdateProgress) Then
    FUpdateProgress(iPosition, iMaxPosition);
End;

(**

  This is an on resize method for the form.

  @precon  None.
  @postcon Centres the Cancel button on the form.

  @param   Sender as a TObject

**)
Procedure TfrmCopyProgress.FormResize(Sender: TObject);

Begin
  btnCancel.Left := (ClientWidth - btnCancel.Width) Div 2;
End;

(**

  This is an on show event handler for the Form.

  @precon  None.
  @postcon Ensures the cancel button is centred on the form.

  @param   Sender as a TObject

**)
Procedure TfrmCopyProgress.FormShow(Sender: TObject);

Begin
  FormResize(Sender);
End;

(**

  This method initialises the dialogue for the copying process.

  @precon  None.
  @postcon The copy progress dialogue is intialised.

  @param   iFileCount as an Integer
  @param   iTotalSize as an Int64
  @param   iWidth     as an Integer

**)
Procedure TfrmCopyProgress.Initialise(iFileCount : Integer; iTotalSize: Int64;
  iWidth : Integer);

Begin
  FCaption := Caption;
  FFileCount := iFileCount;
  FTotalSize := iTotalSize;
  If iWidth > 0 Then
    Width := iWidth;
  FStartTime := Now;
  Show;
  Application.ProcessMessages;
  CheckForCancel;
End;

(**

  This method updates the source, destinatation and filename labels at the start of the
  copying of an individual file.

  @precon  None.
  @postcon The Source, Destinationa dn Filename labels are updated and the progress bar
           for the file set to zero.

  @param   iFile       as an Integer
  @param   strSource   as a String
  @param   strDest     as a String
  @param   strFileName as a String

**)
Procedure TfrmCopyProgress.Progress(iFile : Integer; strSource, strDest,
  strFileName: String);

Begin
  Caption := Format('%S (%1.0n of %1.0n)', [FCaption, Int(iFile), Int(FFileCount)]);
  lblFrom.Caption        := strSource;
  lblTo.Caption          := strDest;
  lblFilename.Caption    := strFileName;
  pbrFile.Position       := 0;
  lblBytesCopied.Caption := '';
  FStartTick             := GetTickCount;
  FLastTick              := FStartTick;
  Application.ProcessMessages;
  CheckForCancel;
End;

(**

  This method updates the file progress bar and the overall progress bar.

  @precon  None.
  @postcon Both the file and the overall progress bars are updated.

  @param   iCopiedSize as an Int64
  @param   iTotalSize  as an Int64

**)
Procedure TfrmCopyProgress.Progress(iCopiedSize, iTotalSize: Int64);

Var
  dblBytesPerSec: Double;
  iTick         : Int64;

Begin
  iTick := GetTickCount;
  If (iTick - FLastTick > 25) Or (iCopiedSize = iTotalSize) Then
    Begin
      If iTick <> FStartTick Then
        dblBytesPerSec := Int(iCopiedSize) * 1000.0 / dblFactor / (iTick - FStartTick)
      Else
        dblBytesPerSec       := 0;
      If iTotalSize = 0 Then
        Inc(iTotalSize);
      lblBytesCopied.Caption :=
        Format('Copied %1.0n kbytes in %1.0n kbytes (%1.1n kbytes/sec) [%1.1f%%]',
        [Int(iCopiedSize / dblFactor), Int(iTotalSize / dblFactor), dblBytesPerSec,
        Int(iCopiedSize) / Int(iTotalSize) * 100.0]);
      pbrFile.Position    := Trunc(Int(iCopiedSize) / Int(iTotalSize) * Int(pbrFile.Max));
      // Workaround for Windows 7 animatation not updating the bar correctly.
      pbrFile.Position := pbrFile.Position - 1;
      pbrFile.Position := pbrFile.Position + 1;
      UpdateOverallProgress(FSize + iCopiedSize);
      //If FTotalSize = 0 Then
      //  Inc(FTotalSize);
      //pbrOverall.Position := Trunc(Int(pbrOverall.Max) * Int(FSize + iCopiedSize) / Int(FTotalSize));
      //// Workaround for Windows 7 animatation not updating the bar correctly.
      //pbrOverall.Position := pbrOverall.Position - 1;
      //pbrOverall.Position := pbrOverall.Position + 1;
      //lblBytesOverallCopied.Caption := Format('Copied %1.0n kbytes in %1.0n kbytes [%1.1f%%]',
      //  [Int(FSize + iCopiedSize) / dblFactor, Int(FTotalSize) / dblFactor,
      //  Int(FSize) / Int(FTotalSize) * 100.0]);
      //DoUpdateProgress(pbrOverall.Position, pbrOverall.Max);
      //Application.ProcessMessages;
      FLastTick := iTick;
    End;
End;

(**

  This method updates the overall progress bar and label.

  @precon  None.
  @postcon The overall progress bar is updated.

  @param   iSize as an Int64

**)
Procedure TfrmCopyProgress.ProgressOverall(iSize: Int64);

Begin
  FSize := iSize;
  UpdateOverallProgress(iSize);
End;

(**

  This method updates the overall progress bar and label based on the size parameter.

  @precon  None.
  @postcon The overall progress bar and label are updated.

  @param   iSize as an Int64

**)
Procedure TfrmCopyProgress.UpdateOverallProgress(iSize: Int64);

Begin
  If FTotalSize = 0 Then
    Inc(FTotalSize);
  pbrOverall.Position := Trunc(Int(pbrOverall.Max) * Int(iSize) / Int(FTotalSize));
  // Workaround for Windows 7 animatation not updating the bar correctly.
  pbrOverall.Position := pbrOverall.Position - 1;
  pbrOverall.Position := pbrOverall.Position + 1;
  lblBytesOverallCopied.Caption := Format('Copied %1.0n kbytes in %1.0n kbytes [%1.1f%%]',
    [Int(iSize) / dblFactor,
    Int(FTotalSize) / dblFactor, Int(iSize) / Int(FTotalSize) * 100.0]);
  UpdateRemainingTime(Int(iSize) / Int(FTotalSize));
  DoUpdateProgress(pbrOverall.Position, pbrOverall.Max);
  Application.ProcessMessages;
  CheckForCancel;
End;

(**

  This method updates the remaining time label on the dialogue based on the start time
  for copying and the current time that has elapsed.

  @precon  None.
  @postcon The remaining time labell is pudated with the estimated time to complete the
           copying operations.

  @param   dblProgress as a Double

**)
Procedure TfrmCopyProgress.UpdateRemainingTime(dblProgress : Double);

Var
  dblElapsed : TDateTime;
  iHours, iMinutes, iSeconds, iMSec : Word;

Begin
  If dblProgress > 0 Then
    Begin
      dblElapsed := Now() - FStartTime;
      DecodeTime((1 - dblProgress) * dblElapsed / dblProgress, iHours, iMinutes,
        iSeconds, iMSec);
      If iHours > 0 Then
        lblRemainingTime.Caption := Format('Time remaining %d hrs and %d mins...',
          [iHours, iMinutes])
      Else If iMinutes > 0 Then
        lblRemainingTime.Caption := Format('Time remaining %d mins and %d secs...',
          [iMinutes, iSeconds])
      Else
        lblRemainingTime.Caption := Format('Time remaining %d secs...', [iSeconds]);
    End Else
      lblRemainingTime.Caption := 'Please wait, calculating remaining time...';
End;

End.
