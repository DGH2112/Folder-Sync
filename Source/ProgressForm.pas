(**

  This module represents a form for displaying progress.

  @Version 1.0
  @Date    01 Jan 2019
  @Author  David Hoyle

**)
Unit ProgressForm;

Interface

Uses
  System.SysUtils,
  System.Classes,
  VCL.Graphics,
  VCL.Controls,
  VCL.Forms,
  VCL.Dialogs,
  VCL.ExtCtrls,
  VCL.StdCtrls,
  VCL.ComCtrls,
  VCL.Buttons,
  WinAPI.Windows,
  WinAPI.Messages,
  SyncModule;

Type
  (** A class to represents a form for displaying progress. **)
  TfrmProgress = Class(TForm)
    pnlInfo: TPanel;
    pbrProgress: TProgressBar;
    btnCancel: TBitBtn;
    lblMessage: TLabel;
    lblFileName: TLabel;
    Procedure btnCancelClick(Sender: TObject);
  Strict Private
    Const
      (** A constant to define the period of time in milliseconds between updates. **)
      iUpdateIntervalInMSecs = 25;
  Strict Private
    FSections      : Array Of TSectionRecord;
    FAbort         : Boolean;
    FUpdateProgress: TUpdateProgress;
    FLastUpdate    : Int64;
  Strict Protected
    Procedure CheckForCancel;
    Procedure DoUpdateProgress(Const iPosition, iMaxPosition: Integer);
  Public
    //Procedure Progress(strMessage: String; iCount: Integer; strFileName: String);
    //  Overload;
    Procedure RegisterSections(Const iSections: Integer); Virtual;
    Procedure InitialiseSection(Const iSection, iMin, iMax: Integer);
    Procedure Progress(Const iSection, iPosition: Integer; Const strMsg, strFileName: String);
    (**
      This property defines a call back event handler for getting the current progress
      position from the dialogue.
      @precon  None.
      @postcon Provides progress information.
      @return  a TUpdateProgress
    **)
    Property OnUpdateProgress: TUpdateProgress Read FUpdateProgress Write FUpdateProgress;
  End;

  (** A event method for feeding back progress messages.
  TProgressMsgProc = Procedure(strMessage: String; iCount: Integer; strFile: String)
    Of Object;
  (** A event method for feeding back progress position.
  TProgressPosProc = Procedure(iPosition, iIndex: Integer) Of Object; **)

Implementation

{$R *.DFM}

Uses
  System.UITypes;

(**

  This is an on click event handler for the Cancel button.

  @precon  None.
  @postcon Sets the cancellation of the processing.

  @param   Sender as a TObject

**)
Procedure TfrmProgress.btnCancelClick(Sender: TObject);

ResourceString
  strCancelPending = ' (Cancel Pending)';

Begin
  FAbort  := True;
  Caption := Caption + strCancelPending;
End;

(**

  This method checks for the cancellation of the process and confirms that is
  what is required.

  @precon  None.
  @postcon Checks for the cancellation of the process and confirms that is
           what is required.

**)
Procedure TfrmProgress.CheckForCancel;

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

  This method calls the Update Progress event handler if it is assigned.

  @precon  None.
  @postcon Calls the Update Progress event handler if it is assigned.

  @param   iPosition    as an Integer as a constant
  @param   iMaxPosition as an Integer as a constant

**)
Procedure TfrmProgress.DoUpdateProgress(Const iPosition, iMaxPosition: Integer);

Begin
  If Assigned(FUpdateProgress) Then
    FUpdateProgress(iPosition, iMaxPosition);
End;

(**

  This method sets the min and max values for the specified section in the progress.

  @precon  iSection must be a valid index into the section array.
  @postcon The min and max values for the specified section in the progress are set.

  @note    RegisterSections should be called before using this method.

  @param   iSection as an Integer as a constant
  @param   iMin     as an Integer as a constant
  @param   iMax     as an Integer as a constant

**)
Procedure TfrmProgress.InitialiseSection(Const iSection, iMin, iMax: Integer);

Begin
  CheckForCancel;
  FSections[iSection].FMin := iMin;
  FSections[iSection].FMax := iMax;
  Show;
  Progress(iSection, iMin, '', '');
  Application.ProcessMessages;
End;

(**

  This method updates the position of the progress bar.

  @precon  Position must be between 0 and Max inclusive.
  @postcon Updates the position of the progress bar.

  @param   iSection    as an Integer as a constant
  @param   iPosition   as an Integer as a constant
  @param   strMsg      as a String as a constant
  @param   strFileName as a String as a constant

**)
Procedure TfrmProgress.Progress(Const iSection, iPosition: Integer;
  Const strMsg, strFileName: String);

Var
  dblSectionWidth: Double;
  dblSectionPos  : Double;
  dblPosition    : Double;
  iDenominator   : Integer;
  iCurrentTime   : Int64;

Begin
  iCurrentTime := GetTickCount64;
  If FLastUpdate + iUpdateIntervalInMSecs <= iCurrentTime Then
    Begin
      SetFocus;
      dblSectionWidth := pbrProgress.Max / Int(Succ(High(FSections)));
      iDenominator := FSections[iSection].FMax - FSections[iSection].FMin;
      If iDenominator <> 0 Then
        dblSectionPos   := dblSectionWidth * iPosition / Int(iDenominator)
      Else
        dblSectionPos := 0;
      dblPosition := pbrProgress.Max * (iSection / Succ(High(FSections))) + dblSectionPos;
      pbrProgress.Position := Trunc(dblPosition);
      // Workaround for windows 7 bug.
      pbrProgress.Position := pbrProgress.Position - 1;
      pbrProgress.Position := pbrProgress.Position + 1;
      DoUpdateProgress(pbrProgress.Position, pbrProgress.Max);
      lblMessage.Caption  := strMsg;
      lblFileName.Caption := strFileName;
      Application.ProcessMessages;
      CheckForCancel;
      FLastUpdate := iCurrentTime;
    End; 
End;

(**

  This method registers the number of sections in the progres dialogue, i.e. folder pairs in the search.

  @precon  None.
  @postcon Registers the number of sections in the progres dialogue, i.e. folder pairs in the search.

  @param   iSections as an Integer as a constant

**)
Procedure TfrmProgress.RegisterSections(Const iSections: Integer);

Const
  iMaxProgressUnits = 1000000;

Begin
  SetLength(FSections, iSections);
  pbrProgress.Position := 0;
  pbrProgress.Max      := iMaxProgressUnits;
  FAbort               := False;
  FLastUpdate := GetTickCount64;
End;

End.
