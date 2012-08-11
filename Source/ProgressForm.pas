(**

  This module represents a form for displaying progress.

  @Version 1.0
  @Date    11 Aug 2012
  @Author  David Hoyle

**)
Unit ProgressForm;

Interface

Uses
  Windows,
  Messages,
  SysUtils,
  Classes,
  Graphics,
  Controls,
  Forms,
  Dialogs,
  ExtCtrls,
  StdCtrls,
  DGHEllipsisLabel,
  ComCtrls,
  Buttons,
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
  Private
    { Private declarations }
    FSections      : Array Of TSectionRecord;
    FAbort         : Boolean;
    FUpdateProgress: TUpdateProgress;
    Procedure CheckForCancel;
    Procedure DoUpdateProgress(iPosition, iMaxPosition: Integer);
  Public
    { Public declarations }
    //Procedure Progress(strMessage: String; iCount: Integer; strFileName: String);
    //  Overload;
    Procedure RegisterSections(iSections: Integer); Virtual;
    Procedure InitialiseSection(iSection, iMin, iMax: Integer);
    Procedure Progress(iSection, iPosition: Integer; strMsg, strFileName: String);
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
{ TfrmProgress }

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

  @param   iPosition    as an Integer
  @param   iMaxPosition as an Integer

**)
Procedure TfrmProgress.DoUpdateProgress(iPosition, iMaxPosition: Integer);

Begin
  If Assigned(FUpdateProgress) Then
    FUpdateProgress(iPosition, iMaxPosition);
End;

(**

  This method sets the min and max values for the specified section in the progress.

  @note RegisterSections should be called before using this method.

  @precon  iSection must be a valid index into the section array.
  @postcon The min and max values for the specified section in the progress are set.

  @param   iSection as an Integer
  @param   iMin     as an Integer
  @param   iMax     as an Integer

**)
Procedure TfrmProgress.InitialiseSection(iSection, iMin, iMax: Integer);

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

  @param   iSection    as an Integer
  @param   iPosition   as an Integer
  @param   strMsg      as a String
  @param   strFileName as a String

**)
Procedure TfrmProgress.Progress(iSection, iPosition: Integer;
  strMsg, strFileName: String);

Var
  dblSectionWidth: Double;
  dblSectionPos  : Double;
  dblPosition    : Double;

Begin
  SetFocus;
  dblSectionWidth := pbrProgress.Max / Int(Succ(High(FSections)));
  dblSectionPos   := dblSectionWidth * iPosition /
    Int(FSections[iSection].FMax - FSections[iSection].FMin);
  dblPosition := pbrProgress.Max * (iSection / Succ(High(FSections))) + dblSectionPos;
  pbrProgress.Position := Trunc(dblPosition);
  DoUpdateProgress(pbrProgress.Position, pbrProgress.Max);
  lblMessage.Caption  := strMsg;
  lblFileName.Caption := strFileName;
  Application.ProcessMessages;
  CheckForCancel;
End;

(**

  This method registers the number of sections in the progres dialogue, i.e.
  folder pairs in the search.

  @precon  None.
  @postcon Registers the number of sections in the progres dialogue, i.e.
           folder pairs in the search.

  @param   iSections as an Integer

**)
Procedure TfrmProgress.RegisterSections(iSections: Integer);

Begin
  SetLength(FSections, iSections);
  pbrProgress.Position := 0;
  pbrProgress.Max      := 1000000;
  FAbort               := False;
End;

(**

  This is an on click event handler for the Cancel button.

  @precon  None.
  @postcon Sets the cancellation of the processing.

  @param   Sender as a TObject

**)
Procedure TfrmProgress.btnCancelClick(Sender: TObject);

Begin
  FAbort  := True;
  Caption := Caption + ' (Cancel Pending)';
End;

End.
