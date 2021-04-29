(**

  This module contains global functions to be used throughout the various versions of
  the folder sync application.

  @Author  David Hoyle
  @Version 1.029
  @Date    29 Apr 2021

**)
Unit ApplicationFunctions;

Interface

Uses
  SyncModule,
  Classes;

Type
  (** This enumerate describe the available command line options switches that can be
      applied to the application. **)
  TCommandLineOption = (
    cloPause,
    cloCheckForUpdates,
    cloHelp,
    cloQuiet,
    clsDeletePermentently,
    clsBatchRecycleFiles,
    clsProceedAutomatically
  );

  (** This is a set of the above command line option switches. **)
  TCommandLineOptions = Set Of TCommandLineOption;

  (** A record to describe the attributes that need to be configured on the command
      line. **)
  TCommandLineOptionsRec = Record
    iCommandLineOptions : TCommandLineOptions;
    iSyncOptions        : TSyncOptions;
    strSourceFldr       : String;
    strDestFldr         : String;
    strFilePatterns     : String;
    strExclusions       : String;
    iMaxFileSize        : Int64;
  End;

  Function UpdateRemainingTime(dblStartTime, dblProgress : Double) : String;
  Procedure ProcessCommandLine(slParams: TStringList; var ComOps : TCommandLineoptionsRec);

Implementation

Uses
  SysUtils;

(**

  This function returns a string representation of the remaining time left to do the
  operation based on the start time and the percentage complete.

  @precon  None.
  @postcon Returns a string representation of the remaining time left to do the
           operation based on the start time and the percentage complete.

  @param   dblStartTime as a Double
  @param   dblProgress  as a Double
  @return  a String

**)
Function UpdateRemainingTime(dblStartTime, dblProgress : Double) : String;

Const
  iRoundPoint : Integer = 5;

Var
  dblElapsed : TDateTime;
  dblRemaining : TDateTime;
  iDays : Integer;
  iHours, iMinutes, iSeconds, iMSec : Word;

Begin
  If dblProgress > 0 Then
    Begin
      dblElapsed := Now() - dblStartTime;
      dblRemaining := (1 - dblProgress) * dblElapsed / dblProgress;
      iDays := Trunc(dblRemaining);
      DecodeTime(dblRemaining, iHours, iMinutes, iSeconds, iMSec);
      If iDays > 0 Then
        Begin
          Result := Format('Remaining %d days and %d hours...   ', [
            iDays,
            iHours
          ])
        End Else
      If iHours > 0 Then
        Begin
          iMinutes := iMinutes + (iRoundPoint - iMinutes Mod iRoundPoint);
          If iMinutes >= 60 Then
            Begin
              Inc(iHours);
              iMinutes := 0;
            End;
          Result := Format('Remaining %d hrs and %d mins...   ', [
            iHours,
            iMinutes
          ])
        End Else
      If iMinutes > 0 Then
        Begin
          iSeconds := iSeconds + (iRoundPoint - iSeconds Mod iRoundPoint);
          If iSeconds >= 60 Then
            Begin
              Inc(iMinutes);
              iSeconds := 0;
            End;
          Result := Format('Remaining %d mins and %d secs...   ', [
            iMinutes,
            iSeconds
          ])
        End Else
        Result := Format('Remaining %d secs...   ', [
          iSeconds + (iRoundPoint - iSeconds Mod iRoundPoint)]);
    End Else
      Result := 'Please wait, calculating remaining time...   ';
End;

(**

  This method parses the size limit command line argument. If an incorrect specification
  an exception is raised.

  @precon  None.
  @postcon Sets the size limit for copying files else raises an exception.

  @param   strOption    as a String
  @param   iMaxFileSize as an Int64 as a reference

**)
Procedure ParseSizeLimit(strOption: String; var iMaxFileSize : Int64);

Const
  strMultipliers = ['k', 'K', 'm', 'M', 'g', 'G', 't', 'T'];

Var
  strValue   : String;
  cM       : Char;
  iSizeLimit : Int64;
  iErrorCode : Integer;

Begin
  strValue := strOption;
  Delete(strValue, 1, 9);
  If Copy(strValue, 1, 1) = '=' Then
    Begin
      Delete(strValue, 1, 1);
      If (Length(strValue) > 0) Then
        Begin
          If (Not CharInSet(strValue[Length(strValue)], ['0'..'0']))
            And (CharInSet(strValue[Length(strValue)], strMultipliers)) Then
            Begin
              cM := strValue[Length(strValue)];
              Delete(strValue, Length(strValue), 1);
              Val(strValue, iSizeLimit, iErrorCode);
              If iErrorCode > 0 Then
                Raise EFldrSyncException.Create('Invalid SizeLimit Number');
              Case cM Of
                'k', 'K': iMaxFileSize := iSizeLimit * 1024;
                'm', 'M': iMaxFileSize := iSizeLimit * 1024 * 1024;
                'g', 'G': iMaxFileSize := iSizeLimit * 1024 * 1024 * 1024;
                't', 'T': iMaxFileSize := iSizeLimit * 1024 * 1024 * 1024 * 1024;
              Else
                iMaxFileSize := iSizeLimit;
              End;
            End Else
              Raise EFldrSyncException.Create('Invalid SizeLimit Multiplier');
        End Else
          Raise EFldrSyncException.Create('Missing SizeLimit Specification');
    End Else
      Raise EFldrSyncException.Create('= expected after SizeLimit');
End;

(**

  This method processes the command line parameters of the console application and sets up
  the applications internal variables based on those settings.

  @precon  None.
  @postcon Processes the command line parameters of the console application and sets up
           the applications internal variables based on those settings.

  @param   slParams as a TStringList
  @param   ComOps   as a TCommandLineoptionsRec as a reference

**)
Procedure ProcessCommandLine(slParams: TStringList; var ComOps : TCommandLineoptionsRec);

Var
  strOption: String;
  i        : Integer;

Begin
  ComOps.iCommandLineOptions := [];
  For i := 0 To slParams.Count - 1 Do
    Begin
      strOption := slParams[i];
      If CharInSet(strOption[1], ['-', '/']) Then
        Begin
          strOption := Copy(strOption, 2, Length(strOption) - 1);
          If CompareText(strOption, 'Quiet') = 0 Then
            Include(ComOps.iCommandLineOptions, cloQuiet)
          Else If strOption = '!' Then
            Include(ComOps.iCommandLineOptions, cloPause)
          Else If strOption = '?' Then
            Include(ComOps.iCommandLineOptions, cloHelp)
          Else If CompareText(strOption, 'Updates') = 0 Then
            Include(ComOps.iCommandLineOptions, cloCheckForUpdates)
          Else If CompareText(strOption, 'PrimaryLeft') = 0 Then
            Include(ComOps.iSyncOptions, soPrimaryLeft)
          Else If CompareText(strOption, 'PrimaryRight') = 0 Then
            Include(ComOps.iSyncOptions, soPrimaryRight)
          Else If CompareText(strOption, 'DeletePermanently') = 0 Then
            Include(ComOps.iCommandLineOptions, clsDeletePermentently)
          Else If CompareText(strOption, 'BatchRecycleFiles') = 0 Then
            Include(ComOps.iCommandLineOptions, clsBatchRecycleFiles)
          Else If CompareText(strOption, 'ProceedAutomatically') = 0 Then
            Include(ComOps.iCommandLineOptions, clsProceedAutomatically)
          Else If CompareText(Copy(strOption, 1, 1), 'E') = 0 Then
            ComOps.strExclusions := StringReplace(Copy(strOption, 2, Length(strOption) - 1), ';',
              #13#10, [rfReplaceAll])
          Else If CompareText(strOption, 'OverwriteReadOnly') = 0 Then
            Include(ComOps.iSyncOptions, soOverwriteReadOnlyFiles)
          Else If CompareText(strOption, 'ConfirmNo') = 0 Then
            Begin
              Include(ComOps.iSyncOptions, soConfirmCopyNo);
              Include(ComOps.iSyncOptions, soConfirmDeleteNo);
              Exclude(ComOps.iSyncOptions, soConfirmCopyYes);
              Exclude(ComOps.iSyncOptions, soConfirmDeleteYes);
            End
          Else If CompareText(strOption, 'ConfirmCopyNo') = 0 Then
            Begin
              Include(ComOps.iSyncOptions, soConfirmCopyNo);
              Exclude(ComOps.iSyncOptions, soConfirmCopyYes);
            End
          Else If CompareText(strOption, 'ConfirmDeleteNo') = 0 Then
            Begin
              Include(ComOps.iSyncOptions, soConfirmDeleteNo);
              Exclude(ComOps.iSyncOptions, soConfirmDeleteYes);
            End
          Else If CompareText(strOption, 'ConfirmYes') = 0 Then
            Begin
              Include(ComOps.iSyncOptions, soConfirmCopyYes);
              Include(ComOps.iSyncOptions, soConfirmDeleteYes);
              Exclude(ComOps.iSyncOptions, soConfirmCopyNo);
              Exclude(ComOps.iSyncOptions, soConfirmDeleteNo);
            End
          Else If CompareText(strOption, 'ConfirmCopyYes') = 0 Then
            Begin
              Include(ComOps.iSyncOptions, soConfirmCopyYes);
              Exclude(ComOps.iSyncOptions, soConfirmCopyNo);
            End
          Else If CompareText(strOption, 'ConfirmDeleteYes') = 0 Then
            Begin
              Include(ComOps.iSyncOptions, soConfirmDeleteYes);
              Exclude(ComOps.iSyncOptions, soConfirmDeleteNo);
            End
          Else If CompareText(strOption, 'NoRecursion') = 0 Then
            Include(ComOps.iSyncOptions, soNoRecursion)
          Else If CompareText(Copy(strOption, 1, 9), 'SizeLimit') = 0 Then
            ParseSizeLimit(strOption, ComOps.iMaxFileSize)
          Else
            Raise EFldrSyncException.CreateFmt('Invalid command line option "%s".',
              [slParams[i]]);
        End Else
        Begin
          If ComOps.strSourceFldr = '' Then
            Begin
              ComOps.strFilePatterns := ExtractFileName(slParams[i]);
              ComOps.strSourceFldr   := ExtractFilePath(slParams[i]);
            End Else
              ComOps.strDestFldr := ExtractFilePath(slParams[i]);
        End;
          End;
  If ComOps.strSourceFldr.Length = 0 Then
    Include(ComOps.iCommandLineOptions, cloHelp);
  If ComOps.strDestFldr.Length = 0 Then
    Include(ComOps.iCommandLineOptions, cloHelp);
End;

End.
