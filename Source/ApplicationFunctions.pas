(**

  This module contains global functions to be used througthout the various versions of
  the folder sync application.

  @Author  David Hoyle
  @Version 1.0
  @Date    28 Mar 2015

**)
Unit ApplicationFunctions;

Interface

  Function UpdateRemainingTime(dblStartTime, dblProgress : Double) : String;

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
  iHours, iMinutes, iSeconds, iMSec : Word;

Begin
  If dblProgress > 0 Then
    Begin
      dblElapsed := Now() - dblStartTime;
      DecodeTime((1 - dblProgress) * dblElapsed / dblProgress, iHours, iMinutes,
        iSeconds, iMSec);
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

End.
