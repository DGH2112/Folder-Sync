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
        Result := Format('Time remaining %d hrs and %d mins...   ', [iHours, iMinutes])
      Else If iMinutes > 0 Then
        Result := Format('Time remaining %d mins and %d secs...   ', [iMinutes, iSeconds])
      Else
        Result := Format('Time remaining %d secs...   ', [iSeconds]);
    End Else
      Result := 'Please wait, calculating remaining time...   ';
End;

End.
