(**
  
  This module contains a custom label which allows the setting of a custom font colour
  while themes are enabled.

  @Version 1.0
  @Author  David Hoyle
  @Date    10 Mar 2013
  
**)
Unit DGHCustomLabel;

Interface

Uses
  System.SysUtils,
  System.Classes,
  Vcl.Controls,
  Vcl.StdCtrls,
  Windows,
  Vcl.Themes,
  Vcl.Styles;

Type
  (** A label component which allows the setting of a custom font colour while themes are
      enabled. **)
  TDGHCustomLabel = Class(TCustomLabel)
  Private
    { Private declarations }
  Protected
    { Protected declarations }
    Procedure DoDrawText(Var Rect: TRect; Flags: Integer); Override;
  Public
    { Public declarations }
  Published
    { Published declarations }
    (** A published property from TCustomLabel. @precon None. @postcon None. **)
    Property Align;
    (** A published property from TCustomLabel. @precon None. @postcon None. **)
    Property Alignment;
    (** A published property from TCustomLabel. @precon None. @postcon None. **)
    Property Anchors;
    (** A published property from TCustomLabel. @precon None. @postcon None. **)
    Property AutoSize;
    (** A published property from TCustomLabel. @precon None. @postcon None. **)
    Property BiDiMode;
    (** A published property from TCustomLabel. @precon None. @postcon None. **)
    Property Caption;
    (** A published property from TCustomLabel. @precon None. @postcon None. **)
    Property Color Nodefault;
    (** A published property from TCustomLabel. @precon None. @postcon None. **)
    Property Constraints;
    (** A published property from TCustomLabel. @precon None. @postcon None. **)
    Property DragCursor;
    (** A published property from TCustomLabel. @precon None. @postcon None. **)
    Property DragKind;
    (** A published property from TCustomLabel. @precon None. @postcon None. **)
    Property DragMode;
    (** A published property from TCustomLabel. @precon None. @postcon None. **)
    Property EllipsisPosition;
    (** A published property from TCustomLabel. @precon None. @postcon None. **)
    Property Enabled;
    (** A published property from TCustomLabel. @precon None. @postcon None. **)
    Property FocusControl;
    (** A published property from TCustomLabel. @precon None. @postcon None. **)
    Property Font;
    (** A published property from TCustomLabel. @precon None. @postcon None. **)
    Property GlowSize; // Windows Vista only
    (** A published property from TCustomLabel. @precon None. @postcon None. **)
    Property ParentBiDiMode;
    (** A published property from TCustomLabel. @precon None. @postcon None. **)
    Property ParentColor;
    (** A published property from TCustomLabel. @precon None. @postcon None. **)
    Property ParentFont;
    (** A published property from TCustomLabel. @precon None. @postcon None. **)
    Property ParentShowHint;
    (** A published property from TCustomLabel. @precon None. @postcon None. **)
    Property PopupMenu;
    (** A published property from TCustomLabel. @precon None. @postcon None. **)
    Property ShowAccelChar;
    (** A published property from TCustomLabel. @precon None. @postcon None. **)
    Property ShowHint;
    (** A published property from TCustomLabel. @precon None. @postcon None. **)
    Property Touch;
    (** A published property from TCustomLabel. @precon None. @postcon None. **)
    Property Transparent;
    (** A published property from TCustomLabel. @precon None. @postcon None. **)
    Property Layout;
    (** A published property from TCustomLabel. @precon None. @postcon None. **)
    Property Visible;
    (** A published property from TCustomLabel. @precon None. @postcon None. **)
    Property WordWrap;
    (** A published property from TCustomLabel. @precon None. @postcon None. **)
    Property OnClick;
    (** A published property from TCustomLabel. @precon None. @postcon None. **)
    Property OnContextPopup;
    (** A published property from TCustomLabel. @precon None. @postcon None. **)
    Property OnDblClick;
    (** A published property from TCustomLabel. @precon None. @postcon None. **)
    Property OnDragDrop;
    (** A published property from TCustomLabel. @precon None. @postcon None. **)
    Property OnDragOver;
    (** A published property from TCustomLabel. @precon None. @postcon None. **)
    Property OnEndDock;
    (** A published property from TCustomLabel. @precon None. @postcon None. **)
    Property OnEndDrag;
    (** A published property from TCustomLabel. @precon None. @postcon None. **)
    Property OnGesture;
    (** A published property from TCustomLabel. @precon None. @postcon None. **)
    Property OnMouseActivate;
    (** A published property from TCustomLabel. @precon None. @postcon None. **)
    Property OnMouseDown;
    (** A published property from TCustomLabel. @precon None. @postcon None. **)
    Property OnMouseMove;
    (** A published property from TCustomLabel. @precon None. @postcon None. **)
    Property OnMouseUp;
    (** A published property from TCustomLabel. @precon None. @postcon None. **)
    Property OnMouseEnter;
    (** A published property from TCustomLabel. @precon None. @postcon None. **)
    Property OnMouseLeave;
    (** A published property from TCustomLabel. @precon None. @postcon None. **)
    Property OnStartDock;
    (** A published property from TCustomLabel. @precon None. @postcon None. **)
    Property OnStartDrag;
  End;

Procedure Register;

Implementation

Uses
 Graphics;

Type
  (** A helper class to help with the drawing of text. **)
  TDGHCustomLabelHelper = Class Helper For TCustomLabel
    Procedure DrawNormalText(DC: HDC; Const Text: UnicodeString; Var TextRect: TRect;
      TextFlags: Cardinal);
  End;

(**

  This method registers the component with the IDE.

  @precon  None.
  @postcon The component is registered with the IDE.

**)
Procedure Register;

Begin
  RegisterComponents('DGH Controls', [TDGHCustomLabel]);
End;

(**

  This is a method of the helper class.

  @precon  None.
  @postcon Calls the components DoDrawNormalText method.

  @param   DC        as a HDC
  @param   Text      as an UnicodeString as a constant
  @param   TextRect  as a TRect as a reference
  @param   TextFlags as a Cardinal

**)
Procedure TDGHCustomLabelHelper.DrawNormalText(DC: HDC; Const Text: UnicodeString;
  Var TextRect: TRect; TextFlags: Cardinal);

Var
  strText : String;
  
Begin
  strText := Text;
  DrawText(DC, strText, Length(strText), TextRect, TextFlags);
  //Self.DoDrawNormalText(DC, Text, TextRect, TextFlags);
End;

(**

  This is an overridden DoDrawText method of the TCustomLabel to allow the settings of
  font colours while themes are enabled.

  @precon  None.
  @postcon Settings of font colours while themes are enabled.

  @param   Rect  as a TRect as a reference
  @param   Flags as an Integer

**)
Procedure TDGHCustomLabel.DoDrawText(Var Rect: TRect; Flags: Integer);

Const
  EllipsisStr                                    = '...';
  Ellipsis: Array [TEllipsisPosition] Of Longint = (0, DT_PATH_ELLIPSIS, DT_END_ELLIPSIS,
    DT_WORD_ELLIPSIS);
    
Var
  Text, DText  : String;
  NewRect      : TRect;
  Height, Delim: Integer;
  
Begin
  Text := GetLabelText;
  If (Flags And DT_CALCRECT <> 0) And ((Text = '') Or ShowAccelChar And (Text[1] = '&')
      And (Length(Text) = 1)) Then
    Text := Text + ' ';

  If Text <> '' Then
    Begin
      If Not ShowAccelChar Then
        Flags     := Flags Or DT_NOPREFIX;
      Flags       := DrawTextBiDiModeFlags(Flags);
      Canvas.Font := Font;
      If (EllipsisPosition <> epNone) And Not AutoSize Then
        Begin
          DText := Text;
          Flags := Flags And Not DT_EXPANDTABS;
          Flags := Flags Or Ellipsis[EllipsisPosition];
          If WordWrap And (EllipsisPosition In [epEndEllipsis, epWordEllipsis]) Then
            Begin
              Repeat
                NewRect := Rect;
                Dec(NewRect.Right, Canvas.TextWidth(EllipsisStr));
                DrawNormalText(Canvas.Handle, DText, NewRect, Flags Or DT_CALCRECT);
                Height := NewRect.Bottom - NewRect.Top;
                If (Height > ClientHeight) And (Height > Canvas.Font.Height) Then
                  Begin
                    Delim := LastDelimiter(' '#9, Text);
                    If Delim = 0 Then
                      Delim := Length(Text);
                    Dec(Delim);
                    If ByteType(Text, Delim) = mbLeadByte Then
                      Dec(Delim);
                    Text  := Copy(Text, 1, Delim);
                    DText := Text + EllipsisStr;
                    If Text = '' Then
                      Break;
                  End
                Else
                  Break;
              Until False;
            End;
          If Text <> '' Then
            Text := DText;
        End;

      If Enabled Or StyleServices.Enabled Then
        DrawNormalText(Canvas.Handle, Text, Rect, Flags)
      Else
        Begin
          OffsetRect(Rect, 1, 1);
          Canvas.Font.Color := clBtnHighlight;
          DrawNormalText(Canvas.Handle, Text, Rect, Flags);
          OffsetRect(Rect, -1, -1);
          Canvas.Font.Color := clBtnShadow;
          DrawNormalText(Canvas.Handle, Text, Rect, Flags);
        End;
    End;
End;

End.
