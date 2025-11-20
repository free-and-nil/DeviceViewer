//
//  Original author: Olaf Hess
//  This work is published from: Germany.
//
//  To the extent possible under law, Olaf Hess has waived all copyright and
//  related or neighboring rights to this source code:
//  http://creativecommons.org/publicdomain/zero/1.0/
//
//  Unless expressly stated otherwise, the person who associated a work with
//  this deed makes no warranties about the work, and disclaims liability for
//  all uses of the work, to the fullest extent permitted by applicable law.
//

{$I ..\switches.inc}

{$IFDEF FPC}
	{$MODE DELPHI}
{$ENDIF}

unit MsgBoxU;

interface

uses Windows;

function MsgBox (const sText: String;
				 const iFlags: Integer = mb_OK) : Integer; overload;
function MsgBox (const sText, sCaption: String;
				 const iFlags: Integer = mb_OK) : Integer; overload;
function MsgBox (const sText: String; const Args: array of const;
				 const iFlags: Integer = mb_OK) : Integer; overload;
function MsgBox (const sText: String; const Args: array of const;
				 const sCaption: String;
                 const iFlags: Integer = mb_OK) : Integer; overload;
function MsgBox (const hWindow: HWnd; const sText: String;
				 const iFlags: Integer = mb_OK) : Integer; overload;
function MsgBox (const hWindow: HWnd; const sText, sCaption: String;
				 const iFlags: Integer = mb_OK) : Integer; overload;
function MsgBox (const hWindow: HWnd;
				 const sText: String; const Args: array of const;
				 const iFlags: Integer = mb_OK) : Integer; overload;
function MsgBox (const hWindow: HWnd;
				 const sText: String; const Args: array of const;
				 const sCaption: String;
                 const iFlags: Integer = mb_OK) : Integer; overload;
function MsgBoxTimeOut (const sText: String; const iTimeOutSec: Integer;
				 	    const iFlags: Integer) : Integer; overload;
function MsgBoxTimeOut (const sText, sCaption: String; const iTimeOutSec: Integer;
				 		const iFlags: Integer) : Integer; overload;
function MsgBoxTimeOut (const sText: String; const Args: array of const;
						const iTimeOutSec: Integer;
				 		const iFlags: Integer) : Integer; overload;
function MsgBoxTimeOut (const sText: String; const Args: array of const;
						const sCaption: String; const iTimeOutSec: Integer;
				 		const iFlags: Integer) : Integer; overload;
function MsgBoxTimeOut (const hWindow: HWnd; const sText: String;
						const iTimeOutSec: Integer;
				 	    const iFlags: Integer) : Integer; overload;
function MsgBoxTimeOut (const hWindow: HWnd; const sText, sCaption: String;
						const iTimeOutSec: Integer;
				 		const iFlags: Integer) : Integer; overload;
function MsgBoxTimeOut (const hWindow: HWnd;
						const sText: String; const Args: array of const;
						const iTimeOutSec: Integer;
				 		const iFlags: Integer) : Integer; overload;
function MsgBoxTimeOut (const hWindow: HWnd;
						const sText: String; const Args: array of const;
						const sCaption: String; const iTimeOutSec: Integer;
				 		const iFlags: Integer) : Integer; overload;

implementation

uses SysUtils,
{$IFNDEF NO_VCL}
	 Forms,
{$ENDIF}
     WinXP_ImportU;

{$IFDEF DEBUG}
ResourceString
	cWindowNotVisibleMsg =
    	'"MessageBox": The window with the handle $%p is not visible!';
{$ENDIF}

(* ---- *)

{$IFDEF DEBUG}
function IsWindowVisibleCheck (const hWindow: THandle) : Boolean;
begin
	if (IsWindowVisible (hWindow)) then
    	Result := true
    else
    begin
    	Result := false;
    	MessageBox (GetDesktopWindow,
        			PChar (Format (cWindowNotVisibleMsg, [{%H-}Pointer (hWindow)])),
                    PChar (ExtractFileName (ParamStr (0))), mb_IconStop);
		exit;
    end; { else }
end; { IsWindowVisibleCheck }
{$ENDIF}

(* ---- *)

function MsgBox (const sText: String; const iFlags: Integer = mb_OK) : Integer;
begin
{$IFNDEF NO_VCL}
	if (Screen <> NIL) and (Screen.ActiveForm <> NIL) and
       (Screen.ActiveForm.Caption <> '') then
    	Result := MsgBox (sText, Screen.ActiveForm.Caption, iFlags)
    else if (Application <> NIL) and (Application.Title <> '') then
    	Result := MsgBox (sText, Application.Title, iFlags)
    else
{$ENDIF}
	Result := MsgBox (sText, ExtractFileName (ParamStr (0)), iFlags);
end; { MsgBox }

(* ---- *)

function MsgBox (const sText, sCaption: String;
				 const iFlags: Integer = mb_OK) : Integer;

(**
var
	hWindow : THandle;
**)

begin
{$IFNDEF NO_VCL}
    if (Application <> NIL) then
    begin
    	Application.ProcessMessages;
    	Result := Application.MessageBox (PChar (sText), PChar (sCaption),
                                          iFlags);
    end { if }
    else
(**
	if (Screen <> NIL) and (Screen.ActiveForm <> NIL) and
       (Screen.ActiveForm.Handle <> 0) then
        hWindow := Screen.ActiveForm.Handle
    else if (Application <> NIL) and (Application.MainForm <> NIL) and
            (Application.MainForm.Handle <> 0) then
    	hWindow := Application.MainForm.Handle
    else hWindow := 0;

    if (hWindow = 0) or (IsWindow (hWindow) = false) then
    	hWindow := GetDesktopWindow;
**)
{$ENDIF}
	Result := MsgBox (GetDesktopWindow, sText, sCaption, iFlags);
end; { MsgBox }

(* ---- *)

function MsgBox (const sText: String; const Args: array of const;
				 const iFlags: Integer = mb_OK) : Integer;
begin
	Result := MsgBox (Format (sText, Args), iFlags);
end; { MsgBox }

(* ---- *)

function MsgBox (const sText: String; const Args: array of const;
				 const sCaption: String;
                 const iFlags: Integer = mb_OK) : Integer; overload;begin
	Result := MsgBox (Format (sText, Args), sCaption, iFlags);
end; { MsgBox }

(* ---- *)

function MsgBox (const hWindow: HWnd; const sText: String;
				 const iFlags: Integer = mb_OK) : Integer;
begin
	Result := MsgBox (hWindow, sText, ExtractFileName (ParamStr (0)), iFlags);
end; { MsgBox }

(* ---- *)

function MsgBox (const hWindow: HWnd; const sText, sCaption: String;
				 const iFlags: Integer = mb_OK) : Integer;
begin
{$IFDEF DEBUG}
	if not (IsWindowVisibleCheck (hWindow)) then
    begin
    	Result := (-1);
		exit;
    end; { if }
{$ENDIF}

	Result := MessageBox (hWindow, PChar (sText), PChar (sCaption), iFlags);
end; { MsgBox }

(* ---- *)

function MsgBox (const hWindow: HWnd;
				 const sText: String; const Args: array of const;
				 const iFlags: Integer = mb_OK) : Integer;
begin
	Result := MsgBox (hWindow, Format (sText, Args), iFlags);
end; { MsgBox }

(* ---- *)

function MsgBox (const hWindow: HWnd;
				 const sText: String; const Args: array of const;
				 const sCaption: String;
                 const iFlags: Integer = mb_OK) : Integer;
begin
	Result := MsgBox (hWindow, Format (sText, Args), sCaption, iFlags);
end; { MsgBox }

(* ---- *)

function MsgBoxTimeOut (const sText: String; const iTimeOutSec: Integer;
				 	    const iFlags: Integer) : Integer;
begin
{$IFNDEF NO_VCL}
	if (Screen <> NIL) and (Screen.ActiveForm <> NIL) and
       (Screen.ActiveForm.Caption <> '') then
    	Result := MsgBoxTimeOut (sText, Screen.ActiveForm.Caption, iTimeOutSec,
        						 iFlags)
    else if (Application <> NIL) and (Application.Title <> '') then
    	Result := MsgBoxTimeOut (sText, Application.Title, iTimeOutSec, iFlags)
    else
{$ENDIF}
	Result := MsgBoxTimeOut (sText, ExtractFileName (ParamStr (0)), iTimeOutSec,
    						 iFlags);
end; { MsgBoxTimeOut }

(* ---- *)

function MsgBoxTimeOut (const sText, sCaption: String; const iTimeOutSec: Integer;
				 		const iFlags: Integer) : Integer;

var
	hWindow : HWnd;

begin
{$IFDEF NO_VCL}
	hWindow := GetDesktopWindow;
{$ELSE}
    if (Screen <> NIL) and (Screen.ActiveForm <> NIL) and
       (Screen.ActiveForm.Handle > 0) then
    	hWindow := Screen.ActiveForm.Handle
    else hWindow := GetDesktopWindow;
{$ENDIF}

	Result := MsgBoxTimeOut (hWindow, sText, sCaption, iTimeOutSec, iFlags);
end; { MsgBoxTimeOut }

(* ---- *)

function MsgBoxTimeOut (const sText: String; const Args: array of const;
						const iTimeOutSec: Integer;
				 		const iFlags: Integer) : Integer;
begin
	Result := MsgBoxTimeOut (sText, Args, ExtractFileName (ParamStr (0)),
    						 iTimeOutSec, iFlags);
end; { MsgBoxTimeOut }

(* ---- *)

function MsgBoxTimeOut (const sText: String; const Args: array of const;
						const sCaption: String; const iTimeOutSec: Integer;
				 		const iFlags: Integer) : Integer;
begin
	Result := MsgBoxTimeOut (Format (sText, Args), sCaption, iTimeOutSec, iFlags)
end; { MsgBoxTimeOut }

(* ---- *)

function MsgBoxTimeOut (const hWindow: HWnd; const sText: String;
						const iTimeOutSec: Integer;
				 	    const iFlags: Integer) : Integer;
begin
	Result := MsgBoxTimeOut (hWindow, sText, ExtractFileName (ParamStr (0)),
    						 iTimeOutSec, iFlags);
end; { MsgBoxTimeOut }

(* ---- *)

function MsgBoxTimeOut (const hWindow: HWnd; const sText, sCaption: String;
						const iTimeOutSec: Integer;
				 		const iFlags: Integer) : Integer;
begin
{$IFDEF DEBUG}
	if not (IsWindowVisibleCheck (hWindow)) then
    begin
    	Result := (-1);
		exit;
    end; { if }
{$ENDIF}

	Result := MessageBoxTimeOut (hWindow, PChar (sText), PChar (sCaption),
    							 iFlags, 0, iTimeOutSec * 1000);
end; { MsgBoxTimeOut }

(* ---- *)

function MsgBoxTimeOut (const hWindow: HWnd;
						const sText: String; const Args: array of const;
						const iTimeOutSec: Integer;
				 		const iFlags: Integer) : Integer;
begin
	Result := MsgBoxTimeOut (hWindow, Format (sText, Args),
    						 ExtractFileName (ParamStr (0)), iTimeOutSec, iFlags)
end; { MsgBoxTimeOut }

(* ---- *)

function MsgBoxTimeOut (const hWindow: HWnd;
						const sText: String; const Args: array of const;
						const sCaption: String; const iTimeOutSec: Integer;
				 		const iFlags: Integer) : Integer;
begin
	Result := MsgBoxTimeOut (hWindow, Format (sText, Args), sCaption,
    						 iTimeOutSec, iFlags);
end; { MsgBoxTimeOut }

(* ---- *)

end.


