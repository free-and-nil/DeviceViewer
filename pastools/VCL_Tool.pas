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

unit VCL_Tool{sU};

interface

uses Windows, Forms, StdCtrls, Dialogs, SysUtils
{$IFDEF DELPHI7_UP}
	 , ExtCtrls
{$ENDIF}
	 ;

function ActiveFormNotNil : Boolean;

{$IFNDEF CLR}
function BatchCopy (const sSourcePath: TFileName;
					sTargetPath: TFileName) : Boolean;
{ Dateien mit Wildcards kopieren.
  -> sSourcePath : Ausgangspfad mit Dateiname, Wildcards im Namen erlaubt.
  -> sTargetPath : Zielpfad, nur die Pfadangabe.
  <- Result : TRUE, wenn OK; sonst FALSE. }
{$ENDIF}

procedure CenterOverActiveForm (const Form: TForm);
{ Fenster über aktivem Form zentrieren }

PROCEDURE CenterOverForm (const ParentForm, Form: TForm);
{ Zentriert ein Fenster über einem anderen }

procedure CenterOverMainForm (const Form: TForm);
{ Fenster über Eltern-Form zentrieren }

procedure ChangeCheckBoxState (const CheckBox: TCheckBox;
							   const NewState: TCheckBoxState);
{$IFDEF SUPPORTS_OVERLOAD}
	overload;

procedure ChangeCheckBoxState (const CheckBox: TCheckBox;
							   const bCheck: Boolean); overload;
{$ENDIF}

procedure ComboBoxAddText (const ComboBox: TComboBox; sText: String
{$IFDEF SUPPORTS_OVERLOAD}
						   = ''
{$ENDIF}
						   );

procedure DelphiDelay (const uMSec: UInt);

procedure DisableEdit (const Edit: TEdit);
{$IFDEF DELPHI7_UP}
	overload;

procedure DisableEdit (const Edit: TLabeledEdit); overload;
{$ENDIF}

procedure EnableEdit (const Edit: TEdit; const bEnable: Boolean
{$IFDEF SUPPORTS_OVERLOAD}
					  = true
{$ENDIF}
					 );

{$IFDEF DELPHI7_UP}
					   overload;

procedure EnableEdit (const Edit: TLabeledEdit;
					  const bEnable: Boolean = true); overload;
{$ENDIF}

function GetActiveFormHandle : HWnd;

function LoadText (const sFileName: String; var sText: String;
                   const bShowErrorDlg: Boolean
{$IFDEF SUPPORTS_DEFAULTPARAMS}
						  						= true
{$ENDIF}
                   ; const bRaiseException: Boolean
{$IFDEF SUPPORTS_DEFAULTPARAMS}
						  						= false
{$ENDIF}
                   ) : Boolean;

function MainWndVisible : Boolean;

function MsgDlg (const Msg: String; const AType: TMsgDlgType;
                 const AButtons: TMsgDlgButtons) : Word;

function SaveText (const sFileName: String; const sText: String;
                   const bShowErrorDlg: Boolean
{$IFDEF SUPPORTS_DEFAULTPARAMS}
						  						= true
{$ENDIF}
                   ; const bRaiseException: Boolean
{$IFDEF SUPPORTS_DEFAULTPARAMS}
						  						= false
{$ENDIF}
                   ) : Boolean;

procedure SelectEditText (const Edit: TEdit; const bSetFocus: Boolean
{$IFDEF SUPPORTS_OVERLOAD}
																	  = true
{$ENDIF}
						  );
{$IFDEF DELPHI7_UP}
	overload;

procedure SelectEditText (const Edit: TLabeledEdit;
						  const bSetFocus: Boolean = true); overload;
{$ENDIF}

procedure SetAppTitle (const sAppName: String; const sFullFileName: TFileName);

procedure ShowHorizontalScrollbar (const ListBox: TListBox);

procedure ToggleCheckBox (const CheckBox: TCheckBox);

procedure VCL_Delay (const lDelay: LongInt);

implementation

uses Classes, Messages, Graphics,
{$IFDEF DELPHI_XE3_UP}
	 UITypes,
{$ENDIF}
{$IFNDEF FPC}
	 WinVista_ImportU,
{$ENDIF}
     WinTools;

(* ---- *)

function ActiveFormNotNil : Boolean;
begin
	Result := (Screen <> NIL) and (Screen.ActiveForm <> NIL);
end; { ActiveFormNotNil }

(* ---- *)

{$IFNDEF CLR}
function BatchCopy (const sSourcePath: TFileName;
					sTargetPath: TFileName) : Boolean;
{ Dateien mit Wildcards kopieren.
  -> sSourcePath : Ausgangspfad mit Dateiname, Wildcards im Namen erlaubt.
  -> sTargetPath : Zielpfad, nur die Pfadangabe.
  <- Result : TRUE, wenn OK; sonst FALSE. }

    (* ---- *)

    function CopyFile (const sSource, sTarget: TFileName) : Boolean;
    { Die Datei "sSource" nach "sTarget" kopieren. Gibt TRUE zurück, wenn OK. }

        (* ---- *)

        procedure CopyError;

        const
        	cMsg = 'Error copying from "%s" to "%s"!';

        begin
            MessageBox (GetDesktopWindow,
            			PChar (Format (cMsg, [sSourcePath, sTargetPath])),
            			PChar (ExtractFileName (ParamStr (0))), mb_IconStop);
        end; { CopyError }

        (* ---- *)

    const
        CBuf = 2048;

    type
        PTBuf = ^TBuf;
        TBuf = array [0..CBuf - 1] of byte;

    var
		fSource, fTarget : File;
        uRead, uWritten : uInt;
        pBuf : PTBuf;

    begin { CopyFile }
        Result := false;

        assign (fSource, sSource);
        assign (fTarget, sTarget);

        reset (fSource, 1);

        if (IOResult = 0) then
        begin
            rewrite (fTarget, 1);

            if (IOResult = 0) then
            begin
                new (pBuf);

                repeat
                	Application.ProcessMessages;

                    BlockRead (fSource, pBuf^, SizeOf (TBuf), uRead{%H-});
                    BlockWrite (fTarget, pBuf^, uRead, uWritten{%H-});
                until ({%H-}uRead = 0) or ({%H-}uWritten <> uRead);

                Dispose (pBuf);

                close (fTarget);

                if (uWritten <> uRead) then
                    CopyError
                else Result := true;
            end { if }
			else CopyError;

            close (fSource);
        end { if }
        else CopyError;
    end; { CopyFile }

    (* ---- *)

const
	cDifferentMsg = 'Source and target directory must be different!';
    cNoFilesFoundMsg = 'No files found!';

var
    pSearch : ^TSearchRec;
    psSourcePath : ^TFileName;
    iResult : Integer;
    byFileMode : Byte;

begin { BatchCopy }
    Result := false;

    if (sTargetPath = '') then
    	exit;

	if (sTargetPath [Length (sTargetPath)] <> '\') then
    	sTargetPath := sTargetPath {%H-}+ '\';

    if (ExtractFilePath (sSourcePath) = sTargetPath) then
    begin
        MessageBox (GetDesktopWindow, cDifferentMsg,
        			PChar (ExtractFileName (ParamStr (0))), mb_IconStop);
        exit;
    end; { if }

    new (pSearch);

    if (FindFirst (sSourcePath, faAnyFile, pSearch^) = 0) then
    begin
		new (psSourcePath);

        psSourcePath^ := ExtractFilePath (sSourcePath);

        byFileMode := FileMode;
		FileMode := 0;

{$IFNDEF DELPHI_XE2_UP}
        iResult := 0;
{$ENDIF}

        repeat
            if (pSearch^.Name <> '.') and (pSearch^ .Name <> '..') then
                if (not CopyFile (psSourcePath^ + pSearch^.Name,
                                  sTargetPath + pSearch^.Name)) then
                    Break;

            iResult := FindNext (pSearch^);

            if (iResult <> 0) then { Suche beendet }
                Result := true;
        until (iResult <> 0);

        FileMode := byFileMode;

        Dispose (psSourcePath);
    end { if }
    else MessageBox (GetDesktopWindow, cNoFilesFoundMsg,
    				 PChar (ExtractFileName (ParamStr (0))), mb_IconStop);

    SysUtils.FindClose (pSearch^); { Für Delphi32 }

    Dispose (pSearch);
end; { BatchCopy }
{$ENDIF}

(* ---- *)

procedure CenterOverActiveForm (const Form: TForm);
{ Fenster über aktiver Form zentrieren.
  Sollte nicht aus "OnShow", sondern aus "OnCreate" aufgerufen werden! }

begin
{$IFDEF SUPPORTS_ASSERT}
	Assert (Form <> NIL);
    Assert (IsWindow (Form.Handle));
{$ENDIF}

    if (ActiveFormNotNil) then
        CenterOverForm (Screen.ActiveForm, Form)
    else if (Application <> NIL) and (Application.MainForm <> NIL) then
        CenterOverForm (Application.MainForm, Form)
    else Form.Position := poScreenCenter;
end; { CenterOverActiveForm }

(* ---- *)

PROCEDURE CenterOverForm (const ParentForm, Form: TForm);
{
  Zentriert ein Fenster über einem anderen.

  ->> hParent : Fenster-Handle des Elternfensters.
  ->> hWindows : Fenster-Handle des Fensters, das verschoben werden soll.
}

	(* ---- *)

    procedure GetMonitorInfo (var MonSize: TPoint);

{$IFDEF DELPHI4_UP}
    var
        iLeftBorder, iIndex : Integer;
{$ENDIF}

    begin
{$IFDEF DELPHI4_UP}
    	MonSize.x := 0;

		with Screen do
            for iIndex := 0 to MonitorCount - 1 do
            begin
            	iLeftBorder := MonSize.x;

                inc (MonSize.x, Monitors [iIndex].Width);
                MonSize.y := Monitors [iIndex].Height;

            	if (ParentForm.Left >= iLeftBorder) and
                   (ParentForm.Left < MonSize.x) then
                	exit;
            end; { for }

{$ENDIF}
		{ Nicht gefunden (außerhalb des Monitors) oder Delphi Version vor 4 }
		with Screen do
        begin
			MonSize.X := Width;
    	    MonSize.Y := Height;
        end; { with }
    end; { GetMonitorInfo }

    (* ---- *)

VAR
    Rect : TRect;
    xParent, yParent, xCorner, yCorner : Integer;
    MonSize: TPoint;

BEGIN { CenterOverForm }
	GetMonitorInfo (MonSize{%H-});

    GetWindowRect (ParentForm.Handle, Rect{%H-});

    WITH Rect DO
    BEGIN
        xParent := (Right - Left) DIV 2; { Mitte des Parent-Fensters }
        yParent := (Bottom - Top) DIV 2;
        xCorner := Left; { Position des linken, oberen Kante }
        yCorner := Top;
    END; { with }

    GetWindowRect (Form.Handle, Rect); { Ausmaße des Kindfensters }

    WITH Rect DO { Fensterposition setzen }
    BEGIN
        { Eckpunkte festlegen }
        xCorner := xCorner + xParent - ((Right - Left) DIV 2);
        yCorner := yCorner + yParent - ((Bottom - Top) DIV 2);

        { Fenster soll sichtbar dargestellt werden }
        IF (xCorner < 0) THEN xCorner := 0;
        IF ((xCorner + (Right - Left)) > MonSize.x) THEN
            xCorner := MonSize.x - (Right - Left);

        IF (yCorner < 0) THEN yCorner := 0;
        IF ((yCorner + (Bottom - Top)) > MonSize.y) THEN
            yCorner := MonSize.y - (Bottom - Top);

        { Fensterposition setzen }
        SetWindowPos (Form.Handle, hwnd_Top, xCorner, yCorner, 0, 0,
                      swp_NoSize or swp_NoZOrder);
    END; { with }
END; { CenterOverForm }

(* ---- *)

procedure CenterOverMainForm (const Form: TForm);
{ Fenster über Hauptfenster zentrieren.
  ->> Form : Form, der zentriert werden soll }

begin
    if (Application.MainForm <> NIL) then
        CenterOverForm (Application.MainForm, Form)
    else CenterOverScreen (Form.Handle);
end; { CenterOverMainForm }

(* ---- *)

procedure ChangeCheckBoxState (const CheckBox: TCheckBox;
							   const NewState: TCheckBoxState);

var
	SaveEvent : TNotifyEvent;

begin
	with CheckBox do
    begin
		SaveEvent := OnClick;
        OnClick := NIL;

        State := NewState;

        OnClick := SaveEvent;
    end; { with }
end; { ChangeCheckBoxState }

(* ---- *)

{$IFDEF DELPHI4_UP}
procedure ChangeCheckBoxState (const CheckBox: TCheckBox;
							   const bCheck: Boolean);
begin
	if (bCheck) then
    	ChangeCheckBoxState (CheckBox, cbChecked)
    else ChangeCheckBoxState (CheckBox, cbUnchecked);
end; { ChangeCheckBoxState }
{$ENDIF}

(* ---- *)

procedure ComboBoxAddText (const ComboBox: TComboBox; sText: String
{$IFDEF SUPPORTS_OVERLOAD}
						   = ''
{$ENDIF}
						   );

var
	sCompare : String;
    iIndex : Integer;

begin
	Assert (ComboBox <> NIL);
    Assert (ComboBox is TCustomComboBox);

    if (sText = '') then
    	sText := ComboBox.Text;

    with ComboBox.Items do
    begin
    	if (Count > 0) then
        begin
        	sCompare := LowerCase (sText);

		    for iIndex := Count - 1 downto 0 do
            	if (LowerCase (Strings [iIndex]) = sCompare) then
                	Delete (iIndex);
        end; { if }

        Insert (0, sText);
    end; { with }

    if (ComboBox.Text <> sText) then
	    ComboBox.Text := sText;
end; { ComboBoxAddText }

(* ---- *)

procedure DelphiDelay (const uMSec: UInt);

var
	uStart : UInt;
{$IFDEF MSWINDOWS}
  {$IFDEF DELPHI2007_UP}
    uStart64 : UInt64;
  {$ELSE}
    uStart64 : Int64;
  {$ENDIF}
{$ENDIF}

begin
{$IFDEF MSWINDOWS}
    if (@GetTickCount64 <> NIL) then
    begin
	    uStart64 := GetTickCount64;

	    while ((GetTickCount64 - uMSec) < uStart64) do
		    Application.ProcessMessages;
    end { if }
    else
{$ENDIF}
	begin
        uStart := {%H-}GetTickCount;

        while (({%H-}GetTickCount - uMSec) < uStart) do
            Application.ProcessMessages;
    end; { else }
end; { DelphiDelay }

(* ---- *)

procedure DisableEdit (const Edit: TEdit);
begin
	EnableEdit (Edit, false);
end; { DisableEdit }

(* ---- *)

{$IFDEF DELPHI7_UP}
procedure DisableEdit (const Edit: TLabeledEdit); overload;
begin
	EnableEdit (Edit, false);
end; { DisableEdit }
{$ENDIF}

(* ---- *)

procedure EnableEdit (const Edit: TEdit; const bEnable: Boolean
{$IFDEF SUPPORTS_OVERLOAD}
					  = true
{$ENDIF}
					 );
begin
	with Edit do
    begin
    	Enabled := bEnable;

        if (bEnable) then
        	Color := clWindow
		else Color := clBtnFace;
    end; { with }
end; { EnableEdit }

(* ---- *)

{$IFDEF DELPHI7_UP}
procedure EnableEdit (const Edit: TLabeledEdit; const bEnable: Boolean = true);
begin
	with Edit do
    begin
    	Enabled := bEnable;

        if (bEnable) then
        	Color := clWindow
		else Color := clBtnFace;
    end; { with }
end; { EnableEdit }
{$ENDIF}

(* ---- *)

function GetActiveFormHandle : HWnd;
begin
	if (ActiveFormNotNil) then
    	Result := Screen.ActiveForm.Handle
    else Result := 0;
end; { GetActiveFormHandle }

(* ---- *)

function LoadText (const sFileName: String; var sText: String;
                   const bShowErrorDlg: Boolean
{$IFDEF SUPPORTS_DEFAULTPARAMS}
						  						 = true
{$ENDIF}
                   ; const bRaiseException: Boolean
{$IFDEF SUPPORTS_DEFAULTPARAMS}
						  						= false
{$ENDIF}
                   ) : Boolean;
begin
    Assert (sFileName <> '');

    Result := false;

    with TStringList.Create do
    	try
            try
                LoadFromFile (sFileName);
                Result := true;
                sText := Text;

            except
	            on E:Exception do
                    if (bShowErrorDlg) then
    	                MessageDlg (E.Message, mtError, [mbOK], 0)
                	else
                        if (bRaiseException) then
                            raise;
            end; { try / except }

        finally
            Free;
        end; { try / finally }
end; { LoadText }

(* ---- *)

function MainWndVisible : Boolean;
begin
	Result := IsWindowVisible (Application.MainForm.Handle);
end; { MainWndVisible }

(* ---- *)

function MsgDlg (const Msg: String; const AType: TMsgDlgType;
				 const AButtons: TMsgDlgButtons) : Word;
{ Dialog über Elternfenster anzeigen }

var
    Rect : TRect;
    hWindow : HWnd;

begin
    if (ActiveFormNotNil) then
	begin
		hWindow := Screen.ActiveForm.Handle;

		if (IsWindow (hWindow)) and
		   (Application.MainForm.WindowState <> wsMaximized) then
		begin
			GetWindowRect (hWindow, Rect{%H-});

			with Rect do
				if (Left < ((GetSystemMetrics (sm_CXMaximized) div 4) * 3)) and
				   (Top < ((GetSystemMetrics (sm_CYMaximized) div 4) * 3)) then
				begin
					Result := MessageDlgPos (Msg, AType, AButtons, 0,
											 Left + ((Right - Left) div 4),
											 Top + ((Bottom - Top) div 4));
					exit;
				end { if }
		end; { if }
	end; { if }

	Result := MessageDlg (Msg, AType, AButtons, 0);
end; { MsgDlg }

(* ---- *)

function SaveText (const sFileName: String; const sText: String;
                   const bShowErrorDlg: Boolean
{$IFDEF SUPPORTS_DEFAULTPARAMS}
						  						= true
{$ENDIF}
                   ; const bRaiseException: Boolean
{$IFDEF SUPPORTS_DEFAULTPARAMS}
						  						= false
{$ENDIF}
                   ) : Boolean;
begin
    Assert (sFileName <> '');

    Result := false;

    with TStringList.Create do
    	try
            try
            	Text := sText;
                SaveToFile (sFileName);
                Result := true;

            except
              	on E:Exception do
                	if (bShowErrorDlg) then
	                    MessageDlg (E.Message, mtError, [mbOK], 0)
                    else
                        if (bRaiseException) then
                            raise;
            end; { try / except }

        finally
            Free;
        end; { try / finally }
end; { SaveText }

(* ---- *)

procedure SelectEditText (const Edit: TEdit; const bSetFocus: Boolean
{$IFDEF SUPPORTS_OVERLOAD}
																	  = true
{$ENDIF}
						  );
begin
    with Edit do
    begin
    	if (bSetFocus) then
        	SetFocus;

        SelStart := 0;
        SelLength := Length (Text);
    end; { with }
end; { SelectEditText }

(* ---- *)

{$IFDEF DELPHI7_UP}
procedure SelectEditText (const Edit: TLabeledEdit;
						  const bSetFocus: Boolean = true);
begin
    with Edit do
    begin
    	if (bSetFocus) then
        	SetFocus;

        SelStart := 0;
        SelLength := Length (Text);
    end; { with }
end; { SelectEditText }
{$ENDIF}

(* ---- *)

procedure SetAppTitle (const sAppName: String; const sFullFileName: TFileName);
{ Den Icon-Titel der Anwendung setzen.
  ->> sAppName : Name der Anwendung.
  ->> sFullFileName : Dateiname, der angezeigt werden soll.
                      Wenn der Wert = '' ist, wird nur der Name der Anwendung
                      angezeigt. }

var
    sFileName : TFileName;

begin
    if (Length (sFullFileName) = 0) then
    begin
        Application.Title := sAppName;
        exit;
    end; { if }

    sFileName := ExtractFileName (sFullFileName);

    if (Length (sFileName) > 0) then
        Application.Title := sAppName + ' - ' + sFileName
    else Application.Title := sAppName;
end; { SetAppTitle }

(* ---- *)

procedure ShowHorizontalScrollbar (const ListBox: TListBox);

var
	i, len, iWidth : Integer;

begin
	with ListBox do
	begin
		iWidth := 0;

		for i := 0 to items.count -1 do
		begin
			len := canvas.textwidth (items [i]);

			if len > iWidth then
				iWidth := len;
		end;

		//set listbox's horizontal scrollbar width
		Perform (LB_SETHORIZONTALEXTENT, iWidth + 100, 0);
	end;
end; { ShowHorizontalScrollbar }

(* ---- *)

procedure ToggleCheckBox (const CheckBox: TCheckBox);
begin
{$IFDEF DELPHI4_UP}
	ChangeCheckBoxState (CheckBox, not CheckBox.Checked);
{$ELSE}
	if (CheckBox.Checked) then
    	ChangeCheckBoxState (CheckBox, cbUnchecked)
    else ChangeCheckBoxState (CheckBox, cbChecked);
{$ENDIF}
end; { ToggleCheckBox }

(* ---- *)

procedure VCL_Delay (const lDelay: LongInt);
{ Anwendung für "lDelay" Millisekunden anhalten.
  ->> lDelay : Anzahl Millisekunden }

begin
    DelphiDelay (lDelay);
end; { VCL_Delay }

(* ---- *)

end.
