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
{$ELSE}
  {$IFNDEF UNICODE}
    {$RANGECHECKS OFF}  // Prevent compiler error
  {$ENDIF}
{$ENDIF}

{$WARN UNIT_PLATFORM OFF}
{$WARN SYMBOL_PLATFORM OFF}

// Das Programm muss ein Manifest enthalten, in dem die
// Microsoft.Windows.Common-Controls v6 oder höher angegeben sind

unit TaskDlgU {$IFDEF FPC} {$IFNDEF WINDOWS} platform {$ENDIF} {$ENDIF} ;

interface

uses Windows, SysUtils,
	 TaskDlgApiU, MethodCallBackStubU, BaseTypesU;

const
	tdiNone = 0;
	tdiWarning = 1;
	tdiError = 2;
	tdiInformation = 3;
	tdiShield = 4;

    TD_NO_ICON = 0;
    idTimedOut = 32000;  // Task Dialog has timed out

type
{$IFNDEF DELPHI2007_UP}
	UINT_PTR = Cardinal;
{$ENDIF}

    TaTaskDlgButtons = array of TTaskDialogButton;

	TTaskDlgButtons = class
	  private
		aTaskDlgButtons : TaTaskDlgButtons;
        aButtonText : {$IFDEF UNICODE} TaString {$ELSE} TaWideString {$ENDIF};
	    FDefaultBtn : Integer;

        function GetCount : Integer;

	  public
		procedure AddButton (const iID: Integer; const sButtonText: String;
							 const bDefault: Boolean);
		function GetButtons : PTaskDialogButton;

        property Count : Integer read GetCount;
		property DefaultBtn : Integer read FDefaultBtn;
	end; { TTaskDlgButtons }

	ETaskDlgError = class (Exception);
	ETaskDlgOSVersionError = class (ETaskDlgError);

	TTaskDlgFlag = (tdfEnableHyperlinks, tdfUseHIconMain, tdfUseHIconFooter,
		tdfAllowDialogCancellation, tdfUseCommandLinks, tdfUseCommandLinksNoIcon,
		tdfExpandFooterArea, tdfExpandedByDefault, tdfVerificationFlagChecked,
		tdfShowProgressBar, tdfShowMarqueeProgressBar, tdfCallbackTimer,
		tdfPositionRelativeToWindow, tdfRtlLayout, tdfNoDefaultRadioButton,
		tdfCanBeMinimized);
	TTaskDlgFlags = set of TTaskDlgFlag;

	TTaskDlgCommonButton = (tcbOk, tcbYes, tcbNo, tcbCancel, tcbRetry, tcbClose);
	TTaskDlgCommonButtons = set of TTaskDlgCommonButton;
    TTaskDlgType = (tdtNone, tdtWarning, tdtError, tdtInformation, tdtShield);
    TTaskDlgResult = (tdrUndefined, tdrOk, tdrYes, tdrNo, tdrCancel, tdrRetry,
                      tdrClose, tdrTimedOut);

	TProgressBarState = (pbstNormal, pbstPause, pbstError);
	TUpdateElement = (tdeContent, tdeExpandedInformation, tdeFooter,
					  tdeMainInstruction);
	TUpdateIcon = (tdieIconMain, tdieIconFooter);

	TTaskDlgEvent = procedure (Sender: TObject) of object;
	TBtnClickedEvent = procedure (Sender: TObject; const iCommandID: Integer;
								  var bCanClose: Boolean) of object;
	THyperlinkClickedEvent = procedure (Sender: TObject;
								   		const sURL: WideString) of object;
 	TTimerEvent = procedure (Sender: TObject; const uElapsed: UInt;
    						 var bResetTimer: Boolean) of object;
	TExpandBtnClickedEvent = procedure (Sender: TObject;
										 const bExpanded: Boolean) of object;
	TRadioBtnClickedEvent = procedure (Sender: TObject;
									   const iButtonID: Integer) of object;
	TVerificationClickedEvent = procedure (Sender: TObject;
										   const bChecked: Boolean) of object;

	TTaskDlg = class
	  private
		FFlags : TTaskDlgFlags;
		FCommonButtons : TTaskDlgCommonButtons;
		FDefaultCommonButton : TTaskDlgCommonButton;

		FMainIcon : HIcon;
		FFooterIcon : HIcon;

		FWindowTitle : {$IFDEF UNICODE} String {$ELSE} WideString {$ENDIF};
		FExpandControlCaption : {$IFDEF UNICODE} String {$ELSE} WideString {$ENDIF};
		FExpandedControlCaption : {$IFDEF UNICODE} String {$ELSE} WideString {$ENDIF};
		FExpandedText : {$IFDEF UNICODE} String {$ELSE} WideString {$ENDIF};
		FFooterText : {$IFDEF UNICODE} String {$ELSE} WideString {$ENDIF};
		FContent : {$IFDEF UNICODE} String {$ELSE} WideString {$ENDIF};
		FMainInstruction : {$IFDEF UNICODE} String {$ELSE} WideString {$ENDIF};
        FTimeoutElapsedSec : UInt;
		FVerificationText : {$IFDEF UNICODE} String {$ELSE} WideString {$ENDIF};

		FModalResult : Integer;
		FRadioButton : Integer;
		FhTaskDlg : HWnd;
		FProgressBarPos : UInt;
		FInstance : LongWord;
		FProgressBarState : TProgressBarState;
		FTopMost : Boolean;
		FProgressBarMarquee : Boolean;
        FTag : NativeInt;
        FTimeoutSec : UInt;
        FVerificationCheckDisabled : Boolean;

        FVerificationFlagChecked : LongBool;

		FOnDestroy : TTaskDlgEvent;
		FOnDialogCreated : TTaskDlgEvent;
		FOnDialogConstructed : TTaskDlgEvent;
		FOnDialogDestroyed : TTaskDlgEvent;
		FOnBtnClicked : TBtnClickedEvent;
		FOnHyperlinkClicked : THyperlinkClickedEvent;
		FOnTimer : TTimerEvent;
		FOnExpandBtnClicked : TExpandBtnClickedEvent;
		FOnHelp : TTaskDlgEvent;
		FOnNavigated : TTaskDlgEvent;
		FOnRadioBtnClicked : TRadioBtnClickedEvent;
		FOnVerificationClicked : TVerificationClickedEvent;

    	CallBackStub, TimerProcStub : ICallbackStub;
        piRadioButton : pInteger;
        pTaskDlgCallback, pTimerCallback : Pointer;
	    pTaskDlgConfig : PTTaskDialogConfig;
		TaskDlgButtons : TTaskDlgButtons;
		TaskDlgRadioButtons : TTaskDlgButtons;
        uTimer : NativeUInt;
        bDialogTimedOut : Boolean;

		procedure BtnClicked (Sender: TObject; const {%H-}iCommandID: Integer;
		                      var bCanClose: Boolean);
        function EnableTopMost : Boolean;
        function GetButtonCount : Integer;
        function GetButtonID (const tcbButton: TTaskDlgCommonButton) : DWord;
        function GetContent : String;
        function GetCustomButtonsCount : Integer;
        function GetDefaultCustomBtnIndex : Integer;
        function GetDefaultRadioBtnIndex : Integer;
        function GetExpandControlCaption : String;
        function GetExpandedControlCaption : String;
        function GetExpandedText : String;
        function GetFooterText : String;
        function GetMainInstruction : String;
        function GetRadioButtonCount : Integer;
        function GetVerificationText : String;
        function GetWindowTitle : String;
        procedure InitTaskDlgConfig (const hParentWnd: HWnd);
//		function MakeProcInstance (M: TMethod) : Pointer;
		procedure SetContent (const sContent: String);
		procedure SetDefaultButton (const DefButton: TTaskDlgCommonButton);
        procedure SetExpandControlCaption (const sCaption: String);
        procedure SetExpandedControlCaption (const sCaption: String);
		procedure SetExpandedText (const sExpandedText: String);
        procedure SetFlags (Flags: TTaskDlgFlags);
		procedure SetMainInstruction (const sMainInstruction: String);
        procedure SetOnBtnClicked (Event: TBtnClickedEvent);
		procedure SetProgressBarMarquee (const bMarquee: Boolean);
		procedure SetProgressBarPos (const uNewPos: UInt);
		procedure SetProgressBarState (const NewState: TProgressBarState);
        procedure SetTimeout (const uTimeoutSec: UInt);
		procedure SetTimerEvent (const TimerProc: TTimerEvent);
		procedure SetTopMost (const bTopMost: Boolean);
		function ShowTaskDialog (const hParentWnd: HWnd) : HResult;
		procedure UpdateElementText (const Element: TUpdateElement;
									 const sText: String);
		procedure SetFooterText (const sFooterText: String);
		procedure UpdateFooterIcon (const hNewIcon: HIcon);
		procedure UpdateIcon (const UpdateIcon: TUpdateIcon;
							  const hNewIcon: HIcon);
		procedure UpdateMainIcon (const hNewIcon: HIcon);

		function TaskDlgCallBack (hwnd: HWND; Msg: UINT; wParam: WPARAM;
							      lParam: LPARAM;
                                  {%H-}pRefData: LONG_PTR) : HResult; stdcall;
        procedure TimeoutTimerProc ({%H-}hWindow: hWnd; {%H-}uMsg: UInt;
                                    {%H-}uID: UInt_Ptr;
                                    {%H-}dwTime: DWord); stdcall;

	  public
		constructor Create;
		destructor Destroy; override;

		procedure AddButton (const iID: Integer; const sButtonText: String;
							 const bDefault: Boolean = false);
		procedure AddRadioButton (const iID: Integer;
								  const sButtonText: String;
								  const bDefault: Boolean = false);
		function Execute (const hParent: HWnd = 0) : Boolean;

		function ClickButton (const tcbButton: TTaskDlgCommonButton) : Boolean;
        															   overload;
		function ClickButton (const uButton_ID: UInt) : Boolean; overload;
		procedure ClickRadioButton (const iButton_ID: Integer);
		procedure ClickVerification (const bCheck, bFocus: Boolean);
		procedure EnableButton (const dwButton_ID: DWord;
								const bEnable: Boolean); overload;
		procedure EnableButton (const tcbButton: TTaskDlgCommonButton;
								const bEnable: Boolean); overload;
		procedure EnableRadioButton (const iButton_ID: Integer;
									 const bEnable: Boolean);
        function GetRadioBtnText (const iIndex: Integer) : String;
		procedure ProgressBarStepBy (const uStepBy: UInt);
		procedure SetButtonElevationRequiredState (const iButton_ID: Integer;
												   const bRequired: Boolean);
		procedure SetProgressBarRange (const wMin, wMax: Word);
        procedure SetVerificationText (const sVerificationText: String);
        procedure SetWindowTitle (const sWindowTitle: String);

        property Buttons : Integer read GetButtonCount;
		property CommonButtons : TTaskDlgCommonButtons read FCommonButtons
													   write FCommonButtons;
		property Content : String read GetContent write SetContent;
        property CustomButtons : Integer read GetCustomButtonsCount;
		property DefaultCommonButton : TTaskDlgCommonButton
                                                       read FDefaultCommonButton
													   write SetDefaultButton;
        property DefaultCustomButton : Integer read GetDefaultCustomBtnIndex;
        property DefaultRadioButton : Integer read GetDefaultRadioBtnIndex;
		property ExpandControlCaption : String read GetExpandControlCaption
											   write SetExpandControlCaption;
		property ExpandedControlCaption : String read GetExpandedControlCaption
        										 write SetExpandedControlCaption;
		property ExpandedText : String read GetExpandedText
									   write SetExpandedText;
		property Flags : TTaskDlgFlags read FFlags write SetFlags;
		property FooterIcon : HIcon read FFooterIcon write UpdateFooterIcon;
		property FooterText : String read GetFooterText write SetFooterText;
		property hTaskDlg : HWnd read FhTaskDlg;
		property Instance : LongWord read FInstance write FInstance;
		property MainIcon : HIcon read FMainIcon write UpdateMainIcon;
		property MainInstruction : String read GetMainInstruction
										  write SetMainInstruction;
		property ModalResult : Integer read FModalResult;
		property ProgressBarPos : UInt read FProgressBarPos
									   write SetProgressBarPos;
		property ProgressBarMarquee : Boolean read FProgressBarMarquee
											  write SetProgressBarMarquee;
		property ProgressBarState : TProgressBarState read FProgressBarState
        											  write SetProgressBarState;
		property RadioButton : Integer read FRadioButton;
        property RadioButtons : Integer read GetRadioButtonCount;
        property Tag : NativeInt read FTag write FTag;
        property TimeoutElapsedSec : UInt read FTimeoutElapsedSec;
        property TimeoutSec : UInt read FTimeoutSec write SetTimeout;
		property TopMost : Boolean read FTopMost write SetTopMost;
        property VerificationFlagChecked : LongBool read FVerificationFlagChecked;
        property VerificationCheckDisabled : Boolean read FVerificationCheckDisabled
           write FVerificationCheckDisabled;
		property VerificationText : String read GetVerificationText
										   write SetVerificationText;
		property WindowTitle : String read GetWindowTitle write SetWindowTitle;

		property OnBtnClicked : TBtnClickedEvent read FOnBtnClicked
												 write SetOnBtnClicked;
		property OnDestroy : TTaskDlgEvent read FOnDestroy write FOnDestroy;
		property OnDialogConstructed : TTaskDlgEvent read FOnDialogConstructed
													 write FOnDialogConstructed;
		property OnDialogCreated : TTaskDlgEvent read FOnDialogCreated
												 write FOnDialogCreated;
		property OnDialogDestroyed : TTaskDlgEvent read FOnDialogDestroyed
												   write FOnDialogDestroyed;
		property OnExpandBtnClicked : TExpandBtnClickedEvent
						     read FOnExpandBtnClicked write FOnExpandBtnClicked;
        property OnHelp : TTaskDlgEvent read FOnHelp write FOnHelp;
		property OnNavigated : TTaskDlgEvent read FOnNavigated
        									 write FOnNavigated;
        property OnRadioBtnClicked : TRadioBtnClickedEvent
        					   read FOnRadioBtnClicked write FOnRadioBtnClicked;
		property OnTimer : TTimerEvent read FOnTimer write SetTimerEvent;
		property OnHyperlinkClicked : THyperlinkClickedEvent
        					 read FOnHyperlinkClicked write FOnHyperlinkClicked;
        property OnVerificationClicked : TVerificationClickedEvent
        			   read FOnVerificationClicked write FOnVerificationClicked;
	end; { TTaskDlg }

implementation

uses Messages;

ResourceString
	cID_Msg = 'Please assign an ID >= 10 for custom buttons!';
	cFooterTextMissing = 'You must specify a FooterText to use a footer icon!';
    cMainIconAssigned = 'A "MainIcon" has already been assigned';
	cNoDefRadioBtn = 'You must specify a default radio button!';
    cRadioBtnIndexErr = 'A radio button with the index %d does not exist!';
	cUnknownMsg = '"TTaskDlg.TaskDlgCallBack": Received an unknown ' +
                  'notification message!'#13#10'ID = %d';
    cWindowsVistaRequired =
    	'To use the "TaskDialog" functions Windows Vista or later is required!';

    cE_OUTOFMEMORY = 'There is insufficient memory to complete the operation.';
	cE_INVALIDARG = 'One or more arguments are invalid.';
	cE_FAIL = 'The operation failed.';

type
	TCallbackProc = function (hwnd: HWND; Msg: UINT; wParam: WPARAM;
							  lParam: LPARAM;
                              lpRefData: LONG_PTR) : HResult of object; stdcall;
    TTimerProc = procedure (hWindow: hWnd; uMsg: UInt; uID: UInt_Ptr;
					        dwTime: DWord) of object; stdcall;

var
    DefDlgProc : {$IFDEF FPC} WNDPROC {$ELSE} Pointer {$ENDIF} = NIL;

(* ---- *)

function DlgWindowProc (hwnd: HWND; Msg: UINT; wParam: WPARAM;
						lParam: LPARAM) : HResult; stdcall;
begin
{$IFDEF DEBUG}
    if (Msg = WM_Notify) then
        OutputDebugString ('WM_Notify');
{$ENDIF}

    Result := CallWindowProc (DefDlgProc, hwnd, Msg, wParam, lParam);
end; { DlgWindowProc }

(* ---- *)

function TTaskDlgButtons.GetCount : Integer;
begin
    Result := Length (aTaskDlgButtons);
end; { TTaskDlgButtons.GetCount }

(* ---- *)

procedure TTaskDlgButtons.AddButton (const iID: Integer;
									 const sButtonText: String;
        					 		 const bDefault: Boolean);
begin
    Assert (sButtonText <> '');

	if (iID < 10) then
		raise Exception.Create (cID_Msg);

	if (bDefault) then
    	FDefaultBtn := iID;

	SetLength (aTaskDlgButtons, Length (aTaskDlgButtons) + 1);
    SetLength (aButtonText, Length (aButtonText) + 1);


    aButtonText [High (aButtonText)] :=
                            {$IFNDEF UNICODE} WideString {$ENDIF} (sButtonText);

    with aTaskDlgButtons [High (aTaskDlgButtons)] do
    begin
		nButtonID := iID;
        pszButtonText := PWideChar (aButtonText [High (aButtonText)]);
	end; { with }
end; { TTaskDlgButtons.AddButton }

(* ---- *)

function TTaskDlgButtons.GetButtons : PTaskDialogButton;
begin
	Result := @aTaskDlgButtons [0];
end; { TTaskDlgButtons.GetButtons }

(* ---- *)

procedure TTaskDlg.BtnClicked (Sender: TObject; const iCommandID: Integer;
							   var bCanClose: Boolean);

(**
resourcestring
	cNotifyMsg = 'Please define your own "TTaskDlg.OnBtnClicked" handler!';
**)

begin
	bCanClose := true;

(**
	if (FCommonButtons <> []) or (TaskDlgButtons.GetCount > 0) then
		MessageBox (FhTaskDlg, PChar (cNotifyMsg),
					PChar (ExtractFileName (ParamStr (0))),
					mb_OK or mb_IconStop);
**)
end; { TTaskDlg.BtnClicked }

(* ---- *)

(**
function TTaskDlg.MakeProcInstance (M: TMethod) : Pointer;
begin
{*
    // http://www.delphipraxis.net/160650-hook-fuehrt-zu-system-exception.html
    GetMem (Result, 7 + 2 * SizeOf (Pointer));

    PByte (Result)^ := $B9;
    Inc (PByte (Result));
    PPointer (Result)^ := M.Data;
    Inc (PPointer (Result));
    PLongWord (Result)^ := $B952515A;
    Inc (PLongInt (Result));
    PPointer (Result)^ := M.Code;
    Inc (PPointer (Result));
    PByte (Result)^ := $FF;
    Inc (PByte (Result));
    PByte (Result)^ := $E1;
    Dec (PByte (Result), 6 + 2 * SizeOf (Pointer));
*}

{*
// Alternative Methode
	// Ausführbaren Speicher alloziieren für 15 Byte an Code
	Result := VirtualAlloc (NIL, 15, MEM_COMMIT, PAGE_EXECUTE_READWRITE);

	asm
		// MOV ECX,
		MOV BYTE PTR [EAX], $B9
		MOV ECX, M.Data
		MOV DWORD PTR [EAX+$1], ECX
		// POP EDX (bisherige Rücksprungadresse nach edx)
		MOV BYTE PTR [EAX+$5], $5A
		// PUSH ECX (self als Parameter 0 anfügen)
		MOV BYTE PTR [EAX+$6], $51
		// PUSH EDX (Rücksprungadresse zurück auf den Stack)
		MOV BYTE PTR [EAX+$7], $52
		// MOV ECX, (Adresse nach ecx laden)
		MOV BYTE PTR [EAX+$8], $B9
		MOV ECX, M.Code
		MOV DWORD PTR [EAX+$9], ECX
		// JMP ECX (Sprung an den ersten abgelegten Befehl und Methode aufrufen)
		MOV BYTE PTR [EAX+$D], $FF
		MOV BYTE PTR [EAX+$E], $E1
		// hier kein Call, ansonsten würde noch eine Rücksprungadresse auf den Stack gelegt
	end;
*}
end; { TTaskDlg.MakeProcInstance }
**)

(* ---- *)

function TTaskDlg.EnableTopMost : Boolean;
begin
	Assert (FhTaskDlg <> 0);
    Assert (IsWindow (FhTaskDlg));

    if (FTopMost) then
		Result := SetWindowPos (FhTaskDlg, HWND_TOPMOST, 0, 0, 0, 0,
					  			SWP_NOMOVE or SWP_NOSIZE)
	else Result := SetWindowPos (FhTaskDlg, HWND_NOTOPMOST, 0, 0, 0, 0,
					  			 SWP_NOMOVE or SWP_NOSIZE);
end; { TTaskDlg.EnableTopMost }

(* ---- *)

function TTaskDlg.GetButtonCount : Integer;
begin
    Result := TaskDlgButtons.Count;
end; { TTaskDlg.GetButtonCount }

(* ---- *)

function TTaskDlg.GetButtonID (const tcbButton: TTaskDlgCommonButton): DWord;
begin
    case tcbButton of
        tcbOk : Result := IDOK;
        tcbYes : Result := IDYES;
        tcbNo : Result := IDNO;
        tcbCancel : Result := IDCANCEL;
        tcbRetry : Result := IDRETRY;
        tcbClose : Result := IDCLOSE;
        else Result := 0;
    end; { case }
end; { TTaskDlg.GetButtonID }

(* ---- *)

function TTaskDlg.GetContent : String;
begin
    Result := String (FContent);
end; { TTaskDlg.GetContent }

(* ---- *)

function TTaskDlg.GetCustomButtonsCount : Integer;
begin
	Result := TaskDlgButtons.Count;
end; { TTaskDlg.GetCustomButtonsCount }

(* ---- *)

function TTaskDlg.GetDefaultCustomBtnIndex : Integer;
begin
    Result := TaskDlgButtons.FDefaultBtn;
end; { TTaskDlg.GetDefaultCustomBtnIndex }

(* ---- *)

function TTaskDlg.GetDefaultRadioBtnIndex : Integer;
begin
    Result := TaskDlgRadioButtons.FDefaultBtn;
end; { TTaskDlg.GetDefaultRadioBtnIndex; }

(* ---- *)

function TTaskDlg.GetExpandControlCaption : String;
begin
    Result :=  String (FExpandControlCaption);
end; { TTaskDlg.GetExpandControlCaption }

(* ---- *)

function TTaskDlg.GetExpandedControlCaption : String;
begin
    Result := String (FExpandedControlCaption);
end; { TTaskDlg.GetExpandedControlCaption }

(* ---- *)

function TTaskDlg.GetExpandedText : String;
begin
    Result := String (FExpandedText);
end; { TTaskDlg.GetExpandedText }

(* ---- *)

function TTaskDlg.GetFooterText : String;
begin
    Result := String (FFooterText);
end; { TTaskDlg.GetFooterText }

(* ---- *)

function TTaskDlg.GetMainInstruction : String;
begin
    Result := String (FMainInstruction);
end; { TTaskDlg.GetMainInstruction }

(* ---- *)

function TTaskDlg.GetRadioButtonCount : Integer;
begin
    Result := TaskDlgRadioButtons.Count;
end; { TTaskDlg.GetRadioButtonCount }

(* ---- *)

function TTaskDlg.GetVerificationText : String;
begin
    Result := String (FVerificationText);
end; { TTaskDlg.GetVerificationText }

(* ---- *)

function TTaskDlg.GetWindowTitle : String;
begin
    Result := String (FWindowTitle);
end; { TTaskDlg.GetWindowTitle }

(* ---- *)

procedure TTaskDlg.InitTaskDlgConfig (const hParentWnd: HWnd);

	(* ---- *)

	procedure ConfigureButtons (var uCount: UInt;
    							var pButtons: PTaskDialogButton;
                                var iDefaultBtn: Integer;
                                const TaskDlgButtons: TTaskDlgButtons);
    begin
        if (TaskDlgButtons.GetCount > 0) then
		begin
            uCount := TaskDlgButtons.GetCount;
			pButtons := TaskDlgButtons.GetButtons;

	        if (TaskDlgButtons.DefaultBtn > 0) then
    	      	iDefaultBtn := TaskDlgButtons.DefaultBtn;
		end { if }
		else
		begin
			uCount := 0;
			pButtons := NIL;
		end; { else }
    end; { ConfigureButtons }

	(* ---- *)

const
	cTaskDlgCommonButtons: array [TTaskDlgCommonButton] of UInt = (
		TDCBF_OK_BUTTON, TDCBF_YES_BUTTON, TDCBF_NO_BUTTON,
		TDCBF_CANCEL_BUTTON, TDCBF_RETRY_BUTTON, TDCBF_CLOSE_BUTTON);

	cTaskDlgIcons: array [tdiNone..tdiShield] of UInt = (
		0, TD_WARNING_ICON, TD_ERROR_ICON, TD_INFORMATION_ICON, TD_SHIELD_ICON);

	cTaskDlgFlags: array [TTaskDlgFlag] of UInt = (
		TDF_Enable_Hyperlinks, TDF_Use_Hicon_Main,
		TDF_Use_Hicon_Footer, TDF_ALLOW_DIALOG_CANCELLATION,
		TDF_USE_COMMAND_LINKS, TDF_USE_COMMAND_LINKS_NO_ICON,
		TDF_EXPAND_FOOTER_AREA, TDF_EXPANDED_BY_DEFAULT,
		TDF_VERIFICATION_FLAG_CHECKED, TDF_SHOW_PROGRESS_BAR,
		TDF_SHOW_MARQUEE_PROGRESS_BAR, TDF_CALLBACK_TIMER,
		TDF_POSITION_RELATIVE_TO_WINDOW, TDF_RTL_LAYOUT,
		TDF_NO_DEFAULT_RADIO_BUTTON, TDF_CAN_BE_MINIMIZED);

	cTaskDlgDefaultButtons: array [TTaskDlgCommonButton] of UInt = (
		IDOK, IDYES, IDNO, IDCANCEL, IDRETRY, IDCLOSE);

var
	CommonButton : TTaskDlgCommonButton;
	TaskDlgFlag: TTaskDlgFlag;

begin { TTaskDlg.InitTaskDlgConfig }
    if (pTaskDlgConfig <> NIL) then
        FreeMem (pTaskDlgConfig);

    GetMem (pTaskDlgConfig, SizeOf (TTaskDialogConfig));

    FillChar ({%H-}pTaskDlgConfig^, SizeOf (TTaskDialogConfig), #0);

	with pTaskDlgConfig^ do
	begin
		// Set Size, Parent window, Flags
		cbSize := SizeOf (TTaskDialogConfig);
		hwndParent := hParentWnd;

		if (FInstance = 0) then
			hInstance := MainInstance
		else hInstance := FInstance;

        for TaskDlgFlag := Low (TTaskDlgFlag) to High (TTaskDlgFlag) do
        	if TaskDlgFlag in FFlags then
            	dwFlags := dwFlags or cTaskDlgFlags [TaskDlgFlag];

        FVerificationFlagChecked := tdfVerificationFlagChecked in FFlags;

        for CommonButton := tcbOk to High (TTaskDlgCommonButton) do
        	if (CommonButton in FCommonButtons) then
            	dwCommonButtons := dwCommonButtons or
                						   CTaskDlgCommonButtons [CommonButton];

		if (FContent <> '') then
			pszContent := PWideChar (FContent);

		if (FMainInstruction <> '') then
			pszMainInstruction := PWideChar (FMainInstruction);

		if (FWindowTitle <> '') then
			pszWindowTitle := PWideChar (FWindowTitle);

        if (tdfUseHiconMain in FFlags) then
			hMainIcon := FMainIcon
		else
		begin
			if (FMainIcon <= tdiShield) then
				pszMainIcon := {%H-}PWideChar (cTaskDlgIcons [FMainIcon])
			else pszMainIcon := {%H-}MakeIntResourceW (FMainIcon);
		end; { else }

		nDefaultButton := cTaskDlgDefaultButtons [FDefaultCommonButton];

		if (FFooterText <> '') then
			pszFooter := PWideChar (FFooterText);

		if (tdfUseHiconFooter in FFlags) then
			hFooterIcon := FFooterIcon
		else
			if (FFooterIcon <= tdiShield) then
				pszFooterIcon := {%H-}PWideChar (cTaskDlgIcons [FFooterIcon])
			else pszFooterIcon := PWideChar (
								  	{%H-}MakeIntResourceW (Word (FFooterIcon)));

        if (hFooterIcon <> 0) then
            Assert (FFooterText <> '', cFooterTextMissing);

		if (FVerificationText <> '') then
        	pszVerificationText := PWideChar (FVerificationText);

        if (FExpandedText <> '') then
        	pszExpandedInformation := PWideChar (FExpandedText);

		if (FExpandControlCaption <> '') then
			pszCollapsedControlText := PWideChar (FExpandControlCaption);

		if (FExpandedControlCaption <> '') then
			pszExpandedControlText := PWideChar (FExpandedControlCaption);

		ConfigureButtons (cButtons, pButtons, nDefaultButton, TaskDlgButtons);

		ConfigureButtons (cRadioButtons, pRadioButtons, nDefaultRadioButton,
						  TaskDlgRadioButtons);

		if (TaskDlgRadioButtons.GetCount > 0) then
		begin
			Assert ((tdfNoDefaultRadioButton in FFlags) or
                    (nDefaultRadioButton > 0), cNoDefRadioBtn);
			piRadioButton := @FRadioButton
		end { if }
		else piRadioButton := NIL;

		pfCallback := TFTaskDialogCallback (pTaskDlgCallback);
	end; { with }
end; { TTaskDlg.InitTaskDlgConfig }

(* ---- *)

procedure TTaskDlg.SetContent (const sContent: String);
begin
	FContent := {$IFNDEF UNICODE} WideString {$ENDIF} (sContent);
	UpdateElementText (tdeContent, sContent)
end; { TTaskDlg.SetContent }

(* ---- *)

procedure TTaskDlg.SetDefaultButton (const DefButton: TTaskDlgCommonButton);
begin
	Assert (DefButton in FCommonButtons);

	FDefaultCommonButton := DefButton;
end; { TTaskDlg.SetDefaultButton }

(* ---- *)

procedure TTaskDlg.SetExpandControlCaption (const sCaption: String);
begin
    FExpandControlCaption := {$IFNDEF UNICODE} WideString {$ENDIF} (sCaption);
end; { TTaskDlg.SetExpandControlCaption }

(* ---- *)

procedure TTaskDlg.SetExpandedControlCaption (const sCaption: String);
begin
    FExpandedControlCaption := {$IFNDEF UNICODE} WideString {$ENDIF} (sCaption);
end; { TTaskDlg.SetExpandedControlCaption }

(* ---- *)

procedure TTaskDlg.SetExpandedText (const sExpandedText: String);
begin
	FExpandedText := {$IFNDEF UNICODE} WideString {$ENDIF} (sExpandedText);
	UpdateElementText (tdeExpandedInformation, sExpandedText)
end; { TTaskDlg.SetExpandedText }

(* ---- *)

procedure TTaskDlg.SetFlags (Flags: TTaskDlgFlags);
begin
    FFlags := Flags;

    if (pTaskDlgConfig <> NIL) then
    begin
        InitTaskDlgConfig (pTaskDlgConfig^.hwndParent);
        SendMessage (FhTaskDlg, TDM_NAVIGATE_PAGE, 0,
                     {%H-}LParam (pTaskDlgConfig));
    end; { if }
end; { TTaskDlg.SetFlags }

(* ---- *)

procedure TTaskDlg.SetMainInstruction (const sMainInstruction: String);
begin
	FMainInstruction := {$IFNDEF UNICODE} WideString {$ENDIF} (sMainInstruction);
	UpdateElementText (tdeMainInstruction, sMainInstruction)
end; { TTaskDlg.SetMainInstruction }

(* ---- *)

procedure TTaskDlg.SetOnBtnClicked (Event: TBtnClickedEvent);
begin
	if (Assigned (Event)) then
    	FOnBtnClicked := Event
    else FOnBtnClicked := BtnClicked;
end; { TTaskDlg.SetOnBtnClicked }

(* ---- *)

procedure TTaskDlg.SetProgressBarMarquee (const bMarquee: Boolean);
begin
	if (FhTaskDlg <> 0) then
	begin
		FProgressBarMarquee := bMarquee;
		SendMessage (FhTaskDlg, TDM_SET_MARQUEE_PROGRESS_BAR,
					 WPARAM (bMarquee), 0);
	end; { if }

    if (bMarquee) then
    begin
        Exclude (FFlags, tdfShowProgressBar);
        Include (FFlags, tdfShowMarqueeProgressBar);
    end { if }
    else
    begin
        Exclude (FFlags, tdfShowMarqueeProgressBar);
        Include (FFlags, tdfShowProgressBar);
    end; { else }
end; { TTaskDlg.SetProgressBarMarquee }

(* ---- *)

procedure TTaskDlg.SetProgressBarPos (const uNewPos: UInt);
begin
	Assert ((tdfShowProgressBar in FFlags) or
			(tdfShowMarqueeProgressBar in FFlags));
	Assert (FhTaskDlg <> 0);

	FProgressBarPos := uNewPos;
	SendMessage (FhTaskDlg, TDM_SET_PROGRESS_BAR_POS, FProgressBarPos, 0);
end; { TTaskDlg.SetProgressBarPos }

(* ---- *)

procedure TTaskDlg.SetProgressBarState (const NewState: TProgressBarState);

var
	iNewState : Integer;
	bResult : Boolean;

begin
	Assert (FhTaskDlg <> 0);

	case NewState of
		pbstNormal : iNewState := PBST_NORMAL;
		pbstPause  : iNewState := PBST_PAUSED;
		pbstError  : iNewState := PBST_ERROR;
        else iNewState := 0;
	end; { case }

	bResult := SendMessage (FhTaskDlg, TDM_SET_PROGRESS_BAR_STATE,
							iNewState, 0) <> 0;

	if (bResult) then
		FProgressBarState := NewState;
end; { TTaskDlg.SetProgressBarState }

(* ---- *)

procedure TTaskDlg.SetTimeout (const uTimeoutSec: UInt);
begin
    if (FTimeoutSec <> uTimeoutSec) then
    begin
        FTimeoutSec := uTimeoutSec;
        FTimeoutElapsedSec := 0;

        if (uTimeoutSec = 0) and (uTimer <> 0) then
        begin
            KillTimer (hTaskDlg, uTimer);
            uTimer := 0;
        end { if }
        else if (uTimeoutSec > 0) and (uTimer = 0) and (hTaskDlg <> 0) then
        begin
            uTimer := SetTimer (0, 0, 1000, pTimerCallback);
            Assert (uTimer <> 0);
        end; { else if }

        bDialogTimedOut := false;
    end; { if }
end; { TTaskDlg.SetTimeout }

(* ---- *)

procedure TTaskDlg.SetTimerEvent (const TimerProc: TTimerEvent);
begin
	FOnTimer := TimerProc;

	if not (tdfCallbackTimer in FFlags) then
        Include (FFlags, tdfCallbackTimer);
end; { TTaskDlg.SetTimerEvent }

(* ---- *)

procedure TTaskDlg.SetTopMost (const bTopMost: Boolean);
begin
	FTopMost := bTopMost;

	if (FhTaskDlg <> 0) then
    	EnableTopMost;
end; { TTaskDlg.SetTopMost }

(* ---- *)

procedure TTaskDlg.SetWindowTitle (const sWindowTitle: String);
begin
	FWindowTitle := {$IFNDEF UNICODE} WideString {$ENDIF} (sWindowTitle);

	if (FhTaskDlg <> 0) then
		SetWindowTextW (FhTaskDlg, PWideChar (FWindowTitle));
end; { TTaskDlg.SetWindowTitle }

(* ---- *)

function TTaskDlg.ShowTaskDialog (const hParentWnd: HWnd) : HResult;

var
    pbVerificationFlagChecked : PBool;

begin { TTaskDlg.ShowTaskDialog }
    InitTaskDlgConfig (hParentWnd);

    try
        if (FVerificationText = '') or (FVerificationCheckDisabled) then
            pbVerificationFlagChecked := NIL
        else pbVerificationFlagChecked := @FVerificationFlagChecked;

	    Result := TaskDialogIndirect (pTaskDlgConfig, FModalResult,
								      piRadioButton, pbVerificationFlagChecked);

    finally
        FreeMem (pTaskDlgConfig);
        pTaskDlgConfig := NIL;
        piRadioButton := NIL;
    end; { try / finally }
end; { TTaskDlg.ShowTaskDialog }

(* ---- *)

procedure TTaskDlg.UpdateElementText (const Element: TUpdateElement;
									  const sText: String);

var
	iElement : Integer;

begin
	if (FhTaskDlg = 0) then
        exit;

    iElement := 0;

	case Element of
		tdeContent : iElement := TDE_CONTENT;
		tdeExpandedInformation : iElement := TDE_EXPANDED_INFORMATION;
		tdeFooter : iElement := TDE_FOOTER;
		tdeMainInstruction : iElement := TDE_MAIN_INSTRUCTION;
	end; { case }

	SendMessage (FhTaskDlg, TDM_UPDATE_ELEMENT_TEXT, iElement,
                 {%H-}LPARAM (PWideChar ({$IFNDEF UNICODE} WideString {$ENDIF}
                                         (sText))))
end; { TTaskDlg.UpdateElementText }

(* ---- *)

procedure TTaskDlg.SetFooterText (const sFooterText: String);
begin
	FFooterText := {$IFNDEF UNICODE} WideString {$ENDIF} (sFooterText);
	UpdateElementText (tdeFooter, sFooterText)
end; { TTaskDlg.SetFooterText }

(* ---- *)

procedure TTaskDlg.UpdateFooterIcon (const hNewIcon: HIcon);
begin
    Assert ((FFooterText <> '') or (hNewIcon = TD_NO_ICON), cFooterTextMissing);

	FFooterIcon := hNewIcon;
	UpdateIcon (tdieIconFooter, hNewIcon);
end; { TTaskDlg.UpdateFooterIcon }

(* ---- *)

procedure TTaskDlg.UpdateIcon (const UpdateIcon: TUpdateIcon;
							   const hNewIcon: HIcon);

var
	iUpdateIcon : Integer;

begin
	if (FhTaskDlg <> 0) then
    begin
        case UpdateIcon of
            tdieIconMain : iUpdateIcon := TDIE_ICON_MAIN;
            tdieIconFooter : iUpdateIcon := TDIE_ICON_FOOTER;
            else iUpdateIcon := 0;
        end; { case }

		SendMessage (FhTaskDlg, TDM_UPDATE_ICON, iUpdateIcon, hNewIcon);
    end { if }
end; { TTaskDlg.UpdateIcon }

(* ---- *)

procedure TTaskDlg.UpdateMainIcon (const hNewIcon: HIcon);
begin
	if (FhTaskDlg = 0) then
        if (FMainIcon <> 0) then
            raise Exception.Create (cMainIconAssigned);

    FMainIcon := hNewIcon;
	UpdateIcon (tdieIconMain, hNewIcon);
end; { TTaskDlg.UpdateMainIcon }

(* ---- *)

function TTaskDlg.TaskDlgCallBack (hwnd: HWND; Msg: UINT; wParam: WPARAM;
                                   lParam: LPARAM;
                                   pRefData: LONG_PTR) : HResult; stdcall;

var
	bReturnValue : Boolean;

begin
	Result := S_OK;

	case Msg of
		TDN_CREATED :
            // Sent by the Task Dialog once the dialog has been created and
            // before it gets displayed
			begin
            	if (FhTaskDlg = 0) then
					FhTaskDlg := hWnd;

				if (FTopMost) then
                    EnableTopMost;

				if (Assigned (FOnDialogCreated)) then
					FOnDialogCreated (Self);

                if (FTimeoutSec > 0) and (uTimer = 0) then
                    uTimer := SetTimer (0, 0, 1000, pTimerCallback);

(**
                DefDlgProc := Pointer (GetWindowLongPtr (hWnd, GWLP_WNDPROC));

                if (DefDlgProc <> NIL) then
                    if (SetWindowLongPtr (hWnd, GWLP_WNDPROC,
                                          NativeUInt (@DlgWindowProc)) = 0) then
                        RaiseLastWin32Error;
**)
			end; { case TDN_CREATED }

		TDN_DESTROYED :
        	begin
            	FhTaskDlg := 0;

                if (uTimer <> 0) then
                begin
                    KillTimer (0, uTimer);
                    uTimer := 0;
                end; { if }

                if (Assigned (FOnDialogDestroyed)) then
					FOnDialogDestroyed (Self);
            end; { case TDN_DESTROYED }

		TDN_DIALOG_CONSTRUCTED :
            // Sent by the task dialog after the dialog has been created and
            // before it is displayed
        	begin
            	if (FhTaskDlg = 0) then
					FhTaskDlg := hWnd;

				if (FTopMost) then
                    EnableTopMost;

				if (Assigned (FOnDialogConstructed)) then
					FOnDialogConstructed (Self);
        	end; { case TDN_DIALOG_CONSTRUCTED }

		TDN_BUTTON_CLICKED :
			if (Assigned (FOnBtnClicked)) then
			begin
				bReturnValue := true;  // bCanClose = true

                // wParam = idYes, idNo, idOk, idCancel for standard buttons
				FOnBtnClicked (Self, wParam, bReturnValue);

				if (bReturnValue = false) then
					Result := S_FALSE;
			end; { if }

		TDN_HYPERLINK_CLICKED :
			if (Assigned (FOnHyperlinkClicked)) then
				FOnHyperlinkClicked (Self, {%H-}PWideChar (lParam));

		TDN_TIMER :
            begin
			    if (Assigned (FOnTimer)) then
                begin
				    bReturnValue := false;  // bResetTimer = false

				    FOnTimer (Self, wParam, bReturnValue);

                    if (bReturnValue = true) then
                	    Result := S_FALSE;
                end; { if }
            end; { case TDN_Timer }

		TDN_EXPANDO_BUTTON_CLICKED  :
			if (Assigned (FOnExpandBtnClicked)) then
				FOnExpandBtnClicked (Self, Bool (wParam));

		TDN_HELP :
			if (Assigned (FOnHelp)) then
				FOnHelp (Self);

		TDN_NAVIGATED :  // Only sent in response to a TDM_NAVIGATE_PAGE message
			if (Assigned (FOnNavigated)) then
				FOnNavigated (Self);

		TDN_RADIO_BUTTON_CLICKED :
			if (Assigned (FOnRadioBtnClicked)) then
				FOnRadioBtnClicked (Self, wParam);

		TDN_VERIFICATION_CLICKED :
        	begin
            	FVerificationFlagChecked := wParam = 1;

				if (Assigned (FOnVerificationClicked)) then
					FOnVerificationClicked (Self, FVerificationFlagChecked);
            end; { case TDN_VERIFICATION_CLICKED }

		else MessageBox (FhTaskDlg, PChar (Format (cUnknownMsg, [Msg])),
						 PChar (ExtractFileName (ParamStr (0))), mb_IconStop);
	end; { case }
end; { TTaskDlg.TaskDlgCallBack }

(* ---- *)

procedure TTaskDlg.TimeoutTimerProc (hWindow: hWnd; uMsg: UInt; uID: UInt_Ptr;
                                     dwTime: DWord); stdcall;
begin
    if (bDialogTimedOut = false) and (FTimeoutSec > 0) then
    begin
        Inc (FTimeoutElapsedSec);

        if (FTimeoutElapsedSec >= FTimeoutSec) then
        begin
            bDialogTimedOut := true;
            PostMessage (hTaskDlg, wm_Close, 0, 0);
        end; { if }
    end; { if }
end; { TTaskDlg.TimeoutTimerProc }

(* ---- *)

constructor TTaskDlg.Create;

	(* ---- *)

	procedure InitTaskDlgCallback;

	var
		CallBackProc : TCallBackProc;

	begin
    	CallBackProc := TaskDlgCallBack;

		CallBackStub := TCallbackStub.Create (Self, @CallBackProc, 5);
        pTaskDlgCallback := CallBackStub.StubPointer;
	end; { InitTaskDlgCallback }

	(* ---- *)

	procedure InitTimerCallback;

	var
		CallBackProc : TTimerProc;

	begin
    	CallBackProc := TimeoutTimerProc;

		TimerProcStub := TCallbackStub.Create (Self, @CallBackProc, 4);
        pTimerCallback := TimerProcStub.StubPointer;
	end; { InitTimerCallback }

	(* ---- *)

begin { TTaskDlg.Create }
	if (Win32MajorVersion < 6) then
		raise ETaskDlgOSVersionError.Create (cWindowsVistaRequired);

	inherited;

	InitTaskDlgCallback;
    InitTimerCallback;

	FOnBtnClicked := BtnClicked; { Default handler to allow closing of dialog }

	TaskDlgButtons := TTaskDlgButtons.Create;
	TaskDlgRadioButtons := TTaskDlgButtons.Create;
end; { TTaskDlg.Create }

(* ---- *)

destructor TTaskDlg.Destroy;
begin
    if (Assigned (FOnDestroy)) then
    	FOnDestroy (Self);

	TaskDlgButtons.Free;
	TaskDlgRadioButtons.Free;

	inherited;
end; { TTaskDlg.Done }

(* ---- *)

procedure TTaskDlg.AddButton (const iID: Integer; const sButtonText: String;
							  const bDefault: Boolean = false);
begin
	Assert (FhTaskDlg = 0);
    Assert (sButtonText <> '');

	if (iID < 10) then
		raise Exception.Create (cID_Msg);

	TaskDlgButtons.AddButton (iID, sButtonText, bDefault);
end; { TTaskDlg.AddButton }

(* ---- *)

procedure TTaskDlg.AddRadioButton (const iID: Integer;
								   const sButtonText: String;
								   const bDefault: Boolean);
begin
	Assert (FhTaskDlg = 0);
    Assert (sButtonText <> '');

	if (iID < 10) then
		raise Exception.Create (cID_Msg);

	TaskDlgRadioButtons.AddButton (iID, sButtonText, bDefault);
end; { TTaskDlg.AddRadioButton }

(* ---- *)

function TTaskDlg.Execute (const hParent: HWnd = 0) : Boolean;

var
	hResult : System.HResult;
	sError : String;

begin
	Assert (FhTaskDlg = 0);

	hResult := ShowTaskDialog (hParent);

	if (hResult <> S_OK) then
	begin
		case hResult of
			E_OUTOFMEMORY : sError := String (cE_OutOfMemory);
			E_INVALIDARG : sError := String (cE_InvalidArg);
			else sError := String (cE_Fail);
		end; { case }

		raise ETaskDlgError.Create (sError{%H-});
	end { if }
    else
    begin
        if (bDialogTimedOut) then
        begin
            Result := false;
            FModalResult := idTimedOut;
        end { if }
        else Result := FModalResult <> idCancel;
    end; { else }
end; { TTaskDlg.Execute }

(* ---- *)

function TTaskDlg.ClickButton (const tcbButton: TTaskDlgCommonButton) : Boolean;
begin
	Result := ClickButton (GetButtonID (tcbButton));
end; { TTaskDlg.ClickButton }

(* ---- *)

function TTaskDlg.ClickButton (const uButton_ID: UInt) : Boolean;
begin
	Assert (FhTaskDlg <> 0);

	Result := SendMessage (FhTaskDlg,
    					   TDM_CLICK_BUTTON, WParam (uButton_ID), 0) <> 0;
end; { TTaskDlg.ClickButton }

(* ---- *)

procedure TTaskDlg.ClickRadioButton (const iButton_ID: Integer);
begin
	Assert (FhTaskDlg <> 0);

	SendMessage (FhTaskDlg, TDM_CLICK_RADIO_BUTTON, iButton_ID, 0);
end; { TTaskDlg.ClickRadioButton }

(* ---- *)

procedure TTaskDlg.ClickVerification (const bCheck, bFocus: Boolean);
begin
	Assert (FhTaskDlg <> 0);

	SendMessage (FhTaskDlg, TDM_CLICK_VERIFICATION,
				 WPARAM (bCheck), LPARAM (bFocus));
end; { TTaskDlg.ClickVerification }

(* ---- *)

procedure TTaskDlg.EnableButton (const dwButton_ID: DWord;
								 const bEnable: Boolean);
begin
	Assert (FhTaskDlg <> 0);
	SendMessage (FhTaskDlg, TDM_ENABLE_BUTTON, dwButton_ID, LPARAM (bEnable));
end; { TTaskDlg.EnableButton }

(* ---- *)

procedure TTaskDlg.EnableButton (const tcbButton: TTaskDlgCommonButton;
  								 const bEnable: Boolean);
begin
	EnableButton (GetButtonID (tcbButton), bEnable);
end; { TTaskDlg.EnableButton }

(* ---- *)

procedure TTaskDlg.EnableRadioButton (const iButton_ID: Integer;
									  const bEnable: Boolean);
begin
	Assert (FhTaskDlg <> 0);

	SendMessage (FhTaskDlg, TDM_ENABLE_RADIO_BUTTON,
				 iButton_ID, LPARAM (bEnable));
end; { TTaskDlg.EnableRadioButton }

(* ---- *)

function TTaskDlg.GetRadioBtnText (const iIndex: Integer): String;
begin
    Assert (iIndex >= 0);

    if (iIndex > Pred (TaskDlgRadioButtons.Count)) then
        raise ETaskDlgError.CreateFmt (cRadioBtnIndexErr, [iIndex]);

    Result := String (TaskDlgRadioButtons.aButtonText [iIndex]);
end; { TTaskDlg.GetRadioBtnText }

(* ---- *)

procedure TTaskDlg.ProgressBarStepBy (const uStepBy: UInt);
begin
	Assert (FhTaskDlg <> 0);
	Assert ((tdfShowProgressBar in FFlags) or
			(tdfShowMarqueeProgressBar in FFlags));

	if (uStepBy = 0) then
		FProgressBarPos := 0
	else Inc (FProgressBarPos, uStepBy);

	SendMessage (FhTaskDlg, TDM_SET_PROGRESS_BAR_POS, FProgressBarPos, 0);
end; { TTaskDlg.ProgressBarStepBy }

(* ---- *)

procedure TTaskDlg.SetButtonElevationRequiredState (const iButton_ID: Integer;
													const bRequired: Boolean);
begin
	Assert (FhTaskDlg <> 0);

	SendMessage (FhTaskDlg, TDM_SET_BUTTON_ELEVATION_REQUIRED_STATE,
				 iButton_ID, LPARAM (bRequired));
end; { TTaskDlg.SetButtonElevationRequiredState }

(* ---- *)

procedure TTaskDlg.SetProgressBarRange (const wMin, wMax: Word);
begin
	Assert (FhTaskDlg <> 0);

	SendMessage (FhTaskDlg, TDM_SET_PROGRESS_BAR_MARQUEE,
				 0, MakeLPARAM (wMin, wMax))
end; { TTaskDlg.SetProgressBarRange }

(* ---- *)

procedure TTaskDlg.SetVerificationText (const sVerificationText: String);
begin
    FVerificationText := {$IFNDEF UNICODE} WideString {$ENDIF} (sVerificationText);
end; { TTaskDlg.SetVerificationText }

(* ---- *)

end.

