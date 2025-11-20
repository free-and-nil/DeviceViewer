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

{$WARN SYMBOL_PLATFORM OFF}

unit NativeApiToolsU;

{$MINENUMSIZE 4}

interface

uses Windows;

type
// http://www.geoffchappell.com/studies/windows/km/ntoskrnl/api/ex/sysinfo/secureboot.htm
    PTSecureBootStatus = ^TSecureBootStatus;
    TSecureBootStatus = record
        fEnabled, fCapable : Boolean;
    end; { TSecureBootStatus }

function CPU_Utilization (out dUtilization: Double) : Boolean;

function IsWindows10 : Boolean;

procedure Get_OS_Version (out dwMajorVer, dwMinorVer: DWord); overload;
function Get_OS_Version (out dwMinorVer: DWord) : DWord; overload;

function GetRegKeyFromRegHandle (const RegHandle: HKey;
								 out sKey: String) : Boolean;

function SecureBootStatus (out SBS: TSecureBootStatus) : Boolean;

implementation

uses SysUtils,
	 NativeApi_ImportU, PasTools, CPU_CountU;

(* ---- *)

function CPU_Utilization (out dUtilization: Double) : Boolean;

	(* ---- *)

    function LargeInt2Double (const LI: LARGE_INTEGER) : Double;
{$IFDEF SUPPORTS_INLINE}
	{$IFNDEF DEBUG}
		inline;
    {$ENDIF}
{$ENDIF}
	begin
  		Result := LI.HighPart * 4.294967296E9 + LI.LowPart
	end; { LargeInt2Double }

    (* ---- *)

const
    uCpuCount : UInt = 0;
    liOldIdleTime : LARGE_INTEGER = ({%H-});
    liOldSystemTime : LARGE_INTEGER = ({%H-});

var
	dSystemTime, dIdleTime : Double;
    SysPerfInfo : TSystemPerformanceInformation;
    SysTimeInfo : TSystemTimeOfDayInformation;

begin { Get_CPU_Utilization }
	Result := false;

    // get new system time
    if (NtQuerySystemInformation (SystemTimeOfDayInformation, @SysTimeInfo,
    							  SizeOf (SysTimeInfo), NIL) <> 0) then
    	exit;

    // get new CPU's idle time
    if (NtQuerySystemInformation (SystemPerformanceInformation, @SysPerfInfo,
    							  SizeOf (SysPerfInfo), NIL) <> 0) then
    	exit;

    // if it's a first call - skip it
    if (uCpuCount <> 0) then
    begin
        // CurrentValue = NewValue - OldValue
        dIdleTime := LargeInt2Double (SysPerfInfo.IdleTime) -
        			 LargeInt2Double (liOldIdleTime);
        dSystemTime := LargeInt2Double (SysTimeInfo.CurrentTime) -
                       LargeInt2Double(liOldSystemTime);

        // CurrentCpuIdle = IdleTime / SystemTime
        dIdleTime := dIdleTime / dSystemTime;

        // CurrentCpuUsage% = 100 - (CurrentCpuIdle * 100) / NumberOfProcessors
        dUtilization := 100.0 - (dIdleTime * 100.0) / uCpuCount + 0.5;
    end { if }
    else
    begin
    	uCpuCount := AvailableProcessorCount;

        liOldIdleTime.QuadPart := 0;
        liOldSystemTime.QuadPart := 0;
    end; { else }

    // store new CPU's idle and system time
    liOldIdleTime := SysPerfInfo.IdleTime;
    liOldSystemTime := SysTimeInfo.CurrentTime;

    Result := true;
end; { Get_CPU_Utilization }

(* ---- *)

function IsWindows10 : Boolean;

var
    dwMajorVer, dwMinorVer : DWord;

begin
	if (Assigned (RtlGetVersion)) then
    begin
	    Get_OS_Version (dwMajorVer, dwMinorVer);
    	Result := dwMajorVer >= 10;
    end { if }
    else Result := false;
end; { IsWindows10 }

(* ---- *)

procedure Get_OS_Version (out dwMajorVer, dwMinorVer: DWord);

var
    VI : TOSVersionInfoW;

begin
    VI.dwOSVersionInfoSize := SizeOf (TOSVersionInfoW);

    Win32Check (RtlGetVersion (VI) = STATUS_SUCCESS);

    dwMajorVer := VI.dwMajorVersion;
    dwMinorVer := VI.dwMinorVersion;
end; { Get_OS_Version }

(* ---- *)

function Get_OS_Version (out dwMinorVer: DWord) : DWord;
begin
    Get_OS_Version (Result, dwMinorVer);
end; { Get_OS_Version }

(* ---- *)

function GetRegKeyFromRegHandle (const RegHandle: HKey;
								 out sKey: String) : Boolean;

var
	uResultLen : ULong;
    pKeyNameInformation : PTKeyNameInformation;
    Status : NTSTATUS;
    iLen : Integer;

begin
	Result := false;

    if not (Assigned (NtQueryKey)) then
    	exit;

    if (NtQueryKey (RegHandle, KeyNameInformation, NIL, 0,
    				@uResultLen) <> STATUS_BUFFER_TOO_SMALL) then
    	exit;

    pKeyNameInformation := MemAlloc (uResultLen);

    try
        Status := NtQueryKey (RegHandle, KeyNameInformation,
    					      pKeyNameInformation, uResultLen, @uResultLen);

        if (Status = STATUS_SUCCESS) then
        begin
		    Result := true;
            iLen := pKeyNameInformation^.NameLength div SizeOf (WideChar);
            SetLength (sKey{%H-}, iLen - 1);
{$IFDEF UNICODE}
		    lstrcpynw (PChar (sKey), @pKeyNameInformation^.Name [0], iLen);
{$ELSE}
		    WideCharToMultiByte (0, 0, @pKeyNameInformation^.Name [0], iLen,
                                 PChar (sKey), iLen, NIL, NIL);
{$ENDIF}
        end; { if }

    finally
    	MemDispose (Pointer (pKeyNameInformation));
    end; { try / finally }
end; { GetRegKeyFromRegHandle }

(* ---- *)

function SecureBootStatus (out SBS: TSecureBootStatus) : Boolean;
// Only implemented as a 64 bit function; requires local admin rights
// http://www.geoffchappell.com/studies/windows/km/ntoskrnl/api/ex/sysinfo/secureboot.htm

var
    pSBS : PTSecureBootStatus;
    uReturn : ULong;

begin
    Result := false;

{$IFDEF DEBUG}
    {$R-}
{$ENDIF}
    if (NtQuerySystemInformation ({%H-}TSystemInformationClass ($91), NIL, 0,
                                  @uReturn) = STATUS_INFO_LENGTH_MISMATCH) then
{$IFDEF DEBUG}
    {$R+}
{$ENDIF}
    begin
        GetMem (pSBS, uReturn);

        try
{$IFDEF DEBUG}
    {$R-}
{$ENDIF}
            if (NtQuerySystemInformation ({%H-}TSystemInformationClass ($91),
                                          pSBS, uReturn,
                                          @uReturn) = STATUS_SUCCESS) then
{$IFDEF DEBUG}
    {$R+}
{$ENDIF}
            begin
                Result := true;
                SBS := pSBS^;
            end; { if }

        finally
            FreeMem (pSBS);
        end; { try / finally }
    end; { if }
end; { SecureBootStatus }

(* ---- *)

end.

