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

{$OPTIMIZATION OFF}

{$ALIGN 4}

unit ProcessorInfoU;

interface

uses Windows;

{$IFNDEF CPUX64}
type
  TProcessorInfo = record
    MaximumBasicFunction    : Cardinal;
    MaximumExtendedFunction : Cardinal;
    VendorID                : array [0..12] of Char;
    Signature               : Cardinal;
    SupportsMMX             : Boolean;
    SupportsSSE             : Boolean;
    SupportsSSE2            : Boolean;
    SupportsSSE3            : Boolean;
    SupportsHyperThreading  : Boolean;
    SupportsIA64            : Boolean;
    SupportsXDBit           : Boolean;
    SupportsEMT64           : Boolean;
  end;
{$ENDIF CPUX64}

function GetNumberOfProcessors : UInt;

{$IFNDEF CPUX64}
function ReadProcessorInfo (var ProcessorInfo: TProcessorInfo) : Boolean;
	register;
{$ENDIF CPUX64}

implementation

(* ---- *)

function GetNumberOfProcessors : UInt;

var
	SystemInfo : TSystemInfo;

begin
	GetSystemInfo (SystemInfo{%H-});

    Result := SystemInfo.dwNumberOfProcessors;
end; { GetNumberOfProcessors }

(* ---- *)

{$IFNDEF CPUX64}
function ReadProcessorInfo (var ProcessorInfo: TProcessorInfo) : Boolean;
	register;

var
    MBF,MEF     : Cardinal;
    Sig         : Cardinal;
    A,B,C       : Cardinal;
    FF1,FF2,FF3 : Cardinal;

begin
    Result := false;

    FillChar (ProcessorInfo, SizeOf (TProcessorInfo), #0);
    ProcessorInfo.MaximumExtendedFunction := $80000000;

{$IFDEF SUPPORTS_REGION}
	{$REGION 'cpuid'}
{$ENDIF}
    try
        asm
            { save registers used by the register calling convention }
            push eax
            push ebx
            push ecx
            push edx
            { call CPUID }
            xor eax,eax
            cpuid
            mov MBF,eax
            { vendor ID }
            mov A,ebx
            mov B,edx
            mov C,ecx
            { signature & feature flags 1-2 }
            mov eax,1
            cpuid
            mov Sig,eax
            mov FF1,edx
            mov FF2,ecx
            { extended functions }
            mov eax,$8000000
            cpuid
            mov MEF,eax
            { feature flags 3 }
            mov eax,$8000001
            cpuid
            mov FF3,edx
            { restore }
            pop edx
            pop ecx
            pop ebx
            pop eax
        end;

    except
	    exit;
    end;
{$IFDEF SUPPORTS_REGION}
	{$ENDREGION}
{$ENDIF}

{$IFDEF SUPPORTS_REGION}
	{$REGION 'parse results'}
{$ENDIF}
    with ProcessorInfo do
    begin
        MaximumBasicFunction := MBF;
        MaximumExtendedFunction := MEF;
        Signature := Sig;
        VendorID[0] := Chr((A and $000000FF) shr 0);
        VendorID[1] := Chr((A and $0000FF00) shr 8);
        VendorID[2] := Chr((A and $00FF0000) shr 16);
        VendorID[3] := Chr((A and $FF000000) shr 24);

        VendorID[4] := Chr((B and $000000FF) shr 0);
        VendorID[5] := Chr((B and $0000FF00) shr 8);
        VendorID[6] := Chr((B and $00FF0000) shr 16);
        VendorID[7] := Chr((B and $FF000000) shr 24);

        VendorID[8] := Chr((C and $000000FF) shr 0);
        VendorID[9] := Chr((C and $0000FF00) shr 8);
        VendorID[10] := Chr((C and $00FF0000) shr 16);
        VendorID[11] := Chr((C and $FF000000) shr 24);
        VendorID[12] := #0;
    end; { with }
{$IFDEF SUPPORTS_REGION}
	{$ENDREGION}
{$ENDIF}

{$IFDEF SUPPORTS_REGION}
	{$REGION 'flags'}
{$ENDIF}
    with ProcessorInfo do
    begin
        SupportsMMX := (FF1 and (1 shl 23)) <> 0;
        SupportsSSE := (FF1 and (1 shl 25)) <> 0;
        SupportsSSE2 := (FF1 and (1 shl 26)) <> 0;
        SupportsSSE3 := (FF2 and (1 shl 0)) <> 0;
        SupportsHyperThreading := (FF1 and (1 shl 28)) <> 0;
        SupportsIA64 := (FF1 and (1 shl 30)) <> 0;
        SupportsXDBit := (FF3 and (1 shl 20)) <> 0;
        SupportsEMT64 := (FF3 and (1 shl 29)) <> 0;
    end; { with }
{$IFDEF SUPPORTS_REGION}
	{$ENDREGION}
{$ENDIF}

    Result := true;
end; { ReadProcessorInfo }
{$ENDIF CPUX64}

(* ---- *)

end.
