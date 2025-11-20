{$I ..\switches.inc}

{$IFDEF FPC}
    {$ASMMODE INTEL}
	{$MODE DELPHI}
{$ELSE}
    {$WARNINGS OFF}
	{$HINTS OFF}
	{$OPTIMIZATION OFF}
{$ENDIF}

{$ALIGN 4}

{$WARN SYMBOL_PLATFORM OFF}

unit CPU_CountU;

interface

uses Windows,
	 WinXP_ImportU;

const
    cAuthenticAMD = 'AuthenticAMD';
    cGenuineIntel = 'GenuineIntel';

function AvailableProcessorCount : DWord;
function AvailableProcessorCoreCount : DWord;
function GetLogicalProcessorInfo (out uNumaCount, uCoreCount, uL1CacheSize,
                                  uL2CacheSize, uL3CacheSize,
                                  uProcessorPackageCount,
                                  uLogicalProcessorCount: UInt) : Boolean;
                                                                       overload;
function GetLogicalProcessorInfo (out uCoreCount, uProcessorPackageCount,
                                      uLogicalProcessorCount: UInt) : Boolean;
                                                                       overload;

function GetHybridFlag : Boolean;

function Get_P_E_LPE_CoreCount (var uP_Cores, uE_Cores, uLPE: UInt) : Boolean;

implementation

uses SysUtils,
	 ProcessorInfoU, SortedListU;

const
	cEAX = 1;
    cEBX = 2;
    cECX = 3;
    cEDX = 4;

type
	TCPU_ID = array [1..4] of Cardinal;  // AX, BX, CX, DX

{$IFNDEF FPC}
    {$IFNDEF DELPHI2007_UP}
        DWORD_PTR = Cardinal;
    {$ENDIF}
{$ENDIF}

var
    VendorID : array [1..12] of AnsiChar;

(* ---- *)

(**

See "GetLogicalProcessorInformation" for Server 2003, XP with SP3 / Vista

http://blogs.msdn.com/oldnewthing/archive/2005/12/16/504659.aspx

re: When hyperthreading is enabled, all the processors are virtual
Friday, December 16, 2005 11:04 AM by David Heffernan
Intel did add to CPUID to check whether HT is available on the processor. The
problem is that it might be on the processor but disabled by the BIOS or the OS.

So the code on Intel's site is what you need. It just so happens that I was
porting this code to my app this week. This is what I came up with:
**)

function AvailableProcessorCount : DWord;
{ returns total number of processors available to system including logical
  hyperthreaded processors }

var
    iIndex : Integer;
    dwMask : {$IFDEF CPUX64} DWORD_PTR {$ELSE} DWord {$ENDIF};
    ProcessAffinityMask, SystemAffinityMask : DWORD_PTR;

begin
    if (GetProcessAffinityMask (GetCurrentProcess, ProcessAffinityMask{%H-},
    						    SystemAffinityMask{%H-})) then
    begin
	    Result := 0;

    	for iIndex := 0 to {$IFDEF CPUX64} 63 {$ELSE} 31 {$ENDIF} do
//    	for iIndex := 0 to 31 do
        begin
		    dwMask := ({$IFDEF CPUX64} NativeUInt {$ENDIF} (1)) shl iIndex;

    		if ((ProcessAffinityMask and dwMask) <> 0) then
			    Inc (Result);
    	end; { for }
    end { if }
    else { can't get the affinity mask so we report the total number of CPUs }
    	Result := GetNumberOfProcessors;
end; (* AvailableProcessorCount *)

(* ---- *)

{ With the 64 bit compiler these assembly functions must not be subfunctions,
  as that is going to lead to incorrect return values }

function CpuId_GetMaxBasicCpuIdLeaf : TCPU_ID;
{ INPUT EAX = 0: Returns CPUID's Highest Value for Basic Processor
  Information and the Vendor Identification String }
asm
{$IFNDEF CPUX64}
    push  ebx
    push  edi
    mov   edi, eax
    mov   eax, 0
    cpuid
    mov   [edi+$0], eax
    mov   [edi+$4], ebx
    mov   [edi+$8], ecx
    mov   [edi+$c], edx
    pop   edi
    pop   ebx
{$ELSE}
    mov   r8, rbx
    mov   r9, rcx
    mov   eax, 0
    cpuid
    mov   [r9+$0], eax
    mov   [r9+$4], ebx
    mov   [r9+$8], ecx
    mov   [r9+$c], edx
    mov   rbx, r8
{$ENDIF}
end; { CpuId_GetMaxBasicCpuIdLeaf }

(* ---- *)

function CpuId_GetModelFamilyStepping  : TCPU_ID;
{ INPUT EAX = 1: Returns Model, Family, Stepping Information }
asm
{$IFNDEF CPUX64}
    push  ebx
    push  edi
    mov   edi, eax
    mov   eax, 1
    cpuid
    mov   [edi+$0], eax
    mov   [edi+$4], ebx
    mov   [edi+$8], ecx
    mov   [edi+$c], edx
    pop   edi
    pop   ebx
{$ELSE}
    mov   r8, rbx
    mov   r9, rcx
    mov   eax, 1
    cpuid
    mov   [r9+$0], eax
    mov   [r9+$4], ebx
    mov   [r9+$8], ecx
    mov   [r9+$c], edx
    mov   rbx, r8
{$ENDIF}
end; { CpuId_GetModelFamilyStepping }

(* ---- *)

function CpuId_GetCPUFeatures : TCPU_ID;
asm
{$IFNDEF CPUX64}
    push  ebx
    push  edi
    mov   edi, eax
    mov   eax, 4
    mov   ecx, 0
    cpuid
    mov   [edi+$0], eax
    mov   [edi+$4], ebx
    mov   [edi+$8], ecx
    mov   [edi+$c], edx
    pop   edi
    pop   ebx
{$ELSE}
    mov   r8, rbx
    mov   r9, rcx
    mov   eax, 4
    mov	  ecx, 0
    cpuid
    mov   [r9+$0], eax
    mov   [r9+$4], ebx
    mov   [r9+$8], ecx
    mov   [r9+$c], edx
    mov   rbx, r8
{$ENDIF}
end; { CpuId_GetCPUFeatures }

(* ---- *)

function CpuId_GetHybridFlag : TCPU_ID;
asm
{$IFNDEF CPUX64}
    push  ebx
    push  edi
    mov   edi, eax
    mov   eax, $7
    mov   ecx, 0
    cpuid
    mov   [edi+$0], eax
    mov   [edi+$4], ebx
    mov   [edi+$8], ecx
    mov   [edi+$c], edx
    pop   edi
    pop   ebx
{$ELSE}
    mov   r8, rbx
    mov   r9, rcx
    mov   eax, $7
    mov	  ecx, 0
    cpuid
    mov   [r9+$0], eax
    mov   [r9+$4], ebx
    mov   [r9+$8], ecx
    mov   [r9+$c], edx
    mov   rbx, r8
{$ENDIF}
end; { CpuId_GetHybridFlag }

(* ---- *)

function CpuId_GetCoreType : TCPU_ID;
asm
{$IFNDEF CPUX64}
    push  ebx
    push  edi
    mov   edi, eax
    mov   eax, $1A
    mov   ecx, 0
    cpuid
    mov   [edi+$0], eax
    mov   [edi+$4], ebx
    mov   [edi+$8], ecx
    mov   [edi+$c], edx
    pop   edi
    pop   ebx
{$ELSE}
    mov   r8, rbx
    mov   r9, rcx
    mov   eax, $1A
    mov	  ecx, 0
    cpuid
    mov   [r9+$0], eax
    mov   [r9+$4], ebx
    mov   [r9+$8], ecx
    mov   [r9+$c], edx
    mov   rbx, r8
{$ENDIF}
end; { CpuId_GetCoreType }

(* ---- *)

function CpuId_GetCacheInfo : TCPU_ID;
asm
{$IFNDEF CPUX64}
    push  ebx
    push  edi
    mov   edi, eax
    mov   eax, 4
    mov   ecx, 3
    cpuid
    mov   [edi+$0], eax
    mov   [edi+$4], ebx
    mov   [edi+$8], ecx
    mov   [edi+$c], edx
    pop   edi
    pop   ebx
{$ELSE}
    mov   r8, rbx
    mov   r9, rcx
    mov   eax, 4
    mov	  ecx, 3
    cpuid
    mov   [r9+$0], eax
    mov   [r9+$4], ebx
    mov   [r9+$8], ecx
    mov   [r9+$c], edx
    mov   rbx, r8
{$ENDIF}
end; { CpuId_GetCacheInfo }

(* ---- *)

function AvailableProcessorCoreCount : DWord;
(*
Returns total number of processors available to system excluding logical
hyperthreaded processors. We only have to do significant work for Intel
processors since they are the only ones which implement hyperthreading.

It's not 100% clear whether the hyperthreading bit (CPUID(1) -> EDX[28]) will be
set for processors with multiple cores but without hyperthreading. My reading of
the documentation is that it will be set but the code is conservative and
performs the APIC ID decoding if either:

1. The hyperthreading bit is set, or
2. The processor reports >1 cores on the physical package.

If either of these conditions hold then we proceed to read the APIC ID for each
logical processor recognised by the OS. This ID can be decoded to the form
(PACKAGE_ID, CORE_ID, LOGICAL_ID) where PACKAGE_ID identifies the physical
processor package, CORE_ID identifies a physical core on that package and
LOGICAL_ID identifies a hyperthreaded processor on that core.

The job of this routine is therefore to count the number of unique cores, that
is the number of unique pairs (PACKAGE_ID, CORE_ID).

If the chip is not an Intel processor, or if it is Intel but doesn't have
multiple logical processors on a physical package then the routine simply
returns AvailableProcessorCount.
*)

    (* ---- *)

    function ProcessorPackageSupportsLogicalProcessors : Boolean;

        (* ---- *)

(**
        type
            TVendor_ID = array [1..12] of AnsiChar;

        function Get_CPU_VendorID : TVendor_ID;
        asm
        {$IFNDEF CPUX64}
            push  ebx
            push  edi
            mov   edi, eax
            mov   eax, 0
            cpuid
            mov   [edi+$0], ebx
            mov   [edi+$4], edx
            mov   [edi+$8], ecx
            pop   edi
            pop   ebx
        {$ELSE}
            mov   r8, rbx
            mov   r9, rcx
            mov   eax, 0
            cpuid
            mov   [r9+$0], ebx
            mov   [r9+$4], edx
            mov   [r9+$8], ecx
            mov   rbx, r8
        {$ENDIF}
        end; { Get_CPU_VendorID }
**)

        (* ---- *)

    const
        HT_BIT = $10000000;
(**
		FAMILY_ID = $00000F00;
        EXT_FAMILY_ID = $00F00000;
        PENTIUM4_ID = $00000F00;
**)

    var
        CPU_ID : TCPU_ID;
//        ProcessorSupportsHT: Boolean;

    begin { ProcessorPackageSupportsLogicalProcessors }
        Result := false;


//      if (VendorID = 'GenuineIntel') then
        begin  // AMD CPUs these days support Hyperthreading as well ...
	        CPU_ID := CpuId_GetModelFamilyStepping;

	        if ((CPU_ID [cEDX] and HT_BIT) <> 0) then
		        Result := True;
        end; { if }
    end; (* ProcessorPackageSupportsLogicalProcessors *)

    (* ---- *)

    function GetLogicalProcessorCountPerPackage : DWord;

    const
	    NUM_LOGICAL_BITS = $00FF0000;

    var
        CPU_ID : TCPU_ID;

    begin
    	CPU_ID := CpuId_GetModelFamilyStepping;

	    Result := ((CPU_ID [cEBX] and NUM_LOGICAL_BITS) shr 16);
    end; (* GetLogicalProcessorCountPerPackage *)

    (* ---- *)

    function GetMaxCoresPerPackage : DWord;

    var
        CPU_ID : TCPU_ID;

    begin
    	CPU_ID := CpuId_GetMaxBasicCpuIdLeaf;

        if CPU_ID [cEAX] >= 4 then
        begin
        	CPU_ID := CpuId_GetCPUFeatures;
            Result := (CPU_ID [cEAX] shr 26) + 1;
        end
        else Result := 1;
    end; (* GetMaxCoresPerPackage *)

    (* ---- *)

    function GetAPIC_ID: DWord;

    var
        CPU_ID : TCPU_ID;

    begin
    	CPU_ID := CpuId_GetModelFamilyStepping;

	    Result := CPU_ID [cEBX] shr 24;
    end; (* GetAPIC_ID *)

    (* ---- *)

var
    i : Integer;
    PackCoreList : TIntegerList;
    ThreadHandle : THandle;
    LogicalProcessorCountPerPackage, MaxCoresPerPackage, LogicalPerCore, APIC_ID,
    // {%H-}PACKAGE_ID, {%H-}CORE_ID, {%H-}LOGICAL_ID,
    PACKAGE_CORE_ID, LOGICAL_ID_MASK, { LOGICAL_ID_SHIFT, CORE_ID_MASK, CORE_ID_SHIFT, }
    ProcessAffinityMask, SystemAffinityMask, ThreadAffinityMask, Mask : DWORD_PTR;

begin { AvailableProcessorCoreCount }
    Result := 0;

    Try
        //see Intel documentation for details on logical processor topology
        if (Win32Platform = VER_PLATFORM_WIN32_NT) then
        begin
            MaxCoresPerPackage := GetMaxCoresPerPackage;

            if (ProcessorPackageSupportsLogicalProcessors) or
               (MaxCoresPerPackage > 1) then
            begin
                LogicalProcessorCountPerPackage := GetLogicalProcessorCountPerPackage;
                LogicalPerCore := LogicalProcessorCountPerPackage div MaxCoresPerPackage;

                LOGICAL_ID_MASK := $FF;
                i := 1;

                while (i < Integer (LogicalPerCore)) do
                begin
                    i := i * 2;
                    LOGICAL_ID_MASK := LOGICAL_ID_MASK shl 1;
                end; { while }

                if (MaxCoresPerPackage > 1) then
                begin
                    i := 1;

                    while (i < Integer (MaxCoresPerPackage)) do
                        i := i * 2;
                end; { if }

                LOGICAL_ID_MASK := not LOGICAL_ID_MASK;

                if (GetProcessAffinityMask (GetCurrentProcess,
                                            ProcessAffinityMask{%H-},
                                            SystemAffinityMask{%H-})) then
                begin
                    ThreadHandle := GetCurrentThread;
                    //get the current thread affinity
                    ThreadAffinityMask := SetThreadAffinityMask (ThreadHandle,
                          								   ProcessAffinityMask);

                    if (ThreadAffinityMask <> 0) then
                        Try
                            PackCoreList := TIntegerList.Create;

                            Try
                                for i := 0 to {$IFNDEF CPUX64} 31 {$ELSE} 63 {$ENDIF} do
                                begin
                                    Mask := {$IFDEF CPUX64} NativeUInt {$ENDIF} (1) shl i;

                                    if ((ProcessAffinityMask and Mask) <> 0) and
                                       (SetThreadAffinityMask (ThreadHandle,
                                                               Mask) <> 0) then
                                    begin
                                        // allow OS to reschedule thread onto
                                        // the selected processor
                                        Sleep(0);
                                        APIC_ID := GetAPIC_ID;
//                                        LOGICAL_ID := APIC_ID and LOGICAL_ID_MASK;
//                                        CORE_ID := (APIC_ID and CORE_ID_MASK) shr LOGICAL_ID_SHIFT;
//                                        PACKAGE_ID := APIC_ID shr (LOGICAL_ID_SHIFT + CORE_ID_SHIFT);
                                        PACKAGE_CORE_ID := APIC_ID and (not LOGICAL_ID_MASK);  //mask out LOGICAL_ID
                                        //identifies the processor core - it's not a value defined by Intel, rather it's defined by us!

                                        if (PackCoreList.IndexOf ({%H-}Pointer (Integer (PACKAGE_CORE_ID))) = (-1)) then
                                            //count the number of unique processor cores
                                            PackCoreList.Add (PACKAGE_CORE_ID);
                                    end; { if }
                                end; { for }

                                Result := PackCoreList.Count;

                            finally
                                FreeAndNil(PackCoreList);
                            end; { try / finally }

                        finally
                            //restore thread affinity
                            SetThreadAffinityMask (ThreadHandle, ThreadAffinityMask);
                        end; { try / finally }
                end; { if }
            end; { if }
        end; { if }

    except
	    ;//some processors don't support CPUID and so will raise exceptions when it is called
    end; { try / except }

    if (Result = 0) then
	    //if we haven't modified Result above, then assume that all logical
        // processors are true physical processor cores
    	Result := AvailableProcessorCount;
end; { AvailableProcessorCoreCount }

(* ---- *)

function GetLogicalProcessorInfo (out uNumaCount, uCoreCount, uL1CacheSize,
                                  uL2CacheSize, uL3CacheSize,
                                  uProcessorPackageCount,
                                  uLogicalProcessorCount: UInt) : Boolean;

	(* ---- *)

    function GetActiveProcessors (const dwMask: DWORD_PTR) : DWord;

    var
    	uIndex : Cardinal;

    begin
        Result := 0;

{$HINTS OFF}
        for uIndex := 0 to (SizeOf (dwMask) * 8) - 1 do
        	if (((1 shl uIndex) and dwMask) = (1 shl uIndex)) then
            	Inc (Result);
{$HINTS ON}
    end; { GetActiveProcessors }

    (* ---- *)

type
	PTaSLPI = ^TaSLPI;
  	TaSLPI = array [0..0] of TSystemLogicalProcessorInformation;

var
	paSLPI : PTaSLPI;
    dwReturnLength : DWord;
    iIndex, iCount : Integer;

begin { GetLogicalProcessorInfo }
	Result := false;

    if not (Assigned (@GetLogicalProcessorInformation)) then
    	exit;

	uNumaCount := 0;
    uCoreCount := 0;
    uL1CacheSize := 0;
    uL2CacheSize := 0;
    uL3CacheSize := 0;
    uProcessorPackageCount := 0;
    uLogicalProcessorCount := 0;

    dwReturnLength := 0;

    if (GetLogicalProcessorInformation (NIL, dwReturnLength) = false) and
       (GetLastError <> ERROR_INSUFFICIENT_BUFFER) then
    	RaiseLastOSError;

    GetMem (paSLPI, dwReturnLength);

    try
		Win32Check (GetLogicalProcessorInformation (Pointer (paSLPI),
        											dwReturnLength));

        iCount := dwReturnLength div SizeOf (TSystemLogicalProcessorInformation);

{$IFDEF DEBUG}
    {$RANGECHECKS OFF}
{$ENDIF}

        for iIndex := 0 to iCount - 1 do
        	with paSLPI^ [iIndex] do
            	case Relationship of
                    RelationProcessorCore:
                    	begin
                      		Inc (uCoreCount);
                            Inc (uLogicalProcessorCount,
                            	 GetActiveProcessors (ProcessorMask));
                        end; { case RelationProcessorCore }

                    RelationNumaNode : Inc (uNumaCount);

                    RelationCache :
                        case Cache.Level of
                        	1 : Inc (uL1CacheSize, Cache.Size);
                            2 : Inc (uL2CacheSize, Cache.Size);
                            3 : Inc (uL3CacheSize, Cache.Size);
                        end; { case Cache of }

                    RelationProcessorPackage : Inc (uProcessorPackageCount);

(**
                    RelationGroup: ;
                    RelationAll: ;
                    else ;
**)
                end; { case Relationship of }

		Result := true;

{$IFDEF DEBUG}
    {$RANGECHECKS ON}
{$ENDIF}

    finally
    	FreeMem (paSLPI);
    end; { try / finally }
end; { GetLogicalProcessorInfo }

(* ---- *)

function GetLogicalProcessorInfo (out uCoreCount, uProcessorPackageCount,
                              		  uLogicalProcessorCount: UInt) : Boolean;

var
	uNumaCount, uL1CacheSize, uL2CacheSize, uL3CacheSize : UInt;

begin
	Result := GetLogicalProcessorInfo (uNumaCount, uCoreCount, uL1CacheSize,
                             		   uL2CacheSize, uL3CacheSize,
                                       uProcessorPackageCount,
                                       uLogicalProcessorCount);

    if (Result) then
    begin
    	if (uProcessorPackageCount = 0) then
        	uProcessorPackageCount := uNumaCount;

        if (uLogicalProcessorCount > (uCoreCount * 2)) then
        	uCoreCount := uLogicalProcessorCount div 2;
    end; { if }
end; { GetLogicalProcessorInfo}

(* ---- *)

function GetHybridFlag : Boolean;

var
    HybridFlag : TCPU_ID;

begin
    if (VendorID = cGenuineIntel) then
    begin
        HybridFlag := CpuId_GetHybridFlag;

        Result := (HybridFlag [cEDX] and (1 shl 15)) <> 0;
    end { if }
    else Result := false;
end; { GetHybridFlag }

(* ---- *)

function Get_P_E_LPE_CoreCount (var uP_Cores, uE_Cores, uLPE: UInt) : Boolean;

var
    iCount, iIndex, iDelta : Integer;
    hProcess : THandle;
    CpuId_Res : TCPU_ID;
    byCoreInfo : Byte;
    lpProcessAffinityMask, lpSystemAffinityMask, lpAffinityMask : DWORD_PTR;

begin
    Result := false;

	uP_Cores := 0;
    uE_Cores := 0;
    uLPE     := 0;

    if not (GetHybridFlag) then
        exit;

    hProcess := GetCurrentProcess;

    iCount := AvailableProcessorCount;
    iDelta := iCount - Integer (AvailableProcessorCoreCount);

    Win32Check (GetProcessAffinityMask (hProcess,
                                        lpProcessAffinityMask{%H-},
                                        lpSystemAffinityMask{%H-}));

    for iIndex := 0 to iCount - 1 do
    begin
        lpAffinityMask := 1 SHL iIndex;

        Win32Check (SetThreadAffinityMask (GetCurrentThread,
                                 lpProcessAffinityMask AND lpAffinityMask) > 0);

        CpuId_Res := CpuId_GetCoreType;
        byCoreInfo := LongRec (CpuId_Res [1]).Bytes [3];

(**
        case byCoreInfo of
            $20 : WriteLn ('Intel Atom');
            $40 : WriteLn ('Intel Core');
        	else WriteLn ('Unknown value');
        end; { case byCoreInfo of }
**)

        if (byCoreInfo = $20) then
        begin
            CpuId_Res := CpuId_GetCacheInfo;

            if (CpuId_Res [cEAX] = 0) then
                Inc (uLPE)  // Low Power Cores do not have a cache
            else Inc (uE_Cores)
        end { if }
        else if (byCoreInfo = $40) then
            Inc (uP_Cores);
    end; { if }

    if (iDelta > 0) then
        Dec (uP_Cores, iDelta);

    Result := true;
end; { Get_P_E_LPE_CoreCount }

(* ---- *)

procedure GetVendorId;

var
    CPU_ID : TCPU_ID;

begin
    CPU_ID := CpuId_GetMaxBasicCpuIdLeaf;

    if (CPU_ID [cEAX] < 1) then
    begin
        FillChar (VendorID, SizeOf (VendorID), #0);
        exit;
    end; { if }

(**
    asm
        PUSH EBX

        //call CPUID with EAX=0 and record the result in VendorID
        MOV EAX,0
        CPUID

        // test the maximum basic CPUID leaf and quit if it's less than 1
        // which we need below
        CMP EAX,1
        JL @@quit

        //record Vendor ID
        MOV [DWord PTR VendorID+0],EBX
        MOV [DWord PTR VendorID+4],EDX
        MOV [DWord PTR VendorID+8],ECX

        //call CPUID with EAX=1 and record the EDX register
        MOV EAX,1
        CPUID
        MOV RegEDX,EDX

        @@quit:
        POP EBX
    end;
**)

    move (CPU_ID [cEBX], {%H-}VendorID [1], 4);
    move (CPU_ID [cEDX], VendorID [5], 4);
    move (CPU_ID [cECX], VendorID [9], 4);
end; { GetVendorId }

(* ---- *)

initialization
    GetVendorId;
end.
