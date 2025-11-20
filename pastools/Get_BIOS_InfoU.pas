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

unit Get_BIOS_InfoU;

interface

uses Windows, Classes;

function Get_BiosInfo (out sVersion: String) : Boolean; overload;
function Get_BiosInfo (out sVersion, sSerialNumber: String;
					   out uReleaseDate: UInt) : Boolean; overload;
function Get_BiosInfo (out sVersion, sSerialNumber: String;
					   out uReleaseDate, uEmbeddedControllerMajor,
                           uEmbeddedControllerMinor: UInt) : Boolean; overload;
procedure Get_BiosInfo (const InfoList: TStrings;
                        const bAllInfo: Boolean = false); overload;
function Get_PC_Info (out sManufacturer, sModel, sVersion: String) : Boolean;
function GetEmbeddedControllerVersion (out uMajor, uMinor: UInt) : Boolean;

implementation

uses SysUtils, ActiveX, Variants, ComObj,
	 PasTools, NativeApiToolsU;

const
    cCimV2 = 'root\CIMV2';
    wbemFlagForwardOnly = $00000020;
    cLocalHost = 'localhost';
    cWQL = 'WQL';

var
    bWin10 : Boolean;

(* ---- *)

function Get_BiosInfo (out sVersion: String) : Boolean;

var
	sSerialNumber : String;
    uReleaseDate, uEmbeddedControllerMajor, uEmbeddedControllerMinor : UInt;

begin
	Result := Get_BiosInfo (sVersion, sSerialNumber, uReleaseDate,
    						uEmbeddedControllerMajor, uEmbeddedControllerMinor);
end; { Get_BiosInfo }

(* ---- *)

function Get_BiosInfo (out sVersion, sSerialNumber: String;
					   out uReleaseDate: UInt) : Boolean;

var
	uEmbeddedControllerMajor, uEmbeddedControllerMinor : UInt;

begin
	Result := Get_BiosInfo (sVersion, sSerialNumber, uReleaseDate,
    						uEmbeddedControllerMajor, uEmbeddedControllerMinor);
end; { Get_BiosInfo }

(* ---- *)

function Get_BiosInfo (out sVersion, sSerialNumber: String;
					   out uReleaseDate, uEmbeddedControllerMajor,
                                      uEmbeddedControllerMinor: UInt) : Boolean;
const
(**
    cQuery = 'SELECT ReleaseDate, SerialNumber, SMBIOSBIOSVersion FROM Win32_BIOS';
    cWQL_W10 = 'SELECT ReleaseDate, SerialNumber, SMBIOSBIOSVersion, ' +
               'EmbeddedControllerMajorVersion, EmbeddedControllerMinorVersion ' +
               'FROM Win32_BIOS';
**)
    cQuery = 'SELECT * FROM Win32_BIOS';
    WbemUser            ='';
    WbemPassword        ='';

var
    FSWbemLocator : OLEVariant;
    FWMIService   : OLEVariant;
    FWbemObjectSet: OLEVariant;
    FWbemObject   : OLEVariant;
    oEnum         : IEnumvariant;
    iValue        : LongWord;
    sReleaseDate  : String;

begin;
    Result := false;

    FSWbemLocator := CreateOleObject ('WbemScripting.SWbemLocator');
    FWMIService := FSWbemLocator.ConnectServer (cLocalHost, cCimV2,
                                                WbemUser, WbemPassword);
    FWbemObjectSet := FWMIService.ExecQuery (cQuery, cWQL, wbemFlagForwardOnly);
    oEnum := IUnknown (FWbemObjectSet._NewEnum) as IEnumVariant;

    if (oEnum.Next (1, FWbemObject, iValue) = 0) then
    begin
        Result := true;

        sVersion := TrimRight (String (FWbemObject.SMBIOSBIOSVersion){%H-});
        sSerialNumber := TrimRight (String (FWbemObject.SerialNumber){%H-});

        if (VarIsNull (FWbemObject.ReleaseDate)) then
        begin  // Surface Pro 4
        	sReleaseDate := '?';
            uReleaseDate := 0;
        end { if }
        else
        begin
        	sReleaseDate := TrimRight (String (FWbemObject.ReleaseDate){%H-});

            if (Length (sReleaseDate) > 8) then
                SetLength (sReleaseDate, 8);

            if not (Str2Int (sReleaseDate, Integer ({%H-}uReleaseDate))) then
                exit;
    	end; { else }

        if (bWin10) then
        begin
//            sValue := Trim (VarToStrDef (FWbemObject.EmbeddedControllerMajorVersion, '0'));
            uEmbeddedControllerMajor := FWbemObject{%H-}.EmbeddedControllerMajorVersion;
//            sValue := Trim (VarToStrDef (FWbemObject.EmbeddedControllerMinorVersion, '0'));
            uEmbeddedControllerMinor := FWbemObject{%H-}.EmbeddedControllerMinorVersion;
        end { if }
        else
        begin
            uEmbeddedControllerMajor := 0;
            uEmbeddedControllerMinor := 0;
        end; { else }

        FWbemObject := {%H-}Unassigned;
    end; { while }
end; { Get_PC_Info }

(* ---- *)

function Get_PC_Info (out sManufacturer, sModel, sVersion: String) : Boolean;

const
//  cQuery = 'SELECT Vendor, Name, Version FROM Win32_ComputerSystemProduct';
    cQuery = 'SELECT * FROM Win32_ComputerSystemProduct';

const
    WbemUser            ='';
    WbemPassword        ='';

var
    FSWbemLocator : OLEVariant;
    FWMIService   : OLEVariant;
    FWbemObjectSet: OLEVariant;
    FWbemObject   : OLEVariant;
    oEnum         : IEnumvariant;
    iValue        : LongWord;

begin
    Result := false;

    FSWbemLocator := CreateOleObject ('WbemScripting.SWbemLocator');
    FWMIService := FSWbemLocator.ConnectServer (cLocalHost, cCimV2, WbemUser,
                                                WbemPassword);
    FWbemObjectSet := FWMIService.ExecQuery (cQuery, cWQL ,wbemFlagForwardOnly);
    oEnum := IUnknown (FWbemObjectSet._NewEnum) as IEnumVariant;

    if (oEnum.Next (1, FWbemObject, iValue) = 0) then
    begin
        Result := true;
        sManufacturer := Trim (String (FWbemObject.Vendor){%H-});
        sModel := Trim (String (FWbemObject.Name){%H-});
        sVersion := Trim (String(FWbemObject.Version){%H-});

        FWbemObject := {%H-}Unassigned;
    end; { if }
end; { Get_PC_Info }

(* ---- *)

procedure Get_BiosInfo (const InfoList: TStrings;
                        const bAllInfo: Boolean = false);

var
	sSerialNumber, sBiosVersion, sManufacturer, sModel, sVersion : String;
    uReleaseDate, uEmbeddedMajor, uEmbeddedMinor : UInt;

begin
    Get_BiosInfo (sBiosVersion, sSerialNumber, uReleaseDate,
                  uEmbeddedMajor, uEmbeddedMinor);
    Get_PC_Info (sManufacturer, sModel, sVersion);

    with InfoList do
    begin
        Add ('Manufacturer: ' + sManufacturer);
        Add ('Model: ' + sModel);
        Add ('Version: ' + sVersion);
        Add ('');
        Add ('Bios version: ' + sBiosVersion);
        Add ('Bios release date: ' + IntToStr (uReleaseDate));
        Add ('');
        Add ('Serial number: ' + sSerialNumber);

        if (bAllInfo) then
        begin

            if (bWin10) then
            begin
                Add ('');
                Add (Format ('Embedded Controller Major Version: %d',
                             [uEmbeddedMajor]));
                Add (Format ('Embedded Controller Minor Version: %d',
                             [uEmbeddedMinor]));
            end; { if }
        end; { if }
    end; { with }
end; { Get_BiosInfo }

(* ---- *)

function GetEmbeddedControllerVersion (out uMajor, uMinor: UInt) : Boolean;

var
	sBiosVersion, sSerialNumber : String;
    uReleaseDate : UInt;

begin
	Result := Get_BiosInfo (sBiosVersion, sSerialNumber, uReleaseDate,
                  			uMajor, uMinor);
end; { GetEmbeddedControllerVersion }

(* ---- *)

initialization
	if (Win32Platform = VER_PLATFORM_WIN32_NT) then
    	bWin10 := IsWindows10
    else bWin10 := false;
end.

