// Copyright (c) 2024 Olaf Hess
// All rights reserved.

// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.

{$IFDEF DeviceViewer}
    {$I .\switches.inc}
{$ELSE}
    {$I ..\switches.inc}
{$ENDIF}

unit FormatOutputU;

interface

uses Classes,
     DeviceViewerU;

function FillOutputList (const OutputList: TStrings; const Devices: TDevices;
                         const bSaveToIni: Boolean = false;
                         const sDeviceName: String = '';
                         const sDevice_ID: String = '';
                         const sThisClassOnly: String = '') : Integer;
function GetDeviceInfoAsText (const Device: TDevice) : String;
function GetDeviceInfoAsIni (const Device: TDevice) : String;
procedure SaveDevicesToFile (const sFileName: String; const Devices: TDevices;
                             const bSaveToIni: Boolean = false);

implementation

uses SysUtils,
     PasTools, Get_BIOS_InfoU, BaseTypesU, Win32ToolsU, WinTools,
     WindowsDisplayName_WMI_U;

const
    cDividerLen = 80;

ResourceString
	cComputerName = 'Computer name = ';
    cIncludeNonPresentMsg = 'Include nonpresent devices = ';

(* ---- *)


function FillOutputList (const OutputList: TStrings; const Devices: TDevices;
                         const bSaveToIni: Boolean = false;
                         const sDeviceName: String = '';
                         const sDevice_ID: String = '';
                         const sThisClassOnly: String = '') : Integer;

    (* ---- *)

    function FindName (const sCaption: String;
                       const bWildcards: Boolean) : Boolean;
    begin
        if (bWildcards) then
            Result := MatchString (sDeviceName, LowerCase (sCaption))
        else Result := CompareText (sDeviceName, sCaption) = 0;
    end; { FindName }

    (* ---- *)

    function Find_ID (const aIDs: TaString) : Boolean;

    var
        iIndex, iLen : Integer;

    begin
        if (Length (aIDs) > 0) then
            for iIndex := 0 to High (aIDs) do
                if (Pos (sDevice_ID, UpperCase (aIDs [iIndex])) = 1) then
                begin
                	iLen := Length (sDevice_ID);

                    if (Length (aIDs [iIndex]) = iLen) or
                       (aIDs [iIndex][iLen + 1] = '&') then
                    begin
                        Result := true;
                        exit;
                    end; { if }
                end; { if }

        Result := false;
    end; { Find_ID }

    (* ---- *)

    procedure IncludeSystemInformation;
    begin
    	OutputList.Add (cComputerName + GetSystemName);
    	OutputList.Add (GetWindowsInfo);
        OutputList.Add ('');
        Get_BiosInfo (OutputList);
        OutputList.Add (cIncludeNonPresentMsg +
                        BoolToStr (Devices.IncludeNonpresentDevices, true));
        OutputList.Add ('');
    end; { IncludeSystemInformation }

    (* ---- *)

var
    iClass, iDevice : Integer;
    Device : TDevice;
    bSearch, bDeviceName, bWildcards, bFirstDevice : Boolean;

begin { FillOutputList }
    Assert (OutputList <> NIL);
    Assert (Devices <> NIL);

    Result := 0;

    if (sDeviceName <> '') or (sDevice_ID <> '') then
    begin
        bSearch := true;
        bDeviceName := sDeviceName <> '';

        if (bDeviceName) then
	        bWildcards := ContainsWildcardChars (sDeviceName)
        else bWildCards := false;
    end { if }
    else
    begin
        bSearch := false;
        bDeviceName := false;
        bWildcards := false;
    end; { else }

    if (bSaveToIni = false) and (bSearch = false) then
        IncludeSystemInformation;

    bFirstDevice := true;

    for iClass := 0 to Devices.ClassCount - 1 do
    begin
        if (sThisClassOnly <> '') then
        	if (Devices.Classes [iClass].Description <> '?') and
           	   (sThisClassOnly <> Devices.Classes [iClass].ClassGuid) then
            	Continue;

        if (bSaveToIni = false) and (bSearch = false) then
        begin
            OutputList.Add (StringOfChar ('=', cDividerLen));
            OutputList.Add (Format ('Class = %s [%s]',
                            [Devices.Classes [iClass].Description,
                             Devices.Classes [iClass].ClassGuid]));
            OutputList.Add (StringOfChar ('=', cDividerLen));

            bFirstDevice := true;
        end; { if }

        for iDevice := 0 to Devices.Classes [iClass].DeviceCount - 1 do
        begin
            Device := Devices.Classes [iClass].Devices [iDevice];

            if (bSearch) then
                if (bDeviceName) then
                begin
                    if (Devices.Classes [iClass].Description = '?') or
                       (FindName (Device.Caption, bWildcards) = false) then
                        Continue
                end { if }
                else
                	if not (Find_ID (Device.Hardware_IDs)) then
                    	Continue;

            Inc (Result);

            if (bFirstDevice) then
                bFirstDevice := false
            else
                if (bSaveToIni) then
                    OutputList.Add ('')
                else OutputList.Add (StringOfChar ('-', cDividerLen));

            if (bSaveToIni) then
                OutputList.Add (GetDeviceInfoAsIni (Device))
            else OutputList.Add (GetDeviceInfoAsText (Device));
        end; { for }
    end; { for }
end; { FillOutputList }

(* ---- *)

function GetDeviceInfoAsIni (const Device: TDevice) : String;

const
    cMsg = '[]'#13#10 +
           cCaption + '=%s'#13#10 +
           cDescription + '=%s'#13#10 +
           cManufacturer + '=%s'#13#10 +
           cDriverVer + '=%s'#13#10 +
           cDriverDate + '=%s'#13#10 +
           cDriverProvider + '=%s'#13#10 +
           cClass + '=%s'#13#10 +
           cDevicePresent + '=%s'#13#10 +
           cDeviceStarted + '=%s'#13#10 +
           cDeviceDisabled + '=%s'#13#10 +
           cDeviceHidden + '=%s'#13#10 +
           cPCI_ID_Prefix + cCount + '=%d'#13#10 +
           '%s'#13#10 { #13#10 } ;

var
    sDriverDate, sHW_IDs : String;
    iCount : Integer;

begin { GetDeviceInfoAsIni }
    with Device do
    begin
        sDriverDate := DriverDateStr;

        if (sDriverDate = '') then
            sDriverDate := 'n/a';

        sHW_IDs := Trim (GetHardware_IDs (iCount, true));

        Result := Format (cMsg,
                          [Caption, Description, Manufacturer, DriverVer,
                           sDriverDate, DriverProvider, ClassDescription,
                           BoolToStr (DevicePresent, true),
                           BoolToStr (DeviceStarted, true),
                           BoolToStr (DeviceDisabled, true),
                           BoolToStr (DeviceHidden, true),
                           iCount, iif (iCount > 0, sHW_IDs, '')]);
    end; { with }
end; { GetDeviceInfoAsIni }

(* ---- *)

function GetDeviceInfoAsText (const Device: TDevice) : String;

const
    cMsg = cCaption + ' = %s'#13#10 +
           cDescription + ' = %s'#13#10 +
           cManufacturer + ' = %s'#13#10 +
           cDriverVer + ' = %s'#13#10 +
           cDriverDate + ' = %s'#13#10 +
           cDriverProvider + ' = %s'#13#10 +
           cClass + ' = %s'#13#10 +
           cDevicePresent + ' = %s'#13#10 +
           cDeviceStarted + ' = %s'#13#10 +
           cDeviceDisabled + ' = %s'#13#10 +
           cDeviceHidden + ' = %s'#13#10 +
           '%s' + cHardware_ID + '%s: %s%s';

var
    sDriverDate, sHW_IDs : String;
    iCount : Integer;

begin
    with Device do
    begin
        sDriverDate := DriverDateStr;

        if (sDriverDate = '') then
            sDriverDate := 'n/a';

        sHW_IDs := Trim (GetHardware_IDs (iCount));

        Result := Format (cMsg,
                          [Caption, Description, Manufacturer, DriverVer,
                           sDriverDate, DriverProvider,
                           ClassDescription,
                           BoolToStr (DevicePresent, true),
                           BoolToStr (DeviceStarted, true),
                           BoolToStr (DeviceDisabled, true),
                           BoolToStr (DeviceHidden, true),
                           iif (iCount > 1, #13#10, ''),
                           iif (iCount > 1, 's', ''),
                           iif (iCount > 1, #13#10, ''),
                           sHW_IDs])
    end; { with }
end; { TDeviceViewerForm.GetDeviceInfoAsText }

(* ---- *)

procedure SaveDevicesToFile (const sFileName: String; const Devices: TDevices;
                             const bSaveToIni: Boolean = false);

var
    OutputList : TStringList;

begin
    Assert (sFileName <> '');
    Assert (Devices <> NIL);

    OutputList := TStringList.Create;

    try
        FillOutputList (OutputList, Devices, bSaveToIni);
        OutputList.SaveToFile (sFileName);

    finally
        OutputList.Free;
    end; { try / finally }
end; { SaveDevicesToFile }

(* ---- *)

end.

