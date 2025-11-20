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

{$IFDEF FPC}
	{$MODE DELPHI}
{$ENDIF}

unit FillDevicesThreadU;

interface

uses Classes,
     DeviceViewerU;

type
    TFillDevicesThread = class (TThread)
      private
        bShowNonPresent : Boolean;
        Devices : TDevices;

      protected
        procedure Execute; override;

      public
        constructor Create (const Devices: TDevices;
                            const bShowNonPresent: Boolean);
    end; { TFillDevicesThread }

implementation

{$IFDEF DEBUG}
uses Windows,
     WinTools;
{$ENDIF}

(* ---- *)

procedure TFillDevicesThread.Execute;

{$IFDEF DEBUG}
var
    uStartTime : UInt;
{$ENDIF}

begin
{$IFDEF DEBUG}
    uStartTime := GetTickCount{%H-};
{$ENDIF}
    Devices.Refresh (bShowNonPresent);

{$IFDEF DEBUG}
    OutputDebugStr ('Elapsed = %d', [GetTickCount {%H-}- uStartTime]);
{$ENDIF}
end; { TFillDevicesThread.Execute }

(* ---- *)

constructor TFillDevicesThread.Create (const Devices: TDevices;
                                          const bShowNonPresent: Boolean);
begin
    Self.bShowNonPresent := bShowNonPresent;
    Self.Devices := Devices;

    inherited Create (false);
    FreeOnTerminate := true;
end; { TFillDevicesThread.Create }

(* ---- *)

end.
