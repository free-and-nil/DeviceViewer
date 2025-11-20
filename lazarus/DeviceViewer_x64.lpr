// Copyright (c) 2024 Olaf Hess
// All rights reserved.

// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.

program DeviceViewer_x64;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Windows, Forms, MainU, WinTools, DevViewerMainFormU;

{$R *.res}

var
  hMutex : THandle;
  iReturnValue : Integer;

begin { DeviceViewer }
  if (ParamCount > 0) then
  begin
    iReturnValue := Main;
    Halt (iReturnValue);
  end; { if }

  hMutex := CreateMutex (NIL, false, 'DeviceViewerMutex');

  if (WaitForSingleObject (hMutex, 0) = wait_TimeOut) then
    if (ActivatePrevInstance ('TDeviceViewerForm') <> 0) then
    begin
      CloseHandle (hMutex);
  	  Halt (0);
    end; { if }

  RequireDerivedFormResource:=True;
  Application.Scaled := True;
  Application.Initialize;
  Application.CreateForm(TDeviceViewerForm, DeviceViewerForm);
  Application.Run;

  CloseHandle (hMutex);
end.

