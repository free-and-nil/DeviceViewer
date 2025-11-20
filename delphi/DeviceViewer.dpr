// Copyright (c) 2024 Olaf Hess
// All rights reserved.

// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.

{$I ..\switches.inc}

{$WARN SYMBOL_PLATFORM OFF}

program DeviceViewer;

{$R 'Version.res' '..\Version.RC'}

uses
  Forms,
  Windows,
  SysUtils,
  DeviceViewerU in '..\PASTOOLS\DeviceViewerU.pas',
  CfgMgr32_ImportU in '..\PASTOOLS\CfgMgr32_ImportU.pas',
  Cfg_ImportU in '..\PASTOOLS\Cfg_ImportU.pas',
  Win32ToolsU in '..\PASTOOLS\Win32ToolsU.pas',
  pastools in '..\PASTOOLS\pastools.pas',
  AboutDlgU in '..\PASTOOLS\AboutDlgU.pas' {AboutDlg},
  WinTools in '..\PASTOOLS\WinTools.pas',
  Get_BIOS_InfoU in '..\PASTOOLS\Get_BIOS_InfoU.pas',
  FormatOutputU in '..\FormatOutputU.pas',
  MsgBoxU in '..\PASTOOLS\MsgBoxU.pas',
  MainU in '..\CmdLine\MainU.pas',
  DevViewerMainFormU in '..\DevViewerMainFormU.pas' {DeviceViewerForm},
  CmdLineU in '..\PasTools\CmdLineU.pas',
  Wow64U in '..\PasTools\Wow64U.pas',
  Delphi_T in '..\PasTools\Delphi_T.pas',
  SetupApi_ImportU in '..\PasTools\SetupApi_ImportU.pas',
  BaseTypesU in '..\PasTools\BaseTypesU.pas',
  VerifyU in '..\PASTOOLS\VerifyU.pas',
  Regstr_ImportU in '..\PasTools\Regstr_ImportU.pas',
  RegistryApiU in '..\PasTools\RegistryApiU.pas',
  WindowsDisplayName_WMI_U in '..\PasTools\WindowsDisplayName_WMI_U.pas',
  FillDevicesThreadU in '..\FillDevicesThreadU.pas',
  TaskDlgU in '..\PasTools\TaskDlgU.pas',
  TaskDlgApiU in '..\PasTools\TaskDlgApiU.pas';

{$R *.res}

(* ---- *)

{$IFDEF REMOTEDEBUGGER}
procedure WaitForRemoteDebugger;
begin
	if not (IsDebuggerPresent) then
		MsgBox ('Attach the Remote Debugger now ...',
        		mb_IconInformation or mb_SystemModal);
end; { WaitForRemoteDebugger; }
{$ENDIF}

(* ---- *)

var
  hMutex : THandle;
  iReturnValue : Integer;

begin { DeviceViewer }
{$IFDEF DELPHI2007_UP}
  ReportMemoryLeaksOnShutdown := DebugHook <> 0;
{$ENDIF}

{$IFDEF REMOTEDEBUGGER}
  WaitForRemoteDebugger;
{$ENDIF}

  if (ParamCount > 0) then
  begin
    iReturnValue := Main;
    Halt (iReturnValue);
  end; { if }

  hMutex := CreateMutex (NIL, false, 'DeviceViewerMutex');

{$IFNDEF DEBUG}
  if (WaitForSingleObject (hMutex, 0) = wait_TimeOut) then
    if (ActivatePrevInstance ('TDeviceViewerForm') <> 0) then
    begin
      VerifyApi (CloseHandle (hMutex));
  	  Halt ($FF);
    end; { if }
{$ENDIF}

  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TDeviceViewerForm, DeviceViewerForm);
  Application.Run;

  VerifyApi (CloseHandle (hMutex));
end.
