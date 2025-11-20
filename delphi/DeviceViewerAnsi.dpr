{$I ..\switches.inc}

{$WARN SYMBOL_PLATFORM OFF}

program DeviceViewerAnsi;

{$R '..\Version.res' '..\Version.RC'}

uses
  Forms,
  Windows,
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
  CmdLineU in '..\PasTools\CmdLineU.pas',
  DevViewerMainFormU in '..\DevViewerMainFormU.pas' {DeviceViewerForm},
  FillDevicesThreadU in '..\FillDevicesThreadU.pas';

{$R *.res}

(* ---- *)

var
  hMutex : THandle;
  iReturnValue : Integer;

begin { DeviceViewer }
{$IFDEF DELPHI2007_UP}
  ReportMemoryLeaksOnShutdown := DebugHook <> 0;
{$ENDIF}

{$IFDEF REMOTEDEBUG}
  MessageBox (GetDesktopWindow, 'Attach the Remote Debugger now ...',
              'Device Viewer', mb_IconInformation or mb_SystemModal);
{$ENDIF}

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
  	  Halt ($FF);
    end; { if }

  Application.Initialize;

{$IFDEF DELPHI2006_UP}
  Application.MainFormOnTaskbar := True;
{$ENDIF}
  Application.CreateForm(TDeviceViewerForm, DeviceViewerForm);
  Application.Run;

  CloseHandle (hMutex);
end.
