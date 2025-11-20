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
    {$mode DELPHI}{$H+}
{$ENDIF}

unit DevViewerMainFormU;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ComCtrls, SyncObjs, Menus, ImgList,
{$IFDEF DELPHI_X2_UP}
  System.ImageList,
{$ENDIF}
  DeviceViewerU, FillDevicesThreadU;

type
  TDeviceViewerForm = class (TForm)
    Devices_TV: TTreeView;
    MainMenu: TMainMenu;
    FileMenu: TMenuItem;
    ViewMenu: TMenuItem;
    FileSaveMenu: TMenuItem;
    FileExitMenu: TMenuItem;
    N1: TMenuItem;
    ViewShowHiddenMenu: TMenuItem;
    SaveDialog: TSaveDialog;
    ImageList: TImageList;
    ViewShowNonPresentMenu: TMenuItem;
    N2: TMenuItem;
    ViewRefreshMenu: TMenuItem;
    PopupMenu: TPopupMenu;
    PopupShowDetailsMenu: TMenuItem;
    HelpMenu: TMenuItem;
    HelpAboutMenu: TMenuItem;
    N3: TMenuItem;
    ViewDisplayBiosInfoMenu: TMenuItem;
    EditMenu: TMenuItem;
    EditFindMenu: TMenuItem;
    EditFindNextMenu: TMenuItem;
    FileLoadSavedListMenu: TMenuItem;
    OpenDialog: TOpenDialog;
    DebugMenu: TMenuItem;
    SetupDiGetDevicePropertyW_Menu: TMenuItem;

    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormPaint(Sender: TObject);
    procedure FileSaveMenuClick(Sender: TObject);
    procedure FileExitMenuClick(Sender: TObject);
    procedure ViewShowHiddenMenuClick(Sender: TObject);
    procedure Devices_TV_DblClick(Sender: TObject);
    procedure Devices_TV_KeyPress(Sender: TObject; var Key: Char);
    procedure ViewShowNonPresentMenuClick(Sender: TObject);
    procedure ViewRefreshMenuClick(Sender: TObject);
    procedure PopupShowDetailsMenuClick(Sender: TObject);
    procedure HelpAboutMenuClick(Sender: TObject);
    procedure ViewDisplayBiosInfoMenuClick(Sender: TObject);
    procedure EditFindMenuClick(Sender: TObject);
    procedure EditFindNextMenuClick(Sender: TObject);
    procedure FileLoadSavedListMenuClick(Sender: TObject);
    procedure SetupDiGetDevicePropertyW_MenuClick(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);

  private
    Devices : TDevices;
    FDL_Thread : TFillDevicesThread;
    Refresh_CS : TCriticalSection;
    sFindStr : String;
    hInfoIcon : HIcon;
    hUser32 : THandle;

    procedure FillDeviceListThreadOnTerminate (Sender: TObject);
    procedure FillListView (const {%H-}bShowNonPresent, bShowHidden: Boolean);
    procedure SearchForDevice (const iStart: Integer);
    procedure InfoTaskDlg (const sMsg: String);
    procedure ShowNodeInfo (const Data: TObject);
    procedure TriggerFillListView;

    procedure wmDeviceChange (var Msg: TMessage); message WM_DEVICECHANGE;

  public
    procedure HideLoadSavedListMenu;
  end; { TDeviceViewerForm }

{$IFDEF DeviceViewer}
var
    DeviceViewerForm : TDeviceViewerForm;
{$ENDIF}

implementation

{$R *.dfm}

uses FormatOutputU,
{$IFNDEF FPC}
  {$IFDEF DEBUG}
     WinTools,
  {$ENDIF}
{$ENDIF}
     AboutDlgU, Get_BIOS_InfoU, Win32ToolsU, MsgBoxU, TaskDlgU, TaskDlgApiU;

ResourceString
    cBiosInformation = 'BIOS Information';
    cCloseMsg = '&Close';
	cFindCaption = 'Search from current position';
    cFindFailureMsg = 'Search string "%s" not found';
    cFindPrompt = 'Search text:';
    cFindStrMissingMsg = 'Please enter a search string!';
    cNodeInfoMsg = 'Description = %s'#13#10 +
                   'GUID = %s'#13#10'Members = %d (%d hidden, %d present)';

const
    // JwaDbt.pas
    DBT_CONFIGCHANGED           = $0018; // sent when a config has changed
    DBT_DEVICEARRIVAL           = $8000; // system detected a new device
    DBT_DEVICEREMOVECOMPLETE    = $8004; // device is gone

(* ---- *)

procedure TDeviceViewerForm.FormCloseQuery (Sender: TObject;
                                            var CanClose: Boolean);
begin
    if (Assigned (FDL_Thread)) then
    begin
        MessageBeep (mb_IconWarning);
        CanClose := false;
    end { if }
    else CanClose := true;
end; { TDeviceViewerForm.FormCloseQuery }

(* ---- *)

procedure TDeviceViewerForm.FormCreate (Sender: TObject);
begin
{$IFDEF DEBUG}
    Caption := Caption + ' [Debug]';
{$ENDIF}

    if (Assigned (Application.MainForm)) then
        {$IFDEF FPC}
        if (Application.MainForm.ClassName <> Self.ClassName) then
        {$ENDIF}
        FileExitMenu.Caption := cCloseMsg;

    if (Application.MainForm = Self) then
    	Application.Title := Caption
    else BorderIcons := BorderIcons - [biMinimize];

    Devices := TDevices.Create (false, false, ImageList);
    Refresh_CS := TCriticalSection.Create;

    SetupDiGetDevicePropertyW_Menu.Checked := Win32MajorVersion >= 6;

    hUser32 := LoadLibrary (PChar (GetSystemDir + '\user32.dll'));

    if (hUser32 <> 0) then
        hInfoIcon := LoadImage (hUser32, MakeIntResource (104), IMAGE_ICON,
                                0, 0, LR_DEFAULTSIZE);

{$IFNDEF DEBUG}
    DebugMenu.Visible := false;
{$ENDIF}

{$IFDEF FPC}
	if (IsWindows_PE) then
		with SaveDialog do
    		Options := Options + [ofOldStyleDialog];
{$ELSE}
  {$IFDEF DELPHI2007_UP}
    UseLatestCommonDialogs := not IsWindows_PE;
  {$ENDIF}
{$ENDIF}
end; { TDeviceViewerForm.FormCreate }

(* ---- *)

procedure TDeviceViewerForm.FormDestroy (Sender: TObject);
begin
    if (hInfoIcon <> 0) then
        DestroyIcon (hInfoIcon);

    if (hUser32 <> 0) then
        FreeLibrary (hUser32);

    Devices.Free;
    Refresh_CS.Free;
end; { TDeviceViewerForm.FormDestroy }

(* ---- *)

procedure TDeviceViewerForm.FormPaint (Sender: TObject);
begin
    OnPaint := NIL;

    Application.ProcessMessages;
    TriggerFillListView;
end; { TDeviceViewerForm.FormPaint }

(* ---- *)

procedure TDeviceViewerForm.FileExitMenuClick (Sender: TObject);
begin
    Close;
end; { TDeviceViewerForm.FileExitMenuClick }

(* ---- *)

procedure TDeviceViewerForm.FileLoadSavedListMenuClick (Sender: TObject);

var
    bHidden, bNonPresent : Boolean;

begin
    with OpenDialog do
    begin
        FileName := '';

        if not (Execute) then
            exit;
    end; { with }

    Devices.Free;
    Devices := TDevices.CreateFromFile (OpenDialog.FileName,
                                        bHidden, bNonPresent);

    Caption := Application.Title +
               ' - [' + ExtractFileName (OpenDialog.FileName) + ']';
    ViewRefreshMenu.Enabled := false;
    Devices_TV.Images := NIL;
    ViewShowNonPresentMenu.Checked := bNonPresent;
    ViewShowHiddenMenu.Checked := bHidden;

    TriggerFillListView;
end; { TDeviceViewerForm.FileLoadSavedListMenuClick }

(* ---- *)

procedure TDeviceViewerForm.FileSaveMenuClick (Sender: TObject);
begin
    with SaveDialog do
    begin
        FileName := '';

        if (Execute) then
            SaveDevicesToFile (FileName, Devices);
    end; { with }
end; { TDeviceViewerForm.FileSaveMenuClick }

(* ---- *)

procedure TDeviceViewerForm.ViewRefreshMenuClick (Sender: TObject);
begin
    TriggerFillListView;
end; { TDeviceViewerForm.ViewRefreshMenuClick }

(* ---- *)

procedure TDeviceViewerForm.EditFindMenuClick (Sender: TObject);

var
	sValue : String;
    iStart : Integer;

begin
	sValue := sFindStr;

	repeat
		if (InputQuery (cFindCaption, cFindPrompt, sValue)) then
        begin
        	if (Trim (sValue) = '') then
            begin
            	MsgBox (cFindStrMissingMsg, mb_IconStop);
                Continue;
            end; { if }
        end { if }
        else exit;
	until (sValue <> '');

    sFindStr := sValue;

    if (Assigned (Devices_TV.Selected)) then
    	iStart := Devices_TV.Selected.AbsoluteIndex
    else iStart := 0;

    SearchForDevice (iStart);
end; { TDeviceViewerForm.EditFindMenuClick }

(* ---- *)

procedure TDeviceViewerForm.EditFindNextMenuClick (Sender: TObject);

var
	iStart : Integer;

begin
	if (sFindStr <> '') then
    begin
		if (Assigned (Devices_TV.Selected)) then
        begin
        	iStart := Succ (Devices_TV.Selected.AbsoluteIndex);

            if (iStart = Devices_TV.Items.Count) then
            	iStart := 0;
        end { if }
        else iStart := 0;

        SearchForDevice (iStart);
    end { if }
    else EditFindMenuClick (Sender);
end; { TDeviceViewerForm.EditFindNextMenuClick }

(* ---- *)

procedure TDeviceViewerForm.ViewShowHiddenMenuClick (Sender: TObject);
begin
    ViewShowHiddenMenu.Checked := not ViewShowHiddenMenu.Checked;

    if not (ViewShowHiddenMenu.Checked) then
        ViewShowNonPresentMenu.Checked := false;

    TriggerFillListView;
end; { TDeviceViewerForm.ViewShowHiddenMenuClick }

(* ---- *)

procedure TDeviceViewerForm.ViewShowNonPresentMenuClick (Sender: TObject);
begin
    ViewShowNonPresentMenu.Checked := not ViewShowNonPresentMenu.Checked;

    if (ViewShowNonPresentMenu.Checked) then
        ViewShowHiddenMenu.Checked := true;

    TriggerFillListView;
end; { TDeviceViewerForm.ViewShowNonPresentMenuClick }

(* ---- *)

procedure TDeviceViewerForm.ViewDisplayBiosInfoMenuClick (Sender: TObject);

var
    OutputList : TStringList;

begin
    OutputList := TStringList.Create;

    try
        Get_BiosInfo (OutputList);

        Application.MessageBox (PChar (TrimRight (OutputList.Text)),
                                PChar (cBiosInformation), mb_IconInformation);

    finally
        OutputList.Free;
    end; { try / finally }
end; { TDeviceViewerForm.ViewDisplayBiosInfoMenuClick }

(* ---- *)

procedure TDeviceViewerForm.SetupDiGetDevicePropertyW_MenuClick (Sender: TObject);
begin
    with SetupDiGetDevicePropertyW_Menu do
        Checked := not Checked;

    Devices.UseVistaFunctions := SetupDiGetDevicePropertyW_Menu.Checked;
    TriggerFillListView;
end; { TDeviceViewerForm.SetupDiGetDevicePropertyW_MenuClick }

(* ---- *)

procedure TDeviceViewerForm.HelpAboutMenuClick (Sender: TObject);
begin
    with TAboutDlg.Create (Self) do
        try
            ShowModal;

        finally
            Release;
        end; { try / finally }
end; { TDeviceViewerForm.HelpAboutMenuClick }

(* ---- *)

procedure TDeviceViewerForm.PopupShowDetailsMenuClick (Sender: TObject);
begin
    if (Assigned (Devices_TV.Selected)) then
        ShowNodeInfo (Devices_TV.Selected.Data);
end; { TDeviceViewerForm.PopupShowDetailsMenuClick }

(* ---- *)

procedure TDeviceViewerForm.Devices_TV_DblClick (Sender: TObject);
begin
    if (Assigned (Devices_TV.Selected)) then
        if not (Devices_TV.Selected.HasChildren) then
            ShowNodeInfo (Devices_TV.Selected.Data)
end; { TDeviceViewerForm.Devices_TV_DblClick }

(* ---- *)

procedure TDeviceViewerForm.Devices_TV_KeyPress (Sender: TObject;
                                                  var Key: Char);
begin
    if (Byte (Key) = VK_RETURN) then
        if (Assigned (Devices_TV.Selected)) then
            ShowNodeInfo (Devices_TV.Selected.Data)
end; { TDeviceViewerForm.Devices_TV_KeyPress }

(* ---- *)

procedure TDeviceViewerForm.FillDeviceListThreadOnTerminate (Sender: TObject);
begin
    FillListView (ViewShowNonPresentMenu.Checked, ViewShowHiddenMenu.Checked);
    FDL_Thread := NIL;
end; { TDeviceViewerForm.FillDeviceListThreadOnTerminate }

(* ---- *)

procedure TDeviceViewerForm.FillListView (
                                   const bShowNonPresent, bShowHidden: Boolean);

    (* ---- *)

    function VisibleDevices (const DeviceClass: TDeviceClass) : Boolean;

    var
        iNonPresent, iHidden : Integer;

    begin
        Result := false;

        iHidden := DeviceClass.HiddenDevices;

        if (ViewShowHiddenMenu.Checked) and (iHidden > 0) then
            Result := true
        else
        begin
            iNonPresent := DeviceClass.NonPresentDevices;

            if (ViewShowNonPresentMenu.Checked and (iNonPresent > 0)) or
               (((DeviceClass.DeviceCount - iHidden) - iNonPresent) > 0) then
                Result := true;
        end; { else }
    end; { VisibleDevices }

    (* ---- *)

var
    DeviceClass : TDeviceClass;
    Device : TDevice;
    iDevice, iClass : Integer;
    Class_TN : TTreeNode;
    sCaption : String;

begin { TDeviceViewerForm.FillListView }
    Devices_TV.Items.BeginUpdate;

    try
        if (Devices_TV.Items.Count > 0) then
            Devices_TV.Items.Clear;

        for iClass := 0 to Devices.ClassCount - 1 do
        begin
            DeviceClass := Devices.Classes [iClass];

            if not (VisibleDevices (DeviceClass)) then
                Continue;

            Class_TN := Devices_TV.Items.Add (NIL, DeviceClass.Description);
            Class_TN.Data := DeviceClass;
            Class_TN.ImageIndex := DeviceClass.ImageIndex;
            Class_TN.SelectedIndex := DeviceClass.ImageIndex;

            for iDevice := 0 to DeviceClass.DeviceCount - 1 do
            begin
                Device := DeviceClass.Devices [iDevice];

                if (Device.DeviceHidden) and (bShowHidden = false) then
                    Continue;

                if (Device.Caption <> '') then
                    sCaption := Device.Caption
                else sCaption := '?';

                with Devices_TV.Items.AddChild (Class_TN, sCaption) do
                begin
                    Data := Device;
                    ImageIndex := Device.ImageIndex;
                    SelectedIndex := ImageIndex;
                end; { with }
            end; { with }
        end; { for }

    finally
        Devices_TV.Items.EndUpdate;
        Screen.Cursor := crDefault;
        Refresh_CS.Leave;
    end; { try / finally }

    if (Devices_TV.Items.Count > 0) then
        Devices_TV.Selected := Devices_TV.Items [0];
end; { TDeviceViewerForm.FillListView }

(* ---- *)

procedure TDeviceViewerForm.SearchForDevice (const iStart: Integer);

var
	iIndex : Integer;
    sValue : String;

begin
	sValue := LowerCase (sFindStr);

	for iIndex := iStart to Devices_TV.Items.Count - 1 do
    	if (Pos (sValue, LowerCase (Devices_TV.Items [iIndex].Text)) > 0) then
        begin
        	Devices_TV.Selected := Devices_TV.Items [iIndex];
			exit;
        end; { if }

	if (CheckWin32Version (5, 1)) then
    	MsgBoxTimeOut (cFindFailureMsg, [sFindStr], 5, mb_IconInformation)
    else MsgBox (cFindFailureMsg, [sFindStr], mb_IconInformation);
end; { TDeviceViewerForm.SearchForDevice }

(* ---- *)

procedure TDeviceViewerForm.InfoTaskDlg (const sMsg: String);

var
    Icon : TIcon;

begin
    Assert (sMsg <> '');

    Icon := TIcon.Create;

    try
        ImageList.GetIcon (Devices_TV.Selected.ImageIndex, Icon);

        with TTaskDlg.Create do
            try
                Flags := [tdfUseHIconMain, tdfAllowDialogCancellation];
                WindowTitle := Caption;
                MainInstruction := Devices_TV.Selected.Text;
                Content := sMsg;
                CommonButtons := [tcbClose];
                MainIcon := Icon.Handle;

                Execute;

            finally
                Free;
            end; { try / finally }

    finally
        Icon.Free;
    end; { try / finally }
end; { TDeviceViewerForm.InfoTaskDlg }

(* ---- *)

procedure TDeviceViewerForm.ShowNodeInfo (const Data: TObject);

    (* ---- *)

    procedure ShowMsg (const sMsg: String);
    begin
        Assert (sMsg <> '');

        if (Win32MajorVersion >= 6) then
            InfoTaskDlg (sMsg)
        else MsgBox (Handle, sMsg, Devices_TV.Selected.Text, mb_IconInformation)
    end; { ShowMsg }

    (* ---- *)

    procedure ShowClassInfo (const DeviceClass: TDeviceClass);

    var
        iIndex, iCount, iHidden, iPresent : Integer;

    begin
        iCount := 0;
        iHidden := 0;
        iPresent := 0;

        for iIndex := 0 to DeviceClass.DeviceCount - 1 do
        begin
            if (DeviceClass.Devices [iIndex].DeviceHidden) and
               (ViewShowHiddenMenu.Checked = false) then
                Inc (iHidden);

            if (DeviceClass.Devices [iIndex].DevicePresent) then
                Inc (iPresent);

            Inc (iCount);
        end; { for }

        with DeviceClass do
            ShowMsg (Format (cNodeInfoMsg,
                             [Description, ClassGuid, iCount, iHidden, iPresent]))
    end; { ShowClassInfo }

    (* ---- *)

begin { TDeviceViewerForm.ShowNodeInfo }
    if (Data = NIL) then
        exit
    else if (Data is TDevice) then
        ShowMsg (GetDeviceInfoAsText (Data as TDevice))
    else if (Data is TDeviceClass) then
        ShowClassInfo (Data as TDeviceClass)
end; { TDeviceViewerForm.ShowNodeInfo }

(* ---- *)

procedure TDeviceViewerForm.TriggerFillListView;
begin
    Refresh_CS.Enter;

    Screen.Cursor := crHourGlass;

    if (Assigned (Devices_TV.Images)) then
    begin
        if (Devices_TV.Items.Count > 0) then
            Devices_TV.Items.Clear;

        Application.ProcessMessages;

        FDL_Thread := TFillDevicesThread.Create (Devices,
                                                 ViewShowNonPresentMenu.Checked);
        FDL_Thread.OnTerminate := FillDeviceListThreadOnTerminate;
    end { if }
    else FillListView (ViewShowNonPresentMenu.Checked,
                       ViewShowHiddenMenu.Checked);
end; { TDeviceViewerForm.TriggerFillListView }

(* ---- *)

procedure TDeviceViewerForm.wmDeviceChange (var Msg: TMessage);
begin
    if (Msg.WParam = DBT_CONFIGCHANGED) or (Msg.WParam = DBT_DEVICEARRIVAL) or
       (Msg.WParam = DBT_DEVICEREMOVECOMPLETE) then
        FillListView (ViewShowNonPresentMenu.Checked,
                      ViewShowHiddenMenu.Checked);

    Msg.Result := Integer (true);
end; { TDeviceViewerForm.wmDeviceChange }

(* ---- *)

procedure TDeviceViewerForm.HideLoadSavedListMenu;
begin
    FileLoadSavedListMenu.Visible := false;
end; { TDeviceViewerForm.HideLoadSavedListMenu }

(* ---- *)

end.
