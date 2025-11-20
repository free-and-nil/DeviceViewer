{$I ..\switches.inc}

{$WARN SYMBOL_PLATFORM OFF}

unit MainU;

interface

function Main : Integer;

implementation

uses Classes, SysUtils, ActiveX, Windows, RTLConsts,
     FormatOutputU,
     DeviceViewerU, MsgBoxU, CmdLineU, Wow64U, Delphi_T, PasTools;

{$IFNDEF FPC}
  {$IFNDEF DELPHI2009}
type
    EFileNotFoundException = class (Exception);
  {$ENDIF}
{$ENDIF}

const
    cDevice_ID = 'Device_ID';
    cDeviceClass = 'DeviceClass';
    cDeviceName = 'DeviceName';
    cProcess_INF = 'Process_INF';
    cSaveAsIni = 'SaveAsIni';

ResourceString
    cParamsMsg = 'DeviceList.exe [.LOG / .INI file name (optional)]'#13#10#13#10 +
   				 'Additional parameters (file name must be specified):'#13#10 +
                 '/SaveAsIni: Save to file in INI format'#13#10 +
                 '/Device_ID [PCI ID] (with "SaveAsIni")'#13#10 +
                 '/DeviceName [device name, wildcards supported] (with "SaveAsIni")'#13#10 +
                 '/Process_INF [.INF file name] (with "SaveAsIni")'#13#10 +
                 '/DeviceClass [class GUID, optional] (with "SaveAsIni")';
    cVistaRequiredMsg =
    	'The parameter "/Process_INF" requires Windows Vista or later';

var
    _bSaveAsIni : Boolean = false;
    _sDeviceName : String = '';
    _sDevice_ID : String = '';
    _sInf_File : String = '';
    _sDeviceClass : String = '';

(* ---- *)

function CheckParams : Boolean;

var
    CmdLineParams : ICmdLineParams;

begin
    Result := false;

    CmdLineParams := TCmdLineParams.Create (puaError);

    CmdLineParams.AddParam ('', 1);

    CmdLineParams.AddParam (cSaveAsIni, true);

    with CmdLineParams.AddParam (cDevice_ID, true, vkNextParam) do
    begin
        CannotBeUsedWith ([cDeviceName, cProcess_INF]);
        MustBeUsedWith ([cSaveAsIni]);
    end; { with }

    CmdLineParams.AddParamValue (cDevice_ID);

    with CmdLineParams.AddParam (cDeviceName, true, vkNextParam) do
    begin
        CannotBeUsedWith ([cDevice_ID, cProcess_INF]);
        MustBeUsedWith ([cSaveAsIni]);
    end; { with }

    CmdLineParams.AddParamValue (cDeviceName);

    with CmdLineParams.AddParam (cProcess_INF, true, vkNextParam) do
    begin
        CannotBeUsedWith ([cDeviceName, cDevice_ID]);
        MustBeUsedWith ([cSaveAsIni]);
    end; { with }

    CmdLineParams.AddParamValue (cProcess_INF);

    with CmdLineParams.AddParam (cDeviceClass, true, vkNextParam) do
        MustBeUsedWith ([cSaveAsIni]);

    CmdLineParams.AddParamValue (cDeviceClass);

    try
        CmdLineParams.ParseParams;

        _bSaveAsIni := CmdLineParams.ParamExists (cSaveAsIni);

        if (CmdLineParams.ParamExists (cDevice_ID)) then
            _sDevice_ID := CmdLineParams.GetValue (cDevice_ID);

        if (CmdLineParams.ParamExists (cDeviceName)) then
            _sDeviceName := CmdLineParams.GetValue (cDeviceName);

        if (CmdLineParams.ParamExists (cProcess_INF)) then
        begin
            _sInf_File := CmdLineParams.GetValue (cProcess_INF);

            if (Win32MajorVersion < 6) then
            	raise EOSError.Create (cVistaRequiredMsg);
        end; { if }

        if (CmdLineParams.ParamExists (cDeviceClass)) then
            _sDeviceClass := CmdLineParams.GetValue (cDeviceClass);

        Result := true;

    except
        on E: Exception do
			if (IsConsole) then
                WriteLn (E.Message)
            else
            	if (CheckWin32Version (5, 1)) then
	            	MsgBoxTimeOut (E.Message, 60, mb_IconStop or mb_SystemModal)
                else MsgBox (E.Message, mb_IconStop or mb_SystemModal);
    end; { try / except }
end; { CheckParams }

(* ---- *)

procedure Fill_Device_IDs_List (out Device_ID_List: TStringList);

    (* ---- *)

    function IsDevice_ID_Section (const sLine: String) : Boolean;

    const
        cNTamd64 = '.NTamd64';
{$IFNDEF CPUX64}
        cNTx86 = '.NTx86';
{$ENDIF}

    var
        sCPU_ID : String;

    begin
        Assert (sLine <> '');

{$IFDEF CPUX64}
        sCPU_ID := cNTamd64;
{$ELSE}
        if (IsWow64) then
            sCPU_ID := cNTamd64
        else sCPU_ID := cNTx86;
{$ENDIF}

        Result := Pos (LowerCase (sCPU_ID), LowerCase (sLine)) > 0;
    end; { IsDevice_ID_Section }

    (* ---- *)

    function Extract_PCI_ID (const sLine: String; var sPCI_ID: String) : Boolean;

    var
        iPos : Integer;

    begin
        Result := false;

        iPos := LastPos (',', sLine);

        if (iPos = 0) then
            exit;

        sPCI_ID := TrimLeft (Copy (sLine, iPos + 1, Length (sLine) - iPos));

        iPos := Pos ('&', sPCI_ID);

        if (iPos = 0) then
            exit;

        iPos := NextPos ('&', sPCI_ID, iPos + 1);

        if (iPos > 0) then
            SetLength (sPCI_ID, iPos - 1);

        iPos := Pos (';', sPCI_ID);

        if (iPos > 0) then
            SetLength (sPCI_ID, iPos - 1);

        sPCI_ID := TrimRight (sPCI_ID);

        Result := true;
    end; { Extract_PCI_ID }

    (* ---- *)

    function Fill_Device_ID_List (const Inf_List: TStringList) : Boolean;

    var
        sLine, sPCI_ID : String;
        bInSection : Boolean;
        iIndex : Integer;

    begin
        bInSection := false;

        for iIndex := 0 to Inf_List.Count - 1 do
        begin
            sLine := Inf_List [iIndex];

            if (sLine = '') or (sLine [1] = ';') then
                Continue;

            if (sLine [1] = '[') then
            begin
                bInSection := IsDevice_ID_Section (sLine);
                Continue;
            end; { if }

            if not (bInSection) then
                Continue;

            if (TrimLeft (sLine)[1] <> '%') then
                Continue;

            if (Extract_PCI_ID (TrimRight (sLine), sPCI_ID{%H-})) then
                Device_ID_List.Add (UpperCase (sPCI_ID));
        end; { for }

        Result := Inf_List.Count > 0;
    end; { Fill_Device_ID_List }

    (* ---- *)

var
    Inf_List : TStringList;

begin { Fill_Device_IDs_List }
    Assert ({%H-}Device_ID_List = NIL);
    Assert (_sInf_File <> '');

    if not (FileExists (_sInf_File)) then
        raise EFileNotFoundException.CreateFmt (SFOpenError, [_sInf_File]);

    Inf_List := TStringList.Create;

    try
        Inf_List.LoadFromFile (_sInf_File { , TEncoding.Unicode});

        Device_ID_List := CreateSortedStringList (dupIgnore, true);

        Fill_Device_ID_List (Inf_List);

    finally
        Inf_List.Free;
    end; { try / finally }
end; { Fill_Device_IDs_List }

(* ---- *)

function FillList (const OutputList: TStrings;
                   const Device_IDs_List: TStrings) : Integer;

    (* ---- *)

    procedure FormatIniFile;

    const
        cIniHeader = '[Devices]'#13#10'Count=%d'#13#10;

    var
        iIndex, iCount : Integer;
        sLine : String;

    begin
    	if (Result > 0) then
        begin
            iCount := 1;

            for iIndex := 0 to OutputList.Count - 1 do
                if (Pos ('[]', OutputList [iIndex]) = 1) then
                begin
                    sLine := OutputList [iIndex];
                    Insert (IntToStr (iCount), sLine, 2);
                    OutputList [iIndex] := sLine;
                    Inc (iCount);
                end; { if }
        end; { if }

    	OutputList.Insert (0, Format (cIniHeader, [Result]));
    end; { FormatIniFile }

    (* ---- *)

var
    Devices : TDevices;
    iIndex : Integer;

begin { FillList }
    Assert (OutputList <> NIL);

    Result := 0;

    Devices := TDevices.Create (true);

    try
        if (Assigned (Device_IDs_List)) then
            for iIndex := 0 to Device_IDs_List.Count - 1 do
                Inc (Result, FillOutputList (OutputList, Devices, _bSaveAsIni,
                                             LowerCase (_sDeviceName),
                                             Device_IDs_List [iIndex],
                                             UpperCase (_sDeviceClass)))
        else Result := FillOutputList (OutputList, Devices, _bSaveAsIni,
                                       LowerCase (_sDeviceName), '',
                                       UpperCase (_sDeviceClass));

    finally
        Devices.Free;
    end; { try / finally }

    if (_bSaveAsIni) then
        FormatIniFile;
end; { FillList }

(* ---- *)

function Main : Integer;

	(* ---- *)

    function OutputFolderExists (const sFileName: String) : Boolean;

    var
    	sFolder : String;

    begin
    	Assert (sFileName <> '');

        Result := false;

        sFolder := ExtractFileDir (sFileName);

        if (sFolder <> '') then
			if (DirectoryExists (sFolder)) then
            	Result := true
            else
            	if (ForceDirectories (sFolder)) then
                	Result := true;
    end; { OutputFolderExists }

    (* ---- *)

var
    OutputList, Device_IDs_List : TStringList;
    iIndex : Integer;
    sMsg : String;

begin { Main }
    Result := $FF;
    Device_IDs_List := NIL;

    if (IsConsole) then
        WriteLn;

    if ((ParamCount > 0) and (Pos ('?', ParamStr (1)) <> 0)) then
    begin
        if (IsConsole) then
            WriteLn (cParamsMsg)
        else
            if (CheckWin32Version (5, 1)) then
                MsgBoxTimeOut (cParamsMsg, 60, mb_IconInformation)
            else MsgBox (cParamsMsg, mb_IconInformation);

        exit;
    end { if }
    else if (CheckParams = false) then
        exit;

    try
        if (_sInf_File <> '') then
            Fill_Device_IDs_List (Device_IDs_List)
        else if (_sDevice_ID <> '') then
        begin
            Device_IDs_List := TStringList.Create;
            Device_IDs_List.Add (UpperCase (_sDevice_ID));
        end; { else if }

    	if (IsConsole) then
            Win32Check (CoInitialize (NIL) = S_OK);

        try
            OutputList := TStringList.Create;

            try
                Result := FillList (OutputList, Device_IDs_List);

				if (IsConsole) then
                    for iIndex := 0 to OutputList.Count - 1 do
                        WriteLn (OutputList [iIndex]);

                if (ParamCount > 0) then
                	if (OutputList.Count > 0) then
						try
                    		OutputList.SaveToFile (ParamStr (1));

                        except
                            on E: Exception do
                            	if (CheckWin32Version (5, 1)) then
                                	MsgBoxTimeOut (E.Message, 15, mb_IconStop)
                                else MsgBox (E.Message, mb_IconStop)
                        end { try / except }
                    else if (FileExists (ParamStr (1))) then
                    	SysUtils.DeleteFile (ParamStr (1));

            finally
                if (Assigned (Device_IDs_List)) then
                    Device_IDs_List.Free;

                OutputList.Free;
            end; { try / finally }

        finally
            if (IsConsole) then
                CoUninitialize;
        end; { try / finally }

    except
    	on E: Exception do
        begin
			if (IsConsole) then
        	    Writeln (E.ClassName, ': ', E.Message)
            else
            begin
                sMsg := E.ClassName + ': ' + E.Message;

                if (CheckWin32Version (5, 1)) then
                    MsgBoxTimeOut (sMsg, 60, mb_IconStop)
                else MsgBox (sMsg, mb_IconStop);
            end; { else }

            Result := 0;
        end; { on E: Exception do }
    end; { try / except }
end; { Main }

(* ---- *)

end.
