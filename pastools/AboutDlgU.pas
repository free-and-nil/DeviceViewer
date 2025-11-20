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

unit AboutDlgU;

interface

uses
  SysUtils, Windows, Classes, Graphics, Controls,
  StdCtrls, ExtCtrls, Forms;

type
  TAboutDlg = class (TForm)
	OKBtn: TButton;
    IconImage: TImage;
    Bevel1: TBevel;
    Bevel2: TBevel;
    VerLbl: TLabel;
    CompanyLbl: TLabel;
    CommentLbl: TLabel;
    AppNameLbl: TStaticText;

    procedure FormShow(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);

  private
    AppIcon : TIcon;
    bFreeIconOnClose : Boolean;

    function GetVersionInfo : Boolean;

  public
  	procedure SetIcon (const AIcon: TIcon; const bFreeOnClose: Boolean = true);
  end; { TAboutDlg }

implementation

{$R *.DFM}

uses Delphi_T, {$IFNDEF FPC} VCL_Tool, {$ENDIF} Win32ToolsU;

(* ---- *)

function TAboutDlg.GetVersionInfo : Boolean;

    (* ---- *)

(**
    procedure GetFileTime (const dwLowDateTime, dwHighDateTime: DWord);

    var
        FileTime : TFileTime;
        SystemTime : TSystemTime;
		sDate : string;

    begin
        FileTime.dwLowDateTime := dwLowDateTime;
        FileTime.dwHighDateTime := dwHighDateTime;

        if (FileTimeToSystemTime (FileTime, SystemTime)) then
            with SystemTime do
            begin
                sDate := IntToStr (wDay) + '.' + IntToStr (wMonth) + '.' +
                         IntToStr (wYear);
                DatumLbl.Caption := sDate;
            end { with }
        else DatumLbl.Caption := '???';
    end; { GetFileTime }
**)

    (* ---- *)

const
	casInfo : array [1..4] of String =
	  ('CompanyName', 'ProductName', 'FileVersion', 'Comments');
	cLocaleUS = '040904E4';
	cLocaleGer = '040704E4';
    cValue = 'StringFileInfo\%s\%s';

var
	dwSize, dwLen, dwHandle : DWord;
	pBuffer, pchValue : PChar;
	iPos : Integer;
	sLabel, sVersion, sDate, sComment, sLocale, sAbout : String;
	bResult : Boolean;
    sModuleName : TFileName;

begin { Siehe http://www.inprise.com/devsupport/delphi/ti_list/TI3241.html }
	Result := false;

    sModuleName := GetModuleName;

	dwSize := GetFileVersionInfoSize (PChar (sModuleName), dwHandle{%H-});

	if (dwSize > 0) then
	begin
		pBuffer := AllocMem (dwSize);

		if (GetFileVersionInfo (PChar (sModuleName), 0, dwSize, pBuffer)) then
		begin
        	sLabel := PChar (pBuffer);

			bResult := VerQueryValue (pBuffer,
            						  PChar ('StringFileInfo\' + cLocaleGer +
                                      		 '\' + casInfo [1]),
									  Pointer ({%H-}pchValue), dwLen{%H-});

			if (bResult) then
			begin
				sLocale := cLocaleGer;
				sAbout := 'Über';
			end { if }
			else
			begin
				bResult := VerQueryValue (pBuffer,
                						  PChar ('StringFileInfo\' + cLocaleUS +
                                          		 '\' + casInfo [1]),
										  Pointer (pchValue), dwLen);
				if (bResult) then
				begin
					sLocale := cLocaleUS;
					sAbout := 'About';
				end; { if }
			end; { else }

			if (bResult) then
			begin
				sLabel := pchValue;
				iPos := Pos ('&', sLabel);

				if (iPos > 0) then
					Insert ('&', sLabel, iPos);

				CompanyLbl.Caption := sLabel;
			end { if }
			else CompanyLbl.Caption := '???';

			if (VerQueryValue (pBuffer,
            				   PChar (Format (cValue,
                                              [{%H-}sLocale, casInfo [2]])),
                               Pointer (pchValue), dwLen)) then
			begin
				AppNameLbl.Caption := pchValue;
				Caption := {%H-}sAbout + ' "' + pchValue + '"';
			end { if }
			else AppNameLbl.Caption := '???';

			if (VerQueryValue (pBuffer,
            				   PChar (Format (cValue, [sLocale, casInfo [3]])),
                               Pointer (pchValue), dwLen)) then
				sVersion := pchValue
			else sVersion := '???';

			if (VerQueryValue (pBuffer,
            				   PChar (Format (cValue, [sLocale, casInfo [4]])),
                               Pointer (pchValue), dwLen)) then
				sComment := pchValue
			else sComment := '';

			CommentLbl.Caption := sComment;

			sDate := GetFileDateString (sModuleName);

			VerLbl.Caption := Format ('Version %s    (%s)', [sVersion, sDate]);

			Result := true;
		end; { if }

		FreeMem (pBuffer, dwSize);
	end; { if }
end; { TAboutDlg.GetVersionInfo }

(* ---- *)

procedure TAboutDlg.FormCreate (Sender: TObject);
begin
{$IFDEF FPC}
    Position := poOwnerFormCenter;
{$ELSE}
	CenterOverActiveForm (Self);
{$ENDIF}

    GetVersionInfo;
end; { TAboutDlg.FormCreate }

(* ---- *)

procedure TAboutDlg.FormDestroy (Sender: TObject);
begin
    if (Assigned (AppIcon)) then
    	if (bFreeIconOnClose) then
        	AppIcon.Free;
end; { TAboutDlg.FormDestroy }

(* ---- *)

procedure TAboutDlg.FormShow (Sender: TObject);

var
    iWidth : Integer;

begin
{$IFDEF DEBUG}
	Caption := Caption + ' DEBUG';
{$ENDIF}

    if (Assigned (AppIcon)) then
    	IconImage.Picture.Icon := AppIcon
    else IconImage.Picture.Icon := Application.Icon;

    with TCanvas.Create do
        try
            Handle := GetDC (AppNameLbl.Handle);

            iWidth := TextWidth (AppNameLbl.Caption);

            if (iWidth > AppNameLbl.Width) then
            	AppNameLbl.Font.Size := AppNameLbl.Font.Size - 4;

        finally
        	ReleaseDC (AppNameLbl.Handle, Handle);
            Free;
        end; { try }
end; { TAboutDlg.FormShow }

(* ---- *)

procedure TAboutDlg.SetIcon (const AIcon: TIcon;
							 const bFreeOnClose: Boolean = true);
begin
{$IFDEF DELPHI7_UP}
	Assert (Icon.HandleAllocated);
{$ENDIF}

	Self.Icon := AIcon;
    bFreeIconOnClose := bFreeOnClose;
end; { TAboutDlg.SetIcon }

(* ---- *)

end.


