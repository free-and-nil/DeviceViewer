// https://www.lazarusforum.de/viewtopic.php?t=6232

unit LazarusShellImageListU;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Windows, Controls, Graphics, ShellAPI;

type
  { TShellImageList }

  TShellImageListSize = (isSmall, isLarge);

  TShellImageList = class(TImageList)
  private
    FSHSize: TShellImageListSize;
    FSimpleAddIcon: boolean;
    FTypeNames: TStringList;
    function GetszTypeName(Index: integer): string;
    procedure SetSHSize(AValue: TShellImageListSize);
  protected
    function DoAddIcon(Index: integer; NewIcon: TIcon): boolean; virtual;
    property szTypeNames: TStringList read FTypeNames;
  public
    constructor Create(AOwner: TComponent); override; overload;
    constructor Create(AOwner: TComponent; ASHSize: TShellImageListSize); overload;
    destructor Destroy; override;
    function AddShellIcon(const AFilename: string): integer; overload;
    function AddShellIcon(AIcon: HIcon; const Discription: string): integer; overload;
    function GetIndexbyFilename(const AFilename: string): integer;
    function GetIndexbyTypeName(const ATypeName: string): integer;
    class function GetShellFileInfo(const AName: string; const AFlags: integer; const FileAttributes: integer=0): TSHFileInfo;
    property SHSize: TShellImageListSize read FSHSize write SetSHSize;
    property SimpleAddIcon: boolean read FSimpleAddIcon write FSimpleAddIcon default true;
    property szTypeName[Index: integer]: string read GetszTypeName;
  end;

implementation

{ TShellImageList }

constructor TShellImageList.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FTypeNames:=TStringList.Create;
  SetSHSize(isLarge);
  FSimpleAddIcon:=true;
end;

constructor TShellImageList.Create(AOwner: TComponent;
  ASHSize: TShellImageListSize);
begin
  Create(AOwner);
  SetSHSize(ASHSize);
end;

destructor TShellImageList.Destroy;
begin
  FTypeNames.Free;
  inherited Destroy;
end;

function TShellImageList.GetszTypeName(Index: integer): string;
begin
  result:=FTypeNames[Index];
end;

procedure TShellImageList.SetSHSize(AValue: TShellImageListSize);
begin
  if FSHSize=AValue then Exit;
  FSHSize:=AValue;
  case FSHSize of
    isSmall: begin
      Height:=16;
      Width:=16;
    end;
    isLarge: begin
      Height:=32;
      Width:=32;
    end;
  end;
end;

function TShellImageList.DoAddIcon(Index: integer; NewIcon: TIcon): boolean;
var
  Icon: TIcon;
begin
  result:=true;
  if (not FSimpleAddIcon) AND (Count>0) then begin
    Icon:=TIcon.Create;
    try
      GetIcon(Index, Icon);
      result:=not (CompareMem(Icon.RawImage.Data, NewIcon.RawImage.Data, Width*Height));
    finally
      Icon.Free;
    end;
  end
  else
    result:=Index<0;
end;

function TShellImageList.AddShellIcon(const AFilename: string): integer;
var
  SFI: TSHFileInfo;
  Flags: integer;
begin
  result:=-1;

  Flags:=SHGFI_ICON OR SHGFI_TYPENAME;
  case FSHSize of
    isSmall: Flags:=Flags OR SHGFI_SMALLICON;
    isLarge: Flags:=Flags OR SHGFI_LARGEICON;
  end;

  SFI:=GetShellFileInfo(AFilename, Flags, Windows.FILE_ATTRIBUTE_NORMAL);

  result:=AddShellIcon(SFI.hIcon, SFI.szTypeName);
end;

function TShellImageList.AddShellIcon(AIcon: HIcon; const Discription: string): integer;
var
  Icon: TIcon;
  i: integer;
begin
  result:=-1;
  if (AIcon<>0) then begin
    i:=GetIndexByTypeName(Discription);
    Icon:=TIcon.Create;
    try
      Icon.Handle:=AIcon;
      if DoAddIcon(i, Icon) then begin
        result:=AddIcon(Icon);
        FTypeNames.Add(Discription);
      end
      else
        result:=i;
    finally
      Icon.Free
    end;
  end;
end;

function TShellImageList.GetIndexbyFilename(const AFilename: string): integer;
begin
  result:=GetIndexbyTypeName(GetShellFileInfo(AFilename, SHGFI_TYPENAME).szTypeName);
end;

function TShellImageList.GetIndexbyTypeName(const ATypeName: string): integer;
var
  n: integer;
begin
  result:=-1;
  if FTypeNames.Count>0 then
    for n:=0 to FTypeNames.Count-1 do
      if (FTypeNames[n]=ATypeName) then begin
        result:=n;
        break;
      end;
end;

class function TShellImageList.GetShellFileInfo(const AName: string;
  const AFlags: integer; const FileAttributes: integer): TSHFileInfo;
begin
  ZeroMemory(@result, SizeOf(result));
  SHGetFileInfo(PChar(AName), FileAttributes, result, SizeOf(result), AFlags);
end;

end.

