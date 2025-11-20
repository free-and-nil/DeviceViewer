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

unit SortedListU;

interface

uses Classes;

type
	TSortedListCompare = function (Item1, Item2: Pointer) : Integer;
	TListSortKeyOf = function (const pItem: Pointer) : Pointer;
	TStringCompare = function (lpString1, lpString2: PChar) : Integer; stdcall;
    TOnDestroyListEvent = procedure (ObjectList: TList) of object;

    TSortedList = class (TList)
      private
        FDuplicates : TDuplicates;
        FAllowInserts : Boolean;

        bSorted : Boolean;

      public
        SortedListCompare : TSortedListCompare;
        SortedListKeyOf : TListSortKeyOf;

      	constructor Create (const bSorted: Boolean = true);

        function Add (const pItem: Pointer) : Integer;
        function IndexOf (const pItem: Pointer) : Integer;
        procedure Insert (const iIndex: Integer; const pItem: Pointer);
        function Search (pKey: Pointer; var iIndex: Integer) : Boolean;

        property Duplicates : TDuplicates read FDuplicates write FDuplicates
        					  default dupIgnore;
        property AllowInserts : Boolean read FAllowInserts write FAllowInserts;
    end; { TSortedList }

    TIntegerList = class;

    TIntegerListEnumerator = class
      private
      	FIndex : Integer;
        FList : TIntegerList;

      public
        constructor Create (const List: TIntegerList);
        function GetCurrent : Integer;
        function MoveNext : Boolean;
        property Current : Integer read GetCurrent;
    end; { TIntegerListEnumerator }

    TIntegerList = class (TSortedList)
      private
      	FObjects : TList;
        FOnDestroyList : TOnDestroyListEvent;

        function GetObject (iIndex: Integer) : TObject;
      	function GetValue (iIndex: Integer) : Integer;

      public
      	constructor Create (const bSorted: Boolean = true;
        					const bContainsObjects: Boolean = false);
        destructor Destroy; override;

        function Add (const iItem: Integer) : Integer;
        function AddObject (const iItem: Integer;
        					const AObject: TObject) : Integer;
        function GetEnumerator : TIntegerListEnumerator;
        function Search (const iValue: Integer; var iIndex: Integer) : Boolean;

        property Objects [iIndex: Integer] : TObject read GetObject;
        property Values [iIndex: Integer] : Integer read GetValue; default;
        property OnDestroyList : TOnDestroyListEvent read FOnDestroyList
                                                     write FOnDestroyList;
    end; { TIntegerList }

    TNativeUIntList = class;

    TNativeUIntListEnumerator = class
      private
      	FIndex : Integer;
        FList : TNativeUIntList;
        FOnDestroyList : TOnDestroyListEvent;

      public
        constructor Create (const List: TNativeUIntList);
        function GetCurrent : NativeUInt;
        function MoveNext : Boolean;
        property Current : NativeUInt read GetCurrent;
        property OnDestroyList : TOnDestroyListEvent read FOnDestroyList
                                                     write FOnDestroyList;
    end; { TNativeUIntListEnumerator }

    TNativeUIntList = class (TSortedList)
      private
      	FObjects : TList;
        FOnDestroyList : TOnDestroyListEvent;

        function GetObject (iIndex: Integer) : TObject;
      	function GetValue (iIndex: Integer) : NativeUInt;

      public
      	constructor Create (const bSorted: Boolean = true;
        					const bContainsObjects: Boolean = false);
        destructor Destroy; override;

        function Add (const uItem: NativeUInt) : Integer;
        function AddObject (const uItem: NativeUInt;
        					const AObject: TObject) : Integer;
        function GetEnumerator : TNativeUIntListEnumerator;
        function Search (const uValue: NativeUInt;
                         var iIndex: Integer) : Boolean;

        property Objects [iIndex: Integer] : TObject read GetObject;
        property Values [iIndex: Integer] : NativeUInt read GetValue; default;
        property OnDestroyList : TOnDestroyListEvent read FOnDestroyList
                                                     write FOnDestroyList;
    end; { TNativeUIntList }

    TSortedStringList = class (TSortedList)
      private
        FCaseSensitive : Boolean;

        procedure SetCaseSensitive (bCaseSensitive: Boolean);

      public
        constructor Create (const bSorted: Boolean = true);

        property CaseSensitive : Boolean read FCaseSensitive
        								 write SetCaseSensitive;
    end; { TSortedStringList }

(**
TSortedList.Compare     (method)

Syntax

function Compare(Key1, Key2: Pointer): Integer; virtual;

Description

(Override: Always)  Compare is an abstract method that must be overridden in
				    all descendant types.

Compare should compare the two key values, and return a result as follows:

-1	if Key1 < Key2
 0	if Key1 = Key2
 1	if Key1 > Key2

Key1 and Key2 are Pointer values, as extracted from their corresponding
collection items by the KeyOf method. The Search method implements a binary
search through the collection's items using Compare to compare the items.


function TSortedList.KeyOf (const pItem: Pointer) : Pointer;
{ KeyOf zeigt auf das Element in der Liste, das zum Vergleich benutzt werden
  soll }

begin
    KeyOf := pItem;
end; { TSortedList.KeyOf }

**)

implementation

uses RTLConsts, SysUtils;

(* ---- *)

constructor TSortedList.Create (const bSorted: Boolean = true);
begin
	inherited Create;

	Self.bSorted := bSorted;
end; { TSortedList.Create }

(* ---- *)

function TSortedList.Add (const pItem: Pointer) : Integer;
{ Einen Eintrag sortiert in die Liste einfügen. Gibt die Position zurück. }

begin
	if (bSorted) then
    begin
		Assert (Assigned (SortedListKeyOf));

        if (Search (SortedListKeyOf (pItem), Result{%H-})) then
            case FDuplicates of
                dupIgnore : Exit;
                dupError : raise EListError.CreateFmt (SDuplicateItem, [0]);
            end; { case }

        inherited Insert (Result, pItem);
    end { if }
    else Result := inherited Add (pItem);
end; { TSortedList.Add }

(* ---- *)

function TSortedList.IndexOf (const pItem: Pointer) : Integer;
{ Den Index des Elements "pItem" in der Liste ermitteln.
  <<- IndexOf : Den Index (von 0 an) oder -1 im Fehlerfall. }

begin
	Assert (Assigned (SortedListKeyOf));

    if not (Search (SortedListKeyOf (pItem), Result{%H-})) then
    	Result := (-1);
end; { TSortedList.IndexOf }

(* ---- *)

procedure TSortedList.Insert (const iIndex: Integer; const pItem: Pointer);
begin
	if (FAllowInserts) then
    	inherited Insert (iIndex, pItem)
    else raise EListError.Create (SSortedListError);
end; { TSortedList.Insert }

(* ---- *)

function TSortedList.Search (pKey: Pointer; var iIndex: Integer) : Boolean;
{ Returns True if the item identified by Key is found in the sorted collection.
  If the item is found, iIndex is set to the found index; otherwise, iIndex is
  set to the index where the item would be placed if inserted. }

var
    L, H, I, C : Integer;

begin
    Assert (Assigned (SortedListCompare));
	Assert (Assigned (SortedListKeyOf));

    Result := false;

    L := 0;
    H := Count - 1;

    if (H = (-1)) then
    begin { Das erste Element in der Liste }
    	iIndex := 0;
        exit;
    end; { if }

    while (L <= H) do
    begin
        I := (L + H) shr 1; { Durch zwei teilen }

        C := SortedListCompare (SortedListKeyOf (Items [I]), pKey);

        if (C < 0) then
            L := I + 1
        else
        begin
            H := I - 1;

            if (C = 0) then
                Result := true;
        end; { else }
    end; { while }

    iIndex := L;
end; { TSortedList.Search }

(* ---- *)

function IntListCompare (pKey1, pKey2: Pointer) : Integer;
begin
	if ({%H-}Integer (pKey1) < {%H-}Integer (pKey2)) then
		Result := (-1)
	else if ({%H-}Integer (pKey1) > {%H-}Integer (pKey2)) then
		Result := 1
	else Result := 0;
end; { IntListCompare }

(* ---- *)

function IntListKeyOf (const pItem: Pointer) : Pointer;
begin
	Result := pItem;
end; { IntListKeyOf }

(* ---- *)

constructor TIntegerListEnumerator.Create (const List: TIntegerList);
begin
	inherited Create;

    FList := List;
    FIndex := (-1);
end; { TIntegerListEnumerator.Create }

(* ---- *)

function TIntegerListEnumerator.GetCurrent : Integer;
begin
	Assert (FIndex >= 0);
	Result := FList.GetValue (FIndex);
end; { TIntegerListEnumerator.GetCurrent }

(* ---- *)

function TIntegerListEnumerator.MoveNext : Boolean;
begin
	if (FIndex < Pred (FList.Count)) then
    begin
		Result := true;
    	Inc (FIndex);
    end { if }
    else Result := false;
end; { TIntegerListEnumerator.MoveNext }

(* ---- *)

function TIntegerList.GetObject (iIndex: Integer) : TObject;
begin
	Result := TObject (FObjects [iIndex]);
end; { TIntegerList.GetObject }

(* ---- *)

function TIntegerList.GetValue (iIndex: Integer) : Integer;
begin
	Result := {%H-}Integer (Items [iIndex]);
end; { TIntegerList.GetValue }

(* ---- *)

constructor TIntegerList.Create (const bSorted: Boolean = true;
        						 const bContainsObjects: Boolean = false);
begin
	inherited Create (bSorted);

    SortedListCompare := IntListCompare;
    SortedListKeyOf := IntListKeyOf;

    if (bContainsObjects) then
    	FObjects := TList.Create;
end; { TIntegerList.Create }

(* ---- *)

destructor TIntegerList.Destroy;
begin
    if (Assigned (FOnDestroyList)) then
        FOnDestroyList (FObjects);

    FObjects.Free;

  	inherited;
end; { TIntegerList.Destroy }

(* ---- *)

function TIntegerList.Add (const iItem: Integer) : Integer;
begin
	Result := inherited Add ({%H-}Pointer (iItem));

    if (Assigned (FObjects)) then
    	FObjects.Insert (Result, NIL);
end; { TIntegerList.Add }

(* ---- *)

function TIntegerList.AddObject (const iItem: Integer;
								 const AObject: TObject) : Integer;
begin
	Result := inherited Add ({%H-}Pointer (iItem));
    FObjects.Insert (Result, Pointer (AObject));
end; { TIntegerList.AddObject }

(* ---- *)

function TIntegerList.GetEnumerator : TIntegerListEnumerator;
begin
	Result := TIntegerListEnumerator.Create (Self);
end; { TIntegerList.GetEnumerator }

(* ---- *)

function TIntegerList.Search (const iValue: Integer;
							  var iIndex: Integer) : Boolean;
begin
	Result := inherited Search ({%H-}Pointer (iValue), iIndex);
end; { TIntegerList.Search }

(* ---- *)

function StringListCompareCaseSensitive (Item1, Item2: Pointer) : Integer;
begin
	Result := StrComp (PChar (Item1), PChar (Item2));
end; { StringListCompareCaseSensitive }

(* ---- *)

function StringListCompareCaseInsensitive (Item1, Item2: Pointer) : Integer;
begin
	Result := StrIComp (PChar (Item1), PChar (Item2));
end; { StringListCompareCaseInsensitive }

(* ---- *)

procedure TSortedStringList.SetCaseSensitive (bCaseSensitive: Boolean);
begin
	FCaseSensitive := bCaseSensitive;

    if (FCaseSensitive) then
    	SortedListCompare := StringListCompareCaseSensitive
    else SortedListCompare := StringListCompareCaseInsensitive;
end; { TSortedStringList.SetCaseSensitive }

(* ---- *)

constructor TSortedStringList.Create (const bSorted: Boolean = true);
begin
	inherited;

    SetCaseSensitive (FCaseSensitive);
end; { TSortedStringList.Create }

(* ---- *)

function NativeUIntListCompare (pKey1, pKey2: Pointer) : Integer;
begin
{$IFDEF CPUX64}
	if ({%H-}NativeUInt (pKey1) < {%H-}NativeUInt (pKey2)) then
		Result := (-1)
	else if ({%H-}NativeUInt (pKey1) > {%H-}NativeUInt (pKey2)) then
		Result := 1
	else Result := 0;
{$ELSE}
	if ({%H-}Cardinal (pKey1) < {%H-}Cardinal (pKey2)) then
		Result := (-1)
	else if ({%H-}Cardinal (pKey1) > {%H-}Cardinal (pKey2)) then
		Result := 1
	else Result := 0;
{$ENDIF}
end; { NativeUIntListCompare }

(* ---- *)

function NativeUIntListKeyOf (const pItem: Pointer) : Pointer;
begin
	Result := pItem;
end; { NativeUIntListKeyOf }

(* ---- *)

constructor TNativeUIntListEnumerator.Create (const List: TNativeUIntList);
begin
	inherited Create;

    FList := List;
    FIndex := (-1);
end; { TNativeUIntListEnumerator.Create }

(* ---- *)

function TNativeUIntListEnumerator.GetCurrent : NativeUInt;
begin
	Assert (FIndex >= 0);
	Result := FList.GetValue (FIndex);
end; { TNativeUIntListEnumerator.GetCurrent }

(* ---- *)

function TNativeUIntListEnumerator.MoveNext : Boolean;
begin
	if (FIndex < Pred (FList.Count)) then
    begin
		Result := true;
    	Inc (FIndex);
    end { if }
    else Result := false;
end; { TNativeUIntListEnumerator.MoveNext }

(* ---- *)

function TNativeUIntList.GetObject (iIndex: Integer) : TObject;
begin
	Result := TObject (FObjects [iIndex]);
end; { TNativeUIntList.GetObject }

(* ---- *)

function TNativeUIntList.GetValue (iIndex: Integer) : NativeUInt;
begin
	Result := {%H-}NativeUInt (Items [iIndex]);
end; { TNativeUIntList.GetValue }

(* ---- *)

constructor TNativeUIntList.Create (const bSorted, bContainsObjects: Boolean);
begin
	inherited Create (bSorted);

    SortedListCompare := NativeUIntListCompare;
    SortedListKeyOf := NativeUIntListKeyOf;

    if (bContainsObjects) then
    	FObjects := TList.Create;
end; { TNativeUIntList.Create }

(* ---- *)

destructor TNativeUIntList.Destroy;
begin
    if (Assigned (FOnDestroyList)) then
        FOnDestroyList (FObjects);

    FObjects.Free;

  	inherited;
end; { TNativeUIntList.Destroy }

(* ---- *)

function TNativeUIntList.Add (const uItem: NativeUInt) : Integer;
begin
	Result := inherited Add ({%H-}Pointer (uItem));

    if (Assigned (FObjects)) then
    	FObjects.Insert (Result, NIL);
end; { TNativeUIntList.Add }

(* ---- *)

function TNativeUIntList.AddObject (const uItem: NativeUInt;
                                    const AObject: TObject) : Integer;
begin
	Result := inherited Add ({%H-}Pointer (uItem));
    FObjects.Insert (Result, Pointer (AObject));
end; { TNativeUIntList.AddObject }

(* ---- *)

function TNativeUIntList.GetEnumerator : TNativeUIntListEnumerator;
begin
	Result := TNativeUIntListEnumerator.Create (Self);
end; { TNativeUIntList.GetEnumerator }

(* ---- *)

function TNativeUIntList.Search (const uValue: NativeUInt;
                                 var iIndex: Integer) : Boolean;
begin
	Result := inherited Search ({%H-}Pointer (uValue), iIndex);
end; { TNativeUIntList.Search }

(* ---- *)

end.


