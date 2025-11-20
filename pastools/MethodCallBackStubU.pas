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

// http://stackoverflow.com/questions/8283490/how-to-turn-a-method-to-a-callback-procedure-in-64bit-delphi-xe2

// https://code.google.com/p/mustangpeakcommonlib/
// https://github.com/TurboPack/MustangpeakCommonLib
// MPCommonUtilities.pas

// Helpers to create a callback function out of a object method
{ ----------------------------------------------------------------------------- }
{ This is a piece of magic by Jeroen Mineur.  Allows a class method to be used  }
{ as a callback. Create a stub using CreateStub with the instance of the object }
{ the callback should call as the first parameter and the method as the second  }
{ parameter, ie @TForm1.MyCallback or declare a type of object for the callback }
{ method and then use a variable of that type and set the variable to the       }
{ method and pass it:                                                           }
{ 64-bit code by Andrey Gruzdev                                                 }
{                                                                               }
{ type                                                                          }
{   TEnumWindowsFunc = function (AHandle: hWnd; Param: lParam): BOOL of object; stdcall; }
{                                                                               }
{  TForm1 = class(TForm)                                                        }
{  private                                                                      }
{    function EnumWindowsProc(AHandle: hWnd; Param: lParam): BOOL; stdcall;     }
{  end;                                                                         }
{                                                                               }
{  var                                                                          }
{    MyFunc: TEnumWindowsFunc;                                                  }
{    Stub: ICallbackStub;                                                       }
{  begin                                                                        }
{    MyFunct := EnumWindowsProc;                                                }
{    Stub := TCallBackStub.Create(Self, MyFunct, 2);                            }
{     ....                                                                      }
{     ....                                                                      }
{  Now Stub.StubPointer can be passed as the callback pointer to any windows API}
{  The Stub will be automatically disposed when the interface variable goes out }
{  of scope                                                                     }
{ ----------------------------------------------------------------------------- }

{$WARN SYMBOL_PLATFORM OFF}

unit MethodCallBackStubU;

interface

// Helpers to create a callback function out of a object method
type
    ICallbackStub = interface (IInterface)
    	function GetStubPointer: Pointer;
      	property StubPointer : Pointer read GetStubPointer;
    end; { ICallbackStub }

    TCallbackStub = class (TInterfacedObject, ICallbackStub)
      private
        fStubPointer : Pointer;
        fCodeSize : integer;
        function GetStubPointer: Pointer;

      public
        constructor Create(Obj : TObject; MethodPtr: Pointer; {%H-}NumArgs : integer);
        destructor Destroy; override;
    end; { TCallbackStub }

implementation

uses Windows, SysUtils;

{$IFNDEF CPUX64}
const
  AsmPopEDX = $5A;
  AsmMovEAX = $B8;
  AsmPushEAX = $50;
  AsmPushEDX = $52;
  AsmJmpShort = $E9;

type
  TStub = packed record
    PopEDX: Byte;
    MovEAX: Byte;
    SelfPointer: Pointer;
    PushEAX: Byte;
    PushEDX: Byte;
    JmpShort: Byte;
    Displacement: Integer;
  end;
{$ENDIF}

(* ---- *)

constructor TCallBackStub.Create (Obj: TObject; MethodPtr: Pointer;
                                  NumArgs: integer);
{$IFNDEF CPUX64}
var
  Stub: ^TStub;

begin
  // Allocate memory for the stub
  // 1/10/04 Support for 64 bit, executable code must be in virtual space
  Stub := VirtualAlloc (nil, SizeOf (TStub), MEM_COMMIT, PAGE_EXECUTE_READWRITE);

  // Pop the return address off the stack
  Stub^.PopEDX := AsmPopEDX;

  // Push the object pointer on the stack
  Stub^.MovEAX := AsmMovEAX;
  Stub^.SelfPointer := Obj;
  Stub^.PushEAX := AsmPushEAX;

  // Push the return address back on the stack
  Stub^.PushEDX := AsmPushEDX;

  // Jump to the 'real' procedure, the method.
  Stub^.JmpShort := AsmJmpShort;
  Stub^.Displacement := ({%H-}Integer(MethodPtr) - {%H-}Integer(@(Stub^.JmpShort))) -
    (SizeOf (Stub^.JmpShort) + SizeOf (Stub^.Displacement));

  // Return a pointer to the stub
  fCodeSize := SizeOf (TStub);
  fStubPointer := Stub;
{$ELSE CPUX64}
const
  RegParamCount = 4;
  ShadowParamCount = 4;

  Size32Bit = 4;
  Size64Bit = 8;

  ShadowStack   = ShadowParamCount * Size64Bit;
  SkipParamCount = RegParamCount - ShadowParamCount;

  StackSrsOffset = 3;
  c64stack: array[0..14] of byte = (
    $48, $81, $ec, 00, 00, 00, 00,//     sub rsp,$0
    $4c, $89, $8c, $24, ShadowStack, 00, 00, 00//     mov [rsp+$20],r9
    );

  CopySrcOffset=4;
  CopyDstOffset=4;
  c64copy: array[0..15] of byte = (
    $4c, $8b, $8c, $24,  00, 00, 00, 00,//     mov r9,[rsp+0]
    $4c, $89, $8c, $24, 00, 00, 00, 00//     mov [rsp+0],r9
    );

  RegMethodOffset = 10;
  RegSelfOffset = 11;
  c64regs: array[0..28] of byte = (
    $4d, $89, $c1,      //   mov r9,r8
    $49, $89, $d0,      //   mov r8,rdx
    $48, $89, $ca,      //   mov rdx,rcx
    $48, $b9, 00, 00, 00, 00, 00, 00, 00, 00, // mov rcx, Obj
    $48, $b8, 00, 00, 00, 00, 00, 00, 00, 00 // mov rax, MethodPtr
    );

  c64jump: array[0..2] of byte = (
    $48, $ff, $e0  // jump rax
    );

  CallOffset = 6;
  c64call: array[0..10] of byte = (
    $48, $ff, $d0,    //    call rax
    $48, $81,$c4,  00, 00, 00, 00,   //     add rsp,$0
    $c3// ret
    );

var
    i :  Integer;
    P,PP,Q : PByte;
    lCount  : Integer;
    lSize  : Integer;
    lOffset  : Integer;

begin
    lCount := SizeOf (c64regs);
    if (NumArgs >= RegParamCount) then
    	Inc (lCount, SizeOf (c64stack) + (NumArgs - RegParamCount) *
        			 SizeOf (c64copy) + SizeOf (c64call))
    else Inc (lCount, SizeOf (c64jump));

    Q := VirtualAlloc (nil, lCount, MEM_COMMIT, PAGE_EXECUTE_READWRITE);
    P := Q;

    lSize := 0;
    if NumArgs>=RegParamCount then
    begin
        lSize := ( 1+ ((NumArgs + 1 - SkipParamCount) div 2) * 2 )* Size64Bit;   // 16 byte stack align

        pp := p;
        move(c64stack,P^,SizeOf (c64stack));
        Inc(P,StackSrsOffset);
        move(lSize,P^,Size32Bit);
        p := pp;
        Inc(P,SizeOf (c64stack));

        for I := 0 to NumArgs - RegParamCount - 1 do
        begin
            pp := p;
            move(c64copy,P^,SizeOf (c64copy));
            Inc(P,CopySrcOffset);
            lOffset := lSize + (i+ShadowParamCount + 1)*Size64Bit;
            move(lOffset,P^,Size32Bit);
            Inc(P,CopyDstOffset+Size32Bit);
            lOffset := (i+ShadowParamCount + 1)*Size64Bit;
            move(lOffset,P^,Size32Bit);
            p := pp;
            Inc(P,SizeOf (c64copy));
        end;
    end;

    pp := p;
    move(c64regs,P^,SizeOf (c64regs));
    Inc(P,RegSelfOffset);
    move(Obj,P^,SizeOf (Obj));
    Inc(P,RegMethodOffset);
    move(MethodPtr,P^,SizeOf (MethodPtr));
    p := pp;
    Inc(P,SizeOf (c64regs));

    if NumArgs<RegParamCount then
      move(c64jump,P^,SizeOf (c64jump))
    else
    begin
      move(c64call,P^,SizeOf (c64call));
      Inc(P,CallOffset);
      move(lSize,P^,Size32Bit);
    end;

    fCodeSize := lcount;
    fStubPointer := Q;
{$ENDIF CPUX64}
end; { TCallBackStub.Create }

(* ---- *)

destructor TCallBackStub.Destroy;
begin
    Win32Check (VirtualFree (fStubPointer, 0, MEM_RELEASE));

    inherited;
end; { TCallBackStub.Destroy }

(* ---- *)

function TCallBackStub.GetStubPointer : Pointer;
begin
	Result := fStubPointer;
end; { TCallBackStub.GetStubPointer }

(* ---- *)

end.

