unit Int64Em;

{
  Inno Setup
  Copyright (C) 1997-2004 Jordan Russell
  Portions by Martijn Laan
  For conditions of distribution and use, see LICENSE.TXT.

  Declaration of the Integer64 type - which represents an *unsigned* 64-bit
  integer value - and functions for manipulating Integer64's.
  (We can't use the Int64 type since it's only available in Delphi 4 and
  later.)

  $jrsoftware: issrc/Projects/Int64Em.pas,v 1.9 2004/11/07 19:23:06 jr Exp $
}

interface

type
  Integer64 = packed record
    Lo, Hi: LongWord;
  end;

function Compare64(const N1, N2: Integer64): Integer;
procedure Inc64(var X: Integer64; N: LongWord);

implementation

function Compare64(const N1, N2: Integer64): Integer;
{ If N1 = N2, returns 0.
  If N1 > N2, returns 1.
  If N1 < N2, returns -1. }
asm
  { Compare high words }
  mov  ecx, [eax+4]
  cmp  ecx, [edx+4]
  ja   @@return1
  jb   @@returnminus1
  { High words equal; compare low words }
  mov  ecx, [eax]
  cmp  ecx, [edx]
  ja   @@return1
  jb   @@returnminus1
  jmp  @@return0
@@return1:
  xor  eax, eax
  inc  eax
  jmp  @@exit
@@returnminus1:
  or   eax, -1
  jmp  @@exit
@@return0:
  xor  eax,eax
@@exit:
end;

procedure Inc64(var X: Integer64; N: LongWord);
asm
  add  [eax], edx
  adc  dword ptr [eax+4], 0
end;

end.
