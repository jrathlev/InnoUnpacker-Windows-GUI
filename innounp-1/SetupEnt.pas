unit SetupEnt;

{
  Inno Setup
  Copyright (C) 1997-2004 Jordan Russell
  Portions by Martijn Laan
  For conditions of distribution and use, see LICENSE.TXT.

  Functions for handling records with embedded long strings

  $jrsoftware: issrc/Projects/SetupEnt.pas,v 1.6 2009/05/27 10:03:49 mlaan Exp $
}

interface

uses
  Compress;

procedure SECompressedBlockRead(const R: TAbstractBlockReader; var Buf;
  const Count: Cardinal; const NumWideStrings, NumAnsiStrings: Integer);
procedure SECompressedBlockSkip(const R: TAbstractBlockReader;
  const Count: Cardinal; const NumWideStrings, NumAnsiStrings: Integer);

implementation

uses Main;

procedure SECompressedBlockRead(const R: TAbstractBlockReader; var Buf;
  const Count: Cardinal; const NumWideStrings, NumAnsiStrings: Integer);
var
  P: Pointer;
  I: Integer;
  Len: Integer;
  S: WideString;
  AnsiS: AnsiString;
begin
  P := @Buf;
  for I := 1 to NumWideStrings do begin
    R.Read(Len, SizeOf(Len));
    SetLength(S, Len div SizeOf(WideChar));
    if Len > 0 then R.Read(S[1], Len);
    if Ver>=6000 then String(P^) := S
    else WideString(P^) := S;
    Inc(Cardinal(P), SizeOf(Pointer));
  end;
  for I := 1 to NumAnsiStrings do begin
    R.Read(Len, SizeOf(Len));
    SetLength(AnsiS, Len);
    if Len > 0 then R.Read(AnsiS[1], Len);
    AnsiString(P^) := AnsiS;
    Inc(Cardinal(P), SizeOf(Pointer));
  end;
  R.Read(P^,Count - (Cardinal(NumWideStrings + NumAnsiStrings) * SizeOf(Pointer)));
end;

procedure SECompressedBlockSkip(const R: TAbstractBlockReader;
  const Count: Cardinal; const NumWideStrings, NumAnsiStrings: Integer);
var
//  P: Pointer;
  I: Integer;
  Len: Integer;
  S: WideString;
  AnsiS: AnsiString;
begin
//  P := @Buf;
  for I := 1 to NumWideStrings do begin
    R.Read(Len, SizeOf(Len));
    SetLength(S, Len div SizeOf(WideChar));
    if Len > 0 then
      R.Read(S[1], Len);
//    String(P^) := S;
//    Inc(Cardinal(P), SizeOf(Pointer));
  end;
  for I := 1 to NumAnsiStrings do begin
    R.Read(Len, SizeOf(Len));
    SetLength(AnsiS, Len);
    if Len > 0 then
      R.Read(AnsiS[1], Len);
//    AnsiString(P^) := AnsiS;
//    Inc(Cardinal(P), SizeOf(Pointer));
  end;
  R.Skip(Count - (Cardinal(NumWideStrings + NumAnsiStrings) * SizeOf(Pointer)));
end;

end.
