unit CallOptimizer;

{
  Inno Setup
  Copyright (C) 1997-2004 Jordan Russell
  Portions by Martijn Laan
  For conditions of distribution and use, see LICENSE.TXT.

  Call Instruction Optimizer

  $jrsoftware: issrc/Projects/CallOptimizer.pas,v 1.3 2004/02/25 22:17:01 jr Exp $
}

interface

type
  TCallInstructionOptimizer = class
  private
    FEncode: Boolean;
    FOffset: LongWord;
    FAddr: LongWord;
    FAddrBytesLeft: Cardinal;
  public
    constructor Create(AEncode: Boolean);
    procedure Code(var Buffer; BufSize: Cardinal);
  end;

implementation

{ TCallInstructionOptimizer }

constructor TCallInstructionOptimizer.Create(AEncode: Boolean);
begin
  inherited Create;
  FEncode := AEncode;
  FOffset := 5;  { size of JMP/CALL xx xx xx xx }
end;

procedure TCallInstructionOptimizer.Code(var Buffer; BufSize: Cardinal);
type
  PByteArray = ^TByteArray;
  TByteArray = array[0..$7FFFFFFE] of Byte;
var
  P: PByteArray;
  I: Cardinal;
begin
  P := @Buffer;
  I := 0;
  while I < BufSize do begin
    if FAddrBytesLeft = 0 then begin
      { Does it appear to be a CALL or JMP instruction with a relative 32-bit
        address? If so, we're going to change the address to be relative to the
        beginning instead of to the next instruction. }
      if (P[I] = $E8) or (P[I] = $E9) then begin
        FAddr := FOffset;
        if not FEncode then
          FAddr := -FAddr;
        FAddrBytesLeft := 4;
      end;
    end
    else begin
      Inc(FAddr, P[I]);
      P[I] := Byte(FAddr);
      FAddr := FAddr shr 8;
      Dec(FAddrBytesLeft);
    end;
    Inc(I);
    Inc(FOffset);  { yes, this can wrap around, but that's fine }
  end;
end;

end.
