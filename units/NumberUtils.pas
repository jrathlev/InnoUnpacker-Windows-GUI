(* Delphi Unit
   number conversions
   ==================

   - conversion of float numbers to engeneering format and to values with unit prefixes
   - conversion Deg:Min:Sec -> String and back
   - conversion of Hex, Octal und Binary <-> string
   - conversion of set <-> hex

   © Dr. J. Rathlev, D-24222 Schwentinental (kontakt(a)rathlev-home.de)

   The contents of this file may be used under the terms of the
   Mozilla Public License ("MPL") or
   GNU Lesser General Public License Version 2 or later (the "LGPL")

   Software distributed under this License is distributed on an "AS IS" basis,
   WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
   the specific language governing rights and limitations under the License.

   Vers. 1 - Jun. 1989
   Vers. 2 - May 2015
   last modified:  April 2020
   *)

unit NumberUtils;

interface

uses System.SysUtils, System.Types, System.Math;

const
  PrefixCount = 11;
  Prefixes   : array [1..PrefixCount-1] of AnsiChar = ('f','p','n','µ','m',' ','k','M','G','T');

type
  TNumMode = (nmDecimal,nmHex,nmOctal,nmBin,nmZeroDec);

  EinhStr = string[5];
  TAngleFormat = (afDecDegree,afDecMinutes,afSeconds);

  TSetArray = array [1..32] of byte;
  PSetArray = ^TSetArray;

  TWord = record
    case integer of
    0 : (Both : word);
    1 : (Lo,Hi : byte);
    end;

  TLongWord = record
    case integer of
    0 : (LongWord : cardinal);
    1 : (Lo,Hi : word);
    2 : (LoL,LoH,HiL,HiH : byte);
    3 : (Bytes : array [0..3] of Byte);
    end;

  TInt64 = record
    case integer of
    0: (AsInt64 : int64);
    1: (Lo, Hi  : Cardinal);
    2: (Cardinals: array [0..1] of Cardinal);
    3: (Words: array [0..3] of Word);
    4: (Bytes: array [0..7] of Byte);
    end;

{ ---------------------------------------------------------------- }
// Cardinal in Hi- oder Lo-Wort zerlegen
function HiWord (n : cardinal) : word;

function LoWord (n : cardinal) : word;

// Word aus Hi- und Lo-Byte zusammensetzen
function BytesToWord (HiByte,LoByte: byte) : word;

// Cardinal aus Hi- und Lo-Wort zusammensetzen
function WordsToCardinal (HiWord,LoWord : word) : cardinal;

// Cardinal aus 4 Bytes zusammensetzen
function BytesToCardinal (HHByte,HLByte,LHByte,LLByte : word) : cardinal;

// Int64 aus Hi- und Lo-Cardinal zusammensetzen
function CardinalToInt64 (HiCard,LoCard : cardinal) : int64;

{ ---------------------------------------------------------------- }
// Bestimmung des nächstkleineren ganzzahligen Anteils einer Zahl
function IInt (x : double) : integer;
function AInt (x : double) : double;

// (Vorzeichen von arg2) * arg1
function ISign (x,y : integer) : integer;
function ASign (x,y : double) : double;

(* Typumwandlung prüfen *)
function CheckValue (s : string;
                     DecSep : char = #0) : boolean;

(* an Dezimalstelle runden *)
function DecimalRound (Value : double; Decimal : integer) : double;

(* Normalisieren eines Wertes in den Bereich 0.1 .. 999.9 *)
procedure Normal (var Value,Factor : extended;
                  var PrefixIndex  : integer);

(* Zahlenwert mit nachfolgendem Einheitenvorsatz erzeugen *)
function FloatToStrP (Value : extended;
                      n,nd  : integer;
                      Einh  : EinhStr = '';
                      DecSep  : char = #0) : string;

(* Zahlenwert aus String mit nachfolgendem Einheitenvorsatz erzeugen *)
function PrefixStrToVal (ValStr    : string;
                         var Value : extended;
                         DecSep    : char = #0) : boolean; overload;

function PrefixStrToVal (ValStr    : string;
                         var Value : double;
                         DecSep    : char = #0) : boolean; overload;

(* Zahlen auf Float umsetzen - optional: spez. Dezimaltrenner*)
function FloatToStrG (Value   : extended;
                      Format  : TFloatFormat;
                      Precision,Digits: Integer;
                      DecSep  : char = #0) : string;

(* Zahlen auf "Fix-Format" umsetzen *)
function FloatToStrX (Value   : extended;
                      Precision,Digits: Integer;
                      DecSep  : char = #0) : string;

(* Zahlen auf "Sci-Format" umsetzen *)
function FloatToStrS (Value   : extended;
                      Digits  : integer;
                      DecSep  : char = #0) : string;

(* Zahlen auf "Eng-Format" umsetzen *)
function FloatToStrE (Value   : extended;
                      Digits  : integer;
                      DecSep  : char = #0) : string;

(* Zahlen auf "Eng-Format" mit Vorsätzen (m,k,..)  umsetzen *)
function FloatToPrefixStr (Value     : extended;
                           Digits    : integer;
                           Separator : string = '';
                           DecSep    : char = #0) : string;

(* Zahlen auf "ffXixed-Format" umsetzen *)
function FloatToFixStr (Value     : extended;
                        Digits    : integer;
                        DecSep    : char = #0) : string;

(* In Winkelgrade umrechnen *)
function DegToStr (v : extended;
                   Format : TAngleFormat;
                   w,d : integer;
                   DecSep  : char = #0) : string;

function StrToDeg (s : string;
                   var v : extended;
                   DecSep  : char = #0) : boolean;

// Integer-Zahl in String umsetzen
function StrInt (x : int64;
                 n : integer = 0) : string;

// Integer-Zahl in String mit führenden Nullen umsetzen
function ZStrInt (x : int64;
                  n : integer) : string;

// Stellenanzahl einer Integerzahl
function NumberOfDigits (Value : int64) : integer;

// Integer-Zahl als Hex-Zahl, Digits = Mindestanzahl der Stellen,
//      Group = Gruppierung von Stellen
function IntToHexStr (Value,Digits,Group : cardinal) : string;

// Integer-Zahl als Dezimal-Zahl, Digits = Mindestanzahl der Stellen,
//      Group = Gruppierung von Stellen
function IntToDecimal (Value : int64; Digits,Group : cardinal;
                       Zeros : boolean = false) : string;

// Integer-Zahl als Oktal-Zahl, Digits = Mindestanzahl der Stellen,
//      Group = Gruppierung von Stellen
function IntToOctal (Value,Digits,Group : cardinal) : string;

// Integer-Zahl als Binär-Zahl, Digits = Mindestanzahl der Stellen
//      Group = Gruppierung von Stellen
function IntToBin (Value,Digits,Group : cardinal) : string;

// Konvertiere String als Hexwert (ohne $)
function TryHexStrToInt64(S: string; var val : int64) : boolean;
function TryHexStrToInt(const S : string; var val : integer) : boolean;
function HexStrToInt (const S : string) : int64;

// Konvertiere String als Oktalwert
function TryOctalStrToInt64(S: string; var val : int64) : boolean;
function TryOctalStrToInt(const S : string; var val : integer) : boolean;
function OctalStrToInt (const S : string) : int64;

// Konvertiere String als Binärwert
function TryBinStrToInt64(S: string; var val : int64) : boolean;
function TryBinStrToInt(const S : string; var val : integer) : boolean;
function BinStrToInt (const S : string): int64;

// Convert boolean to adjustable string
function BooleanToString(B : boolean; const sTrue : string = 'TRUE';
  sFalse : string = 'FALSE') : string;

// Bytes, je nach Größe in String umsetzen
function SizeToStr (Bytes : int64; Decimal : boolean = false;
  NoZeroDecimals : boolean = false; DecSep : char = #0) : string;

// String in Dateigröße wandeln
function StrToSize (s : string; var size : int64; DecSep : char = #0) : boolean;

// Umwandeln einer Menge in einen Hex-String und zurück
function SetToHex (ASet : PSetArray; Low,High : integer) : string;
procedure HexToSet (const S : string; ASet: PSetArray);

function SetToWord (ASet : PSetArray) : word;
procedure WordToSet (n : word; ASet : PSetArray);

//=====================================================================
implementation

uses UnitConsts;

const
  PfFak : array [0..PrefixCount-1] of extended = (1.0,1E15,1E12,1E9,1E6,1E3,
                                        1.0,1E-3,1E-6,1E-9,1E-12);
  FFak : array [-2..8] of double = (0.01,0.1,1.0,10.0,100.0,1000.0,1E4,1E5,1E6,1E7,1E8);
  NFak : array [1..2] of integer = (10,100);

{ ------------------------------------------------------------------- }
(* alle Leerstellen im String löschen *)
function RemoveSpaces (const S : string) : string;
var
  i  : integer;
begin
  Result:=s; i:=1;
  while (i<=length(Result)) do begin
    if Result[i]=' ' then delete(Result,i,1) else inc(i);
    end;
  end;

function InsertChars (const s : string; ch : char; n : integer) : string;
var
  i  : integer;
begin
  Result:=s;
  for i:=succ(length(Result)) to n do Result:=ch+Result;
  end;

function ReplaceChars (const s         : string;
                       OldChar,NewChar : char) : string;
var
  i  : integer;
begin
  Result:=s;
  for i:=1 to length(Result) do if Result[i]=OldChar then Result[i]:=NewChar;
  end;

{ ---------------------------------------------------------------- }
(* Cardinal in Hi- oder Lo-Wort zerlegen *)
function HiWord (n : cardinal) : word;
var
  Long : TLongWord absolute n;
begin
  HiWord:=Long.Hi;
  end;

function LoWord (n : cardinal) : word;
var
  Long : TLongWord absolute n;
begin
  LoWord:=Long.Lo;
  end;

(* Word aus Hi- und Lo-Byte zusammensetzen *)
function BytesToWord (HiByte,LoByte: byte) : word;
var
  Long : TWord;
begin
  with Long do begin
    Hi:=HiByte; Lo:=LoByte;
    Result:=Both;
    end;
  end;

(* Cardinal aus Hi- und Lo-Wort zusammensetzen *)
function WordsToCardinal (HiWord,LoWord : word) : cardinal;
var
  Long : TLongWord;
begin
  with Long do begin
    Hi:=HiWord; Lo:=LoWord;
    Result:=LongWord;
    end;
  end;

(* Cardinal aus 4 Bytes zu sammensetzen *)
function BytesToCardinal (HHByte,HLByte,LHByte,LLByte : word) : cardinal;
var
  Long : TLongWord;
begin
  with Long do begin
    LoL:=LLbyte; LoH:=LHByte;
    HiL:=HLByte; HiH:=HHByte;
    Result:=LongWord;
    end;
  end;

(* Int64 aus Hi- und Lo-Cardinal zusammensetzen *)
function CardinalToInt64 (HiCard,LoCard : cardinal) : int64;
var
  Long : TInt64;
begin
  with Long do begin
    Hi:=HiCard; Lo:=LoCard;
    Result:=AsInt64;
    end;
  end;

(* ----------FUNCTION--AINT---------------------------------------------
   Bestimmung des nächstkleineren ganzzahligen Anteils einer Zahl
*)
function AInt (x : double) : double;
var
  xx  : double;
begin
  xx:=int(x);
  if (x<0.0) and (x<>xx) then xx:=xx-1.0;
  Result:=xx;
  end;

function IInt (x : double) : integer;
begin
  Result:=round(AInt(x));
  end;

(* ----------FUNCTION--ISIGN--------------------------------------------
  (Vorzeichen von arg2) * arg1
*)
function ISign (x,y : integer) : integer;
begin
  if y<0 then Result:=-x else if y>0 then Result:=x else Result:=0;
  end;

(* ----------FUNCTION--SIGN---------------------------------------------
  (Vorzeichen von arg2) * arg1
*)
function ASign (x,y : double) : double;
begin
  if y<0.0 then Result:=-x else if y>0.0 then Result:=x else Result:=0.0;
  end;

(* ----------PROCEDURE-CheckValue---------------------------------------- *)
function CheckValue (s : string;
                     DecSep : char = #0) : boolean;
var
  fs   : TFormatSettings;
begin
  fs:=FormatSettings;
  if DecSep<>#0 then fs.DecimalSeparator:=DecSep;
  Result:=true;
  if length(s)>0 then begin
    try
      StrToFloat(s,fs);
    except
      on EConvertError do Result:=false;
      end;
    end;
  end;

(* ----------FUNCTION DECIMALROUND------------------------------------------- *)
(* an Dezimalstelle runden *)
function DecimalRound (Value : double; Decimal : integer) : double;
var
  v : integer;
begin
  if Decimal<-2 then Decimal:=-2;
  if Decimal>8 then Decimal:=8;
  v:=round(abs(Value)*FFak[Decimal]);
  Result:=v/FFak[Decimal];
  if Value<0 then Result:=-Result;
  end;

(* ----------PROCEDURE-NORMAL------------------------------------------- *)
(* Normalisieren eines Wertes in den Bereich 0.1 .. 999.9 *)
procedure Normal (var Value,Factor : extended;
                  var PrefixIndex  : integer);
var
  AVal : double;
  k    : integer;
begin
  AVal:=abs(Value);
  if (AVal<1E-15) or (AVal>1E15) then k:=6
  else begin
    k:=1;
    if (AVal>=0.999E-13) then inc(k);
    if (AVal>=0.999E-10) then inc(k);
    if (AVal>=0.999E-7) then inc(k);
    if (AVal>=0.999E-4) then inc(k);
    if (AVal>=0.999E-1) then inc(k);
    if (AVal>=0.999E3) then inc(k);
    if (AVal>=0.999E6) then inc(k);
    if (AVal>=0.999E9) then inc(k);
    if (AVal>=0.999E12) then inc(k);
    end;
  Factor:=PfFak[k];
  Value:=Value*Factor;
  PrefixIndex:=k;
  end;

(* ----------FUNCTION--PREFIXVAL---------------------------------------- *)
(* Zahlenwert mit nachfolgendem Einheitenvorsatz erzeugen *)
function FloatToStrP(Value   : extended;
                     n,nd    : integer;
                     Einh    : EinhStr = '';
                     DecSep  : char = #0) : string;
(* n    = Anzahl der Stellen
        = 0: automatische Stellenanpassung
   nd   = Anzahl der Stellen hinter dem Komma
          <0 : automatisch max. -(nd+1) Stellen hinter dem Komma
   Einh = nachfolgendes Einheitenzeichen *)
var
  s    : string;
  pf   : AnsiChar;
  f    : extended;
  i    : integer;
  fs   : TFormatSettings;
begin
  fs:=FormatSettings;
  if DecSep<>#0 then fs.DecimalSeparator:=DecSep;
  Normal (Value,f,i); pf:=Prefixes[i];
  if nd<0 then begin
    nd:=abs(nd)-2;
    if abs(Value)<100.0 then inc(nd);
    if abs(Value)<10.0 then inc(nd);
    if abs(Value)<1.0 then inc(nd);
    if abs(Value)<0.0999 then nd:=0;
    if nd<0 then nd:=0;
    end;
  if n=0 then begin
    n:=3+nd; if nd>0 then inc(n);
    if abs(Value)<100.0 then dec(n);
    if abs(Value)<10.0 then dec(n);
    end;
  if Value<0.0 then inc(n);
  s:=FloatToStrF(Value,ffFixed,n,nd,fs);
  if Einh='' then begin
    if pf<>' ' then Result:=s+pf
    else Result:=s;
    end
  else if pf<>' ' then Result:=s+' '+pf+Einh
    else Result:=s+' '+Einh+' ';
  end;

(* ----------FUNCTION--VALPREFIX---------------------------------------- *)
(* Zahlenwert aus String mit nachfolgendem Einheitenvorsatz erzeugen *)
function PrefixStrToVal (ValStr    : string;
                         var Value : extended;
                         DecSep    : char = #0) : boolean;
(* PrefixVal = true : ok
             = false : Fehler bei der Konvertierung *)
var
  k,n  : integer;
  c    : Char;
  fs   : TFormatSettings;
begin
  fs:=FormatSettings;
  if DecSep<>#0 then fs.DecimalSeparator:=DecSep;
  Result:=false;
  ValStr:=Trim(ValStr);
  (* Prüfe letztes Zeichen *)
  n:=length(ValStr);
  if n>0 then begin
    c:=ValStr[n];
    Value:=0.0; k:=0;
    if (c<'0') or (c>'9') then begin
      k:=1;
      while (k<PrefixCount) and (WideChar(Prefixes[k])<>c) do inc(k);
      end;
    if k<PrefixCount then begin
      if k>0 then begin
        dec(n);
        while (n>0) and (ValStr[n]=' ') do dec(n);
        end;
      if TryStrToFloat(copy(ValStr,1,n),Value,fs) then begin
        Value:=Value/PfFak[k];
        Result:=true;
        end;
      end
    end
  else begin
    Value:=0; Result:=true;
    end;
  end;

function PrefixStrToVal (ValStr    : string;
                         var Value : double;
                         DecSep    : char = #0) : boolean;
var
  LValue: Extended;
begin
  Result:=PrefixStrToVal (ValStr,LValue,DecSep);
  if Result then if (LValue<-MaxDouble) or (LValue>MaxDouble) then Result := False;
  if Result then Value:=LValue;
  end;

(* ----------FUNCTION--FloatToEng---------------------------------------- *)
procedure FloatToEng (Value   : extended;
                      n       : integer;
                      var Mant : string;
                      var Exp : integer;
                      DecSep  : char = #0);
// Value   = Wert
// n       = Anzahl der signifikanten Stellen
// DecSep  = Dez.trenner
// Mant    = Mantisse mit n signif. Stellen
// Exp     = Exponent in 3er-Stufen
var
  f    : extended;
  nd,v : integer;
begin
  if DecSep=#0 then DecSep:=FormatSettings.DecimalSeparator;
  Normal (Value,f,Exp);
  if n<=1 then n:=2;
  if n>8 then n:=8;
  nd:=0;
  if abs(Value)>=0.99999 then inc(nd);
  if abs(Value)>=9.9999 then inc(nd);
  if abs(Value)>=99.999 then inc(nd);
  v:=round(abs(Value)*FFak[n-nd]);
  if n<nd then v:=v*NFak[nd-n];
  if v=0 then begin
    Mant:='0'+DecSep;
    for nd:=1 to n do Mant:=Mant+'0';
    end
  else begin
    Mant:=IntToStr(v);
    if nd>0 then begin
      if n>nd then insert(DecSep,Mant,nd+1);
      end
    else Insert('0'+DecSep,Mant,1);
    if Value<0 then Insert('-',Mant,1);
    end;
  end;

(* Zahlen auf "Eng-Format" umsetzen (3er Potenzen) *)
function FloatToStrE (Value   : extended;
                      Digits  : integer;
                      DecSep  : char = #0) : string;
(* n    = Anzahl der signifikanten Stellen *)
var
  s : string;
  k : integer;
begin
  if Digits=1 then Result:=IntToStr(round(Value))
  else begin
      if Digits=0 then begin
      FloatToEng (Value,4,s,k,DecSep);
      if (Pos(DecSep,s)>0) then begin  // automatisch
        while s[length(s)]='0' do delete(s,length(s),1);
        if s[length(s)]=DecSep then delete(s,length(s),1);
        end;
      end
    else FloatToEng (Value,Digits,s,k,DecSep);
    if k<>6 then Result:=s+'E'+IntToStr((k-6)*3)
    else Result:=s;
    end;
  end;

(* ----------FUNCTION--FloatToPlotString---------------------------------------- *)
(* Zahlen auf "Eng-Format" mit Vorsätzen (m,k,..)  umsetzen *)
function FloatToPrefixStr (Value     : extended;
                           Digits    : integer;
                           Separator : string = '';
                           DecSep    : char = #0) : string;
(* n    = Anzahl der signifikanten Stellen *)
var
  s   : string;
  k   : integer;
begin
  if Digits=1 then Result:=IntToStr(round(Value))
  else begin
    FloatToEng (Value,Digits,s,k,DecSep);
    if (k<1) or (k>=PrefixCount) then Result:=s+'E'+IntToStr((k-6)*3)
    else if k<>6 then Result:=s+Separator+Prefixes[k] else Result:=s;
    end;
  end;

(* ----------FUNCTION--FloatToStringG---------------------------------------- *)
(* Zahlen auf Float umsetzen - optional: spez. Dezimaltrenner*)
function FloatToStrG (Value   : extended;
                      Format  : TFloatFormat;
                      Precision,Digits : Integer;
                      DecSep  : char = #0) : string;
var
  fs : TFormatSettings;
begin
  fs:=FormatSettings;
  if DecSep<>#0 then fs.DecimalSeparator:=DecSep;
  Result:=FloatToStrF (Value,Format,Precision,Digits,fs);
  end;


(* ----------FUNCTION--FloatToStringX---------------------------------------- *)
(* Zahlen auf "Fix-Format" umsetzen *)
function FloatToStrX (Value   : extended;
                      Precision,Digits : Integer;
                      DecSep  : char = #0) : string;
begin
  Result:=FloatToStrG(Value,ffFixed,Precision,Digits,DecSep);
  end;

(* ----------FUNCTION--FloatToStringS---------------------------------------- *)
(* Zahlen auf "Sci-Format" umsetzen  *)
function FloatToStrS (Value   : extended;
                      Digits  : integer;
                      DecSep  : char = #0) : string;
var
  k : integer;
  fs : TFormatSettings;
begin
  if Digits=1 then Result:=IntToStr(round(Value))
  else begin
    fs:=FormatSettings;
    if DecSep<>#0 then fs.DecimalSeparator:=DecSep;
    if Digits=0 then begin
      Result:=FloatToStrF(Value,ffExponent,4,1,fs);
      k:=Pos('E',Result)-1;
      while Result[k]='0' do begin
        delete(Result,k,1); dec(k);
        end;
      if Result[k]=DecSep then delete(Result,k,1);
      end
    else Result:=FloatToStrF(Value,ffExponent,Digits,1,fs);
    k:=Pos('E',Result);
    if (k>0) then begin
      if (Result[k+1]='+') then Delete(Result,k+1,1);  // '+' im Exp. entfernen
      if (Result[k+1]='0') then Delete(Result,k,2);  // 'E0' entfernen
      end;
    end;
  end;


(* ----------FUNCTION--FloatToFixString---------------------------------------- *)
(* Zahlen auf "ffXixed-Format" umsetzen *)
function FloatToFixStr (Value     : extended;
                        Digits    : integer;
                        DecSep    : char = #0) : string;
(* Digits = Anzahl der signifikanten Stellen + 100 * Stellen hinter dem Komma *)
var
  k   : integer;
  fs  : TFormatSettings;
begin
  if Digits=1 then Result:=IntToStr(round(Value))
  else begin
    fs:=FormatSettings;
    if DecSep<>#0 then fs.DecimalSeparator:=DecSep;
    if Digits=0 then Result:=FloatToStrF (Value,ffGeneral,1,1,fs)
    else begin
      k:=Digits div 100; Digits:=Digits mod 100;
      if k=0 then begin
        if abs(Value)<MinDouble then k:=1 else k:=round(aint(Log10(abs(Value))))+1;
        if k<0 then begin
          if k+Digits>=0 then begin
            k:=Digits; inc(Digits);
            end;
          end
        else if Digits>=k then k:=Digits-k
        else k:=0;
        end;
      if k>=0 then Result:=FloatToStrF (Value,ffFixed,Digits,k,fs)
      else Result:=FloatToStrF (Value,ffExponent,Digits,1,fs);
      end;
    end;
  end;

{------------------------------------------------------------------}
(* Winkel-Wert konvertieren *)
function StrToDeg (s     : string;
                   var v : extended;
                   DecSep  : char = #0) : boolean;
var
 t      : String;
 x,y    : extended;
 k      : integer;
 fs     : TFormatSettings;
begin
  fs:=FormatSettings;
  if DecSep<>#0 then fs.DecimalSeparator:=DecSep;
  Result:=false;
  if length(s)<>0 then begin
    k:=pos(':',s);
    if k=0 then k:=pos('/',s);
    if k=0 then k:=pos(#$B0,s);   // °
    if k=0 then Result:=TryStrToFloat(s,v,fs)
    else begin (* Deg:Min *)
      Result:=TryStrToFloat(copy(s,1,pred(k)),x,fs); // degree
      if Result then begin
        t:=copy (s,succ(k),length(s));
        k:=pos(':',t);
        if k=0 then k:=pos('/',t);
        if k=0 then k:=pos(#$27,s);   // '
        if k=0 then begin (* dec. min *)
          if length(t)>0 then begin
            Result:=TryStrToFloat(t,v,fs);
            if Result and (v<60.0) then begin
              if x>=0.0 then v:=x+v/60.0
              else v:=x-v/60.0;
              end
            else Result:=true;
            end
          else v:=x;
          end
        else begin (* sec. *)
          Result:=TryStrToFloat(copy (t,1,pred(k)),y,fs);    // min.
          if Result and (y<60.0) then begin
            t:=copy (t,succ(k),length(t));
            if length(t)>0 then begin
              Result:=TryStrToFloat(t,v,fs);      // sec.
              if Result and (v<60.0) then begin
                if x>=0.0 then v:=x+(y+v/60.0)/60.0
                else v:=x-(y-v/60.0)/60.0;
                end
              else Result:=true;
              end
            else begin
              if x>=0.0 then v:=x+y/60.0
              else v:=x-y/60.0;
              end;
            end
          else Result:=true;
          end;
        end;
      end;
    end
  else v:=0.0;
  end;

(* Winkel in String mit vorgebenem Format umsetzen
   Stellen: w : gesamt
            d : hinter Komma (einschl. Minuten und Sekunden
   Typ : m = afDecDegree  : Grad,xxx
             afDecMinutes : Grad:Minuten,xx
             afSeconds    : Grad:Minuten:Sekunden,xxx *)
function DegToStr(v       : extended;
                  Format  : TAngleFormat;
                  w,d     : integer;
                  DecSep  : char = #0) : string;
var
  s,t   : String;
  x,y,z : extended;
  fs    : TFormatSettings;
begin
  x:=int(abs(v)); y:=60.0*frac(abs(v));
  if DecSep=#0 then DecSep:=FormatSettings.DecimalSeparator;
  fs:=FormatSettings;
  fs.DecimalSeparator:=DecSep;
  case Format of
  afDecDegree : Result:=FloatToStrF(v,ffFixed,w,d,fs); //str(v:w:d,s);
  afDecMinutes : begin (* Grad:Min *)
      t:=FloatToStrF(y,ffFixed,w-2,d-2);  // Minuten
      if copy(t,1,2)='60' then begin
        y:=0.0; x:=x+1.0;
        t:=FloatToStrF(y,ffFixed,w-2,d-2,fs);  // Minuten
        end;
      if y<10 then t:='0'+t;
      if v<0.0 then x:=-x;
      Result:=FloatToStrF(x,ffFixed,2,0,fs)+':'+t; // Grade
      end;
  afSeconds : begin (* Grad:Min:Sek *)
      z:=int(y); y:=60.0*frac(y);
      t:=FloatToStrF(y,ffFixed,w-4,d-4,fs);  // Sekunden
      if copy(t,1,2)='60' then begin
        y:=0.0; z:=z+1.0;
        if z>=60.0 then begin
          z:=0.0; x:=x+1.0;
          end;
        t:=FloatToStrF(y,ffFixed,w-4,d-4,fs);  // Sekunden
        end;
      if y<10 then t:='0'+t;
      s:=FloatToStrF(z,ffFixed,2,0);  // Minuten
      if z<10 then s:='0'+s;
      t:=s+':'+t;
      if v<0.0 then x:=-x;
      Result:=FloatToStrF(x,ffFixed,2,0,fs)+':'+t; // Grade
      end;
    end;
  end;

{ ---------------------------------------------------------------- }
(* Integer-Zahl in String umsetzen *)
function StrInt (x : int64;
                 n : integer) : string;
(* n > 0 : Anzahl der Stellen
     = 0 : unformatiert
     < 0 : unformatiert mit Dreiergruppierung *)
var
  s   : string;
  i,j : integer;
begin
  if n<>0 then begin
    s:=Format('%'+IntToStr(abs(n))+'d',[x]);
    if n<0 then begin
      i:=succ(length(s));
      j:=i;
      while (j>4) do begin
        dec(j,3); insert (FormatSettings.ThousandSeparator,s,j);
        end;
      end;
    end
  else s:=IntToStr(x);
  Result:=s;
  end;

{ ---------------------------------------------------------------- }
(* Integer-Zahl in String mit führenden Nullen umsetzen *)
function ZStrInt (x : int64;
                  n : integer) : string;
begin
  Result:=Format('%.'+IntToStr(n)+'d',[x]);
  end;

{ ---------------------------------------------------------------- }
function GroupDigits (s : string; Group : integer; Sep : char = Space) : string;
var
  i : integer;
begin
  if (Group>0) then begin
    i:=length(s);
    while (i>Group) do begin
      dec(i,Group); Insert(Sep,s,i+1);
      end;
    end;
  Result:=s;
  end;

{ ---------------------------------------------------------------- }
function NumberOfDigits (Value : int64) : integer;
begin
  if Value<0 then Result:=1 else Result:=0;
  Value:=abs(Value);
  repeat
    Value:=Value div 10;
    inc(Result);
    until Value=0;
  end;

{ ---------------------------------------------------------------- }
function IntToHexStr (Value,Digits,Group : cardinal) : string;
begin
  Result:=GroupDigits(IntToHex(Value,Digits),Group);
  end;

{ ---------------------------------------------------------------- }
function IntToDecimal(Value : int64; Digits,Group : cardinal; Zeros : boolean = false) : string;
begin
  if Zeros then Result:=GroupDigits(ZStrInt(Value,Digits),Group)
  else Result:=GroupDigits(StrInt(Value,Digits),Group);
  end;

{ ---------------------------------------------------------------- }
function IntToOctal (Value,Digits,Group : cardinal) : string;
begin
  Result:='';
  repeat
    Result:=chr((Value and 7)+48)+Result;
    Value:=Value div 8;
    until Value=0;
  Result:=GroupDigits(InsertChars(Result,'0',Digits),Group);
  end;

{ ---------------------------------------------------------------- }
function IntToBin (Value,Digits,Group : cardinal) : string;
begin
  Result:='';
  repeat
    Result:=chr((Value and 1)+48)+Result;
    Value:=Value shr 1;
    until Value=0;
  Result:=GroupDigits(InsertChars(Result,'0',Digits),Group);
  end;

{ ---------------------------------------------------------------- }
function TryHexStrToInt64(S : string; var val : int64) : boolean;
var
  minus : boolean;
begin
  val:=0; Result:=false;
  if length(s)>0 then begin
    minus:=s[1]='-';
    if minus then delete(s,1,1);
    if (length(s)>0) and (s[1]<>'$') then s:='$'+s;
    Result:=TryStrToInt64(RemoveSpaces(s),val);
    if Result and minus then val:=-val;
    end;
  end;

function TryHexStrToInt(const S : string; var val : integer) : boolean;
var
  n : int64;
begin
  n:=val; Result:=TryHexStrToInt64(S,n); val:=n;
  end;

function HexStrToInt (const S : string) : int64;
begin
  Result:=0;
  if not TryHexStrToInt64(S,Result) then
    raise EConvertError.Create(Format(rsHexError,[s]));
  end;

function TryOctalStrToInt64(S : string; var val : int64) : boolean;
var
  i     : integer;
  minus : boolean;
begin
  val:=0; Result:=false;
  if length(s)>0 then begin
    minus:=s[1]='-';
    if minus then delete(s,1,1);
    if (length(s)>0) and (s[1]='%') then delete(s,1,1);
    s:=RemoveSpaces(s);
    for i:=1 to length(s) do begin
      if (s[i]<#48) or (s[i]>#55) then exit
      else val:=8*val+ord(s[i])-48;
      end;
    if minus then val:=-val;
    Result:=true;
    end;
  end;

function TryOctalStrToInt(const S : string; var val : integer) : boolean;
var
  n : int64;
begin
  n:=val; Result:=TryOctalStrToInt64(S,n); val:=n;
  end;

function OctalStrToInt (const S: string) : int64;
begin
  Result:=0;
  if not TryOctalStrToInt64(s,Result) then
    raise EConvertError.Create(Format(rsOctError,[s]));
  end;

{ --------------------------------------------------------------- }
function TryBinStrToInt64(S: string; var val : int64) : boolean;
var
  i     : integer;
  minus : boolean;
begin
  val:=0; Result:=false;
  if length(s)>0 then begin
    minus:=s[1]='-';
    if minus then delete(s,1,1);
    if (length(s)>0) and (s[1]='!') then delete(s,1,1);
    s:=RemoveSpaces(s);
    for i:=1 to length(s) do begin
      if (s[i]<#48) or (s[i]>#49) then Exit
      else val:=2*val+ord(s[i])-48;
      end;
    if minus then val:=-val;
    Result:=true;
    end;
  end;

function TryBinStrToInt(const S : string; var val : integer) : boolean;
var
  n : int64;
begin
  n:=val; Result:=TryBinStrToInt64(S,n); val:=n;
  end;

function BinStrToInt (const S: string) : int64;
begin
  Result:=0;
  if not TryBinStrToInt64(s,Result) then
    raise EConvertError.Create(Format(rsBinError,[s]));
  end;

{ ---------------------------------------------------------------- }
// Convert boolean to adjustable string
function BooleanToString(B : boolean; const sTrue : string = 'TRUE';
  sFalse : string = 'FALSE') : string;
begin
  if B then Result:=sTrue else Result:=sFalse;
  end;

{ ---------------------------------------------------------------- }
const
  sBytes = 'B';
  sKiBytes = 'KiB';    // 1024 byte
  sMiBytes = 'MiB';    // 1024*1024 byte
  sGiBytes = 'GiB';    // 1024*1024*1024 byte
  sKBytes = 'kB';      // 1000 byte
  sMBytes = 'MB';      // 1000*1000 byte
  sGBytes = 'GB';      // 1000*1000*1000 byte
  sGetBytePrefixes = 'KMGT';

  Suffixes : array[boolean,1..3] of string =
    ((sKiBytes,sMiBytes,sGiBytes),(sKBytes,sMBytes,sGBytes));

// Convert number of bytes to string using prefixes from IEC 1996
// DecSep : replace character from system default
// Decimal : use 1000 as divisor instead of 1024
// NoZeroDecimals : do not show zero fractions
function SizeToStr (Bytes : int64; Decimal : boolean = false;
  NoZeroDecimals : boolean = false; DecSep : char = #0) : string;
var
  v : extended;
  n,d : integer;
  fs: TFormatSettings;

  function ZeroFrac (val : extended; Dec : integer) : boolean;
  const
    FVal : array [1..2] of extended = (0.1,0.01);
  begin
    if Dec<1 then Result:=true
    else if Dec>2 then Result:=false
    else Result:=abs(val-round(val))<FVal[Dec];
    end;

begin
  if Decimal then d:=1000 else d:=1024;
  if Bytes<d then Result:=IntToStr(Bytes)+' '+sBytes  // bytes
  else begin
    fs:=FormatSettings;
    if DecSep<>#0 then fs.DecimalSeparator:=DecSep;
    v:=Bytes/d;
    if v<d then begin
      if v<10 then n:=2 else if v<100 then n:=1 else n:=0;
      if NoZeroDecimals and ZeroFrac(v,n) then n:=0;
      Result:=Format('%.'+IntToStr(n)+'f ',[v],fs)+Suffixes[Decimal,1]; // Kibibyte/kilobyte
      end
    else begin
      v:=v/d;
      if v<d then begin
        if v<10 then n:=2 else if v<100 then n:=1 else n:=0;
        if NoZeroDecimals and ZeroFrac(v,n) then n:=0;
        Result:=Format('%.'+IntToStr(n)+'f ',[v],fs)+Suffixes[Decimal,2]// Mebibyte/Megabyte
        end
      else begin
        if NoZeroDecimals and ZeroFrac(v,2) then n:=0 else n:=2;
        Result:=Format('%.'+IntToStr(n)+'f ',[v/d],fs)+Suffixes[Decimal,3]; // Gibibyte/Gigabyte
        end
      end;
    end;
  end;

// Convert string using prefixes from IEC 1996 or SI to bytes
function StrToSize (s : string; var size : int64; DecSep : char = #0) : boolean;
var
  f,x : double;
  i,n,fac : integer;
  c   : char;
  pf  : string;
  fs  : TFormatSettings;
begin
  fs:=FormatSettings;
  if DecSep<>#0 then fs.DecimalSeparator:=DecSep;
  Result:=false; size:=0; fac:=1000;
  pf:=sGetBytePrefixes;
  s:=Trim(s);
  if length(s)=0 then s:='0';
  if UpCase(s[length(s)])=sBytes then delete(s,length(s),1);   // bytes
  if UpCase(s[length(s)])='I' then begin
    delete(s,length(s),1);   // optional "i"
    fac:=1024;
    end;
  s:=Trim(s);
  c:=UpCase(s[length(s)]);
  if (c>='0') then begin
    f:=1;
    if (c>'9') then begin
      n:=pos(' ',s);
      if n<=0 then n:=length(s); //Exit;
      i:=0;
      repeat
        f:=f*fac; inc(i);
        until (i>=length(pf)) or (c=pf[i]);
      if i>=length(pf) then Exit;
      s:=copy(s,1,n-1);
      end;
    Result:=TryStrToFloat(s,x,fs);
    if Result then size:=round(x*f);
    end;
  end;

{ ---------------------------------------------------------------- }
// Umwandeln einer Menge in einen Hex-String und zurück
// Format des Strings: nnaabb.. (nn = Hexwert der nachfolgenden Bytes)
function SetToHex (ASet : PSetArray; Low,High : integer) : string;
var
  i : integer;
  b : byte;
begin
  b:=(High-Low) div 8+1;
  Result:=IntToHex(b,2);
  for i:=1 to b do Result:=Result+IntToHex(ASet^[i],2);
  end;

procedure HexToSet (const S : string; ASet: PSetArray);
var
  i,n : integer;
begin
  if (length(s)>=4) and TryStrToInt('$'+copy(s,1,2),n) then begin
    if length(s)>=2*(n+1) then begin
      if n>32 then n:=32;
      for i:=1 to n do if TryStrToInt('$'+copy(s,2*i+1,2),n) then ASet^[i]:=byte(n);
      end;
    end;
  end;

function SetToWord (ASet : PSetArray) : word;
begin
  Move(ASet^[1],Result,2);
  end;

procedure WordToSet (n : word; ASet : PSetArray);
begin
  Move(n,ASet^[1],2);
  end;

end.
