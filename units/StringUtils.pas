(* Delphi-Unit
   collection of routines for string processing
   ============================================

   - Conversions ANSI <-> OEM
   - Character manipulations (e.g. add/remove charachters)
   - Extract substrings and numbers until next delimiter
   - Filesize as string
   - Routines to parse commandlines
   - Quicksort

   © Dr. J. Rathlev, D-24222 Schwentinental (kontakt(a)rathlev-home.de)

   The contents of this file may be used under the terms of the
   Mozilla Public License ("MPL") or
   GNU Lesser General Public License Version 2 or later (the "LGPL")

   Software distributed under this License is distributed on an "AS IS" basis,
   WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
   the specific language governing rights and limitations under the License.

   New compilation - May 2007
   last modified:  Sept. 2019
   *)

unit StringUtils;

interface

uses System.SysUtils;

type
  TCvTable = array [0..127] of AnsiChar;
  TTextChange = (tcNone,tcLower,tcUpper);

  // Compare function for Quicksort
  TCompFunction = function (const arg1,arg2) : boolean of object;

const
  // code pages
  cpOem = 437;
  cpAnsi = 850;
  cpLatin1 = 1252;
  cpAscii = 20127;
  cpIso8859_1 = 28591;  // Western European (Latin1)
  cpIso8859_15 = 28605; // Euro zone ( Latin 9)

  ChrID = '#';             // Kennzeichner für num. Zeichenwerte
  VertBar = '|';
  SQuote = '''';
  Quote = '"';
  Period = '.';
  Colon = ':';
  Space = #32;
  Semicolon = ';';
  Comma = ',';
  Slash = '/';
  Tab = #9;
  CrLf = #13#10;
  Cr = #13;
  Lf = #10;

  FloatChars : set of char = ['0'..'9',Period,'-','+','E','e'];

  xDef = 1.0;              // Standardwert ReadNxtDbl
  nDef = 1;                // Standardwert ReadNxtInt
  cDef = #0;               // Standardwert ReadNxtChr

  TabToANSI : TCvTable =(
    #$C7,#$FC,#$E9,#$E2,#$E4,#$E0,#$E5,#$E7,#$EA,#$EB,#$E9,#$E8,#$EF,#$EE,#$C4,#$C5,
    #$C9,#$E6,#$C6,#$F4,#$F6,#$F2,#$FB,#$F9,#$FF,#$D6,#$DC,#$A2,#$A3,#$A5,#$20,#$20,
    #$E1,#$ED,#$F3,#$FA,#$F1,#$D1,#$61,#$6F,#$BF,#$20,#$AC,#$BD,#$BC,#$A1,#$AB,#$BB,
    #$20,#$20,#$20,#$A6,#$20,#$20,#$20,#$20,#$20,#$20,#$20,#$20,#$20,#$20,#$20,#$20,
    #$20,#$20,#$20,#$20,#$AD,#$20,#$20,#$20,#$20,#$20,#$20,#$20,#$20,#$20,#$20,#$20,
    #$20,#$20,#$20,#$20,#$20,#$20,#$20,#$20,#$20,#$20,#$20,#$20,#$20,#$20,#$20,#$20,
    #$20,#$DF,#$B6,#$20,#$20,#$20,#$B5,#$20,#$20,#$20,#$20,#$20,#$20,#$20,#$20,#$20,
    #$20,#$B1,#$20,#$20,#$20,#$20,#$F7,#$20,#$B0,#$20,#$B7,#$20,#$B3,#$B2,#$20,#$20);
  TabToOEM : TCvTable =(
    #$20,#$20,#$20,#$20,#$20,#$20,#$20,#$20,#$20,#$20,#$20,#$20,#$20,#$20,#$20,#$20,
    #$20,#$20,#$20,#$20,#$20,#$20,#$20,#$20,#$20,#$20,#$20,#$20,#$20,#$20,#$20,#$20,
    #$20,#$AD,#$9B,#$9C,#$20,#$9D,#$B3,#$15,#$20,#$20,#$20,#$AE,#$AA,#$C4,#$20,#$20,
    #$F8,#$F1,#$FD,#$FC,#$20,#$E6,#$E3,#$20,#$20,#$20,#$20,#$AF,#$AC,#$AB,#$20,#$A8,
    #$41,#$41,#$41,#$41,#$8E,#$8F,#$92,#$80,#$45,#$90,#$45,#$45,#$49,#$49,#$49,#$49,
    #$44,#$A5,#$4F,#$4F,#$4F,#$4F,#$99,#$20,#$20,#$55,#$55,#$55,#$9A,#$59,#$20,#$E1,
    #$85,#$61,#$83,#$61,#$84,#$86,#$91,#$87,#$8A,#$82,#$88,#$89,#$8D,#$A1,#$8C,#$8B,
    #$20,#$A4,#$95,#$A2,#$93,#$6F,#$94,#$F6,#$20,#$97,#$A3,#$96,#$81,#$79,#$20,#$98);

  UpperLetters : TSysCharSet = ['A'..'Z'];
  LowerLetters : TSysCharSet = ['a'..'z'];
  Digits : TSysCharSet = ['0'..'9'];
  HexDigits : TSysCharSet = ['0'..'9','A'..'F','a'..'f'];
  AlphaNum : TSysCharSet = ['0'..'9','A'..'Z','a'..'z'];

{ ---------------------------------------------------------------- }
// Umwandeln eines DOS-Zeichens in eines Großbuchstaben (auch Umlaute)
function OEMUpCase (c : AnsiChar) : AnsiChar;

// Umwandeln eines ANSI-Zeichens in eines Großbuchstaben (auch Umlaute)
function ANSIUpCase (c : AnsiChar) : AnsiChar;

// Umwandeln eines DOS-Strings in Großbuchstaben (auch Umlaute)
function OEMUpString (const S : AnsiString) : AnsiString;

// Umwandeln eines ANSI-Strings in Großbuchstaben (auch Umlaute)
function ANSIUpString (const S : AnsiString) : AnsiString;

// Umwandeln von ISO-8859-Zeichen für eine Sortierung
function ANSISortConvert (const S : string) : string;
function ANSISortUpper (const S : string) : string;

// Umwandeln eines Strings in Großbuchstaben (Umlaute in AE, OE, UE) zu Sortierzwecken
function OEMUpSort (const S : AnsiString) : AnsiString;

// Umwandeln eines OEM-Strings nach ANSI
function StrToAnsi (const s : AnsiString) : AnsiString;

// Umwandeln eines ANSI-Strings nach OEM
function StrToOEM (const s : AnsiString) : AnsiString;

// Umwandeln eines Zeichens in Kleinbuchstaben  (fehlt in Unit System)
function LowCase(Ch: WideChar): WideChar;

function IsAnsiStr (const ws : string) : boolean;

{ ---------------------------------------------------------------- }
// Substring-Pos. ohne Unterschreidung groß/klein
function TextPos (const Substr, S: string): Integer;
function TextPosEx (const Substr, S: string; Offset : cardinal): Integer;

// Substring-Pos. von rechts ohne Unterscheidung groß/klein
function RightTextPos(const Substr,s : string) : integer;
function RightTextPosEx (const Substr, S: string; Offset : cardinal): Integer;

// prüfen, ob Text bestimmte Zeichen enthält
function ContainsCharacters (const AText : string; AChars : array of Char) : boolean;

// Zeichen aus Text entfernen
function RemoveCharacters (const AText : AnsiString; AChars : array of AnsiChar) : AnsiString; overload;
function RemoveCharacters (const AText : string; AChars : array of Char) : string; overload;

// Text auf Groß- oder Kleinschreibung ändern
function TextChangeCase(const s: string; CaseMode : TTextChange) : string;

// Info-String aus zwei Zeilen erzeugen
function MakeInfoStr (const s1,s2 : string) : string;

// erster Buchstabe groß
function SetInitial(const s : string) : string;

{ ---------------------------------------------------------------- }
// String mit Leerstellen erzeugen
function FillSpace (len : integer) : string;

// führende Leerstellen ergänzen
function AddSp (const S : string;
                len     : integer) : string;

// Leerstellen am Ende ergänzen
function ExtSp (const S : string;
                len   : integer) : string;

// führende Leerstellen löschen
function RemSp (const S : string) : string;

// Leerstellen am Ende löschen
function CutSp (const S : string) : string;

// alle Leerstellen im String löschen
function RemoveSpaces (const S : string) : string;
function DelSp (const S : string) : string;

// mehrfache Leerstellen im String löschen
function DelMultSp (const S : string) : string;

// Steuerzeichen im String löschen bzw. ersetzen
function DelCtrlChars(const S : string) : string;
function CrLfToChar (s : string; c : char) : string;
function CharToCr (s : string; c : char) : string;

// erstes nichtleeres Zeichen suchen
function SeekNoSp (const s : string; Offset : cardinal = 1) : integer;

// nächstes Leerzeichen oder Tab suchen
function SeekSpace (const s : string; Offset : cardinal = 1) : integer;

// String mit Zeichen erzeugen
function FillStr (c   : char;
                  len : integer) : string;

// prüfen, ob leerer String
function IsEmptyStr (const s : string) : boolean;
function NotEmptyStr (const s : string) : boolean;

// führende Zeichen ergänzen
function AddChar (const S : string;
                  c       : char;
                  len     : integer) : string;

// Zeichen am Ende ergänzen
function ExtChar (const S : string;
                  c       : char;
                  len     : integer) : string;

// ein Zeichen durch ein anderes ersetzen
function ReplChars (const s         : AnsiString;
                    OldChar,NewChar : AnsiChar) : AnsiString; overload;
function ReplChars (const s         : string;
                    OldChar,NewChar : char) : string; overload;

// alle aktuellen Dezimaltrenner durch Punkt ersetzen
function ReplaceDecSepWithPeriod (const s : string) : string;

// alle Punkte durch aktuellen Dezimlatrenner ersetzen
function ReplacePeriodWithDecSep (const s : string) : string;

// führende Zeichen löschen
function RemChar (const S : string; c : char) : string;

// Zeichen am Ende löschen
function CutChar (const S : string; c : char) : string;

// Zeichen im String löschen
function DelChar (const S : AnsiString; c : AnsiChar) : AnsiString; overload;
function DelChar (const S : string; c : char) : string; overload;

// Leerzeichen gegen _ und / gegen % austauschen oder zurück
function ReplOptionChar (s : string; Back : boolean) : string;

// Zeichen prüfen
function IsLetter (c : char) : boolean;
function IsDigit (c : char) : boolean;
function IsHexDigit (c : char) : boolean;

// Zahl in Dreíergruppen (fix und float)
function InsertThousandSeparators (const NumStr : string; ThSep : char;
                                   OnlyFrac : boolean = false) : string;

// spez. Zeichen in Strings durch Hexzahl (Unicode) ersetzen und umgekehrt
function SpecCharToValue(const ss,sr : string) : string;
function ValueToSpecChar(const FName : string) : string;

// String auf feste Länge bringen (kürzen oder mit Leerzeichen auffüllen)
function FixLength (const S : string;
                    len     : integer) : string;

// Teilstring löschen als Funktion
function DeleteSubString(const s : string; Index,Count : integer)  : string;

// String am Anfang löschen bis ausschließlich "c"
function DeleteUntilChar(const s : string; c : char) : string;

// Anzahl von SubStr in S zählen
function CountSubStr (const ASubStr,AString : string) : integer;

{ ---------------------------------------------------------------- }
(* Integer, Double oder String aus einem String s bis zum nächsten
   Begrenzer (Del) lesen
   s wird um den verarbeiteten Teil gekürzt *)
function ReadNxtInt (var s   : String;
                     Del     : char;
                     Default : int64;
                     var err : boolean) : int64; overload;

function ReadNxtInt (var s   : String;
                     Del     : char;
                     Default : int64) : int64; overload;

function ReadNxtDbl (var s   : String;
                     Del     : char;
                     Default : double;
                     var err : boolean;
                     DecSep : char = #0) : double; overload;

function ReadNxtDbl (var s   : String;
                     Del     : char;
                     Default : double;
                     DecSep : char = #0) : double; overload;

function ReadNxtStr (var s   : AnsiString;
                     Del     : AnsiChar) : AnsiString; overload;
function ReadNxtStr (var s   : AnsiString;
                     Del     : AnsiChar;
                     Default : AnsiString) : AnsiString; overload;
function ReadNxtStr (var s   : String;
                     Del     : char) : string; overload;
function ReadNxtStr (var s   : String;
                     Del     : char;
                     Default : string) : string; overload;

function TryReadNxtInt (s : string; Del : char; var Value : integer) : boolean;

function GetNxtStr (const s   : String; Del : char) : string;
function GetTrimStr (const S : string; Del : char) : string;
function GetNxtInt (const S : string; Del : char; Default : int64) : int64;

function ReadNxtWord (var s : string;
                      Dels  : TSysCharSet) : string;

function ReadNxtChr (var s   : String;
                     Del     : char;
                     Default : char;
                     var err : boolean) : char; overload;

function ReadNxtChr (var s   : String;
                     Del     : char;
                     Default : char) : char; overload;

function ReadNxtQuotedStr (var s : string;
                           ADelim,AQuote : char) : string;

function MakeQuotedStr (const s : string; CheckChars : array of char) : string;
function ExtractEnclosedString(const AString : string; EncChar : Char; var Pos : integer) : string;

function ReplaceEnvString(const AString,AEnv : string) : string;
function ReplaceEnvVariables (const ALine : string) : string;

function MergeStringList (const ACommaText : string) : string;
function DelimitedTextToLines (DelimitedText : string; Delimiter,QuoteChar : char; var ACount : integer) : string;
function CommaTextToLines (Commatext : string; var ACount : integer) : string; overload
function CommaTextToLines (Commatext : string) : string; overload
function CountLines (const Text : string) : integer;

// duplicate all quote characters (e..g &, ")
function DuplicateQuotes(const S: string; Quote : Char): string;

// Position of char in string starting at "Offset", ignore parts between quotes
function PosChar (const s: string; AChar,AQuote : char; Offset: Cardinal): Integer;

function GetPluralString (const sNo,sOne,sMany : string; n : integer) : string; overload;
function GetPluralString (const sNo,sOne,sMany : string; n : integer; const s : string) : string; overload;
function GetPluralString (const sOne,sMany : string; n : integer) : string; overload;

{ ---------------------------------------------------------------- }
// Check if AText matches filters: AFilter="<filter1><sep><filter2><sep>.."
function MatchesFilter(const AText,AFilter : string; Sep : char) : boolean;

{ ---------------------------------------------------------------- }
// Routinen zur Auswertung einer Befehlszeile
// prüfe, ob die ersten Zeichen einer Option mit dem Parameter übereinstimmen
function CompareOption (const Param,Option : string) : boolean;

// Option vom Typ /option:value einlesen
function ReadOptionValue (var Param : string; const Option : string) : boolean;

// Steuerdatei mit Startoptionen einlesen
function ReadOptionFile (FName : string) : string;

{ ---------------------------------------------------------------- }
// schnelles Sortieren einer Liste
procedure QuickSort (var SList; Count,RSize : Integer; Compare : TCompFunction);

implementation

uses System.StrUtils, System.Masks, System.Classes, ExtSysUtils;

{ ---------------------------------------------------------------- }
(* Umwandeln eines Zeichens in eines Großbuchstaben (auch Umlaute)
   OEM-Zeichen *)
function OEMUpCase (c : AnsiChar) : AnsiChar;
begin
  if c<#127 then Result:=upcase(c)
  else begin
    case c of
    '„' : Result:='Ž';
    '”' : Result:='™';
    '' : Result:='š';
    'á' : Result:='S';
      else Result:=c;
      end;
    end;
  end;

{ ---------------------------------------------------------------- }
(* Umwandeln eines Zeichens in eines Großbuchstaben (auch Umlaute)
   ANSI-Zeichen *)
function ANSIUpCase (c : AnsiChar) : AnsiChar;
begin
  if c<#224 then Result:=upcase(c)
  else Result:=AnsiChar(ord(c)-32);
{  begin
    case c of
    'ä' : Result:='Ä';
    'ö' : Result:='Ö';
    'ü' : Result:='Ü';
    'ß' : Result:='S';
      else Result:=c;
      end;
    end;          }
  end;

{ ---------------------------------------------------------------- }
(* Umwandeln eines Strings in Großbuchstaben (auch Umlaute) *)
function OEMUpString (const S : AnsiString) : AnsiString;
var
  i  : integer;
  ns : AnsiString;
  c  : AnsiChar;
begin
  ns:='';
  for i:=1 to length(s) do begin
    c:=s[i];
    if c<#127 then ns:=ns+upcase(c)
    else begin
      case c of
      '„' : ns:=ns+'Ž';
      '”' : ns:=ns+'™';
      '' : ns:=ns+'š';
      'á' : ns:=ns+'SS';
        else ns:=ns+c;
        end;
      end;
    end;
  OEMUpString:=ns;
  end;

{ ---------------------------------------------------------------- }
(* Umwandeln eines ANSI-Strings in Großbuchstaben (auch Umlaute) *)
function ANSIUpString (const S : AnsiString) : AnsiString;
var
  i  : integer;
  ns : AnsiString;
  c  : AnsiChar;
begin
  ns:='';
  for i:=1 to length(s) do begin
    c:=s[i];
    if c<#127 then ns:=ns+upcase(c)
    else begin
      case c of
      'ä' : ns:=ns+'Ä';
      'ö' : ns:=ns+'Ö';
      'ü' : ns:=ns+'Ü';
      'ß' : ns:=ns+'SS';
        else ns:=ns+c;
        end;
      end;
    end;
  ANSIUpString:=ns;
  end;

{ ---------------------------------------------------------------- }
(* Umwandeln von ISO-8859-Zeichen für eine Sortierung *)
function ANSISortConvert (const S : string) : string;
var
  i  : integer;
  c  : char;
begin
  Result:='';
  for i:=1 to length(s) do begin
    c:=s[i];
    if c<#127 then Result:=Result+c
    else begin
      case c of
      'Ä' : Result:=Result+'Ae';
      'Ö' : Result:=Result+'Oe';
      'Ü' : Result:=Result+'Ue';
      'ä' : Result:=Result+'ae';
      'ö' : Result:=Result+'oe';
      'ü' : Result:=Result+'ue';
      'ß' : Result:=Result+'ss';
      #$C6 : Result:=Result+#$5B; // nach 'Z'  - dänisch
      #$D8 : Result:=Result+#$5C; // nach 'Z'
      #$C5 : Result:=Result+#$5D; // nach 'Z'
      #$E6 : Result:=Result+#$7B; // nach 'z'
      #$F8 : Result:=Result+#$7C; // nach 'z'
      #$E5 : Result:=Result+#$7D; // nach 'z'
      #$C0..#$C3 : Result:=Result+'A';
      #$C7 : Result:=Result+'C';
      #$C8..#$CB : Result:=Result+'E';
      #$CC..#$CF : Result:=Result+'I';
      #$D0 : Result:=Result+'D';
      #$D1 : Result:=Result+'N';
      #$D2..#$D5 : Result:=Result+'O';
      #$D9..#$DB :  Result:=Result+'U';
      #$DD : Result:=Result+'Y';
      #$E0..#$E3 : Result:=Result+'a';
      #$E7 : Result:=Result+'c';
      #$E8..#$EB : Result:=Result+'e';
      #$EC..#$EF : Result:=Result+'i';
      #$F0 : Result:=Result+'d';
      #$F1 : Result:=Result+'n';
      #$F2..#$F5 : Result:=Result+'o';
      #$F9..#$FB :  Result:=Result+'u';
      #$FD,#$FF : Result:=Result+'y';
      else Result:=Result+c;
        end;
      end;
    end;
  end;

(* Umwandeln von ISO-8859-Zeichen für eine Sortierung mit Großbuchstaben*)
function ANSISortUpper (const S : string) : string;
begin
  Result:=AnsiUpperCase(ANSISortConvert(s));
  end;

{ ---------------------------------------------------------------- }
(* Umwandeln eines Strings in Großbuchstaben (Umlaute in AE, OE, UE)
   zu Sortierzwecken *)
function OEMUpSort (const S : AnsiString) : AnsiString;
var
  i  : integer;
  ns : AnsiString;
  c  : AnsiChar;
begin
  ns:='';
  for i:=1 to length(s) do begin
    c:=s[i];
    if c<#127 then ns:=ns+upcase(c)
    else begin
      case c of
      'Ž','„' : ns:=ns+'AE';
      '™','”' : ns:=ns+'OE';
      'š','' : ns:=ns+'UE';
      'á'     : ns:=ns+'SS';
        else ns:=ns+c;
        end;
      end;
    end;
  OEMUpSort:=ns;
  end;

{ ---------------------------------------------------------------- }
(* Umwandeln eines OEM-Strings nach ANSI *)
function StrToAnsi (const s : AnsiString) : AnsiString;
var
  i  : integer;
begin
  Result:=s;
  for i:=1 to length(Result) do begin
    if Result[i]>=#128 then Result[i]:=TabToANSI[ord(Result[i])-128]
    else if Result[i]=#$15 then Result[i]:=#$A7;
    end;
  end;

{ ---------------------------------------------------------------- }
(* Umwandeln eines ANSI-Strings nach OEM *)
function StrToOEM (const s : AnsiString) : AnsiString;
var
  i  : integer;
begin
  Result:=s;
  for i:=1 to length(Result) do begin
    if Result[i]>=#128 then Result[i]:=TabToOEM[ord(Result[i])-128];
    end;
  end;

{ ---------------------------------------------------------------- }
// Umwandeln eines Zeichens in Kleinbuchstaben  (fehlt in Unit System)
function LowCase(Ch: WideChar): WideChar;
begin
  Result:=Ch;
  if (Ch>='A') and (Ch<='Z') then inc(Result,Ord('a') - Ord('A'));
  end;

{ ------------------------------------------------------------------- }
// check if string has characteres >#255
function IsAnsiStr (const ws : string) : boolean;
var
  i : integer;
begin
  Result:=false;
  for i:=1 to length(ws) do if WordRec(ws[i]).Hi<>0 then exit;
  Result:=true;
  end;

{------------------------------------------------------------------}
(* Substring-Pos. ohne Unterschreidung groß/klein *)
function TextPos (const Substr, S: string): Integer;
begin
  Result:=Pos(AnsiUppercase(Substr),AnsiUppercase(s));
  end;

function TextPosEx (const Substr, S: string; Offset : cardinal): Integer;
begin
  Result:=PosEx(AnsiUppercase(Substr),AnsiUppercase(s),Offset);
  end;

(* Substring-Pos. von rechts ohne Unterscheidung groß/klein *)
function RightTextPos(const Substr,s : string) : integer;
begin
  Result:=RightTextPosEx(Substr,s,length(s));
  end;

function RightTextPosEx (const Substr, S: string; Offset : cardinal): Integer;
begin
  Result:=0;
  if length(s)>0 then begin
    Result:=Offset;
    while not AnsiSameText(Substr,copy(s,Result,length(Substr))) and (Result>0) do dec(Result)
    end;
  end;

function ContainsCharacters (const AText : string; AChars : array of Char) : boolean;
var
  i : integer;
begin
  Result:=true;
  for i:=Low(AChars) to High(AChars) do if TextPos(AChars[i],AText)>0 then Exit;
  Result:=false;
  end;

// Zeichen aus Text entfernen
function RemoveCharacters (const AText : AnsiString; AChars : array of AnsiChar) : AnsiString;
var
  i : integer;
begin
  Result:=AText;
  if length(AText)>0 then
    for i:=Low(AChars) to High(AChars) do Result:=AnsiReplaceStr(Result,AChars[i],'');
  end;

function RemoveCharacters (const AText : string; AChars : array of Char) : string;
var
  i : integer;
begin
  Result:=AText;
  if length(AText)>0 then
    for i:=Low(AChars) to High(AChars) do Result:=AnsiReplaceStr(Result,AChars[i],'');
  end;

function SetInitial(const s : string) : string;
begin
  Result:=s;
  if length(s)>0 then Result[1]:=UpCase(Result[1]);
  end;

{ --------------------------------------------------------------- }
// Text auf Groß- oder Kleinschreibung ändern
function TextChangeCase(const s: string; CaseMode : TTextChange) : string;
begin
  if CaseMode=tcLower then Result:=AnsiLowercase(s)
  else if CaseMode=tcUpper then Result:=AnsiUppercase(s)
  else Result:=s;
  end;

{ --------------------------------------------------------------- }
// Info-String aus zwei Zeilen erzeugen
function MakeInfoStr (const s1,s2 : string) : string;
begin
  Result:=StringReplace(s1,'&','&&',[rfReplaceAll])+sLineBreak+
          StringReplace(s2,'&','&&',[rfReplaceAll]);
  end;

{------------------------------------------------------------------}
(* String mit Leerstellen erzeugen *)
function FillSpace (len : integer) : string;
var
  i : integer;
  s : string;
begin
  s:='';
  for i:=1 to len do s:=s+' ';
  Result:=s;
  end;

{------------------------------------------------------------------}
(* führende Leerstellen ergänzen *)
function AddSp (const S : string;
                len     : integer) : string;
var
  i  : integer;
begin
  Result:=s;
  for i:=succ(length(Result)) to len do Result:=' '+Result;
  end;

{------------------------------------------------------------------}
(* Leerstellen am Ende ergänzen *)
function ExtSp (const S : string;
                len     : integer) : string;
var
  i  : integer;
begin
  Result:=s;
  for i:=succ(length(Result)) to len do Result:=Result+' ';
  end;

{------------------------------------------------------------------}
(* führende Leerstellen und Tabs löschen *)
function RemSp (const S : string) : string;
var
  i : integer;
begin
  Result:=s; i:=1;
  while (i<=length(Result)) and ((Result[i]=' ') or (Result[i]=Tab)) do inc(i);
  delete(Result,1,pred(i));
  end;

{ --------------------------------------------------------------- }
(* Leerstellen am Ende löschen *)
function CutSp (const S : string) : string;
var
  i : integer;
begin
  Result:=s; i:=length(Result);
  while (i>0) and (Result[i]=' ') do dec(i);
  delete(Result,succ(i),length(Result));
  end;

{ --------------------------------------------------------------- }
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

function DelSp (const S : string) : string;  // for compatibility
begin
  Result:=RemoveSpaces(s);
  end;

{ --------------------------------------------------------------- }
(* mehrfache Leerstellen im String in einfache umwandeln *)
function DelMultSp (const S : string) : string;
var
  i  : integer;
begin
  Result:=s;
  if length(Result)>0 then begin
    i:=1;
    while (i<length(Result)) do begin
      if (Result[i]=' ') and (Result[i+1]=' ') then delete(Result,i,1) else inc(i);
      end;
    if (Result[i]=' ') then delete(Result,i,1);
    end;
  end;

{ --------------------------------------------------------------- }
(* Steuerzeichen im String löschen *)
function DelCtrlChars(const S : string) : string;
var
  i  : integer;
begin
  Result:=s;
  if length(Result)>0 then begin
    i:=1;
    while (i<=length(Result)) do begin
      if Result[i]<#32 then delete(Result,i,1) else inc(i);
      end;
    end;
  end;

// Cr-Lf im String durch spez. Zeichen ersetzen
function CrLfToChar (s : string; c : char) : string;
begin
  Result:=DelCtrlChars(AnsiReplaceStr(s,#$0D,c));
  end;

// spez. Zeichen im String durch Cr-Lf ersetzen
function CharToCr (s : string; c : char) : string;
begin
  Result:=AnsiReplaceStr(s,c,#$0D);
  end;

{ --------------------------------------------------------------- }
(* erstes nichtleeres Zeichen suchen *)
function SeekNoSp (const s : string; Offset : cardinal) : integer;
var
  i  : integer;
begin
  if length(s)=0 then Result:=0
  else begin
    i:=Offset;
    while (i<=length(s)) and ((s[i]=' ') or (s[i]=Tab)) do inc(i);
    if i<=length(s) then Result:=i else Result:=0;
    end;
  end;

{ --------------------------------------------------------------- }
(* Position für nächstes Leerzeichen oder Tab suchen,
   bei Stringende = Länge+1 *)
function SeekSpace (const s : string; Offset : cardinal) : integer;
var
  i  : integer;
begin
  if length(s)=0 then Result:=0
  else begin
    i:=Offset;
    while (i<=length(s)) and (s[i]<>' ') and (s[i]<>Tab) do inc(i);
    Result:=i;
    end;
  end;

{ --------------------------------------------------------------- }
(* String mit Zeichen erzeugen *)
function FillStr (c   : char;
                  len : integer) : string;
var
  i : byte;
  s : string;
begin
  s:='';
  for i:=1 to len do s:=s+c;
  Result:=s;
  end;

{ --------------------------------------------------------------- }
// prüfen, ob leerer String
function IsEmptyStr (const s : string) : boolean;
begin
  Result:=length(s)=0;
  end;

function NotEmptyStr (const s : string) : boolean;
begin
  Result:=length(s)>0;
  end;

{ --------------------------------------------------------------- }
(* führende Zeichen ergänzen *)
function AddChar (const S : string;
                  c       : char;
                  len     : integer) : string;
var
  i  : integer;
begin
  Result:=s;
  for i:=succ(length(Result)) to len do Result:=c+Result;
  end;

{ --------------------------------------------------------------- }
(* Zeichen am Ende ergänzen *)
function ExtChar (const S : string;
                  c       : char;
                  len     : integer) : string;
var
  i  : integer;
begin
  Result:=s;
  for i:=succ(length(Result)) to len do Result:=Result+c;
  end;

{ --------------------------------------------------------------- }
// ein Zeichen durch ein anderes ersetzen
function ReplChars (const s         : AnsiString;
                    OldChar,NewChar : AnsiChar) : AnsiString;
var
  i  : integer;
begin
  Result:=s;
  for i:=1 to length(Result) do if Result[i]=OldChar then Result[i]:=NewChar;
  end;

function ReplChars (const s         : string;
                    OldChar,NewChar : char) : string;
var
  i  : integer;
begin
  Result:=s;
  for i:=1 to length(Result) do if Result[i]=OldChar then Result[i]:=NewChar;
  end;

{ --------------------------------------------------------------- }
// alle aktuellen Dezimaltrenner durch Punkt ersetzen
function ReplaceDecSepWithPeriod (const s : string) : string;
begin
  with FormatSettings do
    if DecimalSeparator<>Period then Result:=ReplChars(s,DecimalSeparator,Period)
    else Result:=s;
  end;

// alle Punkte durch aktuellen Dezimaltrenner ersetzen
function ReplacePeriodwithDecSep (const s : string) : string;
begin
  with FormatSettings do
    if DecimalSeparator<>Period then Result:=ReplChars(s,Period,DecimalSeparator)
    else Result:=s;
  end;

{ --------------------------------------------------------------- }
// führende Zeichen löschen
function RemChar (const S : string; c : char) : string;
var
  i : integer;
begin
  Result:=s; i:=1;
  while (i<=length(Result)) and (Result[i]=c) do inc(i);
  delete(Result,1,pred(i));
  end;

{ --------------------------------------------------------------- }
// Zeichen am Ende löschen
function CutChar (const S : string; c : char) : string;
var
  i : integer;
begin
  Result:=s; i:=length(Result);
  while (i>0) and (Result[i]=c) do dec(i);
  delete(Result,succ(i),length(Result));
  end;

{ --------------------------------------------------------------- }
// Zeichen im String löschen
function DelChar (const S : AnsiString; c : AnsiChar) : AnsiString;
var
  i  : integer;
begin
  Result:=s; i:=1;
  while (i<=length(Result)) do begin
    if Result[i]=c then delete(Result,i,1) else inc(i);
    end;
  end;

function DelChar (const S : string; c : char) : string;
var
  i  : integer;
begin
  Result:=s; i:=1;
  while (i<=length(Result)) do begin
    if Result[i]=c then delete(Result,i,1) else inc(i);
    end;
  end;

{ --------------------------------------------------------------- }
(* Leerzeichen gegen _ und / gegen % austauschen oder zurück *)
function ReplOptionChar (s : string; Back : boolean) : string;
var
  j : integer;
begin
  if Back then begin
    for j:=1 to length(s) do begin
      if s[j]='_' then s[j]:=' ';
      if s[j]='%' then s[j]:='/';
      end;
    end
  else begin
    for j:=1 to length(s) do begin
      if s[j]=' ' then s[j]:='_';
      if s[j]='/' then s[j]:='%';
      end;
    end;
  Result:=s;
  end;

{ --------------------------------------------------------------- }
// Zeichen prüfen
function IsLetter (c : char) : boolean;
begin
  Result:=CharInSet(c,UpperLetters) or CharInSet(c,LowerLetters);
  end;

function IsDigit (c : char) : boolean;
begin
  Result:=CharInSet(c,Digits);
  end;

function IsHexDigit (c : char) : boolean;
begin
  Result:=CharInSet(c,HexDigits);
  end;

function PosNonDigit (const s : string; n : integer) : integer;
begin
  while (n<=length(s)) and IsDigit(s[n]) do inc(n);
  if n>length(s) then Result:=0 else Result:=n;
  end;

{ --------------------------------------------------------------- }
// Fließkommazahl in Dreíergruppen
// OnlyFrac = true: nur rechts vom Komma
function InsertThousandSeparators (const NumStr : string; ThSep : char;
                                   OnlyFrac : boolean = false) : string;
var
  i,n,k : integer;
  neg    : boolean;
begin
  Result:=Trim(NumStr);
  if (length(Result)>0) and (ThSep<>#0) then begin
    neg:= Result[1]='-';
    if neg then delete(Result,1,1);
    n:=PosEx(FormatSettings.DecimalSeparator,Result);
    if (n=0) and OnlyFrac then Exit;
    k:=PosNonDigit(Result,n+1);
    if k=0 then begin
      if n=0 then n:=length(Result)+1;
      k:=length(Result)+1
      end
    else begin
      if n=0 then n:=k;
      end;
    i:=n;
    while i+3<k do begin // hinter dem Komma
      inc(i,4); Insert(ThSep,Result,i);
      end;
    if not OnlyFrac then while (n>4) do begin       // vor dem Komma
      dec(n,3); Insert(ThSep,Result,n);
      end;
    if neg then Result:='-'+Result;
    end;
  end;

{ --------------------------------------------------------------- }
// spez. Zeichen in Strings durch Hexzahl (Unicode) ersetzen und umgekehrt
function SpecCharToValue(const ss,sr : string) : string;
var
  i,j  : integer;
  s,sn : string;
begin
  Result:=ss;
  for j:=1 to length(sr) do begin
    s:=''; sn:='%'+IntToHex(word(sr[j]),4);
    for i:=1 to length(Result) do if Result[i]=sr[j] then s:=s+sn else s:=s+Result[i];
    Result:=s;
    end;
  end;

function ValueToSpecChar(const FName : string) : string;
var
  i,j : integer;
begin
  Result:='';
  i:=1;
  while i<=length(FName) do begin
    if FName[i]='%' then begin
      if (i+4<=length(FName)) and TryStrToInt('$'+copy(FName,i+1,4),j) then begin
        Result:=Result+Char(j);
        inc(i,4);
        end
      else Result:=Result+FName[i];
      end
    else Result:=Result+FName[i];
    inc(i);
    end;
  end;

{ --------------------------------------------------------------- }
(* String auf feste Länge bringen (kürzen oder mit Leerzeichen auffüllen *)
function FixLength (const S : string;
                    len : integer) : string;
begin
  if length(s)<len then FixLength:=ExtSp (S,len)
  else FixLength:=copy (s,1,len);
  end;

{ --------------------------------------------------------------- }
// Teilstring löschen als Funktion
function DeleteSubString(const s : string; Index,Count : integer)  : string;
begin
  Result:=s;
  Delete(Result,Index,Count);
  end;

{ --------------------------------------------------------------- }
// String am Anfang löschen bis ausschließlich "c", wenn nicht gefunden alles löschen
function DeleteUntilChar(const s : string; c : char) : string;
var
  n : integer;
begin
  Result:=s;
  n:=pos(c,Result);
  if n>0 then Delete(Result,1,n-1) else Result:='';
  end;

{ --------------------------------------------------------------- }
(* Anzahl von SubStr in S zählen *)
function CountSubStr (const ASubStr,AString : string) : integer;
var
  n : integer;
begin
  Result:=0; n:=1;
  repeat
    n:=PosEx(ASubStr,AString,n+1);
    if n>0 then inc(Result);
    until n=0;
  end;

{ ------------------------------------------------------------------- }
(* Integer, Double oder String aus einem String s bis zum nächsten Tenner lesen
   s wird um den verarbeiteten Teil gekürzt *)
function ReadNxtInt (var s   : String;
                     Del     : char;
                     Default : int64;
                     var err : boolean) : int64;
var
  n    : int64;
  i,ic : integer;
begin
  s:=TrimLeft(s); i:=pos(Del,s);
  if i=0 then i:=succ(length(s));
  val(copy(s,1,pred(i)),n,ic);
  if ic=0 then ReadNxtInt:=n
  else begin
    ReadNxtInt:=Default;
    err:=true;
    end;
  delete(s,1,i);
  end;

function ReadNxtInt (var s   : String;
                     Del     : char;
                     Default : int64) : int64;
var
  err : boolean;
begin
  Result:=ReadNxtInt(s,Del,Default,err);
  end;

function TryReadNxtInt (s : string; Del : char; var Value : integer) : boolean;
begin
  Result:=false;
  Value:=ReadNxtInt(s,Del,0,Result);
  Result:=not Result;
  end;

{ ------------------------------------------------------------------- }
function ReadNxtDbl (var s   : String;
                     Del     : char;
                     Default : double;
                     var err : boolean;
                     DecSep : char = #0) : double;
var
  i    : integer;
  x    : double;
  fs   : TFormatSettings;
begin
  fs:=FormatSettings;
  if DecSep<>#0 then fs.DecimalSeparator:=DecSep;
  s:=TrimLeft(s); i:=pos(Del,s);
  if i=0 then i:=succ(length(s));
  if TryStrToFloat(copy(s,1,pred(i)),x,fs) then Result:=x
  else begin
    Result:=Default;
    err:=true;
    end;
  delete(s,1,i);
  end;

function ReadNxtDbl (var s   : String;
                     Del     : char;
                     Default : double;
                     DecSep : char = #0) : double;
var
  err : boolean;
begin
  Result:=ReadNxtDbl(s,Del,Default,err,DecSep);
  end;

{ ------------------------------------------------------------------- }
function ReadNxtStr (var s   : AnsiString;
                     Del     : AnsiChar) : AnsiString;
var
  i : integer;
begin
  if length(s)>0 then begin
    i:=pos (Del,s);
    if i=0 then i:=succ(length(s));
    ReadNxtStr:=copy(s,1,pred(i));
    delete(s,1,i);
    end
  else ReadNxtStr:='';
  end;

function ReadNxtStr (var s   : AnsiString;
                     Del     : AnsiChar;
                     Default : AnsiString) : AnsiString; overload;
var
  t : AnsiString;
begin
  t:=ReadNxtStr(s,Del);
  if length(t)=0 then Result:=Default else Result:=t;
  end;

function ReadNxtStr (var s   : String;
                     Del     : char) : string;
var
  i : integer;
begin
  if length(s)>0 then begin
    i:=pos (Del,s);
    if i=0 then i:=succ(length(s));
    ReadNxtStr:=copy(s,1,pred(i));
    delete(s,1,i);
    end
  else ReadNxtStr:='';
  end;

function ReadNxtStr (var s   : String;
                     Del     : char;
                     Default : string) : string;
var
  t : string;
begin
  t:=ReadNxtStr(s,Del);
  if length(t)=0 then Result:=Default else Result:=t;
  end;

function GetNxtStr (const S   : String; Del : char) : string;
var
  i : integer;
begin
  if length(s)>0 then begin
    i:=pos (Del,s);
    if i=0 then i:=succ(length(s));
    Result:=copy(s,1,pred(i));
    end
  else Result:='';
  end;

function GetTrimStr(const S : string; Del : char) : string;
begin
  Result:=Trim(GetNxtStr(s,Del));
  end;

function GetNxtInt(const S : string; Del : char; Default : int64) : int64;
begin
  if not TryStrToInt64(GetTrimStr(S,Del),Result) then Result:=Default;
  end;

{ ------------------------------------------------------------------- }
(* Wort bis zum nächsten Delimiter lesen *)
function ReadNxtWord (var s : string;
                      Dels  : TSysCharSet) : string;
var
  i,n : integer;
begin
  n:=length(s);
  if n>0 then begin
    i:=1;
    while (i<=n) and not CharInSet(AnsiChar(s[i]),Dels) do inc(i);
    Result:=copy(s,1,pred(i));
    delete(s,1,pred(i));
    end
  else Result:='';
  end;

{ ------------------------------------------------------------------- }
(* ein Zeichen lesen oder num. Wert #nnn in Zeichen umwandeln *)
function ReadNxtChr (var s   : String;
                     Del     : char;
                     Default : char;
                     var err : boolean) : char;
var
  n,i,ic : integer;
begin
  if (length(s)>0) then begin
    i:=pos (Del,s);
    if (s[1]=ChrID) then begin  (* # *)
      if i=0 then i:=succ(length(s));
      val(copy(s,2,pred(i)),n,ic);
      if ic=0 then ReadNxtChr:=chr(n)
      else begin
        ReadNxtChr:=Default; err:=true;
        end;
      end
    else ReadNxtChr:=s[1];
    delete(s,1,i);
    end
  else begin
    ReadNxtChr:=Default; err:=true;
    end;
  end;

function ReadNxtChr (var s   : String;
                     Del     : char;
                     Default : char): char;
var
  err : boolean;
begin
  Result:=ReadNxtChr(s,Del,Default,err);
  end;

{ ------------------------------------------------------------------- }
function ReadNxtQuotedStr (var s : string; ADelim,AQuote : char) : string;
var
  n : integer;
  qs,fin : boolean;
begin
  s:=TrimLeft(s);
  if length(s)>0 then begin
    if s[1]=AQuote then begin
      n:=2; qs:=true; fin:=false;
      repeat
        if s[n]=AQuote then qs:=not qs
        else fin:=not qs and (s[n]=ADelim);
        if not fin then inc(n);
        until (n>length(s)) or fin;
      if n<=3 then result:=''
      else Result:=AnsiDequotedStr(TrimRight(copy(s,1,pred(n))),AQuote);
      end
    else begin
      n:=pos(ADelim,s);
      if n=0 then n:=succ(length(s));
      Result:=copy(s,1,pred(n));
      end;
    delete(s,1,n);
    end
  else Result:='';
  end;

function MakeQuotedStr (const s : string; CheckChars : array of char) : string;
var
  i : integer;
  ok : boolean;
begin
  if (length(s)=0) then Result:=''
  else begin
    if length(CheckChars)=0 then ok:=true
    else begin
      ok:=false;
      for i:=Low(CheckChars) to High(CheckChars) do ok:=ok or (Pos(CheckChars[i],s)>0);
      end;
    if ok then Result:=AnsiQuotedStr(s,Quote) else Result:=s;
    end;
  end;

{ ------------------------------------------------------------------- }
// extract enclosed string
function ExtractEnclosedString(const AString : string; EncChar : Char; var Pos : integer) : string;
var
  n,k : integer;
begin
  Result:='';
  n:=PosEx(EncChar,AString,Pos);
  if n>0 then begin
    k:=PosEx(EncChar,AString,n+1);
    if k>0 then begin
      Pos:=k+1;
      Result:=copy(AString,n,Pos-n);
      end;
    end
  end;

{ ------------------------------------------------------------------- }
// replace environment variable
function ReplaceEnvString(const AString,AEnv : string) : string;
var
  s : string;
begin
  s:=GetEnvironmentVariable(AnsiDequotedStr(AEnv,'%'));
  if length(s)>0 then Result:=AnsiReplaceText(AString,AEnv,s)
  else Result:=AString;
  end;

function ReplaceEnvVariables (const ALine : string) : string;
var
  s : string;
  n : integer;
begin
  Result:=ALine; n:=1;
  s:=ExtractEnclosedString(ALine,'%',n);
  while length(s)>0 do begin
    Result:=ReplaceEnvString(Result,s);
    s:=ExtractEnclosedString(ALine,'%',n);
    end;
  end;

{ ------------------------------------------------------------------- }
function MergeStringList (const ACommaText : string) : string;
var
  sl : TStringList;
begin
  sl:=TStringList.Create;
  with sl do begin
    Sorted:=true; Duplicates:=dupIgnore;
    CommaText:=ACommaText;
    Result:=CommaText;
    Free;
    end;
  end;

function DelimitedTextToLines (DelimitedText : string; Delimiter,QuoteChar : char; var ACount : integer) : string;
begin
  ACount:=0;
  Result:=ReadNxtQuotedStr(DelimitedText,Delimiter,QuoteChar);
  if length(Result)>0 then inc(ACount);
  while length(DelimitedText)>0 do begin
    Result:=Result+sLineBreak+ReadNxtQuotedStr(DelimitedText,Delimiter,QuoteChar);
    inc(ACount);
    end;
  end;

function CommaTextToLines (Commatext : string; var ACount : integer) : string;
begin
  Result:=DelimitedTextToLines(Commatext,Comma,Quote,ACount);
  end;

function CommaTextToLines (Commatext : string) : string;
var
  n : integer;
begin
  Result:=CommaTextToLines(Commatext,n);
  end;

// Count number of lines in Text separated by sLineBreak
function CountLines (const Text : string) : integer;
var
  n : integer;
begin
  n:=0; Result:=1;
  repeat
    n:=PosEx(sLineBreak,Text,n+1);
    if n>0 then inc(Result);
    until (n=0) or (n>=length(Text));
  end;

function DuplicateQuotes(const S: string; Quote : Char): string;
var
  I : Integer;
begin
  Result:=S;
  for I := Length(Result) downto 1 do if Result[I]=Quote then Insert(Quote,Result,I);
  end;

{ ------------------------------------------------------------------- }
// Position of char in string starting at "Offset", ignore parts between quotes
function PosChar (const s: string; AChar,AQuote : char; Offset: Cardinal): Integer;
var
  i,ls : integer;
  qs   : boolean;
begin
  i:=Offset;
  ls:=length(s);
  qs:=true;
  while i<=ls do begin
    if s[i]=AQuote then qs:=not qs
    else if qs and (s[i]=AChar) then begin
      Result:=i;
      exit;
      end;
    inc(i);
    end;
  Result:=0;
  end;

{ ------------------------------------------------------------------- }
function GetPluralString (const sNo,sOne,sMany : string; n : integer) : string;
begin
  if n=1 then Result:=sOne
  else if (n=0) and (length(sNo)>0) then Result:=sNo
  else Result:=TryFormat(sMany,[n]);
  end;

function GetPluralString (const sNo,sOne,sMany : string; n : integer; const s : string) : string;
begin
  if n=1 then Result:=TryFormat(sOne,[s])
  else if (n=0) and (length(sNo)>0) then Result:=sNo
  else Result:=TryFormat(sMany,[n,s]);
  end;

function GetPluralString (const sOne,sMany : string; n : integer) : string;
begin
  if n=1 then Result:='1 '+sOne else Result:=IntToStr(n)+Space+sMany;
  end;

{ --------------------------------------------------------------- }
// Check if AText matches filters: AFilter="<filter1><sep><filter2><sep>.."
function MatchesFilter(const AText,AFilter : string; Sep : char) : boolean;
var
  s : string;
begin
  s:=AFilter;
  repeat
    try
      Result:=MatchesMask(AText,ReadNxtStr(s,Sep));
    except
      Result:=false;
      end;
    until Result or (length(s)=0);
  end;

{ ---------------------------------------------------------------- }
// Routinen zur Auswertung einer Befehlszeile
// prüfe, ob die ersten Zeichen einer Option mit dem Parameter übereinstimmen
function CompareOption (const Param,Option : string) : boolean;
begin
  Result:=AnsiLowercase(Param)=copy(Option,1,length(Param));
  end;

// Option vom Typ option:value einlesen
function ReadOptionValue (var Param : string; const Option : string) : boolean;
var
  i : integer;
begin
  Result:=false;
  i:=AnsiPos(':',Param);
  if i=0 then exit;
  if not CompareOption(copy(Param,1,i-1),Option) then exit;
  Delete(Param,1,i); Result:=true;
  end;

// Steuerdatei mit Startoptionen einlesen
function ReadOptionFile (FName : string) : string;
var
  f : TextFile;
  s : string;
begin
  Result:='';
  if FileExists(FName) then begin
    AssignFile(f,FName); Reset(f);
    while not Eof(f) do begin
      readln(f,s);
      s:=Trim(ReadNxtStr(s,';'));  // bis Kommentar ";" lesen
      if length(s)>0 then Result:=Result+'|'+s
      end;
    CloseFile(f);
    if length(Result)>0 then Delete(Result,1,1);
    end
  end;

//-----------------------------------------------------------------------------
{ QUICKSORT sorts elements in the array "SList" of any record type of
  size "RSize" (in bytes) with indices between 0 and "Count"-1 (both inclusive).
  Note that the QuickSort procedure provides only an "interface" to the program.
  The actual processing takes place in the "Sort" procedure, which executes
  itself recursively.
  "Compare" is a callback function of type "TCompFunction" (see begin of unit).
  Algorithm based on Borland pascal sample "qsort.pas",
  modified J. Rathlev, Nov. 2001 (rathlev@physik.uni-kiel.de) }
//-----------------------------------------------------------------------------
type
  TByteDynArray = array of byte;

procedure QuickSort (var SList; Count,RSize : Integer; Compare : TCompFunction);
var
  {the  following variables are global to "Sort" to save memory in recursive calls }
  tmp,ns  : TByteDynArray;
  pl      : PByteArray;
  n,m     : integer;

  procedure Sort(nl,nr: Integer);
  var
    i,j   : integer;
  begin
    n:=RSize*((nl+nr) div 2);
    move (pl^[n],ns[0],RSize);
    i:=nl; j:=nr;
    repeat
      while Compare(pl^[RSize*i],ns[0]) do inc(i);
      while Compare(ns[0],pl^[RSize*j]) do dec(j);
      if i<=j then begin
        n:=RSize*i; m:=RSize*j;
        move (pl^[n],tmp[0],RSize);
        move (pl^[m],pl^[n],RSize);
        move (tmp[0],pl^[m],RSize);
        inc(i); dec(j);
        end;
      until i>j;
    if nl<j then Sort(nl,j);
    if i<nr then Sort(i,nr);
    end;

begin {QuickSort};
  SetLength(Tmp,RSize); SetLength(ns,RSize);
  pl:=@SList;
  Sort(0,Count-1);
  Tmp:=nil; ns:=nil;
  end;

end.
