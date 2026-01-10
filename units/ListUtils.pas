(* Delphi-Unit
   collection of routines to process lists
   =======================================

   separated from former FileUtils

   © Dr. J. Rathlev, D-24222 Schwentinental (kontakt(a)rathlev-home.de)

   The contents of this file may be used under the terms of the
   Mozilla Public License ("MPL") or
   GNU Lesser General Public License Version 2 or later (the "LGPL")

   Software distributed under this License is distributed on an "AS IS" basis,
   WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
   the specific language governing rights and limitations under the License.

   October 2024
   last modified: March 2025
   *)

unit ListUtils;

interface

uses System.SysUtils, System.Classes, System.IniFiles, Vcl.StdCtrls, Vcl.ComCtrls;

type
  TItemInfo = class (TObject)
    InfoString     : string;
    Tag            : integer;
    constructor Create (const AInfo : string; ATag : integer = 0);
    end;

const
  defMaxHist : integer = 50;

{ ---------------------------------------------------------------- }
// History list management
procedure LoadHistory (IniFile : TCustomIniFile; const Section,Ident : string;
                       History : TStrings; MaxCount : integer; CvQuote : boolean = false); overload;
procedure LoadHistory (IniFile : TCustomIniFile; const Section,Ident : string;
                       History : TStrings; CvQuote : boolean = false); overload;
procedure LoadHistory (IniFile : TCustomIniFile; const Section : string;
                       History : TStrings; CvQuote : boolean = false); overload;
procedure LoadHistory (IniFile : TCustomIniFile; const Section : string;
                       Combo : TComboBox; MaxHist : integer = -1; CvQuote : boolean = false); overload;
procedure LoadHistory (IniFile : TCustomIniFile; const Section,Ident : string;
                       Combo : TComboBox; MaxHist : integer = -1; CvQuote : boolean = false); overload;
procedure LoadHistoryList (IniFile : TCustomIniFile; const Section : string;
                       Combo : TComboBox; MaxHist : integer = -1; CvQuote : boolean = false); overload;
procedure LoadHistoryList (IniFile : TCustomIniFile; const Section,Ident : string;
                       Combo : TComboBox; MaxHist : integer = -1; CvQuote : boolean = false); overload;
procedure LoadHistory (const IniName,Section,Ident : string;
                       History : TStrings; MaxCount : integer; CvQuote : boolean = false); overload;
procedure LoadHistory (const IniName,Section,Ident : string;
                       History : TStrings; CvQuote : boolean = false); overload;
procedure LoadHistory (const IniName,Section : string;
                       Combo : TComboBox; MaxHist : integer = -1; CvQuote : boolean = false); overload;

procedure SaveHistory (IniFile : TCustomIniFile; const Section,Ident : string;
                       Erase : boolean; History : TStrings; MaxCount : integer; CvQuote : boolean = false); overload;
procedure SaveHistory (IniFile : TCustomIniFile; const Section,Ident : string;
                       Erase : boolean; History : TStrings; CvQuote : boolean = false); overload;
procedure SaveHistory (IniFile : TCustomIniFile; const Section : string;
                       Erase : boolean; History : TStrings; CvQuote : boolean = false); overload;
procedure SaveHistory (IniFile : TCustomIniFile; const Section : string;
                       History : TStrings; CvQuote : boolean = false); overload;
procedure SaveHistory (IniFile : TCustomIniFile; const Section : string; Erase : boolean;
                       Combo : TComboBox; MaxHist : integer = -1; CvQuote : boolean = false); overload;
procedure SaveHistory (IniFile : TCustomIniFile; const Section : string;
                       Combo : TComboBox; MaxHist : integer = -1; CvQuote : boolean = false); overload;
procedure SaveHistory (IniFile : TCustomIniFile; const Section,Ident : string; Erase : boolean;
                       Combo : TComboBox; MaxHist : integer = -1; CvQuote : boolean = false); overload;
procedure SaveHistory (const IniName,Section,Ident : string;
                       Erase : boolean; History : TStrings; MaxCount : integer; CvQuote : boolean = false); overload;
procedure SaveHistory (const IniName,Section,Ident : string;
                       Erase : boolean; History : TStrings; CvQuote : boolean = false); overload;
procedure SaveHistory (const IniName,Section : string; Erase : boolean;
                       Combo : TComboBox; MaxHist : integer = -1; CvQuote : boolean = false); overload;

procedure AddToHistory (History : TStrings; const hs : string; MaxCount : integer); overload;
procedure AddToHistory (History : TStrings; const hs : string); overload;
procedure AddToHistory (Combo : TComboBox; const hs : string); overload;
procedure AddToHistory (Combo : TComboBox); overload;
procedure UpdateHistory (Combo : TComboBox);
procedure AddToHistoryList (Combo : TComboBox; const hs : string);
procedure RemoveFromHistory (History : TStrings; const hs : string);

{ ---------------------------------------------------------------- }
// Combo box list management
procedure LoadInfoList (IniFile : TCustomIniFile; const Section,ItemId,InfoId : string;
                        ACombo : TComboBox; MaxItems : integer = -1);
procedure SaveInfoList (IniFile : TCustomIniFile; const Section,ItemId,InfoId : string;
                        Erase : boolean; ACombo : TComboBox; MaxItems : integer = -1);
procedure AddToInfoList (ACombo : TComboBox; const s : string; MaxItems : integer = -1);
procedure ChangeInfoInList (ACombo : TComboBox; const s : string; ATag : integer); overload;
procedure ChangeInfoInList (ACombo : TComboBox; const s : string); overload;
function GetInfoFromList (ACombo : TComboBox) : string;
function GetTagFromList (ACombo : TComboBox) : integer;

{ ---------------------------------------------------------------- }
// Entferne alle Objekte einer String-Liste oder einer ListView-Liste aus dem Speicher
procedure FreeListObjects (Liste : TStrings); overload;
procedure FreeListViewData (Liste : TListItems);

{ ---------------------------------------------------------------- }
// Ausgewählten Eintrag in einer ListBox
function GetSelectedItem (ListBox : TListBox) : string;

{ ---------------------------------------------------------------- }
// Replacement for TStrings functions: IndexOfName, Name and Value
function GetIndexOfName (AList : TStrings; const AName : string) : integer;
function GetName (AList : TStrings; AIndex : integer) : string;
function GetValue (AList : TStrings; AIndex : integer) : string;
procedure TrimEndOfList (AList : TStrings);

{ ---------------------------------------------------------------- }
// Listview-Index aus Caption ermitteln (wie IndexOf bei TListBox)
function GetListViewIndex (lv : TListView; const ACaption : string): integer; overload;
function GetListViewIndex (lvi : TListItems; const ACaption : string): integer; overload;

// Subitem-Index aus der Mausposition ermitteln (nur vsReport)
function GetColumnIndexAt (ListView : TListView; Pos : integer) : integer;

// TopItem auf Index setzen (nur vsReport)
procedure SetListViewTopItem (lv : TListView; AIndex : integer; Select : boolean);

implementation

uses StringUtils;

{ ------------------------------------------------------------------- }
// History list management
const
  iniHist = 'History';

procedure LoadHistory (IniFile : TCustomIniFile; const Section,Ident : string;
                       History : TStrings; MaxCount : integer; CvQuote : boolean);
var
  i : integer;
  s,si : string;
begin
  with IniFile do begin
    if SectionExists(Section) then begin
      if length(Ident)=0 then si:=iniHist else si:=Ident;
      History.Clear;
      for i:=0 to MaxCount-1 do begin
        s:=ReadString(Section,si+IntToStr(i),'');
        if length(s)>0 then begin
          if CvQuote then s:=ReplChars(s,'#',Quote);
          if History.IndexOf(s)<0 then History.Add(s);  // no duplicates
          end;
        end;
      end;
    end;
  end;

procedure LoadHistory (IniFile : TCustomIniFile; const Section,Ident : string;
                       History : TStrings; CvQuote : boolean);
begin
  LoadHistory(IniFile,Section,Ident,History,defMaxHist,CvQuote);
  end;

procedure LoadHistory (IniFile : TCustomIniFile; const Section : string;
                       History : TStrings; CvQuote : boolean);
begin
  LoadHistory(IniFile,Section,'',History,defMaxHist,CvQuote);
  end;

procedure LoadHistory (const IniName,Section,Ident : string;
                       History : TStrings; MaxCount : integer; CvQuote : boolean);
var
  IniFile : TMemIniFile;
begin
  IniFile:=TMemIniFile.Create(IniName);
  LoadHistory(IniFile,Section,Ident,History,MaxCount,CvQuote);
  IniFile.Free;
  end;

procedure LoadHistory (const IniName,Section,Ident : string;
                       History : TStrings; CvQuote : boolean);
begin
  LoadHistory(IniName,Section,Ident,History,defMaxHist,CvQuote);
  end;

procedure LoadHistoryList (IniFile : TCustomIniFile; const Section : string;
                       Combo : TComboBox; MaxHist : integer = -1; CvQuote : boolean = false);
var
  n : integer;
begin
  with Combo do begin
    if MaxHist<0 then n:=DropDownCount else n:=MaxHist;
    LoadHistory(IniFile,Section,'',Items,n,CvQuote);
    if Items.Count>0 then ItemIndex:=0;
    end;
  end;

procedure LoadHistoryList (IniFile : TCustomIniFile; const Section,Ident : string;
                       Combo : TComboBox; MaxHist : integer = -1; CvQuote : boolean = false);
var
  n : integer;
begin
  with Combo do begin
    if MaxHist<0 then n:=DropDownCount else n:=MaxHist;
    LoadHistory(IniFile,Section,Ident,Items,n,CvQuote);
    if Items.Count>0 then ItemIndex:=0;
    end;
  end;

procedure LoadHistory (IniFile : TCustomIniFile; const Section,Ident : string;
                       Combo : TComboBox; MaxHist : integer = -1; CvQuote : boolean = false);
begin
  with Combo do begin
    LoadHistoryList(IniFile,Section,Ident,Combo,MaxHist,CvQuote);
    if (Items.Count<=1) then Style:=csSimple else Style:=csDropDown;
    end;
  end;

procedure LoadHistory (IniFile : TCustomIniFile; const Section : string;
                       Combo : TComboBox; MaxHist : integer = -1; CvQuote : boolean = false);
begin
  with Combo do begin
    LoadHistoryList(IniFile,Section,Combo,MaxHist,CvQuote);
    if (Items.Count<=1) then Style:=csSimple else Style:=csDropDown;
    end;
  end;

procedure LoadHistory (const IniName,Section : string;
                       Combo : TComboBox; MaxHist : integer; CvQuote : boolean);
var
  n : integer;
begin
  with Combo do begin
    if MaxHist<0 then n:=DropDownCount else n:=MaxHist;
    LoadHistory(IniName,Section,'',Items,n,CvQuote);
    if Items.Count>0 then ItemIndex:=0;
    if (Items.Count<=1) then Style:=csSimple else Style:=csDropDown;
    end;
  end;

procedure SaveHistory (IniFile : TCustomIniFile; const Section,Ident : string;
                       Erase : boolean; History : TStrings; MaxCount : integer; CvQuote : boolean);
var
  i,n : integer;
  s,si : string;
begin
  with IniFile do begin
    if length(Ident)=0 then si:=iniHist else si:=Ident;
    if Erase then EraseSection (Section);
    with History do begin
      if Count>MaxCount then n:=MaxCount else n:=Count;
      for i:=0 to n-1 do begin
        s:=Strings[i];
        if CvQuote then s:=ReplChars(s,Quote,'#');
        WriteString(Section,si+IntToStr(i),s);
        end;
      end;
    end;
  end;

procedure SaveHistory (IniFile : TCustomIniFile; const Section,Ident : string;
                       Erase : boolean; History : TStrings; CvQuote : boolean);
begin
  SaveHistory(IniFile,Section,Ident,Erase,History,defMaxHist,CvQuote);
  end;

procedure SaveHistory (IniFile : TCustomIniFile; const Section : string;
                       Erase : boolean; History : TStrings; CvQuote : boolean);
begin
  SaveHistory(IniFile,Section,'',Erase,History,defMaxHist,CvQuote);
  end;

procedure SaveHistory (IniFile : TCustomIniFile; const Section : string;
                       History : TStrings; CvQuote : boolean = false);
begin
  SaveHistory(IniFile,Section,'',true,History,defMaxHist,CvQuote);
  end;

procedure SaveHistory (const IniName,Section,Ident : string;
                       Erase : boolean; History : TStrings; MaxCount : integer; CvQuote : boolean);
var
  IniFile : TMemIniFile;
begin
  IniFile:=TMemIniFile.Create(IniName);
  SaveHistory(IniFile,Section,Ident,Erase,History,defMaxHist,CvQuote);
  try
    IniFile.UpdateFile;
  finally
    IniFile.Free;
    end;
  end;

procedure SaveHistory (const IniName,Section,Ident : string;
                       Erase : boolean; History : TStrings; CvQuote : boolean);
begin
  SaveHistory(IniName,Section,Ident,Erase,History,defMaxHist,CvQuote);
  end;

procedure SaveHistory (IniFile : TCustomIniFile; const Section : string; Erase : boolean;
                       Combo : TComboBox; MaxHist : integer = -1; CvQuote : boolean = false);
var
  n : integer;
begin
  with Combo do begin
    if MaxHist<0 then n:=DropDownCount else n:=MaxHist;
    SaveHistory(IniFile,Section,'',Erase,Items,n,CvQuote);
    end;
  end;

procedure SaveHistory (IniFile : TCustomIniFile; const Section,Ident : string; Erase : boolean;
                       Combo : TComboBox; MaxHist : integer = -1; CvQuote : boolean = false);
var
  n : integer;
begin
  with Combo do begin
    if MaxHist<0 then n:=DropDownCount else n:=MaxHist;
    SaveHistory(IniFile,Section,Ident,Erase,Items,n,CvQuote);
    end;
  end;

procedure SaveHistory (IniFile : TCustomIniFile; const Section : string;
                       Combo : TComboBox; MaxHist : integer = -1; CvQuote : boolean = false); overload;
begin
  SaveHistory (IniFile,Section,true,Combo,MaxHist,CvQuote);
  end;

procedure SaveHistory (const IniName,Section : string; Erase : boolean;
                       Combo : TComboBox; MaxHist : integer; CvQuote : boolean);
var
  n : integer;
begin
  with Combo do begin
    if MaxHist<0 then n:=DropDownCount else n:=MaxHist;
    SaveHistory(IniName,Section,'',Erase,Items,n,CvQuote);
    end;
  end;

// move or add item "hs" to begin of history list
procedure AddToHistory (History : TStrings; const hs : string; MaxCount : integer);
var
  n : integer;
begin
if length(hs)>0 then with History do begin
    n:=IndexOf(hs);
    if n<0 then begin
      if Count>=MaxCount then Delete (Count-1);
      Insert (0,hs);
      end
    else begin
      if n>0 then Move (n,0);
      Strings[0]:=hs;  // update string anyway, e.g. if case was changed
      end;
    end;
  end;

procedure AddToHistory (History : TStrings; const hs : string);
begin
  AddToHistory (History,hs,defMaxHist);
  end;

procedure AddToHistory (Combo : TComboBox; const hs : string);
begin
  with Combo do begin
    AddToHistory (Items,hs,DropDownCount);
    if Items.Count>0 then ItemIndex:=0;
    if Style<>csDropDownList then begin
      if (Items.Count<=1) then Style:=csSimple else Style:=csDropDown;
      end;
    end;
  end;

procedure AddToHistory (Combo : TComboBox);
begin
  AddToHistory(Combo,Combo.Text);
  end;

procedure AddToHistoryList (Combo : TComboBox; const hs : string);
begin
  with Combo do begin
    AddToHistory (Items,hs,DropDownCount);
    if Items.Count>0 then ItemIndex:=0;
    end;
  end;

procedure UpdateHistory (Combo : TComboBox);
begin
  with Combo do AddToHistory(Combo,Items[ItemIndex]);
  end;

procedure RemoveFromHistory (History : TStrings; const hs : string);
var
  n : integer;
begin
  if length(hs)>0 then with History do begin
    n:=IndexOf(hs);
    if n>=0 then Delete(n);
    end;
  end;

{ ------------------------------------------------------------------- }
constructor TItemInfo.Create (const AInfo : string; ATag : integer);
begin
  inherited Create;
  InfoString:=AInfo; Tag:=Atag;
  end;

{ ------------------------------------------------------------------- }
// Combo box list management
procedure LoadInfoList (IniFile : TCustomIniFile; const Section,ItemId,InfoId : string;
                        ACombo : TComboBox; MaxItems : integer);
var
  i,n    : integer;
  s,t,ti : string;
begin
  with IniFile do begin
    if SectionExists(Section) then with ACombo do begin
      Clear;
      if MaxItems<0 then MaxItems:=DropDownCount;
      for i:=0 to MaxItems-1 do begin
        s:=ReadString(Section,Format(ItemId+'%.2u',[i]),'');
        t:=ReadString(Section,Format(InfoId+'%.2u',[i]),'');
        if length(s)>0 then begin
          ti:=ReadNxtStr(t,',');
          if t.IsEmpty or not TryStrToInt(t,n) then n:=0;
          Items.AddObject(s,TItemInfo.Create(ti,n));
          end;
        end;
      end;
    end;
  end;

procedure SaveInfoList (IniFile : TCustomIniFile; const Section,ItemId,InfoId : string;
                        Erase : boolean; ACombo : TComboBox; MaxItems : integer);
var
  i,n  : integer;
  di : TItemInfo;
begin
  with IniFile do begin
    if Erase then EraseSection (Section);
    with ACombo do begin
      if MaxItems<0 then n:=DropDownCount else n:=MaxItems;
      with Items do if Count<n then n:=Count;
      for i:=0 to n-1 do begin
        WriteString(Section,Format(ItemId+'%.2u',[i]),Items[i]);
        di:=Items.Objects[i] as TItemInfo;
        if assigned(di) then if not di.InfoString.IsEmpty then
           WriteString(Section,Format(InfoId+'%.2u',[i]),di.InfoString+','+IntToStr(di.Tag));
        end;
      end;
    end;
  end;

// move or add item "s" to begin of list
procedure AddToInfoList (ACombo : TComboBox; const s : string; MaxItems : integer);
var
  n : integer;
begin
  if length(s)>0 then with ACombo do begin
    n:=Items.IndexOf(s);
    if n<0 then begin
      if MaxItems<0 then MaxItems:=DropDownCount;
      with Items do begin
        if Count>=MaxItems then Delete (Count-1);
        InsertObject (0,s,TItemInfo.Create(''));
        end;
      end
    else Items.Move (n,0);
    ItemIndex:=0;
    end;
  end;

procedure ChangeInfoInList (ACombo : TComboBox; const s : string; ATag : integer);
begin
  with ACombo do if ItemIndex>=0 then begin
    if assigned(Items.Objects[ItemIndex]) then with (Items.Objects[ItemIndex] as TItemInfo) do begin
      InfoString:=s; Tag:=ATag;
      end;
    end;
  end;

procedure ChangeInfoInList (ACombo : TComboBox; const s : string);
begin
  with ACombo do if ItemIndex>=0 then begin
    if assigned(Items.Objects[ItemIndex]) then (Items.Objects[ItemIndex] as TItemInfo).InfoString:=s;
    end;
  end;

function GetInfoFromList (ACombo : TComboBox) : string;
begin
  with ACombo do if (Items.Count>0) and (ItemIndex>=0) and assigned(Items.Objects[ItemIndex])then
    Result:=(Items.Objects[ItemIndex] as TItemInfo).InfoString
  else Result:='';
  end;

function GetTagFromList (ACombo : TComboBox) : integer;
begin
  with ACombo do if (Items.Count>0) and (ItemIndex>=0) and assigned(Items.Objects[ItemIndex]) then
    Result:=(Items.Objects[ItemIndex] as TItemInfo).Tag
  else Result:=0;
  end;

//-----------------------------------------------------------------------------
procedure FreeListObjects (Liste : TStrings);
var
  i : integer;
begin
  with Liste do begin
    for i:=0 to Count-1 do if assigned(Objects[i]) then begin
      try Objects[i].Free; except end;
      Objects[i]:=nil;
      end;
    end;
  end;

procedure FreeListViewData (Liste : TListItems);
var
  i : integer;
begin
  with Liste do for i:=0 to Count-1 do with Item[i] do if Data<>nil then begin
    TObject(Data).Free; Data:=nil;
    end;
  end;

{ ---------------------------------------------------------------- }
// Ausgewählten Eintrag in einer ListBox
function GetSelectedItem (ListBox : TListBox) : string;
begin
  with ListBox do if ItemIndex>=0 then Result:=Items[ItemIndex]
  else Result:='';
  end;

{ ---------------------------------------------------------------- }
// Replacement for TStrings functions: IndexOfName, Name and Value
// NameValueSeparator can have spaces at left and right (e.g. Name - Value)
function GetIndexOfName (AList : TStrings; const AName : string) : integer;
var
  n: integer;
  s: string;
begin
  for Result:=0 to AList.Count-1 do begin
    s:=AList[Result];
    n:=AnsiPos(AList.NameValueSeparator,s);
    if (n<>0) and AnsiSameText(Trim(Copy(s,1,n-1)),AName) then Exit;
    end;
  Result:=-1;
  end;

function GetName (AList : TStrings; AIndex : integer) : string;
var
  n: integer;
  s: string;
begin
  if (AIndex>=0) and (AIndex<AList.Count) then begin
    s:=AList[AIndex];
    n:=AnsiPos(AList.NameValueSeparator,s);
    if n>0 then Result:=Trim(Copy(s,1,n-1))
    else Result:='';
    end
  else Result:='';
  end;

function GetValue (AList : TStrings; AIndex : integer) : string;
var
  n: integer;
  s: string;
begin
  if (AIndex>=0) and (AIndex<AList.Count) then begin
    s:=AList[AIndex];
    n:=AnsiPos(AList.NameValueSeparator,s);
    if n>0 then Result:=Trim(Copy(s,n+1,length(s)))
    else Result:='';
    end
  else Result:='';
  end;

procedure TrimEndOfList (AList : TStrings);
var
  i : integer;
begin
  with AList do for i:=Count-1 downto 0 do begin
    if AList[i].IsEmpty then Delete(i) else Break;
    end;
  end;

//-----------------------------------------------------------------------------
// Listview-Index aus Caption ermitteln (wie IndexOf bei TListBox)
function GetListViewIndex (lv : TListView; const ACaption : string): integer;
begin
  with lv.Items do for Result:=0 to Count-1 do
    if AnsiSameText(Item[Result].Caption,ACaption) then Exit;
  Result:=-1;
  end;

function GetListViewIndex (lvi : TListItems; const ACaption : string): integer;
begin
  with lvi do for Result:=0 to Count-1 do
    if AnsiSameText(Item[Result].Caption,ACaption) then Exit;
  Result:=-1;
  end;

// Subitem-Index aus der Mausposition ermitteln (nur vsReport)
function GetColumnIndexAt (ListView : TListView; Pos : integer) : integer;
var
  x : integer;
begin
  with ListView.Columns do begin
    x:=0;
    for Result:=0 to Count-1 do with Items[Result] do begin
      if (Pos>=x) and (Pos<x+Width) then Exit;
      x:=x+Width;
      end;
    end;
  Result:=-1;
  end;

// TopItem auf Index setzen (nur vsReport)
procedure SetListViewTopItem (lv : TListView; AIndex : integer; Select : boolean);
var
  n : integer;
begin
  with lv do if (AIndex>=0) and (Items.Count>0) and (AIndex<Items.Count) then begin
    with TopItem.DisplayRect(drBounds)do n:=Top-Bottom;
    Scroll(0,n*(TopItem.Index-AIndex));
    if Select then ItemIndex:=AIndex;
    end;
  end;

end.
