(* Delphi Dialog
   Auswahl und Bearbeiten von Listeneinträgen
   ==========================================
   
   © Dr. J. Rathlev, D-24222 Schwentinental (kontakt(a)rathlev-home.de)

   The contents of this file may be used under the terms of the
   Mozilla Public License ("MPL") or
   GNU Lesser General Public License Version 2 or later (the "LGPL")

   Software distributed under this License is distributed on an "AS IS" basis,
   WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
   the specific language governing rights and limitations under the License.
    
   Vers. 1 - Apr. 2005
   last modified: Nov. 2020
    *)
    
unit SelectFromListDlg;

interface

uses Winapi.Windows, System.SysUtils, System.Classes, Vcl.Graphics, Vcl.Forms,
  Vcl.Controls, Vcl.StdCtrls, Vcl.Buttons, Vcl.ExtCtrls, StringUtils;

type
  TCheckEntry = function (const AText : string) : boolean of object;

  TSelectOption = (soPrompt,soEdit,soOrder,soMulti);
  TSelectOptions = set of TSelectOption;

  TSelectFromListDialog = class(TForm)
    OKBtn: TBitBtn;
    CancelBtn: TBitBtn;
    lbxStringList: TListBox;
    gbxEdit: TGroupBox;
    btnInsert: TBitBtn;
    btnDelete: TBitBtn;
    btnEdit: TBitBtn;
    gbxMove: TGroupBox;
    lbDesc: TLabel;
    btnDefault: TBitBtn;
    Panel1: TPanel;
    Panel2: TPanel;
    lbHint: TLabel;
    btnPrompt: TBitBtn;
    UpBtn: TBitBtn;
    DownBtn: TBitBtn;
    procedure btnInsertClick(Sender: TObject);
    procedure btnDeleteClick(Sender: TObject);
    procedure btnEditClick(Sender: TObject);
    procedure lbxStringListDblClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure UpBtnClick(Sender: TObject);
    procedure DownBtnClick(Sender: TObject);
    procedure btnDefaultClick(Sender: TObject);
    procedure btnPromptClick(Sender: TObject);
  private
    { Private declarations }
    FEdit : boolean;
    dist  : integer;
    FText,
    DefDelimitedText : string;
    FCheckEntry : TCheckEntry;
    function DialogPos(Sender: TObject) : TPoint;
  public
    { Public declarations }
{$IFDEF HDPI}   // scale glyphs and images for High DPI
    procedure AfterConstruction; override;
{$EndIf}
    function Execute (APos : TPoint; Titel,Desc,Hint : string;
                      Options : TSelectOptions; ACols : integer;
                      Convert : TTextChange; const Default : string;
                      SList : TStrings; var AText : string;
                      ShowCancel : boolean = true; CheckEntry : TCheckEntry = nil) : TModalResult;  overload;
    function Select (APos : TPoint; Titel,Desc,Hint : string;
                     Options : TSelectOptions; ACols : integer;
                     Convert : TTextChange; const Default : string;
                     SList : TStrings; var AText : string;
                     ShowCancel : boolean = true; CheckEntry : TCheckEntry = nil) : boolean;  overload;
    function Select (APos : TPoint; Titel,Desc,Hint : string;
                     ACols : integer;
                     Convert : TTextChange; const Default : string;
                     var ListText : string;
                     ADel : Char = ','; AQuote : Char ='"') : boolean; overload;
    function Select (APos : TPoint; Titel,Desc,Hint : string; Prompt : boolean;
                     SList : TStrings; var AText  : string) : TModalResult; overload;
    procedure Show (APos : TPoint; Titel,Desc,Hint : string;
                    ACols : integer; SList : TStrings);
  end;

function EditList (APos : TPoint; Titel,Desc,Hint : string;
                   Options : TSelectOptions; ACols : integer;
                   Convert : TTextChange; const Default : string;
                   SList : TStrings; var AText  : string) : boolean;

function SelectFromList (APos : TPoint; Titel,Desc,Hint : string; Prompt : boolean;
                    SList : TStrings; var AText  : string) : boolean;

var
  SelectFromListDialog: TSelectFromListDialog;

implementation

{$R *.DFM}

uses Vcl.Dialogs, InpText, GnuGetText, ExtSysUtils, WinUtils;

{------------------------------------------------------------------- }
procedure TSelectFromListDialog.FormCreate(Sender: TObject);
begin
  TranslateComponent (self,'dialogs');
  FCheckEntry:=nil; dist:=10;
  lbHint.Width:=gbxEdit.Width;
  end;

{$IFDEF HDPI}   // scale glyphs and images for High DPI
procedure TSelectFromListDialog.AfterConstruction;
begin
  inherited;
  if Application.Tag=0 then
    ScaleButtonGlyphs(self,PixelsPerInchOnDesign,Monitor.PixelsPerInch);
  dist:=MulDiv(dist,PixelsPerInchOnDesign,Monitor.PixelsPerInch);
  end;
{$EndIf}

function TSelectFromListDialog.DialogPos(Sender: TObject) : TPoint;
begin
  Result:=BottomLeftPos((Sender as TControl),Point(-100,10));
  end;

procedure TSelectFromListDialog.btnInsertClick(Sender: TObject);
var
  s  : string;
  ok : boolean;
begin
  s:='';
  if InputText(DialogPos(Sender),dgettext('dialogs','Add item'),lbDesc.Caption,false,'',nil,false,s) then begin
    if assigned(FCheckEntry) then ok:=FCheckEntry(s) else ok:=true;
    if ok then with lbxStringList do ItemIndex:=Items.Add(s)
    else ErrorDialog(CursorPos,TryFormat(dgettext('dialogs','Invalid entry: "%s"'),[s]));
    end;
  end;

procedure TSelectFromListDialog.btnDeleteClick(Sender: TObject);
var
  s : string;
  n : integer;
begin
  with lbxStringList do if Multiselect then begin
    if ConfirmDialog (DialogPos(Sender),Caption,dgettext('dialogs','Remove all selected items?'),mbYes) then begin
      for n:=Items.Count-1 downto 0 do if Selected[n] then Items.Delete(n);
      end
    end
  else if ItemIndex>=0 then begin
    s:=Items[ItemIndex];
    if ConfirmDialog (DialogPos(Sender),Caption,TryFormat(dgettext('dialogs','Remove item: "%s"?'),[s]),mbYes) then begin
      n:=ItemIndex;
      Items.Delete(ItemIndex);
      if n>Items.Count then ItemIndex:=Items.Count-1 else ItemIndex:=n;
      end;
    end;
  end;

procedure TSelectFromListDialog.btnEditClick(Sender: TObject);
var
  s  : string;
  ok : boolean;
begin
  with lbxStringList do if ItemIndex>=0 then begin
    s:=Items[ItemIndex];
    if InputText(DialogPos(Sender),dgettext('dialogs','Edit item'),lbDesc.Caption,false,'',nil,false,s) then begin
      if assigned(FCheckEntry) then ok:=FCheckEntry(s) else ok:=true;
      if ok then Items[ItemIndex]:=s
      else ErrorDialog(CursorPos,TryFormat(dgettext('dialogs','Invalid entry: "%s"'),[s]));
      end;
    end;
  end;

procedure TSelectFromListDialog.btnDefaultClick(Sender: TObject);
begin
  if ConfirmDialog(DialogPos(Sender),Caption,dgettext('dialogs','Reset to default values?'),mbYes) then with lbxStringList do begin
    Clear;
    Items.DelimitedText:=DefDelimitedText;
    end;
  end;

procedure TSelectFromListDialog.lbxStringListDblClick(Sender: TObject);
var
  s : string;
begin
  if FEdit then begin
    with lbxStringList do if ItemIndex>=0 then begin
      s:=Items[ItemIndex];
      if InputText(CursorPos(Point(-20,20)),dgettext('dialogs','Edit item'),lbDesc.Caption,false,'',nil,false,s) then Items[ItemIndex]:=s;
      end
    end
  else ModalResult:=mrOK;
  end;

procedure TSelectFromListDialog.btnPromptClick(Sender: TObject);
//var
//  ok : boolean;
begin
  ModalResult:=mrYes;
//  if InputText(DialogPos(Sender),dgettext('dialogs','Edit item'),lbDesc.Caption,false,'',nil,false,FText) then begin
//    if assigned(FCheckEntry) then ok:=FCheckEntry(FText) else ok:=true;
//    if ok then ModalResult:=mrYes
//    else ErrorDialog(CursorPos,TryFormat(dgettext('dialogs','Invalid entry: "%s"'),[FText]));
//    end;
  end;

{------------------------------------------------------------------- }
procedure TSelectFromListDialog.UpBtnClick(Sender: TObject);
var
  n : integer;
begin
  with lbxStringList,Items do if (Count>0) and (ItemIndex>0) then begin
    n:=ItemIndex;
    Exchange(n,n-1);
    ItemIndex:=n-1;
    end;
  end;

procedure TSelectFromListDialog.DownBtnClick(Sender: TObject);
var
  n : integer;
begin
  with lbxStringList,Items do if (Count>0) and (ItemIndex<Count-1) then begin
    n:=ItemIndex;
    Exchange(n,n+1);
    ItemIndex:=n+1;
    end;
  end;

{------------------------------------------------------------------- }
function TSelectFromListDialog.Execute (APos : TPoint; Titel,Desc,Hint : string;
                    Options : TSelectOptions; ACols : integer;
                    Convert : TTextChange; const Default : string;
                    SList : TStrings; var AText : string;
                    ShowCancel : boolean = true; CheckEntry : TCheckEntry = nil) : TModalResult;
var
  i,h,d : integer;
begin
  with APos do begin
    if (Y < 0) or (X < 0) then Position:=poScreenCenter
    else begin
      Position:=poDesigned;
      CheckScreenBounds(Screen,x,y,Width,Height);
      Left:=x; Top:=y;
      end;
    end;
  Caption:=Titel;
  CancelBtn.Visible:=ShowCancel;
  FCheckEntry:=CheckEntry;
  FText:=''; h:=gbxEdit.Top;
  lbDesc.Caption:=Desc;
  lbHint.Caption:=Hint;
  FEdit:=soEdit in Options;
  btnPrompt.Visible:=soPrompt in Options;
  gbxEdit.Visible:=FEdit;
  gbxMove.Visible:=soOrder in Options;
  if length(Default)>0 then begin
    btnDefault.Show;
    with btnDefault do gbxEdit.Height:=Top+Height+dist;
    end
  else begin
    btnDefault.Hide;
    with btnEdit do gbxEdit.Height:=Top+Height+dist;
    with gbxEdit do gbxMove.Top:=Top+Height+dist;
    end;
  inc(h,gbxEdit.Height+dist);
  with gbxMove do if Visible then begin
    lbHint.Top:=Top+Height+dist;
    inc(h,gbxMove.Height+dist);
    end
  else lbHint.Top:=Top;
  inc(h,lbHint.Height);
  with btnPrompt do if Visible then inc(h,Height+dist);
  ClientHeight:=h+2*OKBtn.Height+dist;
  with lbHint do Anchors:=Anchors+[akBottom];
  DefDelimitedText:=Default;
  with lbxStringList do begin
    Items.Delimiter:=SList.Delimiter;
    Items.QuoteChar:=SList.QuoteChar;
    Items:=SList;
    Columns:=ACols;
    ExtendedSelect:=soMulti in Options;
    MultiSelect:=ExtendedSelect;
    ItemIndex:=Items.IndexOf(AText);
    end;
  Result:=ShowModal;
  if Result<>mrCancel then with lbxStringList do begin
    if Convert<>tcNone then with Items do begin
      for i:=0 to Count-1 do Strings[i]:=TextChangeCase(Strings[i],Convert);
      end;
    if FEdit then SList.DelimitedText:=Items.DelimitedText;
    if ItemIndex>=0 then begin
      if soMulti in Options then begin
        AText:='';
        for i:=0 to Items.Count-1 do if Selected[i] then AText:=AText+Items[i]+'|';
        delete(AText,length(Atext),1);
        end
      else AText:=Items[ItemIndex]
      end
    else AText:='';
    end
  end;

function TSelectFromListDialog.Select(APos : TPoint; Titel,Desc,Hint : string;
                    Options : TSelectOptions; ACols : integer;
                    Convert : TTextChange; const Default : string;
                    SList : TStrings; var AText : string;
                    ShowCancel : boolean = true; CheckEntry : TCheckEntry = nil) : boolean;
begin
  Result:=Execute(APos,Titel,Desc,Hint,Options,ACols,tcNone,'',SList,AText,ShowCancel,CheckEntry)<>mrCancel;
  end;

function TSelectFromListDialog.Select(APos : TPoint; Titel,Desc,Hint : string;
                    ACols : integer; Convert : TTextChange; const Default : string;
                    var ListText : string;
                    ADel : Char = ','; AQuote : Char ='"') : boolean;
var
  sl : TStringList;
  s  : string;
begin
  sl:=TStringList.Create;
  with sl do begin
    Sorted:=true; Delimiter:=ADel; QuoteChar:=AQuote;
    DelimitedText:=ListText;
    end;
  Result:=Select(APos,Titel,Desc,Hint,[],ACols,tcNone,'',sl,s,true);
  ListText:=sl.DelimitedText;
  sl.Free;
  end;

function TSelectFromListDialog.Select (APos : TPoint; Titel,Desc,Hint : string; Prompt : boolean;
                    SList : TStrings; var AText  : string) : TModalResult;
var
  so : TSelectOptions;
begin
  if Prompt then so:=[soPrompt] else so:=[];
  Result:=SelectFromListDialog.Execute(APos,Titel,Desc,Hint,so,0,
                                       tcNone,'',SList,AText);
  end;

procedure TSelectFromListDialog.Show (APos : TPoint; Titel,Desc,Hint : string;
                                      ACols : integer; SList : TStrings);
var
  s : string;
begin
  Execute(APos,Titel,Desc,Hint,[],ACols,tcNone,'',SList,s,false);
  end;

{------------------------------------------------------------------- }
function EditList (APos : TPoint; Titel,Desc,Hint : string;
                   Options : TSelectOptions; ACols : integer;
                   Convert : TTextChange; const Default : string;
                   SList : TStrings; var AText  : string) : boolean;
begin
  if not assigned(SelectFromListDialog) then
    SelectFromListDialog:=TSelectFromListDialog.Create(Application);
  Result:=SelectFromListDialog.Select(APos,Titel,Desc,Hint,Options,ACols,
                                       Convert,Default,SList,AText);
  FreeAndNil(SelectFromListDialog)
  end;

function SelectFromList (APos : TPoint; Titel,Desc,Hint : string; Prompt : boolean;
                    SList : TStrings; var AText  : string) : boolean;
var
  so : TSelectOptions;
begin
  if not assigned(SelectFromListDialog) then
    SelectFromListDialog:=TSelectFromListDialog.Create(Application);
  if Prompt then so:=[soPrompt] else so:=[];
  Result:=SelectFromListDialog.Select(APos,Titel,Desc,Hint,so,0,
                                       tcNone,'',SList,AText);
  FreeAndNil(SelectFromListDialog)
  end;

end.
