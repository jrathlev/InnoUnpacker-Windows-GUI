(* Delphi Unit
   Show text and buttons to select several options
   ===============================================
   Messages are accessible to screenreaders (uses "TStaticText")

   © Dr. J. Rathlev, D-24222 Schwentinental (kontakt(a)rathlev-home.de)

   The contents of this file may be used under the terms of the
   Mozilla Public License ("MPL") or
   GNU Lesser General Public License Version 2 or later (the "LGPL")

   Software distributed under this License is distributed on an "AS IS" basis,
   WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
   the specific language governing rights and limitations under the License.

   Vers. 1 - June 2009
   last modified: July 2025
   *)

unit SelectDlg;

interface

uses Winapi.Windows, System.SysUtils, System.Classes, Vcl.Graphics, Vcl.Forms, 
  Vcl.Controls, Vcl.StdCtrls, Vcl.Dialogs, Vcl.Buttons, Vcl.ExtCtrls,
  Vcl.ImgList, System.ImageList, Vcl.Imaging.pngimage;

const
  OptionChecked = $100;
  OptionMask = $FF;

type
  TButtonArray = array of string;

  TSelectDialog = class(TForm)
    imgIcon: TImage;
    ilIcons: TImageList;
    cbOption: TCheckBox;
    stCaption: TStaticText;
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    { Private declarations }
    Buttons : array  of TButton;
    Selection,FBw,FPreSel : integer;
    ActiveScreenReader : boolean;
    FCaption,FOption,FDefault : string;
    FButtons : TButtonArray;
    procedure ButtonClick(Sender: TObject);
  public
    { Public declarations }
{$IFDEF HDPI}   // scale glyphs and images for High DPI
    procedure AfterConstruction; override;
{$EndIf}
    function Execute (const APos : TPoint;
                      const ATitle,ACaption : string;
                      DlgType        : TMsgDlgType;
                      ACaptionFormat : TFontStyles;
                      const AButtons : TButtonArray;
                      AOption        : string = '';
                      ButtonWidth    : integer = 0;
                      PreSelection   : integer = -1;
                      const DefaultButton : string = '') : integer;
    end;

var
  SelectDialog: TSelectDialog;

function SelectOption (const APos : TPoint;
                       const ATitle,ACaption : string;
                       DlgType        : TMsgDlgType;
                       ACaptionFormat : TFontStyles;
                       const AButtons : TButtonArray;
                       AOption        : string = '';
                       ButtonWidth    : integer = 0;
                       PreSelection   : integer = -1;
                       const DefaultButton : string = '') : integer; overload;

function SelectOption (const ATitle,ACaption : string;
                       DlgType        : TMsgDlgType;
                       ACaptionFormat : TFontStyles;
                       const AButtons : TButtonArray;
                       AOption        : string = '';
                       ButtonWidth    : integer = 0;
                       PreSelection   : integer = -1;
                       const DefaultButton : string = '') : integer; overload;

function SelectOption (const ACaption : string;
                       DlgType        : TMsgDlgType;
                       ACaptionFormat : TFontStyles;
                       const AButtons : TButtonArray;
                       AOption        : string = '';
                       ButtonWidth    : integer = 0;
                       PreSelection   : integer = -1;
                       const DefaultButton : string = '') : integer; overload;

implementation

{$R *.dfm}

uses Vcl.Consts, GnuGetText, WinUtils;

procedure TSelectDialog.FormCreate(Sender: TObject);
begin
  TranslateComponent (self,'dialogs');
  ActiveScreenReader:=false;
{$IFDEF ACCESSIBLE}
  if not SystemParametersInfo(SPI_GETSCREENREADER,0,@ActiveScreenReader,0) then ActiveScreenReader:=false;
{$ENDIF}
  end;

procedure TSelectDialog.FormShow(Sender: TObject);
var
  ppiw,l,n,w,ppi,
  h,dh,tw,tl,nr,nl,
  i,j,k              : integer;
begin
  ppi:=Monitor.PixelsPerInch;
  if FBw=0 then FBw:=105;
  FBw:=MulDiv(FBw,ppi,PixelsPerInchOnDesign);
  w:=3*FBw+MulDiv(70,ppi,PixelsPerInchOnDesign);
  l:=MulDiv(50,ppi,PixelsPerInchOnDesign);
  n:=MulDiv(10,ppi,PixelsPerInchOnDesign);
  tw:=MaxTextWidth(FCaption,Canvas);
  tl:=TextLineCount(FCaption);
  with stCaption do begin
    if fsBold in Font.Style then tw:=MulDiv(tw,5,4);
    Caption:=FCaption;
    Left:=l; Height:=tl*MulDiv(abs(Font.Height),12,10)+n;
    if tw<w-l-n then tw:=w-l-n else w:=tw+l+n;
    Width:=tw;
    h:=Top+Height+2*n;
    dh:=MulDiv(abs(Font.Height),24,10);
    TabOrder:=0; TabStop:=ActiveScreenReader;
    end;
  ClientWidth:=w;
  with cbOption do begin
    Checked:=false;
    if length(FOption)>0 then begin
      Left:=l; Width:=w-l-n; Top:=h-n div 2; inc(h,Height);
      Caption:=FOption;
      if FPreSel>0 then Checked:=FPreSel and OptionChecked <>0;
      Visible:=true;
      end
    else Visible:=false;
    end;
  tw:=MulDiv(5,ppi,PixelsPerInchOnDesign);
  n:=length(FButtons)+1;
  nr:=(n-1) div 3;       // Anzahl Zeilen
  nl:=n-3*nr;            // Spalten in letzter Zeile
  w:=ClientWidth-3*(FBw+tw)-tw;
  SetLength(Buttons,n);
  for i:=0 to nr-1 do begin
    for j:=0 to 2 do begin
      k:=3*i+j;
      Buttons[k]:=TButton.Create(self);
      with Buttons[k] do begin
        Parent:=self;
        Height:=dh; Width:=FBw;
        Top:=h; Left:=w+j*(Width+5);
        Caption:=FButtons[k];
        Tag:=k;
        TabStop:=true;
        TabOrder:=k+1;
        ModalResult:=mrOK;
        OnClick:=ButtonClick;
        end;
      end;
    inc(h,MulDiv(dh,30,26));
    end;
  for j:=0 to nl-1 do begin
    k:=3*nr+j;
    Buttons[k]:=TButton.Create(self);
    with Buttons[k] do begin
      Parent:=self;
      Height:=dh; Width:=FBw;
      Top:=h; Left:=w+(3-nl+j)*(Width+5);
      if k=n-1 then begin
        if length(FDefault)=0 then Caption:=dgettext('dialogs','Cancel')
        else Caption:=FDefault;
        Cancel:=true;
        if n>1 then Tag:=-1 else Tag:=0;
        end
      else begin
        Caption:=FButtons[k];
        Tag:=k;
        end;
      TabStop:=true;
      TabOrder:=k+1;
      ModalResult:=mrOK;
      OnClick:=ButtonClick;
      end;
    end;
  ClientHeight:=h+MulDiv(dh,30,26)+tw;
  Selection:=-1;
  if FPreSel>0 then FPreSel:=FPreSel and OptionMask;
  if FPreSel>length(FButtons) then FPreSel:=-1;
  if ActiveScreenReader then begin
    ActiveControl:=nil;
    with cbOption do if Visible then TabOrder:=1;
    end
  else begin
    with cbOption do if Visible then TabOrder:=n;
    if FPreSel<0 then ActiveControl:=Buttons[n-1]
    else ActiveControl:=Buttons[FPreSel];
    end;
  FitToScreen(Screen,self);
  end;

{$IFDEF HDPI}   // scale glyphs and images for High DPI
procedure TSelectDialog.AfterConstruction;
begin
  inherited;
  if Application.Tag=0 then
    ScaleImageList(ilIcons,PixelsPerInchOnDesign,Monitor.PixelsPerInch);
  end;
{$EndIf}

procedure TSelectDialog.ButtonClick(Sender: TObject);
begin
  Selection:=(Sender as TButton).Tag;
  end;

const
  Captions: array[TMsgDlgType] of Pointer = (@SMsgDlgWarning, @SMsgDlgError,
    @SMsgDlgInformation, @SMsgDlgConfirm, nil);

//   Result = -1 : default or cancel button
//             0,1,2,.. : Index of AButtons
function TSelectDialog.Execute (const APos : TPoint;
                                const ATitle,ACaption : string;
                                DlgType        : TMsgDlgType;
                                ACaptionFormat : TFontStyles;
                                const AButtons : TButtonArray;
                                AOption        : string = '';
                                ButtonWidth    : integer = 0;
                                PreSelection   : integer = -1;
                                const DefaultButton : string = '') : integer;
var
  w,h,dh,i,j,n,nr,nl,k,l,bw,ppi,tw,tl : integer;
begin
  if ATitle.IsEmpty then Caption:=LoadResString(Captions[DlgType])
  else Caption:=ATitle;
  FCaption:=ACaption; FOption:=AOption; FDefault:=DefaultButton;
  FButtons:=AButtons;
  FBw:=ButtonWidth; FPreSel:=PreSelection;
  stCaption.Font.Style:=ACaptionFormat;
  with imgIcon do if DlgType=mtCustom then Hide
  else begin
    Picture:=nil;
    ilIcons.GetBitmap(integer(DlgType),Picture.Bitmap);
    Show;
    end;
  AdjustFormPosition(Screen,self,APos);
  ShowModal;
  Result:=Selection;
  if (Result>=0) and cbOption.Checked then Result:=Result+OptionChecked;
  for i:=ControlCount-1 downto 0 do if (Controls[i] is TButton) then Controls[i].Free;
  end;

function SelectOption (const APos : TPoint;
                       const ATitle,ACaption : string;
                       DlgType: TMsgDlgType;
                       ACaptionFormat : TFontStyles;
                       const AButtons : TButtonArray;
                       AOption        : string = '';
                       ButtonWidth    : integer = 0;
                       PreSelection   : integer = -1;
                       const DefaultButton : string = '') : integer;
begin
  if not assigned(SelectDialog)then SelectDialog:=TSelectDialog.Create(Application);
  Result:=SelectDialog.Execute(APos,ATitle,ACaption,DlgType,ACaptionFormat,AButtons,
    AOption,ButtonWidth,PreSelection,DefaultButton);
  FreeAndNil(SelectDialog);
  end;


function SelectOption (const ATitle,ACaption : string;
                       DlgType: TMsgDlgType;
                       ACaptionFormat : TFontStyles;
                       const AButtons : TButtonArray;
                       AOption        : string = '';
                       ButtonWidth    : integer = 0;
                       PreSelection   : integer = -1;
                       const DefaultButton : string = '') : integer;
begin
if not assigned(SelectDialog)then SelectDialog:=TSelectDialog.Create(Application);
  Result:=SelectDialog.Execute(CenterPos,ATitle,ACaption,DlgType,ACaptionFormat,AButtons,
    AOption,ButtonWidth,PreSelection,DefaultButton);
  FreeAndNil(SelectDialog);
  end;

function SelectOption (const ACaption : string;
                       DlgType: TMsgDlgType;
                       ACaptionFormat : TFontStyles;
                       const AButtons : TButtonArray;
                       AOption        : string = '';
                       ButtonWidth    : integer = 0;
                       PreSelection   : integer = -1;
                       const DefaultButton : string = '') : integer;
begin
  Result:=SelectOption('',ACaption,DlgType,ACaptionFormat,AButtons,
    AOption,ButtonWidth,PreSelection,DefaultButton);
  end;

end.
