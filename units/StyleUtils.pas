(* Delphi-Unit
   routines for visual styles (themes)
   ===================================

   © Dr. J. Rathlev, D-24222 Schwentinental (kontakt(a)rathlev-home.de)

   The contents of this file may be used under the terms of the
   Mozilla Public License ("MPL") or
   GNU Lesser General Public License Version 2 or later (the "LGPL")

   Software distributed under this License is distributed on an "AS IS" basis,
   WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
   the specific language governing rights and limitations under the License.

   New compilation - June 2016
   last modified: March 2024
   *)

unit StyleUtils;

interface

uses  System.Sysutils, System.Classes, System.UITypes, Vcl.Themes;

type
  TDisplayMode = (dmDefault,dmLight,dmDark);

const
  IniDispMode = 'DisplayMode';

function ColorDistanceRGBLinear(Old, New: TColor): Integer;

function ReadStyleName (const IniName,CfgSekt : string) : string;
procedure WriteStyleName (const IniName,CfgSekt,AStyle : string);

function GetActiveStyle : string;
function SetStyle (const AStyle : string) : boolean;
function ResetStyle : boolean;
function GetStylesCount : integer;
function GetStylesList (AList : TStrings) : string;

function DarkModeIsEnabled : boolean;      // get system setting
procedure SetDefaultStyles (const ADarkStyle : string); overload;
procedure SetDefaultStyles (const ALightStyle,ADarkStyle : string); overload;
procedure SetSystemStyle;
procedure SelectStyle(ADarkMode : boolean);
procedure HandleStyles (const Section : string);
function StylesEnabled : boolean;

function LoadDisplayModeFromIni(const IniName,CfgSekt : string) : TDisplayMode;
procedure SaveDisplayModeFromIni(const IniName,CfgSekt : string; DisplayMode : TDisplayMode);
procedure SetDisplayMode(DisplayMode : TDisplayMode);

function GetMenuColor(Detail : TThemedMenu; ElementColor : TElementColor; DefaultColor : TColor) : TColor;

function GetColor(StyleColor : TStyleColor; DefaultColor : TColor) : TColor;
function GetFontColor(StyleFont: TStyleFont; DefaultColor : TColor) : TColor;
function GetSysColor(SysColor : TColor): TColor;

implementation

uses System.IniFiles, System.Win.Registry, WinApi.Windows, MsgDialogs;

const
  IniStyle = 'StyleName';

var
  defStyle,LightStyle,DarkStyle : string;

{ ------------------------------------------------------------------- }
function ColorDistanceRGBLinear(Old, New: TColor): Integer;
begin
  if (Old = New) then Result := 0
  else
  // Difference in transparency = max difference
    if (TColorRec(Old).A=0) xor (TColorRec(New).A=0) then Result := 255
  else begin
    Result:=Round(Sqrt((sqr(TColorRec(New).R-TColorRec(Old).R)
                      +sqr(TColorRec(New).G-TColorRec(Old).G)
                      +sqr(TColorRec(New).B-TColorRec(Old).B))/3));
    end;
  end;

{ ------------------------------------------------------------------- }
// Helper functions for visual styles
function ReadStyleName (const IniName,CfgSekt : string) : string;
begin
  Result:='';
  if FileExists(IniName) then with TMemIniFile.Create(IniName) do begin
    Result:=ReadString(CfgSekt,IniStyle,'');
    if (length(Result)=0) and Assigned(TStyleManager.ActiveStyle) then
      Result:=TStyleManager.ActiveStyle.Name;
    end;
  end;

procedure WriteStyleName (const IniName,CfgSekt,AStyle : string);
begin
  if FileExists(IniName) then with TMemIniFile.Create(IniName) do begin
    if length(AStyle)>0 then WriteString(CfgSekt,IniStyle,AStyle)
    else DeleteKey(CfgSekt,IniStyle);
    UpdateFile;
    Free;
    end;
  end;

function GetActiveStyle : string;
begin
  Result:=TStyleManager.ActiveStyle.Name;
  end;

function SetStyle (const AStyle : string) : boolean;
begin
  Result:=false;
  if (length(AStyle)>0) then with TStyleManager do begin
    if not AnsiSameText(ActiveStyle.Name,AStyle) then Result:=TrySetStyle(AStyle,false);
    end;
  end;

function ResetStyle : boolean;
begin
  Result:=SetStyle(defStyle);
  end;

function GetStylesCount : integer;
begin
  Result:=length(TStyleManager.StyleNames);
  end;

function GetStylesList (AList : TStrings) : string;
var
  s : string;
begin
  AList.Clear;
  with TStyleManager do begin
    Result:=ActiveStyle.Name;
    if length(StyleNames)>0 then
      for s in TStyleManager.StyleNames do AList.Add(s);
    end;
  end;

{ ------------------------------------------------------------------- }
// read Windows registry setting
const
  RegPersonalize = 'Software\Microsoft\Windows\CurrentVersion\Themes\Personalize';
  RegLightTheme = 'AppsUseLightTheme';

function DarkModeIsEnabled : boolean;
begin
  Result:=false;
  with TRegistry.Create do begin
    try
      RootKey:=HKEY_CURRENT_USER;
      if OpenKeyReadOnly(RegPersonalize) then begin
        try
          if ValueExists(RegLightTheme) then Result:=ReadInteger(RegLightTheme)=0;
        finally
          CloseKey;
          end;
        end;
    finally
      Free;
      end;
    end;
  end;

procedure SetDefaultStyles (const ALightStyle,ADarkStyle : string);
begin
  LightStyle:=ALightStyle; DarkStyle:=ADarkStyle;
  end;

procedure SetDefaultStyles (const ADarkStyle : string);
begin
  LightStyle:=defStyle; DarkStyle:=ADarkStyle;
  end;

procedure SelectStyle (ADarkMode : boolean);
var
  sn : string;
begin
  if ADarkMode then sn:=DarkStyle else sn:=LightStyle;
  with TStyleManager do if not AnsiSameText(ActiveStyle.Name,sn) then
    TrySetStyle(sn);
  end;

procedure SetSystemStyle;
begin
  SelectStyle(DarkModeIsEnabled);
  end;

// call this procedure on receiving Windows message WM_SETTINGCHANGE
// procedure WMSettingChange(var Message: TWMSettingChange); message WM_SETTINGCHANGE;
procedure HandleStyles (const Section : string);
begin
  if SameText('ImmersiveColorSet',Section) then SetSystemStyle;;
  end;
// Note: alternative Windows message: WM_DWMCOLORIZATIONCOLORCHANGED in WndProc 

function StylesEnabled : boolean;
begin
  Result:=not (StyleServices is TUxThemeStyle);
  end;

function LoadDisplayModeFromIni(const IniName,CfgSekt : string) : TDisplayMode;
begin
  with TMemIniFile.Create(IniName) do begin
    Result:=TDisplayMode(ReadInteger(CfgSekt,IniDispMode,integer(dmDefault)));
    Free;
    end;
  end;

procedure SaveDisplayModeFromIni(const IniName,CfgSekt : string; DisplayMode : TDisplayMode);
begin
  with TMemIniFile.Create(IniName) do begin
    WriteInteger(CfgSekt,IniDispMode,integer(DisplayMode));
    UpdateFile;
    Free;
    end;
  end;

procedure SetDisplayMode(DisplayMode : TDisplayMode);
begin
  case DisplayMode of
  dmLight : SelectStyle(false);
  dmDark  : SelectStyle(true);
  else SetSystemStyle;
    end;
  end;

function GetMenuColor(Detail : TThemedMenu; ElementColor : TElementColor; DefaultColor : TColor) : TColor;
var
  LDetails: TThemedElementDetails;
begin
  if (StyleServices is TUxThemeStyle) then Result:=DefaultColor // user defined color for style 'Windows'
  else begin
    LDetails:=StyleServices.GetElementDetails(Detail);
    if not StyleServices.GetElementColor(LDetails,ElementColor,Result) then Result:=DefaultColor;
    end;
  end;

function GetColor(StyleColor : TStyleColor; DefaultColor : TColor) : TColor;
begin
  if (StyleServices is TUxThemeStyle) then Result:=DefaultColor // user defined color for style 'Windows'
  else Result:=StyleServices.GetStyleColor(StyleColor);
  end;

function GetFontColor(StyleFont: TStyleFont; DefaultColor : TColor) : TColor;
begin
  if (StyleServices is TUxThemeStyle) then Result:=DefaultColor // user defined color for style 'Windows'
  else Result:=StyleServices.GetStyleFontColor(StyleFont);
  end;

function GetSysColor(SysColor : TColor): TColor;
begin
  if (StyleServices is TUxThemeStyle) then Result:=SysColor
  else Result:=StyleServices.GetSystemColor(SysColor);
  end;

initialization
  defStyle:=TStyleManager.ActiveStyle.Name;
  LightStyle:=defStyle; DarkStyle:=defStyle;

end.
