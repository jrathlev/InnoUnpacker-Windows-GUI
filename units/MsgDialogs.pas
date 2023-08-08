(* Delphi-Unit
   Message Dialogs
   ===============
   Uses library function "CreateMessageDialog" from "Vcl.Dialogs"
   The messages are not accessible to screenreaders (use "ShowMessageDlg" instead)

   © Dr. J. Rathlev, D-24222 Schwentinental (kontakt(a)rathlev-home.de)

   The contents of this file may be used under the terms of the
   Mozilla Public License ("MPL") or
   GNU Lesser General Public License Version 2 or later (the "LGPL")

   Software distributed under this License is distributed on an "AS IS" basis,
   WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
   the specific language governing rights and limitations under the License.

   July 2022
   last modified: July 2022
   *)

unit MsgDialogs;

interface

uses System.SysUtils, System.Types, Vcl.Forms, Vcl.Dialogs;

{ ---------------------------------------------------------------- }
// MessageDlg in Bildschirmmitte (X<0) oder an Position X,Y
function MessageDialog(const Title,Msg: string; DlgType: TMsgDlgType;
                       Buttons: TMsgDlgButtons; DefaultButton : TMsgDlgBtn;
                       Pos : TPoint; Delay : integer;
                       AMonitor : TDefaultMonitor = dmActiveForm) : integer; overload;
function MessageDialog(const Title,Msg: string; DlgType: TMsgDlgType;
                       Buttons: TMsgDlgButtons;
                       Pos : TPoint; Delay : integer;
                       AMonitor : TDefaultMonitor = dmActiveForm) : integer; overload;
function MessageDialog(const Title,Msg: string; DlgType: TMsgDlgType;
                       Buttons: TMsgDlgButtons) : integer; overload;
function MessageDialog(const Msg: string; DlgType: TMsgDlgType;
                       Buttons: TMsgDlgButtons) : integer; overload;
function MessageDialog(Pos : TPoint; const Msg: string; DlgType: TMsgDlgType;
                       Buttons: TMsgDlgButtons) : integer;  overload;

function ConfirmDialog (const Title,Msg : string;
                        AMonitor : TDefaultMonitor = dmActiveForm) : boolean; overload;
function ConfirmDialog (const Msg : string; DefaultButton : TMsgDlgBtn = mbYes;
                        AMonitor : TDefaultMonitor = dmActiveForm) : boolean; overload;
function ConfirmDialog (Pos : TPoint; const Msg : string; DefaultButton : TMsgDlgBtn = mbYes;
                        AMonitor : TDefaultMonitor = dmActiveForm) : boolean; overload;
function ConfirmDialog (Pos : TPoint; const Title,Msg : string;
                        AMonitor : TDefaultMonitor = dmActiveForm) : boolean; overload;
function ConfirmDialog (Pos : TPoint; const Title,Msg : string; DefaultButton : TMsgDlgBtn;
                        AMonitor : TDefaultMonitor = dmActiveForm) : boolean; overload;

function ConfirmRetryDialog (const Msg : string) : boolean; overload;
function ConfirmRetryDialog (Pos : TPoint; const Msg : string; DefaultButton : TMsgDlgBtn = mbRetry;
                       AMonitor : TDefaultMonitor = dmActiveForm) : boolean; overload;


procedure InfoDialog (const Title,Msg : string; Delay : integer;
                      AMonitor : TDefaultMonitor = dmActiveForm); overload;
procedure InfoDialog (const Title,Msg : string;
                      AMonitor : TDefaultMonitor = dmActiveForm); overload;
procedure InfoDialog (const Msg : string;
                      AMonitor : TDefaultMonitor = dmActiveForm); overload;
procedure InfoDialog (Pos : TPoint; const Title,Msg : string;
                      AMonitor : TDefaultMonitor = dmActiveForm); overload;
procedure InfoDialog (Pos : TPoint; const Msg : string;
                      AMonitor : TDefaultMonitor = dmActiveForm); overload;

procedure ErrorDialog (const Title,Msg : string; x,y : integer;
                       AMonitor : TDefaultMonitor = dmActiveForm); overload;
procedure ErrorDialog (const Title,Msg : string; Delay : integer;
                       AMonitor : TDefaultMonitor = dmActiveForm); overload;
procedure ErrorDialog (const Msg : string; Delay : integer;
                       AMonitor : TDefaultMonitor = dmActiveForm); overload;
procedure ErrorDialog (const Title,Msg : string;
                       AMonitor : TDefaultMonitor = dmActiveForm); overload;
procedure ErrorDialog (const Msg : string;
                       AMonitor : TDefaultMonitor = dmActiveForm); overload;
procedure ErrorDialog (Pos : TPoint; const Title,Msg : string;
                       AMonitor : TDefaultMonitor = dmActiveForm); overload;
procedure ErrorDialog (Pos : TPoint; const Msg : string;
                       AMonitor : TDefaultMonitor = dmActiveForm); overload;


implementation

uses Vcl.Controls, WinUtils;

{ ---------------------------------------------------------------- }
// neuer Message-Dialog mit Positionspr¸fung
// Delay = 0: ShowModal
//       > 0: Anzeigen und automatisch schlieﬂen nach "Delay" in s
function MessageDialog(const Title,Msg: string; DlgType: TMsgDlgType;
                Buttons: TMsgDlgButtons; DefaultButton : TMsgDlgBtn;
                Pos : TPoint; Delay : integer;
                AMonitor : TDefaultMonitor = dmActiveForm) : integer;
var
  w : integer;
begin
  with CreateMessageDialog(Msg,DlgType,Buttons,DefaultButton) do begin
    Scaled:=true;
    DefaultMonitor:=AMonitor;
    try
      with Pos do begin
        if (Y < 0) and (X < 0) then Position:=poScreenCenter
        else begin
          CheckScreenBounds(Screen,x,y,Width,Height);
          Left:=x; Top:=y;
          end;
        end;
      if length(Title)>0 then begin
        Caption:=Title;
        w:=Canvas.TextWidth(Title)+50;
        if w>ClientWidth then ClientWidth:=w;
        end;
      FormStyle:=fsStayOnTop;
      if Delay<=0 then Result:=ShowModal
      else begin
        Show;
        Delay:=Delay*10;
        repeat
          Application.ProcessMessages;
          Sleep(100);
          dec(Delay);
          until (Delay=0) or (ModalResult<>mrNone);
        if ModalResult=mrNone then begin
          Close;
          Result:=mrOK;
          end
        else Result:=ModalResult;
        end;
    finally
      Free;
      end;
    end;
  end;

function MessageDialog(const Title,Msg: string; DlgType: TMsgDlgType;
                Buttons: TMsgDlgButtons;
                Pos : TPoint; Delay : integer;
                AMonitor : TDefaultMonitor = dmActiveForm) : integer;
var
  DefaultButton: TMsgDlgBtn;
begin
  if mbOk in Buttons then DefaultButton := mbOk else
    if mbYes in Buttons then DefaultButton := mbYes else
      DefaultButton := mbRetry;
  Result:=MessageDialog(Title,Msg,DlgType,Buttons,DefaultButton,Pos,Delay,AMonitor);
end;

function MessageDialog(const Title,Msg: string; DlgType: TMsgDlgType;
  Buttons: TMsgDlgButtons) : integer;
begin
  Result:=MessageDialog(Title,Msg,DlgType,Buttons,CenterPos,0);
  end;

function MessageDialog(const Msg: string; DlgType: TMsgDlgType;
  Buttons: TMsgDlgButtons) : integer;
begin
  Result:=MessageDialog('',Msg,DlgType,Buttons,CenterPos,0);
  end;

function MessageDialog(Pos : TPoint; const Msg: string; DlgType: TMsgDlgType;
  Buttons: TMsgDlgButtons) : integer;
begin
  Result:=MessageDialog('',Msg,DlgType,Buttons,Pos,0);
  end;

{ ---------------------------------------------------------------- }
// Best‰tigung in Bildschirmmitte (X<0) oder an Position X,Y
function ConfirmDialog (Pos : TPoint; const Title,Msg : string;
                        AMonitor : TDefaultMonitor) : boolean;
begin
  Result:=MessageDialog (Title,Msg,mtConfirmation,[mbYes,mbNo],Pos,0,AMonitor)=mrYes;
  end;

// Best‰tigung auf einstellbarem Monitor
function ConfirmDialog (Pos : TPoint; const Msg : string; DefaultButton : TMsgDlgBtn;
                        AMonitor : TDefaultMonitor) : boolean;
begin
  Result:=MessageDialog ('',Msg,mtConfirmation,[mbYes,mbNo],DefaultButton,Pos,0,AMonitor)=mrYes;
  end;

function ConfirmDialog (Pos : TPoint; const Title,Msg : string; DefaultButton : TMsgDlgBtn;
                        AMonitor : TDefaultMonitor) : boolean;
begin
  Result:=MessageDialog (Title,Msg,mtConfirmation,[mbYes,mbNo],DefaultButton,Pos,0,AMonitor)=mrYes;
  end;

// Best‰tigung in Bildschirmmitte
function ConfirmDialog (const Title,Msg : string;
                        AMonitor : TDefaultMonitor) : boolean;
begin
  Result:=ConfirmDialog(CenterPos,Title,Msg,AMonitor);
  end;

function ConfirmDialog (const Msg : string; DefaultButton : TMsgDlgBtn;
                        AMonitor : TDefaultMonitor) : boolean;
begin
  Result:=MessageDialog ('',Msg,mtConfirmation,[mbYes,mbNo],DefaultButton,CenterPos,0,AMonitor)=mrYes;
  end;

function ConfirmRetryDialog (const Msg : string) : boolean; overload;
begin
  Result:=ConfirmDialog(CenterPos,Msg);
  end;

function ConfirmRetryDialog (Pos : TPoint; const Msg : string; DefaultButton : TMsgDlgBtn = mbRetry;
                       AMonitor : TDefaultMonitor = dmActiveForm) : boolean; overload;
begin
  Result:=MessageDialog('',Msg,mtError,[mbRetry,mbCancel],DefaultButton,Pos,0,AMonitor)=mrRetry;
  end;

// Information an Position ausgeben
procedure InfoDialog (Pos : TPoint; const Title,Msg : string;
                     AMonitor : TDefaultMonitor);
begin
  MessageDialog (Title,Msg,mtInformation,[mbOK],Pos,0,AMonitor);
  end;

procedure InfoDialog (Pos : TPoint; const Msg : string;
                      AMonitor : TDefaultMonitor);
begin
  InfoDialog(Pos,'',Msg,AMonitor);
  end;

// Information in Bildschirmmitte ausgeben
procedure InfoDialog (const Title,Msg : string;
                      AMonitor : TDefaultMonitor);
begin
  InfoDialog(CenterPos,Title,Msg,AMonitor);
  end;

// Information in Bildschirmmitte ausgeben und f¸r Delay s anzeigen
procedure InfoDialog (const Title,Msg : string; Delay : integer;
                      AMonitor : TDefaultMonitor);
begin
  MessageDialog (Title,Msg,mtInformation,[mbOK],CenterPos,Delay,AMonitor);
  end;

procedure InfoDialog (const Msg :string;
                      AMonitor : TDefaultMonitor);
begin
  InfoDialog(CenterPos,'',Msg,AMonitor);
  end;

// Fehlermeldung an Position ausgeben
procedure ErrorDialog (const Title,Msg : string; x,y : integer;
                       AMonitor : TDefaultMonitor);
begin
  MessageDialog (Title,Msg,mtError,[mbOK],Point(x,y),0,AMonitor);
  end;

procedure ErrorDialog (Pos : TPoint; const Title,Msg : string;
                       AMonitor : TDefaultMonitor);
begin
  MessageDialog (Title,Msg,mtError,[mbOK],Pos,0,AMonitor);
  end;

procedure ErrorDialog (Pos : TPoint; const Msg : string;
                       AMonitor : TDefaultMonitor);
begin
  ErrorDialog(Pos,'',Msg,AMonitor);
  end;

// Fehlermeldung in Bildschirmmitte ausgeben und f¸r Delay s anzeigen
procedure ErrorDialog (const Title,Msg : string; Delay : integer;
                       AMonitor : TDefaultMonitor);
begin
  MessageDialog (Title,Msg,mtError,[mbOK],CenterPos,Delay,AMonitor);
  end;

procedure ErrorDialog (const Msg : string; Delay : integer;
                       AMonitor : TDefaultMonitor);
begin
  ErrorDialog(CenterPos,'',Msg,AMonitor);
  end;

// Fehlermeldung in Bildschirmmitte ausgeben
procedure ErrorDialog (const Title,Msg : string;
                       AMonitor : TDefaultMonitor);
begin
  ErrorDialog(CenterPos,Title,Msg,AMonitor);
  end;

procedure ErrorDialog (const Msg : string;
                       AMonitor : TDefaultMonitor);
begin
  ErrorDialog(CenterPos,'',Msg,AMonitor);
  end;

end.
