(* Delphi Unit
   Implementation of WMI functions
   ===============================

   © Dr. J. Rathlev, D-24222 Schwentinental (kontakt(a)rathlev-home.de)

   The contents of this file may be used under the terms of the
   GNU Lesser General Public License Version 2 or later (the "LGPL")

   Software distributed under this License is distributed on an "AS IS" basis,
   WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
   the specific language governing rights and limitations under the License.

   https://docs.microsoft.com/en-us/windows/win32/cimwin32prov/operating-system-classes
   
   Vers. 1 - March 2020
   last updated:
   *)

unit WmiUtils;

interface

function OptionalFeatureInstalled (const FeatName : string): boolean;

function Smb1Installed : boolean;

implementation

uses Winapi.Windows, System.SysUtils, WinApi.ActiveX, System.Win.ComObj;

function GetWmiClassItem (const WmiClass,FeatName : string; var Item : OLEVariant) : boolean;
var
  FLocator,FCmiService,
  Items : OLEVariant;
  Enum : IEnumVariant;
  pCf : LongWord;
begin
  Result:=false;
  try
    CoInitialize(nil);
    try
      FLocator:=CreateOleObject('WbemScripting.SWbemLocator');
      FCmiService:=FLocator.ConnectServer('localhost','root\cimv2');
      Items:=FCmiService.ExecQuery(Format('Select * from %s where Name="%s"',[WmiClass,FeatName]));
      Enum:=IUnknown(Items._NewEnum) as IEnumVariant;
      if Enum.Next(1,Item,pCf)=0 then Result:=true;
    except
      end;
  finally
    CoUninitialize;
    end;
  end;

function OptionalFeatureInstalled1 (const FeatName : string): boolean;
var
  Item : OLEVariant;
begin
  Result:=GetWmiClassItem('Win32_OptionalFeature',FeatName,Item);
  if Result then Result:=Item.InstallState=1;
  end;
  
function OptionalFeatureInstalled (const FeatName : string): boolean;
var
  FLocator,FCmiService,
  Items,Item : OLEVariant;
  Enum : IEnumVariant;
  pCf : LongWord;
begin
  Result:=false;
  try
    CoInitialize(nil);
    try
      FLocator:=CreateOleObject('WbemScripting.SWbemLocator');
      FCmiService:=FLocator.ConnectServer('localhost','root\cimv2');
      Items:=FCmiService.ExecQuery(Format('Select * from Win32_OptionalFeature where Name="%s"',[FeatName]));
      Enum:=IUnknown(Items._NewEnum) as IEnumVariant;
      if Enum.Next(1,Item,pCf)=0 then Result:=Item.InstallState=1;
    except
      end;
  finally
    CoUninitialize;
    end;
  end;

function Smb1Installed : boolean;
begin
  Result:=OptionalFeatureInstalled('SMB1Protocol-Client');
  end;

end.
