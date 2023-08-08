(* Delphi Unit
   resource strings of several units - English

   © Dr. J. Rathlev, D-24222 Schwentinental (kontakt(a)rathlev-home.de)

   The contents of this file may be used under the terms of the
   Mozilla Public License ("MPL") or
   GNU Lesser General Public License Version 2 or later (the "LGPL")

   Software distributed under this License is distributed on an "AS IS" basis,
   WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
   the specific language governing rights and limitations under the License.

   Vers. 1 - Jan. 2016
   last updated: Feb. 2017
   *)

unit UnitConsts;

interface

resourcestring


{ ------------------------------------------------------------------- }
// Strings from ExtSysUtils
  rsFormatError  = 'Format error: ';

{ ------------------------------------------------------------------- }
// Strings from NumberUtils
  rsHexError = '%s is not a valid hex value';    //'%s ist kein gültiger Hexwert';
  rsOctError = '%s is not a valid octal value';  //'%s ist kein gültiger Oktalwert';
  rsBinError = '%s is not a valid binary value'; //'%s ist kein gültiger Binärwert';

{ ------------------------------------------------------------------- }
// Strings from LangUtils
  rsSystemDefault = '&System default';
  rsLangRestart = 'Changing language requires the program to be restarted!';

{ ------------------------------------------------------------------- }
// Strings from TextFilter
  rsIllRegExp = 'Illegal regular expression!';
  rsIllmask = 'Illegal mask!';

{ ------------------------------------------------------------------- }
// Strings from WinExecute
  rsExecuteError = 'Error executing process';
  rsCreatePipeError = 'Stdout pipe creation failed';
  rsDupHandleError = 'DuplicateHandle failed';
  rsCloseHandleError = 'Closing handle failed';

{ ------------------------------------------------------------------- }
// Strings from CpDrv
  rsComNoError = 'No errors on COM connection';
  rsComOpenError = 'Error opening COM port';
  rsComStateError = 'Error setting COM port state';
  rsComFlushError = 'Error flushing COM buffers';
  rsComSetupError = 'Error initializing COM port';

{ ------------------------------------------------------------------- }
// Strings from WinUtils
  rsUnknownError = 'Unknown system error code';
  rsWindowsError = 'Windows error';
  rsStorageError = 'Storage error';
  rsRpcError = 'Remote procedure call error';
  rsInterfaceError = 'Interface error';
  rsDispatchError = 'Dispatch error';
  rsPreWin32Error = 'Error in SHFileOperation';
  rsShellExec = 'ShellExecute error';

{ ------------------------------------------------------------------- }
// Strings from SystemInfo
  rsVersionLongStr = '%s%s (Version %d.%d, Build %d, %6:s)';
  rsSPVersionLongStr = '%s%s - Service Pack %5:d (Version %2:d.%3:d, Build %4:d, %6:s)';
  rsVersion10LongStr = '%s%s (Version %s, Build %d, %s)';
  rsVersionShortStr = '%s (%2:s)';
  rsSPVersionShortStr = '%s - Service Pack %1:d (%2:s)';
  rsVersion10ShortStr = '%s (Version %s, %s)';
  rsVersion10Str = '%s (Version %d, Build %d, %s)';
  rsWindows11 = 'Windows 11';

  rsProfessionalN = 'Professional';
  rsProfessionalMC = 'Professional with Media Center';
  rsBusiness = 'Business';
  rsClusterServer = 'Cluster Server';
  rsDataCenterFull = 'Server Datacenter (full)';
  rsDataCenterCode = 'Server Datacenter (core)';
  rsEnterprise = 'Enterprise';
  rsEnterpriseServerF = 'Server Enterprise (full)';
  rsEnterpriseServerC = 'Server Enterprise (core)';
  rsEnterpriseServerIT = 'Server Enterprise for Itanium-based Systems';
  rsHomeBasic = 'Home Basic';
  rsHomePremium = 'Home Premium';
  rsHomePremiumS = 'Home Premium Server';
  rsHomeServer = 'Home Server';
  rsHyperV = 'Hyper-V Server';
  rsEssBusinessMan = 'Essential Business Management Server';
  rsEssBusinessSec = 'Essential Business Security Server';
  rsEssBusinessMess = 'Essential Business Messaging Server';
  rsEssServer = 'Essential Server';
  rsEssServerV = 'Essential Server without Hyper-V';
  rsServerFound = 'Server Foundation';
  rsSmallBusiness = 'Small Business Server';
  rsSmallBusinessP = 'Small Business Server Premium';
  rsStandardServer = 'Server Standard Edition (full)';
  rsStandardServerC = 'Server Standard Edition (core)';
  rsStarter = 'Starter Edition';
  rsStgEnterprise = 'Storage Server Enterprise';
  rsStgExpress = 'Storage Server Express';
  rsStgStandard = 'Storage Server Standard';
  rsStgWorkgroup = 'Storage Server Workgroup';
  rsUltimate = 'Ultimate Edition';
  rsWebServer = 'Web Server Edition';
  rsWebServerC = 'Web Server Edition (core)';
  rsUnlicensed = 'Unlicensed product';

{ ------------------------------------------------------------------- }
// Misc.
  rsVersion = 'Version';
  rsLongCopyRight = '%s Dr. J. Rathlev'+sLineBreak+' 24222 Schwentinental, Germany';
  rsCopyRight = '%s Dr. J. Rathlev';
  rsNotAvail = 'not available';
  rsConnectTo = 'Connect to ..';
  rsUnknown = '<Unknown>';
  rsAnyKey = 'Strike any key to continue ...';

implementation

end.
