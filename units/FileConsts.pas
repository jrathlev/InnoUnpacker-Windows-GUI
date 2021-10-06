(* Resource strings for FileUtils and ExtFileTools

   © Dr. J. Rathlev, D-24222 Schwentinental (kontakt(a)rathlev-home.de)

   The contents of this file may be used under the terms of the
   Mozilla Public License ("MPL") or
   GNU Lesser General Public License Version 2 or later (the "LGPL")

   Software distributed under this License is distributed on an "AS IS" basis,
   WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
   the specific language governing rights and limitations under the License.

   Vers. 1.0 - Jan. 2016
   last modified: Dec. 2017
   *)

unit FileConsts;

interface

resourcestring
  rsErrOpening   = 'Error opening "%s"';
  rsErrCreating  = 'Error creating "%s"';
  rsErrReading   = 'Error reading from "%s"';
  rsExtractErr   = 'Error extracting from "%s"';
  rsErrWriting   = 'Error writing to "%s"';
  rsErrVerify    = 'Error verifying "%s"';
  rsErrClosing   = 'Error closing "%s"';
  rsErrSetAttr   = 'Error setting attributes of "%s"';
  rsErrTimeStamp = 'Error setting timestamps of "%s"';
  rsErrAcl       = 'Error setting permissions of "%s"';
  rsErrDirEntry  = 'Error writing directory entry: "%s"';
  rsErrAddDir    = 'Error adding directory to "%s"';
  rsErrAddFile   = 'Error adding file to "%s"';
  rsErrAddStream = 'Error adding stream to "%s"';
  rsErrCompFile  = 'Error comparing file from "%s" to "%s"';
  rsErrEndRecord = 'Error writing end record';
  rsErrNotFound  = 'File not found: "%s"';
  rsErrSystem    = 'System error: %s';

  rsNoError      = 'No errors';
  rsError        = 'Error - ';
  rsError2       = 'Error(2) - ';
  rsWarning      = 'Warning - ';
  rsInfo         = 'Info - ';
  rsFileCreate   = 'Could not create file';
  rsFileOpen     = 'Could not open file';
  rsFileClose    = 'Could not close file';
  rsFileRead     = 'Could not read from file';
  rsFileWrite    = 'Could not write to file';
  rsFileAttr     = 'Attributes could not be set';
  rsFileFull     = 'Low disk space';
  rsFileGZip     = 'Illegal file header';
  rsFileCheck    = 'Corrupt file';
  rsFileECrypt   = 'Encryption failed';
  rsFileDCrypt   = 'Decryption failed';
  rsFileVerify   = 'Verification failed';
  rsLongPath     = 'Path too long';
  rsNotFound     = 'File not found';
  rsFileTS       = 'Timestamp could not be set';
  rsStorage      = 'Error copying document summary';
  rsTimeout      = 'Timeout error on copying file';
  rsStream       = 'Undefined stream';
  rsAcl          = 'Permissions could not be copied';
  rsFileExists   = 'File already exists';
  rsSzMismatch   = 'Size mismatch';
  rsVerOpen      = 'Could not open destination file for verification';
  rsDirCreate    = 'Could not create directory';
  rsFtpRead      = 'Could not read from FTP';
  rsFtpWrite     = 'Could not write to FTP';
  rsFtpConnect   = 'Could not connect to FTP server';
  rsFtpBroken    = 'FTP connection was closed by server';
  rsFtpDatConn   = 'Could not open FTP data connection';
  rsFtpTimeout   = 'Timeout error on copying file via FTP';
  rsCompare      = 'Different content';
  rsZipCrSeg     = 'Error creating new zip segment';
  rsSignature    = 'Illegal zip signature';
  rsExtract      = 'Could not extract file';
  rsFormat       = 'Unsupported file format';
  rsTmpFile      = 'Could not rename temporary file';
  rsZipRdSeg     = 'Error reading next zip segment';
  rsAltStreams   = 'Error copying alternate data streams';
  rsFileDel      = 'Error deleting file';
  rsFileRen      = 'Error renaming file';

  rsUserBreak    = 'Terminated by user';
  rsUnknownErrCode  = 'Unknown error code ($%.8x)';
  rsCopy         = ' (Copy)';
  rsGZip         = ' (gzip)';
  rsGUnzip       = ' (gunzip)';
  rsZip          = ' (Zip)';
  rsUnzip        = ' (Unzip)';
  rsEnCrypt      = ' (Encrypt)';
  rsDeCrypt      = ' (Decrypt)';

  rsNoFileInfo   = 'File information not available';
  rsDescription  = 'Description: ';
  rsCompany      = 'Company: ';
  rsCopyright    = 'Copyright: ';
  rsVersion      = 'File version: ';
  rsFileDate     = 'Last modified: ';

  rsStrFormatError  = 'Format error: ';

implementation

end.
