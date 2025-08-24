### Inno Setup Unpacker - Windows GUI v 2.0

#### Inspect and unpack InnoSetup archives

[Inno Setup](http://www.jrsoftware.org/isinfo.php) is a popular program
for making software installations. To verify and get files out of the self-extracting 
executable, these open source console applications are available:

- Original version [Innounp](http://sourceforge.net/projects/innounp) (can be used up to InnoSetup 6.1)
- Updated Unicode version [Innounp-2](innounp-2) (can be used up to InnoSetup 6.5.1)

**InnoUnpacker** is a graphical user interface (GUI) for these console applications
that makes the usage more comfortable.
The executable setup to be processed can be loaded via a file selection dialog, just 
by drag & drop, using the command line or by an entry in the Windows context menu for *exe* files. 
Immediately after opening, the basic file info of the setup is displayed. All other functions are activated by clicking on one of the buttons:

- General information about the setup
- A list of all included files 
- All supported languages 
- Verify setup file
- Start file extraction
- Select destination directory for extracted files

Optionally, you can specify whether only the installation script or the files
matching a filter are to be extracted.

**Note on execution as portable program:** 
The program settings are normally stored in the InnoUnpack.ini file in the 
user's *Application data* folder. If the program is started from a device connected 
via USB, the folder of the executable file of the program is used instead. 
The same applies if the program is started with the command line option "/p".


[Application download](https://www.rathlev-home.de/index-e.html?tools/prog-e.html#unpack)

