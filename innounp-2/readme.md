### Inno Setup Unpacker - Console version

#### Unpacker for installations made by Inno Setup

#### The version 2.71 supports Inno Setup versions 2.0.7 to 7.1.1

#### The binary of the previous [version 2.67](bin) is still available

##### This is a branch of the *innounp* project on [SourceForge](http://sourceforge.net/projects/innounp)

[Inno Setup](http://www.jrsoftware.org/isinfo.php) is a popular program
for making software installations. *Inno Setup Unpacker* is a console application
to verify and get files out of the self-extracting executable.

Since there have been no updates of the *SourceForge* project since 2020 and the 
source codes are not yet adapted to the current Delphi versions (Unicode strings are 
the native type since *Delphi 2009*), I have modified them so that they can be 
compiled with newer Delphi versions (tested with *Delphi 10 Seattle*).

At the same time, the program was expanded to support all *Inno Setup* versions from 6.2 onwards.

In addition, some errors have been fixed and new command line options (*-s*, *-l*, 
*-i*, *-h*, *-o*, *-w* , *-r* and *-u*)
were added, as well as exit codes to support the use in batch files. 

The install script is saved in UTF-8 format, optionally in default encoding.

GOG Galaxy installers are detected automatically from the structure of their file
entries (the `before_install`/`after_install` part metadata and the matching stored
part sizes), independent of the publisher information. If the metadata is
inconsistent, a warning is shown and the files are processed as stored. Files stored as compressed,
hashed parts are reconstructed under their original relative filenames, including
GOG dependency files. Part sizes and MD5 checksums, and the MD5 checksum of each
completed file, are verified before it is saved.

The `-s` and `-v` listings show the reconstructed names and sizes. Extraction with
`-x` or `-e`, integrity testing with `-t`, file masks, and `-cDIR` use those recovered
paths. GOG game files are written directly beneath the extraction directory,
with their original subdirectories. The reconstructed install script retains the
stored file entries and their callbacks.


[**Download version 2.67.11**](https://github.com/jrathlev/InnoUnpacker-Windows-GUI/tree/master/innounp-2/bin)

[**Download version 2.71.4**](https://github.com/jrathlev/InnoUnpacker-Windows-GUI/tree/master/innounp-2/bin)

[Home page](https://www.rathlev-home.de/index-e.html?tools/prog-e.html#unpack)

