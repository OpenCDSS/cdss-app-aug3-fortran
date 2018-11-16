# cdss-app-aug3-fortran

AUG3 is a collection of computer applications within Colorado's Decision Support Systems (CDSS) created to calculate and report stream dpeleitons from well pmping in the Denver Basin aquifers.  Note that this collection of applications was intitated in 1985 and the origin of the numbers in the names of any names or files is unknown but is not related to versioning (i.e. AUG3 is not the third version of the program)
CDSS open source software are referred to as being part of OpenCDSS.

* [Background](#background)
* [Repository Contents](#repository-contents)
* [Development Environment](#development-environment)
* [License](#license)
* [Contributing](#contributing)

-----------

## Background ## 

AUG3 consists of four EXCEL interfaces, 17 groundwater models and four fortran programs.  More detailed information and a link to download the applications are available on the [AUG3 Denver Basin Model webpage](http://water.state.co.us/DataMaps/ModelingCDSS/Pages/Aug3.aspx). 
This repository is only for the fortran programs:

1. `aug4.for`:  Pre-processor that generates input files for MODFLOW program
2. `aug4_db.for`:  Pre-processor that generates input files for MODFLOW program run only in locations within the Designated Basins 
3. `dry.for`: Post-processor that looks for and reports any dry cells in the MODFLOW output file
4. `GS3DRIVE.for`: 1983 version of MODFLOW revised in 1985, 1986 and 1990 by CDWR staff to provide stream depletion data file. This program has not been modified in over 27 years and is believed to have been compiled using lahey.  This repository is used only to archive this program.  Future work is needed to make the program compatible with gfortran.

### 2017 improvement to `aug4.for` and `dry.for`

A small project for Colorado Division of Water Resources (DWR) rewrote two aug3 gwbasic scripts (`dry.bas`, `aug4.bas`), converting from GWBASIC into Fortran.
The previous DWR aug3 system requires gwbasic to run scripts like `aug4.bas` and `dry.bas`.
However, gwbasic is an old 16 bit Windows application and is problematic on modern windows computers.
Rewriting `aug4.bas` and `dry.bas` in a compilable computer language familiar to DWR staff allows aug3 to be installed without needing gwbasic.
The Fortran language was chosen because it is familiar to DWR staff,
similar to basic in form, and still a relevant modern computer language with compiler implementations like GNU's gfortran.
The versiom for `aug4.for` for this improvement is 0.81.

### 2018 revision to `aug4.for` creating `aug4_db.for`

`aug4.for` was revised to generate input files for areas in the Laramie-Fox Hills and Arapahoe Aquifers in the Designated Basins.
In 2018 the model files for these aquifers within the Designated Basins were revised because of new geology.
It was also found that these revisions required that the pre-processor needed to expand the area used to generate input information.
The verison was renamed `aug4_db.for` and was given version number 0.91 and this code should only be used for the Designated Basin Excel program.
All other versions of the Excel Program should apply `aug4.for` version 0.81.

## Repository Contents

The repository contains the following major folders and files:

```
build-util/                       Utility scripts to help developers.
doc/                              Software documentation.
doc-doxygen-project/              Doxygen project to auto-generate code documentation.
LICENSE.md                        License file for the software.
README.md                         This file.
resources/                        Other resources used by the software project.
src/                              Source code.
  basic/                          Legacy Basic source code.
    *.BAS                         Legacy Basic source files.
  fortran/                        Current Fortran source code.
    *.for                         Fortran source code.
    *.inc                         Fortran include files.
    makefile                      Current makefile to compile software (use instead of the following).
    makefile_linux                Legacy makefile for Linux (can be phased out).
    makefile_msdos                Legacy makefile for MS-DOS (same as Linux, can be phased out).
test/                             Files used to test the software.
```

The following folder structure is recommended for top-level folders to organize OpenCDSS software projects
and is consistent with other OpenCDSS projects:

```
C:\Users\UserName\                Home folder for user's files on Windows (Linux folder style shown below).
/home/user/                       Home folder for user's files on Linux.
  cdss-dev/                       CDSS development folder for all CDSS projects.
    AUG3/                         AUG3 development files.
      git-repos/                  Folder for Git repositories that comprise AUG3.
        cdss-app-aug3-fortran/    Folder for Git repository.

```

## Development Environment

The software has been tested using MINGW 32-bit gfortran version 5.3.0, 2015.
The development environment is consistent with the OpenCDSS StateMod development environment.
[Development environment documentation](doc/Ref10_MinGW2017.pdf) is also available from a previous project.

To compile the software:

1. Open a Windows command prompt window.
2. Run the `build-util/setup-mingw-env.bat` script to configure the MinGW environment,
mainly to adjust the path to find the MINGW compiler.
This step needs to be done once per command prompt window.
3. Change to the `src/fortran` folder.
4. Compile using the `make` command:
   1. `make clean` - to remove old software
   2. `make` - to compile the software

Perform other development tasks as appropriate.
For example, use a Git Bash window to run Git command line tools.

## License

The software is licensed under GPL v3+.  See the [LICENSE.md](LICENSE.md) file.

## Contributing

Contributions to the software should occur using one of the following methods:

1. Those with commit privileges can make changes to the software by normal development team protocols.
2. Submit suggestions via the GitHub issues.
3. Fork the repository, commit changes to the fork, and do a pull request.
