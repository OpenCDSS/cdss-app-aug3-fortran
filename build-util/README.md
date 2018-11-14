# build-util #

This folder contains useful scripts for software developers, typically run from Git Bash.
The scripts can be run from this folder or another folder (by specifying the path to the script).

* `git-check-aug3.sh` - checks the local and remote Git repository status and indicates
whether pull, push, etc. are needed
* `git-util/` - folder containing generalized Git utilities from the Open Water Foundation
[owf-util-git](https://github.com/OpenWaterFoundation/owf-util-git) repository
* `product-repo-list.txt` - list of repositories for this product, used by Git utilities
* `run-doxygen.sh` - script to run Doxygen software, which auto-generates code documentation in
the `doc-doxygen-project` folder
* `setup-mingw-env.bat` - batch file to configure MINGW `gfortran` compiler environment,
assuming that MINGW `gfortran` has been correctly installed as per the main README

