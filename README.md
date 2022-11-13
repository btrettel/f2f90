README for f2f90: Version 1.6.1, Thu Aug 29, 1997
Author: Peter Rochford, Sverdrup Technology, Inc.

DESCRIPTION:

A utility to convert FORTRAN 77 fixed source form to FORTRAN 90 free 
source form.


CONTENTS:

A Unix compatible version of f2f90 is contained in this directory. The
program will run on non-Unix platforms, but requires setting the LGETARG
logical variable to false in the INPUT subroutine of the main program 
(f2f90.f90).

The files are as follows:

Makefile		Unix makefile
README			This documentation file
f2f90.f90		Fortran 90 conversion program
f2f90.man		man page for f2f90


INSTALLATION:

Edit the makefile for the appropriate compiler options for your platform,
and the destination directories.

Set the LGETARG variable in the INPUT subroutine of f2f90.f90
according to your preference.

To create the executable: make f2f90

To install the executable: make install

To clean up the directory: make clean


MACHINE PLATFORMS: 

The program has been tested on the following architectures.

Cray (8/8/97), Peter Rochford, Sverdrup Technology, Inc.
Sun  (8/14/97), Peter Rochford, Sverdrup Technology, Inc.

MAN PAGES:

If you make additions to the nroff man pages, you can test for the appearance
of the changes by entering the command

nroff -man f2f90.man | more

