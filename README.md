# f2f90 version 1.6.1, 2022-11-13

Authors: Michael Metcalf, Peter A. Rochford, Ben Trettel

## Description

A utility to convert FORTRAN 77 fixed source form to FORTRAN 90 free source form.

## Contents

The files are as follows:

- Makefile: Unix makefile
- README.md: This documentation file
- f2f90.f90: Fortran 90 conversion program
- f2f90.man: man page for f2f90

## Installation

Edit the Makefile for the appropriate compiler options for your platform, and the destination directories.

To create the executable: `make f2f90`

To install the executable: `make install` (temporarily disabled)

To clean up the directory: `make clean`

# MAN pages

If you make additions to the nroff man pages, you can test for the appearance of the changes by entering the command

    nroff -man f2f90.man | more

