#
# Compiler directives
#
# For Cray systems
#MACHINE=CRAY
#FPP_SWITCHES=-Wp"P" -D$(MACHINE)
#FPP=		fpp ${FPP_SWITCHES}

# For Sun systems
MACHINE=STANDARD
FPP_SWITCHES=-P -D$(MACHINE)
FPP=		fpp ${FPP_SWITCHES}

# For Other systems
#MACHINE=NULL
#FPP_SWITCHES=-P -D$(MACHINE)
#FPP=		fpp ${FPP_SWITCHES}

FORTRAN_SWITCHES=
F90_SWITCHES=
F90LINK=	$(FORTRAN90)
 
#  Compilers
FORTRAN=        f77 ${FORTRAN_SWITCHES}
FORTRAN90=	f90 ${F90_SWITCHES}

# Destination directories
# Final directory destination for executable
EXEC_DIR=/home/providence/rochford/f90/crap
# Final directory destination for man page
MAN_DIR=/home/providence/rochford/f90/crap

# Group assignments
GROUP=NRL7320

.SUFFIXES: .F .F90 .f .f90 .o

PROGRAM=f2f90
OBJS=$(PROGRAM).o get_argument.o

$(PROGRAM) : $(OBJS) ;
	$(F90LINK) -o $(PROGRAM) $(OBJS)

clean:
	@touch nosuch.o fort.x x.M x.T x.trace core $(PROGRAM) 
	@-rm *.o fort.* *.M *.T *.trace core $(PROGRAM)
	@sccs clean
	@echo "@(#)                                                    "
	@echo "@(#)    /*****************************************  "
	@echo "@(#)     *  $(PROGRAM) directory has been cleaned.  *  " 
	@echo "@(#)     *****************************************/ "

.F.o:
	$(FPP) $<  $*.f
	$(FORTRAN) -c $*.f
	rm $*.f

.f.o:
	$(FORTRAN) -c $<

.F90.o:
	$(FPP) $<  $*.f90
	$(FORTRAN90) -c $*.f90
	rm $*.f90

.f90.o:
	$(FORTRAN90) -c $<

install:
	cp $(PROGRAM)		$(EXEC_DIR)
	chmod 775		$(EXEC_DIR)/$(PROGRAM)
	chgrp $(GROUP)          $(EXEC_DIR)/$(PROGRAM)

	cp $(PROGRAM).man	$(MAN_DIR)/$(PROGRAM).man
	chmod 664		$(MAN_DIR)/$(PROGRAM).man
	chgrp $(GROUP)          $(MAN_DIR)/$(PROGRAM).man
