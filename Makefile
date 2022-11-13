#
# Compiler directives
#

## Destination directories
## Final directory destination for executable
#EXEC_DIR=/home/providence/rochford/f90/crap
## Final directory destination for man page
#MAN_DIR=/home/providence/rochford/f90/crap

PROGRAM := f2f90

$(PROGRAM):
	gfortran -std=f2003 f2f90.f90 -o $(PROGRAM)

clean:
	rm -fv *.mod $(PROGRAM)

#install:
#	cp $(PROGRAM)		$(EXEC_DIR)
#	chmod 775		$(EXEC_DIR)/$(PROGRAM)
#	chgrp $(GROUP)          $(EXEC_DIR)/$(PROGRAM)

#	cp $(PROGRAM).man	$(MAN_DIR)/$(PROGRAM).man
#	chmod 664		$(MAN_DIR)/$(PROGRAM).man
#	chgrp $(GROUP)          $(MAN_DIR)/$(PROGRAM).man
