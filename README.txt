This directory contains the source for
the Zurich Pascal P4 compiler. The files are:

int.p		- P4 code interpreter
comp0.p		- original compiler as distributed
comp1.p		- same compiler with bugs fixed up to Pascal Newsletter #12
comp2.p		- as above but with experimental changes
		to eliminate the last of the machine dependent code
comp.p		- lower case, entabbed and stripped version of comp2.p
		some typos in comp?.p have been fixed
compvax.p	- essentially the same as comp.p but with changes to allow
		it to compile under 'pc'
compdiffs	- differences between compvax.p and comp.p
compvax.errs	- warning messages
compvax.map	- line numbers of procedures and functions

Note that the characters for ^ and ' are ' and # in the compiler sources.
