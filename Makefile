
include make.inc

.DEFAULT:
	-echo $@ does not exist.
all: rbf
GIT_REV := $(shell git rev-parse --short HEAD 2>/dev/null || echo "0")
buildinfo.f90:
	echo "module buildinfo"                               > buildinfo.f90
	date | sed 's/^/  character(len=80), parameter :: build_time = \"/; s/$$/\"/'     >> buildinfo.f90
	uname -sr | sed 's/^/  character(len=80), parameter :: build_os   = \"/; s/$$/\"/' >> buildinfo.f90
	echo '  character(len=80), parameter :: build_dir  = "$(GIT_REV)"' >> buildinfo.f90
	echo "end module buildinfo"                           >> buildinfo.f90
buildinfo.o: ./buildinfo.f90
	$(FC) $(FFLAGS) $(OTHERFLAGS) -c	./buildinfo.f90
get_dims.o: ./get_dims.f90 utils.o string_utils.o
	$(FC) $(FFLAGS) $(OTHERFLAGS) -c	./get_dims.f90
kdtree2.o: ./kdtree2.f90
	$(FC) $(FFLAGS) $(OTHERFLAGS) -c	./kdtree2.f90
kinddefs.o: ./kinddefs.f90
	$(FC) $(FFLAGS) $(OTHERFLAGS) -c	./kinddefs.f90
main.o: ./main.F90 kdtree2.o options.o get_dims.o kinddefs.o utils.o rbf.o buildinfo.o tec_types.o tecplot.o string_utils.o
	$(FC) $(CPPDEFS) $(CPPFLAGS) $(FPPFLAGS) $(FFLAGS) $(OTHERFLAGS) -c	./main.F90
options.o: ./options.f90 kinddefs.o usage.o
	$(FC) $(FFLAGS) $(OTHERFLAGS) -c	./options.f90
phi.o: ./phi.f90 kinddefs.o
	$(FC) $(FFLAGS) $(OTHERFLAGS) -c	./phi.f90
rbf.o: ./rbf.F90 kinddefs.o phi.o svd.o
	$(FC) $(FFLAGS) $(OTHERFLAGS) -c	./rbf.F90
string_utils.o: ./string_utils.f90 kinddefs.o
	$(FC) $(FFLAGS) $(OTHERFLAGS) -c	./string_utils.f90
svd.o: ./svd.f90 kinddefs.o
	$(FC) $(FFLAGS) $(OTHERFLAGS) -c	./svd.f90
tec_types.o: ./tec_types.f90 kinddefs.o
	$(FC) $(FFLAGS) $(OTHERFLAGS) -c	./tec_types.f90
tecplot.o: ./tecplot.f90 kinddefs.o tec_types.o
	$(FC) $(FFLAGS) $(OTHERFLAGS) -c	./tecplot.f90
usage.o: ./usage.f90 buildinfo.o
	$(FC) $(FFLAGS) $(OTHERFLAGS) -c	./usage.f90
utils.o: ./utils.f90 kinddefs.o
	$(FC) $(FFLAGS) $(OTHERFLAGS) -c	./utils.f90
SRC = ./get_dims.f90 ./kdtree2.f90 ./utils.f90 ./phi.f90 ./main.F90 ./usage.f90 ./kinddefs.f90 ./tecplot.f90 ./options.f90 ./tec_types.f90 ./string_utils.f90 ./buildinfo.f90 ./svd.f90 ./rbf.F90
OBJ = get_dims.o kdtree2.o utils.o phi.o main.o usage.o kinddefs.o tecplot.o options.o  tec_types.o string_utils.o buildinfo.o svd.o rbf.o
clean: neat
	-rm -f .rbf.cppdefs $(OBJ) *.mod buildinfo.f90 rbf modules.log
neat:
	-rm -f $(TMPFILES)
TAGS: $(SRC)
	etags $(SRC)
tags: $(SRC)
	ctags $(SRC)
rbf: $(OBJ) 
	$(LD) $(OBJ) -o rbf  $(LDFLAGS)
