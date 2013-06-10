include ./config.make 

INCL_DIR    = ./include

MAIN_SRC    = aladin2camx_MAIN.F90
MAIN_OBJ    = ${MAIN_SRC:.F90=.o}
MAIN_EXE    = ${MAIN_SRC:.F90=.exe}

SOURCES_F90 = run_info.F90 \
              juldate.F90 \
              get_h_p_t_wv.F90 \
              get_aladin_fields.F90 

SOURCES_f90 = ${SOURCES_F90:.F90=.f90} 
SOURCES_OBJ = ${SOURCES_F90:.F90=.o} 
INTER_FACES = ${SOURCES_F90:.F90=.ifc} 

MODULES_F90 = module_standard_types.F90 \
              module_physical_constants.F90 \
              module_global_variables.F90 \
              module_DateTime.F90 \
              module_smoothing_handler.F90 \
              module_meteo_functions.F90 \
              module_verthor.F90 \
              module_beismet.F90 \
              module_cloud_opt_depth.F90 \
              module_vertical_diffusivity.F90 \
              INTER_FACES.F90
MODULES_f90 = ${MODULES_F90:.F90=.f90} 
MODULES_OBJ = ${MODULES_F90:.F90=.o} 
MODULES_MOD = `echo ${MODULES_F90:.F90=.mod} | ${TR} 'A-Z' 'a-z'`

OBJECTS     = ${SOURCES_OBJ} ${MODULES_OBJ}

# = = = = = = = = = = = = = = = = = = = = = = = =  = = = = = = = = = = = = = = = =
.PHONY: all
all:: ${MAIN_EXE}
	cp ${MAIN_EXE} ../
#	${RM} ${OBJECTS} ${MAIN_OBJ}
#	${RM} ${MODULES_MOD}

${MAIN_EXE}:: ${OBJECTS} ${MAIN_OBJ} 

${MAIN_OBJ}:: ${MAIN_SRC} ${MODULES_OBJ}

${MODULES_OBJ}:: ${MODULES_F90} 

${SOURCES_OBJ}: ${SOURCES_F90} ${MODULES_F90}

INTER_FACES.F90: ${INTER_FACES}
	${RM} $@
	echo "! explicit F90 inferfaces subroutines" > $@
	echo "module Inter_Faces" >> $@
	for srcfile in ${SOURCES_F90} ; do \
	    echo "#include <"`${BASENAME} $$srcfile ".F90"`".ifc>" >> $@ ; \
	done
	echo "END module Inter_Faces" >> $@

.PHONY: install
install:
	echo "check examples/  for namelist and run examples"
	cp aladin2camx_MAIN.exe ../
	#cp aladin2camx.nml.sample ../aladin2camx.nml
	#cp RUN.sh.sample ../RUN.sh
	#cp INFO_GRID.sample ../INFO_GRID
	#cp INFO_RUN.nml.sample ../INFO_RUN.nml
	if ! [ -e ../.aladin2camx.counter ]; then echo 00001 > ../.aladin2camx.counter; fi

.PHONY: interfaces 
interfaces: ${INTER_FACES}

.PHONY: f90_sources
f90_sources: ${SOURCES_f90}

.PHONY: clean
clean:
	${RM} ${OBJECTS} ${MAIN_OBJ}

.PHONY: cleanall
cleanall: clean
	${RM} ${MODULES_MOD} ${MAIN_EXE} ${MAIN_f90} ${SOURCES_f90} ${MODULES_f90}
	${RM} INTER_FACES.F90 ${INTER_FACES} ${INCL_DIR}/*.ifc
	${RM} aladin2camx.*.end
#	${RM} *.log


