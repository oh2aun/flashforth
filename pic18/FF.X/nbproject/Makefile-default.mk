#
# Generated Makefile - do not edit!
#
# Edit the Makefile in the project folder instead (../Makefile). Each target
# has a -pre and a -post target defined where you can add customized code.
#
# This makefile implements configuration specific macros and targets.


# Include project Makefile
ifeq "${IGNORE_LOCAL}" "TRUE"
# do not include local makefile. User is passing all local related variables already
else
include Makefile
# Include makefile containing local settings
ifeq "$(wildcard nbproject/Makefile-local-default.mk)" "nbproject/Makefile-local-default.mk"
include nbproject/Makefile-local-default.mk
endif
endif

# Environment
MKDIR=mkdir -p
RM=rm -f 
MV=mv 
CP=cp 

# Macros
CND_CONF=default
ifeq ($(TYPE_IMAGE), DEBUG_RUN)
IMAGE_TYPE=debug
OUTPUT_SUFFIX=hex
DEBUGGABLE_SUFFIX=elf
FINAL_IMAGE=${DISTDIR}/FF.X.${IMAGE_TYPE}.${OUTPUT_SUFFIX}
else
IMAGE_TYPE=production
OUTPUT_SUFFIX=hex
DEBUGGABLE_SUFFIX=elf
FINAL_IMAGE=${DISTDIR}/FF.X.${IMAGE_TYPE}.${OUTPUT_SUFFIX}
endif

ifeq ($(COMPARE_BUILD), true)
COMPARISON_BUILD=
else
COMPARISON_BUILD=
endif

ifdef SUB_IMAGE_ADDRESS

else
SUB_IMAGE_ADDRESS_COMMAND=
endif

# Object Directory
OBJECTDIR=build/${CND_CONF}/${IMAGE_TYPE}

# Distribution Directory
DISTDIR=dist/${CND_CONF}/${IMAGE_TYPE}

# Source Files Quoted if spaced
SOURCEFILES_QUOTED_IF_SPACED=../src/ff-pic18.S

# Object Files Quoted if spaced
OBJECTFILES_QUOTED_IF_SPACED=${OBJECTDIR}/_ext/1360937237/ff-pic18.o
POSSIBLE_DEPFILES=${OBJECTDIR}/_ext/1360937237/ff-pic18.o.d

# Object Files
OBJECTFILES=${OBJECTDIR}/_ext/1360937237/ff-pic18.o

# Source Files
SOURCEFILES=../src/ff-pic18.S



CFLAGS=
ASFLAGS=
LDLIBSOPTIONS=

############# Tool locations ##########################################
# If you copy a project from one host to another, the path where the  #
# compiler is installed may be different.                             #
# If you open this project with MPLAB X in the new host, this         #
# makefile will be regenerated and the paths will be corrected.       #
#######################################################################
# fixDeps replaces a bunch of sed/cat/printf statements that slow down the build
FIXDEPS=fixDeps

.build-conf:  ${BUILD_SUBPROJECTS}
ifneq ($(INFORMATION_MESSAGE), )
	@echo $(INFORMATION_MESSAGE)
endif
	${MAKE}  -f nbproject/Makefile-default.mk ${DISTDIR}/FF.X.${IMAGE_TYPE}.${OUTPUT_SUFFIX}

MP_PROCESSOR_OPTION=PIC18F26K42
FINAL_IMAGE_NAME_MINUS_EXTENSION=${DISTDIR}/FF.X.${IMAGE_TYPE}
# ------------------------------------------------------------------------------------
# Rules for buildStep: pic-as-assembler
ifeq ($(TYPE_IMAGE), DEBUG_RUN)
${OBJECTDIR}/_ext/1360937237/ff-pic18.o: ../src/ff-pic18.S  nbproject/Makefile-${CND_CONF}.mk 
	@${MKDIR} "${OBJECTDIR}/_ext/1360937237" 
	@${RM} ${OBJECTDIR}/_ext/1360937237/ff-pic18.o 
	${MP_AS} -mcpu=PIC18F26K42 -c \
	-o ${OBJECTDIR}/_ext/1360937237/ff-pic18.o \
	../src/ff-pic18.S \
	 -D__DEBUG=1  -misa=std -msummary=+mem,+psect,+class,-hex,-file,-sha1,-sha256,-xml,-xmlfull -v -fmax-errors=20 -mwarn=0 -xassembler-with-cpp -Wa,-a -Wl,-pudata_acs=000h,-pudatabig=050h,-presetVec=0h,-phi_int=8h,-plo_int=18h
	
else
${OBJECTDIR}/_ext/1360937237/ff-pic18.o: ../src/ff-pic18.S  nbproject/Makefile-${CND_CONF}.mk 
	@${MKDIR} "${OBJECTDIR}/_ext/1360937237" 
	@${RM} ${OBJECTDIR}/_ext/1360937237/ff-pic18.o 
	${MP_AS} -mcpu=PIC18F26K42 -c \
	-o ${OBJECTDIR}/_ext/1360937237/ff-pic18.o \
	../src/ff-pic18.S \
	 -misa=std -msummary=+mem,+psect,+class,-hex,-file,-sha1,-sha256,-xml,-xmlfull -v -fmax-errors=20 -mwarn=0 -xassembler-with-cpp -Wa,-a -Wl,-pudata_acs=000h,-pudatabig=050h,-presetVec=0h,-phi_int=8h,-plo_int=18h
	
endif

# ------------------------------------------------------------------------------------
# Rules for buildStep: pic-as-linker
ifeq ($(TYPE_IMAGE), DEBUG_RUN)
${DISTDIR}/FF.X.${IMAGE_TYPE}.${OUTPUT_SUFFIX}: ${OBJECTFILES}  nbproject/Makefile-${CND_CONF}.mk    
	@${MKDIR} ${DISTDIR} 
	${MP_LD} -mcpu=PIC18F26K42 ${OBJECTFILES_QUOTED_IF_SPACED} \
	-o ${DISTDIR}/FF.X.${IMAGE_TYPE}.${OUTPUT_SUFFIX} \
	 -D__DEBUG=1  -misa=std -msummary=+mem,+psect,+class,-hex,-file,-sha1,-sha256,-xml,-xmlfull -mcallgraph=std -Wl,-Map=${FINAL_IMAGE_NAME_MINUS_EXTENSION}.map -mno-download-hex -Wa,-a -Wl,-pudata_acs=000h,-pudatabig=050h,-presetVec=0h,-phi_int=8h,-plo_int=18h
else
${DISTDIR}/FF.X.${IMAGE_TYPE}.${OUTPUT_SUFFIX}: ${OBJECTFILES}  nbproject/Makefile-${CND_CONF}.mk   
	@${MKDIR} ${DISTDIR} 
	${MP_LD} -mcpu=PIC18F26K42 ${OBJECTFILES_QUOTED_IF_SPACED} \
	-o ${DISTDIR}/FF.X.${IMAGE_TYPE}.${OUTPUT_SUFFIX} \
	 -misa=std -msummary=+mem,+psect,+class,-hex,-file,-sha1,-sha256,-xml,-xmlfull -mcallgraph=std -Wl,-Map=${FINAL_IMAGE_NAME_MINUS_EXTENSION}.map -mno-download-hex -Wa,-a -Wl,-pudata_acs=000h,-pudatabig=050h,-presetVec=0h,-phi_int=8h,-plo_int=18h
endif


# Subprojects
.build-subprojects:


# Subprojects
.clean-subprojects:

# Clean Targets
.clean-conf: ${CLEAN_SUBPROJECTS}
	${RM} -r ${OBJECTDIR}
	${RM} -r ${DISTDIR}

# Enable dependency checking
.dep.inc: .depcheck-impl

DEPFILES=$(shell "${PATH_TO_IDE_BIN}"mplabwildcard ${POSSIBLE_DEPFILES})
ifneq (${DEPFILES},)
include ${DEPFILES}
endif
