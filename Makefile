ifeq (${DOTNET_VERSION},) 
	DOTNET_VERSION=net60
endif
ifeq (${COMPILER_CONFIGURATION},) 
	COMPILER_CONFIGURATION=Release
endif
ifeq (${RID},) 
	RID=osx.12-arm64
endif
ifeq (${DEST},) 
	DEST=/usr/local/bin
endif

COMPILER_PROJECT_FILE=Compiler/Parser.fsproj
TEST_PROJECT_FILE=Compiler/Tests/Tests.fsproj
COMPILER_SOURCES=$(shell ls ./Compiler/*.fs)
COMPILER_EXE=Compiler/bin/${COMPILER_CONFIGURATION}/${DOTNET_VERSION}/${RID}/publish/Parser
INSTALL_EXE=${DEST}/chakra

EXECUTABLE=target/chakra

RUNTIME_SOURCES := Runtime/actors.c
RUNTIME_SOURCES += Runtime/actors.h
RUNTIME_SOURCES += Runtime/channel.c
RUNTIME_SOURCES += Runtime/channel.h
RUNTIME_SOURCES += Runtime/child.c
RUNTIME_SOURCES += Runtime/child.h
RUNTIME_SOURCES += Runtime/main.c
RUNTIME_SOURCES += Runtime/main.h
RUNTIME_SOURCES += Runtime/process.c
RUNTIME_SOURCES += Runtime/process.h
RUNTIME_SOURCES += Runtime/recycler.c
RUNTIME_SOURCES += Runtime/run_table.c
RUNTIME_SOURCES += Runtime/run_table.h
RUNTIME_SOURCES += Runtime/runtime.c
RUNTIME_SOURCES += Runtime/sched.c
RUNTIME_SOURCES += Runtime/sched.h
RUNTIME_SOURCES += Runtime/stdlib.h
RUNTIME_SOURCES += Runtime/stdlib/actors.c
RUNTIME_SOURCES += Runtime/stdlib/format.c
RUNTIME_SOURCES += Runtime/stdlib/io.c
RUNTIME_SOURCES += Runtime/stdlib/list.h
RUNTIME_SOURCES += Runtime/stdlib/math.c
RUNTIME_SOURCES += Runtime/stdlib/string.c
RUNTIME_SOURCES += Runtime/stdlib/timing.c

RUNTIME_ARTIFACTS=$(shell ls ./Runtime/*.{o,a,s,ll,out,h.gch})
RUNTIME_EXECUTABLE=Runtime/runtime.o

all: target ${EXECUTABLE}
	@echo "* Building chakra executable and runtime"

target:
	@echo "* Creating target directory"
	mkdir -p target

${EXECUTABLE}: ${COMPILER_EXE}
	@echo "* Moving" ${EXECUTABLE}
	mkdir -p target
	cp ${COMPILER_EXE} ${EXECUTABLE}

${COMPILER_EXE}: ${COMPILER_SOURCES} ${COMPILER_PROJECT_FILE}
	@echo "* Building" $@
	dotnet publish ${COMPILER_PROJECT_FILE} -r ${RID} -c ${COMPILER_CONFIGURATION} -nologo -p:PublishSingleFile=true --self-contained -p:PublishReadyToRun=true

.PHONY:

compiler_tests: .PHONY
	dotnet test ${TEST_PROJECT_FILE} -r ${RID} -nologo

clean: .PHONY

install: ${INSTALL_EXE}