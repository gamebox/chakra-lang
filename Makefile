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
BLAH=$(shell echo Runtime/**/*.{c, h})
COMPILER_PROJECT_FILE=Compiler/Parser.fsproj
TEST_PROJECT_FILE=Compiler/Tests/Tests.fsproj
COMPILER_SOURCES=$(shell ls ./Compiler/*.fs)
COMPILER_EXE=Compiler/bin/${COMPILER_CONFIGURATION}/${DOTNET_VERSION}/${RID}/publish/Parser
INSTALL_EXE=${DEST}/chakra

EXECUTABLE=target/chakra

RUNTIME_SOURCES=$(shell ls ./Runtime/**/*.{c,h})
RUNTIME_ARTIFACTS=$(shell ls ./Runtime/*.{o,a,s,ll,out,h.gch})
RUNTIME_EXECUTABLE=Runtime/runtime.o

all: target ${EXECUTABLE} ${RUNTIME_EXECUTABLE}
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

clean: .PHONY
	rm -f ${COMPILER_EXE} ${RUNTIME_ARTIFACTS} ${RUNTIME_EXECUTABLE} ${EXECUTABLE}

${RUNTIME_EXECUTABLE}: ${RUNTIME_SOURCES}
	@echo "* Building" $@
	clang -c -gdwarf ${RUNTIME_SOURCES}

install: ${INSTALL_EXE}

${INSTALL_EXE}: ${EXECUTABLE}
	cp ${EXECUTABLE} ${INSTALL_EXE}

compiler_tests: .PHONY
	dotnet test ${TEST_PROJECT_FILE} -r ${RID} -nologo
