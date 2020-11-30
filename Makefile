# This is a comment?

override COMPILER_SOURCES := $(ls Compiler/**/.fs)
override COMPILER_PROJECT_FILE=Compiler/Parser.fsproj
COMPILER_CONFIGURATION=Release
COMPILER_EXE=Compiler/bin/${COMPILER_CONFIGURATION}/netcoreapp3.1/Parser

${COMPILER_EXE}: ${COMPILER_SOURCES} ${COMPILER_PROJECT_FILE}
	@echo "Building compiler"
	dotnet build ${COMPILER_PROJECT_FILE} -c ${COMPILER_CONFIGURATION} -nologo

install: ${COMPILER_EXE}
	mkdir -p target
	cp ${COMPILER_EXE} target/chakra

.PHONY:

clean: .PHONY
	rm ${COMPILER_EXE}