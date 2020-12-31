# This is a comment?

override COMPILER_SOURCES := $(ls Compiler/**/.fs)
override COMPILER_PROJECT_FILE=Compiler/Parser.fsproj
COMPILER_CONFIGURATION=Release
COMPILER_EXE=Compiler/bin/${COMPILER_CONFIGURATION}/netcoreapp3.1/Parser

override RUNTIME_SOURCES := $(ls Runtime/*.{c,h})
override RUNTIME_ARTIFACTS := $(ls Runtime/*.{o,a,s,ll,out})
override RUNTIME_EXECUTABLE=Runtime/a.out


${COMPILER_EXE}: ${COMPILER_SOURCES} ${COMPILER_PROJECT_FILE}
	@echo "Building compiler"
	dotnet build ${COMPILER_PROJECT_FILE} -c ${COMPILER_CONFIGURATION} -nologo

compiler_install: ${COMPILER_EXE}
	mkdir -p target
	cp ${COMPILER_EXE} target/chakra

.PHONY:

compiler_clean: .PHONY
	rm ${COMPILER_EXE}

runtime: ${RUNTIME_EXECUTABLE}
	@echo "BUILDING RUNTIME USING SOURCES: ${RUNTIME_SOURCES}"
	clang -gdwarf ${RUNTIME_SOURCES}

runtime_clean: .PHONY
	rm ${RUNTIME_ARTIFACTS}