#load "Graph.fs"
#load "Codegen.fs"
#load "CConsole.fs"
#load "ParserLibrary.fs"
#load "AST.fs"
#load "TypeSystem.fs"
#load "TypedAST.fs"
#load "ChakraParser.fs"
#load "Pretty.fs"
#load "Env.fs"
#load "TypeGraph.fs"
#load "Annotate.fs"
#load "Llvm.fs"
#load "IRState.fs"
#load "Generate.fs"
#load "Build.fs"

Build.build (Some "./Examples/test")
