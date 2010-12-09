open Microsoft.FSharp.Compiler 

val tcConfig      : string -> Build.tcConfig
val parseAst      : Build.tcConfig -> string list       -> Build.input list
//val initialTcEnv  : Build.tcConfig -> Env.TcGlobals * Build.tcImports 
//val typeCheckAsts : Build.tcConfig -> Env.TcGlobals -> Build.tcImports -> Build.input list  -> Tast.CcuThunk * (Tast.NonLocalPath * Tast.ModuleOrNamespace * Tast.decl list * Expr) list
