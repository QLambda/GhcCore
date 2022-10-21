module CoreDump.Plugin
    ( plugin
    ) where

import GHC.Plugins
import GHC.Unit.Module.Deps
import GHC.Unit.State
import CoreDump.CoreExtract (coreToCProgram)

plugin :: Plugin
plugin = defaultPlugin{
    installCoreToDos = install
}

-- showPprUnsafe
-- showSDocUnsafe
-- pprDeps :: UnitState -> Dependencies -> SDoc
addBreacks::String -> String
addBreacks [] = [] 
addBreacks ('\\':'n':xs) = '\n':addBreacks xs
addBreacks (x:xs) = x:addBreacks xs


_showOuputable out = renderWithContext defaultSDocContext (ppr out)

dumpToFile filename content= do writeFile filename content

getModuleName mod = showPprUnsafe  $ mg_module  mod

coreDump::ModGuts -> CoreM ModGuts
coreDump mod = do 
                    let typesCtor = showSDocUnsafe $ ppr $ mg_tcs mod
                    let coreExpStr = show  (coreToCProgram  mod )
                    let coreExpStrNoType = show  (coreToCProgram  mod )
                    liftIO $ writeFile (getModuleName mod++".CoreTyConst") typesCtor
                    liftIO $ writeFile (getModuleName mod++".Core") coreExpStr
                    liftIO $ writeFile (getModuleName mod++".CoreUntyped") coreExpStrNoType
                    return mod



install :: [CommandLineOption] -> [CoreToDo] -> CoreM [CoreToDo]
install _ todo = do 
                    putMsgS "Installing CoreDump!"
                    return $ CoreDoPluginPass "CoreDump" coreDump:todo
