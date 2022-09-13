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
                    let dependencies = (_showOuputable  $ dep_direct_mods $ mg_deps mod ) ++ "\n\n" ++ (_showOuputable  $ dep_direct_pkgs $ mg_deps mod )
                    let coreExpStr = show  (coreToCProgram  mod )
                    let coreExpStrNoType = show  (coreToCProgram  mod )
                    let coreStr = typesCtor ++ "\n----\n" ++ dependencies ++ "\n--Typed--\n" ++ coreExpStr ++ "\n--No Typed--\n" ++ coreExpStrNoType
                   
                    liftIO $ print coreStr
                    liftIO $ writeFile (getModuleName mod++".Core") coreStr
                    -- liftIO $ print "--- mg_module mod ---"
                    -- liftIO $ print $ showPprUnsafe  $ mg_module  mod
                    -- liftIO $ print "--- mg_deps mod ---"
                    -- liftIO $ print $ showSDocUnsafe  $ pprDeps emptyUnitState $ mg_deps mod
                    -- liftIO $ print "--- mg_tcs mod Type Constructors---"
                    -- liftIO $ print  $ showSDocUnsafe $ ppr $ mg_tcs mod
                    -- liftIO $ print "--- mg_binds mod  Binds ---"
                    -- liftIO $ print  $ showSDocUnsafe $ ppr $ mg_binds mod
                    -- liftIO $ print "--- mg_rules mod  Rules ---"
                    -- liftIO $ print  $ showSDocUnsafe $ ppr $ mg_rules mod
                    -- liftIO $ print "--- mg_complete_matches mod  Complete Matches ---"
                    -- liftIO $ print  $ showSDocUnsafe $ ppr $ mg_complete_matches mod
                    return mod



install :: [CommandLineOption] -> [CoreToDo] -> CoreM [CoreToDo]
install _ todo = do 
                    putMsgS "Installing CoreDump!"
                    return $ CoreDoPluginPass "CoreDump" coreDump:todo
