module CoreDump.Plugin
    ( plugin
    ) where

import GHC.Plugins
import CoreDump.CoreExtract (coreToCProgram)

plugin :: Plugin
plugin = defaultPlugin{
    installCoreToDos = install
}

coreDump::ModGuts -> CoreM ModGuts
coreDump mod = do 
                    liftIO $ print $ coreToCProgram  mod
                    return mod



install :: [CommandLineOption] -> [CoreToDo] -> CoreM [CoreToDo]
install _ todo = do 
                    putMsgS "Installing CoreDump!"
                    return $ [(CoreDoPluginPass "CoreDump" coreDump)]
