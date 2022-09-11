module CoreDump.Plugin
    ( plugin
    ) where
import GhcPlugins

plugin :: Plugin
plugin = defaultPlugin{
    installCoreToDos = install
}

-- test :: IO Int


install :: [CommandLineOption] -> [CoreToDo] -> CoreM [CoreToDo]
install _ todo = do
        putMsgS "Hello!"
        return todo
