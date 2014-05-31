import System.Environment   
import System.Directory  
import System.IO  
import Data.List 
import Control.Exception

dispatch :: String -> [String] -> IO ()
dispatch "bump" = bump

main = do
    (command:argList) <- getArgs
    dispatch command argList

bump :: [String] -> IO ()
bump [fileName, numberString] = do
    contents <- readFile fileName
    let todoTasks = lines contents
        numberedTasks = zipWith (\n line -> show n ++ " - " ++ line) [0..] todoTasks
    putStrLn "These are your TO-DO items:"
    mapM_ putStrLn numberedTasks
    let number = read numberString
        targetLine = (todoTasks !! number)
        deletedTasks = delete targetLine todoTasks
        addedTasks = targetLine:deletedTasks
        newTodoItems = unlines $ addedTasks
        newNumberedTasks = zipWith (\n line -> show n ++ " - " ++ line) [0..] addedTasks
    putStrLn "These are bumped TO-DO items:"
    mapM_ putStrLn newNumberedTasks
    bracketOnError (openTempFile "." "temp")
        (\(tempName, tempHandle) -> do
            hClose tempHandle
            removeFile tempName)
        (\(tempName, tempHandle) -> do
            hPutStr tempHandle newTodoItems
            hClose tempHandle
            removeFile "todo.txt"
            renameFile tempName "todo.txt")