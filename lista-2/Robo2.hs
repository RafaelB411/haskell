data Command = Forward Int | Backward Int | TurnLeft | TurnRight
               deriving (Eq, Show, Read)

data Direction = North | South | West | East
                 deriving (Read, Show)

execOperation :: Direction -> Command -> Direction
-- execute the commands given
execOperation dir (Forward _) = dir
execOperation dir (Backward _) = dir
execOperation dir TurnLeft = case dir of
                             North -> West
                             West -> South
                             South -> East
                             East -> North
execOperation dir TurnRight = case dir of
                              North -> East
                              East -> South
                              South -> West
                              West -> North

faces :: Direction -> [Command] -> Direction
-- return the direction faced after executing the given commands
faces dir [] = dir
faces dir (cmd:cmds) = faces newDir cmds
    where newDir = execOperation dir cmd 

main = do 
       a <- getLine
       b <- getLine
       let result = faces (read a) (read b)
       print result
