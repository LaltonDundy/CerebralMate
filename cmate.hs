import Data.Char
import Data.List.Zipper
import System.Environment

data Tokes = Nada
    | MoveRight | Inc        | Dec      | Output
    | GetByte   | StartWhile | EndWhile | MoveLeft

parse :: String -> [ Tokes ]
parse = map ( \ wrd -> case wrd of
                        x : _ -> if isLower x then firstSet wrd 
                                              else secondSet wrd
                        _ -> Nada  ) . words 
    where firstSet wrd 
            | (length wrd) <  4  = MoveLeft
            | (length wrd) == 4  = MoveRight
            | (length wrd) == 5  = Inc
            | (length wrd) >  5  = Dec

          secondSet wrd 
            | (length wrd) <  4  = Output
            | (length wrd) == 4  = GetByte
            | (length wrd) == 5  = StartWhile
            | (length wrd) >  5  = EndWhile

eval :: ( Zipper Int , Zipper Tokes ) -> IO ( Zipper Int , Zipper Tokes ) 
eval ( ray , ast ) = 

    case ( emptyp ast ) || ( endp ast ) of

         True  -> return ( ray , ast )
         _ -> (
            case cursor ast  of 

                Nada       -> eval ( ray , right ast )
                MoveRight  -> eval ( right ray , right ast )
                MoveLeft   -> eval ( left  ray , right ast )
                Inc        -> eval ( change succ ray , right ast )
                Dec        -> eval ( change pred ray , right ast  )
                Output     -> ( putChar . chr . cursor $ ray ) >> eval ( ray , right ast )
                GetByte    -> do { c <- (fmap ord getChar) ; eval ( change (const c) ray , right ast ); }
                StartWhile -> if (cursor ray)  <  1 then eval . jumpCurs $ ( ray , right ast ) 
                                                    else eval (ray , right ast  )
                EndWhile   -> ( eval . goBack $ ( ray , left ast ) )
            )

    where jumpCurs' (ray , ast ) count = 
            case safeCursor ast of 

                Just StartWhile ->  jumpCurs' (ray , right ast ) (count + 1)
                Just EndWhile   ->  if count == 0 then ( ray , right ast )
                                             else jumpCurs' (ray , right ast) ( count - 1 )
                Nothing         -> error "unbounded looping"
                _               -> jumpCurs' (ray , right ast) count

          goBack'   (ray , ast ) count = 
            case safeCursor ast of 

                Just EndWhile   ->  goBack' (ray , left ast ) (count + 1)
                Just StartWhile ->  if count == 0 then ( ray , ast )
                                           else goBack' (ray , left ast) ( count - 1 )
                Nothing         -> error "unbounded looping"
                _               -> goBack' (ray , left ast) count

          jumpCurs v = jumpCurs' v 0
          goBack v   = goBack'   v 0
          change f ray = replace ( f . cursor $ ray ) ray

evaluate :: Int -> [ Tokes ] -> IO ()
evaluate n lst = eval ( fromList $ replicate n 0 , fromList lst) >> return () 

getSize lst = if (head lst) == "-mem" then read (lst !! 1 ) :: Int else 30
getFile lst = if (head lst) == "-mem" then lst!!2 else head lst

main = getArgs >>= \ arg -> (  ( readFile . getFile $ arg )  >>=  ( evaluate (getSize arg ) ) . parse )
