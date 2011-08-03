module System.Console.Terminfo.PrettyPrint
  ( 
  -- * Raw Effect (requires the effect be present)
    ScopedEffect(..)
  , with
  , Effect(..) -- unpaired effects
  -- ** graceful feature degradation
  , soft
  -- ** Effects (built with soft)
  , blink -- with (soft Blink)
  , bold  -- with (soft Bold)
  , underline -- with (soft Underline)
  , standout -- with (soft Standout)
  , reversed -- with (soft Reversed)
  , protected -- with (soft Protected)
  , invisible -- with (soft Invisible)
  , dim -- with (soft Dim)
  -- ** Colors (built with soft)
  , red
  , black
  , green
  , blue
  , yellow
  , magenta
  , cyan
  , white
  , foreground
  , background
  -- ** Ringing bells
  , Bell(..)
  , ring
  -- * A Color Pretty Printer
  , TermDoc
  , displayDoc
  , displayDoc'
  , displayDoc''
  -- * 
  , SimpleTermDoc
  , evalTermState
  , displayCap
  ) where

import Text.PrettyPrint.Leijen.Extras
import System.Console.Terminfo.Color
import System.Console.Terminfo.Effects
import System.Console.Terminfo.Base
import System.Console.Terminfo.Cursor
import Data.Traversable
import Control.Applicative
import Control.Monad
import Control.Monad.Trans.State
import UI.HSCurses.Curses (initScr, scrSize, endWin)
import Control.Monad.Trans.Class
import Control.Exception (finally)
import System.IO (stdout)


data ScopedEffect
 = Bold
 | Standout
 | Underline
 | Reverse
 | Blink
 | Dim
 | Invisible
 | Protected
 | Foreground Color
 | Background Color
 | Else ScopedEffect ScopedEffect
 | Nop
 deriving (Eq)

data Bell
  = VisibleBellOnly
  | AudibleBellOnly
  | VisibleBellPreferred
  | AudibleBellPreferred
  deriving (Eq,Ord,Show,Enum)

data Effect 
  = Push ScopedEffect
  | Pop
  | Ring Bell -- visual bell ok, audible bell ok, 
  deriving (Eq)

type TermState = [ScopedEffect]

ring :: Bell -> TermDoc
ring b = pure (Ring b)

eval :: Effect -> StateT TermState Capability TermOutput
eval (Push Blink)          = modify (Blink:) *> lift blinkOn
eval (Push Reverse)        = modify (Reverse:) *> lift reverseOn
eval (Push Protected)      = modify (Protected:) *> lift protectedOn
eval (Push Bold)           = modify (Bold:) *> lift boldOn
eval (Push (Foreground n)) = do
  modify (Foreground n:) 
  f <- lift setForegroundColor
  return (f n)
eval (Push (Background n)) = do
  modify (Background n:)
  f <- lift setBackgroundColor
  return (f n)
eval (Push Invisible)      = modify (Invisible:) *> lift invisibleOn
eval (Push Dim)            = modify (Dim:) *> lift dimOn
eval (Push Underline)      = modify (Underline:) *> lift enterUnderlineMode
eval (Push Standout)       = modify (Standout:) *> lift enterStandoutMode
eval (Push Nop)            = modify (Nop:) *> return mempty
eval (Push (Else l r))     = eval (Push l) <|> eval (Push r)
eval (Ring b)              = case b of
  VisibleBellOnly  -> lift $ tryTerm visualBell
  AudibleBellOnly -> lift $ tryTerm bell
  VisibleBellPreferred -> lift $ visualBell `mplus` tryTerm bell
  AudibleBellPreferred -> lift $ bell `mplus` tryTerm visualBell
eval Pop = do 
  ts <- get
  let ts' = drop 1 ts
  put ts'
  flip mplus (replay ts') $ case ts of 
    Standout:_  -> lift exitStandoutMode
    Underline:_ -> lift exitUnderlineMode
    Nop:_       -> return mempty
    _           -> mzero
  where 
   replay xs = do 
     l <- lift allAttributesOff 
     s <- get
     r <- foldr (<#>) mempty <$> traverse (eval . Push) (reverse xs)
     put s
     return $ l <#> r

type TermDoc = Doc Effect
type SimpleTermDoc = SimpleDoc Effect

tryTerm :: MonadPlus m => m TermOutput -> m TermOutput
tryTerm m = m `mplus` return mempty

with :: ScopedEffect -> TermDoc -> TermDoc
with cmd = pure (Push cmd) `enclose` pure Pop

soft :: ScopedEffect -> ScopedEffect
soft l = Else l Nop

foreground, background :: Color -> TermDoc -> TermDoc
foreground n = with (soft (Foreground n))
background n = with (soft (Background n))

red, black, green, yellow, blue, magenta, cyan, white, blink, bold, underline, 
 standout, reversed, protected, invisible, dim :: TermDoc -> TermDoc

blink      = with (soft Blink)
bold       = with (soft Bold)
underline  = with (soft Underline)
reversed   = with (soft Reverse)
protected  = with (soft Protected)
invisible  = with (soft Invisible)
dim        = with (soft Dim) 
standout   = with (soft Standout)

red = foreground Red
black = foreground Black
green = foreground Green
yellow = foreground Yellow
blue = foreground Blue
magenta = foreground Magenta
cyan = foreground Cyan
white = foreground White

displayCap :: SimpleTermDoc -> StateT TermState Capability TermOutput
displayCap = go where
  go SEmpty        = return mempty
  go (SChar c x)   = (termText [c] <#>) <$> go x
  go (SText _ s x) = (termText s <#>) <$> go x
  go (SLine i x)   = (termText ('\n': spaces i) <#>) <$> go x
  go (SEffect e t) = (<#>) <$> eval e <*> go t
    
spaces :: Int -> String
spaces n | n <= 0    = ""
         | otherwise = replicate n ' '

evalTermState :: Monad m => StateT TermState m a -> m a
evalTermState s = liftM fst $ runStateT s []

kludgeWindowSize :: IO Int
kludgeWindowSize = do
   _ <- initScr
   snd <$> scrSize
 `finally` endWin

displayDoc :: Float -> TermDoc -> IO ()
displayDoc ribbon doc = do
  term <- setupTermFromEnv
  displayDoc' term ribbon doc

displayDoc' :: Terminal -> Float -> TermDoc -> IO ()
displayDoc' term ribbon doc = do
  cols <- kludgeWindowSize `mplus` 
          return (maybe 80 id (getCapability term termColumns))
  displayDoc'' term ribbon cols doc

displayDoc'' :: Terminal -> Float -> Int -> TermDoc -> IO ()
displayDoc'' term ribbon cols doc = 
  case getCapability term $ evalTermState $ displayCap sdoc of
    Just output -> runTermOutput term output
    Nothing     -> displayIO stdout sdoc
  where sdoc = renderPretty ribbon cols doc
