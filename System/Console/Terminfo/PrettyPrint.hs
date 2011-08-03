module System.Console.Terminfo.PrettyPrint
  ( PushCommand(..)
  , with
  , blink
  , bold
  , underline
  , standout
  , reversed
  , protected
  , invisible
  , dim
  , soft
  , foreground
  , background
  , Bell(..)
  , ring
  , evalTermState
  , displayCap
  , displayDoc
  ) where

import Text.PrettyPrint.Leijen.Extras
import System.Console.Terminfo.Color
import System.Console.Terminfo.Effects
import System.Console.Terminfo.Base
import Data.Traversable
import Control.Applicative
import Control.Monad
import Control.Monad.Trans.State
import UI.HSCurses.Curses (initScr, scrSize, endWin)
import Control.Monad.Trans.Class
import Control.Exception (finally, throwIO, AssertionFailed(..))
import System.IO (stdout)

newtype Colour = Colour { color :: Color }

instance Eq Colour where
  Colour Black == Colour Black = True
  Colour Red == Colour Red = True
  Colour Green == Colour Green = True
  Colour Yellow == Colour Yellow = True
  Colour Blue == Colour Blue = True
  Colour Magenta == Colour Magenta = True
  Colour Cyan == Colour Cyan = True
  Colour White == Colour White = True
  Colour (ColorNumber n) == Colour (ColorNumber m) = n == m
  _ == _ = False

data PushCommand
 = Bold
 | Standout
 | Underline
 | Reverse
 | Blink
 | Dim
 | Invisible
 | Protected
 | Foreground Colour
 | Background Colour
 | Else PushCommand PushCommand
 | Nop
 deriving (Eq)

data Bell
  = VisibleBellOnly
  | AudibleBellOnly
  | VisibleBellPreferred
  | AudibleBellPreferred
  deriving (Eq,Ord,Show,Enum)

data Command 
  = Push PushCommand
  | Pop
  | Ring Bell -- visual bell ok, audible bell ok, 
  deriving (Eq)

type TermState = [PushCommand]

ring :: Bell -> Doc Command
ring b = pure (Ring b)

eval :: Command -> StateT TermState Capability TermOutput
eval (Push Blink)          = modify (Blink:) *> lift blinkOn
eval (Push Reverse)        = modify (Reverse:) *> lift reverseOn
eval (Push Protected)      = modify (Protected:) *> lift protectedOn
eval (Push Bold)           = modify (Bold:) *> lift boldOn
eval (Push (Foreground n)) = do
  modify (Foreground n:) 
  f <- lift setForegroundColor
  return $ f $ color n
eval (Push (Background n)) = do
  modify (Background n:)
  f <- lift setBackgroundColor
  return $ f $ color n
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
    _           -> mzero
  where 
   replay xs = do 
     l <- lift allAttributesOff 
     r <- foldr (<#>) mempty <$> traverse (eval . Push) (reverse xs)
     return $ l <#> r

tryTerm :: MonadPlus m => m TermOutput -> m TermOutput
tryTerm m = m `mplus` return mempty

with :: PushCommand -> Doc Command -> Doc Command
with cmd = pure (Push cmd) `enclose` pure Pop

soft :: PushCommand -> PushCommand
soft l = Else l Nop

blink, bold, underline, standout, reversed, protected, invisible, dim :: Doc Command -> Doc Command
blink      = with (soft Blink)
bold       = with (soft Bold)
underline  = with (soft Underline)
reversed   = with (soft Reverse)
protected  = with (soft Protected)
invisible  = with (soft Invisible)
dim        = with (soft Dim) 
standout   = with (soft Standout)
foreground, background :: Color -> Doc Command -> Doc Command
foreground n = with (soft (Foreground (Colour n)))
background n = with (soft (Background (Colour n)))

displayCap :: SimpleDoc Command -> StateT TermState Capability TermOutput
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

displayDoc :: Float -> Doc Command -> IO ()
displayDoc ribbon doc = do
    term <- setupTermFromEnv
    cols <- kludgeWindowSize `mplus` return 80
    let sdoc = renderPretty ribbon cols doc
    colored term sdoc `mplus` displayIO stdout sdoc
  where 
    colored term sdoc = case getCapability term $ evalTermState $ displayCap sdoc of
      Just output -> runTermOutput term output
      Nothing     -> throwIO $ AssertionFailed "missing capability" -- TODO: downgrade
