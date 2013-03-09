{-# LANGUAGE CPP #-}
{-# LANGUAGE TypeFamilies #-}
module System.Console.Terminfo.PrettyPrint
  (
  -- * Raw Effect (requires the effect be present)
    ScopedEffect(..)
  , with
  , Effect(..) -- unpaired effects
  -- ** Graceful degradation
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
  , display
  , displayLn
  -- ** Progressively less magical formatting
  , displayDoc
  , displayDoc'
  , displayDoc''
  -- ** A Classy Interface
  , PrettyTerm(..)
  -- ** Evaluation
  , SimpleTermDoc
  , evalTermState
  , displayCap
  ) where

import Text.PrettyPrint.Free
import System.Console.Terminfo.Color
import System.Console.Terminfo.Effects
import System.Console.Terminfo.Base
import System.Console.Terminfo.Cursor
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import Data.Int
import Data.Word
import Data.Foldable (toList)
import Data.Traversable
import Data.Sequence (Seq)
import Numeric.Natural (Natural)
import Data.List.NonEmpty (NonEmpty)
import Control.Applicative
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.State
import Control.Monad.Trans.Class
import Control.Exception (finally)
import System.IO (stdout)
#ifdef Cursed
import System.Console.Terminfo.PrettyPrint.Curses
#endif


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
#ifdef Cursed
kludgeWindowSize = do
   _ <- initScr
   s <- screenWidth
   s <$ guard (s >= 30 && s < 320)
 `finally` endWin
#else
kludgeWindowSize = fail "missing ncurses"
#endif

displayLn :: MonadIO m => PrettyTerm t => t -> m ()
displayLn t = displayDoc 0.6 (prettyTerm t <> linebreak)

display :: (MonadIO m, PrettyTerm t) => t -> m ()
display = displayDoc 0.6

displayDoc :: (MonadIO m, PrettyTerm t) => Float -> t -> m ()
displayDoc ribbon doc = do
  term <- liftIO setupTermFromEnv
  displayDoc' term ribbon doc

displayDoc' :: (MonadIO m, PrettyTerm t) => Terminal -> Float -> t -> m ()
displayDoc' term ribbon doc = do
  cols <- liftIO $ kludgeWindowSize `mplus`
          return (maybe 80 id (getCapability term termColumns))
  displayDoc'' term ribbon (cols - 1) doc

displayDoc'' :: (MonadIO m, PrettyTerm t) => Terminal -> Float -> Int -> t -> m ()
displayDoc'' term ribbon cols doc =
  case getCapability term $ evalTermState $ displayCap sdoc of
    Just output -> liftIO $ runTermOutput term output
    Nothing     -> liftIO $ displayIO stdout sdoc
  where sdoc = renderPretty ribbon cols (prettyTerm doc)

class Pretty t => PrettyTerm t where
  prettyTerm :: t -> TermDoc
  prettyTerm = pretty
  prettyTermList :: [t] -> TermDoc
  prettyTermList = list . map prettyTerm

instance PrettyTerm t => PrettyTerm [t] where
  prettyTerm = prettyTermList

instance PrettyTerm Char where
  prettyTerm = char
  prettyTermList = prettyList

instance e ~ Effect => PrettyTerm (Doc e) where
  prettyTerm = id
  prettyTermList = list

instance PrettyTerm B.ByteString
instance PrettyTerm BL.ByteString
instance PrettyTerm T.Text
instance PrettyTerm TL.Text
instance PrettyTerm Int
instance PrettyTerm Int8
instance PrettyTerm Int16
instance PrettyTerm Int32
instance PrettyTerm Int64
instance PrettyTerm Word
instance PrettyTerm Word8
instance PrettyTerm Word16
instance PrettyTerm Word32
instance PrettyTerm Word64
instance PrettyTerm Bool
instance PrettyTerm Integer
instance PrettyTerm Float
instance PrettyTerm Double
instance PrettyTerm ()
instance PrettyTerm Natural

instance PrettyTerm a => PrettyTerm (Seq a) where
  prettyTerm = prettyTermList . toList

instance PrettyTerm a => PrettyTerm (NonEmpty a) where
  prettyTerm = prettyTermList . toList

instance (PrettyTerm a,PrettyTerm b) => PrettyTerm (a,b) where
  prettyTerm (x,y) = tupled [prettyTerm x, prettyTerm y]

instance (PrettyTerm a,PrettyTerm b,PrettyTerm c) => PrettyTerm (a,b,c) where
  prettyTerm (x,y,z) = tupled [prettyTerm x, prettyTerm y, prettyTerm z]

instance PrettyTerm a => PrettyTerm (Maybe a) where
  prettyTerm Nothing  = empty
  prettyTerm (Just x) = prettyTerm x
