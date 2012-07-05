{-# LANGUAGE ForeignFunctionInterface #-}
module System.Console.Terminfo.PrettyPrint.Curses
  ( initScr
  , screenWidth
  , endWin
  ) where

import Data.Functor
import Foreign.Marshal.Error (throwIfNull)
import Foreign.C.Types (CInt(..))
import Foreign.Ptr (Ptr)
import Foreign.Storable (peek)

type Window = Ptr ()

initScr :: IO Window
initScr = throwIfNull "initscr" initscr

foreign import ccall unsafe "cursed.h initscr" initscr :: IO Window

screenWidth :: IO Int
screenWidth = fromIntegral <$> peek colsPtr

foreign import ccall "cursed.h &COLS" colsPtr :: Ptr CInt
foreign import ccall unsafe "cursed.h endwin" endWin :: IO CInt
