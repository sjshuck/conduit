{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- | Streaming functions for interacting with the filesystem.
module Data.Streaming.Filesystem
    ( DirStream
    , openDirStream
    , readDirStreamTyped
    , closeDirStream
    , FileType (..)
    , resolveFileType
    ) where

#if WINDOWS

import Data.Typeable (Typeable)
import qualified System.Win32 as Win32
import System.FilePath ((</>))
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import System.Directory (doesFileExist, doesDirectoryExist)

data DirStream = DirStream !Win32.HANDLE !Win32.FindData !(IORef Bool)
    deriving Typeable

openDirStream :: FilePath -> IO DirStream
openDirStream fp = do
    (h, fdat) <- Win32.findFirstFile $ fp </> "*"
    imore <- newIORef True -- always at least two records, "." and ".."
    return $! DirStream h fdat imore

closeDirStream :: DirStream -> IO ()
closeDirStream (DirStream h _ _) = Win32.findClose h

readDirStreamTyped :: DirStream -> IO (Maybe (FilePath, Maybe FileType))
readDirStreamTyped ds@(DirStream h fdat imore) = do
    more <- readIORef imore
    if more
        then do
            filename <- Win32.getFindDataFileName fdat
            Win32.findNextFile h fdat >>= writeIORef imore
            if filename == "." || filename == ".."
                then readDirStreamTyped ds
                else return $ Just (filename, Nothing)
        else return Nothing

resolveFileType :: FilePath -> Maybe FileType -> Bool -> IO FileType
resolveFileType fp _ _ = do
    isFile <- doesFileExist fp
    if isFile
        then return FTFile
        else do
            isDir <- doesDirectoryExist fp
            return $ if isDir then FTDirectory else FTOther

#else

import System.Posix.Directory (DirStream, openDirStream, closeDirStream)
import qualified System.Posix.Directory.Internals as I
import qualified System.Posix.Internals as I
import qualified System.Posix.Files as PosixF

peekFpAndType :: I.DirEnt -> IO (FilePath, Maybe FileType)
peekFpAndType de = do
    fp <- I.dirEntName de >>= I.peekFilePath
    dt <- I.dirEntType de
    return (fp, dTypeToFileType dt)

dTypeToFileType :: I.DirType -> Maybe FileType
dTypeToFileType = \case
    I.RegularFileType  -> Just FTFile
    I.DirectoryType    -> Just FTDirectory
    I.SymbolicLinkType -> Just FTSymlink
    I.UnknownType      -> Nothing
    _                  -> Just FTOther

statToFileType :: PosixF.FileStatus -> FileType
statToFileType s
    | PosixF.isRegularFile s  = FTFile
    | PosixF.isDirectory s    = FTDirectory
    | PosixF.isSymbolicLink s = FTSymlink
    | otherwise               = FTOther

readDirStreamTyped :: DirStream -> IO (Maybe (FilePath, Maybe FileType))
readDirStreamTyped ds = go where
    go = I.readDirStreamWith peekFpAndType ds >>= \case
        Just (".",  _) -> go
        Just ("..", _) -> go
        mfpmft         -> return mfpmft

resolveFileType :: FilePath -> Maybe FileType -> Bool -> IO FileType
resolveFileType fp mft followSymlinks =
    let resolveUnknown Nothing = statToFileType <$> if followSymlinks
            then PosixF.getFileStatus fp
            else PosixF.getSymbolicLinkStatus fp
        resolveUnknown (Just ft) = return ft

        resolveSymlink FTSymlink = do
            s <- PosixF.getFileStatus fp
            return $ case statToFileType s of
                FTDirectory | not followSymlinks -> FTSymlink
                ft                               -> ft
        resolveSymlink ft = return ft

    in resolveUnknown mft >>= resolveSymlink

#endif

data FileType
    = FTFile
    | FTDirectory
    | FTSymlink
    | FTOther
