{-#LANGUAGE BangPatterns #-}
module AdvMerge1 
    ( advMerge1Main
    )
    where

import Utils
import Data.Maybe
import Data.Char
import Data.List

import System.IO
import System.Directory
import Control.Monad
import HAviSynth.AviSynth

import Foreign.C.Types
import Foreign.C.String
import Foreign.Ptr
import Foreign.Storable


data ScriptOp = FromLeftRange Int Int
              | FromLeftFrame Int
              | FromRightRange Int Int
              | FromRightFrame Int
    deriving Show
type Script = [ScriptOp]

advMerge1Main env args p = advMergeEntry env args p >>= toRawValue


advMergeEntry :: Ptr C'AVS_ScriptEnvironment
       -> Ptr C'AVS_Value
       -> Ptr ()
       -> IO AVS_Value
advMergeEntry env rargs params = do
    (AVS_ValueArray (base:source:file:_)) <- fromRawValue rargs env
    let leftClip = avs_clip base
        rightClip = avs_clip source
        AVS_ValueString filename = file
    liftM toAVSValue $ advMerge env leftClip rightClip filename
    
    
advMerge:: Ptr C'AVS_ScriptEnvironment
        -> AVS_Clip
        -> AVS_Clip
        -> String
        -> IO AVS_Clip
advMerge env left right scriptfile = do
    escript <- loadScript scriptfile
    case escript of
        Left some -> return $ avs_clip $ avs_error some
        Right script -> do
            base <- avs_blank_clip env left
            runScript env base left right script

loadScript:: String-> IO (Either String Script)
loadScript filename = do
    exists <- doesFileExist filename
    if exists == False then return $ Left "file does not exists"
        else readScript
        where
        readScript = do
            withFile filename ReadMode $ \h->  do
                content <- hGetContents h
                let lined = map ( dropWhile (isSpace)) $! lines content
                    filtered = filter (\s-> length s > 1 && head s /= '#') lined
                    !ret = foldl' parseScriptOp (Right (1,[])) filtered
                return $ case ret of
                    Left some -> Left some
                    Right (_, s) -> Right s

parseScriptOp:: Either String (Int, [ScriptOp])-> String-> Either String (Int, [ScriptOp])
parseScriptOp (Left some) _ = Left some
parseScriptOp (Right (line, tmp)) str | length str < 1 = 
    Left $! "empty line " ++ show line ++ " in script!"
parseScriptOp (Right (!line, tmp)) str = 
    let op = head str
        !parsed = case op of
          'r' -> getOpRight line $ tail str
          'l' -> getOpLeft line $ tail str
          _ -> Left $ "unsupported command " ++ show op ++ " at line " ++ show line
    in case parsed of
        Left some -> Left some
        Right val -> Right (line + 1, tmp ++ [val])
    where
    getOpLeft line s = 
        let params = split isSpace s 
        in case length params of
            2 -> let [from, to] = map read params
                in analysScriptOp line $ FromLeftRange from to
            1 -> let [from] = map read params
                in analysScriptOp line $ FromLeftFrame from
            some | some < 1 -> Left $! "too few parameters for left in line " ++ show line
            some | some > 2 -> Left $! "too many parameters for left in line " ++ show line
    getOpRight line s = 
        let params = split isSpace s 
        in case length params of
            2 -> let [from, to] = map read params
                in analysScriptOp line $ FromRightRange from to
            1 -> let [from] = map read params
                in analysScriptOp line $ FromRightFrame from
            some | some < 1 -> Left $! "too few parameters for right in line " ++ show line
            some | some > 2 -> Left $! "too many parameters for right in line " ++ show line
    

analysScriptOp line (FromLeftRange from to) | from < 0 = 
    Left $ "line " ++ show line ++ " LeftRange: from < 0"
analysScriptOp line (FromLeftRange from to) | to < 0 = 
    Left $ "line " ++ show line ++ " LeftRange: to < 0"
analysScriptOp line (FromRightRange from to) | from < 0 = 
    Left $ "line " ++ show line ++ " LeftRange: from < 0"
analysScriptOp line (FromRightRange from to) | to < 0 = 
    Left $ "line " ++ show line ++ " LeftRange: to < 0"
analysScriptOp line some = Right some


runScript:: Ptr C'AVS_ScriptEnvironment
        -> AVS_Clip
        -> AVS_Clip
        -> AVS_Clip
        -> Script
        -> IO AVS_Clip
runScript env base left right [] = return base
runScript env base left right ((FromLeftRange from to):ss) = do
    trimmed <- trim env left from to
    newbase <- unalignedSplice env base trimmed
    runScript env newbase left right ss
runScript env base left right ((FromLeftFrame from):ss) = do
    trimmed <- trim env left from (-1)
    newbase <- unalignedSplice env base trimmed
    runScript env newbase left right ss
runScript env base left right ((FromRightRange from to):ss) = do
    trimmed <- trim env right from to
    newbase <- unalignedSplice env base trimmed
    runScript env newbase left right ss
runScript env base left right ((FromRightFrame from):ss) = do
    trimmed <- trim env right from (-1)
    newbase <- unalignedSplice env base trimmed
    runScript env newbase left right ss
