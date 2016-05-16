{-#LANGUAGE BangPatterns #-}
module ApplyClip
    ( applyClipMain
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


data ScriptOp = Replace 
                { base_from:: Int
                , base_to:: Int
                , src_from:: Int
                , src_to:: Int
                }
              | Insert 
                { base_to:: Int
                , src_from:: Int
                , src_to:: Int
                }
              | Delete
                { base_from:: Int
                , base_to:: Int
                }
              | Append
                { src_from:: Int
                , src_to:: Int
                }
    deriving Show
type Script = [ScriptOp]

applyClipMain env args p = applyClipEntry env args p >>= toRawValue


applyClipEntry :: Ptr C'AVS_ScriptEnvironment
       -> Ptr C'AVS_Value
       -> Ptr ()
       -> IO AVS_Value
applyClipEntry env rargs params = do
    (AVS_ValueArray (base:source:file:_)) <- fromRawValue rargs env
    let baseClip = avs_clip base
        srcClip = avs_clip source
        AVS_ValueString filename = file
    liftM toAVSValue $ applyClip env baseClip srcClip filename
    
    
applyClip:: Ptr C'AVS_ScriptEnvironment
        -> AVS_Clip
        -> AVS_Clip
        -> String
        -> IO AVS_Clip
applyClip env base source scriptfile = do
    escript <- loadScript scriptfile
    case escript of
        Left some -> return $ avs_clip $ avs_error some
        Right script -> do
            runScript env base source script

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
          'r' -> getOpReplace line $ tail str
          'i' -> getOpInsert line $ tail str
          'd' -> getOpDelete line $ tail str
          'a' -> getOpAppend line $ tail str
          _ -> Left $ "unsupported command " ++ show op ++ " at line " ++ show line
    in case parsed of
        Left some -> Left some
        Right val -> Right (line + 1, tmp ++ [val])
    where
    getOpReplace line s = 
        let params = split isSpace s 
        in case length params of
            4 -> let [baseFrom, baseTo, srcFrom, srcTo] = map read params
                in analysScriptOp line $ Replace baseFrom baseTo srcFrom srcTo
            some | some < 4 -> Left $! "too few parameters for replace in line " ++ show line
            some | some > 4 -> Left $! "too many parameters for replace in line " ++ show line
    
    getOpInsert line s = 
        let params = split isSpace s 
        in case length params of
            3 -> let [baseTo, srcFrom, srcTo] = map read params
                in analysScriptOp line $ Insert baseTo srcFrom srcTo
            some | some < 3 -> Left $! "too few parameters for insert in line " ++ show line
            some | some > 3 -> Left $! "too many parameters for insert in line " ++ show line
    getOpDelete line s = 
        let params = split isSpace s 
        in case length params of
            2 -> let [baseFrom, baseTo] = map read params
                in analysScriptOp line $ Delete baseFrom baseTo
            some | some < 2 -> Left $! "too few parameters for delete in line " ++ show line
            some | some > 2 -> Left $! "too many parameters for delete in line " ++ show line
    getOpAppend line s = 
        let params = split isSpace s 
        in case length params of
            2 -> let [srcFrom, srcTo] = map read params
                in analysScriptOp line $ Append srcFrom srcTo
            some | some < 2 -> Left $! "too few parameters for append in line " ++ show line
            some | some > 2 -> Left $! "too many parameters for append in line " ++ show line
    

analysScriptOp line (Append sf st) | sf < 0 = 
    Left $ "line " ++ show line ++ " Append: src_from < 0"
analysScriptOp line (Append sf st) | sf > st = 
    Left $ "line " ++ show line ++ " Append: src_from > src_to"
analysScriptOp line (Delete bf bt) | bf < 0 = 
    Left $ "line " ++ show line ++ " Delete: base_from < 0"
analysScriptOp line (Delete bf bt) | bf > bt = 
    Left $ "line " ++ show line ++ " Delete: base_from > base_to"
analysScriptOp line (Insert bt sf st) | bt < 0 = 
    Left $ "line " ++ show line ++ " Insert: base_to < 0"
analysScriptOp line (Insert bt sf st) | sf < 0 = 
    Left $ "line " ++ show line ++ " Insert: src_from < 0"
analysScriptOp line (Insert bt sf st) | sf > st = 
    Left $ "line " ++ show line ++ " Insert: src_from > src_to"
analysScriptOp line (Replace bf bt sf st) | bf < 0 = 
    Left $ "line " ++ show line ++ " Replace: base_from < 0"
analysScriptOp line (Replace bf bt sf st) | bf > bt = 
    Left $ "line " ++ show line ++ " Replace: base_from > base_to"
analysScriptOp line (Replace bf bt sf st) | sf < 0 = 
    Left $ "line " ++ show line ++ " Replace: src_from < 0"
analysScriptOp line (Replace bf bt sf st) | sf > st = 
    Left $ "line " ++ show line ++ " Replace: src_from > src_to"
analysScriptOp line some = Right some


runScript:: Ptr C'AVS_ScriptEnvironment
        -> AVS_Clip
        -> AVS_Clip
        -> Script
        -> IO AVS_Clip
runScript env base src [] = return base
runScript env base src ((Replace bf bt sf st):ss) = do
    baseend <- clipGetFramesNum base
    trimmedHead <- do 
            if bf > 0 then liftM Just $ trim env base 0 bf 
                                    >>= \clip-> deleteFrame env clip bf
                else return Nothing
    trimmedSrc <- liftM Just $ trim env src sf st
    trimmedTail <- do 
            if baseend - bt > 1 then liftM Just $ trim env base (bt+1) baseend
                else return Nothing
    let chains = map fromJust $ filter (/=Nothing) [trimmedHead, trimmedSrc, trimmedTail]
    ret <- case chains of
        [only]-> return only
        (x:xs) -> foldM (unalignedSplice env) x xs
    runScript env ret src ss
runScript env base src ((Insert bt sf st):ss) = do
    baseend <- clipGetFramesNum base
    trimmedHead <- do 
            if bt > 0 then liftM Just $ trim env base 0 bt
                                    >>= \clip-> deleteFrame env clip bt
                else return Nothing
    trimmedSrc <- liftM Just $ trim env src sf st
    trimmedTail <- do 
            if baseend - bt > 1 then liftM Just $ trim env base bt baseend
                else return Nothing
    let chains = map fromJust $ filter (/=Nothing) [trimmedHead, trimmedSrc, trimmedTail]
    ret <- case chains of
        [only]-> return only
        (x:xs) -> foldM (unalignedSplice env) x xs
    runScript env ret src ss
runScript env base src ((Delete bf bt):ss) = do
    baseend <- clipGetFramesNum base
    trimmedHead <- do 
            if bf > 0 then liftM Just $ trim env base 0 bf 
                                    >>= \clip-> deleteFrame env clip bf
                else return Nothing
    trimmedTail <- do 
            if baseend - bt > 1 then liftM Just $ trim env base (bt+1) baseend
                else return Nothing
    let chains = map fromJust $ filter (/=Nothing) [trimmedHead, trimmedTail]
    ret <- case chains of
        [only]-> return only
        (x:xs) -> foldM (unalignedSplice env) x xs
    runScript env ret src ss
runScript env base src ((Append sf st):ss) = do
    baseend <- clipGetFramesNum base
    trimmedTail <- trim env src sf st
    ret <- unalignedSplice env base trimmedTail
    runScript env ret src ss
