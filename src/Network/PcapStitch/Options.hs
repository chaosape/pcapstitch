--------------------------------------------------------------------------------
-- | 
--  Copyright: Dan DaCosta 2010, Chaosape LLC
--  License: GPLv3
--  Maintainer: chaosape@chaosape.com
--  Stability: expermental
--  Portability: non-portable
--
-- Description: This file offers the functionality necessary to handle command
-- line arguments.
--------------------------------------------------------------------------------
-- {-# OPTIONS_GHC -Wall -Werror #-}
-- | This module exposes the data structure which stores command line 
-- options.  It is also responsible for santizing the command line arguments
-- that were not provided as options. These should specify file and path of 
-- packet capture files for PcapStitch to use. The 'Options' data structure
-- is created through monadic composition 
-- [http://www.haskell.org/haskellwiki/High-level_option_handling_with_GetOpt].
-- Initial values are provided by startOptions.   
module Network.PcapStitch.Options 
  ( Options ( .. )
  , exitHelp
  , exitError
  , exitErrorHelp
  , parseOptions
  , handlenonoptions
  , startOptions
  , options
  , options_property_horizon )
where
import Prelude hiding (FilePath)
import System.Console.GetOpt
import System.Environment
import Network.PcapStitch.Timestamp
import Control.Monad
import System
import System.Path
import System.Path.IO
import Network.PcapStitch.PacketEquivalencyFunction


--------------------------------------------------------------------------------

-- | The description of a 'Options.packetHorizon' option
stitchHorizonDescription :: String
stitchHorizonDescription = "\
\The stitch horizon specifies how long stitches will continue to be merged with \
\incoming packets. The stitch horizon is constantly moving forward according \
\to the most recent packet read from the network trace file. The default value \ 
\is 30 seconds."
        
-- | The description of the 'Options.stitchesPerPacket' option
mergesPerPacketDescription :: String
mergesPerPacketDescription ="\
\This option specifies the maximum number of packets that will be merged \
\with a stitch if identified as equivelent. The default value is 2."


-- | The description of the 'Options.packetEquivalencyFunction' option
packetEquivalencyFunctionDescription :: String
packetEquivalencyFunctionDescription ="\
\This option specifies how packets are determined to be equivalent. \
\Multiple fields can be used by joining fields with '+'. The default \
\packet equivalency function is '"++defaultPEF++"'.\
\The possible field choices are:"++ ((foldl (\ a e -> a.(showString ",").(showString e)) id fieldDescriptions) "")


pcapstitchDescription :: String
pcapstitchDescription = "\
\PcapStitch associates the same packet from network trace files \
\collected at multiple points in a network simultaneously.  Associations \
\are made through equivalency of control information, payload, and a stitch \
\horizon after collection has completed.  In other words, it stitches \
\together identical packets recorded in libpcap formatted packet traces. \
\PcapStitch can be used to collect information useful towards analysis of \
\communication network protocol performance and artifacts. PcapStitch is a \
\tool designed to be used on Linux variant operating system. Terminology \
\in this help output is defined in 'THESIS TITLE HERE'."



-- | TODO
defaultPEF :: String
defaultPEF = "FileMatch+Link.Payload+Net.Payload+Net.Source+Net.Destination+Net.ID+Net.Offset+Trans.Source+Trans.Destination+App"


-- | Options contains all configurable options from the command line that can
-- be applied the pcapstitch.
data Options = Options  
 { -- | See 'packetHorizonDescription'
   packetHorizon         :: Timestamp
   -- | See 'stitchesPerPacketDescription'
 , stitchesPerPacket     :: Int 
   -- | See 'packetEquivalencyFunctionDescription'
 , packetEquivalencyFunction :: [PacketEquivalencyFunction]}

instance Show Options where
  showsPrec _ (Options a b c) = 
    (showString "Timestamp Horizon =").(shows a).(showString ",").
    (showString "# of Matches =").(shows b).(showString ",").
    (showString "PacketEventManager =").(shows c)

-- | Default options 
startOptions :: Options
startOptions = Options (mkTimestamp 30 0) 2 (readPacketEquivalencyFunction defaultPEF)



-- | This function will convert the non-option inputs from the command line
-- which should be pcap dump files into Path types where the files are either
-- addressed via absolute path addressing or relative path addressing.
handlenonoptions :: [String]  -- ^ The non-option list of strings that should 
                              -- refer to packet captures paths and names.
                 -> [Either (AbsPath fd) (RelPath fd)]   
handlenonoptions files = map mkPathAbsOrRel files


-- | The options available for pcapstitch
options :: [OptDescr (Options -> IO Options)]
options = 
  [
  Option "H" ["packet-horizon"]
    (ReqArg
      buildts
      "Stitch Horizon"
    )
    stitchHorizonDescription
  ,Option "m" ["max-stitch-merge-count"]
    (ReqArg
      (\arg opt -> return opt { stitchesPerPacket = read arg })
      "Maximum # of times a packet can be merged with a stitch."
    )
    mergesPerPacketDescription

  ,Option "p" ["packet-equivalency-function"]
    (ReqArg
      (\arg opt -> return opt { packetEquivalencyFunction = readPacketEquivalencyFunction arg })
      "How packets are merged and compared."
    )
    packetEquivalencyFunctionDescription

  ,Option "h" ["help"]
     (NoArg
       (\_ -> exitHelp))
        "Show help"

  ]
  where 
    buildts arg opt = buildtsI opt . prepareString4TimeStamp $ arg
    buildtsI opt (f,s) = do
      let a = mkTimestamp f s 
      return opt { packetHorizon = a}

-- | This function will take a sting in the form of "[0-9].[0-9]" and convert 
-- it to a tuple that can be used to initialize a 'TimeStamp' data object.
prepareString4TimeStamp ::
                         (Integral a)=> 
                         String -- ^ The string that holds a float 
                                  -- representation
                         -> (a,a)
prepareString4TimeStamp str =
  if dbl<0 
    then error $ "Horizon must be greater than 0"
  else if  partnum < 1 && partnumflt /= 0.0 
    then error $ "Sub-Seconds must be greater than " ++ (show unit)
    else outputprecerr (wholenum, partnum)
  where
    unit = 1/(fromIntegral upperopenbound):: Double
    dbl = read str
    (wholenum,partnum') = properFraction dbl
    partnumflt = (*) (fromIntegral upperopenbound) $ partnum'
    partnum = round partnumflt
    outputprecerr x = 
      if partnumflt+unit < 1 then {-trace "Precision Error Detected!"-} x else x

-- | A function for 'QuickCheck' to test proper handling of the Packet 
-- Horizon options.
-- TODO : When we are using the latest QuickCheck library this double
-- should only be positive.
options_property_horizon :: Double -> Bool
options_property_horizon d =  if d < 0 then True else val
  where
    pts = prepareString4TimeStamp . show 
    ts = tsToDouble . mkTimestamp (fst.pts $ d) $ (snd.pts $ d)
    diff = abs . (-) d $ ts
    val = diff < (1/(fromIntegral upperopenbound))


showHelp :: IO ()
showHelp = do
    prg <- getProgName
    hPutStrLn stderr (usageInfo prg options)
    hPutStrLn stderr pcapstitchDescription

    hFlush stderr

exitHelp :: IO a
exitHelp = do
    showHelp
    exitWith ExitSuccess

exitError :: String -> IO a
exitError msg = do
    hPutStrLn stderr msg
    hPutStrLn stderr ""
    exitFailure

exitErrorHelp :: String -> IO a
exitErrorHelp msg = do
    hPutStrLn stderr msg
    hPutStrLn stderr ""
    showHelp
    exitFailure

{- readArg :: Read a => String -> String -> IO a
readArg name arg = do
    case reads arg of
         ((x, []) : _)  -> return x
         _              -> exitError $ "Can't parse " ++ name ++ " arg" -}

parseOptions :: IO (Options, [String])
parseOptions = do
    (optsActions, rest, errors) <- getArgs >>= return . getOpt RequireOrder options

    when (not (null errors)) $ do
         mapM_ (hPutStrLn stderr) errors
         showHelp
         exitFailure

    opts <- foldl (>>=) (return startOptions) optsActions
    return (opts, rest)
