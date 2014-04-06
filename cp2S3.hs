--
--  Copyright (c) 2014 - 2014 Tad Doxsee
--  All rights reserved.
--
--  Author: Tad Doxsee
--
--  Purpose: To copy files from a named directory to a named
--           AWS S3 bucket. Only files that are not already in the bucket
--           will be copied to the bucket.
--
--  Usage: cp2S3 dirName bucketName
--
--         Also, there must be the file $HOME/.aws-keys with valid AWS access
--         and secret keys in this format
--
--         default AccessKeyID SecretKey
--
--         Or, as noted here (https://github.com/aristidb/aws) you may use
--         environment variables instead.
--
--------------------------------------------------------------------------------

{-# LANGUAGE OverloadedStrings #-}

-- base
import Control.Monad      (forM_, void, when)
import System.Environment (getArgs)
import System.Exit        (exitFailure)

-- aws
import qualified Aws
import qualified Aws.S3 as S3

-- bytestring
import qualified Data.ByteString.Lazy as L

-- containers 
import qualified Data.Set as S

-- directory
import System.Directory (getDirectoryContents)

-- filepath
import System.FilePath.Posix ((</>), takeFileName)

-- http-conduit
import Network.HTTP.Conduit ( RequestBody(RequestBodyLBS), withManager)

-- text
import Data.Text (pack)

-- transformers
import Control.Monad.IO.Class (liftIO)

--------------------------------------------------------------------------------

main :: IO ()
main = do
  
  args <- getArgs
  
  when (length args /= 2) $ do
    putStrLn "usage: cp2S3 dirName bucketName"
    exitFailure
    
  let dirName = args !! 0
      bkt     = pack $ args !! 1
  
  names <- getDirectoryContents dirName
  
  let fileNames = filter (`notElem` [".", ".."]) names
      paths = map (dirName </>) fileNames
  
  -- set up AWS credentials and the default configuration.
  cfg <- Aws.baseConfiguration
  let s3cfg = Aws.defServiceConfig :: S3.S3Configuration Aws.NormalQuery

  withManager $ \mgr -> do
    
    -- get the names of the objects in the S3 bucket and store them
    -- in a Set for fast lookup
    
    gbr <- Aws.pureAws cfg s3cfg mgr $ S3.getBucket bkt    
    let objNames = S.fromList $ map S3.objectKey $ S3.gbrContents gbr
    
    -- for each file in the current working directory, if it is not
    -- already in the bucket, put it there
    
    liftIO $ putStrLn "putting..."
    
    forM_ paths
          $ \p -> when (not (S.member (pack (takeFileName p)) objNames)) $ do
            liftIO $ putStrLn p
            fContents <- liftIO $ L.readFile p

            void $ Aws.pureAws cfg s3cfg mgr
              $ S3.putObject bkt (pack (takeFileName p))
                                 (RequestBodyLBS fContents)
    
    liftIO $ putStrLn "done"
