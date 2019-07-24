module Nym 
    ( NymState
    , dbHandle
    , arguments
    , dbFilename
    , createNymState
    ) where

import Data.Text (Text)

import Cli (Arguments)
import Search as S

data NymState = NymState 
    { dbHandle :: S.Handle
    , arguments :: Arguments
    }

dbFilename :: Text
dbFilename = "nyms.db"

createNymState :: S.DatabasePath -> Arguments -> IO NymState
createNymState dbPath args = do
    handle <- S.createHandle dbPath
    return (NymState handle args)

