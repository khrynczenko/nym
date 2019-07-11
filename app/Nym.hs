module Nym 
    ( NymState
    , dbHandle
    , arguments
    , dbFilename
    , createNymState
    ) where

import qualified Data.Text as T
import Cli (Arguments)
import qualified Search as S

data NymState = NymState 
    { dbHandle :: S.Handle
    , arguments :: Arguments
    }

dbFilename :: T.Text
dbFilename = "nyms.db"

createNymState :: S.DatabasePath -> Arguments -> IO NymState
createNymState dbPath args = do
    handle <- S.createHandle dbPath
    return (NymState handle args)

