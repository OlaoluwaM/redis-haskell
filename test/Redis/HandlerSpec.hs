module Redis.HandlerSpec where

import Control.Monad.IO.Class (MonadIO (..))
import Data.ByteString (ByteString)
import Data.Foldable (for_)
import Redis.Handler (handleCommandReq)
import Redis.Helper (echoCmd, mkBulkString, mkCmdReqStr, pingCmd)
import Redis.RESP (RESPDataType (SimpleString), serializeRESPDataType)
import Redis.Test (runTestM)
import Test.Hspec (Spec, describe, it, shouldBe)

spec_integration_tests :: Spec
spec_integration_tests = do
    describe "Handler integration tests" do
        for_
            [
                ( "Can successfully handle a PING command request with an argument" :: String
                , mkCmdReqStr [pingCmd, mkBulkString "Hello"]
                , serializeRESPDataType . mkBulkString $ "Hello"
                )
            ,
                ( "Can successfully handle a PING command request without an argument"
                , mkCmdReqStr [pingCmd]
                , serializeRESPDataType . SimpleString $ "PONG"
                )
            ,
                ( "Can successfully handle an ECHO command request" :: String
                , mkCmdReqStr [echoCmd, mkBulkString "Check check 1 2 3"]
                , serializeRESPDataType . mkBulkString $ "Check check 1 2 3"
                )
            ]
            $ \(desc, cmdReq, expectedRes) ->
                it desc do
                    result <- liftIO (runTestM @ByteString (handleCommandReq cmdReq) Nothing)
                    result `shouldBe` expectedRes
