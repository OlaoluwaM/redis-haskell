module Redis.Server.Config.ReadersSpec where

import Test.Hspec

import Data.Either (isLeft)
import Data.Foldable (for_)
import Redis.Server.Config.Readers (portReader, replicaOfReader)
import Redis.Server.Config.Types (
    HostPortRec (..),
    Port,
    ReplicaOfType (..),
 )
import Redis.Utils (runReadM)
import Rerefined (unrefine)
import Rerefined.Refine.TH (refineTH)

spec_port_reader :: Spec
spec_port_reader = do
    describe "portReader" do
        it "Reads a valid port" do
            unrefine <$> runReadM @String portReader "6379" `shouldBe` Right 6379

        it "Accepts the lowest valid port" do
            unrefine <$> runReadM @String portReader "1" `shouldBe` Right 1

        it "Accepts the highest valid port" do
            unrefine <$> runReadM @String portReader "65535" `shouldBe` Right 65535

        it "Rejects port 0" do
            runReadM @String portReader "0" `shouldBe` Left "Failed reading: Expected a port between 1 and 65535 but got 0"

        it "Rejects a port just above the 16-bit range" do
            runReadM @String portReader "65536" `shouldBe` Left "Failed reading: Expected a port between 1 and 65535 but got 65536"

        -- Each of these would wrap around to a valid-looking port if parsed directly at a fixed-width type
        it "Rejects a port that would wrap around at Word16 (70000 -> 4464)" do
            runReadM @String portReader "70000" `shouldSatisfy` isLeft

        it "Rejects a port that would wrap around at Word (2^64 + 1 -> 1)" do
            runReadM @String portReader "18446744073709551617" `shouldSatisfy` isLeft

        for_ ["", "abc", "-1", " 6379"] \input ->
            it ("Rejects non-numeric input: " <> show input) do
                runReadM @String portReader input `shouldSatisfy` isLeft

        it "Rejects a port followed by trailing input" do
            runReadM @String portReader "6379abc" `shouldSatisfy` isLeft

spec_replicaof_reader :: Spec
spec_replicaof_reader = do
    describe "replicaOfReader" do
        for_ ["no one", "NO ONE", "No One"] \input ->
            it ("Reads " <> show input <> " case-insensitively as NO_ONE") do
                runReadM @String replicaOfReader input `shouldBe` Right NO_ONE

        it "Reads a hostname and port" do
            runReadM @String replicaOfReader "localhost 6379"
                `shouldBe` Right (HostPort (HostPortRec "localhost" ($$(refineTH 6379) :: Port)))

        it "Reads an IPv4 address and port" do
            runReadM @String replicaOfReader "127.0.0.1 6380"
                `shouldBe` Right (HostPort (HostPortRec "127.0.0.1" ($$(refineTH 6380) :: Port)))

        it "Allows multiple spaces between the host and port" do
            runReadM @String replicaOfReader "localhost    6379"
                `shouldBe` Right (HostPort (HostPortRec "localhost" ($$(refineTH 6379) :: Port)))

        -- Guards against the NO ONE branch partially matching and swallowing a real hostname
        it "Reads a hostname that starts with \"no\"" do
            runReadM @String replicaOfReader "nobody 6379"
                `shouldBe` Right (HostPort (HostPortRec "nobody" ($$(refineTH 6379) :: Port)))

        it "Rejects an out of range port" do
            runReadM @String replicaOfReader "localhost 70000" `shouldSatisfy` isLeft

        it "Rejects port 0" do
            runReadM @String replicaOfReader "localhost 0" `shouldSatisfy` isLeft

        it "Rejects a missing host" do
            runReadM @String replicaOfReader " 6379" `shouldBe` Left "Failed reading: Expected a hostname or IP address"

        it "Rejects a missing port" do
            runReadM @String replicaOfReader "localhost" `shouldSatisfy` isLeft

        it "Rejects the host:port form" do
            runReadM @String replicaOfReader "localhost:6379" `shouldSatisfy` isLeft

        it "Rejects empty input" do
            runReadM @String replicaOfReader "" `shouldSatisfy` isLeft

        it "Rejects trailing input after the port" do
            runReadM @String replicaOfReader "localhost 6379 extra" `shouldSatisfy` isLeft

        it "Rejects trailing input after NO ONE" do
            runReadM @String replicaOfReader "no one extra" `shouldSatisfy` isLeft
