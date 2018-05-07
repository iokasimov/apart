import System.IO (BufferMode(..), hSetBuffering, stdout, stderr)

import Test.Apart.Structures.Graph
import Test.Apart.Structures.Stack
import Test.Apart.Structures.Stream

main = do
    hSetBuffering stdout LineBuffering
    hSetBuffering stderr LineBuffering
