import Stubby (stubby, wait)
import Stubby.CLI.Settings (argParser)
import Options.Applicative (execParser)

main :: IO ()
main = execParser argParser >>= stubby >>= wait
