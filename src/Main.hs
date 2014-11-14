import Stubby (stubby)
import CLIArgs (opts)
import Options.Applicative (execParser)

main :: IO ()
main = execParser opts >>= stubby
