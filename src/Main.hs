import Stubby (stubby)
import CLI.Arguments (opts)
import Options.Applicative (execParser)

main :: IO ()
main = execParser opts >>= stubby
