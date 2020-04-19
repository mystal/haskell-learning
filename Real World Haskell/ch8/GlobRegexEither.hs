-- file: ch08/GlobRegexEither.hs
type GlobError = String

-- Can now return an error message OR a valid regex
globToRegex :: String -> Either GlobError String
