-- file: ch08/ElfMagic.hs
import qualified Data.ByteString.Lazy as L
-- qualified allows us to import things with a different name
-- ByteString is a binary representation of String
-- can be strict or lazy

hasElfMagic :: L.ByteString -> Bool
hasElfMagic content = L.take 4 content == elfMagic
    where elfMagic = L.pack [0x7f, 0x45, 0x4c, 0x46]

isElfFile :: FilePath -> IO Bool
isElfFile path = do
    content <- L.readFile path
    return (hasElfMagic content)
