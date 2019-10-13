module Crypto
    ( CryptoError(..)
    , PrivateNumber
    , PublicNumber(..)
    , SharedKey
    , calculateFirstPublicKey
    , calculateNextPublicKey
    , getRandomPrivateNumber
    , makeSharedKey
    , emptySharedKey
    , encrypt
    , decrypt
    )
    where


import Crypto.Cipher.AES (AES256)
import Crypto.Cipher.Types (BlockCipher(..), Cipher(..), KeySizeSpecifier(..), IV, makeIV)
import Crypto.Error (CryptoFailable(..), CryptoError(..))
import Crypto.Number.Generate
import Crypto.Number.ModArithmetic
import Crypto.Number.Serialize
import Crypto.PubKey.DH
import Crypto.Random
import Data.ByteArray (ByteArray)
import Data.ByteString (ByteString)
import Data.IORef
import Data.Time.Clock
import Data.Time.Clock.POSIX
import System.IO.Unsafe


-- | Parameters for symmetric AES:
--

type ChatCipher = AES256


chatCipherSizeBytes :: Int
chatCipherSizeBytes = (\(KeySizeFixed ks) -> ks) $ cipherKeySize (undefined :: ChatCipher)


-- | Init vector for Cipher (raw)
--
-- Generating:
--   iv <- generateMax (2^256)
ivRaw :: Integer
ivRaw = 108893030028283452400203423681809397132491475841427997093242634712237857563903


-- | Init vector for Cipher
iv :: IV ChatCipher
iv = let bs = blockSize (undefined :: ChatCipher)
     in maybe (error "Wrong key size") id $ makeIV ((i2ospOf_ bs (ivRaw `rem` (2^(8 * bs))))::ByteString)


-- Parameters for DH:
--


-- | 'p' parameter.
--
--   Generating:
--   now <- {- get current time in nanosec -}
--   let s = seedFromInteger now
--       drg = drgNewSeed s
--       (p,_) = withDRG drg $ generateSafePrime 256
--
p :: Integer
p = 114123966413232644604196589138475607226709382029423570771018689092236598435427


-- | 'g' parameter. It must be prime. It is recommended less then 10.
g :: Integer
g = 5


calculateFirstPublicKey :: PrivateNumber -> PublicNumber
calculateFirstPublicKey (PrivateNumber a) = PublicNumber $ expSafe g a p


calculateNextPublicKey :: PublicNumber -> PrivateNumber -> PublicNumber
calculateNextPublicKey (PublicNumber prev) (PrivateNumber a) = PublicNumber $ expSafe prev a p


-- | Simple DRG
--
drgRef :: IORef ChaChaDRG
drgRef = unsafePerformIO $ do
    now <- (floor . (1e9 *) . utcTimeToPOSIXSeconds) <$> getCurrentTime
    let s = seedFromInteger now
        drg = drgNewSeed s
    newIORef drg
{-# NOINLINE drgRef #-}


getRandomPrivateNumber :: IO PrivateNumber
getRandomPrivateNumber = fmap PrivateNumber $ atomicModifyIORef drgRef $ \drg -> swap $ withDRG drg $ generateMax p
  where
    swap (a, b) = (b, a)


makeSharedKey :: PublicNumber -> SharedKey
makeSharedKey (PublicNumber pn) = SharedKey $ i2ospOf_ chatCipherSizeBytes pn


emptySharedKey :: SharedKey
emptySharedKey = SharedKey $ i2ospOf_ chatCipherSizeBytes 0


-- | (From Cryptonite demo) Initialize a block cipher
initCipher :: (BlockCipher c) => SharedKey -> Either CryptoError c
initCipher (SharedKey sk) = case cipherInit sk of
  CryptoFailed e -> Left e
  CryptoPassed a -> Right a


-- | (From Cryptonite demo)
encrypt' :: (BlockCipher c, ByteArray a) => SharedKey -> IV c -> a -> Either CryptoError a
encrypt' secretKey initIV msg =
  case initCipher secretKey of
    Left e -> Left e
    Right c -> Right $ ctrCombine c initIV msg


-- | (From Cryptonite demo)
decrypt' :: (BlockCipher c, ByteArray a) => SharedKey -> IV c -> a -> Either CryptoError a
decrypt' = encrypt'


encrypt :: SharedKey -> ByteString -> Either CryptoError ByteString
encrypt sharedKey message = encrypt' sharedKey iv message


decrypt :: SharedKey -> ByteString -> Either CryptoError ByteString
decrypt sharedKey message = decrypt' sharedKey iv message

