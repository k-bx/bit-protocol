# bit-protocol

A package suitable for binary protocols defined in a manner where you
have bit counts not aligned by 8.

For example, if you have a protocol for sending user profiles saying:

> The value sent must be a base64url-encoded string consisting of four
> values:
> - 6 bits representing user's age
> - 7 bits for their favorite number
> - 5 bits for their lucky number
> - 6 bits for a random number

you could use the library as follows:

```haskell
import Data.BitProtocol
import Data.ByteString.Base64.URL (encode)

main :: IO ()
main = do
  let age = 29
      fav = 12
      lucky = 13
      rand = 14
  -- the number in protocol should be base64url(011101_0001100_01101_001110)
  print $ encode $ encodeBS8 $ [BitsVal 6 age, BitsVal 7 fav, BitsVal 5 lucky, BitsVal 6 rand]
  -- will output "dGNO"
  -- which is the same as `encode (BC8.pack (map chr [0b01110100, 0b01100011, 0b01001110]))`
```

Parsing can be done like this:

```haskell
  let (Right bs) = decode "dGNO"
  let (xs, BitsVal 0 0, "") = parseBS8 [6, 7, 5, 6] bs
  print xs
-- Will output:
--   ( [BitsVal 6 29, BitsVal 7 12, BitsVal 5 13, BitsVal 6 14]
--   , BitsVal 0 0
--   , "")
```

Warning! Does not support negative numbers.

TODO:

- [ ] use shift operations instead of division in more places
- [ ] add a performance test
- [ ] consider adding checks upon arithmetic overflows (if you use
      `Int` in quickcheck test you'll quickly find some)
