module Blake2b exposing (blake2b224, blake2b256, blake2b512)

import Bitwise
import Blake2b.Int128 as Int128 exposing (Int128(..))
import Blake2b.UInt64 as UInt64
import List.Extra
import List.ExtraBis
import UInt64 exposing (UInt64)


blake2b224 : Maybe (List Int) -> List Int -> List Int
blake2b224 mKey =
    blake2b mKey (UInt64.fromInt 28)


blake2b256 : Maybe (List Int) -> List Int -> List Int
blake2b256 mKey =
    blake2b mKey (UInt64.fromInt 32)


blake2b512 : Maybe (List Int) -> List Int -> List Int
blake2b512 mKey =
    blake2b mKey (UInt64.fromInt 64)


{-| BLAKE2-b hash function.

Key and data input are split and padded into "dd" message blocks
d[0..dd-1], each consisting of 16 words (or "bb" bytes).

If a secret key is used (kk > 0), it is padded with zero bytes and
set as d[0]. Otherwise, d[0] is the first data block. The final
data block d[dd-1] is also padded with zero to "bb" bytes (16 words).

The number of blocks is therefore dd = ceil(kk / bb) + ceil(ll / bb).
However, in the special case of an unkeyed empty message (kk = 0 and
ll = 0), we still set dd = 1 and d[0] consists of all zeros.

The following procedure processes the padded data blocks into an
"nn"-byte final hash value.

-}
blake2b : Maybe (List Int) -> UInt64 -> List Int -> List Int
blake2b mKey hashBytesLength input =
    let
        ll =
            List.length input

        endPad =
            List.repeat (modBy 128 (128 - modBy 128 ll)) 0x00

        ( preprocessedInput, kk ) =
            case ( mKey, input ) of
                ( Nothing, [] ) ->
                    ( List.repeat 128 0x00, UInt64.zero )

                ( Nothing, _ ) ->
                    ( input ++ endPad, UInt64.zero )

                ( Just key, _ ) ->
                    let
                        keyLen =
                            List.length key

                        keyBlock =
                            key ++ List.repeat (128 - keyLen) 0x00
                    in
                    ( List.concat [ keyBlock, input, endPad ], UInt64.fromInt keyLen )

        -- Make 128 bytes input blocks ("d[0..dd-1]")
        inputBlocks =
            List.ExtraBis.chunksOf 128 preprocessedInput

        -- Initialize the state ("h")
        initialState =
            blake2bIV
                |> List.Extra.updateAt 0
                    (\w ->
                        UInt64.xor w (UInt64.fromInt 0x01010000)
                            |> UInt64.xor (UInt64.shiftLeftBy 8 kk)
                            |> UInt64.xor hashBytesLength
                    )

        -- Process padded key and data blocks (except the last one)
        updatedState =
            List.take (List.length inputBlocks - 1) inputBlocks
                |> List.foldl
                    (\block ( state, i ) ->
                        let
                            ctr =
                                Int128.mul (UInt64.add i UInt64.one) (UInt64.fromInt 128)
                        in
                        ( compress state block ctr False
                        , UInt64.add i UInt64.one
                        )
                    )
                    ( initialState, UInt64.zero )
                |> Tuple.first

        -- Process final block
        lastBlock =
            List.Extra.last inputBlocks |> Maybe.withDefault []

        lastCtr =
            case UInt64.compare kk UInt64.zero of
                EQ ->
                    Int128 UInt64.zero (UInt64.fromInt ll)

                _ ->
                    Int128 UInt64.zero (UInt64.fromInt <| ll + 128)

        -- bb = 128 bytes
    in
    compress updatedState lastBlock lastCtr True
        |> List.concatMap (UInt64.toBigEndianBytes >> List.reverse)
        |> List.ExtraBis.take64 hashBytesLength


{-| Compression function takes the state vector ("h" in spec), message block
vector ("m" in spec), a 2w-bit (128-bit) offset counter ("t" in spec), and final
block indicator flag ("f" in spec), and returns the new state vector.
-}
compress : List UInt64 -> List Int -> Int128 -> Bool -> List UInt64
compress state block (Int128 ctrHigh ctrLow) finalBlockFlag =
    let
        -- Pad the block and convert to U64 words
        blockWords : List UInt64
        blockWords =
            (block ++ List.repeat (128 - List.length block) 0x00)
                |> List.ExtraBis.chunksOf 8
                |> List.map UInt64.fromLittleEndianBytes

        -- Initialize local vector with state and IV
        vInit : List UInt64
        vInit =
            (state ++ blake2bIV)
                |> List.indexedMap
                    (\i word ->
                        if i == 12 then
                            UInt64.xor word ctrLow

                        else if i == 13 then
                            UInt64.xor word ctrHigh

                        else if i == 14 then
                            if finalBlockFlag then
                                UInt64.xor word UInt64.maxValue

                            else
                                word

                        else
                            word
                    )

        -- Cryptographic mixing step
        sigmaMixingStep : Quadruple64 -> UInt64 -> UInt64 -> List UInt64 -> List UInt64
        sigmaMixingStep { a, b, c, d } si sj v =
            let
                x =
                    blockWords |> List.ExtraBis.get64 si |> Maybe.withDefault UInt64.zero

                y =
                    blockWords |> List.ExtraBis.get64 sj |> Maybe.withDefault UInt64.zero

                va =
                    List.ExtraBis.get64 a v |> Maybe.withDefault UInt64.zero

                vb =
                    List.ExtraBis.get64 b v |> Maybe.withDefault UInt64.zero

                vc =
                    List.ExtraBis.get64 c v |> Maybe.withDefault UInt64.zero

                vd =
                    List.ExtraBis.get64 d v |> Maybe.withDefault UInt64.zero

                -- { vaNew, vbNew, vcNew, vdNew } =
                newVsQuad =
                    mixing va vb vc vd x y
            in
            v
                |> List.ExtraBis.indexedMap64
                    (\i v0 ->
                        if i == a then
                            newVsQuad.a

                        else if i == b then
                            newVsQuad.b

                        else if i == c then
                            newVsQuad.c

                        else if i == d then
                            newVsQuad.d

                        else
                            v0
                    )

        -- Cryptographic mixing round
        sigmaMixingRound : Int -> List UInt64 -> List UInt64
        sigmaMixingRound round v =
            let
                s =
                    sigmaRound round
            in
            sigmaMixingStep (q64FromInts 0 4 8 12) s.i00 s.i01 v
                |> sigmaMixingStep (q64FromInts 1 5 9 13) s.i02 s.i03
                |> sigmaMixingStep (q64FromInts 2 6 10 14) s.i04 s.i05
                |> sigmaMixingStep (q64FromInts 3 7 11 15) s.i06 s.i07
                |> sigmaMixingStep (q64FromInts 0 5 10 15) s.i08 s.i09
                |> sigmaMixingStep (q64FromInts 1 6 11 12) s.i10 s.i11
                |> sigmaMixingStep (q64FromInts 2 7 8 13) s.i12 s.i13
                |> sigmaMixingStep (q64FromInts 3 4 9 14) s.i14 s.i15

        -- Apply 12 mixing rounds to the local vector
        newV =
            sigmaMixingRound 0 vInit
                |> sigmaMixingRound 1
                |> sigmaMixingRound 2
                |> sigmaMixingRound 3
                |> sigmaMixingRound 4
                |> sigmaMixingRound 5
                |> sigmaMixingRound 6
                |> sigmaMixingRound 7
                |> sigmaMixingRound 8
                |> sigmaMixingRound 9
                |> sigmaMixingRound 10
                |> sigmaMixingRound 11
    in
    -- XOR the two halves of v
    List.map3
        (\hi vi vii ->
            UInt64.xor hi vi |> UInt64.xor vii
        )
        state
        (List.ExtraBis.sublist 0 8 newV)
        (List.ExtraBis.sublist 8 8 newV)


blake2bIV : List UInt64
blake2bIV =
    [ UInt64.fromInt24s 0x6A09 0x00E667F3 0x00BCC908
    , UInt64.fromInt24s 0xBB67 0x00AE8584 0x00CAA73B
    , UInt64.fromInt24s 0x3C6E 0x00F372FE 0x0094F82B
    , UInt64.fromInt24s 0xA54F 0x00F53A5F 0x001D36F1
    , UInt64.fromInt24s 0x510E 0x00527FAD 0x00E682D1
    , UInt64.fromInt24s 0x9B05 0x00688C2B 0x003E6C1F
    , UInt64.fromInt24s 0x1F83 0x00D9ABFB 0x0041BD6B
    , UInt64.fromInt24s 0x5BE0 0x00CD1913 0x007E2179
    ]


type alias Quadruple64 =
    { a : UInt64
    , b : UInt64
    , c : UInt64
    , d : UInt64
    }


q64FromInts : Int -> Int -> Int -> Int -> Quadruple64
q64FromInts a b c d =
    Quadruple64 (UInt64.fromInt a) (UInt64.fromInt b) (UInt64.fromInt c) (UInt64.fromInt d)


{-| The mixing function mixes two words x, y with four words v[a], v[b], v[c],
v[d] and returns these four words modified. The vector v is the working vector
of the blake algorithm.

mixing va, vb, vc, vd, x, y -> new (va, vb, vc, vd)

-}
mixing : UInt64 -> UInt64 -> UInt64 -> UInt64 -> UInt64 -> UInt64 -> Quadruple64
mixing va vb vc vd x y =
    let
        vaTemp =
            UInt64.add va vb |> UInt64.add x

        -- R1 = 32
        vdTemp =
            UInt64.xor vd vaTemp |> UInt64.rotateRightBy 32

        vcTemp =
            UInt64.add vc vdTemp

        -- R2 = 24
        vbTemp =
            UInt64.xor vb vcTemp |> UInt64.rotateRightBy 24

        vaNew =
            UInt64.add vaTemp vbTemp |> UInt64.add y

        -- R3 = 16
        vdNew =
            UInt64.xor vdTemp vaNew |> UInt64.rotateRightBy 16

        vcNew =
            UInt64.add vcTemp vdNew

        -- R4 = 63
        vbNew =
            UInt64.xor vbTemp vcNew |> UInt64.rotateRightBy 63
    in
    Quadruple64 vaNew vbNew vcNew vdNew


type alias SigmaRound =
    { i00 : UInt64
    , i01 : UInt64
    , i02 : UInt64
    , i03 : UInt64
    , i04 : UInt64
    , i05 : UInt64
    , i06 : UInt64
    , i07 : UInt64
    , i08 : UInt64
    , i09 : UInt64
    , i10 : UInt64
    , i11 : UInt64
    , i12 : UInt64
    , i13 : UInt64
    , i14 : UInt64
    , i15 : UInt64
    }


intsToSigmaRound : Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> SigmaRound
intsToSigmaRound i00 i01 i02 i03 i04 i05 i06 i07 i08 i09 i10 i11 i12 i13 i14 i15 =
    SigmaRound
        (UInt64.fromInt i00)
        (UInt64.fromInt i01)
        (UInt64.fromInt i02)
        (UInt64.fromInt i03)
        (UInt64.fromInt i04)
        (UInt64.fromInt i05)
        (UInt64.fromInt i06)
        (UInt64.fromInt i07)
        (UInt64.fromInt i08)
        (UInt64.fromInt i09)
        (UInt64.fromInt i10)
        (UInt64.fromInt i11)
        (UInt64.fromInt i12)
        (UInt64.fromInt i13)
        (UInt64.fromInt i14)
        (UInt64.fromInt i15)



-- Selection permutation for one mixing round


sigmaRound : Int -> SigmaRound
sigmaRound round =
    case round |> Bitwise.shiftRightZfBy 0 of
        0 ->
            intsToSigmaRound 0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15

        1 ->
            intsToSigmaRound 14 10 4 8 9 15 13 6 1 12 0 2 11 7 5 3

        2 ->
            intsToSigmaRound 11 8 12 0 5 2 15 13 10 14 3 6 7 1 9 4

        3 ->
            intsToSigmaRound 7 9 3 1 13 12 11 14 2 6 5 10 4 0 15 8

        4 ->
            intsToSigmaRound 9 0 5 7 2 4 10 15 14 1 11 12 6 8 3 13

        5 ->
            intsToSigmaRound 2 12 6 10 0 11 8 3 4 13 7 5 15 14 1 9

        6 ->
            intsToSigmaRound 12 5 1 15 14 13 4 10 0 7 6 3 9 2 8 11

        7 ->
            intsToSigmaRound 13 11 7 14 12 1 3 9 5 0 15 4 8 6 2 10

        8 ->
            intsToSigmaRound 6 15 14 9 11 3 0 8 12 2 13 7 1 4 10 5

        9 ->
            intsToSigmaRound 10 2 8 4 7 6 1 5 15 11 9 14 3 12 13 0

        _ ->
            sigmaRound (modBy 10 round)
