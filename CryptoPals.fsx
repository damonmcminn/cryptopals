open System

module Hex =
  let hexadecimalToInt hex = Convert.ToInt32(hex, 16)
  let stringToByteArray (str:string) =
    str
    |> Seq.chunkBySize 2
    |> Seq.map (String.Concat >> hexadecimalToInt >> byte)
    |> Seq.toArray

module ByteArray =
  let toUTF8 bytes = Text.Encoding.UTF8.GetString bytes
  let toBase64 bytes = Convert.ToBase64String bytes
  let toHex bytes =
    bytes
    |> BitConverter.ToString
    |> fun hex -> hex.Split('-')
    |> String.Concat

module Text =
  let calcObservedCoincidences (text:string) =
    text
    |> Seq.filter Char.IsLetter
    |> Seq.countBy id
    |> Seq.map (fun (_, count) -> float (count * (count - 1)))
    |> Seq.sum

  /// Algorithm from here: http://www.thonky.com/kryptos/index-of-coincidence
  let indexOfCoincidence text =
    let observedCoincidences = calcObservedCoincidences text
    let totalLetters = float text.Length
    let probabilityOfCoincidencesInRandomText  = 0.0385
    let randomCoincidences = probabilityOfCoincidencesInRandomText * totalLetters * (totalLetters - 1.0)

    observedCoincidences / randomCoincidences

  let isEnglish text =
    let ic = indexOfCoincidence text
    ic >= 1.5 && ic <= 2.0

module Util =
  let fixedXOR b1 b2 =
    Seq.zip b1 b2
    |> Seq.map (fun (a, b) -> a ^^^ b)
    |> Seq.toArray

// let input = "1b37373331363f78151b7f2b783431333d78397828372d363c78373e783a393b3736" |> Hex.stringToByteArray
// let result =
//   ['a' .. 'z'] @ ['A' .. 'Z']
//   |> List.map ((fun c -> Seq.map (fun _ -> byte c) [| 1 .. input.Length |]) >> Seq.toArray)
//   |> List.map (Array.zip input)
//   // |> List.map (fun (a, b) -> 1)

// printf "%A" input

// let a = "1c0111001f010100061a024b53535009181c"
// let b = "686974207468652062756c6c277320657965"
// Util.fixedXOR (Hex.stringToByteArray a) (Hex.stringToByteArray b) |> ByteArray.toHex

let singleByte = "1b37373331363f78151b7f2b783431333d78397828372d363c78373e783a393b3736" |> Hex.stringToByteArray
// let letters =
//   ['a' .. 'z'] @ ['A' .. 'Z']
//   |> List.map (byte >> (Array.create singleByte.Length) >> (Util.fixedXOR singleByte) >> ByteArray.toUTF8)
//   |> List.filter Text.isEnglish
//   |> List.head

let r = "Cooking MC's like a pound of bacon" |> Text.indexOfCoincidence