import Test.DocTest
main = doctest ["-ddump-splices", "src/BACnet/Reader/Core.hs",
  "src/BACnet/Prim.hs", "src/BACnet/Writer/Core.hs",
  "src/BACnet/Tag/Core.hs", "src/BACnet/Tag/Reader.hs",
  "src/BACnet/Tag.hs", "src/BACnet/Writer.hs"]
