a = Tx {txInputs = fromList [
    TxInOf {txInRef = TxOutRefOf {txOutRefId = TxIdOf {getTxId = c64870f94c546a69c4c08999f876ca9881af61537e73a4cea788fb914856547d}, txOutRefIdx = 0},
    txInType = ConsumeScriptAddress ValidatorScript { <script> } RedeemerScript { <script> }},
    TxInOf {txInRef = TxOutRefOf {txOutRefId = TxIdOf {getTxId = f2c0f6000559f1b2e9cbfcaea01f90800211b00f314421c247fb3f3aa0fdafa4}, txOutRefIdx = 1},
    txInType = ConsumePublicKeyAddress (PubKey {getPubKey = fc51cd8e6218a1a38da47ed00230f0580816ed13ba3303ac5deb911548908025})}],
    txOutputs = [
        TxOutOf {txOutAddress = AddressOf {getAddress = ec603d619609b69e61f167314d2bb3113cb5147286e14c12d48f4a4afc043898},
            txOutValue = Value {getValue = Map {unMap = [(CurrencySymbol 0,932)]}},
            txOutType = PayToScript DataScript { <script> }},
        TxOutOf {txOutAddress = AddressOf {getAddress = 03d200a81ee0feace8fb845e5ec950a6f9add83709244f7b81134654139f41a4},
            txOutValue = Value {getValue = Map {unMap = [(CurrencySymbol 0,999080)]}},
            txOutType = PayToPubKey (PubKey {getPubKey = fc51cd8e6218a1a38da47ed00230f0580816ed13ba3303ac5deb911548908025})}], txForge = Value {getValue = Map {unMap = []}}, txFee = Ada {getAda = 0}, txValidRange = Interval {ivFrom = Just (Slot {getSlot = 2}), ivTo = Nothing}, txSignatures = fromList [(PubKey {getPubKey = fc51cd8e6218a1a38da47ed00230f0580816ed13ba3303ac5deb911548908025},Signature {getSignature = SizedByteString {unSizedByteString = "u\251\157W\239U\245\150\219\149y\142DGn\190\171/\159;lbD\129\163\DC4\229\150\"5I\153\217\155\SUB\172\n\175;\CAN4\177\136\198\155*^\215\164Z!\152r\153p\227\245Q\DELte\216\184\v"}})]}