{

    // ===== Contract Information ===== //
    // Name: MindHealer Event Reply Contract
    // Description: Contract for the event reply box controlling how the client's funds.
    //              A psychologist can initiate a client refund.
    //              A psychologist can initiate a reward claim for the event.
    //              
    // Version: 1.0.0
    // Author: Luca D'Angelo (ldgaetano@protonmail.com)

    // ===== Box Contents ===== //
    // Tokens
    // 1. (ReplyTokenId, 1)
    // 2. (EventPriceTokenId, EventPrice)
    //
    // Registers
    // R4: GroupElement                     clientGE
    // R5: (Coll[Byte], Coll[Byte])         (partnerLayerOneHash, partnerLayerTwoHash) // Empty Coll[Byte]() if not present.
    // R6: Coll[Byte]                       eventSingletonId

    // ===== Transactions ===== //
    // 1. Refund Event Tx
    // Inputs: Request, Reply, Expert
    // Data Inputs: None
    // Outputs: Request, Client, Expert
    // Context Variables: TxType
    //
    // 2. Claim Reward Tx
    // Inputs: Request, (Reply_1, ... , Reply_N), Expert
    // Data Inputs: None
    // Outputs: Expert, ((PartnerLayer1_1, PartnerLayer2_1), ... , (PartnerLayer1_N, PartnerLayer2_N)), MindHealerFee
    // Context Variables: TxType

    // ===== Compile Time Constants ($) ===== //
    // $mindHealerFeeAddressBytesHash: Coll[Byte]

    // ===== Context Variables (_) ===== //
    // _txType: Int

    // ===== Tx Type ===== //
    // 1: Refund Event Tx
    // 2: Claim Reward Tx


    // ===== Functions ===== //
    // def validToken: Box => Boolean
    // def validBoxTermination: Coll[Byte] => Boolean
    // def isSigmaPropEqualToBoxProp: (SigmaProp, Box) => Boolean
    
    def validToken(boxAndTokenId: (Box, Coll[Byte])): Boolean = { 

        val box: Box = boxAndTokenId._1
        val tokenId: Coll[Byte] = boxAndTokenId._2

        box.tokens.exists({ (token: (Coll[Byte], Long)) => {

            (token._1 == tokenId)

        }})

    }

    def validBoxTermination(boxAndTokenId: (Box, Coll[Byte])): Boolean = {

        val box: Box = boxAndTokenId._1
        val tokenId: Coll[Byte] = boxAndTokenId._2

        OUTPUTS.forall({ (output: Box) => {

            val validTokenBurn: Boolean = {

                output.tokens.forall({ (token: (Coll[Byte], Long)) => { 
                    
                    (token._1 != tokenId) 
                
                }})                

            }

            val validBoxDestruction: Boolean = {

                (output.propositionBytes != box.propositionBytes)

            }

            allOf(Coll(
                validTokenBurn,
                validBoxDestruction
            ))

        }})

    }

    def isSigmaPropEqualToBoxProp(propAndBox: (SigmaProp, Box)): Boolean = {

        val prop: SigmaProp = propAndBox._1
        val box: Box = propAndBox._2

        val propBytes: Coll[Byte] = prop.propBytes
        val treeBytes: Coll[Byte] = box.propositionBytes

        if (treeBytes(0) == 0) {

            (treeBytes == propBytes)

        } else {

            // offset = 1 + <number of VLQ encoded bytes to store propositionBytes.size>
            val offset = if (treeBytes.size > 127) 3 else 2
            (propBytes.slice(1, propBytes.size) == treeBytes.slice(offset, treeBytes.size))

        }

    }

    // ===== Variables ===== //
    val _txType: Option[Int] = getVar[Int](0)

    val replyTokenId: Coll[Byte] = SELF.tokens(0)._1
    val eventPriceTokenId: Coll[Byte] = SELF.tokens(1)._1
    val eventPrice: Long = SELF.tokens(1)._2
    val clientGE: GroupElement = SELF.R4[GroupElement].get
    val partners: (Coll[Byte], Coll[Byte]) = SELF.R5[(Coll[Byte], Coll[Byte])].get
    val partner1: Coll[Byte] = partners._1
    val partner2: Coll[Byte] = partners._2
    val eventSingletonId: Coll[Byte] = SELF.R6[Coll[Byte]].get

    val isPartner1: Boolean = (partner1.size == 32) // 32 bytes from hash output/
    val isPartner2: Boolean = (partner2.size == 32)
    val isPartners: Boolean = (isPartner1 || isPartner2)

    if (_txType.get == 1) {

        // ===== Refund Event Tx ===== //
        val validRefundEventTx: Boolean = {

            // Inputs
            val requestIn: Box = INPUTS(0)

            // Outputs
            val clientOut: Box = OUTPUTS(1)

            // Variables
            val boxAndReplyTokenId: (Box, Coll[Byte]) = (SELF, replyTokenId)

            val validRequest: Boolean = {

                (requestIn.tokens(0)._1 == eventSingletonId) &&
                (requestIn.tokens(1)._1 == replyTokenId)

            }

            val validClient: Boolean = {

                val propAndBox: (SigmaProp, Box) = (clientOut, proveDlog(clientGE))
                
                val validRefund: Boolean = (clientOut.tokens(0) == (eventPriceTokenId, eventPrice))

                isSigmaPropEqualToBoxProp(sigmaPropAndBox) &&
                validRefund

            }

            validRequest &&
            validClient &&
            validBoxTermination(boxAndReplyTokenId)
    
        }

        sigmaProp(validRefundEventTx)

    } else if (_txType.get == 2) {

    } else {
        sigmaProp(false)
    }

}