{

    // ===== Contract Information ===== //
    // Name: MindHealer Event Request Contract
    // Description: Contract for the event request box controlling how the client(s) and psychologist interact with each other during an event.
    //              A psychologist creates a request/intent for an event they want to host.
    //              Clients join the event by paying the event price.
    // Version: 1.0.0
    // Author: Luca D'Angelo (ldgaetano@protonmail.com)

    // ===== Box Contents ===== //
    // Tokens
    // 1. (EventTokenId, Long.MaxValue)
    //
    // Registers
    // R4: GroupElement                     (expertGE) // Psychologist address is initially the client address before the session is accepted.
    // R5: (Int, Int)                       (eventStartTimeBlockHeight, eventEndTimeBlockHeight)
    // R6: Long                             eventPrice
    // R7: Long                             clients // 0L initially.

    // ===== Transactions ===== //
    // 1. Join Event Tx
    // Inputs: Request, Client
    // Data Inputs: None
    // Outputs: Request, Reply 
    // Context Variables: TxType
    //
    // 2. Refund Event Tx
    // Inputs: Request, Reply, Expert
    // Data Inputs: None
    // Outputs: Request, Client, Expert
    // Context Variables: TxType
    //
    // 3. Claim Reward Tx
    // Inputs: Request, (Reply_1, ... , Reply_N), Expert
    // Data Inputs: None
    // Outputs: Expert, ((PartnerLayer1_1, PartnerLayer2_1), ... , (PartnerLayer1_N, PartnerLayer2_N)), MindHealerFee
    // Context Variables: TxType

    // ===== Compile Time Constants ($) ===== //
    // $mindHealerRegistrationTokenId: Coll[Byte]
    // $mindHealerFeeAddressBytesHash: Coll[Byte]
    // $mindHealerReplyContractErgoTreeBytesHash: Coll[Byte]
    // $eventPriceTokenId: Coll[Byte] // e.g. SigUSD

    // ===== Context Variables (_) ===== //
    // _txType: Int

    // ===== Tx Type ===== //
    // 1: Join Event Tx
    // 2: Refund Event Tx
    // 3: Claim Reward Tx

    // ===== Functions ===== //
    // def validToken: Box => Boolean
    // def validTermination: Coll[Byte] => Boolean
    // def isSigmaPropEqualToBoxProp: (SigmaProp, Box) => Boolean
    
    def validToken(boxAndTokenId: (Box, Coll[Byte])): Boolean = { 

        val box: Box = boxAndTokenId._1
        val tokenId: Coll[Byte] = boxAndTokenId._2

        box.tokens.exists({ (token: (Coll[Byte], Long)) => {

            (token._1 == tokenId)

        }})

    }

    def validTermination(tokenId: Coll[Byte]): Boolean = {

        OUTPUTS.forall({ (output: Box) => {

            val validTokenBurn: Boolean = {

                output.tokens.forall({ (token: (Coll[Byte], Long)) => { 
                    
                    (token._1 != tokenId) 
                
                }})                

            }

            val validBoxDestruction: Boolean = {

                (output.propositionBytes != SELF.propositionBytes)

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

    val eventTokenId: Coll[Byte] = SELF.tokens(0)._1
    val eventTokenAmount: Long = SELF.tokens(0)._2
    val expertGE: GroupElement = SELF.R4[GroupElement].get
    val expertSigmaProp: SigmaProp = proveDlog(expertGE)
    val eventTimes: (Int, Int) = SELF.R5[(Int, Int)].get
    val eventStartTimeBlockHeight: Int = eventTimes._1
    val eventEndTimeBlockHeight: Int = eventTimes._2
    val eventPrice: Long = SELF.R6[Long].get
    val clients: Long = SELF.R7[Long].get
    val totalAmount: Long = eventPrice * clients

    val claimBuffer: Int = 720  // The expert can claim only 24hrs after the event ends, since there is 1 block every 2 minutes on average, there are 720 blocks every 24hrs on average.

    val isEventAccepted: Boolean = (clients > 0L)
    val isEventStarted: Boolean = (CONTEXT.HEIGHT > eventStartTimeBlockHeight)
    val isEventOver: Boolean = (CONTEXT.HEIGHT > eventEndTimeBlockHeight)
    val isEventClaim: Boolean = (CONTEXT.HEIGHT > eventEndTimeBlockHeight + claimBuffer)

    if (_txType.get == 1) {

        // ===== Join Event Tx ===== //
        val validJoinEventTx: Boolean = {

            // Outputs
            val requestOut: Box = OUTPUTS(0)
            val replyOut: Box = OUTPUTS(1)

            val validRequestOut: Boolean = {

                val validClientIncrement: Boolean = (requestOut.R7[Long].get == clients + 1L)

                val validRecreation: Boolean = {

                    allOf(Coll(
                        (requestOut.value == SELF.value),
                        (requestOut.propositionBytes == SELF.propositionBytes),
                        (requestOut.tokens(0)._1 == eventTokenId),
                        (requestOut.R4[GroupElement].get == expertGE),
                        (requestOut.R5[(Int, Int)].get == eventTimes),
                        (requestOut.R6[Long].get == eventPrice)
                    ))

                }

                validClientIncrement &&
                validRecreation

            }

            val validReplyOut: Boolean = {

                val validContract: Boolean = (blake2b256(replyOut.propositionBytes) == $mindHealerReplyContractErgoTreeBytesHash)

                val validEvenTokenTransfer: Boolean = {
                    (replyOut.tokens(0) == (eventTokenId, 1L)) && 
                    (requestOut.tokens(0)._2 == eventTokenAmount - 1L)
                }

                val validClientPayment: Boolean = (replyOut.tokens(1) == ($eventPriceTokenId, eventPrice))

                validContract &&
                validEvenTokenTransfer &&
                validClientPayment

            }

            validRequestOut &&
            validReplyOut

        }

        sigmaProp(validJoinEventTx)
    
    } else {
        sigmaProp(false)
    }

}