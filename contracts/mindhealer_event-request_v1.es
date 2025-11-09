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
    //  1. (EventTokenId, 1)
    //  2. (ReplyTokenId, Long.MaxValue)
    //
    // Registers
    // R4: GroupElement                     expertGE // Psychologist address is initially the client address before the session is accepted.
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
    // def validBoxTermination: Coll[Byte] => Boolean
    // def isSigmaPropEqualToBoxProp: (SigmaProp, Box) => Boolean
    // def isSameTokenAmount: (Coll[Byte], Long) => Boolean
    
    def validToken(boxAndTokenId: (Box, Coll[Byte])): Boolean = { 

        val box: Box = boxAndTokenId._1
        val tokenId: Coll[Byte] = boxAndTokenId._2

        box.tokens.exists({ (token: (Coll[Byte], Long)) => {

            (token._1 == tokenId)

        }})

    }

    def validBoxTermination(boxAndTokenId: (Box, Coll[Byte])): Boolean = { // Split burn and spending into different functions.

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

    def isSameTokenAmount(tokenIdAndOffset: (Coll[Byte], Long)): Boolean = {

        val tokenId: Coll[Byte] = tokenIdAndOffset._1
        val offset: Long        = tokenIdAndOffset._2

        // 1. For each box, get the token with input id.
        // 2. Calculate the cumulative amount.
        val inAmount: Long = INTPUTS.fold(0L, { (input: Box) =>  
            
            val tokens: Coll[(Coll[Byte], Long)] = input.tokens.filter({ (token: (Coll[Byte, Long])) => token._1 == tokenId })

            val amount = tokens.fold(0L, { (acc: Long, token: (Coll[Byte], Long)) => token._2 + acc })

            amount

        })

        val outAmount: Long = OUTPUTS.fold(0L, { (output: Box) =>  
            
            val tokens: Coll[(Coll[Byte], Long)] = output.tokens.filter({ (token: (Coll[Byte, Long])) => token._1 == tokenId })

            val amount = tokens.fold(0L, { (acc: Long, token: (Coll[Byte], Long)) => token._2 + acc })

            amount

        })

        val delta: Long = outAmount - inAmount
        
        if (delta < 0) { // Burn: Less tokens in outputs.
            -1 * delta == offset
        } else { // Mint: More tokens in outputs.
            delta == offset
        }

    }

    // ===== Variables ===== //
    val _txType: Option[Int] = getVar[Int](0)

    val eventTokenId: Coll[Byte] = SELF.tokens(0)._1
    val replyTokenId: Coll[Byte] = SELF.tokens(1)._1
    val replyTokenAmount: Long   = SELF.tokens(1)._2 
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

            val validRequest: Boolean = {

                val validClientIncrement: Boolean = (requestOut.R7[Long].get == clients + 1L)

                val validRecreation: Boolean = {

                    (requestOut.value == SELF.value) &&
                    (requestOut.propositionBytes == SELF.propositionBytes) &&
                    (requestOut.tokens(0)._1 = eventTokenId) &&
                    (requestOut.tokens(1)._1 == replyTokenId) &&
                    (requestOut.R4[GroupElement].get == expertGE) &&
                    (requestOut.R5[(Int, Int)].get == eventTimes) &&
                    (requestOut.R6[Long].get == eventPrice)

                }

                validClientIncrement &&
                validRecreation

            }

            val validReply: Boolean = {

                val validContract: Boolean = (blake2b256(replyOut.propositionBytes) == $mindHealerReplyContractErgoTreeBytesHash)

                val validEvenTokenTransfer: Boolean = {
                    (replyOut.tokens(0) == (replyTokenId, 1L)) && 
                    (requestOut.tokens(0)._2 == replyTokenAmount - 1L)
                }

                val validClientPayment: Boolean = (replyOut.tokens(1) == ($eventPriceTokenId, eventPrice))

                validContract &&
                validEvenTokenTransfer &&
                validClientPayment

            }

            validRequest &&
            validReply

        }

        sigmaProp(validJoinEventTx)
    
    } else if (_txType.get == 2) {

        // ===== Refund Event Tx ===== //
        val validRefundEventTx: Boolean = {

            // Inputs
            val replyIn: Box = INPUTS(1)
            val expertIn: Box = INPUTS(2)

            // Outputs
            val expertOut: Box = OUTPUTS(2)

            val validReply: Boolean = {

                val boxAndTokenId: (Box, Coll[Byte]) = (replyIn, replyTokenId)

                val validErgoTree: Boolean = (blake2b256(replyIn.propositionBytes) == $mindHealerReplyContractErgoTreeBytesHash)

                // Leave remaining checks for the reply box itself.
                validErgoTree &&
                validToken(boxAndTokenId)

            }

            val validExpert: Boolean = {

                val boxAndRegistrationIn: (Box, Coll[Byte]) = (expertIn, $mindHealerRegistrationTokenId)
                val propAndBoxIn: (SigmaProp, Box) = (expertIn, expertSigmaProp)
                
                val boxAndRegistrationOut: (Box, Coll[Byte]) = (expertOut, $mindHealerRegistrationTokenId)
                val propAndBoxOut: (SigmaProp, Box) = (expertOut, expertSigmaProp)
                
                isSigmaPropEqualToBoxProp(propAndBoxIn) &&
                validToken(boxAndRegistrationIn) &&
                isSigmaPropEqualToBoxProp(propAndBoxOut) &&
                validToken(boxAndRegistrationOut)

            }

            val validRequest: Boolean = {

                val tokenIdAndOffset: (Coll[Byte], Long) = (replyTokenId, 1L)

                val validClientDecrement: Boolean = (requestOut.R7[Long].get == clients - 1L) // Remove the client from the count.                
                val validReplyTokenBurn: Boolean = isSameTokenAmount(tokenIdAndOffset) // Burn the reply token instead of returning to the request box.
                
                val validRecreation: Boolean = {

                    (requestOut.value == SELF.value) &&
                    (requestOut.propositionBytes == SELF.propositionBytes) &&
                    (requestOut.tokens(0)._1 == eventTokenId) &&
                    (requestOut.tokens(1) == SELF.tokens(1)) && // Amount of reply tokens in the request box does not change.
                    (requestOut.R4[GroupElement].get == expertGE) &&
                    (requestOut.R5[(Int, Int)].get == eventTimes) &&
                    (requestOut.R6[Long].get == eventPrice)

                }

                validClientDecrement &&
                validReplyTokenBurn &&
                validRecreation

            }

            validReply &&
            validClient &&
            validExpert &&
            validRequest

        }

        sigmaProp(validRefundEventTx)

    } else if (_txType.get == 3) {

        // ===== Claim Reward Tx ===== //
        val validClaimRewardTx: Boolean = {

            // Inputs
            val repliesIn: Coll[Box] = INPUTS.slice(1, INPUTS.size-1)
            val expertIn: Box = INPUTS(INPUTS.size-1)

            // Outputs
            val expertOut: Box = OUTPUTS(0)
            val mindHealerFee: Box = OUTPUTS(1)

            val validReplies: Boolean = {

                repliesIn.forall({ (replyIn: Box) =>  

                    val boxAndTokenId: (Box, Coll[Byte]) = (replyIn, eventTokenId)

                    val validErgoTree: Boolean = (blake2b256(replyIn.propositionBytes) == $mindHealerReplyContractErgoTreeBytesHash)

                    validErgoTree &&
                    validToken(boxAndTokenId)

                })
            
            }

            val validExpert: Boolean = {

                val propAndBoxIn: (SigmaProp, Box) = (expertIn, expertSigmaProp)
                val boxAndRegistrationIn: (Box, Coll[Byte]) = (expertIn, $mindHealerRegistrationTokenId)
                val expertAmount: Long = (80 * clients * eventPrice) / 100L

                val validRegistrationToken: Boolean = (expertOut.tokens(0)._1 == $mindHealerRegistrationTokenId)
                val validPaymentToken: Boolean = (expertOut.tokens(1) == ($eventPriceTokenId, expertAmount))
                
                isSigmaPropEqualToBoxProp(propAndBoxIn) &&
                validToken(boxAndRegistrationIn) &&
                isSigmaPropEqualToBoxProp(propAndBoxOut) &&
                validRegistrationToken &&
                validPaymentToken

            }

            val validRequest: Boolean = {

                val boxAndEventTokenId: (Box, Coll[Byte]) = (SELF, eventTokenId)
                val boxAndReplyTokenId: (Box, Coll[Byte]) = (SELF, replyTokenId)

                validBoxTermination(boxAndEventTokenId)
                validBoxTermination(boxAndReplyTokenId)

            }

            validReplies &&
            validExpert &&
            validMindHealerFee &&
            validRequest

        }

        sigmaProp(validClaimRewardTx)

    } else {
        sigmaProp(false)
    }

}