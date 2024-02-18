{

    // ===== Contract Information ===== //
    // Name: PsyWORKshop Session Contract
    // Description: Contract for the session box.
    // Version: 1.0.0
    // Author: Luca D'Angelo

    // ===== Box Contents ===== //
    // Tokens
    // 1. (SessionSingletonId, 1)
    // 2. (SigUSDId, SessionPrice)
    // 3. (SigUSDId, Collateral) // If provided by the psychologist.
    // Registers
    // R4: (Boolean, Coll[Byte])    (isSessionAccepted, psychologistAddressBytes)
    // R5: Int                      sessionStartTimeBlockHeight
    // R6: Coll[Byte]               clientAddressBytes

    // ===== Relevant Transactions ===== //
    // 1. Accept Session Tx
    // Inputs: Session, PsychologistPK
    // Data Inputs: None
    // Outputs: Session
    // Context Variables: TxType
    // 2. Cancel Session Tx
    // Inputs: Session, PsychologistPK
    // Data Inputs: None
    // Outputs: ClientPK, PsychologistPK, PsyWorkshopFee
    // Context Variables: TxType

    // ===== Compile Time Constants ($) ===== //
    // $psyworkshopRegistrationTokenId: Coll[Byte]
    // $psyworkshopFeeAddress: Coll[Byte]

    // ===== Context Variables (_) ===== //
    // _txType: Byte

    // ===== Tx Types ===== //
    // 1: Accept Session Tx
    // 2: Cancel Session Tx
    // 3: Unaccepted Session Tx

    // ===== Relevant Variables ===== //
    val _txType: Option[Byte] = getVar[Byte](0)
    val SessionSingletonId: Coll[Byte] = SELF.tokens(0)._1
    val sessionPriceTokenId: Coll[Byte] = SELF.tokens(1)._1
    val sessionPrice: Long = SELF.tokens(1)._1
    val sessionStartTimeBlockHeight: Int = SELF.R5[Int].get
    val clientAddress: Coll[Byte] = SELF.R6[Coll[Byte]].get
    val sesssionCancelationPeriod: Int = 720 // The cancelation period is 24hrs, thus since there is 1 block every 2 minutes on average, there are 720 blocks every 24hrs on average.
    val sessionUnacceptedPeriod: Int = 60 // If no psychologist accepts the session within 2hrs of the session start time, thus since there is 1 block every 2 minutes on average, there are 60 blocks every 2hrs on average.

    if (_txType.get == 1.toByte) {

        // ===== Accept Session Tx ===== //
        val validAcceptSessionTx: Boolean = {

            // Inputs
            val psychologistBoxesIn: Box = INPUTS.slice(1, INPUTS.size)
            val psychologistAddressBytes: Coll[Byte] = psychologistBoxesIn(0).propositionBytes

            // Outputs
            val sessionBoxOut: Box = OUTPUTS(0)

            val validPsychologist: Boolean = { // TODO: May need to use AVL trees for this instead.

                psychologistBoxesIn.exists( (box: Box) => {

                    val validAddress: Boolean = {

                        (box.propositionBytes == psychologistAddress)

                    }
        
                    val validToken: Boolean = { 

                        box.tokens.exists( (token: (Coll[Byte], Long)) => {

                            (token._1 == $psyworkshopRegistrationTokenId)

                        })

                    }

                    allOf(Coll(
                        validAddress,
                        validToken
                    ))
        
                })

            }

            val validCollateralTransfer: Boolean = {

                val collateral: Long = sessionBoxOut.tokens(3)

                val validSigUSD: Boolean = (collateral._1 == sessionPriceTokenId)
                val validCollateral: Boolean = (100L * collateral._2 == 10L * sessionPrice)

                allOf(Coll(
                    validSigUSD,
                    validCollateral
                ))

            }

            val validStatusSessionUpdate: Boolean = {

                (true, psychologistAddressBytes) == sessionBoxOut.R4[(Boolean, Coll[Byte])].get

            }

            val validSessionRecreation: Boolean = {

                allOf(Coll(
                    (SELF.value == sessionBoxOut.value),
                    (SELF.propositionBytes == sessionBoxOut.propositionBytes),
                    (SELF.tokens(0) == sessionBoxOut.tokens(0)),
                    (SELF.tokens(1) == sessionBoxOut.tokens(1)),
                    (SELF.R5[Int].get == sessionBoxOut.R5[Int].get),
                    (SELF.R6[Coll[Byte]].get == sessionBoxOut.R6[Coll[Byte]].get)
                ))

            }

            allOf(Coll(
                validPsychologistRegistration,
                validCollateralTransfer,
                validStatusSessionUpdate,
                validSessionRecreation
            ))

        }

        sigmaProp(validAcceptSessionTx)

    } else if (_txType.get == 2.toByte) {

        // ===== Cancel Session Tx ===== //
        val validCancelSessionTx: Boolean = {

            // Inputs
            val psychologistBoxesIn: Box = INPUTS.slice(1, INPUTS.size)
            val psychologistAddress: Coll[Byte] = psychologistBoxesIn(0).propositionBytes

            // Outputs
            val clientPKBoxOut: Box = OUTPUTS(0)
            val psychologistPKBoxOut: Box = OUTPUTS(1)
            val psyworkshopFeeBoxOut: Box = OUTPUTS(2)

            val validCancelationPeriod: Boolean = (sessionStartTimeBlockHeight - CONTEXT.HEIGHT >= sesssionCancelationPeriod)

            val validPsychologist: Boolean = {

                val validRegistrationToken: Boolean = {

                    psychologistBoxesIn.exists( (box: Box) => {

                        val validAddress: Boolean = {

                            (box.propositionBytes == psychologistAddress)

                        }
            
                        val validToken: Boolean = { 

                            box.tokens.exists( (token: (Coll[Byte], Long)) => {

                                (token._1 == $psyworkshopRegistrationTokenId)

                            })

                        }

                        allOf(Coll(
                            validAddress,
                            validToken
                        ))
            
                    })

                }

                val validSession: Boolean = {

                    val sessionStatus: (Boolean, Coll[Byte]) = SELF.R4[(Boolean, Coll[Byte])].get

                    allOf(Coll(
                        (true == sessionStatus._1),
                        (psychologistAddressBytes == sessionStatus._2)
                    ))

                }

                allOf(Coll(
                    validRegistrationToken,
                    validSession
                ))

            }

            val validClientRefundBoxOut: Boolean = {

                val validClientRefundAmount: Boolean = (clientPKBoxOut.tokens == (sessionPriceTokenId, sessionPrice))
                val validClientRefundAddress: Boolean = (clientPKBoxOut.propositionBytes == clientAddress)

                allOf(Coll(
                    validClientRefundAmount,
                    validClientRefundAddress
                ))

            }

            val validPsychologistBoxOut: Boolean = {

                // The fee is 50% of the collateral provided by the psychologist.
                val validCollateralAmount: Boolean = {

                    allOf(Coll(
                        (psychologistPKBoxOut.tokens(0)._1 == SELF.tokens(2)._1),
                        (psychologistPKBoxOut.tokens(0)._2 * 2 == SELF.tokens(2)._2)
                    ))

                }

                val validPsychologistAddress: Boolean = (psychologistPKBoxOut.propositionBytes == SELF.R4[(Boolean, Coll[Byte])].get._2)

                allOf(Coll(
                    validCollateralAmount,
                    validPsychologistAddress
                ))

            }

            val validPsyworkshopFeeBoxOut: Boolean = {

                val validValue: Boolean = (psyworkshopFeeBoxOut.value == SELF.value)

                // The fee is 50% of the collateral provided by the psychologist.
                val validFeeAmount: Boolean = {

                    allOf(Coll(
                        (psyworkshopFeeBoxOut.tokens(0)._1 == SELF.tokens(2)._1),
                        (psyworkshopFeeBoxOut.tokens(0)._2 * 2 == SELF.tokens(2)._2)
                    ))

                }

                val validFeeAddress: Boolean = (psyworkshopFeeBoxOut.propositionBytes == $psyworkshopFeeAddress)

                allOf(Coll(
                    validFeeAmount,
                    validFeeAddress
                ))

            }

            val validSessionBurn: Boolean = {

                OUTPUTS.flatMap( (output: Box) => {

                    output.tokens.map( (token: (Coll[Byte], Long)) => { 
                        
                        token._1 
                    
                    })

                }).forall( (token_id: Coll[Byte]) => { 
                    
                    token_id != $psyworkshopRegistrationTokenId 
                
                })

            }

            allOf(Coll(
                validCancelationPeriod,
                validPsychologist,
                validClientRefundBoxOut,
                validPsychologistBoxOut,
                validPsyworkshopFeeBoxOut,
                validSessionBurn
            ))

        }

        sigmaProp(validCancelSessionTx)

    } else if (_txType.get == 3.toByte) {

        val validUnacceptedSession: Boolean = {

            // Inputs

            // Ouptuts
            val clientPKBoxOut: Box = OUTPUTS(0)

            val validUnacceptedPeriod: Boolean = (sessionStartTimeBlockHeight - CONTEXT.HEIGHT <= sessionUnacceptedPeriod)

            val validUnacceptedSession: Boolean = ((false, Coll[Byte]()) == SELF.R4[(Boolean, Coll[Byte])].get)

            val validClientRefundBoxOut: Boolean = {

                val validClientRefundAmount: Boolean = (clientPKBoxOut.tokens == (sessionPriceTokenId, sessionPrice))
                val validClientRefundAddress: Boolean = (clientPKBoxOut.propositionBytes == clientAddress)

                allOf(Coll(
                    validClientRefundAmount,
                    validClientRefundAddress
                ))

            }            

            val validSessionBurn: Boolean = {

                OUTPUTS.flatMap( (output: Box) => {

                    output.tokens.map( (token: (Coll[Byte], Long)) => { 
                        
                        token._1 
                    
                    })

                }).forall( (token_id: Coll[Byte]) => { 
                    
                    (token_id != SessionSingletonId)
                
                })

            }

            allOf(Coll(
                validUnacceptedPeriod,
                validUnacceptedSession,
                validClientRefundBoxOut,
                validSessionBurn
            ))

        }

        sigmaProp(validUnacceptedSession)

    } else {
        sigmaProp(false)
    }

}