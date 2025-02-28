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
    // 3. (SigUSDId, Collateral) // If provided by the psychologist. // TODO: All of this will be in one index, not split into two, fix where used in the contract.
    // Registers
    // R4: Int                              sessionStartTimeBlockHeight
    // R5: (SigmaProp, Boolean)            (clientAddressSigmaProp, isPresent)
    // R6: (SigmaProp, (Boolean, Boolean)) (psychologistAddressSigmaProp, (isSessionAccepted, isPresent)) 
    // R7: Boolean                          isSessionProblem

    // ===== Box Contents ===== // (new)
    // Tokens
    // 1. (SessionSingletonId, 1)
    // 2. (SigUSDId, SessionPrice + Collateral) // If provided by the psychologist.
    // Registers
    // R4: Int                              sessionStartTimeBlockHeight
    // R5: SigmaProp                        clientAddressSigmaProp
    // R6: SigmaProp                        pyschologistAddressSigmaProp
    // R7: (Boolean, Boolean)               (isSessionAccepted, isSessionProblem) // Both false initially.
    // R8: Long                             sessionPrice
    // R9: Long                             collateral  // Assume 0 initially. 

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
    // $psyworkshopFeeAddressBytes: Coll[Byte]
    // $psyworkshopAdminSigmaProp: SigmaProp
    val $psyworkshopRegistrationTokenId: Coll[Byte] = fromBase16("2c89e1e137d05659e45018fd6412da19c687b4101f241b6014c7cd5321897b1e")
    val $psyworkshopFeeAddressBytes: Coll[Byte] = fromBase16("2c89e1e137d05659e45018fd6412da19c687b4101f241b6014c7cd5321897b1e")
    val $psyworkshopAdminSigmaProp: SigmaProp = PK("9fzRcctiWfzoJyqGtPWqoXPuxSmFw6zpnjtsQ1B6jSN514XqH4q")

    // ===== Context Variables (_) ===== //
    // _txType: Int

    // ===== Tx Type Bytes ===== //
    // 1: Accept Session Tx
    // 2: Cancel Session Tx: Psychologist
    // 3: Cancel Session Tx: Client
    // 4: Refund Tx: Client
    // 5: Session End Tx: No Problem // TODO: Let pyschologist pay themselves after 15 minute delay to give client chance to complain if needed.
    // 6: Session End Tx: Problem
    // 7: Session End Tx: Psychologist Bad
    // 8: Session End Tx: Client Bad
    // 9: Session End Tx: Psyworkshop Bad

    // ===== Functions ===== //
    // def validRegistrationToken: Box => Boolean
    // def validSessionTermination: () => Boolean
    
    def validRegistrationToken(input: Box): Boolean = { 

        input.tokens.exists({ (token: (Coll[Byte], Long)) => {

            (token._1 == $psyworkshopRegistrationTokenId)

        }})

    }

    def validSessionTermination(): Boolean = {

        OUTPUTS.forall({ (output: Box) => {

            val validSingletonBurn: Boolean = {

                output.tokens.forall({ (token: (Coll[Byte], Long)) => { 
                    
                    (token._1 != sessionSingletonId) 
                
                }})                

            }

            val validSessionBoxDestruction: Boolean = {

                (output.propositionBytes != SELF.propositionBytes)

            }

            allOf(Coll(
                validSingletonBurn,
                validSessionBoxDestruction
            ))

        }})

    }

    // ===== Relevant Variables ===== //
    val _txType: Option[Int] = getVar[Int](0)

    val sessionSingletonId: Coll[Byte] = SELF.tokens(0)._1
    val priceTokenId: Coll[Byte] = SELF.tokens(1)._1
    val totalValue: Long = SELF.tokens(1)._2
    val sessionStartTimeBlockHeight: Int = SELF.R4[Int].get
    val clientAddressSigmaProp: SigmaProp = SELF.R5[SigmaProp].get    
    val psychologistAddressSigmaProp: SigmaProp = SELF.R6[SigmaProp].get
    val sessionStatus: (Boolean, Boolean) = SELF.R7[(Boolean, Boolean)].get
    val isSessionAccepted: Boolean = sessionStatus._1
    val isSessionProblem: Boolean = sessionStatus._2
    val sessionPrice: Long = SELF.R8[Long].get
    val collateral: Long = SELF.R9[Long].get

    val sessionLength: Int = 30                         // The session lasts 60 minutes, so 30 blocks on average since there is 1 block every 2 minutes on average.
    val clientSessionCancelationPeriod: Int = 720       // The client cancelation period is 24hrs, thus since there is 1 block every 2 minutes on average, there are 720 blocks every 24hrs on average.
    val psychologistSessionCancelationPeriod: Int = 60  // The psychologist cancelation period is 2hrs, thus since there is 1 block every 2 minutes on average, there are 60 blocks every 2hrs on average.
    val sessionUnacceptedPeriod: Int = 60               // If no psychologist accepts the session within 2hrs of the session start time, thus since there is 1 block every 2 minutes on average, there are 60 blocks every 2hrs on average.
    val fifteenMinutes: Int = 8                         // 1 block every 2 minutes on average, so 7.5 blocks every 15 minutes on average, so we round up.

    val isSessionStarted: Boolean = (CONTEXT.HEIGHT >= sessionStartTimeBlockHeight) && isSessionAccepted
    val isSessionOver: Boolean = (CONTEXT.HEIGHT >= sessionStartTimeBlockHeight + sessionLength)
    val isSessionComplaintTimeOver: Boolean = (CONTEXT.HEIGHT >= sessionStartTimeBlockHeight + sessionLength + fifteenMinutes)
    val isPsychologistSessionCancelTime: Boolean = (CONTEXT.HEIGHT - sessionStartTimeBlockHeight >= psychologistSessionCancelationPeriod)
    val isClientSessionCancelTime: Boolean = (CONTEXT.HEIGHT - sessionStartTimeBlockHeight >= clientSessionCancelationPeriod)
    val isClientSessionCancelTimePenalty: Boolean = (CONTEXT.HEIGHT - sessionStartTimeBlockHeight <= clientSessionCancelationPeriod) && (CONTEXT.HEIGHT - sessionStartTimeBlockHeight > 0)

    if (_txType.get == 1) {

        // ===== Accept Session Tx ===== //
        val validAcceptSessionTx: Boolean = {

            // Inputs
            val psychologistPKBoxIn: Box = INPUTS(1)

            // Outputs
            val sessionBoxOut: Box = OUTPUTS(0)

            val validPsychologistRegistration: Boolean = {

                val validSessionStatusUpdate: Boolean = {

                    val outPsychologistAddress: SigmaProp = sessionBoxOut.R6[SigmaProp].get
                    val outStatus: (Boolean, Boolean) = sessionBoxOut.R7[(Boolean, Boolean)].get

                    allOf(Coll(
                        (outPsychologistAddress !=  $psyworkshopAdminSigmaProp),
                        (outPsychologistAddress.propBytes == psychologistPKBoxIn.propositionBytes),
                        (outStatus == (true, false))
                    ))

                }

                allOf(Coll(
                    validRegistrationToken(psychologistPKBoxIn),
                    validSessionStatusUpdate
                ))

            }

            val validCollateralTransfer: Boolean = {

                val outTotalValue: Long = sessionBoxOut.tokens(1)._2

                val outCollateral: Long = sessionBoxOut.R9[Long].get
                    
                allOf(Coll(
                    (outCollateral > 0),
                    (outCollateral == (10L * sessionPrice) / 100L), 
                    (outTotalValue == sessionPrice + outCollateral)
                ))
                    
            }

            val validSessionRecreation: Boolean = {

                allOf(Coll(
                    (sessionBoxOut.value == SELF.value),
                    (sessionBoxOut.propositionBytes == SELF.propositionBytes),
                    (sessionBoxOut.tokens(0) == (sessionSingletonId, 1L)),
                    (sessionBoxOut.tokens(1)._1 == sessionPriceTokenId),
                    (sessionBoxOut.R4[Int].get == sessionStartTimeBlockHeight),
                    (sessionBoxOut.R5[SigmaProp].get == clientAddressSigmaProp),
                    (sessionBoxOut.R8[Long].get == sessionPrice)
                ))

            }

            allOf(Coll(
                validPsychologistRegistration,
                validCollateralTransfer,
                validSessionRecreation
            ))

        }

        sigmaProp(validAcceptSessionTx) && psychologistAddressSigmaProp

    } else if (_txType.get == 2) {

        // ===== Cancel Session Tx: Psychologist ===== //
        val validCancelSessionPsychologistTx: Boolean = {

            // Inputs
            val psychologistPKBoxIn: Box = INPUTS(1)

            // Outputs
            val clientPKBoxOut: Box = OUTPUTS(0)
            val psychologistPKBoxOut: Box = OUTPUTS(1)
            val psyworkshopFeeBoxOut: Box = OUTPUTS(2)

            val validPsychologist: Boolean = {

                val validAddressBytes: Boolean = (psychologistPKBoxIn.propositionBytes == psychologistAddressSigmaProp.propBytes)

                allOf(Coll(
                    validRegistrationToken(psychologistPKBoxIn),
                    validAddressBytes
                ))

            }

            val validClientRefundBoxOut: Boolean = {

                val validClientRefundAddressBytes: Boolean = (clientPKBoxOut.propositionBytes == clientAddressSigmaProp.propBytes)

                val validClientRefundAmount: Boolean = (clientPKBoxOut.tokens(0) == (sessionPriceTokenId, sessionPrice))

                allOf(Coll(
                    validClientRefundAddressBytes,
                    validClientRefundAmount
                ))

            }

            val validPsychologistPKBoxOut: Boolean = {

                val validPsychologistAddressBytes: Boolean = (psychologistPKBoxOut.propositionBytes == psychologistAddressSigmaProp.propBytes)

                // The fee is 50% of the collateral provided by the psychologist.
                val validCollateralAmount: Boolean = {

                    allOf(Coll(
                        (psychologistPKBoxOut.tokens(0)._1 == sessionPriceTokenId),
                        (psychologistPKBoxOut.tokens(0)._2 == collateral / 2)
                    ))

                }

                allOf(Coll(
                    validPsychologistAddressBytes,
                    validCollateralAmount
                ))

            }

            val validPsyworkshopFeeBoxOut: Boolean = {

                val validValue: Boolean = (psyworkshopFeeBoxOut.value == SELF.value)

                val validFeeAddressBytes: Boolean = (psyworkshopFeeBoxOut.propositionBytes == $psyworkshopFeeAddressBytes)

                // The fee is 50% of the collateral provided by the psychologist.
                val validFeeAmount: Boolean = {

                    val collateralAmount = SELF.tokens(1)._2 - sessionPrice
                    val remainder = collateralAmount - (collateral / 2)

                    allOf(Coll(
                        (psyworkshopFeeBoxOut.tokens(0)._1 == sessionPriceTokenId),
                        (psyworkshopFeeBoxOut.tokens(0)._2 == remainder)
                    ))

                }
                    
                allOf(Coll(
                    validValue,
                    validFeeAddressBytes,
                    validFeeAmount
                ))

            }

            allOf(Coll(
                isSessionAccepted,
                isPsychologistSessionCancelTime,
                validPsychologist,
                validClientRefundBoxOut,
                validPsychologistPKBoxOut,
                validPsyworkshopFeeBoxOut,
                validSessionTermination()
            ))

        }

        sigmaProp(validCancelSessionPsychologistTx) && psychologistAddressSigmaProp

    } else if (_txType.get == 3) {

        // ===== Cancel Session Tx: Client ===== //
        val validCancelSessionClientTx: Boolean = {

            // Inputs
            val clientPKBoxIn: Box = INPUTS(1)

            // Outputs
            val clientPKBoxOut: Box = OUTPUTS(0)
            val psychologistPKBoxOut: Box = OUTPUTS(1)
            val psyworkshopFeeBoxOut: Box = OUTPUTS.getOrElse(2, SELF)
            
            // Relevant Variables
            val remainder = sessionPrice - (sessionPrice / 2)
            val workshopFee = remainder / 2
            val psychFee = remainder - workshopFee

            val validClient: Boolean = (clientPKBoxIn.propositionBytes == psychologistAddressSigmaProp.propBytes)

            val validClientRefundBoxOut: Boolean = {

                val validClientRefundAddressBytes: Boolean = (clientPKBoxOut.propositionBytes == clientAddressSigmaProp.propBytes)

                val validClientRefundAmount: Boolean = {

                    if (isClientSessionCancelTime) {

                        (clientPKBoxOut.tokens(0) == (sessionPriceTokenId, sessionPrice))

                    } else if (isClientSessionCancelTimePenalty) {

                        (clientPKBoxOut.tokens(0) == (sessionPriceTokenId, sessionPrice / 2))

                    } else {
                        false
                    }
                
                }

                allOf(Coll(
                    validClientRefundAddressBytes,
                    validClientRefundAmount
                ))

            }

            val validPsychologistPKBoxOut: Boolean = {

                val validPsychologistAddressBytes: Boolean = (psychologistPKBoxOut.propositionBytes == psychologistAddressSigmaProp.propBytes)

                val validCollateralAmount: Boolean = {


                    if (isClientSessionCancelTime) {

                        // 100% of the collateral is returned.
                        allOf(Coll(
                            (psychologistPKBoxOut.tokens(0)._1 == sessionPriceTokenId),
                            (psychologistPKBoxOut.tokens(0)._2 == collateral)
                        ))


                    } else if (isClientSessionCancelTimePenalty) {

                        // 100% of the collateral is returned + 25% of the sessionPrice.
                        allOf(Coll(
                            (psychologistPKBoxOut.tokens(0)._1 == sessionPriceTokenId),
                            (psychologistPKBoxOut.tokens(0)._2 == collateral + psychFee)
                        ))

                    } else {
                        false
                    }

                }

                allOf(Coll(
                    validPsychologistAddressBytes,
                    validCollateralAmount
                ))

            }

            val validFeeBoxOut: Boolean = {

                if (isClientSessionCancelTimePenalty) {

                    val validValue: Boolean = (psyworkshopFeeBoxOut.value == SELF.value)

                    val validFeeAddressBytes: Boolean = (psyworkshopFeeBoxOut.propositionBytes == $psyworkshopFeeAddressBytes)
                
                    // The fee is 25% of the sessionPrice provided by the client, only if the penalty time is reached.
                    val validFeeAmount: Boolean = {

                        allOf(Coll(
                            (psyworkshopFeeBoxOut.tokens(0)._1 == sessionPriceTokenId),
                            (psyworkshopFeeBoxOut.tokens(0)._2 == workshopFee)
                        ))

                    }
                        
                    allOf(Coll(
                        validValue,
                        validFeeAddressBytes,
                        validFeeAmount
                    ))

                } else {
                    true
                }

            }

            allOf(Coll(
                isSessionAccepted,
                validClient,
                validClientRefundBoxOut,
                validPsychologistPKBoxOut,
                validPsyworkshopFeeBoxOut,
                validSessionTermination()
            ))

        }

        sigmaProp(validCancelSessionClientTx) && clientAddressSigmaProp

    } else if (_txType.get == 4) {

        // ===== Refund Tx: Client ===== //
        val validClientRefundTx: Boolean = {

            // Inputs
            val clientPKBoxIn: Box = INPUTS(1)

            // Outputs
            val clientPKBoxOut: Box = OUTPUTS(0)

            val validClient: Boolean = (clientPKBoxIn.propositionBytes == clientAddressSigmaProp.propBytes)

            val validClientRefundBoxOut: Boolean = {

                val validValue: Boolean = (clientPKBoxOut.value == SELF.value)

                val validClientRefundAddressBytes: Boolean = (clientPKBoxOut.propositionBytes == clientAddressSigmaProp.propBytes)

                val validClientRefundAmount: Boolean = (clientPKBoxOut.tokens(0) == (sessionPriceTokenId, sessionPrice))

                allOf(Coll(
                    validValue,
                    validClientRefundAddressBytes,
                    validClientRefundAmount
                ))

            }

            allOf(Coll(
                !isSessionAccepted,
                validClient,
                validClientRefundBoxOut,
                validSessionTermination()
            ))            

        }

        sigmaProp(validClientRefundTx) && clientAddressSigmaProp

    } else if (_txType.get == 5) {

        // ===== Session End: No Problem Message ===== //
        // Psychologist claims the reward if there is no problem after 15 minutes of the session end.
        val validSessionEndNoProblemMessageTx: Boolean = {

            // Inputs
            val psychologistPKBoxIn: Box = INPUTS(1)

            // Outputs
            val psychologistPKBoxOut: Box = OUTPUTS(0)
            val psyworkshopFeeBoxOut: Box = OUTPUTS(1)
            
            val psychFee: Long = ((950 * sessionPrice) / 1000)
            val workshopFee: Long = sessionPrice - psychFee 

            val validPsychologistBoxOut: Boolean = {

                val validPsychologistAddressBytes: Boolean = (psychologistPKBoxOut.propositionBytes == psychologistAddressSigmaProp.propBytes)

                val validSessionPriceAmount: Boolean = {

                    allOf(Coll(
                        (psychologistPKBoxOut.tokens(0)._1 == sessionPriceTokenId),
                        (psychologistPKBoxOut.tokens(0)._2 == collateral + psychFee)
                    ))

                }

                allOf(Coll(
                    validPsychologistAddressBytes,
                    validSessionPriceAmount,
                    validRegistrationToken(psychologistPKBoxIn)
                ))

            }

            val validPsyworkshopFeeBoxOut: Boolean = {

                val validValue: Boolean = (psyworkshopFeeBoxOut.value == SELF.value)

                val validFeeAddressBytes: Boolean = (psyworkshopFeeBoxOut.propositionBytes == $psyworkshopFeeAddressBytes)

                val validFeeAmount: Boolean = {

                    allOf(Coll(
                        (psyworkshopFeeBoxOut.tokens(0)._1 == sessionPriceTokenId),
                        (psyworkshopFeeBoxOut.tokens(0)._2 == workshopFee)
                    ))

                }

                allOf(Coll(
                    validValue,
                    validFeeAddressBytes,
                    validFeeAmount
                ))

            }

            allOf(Coll(
                isSessionOver,
                isSessionComplaintTimeOver,
                isSessionAccepted,
                !isSessionProblem,
                validPsychologistBoxOut,
                validPsyworkshopFeeBoxOut,
                validSessionTermination()
            ))

        }

        sigmaProp(validSessionEndNoProblemMessageTx) && psychologistAddressSigmaProp

    } else if (_txType.get == 6) {

        // ===== Session End: Problem Message Tx ===== //
        val validSessionEndProblemMessageTx: Boolean = {

            // Inputs
            val clientPKBoxIn: Box = INPUTS(1)

            // Outputs
            val sessionBoxOut: Box = OUTPUTS(0)

            val validClient: Boolean = (clientPKBoxIn.propositionBytes == clientAddressSigmaProp.propBytes)

            val validSessionStatusUpdate: Boolean = (sessionBoxOut.R7[(Boolean, Boolean)].get._2 == true)

            val validSessionRecreation: Boolean = {

                allOf(Coll(
                    (sessionBoxOut.value == SELF.value),
                    (sessionBoxOut.propositionBytes == SELF.propositionBytes),
                    (sessionBoxOut.tokens(0) == SELF.tokens(0)),
                    (sessionBoxOut.tokens(1) == SELF.tokens(1)),
                    (sessionBoxOut.R4[Int].get == SELF.R4[Int].get),
                    (sessionBoxOut.R5[SigmaProp].get == SELF.R5[SigmaProp].get),
                    (sessionBoxOut.R6[SigmaProp].get == SELF.R6[SigmaProp].get),
                    (sessionBoxOut.R7[(Boolean, Boolean)].get._1 == SELF.R7[(Boolean, Boolean)].get._1),
                    (sessionBoxOut.R8[Long].get == SELF.R8[Long].get),
                    (sessionBoxOut.R9[Long].get == SELF.R9[Long].get)
                ))

            }

            allOf(Coll(
                isSessionStarted,
                !isSessionComplaintTimeOver,
                validClient,
                validSessionStatusUpdate,
                validSessionRecreation
            ))

        }

        sigmaProp(validSessionEndProblemMessageTx) && clientAddressSigmaProp

    } else if (_txType.get == 7) {

        // ===== Session End: Problem - Psychologist Bad ===== //
        val validSessionEndProblemMessageOption1Tx: Boolean = {

            // Inputs
            val adminPKBoxIn: Box = INPUTS(1)

            // Outputs
            val clientPKBoxOut: Box = OUTPUTS(0)
            val psychologistPKBoxOut: Box = OUTPUTS(1)

            val clientAmount: Long = (collateral / 2) + sessionPrice
            val workshopFee: Long = collateral - (collateral / 2)

            val validAdmin: Boolean = (adminPKBoxIn.propositionBytes == $psyworkshopAdminSigmaProp.propBytes)

            val validClientRefundBoxOut: Boolean = {

                val validValue: Boolean = (clientPKBoxOut.value == SELF.value)

                val validClientRefundAddressBytes: Boolean = (clientPKBoxOut.propositionBytes == clientAddressSigmaProp.propBytes)

                val validClientRefundAmount: Boolean = {
                    
                    allOf(Coll(
                        (clientPKBoxOut.tokens(0)._1 == sessionPriceTokenId),
                        (clientPKBoxOut.tokens(0)._2 == clientAmount)
                    ))
                    
                }

                allOf(Coll(
                    validValue,
                    validClientRefundAddressBytes,
                    validClientRefundAmount
                ))

            }

            val validPsyworkshopFeeBoxOut: Boolean = {

                val validFeeAddressBytes: Boolean = (psyworkshopFeeBoxOut.propositionBytes == $psyworkshopFeeAddressBytes)

                val validFeeAmount: Boolean = {

                    allOf(Coll(
                        (psyworkshopFeeBoxOut.tokens(0)._1 == sessionPriceTokenId),
                        (psyworkshopFeeBoxOut.tokens(0)._2 == workshopFee)
                    ))

                }

                allOf(Coll(
                    validFeeAddressBytes,
                    validFeeAmount
                ))

            }

            allOf(Coll(
                isSessionProblem,
                validAdmin,
                validClientRefundBoxOut,
                validPsyworkshopFeeBoxOut,
                validSessionTermination()
            ))

        }

        sigmaProp(validSessionEndProblemMessageOption1Tx) && $psyworkshopAdminSigmaProp

    } else if (_txType.get == 12.toByte) {

        // ===== Session End Problem Message - Option 2 Tx ===== //
        val validSessionEndProblemMessageOption2Tx: Boolean = {

            // Inputs
            val adminPKBoxIn: Box = INPUTS(1)

            // Outputs
            val clientPKBoxOut: Box = OUTPUTS(0)
            val psychologistPKBoxOut: Box = OUTPUTS(1)
            val psyworkshopFeeBoxOut: Box = OUTPUTS(2)

            val validSessionPeriod: Boolean = {

                (CONTEXT.HEIGHT >= sessionStartTimeBlockHeight + sessionLength)

            }

            val validSessionProblemStatus: Boolean = {

                (isSessionProblem)

            }

            val validAdminPKBoxIn: Boolean = {

            (adminPKBoxIn.propositionBytes == $psyworkshopAdminSigmaProp.propBytes)

            }

            val validClientRefundBoxOut: Boolean = {

                val validClientRefundAddressBytes: Boolean = {
                    
                    (clientPKBoxOut.propositionBytes == clientAddressSigmaProp.propBytes)
                
                }

                val validClientRefundAmount: Boolean = {
                    
                    (clientPKBoxOut.tokens(0) == (sessionPriceTokenId, sessionPrice / 2))
                    
                }

                allOf(Coll(
                    validClientRefundAmount,
                    validClientRefundAddressBytes
                ))

            }

            val validPsychologistRefundBoxOut: Boolean = {

                val validPsychologistRefundAddressBytes: Boolean = {
                   
                   // TODO: psychologistAddressSigmaProp doesnt exist?
                    // allOf(Coll(
                    //     ($psyworkshopAdminSigmaProp != psychologistAddressSigmaProp.propBytes),
                    //     (clientPKBoxOut.propositionBytes == psychologistAddressSigmaProp.propBytes)
                    // ))    

                    true
                
                }

                val validPsychologistRefundAmount: Boolean = {
                    
                  allOf(Coll(
                    (psychologistPKBoxOut.tokens(0)._1 == sessionPriceTokenId),
                    (psychologistPKBoxOut.tokens(0)._2 == sessionPrice / 2)
                ))
                
                }

                allOf(Coll(
                    validPsychologistRefundAddressBytes,
                    validPsychologistRefundAmount
                ))

            }

            val validPsyworkshopFeeBoxOut: Boolean = {

                val validValue: Boolean = {
                    
                    (psyworkshopFeeBoxOut.value == SELF.value)
                    
                }

                // The fee is 100% of the collateral provided by the psychologist.
                val validFeeAmount: Boolean = {

                    (psyworkshopFeeBoxOut.tokens(0) == SELF.tokens(2))

                }

                val validFeeAddressBytes: Boolean = {
                    
                    (psyworkshopFeeBoxOut.propositionBytes == $psyworkshopFeeAddressBytes)
                    
                }

                allOf(Coll(
                    validValue,
                    validFeeAmount,
                    validFeeAddressBytes
                ))

            }

            val validSessionTermination: Boolean = {

              OUTPUTS.forall({ (output: Box) => {

                  val validSingletonBurn: Boolean = {

                    output.tokens.forall({ (token: (Coll[Byte], Long)) => {   
                       (token._1 != $psyworkshopRegistrationTokenId) 
                    }})  

                  }

                    val validSessionBoxDestruction: Boolean = {

                        (output.propositionBytes != SELF.propositionBytes)

                    }

                    allOf(Coll(
                        validSingletonBurn,
                        validSessionBoxDestruction
                    ))                

              } })

            }

            allOf(Coll(
                validSessionPeriod,
                validSessionProblemStatus,
                validAdminPKBoxIn,
                validClientRefundBoxOut,
                validPsychologistRefundBoxOut,
                validPsyworkshopFeeBoxOut,
                validSessionTermination
            ))

        }

        sigmaProp(validSessionEndProblemMessageOption2Tx) && $psyworkshopAdminSigmaProp
        
    } else {
        sigmaProp(false)
    }

}
