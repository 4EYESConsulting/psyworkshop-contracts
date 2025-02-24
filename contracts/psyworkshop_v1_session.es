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
    // 4: Refund Tx: Client => Only possible if session is not accepted.
    // 5: Unaccepted Session Tx
    // 6: Session End Tx: No Problem // TODO: Let pyschologist pay themselves after 15 minute delay to give client change to complain if needed.
    // 7: Session End Tx: Problem
    // 8: Session End Tx: Psychologist Bad
    // 9: Session End Tx: Client Bad
    // 10: Session End Tx: Psyworkshop Bad

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

            val validClient: Boolean = (clientPKBoxIn.propositionBytes == psychologistAddressSigmaProp.propBytes)

            val validClientRefundBoxOut: Boolean = {

                val validClientRefundAddressBytes: Boolean = (clientPKBoxOut.propositionBytes == clientAddressSigmaProp.propBytes)

                val validClientRefundAmount: Boolean = (clientPKBoxOut.tokens(0) == (sessionPriceTokenId, sessionPrice))

                allOf(Coll(
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

        // ===== Unaccepted Session Tx ===== //
        val validUnacceptedSession: Boolean = {

            // Inputs


            // Ouptuts
            val clientPKBoxOut: Box = OUTPUTS(0)

            val validUnacceptedPeriod: Boolean = {
                
                (sessionStartTimeBlockHeight - CONTEXT.HEIGHT <= sessionUnacceptedPeriod)
                
            }

            val validUnacceptedSession: Boolean = {
                
                // No address bytes, not accepted, not present.
                val validNoAddressBytes: Boolean = {

// TODO: psychologistAddressSigmaProp doesnt exist?
                  true
                    // (psychologistAddressSigmaProp == $psyworkshopAdminSigmaProp)

                }

                val validSessionNotAccepted: Boolean = {

                    (!isSessionAccepted)

                }

                val validPsychologistNotPresent: Boolean = {

                    (!isPsychologistPresent)

                }

                allOf(Coll(
                    validNoAddressBytes,
                    validSessionNotAccepted,
                    validPsychologistNotPresent
                ))
                
            }

            val validClientRefundBoxOut: Boolean = {

                val validClientRefundAmount: Boolean = {

                    (clientPKBoxOut.tokens(0) == (sessionPriceTokenId, sessionPrice))
                
                }
                
                val validClientRefundAddress: Boolean = {

                    (clientPKBoxOut.propositionBytes == clientAddressSigmaProp.propBytes)

                }

                allOf(Coll(
                    validClientRefundAmount,
                    validClientRefundAddress
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

                }})

            }

            allOf(Coll(
                validUnacceptedPeriod,
                validUnacceptedSession,
                validClientRefundBoxOut,
                validSessionTermination
            ))

        }

        sigmaProp(validUnacceptedSession)

    } else if (_txType.get == 4.toByte) {

        // ===== Session Start: Client Confirmation Tx ===== //
        val validSessionStartClientConfirmationTx: Boolean = {

            // Inputs
            val clientPKBoxIn: Box = INPUTS(1)

            // Outputs
            val sessionBoxOut: Box = OUTPUTS(0)

            val validClientConfirmation: Boolean = {

                val validClientAddressBytes: Boolean = {

                    (clientPKBoxIn.propositionBytes == clientAddressSigmaProp.propBytes)

                }

                val validPsychologistSessionStatus: Boolean = {

                    (isSessionAccepted) // Client cannot confirm they are present before the psychologist has accepted the session.

                }

                val validClientSessionStatusUpdate: Boolean = {

                    (sessionBoxOut.R5[(SigmaProp, Boolean)].get == (clientAddressSigmaProp, true))

                }

                allOf(Coll(
                    validClientAddressBytes,
                    validPsychologistSessionStatus,
                    validClientSessionStatusUpdate
                ))

            }

            val validSessionStartTime: Boolean = {

                (CONTEXT.HEIGHT >= sessionStartTimeBlockHeight) // TODO: Maybe come up with better method than this, need to discuss.

            }

            val validSessionRecreation: Boolean = {

                allOf(Coll(
                    (sessionBoxOut.value == SELF.value),
                    (sessionBoxOut.propositionBytes == SELF.propositionBytes),
                    (sessionBoxOut.tokens(0) == (sessionSingletonId, 1L)),
                    (sessionBoxOut.tokens(1) == (sessionPriceTokenId, sessionPrice)),
                    (sessionBoxOut.tokens(2) == SELF.tokens(2)), // Psychologist collateral.
                    (sessionBoxOut.R4[Int].get == sessionStartTimeBlockHeight),
                    (sessionBoxOut.R6[(SigmaProp, (Boolean, Boolean))].get == psychologistSessionStatus)
                ))

            }

            allOf(Coll(
                validClientConfirmation,
                validSessionStartTime,
                validSessionRecreation
            ))

        }

        sigmaProp(validSessionStartClientConfirmationTx) && clientAddressSigmaProp

    } else if (_txType.get == 5.toByte) {

        // ===== Session Start: Psychologist Confirmation Tx ===== //
        val validSessionStartPsychologistConfirmationTx: Boolean = {

            // Inputs
            val psychologistPKBoxIn: Box = INPUTS(1)

            // Outputs
            val sessionBoxOut: Box = OUTPUTS(0)

            val validPsychologistConfirmation: Boolean = {

                val validPsychologistAddressBytes: Boolean = {

                  true
                  // TODO: psychologistAddressSigmaProp doesnt exist?

                    // (psychologistPKBoxIn.propositionBytes == psychologistAddressSigmaProp.propBytes)

                }

                val validPsychologistSessionStatus: Boolean = {

                    (isSessionAccepted) // Psychologist cannot confirm they are present before accepting the session.

                }

                val validPsychologistSessionStatusUpdate: Boolean = {

                  true
                  // TODO: psychologistAddressSigmaProp doesnt exist?

                    // (sessionBoxOut.R6[(SigmaProp, (Boolean, Boolean))].get == (psychologistAddressSigmaProp, (isSessionAccepted, true)))

                }

                allOf(Coll(
                    validPsychologistAddressBytes,
                    validPsychologistSessionStatus,
                    validPsychologistSessionStatusUpdate
                ))                

            }

            val validSessionStartTime: Boolean = {

                if (isPsychologistLate) {

                    // Outputs
                    val clientPKBoxOut: Box = OUTPUTS(1)

                    val validClientPKBoxOut: Boolean = {

                        val validDiscountAddressBytes: Boolean = {

                          (clientPKBoxOut.propositionBytes == clientAddressSigmaProp.propBytes)

                        }

                        val validDiscountToken: Boolean = {
                        
                          (clientPKBoxOut.tokens(0)._1 == sessionPriceTokenId)
                        
                        }

                        val validDiscountAmount: Boolean = {

                            if (isFiveMinutesLate && !isTenMinutesLate && !isFifteenMinutesLate) {
                                
                                (clientPKBoxOut.tokens(0)._2 == sessionPrice / 10) 

                            } else if (isTenMinutesLate && !isFifteenMinutesLate) {

                                (clientPKBoxOut.tokens(0)._2 == sessionPrice / 2)

                            } else {
                                false
                            }

                        }

                        allOf(Coll(
                            validDiscountAddressBytes,
                            validDiscountToken,
                            validDiscountAmount
                        ))

                    }

                    validClientPKBoxOut

                } else {
                    true
                }

            }

            val validSessionRecreation: Boolean = {

                allOf(Coll(
                    (sessionBoxOut.value == SELF.value),
                    (sessionBoxOut.propositionBytes == SELF.propositionBytes),
                    (sessionBoxOut.tokens(0) == (sessionSingletonId, 1L)),
                    (sessionBoxOut.tokens(1) == (sessionPriceTokenId, {

                        if (isFiveMinutesLate) {

                            (sessionPrice * 9) / 10

                        } else if (isTenMinutesLate) {

                            (sessionPrice / 2)

                        } else {

                            sessionPrice

                        }

                    })),
                    (sessionBoxOut.tokens(2) == SELF.tokens(2)), // Psychologist collateral.
                    (sessionBoxOut.R4[Int].get == sessionStartTimeBlockHeight),
                    (sessionBoxOut.R5[(SigmaProp, Boolean)].get == clientSessionStatus)
                ))

            }

            allOf(Coll(
                validPsychologistConfirmation,
                validSessionStartTime,
                validSessionRecreation
            ))

        }

        sigmaProp(validSessionStartPsychologistConfirmationTx) && psychologistAddressBytes

    } else if (_txType.get == 6.toByte) {

        // ===== Session Start: Psychologist Not Present Tx ===== //
        val validSessionStartPsychologistNotPresentTx: Boolean = {

            // Inputs
            
            // Outputs
            val clientPKBoxOut: Box = OUTPUTS(0)
            val psyworkshopFeeBoxOut: Box = OUTPUTS(1)

            if (isFifteenMinutesLate) {

                val validClientSessionStatus: Boolean = {

                    (isClientPresent) // Client must be present.

                }

                val validPsychologistSessionStatus: Boolean = {

                    // TODO: psychologistAddressSigmaProp doesnt exist?
                    val validPsychologistAddressBytes: Boolean = true//(psychologistAddressSigmaProp != $psyworkshopAdminSigmaProp)
                    val validPsychologistSessionAccepted: Boolean = (isSessionAccepted)
                    val validPsychologistSessionNotConfirmed: Boolean = (!isPsychologistPresent)
                    
                    allOf(Coll(
                        isSessionAccepted,
                        !isPsychologistPresent
                    ))

                }

                val validClientPKBoxOut: Boolean = {

                    val validClientAddressBytes: Boolean = {

                        (clientPKBoxOut.propositionBytes == clientAddressSigmaProp.propBytes)

                    }

                    val validClientRefundTokens: Boolean = {

                        (clientPKBoxOut.tokens(0) == (sessionPriceTokenId, sessionPrice))

                    }

                    allOf(Coll(
                        validClientAddressBytes,
                        validClientRefundTokens
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

                    }})

                }

                allOf(Coll(
                    validClientSessionStatus,
                    validPsychologistSessionStatus,
                    validClientPKBoxOut,
                    validPsyworkshopFeeBoxOut,
                    validSessionTermination
                ))                

            } else {
                false
            }

        }

        sigmaProp(validSessionStartPsychologistNotPresentTx)

    } else if (_txType.get == 7.toByte) {

        // ===== Session Start: Client Not Present Tx ===== //
        val validSessionStartClientNotPresentTx: Boolean = {

            // Inputs

            // Outputs
            val psychologistPKBoxOut: Box = OUTPUTS(0)
            val psyworkshopFeeBoxOut: Box = OUTPUTS(1)

            // Relevant Variables
            val collateral: Long = SELF.tokens(2)._2

            if (!isClientPresent) {

                val validPsychologistSessionStatus: Boolean = {

                    allOf(Coll(
                        isSessionAccepted,
                        isPsychologistPresent
                    ))

                }

                val validPsychologistPKBoxOut: Boolean = {

                    val validPsychologistAddressBytes: Boolean = {

                      true
                      // TODO: psychologistAddressSigmaProp doesnt exist?

                        // (psychologistPKBoxOut.propositionBytes == psychologistAddressBytes)

                    }

                    val validPsychologistTokens: Boolean = {

                        (psychologistPKBoxOut.tokens(0) == (sessionPriceTokenId, sessionPrice + (collateral / 2))) // TODO: Ask about this, why should psychologist be penalized for client not showing up.

                    }

                    allOf(Coll(
                        validPsychologistAddressBytes,
                        validPsychologistTokens
                    ))

                }

                val validPsyworkshopFeeBoxOut: Boolean = {

                    val validValue: Boolean = {
                        
                        (psyworkshopFeeBoxOut.value == SELF.value)
                        
                    }

                    // The fee is 100% of the collateral provided by the psychologist.
                    val validFeeAmount: Boolean = {

                        (psyworkshopFeeBoxOut.tokens(0) == (sessionPriceTokenId, (collateral / 2)))

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

                    }})

                }

                allOf(Coll(
                    validPsychologistSessionStatus,
                    validPsychologistPKBoxOut,
                    validPsyworkshopFeeBoxOut,
                    validSessionTermination
                ))                

            } else {
                false
            }

        }

        sigmaProp(validSessionStartClientNotPresentTx)

    } else if (_txType.get == 8.toByte) {

        // ===== Session Start: Client And Psychologist Not Present Tx ===== //
        val validSessionStartClientAndPsychologistNotPresentTx: Boolean = {
            
            // Inputs

            // Outputs
            val clientPKBoxOut: Box = OUTPUTS(0)
            val psyworkshopFeeBoxOut: Box = OUTPUTS(1)

            // Relevant variables
            val isTwentyMinutesPast: Boolean = (CONTEXT.HEIGHT - sessionStartTimeBlockHeight >= twentyMinutes)

            if (isSessionAccepted && !isPsychologistPresent && !isClientPresent && isTwentyMinutesPast) {

                val validClientRefundBoxOut: Boolean = {

                    val validClientAddressBytes: Boolean = {

                      (clientPKBoxOut.propositionBytes == clientAddressSigmaProp.propBytes)

                    }

                    val validClientRefundTokens: Boolean = {

                      (clientPKBoxOut.tokens(0) == (sessionPriceTokenId, sessionPrice))

                    }

                    allOf(Coll(
                        validClientAddressBytes,
                        validClientRefundTokens
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

                    }})

                }

                allOf(Coll(
                    validClientRefundBoxOut,
                    validPsyworkshopFeeBoxOut,
                    validSessionTermination
                ))

            } else {
                false
            }

        }

        sigmaProp(validSessionStartClientAndPsychologistNotPresentTx)

    } else if (_txType.get == 9.toByte) {

        // ===== Session End: No Problem Message ===== //
        val validSessionEndNoProblemMessageTx: Boolean = {

            // Inputs
            val clientPKBoxIn: Box = INPUTS(1)

            // Outputs
            val psychologistPKBoxOut: Box = OUTPUTS(0)
            val psyworkshopFeeBoxOut: Box = OUTPUTS(1)

            val validSessionPeriod: Boolean = {

                (CONTEXT.HEIGHT >= sessionStartTimeBlockHeight + sessionLength)

            }

            val validNoProblemStatus: Boolean = {

                (!isSessionProblem)

            }   

            val validPsychologistBoxOut: Boolean = {

                val validSessionPriceAmount: Boolean = {

                    allOf(Coll(
                        (psychologistPKBoxOut.tokens(0)._1 == SELF.tokens(1)._1),
                        (psychologistPKBoxOut.tokens(0)._2 == SELF.tokens(1)._2 + (SELF.tokens(2)._2 / 2))
                    ))

                }

                val validPsychologist: Boolean = {

                    val validPsychologistAddressBytes: Boolean = {

                      true
                      // TODO: psychologistAddressSigmaProp doesnt exist?

                        // allOf(Coll(
                        //     (psychologistAddressSigmaProp != $psyworkshopAdminSigmaProp),
                        //     (psychologistPKBoxOut.propositionBytes == psychologistAddressSigmaProp.propBytes)
                        // ))

                    }
                    val validPsychologistSessionAccepted: Boolean = (isSessionAccepted)
                    val validPsychologistSessionConfirmed: Boolean = (isPsychologistPresent)

                    allOf(Coll(
                        validPsychologistAddressBytes,
                        validPsychologistSessionAccepted,
                        validPsychologistSessionConfirmed
                    ))
                    
                }

                //TODO VARIABLE DOES NOT EXIST!!

                allOf(Coll(
                    true,//validCollateralAmount,
                    validPsychologist
                ))

            }

            val validPsyworkshopFeeBoxOut: Boolean = {

                val validValue: Boolean = {
                    
                    (psyworkshopFeeBoxOut.value == SELF.value)
                    
                }

                // The fee is 50% of the collateral provided by the psychologist.
                val validFeeAmount: Boolean = {

                    allOf(Coll(
                        (psyworkshopFeeBoxOut.tokens(0)._1 == SELF.tokens(2)._1),
                        (psyworkshopFeeBoxOut.tokens(0)._2 == SELF.tokens(2)._2 / 2)
                    ))

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

                }})

            }

            allOf(Coll(
                validSessionPeriod,
                validNoProblemStatus,
                validPsychologistBoxOut,
                validPsyworkshopFeeBoxOut,
                validSessionTermination
            ))

        }

        sigmaProp(validSessionEndNoProblemMessageTx)

    } else if (_txType.get == 10.toByte) {

        // ===== Session End: Problem Message Tx ===== //
        val validSessionEndProblemMessageTx: Boolean = {

            // Inputs
            val clientPKBoxIn: Box = INPUTS(1)

            // Outputs
            val sessionBoxOut: Box = OUTPUTS(0)

            val validSessionPeriod: Boolean = {

                (CONTEXT.HEIGHT >= sessionStartTimeBlockHeight + sessionLength)

            }

            val validProblemStatusUpdate: Boolean = {

                (sessionBoxOut.R7[Boolean].get == true)

            }

            val validSessionRecreation: Boolean = {

                allOf(Coll(
                    (sessionBoxOut.value == SELF.value),
                    (sessionBoxOut.propositionBytes == SELF.propositionBytes),
                    (sessionBoxOut.tokens(0) == SELF.tokens(0)),
                    (sessionBoxOut.tokens(1) == SELF.tokens(1)),
                    (sessionBoxOut.tokens(2) == SELF.tokens(2)),
                    (sessionBoxOut.R4[Int].get == SELF.R4[Int].get),
                    (sessionBoxOut.R5[(SigmaProp, Boolean)].get == SELF.R5[(SigmaProp, Boolean)].get),
                    (sessionBoxOut.R6[(SigmaProp, (Boolean, Boolean))].get == SELF.R6[(SigmaProp, (Boolean, Boolean))].get)
                ))

            }

            allOf(Coll(
                validSessionPeriod,
                validProblemStatusUpdate,
                validSessionRecreation
            ))

        }

        sigmaProp(validSessionEndProblemMessageTx)

    } else if (_txType.get == 11.toByte) {

        // ===== Session End: Problem Message - Option 1 Tx ===== //
        val validSessionEndProblemMessageOption1Tx: Boolean = {

            // Inputs
            val adminPKBoxIn: Box = INPUTS(1)

            // Outputs
            val clientPKBoxOut: Box = OUTPUTS(0)
            val psychologistPKBoxOut: Box = OUTPUTS(1)

            val validSessionPeriod: Boolean = {

                (CONTEXT.HEIGHT > sessionStartTimeBlockHeight + sessionLength)

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
                    
                    (clientPKBoxOut.tokens(0) == SELF.tokens(1))
                    
                }

                allOf(Coll(
                    validClientRefundAmount,
                    validClientRefundAddressBytes
                ))

            }

            val validPsychologistRefundBoxOut: Boolean = {

                val validPsychologistRefundAddressBytes: Boolean = {

                  true
                  // TODO: psychologistAddressSigmaProp doesnt exist?
                    
                    // allOf(Coll(
                    //     ($psyworkshopAdminSigmaProp != psychologistAddressSigmaProp.propBytes),
                    //     (clientPKBoxOut.propositionBytes == psychologistAddressSigmaProp.propBytes)
                    // ))    
                
                }

                val validPsychologistRefundAmount: Boolean = {
                    
                    (psychologistPKBoxOut.tokens(0) == SELF.tokens(2))
                    
                }

                allOf(Coll(
                    validPsychologistRefundAddressBytes,
                    validPsychologistRefundAmount
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

                }})

            }

            allOf(Coll(
                validSessionPeriod,
                validSessionProblemStatus,
                validAdminPKBoxIn,
                validClientRefundBoxOut,
                validPsychologistRefundBoxOut,
                validSessionTermination
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
