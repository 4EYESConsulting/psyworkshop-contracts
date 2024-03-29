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
    // R4: Int                              sessionStartTimeBlockHeight // TODO: Ask about session length or session end time.
    // R5: (SigmaProp, Boolean)            (clientAddressSigmaProp, isPresent)
    // R6: (SigmaProp, (Boolean, Boolean)) (psychologistAddressSigmaProp, (isSessionAccepted, isPresent)) 
    // R7: Boolean                          isSessionProblem

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

    // ===== Context Variables (_) ===== //
    // _txType: Byte

    // ===== Tx Type Bytes ===== //
    //  1: Accept Session Tx
    //  2: Cancel Session Tx
    //  3: Unaccepted Session Tx
    //  4: Session Start: Client Confirmation Tx
    //  5: Session Start: Psychologist Confirmation Tx
    //  6: Session Start: Psychologist Not Present Tx
    //  7: Session Start: Client Not Present Tx
    //  8: Session Start: Client And Psychologist Not Present Tx
    //  9: Session End: No Problem Message Tx
    // 10: Session End: Problem Message Tx
    // 11: Session End: Problem Message - Option 1 Tx
    // 12: Session End: Problem Message - Option 2 Tx

    // ===== Relevant Variables ===== //
    val _txType: Option[Byte] = getVar[Byte](0)

    val sessionSingletonId: Coll[Byte] = SELF.tokens(0)._1
    
    val sessionPriceTokenId: Coll[Byte] = SELF.tokens(1)._1
    val sessionPrice: Long = SELF.tokens(1)._1
    
    val sessionStartTimeBlockHeight: Int = SELF.R4[Int].get
    
    val clientSessionStatus: (SigmaProp, Boolean) = SELF.R5[(SigmaProp, Boolean)].get
    val clientAddressSigmaProp: SigmaProp = clientSessionStatus._1
    val isClientPresent: Boolean = clientSessionStatus._2
    
    val psychologistSessionStatus: (SigmaProp, (Boolean, Boolean)) = SELF.R6[(SigmaProp, (Boolean, Boolean))].get
    val psychologistAddressBytes: SigmaProp = psychologistSessionStatus._1
    val isSessionAccepted: Boolean = psychologistSessionStatus._2._1
    val isPsychologistPresent: Boolean = psychologistSessionStatus._2._2

    val isSessionProblem: Boolean = SELF.R7[Boolean].get

    val sessionLength: Int = 30                 // The session lasts 60 minutes, so 30 blocks on average since there is 1 block every 2 minutes on average.
    val sesssionCancelationPeriod: Int = 720    // The cancelation period is 24hrs, thus since there is 1 block every 2 minutes on average, there are 720 blocks every 24hrs on average.
    val sessionUnacceptedPeriod: Int = 60       // If no psychologist accepts the session within 2hrs of the session start time, thus since there is 1 block every 2 minutes on average, there are 60 blocks every 2hrs on average.
    val fiveMinutes: Int = 3                    // 1 block every 2 minutes on average, so 2.5 blocks every 5 minutes on average, so we round up.
    val tenMinutes: Int = 5                     // 1 block every 2 minutes on average, so 5 blocks every 10 minutes on average.
    val fifteenMinutes: Int = 8                 // 1 block every 2 minutes on average, so 7.5 blocks every 15 minutes on average, so we round up.
    val twentyMinutes: Int = 10                 // 1 block every 2 minutes on average, so 10 blocks every 20 minutes on average.

    val isFiveMinutesLate: Boolean = (CONTEXT.HEIGHT - sessionStartTimeBlockHeight >= fiveMinutes)
    val isTenMinutesLate: Boolean = (CONTEXT.HEIGHT - sessionStartTimeBlockHeight >= tenMinutes)
    val isFifteenMinutesLate: Boolean = (CONTEXT.HEIGHT - sessionStartTimeBlockHeight >= fifteenMinutes)
    val isPsychologistLate: Boolean = isFiveMinutesLate

    if (_txType.get == 1.toByte) {

        // ===== Accept Session Tx ===== //
        val validAcceptSessionTx: Boolean = {

            // Inputs
            val psychologistPKBoxIn: Box = INPUTS(1)

            // Outputs
            val sessionBoxOut: Box = OUTPUTS(0)

            val validPsychologistRegistration: Boolean = {

                val validRegistrationToken: Boolean = { 

                    psychologistPKBoxIn.tokens.exists( (token: (Coll[Byte], Long)) => {

                        (token._1 == $psyworkshopRegistrationTokenId)

                    })

                }

                val validSessionStatusUpdate: Boolean = {

                    val outStatus: (SigmaProp, (Boolean, Boolean)) = sessionBoxOut.R6[(SigmaProp, (Boolean, Boolean))].get

                    allOf(Coll(
                        (outStatus._1 != $psyworkshopAdminSigmaProp),
                        (outStatus._1.propBytes == psychologistPKBoxIn.propositionBytes),
                        (outStatus._2 == (true, false))
                    ))

                }

                allOf(Coll(
                    validRegistrationToken,
                    validSessionStatusUpdate
                ))

            }

            val validCollateralTransfer: Boolean = {

                val collateral: Long = sessionBoxOut.tokens(2)

                val validSigUSD: Boolean = {
                    
                    (collateral._1 == sessionPriceTokenId)
                    
                }

                val validCollateral: Boolean = {
                    
                    (collateral._2 >= (10L * sessionPrice) / 100L)
                    
                }

                allOf(Coll(
                    validSigUSD,
                    validCollateral
                ))

            }

            val validSessionRecreation: Boolean = {

                allOf(Coll(
                    (sessionBoxOut.value == SELF.value),
                    (sessionBoxOut.propositionBytes == SELF.propositionBytes),
                    (sessionBoxOut.tokens(0) == (sessionSingletonId, 1L)),
                    (sessionBoxOut.tokens(1) == (sessionPriceTokenId, sessionPrice)),
                    (sessionBoxOut.R4[Int].get == sessionStartTimeBlockHeight),
                    (sessionBoxOut.R5[(SigmaProp, Boolean)].get == (clientAddressSigmaProp, false))
                ))

            }

            allOf(Coll(
                validPsychologistRegistration,
                validCollateralTransfer,
                validSessionRecreation
            ))

        }

        sigmaProp(validAcceptSessionTx) && psychologistAddressSigmaProp

    } else if (_txType.get == 2.toByte) {

        // ===== Cancel Session Tx ===== //
        val validCancelSessionTx: Boolean = {

            // Inputs
            val psychologistPKBoxIn: Box = INPUTS(1)
            val psychologistPKBoxIn: Box = INPUTS(1)

            // Outputs
            val clientPKBoxOut: Box = OUTPUTS(0)
            val psychologistPKBoxOut: Box = OUTPUTS(1)
            val psyworkshopFeeBoxOut: Box = OUTPUTS(2)

            val validCancelationPeriod: Boolean = {
                
                (sessionStartTimeBlockHeight - CONTEXT.HEIGHT >= sesssionCancelationPeriod)
                
            }

            val validPsychologist: Boolean = {

                val validRegistrationToken: Boolean = { 

                    psychologistPKBoxIn.tokens.exists( (token: (Coll[Byte], Long)) => {

                        (token._1 == $psyworkshopRegistrationTokenId)

                    })

                }

                val validSession: Boolean = {

                    val validAddressBytes: Boolean = {

                        (psychologistPKBoxIn.propositionBytes == psychologistAddressSigmaProp.propBytes)

                    }

                    val validAcceptedSession: Boolean = {

                        (isSessionAccepted)

                    }

                    allOf(Coll(
                        validAddressBytes,
                        validAcceptedSession
                    ))

                }

                allOf(Coll(
                    validRegistrationToken,
                    validSession
                ))

            }

            val validClientRefundBoxOut: Boolean = {

                val validClientRefundAddressBytes: Boolean = {
                    
                    (clientPKBoxOut.propositionBytes == clientAddressSigmaProp.propBytes)
                
                }

                val validClientRefundAmount: Boolean = {
                    
                    (clientPKBoxOut.tokens(0) == (sessionPriceTokenId, sessionPrice))
                    
                }

                allOf(Coll(
                    validClientRefundAmount,
                    validClientRefundAddressBytes
                ))

            }

            val validPsychologistPKBoxOut: Boolean = {

                // The fee is 50% of the collateral provided by the psychologist.
                val validCollateralAmount: Boolean = {

                    allOf(Coll(
                        (psychologistPKBoxOut.tokens(0)._1 == SELF.tokens(2)._1),
                        (psychologistPKBoxOut.tokens(0)._2 == SELF.tokens(2)._2 / 2)
                    ))

                }

                val validPsychologistAddressBytes: Boolean = {
                    
                    allOf(Coll(
                        (psychologistAddressSigmaProp != $psyworkshopAdminSigmaProp),
                        (psychologistPKBoxOut.propositionBytes == psychologistAddressSigmaProp.propBytes)
                    ))
                    
                }

                allOf(Coll(
                    validCollateralAmount,
                    validPsychologistAddressBytes
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

                OUTPUTS.forall( (output: Box) => {

                    val validSingletonBurn: Boolean = {

                        output.tokens.forall( (token: (Coll[Byte], Long)) => { 
                            
                            (token._1 != $psyworkshopRegistrationTokenId) 
                        
                        })                        

                    }

                    val validSessionBoxDestruction: Boolean = {

                        (output.propositionBytes != SELF.propositionBytes)

                    }

                    allOf(Coll(
                        validSingletonBurn,
                        validSessionBoxDestruction
                    ))

                })

            }

            allOf(Coll(
                validCancelationPeriod,
                validPsychologist,
                validClientRefundBoxOut,
                validPsychologistPKBoxOut,
                validPsyworkshopFeeBoxOut,
                validSessionTermination
            ))

        }

        sigmaProp(validCancelSessionTx) && psychologistAddressSigmaProp

    } else if (_txType.get == 3.toByte) {

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

                    (psychologistAddressSigmaProp == $psyworkshopAdminSigmaProp)

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

                OUTPUTS.forall( (output: Box) => {

                    val validSingletonBurn: Boolean = {

                        output.tokens.forall( (token: (Coll[Byte], Long)) => { 
                            
                            (token._1 != $psyworkshopRegistrationTokenId) 
                        
                        })                        

                    }

                    val validSessionBoxDestruction: Boolean = {

                        (output.propositionBytes != SELF.propositionBytes)

                    }

                    allOf(Coll(
                        validSingletonBurn,
                        validSessionBoxDestruction
                    ))

                })

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
                    (sessionBoxOut.tokens(2) == SELF.tokens(2)) // Psychologist collateral.
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

                    (psychologistPKBoxIn.propositionBytes == psychologistAddressSigmaProp.propBytes)

                }

                val validPsychologistSessionStatus: Boolean = {

                    (isSessionAccepted) // Psychologist cannot confirm they are present before accepting the session.

                }

                val validPsychologistSessionStatusUpdate: Boolean = {

                    (sessionBoxOut.R6[(SigmaProp, (Boolean, Boolean))].get == (psychologistAddressSigmaProp, (isSessionAccepted, true)))

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
                        
                            (clientPKBoxOut.tokens(0)._1 = sessionPriceTokenId)
                        
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
                            validDiscountAddressBytes
                            validDiscountToken,
                            validDiscountAmount
                        ))

                    }

                    allOf(Coll(
                        validClientPKBoxOut
                    ))

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

                    val validPsychologistAddressBytes: Boolean = (psychologistAddressSigmaProp != $psyworkshopAdminSigmaProp)
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
                        validClientAddressBytes
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

                    OUTPUTS.forall( (output: Box) => {

                        val validSingletonBurn: Boolean = {

                            output.tokens.forall( (token: (Coll[Byte], Long)) => { 
                                
                                (token._1 != $psyworkshopRegistrationTokenId) 
                            
                            })                        

                        }

                        val validSessionBoxDestruction: Boolean = {

                            (output.propositionBytes != SELF.propositionBytes)

                        }

                        allOf(Coll(
                            validSingletonBurn,
                            validSessionBoxDestruction
                        ))

                    })

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

                        (psychologistPKBoxOut.propositionBytes == psychologistAddressBytes)

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

                    OUTPUTS.forall( (output: Box) => {

                        val validSingletonBurn: Boolean = {

                            output.tokens.forall( (token: (Coll[Byte], Long)) => { 
                                
                                (token._1 != $psyworkshopRegistrationTokenId) 
                            
                            })                        

                        }

                        val validSessionBoxDestruction: Boolean = {

                            (output.propositionBytes != SELF.propositionBytes)

                        }

                        allOf(Coll(
                            validSingletonBurn,
                            validSessionBoxDestruction
                        ))

                    })

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
                        validClientAddressBytes
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

                    OUTPUTS.forall( (output: Box) => {

                        val validSingletonBurn: Boolean = {

                            output.tokens.forall( (token: (Coll[Byte], Long)) => { 
                                
                                (token._1 != $psyworkshopRegistrationTokenId) 
                            
                            })                        

                        }

                        val validSessionBoxDestruction: Boolean = {

                            (output.propositionBytes != SELF.propositionBytes)

                        }

                        allOf(Coll(
                            validSingletonBurn,
                            validSessionBoxDestruction
                        ))

                    })

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

                        allOf(Coll(
                            (psychologistAddressSigmaProp != $psyworkshopAdminSigmaProp),
                            (psychologistPKBoxOut.propositionBytes == psychologistAddressSigmaProp.propBytes)
                        ))

                    }
                    val validPsychologistSessionAccepted: Boolean = (isSessionAccepted)
                    val validPsychologistSessionConfirmed: Boolean = (isPsychologistPresent)

                    allOf(Coll(
                        validPsychologistAddressBytes,
                        validPsychologistSessionAccepted,
                        validPsychologistSessionConfirmed
                    ))
                    
                }

                allOf(Coll(
                    validCollateralAmount,
                    validPsychologistAddressBytes
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

                OUTPUTS.forall( (output: Box) => {

                    val validSingletonBurn: Boolean = {

                        output.tokens.forall( (token: (Coll[Byte], Long)) => { 
                            
                            (token._1 != $psyworkshopRegistrationTokenId) 
                        
                        })                        

                    }

                    val validSessionBoxDestruction: Boolean = {

                        (output.propositionBytes != SELF.propositionBytes)

                    }

                    allOf(Coll(
                        validSingletonBurn,
                        validSessionBoxDestruction
                    ))

                })

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
                    (sessionBoxOut.tokens(2) == SELF.tokens(2))
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
                    
                    allOf(Coll(
                        ($psyworkshopAdminSigmaProp.propositionBytes != psychologistAddressSigmaProp.propBytes),
                        (clientPKBoxOut.propositionBytes == psychologistAddressSigmaProp.propBytes)
                    ))    
                
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

                OUTPUTS.forall( (output: Box) => {

                    val validSingletonBurn: Boolean = {

                        output.tokens.forall( (token: (Coll[Byte], Long)) => { 
                            
                            (token._1 != $psyworkshopRegistrationTokenId) 
                        
                        })                        

                    }

                    val validSessionBoxDestruction: Boolean = {

                        (output.propositionBytes != SELF.propositionBytes)

                    }

                    allOf(Coll(
                        validSingletonBurn,
                        validSessionBoxDestruction
                    ))

                })

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
                    
                    allOf(Coll(
                        ($psyworkshopAdminSigmaProp.propositionBytes != psychologistAddressSigmaProp.propBytes),
                        (clientPKBoxOut.propositionBytes == psychologistAddressSigmaProp.propBytes)
                    ))    
                
                }

                val validPsychologistRefundAmount: Boolean = {
                    
                    (psychologistPKBoxOut.tokens(0) == (sessionPriceTokenId, sessionPrice / 2))
                    
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

                OUTPUTS.forall( (output: Box) => {

                    val validSingletonBurn: Boolean = {

                        output.tokens.forall( (token: (Coll[Byte], Long)) => { 
                            
                            (token._1 != $psyworkshopRegistrationTokenId) 
                        
                        })                        

                    }

                    val validSessionBoxDestruction: Boolean = {

                        (output.propositionBytes != SELF.propositionBytes)

                    }

                    allOf(Coll(
                        validSingletonBurn,
                        validSessionBoxDestruction
                    ))

                })

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