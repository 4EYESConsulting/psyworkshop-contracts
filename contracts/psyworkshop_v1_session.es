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
    // R4: Int                              sessionStartTimeBlockHeight
    // R5: (Coll[Byte], Boolean)            (clientAddressBytes, isPresent)
    // R6: (Coll[Byte], (Boolean, Boolean)) (psychologistAddressBytes, (isSessionAccepted, isPresent)) 

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

    // ===== Context Variables (_) ===== //
    // _txType: Byte

    // ===== Tx Type Bytes ===== //
    // 1: Accept Session Tx
    // 2: Cancel Session Tx
    // 3: Unaccepted Session Tx
    // 4: Session Start: Client Confirmation Tx
    // 5: Session Start: Psychologist Confirmation Tx
    // 6: Session Start: Psychologist Not Present Tx
    // 7: Session Start: Client Not Present Tx

    // ===== Relevant Variables ===== //
    val _txType: Option[Byte] = getVar[Byte](0)

    val sessionSingletonId: Coll[Byte] = SELF.tokens(0)._1
    
    val sessionPriceTokenId: Coll[Byte] = SELF.tokens(1)._1
    val sessionPrice: Long = SELF.tokens(1)._1
    
    val sessionStartTimeBlockHeight: Int = SELF.R4[Int].get
    
    val clientSessionStatus: (Coll[Byte], Boolean) = SELF.R5[(Coll[Byte], Boolean)].get
    val clientAddressBytes: Coll[Byte] = clientSessionStatus._1
    val isClientPresent: Boolean = clientSessionStatus._2
    
    val psychologistSessionStatus: (Coll[Byte], (Boolean, Boolean)) = SELF.R6[(Coll[Byte], (Boolean, Boolean))].get
    val psychologistAddressBytes: Coll[Byte] = psychologistSessionStatus._1
    val isSessionAccepted: Boolean = psychologistSessionStatus._2._1
    val isPsychologistPresent: Boolean = psychologistSessionStatus._2._2

    val sesssionCancelationPeriod: Int = 720    // The cancelation period is 24hrs, thus since there is 1 block every 2 minutes on average, there are 720 blocks every 24hrs on average.
    val sessionUnacceptedPeriod: Int = 60       // If no psychologist accepts the session within 2hrs of the session start time, thus since there is 1 block every 2 minutes on average, there are 60 blocks every 2hrs on average.
    val fiveMinutes: Int = 3                    // 1 block every 2 minutes on average, so 2.5 blocks every 5 minutes on average, so we round up.
    val tenMinutes: Int = 5                     // 1 block every 2 minutes on average, so 5 blocks every 10 minutes on average.
    val fifteenMinutes: Int = 8                 // 1 block every 2 minutes on average, so 7.5 blocks every 15 minutes on average, so we round up.

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

                    (sessionBoxOut.R6[(Coll[Byte], (Boolean, Boolean))].get == (psychologistPKBoxIn.propositionBytes, (true, false)))

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
                    
                    (100L * collateral._2 == 10L * sessionPrice)
                    
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
                    (sessionBoxOut.R5[(Coll[Byte], Boolean)].get == (clientAddressBytes, false))
                ))

            }

            allOf(Coll(
                validPsychologistRegistration,
                validCollateralTransfer,
                validSessionRecreation
            ))

        }

        sigmaProp(validAcceptSessionTx)

    } else if (_txType.get == 2.toByte) {

        // ===== Cancel Session Tx ===== //
        val validCancelSessionTx: Boolean = {

            // Inputs
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

                        (psychologistPKBoxIn.propositionBytes == psychologistAddressBytes)

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
                    
                    (clientPKBoxOut.propositionBytes == clientAddressBytes)
                
                }

                val validClientRefundAmount: Boolean = {
                    
                    (clientPKBoxOut.tokens == (sessionPriceTokenId, sessionPrice))
                    
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
                    
                    (psychologistPKBoxOut.propositionBytes == psychologistAddressBytes)
                    
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

        sigmaProp(validCancelSessionTx)

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

                    (psychologistAddressBytes.size == 0)

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

                    (clientPKBoxOut.tokens == (sessionPriceTokenId, sessionPrice))
                
                }
                
                val validClientRefundAddress: Boolean = {

                    (clientPKBoxOut.propositionBytes == clientAddressBytes)

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

                    (clientPKBoxIn.propositionBytes == clientAddressBytes)

                }

                val validPsychologistSessionStatus: Boolean = {

                    (isSessionAccepted) // Client cannot confirm they are present before the psychologist has accepted the session.

                }

                val validClientSessionStatusUpdate: Boolean = {

                    (sessionBoxOut.R5[(Coll[Byte], Boolean)].get == (clientAddressBytes, true))

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
                    (sessionBoxOut.R6[(Coll[Byte], (Boolean, Boolean))].get == psychologistSessionStatus)
                ))

            }

            allOf(Coll(
                validClientConfirmation,
                validSessionStartTime,
                validSessionRecreation
            ))

        }

        sigmaProp(validSessionStartClientConfirmationTx)

    } else if (_txType.get == 5.toByte) {

        val validSessionStartPsychologistConfirmationTx: Boolean = {

            // Inputs
            val psychologistPKBoxIn: Box = INPUTS(1)

            // Outputs
            val sessionBoxOut: Box = OUTPUTS(0)

            val validPsychologistConfirmation: Boolean = {

                val validPsychologistAddressBytes: Boolean = {

                    (psychologistPKBoxIn.propositionBytes == psychologistAddressBytes)

                }

                val validPsychologistSessionStatus: Boolean = {

                    (isSessionAccepted) // Psychologist cannot confirm they are present before accepting the session.

                }

                val validPsychologistSessionStatusUpdate: Boolean = {

                    (sessionBoxOut.R6[(Coll[Byte], (Boolean, Boolean))].get == (psychologistAddressBytes, (isSessionAccepted, true)))

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

                            (clientPKBoxOut.propositionBytes == clientAddressBytes)

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
                    (sessionBoxOut.R5[(Coll[Byte], Boolean)].get == clientSessionStatus)
                ))

            }

            allOf(Coll(
                validPsychologistConfirmation,
                validSessionStartTime,
                validSessionRecreation
            ))

        }

        sigmaProp(validSessionStartPsychologistConfirmationTx)

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

                    val validPsychologistSessionAccepted: Boolean = (isSessionAccepted)
                    val validPsychologistSessionNotConfirmed: Boolean = (!isPsychologistPresent)
                    
                    allOf(Coll(
                        isSessionAccepted,
                        !isPsychologistPresent
                    ))

                }

                val validClientPKBoxOut: Boolean = {

                    val validClientAddressBytes: Boolean = {

                        (clientPKBoxOut.propositionBytes == clientAddressBytes)

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

                        (psychologistPKBoxOut.tokens(0) == (sessionPriceTokenId, sessionPrice + SELF))

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

        sigmaProp(validSessionStartClientNotPresentTx)

    } else {
        sigmaProp(false)
    }

}