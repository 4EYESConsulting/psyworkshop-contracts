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

    // ===== Tx Types ===== //
    // 1: Accept Session Tx
    // 2: Cancel Session Tx
    // 3: Unaccepted Session Tx
    // 4: Session Start: Client Confirmation Tx
    // 5: Session Start: Psychologist Confirmation Tx
    // 6: Session Start: Psychologist Not Present Tx

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
    val isPschologistPresent: Boolean = psychologistSessionStatus._2._2

    val sesssionCancelationPeriod: Int = 720    // The cancelation period is 24hrs, thus since there is 1 block every 2 minutes on average, there are 720 blocks every 24hrs on average.
    val sessionUnacceptedPeriod: Int = 60       // If no psychologist accepts the session within 2hrs of the session start time, thus since there is 1 block every 2 minutes on average, there are 60 blocks every 2hrs on average.

    if (_txType.get == 1.toByte) {

        // ===== Accept Session Tx ===== //
        val validAcceptSessionTx: Boolean = {

            // Inputs
            val psychologistBoxIn: Box = INPUTS(1)

            // Outputs
            val sessionBoxOut: Box = OUTPUTS(0)

            val validPsychologistRegistration: Boolean = {
    
                val validRegistrationToken: Boolean = { 

                    psychologistBoxIn.tokens.exists( (token: (Coll[Byte], Long)) => {

                        (token._1 == $psyworkshopRegistrationTokenId)

                    })

                }

                val validSessionStatusUpdate: Boolean = {

                    (sessionBoxOut.R6[(Coll[Byte], (Boolean, Boolean))].get == (psychologistBoxIn.propositionBytes, (true, false)))

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
            val psychologistBox: Box = INPUTS(1)

            // Outputs
            val clientPKBoxOut: Box = OUTPUTS(0)
            val psychologistPKBoxOut: Box = OUTPUTS(1)
            val psyworkshopFeeBoxOut: Box = OUTPUTS(2)

            val validCancelationPeriod: Boolean = {
                
                (sessionStartTimeBlockHeight - CONTEXT.HEIGHT >= sesssionCancelationPeriod)
                
            }

            val validPsychologist: Boolean = {

                val validRegistrationToken: Boolean = { 

                    psychologistBoxIn.tokens.exists( (token: (Coll[Byte], Long)) => {

                        (token._1 == $psyworkshopRegistrationTokenId)

                    })

                }

                val validSession: Boolean = {

                    val validAddress: Boolean = {

                        (psychologistBoxIn.propositionBytes == psychologistAddressBytes)

                    }

                    val validAcceptedSession: Boolean = {

                        (isSessionAccepted)

                    }

                    allOf(Coll(
                        validAddress,
                        validAcceptedSession
                    ))

                }

                allOf(Coll(
                    validRegistrationToken,
                    validSession
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

            val validPsychologistBoxOut: Boolean = {

                // The fee is 50% of the collateral provided by the psychologist.
                val validCollateralAmount: Boolean = {

                    allOf(Coll(
                        (psychologistPKBoxOut.tokens(0)._1 == SELF.tokens(2)._1),
                        (psychologistPKBoxOut.tokens(0)._2 * 2 == SELF.tokens(2)._2)
                    ))

                }

                val validPsychologistAddress: Boolean = {
                    
                    (psychologistPKBoxOut.propositionBytes == psychologistAddressBytes)
                    
                }

                allOf(Coll(
                    validCollateralAmount,
                    validPsychologistAddress
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
                        (psyworkshopFeeBoxOut.tokens(0)._2 * 2 == SELF.tokens(2)._2)
                    ))

                }

                val validFeeAddress: Boolean = {
                    
                    (psyworkshopFeeBoxOut.propositionBytes == $psyworkshopFeeAddressBytes)
                    
                }

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
                    
                    (token_id != $psyworkshopRegistrationTokenId) 
                
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
                val validNoAddress: Boolean = {

                    (Coll[Byte]() == psychologistAddressBytes)

                }

                val validSessionNotAccepted: Boolean = {

                    (!isSessionAccepted)

                }

                val validPsychologistNotPresent: Boolean = {

                    (!isPschologistPresent)

                }

                allOf(Coll(
                    validNoAddress,
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

    } else if (_txType.get == 4.toByte) {

        // ===== Session Start: Client Confirmation Tx ===== //
        val validSessionStartClientConfirmationTx: Boolean = {

            // Inputs
            val clientPKBoxIn: Box = INPUTS(1)

            // Outputs
            val sessionBoxOut: Box = OUTPUTS(0)

            val validClientConfirmation: Boolean = {

                val validClientAddress: Boolean = {

                    (clientPKBoxIn.propositionBytes == clientAddressBytes)

                }


                val validPsychologistSessionStatus: Boolean = {

                    (isSessionAccepted) // Client cannot confirm they are present before the psychologist has accepted the session.

                }

                val validClientSessionStatusUpdate: Boolean = {

                    (sessionBoxOut.R5[(Coll[Byte], Boolean)].get == (clientAddressBytes, true))

                }

                allOf(Coll(
                    validClientAddress,
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

                val validPsychologistAddress: Boolean = {

                    (psychologistPKBoxIn.propositionBytes == psychologistAddressBytes)

                }


                val validPsychologistSessionStatus: Boolean = {

                    (isSessionAccepted) // Psychologist cannot confirm they are present before accepting the session.

                }

                val validPsychologistSessionStatusUpdate: Boolean = {

                    (sessionBoxOut.R6[(Coll[Byte], (Boolean, Boolean))].get == (psychologistAddressBytes, (isSessionAccepted, true)))

                }

                allOf(Coll(
                    validPsychologistAddress,
                    validPsychologistSessionStatus,
                    validPsychologistSessionStatusUpdate
                ))                

            }

            val validSessionRecreation: Boolean = {

                allOf(Coll(
                    (sessionBoxOut.value == SELF.value),
                    (sessionBoxOut.propositionBytes == SELF.propositionBytes),
                    (sessionBoxOut.tokens(0) == (sessionSingletonId, 1L)),
                    (sessionBoxOut.tokens(1) == (sessionPriceTokenId, sessionPrice)),
                    (sessionBoxOut.tokens(2) == SELF.tokens(2)) // Psychologist collateral.
                    (sessionBoxOut.R4[Int].get == sessionStartTimeBlockHeight),
                    (sessionBoxOut.R5[(Coll[Byte], Boolean)].get == clientSessionStatus)
                ))

            }

            allOf(Coll(
                validPsychologistConfirmation,
                validSessionRecreation
            ))

        }

        sigmaProp(validSessionStartPsychologistConfirmationTx)

    } else if (_txType.get == 6.toByte) {

        // ===== Session Start: Psychologist Not Present Tx ===== //
        val validSessionStartPsychologistNotPresentTx: Boolean = {

            

        }

        sigmaProp(validSessionStartPsychologistNotPresentTx)

    } else {
        sigmaProp(false)
    }

}