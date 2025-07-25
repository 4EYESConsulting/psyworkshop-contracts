{

    // ===== Contract Information ===== //
    // Name: PsyWORKshop Session Contract
    // Description: Contract for the session box controlling how the client and psychologist interact with each other during a session.
    //              A client creates a request for a session, paying the session price.
    //              A psychologist can accept the request, providing collateral that can be claimed in the case of a dispute.
    // Version: 1.0.0
    // Author: Luca D'Angelo (ldgaetano@protonmail.com)

    // ===== Box Contents ===== //
    // Tokens
    // 1. (SessionSingletonId, 1)
    // 2. (SigUSDId, SessionPrice + ?Collateral) // If provided by the psychologist.
    //
    // Registers
    // R4: Int                              sessionStartTimeBlockHeight
    // R5: (SigmaProp, SigmaProp)           (clientAddressSigmaProp, pyschologistAddressSigmaProp) // Psychologist address is initially the client address before the session is accepted.
    // R6: (Coll[Byte], Coll[Byte])         (partnerLayerOneAddressBytes, partnerLayerTwoAddressBytes) // Empty Coll[Byte]() if not present.                            
    // R7: (Boolean, Boolean)               (isSessionAccepted, isSessionProblem) // Both false initially.
    // R8: Long                             sessionPrice
    // R9: Long                             collateral  // Assume 0 initially.

    // ===== Transactions ===== //
    // 1. Accept Session Tx
    // Inputs: Session, Psychologist
    // Data Inputs: None
    // Outputs: Session, Psycholgoist, 
    // Context Variables: TxType
    //
    // 2. Cancel Session Tx: Psychologist
    // Inputs: Session, PsychologistPK
    // Data Inputs: None
    // Outputs: Client, Psychologist, PsyWorkshopFee
    // Context Variables: TxType
    //
    // 3. Cancel Session Tx: Client
    // Inputs: Session, Client
    // Data Inputs: None
    // Outputs: Client, Psychologist, PsyWorkshopFee
    // Context Variables: TxType
    //
    // 4. Refund Tx: Client
    // Inputs: Session, Client
    // Data Inputs: None
    // Outputs: Client
    // Context Variables: TxType
    //
    // 5. Session End Tx: No Problem
    // Inputs: Session, Psychologist
    // Data Inputs: None
    // Outputs: Psychologist, ?PartnerLayerOneFee, ?PartnerLayerTwoFee, PsyWorkshopFee
    // Context Variables: TxType
    //
    // 6. Session End Tx: Problem
    // Inputs: Session, Client
    // Data Inputs: None
    // Outputs: Session
    // Context Variables: TxType
    //
    // 7. Session End Tx: Psychologist Bad
    // Inputs: Session, Admin
    // Data Inputs: None
    // Outputs: Client, PsyWorkshopFee
    //
    // 8. Session End Tx: Client Bad
    // Inputs: Session, Admin
    // Data Inputs: None
    // Outputs: Psychologist, ?PartnerLayerOneFee, ?PartnerLayerTwoFee, PsyWorkshopFee
    // Context Variables: TxType
    //
    // 9. Session End Tx: Psyworkshop Bad
    // Inputs: Session, Admin
    // Data Inputs: None
    // Outputs: Client, Psychologist
    // Context Variables: TxType

    // ===== Compile Time Constants ($) ===== //
    // $psyworkshopRegistrationTokenId: Coll[Byte]
    // $psyworkshopFeeAddressBytes: Coll[Byte]
    // $psyworkshopAdminSigmaProp: SigmaProp

    // ===== Context Variables (_) ===== //
    // _txType: Int

    // ===== Tx Type ===== //
    // 1: Accept Session Tx
    // 2: Cancel Session Tx: Psychologist
    // 3: Cancel Session Tx: Client
    // 4: Refund Tx: Client
    // 5: Session End Tx: No Problem
    // 6: Session End Tx: Problem
    // 7: Session End Tx: Psychologist Bad
    // 8: Session End Tx: Client Bad
    // 9: Session End Tx: Psyworkshop Bad

    // ===== Functions ===== //
    // def validRegistrationToken: Box => Boolean
    // def validSessionTermination: Coll[Byte] => Boolean
    // def isSigmaPropEqualToBoxProp: (SigmaProp, Box) => Boolean
    
    def validRegistrationToken(box: Box): Boolean = { 

        box.tokens.exists({ (token: (Coll[Byte], Long)) => {

            (token._1 == $psyworkshopRegistrationTokenId)

        }})

    }

    def validSessionTermination(sessionSingletonId: Coll[Byte]): Boolean = {

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

    val sessionSingletonId: Coll[Byte] = SELF.tokens(0)._1
    val sessionPriceTokenId: Coll[Byte] = SELF.tokens(1)._1
    val totalValue: Long = SELF.tokens(1)._2
    val sessionStartTimeBlockHeight: Int = SELF.R4[Int].get
    val clientAddressSigmaProp: SigmaProp = SELF.R5[(SigmaProp, SigmaProp)].get._1    
    val psychologistAddressSigmaProp: SigmaProp = SELF.R5[(SigmaProp, SigmaProp)].get._2
    val partnerLayerOneAddressBytes: Coll[Byte] = SELF.R6[(Coll[Byte], Coll[Byte])].get._1
    val partnerLayerTwoAddressBytes: Coll[Byte] = SELF.R6[(Coll[Byte], Coll[Byte])].get._2
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
    val isPsychologistSessionCancelTime: Boolean = (sessionStartTimeBlockHeight - CONTEXT.HEIGHT >= psychologistSessionCancelationPeriod)
    val isClientSessionCancelTime: Boolean = (sessionStartTimeBlockHeight - CONTEXT.HEIGHT >= clientSessionCancelationPeriod)
    val isClientSessionCancelTimePenalty: Boolean = (CONTEXT.HEIGHT - sessionStartTimeBlockHeight <= clientSessionCancelationPeriod) && (CONTEXT.HEIGHT - sessionStartTimeBlockHeight > 0)
    val isPartnerLayerOnePresent: Boolean = (partnerLayerOneAddressBytes.size > 0)
    val isPartnerLayerTwoPresent: Boolean = (partnerLayerTwoAddressBytes.size > 0)

    if (_txType.get == 1) {

        // ===== Accept Session Tx ===== //
        val validAcceptSessionTx: Boolean = {

            // Inputs
            val psychologistPKBoxIn: Box = INPUTS(1)

            // Outputs
            val sessionBoxOut: Box = OUTPUTS(0)
            val psychologistPKBoxOut: Box = OUTPUTS(1)

            val validPsychologistRegistration: Boolean = {

                val validSessionStatusUpdate: Boolean = {

                    val outPsychologistAddress: SigmaProp = sessionBoxOut.R5[(SigmaProp, SigmaProp)].get._2
                    val outStatus: (Boolean, Boolean) = sessionBoxOut.R7[(Boolean, Boolean)].get
                    val propAndBoxPsych: (SigmaProp, Box) = (outPsychologistAddress, psychologistPKBoxIn)

                    allOf(Coll(
                        (outPsychologistAddress !=  $psyworkshopAdminSigmaProp),
                        isSigmaPropEqualToBoxProp(propAndBoxPsych),
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
                    (sessionBoxOut.R5[(SigmaProp, SigmaProp)].get._1 == clientAddressSigmaProp),
                    (sessionBoxOut.R6[(Coll[Byte], Coll[Byte])].get == SELF.R6[(Coll[Byte], Coll[Byte])].get),
                    (sessionBoxOut.R8[Long].get == sessionPrice)
                ))

            }

            val validPsychologistBoxOut: Boolean = {

                allOf(Coll(
                    (psychologistPKBoxOut.propositionBytes == psychologistPKBoxIn.propositionBytes),
                    validRegistrationToken(psychologistPKBoxOut)
                ))

            }

            allOf(Coll(
                validPsychologistRegistration,
                validCollateralTransfer,
                validSessionRecreation,
                validPsychologistBoxOut
            ))

        }

        sigmaProp(validAcceptSessionTx)

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

                val propAndBoxPsych: (SigmaProp, Box) = (psychologistAddressSigmaProp, psychologistPKBoxIn)

                allOf(Coll(
                    isSigmaPropEqualToBoxProp(propAndBoxPsych),
                    validRegistrationToken(psychologistPKBoxIn)
                ))

            }

            val validClientRefundBoxOut: Boolean = {

                val propAndBoxClient: (SigmaProp, Box) = (clientAddressSigmaProp, clientPKBoxOut)

                val validClientRefundAmount: Boolean = (clientPKBoxOut.tokens(0) == (sessionPriceTokenId, sessionPrice))

                allOf(Coll(
                    isSigmaPropEqualToBoxProp(propAndBoxClient),
                    validClientRefundAmount
                ))

            }

            val validPsychologistPKBoxOut: Boolean = {

                val validPsychologistAddressBytes: Boolean = (psychologistPKBoxOut.propositionBytes == psychologistPKBoxIn.propositionBytes)

                // The fee is 50% of the collateral provided by the psychologist.
                val validCollateralAmount: Boolean = {

                    allOf(Coll(
                        (psychologistPKBoxOut.tokens(0)._1 == sessionPriceTokenId),
                        (psychologistPKBoxOut.tokens(0)._2 == collateral / 2)
                    ))

                }

                allOf(Coll(
                    validPsychologistAddressBytes,
                    validCollateralAmount,
                    validRegistrationToken(psychologistPKBoxOut)
                ))

            }

            val validPsyworkshopFeeBoxOut: Boolean = {

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
                validSessionTermination(sessionSingletonId)
            ))

        }

        sigmaProp(validCancelSessionPsychologistTx)

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

            val propAndBoxClient: (SigmaProp, Box) = (clientAddressSigmaProp, clientPKBoxIn)
            val validClient: Boolean = isSigmaPropEqualToBoxProp(propAndBoxClient)

            val validClientRefundBoxOut: Boolean = {

                val validClientRefundAddressBytes: Boolean = (clientPKBoxOut.propositionBytes == clientPKBoxIn.propositionBytes)

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

                val propAndBoxPsych: (SigmaProp, Box) = (psychologistAddressSigmaProp, psychologistPKBoxOut)
                val validPsychologistAddressBytes: Boolean = isSigmaPropEqualToBoxProp(propAndBoxPsych)

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

            val validPsyworkshopFeeBoxOut: Boolean = {

                if (isClientSessionCancelTimePenalty) {

                    val validFeeAddressBytes: Boolean = (psyworkshopFeeBoxOut.propositionBytes == $psyworkshopFeeAddressBytes)
                
                    // The fee is 25% of the sessionPrice provided by the client, only if the penalty time is reached.
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
                validSessionTermination(sessionSingletonId)
            ))

        }

        sigmaProp(validCancelSessionClientTx)

    } else if (_txType.get == 4) {

        // ===== Refund Tx: Client ===== //
        val validClientRefundTx: Boolean = {

            // Inputs
            val clientPKBoxIn: Box = INPUTS(1)

            // Outputs
            val clientPKBoxOut: Box = OUTPUTS(0)

            val propAndBoxClient: (SigmaProp, Box) = (clientAddressSigmaProp, clientPKBoxIn)
            val validClient: Boolean = isSigmaPropEqualToBoxProp(propAndBoxClient)

            val validClientRefundBoxOut: Boolean = {

                val validClientRefundAddressBytes: Boolean = (clientPKBoxOut.propositionBytes == clientPKBoxIn.propositionBytes)

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
                validSessionTermination(sessionSingletonId)
            ))            

        }

        sigmaProp(validClientRefundTx)

    } else if (_txType.get == 5) {

        // ===== Session End: No Problem ===== //
        // Psychologist claims the reward if there is no problem after 15 minutes of the session end.
        val validSessionEndNoProblemTx: Boolean = {

            // Inputs
            val psychologistPKBoxIn: Box = INPUTS(1)

            // Outputs
            val psychologistPKBoxOut: Box = OUTPUTS(0)
            val partnerLayerOneFeeBoxOut: Box = OUTPUTS.getOrElse(1, SELF)
            val partnerLayerTwoFeeBoxOut: Box = OUTPUTS.getOrElse(2, SELF)
            val psyworkshopFeeBoxOut: Box = {
                if (isPartnerLayerOnePresent && isPartnerLayerTwoPresent) {
                    OUTPUTS(3)
                } else if (isPartnerLayerOnePresent) {
                    OUTPUTS(2)
                } else {
                    OUTPUTS(1)
                }
            }

            val psychFee: Long = ((800 * sessionPrice) / 1000)
            val partnerLayerOneFee: Long = if (isPartnerLayerOnePresent) ((120 * sessionPrice) / 1000) else 0L
            val partnerLayerTwoFee: Long = if (isPartnerLayerTwoPresent) ((30 * sessionPrice) / 1000) else 0L 
            val workshopFee: Long = sessionPrice - (psychFee + partnerLayerOneFee + partnerLayerTwoFee)

            val validPsychologist: Boolean = {

                val propAndBoxPsych: (SigmaProp, Box) = (psychologistAddressSigmaProp, psychologistPKBoxIn)
                val validAddressBytes: Boolean = isSigmaPropEqualToBoxProp(propAndBoxPsych)

                allOf(Coll(
                    validAddressBytes,
                    validRegistrationToken(psychologistPKBoxIn)
                ))

            } 

            val validPsychologistBoxOut: Boolean = {

                val validPsychologistAddressBytes: Boolean = (psychologistPKBoxOut.propositionBytes == psychologistPKBoxIn.propositionBytes)

                val validSessionPriceAmount: Boolean = {

                    allOf(Coll(
                        (psychologistPKBoxOut.tokens(0)._1 == sessionPriceTokenId),
                        (psychologistPKBoxOut.tokens(0)._2 == collateral + psychFee)
                    ))

                }

                allOf(Coll(
                    validPsychologistAddressBytes,
                    validSessionPriceAmount,
                    validRegistrationToken(psychologistPKBoxOut)
                ))

            }

            val validLayerOneBoxOut: Boolean = {

                if (isPartnerLayerOnePresent) {

                    val validFeeAddressBytes: Boolean = (partnerLayerOneFeeBoxOut.propositionBytes == partnerLayerOneAddressBytes)

                    val validFeeAmount: Boolean = {

                        allOf(Coll(
                            (partnerLayerOneFeeBoxOut.tokens(0)._1 == sessionPriceTokenId),
                            (partnerLayerOneFeeBoxOut.tokens(0)._2 == partnerLayerOneFee)
                        ))

                    }

                    allOf(Coll(
                        validFeeAddressBytes,
                        validFeeAmount
                    ))

                } else {
                    true
                }

            }

            val validLayerTwoBoxOut: Boolean = {

                if (isPartnerLayerTwoPresent && isPartnerLayerOnePresent) {

                    val validFeeAddressBytes: Boolean = (partnerLayerTwoFeeBoxOut.propositionBytes == partnerLayerTwoAddressBytes)

                    val validFeeAmount: Boolean = {

                        allOf(Coll(
                            (partnerLayerTwoFeeBoxOut.tokens(0)._1 == sessionPriceTokenId),
                            (partnerLayerTwoFeeBoxOut.tokens(0)._2 == partnerLayerTwoFee)
                        ))

                    }

                    allOf(Coll(
                        validFeeAddressBytes,
                        validFeeAmount
                    ))

                } else {
                    true
                }

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
                isSessionOver,
                isSessionComplaintTimeOver,
                isSessionAccepted,
                !isSessionProblem,
                validPsychologist,
                validPsychologistBoxOut,
                validLayerOneBoxOut,
                validLayerTwoBoxOut,
                validPsyworkshopFeeBoxOut,
                validSessionTermination(sessionSingletonId)
            ))

        }

        sigmaProp(validSessionEndNoProblemTx)

    } else if (_txType.get == 6) {

        // ===== Session End: Problem Tx ===== //
        val validSessionEndProblemTx: Boolean = {

            // Inputs
            val clientPKBoxIn: Box = INPUTS(1)

            // Outputs
            val sessionBoxOut: Box = OUTPUTS(0)

            val propAndBoxClient: (SigmaProp, Box) = (clientAddressSigmaProp, clientPKBoxIn)
            val validClient: Boolean = isSigmaPropEqualToBoxProp(propAndBoxClient)

            val validSessionStatusUpdate: Boolean = (sessionBoxOut.R7[(Boolean, Boolean)].get._2 == true)

            val validSessionRecreation: Boolean = {

                allOf(Coll(
                    (sessionBoxOut.value == SELF.value),
                    (sessionBoxOut.propositionBytes == SELF.propositionBytes),
                    (sessionBoxOut.tokens(0) == SELF.tokens(0)),
                    (sessionBoxOut.tokens(1) == SELF.tokens(1)),
                    (sessionBoxOut.R4[Int].get == SELF.R4[Int].get),
                    (sessionBoxOut.R5[(SigmaProp, SigmaProp)].get == SELF.R5[(SigmaProp, SigmaProp)].get),
                    (sessionBoxOut.R6[(Coll[Byte], Coll[Byte])].get == SELF.R6[(Coll[Byte], Coll[Byte])].get),
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

        sigmaProp(validSessionEndProblemTx)

    } else if (_txType.get == 7) {

        // ===== Session End: Psychologist Bad ===== //
        val validSessionEndPsychologistBadTx: Boolean = {

            // Inputs
            val adminPKBoxIn: Box = INPUTS(1)

            // Outputs
            val clientPKBoxOut: Box = OUTPUTS(0)
            val psyworkshopFeeBoxOut: Box = OUTPUTS(1)

            val clientAmount: Long = (collateral / 2) + sessionPrice
            val workshopFee: Long = collateral - (collateral / 2)

            val propAndBoxAdmin: (SigmaProp, Box) = ($psyworkshopAdminSigmaProp, adminPKBoxIn)
            val validAdmin: Boolean = isSigmaPropEqualToBoxProp(propAndBoxAdmin)

            val validClientRefundBoxOut: Boolean = {

                val propAndBoxClient: (SigmaProp, Box) = (clientAddressSigmaProp, clientPKBoxOut)
                val validClientRefundAddressBytes: Boolean = isSigmaPropEqualToBoxProp(propAndBoxClient)

                val validClientRefundAmount: Boolean = {
                    
                    allOf(Coll(
                        (clientPKBoxOut.tokens(0)._1 == sessionPriceTokenId),
                        (clientPKBoxOut.tokens(0)._2 == clientAmount)
                    ))
                    
                }

                allOf(Coll(
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
                validSessionTermination(sessionSingletonId)
            ))

        }

        sigmaProp(validSessionEndPsychologistBadTx)

    } else if (_txType.get == 8) {

        // ===== Session End: Client Bad ===== //
        val validSessionEndClientBadTx: Boolean = {

            // Inputs
            val adminPKBoxIn: Box = INPUTS(1)

            // Outputs
            val psychologistPKBoxOut: Box = OUTPUTS(0)
            val partnerLayerOneFeeBoxOut: Box = OUTPUTS.getOrElse(1, SELF)
            val partnerLayerTwoFeeBoxOut: Box = OUTPUTS.getOrElse(2, SELF)
            val psyworkshopFeeBoxOut: Box = {
                if (isPartnerLayerOnePresent && isPartnerLayerTwoPresent) {
                    OUTPUTS(3)
                } else if (isPartnerLayerOnePresent) {
                    OUTPUTS(2)
                } else {
                    OUTPUTS(1)
                }
            }

            val psychFee: Long = ((800 * sessionPrice) / 1000)
            val partnerLayerOneFee: Long = if (isPartnerLayerOnePresent) ((120 * sessionPrice) / 1000) else 0L
            val partnerLayerTwoFee: Long = if (isPartnerLayerTwoPresent) ((30 * sessionPrice) / 1000) else 0L 
            val workshopFee: Long = sessionPrice - (psychFee + partnerLayerOneFee + partnerLayerTwoFee)

            val propAndBoxAdmin: (SigmaProp, Box) = ($psyworkshopAdminSigmaProp, adminPKBoxIn)
            val validAdmin: Boolean = isSigmaPropEqualToBoxProp(propAndBoxAdmin)

            val validPsychologistBoxOut: Boolean = {

                val propAndBoxPsych: (SigmaProp, Box) = (psychologistAddressSigmaProp, psychologistPKBoxOut)
                val validPsychologistAddressBytes: Boolean = isSigmaPropEqualToBoxProp(propAndBoxPsych)

                val validSessionPriceAmount: Boolean = {

                    allOf(Coll(
                        (psychologistPKBoxOut.tokens(0)._1 == sessionPriceTokenId),
                        (psychologistPKBoxOut.tokens(0)._2 == collateral + psychFee)
                    ))

                }

                allOf(Coll(
                    validPsychologistAddressBytes,
                    validSessionPriceAmount
                ))

            }

            val validLayerOneBoxOut: Boolean = {

                if (isPartnerLayerOnePresent) {

                    val validFeeAddressBytes: Boolean = (partnerLayerOneFeeBoxOut.propositionBytes == partnerLayerOneAddressBytes)

                    val validFeeAmount: Boolean = {

                        allOf(Coll(
                            (partnerLayerOneFeeBoxOut.tokens(0)._1 == sessionPriceTokenId),
                            (partnerLayerOneFeeBoxOut.tokens(0)._2 == partnerLayerOneFee)
                        ))

                    }

                    allOf(Coll(
                        validFeeAddressBytes,
                        validFeeAmount
                    ))

                } else {
                    true
                }

            }

            val validLayerTwoBoxOut: Boolean = {

                if (isPartnerLayerTwoPresent && isPartnerLayerOnePresent) {

                    val validFeeAddressBytes: Boolean = (partnerLayerTwoFeeBoxOut.propositionBytes == partnerLayerTwoAddressBytes)

                    val validFeeAmount: Boolean = {

                        allOf(Coll(
                            (partnerLayerTwoFeeBoxOut.tokens(0)._1 == sessionPriceTokenId),
                            (partnerLayerTwoFeeBoxOut.tokens(0)._2 == partnerLayerTwoFee)
                        ))

                    }

                    allOf(Coll(
                        validFeeAddressBytes,
                        validFeeAmount
                    ))

                } else {
                    true
                }

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
                validLayerOneBoxOut,
                validLayerTwoBoxOut,
                validPsyworkshopFeeBoxOut,
                validSessionTermination(sessionSingletonId)
            ))

        }

        sigmaProp(validSessionEndClientBadTx)
        
    } else if (_txType.get == 9) {

        // ===== Session End: Service Bad ===== //
        val validSessionEndServiceBadTx: Boolean = {

            // Inputs
            val adminPKBoxIn: Box = INPUTS(1)

            // Outputs
            val clientPKBoxOut: Box = OUTPUTS(0)
            val psychologistPKBoxOut: Box = OUTPUTS(1)

            val propAndBoxAdmin: (SigmaProp, Box) = ($psyworkshopAdminSigmaProp, adminPKBoxIn)
            val validAdmin: Boolean = isSigmaPropEqualToBoxProp(propAndBoxAdmin)

            val validClientRefundBoxOut: Boolean = {

                val propAndBoxClient: (SigmaProp, Box) = (clientAddressSigmaProp, clientPKBoxOut)
                val validClientRefundAddressBytes: Boolean = isSigmaPropEqualToBoxProp(propAndBoxClient)

                val validClientRefundAmount: Boolean = {
                    
                    allOf(Coll(
                        (clientPKBoxOut.tokens(0)._1 == sessionPriceTokenId),
                        (clientPKBoxOut.tokens(0)._2 == sessionPrice)
                    ))
                    
                }

                allOf(Coll(
                    validClientRefundAddressBytes,
                    validClientRefundAmount
                ))

            }            

            val validPsychologistBoxOut: Boolean = {

                val propAndBoxPsych: (SigmaProp, Box) = (psychologistAddressSigmaProp, psychologistPKBoxOut)
                val validPsychologistAddressBytes: Boolean = isSigmaPropEqualToBoxProp(propAndBoxPsych)

                val validSessionPriceAmount: Boolean = {

                    allOf(Coll(
                        (psychologistPKBoxOut.tokens(0)._1 == sessionPriceTokenId),
                        (psychologistPKBoxOut.tokens(0)._2 == collateral)
                    ))

                }

                allOf(Coll(
                    validPsychologistAddressBytes,
                    validSessionPriceAmount
                ))

            }

            allOf(Coll(
                isSessionProblem,
                validAdmin,
                validClientRefundBoxOut,
                validPsychologistBoxOut,
                validSessionTermination(sessionSingletonId)
            ))

        }

        sigmaProp(validSessionEndServiceBadTx)
    
    } else {
        sigmaProp(false)
    }

}