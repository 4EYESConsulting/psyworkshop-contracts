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
    // R4: Boolean  isSessionAccepted

    // ===== Relevant Transactions ===== //
    // 1. Accept Session
    // Inputs: Session, PsychologistPK
    // Data Inputs: None
    // Outputs: Session
    // Context Variables: TxType

    // ===== Compile Time Constants ($) ===== //
    // $psyWorkshopRegistrationTokenId: Coll[Byte]

    // ===== Context Variables (_) ===== //
    // _txType: Byte

    // ===== Relevant Variables ===== //
    val _txType: Option[Byte] = getVar[Byte](0)
    val _psychologistRegistrationIssuerBox: Option[Box] = getVar[Box](1)
    val sessionPriceTokenId: Coll[Byte] = SELF.tokens(2)._1
    val sessionPrice: Long = SELF.tokens(2)._1

    if (_txType.get == 1.toByte) {

        // ===== Accept Session Tx ===== //
        val validAcceptSessionTx: Boolean = {

            // Inputs
            val psychologistBoxesIn: Box = INPUTS.slice(1, INPUTS.size)

            // Outputs
            val sessionBoxOut: Box = OUTPUTS(0)

            val validPsychologistRegistration: Boolean = { // TODO: May need to use AVL trees for this instead.

                psychologistBoxesIn.exists({ (box: Box) =>
                
                    box.tokens.exists({ (token: (Coll[Byte], Long)) =>

                        (token._1 == $psyWorkshopRegistrationTokenId)

                    })
                
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

            val validSessionRecreation: Boolean = {

                val isSessionAccepted: Boolean = sessionBoxOut.R4[Boolean].get

                allOf(Coll(
                    (SELF.value == sessionBoxOut.value),
                    (SELF.propositionBytes == sessionBoxOut.propositionBytes),
                    (SELF.tokens(0) == sessionBoxOut.tokens(0)),
                    (SELF.tokens(1) == sessionBoxOut.tokens(1)),
                    isSessionAccepted
                ))

            }

            allOf(Coll(
                validPsychologistRegistration,
                validCollateralTransfer,
                validSessionRecreation
            ))

        }

        sigmaProp(validAcceptSessionTx)

    } else {
        sigmaProp(false)
    }

    
}