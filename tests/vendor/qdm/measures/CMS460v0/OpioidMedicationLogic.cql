library OpioidMedicationLogic version '1.17.000'

using QDM version '5.3'

include OpioidData version '0.8.000' called OpioidDataLibrary

codesystem "LOINC": 'urn:oid:2.16.840.1.113883.6.1'
codesystem "SNOMEDCT": 'urn:oid:2.16.840.1.113883.6.96'

code "Birthdate": '21112-8' from "LOINC" display 'Birthdate'
code "Dead": '419099009' from "SNOMEDCT" display 'Dead'

parameter "Measurement Period" Interval<DateTime>

context Patient

define function "ToDaily"(period System.Quantity, frequency Integer):
	/* This function calculates the daily dosage of medication using the Period (Years, Months, Weeks, Days, Hours, Minutes or Seconds) to determine frequency */
	case period.unit
		when 'h' then frequency * ( 24.0 / period.value )
		when 'min' then frequency * ( 24.0 / period.value ) * 60
		when 's' then frequency * ( 24.0 / period.value ) * 60 * 60
		when 'd' then frequency * ( 24.0 / period.value ) / 24
		when 'wk' then frequency * ( 24.0 / period.value ) / ( 24 * 7 )
		when 'mo' then frequency * ( 24.0 / period.value ) / ( 24 * 30 ) /* assuming 30 days in month */
		when 'a' then frequency * ( 24.0 / period.value ) / ( 24 * 365 ) /* assuming 365 days in year */ 
		else null 
	end

define function "GetIngredients"(rxNormCode Code):
	/* This function matches the Drug code to the Opioid Data Library Drug Code, then returns the Drug Code, Drug Name, Dose Form, Strength and Unit*/
	OpioidDataLibrary."DrugIngredients" DrugIngredientElement
		where DrugIngredientElement.drugCode = ToInteger(rxNormCode.code)
		return {
			rxNormCode: Code { code: ToString(DrugIngredientElement.drugCode), system: '2.16.840.1.113883.6.88', display: DrugIngredientElement.drugName },
			doseFormCode: Code { code: ToString(DrugIngredientElement.doseFormCode), system: '2.16.840.1.113883.6.88', display: DrugIngredientElement.doseFormName },
			doseFormName: DrugIngredientElement.doseFormName,
			ingredientCode: Code { code: ToString(DrugIngredientElement.ingredientCode), system: '2.16.840.1.113883.6.88', display: DrugIngredientElement.ingredientName },
			ingredientName: DrugIngredientElement.ingredientName,
			strength: EnsureMicrogramQuantity(Quantity { value: DrugIngredientElement.strengthValue, unit: ToUnifiedCodeForUnitsOfMeasure(DrugIngredientElement.strengthUnit)})
		}

define function "IsTransdermalPatch"(doseFormCode Code):
	/*This function examines Drug Form Code, and if given dose form is a patch (transdermal), then returns 'true' for any functions using that value*/
	ToInteger(doseFormCode.code)= 316987

define function "CalculateMorphineMilligramEquivalents"(medications List<Tuple {
	rxNormCode Code,
	doseQuantity Quantity,
	dosesPerDay Decimal
}>):
	/* This function returns the MME dosage using values passed from the GetIngredients, EnsureMicrogramQuantity, GetMedicationDailyDose, GetMedDailyDoseDescription, GetConversionFactor functions*/
	Flatten(medications Med
			let Ingredients: "GetIngredients"(Med.rxNormCode)
			return Ingredients Drug
				let adjustedDoseQuantity: "EnsureMicrogramQuantity"(Med.doseQuantity),
				dailyDose: "GetMedicationDailyDose"(Drug.ingredientCode, Drug.strength, Drug.doseFormCode, adjustedDoseQuantity, Med.dosesPerDay),
				factor: "GetConversionFactor"(Drug.ingredientCode, dailyDose, Drug.doseFormCode),
				dailyDoseDescription: "GetMedDailyDoseDescription"(Drug.ingredientCode, Drug.ingredientName, Drug.strength, Drug.doseFormCode, Drug.doseFormName, adjustedDoseQuantity, Med.dosesPerDay, dailyDose),
				MME: Quantity { value: dailyDose.value * factor, unit: dailyDose.unit + '/d' }
				return {
					rxNormCode: Med.rxNormCode,
					doseFormCode: Drug.doseFormCode,
					doseQuantity: adjustedDoseQuantity,
					dosesPerDay: Med.dosesPerDay,
					ingredientCode: Drug.ingredientCode,
					ingredientName: Drug.ingredientName,
					strength: Drug.strength,
					dailyDose: dailyDose,
					dailyDoseDescription: dailyDoseDescription,
					conversionFactor: factor,
					MME: MME
				}
	)

define function "GetConversionFactor"(ingredientCode Code, dailyDose Quantity, doseFormCode Code):
	/* This function specifies the conversion factor (based on the Ingredient Code) to be used by the Calculate MME function */
	case ToInteger(ingredientCode.code)
		when 161 then 0  /*	Acetaminophen */
		when 1191 then 0 /*	Aspirin */
		when 1223 then 0 /*	Atropine */
		when 1767 then 0 /*	Brompheniramine */
		when 1819 then ( /*	Buprenorphine */ case
				when ToInteger(doseFormCode.code)= 316987 then 12.6 /* Transdermal system */ 
				else 30 /* Tablet or Film (or Film in MCG) */ 
			end
		)
		when 1841 then 7 /*	Butorphanol */
		when 1886 then 0 /*	Caffeine */
		when 2101 then 0 /*	Carisoprodol */
		when 2354 then 0 /*	chlorcyclizine */
		when 2400 then 0 /*	Chlorpheniramine */
		when 2670 then 0.15 /*	Codeine */
		when 3423 then 4 /*	Hydromorphone */
		when 3498 then 0 /*	Diphenhydramine */
		when 4337 then ( /*	Fentanyl */ case
				when ToInteger(doseFormCode.code)in { 970789, 317007, 316992 } then 0.13 /* Buccal Tablet, Sublingual Tablet, Oral Lozenge */
				when ToInteger(doseFormCode.code)= 346163 then 0.18 /* Buccal Film */
				when ToInteger(doseFormCode.code)in { 126542, 346163 } then 0.16 /* Nasal Spray, Mucosal Spray */
				when "IsTransdermalPatch"(doseFormCode)then 7.2 /* Transdermal system */ 
				else 1000 
			end
		)
		when 5032 then 0 /*	Guaifenesin */
		when 5489 then 1 /*	Hydrocodone */
		when 5640 then 0 /*	Ibuprofen */
		when 6102 then 0 /*	Kaolin */
		when 6378 then 11 /*	Levorphanol (NOTE: Given as Levorphanol tartrate in the CDC conversion table) */
		when 6754 then 0.1 /*	Meperidine */
		when 6813 then ( /*	Methadone */ case
				when dailyDose.value between 1 and 20 then 4
				when dailyDose.value between 21 and 40 then 8
				when dailyDose.value between 41 and 60 then 10
				when dailyDose.value >= 61 then 12 
				else 1000 
			end
		)
		when 7052 then 1 /*	Morphine */
		when 7238 then 1 /*Nalbuphine*/
		when 7242 then 0 /*	Naloxone */
		when 7243 then 0 /*	Naltrexone */
		when 7676 then 1 /*Opium*/
		when 7804 then 1.5 /*	Oxycodone */
		when 7814 then 3 /*	Oxymorphone */
		when 8001 then 0.37 /*	Pentazocine */
		when 8163 then 0 /*	Phenylephrine */
		when 8175 then 0 /*	Phenylpropanolamine */
		when 8745 then 0 /*	Promethazine */
		when 8896 then 0 /*	Pseudoephedrine */
		when 9009 then 0 /*	Pyrilamine */
		when 10689 then 0.1 /*	Tramadol */
		when 10849 then 0 /*	Triprolidine */
		when 19759 then 0 /*	bromodiphenhydramine */
		when 19860 then 0 /*	butalbital */
		when 22696 then 0 /*	dexbrompheniramine */
		when 22697 then 0 /*	dexchlorpheniramine */
		when 23088 then 0.25 /*	dihydrocodeine */
		when 27084 then 0 /*	homatropine */
		when 35780 then 0 /*	ropivacaine */
		when 237005 then 8 /*	Levomethadyl (NOTE: given as Levomethadyl acetate in the CDC conversion table) */
		when 636827 then 0 /*	guaiacolsulfonate */
		when 787390 then 0.4 /*	tapentadol */ 
		else 0 
	end

define function "GetMedDailyDoseDescription"(ingredientCode Code, ingredientName String, strength Quantity, doseFormCode Code, doseFormName String, doseQuantity Quantity, dosesPerDay Decimal, dailyDose Quantity):
	/* This function uses the Dose Code, Dose Form, Dose Strength and Dose Unit to determine the drug Name that will be used by the Calculate MME function*/
	case
	    /* if patch */
		when "IsTransdermalPatch"(doseFormCode)then
	      /* buprenorphine or fentanyl patch */ if ToInteger(ingredientCode.code)in { 1819, 4337 } then ingredientName + ' patch: ' + ToString(doseQuantity.value)+ ' * ' + ToString(strength)+ ' = ' + ToString(dailyDose)
			else null
	
	
	    /* if dose unit in actual mass units (mg or mcg -- when it's a single med) */
		when doseQuantity.unit in { 'mg', 'mcg' } then ingredientName + ' ' + doseFormName + ': ' + ToString(dosesPerDay)+ '/d * ' + ToString(doseQuantity)+ ' = ' + ToString(dailyDose)
	
	
	    /* if doseQuantity in actual volume units (mL) or not in actual units (e.g. 1 tab, 1 spray) */
		else ingredientName + ' ' + doseFormName + ': ' + ToString(dosesPerDay)+ '/d * ' + ToString(doseQuantity)+ ' * ' + ToString(strength)+ ' = ' + ToString(dailyDose)
	end

define function "GetMedicationDailyDose"(ingredientCode Code, strength Quantity, doseFormCode Code, doseQuantity Quantity, dosesPerDay Decimal):
	/* This function uses the Dose Code, Dose Form, Dose Strength and Dose Unit to determine the drug Quantity that will be used by the Calculate MME function*/
	case
	  /* if patch --> daily dose = dose value (e.g, number patches with doseQuantity unit = "patch") * per-hour strength */
		when "IsTransdermalPatch"(doseFormCode)then
	      /* buprenorphine or fentanyl patch */ if ToInteger(ingredientCode.code)in { 1819, 4337 } then Quantity { value: doseQuantity.value * strength.value, unit: strength.unit } 
			else null
	
	
	    /* if dose unit in actual mass units (mg or mcg -- when it's a single med) --> daily dose = numTimesPerDay * dose */
		when doseQuantity.unit in { 'mg', 'mcg' } then Quantity { value: dosesPerDay * doseQuantity.value, unit: doseQuantity.unit }
	
	
	    /* if doseQuantity is in actual volume units (mL) --> daily dose = numTimesPerDay * dose * strength */
		when doseQuantity.unit = 'mL'
			and ( PositionOf('/mL', strength.unit)= Length(strength.unit)- 3 ) then Quantity { value: dosesPerDay * doseQuantity.value * strength.value, unit: Substring(strength.unit, 0, PositionOf('/', strength.unit))}
	
	
	/* if doseQuantity is not in actual units (e.g., 1 tab, 1 spray -- when it's a combo med with a unit of tablet, or it's mg/actuat) -->  daily dose = numTimesPerDay * dose value * strength value */ 
		else Quantity { value: dosesPerDay * doseQuantity.value * strength.value, unit: Substring(strength.unit, 0, PositionOf('/', strength.unit))} 
	end

define function "ToUnifiedCodeForUnitsOfMeasure"(unit String):
	/* This function converts the dose units to the correct Unified Code for Units Of Measure (UCUM) unit */
	case unit
		when 'MG' then 'mg'
		when 'MG/ACTUAT' then 'mg/{actuat}'
		when 'MG/HR' then 'mg/h'
		when 'MG/ML' then 'mg/mL' 
		else 'unknown{' + unit + '}' 
	end

define function "EnsureMicrogramQuantity"(strength Quantity):
	/* This function checks the Drug strength, and if it is less than 0.1 and the strength unit (mg) is zero, then the Quantity is set to the strength value x 1000 and unit of mcg */
	if strength.value < 0.1
		and ( PositionOf('mg', strength.unit)= 0 ) then Quantity { value: strength.value * 1000, unit: 'mcg' + Substring(strength.unit, 2)} 
		else strength
