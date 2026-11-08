library PotentialOpioidOveruse version '0.1.068'

using QDM version '5.3'

include MATGlobalCommonFunctions version '2.0.000' called Global
include OpioidMedicationLogic version '1.17.000' called OpioidLogicLibrary
include DayMonthTimings version '0.6.000' called Timing

codesystem "SNOMEDCT:2018-03": 'urn:oid:2.16.840.1.113883.6.96' version 'urn:hl7:version:2018-03'

valueset "ONC Administrative Sex": 'urn:oid:2.16.840.1.113762.1.4.1'
valueset "Race": 'urn:oid:2.16.840.1.114222.4.11.836'
valueset "Ethnicity": 'urn:oid:2.16.840.1.114222.4.11.837'
valueset "Payer": 'urn:oid:2.16.840.1.114222.4.11.3591'
valueset "Office Visit": 'urn:oid:2.16.840.1.113883.3.464.1003.101.12.1001'
valueset "Preventive Care Services - Established Office Visit, 18 and Up": 'urn:oid:2.16.840.1.113883.3.464.1003.101.12.1025'
valueset "Preventive Care Services-Initial Office Visit, 18 and Up": 'urn:oid:2.16.840.1.113883.3.464.1003.101.12.1023'
valueset "Preventive Care Services - Other": 'urn:oid:2.16.840.1.113883.3.464.1003.101.12.1030'
valueset "Face to Face Interaction - No ED": 'urn:oid:2.16.840.1.113762.1.4.1080.2'
valueset "Palliative Care": 'urn:oid:2.16.840.1.113883.3.600.1.1579'
valueset "Hospice Care SNO": 'urn:oid:2.16.840.1.113883.3.3157.1004.20'
valueset "All Cancer": 'urn:oid:2.16.840.1.113883.3.464.1003.108.12.1011'
valueset "Sickle Cell Disease SNOMEDCT": 'urn:oid:2.16.840.1.113762.1.4.1029.33'
valueset "Outpatient Consultation": 'urn:oid:2.16.840.1.113883.3.464.1003.101.12.1008'
valueset "Opioid Medications": 'urn:oid:2.16.840.1.113883.3.3157.1004.26'

code "Every eight hours (qualifier value)": '307469008' from "SNOMEDCT:2018-03" display 'Every eight hours (qualifier value)'
code "Every forty eight hours (qualifier value)": '396131002' from "SNOMEDCT:2018-03" display 'Every forty eight hours (qualifier value)'
code "Every four hours as required (qualifier value)": '1791000175106' from "SNOMEDCT:2018-03" display 'Every four hours as required (qualifier value)'
code "Every seventy two hours (qualifier value)": '396143001' from "SNOMEDCT:2018-03" display 'Every seventy two hours (qualifier value)'
code "Every six hours (qualifier value)": '307468000' from "SNOMEDCT:2018-03" display 'Every six hours (qualifier value)'
code "Every thirty six hours (qualifier value)": '396126004' from "SNOMEDCT:2018-03" display 'Every thirty six hours (qualifier value)'
code "Every twelve hours (qualifier value)": '307470009' from "SNOMEDCT:2018-03" display 'Every twelve hours (qualifier value)'
code "Every twenty four hours (qualifier value)": '396125000' from "SNOMEDCT:2018-03" display 'Every twenty four hours (qualifier value)'
code "Every two to four hours (qualifier value)": '225752000' from "SNOMEDCT:2018-03" display 'Every two to four hours (qualifier value)'
code "Four times daily (qualifier value)": '307439001' from "SNOMEDCT:2018-03" display 'Four times daily (qualifier value)'
code "Once daily (qualifier value)": '229797004' from "SNOMEDCT:2018-03" display 'Once daily (qualifier value)'
code "Three times daily (qualifier value)": '229798009' from "SNOMEDCT:2018-03" display 'Three times daily (qualifier value)'
code "Twice a day (qualifier value)": '229799001' from "SNOMEDCT:2018-03" display 'Twice a day (qualifier value)'

parameter "Measurement Period" Interval<DateTime>

context Patient

define "SDE Ethnicity":
	["Patient Characteristic Ethnicity": "Ethnicity"]

define "SDE Payer":
	["Patient Characteristic Payer": "Payer"]

define "SDE Race":
	["Patient Characteristic Race": "Race"]

define "SDE Sex":
	["Patient Characteristic Sex": "ONC Administrative Sex"]

define "Numerator":
	"MME Greater Than 90"

define "MME Sum Days":
	distinct ( "MME Days" MMEDays
		return MMEDays.Day ) Days
		let totalMME: Sum("MME Days" Total
				where Total.Day same day as Days
				return all Total.MME
		)
		return {
			Day: Days,
			totalMME: totalMME
		}

define "MME Greater Than 90":
	/*Using the MME interval passed from the "MMEIntervals" definition, count the number of instances where the MME interval is greater than 90 days*/
	Count("MME Sum Days" MMESum
			where MMESum.totalMME.value >= 90
	)>= 90

define "Denominator":
	"Initial Population"

define "Encounters during Measurement Period":
	/*A Face to Face Interaction, Office Visit, Preventative Care Services  Established Office Visit for 18 years and over, Preventative Care Services - Initial Office Visit for 18 years and over, Outpatient Consultation or Preventative Care Services - Other encounter performed during the Measurement Period*/
	( ["Encounter, Performed": "Face to Face Interaction - No ED"]
		union ["Encounter, Performed": "Office Visit"]
		union ["Encounter, Performed": "Preventive Care Services - Established Office Visit, 18 and Up"]
		union ["Encounter, Performed": "Preventive Care Services-Initial Office Visit, 18 and Up"]
		union ["Encounter, Performed": "Outpatient Consultation"]
		union ["Encounter, Performed": "Preventive Care Services - Other"] ) Encounter
		where Encounter.relevantPeriod during "Measurement Period"

define "OpioidMedications":
	/*An opioid medication order during or overlapping the start of the measurement period*/
	["Medication, Order": "Opioid Medications"] OpioidMedicationOrder
		where OpioidMedicationOrder.relevantPeriod starts during "Measurement Period"
			or OpioidMedicationOrder.relevantPeriod overlaps "Measurement Period"

define "MME Days":
	/*Using the rxNorm Code passed from the PrescriptionDays definition, and retun the number of days a medication was ordered*/
	flatten ( "Prescription Days" PrescriptionDays
			return all ( PrescriptionDays.days_period Days
					return {
						rxNormCode: PrescriptionDays.rxNormCode,
						Day: Days,
						MME: PrescriptionDays.MME
					}
			)
	)

define "Denominator Exclusion":
	/*Patients receiving palliative or hospice treatment, or patients with cancer, or patients with sickle cell disease during the measurement period are excluded.*/
	exists ( "Encounters during Measurement Period" Encounter
			with ["Intervention, Order": "Palliative Care"] PalliativeCareOrder
				such that PalliativeCareOrder.authorDatetime during Encounter.relevantPeriod
	)
		or exists ( "Encounters during Measurement Period" Encounter
				with ["Intervention, Performed": "Palliative Care"] PalliativeCareIntervention
					such that PalliativeCareIntervention.relevantPeriod overlaps Encounter.relevantPeriod
						or PalliativeCareIntervention.relevantPeriod starts during Encounter.relevantPeriod
		)
		or exists ( "Encounters during Measurement Period" Encounter
				with ( ["Intervention, Order": "Hospice Care SNO"]
					union ["Intervention, Performed": "Hospice Care SNO"]
					union ["Intervention, Recommended": "Hospice Care SNO"] ) Hospice
					such that Hospice.authorDatetime during "Measurement Period"
		)
		or exists ( "Encounters during Measurement Period" Encounter
				with ["Diagnosis": "All Cancer"] CancerDiagnosis
					such that CancerDiagnosis.prevalencePeriod starts 1 year or less before start of "Measurement Period"
						or CancerDiagnosis.prevalencePeriod starts during "Measurement Period"
		)
		or exists ( "Encounters during Measurement Period" Encounter
				with ["Diagnosis": "Sickle Cell Disease SNOMEDCT"] DiagnosisSickle
					such that DiagnosisSickle.prevalencePeriod starts before start of "Measurement Period"
						or DiagnosisSickle.prevalencePeriod starts during "Measurement Period"
		)

define "Prescriptions with MME":
	/*Using the MME calculated from the Opioid Medication Library and return the opioid code, opioid author datetime, opioid effective period, and the Conversion Factor*/
	"OpioidMedications" Opioid
		let dosesPerDay: "DosesPerDay"(Opioid.frequency),
		MME: singleton from ( OpioidLogicLibrary."CalculateMorphineMilligramEquivalents"({ {
					rxNormCode: Opioid.code,
					doseQuantity: Opioid.dosage,
					dosesPerDay: dosesPerDay
				} }
			)
		),
		medPeriod: Opioid.relevantPeriod
			intersect "Measurement Period"
		return {
			rxNormCode: Opioid.code,
			authortime: Opioid.authorDatetime,
			effectivePeriod: medPeriod,
			conversionFactor: MME.conversionFactor,
			MME: MME.MME
		}

define "Initial Population":
	/*Patients 18 years of age and older prescribed a 90-day or greater supply with no more than a 7 day gap between prescription of opioids and who have a visit during the measurement period*/
	Global.CalendarAgeInYearsAt(Patient.birthDatetime, start of "Measurement Period")>= 18
		and exists ( "Encounters during Measurement Period" )
		and exists ( "Periods With and Without 7 Day Gap With Cumulative Med Duration 90 days or Greater" )

define "Prescription Days":
	/*Using the values passed from the Prescription with MMEs definition, this definition creates a single list containing the days within the period*/
	"Prescriptions with MME" ScriptMME
		let period: Timing."DaysInPeriod"(ScriptMME.effectivePeriod)
		return {
			rxNormCode: ScriptMME.rxNormCode,
			days_period: period, /* DaysInPeriod returns a list of Dates in the interval*/
			MME: ScriptMME.MME
		}

define "Opioid Med Periods In Measurement Period":
	/* Creates a list of tuples that have the medication and the period of the medication in the measurement period. */
	"OpioidMedications" Opioid
		return {
			periodInMP: Opioid.relevantPeriod intersect "Measurement Period",
			med: Opioid
		}

define "Opioid Periods":
	/* All intervals of opioid medications, clamped to the measurement period, collapsed */
	collapse("Opioid Med Periods In Measurement Period" Opioid return Opioid.periodInMP)

define "Overall Active Opioid Period":
	/* The overall period of opioid medications. start of first to end of last */
	Interval[
		start of First("Opioid Med Periods In Measurement Period" OpioidPeriod sort by start of periodInMP).periodInMP,
		end of Last("Opioid Med Periods In Measurement Period" OpioidPeriod sort by end of periodInMP).periodInMP
		]

define "Gaps In Active Opioid Period":
	/* The list of all gaps in the overall opioid period. */
	"Opioid Periods" Period
		let NextIndex: IndexOf("Opioid Periods", Period) + 1
		where NextIndex < Length("Opioid Periods")
		return Interval[end of Period, start of "Opioid Periods"[NextIndex]]

define "Gaps Greater Than 7 Days":
	/* The list of gaps in the overall period that are greater than 7 days */
	"Gaps In Active Opioid Period" Gap
		where duration in days of Gap > 7

define "Periods With Allowed Gaps":
	/* The lists of periods with allowed gaps. This uses "Gaps Greater Than 7 Days" to construct the periods. */
	if Length("Gaps Greater Than 7 Days") < 1
		/* If there are no gaps, use the overall period */
		then { "Overall Active Opioid Period" }
		else 
			/* First interval from start of overall period to first gap */
			{ Interval[start of "Overall Active Opioid Period", start of "Gaps Greater Than 7 Days"[0]] } union 
			/* Rest of intervals. Foreach: start at end of current gap, end at end of next gap or end of overall period */
			"Gaps Greater Than 7 Days" Gap
				let NextGapIndex: IndexOf("Gaps Greater Than 7 Days", Gap) + 1
				return if NextGapIndex < Length("Gaps Greater Than 7 Days")
					then Interval[end of Gap, start of "Gaps Greater Than 7 Days"[NextGapIndex]]
					else Interval[end of Gap, end of "Overall Active Opioid Period"]

define "Periods With Allowed Gaps With Meds and Durations":
	/* List of each period with allowed gaps with the list of medications and the Cumulative Medication Duration for the
		medications in period */
	"Periods With Allowed Gaps" Period
		let Meds: ("Opioid Med Periods In Measurement Period" Opioid where Opioid.periodInMP during Period)
		return {
			period: Period,
			meds: Meds Med return Med.med,
			cmd: "Cumulative Duration"(Meds Med return Med.periodInMP)
		}

define function "Cumulative Duration"(Periods List<Interval<DateTime>>):
	Sum((collapse(Periods)) Period
		return all duration in days of Period
	)

define "Periods With and Without 7 Day Gap With Cumulative Med Duration 90 days or Greater":
	/* The list of periods with Cumulative Medication Duration greater than or equal to 90 days. */
	"Periods With Allowed Gaps With Meds and Durations" Period
		where Period.cmd >= 90

define function "Cumulative Medication Duration"(Medication List<"Medication, Order">):
	Sum((collapse(Medication.relevantPeriod))CumulativeDosages
			return all duration in days of CumulativeDosages
	)

define function "DosesPerDay"(frequency Code):
	case
		when frequency ~ "Once daily (qualifier value)" then 1.0
		when frequency ~ "Twice a day (qualifier value)" then 2.0
		when frequency ~ "Three times daily (qualifier value)" then 3.0
		when frequency ~ "Four times daily (qualifier value)" then 4.0
		when frequency ~ "Every four hours as required (qualifier value)" then 6.0
		when frequency ~ "Every two to four hours (qualifier value)" then 12.0
		when frequency ~ "Every twenty four hours (qualifier value)" then 1.0
		when frequency ~ "Every twelve hours (qualifier value)" then 2.0
		when frequency ~ "Every thirty six hours (qualifier value)" then 0.67
		when frequency ~ "Every eight hours (qualifier value)" then 3.0
		when frequency ~ "Every six hours (qualifier value)" then 4.0
		when frequency ~ "Every seventy two hours (qualifier value)" then 0.34
		when frequency ~ "Every forty eight hours (qualifier value)" then 0.5 
		else null 
	end
