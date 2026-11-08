library CoreClinicalDataElementsfortheHybridHospitalWideReadmissionHWRMeasurewithClaimsandElectronicHealthRecordData version '0.1.016'

using QDM version '5.3'

include MATGlobalCommonFunctions version '2.0.000' called Global

codesystem "LOINC:2.58": 'urn:oid:2.16.840.1.113883.6.1' version 'urn:hl7:version:2.58'
codesystem "LOINC:2.63": 'urn:oid:2.16.840.1.113883.6.1' version 'urn:hl7:version:2.63'

valueset "Payer": 'urn:oid:2.16.840.1.114222.4.11.3591'
valueset "Bicarbonate Lab Test": 'urn:oid:2.16.840.1.113762.1.4.1045.138'
valueset "Body Temperature LOINC": 'urn:oid:2.16.840.1.113762.1.4.1045.152'
valueset "Body Weight": 'urn:oid:2.16.840.1.113762.1.4.1045.159'
valueset "Creatinine Lab Test": 'urn:oid:2.16.840.1.113883.3.666.5.2363'
valueset "Glucose Lab Test": 'urn:oid:2.16.840.1.113762.1.4.1045.134'
valueset "Hematocrit lab test": 'urn:oid:2.16.840.1.113762.1.4.1045.114'
valueset "Medicare payer": 'urn:oid:2.16.840.1.113762.1.4.1104.10'
valueset "Potassium Lab Test": 'urn:oid:2.16.840.1.113762.1.4.1045.117'
valueset "Sodium Lab Test V": 'urn:oid:2.16.840.1.113762.1.4.1045.119'
valueset "White Blood Cells Count Lab Test": 'urn:oid:2.16.840.1.113762.1.4.1045.129'
valueset "Acute care hospital Inpatient Encounter": 'urn:oid:2.16.840.1.113883.3.666.5.2289'

code "Heart rate": '8867-4' from "LOINC:2.58" display 'Heart rate'
code "Oxygen saturation in Arterial blood by Pulse oximetry": '59408-5' from "LOINC:2.63" display 'Oxygen saturation in Arterial blood by Pulse oximetry'
code "Respiratory rate": '9279-1' from "LOINC:2.63" display 'Respiratory rate'
code "Systolic blood pressure": '8480-6' from "LOINC:2.58" display 'Systolic blood pressure'

parameter "Measurement Period" Interval<DateTime>

context Patient

define "Denominator":
	"Initial Population"

define "Initial Population":
	"IP Encounters"

define "Numerator":
	"Initial Population"

define "Results":
	{
	    // First physical exams
		firstHR: "FirstPhysicalExam"(["Physical Exam, Performed": "Heart rate"]),
		firstSBP: "FirstPhysicalExam"(["Physical Exam, Performed": "Systolic blood pressure"]),
		firstRR: "FirstPhysicalExam"(["Physical Exam, Performed": "Respiratory rate"]),
		firstTemp: "FirstPhysicalExam"(["Physical Exam, Performed": "Body Temperature LOINC"]),
		firstO2S: "FirstPhysicalExam"(["Physical Exam, Performed": "Oxygen saturation in Arterial blood by Pulse oximetry"]),
	    // Weight uses lab test timing
		firstWeight: "FirstPhysicalExamWithLabTiming"(["Physical Exam, Performed": "Body Weight"]),
	
	    // First lab tests
		firstHemat: "FirstLabTest"(["Laboratory Test, Performed": "Hematocrit lab test"]),
		firstWBC: "FirstLabTest"(["Laboratory Test, Performed": "White Blood Cells Count Lab Test"]),
		firstPotass: "FirstLabTest"(["Laboratory Test, Performed": "Potassium Lab Test"]),
		firstSodium: "FirstLabTest"(["Laboratory Test, Performed": "Sodium Lab Test V"]),
		firstBicarb: "FirstLabTest"(["Laboratory Test, Performed": "Bicarbonate Lab Test"]),
		firstCreat: "FirstLabTest"(["Laboratory Test, Performed": "Creatinine Lab Test"]),
		firstGlucose: "FirstLabTest"(["Laboratory Test, Performed": "Glucose Lab Test"])
	}

define "IP Encounters":
	from
		["Encounter, Performed": "Acute care hospital Inpatient Encounter"] Encounter,
		["Patient Characteristic": "Payer"] Payer,
		["Patient Characteristic Birthdate"] Birthdate
		where Payer.code in "Medicare payer"
			and Global."Hospitalization Length of Stay"(Encounter)< 365
			and Encounter.relevantPeriod ends during "Measurement Period"
			and Global."CalendarAgeInYearsAt"(Birthdate.birthDatetime, start of Encounter.relevantPeriod)>= 65
		return Encounter

define function "FirstPhysicalExamWithLabTiming"(exam List<QDM.PositivePhysicalExamPerformed>):
	"IP Encounters" Encounter
		return First(exam e
				where e.relevantPeriod starts during Interval[start of Encounter.relevantPeriod - 1440 minutes, start of Encounter.relevantPeriod + 1440 minutes]
				sort by start of relevantPeriod
		).result

define function "FirstLabTest"(lab List<QDM.PositiveLaboratoryTestPerformed>):
	"IP Encounters" Encounter
		return First(lab l
				where l.resultDatetime during Interval[start of Encounter.relevantPeriod - 1440 minutes, start of Encounter.relevantPeriod + 1440 minutes]
				sort by start of relevantPeriod
		).result

define function "FirstPhysicalExam"(exam List<QDM.PositivePhysicalExamPerformed>):
	"IP Encounters" Encounter
		return First(exam e
				where e.relevantPeriod starts during Interval[start of Encounter.relevantPeriod - 1440 minutes, start of Encounter.relevantPeriod + 120 minutes]
				sort by start of relevantPeriod
		).result
