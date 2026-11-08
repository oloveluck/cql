library TJC_Overall version '7.0.000'

using QDM version '5.3'

include MATGlobalCommonFunctions version '2.0.000' called global

codesystem "LOINC:2.46": 'urn:oid:2.16.840.1.113883.6.1' version 'urn:hl7:version:2.46'
codesystem "SNOMEDCT:2016-03": 'urn:oid:2.16.840.1.113883.6.96' version 'urn:hl7:version:2016-03'

valueset "Encounter Inpatient": 'urn:oid:2.16.840.1.113883.3.666.5.307'
valueset "Non-Elective Inpatient Encounter": 'urn:oid:2.16.840.1.113883.3.117.1.7.1.424'
valueset "Ischemic Stroke": 'urn:oid:2.16.840.1.113883.3.117.1.7.1.247'
valueset "Hemorrhagic Stroke": 'urn:oid:2.16.840.1.113883.3.117.1.7.1.212'
valueset "Comfort Measures": 'urn:oid:1.3.6.1.4.1.33895.1.3.0.45'
valueset "Emergency Department Visit": 'urn:oid:2.16.840.1.113883.3.117.1.7.1.292'
valueset "Left Against Medical Advice": 'urn:oid:2.16.840.1.113883.3.117.1.7.1.308'
valueset "Discharged to Health Care Facility for Hospice Care": 'urn:oid:2.16.840.1.113883.3.117.1.7.1.207'
valueset "Patient Expired": 'urn:oid:2.16.840.1.113883.3.117.1.7.1.309'
valueset "Discharge To Acute Care Facility": 'urn:oid:2.16.840.1.113883.3.117.1.7.1.87'
valueset "Discharged to Home for Hospice Care": 'urn:oid:2.16.840.1.113883.3.117.1.7.1.209'

code "Birthdate": '21112-8' from "LOINC:2.46" display 'Birth date'
code "Dead": '419099009' from "SNOMEDCT:2016-03" display 'Dead'

parameter "Measurement Period" Interval<DateTime>

context Patient

define "Encounter with Principal Diagnosis and Age":
	"Encounter with Principal Diagnosis of Hemorrhagic or Ischemic Stroke" NonElectiveEncounter
		with ["Patient Characteristic Birthdate"] P
			such that global."CalendarAgeInYearsAt"(P.birthDatetime, start of NonElectiveEncounter.relevantPeriod)>= 18

define "Encounter with Principal Diagnosis of Hemorrhagic or Ischemic Stroke":
	"Non Elective Inpatient Encounter" NonElectiveEncounter
		where NonElectiveEncounter.principalDiagnosis in "Hemorrhagic Stroke"
			or NonElectiveEncounter.principalDiagnosis in "Ischemic Stroke"

define "Encounter with Principal Diagnosis of Ischemic Stroke":
	"Encounter with Principal Diagnosis and Age" NonElectiveEncounter
		where NonElectiveEncounter.principalDiagnosis in "Ischemic Stroke"

define "Inpatient Encounter":
	["Encounter, Performed": "Encounter Inpatient"] Encounter
		where Encounter.lengthOfStay <= 120 days
			and Encounter.relevantPeriod ends during "Measurement Period"

define "Ischemic Stroke Encounters with Discharge Status and Comfort Measure Exclusions":
	distinct(("Encounter with Principal Diagnosis of Ischemic Stroke" Encounter
				where Encounter.dischargeDisposition in "Discharge To Acute Care Facility"
					or Encounter.dischargeDisposition in "Left Against Medical Advice"
					or Encounter.dischargeDisposition in "Patient Expired"
					or Encounter.dischargeDisposition in "Discharged to Home for Hospice Care"
					or Encounter.dischargeDisposition in "Discharged to Health Care Facility for Hospice Care"
		)
	)

define "Non Elective Inpatient Encounter":
	["Encounter, Performed": "Non-Elective Inpatient Encounter"] NonElectiveEncounter
		where NonElectiveEncounter.lengthOfStay <= 120 days
			and NonElectiveEncounter.relevantPeriod ends during "Measurement Period"
