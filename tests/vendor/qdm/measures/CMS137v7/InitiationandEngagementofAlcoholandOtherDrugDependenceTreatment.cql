library InitiationandEngagementofAlcoholandOtherDrugDependenceTreatment version '7.2.000'

using QDM version '5.3'

include Hospice version '1.0.000' called Hospice
include MATGlobalCommonFunctions version '2.0.000' called Global

valueset "ONC Administrative Sex": 'urn:oid:2.16.840.1.113762.1.4.1'
valueset "Race": 'urn:oid:2.16.840.1.114222.4.11.836'
valueset "Ethnicity": 'urn:oid:2.16.840.1.114222.4.11.837'
valueset "Payer": 'urn:oid:2.16.840.1.114222.4.11.3591'
valueset "Alcohol and Drug Dependence": 'urn:oid:2.16.840.1.113883.3.464.1003.106.12.1001'
valueset "Alcohol and Drug Dependence Treatment": 'urn:oid:2.16.840.1.113883.3.464.1003.106.12.1005'
valueset "Detoxification Visit": 'urn:oid:2.16.840.1.113883.3.464.1003.101.12.1059'
valueset "Discharge Services - Hospital Inpatient": 'urn:oid:2.16.840.1.113883.3.464.1003.101.12.1007'
valueset "Discharge Services - Hospital Inpatient Same Day Discharge": 'urn:oid:2.16.840.1.113883.3.464.1003.101.12.1006'
valueset "Emergency Department Visit": 'urn:oid:2.16.840.1.113883.3.464.1003.101.12.1010'
valueset "Hospital Inpatient Visit - Initial": 'urn:oid:2.16.840.1.113883.3.464.1003.101.12.1004'
valueset "Hospital Observation Care - Initial": 'urn:oid:2.16.840.1.113883.3.464.1003.101.12.1002'
valueset "Office Visit": 'urn:oid:2.16.840.1.113883.3.464.1003.101.12.1001'
valueset "Psych Visit - Psychotherapy": 'urn:oid:2.16.840.1.113883.3.526.3.1496'

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

define "Denominator":
	"Initial Population"

define "Numerator 2":
	"Has Continuation Of Treatments"

define "Stratification 2":
	exists ( ["Patient Characteristic Birthdate"] BirthDate
			where Global."CalendarAgeInYearsAt"(BirthDate.birthDatetime, start of "Measurement Period")>= 18
	)

define "Qualifying Encounters":
	( ["Encounter, Performed": "Office Visit"]
		union ["Encounter, Performed": "Emergency Department Visit"]
		union ["Encounter, Performed": "Detoxification Visit"]
		union ["Encounter, Performed": "Hospital Observation Care - Initial"]
		union ["Encounter, Performed": "Hospital Inpatient Visit - Initial"]
		union ["Encounter, Performed": "Discharge Services - Hospital Inpatient Same Day Discharge"]
		union ["Encounter, Performed": "Discharge Services - Hospital Inpatient"] ) ValidEncounters
		where ValidEncounters.relevantPeriod during "Measurement Period"

define "First Dependence Diagnosis":
	First(["Diagnosis": "Alcohol and Drug Dependence"] InitialDependenceDiagnosis
			with("Qualifying Encounters")ValidEncounter
				such that InitialDependenceDiagnosis.prevalencePeriod starts during ValidEncounter.relevantPeriod
					and InitialDependenceDiagnosis.prevalencePeriod starts 319 days or less after start of "Measurement Period"
			sort by start of prevalencePeriod
	)

define "Treatment Initiation":
	( ( ["Encounter, Performed": "Alcohol and Drug Dependence Treatment"]
			union ["Encounter, Performed": "Psych Visit - Psychotherapy"] ) InitiatedTreatment
			with ( "First Dependence Diagnosis" ) FirstDiagnosed
				such that InitiatedTreatment.relevantPeriod starts 14 days or less after day of start of FirstDiagnosed.prevalencePeriod
	)

define "Stratification 1":
	exists ( ["Patient Characteristic Birthdate"] BirthDate
			where Global."CalendarAgeInYearsAt"(BirthDate.birthDatetime, start of "Measurement Period")>= 13
				and Global."CalendarAgeInYearsAt"(BirthDate.birthDatetime, start of "Measurement Period")< 18
	)

define "Denominator Exclusions":
	Hospice."Has Hospice"
		or exists "Has Prior Dependence"

define "Numerator":
	exists "Treatment Initiation"

define "Has Prior Dependence":
	["Diagnosis": "Alcohol and Drug Dependence"] PriorDependenceDiagnosis
		with ( "First Dependence Diagnosis" ) FirstDiagnosed
			such that PriorDependenceDiagnosis.prevalencePeriod starts 60 days or less before start of FirstDiagnosed.prevalencePeriod

define "Has Continuation Of Treatments":
	Count((["Encounter, Performed": "Alcohol and Drug Dependence Treatment"]
			union ["Encounter, Performed": "Psych Visit - Psychotherapy"])ContinuedTreatment
			with("Treatment Initiation")InitiatedTreatment
				such that ContinuedTreatment.relevantPeriod starts 30 days or less after start of InitiatedTreatment.relevantPeriod
	)>= 2

define "Initial Population":
	exists ["Patient Characteristic Birthdate"] BirthDate
		where Global."CalendarAgeInYearsAt"(BirthDate.birthDatetime, start of "Measurement Period")>= 13
			and "First Dependence Diagnosis" is not null
