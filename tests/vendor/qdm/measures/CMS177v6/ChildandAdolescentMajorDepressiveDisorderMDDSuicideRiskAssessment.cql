library ChildandAdolescentMajorDepressiveDisorderMDDSuicideRiskAssessment version '6.0.002'

using QDM version '5.3'

valueset "ONC Administrative Sex": 'urn:oid:2.16.840.1.113762.1.4.1'
valueset "Race": 'urn:oid:2.16.840.1.114222.4.11.836'
valueset "Ethnicity": 'urn:oid:2.16.840.1.114222.4.11.837'
valueset "Payer": 'urn:oid:2.16.840.1.114222.4.11.3591'
valueset "Face-to-Face Interaction": 'urn:oid:2.16.840.1.113883.3.464.1003.101.12.1048'
valueset "Group Psychotherapy": 'urn:oid:2.16.840.1.113883.3.526.3.1187'
valueset "Major Depressive Disorder-Active": 'urn:oid:2.16.840.1.113883.3.526.3.1491'
valueset "Office Visit": 'urn:oid:2.16.840.1.113883.3.464.1003.101.12.1001'
valueset "Outpatient Consultation": 'urn:oid:2.16.840.1.113883.3.464.1003.101.12.1008'
valueset "Psych Visit - Diagnostic Evaluation": 'urn:oid:2.16.840.1.113883.3.526.3.1492'
valueset "Psych Visit - Family Psychotherapy": 'urn:oid:2.16.840.1.113883.3.526.3.1018'
valueset "Psych Visit - Psychotherapy": 'urn:oid:2.16.840.1.113883.3.526.3.1496'
valueset "Psychoanalysis": 'urn:oid:2.16.840.1.113883.3.526.3.1141'
valueset "Suicide Risk Assessment": 'urn:oid:2.16.840.1.113883.3.526.3.1484'

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

define "Numerator":
	"Encounter with Major Depressive Disorder Diagnosis" Enc
		with ["Intervention, Performed": "Suicide Risk Assessment"] Intervention
			such that Intervention.relevantPeriod during Enc.relevantPeriod

define "Encounter with Major Depressive Disorder Diagnosis":
	( ["Encounter, Performed": "Office Visit"]
		union ["Encounter, Performed": "Outpatient Consultation"]
		union ["Encounter, Performed": "Face-to-Face Interaction"]
		union ["Encounter, Performed": "Psych Visit - Diagnostic Evaluation"]
		union ["Encounter, Performed": "Psych Visit - Family Psychotherapy"]
		union ["Encounter, Performed": "Psych Visit - Psychotherapy"]
		union ["Encounter, Performed": "Psychoanalysis"]
		union ["Encounter, Performed": "Group Psychotherapy"] ) Enc
		where exists ( Enc.diagnoses Diagnosis
				where Diagnosis in "Major Depressive Disorder-Active"
		)
			and Enc.relevantPeriod during "Measurement Period"

define "Initial Population":
	"Encounter with Major Depressive Disorder Diagnosis" Enc
		where AgeInYearsAt(start of "Measurement Period")>= 6
			and AgeInYearsAt(start of "Measurement Period")< 17
