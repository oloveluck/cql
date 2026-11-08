library PD0329 version '0.0.004'

using QDM version '5.3'

valueset "ONC Administrative Sex": 'urn:oid:2.16.840.1.113762.1.4.1'
valueset "Race": 'urn:oid:2.16.840.1.114222.4.11.836'
valueset "Ethnicity": 'urn:oid:2.16.840.1.114222.4.11.837'
valueset "Payer": 'urn:oid:2.16.840.1.114222.4.11.3591'
valueset "Medicare Fee For Service": 'urn:oid:2.16.840.1.113883.3.1240.15.2.4008'
valueset "Inpatient Encounter": 'urn:oid:2.16.840.1.113883.3.464.1003.101.12.1060'

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

define "Interesting Encounter":
	["Encounter, Performed": "Inpatient Encounter"] Enc
		where start of Enc.relevantPeriod during "Measurement Period"

define "Medicare Fee":
	"Interesting Encounter" Enc
		with ["Patient Characteristic Payer": "Medicare Fee For Service"] Fee
			such that Fee.relevantPeriod overlaps Enc.relevantPeriod

define "Denominator":
	"Initial Population"

define "Numerator":
	exists "Medicare Fee"

define "Initial Population":
	Patient.birthDatetime in "IntervalWithTZOffsets"

define "IntervalWithTZOffsets":
	Interval[DateTime(year from start of "Measurement Period", 8, 1), DateTime(year from start of "Measurement Period", 12, 31)]
