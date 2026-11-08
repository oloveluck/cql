library DepressionUtilizationofthePHQ9Tool version '6.1.001'

using QDM version '5.3'

codesystem "SNOMEDCT:2016-03": 'urn:oid:2.16.840.1.113883.6.96' version 'urn:hl7:version:2016-03'

valueset "ONC Administrative Sex": 'urn:oid:2.16.840.1.113762.1.4.1'
valueset "Race": 'urn:oid:2.16.840.1.114222.4.11.836'
valueset "Ethnicity": 'urn:oid:2.16.840.1.114222.4.11.837'
valueset "Payer": 'urn:oid:2.16.840.1.114222.4.11.3591'
valueset "Bipolar Disorder": 'urn:oid:2.16.840.1.113883.3.67.1.101.1.128'
valueset "Care Services in Long-Term Residential Facility": 'urn:oid:2.16.840.1.113883.3.464.1003.101.12.1014'
valueset "Dysthymia": 'urn:oid:2.16.840.1.113883.3.67.1.101.1.254'
valueset "Face to Face Interaction - No ED": 'urn:oid:2.16.840.1.113762.1.4.1080.1'
valueset "Major Depression Including Remission": 'urn:oid:2.16.840.113883.3.67.1.101.3.2444'
valueset "Office Visit": 'urn:oid:2.16.840.1.113883.3.464.1003.101.12.1001'
valueset "Palliative Care": 'urn:oid:2.16.840.1.113883.3.600.1.1579'
valueset "Personality Disorder": 'urn:oid:2.16.840.1.113883.3.67.1.101.1.246'
valueset "PHQ-9 Tool": 'urn:oid:2.16.840.1.113883.3.67.1.101.11.723'
valueset "Psych Visit": 'urn:oid:2.16.840.113883.3.67.1.101.3.2445'
valueset "Palliative care encounter": 'urn:oid:2.16.840.1.113883.3.600.1.1575'

code "Dead": '419099009' from "SNOMEDCT:2016-03" display 'Dead'

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

define "Initial Population 1":
	AgeInYearsAt(start of "Measurement Period")>= 18
		and ( exists ( "Depression Office Visit Encounter 1" )
				or exists ( "Depression Behavioral Health Encounter 1" )
				or exists ( "Depression Face to Face Encounter 1" )
		)

define "Initial Population 2":
	AgeInYearsAt(start of "Measurement Period")>= 18
		and ( exists ( "Depression Office Visit Encounter 2" )
				or exists ( "Depression Behavioral Health Encounter 2" )
				or exists ( "Depression Face to Face Encounter 2" )
		)

define "Initial Population 3":
	AgeInYearsAt(start of "Measurement Period")>= 18
		and ( exists ( "Depression Office Visit Encounter 3" )
				or exists ( "Depression Behavioral Health Encounter 3" )
				or exists ( "Depression Face to Face Encounter 3" )
		)

define "Palliative Care Order":
	["Intervention, Order": "Palliative Care"] OrderedPalliativeCare
		where OrderedPalliativeCare.authorDatetime 1 year or less before end of "Measurement Period"

define "Long Term Care":
	["Encounter, Performed": "Care Services in Long-Term Residential Facility"] EncounterLongTermCare
		where EncounterLongTermCare.relevantPeriod starts 1 year or less before end of "Measurement Period"

define "Expired":
	["Patient Characteristic Expired": "Dead"] Deceased
		where Deceased.expiredDatetime before end of "Measurement Period"

define "Denominator 1":
	"Initial Population 1"

define "Denominator 2":
	"Initial Population 2"

define "Denominator 3":
	"Initial Population 3"

define "Denominator Exclusion 1":
	"Denominator Exclusions"

define "Denominator Exclusion 2":
	"Denominator Exclusions"

define "Denominator Exclusion 3":
	"Denominator Exclusions"

define "Encounter Palliative Care":
	["Encounter, Performed": "Palliative care encounter"] PalliativeCareEncounter
		where PalliativeCareEncounter.relevantPeriod starts 1 year or less before end of "Measurement Period"

define "Depression Behavioral Health Encounter 2":
	["Encounter, Performed": "Psych Visit"] BehaviorHealthEncounter2
		where ( BehaviorHealthEncounter2.principalDiagnosis in "Major Depression Including Remission"
				or BehaviorHealthEncounter2.principalDiagnosis in "Dysthymia"
		)
			and BehaviorHealthEncounter2.relevantPeriod ends 8 months or less after start of "Measurement Period"
			and difference in months between end of BehaviorHealthEncounter2.relevantPeriod and ( start of "Measurement Period" + 8 months ) <= 4

define "Numerator 2":
	exists ( ["Assessment, Performed": "PHQ-9 Tool"] DepressionAssessment2
			where DepressionAssessment2.result is not null
				and DepressionAssessment2.authorDatetime 8 months or less after start of "Measurement Period"
				and difference in months between DepressionAssessment2.authorDatetime and ( start of "Measurement Period" + 8 months ) <= 4
	)

define "Depression Behavioral Health Encounter 3":
	["Encounter, Performed": "Psych Visit"] BehaviorHealthEncounter3
		where ( BehaviorHealthEncounter3.principalDiagnosis in "Major Depression Including Remission"
				or BehaviorHealthEncounter3.principalDiagnosis in "Dysthymia"
		)
			and BehaviorHealthEncounter3.relevantPeriod ends during "Measurement Period"
			and difference in months between start of "Measurement Period" and end of BehaviorHealthEncounter3.relevantPeriod <= 3

define "Depression Behavioral Health Encounter 1":
	["Encounter, Performed": "Psych Visit"] BehaviorHealthEncounter1
		where ( BehaviorHealthEncounter1.principalDiagnosis in "Major Depression Including Remission"
				or BehaviorHealthEncounter1.principalDiagnosis in "Dysthymia"
		)
			and BehaviorHealthEncounter1.relevantPeriod ends during "Measurement Period"
			and difference in months between end of BehaviorHealthEncounter1.relevantPeriod and end of "Measurement Period" <= 3

define "Depression Diagnoses":
	( ["Diagnosis": "Major Depression Including Remission"]
		union ["Diagnosis": "Dysthymia"] ) Diagnoses
		where Diagnoses.prevalencePeriod is not null

define "Disorder Diagnoses":
	( ["Diagnosis": "Bipolar Disorder"]
		union ["Diagnosis": "Personality Disorder"] ) DisorderDiagnoses
		where DisorderDiagnoses.prevalencePeriod starts before end of "Measurement Period"

define "Depression Face to Face Encounter 1":
	["Encounter, Performed": "Face to Face Interaction - No ED"] FacetoFaceEncounter1
		with "Depression Diagnoses" Diagnoses
			such that FacetoFaceEncounter1.relevantPeriod overlaps Diagnoses.prevalencePeriod
				and FacetoFaceEncounter1.relevantPeriod ends during "Measurement Period"
				and difference in months between end of FacetoFaceEncounter1.relevantPeriod and end of "Measurement Period" <= 3

define "Depression Face to Face Encounter 2":
	["Encounter, Performed": "Face to Face Interaction - No ED"] FacetoFaceEncounter2
		with "Depression Diagnoses" Diagnoses
			such that FacetoFaceEncounter2.relevantPeriod overlaps Diagnoses.prevalencePeriod
				and FacetoFaceEncounter2.relevantPeriod ends 8 months or less after start of "Measurement Period"
				and difference in months between end of FacetoFaceEncounter2.relevantPeriod and ( start of "Measurement Period" + 8 months ) <= 4

define "Depression Face to Face Encounter 3":
	["Encounter, Performed": "Face to Face Interaction - No ED"] FacetoFaceEncounter3
		with "Depression Diagnoses" Diagnoses
			such that FacetoFaceEncounter3.relevantPeriod overlaps Diagnoses.prevalencePeriod
				and FacetoFaceEncounter3.relevantPeriod ends during "Measurement Period"
				and difference in months between start of "Measurement Period" and end of FacetoFaceEncounter3.relevantPeriod <= 3

define "Depression Office Visit Encounter 1":
	["Encounter, Performed": "Office Visit"] OfficeVisitEncounter1
		with "Depression Diagnoses" Diagnoses
			such that OfficeVisitEncounter1.relevantPeriod overlaps Diagnoses.prevalencePeriod
				and OfficeVisitEncounter1.relevantPeriod ends during "Measurement Period"
				and difference in months between end of OfficeVisitEncounter1.relevantPeriod and end of "Measurement Period" <= 3

define "Depression Office Visit Encounter 2":
	["Encounter, Performed": "Office Visit"] OfficeVisitEncounter2
		with "Depression Diagnoses" Diagnoses
			such that OfficeVisitEncounter2.relevantPeriod overlaps Diagnoses.prevalencePeriod
				and OfficeVisitEncounter2.relevantPeriod ends 8 months or less after start of "Measurement Period"
				and difference in months between end of OfficeVisitEncounter2.relevantPeriod and ( start of "Measurement Period" + 8 months ) <= 4

define "Depression Office Visit Encounter 3":
	["Encounter, Performed": "Office Visit"] OfficeVisitEncounter3
		with "Depression Diagnoses" Diagnoses
			such that OfficeVisitEncounter3.relevantPeriod overlaps Diagnoses.prevalencePeriod
				and OfficeVisitEncounter3.relevantPeriod ends during "Measurement Period"
				and difference in months between start of "Measurement Period" and end of OfficeVisitEncounter3.relevantPeriod <= 3

define "Numerator 1":
	exists ( ["Assessment, Performed": "PHQ-9 Tool"] DepressionAssessment1
			where DepressionAssessment1.result is not null
				and DepressionAssessment1.authorDatetime during "Measurement Period"
				and difference in months between DepressionAssessment1.authorDatetime and end of "Measurement Period" <= 3
	)

define "Denominator Exclusions":
	exists ( "Palliative Care Order" )
		or exists ( "Encounter Palliative Care" )
		or exists ( "Long Term Care" )
		or exists ( "Expired" )
		or exists ( "Disorder Diagnoses" )

define "Numerator 3":
	exists ( ["Assessment, Performed": "PHQ-9 Tool"] DepressionAssessment3
			where DepressionAssessment3.result is not null
				and DepressionAssessment3.authorDatetime during "Measurement Period"
				and difference in months between start of "Measurement Period" and DepressionAssessment3.authorDatetime <= 3
	)
