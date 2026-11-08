library DiabetesFootExam version '7.5.000'

using QDM version '5.3'

include Adult_Outpatient_Encounters version '1.1.000' called AdultOutpatientEncounters
include Hospice version '1.0.000' called Hospice
include MATGlobalCommonFunctions version '2.0.000' called Global

codesystem "SNOMEDCT:2017-09": 'urn:oid:2.16.840.1.113883.6.96' version 'urn:hl7:version:2017-09'

valueset "ONC Administrative Sex": 'urn:oid:2.16.840.1.113762.1.4.1'
valueset "Race": 'urn:oid:2.16.840.1.114222.4.11.836'
valueset "Ethnicity": 'urn:oid:2.16.840.1.114222.4.11.837'
valueset "Payer": 'urn:oid:2.16.840.1.114222.4.11.3591'
valueset "Bilateral amputation of leg below or above knee": 'urn:oid:2.16.840.1.113883.3.464.1003.113.12.1056'
valueset "Diabetes": 'urn:oid:2.16.840.1.113883.3.464.1003.103.12.1001'
valueset "Left": 'urn:oid:2.16.840.1.113883.3.464.1003.122.12.1036'
valueset "Left Unilateral Amputation Procedure Above or Below Knee": 'urn:oid:2.16.840.1.113883.3.464.1003.113.12.1072'
valueset "Right": 'urn:oid:2.16.840.1.113883.3.464.1003.122.12.1035'
valueset "Right Unilateral Amputation Procedure Above or Below Knee": 'urn:oid:2.16.840.1.113883.3.464.1003.113.12.1073'
valueset "Unilateral Amputation Procedure Below or Above Knee, Unspecified Laterality": 'urn:oid:2.16.840.1.113883.3.464.1003.103.12.1022'
valueset "Unilateral Amputation Below or Above Knee, Unspecified Laterality": 'urn:oid:2.16.840.1.113883.3.464.1003.113.12.1059'
valueset "Left Unilateral Amputation Above or Below Knee": 'urn:oid:2.16.840.1.113883.3.464.1003.113.12.1058'
valueset "Right Unilateral Amputation Above or Below Knee": 'urn:oid:2.16.840.1.113883.3.464.1003.113.12.1057'

code "Diabetic foot examination (regime/therapy)": '401191002' from "SNOMEDCT:2017-09" display 'Diabetic foot examination (regime/therapy)'
code "Monofilament foot sensation test (procedure)": '134388005' from "SNOMEDCT:2017-09" display 'Monofilament foot sensation test (procedure)'
code "Pedal pulse taking (procedure)": '91161007' from "SNOMEDCT:2017-09" display 'Pedal pulse taking (procedure)'

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
	exists ( "Sensory Foot Exam" )
		and exists ( "Visual Foot Exam" )
		and exists ( "Pedal Pulse Checked" )

define "Pedal Pulse Checked":
	["Assessment, Performed": "Pedal pulse taking (procedure)"] PedalPulseCheck
		where PedalPulseCheck.result is not null
			and PedalPulseCheck.authorDatetime during "Measurement Period"

define "Visual Foot Exam":
	["Assessment, Performed": "Diabetic foot examination (regime/therapy)"] FootExamVisual
		where FootExamVisual.result is not null
			and FootExamVisual.authorDatetime during "Measurement Period"

define "Bilateral Lower Extremity Amputation":
	["Diagnosis": "Bilateral amputation of leg below or above knee"] LowerBilateralAmputation
		where LowerBilateralAmputation.prevalencePeriod starts on or before end of "Measurement Period"

define "Sensory Foot Exam":
	["Assessment, Performed": "Monofilament foot sensation test (procedure)"] MonofilamentTest
		where MonofilamentTest.result is not null
			and MonofilamentTest.authorDatetime during "Measurement Period"

define "Denominator Exclusions":
	Hospice."Has Hospice"
		or exists ( "Bilateral Lower Extremity Amputation" )
		or ( exists ( "Left Lower Extremity Amputation" )
				and exists ( "Right Lower Extremity Amputation" )
		)

define "Initial Population":
	exists ( ["Patient Characteristic Birthdate"] Birthdate
			where Global."CalendarAgeInYearsAt"(Birthdate.birthDatetime, start of "Measurement Period")in Interval[18, 75 )
	)
		and exists ( AdultOutpatientEncounters."Qualifying Encounters" )
		and exists ( ["Diagnosis": "Diabetes"] Diabetes
				where Diabetes.prevalencePeriod overlaps "Measurement Period"
		)

define "Left Lower Extremity Amputation":
	( ( ( ["Diagnosis": "Unilateral Amputation Below or Above Knee, Unspecified Laterality"]
			union ["Procedure, Performed": "Unilateral Amputation Procedure Below or Above Knee, Unspecified Laterality"] ) UnilateralAmputation
			where UnilateralAmputation.anatomicalLocationSite in "Left"
	)
		union ( ["Diagnosis": "Left Unilateral Amputation Above or Below Knee"]
				union ["Procedure, Performed": "Left Unilateral Amputation Procedure Above or Below Knee"]
		) ) LeftLowerAmputation
		where Coalesce(LeftLowerAmputation.prevalencePeriod, LeftLowerAmputation.relevantPeriod)starts on or before end of "Measurement Period"

define "Right Lower Extremity Amputation":
	( ( ( ["Diagnosis": "Unilateral Amputation Below or Above Knee, Unspecified Laterality"]
			union ["Procedure, Performed": "Unilateral Amputation Procedure Below or Above Knee, Unspecified Laterality"] ) UnilateralAmputation
			where UnilateralAmputation.anatomicalLocationSite in "Right"
	)
		union ( ["Diagnosis": "Right Unilateral Amputation Above or Below Knee"]
				union ["Procedure, Performed": "Right Unilateral Amputation Procedure Above or Below Knee"]
		) ) RightLowerAmputation
		where Coalesce(RightLowerAmputation.prevalencePeriod, RightLowerAmputation.relevantPeriod)starts on or before end of "Measurement Period"
