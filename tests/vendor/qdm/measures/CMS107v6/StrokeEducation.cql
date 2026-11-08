library StrokeEducation version '6.1.011'

using QDM version '5.3'

include TJC_Overall version '7.0.000' called TJC
include MATGlobalCommonFunctions version '2.0.000' called Global

valueset "ONC Administrative Sex": 'urn:oid:2.16.840.1.113762.1.4.1'
valueset "Race": 'urn:oid:2.16.840.1.114222.4.11.836'
valueset "Ethnicity": 'urn:oid:2.16.840.1.114222.4.11.837'
valueset "Payer": 'urn:oid:2.16.840.1.114222.4.11.3591'
valueset "Activation of Emergency Medical System Education": 'urn:oid:2.16.840.1.113883.3.117.1.7.1.377'
valueset "Comfort Measures": 'urn:oid:1.3.6.1.4.1.33895.1.3.0.45'
valueset "Discharge To Home Or Police Custody": 'urn:oid:2.16.840.1.113883.3.117.1.7.1.82'
valueset "Instructions for Follow Up After Discharge": 'urn:oid:2.16.840.1.113883.3.117.1.7.1.378'
valueset "Patient Refusal": 'urn:oid:2.16.840.1.113883.3.117.1.7.1.93'
valueset "Prescribed Medications Education": 'urn:oid:2.16.840.1.113883.3.117.1.7.1.379'
valueset "Risk Factors Education": 'urn:oid:2.16.840.1.113883.3.117.1.7.1.380'
valueset "Warning Signs and Symptoms Education": 'urn:oid:2.16.840.1.113883.3.117.1.7.1.381'
valueset "Written Information Given": 'urn:oid:2.16.840.1.113883.3.117.1.7.1.415'

context Patient

define "SDE Ethnicity":
	["Patient Characteristic Ethnicity": "Ethnicity"]

define "SDE Payer":
	["Patient Characteristic Payer": "Payer"]

define "SDE Race":
	["Patient Characteristic Race": "Race"]

define "SDE Sex":
	["Patient Characteristic Sex": "ONC Administrative Sex"]

define "Comfort Measures During Hospitalization":
	"Encounter With Discharge Status of Discharge To Home Or Police Custody" A
		with "Intervention Comfort Measures" B
			such that Coalesce(start of B.relevantPeriod, B.authorDatetime)during Global."Hospitalization"(A)

define "Denominator":
	"Encounter With Discharge Status of Discharge To Home Or Police Custody"

define "Denominator Exclusions":
	"Comfort Measures During Hospitalization"

define "Encounter With Discharge Status of Discharge To Home Or Police Custody":
	TJC."Encounter with Principal Diagnosis and Age" A
		where A.dischargeDisposition in "Discharge To Home Or Police Custody"

define "Initial Population":
	TJC."Encounter with Principal Diagnosis and Age"

define "Intervention Comfort Measures":
	["Intervention, Order": "Comfort Measures"]
		union ["Intervention, Performed": "Comfort Measures"]

define "Numerator":
	distinct(((("Encounter With Discharge Status of Discharge To Home Or Police Custody" A
						with ["Communication: From Provider To Patient": "Activation of Emergency Medical System Education"] B
							such that B.authorDatetime during A.relevantPeriod
				)
					intersect("Encounter With Discharge Status of Discharge To Home Or Police Custody" A
							with ["Communication: From Provider To Patient": "Instructions for Follow Up After Discharge"] C
								such that C.authorDatetime during A.relevantPeriod
					)
					intersect("Encounter With Discharge Status of Discharge To Home Or Police Custody" A
							with ["Communication: From Provider To Patient": "Prescribed Medications Education"] D
								such that D.authorDatetime during A.relevantPeriod
					)
					intersect("Encounter With Discharge Status of Discharge To Home Or Police Custody" A
							with ["Communication: From Provider To Patient": "Risk Factors Education"] E
								such that E.authorDatetime during A.relevantPeriod
					)
					intersect("Encounter With Discharge Status of Discharge To Home Or Police Custody" A
							with ["Communication: From Provider To Patient": "Warning Signs and Symptoms Education"] F
								such that F.authorDatetime during A.relevantPeriod
					)
					intersect("Encounter With Discharge Status of Discharge To Home Or Police Custody" A
							with ["Communication: From Provider To Patient": "Written Information Given"] G
								such that G.authorDatetime during A.relevantPeriod
					)
					union("Written Information Not Done During IP")
			)
		)
	)

define "Written Information Not Done During IP":
	"Encounter With Discharge Status of Discharge To Home Or Police Custody" E
		with "Written Information Not Given" W
			such that W.authorDatetime during E.relevantPeriod

define "Written Information Not Given":
	["Communication: From Provider To Patient, Not Done": "Written Information Given"] W
		where W.negationRationale in "Patient Refusal"
