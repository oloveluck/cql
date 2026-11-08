library TEST_VTEICU56 version '0.2.000'

using QDM version '5.6'

include MATGlobalCommonFunctionsNEW version '7.0.000' called Global

codesystem "LOINC": 'urn:oid:2.16.840.1.113883.6.1' 

valueset "Obstetrics": 'urn:oid:2.16.840.1.113883.3.117.1.7.1.263' 
valueset "Obstetrics VTE": 'urn:oid:2.16.840.1.113883.3.117.1.7.1.264' 
valueset "Venous Thromboembolism": 'urn:oid:2.16.840.1.113883.3.117.1.7.1.279' 

code "Birth date": '21112-8' from "LOINC" display 'Birth date'

parameter "Measurement Period" Interval<DateTime>

context Patient

define "Admission Without VTE or Obstetrical Conditions":
  Global."Inpatient Encounter" InpatientEncounter
    where not ( exists ( InpatientEncounter.diagnoses EncounterDiagnoses
          where ( EncounterDiagnoses.code in "Obstetrics"
              or EncounterDiagnoses.code in "Venous Thromboembolism"
              or EncounterDiagnoses.code in "Obstetrics VTE"
          )
      )
    )

define "Encounter With Age Range and Without VTE Diagnosis or Obstetrical Conditions":
  ( Global."Inpatient Encounter" InpatientEncounter
      with ["Patient Characteristic Birthdate": "Birth date"] BirthDate
        such that Global."CalendarAgeInYearsAt" ( BirthDate.birthDatetime, start of InpatientEncounter.relevantPeriod ) >= 18
  )
    intersect "Admission Without VTE or Obstetrical Conditions"

define function "FromDayOfStartOfHospitalizationToDayAfterAdmission"(Encounter "Encounter, Performed" ):
  Interval[Global."ToDate"(start of Global."HospitalizationWithObservation"(Encounter)), Global."ToDate"(start of Encounter.relevantPeriod + 2 days))

define function "StartOfFirstICU"(Encounter "Encounter, Performed" ):
  start of "FirstICULocationPeriod"(Encounter)

define function "FromDayOfStartOfHospitalizationToDayAfterFirstICU"(Encounter "Encounter, Performed" ):
  Interval[Global."ToDate"(start of Global."HospitalizationWithObservation"(Encounter)), Global."ToDate"(StartOfFirstICU(Encounter)+ 2 days))

define function "FirstICULocationPeriod"(Encounter "Encounter, Performed" ):
  Global."FirstInpatientIntensiveCareUnit" ( Encounter ).locationPeriod

define function "CalendarDayOfOrDayAfter"(StartValue DateTime ):
  Interval[Global."ToDate" ( StartValue ), Global."ToDate" ( StartValue + 2 days ) )

