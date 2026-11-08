library LikeCMS32 version '0.0.003'

/*================================
eCQM ID: CMS902
Patient-based: No
Scoring: continuous variable
CMS Program Group: Eligible Hospitals (EH)
================================*/

using QDM version '5.6'

codesystem "SNOMEDCT": 'urn:oid:2.16.840.1.113883.6.96'

valueset "Discharge To Acute Care Facility": 'urn:oid:2.16.840.1.113883.3.117.1.7.1.87'
valueset "Emergency Department Visit": 'urn:oid:2.16.840.1.113883.3.117.1.7.1.292'
valueset "Encounter Inpatient": 'urn:oid:2.16.840.1.113883.3.666.5.307'
valueset "Ethnicity": 'urn:oid:2.16.840.1.114222.4.11.837'
valueset "Observation Services": 'urn:oid:2.16.840.1.113762.1.4.1111.143'
valueset "ONC Administrative Sex": 'urn:oid:2.16.840.1.113762.1.4.1'
valueset "Payer": 'urn:oid:2.16.840.1.114222.4.11.3591'
valueset "Psychiatric/Mental Health Diagnosis": 'urn:oid:2.16.840.1.113883.3.117.1.7.1.299'
valueset "Race": 'urn:oid:2.16.840.1.114222.4.11.836'

code "Patient deceased during stay (discharge status = dead) (finding)": '371828006' from "SNOMEDCT" display 'Patient deceased during stay (discharge status = dead) (finding)'

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

define "Emergency Department ED Visit":
  ["Encounter, Performed": "Emergency Department Visit"] EDVisit
    where EDVisit.relevantPeriod during "Measurement Period"

define "Strat2":
  "Emergency Department ED Visit" EDVisit
    where EDVisit.dischargeDisposition in "Discharge To Acute Care Facility"

define "Initial Population":
  ["Encounter, Performed": "Emergency Department Visit"] EDVisit
    where EDVisit.relevantPeriod during "Measurement Period"

define "Measure Population":
  "Initial Population" EDVisit
    where "Measure Observation"(EDVisit)is not null

define "Measure Population Exclusions":
  ( "Emergency Department ED Visit" EDVisit
      where EDVisit.dischargeDisposition ~ "Patient deceased during stay (discharge status = dead) (finding)"
  )
    union ( "Emergency Department ED Visit" EDVisit
        with ["Encounter, Performed": "Encounter Inpatient"] Encounter
          such that EDVisit.relevantPeriod ends 1 hour or less before or on start of Encounter.relevantPeriod
    )

define "Strat1":
  "Emergency Department ED Visit" EDVisit
    where exists ( EDVisit.diagnoses Diag
        where Diag.rank = 1
          and Diag.code in "Psychiatric/Mental Health Diagnosis"
    )

define "Strat3":
  "Emergency Department ED Visit" EDVisit
    let DiagCode: GetPrincipalDiagnosisCode(EDVisit)
    where ( DiagCode is null
        or not ( DiagCode in "Psychiatric/Mental Health Diagnosis" )
    )
      and not ( EDVisit.dischargeDisposition in "Discharge To Acute Care Facility" )

define function "Observation Services Order"(Enc "Encounter, Performed"):
  Last(["Encounter, Order": "Observation Services"] ObservationOrder
      where ObservationOrder.authorDatetime during Enc.relevantPeriod
      sort by authorDatetime
  )

define function "Departure Time"(Enc "Encounter, Performed"):
  Last(Enc.facilityLocations Location
      return 
      end of Location.locationPeriod
      sort ascending
  )

define function "Measure Observation"(Enc "Encounter, Performed"):
  "Arrival to Observation Order or Departure Time"(Enc)

define function "ArrivalTime"(Enc "Encounter, Performed"):
  First(Enc.facilityLocations Location
      return start of Location.locationPeriod
      sort ascending
  )

define function "Arrival to Observation Order or Departure Time"(Enc "Encounter, Performed"):
  duration in minutes of Interval["ArrivalTime"(Enc), Coalesce("Observation Services Order"(Enc).authorDatetime, "Departure Time"(Enc))]

define function "GetPrincipalDiagnosisCode"(Enc "Encounter, Performed"):
  singleton from ( Enc.diagnoses Diag
      where Diag.rank = 1
      return Diag.code
  )