library CombinedVTEandBleedingRatio version '0.0.002'

using QDM version '5.6'

include THATKACommon version '1.0.000' called THA_TKA_Common

valueset "Ethnicity": 'urn:oid:2.16.840.1.114222.4.11.837'
valueset "ONC Administrative Sex": 'urn:oid:2.16.840.1.113762.1.4.1'
valueset "Payer": 'urn:oid:2.16.840.1.114222.4.11.3591'
valueset "Race": 'urn:oid:2.16.840.1.114222.4.11.836'

context Patient

define "Bleeding Weight":
  1

define "Initial Population":
  THA_TKA_Common."Initial Population"

define "Denominator":
  THA_TKA_Common."Denominator"

/*For the measure population exclusions, we only want to exclude characteristics that are common to both events. Otherwise, we might exclude an event for a VTE exclusion, and end up missing it when it should have counted for a Bleeding event.*/

define "Denominator Exclusion":
  THA_TKA_Common."Common Denominator Exclusions"

define "Numerator":
  THA_TKA_Common."Bleeding-Specific Numerator"
    or THA_TKA_Common."VTE-Specific Numerator"

define "SDE Ethnicity":
  ["Patient Characteristic Ethnicity": "Ethnicity"]

define "SDE Payer":
  ["Patient Characteristic Payer": "Payer"]

define "SDE Race":
  ["Patient Characteristic Race": "Race"]

define "SDE Sex":
  ["Patient Characteristic Sex": "ONC Administrative Sex"]

define "VTE Weight":
  2.7

define function "Measure Denominator Score"():
  ( ToScore(THA_TKA_Common."Denominator"
        and not THA_TKA_Common."VTE-Specific Denominator Exclusions"
    )* "VTE Weight"
  ) + ( ToScore(THA_TKA_Common."Denominator"
        and not THA_TKA_Common."Bleeding-Specific Denominator Exclusions"
    )* "Bleeding Weight"
  )

/*So this is a patient-based linear combination, which we can then weight by introducing the weighting factor as a component of the measure observation calculation*/

define function "Measure Numerator Score"():
  ( ToScore(THA_TKA_Common."VTE-Specific Numerator"
        and not THA_TKA_Common."VTE-Specific Denominator Exclusions"
    )* "VTE Weight"
  ) + ( ToScore(THA_TKA_Common."Bleeding-Specific Numerator"
        and not THA_TKA_Common."Bleeding-Specific Denominator Exclusions"
    )* "Bleeding Weight"
  )

/*This function is used to convert the boolean-valued determination of an event to a score.*/

define function "ToScore"(Value Boolean):
  if Value then 1 
    else 0