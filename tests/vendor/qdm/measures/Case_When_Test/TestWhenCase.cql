library TestWhenCase version '0.0.008'

using QDM version '5.6'

valueset "Digoxin Medications": 'urn:oid:2.16.840.1.113883.3.464.1003.1065'

parameter "Measurement Period" Interval<DateTime>

context Patient

define "Initial Population":
  exists "test when then case"

define "test when then case":
  ( "More Than One Order"(["Medication, Order": "Digoxin Medications"]) ) DigoxinOrdered
    return DigoxinOrdered Order
      let MedicationStrength: ( case
          when Order.refills > 10 then true
          when Order.refills < 10 then false 
          else null end
      )
      return MedicationStrength

define function "More Than One Order"(Medication List<"Medication, Order">):
  "Medication" OrderMedication1
    with "Medication" OrderMedication2
      such that ( OrderMedication1.authorDatetime during "Measurement Period"
          and OrderMedication1.refills >= 1
      )
        or ( date from OrderMedication1.authorDatetime !~ date from OrderMedication2.authorDatetime
            and OrderMedication1.authorDatetime during "Measurement Period"
            and OrderMedication2.authorDatetime during "Measurement Period"
        )
        or ( date from OrderMedication1.authorDatetime ~ date from OrderMedication2.authorDatetime
            and OrderMedication1.authorDatetime during "Measurement Period"
            and date from start of OrderMedication1.relevantPeriod !~ date from start of OrderMedication2.relevantPeriod
            and start of OrderMedication1.relevantPeriod during "Measurement Period"
            and start of OrderMedication2.relevantPeriod during "Measurement Period"
        )
    return OrderMedication1