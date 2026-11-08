library StatinTherapy version '0.0.004'

using QDM version '5.0.2'

valueset "ONC Administrative Sex": 'urn:oid:2.16.840.1.113762.1.4.1' 

valueset "Race": 'urn:oid:2.16.840.1.114222.4.11.836' 

valueset "Ethnicity": 'urn:oid:2.16.840.1.114222.4.11.837' 

valueset "Payer": 'urn:oid:2.16.840.1.114222.4.11.3591' 

valueset "Office Visit": 'urn:oid:2.16.840.1.113883.3.464.1003.101.11.1005' 

valueset "Outpatient Consultation": 'urn:oid:2.16.840.1.113883.3.464.1003.101.11.1040' 

valueset "Preventive Care Services-Initial Office Visit, 18 and Up": 'urn:oid:2.16.840.1.113883.3.464.1003.101.11.1115' 

valueset "Preventive Care Services - Established Office Visit, 18 and Up": 'urn:oid:2.16.840.1.113883.3.464.1003.101.11.1125' 

valueset "Preventive Care Services-Individual Counseling": 'urn:oid:2.16.840.1.113883.3.464.1003.101.12.1026' 

valueset "Preventive Care Services - Other": 'urn:oid:2.16.840.1.113883.3.464.1003.101.12.1030' 

valueset "Annual Wellness Visit": 'urn:oid:2.16.840.1.113883.3.526.2.1363' 

valueset "Outpatient Encounters for Preventive Care": 'urn:oid:2.16.840.1.113762.1.4.1047.9' 

valueset "PCI": 'urn:oid:2.16.840.1.113762.1.4.1045.67' 

valueset "Myocardial Infarction": 'urn:oid:2.16.840.1.113883.3.526.3.403' 

valueset "Cerebrovascular disease, Stroke, TIA": 'urn:oid:2.16.840.1.113762.1.4.1047.44' 

valueset "CABG Surgeries": 'urn:oid:2.16.840.1.113883.3.666.5.694' 

valueset "Atherosclerosis and Peripheral Arterial Disease": 'urn:oid:2.16.840.1.113762.1.4.1047.21' 

valueset "Carotid Intervention": 'urn:oid:2.16.840.1.113883.3.117.1.7.1.204' 

valueset "Ischemic heart disease or coronary occlusion, rupture, or thrombosis": 'urn:oid:2.16.840.1.113762.1.4.1047.46' 

valueset "Stable and Unstable Angina": 'urn:oid:2.16.840.1.113762.1.4.1047.47' 

valueset "Palliative Care": 'urn:oid:2.16.840.1.113883.3.600.1.1579' 

valueset "End Stage Renal Disease": 'urn:oid:2.16.840.1.113883.3.526.3.353' 

valueset "Hepatitis A": 'urn:oid:2.16.840.1.113883.3.464.1003.110.12.1024' 

valueset "Hepatitis B": 'urn:oid:2.16.840.1.113883.3.67.1.101.1.269' 

valueset "Liver Disease": 'urn:oid:2.16.840.1.113762.1.4.1047.42' 

valueset "Statin Allergen": 'urn:oid:2.16.840.1.113883.3.117.1.7.1.423' 

valueset "Pregnancy Dx": 'urn:oid:2.16.840.1.113883.3.600.1.1623' 

valueset "Breastfeeding": 'urn:oid:2.16.840.1.113762.1.4.1047.73' 

valueset "Rhabdomyolysis": 'urn:oid:2.16.840.1.113762.1.4.1047.102' 

valueset "High intensity statin therapy": 'urn:oid:2.16.840.1.113762.1.4.1047.97' 

valueset "Low intensity statin therapy": 'urn:oid:2.16.840.1.113762.1.4.1047.107' 

valueset "Moderate intensity statin therapy": 'urn:oid:2.16.840.1.113762.1.4.1047.98' 

valueset "LDL Test": 'urn:oid:2.16.840.1.113883.3.464.1003.198.11.1029' 

valueset "Hypercholesterolemia": 'urn:oid:2.16.840.1.113762.1.4.1047.100' 

valueset "Diabetes": 'urn:oid:2.16.840.1.113883.3.464.1003.103.12.1001' 

parameter "Measurement Period" Interval<DateTime>

context Patient

define "SDE Ethnicity": ["Patient Characteristic Ethnicity": "Ethnicity"]

define "SDE Payer": ["Patient Characteristic Payer": "Payer"]

define "SDE Race": ["Patient Characteristic Race": "Race"]

define "SDE Sex": ["Patient Characteristic Sex": "ONC Administrative Sex"]

define "Valid Encounters": ["Encounter, Performed": "Annual Wellness Visit"]
union ["Encounter, Performed":"Office Visit"]
union ["Encounter, Performed":"Outpatient Consultation"]
union ["Encounter, Performed":"Outpatient Encounters for Preventive Care"]
union ["Encounter, Performed":"Preventive Care Services - Established Office Visit, 18 and Up"]
union ["Encounter, Performed":"Preventive Care Services - Other"]
union ["Encounter, Performed":"Preventive Care Services-Individual Counseling"]
union ["Encounter, Performed":"Preventive Care Services-Initial Office Visit, 18 and Up"]

define "History of Clinical ASCVD Procedures": ["Procedure, Performed": "PCI"]
 union ["Procedure, Performed": "CABG Surgeries"]
 union ["Procedure, Performed": "Carotid Intervention"]

define "History of Clinical ASCVD Diagnoses": ["Diagnosis": "Myocardial Infarction"]
union ["Diagnosis": "Cerebrovascular disease, Stroke, TIA" ]
union ["Diagnosis": "Atherosclerosis and Peripheral Arterial Disease" ]
union ["Diagnosis": "Ischemic heart disease or coronary occlusion, rupture, or thrombosis" ]
union ["Diagnosis": "Stable and Unstable Angina" ]

define "PalliativeCare": exists (["Intervention, Performed":"Palliative Care"] palliative 
 where palliative.relevantPeriod overlaps "Measurement Period")

define "MedsAdverse": exists (["Adverse Event": "Statin Allergen"] adverse
 where adverse.relevantPeriod overlaps "Measurement Period")

define "MedsAllergyIntolerance": exists (["Allergy/Intolerance": "Statin Allergen"] allergy
 where allergy.prevalencePeriod overlaps "Measurement Period")

define "In Demographic": AgeInYearsAt(start of "Measurement Period") >= 21

define "Encounters During Measurement Period": "Valid Encounters" Encounter
where Encounter.relevantPeriod during "Measurement Period"

define "Initial Population": "In Demographic" and
exists("Encounters During Measurement Period")

define "Denominator 1": "Initial Population" and
("ASCVD Diagnosis Before MP" or
"ASCVD Procedure Before MP")

define "ASCVD Diagnosis Before MP": exists ("History of Clinical ASCVD Diagnoses" diag
where diag.prevalencePeriod starts before end of "Measurement Period")

define "ASCVD Procedure Before MP": exists ("History of Clinical ASCVD Procedures" proc
where proc.relevantPeriod starts before end of "Measurement Period")

define "Statin Active": ["Medication, Active": "Low intensity statin therapy"]
union ["Medication, Active": "Moderate intensity statin therapy"]
union ["Medication, Active": "High intensity statin therapy"]

define "Statin starts before end of MP": "Statin Active" statin
where statin.relevantPeriod starts before end of "Measurement Period" and not
 (statin.relevantPeriod ends before end of "Measurement Period")

define "Statin order": ["Medication, Order": "Low intensity statin therapy"]
union ["Medication, Order": "Moderate intensity statin therapy"]
union ["Medication, Order": "High intensity statin therapy"]

define "Statin order during MP": "Statin order" statin
where statin.authorDatetime during "Measurement Period"

define "Numerator": exists ("Statin order during MP") or
exists ("Statin starts before end of MP")

define "LDL gte 190": exists (["Laboratory Test, Performed": "LDL Test"] Ldl
where Ldl.result >= 190'mg/dl' and 
    Ldl.relevantPeriod starts before end of "Measurement Period")

define "Hypercholesterolemia Diagnosis": exists (["Diagnosis":"Hypercholesterolemia"] Hyper
 where Hyper.prevalencePeriod starts before end of "Measurement Period")

define "Denominator 2": "Initial Population" and
("LDL gte 190" or
"Hypercholesterolemia Diagnosis") and not 
("ASCVD Diagnosis Before MP" or 
"ASCVD Procedure Before MP")

define "Pregnancy": exists (["Diagnosis": "Pregnancy Dx"] preg
where preg.prevalencePeriod overlaps "Measurement Period")

define "Breastfeed": exists (["Diagnosis": "Breastfeeding"] breastfeed
where breastfeed.prevalencePeriod overlaps "Measurement Period")

define "Rhabdo": exists (["Diagnosis": "Rhabdomyolysis"] rhabdo
 where rhabdo.prevalencePeriod overlaps "Measurement Period")

define "Denex": "Pregnancy" or
"Breastfeed" or
"Rhabdo"

define "ESRD": exists (
 ["Diagnosis":"End Stage Renal Disease"] esrd
where esrd.prevalencePeriod during "Measurement Period")

define "HepA": exists (
 ["Diagnosis":"Hepatitis A"] HepA
where HepA.prevalencePeriod during "Measurement Period")

define "HepB": exists (
 ["Diagnosis":"Hepatitis B"] HepB
where HepB.prevalencePeriod during "Measurement Period")

define "Liver Disease Diagnosis": exists (
 ["Diagnosis":"Liver Disease"] Liver
where Liver.prevalencePeriod during "Measurement Period")

define "Denexcep": "PalliativeCare" or
"ESRD" or
"HepA" or
"HepB" or
"Liver Disease Diagnosis" or
"MedsAdverse" or
"MedsAllergyIntolerance"

define "Age 40 to 75": AgeInYearsAt(start of "Measurement Period") >=40 and
 AgeInYearsAt(start of "Measurement Period") <=75

define "Diabetes Diagnosis": exists (["Diagnosis": "Diabetes"] diab
 where diab.prevalencePeriod starts before end of "Measurement Period")

define "Denominator 3": "Initial Population" and
"Diabetes Diagnosis" and
"LDL 70 to 189 2 years prior" and
"Age 40 to 75" and not
"ASCVD Diagnosis Before MP" and not
"ASCVD Procedure Before MP" and not 
"LDL gte 190" and not
"Hypercholesterolemia Diagnosis"

define "LDL between 70 and 189": ["Laboratory Test, Performed": "LDL Test"] ldl
where ldl.result < 190 'mg/dL' and ldl.result >=70 'mg/dL'

define "LDL 70 to 189 2 years prior": exists ("LDL between 70 and 189" ldl
where ldl.relevantPeriod starts 2 years before end "Measurement Period")

define "LDL in past 2 years": ["Laboratory Test, Performed": "LDL Test"] ldl
  where ldl.relevantPeriod starts 2 years before start "Measurement Period"   and ldl.result is not null
  sort by start of ldl.relevantPeriod

define "LDL lt 70": exists ({
 "Most recent LDL" recldl
  where recldl.result <70 'mg/dL'
 })

define "Most recent LDL": Last ("LDL in past 2 years")

define "Denexcep Den3": "PalliativeCare" or
"ESRD" or
"HepA" or
"HepB" or
"Liver Disease Diagnosis" or
"MedsAdverse" or
"MedsAllergyIntolerance" or
"LDL lt 70"

