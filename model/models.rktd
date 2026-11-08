;; The data models shipped with the CQL engine (modelinfo.rkt reads this): name, version, url (so a
;; model is found by url without loading the others), the ModelForm
;; file (gen-modelinfo.rkt from the ModelInfo document beside it) and the representation file, or #f
;; for a mapping model, whose ELM reads its target model's data (QI-Core and US Core read FHIR).
(("FHIR" "4.0.1" "http://hl7.org/fhir" "fhir-4.0.1.rktd" "fhir-4.0.1.representation.rktd")
 ("QICore" "4.1.1" "http://hl7.org/fhir/us/qicore" "qicore-4.1.1.rktd" #f)
 ("QICore" "5.0.0" "http://hl7.org/fhir/us/qicore" "qicore-5.0.0.rktd" #f)
 ("QICore" "6.0.0" "http://hl7.org/fhir/us/qicore" "qicore-6.0.0.rktd" #f)
 ("USCore" "3.1.1" "http://hl7.org/fhir/us/core" "uscore-3.1.1.rktd" #f)
 ("USCore" "6.1.0" "http://hl7.org/fhir/us/core" "uscore-6.1.0.rktd" #f)
 ("QDM" "5.0.2" "urn:healthit-gov:qdm:v5_0_1_draft" "qdm-5.0.2.rktd" "qdm.representation.rktd")
 ("QDM" "5.3" "urn:healthit-gov:qdm:v5_3" "qdm-5.3.rktd" "qdm.representation.rktd")
 ("QDM" "5.4" "urn:healthit-gov:qdm:v5_4" "qdm-5.4.rktd" "qdm.representation.rktd")
 ("QDM" "5.5" "urn:healthit-gov:qdm:v5_5" "qdm-5.5.rktd" "qdm.representation.rktd")
 ("QDM" "5.6" "urn:healthit-gov:qdm:v5_6" "qdm-5.6.rktd" "qdm.representation.rktd"))
