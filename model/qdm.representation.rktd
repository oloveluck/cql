;; How QDM data look to the CQL engine (representation.rkt): the cqm-models JSON that Bonnie, MADiE
;; and cqm-execution exchange. A data element names its class under _type as "QDM::<Class>"; its codes
;; are the list dataElementCodes (the element `code` is the first; a retrieve matches any of them); its
;; id is `id`, else mongoose's `_id`;
;; written with bare OIDs as systems, which the libraries declare as urn:oid: urls; a choice element is
;; written under its own name; intervals are {low, high, lowClosed, highClosed}. A Positive/Negative
;; profile is the data element without/with a negationRationale (cqm-models' getByProfile).
((type-key . _type)
 (type-prefix . "QDM::")
 (choice . plain)
 (reference . key)
 (element-keys ("code" "dataElementCodes" "code") ("id" "id" "_id"))
 (code-system-prefix . "urn:oid:")
 (profiles ("Negative" present "negationRationale") ("Positive" absent "negationRationale")))
