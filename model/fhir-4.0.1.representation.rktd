;; How FHIR JSON looks to the CQL engine (representation.rkt): a resource names its type under
;; resourceType; a choice element is written as name + Type (valueQuantity); a primitive may be a bare
;; JSON scalar standing for its `value` element; a Reference names a record as "Type/id".
((type-key . resourceType) (choice . suffixed) (scalar-element . "value") (reference . typed))
