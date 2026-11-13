# cql — Clinical Quality Language

[CQL](https://cql.hl7.org/) is the language health care writes clinical logic in: quality measures,
decision-support rules, cohort definitions. This package runs it. You write a library of named
definitions, run it over a patient's record, and get each definition's answer; a mistake in the
library is reported at its line.

```
$ cql adults.cql --data patient/ --define "Adult Female" --define "Tested" --define "Highest HbA1c"
Adult Female: true
Tested: true
Highest HbA1c: 9.4
```

It reads FHIR R4 data, QI-Core and US Core, QDM, or a data model you describe, and computes quality
measures over them.

## Install

Build the [fhir family's tools](https://github.com/oloveluck/fhir#install) once; `cql` is one of
them. With Racket, `raco cql` is the same command.

## Documentation

**[oloveluck.github.io/cql](https://oloveluck.github.io/cql/)**: a first example explained line by
line, how to write a library, measures, data models, and the reference.

- How well it works (1820 of the 1823 tests in HL7's CQL suite, and published measures over their
  own test patients): [TESTING.md](TESTING.md).
- For AI agents: [guide.md](guide.md), a cheat sheet and recipes, each one tested; and the
  [cql skill](.claude/skills/cql/SKILL.md).
- How it is built: [CLAUDE.md](CLAUDE.md).

Part of the [fhir](https://github.com/oloveluck/fhir) family. License: MIT.
