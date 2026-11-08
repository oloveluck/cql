#!/usr/bin/env python3
"""extract.py SRC — vendor cqm-execution's QDM test fixtures (Apache-2.0) here.

SRC is a checkout of https://github.com/projecttacoma/cqm-execution. From spec/fixtures/json, for every
measure under cqm_measures/ that has patients under patients/:
  measures/<Measure>/<Library>.cql        each CQL library of the measure, verbatim, named by its own
                                          `library` line
  measures/<Measure>/<Library>.elm.json   its ELM as the measure document carries it (the reference
                                          translator's), annotations dropped
  measures/<Measure>/measure.json         the measure document without its libraries: ids, period,
                                          scoring, calculation method, population sets
  measures/<Measure>/value_sets.json      the measure's value sets, verbatim (cqm-models' format)
  patients/<Measure>/<case>.json          its test patients, verbatim (cqm-models' QDM patient JSON,
                                          with the expected populations)
"""
import json, os, re, sys, glob, shutil

src = os.path.join(sys.argv[1], "spec", "fixtures", "json")
here = os.path.dirname(os.path.abspath(__file__))

def noann(x):
    if isinstance(x, dict): return {k: noann(v) for k, v in x.items() if k != "annotation"}
    if isinstance(x, list): return [noann(v) for v in x]
    return x

for d in sorted(glob.glob(os.path.join(src, "cqm_measures", "*"))):
    name = os.path.basename(d)
    pats = sorted(glob.glob(os.path.join(src, "patients", name, "*.json")))
    docs = [f for f in glob.glob(os.path.join(d, "*.json")) if not f.endswith("value_sets.json")]
    if not pats or not docs: continue
    m = json.load(open(docs[0]))
    out = os.path.join(here, "measures", name)
    os.makedirs(out, exist_ok=True)
    for lib in m["cql_libraries"]:
        # each named by its own identifier — the CQL by its `library` line, the ELM by its identifier:
        # a document may attach the CQL texts to the wrong entries (CMS460v0's are shuffled; its ELM,
        # which is what cqm-execution runs, is not)
        name_ = lib["elm"]["library"]["identifier"]["id"]
        cql_name = re.search(r"^\s*library\s+\"?([A-Za-z0-9_]+)", lib["cql"], re.M).group(1)
        open(os.path.join(out, cql_name + ".cql"), "w").write(lib["cql"])
        json.dump(noann(lib["elm"]), open(os.path.join(out, name_ + ".elm.json"), "w"),
                  separators=(",", ":"), sort_keys=True)
    keep = ["cms_id", "title", "hqmf_set_id", "main_cql_library", "measure_period", "measure_scoring",
            "calculation_method", "composite", "population_sets", "measure_attributes"]
    json.dump({k: m[k] for k in keep if k in m}, open(os.path.join(out, "measure.json"), "w"), indent=1, sort_keys=True)
    shutil.copy(os.path.join(d, "value_sets.json"), out)
    pdir = os.path.join(here, "patients", name)
    os.makedirs(pdir, exist_ok=True)
    for p in pats: shutil.copy(p, pdir)
    print(name, len(m["cql_libraries"]), "libraries,", len(pats), "patients")
