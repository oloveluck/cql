#!/usr/bin/env python3
"""extract.py SRC — vendor a slice of cqframework/ecqm-content-qicore-2024 (CC0) here.

SRC is a checkout of https://github.com/cqframework/ecqm-content-qicore-2024. Writes:
  elm-corpus/ecqm-content-qicore-2024/<Library>.cql + <Library>.elm.json
                             every library with published ELM: its CQL (input/cql), verbatim, and
                             its ELM (input/resources/library, application/elm+json) as the reference
                             translator wrote it, annotations dropped
  measure/<Measure>.json     the measures below (input/resources/measure), verbatim
  tests/<Measure>/<case>/    their test cases (input/tests/measure), verbatim, and README.txt (the
                             case names); the loose files beside them (the same cases flattened) are not
  valueset/<file>.json       the value sets those measures' libraries declare, with expansions
                             (input/vocabulary/valueset/external), verbatim
"""
import base64, json, os, re, shutil, sys, glob

MEASURES = ["BreastCancerScreeningFHIR", "CervicalCancerScreeningFHIR",
            "ControllingHighBloodPressureFHIR", "DiabetesGlycemicStatusAssessmentGreaterThan9PercentFHIR"]
src = os.path.join(sys.argv[1], "input")
here = os.path.dirname(os.path.abspath(__file__))

def noann(x):
    if isinstance(x, dict): return {k: noann(v) for k, v in x.items() if k != "annotation"}
    if isinstance(x, list): return [noann(v) for v in x]
    return x

corpus = os.path.join(here, "elm-corpus", "ecqm-content-qicore-2024")
os.makedirs(corpus, exist_ok=True)
for f in sorted(glob.glob(os.path.join(src, "resources", "library", "*.json"))):
    lib = json.load(open(f))
    cql = os.path.join(src, "cql", lib["name"] + ".cql")
    for c in lib.get("content", []):
        if c["contentType"] == "application/elm+json" and os.path.exists(cql):
            elm = noann(json.loads(base64.b64decode(c["data"])))
            json.dump(elm, open(os.path.join(corpus, lib["name"] + ".elm.json"), "w"), separators=(",", ":"), sort_keys=True)
            shutil.copy(cql, corpus)

def includes(n, seen):
    if n in seen: return
    seen.add(n)
    for m in re.findall(r"^include\s+(\w+)", open(os.path.join(src, "cql", n + ".cql")).read(), re.M):
        includes(m, seen)
libs = set()
for m in MEASURES: includes(m, libs)
urls = set()
for n in libs:
    urls |= set(re.findall(r"^valueset\s+\"[^\"]*\"\s*:\s*'([^']+)'", open(os.path.join(src, "cql", n + ".cql")).read(), re.M))

os.makedirs(os.path.join(here, "measure"), exist_ok=True)
for m in MEASURES:
    shutil.copy(os.path.join(src, "resources", "measure", m + ".json"), os.path.join(here, "measure"))
    dst = os.path.join(here, "tests", m)
    if os.path.exists(dst): shutil.rmtree(dst)
    # the case directories and README.txt (case names); the measure directory's loose files are
    # MADiE's flattened copy of the same cases (resource ids made unique), left out
    shutil.copytree(os.path.join(src, "tests", "measure", m), dst,
                    ignore=lambda d, names: [n for n in names if d.rstrip("/").endswith(m) and n.endswith(".json")])
os.makedirs(os.path.join(here, "valueset"), exist_ok=True)
found = set()
for f in sorted(glob.glob(os.path.join(src, "vocabulary", "valueset", "external", "*.json"))):
    vs = json.load(open(f))
    if vs.get("url") in urls:
        shutil.copy(f, os.path.join(here, "valueset")); found.add(vs["url"])
print(len(libs), "libraries for the measures;", len(urls), "value sets declared,", len(found), "found")
for u in sorted(urls - found): print("  missing value set:", u)
