# SOPLIB Metadata

Generated metadata for scheduler structure and execution flow. The files in this directory complement the JCL, PROCLIB, COBOL, copybook, PSB, and DBD sources in this repository.

## Files

- `members.jsonl`: One record per SYSTEM, ROUTINE, or JOB member, including hierarchy, children, catalogue words, and scheduling properties.
- `dependencies.jsonl`: Ordering relationships, mutual-exclusion rules, and classification records extracted from SOPLIB members.

For IMS transaction/PSB mapping, see `metadata/ims/`. For the system/functional-area ownership index, see `metadata/reference/`.

## Dependency schema

Ordering records use `relation: ">"`:

```json
{
  "member": "TIMESAM",
  "predecessor": "TIME0400",
  "successor": "WBMPS004",
  "relation": ">",
  "predecessor_external": true,
  "successor_external": true
}
```

`predecessor` must complete before `successor` can start. The external flags mean that the endpoint is not declared in the current member's child list. They do not mean that the endpoint is absent from the repository.

Exclusion records use `relation: "<>"`:

```json
{
  "member": "WBATCH2",
  "job1": "JOB1",
  "job2": "JOB2",
  "relation": "<>",
  "self_exclusion": false
}
```

Equal `job1` and `job2` values indicate self-exclusion. Different values indicate that the two jobs must not run concurrently.

Classification records use a single `job` field:

- `relation: "root"`: the job has no predecessor in the current member's dependency graph. Multiple roots are valid and may start independently or in parallel.
- `relation: "independent"`: the job is declared by the member but has no explicit ordering relationship.

## Using the metadata

Use `members.jsonl` for structural membership and `dependencies.jsonl` for scheduler flow. For a job-impact or what-if analysis, follow the job from the metadata into its JCL file, then into any invoked PROCLIB procedure, and finally into the program, copybook, PSB, DBD, dataset, and return-code references found there.

Job names and application program names do not always match directly. Treat JCL `EXEC` statements, procedure parameters, `PGM` values, and application identifiers as evidence for each hop, and identify unresolved or convention-based links explicitly.

## Provenance

These files are generated from SOPLIB `.sop` members by `soplib_parser.py`. Regenerate them when the source SOPLIB extract changes. They describe scheduler metadata and are not a complete runtime model of failure handling, restart behavior, data dependencies, or external systems.
