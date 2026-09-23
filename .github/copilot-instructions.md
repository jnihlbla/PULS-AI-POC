# PULS-Cobol Analysis Guidance

## Repository layers

- `metadata/soplib/members.jsonl`: SYSTEM, ROUTINE, and JOB hierarchy.
- `metadata/soplib/dependencies.jsonl`: scheduler ordering, mutual exclusion, root, and independent classifications.
- `JCL/`: job definitions and EXEC procedure references.
- `Proclib/`: reusable JCL procedures and program execution steps.
- `Cobol/`: COBOL application source.
- `CobolCpy/`: COBOL copybooks.
- `PSB/`: IMS program specifications.
- `DBD/`: IMS database definitions.
- `MFS/`: IMS Message Format Service screen definitions (Swedish/English variants of the same screen).
- `metadata/ims/transactions.jsonl`: IMS transaction-to-PSB mapping (trancode, psb_name, program_name).
- `Constant/`, `Eplus/`, and `EplusCpy/`: supporting definitions and data structures.

## Analysis workflow

For scheduler and flow questions, start with the SOPLIB metadata.

For a job implementation or impact question, follow this order:

1. Find the job in `metadata/soplib/members.jsonl` and `metadata/soplib/dependencies.jsonl`.
2. Open the corresponding file under `JCL/`.
3. Follow `EXEC` procedure names into `Proclib/`.
4. Inspect `PGM`, `PARM`, DD statements, and included members.
5. Resolve application programs into `Cobol/`.
6. Follow `COPY`, PSB, DBD, dataset, and called-program references.

Do not assume that a job name, procedure name, and COBOL program name are identical. Use JCL `EXEC` statements, procedure parameters, `PGM` values, application identifiers, and source references as evidence for each hop.

Resolve every COPY reference in a COBOL program against `CobolCpy/` (or `EplusCpy/` for Eplus copybooks), even when it does not look like a standard `COPY member.` statement — e.g. a preprocessor-style `-COPY` directive, a `COPY` inside a comment, continuation line, or embedded in a compiler directive. Search for the member name itself in `CobolCpy/`/`EplusCpy/` if the statement syntax is unfamiliar, rather than skipping it because it doesn't match the expected COBOL grammar.

## Metadata interpretation

In `dependencies.jsonl`:

- `relation: ">"` means the `predecessor` must complete before the `successor` can start.
- `predecessor_external` and `successor_external` mean the endpoint is not declared in the current member's child list. They do not mean the endpoint is absent from the repository.
- `relation: "<>"` means mutual exclusion. Equal `job1` and `job2` values indicate self-exclusion; different values indicate that the two jobs must not run concurrently.
- `relation: "root"` means the job has no predecessor in the current member's dependency graph. Multiple roots are valid and may start independently or in parallel.
- `relation: "independent"` means the job is declared by the member but has no explicit ordering relationship.

Keep hierarchy relationships from `members.jsonl` separate from execution relationships in `dependencies.jsonl`.

## Naming conventions

- IMS transaction to PSB/program: see `metadata/ims/transactions.jsonl`. `program_name = psb_name + "00"` (all IMS programs are COBOL).
- MFS member to PSB/program: MFS names are 6 characters; position 3 is a language indicator (`F`=Swedish, `N`=English). Replacing position 3 with `0` gives the PSB name (e.g. `W0F107`/`W0N107` -> `W00107`); appending `00` to that PSB name gives the program name (e.g. `W00107` -> `W0010700`). Both language variants of an MFS screen map to the same PSB and program.
- System/program prefix to functional area: see `metadata/reference/system_areas.jsonl` for a prefix-to-description-to-area mapping (areas: TEKN=technical, F&PL=forecast/planning, UTOR=outbound order, EKON=finance, UTWH=outbound warehouse, INLE=inbound). This is a manually transcribed, possibly incomplete or stale ownership index, not a verified fact — use it to form hypotheses about capability/domain, then confirm against JCL, COBOL, PSB, or DBD evidence.
- Program/load-module name: `Wsssppmm` — `sss`=system number, `pp`=program id, `mm`=`00` for the main program or a subroutine id otherwise (e.g. `W40511` main is `W4051100`, subroutine `10` is `W2111510`). Online systems have `0` in position 3; a non-zero value there usually indicates a batch module. Load module and PSB names normally omit the trailing `mm`/`00`.
- Common/shared subroutine prefixes: `Wsss` = subroutine scoped to system `sss` (may still be called by other systems); `W009` and `W006` = subroutines common to all systems (`W006` specifically for IMS online); `WZnn` = "new" common subroutines (e.g. `WZ11RECV`, `WZ11SEND`).
- DD names in JCL/Proclib: `WsssppDx` = program name (`Wssspp`) + literal `D` + a sequence id (`1`-`9`, `a`-`z`).
- Copybook (`CobolCpy`/`EplusCpy`) naming has several overlapping patterns and is not fully standardized: `Wsssxxx` (record id), `Wsssxx` (file id), `Wssspppp` (subroutine), `Wdddnn` (`Wddd`=database, `nn`=segment number, root segment = `01`). Copybooks for database segments are usually named after the segment.
- DBD naming: physical DBDs are `Wdan[b]` (`d`=fixed `D` or first char of system id, `a`=database group letter, `n`=sequence number in group, optional `b`=secondary-index letter); logical DBDs are `WLxxxy[b]` (`xxx`=database group name, `y`=sequence letter, optional `b`=secondary-index letter).
- Production/test file naming in JCL/Proclib: production is `Wsss.xxxxxx.Wsssff(gen)` (`sss`=system, `xxxxxx`=routine name, `ff`=file id); test files use the same pattern under a `W.stage.` or `W.XDEV.` prefix.
- Proclib member naming: `WsssPxxx` (`sss`=system, `xxx`=procedure id, often `0xx` where `xx` is the program id). JCL/job naming: `WsssJxxx` (`xxx` normally matches the main procedure executed); `WsssrrRS`/`WsssrrRE` start/end a job within routine `Wsssrr`. Other naming patterns exist for special job types.
- Transaction naming (trancode, position-based): position 3 `A` = API transaction (e.g. `WLA134`, `W4A343`); position 3 `W` = web PULS; position 3 `T` = classic MFS transaction (occasionally reused in web). Position 7 `X`/`Y` = background transaction (no UI/API/web/screen); position 7 `U` = update access, except API transactions where the same `A` transaction handles both read and update.
- SOP routine frequency suffix: `WsssXn` where `X` indicates scheduling frequency — `S`=multiple times/day, `D`=daily, `V`=weekly, `M`=monthly, `B`=on-demand (ordered), `P`=periodic, `R`=periodic/accounting-period, `Q`=quarterly, `Y`=yearly, `X`=triggered by MQ/VCOM message arrival.

## What-if analysis

Distinguish confirmed facts from naming-based candidates and unresolved references. For failure or removal analysis, identify:

- Direct predecessors and successors.
- Downstream jobs that may be blocked.
- Root or independent status.
- Mutual-exclusion constraints.
- JCL and PROCLIB implementation steps.
- COBOL programs, copybooks, PSBs, DBDs, datasets, and called programs involved.
- IF-ABEND, return-code, restart, and recovery behavior when present.

Do not claim that a job failure definitely prevents a successor from running unless the available scheduler, JCL, or application evidence supports that conclusion. State assumptions and missing external scheduler behavior explicitly.
