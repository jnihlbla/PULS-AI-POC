# Reference Metadata

Manually transcribed reference data that does not fit the SOPLIB or IMS provenance categories.

## Files

- `system_areas.jsonl`: System/program-number prefix to functional area mapping. Fields: `system`, `description`, `area`.

`system` may be a single prefix, a range (e.g. `WF0201-WF0219`), or a wildcard (e.g. `WXTRS*`). Area codes:

| Area  | Meaning |
|-------|---------|
| TEKN  | Technical/platform team |
| F&PL  | Forecast and Planning |
| UTOR  | Outbound Order |
| EKON  | Finance |
| UTWH  | Outbound Warehouse |
| INLE  | Inbound |

An `area` of `null` means the source row's area was unclear or unassigned.

## Provenance

Manually transcribed from a human-maintained mainframe document (the `WSYST` journal member: a team ownership/on-call roster with a system-to-description-to-area table). Personnel and contact-initial columns were intentionally excluded — they change too often and aren't relevant to code analysis. Treat this file as a heuristic ownership/capability hint, not a verified fact — confirm anything load-bearing against JCL, COBOL, PSB, or DBD evidence.
