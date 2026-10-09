# SMART Release & Return: daily self-testing with two lateral flow devices to hasten safe return of healthcare workers

**Paper.** Zhang X, Cheyne CP, Jones C, Humann M, Leeming G, Smith C, Hughes D, Burnside G, Dodd S, Penrice-Randal R, Dong D, Semple M, Neal T, Tunkel S, Fowler T, Turtle L, García-Fiñana M, Buchan I. Can self-testing be enhanced to hasten safe return of healthcare workers in pandemics? Random order, open label trial using two manufacturers’ SARS-CoV-2 lateral flow devices concurrently and nested viral culture study. *BMC Infectious Diseases* 2024;24:1276. https://doi.org/10.1186/s12879-024-10155-z

**What it is.** An urgent NHS pilot (IRAS 311842) at Liverpool University Hospitals NHS Foundation Trust, requested by the Merseyside Resilience Forum during the Omicron staffing crisis of December 2021, in which staff attending for a PCR test took two manufacturers’ lateral flow devices daily in random order, with a nested viral culture study, to establish how many consecutive negative LFTs are needed for safe release from isolation.

## Files

The study documents and the fully anonymised dataset are kept at the **root of this repository**, because the paper’s data availability statement links to those exact paths; please do not move or rename them.

| File | Contents |
|---|---|
| [`../../SMART_Release_Return.pdf`](../../SMART_Release_Return.pdf) | Study protocol, version 4.7 (26 July 2022), with earlier version history |
| [`../../SMART_RR_SAP.pdf`](../../SMART_RR_SAP.pdf) | Statistical analysis plan, version 1.2 (12 September 2022): Chris Cheyne, Girvan Burnside, Susanna Dodd, David Hughes, Iain Buchan, Marta García-Fiñana |
| [`../../SMART_RR_Participant_Information.pdf`](../../SMART_RR_Participant_Information.pdf) | Participant information sheet |
| [`../../SMART_RR_Anonymised_Data.zip`](../../SMART_RR_Anonymised_Data.zip) | `SMART_RR_anon_data.txt` (tab-delimited, 3,862 test records from 227 participants) and `SMART_RR_anon_data_datadictionary.xlsx` |

## Data dictionary (`SMART_RR_anon_data.txt`)

| Field | Description | Type | Categories |
|---|---|---|---|
| `anonID` | Anonymised person-level ID | 19-character alphanumeric | |
| `studyday` | Day from consent (day 0 = day of consent) | integer | |
| `TestOrder` | Order in which tests were recorded on a given day | integer | |
| `kit` | PCR or LFT | categorical | 1 PCR, 2 LFT, 999 NA |
| `result` | Test result | categorical | 0 negative, 1 positive, 999 equivocal |
| `code` | Test code | categorical | 1 COV19, 999 NA |
| `type` | LFT manufacturer | categorical | 1 Orient Gene, 2 Innova, 999 NA |
| `ProcessCode` | Processing route | categorical | 1 AP, 2 GLS, 3 LML, 4 LFDSelfImageReader, 5 LFDSelfTest, 999 NA |
| `ConsentNumber` | Consent number for the individual | integer | |

## Analysis code

The analyses specified in the SAP were run on a de-identified extract and the scripts have not yet been published. Adding them to this folder (as `analysis/`) is the outstanding task for this paper.
