# PV System Design Portfolio

Three self-initiated concept studies in the Nürnberg region, each worked through the same way: **size by hand first, then check in CAD and PVsyst, then decide with a 25-year economic model.**

**Mohamad Tayssir Almariri**, M.Eng. student, Energy Engineering & Management (Solar & PV Design), TH Nürnberg / HS Ansbach
[LinkedIn](https://www.linkedin.com/in/-tayssir-almariri) · tayssir.almariri@gmail.com

---

## 1. Residential PV + battery, 4,500 kWh/yr → [`01-residential-4500kWh`](01-residential-4500kWh)

| | |
|---|---|
| System | 5.66 kWp (13 × 435 Wp), SMA STP 5.0 Smart Energy, SMA Home Storage 6.56 kWh |
| Yield | 6,403 kWh/yr · 1,132 kWh/kWp · PR 84.4 % |
| Autarky | 39 % without → 70 % with battery |
| Economics | payback 8.2 yr · NPV 11,013 € · IRR 10.6 % (with battery) |

## 2. Residential PV + battery, 9,000 kWh/yr → [`02-residential-9000kWh`](02-residential-9000kWh)

| | |
|---|---|
| System | 10.88 kWp (25 × 435 Wp), SMA STP 10.0 Smart Energy, SMA Home Storage 9.84 kWh |
| Strings | 10 + 15, matched to the unequal MPP inputs (6 kW / 12 kW) |
| Yield | 12,559 kWh/yr · 1,155 kWh/kWp · PR 86.1 % |
| Autarky | 39 % without → 65 % with battery |
| Economics | payback 7.3 yr · NPV 24,752 € · IRR 12.7 % (with battery) |

## 3. Commercial rooftop, 512 kWp logistics centre → [`03-commercial-512kWp`](03-commercial-512kWp)

| | |
|---|---|
| System | 1,078 × 475 Wp, 77 strings, 4 × SMA STP110-60 (DC/AC 1.16) |
| Design | row pitch 6.29 m (landscape), DC/AC cable sizing, single-line diagram |
| Yield | 584 MWh/yr · 1,141 kWh/kWp · PR 84.2 % |
| Economics | LCOE 8.7 ct/kWh · payback 7.7 yr · IRR 12.7 % |

---

## Files

| File | What it is |
|---|---|
| `*_Portfolio*.pdf` | The full study: inputs, hand calculations, PVsyst results, hand-vs-simulation check, economics, open points |
| `PV_Residential_Economics*.xlsx` | Formula-driven calculator: 25-year cash flow, payback, NPV, IRR, LCOE, sensitivity. Blue cells are inputs |
| `pv_house*.lsp` | AutoLISP script: draws house, obstacles and modules on the tilted roof in AutoCAD and writes a Collada scene for PVsyst near shading (commands `PVHOUSE`, `PVEXPORT`) |
| `scene_PV_*.dae` | The generated 3D shading scenes, ready for PVsyst import |

## Tools

Hand calculation · PVsyst 8.1 (evaluation licence) · AutoCAD + AutoLISP · Blender · Excel · Meteonorm 9.0

## Note

These are concept studies for learning and portfolio purposes, not designs for construction. Exact building locations are withheld. Prices are 2026 market averages, not quotes.
