# Spatial, Climate & Physical Feature Extraction Specification (Tier 2)

## Overview

This specification documents the spatial, physical, and environmental feature extraction pipeline implemented in the congressional database (`congress.sqlite`). It covers:

1. **Physical & Thermal Energy Conversions**: Fuel quantity standardization into thermal energy (MMBtu) and carbon dioxide equivalent emissions ($\text{CO}_2$).
2. **Geospatial Point-in-Polygon & Zonal Extractions**: Assigning power plant coordinates to Congressional Districts across redistricting cycles (103rd–114th) and extracting PRISM raster weather metrics.
3. **Degree Day Calculations**: Population-weighted and polygon-averaged Heating Degree Days (HDD) and Cooling Degree Days (CDD).
4. **Inflation Adjustments**: Normalizing nominal expenditures and energy prices to constant real dollars using the Bureau of Labor Statistics Consumer Price Index (`bls_cpi`).

---

## 1. Physical Units & Thermal Conversions

### 1.1 Fuel Quantity to Thermal Energy (MMBtu)

EIA survey forms (Form EIA-759, EIA-906, EIA-920, and EIA-923) report consumption across heterogeneous physical units. In `eia_survey_gen` and `eia_generation`, physical units are converted into Million British Thermal Units (MMBtu):

$$\text{Fuel Heat Content (MMBtu)} = \text{Physical Quantity} \times \text{Heat Content Factor}$$

| Energy Source Code | Description | Physical Unit | Default Heat Content Factor |
| :--- | :--- | :--- | :--- |
| `BIT` | Bituminous Coal | Short Tons (2,000 lbs) | ~24.50 MMBtu / ton |
| `SUB` | Subbituminous Coal | Short Tons (2,000 lbs) | ~17.25 MMBtu / ton |
| `LIG` | Lignite Coal | Short Tons (2,000 lbs) | ~14.00 MMBtu / ton |
| `DFO` | Distillate Fuel Oil (#1, #2, #4 diesel) | Barrels (42 US gal) | 5.825 MMBtu / barrel |
| `RFO` | Residual Fuel Oil (#5, #6 heavy oil) | Barrels (42 US gal) | 6.287 MMBtu / barrel |
| `NG` | Natural Gas | Thousand Cubic Feet (Mcf) | 1.028 MMBtu / Mcf |
| `NUC` | Nuclear (Uranium fuel) | MWh equivalent | Zero direct combustion |
| `WAT` | Hydroelectric | MWh equivalent | Zero direct combustion |
| `WND` | Wind | MWh equivalent | Zero direct combustion |
| `SUN` | Solar (Photovoltaic / Thermal) | MWh equivalent | Zero direct combustion |

*Note:* In Form 923 and recent Form 906 surveys, facilities report direct `elec_fuel_consumption_mmbtus`, which takes precedence over calculated heat content.

---

### 1.2 Carbon Dioxide ($\text{CO}_2$) Emissions Factors

Combustion emissions are computed using fuel-specific carbon coefficients documented in EPA Federal Register guidelines and EIA technical notes, stored in SQLite tables `eia_fuel_factors` and `eia_fuel_factors_old`:

$$\text{CO}_2\text{ (Metric Tons)} = \frac{\text{Fuel Consumption (MMBtu)} \times k_{\text{CO}_2}}{1,000}$$

where $k_{\text{CO}_2}$ is the emissions coefficient in $\text{kg CO}_2 / \text{MMBtu}$, and 1 metric ton $= 1,000\text{ kg} = 2,204.62\text{ lbs}$.

| Fuel Code | Energy Source | $k_{\text{CO}_2}$ ($\text{kg CO}_2/\text{MMBtu}$) | $k_{\text{CH}_4}$ ($\text{g CH}_4/\text{MMBtu}$) | $k_{\text{N}_2\text{O}}$ ($\text{g N}_2\text{O}/\text{MMBtu}$) |
| :--- | :--- | :--- | :--- | :--- |
| `BIT` | Bituminous Coal | 93.30 | 11.0 | 1.6 |
| `SUB` | Subbituminous Coal | 97.17 | 11.0 | 1.6 |
| `LIG` | Lignite Coal | 97.72 | 11.0 | 1.6 |
| `DFO` | Distillate Fuel Oil | 73.16 | 3.0 | 0.6 |
| `RFO` | Residual Fuel Oil | 75.10 | 3.0 | 0.6 |
| `NG` | Natural Gas | 53.06 | 1.0 | 0.1 |
| `WDL` | Wood Waste / Black Liquor | 93.80 | 32.0 | 4.2 |

*Zero-emissions fuels:* Nuclear (`NUC`), Hydroelectric (`WAT`), Wind (`WND`), Solar (`SUN`), and Geothermal (`GEO`) have $k_{\text{CO}_2} = 0$.

---

## 2. Geospatial Feature Extraction

### 2.1 Power Plant Congressional District Assignment (`eia_plants_district`)

To track local energy economies over time, the pipeline maps 7,258 power plants with GPS coordinates in `eia_plants` to US Congressional Districts across decennial redistricting plans (103rd–114th Congresses):

1. **Boundary Geometries:** UCLA Congressional District shapefiles (`raw/cd-maps/districtShapes103` through `districtShapes113`) projected to NAD83 (`EPSG:4269`).
2. **Point-in-Polygon Overlay:** Implemented in [`analysis/eia-plants-district.R`](file:///Users/jrj/Projects/congress/analysis/eia-plants-district.R) using `sf::st_join()`.
3. **Boundary Edge Cases (Offshore & Coastal Facilities):**
   - Plants situated on coastal spits, offshore islands, or municipal harbor lines that fall outside polygon edges are identified (`is.na(DISTRICT)`).
   - Resolved using nearest neighbor distance search via `sf::st_nearest_feature()` constrained strictly within the plant's state (`STATENAME == state_name`).
   - Guarantees 100% resolution (0 missing districts) across all 87,096 plant-congress records.
4. **Redistricting Cycles:**
   - 103rd–107th Congresses: 1990 census plan.
   - 108th–112th Congresses: 2000 census plan.
   - 113th–114th Congresses: 2010 census plan (114th Congress uses 113th boundary definitions).

---

### 2.2 District-Level Generation & Emissions Synthesis (`eia_district_emissions`)

Annual congressional district energy generation and emissions are aggregated in [`analysis/eia-district-emissions.R`](file:///Users/jrj/Projects/congress/analysis/eia-district-emissions.R):

$$\text{Generation}_{c, d, y} = \sum_{p \in (c, d)} \text{Net Generation MWh}_{p, y}$$
$$\text{CO}_{2; c, d, y} = \sum_{p \in (c, d)} \text{CO}_{2}\text{ Metric Tons}_{p, y}$$

Observations cover 9,165 district-year combinations spanning 1993–2016.

---

### 2.3 PRISM Climate & Degree Day Extraction (`data/prism.R`)

The PRISM Climate Group (Oregon State University) produces daily gridded meteorological rasters of the contiguous US at 4km resolution.

1. **Extraction Geometry:** Congressional district polygons from `raw/cd-maps/`.
2. **Zonal Extraction:** [`data/prism.R`](file:///Users/jrj/Projects/congress/data/prism.R) applies `terra::extract()` with area-weighted polygon averaging:
   $$\bar{T}_{d, t} = \frac{1}{|A_d|} \int_{A_d} T(x, y, t) \, dA$$
3. **Degree Day Formulation:**
   - Mean daily temperature in Fahrenheit:
     $$T_F = \frac{9}{5} \bar{T}_C + 32$$
   - Base temperature threshold: $T_{\text{base}} = 65^\circ\text{F} \approx 18.33^\circ\text{C}$.
   - **Cooling Degree Days (CDD):**
     $$\text{CDD}_t = \max(0, T_{F, t} - 65)$$
   - **Heating Degree Days (HDD):**
     $$\text{HDD}_t = \max(0, 65 - T_{F, t})$$
   - Annual totals are accumulated across daily observations:
     $$\text{CDD}_{\text{annual}} = \sum_{t=1}^{365} \text{CDD}_t, \quad \text{HDD}_{\text{annual}} = \sum_{t=1}^{365} \text{HDD}_t$$

---

### 2.4 NOAA CPC Population-Weighted State Degree Days (`noaa_degree_days`)

State-level heating and cooling degree days are ingested directly from NOAA Climate Prediction Center via [`data/noaa.R`](file:///Users/jrj/Projects/congress/data/noaa.R):
- Table `noaa_degree_days`: 420,768 daily observations across 48 CONUS states (1993–2016).
- View `noaa_state_degree_days`: 1,152 annual state-level aggregations.

---

## 3. Inflation Adjustments & Real-Dollar Deflators

### 3.1 Consumer Price Index (CPI-U) Series (`bls_cpi`)

Nominal monetary variables—including campaign contributions (`crp_pacs`), federal outlays (`cffr`), and retail energy prices (`eia_prices`, `eia_states`)—are deflated to constant baseline dollars using the Consumer Price Index for All Urban Consumers (CPI-U) series from `bls_cpi`:

$$\text{Real Dollars}_{\text{base}} = \text{Nominal Dollars}_t \times \left( \frac{\text{CPI}_{\text{base}}}{\text{CPI}_t} \right)$$

### 3.2 Reference CPI-U Values (1990–2016)

| Year | CPI-U (Annual Avg) | Deflator to 2010$ ($\frac{218.056}{\text{CPI}_t}$) | Deflator to 2016$ ($\frac{240.007}{\text{CPI}_t}$) |
| :--- | :--- | :--- | :--- |
| **1990** | 130.700 | 1.6684 | 1.8363 |
| **1992** | 140.300 | 1.5542 | 1.7107 |
| **1994** | 148.200 | 1.4714 | 1.6195 |
| **1996** | 156.900 | 1.3898 | 1.5297 |
| **1998** | 163.000 | 1.3378 | 1.4724 |
| **2000** | 172.200 | 1.2663 | 1.3938 |
| **2002** | 179.900 | 1.2121 | 1.3341 |
| **2004** | 188.900 | 1.1543 | 1.2705 |
| **2006** | 201.600 | 1.0816 | 1.1905 |
| **2008** | 215.303 | 1.0128 | 1.1147 |
| **2010** | **218.056** | **1.0000** | 1.1007 |
| **2012** | 229.594 | 0.9497 | 1.0454 |
| **2014** | 236.736 | 0.9211 | 1.0138 |
| **2016** | **240.007** | 0.9085 | **1.0000** |

### 3.3 Pipeline Applications
- In [`analysis/eia-states.R`](file:///Users/jrj/Projects/congress/analysis/eia-states.R), nominal fuel and electricity prices from `eia_prices` are deflated to real dollars per MMBtu before calculating state-level energy prices.
- In master panel synthesis (`analysis/panel.R`), district CFFR federal spending and PAC receipts are deflated to real constant dollars to ensure cross-temporal comparability across the 103rd through 114th Congresses.
