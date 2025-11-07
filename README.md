# UI Aerosol Hygroscopicity and Mixing State Package

**Authors:** Meng Zhou, Weizhi Deng, Jun Wang
**Initial release:** January 2020  

This package computes the hygroscopic growth, mixing state, and optical properties of aerosol populations under varying humidity and composition. It links aerosol microphysical properties (size distribution, composition, density, refractive index) to bulk optical quantities such as extinction, single-scattering albedo, asymmetry factor, and phase function.

The code is designed for applications in aerosol–radiation interaction studies, remote sensing, and climate / air-quality modeling.

---

## 1. Theoretical Background (Brief)

For a monodisperse aerosol population with particle radius \(r\), density \(\rho\), and mass concentration \(C\), the number concentration \(N\) is

\[
N = \frac{C}{\rho v} = \frac{3C}{4\pi r^3 \rho},
\]

where \(v = \frac{4}{3}\pi r^3\) is the particle volume.

The extinction coefficient is

\[
\beta_e = N \pi r^2 Q_{\mathrm{ext}},
\]

with \(Q_{\mathrm{ext}}\) the extinction efficiency, and the aerosol optical depth (AOD) along path \(s_1\)–\(s_2\) is

\[
\tau(s_1, s_2) = \int_{s_1}^{s_2} \beta_e(s)\, ds.
\]

For a polydisperse population with size distribution \(n(r)\), the extinction coefficient can be expressed as

\[
\beta_e = \frac{3 C \,\tilde{Q}_{\mathrm{ext}}}{4 \rho r_e},
\]

where

- \(r_e\) is the effective radius,
- \(\tilde{Q}_{\mathrm{ext}}\) is an area-weighted extinction efficiency.

The model uses these relationships, together with hygroscopic growth and mixing rules, to compute optical properties for externally and internally mixed sulfate, black carbon (BC), and dust.

---

## 2. Directory Structure

A typical directory layout for this package:

- `src/`  
  Uncompiled source codes and the main `Makefile` for building the executable.

- `run/`  
  Run directory containing:
  - `namelist.ini` (mandatory input file),
  - any auxiliary configuration needed for a specific experiment,
  - the main executable (e.g., `hygroscopic.exe`) copied here after compilation.

- `data/`  
  Ancillary data needed by the model (e.g., optical properties, lookup tables, etc.).

You may adjust the exact paths in `namelist.ini` to match your local file system.

---

## 3. Compilation

A full compilation is required the first time you obtain the codes (or after modifying source files).

From the `src/` directory:

```bash
cd src/
make
