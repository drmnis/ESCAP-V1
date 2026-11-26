# ESCAP: builds the example data files in example_data/ (run from the ESCAP folder).
# Files 01, 02 and 09-11 are taken from the author's own data shipped with ESCAP (www/);
# the others are simulated with realistic food-science values (fixed seed, reproducible).
import shutil, numpy as np, pandas as pd
from pathlib import Path

rng = np.random.default_rng(2026)
www = Path("www"); out = Path("example_data"); out.mkdir(exist_ok=True)
r2 = lambda x, d=2: np.round(x, d)

# ---- 01, 02: from the author's storage experiment (www/example_data.csv)
ex = pd.read_csv(www / "example_data.csv")
ex = ex.rename(columns={"condition": "Packaging", "min": "Storage (day)", "treatment": "Treatment",
                        "Flavonoid": "Flavonoid (mg/g)", "TSS": "TSS (°Brix)", "TA": "TA (%)",
                        "Reducing sugar": "Reducing sugar (%)", "L": "L*", "a": "a*", "b": "b*"})
for c in ["L*", "a*", "b*"]: ex[c] = r2(ex[c])
ex["Replicate"] = ex.groupby(["Packaging", "Treatment"]).cumcount() + 1
one = ex[ex["Packaging"] == "P"].drop(columns=["Packaging", "Storage (day)"])
one = one.rename(columns={"Replicate": "Block"})
one = one[["Treatment", "Block"] + [c for c in one.columns if c not in ("Treatment", "Block")]]
one.to_csv(out / "01_one_way_anova_RCBD.csv", index=False)
ex.drop(columns=["Storage (day)"]).to_csv(out / "02_two_way_anova_packaging_x_treatment.csv", index=False)

# ---- 03 ANCOVA: edible coatings on tomato, initial fruit weight as covariate
rows = []
eff = {"Uncoated": 0.0, "Chitosan 1%": -1.6, "Aloe vera gel": -1.1, "Chitosan + Aloe": -2.3}
for t, e in eff.items():
    for r in range(1, 9):
        w0 = rng.normal(118, 14)
        loss = 7.8 + e + 0.035 * (w0 - 118) + rng.normal(0, 0.45)
        firm = 32 - 1.8 * e + rng.normal(0, 1.6)
        rows.append([t, r, r2(w0, 1), r2(loss), r2(firm, 1)])
pd.DataFrame(rows, columns=["Coating", "Fruit no.", "Initial weight (g)", "Weight loss (%)", "Firmness (N)"]) \
  .to_csv(out / "03_ancova_coating_weight_loss.csv", index=False)

# ---- 04 independent t-test: two drying methods
rows = []
for m, (mo, vc, aw) in {"Hot-air drying": (11.8, 41.0, 0.52), "Freeze drying": (9.6, 58.5, 0.41)}.items():
    for r in range(1, 9):
        rows.append([m, r, r2(rng.normal(mo, 0.7)), r2(rng.normal(vc, 3.5), 1), r2(rng.normal(aw, 0.03), 3)])
pd.DataFrame(rows, columns=["Drying method", "Batch", "Moisture (% wb)", "Vitamin C (mg/100 g)", "Water activity"]) \
  .to_csv(out / "04_t_test_independent_drying.csv", index=False)

# ---- 05 paired t-test: panel scores before and after training
rows = []
for p in range(1, 13):
    base = rng.normal(5.4, 0.9)
    rows += [[f"P{p:02d}", "Before training", r2(base, 1)], [f"P{p:02d}", "After training", r2(base + rng.normal(0.9, 0.5), 1)]]
pd.DataFrame(rows, columns=["Panelist", "Session", "Discrimination score"]).to_csv(out / "05_t_test_paired_panel.csv", index=False)

# ---- 06 Kruskal-Wallis: 9-point hedonic scores (ordinal)
rows = []
means = {"F1 (0% moringa)": (7.4, 7.2, 7.0, 7.3), "F2 (2% moringa)": (7.1, 7.0, 7.1, 7.1),
         "F3 (4% moringa)": (6.2, 6.1, 6.8, 6.3), "F4 (6% moringa)": (5.1, 5.0, 6.4, 5.2)}
for f, m in means.items():
    for p in range(1, 31):
        rows.append([f, p] + [int(np.clip(np.rint(rng.normal(mu, 1.1)), 1, 9)) for mu in m])
pd.DataFrame(rows, columns=["Formulation", "Panelist", "Color", "Flavor", "Texture", "Overall acceptability"]) \
  .to_csv(out / "06_kruskal_hedonic_scores.csv", index=False)

# ---- 07 descriptive, correlation, regression: mango quality (40 fruits)
n = 40; ripe = rng.uniform(0, 1, n)
d = pd.DataFrame({
    "Sample": [f"M{i:02d}" for i in range(1, n + 1)],
    "Cultivar": rng.choice(["Amrapali", "Langra", "Fazli"], n),
    "TSS (°Brix)": r2(10 + 11 * ripe + rng.normal(0, 0.8, n), 1),
    "TA (%)": r2(1.4 - 1.1 * ripe + rng.normal(0, 0.08, n)),
    "pH": r2(3.1 + 1.4 * ripe + rng.normal(0, 0.12, n)),
    "Firmness (N)": r2(68 - 52 * ripe + rng.normal(0, 4, n), 1),
    "Vitamin C (mg/100 g)": r2(55 - 22 * ripe + rng.normal(0, 4, n), 1),
    "Total phenolics (mg GAE/100 g)": r2(95 - 30 * ripe + rng.normal(0, 7, n), 1),
    "L*": r2(62 + 8 * ripe + rng.normal(0, 2, n)),
    "a*": r2(-8 + 20 * ripe + rng.normal(0, 1.8, n)),
    "b*": r2(38 + 18 * ripe + rng.normal(0, 2.5, n)),
})
d.to_csv(out / "07_correlation_regression_mango_quality.csv", index=False)

# ---- 08 kinetics: ascorbic acid in juice, first order, Arrhenius Ea = 62 kJ/mol
R = 8.314; Ea = 62000; k25 = 0.021  # 1/day at 25 C
rows = []
for T in (25, 35, 45):
    k = k25 * np.exp(-Ea / R * (1 / (T + 273.15) - 1 / 298.15))
    for t in (0, 3, 6, 9, 12, 15, 20, 25, 30):
        for r in range(1, 4):
            rows.append([T, t, r, r2(48.5 * np.exp(-k * t) * (1 + rng.normal(0, 0.02)))])
pd.DataFrame(rows, columns=["Temperature (°C)", "Time (day)", "Replicate", "Ascorbic acid (mg/100 mL)"]) \
  .to_csv(out / "08_kinetics_ascorbic_acid.csv", index=False)

# ---- 09 chi-square / logistic: the author's prevalence data
shutil.copy(www / "chi_square_data.csv", out / "09_chi_square_prevalence.csv")
# ---- 10 NIR spectra (gasoline octane, public pls data set) for PCA / PLSR / PLS-DA
shutil.copy(www / "gasoline.csv", out / "10_NIR_spectra_octane_PCA_PLSR_PLSDA.csv")
# ---- 11 circular stacked bar plot (ID, group, values)
shutil.copy(www / "circular_stacked_bar_plot_data.csv", out / "11_circular_bar_plot.csv")

# ---- 12 DoE: face-centred-free rotatable CCD, ultrasound extraction (2 factors, 5 centre points)
a = np.sqrt(2)
coded = [(-1, -1), (1, -1), (-1, 1), (1, 1), (-a, 0), (a, 0), (0, -a), (0, a)] + [(0, 0)] * 5
rows = []
for i, (x1, x2) in enumerate(coded, 1):
    temp = 50 + 10 * x1; time = 30 + 10 * x2
    y = 18.2 + 2.1 * x1 + 1.4 * x2 - 0.9 * x1 * x2 - 1.8 * x1**2 - 1.1 * x2**2 + rng.normal(0, 0.35)
    tpc = 41 + 3.2 * x1 + 2.0 * x2 - 2.6 * x1**2 - 1.5 * x2**2 + rng.normal(0, 0.7)
    rows.append([i, r2(temp, 1), r2(time, 1), r2(y), r2(tpc, 1)])
pd.DataFrame(rows, columns=["Run", "Temperature (°C)", "Time (min)", "Yield (%)", "TPC (mg GAE/g)"]) \
  .to_csv(out / "12_doe_CCD_ultrasound_extraction.csv", index=False)

# ---- 13 storage time series (scatter, line, dual axis, contour)
rows = []
for trt, s in {"Control": 1.0, "SAEW wash": 0.7, "Ultrasound + SAEW": 0.5}.items():
    for day in (0, 3, 6, 9, 12, 15, 18, 21):
        for r in range(1, 4):
            rows.append([trt, day, r,
                         r2(62 - 0.55 * s * day + rng.normal(0, 0.8)),
                         r2(-12 + 0.9 * s * day + rng.normal(0, 0.6)),
                         r2(4.0 + 0.11 * s * day + rng.normal(0, 0.12)),
                         r2(1.2 + 0.28 * s * day + rng.normal(0, 0.15)),
                         r2(85 - 2.6 * s * day + rng.normal(0, 2.5), 1)])
pd.DataFrame(rows, columns=["Treatment", "Storage (day)", "Replicate", "L*", "a*", "Respiration (log)",
                            "Weight loss (%)", "Firmness (N)"]).to_csv(out / "13_storage_time_series.csv", index=False)

# ---- 14 sensory profile (spider plot): QDA means
attrs = ["Sweetness", "Sourness", "Bitterness", "Aroma", "Color", "Crispness", "Juiciness", "Aftertaste"]
prof = {"Product A": [7.2, 3.1, 1.2, 6.8, 7.5, 6.9, 7.0, 3.0], "Product B": [6.1, 4.5, 1.8, 6.0, 6.4, 7.8, 6.2, 3.6],
        "Product C": [5.0, 5.8, 2.9, 5.1, 5.6, 5.2, 5.0, 4.8], "Product D": [7.8, 2.4, 1.0, 7.4, 6.9, 6.1, 7.6, 2.5]}
rows = []
for p, m in prof.items():
    for r in range(1, 4): rows.append([p, r] + list(r2(np.clip(np.array(m) + rng.normal(0, 0.35, len(m)), 0, 10), 1)))
pd.DataFrame(rows, columns=["Product", "Session"] + attrs).to_csv(out / "14_sensory_profile_spider.csv", index=False)
print("written:", sorted(p.name for p in out.glob("*.csv")))
