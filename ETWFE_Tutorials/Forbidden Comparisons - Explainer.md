# Forbidden Comparisons — an intuitive explainer

*A companion note to the ETWFE work. It explains, in plain language and with
worked numbers, why the ordinary "two-way fixed effects" (TWFE) way of
estimating an average treatment effect can go wrong when different units are
treated at different times — and why the Extended TWFE (ETWFE) fixes it.*

---

## 1. The setting: staggered treatment

Imagine we want the effect of signing a Free Trade Agreement (FTA) on trade.
Different country-pairs sign in different years. Call the year a pair first
signs its **cohort**. A pair that never signs is a **never-treated** control.

Here is a tiny world: three periods, one early cohort (**E**, signs in period
2), one late cohort (**L**, signs in period 3), and a never-treated control
(**N**). A `.` means untreated, an `X` means treated:

```
            period 1    period 2    period 3
   E  .           X           X
   L  .           .           X
   N  .           .           .
```

Difference-in-differences (DiD) asks: how much did a treated group's outcome
move **relative to a comparison group that did not change treatment status**?

---

## 2. The one-sentence definition

> A **forbidden comparison** is a difference-in-differences in which the
> "control" group is a unit that has **already been treated**.

The whole idea of a control group is that it tells you what *would have
happened without treatment* (the counterfactual trend). An already-treated unit
does **not** do that — its outcome is still moving *because of its own
treatment*. Using it as a control contaminates the answer.

---

## 3. The building block: a single 2×2 DiD

Every TWFE regression, under the hood, is a **weighted average of little 2×2
DiDs** (Goodman-Bacon 2021). With staggered timing there are three kinds:

| Comparison | Treated group | Control group | Verdict |
|---|---|---|---|
| **(a)** treated vs never-treated | E or L | N | ✅ clean |
| **(b)** early vs late, *before* late is treated | E | L (not yet treated) | ✅ clean (a "not-yet-treated" control is fine) |
| **(c)** late vs early, *after* early is treated | L | E (**already treated**) | ⛔ **forbidden** |

Comparison **(c)** is the troublemaker. Let's see exactly what it computes.

### The master formula

Write the untreated trend with time effects `λ₁, λ₂, λ₃` (common to everyone).
Let the early cohort E's treatment effect be `a₂` in period 2 and `a₃` in
period 3, and let the late cohort L's period-3 effect be `b`.

The forbidden 2×2 (c) compares L's change from period 2→3 against E's change
from period 2→3:

```
  DiD(c) = [ L₃ − L₂ ]            −  [ E₃ − E₂ ]
         = [ (λ₃ + b) − λ₂ ]      −  [ (λ₃ + a₃) − (λ₂ + a₂) ]
         =  b                     −  (a₃ − a₂)
```

**So the forbidden comparison does not give `b` (the effect we want). It gives:**

```
        DiD(c) = b − (a₃ − a₂)
                     └────────┘
             the CHANGE in the early cohort's effect
             over the comparison window = the bias
```

This one line is the key to everything below. The bias is **minus the change in
the already-treated control's own effect**. Hold onto it.

---

## 4. Case A — effects that are heterogeneous **OVER TIME** (dynamics)

This is the dangerous case. Suppose an FTA's effect **grows** as it beds in
(tariffs phase out, supply chains adjust). Concretely:

- Common time trend: `λ₁ = 0, λ₂ = 1, λ₃ = 2`
- Early cohort E's effect grows: `a₂ = 1`, then `a₃ = 3`
- Late cohort L's genuine period-3 effect: `b = 1` (positive!)

Potential outcomes (log trade):

| | period 1 | period 2 | period 3 |
|---|---:|---:|---:|
| **E** | 0 | 1 + 1 = **2** | 2 + 3 = **5** |
| **L** | 0 | **1** | 2 + 1 = **3** |
| **N** | 0 | **1** | **2** |

**The clean way** (compare L to never-treated N):
```
  DiD(clean) = [L₃ − L₂] − [N₃ − N₂] = (3 − 1) − (2 − 1) = 2 − 1 = +1   ✅ correct
```

**The forbidden way** (compare L to already-treated E):
```
  DiD(c) = [L₃ − L₂] − [E₃ − E₂] = (3 − 1) − (5 − 2) = 2 − 3 = −1   ⛔ wrong SIGN
```

Check against the master formula: `b − (a₃ − a₂) = 1 − (3 − 1) = 1 − 2 = −1`. ✔

**What happened in words:** between periods 2 and 3, E's outcome rose by 3. Only
1 of that was the common time trend; the other 2 was E's *own effect growing*
from 1 to 3. The forbidden comparison blames the whole rise of 3 on "time," so
it over-subtracts and turns L's true `+1` into `−1`. A positive effect is
reported as negative — purely an artefact of using an already-treated control
whose effect was still moving.

> **Intuition:** you can't use a runner who is *still accelerating* as your
> "standing still" baseline.

---

## 5. Case B — effects that are heterogeneous **ACROSS COHORTS**

Now suppose each cohort's effect is **constant over time**, but **different
cohorts have different effects**. Say E's effect is `a = 3` (a deep agreement)
and L's is `b = 1` (a shallow one), both flat:

| | period 1 | period 2 | period 3 |
|---|---:|---:|---:|
| **E** | 0 | 1 + 3 = **4** | 2 + 3 = **5** |
| **L** | 0 | **1** | 2 + 1 = **3** |

First, notice the forbidden comparison is now **not biased**, because a constant
effect differences out (`a₃ − a₂ = 3 − 3 = 0`):
```
  DiD(c) = b − (a₃ − a₂) = 1 − 0 = 1   ✅ (recovers L's effect)
```
So with *constant* effects, each individual 2×2 is fine. The problem is subtler
and lives in **how TWFE averages the pieces**.

TWFE does **not** report the simple average of the cohort effects. It reports a
**variance-weighted** average (Goodman-Bacon weights), which mechanically
**over-weights cohorts treated in the middle of the panel** (they have the most
"treatment variance") and **under-weights** cohorts treated early or late.

- The number you *want* (equal weight on the two cohorts): `(3 + 1) / 2 = 2`.
- What TWFE might report, if the timing gives weights `s_E = 0.75, s_L = 0.25`:
  `0.75·3 + 0.25·1 = 2.5`.

Same data, same (correct) building blocks, but the headline number is **2.5 vs
the intended 2** — it silently over-represents the big-effect cohort. In richer
designs some of these weights become **negative** (Section 6), and then the
single number can even fall *outside* the range of every true cohort effect.

> **Intuition:** even when each ingredient is measured correctly, TWFE mixes
> them with a recipe you didn't choose — so the "average effect" is not the
> average you meant.

---

## 6. The mathematical foundation

Two results make this precise; both say the TWFE coefficient is a weighted sum
of the true cohort-by-time effects, but with **weights you didn't pick**.

**(i) de Chaisemartin & D'Haultfœuille (2020).** The TWFE estimand is
```
        β^TWFE  =  Σ (g,t) : treated   w_{gt} · ATT_{gt} ,     with   Σ w_{gt} = 1 ,
```
but **some of the weights `w_{gt}` can be negative.** Because it is not a
*convex* combination (weights don't all lie in [0,1]), `β^TWFE` can be negative
even when **every** `ATT_{gt} > 0`. The negative weights land on cohort-periods
that are used as controls in forbidden comparisons.

**(ii) Goodman-Bacon (2021).** The same coefficient equals a weighted average of
all possible 2×2 DiDs,
```
        β^TWFE  =  Σ_b  s_b · DiD_b ,
```
where the weights `s_b ≥ 0` are proportional to each pair's group size and
**treatment-timing variance** (hence the "middle cohorts count most" effect of
Case B), and the set of `DiD_b` **includes the forbidden "already-treated-as-
control" comparisons** of Case A. The bias of each such term is exactly the
`−(a₃ − a₂)` we derived in Section 3.

Putting the two cases together: **over-time heterogeneity** makes the forbidden
2×2s themselves *wrong* (Case A); **across-cohort heterogeneity** makes the
*weights* misrepresentative and, with negative weights, non-convex (Case B).
Either way the single TWFE number is not the average effect you wanted.

---

## 7. The cure — the ETWFE / cohort-specific baseline

The fix is to **never let a treated unit act as a control.** In the ETWFE
(Wooldridge 2021, 2023; Nagengast & Yotov 2023) we:

1. Give **every treated cohort-year its own dummy** `δ_gs` (a saturated set of
   cohort × year terms). Each cohort is then compared only against its **own
   pre-treatment periods** and the **never-treated** pairs — the clean
   comparisons (a) and (b), never the forbidden (c).
2. Estimate all the `δ_gs` cleanly, then **aggregate them ourselves** with
   transparent weights (e.g. each cell's share of treated observations,
   `N_gs / N_D`) — so we choose the recipe, not the regression.

In the code (`Run 2 …ETWFE….R`, and the step-by-step tutorial) this is the block
that builds `treat_cohort_year`: post-treatment cells get their own
`g<cohort>_y<year>` label, while never-treated pairs **and every cohort's own
pre-treatment years** share the single `0_baseline` reference. That baseline
*is* the cohort-specific counterfactual.

### Back to our real result (sector `b_t`, Beverages & Tobacco)

| Estimate | FTA effect |
|---|---:|
| Naive dummy, full sample (Run 1) | **+0.29** *(and "significant")* |
| Naive dummy, always-treated pairs removed | −0.10 |
| **ETWFE** (cohort-specific baselines) | **−0.19** |

The naive method reports a large positive, significant FTA effect. But it is
leaning on forbidden comparisons — including long-standing agreements (like the
EU) that were *always treated* in our window and have no clean "before." Once
each cohort is measured against its own clean baseline, the estimated effect for
this sector is actually **negative**. That is not a bug; it is the naive method's
bias being removed — exactly the pattern this whole literature warns about.

---

### One-line summary

**A control group has one job — to show the no-treatment trend. An already-
treated unit can't do that job, because it is still responding to its own
treatment. TWFE hires it anyway (Case A), and mixes everyone with the wrong
weights (Case B). ETWFE only ever uses clean controls, and lets you choose the
weights.**

*References: Goodman-Bacon (2021); de Chaisemartin & D'Haultfœuille (2020);
Callaway & Sant'Anna (2021); Wooldridge (2021, 2023); Nagengast & Yotov (2023).*
