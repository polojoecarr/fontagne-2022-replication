# Forbidden Comparisons — Extension Tutorial (review notes & further worked examples)

*This document reviews "Forbidden Comparisons - Explainer.md" **without changing
it**. Read the two side by side: every comment below is keyed to a section of
the original. It has three jobs:*

1. *certify what the original gets right (every calculation has been re-checked
   numerically);*
2. *flag a small number of places where the wording deserves sharpening or a
   caveat (Comments C1–C4);*
3. *extend the material with two new worked examples (E1–E2) that make the
   "weights" story concrete — including the one pathology the original
   describes but never actually shows: **negative weights**.*

*All numbers below were verified by running the regressions in R (the snippet
in Section 5 reproduces them in ~12 lines).*

---

## 1. Review verdict

The explainer is **accurate and well built**. In particular:

| Original claim | Re-checked | Result |
|---|---|---|
| Master formula `DiD(c) = b − (a₃ − a₂)` (Sec. 3) | algebra re-derived | ✅ correct |
| Case A clean DiD = **+1** (Sec. 4) | recomputed | ✅ correct |
| Case A forbidden DiD = **−1**, sign flip (Sec. 4) | recomputed | ✅ correct |
| Case B forbidden DiD = **+1**, unbiased when effects are flat (Sec. 5) | recomputed | ✅ correct |
| `b_t` table: +0.29 / −0.10 / −0.19 (Sec. 7) | matches the tutorial script's Section 5 output (+0.293 / −0.104 / −0.187) | ✅ consistent |

The pedagogical spine — *one* master formula (`bias = minus the change in the
already-treated control's own effect`) reused everywhere — is the right way to
teach this and should not be touched.

The comments below are refinements, not repairs.

---

## 2. Comments and corrections (keyed to the original's sections)

### C1 (Sections 5–6). Be precise about *which* weights can go negative

The original says (Sec. 5): *"In richer designs some of these weights become
negative (Section 6)"*. Two different weighting schemes are in play, and only
one of them can go negative:

* **Goodman-Bacon weights** (on the 2×2 DiDs, Sec. 6-ii) are **always ≥ 0**.
  The Bacon decomposition never has negative weights — its problem is that some
  of the (positively-weighted) 2×2s are themselves *contaminated* (the
  forbidden ones).
* **de Chaisemartin–D'Haultfœuille weights** (on the underlying cohort-time
  effects `ATT_gt`, Sec. 6-i) are the ones that **can be negative**.

These are two decompositions of the *same* coefficient, so no contradiction —
but when teaching it, keep the two ledgers separate: *"Bacon: good weights on
some bad comparisons. dCDH: bad weights on the good (true) effects."*

A second sharpening: **when effects are constant over time within each cohort**
(the original's Case B, with a balanced panel), the pooled TWFE coefficient is
a *convex* combination of the cohort effects. Wrong weights, yes — but it can
never leave the range `[1, 3]` in that example, and can never flip sign. The
sign-flip / out-of-range pathology **requires effects that move over time**
(Case A dynamics). So the honest slogan is:

> *Across-cohort heterogeneity alone ⇒ a mis-weighted but bounded average.*
> *Add over-time dynamics ⇒ all bets are off (zero, wrong sign, out of range).*

### C2 (Section 5). In the original's own 3-period example, TWFE actually gets 2.0

The original illustrates mis-weighting with hypothetical weights
(`s_E = 0.75, s_L = 0.25 → 2.5`), and is careful to say "might". Worth knowing:
if you actually run pooled TWFE on that exact Case B example (E, L and the
never-treated N), you get **exactly 2.0** — the equal-cohort-weight answer.
The tiny 3-period design happens to be too symmetric for the mis-weighting to
bite (see E1 for why: the design's weights are ½, 0, ½, and with E's effect
flat the zero weight is harmless).

The mis-weighting story is real, but it needs **longer panels, unequal cohort
sizes, or mid-panel timing** to show up. If you teach from the original, flag
that the 0.75/0.25 numbers are illustrative, not derived from the example on
the page.

### C3 (Section 6). The weights depend only on the *design*, not the outcomes

A useful fact the original leaves implicit: the dCDH weights are a function of
the **treatment layout alone** (who is treated when), not of the outcome data.
You can therefore compute them *before* running any outcome regression and
diagnose in advance how exposed your setting is to this problem (this is
exactly what the Stata command `twowayfeweights` does). E1 and E2 below compute
them by hand for our toy designs.

### C4 (Section 7). What the b_t result does — and does not — establish

The original closes: the ETWFE estimate for Beverages & Tobacco is **−0.19**,
and this "is the naive method's bias being removed". Agreed — but add one
caveat when presenting this result: removing forbidden comparisons removes
*that specific bias*, it does not make the estimate assumption-free. The −0.19
still rests on:

* **no anticipation** (pairs don't change behaviour just before signing), and
* **parallel trends** — which under PPML is stated in *ratio* form (treated and
  control trade would have *grown by the same factor* absent treatment), not in
  differences.

So the correct reading is: *"the positive naive effect does not survive clean
identification"* — not *"FTAs demonstrably reduce beverage/tobacco trade."*

---

## 3. Extension E1 — run the original's Case A through the actual regression

The original shows the forbidden 2×2 in isolation flips the sign (+1 → −1).
A natural student question: *"fine, but what does the pooled TWFE regression —
which mixes clean and forbidden pieces — actually report on those nine cells?"*

Answer, computed exactly (see Section 5 to verify): **β̂ = 1.0**.

The three true effects and the weights TWFE silently assigns them:

| Treated cell | True effect | TWFE (dCDH) weight | Contribution |
|---|---:|---:|---:|
| E, period 2 (short-run) | +1 | **0.5** | +0.5 |
| E, period 3 (long-run)  | +3 | **0.0** | 0 |
| L, period 3             | +1 | **0.5** | +0.5 |
| **TWFE reports** | | | **+1.0** |

The honest equal-weight average of the three effects is (1+3+1)/3 = **+1.67**.
TWFE reports **+1.0** — a 40% understatement — because it puts **zero weight on
the long-run effect**. Not a small weight: *zero*. The regression never looked
at the +3 at all.

> **Teaching point this adds to the original:** in the pooled regression the
> forbidden comparison doesn't always announce itself as a wrong sign. Often it
> shows up more quietly, as **weights that erase exactly the effects you care
> most about** (here, the long-run effect). The sign flip of Section 4 of the
> original is the loud version; this is the silent one.

## 4. Extension E2 — the missing demonstration: genuinely *negative* weights

The original *states* (Sec. 6) that weights can be negative and the coefficient
can be negative even when every true effect is positive — but never exhibits
it. Here is the smallest example, and it is directly relevant to our project.

Take the same two cohorts E and L, but **remove the never-treated group N**
(imagine a dataset containing only country-pairs that sign an FTA at some
point). The design's weights become:

| Treated cell | True effect | Weight | Contribution |
|---|---:|---:|---:|
| E, period 2 | +1 | **+1.0** | +1.0 |
| E, period 3 | +3 | **−0.5** | −1.5 |
| L, period 3 | +1 | **+0.5** | +0.5 |
| **TWFE reports** | | (weights sum to 1) | **0.0** |

**Every true effect is positive (+1, +3, +1), and pooled TWFE reports exactly
zero.** Push the long-run effect to +5 and TWFE reports **−1**: a fully-fledged
sign flip in the *pooled* regression, produced entirely by the −0.5 weight on
the early cohort's long-run cell — which is precisely the cell being (mis)used
as a control for the late cohort. This is the master formula's
`−(a₃ − a₂)` wearing its regression-weights costume.

Why this matters for our project: losing the never-treated group is not exotic.
It is the same disease as our **always-treated pairs** in `b_t` (EU-style
agreements with no observable "before") — designs short on clean controls push
the weights toward the pathological pattern. That is why Run 2 both *drops
always-treated pairs* and *keeps never-treated pairs in the baseline*.

## 5. Verify everything yourself (12 lines of R)

```r
library(fixest)

## Case A design: E treated from t2, L from t3, N never; lambda=(0,1,2)
dA <- data.frame(unit = rep(c("E","L","N"), each = 3),
                 time = rep(1:3, 3),
                 D    = c(0,1,1,  0,0,1,  0,0,0),
                 Y    = c(0,2,5,  0,1,3,  0,1,2))     # effects: 1, 3, 1
coef(feols(Y ~ D | unit + time, dA))["D"]     # 1.0   (E1: not 1.67)

dB <- dA; dB$Y <- c(0,4,5,  0,1,3,  0,1,2)            # Case B: flat effects 3 and 1
coef(feols(Y ~ D | unit + time, dB))["D"]     # 2.0   (C2: the "right" answer, by luck)

dC <- subset(dA, unit != "N")                          # E2: no never-treated group
coef(feols(Y ~ D | unit + time, dC))["D"]     # 0.0   (all true effects positive!)

dD <- dC; dD$Y[dD$unit == "E"] <- c(0, 2, 7)           # long-run effect now +5
coef(feols(Y ~ D | unit + time, dD))["D"]     # -1.0  (sign flip in the pooled model)
```

And the weights themselves (they come from the design only — comment C3):

```r
w <- resid(feols(D ~ 1 | unit + time, dA)); w <- w[dA$D==1]/sum(w[dA$D==1]); round(w,2)
#  0.5  0.0  0.5      (with N)
w <- resid(feols(D ~ 1 | unit + time, dC)); w <- w[dC$D==1]/sum(w[dC$D==1]); round(w,2)
#  1.0 -0.5  0.5      (without N: a genuinely negative weight)
```

## 6. One extra intuition worth keeping

The original's runner and recipe analogies are good. Here is one more that
lands well, for the medical-minded:

> You are testing a new drug. As your "placebo group" you use patients who took
> the drug **last month** and whose fever is *still coming down*. Compared to
> them, your new patients look like the drug does nothing — or makes them
> worse. Nothing is wrong with the drug; everything is wrong with the placebo
> group.

And a one-liner for E1's silent version:

> TWFE didn't mis-measure the long-run effect — **it never weighed it**.

---

## 7. Summary of this review

| # | Where | Type | One-line takeaway |
|---|---|---|---|
| C1 | Sec. 5–6 | sharpening | Bacon weights ≥ 0 (bad *comparisons*); dCDH weights can be < 0 (bad *weights*). Flat effects ⇒ bounded error; dynamics ⇒ anything goes. |
| C2 | Sec. 5 | correction-of-emphasis | On the page's own example TWFE = 2.0; the 0.75/0.25 mis-weighting needs richer designs. |
| C3 | Sec. 6 | addition | Weights are computable from the design alone — diagnose before you estimate. |
| C4 | Sec. 7 | caveat | ETWFE removes forbidden-comparison bias; it does not remove the need for no-anticipation + (ratio) parallel trends. |
| E1 | new | extension | Pooled TWFE on Case A gives 1.0 vs honest 1.67 — the long-run effect gets weight **zero**. |
| E2 | new | extension | Drop the never-treated group: weights (1, −0.5, +0.5); TWFE = 0 with all-positive effects, −1 with stronger dynamics. |

*Everything above was verified numerically with `fixest::feols`; the snippets in
Section 5 reproduce every number.*
