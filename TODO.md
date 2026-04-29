**Reminder: investigate simulation failure for religious_service → purpose cell**

To follow up on later — this looks like probably bad luck on this seed, but combined with structural factors that made it the most fragile of the eight cells. The list below sets out potential explanations, in rough order of importance, that should be verified rather than taken as established:

1. The estimate is roughly 3 SE below the truth (0.121 vs 0.042 with SE ≈ 0.024). That is a tail event but not an impossible one — about 0.1% under correct estimation. The other three religious outcomes all sit within 1 SE of their true ATEs, so the simplest reading is that this particular seed produced an unlucky AIPW realisation for one of eight cells.

2. Religious → purpose has the worst signal-to-noise of the eight pairs. The true ATE is small (0.12), the outcome residual standard deviation after baseline adjustment is ≈ 0.5, and the train/test split halves the effective sample for inference. Religious → self-esteem has an even smaller true ATE (0.08) but happened to overshoot rather than undershoot in this seed; that one passed by luck. The two were the obvious candidates for failure before anything was run.

3. Religious_service has the lowest prevalence of the three exposures (≈ 0.27 vs ≈ 0.55 for community and ≈ 0.45 for volunteer). Lower prevalence inflates the variance of AIPW weights through the 1/π̂(x) and 1/(1−π̂(x)) terms, especially in strata where propensity is near the boundary (high agreeableness × partnered × religious_t0 = 1). The forest's `tune.parameters = "all"` then leans toward more conservative splits when residual variance is high, which can attenuate estimated effects toward zero.

4. The CATE formula has a negative effect modifier (−0.04 × openness) that the forest doesn't pick up easily. Openness doesn't enter the propensity model and doesn't load on baseline purpose, so the forest has no nearby signal telling it to split on openness. This affects per-individual τ̂(x) more than the population mean, so it's a secondary contributor at most — but combined with item 3 it can produce the kind of "shrinkage toward zero" we observe.

**NOTE:** this is what real-data borderline cases look like. The simulator is doing the right thing by not always handing students a clean win. A student who picks religious_service → purpose has a chance to write a more interesting Discussion section about null findings, identification assumptions, and what they would need to do differently in real data.

**Cheap empirical check:** to verify the bad-luck-vs-systematic question, refit the religious cache at a few nearby seeds (e.g. 2027, 2028, 2029) and see how often this cell fails.
