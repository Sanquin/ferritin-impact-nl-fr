# Impact of Ferritin-Guided Donation Interval Policies in the Netherlands and France

**Collaborators:** EFS (Établissement Français du Sang) & Sanquin  
**Manuscript title:** Impact of ferritin-guided donation interval policies in the Netherlands and France

This repository contains the code and analysis for evaluating the impact of ferritin-guided donation interval policies on donor health and blood supply in the Netherlands and France.  


---

## Abstract

Many blood establishments have hemoglobin (Hb) measurement policies to prevent anemia in blood donors. However, despite evidence of iron deficiency (ID) in donors with normal Hb levels, only few blood establishments have implemented iron management strategies. Recently, the Dutch and French national blood services implemented ferritin-guided donation interval policies, albeit in different ways. While in the Netherlands ferritin is measured every 5 donations, in France ferritin measurements are performed in predefined risk groups. We compared rates of ID, low ferritin (15-30 ng/mL), and low Hb between the Netherlands and France before and after policy implementation. We also compared donor return rates and ferritin levels after ferritin-based deferrals. We found that before the policy change there were differences in rates of ID and low ferritin, but Hb deferral rates were very similar. After the policy change, more ferritin measurements were performed in the Netherlands, but both countries had similar ID rates (~4.5% and ~1% of measured females and males, respectively). Return rates within one year after the end of deferral for ID were similar in both countries (~60% for females and ~80% for males), but French donors had higher ferritin upon return despite the shorter deferral period. Because this is an observational study using retrospective data, and due to a lack of the standardization of ferritin measurements, comparisons need to be interpreted with caution. Nonetheless, the results offer valuable insights concerning the impact of ferritin-guided donation intervals for blood establishments that consider the implementation of similar policies.

## Repository layout and running order
Analyses are implemented as numbered R scripts. Scripts **1a** and **3a** combine results from steps **1** and **3** across both countries.

- 1_Ferritin_distribution.R: calculates the ferritin distribution of new and repeat donors. 
- 1a_combine_Ferritin_distribution.R: combines the results from the Netherlands and France.
- 2_deferral_rates.R: calculates the deferral rates for Hb, iron deficiency and low ferritin.
- 3_Hb_Fer_deferral_over_time.R: calculates the deferral rates by quarter of the year.
- 3a_combine_Hb_Fer_deferral_over_time.R: combines the results from the Netherlands and France.
- 4_Hb_levels_over_time.R: calculates the mean Hb levels per quarter of the year.
- 5_donor_return.R: calculates donor return within a year after the end of deferral and ferritin levels upon return.

