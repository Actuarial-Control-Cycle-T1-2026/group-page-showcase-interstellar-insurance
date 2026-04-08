[![Review Assignment Due Date](https://classroom.github.com/assets/deadline-readme-button-22041afd0340ce965d47ae6ef1cefeee28c7c493a6346c4f15d667ab976d596c.svg)](https://classroom.github.com/a/FxAEmrI0)
# SOA Case Study: The Pricing Frontier

Team Members: Maggie Lam, Gemma Lapins-Silvirs, Pankti Patel, Dhruti Patel, Ksenia Sofronova

---

## Table of Contents
1. [Overview](#overview)
2. [Final Recommendation](#final-recommendation)
3. [Product Design](#product-design)
4. [Cost and Pricing Result](#cost-and-pricing-result)
5. [Risk Assessment](#risk-assessment)
6. [Assumptions](#assumptions)
7. [Data and Limitations](#data-and-limitations)
8. [References](#references)

### Overview


> We were tasked to design and evaluate cost models for Cosmic Quarry Mining Corporation. This space mining company operates across three distinct solar systems, and the project involves four key hazard areas to provide insurance for: equipment failure, cargo loss, workers’ compensation and business interruptions. Considering statistical measures, short and long-term ranges were provided for costs, returns and net revenue alongside recommended product designs tailored to each system's unique risk profiles and specific challenges, while also ensuring the solutions are adaptable to future uncertainty. 

---

### Final Recommendation

---

### Product Design


---

### Cost and Pricing Result
> **Note:** All modelling was calculated per-hazard, per-system, however the summary above details results on a per-hazard aggregate level.

All four portfolios demonstrate positive profitability, with varying levels of volatility:

- **Business Interruption** generates solid short and long-term net revenue of Đ1.91bn. Note the extremely wide returns range reflects significant volatility embedded in the cost model due to the heavy-tailed distribution.
- **Cargo Loss** produces the highest numbers in terms of cost and premiums, although net margins and net revenue remain strong at Đ2.25bn.
- **Equipment Failure** performs strongest, generating highest short-term revenue of Đ6.89bn.
- **Workers' Compensation** delivers the most stable results, with low but consistent revenue of Đ0.89bn. The tight confidence intervals are supported by diversification across a large workforce, resulting in moderate but reliable profitability.

---

### Risk Assessment 


---

### Assumptions 
Several key assumptions shaped the modelling process.

Economic and Projection Assumptions: Simplified and interpretable economic assumptions are applied for projection purposes. Inflation and interest rates are derived from historical data using smoothing and regression approaches where appropriate. Interpolated yield curves are used for longer horizons.

System mapping and exposure: New solar systems are modelled using proxy systems based on qualitative risk similarities. Exposure is also estimated and scaled in certain cases, assuming homogenous risk within each system. 

Modelling: Frequency-severity models are assumed for cost modelling across the four hazard areas. For frequency, Negative Binomial or Poisson distributions are used, while severity uses heavy-tailed distributions such as lognormal or spliced distribution models. Monte Carlo simulation approaches are used to estimate aggregate losses; or these 100,000 runs are assumed to be adequate. 

Pricing and Risk analysis: premiums are calculated using the standard deviation principle reflecting risk appetite, simplifying assumptions surrounding reinsurance and other expense costs. For stress testing and scenario analysis, deterministic shocks are applied to modelling assumptions to assess extreme outcomes without altering the underlying distribution. 

---

### Data and Limitations
Modelling was done using historical data, qualitative judgements from the Interstellar Mining History and industry benchmarking of numerical and qualitative results.

Historical data availability: Modelling relied heavily on historical data. Where unavailable, such as significant gaps in current exposure data, qualitative judgements and simplified assumptions were applied to create allocation assumptions. 

Data Quality: Missing, misaligned and outlier values were identified in provided datasets, requiring cleaning, truncating or other processes to maintain usability. 

Model limitations: Statistical significance of provided variables was limited, leading to simplified modelling that indicates need for improved data collection and underwriting reporting. 

---

### References


---

