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

We were tasked to design and evaluate cost models for Cosmic Quarry Mining Corporation. This space mining company operates across three distinct solar systems, and the project involves four key hazard areas to provide insurance for: equipment failure, cargo loss, workers’ compensation and business interruptions. Considering statistical measures, short and long-term ranges were provided for costs, returns and net revenue alongside recommended product designs tailored to each system's unique risk profiles and specific challenges, while also ensuring the solutions are adaptable to future uncertainty. 

---

### Final Recommendation

Overall, three out of the four hazard areas were proposed as viable risks to insure. Equipment failure and workers’ compensation products were supported fully, both lines of business showing strong and consistent profitability. Capped coverage and targeted exclusions were recommended in terms of business interruption insurance to manage the extreme tail-risk exhibited historically. Cargo loss is not recommended at present due to high severity risk and disproportionate premium requirements, making self-insurance a more viable interim strategy until risk profiles lessen.  

---

### Product Design


---

### Cost and Pricing Result
> **Note:** All modelling was calculated per-hazard, per-system, however the summary above details results on a per-hazard aggregate level.

| System | Horizon | Cost (Đ billions) | Return (Đ billions) | Net Revenue (Đ billions) | Loss Ratio |
|---|---|---|---|---|---|
| Business Interruption | ST | 2.74 (2.46 – 3.02) | 4.66 (1.92 – 33.04) | 1.91 (1.64 – 2.20) | 58.92% |
| Business Interruption | LT | 3.19 (2.86 – 3.51) | 5.51 (2.22 – 38.46) | 2.32 (1.90 – 2.55) | 58.92% |
| Cargo Loss | ST | 8.70 (7.29 – 10.33) | 10.95 (9.18 – 13.04) | 2.25 (1.87 – 2.68) | 79.45% |
| Cargo Loss | LT | 10.14 (8.50 – 12.04) | 12.76 (10.70 – 15.20) | 2.62 (2.18 – 3.12) | 79.46% |
| Equipment Failure | ST | 25.91 (24.61 – 27.20) | 32.80 (31.16 – 34.44) | 6.89 (6.55 – 7.24) | 79.0% |
| Equipment Failure | LT | 29.10 (27.65 – 30.56) | 50.89 (48.35 – 53.33) | 21.79 (20.70 – 22.88) | 57.2% |
| Workers Compensation | ST | 3.94 (3.42 – 4.49) | 4.83 (4.22 – 5.49) | 0.89 (0.80 – 1.00) | 84.1% |
| Workers Compensation | LT | 4.99 (4.34 – 5.68) | 6.41 (5.64 – 7.24) | 1.42 (1.30 – 1.55) | 80.1% |

All four portfolios demonstrate positive profitability, with varying levels of volatility:

- **Business Interruption** generates solid short and long-term net revenue of Đ1.91bn. Note the extremely wide returns range reflects significant volatility embedded in the cost model due to the heavy-tailed distribution.
- **Cargo Loss** produces the highest numbers in terms of cost and premiums, although net margins and net revenue remain strong at Đ2.25bn.
- **Equipment Failure** performs strongest, generating highest short-term revenue of Đ6.89bn.
- **Workers' Compensation** delivers the most stable results, with low but consistent revenue of Đ0.89bn. The tight confidence intervals are supported by diversification across a large workforce, resulting in moderate but reliable profitability.

---

### Risk Assessment 

Risk analysis was conducted across all solar systems: Helionis Cluster, Bayesia System and Oryn Delta. Each presented distinct and interrelated challenges across hazard areas. Key drivers included radiation exposure, orbital debris, geological instability and communication disruptions. 

Overall, Helionis Cluster was found to be the most stable, followed by the Bayesia System, which presented moderate risk driven by environmental volatility, and finally Oryn Delta carried the highest risk due to harsh physical conditions and frontier expansion operations bringing general uncertainty. 

Scenario analysis shows strong profitability under best and moderate conditions across all products, but in worst case scenarios, significant tail risk emerges, partially for cargo loss. This highlights the need for further risk mitigation strategies, including reinsurance products, and close monitoring of data. 


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

> Gallagher. (2024). Marine and Cargo Insurance - Understand the Essential Protections. Gallagher. https://www.ajg.com/au/news-and-insights/marine-and-cargo-insurance-understand-the- essential-protections/ 
icare NSW Calculating the Cost of Your Workers Compensation Premium. Insurance and Care NSW. https://www.icare.nsw.gov.au/employers/premiums/calculating-the-cost-of-your-premium 
Lynch, R. (2025). Freight Insurance in Australia: What It Covers & How Much It Costs. Couriers and Freight. https://www.couriersandfreight.com.au/blog/freight-insurance-in-australia 

> Mata, A. (2025) Actuarial Pricing Models and Methods: The Complete Guide. MatBlas. https://matblas.com/actuarial-pricing-models-and-methods-the-complete-guide/

> Resources Safety and Health Queensland. (2010, November 10). Severe weather preparedness. Retrieved March 2026, from Resources Safety and Health Queensland: https://www.rshq.qld.gov.au/safety-notices/mines/severe-weather-preparedness#:~:text=Severe%20weather%20event%20warning%20and,to%20a%20place%20of%20safety

> Simplistics (QuantPsych) (2021) Understanding Generalized Linear Models (Logistic, Poisson, etc.). https://youtu.be/SqN-qlQOM5A

> World Bank Group. (2018). Developing Parametric Insurance for Weather Related Risks for Indonesia. World Bank Group.

> Yadav, M., & Roychoudhury, B. (2018). Handling missing values: A study of popular imputation packages in R. Science Direct. https://www.sciencedirect.com/science/article/abs/pii/S0950705118303381?via%3Dihub 

> Zak, S. (2025). Data Mapping Between Systems: Keys, Values, Relationships (with P&C Insurance Examples). RecordLiner. https://recordlinker.com/data-mapping-guide/ 


---

