# EMC_Sensitivity_Analysis

The sensitivity of a stormwater industry-defining water quality metric, the flow-weighted event mean concentration (EMC) to non-standardized calculation protocols is evaluated herein to improve confidence in the validity of these protocols and provide recommendations for minimizing bias where validation is not possible. EMCs may be generated by post-storm compositing of discrete (a.k.a. grab) samples or by collecting flow-weighted composite samples using automated equipment with integrated flow meters. Three methodological crossroads were assessed for their relative impact on the flow-weighted EMC value including the flow attribution (i.e., what volume of runoff corresponds to a given sample or sample aliquot), volume integration (i.e., the numerical method by which volume is calculated from flow), and flow-sample resolution (i.e., how frequently are flowrate and concentration observed). Field monitoring campaigns produced TSS pollutographs and hydrographs characterizing untreated runoff from a parking lot and an asphalt street, and treated runoff from a grassed swale system, a treatment wetland, and a permeable pavement were used to generate EMCs using each available calculation scheme. A combinatorial averaging method was adopted to quantify each scheme’s effect; the resultant EMC is compared against an adopted benchmark EMC. EMC outcomes from methodologies that use prior flow attribution (the volume of water preceding a sample is assumed to have concentration equal to that sample) underestimate the presumed highest accuracy benchmark EMC (representing trapezoidal volume integration, central flow-sample attribution, and highest available flow resolution) by 17.2% on average, regardless of the volume integration method or flow-sample resolution used. This is caused by the asymmetry of a typical urban runoff pollutograph, and typical under-sampling during the immediate stages of runoff when pollutant concentrations are most elevated. No other decision-variables demonstrated a meaningful effect, when quantified by the ratio of relative effect size to the standard deviation of the error. Composite sampling with autosamplers fits a prior flow-sample attribution scheme, but “Take First” and “Override” advanced program options can ameliorate the significant bias to better ensure the entirety of the hydrograph is sampled. Outcomes are meaningful for stormwater managers to reduce potential uncertainty when characterizing stormwater control measure (SCM) and collating SCM performance monitoring data across studies with differing sample collection methods. An open source web application is offered for any user to complete flow-weighted EMC calculations using the benchmark method.

Partially in response to the lack of guidance around EMC calculation from monitored data, a publicly available web application was developed using Rshiny (1.7.4). The application uses the benchmark flow attribution and integration methods to solve Equation 2 for the relative volumes associated with each sample, or the EMC from pollutograph data. The application, the flow-weighted compositing and event mean concentration calculator (FWC-EMC), can be found at https://sccwrp.shinyapps.io/FWC_EMC_Calculator/ with the source code available on GitHub (https://github.com/SCCWRP/FWCCalculator). Stormwater managers may use this tool to develop flow-weighted composites from collected grab-samples, or else to compute the EMC from known grab-sample (pollutograph) concentrations. 
