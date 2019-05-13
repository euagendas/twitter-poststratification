# Twitter Poststratification
This provides an R implementation for the different poststratification models described in the Web Conference (WWW) 2019 paper titled  [Demographic Inference and Representative Population Estimates from Multilingual Social Media Data](https://doi.org/10.1145/3308558.3313684).

## About
To correct for sampling biases in using social media data for downstream applications, this implementation provides interpretable multilevel regression methods to predict the population of a group (e.g., country or nuts3 region) using non-representative social media counts. The poststratification models were evaluated on publicly available ground-truth census data and  inferred joint population counts from Twitter using [M3-inference tool] (https://github.com/euagendas/m3inference)

Using the best performing poststratification model, we provide a code to compute the inclusion probability of an individual from a group with given demographics (e.g., age: 18-29, gender: Female) to be on a given social media platform. 


## Preprocessing
The Python notebooks are used to preprocess the dataset required for the debiasing code in R. They are: 
- nuts3_data_prep.ipynb : this preprocessing is when M3 inference distinguishes between organizational and non-organizational Twitter accounts
- nuts3_data_prep.orgs-as-humans.ipynb : this preprocessing is when M3 inference treats organizational accounts as humans/personal accounts 

## Debiasing models
The debiasing models using R syntax are:
#### N ∼ M is our base model that uses only the total population count from the census (N ) and Twitter (M).
formular <- 'census ~ twitter + (twitter+0|country)'

#### N ∼ \sum_g  M(g) uses gender marginal counts only (i.e., the total counts of males and females not broken down by ages).
formular <- 'census ~ gender_F + gender_M + (0+gender_F |country) + (0+gender_M |country)'

#### N ∼ \sum_a M(a) uses age marginal counts only.
formular <- 'census ~ age_0017 + age_1829 + age_3039 + age_4099 + (0+age_0017 |country) + (0+age_1829 |country) + (0+age_3039 |country) + (0+age_4099 |country)'

#### N ∼ \sum_{a,g} M(a, g) uses the joint histograms inferred from Twitter but only the total population values from the census.
formular <- 'census ~ tw_0017F + tw_1829F + tw_3039F + tw_4099F + tw_0017M + tw_1829M + tw_3039M + tw_4099M + (0+tw_0017F |country) + (0+tw_1829F |country) + (0+tw_3039F |country) + (0+tw_4099F |country) + (0+tw_0017M |country) + (0+tw_1829M |country) + (0+tw_3039M |country) + (0+tw_4099M |country) '

#### log N(a, g) ∼ log M(a, g) + a + g uses the joint histograms inferred from Twitter and the joint histograms from the census.
formular <- 'census ~ twitter + age+gender + (0+twitter |country) + (0+age+gender|country)'

## Evaluation and Cross validation
We evaluate the debiasing models using mean abssolute percentage error in the following leave-one-group cross-validation
settings: 
- leave one region out: debias_twitter_leave_one_region_CV.R
- leave one country out (i.e., leave out all regions from a given country) : debias_twitter_leave_one_country_CV.R
- and leave one stratum out (e.g.,leave out only females aged 18-29): debias_twitter_leave_one_stratum_CV.R

## Impact of treating organizational account as humans
For the leave-one-region out cross-validation, we provided two implementation: 
 - Treating organizational accounts as humans: debias_twitter_leave_one_region_CV_org_as_human.R
 - Ignoring organization account counts: debias_twitter_leave_one_region_CV.R
 
## Computing inclusion probability
TODO

## Citation
Please cite our [WWW 2019 paper](https://doi.org/10.1145/3308558.3313684) if you use these scripts in your project.

```
@inproceedings{wang2019demographic,
  title={Demographic Inference and Representative Population Estimates from Multilingual Social Media Data},
  author={Wang, Zijian and Hale, Scott A. and Adelani, David and Grabowicz, Przemyslaw A. and Hartmann, Timo and Fl{\"o"}ck, Fabian and Jurgens, David},
  booktitle={Proceedings of the 2019 World Wide Web Conference},
  year={2019},
  organization={ACM}
}
```

## More Questions

We use issues on this GitHub for all questions or suggestions.  For specific inqueries, please contact us as `hello@euagendas.org`.  Please note that we are unable to release or provide training data for this model due to legal restrictions.

## License

This source code is licensed under the GNU Affero General Public License, which allows for non-commercial re-use of this software.  For commercial inqueries, please contact us directly. Please see the LICENSE file in the root directory of this source tree for details.
