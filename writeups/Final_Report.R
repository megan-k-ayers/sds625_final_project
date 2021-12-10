
#' ---
#' title: "Exploring the relationship between climate attitudes and extreme weather events - S&DS 625 Final Project"
#' date: "`r Sys.Date()`"
#' author: "Megan Ayers"
#' output: pdf_document
#' ---

#' # Introduction
#' \subsection{Contextualizing and motivating}
#' Climate change and environmental degradation is one of the most existential
#' crises of our time, if not the most existential. Unfortunately, many
#' Americans do not agree with this statement and/or do not fully comprehend the
#' implications of our global future if we do not address the causes of
#' climate change. Without recognition of the seriousness of climate change,
#' harmful consumer habits perpetuate, eco-friendly policies falter due to lack
#' of support, and the industries and corporations that negatively impact the
#' environment are not held accountable nor pressured to abandon the status-quo.
#' Climate communication is a field that works with these issues, and in
#' particular, with how to educate and persuade individuals about climate
#' change and the necessity of acting to reduce it. The hope is that
#' the more people there are that recognize climate change as a serious threat,
#' the more likely we are to have politicians and corporations that are
#' pressured to use their positions of power to enact positive change when it
#' comes to the environment. 
#' 
#' This contextualizes my goal for this final project - to better understand
#' some aspects of climate change beliefs at the county-level. If we don't 
#' understand the beliefs that people currently have about climate change,
#' and what factors might influence those beliefs, then we will be less
#' effective at trying to persuade those people than if we can be understanding
#' and empathetic at the same time as being convincing. In particular I am
#' curious to see how local extreme weather events relate to county-level
#' climate change beliefs. I would expect that personally experiencing one of
#' the widely broadcasted affects of climate change - increased extreme weather
#' events such as drought, hurricanes, wildfires, winter storms, etc - would
#' cause a person to take climate change more seriously, compared to someone
#' who just hears about these things happening on the news. However, there are
#' other cultural and demographic factors that I would expect to have an effect
#' on climate change opinions that also need to be controlled for, such as
#' political leanings and age.
#' 
#' 
#' \subsection{Data sources}
#' To explore the relationship between extreme weather events and climate
#' change opinion with consideration given to the additional impact of
#' political leanings and demographics on climate change beliefs, I gathered
#' information from multiple data sources. 
#' 
#' To attempt to understand climate change beliefs, which I will consider to be
#' the response variable, I will use data collected and processed by the Yale
#' Program on Climate Change Communication. This data consists of US
#' county-level estimates of climate change beliefs, i.e. x% of the population
#' in New Haven County believes that climate change is happening. These
#' estimates are calculated from a large-scale nation-wide survey (n > 25,000)
#' conducted in spring of 2020, and were "derived from a statistical model using
#' multilevel regression with post-stratification (MRP)". Though using
#' individual-level data could be more convincing for arguing existence of a
#' relationship between extreme weather event experiences and climate change
#' beliefs (the unit "county" does not itself have a climate change belief),
#' actually obtaining the supplementary data at this level was not feasible for
#' this project due to data privacy issues. 
#' 
#' I pulled supplementary demographic data at the county level from the Census
#' Bureau's ACS 2019 estimates using the R package `tidycensus`. To understand
#' the political leaning of each county, I used 2020 general election results
#' at the county level. These came from a data set containing general election
#' results from 2000-2020 maintained by the MIT Election Data and Science Lab
#' and accessed via the Harvard Dataverse. 
#' 
#' Identifying counties that experienced an extreme weather event around the
#' time of the climate opinion survey is not cut-and-dried. How should one
#' define "extreme weather"? How long does the memory of an extreme weather
#' event impact behavior or beliefs? There may be multiple reasonable responses
#' to these questions. This report defines "extreme weather" as
#' weather events that were declared disasters by FEMA
#' (Federal Emergency Management Agency) in 2019. These events are recorded in
#' the online OpenFEMA Data Sets at the state and county level.
#' I chose to use FEMA disasters as the indicator of extreme weather because
#' it signals a disaster beyond what state governments typically expect and are
#' prepared to handle. This accounts for the fact that independently of climate
#' change, some parts of the country naturally experience - and are used to
#' experiencing - more severe weather than others. I limited the analysis to
#' considering weather events in 2019, which were most recent to when
#' the climate opinion survey was conducted. To use events from any previous
#' years would be to assume that large majorities of individuals stayed in the
#' same counties longer, and also that weather events from 2+ years prior would
#' have a lasting impact on beliefs, which I do not think are safe assumptions.
#' 


#' # Data Preparation
#' 
#' Merging these data sources required considerable cleaning and quality
#' assurance checks of each individual source.
#' 
#' \subsection{Climate change opinion survey data}
#' Because the providers of the climate change opinion survey already processed
#' the individual responses into county-level estimates, this data is very clean
#' to begin with. Data is available for every county reported on by the Census
#' in 2020, there are no duplicate rows, and the response variable `happening`
#' (indicating the proportion of county population that believes climate change
#' is happening) is relatively Normal in distribution.
#' 
#' \subsection{Election data}
#' This data set required a little more work to clean up. Data was reported for
#' one county that was not recognized as a by the Census, and was dropped. For
#' some reason, San Joaquin County CA had missing data, which I was able to 
#' manually find from the county website and replace. Votes were sometimes
#' recorded by the mode in which they were received (ex. "Early Vote", 
#' "Absentee", "Election Day" etc.) and sometimes in total, so aggregation to 
#' the total county level had to be standardized between counties. I was able
#' to rectify all differences in county names between this source and the 
#' Census Bureau (which I regarded as ground truth for standardizing all sources
#' to use FIPS codes for future merging) except for voting districts in Alaska.
#' I was unable to find a way to map voting districts to Census-reported
#' counties, so I dropped Alaska from the analysis. Although the other data
#' sources do have information on Alaska, I found political leaning to be such a
#' strong indicator of climate change belief that I think it is preferable to
#' drop it entirely from the analysis rather than introduce missing values for
#' political leaning.
#' 
#' \subsection{Census demographic data}
#' 
#' 
#' 
#' 
#' 
#' 
#' \section{Analysis}
#' 
#' \textcolor{red}{Remember to discuss ideas inspired by Causal Inference but
#' not actually valid in that framework}
#' Furthermore, it is difficult to guarantee that an
#' individual experienced an extreme weather event, even if they are recorded
#' as living in the county where one occurred, and "treatment" of experiencing
#' a weather event is not randomized.
#' 
#' \textcolor{red}{Try repeating analysis with just hurricanes, fires}
#' 
#' 


#' # Results
#' 


#' # Conclusion
#' 


#' # References
#' 
#' ### Contextualizing the problem of climate change and climate change communication
#' - https://www.theguardian.com/environment/ng-interactive/2021/oct/14/climate-change-happening-now-stats-graphs-maps-cop26
#' - https://climate-xchange.org/communicating-the-climate-crisis/
#' 
#' ### Data sources (\textcolor{red}{Use hyperlinks with text describing which data set this is})
#' - https://climatecommunication.yale.edu/
#' - https://climatecommunication.yale.edu/visualizations-data/ycom-us/
#' - MIT Election Data and Science Lab, 2018, "County Presidential Election Returns 2000-2020", https://doi.org/10.7910/DVN/VOQCHQ, Harvard Dataverse, V9, UNF:6:qSwUYo7FKxI6vd/3Xev2Ng== [fileUNF]
#' 
#' ### Analysis documentation
#' - https://kosukeimai.github.io/MatchIt/reference/method_optimal.html
#' - https://cran.r-project.org/web/packages/tidycensus/tidycensus.pdf
#' 


