# Investigating County-level Theft Rates in the U.S. in 2020<br/>STAT  471 Final Project<br/>Mason Shihab and Diyang Chu<br/> December 19, 2021


We aim to generate insights as to features that might predict variation in the rates of thefts
committed per capita. In doing so, we hoped to understand if economic, health, criminal justice, or other
types of variables could be used to help with this analysis.

We collected data from a variety of mostly government sources, with a plurality coming from the
American Community survey in particular, and its parent organization, the US Census Bureau, in general.
Our response variable, "theft crimes known to law enforcement," was provided by the FBI. Other
variables were aggregated by nonprofit organizations such as The Marshall project and the Brookings
Institute. Also of note is the Police Scorecard Project. Comprehensive information about our data sources
is available in the Data section below.

We performed our analysis using two distinct supervised learning methodologies: penalized
regression as well as tree based methods. We use both of these tools to create predictive models. However,
rather than attempting to predict future thefts, we instead use these analyses to look backwards and
attempt to gain insights and associations with thefts committed in 2020. After reviewing our models to
gain insights, we compare them against each other as well as the intercept-only model via root mean
squared error metric so that the error rate can be on the same scale as the response variable.

Our models are difficult to draw any conclusions from because they are not significantly more
effective than the intercept-only model. Nevertheless, we do find some interesting associations using the
gradient boosting model. The prime takeaway here is that the rate of theft cannot be easily predicted, even
using metrics that would likely be seen as predictive, such as police accountability, poverty, and health
metrics. Indeed, the biggest conclusion may be that there is no satisfactory conclusion here. Current data
may not be sufficient for a proper national analysis, and future research may need to be focused on the
local level, with partnership from governments to provide complete data.