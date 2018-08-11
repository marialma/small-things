# Urgent Care Access in the US: Part One


### Why I did this
[This KFF brief](https://www.kff.org/medicaid/issue-brief/a-look-at-rural-hospital-closures-and-implications-for-access-to-care/) highlights a big problem - closures of rural hospitals impacts a rural community's access to healthcare & emergency care. There have been [several](http://www.modernhealthcare.com/article/20180609/NEWS/180609929) [news](https://www.statnews.com/2018/01/08/medicaid-hospital-closures/) [stories](https://www.hrsa.gov/enews/past-issues/2017/october-19/hospitals-closing-increase.html) and such about this topic. However, I noticed that people haven't really quantified this. A quick pubmed search for "hospital closures" doesn't yield any recent, satisfying results. I thought I'd try to take a swing at this, and try to better understand what kind of effect hospital closures have had.



### Methods & Data Sources
[HIFLD has a dataset](https://hifld-geoplatform.opendata.arcgis.com/datasets/hospitals?geometry=-182.07%2C7.695%2C35.899%2C56.744&orderBy=TRAUMA&orderByAsc=false)  containing the locations of hospitals within the US. This dataset was generated from state departments and federal resources, and was last updated in May of 2017.

[Sheps Center published a list of rural hospital closures](http://www.shepscenter.unc.edu/programs-projects/rural-health/rural-hospital-closures/), though this data is likely to be incomplete. HIFLD's data also includes a column for the 'status' of a hospital being open or closed, but this list is also not complete.

I went through the HIFLD data, cross-referencing the Sheps Center's list, then added status information for every one I could find into the HIFLD list. This turned out to be around 1% of the HIFLD list (and also only the hospitals with acute care centers that have closed). It's about as complete as I can make it, without manually checking more.

A lot of inspiration was taken from this 2009 paper, [Access to Emergency Care in the US, from Carr et al](https://www.ncbi.nlm.nih.gov/pmc/articles/PMC2728684/), though I decided against using their method for estimating travel time.

I used the [Centers of Population](https://www.census.gov/geo/reference/centersofpop.html) for all the census tracts in the US. Block groups were ignored for two reasons: computational capacity, and being able to potentially use ACS survey data.

I used Python to take the list of population centers, and find the nearest hospital with an acute care center for each one. Second nearest hospitals were not taken into consideration. This took a long time.

Taking the Sheps Center closure data as the 'validated' data set, I then took communities that would have been affected by the closure, and calculated the next nearest hospital for those communities.

In order to map it, I ended up going back to R. It turns out, Dr. Kyle Walker has created an R package, [tidycensus](https://walkerke.github.io/tidycensus/index.html), which made it really easy to get tract feature geometry. The bonus of tidycensus is that, for additional analysis, I can grab other demographic information to merge into the dataset.

### Results

[Boxplot - comparison]

This box plot illustrates descriptive statistics for the communities served by hospitals that have closed. The median distance to the nearest hospital changed from being **10.3 km (6.4 mi)**  away to being **23.1 km (14.4 mi)** away.

(In case you're not satisfied with the 'naked eye' approach, I also estimated effect size using Cohen's *d*:  we get a **large effect size** of d = 0.97)

[Boxplot - distance change]

On average, hospital closures meant that each individual community had to travel an **additional 13.2 km (8.2 mi)** to get to the next nearest acute care center, with a quarter communities having to travel upwards of an additional **20 km (12.4 mi)** to get emergency care.


While some of the communities may also be served by clinics and outpatient centers, as well as long term care centers, the loss of emergency rooms still represents a significant impact on care. Previous studies, [like this one from Nicholl et al (2007)](https://www.ncbi.nlm.nih.gov/pubmed/17711952), have demonstrated that increases in distance to care are strongly associated with an increased risk of death.




### Next Week
Stay tuned for:
