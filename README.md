# US State Organic Ag

- Trends of USA Organic Agriculture trends on farm numbers, hectares, and sales over the years.
- panel data analysis?
- goal: predict farm number, area, and sales for next three census (2022, 2027, 2032)

### Content

#### Raw Data

**Preprocessing:** Data were copied from TXT and PDF files provided by the USDA Organic 
Production Surveys: https://www.nass.usda.gov/Surveys/Guide_to_NASS_Surveys/Organic_Production/index.php. Units were converted from acres to hectares.



**Variables:**
* `state`
* `year`
* `farm_number`: number of farms
* `farm_ha`: area of farms in hectares (ha)
* `farm_sales`: total sales

**Files**
- `1997_area.csv`
  - columns: state, year, farm_ha
  - source: Greene 2001
- `2007_2012_2017_number_area_sales.csv`
  - columns: year,state,farm_number,farm_ha,sales
  - source: NASS
- `2014_2015_farm_number.csv`  
  - columns: state, year, farm_number
  - source: USDA 2016
- `2016_number_area_sales.csv`  
  - columns: state,year,farm_number,farm_number_cv,farm_ha,farm_ha_cv,sales,sales_cv
  - source: USDA 2017
- `2019_number_area_sales.csv`  
  - columns: state,year,farm_number,farm_number_cv,farm_ha,farm_ha_cv,sales,sales_cv
  - source: USDA 2017

**Notes:**
> "The 2014 and 2015 Organic Certifier Survey data is a tabulation of USDA-accredited organic certifiers’ acreage and livestock data received. 
The data underwent editing, summarization, and disclosure programming prior to publishing. The 2008, 2011, 2014, and 2015 organic producer surveys as well as the 2007 and 2012 Census of Agriculture collected and published data on organic operations which had production in the reference year. 
The data from these programs were adjusted for non-response, misclassification, and coverage. Therefore, the certifier survey data and producer survey/census data are not comparable and data users should account for these differences whenreviewing the data. 
Data users should allow for differences when comparing the data between datasets including reference periods, organic definitions, and differing survey methodologies." - USDA 2016

* `[variable]_cv`: defined as "statistical precision estimates for the number of farms and acres and the total value of sales for the United States and for each state." collected for 2016, 2019; also available for [2014](https://www.nass.usda.gov/Publications/AgCensus/2012/Online_Resources/Organics/ORGANICS.txt), [2015](https://downloads.usda.library.cornell.edu/usda-esmis/files/zg64tk92g/pr76f6075/hx11xj08s/OrganicProduction-09-15-2016.txt)

**Sources:**

* Greene, Catherine R. US organic farming emerges in the 1990s: adoption of certified systems. No. 1474-2016-120887. 2001.
* USDA (2012) "2011 Certified Organic Production Survey" [TXT] (https://downloads.usda.library.cornell.edu/usda-esmis/files/zg64tk92g/8623j1717/4b29b896g/OrganicProduction-10-04-2012.txt)
* USDA (2016) "2014 and 2015 Organic Certifier Data" [pdf](https://www.nass.usda.gov/Surveys/Guide_to_NASS_Surveys/Organic_Production/Organic_Certifiers/2016/USDA_Accredited_Certifying_Agent_Certified_Organic_Data_2014_2015.pdf). Raw data from [2014](https://www.nass.usda.gov/Publications/AgCensus/2012/Online_Resources/Organics/ORGANICS.txt), [2015](https://downloads.usda.library.cornell.edu/usda-esmis/files/zg64tk92g/pr76f6075/hx11xj08s/OrganicProduction-09-15-2016.txt)
* USDA (2017) "2016 Certified Organic Survey" [TXT] https://downloads.usda.library.cornell.edu/usda-esmis/files/zg64tk92g/70795b52w/6q182n781/OrganicProduction-09-20-2017_correction.txt
* USDA (2017) 2017 Census of Agriculture - State Data [PDF](https://www.nass.usda.gov/Publications/AgCensus/2017/Full_Report/Volume_1,_Chapter_2_US_State_Level/st99_2_0042_0042.pdf) (includes 2017 and 2012 data)
* USDA (2020) "2019 Certified Organic Survey" Volume 3 Special Studies Part 4. AC-17-SS-4: [TXT](https://www.nass.usda.gov/Publications/AgCensus/2017/Online_Resources/Organics/ORGANICS.txt)


#### Analysis Code

Found in `code/` folder.

* `merge_data.R`

### TODO

- Check model structure
- handle missing data 
  - option 1 (easier, likely sufficient?): estimate using percentage increase since last census? (2017)
  - option 2: missing data model?

### Abstract

Organic Agriculture in the United States: Increase Trends in Regenerative Land
Management strategies and challenges.

The need to reduce negative impacts of agriculture on the environment and the consumer
demands for food produced without synthetic chemical inputs have led to organic agriculture
production systems. The United States (US) commands about half of the market share of
organic produce worldwide and is also home to about half of the world's organic farmers, but
limited information is available on trends in land-use, on-farm practices, and challenges in
organic crop production systems. This study used 2007, 2012, and 2019 agriculture census data
from the United States Department of Agriculture (USDA) to investigate trends in the US
organic agriculture market, on-farm regenerative practices, and challenges. From 2007 to 2017, the
number of certified organic farms, land area, and sales value increased by 15, 35, and 214%,
respectively. The most common regenerative agriculture practice in certified organic crop
production systems is the use of green and animal manures. All regenerative practices
evaluated trended positive (1-25%) except organic mulches and composts use, which reduced
by 9%. The most significant primary challenges faced by certified organic farmers were related
to organic practices regulations and production costs with remarkably increasing trends (115
to 428%) for all investigated challenges from 2007 to 2017. The US certified organic production
has enormous future potentials in farm numbers, land area, market, and their increasing
contribution to the environmentally friendly regenerative practices. However, this sector
requires efforts to address the primary challenges facing the organic farmers, and this will
necessitate farm-level research and policy reviews with farmers involvement.

### Contact Info

Project Lead: Dr. Isaac Mpanga
Area Associate Agent
Commercial Horticulture and Small Acreage
2830 N Commonwealth Dr, Camp Verde, AZ 86322
email: mpangai@arizona.edu


Analysis: 
David LeBauer 
email: dlebauer@arizona.edu