# US State Organic Ag

- Trends of USA Organic Agriculture trends on farm numbers, hectares, and sales over the years.
- panel data analysis?
- goal: predict farm number, area, and sales for next three census (2022, 2027, 2032)

### Content

#### Raw Data

- `raw_data/USA organic agriculture state data in farms_landarea_sales value.csv`
  - columns: year,state,farm_number,farm_ha,sales
  - source: NASS
- `raw_data/state_organic_area.csv`
  - columns: state, 1997, 2012, 2014, 2015, 2017
  - value: area (ha) under organic production
  - source: 
    - 1997 data: Greene 2001 
    - Other data: NASS
- `raw_data/us_organic_area.csv`  
  - columns: state, year, area
  - area in (ha)
  - source: 
    - 1992-1997 data: Greene 2001
    - Other data: NASS


Greene, Catherine R. US organic farming emerges in the 1990s: adoption of certified systems. No. 1474-2016-120887. 2001.

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