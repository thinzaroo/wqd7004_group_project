# wqd7004_group_project

Step-by-step notes for WQD 7004 Group Project

1. Data Set selection
- Selected Area: Kuala Lumpur
- Residential Type: Condo/Serviced Apartments
- Category: Rental
- Sort type: recent
2. Compile list of districts in kuala lumpur
3. Compile list of areas in kuala lumpur
4. Web Scraping using “rvest” package (refer to web_scraping_script.R)
- https://www.youtube.com/watch?v=4IYfYx4yoAI
- Install Chrome extension: Selector Gadget
5. Error 429, server prevents direct web scraping, need to change strategy
6. Download html files manually, inspect the source, found JSON. So we scrapped the JSON from javascript tag.
7. Use R code to scrap data from JSON , convert to dataframe, and save to csv
8 So far finished for 12 areas (200x12~2400 data points scraped)
  1, Taman Tun Dr Ismail
  2, Mont Kiara
  3, KLCC
  4, Bangsar South
  5, Taman Desa
  6, Bukit Bintang
  7, Sri Hartamas
  8, Damansara Heights
  9, Bangsar
  10, Brickfields
  11, Seputeh (only 156 listings available for this area)
  12, Kuchai Lama
9. Need to merge the data frame
10. Pls keep the individual json files, it contains a lot more features, if we want, we can always get back from there. You can discard the html files after scraped.

Next:
- Merge datasets
- Change format, clean data, remove outliers
- Exploratory Data Analysis
- What questions are you trying to solve (or prove wrong)?
- What kind of data do you have and how do you treat different types?
- What’s missing from the data and how do you deal with it?
- Where are the outliers and why should you care about them?
- How can you add, change or remove features to get more out of your data?

Research Questions: 
https://docs.google.com/document/d/13hJNOODNH9XW55rJIhmNJZhbspSvF81k-gwodAnnh8A/edit?usp=sharing

