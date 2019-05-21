##SKT_BigDataHub
##API호출

install.packages('httr')
install.packages("jsonlite")
library(httr)
library(jsonlite)
httpResponse <- GET(url = paste0("https://api.bigdatahub.co.kr/v1/datahub/datasets/search.json?pid=1000908",
                                 "&$count=25"),
                    add_headers(TDCAccessKey='**SKT BIG DATA HUB API KEY'),
                    accept_json())
getapi_data = fromJSON(content(httpResponse, "text"))
geocode_center <- getapi_data$entry[-1]
