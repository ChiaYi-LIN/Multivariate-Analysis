#%%
from selenium import webdriver
from itertools import combinations
import pandas as pd

#%%
def address_to_valid(place):
    if place == "Keelung & Changxing Intersection":
        place = "Youbike 基隆長興"
    elif place == "Wolong & Leye Intersection":
        place = "Youbike 臥龍樂業"
    elif place == "Roosevelt & Xinsheng S. Intersection":
        place = "Youbike 羅斯福新生"
    elif place == "JianGuo & Heping Intersection":
        place = "Youbike 建國和平"    
    return(place)

def valid_to_original_address(place):
    if place == "Youbike 基隆長興":
        place = "Keelung & Changxing Intersection"
    elif place == "Youbike 臥龍樂業":
        place = "Wolong & Leye Intersection"
    elif place == "Youbike 羅斯福新生":
        place = "Roosevelt & Xinsheng S. Intersection"
    elif place == "Youbike 建國和平":
        place = "JianGuo & Heping Intersection"
    return(place)

def scrap_distance_between_youbike_stations():
    browser = webdriver.Chrome()
    youbike_data = pd.read_csv("201601.txt", sep=" ")
    daan_data = youbike_data.drop_duplicates("rent_sta").loc[youbike_data["rent_sta_sarea"]==12]
    daan_all_stations = daan_data["rent_sta"].values
    results = []
    for each_combinatiion in combinations(daan_all_stations, 2):
        from_place = address_to_valid(each_combinatiion[0])
        to_place = address_to_valid(each_combinatiion[1])
        
        url = "https://www.google.com/maps/dir/" + from_place + "/" + to_place + "/data=!4m2!4m1!3e2"
        browser.get(url)
        try:
            browser.implicitly_wait(10)
            distance = browser.find_element_by_xpath('//div[@jstcache="451"]').text
        except:
            distance = "Error"

        from_place = valid_to_original_address(from_place)
        to_place = valid_to_original_address(to_place)
        print(from_place + " to " + to_place + ": " + distance)
        one_result = {
            "from station": from_place,
            "to station": to_place,
            "distance": distance
        }
        results.append(one_result)
    browser.quit()
    return(results)

def scrap_distance_between_youbike_and_metro_station():
    browser = webdriver.Chrome()
    youbike_data = pd.read_csv("201601.txt", sep=" ")
    daan_data = youbike_data.drop_duplicates("rent_sta").loc[youbike_data["rent_sta_sarea"]==12]
    daan_all_stations = daan_data["rent_sta"].values
    metro_data = pd.read_csv("taipei_metro_stations.csv", sep=",", encoding="cp950")
    metro_stations = metro_data["English Station Name"].values
    results = []
    
    for youbike_station in daan_all_stations[25:]:
        for metro_station in metro_stations:
        
            youbike_station = address_to_valid(youbike_station)

            url = "https://www.google.com/maps/dir/" + youbike_station + "/" + metro_station + "/data=!4m2!4m1!3e2"
            browser.get(url)
            try:
                browser.implicitly_wait(10)
                distance = browser.find_element_by_xpath('//div[@jstcache="451"]').text
            except:
                distance = "Error"

            youbike_station = valid_to_original_address(youbike_station)
            print(youbike_station + " to " + metro_station + ": " + distance)
            one_result = {
                "from station": youbike_station,
                "to station": metro_station,
                "distance": distance
            }
            results.append(one_result)
    browser.quit()
    return(results)

def bs4_youbike_metro():
    youbike_data = pd.read_csv("201601.txt", sep=" ")
    daan_data = youbike_data.drop_duplicates("rent_sta").loc[youbike_data["rent_sta_sarea"]==12]
    daan_all_stations = daan_data["rent_sta"].values
    metro_data = pd.read_csv("taipei_metro_stations.csv", sep=",", encoding="cp950")
    metro_stations = metro_data["English Station Name"].values
    # results = []
    for metro_station in metro_stations:
        for youbike_station in daan_all_stations:
            youbike_station = address_to_valid(youbike_station)

            url = "https://www.google.com/maps/dir/" + youbike_station + "/" + metro_station + "/data=!4m2!4m1!3e2"
            print(url)
            soup = BeautifulSoup(requests.get(url).text, "html.parser")
            # print(soup.getText())
            # distance = soup.find("div", {"jstcache" : "451"}).getText()
            # print(distance)
            return 0

#%%
# result_to_pd = pd.DataFrame(scrap_distance_between_youbike_stations())
# result_to_pd.to_csv("distance_between_each_station(Da'an).csv", sep=",", encoding="utf_8_sig")


#%%
result_to_pd = pd.DataFrame(scrap_distance_between_youbike_and_metro_station())
result_to_pd.to_csv("distance_between_youbike_and_metro_station.csv", sep=",", encoding="utf_8_sig")

#%%
# data = pd.read_csv("./distance_between_youbike_and_metro_station(1~31).csv", sep=",", encoding="cp950")
# data.columns = ["Distance_Meters", "Youbike_Station", "Metro_Station"]
# for i in range(len(data)):
#     distance = data["Distance_Meters"].iloc[i].split()[0]
#     meter = data["Distance_Meters"].iloc[i].split()[1]
#     if meter == "公里":
#         distance = float(distance)*1000
#         data["Distance_Meters"].iloc[i] = distance
#     else:
#         data["Distance_Meters"].iloc[i] = distance

# data.to_csv("distance_between_youbike_and_metro_station.csv", sep=",", encoding="utf_8_sig")

    
