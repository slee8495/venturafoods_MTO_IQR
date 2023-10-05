library(tidyverse)
library(magrittr)
library(openxlsx)
library(readxl)
library(writexl)
library(reshape2)
library(skimr)
library(janitor)
library(lubridate)


mto_tab <- read_excel("MTO.xlsx",
                      sheet = "Ship Loc Pivot - MTO")


mto_tab[-1:-2, -12:-ncol(mto_tab)] -> mto_tab_2

colnames(mto_tab_2) <- mto_tab_2[1, ]


# For Page 1
mto_tab_2 %>% 
  dplyr::slice(-1) %>% 
  janitor::clean_names() %>% 
  readr::type_convert() %>%
  dplyr::filter(sum_of_on_hand_open_order_all != 0) %>%
  dplyr::rename(Location = loc,
                Item = item_2,
                Description = description,
                Formula = formula,
                MPF = mpf,
                Unit_Cost = unit_cost,
                Count_of_Item = count_of_item,
                On_Hand = on_hand,
                Open_Orders = open_orders,
                On_Hand_minus_Open_Orders = sum_of_on_hand_open_order_all,
                Exposure_Amount = sum_of_on_hand_open_orders_all) %>% 
  dplyr::arrange(Location, desc(Exposure_Amount)) -> mto_tab_2

# For Page 2
mto_tab_2 %>% 
  dplyr::group_by(Location) %>% 
  summarise(Count_of_Item = sum(Count_of_Item),
            On_Hand = sum(On_Hand),
            Open_Orders = sum(Open_Orders),
            On_Hand_minus_Open_Orders = sum(On_Hand_minus_Open_Orders),
            Sum_Exposure = sum(Exposure_Amount)) %>% 
  arrange(as.numeric(Location)) %>% 
  dplyr::rename("Count of Item" = Count_of_Item,
                "On Hand" = On_Hand,
                "Open Orders" = Open_Orders,
                "On Hand - Open Orders" = On_Hand_minus_Open_Orders,
                "Exposure Amount" = Sum_Exposure) -> mto_tab_3

# Rename Page 1 Table
mto_tab_2 %>% 
  dplyr::rename("Unit Cost" = Unit_Cost,
                "Count of Item" = Count_of_Item,
                "On Hand" = On_Hand,
                "Open Orders" = Open_Orders,
                "On Hand - Open Orders" = On_Hand_minus_Open_Orders,
                "Exposure Amount" = Exposure_Amount) -> mto_tab_2
