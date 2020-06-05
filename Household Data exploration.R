
attach(house_rec)


# To see the share of people in each region who have toilets 

toilettable <- table(marz, h14_toilet)
prop.table(toilettable, margin=1)*100


# To see the share of people with Internet access in each region 
internettable <- table(marz, h09_internet)
prop.table(internettable, margin=1)*100

# To see the share of large HH  in each region 

peopletable <- table(marz, h00_numppl)
prop.table(peopletable, margin=1)*100
# To see the share of ownership in each region 

ownershiptable <- table(marz, h01_ownership)
prop.table(ownershiptable, margin=1)*100

# To see the share of urbanisation in each region 
placetable <- table(marz, id_ur)
prop.table(placetable, margin=1)*100

# To see the age of houses in each region
builttable <- table(marz, h03_datebuilt)
prop.table(builttable, margin=1)*100

# To see waste removal in each region
wastetable <- table(marz, h15_waste)
prop.table(wastetable, margin=1)*100

# To see the type of accommodation in each region

typetable <- table(marz, h02_type)
prop.table(typetable, margin=1)*100