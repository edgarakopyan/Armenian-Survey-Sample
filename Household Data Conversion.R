cspro.factor.type = 1
cspro.factor.create.new.variable = FALSE
library("dplyr")
# CSPro Export Factor Options:

#	cspro.factor.type (0): do not use factors
#	cspro.factor.type (1): factor only discrete numeric variables
#	cspro.factor.type (2): factor both discrete numeric and alpha variables

#	cspro.factor.create.new.variable: TRUE to add the factored variables as separate variables


house_rec <- read.fortran("HOUSE_REC.DAT",c("I3","I1","I5","I2","I1","I1","I1","I1","I2","I4","I1","I1","I1","I1","I1","I1","I1","I1","I1","I1","I1","I1","I1","I1","I2","I2","I2","I1"))

names(house_rec) <- c("id_marz","id_ur","id_hhnum","h00_numppl","h01_ownership","h02_type","h03_datebuilt","h04_walls","h05_roomcount","h06_roomsize","h07_phone","h08_comp","h09_internet","h10_heat","h11_water","h12_canalisation","h13_bathtub","h14_toilet","h15_waste","d01_getsmoney","d02_births","d02_1_regbirth","d02_2notregbirth","d03_disability","d03_disab1","d03_disab2","d03_disab3","d03_notdisabilities")

if( cspro.factor.type != 0 ) {

	if( cspro.factor.create.new.variable ) {
		house_rec$id_marz.f <- factor(house_rec$id_marz,levels = c(901,902,903,904,905,906,907,908,909,910,911),labels = c("901","902","903","904","905","906","907","908","909","910","911"))
	} else {
		house_rec$id_marz <- factor(house_rec$id_marz,levels = c(901,902,903,904,905,906,907,908,909,910,911),labels = c("901","902","903","904","905","906","907","908","909","910","911"))
	}

	if( cspro.factor.create.new.variable ) {
		house_rec$id_ur.f <- factor(house_rec$id_ur,levels = c(1,2),labels = c("Urban","Rural"))
	} else {
		house_rec$id_ur <- factor(house_rec$id_ur,levels = c(1,2),labels = c("Urban","Rural"))
	}

	if( cspro.factor.create.new.variable ) {
		house_rec$h01_ownership.f <- factor(house_rec$h01_ownership,levels = c(1,2,3,4,5,6),labels = c("The H/H member(s)","State","Community","Legal entity","Other person(s)","Homeless"))
	} else {
		house_rec$h01_ownership <- factor(house_rec$h01_ownership,levels = c(1,2,3,4,5,6),labels = c("The H/H member(s)","State","Community","Legal entity","Other person(s)","Homeless"))
	}

	if( cspro.factor.create.new.variable ) {
		house_rec$h02_type.f <- factor(house_rec$h02_type,levels = c(1,2,3,4,5,6,7,8,9),labels = c("Dwelling house (detached house)","Part of dwelling house","Apartment","Part of apartment","Cottage","Shelter","Co-residence dwelling","Dormitory","Other dwelling unit"))
	} else {
		house_rec$h02_type <- factor(house_rec$h02_type,levels = c(1,2,3,4,5,6,7,8,9),labels = c("Dwelling house (detached house)","Part of dwelling house","Apartment","Part of apartment","Cottage","Shelter","Co-residence dwelling","Dormitory","Other dwelling unit"))
	}

	if( cspro.factor.create.new.variable ) {
		house_rec$h03_datebuilt.f <- factor(house_rec$h03_datebuilt,levels = c(1,2,3,4,5,6),labels = c("Up to 1950","1951-1970","1971-1980","1981-1990","1991-2000","2001-2011"))
	} else {
		house_rec$h03_datebuilt <- factor(house_rec$h03_datebuilt,levels = c(1,2,3,4,5,6),labels = c("Up to 1950","1951-1970","1971-1980","1981-1990","1991-2000","2001-2011"))
	}

	if( cspro.factor.create.new.variable ) {
		house_rec$h04_walls.f <- factor(house_rec$h04_walls,levels = c(1,2,3,4,5,6),labels = c("Stone","Panel","Monolith","Wood","Mixed material","Other"))
	} else {
		house_rec$h04_walls <- factor(house_rec$h04_walls,levels = c(1,2,3,4,5,6),labels = c("Stone","Panel","Monolith","Wood","Mixed material","Other"))
	}

	if( cspro.factor.create.new.variable ) {
		house_rec$h07_phone.f <- factor(house_rec$h07_phone,levels = c(1,2),labels = c("Yes","No"))
	} else {
		house_rec$h07_phone <- factor(house_rec$h07_phone,levels = c(1,2),labels = c("Yes","No"))
	}

	if( cspro.factor.create.new.variable ) {
		house_rec$h08_comp.f <- factor(house_rec$h08_comp,levels = c(1,2),labels = c("Yes","No"))
	} else {
		house_rec$h08_comp <- factor(house_rec$h08_comp,levels = c(1,2),labels = c("Yes","No"))
	}

	if( cspro.factor.create.new.variable ) {
		house_rec$h09_internet.f <- factor(house_rec$h09_internet,levels = c(1,2,3),labels = c("Yes, permanently","Yes, not permanently","No"))
	} else {
		house_rec$h09_internet <- factor(house_rec$h09_internet,levels = c(1,2,3),labels = c("Yes, permanently","Yes, not permanently","No"))
	}

	if( cspro.factor.create.new.variable ) {
		house_rec$h10_heat.f <- factor(house_rec$h10_heat,levels = c(1,2,3,4,5,6,7,8),labels = c("Individual heating system","Central heating","Gaz furnace","Electric heater","Wood furnace","Animal dung furnace","Other","Not heating"))
	} else {
		house_rec$h10_heat <- factor(house_rec$h10_heat,levels = c(1,2,3,4,5,6,7,8),labels = c("Individual heating system","Central heating","Gaz furnace","Electric heater","Wood furnace","Animal dung furnace","Other","Not heating"))
	}

	if( cspro.factor.create.new.variable ) {
		house_rec$h11_water.f <- factor(house_rec$h11_water,levels = c(1,2,3,4,5,6,7,8),labels = c("Central water supply in dwelling unit","Central water supply in the building but not in dwelling unit","Central water supply in the yard","Carried/bought water","Water collection own system","River/spring","Well","Other"))
	} else {
		house_rec$h11_water <- factor(house_rec$h11_water,levels = c(1,2,3,4,5,6,7,8),labels = c("Central water supply in dwelling unit","Central water supply in the building but not in dwelling unit","Central water supply in the yard","Carried/bought water","Water collection own system","River/spring","Well","Other"))
	}

	if( cspro.factor.create.new.variable ) {
		house_rec$h12_canalisation.f <- factor(house_rec$h12_canalisation,levels = c(1,2,3,4),labels = c("Connected to the central sewerage system","Has local net","Other","Doesn't have"))
	} else {
		house_rec$h12_canalisation <- factor(house_rec$h12_canalisation,levels = c(1,2,3,4),labels = c("Connected to the central sewerage system","Has local net","Other","Doesn't have"))
	}

	if( cspro.factor.create.new.variable ) {
		house_rec$h13_bathtub.f <- factor(house_rec$h13_bathtub,levels = c(1,2,3,4,5,6),labels = c("Yes, in dwelling unit","Yes, in the building but not in dwelling unit, only for the H/H use","Yes, in the building but not in dwelling unit , used with other H/Hs","Yes, out of the building, only for the H/H","Yes, out of the building, used with other H/Hs","No"))
	} else {
		house_rec$h13_bathtub <- factor(house_rec$h13_bathtub,levels = c(1,2,3,4,5,6),labels = c("Yes, in dwelling unit","Yes, in the building but not in dwelling unit, only for the H/H use","Yes, in the building but not in dwelling unit , used with other H/Hs","Yes, out of the building, only for the H/H","Yes, out of the building, used with other H/Hs","No"))
	}

	if( cspro.factor.create.new.variable ) {
		house_rec$h14_toilet.f <- factor(house_rec$h14_toilet,levels = c(1,2,3,4,5,6,7,8),labels = c("In dwelling unit, only for the H/H use","In dwelling unit , used with other H/Hs","Out of dwelling, only for the H/H","Out of dwelling, used with other H/Hs","Only for the H/H use","Used with other H/Hs","Other","Doesn't have"))
	} else {
		house_rec$h14_toilet <- factor(house_rec$h14_toilet,levels = c(1,2,3,4,5,6,7,8),labels = c("In dwelling unit, only for the H/H use","In dwelling unit , used with other H/Hs","Out of dwelling, only for the H/H","Out of dwelling, used with other H/Hs","Only for the H/H use","Used with other H/Hs","Other","Doesn't have"))
	}

	if( cspro.factor.create.new.variable ) {
		house_rec$h15_waste.f <- factor(house_rec$h15_waste,levels = c(1,2,3,4,5),labels = c("Regular remove","Irregular remove","Thrown into the regular remove bin","Thrown into the irregular remove bin","Other"))
	} else {
		house_rec$h15_waste <- factor(house_rec$h15_waste,levels = c(1,2,3,4,5),labels = c("Regular remove","Irregular remove","Thrown into the regular remove bin","Thrown into the irregular remove bin","Other"))
	}

	if( cspro.factor.create.new.variable ) {
		house_rec$d01_getsmoney.f <- factor(house_rec$d01_getsmoney,levels = c(1,2,3,4),labels = c("Yes, regularly","Yes, sometimes","Yes, seldom","No"))
	} else {
		house_rec$d01_getsmoney <- factor(house_rec$d01_getsmoney,levels = c(1,2,3,4),labels = c("Yes, regularly","Yes, sometimes","Yes, seldom","No"))
	}

	if( cspro.factor.create.new.variable ) {
		house_rec$d03_notdisabilities.f <- factor(house_rec$d03_notdisabilities,levels = c(1),labels = c("No disabled"))
	} else {
		house_rec$d03_notdisabilities <- factor(house_rec$d03_notdisabilities,levels = c(1),labels = c("No disabled"))
	}

}

rm(cspro.factor.type)
rm(cspro.factor.create.new.variable)


key <- data.frame(id_marz=as.factor(901:911), marz=paste(c("Yerevan city", "Aragatsotn", "Ararat", "Armavir","Gegharkunik","Lori","Kotayk","Shirak","Syunik","Vayots Dzor", "Tavush" )), stringsAsFactors = FALSE)
house_rec <- left_join(house_rec, key, by="id_marz")






