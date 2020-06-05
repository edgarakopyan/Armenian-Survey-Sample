#Data cleaning 

# There are at most 20 people within one household. Hence we need to remove variables for other 79 people. 

person_rec <- person_rec[,colSums(is.na(person_rec))<nrow(person_rec)]

person_rec <- Filter(function(x)(length(unique(x))>1), person_rec)

# Initially: 3073 columns, after cleaning 664

# Create nationality subset 
grep("p12_nationality_01", colnames(person_rec))
grep("p12_nationality_20", colnames(person_rec))
grep("marz", colnames(person_rec))
nationality <- person_rec[,c(164:183, 664 ) ]
# Create nationality table 
nationalitytable <- table(marz, p12_nationality_01) + table(marz, p12_nationality_02) + table(marz, p12_nationality_03) + table(marz, p12_nationality_04) +
  table(marz, p12_nationality_05) + table(marz, p12_nationality_06) + table(marz, p12_nationality_07) + table(marz, p12_nationality_08) + table(marz, p12_nationality_09) + 
  table(marz, p12_nationality_10) + table(marz, p12_nationality_11) + table(marz, p12_nationality_12) + table(marz, p12_nationality_13) + table(marz, p12_nationality_14) +
  table(marz, p12_nationality_15) + table(marz, p12_nationality_16) + table(marz, p12_nationality_17) + table(marz, p12_nationality_18) + table(marz, p12_nationality_19) +
  table(marz, p12_nationality_20)
prop.table(nationalitytable, margin=1)*100

# Language table 

langtable <- table(marz, p13_lang_01) + table(marz, p13_lang_02) + table(marz, p13_lang_03) + table(marz, p13_lang_04) +
  table(marz, p13_lang_05) + table(marz, p13_lang_06) + table(marz, p13_lang_07) + table(marz, p13_lang_08) + table(marz, p13_lang_09) + 
  table(marz, p13_lang_10) + table(marz, p13_lang_11) + table(marz, p13_lang_12) + table(marz, p13_lang_13) + table(marz, p13_lang_14) +
  table(marz, p13_lang_15) + table(marz, p13_lang_16) + table(marz, p13_lang_17) + table(marz, p13_lang_18) + table(marz,p13_lang_19) +
  table(marz, p13_lang_20)
prop.table(langtable, margin=1)*100

# Religion table 

religtable <- table(marz, p14_relig_01) + table(marz, p14_relig_02) + table(marz, p14_relig_03) + table(marz, p14_relig_04) +
  table(marz, p14_relig_05) + table(marz, p14_relig_06) + table(marz, p14_relig_07) + table(marz, p14_relig_08) + table(marz, p14_relig_09) + 
  table(marz, p14_relig_10) + table(marz, p14_relig_11) + table(marz,p14_relig_12) + table(marz, p14_relig_13) + table(marz, p14_relig_14) +
  table(marz, p14_relig_15) + table(marz, p14_relig_16) + table(marz, p14_relig_17) + table(marz, p14_relig_18) + table(marz,p14_relig_19) +
  table(marz, p14_relig_20)
prop.table(religtable, margin=1)*100

# Citizenship table 

citiztable <- table(marz, p11_citizenship_01) + table(marz, p11_citizenship_02) + table(marz, p11_citizenship_03) + table(marz, p11_citizenship_04) +
  table(marz, p11_citizenship_05) + table(marz, p11_citizenship_06) + table(marz, p11_citizenship_07) + table(marz, p11_citizenship_08) + table(marz, p11_citizenship_09) + 
  table(marz, p11_citizenship_10) + table(marz, p11_citizenship_11) + table(marz,p11_citizenship_12) + table(marz, p11_citizenship_13) + table(marz, p11_citizenship_14) +
  table(marz, p11_citizenship_15) + table(marz, p11_citizenship_16) + table(marz, p11_citizenship_17) + table(marz, p11_citizenship_18) + table(marz,p11_citizenship_19) +
  table(marz, p11_citizenship_20)
prop.table(citizntable, margin=1)*100

# Income table 

incometable <- table(marz, p20_income_01) + table(marz, p20_income_02) + table(marz, p20_income_03) + table(marz, p20_income_04) +
  table(marz, p20_income_05) + table(marz, p20_income_06) + table(marz, p20_income_07) + table(marz, p20_income_08) + table(marz, p20_income_09) + 
  table(marz, p20_income_10) + table(marz, p20_income_11) + table(marz,p20_income_12) + table(marz, p20_income_13) + table(marz, p20_income_14) +
  table(marz, p20_income_15) + table(marz, p20_income_16) + table(marz, p20_income_17) + table(marz, p20_income_18) + table(marz, p20_income_19) +
  table(marz, p20_income_20)
prop.table(incometable, margin=1)*100

# Education table 

edutable <- table(marz, p21_educ_lvl_01) + table(marz, p21_educ_lvl_02) + table(marz, p21_educ_lvl_03) + table(marz, p21_educ_lvl_04) +
  table(marz, p21_educ_lvl_05) + table(marz, p21_educ_lvl_06) + table(marz, p21_educ_lvl_07) + table(marz, p21_educ_lvl_08) + table(marz, p21_educ_lvl_09) + 
  table(marz, p21_educ_lvl_10) + table(marz, p21_educ_lvl_11) + table(marz, p21_educ_lvl_12) + table(marz, p21_educ_lvl_13) + table(marz, p21_educ_lvl_14) +
  table(marz, p21_educ_lvl_15) + table(marz, p21_educ_lvl_16) + table(marz, p21_educ_lvl_17) + table(marz, p21_educ_lvl_18) + table(marz, p21_educ_lvl_19) +
  table(marz, p21_educ_lvl_20)
prop.table(edutable, margin=1)*100

# Literacy level table 

littable <- table(marz, p22_literate_01) + table(marz, p22_literate_02) + table(marz, p22_literate_03) + table(marz, p22_literate_04) +
  table(marz,p22_literate_05) + table(marz, p22_literate_06) + table(marz, p22_literate_07) + table(marz, p22_literate_08) + table(marz, p22_literate_09) + 
  table(marz, p22_literate_10) + table(marz, p22_literate_11) + table(marz, p22_literate_12) + table(marz, p22_literate_13) + table(marz, p22_literate_14) +
  table(marz, p22_literate_15) + table(marz, p22_literate_16) + table(marz, p22_literate_17) + table(marz, p22_literate_18) + table(marz, p22_literate_19) +
  table(marz, p22_literate_20)
prop.table(littable, margin=1)*100

# Occupation status table 

ocstable <- table(marz, p29_status_01) + table(marz, p29_status_02) + table(marz, p29_status_03) + table(marz, p29_status_04) +
  table(marz,p29_status_05) + table(marz, p29_status_06) + table(marz, p29_status_07) + table(marz, p29_status_08) + table(marz, p29_status_09) + 
  table(marz, p29_status_10) + table(marz, p29_status_11) + table(marz, p29_status_12) + table(marz, p29_status_13) + table(marz, p29_status_14) +
  table(marz, p29_status_15) + table(marz, p29_status_16) + table(marz, p29_status_17) + table(marz, p29_status_18) + table(marz, p29_status_19) +
  table(marz, p29_status_20)
prop.table(ocstable, margin=1)*100

# Age table 

agetable <- table(marz, p09d_age_01) + table(marz, p09d_age_02) + table(marz, p09d_age_03) + table(marz, p09d_age_04) +
  table(marz,p09d_age_05) + table(marz, p09d_age_06) + table(marz, p09d_age_07) + table(marz, p09d_age_08) + table(marz, p09d_age_09) + 
  table(marz, p09d_age_10) + table(marz, p09d_age_11) + table(marz, p09d_age_12) + table(marz, p09d_age_13) + table(marz, p09d_age_14) +
  table(marz, p09d_age_15) + table(marz, p09d_age_16) + table(marz, p09d_age_17) + table(marz, p09d_age_18) + table(marz, p09d_age_19) +
  table(marz, p09d_age_20)
prop.table(agetable, margin=1)*100

# Morital Status table 

mstable <- table(marz, p24a_ms_01) + table(marz, p24a_ms_02) + table(marz, p24a_ms_03) + table(marz, p24a_ms_04) +
  table(marz,p24a_ms_05) + table(marz, p24a_ms_06) + table(marz, p24a_ms_07) + table(marz, p24a_ms_08) + table(marz, p24a_ms_09) + 
  table(marz, p24a_ms_10) + table(marz, p24a_ms_11) + table(marz, p24a_ms_12) + table(marz, p24a_ms_13) + table(marz, p24a_ms_14) +
  table(marz, p24a_ms_15) + table(marz, p24a_ms_16) + table(marz, p24a_ms_17) + table(marz, p24a_ms_18) + table(marz, p24a_ms_19) +
  table(marz, p24a_ms_20)
prop.table(mstable, margin=1)*100

# Sex table 

sextable <- table(marz, p08_sex_01) + table(marz, p08_sex_02) + table(marz, p08_sex_03) + table(marz, p08_sex_04) +
  table(marz,p08_sex_05) + table(marz, p08_sex_06) + table(marz, p08_sex_07) + table(marz, p08_sex_08) + table(marz, p08_sex_09) + 
  table(marz, p08_sex_10) + table(marz, p08_sex_11) + table(marz, p08_sex_12) + table(marz, p08_sex_13) + table(marz, p08_sex_14) +
  table(marz, p08_sex_15) + table(marz, p08_sex_16) + table(marz, p08_sex_17) + table(marz, p08_sex_18) + table(marz, p08_sex_19) +
  table(marz, p08_sex_20)
prop.table(sextable, margin=1)*100
