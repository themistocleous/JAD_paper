# Some basic statistics about the differences between Groups in R

# GENDER
GENDER_variant  = c("l","n","s","l", "n", "s")
GENDER_gender = c("F","F","F","M","M","M")
GENDER_values = c(2457, 1129,962,1727,1594,529)
kruskal.test(GENDER_values~GENDER_variant)


#AGE
AGE_variant = c("l","l","l","l","s","n","n","l","l","s","n","s","n","l","n","l","l","s","n","l","s","l","n","l","s","l","s","n","l")
AGE_values = c(191,235,130,119,723,1426,64,459,451,191,273,266,873,573,237,404,355,92,565,447,240,94,74,268,523,39,238,70)
AGE_age = c(51.0,53.0,54.0,55.0,59.0,60.0,62.0,63.0,64.0,64.0,64.0,65.0,66.0,69.0,69.0,70.0,71.0,71.0,71.0,72.0,73.0,73.0,73.0,74.0,75.0,77.0,77.0,80.0,80.0)
kruskal.test(AGE_age~AGE_variant)


# EDUCATION
EDU_values = c(39,431,199,152,64,1347,1253,375,94,1404,723,298,635,69)
EDU_variant = c("s","l","s","n","l","l","s","n","n","l","s","n","l","s")
EDU_edu = c(10, 12, 12,14,15,16,16,16,17,18,18,18,20,20)
kruskal.test(EDU_edu~EDU_variant)

# LANG_SEVERITY
LANG_SEVERITY_values = c(875,1001,418,138,1550,1297,1139,829,237,45)
LANG_SEVERITY_lang = c(0.5,1,1,1,2,2,2,3,3,3)
LANG_SEVERITY_variant = c("l","l","n","s","s","l","n","s","l","n")
kruskal.test(LANG_SEVERITY_lang ~LANG_SEVERITY_variant)

# TOTAL_SEVERITY
TOTAL_SEVERITY_total = c(1,1.5,2,3,3,3.5,4,5,5,5.5,6,6.5,7,7.5,8,8,8,8.5,9,9,9.5,9.5,10,10,14,15,17.5)
TOTAL_SEVERITY_variant = c("s","l","l","n","l","l","s","l","n","s","n","l","l","s","s","n","l","l","s","n","l","n","n","l","s","n","s")
TOTAL_SEVERITY_values = c(138,743,191,656,74,569,355,404,94,723,275,523,240,199,447,142,130,64,273,92,235,6,298,237,69,39,313)
kruskal.test(TOTAL_SEVERITY_total ~ TOTAL_SEVERITY_variant)


# NUMBER_OF_WORDS
WORDS_values = c(3, 70,74,62,56,202,83,112,31,98,35,56,81,50,216,67,83,168,431,123,441,67,58,100,91,43,47,45,362,46,30,211,11,46,45,126,30,116,60,140,85,53,95,171)
WORDS_variant = c("n","n","n","n","n","n","n","n","n","n","n","l","l","l","l","l","l","l","l","l","l","l","l","l","l","l","l","l","l","s","s","s","s","s","s","s","s","s","s","s","s","s","s","s")
kruskal.test(WORDS_values ~ WORDS_variant)
summary(aov(WORDS_values ~ WORDS_variant))