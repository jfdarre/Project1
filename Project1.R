# http://rpubs.com/jfdarre/119147

      ##############################################################################
      ##############################################################################
      ###                                                                        ###
      ###                               Project 1:                               ###
      ###                     Annalysis of Lending Club data                     ###
      ###                                                                        ###
      ##############################################################################
      ##############################################################################



#######################################################################################################################
# Loading Libraries, and data
#######################################################################################################################
# What do the different Note statuses mean?
# http://kb.lendingclub.com/investor/articles/Investor/What-do-the-different-Note-statuses-mean/?l=en_US&fs=RelatedArticle
      
library(ggplot2)
library(dplyr)
library(reshape)
library(ggthemes)
library(RColorBrewer)
library(maps)
library(lattice)
library(plotly)


# setting up and loading loan data
rm(list=ls())
setwd('/Users/jfdarre/Documents/NYCDS/Project1')
report_date = 201506
load("LC.RData")
LC0 = LC





#######################################################################################################################
# Looking at some examples to understand the fields
#######################################################################################################################

# illustrative example
a = c("id","member_id","loan_amnt","funded_amnt","funded_amnt_inv","term","int_rate","installment","grade","sub_grade","emp_title","emp_length","home_ownership","annual_inc","verification_status","issue_d","loan_status","pymnt_plan","url","desc","purpose","title","zip_code","addr_state","dti","delinq_2yrs","earliest_cr_line","fico_range_low","fico_range_high","inq_last_6mths","mths_since_last_delinq","mths_since_last_record","open_acc","pub_rec","revol_bal","revol_util","total_acc","initial_list_status","out_prncp","out_prncp_inv","total_pymnt","total_pymnt_inv","total_rec_prncp","total_rec_int","total_rec_late_fee","recoveries","collection_recovery_fee","last_pymnt_d","last_pymnt_amnt","next_pymnt_d","last_credit_pull_d","last_fico_range_high","last_fico_range_low","collections_12_mths_ex_med","mths_since_last_major_derog","policy_code")
b = c("36805548","39558264","10400","10400","10400"," 36 months","  6.99%","321.08","A","A3","Truck Driver Delivery Personel","8 years","MORTGAGE","58000","not verified","Dec-2014","Current","n","https://www.lendingclub.com/browse/loanDetail.action?loan_id=36805548","","credit_card","Credit card refinancing","937xx","CA","14.92","0","Sep-1989","710","714","2","42","","17","0","6133","31.6%","36","w","8544.32","8544.32","2237.46","2237.46","1855.68","381.78","0.0","0.0","0.0","Aug-2015","321.08","Sep-2015","Aug-2015","679","675","0","59","1")
x = data.frame(a,b)
print(x)

# looking at examples of status containing "does not meet the credit policy"
filter(LC0, loan_status == "Does not meet the credit policy.  Status:Current")
t(filter(LC0, id == 627829))

# looking for loans that were delinquent at some point in time, and looking at an example
filter(LC, recoveries != 0)
t(filter(LC, id == 1069559))




#######################################################################################################################
# useful functions that we will use to create bucket names
#######################################################################################################################

# used to create FICO bins to group fico scores
bin_name = function(x) {
  low = 490 + (x - 1) * 30
  high = low + 30
  paste(low, high, sep = "-")
}

# used to generate summaries grouping by ... and showing how much loan amount is in each status
sumPerSatus = function(x, ...){
  x %>% group_by(...) %>%
    summarize(., charged   = round(sum(loan_status_new == "Charged Off") / n() * 100, 2),
                 net_EL    = round(sum((1 - total_rec_prncp / funded_amnt) * 100) / n(), 2),
                 avg_fico  = round(mean(fico_range_high)),
                 avg_grade = sub_grade_vec[mean(LC_score)])
}

# calculating retrun on investment
roi = function(x, ...){
  x %>% group_by(...) %>%
    summarize(., roi       = round(sum((total_pymnt / funded_amnt) * 100) / n(), 2))
}

# used to generate summaries grouping by ... and showing how much loan amount, principal out and recieved principle
sumAmnts = function(x, ...) {
  x %>% group_by(., ...) %>%
    summarise(., total_issued = prettyNum(round(sum(loan_amnt/1)),big.mark = ","),
              n = prettyNum(round(n()),big.mark = ","))
}

# used to generate summaries grouping by ... and showing usefull statics about each group
sumStats = function(x, ...) {
  x %>% group_by(., ...) %>%
    summarise(., median = prettyNum(round(median(loan_amnt/1)),big.mark = ","),
              average = prettyNum(round(mean(loan_amnt/1)),big.mark = ","),
              stdev = prettyNum(round(sd(loan_amnt/1)),big.mark = ","))
}

# used to generate summaries grouping by ... and showing how much loan amount, principal out and recieved principle
sumAmnt = function(x, ...) {
  x %>% group_by(., ...) %>%
    summarise(., total_issued = round(sum(loan_amnt/1e6),1),
              n = round(n()))
}

# used to generate summaries grouping by ... and showing usefull statics about each group
sumStat = function(x, ...) {
  x %>% group_by(., ...) %>%
    summarise(., median = round(median(loan_amnt/1)),
              average = round(mean(loan_amnt/1)),
              stdev = round(sd(loan_amnt/1)))
}




#######################################################################################################################
# Modifying, cleaning the data and adding usefull columns to the original data
#######################################################################################################################

# removing policy_code == 2, i.e. "not public" and then removing the comlumn
sum(filter(LC0, policy_code != 1)$loan_amnt, na.rm = T)
LC = filter(LC, policy_code == 1)
LC = select(LC, -policy_code)

# removing 28 records with a lot of missing data:
sum(filter(LC0, is.na(pub_rec))$loan_amnt, na.rm = T)
LC = filter(LC, !is.na(pub_rec))

# Filtering out the entries where last_fico_range_high = 0
sum(filter(LC0, last_fico_range_high == 0)$loan_amnt, na.rm = T)
LC = filter(LC, last_fico_range_high != 0)

# Removing the loans without any entry for revol_util
sum(filter(LC0, revol_util == "")$loan_amnt, na.rm = T)
LC = filter(LC, revol_util != "")

# Removing the loans with fico scores < 660 as they are very few of them, and LC changed their
# policy and does not issue loan for scores below 660
sum(filter(LC0, fico_range_high < 660)$loan_amnt, na.rm = T)
LC = filter(LC, fico_range_high >= 660)

# Removing "Does not meet the credit policy.  Status:" from:
# Does not meet the credit policy.  Status:Charged Off
# Does not meet the credit policy.  Status:Current
# Does not meet the credit policy.  Status:Fully Paid
LC = mutate(LC, loan_status_new =
                ifelse(grepl("Does not meet the credit policy.  Status:", loan_status),
                       gsub("Does not meet the credit policy.  Status:","",loan_status),
                       loan_status))


# adding issue year, quarter
# adding FICO scores buckets
Months = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
LC = mutate(LC, issue_y = strtoi(substr(issue_d, 5, 9)),
                issue_m = match(substr(issue_d, 1, 3),Months),
                issue_ym = issue_y * 100 + issue_m,
                issue_q = ceiling(issue_m / 3),
                issue_yq = paste(issue_y, "-Q", issue_q, sep = ""),
                n = 1)

# FICO buckets for future visualizations
LC = mutate(LC, FICO_buckets_Original = ceiling((fico_range_high - 490) / 30),
                FICO_buckets_Last = ceiling((fico_range_high - 490) / 30),
                FICO_bin_name_Original = sapply(FICO_buckets_Original, bin_name),
                FICO_bin_name_Last = sapply(FICO_buckets_Last, bin_name))

# Add a feature "matured" for Loans that have or would have matured by report_date
LC = mutate(LC, matured = ifelse((issue_ym + ifelse(term == " 36 months", 300, 500)) > report_date, F, T))

# reduce the number of categories of purpose
LC = mutate(LC, purpose_new = ifelse(purpose == "credit_card" | purpose == "debt_consolidation", "debt",
                              ifelse(purpose == "car" | purpose == "major_purchase" | purpose == "vacation" | purpose == "wedding" | purpose == "medical" | purpose == "other", "purchase",
                              ifelse(purpose == "house" | purpose == "home_improvement" | purpose == "moving" | purpose == "renewable_energy", "purchase",
                                     purpose))))

# reduce the number of categories of purpose
LC = mutate(LC, home = ifelse(home_ownership == "ANY" | home_ownership == "NONE", "OTHER", home_ownership))

# give LC grade numeric values
sub_grade_vec = unique(LC$sub_grade) %>% .[order(., decreasing = T)]
LC = mutate(LC, LC_score = match(sub_grade, sub_grade_vec))

# adding a feature credit_ym corresponding to how many years old is the credit history of a borrower:
LC = mutate(LC, credit_ym = round(((floor(issue_ym/100)*100 + ((issue_ym - floor(issue_ym/100)*100)-1)/12*100)
                                   - (strtoi(substr(earliest_cr_line, 5, 9)) * 100 + (match(substr(earliest_cr_line, 1, 3),Months)-1)/12*100))/100,1))

# creating issue_y buckets:
LC = mutate(LC, issue_bucket = ifelse(issue_y <= 2012, "2007-2012", issue_y))

# delinq_2yrs buckets:
LC = mutate(LC, delinq_bucket = ifelse(delinq_2yrs >= 2, "2+", delinq_2yrs))

# inq_last_6mths buckets:
LC = mutate(LC, inq_bucket = ifelse(inq_last_6mths >= 7, "7+", 
                             ifelse(inq_last_6mths >= 5, "5-6", 
                             ifelse(inq_last_6mths >= 3, "3-4", 
                             ifelse(inq_last_6mths >= 1, "1-2", 0)))))

# public record buckets: 
LC = mutate(LC, rec_bucket = #ifelse(pub_rec >= 10, "10+", 
                             #ifelse(pub_rec >= 7, "7-9", 
                             #ifelse(pub_rec >= 4, "4-6", 
                             ifelse(pub_rec >= 1, "1+", 0))#)))

# Annual income quantile buckets:
groupvec = quantile(LC$annual_inc, seq(0,1,0.1))
labels = c(0, prettyNum(groupvec[2:10], big.mark = ","), "+inf")
labels = paste(labels[1:10], labels[2:11], sep = "-")
LC = mutate(LC, annual_inc_bucket = cut(LC$annual_inc, breaks = groupvec, labels = factor(labels), include.lowest=TRUE))

# DTI quantile buckets:
groupvec = quantile(LC$dti, seq(0,1,0.1))
labels = c(0, prettyNum(groupvec[2:10], big.mark = ","), "+inf")
labels = paste(labels[1:10], labels[2:11], sep = "-")
LC = mutate(LC, dti_bucket = cut(LC$dti, breaks = groupvec, labels = factor(labels), include.lowest=TRUE))

# Revolving balance quantile buckets:
LC = mutate(LC, revol = as.numeric(gsub("%","",revol_util)))
groupvec = quantile(LC$revol, seq(0,1,0.1))
labels = c(0, prettyNum(groupvec[2:10], big.mark = ","), "+inf")
labels = paste(labels[1:10], labels[2:11], sep = "-")
LC = mutate(LC, revol_bucket = cut(LC$revol, breaks = groupvec, labels = factor(labels), include.lowest=TRUE))

# Revolving balance quantile buckets:
groupvec = quantile(LC$revol_bal, seq(0,1,0.1))
labels = c(0, prettyNum(groupvec[2:10], big.mark = ","), "+inf")
labels = paste(labels[1:10], labels[2:11], sep = "-")
LC = mutate(LC, revol_bal_bucket = cut(LC$revol_bal, breaks = groupvec, labels = factor(labels), include.lowest=TRUE))

# total accounts buckets:
groupvec = quantile(LC$total_acc, seq(0,1,0.1))
labels = c(0, prettyNum(groupvec[2:10], big.mark = ","), "+inf")
labels = paste(labels[1:10], labels[2:11], sep = "-")
LC = mutate(LC, total_acc_bucket = cut(LC$total_acc, breaks = groupvec, labels = factor(labels), include.lowest=TRUE))

# open accounts buckets:
groupvec = quantile(LC$open_acc, seq(0,1,0.1))
labels = c(0, prettyNum(groupvec[2:10], big.mark = ","), "+inf")
labels = paste(labels[1:10], labels[2:11], sep = "-")
LC = mutate(LC, open_acc_bucket = cut(LC$open_acc, breaks = groupvec, labels = factor(labels), include.lowest=TRUE))

# credit_y quantile buckets:
groupvec = quantile(LC$credit_ym, seq(0,1,0.1))
labels = c(0, prettyNum(groupvec[2:10], big.mark = ","), "+inf")
labels = paste(labels[1:10], labels[2:11], sep = "-")
LC = mutate(LC, credit_ym_bucket = cut(LC$credit_ym, breaks = groupvec, labels = factor(labels), include.lowest=TRUE))

# create sub table of only the matured loans:
LCmatured = filter(LC, matured == T)

# we'll re-do the quantile buckets for LCmatured to adjust them:
# Annual income quantile buckets:
groupvec = quantile(LCmatured$annual_inc, seq(0,1,0.1))
labels = c(0, prettyNum(groupvec[2:10], big.mark = ","), "+inf")
labels = paste(labels[1:10], labels[2:11], sep = "-")
LCmatured = mutate(LCmatured, annual_inc_bucket = cut(LCmatured$annual_inc, breaks = groupvec, labels = factor(labels), include.lowest=TRUE))

# DTI quantile buckets:
groupvec = quantile(LCmatured$dti, seq(0,1,0.1))
labels = c(0, prettyNum(groupvec[2:10], big.mark = ","), "+inf")
labels = paste(labels[1:10], labels[2:11], sep = "-")
LCmatured = mutate(LCmatured, dti_bucket = cut(LCmatured$dti, breaks = groupvec, labels = factor(labels), include.lowest=TRUE))

# Revolving balance utilization quantile buckets:
# First we need to mutate a field to convert them to numeric:
groupvec = quantile(LCmatured$revol, seq(0,1,0.1))
labels = c(0, prettyNum(groupvec[2:10], big.mark = ","), "+inf")
labels = paste(labels[1:10], labels[2:11], sep = "-")
LCmatured = mutate(LCmatured, revol_bucket = cut(LCmatured$revol, breaks = groupvec, labels = factor(labels), include.lowest=TRUE))

# Revolving balance quantile buckets:
groupvec = quantile(LCmatured$revol_bal, seq(0,1,0.1))
labels = c(0, prettyNum(groupvec[2:10], big.mark = ","), "+inf")
labels = paste(labels[1:10], labels[2:11], sep = "-")
LCmatured = mutate(LCmatured, revol_bal_bucket = cut(LCmatured$revol_bal, breaks = groupvec, labels = factor(labels), include.lowest=TRUE))

# total accounts buckets:
groupvec = quantile(LCmatured$total_acc, seq(0,1,0.1))
labels = c(0, prettyNum(groupvec[2:10], big.mark = ","), "+inf")
labels = paste(labels[1:10], labels[2:11], sep = "-")
LCmatured = mutate(LCmatured, total_acc_bucket = cut(LCmatured$total_acc, breaks = groupvec, labels = factor(labels), include.lowest=TRUE))

# open accounts buckets:
groupvec = quantile(LCmatured$open_acc, seq(0,1,0.1))
labels = c(0, prettyNum(groupvec[2:10], big.mark = ","), "+inf")
labels = paste(labels[1:10], labels[2:11], sep = "-")
LCmatured = mutate(LCmatured, open_acc_bucket = cut(LCmatured$open_acc, breaks = groupvec, labels = factor(labels), include.lowest=TRUE))

# credit_y quantile buckets:
groupvec = quantile(LCmatured$credit_ym, seq(0,1,0.1))
labels = c(0, prettyNum(groupvec[2:10], big.mark = ","), "+inf")
labels = paste(labels[1:10], labels[2:11], sep = "-")
LCmatured = mutate(LCmatured, credit_ym_bucket = cut(LCmatured$credit_ym, breaks = groupvec, labels = factor(labels), include.lowest=TRUE))





#######################################################################################################################
# First look at the data, exploratory research
#######################################################################################################################

# Looking at loan amounts types and loan amounts in each category
min(LC$loan_amnt)
max(LC$loan_amnt)
prettyNum(sum(LC$loan_amnt/1), big.mark = ",")
prettyNum(sum(LC0$loan_amnt/1, na.rm = T), big.mark = ",")


# Looking at total loan amnts per loan_status
# Report showing the amounts and number of loans in each category, here the latest status of the loans
sumAmnts(LC, loan_status_new)
# Report showing some statistics for each category, here the latest status of the loans
sumStats(LC, loan_status_new)
# Pie chart of the distribution of Loans across the different status:
sumAmnt(LC, loan_status_new) %>% merge(sumPerSatus(LC, loan_status_new)) %>%
  plot_ly(type = "pie", 
          labels = loan_status_new, 
          values = total_issued, 
          hole = 0.5,
          marker = list(colors = c("#f2f0f7","#dadaeb","#bcbddc","#9e9ac8","#807dba","#6a51a3","#4a1486"),
                        line = list(width = 1, color = "rgb(52, 110, 165)")),
          sort = F,
          direction = "counterclockwise",
          rotation = 90,
          textinfo = "label+percent",
          textfont = list(size = 14),
          text = paste("Default rates: ", charged),
          textposition = "outside") %>%
  layout(title = 'LOAN ISSUED GROUPED BY STATUS<br>(Hover for breakdown)',
         height = 500, width = 1274, autosize = T, 
         legend = list(font = list(size = 16), x = 1, y = 1, traceorder = "normal"))


# Summary by issue year
# Report showing the amounts and number of loans in each category, here the year of issuance
sumAmnts(LC, issue_y)
# Report showing some statistics for each category, here the year of issuance
sumStats(LC, issue_y)
# Bar chart of LC's quaterly loan issuance
sumAmnt(LC, issue_yq) %>%
  plot_ly(type = "bar", 
          x = issue_yq,
          y = total_issued,
          marker = list(color = total_issued,
                        colorscale = list(c(0, "rgb(183, 232, 161)"), list(1, "rgb(106, 168, 79)")),
                        line = list(width = 1, color = "rgb(255, 255, 255)"))
  ) %>%
  layout(title = "QUATERLY LOAN ISSUANCE", bargap = 0,
         yaxis = list(title = "TOTAL LOAN ISSUED IN MLN USD"),
         xaxis = list(title = "YEAR OF ISSUE", range = c(7,32.5), dtick = 2))


# Looking at grade statistics:
# Report showing the amounts and number of loans in each category, here LC grades
sumAmnts(LC, grade)
# Same report as above but constrained to LCmatured for comparison:
sumAmnts(LCmatured, grade)
# Report showing some statistics for each category, here LC grades
sumStats(LC, grade)
# Same report as above but constrained to LCmatured for comparison:
sumStats(LCmatured, grade)
# Report showing charge off rates, average LC scores and FICO scores for each category, here LC grades
sumPerSatus(LC, grade)
# Same report as above but constrained to LCmatured for comparison:
sumPerSatus(LCmatured, grade)
# Pie chart of the distribution of Loans across the different LC grades:
sumAmnt(LC, grade) %>% merge(sumPerSatus(LC, grade)) %>%
  plot_ly(type = "pie", 
          labels = grade, 
          values = total_issued, 
          hole = 0.5,
          marker = list(colors = c("#f2f0f7","#dadaeb","#bcbddc","#9e9ac8","#807dba","#6a51a3","#4a1486"),
                        line = list(width = 1, color = "rgb(52, 110, 165)")),
          sort = F,
          direction = "counterclockwise",
          rotation = 120,
          textinfo = "label+percent",
          textfont = list(size = 14),
          text = paste("Default rates: ", charged),
          textposition = "outside") %>%
  layout(title = 'LOAN ISSUED GROUPED BY HOME OWNERSHIP<br>(Hover for breakdown)',
         height = 731, width = 1274, autosize = T, 
         legend = list(font = list(size = 16), x = 0.88, y = 1, traceorder = "normal"))
      

# Now let's have a look at FICO scores:
# Report showing the amounts and number of loans in each category, here the FICO score buckets
sumAmnts(LC, FICO_bin_name_Original)
# Report showing some statistics for each category, here the FICO score buckets
sumStats(LC, FICO_bin_name_Original)
# Report showing charge off rates, average LC scores and FICO scores for each category, here the FICO score buckets
sumPerSatus(LC, FICO = FICO_bin_name_Original)
# Same report as above but constrained to LCmatured for comparison:
sumPerSatus(LCmatured, FICO = FICO_bin_name_Original)
# Pie chart of the distribution of Loans across the different FICO score buckets:
sumAmnt(LC, FICO_bin_name_Original) %>% merge(sumPerSatus(LC, FICO_bin_name_Original)) %>%
  plot_ly(type = "pie", 
          labels = FICO_bin_name_Original, 
          values = total_issued, 
          hole = 0.5,
          marker = list(colors = c("#f2f0f7","#dadaeb","#bcbddc","#9e9ac8","#807dba","#6a51a3","#4a1486"),
                        line = list(width = 1, color = "rgb(52, 110, 165)")),
          sort = F,
          direction = "counterclockwise",
          rotation = 120,
          textinfo = "label+percent",
          textfont = list(size = 14),
          text = paste("Default rates: ", charged),
          textposition = "outside") %>%
  layout(title = 'LOAN ISSUED GROUPED BY HOME OWNERSHIP<br>(Hover for breakdown)',
         height = 731, width = 1274, autosize = T, 
         legend = list(font = list(size = 16), x = 0.88, y = 1, traceorder = "normal"))






#######################################################################################################################
# Visualizations of LC Grade vs FICO Scores
#######################################################################################################################

# the first plots are investigationg if there's a correlation between LC grade and the FICO scores of the borrower:
# plot 1 shows the average fico scores of the borrowers against their LC grade:
# as expected, we see that the lower your fico score the worst your LC grade is going to be
gradeVSfico = group_by(LC, grade) %>% summarise(., avg_fico = mean(fico_range_high))
qplot(grade, avg_fico, data = gradeVSfico, geom = "boxplot", color = grade) +
  theme_bw() +
  xlab(toupper("LC Grades ranging from A to G")) +
  ylab("FICO SCORES") +
  ggtitle("AVERAGE FICO SCORE VS. LC GRADES")



# plot 2 shows the average fico scores of the borrowers against their LC sub_grade:
# grades ranging from D1 to G5 (20 levels) have average fico scores ranging from 678 to 688!
sub_gradeVSfico = group_by(LC, sub_grade) %>% summarise(., avg_fico = mean(fico_range_high))
qplot(x = sub_grade, y = avg_fico, data = sub_gradeVSfico, group = 1, geom = "point", size = I(2), col = I("steelblue")) + 
  theme_bw() + 
  geom_hline(y = 688, color = "red") + 
  geom_hline(y = 678, color = "red") + 
  xlab(toupper("LC SUB Grades ranging from A1 to G5")) +
  ylab("FICO SCORES") +
  ggtitle(toupper("Average FICO score vs. LC SUB Grades"))


# plot 3 shows the distribution of fico scores of the borrowers against their LC's grade:
# although there is a trend, we can see on this plot that some borrowers with very high FICO scores
# got really poor LC Grade
qplot(grade, fico_range_high, data = LC, geom = "boxplot", color = grade) +
  theme_bw() +
  xlab(toupper("LC Grades ranging from A to G")) +
  ylab("FICO SCORES") +
  ggtitle(toupper("Box plot of the FICO score distribution for each LC Grades"))


# plot 4 shows the densities of fico scores of the borrowers against their LC's grade:
# there seems to be a cutoff where if your FICO score is below the trigger then it will be impossible
# to get a LC grade of A, B or C.
# we can also see that there are no obvious differences between D, E, F and G and we'll need to explore further.
qplot(grade, fico_range_high, data = LC, geom = "violin", color = grade) +
  theme_bw() +
  xlab(toupper("LC Grades ranging from A to G")) +
  ylab("FICO SCORES") +
  ggtitle(toupper("Box plot of the FICO score distribution for each LC Grades"))


# plot 5 is the same as plot 4 but "warpped" over the years.
# this plot shows how much LC's methodology has evolved over the years
# we see that in 2009 LC seems to have introduced a cutoff at 660 for the loans available to the public
# from 2010 to 2015 we can observe that the distributions are scretching more and more reflecting the fact
# that LC's model is relying less and less on FICO scores and probably more and more on their own data.
qplot(factor(issue_y), fico_range_high, data = filter(LC, grade != "G"), geom = "violin", color = grade) +
  theme_bw() +
  xlab(toupper("LC Grades ranging from A to G")) +
  ylab("FICO SCORES") +
  ggtitle(toupper("Box plot of the FICO score distribution for each LC Grades")) +
  facet_wrap( ~ grade, ncol = 2)




#######################################################################################################################
# Visualizations of LC Grade and FICO Scores vs other relevant variables
#######################################################################################################################

# Home ownership: Does home ownership have any relashionship with LC grades, FICO scores and Charge Off rates?
# Report showing the amounts and number of loans in each category, here the home ownership status of the borrower.
sumAmnts(LCmatured, home)
# Report showing some statistics for each category, here the home ownership status of the borrower.
sumPerSatus(LCmatured, home)
# Home ownership: here is a bubble graph of our 4 home ownership categories:
sumAmnt(LCmatured, home) %>% merge(sumPerSatus(LCmatured, home)) %>%
plot_ly(x = n, 
        y = charged, 
        size = total_issued,
        text = paste(home, "<br>", "Total Issued: ", prettyNum(total_issued, big.mark = ",")),
        mode = "markers", 
        marker = list(color = net_EL,
                      colorscale = list(c(0, "rgb(255, 255, 255)"), list(1, "rgb(65, 50, 103)")),
                      colorbar = list(title = "Net Expected Loss"),
                      cauto = F,
                      cmin = 5,
                      cmax = 13,
                      opacity = 1,
                      line = list(size = 2))
        ) %>%
  layout(xaxis = list(title = toupper("Number of issued loans")),
         yaxis = list(title = toupper("Default rates"), range = c(-2, 25)),
         title = toupper('Loan Issued grouped by home ownership<br>(Hover for breakdown)'))
# Home ownership: here is a pie chart of our 4 home ownership categories:
sumAmnt(LCmatured, home) %>% merge(sumPerSatus(LCmatured, home)) %>%
  plot_ly(type = "pie", 
          labels = home, 
          values = total_issued, 
          hole = 0.5,
          marker = list(colors = c("#f0f9e8", "#bae4bc", "#7bccc4", "#2b8cbe"), 
                        line = list(width = 1, color = "rgb(52, 110, 165)")),
          sort = F,
          direction = "counterclockwise",
          rotation = -45,
          textinfo = "label+percent",
          textfont = list(size = 14),
          text = paste("Default rates: ", charged),
          textposition = "outside") %>%
  layout(title = 'LOAN ISSUED GROUPED BY HOME OWNERSHIP<br>(Hover for breakdown)',
         height = 731, width = 1274, autosize = T, 
         legend = list(font = list(size = 16), x = 0.88, y = 1, traceorder = "normal"))



# Purpose: We want to see if there is any relations between LC Grades, FICO scores, Charge Off rates and purpose
# There is a strong correlation between charge off rates and if the purpose is either educational or small business.
# these loans tend to be a lot riskier. The fico score does not reflect this while the LC score seems to partially
# capture that risk:
# Report showing the amounts and number of loans in each category, here the purpose of the borrower.
sumAmnts(LCmatured, purpose_new)
# Report showing some statistics for each category, here the purpose of the borrower.
sumPerSatus(LCmatured, purpose_new)
# Home ownership: here is a pie chart of our different purposes categories:
sumAmnt(LCmatured, purpose_new) %>% merge(sumPerSatus(LCmatured, purpose_new)) %>%
  plot_ly(type = "pie", 
          labels = purpose_new, 
          values = total_issued, 
          hole = 0.5,
          marker = list(colors = c("#f0f9e8", "#bae4bc", "#7bccc4", "#2b8cbe"), 
                        line = list(width = 1, color = "rgb(52, 110, 165)")),
          sort = F,
          direction = "counterclockwise",
          rotation = -45,
          textinfo = "label+percent",
          textfont = list(size = 14),
          text = paste("Default rates: ", charged),
          textposition = "outside") %>%
  layout(title =  'LOAN ISSUED GROUPED BY PURPOSE<br>(Hover for breakdown)',
         height = 731, width = 1274, autosize = T, 
         legend = list(font = list(size = 16), x = 0.88, y = 1, traceorder = "normal"))


# Revolving Balance, along with Employment length are actually the features with the least obvious link to default rates:
# Report showing the amounts and number of loans in each category, here the revolving balance of the borrower.
sumAmnts(LCmatured, revol_bal_bucket)
# Report showing some statistics for each category, here the revolving balance of the borrower.
sumPerSatus(LCmatured, revol_bal_bucket)
# Report showing the amounts and number of loans in each category, here the employement length of the borrower.
sumAmnts(LCmatured, emp_length)[c(12,1,2,4,5,6,7,8,9,10,11,3),]
# Report showing some statistics for each category, here the employement length of the borrower.
sumPerSatus(LCmatured, emp_length)[c(12,1,2,4,5,6,7,8,9,10,11,3),]


# Number of delinquencies during the past 2 years: This feature is somewhat linked to charge off rates but a vast
# majority of the borrowers actually have 0 delinquencies, which is not helpful distinguishing between them.
# Report showing the amounts and number of loans in each category, here the number of delinquencies of the borrower.
sumAmnts(LCmatured, delinq_bucket)
# Report showing some statistics for each category, here the number of delinquencies of the borrower.
sumPerSatus(LCmatured, delinq_bucket)


# Total number of accounts and Open accounts are not the most impactfull features:
# Fully paying borrower tend to have slightly more accounts.
# Report showing the amounts and number of loans in each category, here the total number of accounts of the borrower.
sumAmnts(LCmatured, total_acc_bucket)
# Report showing some statistics for each category, here the total number of accounts of the borrower.
sumPerSatus(LCmatured, total_acc_bucket)
# Report showing the amounts and number of loans in each category, here the number of opened accounts of the borrower.
sumAmnts(LCmatured, open_acc_bucket)
# Report showing some statistics for each category, here the number of opened accounts of the borrower.
sumPerSatus(LCmatured, open_acc_bucket)


# DTI = debt to income ratio, the lower the better, a DTI of 5 means your debts payment excluding mortgage,
# are only 5% of your gross income
# DTI vs LC Grade, although having the expected trend, does not seem to show an strong dependency.
# DTI vs FICO score show a strong relationship for scores ranging from 700-850 but no correlation for scores < 700
# But overall DTI has some impact on charge off probabilities.
# Report showing the amounts and number of loans in each category, here the DTI of the borrower.
sumAmnts(LCmatured, dti_bucket)
# Report showing some statistics for each category, here the DTI of the borrower.
sumPerSatus(LCmatured, dti_bucket)
# Boxplot of the distribution the DTI for each grade:
qplot(grade, dti, data = LC, geom = "boxplot", color = grade) +
  theme_bw() +
  xlab(toupper("LC Grades ranging from A to G")) +
  ylab(toupper("DTI (Debt to income ratio, in percent")) +
  ggtitle(toupper("DTI distribution vs. LC Grades"))
# Boxplot of the distribution the DTI for each FICO score bucket:
qplot(FICO_bin_name_Original, dti, data = filter(LC, fico_range_high >= 660 ) , geom = "boxplot", color = FICO_bin_name_Original) +
  theme_bw() +
  xlab(toupper("FICO score buckets")) +
  ylab(toupper("DTI (Debt to income ratio, in percent")) +
  ggtitle(toupper("Average FICO score vs. LC Grades"))


# Public Records: This feature is definitely correlated to charged off rates, but it's value is very low.
# If we consider only the instances where pub_rec was not equal to 0, we realize that its correlation completly vanishes.
# Basically if a borrower has 0 public records is a strong indicator that he has greater chances to pay off his debts,
# but if pub_rec != 0 we cannot take any conclusions
# Report showing the amounts and number of loans in each category, here the number of public records of the borrower.
sumAmnts(LCmatured, rec_bucket)
# Report showing some statistics for each category, here the number of public records of the borrower.
sumPerSatus(LCmatured, rec_bucket)


# Age of Credit history is significant:
# Fully paying borrowers tend to have slightly older credit history, which is to be expected.
# But the main take on Credit history is on borrower that have relatively short credit history.
# We can see that the bottom 10% have credit histories of less than 6 years and have a significantly higher default rates.
# Report showing the amounts and number of loans in each category, here the age of the credit history of the borrower.
sumAmnts(LCmatured, credit_ym_bucket)
# Report showing some statistics for each category, here the age of the credit history of the borrower.
sumPerSatus(LCmatured, credit_ym_bucket)


# Revolving utilization: as expected, higher revolving utilization mean higher risk of default.
# The top 10% has default rates that are almost twice as low as the bottom 10%
# Report showing the amounts and number of loans in each category, here the revolving utilization of the borrower.
sumAmnts(LCmatured, revol_bucket)
# Report showing some statistics for each category, here the revolving utilization of the borrower.
sumPerSatus(LCmatured, revol_bucket)


# Annual Income: This is a very strong feature. People in the top 20% quantile have half as much charged off loans
# compared to the bottom 20%. It is worth noticing though that even the top 20% still get a 9.5% chance off defaulting 
# on their loan which is still very high. So annual income is not a silver bullet either.
# Report showing the amounts and number of loans in each category, here the annual income of the borrower.
sumAmnts(LCmatured, annual_inc_bucket)
# Report showing some statistics for each category, here the age of the annual income of the borrower.
sumPerSatus(LCmatured, annual_inc_bucket)


# Inquieries in the last 6months: This feature is supposed to represent how desperate the borrower is for credit.
# We can see that the correlation between this feature and charged off rates is very strong. This is the feature that has the
# highest impact on default rates. The EL of people with 0 inquieries is more than 4 times smaller than the EL of the 7+ bucket!
# Report showing the amounts and number of loans in each category, here the number of inquieries of the borrower.
sumAmnts(LCmatured, inq_bucket)
# Report showing some statistics for each category, here the age of the number of inquieries of the borrower.
sumPerSatus(LCmatured, inq_bucket)


# Summary of our findings:

# Lending Club by states:
addr_state = c("AL", "AK", "AZ", "AR", "CA", "CO", "CT", "DE", "FL", "GA",
              "HI", "ID", "IL", "IN", "IA", "KS", "KY", "LA", "ME", "MD",
              "MA", "MI", "MN", "MS", "MO", "MT", "NE", "NV", "NH", "NJ",
              "NM", "NY", "NC", "ND", "OH", "OK", "OR", "PA", "RI", "SC",
              "SD", "TN", "TX", "UT", "VT", "VA", "WA", "WV", "WI", "WY")

state_name =  c("Alabama",        "Alaska",         "Arizona",        "Arkansas",       "California",     "Colorado",      
                "Connecticut",    "Delaware",       "Florida",        "Georgia",        "Hawaii",         "Idaho",
                "Illinois",       "Indiana",        "Iowa",           "Kansas",         "Kentucky",       "Louisiana",     
                "Maine",          "Maryland",       "Massachusetts",  "Michigan",       "Minnesota",      "Mississippi",   
                "Missouri",       "Montana",        "Nebraska",       "Nevada",         "New Hampshire",  "New Jersey",    
                "New Mexico",     "New York",       "North Carolina", "North Dakota",   "Ohio",           "Oklahoma",      
                "Oregon",         "Pennsylvania",   "Rhode Island",   "South Carolina", "South Dakota",   "Tennessee",     
                "Texas",          "Utah",           "Vermont",        "Virginia",       "Washington",     "West Virginia", 
                "Wisconsin",      "Wyoming")

sumStates = group_by(LC, addr_state) %>%
  summarise(., total_issued = round(sum(loan_amnt/1e6),1), n = n()) %>%
  merge(sumPerSatus(LC, addr_state)[,c(1,2)]) %>%
  merge(data.frame(addr_state, state_name))

sumStates$hover = with(sumStates, paste(state_name, '<br>', 
                                         "Total amount issued:", prettyNum(total_issued, big.mark = ","), "<br>", 
                                         "Number of issued loans:", prettyNum(n, big.mark = ","), "<br>",
                                         "Default rates:", charged))
sumStates$map_amnt = with(sumStates, pmin(total_issued, 500))


l <- list(color = toRGB("steelblue"), width = 1)
g = list(scope = 'usa', projection = list(type = 'albers usa'), showlakes = F, lakecolor = toRGB('white'))

plot_ly(sumStates, z = map_amnt, text = hover, locations = addr_state, type = 'choropleth',
        locationmode = 'USA-states', color = map_amnt, colors = 'Purples',
        marker = list(line = l), colorbar = list(title = "Amount Issued <br>in millions USD")) %>%
  layout(title = 'Total loan amount issued though LC<br>(Hover for breakdown)', geo = g)

plot_ly(filter(sumStates, n > 200), z = charged, text = hover, locations = addr_state, type = 'choropleth',
        locationmode = 'USA-states', color = total_issued, colors = 'Oranges',
        marker = list(line = l), colorbar = list(title = "Default rates<br>in percentage")) %>%
  layout(title = 'Default rates per states<br>(Hover for breakdown)', geo = g)






# year text labels
l <- list(color = toRGB("steelblue"), width = 1)
g = list(scope = 'usa', projection = list(type = 'albers usa'), showlakes = F, lakecolor = toRGB('white'))

sumStatesYear = group_by(LC, issue_y, addr_state) %>% summarize(., total_issued = round(sum(loan_amnt/1e6),1), n = n()) %>%
  merge(data.frame(addr_state, state_name))
sumStatesYear$map_amnt = with(sumStatesYear, pmin(total_issued, 500))
yrs = unique(sumStatesYear$issue_y)
y_id = seq_along(yrs)
df = data.frame(issue_y = yrs, y_id = y_id)

# id for anchoring traces on different plots
sumStatesYear$y_id = as.integer(factor(sumStatesYear$issue_y))
head(sumStatesYear)

p = plot_ly(sumStatesYear,
            type = 'choropleth',
            z = map_amnt, 
            group = issue_y,
            geo = paste0('geo', y_id),
            showlegend = F,
            locations = addr_state, 
            locationmode = 'USA-states',
            color = map_amnt,
            colors = 'Purples',
            marker = list(line = l), 
            colorbar = list(title = "Amount Issued <br>in millions USD")) %>%
  add_trace(lon = -78, lat = 47, mode = 'text', group = issue_y,
            geo = paste0("geo", id), text = issue_y, data = df) %>%  
  layout(title = 'Total loan amount issued though LC<br>(Hover for breakdown)',
         geo = g,
         autosize = F,
         width = 1000,
         height = 900,
         hovermode = F)


subplot(p) 


df <- read.csv('https://raw.githubusercontent.com/plotly/datasets/master/1962_2006_walmart_store_openings.csv')


# common map properties
g <- list(scope = 'usa', showland = T, landcolor = toRGB("gray90"), showcountries = F, subunitcolor = toRGB("white"))

# year text labels
yrs <- unique(df$YEAR)
id <- seq_along(yrs)
df2 <- data.frame(
  YEAR = yrs,
  id = id
)

# id for anchoring traces on different plots
df$id <- as.integer(factor(df$YEAR))

p <- plot_ly(df, type = 'scattergeo', lon = LON, lat = LAT, group = YEAR,
             geo = paste0("geo", id),
             showlegend = F, marker = list(color = toRGB("blue"), opacity = 0.5),
             filename="r-docs/map-subplots") %>%
  add_trace(lon = -78, lat = 47, mode = 'text', group = YEAR,
            geo = paste0("geo", id),
            text = YEAR, data = df2) %>%
  layout(title = 'New Walmart Stores per year 1962-2006<br> Source: <a href="http://www.econ.umn.edu/~holmes/data/WalMart/index.html">University of Minnesota</a>',
         geo = g)

subplot(p, nrows = 9)







plt_list = list()
for (i in 1:9) {
  plt_name = paste0('p', i)
  geog = paste0('geo',i)
  plt_list[[ plt_name ]]  = 
    plot_ly(filter(sumStatesYear, y_id == i), 
            z = map_amnt, 
            locations = addr_state, 
            type = 'choropleth',
            locationmode = 'USA-states',
            color = map_amnt,
            colors = 'Purples',
            marker = list(line = l), 
            colorbar = list(title = "Amount Issued <br>in millions USD")) %>%
    layout(title = 'Total loan amount issued though LC<br>(Hover for breakdown)', geog = g, hovermode = F)
}
str(plt_list)

subplot(plt_list$p1, plt_list$p2, nrows = 2)





# more
grid (grade / year) plot density income / fico