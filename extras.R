# used to generate summaries grouping by ... and showing how much loan amount is in each status
sumPerSatus = function(x, ...){
  x %>% group_by(...) %>%
    summarize(., paid    = round(sum(loan_status_new == "Fully Paid")/n()*100,2),
                 charged = round(sum(loan_status_new == "Charged Off")/n()*100,2))
}

# used to generate summaries grouping by ... and showing how much loan amount, principal out and recieved principle
sumAmnts = function(x, ...) {
  temp = group_by(x, ...) %>%
    summarise(., total_issued = prettyNum(round(sum(loan_amnt/1)),big.mark = ","),
#              total_out = prettyNum(round(sum(out_prncp/1)),big.mark = ","),
#              total_rec = prettyNum(round(sum(total_rec_prncp/1)),big.mark = ","),
              n = prettyNum(round(n()),big.mark = ","))
  return(temp)
}

# used to generate summaries grouping by ... and showing usefull statics about each group
sumStats = function(x, ...) {
  temp = group_by(x, ...) %>%
    summarise(., median = prettyNum(round(median(loan_amnt/1)),big.mark = ","),
              average = prettyNum(round(mean(loan_amnt/1)),big.mark = ","),
              stdev = prettyNum(round(sd(loan_amnt/1)),big.mark = ","))
#              lateFee = prettyNum(round(sum(total_rec_late_fee/1)),big.mark = ","),
#              rcvrFee = prettyNum(round(sum(collection_recovery_fee/1)),big.mark = ","),
#              minAmnt = min(loan_amnt),
#              maxAmnt = max(loan_amnt))
  return(temp)
}


densityplot( ~ dti, filter(LCmatured, loan_status_new %in% c("Charged Off", "Fully Paid")), groups = loan_status_new)


# Total number of account isnt significant:
# Fully paying borrower tend to have slightly more accounts.
filter(LCmatured, loan_status_new %in% c("Fully Paid", "Charged Off")) %>%
  group_by(., loan_status_new) %>%
  summarise(., avg_revol = round(mean(total_acc),2), loan_amnt = prettyNum(sum(loan_amnt), big.mark = ","))

sumPerSatus(LCmatured, total_acc_bucket)


# Home ownership: Does home ownership have any relashionship with LC grades, FICO scores and Charge Off rates?
sumAmnts(LCmatured, home)
sumPerSatus(LCmatured, home)

#qplot(factor(issue_y), fico_range_high, data = filter(LC, home != "OTHER", issue_y > 2009), geom = "boxplot") +
#  theme_economist() +
#  scale_fill_economist() +
#  xlab("Issue Year") +
#  ylab("FICO score") +
#  ggtitle("FICO score distribution vs. Issue Year for each Home Ownership status") +
#  facet_wrap( ~ home)

#qplot(home, fico_range_high, data = filter(LC, home != "OTHER", issue_y > 2009), geom = "boxplot") +
#  theme_economist() +
#  scale_fill_economist() +
#  xlab("Home Ownership") +
#  ylab("FICO score") +
#  ggtitle("FICO score distribution vs. Home Ownership for each Issue Years") +
#  facet_wrap( ~ issue_y)

#qplot(factor(issue_y), LC_score, data = filter(LCmatured, home != "OTHER", issue_y > 2009), geom = "boxplot") +
#  theme_economist() +
#  scale_fill_economist() +
#  xlab("Issue Year") +
#  ylab("LC Score") +
#  ggtitle("LC score distribution vs. Issue Year for each Home Ownership status") +
#  facet_wrap( ~ home)

qplot(home, LC_score, data = filter(LCmatured, home != "OTHER", issue_y > 2009), geom = "boxplot") +
  theme_economist() +
  scale_fill_economist() +
  xlab("Home Ownership") +
  ylab("LC score") +
  ggtitle("LC score distribution vs. Home Ownership for each Issue Years") +
  facet_wrap( ~ issue_y)

ggplot(LC, aes(x = LC_score, y = fico_range_high)) +
  stat_density2d(aes(fill = home, colour = home, alpha = ..level.., size = 0.5), geom = "polygon") +
  scale_alpha(range = c(0.35, 0.75)) +
  scale_size(range = c(0,0.2), guide = "none") +
  theme_economist() +
  facet_wrap( ~ home)


# Annual Income: This is a very strong feature. People in the top 20% quantile have half as much charged off loans
# compared to the bottom 20%. It is worth noticing though that even the top 20% still get a 9.5% chance off defaulting 
# on their loan which is still very high. So annual income is not a silver bullet either.
sumPerSatus(LCmatured, annual_inc_bucket) %>%
ggplot(data = ., aes(x = annual_inc_bucket, y = charged)) +
  scale_y_continuous(limits = c(0, 30)) +
  geom_histogram(stat = "identity", colour = I("steelblue"), fill = I("steelblue")) +
  theme_economist() +
  scale_fill_economist() +
  xlab("Annual Income Quantile Buckets") +
  ylab("Percentage of Charged Off Loans") +
  ggtitle("Charge Off risk depending on Annual Income")
