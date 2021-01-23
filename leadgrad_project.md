# Importing data sets into R

test_samples <- read.csv(file.choose(), header=TRUE)
trans_data <- read.csv(file.choose(), header=TRUE)

# QUESTION 1:

# Due to large data set, the approximate probability distribution
# between the test group and control group is a normal distribution.

# Our null hypothesis is that the mean of the test group is equal to the mean
# of the control group. In the context of this experiment, this means that
# forcing members to call in to cancel their subscription has no impact on
# cancellations in comparison to being able to cancel on the web page.


# We form a new data set that includes both test group and transaction data.
data_final <-merge(test_samples, trans_data, by="sample_id")


# We classify transactions by whether customers renewed or canceled their
# subscription. To do this, we will denote "REBILL" at 0 and "CHARGEBACK" or
# "REFUND" as 1. This is so that we have a way of finding the mean of rebills
# to cancellations.

data_final$transaction_type [data_final$transaction_type == "REBILL"] <- 0

data_final$transaction_type [data_final$transaction_type == "REFUND"] <- 1

data_final$transaction_type [data_final$transaction_type == "CHARGEBACK"] <- 1

data_final$transaction_type <- as.numeric(data_final$transaction_type)


# We create a data set for both the test and control group. In order to do this,
# the test_samples values will be used to separate the two based on whether they
# have a value of 1 or 0. The 0 will go into the control group and 1 will go into
# the test group.

control_data <- data_final[test_group == 0, ]

test_data <- data_final[test_group == 1, ]

# Converting the new "transaction type" into a numeric value.This way it can more
# easily be manipulated.
control_data$transaction_type <- as.numeric(control_data$transaction_type)

test_data$transaction_type <- as.numeric(test_data$transaction_type)



# QUESTION #2:

# First we will create a pie graph to show the difference between control and
# test groups, then we will carry out a t-test on the data.

# Counting the number of rebills and chargebacks/refunds.
test_count <- table(control_data$transaction_type)

control_count <- table(test_data$transaction_type)

# Calculating the percentage values for rebills to chargebacks/refunds for
# control versus test groups.
control_percentages <- table(control_data$transaction_type)/4050

test_percentages <- table(test_data$transaction_type)/3380


#Making a pie chart to show difference between the test and control groups.
pie(test_percentages, main = "Test Group")

pie(control_percentages, main = "Control Group")


#Calculating means and standard deviations.
mean(control_data$transaction_type)
sd(control_data$transaction_type)

mean(test_data$transaction_type)
sd(test_data$transaction_type)

# A boxplot of the data for both test and control groups.
boxplot(data_final$transaction_type ~ data_final$test_group)

# Performing the t-test with a confidence interval of 95%.

# Ho: mean of control group is equal to mean of test group
# Ha: mean of test group is greater than mean of control group
# Assuming non-equal variances

t.test(data_final$transaction_type ~ data_final$test_group, mu=0, alt="greater", conf=0.95, var.eq=F, paired=F)

# Therefore, we reject the null hypothesis in favour of the alternative hypothesis.
# We conclude that having customers call in leads to less cancellations
# than allowing them to cancel on the website.

# Answering the question posed, forcing users to call in is very likely to generate
# at least one additional rebill considering the large sample size.



# QUESTION 3:

# To do this, we will use a similar method to the last example but will use
# revenue rather than simply renewals or cancellations, so that the amounts
# of the renewals or cancellations are taken into account.

# A boxplot of the data for both test and control groups.
boxplot(data_final$transaction_amount ~ data_final$test_group)

# We do a t-test with a confidence interval of 95%.

# Ho: mean of control group is equal to mean of test group
# Ha: mean of test group is greater than mean of control group
# Assuming non-equal variances

t.test(data_final$transaction_amount ~ data_final$test_group, mu=0, alt="greater", conf=0.95, var.eq=F, paired=F)

# Therefore, we reject the null hypothesis in favour of the alternative hypothesis.
# We conclude that having customers call in leads to greater revenue than allowing
# them to cancel on the website.



# QUESTION 4:

# Using the results from the second question, we can conclude that forcing users
# to call in will increase the amount of revenue.