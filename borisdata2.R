dataframe <-read.csv("borisdata2.csv")

totalEmployees <- sum(dataframe$Employees)
totalEmployers <- sum(dataframe$Employers)
totalOwnaccountworkers <- sum(dataframe$Ownaccountworkers)

Grandtotal <- totalEmployees + totalEmployers + totalOwnaccountworkers

print(dataframe)

EmployeeQ <- (dataframe$Employees/dataframe$Row.Total - totalEmployees/Grandtotal)/ (totalEmployees/Grandtotal)
EmployerQ <- (dataframe$Employers/dataframe$Row.Total - totalEmployers/Grandtotal)/ (totalEmployers/Grandtotal)
OwnaccountworkerQ <- (dataframe$Ownaccountworkers/dataframe$Row.Total - totalOwnaccountworkers/Grandtotal)/(totalOwnaccountworkers/Grandtotal)

dataframe$Row.Total <- NULL
print(dataframe)

out <- chisq.test(dataframe)
print(out)