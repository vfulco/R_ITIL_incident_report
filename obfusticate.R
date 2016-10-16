## obfusticate names
# this is a brute force approach (it's late and I'm tired)


tmp_category <- as.data.frame(levels(thedata$Category))
names(tmp_category) <- "name"
tmp_category$new_name <- paste("category",1:length(tmp_category$name))
levels(thedata$Category)[match(tmp_category$name, levels(thedata$Category))] <- tmp_category$new_name

tmp_team <- as.data.frame(levels(thedata$Team))
names(tmp_team) <- "name"
tmp_team$new_name <- paste("team",1:length(tmp_team$name))
levels(thedata$Team)[match(tmp_team$name, levels(thedata$Team))] <- tmp_team$new_name

tmp_owner <- as.data.frame(levels(thedata$Owner))
names(tmp_owner) <- "name"
tmp_owner$new_name <- paste("incident owner",1:length(tmp_owner$name))
levels(thedata$Owner)[match(tmp_owner$name, levels(thedata$Owner))] <- tmp_owner$new_name

tmp_customer <- as.data.frame(levels(thedata$Customer))
names(tmp_customer) <- "name"
tmp_customer$new_name <- paste("customer",1:length(tmp_customer$name))
levels(thedata$Customer)[match(tmp_customer$name, levels(thedata$Customer))] <- tmp_customer$new_name

thedata$Customer.Email.Address <- "person@provider.com"
thedata$Inbound.email.address[is.na(thedata$Inbound.email.address) == F] <- "person@provider.com"