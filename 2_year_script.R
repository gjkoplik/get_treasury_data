# Gary Koplik
# Winter, 2018
# 2_year_script.R

# clear old data for replicability
rm(list = ls() )

# load libraries
library(ggplot2)
library(ggthemes)
library(dplyr)
library(tidyr)
library(lubridate)

# read in data

dat <- read.csv("./auction_2-YEAR_tips_N_frn_N.csv")

# switch auction date to posixct
dat$AuctionDate <- as.POSIXct(dat$AuctionDate)

# Offering amount over time
ggplot(data = dat) +
  geom_line(aes(x = AuctionDate, y = OfferingAmount), color = "black", size = 0.7) +
  ylab("Offering Amount") +
  xlab("Auction Date") +
  ggtitle("Offering Amount")  +
  labs(subtitle = "2-Year Note") +
  theme_bw()
ggsave("./figures/two_year_offering_amount.png", dpi = 150, width = 5, height = 4, units = "in")

# interest rate (high yield) over time
ggplot(data = dat) +
  geom_line(aes(x = AuctionDate, y = HighYield), color = "black", size = 0.7) +
  ylab("High Yield") +
  xlab("Auction Date") +
  ggtitle("High Yield") +
  labs(subtitle = "2-Year Note") +
  theme_bw()
ggsave("./figures/two_year_high_yield.png", dpi = 150, width = 5, height = 4, units = "in")

# interest rate vs high yield over time
ggplot(data = dat) +
  geom_line(aes(x = AuctionDate, y = HighYield - InterestRate), color = "black", size = 0.7) +
  ylab("High Yield - Interest Rate") +
  xlab("Auction Date") +
  ggtitle("High Yield - Interest Rate") +
  labs(subtitle = "2-Year Note") +
  theme_bw()
ggsave("./figures/two_year_high_yield_minus_int_rate.png",
       dpi = 150, width = 5, height = 4, units = "in")

# range of yields bid for each auction
ggplot(data = dat) +
  geom_segment(aes(x = AuctionDate, xend = AuctionDate,
                   y = LowYield, yend = HighYield), color = 'black', size = 0.7,
               lineend = "square") +
  geom_point(aes(x = AuctionDate, y = MedianYield), color = "red", size = 0.7) +
  ylab("Yield Bidding Range") +
  xlab("Auction Date") +
  ggtitle("Range of Yields Bid") +
  labs(subtitle = "2-Year Note (Median Yield in Red)") + 
  theme_bw()
ggsave("./figures/two_year_yield_range.png",
       dpi = 150, width = 5, height = 4, units = "in")

# size of yield bidding range
ggplot(data = dat) +
  geom_line(aes(x = AuctionDate,
                   y = HighYield - LowYield), color = 'black', size = 0.7) +
  geom_line(aes(x = AuctionDate, y = HighYield - MedianYield), color = "red", size = 0.7) +
  ylab("Size of Yield Bidding Range") +
  xlab("Auction Date") +
  ggtitle("High Yield - Low Yield") +
  labs(subtitle = "2-Year Note (High Yield - Median Yield in Red)") + 
  theme_bw()
ggsave("./figures/two_year_high_minus_low.png",
       dpi = 150, width = 5, height = 4, units = "in")

# bid to cover
ggplot(data = dat) +
  geom_line(aes(x = AuctionDate,
                y = BidToCoverRatio), color = 'black', size = 0.7) +
  ylab("Bid to Cover Ratio") +
  xlab("Auction Date") +
  ggtitle("2-Year Note Bid to Cover Ratio") +
  theme_bw()
ggsave("./figures/two_year_bid_to_cover.png",
       dpi = 150, width = 5, height = 4, units = "in")

# competitive bids out of total bids over time
ggplot(data = dat) +
  geom_line(aes(x = AuctionDate,
                y = 100 * CompetitiveTendered / TotalTendered), color = 'black', size = 1) +
  ylab("Percent of Total Bids from Competitive Bids") +
  xlab("Auction Date") +
  ggtitle("Percent of Total Bids from Competitive Bids") +
  labs(subtitle = "for 2-Year Notes") +
  theme_bw()

# competitive bids acccepted out of total bids accepted over time
ggplot(data = dat) +
  geom_line(aes(x = AuctionDate,
                y = 100 * CompetitiveAccepted / TotalAccepted), color = 'black', size = 1) +
  ylab("Percent of Accepted Bids that are Competitive") +
  xlab("Auction Date") +
  ggtitle("Percent of Accepted Bids that are Competitive") +
  labs(subtitle = "for 2-Year Notes") +
  theme_bw()
ggsave("./figures/two_year_competitive_bids_accepted.png",
       dpi = 150, width = 5, height = 4, units = "in")

# noncompetitive bids accepted
ggplot(data = dat) +
  geom_line(aes(x = AuctionDate,
                y = 100 * NonCompetitiveAccepted / TotalAccepted),
            color = 'black', size = 1) +
  ylab("Percent of Accepted Bids that are Noncompetitive") +
  xlab("Auction Date") +
  ggtitle("Percent of Accepted Bids that are Noncompetitive") +
  labs(subtitle = "for 2-Year Notes") +
  theme_bw()

# competitive bids acccepted out of total bids accepted over time
#   with breakdown
temp <- dat %>%
  mutate(pd_percent = 100 *( PrimaryDealerAccepted / TotalAccepted)) %>%
  mutate(db_percent = 100 *( DirectBidderAccepted / TotalAccepted)) %>%
  mutate(ib_percent = 100 *( IndirectBidderAccepted / TotalAccepted)) %>%
  mutate(nc_percent = 100 *( NonCompetitiveAccepted / TotalAccepted)) %>%
  mutate(soma_percent = 100 *( SOMAAccepted / TotalAccepted)) %>%
  mutate(fima_percent = 100 *( FIMAAccepted / TotalAccepted)) %>%
  select(AuctionDate, pd_percent, db_percent,
         ib_percent, nc_percent, soma_percent, fima_percent) %>%
  mutate(total_pct = pd_percent + db_percent + ib_percent + nc_percent
                      + soma_percent + fima_percent) %>%
  gather("Bidder", "percent", c(-AuctionDate, -total_pct), factor_key = T)

# change factor level names to look better in final plot
temp$Bidder <- plyr::mapvalues(temp$Bidder,
                               from = c("pd_percent", "db_percent", "ib_percent",
                                        "nc_percent", "soma_percent", "fima_percent"),
                    to = c("Primary Dealer", "Direct Bidder", "Indirect Bidder",
                           "NonCompetitive Bidder", "SOMA", "FIMA"))

ggplot(data = temp) +
  geom_col(aes(x = AuctionDate,
                 fill = Bidder, y = percent),
            color = 'black', size = .01, show.legend = T) +
  # geom_col(aes(x = factor(AuctionDate),
  #               y = 100 * DirectBidderAccepted / TotalAccepted),
  #           color = 'red', size = 1, show.legend = T, position = "stack") +
  # geom_line(aes(x = AuctionDate,
  #               y = 100 * IndirectBidderAccepted / TotalAccepted),
  #           color = 'blue', size = 1, show.legend = T, position = "stack") +
  ylab("Percent of Accepted Bids that are Competitive") +
  xlab("Auction Date") +
  ggtitle("Percent of Accepted Bids from Different Bidders") +
  labs(subtitle = "for 2-Year Notes") +
  scale_fill_brewer(palette = "Set1") +
  theme_bw()
ggsave("./figures/two_year_source_of_bids.png",
       dpi = 150, width = 10, height = 4, units = "in")

# primary dealer bid over total bid
ggplot(data = dat) +
  geom_line(aes(x = AuctionDate,
                y = 100 * PrimaryDealerTendered / CompetitiveTendered),
            color = 'black', size = 1) +
  ylab("Percent Tendered by Primary Dealers (%)") +
  xlab("Auction Date") +
  ggtitle("Percent of Tendered Bids from Primary Dealers") +
  theme_bw()

# primary dealer accepted over total accepted
ggplot(data = dat) +
  geom_line(aes(x = AuctionDate,
                y = 100 * PrimaryDealerAccepted / CompetitiveAccepted),
            color = 'black', size = 0.7) +
  ylab("Percent of Primary Dealer Bids Accepted (%)") +
  xlab("Auction Date") +
  ggtitle("Primary Dealer Takedown \nas a Percentage of Competitive Bids") +
  labs(subtitle = "2-Year Notes") +
  theme_bw()
ggsave("./figures/two_year_primary_dealer_out_of_competitive_accepted.png",
       dpi = 150, width = 5, height = 4, units = "in")

# ratio of primary dealer accepted percentage over primary dealer tendered percentage
ggplot(data = dat) +
  geom_line(aes(x = AuctionDate,
                y = ((PrimaryDealerAccepted / CompetitiveAccepted) /
                  (PrimaryDealerTendered / CompetitiveTendered))),
            color = 'black', size = 1) +
  ylab("Ratio of % Accepted over % Tendered") +
  xlab("Auction Date") +
  ggtitle("Ratio of % Accepted over % Tendered") +
  labs(subtitle = "for Primary Dealer Bids") +
  theme_bw()
