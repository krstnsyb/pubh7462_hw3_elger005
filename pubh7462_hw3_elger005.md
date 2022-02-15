PUBH 7462 Homework 3
================
Kristin Elgersma
2/17/22

-   [Problem 3. Instacart](#problem-3-instacart)

# Problem 3. Instacart

``` r
# Read data
instacart <- read_csv("./data/instacart.csv")
```

## 3.1 Summary Table

``` r
# clean variable names
instacart <- janitor::clean_names(instacart)

instacart <- instacart %>%
  rename( # shorter names for some variables
    cart_order = add_to_cart_order,
    order_hour = order_hour_of_day,
    days_prior = days_since_prior_order
  )

# make factors
instacart$order_id <- as.factor(instacart$order_id)
instacart$product_id <- as.factor(instacart$product_id)
instacart$user_id <- as.factor(instacart$user_id)
instacart$aisle <- as.factor(instacart$aisle)
instacart$department <- as.factor(instacart$department)
instacart$aisle_id <- as.factor(instacart$aisle_id)
instacart$department_id <- as.factor(instacart$department_id)

# create data set
instacart2 <- instacart %>%
  group_by(order_id) %>%
  mutate(pct_reorder = (mean(reordered) * 100)) %>% # create % reordered variable
  group_by(order_id, order_number, days_prior, pct_reorder) %>%
  summarise(
    across(
      c(product_id, aisle_id, department_id),
      n_distinct
    )
  ) %>% # count the number of products, aisles, and departments for each order
  ungroup() %>%
  summarise(
    across(
      c(2:7),
      list(mean = mean, sd = sd, median = median), # get mean, median, sd for each variable of interest
      na.rm = TRUE,
      .names = "{.col}_{.fn}"
    )
  ) %>%
  pivot_longer(cols = c(1:18), names_to = "measure", values_to = "value") %>%
  separate(measure, c("A", "B", "C")) %>% # separate out the variable name and measure
  unite(name, c(A, B), sep = "_", remove = TRUE) %>% # reunite the variable name
  rename(measure = C) %>%
  pivot_wider(names_from = measure, values_from = c(value)) %>%
  mutate(
    name = factor(name),
    item = fct_recode(name, # rename the factor levels to read better in the table
      "Order number" = "order_number",
      "Days since last order" = "days_prior",
      "Percent of items reordered" = "pct_reorder",
      "Number of items purchased" = "product_id",
      "Number of aisles" = "aisle_id",
      "Number of departments" = "department_id"
    )
  )

# create the table
table1 <- instacart2 %>%
  select(item, mean, sd, median) %>%
  gt() %>%
  tab_header("Table 1. Mean, SD, and median of Instacart data") %>%
  cols_align(
    align = c("left"),
    columns = item
  ) %>%
  cols_align(
    align = c("center"),
    columns = c(mean, sd, median)
  ) %>%
  cols_label(
    item = " ",
    mean = "Mean",
    sd = "SD",
    median = "Median"
  )

# save table image
gtsave(table1, "table1.png")
```

<img src="pubh7462_hw3_elger005_files/figure-gfm/unnamed-chunk-2-1.png" style="display: block; margin: auto;" />

## 3.2 Visual display of number of orders

``` r
# create df with number of orders
instacart2 <- instacart %>%
  group_by(aisle) %>%
  summarise(n = n_distinct((order_id))) %>%
  mutate(aisle = str_to_title(aisle)) %>%
  mutate(aisle = as.factor(aisle)) %>%
  mutate(aisle = (fct_reorder(aisle, n)))

# Visualization
instacart2 %>%
  ggplot(aes(x = n, y = aisle, fill = n)) +
  geom_col() +
  scale_fill_gradientn( # choose custom colors and cut points for color scale
    name = "Number of orders",
    colors = c("gray", "skyblue4", "goldenrod", "orchid"),
    values = scales::rescale(c(0, 2000, 2001, 7000, 7001, 11000, 11001, max(instacart2$n)))
  ) +
  labs(
    title = "Figure 1. Number of Instacart orders per aisle",
    x = "Number of orders",
    y = "Aisle"
  ) +
  theme_minimal() +
  theme(legend.position = "none")
```

<img src="pubh7462_hw3_elger005_files/figure-gfm/unnamed-chunk-3-1.png" style="display: block; margin: auto;" />

## 3.3 Visualization of the top 6 aisles in the top 6 departments by items purchased

``` r
# get top 6 departments
insta_dep6 <- instacart %>%
  group_by(department) %>%
  summarise(dept_tot = n()) %>%
  arrange(desc(dept_tot)) %>%
  slice(1:6) %>%
  ungroup()

# get top 6 aisles
insta_aisle6 <- instacart %>%
  filter(department %in% insta_dep6$department) %>% # filter departments to only include those in the top 6
  group_by(department, aisle) %>%
  summarise(aisle_tot = n()) %>%
  arrange(desc(aisle_tot)) %>%
  slice(1:6) %>% # keep only top 6 aisles
  mutate(
    aisle = str_to_title(aisle),
    department = str_to_title(department)
  ) %>%
  ungroup() %>%
  mutate(
    aisle = fct_reorder(aisle, aisle_tot),
    department = fct_reorder(department, desc(aisle_tot))
  )


# Visualization
insta_aisle6 %>%
  ggplot(aes(x = aisle_tot, y = aisle, fill = department)) +
  geom_col() +
  facet_wrap(~department, scales = "free_y") +
  scale_fill_paletteer_d("ggthemes::excel_Feathered") +
  theme_minimal() +
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5)) +
  labs(
    title = "Figure 2. Top 6 departments with the highest number of items purchased and top 6 aisles in each department.",
    y = "Aisles",
    x = "Number of items purchased"
  )
```

<img src="pubh7462_hw3_elger005_files/figure-gfm/unnamed-chunk-4-1.png" style="display: block; margin: auto;" />

## 3.4 Table of top 5 aisles by items purchased and top 5 items purchased in these aisles

``` r
# get top 5 aisles
insta_aisle5 <- instacart %>%
  group_by(aisle) %>%
  summarise(aisle_tot = n()) %>%
  arrange(desc(aisle_tot)) %>%
  slice(1:5)

# get top 5 products
insta_prod5 <- instacart %>%
  filter(aisle %in% insta_aisle5$aisle) %>% # filter aisles to only include those in the top 5
  group_by(aisle, product_name) %>%
  summarise(product_tot = n()) %>%
  arrange(desc(product_tot)) %>%
  slice(1:5) %>% # keep only top 5 products
  ungroup() %>%
  mutate(
    aisle = str_to_title(aisle), # clean up titles
    product_name = str_to_title(product_name),
    product_name = fct_reorder(product_name, desc(product_tot)), # reorder factors highest to lowest
    aisle = fct_reorder(factor(aisle), desc(product_tot))
  )

# create the table
table2 <- insta_prod5 %>%
  group_by(aisle) %>%
  arrange(aisle) %>%
  gt(rowname_col = "product_name", groupname_col = "aisle") %>%
  tab_style(
    style = list(
      cell_fill("lightgray"),
      cell_text(color = "black", weight = "bold")
      ),
    locations = cells_row_groups()) %>%
   tab_style(
     cell_text(indent = 25),
     locations = cells_stub()) %>%
  tab_header("Table 2. Top 5 aisles with the highest number of items purchased and top 5 products in each department.") %>%
  data_color(
    columns = product_tot,
    colors = scales::col_numeric(
      palette = c("white", "skyblue4"),
      domain  = c(0, 19000)
    )
  ) %>%
  cols_label(
    product_tot = "No. Items",
    product_name = "Product Name"
  )

# save table image
gtsave(table2, "table2.png")
```

<img src="pubh7462_hw3_elger005_files/figure-gfm/unnamed-chunk-5-1.png" style="display: block; margin: auto;" />
