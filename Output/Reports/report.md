Rate of Acclimation in Skistodiaptomus pallidus
================
2026-02-18

- [Preliminary Trial](#preliminary-trial)

## Preliminary Trial

The temperatures averages and standard deviations from the two
incubators are shown in this table.

``` r
acc_windows = ctmax_data %>% 
  group_by(exp_rep) %>% 
  summarise(start_time = min(datetime), 
            end_time = max(datetime))

# To do: figure out a way to filter the data based on the acc_windows object; each exp_rep has a separate start and end time that should be used
inc_temps %>% 
  filter(datetime > acc_windows$start_time & datetime < acc_windows$end_time) %>%  
  group_by(incubator_temp) %>% 
  summarise(mean_temp = mean(temp_c), 
            temp_sd = sd(temp_c)) %>% 
  knitr::kable(digits = 2)
```

| incubator_temp | mean_temp | temp_sd |
|---------------:|----------:|--------:|
|             16 |     15.28 |    0.39 |
|             22 |     22.42 |    0.68 |

While the average temperature was closer to the intended temperature in
the 22°C incubator, temperature was more variable than in the 16°C
incubator.

``` r

# Eventually will want to convert datetime into 'time since start' or something like that so all the reps can be overlaid
inc_temps %>% 
  filter(datetime > acc_windows$start_time & datetime < acc_windows$end_time) %>%  
  mutate(inc_id = paste(exp_rep, incubator_id, sep = "_")) %>% 
  ggplot(aes(x = datetime, y = temp_c, colour = factor(incubator_temp), group = inc_id)) + 
  geom_hline(yintercept = c(16,22)) + 
  geom_line(linewidth = 2) + 
  scale_colour_manual(values = c("royalblue", "brown2")) + 
  labs(y = "Temperature (°C)", 
       x = "Date", 
       colour = "Incubator \nTemp. (°C)") + 
  theme_bw(base_size = 25) + 
  theme(panel.grid = element_blank())
```

<img src="../Figures/markdown/unnamed-chunk-2-1.png" alt="" style="display: block; margin: auto;" />

Thermal limit data is shown below. The larger points indicate the mean
for each treatment group on each acclimation day, with the raw data
shown as lighter points in the background. A general trend of increasing
thermal limits in the warming acclimation group is present, but there is
a fair amount of variation. That being said, it is too early to make any
conclusions about specific patterns.

``` r
ctmax_data %>% 
  group_by(pop, treatment, acc_day) %>% 
  summarise(mean_ctmax = mean(ctmax)) %>% 
ggplot(aes(x = acc_day, y = mean_ctmax, colour = treatment)) + 
  facet_wrap(pop~.) + 
  geom_point(data = ctmax_data, aes(y = ctmax),
             alpha = 0.3) + 
  geom_point(size = 3) + 
  geom_line(linewidth = 1.5) + 
  scale_colour_manual(values = c("control" = "royalblue",
                                 "warming" = "brown2")) + 
  labs(x = "Acclimation Day", 
       y = "CTmax (°C)") + 
  theme_matt_facets()
```

<img src="../Figures/markdown/unnamed-chunk-3-1.png" alt="" style="display: block; margin: auto;" />

We will be using a linear model to analyze the data, and to examine
specific patterns in the changes in thermal limits over time. For now,
we use a simple linear regression model: CTmax as a function of
treatment, population, and acclimation day (with all possible
interactions). This model is limited by the small sample size, but
performs reasonably well.

``` r
# this is an initial version of the model; later versions will include experimental replicate and incubator as random effects

model_data = ctmax_data %>% 
  mutate(acc_day = as.factor(acc_day))

prelim.model = lm(ctmax ~ acc_day * treatment * pop, data = model_data)

performance::check_model(prelim.model)
```

<img src="../Figures/markdown/unnamed-chunk-4-1.png" alt="" style="display: block; margin: auto;" />

The model indicates a significant effect of acclimation time, treatment,
and population, along with a significant interaction between acclimation
time and treatment.

``` r
car::Anova(prelim.model) %>% knitr::kable()
```

|                       |    Sum Sq |  Df |    F value |   Pr(\>F) |
|:----------------------|----------:|----:|-----------:|----------:|
| acc_day               | 1.7878421 |   6 |  2.9698374 | 0.0170963 |
| treatment             | 2.9444803 |   1 | 29.3469794 | 0.0000031 |
| pop                   | 4.5608306 |   1 | 45.4567834 | 0.0000000 |
| acc_day:treatment     | 2.3568532 |   6 |  3.9150386 | 0.0036055 |
| acc_day:pop           | 1.3722313 |   6 |  2.2794540 | 0.0550298 |
| treatment:pop         | 0.0202361 |   1 |  0.2016886 | 0.6557852 |
| acc_day:treatment:pop | 0.9251172 |   5 |  1.8440874 | 0.1261873 |
| Residuals             | 4.0133333 |  40 |         NA |        NA |

We’ll calculate estimated marginal means for each treatment group on
each acclimation day. These marginal means can then be used to calculate
pairwise contrasts between the control and warming groups (CTmax in the
22°C group - CTmax in the 16°C group). As such, a positive contrast
indicates an increase in CTmax in the warming group relative to the
control group.

As for other analyses, it’s still too early to draw any major
conclusions from these results. It seems, however, like 6 days may not
be enough for complete acclimation and that there may be differences
between the two populations.

``` r
emmeans::emmeans(prelim.model, ~ treatment | pop * acc_day) %>% 
  emmeans::contrast("revpairwise") %>% as_tibble() %>% 
  mutate(acc_day = as.numeric(as.character(acc_day))) %>% 
  ggplot(aes(x = acc_day, y = estimate)) + 
  facet_wrap(pop~.) + 
  geom_hline(yintercept = 0) +
  geom_errorbar(aes(ymin = estimate - SE, ymax = estimate + SE), 
                linewidth = 1, width = 0.3) + 
  geom_point(size = 3) + 
  geom_smooth(method = "lm") + 
  labs(x = "Acc. Day", 
       y = "Contrast (Warming - Control; °C)") + 
  theme_matt_facets()
```

<img src="../Figures/markdown/unnamed-chunk-6-1.png" alt="" style="display: block; margin: auto;" />
