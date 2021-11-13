Ontario Tech University  
INFR 4350: HCI For Games Fall 2021  
Group Assignment #2  

To submit: R project with code and a report in pdf.  
Due: November 12, 2021, 11:59pm (Late penalty 20% per day)

1) `[50marks]`

    Consider the following data generated by R:

    ```r
    set.seed(5)
    my_data <- data.frame(
       immersion = c(rnorm(12, 70, 10), rnorm(12, 75, 10), rnorm(12, 100, 10)),
       group = gl(3, 12, labels = c("sitting", "standing", "walking"))
    )
    my_data
    ```

    This data represents results from a fake user study, where three groups of participants used VR while doing a task in three different configurations: sitting, standing and walking. Each participant only did one configuration, so the results are independent. The participants rated their subjective feeling of immersion in a hypothetical questionnaire, which generated a score from 0 to 120. Pretend that this immersion score is what is stored in this data frame.

    Run proper statistical tests, which would not inflate the type I error, to reveal if any groups rated immersion differently. If there are differences you also need to identify which of them are significant.

    Make sure to follow a formal procedure of performing all the assumptions tests, and visualizing the data using an appropriate plot.If there are outliers remove them. It’s ok to do this since the sets are independent.

    Report on the findings as shown in class. Make sure to refer to an appropriate figure, which should accompany these results. You will have to generate it in R.

2) `[50 marks]`

    The System Usability Scale (SUS) provides a “quick and dirty”, reliable tool for measuring the usability. It consists of a 10-item questionnaire with five response options for respondents; from Strongly agree to Strongly disagree.Originally created by John Brooke in 1986, it allows you to evaluate a wide variety of products and services, including hardware, software, mobile devices, websites and applications.

    A SUS score can be calculated from the responses of the SUS questionnaire. The scores range from 0 to 100. A SUS score of 68 is considered to be average (see [Measuring Usability with the System Usability Scale (SUS)](https://measuringu.com/sus/) on [measuringu.com](https://measuringu.com/)), so anything above this score is considered above average, and anything below this score is below average.

    The attached [SUS file](SUS.csv) contains data from an experiment where two game development tools were rated. The dependent variable is `Score`. The independent variables are `Order` and `Tool`. `Order` is between-subject. `Tool` is within-subject. As a result, this is a mixed-design study.

    In this experiment, half of the participants rated the tools in the order of `A->B`, and the other half in the order of `B->A`. So this is how the participants differed: they were split in two groups.

    At the same time, the participants did the same tasks in both groups.

    1) Run the assumptions tests that apply, to make sure the data would work with this type of ANOVA. If there are any violations, acknowledge them and state that the reliability of this test will be reduced.
    2) Run the mixed ANOVA using ezANOVA as follows:

        ```r
        M_AnovaModel <- ezANOVA(data = SUS, dv = .(Score), wid = .(Participants), within = .(Tool), between =.(Order), detailed = T, type = 3)
        ```

    3) Draw the interaction plot(s).
    4) Write the report that includes the results of this ANOVA, pairwise post-hoc tests, and the interaction plot(s).
    5) Do you think the order effect matters in this study? Explain what you would have done to avoid this effect (regardless whether you think it matters or not).
