# rshiny-stepped-wedge
Visualize a stepped wedge implementation trial

RShiny Task List by Tab:

Intervention: General
- [For Intervention and Graph tabs] Go button. Only update the plot after pressing button.
- Change text box title "How many implementation conditions are there?" to "Number of Implementation Conditions"
- *DEBUG* Head to head trials only work if the two conditions that are head to head are listed next to each other above. (IC2 and IC4 cannot go head to head.)
	Note: This makes sense to me as a user, though this may be a bug we need to fix? I thought of alternatively having a pop-up message say "Error: Head to head conditions are not sequential" if this happens.
- Change numeric input title "Number of Units" to "Number of input$CohortName" if input$CohortName is not null.
- Insert numeric input "Number of Sub-Units". Change title to "Number of input$EntityName" if input$EntityName is not null.
- Confirm ABA design works.

Intervention: Timing
- *DEBUG* Durations of Implementation Conditions cannot exceed 4 without breaking.
- Update Durations of ICs text boxes to also be dynamic, with respect to the number of ICs indicated on Intervention: General (input$num_ICs)
- Duration of ICs boxes look clunky right now. 
	Note: This will be addressed if we do the long df.

Graph: Settings - none
- Make row widths dependent on the number of sub-units (e.g., for a cohort of 20 clinics, width should be double that of one with 10.)
	Note: I don't love this edit, but will show Hendricks it per his request for feedback.

Graph: Download
- Add null values for Directory and Base File Name

ADVANCED
Note: This is where we will allow people to directly edit the long df.

[Not yet created] Home Page
- Create tab.
- Home Page with introduction to the app. Select language.
	Note from Mia: Hendricks was working on building this in other languages. This aspect is likely paused until the app is done.

[Not yet created] Power Calculator
- Create empty tab. To be populated.
- Go button. Only update the calculation after pressing button.

