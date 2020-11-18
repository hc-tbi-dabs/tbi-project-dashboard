# Technology and Business Innovation Branch
## Project Portfolio Management Dashboard

**Built with R Shiny.**

<br>
<br>
<p align="center">
  <img src="./ppm-logo.png">
</p>
<br>
<br>

TBI Project Portfolio Management Dashboard (TBI PPMD) displays information on schedule, milestones, budget, stage and project health for all IP and SF projects at Technology and Business Innovation Branch (TBI). 

The application consists of:

- ui.R: User interface of the application.

- server.R: Server components of the application.

- global.R: Libraries and data needed for the application.

- functions.R: Extra functions.

TBI PPMD

## Updating data:

There are some tricks to getting new data files to work.

1. You must rename some of the columns so the column names do not contain any new line characters.

2. In the Schedule tab you need to change some of the Schedule.Health.Standard entries to say one of either:
  - 3
  - 4.5
  - 6
  - completed
