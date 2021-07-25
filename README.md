# using-catapultR

Info about the package can be found in:
http://catapultr.catapultsports.com/

This is an example of how catapultR package is used in practice in a professional rugby union club competing in the English Gallagher Premiership.

NB: catapultR is available exclusively for Catapult Sports clients.

The current work flow is: Openfield Console --> Openfield Cloud --> catapultR --> Power Bi.

I. Openfield Console:
This phase occurs during the training session when the sport scientist is live coding blocks of the session and tagging if players are "Full" or "Modified" for the whole session and for the activity, whether a period is part of "Full session" or "Extras, and the Day code is added in the console as well.

II. Openfield Cloud:
This phase occurs after the training session. The tags "Week", "Opposition", "Season" are added to the activites and the tag "Squad" is added to the players in the activtiy.

III. catapultR:
The R script is then run, and data are saved in a .csv file or in a table in MySQL.

IV. Power Bi:
Data are then visualised with MS Power Bi.

This is an example of how the package is used in practice. It would be interesting to hear how other teams / practitioners utilise this package.
