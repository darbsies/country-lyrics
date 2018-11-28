import billboard
import json
import datetime
import csv

outfilename = 'hot_output.csv'
num_weeks = 260

chart_type = 'hot-100'
chart = billboard.ChartData(chart_type)
chart_date = '2018-12-01'

w = csv.writer(open(outfilename,"w"))
w.writerow(['date', 'title', 'artist', 'weeks', 'current', 'peak'])

for i in range (1,num_weeks+1):
    for position in range (0,100):
        song = chart[position]
        date = unicode(str(chart_date))
        title = "\"" + unicode(song.title) + "\""
        artist = "\"" + unicode(song.artist) + "\""
        weeks = str(song.weeks)
        current = str(position + 1)
        peak = str(song.peakPos)
        w.writerow([date, title, artist, weeks, current, peak])

    chart_date = chart.previousDate
    chart = billboard.ChartData(chart_type, str(chart.previousDate))
print 'done'



