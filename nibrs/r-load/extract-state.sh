awk '{ if (substr($0, 101, 2) == "OH") print $0 }' < /opt/data/NIBRS/2013/ICPSR_36121/DS0001/36121-0001-Data.txt > /opt/data/NIBRS/2013/ICPSR_36121/DS0001/Ohio.txt
awk '{ if (substr($0, 101, 2) == "OH") print $0 }' < /opt/data/NIBRS/2013/ICPSR_36121/DS0002/36121-0002-Data.txt > /opt/data/NIBRS/2013/ICPSR_36121/DS0002/Ohio.txt
awk '{ if (substr($0, 101, 2) == "OH") print $0 }' < /opt/data/NIBRS/2013/ICPSR_36121/DS0003/36121-0003-Data.txt > /opt/data/NIBRS/2013/ICPSR_36121/DS0003/Ohio.txt
awk '{ if (substr($0, 101, 2) == "OH") print $0 }' < /opt/data/NIBRS/2013/ICPSR_36121/DS0004/36121-0004-Data.txt > /opt/data/NIBRS/2013/ICPSR_36121/DS0004/Ohio.txt
