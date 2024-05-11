# csv2xAPI

Shiny app to send CSV data to an LRS in the xAPI format. The application takes both wide data and long data formats. There are three steps

1. Upload your data. If it is in the wide format, choose which columns to pivot so you have one row per statement
<img width="1463" alt="xapi1" src="https://github.com/sonsoleslp/csv2xAPI/assets/10948559/c525f537-ce6f-4db6-9b90-5a20d4ffdc0b">

2. Map the columns to the xAPI components (actor, verb, object, etc.)
<img width="1446" alt="xapi2" src="https://github.com/sonsoleslp/csv2xAPI/assets/10948559/57078cbc-2443-42f8-976d-aac7215e28cc">

3. Provide the details for your LRS and hit Send!
<img width="1463" alt="xapi3" src="https://github.com/sonsoleslp/csv2xAPI/assets/10948559/6646bc50-0903-4996-a324-edf209d42d53">

You will be able to see the statements on your LRS
