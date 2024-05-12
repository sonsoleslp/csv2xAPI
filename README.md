# csv2xAPI

Shiny app to send CSV data to an LRS in the xAPI format. The application takes both wide data and long data formats. There are three steps

1. Upload your data (you can use the `survey.csv` file provided as example on the `sample-data` folder). If your is in the wide format (like in the example), choose which columns to pivot so you have one row per statement
<img width="1463" alt="xapi11" src="https://github.com/sonsoleslp/csv2xAPI/assets/10948559/43da8910-7c64-46e2-8fa1-4fc94f6d346b">

2. Map the columns to the xAPI components (actor, verb, object, etc.)
<img width="1463" alt="xapi12" src="https://github.com/sonsoleslp/csv2xAPI/assets/10948559/9533ce4c-41c7-4aef-aaeb-d50576a6c499">

3. Provide the details for your LRS and hit Send!
<img width="1463" alt="xapi13" src="https://github.com/sonsoleslp/csv2xAPI/assets/10948559/be3cdb4b-710e-4f37-a5cc-133812255280">

You will be able to see the statements on your LRS
