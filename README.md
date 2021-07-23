# kitchenhelper
Simple crud website for storing and searching for recipes written in elm

Installation

```
npm install
npm run build
```

After that the site is available under public/index.html

You can run the application locally using:
```
npm run netlify
```

To make it work with persistance you would need to create an accound and DB on airtable.com and create an .env file (in main directory) with following content:
```
AIRTABLE_API_KEY=mysecretairtablekey
AIRTABLE_BASE=appmybasename
AIRTABLE_TABLE=mytablename
```
Remember to not commit the file.
