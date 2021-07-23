require('dotenv').config()
const Airtable = require('airtable-node')

const airtable = new Airtable({apiKey : process.env.AIRTABLE_API_KEY})
                     .base(process.env.AIRTABLE_BASE)
                     .table(process.env.AIRTABLE_TABLE)

const fetch_all_dishes =
    async () => {
  const {records} = await airtable.list({maxRecords : 1000})
  console.log("Records", records)
    dishes = records.map((record) => {
        const { id } = record
    const {name, desc, link, tags} = record.fields
        return {
    id, name, desc, link, tags }
    })
        return dishes
}

const get_function =
    async (event) => {
  switch (event.httpMethod) {
  case "GET":
    if (event.path.endsWith("dishes")) {
      console.log("Fetching all dishes")
      dishes = await fetch_all_dishes();
      return { statusCode: 200, body: JSON.stringify(dishes) }
    } else {
      id = event.path.split("/").slice(-1)[0];
      dish = await airtable.retrieve(id)
      return { statusCode: 200, body: JSON.stringify(dish) }
    }
  case "PUT":
    return { statusCode: 200, body: JSON.stringify("This was a put"), }
  }
}

                     exports.handler = async (event, context) => {
  // console.log(event);
  return get_function(event)
}
