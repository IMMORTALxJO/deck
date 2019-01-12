const checkURL = require("./examples/checkURL.js");
const checkMongo = require("./examples/checkMongo.js");
const output = require("./libs/output.js");

// Create new channel for telegram output
output.add("telegram", msg => {
  const request = require("request");
  const token = "${process.env.TELEGRAM_BOT_TOKEN}";
  const chat_id = "${process.env.TELEGRAM_CHAT_ID}";
  request(
    `https://api.telegram.org/bot${token}/sendMessage?chat_id=${chat_id}&text=${decodeURIComponent(
      msg
    )}`
  );
});

checkMongo("Check mongodb connection", "mongodb://localhost:27017/test", {
  channel: "telegram"
});

checkURL("Check https://google.com", "https://google.com", {
  interval: 30,
  channel: "telegram"
});
