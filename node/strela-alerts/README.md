### Small tools for alerts and notifications

##### Create own monitor function

Example of simple HTTP monitor for checking URL

```
const Monitor = require("../libs/monitor.js");
const request = require("request-promise");

module.exports = (name, url, opts = {}) => {
  new Monitor(
    async function() {
      try {
        const data = await request(url);
        return [false, data];
      } catch (err) {
        return [err, false];
      }
    },
    function(input) {
      const [err, data] = input;
      if (err) return true;
      return false;
    },
    function(status, collected) {
      //      fail
      //      first_fail
      //      alert
      //      success
      //      fixed
      switch (status) {
        case "fixed":
          return `FIXED\nSite is UP${this.meta.name}`;
          break;
        case "failed":
          return `FAILED\nSite is DOWN ${this.meta.name}`;
          break;
      }
      return false;
    },
    {
      interval: 10,
      fails_for_alert: 3,
      channel: "info"
    }
  ).start(name, opts);
};
```

Use new monitor:

```
const checkURL = require("./examples/checkURL.js");
// Check with default parameters
checkURL("Check https://google.com", "https://google.com");

// Check with custom parameters
checkURL("Check https://instagram.com", "https://instagram.com", {
  interval: 30,
  fails_for_alert: 5
});
```

##### Create own output channel

Example of telegram output:

```
const output = require("./libs/output.js");
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
```

Use new output:

```
checkURL("Check https://instagram.com", "https://instagram.com", {
  channel: "telegram"
});
```
