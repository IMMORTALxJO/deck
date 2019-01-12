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
