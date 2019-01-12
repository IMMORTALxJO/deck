const Monitor = require("../libs/monitor.js");
const mongoose = require("mongoose");

module.exports = (name, address, opts = {}) => {
  new Monitor(
    async function() {
      try {
        await mongoose.connect(
          address,
          { useNewUrlParser: true, connectTimeoutMS: 2000 }
        );
        return false;
      } catch (err) {
        return err;
      }
    },
    function(err) {
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
          return `FIXED\n${this.meta.name}\nConnection to mongo is UP`;
          break;
        case "alert":
          return `FAILED\n${this.meta.name}\nCouldn't connect to mongo`;
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
