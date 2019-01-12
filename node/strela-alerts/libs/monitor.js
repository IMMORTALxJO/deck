const output = require("./output.js");

module.exports = class {
  constructor(collector, analyzer, message, opts = {}) {
    this.collector = collector;
    this.analyzer = analyzer;
    this.message = message;
    this.opts = { interval: 10, fails_for_alert: 3, ...opts };
    this.meta = {
      fail_started: false,
      fails_in_raw: 0
    };
  }

  start(name, opts = {}) {
    this.meta.name = name;
    this.opts = { ...this.opts, ...opts };
    setInterval(this.iteration.bind(this), this.opts.interval * 1000);
    this.iteration.call(this);
  }

  async iteration() {
    output.send(
      "info",
      `CHECK: ${this.meta.name}, fails = ${this.meta.fails_in_raw}`
    );
    try {
      let collected_data = await this.collector();
      let need_alert = this.analyzer(collected_data);
      let status = "good";
      if (need_alert) {
        status = "fail";
        if (this.meta.fails_in_raw == 0) {
          status = "first_fail";
          this.meta.fail_started = new Date();
        }
        if (this.meta.fails_in_raw == this.opts.fails_for_alert) {
          status = "alert";
          output.send("info", this.message("alert", collected_data));
        }
        this.meta.fails_in_raw++;
      } else {
        status = "success";
        if (this.meta.fails_in_raw >= this.opts.fails_for_alert) {
          status = "fixed";
          output.send("info", this.message("fixed", collected_data));
        }
        this.meta.fails_in_raw = 0;
      }
      const message = this.message(status, collected_data);
      if (message !== false) output.send(this.opts.channel, message);
    } catch (err) {
      console.error(err);
    }
  }
};
