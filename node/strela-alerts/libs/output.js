const output = {
  outputs: [],
  add: function(channel, func) {
    this.outputs[channel] = func;
  },
  send: function(channels, msg) {
    if (typeof channels == "string") channels = [channels];
    channels.forEach(channel => {
      this.outputs[channel](msg);
    });
  }
};

output.add("info", msg => {
  console.log(`[${new Date().toJSON()}] ${msg}`);
});

module.exports = output;
