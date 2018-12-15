Vue.use(VueMaterial.default);
let APP = new Vue({
  el: '#app',
  data: {
    hosts: {}
  },
  methods: {
    initiate: function(){
      const URL = '/hosts/list';
      this.$http
        .get(URL)
        .then(
          res => {
            res.body.push( location.host );
            res.body.sort( (a,b)=>a.localeCompare(b) ).forEach( hostname=>APP.getContainers(hostname) );
          },
          res => {
            console.error(res);
            alert(`Could not GET ${URL}`);
          },
        );
    },
    getContainers: function(hostname){
      if( hostname.indexOf(':') < 0 )
        hostname += `:${location.port}`;
      const URL = `http://${hostname}/containers/list`;
      this.$http
        .get(URL)
        .then(
          res => {
            this.$set( this.hosts, hostname, res.body )
          },
          res => {
            console.error(res);
            alert(`Could not GET ${URL}`);
          }
        );
    },
    stopContainer: function( hostname, container ){
      const URL = `http://${hostname}/containers/stop/${container.id}`;
      this.$http
        .get(URL)
        .then(
          res => {
            this.$set( this.hosts, hostname, res.body )
          },
          res => {
            console.error(res);
            alert(`Could not GET ${URL}`);
          }
        );
    },
    startContainer: function( hostname, container ){
      const URL = `http://${hostname}/containers/start/${container.id}`;
      this.$http
        .get(URL)
        .then(
          res => {
            this.$set( this.hosts, hostname, res.body )
          },
          res => {
            console.log(res);
            alert(`Could not GET ${URL}`);
          }
        );
    }
  },
  created: function() {
    this.initiate()
  },
});
