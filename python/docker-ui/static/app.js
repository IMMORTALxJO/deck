Vue.use(VueMaterial.default);
let APP = new Vue({
  el: '#app',
  data: {
    hosts: {},
    showLoading: false
  },
  methods: {
    initiate: function(){
      const URL = '/hosts/list';
      this.showLoading = true;
      this.$http
        .get(URL)
        .then(
          res => {
            this.showLoading = false;
            res.body.push( location.host );
            res.body.sort( (a,b)=>a.localeCompare(b) ).forEach( hostname=>APP.getContainers(hostname) );
          },
          res => {
            this.showLoading = false;
            console.error(res);
            alert(`Could not GET ${URL}`);
          },
        );
    },
    getContainers: function(hostname){
      if( hostname.indexOf(':') < 0 )
        hostname += `:${location.port}`;
      const URL = `http://${hostname}/containers/list`;
      this.showLoading = true;
      this.$http
        .get(URL)
        .then(
          res => {
            this.showLoading = false;
            this.$set( this.hosts, hostname, res.body )
          },
          res => {
            this.showLoading = false;
            console.error(res);
            alert(`Could not GET ${URL}`);
          }
        );
    },
    stopContainer: function( hostname, container ){
      const URL = `http://${hostname}/containers/stop/${container.id}`;
      this.showLoading = true;
      this.$http
        .get(URL)
        .then(
          res => {
            this.showLoading = false;
            this.$set( this.hosts, hostname, res.body )
          },
          res => {
            this.showLoading = false;
            console.error(res);
            alert(`Could not GET ${URL}`);
          }
        );
    },
    startContainer: function( hostname, container ){
      const URL = `http://${hostname}/containers/start/${container.id}`;
      this.showLoading = true;
      this.$http
        .get(URL)
        .then(
          res => {
            this.showLoading = false;
            this.$set( this.hosts, hostname, res.body )
          },
          res => {
            this.showLoading = false;
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
