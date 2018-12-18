Vue.use(VueMaterial.default);
let APP = new Vue({
  el: "#app",
  data: {
    showLoading: false,
    containers: [],
    searched: [],
    search: ''
  },
  methods: {
    initiate: function() {
      const URL = "/hosts/list";
      this.showLoading = true;
      this.$http.get(URL).then(
        res => {
          res.body.push(location.host);
          res.body.forEach(host => APP.getContainers(host, ()=>this.searched=this.containers ));
        },
        res => this.errorCatch
      );
    },
    getContainers: function(host, clbk=false) {
      if (host.indexOf(":") < 0) host += `:${location.port}`;
      const URL = `http://${host}/containers/list`;
      this.showLoading = true;
      this.$http.get(URL).then(
        res => {
          this.updateContainers( host, res.body);
          if( clbk )
            clbk();
          this.showLoading = false;
        },
        res => this.errorCatch
      );
    },
    actionContainer: function(action, container) {
      const URL = `http://${container.host}/containers/${action}/${container.longId}`;
      this.showLoading = true;
      this.$http.get(URL).then(
        res => {
          this.updateContainers(container.host, res.body);
          this.showLoading = false;
        },
        res => this.errorCatch
      );
    },
    updateContainers: function(host, containers ){
      this.containers = this.containers.filter( container => container.host != host );
      containers.forEach( container=>{
        container['host'] = host;
        this.containers.push( container );
      });
      this.searchTable();
    },
    errorCatch: function(res) {
      this.showLoading = false;
      console.error(res);
      alert(`Could not GET ${URL}`);
    },
    searchTable: function(){
      const searchItem = this.search.toLowerCase();
      this.searched = ( searchItem == '' ) ?
        this.containers :
        this.containers.filter( container=>{
          return container.host.toLowerCase().includes( searchItem ) ||
          container.name.toLowerCase().includes( searchItem ) ||
          container.longId.toLowerCase().includes( searchItem );
        });
    }
  },
  created: function() {
    this.initiate();
  }
});
