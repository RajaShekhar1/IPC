var agent_inbox = (function() {

  var AgentInboxViewModel = function() {
    var self = this;

    self.envelopes = ko.observableArray(fetch_envelopes());

  };


  function fetch_envelopes() {
    $.ajax("/agents")
  }

  return {
    initialize_inbox: function() {

    }
  }
})();