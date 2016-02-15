
// Simple container for replacement policy details
function ReplacementPolicy() {
    var self = this;

    self.name = ko.observable('');
    self.policy_number = ko.observable('');
    self.insured = ko.observable('');
    self.replaced_or_financing = ko.observable(null);
    self.replacement_reason = ko.observable("");

    self.serialize = function() {
        return {
            name: self.name(),
            policy_number: self.policy_number(),
            insured: self.insured(),
            replaced_or_financing: self.replaced_or_financing(),
            replacement_reason: self.replacement_reason()
        };
    };
}