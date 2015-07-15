#
#Feature: Import enrollment data to produce an imaged form.
#  In order to produce imaged forms from raw enrollment data,
#  As an API User,
#  I want to submit my enrollment data to Take-An-App.
#
#Scenario: User submits minimum valid enrollment data.
#  Given I prepare an enrollment file with basic valid enrollment data for the 'FPPTI' product
#  When I submit the file to the Enrollment API
#  Then I should see a PDF created with the following data:
#    |
