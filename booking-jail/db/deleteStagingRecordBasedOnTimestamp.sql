use ojbc_booking_staging; 
delete from CustodyRelease where CustodyReleaseTimestamp > '2016-12-28';

delete from PrescribedMedication where PrescribedMedicationTimestamp > '2016-12-28';
delete from BehavioralHealthAssessmentCategory where BehavioralHealthAssessmentCategoryTimestamp > '2016-12-28';
delete from BehavioralHealthEvaluation where BehavioralHealthEvaluationTimestamp > '2016-12-28';
delete from Treatment where TreatmentTimestamp > '2016-12-28';
delete from BehavioralHealthAssessment where BehavioralHealthAssessmentTimestamp > '2016-12-28';

delete from BookingCharge where BookingChargeTimestamp > '2016-12-28';
delete from BookingArrest where BookingArrestTimestamp > '2016-12-28';

delete from CustodyStatusChangeCharge where CustodyStatusChangeChargeTimestamp > '2016-12-28';
delete from CustodyStatusChangeArrest where CustodyStatusChangeArrestTimestamp > '2016-12-28';
delete from CustodyStatusChange where CustodyStatusChangeTimestamp > '2016-12-28';
delete from Booking where BookingTimestamp > '2016-12-28';
delete from Location where LocationTimestamp > '2016-12-28';
delete from Person where PersonTimestamp > '2016-12-28';


