drop table if exists FullBookingView;

create table FullBookingView as
select
    JailEpisode.JailEpisodeID,
    BookingNumber,
    EpisodeStartDate,
    EpisodeEndDate,
    ifnull(JailEpisodeArrest.JailEpisodeArrestID, 99998) as JailEpisodeArrestID,
    ifnull(JailEpisodeCharge.JailEpisodeChargeID, 99998) as JailEpisodeChargeID,
    ifnull(JailEpisodeArrest.AgencyID, 99998) as ArrestAgencyID,
    ifnull(JailEpisodeCharge.AgencyID, 99998) as ChargeAgencyID,
    ifnull(ChargeClassTypeID, 99998) as ChargeClassTypeID,
    ifnull(BondStatusTypeID, 99998) as BondStatusTypeID,
    ifnull(BondTypeID, 99998) as BondTypeID,
    ifnull(ChargeTypeID, 99998)as  ChargeTypeID, 
    ifnull(ChargeDispositionTypeID, 99998) as ChargeDispositionTypeID, 
    ifnull(JurisdictionTypeID, 99998) as JurisdictionTypeID, 
    BondAmount,
    OccupationTypeID,
    SexOffenderStatusTypeID,
    FacilityID,
    SupervisionUnitTypeID,
    CaseStatusTypeID,
    EpisodeStartDateID,
    PopulationTypeID,
    LengthOfStay,
    OneYearRebooking,
    PersonAgeTypeID,
    PersonSexTypeID,
    PersonRaceTypeID,
    PersonEthnicityTypeID,
    LanguageTypeID,
    MilitaryServiceStatusTypeID,
    DomicileStatusTypeID,
    WorkReleaseStatusTypeID,
    EducationLevelTypeID,
    ProgramEligibilityTypeID,
    ifnull(BehavioralHealthEvaluationID, 99998) as BehavioralHealthEvaluationID,
    ifnull(BehavioralHealthTreatmentID, 99998) as BehavioralHealthTreatmentID,
    ifnull(MedicaidStatusTypeID, 99998) as MedicaidStatusTypeID,
    ifnull(InTreatmentAtBooking, 'N') as InTreatmentAtBooking,
    BehavioralHealthAssessment.EndedDaysBeforeBooking,
    ifnull(BehavioralHealthEvaluationTypeID, 99998) as BehavioralHealthEvaluationTypeID,
    ifnull(MedicationTypeID, 99998) as MedicationTypeID,
    ifnull(AssessmentCategoryTypeID, 99998) as AssessmentCategoryTypeID,
    ifnull(TreatmentStatusTypeID, 99998) as TreatmentStatusTypeID,
    ifnull(TreatmentAdmissionReasonTypeID, 99998) as TreatmentAdmissionReasonTypeID,
    ifnull(TreatmentProviderTypeID, 99998) as TreatmentProviderTypeID,
    SevereMentalIllnessIndicator,
    ifnull(SevereMentalIllnessIndicator, 99998) as SevereMentalIllnessIndicatorDimension,
    BehavioralHealthTreatment.DaysBeforeBooking,
    ifnull(PrescribedMedicationID,99998) as PrescribedMedicationID,
    if(SixMonthRebooking='Y', 1, 0) * JailEpisode.JailEpisodeID as SixMonthRebookingCount, 
    if(OneYearRebooking='Y', 1, 0) * JailEpisode.JailEpisodeID as OneYearRebookingCount, 
    if(TwoYearRebooking='Y', 1, 0) * JailEpisode.JailEpisodeID as TwoYearRebookingCount, 
    if(IsActive='Y', 1, 0) * JailEpisode.JailEpisodeID as IsActiveBookingCount,
    if(InTreatmentAtBooking='Y', 1, 0) * JailEpisode.JailEpisodeID as InTreatmentAtBookingCount,
    if(SevereMentalIllnessIndicator=1, 1, 0) * JailEpisode.JailEpisodeID as SevereMentalIllnessBookingCount
from
    JailEpisode inner join Person on JailEpisode.PersonID=Person.PersonID
    left join JailEpisodeArrest on JailEpisode.JailEpisodeID=JailEpisodeArrest.JailEpisodeID
    left join JailEpisodeCharge on JailEpisodeArrest.JailEpisodeArrestID=JailEpisodeCharge.JailEpisodeArrestID
    left join BehavioralHealthAssessment on BehavioralHealthAssessment.PersonID=Person.PersonID
    left join BehavioralHealthEvaluation  on BehavioralHealthAssessment.BehavioralHealthAssessmentID=BehavioralHealthEvaluation.BehavioralHealthAssessmentID
    left join BehavioralHealthAssessmentCategory on BehavioralHealthAssessment.BehavioralHealthAssessmentID=BehavioralHealthAssessmentCategory.BehavioralHealthAssessmentID
    left join PrescribedMedication on BehavioralHealthAssessment.BehavioralHealthAssessmentID=PrescribedMedication.BehavioralHealthAssessmentID
    left join BehavioralHealthTreatment  on BehavioralHealthAssessment.BehavioralHealthAssessmentID=BehavioralHealthTreatment.BehavioralHealthAssessmentID;