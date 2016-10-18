drop table if exists FullBookingView;
select
    JailEpisode.JailEpisodeID,
    BookingNumber,
    EpisodeStartDate,
    EpisodeEndDate,
    isnull(JailEpisodeArrest.JailEpisodeArrestID, 99998) as JailEpisodeArrestID,
    isnull(JailEpisodeCharge.JailEpisodeChargeID, 99998) as JailEpisodeChargeID,
    isnull(JailEpisodeArrest.AgencyID, 99998) as ArrestAgencyID,
    isnull(JailEpisodeCharge.AgencyID, 99998) as ChargeAgencyID,
    isnull(ChargeClassTypeID, 99998) as ChargeClassTypeID,
    isnull(BondStatusTypeID, 99998) as BondStatusTypeID,
    isnull(BondTypeID, 99998) as BondTypeID,
    isnull(ChargeTypeID, 99998)as  ChargeTypeID, 
    isnull(ChargeDispositionTypeID, 99998) as ChargeDispositionTypeID, 
    isnull(JurisdictionTypeID, 99998) as JurisdictionTypeID, 
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
    isnull(BehavioralHealthEvaluationID, 99998) as BehavioralHealthEvaluationID,
    isnull(BehavioralHealthTreatmentID, 99998) as BehavioralHealthTreatmentID,
    isnull(MedicaidStatusTypeID, 99998) as MedicaidStatusTypeID,
    isnull(InTreatmentAtBooking, 'N') as InTreatmentAtBooking,
    isnull(BehavioralHealthAssessment.EndedDaysBeforeBooking, 99998) as AssessmentHistoricalPeriodTypeID,
    isnull(BehavioralHealthEvaluationTypeID, 99998) as BehavioralHealthEvaluationTypeID,
    isnull(MedicationTypeID, 99998) as MedicationTypeID,
    isnull(AssessmentCategoryTypeID, 99998) as AssessmentCategoryTypeID,
    isnull(TreatmentStatusTypeID, 99998) as TreatmentStatusTypeID,
    isnull(TreatmentAdmissionReasonTypeID, 99998) as TreatmentAdmissionReasonTypeID,
    isnull(TreatmentProviderTypeID, 99998) as TreatmentProviderTypeID,
    SevereMentalIllnessIndicator,
    isnull(SevereMentalIllnessIndicator, 99998) as SevereMentalIllnessIndicatorDimension,
    isnull(BehavioralHealthTreatment.DaysBeforeBooking, 99998) as TreatmentHistoricalPeriodTypeID,
    isnull(PrescribedMedicationID,99998) as PrescribedMedicationID,
    iif(SixMonthRebooking='Y', 1, 0) * JailEpisode.JailEpisodeID as SixMonthRebookingCount, 
    iif(OneYearRebooking='Y', 1, 0) * JailEpisode.JailEpisodeID as OneYearRebookingCount, 
    iif(TwoYearRebooking='Y', 1, 0) * JailEpisode.JailEpisodeID as TwoYearRebookingCount, 
    iif(IsActive='Y', 1, 0) * JailEpisode.JailEpisodeID as IsActiveBookingCount,
    iif(InTreatmentAtBooking='Y', 1, 0) * JailEpisode.JailEpisodeID as InTreatmentAtBookingCount,
    iif(SevereMentalIllnessIndicator=1, 1, 0) * JailEpisode.JailEpisodeID as SevereMentalIllnessBookingCount
into FullBookingView     
from
    JailEpisode inner join Person on JailEpisode.PersonID=Person.PersonID
    left join JailEpisodeArrest on JailEpisode.JailEpisodeID=JailEpisodeArrest.JailEpisodeID
    left join JailEpisodeCharge on JailEpisodeArrest.JailEpisodeArrestID=JailEpisodeCharge.JailEpisodeArrestID
    left join BehavioralHealthAssessment on BehavioralHealthAssessment.PersonID=Person.PersonID
    left join BehavioralHealthEvaluation  on BehavioralHealthAssessment.BehavioralHealthAssessmentID=BehavioralHealthEvaluation.BehavioralHealthAssessmentID
    left join BehavioralHealthAssessmentCategory on BehavioralHealthAssessment.BehavioralHealthAssessmentID=BehavioralHealthAssessmentCategory.BehavioralHealthAssessmentID
    left join PrescribedMedication on BehavioralHealthAssessment.BehavioralHealthAssessmentID=PrescribedMedication.BehavioralHealthAssessmentID
    left join BehavioralHealthTreatment  on BehavioralHealthAssessment.BehavioralHealthAssessmentID=BehavioralHealthTreatment.BehavioralHealthAssessmentID;