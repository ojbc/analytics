create database ojbc_booking_dimensional;
use ojbc_booking_dimensional;


CREATE TABLE BookingClassificationType (
                BookingClassificationTypeID INT NOT NULL,
                BookingClassificationTypeDescription VARCHAR(60) NOT NULL,
                BookingClassificationTypeCategory VARCHAR(40) NOT NULL,
                PRIMARY KEY (BookingClassificationTypeID)
);


CREATE TABLE TimeSpanType (
                TimeSpanTypeID INT NOT NULL,
                TimeSpanTypeDescription VARCHAR(20) NOT NULL,
                PRIMARY KEY (TimeSpanTypeID)
);


CREATE TABLE RespondingUnitType (
                RespondingUnitTypeID INT NOT NULL,
                RespondingUnitTypeDescription VARCHAR(40) NOT NULL,
                PRIMARY KEY (RespondingUnitTypeID)
);


CREATE TABLE DispositionLocationType (
                DispositionLocationTypeID INT NOT NULL,
                DispositionLocationTypeDescription VARCHAR(80) NOT NULL,
                DispositionLocationTypeCategory VARCHAR(80) NOT NULL,
                PRIMARY KEY (DispositionLocationTypeID)
);


CREATE TABLE CallNatureType (
                CallNatureTypeID INT NOT NULL,
                CallNatureTypeDescription VARCHAR(80) NOT NULL,
                CallNatureTypeCategory VARCHAR(80) NOT NULL,
                PRIMARY KEY (CallNatureTypeID)
);


CREATE TABLE PendingCriminalChargesType (
                PendingCriminalChargesTypeID INT NOT NULL,
                PendingCriminalChargesTypeDescription VARCHAR(80) NOT NULL,
                PRIMARY KEY (PendingCriminalChargesTypeID)
);


CREATE TABLE IndicatorType (
                IndicatorTypeID INT NOT NULL,
                IndicatorTypeDescription VARCHAR(50) NOT NULL,
                PRIMARY KEY (IndicatorTypeID)
);


CREATE TABLE Date (
                DateID INT NOT NULL,
                CalendarDate DATE NOT NULL,
                Year INT NOT NULL,
                YearLabel CHAR(4) NOT NULL,
                CalendarQuarter INT NOT NULL,
                Month INT NOT NULL,
                MonthName VARCHAR(12) NOT NULL,
                FullMonth CHAR(7) NOT NULL,
                Day INT NOT NULL,
                DayOfWeek VARCHAR(9) NOT NULL,
                DayOfWeekSort INT NOT NULL,
                DateMMDDYYYY CHAR(10) NOT NULL,
                WeekStartingDate DATE NOT NULL,
                DaysAgo INT NOT NULL,
                PRIMARY KEY (DateID)
);


CREATE TABLE HistoricalPeriodType (
                HistoricalPeriodTypeID INT NOT NULL,
                DaysAgo INT NOT NULL,
                HistoricalPeriodTypeDescription1 VARCHAR(50) NOT NULL,
                HistoricalPeriodTypeDescription2 VARCHAR(50) NOT NULL,
                PRIMARY KEY (HistoricalPeriodTypeID)
);


CREATE TABLE TreatmentProviderType (
                TreatmentProviderTypeID INT NOT NULL,
                TreatmentProviderTypeDescription VARCHAR(50) NOT NULL,
                PRIMARY KEY (TreatmentProviderTypeID)
);


CREATE TABLE PopulationType (
                PopulationTypeID INT NOT NULL,
                PopulationTypeDescription VARCHAR(50) NOT NULL,
                PRIMARY KEY (PopulationTypeID)
);


CREATE TABLE AssessmentCategoryType (
                AssessmentCategoryTypeID INT NOT NULL,
                AssessmentCategoryTypeDescription VARCHAR(50) NOT NULL,
                PRIMARY KEY (AssessmentCategoryTypeID)
);


CREATE TABLE MedicationType (
                MedicationTypeID INT NOT NULL,
                MedicationTypeDescription VARCHAR(80) NOT NULL,
                MedicationTypeCategory VARCHAR(50) NOT NULL,
                MedicationTypeCode VARCHAR(50) NOT NULL,
                PRIMARY KEY (MedicationTypeID)
);


CREATE TABLE TreatmentAdmissionReasonType (
                TreatmentAdmissionReasonTypeID INT NOT NULL,
                TreatmentAdmissionReasonTypeDescription VARCHAR(50) NOT NULL,
                PRIMARY KEY (TreatmentAdmissionReasonTypeID)
);


CREATE TABLE TreatmentStatusType (
                TreatmentStatusTypeID INT NOT NULL,
                TreatmentStatusTypeDescription VARCHAR(50) NOT NULL,
                PRIMARY KEY (TreatmentStatusTypeID)
);


CREATE TABLE MedicaidStatusType (
                MedicaidStatusTypeID INT NOT NULL,
                MedicaidStatusTypeDescription VARCHAR(50) NOT NULL,
                PRIMARY KEY (MedicaidStatusTypeID)
);


CREATE TABLE ChargeDispositionType (
                ChargeDispositionTypeID INT NOT NULL,
                ChargeDispositionTypeDescription VARCHAR(50) NOT NULL,
                ChargeDispositionTypeCategory VARCHAR(50) NOT NULL,
                PRIMARY KEY (ChargeDispositionTypeID)
);


CREATE TABLE BondStatusType (
                BondStatusTypeID INT NOT NULL,
                BondStatusTypeDescription VARCHAR(50) NOT NULL,
                BondStatusTypeCategory VARCHAR(50) NOT NULL,
                PRIMARY KEY (BondStatusTypeID)
);


CREATE TABLE ChargeClassType (
                ChargeClassTypeID INT NOT NULL,
                ChargeClassTypeDescription VARCHAR(100) NOT NULL,
                ChargeClassTypeCategory VARCHAR(100) NOT NULL,
                PRIMARY KEY (ChargeClassTypeID)
);


CREATE TABLE ProgramEligibilityType (
                ProgramEligibilityTypeID INT NOT NULL,
                ProgramEligibilityTypeDescription VARCHAR(50) NOT NULL,
                PRIMARY KEY (ProgramEligibilityTypeID)
);


CREATE TABLE WorkReleaseStatusType (
                WorkReleaseStatusTypeID INT NOT NULL,
                WorkReleaseStatusTypeDescription VARCHAR(50) NOT NULL,
                PRIMARY KEY (WorkReleaseStatusTypeID)
);


CREATE TABLE SexOffenderStatusType (
                SexOffenderStatusTypeID INT NOT NULL,
                SexOffenderStatusTypeDescription VARCHAR(50) NOT NULL,
                PRIMARY KEY (SexOffenderStatusTypeID)
);


CREATE TABLE MilitaryServiceStatusType (
                MilitaryServiceStatusTypeID INT NOT NULL,
                MilitaryServiceStatusTypeDescription VARCHAR(100) NOT NULL,
                PRIMARY KEY (MilitaryServiceStatusTypeID)
);


CREATE TABLE PersonEthnicityType (
                PersonEthnicityTypeID INT NOT NULL,
                PersonEthnicityTypeDescription VARCHAR(50) NOT NULL,
                PRIMARY KEY (PersonEthnicityTypeID)
);


CREATE TABLE Facility (
                FacilityID INT NOT NULL,
                FacilityDescription VARCHAR(50) NOT NULL,
                PRIMARY KEY (FacilityID)
);


CREATE TABLE LoadHistory (
                LoadHistoryID INT AUTO_INCREMENT NOT NULL,
                LoadHistoryTimestamp TIMESTAMP NOT NULL,
                MostRecentStagingTimestamp TIMESTAMP NOT NULL,
                PRIMARY KEY (LoadHistoryID)
);


CREATE TABLE LanguageType (
                LanguageTypeID INT NOT NULL,
                LanguageTypeDescription VARCHAR(20) NOT NULL,
                PRIMARY KEY (LanguageTypeID)
);


CREATE TABLE EducationLevelType (
                EducationLevelTypeID INT NOT NULL,
                EducationLevelTypeDescription VARCHAR(50) NOT NULL,
                PRIMARY KEY (EducationLevelTypeID)
);


CREATE TABLE OccupationType (
                OccupationTypeID INT NOT NULL,
                OccupationTypeDescription VARCHAR(50) NOT NULL,
                PRIMARY KEY (OccupationTypeID)
);


CREATE TABLE DomicileStatusType (
                DomicileStatusTypeID INT NOT NULL,
                DomicileStatusTypeDescription VARCHAR(50) NOT NULL,
                PRIMARY KEY (DomicileStatusTypeID)
);


CREATE TABLE BondType (
                BondTypeID INT NOT NULL,
                BondTypeDescription VARCHAR(100) NOT NULL,
                BondTypeCategory VARCHAR(50) NOT NULL,
                PRIMARY KEY (BondTypeID)
);


CREATE TABLE SupervisionUnitType (
                SupervisionUnitTypeID INT NOT NULL,
                SupervisionUnitTypeDescription VARCHAR(50) NOT NULL,
                PRIMARY KEY (SupervisionUnitTypeID)
);


CREATE TABLE BehavioralHealthEvaluationType (
                BehavioralHealthEvaluationTypeID INT NOT NULL,
                BehavioralHealthEvaluationTypeDescription VARCHAR(200) NOT NULL,
                BehavioralHealthEvaluationTypeCode VARCHAR(100) NOT NULL,
                BehavioralHealthEvaluationTypeCategory VARCHAR(100) NOT NULL,
                PRIMARY KEY (BehavioralHealthEvaluationTypeID)
);


CREATE TABLE PersonAgeRangeType (
                PersonAgeRangeTypeID INT NOT NULL,
                AgeRange VARCHAR(7) NOT NULL,
                AgeRangeSort VARCHAR(7) NOT NULL,
                PRIMARY KEY (PersonAgeRangeTypeID)
);


CREATE TABLE PersonRaceType (
                PersonRaceTypeID INT NOT NULL,
                PersonRaceTypeDescription VARCHAR(50) NOT NULL,
                PRIMARY KEY (PersonRaceTypeID)
);


CREATE TABLE PersonAgeType (
                PersonAgeTypeID INT NOT NULL,
                AgeInYears VARCHAR(10) NOT NULL,
                PersonAgeRangeTypeID INT NOT NULL,
                PRIMARY KEY (PersonAgeTypeID)
);


CREATE TABLE PersonSexType (
                PersonSexTypeID INT NOT NULL,
                PersonSexTypeDescription VARCHAR(7) NOT NULL,
                PRIMARY KEY (PersonSexTypeID)
);


CREATE TABLE Person (
                PersonID INT AUTO_INCREMENT NOT NULL,
                StagingPersonUniqueIdentifier VARCHAR(100) NOT NULL,
                StagingPersonUniqueIdentifier2 VARCHAR(100) NOT NULL,
                PersonAgeTypeID INT NOT NULL,
                PersonSexTypeID INT NOT NULL,
                PersonRaceTypeID INT NOT NULL,
                PersonEthnicityTypeID INT NOT NULL,
                LanguageTypeID INT NOT NULL,
                EducationLevelTypeID INT NOT NULL,
                OccupationTypeID INT NOT NULL,
                MilitaryServiceStatusTypeID INT NOT NULL,
                DomicileStatusTypeID INT NOT NULL,
                ProgramEligibilityTypeID INT NOT NULL,
                SexOffenderStatusTypeID INT NOT NULL,
                WorkReleaseStatusTypeID INT NOT NULL,
                PopulationTypeID INT NOT NULL,
                PRIMARY KEY (PersonID)
);


CREATE TABLE BehavioralHealthAssessment (
                BehavioralHealthAssessmentID INT AUTO_INCREMENT NOT NULL,
                PersonID INT NOT NULL,
                SevereMentalIllnessIndicator INT NOT NULL,
                MedicaidStatusTypeID INT NOT NULL,
                InTreatmentAtEvent CHAR(1) NOT NULL,
                EndedDaysBeforeEvent INT,
                PRIMARY KEY (BehavioralHealthAssessmentID)
);


CREATE TABLE BehavioralHealthAssessmentCategory (
                BehavioralHealthAssessmentCategoryID INT AUTO_INCREMENT NOT NULL,
                BehavioralHealthAssessmentID INT NOT NULL,
                AssessmentCategoryTypeID INT NOT NULL,
                PRIMARY KEY (BehavioralHealthAssessmentCategoryID)
);


CREATE TABLE PrescribedMedication (
                PrescribedMedicationID INT AUTO_INCREMENT NOT NULL,
                BehavioralHealthAssessmentID INT NOT NULL,
                MedicationTypeID INT NOT NULL,
                PRIMARY KEY (PrescribedMedicationID)
);


CREATE TABLE BehavioralHealthTreatment (
                BehavioralHealthTreatmentID INT AUTO_INCREMENT NOT NULL,
                BehavioralHealthAssessmentID INT NOT NULL,
                TreatmentStatusTypeID INT NOT NULL,
                TreatmentAdmissionReasonTypeID INT NOT NULL,
                TreatmentProviderTypeID INT NOT NULL,
                DaysBeforeEvent INT,
                PRIMARY KEY (BehavioralHealthTreatmentID)
);


CREATE TABLE BehavioralHealthEvaluation (
                BehavioralHealthEvaluationID INT AUTO_INCREMENT NOT NULL,
                BehavioralHealthAssessmentID INT NOT NULL,
                BehavioralHealthEvaluationTypeID INT NOT NULL,
                PRIMARY KEY (BehavioralHealthEvaluationID)
);


CREATE TABLE ChargeType (
                ChargeTypeID INT NOT NULL,
                ChargeTypeDescription VARCHAR(200) NOT NULL,
                ChargeTypeCitation VARCHAR(100) NOT NULL,
                ChargeTypeCategory VARCHAR(100) NOT NULL,
                PRIMARY KEY (ChargeTypeID)
);


CREATE TABLE CaseStatusType (
                CaseStatusTypeID INT NOT NULL,
                CaseStatusTypeDescription VARCHAR(100) NOT NULL,
                CaseStatusTypeCategory VARCHAR(50) NOT NULL,
                PRIMARY KEY (CaseStatusTypeID)
);


CREATE TABLE JurisdictionType (
                JurisdictionTypeID INT NOT NULL,
                JurisdictionTypeDescription VARCHAR(100) NOT NULL,
                JurisdictionCategory VARCHAR(50) NOT NULL,
                PRIMARY KEY (JurisdictionTypeID)
);


CREATE TABLE Agency (
                AgencyID INT NOT NULL,
                AgencyDescription VARCHAR(40) NOT NULL,
                AgencyCategory VARCHAR(50) NOT NULL,
                PRIMARY KEY (AgencyID)
);


CREATE TABLE Incident (
                IncidentID INT NOT NULL,
                PersonID INT NOT NULL,
                ReportingAgencyID INT NOT NULL,
                IncidentReportedDate DATE,
                IncidentReportedDateID INT NOT NULL,
                IncidentReportedHour INT,
                TimeSpanTypeID INT NOT NULL,
                CallNatureTypeID INT NOT NULL,
                DispositionLocationTypeID INT NOT NULL,
                PendingCriminalChargesTypeID INT NOT NULL,
                IncidentNumber VARCHAR(40) NOT NULL,
                OfficerCount INT,
                DurationInMinutes DOUBLE PRECISION,
                CostInUnitMinutes DOUBLE PRECISION,
                DaysSinceLastIncident INT,
                DaysUntilNextIncident INT,
                DaysSinceLastBooking INT,
                DaysUntilNextBooking INT,
                PriorInteractions6Months INT NOT NULL,
                PriorInteractions12Months INT NOT NULL,
                PriorInteractions18Months INT NOT NULL,
                PriorInteractions24Months INT NOT NULL,
                SubstanceAbuseInvolvementIndicator INT NOT NULL,
                CrisisInterventionTeamInvolvementIndicator INT NOT NULL,
                PRIMARY KEY (IncidentID)
);


CREATE TABLE UnitResponse (
                UnitResponseID INT NOT NULL,
                IncidentID INT NOT NULL,
                RespondingUnitTypeID INT NOT NULL,
                ArrivalDateID INT NOT NULL,
                ArrivalDate DATE,
                ArrivalHour INT,
                ClearedDate DATE,
                ClearedDateID INT NOT NULL,
                ClearedHour INT NOT NULL,
                ResponseDurationInMinutes DOUBLE PRECISION,
                PRIMARY KEY (UnitResponseID)
);


CREATE TABLE JailEpisode (
                JailEpisodeID INT NOT NULL,
                PersonID INT NOT NULL,
                BookingNumber VARCHAR(100) NOT NULL,
                EpisodeStartDateID INT NOT NULL,
                EpisodeStartDate DATE NOT NULL,
                EpisodeEndDate DATE,
                EpisodeEndDateID INT NOT NULL,
                LengthOfStay INT NOT NULL,
                IsActive CHAR(1) NOT NULL,
                FacilityID INT NOT NULL,
                SupervisionUnitTypeID INT NOT NULL,
                CaseStatusTypeID INT NOT NULL,
                DaysSinceLastEpisode INT,
                DaysUntilNextEpisode INT,
                SixMonthRebooking CHAR(1) NOT NULL,
                OneYearRebooking CHAR(1) NOT NULL,
                TwoYearRebooking CHAR(1) NOT NULL,
                DaysSinceLastIncident INT,
                DaysUntilNextIncident INT,
                BookingClassificationTypeID INT NOT NULL,
                LoadHistoryID INT NOT NULL,
                PRIMARY KEY (JailEpisodeID)
);


CREATE TABLE JailEpisodeArrest (
                JailEpisodeArrestID INT AUTO_INCREMENT NOT NULL,
                JailEpisodeID INT NOT NULL,
                AgencyID INT NOT NULL,
                ArrestLocationLatitude NUMERIC(14,10),
                ArrestLocationLongitude NUMERIC(14,10),
                PRIMARY KEY (JailEpisodeArrestID)
);


CREATE TABLE JailEpisodeCharge (
                JailEpisodeChargeID INT AUTO_INCREMENT NOT NULL,
                JailEpisodeArrestID INT NOT NULL,
                ChargeTypeID INT NOT NULL,
                ChargeClassTypeID INT NOT NULL,
                ChargeDispositionTypeID INT NOT NULL,
                AgencyID INT NOT NULL,
                JurisdictionTypeID INT NOT NULL,
                BondStatusTypeID INT NOT NULL,
                BondTypeID INT NOT NULL,
                BondAmount DECIMAL(12,2),
                PRIMARY KEY (JailEpisodeChargeID)
);


ALTER TABLE JailEpisode ADD CONSTRAINT bookingclassificationtype_jailepisode_fk
FOREIGN KEY (BookingClassificationTypeID)
REFERENCES BookingClassificationType (BookingClassificationTypeID)
ON DELETE NO ACTION
ON UPDATE NO ACTION;

ALTER TABLE Incident ADD CONSTRAINT timespantype_incident_fk
FOREIGN KEY (TimeSpanTypeID)
REFERENCES TimeSpanType (TimeSpanTypeID)
ON DELETE NO ACTION
ON UPDATE NO ACTION;

ALTER TABLE UnitResponse ADD CONSTRAINT respondingunittype_unitresponse_fk
FOREIGN KEY (RespondingUnitTypeID)
REFERENCES RespondingUnitType (RespondingUnitTypeID)
ON DELETE NO ACTION
ON UPDATE NO ACTION;

ALTER TABLE Incident ADD CONSTRAINT dispositionlocationtype_incident_fk
FOREIGN KEY (DispositionLocationTypeID)
REFERENCES DispositionLocationType (DispositionLocationTypeID)
ON DELETE NO ACTION
ON UPDATE NO ACTION;

ALTER TABLE Incident ADD CONSTRAINT callnaturetype_incident_fk
FOREIGN KEY (CallNatureTypeID)
REFERENCES CallNatureType (CallNatureTypeID)
ON DELETE NO ACTION
ON UPDATE NO ACTION;

ALTER TABLE Incident ADD CONSTRAINT pendingcriminalchargestype_incident_fk
FOREIGN KEY (PendingCriminalChargesTypeID)
REFERENCES PendingCriminalChargesType (PendingCriminalChargesTypeID)
ON DELETE NO ACTION
ON UPDATE NO ACTION;

ALTER TABLE JailEpisode ADD CONSTRAINT date_jailepisode_fk
FOREIGN KEY (EpisodeStartDateID)
REFERENCES Date (DateID)
ON DELETE NO ACTION
ON UPDATE NO ACTION;

ALTER TABLE JailEpisode ADD CONSTRAINT date_jailepisode_fk1
FOREIGN KEY (EpisodeEndDateID)
REFERENCES Date (DateID)
ON DELETE NO ACTION
ON UPDATE NO ACTION;

ALTER TABLE Incident ADD CONSTRAINT date_incident_fk
FOREIGN KEY (IncidentReportedDateID)
REFERENCES Date (DateID)
ON DELETE NO ACTION
ON UPDATE NO ACTION;

ALTER TABLE UnitResponse ADD CONSTRAINT date_unitresponse_fk
FOREIGN KEY (ArrivalDateID)
REFERENCES Date (DateID)
ON DELETE NO ACTION
ON UPDATE NO ACTION;

ALTER TABLE UnitResponse ADD CONSTRAINT date_unitresponse_fk1
FOREIGN KEY (ClearedDateID)
REFERENCES Date (DateID)
ON DELETE NO ACTION
ON UPDATE NO ACTION;

ALTER TABLE BehavioralHealthAssessment ADD CONSTRAINT historicalperiodtype_behavioralhealthassessment_fk
FOREIGN KEY (EndedDaysBeforeEvent)
REFERENCES HistoricalPeriodType (HistoricalPeriodTypeID)
ON DELETE NO ACTION
ON UPDATE NO ACTION;

ALTER TABLE BehavioralHealthTreatment ADD CONSTRAINT historicalperiodtype_behavioralhealthtreatment_fk
FOREIGN KEY (DaysBeforeEvent)
REFERENCES HistoricalPeriodType (HistoricalPeriodTypeID)
ON DELETE NO ACTION
ON UPDATE NO ACTION;

ALTER TABLE BehavioralHealthTreatment ADD CONSTRAINT treatmentprovidertype_behavioralhealthtreatment_fk
FOREIGN KEY (TreatmentProviderTypeID)
REFERENCES TreatmentProviderType (TreatmentProviderTypeID)
ON DELETE NO ACTION
ON UPDATE NO ACTION;

ALTER TABLE Person ADD CONSTRAINT populationtype_person_fk
FOREIGN KEY (PopulationTypeID)
REFERENCES PopulationType (PopulationTypeID)
ON DELETE NO ACTION
ON UPDATE NO ACTION;

ALTER TABLE BehavioralHealthAssessmentCategory ADD CONSTRAINT assessmentcategorytype_behavioralhealthassessmentcategory_fk
FOREIGN KEY (AssessmentCategoryTypeID)
REFERENCES AssessmentCategoryType (AssessmentCategoryTypeID)
ON DELETE NO ACTION
ON UPDATE NO ACTION;

ALTER TABLE PrescribedMedication ADD CONSTRAINT medicationtype_prescribedmedication_fk
FOREIGN KEY (MedicationTypeID)
REFERENCES MedicationType (MedicationTypeID)
ON DELETE NO ACTION
ON UPDATE NO ACTION;

ALTER TABLE BehavioralHealthTreatment ADD CONSTRAINT treatmentadmissionreasontype_behavioralhealthtreatment_fk
FOREIGN KEY (TreatmentAdmissionReasonTypeID)
REFERENCES TreatmentAdmissionReasonType (TreatmentAdmissionReasonTypeID)
ON DELETE NO ACTION
ON UPDATE NO ACTION;

ALTER TABLE BehavioralHealthTreatment ADD CONSTRAINT treatmentstatustype_behavioralhealthtreatment_fk
FOREIGN KEY (TreatmentStatusTypeID)
REFERENCES TreatmentStatusType (TreatmentStatusTypeID)
ON DELETE NO ACTION
ON UPDATE NO ACTION;

ALTER TABLE BehavioralHealthAssessment ADD CONSTRAINT medicaidstatustype_behavioralhealthassessment_fk
FOREIGN KEY (MedicaidStatusTypeID)
REFERENCES MedicaidStatusType (MedicaidStatusTypeID)
ON DELETE NO ACTION
ON UPDATE NO ACTION;

ALTER TABLE JailEpisodeCharge ADD CONSTRAINT chargedispositiontype_jailepisodecharge_fk
FOREIGN KEY (ChargeDispositionTypeID)
REFERENCES ChargeDispositionType (ChargeDispositionTypeID)
ON DELETE NO ACTION
ON UPDATE NO ACTION;

ALTER TABLE JailEpisodeCharge ADD CONSTRAINT bondstatustype_jailepisodecharge_fk
FOREIGN KEY (BondStatusTypeID)
REFERENCES BondStatusType (BondStatusTypeID)
ON DELETE NO ACTION
ON UPDATE NO ACTION;

ALTER TABLE JailEpisodeCharge ADD CONSTRAINT chargeclasstype_jailepisodecharge_fk
FOREIGN KEY (ChargeClassTypeID)
REFERENCES ChargeClassType (ChargeClassTypeID)
ON DELETE NO ACTION
ON UPDATE NO ACTION;

ALTER TABLE Person ADD CONSTRAINT programeligibilitytype_person_fk
FOREIGN KEY (ProgramEligibilityTypeID)
REFERENCES ProgramEligibilityType (ProgramEligibilityTypeID)
ON DELETE NO ACTION
ON UPDATE NO ACTION;

ALTER TABLE Person ADD CONSTRAINT workreleasestatustype_person_fk
FOREIGN KEY (WorkReleaseStatusTypeID)
REFERENCES WorkReleaseStatusType (WorkReleaseStatusTypeID)
ON DELETE NO ACTION
ON UPDATE NO ACTION;

ALTER TABLE Person ADD CONSTRAINT sexoffenderstatustype_person_fk
FOREIGN KEY (SexOffenderStatusTypeID)
REFERENCES SexOffenderStatusType (SexOffenderStatusTypeID)
ON DELETE NO ACTION
ON UPDATE NO ACTION;

ALTER TABLE Person ADD CONSTRAINT militaryservicestatustype_person_fk
FOREIGN KEY (MilitaryServiceStatusTypeID)
REFERENCES MilitaryServiceStatusType (MilitaryServiceStatusTypeID)
ON DELETE NO ACTION
ON UPDATE NO ACTION;

ALTER TABLE Person ADD CONSTRAINT personethnicitytype_person_fk
FOREIGN KEY (PersonEthnicityTypeID)
REFERENCES PersonEthnicityType (PersonEthnicityTypeID)
ON DELETE NO ACTION
ON UPDATE NO ACTION;

ALTER TABLE JailEpisode ADD CONSTRAINT facility_jailepisode_fk
FOREIGN KEY (FacilityID)
REFERENCES Facility (FacilityID)
ON DELETE NO ACTION
ON UPDATE NO ACTION;

ALTER TABLE JailEpisode ADD CONSTRAINT loadhistory_jailepisode_fk
FOREIGN KEY (LoadHistoryID)
REFERENCES LoadHistory (LoadHistoryID)
ON DELETE NO ACTION
ON UPDATE NO ACTION;

ALTER TABLE Person ADD CONSTRAINT language_person_fk
FOREIGN KEY (LanguageTypeID)
REFERENCES LanguageType (LanguageTypeID)
ON DELETE NO ACTION
ON UPDATE NO ACTION;

ALTER TABLE Person ADD CONSTRAINT education_person_fk
FOREIGN KEY (EducationLevelTypeID)
REFERENCES EducationLevelType (EducationLevelTypeID)
ON DELETE NO ACTION
ON UPDATE NO ACTION;

ALTER TABLE Person ADD CONSTRAINT occupation_person_fk
FOREIGN KEY (OccupationTypeID)
REFERENCES OccupationType (OccupationTypeID)
ON DELETE NO ACTION
ON UPDATE NO ACTION;

ALTER TABLE Person ADD CONSTRAINT domicilestatustype_person_fk
FOREIGN KEY (DomicileStatusTypeID)
REFERENCES DomicileStatusType (DomicileStatusTypeID)
ON DELETE NO ACTION
ON UPDATE NO ACTION;

ALTER TABLE JailEpisodeCharge ADD CONSTRAINT bondtype_jailepisodecharge_fk
FOREIGN KEY (BondTypeID)
REFERENCES BondType (BondTypeID)
ON DELETE NO ACTION
ON UPDATE NO ACTION;

ALTER TABLE JailEpisode ADD CONSTRAINT bedtype_booking_fk
FOREIGN KEY (SupervisionUnitTypeID)
REFERENCES SupervisionUnitType (SupervisionUnitTypeID)
ON DELETE NO ACTION
ON UPDATE NO ACTION;

ALTER TABLE BehavioralHealthEvaluation ADD CONSTRAINT behavioralhealthevaluationtype_behavioralhealthevaluation_fk
FOREIGN KEY (BehavioralHealthEvaluationTypeID)
REFERENCES BehavioralHealthEvaluationType (BehavioralHealthEvaluationTypeID)
ON DELETE NO ACTION
ON UPDATE NO ACTION;

ALTER TABLE PersonAgeType ADD CONSTRAINT personagerange_personage_fk
FOREIGN KEY (PersonAgeRangeTypeID)
REFERENCES PersonAgeRangeType (PersonAgeRangeTypeID)
ON DELETE NO ACTION
ON UPDATE NO ACTION;

ALTER TABLE Person ADD CONSTRAINT personrace_person_fk
FOREIGN KEY (PersonRaceTypeID)
REFERENCES PersonRaceType (PersonRaceTypeID)
ON DELETE NO ACTION
ON UPDATE NO ACTION;

ALTER TABLE Person ADD CONSTRAINT personage_person_fk
FOREIGN KEY (PersonAgeTypeID)
REFERENCES PersonAgeType (PersonAgeTypeID)
ON DELETE NO ACTION
ON UPDATE NO ACTION;

ALTER TABLE Person ADD CONSTRAINT personsex_person_fk
FOREIGN KEY (PersonSexTypeID)
REFERENCES PersonSexType (PersonSexTypeID)
ON DELETE NO ACTION
ON UPDATE NO ACTION;

ALTER TABLE JailEpisode ADD CONSTRAINT person_booking_fk
FOREIGN KEY (PersonID)
REFERENCES Person (PersonID)
ON DELETE NO ACTION
ON UPDATE NO ACTION;

ALTER TABLE BehavioralHealthAssessment ADD CONSTRAINT person_behavioralhealthassessment_fk
FOREIGN KEY (PersonID)
REFERENCES Person (PersonID)
ON DELETE NO ACTION
ON UPDATE NO ACTION;

ALTER TABLE Incident ADD CONSTRAINT person_incident_fk
FOREIGN KEY (PersonID)
REFERENCES Person (PersonID)
ON DELETE NO ACTION
ON UPDATE NO ACTION;

ALTER TABLE BehavioralHealthEvaluation ADD CONSTRAINT behavioralhealthassessment_behavioralhealthevaluation_fk
FOREIGN KEY (BehavioralHealthAssessmentID)
REFERENCES BehavioralHealthAssessment (BehavioralHealthAssessmentID)
ON DELETE NO ACTION
ON UPDATE NO ACTION;

ALTER TABLE BehavioralHealthTreatment ADD CONSTRAINT behavioralhealthassessment_behavioralhealthtreatment_fk
FOREIGN KEY (BehavioralHealthAssessmentID)
REFERENCES BehavioralHealthAssessment (BehavioralHealthAssessmentID)
ON DELETE NO ACTION
ON UPDATE NO ACTION;

ALTER TABLE PrescribedMedication ADD CONSTRAINT behavioralhealthassessment_prescribedmedication_fk
FOREIGN KEY (BehavioralHealthAssessmentID)
REFERENCES BehavioralHealthAssessment (BehavioralHealthAssessmentID)
ON DELETE NO ACTION
ON UPDATE NO ACTION;

ALTER TABLE BehavioralHealthAssessmentCategory ADD CONSTRAINT behavioralhealthassessment_behavioralhealthassessmentcategory_fk
FOREIGN KEY (BehavioralHealthAssessmentID)
REFERENCES BehavioralHealthAssessment (BehavioralHealthAssessmentID)
ON DELETE NO ACTION
ON UPDATE NO ACTION;

ALTER TABLE JailEpisodeCharge ADD CONSTRAINT chargetype_charge_fk
FOREIGN KEY (ChargeTypeID)
REFERENCES ChargeType (ChargeTypeID)
ON DELETE NO ACTION
ON UPDATE NO ACTION;

ALTER TABLE JailEpisode ADD CONSTRAINT casestatustype_jailepisode_fk
FOREIGN KEY (CaseStatusTypeID)
REFERENCES CaseStatusType (CaseStatusTypeID)
ON DELETE NO ACTION
ON UPDATE NO ACTION;

ALTER TABLE JailEpisodeCharge ADD CONSTRAINT jurisdictiontype_jailepisodecharge_fk
FOREIGN KEY (JurisdictionTypeID)
REFERENCES JurisdictionType (JurisdictionTypeID)
ON DELETE NO ACTION
ON UPDATE NO ACTION;

ALTER TABLE JailEpisodeCharge ADD CONSTRAINT agencytype_jailepisodecharge_fk
FOREIGN KEY (AgencyID)
REFERENCES Agency (AgencyID)
ON DELETE NO ACTION
ON UPDATE NO ACTION;

ALTER TABLE JailEpisodeArrest ADD CONSTRAINT agencytype_jailepisodearrest_fk
FOREIGN KEY (AgencyID)
REFERENCES Agency (AgencyID)
ON DELETE NO ACTION
ON UPDATE NO ACTION;

ALTER TABLE Incident ADD CONSTRAINT agency_incident_fk
FOREIGN KEY (ReportingAgencyID)
REFERENCES Agency (AgencyID)
ON DELETE NO ACTION
ON UPDATE NO ACTION;

ALTER TABLE UnitResponse ADD CONSTRAINT incident_unitresponse_fk
FOREIGN KEY (IncidentID)
REFERENCES Incident (IncidentID)
ON DELETE NO ACTION
ON UPDATE NO ACTION;

ALTER TABLE JailEpisodeArrest ADD CONSTRAINT jailepisode_jailepisodearrest_fk
FOREIGN KEY (JailEpisodeID)
REFERENCES JailEpisode (JailEpisodeID)
ON DELETE NO ACTION
ON UPDATE NO ACTION;

ALTER TABLE JailEpisodeCharge ADD CONSTRAINT jailepisodearrest_jailepisodecharge_fk
FOREIGN KEY (JailEpisodeArrestID)
REFERENCES JailEpisodeArrest (JailEpisodeArrestID)
ON DELETE NO ACTION
ON UPDATE NO ACTION;