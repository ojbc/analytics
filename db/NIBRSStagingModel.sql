
CREATE TABLE OfficerAssignmentTypeType (
                OfficerAssignmentTypeTypeID INT NOT NULL,
                OfficerAssignmentTypeCode VARCHAR(1),
                OfficerAssignmentTypeDescription VARCHAR(100),
                PRIMARY KEY (OfficerAssignmentTypeTypeID)
);

ALTER TABLE OfficerAssignmentTypeType MODIFY COLUMN OfficerAssignmentTypeCode VARCHAR(1) COMMENT 'Valid Codes: I, B, F, G, R, S, O, and U.';


CREATE TABLE OfficerActivityCircumstanceType (
                OfficerActivityCircumstanceTypeID INT NOT NULL,
                OfficerActivityCircumstanceCode VARCHAR(1),
                OfficerActivityCircumstanceDescription VARCHAR(75),
                PRIMARY KEY (OfficerActivityCircumstanceTypeID)
);

ALTER TABLE OfficerActivityCircumstanceType MODIFY COLUMN OfficerActivityCircumstanceCode VARCHAR(1) COMMENT 'Valid Codes: I, B, F, G, R, S, O, and U.';


CREATE TABLE TypeOfWeaponForceInvolvedType (
                TypeOfWeaponForceInvolvedTypeID INT NOT NULL,
                TypeOfWeaponForceInvolvedCode VARCHAR(2),
                TypeOfWeaponForceInvolvedDescription VARCHAR(30),
                PRIMARY KEY (TypeOfWeaponForceInvolvedTypeID)
);


CREATE TABLE SuspectedDrugTypeType (
                SuspectedDrugTypeTypeID INT NOT NULL,
                SuspectedDrugTypeCode VARCHAR(1),
                SuspectedDrugTypeDescription VARCHAR(20),
                PRIMARY KEY (SuspectedDrugTypeTypeID)
);

ALTER TABLE SuspectedDrugTypeType MODIFY COLUMN SuspectedDrugTypeCode VARCHAR(1) COMMENT 'Valid Codes: A through P, U, and X.';

ALTER TABLE SuspectedDrugTypeType MODIFY COLUMN SuspectedDrugTypeDescription VARCHAR(20) COMMENT 'Valid Codes: A through P, U, and X.';


CREATE TABLE BiasMotivationType (
                BiasMotivationTypeID INT NOT NULL,
                BiasMotivationCode VARCHAR(2),
                BiasMotivationDescription VARCHAR(60),
                PRIMARY KEY (BiasMotivationTypeID)
);


CREATE TABLE MethodOfEntryType (
                MethodOfEntryTypeID INT NOT NULL,
                MethodOfEntryCode VARCHAR(1),
                MethodOfEntryDescription VARCHAR(10),
                PRIMARY KEY (MethodOfEntryTypeID)
);

ALTER TABLE MethodOfEntryType MODIFY COLUMN MethodOfEntryCode VARCHAR(1) COMMENT 'Offense Code is 220
(Burglary).';


CREATE TABLE LocationTypeType (
                LocationTypeTypeID INT NOT NULL,
                LocationTypeCode VARCHAR(2),
                LocationTypeDescription VARCHAR(45),
                PRIMARY KEY (LocationTypeTypeID)
);

ALTER TABLE LocationTypeType COMMENT 'Valid Codes: 01 through 25';

ALTER TABLE LocationTypeType MODIFY COLUMN LocationTypeCode VARCHAR(2) COMMENT 'Valid Codes: 01 through 25';


CREATE TABLE ClearedExceptionallyType (
                ClearedExceptionallyTypeID INT NOT NULL,
                ClearedExceptionallyCode VARCHAR(1),
                ClearedExceptionallyDescription VARCHAR(220),
                PRIMARY KEY (ClearedExceptionallyTypeID)
);

ALTER TABLE ClearedExceptionallyType MODIFY COLUMN ClearedExceptionallyCode VARCHAR(1) COMMENT 'Valid Codes: A, B, C, D, E, and N.';

ALTER TABLE ClearedExceptionallyType MODIFY COLUMN ClearedExceptionallyDescription VARCHAR(220) COMMENT 'Valid Codes: A, B, C, D, E, and N.';


CREATE TABLE DispositionOfArresteeUnder18Type (
                DispositionOfArresteeUnder18TypeID INT NOT NULL,
                DispositionOfArresteeUnder18Code VARCHAR(1),
                DispositionOfArresteeUnder18Description VARCHAR(30),
                PRIMARY KEY (DispositionOfArresteeUnder18TypeID)
);

ALTER TABLE DispositionOfArresteeUnder18Type MODIFY COLUMN DispositionOfArresteeUnder18Code VARCHAR(1) COMMENT 'Valid Codes: H and R.';


CREATE TABLE ArresteeWasArmedWithType (
                ArresteeWasArmedWithTypeID INT NOT NULL,
                ArresteeWasArmedWithCode VARCHAR(2),
                ArresteeWasArmedWithDescription VARCHAR(75),
                PRIMARY KEY (ArresteeWasArmedWithTypeID)
);

ALTER TABLE ArresteeWasArmedWithType MODIFY COLUMN ArresteeWasArmedWithCode VARCHAR(2) COMMENT 'Valid Codes: 01 and 11 through 17';


CREATE TABLE MultipleArresteeSegmentsIndicatorType (
                MultipleArresteeSegmentsIndicatorTypeID INT NOT NULL,
                MultipleArresteeSegmentsIndicatorCode VARCHAR(1),
                MultipleArresteeSegmentsIndicatorDescription VARCHAR(15),
                PRIMARY KEY (MultipleArresteeSegmentsIndicatorTypeID)
);

ALTER TABLE MultipleArresteeSegmentsIndicatorType MODIFY COLUMN MultipleArresteeSegmentsIndicatorCode VARCHAR(1) COMMENT 'Valid Codes: M, C, and N.';


CREATE TABLE TypeOfArrestType (
                TypeOfArrestTypeID INT NOT NULL,
                TypeOfArrestCode VARCHAR(1),
                TypeOfArrestDescription VARCHAR(20),
                PRIMARY KEY (TypeOfArrestTypeID)
);

ALTER TABLE TypeOfArrestType MODIFY COLUMN TypeOfArrestCode VARCHAR(1) COMMENT 'Valid Codes: O, S, and T.';


CREATE TABLE AdditionalJustifiableHomicideCircumstancesType (
                AdditionalJustifiableHomicideCircumstancesID INT NOT NULL,
                AdditionalJustifiableHomicideCircumstancesCode VARCHAR(1),
                AdditionalJustifiableHomicideCircumstancesDescription VARCHAR(80),
                PRIMARY KEY (AdditionalJustifiableHomicideCircumstancesID)
);

ALTER TABLE AdditionalJustifiableHomicideCircumstancesType MODIFY COLUMN AdditionalJustifiableHomicideCircumstancesCode VARCHAR(1) COMMENT 'Valid Codes: A through G';


CREATE TABLE TypeInjuryType (
                TypeInjuryTypeID INT NOT NULL,
                TypeInjuryCode VARCHAR(1) NOT NULL,
                TypeInjuryDescription VARCHAR(25) NOT NULL,
                PRIMARY KEY (TypeInjuryTypeID)
);

ALTER TABLE TypeInjuryType MODIFY COLUMN TypeInjuryCode VARCHAR(1) COMMENT 'Valid Codes: N, B, I, L, M, O, T, and U.';

ALTER TABLE TypeInjuryType MODIFY COLUMN TypeInjuryDescription VARCHAR(25) COMMENT 'Valid Codes: N, B, I, L, M, O, T, and U.';


CREATE TABLE ResidentStatusOfPersonType (
                ResidentStatusOfPersonTypeID INT NOT NULL,
                ResidentStatusOfPersonCode VARCHAR(1),
                ResidentStatusOfPersonDescription VARCHAR(15),
                PRIMARY KEY (ResidentStatusOfPersonTypeID)
);

ALTER TABLE ResidentStatusOfPersonType MODIFY COLUMN ResidentStatusOfPersonCode VARCHAR(1) COMMENT 'Valid Codes: R, N, and U.';


CREATE TABLE EthnicityOfPersonType (
                EthnicityOfPersonTypeID INT NOT NULL,
                EthnicityOfPersonCode VARCHAR(1),
                EthnicityOfPersonDescription VARCHAR(25),
                PRIMARY KEY (EthnicityOfPersonTypeID)
);

ALTER TABLE EthnicityOfPersonType MODIFY COLUMN EthnicityOfPersonCode VARCHAR(1) COMMENT 'Valid Codes: H, N, and U.';


CREATE TABLE TypeOfVictimType (
                TypeOfVictimTypeID INT NOT NULL,
                TypeOfVictimCode VARCHAR(1),
                TypeOfVictimDescription VARCHAR(75),
                PRIMARY KEY (TypeOfVictimTypeID)
);

ALTER TABLE TypeOfVictimType MODIFY COLUMN TypeOfVictimCode VARCHAR(1) COMMENT 'Valid Codes: I, B, F, G, R, S, O, and U.';


CREATE TABLE TypeDrugMeasurementTypeID (
                TypeDrugMeasurementTypeID INT NOT NULL,
                TypeDrugMeasurementCode VARCHAR(2),
                TypeDrugMeasurementDescription VARCHAR(20),
                PRIMARY KEY (TypeDrugMeasurementTypeID)
);

ALTER TABLE TypeDrugMeasurementTypeID MODIFY COLUMN TypeDrugMeasurementCode VARCHAR(2) COMMENT 'Valid Codes: GM, KG, OZ, LB, ML, LT,
FO, GL, DU, NP, and XX.';


CREATE TABLE PropertyDescriptionType (
                PropertyDescriptionTypeID INT NOT NULL,
                PropertyDescriptionCode VARCHAR(2),
                PropertyDescriptionDescription VARCHAR(45),
                PRIMARY KEY (PropertyDescriptionTypeID)
);


CREATE TABLE RaceOfPersonType (
                RaceOfPersonTypeID INT NOT NULL,
                RaceOfPersonCode VARCHAR(1),
                RaceOfPersonDescription VARCHAR(580),
                PRIMARY KEY (RaceOfPersonTypeID)
);


CREATE TABLE SexOfPersonType (
                SexOfPersonTypeID INT NOT NULL,
                SexOfPersonCode VARCHAR(1),
                SexOfPersonDescription VARCHAR(35),
                PRIMARY KEY (SexOfPersonTypeID)
);


CREATE TABLE TypeOfCriminalActivityType (
                TypeOfCriminalActivityTypeID INT NOT NULL,
                TypeOfCriminalActivityCode VARCHAR(1),
                TypeOfCriminalActivityDescription VARCHAR(80),
                PRIMARY KEY (TypeOfCriminalActivityTypeID)
);

ALTER TABLE TypeOfCriminalActivityType COMMENT 'Up to 3 type of criminal activity per offense';

ALTER TABLE TypeOfCriminalActivityType MODIFY COLUMN TypeOfCriminalActivityCode VARCHAR(1) COMMENT 'Valid Codes: B, C, D, E, O, P, T, and U.';


CREATE TABLE OffenderSuspectedOfUsingType (
                OffenderSuspectedOfUsingTypeID INT NOT NULL,
                OffenderSuspectedOfUsingCode VARCHAR(1),
                OffenderSuspectedOfUsingDescription VARCHAR(20),
                PRIMARY KEY (OffenderSuspectedOfUsingTypeID)
);

ALTER TABLE OffenderSuspectedOfUsingType MODIFY COLUMN OffenderSuspectedOfUsingCode VARCHAR(1) COMMENT 'Valid Codes: A, C, D, and N.';


CREATE TABLE SegmentActionTypeType (
                SegmentActionTypeTypeID INT NOT NULL,
                SegmentActionTypeCode VARCHAR(1),
                SegmentActionTypeDescription VARCHAR(25) NOT NULL,
                PRIMARY KEY (SegmentActionTypeTypeID)
);

ALTER TABLE SegmentActionTypeType COMMENT 'Instructs the FBI as to what kind of database
activity is to be performed.
Valid Codes: I, M, D, and W.';


CREATE TABLE RelationshipsVictimToOffendersType (
                RelationshipsVictimToOffendersTypeID INT NOT NULL,
                RelationshipsVictimToOffendersCode VARCHAR(2),
                RelationshipsVictimToOffendersDescription VARCHAR(50),
                PRIMARY KEY (RelationshipsVictimToOffendersTypeID)
);

ALTER TABLE RelationshipsVictimToOffendersType COMMENT 'Relationship Code Lookup';


CREATE TABLE AggravatedAssaultHomicideCircumstancesType (
                AggravatedAssaultHomicideCircumstancesTypeID INT NOT NULL,
                AggravatedAssaultHomicideCircumstancesCode VARCHAR(3),
                AggravatedAssaultHomicideCircumstancesDescription VARCHAR(55),
                PRIMARY KEY (AggravatedAssaultHomicideCircumstancesTypeID)
);

ALTER TABLE AggravatedAssaultHomicideCircumstancesType COMMENT 'use to lookup and validate assault and homicide codes';


CREATE TABLE TypePropertyLossEtcType (
                TypePropertyLossEtcTypeID INT NOT NULL,
                TypePropertyLossEtcCode VARCHAR(1),
                TypePropertyLossEtcDescription VARCHAR(85),
                PRIMARY KEY (TypePropertyLossEtcTypeID)
);

ALTER TABLE TypePropertyLossEtcType COMMENT 'Used to lookup and validate property type codes';


CREATE TABLE UCROffenseCodeType (
                UCROffenseCodeTypeID INT NOT NULL,
                UCROffenseCode VARCHAR(3),
                UCROffenseCodeDescription VARCHAR(70),
                PRIMARY KEY (UCROffenseCodeTypeID)
);

ALTER TABLE UCROffenseCodeType COMMENT 'Lookup table for UCR Offense Codes';


CREATE TABLE UCRArrestOffenseCode (
                UCRArrestOffenseCodeID INT NOT NULL,
                UCROffenseCodeTypeID INT NOT NULL,
                PRIMARY KEY (UCRArrestOffenseCodeID)
);


CREATE TABLE ArrestReportSegment (
                ArresteReportSegmentID INT NOT NULL,
                RecordDescriptionWord VARCHAR(4),
                SegmentLevel VARCHAR(1),
                SegmentActionTypeTypeID INT NOT NULL,
                MonthOfTape VARCHAR(2),
                YearOfTape VARCHAR(4),
                CityIndicator VARCHAR(4),
                ORI VARCHAR(9),
                ArresteeTransactionNumber VARCHAR(12),
                ArresteeSequenceNumber INT NOT NULL,
                ArrestDate DATE,
                TypeOfArrestTypeID INT NOT NULL,
                UCRArrestOffenseCodeID INT NOT NULL,
                AgeOfArrestee VARCHAR(4),
                SexOfPersonTypeID INT NOT NULL,
                RaceOfPersonTypeID INT NOT NULL,
                EthnicityOfPersonTypeID INT NOT NULL,
                ResidentStatusOfPersonTypeID INT,
                DispositionOfArresteeUnder18TypeID INT NOT NULL,
                PRIMARY KEY (ArresteReportSegmentID)
);

ALTER TABLE ArrestReportSegment MODIFY COLUMN ArresteReportSegmentID INTEGER COMMENT 'Valid Values: 01 through 99.';

ALTER TABLE ArrestReportSegment MODIFY COLUMN ArresteeTransactionNumber VARCHAR(12) COMMENT 'Left-justified with blank right-fill.';

ALTER TABLE ArrestReportSegment MODIFY COLUMN ArresteeSequenceNumber INTEGER COMMENT 'Valid Values: 01 through 99.';

ALTER TABLE ArrestReportSegment MODIFY COLUMN ArrestDate DATE COMMENT 'In the format of YYYYMMDD';


CREATE TABLE ArresteeSegment (
                ArresteeSegmentID INT NOT NULL,
                RecordDescriptionWord VARCHAR(4),
                SegmentLevel VARCHAR(1),
                SegmentActionTypeTypeID INT NOT NULL,
                MonthOfTape VARCHAR(2),
                YearOfTape VARCHAR(4),
                CityIndicator VARCHAR(4),
                ORI VARCHAR(9),
                IncidentNumber VARCHAR(12),
                ArresteeSequenceNumber INT NOT NULL,
                ArresteeTransactionNumber VARCHAR(12),
                ArrestDate DATE,
                TypeOfArrestTypeID INT NOT NULL,
                MultipleArresteeSegmentsIndicatorTypeID INT NOT NULL,
                UCRArrestOffenseCodeID INT NOT NULL,
                AgeOfArrestee VARCHAR(4),
                SexOfPersonTypeID INT NOT NULL,
                RaceOfPersonTypeID INT NOT NULL,
                EthnicityOfPersonTypeID INT NOT NULL,
                ResidentStatusOfPersonTypeID INT NOT NULL,
                DispositionOfArresteeUnder18TypeID INT NOT NULL,
                ClearanceIndicator BOOLEAN,
                PRIMARY KEY (ArresteeSegmentID)
);

ALTER TABLE ArresteeSegment MODIFY COLUMN ArresteeSegmentID INTEGER COMMENT 'Valid Values: 01 through 99.';

ALTER TABLE ArresteeSegment MODIFY COLUMN ArresteeSequenceNumber INTEGER COMMENT 'Valid Values: 01 through 99.';

ALTER TABLE ArresteeSegment MODIFY COLUMN ArresteeTransactionNumber VARCHAR(12) COMMENT 'Left-justified with blank right-fill.';

ALTER TABLE ArresteeSegment MODIFY COLUMN ArrestDate DATE COMMENT 'In the format of YYYYMMDD';

ALTER TABLE ArresteeSegment MODIFY COLUMN ClearanceIndicator BOOLEAN COMMENT 'd)';


CREATE TABLE ArresteeWasArmedWith (
                ArresteeWasArmedWithID INT NOT NULL,
                ArresteeSegmentID INT NOT NULL,
                ArresteReportSegmentID INT NOT NULL,
                ArresteeWasArmedWithTypeID INT NOT NULL,
                AutomaticeWeaponIndicator VARCHAR(1),
                PRIMARY KEY (ArresteeWasArmedWithID)
);

ALTER TABLE ArresteeWasArmedWith MODIFY COLUMN ArresteeSegmentID INTEGER COMMENT 'Valid Values: 01 through 99.';

ALTER TABLE ArresteeWasArmedWith MODIFY COLUMN ArresteReportSegmentID INTEGER COMMENT 'Valid Values: 01 through 99.';

ALTER TABLE ArresteeWasArmedWith MODIFY COLUMN AutomaticeWeaponIndicator VARCHAR(1) COMMENT 'Enter A if the weapon above is automatic';


CREATE TABLE Offender (
                OffenderSegmentID INT NOT NULL,
                RecordDescriptorWord VARCHAR(4),
                SegmentLevel VARCHAR(1),
                SegmentActionTypeTypeID INT NOT NULL,
                MonthOfTape VARCHAR(2),
                YearOfTape VARCHAR(4),
                CityIndicator VARCHAR(4),
                ORI VARCHAR(9),
                IncidentNumber VARCHAR(12),
                OffenderSequenceNumber INT,
                AgeOfOffender VARCHAR(4),
                SexOfPersonTypeID INT NOT NULL,
                RaceOfPersonTypeID INT NOT NULL,
                PRIMARY KEY (OffenderSegmentID)
);


CREATE TABLE VictimSegment (
                VictimSegmentID INT NOT NULL,
                RecordDescriptorWord VARCHAR(4),
                SegmentLevel VARCHAR(1) NOT NULL,
                SegmentActionTypeTypeID INT NOT NULL,
                MonthOfTape VARCHAR(2),
                YearOfTape VARCHAR(4),
                CityIndicator VARCHAR(4),
                ORI VARCHAR(9),
                IncidentNumber VARCHAR(12),
                VictimSequenceNumber INT,
                TypeOfVictimTypeID INT NOT NULL,
                OfficerActivityCircumstanceTypeID INT NOT NULL,
                OfficerAssignmentTypeTypeID INT NOT NULL,
                AgeOfVictim VARCHAR(4),
                SexOfPersonTypeID INT NOT NULL,
                RaceOfPersonTypeID INT NOT NULL,
                EthnicityOfPersonTypeID INT NOT NULL,
                ResidentStatusOfPersonTypeID INT NOT NULL,
                AdditionalJustifiableHomicideCircumstancesID INT NOT NULL,
                PRIMARY KEY (VictimSegmentID)
);

ALTER TABLE VictimSegment COMMENT 'This segment is linked to the Offense Segment (s) applicable to this victim. There is one
segment per victim.';

ALTER TABLE VictimSegment MODIFY COLUMN VictimSegmentID INTEGER COMMENT 'Valid Values: 001 through 999.';

ALTER TABLE VictimSegment MODIFY COLUMN VictimSequenceNumber INTEGER COMMENT 'Valid Values: 001 through 999';


CREATE TABLE TypeInjury (
                TypeInjuryID INT NOT NULL,
                VictimSegmentID INT NOT NULL,
                TypeInjuryTypeID INT NOT NULL,
                PRIMARY KEY (TypeInjuryID)
);

ALTER TABLE TypeInjury COMMENT 'can contain five occurences';

ALTER TABLE TypeInjury MODIFY COLUMN VictimSegmentID INTEGER COMMENT 'Valid Values: 001 through 999.';


CREATE TABLE AggravatedAssaultHomicideCircumstances (
                AggravatedAssaultHomicideCircumstancesID INT NOT NULL,
                VictimSegmentID INT NOT NULL,
                AggravatedAssaultHomicideCircumstancesTypeID INT NOT NULL,
                PRIMARY KEY (AggravatedAssaultHomicideCircumstancesID)
);

ALTER TABLE AggravatedAssaultHomicideCircumstances MODIFY COLUMN VictimSegmentID INTEGER COMMENT 'Valid Values: 001 through 999.';


CREATE TABLE RelationshipsVictimToOffenders (
                RelationshipsVictimToOffendersID INT NOT NULL,
                VictimSegmentID INT NOT NULL,
                OffenderSegmentID INT NOT NULL,
                RelationshipsVictimToOffendersTypeID INT NOT NULL,
                OffenderNumbersToBeRelated VARCHAR(2),
                PRIMARY KEY (RelationshipsVictimToOffendersID)
);

ALTER TABLE RelationshipsVictimToOffenders MODIFY COLUMN VictimSegmentID INTEGER COMMENT 'Valid Values: 001 through 999.';


CREATE TABLE VictimConnectedToUCROffenseCode (
                VictimConnectedToUCROffenseCodeID INT NOT NULL,
                VictimSegmentID INT NOT NULL,
                UCROffenseCodeTypeID INT NOT NULL,
                PRIMARY KEY (VictimConnectedToUCROffenseCodeID)
);

ALTER TABLE VictimConnectedToUCROffenseCode MODIFY COLUMN VictimSegmentID INTEGER COMMENT 'Valid Values: 001 through 999.';


CREATE TABLE PropertySegment (
                PropertySegmentID INT NOT NULL,
                RecordDescriptionWord VARCHAR(4),
                SegmentLevel VARCHAR(1),
                SegmentActionTypeTypeID INT NOT NULL,
                MonthOfTape VARCHAR(2),
                YearOfTape VARCHAR(4),
                CityIndicator VARCHAR(4),
                ORI VARCHAR(9) NOT NULL,
                IncidentNumber VARCHAR(12) NOT NULL,
                TypePropertyLossEtcTypeID INT NOT NULL,
                NumberOfStolenMotorVehicles VARCHAR(2) NOT NULL,
                NumberOfRecoveredMotorVehicles VARCHAR(2) NOT NULL,
                PRIMARY KEY (PropertySegmentID)
);


CREATE TABLE SuspectedDrugType (
                SuspectedDrugTypeID INT NOT NULL,
                PropertySegmentID INT NOT NULL,
                EstimatedDrugQuantity VARCHAR(9),
                EstimatedDrugQuantityFraction VARCHAR(3),
                SuspectedDrugTypeTypeID INT NOT NULL,
                TypeDrugMeasurementTypeID INT NOT NULL,
                PRIMARY KEY (SuspectedDrugTypeID)
);

ALTER TABLE SuspectedDrugType COMMENT 'used to collect occurrences of drugs as property';

ALTER TABLE SuspectedDrugType MODIFY COLUMN EstimatedDrugQuantity VARCHAR(9) COMMENT 'etc. involved).
e.g., 000002000 for 2,000 grams (GM).';


CREATE TABLE PropertyDescription (
                PropertyDescriptionID INT NOT NULL,
                PropertySegmentID INT NOT NULL,
                PropertyDescriptionTypeID INT NOT NULL,
                ValueOfProperty NUMERIC NOT NULL,
                DateRecovered DATE NOT NULL,
                PRIMARY KEY (PropertyDescriptionID)
);

ALTER TABLE PropertyDescription COMMENT 'Used to store up to 10 occurrences of property items form PropertySegment';

ALTER TABLE PropertyDescription MODIFY COLUMN DateRecovered DATE COMMENT 'a Element 14 is
5 = Recovered.';


CREATE TABLE OffenseSegment (
                OffenseSegmentID INT NOT NULL,
                RecordDescriptorWord VARCHAR(4),
                SegmentLevel VARCHAR(1),
                SegmentActionTypeTypeID INT NOT NULL,
                MonthOfTape VARCHAR(2),
                YearOfTape VARCHAR(4),
                CityIndicator VARCHAR(4),
                ORI VARCHAR(9),
                IncidentNumber VARCHAR(12),
                OffenseAttemptedCompleted VARCHAR(1),
                LocationTypeTypeID INT NOT NULL,
                NumberOfPremisesEntered VARCHAR(2),
                MethodOfEntryTypeID INT NOT NULL,
                BiasMotivationTypeID INT NOT NULL,
                PRIMARY KEY (OffenseSegmentID)
);

ALTER TABLE OffenseSegment COMMENT 'There is one segment for each different UCR offense code (up to ten) associated with this incident.';

ALTER TABLE OffenseSegment MODIFY COLUMN OffenseAttemptedCompleted VARCHAR(1) COMMENT 'Valid Codes: A and C.';


CREATE TABLE TypeOfWeaponForceInvolved (
                TypeOfWeaponForceInvolvedID INT NOT NULL,
                AutomaticeWeaponIndicator VARCHAR(1) NOT NULL,
                OffenseSegmentID INT NOT NULL,
                TypeOfWeaponForceInvolvedTypeID INT NOT NULL,
                PRIMARY KEY (TypeOfWeaponForceInvolvedID)
);

ALTER TABLE TypeOfWeaponForceInvolved COMMENT 'Up to 3 weapon/force involved per offense';

ALTER TABLE TypeOfWeaponForceInvolved MODIFY COLUMN AutomaticeWeaponIndicator VARCHAR(1) COMMENT 'Enter A if the weapon above is automatic';


CREATE TABLE TypeCriminalActivity (
                TypeCriminalActivityID INT NOT NULL,
                OffenseSegmentID INT NOT NULL,
                TypeOfCriminalActivityTypeID INT NOT NULL,
                PRIMARY KEY (TypeCriminalActivityID)
);


CREATE TABLE OffenderSuspectedOfUsing (
                OffenderSuspectedOfUsingID INT NOT NULL,
                OffenseSegmentID INT NOT NULL,
                OffenderSuspectedOfUsingTypeID INT NOT NULL,
                PRIMARY KEY (OffenderSuspectedOfUsingID)
);


CREATE TABLE AdminSeg (
                AdministrativeSegmentID INT NOT NULL,
                RecordDescriptionWord VARCHAR(4),
                SegmentLevel VARCHAR(1),
                SegmentActionTypeTypeID INT NOT NULL,
                MonthOfTape VARCHAR(2),
                YearOfTape VARCHAR(4),
                CityIndicator VARCHAR(4),
                ORI VARCHAR(9),
                IncidentNumber VARCHAR(12),
                IncidentDate DATE,
                ReportDateIndicator VARCHAR(1),
                IncidentHour VARCHAR(4),
                ClearedExceptionallyTypeID INT NOT NULL,
                PRIMARY KEY (AdministrativeSegmentID)
);

ALTER TABLE AdminSeg MODIFY COLUMN IncidentDate DATE COMMENT 'format as YYYYMMDD';

ALTER TABLE AdminSeg MODIFY COLUMN ReportDateIndicator VARCHAR(1) COMMENT 'e was entered in the Incident Date; otherwise, leave blank';

ALTER TABLE AdminSeg MODIFY COLUMN IncidentHour VARCHAR(4) COMMENT 'Enter time in military hours only If unknown, leave blank.';


CREATE TABLE ZeroReportingSegment (
                ZeroReportingSegmentID INT NOT NULL,
                AdministrativeSegmentID INT NOT NULL,
                RecordDescriptionWord VARCHAR(4),
                SegmentLevel VARCHAR(1),
                MonthOfTape VARCHAR(2),
                YearOfTape VARCHAR(4),
                CityIndicator VARCHAR(4),
                IncidentDate DATE,
                ReportDateIndicator VARCHAR(1),
                IncidentHour VARCHAR(4),
                CleardExceptionally VARCHAR(1),
                ExceptionalClearanceDate DATE,
                PRIMARY KEY (ZeroReportingSegmentID)
);

ALTER TABLE ZeroReportingSegment COMMENT 'One record is to be submitted for each month that a reporting agency has responded that
no crime occurred within the local agency’s jurisdiction. Section I, subsection I, “Zero-
Reporting,” contains guidelines on submission of this data record.';

ALTER TABLE ZeroReportingSegment MODIFY COLUMN IncidentDate DATE COMMENT 'format as YYYYMMDD';

ALTER TABLE ZeroReportingSegment MODIFY COLUMN ReportDateIndicator VARCHAR(1) COMMENT 'e was entered in the
Incident Date; otherwise, leave blank';

ALTER TABLE ZeroReportingSegment MODIFY COLUMN CleardExceptionally VARCHAR(1) COMMENT 'Valid Codes: A, B, C, D, E, and N.';

ALTER TABLE ZeroReportingSegment MODIFY COLUMN ExceptionalClearanceDate DATE COMMENT 'use date format YYYYMMDD';


CREATE TABLE LEOKASegment (
                LEOKASegmentID INT NOT NULL,
                AdministrativeSegmentID INT NOT NULL,
                RecordDescriptionWord VARCHAR(4),
                SegmentLevel VARCHAR(1),
                MonthOfTape VARCHAR(2),
                YearOfTape VARCHAR(4),
                CityIndicator VARCHAR(4),
                Filler VARCHAR(12),
                LEOKAData VARCHAR(600),
                PRIMARY KEY (LEOKASegmentID)
);

ALTER TABLE LEOKASegment COMMENT 'Section I contains guidelines on submission of the Law Enforcement Killed and
Assaulted (LEOKA) data record';

ALTER TABLE LEOKASegment MODIFY COLUMN Filler VARCHAR(12) COMMENT 'Must be blanks.';


CREATE TABLE UCROffenseCodeAssociation (
                UCROffenseCodeID INT NOT NULL,
                AdministrativeSegmentID INT NOT NULL,
                OffenseSegmentID INT NOT NULL,
                PropertySegmentID INT NOT NULL,
                VictimSegmentID INT NOT NULL,
                ArresteeSegmentID INT NOT NULL,
                UCROffenseCodeTypeID INT NOT NULL,
                PRIMARY KEY (UCROffenseCodeID)
);

ALTER TABLE UCROffenseCodeAssociation COMMENT 'Occurs 10 times in administrative segment

Valid Code: Volume 1, section IV contains
information on offense codes.
The original incident’s offense(s) must be
entered to enable identification of the
offense(s) being exceptionally cleared.';

ALTER TABLE UCROffenseCodeAssociation MODIFY COLUMN VictimSegmentID INTEGER COMMENT 'Valid Values: 001 through 999.';

ALTER TABLE UCROffenseCodeAssociation MODIFY COLUMN ArresteeSegmentID INTEGER COMMENT 'Valid Values: 01 through 99.';


ALTER TABLE VictimSegment ADD CONSTRAINT officerassignmenttypetype_victimsegment_fk
FOREIGN KEY (OfficerAssignmentTypeTypeID)
REFERENCES OfficerAssignmentTypeType (OfficerAssignmentTypeTypeID)
ON DELETE NO ACTION
ON UPDATE NO ACTION;

ALTER TABLE VictimSegment ADD CONSTRAINT typeofofficeractivitycircumstancetype_victimsegment_fk
FOREIGN KEY (OfficerActivityCircumstanceTypeID)
REFERENCES OfficerActivityCircumstanceType (OfficerActivityCircumstanceTypeID)
ON DELETE NO ACTION
ON UPDATE NO ACTION;

ALTER TABLE TypeOfWeaponForceInvolved ADD CONSTRAINT typeofweaponforceinvolvedtype_typeofweaponforceinvolved_fk
FOREIGN KEY (TypeOfWeaponForceInvolvedTypeID)
REFERENCES TypeOfWeaponForceInvolvedType (TypeOfWeaponForceInvolvedTypeID)
ON DELETE NO ACTION
ON UPDATE NO ACTION;

ALTER TABLE SuspectedDrugType ADD CONSTRAINT suspecteddrugtypetype_suspecteddrugtype_fk
FOREIGN KEY (SuspectedDrugTypeTypeID)
REFERENCES SuspectedDrugTypeType (SuspectedDrugTypeTypeID)
ON DELETE NO ACTION
ON UPDATE NO ACTION;

ALTER TABLE OffenseSegment ADD CONSTRAINT biasmotivationtype_offensesegment_fk
FOREIGN KEY (BiasMotivationTypeID)
REFERENCES BiasMotivationType (BiasMotivationTypeID)
ON DELETE NO ACTION
ON UPDATE NO ACTION;

ALTER TABLE OffenseSegment ADD CONSTRAINT methodofentrytype_offensesegment_fk
FOREIGN KEY (MethodOfEntryTypeID)
REFERENCES MethodOfEntryType (MethodOfEntryTypeID)
ON DELETE NO ACTION
ON UPDATE NO ACTION;

ALTER TABLE OffenseSegment ADD CONSTRAINT locationtypetype_offensesegment_fk
FOREIGN KEY (LocationTypeTypeID)
REFERENCES LocationTypeType (LocationTypeTypeID)
ON DELETE NO ACTION
ON UPDATE NO ACTION;

ALTER TABLE AdminSeg ADD CONSTRAINT clearedexceptionallytype_adminseg_fk
FOREIGN KEY (ClearedExceptionallyTypeID)
REFERENCES ClearedExceptionallyType (ClearedExceptionallyTypeID)
ON DELETE NO ACTION
ON UPDATE NO ACTION;

ALTER TABLE ArresteeSegment ADD CONSTRAINT dispositionofarresteeunder18lookup_arresteesegment_fk
FOREIGN KEY (DispositionOfArresteeUnder18TypeID)
REFERENCES DispositionOfArresteeUnder18Type (DispositionOfArresteeUnder18TypeID)
ON DELETE NO ACTION
ON UPDATE NO ACTION;

ALTER TABLE ArrestReportSegment ADD CONSTRAINT dispositionofarresteeunder18lookup_arrestreportsegment_fk
FOREIGN KEY (DispositionOfArresteeUnder18TypeID)
REFERENCES DispositionOfArresteeUnder18Type (DispositionOfArresteeUnder18TypeID)
ON DELETE NO ACTION
ON UPDATE NO ACTION;

ALTER TABLE ArresteeWasArmedWith ADD CONSTRAINT arresteearmedwithtypelookup_arresteearmedwith_fk
FOREIGN KEY (ArresteeWasArmedWithTypeID)
REFERENCES ArresteeWasArmedWithType (ArresteeWasArmedWithTypeID)
ON DELETE NO ACTION
ON UPDATE NO ACTION;

ALTER TABLE ArresteeSegment ADD CONSTRAINT multiplearresteesegmentsindicatorlookup_arresteesegment_fk
FOREIGN KEY (MultipleArresteeSegmentsIndicatorTypeID)
REFERENCES MultipleArresteeSegmentsIndicatorType (MultipleArresteeSegmentsIndicatorTypeID)
ON DELETE NO ACTION
ON UPDATE NO ACTION;

ALTER TABLE ArresteeSegment ADD CONSTRAINT typeofarrestlookup_arresteesegment_fk
FOREIGN KEY (TypeOfArrestTypeID)
REFERENCES TypeOfArrestType (TypeOfArrestTypeID)
ON DELETE NO ACTION
ON UPDATE NO ACTION;

ALTER TABLE ArrestReportSegment ADD CONSTRAINT typeofarrestlookup_arrestreportsegment_fk
FOREIGN KEY (TypeOfArrestTypeID)
REFERENCES TypeOfArrestType (TypeOfArrestTypeID)
ON DELETE NO ACTION
ON UPDATE NO ACTION;

ALTER TABLE VictimSegment ADD CONSTRAINT additionaljustifiablehomicidecircumstancestype_victimsegment_fk
FOREIGN KEY (AdditionalJustifiableHomicideCircumstancesID)
REFERENCES AdditionalJustifiableHomicideCircumstancesType (AdditionalJustifiableHomicideCircumstancesID)
ON DELETE NO ACTION
ON UPDATE NO ACTION;

ALTER TABLE TypeInjury ADD CONSTRAINT typeofinjurylookup_injurytable_fk
FOREIGN KEY (TypeInjuryTypeID)
REFERENCES TypeInjuryType (TypeInjuryTypeID)
ON DELETE NO ACTION
ON UPDATE NO ACTION;

ALTER TABLE VictimSegment ADD CONSTRAINT residentstatusofpersontype_victimsegment_fk
FOREIGN KEY (ResidentStatusOfPersonTypeID)
REFERENCES ResidentStatusOfPersonType (ResidentStatusOfPersonTypeID)
ON DELETE NO ACTION
ON UPDATE NO ACTION;

ALTER TABLE ArresteeSegment ADD CONSTRAINT residentstatusofpersontype_arresteesegment_fk
FOREIGN KEY (ResidentStatusOfPersonTypeID)
REFERENCES ResidentStatusOfPersonType (ResidentStatusOfPersonTypeID)
ON DELETE NO ACTION
ON UPDATE NO ACTION;

ALTER TABLE VictimSegment ADD CONSTRAINT ethnicityofpersontype_victimsegment_fk
FOREIGN KEY (EthnicityOfPersonTypeID)
REFERENCES EthnicityOfPersonType (EthnicityOfPersonTypeID)
ON DELETE NO ACTION
ON UPDATE NO ACTION;

ALTER TABLE ArresteeSegment ADD CONSTRAINT ethnicityofpersontype_arresteesegment_fk
FOREIGN KEY (EthnicityOfPersonTypeID)
REFERENCES EthnicityOfPersonType (EthnicityOfPersonTypeID)
ON DELETE NO ACTION
ON UPDATE NO ACTION;

ALTER TABLE ArrestReportSegment ADD CONSTRAINT ethnicityofpersontype_arrestreportsegment_fk
FOREIGN KEY (EthnicityOfPersonTypeID)
REFERENCES EthnicityOfPersonType (EthnicityOfPersonTypeID)
ON DELETE NO ACTION
ON UPDATE NO ACTION;

ALTER TABLE VictimSegment ADD CONSTRAINT typeofvictimtype_victimsegment_fk
FOREIGN KEY (TypeOfVictimTypeID)
REFERENCES TypeOfVictimType (TypeOfVictimTypeID)
ON DELETE NO ACTION
ON UPDATE NO ACTION;

ALTER TABLE SuspectedDrugType ADD CONSTRAINT typedrugmeasurementtypeid_suspecteddrugtype_fk
FOREIGN KEY (TypeDrugMeasurementTypeID)
REFERENCES TypeDrugMeasurementTypeID (TypeDrugMeasurementTypeID)
ON DELETE NO ACTION
ON UPDATE NO ACTION;

ALTER TABLE PropertyDescription ADD CONSTRAINT propertydescriptiontype_propertydescription_fk
FOREIGN KEY (PropertyDescriptionTypeID)
REFERENCES PropertyDescriptionType (PropertyDescriptionTypeID)
ON DELETE NO ACTION
ON UPDATE NO ACTION;

ALTER TABLE VictimSegment ADD CONSTRAINT raceofpersontype_victimsegment_fk
FOREIGN KEY (RaceOfPersonTypeID)
REFERENCES RaceOfPersonType (RaceOfPersonTypeID)
ON DELETE NO ACTION
ON UPDATE NO ACTION;

ALTER TABLE Offender ADD CONSTRAINT raceofpersontype_offender_fk
FOREIGN KEY (RaceOfPersonTypeID)
REFERENCES RaceOfPersonType (RaceOfPersonTypeID)
ON DELETE NO ACTION
ON UPDATE NO ACTION;

ALTER TABLE ArresteeSegment ADD CONSTRAINT raceofpersontype_arresteesegment_fk
FOREIGN KEY (RaceOfPersonTypeID)
REFERENCES RaceOfPersonType (RaceOfPersonTypeID)
ON DELETE NO ACTION
ON UPDATE NO ACTION;

ALTER TABLE ArrestReportSegment ADD CONSTRAINT raceofpersontype_arrestreportsegment_fk
FOREIGN KEY (RaceOfPersonTypeID)
REFERENCES RaceOfPersonType (RaceOfPersonTypeID)
ON DELETE NO ACTION
ON UPDATE NO ACTION;

ALTER TABLE Offender ADD CONSTRAINT sexcodelookup_offender_fk
FOREIGN KEY (SexOfPersonTypeID)
REFERENCES SexOfPersonType (SexOfPersonTypeID)
ON DELETE NO ACTION
ON UPDATE NO ACTION;

ALTER TABLE ArrestReportSegment ADD CONSTRAINT sexcodelookup_arrestreportsegment_fk
FOREIGN KEY (SexOfPersonTypeID)
REFERENCES SexOfPersonType (SexOfPersonTypeID)
ON DELETE NO ACTION
ON UPDATE NO ACTION;

ALTER TABLE VictimSegment ADD CONSTRAINT sexofpersontype_victimsegment_fk
FOREIGN KEY (SexOfPersonTypeID)
REFERENCES SexOfPersonType (SexOfPersonTypeID)
ON DELETE NO ACTION
ON UPDATE NO ACTION;

ALTER TABLE ArresteeSegment ADD CONSTRAINT sexofpersontype_arresteesegment_fk
FOREIGN KEY (SexOfPersonTypeID)
REFERENCES SexOfPersonType (SexOfPersonTypeID)
ON DELETE NO ACTION
ON UPDATE NO ACTION;

ALTER TABLE TypeCriminalActivity ADD CONSTRAINT typeofcriminalactivitytype_typecriminalactivity_fk
FOREIGN KEY (TypeOfCriminalActivityTypeID)
REFERENCES TypeOfCriminalActivityType (TypeOfCriminalActivityTypeID)
ON DELETE NO ACTION
ON UPDATE NO ACTION;

ALTER TABLE OffenderSuspectedOfUsing ADD CONSTRAINT offendersuspectedofusingtype_offendersuspectedofusing_fk
FOREIGN KEY (OffenderSuspectedOfUsingTypeID)
REFERENCES OffenderSuspectedOfUsingType (OffenderSuspectedOfUsingTypeID)
ON DELETE NO ACTION
ON UPDATE NO ACTION;

ALTER TABLE AdminSeg ADD CONSTRAINT segmentactiontype_adminseg_fk
FOREIGN KEY (SegmentActionTypeTypeID)
REFERENCES SegmentActionTypeType (SegmentActionTypeTypeID)
ON DELETE NO ACTION
ON UPDATE NO ACTION;

ALTER TABLE OffenseSegment ADD CONSTRAINT segmentactiontype_offensesegment_fk
FOREIGN KEY (SegmentActionTypeTypeID)
REFERENCES SegmentActionTypeType (SegmentActionTypeTypeID)
ON DELETE NO ACTION
ON UPDATE NO ACTION;

ALTER TABLE PropertySegment ADD CONSTRAINT segmentactiontype_propertysegment_fk
FOREIGN KEY (SegmentActionTypeTypeID)
REFERENCES SegmentActionTypeType (SegmentActionTypeTypeID)
ON DELETE NO ACTION
ON UPDATE NO ACTION;

ALTER TABLE VictimSegment ADD CONSTRAINT segmentactiontype_victimsegment_fk
FOREIGN KEY (SegmentActionTypeTypeID)
REFERENCES SegmentActionTypeType (SegmentActionTypeTypeID)
ON DELETE NO ACTION
ON UPDATE NO ACTION;

ALTER TABLE Offender ADD CONSTRAINT segmentactiontype_offender_fk
FOREIGN KEY (SegmentActionTypeTypeID)
REFERENCES SegmentActionTypeType (SegmentActionTypeTypeID)
ON DELETE NO ACTION
ON UPDATE NO ACTION;

ALTER TABLE ArresteeSegment ADD CONSTRAINT segmentactiontype_arresteesegment_fk
FOREIGN KEY (SegmentActionTypeTypeID)
REFERENCES SegmentActionTypeType (SegmentActionTypeTypeID)
ON DELETE NO ACTION
ON UPDATE NO ACTION;

ALTER TABLE ArrestReportSegment ADD CONSTRAINT segmentactiontype_arrestreportsegment_fk
FOREIGN KEY (SegmentActionTypeTypeID)
REFERENCES SegmentActionTypeType (SegmentActionTypeTypeID)
ON DELETE NO ACTION
ON UPDATE NO ACTION;

ALTER TABLE RelationshipsVictimToOffenders ADD CONSTRAINT relationshipcodelookup_offendervictimrelationship_fk
FOREIGN KEY (RelationshipsVictimToOffendersTypeID)
REFERENCES RelationshipsVictimToOffendersType (RelationshipsVictimToOffendersTypeID)
ON DELETE NO ACTION
ON UPDATE NO ACTION;

ALTER TABLE AggravatedAssaultHomicideCircumstances ADD CONSTRAINT aggravatedassaulthomicidecircumstancestype_aggravatedassaulthomicidecircumstances_fk
FOREIGN KEY (AggravatedAssaultHomicideCircumstancesTypeID)
REFERENCES AggravatedAssaultHomicideCircumstancesType (AggravatedAssaultHomicideCircumstancesTypeID)
ON DELETE NO ACTION
ON UPDATE NO ACTION;

ALTER TABLE PropertySegment ADD CONSTRAINT typepropertylossetctype_propertysegment_fk
FOREIGN KEY (TypePropertyLossEtcTypeID)
REFERENCES TypePropertyLossEtcType (TypePropertyLossEtcTypeID)
ON DELETE NO ACTION
ON UPDATE NO ACTION;

ALTER TABLE VictimConnectedToUCROffenseCode ADD CONSTRAINT ucroffensecodetype_victimconnectedtoucroffensecode_fk
FOREIGN KEY (UCROffenseCodeTypeID)
REFERENCES UCROffenseCodeType (UCROffenseCodeTypeID)
ON DELETE NO ACTION
ON UPDATE NO ACTION;

ALTER TABLE UCRArrestOffenseCode ADD CONSTRAINT ucroffensecodetype_ucrarrestoffensecode_fk
FOREIGN KEY (UCROffenseCodeTypeID)
REFERENCES UCROffenseCodeType (UCROffenseCodeTypeID)
ON DELETE NO ACTION
ON UPDATE NO ACTION;

ALTER TABLE UCROffenseCodeAssociation ADD CONSTRAINT ucroffensecodetype_ucroffensecodeassociation_fk
FOREIGN KEY (UCROffenseCodeTypeID)
REFERENCES UCROffenseCodeType (UCROffenseCodeTypeID)
ON DELETE NO ACTION
ON UPDATE NO ACTION;

ALTER TABLE ArresteeSegment ADD CONSTRAINT ucrarrestoffensecode_arresteesegment_fk
FOREIGN KEY (UCRArrestOffenseCodeID)
REFERENCES UCRArrestOffenseCode (UCRArrestOffenseCodeID)
ON DELETE NO ACTION
ON UPDATE NO ACTION;

ALTER TABLE ArrestReportSegment ADD CONSTRAINT ucrarrestoffensecode_arrestreportsegment_fk
FOREIGN KEY (UCRArrestOffenseCodeID)
REFERENCES UCRArrestOffenseCode (UCRArrestOffenseCodeID)
ON DELETE NO ACTION
ON UPDATE NO ACTION;

ALTER TABLE ArresteeWasArmedWith ADD CONSTRAINT arrestreportsegment_arresteewasarmedwith_fk
FOREIGN KEY (ArresteReportSegmentID)
REFERENCES ArrestReportSegment (ArresteReportSegmentID)
ON DELETE NO ACTION
ON UPDATE NO ACTION;

ALTER TABLE ArresteeWasArmedWith ADD CONSTRAINT arresteesegment_arresteewasarmedwith_fk
FOREIGN KEY (ArresteeSegmentID)
REFERENCES ArresteeSegment (ArresteeSegmentID)
ON DELETE NO ACTION
ON UPDATE NO ACTION;

ALTER TABLE UCROffenseCodeAssociation ADD CONSTRAINT arresteesegment_ucroffensecodeassociation_fk
FOREIGN KEY (ArresteeSegmentID)
REFERENCES ArresteeSegment (ArresteeSegmentID)
ON DELETE NO ACTION
ON UPDATE NO ACTION;

ALTER TABLE RelationshipsVictimToOffenders ADD CONSTRAINT offender_relationshipsvictimtooffenders_fk
FOREIGN KEY (OffenderSegmentID)
REFERENCES Offender (OffenderSegmentID)
ON DELETE NO ACTION
ON UPDATE NO ACTION;

ALTER TABLE VictimConnectedToUCROffenseCode ADD CONSTRAINT victimsegment_victimconnectedtoucroffensecode_fk
FOREIGN KEY (VictimSegmentID)
REFERENCES VictimSegment (VictimSegmentID)
ON DELETE NO ACTION
ON UPDATE NO ACTION;

ALTER TABLE RelationshipsVictimToOffenders ADD CONSTRAINT victimsegment_offendervictimrelationship_fk
FOREIGN KEY (VictimSegmentID)
REFERENCES VictimSegment (VictimSegmentID)
ON DELETE NO ACTION
ON UPDATE NO ACTION;

ALTER TABLE UCROffenseCodeAssociation ADD CONSTRAINT victimsegment_ucroffensecodeassociation_fk
FOREIGN KEY (VictimSegmentID)
REFERENCES VictimSegment (VictimSegmentID)
ON DELETE NO ACTION
ON UPDATE NO ACTION;

ALTER TABLE AggravatedAssaultHomicideCircumstances ADD CONSTRAINT victimsegment_aggravatedassaulthomicidecircumstances_fk
FOREIGN KEY (VictimSegmentID)
REFERENCES VictimSegment (VictimSegmentID)
ON DELETE NO ACTION
ON UPDATE NO ACTION;

ALTER TABLE TypeInjury ADD CONSTRAINT victimsegment_injurytable_fk
FOREIGN KEY (VictimSegmentID)
REFERENCES VictimSegment (VictimSegmentID)
ON DELETE NO ACTION
ON UPDATE NO ACTION;

ALTER TABLE PropertyDescription ADD CONSTRAINT propertysegment_propertydescription_fk
FOREIGN KEY (PropertySegmentID)
REFERENCES PropertySegment (PropertySegmentID)
ON DELETE NO ACTION
ON UPDATE NO ACTION;

ALTER TABLE UCROffenseCodeAssociation ADD CONSTRAINT propertysegment_ucroffensecodeassociation_fk
FOREIGN KEY (PropertySegmentID)
REFERENCES PropertySegment (PropertySegmentID)
ON DELETE NO ACTION
ON UPDATE NO ACTION;

ALTER TABLE SuspectedDrugType ADD CONSTRAINT propertysegment_suspecteddrug_fk
FOREIGN KEY (PropertySegmentID)
REFERENCES PropertySegment (PropertySegmentID)
ON DELETE NO ACTION
ON UPDATE NO ACTION;

ALTER TABLE UCROffenseCodeAssociation ADD CONSTRAINT offensesegment_ucroffensecodeassociation_fk
FOREIGN KEY (OffenseSegmentID)
REFERENCES OffenseSegment (OffenseSegmentID)
ON DELETE NO ACTION
ON UPDATE NO ACTION;

ALTER TABLE OffenderSuspectedOfUsing ADD CONSTRAINT offensesegment_offendersuspectedofusing_fk
FOREIGN KEY (OffenseSegmentID)
REFERENCES OffenseSegment (OffenseSegmentID)
ON DELETE NO ACTION
ON UPDATE NO ACTION;

ALTER TABLE TypeCriminalActivity ADD CONSTRAINT offensesegment_typecriminalactivity_fk
FOREIGN KEY (OffenseSegmentID)
REFERENCES OffenseSegment (OffenseSegmentID)
ON DELETE NO ACTION
ON UPDATE NO ACTION;

ALTER TABLE TypeOfWeaponForceInvolved ADD CONSTRAINT offensesegment_typeofweaponforceinvolved_fk
FOREIGN KEY (OffenseSegmentID)
REFERENCES OffenseSegment (OffenseSegmentID)
ON DELETE NO ACTION
ON UPDATE NO ACTION;

ALTER TABLE UCROffenseCodeAssociation ADD CONSTRAINT adminseg_ucroffensecode_fk
FOREIGN KEY (AdministrativeSegmentID)
REFERENCES AdminSeg (AdministrativeSegmentID)
ON DELETE NO ACTION
ON UPDATE NO ACTION;

ALTER TABLE LEOKASegment ADD CONSTRAINT adminseg_leokasegment_fk
FOREIGN KEY (AdministrativeSegmentID)
REFERENCES AdminSeg (AdministrativeSegmentID)
ON DELETE NO ACTION
ON UPDATE NO ACTION;

ALTER TABLE ZeroReportingSegment ADD CONSTRAINT adminseg_zeroreportingsegment_fk
FOREIGN KEY (AdministrativeSegmentID)
REFERENCES AdminSeg (AdministrativeSegmentID)
ON DELETE NO ACTION
ON UPDATE NO ACTION;
