drop table if exists ArrestView;

create table ArrestView as
select
  Arrest.AgencyID,
  Arrest.ArrestDateTypeID,
  Arrest.ArrestID,
  Booking.BookingDateTypeID,
  Arrest.LocationTypeID,
  Arrest.PersonID,
  Person.PersonAgeTypeID,
	Person.PersonRaceTypeID,
	Person.PersonSexTypeID,
  LocationType.CensusPlaceID,
  LocationType.CountyID,
  LocationType.JusticeFacilityLocationID,
  Booking.DaysSinceLastBooking,
  Booking.BookingID
from Booking, Arrest, Person, LocationType
where Booking.BookingID=Arrest.BookingID and Arrest.PersonID=Person.PersonID and Arrest.LocationTypeID=LocationType.LocationTypeID;

drop table if exists ChargeView;

create table ChargeView as
select
  ArrestView.*,
  Charge.ChargeID,
  Charge.ChargeTypeID,
  Charge.ChargeClassTypeID
from ArrestView, Charge
where ArrestView.ArrestID=Charge.ArrestID;

drop table if exists ProbationSupervisionView;

create table ProbationSupervisionView as
select
  ProbationSupervision.PersonID,
  ProbationSupervision.ProbationSupervisionID,
  ProbationSupervision.TermEndDateTypeID,
  ProbationSupervision.TermStartDateTypeID,
  Person.PersonAgeTypeID,
  Person.PersonRaceTypeID,
  Person.PersonSexTypeID
from ProbationSupervision, Person
where ProbationSupervision.PersonID=Person.PersonID;

drop table if exists ProbationSupervisionStatusView;

create table ProbationSupervisionStatusView as
select
  ProbationSupervisionView.*,
  ProbationSupervisionStatus.ProbationSupervisionStatusID,
  ProbationSupervisionStatus.ProbationOfficialID,
  ProbationSupervisionStatus.SupervisionStatusTypeID,
  ProbationSupervisionStatus.SupervisionLevelTypeID,
  ProbationSupervisionStatus.AgencyID
from ProbationSupervisionView, ProbationSupervisionStatus
where ProbationSupervisionView.ProbationSupervisionID=ProbationSupervisionStatus.ProbationSupervisionID;

drop table if exists ProbationTermArrestView;

create table ProbationTermArrestView as
select
  Arrest.ArrestID,
  ifnull(Arrest.AgencyID, 98) as ArrestAgencyID,
  ifnull(Arrest.ArrestDateTypeID, 99998) as ArrestDateTypeID,
  ifnull(Booking.BookingDateTypeID, 99998) as BookingDateTypeID,
  ifnull(ArrestPerson.PersonAgeTypeID, 998) as ArrestPersonAgeTypeID,
  ifnull(ArrestPerson.PersonRaceTypeID, 98) as ArrestPersonRaceTypeID,
  ifnull(ArrestPerson.PersonSexTypeID, 98) as ArrestPersonSexTypeID,
  ifnull(LocationType.CensusPlaceID, 99998) as CensusPlaceID,
  ifnull(LocationType.CountyID, 99998) as CountyID,
  ifnull(LocationType.JusticeFacilityLocationID, 98) as JusticeFacilityLocationID,
  ifnull(Charge.ChargeTypeID, 99998) as ChargeTypeID,
  ifnull(Charge.ChargeClassTypeID, 98) as ChargeClassTypeID,
  ProbationSupervision.PersonID as ProbationSupervisionPersonID,
  ProbationSupervision.ProbationSupervisionID,
  ProbationSupervision.TermEndDateTypeID,
  ProbationSupervision.TermStartDateTypeID,
  ProbationSupervisionPerson.PersonAgeTypeID as ProbationSupervisionPersonAgeTypeID,
  ProbationSupervisionPerson.PersonRaceTypeID as ProbationSupervisionPersonRaceTypeID,
  ProbationSupervisionPerson.PersonSexTypeID as ProbationSupervisionPersonSexTypeID,
  ProbationSupervisionStatus.ProbationSupervisionStatusID,
  ProbationSupervisionStatus.ProbationOfficialID,
  ProbationSupervisionStatus.SupervisionStatusTypeID,
  ProbationSupervisionStatus.SupervisionLevelTypeID,
  ProbationSupervisionStatus.AgencyID as ProbationAgencyID,
  Booking.DaysSinceLastBooking,
  ProbationTermArrestID
from
  (ProbationSupervision inner join ProbationSupervisionStatus on ProbationSupervision.ProbationSupervisionID=ProbationSupervisionStatus.ProbationSupervisionID
    inner join Person as ProbationSupervisionPerson on ProbationSupervision.PersonID=ProbationSupervisionPerson.PersonID)
  left join (
    ProbationTermArrest inner join Arrest on Arrest.ArrestID=ProbationTermArrest.ArrestID
      inner join Person as ArrestPerson on Arrest.PersonID=ArrestPerson.PersonID
      inner join LocationType on Arrest.LocationTypeID=LocationType.LocationTypeID
      inner join Charge on Arrest.ArrestID=Charge.ArrestID
      inner join Booking on Booking.BookingID=Arrest.BookingID
  ) on ProbationSupervision.ProbationSupervisionID=ProbationTermArrest.ProbationSupervisionID;
