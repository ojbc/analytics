create view ArrestFullView as
	select ArrestID,
		AgencyTypeDescription as ArrestingAgency,
		ParentAgencyDescription as ArrestingParentAgency,
		AgencyTypeCategory as ArrestingAgencyCategory,
		AgencyTypeDetailCategory as ArrestingAgencyDetailCategory,
		ad.CalendarDate as ArrestDate,
		CountyName as ArrestingAgencyCounty,
		ChargeSeverityTypeDescription as ChargeSeverity,
		SexTypeDescription as ArresteeSex,
		RaceTypeDescription as ArresteeRace,
		ChargeOriginTypeDescription as ChargeOrigin,
		DispositionTypeDescription as Disposition,
		if(dd.CalendarDate='1899-12-31', NULL, dd.CalendarDate) as DispositionDate,
		CourtTypeDescription as Court,
		CourtTypeCategory as CourtCategory,
		CourtJurisdictionName as CourtJurisdictionName,
		AgeYearsTypeDescription as ArresteeAge,
		AgeGroupType1Description as AgeGroup,
		HasDisposition,
		DaysToDispose,
		DaysSincePriorArrest
	from Arrest, AgencyType, CountyType, DateType ad, DateType dd, ChargeSeverityType, SexType, RaceType, ChargeOriginType, CourtType, DispositionType, AgeYearsType, AgeGroupType
	where Arrest.ArrestDateTypeID=ad.DateTypeID and
		Arrest.DispositionDateTypeID=dd.DateTypeID and
		Arrest.ArrestingAgencyTypeID=AgencyType.AgencyTypeID and
		AgencyType.CountyTypeID=CountyType.CountyTypeID and
		Arrest.ChargeSeverityTypeID=ChargeSeverityType.ChargeSeverityTypeID and
		Arrest.ArresteeSexTypeID=SexType.SexTypeID and
		Arrest.ArresteeRaceTypeID=RaceType.RaceTypeID and
		Arrest.ArresteeAgeYearsTypeID=AgeYearsType.AgeYearsTypeID and
		AgeYearsType.AgeGroupTypeID=AgeGroupType.AgeGroupTypeID and
		Arrest.ChargeOriginTypeID=ChargeOriginType.ChargeOriginTypeID and
		Arrest.DispositionTypeID=DispositionType.DispositionTypeID and
		Arrest.CourtTypeID=CourtType.CourtTypeID;