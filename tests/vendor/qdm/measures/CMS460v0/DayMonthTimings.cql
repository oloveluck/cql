library DayMonthTimings version '0.6.000'

using QDM version '5.3'

codesystem "LOINC": 'urn:oid:2.16.840.1.113883.6.1'
codesystem "SNOMEDCT": 'urn:oid:2.16.840.1.113883.6.96'

code "Birthdate": '21112-8' from "LOINC" display 'Birthdate'
code "Dead": '419099009' from "SNOMEDCT" display 'Dead'

parameter "Measurement Period" Interval<DateTime>

context Patient

define "Months":
	{ 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12 }

define "Months Containing 28 Days":
	{ 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28 }

define "Months Containing 30 Days":
	{ 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30 }

define "Months Containing 31 Days":
	{ 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31 }

define "Months Containing 29 Days":
	{ 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29 }

define function "DaysInYear"(yr Integer):
	flatten ( Months Mo
			return "DaysInMonth"(yr, Mo)
	)

define function "IsLeapYear"(yr Integer):
	( yr mod 4 = 0
			and yr mod 100 != 0
	)
		or yr mod 400 = 0

define function "DaysInPeriod"(period Interval<DateTime>):
	case
		when start of period is null
			or 
			end of period is null then null
		when start of period same year as 
		end of period then case
			when month from start of period = month from 
			end of period then ( "DaysInMonth"(month from start of period, year from start of period)) D
				where D same day or after start of period
					and D same day or before 
					end of period 
			else ( "DaysInYear"(year from start of period)) D
			where D same day or after start of period
				and D same day or before 
				end of period 
		end
		when difference in years between start of period and 
		end of period = 1 then ( ( "DaysInYear"(year from start of period)) D
				where D same day or after start of period
					and D same day or before 
					end of period
		)
			union ( ( "DaysInYear"(year from 
					end of period
				)) D
					where D same day or after start of period
						and D same day or before 
						end of period
			) 
		else null 
	end

define function "DaysInMonth"(yr Integer, mo Integer):
	(
	case
		when mo in { 1, 3, 5, 7, 8, 10, 12 } then "Months Containing 31 Days"
		when mo = 2 then if "IsLeapYear"(yr)then "Months Containing 29 Days" 
			else "Months Containing 28 Days" 
		else "Months Containing 30 Days" 
	end ) D
		return DateTime(yr, mo, D)
