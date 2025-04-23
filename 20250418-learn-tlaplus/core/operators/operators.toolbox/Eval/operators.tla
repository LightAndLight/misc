---- MODULE operators ----
EXTENDS Integers, FiniteSets

SecondsPerMinute == 60

MinutesToSeconds(m) == m * SecondsPerMinute

FiveMinutesInSeconds == MinutesToSeconds(5)

Abs(x) == IF x < 0 THEN -x ELSE x

AbsNegative5 == Abs(-5)
AbsPositive5 == Abs(5)

FalseImpliesFalse == FALSE => FALSE
FalseImpliesTrue == FALSE => TRUE
TrueImpliesFalse == TRUE => FALSE
TrueImpliesTrue == TRUE => TRUE

ToSeconds(time) == time[1]*3600 + time[2]*60 + time[3]

AddTimes(t1, t2) == <<t1[1] + t2[1], t1[2] + t2[2], t1[3] + t2[3]>>

ClockType == (0..23) \X (0..59) \X (0..59)
ClockTypeCardinality == Cardinality(ClockType)

ToClock(seconds) == CHOOSE x \in ClockType: ToSeconds(x) = seconds
ToClock30573 == ToClock(30573)
====