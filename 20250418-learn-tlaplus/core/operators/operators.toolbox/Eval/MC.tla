---- MODULE MC ----
EXTENDS operators, TLC

\* Constant expression definition @modelExpressionEval
const_expr_174492765961623000 == 
<<
FiveMinutesInSeconds,
AbsNegative5,
AbsPositive5,
FalseImpliesFalse,
FalseImpliesTrue,
TrueImpliesFalse,
TrueImpliesTrue,
ClockType,
ClockTypeCardinality,
ToClock30573
>>
----

\* Constant expression ASSUME statement @modelExpressionEval
ASSUME PrintT(<<"$!@$!@$!@$!@$!",const_expr_174492765961623000>>)
----

=============================================================================
\* Modification History
\* Created Fri Apr 18 08:07:39 AEST 2025 by isaac
