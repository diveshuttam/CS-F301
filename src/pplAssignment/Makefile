build: Driver.scala F2016A7TS0045P.scala 
	scalac Driver.scala F2016A7TS0045P.scala

run: build
	scala pplAssignmentDriver 

test: build
	scala pplAssignment.Driver t0.in > t0.check
	scala pplAssignment.Driver t1.in > t1.check
	scala pplAssignment.Driver t2.in > t2.check
