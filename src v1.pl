subset2([],[]).
subset2([X|S], [X|L]):-
	subset2(S,L).
subset2([_|S], L):-
	subset1(S,L).

choosePreferences(Prefs, ChosenPrefs):-
	subset2(Prefs, ChosenPrefs).
	
preferenceSatisfaction(offer(Dest,Act,Cost,Vf,Vt,period(St,End),
 Dur, NoGuests), Customer, [D,period(P1,P2),activity(A)),budget(B)], S):-
	B =< Cost,
	Dest = D,
	overlapPeriod(period(St,End),period(P1,P2)),
	subset2(Act, A),
	offerMean(offer(Dest,Act,Cost,Vf,Vt,period(St,End), Dur, NoGuests),M),
	customerPreferredMean(Customer, M, S1),
	offerAccommodation(offer(Dest,Act,Cost,Vf,Vt,period(St,End),Dur, NoGuests), A),
	customerPreferredAccommodation(Customer, A, S2),
	helperActivities(Customer,Act, S3),
	S is S1 + S2 + S3.
	
helperActivities(_,[],0).
helperActivities(Customer,[H|T], S):-
	customerPreferredActivity(Customer, H, S1),
	helperActivities(Customer, T, S2),
	S is S1+S2.

overlapPeriod(period(Y1-M1-D1, Y2-M2-D2), period(Y3-M3-D3, Y4-M4-D4)):-
	\+((Y1 > Y4; (Y1 == Y4, M1 > M4); (Y1 == Y4, M1 == M4, D1 > D4));
		(Y2 < Y3; (Y2 == Y3, M2 < M3); (Y2 == Y3, M2 == M3, D2 < D3))).

















