
% offerMean(X, Y) -> Transportation mean Y is used with offer X

offerMean(offer(dahab, [diving, snorkeling, horseRiding], 10000, 2020-02-12, 2020-03-12, period(2020-03-15, 2020-04-15), 10, 5), bus).
offerMean(offer(taba, [diving], 1000, 2020-02-12, 2020-03-12, period(2020-06-01, 2020-08-31), 10, 4), bus).

% offerAccommodation(X, Y) -> Accommodation Y is part of offer X

offerAccommodation(offer(dahab, [diving, snorkeling, horseRiding], 10000, 2020-02-12, 2020-03-12, period(2020-03-15, 2020-04-15), 10, 5), hotel).
offerAccommodation(offer(taba, [diving], 1000, 2020-02-12, 2020-03-12, period(2020-06-01, 2020-08-31), 10, 4), hotel).

% customerPreferredActivity(X, Y, R) -> Y is the preferred activity kind wrt customer X with relevance R

customerPreferredActivity(customer(ahmed, aly, 1993-01-30, single, 0, student), diving, 100).
customerPreferredActivity(customer(ahmed, aly, 1993-01-30, single, 0, student), snorkeling, 100).
customerPreferredActivity(customer(ahmed, aly, 1993-01-30, single, 0, student), horseRiding, 20).
customerPreferredActivity(customer(mohamed, elkasad, 1999-01-30, single, 0, student), snorkeling, 60).
customerPreferredActivity(customer(mohamed, elkasad, 1999-01-30, single, 0, student), diving, 20).
customerPreferredActivity(customer(mohamed, elkasad, 1999-01-30, single, 0, student), horseRiding, 50).

% customerPreferredMean(X, Y, R) -> Y is the preferred transportaion mean wrt customer X with relevance R

customerPreferredMean(customer(ahmed, aly, 1993-01-30, single, 0, student), bus, 100).
customerPreferredMean(customer(mohamed, elkasad, 1999-01-30, single, 0, student), bus, 10).

% customerPreferredAccommodation(X, Y, R) -> Y is the preferred accommodation to customer X with relevance R

customerPreferredAccommodation(customer(ahmed, aly, 1993-01-30, single, 0, student), hotel, 20).
customerPreferredAccommodation(customer(mohamed, elkasad, 1999-01-30, single, 0, student), hotel, 100).


subset2([],[]).
subset2([X|S], [X|L]):-
	subset2(S,L).
subset2([_|S], L):-
	subset2(S,L).

possibleSubset(L, R):-
	subset2(L,Z),
	permutation(Z,R).
	
choosePreferences([dest(D), period(P1, P2),accommodation(A),
activity(Act), budget(B)], ChosenPrefs):-
	subset2(Act, A1),
	subset2([dest(D), period(P1, P2),accommodation(A),budget(B)], A2),
	append([activity(append(A1,A2))], ChosenPrefs). %ERROR HERE
	
preferenceSatisfaction(offer(Dest,Act,Cost,Vf,Vt,period(St,End),
 Dur, NoGuests), Customer, [D,period(P1,P2),activity(A),budget(B)], S):-
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

getOffer([dest(D),period(P1,P2),activity(A),budget(B)], O):-
	O = offer(Dest,Act,Cost, _,_,period(St,End),_, _),
	((D = Dest,
	overlapPeriod(period(P1,P2), period(St, End)),
	subset2(Act,A),
	B >= Cost);
	overlapPeriod(period(P1,P2), period(St, End))).

recommendOfferForCustomer(Prefs, ChosenPrefs, Offer):-
	choosePreferences(Prefs, ChosenPrefs),
	getOffer(ChosenPrefs, Offer).

%recommendOffer(Customers, PreferenceList, Offer, CustomersChosen):-
	











