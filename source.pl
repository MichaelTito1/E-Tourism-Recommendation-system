

subset2([],[]).
subset2([X|S], [X|L]):-
	subset2(S,L).
subset2([activity(X)|S],[activity(M)|L]):-
	subset2(X,M),
	M\=[],
	subset2(S,L).	
	
subset2([_|S], L):-
	subset2(S,L).

possibleSubset(L,R):-
	subset2(L,Z),
	permutation(Z,R).
	
	
choosePreferences(L,R):-
	subset2(L,R).
	

	


overlapPeriod(period(Y1-M1-D1, Y2-M2-D2), period(Y3-M3-D3, Y4-M4-D4)):-
	\+((Y1 > Y4; (Y1 == Y4, M1 > M4); (Y1 == Y4, M1 == M4, D1 > D4));
		(Y2 < Y3; (Y2 == Y3, M2 < M3); (Y2 == Y3, M2 == M3, D2 < D3))).




getOffer([period(P1,P2)|T],offer(A,B,C,D,E,F,G,H)):-

offerMean(offer(A,B,C,D,E,period(P3,P4),G,H),_),
overlapPeriod(period(P1,P2),period(P3,P4)),
getOffer(T,offer(A,B,C,D,E,F,G,H)).


getOffer([dest(A)|T],offer(A,B,C,D,E,F,G,H)):-

offerMean(offer(A,B,C,D,E,F,G,H),_),
getOffer(T,offer(A,B,C,D,E,F,G,H)).


getOffer([activity(P)|T],offer(A,B,C,D,E,F,G,H)):-

offerMean(offer(A,B,C,D,E,F,G,H),_),
possibleSubset(B,P),
getOffer(T,offer(A,B,C,D,E,F,G,H)).

getOffer([budget(P)|T],offer(A,B,C,D,E,F,G,H)):-
offerMean(offer(A,B,C,D,E,F,G,H),_),
P>=C,
getOffer(T,offer(A,B,C,D,E,F,G,H)).

getOffer([mean(M)|T],offer(A,B,C,D,E,F,G,H)):-
offerMean(offer(A,B,C,D,E,F,G,H),M),
getOffer(T,offer(A,B,C,D,E,F,G,H)).

getOffer([accommodation(M)|T],offer(A,B,C,D,E,F,G,H)):-
offerAccommodation(offer(A,B,C,D,E,F,G,H),M),
getOffer(T,offer(A,B,C,D,E,F,G,H)).

getOffer([],offer(A,B,C,D,E,F,G,H)).


recommendOfferForCustomer(Prefs, ChosenPrefs, O):-
possibleSubset(Prefs,ChosenPrefs),
ChosenPrefs\=[],
getOffer(ChosenPrefs,O).

preferenceSatisfaction(O, Customer, ChosenPrefs, S):-
%	O = offer(_, Act, _, _, _, _, _, _),
	loopChosenPrefs(O,ChosenPrefs,Customer,0,S).

loopChosenPrefs(_,[],_,Acc,Acc).
loopChosenPrefs(O,[H|T],Customer,R,Z):-
	H = accommodation(A),
	offerAccommodation(O, A),
	customerPreferredAccommodation(Customer, A, X),
	NewR is R + X,
	loopChosenPrefs(O, T,Customer, NewR, Z).
loopChosenPrefs(O,[H|T],Customer,R,Z):-
	H = mean(M),
	offerMean(O, M),
	customerPreferredMean(Customer, M, X),
	NewR is R + X,
	loopChosenPrefs(O, T,Customer, NewR, Z).
loopChosenPrefs(O,[H|T],Customer,R,Z):-
	H = activity(A),
	O = offer(_, Act, _, _, _, _, _, _),
	possibleSubset(Act, A),
	helperActivities(Customer, A, X),
	NewR is R + X,
	loopChosenPrefs(O, T,Customer, NewR, Z).
loopChosenPrefs(O,[H|T],Customer,R,Z):-
	\+(H = accommodation(A);H = mean(M);H = activity(A)),
	loopChosenPrefs(O, T,Customer, R, Z).

helperActivities(_,[],0).
helperActivities(Customer,[H|T], S):-
	customerPreferredActivity(Customer, H, S1),
	helperActivities(Customer, T, S2),
	S is S1+S2.
	
	
	
	
recommendOffer([Customer|T1],[PreferenceList|T2],offer(A,B,C,D,E,F,G,H),CustomerChosen):-
	Accumulator1=[],
	recommendOfferForCustomer(PreferenceList, ChosenPrefs,offer(A,B,C,D,E,F,G,H)),

	recommendOfferHelper([Customer|T1],[PreferenceList|T2],offer(A,B,C,D,E,F,G,H),Accumulator1,CustomerSatisfactionList),
	
	insertion_sort(CustomerSatisfactionList,SortedCustomerSatisfactionList),
	Accumulator2=[],
	getChosenCustomers(SortedCustomerSatisfactionList,H,Accumulator2,CustomerChosen).

	
	
recommendOfferHelper([],[],offer(A,B,C,D,E,F,G,H),M,M).	
recommendOfferHelper([Customer|T1],[PreferenceList|T2],offer(A,B,C,D,E,F,G,H),Accumulator,CustomerSatisfactionList):-
    recommendOfferForCustomer(PreferenceList, ChosenPrefs,offer(A,B,C,D,E,F,G,H) ),
	preferenceSatisfaction(offer(A,B,C,D,E,F,G,H),Customer,ChosenPrefs,CustomerSatisfaction),
	
	append([cs(Customer,CustomerSatisfaction)],Accumulator,NewAccumulator),
	recommendOfferHelper(T1,T2,offer(A,B,C,D,E,F,G,H),NewAccumulator,CustomerSatisfactionList).
	
getChosenCustomers([],_,A,A).
getChosenCustomers(_,0,A,A).	
getChosenCustomers([cs(A,B)|T],C,Accumulator,CustomerChosen):-
	C>0,
	C1 is C-1,
	append(Accumulator,[A],NewAccumulator),
	getChosenCustomers(T,C1,NewAccumulator,CustomerChosen).
	



insertion_sort(List,Sorted):- i_sort(List,[],Sorted).
i_sort([],X,X).
i_sort([H|T],Accumulator,Sorted):- insert(H,Accumulator,N),i_sort(T,N,Sorted).
insert(X,[],[X]).
insert(cs(C1,X),[cs(C2,Y)|T],[cs(C2,Y)|NT]):- X<Y,insert(cs(C1,X),T,NT).
insert(cs(C1,X),[cs(C2,Y)|T],[cs(C1,X),cs(C2,Y)|T]):- X>=Y.
