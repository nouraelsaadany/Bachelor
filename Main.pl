male(bob).
male(harry).
child(bob,harry).
bird(pengiun).
bird(ostich).
bird(fl).
% USE sentence(Parse_tree, [the,young,boy,pushed,and,stored,a,big,box,in,the,large,empty,room.after,school], []). to try running

% Rules =================

sentence(s(NP,VP)) --> noun_phrase(NP), verb_phrase(VP).
sentence(s(NP,VP,C)) --> noun_phrase(NP), verb_phrase(VP), conjuction(C).

noun_phrase(np(N)) --> noun_phrase_single(N).
noun_phrase(np(NS,C,N)) --> noun_phrase_single(NS), conjuction(C), noun_phrase(N).

noun_phrase_single(nps(P,N)) --> preposition(P), noun(N).
noun_phrase_single(nps(D,N)) --> det(D), noun(N).
noun_phrase_single(nps(A,N)) --> adjectives(A), noun(N).
noun_phrase_single(nps(D,P,N)) --> det(D), preposition(P), noun(N).
noun_phrase_single(nps(D,A,N)) --> det(D), adjectives(A), noun(N).

noun_phrases(ns(N)) --> noun_phrase(N).
noun_phrases(ns(N,NS)) --> noun_phrase(N), noun_phrases(NS).

verb_long(vl(V)) --> verb(V).
verb_long(vl(A,V)) --> adverb(A), verb(V).

verb_phrase_single(vps(VL)) --> verb_long(VL).
verb_phrase_single(vps(VL,NS)) --> verb_long(VL), noun_phrases(NS).

verb_phrase(vp(VPS)) --> verb_phrase_single(VPS).
verb_phrase(vp(VPS,C,V)) --> verb_phrase_single(VPS), conjuction(C), verb_phrase(V).

adjectives(adjs(A)) --> adjective(A).
adjectives(adjs(A,AS)) --> adjective(A), adjectives(AS).

% Knowledge base ========

conjuction(con(and)) --> [and].

det(d(the)) --> [the].
det(d(a)) --> [a].
det(d(an)) --> [an].
det(d(this)) --> [this].
det(d(that)) --> [that].
det(d(these)) --> [these].
det(d(those)) --> [those].
det(d(my)) --> [my].
det(d(your)) --> [your].
det(d(his)) --> [his].
det(d(her)) --> [her].
det(d(its)) --> [its].
det(d(our)) --> [our].
det(d(their)) --> [their].
det(d(few)) --> [few].
det(d(much)) --> [much].
det(d(many)) --> [many].
det(d(some)) --> [some].
det(d(any)) --> [any].
det(d(enough)) --> [enough].

preposition(prop(in)) --> [in].
preposition(prop(on)) --> [on].
preposition(prop(with)) --> [with].
preposition(prop(at)) --> [at].
preposition(prop(from)) --> [from].
preposition(prop(into)) --> [into].
preposition(prop(of)) --> [of].
preposition(prop(to)) --> [to].
preposition(prop(for)) --> [for].
preposition(prop(by)) --> [by].
preposition(prop(after)) --> [after].
preposition(prop(under)) --> [under].
preposition(prop(behind)) --> [behind].
preposition(prop(before)) --> [before].
preposition(prop(across)) --> [across].
preposition(prop(down)) --> [down].
preposition(prop(near)) --> [near].
preposition(prop(off)) --> [off].
preposition(prop(above)) --> [above].
preposition(prop(during)) --> [during].
preposition(prop(including)) --> [including].
preposition(prop(until)) --> [until].
preposition(prop(against)) --> [against].
preposition(prop(throughout)) --> [throughout].
preposition(prop(towards)) --> [towards].
preposition(prop(upon)) --> [upon].

adjective(adj(young)) --> [young].
adjective(adj(old)) --> [old].
adjective(adj(big)) --> [big].
adjective(adj(large)) --> [large].
adjective(adj(empty)) --> [empty].
adjective(adj(poor)) --> [poor].
adjective(adj(white)) --> [white].
adjective(adj(black)) --> [black].
adjective(adj(red)) --> [red].
adjective(adj(yellow)) --> [yellow].
adjective(adj(blue)) --> [blue].
adjective(adj(orange)) --> [orange].
adjective(adj(green)) --> [green].
adjective(adj(grey)) --> [grey].
adjective(adj(gray)) --> [gray].
adjective(adj(brilliant)) --> [brilliant].
adjective(adj(bright)) --> [bright].
adjective(adj(talented)) --> [talented].
adjective(adj(clean)) --> [clean].
adjective(adj(beautiful)) --> [beautiful].
adjective(adj(long)) --> [long].
adjective(adj(short)) --> [short].
adjective(adj(easy)) --> [easy].
adjective(adj(hard)) --> [hard].
adjective(adj(difficult)) --> [difficult].
adjective(adj(brave)) --> [brave].
adjective(adj(nice)) --> [nice].
adjective(adj(calm)) --> [calm].
adjective(adj(fat)) --> [fat].
adjective(adj(thin)) --> [thin].
adjective(adj(quick)) --> [quick].
adjective(adj(slow)) --> [slow].
adjective(adj(fast)) --> [fast].
adjective(adj(cold)) --> [cold].
adjective(adj(hot)) --> [hot].
adjective(adj(heavy)) --> [heavy].
adjective(adj(light)) --> [light].
adjective(adj(normal)) --> [normal].

adverb(adv(quickly)) --> [quickly].
adverb(adv(slowly)) --> [slowly].
adverb(adv(carefully)) --> [carefully].
adverb(adv(suddenly)) --> [suddenly].
adverb(adv(quickly)) --> [quickly].
adverb(adv(quietly)) --> [quietly].
adverb(adv(daily)) --> [daily].
adverb(adv(weekly)) --> [weekly].
adverb(adv(never)) --> [never].
adverb(adv(often)) --> [often].
adverb(adv(early)) --> [early].
adverb(adv(nearly)) --> [nearly].
adverb(adv(simply)) --> [simply].
adverb(adv(certainly)) --> [certainly].
adverb(adv(recently)) --> [recently].
adverb(adv(usually)) --> [usually].
adverb(adv(hardly)) --> [hardly].

noun(n(boy)) --> [boy].
noun(n(girl)) --> [girl].
noun(n(man)) --> [man].
noun(n(woman)) --> [woman].
noun(n(person)) --> [person].
noun(n(box)) --> [box].
noun(n(room)) --> [room].
noun(n(school)) --> [school].
noun(n(building)) --> [building].
noun(n(shed)) --> [shed].
noun(n(envelop)) --> [envelop].
noun(n(tree)) --> [tree].
noun(n(car)) --> [car].
noun(n(guitar)) --> [guitar].
noun(n(shop)) --> [shop].
noun(n(bag)) --> [bag].
noun(n(table)) --> [table].
noun(n(chair)) --> [chair].
noun(n(student)) --> [student].
noun(n(lecturer)) --> [lecturer].
noun(n(professor)) --> [professor].
noun(n(scientist)) --> [scientist].
noun(n(researcher)) --> [researcher].
noun(n(boys)) --> [boys].
noun(n(girls)) --> [girls].
noun(n(mans)) --> [mans].
noun(n(women)) --> [women].
noun(n(people)) --> [people].
noun(n(boxes)) --> [boxes].
noun(n(rooms)) --> [rooms].
noun(n(schools)) --> [schools].
noun(n(buildings)) --> [buildings].
noun(n(sheds)) --> [sheds].
noun(n(envelops)) --> [envelops].
noun(n(trees)) --> [trees].
noun(n(cars)) --> [cars].
noun(n(guitars)) --> [guitars].
noun(n(shops)) --> [shops].
noun(n(bags)) --> [bags].
noun(n(tables)) --> [tables].
noun(n(chairs)) --> [chairs].
noun(n(students)) --> [students].
noun(n(lecturers)) --> [lecturers].
noun(n(professors)) --> [professors].
noun(n(scientists)) --> [scientists].
noun(n(researchers)) --> [researchers].
noun(n(bat)) --> [bat].
noun(n(cat)) --> [cat].

verb(v(push)) --> [push].
verb(v(store)) --> [store].
verb(v(give)) --> [give].
verb(v(climb)) --> [climb].
verb(v(watch)) --> [watch].
verb(v(admire)) --> [admire].
verb(v(appreciate)) --> [appreciate].
verb(v(play)) --> [play].
verb(v(eat)) --> [eat].
verb(v(sleep)) --> [sleep].
verb(v(study)) --> [study].
verb(v(work)) --> [work].
verb(v(travel)) --> [travel].
verb(v(pushed)) --> [pushed].
verb(v(stored)) --> [stored].
verb(v(gave)) --> [gave].
verb(v(climbed)) --> [climbed].
verb(v(watched)) --> [watched].
verb(v(admired)) --> [admired].
verb(v(appreciated)) --> [appreciated].
verb(v(played)) --> [played].
verb(v(ate)) --> [ate].
verb(v(slept)) --> [slept].
verb(v(studied)) --> [studied].
verb(v(worked)) --> [worked].
verb(v(travelled)) --> [travelled].



% lemma(+Lemma,+Category)
% --------------------------------------------------------------------
lemma(a,dtexists).
lemma(all,dtforall).
lemma(almond, adj).
lemma(almond, n).
lemma(an,dtexists).
lemma(are, be).
lemma(banana, n).
lemma(belong,tv).
lemma(blue,adj).
lemma(bottom, adj).
lemma(bowl, n).
lemma(box,n).
lemma(contain, ppred).
lemma(contain,tv).
lemma(container, n).
lemma(did, aux).
lemma(does, aux).
lemma(drank, tv).
lemma(drink, tv).
lemma(each,dtforall).
lemma(eat,tv).
lemma(egg,n).
lemma(empty, adj).
lemma(every,dtforall).
lemma(freezer,n).
lemma(fridge, n).
lemma(green,adj).
lemma(ham,n).
lemma(has,tv).
lemma(in,p).
lemma(inside,p).
lemma(is,be).
lemma(meat, n).
lemma(mia,pn).
lemma(middle, adj ).
lemma(milk, n).
lemma(no,dtnot).
lemma(not, dtnot).
lemma(of,p).
lemma(on,vacp).   
lemma(popsicle, n).
lemma(red,adj).
lemma(sam, pn).
lemma(sandwich,n).
lemma(saw,tv).
lemma(see,tv).
lemma(shelf, n).
lemma(sneeze,iv).
lemma(some,dtexists).
lemma(sue,pn).
lemma(that,rel).
lemma(the,dt).
lemma(there,aux).
lemma(to,aux).
lemma(to,vacp).
lemma(tom,pn).
lemma(top, adj).
lemma(two,dt).
lemma(under,p).
lemma(was,be).
lemma(watermelon, n).
lemma(what,whthing).
lemma(which, whthing).
lemma(white,adj).
lemma(who, whperson).
lemma(yellow,adj).


alpha(not(not(A)), A).

alpha(and(A1, A2), A1, A2).
alpha(not(or(A1, A2)), not(A1), not(A2)).
alpha(not(imp(A1, A2)), A1, not(A2)).
alpha(equ(A1, A2), imp(A1, A2), imp(A2, A1)).

beta(or(A1, A2), A1, A2).
beta(not(and(A1, A2)), not(A1), not(A2)).
beta(imp(A1, A2), not(A1), A2).
beta(not(equ(A1, A2)), not(imp(A1, A2)), not(imp(A2, A1))).

gamma(all(X, A), X, A).
gamma(not(ex(X, A)), X, not(A)).

delta(ex(X, A), X, A).
delta(not(all(X, A)), X, not(A)).


son(X,Y):-
    male(X), child(X,Y).

forAll(X,Y):- terminal(X),terminal(y), connect(x,y).
fly(X, WING):-
    bird(X),
    X \= penguin, 
    X \= ostrich,
    WING \=broken.