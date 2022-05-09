% Start variable
s(DSS, A, Z) :- declarative_sentences(DSS, A, Z).
s(ISS, A, Z) :- interrogatory_sentences(ISS, A, Z).


% Structures
declarative_sentences(dec_ss(DS), A, Z) :- declarative_sentence(DS, A, Z).
declarative_sentences(dec_ss(DS,CON,DSS), A, Z) :- declarative_sentence(DS, A, B), conjunction(CON, B, C), declarative_sentences(DSS, C, Z).

declarative_sentence(dec_s(NPS,VPS), A, Z) :- noun_phrases(NPS, A, B), verb_phrases(VPS, B, Z).

interrogatory_sentences(int_ss(IS), A, Z) :- interrogatory_sentence(IS, A, Z).
interrogatory_sentences(int_ss(IS,CON,ISS), A, Z) :- interrogatory_sentence(IS, A, B), conjunction(CON, B, C), interrogatory_sentences(ISS, C, Z).

interrogatory_sentence(int_s(IP,VPS), A, Z) :- interrogative_pronoun(IP, A, B), verb_phrases(VPS, B, Z).
interrogatory_sentence(int_s(IP,VPS1,VPS2), A, Z) :- interrogative_pronoun(IP, A, B), verb_phrases(VPS1, B, C), verb_phrases(VPS2, C, Z).

noun_phrases(nps(NP), A, Z) :- noun_phrase(NP, A, Z).
noun_phrases(nps(NP,CON,NPS), A, Z) :- noun_phrase(NP, A, B), conjunction(CON, B, C), noun_phrases(NPS, C, Z).

noun_phrase(np(N), A, Z) :- noun(N, A, Z).
noun_phrase(np(N,PP), A, Z) :- noun(N, A, B), perpositional_phrase(PP, B, Z).
noun_phrase(np(AJP,N), A, Z) :- adjective_phrase(AJP, A, B), noun(N, B, Z).
noun_phrase(np(AJP,N,PP), A, Z) :- adjective_phrase(AJP, A, B), noun(N, B, C), perpositional_phrase(PP, C, Z).
noun_phrase(np(DET,N), A, Z) :- determiner(DET, A, B), noun(N, B, Z).
noun_phrase(np(DET,N,PP), A, Z) :- determiner(DET, A, B), noun(N, B, C), perpositional_phrase(PP, C, Z).
noun_phrase(np(DET,AJP,N), A, Z) :- determiner(DET, A, B), adjective_phrase(AJP, B, C), noun(N, C, Z).
noun_phrase(np(DET,AJP,N,PP), A, Z) :- determiner(DET, A, B), adjective_phrase(AJP, B, C), noun(N, C, D), perpositional_phrase(PP, D, Z).

verb_phrases(vps(VP), A, Z) :- verb_phrase(VP, A, Z).
verb_phrases(vps(VP,CON,VPS), A, Z) :- verb_phrase(VP, A, B), conjunction(CON, B, C), verb_phrases(VPS, C, Z).

verb_phrase(vp(VA), A, Z) :- verb_adverb(VA, A, Z).
verb_phrase(vp(VA,NPS), A, Z) :- verb_adverb(VA, A, B), noun_phrases(NPS, B, Z).
verb_phrase(vp(VA,NPS,PP), A, Z) :- verb_adverb(VA, A, B), noun_phrases(NPS, B, C), perpositional_phrase(PP, C, Z).
verb_phrase(vp(VA,NPS,OPP), A, Z) :- verb_adverb(VA, A, B), noun_phrases(NPS, B, C), object_pronoun_phrase(OPP, C, Z).
verb_phrase(vp(VA,NPS,OPP,PP), A, Z) :- verb_adverb(VA, A, B), noun_phrases(NPS, B, C), object_pronoun_phrase(OPP, C, D), perpositional_phrase(PP, D, Z).
verb_phrase(vp(VA,NPS1,NPS2), A, Z) :- verb_adverb(VA, A, B), noun_phrases(NPS1, B, C), noun_phrases(NPS2, C, Z).
verb_phrase(vp(VA,NPS1,NPS2,PP), A, Z) :- verb_adverb(VA, A, B), noun_phrases(NPS1, B, C), noun_phrases(NPS2, C, D), perpositional_phrase(PP, D, Z).
verb_phrase(vp(VA,NPS1,NPS2,OPP), A, Z) :- verb_adverb(VA, A, B), noun_phrases(NPS1, B, C), noun_phrases(NPS2, C, D), object_pronoun_phrase(OPP, D, Z).
verb_phrase(vp(VA,NPS1,NPS2,OPP,PP), A, Z) :- verb_adverb(VA, A, B), noun_phrases(NPS1, B, C), noun_phrases(NPS2, C, D), object_pronoun_phrase(OPP, D, E), perpositional_phrase(PP, E, Z).
verb_phrase(vp(V,NPS,AVP), A, Z) :- verb(V, A, B), noun_phrases(NPS, B, C), adverb_phrase(AVP, C, Z).
verb_phrase(vp(V,NPS,PP,AVP), A, Z) :- verb(V, A, B), noun_phrases(NPS, B, C), perpositional_phrase(PP, C, D), adverb_phrase(AVP, D, Z).
verb_phrase(vp(V,NPS,OPP,AVP), A, Z) :- verb(V, A, B), noun_phrases(NPS, B, C), object_pronoun_phrase(OPP, C, D), adverb_phrase(AVP, D, Z).
verb_phrase(vp(V,NPS,OPP,PP,AVP), A, Z) :- verb(V, A, B), noun_phrases(NPS, B, C), object_pronoun_phrase(OPP, C, D), perpositional_phrase(PP, D, E), adverb_phrase(AVP, E, Z).

verb_adverb(va(V), A, Z) :- verb(V, A, Z).
verb_adverb(va(AVP,V), A, Z) :- adverb_phrase(AVP, A, B), verb(V, B, Z).
verb_adverb(va(V,AVP), A, Z) :- verb(V, A, B), adverb_phrase(AVP, B, Z).

adverb_phrase(avp(AV), A, Z) :- adverb(AV, A, Z).
adverb_phrase(avp(AVC,CON,AV), A, Z) :- adverb_comma(AVC, A, B), conjunction(CON, B, C), adverb(AV, C, Z).

adverb_comma(avc(AV), A, Z) :- adverb(AV, A, Z).
adverb_comma(avc(AV,AVC), A, Z) :- adverb(AV, A, B), adverb_comma(AVC, B, Z).

adjective_phrase(ajp(AJ), A, Z) :- adjective(AJ, A, Z).
adjective_phrase(ajp(AJ,AJP), A, Z) :- adjective(AJ, A, B), adjective_phrase(AJP, B, Z).

perpositional_phrase(pp(P,NP), A, Z) :- preposition(P, A, B), noun_phrase(NP, B, Z).
perpositional_phrase(pp(P,NP,PP), A, Z) :- preposition(P, A, B), noun_phrase(NP, B, C), perpositional_phrase(PP, C, Z).

object_pronoun_phrase(opp(OP,PP,V), A, Z) :- object_pronoun(OP, A, B), personal_pronoun(PP, B, C), verb(V, C, Z).


% Categories
determiner(d(D), [D|X], X) :- d(D).
noun(n(N), [N|X], X) :- n(N).
verb(v(V), [V|X], X) :- v(V).
adjective(adj(AJ), [AJ|X], X) :- adj(AJ).
adverb(adv(AV), [AV|X], X) :- adv(AV).
preposition(p(P), [P|X], X) :- p(P).
personal_pronoun(pp(PP), [PP|X], X) :- pp(PP).
interrogative_pronoun(ip(IP), [IP|X], X) :- ip(IP).
object_pronoun(op(OP), [OP|X], X) :- op(OP).
conjunction(con(C), [C|X], X) :- con(C).


% Lexicon
d(the).
d(a).
d(an).
d(every).
d(all).
d(some).
d(many).

n(cat).
n(bat).
n(boy).
n(girl).
n(man).
n(woman).
n(room).
n(school).
n(box).
n(envelope).
n(shed).
n(building).
n(tree).
n(students).
n(professors).
n(lecturers).
n(scientists).
n(researchers).
n(flower).
n(telescope).
n(desk).
n(moon).
n(eating).
n(drinking).

v(saw).
v(ate).
v(pushed).
v(stored).
v(gave).
v(climbed).
v(watched).
v(admired).
v(appreciated).
v(did).
v(walked).
v(liked).
v(met).
v(cooked).
v(ran).
v(slept).
v(studied).
v(kept).
v(loved).
v(thought).
v(went).
v(had).
v(said).
v(came).
v(got).
v(wanted).
v(used).
v(found).
v(knew).
v(thought).
v(made).
v(do).
v(eat).
v(see).
v(watch).   
v(give).

adj(young).
adj(old).
adj(large).
adj(big).
adj(small).
adj(empty).
adj(poor).
adj(rich).
adj(white).
adj(black).
adj(brilliant).
adj(talented).
adj(bright).
adj(adorable).
adj(aggressive).
adj(adventurous).
adj(angry).
adj(beautiful).
adj(brave).
adj(calm).
adj(busy).
adj(careful).
adj(cheerful).
adj(clean).
adj(clear).
adj(clever).

adv(quickly).
adv(slowly).
adv(boldly).
adv(bravely).
adv(brightly).
adv(cheerfully).
adv(devotedly).
adv(eagerly).
adv(elegantly).
adv(faithfully).
adv(gracefully).
adv(happily).
adv(innocently).
adv(kindly).
adv(perfectly).
adv(politely).
adv(powerfully).
adv(warmly).
adv(angrily).
adv(badly).
adv(foolishly).
adv(lazily).
adv(poorly).
adv(rudely).
adv(rapidly).
adv(daily).

p(in).
p(on).
p(at).
p(to).
p(of).
p(by).
p(with).
p(behind).
p(after).
p(above).
p(under).
p(across).
p(between).
p(up).
p(down).
p(over).

pp(i).
pp(we).
pp(you).
pp(they).
pp(he).
pp(she).
pp(it).

ip(who).
ip(what).

op(whom).

con(and).
