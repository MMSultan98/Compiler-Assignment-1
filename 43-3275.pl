% 1- Start variable
s(DSS, A, Z) :- declarative_sentences(DSS, A, Z).
s(ISS, A, Z) :- interrogatory_sentences(ISS, A, Z).


% 2- Structures

% 2.1 - Sentences
% 2.1.1 - Declaritive sentences
declarative_sentences(dec_ss(DS), A, Z) :- declarative_sentence(DS, A, Z).
declarative_sentences(dec_ss(DSC,CON,DS), A, Z) :- declarative_sentence_comma(DSC, A, B), conjunction(CON, B, C), declarative_sentence(DS, C, Z).

declarative_sentence_comma(dec_sc(DS), A, Z) :- declarative_sentence(DS, A, Z).
declarative_sentence_comma(dec_sc(DS,DSC), A, Z) :- declarative_sentence(DS, A, B), declarative_sentence_comma(DSC, B, Z).

declarative_sentence(dec_s(NPS,VPS), A, Z) :- noun_phrases(NPS, A, B), verb_phrases_past(VPS, B, Z).
declarative_sentence(dec_s(NPS,VPS), A, Z) :- noun_phrases(NPS, A, B), verb_phrases_past_2(VPS, B, Z).

% 2.1.2 - Interrogatory sentences
interrogatory_sentences(int_ss(IS), A, Z) :- interrogatory_sentence(IS, A, Z).
interrogatory_sentences(int_ss(ISC,CON,IS), A, Z) :- interrogatory_sentence_comma(ISC, A, B), conjunction(CON, B, C), interrogatory_sentence(IS, C, Z).

interrogatory_sentence_comma(int_sc(IS), A, Z) :- interrogatory_sentence(IS, A, Z).
interrogatory_sentence_comma(int_sc(IS,ISC), A, Z) :- interrogatory_sentence(IS, A, B), interrogatory_sentence_comma(ISC, B, Z).

interrogatory_sentence(int_s(IP,VPS), A, Z) :- interrogative_pronoun(IP, A, B), verb_phrases_past(VPS, B, Z).
interrogatory_sentence(int_s(IP,V,VPS), A, Z) :- interrogative_pronoun(IP, A, B), verb_past_aux(V, B, C), verb_phrases_past_aux(VPS, C, Z).

% 2.2- Noun phrases
noun_phrases(nps(NP), A, Z) :- noun_phrase(NP, A, Z).
noun_phrases(nps(NPC,CON,NP), A, Z) :- noun_phrase_comma(NPC, A, B), conjunction(CON, B, C), noun_phrase(NP, C, Z).

noun_phrase_comma(npc(NP), A, Z) :- noun_phrase(NP, A, Z).
noun_phrase_comma(npc(NP,NPC), A, Z) :- noun_phrase(NP, A, B), noun_phrase_comma(NPC, B, Z).

noun_phrase(np(N), A, Z) :- noun(N, A, Z).
noun_phrase(np(N,PP), A, Z) :- noun(N, A, B), perpositional_phrase(PP, B, Z).
noun_phrase(np(AJP,N), A, Z) :- adjective_phrase(AJP, A, B), noun(N, B, Z).
noun_phrase(np(AJP,N,PP), A, Z) :- adjective_phrase(AJP, A, B), noun(N, B, C), perpositional_phrase(PP, C, Z).
noun_phrase(np(DET,N), A, Z) :- determiner(DET, A, B), noun(N, B, Z).
noun_phrase(np(DET,N,PP), A, Z) :- determiner(DET, A, B), noun(N, B, C), perpositional_phrase(PP, C, Z).
noun_phrase(np(DET,AJP,N), A, Z) :- determiner(DET, A, B), adjective_phrase(AJP, B, C), noun(N, C, Z).
noun_phrase(np(DET,AJP,N,PP), A, Z) :- determiner(DET, A, B), adjective_phrase(AJP, B, C), noun(N, C, D), perpositional_phrase(PP, D, Z).
noun_phrase(np(N,OPP), A, Z) :- noun(N, A, B), object_pronoun_phrase(OPP, B, Z).
noun_phrase(np(N,PP,OPP), A, Z) :- noun(N, A, B), perpositional_phrase(PP, B, C), object_pronoun_phrase(OPP, C, Z).
noun_phrase(np(AJP,N,OPP), A, Z) :- adjective_phrase(AJP, A, B), noun(N, B, C), object_pronoun_phrase(OPP, C, Z).
noun_phrase(np(AJP,N,PP,OPP), A, Z) :- adjective_phrase(AJP, A, B), noun(N, B, C), perpositional_phrase(PP, C, D), object_pronoun_phrase(OPP, D, Z).
noun_phrase(np(DET,N,OPP), A, Z) :- determiner(DET, A, B), noun(N, B, C), object_pronoun_phrase(OPP, C, Z).
noun_phrase(np(DET,N,PP,OPP), A, Z) :- determiner(DET, A, B), noun(N, B, C), perpositional_phrase(PP, C, D), object_pronoun_phrase(OPP, D, Z).
noun_phrase(np(DET,AJP,N,OPP), A, Z) :- determiner(DET, A, B), adjective_phrase(AJP, B, C), noun(N, C, D), object_pronoun_phrase(OPP, D, Z).
noun_phrase(np(DET,AJP,N,PP,OPP), A, Z) :- determiner(DET, A, B), adjective_phrase(AJP, B, C), noun(N, C, D), perpositional_phrase(PP, D, E), object_pronoun_phrase(OPP, E, Z).

% 2.3- Verb phrases
% 2.3.1- Verb phrases (past tense verbs)
verb_phrases_past(vps(VP), A, Z) :- verb_phrase_past(VP, A, Z).
verb_phrases_past(vps(VPC,CON,VP), A, Z) :- verb_phrase_past_comma(VPC, A, B), conjunction(CON, B, C), verb_phrase_past(VP, C, Z).

verb_phrase_past_comma(vpc(VP), A, Z) :- verb_phrase_past(VP, A, Z).
verb_phrase_past_comma(vpc(VP,VPC), A, Z) :- verb_phrase_past(VP, A, B), verb_phrase_past_comma(VPC, B, Z).

verb_phrase_past(vp(VA), A, Z) :- verb_past_adverb(VA, A, Z).
verb_phrase_past(vp(VAS,NPS), A, Z) :- verb_past_adverbs(VAS, A, B), noun_phrases(NPS, B, Z).
verb_phrase_past(vp(VAS,PP), A, Z) :- verb_past_adverbs(VAS, A, B), perpositional_phrase(PP, B, Z).
verb_phrase_past(vp(VAS,NPS,PP), A, Z) :- verb_past_adverbs(VAS, A, B), noun_phrases(NPS, B, C), perpositional_phrase(PP, C, Z).
verb_phrase_past(vp(VS,NPS,AVP), A, Z) :- verbs_past(VS, A, B), noun_phrases(NPS, B, C), adverb_phrase(AVP, C, Z).
verb_phrase_past(vp(VS,NPS,PP,AVP), A, Z) :- verbs_past(VS, A, B), noun_phrases(NPS, B, C), perpositional_phrase(PP, C, D), adverb_phrase(AVP, D, Z).

% 2.3.2- Verb phrases (past tense verbs with 2 objects)
verb_phrases_past_2(vps(VP), A, Z) :- verb_phrase_past_2(VP, A, Z).
verb_phrases_past_2(vps(VPC,CON,VP), A, Z) :- verb_phrase_past_2_comma(VPC, A, B), conjunction(CON, B, C), verb_phrase_past_2(VP, C, Z).

verb_phrase_past_2_comma(vpc(VP), A, Z) :- verb_phrase_past_2(VP, A, Z).
verb_phrase_past_2_comma(vpc(VP,VPC), A, Z) :- verb_phrase_past_2(VP, A, B), verb_phrase_past_2_comma(VPC, B, Z).

verb_phrase_past_2(vp(VA), A, Z) :- verb_past_2_adverb(VA, A, Z).
verb_phrase_past_2(vp(VAS,NPS1,NPS2), A, Z) :- verb_past_2_adverbs(VAS, A, B), noun_phrases(NPS1, B, C), noun_phrases(NPS2, C, Z).
verb_phrase_past_2(vp(VAS,NPS1,NPS2,PP), A, Z) :- verb_past_2_adverbs(VAS, A, B), noun_phrases(NPS1, B, C), noun_phrases(NPS2, C, D), perpositional_phrase(PP, D, Z).
verb_phrase_past_2(vp(VS,NPS1,NPS2,AVP), A, Z) :- verbs_past_2(VS, A, B), noun_phrases(NPS1, B, C), noun_phrases(NPS2, C, D), adverb_phrase(AVP, D, Z).
verb_phrase_past_2(vp(VS,NPS1,NPS2,PP,AVP), A, Z) :- verbs_past_2(VS, A, B), noun_phrases(NPS1, B, C), noun_phrases(NPS2, C, D), perpositional_phrase(PP, D, E), adverb_phrase(AVP, E, Z).

% 2.3.3- Verb phrases (past tense auxiliary verbs)
verb_phrases_past_aux(vps(VP), A, Z) :- verb_phrase_past_aux(VP, A, Z).
verb_phrases_past_aux(vps(VPC,CON,VP), A, Z) :- verb_phrase_past_aux_comma(VPC, A, B), conjunction(CON, B, C), verb_phrase_past_aux(VP, C, Z).

verb_phrase_past_aux_comma(vpc(VP), A, Z) :- verb_phrase_past_aux(VP, A, Z).
verb_phrase_past_aux_comma(vpc(VP,VPC), A, Z) :- verb_phrase_past_aux(VP, A, B), verb_phrase_past_aux_comma(VPC, B, Z).

verb_phrase_past_aux(vp(NPS,VA), A, Z) :- noun_phrases(NPS, A, B), verb_inf_adverbs(VA, B, Z).
verb_phrase_past_aux(vp(NPS,V,AVP), A, Z) :- noun_phrases(NPS, A, B), verbs_inf(V, B, C), adverb_phrase(AVP, C, Z).

% 2.4- Verbs with adverb phrases
% 2.4.1- Verbs with adverb phrases (past tense verbs)
verb_past_adverbs(vas(VA), A, Z) :- verb_past_adverb(VA, A, Z).
verb_past_adverbs(vas(VAC,CON,VA), A, Z) :- verb_past_adverb_comma(VAC, A, B), conjunction(CON, B, C), verb_past_adverb(VA, C, Z).

verb_past_adverb_comma(vac(VA), A, Z) :- verb_past_adverb(VA, A, Z).
verb_past_adverb_comma(vac(VA,VAC), A, Z) :- verb_past_adverb(VA, A, B), verb_past_adverb_comma(VAC, B, Z).

verb_past_adverb(va(V), A, Z) :- verb_past(V, A, Z).
verb_past_adverb(va(AVP,V), A, Z) :- adverb_phrase(AVP, A, B), verb_past(V, B, Z).
verb_past_adverb(va(V,AVP), A, Z) :- verb_past(V, A, B), adverb_phrase(AVP, B, Z).

% 2.4.2- Verbs with adverb phrases (past tense verbs with 2 objects)
verb_past_2_adverbs(vas(VA), A, Z) :- verb_past_2_adverb(VA, A, Z).
verb_past_2_adverbs(vas(VAC,CON,VA), A, Z) :- verb_past_2_adverb_comma(VAC, A, B), conjunction(CON, B, C), verb_past_2_adverb(VA, C, Z).

verb_past_2_adverb_comma(vac(VA), A, Z) :- verb_past_2_adverb(VA, A, Z).
verb_past_2_adverb_comma(vac(VA,VAC), A, Z) :- verb_past_2_adverb(VA, A, B), verb_past_2_adverb_comma(VAC, B, Z).

verb_past_2_adverb(va(V), A, Z) :- verb_past_2(V, A, Z).
verb_past_2_adverb(va(AVP,V), A, Z) :- adverb_phrase(AVP, A, B), verb_past_2(V, B, Z).
verb_past_2_adverb(va(V,AVP), A, Z) :- verb_past_2(V, A, B), adverb_phrase(AVP, B, Z).

% 2.4.3- Verbs with adverb phrases (infinitive verbs)
verb_inf_adverbs(vas(VA), A, Z) :- verb_inf_adverb(VA, A, Z).
verb_inf_adverbs(vas(VAC,CON,VA), A, Z) :- verb_inf_adverb_comma(VAC, A, B), conjunction(CON, B, C), verb_inf_adverb(VA, C, Z).

verb_inf_adverb_comma(vac(VA), A, Z) :- verb_inf_adverb(VA, A, Z).
verb_inf_adverb_comma(vac(VA,VAC), A, Z) :- verb_inf_adverb(VA, A, B), verb_inf_adverb_comma(VAC, B, Z).

verb_inf_adverb(va(V), A, Z) :- verb_inf(V, A, Z).
verb_inf_adverb(va(AVP,V), A, Z) :- adverb_phrase(AVP, A, B), verb_inf(V, B, Z).
verb_inf_adverb(va(V,AVP), A, Z) :- verb_inf(V, A, B), adverb_phrase(AVP, B, Z).

% 2.5- Verbs
% 2.5.1- Verbs (past tense)
verbs_past(vs(V), A, Z) :- verb_past(V, A, Z).
verbs_past(vs(VC,CON,V), A, Z) :- verb_past_comma(VC, A, B), conjunction(CON, B, C), verb_past(V, C, Z).

verb_past_comma(vc(V), A, Z) :- verb_past(V, A, Z).
verb_past_comma(vc(V,VC), A, Z) :- verb_past(V, A, B), verb_past_comma(VC, B, Z).

% 2.5.2- Verbs (past tense with 2 objects)
verbs_past_2(vs(V), A, Z) :- verb_past_2(V, A, Z).
verbs_past_2(vs(VC,CON,V), A, Z) :- verb_past_2_comma(VC, A, B), conjunction(CON, B, C), verb_past_2(V, C, Z).

verb_past_2_comma(vc(V), A, Z) :- verb_past_2(V, A, Z).
verb_past_2_comma(vc(V,VC), A, Z) :- verb_past_2(V, A, B), verb_past_2_comma(VC, B, Z).

% 2.5.3- Verbs (infinitive)
verbs_inf(vs(V), A, Z) :- verb_inf(V, A, Z).
verbs_inf(vs(VC,CON,V), A, Z) :- verb_inf_comma(VC, A, B), conjunction(CON, B, C), verb_inf(V, C, Z).

verb_inf_comma(vc(V), A, Z) :- verb_inf(V, A, Z).
verb_inf_comma(vc(V,VC), A, Z) :- verb_inf(V, A, B), verb_inf_comma(VC, B, Z).

% 2.6- Adverb phrases
adverb_phrase(avp(AV), A, Z) :- adverb(AV, A, Z).
adverb_phrase(avp(AVC,CON,AV), A, Z) :- adverb_comma(AVC, A, B), conjunction(CON, B, C), adverb(AV, C, Z).

adverb_comma(avc(AV), A, Z) :- adverb(AV, A, Z).
adverb_comma(avc(AV,AVC), A, Z) :- adverb(AV, A, B), adverb_comma(AVC, B, Z).

% 2.7- Adjective phrases
adjective_phrase(ajp(AJ), A, Z) :- adjective(AJ, A, Z).
adjective_phrase(ajp(AJ,AJP), A, Z) :- adjective(AJ, A, B), adjective_phrase(AJP, B, Z).

% 2.8- Perpositional phrases
perpositional_phrase(pp(P,NP), A, Z) :- preposition(P, A, B), noun_phrase(NP, B, Z).
perpositional_phrase(pp(P,NP,PP), A, Z) :- preposition(P, A, B), noun_phrase(NP, B, C), perpositional_phrase(PP, C, Z).

% 2.9- Object pronoun phrases
object_pronoun_phrase(opp(OP,PP,V), A, Z) :- object_pronoun(OP, A, B), personal_pronoun(PP, B, C), verb_past(V, C, Z).
object_pronoun_phrase(opp(OP,NP,V), A, Z) :- object_pronoun(OP, A, B), noun_phrase(NP, B, C), verb_past(V, C, Z).


% 3- Categories
determiner(d(DET), [DET|X], X) :- d(DET).
noun(n(N), [N|X], X) :- n(N).
verb_past(v(V), [V|X], X) :- v_past(V).
verb_past_2(v(V), [V|X], X) :- v_past_2(V).
verb_past_aux(v(V), [V|X], X) :- v_past_aux(V).
verb_inf(v(V), [V|X], X) :- v_inf(V).
adjective(adj(AJ), [AJ|X], X) :- adj(AJ).
adverb(adv(AV), [AV|X], X) :- adv(AV).
preposition(p(P), [P|X], X) :- p(P).
personal_pronoun(pp(PP), [PP|X], X) :- pp(PP).
interrogative_pronoun(ip(IP), [IP|X], X) :- ip(IP).
object_pronoun(op(OP), [OP|X], X) :- opn(OP).
conjunction(con(CON), [CON|X], X) :- con(CON).


% 4- Lexicon

% 4.1- Determiners
d(the).
d(a).
d(an).
d(every).
d(all).
d(some).
d(many).

% 4.2- Nouns
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
n(student).
n(students).
n(professor).
n(professors).
n(lecturer).
n(lecturers).
n(scientist).
n(scientists).
n(researcher).
n(researchers).
n(flower).
n(telescope).
n(desk).
n(moon).
n(eating).
n(drinking).
n(money).
n(meal).
n(food).

% 4.3- Verbs (past tense)
v_past(saw).
v_past(ate).
v_past(drank).
v_past(pushed).
v_past(stored).
v_past(climbed).
v_past(watched).
v_past(admired).
v_past(appreciated).
v_past(did).
v_past(walked).
v_past(liked).
v_past(met).
v_past(cooked).
v_past(ran).
v_past(slept).
v_past(studied).
v_past(kept).
v_past(loved).
v_past(thought).
v_past(went).
v_past(had).
v_past(said).
v_past(came).
v_past(got).
v_past(wanted).
v_past(used).
v_past(found).
v_past(knew).
v_past(thought).
v_past(made).

% 4.4- Verbs (past tense with 2 objects)
v_past_2(gave).
v_past_2(showed).
v_past_2(taught).
v_past_2(wrote).
v_past_2(sold).
v_past_2(owed).
v_past_2(brought).
v_past_2(got).
v_past_2(made).
v_past_2(cooked).

% 4.5- Verbs (past tense auxiliary)
v_past_aux(did).

% 4.6- Verbs (infinitive)
v_inf(do).
v_inf(eat).
v_inf(drink).
v_inf(see).
v_inf(watch).   
v_inf(give).
v_inf(sell).
v_inf(make).
v_inf(get).
v_inf(cook).

% 4.7- Adjectives
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

% 4.8- Adverbs
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
adv(secretly).

% 4.9- Prepositions
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

% 4.10- Personal pronouns
pp(i).
pp(we).
pp(you).
pp(they).
pp(he).
pp(she).
pp(it).

% 4.11- Interrogative pronouns
ip(who).
ip(what).

% 4.12- Object pronouns
opn(whom).

% 4.13- Conjunctions
con(and).
