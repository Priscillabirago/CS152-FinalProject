% NutriLogic Natural Language Parser using DCGs
% This file contains Definite Clause Grammar rules for parsing natural language
% descriptions of symptoms and dietary habits

:- use_module(library(lists)).

% Main entry point for parsing input
parse_input(Input, Symptoms, DietHabits) :-
    tokenize_input(Input, Tokens),
    find_all_symptoms(Tokens, SymptomsList),
    find_all_diet_habits(Tokens, DietHabitsList),
    % Remove duplicates
    sort(SymptomsList, Symptoms),
    sort(DietHabitsList, DietHabits), !.

% Find all symptoms in the input tokens
find_all_symptoms(Tokens, Symptoms) :-
    findall(Symptom, 
            (phrase_in_tokens(symptom_phrase(Symptom), Tokens)),
            Symptoms).

% Find all diet habits in the input tokens
find_all_diet_habits(Tokens, DietHabits) :-
    findall(Habit, 
            (phrase_in_tokens(diet_habit_phrase(Habit), Tokens)),
            DietHabits).

% Helper to find a phrase anywhere in a list of tokens
phrase_in_tokens(Grammar, Tokens) :-
    append(_, Rest, Tokens),
    append(Match, _, Rest),
    phrase(Grammar, Match).

% Basic tokenization - just lowercase and split on spaces
tokenize_input(Input, Tokens) :-
    % Convert to atom if string
    (atom(Input) -> Atom = Input ; atom_string(Input, Atom)),
    % Convert to lowercase
    downcase_atom(Atom, LowerAtom),
    % Replace some word combinations before splitting
    replace_all(LowerAtom, "don't", "dont", Temp1),
    replace_all(Temp1, "can't", "cant", Temp2),
    replace_all(Temp2, "i'm", "i am", Temp3),
    replace_all(Temp3, "are weak", "areweak", Temp4),  % Special handling for "nails are weak"
    % Split on spaces and punctuation
    replace_all(Temp4, ",", " ", Temp5),
    replace_all(Temp5, ".", " ", Temp6),
    replace_all(Temp6, ";", " ", Temp7),
    replace_all(Temp7, ":", " ", Normalized),
    % Split on spaces
    atomic_list_concat(Words, ' ', Normalized),
    % Filter out empty tokens and common stop words
    exclude(stop_word_or_empty, Words, Tokens).

% Helper to do string replacement
replace_all(Input, Search, Replace, Output) :-
    atomic_list_concat(Split, Search, Input),
    atomic_list_concat(Split, Replace, Output).

% Check for empty tokens or stop words
stop_word_or_empty(Word) :-
    Word = ''; member(Word, ['a', 'an', 'the', 'i', 'am', 'is', 'are', 'my', 'mine', 'and', 
                           'have', 'has', 'had', 'get', 'gets', 'getting', 'about', 'from', 'but']).

% Simple direct parse for testing
direct_parse(Tokens, Symptom, Habit) :-
    (phrase(symptom_phrase(Symptom), Tokens) ; Symptom = none),
    (phrase(diet_habit_phrase(Habit), Tokens) ; Habit = none).

% Symptom phrases - map natural language to our formal symptom terms
% Fatigue related
symptom_phrase(fatigue) --> [tired].
symptom_phrase(fatigue) --> [exhausted].
symptom_phrase(fatigue) --> [fatigued].
symptom_phrase(fatigue) --> [feel, tired].
symptom_phrase(fatigue) --> [feeling, tired].
symptom_phrase(fatigue) --> [always, tired].
symptom_phrase(fatigue) --> [no, energy].
symptom_phrase(fatigue) --> [low, energy].
symptom_phrase(fatigue) --> [lack, of, energy].

% Brain fog related
symptom_phrase(brain_fog) --> [brain, fog].
symptom_phrase(brain_fog) --> [foggy].
symptom_phrase(brain_fog) --> [feel, foggy].
symptom_phrase(brain_fog) --> [feeling, foggy].
symptom_phrase(brain_fog) --> [confused].
symptom_phrase(brain_fog) --> [forgetful].
symptom_phrase(brain_fog) --> [foggy, thinking].
symptom_phrase(brain_fog) --> [forget, things].
symptom_phrase(brain_fog) --> [forgetting, things].
symptom_phrase(brain_fog) --> [forget, things, easily].

% Sleep related
symptom_phrase(poor_sleep) --> [poor, sleep].
symptom_phrase(poor_sleep) --> [trouble, sleeping].
symptom_phrase(poor_sleep) --> [insomnia].
symptom_phrase(poor_sleep) --> [cant, sleep].
symptom_phrase(poor_sleep) --> [cant, sleep, well].
symptom_phrase(poor_sleep) --> [difficulty, sleeping].
symptom_phrase(poor_sleep) --> [wake, up, often].
symptom_phrase(poor_sleep) --> [dont, sleep, well].

% Digestive issues
symptom_phrase(digestive_issues) --> [stomach, ache].
symptom_phrase(digestive_issues) --> [stomach, pain].
symptom_phrase(digestive_issues) --> [stomach, hurts].
symptom_phrase(digestive_issues) --> [digestive, issues].
symptom_phrase(digestive_issues) --> [stomach, problems].
symptom_phrase(digestive_issues) --> [bloated].
symptom_phrase(digestive_issues) --> [bloating].
symptom_phrase(digestive_issues) --> [gas].
symptom_phrase(digestive_issues) --> [constipation].
symptom_phrase(digestive_issues) --> [diarrhea].

% Caffeine related
symptom_phrase(caffeine_jitters) --> [jittery].
symptom_phrase(caffeine_jitters) --> [jitters].
symptom_phrase(caffeine_jitters) --> [shaky].
symptom_phrase(caffeine_jitters) --> [heart, racing].
symptom_phrase(caffeine_jitters) --> [too, much, caffeine].
symptom_phrase(caffeine_jitters) --> [caffeine, jitters].

% Concentration related
symptom_phrase(poor_concentration) --> [cant, concentrate].
symptom_phrase(poor_concentration) --> [poor, concentration].
symptom_phrase(poor_concentration) --> [difficulty, concentrating].
symptom_phrase(poor_concentration) --> [trouble, concentrating].
symptom_phrase(poor_concentration) --> [hard, to, concentrate].
symptom_phrase(poor_concentration) --> [trouble, studying].
symptom_phrase(poor_concentration) --> [hard, to, study].

% Anxiety related
symptom_phrase(anxiety) --> [anxious].
symptom_phrase(anxiety) --> [anxiety].
symptom_phrase(anxiety) --> [stressed].
symptom_phrase(anxiety) --> [stressed, out].
symptom_phrase(anxiety) --> [worried].
symptom_phrase(anxiety) --> [overthinking].
symptom_phrase(anxiety) --> [overthinking, everything].
symptom_phrase(anxiety) --> [panicky].

% Other symptoms
symptom_phrase(irritability) --> [irritable].
symptom_phrase(irritability) --> [irritability].
symptom_phrase(irritability) --> [moody].
symptom_phrase(irritability) --> [feel, moody].
symptom_phrase(irritability) --> [mood, swings].
symptom_phrase(irritability) --> [easily, annoyed].

symptom_phrase(weak_nails) --> [weak, nails].
symptom_phrase(weak_nails) --> [my, nails, areweak].
symptom_phrase(weak_nails) --> [nails, areweak].
symptom_phrase(weak_nails) --> [brittle, nails].
symptom_phrase(weak_nails) --> [nails, break, easily].

symptom_phrase(acne) --> [acne].
symptom_phrase(acne) --> [breakouts].
symptom_phrase(acne) --> [acne, breakouts].
symptom_phrase(acne) --> [pimples].
symptom_phrase(acne) --> [skin, problems].

symptom_phrase(afternoon_crash) --> [afternoon, crash].
symptom_phrase(afternoon_crash) --> [crash, in, afternoon].
symptom_phrase(afternoon_crash) --> [tired, after, lunch].
symptom_phrase(afternoon_crash) --> [sleepy, in, afternoon].

% Diet habit phrases - map natural language to our formal diet habit terms
% Water related
diet_habit_phrase(low_water) --> [dont, drink, enough, water].
diet_habit_phrase(low_water) --> [dont, drink, much, water].
diet_habit_phrase(low_water) --> [rarely, drink, water].
diet_habit_phrase(low_water) --> [dehydrated].
diet_habit_phrase(low_water) --> [forgetting, to, drink, water].

% Sugar related
diet_habit_phrase(high_sugar) --> [eat, lot, of, sugar].
diet_habit_phrase(high_sugar) --> [eat, many, sweets].
diet_habit_phrase(high_sugar) --> [love, sweets].
diet_habit_phrase(high_sugar) --> [sweet, tooth].
diet_habit_phrase(high_sugar) --> [sugary, drinks].
diet_habit_phrase(high_sugar) --> [lots, of, soda].
diet_habit_phrase(high_sugar) --> [drink, soda, daily].

% Protein related
diet_habit_phrase(low_protein) --> [dont, eat, much, protein].
diet_habit_phrase(low_protein) --> [low, protein].
diet_habit_phrase(low_protein) --> [rarely, eat, meat].
diet_habit_phrase(low_protein) --> [skip, protein].

% Meal patterns
diet_habit_phrase(skipping_meals) --> [skip, meals].
diet_habit_phrase(skipping_meals) --> [skip, breakfast].
diet_habit_phrase(skipping_meals) --> [often, miss, meals].
diet_habit_phrase(skipping_meals) --> [irregular, eating].
diet_habit_phrase(skipping_meals) --> [forget, to, eat].
diet_habit_phrase(skipping_meals) --> [usually, skip, breakfast].

% Student specific diet habits
diet_habit_phrase(energy_drinks) --> [energy, drinks].
diet_habit_phrase(energy_drinks) --> [drink, energy, drinks].
diet_habit_phrase(energy_drinks) --> [monster, drinks].
diet_habit_phrase(energy_drinks) --> [red, bull].

diet_habit_phrase(coffee_dependent) --> [lots, of, coffee].
diet_habit_phrase(coffee_dependent) --> [drink, lots, of, coffee].
diet_habit_phrase(coffee_dependent) --> [dependent, on, coffee].
diet_habit_phrase(coffee_dependent) --> [need, coffee].
diet_habit_phrase(coffee_dependent) --> [multiple, coffees, daily].
diet_habit_phrase(coffee_dependent) --> [coffee, addict].

diet_habit_phrase(irregular_meals) --> [eat, at, odd, hours].
diet_habit_phrase(irregular_meals) --> [no, meal, schedule].
diet_habit_phrase(irregular_meals) --> [irregular, meal, times].
diet_habit_phrase(irregular_meals) --> [eat, whenever].

diet_habit_phrase(meal_skipping_for_study) --> [skip, meals, to, study].
diet_habit_phrase(meal_skipping_for_study) --> [forget, eating, while, studying].
diet_habit_phrase(meal_skipping_for_study) --> [study, instead, of, eating].

diet_habit_phrase(budget_eating) --> [cheap, food].
diet_habit_phrase(budget_eating) --> [budget, food].
diet_habit_phrase(budget_eating) --> [eat, on, budget].
diet_habit_phrase(budget_eating) --> [cant, afford, healthy, food].

diet_habit_phrase(convenience_foods) --> [microwave, meals].
diet_habit_phrase(convenience_foods) --> [instant, noodles].
diet_habit_phrase(convenience_foods) --> [fast, food].
diet_habit_phrase(convenience_foods) --> [frozen, meals].
diet_habit_phrase(convenience_foods) --> [quick, easy, meals].

diet_habit_phrase(campus_food_only) --> [campus, food].
diet_habit_phrase(campus_food_only) --> [dining, hall, food].
diet_habit_phrase(campus_food_only) --> [cafeteria, food].
diet_habit_phrase(campus_food_only) --> [meal, plan, only].

diet_habit_phrase(vending_machine_reliance) --> [vending, machine].
diet_habit_phrase(vending_machine_reliance) --> [vending, machines].
diet_habit_phrase(vending_machine_reliance) --> [vending, machine, snacks].
diet_habit_phrase(vending_machine_reliance) --> [snack, from, vending, machines].
diet_habit_phrase(vending_machine_reliance) --> [buy, snacks, from, machines].

% Diet types
diet_habit_phrase(vegan) --> [vegan].
diet_habit_phrase(vegan) --> [vegan, diet].
diet_habit_phrase(vegan) --> [plant, based].
diet_habit_phrase(vegan) --> [dont, eat, animal, products].

diet_habit_phrase(high_processed_food) --> [processed, food].
diet_habit_phrase(high_processed_food) --> [junk, food].
diet_habit_phrase(high_processed_food) --> [packaged, food].
diet_habit_phrase(high_processed_food) --> [convenience, food].

% Testing predicates
test_parse(Input) :-
    write('Input: '), write(Input), nl,
    tokenize_input(Input, Tokens),
    write('Tokens: '), write(Tokens), nl,
    find_all_symptoms(Tokens, Symptoms),
    write('Symptoms: '), write(Symptoms), nl,
    find_all_diet_habits(Tokens, DietHabits),
    write('Diet Habits: '), write(DietHabits), nl. 