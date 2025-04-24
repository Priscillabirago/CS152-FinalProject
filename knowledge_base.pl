% NutriLogic - Nutrition Expert System Knowledge Base
% This knowledge base contains rules for diagnosing nutrition-related issues

% Allow for definitions of predicates to be split across the file
:- discontiguous symptom/2.
:- discontiguous diet_habit/2.
:- discontiguous diagnosis/4.
:- discontiguous recommendation/2.

% Define certainty factor combination rules
% Based on MYCIN approach
combine_cf(CF1, CF2, CF) :-
    CF1 >= 0,
    CF2 >= 0,
    CF is CF1 + CF2 * (1 - CF1).

combine_cf(CF1, CF2, CF) :-
    CF1 < 0,
    CF2 < 0,
    CF is CF1 + CF2 * (1 + CF1).

combine_cf(CF1, CF2, CF) :-
    (CF1 < 0, CF2 >= 0; CF1 >= 0, CF2 < 0),
    CF is (CF1 + CF2) / (1 - min(abs(CF1), abs(CF2))).

% Facts about symptoms and their relation to nutrition issues - Student Focused
% Format: symptom(Name, Description)
symptom(fatigue, "Feeling tired or having low energy").
symptom(afternoon_crash, "Experiencing significant energy drop in afternoon").
symptom(brain_fog, "Difficulty concentrating or thinking clearly").
symptom(frequent_headaches, "Having headaches more than twice a week").
symptom(irritability, "Feeling moody or irritable without clear reason").
symptom(poor_sleep, "Difficulty falling asleep or staying asleep").
symptom(digestive_issues, "Experiencing bloating, gas, or irregular bowel movements").
symptom(weak_nails, "Having brittle or weak nails").
symptom(dry_skin, "Having unusually dry skin despite hydration").
symptom(slow_healing, "Wounds or bruises taking longer than usual to heal").
symptom(acne, "Frequent breakouts or persistent acne").
symptom(low_energy, "Consistently low energy levels throughout the day").
symptom(cold_hands, "Unusually cold hands and feet").
symptom(bruising, "Bruising easily or frequently").
symptom(anxiety, "Feeling anxious or on edge regularly").
symptom(depression, "Persistent feelings of sadness or lack of motivation").
symptom(muscle_weakness, "Weakness in muscles or difficulty with physical tasks").
symptom(poor_concentration, "Difficulty maintaining focus on tasks or studying").
symptom(heart_palpitations, "Noticing heart palpitations or irregular heartbeat").
symptom(mouth_sores, "Frequent sores or ulcers in the mouth").
symptom(joint_pain, "Experiencing joint pain or stiffness").
symptom(dry_eyes, "Dry or irritated eyes, especially when looking at screens").
symptom(muscle_cramps, "Experiencing muscle cramps, especially after exercise").
symptom(dizziness, "Feeling dizzy when standing up or during the day").
symptom(exam_anxiety, "Excessive worry and stress during exam periods").
symptom(forgetfulness, "Trouble remembering information for tests or assignments").
symptom(caffeine_jitters, "Feeling shaky after consuming energy drinks or coffee").
symptom(midday_slump, "Sharp decrease in energy and focus during middle of day").
symptom(late_night_hunger, "Strong hunger pangs when studying late at night").
symptom(poor_stamina, "Getting tired quickly during physical activities").
symptom(stress_eating, "Eating more when feeling stressed about assignments").
symptom(post_workout_fatigue, "Excessive fatigue after moderate exercise").
symptom(eye_strain, "Eye fatigue when studying or looking at screens").
symptom(hair_loss, "Noticing unusual hair thinning or loss").
symptom(energy_crashes, "Sudden drops in energy, especially after consuming sugary foods").

% Additional symptoms for diagnosis rules
symptom(weak_immune_system, "Frequent colds or infections during semester").
symptom(poor_immune_function, "Getting sick more often than usual, missing classes").
symptom(tooth_problems, "Dental issues like cavities or sensitivity").
symptom(vision_problems, "Blurry vision or other vision issues, especially when reading").
symptom(weakened_immune_system, "Getting sick easily before or during exams").
symptom(weak_bones, "Brittle bones or increased fracture risk").
symptom(anemia_symptoms, "Pale skin, extreme fatigue, and shortness of breath").
symptom(weight_gain, "Unexplained weight gain despite diet").
symptom(dehydration_symptoms, "Thirst, dry mouth, and dark urine").
symptom(skin_problems, "Various skin issues including rashes or inflammation").

% Facts about dietary habits - Student Focused
% Format: diet_habit(Name, Description)
diet_habit(low_protein, "Consuming less than 50g of protein daily").
diet_habit(high_sugar, "Consuming sugary foods or drinks multiple times daily").
diet_habit(low_water, "Drinking less than 6 glasses of water daily").
diet_habit(low_vegetables, "Eating less than 2 servings of vegetables daily").
diet_habit(low_fruits, "Rarely eating fruits").
diet_habit(high_processed_food, "Eating mostly packaged, fast food, or dining hall foods").
diet_habit(skipping_meals, "Regularly skipping meals, especially breakfast before class").
diet_habit(low_fiber, "Diet lacking in whole grains, legumes, or fibrous foods").
diet_habit(high_caffeine, "Consuming more than 3 caffeinated drinks daily").
diet_habit(high_alcohol, "Consuming alcohol multiple times per week").
diet_habit(dairy_free, "Avoiding dairy products completely").
diet_habit(vegetarian, "Following a vegetarian diet").
diet_habit(vegan, "Following a vegan diet").
diet_habit(gluten_free, "Following a gluten-free diet").
diet_habit(keto, "Following a ketogenic (high-fat, low-carb) diet").
diet_habit(high_salt, "Consuming high amounts of salt or salty foods").
diet_habit(low_fat, "Severely restricting fat intake").
diet_habit(high_omega6, "Diet high in vegetable oils and processed foods").
diet_habit(low_iodine, "Avoiding iodized salt and seafood").
diet_habit(crash_dieting, "Frequently following restrictive diets").
diet_habit(late_night_eating, "Regularly eating late at night while studying").
diet_habit(emotional_eating, "Eating in response to academic stress").
diet_habit(fast_eating, "Eating meals very quickly between classes").
diet_habit(irregular_meals, "Having no regular meal pattern due to class schedule").
diet_habit(low_variety, "Eating the same few foods repeatedly").
diet_habit(low_salt, "Severely restricting salt intake").
diet_habit(high_water, "Drinking excessive amounts of water without electrolytes").
diet_habit(energy_drinks, "Regular consumption of energy drinks, especially during exams").
diet_habit(campus_food_only, "Relying exclusively on campus dining options").
diet_habit(meal_skipping_for_study, "Skipping meals to make time for studying").
diet_habit(budget_eating, "Choosing foods primarily based on low cost").
diet_habit(convenience_foods, "Relying heavily on pre-packaged convenience foods").
diet_habit(coffee_dependent, "Needing multiple coffees to get through the day").
diet_habit(vending_machine_reliance, "Getting many snacks from vending machines").
diet_habit(midnight_snacking, "Regular eating late at night during study sessions").

% Rules for diagnosing nutrition issues with certainty factors
% Format: diagnosis(Issue, CF, Symptom, DietHabit)

% Iron deficiency
diagnosis(iron_deficiency, 0.7, fatigue, low_protein).
diagnosis(iron_deficiency, 0.6, fatigue, vegetarian).
diagnosis(iron_deficiency, 0.8, fatigue, vegan).
diagnosis(iron_deficiency, 0.5, dizziness, _).
diagnosis(iron_deficiency, 0.4, brain_fog, low_protein).
diagnosis(iron_deficiency, 0.3, hair_loss, _).
diagnosis(iron_deficiency, 0.6, weak_nails, vegetarian).
diagnosis(iron_deficiency, 0.6, weak_nails, vegan).
diagnosis(iron_deficiency, 0.7, poor_concentration, vegan).
diagnosis(iron_deficiency, 0.5, anemia_symptoms, vegetarian).

% Dehydration
diagnosis(dehydration, 0.8, fatigue, low_water).
diagnosis(dehydration, 0.6, frequent_headaches, low_water).
diagnosis(dehydration, 0.5, dizziness, low_water).
diagnosis(dehydration, 0.4, dry_skin, _).
diagnosis(dehydration, 0.7, muscle_cramps, high_caffeine).
diagnosis(dehydration, 0.6, muscle_cramps, high_alcohol).
diagnosis(dehydration, 0.7, poor_concentration, energy_drinks).
diagnosis(dehydration, 0.8, dehydration_symptoms, coffee_dependent).

% Vitamin B12 deficiency
diagnosis(b12_deficiency, 0.8, fatigue, vegan).
diagnosis(b12_deficiency, 0.6, brain_fog, vegan).
diagnosis(b12_deficiency, 0.5, brain_fog, vegetarian).
diagnosis(b12_deficiency, 0.4, irritability, vegetarian).
diagnosis(b12_deficiency, 0.7, forgetfulness, vegan).
diagnosis(b12_deficiency, 0.6, poor_concentration, vegetarian).

% Vitamin D deficiency
diagnosis(vitamin_d_deficiency, 0.6, fatigue, _).
diagnosis(vitamin_d_deficiency, 0.5, muscle_weakness, _).
diagnosis(vitamin_d_deficiency, 0.4, hair_loss, _).
diagnosis(vitamin_d_deficiency, 0.6, poor_sleep, _).
diagnosis(vitamin_d_deficiency, 0.7, fatigue, dairy_free).
diagnosis(vitamin_d_deficiency, 0.6, depression, low_fat).
diagnosis(vitamin_d_deficiency, 0.5, stress_eating, _).

% Blood sugar issues
diagnosis(blood_sugar_imbalance, 0.8, afternoon_crash, high_sugar).
diagnosis(blood_sugar_imbalance, 0.7, irritability, high_sugar).
diagnosis(blood_sugar_imbalance, 0.6, brain_fog, high_sugar).
diagnosis(blood_sugar_imbalance, 0.5, fatigue, high_processed_food).
diagnosis(blood_sugar_imbalance, 0.8, afternoon_crash, skipping_meals).
diagnosis(blood_sugar_imbalance, 0.7, midday_slump, energy_drinks).
diagnosis(blood_sugar_imbalance, 0.6, poor_concentration, skipping_meals).
diagnosis(blood_sugar_imbalance, 0.9, energy_crashes, meal_skipping_for_study).

% Magnesium deficiency
diagnosis(magnesium_deficiency, 0.7, muscle_cramps, _).
diagnosis(magnesium_deficiency, 0.6, poor_sleep, _).
diagnosis(magnesium_deficiency, 0.5, fatigue, _).
diagnosis(magnesium_deficiency, 0.4, irritability, high_processed_food).
diagnosis(magnesium_deficiency, 0.6, anxiety, energy_drinks).
diagnosis(magnesium_deficiency, 0.7, exam_anxiety, high_caffeine).

% Protein deficiency
diagnosis(protein_deficiency, 0.8, fatigue, low_protein).
diagnosis(protein_deficiency, 0.7, weak_nails, low_protein).
diagnosis(protein_deficiency, 0.6, hair_loss, low_protein).
diagnosis(protein_deficiency, 0.5, slow_healing, low_protein).
diagnosis(protein_deficiency, 0.7, poor_stamina, vegan).
diagnosis(protein_deficiency, 0.6, post_workout_fatigue, low_protein).
diagnosis(protein_deficiency, 0.8, muscle_weakness, budget_eating).

% Gut health issues
diagnosis(gut_health_issues, 0.8, digestive_issues, high_processed_food).
diagnosis(gut_health_issues, 0.7, digestive_issues, low_fiber).
diagnosis(gut_health_issues, 0.5, brain_fog, low_fiber).
diagnosis(gut_health_issues, 0.4, fatigue, high_sugar).
diagnosis(gut_health_issues, 0.6, acne, high_sugar).
diagnosis(gut_health_issues, 0.6, poor_concentration, high_processed_food).
diagnosis(gut_health_issues, 0.7, digestive_issues, late_night_eating).

% Omega-3 deficiency
diagnosis(omega3_deficiency, 0.6, brain_fog, low_vegetables).
diagnosis(omega3_deficiency, 0.5, dry_skin, vegan).
diagnosis(omega3_deficiency, 0.4, irritability, _).
diagnosis(omega3_deficiency, 0.7, poor_concentration, _).
diagnosis(omega3_deficiency, 0.6, depression, vegan).
diagnosis(omega3_deficiency, 0.5, forgetfulness, low_variety).

% Zinc deficiency
diagnosis(zinc_deficiency, 0.7, acne, _).
diagnosis(zinc_deficiency, 0.6, slow_healing, _).
diagnosis(zinc_deficiency, 0.5, hair_loss, _).
diagnosis(zinc_deficiency, 0.6, weak_immune_system, vegan).
diagnosis(zinc_deficiency, 0.6, weak_immune_system, vegetarian).
diagnosis(zinc_deficiency, 0.7, poor_concentration, low_protein).

% Vitamin A deficiency
diagnosis(vitamin_a_deficiency, 0.6, dry_eyes, _).
diagnosis(vitamin_a_deficiency, 0.5, acne, _).
diagnosis(vitamin_a_deficiency, 0.7, poor_immune_function, _).
diagnosis(vitamin_a_deficiency, 0.6, dry_skin, low_fat).
diagnosis(vitamin_a_deficiency, 0.8, eye_strain, low_vegetables).
diagnosis(vitamin_a_deficiency, 0.7, vision_problems, low_variety).

% Calcium deficiency
diagnosis(calcium_deficiency, 0.8, muscle_cramps, dairy_free).
diagnosis(calcium_deficiency, 0.7, muscle_cramps, vegan).
diagnosis(calcium_deficiency, 0.6, weak_nails, dairy_free).
diagnosis(calcium_deficiency, 0.6, tooth_problems, _).
diagnosis(calcium_deficiency, 0.5, weak_bones, vegan).
diagnosis(calcium_deficiency, 0.6, post_workout_fatigue, dairy_free).

% Potassium deficiency
diagnosis(potassium_deficiency, 0.8, muscle_cramps, _).
diagnosis(potassium_deficiency, 0.7, heart_palpitations, _).
diagnosis(potassium_deficiency, 0.6, fatigue, _).
diagnosis(potassium_deficiency, 0.5, digestive_issues, _).
diagnosis(potassium_deficiency, 0.7, muscle_weakness, _).
diagnosis(potassium_deficiency, 0.8, muscle_weakness, energy_drinks).
diagnosis(potassium_deficiency, 0.6, post_workout_fatigue, low_fruits).

% Electrolyte imbalance
diagnosis(electrolyte_imbalance, 0.8, muscle_cramps, high_water).
diagnosis(electrolyte_imbalance, 0.7, heart_palpitations, high_water).
diagnosis(electrolyte_imbalance, 0.6, fatigue, low_salt).
diagnosis(electrolyte_imbalance, 0.7, headaches, _).
diagnosis(electrolyte_imbalance, 0.5, dizziness, _).
diagnosis(electrolyte_imbalance, 0.8, post_workout_fatigue, low_salt).
diagnosis(electrolyte_imbalance, 0.7, muscle_cramps, energy_drinks).

% Inflammation issues
diagnosis(inflammation_issues, 0.7, joint_pain, high_omega6).
diagnosis(inflammation_issues, 0.6, acne, high_sugar).
diagnosis(inflammation_issues, 0.8, digestive_issues, high_processed_food).
diagnosis(inflammation_issues, 0.5, fatigue, _).
diagnosis(inflammation_issues, 0.7, skin_problems, _).
diagnosis(inflammation_issues, 0.6, brain_fog, convenience_foods).
diagnosis(inflammation_issues, 0.5, midday_slump, campus_food_only).

% Excessive caffeine
diagnosis(excessive_caffeine, 0.8, anxiety, high_caffeine).
diagnosis(excessive_caffeine, 0.7, poor_sleep, high_caffeine).
diagnosis(excessive_caffeine, 0.6, heart_palpitations, high_caffeine).
diagnosis(excessive_caffeine, 0.5, irritability, high_caffeine).
diagnosis(excessive_caffeine, 0.8, dehydration_symptoms, high_caffeine).
diagnosis(excessive_caffeine, 0.9, caffeine_jitters, energy_drinks).
diagnosis(excessive_caffeine, 0.7, anxiety, coffee_dependent).
diagnosis(excessive_caffeine, 0.8, afternoon_crash, coffee_dependent).

% Folate deficiency
diagnosis(folate_deficiency, 0.7, fatigue, _).
diagnosis(folate_deficiency, 0.6, mouth_sores, _).
diagnosis(folate_deficiency, 0.8, brain_fog, _).
diagnosis(folate_deficiency, 0.5, depression, _).
diagnosis(folate_deficiency, 0.7, anemia_symptoms, _).
diagnosis(folate_deficiency, 0.6, forgetfulness, low_vegetables).
diagnosis(folate_deficiency, 0.5, poor_concentration, convenience_foods).

% Student diet syndrome
diagnosis(student_diet_syndrome, 0.8, fatigue, convenience_foods).
diagnosis(student_diet_syndrome, 0.7, brain_fog, campus_food_only).
diagnosis(student_diet_syndrome, 0.6, low_energy, budget_eating).
diagnosis(student_diet_syndrome, 0.9, weight_gain, midnight_snacking).
diagnosis(student_diet_syndrome, 0.8, acne, vending_machine_reliance).
diagnosis(student_diet_syndrome, 0.7, poor_sleep, late_night_eating).
diagnosis(student_diet_syndrome, 0.6, digestive_issues, fast_eating).
diagnosis(student_diet_syndrome, 0.8, afternoon_crash, meal_skipping_for_study).
diagnosis(student_diet_syndrome, 0.7, poor_concentration, irregular_meals).
diagnosis(student_diet_syndrome, 0.6, stress_eating, emotional_eating).

% Recommendations based on diagnoses
% Format: recommendation(Issue, Recommendation)
recommendation(iron_deficiency, "Include more iron-rich foods like leafy greens, beans, and lentils. Consider eating vitamin C foods with iron sources to improve absorption.").
recommendation(dehydration, "Increase water intake to at least 8 glasses daily. Reduce caffeine and alcohol consumption. Try adding electrolytes to water if exercising.").
recommendation(b12_deficiency, "For vegans and vegetarians, consider B12 supplements or consuming B12-fortified foods like nutritional yeast and plant milks.").
recommendation(vitamin_d_deficiency, "Spend 15-30 minutes in sunlight daily if possible. Consider vitamin D supplements, especially during winter months.").
recommendation(blood_sugar_imbalance, "Reduce sugar intake and processed foods. Eat regular meals with protein and healthy fats to stabilize blood sugar.").
recommendation(magnesium_deficiency, "Include magnesium-rich foods like dark chocolate, avocados, nuts, and whole grains. Consider supplements if symptoms persist.").
recommendation(protein_deficiency, "Increase protein intake through lean meats, eggs, dairy, or plant sources like beans, lentils, and tofu. Aim for protein with each meal.").
recommendation(gut_health_issues, "Increase fiber intake through whole grains, fruits, and vegetables. Consider adding fermented foods like yogurt or kimchi for probiotics.").
recommendation(omega3_deficiency, "Include more omega-3 rich foods like flaxseeds, chia seeds, walnuts or fatty fish if not vegetarian/vegan.").
recommendation(zinc_deficiency, "Increase intake of zinc-rich foods like pumpkin seeds, chickpeas, and legumes. Consider a zinc supplement, especially for vegetarians and vegans.").
recommendation(vitamin_a_deficiency, "Add more orange and yellow vegetables (carrots, sweet potatoes), and leafy greens to your diet. Ensure adequate fat intake for proper absorption.").
recommendation(calcium_deficiency, "Include calcium-rich foods like fortified plant milks, tofu, almonds, and leafy greens. Consider a calcium supplement if you avoid dairy completely.").
recommendation(potassium_deficiency, "Add potassium-rich foods like bananas, potatoes, beans, and leafy greens to your diet. Be cautious with supplements as excess potassium can be harmful.").
recommendation(electrolyte_imbalance, "Balance water intake with electrolyte-rich foods. Consider adding a pinch of salt to water during intense exercise or use natural electrolyte drinks.").
recommendation(inflammation_issues, "Adopt an anti-inflammatory diet rich in omega-3s, colorful vegetables, and spices like turmeric. Reduce processed foods, sugar, and vegetable oils.").
recommendation(excessive_caffeine, "Gradually reduce caffeine intake by substituting with herbal teas or decaf options. Be careful of withdrawal symptoms and aim to stop caffeine at least 6 hours before bedtime.").
recommendation(folate_deficiency, "Increase consumption of leafy greens, legumes, and fortified grains. Consider a folate supplement, especially during high-stress academic periods.").
recommendation(student_diet_syndrome, "Plan and prep balanced meals in advance to avoid relying on convenience foods. Include protein, whole grains, and vegetables in every meal. Stock up on healthy snacks for study sessions.").

% Main rule to determine diagnoses for a user
diagnose(Symptoms, Habits, Diagnoses) :-
    findall(diagnosis(Issue, CF), (
        member(Symptom, Symptoms),
        member(Habit, Habits),
        diagnosis(Issue, CF, Symptom, Habit)
    ), AllDiagnoses),
    group_diagnoses(AllDiagnoses, GroupedDiagnoses),
    sort_diagnoses(GroupedDiagnoses, Diagnoses).

% Helper rule to group diagnoses by issue and combine CFs
group_diagnoses([], []).
group_diagnoses([diagnosis(Issue, CF)|Rest], GroupedRest) :-
    group_diagnoses_for_issue(Rest, Issue, CF, RestCF, RestWithoutIssue),
    group_diagnoses(RestWithoutIssue, GroupedRestWithoutIssue),
    GroupedRest = [diagnosis(Issue, RestCF)|GroupedRestWithoutIssue].

group_diagnoses_for_issue([], _, CF, CF, []).
group_diagnoses_for_issue([diagnosis(Issue, CF2)|Rest], Issue, CF1, FinalCF, RestWithoutIssue) :-
    combine_cf(CF1, CF2, CombinedCF),
    group_diagnoses_for_issue(Rest, Issue, CombinedCF, FinalCF, RestWithoutIssue).
group_diagnoses_for_issue([Diagnosis|Rest], Issue, CF, FinalCF, [Diagnosis|RestWithoutIssue]) :-
    Diagnosis = diagnosis(OtherIssue, _),
    Issue \= OtherIssue,
    group_diagnoses_for_issue(Rest, Issue, CF, FinalCF, RestWithoutIssue).

% Helper rule to sort diagnoses by CF (descending) using Prolog's built-in sort
% This replaces the recursive insertion sort that was causing stack overflow
sort_diagnoses(Diagnoses, SortedDiagnoses) :-
    % Convert diagnoses to pairs for sorting
    findall(CF-diagnosis(Issue, CF), member(diagnosis(Issue, CF), Diagnoses), Pairs),
    % Sort pairs by CF (descending)
    sort(Pairs, SortedPairs),
    % Reverse to get descending order
    reverse(SortedPairs, ReversedPairs),
    % Extract diagnoses from pairs
    findall(diagnosis(I, C), member(C-diagnosis(I, C), ReversedPairs), SortedDiagnoses).

% Get recommendations for diagnoses
get_recommendations([], []).
get_recommendations([diagnosis(Issue, CF)|Rest], [recommendation(Issue, Recommendation, CF)|RestRecommendations]) :-
    recommendation(Issue, Recommendation),
    get_recommendations(Rest, RestRecommendations).

% Get all available symptoms for frontend
all_symptoms(Symptoms) :-
    findall(symptom(Name, Description), symptom(Name, Description), Symptoms).

% Get all available diet habits for frontend
all_diet_habits(Habits) :-
    findall(diet_habit(Name, Description), diet_habit(Name, Description), Habits).