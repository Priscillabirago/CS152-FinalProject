#!/usr/bin/env python3
import os
import platform

# Set SWI_HOME_DIR environment variable to fix PySwip error
os.environ["SWI_HOME_DIR"] = "/Applications/SWI-Prolog.app/Contents/Resources/swipl"

# Initialize PySwip for macOS
if platform.system() == 'Darwin':
    # Adding Homebrew's SWI-Prolog to the dynamic library path
    os.environ['DYLD_LIBRARY_PATH'] = '/opt/homebrew/lib/swipl/lib/arm64-darwin'
    os.environ['DYLD_FRAMEWORK_PATH'] = '/opt/homebrew/lib'

from pyswip import Prolog
import json

def test_knowledge_base():
    """Test the Prolog knowledge base with various test cases"""
    prolog = Prolog()
    
    try:
        # Use absolute path to load knowledge base
        import os
        current_dir = os.path.dirname(os.path.abspath(__file__))
        knowledge_base_path = os.path.join(current_dir, "knowledge_base.pl")
        prolog.consult(knowledge_base_path)
        print("Knowledge base loaded successfully.")
    except Exception as e:
        print(f"Error loading knowledge base: {e}")
        return False
    
    print("\n=== Running Test Cases ===\n")
    
    # Test case 1: Fatigue + Low Water Intake
    test_case(prolog, 
              "Case 1: Fatigue + Low Water Intake", 
              ["fatigue"], 
              ["low_water"])
    
    # Test case 2: Multiple symptoms + Multiple diet habits 
    test_case(prolog,
              "Case 2: Multiple Symptoms + Multiple Diet Habits",
              ["fatigue", "brain_fog", "irritability"],
              ["high_sugar", "low_protein", "skipping_meals"])
    
    # Test case 3: Vegan diet with specific symptoms
    test_case(prolog,
              "Case 3: Vegan Diet with Specific Symptoms",
              ["fatigue", "brain_fog", "weak_nails"],
              ["vegan"])
    
    # Test case 4: Processed food diet with digestive issues
    test_case(prolog,
              "Case 4: Processed Food Diet with Digestive Issues",
              ["digestive_issues", "fatigue", "afternoon_crash"],
              ["high_processed_food", "high_sugar"])
    
    # Test case 5: Student specific case - Exam week syndrome
    test_case(prolog,
              "Case 5: Exam Week Syndrome",
              ["caffeine_jitters", "poor_concentration", "anxiety", "poor_sleep"],
              ["energy_drinks", "coffee_dependent", "irregular_meals", "meal_skipping_for_study"])
    
    # Test case 6: Student specific case - Budget dorm diet
    test_case(prolog,
              "Case 6: Budget Dorm Diet",
              ["fatigue", "acne", "brain_fog"],
              ["budget_eating", "convenience_foods", "campus_food_only", "vending_machine_reliance"])
    
    return True

def test_case(prolog, case_name, symptoms, diet_habits):
    """Run a specific test case and display results"""
    print(f"=== {case_name} ===")
    print(f"Symptoms: {', '.join(symptoms)}")
    print(f"Diet Habits: {', '.join(diet_habits)}")
    
    # Format symptoms and diet habits as Prolog lists
    symptoms_list = "[" + ",".join(symptoms) + "]"
    habits_list = "[" + ",".join(diet_habits) + "]"
    
    # Run diagnosis
    query = f"diagnose({symptoms_list}, {habits_list}, Diagnoses)"
    results = list(prolog.query(query))
    
    if not results:
        print("No diagnoses found.")
        return
    
    diagnoses = results[0]["Diagnoses"]
    
    # Get recommendations in the same format as the Flask app
    formatted_diagnoses = []
    for diag in diagnoses:
        # Handle different formats of diagnosis
        if hasattr(diag, 'args'):
            issue = diag.args[0].value
            cf = diag.args[1].value
        else:
            # Parse the string representation
            import re
            match = re.match(r"diagnosis\((\w+),\s*([\d\.]+)\)", str(diag))
            if match:
                issue = match.group(1)
                cf = match.group(2)
            else:
                print(f"Could not parse diagnosis: {diag}")
                continue
                
        formatted_diagnoses.append(f"diagnosis({issue}, {cf})")
    
    diagnoses_list = "[" + ",".join(formatted_diagnoses) + "]"
    recs_query = f"get_recommendations({diagnoses_list}, Recommendations)"
    rec_results = list(prolog.query(recs_query))
    
    if not rec_results:
        print("No recommendations found.")
        return
    
    recommendations = rec_results[0]["Recommendations"]
    
    # Format and display results
    print("\nResults:")
    for rec in recommendations:
        # Handle different formats of recommendation
        if hasattr(rec, 'args'):
            issue = rec.args[0].value
            advice = rec.args[1].value
            cf = float(rec.args[2].value)
        else:
            # Parse the string representation
            import re
            rec_match = re.match(r"recommendation\((\w+),\s*b?'([^']+)',\s*([\d\.]+)\)", str(rec))
            if rec_match:
                issue = rec_match.group(1)
                advice = rec_match.group(2)
                cf = float(rec_match.group(3))
            else:
                print(f"Could not parse recommendation: {rec}")
                continue
        
        # Format the issue name to be more readable
        issue_formatted = issue.replace('_', ' ').title()
        # Handle special cases
        issue_formatted = issue_formatted.replace('B12', 'B12')
        issue_formatted = issue_formatted.replace('Omega3', 'Omega-3')
        issue_formatted = issue_formatted.replace('Vitamin D', 'Vitamin D')
        
        percentage = int(cf * 100)
        confidence = "Very likely" if percentage >= 80 else "Likely" if percentage >= 60 else "Possible" if percentage >= 40 else "Less likely"
        
        print(f"- {issue_formatted}: {confidence} ({percentage}% confidence)")
        print(f"  Advice: {advice}\n")
    
    print("-" * 50 + "\n")

if __name__ == "__main__":
    test_knowledge_base()