#!/usr/bin/env python3
import os
import platform

# Set SWI_HOME_DIR environment variable to fix PySwip error
os.environ["SWI_HOME_DIR"] = "/Applications/SWI-Prolog.app/Contents/Resources/swipl"

# Initialize PySwip for macOS
if platform.system() == 'Darwin':
    # Adding exact paths as in test_system.py to ensure compatibility
    os.environ['DYLD_LIBRARY_PATH'] = '/opt/homebrew/lib/swipl/lib/arm64-darwin'
    os.environ['DYLD_FRAMEWORK_PATH'] = '/opt/homebrew/lib'

from pyswip import Prolog
import re

def escape_string_for_prolog(text):
    """Properly escape a string for safe use in Prolog queries"""
    # Replace single quotes with doubled single quotes (Prolog's escape mechanism)
    text = text.replace("'", "''")
    # Replace other problematic characters
    text = text.replace("\\", "\\\\")
    return text

def test_complex_nlp():
    """Test the NLP parser with various complex natural language inputs"""
    prolog = Prolog()
    
    try:
        # Use absolute path to load both knowledge base and NLP parser
        import os
        current_dir = os.path.dirname(os.path.abspath(__file__))
        nlp_parser_path = os.path.join(current_dir, "nlp_parser.pl")
        print(f"Loading parser from: {nlp_parser_path}")
        prolog.consult(nlp_parser_path)
        print("NLP parser loaded successfully.")
    except Exception as e:
        print(f"Error loading NLP parser: {e}")
        return False
    
    # Define complex test cases - pairs of (input text, expected symptoms, expected diet habits)
    test_cases = [
        # Student lifestyle examples
        (
            "I feel exhausted during finals week, cant sleep well and I skip meals to study",
            ["fatigue", "poor_sleep"],
            ["meal_skipping_for_study"]
        ),
        (
            "I have acne breakouts, feel foggy, and mostly eat cheap dining hall food",
            ["acne", "brain_fog"],
            ["campus_food_only"]
        ),
        (
            "I drink lots of coffee, get jittery often, and crash in the afternoon",
            ["caffeine_jitters", "afternoon_crash"],
            ["coffee_dependent"] 
        ),
        (
            "I'm following a vegan diet but my nails are weak and I feel tired all the time",
            ["weak_nails", "fatigue"],
            ["vegan"]
        ),
        (
            "I have digestive issues, bloating, and I eat a lot of instant noodles",
            ["digestive_issues"],
            ["convenience_foods"]
        ),
        (
            "My stomach hurts and I feel moody, I dont drink much water and love sugary drinks",
            ["digestive_issues", "irritability"],
            ["low_water", "high_sugar"]
        ),
        (
            "I'm stressed about exams, overthinking everything, and mostly eat from vending machines",
            ["anxiety"],
            ["vending_machine_reliance"]
        ),
        (
            "I forget things easily and have trouble concentrating in class. I usually skip breakfast",
            ["brain_fog", "poor_concentration"],
            ["skipping_meals"]
        )
    ]
    
    # Run all test cases
    for i, (input_text, expected_symptoms, expected_habits) in enumerate(test_cases, 1):
        print(f"\nTest Case {i}: '{input_text}'")
        
        # Escape the input text for safe use in Prolog
        escaped_input = escape_string_for_prolog(input_text)
        
        try:
            query_str = f"parse_input('{escaped_input}', Symptoms, DietHabits)"
            result = list(prolog.query(query_str))
            
            if result:
                detected_symptoms = list(result[0]["Symptoms"])
                detected_habits = list(result[0]["DietHabits"])
                
                print(f"  Expected Symptoms: {', '.join(expected_symptoms)}")
                print(f"  Detected Symptoms: {', '.join(detected_symptoms)}")
                
                print(f"  Expected Diet Habits: {', '.join(expected_habits)}")
                print(f"  Detected Diet Habits: {', '.join(detected_habits)}")
                
                # Check if all expected items are detected (not requiring exact match)
                symptoms_match = all(s in detected_symptoms for s in expected_symptoms)
                habits_match = all(h in detected_habits for h in expected_habits)
                
                if symptoms_match and habits_match:
                    print("  ✅ All expected terms found")
                else:
                    print("  ⚠️ Some expected terms missing")
                    if not symptoms_match:
                        missing = [s for s in expected_symptoms if s not in detected_symptoms]
                        print(f"    - Missing symptoms: {', '.join(missing)}")
                    if not habits_match:
                        missing = [h for h in expected_habits if h not in detected_habits]
                        print(f"    - Missing diet habits: {', '.join(missing)}")
                
                # Check for unexpected extras
                unexpected_symptoms = [s for s in detected_symptoms if s not in expected_symptoms]
                unexpected_habits = [h for h in detected_habits if h not in expected_habits]
                
                if unexpected_symptoms or unexpected_habits:
                    print("  ℹ️ Additional terms detected:")
                    if unexpected_symptoms:
                        print(f"    - Additional symptoms: {', '.join(unexpected_symptoms)}")
                    if unexpected_habits:
                        print(f"    - Additional diet habits: {', '.join(unexpected_habits)}")
            else:
                print("  ❌ No parse result returned")
                
        except Exception as e:
            print(f"  ❌ Error: {e}")
        
        print("-" * 60)
    
    return True

if __name__ == "__main__":
    test_complex_nlp() 