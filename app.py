import os
# Set SWI_HOME_DIR environment variable to fix PySwip error
os.environ["SWI_HOME_DIR"] = "/Applications/SWI-Prolog.app/Contents/Resources/swipl"
from pyswip import Prolog
from flask import Flask, render_template, request, jsonify
import re
import sys
import platform
import logging

# Set up logging
logging.basicConfig(filename='app_debug.log', level=logging.DEBUG, 
                    format='%(asctime)s %(levelname)s: %(message)s')
logger = logging.getLogger(__name__)

# Initialize PySwip for macOS
if platform.system() == 'Darwin':
    # Adding Homebrew's SWI-Prolog to the dynamic library path
    os.environ['DYLD_LIBRARY_PATH'] = '/opt/homebrew/lib/swipl/lib/arm64-darwin'
    os.environ['DYLD_FRAMEWORK_PATH'] = '/opt/homebrew/lib'

app = Flask(__name__)
prolog = Prolog()

# Load the knowledge base with absolute path
current_dir = os.path.dirname(os.path.abspath(__file__))
knowledge_base_path = os.path.join(current_dir, "knowledge_base.pl")
nlp_parser_path = os.path.join(current_dir, "nlp_parser.pl")
prolog.consult(knowledge_base_path)
prolog.consult(nlp_parser_path)

def format_certainty(cf):
    """Convert certainty factor to percentage and description"""
    percentage = int(float(cf) * 100)
    
    if percentage >= 80:
        confidence = "Very likely"
    elif percentage >= 60:
        confidence = "Likely"
    elif percentage >= 40:
        confidence = "Possible"
    else:
        confidence = "Less likely"
        
    return percentage, confidence

def format_issue_name(issue):
    """Convert snake_case to Title Case with proper handling of special terms"""
    logger.debug(f"Formatting issue name: {issue}")
    
    # Handle special abbreviations
    issue = re.sub(r'b12', 'B12', issue)
    issue = re.sub(r'omega3', 'Omega-3', issue)
    issue = re.sub(r'vitamin_d', 'Vitamin D', issue)
    issue = re.sub(r'd_deficiency', 'D Deficiency', issue) # For vitamin_d_deficiency
    
    # Convert the rest from snake_case to Title Case
    words = [word.capitalize() for word in issue.split('_')]
    result = ' '.join(words)
    
    logger.debug(f"Formatted name: {result}")
    return result

@app.route('/')
def index():
    """Render the main page"""
    symptoms = list(prolog.query("all_symptoms(Symptoms)"))
    diet_habits = list(prolog.query("all_diet_habits(Habits)"))
    
    logger.debug(f"Raw symptoms: {symptoms}")
    if symptoms:
        logger.debug(f"Symptoms[0]: {symptoms[0]}")
        logger.debug(f"Symptoms[0]['Symptoms'] type: {type(symptoms[0]['Symptoms'])}")
        logger.debug(f"First symptom: {symptoms[0]['Symptoms'][0] if symptoms[0]['Symptoms'] else 'No symptoms'}")
        
    logger.debug(f"Raw diet_habits: {diet_habits}")
    if diet_habits:
        logger.debug(f"Diet_habits[0]: {diet_habits[0]}")
        logger.debug(f"Diet_habits[0]['Habits'] type: {type(diet_habits[0]['Habits'])}")
        logger.debug(f"First habit: {diet_habits[0]['Habits'][0] if diet_habits[0]['Habits'] else 'No habits'}")
    
    if symptoms and diet_habits:
        symptoms = symptoms[0]['Symptoms']
        diet_habits = diet_habits[0]['Habits']
        
        # Format symptoms for template
        formatted_symptoms = []
        for i, symptom in enumerate(symptoms):
            logger.debug(f"Processing symptom {i}: {symptom}, type: {type(symptom)}")
            
            # Parse the symptom string using regex
            symptom_match = re.match(r"symptom\((\w+),\s*b'([^']+)'\)", str(symptom))
            
            if symptom_match:
                name = symptom_match.group(1)
                description = symptom_match.group(2)
                logger.debug(f"Extracted name: {name}, description: {description}")
            elif hasattr(symptom, 'args'):
                # If it's a PySwip Term, use args as before
                logger.debug(f"Symptom {i} has args attribute")
                name = symptom.args[0].value
                description = symptom.args[1].value
                logger.debug(f"Extracted name: {name}, description: {description}")
            else:
                # Fallback if parsing fails
                logger.debug(f"Parsing failed for symptom {i}")
                name = str(symptom)
                description = "Description not available"
                logger.debug(f"Fallback name: {name}")
            
            formatted_symptoms.append({
                'id': name,
                'name': format_issue_name(name),
                'description': description
            })
            
        # Format diet habits for template
        formatted_habits = []
        for i, habit in enumerate(diet_habits):
            logger.debug(f"Processing habit {i}: {habit}, type: {type(habit)}")
            
            # Parse the habit string using regex
            habit_match = re.match(r"diet_habit\((\w+),\s*b'([^']+)'\)", str(habit))
            
            if habit_match:
                name = habit_match.group(1)
                description = habit_match.group(2)
                logger.debug(f"Extracted name: {name}, description: {description}")
            elif hasattr(habit, 'args'):
                # If it's a PySwip Term, use args as before
                logger.debug(f"Habit {i} has args attribute")
                name = habit.args[0].value
                description = habit.args[1].value
                logger.debug(f"Extracted name: {name}, description: {description}")
            else:
                # Fallback if parsing fails
                logger.debug(f"Parsing failed for habit {i}")
                name = str(habit)
                description = "Description not available"
                logger.debug(f"Fallback name: {name}")
            
            formatted_habits.append({
                'id': name,
                'name': format_issue_name(name),
                'description': description
            })
            
        return render_template('index.html', 
                               symptoms=formatted_symptoms,
                               diet_habits=formatted_habits)
    
    return "Error loading knowledge base", 500

@app.route('/parse_natural_language', methods=['POST'])
def parse_natural_language():
    """Process natural language input to extract symptoms and diet habits"""
    data = request.json
    text = data.get('text', '')
    
    if not text:
        return jsonify({
            'success': False,
            'message': 'No text provided'
        })
        
    logger.debug(f"Processing natural language input: {text}")
    
    try:
        # Escape single quotes for Prolog
        escaped_text = text.replace("'", "''")
        
        # Query the Prolog NLP parser
        query = f"parse_input('{escaped_text}', Symptoms, DietHabits)"
        logger.debug(f"Prolog NLP query: {query}")
        
        results = list(prolog.query(query))
        logger.debug(f"Prolog NLP results: {results}")
        
        if not results:
            logger.debug("No results returned from Prolog NLP parser")
            return jsonify({
                'success': True,
                'symptoms': [],
                'diet_habits': []
            })
        
        detected_symptoms = results[0]['Symptoms']
        detected_diet_habits = results[0]['DietHabits']
        
        logger.debug(f"Detected symptoms: {detected_symptoms}")
        logger.debug(f"Detected diet habits: {detected_diet_habits}")
        
        # Get all available symptoms and diet habits for lookup
        all_symptoms = []
        all_habits = []
        
        # Get symptoms from knowledge base
        symptoms_query = list(prolog.query("all_symptoms(Symptoms)"))
        if symptoms_query:
            all_symptoms = symptoms_query[0]['Symptoms']
            logger.debug(f"All symptoms: {all_symptoms}")
            
        # Get diet habits from knowledge base
        habits_query = list(prolog.query("all_diet_habits(Habits)"))
        if habits_query:
            all_habits = habits_query[0]['Habits']
            logger.debug(f"All habits: {all_habits}")
        
        # Format detected symptoms with names
        formatted_symptoms = []
        for symptom in detected_symptoms:
            symptom_str = str(symptom)
            logger.debug(f"Looking for symptom: {symptom_str}")
            formatted_symptoms.append({
                'id': symptom_str,
                'name': format_issue_name(symptom_str)
            })
        
        # Format detected diet habits with names
        formatted_habits = []
        for habit in detected_diet_habits:
            habit_str = str(habit)
            logger.debug(f"Looking for habit: {habit_str}")
            formatted_habits.append({
                'id': habit_str,
                'name': format_issue_name(habit_str)
            })
        
        logger.debug(f"Formatted symptoms to return: {formatted_symptoms}")
        logger.debug(f"Formatted habits to return: {formatted_habits}")
        
        return jsonify({
            'success': True,
            'symptoms': formatted_symptoms,
            'diet_habits': formatted_habits
        })
        
    except Exception as e:
        logger.error(f"Error processing natural language: {e}")
        return jsonify({
            'success': False,
            'message': f'Error processing input: {str(e)}'
        })

@app.route('/diagnose', methods=['POST'])
def diagnose():
    """Process the user's symptoms and diet habits to generate diagnoses"""
    data = request.json
    
    selected_symptoms = data.get('symptoms', [])
    selected_habits = data.get('diet_habits', [])
    
    logger.debug(f"Selected symptoms: {selected_symptoms}")
    logger.debug(f"Selected habits: {selected_habits}")
    
    # Convert to Prolog list format
    symptoms_list = '[' + ','.join(selected_symptoms) + ']'
    habits_list = '[' + ','.join(selected_habits) + ']'
    
    logger.debug(f"Symptoms list for Prolog: {symptoms_list}")
    logger.debug(f"Habits list for Prolog: {habits_list}")
    
    # Query for diagnoses
    query = f"diagnose({symptoms_list}, {habits_list}, Diagnoses)"
    logger.debug(f"Prolog query: {query}")
    
    results = list(prolog.query(query))
    logger.debug(f"Prolog diagnose results: {results}")
    
    if not results:
        logger.debug("No results returned from Prolog diagnose query")
        return jsonify({
            'success': False,
            'message': 'Unable to generate diagnoses with the provided information.'
        })
    
    diagnoses = results[0]['Diagnoses']
    logger.debug(f"Diagnoses: {diagnoses}")
    
    # Get recommendations for the diagnoses
    # Instead of passing the string representation, parse and reconstruct a proper Prolog list
    formatted_diagnoses = []
    for diag in diagnoses:
        match = re.match(r"diagnosis\((\w+),\s*([\d\.]+)\)", diag)
        if match:
            issue = match.group(1)
            cf = match.group(2)
            formatted_diagnoses.append(f"diagnosis({issue}, {cf})")
    
    diagnoses_list = "[" + ",".join(formatted_diagnoses) + "]"
    recommendations_query = f"get_recommendations({diagnoses_list}, Recommendations)"
    logger.debug(f"Recommendations query: {recommendations_query}")
    
    recommendations_results = list(prolog.query(recommendations_query))
    logger.debug(f"Recommendations results: {recommendations_results}")
    
    formatted_results = []
    
    if recommendations_results:
        recommendations = recommendations_results[0]['Recommendations']
        logger.debug(f"Recommendations: {recommendations}")
        
        for rec in recommendations:
            logger.debug(f"Processing recommendation: {rec}, type: {type(rec)}")
            
            # Parse the recommendation string using regex
            rec_match = re.match(r"recommendation\((\w+),\s*b?'([^']+)',\s*([\d\.]+)\)", str(rec))
            
            if rec_match:
                issue = rec_match.group(1)
                advice = rec_match.group(2)
                cf = float(rec_match.group(3))
                logger.debug(f"Extracted issue: {issue}, advice: {advice}, cf: {cf}")
            elif hasattr(rec, 'args'):
                # If it's a PySwip Term, use args as before
                logger.debug(f"Recommendation has args attribute")
                issue = rec.args[0].value
                advice = rec.args[1].value
                cf = float(rec.args[2].value)
                logger.debug(f"Extracted issue: {issue}, advice: {advice}, cf: {cf}")
            else:
                # Handle different data structures
                try:
                    # If it's a tuple-like structure
                    issue = str(rec[0])
                    advice = str(rec[1])
                    cf = float(rec[2])
                    logger.debug(f"Extracted from indexing: issue: {issue}, advice: {advice}, cf: {cf}")
                except (IndexError, TypeError, ValueError):
                    # Fallback with default values
                    logger.debug(f"Parsing failed for recommendation")
                    issue = str(rec)
                    advice = "No specific advice available"
                    cf = 0.5  # Default certainty
                    logger.debug(f"Using fallback values")
            
            percentage, confidence = format_certainty(cf)
            
            formatted_results.append({
                'issue': format_issue_name(issue),
                'advice': advice,
                'confidence': confidence,
                'percentage': percentage
            })
    else:
        logger.debug("No recommendations results returned")
    
    logger.debug(f"Formatted results: {formatted_results}")
    
    return jsonify({
        'success': True,
        'diagnoses': formatted_results
    })

if __name__ == '__main__':
    app.run(debug=True, port=8080) 