# NutriLogic - Student Nutrition Expert System

This is a student-focused nutrition expert system that helps diagnose possible nutrition-related issues based on user symptoms and dietary habits. The system uses a Prolog knowledge base with an easy-to-use web interface built with Flask, and includes natural language processing capabilities.

## Project Structure

- `knowledge_base.pl` - The Prolog knowledge base with rules for nutrition-related diagnoses
- `nlp_parser.pl` - Definite Clause Grammar rules for natural language processing of symptoms and dietary habits
- `app.py` - Flask application that serves as an interface between the frontend and Prolog
- `test_system.py` - Test script to verify the diagnosis system's functionality with sample cases
- `test_complex_nlp.py` - Test script to verify the natural language processing component
- `static/` - Contains CSS, JavaScript, and image files
- `templates/` - Contains HTML templates for the web interface

## Requirements

- Python 3.8 or higher
- SWI-Prolog 8.0 or higher
- Python packages listed in `requirements.txt`

## Installation

### 1. Install SWI-Prolog

#### macOS
```
brew install swi-prolog
```

#### Windows
Download and install from the [SWI-Prolog website](https://www.swi-prolog.org/download/stable)

#### Linux (Ubuntu/Debian)
```
sudo apt-get install swi-prolog
```

### 2. Make sure SWI-Prolog is in your PATH

After installation, verify that SWI-Prolog is correctly installed by running:
```
swipl --version
```

### 3. Install Python dependencies

Create a virtual environment (optional but recommended):
```
python -m venv venv
source venv/bin/activate  # On Windows: venv\Scripts\activate
```

Install the required packages:
```
pip install -r requirements.txt
```

## Running the Application

1. Start the Flask application:
```
python app.py
```

2. Open your browser and navigate to `http://localhost:8080`

## Testing the System

You can run automated test cases for the diagnosis functionality:
```
python test_system.py
```

To test the natural language processing capabilities:
```
python test_complex_nlp.py
```

## Features

- Interactive questionnaire about symptoms and dietary habits
- Natural language input option for describing symptoms and diet
- Special focus on student-specific nutrition issues and scenarios
- Rule-based reasoning to suggest possible nutritional causes
- Certainty factors to indicate confidence in diagnoses (with percentage display)
- Practical advice based on identified issues

## Student-Specific Focus

This expert system is specially designed with college students in mind, addressing common issues such as:
- Exam week nutrition challenges
- Budget-friendly healthy eating options
- Dorm and campus dining nutritional considerations
- Energy drink and caffeine management
- Irregular meal schedules due to classes
- Study-related eating habits

## Troubleshooting

- If you encounter `ERROR: atom_chars/2: Arguments are not sufficiently instantiated`, make sure SWI-Prolog is correctly installed and in your PATH.
- For PySwip issues, ensure you're using a compatible version of SWI-Prolog (8.0.x is recommended).
- On macOS, if you encounter dynamic library loading issues with messages about `libswipl` or `libgmp`, you may need to set environment variables:
  ```python
  os.environ["SWI_HOME_DIR"] = "/Applications/SWI-Prolog.app/Contents/Resources/swipl"
  os.environ['DYLD_LIBRARY_PATH'] = '/opt/homebrew/lib/swipl/lib/arm64-darwin'
  os.environ['DYLD_FRAMEWORK_PATH'] = '/opt/homebrew/lib'
  ```
- On Windows, you may need to copy `libswipl.dll` from your SWI-Prolog installation to your Python directory or add the SWI-Prolog bin directory to your PATH.

## Note

This is an educational tool and not a replacement for professional medical or nutritional advice. Always consult a healthcare provider for serious health concerns. 