document.addEventListener('DOMContentLoaded', function() {
    // Elements
    const diagnoseBtn = document.getElementById('diagnoseBtn');
    const backBtn = document.getElementById('backBtn');
    const questionnaireSection = document.getElementById('questionnaire');
    const resultsSection = document.getElementById('results');
    const diagnoseResults = document.getElementById('diagnoseResults');
    const noResults = document.getElementById('noResults');
    
    // Event listeners
    diagnoseBtn.addEventListener('click', handleDiagnose);
    backBtn.addEventListener('click', handleBack);
    
    // Handle diagnose button click
    async function handleDiagnose() {
        // Get selected symptoms
        const symptomCheckboxes = document.querySelectorAll('.symptom-checkbox:checked');
        const symptoms = Array.from(symptomCheckboxes).map(checkbox => checkbox.value);
        
        // Get selected diet habits
        const dietCheckboxes = document.querySelectorAll('.diet-checkbox:checked');
        const dietHabits = Array.from(dietCheckboxes).map(checkbox => checkbox.value);
        
        // Validation - make sure user selected at least one item
        if (symptoms.length === 0 && dietHabits.length === 0) {
            alert('Please select at least one symptom or dietary habit to get recommendations.');
            return;
        }
        
        try {
            // Show loading state
            diagnoseBtn.textContent = 'Analyzing...';
            diagnoseBtn.disabled = true;
            
            // Send request to backend
            const response = await fetch('/diagnose', {
                method: 'POST',
                headers: {
                    'Content-Type': 'application/json',
                },
                body: JSON.stringify({
                    symptoms: symptoms,
                    diet_habits: dietHabits
                })
            });
            
            const data = await response.json();
            
            // Reset button state
            diagnoseBtn.textContent = 'Get Recommendations';
            diagnoseBtn.disabled = false;
            
            // Display results
            displayResults(data);
            
            // Switch to results view
            questionnaireSection.classList.add('hidden');
            resultsSection.classList.remove('hidden');
            
            // Scroll to top
            window.scrollTo(0, 0);
        } catch (error) {
            console.error('Error:', error);
            alert('Something went wrong. Please try again.');
            
            // Reset button state
            diagnoseBtn.textContent = 'Get Recommendations';
            diagnoseBtn.disabled = false;
        }
    }
    
    // Display diagnosis results
    function displayResults(data) {
        // Clear previous results
        diagnoseResults.innerHTML = '';
        
        if (!data.success || data.diagnoses.length === 0) {
            // Show no results message
            noResults.classList.remove('hidden');
            diagnoseResults.classList.add('hidden');
            return;
        }
        
        // Hide no results message and show results
        noResults.classList.add('hidden');
        diagnoseResults.classList.remove('hidden');
        
        // Create result cards
        data.diagnoses.forEach(diagnosis => {
            const resultCard = createResultCard(diagnosis);
            diagnoseResults.appendChild(resultCard);
        });
    }
    
    // Create a result card for a diagnosis
    function createResultCard(diagnosis) {
        const card = document.createElement('div');
        card.className = 'result-card';
        
        // Create header
        const header = document.createElement('div');
        header.className = 'result-header';
        
        const issue = document.createElement('span');
        issue.className = 'result-issue';
        issue.textContent = diagnosis.issue;
        
        const confidence = document.createElement('span');
        confidence.className = 'confidence-badge';
        confidence.textContent = `${diagnosis.confidence} (${diagnosis.percentage}%)`;
        
        header.appendChild(issue);
        header.appendChild(confidence);
        
        // Create body
        const body = document.createElement('div');
        body.className = 'result-body';
        
        const advice = document.createElement('p');
        advice.className = 'result-advice';
        advice.textContent = diagnosis.advice;
        
        const meterLabel = document.createElement('small');
        meterLabel.textContent = 'Confidence level:';
        
        const meter = document.createElement('div');
        meter.className = 'confidence-meter';
        
        const level = document.createElement('div');
        level.className = 'confidence-level';
        level.style.width = `${diagnosis.percentage}%`;
        
        meter.appendChild(level);
        
        body.appendChild(advice);
        body.appendChild(meterLabel);
        body.appendChild(meter);
        
        // Assemble card
        card.appendChild(header);
        card.appendChild(body);
        
        return card;
    }
    
    // Handle back button click
    function handleBack() {
        // Switch to questionnaire view
        resultsSection.classList.add('hidden');
        questionnaireSection.classList.remove('hidden');
        
        // Scroll to top
        window.scrollTo(0, 0);
    }
}); 