import { useEffect, useState } from 'react';
import { Model } from 'survey-core';
import { Survey } from 'survey-react-ui';
import 'survey-core/survey-core.min.css';
import { surveyJson } from './surveyConfig';
import { processSurveyResults, classifyResponse } from './processor';

function App() {
  const [survey, setSurvey] = useState(null);

  useEffect(() => {
    const surveyModel = new Model(surveyJson);
    
    // Apply modern theme (defaultV2)
    surveyModel.applyTheme({
      "cssVariables": {
        "--sjs-general-backcolor": "rgba(255, 255, 255, 1)",
        "--sjs-font-family": "Inter, sans-serif",
        "--sjs-primary-backcolor": "#4f46e5",
        "--sjs-primary-backcolor-light": "rgba(79, 70, 229, 0.1)",
        "--sjs-primary-forecolor": "#ffffff",
        "--sjs-base-unit": "8px",
        "--sjs-corner-radius": "12px",
        "--sjs-border-default": "rgba(0, 0, 0, 0.12)",
        "--sjs-border-inside": "rgba(0, 0, 0, 0.16)",
        "--sjs-shadow-small": "0px 4px 12px 0px rgba(0, 0, 0, 0.05)",
        "--sjs-shadow-inner": "inset 0px 2px 4px 0px rgba(0, 0, 0, 0.02)"
      }
    });

    surveyModel.onCurrentPageChanging.add((sender, options) => {
      // Classifica a resposta exatamente quando a página tenta avançar (por clique ou Enter)
      if (options.oldCurrentPage) {
        options.oldCurrentPage.questions.forEach(q => {
          if (q.name && !q.name.startsWith("class_")) {
            const classification = classifyResponse(q.value);
            sender.setValue(`class_${q.name}`, classification);
            console.log(`Classified [${q.name}] = ${classification}`);
          }
        });
      }
    });

    surveyModel.onComplete.add((sender, options) => {
      // Pass the complete survey state to our processor to adhere to low coupling
      processSurveyResults(sender);
    });

    setSurvey(surveyModel);
  }, []);

  if (!survey) return null;

  const handleKeyDown = (e) => {
    if (e.key === 'Enter') {
      e.preventDefault();
      if (survey.isLastPage) {
        survey.completeLastPage();
      } else {
        survey.nextPage();
      }
    }
  };

  return (
    <div className="app-container" onKeyDown={handleKeyDown}>
      <div className="glass-panel">
        <Survey model={survey} />
      </div>
    </div>
  );
}

export default App;
