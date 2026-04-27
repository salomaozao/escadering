// A função abaixo é um "Mock" e deverá ser substituída caso conectem LLM no futuro.
export const classifyResponse = (text) => {
  if (!text) return 'A';

  const lowerText = text.toLowerCase();

  // Dicionário temporário para Valores
  const valueKeywords = ['liberdade', 'felicidade', 'paz', 'segurança', 'importante', 'sinto'];
  // Dicionário temporário para Consequências
  const conseqKeywords = ['ajuda', 'rápido', 'tempo', 'útil', 'resultado', 'permite', 'faz com que'];

  if (valueKeywords.some(kw => lowerText.includes(kw))) { return 'V'; }
  if (conseqKeywords.some(kw => lowerText.includes(kw))) { return 'C'; }

  // Resto torna-se atributo por exclusão provisória
  return 'A'; 
};

// Processo nativo da Fase 1 - inalterado
export const processSurveyResults = (sender) => {
  const data = sender.data;
  
  let currentId = parseInt(localStorage.getItem('laddering_current_id') || '0', 10);
  currentId += 1;
  localStorage.setItem('laddering_current_id', currentId.toString());

  const resultRow = {
    ID: currentId,
    A1: data.A1 || "",
    C1: data.C1 || "",
    V1: data.V1 || "",
    A2: data.A2 || "",
    C2: data.C2 || "",
    V2: data.V2 || "",
    A3: data.A3 || "",
    C3: data.C3 || "",
    V3: data.V3 || ""
  };

  console.log("=== NOVA RESPOSTA REGISTRADA (10 Colunas) ===");
  console.table([resultRow]);
  
  return resultRow;
};
