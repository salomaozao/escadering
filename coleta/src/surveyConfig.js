export const surveyJson = {
  title: "Pesquisa de Percepção Baseada em Valores",
  description: "Ajude-nos a entender o que é mais importante para você em relação a este produto/serviço.",
  textUpdateMode: "onTyping",
  showProgressBar: "top",
  pages: [
    {
      name: "ladder_1_A",
      elements: [
        {
          type: "text",
          name: "A1",
          title: "Qual o atributo principal que chamou sua atenção? (1º mais importante)",
          isRequired: true
        }
      ]
    },
    {
      name: "ladder_1_C",
      visibleIf: "{class_A1} != 'C' and {class_A1} != 'V'",
      elements: [
        {
          type: "text",
          name: "C1",
          title: "O que isso traz de consequência?",
          isRequired: true
        }
      ]
    },
    {
      name: "ladder_1_V",
      visibleIf: "{class_A1} != 'V' and {class_C1} != 'V'",
      elements: [
        {
          type: "text",
          name: "V1",
          title: "Por que isso é importante para você?",
          isRequired: true
        }
      ]
    },
    {
      name: "ladder_2_A",
      elements: [
        {
          type: "text",
          name: "A2",
          title: "Qual outro atributo chamou sua atenção? (2º mais importante)"
        }
      ]
    },
    {
      name: "ladder_2_C",
      visibleIf: "{class_A2} != 'C' and {class_A2} != 'V'",
      elements: [
        {
          type: "text",
          name: "C2",
          title: "O que esse segundo atributo traz de consequência?"
        }
      ]
    },
    {
      name: "ladder_2_V",
      visibleIf: "{class_A2} != 'V' and {class_C2} != 'V'",
      elements: [
        {
          type: "text",
          name: "V2",
          title: "E por que isso é importante para você?"
        }
      ]
    },
    {
      name: "ladder_3_A",
      elements: [
        {
          type: "text",
          name: "A3",
          title: "Há ainda um terceiro atributo que chamou sua atenção?"
        }
      ]
    },
    {
      name: "ladder_3_C",
      visibleIf: "{class_A3} != 'C' and {class_A3} != 'V'",
      elements: [
        {
          type: "text",
          name: "C3",
          title: "O que isso traz de consequência?"
        }
      ]
    },
    {
      name: "ladder_3_V",
      visibleIf: "{class_A3} != 'V' and {class_C3} != 'V'",
      elements: [
        {
          type: "text",
          name: "V3",
          title: "Por que isso também é importante para você?"
        }
      ]
    }
  ],
  showQuestionNumbers: "off",
  clearInvisibleValues: "onHidden",
  completeText: "Finalizar"
};
