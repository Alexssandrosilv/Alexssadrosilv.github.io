// Espera a página carregar inteira
window.addEventListener('DOMContentLoaded', (event) => {
  
  // Encontra os dois elementos que criamos: o <select> e a <div> de resultado
  const seletor = document.getElementById('seletor-variavel');
  const resultado = document.getElementById('resultado-modelo');

  // Se o seletor existir na página, execute isso:
  if (seletor) {
    
    // Escuta por uma "mudança" (alguém clicou no dropdown)
    seletor.addEventListener('change', function() {
      
      let conteudoHTML = '';
      
      // Verifica qual foi o valor selecionado
      switch(this.value) {
        
        case 'continua':
          conteudoHTML = `
            <h4>Modelo Sugerido: Regressão Linear (MLG)</h4>
            <p><strong>Pressupostos:</strong> Linearidade, Normalidade dos resíduos ( $Y \\sim N(\\mu, \\sigma^2)$ ), Homocedasticidade. Ideal para dados simétricos.</p>
            <p><strong>Função de Ligação:</strong> Identidade ( $g(\\mu) = \\mu$ )</p>
          `;
          break;
          
        case 'contagem':
          conteudoHTML = `
            <h4>Modelo Sugerido: Regressão de Poisson (MLG)</h4>
            <p><strong>Pressupostos:</strong> Dados são contagens (0, 1, 2,...). Média deve ser igual à variância (equidispersão). A variância aumenta com a média.</p>
            <p><strong>Função de Ligação:</strong> Log ( $\\log(\\mu) = \\mathbf{X}\\beta$ )</p>
            <p><strong>Alternativa (se houver superdispersão):</strong> Binomial Negativa.</p>
          `;
          break;
          
        case 'binaria':
          conteudoHTML = `
            <h4>Modelo Sugerido: Regressão Logística (MLG)</h4>
            <p><strong>Pressupostos:</strong> Resposta é binária (0 ou 1, sucesso/falha, sim/não). Modela a probabilidade de "sucesso" ( $\\pi$ ).</p>
            <p><strong>Função de Ligação:</strong> Logit ( $\\log(\\frac{\\pi}{1-\\pi}) = \\mathbf{X}\\beta$ )</p>
          `;
          break;
          
        case 'proporcao':
          conteudoHTML = `
            <h4>Modelo Sugerido: Regressão Beta</h4>
            <p><strong>Pressupostos:</strong> Resposta é uma taxa ou proporção estritamente entre 0 e 1 (ex: 0.25, 0.87). Muito flexível para modelar formas diferentes.</p>
            <p><strong>Função de Ligação:</strong> Logit (comumente usada para a média $\\mu$)</p>
          `;
          break;

        case 'tempo':
           conteudoHTML = `
            <h4>Modelo Sugerido: Regressão de Sobrevivência (Cox) ou Gama (MLG)</h4>
            <p><strong>Modelo Cox (Semiparamétrico):</strong> Não assume distribuição para o tempo. Modela a *taxa de risco* (hazard). O mais comum.</p>
            <p><strong>Modelo Gama (MLG):</strong> Assume $Y \\sim \\text{Gama}(\\mu, \\phi)$. Requer dados positivos e assimétricos (como tempo de espera).</p>
          `;
          break;
          
        default:
          conteudoHTML = '<p style="color: #888;">Selecione um tipo de variável para ver os pressupostos e o modelo.</p>';
      }
      
      // Coloca o HTML que criamos dentro da <div> de resultado
      resultado.innerHTML = conteudoHTML;
      
      // Pede ao Quarto para "renderizar" o LaTeX (matemática) que acabamos de adicionar
      if (window.MathJax) {
        window.MathJax.typesetPromise([resultado]);
      }
    });
  }
});