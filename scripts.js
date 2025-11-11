// scripts.js

// Espera a página carregar inteiramente para começar
window.onload = function() {
  
  // --- Nossas imagens de fundo (padrões de estatística/dados) ---
  const listaDeFundos = [
    // Padrão de fórmulas matemáticas
    'url("https://www.svgbackgrounds.com/preview/mathematical-expression-paper.svg")',
    
    // Padrão de papel milimetrado
    'url("https://www.svgbackgrounds.com/preview/graph-paper.svg")',
    
    // Padrão de "placa de circuito" (lembra análise)
    'url("https://www.svgbackgrounds.com/preview/circuit-board.svg")',
    
    // Padrão de cubos (lembra blocos de dados)
    'url("https://www.svgbackgrounds.com/preview/overlapping-cubes.svg")'
  ];

  let i = 0;
  
  // Define o primeiro fundo assim que a página carrega
  document.body.style.backgroundImage = listaDeFundos[i];

  // A cada 7 segundos, troca o fundo
  setInterval(() => {
    // Avança para o próximo item da lista
    i = (i + 1) % listaDeFundos.length;
    
    // Aplica o novo fundo ao 'body'
    document.body.style.backgroundImage = listaDeFundos[i];
    
  }, 7000); // 7000 milissegundos = 7 segundos
};