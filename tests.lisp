
(in-package :string-rewriting)

(defun test-rule (rule input expected)
  (let ((result (apply-rule (compile-rule rule) input)))
    (unless (string-equal result expected)
      (format t "Expected [~a] got [~a]~%" expected result))))

(defun test-regex-rule (rule input expected)
  (let ((result (apply-rule (compile-regex-rule rule) input)))
    (unless (string-equal result expected)
      (format t "Expected [~a] got [~a]~%" expected result))))

(defun test-rules (rules input expected)
  (let ((result (apply-rules (compile-rules rules) input)))
    (unless (string-equal result expected)
      (format t "Expected [~a] got [~a]~%" expected result))))

(defun test-multiple-rules ()
  (test-rules (list (list "foo_T1" "bar_T2") (list "bar_T2" "baz_T3")) "foo_T1 bar_T2" "baz_T3 baz_T3")
  (test-rules (list (list "foo_T1" "bar_T2") (list "bar_T2" "baz_T3")) "foo_T1 bar_T3" "baz_T3 bar_T3")
  (test-rules (list (list "foo_T1" "bar_T2") (list "bar_T3" "baz_T3")) "foo_T1 bar_T3" "bar_T2 baz_T3"))

(defun test-invalid-rules ()
  (test-rule (list "*_PCP" "*.ADJ")  "foo_PCP"  "foo_PCP"))

(defun test-strict-matching ()
  (test-rule (list "J_N" "J_NOUN") "Joao_N f" "Joao_N f")
  (test-rule (list "J_N" "J_NOUN") "J_N f"  "J_NOUN f")

  (test-rule (list "J_N" "J_NOUN") "Joao_N" "Joao_N")
  (test-rule (list "J_N" "J_NOUN") "J_N" "J_NOUN")

  (test-rule (list "*_N" "*_NOUN") "Joao_N" "Joao_NOUN")

  (test-rule (list "*_N" "*_NOUN") "Joao_NOUN foi_V" "Joao_NOUN foi_V")
  (test-rule (list "*_N" "*_NOUN") "Joao_N foi_V" "Joao_NOUN foi_V"))

;; 1- transformar "_ART *_ADJ seguido por qualquer coisa que não seja *_N" em "_ART *_N seguido por qualquer coisa que não seja *_N"
;; 2- Transformar uma palavra do tipo "algo-algumacoisa_N" (exemplo: ex-presidente_N) em "algo_PRT -_PUNCT algumacoisa_N"
;; 3- Transformar qualquer número marcado como _N, mesmo que contenha os caracteres , . º (exemplos: 1,5_N; 1.991_N, 1º_N), em _NUM.

;; Para isso, o Alexandre criou as seguintes regras:

;; 1-      (define-rule "@{\\w+_ART} @{\\w+_ADJ} @{\\w+}_@{^N}" "@{\\1} @{\\2_N} @{\\3}_@{\\4}" :mm)
;; 2-      (define-rule "@{[\\w]+}-@{\\w+}_@{\\w+}" "@{\\1}_PRT -_PUNCT @{\\2} @{\\3}" :mm)
;; 3-      (define-rule "@{[\\d\\,\\.\\º]+}_N" "@{[\\d\\,\\.\\º]+}_NUM" :mm)

(defun test-regex-rules ()
  (test-regex-rule (list "(\\w+)_ART (\\w+)_ADJ (\\w+)_([^N])" "\\1_ART \\2_N \\3_\\4") "os_ART velhos_ADJ riram_V" "os_ART velhos_N riram_V")
  (test-regex-rule (list "(\\w+)_ART (\\w+)_ADJ (\\w+)_([^N])" "\\1_ART \\2_N \\3_\\4") "os_ART velhos_ADJ não_ADV correm_V" "os_ART velhos_N não_ADV correm_V")
  (test-regex-rule (list "(\\w+)_ART (\\w+)_ADJ (\\w+)_([^N])" "\\1_ART \\2_N \\3_\\4") "os_ART velhos_ADJ foram_AUX atendidos_V" "os_ART velhos_N foram_AUX atendidos_V")
  (test-regex-rule (list "(\\w+)_ART (\\w+)_ADJ (\\w+)_([^N])" "\\1_ART \\2_N \\3_\\4") "os_ART velhos_ADJ soldados_N reclamaram_V" "os_ART velhos_ADJ soldados_N reclamaram_V")
  (test-regex-rule (list "(\\w+)-(\\w+)_(\\w+)(\\s+)" "\\1_PRT -_PUNCT \\2_\\3\\4") "ex-presidente_N foo" "ex_PRT -_PUNCT presidente_N foo") 
  (test-regex-rule (list "(\\w+)-(\\w+)_(\\w+)$" "\\1_PRT -_PUNCT \\2_\\3") "ex-presidente_N" "ex_PRT -_PUNCT presidente_N") 
  (test-regex-rule (list "([\\d\\,\\.\\º]+)_N(\\s+)" "\\1_NUM\\2") "Ele tinha 1000_N macas." "Ele tinha 1000_NUM macas.")

  (test-regex-rule (list "([\\d\\,\\.\\º]+)_N(\\W*)" "\\1_NUM\\2") "Ele tinha 1000_N" "Ele tinha 1000_NUM")
  (test-regex-rule (list "([\\d\\,\\.\\º]+)_N(\\W*)" "\\1_NUM\\2") "Ele tinha 1000_N foobar." "Ele tinha 1000_NUM foobar.")
  (test-regex-rule (list "([\\d\\,\\.\\º]+)_N(\\W*)" "\\1_NUM\\2") "Ele tinha 1000_N!" "Ele tinha 1000_NUM!")
  (test-regex-rule (list "([\\d\\,\\.\\º]+)_N(\\W*)" "\\1_NUM\\2") "Ele tinha 1000_N, foobar." "Ele tinha 1000_NUM, foobar.")


  (test-regex-rule (list "([\\d\\,\\.\\º]+)_N$" "\\1_NUM") "1000_N" "1000_NUM")
  (test-regex-rule (list "([\\d\\,\\.\\º]+)_N" "\\1_NUM") "1000_N" "1000_NUM")
  (test-regex-rule (list "([\\d\\,\\.\\º]+)_N" "\\1_NUM") "1,000_N" "1,000_NUM")
  (test-regex-rule (list "([\\d\\,\\.\\º]+)_N" "\\1_NUM") "10.00_N" "10.00_NUM")
  (test-regex-rule (list "([\\d\\,\\.\\º]+)_N" "\\1_NUM") "1000º_N" "1000º_NUM"))

(defun test-escaping ()
  (test-rule (list "?_?" "?_PU") "this_IS foo_BAR baz_QWE foo_TYP ?_?" "this_IS foo_BAR baz_QWE foo_TYP ?_PU"))

(defun test-rules-happy-path ()
  (test-rule (list "?_?" "?_PU") "this_IS foo_BAR baz_QWE foo_TYP ?_?" "this_IS foo_BAR baz_QWE foo_TYP ?_PU")
  (test-rule (list "._." "._PU") "this_IS foo_BAR baz_QWE foo_TYP ._." "this_IS foo_BAR baz_QWE foo_TYP ._PU")
  (test-rule (list "foo_BAR baz_QWE" "foo_BAR baz_EWQ") "this_IS foo_BAR baz_QWE foo_TYP" "this_IS foo_BAR baz_EWQ foo_TYP")
  (test-rule (list "*_N passad*_PCP" "*_N passad*_ADJ") "foo_N passados_PCP" "foo_N passados_ADJ")
  (test-rule (list "*_PCP ._." "*_ADJ ._.") "adequadas_PCP a_PREP a_ART" "adequadas_PCP a_PREP a_ART")
  (test-rule (list "*_PCP ._." "*_ADJ ._.") "adequadas_PCP ._." "adequadas_ADJ ._.")
  (test-rule (list "passado_PCP" "passado_ADJ") "A_ART previsão_N vender_V 3.800_NUM colheitadeiras_N em_PREP 94_N ,_, contra_PREP as_ART 2.735_N de_PREP o_ART ano_N passado_PCP ._." "A_ART previsão_N vender_V 3.800_NUM colheitadeiras_N em_PREP 94_N ,_, contra_PREP as_ART 2.735_N de_PREP o_ART ano_N passado_ADJ ._.")
  (test-rule (list "passada_PCP" "passada_ADJ") "A_ART área_N de_PREP plantio_N direto_ADJ de_PREP arroz_N em_PREP a_ART safra_N 94/95_N é_V 59_NUM %_N maior_ADJ que_KS a_PROSUB de_PREP a_ART safra_N passada_PCP ._." "A_ART área_N de_PREP plantio_N direto_ADJ de_PREP arroz_N em_PREP a_ART safra_N 94/95_N é_V 59_NUM %_N maior_ADJ que_KS a_PROSUB de_PREP a_ART safra_N passada_ADJ ._.")
  (test-rule (list "passados_PCP" "passados_ADJ") "E_KC aí_ADV vai_V oportunamente_ADV o_ART lembrete_N de_PREP o_ART escritor_N espanhol_ADJ Miguel_NPROP de_NPROP Cervantes_NPROP ,_, que_PRO-KS-REL há_V quase_ADV cinco_NUM séculos_N passados_PCP ,_, costumava_VAUX dizer_V que_KS um_ART homem_N bem-informado_PCP vale_V por_PREP dois_N ._." "E_KC aí_ADV vai_V oportunamente_ADV o_ART lembrete_N de_PREP o_ART escritor_N espanhol_ADJ Miguel_NPROP de_NPROP Cervantes_NPROP ,_, que_PRO-KS-REL há_V quase_ADV cinco_NUM séculos_N passados_ADJ ,_, costumava_VAUX dizer_V que_KS um_ART homem_N bem-informado_PCP vale_V por_PREP dois_N ._.")
  (test-rule (list "passadas_PCP" "passadas_ADJ") "Em_PREP a_ART época_N ,_, foram_VAUX passadas_PCP procurações_N em_PREP nome_N de_PREP \"_\" fantasmas_N \"_\" autorizando_V a_ART venda_N de_PREP propriedades_N ._." "Em_PREP a_ART época_N ,_, foram_VAUX passadas_ADJ procurações_N em_PREP nome_N de_PREP \"_\" fantasmas_N \"_\" autorizando_V a_ART venda_N de_PREP propriedades_N ._.")
  (test-rule (list "fixo_PCP" "fixo_ADJ") "Pouco_PROSUB importa_V que_KS não_ADV tenha_V emprego_N fixo_PCP ._." "Pouco_PROSUB importa_V que_KS não_ADV tenha_V emprego_N fixo_ADJ ._.")
  (test-rule (list "fixa_PCP" "fixa_ADJ") "Eles_PROPESS trabalham_V para_PREP empresas_N que_PRO-KS-REL distribuem_V os_ART pedidos_N de_PREP os_ART passageiros_N por_PREP o_ART rádio_N ,_, pagando_V uma_ART comissão_N ou_KC taxa_N fixa_PCP por_PREP o_ART serviço_N ._." "Eles_PROPESS trabalham_V para_PREP empresas_N que_PRO-KS-REL distribuem_V os_ART pedidos_N de_PREP os_ART passageiros_N por_PREP o_ART rádio_N ,_, pagando_V uma_ART comissão_N ou_KC taxa_N fixa_ADJ por_PREP o_ART serviço_N ._.")
  (test-rule (list "fixos_PCP" "fixos_ADJ") "Os_ART 11_NUM pontos_N são_V fixos_PCP e_KC considerados_PCP por_PREP a_ART polícia_N as_ART principais_ADJ rotas_N de_PREP fuga_N de_PREP ladrões_N ._." "Os_ART 11_NUM pontos_N são_V fixos_ADJ e_KC considerados_PCP por_PREP a_ART polícia_N as_ART principais_ADJ rotas_N de_PREP fuga_N de_PREP ladrões_N ._.")
  (test-rule (list "fixas_PCP" "fixas_ADJ") "Mas_KC Rosenberg_NPROP concluiu_V que_KS eram_V os_ART porcos_N ,_, e_KC não_ADV os_ART cereais_N ,_, a_ART princiapl_ADJ fonte_N de_PREP alimento_N em_PREP as_ART primeiras_ADJ comunidades_N fixas_PCP ._." "Mas_KC Rosenberg_NPROP concluiu_V que_KS eram_V os_ART porcos_N ,_, e_KC não_ADV os_ART cereais_N ,_, a_ART princiapl_ADJ fonte_N de_PREP alimento_N em_PREP as_ART primeiras_ADJ comunidades_N fixas_ADJ ._.")
  (test-rule (list "*_VAUX sido_PCP *_PCP" "*_VAUX sido_PCP *_V") "Tinha_VAUX sido_PCP apresentado_PCP por_PREP a_ART criadora_N Simone_NPROP Nowak_NPROP ._." "Tinha_VAUX sido_PCP apresentado_V por_PREP a_ART criadora_N Simone_NPROP Nowak_NPROP ._.")
  (test-rule (list "sido_PCP *_V" "sido_VAUX *_V") "Em_PREP um_ART balanço_N de_PREP estes_PROADJ três_NUM meses_N de_PREP a_ART segunda_ADJ etapa_N de_PREP a_ART campanha_N em_PREP o_ART Rio_NPROP ,_, o_ART economista_N Maurício_NPROP de_NPROP Andrade_NPROP diz_V que_KS a_ART preocupação_N tem_VAUX sido_PCP catalogar_V experiências_N de_PREP trabalho_N bem_PCP sucedidas_PCP ._." "Em_PREP um_ART balanço_N de_PREP estes_PROADJ três_NUM meses_N de_PREP a_ART segunda_ADJ etapa_N de_PREP a_ART campanha_N em_PREP o_ART Rio_NPROP ,_, o_ART economista_N Maurício_NPROP de_NPROP Andrade_NPROP diz_V que_KS a_ART preocupação_N tem_VAUX sido_VAUX catalogar_V experiências_N de_PREP trabalho_N bem_PCP sucedidas_PCP ._.")
  (test-rule (list "ficado_PCP *_V" "ficado_VAUX *_V") "ficado_PCP foo_V" "ficado_VAUX foo_V")
  (test-rule (list "estado_PCP *_V" "estado_VAUX *_V") "estado_PCP foo_V" "estado_VAUX foo_V")
  (test-rule (list "sido_PCP" "sido_V") "Um_PROSUB de_PREP os_ART únicos_ADJ pontos_N obscuros_ADJ era_V a_ART compra_N de_PREP três_NUM ambulâncias_N em_PREP 1992_N que_PRO-KS-REL nunca_ADV haviam_VAUX sido_PCP entregues_PCP ._." "Um_PROSUB de_PREP os_ART únicos_ADJ pontos_N obscuros_ADJ era_V a_ART compra_N de_PREP três_NUM ambulâncias_N em_PREP 1992_N que_PRO-KS-REL nunca_ADV haviam_VAUX sido_V entregues_PCP ._.")
  (test-rule (list "ficado_PCP" "ficado_V") "Rivas_NPROP veio_V a_PREP o_ART país_N participar_V de_PREP um_ART seminário_N sobre_PREP cultura_N indigenista_ADJ e_KC disse_V ter_VAUX ficado_PCP \"_\" horrorizado_PCP \"_\" com_PREP as_ART declarações_N de_PREP Jaguaribe_NPROP ._." "Rivas_NPROP veio_V a_PREP o_ART país_N participar_V de_PREP um_ART seminário_N sobre_PREP cultura_N indigenista_ADJ e_KC disse_V ter_VAUX ficado_V \"_\" horrorizado_PCP \"_\" com_PREP as_ART declarações_N de_PREP Jaguaribe_NPROP ._.")
  (test-rule (list "estado_PCP" "estado_V") "estado_PCP foo_BAR" "estado_V foo_BAR")
  (test-rule (list "*_PCP ,_," "*_ADJ ,_,") "É_V que_KS a_ART velocidade_N de_PREP a_ART ocupação_N de_PREP o_ART Cerrado_NPROP ,_, feito_PCP de_PREP maneira_N desordenada_PCP ,_, veio_V a_ART degradação_N de_PREP as_ART pastagens_N e_KC a_ART erosão_N ._." "É_V que_KS a_ART velocidade_N de_PREP a_ART ocupação_N de_PREP o_ART Cerrado_NPROP ,_, feito_PCP de_PREP maneira_N desordenada_ADJ ,_, veio_V a_ART degradação_N de_PREP as_ART pastagens_N e_KC a_ART erosão_N ._.")
  (test-rule (list "*_VAUX *_PCP" "*_VAUX *_V") "Somente_PDEN em_PREP 93_N ,_, foram_VAUX realizados_PCP quatro_NUM leilões_N oficiais_ADJ ._." "Somente_PDEN em_PREP 93_N ,_, foram_VAUX realizados_V quatro_NUM leilões_N oficiais_ADJ ._.")
  (test-rule (list "*_VAUX *_ADV *_PCP" "*_VAUX *_ADV *_V") "Eles_PROPESS subiram_V a_PREP o_ART palco_N e_KC foram_VAUX bastante_ADV aplaudidos_PCP ._." "Eles_PROPESS subiram_V a_PREP o_ART palco_N e_KC foram_VAUX bastante_ADV aplaudidos_V ._.")
  (test-rule (list "*_PCP por_PREP" "*_V por_PREP") "Estudo_N feito_PCP por_PREP pesquisadores_N de_PREP a_ART Universidade_NPROP Federal_NPROP do_NPROP Piauí_NPROP revela_V que_KS 15,7_NUM milhões_N de_PREP pessoas_N são_VAUX afetadas_PCP por_PREP a_ART desertificação_N que_PRO-KS-REL ocorre_V em_PREP o_ART Nordeste_NPROP ._." "Estudo_N feito_V por_PREP pesquisadores_N de_PREP a_ART Universidade_NPROP Federal_NPROP do_NPROP Piauí_NPROP revela_V que_KS 15,7_NUM milhões_N de_PREP pessoas_N são_VAUX afetadas_V por_PREP a_ART desertificação_N que_PRO-KS-REL ocorre_V em_PREP o_ART Nordeste_NPROP ._.")
  (test-rule (list "já_ADV *_PCP" "já_ADV *_V") "Os_ART filmes_N já_ADV utilizados_PCP devem_VAUX ser_VAUX guardados_PCP em_PREP a_ART embalagem_N original_ADJ e_KC em_PREP local_N arejado_PCP e_KC seco_ADJ ._." "Os_ART filmes_N já_ADV utilizados_V devem_VAUX ser_VAUX guardados_PCP em_PREP a_ART embalagem_N original_ADJ e_KC em_PREP local_N arejado_PCP e_KC seco_ADJ ._.")
  (test-rule (list "já_ADV *_ADV *_PCP" "já_ADV *_ADV *_V") "já_ADV foo_ADV bar_PCP" "já_ADV foo_ADV bar_V")
  (test-rule (list "quando_ADV *_PCP" "quando_ADV *_V") "A_ART colorização_N de_PREP uma_ART fotografia_N ,_, como_ADV-KS-REL era_V feita_PCP antigamente_ADV ,_, ganha_V outras_PROADJ dimensões_N quando_ADV elaborada_PCP em_PREP o_ART computador_N ._." "A_ART colorização_N de_PREP uma_ART fotografia_N ,_, como_ADV-KS-REL era_V feita_PCP antigamente_ADV ,_, ganha_V outras_PROADJ dimensões_N quando_ADV elaborada_V em_PREP o_ART computador_N ._.")
  (test-rule (list "quando_ADV *_ADV *_PCP" "quando_ADV *_ADV *_V") "Mesmo_PDEN quando_ADV convenientemente_ADV cuidados_PCP ,_, como_KS é_V o_ART caso_N de_PREP o_ART São_NPROP João_NPROP ,_, o_ART mais_ADV novo_ADJ de_PREP eles_PROPESS ._." "Mesmo_PDEN quando_ADV convenientemente_ADV cuidados_V ,_, como_KS é_V o_ART caso_N de_PREP o_ART São_NPROP João_NPROP ,_, o_ART mais_ADV novo_ADJ de_PREP eles_PROPESS ._.")
  (test-rule (list "depois_PREP de_PREP *_PCP" "depois_PREP de_PREP *_V") "O_ART deputado_N Paulo_NPROP Portugal_NPROP (_( )_) foi_VAUX julgado_PCP por_PREP o_ART plenário_N 222_NUM dias_N depois_PREP de_PREP encerrada_PCP a_ART CPI_NPROP ._." "O_ART deputado_N Paulo_NPROP Portugal_NPROP (_( )_) foi_VAUX julgado_PCP por_PREP o_ART plenário_N 222_NUM dias_N depois_PREP de_PREP encerrada_V a_ART CPI_NPROP ._.")
  (test-rule (list "tão_ADV *_PCP" "tão_ADV *_ADJ") "Ela_PROPESS ficou_V tão_ADV decepcionada_PCP com_PREP a_ART recusa_N que_KS até_PDEN cancelou_V o_PRO-KS que_PRO-KS teria_VAUX sido_PCP sua_PROADJ primeira_ADJ (_( e_KC )_) viagem_N a_PREP o_ART Brasil_NPROP em_PREP a_ART companhia_N de_PREP o_ART marido_N ,_, David_NPROP Sebastian_NPROP ._." "Ela_PROPESS ficou_V tão_ADV decepcionada_ADJ com_PREP a_ART recusa_N que_KS até_PDEN cancelou_V o_PRO-KS que_PRO-KS teria_VAUX sido_PCP sua_PROADJ primeira_ADJ (_( e_KC )_) viagem_N a_PREP o_ART Brasil_NPROP em_PREP a_ART companhia_N de_PREP o_ART marido_N ,_, David_NPROP Sebastian_NPROP ._.")
  (test-rule (list "até_PREP então_ADV *_PCP" "até_PREP então_ADV *_ADJ") "A_ART tarefa_N era_V portanto_KC grande_ADJ ,_, mesmo_PDEN porque_KS as_ART nações_N até_PREP então_ADV selecionadas_PCP ,_, esmeraram_V se_PROPESS em_PREP a_ART idealização_N de_PREP suas_PROADJ exposições_N ,_, exibindo_V a_ART sua_PROADJ tecnologia_N ,_, apresentando_V a_ART sua_PROADJ literatura_N ,_, ou_KC tematizando_V a_ART sua_PROADJ história_N ._." "A_ART tarefa_N era_V portanto_KC grande_ADJ ,_, mesmo_PDEN porque_KS as_ART nações_N até_PREP então_ADV selecionadas_ADJ ,_, esmeraram_V se_PROPESS em_PREP a_ART idealização_N de_PREP suas_PROADJ exposições_N ,_, exibindo_V a_ART sua_PROADJ tecnologia_N ,_, apresentando_V a_ART sua_PROADJ literatura_N ,_, ou_KC tematizando_V a_ART sua_PROADJ história_N ._.")
  (test-rule (list "muito_ADV *_PCP" "muito_ADV *_ADJ") "\"_\" Ficamos_V muito_ADV impressionados_PCP com_PREP o_PROSUB que_PRO-KS-REL ouvimos_V e_KC vimos_V em_PREP o_ART Borel_NPROP ._." "\"_\" Ficamos_V muito_ADV impressionados_ADJ com_PREP o_PROSUB que_PRO-KS-REL ouvimos_V e_KC vimos_V em_PREP o_ART Borel_NPROP ._.")
  (test-rule (list "*_ADV *_PCP" "*_ADV *_ADJ")  "foo_ADV bar_PCP" "foo_ADV bar_ADJ")
  (test-rule (list "*_ADJ e_KC *_PCP" "*_ADJ e_KC *_ADJ") "Quarta-feira_N ,_, a_PREP as_ART 20h_N ,_, o_ART pregão_N deve_VAUX vender_V 30_NUM animais_N ,_, entre_PREP nacionais_ADJ e_KC importados_PCP ._." "Quarta-feira_N ,_, a_PREP as_ART 20h_N ,_, o_ART pregão_N deve_VAUX vender_V 30_NUM animais_N ,_, entre_PREP nacionais_ADJ e_KC importados_ADJ ._."))

(defun test-all ()
  (test-regex-rules)
  (test-escaping)
  (test-strict-matching)
  (test-rules-happy-path)
  (test-multiple-rules)
  (test-invalid-rules))

