;;; ==============================================================
;;; MÒDULS DEL SISTEMA
;;; ==============================================================

(defmodule MAIN (export ?ALL))
(defmodule RECOLLIDA-DADES (import MAIN ?ALL) (export ?ALL))
(defmodule INFERENCIA (import MAIN ?ALL) (export ?ALL))
(defmodule RECOMANACIO (import MAIN ?ALL))

;;; ==============================================================
;;; EL MOTLLE PRINCIPAL (L'hem de definir abans d'usar-lo)
;;; ==============================================================

(deftemplate MAIN::VIATGE
    ;; =============================
    ;; SOLUCIÓ ABSTRACTA (SA)
    ;; =============================
    (slot objectiu)          
    (slot nivell_allotjament)

    ;; =============================
    ;; SOLUCIÓ CONCRETA (SC)
    ;; =============================
    (multislot ciutats)
    (multislot dies_ciutat)
    (slot dies_total (default 0))
    (slot cost_total (default 0))
    (multislot transports)
    (multislot atraccions)
    (multislot allotjaments)
    (slot puntuacio (default 0))
)

;;; ==============================================================
;;; CONTROL DE FLUX (MAIN)
;;; ==============================================================

(defrule MAIN::inici
    (declare (salience 100))
    =>
    (printout t "==========================================" crlf)
    (printout t "BENVINGUT A L'AGENCIA AL FIN DEL MUNDO" crlf)
    (printout t "==========================================" crlf)
    (make-instance [usuari_actual] of USUARI)
    (focus RECOLLIDA-DADES INFERENCIA RECOMANACIO)
)

;;; ==============================================================
;;; MÒDUL 1: RECOLLIDA DE DADES
;;; ==============================================================

(defrule RECOLLIDA-DADES::pregunta-edat
    =>
    (printout t "Quants anys tens? ")
    (bind ?resp (read))
    (send [usuari_actual] put-edat ?resp)
)

(defrule RECOLLIDA-DADES::pregunta-persones
    =>
    (printout t "Quantes persones voldreu viatjar? ")
    (bind ?resp (read))
    (if (eq ?resp 1) then
        (send [usuari_actual] put-perfil_deduit INDIVIDUAL)
        (send [usuari_actual] put-viatja_amb_nens FALSE)
    )
    (send [usuari_actual] put-persones_viatjant ?resp)
)

(defrule RECOLLIDA-DADES::pregunta-grup
    ;; Només s'executa si l'usuari ja ha dit quantes persones viatgen i el perfil encara és DESCONEGUT
    (object (is-a USUARI) (name [usuari_actual]) (persones_viatjant ?n) (perfil_deduit DESCONEGUT))
    =>
    (if (eq ?n 2) then
        (printout t "Viatges amb la teva parella (si/no): ")
        (bind ?resp (read))
        (if (eq ?resp si) then
            (send [usuari_actual] put-perfil_deduit PARELLA)
            (send [usuari_actual] put-viatja_amb_nens FALSE)
        else
            (send [usuari_actual] put-perfil_deduit GRUP)
            (send [usuari_actual] put-viatja_amb_nens FALSE)
        )
    else (if (> ?n 2) then
        (printout t "Viatges en familia o amb amics? (familia/amics): ")
        (bind ?resp (read))
        (if (eq ?resp familia) then
            (send [usuari_actual] put-perfil_deduit FAMILIA)
            (send [usuari_actual] put-viatja_amb_nens TRUE)
        else (if (eq ?resp amics) then
            (send [usuari_actual] put-perfil_deduit GRUP)
            (send [usuari_actual] put-viatja_amb_nens FALSE)
        ))
    ))
)

(defrule RECOLLIDA-DADES::pregunta-pressupost
    =>
    (printout t "Quin es el teu pressupost maxim aproximat (en euros)? ")
    (bind ?pres (read))
    (send [usuari_actual] put-pressupost_max ?pres)
)

(defrule RECOLLIDA-DADES::pregunta-dies
    =>
    (printout t "Quants dies tens disponibles per viatjar?" crlf
    "Dies mínims: ")
    (bind ?dies-min (read))
    (printout t "Dies màxims: ")
    (bind ?dies-max (read))

    (send [usuari_actual] put-dies_min ?dies-min)
    (send [usuari_actual] put-dies_max ?dies-max)
)

(defrule RECOLLIDA-DADES::pregunta-num-ciutats
    =>
    (printout t "Quantes ciutats t'interesa visitar?" crlf
    "Com a mínim: ")
    (bind ?ciutats-min (read))
    (printout t "Com a màxim: ")
    (bind ?ciutats-max (read))

    (send [usuari_actual] put-ciutats_min ?ciutats-min)
    (send [usuari_actual] put-ciutats_max ?ciutats-max)
)

(defrule RECOLLIDA-DADES::pregunta-num-dies-ciutats
    =>
    (printout t "I quants dies estaries a cada ciutat?" crlf
    "Com a mínim: ")
    (bind ?dies-ciutats-min (read))
    (printout t "Com a màxim: ")
    (bind ?dies-ciutats-max (read))

    (send [usuari_actual] put-dies_ciutats_min ?dies-ciutats-min)
    (send [usuari_actual] put-dies_ciutats_max ?dies-ciutats-max)
)

(defrule RECOLLIDA-DADES::pregunta-transport
    =>
    (printout t "Hi ha algun tipus de transporte (avió, vaixell, tren, autobus o cotxe) que no puguis agafar?" crlf)
    (printout t "Introdueix-los un a un. Escriu 'fi' quan hagis acabat o si no en vols prohibir cap." crlf)
    
    (bind $?prohibits (create$))
    (printout t "Transport prohibit: ")
    (bind ?resp (read))
    
    (while (neq ?resp fi)
        (bind ?simbol CAP)
        (if (or (eq ?resp avio) (eq ?resp avió)) then (bind ?simbol AVIO))
        (if (eq ?resp vaixell)                 then (bind ?simbol VAIXELL))
        (if (eq ?resp tren)                    then (bind ?simbol TREN))
        (if (eq ?resp autobus)                  then (bind ?simbol AUTOBUS))
        (if (eq ?resp cotxe)                   then (bind ?simbol COTXE))
        
        (if (and (neq ?simbol CAP) (not (member$ ?simbol $?prohibits)))
            then (bind $?prohibits (insert$ $?prohibits (+ (length$ $?prohibits) 1) ?simbol))
        )
        
        (printout t "Algú més? (o 'fi' per acabar): ")
        (bind ?resp (read))
    )
    
    (if (= (length$ $?prohibits) 0)
        then (bind $?prohibits (create$ CAP))
    )
    
    (send [usuari_actual] put-transport_prohibit $?prohibits)
)

(defrule RECOLLIDA-DADES::pregunta-sacrifica-qualitat
    =>
    (printout t "Estaries disposat/da a sacrificar qualitat d'allotjament per estalviar? (si/no): ")
    (bind ?resp (read))
    (if (eq ?resp si) then
        (send [usuari_actual] put-sacrifica_qualitat TRUE)
    else
        (send [usuari_actual] put-sacrifica_qualitat FALSE)
    )
)

(defrule RECOLLIDA-DADES::pregunta-destins-coneguts
    =>
    (printout t "Tens preferència per visitar llocs menys coneguts? (si/no): ")
    (bind ?resp (read))
    (if (eq ?resp si) then
        (send [usuari_actual] put-llocs_menys_coneguts TRUE)
    else
        (send [usuari_actual] put-llocs_menys_coneguts FALSE)
    )
)

(defrule RECOLLIDA-DADES::pregunta-estrelles
    =>
    (printout t "Quina es la qualitat minima (en estrelles) que exigeixes per a l'allotjament? (1-5): ")
    (bind ?resp (read))
    (send [usuari_actual] put-qualitat_min_estrelles ?resp)
)

(defrule RECOLLIDA-DADES::pregunta-nivell-cultural
    =>
    (printout t "Quin es el teu nivell d'interes cultural? (baix/mitja/alt): ")
    (bind ?resp (read))
    (if (eq ?resp baix) then (send [usuari_actual] put-nivell_cultural BAIX)
    else (if (eq ?resp mitja) then (send [usuari_actual] put-nivell_cultural MITJA)
    else (send [usuari_actual] put-nivell_cultural ALT)))
)

(defrule RECOLLIDA-DADES::pregunta-esdeveniment
    =>
    (printout t "Aquest viatge es per algun esdeveniment especial? (aniversari és de parella) (boda/fi_de_curs/aniversari/jubilacio/cap): ")
    (bind ?resp (read))
    (if (eq ?resp boda) then (send [usuari_actual] put-esdev_especial BODA)
    else (if (eq ?resp fi_de_curs) then (send [usuari_actual] put-esdev_especial FI_DE_CURS)
    else (if (eq ?resp aniversari) then (send [usuari_actual] put-esdev_especial ANIVERSARI)
    else (if (eq ?resp jubilacio) then (send [usuari_actual] put-esdev_especial JUBILACIO)
    else (send [usuari_actual] put-esdev_especial CAP)))))
)

;;; ==============================================================
;;; MÒDUL 2: INFERÈNCIA DEL PERFIL (Coneixement Expert)
;;; ==============================================================

(defrule INFERENCIA::abstraccio-edat
    (object (is-a USUARI) (name [usuari_actual]) (edat ?e) (rang_edat DESCONEGUT))
    =>
    (if (< ?e 18) then (send [usuari_actual] put-rang_edat MENOR)
    else (if (<= ?e 30) then (send [usuari_actual] put-rang_edat JOVE)
    else (if (<= ?e 55) then (send [usuari_actual] put-rang_edat MITJANA-EDAT)
    else (send [usuari_actual] put-rang_edat GRAN))))
)

(defrule INFERENCIA::abstraccio-actitud
    (object (is-a USUARI) (name [usuari_actual]) (sacrifica_qualitat ?sac) (actitud_despesa DESCONEGUT))
    =>
    (if (eq ?sac TRUE) then (send [usuari_actual] put-actitud_despesa ESTALVIADOR)
    else (send [usuari_actual] put-actitud_despesa EXIGENT))
)

(defrule INFERENCIA::abstraccio-pressupost
    (object (is-a USUARI) (name [usuari_actual]) 
            (pressupost_max ?pres) (dies_max ?dies) (persones_viatjant ?pers)
            (nivell_pressupost DESCONEGUT))
    (test (> ?dies 0))
    =>
    ;; Calculem la despesa màxima per persona i dia per saber el nivell econòmic real
    (bind ?pres_diari (/ ?pres (* ?dies ?pers)))
    (if (< ?pres_diari 60) then (send [usuari_actual] put-nivell_pressupost BAIX)
    else (if (<= ?pres_diari 150) then (send [usuari_actual] put-nivell_pressupost NORMAL)
    else (send [usuari_actual] put-nivell_pressupost ALT)))
)

;; Regla 1: Esdeveniments especials tenen prioritat absoluta
(defrule INFERENCIA::heuristica-esdeveniment-romantic
    (object (is-a USUARI) (esdev_especial BODA | ANIVERSARI))
    =>
    (assert (diagnostic-objectiu ROMANTIC))
    (assert (diagnostic-allotjament LUXE))
)

(defrule INFERENCIA::heuristica-esdeveniment-fidecurs
    (object (is-a USUARI) (esdev_especial FI_DE_CURS))
    =>
    (assert (diagnostic-objectiu DIVERSIO))
    (assert (diagnostic-allotjament ECONOMIC))
)
(defrule INFERENCIA::heuristica-esdeveniment-jubilacio
    (object (is-a USUARI) (esdev_especial JUBILACIO))
    =>
    (assert (diagnostic-objectiu CULTURAL))
    (assert (diagnostic-allotjament ESTANDARD)) ;; Basat en la recomanació de l'expert: Cultural i Confort
)

;; Regla 2: Famílies
(defrule INFERENCIA::heuristica-familia
    (object (is-a USUARI) (esdev_especial CAP) (perfil_deduit FAMILIA))
    =>
    (assert (diagnostic-objectiu DIVERSIO))
    (assert (diagnostic-allotjament APARTAMENT_FAMILIAR))
)

;; Regla 3: Joves i aventurers
(defrule INFERENCIA::heuristica-joves-aventura
    (object (is-a USUARI) (esdev_especial CAP) (perfil_deduit GRUP | INDIVIDUAL) 
            (rang_edat JOVE) (actitud_despesa ESTALVIADOR))
    =>
    (assert (diagnostic-objectiu AVENTURA))
    (assert (diagnostic-allotjament ECONOMIC))
)

;; Regla 4: Perfil Cultural i Exigent
(defrule INFERENCIA::heuristica-cultural-exigent
    (object (is-a USUARI) (esdev_especial CAP) (nivell_cultural ALT) 
            (actitud_despesa EXIGENT) (perfil_deduit PARELLA | INDIVIDUAL))
    =>
    (assert (diagnostic-objectiu CULTURAL))
    (assert (diagnostic-allotjament ESTANDARD))
)

;; Regla 5: Fallback (Si no encaixa en cap perfil anterior, receptem descans estàndard)
(defrule INFERENCIA::heuristica-per-defecte
    (declare (salience -5)) ;; Salience negatiu assegura que es dispara només si les altres no ho han fet
    (not (diagnostic-objectiu ?))
    (not (VIATGE))
    =>
    (assert (diagnostic-objectiu DESCANS))
    (assert (diagnostic-allotjament ESTANDARD))
)

(defrule INFERENCIA::crea-solucio-abstracta
    (declare (salience -10)) ;; S'executa al final de tota la inferència
    ?obj <- (diagnostic-objectiu ?o)
    ?allotj <- (diagnostic-allotjament ?a)
    =>
    (printout t crlf ">> [DIAGNOSTIC EXPERT] Objectiu viatge: " ?o " | Allotjament ideal: " ?a crlf)
    
    ;; Creem el motlle buit de la ruta amb el diagnòstic incrustat
    (assert (VIATGE (objectiu ?o) (nivell_allotjament ?a)))
    
    ;; Netegem els fets temporals de la memòria
    (retract ?obj ?allotj)
)

;;; ==============================================================
;;; MÒDUL 3: RECOMANACIÓ I FILTRATGE
;;; ==============================================================

;;; --- Càrrega de dades ---
(defrule RECOMANACIO::carrega-dades
    (declare (salience 50))
    =>    
    (make-instance [c1] of CIUTAT (nom "Paris")    (tematica METROPOLI HISTORICA)    (tipus_viatge ROMANTIC CULTURAL))
    (make-instance [c2] of CIUTAT (nom "Roma")     (tematica HISTORICA MEDITERRANIA) (tipus_viatge CULTURAL ROMANTIC))
    (make-instance [c3] of CIUTAT (nom "Venecia")  (tematica MEDITERRANIA) (tipus_viatge ROMANTIC CULTURAL))
    (make-instance [c4] of CIUTAT (nom "Orlando")  (tematica METROPOLI)    (tipus_viatge DIVERSIO))
    (make-instance [c5] of CIUTAT (nom "Tenerife") (tematica COSTA_PLATJA) (tipus_viatge DIVERSIO AVENTURA))
    (make-instance [c6] of CIUTAT (nom "Maldives") (tematica EXOTICA)      (tipus_viatge ROMANTIC))
    (make-instance [c7] of CIUTAT (nom "Londres")  (tematica METROPOLI)    (tipus_viatge CULTURAL))
    (make-instance [c8] of CIUTAT (nom "Berlin")   (tematica HISTORICA)    (tipus_viatge CULTURAL))
    (make-instance [c9] of CIUTAT (nom "Bali")     (tematica EXOTICA)      (tipus_viatge DIVERSIO AVENTURA))
    (make-instance [c10] of CIUTAT (nom "Barcelona") (tematica MEDITERRANIA) (tipus_viatge CULTURAL DIVERSIO))
    (make-instance [c11] of CIUTAT (nom "Zermatt") (tematica MUNTANYA) (tipus_viatge AVENTURA DESCANS))
    (make-instance [c12] of CIUTAT (nom "Kyoto") (tematica HISTORICA) (tipus_viatge CULTURAL ROMANTIC))
    (make-instance [c13] of CIUTAT (nom "Cancun") (tematica COSTA_PLATJA) (tipus_viatge DIVERSIO DESCANS))
    (make-instance [c14] of CIUTAT (nom "Nova York") (tematica METROPOLI) (tipus_viatge CULTURAL DIVERSIO))
    (make-instance [c15] of CIUTAT (nom "Santorini") (tematica MEDITERRANIA COSTA_PLATJA) (tipus_viatge ROMANTIC DESCANS))
    (make-instance [c16] of CIUTAT (nom "Dubai") (tematica METROPOLI EXOTICA) (tipus_viatge DIVERSIO))
    (make-instance [c17] of CIUTAT (nom "Chamonix") (tematica MUNTANYA) (tipus_viatge AVENTURA DESCANS))
    (make-instance [c18] of CIUTAT (nom "Atenes") (tematica HISTORICA MEDITERRANIA) (tipus_viatge CULTURAL))
    (make-instance [c19] of CIUTAT (nom "Phuket") (tematica EXOTICA COSTA_PLATJA) (tipus_viatge DESCANS AVENTURA))
    (make-instance [c20] of CIUTAT (nom "Praga") (tematica HISTORICA) (tipus_viatge ROMANTIC CULTURAL))
    (make-instance [c21] of CIUTAT (nom "Amsterdam") (tematica METROPOLI) (tipus_viatge CULTURAL DIVERSIO))
    (make-instance [c22] of CIUTAT (nom "Aspen") (tematica MUNTANYA) (tipus_viatge AVENTURA DIVERSIO))
    (make-instance [c23] of CIUTAT (nom "Marrakech") (tematica HISTORICA EXOTICA) (tipus_viatge CULTURAL AVENTURA))
    (make-instance [c24] of CIUTAT (nom "Eivissa") (tematica MEDITERRANIA COSTA_PLATJA) (tipus_viatge DIVERSIO))
    (make-instance [c25] of CIUTAT (nom "Viena") (tematica HISTORICA) (tipus_viatge CULTURAL ROMANTIC))
    (make-instance [c26] of CIUTAT (nom "Reykjavik") (tematica EXOTICA MUNTANYA) (tipus_viatge AVENTURA))
    (make-instance [c27] of CIUTAT (nom "Sydney") (tematica METROPOLI COSTA_PLATJA) (tipus_viatge CULTURAL DIVERSIO))
    (make-instance [c28] of CIUTAT (nom "Florencia") (tematica HISTORICA) (tipus_viatge CULTURAL ROMANTIC))
    (make-instance [c29] of CIUTAT (nom "Bora Bora") (tematica EXOTICA COSTA_PLATJA) (tipus_viatge ROMANTIC DESCANS))
    (make-instance [c30] of CIUTAT (nom "Interlaken") (tematica MUNTANYA) (tipus_viatge AVENTURA DESCANS))

    (make-instance [h1] of ALLOTJAMENT (nom "Hotel Paris Luxe") (ciutat [c1]) (tipus HOTEL) (estrelles 4) (preu_nit 120))
    (make-instance [h2] of ALLOTJAMENT (nom "Alberg Montmartre") (ciutat [c1]) (tipus ALBERG) (estrelles 2) (preu_nit 65))
    (make-instance [h3] of ALLOTJAMENT (nom "Resort Champs-Élysées") (ciutat [c1]) (tipus RESORT) (estrelles 5) (preu_nit 200))
    (make-instance [h4] of ALLOTJAMENT (nom "Hotel Roma Antic") (ciutat [c2]) (tipus HOTEL) (estrelles 3) (preu_nit 80))
    (make-instance [h5] of ALLOTJAMENT (nom "Apartament Colosseu") (ciutat [c2]) (tipus APARTAMENT) (estrelles 4) (preu_nit 110))
    (make-instance [h6] of ALLOTJAMENT (nom "Hotel Venecia") (ciutat [c3]) (tipus HOTEL) (estrelles 3) (preu_nit 95))
    (make-instance [h7] of ALLOTJAMENT (nom "Resort Gran Canal") (ciutat [c3]) (tipus RESORT) (estrelles 5) (preu_nit 210))
    (make-instance [h8] of ALLOTJAMENT (nom "Hotel Orlando") (ciutat [c4]) (tipus HOTEL) (estrelles 3) (preu_nit 100))
    (make-instance [h9] of ALLOTJAMENT (nom "Resort Disney") (ciutat [c4]) (tipus RESORT) (estrelles 4) (preu_nit 140))
    (make-instance [h10] of ALLOTJAMENT (nom "Apartament Teide") (ciutat [c5]) (tipus APARTAMENT) (estrelles 2) (preu_nit 45))
    (make-instance [h11] of ALLOTJAMENT (nom "Hotel Tenerife") (ciutat [c5]) (tipus HOTEL) (estrelles 4) (preu_nit 85))
    (make-instance [h12] of ALLOTJAMENT (nom "Resort Maldives Luxe") (ciutat [c6]) (tipus RESORT) (estrelles 5) (preu_nit 250))
    (make-instance [h13] of ALLOTJAMENT (nom "Hotel Maldives") (ciutat [c6]) (tipus HOTEL) (estrelles 3) (preu_nit 130))
    (make-instance [h14] of ALLOTJAMENT (nom "Hotel Londres") (ciutat [c7]) (tipus HOTEL) (estrelles 3) (preu_nit 105))
    (make-instance [h15] of ALLOTJAMENT (nom "Apartament Soho") (ciutat [c7]) (tipus APARTAMENT) (estrelles 4) (preu_nit 150))
    (make-instance [h16] of ALLOTJAMENT (nom "Alberg Camden") (ciutat [c7]) (tipus ALBERG) (estrelles 2) (preu_nit 70))
    (make-instance [h17] of ALLOTJAMENT (nom "Hotel Berlin") (ciutat [c8]) (tipus HOTEL) (estrelles 3) (preu_nit 70))
    (make-instance [h18] of ALLOTJAMENT (nom "Alberg Kreuzberg") (ciutat [c8]) (tipus ALBERG) (estrelles 1) (preu_nit 35))
    (make-instance [h19] of ALLOTJAMENT (nom "Resort Bali") (ciutat [c9]) (tipus RESORT) (estrelles 4) (preu_nit 65))
    (make-instance [h20] of ALLOTJAMENT (nom "Alberg Ubud") (ciutat [c9]) (tipus ALBERG) (estrelles 2) (preu_nit 30))
    (make-instance [h21] of ALLOTJAMENT (nom "Hotel Barcelona Rambles") (ciutat [c10]) (tipus HOTEL) (estrelles 4) (preu_nit 130))
    (make-instance [h22] of ALLOTJAMENT (nom "Apartament Gothic Quarter") (ciutat [c10]) (tipus APARTAMENT) (estrelles 3) (preu_nit 90))
    (make-instance [h23] of ALLOTJAMENT (nom "Apartament Zermatt Alpí") (ciutat [c11]) (tipus APARTAMENT) (estrelles 3) (preu_nit 110))
    (make-instance [h24] of ALLOTJAMENT (nom "Alberg Matterhorn") (ciutat [c11]) (tipus ALBERG) (estrelles 2) (preu_nit 50))
    (make-instance [h25] of ALLOTJAMENT (nom "Ryokan Kyoto Tradicional") (ciutat [c12]) (tipus HOTEL) (estrelles 4) (preu_nit 160))
    (make-instance [h26] of ALLOTJAMENT (nom "Hotel Kyoto Econòmic") (ciutat [c12]) (tipus HOTEL) (estrelles 2) (preu_nit 50))
    (make-instance [h27] of ALLOTJAMENT (nom "Apartament Kyoto Zen") (ciutat [c12]) (tipus APARTAMENT) (estrelles 3) (preu_nit 85))
    (make-instance [h28] of ALLOTJAMENT (nom "Resort Cancun Platja") (ciutat [c13]) (tipus RESORT) (estrelles 5) (preu_nit 220))
    (make-instance [h29] of ALLOTJAMENT (nom "Resort Cancun Familiar") (ciutat [c13]) (tipus RESORT) (estrelles 3) (preu_nit 120))
    (make-instance [h30] of ALLOTJAMENT (nom "Hotel Cancun Básico") (ciutat [c13]) (tipus HOTEL) (estrelles 2) (preu_nit 60))
    (make-instance [h31] of ALLOTJAMENT (nom "Hotel Manhattan Central") (ciutat [c14]) (tipus HOTEL) (estrelles 4) (preu_nit 180))
    (make-instance [h32] of ALLOTJAMENT (nom "Alberg Times Square") (ciutat [c14]) (tipus ALBERG) (estrelles 2) (preu_nit 90))
    (make-instance [h33] of ALLOTJAMENT (nom "Apartament Santorini Blau") (ciutat [c15]) (tipus APARTAMENT) (estrelles 3) (preu_nit 95))
    (make-instance [h34] of ALLOTJAMENT (nom "Apartament Santorini Villa") (ciutat [c15]) (tipus APARTAMENT) (estrelles 5) (preu_nit 250))
    (make-instance [h35] of ALLOTJAMENT (nom "Hotel Oia Views") (ciutat [c15]) (tipus HOTEL) (estrelles 4) (preu_nit 150))
    (make-instance [h36] of ALLOTJAMENT (nom "Burj Resort Dubai") (ciutat [c16]) (tipus RESORT) (estrelles 5) (preu_nit 300))
    (make-instance [h37] of ALLOTJAMENT (nom "Hotel Dubai City") (ciutat [c16]) (tipus HOTEL) (estrelles 3) (preu_nit 110))
    (make-instance [h38] of ALLOTJAMENT (nom "Hotel Dubai Budget") (ciutat [c16]) (tipus HOTEL) (estrelles 2) (preu_nit 70))
    (make-instance [h39] of ALLOTJAMENT (nom "Hotel Chamonix Mont Blanc") (ciutat [c17]) (tipus HOTEL) (estrelles 4) (preu_nit 140))
    (make-instance [h40] of ALLOTJAMENT (nom "Camping Chamonix Sud") (ciutat [c17]) (tipus CAMPING) (estrelles 3) (preu_nit 40))
    (make-instance [h41] of ALLOTJAMENT (nom "Hotel Atenes Històric") (ciutat [c18]) (tipus HOTEL) (estrelles 3) (preu_nit 75))
    (make-instance [h42] of ALLOTJAMENT (nom "Hotel Acròpolis Luxury") (ciutat [c18]) (tipus HOTEL) (estrelles 5) (preu_nit 200))
    (make-instance [h43] of ALLOTJAMENT (nom "Alberg Plaka") (ciutat [c18]) (tipus ALBERG) (estrelles 2) (preu_nit 35))
    (make-instance [h44] of ALLOTJAMENT (nom "Resort Phuket Tropical") (ciutat [c19]) (tipus RESORT) (estrelles 5) (preu_nit 190))
    (make-instance [h45] of ALLOTJAMENT (nom "Resort Phuket Eco") (ciutat [c19]) (tipus RESORT) (estrelles 3) (preu_nit 90))
    (make-instance [h46] of ALLOTJAMENT (nom "Apartament Phuket Sea") (ciutat [c19]) (tipus APARTAMENT) (estrelles 3) (preu_nit 80))
    (make-instance [h47] of ALLOTJAMENT (nom "Hotel Praga Antic") (ciutat [c20]) (tipus HOTEL) (estrelles 3) (preu_nit 65))
    (make-instance [h48] of ALLOTJAMENT (nom "Apartament Vltava River") (ciutat [c20]) (tipus APARTAMENT) (estrelles 4) (preu_nit 90))
    (make-instance [h49] of ALLOTJAMENT (nom "Apartament Amsterdam Canal") (ciutat [c21]) (tipus APARTAMENT) (estrelles 4) (preu_nit 140))
    (make-instance [h50] of ALLOTJAMENT (nom "Apartament Amsterdam Studio") (ciutat [c21]) (tipus APARTAMENT) (estrelles 2) (preu_nit 75))
    (make-instance [h51] of ALLOTJAMENT (nom "Alberg Amsterdam Center") (ciutat [c21]) (tipus ALBERG) (estrelles 2) (preu_nit 50))
    (make-instance [h52] of ALLOTJAMENT (nom "Resort Aspen Neu") (ciutat [c22]) (tipus RESORT) (estrelles 5) (preu_nit 250))
    (make-instance [h53] of ALLOTJAMENT (nom "Resort Aspen Ski Club") (ciutat [c22]) (tipus RESORT) (estrelles 4) (preu_nit 190))
    (make-instance [h54] of ALLOTJAMENT (nom "Hotel Aspen Inn") (ciutat [c22]) (tipus HOTEL) (estrelles 3) (preu_nit 120))
    (make-instance [h55] of ALLOTJAMENT (nom "Riad Marrakech Oasis") (ciutat [c23]) (tipus HOTEL) (estrelles 4) (preu_nit 90))
    (make-instance [h56] of ALLOTJAMENT (nom "Riad Marrakech Palace") (ciutat [c23]) (tipus HOTEL) (estrelles 5) (preu_nit 150))
    (make-instance [h57] of ALLOTJAMENT (nom "Apartament Medina") (ciutat [c23]) (tipus APARTAMENT) (estrelles 3) (preu_nit 60))
    (make-instance [h58] of ALLOTJAMENT (nom "Alberg Eivissa Party") (ciutat [c24]) (tipus ALBERG) (estrelles 2) (preu_nit 55))
    (make-instance [h59] of ALLOTJAMENT (nom "Alberg Eivissa Chill") (ciutat [c24]) (tipus ALBERG) (estrelles 1) (preu_nit 30))
    (make-instance [h60] of ALLOTJAMENT (nom "Resort Eivissa Platja") (ciutat [c24]) (tipus RESORT) (estrelles 4) (preu_nit 180))
    (make-instance [h61] of ALLOTJAMENT (nom "Hotel Viena Imperial") (ciutat [c25]) (tipus HOTEL) (estrelles 5) (preu_nit 170))
    (make-instance [h62] of ALLOTJAMENT (nom "Hotel Viena Centro") (ciutat [c25]) (tipus HOTEL) (estrelles 3) (preu_nit 90))
    (make-instance [h63] of ALLOTJAMENT (nom "Apartament Viena Clasica") (ciutat [c25]) (tipus APARTAMENT) (estrelles 4) (preu_nit 120))
    (make-instance [h64] of ALLOTJAMENT (nom "Hotel Reykjavik Aurora") (ciutat [c26]) (tipus HOTEL) (estrelles 4) (preu_nit 150))
    (make-instance [h65] of ALLOTJAMENT (nom "Hotel Reykjavik Budget") (ciutat [c26]) (tipus HOTEL) (estrelles 2) (preu_nit 80))
    (make-instance [h66] of ALLOTJAMENT (nom "Camping Reykjavik Nord") (ciutat [c26]) (tipus CAMPING) (estrelles 2) (preu_nit 40))
    (make-instance [h67] of ALLOTJAMENT (nom "Apartament Sydney Harbour") (ciutat [c27]) (tipus APARTAMENT) (estrelles 4) (preu_nit 130))
    (make-instance [h68] of ALLOTJAMENT (nom "Apartament Sydney Loft") (ciutat [c27]) (tipus APARTAMENT) (estrelles 3) (preu_nit 90))
    (make-instance [h69] of ALLOTJAMENT (nom "Hotel Sydney Central") (ciutat [c27]) (tipus HOTEL) (estrelles 3) (preu_nit 110))
    (make-instance [h70] of ALLOTJAMENT (nom "Hotel Florència Renacimiento") (ciutat [c28]) (tipus HOTEL) (estrelles 4) (preu_nit 140))
    (make-instance [h71] of ALLOTJAMENT (nom "Hotel Florència Express") (ciutat [c28]) (tipus HOTEL) (estrelles 2) (preu_nit 60))
    (make-instance [h72] of ALLOTJAMENT (nom "Apartament Florència Art") (ciutat [c28]) (tipus APARTAMENT) (estrelles 3) (preu_nit 100))
    (make-instance [h73] of ALLOTJAMENT (nom "Resort Bora Bora Paradís") (ciutat [c29]) (tipus RESORT) (estrelles 5) (preu_nit 350))
    (make-instance [h74] of ALLOTJAMENT (nom "Resort Bora Bora Beach") (ciutat [c29]) (tipus RESORT) (estrelles 4) (preu_nit 220))
    (make-instance [h75] of ALLOTJAMENT (nom "Hotel Bora Bora Bungalow") (ciutat [c29]) (tipus HOTEL) (estrelles 4) (preu_nit 200))
    (make-instance [h76] of ALLOTJAMENT (nom "Camping Interlaken Llac") (ciutat [c30]) (tipus CAMPING) (estrelles 3) (preu_nit 45))
    (make-instance [h77] of ALLOTJAMENT (nom "Camping Interlaken Wild") (ciutat [c30]) (tipus CAMPING) (estrelles 1) (preu_nit 25))
    (make-instance [h78] of ALLOTJAMENT (nom "Hotel Interlaken Natura") (ciutat [c30]) (tipus HOTEL) (estrelles 4) (preu_nit 130))

    ;; París (c1)
    (make-instance [a1]  of ATRACCIO (nom "Torre Eiffel")       (ciutat [c1]) (duracio 1.0) (preu_atraccio 25) (coneguda TRUE)  (tematica ROMANTIC))
    (make-instance [a2]  of ATRACCIO (nom "Museu del Louvre")   (ciutat [c1]) (duracio 1.0) (preu_atraccio 17) (coneguda TRUE)  (tematica CULTURAL))
    (make-instance [a3]  of ATRACCIO (nom "Notre-Dame")         (ciutat [c1]) (duracio 0.5) (preu_atraccio 0)  (coneguda TRUE)  (tematica CULTURAL))
    (make-instance [a27] of ATRACCIO (nom "Les Catacumbes")     (ciutat [c1]) (duracio 0.5) (preu_atraccio 29) (coneguda FALSE) (tematica CULTURAL))
    (make-instance [a28] of ATRACCIO (nom "Sainte-Chapelle")    (ciutat [c1]) (duracio 0.5) (preu_atraccio 15) (coneguda FALSE) (tematica CULTURAL))
    (make-instance [a29] of ATRACCIO (nom "Canal Saint-Martin") (ciutat [c1]) (duracio 0.5) (preu_atraccio 0)  (coneguda FALSE) (tematica ROMANTIC))

    ;; Roma (c2)
    (make-instance [a4]  of ATRACCIO (nom "Coliseu")            (ciutat [c2]) (duracio 1.0) (preu_atraccio 16) (coneguda TRUE)  (tematica CULTURAL))
    (make-instance [a5]  of ATRACCIO (nom "Fontana di Trevi")   (ciutat [c2]) (duracio 0.5) (preu_atraccio 0)  (coneguda TRUE)  (tematica ROMANTIC))
    (make-instance [a6]  of ATRACCIO (nom "Vaticà")             (ciutat [c2]) (duracio 2.0) (preu_atraccio 17) (coneguda TRUE)  (tematica CULTURAL))
    (make-instance [a30] of ATRACCIO (nom "Termes de Caracal·la")(ciutat [c2]) (duracio 0.5) (preu_atraccio 10) (coneguda FALSE) (tematica CULTURAL))
    (make-instance [a31] of ATRACCIO (nom "Via Appia Antica")   (ciutat [c2]) (duracio 1.0) (preu_atraccio 0)  (coneguda FALSE) (tematica CULTURAL))
    (make-instance [a32] of ATRACCIO (nom "Barri de Trastevere")(ciutat [c2]) (duracio 0.5) (preu_atraccio 0)  (coneguda FALSE) (tematica ROMANTIC))

    ;; Venècia (c3)
    (make-instance [a7]  of ATRACCIO (nom "Canal Gran")         (ciutat [c3]) (duracio 0.5) (preu_atraccio 0)  (coneguda TRUE)  (tematica ROMANTIC))
    (make-instance [a8]  of ATRACCIO (nom "Plaça de Sant Marc") (ciutat [c3]) (duracio 0.5) (preu_atraccio 0)  (coneguda TRUE)  (tematica CULTURAL))
    (make-instance [a9]  of ATRACCIO (nom "Pont de Rialto")     (ciutat [c3]) (duracio 0.5) (preu_atraccio 0)  (coneguda TRUE)  (tematica ROMANTIC))
    (make-instance [a33] of ATRACCIO (nom "Illa de Burano")     (ciutat [c3]) (duracio 1.0) (preu_atraccio 10) (coneguda FALSE) (tematica ROMANTIC))
    (make-instance [a34] of ATRACCIO (nom "Llibreria Acqua Alta")(ciutat [c3]) (duracio 0.5) (preu_atraccio 0)  (coneguda FALSE) (tematica CULTURAL))
    (make-instance [a35] of ATRACCIO (nom "Ghetto Jueu")        (ciutat [c3]) (duracio 0.5) (preu_atraccio 0)  (coneguda FALSE) (tematica CULTURAL))

    ;; Orlando (c4)
    (make-instance [a10] of ATRACCIO (nom "Walt Disney World")  (ciutat [c4]) (duracio 2.0) (preu_atraccio 120) (coneguda TRUE)  (tematica DIVERSIO))
    (make-instance [a11] of ATRACCIO (nom "Universal Studios")  (ciutat [c4]) (duracio 1.0) (preu_atraccio 115) (coneguda TRUE)  (tematica DIVERSIO))
    (make-instance [a36] of ATRACCIO (nom "Gatorland")          (ciutat [c4]) (duracio 0.5) (preu_atraccio 35)  (coneguda FALSE) (tematica DIVERSIO))
    (make-instance [a37] of ATRACCIO (nom "Harry P. Leu Gardens")(ciutat [c4]) (duracio 0.5) (preu_atraccio 15)  (coneguda FALSE) (tematica ROMANTIC))
    (make-instance [a38] of ATRACCIO (nom "Wekiwa Springs")     (ciutat [c4]) (duracio 1.0) (preu_atraccio 6)   (coneguda FALSE) (tematica DIVERSIO))

    ;; Tenerife (c5)
    (make-instance [a12] of ATRACCIO (nom "Teide")              (ciutat [c5]) (duracio 1.0) (preu_atraccio 25) (coneguda TRUE)  (tematica DIVERSIO))
    (make-instance [a13] of ATRACCIO (nom "Loro Parque")        (ciutat [c5]) (duracio 1.0) (preu_atraccio 40) (coneguda TRUE)  (tematica DIVERSIO))
    (make-instance [a14] of ATRACCIO (nom "Siam Park")          (ciutat [c5]) (duracio 1.0) (preu_atraccio 40) (coneguda TRUE)  (tematica DIVERSIO))
    (make-instance [a39] of ATRACCIO (nom "Anaga Rural Park")   (ciutat [c5]) (duracio 1.0) (preu_atraccio 0)  (coneguda FALSE) (tematica CULTURAL))
    (make-instance [a40] of ATRACCIO (nom "Cueva del Viento")   (ciutat [c5]) (duracio 0.5) (preu_atraccio 20) (coneguda FALSE) (tematica DIVERSIO))
    (make-instance [a41] of ATRACCIO (nom "Acantilados de Los Gigantes") (ciutat [c5]) (duracio 0.5) (preu_atraccio 0) (coneguda FALSE) (tematica ROMANTIC))

    ;; Maldives (c6)
    (make-instance [a15] of ATRACCIO (nom "Atòl de Malé")       (ciutat [c6]) (duracio 0.5) (preu_atraccio 0)  (coneguda TRUE)  (tematica DIVERSIO))
    (make-instance [a16] of ATRACCIO (nom "Esculls de corall")  (ciutat [c6]) (duracio 0.5) (preu_atraccio 0)  (coneguda TRUE)  (tematica DIVERSIO))
    (make-instance [a17] of ATRACCIO (nom "Platges de sorra blanca") (ciutat [c6]) (duracio 1.0) (preu_atraccio 0) (coneguda TRUE)  (tematica ROMANTIC))
    (make-instance [a42] of ATRACCIO (nom "Illa Vaadhoo (Mar d'estrelles)") (ciutat [c6]) (duracio 0.5) (preu_atraccio 0) (coneguda FALSE) (tematica ROMANTIC))
    (make-instance [a43] of ATRACCIO (nom "Illa local de Maafushi") (ciutat [c6]) (duracio 1.0) (preu_atraccio 0) (coneguda FALSE) (tematica CULTURAL))
    (make-instance [a44] of ATRACCIO (nom "Mesquita de Malé")   (ciutat [c6]) (duracio 0.5) (preu_atraccio 0)  (coneguda FALSE) (tematica CULTURAL))

    ;; Londres (c7)
    (make-instance [a18] of ATRACCIO (nom "Big Ben")            (ciutat [c7]) (duracio 0.5) (preu_atraccio 0)  (coneguda TRUE)  (tematica CULTURAL))
    (make-instance [a19] of ATRACCIO (nom "London Eye")         (ciutat [c7]) (duracio 0.5) (preu_atraccio 30) (coneguda TRUE)  (tematica DIVERSIO))
    (make-instance [a20] of ATRACCIO (nom "British Museum")     (ciutat [c7]) (duracio 2.0) (preu_atraccio 0)  (coneguda TRUE)  (tematica CULTURAL))
    (make-instance [a45] of ATRACCIO (nom "Sky Garden")         (ciutat [c7]) (duracio 0.5) (preu_atraccio 0)  (coneguda FALSE) (tematica ROMANTIC))
    (make-instance [a46] of ATRACCIO (nom "Camden Market")      (ciutat [c7]) (duracio 1.0) (preu_atraccio 0)  (coneguda FALSE) (tematica DIVERSIO))
    (make-instance [a47] of ATRACCIO (nom "Leadenhall Market")  (ciutat [c7]) (duracio 0.5) (preu_atraccio 0)  (coneguda FALSE) (tematica CULTURAL))

    ;; Berlín (c8)
    (make-instance [a21] of ATRACCIO (nom "Porta de Brandenburg") (ciutat [c8]) (duracio 0.5) (preu_atraccio 0)  (coneguda TRUE)  (tematica CULTURAL))
    (make-instance [a22] of ATRACCIO (nom "Mur de Berlín")      (ciutat [c8]) (duracio 1.0) (preu_atraccio 0)  (coneguda TRUE)  (tematica CULTURAL))
    (make-instance [a23] of ATRACCIO (nom "Illa dels Museus")   (ciutat [c8]) (duracio 2.0) (preu_atraccio 18) (coneguda TRUE)  (tematica CULTURAL))
    (make-instance [a48] of ATRACCIO (nom "Búnquer de la Guerra Freda") (ciutat [c8]) (duracio 0.5) (preu_atraccio 12) (coneguda FALSE) (tematica CULTURAL))
    (make-instance [a49] of ATRACCIO (nom "Teufelsberg")        (ciutat [c8]) (duracio 1.0) (preu_atraccio 8)  (coneguda FALSE) (tematica DIVERSIO))
    (make-instance [a50] of ATRACCIO (nom "Barri de Nikolaiviertel") (ciutat [c8]) (duracio 0.5) (preu_atraccio 0) (coneguda FALSE) (tematica ROMANTIC))

    ;; Bali (c9)
    (make-instance [a24] of ATRACCIO (nom "Temple d'Uluwatu")   (ciutat [c9]) (duracio 1.0) (preu_atraccio 3)  (coneguda TRUE)  (tematica CULTURAL))
    (make-instance [a25] of ATRACCIO (nom "Camps d'arròs d'Ubud") (ciutat [c9]) (duracio 0.5) (preu_atraccio 0)  (coneguda TRUE)  (tematica ROMANTIC))
    (make-instance [a26] of ATRACCIO (nom "Tanah Lot")          (ciutat [c9]) (duracio 0.5) (preu_atraccio 3)  (coneguda TRUE)  (tematica CULTURAL))
    (make-instance [a51] of ATRACCIO (nom "Cascada Sekumpul")   (ciutat [c9]) (duracio 1.0) (preu_atraccio 2)  (coneguda FALSE) (tematica DIVERSIO))
    (make-instance [a52] of ATRACCIO (nom "Illa de Nusa Penida")(ciutat [c9]) (duracio 2.0) (preu_atraccio 15) (coneguda FALSE) (tematica DIVERSIO))
    (make-instance [a53] of ATRACCIO (nom "Sidemen Valley")     (ciutat [c9]) (duracio 1.0) (preu_atraccio 0)  (coneguda FALSE) (tematica ROMANTIC))

    ;; Barcelona (c10)
    (make-instance [a54] of ATRACCIO (nom "Sagrada Família") (ciutat [c10]) (duracio 1.0) (preu_atraccio 26) (coneguda TRUE) (tematica CULTURAL))
    (make-instance [a55] of ATRACCIO (nom "Park Güell") (ciutat [c10]) (duracio 0.5) (preu_atraccio 10) (coneguda TRUE) (tematica CULTURAL))
    (make-instance [a56] of ATRACCIO (nom "Barri Gòtic") (ciutat [c10]) (duracio 0.5) (preu_atraccio 0) (coneguda TRUE) (tematica CULTURAL))
    (make-instance [a57] of ATRACCIO (nom "Búnquers del Carmel") (ciutat [c10]) (duracio 0.5) (preu_atraccio 0) (coneguda FALSE) (tematica ROMANTIC))
    (make-instance [a58] of ATRACCIO (nom "Museu Picasso") (ciutat [c10]) (duracio 1.0) (preu_atraccio 12) (coneguda FALSE) (tematica CULTURAL))
    (make-instance [a59] of ATRACCIO (nom "Font Màgica") (ciutat [c10]) (duracio 0.5) (preu_atraccio 0) (coneguda FALSE) (tematica DIVERSIO))

    ;; Zermatt (c11)
    (make-instance [a60] of ATRACCIO (nom "Matterhorn Glacier Paradise") (ciutat [c11]) (duracio 1.0) (preu_atraccio 90) (coneguda TRUE) (tematica DIVERSIO))
    (make-instance [a61] of ATRACCIO (nom "Gornergrat") (ciutat [c11]) (duracio 1.0) (preu_atraccio 80) (coneguda TRUE) (tematica CULTURAL))
    (make-instance [a62] of ATRACCIO (nom "Poble de Zermatt") (ciutat [c11]) (duracio 0.5) (preu_atraccio 0) (coneguda TRUE) (tematica CULTURAL))
    (make-instance [a63] of ATRACCIO (nom "Museu Matterhorn") (ciutat [c11]) (duracio 0.5) (preu_atraccio 10) (coneguda FALSE) (tematica CULTURAL))
    (make-instance [a64] of ATRACCIO (nom "Caminada dels 5 Llacs") (ciutat [c11]) (duracio 1.0) (preu_atraccio 0) (coneguda FALSE) (tematica ROMANTIC))

    ;; Kyoto (c12)
    (make-instance [a65] of ATRACCIO (nom "Fushimi Inari") (ciutat [c12]) (duracio 1.0) (preu_atraccio 0) (coneguda TRUE) (tematica CULTURAL))
    (make-instance [a66] of ATRACCIO (nom "Kinkaku-ji (Pavelló Daurat)") (ciutat [c12]) (duracio 0.5) (preu_atraccio 5) (coneguda TRUE) (tematica CULTURAL))
    (make-instance [a67] of ATRACCIO (nom "Bosc de Bambú d'Arashiyama") (ciutat [c12]) (duracio 0.5) (preu_atraccio 0) (coneguda TRUE) (tematica ROMANTIC))
    (make-instance [a68] of ATRACCIO (nom "Gion (Barri de les Geishes)") (ciutat [c12]) (duracio 0.5) (preu_atraccio 0) (coneguda FALSE) (tematica CULTURAL))
    (make-instance [a69] of ATRACCIO (nom "Temple Kiyomizu-dera") (ciutat [c12]) (duracio 1.0) (preu_atraccio 4) (coneguda FALSE) (tematica CULTURAL))
    (make-instance [a70] of ATRACCIO (nom "Mercat de Nishiki") (ciutat [c12]) (duracio 0.5) (preu_atraccio 0) (coneguda FALSE) (tematica DIVERSIO))

    ;; Cancun (c13)
    (make-instance [a71] of ATRACCIO (nom "Chichén Itzá") (ciutat [c13]) (duracio 1.0) (preu_atraccio 30) (coneguda TRUE) (tematica CULTURAL))
    (make-instance [a72] of ATRACCIO (nom "Platja Delfines") (ciutat [c13]) (duracio 0.5) (preu_atraccio 0) (coneguda TRUE) (tematica DIVERSIO))
    (make-instance [a73] of ATRACCIO (nom "Xcaret Park") (ciutat [c13]) (duracio 1.0) (preu_atraccio 100) (coneguda TRUE) (tematica DIVERSIO))
    (make-instance [a74] of ATRACCIO (nom "Cenote Dos Ojos") (ciutat [c13]) (duracio 0.5) (preu_atraccio 20) (coneguda FALSE) (tematica DIVERSIO))
    (make-instance [a75] of ATRACCIO (nom "Isla Mujeres") (ciutat [c13]) (duracio 1.0) (preu_atraccio 15) (coneguda FALSE) (tematica ROMANTIC))

    ;; Nova York (c14)
    (make-instance [a76] of ATRACCIO (nom "Central Park") (ciutat [c14]) (duracio 1.0) (preu_atraccio 0) (coneguda TRUE) (tematica ROMANTIC))
    (make-instance [a77] of ATRACCIO (nom "Empire State Building") (ciutat [c14]) (duracio 0.5) (preu_atraccio 45) (coneguda TRUE) (tematica CULTURAL))
    (make-instance [a78] of ATRACCIO (nom "Estàtua de la Llibertat") (ciutat [c14]) (duracio 1.0) (preu_atraccio 25) (coneguda TRUE) (tematica CULTURAL))
    (make-instance [a79] of ATRACCIO (nom "MoMA") (ciutat [c14]) (duracio 1.0) (preu_atraccio 25) (coneguda FALSE) (tematica CULTURAL))
    (make-instance [a80] of ATRACCIO (nom "High Line Park") (ciutat [c14]) (duracio 0.5) (preu_atraccio 0) (coneguda FALSE) (tematica ROMANTIC))
    (make-instance [a81] of ATRACCIO (nom "Brooklyn Bridge") (ciutat [c14]) (duracio 0.5) (preu_atraccio 0) (coneguda FALSE) (tematica ROMANTIC))

    ;; Santorini (c15)
    (make-instance [a82] of ATRACCIO (nom "Oia (Capvespre)") (ciutat [c15]) (duracio 0.5) (preu_atraccio 0) (coneguda TRUE) (tematica ROMANTIC))
    (make-instance [a83] of ATRACCIO (nom "Fira (Capital)") (ciutat [c15]) (duracio 0.5) (preu_atraccio 0) (coneguda TRUE) (tematica CULTURAL))
    (make-instance [a84] of ATRACCIO (nom "Platja Vermella") (ciutat [c15]) (duracio 0.5) (preu_atraccio 0) (coneguda TRUE) (tematica DIVERSIO))
    (make-instance [a85] of ATRACCIO (nom "Jaciment d'Akrotiri") (ciutat [c15]) (duracio 1.0) (preu_atraccio 12) (coneguda FALSE) (tematica CULTURAL))
    (make-instance [a86] of ATRACCIO (nom "Tour del Volcà") (ciutat [c15]) (duracio 1.0) (preu_atraccio 25) (coneguda FALSE) (tematica DIVERSIO))

    ;; Dubai (c16)
    (make-instance [a87] of ATRACCIO (nom "Burj Khalifa") (ciutat [c16]) (duracio 0.5) (preu_atraccio 40) (coneguda TRUE) (tematica DIVERSIO))
    (make-instance [a88] of ATRACCIO (nom "Dubai Mall") (ciutat [c16]) (duracio 1.0) (preu_atraccio 0) (coneguda TRUE) (tematica DIVERSIO))
    (make-instance [a89] of ATRACCIO (nom "Palm Jumeirah") (ciutat [c16]) (duracio 0.5) (preu_atraccio 0) (coneguda TRUE) (tematica DIVERSIO))
    (make-instance [a90] of ATRACCIO (nom "Safari pel desert") (ciutat [c16]) (duracio 1.0) (preu_atraccio 60) (coneguda FALSE) (tematica DIVERSIO))
    (make-instance [a91] of ATRACCIO (nom "Dubai Frame") (ciutat [c16]) (duracio 0.5) (preu_atraccio 15) (coneguda FALSE) (tematica CULTURAL))
    (make-instance [a92] of ATRACCIO (nom "Barri d'Al Fahidi") (ciutat [c16]) (duracio 0.5) (preu_atraccio 0) (coneguda FALSE) (tematica CULTURAL))

    ;; Chamonix (c17)
    (make-instance [a93] of ATRACCIO (nom "Aiguille du Midi") (ciutat [c17]) (duracio 1.0) (preu_atraccio 70) (coneguda TRUE) (tematica DIVERSIO))
    (make-instance [a94] of ATRACCIO (nom "Mer de Glace") (ciutat [c17]) (duracio 1.0) (preu_atraccio 35) (coneguda TRUE) (tematica CULTURAL))
    (make-instance [a95] of ATRACCIO (nom "Lac Blanc") (ciutat [c17]) (duracio 1.0) (preu_atraccio 0) (coneguda TRUE) (tematica ROMANTIC))
    (make-instance [a96] of ATRACCIO (nom "Telecabina Flégère") (ciutat [c17]) (duracio 0.5) (preu_atraccio 20) (coneguda FALSE) (tematica DIVERSIO))
    (make-instance [a97] of ATRACCIO (nom "Museu Alpí") (ciutat [c17]) (duracio 0.5) (preu_atraccio 8) (coneguda FALSE) (tematica CULTURAL))

    ;; Atenes (c18)
    (make-instance [a98] of ATRACCIO (nom "Acròpolis") (ciutat [c18]) (duracio 1.0) (preu_atraccio 20) (coneguda TRUE) (tematica CULTURAL))
    (make-instance [a99] of ATRACCIO (nom "Àgora Antiga") (ciutat [c18]) (duracio 0.5) (preu_atraccio 8) (coneguda TRUE) (tematica CULTURAL))
    (make-instance [a100] of ATRACCIO (nom "Barri de Plaka") (ciutat [c18]) (duracio 0.5) (preu_atraccio 0) (coneguda TRUE) (tematica ROMANTIC))
    (make-instance [a101] of ATRACCIO (nom "Temple de Zeus") (ciutat [c18]) (duracio 0.5) (preu_atraccio 6) (coneguda FALSE) (tematica CULTURAL))
    (make-instance [a102] of ATRACCIO (nom "Mont Licabet") (ciutat [c18]) (duracio 0.5) (preu_atraccio 0) (coneguda FALSE) (tematica ROMANTIC))
    (make-instance [a103] of ATRACCIO (nom "Museu Arqueològic") (ciutat [c18]) (duracio 1.0) (preu_atraccio 10) (coneguda FALSE) (tematica CULTURAL))

    ;; Phuket (c19)
    (make-instance [a104] of ATRACCIO (nom "Gran Buda") (ciutat [c19]) (duracio 0.5) (preu_atraccio 0) (coneguda TRUE) (tematica CULTURAL))
    (make-instance [a105] of ATRACCIO (nom "Illa James Bond") (ciutat [c19]) (duracio 1.0) (preu_atraccio 50) (coneguda TRUE) (tematica DIVERSIO))
    (make-instance [a106] of ATRACCIO (nom "Platja Patong") (ciutat [c19]) (duracio 1.0) (preu_atraccio 0) (coneguda TRUE) (tematica DIVERSIO))
    (make-instance [a107] of ATRACCIO (nom "Temple Wat Chalong") (ciutat [c19]) (duracio 0.5) (preu_atraccio 0) (coneguda FALSE) (tematica CULTURAL))
    (make-instance [a108] of ATRACCIO (nom "Santuari dels Elefants") (ciutat [c19]) (duracio 1.0) (preu_atraccio 70) (coneguda FALSE) (tematica DIVERSIO))

    ;; Praga (c20)
    (make-instance [a109] of ATRACCIO (nom "Pont de Carles") (ciutat [c20]) (duracio 0.5) (preu_atraccio 0) (coneguda TRUE) (tematica ROMANTIC))
    (make-instance [a110] of ATRACCIO (nom "Castell de Praga") (ciutat [c20]) (duracio 1.0) (preu_atraccio 15) (coneguda TRUE) (tematica CULTURAL))
    (make-instance [a111] of ATRACCIO (nom "Rellotge Astronòmic") (ciutat [c20]) (duracio 0.5) (preu_atraccio 0) (coneguda TRUE) (tematica CULTURAL))
    (make-instance [a112] of ATRACCIO (nom "Barri Jueu") (ciutat [c20]) (duracio 0.5) (preu_atraccio 12) (coneguda FALSE) (tematica CULTURAL))
    (make-instance [a113] of ATRACCIO (nom "Mala Strana") (ciutat [c20]) (duracio 0.5) (preu_atraccio 0) (coneguda FALSE) (tematica ROMANTIC))
    (make-instance [a114] of ATRACCIO (nom "Mur de Lennon") (ciutat [c20]) (duracio 0.5) (preu_atraccio 0) (coneguda FALSE) (tematica DIVERSIO))

    ;; Amsterdam (c21)
    (make-instance [a115] of ATRACCIO (nom "Casa d'Anne Frank") (ciutat [c21]) (duracio 0.5) (preu_atraccio 14) (coneguda TRUE) (tematica CULTURAL))
    (make-instance [a116] of ATRACCIO (nom "Museu Van Gogh") (ciutat [c21]) (duracio 1.0) (preu_atraccio 20) (coneguda TRUE) (tematica CULTURAL))
    (make-instance [a117] of ATRACCIO (nom "Passeig pels canals") (ciutat [c21]) (duracio 0.5) (preu_atraccio 15) (coneguda TRUE) (tematica ROMANTIC))
    (make-instance [a118] of ATRACCIO (nom "Vondelpark") (ciutat [c21]) (duracio 0.5) (preu_atraccio 0) (coneguda FALSE) (tematica DIVERSIO))
    (make-instance [a119] of ATRACCIO (nom "Barri Jordaan") (ciutat [c21]) (duracio 0.5) (preu_atraccio 0) (coneguda FALSE) (tematica ROMANTIC))

    ;; Aspen (c22)
    (make-instance [a120] of ATRACCIO (nom "Maroon Bells") (ciutat [c22]) (duracio 0.5) (preu_atraccio 10) (coneguda TRUE) (tematica ROMANTIC))
    (make-instance [a121] of ATRACCIO (nom "Aspen Mountain Ski") (ciutat [c22]) (duracio 1.0) (preu_atraccio 150) (coneguda TRUE) (tematica DIVERSIO))
    (make-instance [a122] of ATRACCIO (nom "Passeig històric pel poble") (ciutat [c22]) (duracio 0.5) (preu_atraccio 0) (coneguda TRUE) (tematica CULTURAL))
    (make-instance [a123] of ATRACCIO (nom "Museu d'Art d'Aspen") (ciutat [c22]) (duracio 0.5) (preu_atraccio 0) (coneguda FALSE) (tematica CULTURAL))
    (make-instance [a124] of ATRACCIO (nom "Senderisme a Hunter Creek") (ciutat [c22]) (duracio 1.0) (preu_atraccio 0) (coneguda FALSE) (tematica DIVERSIO))
    (make-instance [a125] of ATRACCIO (nom "Escultura 'Aspen Art'") (ciutat [c22]) (duracio 0.5) (preu_atraccio 0) (coneguda FALSE) (tematica CULTURAL))

    ;; Marrakech (c23)
    (make-instance [a126] of ATRACCIO (nom "Plaça Jemaa el-Fna") (ciutat [c23]) (duracio 1.0) (preu_atraccio 0) (coneguda TRUE) (tematica CULTURAL))
    (make-instance [a127] of ATRACCIO (nom "Jardí Majorelle") (ciutat [c23]) (duracio 0.5) (preu_atraccio 12) (coneguda TRUE) (tematica ROMANTIC))
    (make-instance [a128] of ATRACCIO (nom "Mesquita Koutoubia") (ciutat [c23]) (duracio 0.5) (preu_atraccio 0) (coneguda TRUE) (tematica CULTURAL))
    (make-instance [a129] of ATRACCIO (nom "Palau Bahia") (ciutat [c23]) (duracio 0.5) (preu_atraccio 7) (coneguda FALSE) (tematica CULTURAL))
    (make-instance [a130] of ATRACCIO (nom "Soc de Marrakech") (ciutat [c23]) (duracio 1.0) (preu_atraccio 0) (coneguda FALSE) (tematica DIVERSIO))

    ;; Eivissa (c24)
    (make-instance [a131] of ATRACCIO (nom "Dalt Vila") (ciutat [c24]) (duracio 0.5) (preu_atraccio 0) (coneguda TRUE) (tematica CULTURAL))
    (make-instance [a132] of ATRACCIO (nom "Platja d'en Bossa") (ciutat [c24]) (duracio 1.0) (preu_atraccio 0) (coneguda TRUE) (tematica DIVERSIO))
    (make-instance [a133] of ATRACCIO (nom "Es Vedrà") (ciutat [c24]) (duracio 0.5) (preu_atraccio 0) (coneguda TRUE) (tematica ROMANTIC))
    (make-instance [a134] of ATRACCIO (nom "Cova de Can Marçà") (ciutat [c24]) (duracio 0.5) (preu_atraccio 12) (coneguda FALSE) (tematica DIVERSIO))
    (make-instance [a135] of ATRACCIO (nom "Poble de Santa Gertrudis") (ciutat [c24]) (duracio 0.5) (preu_atraccio 0) (coneguda FALSE) (tematica CULTURAL))
    (make-instance [a136] of ATRACCIO (nom "Mercat de Las Dalias") (ciutat [c24]) (duracio 0.5) (preu_atraccio 0) (coneguda FALSE) (tematica DIVERSIO))

    ;; Viena (c25)
    (make-instance [a137] of ATRACCIO (nom "Palau de Schönbrunn") (ciutat [c25]) (duracio 1.0) (preu_atraccio 22) (coneguda TRUE) (tematica CULTURAL))
    (make-instance [a138] of ATRACCIO (nom "Òpera Estatal") (ciutat [c25]) (duracio 0.5) (preu_atraccio 15) (coneguda TRUE) (tematica ROMANTIC))
    (make-instance [a139] of ATRACCIO (nom "Catedral de Sant Esteve") (ciutat [c25]) (duracio 0.5) (preu_atraccio 6) (coneguda TRUE) (tematica CULTURAL))
    (make-instance [a140] of ATRACCIO (nom "Museu Belvedere") (ciutat [c25]) (duracio 1.0) (preu_atraccio 16) (coneguda FALSE) (tematica CULTURAL))
    (make-instance [a141] of ATRACCIO (nom "Prater (Parc d'atraccions)") (ciutat [c25]) (duracio 1.0) (preu_atraccio 0) (coneguda FALSE) (tematica DIVERSIO))

    ;; Reykjavik (c26)
    (make-instance [a142] of ATRACCIO (nom "Església Hallgrímskirkja") (ciutat [c26]) (duracio 0.5) (preu_atraccio 8) (coneguda TRUE) (tematica CULTURAL))
    (make-instance [a143] of ATRACCIO (nom "Blue Lagoon") (ciutat [c26]) (duracio 1.0) (preu_atraccio 80) (coneguda TRUE) (tematica DIVERSIO))
    (make-instance [a144] of ATRACCIO (nom "Museu Perlan") (ciutat [c26]) (duracio 1.0) (preu_atraccio 25) (coneguda TRUE) (tematica CULTURAL))
    (make-instance [a145] of ATRACCIO (nom "Cercle Daurat (Tour)") (ciutat [c26]) (duracio 1.0) (preu_atraccio 50) (coneguda FALSE) (tematica DIVERSIO))
    (make-instance [a146] of ATRACCIO (nom "Passeig Marítim Sun Voyager") (ciutat [c26]) (duracio 0.5) (preu_atraccio 0) (coneguda FALSE) (tematica ROMANTIC))

    ;; Sydney (c27)
    (make-instance [a147] of ATRACCIO (nom "Opera House") (ciutat [c27]) (duracio 0.5) (preu_atraccio 35) (coneguda TRUE) (tematica CULTURAL))
    (make-instance [a148] of ATRACCIO (nom "Harbour Bridge") (ciutat [c27]) (duracio 1.0) (preu_atraccio 0) (coneguda TRUE) (tematica DIVERSIO))
    (make-instance [a149] of ATRACCIO (nom "Bondi Beach") (ciutat [c27]) (duracio 1.0) (preu_atraccio 0) (coneguda TRUE) (tematica DIVERSIO))
    (make-instance [a150] of ATRACCIO (nom "Royal Botanic Garden") (ciutat [c27]) (duracio 0.5) (preu_atraccio 0) (coneguda FALSE) (tematica ROMANTIC))
    (make-instance [a151] of ATRACCIO (nom "Taronga Zoo") (ciutat [c27]) (duracio 1.0) (preu_atraccio 45) (coneguda FALSE) (tematica DIVERSIO))

    ;; Florència (c28)
    (make-instance [a152] of ATRACCIO (nom "Duomo") (ciutat [c28]) (duracio 1.0) (preu_atraccio 20) (coneguda TRUE) (tematica CULTURAL))
    (make-instance [a153] of ATRACCIO (nom "Galeria Uffizi") (ciutat [c28]) (duracio 1.0) (preu_atraccio 20) (coneguda TRUE) (tematica CULTURAL))
    (make-instance [a154] of ATRACCIO (nom "Ponte Vecchio") (ciutat [c28]) (duracio 0.5) (preu_atraccio 0) (coneguda TRUE) (tematica ROMANTIC))
    (make-instance [a155] of ATRACCIO (nom "Piazzale Michelangelo") (ciutat [c28]) (duracio 0.5) (preu_atraccio 0) (coneguda FALSE) (tematica ROMANTIC))
    (make-instance [a156] of ATRACCIO (nom "Jardins de Boboli") (ciutat [c28]) (duracio 0.5) (preu_atraccio 10) (coneguda FALSE) (tematica CULTURAL))

    ;; Bora Bora (c29)
    (make-instance [a157] of ATRACCIO (nom "Matira Beach") (ciutat [c29]) (duracio 1.0) (preu_atraccio 0) (coneguda TRUE) (tematica ROMANTIC))
    (make-instance [a158] of ATRACCIO (nom "Excursió de Snorkeling") (ciutat [c29]) (duracio 1.0) (preu_atraccio 50) (coneguda TRUE) (tematica DIVERSIO))
    (make-instance [a159] of ATRACCIO (nom "Mount Otemanu") (ciutat [c29]) (duracio 1.0) (preu_atraccio 0) (coneguda TRUE) (tematica DIVERSIO))
    (make-instance [a160] of ATRACCIO (nom "Tour en vaixell privat") (ciutat [c29]) (duracio 1.0) (preu_atraccio 200) (coneguda FALSE) (tematica ROMANTIC))
    (make-instance [a161] of ATRACCIO (nom "Jardí de corall") (ciutat [c29]) (duracio 0.5) (preu_atraccio 30) (coneguda FALSE) (tematica DIVERSIO))

    ;; Interlaken (c30)
    (make-instance [a162] of ATRACCIO (nom "Harder Kulm") (ciutat [c30]) (duracio 0.5) (preu_atraccio 30) (coneguda TRUE) (tematica DIVERSIO))
    (make-instance [a163] of ATRACCIO (nom "Llac Brienz") (ciutat [c30]) (duracio 1.0) (preu_atraccio 25) (coneguda TRUE) (tematica ROMANTIC))
    (make-instance [a164] of ATRACCIO (nom "Jungfraujoch") (ciutat [c30]) (duracio 1.0) (preu_atraccio 150) (coneguda TRUE) (tematica DIVERSIO))
    (make-instance [a165] of ATRACCIO (nom "Parapent") (ciutat [c30]) (duracio 0.5) (preu_atraccio 180) (coneguda FALSE) (tematica DIVERSIO))
    (make-instance [a166] of ATRACCIO (nom "Coves de Sant Beatus") (ciutat [c30]) (duracio 0.5) (preu_atraccio 15) (coneguda FALSE) (tematica CULTURAL))
    
    ;; Rutes des de París
    (make-instance [t1] of TRANSPORT (ciutat_origen "Paris") (ciutat_desti "Londres") (tipus_transport TREN) (preu_transport 50))
    (make-instance [t2] of TRANSPORT (ciutat_origen "Paris") (ciutat_desti "Berlin")  (tipus_transport TREN) (preu_transport 50))
    (make-instance [t3] of TRANSPORT (ciutat_origen "Paris") (ciutat_desti "Roma")    (tipus_transport AVIO) (preu_transport 150))
    (make-instance [t4] of TRANSPORT (ciutat_origen "Paris") (ciutat_desti "Orlando") (tipus_transport AVIO) (preu_transport 150))

    ;; Rutes des de Roma
    (make-instance [t5] of TRANSPORT (ciutat_origen "Roma") (ciutat_desti "Venecia") (tipus_transport TREN) (preu_transport 50))
    (make-instance [t6] of TRANSPORT (ciutat_origen "Roma") (ciutat_desti "Paris")   (tipus_transport TREN) (preu_transport 50))
    (make-instance [t7] of TRANSPORT (ciutat_origen "Roma") (ciutat_desti "Berlin")  (tipus_transport AVIO) (preu_transport 150))
    (make-instance [t8] of TRANSPORT (ciutat_origen "Roma") (ciutat_desti "Palma")   (tipus_transport VAIXELL) (preu_transport 80))

    ;; Rutes de connexió a Venècia
    (make-instance [t9]  of TRANSPORT (ciutat_origen "Venecia") (ciutat_desti "Roma")    (tipus_transport TREN) (preu_transport 50))
    (make-instance [t10] of TRANSPORT (ciutat_origen "Venecia") (ciutat_desti "Londres")   (tipus_transport VAIXELL) (preu_transport 80))

    ;; Rutes des de Londres
    (make-instance [t11] of TRANSPORT (ciutat_origen "Londres") (ciutat_desti "Berlin")    (tipus_transport TREN) (preu_transport 50))
    (make-instance [t12] of TRANSPORT (ciutat_origen "Londres") (ciutat_desti "Tenerife") (tipus_transport AVIO) (preu_transport 150))
    (make-instance [t13] of TRANSPORT (ciutat_origen "Londres") (ciutat_desti "Maldives") (tipus_transport AVIO) (preu_transport 150))

    ;; Rutes des de Berlín
    (make-instance [t14] of TRANSPORT (ciutat_origen "Berlin") (ciutat_desti "Paris")   (tipus_transport TREN) (preu_transport 50))
    (make-instance [t15] of TRANSPORT (ciutat_origen "Berlin") (ciutat_desti "Roma")    (tipus_transport AVIO) (preu_transport 150))
    (make-instance [t16] of TRANSPORT (ciutat_origen "Berlin") (ciutat_desti "Bali")    (tipus_transport AVIO) (preu_transport 150))

    ;; Rutes de tornada o enllaç des d'illes o intercontinentals
    (make-instance [t17] of TRANSPORT (ciutat_origen "Tenerife") (ciutat_desti "Orlando")  (tipus_transport AVIO) (preu_transport 150))
    (make-instance [t18] of TRANSPORT (ciutat_origen "Orlando")  (ciutat_desti "Londres") (tipus_transport AVIO) (preu_transport 150))
    (make-instance [t19] of TRANSPORT (ciutat_origen "Maldives") (ciutat_desti "Bali")    (tipus_transport AVIO) (preu_transport 150))
    (make-instance [t20] of TRANSPORT (ciutat_origen "Bali")     (ciutat_desti "Paris")   (tipus_transport AVIO) (preu_transport 150))

    ;; Connexions des de ciutats clàssiques cap a noves
    (make-instance [t21] of TRANSPORT (ciutat_origen "Roma") (ciutat_desti "Florencia") (tipus_transport TREN) (preu_transport 40))
    (make-instance [t22] of TRANSPORT (ciutat_origen "Paris") (ciutat_desti "Barcelona") (tipus_transport TREN) (preu_transport 60))
    (make-instance [t23] of TRANSPORT (ciutat_origen "Berlin") (ciutat_desti "Praga") (tipus_transport TREN) (preu_transport 45))
    (make-instance [t24] of TRANSPORT (ciutat_origen "Londres") (ciutat_desti "Amsterdam") (tipus_transport TREN) (preu_transport 55))
    (make-instance [t25] of TRANSPORT (ciutat_origen "Venecia") (ciutat_desti "Viena") (tipus_transport TREN) (preu_transport 70))

    ;; Connexions entre noves ciutats
    (make-instance [t26] of TRANSPORT (ciutat_origen "Barcelona") (ciutat_desti "Eivissa") (tipus_transport VAIXELL) (preu_transport 50))
    (make-instance [t27] of TRANSPORT (ciutat_origen "Barcelona") (ciutat_desti "Florencia") (tipus_transport AVIO) (preu_transport 90))
    (make-instance [t28] of TRANSPORT (ciutat_origen "Praga") (ciutat_desti "Viena") (tipus_transport TREN) (preu_transport 40))
    (make-instance [t29] of TRANSPORT (ciutat_origen "Zermatt") (ciutat_desti "Interlaken") (tipus_transport TREN) (preu_transport 35))
    (make-instance [t30] of TRANSPORT (ciutat_origen "Reykjavik") (ciutat_desti "Amsterdam") (tipus_transport AVIO) (preu_transport 120))
    (make-instance [t31] of TRANSPORT (ciutat_origen "Dubai") (ciutat_desti "Phuket") (tipus_transport AVIO) (preu_transport 250))
    (make-instance [t32] of TRANSPORT (ciutat_origen "Sydney") (ciutat_desti "Bali") (tipus_transport AVIO) (preu_transport 200))
    (make-instance [t33] of TRANSPORT (ciutat_origen "Cancun") (ciutat_desti "Nova York") (tipus_transport AVIO) (preu_transport 180))
    (make-instance [t34] of TRANSPORT (ciutat_origen "Marrakech") (ciutat_desti "Atenes") (tipus_transport AVIO) (preu_transport 130))
    (make-instance [t35] of TRANSPORT (ciutat_origen "Atenes") (ciutat_desti "Santorini") (tipus_transport VAIXELL) (preu_transport 40))
    (make-instance [t36] of TRANSPORT (ciutat_origen "Chamonix") (ciutat_desti "Zermatt") (tipus_transport COTXE) (preu_transport 60))

    ;; Connexions llargues / transcontinentals
    (make-instance [t37] of TRANSPORT (ciutat_origen "Nova York") (ciutat_desti "Londres") (tipus_transport AVIO) (preu_transport 400))
    (make-instance [t38] of TRANSPORT (ciutat_origen "Kyoto") (ciutat_desti "Sydney") (tipus_transport AVIO) (preu_transport 500))
    (make-instance [t39] of TRANSPORT (ciutat_origen "Bora Bora") (ciutat_desti "Sydney") (tipus_transport AVIO) (preu_transport 300))
    (make-instance [t40] of TRANSPORT (ciutat_origen "Aspen") (ciutat_desti "Nova York") (tipus_transport AVIO) (preu_transport 250))

    ;; Rutes de transport intern o proximitat (exemple de tornada)
    (make-instance [t41] of TRANSPORT (ciutat_origen "Eivissa") (ciutat_desti "Barcelona") (tipus_transport VAIXELL) (preu_transport 50))
    (make-instance [t42] of TRANSPORT (ciutat_origen "Viena") (ciutat_desti "Praga") (tipus_transport TREN) (preu_transport 40))
    (make-instance [t43] of TRANSPORT (ciutat_origen "Florencia") (ciutat_desti "Roma") (tipus_transport TREN) (preu_transport 40))
    (make-instance [t44] of TRANSPORT (ciutat_origen "Interlaken") (ciutat_desti "Zermatt") (tipus_transport TREN) (preu_transport 35))

    (make-instance [t45] of TRANSPORT (ciutat_origen "Kyoto") (ciutat_desti "Bali") (tipus_transport AVIO) (preu_transport 200))
    (make-instance [t46] of TRANSPORT (ciutat_origen "Santorini") (ciutat_desti "Atenes") (tipus_transport VAIXELL) (preu_transport 40))
    (make-instance [t47] of TRANSPORT (ciutat_origen "Phuket") (ciutat_desti "Dubai") (tipus_transport AVIO) (preu_transport 250))
    (make-instance [t48] of TRANSPORT (ciutat_origen "Marrakech") (ciutat_desti "Dubai") (tipus_transport AVIO) (preu_transport 200))
    (make-instance [t49] of TRANSPORT (ciutat_origen "Sydney") (ciutat_desti "Kyoto") (tipus_transport AVIO) (preu_transport 400))
    (make-instance [t50] of TRANSPORT (ciutat_origen "Bora Bora") (ciutat_desti "Kyoto") (tipus_transport AVIO) (preu_transport 500))
    (make-instance [t51] of TRANSPORT (ciutat_origen "Aspen") (ciutat_desti "Tenerife") (tipus_transport AVIO) (preu_transport 300))
    (make-instance [t52] of TRANSPORT (ciutat_origen "Reykjavik") (ciutat_desti "Tenerife") (tipus_transport AVIO) (preu_transport 200))

    ;; --- Nuevas rutas adicionales ---
    (make-instance [t53] of TRANSPORT (ciutat_origen "Amsterdam") (ciutat_desti "Reykjavik") (tipus_transport AVIO) (preu_transport 180))
    (make-instance [t54] of TRANSPORT (ciutat_origen "Atenes") (ciutat_desti "Barcelona") (tipus_transport AVIO) (preu_transport 110))
    (make-instance [t55] of TRANSPORT (ciutat_origen "Bali") (ciutat_desti "Phuket") (tipus_transport AVIO) (preu_transport 160))
    (make-instance [t56] of TRANSPORT (ciutat_origen "Barcelona") (ciutat_desti "Paris") (tipus_transport TREN) (preu_transport 60))
    (make-instance [t57] of TRANSPORT (ciutat_origen "Berlin") (ciutat_desti "Londres") (tipus_transport TREN) (preu_transport 55))
    (make-instance [t58] of TRANSPORT (ciutat_origen "Bora Bora") (ciutat_desti "Maldives") (tipus_transport AVIO) (preu_transport 450))
    (make-instance [t59] of TRANSPORT (ciutat_origen "Cancun") (ciutat_desti "Tenerife") (tipus_transport AVIO) (preu_transport 350))
    (make-instance [t60] of TRANSPORT (ciutat_origen "Chamonix") (ciutat_desti "Paris") (tipus_transport TREN) (preu_transport 80))
    (make-instance [t61] of TRANSPORT (ciutat_origen "Dubai") (ciutat_desti "Maldives") (tipus_transport AVIO) (preu_transport 210))
    (make-instance [t62] of TRANSPORT (ciutat_origen "Eivissa") (ciutat_desti "Roma") (tipus_transport AVIO) (preu_transport 70))
    (make-instance [t63] of TRANSPORT (ciutat_origen "Florencia") (ciutat_desti "Venecia") (tipus_transport TREN) (preu_transport 45))
    (make-instance [t64] of TRANSPORT (ciutat_origen "Interlaken") (ciutat_desti "Berlin") (tipus_transport TREN) (preu_transport 90))
    (make-instance [t65] of TRANSPORT (ciutat_origen "Kyoto") (ciutat_desti "Bora Bora") (tipus_transport AVIO) (preu_transport 550))
    (make-instance [t66] of TRANSPORT (ciutat_origen "Maldives") (ciutat_desti "Sydney") (tipus_transport AVIO) (preu_transport 400))
    (make-instance [t67] of TRANSPORT (ciutat_origen "Marrakech") (ciutat_desti "Barcelona") (tipus_transport AVIO) (preu_transport 80))
    (make-instance [t68] of TRANSPORT (ciutat_origen "Nova York") (ciutat_desti "Paris") (tipus_transport AVIO) (preu_transport 350))
    (make-instance [t69] of TRANSPORT (ciutat_origen "Orlando") (ciutat_desti "Cancun") (tipus_transport AVIO) (preu_transport 90))
    (make-instance [t70] of TRANSPORT (ciutat_origen "Phuket") (ciutat_desti "Bora Bora") (tipus_transport AVIO) (preu_transport 450))
    (make-instance [t71] of TRANSPORT (ciutat_origen "Praga") (ciutat_desti "Berlin") (tipus_transport TREN) (preu_transport 45))
    (make-instance [t72] of TRANSPORT (ciutat_origen "Reykjavik") (ciutat_desti "Paris") (tipus_transport AVIO) (preu_transport 190))
    (make-instance [t73] of TRANSPORT (ciutat_origen "Santorini") (ciutat_desti "Roma") (tipus_transport AVIO) (preu_transport 90))
    (make-instance [t74] of TRANSPORT (ciutat_origen "Sydney") (ciutat_desti "Dubai") (tipus_transport AVIO) (preu_transport 500))
    (make-instance [t75] of TRANSPORT (ciutat_origen "Tenerife") (ciutat_desti "Barcelona") (tipus_transport AVIO) (preu_transport 70))
    (make-instance [t76] of TRANSPORT (ciutat_origen "Venecia") (ciutat_desti "Florencia") (tipus_transport TREN) (preu_transport 45))
    (make-instance [t77] of TRANSPORT (ciutat_origen "Viena") (ciutat_desti "Venecia") (tipus_transport TREN) (preu_transport 70))
)

;; Selecciona totes les atraccions que capiguen en els dies disponibles
;; Retorna atraccions i preu_atraccions
(deffunction selecciona-atraccions-ciutat (?ciutat ?dies_disponibles)
    ;; 1. Recollim totes les atraccions de la ciutat
    (bind ?llista-atraccions (create$))
    (do-for-all-instances ((?a ATRACCIO))
        (eq (send ?a get-ciutat) ?ciutat)
        (bind ?llista-atraccions (create$ ?llista-atraccions (instance-name ?a)))
    )
    ;; 2. Mirem la preferència de l'usuari
    (bind ?actitud_despesa (send [usuari_actual] get-actitud_despesa))
    (bind ?llocs_menys_coneguts (send [usuari_actual] get-llocs_menys_coneguts))

    ;; Ordenar per preu (bubble sort)
    (if (eq ?actitud_despesa ESTALVIADOR) then
        (bind ?mida (length$ ?llista-atraccions))
        (loop-for-count (?i 1 ?mida) do
            (loop-for-count (?j (+ ?i 1) ?mida) do
                (bind ?atraccio-i (nth$ ?i ?llista-atraccions))
                (bind ?atraccio-j (nth$ ?j ?llista-atraccions))
                
                (bind ?preu-i (send ?atraccio-i get-preu_atraccio))
                (bind ?preu-j (send ?atraccio-j get-preu_atraccio))
                
                (if (> ?preu-i ?preu-j) then
                    (bind ?llista-atraccions (replace$ ?llista-atraccions ?i ?i ?atraccio-j))
                    (bind ?llista-atraccions (replace$ ?llista-atraccions ?j ?j ?atraccio-i))
                )
            )
        )
    ;; Ordenar de desconegudes a conegudes
    elseif (eq ?llocs_menys_coneguts TRUE) then
        (bind ?mida (length$ ?llista-atraccions))
        (loop-for-count (?i 1 ?mida) do
            (loop-for-count (?j (+ ?i 1) ?mida) do
                (bind ?atraccio-i (nth$ ?i ?llista-atraccions))
                (bind ?atraccio-j (nth$ ?j ?llista-atraccions))
                
                (bind ?pes-i (if (eq (send ?atraccio-i get-coneguda) TRUE) then 1 else 0))
                (bind ?pes-j (if (eq (send ?atraccio-j get-coneguda) TRUE) then 1 else 0))
                
                (if (> ?pes-i ?pes-j) then
                    (bind ?llista-atraccions (replace$ ?llista-atraccions ?i ?i ?atraccio-j))
                    (bind ?llista-atraccions (replace$ ?llista-atraccions ?j ?j ?atraccio-i))
                )
            )
        )
    ;; Ordenar de importants a menys importants
    else
        (bind ?mida (length$ ?llista-atraccions))
        (loop-for-count (?i 1 ?mida) do
            (loop-for-count (?j (+ ?i 1) ?mida) do
                (bind ?atraccio-i (nth$ ?i ?llista-atraccions))
                (bind ?atraccio-j (nth$ ?j ?llista-atraccions))
                
                (bind ?pes-i (if (eq (send ?atraccio-i get-coneguda) TRUE) then 0 else 1))
                (bind ?pes-j (if (eq (send ?atraccio-j get-coneguda) TRUE) then 0 else 1))
                
                (if (> ?pes-i ?pes-j) then
                    (bind ?llista-atraccions (replace$ ?llista-atraccions ?i ?i ?atraccio-j))
                    (bind ?llista-atraccions (replace$ ?llista-atraccions ?j ?j ?atraccio-i))
                )
            )
        )
    )
    (bind ?seleccionades (create$))
    (bind ?dies_usats 0)
    (bind ?preu_atraccions 0)
    ;; 3. Seleccionem atraccions (mirant d'omplir els dies disponibles)
    (progn$ (?a ?llista-atraccions)
        (bind ?dur (send ?a get-duracio))
        (if (<= (+ ?dies_usats ?dur) ?dies_disponibles) then
            (bind ?seleccionades (create$ ?seleccionades ?a))
            (bind ?preu_atraccions (+ ?preu_atraccions (send ?a get-preu_atraccio)))
            (bind ?dies_usats (+ ?dies_usats ?dur))
        )
    )
    (create$ ?preu_atraccions ?seleccionades)    
)

(defrule RECOMANACIO::filtra-allotjaments-valids
    (declare (salience 30))
    (object (is-a USUARI) (sacrifica_qualitat ?sacrifica))
    (object (is-a ALLOTJAMENT) (name ?allotjament) (estrelles ?estrelles))
    (test (or (eq ?sacrifica TRUE) (>= ?estrelles 3)))
    =>
    (assert (allotjament-valid ?allotjament))
)

(defrule RECOMANACIO::crea-seq-un-element
    (declare (salience 20))
    (object (is-a USUARI) (name [usuari_actual])
            (persones_viatjant  ?n_persones)
            (dies_max           ?dies-max)
            (pressupost_max     ?pres)
            (sacrifica_qualitat ?sacrifica)
            (dies_ciutats_min   ?dies_ciutats_min)
            (dies_ciutats_max   ?dies_ciutats_max))

    (VIATGE (objectiu ?obj) (nivell_allotjament ?niv) (ciutats) (cost_total 0))

    (object (is-a CIUTAT) (name ?ciutat_desti) (tipus_viatge $? ?obj $?))

    (object (is-a ALLOTJAMENT)
        (name ?allotjament)
        (ciutat ?ciutat_desti)
        (preu_nit ?preu_allotjament)
    )
    (allotjament-valid ?allotjament)
    =>  
    (loop-for-count (?dur ?dies_ciutats_min ?dies_ciutats_max) do
        (if (<= ?dur ?dies-max) then 
            (bind $?atraccions-i-preu (selecciona-atraccions-ciutat ?ciutat_desti ?dur))
            (bind ?preu_atraccions (nth$ 1 $?atraccions-i-preu))
            (bind $?atraccions (rest$ $?atraccions-i-preu))
            (bind ?preu_total (* ?n_persones (+ (* ?preu_allotjament ?dur) ?preu_atraccions)))
            
            ;; Comprovem el preu aquí perquè necessitem preu_atraccions
            (if (or (<= ?preu_total ?pres)
                    (and (eq ?sacrifica TRUE) (<= ?preu_total (* ?pres 1.25))))
                then
                (assert (VIATGE (objectiu ?obj)
                                (nivell_allotjament ?niv)
                                (ciutats ?ciutat_desti)
                                (dies_ciutat ?dur)
                                (dies_total ?dur)
                                (cost_total ?preu_total)
                                (transports (create$))
                                (atraccions $?atraccions)
                                (allotjaments ?allotjament)))
            )
        )
    )
)

(defrule RECOMANACIO::exten-sequencia
    (declare (salience 10))
    ;; Mirem si podem afegir element a seq
    (object (is-a USUARI) (name [usuari_actual])
            (persones_viatjant  ?n_persones)
            (dies_max           ?dies-max)
            (ciutats_max        ?ciutats_max)
            (dies_ciutats_min   ?dies_ciutats_min)
            (dies_ciutats_max   ?dies_ciutats_max)
            (pressupost_max     ?pres)
            (sacrifica_qualitat ?sacrifica)
            (transport_prohibit $?trans_prohibit))

    (VIATGE (objectiu ?obj)
            (nivell_allotjament ?niv)
            (ciutats $?cs)
            (dies_ciutat $?ds)
            (dies_total ?dies_total)
            (cost_total ?cost_total)
            (transports $?ts)
            (atraccions $?as)
            (allotjaments $?allotjaments))

    ;; necessitem l'ultima ciutat per assegurar que el transport sigui possible
    (object (is-a CIUTAT) (name ?ciutat_origen))
    (test (eq ?ciutat_origen (nth$ (length$ $?cs) $?cs)))

    (object (is-a CIUTAT) (name ?ciutat_desti) (tipus_viatge $? ?obj $?))

    ;; assegurem que existeix transport que conecti ciutats
    (object (is-a TRANSPORT)
            (name ?id_transport)
            (ciutat_origen ?id_origen)
            (ciutat_desti ?id_desti)
            (tipus_transport ?tipus_transport)
            (preu_transport ?preu_transport))
    (test (eq (send ?ciutat_origen get-nom) ?id_origen))
    (test (eq (send ?ciutat_desti get-nom) ?id_desti))
    
    (object (is-a ALLOTJAMENT)
        (name ?allotjament)
        (ciutat ?ciutat_desti)
        (preu_nit ?preu_allotjament)
    )
    (allotjament-valid ?allotjament)

    (test (not (member$ ?tipus_transport $?trans_prohibit)))
    
    ;; nou element no esta a seq
    (test (not (member$ ?ciutat_desti ?cs)))

    ;; la seq de viatges (ciutats visitades) esta per sota de max
    (test (<= (+ (length$ $?cs) 1) ?ciutats_max))

    =>
    (loop-for-count (?dur ?dies_ciutats_min ?dies_ciutats_max) do
        (if (<= (+ ?dies_total ?dur) ?dies-max) then
            (bind $?atraccions-i-preu (selecciona-atraccions-ciutat ?ciutat_desti ?dur))
            (bind ?preu_atraccions (nth$ 1 $?atraccions-i-preu))
            (bind $?atraccions (rest$ $?atraccions-i-preu))
            (bind ?preu_total (* ?n_persones (+ (* ?preu_allotjament ?dur) ?preu_atraccions ?preu_transport)))
            
            ;; Comprovem el preu aquí perquè necessitem preu_atraccions
            (if (or (<= (+ ?cost_total ?preu_total) ?pres)
                    (and (eq ?sacrifica TRUE) (<= (+ ?cost_total ?preu_total) (* ?pres 1.25))))
                then
                (assert (VIATGE
                    (objectiu ?obj)
                    (nivell_allotjament ?niv)
                    (ciutats     $?cs ?ciutat_desti)
                    (dies_ciutat $?ds ?dur)
                    (dies_total  (+ ?dies_total ?dur))
                    (cost_total  (+ ?cost_total ?preu_total))
                    (transports  $?ts ?id_transport)
                    (atraccions $?as $?atraccions)
                    (allotjaments $?allotjaments ?allotjament))
                )
            )
        )
    )
)

(deffunction compta-atraccions-tematica (?tematica $?atraccions)
    (bind ?comptador 0)
    (progn$ (?a $?atraccions)
        (if (eq (send ?a get-tematica) ?tematica) then
            (bind ?comptador (+ ?comptador 1))
        )
    )
    ?comptador
)

(defrule RECOMANACIO::puntua-coherencia-objectiu
    (declare (salience 5))
    ?viatge <- (VIATGE (objectiu ?obj) (ciutats $?cs) (dies_ciutat $?dies_ciutat) (cost_total ?cost) (atraccions $?as) (allotjaments $?allotjaments) (transports $?ts) (puntuacio 0))
    (object (is-a USUARI) (pressupost_max ?pres) (actitud_despesa ?actitud_despesa))
    =>
    (bind ?punts 0)
    (bind ?punts (+ ?punts (* 10 (compta-atraccions-tematica ?obj $?as))))
    (if (<= ?cost ?pres) then
        (bind ?punts (+ ?punts 20))
    )
    (if (eq ?actitud_despesa ESTALVIADOR) then
        (bind ?punts (- ?punts (* 0.02 ?cost))) ;; 500 eur resta 10
    else
        (bind ?punts (- ?punts (* 0.001 ?cost)))
    )

    (modify ?viatge (puntuacio ?punts))
)

(defrule RECOMANACIO::imprimeix-text-dalt
    (declare (salience -3))
    (not (text-impres))
    (object (is-a USUARI) (name [usuari_actual])
            (dies_min        ?dies-min)
            (dies_max        ?dies-max))

    ;; no imprimeix si no hi ha sol vàlida
    (VIATGE (objectiu ?obj) (dies_total ?dies))
    (test (>= ?dies ?dies-min))
    =>
    (printout t crlf "==========================================" crlf)
    (printout t " ITINERARIS RECOMANATS (" ?obj "):" crlf)
    (printout t "==========================================" crlf)
    (assert (text-impres))
)

(deffunction ordena_viatges (?dies_min ?ciutats_min) 
    (bind ?llista (create$))
    (do-for-all-facts ((?v VIATGE))
        ;; no ho hem comprovat encara
        (and (>= ?v:dies_total ?dies_min)
             (>= (length$ ?v:ciutats) ?ciutats_min))
        (bind ?llista (create$ ?llista ?v))
    )

    ;; Bubble sort per puntuació descendent
    (bind ?mida (length$ ?llista))
    (loop-for-count (?i 1 ?mida) do
        (loop-for-count (?j (+ ?i 1) ?mida) do
            (bind ?vi (nth$ ?i ?llista))
            (bind ?vj (nth$ ?j ?llista))
            (if (< (fact-slot-value ?vi puntuacio)
                   (fact-slot-value ?vj puntuacio)) then
                (bind ?llista (replace$ ?llista ?i ?i ?vj))
                (bind ?llista (replace$ ?llista ?j ?j ?vi))
            )
        )
    )
    ?llista
)

(deffunction imprimir_n_millors (?n ?viatges ?pres ?sacrifica) 
    (bind ?mida (length$ ?viatges))
    (bind ?m (min ?n ?mida))
    (loop-for-count (?i 1 ?m) do
        (bind ?v (nth$ ?i ?viatges))
        
        (bind ?cs (fact-slot-value ?v ciutats))
        (bind ?ds (fact-slot-value ?v dies_ciutat))
        (bind ?dies (fact-slot-value ?v dies_total))
        (bind ?cost (fact-slot-value ?v cost_total))
        (bind ?ts (fact-slot-value ?v transports))
        (bind ?as (fact-slot-value ?v atraccions))
        (bind ?allotjaments (fact-slot-value ?v allotjaments))
        (bind ?puntuacio (fact-slot-value ?v puntuacio))

        (printout t crlf "Ruta (" (length$ ?cs) " destins): ")
        ;; Imprimir ciutats
        ;; progn -> es repeteixen les inst del bloc 1 cop per element llista
        (progn$ (?ciutat ?cs)
            (bind ?dur (nth$ ?ciutat-index $?ds))
            (printout t (send ?ciutat get-nom) " ["?dur "d]")
            (if (< ?ciutat-index (length$ ?cs)) then
                (printout t " → "))
        )
        ;; Imprimir allotjaments
        (printout t crlf "   Allotjaments: ")
        (progn$ (?allotjament ?allotjaments)
            (bind ?ciutat (send ?allotjament get-ciutat))
            (printout t (send ?ciutat get-nom) ": " (send ?allotjament get-nom) 
                        " " (send ?allotjament get-estrelles) "* ("
                        (send ?allotjament get-preu_nit) "€)")
            (if (< ?allotjament-index (length$ ?allotjaments)) then 
                (printout t ", "))
        )
        
        ;; Imprimir atraccions 
        (printout t crlf "   Visites: ")
        (progn$ (?ciutat ?cs)
            ;; Subcadena de atraccions (de la ciutat)
            (bind ?as_ciutat (create$))
            (progn$ (?a ?as)
                (if (eq (send ?a get-ciutat) ?ciutat) then
                    (bind ?as_ciutat (create$ ?as_ciutat ?a)))
            )

            (printout t crlf "     · " (send ?ciutat get-nom) ": ")
            (if (> (length$ ?as_ciutat) 0) then        
                (progn$ (?a $?as_ciutat)
                    (printout t (send ?a get-nom) " (" (send ?a get-preu_atraccio) "€)")
                    (if (< ?a-index (length$ $?as_ciutat)) then
                        (printout t ", "))
                )
            else
                (printout t "(sense visites)")
            )
        )
        ;; Imprimir cost i dies
        (if (<= ?cost ?pres)then
            (printout t crlf "   Dies total: " ?dies
                            " | Preu total: " ?cost " €" crlf)
        else (if (and (eq ?sacrifica TRUE)
                    (<= ?cost (* ?pres 1.25))) then
            (printout t crlf "   Dies total: " ?dies
                            " | PREU AJUSTAT: " ?cost " €" crlf)
        )
        )
        ;; Imprimir transport
        (if (> (length$ ?ts) 0) then
            (printout t "   Transports: ")
            (bind ?idx 1)
            (progn$ (?transport ?ts)
                (printout t (send ?transport get-ciutat_origen) 
                "-" (send ?transport get-ciutat_desti) 
                ": " (send ?transport get-tipus_transport)
                " (" (send ?transport get-preu_transport) " €)")
                (if (< ?idx (length$ ?ts)) then (printout t " → "))
                (bind ?idx (+ ?idx 1))
            )
            (printout t crlf)
        )

        ;; Imprimir puntuacio
        (printout t "   Puntuació: " ?puntuacio crlf)
    )
)

(defrule RECOMANACIO::imprimeix-sequencia-valida
    (declare (salience -5))
    (object (is-a USUARI) (name [usuari_actual])
            (dies_min ?dies_min)
            (ciutats_min ?ciutats_min)
            (pressupost_max ?pres)
            (sacrifica_qualitat ?sacrifica))
    =>
    (bind $?viatges_ordenats_per_puntuacio (ordena_viatges ?dies_min ?ciutats_min))
    
    (if (> (length$ $?viatges_ordenats_per_puntuacio) 0) then
        (imprimir_n_millors 2 ?viatges_ordenats_per_puntuacio ?pres ?sacrifica)
        (assert (solucio-trobada))
    )
)

(defrule RECOMANACIO::sense-propostes
    (declare (salience -10))
    (not (solucio-trobada))
    =>
    (printout t crlf "==========================================" crlf)
    (printout t " HO SENTIM, CAP ITINERARI COMPLEIX LES RESTRICCIONS." crlf)
    (printout t " Prova d'augmentar el pressupost, l'interval de dies," crlf)
    (printout t " o marca 'sacrifica qualitat'." crlf)
    (printout t "==========================================" crlf)
)


