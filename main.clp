;;; ==============================================================
;;; MÒDULS DEL SISTEMA
;;; ==============================================================

(defmodule MAIN (export ?ALL))
(defmodule RECOLLIDA-DADES (import MAIN ?ALL) (export ?ALL))
(defmodule INFERENCIA (import MAIN ?ALL) (export ?ALL))
(defmodule RECOMANACIO (import MAIN ?ALL))

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

(defrule RECOLLIDA-DADES::pregunta-pressupost
    =>
    (printout t "Quin es el teu pressupost maxim aproximat (en euros)? ")
    (bind ?pres (read))
    (send [usuari_actual] put-pressupost_max ?pres)
)

(defrule RECOLLIDA-DADES::pregunta-dies
    =>
    (printout t "Quants dies tens disponibles per viatjar? ")
    (bind ?dies (read))
    (send [usuari_actual] put-dies_disponibles ?dies)
)

(defrule RECOLLIDA-DADES::pregunta-grup
    =>
    (printout t "Viatges sol, en parella, en familia o amb amics? (individual/parella/familia/amics): ")
    (bind ?resp (read))
    (if (eq ?resp familia) then
        (send [usuari_actual] put-perfil_deduit FAMILIA)
        (send [usuari_actual] put-viatja_amb_nens TRUE)
    else (if (eq ?resp parella) then
        (send [usuari_actual] put-perfil_deduit PARELLA)
    else (if (eq ?resp individual) then
        (send [usuari_actual] put-perfil_deduit INDIVIDUAL)
    else (if (eq ?resp amics) then
        (send [usuari_actual] put-perfil_deduit GRUP)
    ))))
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

;;; ==============================================================
;;; MÒDUL 2: INFERÈNCIA DEL PERFIL (Coneixement Expert)
;;; ==============================================================

(defrule INFERENCIA::dedueix-objectiu-familia
    (object (is-a USUARI) (name [usuari_actual]) (viatja_amb_nens TRUE) (objectiu_deduit DESCONEGUT))
    =>
    (printout t crlf ">> [INFERENCIA] Viatja amb nens. Descartem descans pur. Objectiu: DIVERSIO." crlf)
    (send [usuari_actual] put-objectiu_deduit DIVERSIO)
)

(defrule INFERENCIA::dedueix-objectiu-parella
    (object (is-a USUARI) (name [usuari_actual]) (perfil_deduit PARELLA) (objectiu_deduit DESCONEGUT))
    =>
    (printout t crlf ">> [INFERENCIA] Viatja en parella. Suggerim un viatge ROMANTIC." crlf)
    (send [usuari_actual] put-objectiu_deduit ROMANTIC)
)

(defrule INFERENCIA::dedueix-objectiu-individual
    (object (is-a USUARI) (name [usuari_actual]) (perfil_deduit INDIVIDUAL) (objectiu_deduit DESCONEGUT))
    =>
    (printout t crlf ">> [INFERENCIA] Viatja en solitari. Suggerim un viatge CULTURAL per enriquir-se." crlf)
    (send [usuari_actual] put-objectiu_deduit CULTURAL)
)

(defrule INFERENCIA::dedueix-objectiu-amics
    (object (is-a USUARI) (name [usuari_actual]) (perfil_deduit GRUP) (objectiu_deduit DESCONEGUT))
    =>
    (printout t crlf ">> [INFERENCIA] Viatge amb amics. Suggerim un viatge orientat a la DIVERSIO." crlf)
    (send [usuari_actual] put-objectiu_deduit DIVERSIO)
)

;;; ==============================================================
;;; MÒDUL 3: RECOMANACIÓ I FILTRATGE
;;; ==============================================================

(defrule RECOMANACIO::carrega-dades
    (declare (salience 50))
    =>
    ;; CIUTATS
    (make-instance [c1] of CIUTAT (nom "Paris") (tematica EUROPEA_GRAN) (cost_diari_estimat 150))
    (make-instance [c2] of CIUTAT (nom "Roma") (tematica MEDITERRANIA) (cost_diari_estimat 120))
    (make-instance [c3] of CIUTAT (nom "Venecia") (tematica MEDITERRANIA) (cost_diari_estimat 160))
    (make-instance [c4] of CIUTAT (nom "Orlando") (tematica DIVERSIO) (cost_diari_estimat 200))
    (make-instance [c5] of CIUTAT (nom "Tenerife") (tematica DIVERSIO) (cost_diari_estimat 90))
    (make-instance [c6] of CIUTAT (nom "Maldives") (tematica NATURALESA) (cost_diari_estimat 300))
    (make-instance [c7] of CIUTAT (nom "Londres") (tematica EUROPEA_GRAN) (cost_diari_estimat 180))
    (make-instance [c8] of CIUTAT (nom "Berlin") (tematica EUROPEA_GRAN) (cost_diari_estimat 110))
    (make-instance [c9] of CIUTAT (nom "Bali") (tematica NATURALESA) (cost_diari_estimat 80))

    ;; VIATGES ROMÀNTICS
    (make-instance [v1] of VIATGE (id_viatge "V1") (tipus_viatge ROMANTIC) (ciutat_desti "Paris") (preu_total 1200) (duracio_dies 7))
    (make-instance [v2] of VIATGE (id_viatge "V2") (tipus_viatge ROMANTIC) (ciutat_desti "Venecia") (preu_total 1400) (duracio_dies 5))
    (make-instance [v3] of VIATGE (id_viatge "V3") (tipus_viatge ROMANTIC) (ciutat_desti "Maldives") (preu_total 3500) (duracio_dies 10))
    
    ;; VIATGES DE DIVERSIÓ (Família/Amics)
    (make-instance [v4] of VIATGE (id_viatge "V4") (tipus_viatge DIVERSIO) (ciutat_desti "Orlando") (preu_total 2500) (duracio_dies 10))
    (make-instance [v5] of VIATGE (id_viatge "V5") (tipus_viatge DIVERSIO) (ciutat_desti "Tenerife") (preu_total 900) (duracio_dies 7))
    (make-instance [v6] of VIATGE (id_viatge "V6") (tipus_viatge DIVERSIO) (ciutat_desti "Bali") (preu_total 1800) (duracio_dies 12))

    ;; VIATGES CULTURALS (Individual)
    (make-instance [v7] of VIATGE (id_viatge "V7") (tipus_viatge CULTURAL) (ciutat_desti "Roma") (preu_total 950) (duracio_dies 6))
    (make-instance [v8] of VIATGE (id_viatge "V8") (tipus_viatge CULTURAL) (ciutat_desti "Londres") (preu_total 1300) (duracio_dies 7))
    (make-instance [v9] of VIATGE (id_viatge "V9") (tipus_viatge CULTURAL) (ciutat_desti "Berlin") (preu_total 850) (duracio_dies 5))
)

(defrule RECOMANACIO::genera-dues-propostes
    ;; Afegim la condició per comprovar que no s'hagin imprès ja
    (not (propostes-impreses)) 
    
    (object (is-a USUARI) (name [usuari_actual]) (objectiu_deduit ?obj) (pressupost_max ?pres) (dies_disponibles ?dies) (sacrifica_qualitat ?sacrifica))
    
    (object (is-a VIATGE) (name ?v1) (tipus_viatge ?obj) (ciutat_desti ?desti1) (preu_total ?preu1) (duracio_dies ?dur1))
    (object (is-a VIATGE) (name ?v2) (tipus_viatge ?obj) (ciutat_desti ?desti2) (preu_total ?preu2) (duracio_dies ?dur2))
    
    (test (neq ?v1 ?v2)) 
    (test (neq ?desti1 ?desti2)) 
    (test (< (str-compare ?desti1 ?desti2) 0)) 
    
    (test (<= ?dur1 ?dies))
    (test (<= ?dur2 ?dies))
    
    (test (or (<= ?preu1 ?pres) (and (eq ?sacrifica TRUE) (<= ?preu1 (* ?pres 1.25)))))
    (test (or (<= ?preu2 ?pres) (and (eq ?sacrifica TRUE) (<= ?preu2 (* ?pres 1.25)))))
    =>
    (printout t crlf "==========================================" crlf)
    (printout t " TENIM 2 PROPOSTES PER A TU (" ?obj "):" crlf)
    (printout t "==========================================" crlf)
    
    (if (> ?preu1 ?pres) then
        (printout t " OPCIO 1: " ?desti1 " (" ?dur1 " dies) -> PREU AJUSTAT (Original: " ?preu1 " euros)" crlf)
    else
        (printout t " OPCIO 1: " ?desti1 " (" ?dur1 " dies) -> Preu normal: " ?preu1 " euros" crlf)
    )
    
    (if (> ?preu2 ?pres) then
        (printout t " OPCIO 2: " ?desti2 " (" ?dur2 " dies) -> PREU AJUSTAT (Original: " ?preu2 " euros)" crlf)
    else
        (printout t " OPCIO 2: " ?desti2 " (" ?dur2 " dies) -> Preu normal: " ?preu2 " euros" crlf)
    )
    (printout t "==========================================" crlf)
    
    (assert (solucio-trobada))
    ;; Marquem que ja hem imprès les propostes per aturar el bucle
    (assert (propostes-impreses)) 
)

(defrule RECOMANACIO::sense-propostes
    (declare (salience -10)) 
    (not (solucio-trobada))
    =>
    (printout t crlf "==========================================" crlf)
    (printout t " HO SENTIM, CAP VIATGE COMPLEIX LES RESTRICCIONS." crlf)
    (printout t " Prova d'augmentar el pressupost, els dies, o sacrificar qualitat." crlf)
    (printout t "==========================================" crlf)
)