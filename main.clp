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

(defrule RECOLLIDA-DADES::pregunta-edat
    =>
    (printout t "Quants anys tens? ")
    (bind ?resp (read))
    (if (< ?resp 18) then
        (send [usuari_actual] put-rang_edat MENOR)
        (send [usuari_actual] put-viatja_amb_nens TRUE)
    else (if (and (>= ?resp 18) (< ?resp 45)) then
        (send [usuari_actual] put-rang_edat JOVE)
    else (if (and (>= ?resp 45) (< ?resp 65)) then
        (send [usuari_actual] put-rang_edat MITJANA-EDAT)
    else (if (> ?resp 65) then
        (send [usuari_actual] put-rang_edat GRAN)
    ))))
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
    (printout t "Hi ha algun tipus de transporte (avió, vaixell o tren) que no puguis agafar?" crlf)
    (printout t "Introdueix-los un a un. Escriu 'fi' quan hagis acabat o si no en vols prohibir cap." crlf)
    
    (bind $?prohibits (create$))
    (printout t "Transport prohibit: ")
    (bind ?resp (read))
    
    (while (neq ?resp fi)
        (bind ?simbol CAP)
        (if (or (eq ?resp avio) (eq ?resp avió)) then (bind ?simbol AVIO))
        (if (eq ?resp vaixell)                 then (bind ?simbol VAIXELL))
        (if (eq ?resp tren)                    then (bind ?simbol TREN))
        
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

(deftemplate RECOMANACIO::SOLUCIO-PARCIAL
    (multislot viatges)
    (slot dies_total (default 0))
    (slot cost_total (default 0))
    (multislot transports)
)

;;; --- Càrrega de dades (sense canvis) ---
(defrule RECOMANACIO::carrega-dades
    (declare (salience 50))
    =>
    (make-instance [c1] of CIUTAT (nom "Paris")    (tematica EUROPEA_GRAN)  (cost_diari_estimat 150))
    (make-instance [c2] of CIUTAT (nom "Roma")     (tematica MEDITERRANIA)  (cost_diari_estimat 120))
    (make-instance [c3] of CIUTAT (nom "Venecia")  (tematica MEDITERRANIA)  (cost_diari_estimat 160))
    (make-instance [c4] of CIUTAT (nom "Orlando")  (tematica DIVERSIO)      (cost_diari_estimat 200))
    (make-instance [c5] of CIUTAT (nom "Tenerife") (tematica DIVERSIO)      (cost_diari_estimat 90))
    (make-instance [c6] of CIUTAT (nom "Maldives") (tematica NATURALESA)    (cost_diari_estimat 300))
    (make-instance [c7] of CIUTAT (nom "Londres")  (tematica EUROPEA_GRAN)  (cost_diari_estimat 180))
    (make-instance [c8] of CIUTAT (nom "Berlin")   (tematica EUROPEA_GRAN)  (cost_diari_estimat 110))
    (make-instance [c9] of CIUTAT (nom "Bali")     (tematica NATURALESA)    (cost_diari_estimat 80))

    (make-instance [v1] of VIATGE (id_viatge "V1") (tipus_viatge ROMANTIC) (ciutat_desti "Paris")    (preu_total 1200) (duracio_dies 7))
    (make-instance [v2] of VIATGE (id_viatge "V2") (tipus_viatge ROMANTIC) (ciutat_desti "Venecia")  (preu_total 1400) (duracio_dies 5))
    (make-instance [v3] of VIATGE (id_viatge "V3") (tipus_viatge ROMANTIC) (ciutat_desti "Maldives") (preu_total 3500) (duracio_dies 10))
    (make-instance [v4] of VIATGE (id_viatge "V4") (tipus_viatge DIVERSIO) (ciutat_desti "Orlando")  (preu_total 2500) (duracio_dies 10))
    (make-instance [v5] of VIATGE (id_viatge "V5") (tipus_viatge DIVERSIO) (ciutat_desti "Tenerife") (preu_total 900)  (duracio_dies 7))
    (make-instance [v6] of VIATGE (id_viatge "V6") (tipus_viatge DIVERSIO) (ciutat_desti "Bali")     (preu_total 1800) (duracio_dies 12))
    (make-instance [v7] of VIATGE (id_viatge "V7") (tipus_viatge CULTURAL) (ciutat_desti "Roma")     (preu_total 950)  (duracio_dies 6))
    (make-instance [v8] of VIATGE (id_viatge "V8") (tipus_viatge CULTURAL) (ciutat_desti "Londres")  (preu_total 1300) (duracio_dies 7))
    (make-instance [v9] of VIATGE (id_viatge "V9") (tipus_viatge CULTURAL) (ciutat_desti "Berlin")   (preu_total 850)  (duracio_dies 5))

    ;; Rutes des de París
    (make-instance [t1] of TRANSPORT (ciutat_origen "Paris") (ciutat_desti "Londres") (tipus_transport TREN))
    (make-instance [t2] of TRANSPORT (ciutat_origen "Paris") (ciutat_desti "Berlin")  (tipus_transport TREN))
    (make-instance [t3] of TRANSPORT (ciutat_origen "Paris") (ciutat_desti "Roma")    (tipus_transport AVIO))
    (make-instance [t4] of TRANSPORT (ciutat_origen "Paris") (ciutat_desti "Orlando") (tipus_transport AVIO))

    ;; Rutes des de Roma
    (make-instance [t5] of TRANSPORT (ciutat_origen "Roma") (ciutat_desti "Venecia") (tipus_transport TREN))
    (make-instance [t6] of TRANSPORT (ciutat_origen "Roma") (ciutat_desti "Paris")   (tipus_transport TREN))
    (make-instance [t7] of TRANSPORT (ciutat_origen "Roma") (ciutat_desti "Berlin")  (tipus_transport AVIO))
    (make-instance [t8] of TRANSPORT (ciutat_origen "Roma") (ciutat_desti "Palma")   (tipus_transport VAIXELL))

    ;; Rutes de connexió a Venècia
    (make-instance [t9]  of TRANSPORT (ciutat_origen "Venecia") (ciutat_desti "Roma")    (tipus_transport TREN))
    (make-instance [t10] of TRANSPORT (ciutat_origen "Venecia") (ciutat_desti "Londres")   (tipus_transport VAIXELL))

    ;; Rutes des de Londres
    (make-instance [t11] of TRANSPORT (ciutat_origen "Londres") (ciutat_desti "Berlin")    (tipus_transport TREN))
    (make-instance [t12] of TRANSPORT (ciutat_origen "Londres") (ciutat_desti "Tenerife") (tipus_transport AVIO))
    (make-instance [t13] of TRANSPORT (ciutat_origen "Londres") (ciutat_desti "Maldives") (tipus_transport AVIO))

    ;; Rutes des de Berlín
    (make-instance [t14] of TRANSPORT (ciutat_origen "Berlin") (ciutat_desti "Paris")   (tipus_transport TREN))
    (make-instance [t15] of TRANSPORT (ciutat_origen "Berlin") (ciutat_desti "Roma")    (tipus_transport AVIO))
    (make-instance [t16] of TRANSPORT (ciutat_origen "Berlin") (ciutat_desti "Bali")    (tipus_transport AVIO))

    ;; Rutes de tornada o enllaç des d'illes o intercontinentals
    (make-instance [t17] of TRANSPORT (ciutat_origen "Tenerife") (ciutat_desti "Madrid")  (tipus_transport AVIO))
    (make-instance [t18] of TRANSPORT (ciutat_origen "Orlando")  (ciutat_desti "Londres") (tipus_transport AVIO))
    (make-instance [t19] of TRANSPORT (ciutat_origen "Maldives") (ciutat_desti "Roma")    (tipus_transport AVIO))
    (make-instance [t20] of TRANSPORT (ciutat_origen "Bali")     (ciutat_desti "Paris")   (tipus_transport AVIO))

)

(defrule RECOMANACIO::crea-seq-un-element
    (declare (salience 20))
    (object (is-a USUARI) (name [usuari_actual])
            (objectiu_deduit    ?obj)
            (dies_max           ?dies-max)
            (pressupost_max     ?pres)
            (sacrifica_qualitat ?sacrifica)
            (dies_ciutats_min   ?dies_ciutats_min)
            (dies_ciutats_max   ?dies_ciutats_max))
    (object (is-a VIATGE) (name ?v)
            (tipus_viatge  ?obj)
            (duracio_dies  ?dur)
            (preu_total    ?preu))
    (test (<= ?dur ?dies-max))
    (test (or (<= ?preu ?pres)
              (and (eq ?sacrifica TRUE) (<= ?preu (* ?pres 1.25)))))
    (test (<= ?dur ?dies_ciutats_max))
    (test (>= ?dur ?dies_ciutats_min))
    =>
    (assert (SOLUCIO-PARCIAL (viatges ?v)
                             (dies_total ?dur)
                             (cost_total ?preu)
                             (transports (create$))))
)

(defrule RECOMANACIO::exten-sequencia
    ;; Mirem si podem afegir element a seq
    (object (is-a USUARI) (name [usuari_actual])
            (objectiu_deduit    ?obj)
            (dies_max           ?dies-max)
            (ciutats_max        ?ciutats_max)
            (dies_ciutats_min   ?dies_ciutats_min)
            (dies_ciutats_max   ?dies_ciutats_max)
            (pressupost_max     ?pres)
            (sacrifica_qualitat ?sacrifica)
            (transport_prohibit $?trans_prohibit))

    (SOLUCIO-PARCIAL (viatges $?vs)
                     (dies_total ?dies-act)
                     (cost_total ?cost-act)
                     (transports $?ts))

    (object (is-a VIATGE) (name ?ultim_viatge) (ciutat_desti ?ciutat_origen))
    (test (eq ?ultim_viatge (nth$ (length$ $?vs) $?vs)))

    (object (is-a VIATGE) (name ?v)
            (tipus_viatge  ?obj)
            (duracio_dies  ?dur)
            (preu_total    ?preu)
            (ciutat_desti ?ciutat_desti))

    ;; assegurem que existeix transport que conecti ciutats
    (object (is-a TRANSPORT)
            (name ?id_transport)
            (ciutat_origen ?ciutat_origen)
            (ciutat_desti ?ciutat_desti)
            (tipus_transport ?tipus_transport))

    (test (not (member$ ?tipus_transport $?trans_prohibit)))
    
    ;; nou element no esta a seq
    (test (not (member$ ?v ?vs)))
    ;; per no repetir permutacions ordenem els viatges per ordre
    ;; de llargada del nom (es pot treure si ens interessa)
    ;;(test (> (str-compare (str-cat ?v)
    ;;                      (str-cat (nth$ (length$ ?vs) $?vs)))
    ;;         0))
    ;; preconds de la solució parcial
    ;; la suma de dies està per sota del màxim
    (test (<= (+ ?dies-act ?dur) ?dies-max))
    ;; el cost combinat és assolible
    (test (or (<= (+ ?cost-act ?preu) ?pres)
              (and (eq ?sacrifica TRUE)
                   (<= (+ ?cost-act ?preu) (* ?pres 1.25)))))
    ;; la seq de viatges (ciutats visitades) esta per sota de max
    (test (<= (+ (length$ $?vs) 1) ?ciutats_max))
    ;; la parada a la ciutat es coherent amb el rang dels dies per ciutat
    (test (<= ?dur ?dies_ciutats_max))
    (test (>= ?dur ?dies_ciutats_min))
    =>
    (assert (SOLUCIO-PARCIAL
        (viatges     $?vs ?v)
        (dies_total  (+ ?dies-act ?dur))
        (cost_total  (+ ?cost-act ?preu))
        (transports  $?ts ?id_transport)))
)

(defrule RECOMANACIO::imprimeix-text-dalt
    (declare (salience -3))
    (not (text-impres))
    (object (is-a USUARI) (name [usuari_actual])
            (dies_min        ?dies-min)
            (dies_max        ?dies-max)
            (objectiu_deduit ?obj))
    ;; no imprimeix si no hi ha sol vàlida
    (SOLUCIO-PARCIAL (dies_total ?dies))
    (test (>= ?dies ?dies-min))
    =>
    (printout t crlf "==========================================" crlf)
    (printout t " ITINERARIS RECOMANATS (" ?obj "):" crlf)
    (printout t "==========================================" crlf)
    (assert (text-impres))
)

(defrule RECOMANACIO::imprimeix-sequencia-valida
    (declare (salience -5))
    (object (is-a USUARI) (name [usuari_actual])
            (dies_min ?dies-min)
            (ciutats_min ?ciutats-min)
            (pressupost_max ?pres)
            (sacrifica_qualitat ?sacrifica))
    (SOLUCIO-PARCIAL 
        (viatges $?vs) 
        (dies_total ?dies) 
        (cost_total ?cost)
        (transports $?ts))
    (test (>= ?dies ?dies-min))
    (test (>= (length $?vs) ?ciutats-min))
    =>
    (printout t crlf "Ruta (" (length$ ?vs) " destins): ")
    ;; progn -> es repeteixen les inst del bloc 1 cop per element llista
    (progn$ (?v ?vs)
        (printout t (send ?v get-ciutat_desti)
                    " [" (send ?v get-duracio_dies) "d]")
        (if (< ?v-index (length$ ?vs)) then
            (printout t " → "))
    )
    (if (<= ?cost ?pres)then
        (printout t crlf "   Dies total: " ?dies
                        " | Preu total: " ?cost " €" crlf)
    else (if (and (eq ?sacrifica TRUE)
                   (<= ?cost (* ?pres 1.25))) then
        (printout t crlf "   Dies total: " ?dies
                        " | PREU AJUSTAT: " ?cost " €" crlf)
    )
    )

    (if (> (length$ ?ts) 0) then
        (printout t "   Transports: ")
        (bind ?idx 1)
        (progn$ (?transport ?ts)
            (printout t (send ?transport get-ciutat_origen) 
            "-" (send ?transport get-ciutat_desti) 
            ": " (send ?transport get-tipus_transport))
            (if (< ?idx (length$ ?ts)) then (printout t " → "))
            (bind ?idx (+ ?idx 1))
        )
        (printout t crlf)
    )

    (assert (solucio-trobada))
)

(defrule RECOMANACIO::prova-transports
    (object (is-a TRANSPORT) (ciutat_origen "Londres") (ciutat_desti "Berlin"))
    =>
    (printout t "Transport Londres->Berlin trobat!" crlf))

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


