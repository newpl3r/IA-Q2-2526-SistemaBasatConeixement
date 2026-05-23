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
    (multislot atraccions)
    (multislot allotjaments)
)

;;; --- Càrrega de dades (sense canvis) ---
(defrule RECOMANACIO::carrega-dades
    (declare (salience 50))
    =>    
    (make-instance [c1] of CIUTAT (nom "Paris")    (tematica EUROPEA_GRAN))
    (make-instance [c2] of CIUTAT (nom "Roma")     (tematica MEDITERRANIA))
    (make-instance [c3] of CIUTAT (nom "Venecia")  (tematica MEDITERRANIA))
    (make-instance [c4] of CIUTAT (nom "Orlando")  (tematica DIVERSIO))
    (make-instance [c5] of CIUTAT (nom "Tenerife") (tematica DIVERSIO))
    (make-instance [c6] of CIUTAT (nom "Maldives") (tematica NATURALESA))
    (make-instance [c7] of CIUTAT (nom "Londres")  (tematica EUROPEA_GRAN))
    (make-instance [c8] of CIUTAT (nom "Berlin")   (tematica EUROPEA_GRAN))
    (make-instance [c9] of CIUTAT (nom "Bali")     (tematica NATURALESA))

    (make-instance [h1] of ALLOTJAMENT (nom "PA") (ciutat [c1]) (estrelles 4) (preu_nit 120))
    (make-instance [h2] of ALLOTJAMENT (nom "PA") (ciutat [c1]) (estrelles 2) (preu_nit 65))
    (make-instance [h3] of ALLOTJAMENT (nom "PA") (ciutat [c1]) (estrelles 5) (preu_nit 200))
    (make-instance [h4] of ALLOTJAMENT (nom "RA") (ciutat [c2]) (estrelles 3) (preu_nit 80))
    (make-instance [h5] of ALLOTJAMENT (nom "RA") (ciutat [c2]) (estrelles 4) (preu_nit 110))
    (make-instance [h6] of ALLOTJAMENT (nom "VA") (ciutat [c3]) (estrelles 3) (preu_nit 95))
    (make-instance [h7] of ALLOTJAMENT (nom "VA") (ciutat [c3]) (estrelles 5) (preu_nit 210))
    (make-instance [h8] of ALLOTJAMENT (nom "OA") (ciutat [c4]) (estrelles 3) (preu_nit 100))
    (make-instance [h9] of ALLOTJAMENT (nom "OA") (ciutat [c4]) (estrelles 4) (preu_nit 140))
    (make-instance [h10] of ALLOTJAMENT (nom "TA") (ciutat [c5]) (estrelles 2) (preu_nit 45))
    (make-instance [h11] of ALLOTJAMENT (nom "TA") (ciutat [c5]) (estrelles 4) (preu_nit 85))
    (make-instance [h12] of ALLOTJAMENT (nom "MA") (ciutat [c6]) (estrelles 5) (preu_nit 250))
    (make-instance [h13] of ALLOTJAMENT (nom "MA") (ciutat [c6]) (estrelles 3) (preu_nit 130))
    (make-instance [h14] of ALLOTJAMENT (nom "LA") (ciutat [c7]) (estrelles 3) (preu_nit 105))
    (make-instance [h15] of ALLOTJAMENT (nom "LA") (ciutat [c7]) (estrelles 4) (preu_nit 150))
    (make-instance [h16] of ALLOTJAMENT (nom "LA") (ciutat [c7]) (estrelles 2) (preu_nit 70))
    (make-instance [h17] of ALLOTJAMENT (nom "BA") (ciutat [c8]) (estrelles 3) (preu_nit 70))
    (make-instance [h18] of ALLOTJAMENT (nom "BA") (ciutat [c8]) (estrelles 1) (preu_nit 35))
    (make-instance [h19] of ALLOTJAMENT (nom "BA") (ciutat [c9]) (estrelles 4) (preu_nit 65))
    (make-instance [h20] of ALLOTJAMENT (nom "BA") (ciutat [c9]) (estrelles 2) (preu_nit 30))

    ;; París
    (make-instance [a1]  of ATRACCIO (nom "Torre Eiffel")       (ciutat [c1]) (duracio 1.0) (preu_atraccio 25))
    (make-instance [a2]  of ATRACCIO (nom "Museu del Louvre")   (ciutat [c1]) (duracio 1.0) (preu_atraccio 17))
    (make-instance [a3]  of ATRACCIO (nom "Notre-Dame")         (ciutat [c1]) (duracio 0.5) (preu_atraccio 0))
    ;; Roma
    (make-instance [a4]  of ATRACCIO (nom "Coliseu")            (ciutat [c2]) (duracio 1.0) (preu_atraccio 16))
    (make-instance [a5]  of ATRACCIO (nom "Fontana di Trevi")   (ciutat [c2]) (duracio 0.5) (preu_atraccio 0))
    (make-instance [a6]  of ATRACCIO (nom "Vaticà")             (ciutat [c2]) (duracio 2.0) (preu_atraccio 17))
    ;; Venècia
    (make-instance [a7]  of ATRACCIO (nom "Canal Gran")         (ciutat [c3]) (duracio 0.5) (preu_atraccio 0))
    (make-instance [a8]  of ATRACCIO (nom "Plaça de Sant Marc") (ciutat [c3]) (duracio 0.5) (preu_atraccio 0))
    (make-instance [a9]  of ATRACCIO (nom "Pont de Rialto")     (ciutat [c3]) (duracio 0.5) (preu_atraccio 0))
    ;; Orlando
    (make-instance [a10] of ATRACCIO (nom "Walt Disney World")  (ciutat [c4]) (duracio 2.0) (preu_atraccio 120))
    (make-instance [a11] of ATRACCIO (nom "Universal Studios")  (ciutat [c4]) (duracio 1.0) (preu_atraccio 115))
    ;; Tenerife
    (make-instance [a12] of ATRACCIO (nom "Teide")              (ciutat [c5]) (duracio 1.0) (preu_atraccio 25))
    (make-instance [a13] of ATRACCIO (nom "Loro Parque")        (ciutat [c5]) (duracio 1.0) (preu_atraccio 40))
    (make-instance [a14] of ATRACCIO (nom "Siam Park")          (ciutat [c5]) (duracio 1.0) (preu_atraccio 40))
    ;; Maldives
    (make-instance [a15] of ATRACCIO (nom "Atòl de Malé")       (ciutat [c6]) (duracio 0.5) (preu_atraccio 0))
    (make-instance [a16] of ATRACCIO (nom "Esculls de corall")  (ciutat [c6]) (duracio 0.5) (preu_atraccio 0))
    (make-instance [a17] of ATRACCIO (nom "Platges de sorra blanca") (ciutat [c6]) (duracio 1.0) (preu_atraccio 0))
    ;; Londres
    (make-instance [a18] of ATRACCIO (nom "Big Ben")            (ciutat [c7]) (duracio 0.5) (preu_atraccio 0))
    (make-instance [a19] of ATRACCIO (nom "London Eye")         (ciutat [c7]) (duracio 0.5) (preu_atraccio 30))
    (make-instance [a20] of ATRACCIO (nom "British Museum")     (ciutat [c7]) (duracio 2.0) (preu_atraccio 0))
    ;; Berlín
    (make-instance [a21] of ATRACCIO (nom "Porta de Brandenburg") (ciutat [c8]) (duracio 0.5) (preu_atraccio 0))
    (make-instance [a22] of ATRACCIO (nom "Mur de Berlín")      (ciutat [c8]) (duracio 1.0) (preu_atraccio 0))
    (make-instance [a23] of ATRACCIO (nom "Illa dels Museus")   (ciutat [c8]) (duracio 2.0) (preu_atraccio 18))
    ;; Bali
    (make-instance [a24] of ATRACCIO (nom "Temple d'Uluwatu")   (ciutat [c9]) (duracio 1.0) (preu_atraccio 3))
    (make-instance [a25] of ATRACCIO (nom "Camps d'arròs d'Ubud") (ciutat [c9]) (duracio 0.5) (preu_atraccio 0))
    (make-instance [a26] of ATRACCIO (nom "Tanah Lot")          (ciutat [c9]) (duracio 0.5) (preu_atraccio 3))
    
    (make-instance [v1] of VIATGE (id_viatge "V1") (tipus_viatge ROMANTIC) (ciutat_desti [c1]) (preu_total 1200) (duracio_dies 7))
    (make-instance [v2] of VIATGE (id_viatge "V2") (tipus_viatge ROMANTIC) (ciutat_desti [c3]) (preu_total 1400) (duracio_dies 5))
    (make-instance [v3] of VIATGE (id_viatge "V3") (tipus_viatge ROMANTIC) (ciutat_desti [c6]) (preu_total 3500) (duracio_dies 10))
    (make-instance [v4] of VIATGE (id_viatge "V4") (tipus_viatge DIVERSIO) (ciutat_desti [c4]) (preu_total 2500) (duracio_dies 10))
    (make-instance [v5] of VIATGE (id_viatge "V5") (tipus_viatge DIVERSIO) (ciutat_desti [c5]) (preu_total 900)  (duracio_dies 7))
    (make-instance [v6] of VIATGE (id_viatge "V6") (tipus_viatge DIVERSIO) (ciutat_desti [c9]) (preu_total 1800) (duracio_dies 12))
    (make-instance [v7] of VIATGE (id_viatge "V7") (tipus_viatge CULTURAL) (ciutat_desti [c2]) (preu_total 950)  (duracio_dies 2))
    (make-instance [v8] of VIATGE (id_viatge "V8") (tipus_viatge CULTURAL) (ciutat_desti [c7]) (preu_total 1300) (duracio_dies 7))
    (make-instance [v9] of VIATGE (id_viatge "V9") (tipus_viatge CULTURAL) (ciutat_desti [c8]) (preu_total 850)  (duracio_dies 5))

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
    (make-instance [t17] of TRANSPORT (ciutat_origen "Tenerife") (ciutat_desti "Madrid")  (tipus_transport AVIO) (preu_transport 150))
    (make-instance [t18] of TRANSPORT (ciutat_origen "Orlando")  (ciutat_desti "Londres") (tipus_transport AVIO) (preu_transport 150))
    (make-instance [t19] of TRANSPORT (ciutat_origen "Maldives") (ciutat_desti "Roma")    (tipus_transport AVIO) (preu_transport 150))
    (make-instance [t20] of TRANSPORT (ciutat_origen "Bali")     (ciutat_desti "Paris")   (tipus_transport AVIO) (preu_transport 150))
)

;; Selecciona totes les atraccions que capiguen en els dies disponibles
;; Retorna atraccions i preu_atraccions
(deffunction selecciona-atraccions-ciutat (?ciutat ?dies_disponibles)
    (bind ?seleccionades (create$))
    (bind ?dies_usats 0)
    (bind ?preu_atraccions 0)
    (do-for-all-instances ((?a ATRACCIO))
        (eq (send ?a get-ciutat) ?ciutat)
        (bind ?dur (send ?a get-duracio))
        (if (<= (+ ?dies_usats ?dur) ?dies_disponibles) then
            (bind ?seleccionades (create$ ?seleccionades (instance-name ?a)))
            (bind ?preu_atraccions (+ ?preu_atraccions (send ?a get-preu_atraccio)))
            (bind ?dies_usats (+ ?dies_usats ?dur))
        )
    )
    (create$ ?preu_atraccions ?seleccionades)
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
            (preu_total    ?preu)
            (ciutat_desti  ?ciutat_desti))
    (object (is-a ALLOTJAMENT)
        (name ?allotjament)
        (ciutat ?ciutat_desti)
        (estrelles ?estrelles)
        (preu_nit ?preu_allotjament)
    )
    (test (<= ?dur ?dies-max))
    (test (<= ?dur ?dies_ciutats_max))
    (test (>= ?dur ?dies_ciutats_min))
    (test (or (eq ?sacrifica TRUE) (>= ?estrelles 3))) ;; aixo s'hauria d'abstraure
    =>  
    (bind $?atraccions-i-preu (selecciona-atraccions-ciutat ?ciutat_desti ?dur))
    (bind ?preu_atraccions (nth$ 1 $?atraccions-i-preu))
    (bind $?atraccions (rest$ $?atraccions-i-preu))
    (bind ?preu_total (+ ?preu (* ?preu_allotjament ?dur) ?preu_atraccions))
    
    ;; Comprovem el preu aquí perquè necessitem preu_atraccions
    (if (or (<= ?preu_total ?pres)
            (and (eq ?sacrifica TRUE) (<= ?preu_total (* ?pres 1.25))))
        then
        (assert (SOLUCIO-PARCIAL (viatges ?v)
                                 (dies_total ?dur)
                                 (cost_total ?preu_total)
                                 (transports (create$))
                                 (atraccions $?atraccions)
                                 (allotjaments ?allotjament)))
    )
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
                     (dies_total ?dies_total)
                     (cost_total ?cost_total)
                     (transports $?ts)
                     (atraccions $?as)
                     (allotjaments $?allotjaments))

    ;; necessitem l'ultim viatge per assegurar que el transport sigui possible
    (object (is-a VIATGE) (name ?ultim_viatge) (ciutat_desti ?ciutat_origen))
    (test (eq ?ultim_viatge (nth$ (length$ $?vs) $?vs)))

    (object (is-a VIATGE) (name ?v)
            (tipus_viatge  ?obj)
            (duracio_dies  ?dur)
            (preu_total    ?preu)
            (ciutat_desti ?ciutat_desti))

    (object (is-a ALLOTJAMENT)
        (name ?allotjament)
        (ciutat ?ciutat_desti)
        (estrelles ?estrelles)
        (preu_nit ?preu_allotjament)
    )
    ;; assegurem que existeix transport que conecti ciutats
    (object (is-a TRANSPORT)
            (name ?id_transport)
            (ciutat_origen ?id_origen)
            (ciutat_desti ?id_desti)
            (tipus_transport ?tipus_transport)
            (preu_transport ?preu_transport))
    
    (test (eq (send ?ciutat_origen get-nom) ?id_origen))
    (test (eq (send ?ciutat_desti get-nom) ?id_desti))

    (test (not (member$ ?tipus_transport $?trans_prohibit)))
    
    ;; nou element no esta a seq
    (test (not (member$ ?v ?vs)))
    
    ;; la suma de dies està per sota del màxim
    (test (<= (+ ?dies_total ?dur) ?dies-max))

    ;; la seq de viatges (ciutats visitades) esta per sota de max
    (test (<= (+ (length$ $?vs) 1) ?ciutats_max))
    
    ;; la parada a la ciutat es coherent amb el rang dels dies per ciutat
    (test (<= ?dur ?dies_ciutats_max))
    (test (>= ?dur ?dies_ciutats_min))

    (test (or (eq ?sacrifica TRUE) (>= ?estrelles 3))) ;; aixo s'hauria d'abstraure
    =>
    (bind $?atraccions-i-preu (selecciona-atraccions-ciutat ?ciutat_desti ?dur))
    (bind ?preu_atraccions (nth$ 1 $?atraccions-i-preu))
    (bind $?atraccions (rest$ $?atraccions-i-preu))
    (bind ?preu_total (+ ?preu (* ?preu_allotjament ?dur) ?preu_atraccions ?preu_transport))
    ;; Comprovem el preu aquí perquè necessitem preu_atraccions
    (if (or (<= (+ ?cost_total ?preu_total) ?pres)
            (and (eq ?sacrifica TRUE) (<= (+ ?cost_total ?preu_total) (* ?pres 1.25))))
        then
        (assert (SOLUCIO-PARCIAL
            (viatges     $?vs ?v)
            (dies_total  (+ ?dies_total ?dur))
            (cost_total  (+ ?cost_total ?preu_total))
            (transports  $?ts ?id_transport)
            (atraccions $?as $?atraccions)
            (allotjaments $?allotjaments ?allotjament))
        )
    )
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
        (transports $?ts)
        (atraccions $?as))
    (test (>= ?dies ?dies-min))
    (test (>= (length $?vs) ?ciutats-min))
    =>
    (printout t crlf "Ruta (" (length$ ?vs) " destins): ")
    ;; Imprimir ciutats
    ;; progn -> es repeteixen les inst del bloc 1 cop per element llista
    (progn$ (?v ?vs)
        (bind ?ciutat (send ?v get-ciutat_desti))
        (printout t (send ?ciutat get-nom) " [" (send ?v get-duracio_dies) "d]")
        (if (< ?v-index (length$ ?vs)) then
            (printout t " → "))
    )
    ;; Imprimir allotjaments
    (printout t crlf "   Allotjaments: ")
    
    ;; Imprimir atraccions 
    (printout t crlf "   Visites: ")
    (progn$ (?v ?vs)
        (bind ?ciutat (send ?v get-ciutat_desti))
        
        ;; Subcadena de as (de la ciutat)
        (bind ?as_ciutat (create$))
        (progn$ (?a ?as)
            (if (eq (send ?a get-ciutat) ?ciutat) then
                (bind ?as_ciutat (create$ ?as_ciutat ?a)))
        )

        (printout t crlf "     · " (send ?ciutat get-nom) ": ")
        (if (> (length$ ?as_ciutat) 0) then        
            (progn$ (?a $?as_ciutat)
                (printout t (send ?a get-nom))
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

    (assert (solucio-trobada))
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


