 ;;; Definició de les classes principals basades en l'ontologia

(defclass USUARI (is-a USER)
    ;; =============================
    ;; PROBLEMA CONCRET (PC)
    ;; =============================
    (slot edat (type INTEGER))
    (slot persones_viatjant (type INTEGER))
    (slot viatja_amb_nens (type SYMBOL) (allowed-values TRUE FALSE))

    (slot pressupost_max (type INTEGER))
    (slot dies_min (type INTEGER))
    (slot dies_max (type INTEGER))
    (slot ciutats_min (type INTEGER))
    (slot ciutats_max (type INTEGER))
    (slot dies_ciutats_min (type INTEGER))
    (slot dies_ciutats_max (type INTEGER))
    (multislot transport_prohibit (type SYMBOL) (allowed-values TREN AVIO VAIXELL AUTOBUS COTXE CAP))
    (slot qualitat_min_estrelles (type INTEGER) (default 1))

    (slot sacrifica_qualitat (type SYMBOL) (allowed-values TRUE FALSE))
    (slot esdev_especial (type SYMBOL) (allowed-values CAP BODA FI_DE_CURS ANIVERSARI JUBILACIO) (default CAP))
    (slot nivell_cultural (type SYMBOL) (allowed-values BAIX MITJA ALT) (default MITJA))
    (slot llocs_menys_coneguts (type SYMBOL) (allowed-values TRUE FALSE))

    ;; =============================
    ;; PROBLEMA ABSTRACTE (PA)
    ;; =============================
    (slot rang_edat (type SYMBOL) (allowed-values MENOR JOVE MITJANA-EDAT GRAN DESCONEGUT) (default DESCONEGUT))
    (slot perfil_deduit (type SYMBOL) (allowed-values INDIVIDUAL PARELLA FAMILIA GRUP DESCONEGUT) (default DESCONEGUT))
    (slot nivell_pressupost (type SYMBOL) (allowed-values BAIX NORMAL ALT DESCONEGUT) (default DESCONEGUT))
    (slot actitud_despesa (type SYMBOL) (allowed-values ESTALVIADOR EXIGENT DESCONEGUT) (default DESCONEGUT))
)


(defclass CIUTAT (is-a USER)
    (slot nom (type STRING))
    (multislot tematica (type SYMBOL) (allowed-values MEDITERRANIA METROPOLI EXOTICA MUNTANYA COSTA_PLATJA HISTORICA))    
    (multislot tipus_viatge (type SYMBOL) (allowed-values ROMANTIC CULTURAL DESCANS DIVERSIO AVENTURA))
)


(defclass TRANSPORT (is-a USER)
    (slot ciutat_origen (type STRING))
    (slot ciutat_desti (type STRING))
    (slot tipus_transport (type SYMBOL) (allowed-values TREN AVIO VAIXELL AUTOBUS COTXE CAP))
    (slot preu_transport (type INTEGER))
)


(defclass ATRACCIO (is-a USER)
    (slot nom (type STRING))
    (slot ciutat (type INSTANCE) (allowed-classes CIUTAT))
    (slot duracio (type FLOAT)) ;; en dies (0.5, 1, 2)
    (slot preu_atraccio (type INTEGER))
    (slot coneguda (type SYMBOL) (allowed-values TRUE FALSE) (default TRUE))
    (slot importancia (type SYMBOL) (allowed-values IMPRESCINDIBLE RECOMANABLE SECUNDARIA) (default RECOMANABLE))
)


(defclass ALLOTJAMENT (is-a USER)
    (slot nom (type STRING))
    (slot ciutat (type INSTANCE) (allowed-classes CIUTAT))
    (slot tipus (type SYMBOL) (allowed-values HOTEL APARTAMENT ALBERG RESORT CAMPING))
    (slot estrelles (type INTEGER))
    (slot preu_nit (type INTEGER))
) 