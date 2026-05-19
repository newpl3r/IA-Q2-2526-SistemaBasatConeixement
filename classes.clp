;;; Definició de les classes principals basades en l'ontologia

(defclass USUARI (is-a USER)
    (slot pressupost_max (type INTEGER))
    (slot dies_disponibles (type INTEGER))
    (slot viatja_amb_nens (type SYMBOL) (allowed-values TRUE FALSE))
    (slot sacrifica_qualitat (type SYMBOL) (allowed-values TRUE FALSE))
    (slot perfil_deduit (type SYMBOL) (allowed-values INDIVIDUAL PARELLA FAMILIA GRUP DESCONEGUT) (default DESCONEGUT))
    (slot objectiu_deduit (type SYMBOL) (allowed-values ROMANTIC CULTURAL DESCANS DIVERSIO DESCONEGUT) (default DESCONEGUT))
)

(defclass CIUTAT (is-a USER)
    (slot nom (type STRING))
    (slot tematica (type SYMBOL) (allowed-values MEDITERRANIA EUROPEA_GRAN NATURALESA))
    (slot cost_diari_estimat (type INTEGER))
)

(defclass VIATGE (is-a USER)
    (slot id_viatge (type STRING))
    (slot tipus_viatge (type SYMBOL) (allowed-values ROMANTIC CULTURAL DESCANS DIVERSIO))
    (slot ciutat_desti (type STRING))
    (slot preu_total (type INTEGER))
    (slot duracio_dies (type INTEGER))
)