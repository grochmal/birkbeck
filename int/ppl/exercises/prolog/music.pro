song(doors, the_end).
song(zeppelin, stairway_to_heaven).
song(zeppelin, riverside_blues).
song(acdc, highway_to_hell).

band(zeppelin, page).
band(zeppelin, plant).
band(zeppelin, jones).
band(zeppelin, bonham).
band(doors, morrison).
band(doors, manzanek).
band(doors, densmore).
band(doors, krieger).

sing(X, Y) :- song(Z, X), band(Z, Y).

