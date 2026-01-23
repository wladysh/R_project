# ============================================================
# Hierarchisches Clustering (Ward): Supermarkt (12 Produkte)==========================

# -------------------------
# 1) Produkte + "Ground-Truth"-Kategorien 
# -------------------------


kategorie <- c(
  rep("Obst", 2),
  rep("Gemüse", 2),
  rep("Milchprodukte", 2),
  rep("Fleisch/Fisch", 2),
  rep("Hygiene", 2),
  rep("Reinigung", 2)
)
names(kategorie) <- produkte

# -------------------------
# 2) Feature-Matrix (0..10), bewusst so gewählt, dass die "Supermarkt-Logik" entsteht
# -------------------------
# Essbarkeit      : 10 = essbar, 0 = nicht essbar
# Kühlbedarf      : 10 = Kühlschrank nötig
# Süße            : hoch bei Obst
# GemüseFrische   : hoch bei Gemüse (trennt Obst vs Gemüse)
# Tierisch        : hoch bei Milchprodukten und besonders Fleisch/Fisch
# Körperpflege    : hoch bei Hygieneartikeln
# Reinigungskraft : hoch bei Reinigungsmitteln

daten <- data.frame(
  Essbarkeit      = c(10,10, 10,10, 10,10, 10,10,  0, 0,  0, 0),
  Kuehlbedarf     = c( 0, 0,  0, 0, 10,10, 10,10,  0, 0,  0, 0),
  Suesse          = c( 8, 9,  1, 0,  2, 3,  0, 0,  0, 1,  0, 0),
  GemueseFrische  = c( 2, 1,  9,10,  1, 1,  0, 0,  0, 0,  0, 0),
  Tierisch        = c( 0, 0,  0, 0,  7, 6, 10,10,  0, 0,  0, 0),
  Koerperpflege   = c( 0, 0,  0, 0,  0, 0,  0, 0, 10,10,  0, 0),
  Reinigungskraft = c( 0, 0,  0, 0,  0, 0,  0, 0,  1, 0, 10, 9)
)

rownames(daten) <- produkte







cat("Feature-Matrix (Rohdaten):\n")
print(daten)

# -------------------------
# 3) Vorverarbeitung: Skalierung (wichtig bei unterschiedlichen Skalen)
# -------------------------
daten_scaled <- scale(daten)

# -------------------------
# 4) Distanz + hierarchisches Clustering
# -------------------------
distanz <- dist(daten_scaled, method = "euclidean")

# Ward.D2 ist für numerische Features oft sehr "sauber" (kompakte Cluster)
hc <- hclust(distanz, method = "average")
# Alternative (falls gewünscht): hc <- hclust(distanz, method = "complete")

# Optional: Achse umdrehen (Root unten, Blätter oben)
root_unten <- FALSE   # TRUE setzen, wenn du "Wurzel unten, Blätter oben" willst
hmax <- max(hc$height)

# -------------------------
# 5) Plot-Funktion: Denndrogramm + Cut + Cluster-Ausgabe
# -------------------------
zeige_schnitt <- function(k, rahmen_farbe) {
  yl <- if (root_unten) c(hmax, 0) else c(0, hmax)
  
  plot(
    hc,
    main = paste0("Dendrogramm (k = ", k, ")"),
    xlab = "",
    ylab = "Unähnlichkeit (Distanz)",
    sub  = "Distanz: euclid. auf scale(); Linkage: Ward.D2",
    hang = -1,
    ylim = yl
  )
  
  rect.hclust(hc, k = k, border = rahmen_farbe)
  
  cluster <- cutree(hc, k = k)
  
  cat("\n=============================\n")
  cat("Clusterzuordnung für k =", k, "\n")
  print(split(names(cluster), cluster))
  
  cat("\nKategorie x Cluster (zur Interpretation):\n")
  print(table(kategorie, cluster))
}

# -------------------------
# 6) Drei Schnitte nebeneinander: k = 2 / 4 / 6
# -------------------------
op <- par(no.readonly = TRUE)
on.exit(par(op), add = TRUE)

par(mfrow = c(1, 3), mar = c(7, 4, 4, 1) + 0.1)

zeige_schnitt(2, "red")        # grob: Essbar vs Nicht essbar
zeige_schnitt(4, "blue")       # Zonen: frisch / Kühlung / Bad / Küche-Reinigung
zeige_schnitt(6, "darkgreen")  # die "idealen Paare": 6 Kategorien à 2 Produkte
zeige_schnitt(10, "pink") 
par(mfrow = c(1, 1))

