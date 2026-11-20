## =============================================================================
## Tree carbon-allocation rings: Growth / Defense / Reproduction across
## life stages (Seedling -> Juvenile tree -> Mature tree)
##
## THE KEY IDEA: each segment's radial thickness is DIRECTLY PROPORTIONAL to
## its percentage value (half_width = pct * constant, no floor, no offset),
## on ONE shared scale across all three rings. So a 25% segment is exactly
## 25/13 times as thick as a 13% segment -- the ratio you see is the data.
##
## Pure base-R graphics. No packages required.
## =============================================================================

## ---- 1. Colors --------------------------------------------------------------
col_growth  <- "#4F7FB0"
col_defense <- "#E08214"
col_repro   <- "#4F9D5B"

## ---- 2. Geometry helpers -----------------------------------------------------

deg2rad <- function(deg) deg * pi / 180

# Point(s) at radius r, angle 'deg' measured CLOCKWISE from 12 o'clock
circle_xy <- function(cx, cy, r, deg) {
  rad <- deg2rad(deg)
  data.frame(x = cx + r * sin(rad), y = cy + r * cos(rad))
}

arc_xy <- function(cx, cy, r, deg1, deg2, n = 200) {
  circle_xy(cx, cy, r, seq(deg1, deg2, length.out = n))
}

# A filled arc "band": outer edge at r + half_width, inner edge at r - half_width
draw_band <- function(cx, cy, r, deg1, deg2, col, half_width) {
  outer <- arc_xy(cx, cy, r + half_width, deg1, deg2)
  inner <- arc_xy(cx, cy, r - half_width, deg2, deg1)
  polygon(c(outer$x, inner$x), c(outer$y, inner$y), col = col, border = NA)
}

## ---- 3. Data ------------------------------------------------------------------

# Four life stages, partitioning the full 360 degrees with NO gaps between
# them -- each stage's end angle is the next stage's start angle exactly,
# so within a ring, adjacent segments touch rather than leaving a break.
stages <- list(
  Seed     = c(0,   15),
  Seedling = c(15,  95),
  Juvenile = c(95,  220),
  Mature   = c(220, 360)
)

# Growth and defense have no allocation at the Seed stage (a seed itself
# neither grows nor defends), so they only carry Seedling/Juvenile/Mature.
growth_data <- data.frame(
  stage = c("Seedling", "Juvenile", "Mature"),
  pct   = c(46, 25, 5)
)
defense_data <- data.frame(
  stage = c("Seedling", "Juvenile", "Mature"),
  pct   = c(13, 6, 3)
)
# Reproduction's 2% belongs to the Seed stage (it's the cost of producing
# the seed itself), not Juvenile -- and the 1% is at Mature tree.
repro_data <- data.frame(
  stage = c("Seed", "Mature"),
  pct   = c(2, 1)
)
# Note: 46+25+5+13+6+3+2+1 = 101, matching the slight rounding in the
# original figure (76% + 22% + 3% = 101%). Adjust freely -- the scale below
# re-derives itself from whatever values you put in.

## ---- 4. Thickness scale: half-width is DIRECTLY PROPORTIONAL to pct ---------
## This is the part that matters most. There is no floor and no offset --
## half_width = pct * unit_scale, full stop. That means the ratio between any
## two segments' thicknesses on the page is exactly the ratio of their
## percentages (a 25% segment is precisely 25/13 times as thick as a 13% one),
## on one shared scale across all three rings. Nothing is propped up or
## exaggerated for legibility.
##
## The honest cost of that: a 1% segment will render as a near-hairline,
## because it genuinely is tiny relative to 46%. If you need small segments
## to stay legible, the transparent way to do that is to add a small
## reference scale bar showing what a given % looks like, rather than
## distorting the bands themselves -- ask if you'd like that added.

all_pct    <- c(growth_data$pct, defense_data$pct, repro_data$pct)
max_pct    <- max(all_pct)
max_hw     <- 0.10
unit_scale <- max_hw / max_pct    # half-width "per percentage point"

pct_to_halfwidth <- function(pct) {
  pct * unit_scale
}

## ---- 5. Ring-drawing routine ---------------------------------------------------

draw_ring <- function(data, radius, label_offset, col) {
  for (i in seq_len(nrow(data))) {
    b  <- stages[[data$stage[i]]]
    hw <- pct_to_halfwidth(data$pct[i])
    draw_band(0, 0, radius, b[1], b[2], col, hw)
    p <- circle_xy(0, 0, radius + label_offset, mean(b))
    text(p$x, p$y, paste0(data$pct[i], "%"), col = col, cex = 0.95, font = 2, xpd = TRUE)
  }
}

## ---- 6. Thickness legend (calibration key) -------------------------------------
## A small set of reference bars, using the exact same pct_to_halfwidth()
## function as the rings above -- so the legend can't drift out of sync with
## what's actually drawn. Pick whatever round reference values are useful;
## they don't need to match the real data values.

draw_thickness_legend <- function(x_center, y_center, values = c(5, 15, 30, 45),
                                   bar_width = 0.30, gap = 0.36) {
  n  <- length(values)
  xs <- x_center + (seq_len(n) - (n + 1) / 2) * gap

  text(x_center, y_center + 0.18, "line thickness = % (to scale)",
       cex = 0.85, font = 3, col = "grey30", xpd = TRUE)

  for (i in seq_len(n)) {
    hw <- pct_to_halfwidth(values[i])
    rect(xs[i] - bar_width / 2, y_center - hw, xs[i] + bar_width / 2, y_center + hw,
         col = "grey45", border = NA, xpd = TRUE)
    text(xs[i], y_center - hw - 0.06, paste0(values[i], "%"),
         cex = 0.85, col = "grey30", xpd = TRUE)
  }
}

## ---- 7. Category legend & life-stage phase labels --------------------------------
## Bringing back the GROWTH / DEFENSE / REPRODUCTION key, plus simple text
## markers for the three phases the rings actually carry data for. No icons,
## no leader lines, no outer ring -- just labels.

draw_category_legend <- function(x, y_top, line_gap = 0.12) {
  text(x, y_top,             paste0("Growth (",       sum(growth_data$pct),  "%)"),
       col = col_growth,  font = 2, cex = 1.05, adj = 0, xpd = TRUE)
  text(x, y_top - line_gap,  paste0("Defense (",      sum(defense_data$pct), "%)"),
       col = col_defense, font = 2, cex = 1.05, adj = 0, xpd = TRUE)
  text(x, y_top - 2*line_gap,paste0("Reproduction (", sum(repro_data$pct),   "%)"),
       col = col_repro,   font = 2, cex = 1.05, adj = 0, xpd = TRUE)
}

add_phase_label <- function(deg, label, adj, radius = 1.00) {
  p <- circle_xy(0, 0, radius, deg)
  text(p$x, p$y, label, adj = adj, cex = 1.0, font = 2, col = "black", xpd = TRUE)
}

## ---- 8. Plot --------------------------------------------------------------------

plot_allocation_rings <- function() {

  par(mar = c(0.5, 0.5, 0.5, 0.5), bg = "white")
  plot(NA, xlim = c(-1.4, 1.4), ylim = c(-1.45, 1.35), asp = 1,
       axes = FALSE, xlab = "", ylab = "")

  R_growth  <- 0.80
  R_defense <- 0.55
  R_repro   <- 0.32

  draw_ring(growth_data,  R_growth,  0.19, col_growth)
  draw_ring(defense_data, R_defense, 0.085, col_defense)
  draw_ring(repro_data,   R_repro,   0.075, col_repro)

  draw_category_legend(-1.35, 1.25)

  add_phase_label(7,   "Seed",          c(0.5, 0))
  add_phase_label(57,  "Seedling",      c(-0.5,   0))
  add_phase_label(115, "Juvenile",      c(0,   0.5))
  add_phase_label(240, "Mature tree",   c(1,   0.5))
}


pdf("Figure2.pdf")
plot_allocation_rings()
dev.off()




