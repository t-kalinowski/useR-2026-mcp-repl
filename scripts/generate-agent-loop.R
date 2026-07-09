asset_dir <- file.path("assets", "generated")
if (!dir.exists(asset_dir)) {
  dir.create(asset_dir, recursive = TRUE)
}
stopifnot(dir.exists(asset_dir))

png(
  filename = file.path(asset_dir, "agent-iteration-loop.png"),
  width = 11,
  height = 5.8,
  units = "in",
  res = 180,
  bg = "#ffffff"
)

fg <- "#172033"
muted <- "#6b7280"
panel_border <- "#273447"
shadow <- "#d8dee8"
fills <- c("#eef4ff", "#eff8f1", "#fff3e8")
borders <- c("#315cf6", "#26804f", "#c65f1a")

par(
  mar = c(0, 0, 0, 0),
  pty = "m",
  xaxs = "i",
  yaxs = "i",
  family = "sans"
)
plot.new()
plot.window(xlim = c(0, 100), ylim = c(0, 58))

centers <- rbind(
  generate = c(50, 45.5),
  read = c(74, 17),
  step = c(26, 17)
)
half_width <- 13.5
half_height <- 5.8

# text(50, 26.5, "EDA Loop", col = muted, cex = 2.25, font = 2)

edge_gap <- 0.8
arrow_lwd <- 5.5
cycle_center <- c(50, 26.5)
cycle_rx <- 38
cycle_ry <- 23

box_edges <- function(center) {
  stopifnot(length(center) == 2)
  c(
    left = center[1] - half_width,
    right = center[1] + half_width,
    bottom = center[2] - half_height,
    top = center[2] + half_height
  )
}

angle_for_x <- function(x, upper) {
  stopifnot(
    length(x) == 1,
    length(upper) == 1,
    is.logical(upper)
  )

  theta <- acos((x - cycle_center[1]) / cycle_rx) * 180 / pi
  if (upper) theta else -theta
}

angle_for_y <- function(y, right) {
  stopifnot(
    length(y) == 1,
    length(right) == 1,
    is.logical(right)
  )

  theta <- asin((y - cycle_center[2]) / cycle_ry) * 180 / pi
  if (right) theta else -180 - theta
}

draw_cycle_arrow <- function(from_degrees, to_degrees) {
  stopifnot(
    length(from_degrees) == 1,
    length(to_degrees) == 1,
    is.numeric(from_degrees),
    is.numeric(to_degrees),
    from_degrees > to_degrees
  )

  theta <- seq(from_degrees, to_degrees, length.out = 90) * pi / 180
  x <- cycle_center[1] + cycle_rx * cos(theta)
  y <- cycle_center[2] + cycle_ry * sin(theta)

  lines(
    x,
    y,
    col = panel_border,
    lwd = arrow_lwd,
    lend = "round"
  )

  arrow_start <- length(x) - 7
  arrows(
    x[arrow_start],
    y[arrow_start],
    x[length(x)],
    y[length(y)],
    length = 0.14,
    angle = 23,
    code = 2,
    col = panel_border,
    lwd = arrow_lwd
  )
}

generate_box <- box_edges(centers["generate", ])
read_box <- box_edges(centers["read", ])
step_box <- box_edges(centers["step", ])

draw_panel_shadow <- function(center) {
  stopifnot(length(center) == 2)
  rect(
    center[1] - half_width + 0.85,
    center[2] - half_height - 0.85,
    center[1] + half_width + 0.85,
    center[2] + half_height - 0.85,
    col = shadow,
    border = NA
  )
}

draw_panel_body <- function(center, label, fill, border) {
  stopifnot(length(center) == 2, length(label) == 1)
  rect(
    center[1] - half_width,
    center[2] - half_height,
    center[1] + half_width,
    center[2] + half_height,
    col = fill,
    border = border,
    lwd = 3
  )
  text(center[1], center[2], label, col = fg, cex = 1.5, font = 2)
}

draw_panel_shadow(centers["generate", ])
draw_panel_shadow(centers["read", ])
draw_panel_shadow(centers["step", ])

draw_cycle_arrow(
  angle_for_x(generate_box["right"] + edge_gap, upper = TRUE),
  angle_for_y(read_box["top"] + edge_gap, right = TRUE)
)
draw_cycle_arrow(
  angle_for_y(read_box["bottom"] - edge_gap, right = TRUE),
  angle_for_y(step_box["bottom"] - edge_gap, right = FALSE)
)
draw_cycle_arrow(
  angle_for_y(step_box["top"] + edge_gap, right = FALSE),
  angle_for_x(generate_box["left"] - edge_gap, upper = TRUE) - 360
)

draw_panel_body(centers["generate", ], "Run R code", fills[1], borders[1])
draw_panel_body(centers["read", ], "Read output", fills[2], borders[2])
draw_panel_body(centers["step", ], "Pick the next step", fills[3], borders[3])

invisible(dev.off())
