asset_dir <- file.path("assets", "generated")
if (!dir.exists(asset_dir)) {
  dir.create(asset_dir, recursive = TRUE)
}
stopifnot(dir.exists(asset_dir))

png(
  filename = file.path(asset_dir, "two-session-shapes.png"),
  width = 13.5,
  height = 6,
  units = "in",
  res = 150,
  bg = "#ffffff"
)

bg <- "#ffffff"
fg <- "#111827"
muted <- "#475569"
panel <- "#cbd5e1"
code_fill <- "#e5e7eb"
blue <- "#0369a1"
blue_muted <- "#64748b"
blue_fill <- "#e0f2fe"
green <- "#15803d"
green_fill <- "#dcfce7"

par(
  mar = c(0, 0, 0, 0),
  pty = "m",
  xaxs = "i",
  yaxs = "i",
  bg = bg,
  family = "sans"
)
plot.new()
plot.window(xlim = c(0, 100), ylim = c(0, 60))
rect(0, 0, 100, 60, col = bg, border = NA)

x_unit <- par("pin")[1] / diff(par("usr")[1:2])
y_unit <- par("pin")[2] / diff(par("usr")[3:4])
person_radius <- 3.35

draw_box <- function(x0, y0, x1, y1, label, fill, border, cex = 1.05) {
  rect(x0, y0, x1, y1, col = fill, border = border, lwd = 2.6)
  text((x0 + x1) / 2, (y0 + y1) / 2, label, col = fg, cex = cex, font = 2)
}

draw_code <- function(x, y, label, cex) {
  stopifnot(is.character(label), length(label) == 1, cex > 0)
  width <- strwidth(label, units = "user", cex = cex, font = 2, family = "mono")
  height <- strheight(
    label,
    units = "user",
    cex = cex,
    font = 2,
    family = "mono"
  )
  rect(
    x - width / 2 - 0.55,
    y - height / 2 - 0.42,
    x + width / 2 + 0.55,
    y + height / 2 + 0.42,
    col = code_fill,
    border = NA
  )
  text(x, y, label, col = fg, cex = cex, font = 2, family = "mono")
}

draw_inline_code_label <- function(x, y, code, suffix, cex = 1.35) {
  stopifnot(is.character(suffix), length(suffix) == 1)
  code_width <- strwidth(
    code,
    units = "user",
    cex = cex,
    font = 2,
    family = "mono"
  )
  suffix_width <- strwidth(suffix, units = "user", cex = cex, font = 2)
  code_outer_width <- code_width + 1.1
  gap <- 0.12
  left <- x - (code_outer_width + gap + suffix_width) / 2
  draw_code(left + code_outer_width / 2, y, code, cex)
  text(
    left + code_outer_width + gap,
    y,
    suffix,
    col = fg,
    cex = cex,
    font = 2,
    adj = c(0, 0.5)
  )
}

draw_mcp_repl_box <- function(x0, y0, x1, y1, fill, border, cex = 0.95) {
  rect(x0, y0, x1, y1, col = fill, border = border, lwd = 2.6)
  draw_code((x0 + x1) / 2, (y0 + y1) / 2 + 1.45, "mcp-repl", cex = cex)
  text(
    (x0 + x1) / 2,
    (y0 + y1) / 2 - 2.05,
    "R session",
    col = fg,
    cex = cex,
    font = 2
  )
}

draw_person <- function(x, y, border) {
  symbols(
    x,
    y,
    circles = person_radius,
    inches = FALSE,
    add = TRUE,
    fg = border,
    bg = bg,
    lwd = 2.8
  )
  symbols(
    x,
    y + 1.15,
    circles = 0.9,
    inches = FALSE,
    add = TRUE,
    fg = border,
    bg = bg,
    lwd = 2.4
  )
  shoulder_x <- seq(-1.95, 1.95, length.out = 32)
  shoulder_y <- -1.8 + 0.9 * sqrt(pmax(0, 1 - (shoulder_x / 1.95)^2))
  lines(x + shoulder_x, y + shoulder_y, col = border, lwd = 2.4, lend = "round")
  text(x, y - 5.25, "Human", col = fg, cex = 0.8, font = 2)
}

draw_arrow_between <- function(from, to, col, start_gap, end_gap, arrow) {
  stopifnot(length(from) == 2, length(to) == 2)
  dx <- to[1] - from[1]
  dy <- to[2] - from[2]
  data_distance <- sqrt(dx^2 + dy^2)
  visual_distance <- sqrt((dx * x_unit)^2 + (dy * y_unit)^2)
  stopifnot(
    start_gap >= 0,
    end_gap >= 0,
    arrow$shaft_length > 0,
    arrow$edge_gap >= 0,
    arrow$head_length > 0,
    arrow$lwd > 0,
    is.character(arrow$lty),
    length(arrow$lty) == 1,
    data_distance > 0,
    visual_distance > 0
  )
  start_fraction <- start_gap / data_distance
  end_fraction <- 1 - end_gap / data_distance
  edge_gap_fraction <- arrow$edge_gap / visual_distance
  arrow_start <- start_fraction + edge_gap_fraction
  arrow_end <- end_fraction - edge_gap_fraction
  actual_shaft_length <- visual_distance * (arrow_end - arrow_start)
  stopifnot(
    arrow_start < arrow_end,
    isTRUE(all.equal(
      actual_shaft_length,
      arrow$shaft_length,
      tolerance = 1e-3
    ))
  )
  arrows(
    from[1] + arrow_start * dx,
    from[2] + arrow_start * dy,
    from[1] + arrow_end * dx,
    from[2] + arrow_end * dy,
    length = arrow$head_length,
    angle = 24,
    code = 3,
    col = col,
    lwd = arrow$lwd,
    lty = arrow$lty
  )
}

circle_edge_gap <- function(from, to, radius) {
  dx <- to[1] - from[1]
  dy <- to[2] - from[2]
  data_distance <- sqrt(dx^2 + dy^2)
  visual_distance <- sqrt((dx * x_unit)^2 + (dy * y_unit)^2)
  stopifnot(visual_distance > 0)
  data_distance * radius * x_unit / visual_distance
}

box_edge_gap <- function(from, to, half_width, half_height) {
  dx <- to[1] - from[1]
  dy <- to[2] - from[2]
  distance <- sqrt(dx^2 + dy^2)
  stopifnot(distance > 0)
  x_scale <- if (dx == 0) Inf else half_width / abs(dx)
  y_scale <- if (dy == 0) Inf else half_height / abs(dy)
  distance * min(x_scale, y_scale)
}

agent_arrow <- list(
  shaft_length = 0.60,
  edge_gap = 0.08,
  head_length = 0.10,
  lwd = 2.8,
  lty = "solid"
)
optional_agent_arrow <- list(
  shaft_length = 0.60,
  edge_gap = 0.08,
  head_length = 0.10,
  lwd = 2.4,
  lty = "22"
)
ide_arrow <- list(
  shaft_length = 1.00,
  edge_gap = 0.16,
  head_length = 0.11,
  lwd = 2.8,
  lty = "solid"
)

rect(c(2, 52), 7, c(48, 98), 53, border = panel, col = NA, lwd = 1.8)
draw_inline_code_label(25, 49.5, "mcp-repl", ": agent-owned")
text(
  75,
  49.5,
  "Posit Assistant: Human in the loop",
  col = fg,
  cex = 1.35,
  font = 2
)
text(
  c(25, 75),
  c(13, 10.7),
  c(
    "Agent can run unattended.\nHuman only interacts with the Agent.",
    "Human and agent share the same R Session."
  ),
  col = muted,
  cex = c(0.91, 0.98)
)

agent_open <- agent_arrow$shaft_length + 2 * agent_arrow$edge_gap
agent_node <- c(25, 31)
agent_human <- c(
  agent_node[1] - (agent_open / x_unit + person_radius + 7),
  31
)
agent_repl <- c(agent_node[1] + agent_open / x_unit + 7 + 5, 31)
draw_arrow_between(
  agent_human,
  agent_node,
  blue_muted,
  start_gap = circle_edge_gap(agent_human, agent_node, person_radius),
  end_gap = box_edge_gap(agent_node, agent_human, 7, 4.5),
  arrow = optional_agent_arrow
)
draw_arrow_between(
  agent_node,
  agent_repl,
  blue,
  start_gap = box_edge_gap(agent_node, agent_repl, 7, 4.5),
  end_gap = box_edge_gap(agent_repl, agent_node, 5, 6.2),
  arrow = agent_arrow
)
draw_person(agent_human[1], agent_human[2], blue_muted)
draw_box(18, 26.5, 32, 35.5, "Agent", blue_fill, blue)
draw_mcp_repl_box(37, 24.8, 47, 37.2, blue_fill, blue)

ide_open <- ide_arrow$shaft_length + 2 * ide_arrow$edge_gap
triangle_top_y <- 41.5
triangle_left <- c(62, triangle_top_y)
triangle_right <- c(
  triangle_left[1] + ide_open / x_unit + person_radius + 8.2,
  triangle_top_y
)
triangle_bottom <- c(71.70876, 22.78294)

draw_arrow_between(
  triangle_left,
  triangle_right,
  green,
  start_gap = circle_edge_gap(triangle_left, triangle_right, person_radius),
  end_gap = box_edge_gap(triangle_right, triangle_left, 8.2, 4.4),
  arrow = ide_arrow
)
draw_arrow_between(
  triangle_right,
  triangle_bottom,
  green,
  start_gap = box_edge_gap(triangle_right, triangle_bottom, 8.2, 4.4),
  end_gap = box_edge_gap(triangle_bottom, triangle_right, 8.8, 4.2),
  arrow = ide_arrow
)
draw_arrow_between(
  triangle_bottom,
  triangle_left,
  green,
  start_gap = box_edge_gap(triangle_bottom, triangle_left, 8.8, 4.2),
  end_gap = circle_edge_gap(triangle_left, triangle_bottom, person_radius),
  arrow = ide_arrow
)
draw_person(triangle_left[1], triangle_left[2], green)
draw_box(
  triangle_right[1] - 8.2,
  triangle_right[2] - 4.4,
  triangle_right[1] + 8.2,
  triangle_right[2] + 4.4,
  "Assistant",
  green_fill,
  green
)
draw_box(
  triangle_bottom[1] - 8.8,
  triangle_bottom[2] - 4.2,
  triangle_bottom[1] + 8.8,
  triangle_bottom[2] + 4.2,
  "IDE R session",
  green_fill,
  green,
  cex = 0.98
)

invisible(dev.off())
