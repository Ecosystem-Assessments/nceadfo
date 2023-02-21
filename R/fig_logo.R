#' Project logo
#'
#' Function to generate project logo
#'
#' @keywords figure
#'
#' @export
#'

fig_logo <- function() {
  out <- here::here("figures","logo")
  chk_create(out)
  dat <- stars::read_stars(here::here("output","cea","cea_network-2016_2021.tif"))
  
  # Thanks to hexSticker package: https://github.com/GuangchuangYu/hexSticker
  hexSticker::sticker(
    ~image(dat, col = viridis::viridis(100)),
    package="nceadfo",
    p_size=20,
    s_x=1,
    s_y=.85,
    p_y = 1.5,
    s_width=1.5,
    s_height=1.5,
    p_family = "serif",
    h_fill = "#a6b6c8",
    h_color = "#0f3c4f",
    p_color = "#0f3c4f",
    filename=here::here(out, "logo.png")
  )

  man <- here::here("man","figures")
  chk_create(man)
  file.copy(
    from = here::here(out,"logo.png"), 
    to = here::here(man, "logo.png"), 
    overwrite = TRUE
  )
}
