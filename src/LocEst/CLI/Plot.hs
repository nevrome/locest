{-# LANGUAGE QuasiQuotes #-}
module LocEst.CLI.Plot where

import Language.R.Instance as R
import Language.R.QQ

data PlotOptions = PlotOptions {
    _plotInFile   :: FilePath,
    _plotOutFile :: FilePath
}

runPlot :: PlotOptions -> IO ()
runPlot (PlotOptions inFile outFile) = do
    R.withEmbeddedR R.defaultConfig $ R.runRegion $ tilePlot inFile outFile

tilePlot :: FilePath -> FilePath -> R s ()
tilePlot inFile outFile = do
    _ <- [r|
      message(R.version$version.string)
      library(magrittr)
      library(ggplot2)
      message("Reading file")
      search <- readr::read_tsv(inFile_hs)
      message("Preparing plot")
      {
      search %>%
        ggplot() +
        ggrastr::geom_tile_rast(
          data = search,
          mapping = aes(
            x, y,
            fill = probability
          ),
          raster.dpi = 150
        ) +
        scale_fill_gradientn(
          values = c(0, 0.05, 0.1, 0.2, 0.5, 1),
          limits = c(0, 0.0004),
          oob = scales::squish,
          colors = c("white", wesanderson::wes_palette("Zissou1")[c(2,1,3,4,5)]),
          labels = scales::comma
        ) +
        theme_bw() +
        guides(
          fill = guide_colorbar(
            title = "Similarity probability      ", barwidth = 25, barheight = 0.6
          )
        ) +
        theme(
          legend.position = "bottom",
          legend.title = element_text(face = "bold"),
          panel.background = element_rect(fill = "lightgrey")
        )
      } %>%
      ggsave(
        outFile_hs,
        plot = .,
        device = "png",
        scale = 0.5,
        dpi = 300,
        width = 500, height = 325, units = "mm",
        limitsize = F
      )
    |]
    return ()
