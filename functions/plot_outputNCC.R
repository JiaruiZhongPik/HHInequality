

plot_outputNCC <- function(){
  theme_set(theme_minimal(base_family = "Arial", base_size = 7))
  
  nature_dims <- function(type = c("research_1col","research_2col"),
                          caption_words = 150) {
    type <- match.arg(type)
    width_mm <- switch(type, research_1col = 88, research_2col = 180)
    max_height_mm <-
      if (caption_words < 50 && type == "research_1col") 220 else
        if (caption_words < 50 && type == "research_2col") 225 else
          if (caption_words < 150 && type == "research_1col") 180 else
            if (caption_words < 150 && type == "research_2col") 210 else
              if (caption_words < 300 && type == "research_1col") 130 else
                185   # research_2col, 150–299
    list(width_mm = width_mm, max_height_mm = max_height_mm)
  }
  
  nature_save_pdf <- function(plot, dir, file,
                              width_mm, height_mm,
                              max_height_mm = NULL,
                              use_cairo = TRUE,
                              bg = "white") {
    if (!dir.exists(dir)) dir.create(dir, recursive = TRUE)
    full_path <- file.path(dir, file)
    if (!is.null(max_height_mm) && height_mm > max_height_mm) {
      warning(sprintf("Height %.1f mm exceeds Nature max %.1f mm.", height_mm, max_height_mm))
    }
    ggsave(
      filename = full_path, plot = plot,
      width = width_mm/25.4, height = height_mm/25.4, units = "in",
      device = if (use_cairo) cairo_pdf else "pdf",
      bg = bg, limitsize = FALSE
    )
    message("Saved PDF: ", normalizePath(full_path, mustWork = FALSE))
    invisible(full_path)
  }
  
  nature_export_multipanel_both <- function(plots, stem, dir,
                                            type = c("research_1col","research_2col","review_1col","review_2col","review_3col"),
                                            caption_words = 150,
                                            layout = c("ncol","nrow"), n = 2,
                                            rel_widths = NULL, rel_heights = NULL,
                                            custom_height_mm = NULL,
                                            tag_levels = "a", tag_prefix = "", tag_suffix = "",
                                            tag_fontfamily = "Arial", tag_face = "bold", tag_size = 7,
                                            panel_spacing_mm = 2, bg = "white",
                                            word_format = c("svg","emf","png"), png_dpi = 300,
                                            collect_guides = TRUE, legend_position = "bottom", legend_direction = "horizontal") {
    
    type <- match.arg(type); layout <- match.arg(layout); word_format <- match.arg(word_format)
    if (!is.list(plots)) plots <- list(plots)
    if (!dir.exists(dir)) dir.create(dir, recursive = TRUE)
    
    dims <- nature_dims(type, caption_words)
    height_mm <- if (is.null(custom_height_mm)) dims$max_height_mm else custom_height_mm
    
    wrap <- if (layout == "ncol") wrap_plots(plots, ncol = n) else wrap_plots(plots, nrow = n)
    if (!is.null(rel_widths))  wrap <- wrap + plot_layout(widths  = rel_widths)
    if (!is.null(rel_heights)) wrap <- wrap + plot_layout(heights = rel_heights)
    wrap <- wrap + plot_layout(guides = if (collect_guides) "collect" else "keep") &
      theme(plot.margin = unit(rep(panel_spacing_mm, 4), "mm")) &
      theme(legend.position = legend_position,
            legend.direction = legend_direction,
            legend.box = if (legend_direction == "horizontal") "horizontal" else "vertical")
    wrap <- wrap + plot_annotation(tag_levels = tag_levels, tag_prefix = tag_prefix, tag_suffix = tag_suffix) &
      theme(plot.tag.position = c(0,1),
            plot.tag = element_text(family = tag_fontfamily, face = tag_face, size = tag_size, hjust = 0, vjust = 1))
    
    # 1) PDF
    nature_save_pdf(
      plot = wrap,
      dir  = dir,
      file = paste0(stem, ".pdf"),
      width_mm  = dims$width_mm,
      height_mm = height_mm,
      max_height_mm = dims$max_height_mm,
      bg = bg
    )
    
    # 2) Word copy path
    word_path <- file.path(dir, paste0(stem, ".", match.arg(word_format,
                                                            c("svg","emf","png"))))
    
    if (word_format == "svg") {
      if (!requireNamespace("svglite", quietly = TRUE)) stop("svglite not installed.")
      svglite::svglite(word_path, width = dims$width_mm/25.4, height = height_mm/25.4, bg = bg)
      print(wrap); dev.off()
    } else if (word_format == "emf") {
      if (.Platform$OS.type == "windows") {
        grDevices::win.metafile(word_path, width = dims$width_mm/25.4, height = height_mm/25.4)
        print(wrap); dev.off()
      } else if (requireNamespace("devEMF", quietly = TRUE)) {
        devEMF::emf(file = word_path, width = dims$width_mm/25.4, height = height_mm/25.4, bg = bg)
        print(wrap); dev.off()
      } else {
        warning("devEMF not available; falling back to PNG.")
        word_format <- "png"
        word_path <- file.path(dir, paste0(stem, ".png"))
        if (!requireNamespace("ragg", quietly = TRUE)) stop("ragg not installed for PNG output.")
        ragg::agg_png(word_path,
                      width  = round(dims$width_mm/25.4 * png_dpi),
                      height = round(height_mm/25.4 * png_dpi),
                      units = "px", res = png_dpi, background = bg)
        print(wrap); dev.off()
      }
    }
    if (word_format == "png") {
      if (!requireNamespace("ragg", quietly = TRUE)) stop("ragg not installed for PNG output.")
      ragg::agg_png(word_path,
                    width  = round(dims$width_mm/25.4 * png_dpi),
                    height = round(height_mm/25.4 * png_dpi),
                    units = "px", res = png_dpi, background = bg)
      print(wrap); dev.off()
    }
    
    message("Saved Word copy: ", normalizePath(word_path, mustWork = FALSE))
    invisible(list(pdf = file.path(dir, paste0(stem, ".pdf")), word = word_path))
  }
  
  `%or%` <- function(x, y) if (is.null(x)) y else x
  
  extract_panels <- function(x, plot_key = "plot", width_key = "width", height_key = "height") {
    out <- lapply(x, function(el) {
      if (is.list(el) && !is.null(el[[plot_key]])) {
        list(plot = el[[plot_key]],
             width = el[[width_key]] %or% 1,
             height = el[[height_key]] %or% NA_real_)
      } else NULL
    })
    out <- Filter(Negate(is.null), out)
    if (length(out) == 0) stop("No panels found with key '", plot_key, "'.")
    out
  }
  
  nature_export_from_list <- function(x, stem, dir = ".",
                                      type = c("research_1col","research_2col","review_1col","review_2col","review_3col"),
                                      caption_words = 150, ncol = 2, custom_height_mm = NULL,
                                      tag_levels = "a", panel_spacing_mm = 2,
                                      word_format = c("svg","emf","png"), png_dpi = 300,
                                      collect_guides = TRUE, legend_position = "bottom", legend_direction = "horizontal") {
    
    type <- match.arg(type); word_format <- match.arg(word_format)
    if (!dir.exists(dir)) dir.create(dir, recursive = TRUE)
    
    panels <- extract_panels(x)
    plots  <- lapply(panels, `[[`, "plot")
    rel_w  <- vapply(panels, function(e) e$width %or% 1, numeric(1))
    
    nature_export_multipanel_both(
      plots = plots,
      stem  = stem,
      dir   = dir,
      type  = type,
      caption_words = caption_words,
      layout = "ncol", n = ncol,
      rel_widths = rel_w,
      custom_height_mm = custom_height_mm,
      tag_levels = tag_levels,
      panel_spacing_mm = panel_spacing_mm,
      word_format = word_format,
      png_dpi = png_dpi,
      collect_guides = collect_guides,
      legend_position = legend_position,
      legend_direction = legend_direction
    )
  }
  
  
  
  # example for use
  figure1 <- plot_output(outputPath = outputPath, 
                         plotdataWelf  = decileWelfChange, 
                         data2 = decileConsShare, 
                         data3 = data, 
                         plotdataIneq = ineq,
                         exampleReg = 'IND',
                         plotlist = c('welfByPeriod','ineqWorld'),
                         micro_model = micro_model, fixed_point = fixed_point, isDisplay= F, isExport = F)
  
  
  
  
  figure2 <- plot_output(outputPath = outputPath, 
                         plotdataWelf  = decileWelfChange, 
                         data2 = decileConsShare, 
                         data3 = data, 
                         plotdataIneq = ineq,
                         exampleReg = 'IND',
                         plotlist = c('ineqGlobalBetweenWithinTheilL','ineqGlobalWithinRegTheilL'),
                         micro_model = micro_model, fixed_point = fixed_point, isDisplay= F, isExport = F)
  
  
  
  figure3 <- plot_output(outputPath = outputPath, 
                         plotdataWelf  = decileWelfChange, 
                         data2 = decileConsShare, 
                         data3 = data, 
                         plotdataIneq = ineq,
                         exampleReg = 'IND',
                         plotlist = c('ineqTheilLRegBySecbyScen'),
                         micro_model = micro_model, fixed_point = fixed_point, isDisplay= F, isExport = F)
  
  
  
  figure4 <- plot_output(outputPath = outputPath, 
                         plotdataWelf  = decileWelfChange, 
                         data2 = decileConsShare, 
                         data3 = data, 
                         plotdataIneq = ineq,
                         exampleReg = 'IND',
                         plotlist = c('ineqWorldWithTransf'),
                         micro_model = micro_model, fixed_point = fixed_point, isDisplay= F, isExport = F)
  
  
  
  nature_export_from_list(
    figure1,
    dir = "figure/ncc",
    stem = "Figure1",
    type = "research_2col",
    caption_words = 120,
    ncol =1,
    custom_height_mm = 160,
    word_format = "png"     # or "emf"/"png"
  )
  
  nature_export_from_list(
    figure2,
    dir = "figure/ncc",
    stem = "Figure2",
    type = "research_2col",
    caption_words = 120,
    ncol = 1,
    custom_height_mm = 160,
    word_format = "png"     # or "emf"/"png"
  )
  
  nature_export_from_list(
    figure3,
    dir = "figure/ncc",
    stem = "Figure3",
    type = "research_2col",
    caption_words = 120,
    ncol = 2,
    custom_height_mm = 160,
    word_format = "png"     # or "emf"/"png"
  )
  
  nature_export_from_list(
    figure4,
    dir = "figure/ncc",
    stem = "Figure4",
    type = "research_2col",
    caption_words = 120,
    ncol = 1,
    custom_height_mm = 80,
    word_format = "png"     # or "emf"/"png"
  )
  
}


