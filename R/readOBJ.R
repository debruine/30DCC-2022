#con <- "book/data/lisa_neutral.obj"

readOBJ <- function (con, ...) {
  lines <- readLines(con)
  instrs <- sub(" .*", "", lines)
  vertices <- read.table(textConnection(lines[instrs == "v"]),
                         col.names = c("instr", "x", "y", "z"),
                         colClasses = c(instr = "character",
                                        x = "numeric",
                                        y = "numeric",
                                        z = "numeric"))
  vertices <- with(vertices, rbind(x, y, z))
  subset <- lines[instrs == "vn"]
  if (length(subset)) {
    fields <- count.fields(textConnection(subset))
    if (!all(fields == 4))
      stop("Normals must have 4 fields")
    vn <- read.table(textConnection(subset),
                     col.names = c("instr", "x", "y", "z"),
                     colClasses = c(instr = "character",
                                    x = "numeric", y = "numeric", z = "numeric"))
    vn <- rbind(t(vn[, 2:4]), 1)
  } else {
    vn <- matrix(numeric(), nrow = 4, ncol = 0)
  }
  subset <- lines[instrs == "vt"]
  if (length(subset)) {
    fields <- count.fields(textConnection(subset))
    if (length(unique(fields)) != 1)
      stop("Textures must have consistent field count")
    fields <- fields[1]
    colClasses <- c("character", rep("numeric", fields -
                                       1))
    vt <- read.table(textConnection(subset), colClasses = colClasses)
    if (fields == 2)
      vt <- cbind(vt, 0)
    vt <- t(vt[, 2:3])
  } else {
    vt <- matrix(numeric(), nrow = 2, ncol = 0)
  }
  polys <- gsub("/[^ ]*", "", lines[instrs == "f"])
  polys <- strsplit(polys, " ")
  polys <- lapply(polys, function(poly) as.numeric(poly[-1]))
  normals <- gsub("(^| *)([^/ ]*/?){0,2}", "\\1", lines[instrs == "f"])
  normals <- strsplit(normals, " ")
  normals <- lapply(normals, function(normal) as.numeric(normal[nchar(normal) > 0]))
  textures <- gsub("(^| *)([^/ ]*/?){0,1}", "\\1", lines[instrs == "f"])
  textures <- gsub("/[^ ]*", "", textures)
  textures <- strsplit(textures, " ")
  textures <- lapply(textures, function(texture) as.numeric(texture[nchar(texture) > 0]))
  nverts <- sapply(polys, length)
  nnorms <- sapply(normals, length)
  ntexts <- sapply(textures, length)
  hasnormals <- nnorms == nverts
  hastextures <- ntexts == nverts
  if (any(hasnormals) || any(hastextures)) {
    vlinks <- vector("list", ncol(vertices))
    for (i in seq_along(polys)) {
      nvec <- tvec <- NA
      if (hasnormals[i])
        nvec <- as.numeric(normals[[i]])
      if (hastextures[i])
        tvec <- as.numeric(textures[[i]])
      vvec <- as.numeric(polys[[i]])
      for (j in seq_along(vvec)) {
        vlinks[[vvec[j]]] <- rbind(vlinks[[vvec[j]]],
                                   c(nvec[j], tvec[j], i, j))
      }
    }
    total <- 0
    for (i in seq_along(vlinks)) {
      vl2 = vlinks[[i]][, 2]
      if (is.null(vl2)) {
        vlinks[[i]] <- matrix(nrow = 0, ncol = 4)
        total <- total + 1
      } else {
        vlinks[[i]] <- vlinks[[i]][order(vl2), , drop = FALSE]
        total <- total + max(1, length(unique(vl2)))
      }
    }
    last <- ncol(vertices)
    vertices <- cbind(vertices, matrix(NA_real_, 3, total - ncol(vertices)))
    vnormals <- matrix(0, 4, total)
    vtexcoords <- matrix(NA_real_, 2, total)
    for (i in seq_along(vlinks)) {
      links <- vlinks[[i]]
      if (nrow(links)) {
        for (j in seq_len(nrow(links))) if (!is.na(links[j, 1]))
          vnormals[, i] <- vnormals[, i] + vn[, links[1, 1]]
        if (!is.na(links[1, 2]))
          vtexcoords[, i] <- vt[, links[1, 2]]
        same <- duplicated(links[, 2])
        duped <- FALSE
        for (j in seq_len(nrow(links))[-1]) {
          if (!same[j]) {
            last <- last + 1
            vertices[, last] <- vertices[, i]
            vnormals[, last] <- vnormals[, i]
            duped <- TRUE
          }
          if (duped) {
            polys[[links[j, 3]]][links[j, 4]] <- last
            if (!is.na(links[j, 2]))
              vtexcoords[, last] <- vt[, links[j, 2]]
          }
        }
      }
    }
  }
  triangles <- do.call(cbind, polys[nverts == 3])
  if (!length(triangles))
    triangles <- matrix(numeric(), 3, 0)
  quads <- do.call(cbind, polys[nverts == 4])
  others <- which(!(nverts %in% 3:4))
  for (i in seq_along(others)) {
    v <- polys[[others[i]]]
    tri <- triangulate(t(vertices[, v]))
    tri <- structure(v[tri], dim = dim(tri))
    triangles <- cbind(triangles, tri)
  }
  ignored <- unique(instrs)
  ignored <- ignored[!(ignored %in% c("v", "vn", "vt", "f",
                                      "", "#"))]
  if (length(ignored))
    warning(gettextf("Instructions %s ignored", paste0("\"",
                                                       ignored, "\"", collapse = ", ")), domain = NA)
  result <- rgl::tmesh3d(vertices, triangles, homogeneous = FALSE,
                    ...)
  if (length(quads))
    result$ib <- quads
  if (any(hasnormals))
    result$normals <- vnormals[1:3, ]/rep(vnormals[4, ],
                                          each = 3)
  if (any(hastextures))
    result$texcoords <- vtexcoords
  result
}
