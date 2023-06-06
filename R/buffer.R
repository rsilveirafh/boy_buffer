#funcao:

buffer.f <- function(foo, buffer, reps) {
	suitable <- list()
	for (k in 1:reps) {
		outvec <- as.numeric(c())
		dropvec <- c()
		for (i in 1:nrow(foo)) {
			if (length(dropvec) < nrow(foo)) {
				if (i > 1) {
					rowsleft <- (1:nrow(foo))[-c(dropvec)]
				} else {
					rowsleft <- 1:nrow(foo)
				}
				outpoint <- as.numeric(sample(as.character(rowsleft), 
					1))
				outvec[i] <- outpoint
				outcoord <- foo[outpoint, c("x", "y")]
				dropvec <- c(dropvec, which(sqrt((foo$x - outcoord$x)^2 + 
					(foo$y - outcoord$y)^2) < buffer))
				dropvec <- dropvec[!duplicated(dropvec)]
			}
		}
		suitable[[k]] <- outvec
	}
	best <- unlist(suitable[which.max(lapply(suitable, length))])
	foo[best, ]
}

## Definir o arquivo que contem os pontos
## Criar objeto apenas com long lat (nessa ordem)
## Retirar pontos que estao a um raio de 500m uns dos outros 
# buffer.f(data, distancia, iteracoes)
# buffer.f(species, 0.5, 1000)
