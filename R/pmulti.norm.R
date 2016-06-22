pmulti.norm <- 
function(Zx, Zy, rho = 0, conv = 1e-006)
{
	if(abs(rho) > 1) {
		stop("| rho | > 1")
	}
	Bprob <- 0
	if(Zx == 0) {
		tmp <- Zx
		Zx <- Zy
		Zy <- tmp
	}
	UPx <- pnorm( - Zx)/2
	UPy <- pnorm( - Zy)/2
	if(rho == 0) {
		Bprob <- 4 * UPx * UPy
	}
	else {
		RR <- 1 - (rho^2)
		if(RR == 0) {
			if(rho > 0) {
				if((Zx - Zy) < 0) {
				  Bprob <- 2 * UPy
				}
				else {
				  Bprob <- 2 * UPx
				}
			}
			else {
				if((Zx + Zy) >= 0) {
				  Bprob <- 0
				}
				else {
				  Bprob <- (2 * (UPx + UPy)) - 1
				}
			}
		}
		else {
			CONT <- F
			conn <- pi * conv
			sqrr <- sqrt(RR)
			if(Zx != 0) {
				Bprob <- UPx
				if((Zx * Zy) != 0) {
				  if((Zx * Zy) < 0) {
				    Bprob <- Bprob - 0.5
				  }
				  Bprob <- Bprob + UPy
				}
			}
			else {
				if(Zy != 0) {
				  Bprob <- Bprob + UPy
				}
				else {
				  Bprob <- (atan(rho/sqrr)/(2 * pi)) + 0.25
				  CONT <- T
				}
			}
			wh <-  - Zx
			wk <- ((Zy/Zx) - rho)/sqrr
			gw <- 2 * UPx
			swt <- F
			while(!CONT) {
				SGN <- -1
				TTT <- 0
				if(wk == 0) {
				  if(swt) {
				    break
				  }
				  if(Zy == 0) {
				    break
				  }
				  wh <-  - Zy
				  wk <- ((Zx/Zy) - rho)/sqrr
				  gw <- 2 * UPy
				  swt <- T
				}
				if(wk == 0) {
				  break
				}
				tst <- abs(wk) - 1
				if(tst == 0) {
				  TTT <- (wk * gw * (1 - gw))/2
				  Bprob <- Bprob + (SGN * TTT)
				  if(swt) {
				    break
				  }
				  if(Zy == 0) {
				    break
				  }
				  wh <-  - Zy
				  wk <- ((Zx/Zy) - rho)/sqrr
				  gw <- 2 * UPy
				  swt <- T
				}
				else {
				  if(tst > 0) {
				    SGN <-  - SGN
				    wh <- wh * wk
				    g2 <- pnorm(wh)
				    wk <- 1/wk
				    if(wk < 0) {
				      Bprob <- Bprob + 0.5
				    }
				    Bprob <- Bprob - ((gw + g2)/2) + (gw * g2)
				  }
				  h2 <- wh * wh
				  a2 <- wk * wk
				  h4 <- h2/2
				  if(h4 < 80) {
				    EX <- exp( - h4)
				  }
				  else {
				    EX <- 0
				  }
				  w2 <- h4 * EX
				  AP <- 1
				  s2 <- AP - EX
				  SP <- 1
				  s1 <- 0
				  SN <- s1
				  conex <- abs(conn/wk)
				  CN <- (AP * s2)/(SN + SP)
				  s1 <- s1 + CN
				  while((abs(CN) - conex) > conv) {
				    SN <- SP
				    SP <- SP + 1
				    s2 <- s2 - w2
				    w2 <- (w2 * h4)/SP
				    AP <-  - AP * a2
				    CN <- (AP * s2)/(SN + SP)
				    s1 <- s1 + CN
				  }
				  TTT <- (atan(wk) - (wk * s1))/(2 * pi)
				  Bprob <- Bprob + (SGN * TTT)
				  if(swt) {
				    break
				  }
				  if(Zy == 0) {
				    break
				  }
				  wh <-  - Zy
				  wk <- ((Zx/Zy) - rho)/sqrr
				  gw <- 2 * UPy
				  swt <- T
				}
			}
		}
	}
	if(Bprob < 0) {
		Bprob <- 0
	}
	if(Bprob > 1) {
		Bprob <- 1
	}
	return(1 - Bprob)
}

