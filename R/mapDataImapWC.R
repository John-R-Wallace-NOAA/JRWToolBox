

mapDataImapWC <- function(XY, label = seq_along(XY[,1]), figureDelta = 0.2, labelDelta = 0.05) {
    N <- nrow(XY)
 '  '   
 '  # Just the data  '
    dev.new()
    plot(XY, xlim = c(min(XY[,1]) - figureDelta, max(XY[,1]) + figureDelta), ylim = c(min(XY[,2]) - figureDelta, max(XY[,2]) + figureDelta), type = 'o')
    text(XY + data.frame(rep(labelDelta, N), rep(0, N)), label = label)
 '  ' 
 '  # West Coast  '
    dev.new()
    imap(longrange = c(-130, -115), latrange = c(32, 48.5), zoom = FALSE)
    points(XY, type = 'o')
    text(XY + data.frame(rep(labelDelta, N), rep(0, N)), labels = label)
 '  '
 '  # Zoomed in'
    dev.new()
    imap(longrange = c(min(XY[,1]) - figureDelta, max(XY[,1]) + figureDelta), latrange = c(min(XY[,2]) - figureDelta, max(XY[,2]) + figureDelta), zoom=FALSE)
    points(XY, type = 'o')
    text(XY + data.frame(rep(labelDelta, N), rep(0, N)), label = label)
 
}
