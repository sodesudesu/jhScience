create_ultraman_like_pattern <- function() {
  theta <- seq(0, 100 * pi, length.out = 10000)
  r <- 0.2 + 0.8 * exp(0.3 * theta) * sin(15 * theta)
  x <- r * cos(theta)
  y <- r * sin(theta)

  # ランダムな色のリストを生成
  colors <- c("blue", "cyan", "black", "orange", "purple")
  alpha_values <- runif(length(theta), 0.1, 0.5)
  selected_colors <- rgb(t(col2rgb(colors) / 255), alpha = alpha_values)

  # 新しいプロットを開始
  png(file="ultraman_like_pattern.png", width=8.27*100, height=11.69*100, units="mm", res=300)
  plot(x, y, type='n', xlab='', ylab='', xlim=c(-1.5, 1.5), ylim=c(-1.5, 1.5))

  # 各点ごとにプロット
  for (i in 2:length(theta)) {
    lines(x[i-1:i], y[i-1:i], col=selected_colors[i], lwd=2)
  }

  # プロットを終了
  dev.off()
}

# PNG に描画
create_ultraman_like_pattern()

