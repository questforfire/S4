# Q1.
p_drug <- 0.8
n <- 12
min_patients <- 10

# Prob for >=10 == Prob for > 9, as distribution is DISCRETE

ans <- pbinom(min_patients-1, n, p_drug, lower.tail=FALSE)
ans

# Q2.
u <- -3
b <- 2

z <- function(x) {
	return( (x-u)/b )
}

# i. To find value of A, integral of f(x) over all x = 1
# 	Solving this,
g <- function(x) {
	return( (1/b)*exp(-(z(x)^2)/2) )
}
A <- 1 / integrate(g, -Inf, Inf)$value
A

f <- function(x) {
	return( (A/b)*exp(-(z(x)^2)/2) )
}

# ii. To derive CDF, we take integral of f from -Inf to x
F <- function(x) {
	return( integrate(f, -Inf, x)$value )
}
x <- seq(-5, 5, 0.5) # This generates the sequence -5, -4.5, -4, -3.5, -3, ...
plot(
	x,
	sapply(x, f), # This applies function F on each value of x
	type="o",
	ylim=c(0.00, 1.00), # Makes sure we can see the whole graph
	col="red",
	main="Graph of PDF & CDF"
)
lines(x, sapply(x, F), type="o", col="green")
legend("topleft", legend=c("PDF", "CDF"), fill=c("red", "green"))

# iii. E(X) = integral of x*PDF from -Inf to Inf
#   & E(X^2)= integral of x^2 * PDF .....
e1 <- function(x) {
	return(x * f(x))
}
E_X1 <- integrate(e, -Inf, Inf)$value
E_X1
e2 <- function(x) {
	return(x^2 * f(x))
}
E_X2 <- integrate(e2, -Inf, Inf)$value
E_X2


# Q3. Uses Bayes theorem
p_machine <- c(0.25, 0.35, 0.40)
p_defective <- c(0.05, 0.04, 0.02)

p_A <- p_machine[1]*p_defective[1] / sum(p_machine * p_defective)
p_B <- p_machine[2]*p_defective[2] / sum(p_machine * p_defective)
p_C <- p_machine[3]*p_defective[3] / sum(p_machine * p_defective)

p_A
p_B
p_C