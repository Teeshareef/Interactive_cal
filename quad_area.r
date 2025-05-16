# -----------------------------
# interactive_calc.R
# -----------------------------
# This script defines two functions: one for solving a quadratic equation,
# and one for calculating the area of a triangle.
# It then runs both functions interactively by prompting the user for input.

# === 1) Define the two helper functions ===

# Function to solve a quadratic equation of the form ax² + bx + c = 0
solve_quadratic <- function(a, b, c) {
  if (a == 0) {
    # 'a' cannot be zero in a quadratic equation
    return("❗ 'a' must not be zero.")
  }

  # Calculate the discriminant (b² - 4ac)
  disc <- b^2 - 4*a*c

  if (disc > 0) {
    # Two distinct real roots
    r1 <- (-b + sqrt(disc)) / (2*a)
    r2 <- (-b - sqrt(disc)) / (2*a)
    return(paste("Two real roots: x₁ =", round(r1, 2), "x₂ =", round(r2, 2)))
  } else if (disc == 0) {
    # One repeated real root
    r <- -b / (2*a)
    return(paste("One real root: x =", round(r, 2)))
  } else {
    # Discriminant < 0 → complex roots
    return("⚠️ Complex roots (discriminant < 0).")
  }
}

# Function to calculate the area of a triangle using base and height
triangle_area <- function(base, height) {
  if (base <= 0 || height <= 0) {
    # Validate input: dimensions must be positive
    return("❗ Base and height must be positive.")
  }

  # Use the formula: (1/2) × base × height
  area <- 0.5 * base * height
  return(paste("Triangle area =", round(area, 2)))
}

# === 2) Main function to interact with user and display results ===
main <- function() {
  # --- Quadratic Equation ---
  cat("=== Quadratic Solver ===\n")
  a <- as.numeric(readline("Enter a (≠ 0): "))     # Prompt for coefficient 'a'
  b <- as.numeric(readline("Enter b: "))           # Prompt for coefficient 'b'
  c <- as.numeric(readline("Enter c: "))           # Prompt for coefficient 'c'
  cat(solve_quadratic(a, b, c), "\n\n")            # Display result

  # --- Triangle Area Calculation ---
  cat("=== Triangle Area ===\n")
  base   <- as.numeric(readline("Enter base: "))   # Prompt for base
  height <- as.numeric(readline("Enter height: ")) # Prompt for height
  cat(triangle_area(base, height), "\n")           # Display area
}

# === 3) Only run main() if script is executed directly (not sourced) ===
if (interactive()) {
  main()
}
