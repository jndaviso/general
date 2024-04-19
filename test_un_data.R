# Vector of 100 country names
country_names <- c("Afghanistan", "Albania", "Algeria", "Andorra", "Angola", 
                   "Antigua and Barbuda", "Argentina", "Armenia", "Australia", "Austria", 
                   "Azerbaijan", "Bahamas", "Bahrain", "Bangladesh", "Barbados", 
                   "Belarus", "Belgium", "Belize", "Benin", "Bhutan", 
                   "Bolivia", "Botswana", "Brazil", "Brunei", 
                   "Bulgaria", "Burkina Faso", "Burundi", "Côte d'Ivoire", "Cabo Verde", 
                   "Cambodia", "Cameroon", "Canada", "Central African Republic", "Chad", 
                   "Chile", "China", "Colombia", "Comoros", 
                   "Costa Rica", "Croatia", "Cuba", "Cyprus", "Czechia (Czech Republic)", 
                   "Denmark", "Djibouti", "Dominican Republic", 
                   "Ecuador", "Egypt", "El Salvador", "Eritrea", 
                   "Estonia", "Eswatini", "Ethiopia", "Fiji", "Finland", 
                   "France", "Gabon", "Gambia", "Georgia", "Germany", 
                   "Ghana", "Greece", "Grenada", "Guatemala", "Guinea", 
                   "Guinea-Bissau", "Guyana", "Haiti", "Holy See", "Honduras", 
                   "Hungary", "Iceland", "India", "Indonesia", "Iran", 
                   "Iraq", "Ireland", "Israel", "Italy", "Jamaica", 
                   "Japan", "Jordan", "Kazakhstan", "Kenya", "Kiribati", 
                   "Kuwait", "Kyrgyzstan", "Laos", "Latvia", "Lebanon", 
                   "Lesotho", "Liberia", "Libya", "Liechtenstein", "Lithuania", 
                   "Luxembourg", "Madagascar", "Malawi", "Malaysia", "Maldives")

# Predefine the 3D array with NA values, dimensions (100, 100, 100)
array_mx <- array(NA, dim = c(100, 100, 100))

# Repeat the following process 100 times
for (k in 1:100) {
  # Generate random data with a set seed for reproducibility in each iteration
  set.seed(42 + k)  # Adjusting seed for variety while maintaining reproducibility
  values <- sample(c("Y", "N", "A", NA), 100, replace = TRUE, prob = c(0.25, 0.25, 0.25, 0.25))
  
  # Create a data frame
  country_data <- data.frame(Country = country_names, Value = values)
  
  # Convert Values to numeric factors
  country_data$Value <- factor(country_data$Value, levels = c("Y", "N", "A"), labels = c(1, -1, 0))
  
  # The number of countries
  n <- length(country_data$Value)
  
  # Initialize matrix for this iteration
  mx <- matrix(data = NA, nrow = n, ncol = n)
  
  # Populate the matrix
  for (j in 1:n) {
    for (i in 1:n) {
      if (is.na(country_data$Value[i]) || is.na(country_data$Value[j])) {
        mx[i, j] <- NA
      } 
      if (i == j){
        mx[i, j] <- NA
      }
            else {
        mx[i, j] <- as.numeric(levels(country_data$Value[i]))[country_data$Value[i]] * 
          as.numeric(levels(country_data$Value[j]))[country_data$Value[j]]
      }
    }
  }
  
  # Store the matrix in the corresponding layer of the array
  array_mx[,,k] <- mx
}


View(array_mx[,,1])

# Sum on the third dimension
result <- apply(array_mx, c(1, 2), sum, na.rm = TRUE)

colnames(result) <- country_names
rownames(result) <- country_names

