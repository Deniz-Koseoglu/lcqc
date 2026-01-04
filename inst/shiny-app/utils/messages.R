# utils/messages.R

# Function to show an error notification
show_error <- function(session, message) {
  showNotification(message, type = "error")
}

# Function to show a warning notification
show_warning <- function(session, message) {
  showNotification(message, type = "warning")
}