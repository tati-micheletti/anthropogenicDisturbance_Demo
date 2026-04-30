separateByReplicate <- function(spatVector) {
  
  # Ensure the input is a SpatVector and has the specified replicate column
  if (!inherits(spatVector, "SpatVector")) {
    stop("Input must be a SpatVector object.")
  }
  if (!"Replicate" %in% names(spatVector)) {
    stop(paste0("Replicate column 'Replicate' not found in SpatVector attributes."))
  }
  
  # Get unique replicate names
  replicates <- unique(spatVector$Replicate)
  
  # Create an empty list to store the separated SpatVectors
  replicate_list <- list()

  # Loop through each replicate and subset the SpatVector
  for (rep_name in replicates) {
    message(paste0("Separating replicate: ", rep_name))
    
    # Subset the SpatVector for the current replicate
    # Using '== rep_name' works for character/factor comparison
    current_replicate_sv <- subset(spatVector, spatVector$Replicate == rep_name, )
    
    # Store in the list with the replicate name as the list element name
    replicate_list[[rep_name]] <- current_replicate_sv
  }
  
  return(replicate_list)
}
