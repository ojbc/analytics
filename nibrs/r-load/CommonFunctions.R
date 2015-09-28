# common functions

getChildSegmentID <- function(incidentID, childSegmentSequenceNumber) {
  # different implementation needed if there were ever more than 10 child sequence numbers in NIBRS...but that's unlikely
  (incidentID*10) + (childSegmentSequenceNumber - 1)
}