Public x As Integer
Public y As Integer

Function add(other As point) As point
  Dim result As point
  Set result = New point
  result.x = x + other.x
  result.y = y + other.y
  
  Set add = result
End Function
