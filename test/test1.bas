Function add(x, y)
  add = x + y
End Function

Function main()
  a = 10
  b = 5
  
  ' コメント
  If a - 5 Then
    b = 20
  End If
  
  sum = 0
  For i = 1 To 10
    sum = sum + i
  Next
  
  Debug.Print "a"
  ' main = a + add(5 * 3, b)
  main = "あいうえお" & "かきく: " & sum
End Function
