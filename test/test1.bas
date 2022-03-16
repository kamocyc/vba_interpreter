Function add(x As Integer, y As Integer) As Integer
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
  Debug.Print a + add(5 * 3, b)
  
  aa()
  
  main = "あいうえお" & "かきく: " & sum
End Function

' 現状、空の関数が書けない。後で直す
Function aa()
  Debug.Print "b"
End Function
