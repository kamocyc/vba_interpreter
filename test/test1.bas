Private Sub say_helloworld_Click()
   MsgBox "Hi"
End Sub

Rem This is comment

Function add(x As Integer, y As Integer) As Integer
  add = x + y
End Function

Sub main()
  say_helloworld_Click
  
  Dim a As Integer
  Dim b As Integer
  
  a = 10
  b = 5
  
  ' コメント
  If a - 5 Then
    b = 20
  End If
  
  Dim sum As Integer
  Dim i As Integer
  
  sum = 0
  For i = 1 To 10
    sum = sum + i
  Next i
  
  Debug.Print "a"
  Debug.Print a + add(5 * 3, b)
  
  aa
  
  Debug.Print "あいうえお" & "かきく: " & sum
  ' main = 
End Sub

' 現状、空の関数が書けない。後で直す
Sub aa()
  Debug.Print "b"
End Sub
