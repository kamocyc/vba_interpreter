Private Sub say_helloworld_Click()
   MsgBox "Hi"
End Sub

Rem This is comment

Function add(x As Integer, y As Integer) As Integer
  add = x + y
End Function

Sub main()
  ' 
  ' Cells.Set(1, 2, 3)
  ' Dim i1 As Integer
  ' For i1 = 1 To 10
  '   Cells.Item(i1, 1) = 10 + i1
  ' Next i1
  
  ' Cells.Item(1, 3).Value = "Hello"
  ' Cells.Item(1, 4).Value = "World"
  ' Cells.Item(1, 5).Value = "!"
  
  Dim p1 As point
  Set p1 = New point
  p1.x = 1
  p1.y = 2
  Dim p2 As point
  Set p2 = New point
  p2.x = 1 + 10
  p2.y = 2 + 10
  Debug.Print "p1.x = " & p1.x
  Debug.Print "p1.y = " & p1.y
  Debug.Print "p2.x = " & p2.x
  Debug.Print "p2.y = " & p2.y
  Dim p3 As point
  Set p3 = p1.add(p2)
  Debug.Print "p3.x = " & p3.x
  Debug.Print "p3.y = " & p3.y
  
  
  
  Dim t As Object
  Set t = New test2
  
  t.hoge = "hogehoge"
  Debug.Print "hoge=" & t.hoge
  t.set1 "fuga"
  Debug.Print "hoge=" & t.hoge
  
  Dim i1 As Integer
  Dim i2 As Integer
  For i1 = 1 To 9
    For i2 = 1 To 9
      Cells(i1, i2) = i1 * i2
    Next
  Next
  
  ' Cells(2, 2) = "hoeg"
  Debug.Print Cells(2, 2)
  Debug.Print Cells.Item(1, 1).Value
  Debug.Print Cells.Range("B1", 1)
  
  Dim name As String
  ' name = InputBox("Enter your name")
  MsgBox "Hello " & name

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
