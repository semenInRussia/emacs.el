(require 'agnifize)

(ert-deftest agnifize-test-variables
    ()
  (with-temp-buffer
    (insert "a = 2
b = 3
c = map(1 2 3)
if b == 3:
  print(a)
else:
  d = 6")
    (should
     (equal
      (agnifize--variables (point-min) (point-max))
      '("a" "b" "c" "d")))))

(ert-deftest agnifize-test-rename
    ()
  (with-temp-buffer
    (insert "a = 2
b = 3

if a != 2 and b % 2 == 0:
  print(b)
else:
  print(b)")
    (agnifize--rename "a" "alias")
    (should
     (equal
      (buffer-string)
      "alias = 2
b = 3

if alias != 2 and b % 2 == 0:
  print(b)
else:
  print(b)"))))

(ert-deftest agnifize-test-minimize-bin-ops
    ()
  (with-temp-buffer
    (insert "a = 2
b = 3

if a != 2 and b % 2 == 0:
  print(b)
else:
  print(b)")
    (agnifize--minimize-bin-ops)
    (should
     (equal
      (buffer-string)
      "a=2
b=3

if a!=2 and b%2==0:
  print(b)
else:
  print(b)"))))

(ert-deftest agnifize-test-minimize-bin-ops
    ()
  (with-temp-buffer
    (python-mode)
    (insert "a=2
b=3

### djeidje

if a!=2 and b%2==0:
  print(b)
### djedijeide
else:
  print(b)")
    (agnifize--change-comments)
    (should
     (equal
      (buffer-string)
      "a=2
b=3

djeidje

if a!=2 and b%2==0:
  print(b)
djedijeide
else:
  print(b)"))))

(ert-deftest agnifize-test-delete-empty-lines
    ()
  (with-temp-buffer
    (insert "a=2
b=3

if a!=2 and b%2==0:
  print(b)

else:
  print(b)")
    (agnifize--delete-empty-lines (point-min) (point-max))
    (should
     (equal
      (buffer-string)
      "a=2
b=3
if a!=2 and b%2==0:
  print(b)
else:
  print(b)"))))
