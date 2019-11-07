# sokoban-5

Opis zadania w pliku ZADANIE.md. W pliku README.md proszę dodać opis swojego rozwiązania.


W moim rozwiązaniu zastosowałem kod z poprzedniego zadania. Przerobiłem go zgodnie z wskazówkami z zadania m.in.
    Prezentowałem elementy w następujący sposób
        Wall        - #
        Player      - @
        Box         - $
        Floor       - ' '
    Zastosowałem poniższa definicję Picture i DrawFun
        type DrawFun = Integer -> Integer -> Char
        type Picture = DrawFun -> DrawFun
    Nie implementowałem obsługi strzałek, gracz porusza się przy użyciu klawiszy WSAD