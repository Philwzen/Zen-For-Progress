
     IF VALID-HANDLE(h-asyncserver) And
        h-asyncserver:Connected()
     then h-asyncserver:disconnect().
