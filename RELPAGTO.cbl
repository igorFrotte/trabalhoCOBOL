       IDENTIFICATION DIVISION.
       PROGRAM-ID. RELPAGTO.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT HISTPAGTO-FILE ASSIGN TO "HISTPAGTO.DAT"
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS WS-STATUS-HIST.

       DATA DIVISION.
       FILE SECTION.

       FD HISTPAGTO-FILE.
       01 HIST-REG.
           05 H-NUM-DOC     PIC 9(10).
           05 H-CNPJ-F      PIC 9(14).
           05 H-DT-PG       PIC 9(8).  *> AAAAMMDD
           05 H-V-PG        PIC 9(10)V99.

       WORKING-STORAGE SECTION.
       01 WS-STATUS-HIST        PIC XX.
       01 WS-DATA-INI           PIC 9(8).
       01 WS-DATA-FINAL         PIC 9(8).
       01 CONTADOR              PIC 9(5) VALUE ZEROS.
       01 WS-TOTAL-PAGO         PIC 9(10)V99 VALUE ZEROS.

       PROCEDURE DIVISION.

       MAIN-PROCEDURE.
           DISPLAY "RELATÓRIO DE PAGAMENTOS POR PERÍODO"

           DISPLAY "Informe a data inicial (AAAAMMDD): "
           ACCEPT WS-DATA-INI

           DISPLAY "Informe a data final (AAAAMMDD): "
           ACCEPT WS-DATA-FINAL

           IF WS-DATA-FINAL < WS-DATA-INI
               DISPLAY "Erro: Data final menor que a inicial."
               STOP RUN
           END-IF

           OPEN INPUT HISTPAGTO-FILE
           IF WS-STATUS-HIST NOT = "00"
               DISPLAY "Erro ao abrir HISTPAGTO.DAT: " WS-STATUS-HIST
               STOP RUN
           END-IF

           DISPLAY "==================================================="
           DISPLAY "  DOC  | CNPJ FORNECEDOR  | DATA PGTO  | VALOR PAGO"
           DISPLAY "------------------------------- -------------------"

           PERFORM LER-HISTORICO

           PERFORM FIM-RELATORIO

           CLOSE HISTPAGTO-FILE
           EXIT PROGRAM.

       LER-HISTORICO.
           READ HISTPAGTO-FILE
               AT END
                   MOVE "10" TO WS-STATUS-HIST
                   EXIT PARAGRAPH
           END-READ

           PERFORM UNTIL WS-STATUS-HIST = "10"
               IF H-DT-PG >= WS-DATA-INI AND H-DT-PG <= WS-DATA-FINAL
                   ADD 1 TO CONTADOR
                   ADD H-V-PG TO WS-TOTAL-PAGO
                   DISPLAY H-NUM-DOC" | "H-CNPJ-F" | "H-DT-PG" | "H-V-PG
               END-IF

               READ HISTPAGTO-FILE
                   AT END
                       MOVE "10" TO WS-STATUS-HIST
               END-READ
           END-PERFORM.

       FIM-RELATORIO.
           IF CONTADOR = 0
               DISPLAY "Nenhum pagamento encontrado no período."
           ELSE
               DISPLAY "----------------------------------------------"
               DISPLAY "Total de pagamentos: " CONTADOR
               DISPLAY "Valor total pago:    " WS-TOTAL-PAGO
           END-IF.
