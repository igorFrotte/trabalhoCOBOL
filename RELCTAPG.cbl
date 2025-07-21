       IDENTIFICATION DIVISION.
       PROGRAM-ID. RELCTAPG.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT CONTAPAGAR-FILE ASSIGN TO "CONTAPAGAR.DAT"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS SEQUENTIAL
               RECORD KEY IS CP-CHAVE
               FILE STATUS IS WS-STATUS-FILE.

       DATA DIVISION.
       FILE SECTION.

       FD CONTAPAGAR-FILE.
       01 CONTA-REG.
           05 CP-CHAVE         PIC X(24).
           05 CP-N             PIC 9(10).
           05 CP-CNPJ-F        PIC 9(14).
           05 CP-DATA-EMISSAO  PIC 9(8).
           05 CP-DATA-V        PIC 9(8).
           05 CP-V             PIC 9(10)V99.
           05 CP-SITUACAO      PIC X(1).
           05 CP-DATA-PGTO     PIC 9(8).

       WORKING-STORAGE SECTION.
       01 WS-STATUS-FILE       PIC XX.
       01 CONTADOR             PIC 9(5) VALUE ZEROS.

       PROCEDURE DIVISION.

       MAIN-PROCEDURE.
           DISPLAY "RELATÓRIO DE CONTAS EM ABERTO"
           OPEN INPUT CONTAPAGAR-FILE
           IF WS-STATUS-FILE NOT = "00"
               DISPLAY "Erro ao abrir CONTAPAGAR.DAT: " WS-STATUS-FILE
               STOP RUN
           END-IF

           DISPLAY "==================================================="
           DISPLAY "  DOC   |  CNPJ FORNECEDOR | VENCIMENTO  |  VALOR "
           DISPLAY "---------------------------------------------------"

           PERFORM LER-REGISTROS

           PERFORM FIM-RELATORIO

           CLOSE CONTAPAGAR-FILE
           STOP RUN.

       LER-REGISTROS.
           READ CONTAPAGAR-FILE
               AT END
                   MOVE "10" TO WS-STATUS-FILE
                   EXIT PARAGRAPH
           END-READ

           PERFORM UNTIL WS-STATUS-FILE = "10"
               IF CP-SITUACAO = "A"
                   ADD 1 TO CONTADOR
                   DISPLAY CP-N" | "CP-CNPJ-F" | "CP-DATA-V" | "CP-V
               END-IF
               READ CONTAPAGAR-FILE
                   AT END
                       MOVE "10" TO WS-STATUS-FILE
               END-READ
           END-PERFORM.

       FIM-RELATORIO.
           IF CONTADOR = 0
               DISPLAY "Nenhuma conta em aberto encontrada."
           ELSE
               DISPLAY "Total de contas em aberto: " CONTADOR
           END-IF.
