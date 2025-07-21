       IDENTIFICATION DIVISION.
       PROGRAM-ID. PGTOCONT.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT CONTAPAGAR-FILE ASSIGN TO "CONTAPAGAR.DAT"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS CP-CHAVE
               FILE STATUS IS WS-STATUS-CONTAS.

           SELECT HISTPAGTO-FILE ASSIGN TO "HISTPAGTO.DAT"
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS WS-STATUS-HIST.

       DATA DIVISION.
       FILE SECTION.

       FD CONTAPAGAR-FILE.
       01 CONTA-REG.
           05 CP-CHAVE          PIC X(24).
           05 CP-NUM-DOC        PIC 9(10).
           05 CP-CNPJ-FORN      PIC 9(14).
           05 CP-DATA-EMISSAO   PIC 9(8).
           05 CP-DATA-VENC      PIC 9(8).
           05 CP-VALOR          PIC 9(10)V99.
           05 CP-SITUACAO       PIC X(1).
           05 CP-DATA-PGTO      PIC 9(8).

       FD HISTPAGTO-FILE.
       01 HIST-REG.
           05 H-NUM-DOC         PIC 9(10).
           05 H-CNPJ-FORN       PIC 9(14).
           05 H-DATA-PGTO       PIC 9(8).
           05 H-VALOR-PAGO      PIC 9(10)V99.

       WORKING-STORAGE SECTION.
       01 WS-STATUS-CONTAS      PIC XX.
       01 WS-STATUS-HIST        PIC XX.
       01 CONTINUA              PIC X VALUE "S".
       01 RESPOSTA              PIC X.

       01 WS-VALOR-TXT          PIC X(15).
       01 WS-VALOR-NUM          PIC 9(10)V99.

       01 WS-CHAVE              PIC X(24).

       PROCEDURE DIVISION.

       MAIN-PROCEDURE.
           MOVE "S" TO CONTINUA
           DISPLAY "INICIO DO PAGAMENTO DE CONTAS"
           OPEN I-O CONTAPAGAR-FILE
           IF WS-STATUS-CONTAS NOT = "00"
               DISPLAY "Erro ao abrir CONTAPAGAR.DAT:" WS-STATUS-CONTAS
               STOP RUN
           END-IF

           OPEN OUTPUT HISTPAGTO-FILE
           IF WS-STATUS-HIST NOT = "00"
               DISPLAY "Erro ao abrir HISTPAGTO.DAT: " WS-STATUS-HIST
               STOP RUN
           END-IF

           PERFORM UNTIL CONTINUA NOT = "S"
               PERFORM EFETUAR-PAGAMENTO
               DISPLAY "Deseja registrar outro pagamento? (S/N): "
               ACCEPT RESPOSTA
               MOVE FUNCTION UPPER-CASE(RESPOSTA) TO CONTINUA
           END-PERFORM

           CLOSE CONTAPAGAR-FILE
           CLOSE HISTPAGTO-FILE
           EXIT PROGRAM.

       EFETUAR-PAGAMENTO.
           DISPLAY "Número do Documento: " ACCEPT CP-NUM-DOC
           DISPLAY "CNPJ do Fornecedor: " ACCEPT CP-CNPJ-FORN

           STRING
               CP-NUM-DOC DELIMITED BY SIZE
               CP-CNPJ-FORN DELIMITED BY SIZE
               INTO CP-CHAVE

           READ CONTAPAGAR-FILE
               INVALID KEY
                   DISPLAY "Conta não localizada."
                   EXIT PARAGRAPH
           END-READ

           IF CP-SITUACAO NOT = "A"
               DISPLAY "Conta não está em aberto: " CP-SITUACAO
               EXIT PARAGRAPH
           END-IF

           DISPLAY "Data de Pagamento (AAAAMMDD): "
           ACCEPT CP-DATA-PGTO

           DISPLAY "Valor Pago: "
           ACCEPT WS-VALOR-TXT

           IF WS-VALOR-TXT = SPACES
               DISPLAY "Erro: Valor não informado."
               EXIT PARAGRAPH
           END-IF

           COMPUTE WS-VALOR-NUM = FUNCTION NUMVAL(WS-VALOR-TXT)

           IF WS-VALOR-NUM <= 0
               DISPLAY "Erro: Valor inválido."
               EXIT PARAGRAPH
           END-IF

           MOVE WS-VALOR-NUM TO CP-VALOR
           MOVE "P" TO CP-SITUACAO

           REWRITE CONTA-REG
           IF WS-STATUS-CONTAS = "00"
               DISPLAY "Conta paga com sucesso!"
           ELSE
               DISPLAY "Erro ao atualizar conta: " WS-STATUS-CONTAS
               EXIT PARAGRAPH
           END-IF

           MOVE CP-NUM-DOC    TO H-NUM-DOC
           MOVE CP-CNPJ-FORN  TO H-CNPJ-FORN
           MOVE CP-DATA-PGTO  TO H-DATA-PGTO
           MOVE CP-VALOR      TO H-VALOR-PAGO

           WRITE HIST-REG
           IF WS-STATUS-HIST = "00"
               DISPLAY "Registro histórico gravado."
           ELSE
               DISPLAY "Erro ao gravar histórico: " WS-STATUS-HIST
           END-IF.
