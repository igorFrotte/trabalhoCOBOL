       IDENTIFICATION DIVISION.
       PROGRAM-ID. LANCCONT.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT FORNECEDOR-FILE ASSIGN TO "FORNECEDOR.DAT"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS F-CNPJ
               FILE STATUS IS WS-STATUS-FILE-F.

           SELECT CONTAPAGAR-FILE ASSIGN TO "CONTAPAGAR.DAT"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS CP-CHAVE
               FILE STATUS IS WS-STATUS-FILE-C.

       DATA DIVISION.
       FILE SECTION.

       FD FORNECEDOR-FILE.
       01 FORNECEDOR-REG.
           05 F-CNPJ            PIC 9(14).
           05 F-RAZAO-SOCIAL    PIC X(40).
           05 F-ENDERECO        PIC X(50).
           05 F-TELEFONE        PIC 9(11).
           05 F-EMAIL           PIC X(30).
           05 F-ATIVO           PIC X(1).

       FD CONTAPAGAR-FILE.
       01 CONTA-REG.
           05 CP-CHAVE          PIC X(24).  *> 10 (nº doc) + 14 (CNPJ)
           05 CP-NUM-DOC        PIC 9(10).
           05 CP-CNPJ-FORN      PIC 9(14).
           05 CP-DATA-EMISSAO   PIC 9(8).  *> AAAAMMDD
           05 CP-DATA-VENC      PIC 9(8).
           05 CP-VALOR          PIC 9(10)V99.
           05 CP-SITUACAO       PIC X(1).  *> A=ABERTO,P=PAGO,C=CANCEL
           05 CP-DATA-PGTO      PIC 9(8).

       WORKING-STORAGE SECTION.
       01 WS-STATUS-FILE-F      PIC XX.
       01 WS-STATUS-FILE-C      PIC XX.
       01 CONTINUA              PIC X VALUE "S".
       01 RESPOSTA              PIC X.

       01 WS-VALOR-TXT          PIC X(15).
       01 WS-VALOR-NUM          PIC 9(10)V99.

       PROCEDURE DIVISION.

       MAIN-PROCEDURE.
           DISPLAY "INICIO"
           OPEN I-O FORNECEDOR-FILE
           IF WS-STATUS-FILE-F NOT = "00"
               DISPLAY "Erro ao abrir FORNECEDOR.DAT: " WS-STATUS-FILE-F
               STOP RUN
           END-IF

           OPEN I-O CONTAPAGAR-FILE
           IF WS-STATUS-FILE-C NOT = "00"
               DISPLAY "Erro ao abrir CONTAPAGAR.DAT: " WS-STATUS-FILE-C
               STOP RUN
           END-IF

           PERFORM UNTIL CONTINUA = "N"
               PERFORM LANCA-CONTA
               DISPLAY "Deseja lançar outra conta? (S/N): "
               ACCEPT RESPOSTA
               MOVE FUNCTION UPPER-CASE(RESPOSTA) TO CONTINUA
           END-PERFORM

           CLOSE FORNECEDOR-FILE
           CLOSE CONTAPAGAR-FILE
           STOP RUN.

       LANCA-CONTA.
           DISPLAY "Número do Documento: "
           ACCEPT CP-NUM-DOC

           DISPLAY "CNPJ do Fornecedor (14 dígitos): "
           ACCEPT CP-CNPJ-FORN
           MOVE CP-CNPJ-FORN TO F-CNPJ

           READ FORNECEDOR-FILE
               INVALID KEY
                   DISPLAY "Fornecedor não cadastrado ou inativo."
                   EXIT PARAGRAPH
               NOT INVALID KEY
                   IF F-ATIVO NOT = "S"
                       DISPLAY "Fornecedor está inativo."
                       EXIT PARAGRAPH
                   END-IF
           END-READ

           DISPLAY "Data de Emissão (AAAAMMDD): "
           ACCEPT CP-DATA-EMISSAO

           DISPLAY "Data de Vencimento (AAAAMMDD): "
           ACCEPT CP-DATA-VENC

           IF CP-DATA-VENC < CP-DATA-EMISSAO
               DISPLAY "Erro: Data de vencimento anterior à emissão."
               EXIT PARAGRAPH
           END-IF

           DISPLAY "Valor (ex: 1000.50): "
           ACCEPT WS-VALOR-TXT

           IF WS-VALOR-TXT = SPACES
               DISPLAY "Erro: Valor não informado."
               EXIT PARAGRAPH
           END-IF

           IF WS-VALOR-TXT(1:1) = "-"
               DISPLAY "Erro: Valor negativo não permitido."
               EXIT PARAGRAPH
           END-IF

           COMPUTE WS-VALOR-NUM = FUNCTION NUMVAL(WS-VALOR-TXT)

           IF WS-VALOR-NUM <= 0
               DISPLAY "Erro: Valor deve ser maior que zero."
               EXIT PARAGRAPH
           END-IF

           MOVE WS-VALOR-NUM TO CP-VALOR
           MOVE "A" TO CP-SITUACAO
           MOVE ZEROS TO CP-DATA-PGTO

           *> Montar chave composta: número do doc + cnpj
           STRING
               CP-NUM-DOC DELIMITED BY SIZE
               CP-CNPJ-FORN DELIMITED BY SIZE
               INTO CP-CHAVE

           *> Verifica se já existe essa chave
           READ CONTAPAGAR-FILE
               INVALID KEY
                   *> Se não existe, pode gravar
                   WRITE CONTA-REG
                   IF WS-STATUS-FILE-C = "00"
                       DISPLAY "Conta lançada com sucesso!"
                   ELSE
                       DISPLAY "Erro ao lançar: " WS-STATUS-FILE-C
                   END-IF
               NOT INVALID KEY
                   DISPLAY "Erro: Já existe com esse número + CNPJ."
           END-READ.
