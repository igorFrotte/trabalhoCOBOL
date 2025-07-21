       IDENTIFICATION DIVISION.
       PROGRAM-ID. CONSFORN.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT FORNECEDOR-FILE ASSIGN TO "FORNECEDOR.DAT"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS F-CNPJ
               FILE STATUS IS WS-STATUS-FILE.

       DATA DIVISION.
       FILE SECTION.
       FD FORNECEDOR-FILE.
       01 FORNECEDOR-REG.
           05 F-CNPJ         PIC 9(14).
           05 F-RAZAO-SOCIAL PIC X(40).
           05 F-ENDERECO     PIC X(50).
           05 F-TELEFONE     PIC 9(11).
           05 F-EMAIL        PIC X(30).
           05 F-ATIVO        PIC X(1).  *> S = ativo, N = inativo

       WORKING-STORAGE SECTION.
       01 WS-STATUS-FILE     PIC XX.
       01 CONTINUA           PIC X VALUE "S".
       01 RESPOSTA           PIC X.

       PROCEDURE DIVISION.

       MAIN-PROCEDURE.
           DISPLAY "INÍCIO DA CONSULTA DE FORNECEDOR"
           OPEN INPUT FORNECEDOR-FILE
           IF WS-STATUS-FILE NOT = "00"
               DISPLAY "Erro ao abrir FORNECEDOR.DAT: " WS-STATUS-FILE
               STOP RUN
           END-IF

           PERFORM UNTIL CONTINUA NOT = "S"
               PERFORM CONSULTAR-FORNECEDOR
               DISPLAY "Deseja consultar outro fornecedor? (S/N): "
               ACCEPT RESPOSTA
               MOVE FUNCTION UPPER-CASE(RESPOSTA) TO CONTINUA
           END-PERFORM

           CLOSE FORNECEDOR-FILE
           STOP RUN.

       CONSULTAR-FORNECEDOR.
           DISPLAY "CNPJ do Fornecedor (14 dígitos): "
           ACCEPT F-CNPJ

           READ FORNECEDOR-FILE
               INVALID KEY
                   DISPLAY "Fornecedor não encontrado."
                   EXIT PARAGRAPH
           END-READ

           DISPLAY "===== DADOS DO FORNECEDOR ====="
           DISPLAY "CNPJ:           " F-CNPJ
           DISPLAY "Razão Social:   " F-RAZAO-SOCIAL
           DISPLAY "Endereço:       " F-ENDERECO
           DISPLAY "Telefone:       " F-TELEFONE
           DISPLAY "Email:          " F-EMAIL
           DISPLAY "Ativo?          " F-ATIVO
           DISPLAY "================================".
