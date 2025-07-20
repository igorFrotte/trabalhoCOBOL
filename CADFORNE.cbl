       IDENTIFICATION DIVISION.
       PROGRAM-ID. CADFORNE.

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
       01 OPCAO              PIC 9.
       01 CONTINUA           PIC X VALUE "S".

       PROCEDURE DIVISION.

       MAIN-PROCEDURE.
           OPEN I-O FORNECEDOR-FILE
           IF WS-STATUS-FILE NOT = "00"
               DISPLAY "Erro ao abrir. Status: " WS-STATUS-FILE
               STOP RUN
           END-IF

           PERFORM UNTIL CONTINUA NOT = "S"
               DISPLAY "===== CADASTRO DE FORNECEDORES ====="
               DISPLAY "1 - Incluir"
               DISPLAY "2 - Alterar"
               DISPLAY "3 - Excluir (lógica)"
               DISPLAY "4 - Sair"
               ACCEPT OPCAO

               EVALUATE OPCAO
                   WHEN 1
                       PERFORM INCLUIR-FORNECEDOR
                   WHEN 2
                       PERFORM ALTERAR-FORNECEDOR
                   WHEN 3
                       PERFORM EXCLUIR-FORNECEDOR
                   WHEN 4
                       MOVE "N" TO CONTINUA
                   WHEN OTHER
                       DISPLAY "Opção inválida."
               END-EVALUATE
           END-PERFORM

           CLOSE FORNECEDOR-FILE
           STOP RUN.

       INCLUIR-FORNECEDOR.
           DISPLAY "CNPJ (14 dígitos): " ACCEPT F-CNPJ
           READ FORNECEDOR-FILE
               INVALID KEY
                   DISPLAY "Razão Social: " ACCEPT F-RAZAO-SOCIAL
                   DISPLAY "Endereço: " ACCEPT F-ENDERECO
                   DISPLAY "Telefone: " ACCEPT F-TELEFONE
                   DISPLAY "Email: " ACCEPT F-EMAIL
                   MOVE "S" TO F-ATIVO
                   WRITE FORNECEDOR-REG
                   IF WS-STATUS-FILE = "00"
                       DISPLAY "Fornecedor incluído com sucesso!"
                   ELSE
                       DISPLAY "Erro ao incluir: " WS-STATUS-FILE
                   END-IF
               NOT INVALID KEY
                   DISPLAY "Fornecedor já cadastrado."
           END-READ.

       ALTERAR-FORNECEDOR.
           DISPLAY "CNPJ a alterar: " ACCEPT F-CNPJ
           READ FORNECEDOR-FILE
               INVALID KEY
                   DISPLAY "Fornecedor não encontrado."
               NOT INVALID KEY
                   DISPLAY "Nova Razão Social: " ACCEPT F-RAZAO-SOCIAL
                   DISPLAY "Novo Endereço: " ACCEPT F-ENDERECO
                   DISPLAY "Novo Telefone: " ACCEPT F-TELEFONE
                   DISPLAY "Novo Email: " ACCEPT F-EMAIL
                   REWRITE FORNECEDOR-REG
                   IF WS-STATUS-FILE = "00"
                       DISPLAY "Dados atualizados com sucesso!"
                   ELSE
                       DISPLAY "Erro ao atualizar: " WS-STATUS-FILE
                   END-IF
           END-READ.

       EXCLUIR-FORNECEDOR.
           DISPLAY "CNPJ a excluir: " ACCEPT F-CNPJ
           READ FORNECEDOR-FILE
               INVALID KEY
                   DISPLAY "Fornecedor não encontrado."
               NOT INVALID KEY
                   MOVE "N" TO F-ATIVO
                   REWRITE FORNECEDOR-REG
                   IF WS-STATUS-FILE = "00"
                       DISPLAY "Fornecedor desativado com sucesso!"
                   ELSE
                       DISPLAY "Erro ao desativar: " WS-STATUS-FILE
                   END-IF
           END-READ.
