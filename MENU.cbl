       IDENTIFICATION DIVISION.
       PROGRAM-ID. MENU.

       DATA DIVISION.
       WORKING-STORAGE SECTION.

       01 OPCAO-MENU        PIC 9.
       01 OPCAO-CONSULTA    PIC 9.
       01 OPCAO-RELATORIO   PIC 9.
       01 CONTINUA          PIC X VALUE "S".

       PROCEDURE DIVISION.

       MAIN-PROCEDURE.
           PERFORM UNTIL CONTINUA NOT = "S"
               DISPLAY "=========== MENU PRINCIPAL ==========="
               DISPLAY "1 - Cadastro de Fornecedores"
               DISPLAY "2 - Lançamento de Contas"
               DISPLAY "3 - Pagamento de Contas"
               DISPLAY "4 - Consultas"
               DISPLAY "5 - Relatórios"
               DISPLAY "6 - Sair"
               DISPLAY "======================================"
               DISPLAY "Escolha uma opção: "
               ACCEPT OPCAO-MENU

               EVALUATE OPCAO-MENU
                   WHEN 1
                       CALL "CADFORNE"
                   WHEN 2
                       CALL "LANCCONT"
                   WHEN 3
                       CALL "PGTOCONT"
                   WHEN 4
                       PERFORM SUBMENU-CONSULTA
                   WHEN 5
                       PERFORM SUBMENU-RELATORIO
                   WHEN 6
                       MOVE "N" TO CONTINUA
                   WHEN OTHER
                       DISPLAY "Opção inválida. Tente novamente."
               END-EVALUATE
           END-PERFORM

           DISPLAY "Encerrando o sistema..."
           STOP RUN.

       SUBMENU-CONSULTA.
           DISPLAY "------ CONSULTAS DISPONÍVEIS ------"
           DISPLAY "1 - Consultar Fornecedor por CNPJ"
           DISPLAY "2 - Voltar ao Menu Principal"
           DISPLAY "----------------------------------"
           DISPLAY "Escolha uma opção: "
           ACCEPT OPCAO-CONSULTA

           EVALUATE OPCAO-CONSULTA
               WHEN 1
                   CALL "CONSFORN"
               WHEN 2
                   CONTINUE
               WHEN OTHER
                   DISPLAY "Opção inválida no menu de consultas."
           END-EVALUATE.

       SUBMENU-RELATORIO.
           DISPLAY "------ RELATÓRIOS DISPONÍVEIS ------"
           DISPLAY "1 - Relatório de Contas em Aberto"
           DISPLAY "2 - Relatório de Pagamentos por Período"
           DISPLAY "3 - Voltar ao Menu Principal"
           DISPLAY "------------------------------------"
           DISPLAY "Escolha uma opção: "
           ACCEPT OPCAO-RELATORIO

           EVALUATE OPCAO-RELATORIO
               WHEN 1
                   CALL "RELCTAPG"
               WHEN 2
                   CALL "RELPAGTO"
               WHEN 3
                   CONTINUE
               WHEN OTHER
                   DISPLAY "Opção inválida no menu de relatórios."
           END-EVALUATE.
