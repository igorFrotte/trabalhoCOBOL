       IDENTIFICATION DIVISION.
       PROGRAM-ID. LE-NOME.

       ENVIRONMENT DIVISION.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  NOME-USUARIO        PIC A(50).

       PROCEDURE DIVISION.
       INICIO.
           DISPLAY "Digite seu nome: "
           ACCEPT NOME-USUARIO
           DISPLAY "O nome digitado foi: " NOME-USUARIO
           STOP RUN.
