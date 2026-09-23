000010*** EDIT ALLOWED                                                          
000011                                                                          
000020*    EDI TSR TRANSPORT SERVICE REQUIREMENTS                               
000030*    ANVÄNDS FÖR IFCSUM MEDDELANDE, GENERELL COPYTEXT                     
000040*                                                                         
000050*    FUNCTION,                                                            
000060*    -TO SPECIFY THE CONTRACT AND CARRIAGE CONDITIONS AND                 
      *     SERVICE AND PRIORITY REQ FOR THE TRANSPORT                          
000070*                                                                         
000100 01  WEDITSR.                                                             
000230     03 TSR-IDPTYP                             PIC X(03).                 
000240*                                              TSR                        
000501     03 TSR-LENGTH                             PIC 9(03).                 
000502*                                              LENGTH = 045               
000503*                                                                         
000519     03 TSR-C536-CONTR-CARR-COND.                                         
000520*                                                                         
000521        05 TSR-4065-CONTR-COND-CODED           PIC X(03).                 
000520*                                                                         
000521        05 TSR-1131-CODE-LIST-QUAL             PIC X(03).                 
000520*                                                                         
000521        05 TSR-3055-CODE-LIST-RESP-COD         PIC X(03).                 
000522*                                                                         
000523     03 TSR-C233-SERVICE.                                                 
000524*                                                                         
000525        05 TSR-7273-SERVICE-REQ-CODED          PIC X(03).                 
000526*                                                                         
000527        05 TSR-1131-CODE-LIST-QUAL             PIC X(03).                 
000524*                                                                         
000525        05 TSR-3055-CODE-LIST-RES-COD2         PIC X(03).                 
000526*                                                                         
000527        05 TSR-7273-SERVICE-REQ-CODED2         PIC X(03).                 
000528*                                                                         
000527        05 TSR-1131-CODE-LIST-QUAL3            PIC X(03).                 
000524*                                                                         
000525        05 TSR-3055-CODE-LIST-RES-COD3         PIC X(03).                 
000526*                                                                         
000523     03 TSR-C537-TRANSPORT-PRIO.                                          
000524*                                                                         
000525        05 TSR-4219-TRANSP-PRIO-CODED          PIC X(03).                 
000526*                                                                         
000527        05 TSR-1131-CODE-LIST-QUAL4            PIC X(03).                 
000528*                                                                         
000529        05 TSR-3055-CODE-LIST-RES-COD4         PIC X(03).                 
000533*                                                                         
000523     03 TSR-8141-TRANSIT-DIR-CODED             PIC X(03).                 
000524*                                                                         
000534     03 TSR-C703-NATURE-OF-CARGO.                                         
000535*                                                                         
000536        05 TSR-7085-NATURE-CARGO-CODED         PIC X(03).                 
000537*                                                                         
000538        05 TSR-1131-CODE-LIST-QUAL5            PIC X(03).                 
000528*                                                                         
000529        05 TSR-3055-CODE-LIST-RES-COD5         PIC X(03).                 
000541*                                                                         
000550*** END OF VILMAII-COPY LENGTH=051                                        
