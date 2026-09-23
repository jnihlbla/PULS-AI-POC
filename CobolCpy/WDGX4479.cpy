000100 01  4479-WDGX4479-CTX.                                                   
000200*                                 LASTBÄRARE PROFORMA                     
000300*                                 FYSISK NYCKEL WDGXKEY:                  
000400*                                 (IDHTYP   + IDDC    +                   
000500*                                 (IDTRPTNR + IDLBBET +                   
000600*                                  LOW-VALUE)                             
000700     03 4479-IDHTYP          PIC X(4).                                    
000800*                                 HÄNDELSETYP                             
000900     03 4479-IDDC            PIC X(2).                                    
001000*                                 IDENTIFIERARE LAGER                     
001100*                                 WAREHOUSE IDENTIFIER                    
001200     03 4479-IDTRPTNR        PIC S9(3)           COMP-3.                  
001300*                                 TRANSPORTIDENTITET                      
001400*                                 TRANSPORT IDENTITY                      
001500     03 4479-IDLBBET         PIC X(12).                                   
001600*                                 LASTBÄRARBETECKNING                     
001700*                                 TRAILER NUMBER                          
001800     03 4479-LOW-VALUE       PIC X(10).                                   
001900*** END OF VILMAII-COPY LENGTH= 30 BYTES                                  
