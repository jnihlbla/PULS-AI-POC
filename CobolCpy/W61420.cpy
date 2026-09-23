000100 01  W61420.                                                              
000200*                                 COPYTEXT FIL W61420                     
000300     03 IDARTNR              PIC S9(9)           COMP-3.                  
000400*                                 ARTIKELNUMMER                           
000500     03 IDLEVNR              PIC X(5).                                    
000600*                                 LEVERANT÷RNUMMER                        
000700     03 TIAAMMDD             PIC S9(7)           COMP-3.                  
000800*                                 ≈R - M≈NAD - DAG  (≈≈MMDD)              
000900     03 IDNAMN               PIC X(40).                                   
001000*                                 NAMN                                    
001100     03 IDTFN                PIC X(20).                                   
001200*                                 TELEFONNUMMER EXTERNT                   
001300     03 IDMAIL               PIC X(60).                                   
001400*                                 MAIL ADRESS                             
001500     03 DAREGDAT             PIC 9(8).                                    
001600*                                 REGISTRERINGSDATUM (≈≈≈≈MMDD)           
001700     03 KVAINF.                                                           
001800        05 TEKVAINF-EXT      OCCURS 7 TIMES                               
001900                             PIC X(79).                                   
002000*                                 KVALITETS INFORMATION EXTERNT           
002100*** END OF VILMAII-COPY LENGTH= 695 BYTES                                 
