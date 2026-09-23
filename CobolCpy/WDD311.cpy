000100 01  TEXT-WDD311.                                                         
000200*                                 BENÄMNINGSREGISTER                      
000300*                                 TEXT-SEGMENT                            
000400*                                 FYSISK NYCKEL IDSKYLT                   
000500*                                 SÖKBEGREPP BEART                        
000600     03 TEXT-IDSKYLT         PIC X(3).                                    
000700*                                 NATIONALITETSTECKEN                     
000800*                                 SPRÅKIDENTIFIKATION                     
000900     03 TEXT-BEARTEXT        PIC X(100).                                  
001000*                                 UTÖKAD ARTIKELBENÄMNING                 
001100     03 TEXT-BEART-FILLER REDEFINES TEXT-BEARTEXT.                        
001200        05 TEXT-BEART        PIC X(25).                                   
001300*                                 ARTIKELBENÄMNING                        
001400        05 FILLER            PIC X(75).                                   
001500     03 TEXT-FLOVERSATT      PIC X.                                       
001600*                                 KAN TEXT ÖVERSÄTTAS, I KATALOG          
001700*                                 ÄR  TEXT ÖVERSATT,   I BENREG.          
001800     03 TEXT-TIUPPDAT        PIC S9(7)           COMP-3.                  
001900*                                 UPPDATERINGSDATUM  (ÅÅMMDD)             
002000     03 TEXT-FILLER          PIC X(12).                                   
002100*** END OF VILMAII-COPY LENGTH= 120 BYTES                                 
