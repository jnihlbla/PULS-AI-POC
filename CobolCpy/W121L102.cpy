000100 01  W121L102.                                                            
000200*                                 LÄNKAREA NR 2                           
000300*                                 ANVÄNDS VID LÄSNING AV TEXT             
000400*                                 BENREG FRÅN W1211000                    
000500     03 IDSKYLT              PIC X(3).                                    
000600*                                 NATIONALITETSTECKEN                     
000700*                                 SPRÅKIDENTIFIKATION                     
000800     03 BEARTEXT             PIC X(100).                                  
000900*                                 UTÖKAD ARTIKELBENÄMNING                 
001000     03 FLOVERSATT           PIC X.                                       
001100*                                 KAN TEXT ÖVERSÄTTAS, I KATALOG          
001200*                                 ÄR  TEXT ÖVERSATT,   I BENREG.          
001300     03 TIUPPDAT             PIC S9(7)           COMP-3.                  
001400*                                 UPPDATERINGSDATUM  (ÅÅMMDD)             
001500*** END OF VILMAII-COPY LENGTH= 108 BYTES                                 
