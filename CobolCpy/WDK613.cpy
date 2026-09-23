000100 01  EMB-WDK613.                                                          
000200*                                 EMBALLAGE                               
000300*                                 FYSISK NYCKEL KDEMBAL                   
000400     03 EMB-KDEMBKEY         PIC X(3).                                    
000500*                                 NYCKEL FÖR ATT SÄRSKILJA EMABAL         
000600*                                 LAGE ÅT                                 
000700*                                 KEY TO SEPERATE PACKAGES                
000800     03 EMB-IDARTNR-EMB      PIC S9(9)           COMP-3.                  
000900*                                 EMBALLAGE-ARTIKELNUMMER                 
001000     03 EMB-KVQPACK-EMB      PIC S9(5)           COMP-3.                  
001100*                                 ANTAL I FÖRPACKNING                     
001200*                                 GÄLLER FÖR EXTRAEMBALLAGEN              
001300     03 EMB-KDEMBKOD         PIC S9(3)           COMP-3.                  
001400*                                 EMBALLAGEKOD                            
001500*** END OF VILMAII-COPY LENGTH= 13 BYTES                                  
