000100 01  W27125.                                                              
000200*                                 COPYTEXT TILL FILEN W27125              
000300*                                 SKAPA REFILLORDER VID                   
000400*                                 INLEVERANS MOT                          
000500*                                 CROSS DOCKING OMRÅDE                    
000600     03 ANROPSTYP            PIC 9(2).                                    
000700     03 IDARTNR              PIC S9(9)           COMP-3.                  
000800*                                 ARTIKELNUMMER                           
000900     03 KVANTAL-CD           OCCURS 4 TIMES                               
001000                             PIC S9(7)           COMP-3.                  
001100*                                 ANTAL SOM SKA STYRAS TILL ETT           
001200*                                 CROSS DOCKING OMRÅDE MEN EJ             
001300*                                 HAR UPPDATERAT SALDO (KVLS-CD)          
001400     03 IDDC-GRUPP.                                                       
001500        05 IDDC              OCCURS 20 TIMES                              
001600                             PIC X(2).                                    
001700*                                 IDENTIFIERARE LAGER                     
001800     03 KVBEHOV-DC-GRUPP.                                                 
001900        05 KVBEHOV-DC        OCCURS 20 TIMES                              
002000                             PIC S9(7)V9(2)      COMP-3.                  
002100*                                 BEHOVSSTORLEK                           
002200     03 IX                   PIC 9(2).                                    
002300     03 KVANTAL-CD-KOMPL     PIC S9(7)           COMP-3.                  
002400*                                 ANTAL SOM KAN STYRAS TILL               
002500*                                 CROSS DOCKING OMRÅDE                    
002600*                                 KONTROLL GÖRS I W27125 OM DET           
002700*                                 FINNS REFILLORDER SOM HAR               
002800*                                 YTTERLIGARE BEHOV MOT CROSS             
002900*                                 DOCKING OMRÅDEN                         
003000     03 KVANTAL-CD-RES       PIC S9(7)           COMP-3.                  
003100*                                 ANTAL SOM SKA STYRAS TILL               
003200*                                 CROSS DOCKING OMRÅDE MEN DÄR            
003300*                                 DET REDAN LIGGER EN REFILLORDER         
003400*                                 6115 FÅR DETTA SOM ETT SVAR             
003500*                                 TILLBAKA FRÅN W27125                    
003600*** END OF VILMAII-COPY LENGTH= 173 BYTES                                 
