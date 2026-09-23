000100 01  EMB-W01164X.                                                         
000200*                                 A COPY OF W01164 - TO CREATE WX         
000300*                                 TR FILE IN EDITABLE FORMAT              
000400     03 EMB-IDARTNR          PIC Z(7)9                                    
000500                             VALUE ZEROS.                                 
000600*                                 ARTIKELNUMMER                           
000700*                                 PART NUMBER                             
000800     03 EMB-WDK613.                                                       
000900*                                 EMBALLAGE                               
001000*                                 FYSISK NYCKEL KDEMBAL                   
001100        05 EMB-KDEMBKEY      PIC X(3)                                     
001200                             VALUE SPACES.                                
001300*                                 NYCKEL FÖR ATT SÄRSKILJA EMABAL         
001400*                                 LAGE ÅT                                 
001500*                                 KEY TO SEPERATE PACKAGES                
001600        05 EMB-IDARTNR-EMB   PIC Z(7)9                                    
001700                             VALUE ZEROS.                                 
001800*                                 EMBALLAGE-ARTIKELNUMMER                 
001900        05 EMB-KVQPACK-EMB   PIC -(5)9                                    
002000                             VALUE ZEROS.                                 
002100*                                 ANTAL I FÖRPACKNING                     
002200*                                 GÄLLER FÖR EXTRAEMBALLAGEN              
002300        05 EMB-KDEMBKOD      PIC Z(2)9                                    
002400                             VALUE ZEROS.                                 
002500*                                 EMBALLAGEKOD                            
002600*** END OF VILMAII-COPY LENGTH= 28 BYTES                                  
