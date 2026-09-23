000100 01  GLO-WDT211.                                                          
000200*                                 GATE REGISTER                           
000300*                                 UNDANTAGS GATE PER LAGEROMRÅDE          
000400*                                 FYSISK NYCKEL: WDT211KY                 
000500*                                 (ADLAGOMR + TIAAVV-FOM)                 
000600     03 GLO-ADLAGOMR         PIC S9(3)           COMP-3.                  
000700*                                 LAGEROMRÅDE                             
000800*                                 AREA                                    
000900     03 GLO-TIAAVV-FOM       PIC S9(5)           COMP-3.                  
001000*                                 ÅR - VECKA  (ÅÅVV)                      
001100*                                 YEAR - WEEK  (YYWW)                     
001200     03 GLO-ADINPORT-LO      PIC X(8).                                    
001300*                                 UNDANTAG AVLASTNING PORT PER LO         
001400*                                 EXCEPTION GATE PER AREA                 
001500     03 GLO-IDUSER           PIC X(8).                                    
001600*                                 ANVÄNDARENS SÄKERHETS ID                
001700*                                 USER SECURITY-IDENTITY                  
001800     03 GLO-TIUPPDAT         PIC S9(7)           COMP-3.                  
001900*                                 UPPDATERINGSDATUM  (ÅÅMMDD)             
002000*                                 UPDATING DATE     (YYMMDD)              
002100*** END OF VILMAII-COPY LENGTH= 25 BYTES                                  
