000100 01  GFT-WDT212.                                                          
000200*                                 GATE REGISTER                           
000300*                                 UNDANTAGS GATE PER FÖRPACKN.TYP         
000400*                                 FYSISK NYCKEL: WDT212KY                 
000500*                                 (BEFT + TIAAVV-FOM)                     
000600     03 GFT-BEFT             PIC 9(2).                                    
000700*                                 FÖRPACKNINGSTYP                         
000800*                                 PACKAGING TYPE                          
000900     03 GFT-TIAAVV-FOM       PIC S9(5)           COMP-3.                  
001000*                                 ÅR - VECKA  (ÅÅVV)                      
001100*                                 YEAR - WEEK  (YYWW)                     
001200     03 GFT-ADINPORT-FT      PIC X(8).                                    
001300*                                 UNDANTAG AVLASTN./FÖRPACKN.TYP          
001400*                                 EXCEPTION GATE / PACKAGING TYPE         
001500     03 GFT-IDUSER           PIC X(8).                                    
001600*                                 ANVÄNDARENS SÄKERHETS ID                
001700*                                 USER SECURITY-IDENTITY                  
001800     03 GFT-TIUPPDAT         PIC S9(7)           COMP-3.                  
001900*                                 UPPDATERINGSDATUM  (ÅÅMMDD)             
002000*                                 UPDATING DATE     (YYMMDD)              
002100*** END OF VILMAII-COPY LENGTH= 25 BYTES                                  
