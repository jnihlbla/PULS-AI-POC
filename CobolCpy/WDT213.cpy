000100 01  GART-WDT213.                                                         
000200*                                 GATE REGISTER                           
000300*                                 UNDANTAGS GATE PER ARTIKEL              
000400*                                 FYSISK NYCKEL: WDT213KY                 
000500*                                 (IDARTNR + TIAAVV-FOM)                  
000600     03 GART-IDARTNR         PIC S9(9)           COMP-3.                  
000700*                                 ARTIKELNUMMER                           
000800*                                 PART NUMBER                             
000900     03 GART-TIAAVV-FOM      PIC S9(5)           COMP-3.                  
001000*                                 ÅR - VECKA  (ÅÅVV)                      
001100*                                 YEAR - WEEK  (YYWW)                     
001200     03 GART-ADINPORT-ART    PIC X(8).                                    
001300*                                 UNDANTAG AVLASTN.PORT ARTIKEL           
001400*                                 EXCEPTION GATE PER PART NUMBER          
001500     03 GART-IDUSER          PIC X(8).                                    
001600*                                 ANVÄNDARENS SÄKERHETS ID                
001700*                                 USER SECURITY-IDENTITY                  
001800     03 GART-TIUPPDAT        PIC S9(7)           COMP-3.                  
001900*                                 UPPDATERINGSDATUM  (ÅÅMMDD)             
002000*                                 UPDATING DATE     (YYMMDD)              
002100*** END OF VILMAII-COPY LENGTH= 28 BYTES                                  
