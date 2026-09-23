000100 01  PRES-W403PRES.                                                       
000200*                                 3IV PRINT RESULT POST                   
000300*                                 3IV PRINT RESULT RECORD                 
000400*                                 IDRTYP3IV="PrintResult"                 
000500     03 PRES-IDRTYP3IV       PIC X(30).                                   
000600*                                 3IV RECORD-TYP                          
000700*                                 3IV RECORD TYPE                         
000800     03 PRES-IDPRODNR        PIC Z(6)9.                                   
000900*                                 PRODUKTIONSNUMMER                       
001000*                                 PRODUCTION NUMBER                       
001100     03 PRES-IDPLKLST        PIC 9(3).                                    
001200*                                 PLOCKLISTNUMMER                         
001300*                                 PICKING LIST NUMBER                     
001400     03 PRES-IDLOPNR-ORD     PIC Z(2)9.                                   
001500*                                 ORDERNS ORDNINGSNUMMER INOM             
001600*                                 EN PLOCKSATS                            
001700*                                 SEQUENCE-NUMBER FOR AN ORDER            
001800*                                 WITHIN A PICKING UNIT                   
001900     03 PRES-IDKOLLI         PIC Z(4)9.                                   
002000*                                 KOLLINUMMER                             
002100*                                 CASE NUMBER                             
002200     03 PRES-KDPCD3IV        PIC 9(3).                                    
002300*                                 KONTROLLSIFFOR FÖR FÖLJESEDEL           
002400*                                 CHECK DIGITS FOR DELIVER NOTE           
002500*** END OF VILMAII-COPY LENGTH= 51 BYTES                                  
