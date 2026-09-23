000100 01  PRIS-W403PRIS.                                                       
000200*                                 3IV PRINT SUMMARY POST                  
000300*                                 3IV PRINT SUMMARY RECORD                
000400*                                 IDRTYP3IV="PrintSummary"                
000500     03 PRIS-IDRTYP3IV       PIC X(30).                                   
000600*                                 3IV RECORD-TYP                          
000700*                                 3IV RECORD TYPE                         
000800     03 PRIS-IDPRODNR        PIC 9(7).                                    
000900*                                 PRODUKTIONSNUMMER                       
001000*                                 PRODUCTION NUMBER                       
001100     03 PRIS-IDPLKLST        PIC 9(3).                                    
001200*                                 PLOCKLISTNUMMER                         
001300*                                 PICKING LIST NUMBER                     
001400     03 PRIS-IDLOPNR-ORD     PIC 9(3).                                    
001500*                                 ORDERNS ORDNINGSNUMMER INOM             
001600*                                 EN PLOCKSATS                            
001700*                                 SEQUENCE-NUMBER FOR AN ORDER            
001800*                                 WITHIN A PICKING UNIT                   
001900     03 PRIS-IDKOLLI         PIC 9(5).                                    
002000*                                 KOLLINUMMER                             
002100*                                 CASE NUMBER                             
002200     03 PRIS-KDPRTVAL        PIC 9(2).                                    
002300*                                 PRINTER-VAL KOD                         
002400*                                 PRINTER SELECT CODE                     
002500*** END OF VILMAII-COPY LENGTH= 50 BYTES                                  
