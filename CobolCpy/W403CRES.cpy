000100 01  CRES-W403CRES.                                                       
000200*                                 3IV PICK TASK POST                      
000300*                                 3IV PICK TASK RECORD                    
000400*                                 IDRTYP3IV="PickTask"                    
000500     03 CRES-IDRTYP3IV       PIC X(30).                                   
000600*                                 3IV RECORD-TYP                          
000700*                                 3IV RECORD TYPE                         
000800     03 CRES-IDPRODNR        PIC Z(6)9.                                   
000900*                                 PRODUKTIONSNUMMER                       
001000*                                 PRODUCTION NUMBER                       
001100     03 CRES-IDPLKLST        PIC 9(3).                                    
001200*                                 PLOCKLISTNUMMER                         
001300*                                 PICKING LIST NUMBER                     
001400     03 CRES-IDLOPNR-ORD     PIC Z(2)9.                                   
001500*                                 ORDERNS ORDNINGSNUMMER INOM             
001600*                                 EN PLOCKSATS                            
001700*                                 SEQUENCE-NUMBER FOR AN ORDER            
001800*                                 WITHIN A PICKING UNIT                   
001900     03 CRES-IDKOLLI         PIC Z(4)9.                                   
002000*                                 KOLLINUMMER                             
002100*                                 CASE NUMBER                             
002200     03 CRES-ADFLLOC.                                                     
002300*                                 FÄRDIGLAGERPLATS                        
002400*                                 SHIPMENT AREA LOCATION                  
002500        05 CRES-ADFLGEO      PIC X(3).                                    
002600*                                 GEOGRAFISKT OMRÅDE FÄRDIGLAGER          
002700*                                 GEOGRAPHIC AREA                         
002800        05 CRES-ADFLOMR      PIC 9(3).                                    
002900*                                 LASTNINGSOMR/STÄLL FÄRDIGLAGER          
003000*                                 DELIVERY AREA                           
003100        05 CRES-ADRUTNIV     PIC 9(3).                                    
003200*                                 RUTA/NIVÅ I FÄRDIGLAGRET                
003300*                                 SQUARE/LEVEL IN DEL. AREA               
003400*** END OF VILMAII-COPY LENGTH= 57 BYTES                                  
