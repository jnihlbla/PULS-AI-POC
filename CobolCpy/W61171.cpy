000100 01  W61171-CTX.                                                          
000200*                                 POSTER TILL VOLVO TRANSPORT             
000300*                                                                         
000400*                                 VÄRDE PER INLEVERERAD ARTIKEL           
000500*                                 FRÅN ANNAT EU-LAND TILL CDC             
000600*                                 SENASTE MÅNADEN.                        
000700*                                                                         
000800*                                 ITEM TO VOLVO TRANSPORT                 
000900*                                                                         
001000*                                 VALUE PER PART NO                       
001100*                                 DELIVERED TO CDC DURING LAST            
001200*                                 MONTH                                   
001300*                                                                         
001400     03 IDARTNR              PIC S9(9)           COMP-3.                  
001500*                                 ARTIKELNUMMER                           
001600*                                 PART NUMBER                             
001700     03 IDLANDX2             PIC X(2).                                    
001800*                                 2-STÄLLIG LANDSBETECKNINGSKOD           
001900*                                 2-LETTER CODE FOR COUNTRY               
002000     03 KVANTMOT             PIC S9(7)           COMP-3.                  
002100*                                 ANTAL MOTTAGET                          
002200*                                 QUANTITY RECEIVED                       
002300     03 SUARTBES             PIC S9(9)V9(2)      COMP-3.                  
002400*                                 SUMMA RADVÄRDE BEST.PRIS                
002500*                                 SUM LINEVALUE ORDERPRICE                
002600     03 KDSORT               PIC X(2).                                    
002700*                                 SORT-KOD                                
002800*                                 UNIT OF MEASURE                         
002900     03 TIAAMMDD-GAELL       PIC S9(7)           COMP-3.                  
003000*                                 GÄLLANDE DATUM                          
003100*** END OF VILMAII-COPY LENGTH= 23 BYTES                                  
