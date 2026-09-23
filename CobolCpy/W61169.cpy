000100 01  W61169.                                                              
000200*                                 POSTER TILL VOLVO TRANSPORT             
000300*                                                                         
000400*                                 VÄRDE PER INLEVERERAD ARTIKEL           
000500*                                                    TILL CDC             
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
001700     03 KVANTMOT             PIC S9(7)           COMP-3.                  
001800*                                 ANTAL MOTTAGET                          
001900*                                 QUANTITY RECEIVED                       
002000     03 SUARTBES             PIC S9(9)V9(2)      COMP-3.                  
002100*                                 SUMMA RADVÄRDE BEST.PRIS                
002200*                                 SUM LINEVALUE ORDERPRICE                
002300     03 KDSORT               PIC X(2).                                    
002400*                                 SORT-KOD                                
002500*                                 UNIT OF MEASURE                         
002600     03 TIAAMMDD-GAELL       PIC S9(7)           COMP-3.                  
002700*                                 GÄLLANDE DATUM                          
002800     03 IDLEVNR              PIC X(5).                                    
002900*                                 LEVERANTÖRNUMMER                        
003000*                                 SUPPLIER NUMBER (VENDOR NUMBER)         
003100     03 KDRT                 PIC S9(3)           COMP-3.                  
003200*                                 REDOVISNINGSTYP                         
003300*                                 TYPE OF ACCOUNTING                      
003400*** END OF VILMAII-COPY LENGTH= 28 BYTES                                  
