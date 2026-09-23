000100 01  4472-WDGX4472.                                                       
000200*                                 ORDERK÷N/PRC OCH RFS                    
000300*                                 FYSISK NYCKEL                           
000400*                                 KDSEGKEY = 1                            
000500     03 4472-KDSEGKEY        PIC X.                                       
000600*                                 TEKNISK SEGMENT-NYCKEL                  
000700*                                 TECHNICAL SEGMENT KEY                   
000800     03 4472-KVRADER-DAG     PIC S9(5)           COMP-3.                  
000900*                                 ANTAL RADER IDAG.                       
001000*                                 NUMBER OF LINES TODAY                   
001100     03 4472-SUPTID-DAG      PIC S9(3)V9(2)      COMP-3.                  
001200*                                 TOTAL PRODUKTIONSTID IDAG               
001300*                                 TOTAL PRODUCTIONTIME TODAY              
001400     03 4472-RFSDAG          OCCURS 30 TIMES.                             
001500        05 4472-TIRFS        PIC S9(11)          COMP-3.                  
001600*                                 KLART F÷R TRANSPORT ≈≈MMDDTTMM          
001700*                                 READY FOR SHIPMENT  YYMMDDHHMM          
001800        05 4472-SUPTID       PIC S9(3)V9(2)      COMP-3.                  
001900*                                 TOTAL PRODUKTIONSTID TIM+MIN            
002000*                                 TOTAL PRODUCTIONTIME HOUR MIN.          
002100        05 4472-KVRADER      PIC S9(5)           COMP-3.                  
002200*                                 ANTAL RADER                             
002300*                                 NUMBER OF LINES                         
002400        05 4472-SHIFT        OCCURS 3 TIMES.                              
002500           07 4472-IDSHIFT   PIC X.                                       
002600*                                 SHIFT IDENTITET                         
002700*                                 SHIFT IDENTITY                          
002800           07 4472-SUPTID-PRAPP                                           
002900                             PIC S9(3)V9(2)      COMP-3.                  
003000*                                 TOTAL PRODUKTIONSTID PACKRAPP           
003100*                                 TOTAL PRODUCTIONTIME PACKREP            
003200           07 4472-KVRADER-PRAPP                                          
003300                             PIC S9(5)           COMP-3.                  
003400*                                 ANTAL RADER PACKRAPPORTERAT             
003500*                                 NUMBER OF LINES PACKREPORTED            
003600     03 4472-FILLER          PIC X(3).                                    
003700*** END COPY WDGX4472C0  LENGTH=1000                                      
