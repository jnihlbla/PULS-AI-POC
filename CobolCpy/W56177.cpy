000100 01  W56177.                                                              
000200*                                 LATEST DYNAMIC BOOKING EVENTS           
000300     03 IDDC-REC             PIC X(2).                                    
000400*                                 IDENTIFIERARE LAGER                     
000500     03 TIREGDAT             PIC S9(7)           COMP-3.                  
000600*                                 REGISTRERINGSDATUM (ÅÅMMDD)             
000700     03 IDVERGL              PIC X(10).                                   
000800*                                 VERIFIKATIONSID FÖR HUVUDBOKEN          
000900     03 PRKURS               PIC S9(6)V9(5)      COMP-3.                  
001000*                                 VALUTAKURS                              
001100     03 FREIGHT-MODE         PIC X(25).                                   
001200     03 TOTAL-PRICE          PIC S9(7)V9(2)      COMP-3.                  
001300*                                 ARTIKELPRIS NETTO                       
001400     03 PRFOERS              PIC S9(7)V9(2)      COMP-3.                  
001500*                                 FÖRSÄKRINGSPREMIE                       
001600     03 PREMBHNT             PIC S9(7)V9(2)      COMP-3.                  
001700*                                 EMBALLAGE O HANTERINGSKOST              
001800     03 PRFRAKT              PIC S9(7)V9(2)      COMP-3.                  
001900*                                 FRAKTKOSTNAD                            
002000     03 LINE-DDI             PIC S9(9)V9(2).                              
002100*                                 SUMMABELOPP                             
002200     03 LINE-SUM             PIC S9(9)V9(2).                              
002300*                                 SUMMABELOPP                             
002400*** END OF VILMAII-COPY LENGTH= 89 BYTES                                  
