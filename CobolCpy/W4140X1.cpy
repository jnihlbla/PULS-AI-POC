000100 01  0X1-W4140X1.                                                         
000200*                                 ORDERHUVUDTRANSAKTIONER                 
000300*                                 FÖR SPX, POSTTYP 01X                    
000400     03 0X1-IDPTYP           PIC X(3).                                    
000500*                                 POSTTYP                                 
000600*                                 RECORD TYPE                             
000700     03 0X1-IDSYSTEM         PIC X(4).                                    
000800*                                 VOLVO VCCS SYSTEMNUMMER                 
000900*                                 VOLVO VCCS SYSTEM NUMBER                
001000     03 0X1-IDDISTR          PIC S9(5)           COMP-3.                  
001100*                                 DISTRIKTNUMMER                          
001200*                                 DISTRICT NUMBER                         
001300     03 0X1-IDKUNDNR         PIC S9(7)           COMP-3.                  
001400*                                 KUNDNUMMER                              
001500*                                 CUSTOMER NO                             
001600     03 0X1-IDKUNDRF         PIC X(10).                                   
001700*                                 KUNDENS REFERENS (ORDERID)              
001800*                                 CUSTOMER REFERENCE (ORDER ID)           
001900     03 0X1-IDDC             PIC X(2).                                    
002000*                                 IDENTIFIERARE LAGER                     
002100*                                 WAREHOUSE IDENTIFIER                    
002200     03 0X1-KDORDKL          PIC S9              COMP-3.                  
002300*                                 ORDERKLASS                              
002400*                                 ORDER CLASS                             
002500     03 0X1-KDFRAKT          PIC S9(3)           COMP-3.                  
002600*                                 FRAKTSÄTT DC TILL KUND                  
002700*                                 FREIGHT CODE                            
002800     03 0X1-TIREGDAT         PIC S9(7)           COMP-3.                  
002900*                                 REGISTRERINGSDATUM (ÅÅMMDD)             
003000*                                 REGISTRATION DATE (YYMMDD)              
003100     03 0X1-TIRFS            PIC S9(11)          COMP-3.                  
003200*                                 KLART FÖR TRANSPORT ÅÅMMDDTTMM          
003300*                                 READY FOR SHIPMENT  YYMMDDHHMM          
003400     03 0X1-BEKUNDRF         PIC X(15).                                   
003500*                                 KUNDENS REFERENS                        
003600*                                 CUSTOMERS REFERENCE                     
003700*** END OF VILMAII-COPY LENGTH= 54 BYTES                                  
