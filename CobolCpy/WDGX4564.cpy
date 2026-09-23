000100 01  4564-WDGX4564-CTX.                                                   
000200*                                 RESTORDER NOTERINGAR                    
000300*                                 FYSISK NYCKEL: KY4564                   
000400*                                 (IDGMTREF + IDARTNR + IDLOPNR)          
000500     03 4564-IDGMTREF.                                                    
000600*                                 GODSMOTTAGAREREFERENS                   
000700*                                 GOODS RECEIVER REFERENS                 
000800        05 4564-IDDISTR      PIC S9(5)           COMP-3.                  
000900*                                 DISTRIKTNUMMER                          
001000*                                 DISTRICT NUMBER                         
001100        05 4564-IDKUNDNR     PIC S9(7)           COMP-3.                  
001200*                                 KUNDNUMMER                              
001300*                                 CUSTOMER NO                             
001400        05 4564-IDKUNDRF-GRP.                                             
001500*                                 KUNDENS REFERENS (ORDERID)              
001600*                                 CUSTOMER REFERENCE (ORDER ID)           
001700           07 4564-IDKUNDRF  PIC X(10).                                   
001800*                                 KUNDENS REFERENS (ORDERID)              
001900*                                 CUSTOMER REFERENCE (ORDER ID)           
002000           07 4564-IDORDNR5-FILLER REDEFINES 4564-IDKUNDRF.               
002100              09 4564-IDORDNR5                                            
002200                             PIC 9(5).                                    
002300*                                 ORDERNUMMER                             
002400*                                 ORDER NUMBER                            
002500              09 FILLER      PIC X(5).                                    
002600           07 4564-IDORDNR7-FILLER REDEFINES 4564-IDKUNDRF.               
002700              09 4564-IDORDNR7                                            
002800                             PIC 9(7).                                    
002900*                                 ORDERNUMMER                             
003000*                                 ORDER NUMBER                            
003100              09 FILLER      PIC X(3).                                    
003200     03 4564-IDARTNR         PIC S9(9)           COMP-3.                  
003300*                                 ARTIKELNUMMER                           
003400*                                 PART NUMBER                             
003500     03 4564-IDLOPNR         PIC S9(3)           COMP-3.                  
003600*                                 LÖPNUMMER                               
003700*                                 SEQUENCE NUMBER                         
003800     03 4564-TIREPDAT        PIC S9(7)           COMP-3.                  
003900*                                 REPAIR DATE                             
004000*                                 REPAIR DATE                             
004100     03 4564-BETEXT-010      PIC X(10).                                   
004200*** END OF VILMAII-COPY LENGTH= 38 BYTES                                  
