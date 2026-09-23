000100 01  AVSO-W413AVSO.                                                       
000200*                                 LÄNKAREA TILL W413AVSO -                
000300*                                 WOPS ORDERAVSLUT                        
000400     03 AVSO-IDGMTREF.                                                    
000500*                                 GODSMOTTAGAREREFERENS                   
000600        05 AVSO-IDDISTR      PIC S9(5)           COMP-3.                  
000700*                                 DISTRIKTNUMMER                          
000800        05 AVSO-IDKUNDNR     PIC S9(7)           COMP-3.                  
000900*                                 KUNDNUMMER                              
001000        05 AVSO-IDKUNDRF-GRP.                                             
001100*                                 KUNDENS REFERENS (ORDERID)              
001200           07 AVSO-IDKUNDRF  PIC X(10).                                   
001300*                                 KUNDENS REFERENS (ORDERID)              
001400           07 AVSO-IDORDNR5-FILLER REDEFINES AVSO-IDKUNDRF.               
001500              09 AVSO-IDORDNR5                                            
001600                             PIC 9(5).                                    
001700*                                 ORDERNUMMER                             
001800              09 FILLER      PIC X(5).                                    
001900           07 AVSO-IDORDNR7-FILLER REDEFINES AVSO-IDKUNDRF.               
002000              09 AVSO-IDORDNR7                                            
002100                             PIC 9(7).                                    
002200*                                 ORDERNUMMER                             
002300              09 FILLER      PIC X(3).                                    
002400     03 AVSO-IDORDER         PIC S9(7)           COMP-3.                  
002500*                                 VOLVO PARTS ORDERNUMMER                 
002600     03 AVSO-IDDC            PIC X(2).                                    
002700*                                 IDENTIFIERARE LAGER                     
002800     03 AVSO-TIRFS           PIC S9(11)          COMP-3.                  
002900*                                 KLART FÖR TRANSPORT ÅÅMMDDTTMM          
003000     03 AVSO-TITRPAVT.                                                    
003100*                                 TRANSPORTAVGÅNGSTIDPUNKT                
003200        05 AVSO-TIAAMMDD     PIC S9(7)           COMP-3.                  
003300*                                 ÅR - MÅNAD - DAG  (ÅÅMMDD)              
003400        05 AVSO-TIHHMM       PIC S9(5)           COMP-3.                  
003500*                                 KLOCKSLAG (TIMMAR OCH MINUTER)          
003600     03 AVSO-KDSVAR          PIC X.                                       
003700*                                 SVAR FRÅN PROGRAM ELLER SKÄRM           
003800     03 AVSO-IDTRANS         PIC X(4).                                    
003900*                                 BILDNUMMER                              
004000*** END OF VILMAII-COPY LENGTH= 41 BYTES                                  
