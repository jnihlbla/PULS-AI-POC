000100 01  MID-W4I51701.                                                        
000200*                                 COPYTEXT FÖR MID W4I51701               
000300*                                                                         
000400     03 MID-IDDISTR-IN       PIC X(4).                                    
000500*                                 DISTRIKTNUMMER                          
000600     03 MID-IDDISTR-UT       PIC X(4).                                    
000700*                                 DISTRIKTNUMMER                          
000800     03 MID-IDKUNDNR-IN      PIC X(6).                                    
000900*                                 KUNDNUMMER                              
001000     03 MID-IDKUNDNR-UT      PIC X(6).                                    
001100*                                 KUNDNUMMER                              
001200     03 MID-IDDC-IN          PIC X(2).                                    
001300*                                 IDENTIFIERARE LAGER                     
001400     03 MID-IDDC-UT          PIC X(2).                                    
001500*                                 IDENTIFIERARE LAGER                     
001600     03 MID-KDORDKL-IN       PIC X.                                       
001700*                                 ORDERKLASS                              
001800     03 MID-KDORDKL-UT       PIC X.                                       
001900*                                 ORDERKLASS                              
002000     03 MID-KDFRAKT-IN       PIC X(2).                                    
002100*                                 FRAKTSÄTT C1-C2 TILL KUND               
002200     03 MID-KDFRAKT-UT       PIC X(2).                                    
002300*                                 FRAKTSÄTT C1-C2 TILL KUND               
002400     03 MID-KDORDSTA-IN      PIC X.                                       
002500*                                 VOLVOORDERSTATUS                        
002600     03 MID-KDORDSTA-UT      PIC X.                                       
002700*                                 VOLVOORDERSTATUS                        
002800     03 MID-IDKUNDNR-ENTER   PIC 9(6).                                    
002900*                                 KUNDNUMMER                              
003000     03 MID-IDKUNDNR-NEXT    PIC 9(6).                                    
003100*                                 KUNDNUMMER                              
003200     03 MID-IDPRODNR-ENTER   PIC 9(7).                                    
003300*                                 PRODUKTIONSNUMMER                       
003400     03 MID-IDPRODNR-NEXT    PIC 9(7).                                    
003500*                                 PRODUKTIONSNUMMER                       
003600     03 MID-IDKUNDRF-ENTER   PIC X(10).                                   
003700*                                 KUNDENS REFERENS (ORDERID)              
003800     03 MID-IDKUNDRF-NEXT    PIC X(10).                                   
003900*                                 KUNDENS REFERENS (ORDERID)              
004000     03 MID-INPUT            OCCURS 14 TIMES.                             
004100*                                 INMATNINGSFÄLT                          
004200        05 MID-IDTRANS       PIC X(4).                                    
004300*                                 BILDNUMMER                              
004400        05 MID-IDKUNDNR      PIC X(6).                                    
004500*                                 KUNDNUMMER                              
004600        05 MID-IDORDNR7      PIC X(7).                                    
004700*                                 ORDERNUMMER                             
004800*** END OF VILMAII-COPY LENGTH= 316 BYTES                                 
