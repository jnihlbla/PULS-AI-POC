000100 01  MID-W4I51401.                                                        
000200*                                 MID-COPYTEXT FÖR W4I51401               
000300     03 MID-INPUT.                                                        
000400*                                 NYCKLAR IN FÖR W4I51401                 
000500        05 MID-IDDISTR-IN    PIC X(4).                                    
000600*                                 DISTRIKTNUMMER                          
000700        05 MID-IDDISTR-UT    PIC X(4).                                    
000800*                                 DISTRIKTNUMMER                          
000900        05 MID-IDKUNDNR-IN   PIC X(6).                                    
001000*                                 KUNDNUMMER                              
001100        05 MID-IDKUNDNR-UT   PIC X(6).                                    
001200*                                 KUNDNUMMER                              
001300        05 MID-IDDC-IN       PIC X(2).                                    
001400*                                 IDENTIFIERARE LAGER                     
001500        05 MID-IDDC-UT       PIC X(2).                                    
001600*                                 IDENTIFIERARE LAGER                     
001700        05 MID-KDORDKL-IN    PIC X.                                       
001800*                                 ORDERKLASS                              
001900        05 MID-KDORDKL-UT    PIC X.                                       
002000*                                 ORDERKLASS                              
002100        05 MID-KDFRAKT-IN    PIC X(2).                                    
002200*                                 FRAKTSÄTT DC TILL KUND                  
002300        05 MID-KDFRAKT-UT    PIC X(2).                                    
002400*                                 FRAKTSÄTT DC TILL KUND                  
002500        05 MID-KDSTATUS-IN   PIC X.                                       
002600*                                 VOLVOORDERSTATUS                        
002700        05 MID-KDSTATUS-UT   PIC X.                                       
002800*                                 VOLVOORDERSTATUS                        
002900     03 MID-INPUT            OCCURS 13 TIMES.                             
003000*                                 INMATNINGSFÄLT                          
003100        05 MID-IDTRANS       PIC X(4).                                    
003200*                                 BILDNUMMER                              
003300        05 MID-IDKUNDNR      PIC X(6).                                    
003400*                                 KUNDNUMMER                              
003500        05 FILLER            PIC X.                                       
003600        05 MID-IDORDNR7      PIC X(7).                                    
003700*                                 ORDERNUMMER                             
003800        05 FILLER            PIC X(60).                                   
003900*** END OF VILMAII-COPY LENGTH= 1046 BYTES                                
