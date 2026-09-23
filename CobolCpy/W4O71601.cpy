000100 01  MOD-W4O71601.                                                        
000200*                                 MOD-COPYTEXT FÖR W4071600               
000300     03 MOD-IDTRANS          PIC X(4).                                    
000400*                                 BILDNUMMER                              
000500     03 MOD-TEMFSFEL         PIC X(40).                                   
000600*                                 MFS FELMEDDELANDE                       
000700     03 MOD-IDDISTR-IN       PIC X(4).                                    
000800*                                 DISTRIKTNUMMER                          
000900     03 MOD-IDDISTR-UT       PIC X(4).                                    
001000*                                 DISTRIKTNUMMER                          
001100     03 MOD-IDKUNDNR-IN      PIC X(6).                                    
001200*                                 KUNDNUMMER                              
001300     03 MOD-IDKUNDNR-UT      PIC X(6).                                    
001400*                                 KUNDNUMMER                              
001500     03 MOD-IDRAPPNR-IN      PIC X(7).                                    
001600*                                 RAPPORT NUMMER                          
001700     03 MOD-IDRAPPNR-UT      PIC X(7).                                    
001800*                                 RAPPORT NUMMER                          
001900     03 MOD-IDARTNR-IN       PIC X(8).                                    
002000*                                 ARTIKELNUMMER                           
002100     03 MOD-IDARTNR-UT       PIC X(8).                                    
002200*                                 ARTIKELNUMMER                           
002300     03 MOD-IDRADNR-IN       PIC X(4).                                    
002400*                                 RADNUMMER                               
002500     03 MOD-IDRADNR-UT       PIC X(4).                                    
002600*                                 RADNUMMER                               
002700     03 MOD-SUACKFSG-KRE-SPAR                                             
002800                             PIC 9(9)V9(2).                               
002900*                                 ACKUMULERAD FÖRSÄLJNING                 
003000*                                 (FÖRSÄLJNINGSVÄRDE)                     
003100     03 MOD-SUACKFSG-DEB-SPAR                                             
003200                             PIC 9(9)V9(2).                               
003300*                                 ACKUMULERAD FÖRSÄLJNING                 
003400*                                 (FÖRSÄLJNINGSVÄRDE)                     
003500     03 MOD-IDARTNR-ENTER    PIC 9(8).                                    
003600*                                 ARTIKELNUMMER                           
003700     03 MOD-IDRADNR-ENTER    PIC 9(5).                                    
003800*                                 RADNUMMER                               
003900     03 MOD-IDARTNR-NEXT     PIC 9(8).                                    
004000*                                 ARTIKELNUMMER                           
004100     03 MOD-IDRADNR-NEXT     PIC 9(5).                                    
004200*                                 RADNUMMER                               
004300     03 MOD-INFO-RAD         OCCURS 11 TIMES.                             
004400*                                 RADINFORMATION                          
004500        05 MOD-IDORDNR       PIC Z(4)9.                                   
004600*                                 ORDERNUMMER UTGÅR PD90                  
004700        05 FILLER            PIC X.                                       
004800        05 MOD-IDARTNR       PIC Z(8)9.                                   
004900*                                 ARTIKELNUMMER                           
005000        05 FILLER            PIC X(2).                                    
005100        05 MOD-KDANMORS      PIC X(2).                                    
005200*                                 ORSAK TILL LEVERANSANMÄRKNING           
005300        05 FILLER            PIC X.                                       
005400        05 MOD-KVLEVANM      PIC Z(5)9.                                   
005500*                                 LEVERANSANMÄRKNINGSANTAL                
005600        05 MOD-PRARTBTO      PIC Z(6)9.9(2).                              
005700*                                 FÖRSÄLJNINGSPRIS BRUTTO (KR)            
005800        05 FILLER            PIC X.                                       
005900        05 MOD-SUARTOMK-TK   PIC Z(8)9.9(2).                              
006000*                                 SUMMA OMKOSTNADER PER ARTIKEL           
006100*                                                                         
006200        05 FILLER            PIC X.                                       
006300        05 MOD-SUARTFSG-RAD  PIC Z(8)9.9(2).                              
006400*                                 SUMMA FÖRSÄLJNING PER ARTIKEL           
006500*                                                                         
006600        05 FILLER            PIC X.                                       
006700        05 MOD-IDKNOTNR      PIC Z(6)9.                                   
006800*                                 KREDITNOTANUMMER                        
006900        05 FILLER            PIC X.                                       
007000        05 MOD-TIKRENOT      PIC 9(6).                                    
007100*                                 DATUM KREDITNOTA                        
007200     03 MOD-SUACKFSG-KRE-TOT PIC Z(8)9.9(2).                              
007300*                                 ACKUMULERAD FÖRSÄLJNING                 
007400*                                 (FÖRSÄLJNINGSVÄRDE)                     
007500     03 MOD-SUACKFSG-DEB-TOT PIC Z(8)9.9(2).                              
007600*                                 ACKUMULERAD FÖRSÄLJNING                 
007700*                                 (FÖRSÄLJNINGSVÄRDE)                     
007800     03 MOD-KDVALISO         PIC X(3).                                    
007900*                                 VALUTAKOD ENLIGT ISO-STANDARD.          
008000     03 MOD-TEMFSINF         PIC X(55).                                   
008100*                                 INFORMATIONSMEDDELANDE                  
008200*** END OF VILMAII-COPY LENGTH= 1079 BYTES                                
