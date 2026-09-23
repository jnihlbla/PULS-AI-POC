000100 01  MOD-W4O72401.                                                        
000200*                                 MOD-COPYTEXT FÖR W4072400               
000300     03 MOD-IDTRANS          PIC X(4).                                    
000400*                                 BILDNUMMER                              
000500     03 MOD-TEMFSFEL         PIC X(40).                                   
000600*                                 MFS FELMEDDELANDE                       
000700     03 MOD-IDARTNR-IN       PIC X(9).                                    
000800*                                 ARTIKELNUMMER                           
000900     03 MOD-IDARTNR-UT       PIC X(9).                                    
001000*                                 ARTIKELNUMMER                           
001100     03 MOD-IDDISTR-IN       PIC X(4).                                    
001200*                                 DISTRIKTNUMMER                          
001300     03 MOD-IDDISTR-UT       PIC X(4).                                    
001400*                                 DISTRIKTNUMMER                          
001500     03 MOD-IDKUNDNR-IN      PIC X(6).                                    
001600*                                 KUNDNUMMER                              
001700     03 MOD-IDKUNDNR-UT      PIC X(6).                                    
001800*                                 KUNDNUMMER                              
001900     03 MOD-KDANMORS-IN      PIC X(2).                                    
002000*                                 ORSAK TILL LEVERANSANMÄRKNING           
002100     03 MOD-KDANMORS-UT      PIC X(2).                                    
002200*                                 ORSAK TILL LEVERANSANMÄRKNING           
002300     03 MOD-IDDC-IN          PIC X(2).                                    
002400*                                 IDENTIFIERARE LAGER                     
002500     03 MOD-IDDC-UT          PIC X(2).                                    
002600*                                 IDENTIFIERARE LAGER                     
002700     03 MOD-IDDISTR-ENTER    PIC 9(4).                                    
002800*                                 DISTRIKTNUMMER                          
002900     03 MOD-IDKUNDNR-ENTER   PIC 9(6).                                    
003000*                                 KUNDNUMMER                              
003100     03 MOD-IDRAPPNR-ENTER   PIC 9(7).                                    
003200*                                 RAPPORT NUMMER                          
003300     03 MOD-IDRADNR-ENTER    PIC 9(5).                                    
003400*                                 RADNUMMER                               
003500     03 MOD-IDDISTR-NEXT     PIC 9(4).                                    
003600*                                 DISTRIKTNUMMER                          
003700     03 MOD-IDKUNDNR-NEXT    PIC 9(6).                                    
003800*                                 KUNDNUMMER                              
003900     03 MOD-IDRAPPNR-NEXT    PIC 9(7).                                    
004000*                                 RAPPORT NUMMER                          
004100     03 MOD-IDRADNR-NEXT     PIC 9(5).                                    
004200*                                 RADNUMMER                               
004300     03 MOD-RADER            OCCURS 13 TIMES.                             
004400*                                 RADINFORMATION                          
004500        05 MOD-KDCMDVAL-ATTR PIC X(2).                                    
004600*                                 MFS ATTRIBUTFÄLT                        
004700        05 MOD-KDCMDVAL      PIC X(3).                                    
004800*                                 GENERELL KOMMANDOKOD                    
004900        05 MOD-IDDISTR       PIC Z(3)9.                                   
005000*                                 DISTRIKTNUMMER                          
005100        05 MOD-IDKUNDNR      PIC Z(5)9.                                   
005200*                                 KUNDNUMMER                              
005300        05 MOD-IDRAPPNR      PIC Z(6)9.                                   
005400*                                 RAPPORT NUMMER                          
005500        05 MOD-IDDC          PIC X(2).                                    
005600*                                 IDENTIFIERARE LAGER                     
005700        05 MOD-TILEVANM      PIC 9(6).                                    
005800*                                 DATUM LEVERANSANMÄRKNING                
005900        05 MOD-KDANMORS      PIC X(2).                                    
006000*                                 ORSAK TILL LEVERANSANMÄRKNING           
006100        05 MOD-KDKREBEH      PIC X(3).                                    
006200*                                 BEHANDLINGSSTATUS                       
006300        05 MOD-KVLEVANM      PIC Z(5)9.                                   
006400*                                 LEVERANSANMÄRKNINGSANTAL                
006500        05 MOD-DATUM-TEXT    PIC X(3).                                    
006600        05 MOD-TIRETILL      PIC 9(6).                                    
006700*                                 RETURTILLSTÅNDSDATUM                    
006800        05 MOD-KVRETINL      PIC Z(5)9.                                   
006900*                                 INLAGT ANTAL VID RETUR                  
007000        05 MOD-KVRETINL-SKR  PIC Z(5)9.                                   
007100*                                 INRPT ANTAL SOM SKROTATS                
007200        05 MOD-FLTEXT        PIC X.                                       
007300*                                 FINNS TEXTINFORMATION ?                 
007400     03 MOD-TEMFSINF         PIC X(55).                                   
007500*                                 INFORMATIONSMEDDELANDE                  
007600*** END OF VILMAII-COPY LENGTH= 1008 BYTES                                
