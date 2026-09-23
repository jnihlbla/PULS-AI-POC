000100 01  MID-W3I15101.                                                        
000200*                                 MID-COPYTEXT FÖR W3015100               
000300     03 MID-IDARTNR-IN       PIC X(9).                                    
000400*                                 ARTIKELNUMMER                           
000500     03 MID-IDARTNR-UT       PIC X(9).                                    
000600*                                 ARTIKELNUMMER                           
000700     03 MID-IDPRODNR-LO      PIC 9(9).                                    
000800*                                 ARTIKELNUMMER                           
000900     03 MID-IDPRODNR-HI      PIC 9(9).                                    
001000*                                 ARTIKELNUMMER                           
001100     03 MID-BELEV-LO         PIC X(30).                                   
001200*                                 LEVERANTÖRENS ARTIKELBENÄMNING          
001300     03 MID-IDDISTR-RENOV    PIC 9(5).                                    
001400*                                 DISTRIKTNUMMER                          
001500     03 MID-IDDISTR-NDC      PIC 9(5).                                    
001600*                                 DISTRIKTNUMMER                          
001700     03 MID-IDDISTR-CAN      PIC 9(5).                                    
001800*                                 DISTRIKTNUMMER                          
001900     03 MID-IDDISTR-PAC      PIC 9(5).                                    
002000*                                 DISTRIKTNUMMER                          
002100     03 MID-IDDISTR-AUS      PIC 9(5).                                    
002200*                                 DISTRIKTNUMMER                          
002300     03 MID-IDDISTR-CHN      PIC 9(5).                                    
002400*                                 DISTRIKTNUMMER                          
002500     03 MID-IDDISTR-KOR      PIC 9(5).                                    
002600*                                 DISTRIKTNUMMER                          
002700     03 MID-IDDISTR-MY       PIC 9(5).                                    
002800*                                 DISTRIKTNUMMER                          
002900     03 MID-IDDISTR-TW       PIC 9(5).                                    
003000*                                 DISTRIKTNUMMER                          
003100     03 MID-IDDISTR-TH       PIC 9(5).                                    
003200*                                 DISTRIKTNUMMER                          
003300     03 MID-TEBYTNOT         OCCURS 4 TIMES                               
003400                             PIC X(30).                                   
003500*                                 BYTES ARTIKEL NOTERING                  
003600     03 MID-BETFLEV          PIC X(30).                                   
003700*                                 TILLFÄLLIG LEVERANTÖR                   
003800     03 MID-KVLS-MAXCORE-IN  PIC 9(7).                                    
003900*                                 LAGERSALDO MAXCORE                      
004000     03 MID-UPPDATERINGSSORT PIC X.                                       
004100     03 MID-IDPRODNR-IN      PIC 9(9).                                    
004200*                                 ARTIKELNUMMER                           
004300     03 MID-IDDISTR-RENOV-IN PIC 9(5).                                    
004400*                                 DISTRIKTNUMMER                          
004500     03 MID-IDDISTR-NDC-IN   PIC 9(5).                                    
004600*                                 DISTRIKTNUMMER                          
004700     03 MID-IDDISTR-CAN-IN   PIC 9(5).                                    
004800*                                 DISTRIKTNUMMER                          
004900     03 MID-IDDISTR-PAC-IN   PIC 9(5).                                    
005000*                                 DISTRIKTNUMMER                          
005100     03 MID-IDDISTR-AUS-IN   PIC 9(5).                                    
005200*                                 DISTRIKTNUMMER                          
005300     03 MID-IDDISTR-CHN-IN   PIC 9(5).                                    
005400*                                 DISTRIKTNUMMER                          
005500     03 MID-IDDISTR-KOR-IN   PIC 9(5).                                    
005600*                                 DISTRIKTNUMMER                          
005700     03 MID-IDDISTR-MY-IN    PIC 9(5).                                    
005800*                                 DISTRIKTNUMMER                          
005900     03 MID-IDDISTR-TW-IN    PIC 9(5).                                    
006000*                                 DISTRIKTNUMMER                          
006100     03 MID-IDDISTR-TH-IN    PIC 9(5).                                    
006200*                                 DISTRIKTNUMMER                          
006300     03 MID-BELEV-HI         PIC X(30).                                   
006400*                                 LEVERANTÖRENS ARTIKELBENÄMNING          
006500     03 MID-BELEV            PIC X(30).                                   
006600*                                 LEVERANTÖRENS ARTIKELBENÄMNING          
006700*** END OF VILMAII-COPY LENGTH= 393 BYTES                                 
