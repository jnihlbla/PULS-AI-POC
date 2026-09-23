000100 01  W11695.                                                              
000200*                                 BRUTTOPRIS PER SÄLJBOLAG.               
000300*                                 PRISERNA KOMMER FRÅN PRICE SC.          
000400     03 IDARTNR              PIC S9(9)           COMP-3.                  
000500*                                 ARTIKELNUMMER                           
000600     03 IDLANDX2             PIC X(2).                                    
000700      88 AUSTRALIA           VALUE 'AU'.                                  
000800      88 AUSTRIA             VALUE 'AT'.                                  
000900      88 BELGIUM             VALUE 'BE'.                                  
001000      88 BRASIL              VALUE 'BR'.                                  
001100      88 CANADA              VALUE 'CA'.                                  
001200      88 SWIZERLAND          VALUE 'CH'.                                  
001300      88 CHINA               VALUE 'CN'.                                  
001400      88 GERMANY             VALUE 'DE'.                                  
001500      88 SPAIN               VALUE 'ES'.                                  
001600      88 FINLAND             VALUE 'FI'.                                  
001700      88 FRANCE              VALUE 'FR'.                                  
001800      88 ENGLAND             VALUE 'GB'.                                  
001900      88 HUNGARY             VALUE 'HU'.                                  
002000      88 INDIA               VALUE 'IN'.                                  
002100      88 ITALY               VALUE 'IT'.                                  
002200      88 JAPAN               VALUE 'JP'.                                  
002300      88 KOREA               VALUE 'KR'.                                  
002400      88 MALAYSIA            VALUE 'MY'.                                  
002500      88 MAROCKO             VALUE 'MA'.                                  
002600      88 MEXICO              VALUE 'MX'.                                  
002700      88 HOLLAND             VALUE 'NL'.                                  
002800      88 NORWAY              VALUE 'NO'.                                  
002900      88 POLAND              VALUE 'PL'.                                  
003000      88 RUSSIA              VALUE 'RU'.                                  
003100      88 SWEDEN              VALUE 'SE'.                                  
003200      88 THAILAND            VALUE 'TH'.                                  
003300      88 TURKEY              VALUE 'TR'.                                  
003400      88 TAIWAN              VALUE 'TW'.                                  
003500      88 USA                 VALUE 'US'.                                  
003600      88 SOUTH-AFRICA        VALUE 'ZA'.                                  
003700      88 NDC-LAND            VALUE 'AU'                                   
003800                             'CA'                                         
003900                             'CN'                                         
004000                             'JP'                                         
004100                             'US'.                                        
004200*                                 2-STÄLLIG LANDSBETECKNINGSKOD           
004300     03 KDVALISO             PIC X(3).                                    
004400*                                 VALUTAKOD ENLIGT ISO-STANDARD.          
004500     03 PRARTBTO-SC          PIC S9(9)V9(2)      COMP-3.                  
004600*                                 BRUTTOPRIS PER SÄLJBOLAG                
004700*** END OF VILMAII-COPY LENGTH= 16 BYTES                                  
