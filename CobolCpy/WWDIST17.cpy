000100*** EDIT ALLOWED                                                          
000200******************************************************************        
000300* DISTRICTS FOR WEIGHT AND VOLUME CONTROL FOR CDC VOR ORDERS.             
000400* FC IS DEFAULT IF PART WEIGHT > 20KG OR VOL > 0.1M3                      
000500* OR VSOP > 220.IF WEIGHT < 20KG & VOL < 0.1 M3 & VSOP < 220,THEN         
000600* FC SHOULD BE 17                                                         
000700******************************************************************        
000800 01  DIST17-IDDISTR          PIC S9(5)     COMP-3.                        
000900**                                                                        
001000     88  DIST17-CDC-VOR-FC         VALUE  1090 1030                       
001100                                          1378 1338                       
001200                                          1558 1538                       
001300                                          1778 1738                       
001400                                          1822 1832                       
001500                                          1958 1938                       
001600                                          2178 2138                       
001700                                          2364 2334                       
001800                                          2365 2335                       
001900                                          2378 2338.                      
002000**                                                                        
002100                                                                          
002200     88  DIST17-CDC-VOR-FC-FI      VALUE  1090 1030.                      
002300     88  DIST17-CDC-VOR-FC-GB      VALUE  1378 1338.                      
002400     88  DIST17-CDC-VOR-FC-GR      VALUE  1558 1538.                      
002500     88  DIST17-CDC-VOR-FC-IE      VALUE  1778 1738.                      
002600     88  DIST17-CDC-VOR-FC-IT      VALUE  1822 1832.                      
002700     88  DIST17-CDC-VOR-FC-PT      VALUE  1958 1938.                      
002800     88  DIST17-CDC-VOR-FC-ES      VALUE  2178 2138.                      
002900     88  DIST17-CDC-VOR-FC-HU      VALUE  2364 2334.                      
003000     88  DIST17-CDC-VOR-FC-CZ      VALUE  2365 2335.                      
003100     88  DIST17-CDC-VOR-FC-AT      VALUE  2378 2338.                      
003200*                                                                         
003300*** END COPY WWDIST17    LENGTH=3                                         
