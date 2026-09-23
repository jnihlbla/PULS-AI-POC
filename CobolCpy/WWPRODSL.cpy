000100*** EDIT ALLOWED                                                          
000200 01  TEST-KDPRODSL                    PIC S9(3) COMP-3.                   
000300                                                                          
000400     88  GOOD-KDPRODSL                VALUE 11 13 THRU 19                 
000500                                            21 24 THRU 27 29              
000600                                            31 33 THRU 39                 
000700                                            51 53 THRU 59                 
000800                                            71 THRU 74                    
000900                                            91 93 THRU 99.                
001000                                                                          
001100     88  KDPRODSL-VCC                 VALUE 11 13 THRU 19.                
001200     88  KDPRODSL-VCBV                VALUE 21 25 THRU 27 29.             
001300     88  KDPRODSL-LYNK                VALUE 31 33 THRU 39.                
001400     88  KDPRODSL-POLESTAR            VALUE 51 53 THRU 59.                
001500     88  KDPRODSL-BIMA                VALUE 71 THRU 74.                   
001600     88  KDPRODSL-LOCAL               VALUE 91 93 THRU 99.                
001700                                                                          
001800     88  KDPRODSL-SPARE-PARTS         VALUE 11 31 51.                     
001900     88  KDPRODSL-CHEMICAL            VALUE 13 33 53.                     
002000     88  KDPRODSL-BYTES               VALUE 14 24 34 54.                  
002100     88  KDPRODSL-ACC                 VALUE 15 35 55.                     
002200     88  KDPRODSL-WHEELS              VALUE 16 36 56.                     
002300     88  KDPRODSL-SERVICES            VALUE 17 37 57.                     
002400     88  KDPRODSL-TOOLS               VALUE 18 38 58.                     
002500     88  KDPRODSL-EMB                 VALUE 19 39 59.                     
002600     88  KDPRODSL-PARTS-CHEM          VALUE 11 13 31 33 51 53.            
002700     88  KDPRODSL-PARTS-BYTES         VALUE 11 14 24 31 34 51 54.         
002800     88  KDPRODSL-PARTS-ACC           VALUE 11 15 31 35 51 55.            
002900     88  KDPRODSL-X5-X7               VALUE 15 THRU 17                    
003000                                            35 THRU 37                    
003000                                            55 THRU 57.                   
003100     88  KDPRODSL-UTAN-EMB            VALUE 11 13 THRU 18                 
                                                  24                            
003200                                            31 33 THRU 38                 
003300                                            51 53 THRU 58.                
003400                                                                          
003500     88  KDPRODSL-BRANDON             VALUE 25.                           
003600                                                                          
003700     88  KDPRODSL-VCBV-PARTS          VALUE 21.                           
003800     88  KDPRODSL-VCBV-BYTES          VALUE 24.                           
003900     88  KDPRODSL-VCBV-WHEELS         VALUE 26.                           
004000     88  KDPRODSL-VCBV-SERVICES       VALUE 27.                           
004100     88  KDPRODSL-VCBV-TOOLS          VALUE 28.                           
004200     88  KDPRODSL-VCBV-EMB            VALUE 29.                           
004300                                                                          
004400     88  KDPRODSL-LOCAL-PARTS         VALUE 91.                           
004500     88  KDPRODSL-LOCAL-CHEM          VALUE 93.                           
004600     88  KDPRODSL-LOCAL-BYTES         VALUE 94.                           
004700     88  KDPRODSL-LOCAL-ACC           VALUE 95.                           
004800     88  KDPRODSL-LOCAL-WHEELS        VALUE 96.                           
004900     88  KDPRODSL-LOCAL-SERVICES      VALUE 97.                           
005000     88  KDPRODSL-LOCAL-TOOLS         VALUE 98.                           
005100     88  KDPRODSL-LOCAL-EMB           VALUE 99.                           
005200*                                                                         
005300     88  KDPRODSL-VOLVO-ALL           VALUE 11 13 THRU 19                 
005400                                            21 24 THRU 27 29              
005500                                            31 33 THRU 39                 
005600                                            51 53 THRU 59.                
005700     88  KDPRODSL-VOLVO-UTAN-EMB      VALUE 11 13 THRU 18                 
005800                                            21 24 THRU 27                 
005900                                            31 33 THRU 38                 
006000                                            51 53 THRU 58.                
006100     88  KDPRODSL-VOLVO-PARTS         VALUE 11 21 31 51.                  
006200     88  KDPRODSL-VOLVO-BYTES         VALUE 14 24 34 54.                  
006300     88  KDPRODSL-VOLVO-WHEELS        VALUE 16 26 36 56.                  
006400     88  KDPRODSL-VOLVO-SERVICES      VALUE 17 27 37 57.                  
006500     88  KDPRODSL-VOLVO-EMB           VALUE 19 29 39 59.                  
006600     88  KDPRODSL-VOLVO-BIMA          VALUE 11 13 THRU 19                 
006700                                            21 24 THRU 27 29              
006800                                            31 33 THRU 39                 
006900                                            51 53 THRU 59                 
007000                                            71 THRU 74.                   
007100                                                                          
007200     88  KDPRODSL-BIMA-LOCAL          VALUE 71 THRU 74                    
007300                                            91 93 THRU 99.                
007400                                                                          
007500 01  CONST-KDPRODSL.                                                      
007600     03  WC-KDPRODSL-VCC-PARTS        PIC S9(3) COMP-3 VALUE 11.          
007700     03  FILLER                       PIC X(52).                          
007800*** END COPY WWPRODSL  LENGTH=56                                          
