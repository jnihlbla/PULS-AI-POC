000100 01  MOD-W6O15801.                                                        
000200*                                 COPYTEXT FOR MOD W6O15801               
000300     03 MOD-IDTRANS          PIC X(4).                                    
000400*                                 SCREEN NUMBER                           
000500     03 MOD-TEMFSFEL         PIC X(40).                                   
000600*                                 MFS ERROR MESSAGE                       
000700     03 MOD-BEFT-IN          PIC Z9.                                      
000800*                                 PACKAGING TYPE                          
000900     03 MOD-ADLAGOMR-IN REDEFINES MOD-BEFT-IN                             
001000                             PIC Z9.                                      
001100*                                 AREA                                    
001200     03 MOD-KDSORT1-IN       PIC X.                                       
001300*                                 CODE FOR SORTING                        
001400     03 MOD-BEFT-UT          PIC X(2).                                    
001500*                                 PACKAGING TYPE                          
001600     03 MOD-ADLAGOMR-UT REDEFINES MOD-BEFT-UT                             
001700                             PIC Z9.                                      
001800*                                 AREA                                    
001900     03 MOD-KDSORT1-UT       PIC X.                                       
002000*                                 CODE FOR SORTING                        
002100     03 MOD-IDDC             PIC X(2).                                    
002200*                                 WAREHOUSE IDENTIFIER                    
002300     03 MOD-ADINPORT-CDC     PIC X(8).                                    
002400*                                 LOADING GATE                            
002500     03 MOD-OUTPUT           OCCURS 12 TIMES.                             
002600*                                 OCCURS CLAUSE FOR W6O15801 COPY         
002700*                                 TEXT                                    
002800        05 MOD-KDCMDVAL-ATTR PIC X(2).                                    
002900        05 MOD-KDCMDVAL      PIC X.                                       
003000*                                 GENERAL COMMAND-CODE                    
003100        05 MOD-ADLAGOMR-ATTR PIC X(2).                                    
003200        05 MOD-ADLAGOMR      PIC Z9.                                      
003300*                                 AREA                                    
003400        05 MOD-BEFT-ATTR     PIC X(2).                                    
003500        05 MOD-BEFT          PIC Z9.                                      
003600*                                 PACKAGING TYPE                          
003700        05 MOD-IDARTNR-ATTR  PIC X(2).                                    
003800        05 MOD-IDARTNR       PIC Z(9).                                    
003900*                                 PART NUMBER                             
004000        05 MOD-ADINPORT-ATTR PIC X(2).                                    
004100        05 MOD-ADINPORT      PIC X(8).                                    
004200*                                 LOADING GATE                            
004300        05 MOD-TIAAVV-FOM-ATTR                                            
004400                             PIC X(2).                                    
004500        05 MOD-TIAAVV-FOM    PIC 9(4).                                    
004600*                                 YEAR - WEEK  (YYWW)                     
004700        05 MOD-IDUSER-ATTR   PIC X(2).                                    
004800        05 MOD-IDUSER        PIC X(8).                                    
004900*                                 USER SECURITY-IDENTITY                  
005000        05 MOD-TIUPPDAT-ATTR PIC X(2).                                    
005100        05 MOD-TIUPPDAT      PIC 9(6).                                    
005200*                                 UPDATING DATE     (YYMMDD)              
005300     03 MOD-ADLAGOMR-UP-ATTR PIC X(2).                                    
005400     03 MOD-ADLAGOMR-UP      PIC Z9.                                      
005500*                                 AREA                                    
005600     03 MOD-BEFT-UP-ATTR     PIC X(2).                                    
005700     03 MOD-BEFT-UP          PIC 9(2).                                    
005800*                                 PACKAGING TYPE                          
005900     03 MOD-IDARTNR-UP-ATTR  PIC X(2).                                    
006000     03 MOD-IDARTNR-UP       PIC Z(9).                                    
006100*                                 PART NUMBER                             
006200     03 MOD-ADINPORT-UP-ATTR PIC X(2).                                    
006300     03 MOD-ADINPORT-UP      PIC X(8).                                    
006400*                                 LOADING GATE                            
006500     03 MOD-TIAAVV-FOM-UP-ATTR                                            
006600                             PIC X(2).                                    
006700     03 MOD-TIAAVV-FOM-UP    PIC X(4).                                    
006800*                                 YEAR - WEEK  (YYWW)                     
006900     03 MOD-TEMFSINF         PIC X(55).                                   
007000*                                 INFORMATION MESSAGE                     
007100*** END OF VILMAII-COPY LENGTH= 822 BYTES                                 
