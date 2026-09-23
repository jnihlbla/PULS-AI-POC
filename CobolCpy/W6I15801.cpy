000100 01  MID-W6I15801.                                                        
000200*                                 COPYTEXT FOR MID W6I15801               
000300     03 MID-BEFT-IN          PIC 9(2).                                    
000400*                                 PACKAGING TYPE                          
000500     03 MID-ADLAGOMR-IN REDEFINES MID-BEFT-IN                             
000600                             PIC 9(2).                                    
000700*                                 AREA                                    
000800     03 MID-KDSORT1-IN       PIC X.                                       
000900*                                 CODE FOR SORTING                        
001000     03 MID-INPUT            OCCURS 12 TIMES.                             
001100*                                 OCCURS CLAUSE FOR W6I15801 COPY         
001200*                                 TEXT                                    
001300        05 MID-KDCMDVAL      PIC X.                                       
001400*                                 GENERAL COMMAND-CODE                    
001500        05 MID-ADLAGOMR      PIC 9(2).                                    
001600*                                 AREA                                    
001700        05 MID-BEFT          PIC 9(2).                                    
001800*                                 PACKAGING TYPE                          
001900        05 MID-IDARTNR       PIC 9(9).                                    
002000*                                 PART NUMBER                             
002100        05 MID-ADINPORT      PIC X(4).                                    
002200*                                 LOADING GATE                            
002300        05 MID-TIAAVV-FOM    PIC X(4).                                    
002400*                                 YEAR - WEEK  (YYWW)                     
002500        05 MID-IDUSER        PIC X(8).                                    
002600*                                 USER SECURITY-IDENTITY                  
002700        05 MID-TIUPPDAT      PIC 9(6).                                    
002800*                                 UPDATING DATE     (YYMMDD)              
002900     03 MID-ADLAGOMR-UP      PIC 9(2).                                    
003000*                                 AREA                                    
003100     03 MID-BEFT-UP          PIC 9(2).                                    
003200*                                 PACKAGING TYPE                          
003300     03 MID-IDARTNR-UP       PIC 9(9).                                    
003400*                                 PART NUMBER                             
003500     03 MID-ADINPORT-UP      PIC X(4).                                    
003600*                                 LOADING GATE                            
003700     03 MID-TIAAVV-FOM-UP    PIC X(4).                                    
003800*                                 YEAR - WEEK  (YYWW)                     
003900*** END OF VILMAII-COPY LENGTH= 456 BYTES                                 
