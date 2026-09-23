000100 01  MOD-W2O14701.                                                        
000200     03 MOD-IDTRANS          PIC X(4).                                    
000300*                                 SCREEN NUMBER                           
000400     03 MOD-TEMFSFEL         PIC X(40).                                   
000500*                                 MFS ERROR MESSAGE                       
000600     03 MOD-IDANSK-IN-ATTR   PIC X(2).                                    
000700     03 MOD-IDANSK-IN        PIC X(2).                                    
000800*                                 MFS DISPOSITION OF INPUT FIELD          
000900     03 MOD-IDANSK-UT        PIC Z(2)9.                                   
001000*                                 PROCURER NO.                            
001100     03 MOD-IDANSK-TO-IN-ATTR                                             
001200                             PIC X(2).                                    
001300     03 MOD-IDANSK-TO-IN     PIC X(2).                                    
001400*                                 MFS DISPOSITION OF INPUT FIELD          
001500     03 MOD-IDANSK-TO-UT     PIC Z(2)9.                                   
001600*                                 PROCURER NO.                            
001700     03 MOD-IDLEVNR-IN-ATTR  PIC X(2).                                    
001800     03 MOD-IDLEVNR-IN       PIC X(2).                                    
001900*                                 MFS DISPOSITION OF INPUT FIELD          
002000     03 MOD-IDLEVNR-UT       PIC X(5).                                    
002100*                                 SUPPLIER NUMBER (VENDOR NUMBER)         
002200     03 MOD-KDLPORS-IN-ATTR  PIC X(2).                                    
002300     03 MOD-KDLPORS-IN       PIC X(2).                                    
002400*                                 MFS DISPOSITION OF INPUT FIELD          
002500     03 MOD-KDLPORS-UT       PIC Z9.                                      
002600     03 MOD-KDLEVPLF-IN-ATTR PIC X(2).                                    
002700     03 MOD-KDLEVPLF-IN      PIC X(2).                                    
002800*                                 MFS DISPOSITION OF INPUT FIELD          
002900     03 MOD-KDLEVPLF-UT      PIC X.                                       
003000*                                 CODE FOR APPROVAL OF SCHEDULE P         
003100*                                 ROPOSAL                                 
003200     03 MOD-IDPAGE           PIC X(3).                                    
003300     03 MOD-KVRADER          PIC Z(4)9.                                   
003400*                                 NUMBER OF LINES                         
003500     03 MOD-RAD              OCCURS 15 TIMES.                             
003600        05 MOD-KDCMDVAL-ATTR PIC X(2).                                    
003700        05 MOD-KDCMDVAL      PIC X.                                       
003800*                                 GENERAL COMMAND-CODE                    
003900        05 MOD-IDARTNR       PIC Z(7)9.                                   
004000*                                 PART NUMBER                             
004100        05 MOD-BEART         PIC X(25).                                   
004200*                                 PART DESCRIPTION                        
004300        05 MOD-IDLEVNR       PIC X(5).                                    
004400*                                 SUPPLIER NUMBER (VENDOR NUMBER)         
004500        05 MOD-KDLPORS-TEXT  OCCURS 3 TIMES                               
004600                             PIC X(8).                                    
004700        05 MOD-KDLEVPLF      PIC X.                                       
004800*                                 CODE FOR APPROVAL OF SCHEDULE P         
004900*                                 ROPOSAL                                 
005000        05 MOD-TIOMSPEC      PIC 9(4).                                    
005100     03 MOD-TEMFSINF         PIC X(55).                                   
005200*                                 INFORMATION MESSAGE                     
005300*** END OF VILMAII-COPY LENGTH= 1191 BYTES                                
