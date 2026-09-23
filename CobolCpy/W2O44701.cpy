000100 01  MOD-W2O44701.                                                        
000200     03 MOD-IDTRANS          PIC X(4).                                    
000300*                                 SCREEN NUMBER                           
000400     03 MOD-TEMFSFEL         PIC X(40).                                   
000500*                                 MFS ERROR MESSAGE                       
000600     03 MOD-IDANSK-FOM-IN-ATTR                                            
000700                             PIC X(2).                                    
000800     03 MOD-IDANSK-FOM-IN    PIC X(2).                                    
000900*                                 MFS DISPOSITION OF INPUT FIELD          
001000     03 MOD-IDANSK-FOM-UT    PIC Z(2)9.                                   
001100*                                 LOWEST PURCHASE PLANNER NUMBER          
001200     03 MOD-IDANSK-TOM-IN-ATTR                                            
001300                             PIC X(2).                                    
001400     03 MOD-IDANSK-TOM-IN    PIC X(2).                                    
001500*                                 MFS DISPOSITION OF INPUT FIELD          
001600     03 MOD-IDANSK-TOM-UT    PIC Z(2)9.                                   
001700*                                 HIGHEST PURCHASE PLANNER NO             
001800     03 MOD-IDLEVNR-IN-ATTR  PIC X(2).                                    
001900     03 MOD-IDLEVNR-IN       PIC X(2).                                    
002000*                                 MFS DISPOSITION OF INPUT FIELD          
002100     03 MOD-IDLEVNR-UT       PIC X(5).                                    
002200*                                 SUPPLIER NUMBER (VENDOR NUMBER)         
002300     03 MOD-KDLPORS-IN-ATTR  PIC X(2).                                    
002400     03 MOD-KDLPORS-IN       PIC X(2).                                    
002500*                                 MFS DISPOSITION OF INPUT FIELD          
002600     03 MOD-KDLPORS-UT       PIC Z9.                                      
002700     03 MOD-KDLEVPLF-IN-ATTR PIC X(2).                                    
002800     03 MOD-KDLEVPLF-IN      PIC X(2).                                    
002900*                                 MFS DISPOSITION OF INPUT FIELD          
003000     03 MOD-KDLEVPLF-UT      PIC X.                                       
003100*                                 CODE FOR APPROVAL OF SCHEDULE P         
003200*                                 ROPOSAL                                 
003300     03 MOD-IDDC-IN-ATTR     PIC X(2).                                    
003400     03 MOD-IDDC-IN          PIC X(2).                                    
003500*                                 MFS DISPOSITION OF INPUT FIELD          
003600     03 MOD-IDDC-UT          PIC X(2).                                    
003700*                                 WAREHOUSE IDENTIFIER                    
003800     03 MOD-IDPAGE           PIC X(3).                                    
003900     03 MOD-KVRADER          PIC Z(4)9.                                   
004000*                                 NUMBER OF LINES                         
004100     03 MOD-RAD              OCCURS 15 TIMES.                             
004200        05 MOD-KDCMDVAL-ATTR PIC X(2).                                    
004300        05 MOD-KDCMDVAL      PIC X.                                       
004400*                                 GENERAL COMMAND-CODE                    
004500        05 MOD-IDDC          PIC X(2).                                    
004600*                                 WAREHOUSE IDENTIFIER                    
004700        05 MOD-IDARTNR       PIC Z(8)9.                                   
004800*                                 PART NUMBER                             
004900        05 MOD-BEART         PIC X(22).                                   
005000        05 MOD-IDLEVNR       PIC X(5).                                    
005100*                                 SUPPLIER NUMBER (VENDOR NUMBER)         
005200        05 MOD-KDLPORS-TEXT  OCCURS 3 TIMES                               
005300                             PIC X(8).                                    
005400        05 MOD-KDLEVPLF      PIC X.                                       
005500*                                 CODE FOR APPROVAL OF SCHEDULE P         
005600*                                 ROPOSAL                                 
005700        05 MOD-TIOMSPEC      PIC 9(4).                                    
005800     03 MOD-TEMFSINF         PIC X(55).                                   
005900*                                 INFORMATION MESSAGE                     
006000*** END OF VILMAII-COPY LENGTH= 1197 BYTES                                
