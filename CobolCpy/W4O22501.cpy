000100 01  MOD-W4O22501.                                                        
000200     03 MOD-IDTRANS          PIC X(4).                                    
000300*                                 SCREEN NUMBER                           
000400     03 MOD-TEMFSFEL         PIC X(40).                                   
000500*                                 MFS ERROR MESSAGE                       
000600     03 MOD-IDROLL-IN        PIC X(5).                                    
000700*                                 VOR ROLE ID                             
000800     03 MOD-IDDISTR-IN       PIC X(4).                                    
000900*                                 DISTRICT NUMBER                         
001000     03 MOD-IDKUNDNR-IN      PIC X(6).                                    
001100*                                 CUSTOMER NO                             
001200     03 MOD-IDANSK-IN        PIC X(3).                                    
001300*                                 PROCURER NO.                            
001400     03 MOD-IDARTNR-IN       PIC X(9).                                    
001500*                                 PART NUMBER                             
001600     03 MOD-KDSORT-IN        PIC X(2).                                    
001700*                                 FIELD FOR SORTING PURPOSE               
001800     03 MOD-TEVORMRK-IN      PIC X(2).                                    
001900     03 MOD-IDROLL-UT        PIC X(5).                                    
002000*                                 VOR ROLE ID                             
002100     03 MOD-IDDISTR-UT       PIC X(4).                                    
002200*                                 DISTRICT NUMBER                         
002300     03 MOD-IDKUNDNR-UT      PIC X(6).                                    
002400*                                 CUSTOMER NO                             
002500     03 MOD-IDANSK-UT        PIC X(3).                                    
002600*                                 PROCURER NO.                            
002700     03 MOD-IDARTNR-UT       PIC X(9).                                    
002800*                                 PART NUMBER                             
002900     03 MOD-KDSORT-UT        PIC X(2).                                    
003000*                                 FIELD FOR SORTING PURPOSE               
003100     03 MOD-TEVORMRK-UT      PIC X(2).                                    
003200     03 MOD-OUTPUT           OCCURS 13 TIMES.                             
003300        05 MOD-CMD-ATTR      PIC X(2).                                    
003400        05 MOD-CMD           PIC X.                                       
003500        05 MOD-TIREGDAT-ATTR PIC X(2).                                    
003600        05 MOD-TIREGDAT      PIC X(6).                                    
003700        05 MOD-TIREGDAT-KOD-ATTR                                          
003800                             PIC X(2).                                    
003900        05 MOD-TIREGDAT-KOD  PIC X.                                       
004000        05 MOD-TEVORMRK-ATTR PIC X(2).                                    
004100        05 MOD-TEVORMRK      PIC X(2).                                    
004200        05 MOD-IDDISTR-ATTR  PIC X(2).                                    
004300        05 MOD-IDDISTR       PIC Z(3)9.                                   
004400*                                 DISTRICT NUMBER                         
004500        05 MOD-IDKUNDNR-ATTR PIC X(2).                                    
004600        05 MOD-IDKUNDNR      PIC Z(5)9.                                   
004700*                                 CUSTOMER NO                             
004800        05 MOD-IDORDNR7-ATTR PIC X(2).                                    
004900        05 MOD-IDORDNR7      PIC Z(6)9.                                   
005000*                                 ORDER NUMBER                            
005100        05 MOD-IDANSK        PIC Z(2)9.                                   
005200*                                 PROCURER NO.                            
005300        05 MOD-IDARTNR       PIC Z(8)9.                                   
005400*                                 PART NUMBER                             
005500        05 MOD-KVBEART       PIC Z(5)9.                                   
005600*                                 ORDERED QUANTITY                        
005700        05 MOD-DIFF          PIC Z(5)9.                                   
005800*                                 ORDERED QUANTITY                        
005900        05 MOD-KDORDBEK      PIC 9(2).                                    
006000*                                 ORDERCONFIMATIONCODE                    
006100        05 MOD-IDDC          PIC X(2).                                    
006200*                                 WAREHOUSE IDENTIFIER                    
006300        05 MOD-TEVORTXT-FL   PIC X.                                       
006400        05 MOD-TEVORSC-FL-ATTR                                            
006500                             PIC X(2).                                    
006600        05 MOD-TEVORSC-FL    PIC X.                                       
006700        05 MOD-TEVORNOT-FL   PIC X(3).                                    
006800        05 MOD-TELOSNOT-FL   PIC X.                                       
006900     03 MOD-TEMFSINF         PIC X(55).                                   
007000*                                 INFORMATION MESSAGE                     
007100*** END OF VILMAII-COPY LENGTH= 1162 BYTES                                
