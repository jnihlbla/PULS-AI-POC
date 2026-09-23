000100 01  W4832701.                                                            
000200*                                 WDB501 DATA TO DATA                     
000300*                                 LAKE                                    
000400     03 IDDC                 PIC X(2).                                    
000500*                                 IDENTIFIERARE LAGER                     
000600*                                 WAREHOUSE IDENTIFIER                    
000700     03 TAB-1                PIC X.                                       
000800*                                 TAB-TECKEN                              
000900*                                 TAB-CHARACTER                           
001000     03 KDFRAKT              PIC Z9.                                      
001100*                                 FRAKTSÄTT DC TILL KUND                  
001200*                                 FREIGHT CODE                            
001300     03 TAB-2                PIC X.                                       
001400*                                 TAB-TECKEN                              
001500*                                 TAB-CHARACTER                           
001600     03 IDDISTR              PIC Z(3)9.                                   
001700*                                 DISTRIKTNUMMER                          
001800*                                 DISTRICT NUMBER                         
001900     03 TAB-3                PIC X.                                       
002000*                                 TAB-TECKEN                              
002100*                                 TAB-CHARACTER                           
002200     03 IDKUNDNR             PIC Z(5)9.                                   
002300*                                 KUNDNUMMER                              
002400*                                 CUSTOMER NO                             
002500     03 TAB-4                PIC X.                                       
002600*                                 TAB-TECKEN                              
002700*                                 TAB-CHARACTER                           
002800     03 BEGMRK-RAD1          PIC X(30).                                   
002900*                                 GODSMÄRKE  RAD1                         
003000*                                 GOODS MARKING  LINE1                    
003100     03 TAB-5                PIC X.                                       
003200*                                 TAB-TECKEN                              
003300*                                 TAB-CHARACTER                           
003400     03 BEGMRK-RAD2          PIC X(30).                                   
003500*                                 GODSMÄRKE  RAD2                         
003600*                                 GOODS MARKING  LINE2                    
003700     03 TAB-6                PIC X.                                       
003800*                                 TAB-TECKEN                              
003900*                                 TAB-CHARACTER                           
004000     03 IDTRPLOS-0           PIC X(3).                                    
004100*                                 TRANSPORTLÖSNING                        
004200*                                 TRANSPORTSOLUTION                       
004300     03 TAB-7                PIC X.                                       
004400*                                 TAB-TECKEN                              
004500*                                 TAB-CHARACTER                           
004600     03 IDTRPVAR-0           PIC X(2).                                    
004700*                                 TRANSPORTLÖSNINGSGRUPP                  
004800*                                 TRANSPORTSOLUTIONGROUP                  
004900     03 TAB-8                PIC X.                                       
005000*                                 TAB-TECKEN                              
005100*                                 TAB-CHARACTER                           
005200     03 IDTRPLOS-1           PIC X(3).                                    
005300*                                 TRANSPORTLÖSNING                        
005400*                                 TRANSPORTSOLUTION                       
005500     03 TAB-9                PIC X.                                       
005600*                                 TAB-TECKEN                              
005700*                                 TAB-CHARACTER                           
005800     03 IDTRPVAR-1           PIC X(2).                                    
005900*                                 TRANSPORTLÖSNINGSGRUPP                  
006000*                                 TRANSPORTSOLUTIONGROUP                  
006100     03 TAB-10               PIC X.                                       
006200*                                 TAB-TECKEN                              
006300*                                 TAB-CHARACTER                           
006400     03 IDTRPLOS-2           PIC X(3).                                    
006500*                                 TRANSPORTLÖSNING                        
006600*                                 TRANSPORTSOLUTION                       
006700     03 TAB-11               PIC X.                                       
006800*                                 TAB-TECKEN                              
006900*                                 TAB-CHARACTER                           
007000     03 IDTRPVAR-2           PIC X(2).                                    
007100*                                 TRANSPORTLÖSNINGSGRUPP                  
007200*                                 TRANSPORTSOLUTIONGROUP                  
007300     03 TAB-12               PIC X.                                       
007400*                                 TAB-TECKEN                              
007500*                                 TAB-CHARACTER                           
007600     03 IDTRPLOS-3           PIC X(3).                                    
007700*                                 TRANSPORTLÖSNING                        
007800*                                 TRANSPORTSOLUTION                       
007900     03 TAB-13               PIC X.                                       
008000*                                 TAB-TECKEN                              
008100*                                 TAB-CHARACTER                           
008200     03 IDTRPVAR-3           PIC X(2).                                    
008300*                                 TRANSPORTLÖSNINGSGRUPP                  
008400*                                 TRANSPORTSOLUTIONGROUP                  
008500     03 TAB-14               PIC X.                                       
008600*                                 TAB-TECKEN                              
008700*                                 TAB-CHARACTER                           
008800     03 IDTRPLOS-4           PIC X(3).                                    
008900*                                 TRANSPORTLÖSNING                        
009000*                                 TRANSPORTSOLUTION                       
009100     03 TAB-15               PIC X.                                       
009200*                                 TAB-TECKEN                              
009300*                                 TAB-CHARACTER                           
009400     03 IDTRPVAR-4           PIC X(2).                                    
009500*                                 TRANSPORTLÖSNINGSGRUPP                  
009600*                                 TRANSPORTSOLUTIONGROUP                  
009700     03 TAB-16               PIC X.                                       
009800*                                 TAB-TECKEN                              
009900*                                 TAB-CHARACTER                           
010000     03 KDFDKRAV             PIC Z(2)9.                                   
010100*                                 TRANSPORTFÖRPACKNINGSKOD                
010200*                                 PACKING CODE                            
010300     03 TAB-17               PIC X.                                       
010400*                                 TAB-TECKEN                              
010500*                                 TAB-CHARACTER                           
010600     03 KDFKTYP              PIC X.                                       
010700*                                 TYP AV AVVIKELSE I FK-SEGMENTET         
010800*                                 FREIGHT CODE SEGMENT TYPE               
010900     03 TAB-18               PIC X.                                       
011000*                                 TAB-TECKEN                              
011100*                                 TAB-CHARACTER                           
011200     03 KDGRANS              PIC Z(2)9.                                   
011300*                                 GRÄNSKOD                                
011400*                                 BORDER CODE                             
011500     03 TAB-19               PIC X.                                       
011600*                                 TAB-TECKEN                              
011700*                                 TAB-CHARACTER                           
011800     03 KDTRPKAT             PIC X.                                       
011900*                                 TRANSPORTKATEGORI                       
012000*                                 TRANSPORT CATEGORY                      
012100     03 TAB-20               PIC X.                                       
012200*                                 TAB-TECKEN                              
012300*                                 TAB-CHARACTER                           
012400     03 PRLEGKST             PIC Z(6)9.9(2).                              
012500*                                 LEGALISERINSKOSTNAD                     
012600*                                 LEGALIZATION FEE                        
012700     03 TAB-21               PIC X.                                       
012800*                                 TAB-TECKEN                              
012900*                                 TAB-CHARACTER                           
013000     03 REFOERS              PIC Z9.9(3).                                 
013100*                                 FÖRSÄKRINGKOSTNADSFAKTOR                
013200*                                 INSURANCE COSTS                         
013300*** END OF VILMAII-COPY LENGTH= 144 BYTES                                 
