000100 01  FK-WDB501.                                                           
000200*                                 KUNDREGISTER                            
000300*                                 FRAKT INFO                              
000400*                                 FYSISK NYCKEL: WDB501KY                 
000500*                                 (IDDC + KDFRAKT + IDDISTR               
000600*                                 +IDKUNDNR )                             
000700     03 FK-IDDC              PIC X(2).                                    
000800*                                 IDENTIFIERARE LAGER                     
000900*                                 WAREHOUSE IDENTIFIER                    
001000     03 FK-KDFRAKT           PIC S9(3)           COMP-3.                  
001100*                                 FRAKTSÄTT DC TILL KUND                  
001200*                                 FREIGHT CODE                            
001300     03 FK-IDGMT.                                                         
001400*                                 GODSMOTTAGARE                           
001500*                                 GOODS RECEIVER                          
001600        05 FK-IDDISTR        PIC S9(5)           COMP-3.                  
001700*                                 DISTRIKTNUMMER                          
001800*                                 DISTRICT NUMBER                         
001900        05 FK-IDKUNDNR       PIC S9(7)           COMP-3.                  
002000*                                 KUNDNUMMER                              
002100*                                 CUSTOMER NO                             
002200     03 FK-BEGMRK.                                                        
002300*                                 GODSMÄRKE                               
002400*                                 GOODS MARKING                           
002500        05 FK-BEGMRK-RAD1    PIC X(30).                                   
002600*                                 GODSMÄRKE  RAD1                         
002700*                                 GOODS MARKING  LINE1                    
002800        05 FK-BEGMRK-RAD2    PIC X(30).                                   
002900*                                 GODSMÄRKE  RAD2                         
003000*                                 GOODS MARKING  LINE2                    
003100     03 FK-IDTRP-0.                                                       
003200*                                 TRANSPORTIDENTITET KLASS 0              
003300*                                 TRANSPORT IDENTITET CLASS 0             
003400        05 FK-IDTRPLOS-0     PIC X(3).                                    
003500*                                 TRANSPORTLÖSNING                        
003600*                                 TRANSPORTSOLUTION                       
003700        05 FK-IDTRPVAR-0     PIC X(2).                                    
003800*                                 TRANSPORTLÖSNINGSGRUPP                  
003900*                                 TRANSPORTSOLUTIONGROUP                  
004000     03 FK-IDTRP-1.                                                       
004100*                                 TRANSPORTIDENTITET KLASS 1              
004200*                                 TRANSPORT IDENTITET CLASS 1             
004300        05 FK-IDTRPLOS-1     PIC X(3).                                    
004400*                                 TRANSPORTLÖSNING                        
004500*                                 TRANSPORTSOLUTION                       
004600        05 FK-IDTRPVAR-1     PIC X(2).                                    
004700*                                 TRANSPORTLÖSNINGSGRUPP                  
004800*                                 TRANSPORTSOLUTIONGROUP                  
004900     03 FK-IDTRP-2.                                                       
005000*                                 TRANSPORTIDENTITET KLASS 2              
005100*                                 TRANSPORT IDENTITET CLASS 2             
005200        05 FK-IDTRPLOS-2     PIC X(3).                                    
005300*                                 TRANSPORTLÖSNING                        
005400*                                 TRANSPORTSOLUTION                       
005500        05 FK-IDTRPVAR-2     PIC X(2).                                    
005600*                                 TRANSPORTLÖSNINGSGRUPP                  
005700*                                 TRANSPORTSOLUTIONGROUP                  
005800     03 FK-IDTRP-3.                                                       
005900*                                 TRANSPORTIDENTITET KLASS 3              
006000*                                 TRANSPORT IDENTITET CLASS 3             
006100        05 FK-IDTRPLOS-3     PIC X(3).                                    
006200*                                 TRANSPORTLÖSNING                        
006300*                                 TRANSPORTSOLUTION                       
006400        05 FK-IDTRPVAR-3     PIC X(2).                                    
006500*                                 TRANSPORTLÖSNINGSGRUPP                  
006600*                                 TRANSPORTSOLUTIONGROUP                  
006700     03 FK-IDTRP-4.                                                       
006800*                                 TRANSPORTIDENTITET KLASS 4              
006900*                                 TRANSPORT IDENTITET CLASS 4             
007000        05 FK-IDTRPLOS-4     PIC X(3).                                    
007100*                                 TRANSPORTLÖSNING                        
007200*                                 TRANSPORTSOLUTION                       
007300        05 FK-IDTRPVAR-4     PIC X(2).                                    
007400*                                 TRANSPORTLÖSNINGSGRUPP                  
007500*                                 TRANSPORTSOLUTIONGROUP                  
007600     03 FK-KDFDKRAV          PIC S9(3)           COMP-3.                  
007700*                                 TRANSPORTFÖRPACKNINGSKOD                
007800*                                 PACKING CODE                            
007900     03 FK-KDFKTYP           PIC X.                                       
008000*                                 TYP AV AVVIKELSE I FK-SEGMENTET         
008100*                                 FREIGHT CODE SEGMENT TYPE               
008200     03 FK-KDGRANS           PIC S9(3)           COMP-3.                  
008300*                                 GRÄNSKOD                                
008400*                                 BORDER CODE                             
008500     03 FK-KDTRPKAT          PIC X.                                       
008600*                                 TRANSPORTKATEGORI                       
008700*                                 TRANSPORT CATEGORY                      
008800     03 FK-PRLEGKST          PIC S9(7)V9(2)      COMP-3.                  
008900*                                 LEGALISERINSKOSTNAD                     
009000*                                 LEGALIZATION FEE                        
009100     03 FK-REFOERS           PIC S9(2)V9(3)      COMP-3.                  
009200*                                 FÖRSÄKRINGKOSTNADSFAKTOR                
009300*                                 INSURANCE COSTS                         
009400*** END OF VILMAII-COPY LENGTH= 110 BYTES                                 
