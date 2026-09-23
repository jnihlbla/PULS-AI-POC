000100 01  4446-WDGX4446.                                                       
000200*                                 BESKRIVNING AV                          
000300*                                 PRC STYR TABELLER                       
000400*                                 FYSISK NYCKEL WDGXKEY:                  
000500*                                 (IDRADNR + LOW-VALUE)                   
000600     03 4446-IDRADNR         PIC S9(5)           COMP-3.                  
000700*                                 RADNUMMER                               
000800*                                 LINE NO                                 
000900     03 4446-LOW-VALUE       PIC X(2).                                    
001000     03 4446-IDGMTOMR-FOM    PIC 9(4).                                    
001100*                                 GODSMOTTAGAREOMRÅDE FRÅN                
001200*                                 GOODS RECEIVING AREA FROM               
001300     03 4446-IDGMTOMR-TOM    PIC 9(4).                                    
001400*                                 GODSMOTTAGAREOMRÅDE TILL                
001500*                                 GOODS RECEIVING AREA TO                 
001600     03 4446-IDHLO-FOM       PIC 9(2).                                    
001700*                                 HUVUDLAGEROMRÅDE FRÅN OCH MED           
001800*                                 MAIN AREA FROM                          
001900     03 4446-IDHLO-TOM       PIC 9(2).                                    
002000*                                 HUVUDLAGEROMRÅDE TILL OCH MED           
002100*                                 MAIN AREA TO                            
002200     03 4446-IDTRP.                                                       
002300*                                 TRANSPORTIDENTITET                      
002400*                                 TRANSPORTIDENTITY                       
002500        05 4446-IDTRPLOS     PIC X(3).                                    
002600*                                 TRANSPORTLÖSNING                        
002700*                                 TRANSPORTSOLUTION                       
002800        05 4446-IDTRPVAR     PIC X(2).                                    
002900*                                 TRANSPORTLÖSNINGSGRUPP                  
003000*                                 TRANSPORTSOLUTIONGROUP                  
003100     03 4446-KDFRAKT-FOM     PIC S9(3)           COMP-3.                  
003200*                                 FRAKTSÄTT FRÅN OCH MED                  
003300*                                 FREIGHT CODE FROM                       
003400     03 4446-KDFRAKT-TOM     PIC S9(3)           COMP-3.                  
003500*                                 FRAKTSÄTT TILL OCH MED                  
003600*                                 FREIGHT CODE TO                         
003700     03 4446-KDPRODKL-FOM    PIC X.                                       
003800*                                 PRODUKTIONSKLASS FRÅN                   
003900*                                 PRODUCTION CLASS FROM                   
004000     03 4446-KDPRODKL-TOM    PIC X.                                       
004100*                                 PRODUKTIONSKLASS TILL                   
004200*                                 PRODUCTION CLASS TO                     
004300     03 4446-IDPRC.                                                       
004400*                                 PRODUKTIONSKANAL                        
004500*                                 PRODUCTION CHANNEL                      
004600        05 4446-IDPRCBAS     PIC X(3).                                    
004700*                                 PRC-BAS                                 
004800*                                 PRC-BASIC                               
004900        05 4446-IDPRCVAR     PIC X.                                       
005000*                                 PRC-VARIANT                             
005100*                                 PRC-VARIANT                             
005200     03 4446-IDPTIDTAB       PIC 9(2).                                    
005300*                                 PRODUKTIONSTIDTABELLSIDENTITET          
005400*                                 PRODUCTION TIME TABLE IDENT.            
005500     03 4446-FILLER          PIC X(26).                                   
005600*** END COPY WDGX4446C0  LENGTH=60                                        
