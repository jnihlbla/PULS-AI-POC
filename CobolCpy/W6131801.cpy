000100 01  W6131801.                                                            
000200*                                 VECKANS FÖRPACKNINGAR PÅ SVS            
000300     03 FILLER               PIC X.                                       
000400     03 IDDC                 PIC X(2).                                    
000500*                                 IDENTIFIERARE LAGER                     
000600     03 KDFORP.                                                           
000700*                                 FÖRPACKNINGSKOD                         
000800        05 KDFORPPL          PIC 9.                                       
000900*                                 FÖRPACKNINGSPLATS                       
001000        05 KDFORPGP          PIC 9(2).                                    
001100*                                 FÖRPACKNINGSGRUPP                       
001200        05 KDFORPUF          PIC 9.                                       
001300*                                 UPPRÄKNINGSFAKTOR                       
001400     03 KVANTAL-ST           PIC 9(7).                                    
001500*                                 STYCKPACKAT ANTAL PÅ SVS                
001600*                                                                         
001700     03 KVANTAL-28           PIC 9(7).                                    
001800*                                 ETIKETTERAT ANTAL PÅ SVS                
001900*                                                                         
002000     03 KVANTAL-GR           PIC 9(7).                                    
002100*                                 GROVPACKAT ANTAL PÅ SVS                 
002200     03 KVANTAL-MA           PIC 9(7).                                    
002300*                                 MASKINPACKAT ANTAL PÅ SVS               
002400*                                                                         
002500     03 TIAAVV               PIC X(4).                                    
002600*                                 ÅR - VECKA  (ÅÅVV)                      
002700*** END OF VILMAII-COPY LENGTH= 39 BYTES                                  
