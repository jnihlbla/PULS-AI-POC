000100 01  4104-WDGX4104.                                                       
000200*                                 ATTEST AV KREDITNOTA                    
000300*                                 VÄRDE PER KREDITNOTA                    
000400*                                 WDR5                                    
000500*                                 FYSISK NYCKEL: KY4104                   
000600*                                 (IDDC + KDKRENOT)                       
000700     03 4104-IDDC            PIC X(2).                                    
000800*                                 IDENTIFIERARE LAGER                     
000900*                                 WAREHOUSE IDENTIFIER                    
001000     03 4104-KDKRENOT        PIC X(2).                                    
001100*                                 TYP AV KREDITERING                      
001200*                                 CREDIT NOTE TYPE                        
001300     03 4104-FLKREATT        PIC X.                                       
001400*                                 ATTEST AV KREDITNOTA                    
001500*                                 ATEST OF CREDIT NOTE                    
001600     03 4104-KDVALISO        PIC X(3).                                    
001700*                                 VALUTAKOD ENLIGT ISO-STANDARD.          
001800*                                 CURRENCY CODE BY ISO-STANDARD.          
001900     03 4104-SUKRENOT        PIC S9(7)V9(2)      COMP-3.                  
002000*                                 KREDITNOTASUMMA                         
002100*                                 CREDIT NOTE TOTAL                       
002200     03 4104-FILLER          PIC X(7).                                    
002300*** END OF VILMAII-COPY LENGTH= 20 BYTES                                  
