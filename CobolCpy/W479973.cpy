000100 01  W479973.                                                             
000200*                                 CTXT FÖR SKAPANDE AV PREEXTRAKT         
000300*                                 FÖR WDQ2; SEG WDQ212.                   
000400     03 IDSEGM               PIC X(6).                                    
000500*                                 SEGMENT                                 
000600     03 IDDC                 PIC X(2).                                    
000700*                                 IDENTIFIERARE LAGER                     
000800     03 KDFRAKT              PIC S9(3)           COMP-3.                  
000900*                                 FRAKTSÄTT C1-C2 TILL KUND               
001000     03 IDPRC-GRP.                                                        
001100*                                 PRODUKTIONSKANALER                      
001200        05 IDPRC             OCCURS 99 TIMES.                             
001300*                                 PRODUKTIONSKANAL                        
001400           07 IDPRCBAS       PIC X(3).                                    
001500*                                 PRC-BAS                                 
001600           07 IDPRCVAR       PIC X.                                       
001700*                                 PRC-VARIANT                             
