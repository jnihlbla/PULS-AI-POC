000100 01  4437-WDGX4437.                                                       
000200*                                 ARBETSTIDSKALENDER                      
000300*                                 FYSISK NYCKEL WDGXKEY:                  
000400*                                   (IDHTYP + IDDC +                      
000500*                                    IDPRC  + LOW-VALUE)                  
000600     03 4437-IDHTYP          PIC X(4).                                    
000700*                                 HÄNDELSETYP                             
000800     03 4437-IDDC            PIC X(2).                                    
000900*                                 IDENTIFIERARE LAGER                     
001000*                                 WAREHOUSE IDENTIFIER                    
001100     03 4437-IDPRC.                                                       
001200*                                 PRODUKTIONSKANAL                        
001300*                                 PRODUCTION CHANNEL                      
001400        05 4437-IDPRCBAS     PIC X(3).                                    
001500*                                 PRC-BAS                                 
001600*                                 PRC-BASIC                               
001700        05 4437-IDPRCVAR     PIC X.                                       
001800*                                 PRC-VARIANT                             
001900*                                 PRC-VARIANT                             
002000     03 4437-LOW-VALUE       PIC X(20).                                   
002100*** END COPY WDGX4437    LENGTH=30                                        
