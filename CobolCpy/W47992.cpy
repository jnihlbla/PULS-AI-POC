000100 01  W47992.                                                              
000200*                                 CTXT FÖR PREEXTRAKT AV WDQ2.            
000300     03 IDORDER              PIC S9(7)           COMP-3.                  
000400*                                 VOLVO PARTS ORDERNUMMER                 
000500     03 IDDC                 PIC X(2).                                    
000600*                                 IDENTIFIERARE LAGER                     
000700     03 KDFRAKT              PIC S9(3)           COMP-3.                  
000800*                                 FRAKTSÄTT C1-C2 TILL KUND               
000900     03 KDORDKL              PIC S9              COMP-3.                  
001000*                                 ORDERKLASS                              
001100     03 IDPRC-GRP.                                                        
001200*                                 PRODUKTIONSKANALER                      
001300        05 IDPRC             OCCURS 99 TIMES.                             
001400*                                 PRODUKTIONSKANAL                        
001500           07 IDPRCBAS       PIC X(3).                                    
001600*                                 PRC-BAS                                 
001700           07 IDPRCVAR       PIC X.                                       
001800*                                 PRC-VARIANT                             
001900*** END OF VILMAII-COPY LENGTH= 405 BYTES                                 
