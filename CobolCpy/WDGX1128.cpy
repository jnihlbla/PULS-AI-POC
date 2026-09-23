000100 01  1128-WDGX1128.                                                       
000200*                                 BASLAGER                                
000300*                                 ORDERGENERERING                         
000400*                                 FYSISK NYCKEL: WDGXKEY                  
000500*                                 (IDDISTR + IDKUNDNR)                    
000600     03 1128-IDDISTR         PIC S9(5)           COMP-3.                  
000700*                                 DISTRIKTNUMMER                          
000800*                                 DISTRICT NUMBER                         
000900     03 1128-IDKUNDNR        PIC S9(7)           COMP-3.                  
001000*                                 KUNDNUMMER                              
001100*                                 CUSTOMER NO                             
001200     03 1128-KVBASLMK        PIC S9(7)           COMP-3.                  
001300*                                 BASLAGER KUND KVANTITET                 
001400*                                 BASIC STOCK CUSTUMER QUANTITY           
001500     03 FILLER               PIC X(10).                                   
001600*** END COPY WDGX1128C0  LENGTH=21                                        
