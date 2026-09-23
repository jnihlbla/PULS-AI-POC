000100 01  W37110.                                                              
000200*                                 GODKÄNDA BYTESOBJEKT FRÅN BILD          
000300*                                 3171-3172                               
000400*                                 TILL BYTES (W371)                       
000500     03 IDPTYP               PIC X(3).                                    
000600*                                 POSTTYP                                 
000700     03 IDGMTREF.                                                         
000800*                                 GODSMOTTAGAREREFERENS                   
000900        05 IDDISTR           PIC S9(5)           COMP-3.                  
001000*                                 DISTRIKTNUMMER                          
001100        05 IDKUNDNR          PIC S9(7)           COMP-3.                  
001200*                                 KUNDNUMMER                              
001300        05 IDKUNDRF          PIC X(10).                                   
001400*                                 KUNDENS REFERENS (ORDERID)              
001500        05 IDORDNR5-FILLER REDEFINES IDKUNDRF.                            
001600           07 IDORDNR5       PIC 9(5).                                    
001700*                                 ORDERNUMMER                             
001800           07 FILLER         PIC X(5).                                    
001900        05 IDORDNR7-FILLER REDEFINES IDKUNDRF.                            
002000           07 IDORDNR7       PIC 9(7).                                    
002100*                                 ORDERNUMMER                             
002200           07 FILLER         PIC X(3).                                    
002300     03 IDARTNR-OBJ          PIC S9(9)           COMP-3.                  
002400*                                 OBJEKTNUMMER                            
002500     03 IDDC                 PIC X(2).                                    
002600*                                 IDENTIFIERARE LAGER                     
002700     03 KVRETUR-GODK         PIC S9(5)           COMP-3.                  
002800*                                 ANTAL I RETUR                           
002900     03 BERADREF             PIC X(10).                                   
003000*                                 KUNDENS RADREFERENS                     
003100     03 KDBYTREF             PIC X(3).                                    
003200*                                 CENTRAL REFERENS                        
003300*** END OF VILMAII-COPY LENGTH= 43 BYTES                                  
