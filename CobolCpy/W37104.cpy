000100 01  W37104.                                                              
000200*                                 GODKÄNDA BYTESOBJEKT FRÅN BILD          
000300*                                 3171-3172                               
000400*                                 TILL BYTES                              
000500     03 IDPTYP               PIC X(3).                                    
000600*                                 POSTTYP                                 
000700     03 IDGMTREF.                                                         
000800*                                 GODSMOTTAGAREREFERENS                   
000900        05 IDDISTR           PIC S9(5)           COMP-3.                  
001000*                                 DISTRIKTNUMMER                          
001100        05 IDKUNDNR          PIC S9(7)           COMP-3.                  
001200*                                 KUNDNUMMER                              
001300        05 IDKUNDRF-GRP.                                                  
001400*                                 KUNDENS REFERENS (ORDERID)              
001500           07 IDKUNDRF       PIC X(10).                                   
001600*                                 KUNDENS REFERENS (ORDERID)              
001700           07 IDORDNR5-FILLER REDEFINES IDKUNDRF.                         
001800              09 IDORDNR5    PIC 9(5).                                    
001900*                                 ORDERNUMMER                             
002000              09 FILLER      PIC X(5).                                    
002100           07 IDORDNR7-FILLER REDEFINES IDKUNDRF.                         
002200              09 IDORDNR7    PIC 9(7).                                    
002300*                                 ORDERNUMMER                             
002400              09 FILLER      PIC X(3).                                    
002500     03 IDARTNR-OBJ          PIC S9(9)           COMP-3.                  
002600*                                 OBJEKTNUMMER                            
002700     03 IDDC                 PIC X(2).                                    
002800*                                 IDENTIFIERARE LAGER                     
002900     03 KVRETUR-GODK         PIC S9(7)           COMP-3.                  
003000*                                 ANTAL I RETUR                           
003100     03 TIREGDAT-GODK        PIC S9(7)           COMP-3.                  
003200*                                 REGISTRERINGSDATUM (ÅÅMMDD)             
003300     03 IDBYTRAP             PIC S9(7)           COMP-3.                  
003400*                                 RAPPORTNUMMER  BYTES                    
003500     03 KDBYTREF             PIC X(3).                                    
003600*                                 CENTRAL REFERENS                        
003700     03 FLSKROT              PIC X.                                       
003800*                                 SKROTNINGSMARKERING                     
003900*** END OF VILMAII-COPY LENGTH= 43 BYTES                                  
