000100 01  W37149.                                                              
000200*                                 GODKÄNDA BYTES OBJEKT SENASTE           
000300*                                 MÅNADEN KOMPLETTERAD MED LEVNR          
000400     03 IDGMTREF.                                                         
000500*                                 GODSMOTTAGAREREFERENS                   
000600        05 IDDISTR           PIC S9(5)           COMP-3.                  
000700*                                 DISTRIKTNUMMER                          
000800        05 IDKUNDNR          PIC S9(7)           COMP-3.                  
000900*                                 KUNDNUMMER                              
001000        05 IDKUNDRF-GRP.                                                  
001100*                                 KUNDENS REFERENS (ORDERID)              
001200           07 IDKUNDRF       PIC X(10).                                   
001300*                                 KUNDENS REFERENS (ORDERID)              
001400           07 IDORDNR5-FILLER REDEFINES IDKUNDRF.                         
001500              09 IDORDNR5    PIC 9(5).                                    
001600*                                 ORDERNUMMER                             
001700              09 FILLER      PIC X(5).                                    
001800           07 IDORDNR7-FILLER REDEFINES IDKUNDRF.                         
001900              09 IDORDNR7    PIC 9(7).                                    
002000*                                 ORDERNUMMER                             
002100              09 FILLER      PIC X(3).                                    
002200     03 IDBYTRAP             PIC S9(7)           COMP-3.                  
002300*                                 RAPPORTNUMMER  BYTES                    
002400     03 TIREGDAT-DEALER      PIC S9(7)           COMP-3.                  
002500*                                 REGISTRERINGSDATUM (ÅÅMMDD)             
002600     03 IDFAKT               PIC S9(7)           COMP-3.                  
002700*                                 FAKTURANUMMER                           
002800     03 IDBYTRAD             PIC S9(5)           COMP-3.                  
002900*                                 RADNUMMER                               
003000     03 IDARTNR-OBJ          PIC S9(9)           COMP-3.                  
003100*                                 OBJEKTNUMMER                            
003200     03 IDTABNR              PIC S9(3)           COMP-3.                  
003300*                                 TABELLNUMMER                            
003400     03 BEART-SVE            PIC X(25).                                   
003500*                                 ARTIKELBENÄMNING                        
003600     03 BEART-ENG            PIC X(25).                                   
003700*                                 ARTIKELBENÄMNING                        
003800     03 IDFKNGRP             PIC S9(5)           COMP-3.                  
003900*                                 FUNKTIONSGRUPP                          
004000     03 KVRETUR-URSP         PIC S9(7)           COMP-3.                  
004100*                                 ANTAL OBJEKT RETURER.                   
004200     03 KVRETUR-GODK         PIC S9(7)           COMP-3.                  
004300*                                 ANTAL GODKÄNDA BYTESOBJEKT              
004400     03 KDBYTSTA-OBJ         PIC X.                                       
004500*                                 STATUSKOD BYTESOBJEKT                   
004600     03 TIANKDAG             PIC S9(7)           COMP-3.                  
004700*                                 ANKOMSTDAG                              
004800     03 TIREGDAT-GODK        PIC S9(7)           COMP-3.                  
004900*                                 REGISTRERINGSDATUM (ÅÅMMDD)             
005000     03 IDLEVNR              PIC X(5).                                    
005100*                                 LEVERANTÖRNUMMER                        
005200     03 IDUSER               PIC X(8).                                    
005300*                                 ANVÄNDARENS SÄKERHETS ID                
005400     03 BERADREF             PIC X(10).                                   
005500*                                 KUNDENS RADREFERENS                     
005600     03 KDBYTREF             PIC X(3).                                    
005700*                                 CENTRAL REFERENS                        
005800     03 IDDC                 PIC X(2).                                    
005900*                                 IDENTIFIERARE LAGER                     
006000*** END OF VILMAII-COPY LENGTH= 137 BYTES                                 
