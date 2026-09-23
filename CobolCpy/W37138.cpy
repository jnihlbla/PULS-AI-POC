000100 01  W37138.                                                              
000200*                                 GODKÄNDA BYTES OBJEKT TILL              
000300*                                 HIST-FIL VID RENSNING AV WDM6           
000400     03 IDGMTREF.                                                         
000500*                                 GODSMOTTAGAREREFERENS                   
000600        05 IDDISTR           PIC S9(5)           COMP-3.                  
000700*                                 DISTRIKTNUMMER                          
000800        05 IDKUNDNR          PIC S9(7)           COMP-3.                  
000900*                                 KUNDNUMMER                              
001000        05 IDKUNDRF          PIC X(10).                                   
001100*                                 KUNDENS REFERENS (ORDERID)              
001200        05 IDORDNR5-FILLER REDEFINES IDKUNDRF.                            
001300           07 IDORDNR5       PIC 9(5).                                    
001400*                                 ORDERNUMMER                             
001500           07 FILLER         PIC X(5).                                    
001600        05 IDORDNR7-FILLER REDEFINES IDKUNDRF.                            
001700           07 IDORDNR7       PIC 9(7).                                    
001800*                                 ORDERNUMMER                             
001900           07 FILLER         PIC X(3).                                    
002000     03 IDBYTRAP             PIC S9(7)           COMP-3.                  
002100*                                 RAPPORTNUMMER  BYTES                    
002200     03 TIREGDAT-DEALER      PIC S9(7)           COMP-3.                  
002300*                                 REGISTRERINGSDATUM (ÅÅMMDD)             
002400     03 IDFAKT               PIC S9(7)           COMP-3.                  
002500*                                 FAKTURANUMMER                           
002600     03 IDBYTRAD             PIC S9(5)           COMP-3.                  
002700*                                 RADNUMMER                               
002800     03 IDARTNR-OBJ          PIC S9(9)           COMP-3.                  
002900*                                 OBJEKTNUMMER                            
003000     03 IDTABNR              PIC S9(3)           COMP-3.                  
003100*                                 TABELLNUMMER                            
003200     03 BEART-SVE            PIC X(25).                                   
003300*                                 ARTIKELBENÄMNING                        
003400     03 BEART-ENG            PIC X(25).                                   
003500*                                 ARTIKELBENÄMNING                        
003600     03 IDFKNGRP             PIC S9(5)           COMP-3.                  
003700*                                 FUNKTIONSGRUPP                          
003800     03 KVRETUR-URSP         PIC S9(7)           COMP-3.                  
003900*                                 ANTAL OBJEKT RETURER.                   
004000     03 KVRETUR-GODK         PIC S9(7)           COMP-3.                  
004100*                                 ANTAL GODKÄNDA BYTESOBJEKT              
004200     03 KDBYTSTA-OBJ         PIC X.                                       
004300*                                 STATUSKOD BYTESOBJEKT                   
004400     03 TIANKDAG             PIC S9(7)           COMP-3.                  
004500*                                 ANKOMSTDAG                              
004600     03 TIREGDAT-GODK        PIC S9(7)           COMP-3.                  
004700*                                 REGISTRERINGSDATUM (ÅÅMMDD)             
004800     03 IDUSER               PIC X(8).                                    
004900*                                 ANVÄNDARENS SÄKERHETS ID                
005000     03 BERADREF             PIC X(10).                                   
005100*                                 KUNDENS RADREFERENS                     
005200     03 KDBYTREF             PIC X(3).                                    
005300*                                 CENTRAL REFERENS                        
005400     03 IDDC                 PIC X(2).                                    
005500*                                 IDENTIFIERARE LAGER                     
005600*** END OF VILMAII-COPY LENGTH= 132 BYTES                                 
