000100 01  MOD-W1O21101.                                                        
000200*                                 MOD-COPYTEXT FÖR W1021100               
000300     03 MOD-IDTRANS          PIC X(4).                                    
000400*                                 BILDNUMMER                              
000500     03 MOD-TEMFSFEL         PIC X(40).                                   
000600*                                 MFS FELMEDDELANDE                       
000700     03 MOD-IDARTNR-IN       PIC X(9).                                    
000800*                                 ARTIKELNUMMER                           
000900     03 MOD-IDARTNR-UT       PIC X(9).                                    
001000*                                 ARTIKELNUMMER                           
001100     03 MOD-IDSKYLT-IN       PIC X(3).                                    
001200*                                 NATIONALITETSTECKEN                     
001300*                                 SPRÅKIDENTIFIKATION                     
001400     03 MOD-IDSKYLT-UT       PIC X(3).                                    
001500*                                 NATIONALITETSTECKEN                     
001600*                                 SPRÅKIDENTIFIKATION                     
001700     03 MOD-IDARTNR-NY-ATTR  PIC X(2).                                    
001800*                                 MFS ATTRIBUTFÄLT                        
001900     03 MOD-IDARTNR-NY       PIC X(9).                                    
002000*                                 ARTIKELNUMMER                           
002100     03 MOD-BEART-UT         PIC X(25).                                   
002200*                                 ARTIKELBENÄMNING                        
002300     03 MOD-KDBENHOM-UT      PIC 9.                                       
002400*                                 HOMONYMKOD                              
002500     03 MOD-TIREGDAT         PIC 9(6).                                    
002600*                                 REGISTRERINGSDATUM (ÅÅMMDD)             
002700     03 MOD-BEART-IN-ATTR    PIC X(2).                                    
002800*                                 MFS ATTRIBUTFÄLT                        
002900     03 MOD-BEART-IN         PIC X(25).                                   
003000*                                 ARTIKELBENÄMNING                        
003100     03 MOD-KDBENHOM-IN-ATTR PIC X(2).                                    
003200*                                 MFS ATTRIBUTFÄLT                        
003300     03 MOD-KDBENHOM-IN      PIC 9.                                       
003400*                                 HOMONYMKOD                              
003500     03 MOD-KDPRODSL-UT      PIC Z9.                                      
003600*                                 PRODUKTSLAG                             
003700     03 MOD-IDFKNGRP-UT      PIC Z(3)9.                                   
003800*                                 FUNKTIONSGRUPP                          
003900     03 MOD-IDSTRTYP-UT      PIC X.                                       
004000*                                 STRUKTURTYP                             
004100     03 MOD-BORT-ATTR        PIC X(2).                                    
004200*                                 MFS ATTRIBUTFÄLT                        
004300     03 MOD-BORT             PIC X.                                       
004400     03 MOD-KDPRODSL-IN-ATTR PIC X(2).                                    
004500*                                 MFS ATTRIBUTFÄLT                        
004600     03 MOD-KDPRODSL-IN      PIC X(2).                                    
004700*                                 PRODUKTSLAG                             
004800     03 MOD-IDFKNGRP-IN-ATTR PIC X(2).                                    
004900*                                 MFS ATTRIBUTFÄLT                        
005000     03 MOD-IDFKNGRP-IN      PIC Z(3)9.                                   
005100*                                 FUNKTIONSGRUPP                          
005200     03 MOD-IDSTRTYP-IN-ATTR PIC X(2).                                    
005300*                                 MFS ATTRIBUTFÄLT                        
005400     03 MOD-IDSTRTYP-IN      PIC X.                                       
005500*                                 STRUKTURTYP                             
005600     03 MOD-IDLEVNR          PIC X(5).                                    
005700*                                 LEVERANTÖRNUMMER                        
005800     03 MOD-KDERS            PIC Z9.                                      
005900*                                 ERSÄTTNINGSKOD                          
006000     03 MOD-BELEVART         PIC X(30).                                   
006100*                                 LEVERANTÖRENS ARTIKELBENÄMNING          
006200     03 MOD-IDAO             OCCURS 5 TIMES                               
006300                             PIC X(10).                                   
006400*                                 ÄNDRINGSORDERNUMMER                     
006500     03 MOD-TEARTNOT         PIC X(40).                                   
006600*                                 ARTIKEL NOTERING                        
006700     03 MOD-TEARTNOT-7       PIC X(40).                                   
006800*                                 ARTIKEL NOTERING                        
006900     03 MOD-TESTRNOT-GRUPP   OCCURS 2 TIMES.                              
007000*                                                                         
007100        05 MOD-TESTRNOT-ATTR PIC X(2).                                    
007200*                                 MFS ATTRIBUTFÄLT                        
007300        05 MOD-TESTRNOT      PIC X(70).                                   
007400*                                 STRUKTURNOTERING                        
007500     03 MOD-TEMFSINF         PIC X(55).                                   
007600*                                 INFORMATIONSMEDDELANDE                  
007700*** END OF VILMAII-COPY LENGTH= 530 BYTES                                 
