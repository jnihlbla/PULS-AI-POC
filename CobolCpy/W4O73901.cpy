000100 01  MOD-W4O73901.                                                        
000200*                                 MOD-COPYTEXT FOR PGM W4073900           
000300     03 MOD-IDTRANS          PIC X(4).                                    
000400*                                 BILDNUMMER                              
000500     03 MOD-TEMFSFEL         PIC X(40).                                   
000600*                                 MFS FELMEDDELANDE                       
000700     03 MOD-IDDISTR-IN       PIC X(2).                                    
000800*                                 MFS BEHANDLING AV INPUTFÄLT             
000900     03 MOD-IDDISTR-UT       PIC X(4).                                    
001000*                                 DISTRIKTNUMMER                          
001100     03 MOD-IDKUNDNR-IN      PIC X(2).                                    
001200*                                 MFS BEHANDLING AV INPUTFÄLT             
001300     03 MOD-IDKUNDNR-UT      PIC X(6).                                    
001400*                                 KUNDNUMMER                              
001500     03 MOD-IDRAPPNR-IN      PIC X(2).                                    
001600*                                 MFS BEHANDLING AV INPUTFÄLT             
001700     03 MOD-IDRAPPNR-UT      PIC X(7).                                    
001800*                                 RAPPORT NUMMER                          
001900     03 MOD-IDARTNR-IN       PIC X(2).                                    
002000*                                 MFS BEHANDLING AV INPUTFÄLT             
002100     03 MOD-IDARTNR-UT       PIC X(9).                                    
002200*                                 ARTIKELNUMMER                           
002300     03 MOD-IDRADNR-IN       PIC X(2).                                    
002400*                                 MFS BEHANDLING AV INPUTFÄLT             
002500     03 MOD-IDRADNR-UT       PIC X(4).                                    
002600*                                 RADNUMMER                               
002700     03 MOD-TIRETILL         PIC 9(6).                                    
002800*                                 RETURTILLSTÅNDSDATUM                    
002900     03 MOD-TIRETANK         PIC 9(6).                                    
003000*                                 ANKOMSTDATUM                            
003100     03 MOD-KDLEVANM         PIC X.                                       
003200*                                 STATUS LEVERANSANMÄRKNING               
003300     03 MOD-IDANSTNR-RET     PIC Z(4)9.                                   
003400*                                 ANSTÄLLNINGSNUMMER RETURAVDELN.         
003500     03 MOD-RADER            OCCURS 10 TIMES.                             
003600*                                 RADINFORMATION                          
003700        05 MOD-IDTRANS-ATTR  PIC X(2).                                    
003800*                                 MFS ATTRIBUTFÄLT                        
003900        05 MOD-IDTRANS-HOPP  PIC X(4).                                    
004000*                                 BILDNUMMER                              
004100        05 MOD-IDARTNR       PIC Z(7)9.                                   
004200*                                 ARTIKELNUMMER                           
004300        05 MOD-IDRADNR       PIC Z(3)9.                                   
004400*                                 RADNUMMER                               
004500        05 MOD-IDDC          PIC X(2).                                    
004600*                                 IDENTIFIERARE LAGER                     
004700        05 MOD-IDORDNR5      PIC Z(4)9.                                   
004800*                                 ORDERNUMMER                             
004900        05 MOD-KVLEVANM      PIC Z(4)9.                                   
005000*                                 LEVERANSANMÄRKNINGSANTAL                
005100        05 MOD-TIINLINL      PIC 9(6).                                    
005200*                                 RAPPORTERINGSDATUM INLAGD (R32)         
005300        05 MOD-KVRETINL      PIC Z(5)9.                                   
005400*                                 INLAGT ANTAL VID RETUR                  
005500        05 MOD-KVRETINL-SKR  PIC Z(5)9.                                   
005600*                                 INRPT ANTAL SOM SKROTATS                
005700        05 MOD-KVAVV-KVANT   PIC Z(5)9.                                   
005800*                                 ANTALSAVVIKELSE KVANTITET               
005900        05 MOD-KVAVV-KVAL    PIC Z(5)9.                                   
006000*                                 ANTALSAVVIKELSE KVALITET                
006100        05 MOD-FLTEXT        PIC X.                                       
006200*                                 FINNS TEXTINFORMATION ?                 
006300        05 MOD-IDARTNR-DEL   PIC Z(7)9.                                   
006400*                                 LEVERERAD ARTIKEL                       
006500     03 MOD-INDATA.                                                       
006600*                                 INDATA BILD 4739                        
006700        05 MOD-IDARTNR-UPD-ATTR                                           
006800                             PIC X(2).                                    
006900*                                 MFS ATTRIBUTFÄLT                        
007000        05 MOD-IDARTNR-UPD   PIC X(2).                                    
007100*                                 MFS BEHANDLING AV INPUTFÄLT             
007200        05 MOD-IDRADNR-UPD-ATTR                                           
007300                             PIC X(2).                                    
007400*                                 MFS ATTRIBUTFÄLT                        
007500        05 MOD-IDRADNR-UPD   PIC X(2).                                    
007600*                                 MFS BEHANDLING AV INPUTFÄLT             
007700     03 MOD-TEMFSINF         PIC X(55).                                   
007800*                                 INFORMATIONSMEDDELANDE                  
007900*** END OF VILMAII-COPY LENGTH= 855 BYTES                                 
