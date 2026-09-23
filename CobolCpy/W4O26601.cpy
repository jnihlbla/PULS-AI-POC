000100 01  MOD-W4O26601.                                                        
000200*                                 MOD-COPYTEXT FÖR W4026600               
000300     03 MOD-IDTRANS          PIC X(4).                                    
000400*                                 BILDNUMMER                              
000500     03 MOD-TEMFSFEL         PIC X(40).                                   
000600*                                 MFS FELMEDDELANDE                       
000700     03 MOD-IDDISTR-IN       PIC X(4).                                    
000800*                                 DISTRIKTNUMMER                          
000900     03 MOD-IDKUNDNR-IN      PIC X(6).                                    
001000*                                 KUNDNUMMER                              
001100     03 MOD-IDORDNR7-IN      PIC X(7).                                    
001200*                                 ORDERNUMMER                             
001300     03 MOD-IDARTNR-IN       PIC X(9).                                    
001400*                                 ARTIKELNUMMER                           
001500     03 MOD-IDDISTR-UT       PIC X(4).                                    
001600*                                 DISTRIKTNUMMER                          
001700     03 MOD-IDKUNDNR-UT      PIC X(6).                                    
001800*                                 KUNDNUMMER                              
001900     03 MOD-IDORDNR7-UT      PIC X(7).                                    
002000*                                 ORDERNUMMER                             
002100     03 MOD-IDARTNR-UT       PIC X(9).                                    
002200*                                 ARTIKELNUMMER                           
002300     03 MOD-TEDDI            PIC X(11).                                   
002400*                                 TEXTFÄLT DDI                            
002500     03 MOD-VKORDNTO         PIC Z(5)9.9.                                 
002600*                                 ORDERVIKT NETTO (KG)                    
002700     03 MOD-SUFKTBEL         PIC Z(8)9.9(2).                              
002800*                                 SUMMA FAKTURERAT BELOPP                 
002900     03 MOD-TEASTRIX         PIC X.                                       
003000*                                 ASTERISK                                
003100     03 MOD-PRODSL-KVMOTOR   PIC X.                                       
003200*                                 ALLMÄN FLAGGA                           
003300     03 MOD-PRODSL-KVKAROSS  PIC X.                                       
003400*                                 ALLMÄN FLAGGA                           
003500     03 MOD-VKORDBTO-ATTR    PIC X(2).                                    
003600*                                 MFS ATTRIBUTFÄLT                        
003700     03 MOD-VKORDBTO         PIC Z(5)9.9.                                 
003800*                                 ORDERVIKT BRUTTO (KG)                   
003900     03 MOD-KDFARLIG         PIC X.                                       
004000*                                 ALLMÄN FLAGGA                           
004100     03 MOD-VLORDBTO-ATTR    PIC X(2).                                    
004200*                                 MFS ATTRIBUTFÄLT                        
004300     03 MOD-VLORDBTO         PIC Z(3)9.9(3).                              
004400*                                 ORDERVOLYM BRUTTO (M3)                  
004500     03 MOD-KDBETVIL-ATTR    PIC X(2).                                    
004600*                                 MFS ATTRIBUTFÄLT                        
004700     03 MOD-KDBETVIL         PIC X(4).                                    
004800*                                 BETALNINGSVILLKOR SAP                   
004900     03 MOD-BEBETVIL         PIC X(30).                                   
005000*                                 BETALNINGSVILLKORSTEXT                  
005100     03 MOD-KDLEVVIL-ATTR    PIC X(2).                                    
005200*                                 MFS ATTRIBUTFÄLT                        
005300     03 MOD-KDLEVVIL         PIC 9.                                       
005400*                                 LEVERANSVILLKOR                         
005500     03 MOD-BELEVVIL         PIC X(35).                                   
005600*                                 LEVERANSVILLKOR                         
005700     03 MOD-TIGILTIG-ATTR    PIC X(2).                                    
005800*                                 MFS ATTRIBUTFÄLT                        
005900     03 MOD-TIGILTIG         PIC 9(6).                                    
006000*                                 GILTIGHETSDATUM (ÅÅMMDD)                
006100     03 MOD-TIFORDAT-ATTR    PIC X(2).                                    
006200*                                 MFS ATTRIBUTFÄLT                        
006300     03 MOD-TIFORDAT         PIC 9(6).                                    
006400*                                 FÖRFALLODATUM                           
006500     03 MOD-FLAGGA-PRELPRIS-ATTR                                          
006600                             PIC X(2).                                    
006700*                                 MFS ATTRIBUTFÄLT                        
006800     03 MOD-FLAGGA-PRELPRIS  PIC X.                                       
006900*                                 ALLMÄN FLAGGA                           
007000     03 MOD-IDPRT-UTSKRIFT-ATTR                                           
007100                             PIC X(2).                                    
007200*                                 MFS ATTRIBUTFÄLT                        
007300     03 MOD-IDPRT-UTSKRIFT   PIC X(3).                                    
007400*                                 LOGISK PRINTERIDENTITET                 
007500     03 MOD-IDPRT-RELEASE-ATTR                                            
007600                             PIC X(2).                                    
007700*                                 MFS ATTRIBUTFÄLT                        
007800     03 MOD-IDPRT-RELEASE    PIC X(3).                                    
007900*                                 LOGISK PRINTERIDENTITET                 
008000     03 MOD-IDKUNDNR-NY-ATTR PIC X(2).                                    
008100*                                 MFS ATTRIBUTFÄLT                        
008200     03 MOD-IDKUNDNR-NY      PIC X(6).                                    
008300*                                 KUNDNUMMER                              
008400     03 MOD-TEMFSINF         PIC X(55).                                   
008500*                                 INFORMATIONSMEDDELANDE                  
008600*** END OF VILMAII-COPY LENGTH= 317 BYTES                                 
