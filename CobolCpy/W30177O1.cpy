000100 01  RESP-W30177O1.                                                       
000200*                                 RESP-COPYTEXT FÖR BILD 3177             
000300*                                                                         
000400*                                 BYTES HISTORY SCREEN                    
000500     03 RESP-IDARTNR-START   PIC 9(9).                                    
000600*                                 ARTIKELNUMMER                           
000700     03 RESP-IDDISTR-START   PIC 9(5).                                    
000800*                                 DISTRIKTNUMMER                          
000900     03 RESP-IDKUNDNR-START  PIC 9(7).                                    
001000*                                 KUNDNUMMER                              
001100     03 RESP-IDBYTRAP-START  PIC 9(7).                                    
001200*                                 RAPPORTNUMMER  BYTES                    
001300     03 RESP-KDBYTSTA-START  PIC X.                                       
001400*                                 STATUSKOD BYTESOBJEKT                   
001500     03 RESP-IDDC-START      PIC X(2).                                    
001600*                                 IDENTIFIERARE LAGER                     
001700     03 RESP-IDBYTRAD-START  PIC 9(5).                                    
001800*                                 RADNUMMER                               
001900     03 RESP-IDARNTR-NEXT    PIC 9(9).                                    
002000*                                 ARTIKELNUMMER                           
002100     03 RESP-IDDISTR-NEXT    PIC 9(5).                                    
002200*                                 DISTRIKTNUMMER                          
002300     03 RESP-IDKUNDNR-NEXT   PIC 9(7).                                    
002400*                                 KUNDNUMMER                              
002500     03 RESP-IDBYTRAP-NEXT   PIC 9(7).                                    
002600*                                 RAPPORTNUMMER  BYTES                    
002700     03 RESP-KDBYTSTA-NEXT   PIC X.                                       
002800*                                 STATUSKOD BYTESOBJEKT                   
002900     03 RESP-IDDC-NEXT       PIC X(2).                                    
003000*                                 IDENTIFIERARE LAGER                     
003100     03 RESP-IDBYTRAD-NEXT   PIC 9(5).                                    
003200*                                 RADNUMMER                               
003300     03 RESP-KVRADER         PIC 9(5).                                    
003400*                                 ANTAL RADER                             
003500     03 RESP-DATA-UT         OCCURS 500 TIMES.                            
003600*                                 RAPPORTERINGS-FÄLT                      
003700        05 RESP-IDDISTR-LINE PIC Z(4).                                    
003800*                                 DISTRIKTNUMMER                          
003900        05 RESP-IDKUNDNR-LINE                                             
004000                             PIC Z(6).                                    
004100*                                 KUNDNUMMER                              
004200        05 RESP-IDBYTRAP-LINE                                             
004300                             PIC Z(7).                                    
004400*                                 RAPPORTNUMMER  BYTES                    
004500        05 RESP-TIREGDAT-LINE                                             
004600                             PIC 9(6).                                    
004700*                                 REGISTRERINGSDATUM (ÅÅMMDD)             
004800        05 RESP-TIANKDAG-LINE                                             
004900                             PIC 9(6).                                    
005000*                                 ANKOMSTDAG                              
005100        05 RESP-TIREGDAT-GODK-LINE                                        
005200                             PIC 9(6).                                    
005300*                                 REGISTRERINGSDATUM (ÅÅMMDD)             
005400        05 RESP-KDBYTSTA-LINE                                             
005500                             PIC X.                                       
005600*                                 STATUSKOD BYTESOBJEKT                   
005700        05 RESP-FLBYTGAR-LINE                                             
005800                             PIC X.                                       
005900*                                 GARANTI RAPPORT FLAGGA                  
006000*                                 Y = GARANTI                             
006100*                                 N = EJ GARANTI                          
006200        05 RESP-KVRETUR-URSP-LINE                                         
006300                             PIC Z(7).                                    
006400*                                 ANTAL I RETUR                           
006500        05 RESP-KVRETUR-GODK-LINE                                         
006600                             PIC Z(7).                                    
006700*                                 ANTAL I RETUR                           
006800        05 RESP-ANMARKNINGKOD-LINE                                        
006900                             PIC Z(3).                                    
007000*** END OF VILMAII-COPY LENGTH= 27077 BYTES                               
