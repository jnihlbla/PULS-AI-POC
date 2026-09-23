000100 01  MOD-W3O17701.                                                        
000200*                                 MOD-COPYTEXT FÖR BILD 3177              
000300*                                 BYTES HISTORY SCREEN                    
000400     03 MOD-IDTRANS          PIC X(4).                                    
000500*                                 BILDNUMMER                              
000600     03 MOD-TEMFSFEL         PIC X(40).                                   
000700*                                 MFS FELMEDDELANDE                       
000800     03 MOD-IDARTNR-IN       PIC Z(9).                                    
000900*                                 ARTIKELNUMMER                           
001000     03 MOD-IDDISTR-IN       PIC Z(4).                                    
001100*                                 DISTRIKTNUMMER                          
001200     03 MOD-IDKUNDNR-IN      PIC Z(6).                                    
001300*                                 KUNDNUMMER                              
001400     03 MOD-IDBYTRAP-IN      PIC Z(7).                                    
001500*                                 RAPPORTNUMMER  BYTES                    
001600     03 MOD-KDBYTSTA-IN      PIC X.                                       
001700*                                 STATUSKOD BYTESOBJEKT                   
001800     03 MOD-IDDC-IN          PIC X(2).                                    
001900*                                 IDENTIFIERARE LAGER                     
002000     03 MOD-IDARTNR-UT       PIC Z(9).                                    
002100*                                 ARTIKELNUMMER                           
002200     03 MOD-IDDISTR-UT       PIC Z(4).                                    
002300*                                 DISTRIKTNUMMER                          
002400     03 MOD-IDKUNDNR-UT      PIC Z(6).                                    
002500*                                 KUNDNUMMER                              
002600     03 MOD-IDBYTRAP-UT      PIC Z(7).                                    
002700*                                 RAPPORTNUMMER  BYTES                    
002800     03 MOD-KDBYTSTA-UT      PIC X.                                       
002900*                                 STATUSKOD BYTESOBJEKT                   
003000     03 MOD-IDDC-UT          PIC X(2).                                    
003100*                                 IDENTIFIERARE LAGER                     
003200     03 MOD-DATA-UT          OCCURS 13 TIMES.                             
003300*                                 RAPPORTERINGS-FÄLT                      
003400        05 MOD-KDSVAR-ATTR   PIC X(2).                                    
003500*                                 MFS ATTRIBUTFÄLT                        
003600        05 MOD-KDSVAR        PIC X.                                       
003700*                                 SVAR FRÅN PROGRAM ELLER SKÄRM           
003800        05 MOD-IDDISTR       PIC Z(4).                                    
003900*                                 DISTRIKTNUMMER                          
004000        05 MOD-IDKUNDNR      PIC Z(6).                                    
004100*                                 KUNDNUMMER                              
004200        05 MOD-IDBYTRAP      PIC Z(7).                                    
004300*                                 RAPPORTNUMMER  BYTES                    
004400        05 MOD-TIREGDAT      PIC 9(6).                                    
004500*                                 REGISTRERINGSDATUM (ÅÅMMDD)             
004600        05 MOD-TIANKDAG      PIC 9(6).                                    
004700*                                 ANKOMSTDAG                              
004800        05 MOD-TIREGDAT-GODK PIC 9(6).                                    
004900*                                 REGISTRERINGSDATUM (ÅÅMMDD)             
005000        05 MOD-KDBYTSTA      PIC X.                                       
005100*                                 STATUSKOD BYTESOBJEKT                   
005200        05 MOD-FLBYTGAR      PIC X.                                       
005300*                                 GARANTI RAPPORT FLAGGA                  
005400*                                 Y = GARANTI                             
005500*                                 N = EJ GARANTI                          
005600        05 MOD-KVRETUR-URSP  PIC Z(7).                                    
005700*                                 ANTAL I RETUR                           
005800        05 MOD-KVRETUR-GODK  PIC Z(7).                                    
005900*                                 ANTAL I RETUR                           
006000        05 MOD-ANMARKNINGKOD PIC Z(3).                                    
006100     03 MOD-TEMFSINF         PIC X(55).                                   
006200*                                 INFORMATIONSMEDDELANDE                  
006300*** END OF VILMAII-COPY LENGTH= 898 BYTES                                 
