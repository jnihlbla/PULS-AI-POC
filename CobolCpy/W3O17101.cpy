000100 01  MOD-W3O17101.                                                        
000200*                                 COPYTEXT FÖR MOD                        
000300*                                 W3O17101                                
000400     03 MOD-IDTRANS          PIC X(4).                                    
000500*                                 BILDNUMMER                              
000600     03 MOD-TEMFSFEL         PIC X(40).                                   
000700*                                 MFS FELMEDDELANDE                       
000800     03 MOD-IDDISTR-IN       PIC X(4).                                    
000900*                                 DISTRIKTNUMMER                          
001000     03 MOD-IDDISTR-UT       PIC X(4).                                    
001100*                                 DISTRIKTNUMMER                          
001200     03 MOD-IDKUNDNR-IN      PIC X(6).                                    
001300*                                 KUNDNUMMER                              
001400     03 MOD-IDKUNDNR-UT      PIC X(6).                                    
001500*                                 KUNDNUMMER                              
001600     03 MOD-IDBYTRAP-IN      PIC X(7).                                    
001700*                                 RAPPORTNUMMER  BYTES                    
001800     03 MOD-IDBYTRAP-UT      PIC X(7).                                    
001900*                                 RAPPORTNUMMER  BYTES                    
002000     03 MOD-KDBYTSTA-IN      PIC X.                                       
002100*                                 STATUSKOD BYTESOBJEKT                   
002200     03 MOD-KDBYTSTA-UT      PIC X.                                       
002300*                                 STATUSKOD BYTESOBJEKT                   
002400     03 MOD-IDDC-IN          PIC X(2).                                    
002500*                                 IDENTIFIERARE LAGER                     
002600     03 MOD-IDDC-UT          PIC X(2).                                    
002700*                                 IDENTIFIERARE LAGER                     
002800     03 MOD-DISTR-GRUPP      OCCURS 13 TIMES.                             
002900        05 MOD-KDSVAR-ATTR   PIC X(2).                                    
003000*                                 MFS ATTRIBUTFÄLT                        
003100        05 MOD-KDSVAR        PIC X.                                       
003200*                                 SVAR FRÅN PROGRAM ELLER SKÄRM           
003300        05 MOD-IDDISTR       PIC Z(3)9.                                   
003400*                                 DISTRIKTNUMMER                          
003500        05 MOD-IDKUNDNR      PIC Z(6).                                    
003600*                                 KUNDNUMMER                              
003700        05 MOD-IDFAKT        PIC Z(6)9.                                   
003800*                                 FAKTURANUMMER                           
003900        05 MOD-IDBYTRAP      PIC Z(6)9.                                   
004000*                                 RAPPORTNUMMER  BYTES                    
004100        05 MOD-KVRETUR-TOT   PIC -(6)9.                                   
004200*                                 ANTAL RETURER TOTALT                    
004300        05 MOD-TIREGDAT      PIC 9(6).                                    
004400*                                 REGISTRERINGSDATUM (ÅÅMMDD)             
004500        05 MOD-KDBYTSTA-ATTR PIC X(2).                                    
004600*                                 MFS ATTRIBUTFÄLT                        
004700        05 MOD-KDBYTSTA      PIC X.                                       
004800*                                 STATUSKOD BYTESOBJEKT                   
004900        05 MOD-FLBYTGAR      PIC X.                                       
005000*                                 GARANTI RAPPORT FLAGGA                  
005100*                                 Y = GARANTI                             
005200*                                 N = EJ GARANTI                          
005300        05 MOD-ADBYTANK-ATTR PIC X(2).                                    
005400*                                 MFS ATTRIBUTFÄLT                        
005500        05 MOD-ADBYTANK      PIC X(10).                                   
005600*                                 ANKOMSTADRESS BYTESOBJEKT               
005700        05 MOD-TIANKDAG      PIC 9(6).                                    
005800*                                 ANKOMSTDAG                              
005900        05 MOD-TIREGDAT-GODK PIC 9(6).                                    
006000*                                 REGISTRERINGSDATUM (ÅÅMMDD)             
006100     03 MOD-TEMFSINF         PIC X(55).                                   
006200*                                 INFORMATIONSMEDDELANDE                  
006300*** END OF VILMAII-COPY LENGTH= 1023 BYTES                                
