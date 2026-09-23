000100 01  MOD-W6O20101.                                                        
000200*                                 COPYTEXT F÷R MOD W6020101               
000300*                                                                         
000400     03 MOD-IDTRANS          PIC X(4).                                    
000500*                                 BILDNUMMER                              
000600*                                 SCREEN NUMBER                           
000700     03 MOD-TEMFSFEL         PIC X(40).                                   
000800*                                 MFS FELMEDDELANDE                       
000900*                                 MFS ERROR MESSAGE                       
001000     03 MOD-KDKRSTA-IN       PIC X.                                       
001100*                                 KONTROLLRAPPORT STATUS                  
001200*                                 INSPECTION REPORT STATUS                
001300     03 MOD-KDKRSTA-UT       PIC X.                                       
001400*                                 KONTROLLRAPPORT STATUS                  
001500*                                 INSPECTION REPORT STATUS                
001600     03 MOD-KDPERSTYP-IN     PIC X.                                       
001700     03 MOD-KDPERSTYP-UT     PIC X.                                       
001800     03 MOD-IDPERSON-IN      PIC X(3).                                    
001900*                                 PERSONKOD                               
002000*                                 STAFF CODE                              
002100     03 MOD-IDPERSON-UT      PIC X(3).                                    
002200*                                 PERSONKOD                               
002300*                                 STAFF CODE                              
002400     03 MOD-KDBEHX-IN        PIC X.                                       
002500*                                 BEHANDLINGSKOD-X                        
002600     03 MOD-KDBEHX-UT        PIC X.                                       
002700*                                 BEHANDLINGSKOD-X                        
002800     03 MOD-IDTYP-IN         PIC X.                                       
002900     03 MOD-IDTYP-UT         PIC X.                                       
003000     03 MOD-IDARTNR-IN       PIC X(9).                                    
003100*                                 ARTIKELNUMMER                           
003200*                                 PART NUMBER                             
003300     03 MOD-IDARTNR-UT       PIC X(9).                                    
003400*                                 ARTIKELNUMMER                           
003500*                                 PART NUMBER                             
003600     03 MOD-FLANNULL-IN      PIC X.                                       
003700*                                 ANNULLATION                             
003800*                                 CANCELLATION                            
003900     03 MOD-FLANNULL-UT      PIC X.                                       
004000*                                 ANNULLATION                             
004100*                                 CANCELLATION                            
004200     03 MOD-IDDC-IN          PIC X(2).                                    
004300*                                 IDENTIFIERARE LAGER                     
004400*                                 WAREHOUSE IDENTIFIER                    
004500     03 MOD-IDDC-UT          PIC X(2).                                    
004600*                                 IDENTIFIERARE LAGER                     
004700*                                 WAREHOUSE IDENTIFIER                    
004800     03 MOD-IDDC-ENTER       PIC X(2).                                    
004900*                                 IDENTIFIERARE LAGER                     
005000*                                 WAREHOUSE IDENTIFIER                    
005100     03 MOD-IDDC-NEXT        PIC X(2).                                    
005200*                                 IDENTIFIERARE LAGER                     
005300*                                 WAREHOUSE IDENTIFIER                    
005400     03 MOD-TIREGDAT-ENTER   PIC 9(6).                                    
005500*                                 REGISTRERINGSDATUM (≈≈MMDD)             
005600*                                 REGISTRATION DATE (YYMMDD)              
005700     03 MOD-TIREGDAT-NEXT    PIC 9(6).                                    
005800*                                 REGISTRERINGSDATUM (≈≈MMDD)             
005900*                                 REGISTRATION DATE (YYMMDD)              
006000     03 MOD-KDKRSTA-ENTER    PIC X.                                       
006100*                                 KONTROLLRAPPORT STATUS                  
006200*                                 INSPECTION REPORT STATUS                
006300     03 MOD-KDKRSTA-NEXT     PIC X.                                       
006400*                                 KONTROLLRAPPORT STATUS                  
006500*                                 INSPECTION REPORT STATUS                
006600     03 MOD-IDLOPNRM-ENTER   PIC X(9).                                    
006700*                                 L÷PNUMMER MOTTAGNINGSRAPPORT            
006800*                                 (0VVDLLLLK)                             
006900*                                 SERIAL NO RECEIVING REPORT              
007000*                                 (0WWDLLLLC)                             
007100     03 MOD-IDLOPNRM-NEXT    PIC X(9).                                    
007200*                                 L÷PNUMMER MOTTAGNINGSRAPPORT            
007300*                                 (0VVDLLLLK)                             
007400*                                 SERIAL NO RECEIVING REPORT              
007500*                                 (0WWDLLLLC)                             
007600     03 MOD-IDFTG-ENTER      PIC 9(2).                                    
007700*                                 F÷RETAGSID EKONOM REDOVISNING           
007800*                                 COMPANY IDENTITY ACCOUNTING             
007900     03 MOD-IDFTG-NEXT       PIC 9(2).                                    
008000*                                 F÷RETAGSID EKONOM REDOVISNING           
008100*                                 COMPANY IDENTITY ACCOUNTING             
008200     03 MOD-IDKR-ENTER       PIC 9(5).                                    
008300*                                 KONTROLLRAPPORT NUMMER                  
008400*                                 INSPECTION REPORT NUMBER                
008500     03 MOD-IDKR-NEXT        PIC 9(5).                                    
008600*                                 KONTROLLRAPPORT NUMMER                  
008700*                                 INSPECTION REPORT NUMBER                
008800     03 MOD-UPDATE           OCCURS 13 TIMES.                             
008900*                                 TABELL-UPDATE                           
009000        05 MOD-KDBEHX-ATTR   PIC X(2).                                    
009100*                                 MFS ATTRIBUTFƒLT                        
009200        05 MOD-KDBEHX-UPDATE PIC X.                                       
009300*                                 BEHANDLINGSKOD-X                        
009400     03 MOD-IDARTNR-RAD      OCCURS 13 TIMES                              
009500                             PIC Z(8)9.                                   
009600*                                 ARTIKELNUMMER                           
009700*                                 PART NUMBER                             
009800     03 MOD-BEART-RAD        OCCURS 13 TIMES                              
009900                             PIC X(14).                                   
010000     03 MOD-TIREGDAT-RAD     OCCURS 13 TIMES                              
010100                             PIC 9(6).                                    
010200*                                 REGISTRERINGSDATUM (≈≈MMDD)             
010300*                                 REGISTRATION DATE (YYMMDD)              
010400     03 MOD-IDKR-RAD         OCCURS 13 TIMES                              
010500                             PIC 9(5).                                    
010600*                                 KONTROLLRAPPORT NUMMER                  
010700*                                 INSPECTION REPORT NUMBER                
010800     03 MOD-IDLOPNRM-RAD     OCCURS 13 TIMES                              
010900                             PIC Z(8)9.                                   
011000*                                 L÷PNUMMER MOTTAGNINGSRAPPORT            
011100*                                 (0VVDLLLLK)                             
011200*                                 SERIAL NO RECEIVING REPORT              
011300*                                 (0WWDLLLLC)                             
011400     03 MOD-IDLEVNR-RAD      OCCURS 13 TIMES                              
011500                             PIC X(5).                                    
011600*                                 LEVERANT÷RNUMMER                        
011700*                                 SUPPLIER NUMBER (VENDOR NUMBER)         
011800     03 MOD-TYP-RAD          OCCURS 13 TIMES                              
011900                             PIC X.                                       
012000     03 MOD-KVART-RET-RAD    OCCURS 13 TIMES                              
012100                             PIC Z(6)9.                                   
012200*                                 ANTAL ARTNR PER BRYTBEGREPP             
012300*                                 NO OF PARTNOS PER TYPE                  
012400     03 MOD-BEKRBEH-RAD      OCCURS 13 TIMES                              
012500                             PIC X(10).                                   
012600     03 MOD-IDARTNR-ENTER    PIC 9(9).                                    
012700*                                 ARTIKELNUMMER                           
012800*                                 PART NUMBER                             
012900     03 MOD-IDARTNR-NEXT     PIC 9(9).                                    
013000*                                 ARTIKELNUMMER                           
013100*                                 PART NUMBER                             
013200     03 MOD-DAREGDAT-9KOMPL-ENTER                                         
013300                             PIC 9(8).                                    
013400*                                 DATUMETS 9-KOMPLEMENT                   
013500*                                 DATES 9-COMPLEMENT                      
013600     03 MOD-DAREGDAT-9KOMPL-NEXT                                          
013700                             PIC 9(8).                                    
013800*                                 DATUMETS 9-KOMPLEMENT                   
013900*                                 DATES 9-COMPLEMENT                      
014000     03 MOD-IDLEVNR-ENTER    PIC X(5).                                    
014100*                                 LEVERANT÷RNUMMER                        
014200*                                 SUPPLIER NUMBER (VENDOR NUMBER)         
014300     03 MOD-IDLEVNR-NEXT     PIC X(5).                                    
014400*                                 LEVERANT÷RNUMMER                        
014500*                                 SUPPLIER NUMBER (VENDOR NUMBER)         
014600     03 MOD-KVKRKNTR-ENTER   PIC 9.                                       
014700*                                 REKNEVERK ANTAL/KVALITET AVV            
014800*                                 COUNTER QUANTITY/QUALITY DEV            
014900     03 MOD-KVKRKNTR-NEXT    PIC 9.                                       
015000*                                 REKNEVERK ANTAL/KVALITET AVV            
015100*                                 COUNTER QUANTITY/QUALITY DEV            
015200     03 MOD-IDKR-SPAR        OCCURS 13 TIMES                              
015300                             PIC 9(5).                                    
015400*                                 KONTROLLRAPPORT NUMMER                  
015500*                                 INSPECTION REPORT NUMBER                
015600     03 MOD-TEMFSINF         PIC X(55).                                   
015700*                                 INFORMATIONSMEDDELANDE                  
015800*                                 INFORMATION MESSAGE                     
015900*** END OF VILMAII-COPY LENGTH= 1195 BYTES                                
