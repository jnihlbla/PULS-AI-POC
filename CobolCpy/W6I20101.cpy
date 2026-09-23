000100 01  MID-W6I20101.                                                        
000200*                                                                         
000300     03 MID-KDKRSTA-IN       PIC X.                                       
000400*                                 KONTROLLRAPPORT STATUS                  
000500*                                 INSPECTION REPORT STATUS                
000600     03 MID-KDKRSTA-UT       PIC X.                                       
000700*                                 KONTROLLRAPPORT STATUS                  
000800*                                 INSPECTION REPORT STATUS                
000900     03 MID-KDPERSTYP-IN     PIC X.                                       
001000     03 MID-KDPERSTYP-UT     PIC X.                                       
001100     03 MID-IDPERSON-IN      PIC X(3).                                    
001200*                                 PERSONKOD                               
001300*                                 STAFF CODE                              
001400     03 MID-IDPERSON-UT      PIC X(3).                                    
001500*                                 PERSONKOD                               
001600*                                 STAFF CODE                              
001700     03 MID-KDBEHX-IN        PIC X.                                       
001800*                                 BEHANDLINGSKOD-X                        
001900     03 MID-KDBEHX-UT        PIC X.                                       
002000*                                 BEHANDLINGSKOD-X                        
002100     03 MID-IDTYP-IN         PIC X.                                       
002200     03 MID-IDTYP-UT         PIC X.                                       
002300     03 MID-IDARTNR-IN       PIC X(9).                                    
002400*                                 ARTIKELNUMMER                           
002500*                                 PART NUMBER                             
002600     03 MID-IDARTNR-UT       PIC X(9).                                    
002700*                                 ARTIKELNUMMER                           
002800*                                 PART NUMBER                             
002900     03 MID-FLANNULL-IN      PIC X.                                       
003000*                                 ANNULLATION                             
003100*                                 CANCELLATION                            
003200     03 MID-FLANNULL-UT      PIC X.                                       
003300*                                 ANNULLATION                             
003400*                                 CANCELLATION                            
003500     03 MID-IDDC-IN          PIC X(2).                                    
003600*                                 IDENTIFIERARE LAGER                     
003700*                                 WAREHOUSE IDENTIFIER                    
003800     03 MID-IDDC-UT          PIC X(2).                                    
003900*                                 IDENTIFIERARE LAGER                     
004000*                                 WAREHOUSE IDENTIFIER                    
004100     03 MID-IDDC-ENTER       PIC X(2).                                    
004200*                                 IDENTIFIERARE LAGER                     
004300*                                 WAREHOUSE IDENTIFIER                    
004400     03 MID-IDDC-NEXT        PIC X(2).                                    
004500*                                 IDENTIFIERARE LAGER                     
004600*                                 WAREHOUSE IDENTIFIER                    
004700     03 MID-TIREGDAT-ENTER   PIC 9(6).                                    
004800*                                 REGISTRERINGSDATUM (≈≈MMDD)             
004900*                                 REGISTRATION DATE (YYMMDD)              
005000     03 MID-TIREGDAT-NEXT    PIC 9(6).                                    
005100*                                 REGISTRERINGSDATUM (≈≈MMDD)             
005200*                                 REGISTRATION DATE (YYMMDD)              
005300     03 MID-KDKRSTA-ENTER    PIC X.                                       
005400*                                 KONTROLLRAPPORT STATUS                  
005500*                                 INSPECTION REPORT STATUS                
005600     03 MID-KDKRSTA-NEXT     PIC X.                                       
005700*                                 KONTROLLRAPPORT STATUS                  
005800*                                 INSPECTION REPORT STATUS                
005900     03 MID-IDLOPNRM-ENTER   PIC X(9).                                    
006000*                                 L÷PNUMMER MOTTAGNINGSRAPPORT            
006100*                                 (0VVDLLLLK)                             
006200*                                 SERIAL NO RECEIVING REPORT              
006300*                                 (0WWDLLLLC)                             
006400     03 MID-IDLOPNRM-NEXT    PIC X(9).                                    
006500*                                 L÷PNUMMER MOTTAGNINGSRAPPORT            
006600*                                 (0VVDLLLLK)                             
006700*                                 SERIAL NO RECEIVING REPORT              
006800*                                 (0WWDLLLLC)                             
006900     03 MID-IDFTG-ENTER      PIC 9(2).                                    
007000*                                 F÷RETAGSID EKONOM REDOVISNING           
007100*                                 COMPANY IDENTITY ACCOUNTING             
007200     03 MID-IDFTG-NEXT       PIC 9(2).                                    
007300*                                 F÷RETAGSID EKONOM REDOVISNING           
007400*                                 COMPANY IDENTITY ACCOUNTING             
007500     03 MID-IDKR-ENTER       PIC 9(5).                                    
007600*                                 KONTROLLRAPPORT NUMMER                  
007700*                                 INSPECTION REPORT NUMBER                
007800     03 MID-IDKR-NEXT        PIC 9(5).                                    
007900*                                 KONTROLLRAPPORT NUMMER                  
008000*                                 INSPECTION REPORT NUMBER                
008100     03 MID-IDARTNR-ENTER    PIC 9(9).                                    
008200*                                 ARTIKELNUMMER                           
008300*                                 PART NUMBER                             
008400     03 MID-IDARTNR-NEXT     PIC 9(9).                                    
008500*                                 ARTIKELNUMMER                           
008600*                                 PART NUMBER                             
008700     03 MID-DAREGDAT-9KOMPL-ENTER                                         
008800                             PIC 9(8).                                    
008900*                                 DATUMETS 9-KOMPLEMENT                   
009000*                                 DATES 9-COMPLEMENT                      
009100     03 MID-DAREGDAT-9KOMPL-NEXT                                          
009200                             PIC 9(8).                                    
009300*                                 DATUMETS 9-KOMPLEMENT                   
009400*                                 DATES 9-COMPLEMENT                      
009500     03 MID-IDLEVNR-ENTER    PIC X(5).                                    
009600*                                 LEVERANT÷RNUMMER                        
009700*                                 SUPPLIER NUMBER (VENDOR NUMBER)         
009800     03 MID-IDLEVNR-NEXT     PIC X(5).                                    
009900*                                 LEVERANT÷RNUMMER                        
010000*                                 SUPPLIER NUMBER (VENDOR NUMBER)         
010100     03 MID-KVKRKNTR-ENTER   PIC 9.                                       
010200*                                 REKNEVERK ANTAL/KVALITET AVV            
010300*                                 COUNTER QUANTITY/QUALITY DEV            
010400     03 MID-KVKRKNTR-NEXT    PIC 9.                                       
010500*                                 REKNEVERK ANTAL/KVALITET AVV            
010600*                                 COUNTER QUANTITY/QUALITY DEV            
010700     03 MID-IDKR-SPAR        OCCURS 13 TIMES                              
010800                             PIC 9(5).                                    
010900*                                 KONTROLLRAPPORT NUMMER                  
011000*                                 INSPECTION REPORT NUMBER                
011100     03 MID-INPUT.                                                        
011200*                                 INDATA F÷R UPPDATERING                  
011300        05 MID-KDBEHX-UPDATE OCCURS 13 TIMES                              
011400                             PIC X.                                       
011500*                                 BEHANDLINGSKOD-X                        
011600*** END OF VILMAII-COPY LENGTH= 212 BYTES                                 
