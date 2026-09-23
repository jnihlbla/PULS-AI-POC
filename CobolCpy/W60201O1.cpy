000100 01  RESP-W60201O1.                                                       
000200*                                 COPYTEXT F÷R RESP W6020101              
000300*                                                                         
000400     03 RESP-IDDC-START      PIC X(2).                                    
000500*                                 IDENTIFIERARE LAGER                     
000600*                                 WAREHOUSE IDENTIFIER                    
000700     03 RESP-IDDC-NEXT       PIC X(2).                                    
000800*                                 IDENTIFIERARE LAGER                     
000900*                                 WAREHOUSE IDENTIFIER                    
001000     03 RESP-TIREGDAT-START  PIC 9(6).                                    
001100*                                 REGISTRERINGSDATUM (≈≈MMDD)             
001200*                                 REGISTRATION DATE (YYMMDD)              
001300     03 RESP-TIREGDAT-NEXT   PIC 9(6).                                    
001400*                                 REGISTRERINGSDATUM (≈≈MMDD)             
001500*                                 REGISTRATION DATE (YYMMDD)              
001600     03 RESP-KDKRSTA-START   PIC X.                                       
001700*                                 KONTROLLRAPPORT STATUS                  
001800*                                 INSPECTION REPORT STATUS                
001900     03 RESP-KDKRSTA-NEXT    PIC X.                                       
002000*                                 KONTROLLRAPPORT STATUS                  
002100*                                 INSPECTION REPORT STATUS                
002200     03 RESP-IDLOPNRM-START  PIC X(9).                                    
002300*                                 L÷PNUMMER MOTTAGNINGSRAPPORT            
002400*                                 (0VVDLLLLK)                             
002500*                                 SERIAL NO RECEIVING REPORT              
002600*                                 (0WWDLLLLC)                             
002700     03 RESP-IDLOPNRM-NEXT   PIC X(9).                                    
002800*                                 L÷PNUMMER MOTTAGNINGSRAPPORT            
002900*                                 (0VVDLLLLK)                             
003000*                                 SERIAL NO RECEIVING REPORT              
003100*                                 (0WWDLLLLC)                             
003200     03 RESP-IDFTG-START     PIC 9(2).                                    
003300*                                 F÷RETAGSID EKONOM REDOVISNING           
003400*                                 COMPANY IDENTITY ACCOUNTING             
003500     03 RESP-IDFTG-NEXT      PIC 9(2).                                    
003600*                                 F÷RETAGSID EKONOM REDOVISNING           
003700*                                 COMPANY IDENTITY ACCOUNTING             
003800     03 RESP-IDKR-START      PIC 9(5).                                    
003900*                                 KONTROLLRAPPORT NUMMER                  
004000*                                 INSPECTION REPORT NUMBER                
004100     03 RESP-IDKR-NEXT       PIC 9(5).                                    
004200*                                 KONTROLLRAPPORT NUMMER                  
004300*                                 INSPECTION REPORT NUMBER                
004400     03 RESP-IDARTNR-START   PIC 9(9).                                    
004500*                                 ARTIKELNUMMER                           
004600*                                 PART NUMBER                             
004700     03 RESP-IDARTNR-NEXT    PIC 9(9).                                    
004800*                                 ARTIKELNUMMER                           
004900*                                 PART NUMBER                             
005000     03 RESP-DAREGDAT-9KOMPL-START                                        
005100                             PIC 9(8).                                    
005200*                                 DATUMETS 9-KOMPLEMENT                   
005300*                                 DATES 9-COMPLEMENT                      
005400     03 RESP-DAREGDAT-9KOMPL-NEXT                                         
005500                             PIC 9(8).                                    
005600*                                 DATUMETS 9-KOMPLEMENT                   
005700*                                 DATES 9-COMPLEMENT                      
005800     03 RESP-IDLEVNR-START   PIC X(5).                                    
005900*                                 LEVERANT÷RNUMMER                        
006000*                                 SUPPLIER NUMBER (VENDOR NUMBER)         
006100     03 RESP-IDLEVNR-NEXT    PIC X(5).                                    
006200*                                 LEVERANT÷RNUMMER                        
006300*                                 SUPPLIER NUMBER (VENDOR NUMBER)         
006400     03 RESP-KVKRKNTR-START  PIC 9.                                       
006500*                                 REKNEVERK ANTAL/KVALITET AVV            
006600*                                 COUNTER QUANTITY/QUALITY DEV            
006700     03 RESP-KVKRKNTR-NEXT   PIC 9.                                       
006800*                                 REKNEVERK ANTAL/KVALITET AVV            
006900*                                 COUNTER QUANTITY/QUALITY DEV            
007000     03 RESP-KVRADER         PIC 9(5).                                    
007100*                                 ANTAL RADER                             
007200*                                 NUMBER OF LINES                         
007300     03 RESP-LINE            OCCURS 500 TIMES.                            
007400*                                 TABELL-UPDATE                           
007500        05 RESP-KDBEHX-LINE-ATTR                                          
007600                             PIC X(2).                                    
007700*                                 MFS ATTRIBUTFƒLT                        
007800        05 RESP-KDBEHX-UPDATE-LINE                                        
007900                             PIC X.                                       
008000*                                 BEHANDLINGSKOD-X                        
008100        05 RESP-IDARTNR-LINE PIC Z(8)9.                                   
008200*                                 ARTIKELNUMMER                           
008300*                                 PART NUMBER                             
008400        05 RESP-BEART-LINE   PIC X(14).                                   
008500        05 RESP-TIREGDAT-LINE                                             
008600                             PIC 9(6).                                    
008700*                                 REGISTRERINGSDATUM (≈≈MMDD)             
008800*                                 REGISTRATION DATE (YYMMDD)              
008900        05 RESP-IDKR-LINE    PIC 9(5).                                    
009000*                                 KONTROLLRAPPORT NUMMER                  
009100*                                 INSPECTION REPORT NUMBER                
009200        05 RESP-IDLOPNRM-LINE                                             
009300                             PIC Z(8)9.                                   
009400*                                 L÷PNUMMER MOTTAGNINGSRAPPORT            
009500*                                 (0VVDLLLLK)                             
009600*                                 SERIAL NO RECEIVING REPORT              
009700*                                 (0WWDLLLLC)                             
009800        05 RESP-IDLEVNR-LINE PIC X(5).                                    
009900*                                 LEVERANT÷RNUMMER                        
010000*                                 SUPPLIER NUMBER (VENDOR NUMBER)         
010100        05 RESP-TYP-LINE     PIC X.                                       
010200        05 RESP-KVART-RET-LINE                                            
010300                             PIC Z(6)9.                                   
010400*                                 ANTAL ARTNR PER BRYTBEGREPP             
010500*                                 NO OF PARTNOS PER TYPE                  
010600        05 RESP-BEKRBEH-LINE PIC X(10).                                   
010700*** END OF VILMAII-COPY LENGTH= 34601 BYTES                               
