000100 01  MOD-W6O11401.                                                        
000200*                                 MODCOPYTEXT TILL W60114.                
000300     03 MOD-IDTRANS          PIC X(4).                                    
000400*                                 BILDNUMMER                              
000500*                                 SCREEN NUMBER                           
000600     03 MOD-TEMFSFEL         PIC X(40).                                   
000700*                                 MFS FELMEDDELANDE                       
000800*                                 MFS ERROR MESSAGE                       
000900     03 MOD-IDLEVNR-IN       PIC X(5).                                    
001000*                                 LEVERANTÖRNUMMER                        
001100*                                 SUPPLIER NUMBER (VENDOR NUMBER)         
001200     03 MOD-IDFS-IN          PIC X(8).                                    
001300*                                 FÖLJESEDELSNUMMER ENL ODETTE            
001400*                                 ADVICE NOTE NUMBER ODETTE               
001500     03 MOD-TIAVIDAT-IN      PIC X(6).                                    
001600*                                 AVISERINGSDATUM (YYMMDD)                
001700*                                 ADVICE NOTE DATE                        
001800     03 MOD-FLKLIVIS-IN      PIC X.                                       
001900*                                 JA/NEJ-FLAGGA                           
002000     03 MOD-ADINLOMR-PRT-IN  PIC X(4).                                    
002100*                                 PRINTERPLACERING                        
002200*                                 PLACE OF A PRINTER                      
002300     03 MOD-IDDC-IN          PIC X(2).                                    
002400*                                 IDENTIFIERARE LAGER                     
002500*                                 WAREHOUSE IDENTIFIER                    
002600     03 MOD-IDLEVNR-UT       PIC X(5).                                    
002700*                                 LEVERANTÖRNUMMER                        
002800*                                 SUPPLIER NUMBER (VENDOR NUMBER)         
002900     03 MOD-IDFS-UT          PIC X(8).                                    
003000*                                 FÖLJESEDELSNUMMER ENL ODETTE            
003100*                                 ADVICE NOTE NUMBER ODETTE               
003200     03 MOD-TIAVIDAT-UT      PIC X(6).                                    
003300*                                 AVISERINGSDATUM (YYMMDD)                
003400*                                 ADVICE NOTE DATE                        
003500     03 MOD-FLKLIVIS-UT      PIC X.                                       
003600*                                 JA/NEJ-FLAGGA                           
003700     03 MOD-ADINLOMR-PRT-UT  PIC X(4).                                    
003800*                                 PRINTERPLACERING                        
003900*                                 PLACE OF A PRINTER                      
004000     03 MOD-IDDC-UT          PIC X(2).                                    
004100*                                 IDENTIFIERARE LAGER                     
004200*                                 WAREHOUSE IDENTIFIER                    
004300     03 MOD-IDRADNR-ENTER    PIC 9(3).                                    
004400*                                 RADNUMMER                               
004500*                                 LINE NO                                 
004600     03 MOD-IDRADNR-NEXT     PIC 9(3).                                    
004700*                                 RADNUMMER                               
004800*                                 LINE NO                                 
004900     03 MOD-IDRADNR-INL-ENTER                                             
005000                             PIC 9(5).                                    
005100*                                 RADNUMMER INLEVERANS                    
005200*                                 LINE NUMBER GOODS RECEIVING             
005300     03 MOD-IDRADNR-INL-NEXT PIC 9(5).                                    
005400*                                 RADNUMMER INLEVERANS                    
005500*                                 LINE NUMBER GOODS RECEIVING             
005600     03 MOD-TIAVIDAT         PIC 9(6).                                    
005700*                                 AVISERINGSDATUM (YYMMDD)                
005800*                                 ADVICE NOTE DATE                        
005900     03 MOD-IDLBBET          PIC X(12).                                   
006000*                                 LASTBÄRARBETECKNING                     
006100*                                 TRAILER NUMBER                          
006200     03 MOD-TIANKDAG         PIC 9(6).                                    
006300*                                 ANKOMSTDAG                              
006400*                                 RECEIVING DATE                          
006500     03 MOD-IDLEVNR          PIC X(5).                                    
006600*                                 LEVERANTÖRNUMMER                        
006700*                                 SUPPLIER NUMBER (VENDOR NUMBER)         
006800     03 MOD-FLBORT           PIC X.                                       
006900*                                 BORTTAGNINGSFLAGGA                      
007000     03 MOD-TIAVIDAT-UPD-ATTR                                             
007100                             PIC X(2).                                    
007200*                                 MFS ATTRIBUTFÄLT                        
007300     03 MOD-TIAVIDAT-UPD     PIC X(6).                                    
007400*                                 AVISERINGSDATUM (YYMMDD)                
007500*                                 ADVICE NOTE DATE                        
007600     03 MOD-IDLBBET-UPD-ATTR PIC X(2).                                    
007700*                                 MFS ATTRIBUTFÄLT                        
007800     03 MOD-IDLBBET-UPD      PIC X(12).                                   
007900*                                 LASTBÄRARBETECKNING                     
008000*                                 TRAILER NUMBER                          
008100     03 MOD-TIANKDAG-UPD-ATTR                                             
008200                             PIC X(2).                                    
008300*                                 MFS ATTRIBUTFÄLT                        
008400     03 MOD-TIANKDAG-UPD     PIC X(6).                                    
008500*                                 ANKOMSTDAG                              
008600*                                 RECEIVING DATE                          
008700     03 MOD-IDLEVNR-UPD-ATTR PIC X(2).                                    
008800*                                 MFS ATTRIBUTFÄLT                        
008900     03 MOD-IDLEVNR-UPD      PIC X(5).                                    
009000*                                 LEVERANTÖRNUMMER                        
009100*                                 SUPPLIER NUMBER (VENDOR NUMBER)         
009200     03 MOD-FLBORT-UPD-ATTR  PIC X(2).                                    
009300*                                 MFS ATTRIBUTFÄLT                        
009400     03 MOD-FLBORT-UPD       PIC X.                                       
009500*                                 BORTTAGNINGSFLAGGA                      
009600     03 MOD-IDFTG            PIC X(2).                                    
009700*                                 FÖRETAGSID EKONOM REDOVISNING           
009800*                                 COMPANY IDENTITY ACCOUNTING             
009900     03 MOD-IDKONTO          PIC Z(9)9.                                   
010000*                                 KONTO                                   
010100*                                 ACCOUNT                                 
010200     03 MOD-IDANALYS         PIC X(12).                                   
010300*                                 ANALYSNUMMER                            
010400*                                 ANALYSIS NUMBER                         
010500     03 MOD-IDKST            PIC X(10).                                   
010600*                                 KOSTNADSSTÄLLE                          
010700*                                 COST CENTRE                             
010800     03 MOD-IDFTG-UPD-ATTR   PIC X(2).                                    
010900*                                 MFS ATTRIBUTFÄLT                        
011000     03 MOD-IDFTG-UPD        PIC X(2).                                    
011100*                                 FÖRETAGSID EKONOM REDOVISNING           
011200*                                 COMPANY IDENTITY ACCOUNTING             
011300     03 MOD-IDKONTO-UPD-ATTR PIC X(2).                                    
011400*                                 MFS ATTRIBUTFÄLT                        
011500     03 MOD-IDKONTO-UPD      PIC Z(9)9.                                   
011600*                                 KONTO                                   
011700*                                 ACCOUNT                                 
011800     03 MOD-IDANALYS-UPD-ATTR                                             
011900                             PIC X(2).                                    
012000*                                 MFS ATTRIBUTFÄLT                        
012100     03 MOD-IDANALYS-UPD     PIC X(12).                                   
012200*                                 ANALYSNUMMER                            
012300*                                 ANALYSIS NUMBER                         
012400     03 MOD-IDKST-UPD-ATTR   PIC X(2).                                    
012500*                                 MFS ATTRIBUTFÄLT                        
012600     03 MOD-IDKST-UPD        PIC X(10).                                   
012700*                                 KOSTNADSSTÄLLE                          
012800*                                 COST CENTRE                             
012900     03 MOD-UPDATE.                                                       
013000*                                 UPDATE                                  
013100        05 MOD-UPDATE        OCCURS 8 TIMES.                              
013200*                                 UPDATE                                  
013300           07 MOD-KDCMD-RAD-ATTR                                          
013400                             PIC X(2).                                    
013500*                                 MFS ATTRIBUTFÄLT                        
013600           07 MOD-KDCMD-RAD  PIC X.                                       
013700*                                 RAD-UPPDATERINGSKOMMANDO                
013800*                                  BLANK  = INGENTING                     
013900*                                  D , B  = DELETE                        
014000*                                  R , Ä  = REPLACE                       
014100*                                  I,N,A  = INSERT                        
014200*                                  S , V  = SELECT                        
014300*                                  P , P  = PRINT                         
014400*                                  C , K  = COPY                          
014500*                                 LINE UPDATE COMMAND                     
014600        05 MOD-UPDATE        OCCURS 8 TIMES.                              
014700*                                 UPDATE                                  
014800           07 MOD-IDARTNR-RAD-ATTR                                        
014900                             PIC X(2).                                    
015000*                                 MFS ATTRIBUTFÄLT                        
015100           07 MOD-IDARTNR-RAD                                             
015200                             PIC X(8).                                    
015300*                                 ARTIKELNUMMER                           
015400*                                 PART NUMBER                             
015500        05 MOD-UPDATE        OCCURS 8 TIMES.                              
015600*                                 UPDATE                                  
015700           07 MOD-KVAVIS-RAD-ATTR                                         
015800                             PIC X(2).                                    
015900*                                 MFS ATTRIBUTFÄLT                        
016000           07 MOD-KVAVIS-RAD PIC X(6).                                    
016100*                                 AVISERAT ANTAL                          
016200*                                 QUANTITY NOTIFIED                       
016300        05 MOD-UPDATE        OCCURS 8 TIMES.                              
016400*                                 UPDATE                                  
016500           07 MOD-KDRT-RAD-ATTR                                           
016600                             PIC X(2).                                    
016700*                                 MFS ATTRIBUTFÄLT                        
016800           07 MOD-KDRT-RAD   PIC X(2).                                    
016900*                                 REDOVISNINGSTYP                         
017000*                                 TYPE OF ACCOUNTING                      
017100        05 MOD-KVKOLLI-RAD   OCCURS 8 TIMES                               
017200                             PIC X(3).                                    
017300        05 MOD-IDOKOLLI-RAD  OCCURS 8 TIMES                               
017400                             PIC X(9).                                    
017500*                                 ODETTE KOLLINUMMER                      
017600*                                 ODETTE CASE NUMBER                      
017700        05 MOD-KVINLART-RAD  OCCURS 8 TIMES                               
017800                             PIC X(6).                                    
017900*                                 ANTAL I PARTIRAD                        
018000*                                 QTY/LINE IN A LOT                       
018100        05 MOD-IDARTNR-SPAR  OCCURS 8 TIMES                               
018200                             PIC Z(7)9.                                   
018300*                                 ARTIKELNUMMER                           
018400*                                 PART NUMBER                             
018500        05 MOD-IDRADNR-SPAR  OCCURS 8 TIMES                               
018600                             PIC 9(3).                                    
018700*                                 RADNUMMER                               
018800*                                 LINE NO                                 
018900        05 MOD-IDRADNR-INL-SPAR                                           
019000                             OCCURS 8 TIMES                               
019100                             PIC 9(5).                                    
019200*                                 RADNUMMER INLEVERANS                    
019300*                                 LINE NUMBER GOODS RECEIVING             
019400     03 MOD-IDARTNR-UPD-ATTR PIC X(2).                                    
019500*                                 MFS ATTRIBUTFÄLT                        
019600     03 MOD-IDARTNR-UPD      PIC X(8).                                    
019700*                                 ARTIKELNUMMER                           
019800*                                 PART NUMBER                             
019900     03 MOD-KVAVIS-UPD-ATTR  PIC X(2).                                    
020000*                                 MFS ATTRIBUTFÄLT                        
020100     03 MOD-KVAVIS-UPD       PIC X(6).                                    
020200*                                 AVISERAT ANTAL                          
020300*                                 QUANTITY NOTIFIED                       
020400     03 MOD-KDRT-UPD-ATTR    PIC X(2).                                    
020500*                                 MFS ATTRIBUTFÄLT                        
020600     03 MOD-KDRT-UPD         PIC X(2).                                    
020700*                                 REDOVISNINGSTYP                         
020800*                                 TYPE OF ACCOUNTING                      
020900     03 MOD-KDRT-IN          PIC X(2).                                    
021000*                                 REDOVISNINGSTYP                         
021100*                                 TYPE OF ACCOUNTING                      
021200     03 MOD-KDRT-UT          PIC X(2).                                    
021300*                                 REDOVISNINGSTYP                         
021400*                                 TYPE OF ACCOUNTING                      
021500     03 MOD-IDLBBET-IN       PIC X(12).                                   
021600*                                 LASTBÄRARBETECKNING                     
021700*                                 TRAILER NUMBER                          
021800     03 MOD-IDLBBET-UT       PIC X(12).                                   
021900*                                 LASTBÄRARBETECKNING                     
022000*                                 TRAILER NUMBER                          
022100     03 MOD-IDLOPNRM-IN      PIC X(8).                                    
022200*                                 LÖPNUMMER MOTTAGNINGSRAPPORT            
022300*                                 (0VVDLLLLK)                             
022400*                                 SERIAL NO RECEIVING REPORT              
022500*                                 (0WWDLLLLC)                             
022600     03 MOD-IDLOPNRM-UT      PIC X(8).                                    
022700*                                 LÖPNUMMER MOTTAGNINGSRAPPORT            
022800*                                 (0VVDLLLLK)                             
022900*                                 SERIAL NO RECEIVING REPORT              
023000*                                 (0WWDLLLLC)                             
023100     03 MOD-TEMFSINF         PIC X(55).                                   
023200*                                 INFORMATIONSMEDDELANDE                  
023300*                                 INFORMATION MESSAGE                     
023400*** END OF VILMAII-COPY LENGTH= 851 BYTES                                 
