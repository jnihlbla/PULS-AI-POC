000100 01  RESP-W60114O1.                                                       
000200*                                 RESPCOPYTEXT TILL W60114.               
000300*                                                                         
000400     03 RESP-IDRADNR-START   PIC 9(3).                                    
000500*                                 RADNUMMER                               
000600*                                 LINE NO                                 
000700     03 RESP-IDRADNR-INL-START                                            
000800                             PIC 9(5).                                    
000900*                                 RADNUMMER INLEVERANS                    
001000*                                 LINE NUMBER GOODS RECEIVING             
001100     03 RESP-IDRADNR-NEXT    PIC 9(3).                                    
001200*                                 RADNUMMER                               
001300*                                 LINE NO                                 
001400     03 RESP-IDRADNR-INL-NEXT                                             
001500                             PIC 9(5).                                    
001600*                                 RADNUMMER INLEVERANS                    
001700*                                 LINE NUMBER GOODS RECEIVING             
001800     03 RESP-OUTPUT.                                                      
001900*                                 UPDATE                                  
002000        05 RESP-TIAVIDAT     PIC 9(6).                                    
002100*                                 AVISERINGSDATUM (YYMMDD)                
002200*                                 ADVICE NOTE DATE                        
002300        05 RESP-IDLBBET      PIC X(12).                                   
002400*                                 LASTBÄRARBETECKNING                     
002500*                                 TRAILER NUMBER                          
002600        05 RESP-TIANKDAG     PIC 9(6).                                    
002700*                                 ANKOMSTDAG                              
002800*                                 RECEIVING DATE                          
002900        05 RESP-IDLEVNR      PIC X(5).                                    
003000*                                 LEVERANTÖRNUMMER                        
003100*                                 SUPPLIER NUMBER (VENDOR NUMBER)         
003200        05 RESP-FLBORT       PIC X.                                       
003300*                                 BORTTAGNINGSFLAGGA                      
003400        05 RESP-TIAVIDAT-UPD PIC 9(6).                                    
003500*                                 AVISERINGSDATUM (YYMMDD)                
003600*                                 ADVICE NOTE DATE                        
003700        05 RESP-TIAVIDAT-UPD-ATTR                                         
003800                             PIC X(2).                                    
003900*                                 MFS ATTRIBUTFÄLT                        
004000        05 RESP-IDLBBET-UPD  PIC X(12).                                   
004100*                                 LASTBÄRARBETECKNING                     
004200*                                 TRAILER NUMBER                          
004300        05 RESP-IDLBBET-UPD-ATTR                                          
004400                             PIC X(2).                                    
004500*                                 MFS ATTRIBUTFÄLT                        
004600        05 RESP-TIANKDAG-UPD PIC 9(6).                                    
004700*                                 ANKOMSTDAG                              
004800*                                 RECEIVING DATE                          
004900        05 RESP-TIANKDAG-UPD-ATTR                                         
005000                             PIC X(2).                                    
005100*                                 MFS ATTRIBUTFÄLT                        
005200        05 RESP-IDLEVNR-UPD  PIC X(5).                                    
005300*                                 LEVERANTÖRNUMMER                        
005400*                                 SUPPLIER NUMBER (VENDOR NUMBER)         
005500        05 RESP-IDLEVNR-UPD-ATTR                                          
005600                             PIC X(2).                                    
005700*                                 MFS ATTRIBUTFÄLT                        
005800        05 RESP-FLBORT-UPD   PIC X.                                       
005900*                                 BORTTAGNINGSFLAGGA                      
006000        05 RESP-FLBORT-UPD-ATTR                                           
006100                             PIC X(2).                                    
006200*                                 MFS ATTRIBUTFÄLT                        
006300        05 RESP-IDFTG        PIC X(2).                                    
006400*                                 FÖRETAGSID EKONOM REDOVISNING           
006500*                                 COMPANY IDENTITY ACCOUNTING             
006600        05 RESP-IDKONTO      PIC Z(9)9.                                   
006700*                                 KONTO                                   
006800*                                 ACCOUNT                                 
006900        05 RESP-IDANALYS     PIC X(12).                                   
007000*                                 ANALYSNUMMER                            
007100*                                 ANALYSIS NUMBER                         
007200        05 RESP-IDKST        PIC X(10).                                   
007300*                                 KOSTNADSSTÄLLE                          
007400*                                 COST CENTRE                             
007500        05 RESP-IDFTG-UPD    PIC X(2).                                    
007600*                                 FÖRETAGSID EKONOM REDOVISNING           
007700*                                 COMPANY IDENTITY ACCOUNTING             
007800        05 RESP-IDFTG-UPD-ATTR                                            
007900                             PIC X(2).                                    
008000*                                 MFS ATTRIBUTFÄLT                        
008100        05 RESP-IDKONTO-UPD  PIC Z(9)9.                                   
008200*                                 KONTO                                   
008300*                                 ACCOUNT                                 
008400        05 RESP-IDKONTO-UPD-ATTR                                          
008500                             PIC X(2).                                    
008600*                                 MFS ATTRIBUTFÄLT                        
008700        05 RESP-IDANALYS-UPD PIC X(12).                                   
008800*                                 ANALYSNUMMER                            
008900*                                 ANALYSIS NUMBER                         
009000        05 RESP-IDANALYS-UPD-ATTR                                         
009100                             PIC X(2).                                    
009200*                                 MFS ATTRIBUTFÄLT                        
009300        05 RESP-IDKST-UPD    PIC X(10).                                   
009400*                                 KOSTNADSSTÄLLE                          
009500*                                 COST CENTRE                             
009600        05 RESP-IDKST-UPD-ATTR                                            
009700                             PIC X(2).                                    
009800*                                 MFS ATTRIBUTFÄLT                        
009900        05 RESP-IDARTNR-UPD  PIC X(8).                                    
010000*                                 ARTIKELNUMMER                           
010100*                                 PART NUMBER                             
010200        05 RESP-IDARTNR-UPD-ATTR                                          
010300                             PIC X(2).                                    
010400*                                 MFS ATTRIBUTFÄLT                        
010500        05 RESP-KVAVIS-UPD   PIC X(6).                                    
010600*                                 AVISERAT ANTAL                          
010700*                                 QUANTITY NOTIFIED                       
010800        05 RESP-KVAVIS-UPD-ATTR                                           
010900                             PIC X(2).                                    
011000*                                 MFS ATTRIBUTFÄLT                        
011100        05 RESP-KDRT-UPD     PIC X(2).                                    
011200*                                 REDOVISNINGSTYP                         
011300*                                 TYPE OF ACCOUNTING                      
011400        05 RESP-KDRT-UPD-ATTR                                             
011500                             PIC X(2).                                    
011600*                                 MFS ATTRIBUTFÄLT                        
011700        05 RESP-KVRADER      PIC 9(5).                                    
011800*                                 ANTAL RADER                             
011900*                                 NUMBER OF LINES                         
012000        05 RESP-UPDATE       OCCURS 500 TIMES.                            
012100*                                 UPDATE                                  
012200           07 RESP-KDCMD-LINE                                             
012300                             PIC X.                                       
012400*                                 RAD-UPPDATERINGSKOMMANDO                
012500*                                  BLANK  = INGENTING                     
012600*                                  D , B  = DELETE                        
012700*                                  R , Ä  = REPLACE                       
012800*                                  I,N,A  = INSERT                        
012900*                                  S , V  = SELECT                        
013000*                                  P , P  = PRINT                         
013100*                                  C , K  = COPY                          
013200*                                 LINE UPDATE COMMAND                     
013300           07 RESP-KDCMD-LINE-ATTR                                        
013400                             PIC X(2).                                    
013500*                                 MFS ATTRIBUTFÄLT                        
013600           07 RESP-IDARTNR-LINE                                           
013700                             PIC Z(7)9.                                   
013800*                                 ARTIKELNUMMER                           
013900*                                 PART NUMBER                             
014000           07 RESP-IDARTNR-LINE-ATTR                                      
014100                             PIC X(2).                                    
014200*                                 MFS ATTRIBUTFÄLT                        
014300           07 RESP-KVAVIS-LINE                                            
014400                             PIC Z(5)9.                                   
014500*                                 AVISERAT ANTAL                          
014600*                                 QUANTITY NOTIFIED                       
014700           07 RESP-KVAVIS-LINE-ATTR                                       
014800                             PIC X(2).                                    
014900*                                 MFS ATTRIBUTFÄLT                        
015000           07 RESP-KDRT-LINE PIC Z9.                                      
015100*                                 REDOVISNINGSTYP                         
015200*                                 TYPE OF ACCOUNTING                      
015300           07 RESP-KDRT-LINE-ATTR                                         
015400                             PIC X(2).                                    
015500*                                 MFS ATTRIBUTFÄLT                        
015600           07 RESP-KVKOLLI-LINE                                           
015700                             PIC X(3).                                    
015800           07 RESP-IDOKOLLI-LINE                                          
015900                             PIC Z(8)9.                                   
016000*                                 ODETTE KOLLINUMMER                      
016100*                                 ODETTE CASE NUMBER                      
016200           07 RESP-KVINLART-LINE                                          
016300                             PIC X(6).                                    
016400*                                 ANTAL I PARTIRAD                        
016500*                                 QTY/LINE IN A LOT                       
016600           07 RESP-IDARTNR-SPAR-LINE                                      
016700                             PIC Z(7)9.                                   
016800*                                 ARTIKELNUMMER                           
016900*                                 PART NUMBER                             
017000           07 RESP-IDRADNR-SPAR-LINE                                      
017100                             PIC 9(3).                                    
017200*                                 RADNUMMER                               
017300*                                 LINE NO                                 
017400           07 RESP-IDRADNR-INL-SPAR-LINE                                  
017500                             PIC 9(5).                                    
017600*                                 RADNUMMER INLEVERANS                    
017700*                                 LINE NUMBER GOODS RECEIVING             
017800*** END OF VILMAII-COPY LENGTH= 29689 BYTES                               
