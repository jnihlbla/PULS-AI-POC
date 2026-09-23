000100 01  REQU-W60114I1.                                                       
000200*                                 REQU-COPYTEXT FÖR W60114                
000300*                                                                         
000400     03 REQU-IDLEVNR-KEY     PIC X(5).                                    
000500*                                 LEVERANTÖRNUMMER                        
000600     03 REQU-IDFS-KEY        PIC X(8).                                    
000700*                                 FÖLJESEDELSNUMMER ENL ODETTE            
000800     03 REQU-TIAVIDAT-KEY    PIC X(6).                                    
000900*                                 AVISERINGSDATUM (YYMMDD)                
001000     03 REQU-ADINLOMR-PRT-KEY                                             
001100                             PIC X(4).                                    
001200*                                 PRINTERPLACERING                        
001300     03 REQU-IDDC-KEY        PIC X(2).                                    
001400*                                 IDENTIFIERARE LAGER                     
001500     03 REQU-KDRT-KEY        PIC X(2).                                    
001600*                                 REDOVISNINGSTYP                         
001700     03 REQU-IDLBBET-KEY     PIC X(12).                                   
001800*                                 LASTBÄRARBETECKNING                     
001900     03 REQU-FLKLIVIS-KEY    PIC X.                                       
002000*                                 JA/NEJ-FLAGGA                           
002100     03 REQU-IDRADNR-START   PIC 9(4).                                    
002200*                                 RADNUMMER                               
002300     03 REQU-IDRADNR-INL-START                                            
002400                             PIC 9(5).                                    
002500*                                 RADNUMMER INLEVERANS                    
002600     03 REQU-KVRADER         PIC 9(5).                                    
002700*                                 ANTAL RADER                             
002800     03 REQU-INPUT.                                                       
002900*                                 INDATA FÖR UPPDATERING                  
003000        05 REQU-TIAVIDAT-UPD PIC X(6).                                    
003100*                                 AVISERINGSDATUM (YYMMDD)                
003200        05 REQU-IDLBBET-UPD  PIC X(12).                                   
003300*                                 LASTBÄRARBETECKNING                     
003400        05 REQU-TIANKDAG-UPD PIC X(6).                                    
003500*                                 ANKOMSTDAG                              
003600        05 REQU-IDLEVNR-UPD  PIC X(5).                                    
003700*                                 LEVERANTÖRNUMMER                        
003800        05 REQU-FLBORT-UPD   PIC X.                                       
003900*                                 BORTTAGNINGSFLAGGA                      
004000        05 REQU-IDFTG-UPD    PIC X(2).                                    
004100*                                 FÖRETAGSID EKONOM REDOVISNING           
004200        05 REQU-IDKONTO-UPD  PIC X(10).                                   
004300*                                 KONTO                                   
004400        05 REQU-IDANALYS-UPD PIC X(12).                                   
004500*                                 ANALYSNUMMER                            
004600        05 REQU-IDKST-UPD    PIC X(10).                                   
004700*                                 KOSTNADSSTÄLLE                          
004800        05 REQU-IDARTNR-UPD  PIC X(8).                                    
004900*                                 ARTIKELNUMMER                           
005000        05 REQU-KVAVIS-UPD   PIC X(6).                                    
005100*                                 AVISERAT ANTAL                          
005200        05 REQU-KDRT-UPD     PIC X(2).                                    
005300*                                 REDOVISNINGSTYP                         
005400        05 REQU-INPUT-LINE   OCCURS 500 TIMES.                            
005500*                                 INDATA FÖR UPPDATERING                  
005600           07 REQU-KDCMD-LINE                                             
005700                             PIC X.                                       
005800            88 REQU-KDCMD-INGENTING                                       
005900                             VALUE ' '.                                   
006000            88 REQU-KDCMD-DELETE                                          
006100                             VALUE 'D'                                    
006200                             'B'.                                         
006300            88 REQU-KDCMD-REPLACE                                         
006400                             VALUE 'R'                                    
006500                             'Ä'.                                         
006600            88 REQU-KDCMD-INSERT                                          
006700                             VALUE 'I'                                    
006800                             'N'                                          
006900                             'A'.                                         
007000            88 REQU-KDCMD-SELECT                                          
007100                             VALUE 'S'                                    
007200                             'V'.                                         
007300            88 REQU-KDCMD-PRINT                                           
007400                             VALUE 'P'                                    
007500                             'P'.                                         
007600            88 REQU-KDCMD-COPY                                            
007700                             VALUE 'C'                                    
007800                             'K'.                                         
007900*                                 RAD-UPPDATERINGSKOMMANDO                
008000*                                  BLANK  = INGENTING                     
008100*                                  D , B  = DELETE                        
008200*                                  R , Ä  = REPLACE                       
008300*                                  I,N,A  = INSERT                        
008400*                                  S , V  = SELECT                        
008500*                                  P , P  = PRINT                         
008600*                                  C , K  = COPY                          
008700           07 REQU-IDARTNR-LINE                                           
008800                             PIC X(8).                                    
008900*                                 ARTIKELNUMMER                           
009000           07 REQU-KVAVIS-LINE                                            
009100                             PIC X(6).                                    
009200*                                 AVISERAT ANTAL                          
009300           07 REQU-KDRT-LINE PIC X(2).                                    
009400*                                 REDOVISNINGSTYP                         
009500     03 REQU-INPUT-SPAR      OCCURS 500 TIMES.                            
009600*                                 INDATA FÖR UPPDATERING                  
009700        05 REQU-IDARTNR-SPAR-LINE                                         
009800                             PIC 9(8).                                    
009900*                                 ARTIKELNUMMER                           
010000        05 REQU-IDRADNR-SPAR-LINE                                         
010100                             PIC 9(3).                                    
010200*                                 RADNUMMER                               
010300        05 REQU-IDRADNR-INL-SPAR-LINE                                     
010400                             PIC 9(5).                                    
010500*                                 RADNUMMER INLEVERANS                    
010600*** END OF VILMAII-COPY LENGTH= 16634 BYTES                               
