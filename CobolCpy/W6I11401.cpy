000100 01  MID-W6I11401.                                                        
000200*                                 MID-COPYTEXT FÖR W60114                 
000300     03 MID-NYCKLAR-GRP.                                                  
000400*                                 BILDGRUPP 6111-6119                     
000500        05 MID-IDLEVNR-IN    PIC X(5).                                    
000600*                                 LEVERANTÖRNUMMER                        
000700        05 MID-IDLEVNR-UT    PIC X(5).                                    
000800*                                 LEVERANTÖRNUMMER                        
000900        05 MID-IDFS-IN       PIC X(8).                                    
001000*                                 FÖLJESEDELSNUMMER ENL ODETTE            
001100        05 MID-IDFS-UT       PIC X(8).                                    
001200*                                 FÖLJESEDELSNUMMER ENL ODETTE            
001300        05 MID-TIAVIDAT-IN   PIC X(6).                                    
001400*                                 AVISERINGSDATUM (YYMMDD)                
001500        05 MID-TIAVIDAT-UT   PIC X(6).                                    
001600*                                 AVISERINGSDATUM (YYMMDD)                
001700        05 MID-ADINLOMR-PRT-IN                                            
001800                             PIC X(4).                                    
001900*                                 PRINTERPLACERING                        
002000        05 MID-ADINLOMR-PRT-UT                                            
002100                             PIC X(4).                                    
002200*                                 PRINTERPLACERING                        
002300        05 MID-IDDC-IN       PIC X(2).                                    
002400*                                 IDENTIFIERARE LAGER                     
002500        05 MID-IDDC-UT       PIC X(2).                                    
002600*                                 IDENTIFIERARE LAGER                     
002700        05 MID-KDRT-IN       PIC X(2).                                    
002800*                                 REDOVISNINGSTYP                         
002900        05 MID-KDRT-UT       PIC X(2).                                    
003000*                                 REDOVISNINGSTYP                         
003100        05 MID-IDLBBET-IN    PIC X(12).                                   
003200*                                 LASTBÄRARBETECKNING                     
003300        05 MID-IDLBBET-UT    PIC X(12).                                   
003400*                                 LASTBÄRARBETECKNING                     
003500        05 MID-FLKLIVIS-IN   PIC X.                                       
003600*                                 JA/NEJ-FLAGGA                           
003700        05 MID-FLKLIVIS-UT   PIC X.                                       
003800*                                 JA/NEJ-FLAGGA                           
003900        05 MID-IDLOPNRM-IN   PIC X(8).                                    
004000*                                 LÖPNUMMER MOTTAGNINGSRAPPORT            
004100*                                 (0VVDLLLLK)                             
004200        05 MID-IDLOPNRM-UT   PIC X(8).                                    
004300*                                 LÖPNUMMER MOTTAGNINGSRAPPORT            
004400*                                 (0VVDLLLLK)                             
004500     03 MID-IDRADNR-ENTER    PIC 9(3).                                    
004600*                                 RADNUMMER                               
004700     03 MID-IDRADNR-NEXT     PIC 9(3).                                    
004800*                                 RADNUMMER                               
004900     03 MID-IDRADNR-INL-ENTER                                             
005000                             PIC 9(5).                                    
005100*                                 RADNUMMER INLEVERANS                    
005200     03 MID-IDRADNR-INL-NEXT PIC 9(5).                                    
005300*                                 RADNUMMER INLEVERANS                    
005400     03 MID-INPUT.                                                        
005500*                                 INDATA FÖR UPPDATERING                  
005600        05 MID-TIAVIDAT-UPD  PIC X(6).                                    
005700*                                 AVISERINGSDATUM (YYMMDD)                
005800        05 MID-IDLBBET-UPD   PIC X(12).                                   
005900*                                 LASTBÄRARBETECKNING                     
006000        05 MID-TIANKDAG-UPD  PIC X(6).                                    
006100*                                 ANKOMSTDAG                              
006200        05 MID-IDLEVNR-UPD   PIC X(5).                                    
006300*                                 LEVERANTÖRNUMMER                        
006400        05 MID-FLBORT-UPD    PIC X.                                       
006500*                                 BORTTAGNINGSFLAGGA                      
006600        05 MID-IDFTG-UPD     PIC X(2).                                    
006700*                                 FÖRETAGSID EKONOM REDOVISNING           
006800        05 MID-IDKONTO-UPD   PIC 9(10).                                   
006900*                                 KONTO                                   
007000        05 MID-IDANALYS-UPD  PIC X(12).                                   
007100*                                 ANALYSNUMMER                            
007200        05 MID-IDKST-UPD     PIC X(10).                                   
007300*                                 KOSTNADSSTÄLLE                          
007400        05 MID-RAD.                                                       
007500*                                 INDATA FÖR UPPDATERING                  
007600           07 MID-KDCMD-RAD  OCCURS 8 TIMES                               
007700                             PIC X.                                       
007800            88 MID-KDCMD-INGENTING                                        
007900                             VALUE ' '.                                   
008000            88 MID-KDCMD-DELETE                                           
008100                             VALUE 'D'                                    
008200                             'B'.                                         
008300            88 MID-KDCMD-REPLACE                                          
008400                             VALUE 'R'                                    
008500                             'Ä'.                                         
008600            88 MID-KDCMD-INSERT                                           
008700                             VALUE 'I'                                    
008800                             'N'                                          
008900                             'A'.                                         
009000            88 MID-KDCMD-SELECT                                           
009100                             VALUE 'S'                                    
009200                             'V'.                                         
009300            88 MID-KDCMD-PRINT                                            
009400                             VALUE 'P'                                    
009500                             'P'.                                         
009600            88 MID-KDCMD-COPY                                             
009700                             VALUE 'C'                                    
009800                             'K'.                                         
009900*                                 RAD-UPPDATERINGSKOMMANDO                
010000*                                  BLANK  = INGENTING                     
010100*                                  D , B  = DELETE                        
010200*                                  R , Ä  = REPLACE                       
010300*                                  I,N,A  = INSERT                        
010400*                                  S , V  = SELECT                        
010500*                                  P , P  = PRINT                         
010600*                                  C , K  = COPY                          
010700           07 MID-IDARTNR-RAD                                             
010800                             OCCURS 8 TIMES                               
010900                             PIC X(8).                                    
011000*                                 ARTIKELNUMMER                           
011100           07 MID-KVAVIS-RAD OCCURS 8 TIMES                               
011200                             PIC X(6).                                    
011300*                                 AVISERAT ANTAL                          
011400           07 MID-KDRT-RAD   OCCURS 8 TIMES                               
011500                             PIC X(2).                                    
011600*                                 REDOVISNINGSTYP                         
011700        05 MID-IDARTNR-UPD   PIC X(8).                                    
011800*                                 ARTIKELNUMMER                           
011900        05 MID-KVAVIS-UPD    PIC X(6).                                    
012000*                                 AVISERAT ANTAL                          
012100        05 MID-KDRT-UPD      PIC X(2).                                    
012200*                                 REDOVISNINGSTYP                         
012300     03 MID-IDARTNR-SPAR     OCCURS 8 TIMES                               
012400                             PIC 9(8).                                    
012500*                                 ARTIKELNUMMER                           
012600     03 MID-IDRADNR-SPAR     OCCURS 8 TIMES                               
012700                             PIC 9(3).                                    
012800*                                 RADNUMMER                               
012900     03 MID-IDRADNR-INL-SPAR OCCURS 8 TIMES                               
013000                             PIC 9(5).                                    
013100*                                 RADNUMMER INLEVERANS                    
013200*** END OF VILMAII-COPY LENGTH= 456 BYTES                                 
