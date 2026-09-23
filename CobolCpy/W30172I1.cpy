000100 01  REQU-W30172I1.                                                       
000200*                                 REQUEST COPYTEXT FOR W3017210           
000300     03 REQU-IDDC-KEY        PIC X(2).                                    
000400*                                 IDENTIFIERARE LAGER                     
000500*                                 WAREHOUSE IDENTIFIER                    
000600     03 REQU-IDDISTR-KEY     PIC X(4).                                    
000700*                                 DISTRIKTNUMMER                          
000800*                                 DISTRICT NUMBER                         
000900     03 REQU-IDBYTRAP-KEY    PIC X(7).                                    
001000*                                 RAPPORTNUMMER  BYTES                    
001100*                                 REPORTNUMBER   EXCHANGE                 
001200     03 REQU-KDPRT           PIC X(3).                                    
001300*                                 PRINTERKOD                              
001400*                                 PRINTERCODE                             
001500     03 REQU-IDSPRAK         PIC X(2).                                    
001600*                                 2-STÄLLIG ISO SPRÅKKOD                  
001700*                                 2-LETTER ISO LANGUAGE CODE              
001800     03 REQU-IDTRANS-FROM    PIC X(4).                                    
001900*                                 BILDNUMMER                              
002000*                                 SCREEN NUMBER                           
002100     03 REQU-IDBYTRAD-START  PIC 9(5).                                    
002200*                                 RADNUMMER                               
002300*                                 LINE NO                                 
002400     03 REQU-FLAGGA          PIC X.                                       
002500*                                 ALLMÄN FLAGGA                           
002600*                                 GENERAL FLAG                            
002700     03 REQU-IDKUNDNR        PIC 9(7).                                    
002800*                                 KUNDNUMMER                              
002900*                                 CUSTOMER NO                             
003000     03 REQU-IDFAKT          PIC 9(7).                                    
003100*                                 FAKTURANUMMER                           
003200*                                 INVOICE NO.                             
003300     03 REQU-IDARTNR-OBJ-SPAERR                                           
003400                             PIC X(9).                                    
003500*                                 OBJEKTNUMMER                            
003600     03 REQU-IDBYTRAD-3173   PIC 9(5).                                    
003700*                                 RADNUMMER                               
003800*                                 LINE NO                                 
003900     03 REQU-INPUT.                                                       
004000*                                 RADINFORMATION                          
004100*                                 LINE INFORMATION                        
004200        05 REQU-KVRETUR-IN   PIC 9(5).                                    
004300*                                 ANTAL I RETUR                           
004400*                                 NUMBER IN RETURN                        
004500        05 REQU-KDBYTREF-IN  PIC X(3).                                    
004600*                                 CENTRAL REFERENS                        
004700*                                 CENTRAL REFERENCE                       
004800        05 REQU-FLSKROT-IN   PIC X.                                       
004900*                                 SKROTNINGSMARKERING                     
005000*                                 SCRAPPING FLAG                          
005100        05 REQU-FLGODK-IN    PIC X.                                       
005200*                                 GODKÄNT?  JA/NEJ                        
005300*                                 APPROVED? J/N                           
005400     03 REQU-KVRADER         PIC 9(5).                                    
005500*                                 ANTAL RADER                             
005600*                                 NUMBER OF LINES                         
005700     03 REQU-RADINFO         OCCURS 300 TIMES.                            
005800*                                 RADINFORMATION                          
005900*                                 LINE INFORMATION                        
006000        05 REQU-KDCMD-LINE   PIC X.                                       
006100*                                 RAD-UPPDATERINGSKOMMANDO                
006200*                                  BLANK  = INGENTING                     
006300*                                  D , B  = DELETE                        
006400*                                  R , Ä  = REPLACE                       
006500*                                  I,N,A  = INSERT                        
006600*                                  S , V  = SELECT                        
006700*                                  P , P  = PRINT                         
006800*                                  C , K  = COPY                          
006900*                                 LINE UPDATE COMMAND                     
007000        05 REQU-IDARTNR-OBJ-LINE                                          
007100                             PIC 9(9).                                    
007200*                                 OBJEKTNUMMER                            
007300        05 REQU-KVPOINT-LINE PIC 9(6).                                    
007400*                                 POINT VALUE                             
007500        05 REQU-KVRETUR-GODK-LINE                                         
007600                             PIC 9(5).                                    
007700*                                 ANTAL I RETUR                           
007800*                                 NUMBER IN RETURN                        
007900        05 REQU-KDBYTSTA-LINE                                             
008000                             PIC X.                                       
008100*                                 STATUSKOD BYTESOBJEKT                   
008200*                                 STATUSCODE EXCH CORES                   
008300        05 REQU-KDBYTREF-LINE                                             
008400                             PIC X(3).                                    
008500*                                 CENTRAL REFERENS                        
008600*                                 CENTRAL REFERENCE                       
008700        05 REQU-FLSKROT-LINE PIC X.                                       
008800*                                 SKROTNINGSMARKERING                     
008900*                                 SCRAPPING FLAG                          
009000        05 REQU-IDBYTRAD-LINE                                             
009100                             PIC 9(5).                                    
009200*                                 RADNUMMER                               
009300*                                 LINE NO                                 
009400        05 REQU-IDTABNR-LINE PIC 9(3).                                    
009500*                                 TABELLNUMMER                            
009600*                                 TABELNUMBER                             
009700*** END OF VILMAII-COPY LENGTH= 10271 BYTES                               
