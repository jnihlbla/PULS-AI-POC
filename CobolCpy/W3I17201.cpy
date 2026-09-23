000100 01  MID-W3I17201.                                                        
000200*                                 MID-COPYTEXT FÖR W3017200               
000300     03 MID-IDDISTR-IN       PIC X(4).                                    
000400*                                 DISTRIKTNUMMER                          
000500     03 MID-IDDISTR-UT       PIC X(4).                                    
000600*                                 DISTRIKTNUMMER                          
000700     03 MID-IDBYTRAP-IN      PIC X(7).                                    
000800*                                 RAPPORTNUMMER  BYTES                    
000900     03 MID-IDBYTRAP-UT      PIC X(7).                                    
001000*                                 RAPPORTNUMMER  BYTES                    
001100     03 MID-KDPRT            PIC X(3).                                    
001200*                                 PRINTERKOD                              
001300     03 MID-FLAGGA           PIC X.                                       
001400*                                 ALLMÄN FLAGGA                           
001500     03 MID-IDKUNDNR         PIC 9(7).                                    
001600*                                 KUNDNUMMER                              
001700     03 MID-IDFAKT           PIC 9(7).                                    
001800*                                 FAKTURANUMMER                           
001900     03 MID-RADINFO          OCCURS 9 TIMES.                              
002000*                                 RADINFORMATION                          
002100        05 MID-SELECT-URVAL  PIC X.                                       
002200        05 MID-OBJNR         PIC 9(9).                                    
002300*                                 OBJEKTNUMMER                            
002400        05 MID-KVPOINT       PIC 9(6).                                    
002500*                                 POINT VALUE                             
002600        05 MID-KVRETUR-GODK  PIC 9(3).                                    
002700        05 MID-KDBYTSTA-IN-UT-RAD                                         
002800                             PIC X.                                       
002900*                                 STATUSKOD BYTESOBJEKT                   
003000        05 MID-ANMARK-IN-UT  PIC X(3).                                    
003100        05 MID-FLSKROT-IN-UT PIC X.                                       
003200*                                 SKROTNINGSMARKERING                     
003300        05 MID-IDBYTRAD      PIC 9(5).                                    
003400*                                 RADNUMMER                               
003500        05 MID-IDTABNR       PIC 9(3).                                    
003600*                                 TABELLNUMMER                            
003700     03 MID-OBJNR-SPAERR     PIC X(9).                                    
003800*                                 OBJEKTNUMMER                            
003900     03 MID-IDBYTRAD-3173    PIC 9(5).                                    
004000*                                 RADNUMMER                               
004100     03 MID-INPUT.                                                        
004200*                                 RADINFORMATION                          
004300        05 MID-ANTAL-IN      PIC 9(3).                                    
004400        05 MID-ANMARK-IN     PIC X(3).                                    
004500        05 MID-FLSKROT-IN    PIC X.                                       
004600*                                 SKROTNINGSMARKERING                     
004700        05 MID-GODK-IN       PIC X.                                       
004800*** END OF VILMAII-COPY LENGTH= 350 BYTES                                 
