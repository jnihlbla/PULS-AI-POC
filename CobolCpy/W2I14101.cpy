000100 01  MID-W2I14101.                                                        
000200*                                 MID-COPYTEXT FÖR W2014100               
000300     03 MID-IDANSK-FROM-IN   PIC X(3).                                    
000400*                                 ANSKAFFARNUMMER                         
000500     03 MID-IDANSK-FROM-UT   PIC X(3).                                    
000600*                                 ANSKAFFARNUMMER                         
000700     03 MID-IDANSK-TOM-IN    PIC X(3).                                    
000800*                                 ANSKAFFARNUMMER                         
000900     03 MID-IDANSK-TOM-UT    PIC X(3).                                    
001000*                                 ANSKAFFARNUMMER                         
001100     03 MID-IDPROJ-IN        PIC X(4).                                    
001200*                                 PROJEKTIDENTITET                        
001300     03 MID-IDPROJ-UT        PIC X(4).                                    
001400*                                 PROJEKTIDENTITET                        
001500     03 MID-ANT-ARTIKLAR     PIC 9(5).                                    
001600*                                 ANTAL ARTIKLAR PER BRYTBEGREPP          
001700     03 MID-ANT-PASS-INLV    PIC 9(5).                                    
001800*                                 ANTAL ARTIKLAR PER BRYTBEGREPP          
001900     03 MID-ANT-PISK         PIC 9(5).                                    
002000*                                 ANTAL ARTIKLAR PER BRYTBEGREPP          
002100     03 MID-HELA-BASEN-LAEST PIC X.                                       
002200     03 MID-IDANSK-LO        PIC X(3).                                    
002300*                                 ANSKAFFARNUMMER                         
002400     03 MID-IDANSK-HI        PIC X(3).                                    
002500*                                 ANSKAFFARNUMMER                         
002600     03 MID-IDPROJ-LO        PIC X(4).                                    
002700*                                 PROJEKTIDENTITET                        
002800     03 MID-IDPROJ-HI        PIC X(4).                                    
002900*                                 PROJEKTIDENTITET                        
003000     03 MID-FLPISK-LO        PIC X.                                       
003100*                                 PISK ARTIKEL                            
003200     03 MID-FLPISK-HI        PIC X.                                       
003300*                                 PISK ARTIKEL                            
003400     03 MID-TIFINLEV-LO      PIC 9(6).                                    
003500*                                 PUBLICERINGSDATUM  (AAMMDD)             
003600     03 MID-TIFINLEV-HI      PIC 9(6).                                    
003700*                                 PUBLICERINGSDATUM  (AAMMDD)             
003800     03 MID-IDAO-LO          PIC X(10).                                   
003900*                                 ÄNDRINGSORDERNUMMER                     
004000     03 MID-IDAO-HI          PIC X(10).                                   
004100*                                 ÄNDRINGSORDERNUMMER                     
004200     03 MID-INFO-RAD         OCCURS 12 TIMES.                             
004300*                                 RADINFORMATION                          
004400        05 MID-SELECT-ARTIKEL                                             
004500                             PIC X.                                       
004600        05 MID-IDARTNR       PIC 9(9).                                    
004700*                                 ARTIKELNUMMER                           
004800        05 MID-NY-IDANSK     PIC 9(3).                                    
004900*                                 ANSKAFFARNUMMER                         
005000     03 MID-IDARTNR-LO       PIC 9(9).                                    
005100*                                 ARTIKELNUMMER                           
005200     03 MID-IDARTNR-HI       PIC 9(9).                                    
005300*                                 ARTIKELNUMMER                           
005400*** END COPY W2I14101C0  LENGTH=258                                       
