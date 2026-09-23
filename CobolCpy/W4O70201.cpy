000100 01  MOD-W4O70201.                                                        
000200*                                 MOD-COPYTEXT FÖR W4070200               
000300     03 MOD-IDTRANS          PIC X(4).                                    
000400*                                 BILDNUMMER                              
000500     03 MOD-TEMFSFEL         PIC X(40).                                   
000600*                                 MFS FELMEDDELANDE                       
000700     03 MOD-IDARTNR-IN       PIC X(9).                                    
000800*                                 ARTIKELNUMMER                           
000900     03 MOD-IDFTG-IN         PIC X(2).                                    
001000*                                 FÖRETAGSID EKONOM REDOVISNING           
001100     03 MOD-IDARTNR-UT       PIC X(9).                                    
001200*                                 ARTIKELNUMMER                           
001300     03 MOD-IDFTG-UT         PIC X(2).                                    
001400*                                 FÖRETAGSID EKONOM REDOVISNING           
001500     03 MOD-IDARTNR-ENTER    PIC 9(9).                                    
001600*                                 ARTIKELNUMMER                           
001700     03 MOD-TIGILTIG-FOM-ENTER                                            
001800                             PIC 9(6).                                    
001900*                                 GILTIGHETSDATUM FOM                     
002000     03 MOD-KDANMORS-ENTER   PIC X(2).                                    
002100*                                 ORSAK TILL LEVERANSANMÄRKNING           
002200     03 MOD-IDARTNR-NEXT     PIC 9(9).                                    
002300*                                 ARTIKELNUMMER                           
002400     03 MOD-TIGILTIG-FOM-NEXT                                             
002500                             PIC 9(6).                                    
002600*                                 GILTIGHETSDATUM FOM                     
002700     03 MOD-KDANMORS-NEXT    PIC X(2).                                    
002800*                                 ORSAK TILL LEVERANSANMÄRKNING           
002900     03 MOD-RAD-1.                                                        
003000*                                 RADINFORMATION                          
003100        05 MOD-IDARTNR-ATTR  PIC X(2).                                    
003200*                                 MFS ATTRIBUTFÄLT                        
003300        05 MOD-IDARTNR-RAD1  PIC Z(8)9.                                   
003400*                                 ARTIKELNUMMER                           
003500        05 MOD-TIGILTIG-FOM-ATTR                                          
003600                             PIC X(2).                                    
003700*                                 MFS ATTRIBUTFÄLT                        
003800        05 MOD-TIGILTIG-FOM-RAD1                                          
003900                             PIC 9(6).                                    
004000*                                 GILTIGHETSDATUM FOM                     
004100        05 MOD-TIGILTIG-TOM-ATTR                                          
004200                             PIC X(2).                                    
004300*                                 MFS ATTRIBUTFÄLT                        
004400        05 MOD-TIGILTIG-TOM-RAD1                                          
004500                             PIC 9(6).                                    
004600*                                 GILTIGHETSDATUM TOM                     
004700        05 MOD-IDFTG-ATTR    PIC X(2).                                    
004800*                                 MFS ATTRIBUTFÄLT                        
004900        05 MOD-IDFTG-RAD1    PIC 9(2).                                    
005000*                                 FÖRETAGSID EKONOM REDOVISNING           
005100        05 MOD-KDANMORS-ATTR PIC X(2).                                    
005200*                                 MFS ATTRIBUTFÄLT                        
005300        05 MOD-KDANMORS-RAD1 PIC X(2).                                    
005400*                                 ORSAK TILL LEVERANSANMÄRKNING           
005500        05 MOD-IDANALYS-ATTR PIC X(2).                                    
005600*                                 MFS ATTRIBUTFÄLT                        
005700        05 MOD-IDANALYS-RAD1 PIC Z(12).                                   
005800*                                 ANALYSNUMMER                            
005900        05 MOD-IDKONTO-ATTR  PIC X(2).                                    
006000*                                 MFS ATTRIBUTFÄLT                        
006100        05 MOD-IDKONTO-RAD1  PIC Z(10).                                   
006200*                                 KONTO                                   
006300        05 MOD-IDKST-ATTR    PIC X(2).                                    
006400*                                 MFS ATTRIBUTFÄLT                        
006500        05 MOD-IDKST-RAD1    PIC X(10).                                   
006600*                                 KOSTNADSSTÄLLE                          
006700        05 MOD-IDUSER-ATTR   PIC X(2).                                    
006800*                                 MFS ATTRIBUTFÄLT                        
006900        05 MOD-IDUSER-RAD1   PIC X(2).                                    
007000        05 MOD-KVART-RAD1    PIC Z(6)9.                                   
007100*                                 ANTAL ARTNR PER BRYTBEGREPP             
007200     03 MOD-RADER-2-11       OCCURS 10 TIMES.                             
007300*                                 RADINFORMATION                          
007400        05 MOD-IDARTNR       PIC Z(8)9.                                   
007500*                                 ARTIKELNUMMER                           
007600        05 MOD-TIGILTIG-FOM  PIC 9(6).                                    
007700*                                 GILTIGHETSDATUM FOM                     
007800        05 MOD-TIGILTIG-TOM  PIC 9(6).                                    
007900*                                 GILTIGHETSDATUM TOM                     
008000        05 MOD-IDFTG         PIC 9(2).                                    
008100*                                 FÖRETAGSID EKONOM REDOVISNING           
008200        05 MOD-KDANMORS      PIC X(2).                                    
008300*                                 ORSAK TILL LEVERANSANMÄRKNING           
008400        05 MOD-IDANALYS      PIC Z(12).                                   
008500*                                 ANALYSNUMMER                            
008600        05 MOD-IDKONTO       PIC Z(10).                                   
008700*                                 KONTO                                   
008800        05 MOD-IDKST         PIC X(10).                                   
008900*                                 KOSTNADSSTÄLLE                          
009000        05 MOD-IDUSER        PIC X(2).                                    
009100        05 MOD-KVART         PIC Z(6)9.                                   
009200*                                 ANTAL ARTNR PER BRYTBEGREPP             
009300     03 MOD-IDARTNR-UPP-ATTR PIC X(2).                                    
009400*                                 MFS ATTRIBUTFÄLT                        
009500     03 MOD-IDARTNR-UPP      PIC Z(8)9.                                   
009600*                                 ARTIKELNUMMER                           
009700     03 MOD-TIGILTIG-FOM-UPP-ATTR                                         
009800                             PIC X(2).                                    
009900*                                 MFS ATTRIBUTFÄLT                        
010000     03 MOD-TIGILTIG-FOM-UPP PIC 9(6).                                    
010100*                                 GILTIGHETSDATUM FOM                     
010200     03 MOD-TIGILTIG-TOM-UPP-ATTR                                         
010300                             PIC X(2).                                    
010400*                                 MFS ATTRIBUTFÄLT                        
010500     03 MOD-TIGILTIG-TOM-UPP PIC 9(6).                                    
010600*                                 GILTIGHETSDATUM TOM                     
010700     03 MOD-IDFTG-UPP-ATTR   PIC X(2).                                    
010800*                                 MFS ATTRIBUTFÄLT                        
010900     03 MOD-IDFTG-UPP        PIC 9(2).                                    
011000*                                 FÖRETAGSID EKONOM REDOVISNING           
011100     03 MOD-KDANMORS-UPP-ATTR                                             
011200                             PIC X(2).                                    
011300*                                 MFS ATTRIBUTFÄLT                        
011400     03 MOD-KDANMORS-UPP     PIC X(2).                                    
011500*                                 ORSAK TILL LEVERANSANMÄRKNING           
011600     03 MOD-IDANALYS-UPP-ATTR                                             
011700                             PIC X(2).                                    
011800*                                 MFS ATTRIBUTFÄLT                        
011900     03 MOD-IDANALYS-UPP     PIC Z(12).                                   
012000*                                 ANALYSNUMMER                            
012100     03 MOD-IDKONTO-UPP-ATTR PIC X(2).                                    
012200*                                 MFS ATTRIBUTFÄLT                        
012300     03 MOD-IDKONTO-UPP      PIC Z(10).                                   
012400*                                 KONTO                                   
012500     03 MOD-IDKST-UPP-ATTR   PIC X(2).                                    
012600*                                 MFS ATTRIBUTFÄLT                        
012700     03 MOD-IDKST-UPP        PIC X(10).                                   
012800*                                 KOSTNADSSTÄLLE                          
012900     03 MOD-IDUSER-UPP-ATTR  PIC X(2).                                    
013000*                                 MFS ATTRIBUTFÄLT                        
013100     03 MOD-IDUSER-UPP       PIC X(2).                                    
013200     03 MOD-FLBORT-UPP-ATTR  PIC X(2).                                    
013300*                                 MFS ATTRIBUTFÄLT                        
013400     03 MOD-FLBORT-UPP       PIC X.                                       
013500*                                 BORTTAGNINGSFLAGGA                      
013600     03 MOD-TEMFSINF         PIC X(55).                                   
013700*                                 INFORMATIONSMEDDELANDE                  
013800*** END OF VILMAII-COPY LENGTH= 979 BYTES                                 
