000100 01  REQU-W30183I1.                                                       
000200*                                 REQU-COPYTEXT FÖR W3018300              
000300*                                                                         
000400     03 REQU-IDBYTKOL-KEY    PIC X(3).                                    
000500*                                 BYTES KOLLINUMMER                       
000600     03 REQU-IDARTNR-OBJ-KEY PIC X(9).                                    
000700*                                 OBJEKTNUMMER                            
000800     03 REQU-IDDISTR-KEY     PIC X(4).                                    
000900*                                 DISTRIKTNUMMER                          
001000     03 REQU-IDBYTKOL-START  PIC 9(3).                                    
001100*                                 BYTES KOLLINUMMER                       
001200     03 REQU-IDARTNR-OBJ-START                                            
001300                             PIC 9(9).                                    
001400*                                 OBJEKTNUMMER                            
001500     03 REQU-IDDISTR-START   PIC 9(5).                                    
001600*                                 DISTRIKTNUMMER                          
001700     03 REQU-IDDC            PIC X(2).                                    
001800*                                 IDENTIFIERARE LAGER                     
001900     03 REQU-IDSPRAK         PIC X(2).                                    
002000*                                 2-STÄLLIG ISO SPRÅKKOD                  
002100     03 REQU-KVRADER         PIC 9(5).                                    
002200*                                 ANTAL RADER                             
002300     03 REQU-NYRAD.                                                       
002400*                                 RADINFORMATION                          
002500        05 REQU-IDARTNR-OBJ-UPPD                                          
002600                             PIC X(9).                                    
002700*                                 OBJEKTNUMMER                            
002800        05 REQU-KVRETUR-UPPD PIC X(7).                                    
002900*                                 ANTAL I RETUR                           
003000        05 REQU-FLBYTKNR-UPPD                                             
003100                             PIC X.                                       
003200*                                 KOLLI PROFORMA FLAGGA                   
003300*                                 Y = NYTT KOLLINR                        
003400*                                 N = SAMMA KOLLINR                       
003500     03 REQU-BORT.                                                        
003600*                                 RADINFORMATION                          
003700        05 REQU-IDARTNR-OBJ-BORT                                          
003800                             PIC X(9).                                    
003900*                                 OBJEKTNUMMER                            
004000        05 REQU-KVRETUR-BORT PIC X(7).                                    
004100*                                 ANTAL I RETUR                           
004200        05 REQU-IDBYTKOL-BORT                                             
004300                             PIC X(3).                                    
004400*                                 BYTES KOLLINUMMER                       
004500     03 REQU-GODKRAD.                                                     
004600*                                 RADINFORMATION                          
004700        05 REQU-VKORDBTO-FAKT-UPD                                         
004800                             PIC X(8).                                    
004900*                                 ORDERVIKT BRUTTO PER FAKTURA            
005000        05 REQU-VLORDBTO-FAKT-UPD                                         
005100                             PIC X(8).                                    
005200*                                 ORDERVOLYM BRUTTO PER FAKTURA           
005300        05 REQU-GODK-FAKT    PIC X.                                       
005400*** END OF VILMAII-COPY LENGTH= 95 BYTES                                  
