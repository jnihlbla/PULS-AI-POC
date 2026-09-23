000100 01  MOD-W6O20801.                                                        
000200*                                 MOD-COPYTEXT                            
000300*                                 FÖR W6O208                              
000400     03 MOD-IDTRANS          PIC X(4).                                    
000500*                                 BILDNUMMER                              
000600     03 MOD-TEMFSFEL         PIC X(40).                                   
000700*                                 MFS FELMEDDELANDE                       
000800     03 MOD-IDKR-IN          PIC X(2).                                    
000900*                                 MFS BEHANDLING AV INPUTFÄLT             
001000     03 MOD-IDKR-UT          PIC 9(5).                                    
001100*                                 KONTROLLRAPPORT NUMMER                  
001200     03 MOD-IDVERNR-FAKT     PIC 9(8).                                    
001300*                                 VERIFIKATIONSNUMMER                     
001400     03 MOD-TIFAKT           PIC 9(6).                                    
001500*                                 FAKTURERINGSDATUM (ÅÅMMDD)              
001600     03 MOD-IDVERNR-KRED     PIC X(9).                                    
001700*                                 VERIFIKATIONSNUMMER                     
001800     03 MOD-TIKRED           PIC 9(6).                                    
001900*                                 KREDITERINGSDATUM (ÅÅMMDD)              
002000     03 MOD-IDARTNR          PIC Z(8)9.                                   
002100*                                 ARTIKELNUMMER                           
002200     03 MOD-IDLEVNR          PIC X(5).                                    
002300*                                 LEVERANTÖRNUMMER                        
002400     03 MOD-KDPERSON-ATTR    PIC X(2).                                    
002500*                                 MFS ATTRIBUTFÄLT                        
002600     03 MOD-KDPERSON         PIC X(3).                                    
002700*                                 PERSONKOD                               
002800     03 MOD-BENAEMN          PIC X(25).                                   
002900*                                 BENÄMNING                               
003000     03 MOD-PRARTBEL-FAKT    PIC Z(7)9.9(3).                              
003100*                                 BESTPRIS LEVERANTÖRENS VALUTA           
003200     03 MOD-PRARTBEL-KRED    PIC Z(7)9.9(3).                              
003300*                                 BESTPRIS LEVERANTÖRENS VALUTA           
003400     03 MOD-SUOMK-FAKT-INT   PIC Z(6)9.9(2).                              
003500*                                 SUMMA OMKOSTNADER                       
003600     03 MOD-SUOMK-KRED-INT-ATTR                                           
003700                             PIC X(2).                                    
003800*                                 MFS ATTRIBUTFÄLT                        
003900     03 MOD-SUOMK-KRED-INT   PIC Z(6)9.9(2).                              
004000*                                 SUMMA OMKOSTNADER                       
004100     03 MOD-FLALL-SUOMK-INT-ATTR                                          
004200                             PIC X(2).                                    
004300*                                 MFS ATTRIBUTFÄLT                        
004400     03 MOD-FLALL-SUOMK-INT  PIC X.                                       
004500*                                 HELA BELOPPET VALT                      
004600     03 MOD-SUOMK-FAKT-EXT   PIC Z(6)9.9(2).                              
004700*                                 SUMMA OMKOSTNADER                       
004800     03 MOD-SUOMK-KRED-EXT-ATTR                                           
004900                             PIC X(2).                                    
005000*                                 MFS ATTRIBUTFÄLT                        
005100     03 MOD-SUOMK-KRED-EXT   PIC Z(6)9.9(2).                              
005200*                                 SUMMA OMKOSTNADER                       
005300     03 MOD-FLALL-SUOMK-EXT-ATTR                                          
005400                             PIC X(2).                                    
005500*                                 MFS ATTRIBUTFÄLT                        
005600     03 MOD-FLALL-SUOMK-EXT  PIC X.                                       
005700*                                 HELA BELOPPET VALT                      
005800     03 MOD-SUMAT-FAKT       PIC Z(6)9.9(2).                              
005900*                                 MATERIALKOSTNAD                         
006000     03 MOD-SUMAT-KRED-ATTR  PIC X(2).                                    
006100*                                 MFS ATTRIBUTFÄLT                        
006200     03 MOD-SUMAT-KRED       PIC X(10).                                   
006300*                                 MATERIALKOSTNAD                         
006400     03 MOD-SUMAT-KRED-NUM REDEFINES MOD-SUMAT-KRED                       
006500                             PIC Z(6)9.9(2).                              
006600*                                 MATERIALKOSTNAD                         
006700     03 MOD-FLALL-SUMAT-ATTR PIC X(2).                                    
006800*                                 MFS ATTRIBUTFÄLT                        
006900     03 MOD-FLALL-SUMAT      PIC X.                                       
007000*                                 HELA BELOPPET VALT                      
007100     03 MOD-SUBEL-FAKT-EJMOMS                                             
007200                             PIC Z(9).9(2).                               
007300*                                 SUMMABELOPP                             
007400     03 MOD-SUBEL-KRED-EJMOMS                                             
007500                             PIC Z(9).9(2).                               
007600*                                 SUMMABELOPP                             
007700     03 MOD-PRMOMS-FAKT      PIC Z(6)9.9(2).                              
007800*                                 MERVÄRDESSKATT                          
007900     03 MOD-PRMOMS-KRED      PIC X(10).                                   
008000*                                 MERVÄRDESSKATT                          
008100     03 MOD-PRMOMS-KRED-NUM REDEFINES MOD-PRMOMS-KRED                     
008200                             PIC Z(6)9.9(2).                              
008300*                                 MERVÄRDESSKATT                          
008400     03 MOD-SUBEL-FAKT-MOMS  PIC Z(9).9(2).                               
008500*                                 SUMMABELOPP                             
008600     03 MOD-SUBEL-KRED-MOMS  PIC Z(9).9(2).                               
008700*                                 SUMMABELOPP                             
008800     03 MOD-KDVALISO         PIC X(3).                                    
008900*                                 VALUTAKOD ENLIGT ISO-STANDARD.          
009000     03 MOD-IDPTYP           PIC X(3).                                    
009100*                                 POSTTYP                                 
009200     03 MOD-TEKREKON-INT-ATTR                                             
009300                             PIC X(2).                                    
009400*                                 MFS ATTRIBUTFÄLT                        
009500     03 MOD-TEKREKON-INT     PIC X(70).                                   
009600*                                 NOTERING EKONOMI INTERN                 
009700     03 MOD-TEKREKON-EXT-ATTR                                             
009800                             PIC X(2).                                    
009900*                                 MFS ATTRIBUTFÄLT                        
010000     03 MOD-TEKREKON-EXT     PIC X(70).                                   
010100*                                 NOTERING EKONOMI EXTERN                 
010200     03 MOD-TEKREKON-ATTR    PIC X(2).                                    
010300*                                 MFS ATTRIBUTFÄLT                        
010400     03 MOD-TEKREKON         PIC X(70).                                   
010500*                                 NOTERING EKONOMI                        
010600*                                                                         
010700     03 MOD-TEMFSINF         PIC X(55).                                   
010800*                                 INFORMATIONSMEDDELANDE                  
010900*** END OF VILMAII-COPY LENGTH= 568 BYTES                                 
