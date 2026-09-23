000100 01  RESP-WL0199O1.                                                       
000200*                                 RESPONS FROM PGM WL0199                 
000300     03 RESP-IDDC-KEY        PIC X(2).                                    
000400*                                 IDENTIFIERARE LAGER                     
000500     03 RESP-IDANSTNR-KEY    PIC Z(5).                                    
000600*                                 ANSTÄLLNINGSNUMMER                      
000700     03 RESP-IDPRODNR-KEY    PIC Z(7).                                    
000800*                                 PRODUKTIONSNUMMER                       
000900     03 RESP-IDPLKLST-KEY    PIC Z(3).                                    
001000*                                 PLOCKLISTNUMMER                         
001100     03 RESP-KVRADER-MAX1    PIC 9(5).                                    
001200*                                 MAX INDEX KOPPLAT TILL OCCURS N         
001300*                                 EDAN.                                   
001400     03 RESP-FLSISTAK        PIC X.                                       
001500*                                 SISTA KOLLI I ORDERN?                   
001600     03 RESP-FLSKRIV-CLABEL  PIC X.                                       
001700*                                 J/Y = SKRIV BEGÄRD LISTA                
001800     03 RESP-FLSKRIV-DELNOTE PIC X.                                       
001900*                                 J/Y = SKRIV BEGÄRD LISTA                
002000     03 RESP-PRTVAL-ADRESSFL PIC X(2).                                    
002100*                                 PRINTER-VAL KOD ADRESS FLAGGA           
002200     03 RESP-PRTVAL-FOLJEFL  PIC X(2).                                    
002300*                                 PRINTER-VAL KOD FÖLJESEDEL              
002400     03 RESP-WL0123I1.                                                    
002500*                                 REQUEST TO PGM WL0123                   
002600        05 RESP-L123-IDDC-KEY                                             
002700                             PIC X(2).                                    
002800*                                 IDENTIFIERARE LAGER                     
002900        05 RESP-L123-IDANSTNR-KEY                                         
003000                             PIC Z(4)9.                                   
003100*                                 ANSTÄLLNINGSNUMMER                      
003200        05 RESP-L123-IDDISTR-KEY                                          
003300                             PIC Z(3)9.                                   
003400*                                 DISTRIKTNUMMER                          
003500        05 RESP-L123-IDKUNDNR-KEY                                         
003600                             PIC Z(5)9.                                   
003700*                                 KUNDNUMMER                              
003800        05 RESP-L123-IDORDNR-KEY                                          
003900                             PIC Z(4)9.                                   
004000*                                 ORDERNUMMER                             
004100        05 RESP-L123-IDKOLLI-KEY                                          
004200                             PIC Z(4)9.                                   
004300*                                 KOLLINUMMER                             
004400        05 RESP-L123-IDPRODNR-KEY                                         
004500                             PIC Z(6)9.                                   
004600*                                 PRODUKTIONSNUMMER                       
004700        05 RESP-L123-FLAGGA  PIC X.                                       
004800*                                 ALLMÄN FLAGGA                           
004900        05 RESP-L123-FLSVAR  PIC X.                                       
005000*                                 ALLMÄN SVARSFLAGGA                      
005100        05 RESP-L123-IDPLKLST-SPAR                                        
005200                             PIC Z(2)9.                                   
005300*                                 PLOCKLISTNUMMER                         
005400        05 RESP-L123-IDRADNR-SPAR                                         
005500                             PIC Z(3)9.                                   
005600*                                 RADNUMMER                               
005700     03 RESP-WL0128I1.                                                    
005800*                                 REQUEST FOR PGM WL0128                  
005900*                                 PRINTING OF CASE LABEL                  
006000        05 RESP-L128-FLBG    PIC X.                                       
006100*                                 ALLMÄN FLAGGA                           
006200        05 RESP-L128-KVRADER PIC Z(4)9.                                   
006300*                                 ANTAL RADER                             
006400        05 RESP-L128-RAD     OCCURS 50 TIMES.                             
006500*                                 REQUEST-COPYTEXT FÖR WL0128             
006600*                                                                         
006700           07 RESP-L128-IDDC-KEY                                          
006800                             PIC X(2).                                    
006900*                                 IDENTIFIERARE LAGER                     
007000           07 RESP-L128-IDDISTR-KEY                                       
007100                             PIC Z(3)9.                                   
007200*                                 DISTRIKTNUMMER                          
007300           07 RESP-L128-IDKUNDNR-KEY                                      
007400                             PIC Z(5)9.                                   
007500*                                 KUNDNUMMER                              
007600           07 RESP-L128-IDORDNR-KEY                                       
007700                             PIC Z(4)9.                                   
007800*                                 ORDERNUMMER UTGÅR PD90                  
007900           07 RESP-L128-IDKOLLI-KEY                                       
008000                             PIC Z(4)9.                                   
008100*                                 KOLLINUMMER                             
008200           07 RESP-L128-IDPRODNR-KEY                                      
008300                             PIC Z(6)9.                                   
008400*                                 PRODUKTIONSNUMMER                       
008500           07 RESP-L128-CLABEL                                            
008600                             PIC X.                                       
008700*                                 J/Y = SKRIV BEGÄRD LISTA                
008800           07 RESP-L128-IDKOLLI-TOM                                       
008900                             PIC Z(4)9.                                   
009000*                                 KOLLINUMMER TILL OCH MED                
009100     03 RESP-WL0129I1.                                                    
009200*                                 REQUEST TO PGM  WL0129                  
009300*                                 PRINTING OF DELIVERY NOTE               
009400        05 RESP-L129-FLBG    PIC X.                                       
009500*                                 ALLMÄN FLAGGA                           
009600        05 RESP-L129-KVRADER PIC Z(4)9.                                   
009700*                                 ANTAL RADER                             
009800        05 RESP-L129-RAD     OCCURS 50 TIMES.                             
009900*                                 REQUEST TILL PGM WL0129                 
010000*                                                                         
010100           07 RESP-L129-IDDC-KEY                                          
010200                             PIC X(2).                                    
010300*                                 IDENTIFIERARE LAGER                     
010400           07 RESP-L129-IDDISTR-KEY                                       
010500                             PIC Z(3)9.                                   
010600*                                 DISTRIKTNUMMER                          
010700           07 RESP-L129-IDKUNDNR-KEY                                      
010800                             PIC Z(5)9.                                   
010900*                                 KUNDNUMMER                              
011000           07 RESP-L129-IDORDNR-KEY                                       
011100                             PIC Z(4)9.                                   
011200*                                 ORDERNUMMER UTGÅR PD90                  
011300           07 RESP-L129-IDKOLLI-KEY                                       
011400                             PIC Z(4)9.                                   
011500*                                 KOLLINUMMER                             
011600           07 RESP-L129-IDKOLLI-TOM                                       
011700                             PIC Z(4)9.                                   
011800*                                 KOLLINUMMER                             
011900           07 RESP-L129-FLSKRIV-DELNOTE                                   
012000                             PIC X.                                       
012100*                                 J/Y = SKRIV BEGÄRD LISTA                
012200     03 RESP-RAD             OCCURS 1 TO 100 TIMES                        
012300                             DEPENDING ON RESP-KVRADER-MAX1.              
012400        05 RESP-IDKOLLI      PIC 9(5).                                    
012500*                                 KOLLINUMMER                             
012600        05 RESP-KDKOLLI      PIC X(8).                                    
012700*                                 KOLLIKOD                                
012800        05 RESP-IDRADNR-FOM  PIC 9(4).                                    
012900*                                 RADNUMMER                               
013000        05 RESP-IDRADNR-TOM  PIC 9(4).                                    
013100*                                 RADNUMMER                               
013200        05 RESP-KVLEVART     PIC 9(7).                                    
013300*                                 LEVERERAT ANTAL STYCK                   
013400        05 RESP-KDEMBTYP     PIC 9.                                       
013500*                                 EMBALLAGETYP                            
013600        05 RESP-DIKOLLIL     PIC 9(4).                                    
013700*                                 DISTRIKTNUMMER                          
013800        05 RESP-DIKOLLIB     PIC 9(3).                                    
013900*                                 ANTAL RADER                             
014000        05 RESP-DIKOLLIH     PIC 9(3).                                    
014100*                                 ANTAL RADER                             
014200        05 RESP-IDMSG-ERROR-LINE                                          
014300                             PIC X(3).                                    
014400*                                 FELMEDDELANDE ID                        
014500*** END OF VILMAII-COPY LENGTH= 7434 BYTES                                
