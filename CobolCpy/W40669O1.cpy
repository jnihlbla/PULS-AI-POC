000100 01  RESP-W40669O1.                                                       
000200*                                 RESP-COPYTEXT FÖR W40669                
000300*                                                                         
000400*                                 PRIME COUNT SELECTION                   
000500     03 RESP-IDDISTR-START   PIC 9(5).                                    
000600*                                 DISTRIKTNUMMER                          
000700     03 RESP-IDKUNDNR-START  PIC 9(7).                                    
000800*                                 KUNDNUMMER                              
000900     03 RESP-IDKUNDRF-START  PIC X(10).                                   
001000*                                 KUNDENS REFERENS (ORDERID)              
001100     03 RESP-IDPRODNR-START  PIC 9(7).                                    
001200*                                 PRODUKTIONSNUMMER                       
001300     03 RESP-IDKOLLI-START   PIC 9(5).                                    
001400*                                 KOLLINUMMER                             
001500     03 RESP-IDDISTR-NEXT    PIC 9(5).                                    
001600*                                 DISTRIKTNUMMER                          
001700     03 RESP-IDKUNDNR-NEXT   PIC 9(7).                                    
001800*                                 KUNDNUMMER                              
001900     03 RESP-IDKUNDRF-NEXT   PIC X(10).                                   
002000*                                 KUNDENS REFERENS (ORDERID)              
002100     03 RESP-IDPRODNR-NEXT   PIC 9(7).                                    
002200*                                 PRODUKTIONSNUMMER                       
002300     03 RESP-IDKOLLI-NEXT    PIC 9(5).                                    
002400*                                 KOLLINUMMER                             
002500     03 RESP-BEGMT-RAD1-UPD  PIC X(35).                                   
002600*                                 GODSMOTTAGARNAMN RAD 1                  
002700     03 RESP-BEGMT-RAD1-UT   PIC X(35).                                   
002800*                                 GODSMOTTAGARNAMN RAD 1                  
002900     03 RESP-BEGMT-RAD2-UPD  PIC X(35).                                   
003000*                                 GODSMOTTAGARNAMN RAD 2                  
003100     03 RESP-BEGMT-RAD2-UT   PIC X(35).                                   
003200*                                 GODSMOTTAGARNAMN RAD 2                  
003300     03 RESP-ADGMT-GATA-UPD  PIC X(35).                                   
003400*                                 GODSMOTTAGARADRESS GATA                 
003500     03 RESP-ADGMT-GATA-UT   PIC X(35).                                   
003600*                                 GODSMOTTAGARADRESS GATA                 
003700     03 RESP-ADGMT-PADR-UPD  PIC X(35).                                   
003800*                                 GODSMOTTAGARADRESS POSTADRESS           
003900     03 RESP-ADGMT-PADR-UT   PIC X(35).                                   
004000*                                 GODSMOTTAGARADRESS POSTADRESS           
004100     03 RESP-KDCMD-UPD-ATTR  PIC X(2).                                    
004200*                                 MFS ATTRIBUTFÄLT                        
004300     03 RESP-KDCMD-UPD       PIC X.                                       
004400*                                 RAD-UPPDATERINGSKOMMANDO                
004500*                                  BLANK  = INGENTING                     
004600*                                  D , B  = DELETE                        
004700*                                  R , Ä  = REPLACE                       
004800*                                  I,N,A  = INSERT                        
004900*                                  S , V  = SELECT                        
005000*                                  P , P  = PRINT                         
005100*                                  C , K  = COPY                          
005200     03 RESP-IDORDNR7-UPD-ATTR                                            
005300                             PIC X(2).                                    
005400*                                 MFS ATTRIBUTFÄLT                        
005500     03 RESP-IDORDNR7-UPD    PIC Z(6)9.                                   
005600*                                 ORDERNUMMER                             
005700     03 RESP-IDPRODNR-UPD-ATTR                                            
005800                             PIC X(2).                                    
005900*                                 MFS ATTRIBUTFÄLT                        
006000     03 RESP-IDPRODNR-UPD    PIC Z(6)9.                                   
006100*                                 PRODUKTIONSNUMMER                       
006200     03 RESP-IDKOLLI-UPD-ATTR                                             
006300                             PIC X(2).                                    
006400*                                 MFS ATTRIBUTFÄLT                        
006500     03 RESP-IDKOLLI-UPD     PIC Z(4)9.                                   
006600*                                 KOLLINUMMER                             
006700     03 RESP-IDPSN-UPD-ATTR  PIC X(2).                                    
006800*                                 MFS ATTRIBUTFÄLT                        
006900     03 RESP-IDPSN-UPD       PIC Z(2)9.                                   
007000*                                 PROPER SHIPPING NAME                    
007100     03 RESP-KDORDKL-UPD-ATTR                                             
007200                             PIC X(2).                                    
007300*                                 MFS ATTRIBUTFÄLT                        
007400     03 RESP-KDORDKL-UPD     PIC Z.                                       
007500*                                 ORDERKLASS                              
007600     03 RESP-KDKOLLI-UPD-ATTR                                             
007700                             PIC X(2).                                    
007800*                                 MFS ATTRIBUTFÄLT                        
007900     03 RESP-KDKOLLI-UPD     PIC X(8).                                    
008000*                                 KOLLIKOD                                
008100     03 RESP-VKORDBTO-KOLLI-UPD-ATTR                                      
008200                             PIC X(2).                                    
008300*                                 MFS ATTRIBUTFÄLT                        
008400     03 RESP-VKORDBTO-KOLLI-UPD                                           
008500                             PIC Z(5)9.9.                                 
008600*                                 ORDERVIKT BRUTTO PER KOLLI              
008700     03 RESP-BESORT          PIC X(6).                                    
008800*                                 BENÄMNING PÅ SORT/ENHET                 
008900     03 RESP-KVRADER         PIC 9(5).                                    
009000*                                 ANTAL RADER                             
009100     03 RESP-TABELLRAD       OCCURS 500 TIMES.                            
009200*                                 GRUPP MED TABELL RADER                  
009300        05 RESP-CMD-LINE-ATTR                                             
009400                             PIC X(2).                                    
009500*                                 MFS ATTRIBUTFÄLT                        
009600        05 RESP-CMD-LINE     PIC X.                                       
009700        05 RESP-IDORDNR7-LINE                                             
009800                             PIC Z(6)9.                                   
009900*                                 ORDERNUMMER                             
010000        05 RESP-IDPRODNR-LINE                                             
010100                             PIC Z(6)9.                                   
010200*                                 PRODUKTIONSNUMMER                       
010300        05 RESP-IDKOLLI-LINE PIC Z(4)9.                                   
010400*                                 KOLLINUMMER                             
010500        05 RESP-IDPSN-LINE   PIC X(3).                                    
010600*                                 PROPER SHIPPING NAME                    
010700        05 RESP-KDORDKL-LINE PIC Z.                                       
010800*                                 ORDERKLASS                              
010900        05 RESP-KDKOLLI-LINE PIC X(8).                                    
011000*                                 KOLLIKOD                                
011100        05 RESP-VKORDBTO-KOLLI-LINE-ATTR                                  
011200                             PIC X(2).                                    
011300*                                 MFS ATTRIBUTFÄLT                        
011400        05 RESP-VKORDBTO-KOLLI-LINE                                       
011500                             PIC Z(5)9.9.                                 
011600*                                 ORDERVIKT BRUTTO PER KOLLI              
011700*** END OF VILMAII-COPY LENGTH= 22415 BYTES                               
