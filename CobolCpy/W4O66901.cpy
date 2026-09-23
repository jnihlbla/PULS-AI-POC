000100 01  MOD-W4O66901.                                                        
000200*                                 MOD-COPYTEXT FÖR W40669                 
000300*                                 PRIME COUNT SELECTION                   
000400     03 MOD-IDTRANS          PIC X(4).                                    
000500*                                 BILDNUMMER                              
000600     03 MOD-TEMFSFEL         PIC X(40).                                   
000700*                                 MFS FELMEDDELANDE                       
000800     03 MOD-TISKEPPN-IN      PIC X(6).                                    
000900*                                 SKEPPNINGSDATUM  (ÅÅMMDD)               
001000     03 MOD-TISKEPPN-UT      PIC X(6).                                    
001100*                                 SKEPPNINGSDATUM  (ÅÅMMDD)               
001200     03 MOD-IDTRPTNR-IN      PIC X(3).                                    
001300*                                 TRANSPORTIDENTITET                      
001400     03 MOD-IDTRPTNR-UT      PIC X(3).                                    
001500*                                 TRANSPORTIDENTITET                      
001600     03 MOD-IDLBBET-IN       PIC X(12).                                   
001700*                                 LASTBÄRARBETECKNING                     
001800     03 MOD-IDLBBET-UT       PIC X(12).                                   
001900*                                 LASTBÄRARBETECKNING                     
002000     03 MOD-IDDISTR-IN       PIC X(4).                                    
002100*                                 DISTRIKTNUMMER                          
002200     03 MOD-IDDISTR-UT       PIC X(4).                                    
002300*                                 DISTRIKTNUMMER                          
002400     03 MOD-IDKUNDNR-IN      PIC X(6).                                    
002500*                                 KUNDNUMMER                              
002600     03 MOD-IDKUNDNR-UT      PIC X(6).                                    
002700*                                 KUNDNUMMER                              
002800     03 MOD-IDDC-UT          PIC X(2).                                    
002900*                                 IDENTIFIERARE LAGER                     
003000     03 MOD-BEGMT-RAD1-IN    PIC X(35).                                   
003100*                                 GODSMOTTAGARNAMN RAD 1                  
003200     03 MOD-BEGMT-RAD1-UT    PIC X(35).                                   
003300*                                 GODSMOTTAGARNAMN RAD 1                  
003400     03 MOD-BEGMT-RAD2-IN    PIC X(35).                                   
003500*                                 GODSMOTTAGARNAMN RAD 2                  
003600     03 MOD-BEGMT-RAD2-UT    PIC X(35).                                   
003700*                                 GODSMOTTAGARNAMN RAD 2                  
003800     03 MOD-ADGMT-GATA-IN    PIC X(35).                                   
003900*                                 GODSMOTTAGARADRESS GATA                 
004000     03 MOD-ADGMT-GATA-UT    PIC X(35).                                   
004100*                                 GODSMOTTAGARADRESS GATA                 
004200     03 MOD-ADGMT-PADR-IN    PIC X(35).                                   
004300*                                 GODSMOTTAGARADRESS POSTADRESS           
004400     03 MOD-ADGMT-PADR-UT    PIC X(35).                                   
004500*                                 GODSMOTTAGARADRESS POSTADRESS           
004600     03 MOD-TABELLRAD        OCCURS 6 TIMES.                              
004700*                                 GRUPP MED TABELL RADER                  
004800        05 MOD-CMD-ATTR      PIC X(2).                                    
004900*                                 MFS ATTRIBUTFÄLT                        
005000        05 MOD-CMD           PIC X.                                       
005100        05 MOD-IDORDNR7      PIC X(7).                                    
005200*                                 ORDERNUMMER                             
005300        05 MOD-IDPRODNR      PIC X(7).                                    
005400*                                 PRODUKTIONSNUMMER                       
005500        05 MOD-IDKOLLI       PIC X(5).                                    
005600*                                 KOLLINUMMER                             
005700        05 MOD-IDPSN         PIC X(3).                                    
005800*                                 PROPER SHIPPING NAME                    
005900        05 MOD-KDORDKL       PIC X.                                       
006000*                                 ORDERKLASS                              
006100        05 MOD-KDKOLLI       PIC X(8).                                    
006200*                                 KOLLIKOD                                
006300        05 MOD-VKORDBTO-KOLLI-ATTR                                        
006400                             PIC X(2).                                    
006500*                                 MFS ATTRIBUTFÄLT                        
006600        05 MOD-VKORDBTO-KOLLI                                             
006700                             PIC Z(5)9.9.                                 
006800*                                 ORDERVIKT BRUTTO PER KOLLI              
006900     03 MOD-KDCMD-NY-ATTR    PIC X(2).                                    
007000*                                 MFS ATTRIBUTFÄLT                        
007100     03 MOD-KDCMD-NY         PIC X.                                       
007200*                                 RAD-UPPDATERINGSKOMMANDO                
007300*                                  BLANK  = INGENTING                     
007400*                                  D , B  = DELETE                        
007500*                                  R , Ä  = REPLACE                       
007600*                                  I,N,A  = INSERT                        
007700*                                  S , V  = SELECT                        
007800*                                  P , P  = PRINT                         
007900*                                  C , K  = COPY                          
008000     03 MOD-IDORDNR7-NY-ATTR PIC X(2).                                    
008100*                                 MFS ATTRIBUTFÄLT                        
008200     03 MOD-IDORDNR7-NY      PIC X(7).                                    
008300*                                 ORDERNUMMER                             
008400     03 MOD-IDPRODNR-NY-ATTR PIC X(2).                                    
008500*                                 MFS ATTRIBUTFÄLT                        
008600     03 MOD-IDPRODNR-NY      PIC X(7).                                    
008700*                                 PRODUKTIONSNUMMER                       
008800     03 MOD-IDKOLLI-NY-ATTR  PIC X(2).                                    
008900*                                 MFS ATTRIBUTFÄLT                        
009000     03 MOD-IDKOLLI-NY       PIC X(5).                                    
009100*                                 KOLLINUMMER                             
009200     03 MOD-IDPSN-NY-ATTR    PIC X(2).                                    
009300*                                 MFS ATTRIBUTFÄLT                        
009400     03 MOD-IDPSN-NY         PIC X(3).                                    
009500*                                 PROPER SHIPPING NAME                    
009600     03 MOD-KDORDKL-NY-ATTR  PIC X(2).                                    
009700*                                 MFS ATTRIBUTFÄLT                        
009800     03 MOD-KDORDKL-NY       PIC X.                                       
009900*                                 ORDERKLASS                              
010000     03 MOD-KDKOLLI-NY-ATTR  PIC X(2).                                    
010100*                                 MFS ATTRIBUTFÄLT                        
010200     03 MOD-KDKOLLI-NY       PIC X(8).                                    
010300*                                 KOLLIKOD                                
010400     03 MOD-VKORDBTO-KOLLI-NY-ATTR                                        
010500                             PIC X(2).                                    
010600*                                 MFS ATTRIBUTFÄLT                        
010700     03 MOD-VKORDBTO-KOLLI-NY                                             
010800                             PIC Z(5)9.9.                                 
010900*                                 ORDERVIKT BRUTTO PER KOLLI              
011000     03 MOD-BESORT           PIC X(6).                                    
011100*                                 BENÄMNING PÅ SORT/ENHET                 
011200     03 MOD-TEMFSINF         PIC X(55).                                   
011300*                                 INFORMATIONSMEDDELANDE                  
011400*** END OF VILMAII-COPY LENGTH= 769 BYTES                                 
