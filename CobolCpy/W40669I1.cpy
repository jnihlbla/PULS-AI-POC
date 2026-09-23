000100 01  REQU-W40669I1.                                                       
000200*                                 REQU-COPYTEXT FÖR W40669                
000300*                                                                         
000400     03 REQU-TISKEPPN-KEY    PIC 9(6).                                    
000500*                                 SKEPPNINGSDATUM  (ÅÅMMDD)               
000600     03 REQU-IDTRPTNR-KEY    PIC 9(3).                                    
000700*                                 TRANSPORTIDENTITET                      
000800     03 REQU-IDLBBET-KEY     PIC X(12).                                   
000900*                                 LASTBÄRARBETECKNING                     
001000     03 REQU-IDDISTR-KEY     PIC 9(4).                                    
001100*                                 DISTRIKTNUMMER                          
001200     03 REQU-IDKUNDNR-KEY    PIC 9(6).                                    
001300*                                 KUNDNUMMER                              
001400     03 REQU-IDDC-KEY        PIC X(2).                                    
001500*                                 IDENTIFIERARE LAGER                     
001600     03 REQU-IDDISTR-START   PIC 9(5).                                    
001700*                                 DISTRIKTNUMMER                          
001800     03 REQU-IDKUNDNR-START  PIC 9(7).                                    
001900*                                 KUNDNUMMER                              
002000     03 REQU-IDKUNDRF-START  PIC X(10).                                   
002100*                                 KUNDENS REFERENS (ORDERID)              
002200     03 REQU-IDPRODNR-START  PIC 9(7).                                    
002300*                                 PRODUKTIONSNUMMER                       
002400     03 REQU-IDKOLLI-START   PIC 9(5).                                    
002500*                                 KOLLINUMMER                             
002600     03 REQU-KVRADER         PIC 9(5).                                    
002700*                                 ANTAL RADER                             
002800     03 REQU-BEGMT-RAD1-UPD  PIC X(35).                                   
002900*                                 GODSMOTTAGARNAMN RAD 1                  
003000     03 REQU-BEGMT-RAD1-UT   PIC X(35).                                   
003100*                                 GODSMOTTAGARNAMN RAD 1                  
003200     03 REQU-BEGMT-RAD2-UPD  PIC X(35).                                   
003300*                                 GODSMOTTAGARNAMN RAD 2                  
003400     03 REQU-BEGMT-RAD2-UT   PIC X(35).                                   
003500*                                 GODSMOTTAGARNAMN RAD 2                  
003600     03 REQU-ADGMT-GATA-UPD  PIC X(35).                                   
003700*                                 GODSMOTTAGARADRESS GATA                 
003800     03 REQU-ADGMT-GATA-UT   PIC X(35).                                   
003900*                                 GODSMOTTAGARADRESS GATA                 
004000     03 REQU-ADGMT-PADR-UPD  PIC X(35).                                   
004100*                                 GODSMOTTAGARADRESS POSTADRESS           
004200     03 REQU-ADGMT-PADR-UT   PIC X(35).                                   
004300*                                 GODSMOTTAGARADRESS POSTADRESS           
004400     03 REQU-KDCMD-UPD       PIC X.                                       
004500*                                 RAD-UPPDATERINGSKOMMANDO                
004600*                                  BLANK  = INGENTING                     
004700*                                  D , B  = DELETE                        
004800*                                  R , Ä  = REPLACE                       
004900*                                  I,N,A  = INSERT                        
005000*                                  S , V  = SELECT                        
005100*                                  P , P  = PRINT                         
005200*                                  C , K  = COPY                          
005300     03 REQU-IDORDNR7-UPD    PIC 9(7).                                    
005400*                                 ORDERNUMMER                             
005500     03 REQU-IDPRODNR-UPD    PIC 9(7).                                    
005600*                                 PRODUKTIONSNUMMER                       
005700     03 REQU-IDKOLLI-UPD     PIC 9(5).                                    
005800*                                 KOLLINUMMER                             
005900     03 REQU-IDPSN-UPD       PIC 9(3).                                    
006000*                                 PROPER SHIPPING NAME                    
006100     03 REQU-KDORDKL-UPD     PIC 9.                                       
006200*                                 ORDERKLASS                              
006300     03 REQU-KDKOLLI-UPD     PIC X(8).                                    
006400*                                 KOLLIKOD                                
006500     03 REQU-VKORDBTO-KOLLI-UPD                                           
006600                             PIC X(8).                                    
006700*                                 ORDERVIKT BRUTTO PER KOLLI              
006800     03 REQU-TABELLRAD       OCCURS 500 TIMES.                            
006900*                                 GRUPP MED TABELL RADER                  
007000        05 REQU-CMD-LINE     PIC X.                                       
007100        05 REQU-IDORDNR7-LINE                                             
007200                             PIC 9(7).                                    
007300*                                 ORDERNUMMER                             
007400        05 REQU-IDPRODNR-LINE                                             
007500                             PIC 9(7).                                    
007600*                                 PRODUKTIONSNUMMER                       
007700        05 REQU-IDKOLLI-LINE PIC 9(5).                                    
007800*                                 KOLLINUMMER                             
007900        05 REQU-IDPSN-LINE   PIC 9(3).                                    
008000*                                 PROPER SHIPPING NAME                    
008100        05 REQU-KDORDKL-LINE PIC 9.                                       
008200*                                 ORDERKLASS                              
008300        05 REQU-KDKOLLI-LINE PIC X(8).                                    
008400*                                 KOLLIKOD                                
008500        05 REQU-VKORDBTO-KOLLI-LINE                                       
008600                             PIC X(8).                                    
008700*                                 ORDERVIKT BRUTTO PER KOLLI              
008800*** END OF VILMAII-COPY LENGTH= 20392 BYTES                               
