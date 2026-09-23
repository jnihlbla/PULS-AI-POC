000100 01  MID-W4I66901.                                                        
000200*                                 MID-COPYTEXT FÖR W40669                 
000300     03 MID-TISKEPPN-IN      PIC X(6).                                    
000400*                                 SKEPPNINGSDATUM  (ÅÅMMDD)               
000500     03 MID-TISKEPPN-UT      PIC X(6).                                    
000600*                                 SKEPPNINGSDATUM  (ÅÅMMDD)               
000700     03 MID-IDTRPTNR-IN      PIC X(3).                                    
000800*                                 TRANSPORTIDENTITET                      
000900     03 MID-IDTRPTNR-UT      PIC X(3).                                    
001000*                                 TRANSPORTIDENTITET                      
001100     03 MID-IDLBBET-IN       PIC X(12).                                   
001200*                                 LASTBÄRARBETECKNING                     
001300     03 MID-IDLBBET-UT       PIC X(12).                                   
001400*                                 LASTBÄRARBETECKNING                     
001500     03 MID-IDDISTR-IN       PIC X(4).                                    
001600*                                 DISTRIKTNUMMER                          
001700     03 MID-IDDISTR-UT       PIC X(4).                                    
001800*                                 DISTRIKTNUMMER                          
001900     03 MID-IDKUNDNR-IN      PIC X(6).                                    
002000*                                 KUNDNUMMER                              
002100     03 MID-IDKUNDNR-UT      PIC X(6).                                    
002200*                                 KUNDNUMMER                              
002300     03 MID-IDDC-UT          PIC X(2).                                    
002400*                                 IDENTIFIERARE LAGER                     
002500     03 MID-BEGMT-RAD1-IN    PIC X(35).                                   
002600*                                 GODSMOTTAGARNAMN RAD 1                  
002700     03 MID-BEGMT-RAD1-UT    PIC X(35).                                   
002800*                                 GODSMOTTAGARNAMN RAD 1                  
002900     03 MID-BEGMT-RAD2-IN    PIC X(35).                                   
003000*                                 GODSMOTTAGARNAMN RAD 2                  
003100     03 MID-BEGMT-RAD2-UT    PIC X(35).                                   
003200*                                 GODSMOTTAGARNAMN RAD 2                  
003300     03 MID-ADGMT-GATA-IN    PIC X(35).                                   
003400*                                 GODSMOTTAGARADRESS GATA                 
003500     03 MID-ADGMT-GATA-UT    PIC X(35).                                   
003600*                                 GODSMOTTAGARADRESS GATA                 
003700     03 MID-ADGMT-PADR-IN    PIC X(35).                                   
003800*                                 GODSMOTTAGARADRESS POSTADRESS           
003900     03 MID-ADGMT-PADR-UT    PIC X(35).                                   
004000*                                 GODSMOTTAGARADRESS POSTADRESS           
004100     03 MID-TABELLRAD        OCCURS 6 TIMES.                              
004200*                                 GRUPP MED TABELL RADER                  
004300        05 MID-CMD           PIC X.                                       
004400        05 MID-IDORDNR7      PIC X(7).                                    
004500*                                 ORDERNUMMER                             
004600        05 MID-IDPRODNR      PIC X(7).                                    
004700*                                 PRODUKTIONSNUMMER                       
004800        05 MID-IDKOLLI       PIC X(5).                                    
004900*                                 KOLLINUMMER                             
005000        05 MID-IDPSN         PIC X(3).                                    
005100*                                 PROPER SHIPPING NAME                    
005200        05 MID-KDORDKL       PIC X.                                       
005300*                                 ORDERKLASS                              
005400        05 MID-KDKOLLI       PIC X(8).                                    
005500*                                 KOLLIKOD                                
005600        05 MID-VKORDBTO-KOLLI                                             
005700                             PIC X(8).                                    
005800*                                 ORDERVIKT BRUTTO PER KOLLI              
005900     03 MID-KDCMD-NY         PIC X.                                       
006000*                                 RAD-UPPDATERINGSKOMMANDO                
006100*                                  BLANK  = INGENTING                     
006200*                                  D , B  = DELETE                        
006300*                                  R , Ä  = REPLACE                       
006400*                                  I,N,A  = INSERT                        
006500*                                  S , V  = SELECT                        
006600*                                  P , P  = PRINT                         
006700*                                  C , K  = COPY                          
006800     03 MID-IDORDNR7-NY      PIC X(7).                                    
006900*                                 ORDERNUMMER                             
007000     03 MID-IDPRODNR-NY      PIC X(7).                                    
007100*                                 PRODUKTIONSNUMMER                       
007200     03 MID-IDKOLLI-NY       PIC X(5).                                    
007300*                                 KOLLINUMMER                             
007400     03 MID-IDPSN-NY         PIC X(3).                                    
007500*                                 PROPER SHIPPING NAME                    
007600     03 MID-KDORDKL-NY       PIC X.                                       
007700*                                 ORDERKLASS                              
007800     03 MID-KDKOLLI-NY       PIC X(8).                                    
007900*                                 KOLLIKOD                                
008000     03 MID-VKORDBTO-KOLLI-NY                                             
008100                             PIC X(8).                                    
008200*                                 ORDERVIKT BRUTTO PER KOLLI              
008300*** END OF VILMAII-COPY LENGTH= 624 BYTES                                 
