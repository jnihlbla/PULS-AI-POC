000100 01  RESP-WL0140O1.                                                       
000200*                                 RESPONS FROM PGM WL0140                 
000300*                                                                         
000400     03 RESP-IDDC-KEY        PIC X(2).                                    
000500*                                 IDENTIFIERARE LAGER                     
000600     03 RESP-IDDISTR-KEY     PIC Z(3)9.                                   
000700*                                 DISTRIKTNUMMER                          
000800     03 RESP-IDKUNDNR-KEY    PIC Z(5)9.                                   
000900*                                 KUNDNUMMER                              
001000     03 RESP-IDKUNDRF-KEY    PIC Z(6)9.                                   
001100*                                 ORDERNUMMER                             
001200     03 RESP-IDARTNR-KEY     PIC Z(7)9.                                   
001300*                                 ARTIKELNUMMER                           
001400     03 RESP-IDKOLLI-KEY     PIC Z(4)9.                                   
001500*                                 KOLLINUMMER                             
001600     03 RESP-IDPRODNR-KEY    PIC Z(6)9.                                   
001700*                                 PRODUKTIONSNUMMER                       
001800     03 RESP-TEDDI           PIC X(11).                                   
001900*                                 TEXTFÄLT DDI                            
002000     03 RESP-KDORDKL         PIC 9.                                       
002100*                                 ORDERKLASS                              
002200     03 RESP-KDFRAKT         PIC Z9.                                      
002300*                                 FRAKTSÄTT DC TILL KUND                  
002400     03 RESP-KDFAKTYP        PIC X.                                       
002500*                                 FAKTURATYP                              
002600     03 RESP-IDKONTO         PIC Z(9)9.                                   
002700*                                 KONTO                                   
002800     03 RESP-IDANALYS        PIC X(12).                                   
002900*                                 ANALYSNUMMER                            
003000     03 RESP-IDKST           PIC X(10).                                   
003100*                                 KOSTNADSSTÄLLE                          
003200     03 RESP-BEGMT-RAD1      PIC X(35).                                   
003300*                                 GODSMOTTAGARNAMN RAD 1                  
003400     03 RESP-BEGMT-RAD2      PIC X(35).                                   
003500*                                 GODSMOTTAGARNAMN RAD 2                  
003600     03 RESP-ADGMT-GATA      PIC X(35).                                   
003700*                                 GODSMOTTAGARADRESS GATA                 
003800     03 RESP-ADGMT-PADR      PIC X(35).                                   
003900*                                 GODSMOTTAGARADRESS POSTADRESS           
004000     03 RESP-ADGMT-LAND      PIC X(35).                                   
004100*                                 GODSMOTTAGARADRESS LAND                 
004200     03 RESP-TIAAMMDD        PIC 9(6).                                    
004300*                                 ÅR - MÅNAD - DAG  (ÅÅMMDD)              
004400     03 RESP-TIHHMM          PIC 9(4).                                    
004500*                                 KLOCKSLAG (TIMMAR OCH MINUTER)          
004600     03 RESP-IDTRP.                                                       
004700*                                 TRANSPORTIDENTITET                      
004800        05 RESP-IDTRPLOS     PIC X(3).                                    
004900*                                 TRANSPORTLÖSNING                        
005000        05 RESP-IDTRPVAR     PIC X(2).                                    
005100*                                 TRANSPORTLÖSNINGSGRUPP                  
005200     03 RESP-IDSYSTEM        PIC X(4).                                    
005300*                                 VOLVO VCCS SYSTEMNUMMER                 
005400     03 RESP-INTERN          PIC X(22).                                   
005500     03 RESP-IDORDER         PIC Z(6)9.                                   
005600*                                 VOLVO PARTS ORDERNUMMER                 
005700     03 RESP-KVORDRAD        PIC Z(4)9.                                   
005800*                                 ANTAL ORDERRADER                        
005900     03 RESP-KVKOLPAC        PIC Z(3)9.                                   
006000*                                 ANTAL PACK RAPPORTERADE KOLLI           
006100     03 RESP-VKORDNTO        PIC Z(5)9.9.                                 
006200*                                 ORDERVIKT NETTO (KG)                    
006300     03 RESP-VKORDBTO        PIC Z(5)9.9.                                 
006400*                                 ORDERVIKT BRUTTO (KG)                   
006500     03 RESP-KVORDRAD-PACK   PIC Z(4)9.                                   
006600*                                 ANTAL PACKADE ORDERRADER                
006700     03 RESP-KVKOLLI-LAST    PIC Z(3)9.                                   
006800*                                 ANTAL LASTNINGSRAPPORTERADE             
006900*                                 KOLLIN                                  
007000     03 RESP-VLORDNTO        PIC Z(3)9.9(3).                              
007100*                                 ORDERVOLYM NETTO (M3)                   
007200     03 RESP-VLORDBTO        PIC Z(3)9.9(3).                              
007300*                                 ORDERVOLYM BRUTTO (M3)                  
007400     03 RESP-SUORDV          PIC Z(8)9.9(2).                              
007500*                                 SUMMA ORDERVÄRDE                        
007600     03 RESP-TEASTRIX        PIC X.                                       
007700*                                 ASTERISK                                
007800     03 RESP-KVKOLLI-FAKT    PIC Z(3)9.                                   
007900*                                 ANTAL FAKTURERADE KOLLIN                
008000     03 RESP-KVRADER         PIC Z(4)9.                                   
008100*                                 ANTAL RADER                             
008200     03 RESP-RAD             OCCURS 8 TIMES.                              
008300*                                 RAD                                     
008400        05 RESP-IDDC-RAD     PIC X(2).                                    
008500*                                 IDENTIFIERARE LAGER                     
008600        05 RESP-IDPRODNR-RAD PIC Z(6)9.                                   
008700*                                 PRODUKTIONSNUMMER                       
008800        05 RESP-IDLEVNR-RAD  PIC X(5).                                    
008900*                                 LEVERANTÖRNUMMER                        
009000        05 RESP-TISKEPPN-DDC PIC Z(6).                                    
009100*                                 SKEPPNINGSDATUM  (ÅÅMMDD)               
009200*** END OF VILMAII-COPY LENGTH= 541 BYTES                                 
