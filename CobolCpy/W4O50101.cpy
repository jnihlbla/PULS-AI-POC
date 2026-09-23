000100 01  MOD-W4O50101.                                                        
000200*                                 COPYTEXT FÖR MOD W4050101               
000300*                                                                         
000400     03 MOD-IDTRANS          PIC X(4).                                    
000500*                                 BILDNUMMER                              
000600     03 MOD-TEMFSFEL         PIC X(40).                                   
000700*                                 MFS FELMEDDELANDE                       
000800     03 MOD-IDDISTR-IN       PIC X(4).                                    
000900*                                 DISTRIKTNUMMER                          
001000     03 MOD-IDDISTR-UT       PIC X(4).                                    
001100*                                 DISTRIKTNUMMER                          
001200     03 MOD-IDKUNDNR-IN      PIC X(6).                                    
001300*                                 KUNDNUMMER                              
001400     03 MOD-IDKUNDNR-UT      PIC X(6).                                    
001500*                                 KUNDNUMMER                              
001600     03 MOD-IDKUNDRF-IN      PIC X(7).                                    
001700*                                 ORDERNUMMER                             
001800     03 MOD-IDKUNDRF-UT      PIC X(7).                                    
001900*                                 ORDERNUMMER                             
002000     03 MOD-IDARTNR-IN       PIC X(9).                                    
002100*                                 ARTIKELNUMMER                           
002200     03 MOD-IDARTNR-UT       PIC X(9).                                    
002300*                                 ARTIKELNUMMER                           
002400     03 MOD-IDKOLLI-IN       PIC X(5).                                    
002500*                                 KOLLINUMMER                             
002600     03 MOD-IDKOLLI-UT       PIC X(5).                                    
002700*                                 KOLLINUMMER                             
002800     03 MOD-IDPRODNR-IN      PIC X(7).                                    
002900*                                 PRODUKTIONSNUMMER                       
003000     03 MOD-IDPRODNR-UT      PIC X(7).                                    
003100*                                 PRODUKTIONSNUMMER                       
003200     03 MOD-IDDC-IN          PIC X(2).                                    
003300*                                 IDENTIFIERARE LAGER                     
003400     03 MOD-IDDC-UT          PIC X(2).                                    
003500*                                 IDENTIFIERARE LAGER                     
003600     03 MOD-TEDDI            PIC X(11).                                   
003700*                                 TEXTFÄLT DDI                            
003800     03 MOD-IDDC-RAD         OCCURS 13 TIMES                              
003900                             PIC X(2).                                    
004000*                                 IDENTIFIERARE LAGER                     
004100     03 MOD-IDPRODNR-RAD     OCCURS 13 TIMES                              
004200                             PIC Z(6)9.                                   
004300*                                 PRODUKTIONSNUMMER                       
004400     03 MOD-IDLEVNR-RAD      OCCURS 13 TIMES                              
004500                             PIC X(5).                                    
004600*                                 LEVERANTÖRNUMMER                        
004700     03 MOD-TISKEPPN-DDC     OCCURS 13 TIMES                              
004800                             PIC 9(6).                                    
004900*                                 SKEPPNINGSDATUM  (ÅÅMMDD)               
005000     03 MOD-KDORDKL          PIC 9.                                       
005100*                                 ORDERKLASS                              
005200     03 MOD-KDFRAKT          PIC Z9.                                      
005300*                                 FRAKTSÄTT DC TILL KUND                  
005400     03 MOD-KDFAKTYP         PIC X.                                       
005500*                                 FAKTURATYP                              
005600     03 MOD-IDDC-EXP         PIC X(2).                                    
005700*                                 DC FÖR STUDS FLÖDE VID EXPORT           
005800     03 MOD-KONTO            PIC X(7).                                    
005900     03 MOD-IDKONTO          PIC Z(9)9.                                   
006000*                                 KONTO                                   
006100     03 MOD-ANALYS           PIC X(9).                                    
006200     03 MOD-IDANALYS         PIC X(12).                                   
006300*                                 ANALYSNUMMER                            
006400     03 MOD-KST              PIC X(2).                                    
006500     03 MOD-IDKST            PIC X(10).                                   
006600*                                 KOSTNADSSTÄLLE                          
006700     03 MOD-BEGMT-RAD1       PIC X(35).                                   
006800*                                 GODSMOTTAGARNAMN RAD 1                  
006900     03 MOD-BEGMT-RAD2       PIC X(35).                                   
007000*                                 GODSMOTTAGARNAMN RAD 2                  
007100     03 MOD-ADGMT-GATA       PIC X(35).                                   
007200*                                 GODSMOTTAGARADRESS GATA                 
007300     03 MOD-ADGMT-PADR       PIC X(35).                                   
007400*                                 GODSMOTTAGARADRESS POSTADRESS           
007500     03 MOD-ADGMT-LAND       PIC X(35).                                   
007600*                                 GODSMOTTAGARADRESS LAND                 
007700     03 MOD-TIAAMMDD         PIC 9(6).                                    
007800*                                 ÅR - MÅNAD - DAG  (ÅÅMMDD)              
007900     03 MOD-TIHHMM           PIC X(5).                                    
008000*                                 KLOCKSLAG (TIMMAR OCH MINUTER)          
008100     03 MOD-IDTRP.                                                        
008200*                                 TRANSPORTIDENTITET                      
008300        05 MOD-IDTRPLOS      PIC X(3).                                    
008400*                                 TRANSPORTLÖSNING                        
008500        05 MOD-IDTRPVAR      PIC X(2).                                    
008600*                                 TRANSPORTLÖSNINGSGRUPP                  
008700     03 MOD-IDSYSTEM         PIC X(4).                                    
008800*                                 VOLVO VCCS SYSTEMNUMMER                 
008900     03 MOD-INTERN           PIC X(22).                                   
009000     03 MOD-IDORDER          PIC Z(6)9.                                   
009100*                                 VOLVO PARTS ORDERNUMMER                 
009200     03 MOD-KVRADER          PIC Z(4)9.                                   
009300*                                 ANTAL RADER                             
009400     03 MOD-KVKOLPAC         PIC Z(3)9.                                   
009500*                                 ANTAL PACK RAPPORTERADE KOLLI           
009600     03 MOD-VKORDNTO         PIC Z(5)9.9.                                 
009700*                                 ORDERVIKT NETTO (KG)                    
009800     03 MOD-VKORDBTO         PIC Z(5)9.9.                                 
009900*                                 ORDERVIKT BRUTTO (KG)                   
010000     03 MOD-KVORDRAD-PACK    PIC Z(4)9.                                   
010100*                                 ANTAL PACKADE ORDERRADER                
010200     03 MOD-KVKOLLI-LAST     PIC Z(3)9.                                   
010300*                                 ANTAL LASTNINGSRAPPORTERADE             
010400*                                 KOLLIN                                  
010500     03 MOD-VLORDNTO         PIC Z(3)9.9(3).                              
010600*                                 ORDERVOLYM NETTO (M3)                   
010700     03 MOD-VLORDBTO         PIC Z(3)9.9(3).                              
010800*                                 ORDERVOLYM BRUTTO (M3)                  
010900     03 MOD-SUORDV           PIC Z(8)9.9(2).                              
011000*                                 SUMMA ORDERVÄRDE                        
011100     03 MOD-TEASTRIX         PIC X.                                       
011200*                                 ASTERISK                                
011300     03 MOD-KVKOLLI-FAKT     PIC Z(3)9.                                   
011400*                                 ANTAL FAKTURERADE KOLLIN                
011500     03 MOD-TEMFSINF         PIC X(55).                                   
011600*                                 INFORMATIONSMEDDELANDE                  
011700*** END OF VILMAII-COPY LENGTH= 797 BYTES                                 
