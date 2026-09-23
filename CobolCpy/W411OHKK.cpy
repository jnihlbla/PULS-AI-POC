000100 01  OHKK-W411OHKK.                                                       
000200*                                 LÄNKAREA TILL W411OHKK -                
000300*                                 KONTROLL OM ORDERN OMFATTAS AV          
000400*                                 REGLER FÖR TVINGANDE TILLÄGG            
000500*                                 ELLER TPO2-LÖSNING FÖR SLATTORD         
000600*                                 ER TILL CDC                             
000700*                                                                         
000800*                                 OM TVINGANDE TILLÄGG SKALL GÖRA         
000900*                                 S KONTROLLERAS OM ORDER ATT             
001000*                                 SLÅ IHOP MED FINNS.                     
001100     03 OHKK-KDCALL          PIC S9(3)           COMP-3.                  
001200*                                 ANROPSTYP                               
001300     03 OHKK-IDSYSTEM        PIC X(4).                                    
001400*                                 VOLVO VCCS SYSTEMNUMMER                 
001500     03 OHKK-IDDISTR         PIC S9(5)           COMP-3.                  
001600*                                 DISTRIKTNUMMER                          
001700     03 OHKK-IDKUNDNR        PIC S9(7)           COMP-3.                  
001800*                                 KUNDNUMMER                              
001900     03 OHKK-IDORDNR-IN      PIC 9(7).                                    
002000*                                 ORDERNUMMER                             
002100     03 OHKK-KDORDKL         PIC S9              COMP-3.                  
002200*                                 ORDERKLASS                              
002300     03 OHKK-ADBETRAD-1      PIC X(35).                                   
002400*                                 ADRESSRAD BETALNINGSANSVARIG            
002500     03 OHKK-ADBETRAD-2      PIC X(35).                                   
002600*                                 ADRESSRAD BETALNINGSANSVARIG            
002700     03 OHKK-ADGMT.                                                       
002800*                                 GODSMOTTAGARADRESS                      
002900        05 OHKK-ADGMT-GATA   PIC X(35).                                   
003000*                                 GODSMOTTAGARADRESS GATA                 
003100        05 OHKK-ADGMT-PADR   PIC X(35).                                   
003200*                                 GODSMOTTAGARADRESS POSTADRESS           
003300        05 OHKK-ADPOST-PNRORT REDEFINES OHKK-ADGMT-PADR.                  
003400*                                 POSTNUMMER + ORT                        
003500           07 OHKK-ADPOSTNR  PIC X(10).                                   
003600*                                 POSTNUMMER I ADRESS                     
003700           07 OHKK-ADCITY    PIC X(25).                                   
003800*                                 BENÄMNING PÅ STAD                       
003900        05 OHKK-ADPOST-ORTPNR REDEFINES OHKK-ADGMT-PADR.                  
004000*                                 ORT + POSTNUMMER                        
004100           07 OHKK-ADCITY    PIC X(25).                                   
004200*                                 BENÄMNING PÅ STAD                       
004300           07 OHKK-ADPOSTNR  PIC X(10).                                   
004400*                                 POSTNUMMER I ADRESS                     
004500        05 OHKK-ADGMT-LAND   PIC X(35).                                   
004600*                                 GODSMOTTAGARADRESS LAND                 
004700     03 OHKK-BEBETRAD-1      PIC X(35).                                   
004800*                                 DEL AV BETALNINGSANSVARIGS NAMN         
004900     03 OHKK-BEBETRAD-2      PIC X(35).                                   
005000*                                 DEL AV BETALNINGSANSVARIGS NAMN         
005100     03 OHKK-BEGMRK.                                                      
005200*                                 GODSMÄRKE                               
005300        05 OHKK-BEGMRK-RAD1  PIC X(30).                                   
005400*                                 GODSMÄRKE  RAD1                         
005500        05 OHKK-BEGMRK-RAD2  PIC X(30).                                   
005600*                                 GODSMÄRKE  RAD2                         
005700     03 OHKK-BEGMT.                                                       
005800*                                 GODSMOTTAGARNAMN                        
005900        05 OHKK-BEGMT-RAD1   PIC X(35).                                   
006000*                                 GODSMOTTAGARNAMN RAD 1                  
006100        05 OHKK-BEGMT-RAD2   PIC X(35).                                   
006200*                                 GODSMOTTAGARNAMN RAD 2                  
006300     03 OHKK-FLAUTFAK        PIC X.                                       
006400*                                 AUTOMATFAKTURERING ?                    
006500     03 OHKK-FLAUTPAC        PIC X.                                       
006600*                                 AUTOMATISK PACKRAPPORTERING             
006700     03 OHKK-FLEMBORD        PIC X.                                       
006800*                                 EMBALLAGEORDER ?                        
006900     03 OHKK-FLFORBI         PIC X.                                       
007000*                                 FÖRBIORDERFLAGGA                        
007100     03 OHKK-FLLSBOK         PIC X.                                       
007200*                                 LAGERAVBOKNING                          
007300     03 OHKK-FLORDSPE        PIC X.                                       
007400*                                 SPECIALORDERFLAGGA                      
007500     03 OHKK-FLOVRLEV        PIC X.                                       
007600*                                 ÖVERLEVERANS                            
007700     03 OHKK-FLPRELRO        PIC X.                                       
007800*                                 PRELIMINÄR RESTORDERFLAGGA              
007900     03 OHKK-FLPRERS         PIC X.                                       
008000*                                 PRISERSÄTTNINGSFLAGGA                   
008100     03 OHKK-FLRESTN         PIC X.                                       
008200*                                 RESTNOTERING ?                          
008300     03 OHKK-FLVORKO         PIC X.                                       
008400*                                 VOR-KÖ FLAGGA                           
008500     03 OHKK-IDANALYS        PIC X(12).                                   
008600*                                 ANALYSNUMMER                            
008700     03 OHKK-IDBIPREF        PIC X(7).                                    
008800*                                 BIPACKNINGSREFERENS                     
008900     03 OHKK-IDDC-TVS        PIC X(2).                                    
009000*                                 DISTRIBUTIONCENTER                      
009100*                                 TVÅNGSSTYRNING                          
009200     03 OHKK-IDDEPOT         PIC X(2).                                    
009300*                                 TRANSPORT DEPOT                         
009400     03 OHKK-IDDEPT          PIC 9(2).                                    
009500*                                 AVDELNING I VERKSTAD                    
009600     03 OHKK-IDFTG           PIC 9(2).                                    
009700*                                 FÖRETAGSID EKONOM REDOVISNING           
009800     03 OHKK-IDKAMPRF        PIC S9(7)           COMP-3.                  
009900*                                 KAMPANJREFERENS                         
010000     03 OHKK-IDKONTO         PIC S9(11)          COMP-3.                  
010100*                                 KONTO                                   
010200     03 OHKK-IDKST           PIC X(10).                                   
010300*                                 KOSTNADSSTÄLLE                          
010400     03 OHKK-IDRFTAB         PIC X(3).                                    
010500*                                 RANSONERINGSFAKTORTABELL                
010600     03 OHKK-IDROUTE         PIC X.                                       
010700*                                 TRANSPORT ROUTE                         
010800     03 OHKK-IDSKYLT         PIC X(3).                                    
010900*                                 NATIONALITETSTECKEN                     
011000*                                 SPRÅKIDENTIFIKATION                     
011100     03 OHKK-IDZON           PIC X(2).                                    
011200*                                 TRANSPORTVÄG (RUTT,ZON)                 
011300     03 OHKK-KDFAKTYP        PIC X.                                       
011400*                                 FAKTURATYP                              
011500     03 OHKK-KDFRAKT         PIC S9(3)           COMP-3.                  
011600*                                 FRAKTSÄTT DC TILL KUND                  
011700     03 OHKK-KDORDING        PIC S9              COMP-3.                  
011800*                                 UPPDATERING ORDERINGÅNG                 
011900     03 OHKK-KDVRINFO        PIC S9              COMP-3.                  
012000*                                 PÅVERKAN I VR/DSP SYSTEM                
012100     03 OHKK-KDTPOTYP        PIC S9              COMP-3.                  
012200*                                 TYP AV TIDPLANERAD ORDER                
012300     03 OHKK-KDTULLVE        PIC S9              COMP-3.                  
012400*                                 TYP AV PRIS PÅ TULLFAKTURA              
012500     03 OHKK-KVDAGAR-DOW     PIC S9(3)           COMP-3.                  
012600*                                 ANTAL DAGAR FÖRE DC CLEARING            
012700     03 OHKK-RESLATT         PIC S9(3)           COMP-3.                  
012800*                                 SLATTGRÄNS                              
012900     03 OHKK-TITPO           PIC S9(7)           COMP-3.                  
013000*                                 PLANERAD ORDERDATUM                     
013100     03 OHKK-FLORDTIL        PIC X.                                       
013200*                                 TVINGANDE TILÄGG ORDER                  
013300     03 OHKK-FILLERX2        PIC X(2).                                    
013400     03 OHKK-IDORDER         PIC S9(7)           COMP-3.                  
013500*                                 VOLVO PARTS ORDERNUMMER                 
013600     03 OHKK-IDORDNR-UT      PIC 9(7).                                    
013700*                                 ORDERNUMMER                             
013800     03 OHKK-KDFRAKT-DEF     PIC S9(3)           COMP-3.                  
013900*                                 FRAKTSÄTT DC TILL KUND                  
014000     03 OHKK-FLSOFT          PIC X.                                       
014100*                                 FLAGGA SOFTVARA                         
014200*** END OF VILMAII-COPY LENGTH= 495 BYTES                                 
