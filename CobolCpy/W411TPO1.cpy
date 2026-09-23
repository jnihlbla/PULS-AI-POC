000100 01  TPO1-W411TPO1.                                                       
000200*                                 LÄNKAREA MELLAN RADPGM OCH SUB-         
000300*                                 MODUL FÖR TPO1.                         
000400     03 TPO1-INDATA.                                                      
000500*                                                                         
000600        05 TPO1-IDDISTR      PIC 9(4).                                    
000700*                                 DISTRIKTNUMMER                          
000800        05 TPO1-IDKUNDNR     PIC 9(6).                                    
000900*                                 KUNDNUMMER                              
001000        05 TPO1-IDKUNDRF     PIC X(10).                                   
001100*                                 KUNDENS REFERENS (ORDERID)              
001200        05 TPO1-IDARTNR      PIC 9(8).                                    
001300*                                 ARTIKELNUMMER                           
001400        05 TPO1-BERADREF     PIC X(10).                                   
001500*                                 KUNDENS RADREFERENS                     
001600        05 TPO1-IDANSK       PIC 9(3).                                    
001700*                                 ANSKAFFARNUMMER                         
001800        05 TPO1-IDKONTO      PIC 9(10).                                   
001900*                                 KONTO                                   
002000        05 TPO1-IDKST        PIC X(10).                                   
002100*                                 KOSTNADSSTÄLLE                          
002200        05 TPO1-IDANALYS     PIC X(12).                                   
002300*                                 ANALYSNUMMER                            
002400        05 TPO1-KDDSP        PIC 9.                                       
002500*                                 PÅVERKAN PÅ DSP                         
002600        05 TPO1-KDFAKTYP     PIC X.                                       
002700*                                 FAKTURATYP                              
002800        05 TPO1-KDFRAKT      PIC 9(2).                                    
002900*                                 FRAKTSÄTT DC TILL KUND                  
003000        05 TPO1-KDKVBRYT     PIC 9.                                       
003100*                                 KOD OM KVANTFÖRP SKALL BRYTAS           
003200        05 TPO1-KDORDING     PIC 9.                                       
003300*                                 UPPDATERING ORDERINGÅNG                 
003400        05 TPO1-KDORDKL      PIC 9.                                       
003500*                                 ORDERKLASS                              
003600        05 TPO1-KDPRODSL     PIC 9(2).                                    
003700*                                 PRODUKTSLAG                             
003800        05 TPO1-KDTPOTYP     PIC 9.                                       
003900*                                 TYP AV TIDPLANERAD ORDER                
004000        05 TPO1-KDVRINFO     PIC 9.                                       
004100*                                 PÅVERKAN I VR/DSP SYSTEM                
004200        05 TPO1-KVBEART-Q    PIC 9(6).                                    
004300*                                 BESTÄLLT KVANTANPASSAT ANTAL            
004400        05 TPO1-PRARTNTO     PIC 9(7)V9(2).                               
004500*                                 ARTIKELPRIS NETTO                       
004600        05 TPO1-REKSIFFR     PIC 9.                                       
004700*                                 KONTROLLSIFFRA                          
004800        05 TPO1-TITPO        PIC 9(6).                                    
004900*                                 PLANERAD ORDERDATUM                     
005000        05 TPO1-KDPRTYP      PIC X.                                       
005100*                                 TYP AV PRISTILLÄMPNING                  
005200        05 TPO1-BEVOLREF     PIC X(10).                                   
005300*                                 VOLVO REFERENS                          
005400        05 TPO1-FLINVEST     PIC X.                                       
005500*                                 BYTES INVENTERINGSFLAGGA                
005600        05 TPO1-FLPRTILL     PIC X.                                       
005700*                                 PRISTILLÄGGS FLAGGA                     
005800        05 TPO1-BEKUNDRF     PIC X(15).                                   
005900*                                 KUNDENS REFERENS                        
006000        05 TPO1-IDKAMPRF     PIC 9(7).                                    
006100*                                 KAMPANJREFERENS                         
006200        05 TPO1-IDLEVNR      PIC X(5).                                    
006300*                                 LEVERANTÖRNUMMER                        
006400        05 TPO1-IDSYSTEM     PIC X(4).                                    
006500*                                 VOLVO VCCS SYSTEMNUMMER                 
006600        05 TPO1-FLTPO1       PIC X.                                       
006700*                                 ARTIKELN GODKÄND FÖR TPO1               
006800        05 TPO1-KVFRYSTI     PIC 9(2).                                    
006900*                                 FRYSTID FÖR TPO-ORDER                   
007000        05 TPO1-KDORDBEH     PIC 9.                                       
007100*                                 STATUSKOD ORDERBEHANDLING               
007200        05 TPO1-FLFORBI      PIC X.                                       
007300*                                 FÖRBIORDERFLAGGA                        
007400        05 TPO1-FLORDSPE     PIC X.                                       
007500*                                 SPECIALORDERFLAGGA                      
007600        05 TPO1-FLOVRLEV     PIC X.                                       
007700*                                 ÖVERLEVERANS                            
007800        05 TPO1-KDORDTYP-LDC PIC X(2).                                    
007900*                                 ORDERTYP HOS DEALER                     
008000        05 TPO1-TIREPDAT     PIC 9(6).                                    
008100*                                 REPAIR DATE                             
008200        05 TPO1-IDKUNDRF-WIP PIC X(10).                                   
008300*                                 REPARATIONS ORDERNR, LDC KUND           
008400     03 TPO1-DEAL-PR-LINE.                                                
008500*                                 DEALERPRIS (RAD)                        
008600        05 TPO1-IDPRQUES     PIC 9(7).                                    
008700*                                 PRISFRÅGA NR                            
008800        05 TPO1-PRARTNTO-LOC PIC S9(7)V9(2)      COMP-3.                  
008900*                                 ARTIKELPRIS NETTO LOKAL VALUTA          
009000        05 TPO1-PRARTNTO-LOCPREL                                          
009100                             PIC S9(7)V9(2)      COMP-3.                  
009200*                                 PREL NETTO SLUTKUNDSPRIS I              
009300*                                 LOKAL VALUTA                            
009400        05 TPO1-PRARTBTO-LOC PIC S9(7)V9(2)      COMP-3.                  
009500*                                 PRIS I LOKAL VALUTA                     
009600        05 TPO1-KDVALISO     PIC X(3).                                    
009700*                                 VALUTAKOD ENLIGT ISO-STANDARD.          
009800        05 TPO1-KDVAT        PIC X(2).                                    
009900*                                 MOMSKOD                                 
010000        05 TPO1-RERAB        PIC S9(2)V9(1)      COMP-3.                  
010100*                                 RABATTSATS (PROCENT)                    
010200        05 TPO1-KDRAB        PIC X(5).                                    
010300*                                 RABATTKOD                               
010400        05 TPO1-BEART-VIPS   PIC X(25).                                   
010500*                                 VIPS ARTIKELBENÄMNING                   
010600*                                 PÅ DEALERNS SPRÅK                       
010700     03 TPO1-UTDATA.                                                      
010800*                                                                         
010900        05 TPO1-KDORDBEK     PIC 9(2).                                    
011000*                                 ORDERBEKRÄFTELSEKOD                     
011100        05 TPO1-FLKLAR       PIC X.                                       
011200*                                 AVSLUTNINGSMARKERING                    
011300        05 TPO1-KVANNANT     PIC 9(6).                                    
011400*                                 ANNULLERAT ANTAL ARTIKLAR               
011500*** END OF VILMAII-COPY LENGTH= 243 BYTES                                 
