000100 01  TPO3-W411TPO3.                                                       
000200*                                 LÄNKAREA MELLAN RADPGM OCH SUB-         
000300*                                 MODUL FÖR TPO3.                         
000400     03 TPO3-INDATA.                                                      
000500*                                                                         
000600        05 TPO3-IDDISTR      PIC 9(4).                                    
000700*                                 DISTRIKTNUMMER                          
000800        05 TPO3-IDKUNDNR     PIC 9(6).                                    
000900*                                 KUNDNUMMER                              
001000        05 TPO3-IDKUNDRF     PIC X(10).                                   
001100*                                 KUNDENS REFERENS (ORDERID)              
001200        05 TPO3-IDARTNR      PIC 9(8).                                    
001300*                                 ARTIKELNUMMER                           
001400        05 TPO3-BERADREF     PIC X(10).                                   
001500*                                 KUNDENS RADREFERENS                     
001600        05 TPO3-IDANSK       PIC 9(3).                                    
001700*                                 ANSKAFFARNUMMER                         
001800        05 TPO3-IDKONTO      PIC 9(10).                                   
001900*                                 KONTO                                   
002000        05 TPO3-IDKST        PIC X(10).                                   
002100*                                 KOSTNADSSTÄLLE                          
002200        05 TPO3-IDANALYS     PIC X(12).                                   
002300*                                 ANALYSNUMMER                            
002400        05 TPO3-KDDSP        PIC 9.                                       
002500*                                 PÅVERKAN PÅ DSP                         
002600        05 TPO3-KDFAKTYP     PIC X.                                       
002700*                                 FAKTURATYP                              
002800        05 TPO3-KDFRAKT      PIC 9(2).                                    
002900*                                 FRAKTSÄTT DC TILL KUND                  
003000        05 TPO3-KDKVBRYT     PIC 9.                                       
003100*                                 KOD OM KVANTFÖRP SKALL BRYTAS           
003200        05 TPO3-KDORDING     PIC 9.                                       
003300*                                 UPPDATERING ORDERINGÅNG                 
003400        05 TPO3-KDORDKL      PIC 9.                                       
003500*                                 ORDERKLASS                              
003600        05 TPO3-KDPRODSL     PIC 9(2).                                    
003700*                                 PRODUKTSLAG                             
003800        05 TPO3-KDVRINFO     PIC 9.                                       
003900*                                 PÅVERKAN I VR/DSP SYSTEM                
004000        05 TPO3-KVBEART-Q    PIC 9(6).                                    
004100*                                 BESTÄLLT KVANTANPASSAT ANTAL            
004200        05 TPO3-PRARTNTO     PIC 9(7)V9(2).                               
004300*                                 ARTIKELPRIS NETTO                       
004400        05 TPO3-REKSIFFR     PIC 9.                                       
004500*                                 KONTROLLSIFFRA                          
004600        05 TPO3-TITPO        PIC 9(6).                                    
004700*                                 PLANERAD ORDERDATUM                     
004800        05 TPO3-KDPRTYP      PIC X.                                       
004900*                                 TYP AV PRISTILLÄMPNING                  
005000        05 TPO3-BEVOLREF     PIC X(10).                                   
005100*                                 VOLVO REFERENS                          
005200        05 TPO3-FLINVEST     PIC X.                                       
005300*                                 BYTES INVENTERINGSFLAGGA                
005400        05 TPO3-FLPRTILL     PIC X.                                       
005500*                                 PRISTILLÄGGS FLAGGA                     
005600        05 TPO3-BEKUNDRF     PIC X(15).                                   
005700*                                 KUNDENS REFERENS                        
005800        05 TPO3-IDKAMPRF     PIC 9(7).                                    
005900*                                 KAMPANJREFERENS                         
006000        05 TPO3-IDLEVNR      PIC X(5).                                    
006100*                                 LEVERANTÖRNUMMER                        
006200        05 TPO3-IDSYSTEM     PIC X(4).                                    
006300*                                 VOLVO VCCS SYSTEMNUMMER                 
006400        05 TPO3-KDTPOTYP     PIC 9.                                       
006500*                                 TYP AV TIDPLANERAD ORDER                
006600        05 TPO3-FLFORBI      PIC X.                                       
006700*                                 FÖRBIORDERFLAGGA                        
006800        05 TPO3-KDORDTYP-LDC PIC X(2).                                    
006900*                                 ORDERTYP HOS DEALER                     
007000        05 TPO3-TIREPDAT     PIC 9(6).                                    
007100*                                 REPAIR DATE                             
007200        05 TPO3-IDKUNDRF-WIP PIC X(10).                                   
007300*                                 REPARATIONS ORDERNR, LDC KUND           
007400     03 TPO3-DEAL-PR-LINE.                                                
007500*                                 DEALERPRIS (RAD)                        
007600        05 TPO3-IDPRQUES     PIC 9(7).                                    
007700*                                 PRISFRÅGA NR                            
007800        05 TPO3-PRARTNTO-LOC PIC S9(7)V9(2)      COMP-3.                  
007900*                                 ARTIKELPRIS NETTO LOKAL VALUTA          
008000        05 TPO3-PRARTNTO-LOCPREL                                          
008100                             PIC S9(7)V9(2)      COMP-3.                  
008200*                                 PREL NETTO SLUTKUNDSPRIS I              
008300*                                 LOKAL VALUTA                            
008400        05 TPO3-PRARTBTO-LOC PIC S9(7)V9(2)      COMP-3.                  
008500*                                 PRIS I LOKAL VALUTA                     
008600        05 TPO3-KDVALISO     PIC X(3).                                    
008700*                                 VALUTAKOD ENLIGT ISO-STANDARD.          
008800        05 TPO3-KDVAT        PIC X(2).                                    
008900*                                 MOMSKOD                                 
009000        05 TPO3-RERAB        PIC S9(2)V9(1)      COMP-3.                  
009100*                                 RABATTSATS (PROCENT)                    
009200        05 TPO3-KDRAB        PIC X(5).                                    
009300*                                 RABATTKOD                               
009400        05 TPO3-BEART-VIPS   PIC X(25).                                   
009500*                                 VIPS ARTIKELBENÄMNING                   
009600*                                 PÅ DEALERNS SPRÅK                       
009700     03 TPO3-KDORDBEK        PIC 9(2).                                    
009800*                                 ORDERBEKRÄFTELSEKOD                     
009900     03 TPO3-FLKLAR          PIC X.                                       
010000*                                 AVSLUTNINGSMARKERING                    
010100*** END OF VILMAII-COPY LENGTH= 231 BYTES                                 
