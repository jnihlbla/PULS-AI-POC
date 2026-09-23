000100 01  KAMP-W411KAMP.                                                       
000200*                                 LÄNKAREA MELLAN RADPGM OCH SUB-         
000300*                                 MODUL FÖR TPO4.                         
000400     03 KAMP-INDATA.                                                      
000500*                                                                         
000600        05 KAMP-IDDC         PIC X(2).                                    
000700*                                 IDENTIFIERARE LAGER                     
000800        05 KAMP-IDDISTR      PIC 9(4).                                    
000900*                                 DISTRIKTNUMMER                          
001000        05 KAMP-IDKUNDNR     PIC 9(6).                                    
001100*                                 KUNDNUMMER                              
001200        05 KAMP-IDKUNDRF     PIC X(10).                                   
001300*                                 KUNDENS REFERENS (ORDERID)              
001400        05 KAMP-IDARTNR      PIC 9(8).                                    
001500*                                 ARTIKELNUMMER                           
001600        05 KAMP-BERADREF     PIC X(10).                                   
001700*                                 KUNDENS RADREFERENS                     
001800        05 KAMP-IDANSK       PIC 9(3).                                    
001900*                                 ANSKAFFARNUMMER                         
002000        05 KAMP-IDKONTO      PIC 9(10).                                   
002100*                                 KONTO                                   
002200        05 KAMP-IDANALYS     PIC X(12).                                   
002300*                                 ANALYSNUMMER                            
002400        05 KAMP-IDKST        PIC X(10).                                   
002500*                                 KOSTNADSSTÄLLE                          
002600        05 KAMP-KDDSP        PIC 9.                                       
002700*                                 PÅVERKAN PÅ DSP                         
002800        05 KAMP-KDFAKTYP     PIC X.                                       
002900*                                 FAKTURATYP                              
003000        05 KAMP-KDFRAKT      PIC 9(2).                                    
003100*                                 FRAKTSÄTT DC TILL KUND                  
003200        05 KAMP-KDKVBRYT     PIC 9.                                       
003300*                                 KOD OM KVANTFÖRP SKALL BRYTAS           
003400        05 KAMP-KDORDING     PIC 9.                                       
003500*                                 UPPDATERING ORDERINGÅNG                 
003600        05 KAMP-KDORDKL      PIC 9.                                       
003700*                                 ORDERKLASS                              
003800        05 KAMP-KDPRODSL     PIC 9(2).                                    
003900*                                 PRODUKTSLAG                             
004000        05 KAMP-KDVRINFO     PIC 9.                                       
004100*                                 PÅVERKAN I VR/DSP SYSTEM                
004200        05 KAMP-KVBEART-Q    PIC 9(6).                                    
004300*                                 BESTÄLLT KVANTANPASSAT ANTAL            
004400        05 KAMP-PRARTNTO     PIC 9(7)V9(2).                               
004500*                                 ARTIKELPRIS NETTO                       
004600        05 KAMP-REKSIFFR     PIC 9.                                       
004700*                                 KONTROLLSIFFRA                          
004800        05 KAMP-TITPO        PIC 9(6).                                    
004900*                                 PLANERAD ORDERDATUM                     
005000        05 KAMP-KDPRTYP      PIC X.                                       
005100*                                 TYP AV PRISTILLÄMPNING                  
005200        05 KAMP-BEVOLREF     PIC X(10).                                   
005300*                                 VOLVO REFERENS                          
005400        05 KAMP-FLINVEST     PIC X.                                       
005500*                                 BYTES INVENTERINGSFLAGGA                
005600        05 KAMP-FLPRTILL     PIC X.                                       
005700*                                 PRISTILLÄGGS FLAGGA                     
005800        05 KAMP-BEKUNDRF     PIC X(15).                                   
005900*                                 KUNDENS REFERENS                        
006000        05 KAMP-IDKAMPRF     PIC 9(7).                                    
006100*                                 KAMPANJREFERENS                         
006200        05 KAMP-IDLEVNR      PIC X(5).                                    
006300*                                 LEVERANTÖRNUMMER                        
006400        05 KAMP-IDSYSTEM     PIC X(4).                                    
006500*                                 VOLVO VCCS SYSTEMNUMMER                 
006600        05 KAMP-KVFRYSTI     PIC 9(2).                                    
006700*                                 FRYSTID FÖR TPO-ORDER                   
006800        05 KAMP-KDTPOTYP     PIC 9.                                       
006900*                                 TYP AV TIDPLANERAD ORDER                
007000        05 KAMP-FLFORBI      PIC X.                                       
007100*                                 FÖRBIORDERFLAGGA                        
007200        05 KAMP-FLORDSPE     PIC X.                                       
007300*                                 SPECIALORDERFLAGGA                      
007400        05 KAMP-FLOVRLEV     PIC X.                                       
007500*                                 ÖVERLEVERANS                            
007600     03 KAMP-DEAL-PR-LINE.                                                
007700*                                 DEALERPRIS (RAD)                        
007800        05 KAMP-IDPRQUES     PIC 9(7).                                    
007900*                                 PRISFRÅGA NR                            
008000        05 KAMP-PRARTNTO-LOC PIC S9(7)V9(2)      COMP-3.                  
008100*                                 ARTIKELPRIS NETTO LOKAL VALUTA          
008200        05 KAMP-PRARTNTO-LOCPREL                                          
008300                             PIC S9(7)V9(2)      COMP-3.                  
008400*                                 PREL NETTO SLUTKUNDSPRIS I              
008500*                                 LOKAL VALUTA                            
008600        05 KAMP-PRARTBTO-LOC PIC S9(7)V9(2)      COMP-3.                  
008700*                                 PRIS I LOKAL VALUTA                     
008800        05 KAMP-KDVALISO     PIC X(3).                                    
008900*                                 VALUTAKOD ENLIGT ISO-STANDARD.          
009000        05 KAMP-KDVAT        PIC X(2).                                    
009100*                                 MOMSKOD                                 
009200        05 KAMP-RERAB        PIC S9(2)V9(1)      COMP-3.                  
009300*                                 RABATTSATS (PROCENT)                    
009400        05 KAMP-KDRAB        PIC X(5).                                    
009500*                                 RABATTKOD                               
009600        05 KAMP-BEART-VIPS   PIC X(25).                                   
009700*                                 VIPS ARTIKELBENÄMNING                   
009800*                                 PÅ DEALERNS SPRÅK                       
009900     03 KAMP-KDORDBEK        PIC 9(2).                                    
010000*                                 ORDERBEKRÄFTELSEKOD                     
010100     03 KAMP-FLKLAR          PIC X.                                       
010200*                                 AVSLUTNINGSMARKERING                    
010300*** END OF VILMAII-COPY LENGTH= 219 BYTES                                 
