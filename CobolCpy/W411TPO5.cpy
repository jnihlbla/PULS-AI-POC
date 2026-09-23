000100 01  TPO5-W411TPO5.                                                       
000200*                                 LÄNKAREA MELLAN RADPGM OCH SUB-         
000300*                                 MODUL FÖR TPO5.                         
000400     03 TPO5-INDATA.                                                      
000500*                                                                         
000600        05 TPO5-IDDISTR      PIC 9(4).                                    
000700*                                 DISTRIKTNUMMER                          
000800        05 TPO5-IDKUNDNR     PIC 9(6).                                    
000900*                                 KUNDNUMMER                              
001000        05 TPO5-IDKUNDRF     PIC X(10).                                   
001100*                                 KUNDENS REFERENS (ORDERID)              
001200        05 TPO5-IDARTNR      PIC 9(8).                                    
001300*                                 ARTIKELNUMMER                           
001400        05 TPO5-BERADREF     PIC X(10).                                   
001500*                                 KUNDENS RADREFERENS                     
001600        05 TPO5-IDANSK       PIC 9(3).                                    
001700*                                 ANSKAFFARNUMMER                         
001800        05 TPO5-IDKONTO      PIC 9(10).                                   
001900*                                 KONTO                                   
002000        05 TPO5-IDKST        PIC X(10).                                   
002100*                                 KOSTNADSSTÄLLE                          
002200        05 TPO5-IDANALYS     PIC X(12).                                   
002300*                                 ANALYSNUMMER                            
002400        05 TPO5-KDDSP        PIC 9.                                       
002500*                                 PÅVERKAN PÅ DSP                         
002600        05 TPO5-KDFAKTYP     PIC X.                                       
002700*                                 FAKTURATYP                              
002800        05 TPO5-KDFRAKT      PIC 9(2).                                    
002900*                                 FRAKTSÄTT DC TILL KUND                  
003000        05 TPO5-KDKVBRYT     PIC 9.                                       
003100*                                 KOD OM KVANTFÖRP SKALL BRYTAS           
003200        05 TPO5-KDORDING     PIC 9.                                       
003300*                                 UPPDATERING ORDERINGÅNG                 
003400        05 TPO5-KDORDKL      PIC 9.                                       
003500*                                 ORDERKLASS                              
003600        05 TPO5-KDPRODSL     PIC 9(2).                                    
003700*                                 PRODUKTSLAG                             
003800        05 TPO5-KDTPOTYP     PIC 9.                                       
003900*                                 TYP AV TIDPLANERAD ORDER                
004000        05 TPO5-KDVRINFO     PIC 9.                                       
004100*                                 PÅVERKAN I VR/DSP SYSTEM                
004200        05 TPO5-KVBEART-Q    PIC 9(6).                                    
004300*                                 BESTÄLLT KVANTANPASSAT ANTAL            
004400        05 TPO5-PRARTNTO     PIC 9(7)V9(2).                               
004500*                                 ARTIKELPRIS NETTO                       
004600        05 TPO5-REKSIFFR     PIC 9.                                       
004700*                                 KONTROLLSIFFRA                          
004800        05 TPO5-TITPO        PIC 9(6).                                    
004900*                                 PLANERAD ORDERDATUM                     
005000        05 TPO5-KDPRTYP      PIC X.                                       
005100*                                 TYP AV PRISTILLÄMPNING                  
005200        05 TPO5-BEVOLREF     PIC X(10).                                   
005300*                                 VOLVO REFERENS                          
005400        05 TPO5-FLINVEST     PIC X.                                       
005500*                                 BYTES INVENTERINGSFLAGGA                
005600        05 TPO5-FLPRTILL     PIC X.                                       
005700*                                 PRISTILLÄGGS FLAGGA                     
005800        05 TPO5-BEKUNDRF     PIC X(15).                                   
005900*                                 KUNDENS REFERENS                        
006000        05 TPO5-IDKAMPRF     PIC 9(7).                                    
006100*                                 KAMPANJREFERENS                         
006200        05 TPO5-IDLEVNR      PIC X(5).                                    
006300*                                 LEVERANTÖRNUMMER                        
006400        05 TPO5-IDSYSTEM     PIC X(4).                                    
006500*                                 VOLVO VCCS SYSTEMNUMMER                 
006600        05 TPO5-KVFRYSTI     PIC 9(2).                                    
006700*                                 FRYSTID FÖR TPO-ORDER                   
006800        05 TPO5-KDORDTYP-LDC PIC X(2).                                    
006900*                                 ORDERTYP HOS DEALER                     
007000        05 TPO5-TIREPDAT     PIC 9(6).                                    
007100*                                 REPAIR DATE                             
007200        05 TPO5-IDKUNDRF-WIP PIC X(10).                                   
007300*                                 REPARATIONS ORDERNR, LDC KUND           
007400     03 TPO5-DEAL-PR-LINE.                                                
007500*                                 DEALERPRIS (RAD)                        
007600        05 TPO5-IDPRQUES     PIC 9(7).                                    
007700*                                 PRISFRÅGA NR                            
007800        05 TPO5-PRARTNTO-LOC PIC S9(7)V9(2)      COMP-3.                  
007900*                                 ARTIKELPRIS NETTO LOKAL VALUTA          
008000        05 TPO5-PRARTNTO-LOCPREL                                          
008100                             PIC S9(7)V9(2)      COMP-3.                  
008200*                                 PREL NETTO SLUTKUNDSPRIS I              
008300*                                 LOKAL VALUTA                            
008400        05 TPO5-PRARTBTO-LOC PIC S9(7)V9(2)      COMP-3.                  
008500*                                 PRIS I LOKAL VALUTA                     
008600        05 TPO5-KDVALISO     PIC X(3).                                    
008700*                                 VALUTAKOD ENLIGT ISO-STANDARD.          
008800        05 TPO5-KDVAT        PIC X(2).                                    
008900*                                 MOMSKOD                                 
009000        05 TPO5-RERAB        PIC S9(2)V9(1)      COMP-3.                  
009100*                                 RABATTSATS (PROCENT)                    
009200        05 TPO5-KDRAB        PIC X(5).                                    
009300*                                 RABATTKOD                               
009400        05 TPO5-BEART-VIPS   PIC X(25).                                   
009500*                                 VIPS ARTIKELBENÄMNING                   
009600*                                 PÅ DEALERNS SPRÅK                       
009700     03 TPO5-UTDATA.                                                      
009800*                                                                         
009900        05 TPO5-KDORDBEK     PIC 9(2).                                    
010000*                                 ORDERBEKRÄFTELSEKOD                     
010100        05 TPO5-FLKLAR       PIC X.                                       
010200*                                 AVSLUTNINGSMARKERING                    
010300*** END OF VILMAII-COPY LENGTH= 232 BYTES                                 
