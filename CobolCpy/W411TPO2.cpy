000100 01  TPO2-W411TPO2.                                                       
000200*                                 LÄNKAREA MELLAN RADPGM OCH SUB-         
000300*                                 MODUL FÖR TPO2.                         
000400     03 TPO2-INDATA.                                                      
000500*                                                                         
000600        05 TPO2-IDDISTR      PIC 9(4).                                    
000700*                                 DISTRIKTNUMMER                          
000800        05 TPO2-IDKUNDNR     PIC 9(6).                                    
000900*                                 KUNDNUMMER                              
001000        05 TPO2-IDKUNDRF     PIC X(10).                                   
001100*                                 KUNDENS REFERENS (ORDERID)              
001200        05 TPO2-IDARTNR      PIC 9(8).                                    
001300*                                 ARTIKELNUMMER                           
001400        05 TPO2-BERADREF     PIC X(10).                                   
001500*                                 KUNDENS RADREFERENS                     
001600        05 TPO2-IDANSK       PIC 9(3).                                    
001700*                                 ANSKAFFARNUMMER                         
001800        05 TPO2-IDKONTO      PIC 9(10).                                   
001900*                                 KONTO                                   
002000        05 TPO2-IDKST        PIC X(10).                                   
002100*                                 KOSTNADSSTÄLLE                          
002200        05 TPO2-IDANALYS     PIC X(12).                                   
002300*                                 ANALYSNUMMER                            
002400        05 TPO2-KDDSP        PIC 9.                                       
002500*                                 PÅVERKAN PÅ DSP                         
002600        05 TPO2-KDFAKTYP     PIC X.                                       
002700*                                 FAKTURATYP                              
002800        05 TPO2-KDFRAKT      PIC 9(2).                                    
002900*                                 FRAKTSÄTT DC TILL KUND                  
003000        05 TPO2-KDKVBRYT     PIC 9.                                       
003100*                                 KOD OM KVANTFÖRP SKALL BRYTAS           
003200        05 TPO2-KDORDING     PIC 9.                                       
003300*                                 UPPDATERING ORDERINGÅNG                 
003400        05 TPO2-KDORDKL      PIC 9.                                       
003500*                                 ORDERKLASS                              
003600        05 TPO2-KDPRODSL     PIC 9(2).                                    
003700*                                 PRODUKTSLAG                             
003800        05 TPO2-KDTPOTYP     PIC 9.                                       
003900*                                 TYP AV TIDPLANERAD ORDER                
004000        05 TPO2-KDVRINFO     PIC 9.                                       
004100*                                 PÅVERKAN I VR/DSP SYSTEM                
004200        05 TPO2-KVBEART-Q    PIC 9(6).                                    
004300*                                 BESTÄLLT KVANTANPASSAT ANTAL            
004400        05 TPO2-PRARTNTO     PIC 9(7)V9(2).                               
004500*                                 ARTIKELPRIS NETTO                       
004600        05 TPO2-REKSIFFR     PIC 9.                                       
004700*                                 KONTROLLSIFFRA                          
004800        05 TPO2-TITPO        PIC 9(6).                                    
004900*                                 PLANERAD ORDERDATUM                     
005000        05 TPO2-KDPRTYP      PIC X.                                       
005100*                                 TYP AV PRISTILLÄMPNING                  
005200        05 TPO2-BEVOLREF     PIC X(10).                                   
005300*                                 VOLVO REFERENS                          
005400        05 TPO2-FLINVEST     PIC X.                                       
005500*                                 BYTES INVENTERINGSFLAGGA                
005600        05 TPO2-FLORDSPE     PIC X.                                       
005700*                                 SPECIALORDERFLAGGA                      
005800        05 TPO2-FLOVRLEV     PIC X.                                       
005900*                                 ÖVERLEVERANS                            
006000        05 TPO2-FLPRTILL     PIC X.                                       
006100*                                 PRISTILLÄGGS FLAGGA                     
006200        05 TPO2-BEKUNDRF     PIC X(15).                                   
006300*                                 KUNDENS REFERENS                        
006400        05 TPO2-IDKAMPRF     PIC 9(7).                                    
006500*                                 KAMPANJREFERENS                         
006600        05 TPO2-IDLEVNR      PIC X(5).                                    
006700*                                 LEVERANTÖRNUMMER                        
006800        05 TPO2-IDSYSTEM     PIC X(4).                                    
006900*                                 VOLVO VCCS SYSTEMNUMMER                 
007000        05 TPO2-KDUART       PIC X.                                       
007100*                                 UNDANTAGSARTIKEL                        
007200        05 TPO2-KVFRYSTI     PIC 9(2).                                    
007300*                                 FRYSTID FÖR TPO-ORDER                   
007400        05 TPO2-KDORDBEH     PIC 9.                                       
007500*                                 STATUSKOD ORDERBEHANDLING               
007600        05 TPO2-FLFORBI      PIC X.                                       
007700*                                 FÖRBIORDERFLAGGA                        
007800        05 TPO2-FLTILLK      PIC X.                                       
007900*                                 TILLKOMMANDE ARTIKEL ?                  
008000        05 TPO2-TIDISPIN     PIC 9(6).                                    
008100*                                 DISP-DATUM NÄSTA INLEV (ÅÅMMDD)         
008200        05 TPO2-KVBEART      PIC 9(6).                                    
008300*                                 BESTÄLLT ANTAL STYCKEN                  
008400        05 TPO2-KVQPACK-1    PIC 9(5).                                    
008500*                                 ANTAL I Q1 FÖRPACKNING                  
008600        05 TPO2-BEVARREF     PIC X(10).                                   
008700*                                 VÅR REFERENS                            
008800        05 TPO2-KDORDTYP-LDC PIC X(2).                                    
008900*                                 ORDERTYP HOS DEALER                     
009000        05 TPO2-TIREPDAT     PIC 9(6).                                    
009100*                                 REPAIR DATE                             
009200        05 TPO2-IDKUNDRF-WIP PIC X(10).                                   
009300*                                 REPARATIONS ORDERNR, LDC KUND           
009400        05 TPO2-FILLERX2     PIC X(2).                                    
009500     03 TPO2-DEAL-PR-LINE.                                                
009600*                                 DEALERPRIS (RAD)                        
009700        05 TPO2-IDPRQUES     PIC 9(7).                                    
009800*                                 PRISFRÅGA NR                            
009900        05 TPO2-PRARTNTO-LOC PIC S9(7)V9(2)      COMP-3.                  
010000*                                 ARTIKELPRIS NETTO LOKAL VALUTA          
010100        05 TPO2-PRARTNTO-LOCPREL                                          
010200                             PIC S9(7)V9(2)      COMP-3.                  
010300*                                 PREL NETTO SLUTKUNDSPRIS I              
010400*                                 LOKAL VALUTA                            
010500        05 TPO2-PRARTBTO-LOC PIC S9(7)V9(2)      COMP-3.                  
010600*                                 PRIS I LOKAL VALUTA                     
010700        05 TPO2-KDVALISO     PIC X(3).                                    
010800*                                 VALUTAKOD ENLIGT ISO-STANDARD.          
010900        05 TPO2-KDVAT        PIC X(2).                                    
011000*                                 MOMSKOD                                 
011100        05 TPO2-RERAB        PIC S9(2)V9(1)      COMP-3.                  
011200*                                 RABATTSATS (PROCENT)                    
011300        05 TPO2-KDRAB        PIC X(5).                                    
011400*                                 RABATTKOD                               
011500        05 TPO2-BEART-VIPS   PIC X(25).                                   
011600*                                 VIPS ARTIKELBENÄMNING                   
011700*                                 PÅ DEALERNS SPRÅK                       
011800     03 TPO2-UTDATA.                                                      
011900*                                                                         
012000        05 TPO2-KDORDBEK     PIC 9(2).                                    
012100*                                 ORDERBEKRÄFTELSEKOD                     
012200        05 TPO2-FLKLAR       PIC X.                                       
012300*                                 AVSLUTNINGSMARKERING                    
012400*** END OF VILMAII-COPY LENGTH= 267 BYTES                                 
