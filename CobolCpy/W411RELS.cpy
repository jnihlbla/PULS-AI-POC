000100 01  RELS-W411RELS.                                                       
000200*                                 LÄNKAREA MELLAN RADPGM OCH SUB-         
000300*                                 MODUL FÖR RELS.                         
000400     03 RELS-INDATA.                                                      
000500*                                                                         
000600        05 RELS-IDDISTR      PIC 9(4).                                    
000700*                                 DISTRIKTNUMMER                          
000800        05 RELS-IDKUNDNR     PIC 9(6).                                    
000900*                                 KUNDNUMMER                              
001000        05 RELS-IDKUNDRF     PIC X(10).                                   
001100*                                 KUNDENS REFERENS (ORDERID)              
001200        05 RELS-IDARTNR      PIC 9(8).                                    
001300*                                 ARTIKELNUMMER                           
001400        05 RELS-BERADREF     PIC X(10).                                   
001500*                                 KUNDENS RADREFERENS                     
001600        05 RELS-IDANSK       PIC 9(3).                                    
001700*                                 ANSKAFFARNUMMER                         
001800        05 RELS-IDKONTO      PIC 9(10).                                   
001900*                                 KONTO                                   
002000        05 RELS-IDKST        PIC X(10).                                   
002100*                                 KOSTNADSSTÄLLE                          
002200        05 RELS-IDANALYS     PIC X(12).                                   
002300*                                 ANALYSNUMMER                            
002400        05 RELS-KDDSP        PIC 9.                                       
002500*                                 PÅVERKAN PÅ DSP                         
002600        05 RELS-KDFAKTYP     PIC X.                                       
002700*                                 FAKTURATYP                              
002800        05 RELS-KDFRAKT      PIC 9(2).                                    
002900*                                 FRAKTSÄTT DC TILL KUND                  
003000        05 RELS-KDKVBRYT     PIC 9.                                       
003100*                                 KOD OM KVANTFÖRP SKALL BRYTAS           
003200        05 RELS-KDORDING     PIC 9.                                       
003300*                                 UPPDATERING ORDERINGÅNG                 
003400        05 RELS-KDORDKL      PIC 9.                                       
003500*                                 ORDERKLASS                              
003600        05 RELS-KDPRODSL     PIC 9(2).                                    
003700*                                 PRODUKTSLAG                             
003800        05 RELS-KDTPOTYP     PIC 9.                                       
003900*                                 TYP AV TIDPLANERAD ORDER                
004000        05 RELS-KDVRINFO     PIC 9.                                       
004100*                                 PÅVERKAN I VR/DSP SYSTEM                
004200        05 RELS-KVBEART-Q    PIC 9(6).                                    
004300*                                 BESTÄLLT KVANTANPASSAT ANTAL            
004400        05 RELS-PRARTNTO     PIC 9(7)V9(2).                               
004500*                                 ARTIKELPRIS NETTO                       
004600        05 RELS-PRAVCOST     PIC 9(7)V9(2).                               
004700*                                 MEDELVÄRDESKOSTNAD I UTL.VALUTA         
004800        05 RELS-REKSIFFR     PIC 9.                                       
004900*                                 KONTROLLSIFFRA                          
005000        05 RELS-TITPO        PIC 9(6).                                    
005100*                                 PLANERAD ORDERDATUM                     
005200        05 RELS-KDPRTYP      PIC X.                                       
005300*                                 TYP AV PRISTILLÄMPNING                  
005400        05 RELS-BEVOLREF     PIC X(10).                                   
005500*                                 VOLVO REFERENS                          
005600        05 RELS-FLINVEST     PIC X.                                       
005700*                                 BYTES INVENTERINGSFLAGGA                
005800        05 RELS-FLORDSPE     PIC X.                                       
005900*                                 SPECIALORDERFLAGGA                      
006000        05 RELS-FLOVRLEV     PIC X.                                       
006100*                                 ÖVERLEVERANS                            
006200        05 RELS-FLPRTILL     PIC X.                                       
006300*                                 PRISTILLÄGGS FLAGGA                     
006400        05 RELS-BEKUNDRF     PIC X(15).                                   
006500*                                 KUNDENS REFERENS                        
006600        05 RELS-IDKAMPRF     PIC 9(7).                                    
006700*                                 KAMPANJREFERENS                         
006800        05 RELS-IDLEVNR      PIC X(5).                                    
006900*                                 LEVERANTÖRNUMMER                        
007000        05 RELS-IDSYSTEM     PIC X(4).                                    
007100*                                 VOLVO VCCS SYSTEMNUMMER                 
007200        05 RELS-KDUART       PIC X.                                       
007300*                                 UNDANTAGSARTIKEL                        
007400        05 RELS-KVFRYSTI     PIC 9(2).                                    
007500*                                 FRYSTID FÖR TPO-ORDER                   
007600        05 RELS-KDORDBEH     PIC 9.                                       
007700*                                 STATUSKOD ORDERBEHANDLING               
007800        05 RELS-FLFORBI      PIC X.                                       
007900*                                 FÖRBIORDERFLAGGA                        
008000        05 RELS-FLTILLK      PIC X.                                       
008100*                                 TILLKOMMANDE ARTIKEL ?                  
008200        05 RELS-TIDISPIN     PIC 9(6).                                    
008300*                                 DISP-DATUM NÄSTA INLEV (ÅÅMMDD)         
008400        05 RELS-KVBEART      PIC 9(6).                                    
008500*                                 BESTÄLLT ANTAL STYCKEN                  
008600        05 RELS-KVQPACK-1    PIC 9(5).                                    
008700*                                 ANTAL I Q1 FÖRPACKNING                  
008800        05 RELS-BEVARREF     PIC X(10).                                   
008900*                                 VÅR REFERENS                            
009000        05 RELS-KDORDTYP-LDC PIC X(2).                                    
009100*                                 ORDERTYP HOS DEALER                     
009200        05 RELS-TIREPDAT     PIC 9(6).                                    
009300*                                 REPAIR DATE                             
009400        05 RELS-IDKUNDRF-WIP PIC X(10).                                   
009500*                                 REPARATIONS ORDERNR, LDC KUND           
009600     03 RELS-DEAL-PR-LINE.                                                
009700*                                 DEALERPRIS (RAD)                        
009800        05 RELS-IDPRQUES     PIC 9(7).                                    
009900*                                 PRISFRÅGA NR                            
010000        05 RELS-PRARTNTO-LOC PIC S9(7)V9(2)      COMP-3.                  
010100*                                 ARTIKELPRIS NETTO LOKAL VALUTA          
010200        05 RELS-PRARTNTO-LOCPREL                                          
010300                             PIC S9(7)V9(2)      COMP-3.                  
010400*                                 PREL NETTO SLUTKUNDSPRIS I              
010500*                                 LOKAL VALUTA                            
010600        05 RELS-PRARTBTO-LOC PIC S9(7)V9(2)      COMP-3.                  
010700*                                 PRIS I LOKAL VALUTA                     
010800        05 RELS-KDVALISO     PIC X(3).                                    
010900*                                 VALUTAKOD ENLIGT ISO-STANDARD.          
011000        05 RELS-KDVAT        PIC X(2).                                    
011100*                                 MOMSKOD                                 
011200        05 RELS-RERAB        PIC S9(2)V9(1)      COMP-3.                  
011300*                                 RABATTSATS (PROCENT)                    
011400        05 RELS-KDRAB        PIC X(5).                                    
011500*                                 RABATTKOD                               
011600        05 RELS-BEART-VIPS   PIC X(25).                                   
011700*                                 VIPS ARTIKELBENÄMNING                   
011800*                                 PÅ DEALERNS SPRÅK                       
011900     03 RELS-UTDATA.                                                      
012000*                                                                         
012100        05 RELS-KDORDBEK     PIC 9(2).                                    
012200*                                 ORDERBEKRÄFTELSEKOD                     
012300        05 RELS-FLKLAR       PIC X.                                       
012400*                                 AVSLUTNINGSMARKERING                    
012500*** END OF VILMAII-COPY LENGTH= 274 BYTES                                 
