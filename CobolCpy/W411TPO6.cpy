000100 01  TPO6-W411TPO6.                                                       
000200*                                 LÄNKAREA MELLAN RADPGM OCH SUB-         
000300*                                 MODUL FÖR TPO6.                         
000400     03 TPO6-INDATA.                                                      
000500*                                                                         
000600        05 TPO6-IDDISTR      PIC 9(4).                                    
000700*                                 DISTRIKTNUMMER                          
000800        05 TPO6-IDKUNDNR     PIC 9(6).                                    
000900*                                 KUNDNUMMER                              
001000        05 TPO6-IDKUNDRF     PIC X(10).                                   
001100*                                 KUNDENS REFERENS (ORDERID)              
001200        05 TPO6-IDARTNR      PIC 9(8).                                    
001300*                                 ARTIKELNUMMER                           
001400        05 TPO6-IDDC-DAY     PIC X(2).                                    
001500*                                 IDENTIFIERARE DAGORDERLAGER             
001600        05 TPO6-BERADREF     PIC X(10).                                   
001700*                                 KUNDENS RADREFERENS                     
001800        05 TPO6-IDANSK       PIC 9(3).                                    
001900*                                 ANSKAFFARNUMMER                         
002000        05 TPO6-IDKONTO      PIC 9(10).                                   
002100*                                 KONTO                                   
002200        05 TPO6-IDKST        PIC X(10).                                   
002300*                                 KOSTNADSSTÄLLE                          
002400        05 TPO6-IDANALYS     PIC X(12).                                   
002500*                                 ANALYSNUMMER                            
002600        05 TPO6-KDDSP        PIC 9.                                       
002700*                                 PÅVERKAN PÅ DSP                         
002800        05 TPO6-KDFAKTYP     PIC X.                                       
002900*                                 FAKTURATYP                              
003000        05 TPO6-KDFRAKT      PIC 9(2).                                    
003100*                                 FRAKTSÄTT DC TILL KUND                  
003200        05 TPO6-KDKVBRYT     PIC 9.                                       
003300*                                 KOD OM KVANTFÖRP SKALL BRYTAS           
003400        05 TPO6-KDORDING     PIC 9.                                       
003500*                                 UPPDATERING ORDERINGÅNG                 
003600        05 TPO6-KDORDKL      PIC 9.                                       
003700*                                 ORDERKLASS                              
003800        05 TPO6-KDPRODSL     PIC 9(2).                                    
003900*                                 PRODUKTSLAG                             
004000        05 TPO6-KDTPOTYP     PIC 9.                                       
004100*                                 TYP AV TIDPLANERAD ORDER                
004200        05 TPO6-KDVRINFO     PIC 9.                                       
004300*                                 PÅVERKAN I VR/DSP SYSTEM                
004400        05 TPO6-KDUART       PIC X.                                       
004500*                                 UNDANTAGSARTIKEL                        
004600        05 TPO6-KVBEART-Q    PIC 9(6).                                    
004700*                                 BESTÄLLT KVANTANPASSAT ANTAL            
004800        05 TPO6-PRARTNTO     PIC 9(7)V9(2).                               
004900*                                 ARTIKELPRIS NETTO                       
005000        05 TPO6-REDIRLEV     PIC 9V9(2).                                  
005100*                                 DIREKTLEVERANSANDEL                     
005200        05 TPO6-REKSIFFR     PIC 9.                                       
005300*                                 KONTROLLSIFFRA                          
005400        05 TPO6-KDPRTYP      PIC X.                                       
005500*                                 TYP AV PRISTILLÄMPNING                  
005600        05 TPO6-BEVOLREF     PIC X(10).                                   
005700*                                 VOLVO REFERENS                          
005800        05 TPO6-FLINVEST     PIC X.                                       
005900*                                 BYTES INVENTERINGSFLAGGA                
006000        05 TPO6-FLPRTILL     PIC X.                                       
006100*                                 PRISTILLÄGGS FLAGGA                     
006200        05 TPO6-FLREFILL     PIC X.                                       
006300*                                 REFILLARTIKEL                           
006400        05 TPO6-BEKUNDRF     PIC X(15).                                   
006500*                                 KUNDENS REFERENS                        
006600        05 TPO6-IDKAMPRF     PIC 9(7).                                    
006700*                                 KAMPANJREFERENS                         
006800        05 TPO6-IDLEVNR      PIC X(5).                                    
006900*                                 LEVERANTÖRNUMMER                        
007000        05 TPO6-IDSYSTEM     PIC X(4).                                    
007100*                                 VOLVO VCCS SYSTEMNUMMER                 
007200        05 TPO6-FLFORBI      PIC X.                                       
007300*                                 FÖRBIORDERFLAGGA                        
007400        05 TPO6-KDORDTYP-LDC PIC X(2).                                    
007500*                                 ORDERTYP HOS DEALER                     
007600        05 TPO6-TIREPDAT     PIC 9(6).                                    
007700*                                 REPAIR DATE                             
007800        05 TPO6-IDKUNDRF-WIP PIC X(10).                                   
007900*                                 REPARATIONS ORDERNR, LDC KUND           
008000        05 TPO6-KDOI         PIC X(2).                                    
008100*                                 ORDERINGÅNGSTYP                         
008200        05 TPO6-CLEARGROUP.                                               
008300*                                 CLEARINGAREA FÖR ORDERINGÅNG            
008400           07 TPO6-CLEARAREA OCCURS 7 TIMES.                              
008500*                                 CLEARINGAREA FÖR ORDERINGÅNG            
008600              09 TPO6-IDDC-CLEAR                                          
008700                             PIC X(2).                                    
008800*                                 LAGERPRIORITERING VID                   
008900*                                 ORDERCLEARING                           
009000              09 TPO6-FLLF   PIC X.                                       
009100*                                 ARTIKEL LAGERFÖRES                      
009200              09 TPO6-FLCLEAR                                             
009300                             PIC X.                                       
009400*                                 ORDERRAD CLEAR FLAGGA                   
009500     03 TPO6-DEAL-PR-LINE.                                                
009600*                                 DEALERPRIS (RAD)                        
009700        05 TPO6-IDPRQUES     PIC 9(7).                                    
009800*                                 PRISFRÅGA NR                            
009900        05 TPO6-PRARTNTO-LOC PIC S9(7)V9(2)      COMP-3.                  
010000*                                 ARTIKELPRIS NETTO LOKAL VALUTA          
010100        05 TPO6-PRARTNTO-LOCPREL                                          
010200                             PIC S9(7)V9(2)      COMP-3.                  
010300*                                 PREL NETTO SLUTKUNDSPRIS I              
010400*                                 LOKAL VALUTA                            
010500        05 TPO6-PRARTBTO-LOC PIC S9(7)V9(2)      COMP-3.                  
010600*                                 PRIS I LOKAL VALUTA                     
010700        05 TPO6-KDVALISO     PIC X(3).                                    
010800*                                 VALUTAKOD ENLIGT ISO-STANDARD.          
010900        05 TPO6-KDVAT        PIC X(2).                                    
011000*                                 MOMSKOD                                 
011100        05 TPO6-RERAB        PIC S9(2)V9(1)      COMP-3.                  
011200*                                 RABATTSATS (PROCENT)                    
011300        05 TPO6-KDRAB        PIC X(5).                                    
011400*                                 RABATTKOD                               
011500        05 TPO6-BEART-VIPS   PIC X(25).                                   
011600*                                 VIPS ARTIKELBENÄMNING                   
011700*                                 PÅ DEALERNS SPRÅK                       
011800     03 TPO6-UTDATA.                                                      
011900*                                                                         
012000        05 TPO6-KDORDBEK     PIC 9(2).                                    
012100*                                 ORDERBEKRÄFTELSEKOD                     
012200        05 TPO6-FLKLAR       PIC X.                                       
012300*                                 AVSLUTNINGSMARKERING                    
012400*** END OF VILMAII-COPY LENGTH= 262 BYTES                                 
