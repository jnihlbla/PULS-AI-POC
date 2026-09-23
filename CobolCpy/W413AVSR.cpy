000100 01  AVSR-W413AVSR.                                                       
000200*                                 LÄNKAREA TILL W413AVSR -                
000300*                                 WOPS RADBEHANDLING                      
000400     03 AVSR-KDCALL          PIC S9(3)           COMP-3.                  
000500*                                 ANROPSTYP                               
000600     03 AVSR-IDORDER         PIC S9(7)           COMP-3.                  
000700*                                 VOLVO PARTS ORDERNUMMER                 
000800     03 AVSR-KDORDKL         PIC S9              COMP-3.                  
000900*                                 ORDERKLASS                              
001000     03 AVSR-KDFRAKT         PIC S9(3)           COMP-3.                  
001100*                                 FRAKTSÄTT DC TILL KUND                  
001200     03 AVSR-KDROPACK        PIC X.                                       
001300*                                 FRISLÄPPNINGSKOD RO/DO                  
001400     03 AVSR-TIREGDAT        PIC S9(7)           COMP-3.                  
001500*                                 REGISTRERINGSDATUM (ÅÅMMDD)             
001600     03 AVSR-TIHHMM          PIC S9(5)           COMP-3.                  
001700*                                 KLOCKSLAG (TIMMAR OCH MINUTER)          
001800     03 AVSR-ORDERRAD        OCCURS 100 TIMES.                            
001900        05 AVSR-ADLAGOMR     PIC S9(3)           COMP-3.                  
002000*                                 LAGEROMRÅDE                             
002100        05 AVSR-IDLEVNR      PIC X(5).                                    
002200*                                 LEVERANTÖRNUMMER                        
002300        05 AVSR-IDDC         PIC X(2).                                    
002400*                                 IDENTIFIERARE LAGER                     
002500        05 AVSR-KDORDSTA     PIC X(2).                                    
002600*                                 VOLVOORDERSTATUS                        
002700        05 AVSR-KDSPEEMB     PIC 9.                                       
002800*                                 SPECIALEMBALLAGEKOD                     
002900        05 AVSR-KDVIA        PIC X(2).                                    
003000*                                 KOD FöR LEVERANS VIA                    
003100        05 AVSR-KVANNANT     PIC S9(7)           COMP-3.                  
003200*                                 ANNULLERAT ANTAL ARTIKLAR               
003300        05 AVSR-KVBEART-Q    PIC S9(7)           COMP-3.                  
003400*                                 BESTÄLLT KVANTANPASSAT ANTAL            
003500        05 AVSR-KVDAGAR-DIFF PIC S9(3)           COMP-3.                  
003600*                                 TRANSPORT DAY DIFF. (+/- CONTRA         
003700*                                  CDC)                                   
003800        05 AVSR-PRARTNTO     PIC S9(7)V9(2)      COMP-3.                  
003900*                                 ARTIKELPRIS NETTO                       
004000        05 AVSR-PRAVCOST     PIC S9(7)V9(2)      COMP-3.                  
004100*                                 MEDELVÄRDESKOSTNAD I UTL.VALUTA         
004200        05 AVSR-TISKEPPN-DDC PIC S9(7)           COMP-3.                  
004300*                                 SKEPPNINGSDATUM DLEV (ÅÅMMDD)           
004400        05 AVSR-VKART        PIC S9(7)           COMP-3.                  
004500*                                 ARTIKELVIKT (G)                         
004600        05 AVSR-VLARTNTO     PIC S9(8)V9(1)      COMP-3.                  
004700*                                 ARTIKELVOLYM (CM3)                      
004800        05 AVSR-KDVSOP       PIC S9(3)           COMP-3.                  
004900*                                 VSOP-KOD                                
005000        05 AVSR-KDFARLIG     PIC S9              COMP-3.                  
005100*                                 KOD FÖR FARLIGT GODS                    
005200        05 AVSR-DEAL-PR-LINE.                                             
005300*                                 DEALERPRIS (RAD)                        
005400           07 AVSR-IDPRQUES  PIC 9(7).                                    
005500*                                 PRISFRÅGA NR                            
005600           07 AVSR-PRARTNTO-LOC                                           
005700                             PIC S9(7)V9(2)      COMP-3.                  
005800*                                 ARTIKELPRIS NETTO LOKAL VALUTA          
005900           07 AVSR-PRARTNTO-LOCPREL                                       
006000                             PIC S9(7)V9(2)      COMP-3.                  
006100*                                 PREL NETTO SLUTKUNDSPRIS I              
006200*                                 LOKAL VALUTA                            
006300           07 AVSR-PRARTBTO-LOC                                           
006400                             PIC S9(7)V9(2)      COMP-3.                  
006500*                                 PRIS I LOKAL VALUTA                     
006600           07 AVSR-KDVALISO  PIC X(3).                                    
006700*                                 VALUTAKOD ENLIGT ISO-STANDARD.          
006800           07 AVSR-KDVAT     PIC X(2).                                    
006900*                                 MOMSKOD                                 
007000           07 AVSR-RERAB     PIC S9(2)V9(1)      COMP-3.                  
007100*                                 RABATTSATS (PROCENT)                    
007200           07 AVSR-KDRAB     PIC X(5).                                    
007300*                                 RABATTKOD                               
007400           07 AVSR-BEART-VIPS                                             
007500                             PIC X(25).                                   
007600*                                 VIPS ARTIKELBENÄMNING                   
007700*                                 PÅ DEALERNS SPRÅK                       
007800*** END OF VILMAII-COPY LENGTH= 10917 BYTES                               
