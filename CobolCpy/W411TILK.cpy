000100 01  TILK-W411TILK.                                                       
000200*                                 LÄNKAREA TILL W411TILK -                
000300*                                 KONTROLLERA TILLKOMMANDE ARTIKL         
000400*                                 AR                                      
000500     03 TILK-W411TILK-GRP    OCCURS 20 TIMES.                             
000600        05 TILK-IDARTNR      PIC S9(9)           COMP-3.                  
000700*                                 ARTIKELNUMMER                           
000800        05 TILK-IDARTNR-TILLK                                             
000900                             PIC S9(9)           COMP-3.                  
001000*                                 TILLKOMMANDE ARTIKELNUMMER              
001100        05 TILK-BEERS        PIC X(20).                                   
001200*                                 ERSÄTTNINGSTEXT                         
001300        05 TILK-DIERS-ERS    PIC S9(4)V9(3)      COMP-3.                  
001400*                                 ERSATT ARTIKELANTAL                     
001500        05 TILK-DIERS-TILLK  PIC S9(4)V9(3)      COMP-3.                  
001600*                                 TILLKOMMANDE ARTIKELANTAL               
001700        05 TILK-FLFINLV      PIC X.                                       
001800        05 TILK-FLPRTILL     PIC X.                                       
001900*                                 PRISTILLÄGGS FLAGGA                     
002000        05 TILK-FLTILLK-X    PIC X.                                       
002100*                                 TILLKOMMANDE ARTIKEL ?                  
002200        05 TILK-KDERS        PIC S9(3)           COMP-3.                  
002300*                                 ERSÄTTNINGSKOD                          
002400        05 TILK-KDPRTYP      PIC X.                                       
002500*                                 TYP AV PRISTILLÄMPNING                  
002600        05 TILK-KVBEART      PIC S9(7)           COMP-3.                  
002700*                                 BESTÄLLT ANTAL STYCKEN                  
002800        05 TILK-PRARTNTO     PIC S9(7)V9(2)      COMP-3.                  
002900*                                 ARTIKELPRIS NETTO                       
003000        05 TILK-TIPRIS       PIC S9(7)           COMP-3.                  
003100*                                 PRISTILLÄMPNINGSDATUM  (ÅÅMMDD)         
003200        05 TILK-REKSIFFR-TILLK                                            
003300                             PIC S9              COMP-3.                  
003400*                                 TILLKOMMANDE KONTROLLSIFFRA             
003500        05 TILK-DEAL-PR-LINE.                                             
003600*                                 DEALERPRIS (RAD)                        
003700           07 TILK-IDPRQUES  PIC 9(7).                                    
003800*                                 PRISFRÅGA NR                            
003900           07 TILK-PRARTNTO-LOC                                           
004000                             PIC S9(7)V9(2)      COMP-3.                  
004100*                                 ARTIKELPRIS NETTO LOKAL VALUTA          
004200           07 TILK-PRARTNTO-LOCPREL                                       
004300                             PIC S9(7)V9(2)      COMP-3.                  
004400*                                 PREL NETTO SLUTKUNDSPRIS I              
004500*                                 LOKAL VALUTA                            
004600           07 TILK-PRARTBTO-LOC                                           
004700                             PIC S9(7)V9(2)      COMP-3.                  
004800*                                 PRIS I LOKAL VALUTA                     
004900           07 TILK-KDVALISO  PIC X(3).                                    
005000*                                 VALUTAKOD ENLIGT ISO-STANDARD.          
005100           07 TILK-KDVAT     PIC X(2).                                    
005200*                                 MOMSKOD                                 
005300           07 TILK-RERAB     PIC S9(2)V9(1)      COMP-3.                  
005400*                                 RABATTSATS (PROCENT)                    
005500           07 TILK-KDRAB     PIC X(5).                                    
005600*                                 RABATTKOD                               
005700           07 TILK-BEART-VIPS                                             
005800                             PIC X(25).                                   
005900*                                 VIPS ARTIKELBENÄMNING                   
006000*                                 PÅ DEALERNS SPRÅK                       
006100*** END OF VILMAII-COPY LENGTH= 2340 BYTES                                
