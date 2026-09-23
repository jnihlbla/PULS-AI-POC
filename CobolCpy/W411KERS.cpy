000100 01  KERS-W411KERS.                                                       
000200*                                 LÄNKAREA TILL W411KERS -                
000300*                                 KONTROLLERA ERSÄTTNINGAR                
000400     03 KERS-INDATA.                                                      
000500        05 KERS-IDARTNR      PIC S9(9)           COMP-3.                  
000600*                                 ARTIKELNUMMER                           
000700        05 KERS-FLPRERS      PIC X.                                       
000800*                                 PRISERSÄTTNINGSFLAGGA                   
000900        05 KERS-FLPRTILL     PIC X.                                       
001000*                                 PRISTILLÄGGS FLAGGA                     
001100        05 KERS-FLFORBI      PIC X.                                       
001200*                                 FÖRBIORDERFLAGGA                        
001300        05 KERS-FLORDSPE     PIC X.                                       
001400*                                 SPECIALORDERFLAGGA                      
001500        05 KERS-FLOVRLEV     PIC X.                                       
001600*                                 ÖVERLEVERANS                            
001700        05 KERS-IDKAMPRF     PIC S9(7)           COMP-3.                  
001800*                                 KAMPANJREFERENS                         
001900        05 KERS-KDERS        PIC S9(3)           COMP-3.                  
002000*                                 ERSÄTTNINGSKOD                          
002100        05 KERS-KDERS-UTG    PIC S9(3)           COMP-3.                  
002200*                                 ERSÄTTNINGSKOD UTGÅNGEN ARTIKEL         
002300        05 KERS-KDPRTYP      PIC X.                                       
002400*                                 TYP AV PRISTILLÄMPNING                  
002500        05 KERS-KDTPOTYP     PIC S9              COMP-3.                  
002600*                                 TYP AV TIDPLANERAD ORDER                
002700        05 KERS-KDUART       PIC X.                                       
002800*                                 UNDANTAGSARTIKEL                        
002900        05 KERS-KVBEART      PIC S9(7)           COMP-3.                  
003000*                                 BESTÄLLT ANTAL STYCKEN                  
003100        05 KERS-PRARTNTO     PIC S9(7)V9(2)      COMP-3.                  
003200*                                 ARTIKELPRIS NETTO                       
003300        05 KERS-TIPRIS       PIC S9(7)           COMP-3.                  
003400*                                 PRISTILLÄMPNINGSDATUM  (ÅÅMMDD)         
003500        05 KERS-IDDC         PIC X(2).                                    
003600*                                 IDENTIFIERARE LAGER                     
003700     03 KERS-DEAL-PR-LINE.                                                
003800*                                 DEALERPRIS (RAD)                        
003900        05 KERS-IDPRQUES     PIC 9(7).                                    
004000*                                 PRISFRÅGA NR                            
004100        05 KERS-PRARTNTO-LOC PIC S9(7)V9(2)      COMP-3.                  
004200*                                 ARTIKELPRIS NETTO LOKAL VALUTA          
004300        05 KERS-PRARTNTO-LOCPREL                                          
004400                             PIC S9(7)V9(2)      COMP-3.                  
004500*                                 PREL NETTO SLUTKUNDSPRIS I              
004600*                                 LOKAL VALUTA                            
004700        05 KERS-PRARTBTO-LOC PIC S9(7)V9(2)      COMP-3.                  
004800*                                 PRIS I LOKAL VALUTA                     
004900        05 KERS-KDVALISO     PIC X(3).                                    
005000*                                 VALUTAKOD ENLIGT ISO-STANDARD.          
005100        05 KERS-KDVAT        PIC X(2).                                    
005200*                                 MOMSKOD                                 
005300        05 KERS-RERAB        PIC S9(2)V9(1)      COMP-3.                  
005400*                                 RABATTSATS (PROCENT)                    
005500        05 KERS-KDRAB        PIC X(5).                                    
005600*                                 RABATTKOD                               
005700        05 KERS-BEART-VIPS   PIC X(25).                                   
005800*                                 VIPS ARTIKELBENÄMNING                   
005900*                                 PÅ DEALERNS SPRÅK                       
006000     03 KERS-UTDATA.                                                      
006100        05 KERS-KDORDBEK     PIC 9(2).                                    
006200*                                 ORDERBEKRÄFTELSEKOD                     
006300*** END OF VILMAII-COPY LENGTH= 97 BYTES                                  
