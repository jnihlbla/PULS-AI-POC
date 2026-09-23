000100 01  W212L005.                                                            
000200*                                 LÄNKAREA VID CALL MOT IMSMODUL          
000300*                                 I W21202.  ANVÄNDS VID BORTTAG          
000400*                                 AV AVTALS OCH BESTÄLL                   
000500*                                 NINGSSEGMENT.                           
000600*                                                                         
000700     03 KDCALL               PIC S9(3)           COMP-3.                  
000800      88 BORTTAG-BEST        VALUE +10.                                   
000900      88 BORTTAG-AVTAL       VALUE +11.                                   
001000     03 NYCKLAR.                                                          
001100        05 IDARTNR           PIC S9(9)           COMP-3.                  
001200*                                 ARTIKELNUMMER                           
001300        05 IDBEST            PIC S9(13)          COMP-3.                  
001400*                                 BESTÄLLNINGS-ID (PPPBBBBBBSSS)          
001500*                                 PPP   = (PREFIX) INKÖPARNR              
001600*                                 BBBBBB= BESTÄLLARNR                     
001700*                                 SSS   = (SUFFIX) GODSM/PROD.KOD         
001800        05 TIBEST            PIC S9(7)           COMP-3.                  
001900*                                 BESTÄLLNINGSDATUM (ÅÅMMDD)              
002000        05 IDAVTAL           PIC S9(13)          COMP-3.                  
002100*                                 AVTALSIDENTITET  (PPPBBBBBSSS)          
002200*                                 PPP   = INKÖPARNR (PREFIX)              
002300*                                 BBBBB = BESTÄLLARNR                     
002400*                                 SSS   = SUFFIX                          
002500        05 FLAVRART          PIC X.                                       
002600*                                 AVROPSARTIKEL                           
002700*** END COPY W212L005C0  LENGTH=26                                        
