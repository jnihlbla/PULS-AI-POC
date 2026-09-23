000100 01  W212L003.                                                            
000200*                                 LÄNKAREA VID CALL MOT IMSMODUL          
000300*                                 I W21202.                               
000400*                                                                         
000500*                                                                         
000600     03 KDCALL               PIC S9(3)           COMP-3.                  
000700      88 HAMTA-BEST-INFO-KEY VALUE +3.                                    
000800      88 HAMTA-AVTAL-INFO-KEY                                             
000900                             VALUE +4.                                    
001000     03 NYCKLAR.                                                          
001100        05 IDARTNR           PIC S9(9)           COMP-3.                  
001200*                                 ARTIKELNUMMER                           
001300     03 ANTAL-BEST           PIC 9.                                       
001400*                                 BESTÄLLT ANTAL       KVBEST-002         
001500     03 IO-AREA.                                                          
001600        05 IDBEST-1          PIC S9(13)          COMP-3.                  
001700*                                 BESTÄLLNINGS-ID (PPPBBBBBBSSS)          
001800*                                 PPP   = (PREFIX) INKÖPARNR              
001900*                                 BBBBBB= BESTÄLLARNR                     
002000*                                 SSS   = (SUFFIX) GODSM/PROD.KOD         
002100        05 IDBEST-2          PIC S9(13)          COMP-3.                  
002200*                                 BESTÄLLNINGS-ID (PPPBBBBBBSSS)          
002300*                                 PPP   = (PREFIX) INKÖPARNR              
002400*                                 BBBBBB= BESTÄLLARNR                     
002500*                                 SSS   = (SUFFIX) GODSM/PROD.KOD         
002600        05 IDBEST-3          PIC S9(13)          COMP-3.                  
002700*                                 BESTÄLLNINGS-ID (PPPBBBBBBSSS)          
002800*                                 PPP   = (PREFIX) INKÖPARNR              
002900*                                 BBBBBB= BESTÄLLARNR                     
003000*                                 SSS   = (SUFFIX) GODSM/PROD.KOD         
003100        05 IDBEST-4          PIC S9(13)          COMP-3.                  
003200*                                 BESTÄLLNINGS-ID (PPPBBBBBBSSS)          
003300*                                 PPP   = (PREFIX) INKÖPARNR              
003400*                                 BBBBBB= BESTÄLLARNR                     
003500*                                 SSS   = (SUFFIX) GODSM/PROD.KOD         
003600        05 IDBEST-5          PIC S9(13)          COMP-3.                  
003700*                                 BESTÄLLNINGS-ID (PPPBBBBBBSSS)          
003800*                                 PPP   = (PREFIX) INKÖPARNR              
003900*                                 BBBBBB= BESTÄLLARNR                     
004000*                                 SSS   = (SUFFIX) GODSM/PROD.KOD         
004100        05 IDBEST-6          PIC S9(13)          COMP-3.                  
004200*                                 BESTÄLLNINGS-ID (PPPBBBBBBSSS)          
004300*                                 PPP   = (PREFIX) INKÖPARNR              
004400*                                 BBBBBB= BESTÄLLARNR                     
004500*                                 SSS   = (SUFFIX) GODSM/PROD.KOD         
004600        05 IDBEST-7          PIC S9(13)          COMP-3.                  
004700*                                 BESTÄLLNINGS-ID (PPPBBBBBBSSS)          
004800*                                 PPP   = (PREFIX) INKÖPARNR              
004900*                                 BBBBBB= BESTÄLLARNR                     
005000*                                 SSS   = (SUFFIX) GODSM/PROD.KOD         
005100        05 IDLEVNR-1         PIC X(5).                                    
005200*                                 LEVERANTÖRNUMMER                        
005300        05 IDLEVNR-2         PIC X(5).                                    
005400*                                 LEVERANTÖRNUMMER                        
005500        05 IDLEVNR-3         PIC X(5).                                    
005600*                                 LEVERANTÖRNUMMER                        
005700        05 IDLEVNR-4         PIC X(5).                                    
005800*                                 LEVERANTÖRNUMMER                        
005900        05 IDLEVNR-5         PIC X(5).                                    
006000*                                 LEVERANTÖRNUMMER                        
006100        05 IDLEVNR-6         PIC X(5).                                    
006200*                                 LEVERANTÖRNUMMER                        
006300        05 IDLEVNR-7         PIC X(5).                                    
006400*                                 LEVERANTÖRNUMMER                        
006500        05 TIBEST-1          PIC S9(7)           COMP-3.                  
006600*                                 BESTÄLLNINGSDATUM (ÅÅMMDD)              
006700        05 TIBEST-2          PIC S9(7)           COMP-3.                  
006800*                                 BESTÄLLNINGSDATUM (ÅÅMMDD)              
006900        05 TIBEST-3          PIC S9(7)           COMP-3.                  
007000*                                 BESTÄLLNINGSDATUM (ÅÅMMDD)              
007100        05 TIBEST-4          PIC S9(7)           COMP-3.                  
007200*                                 BESTÄLLNINGSDATUM (ÅÅMMDD)              
007300        05 TIBEST-5          PIC S9(7)           COMP-3.                  
007400*                                 BESTÄLLNINGSDATUM (ÅÅMMDD)              
007500        05 TIBEST-6          PIC S9(7)           COMP-3.                  
007600*                                 BESTÄLLNINGSDATUM (ÅÅMMDD)              
007700        05 TIBEST-7          PIC S9(7)           COMP-3.                  
007800*                                 BESTÄLLNINGSDATUM (ÅÅMMDD)              
007900        05 KDBEH-BEST-1      PIC S9              COMP-3.                  
008000*                                 BEHANDLINGSKOD BESTÄLLNING              
008100        05 KDBEH-BEST-2      PIC S9              COMP-3.                  
008200*                                 BEHANDLINGSKOD BESTÄLLNING              
008300        05 KDBEH-BEST-3      PIC S9              COMP-3.                  
008400*                                 BEHANDLINGSKOD BESTÄLLNING              
008500        05 KDBEH-BEST-4      PIC S9              COMP-3.                  
008600*                                 BEHANDLINGSKOD BESTÄLLNING              
008700        05 KDBEH-BEST-5      PIC S9              COMP-3.                  
008800*                                 BEHANDLINGSKOD BESTÄLLNING              
008900        05 KDBEH-BEST-6      PIC S9              COMP-3.                  
009000*                                 BEHANDLINGSKOD BESTÄLLNING              
009100        05 KDBEH-BEST-7      PIC S9              COMP-3.                  
009200*                                 BEHANDLINGSKOD BESTÄLLNING              
009300     03 TABELL-AREA REDEFINES IO-AREA.                                    
009400        05 TAB-IDBEST        OCCURS 7 TIMES                               
009500                             PIC S9(13)          COMP-3.                  
009600*                                 BESTÄLLNINGS-ID (PPPBBBBBBSSS)          
009700*                                 PPP   = (PREFIX) INKÖPARNR              
009800*                                 BBBBBB= BESTÄLLARNR                     
009900*                                 SSS   = (SUFFIX) GODSM/PROD.KOD         
010000        05 TAB-IDLEVNR       OCCURS 7 TIMES                               
010100                             PIC X(5).                                    
010200*                                 LEVERANTÖRNUMMER                        
010300        05 TAB-TIBEST        OCCURS 7 TIMES                               
010400                             PIC S9(7)           COMP-3.                  
010500*                                 BESTÄLLNINGSDATUM (ÅÅMMDD)              
010600        05 TAB-KDBEH-BEST    OCCURS 7 TIMES                               
010700                             PIC S9              COMP-3.                  
010800*                                 BEHANDLINGSKOD BESTÄLLNING              
010900*** END OF VILMAII-COPY LENGTH= 127 BYTES                                 
