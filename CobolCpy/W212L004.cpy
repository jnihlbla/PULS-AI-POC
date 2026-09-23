000100 01  W212L004.                                                            
000200*                                 LÄNKAREA VID CALL MOT IMSMODUL          
000300*                                 I W21202.                               
000400*                                 LÄSER-UPPDATERAR-LÄGGER UPP             
000500*                                 BESTÄLLNINGS OCH                        
000600*                                 AVTALS-SEGMENT. INKL.                   
000700*                                 NOTERINGSSEGMENT                        
000800*                                                                         
000900     03 KDCALL               PIC S9(3)           COMP-3.                  
001000      88 UPPDAT-BEST-INFO    VALUE +5.                                    
001100      88 NYUPPL-BEST-INFO    VALUE +6.                                    
001200      88 NYUPPL-AVT-INFO     VALUE +7.                                    
001300      88 HAMTA-BEST-INFO     VALUE +8.                                    
001400      88 HAMTA-AVT-INFO      VALUE +9.                                    
001500      88 UPPDAT-AVT-INFO     VALUE +14.                                   
001600      88 NYUPPL-BEST-NOT     VALUE +15.                                   
001700      88 NYUPPL-AVT-NOT      VALUE +16.                                   
001800      88 HAMTA-NEXT-BEST-INFO                                             
001900                             VALUE +18.                                   
002000     03 FLJANEJ-BESTINFO     PIC X.                                       
002100      88 BESTINFO-FINNS      VALUE 'J'.                                   
002200      88 BESTINFO-SAKNAS     VALUE 'N'.                                   
002300*                                 JA/NEJ-FLAGGA FÖR R2XX                  
002400     03 NYCKLAR.                                                          
002500        05 IDARTNR           PIC S9(9)           COMP-3.                  
002600*                                 ARTIKELNUMMER                           
002700        05 IDLEVNR           PIC X(5).                                    
002800*                                 LEVERANTÖRNUMMER                        
002900        05 IDAVTAL           PIC S9(13)          COMP-3.                  
003000*                                 AVTALSIDENTITET  (PPPBBBBBSSS)          
003100*                                 PPP   = INKÖPARNR (PREFIX)              
003200*                                 BBBBB = BESTÄLLARNR                     
003300*                                 SSS   = SUFFIX                          
003400        05 IDBEST            PIC S9(13)          COMP-3.                  
003500*                                 BESTÄLLNINGS-ID (PPPBBBBBBSSS)          
003600*                                 PPP   = (PREFIX) INKÖPARNR              
003700*                                 BBBBBB= BESTÄLLARNR                     
003800*                                 SSS   = (SUFFIX) GODSM/PROD.KOD         
003900        05 FLAVRART          PIC X.                                       
004000*                                 AVROPSARTIKEL                           
004100     03 IO-AREA.                                                          
004200*                                 DATAELEMENT PÅ                          
004300*                                 BESTÄLLNINGSSEGMENTET.                  
004400*                                 BESTÄLLNINGSINFORMATION FYSISK          
004500*                                 NYCKEL IDBEST                           
004600*                                                                         
004700        05 IDBEST-REG        PIC S9(13)          COMP-3.                  
004800*                                 BESTÄLLNINGS-ID (PPPBBBBBBSSS)          
004900*                                 PPP   = (PREFIX) INKÖPARNR              
005000*                                 BBBBBB= BESTÄLLARNR                     
005100*                                 SSS   = (SUFFIX) GODSM/PROD.KOD         
005200        05 IDLEVNR-BEST      PIC X(5).                                    
005300*                                 LEVERANTÖR ENL. BESTÄLLNING             
005400        05 KDBEH-BEST        PIC S9              COMP-3.                  
005500*                                 BEHANDLINGSKOD BESTÄLLNING              
005600        05 KVBEST            PIC S9(7)           COMP-3.                  
005700*                                 BESTÄLLT ANTAL                          
005800        05 KVBEST-BEKR       PIC S9(7)           COMP-3.                  
005900*                                 BEKRÄFTAT BESTÄLLT ANTAL                
006000        05 TIBEST            PIC S9(7)           COMP-3.                  
006100*                                 BESTÄLLNINGSDATUM (ÅÅMMDD)              
006200        05 IDAVTAL-REG       PIC S9(13)          COMP-3.                  
006300*                                 AVTALSIDENTITET  (PPPBBBBBSSS)          
006400*                                 PPP   = INKÖPARNR (PREFIX)              
006500*                                 BBBBB = BESTÄLLARNR                     
006600*                                 SSS   = SUFFIX                          
006700        05 IDLEVNR-AVT       PIC X(5).                                    
006800*                                 LEVERANTÖR ENLIGT AVTAL                 
006900        05 KDBEH-AVT         PIC S9              COMP-3.                  
007000*                                 BEHANDLINGSKOD AVTAL                    
007100        05 KVAVTANT          PIC S9(7)           COMP-3.                  
007200*                                 ÅRSANTAL AVTAL                          
007300        05 TIAVTAL           PIC S9(7)           COMP-3.                  
007400*                                 AVTALSDATUM  (ÅÅMMDD)                   
007500        05 KDNOTTYP-INTRANS  PIC S9              COMP-3.                  
007600*                                 NOTERINGSTYP                            
007700        05 TENOT-INTRANS     PIC X(20).                                   
007800*                                 NOTERING KÖPVILLKOR                     
007900*** END OF VILMAII-COPY LENGTH= 95 BYTES                                  
