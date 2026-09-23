000100 01  W221L501.                                                            
000200*                                                                         
000300     03 KDCALL               PIC S9(3)           COMP-3.                  
000400      88 LAES-ARTIKEL-DATA   VALUE +501.                                  
000500      88 LAES-LEVERANTOER-DATA                                            
000600                             VALUE +502.                                  
000700      88 NYUPPL-LEVERANTOER  VALUE +503.                                  
000800      88 UPPDAT-MATINFO      VALUE +504.                                  
000900      88 UPPDAT-OMSPEC       VALUE +505.                                  
001000      88 BORTTAG-OMSPEC      VALUE +506.                                  
001100      88 NYUPPL-OMSPEC       VALUE +507.                                  
001200*                                 ANROPSTYP       KDCALL-W221-002         
001300     03 FLJANEJ-ANROP        PIC X.                                       
001400      88 ANROP-OK            VALUE 'J'.                                   
001500      88 ANROP-FEL           VALUE 'N'.                                   
001600      88 SEGMENT-SAKNAS      VALUE 'S'.                                   
001700*                                 JA/NEJ-FLAGGA FÖR R2XX                  
001800     03 IDARTNR              PIC S9(9)           COMP-3.                  
001900*                                 ARTIKELNUMMER                           
002000     03 KDPRODSL             PIC S9(3)           COMP-3.                  
002100*                                 PRODUKTSLAG                             
002200     03 TIAAVV-AKT           PIC S9(5)           COMP-3.                  
002300*                                                                         
002400     03 REGISTER-DATA-LAES.                                               
002500        05 TIFINLV           PIC S9(5)           COMP-3.                  
002600*                                 PUBLICERINGSVECKA, (ÅÅVVD  D=1)         
002700        05 FLAVRART          PIC X.                                       
002800*                                 AVROPSARTIKEL                           
002900        05 FLJIT             PIC X.                                       
003000*                                 JUST-IN-TIME FLAGGA                     
003100        05 FLMANQ            PIC X.                                       
003200*                                 MANUELL HEMTAGNINGSKVANTITET            
003300        05 IDANSK            PIC S9(3)           COMP-3.                  
003400*                                 ANSKAFFARNUMMER                         
003500        05 IDLEVNR           PIC X(5).                                    
003600*                                 LEVERANTÖRNUMMER                        
003700        05 IDLKTO            PIC S9(7)           COMP-3.                  
003800*                                 LAGERKONTO (FFHHHUU)                    
003900        05 KDERS             OCCURS 2 TIMES                               
004000                             PIC S9(3)           COMP-3.                  
004100*                                 ERSÄTTNINGSKOD                          
004200        05 KDAVT             PIC S9              COMP-3.                  
004300*                                 AVTALSMÄRKNING                          
004400        05 KDHF              PIC S9              COMP-3.                  
004500*                                 HUVUDFÖRRÅDSMÄRKNING                    
004600        05 KDUART            PIC X.                                       
004700*                                 UNDANTAGSARTIKEL                        
004800        05 KDOPPLAN          PIC X.                                       
004900*                                 OPTIMAL PLAN INOM FRYSTID               
005000        05 FLOREGPB          OCCURS 2 TIMES                               
005100                             PIC X.                                       
005200*                                 OREGELBUNDEN PROGNOS (PB) ?             
005300        05 KDVVKL            PIC S9              COMP-3.                  
005400*                                 VOLYMVÄRDESKLASS                        
005500        05 KVAKS-PAV         PIC S9(7)           COMP-3.                  
005600*                                 DEL AV AK PÅ VÄG                        
005700        05 KVAKS             OCCURS 2 TIMES                               
005800                             PIC S9(7)           COMP-3.                  
005900*                                 ANKOMSTSALDO                            
006000        05 KVBK              PIC S9(7)           COMP-3.                  
006100*                                 EKONOMISK BESTÄLLNINGSKVANTITET         
006200        05 KVBR              PIC S9(7)           COMP-3.                  
006300*                                 BESTÄLLNINGSREST                        
006400        05 KVDAGAR-INLEV     PIC S9(3)           COMP-3.                  
006500*                                 INLEVERANSTID     (ANTAL DAGAR)         
006600        05 KVDAGAR-TT        PIC S9(3)           COMP-3.                  
006700*                                 DAGAR TULL- OCH TRANSPORT-TID           
006800        05 KVLAAN            PIC S9(7)           COMP-3.                  
006900*                                 LÅNESALDO                               
007000        05 KVLS-SDC-OVER     PIC S9(7)           COMP-3.                  
007100*                                 LAGERSALDO                              
007200        05 KVLS              OCCURS 2 TIMES                               
007300                             PIC S9(7)           COMP-3.                  
007400*                                 LAGERSALDO                              
007500        05 KVOKS-BULK        OCCURS 2 TIMES                               
007600                             PIC S9(7)           COMP-3.                  
007700*                                 ORDERKÖSALDO, KLASS 2-4                 
007800        05 KVOKS-DAG         OCCURS 2 TIMES                               
007900                             PIC S9(7)           COMP-3.                  
008000*                                 ORDERKÖSALDO, KLASS 1                   
008100        05 KVOKS-VOR         OCCURS 2 TIMES                               
008200                             PIC S9(7)           COMP-3.                  
008300*                                 ORDERKÖSALDO, VOR                       
008400        05 KVPALL            PIC S9(7)           COMP-3.                  
008500*                                 ANTAL I PALL                            
008600        05 KVPB-SEP          PIC S9(6)V9(1)      COMP-3.                  
008700*                                 SEPARAT PERIODBEHOV                     
008800        05 KVPB-SATS         PIC S9(6)V9(1)      COMP-3.                  
008900*                                 SATS-PERIODBEHOV                        
009000        05 KVPB-TPO          PIC S9(6)V9(1)      COMP-3.                  
009100*                                 PERIODBEHOV FÖR TPO1 OCH TPO2           
009200        05 KVPB-SDC          PIC S9(6)V9(1)      COMP-3.                  
009300*                                 PERIODBEHOV FÖR SAMTL SDC:ER            
009400        05 KVPB-TOT          OCCURS 2 TIMES                               
009500                             PIC S9(6)V9(1)      COMP-3.                  
009600*                                 TOTALT PERIODBEHOV                      
009700        05 KVQ               PIC S9(7)           COMP-3.                  
009800*                                 EKONOMISK HEMTAGNINGSKVANTITET          
009900        05 KVQ-JUST          PIC S9(7)           COMP-3.                  
010000*                                 NY EKON HEMTAGNINGSKVANTITET            
010100        05 KVQPACK-1         PIC S9(5)           COMP-3.                  
010200*                                 ANTAL I Q1 FÖRPACKNING                  
010300        05 KVRESS            OCCURS 2 TIMES                               
010400                             PIC S9(7)           COMP-3.                  
010500*                                 RESERVERAT ANTAL ARTIKLAR               
010600        05 KVROS             OCCURS 2 TIMES                               
010700                             PIC S9(7)           COMP-3.                  
010800*                                 RESTORDERSALDO                          
010900        05 KVSLAGER          OCCURS 2 TIMES                               
011000                             PIC S9(7)           COMP-3.                  
011100*                                 SÄKERHETSLAGER                          
011200        05 KVVECKOR-BT       PIC S9(3)           COMP-3.                  
011300*                                 ANTAL VECKOR BESTÄLLNINGSTID            
011400        05 KVVECKOR-FT       PIC S9(3)           COMP-3.                  
011500*                                 ANTAL VECKOR FRYSNINGSTID               
011600        05 KVVECKOR-LT       PIC S9(3)           COMP-3.                  
011700*                                 ANTAL VECKOR LEDTID                     
011800        05 PRARTBES          PIC S9(7)V9(2)      COMP-3.                  
011900*                                 BESTÄLLNINGSPRIS I KRONOR               
012000        05 RVPROFEL          OCCURS 2 TIMES                               
012100                             PIC S9(3)           COMP-3.                  
012200*                                 ANTAL STORA PROGNOSFEL                  
012300        05 RVPROURS          OCCURS 2 TIMES                               
012400                             PIC S9(3)           COMP-3.                  
012500*                                 ANTAL PROGNOSFEL I FÖLJD                
012600        05 TIQJUST           PIC S9(5)           COMP-3.                  
012700*                                 DATUM NY HEMTAGN KVANT (ÅÅVV)           
012800        05 IDLEVNR-SHIP      PIC X(5).                                    
012900*                                 SKEPPANDE LEVERANTÖR                    
013000     03 REGISTER-DATA-UPPD.                                               
013100        05 KDLPORS-GRP.                                                   
013200           07 KDLPORS-TAB    OCCURS 3 TIMES                               
013300                             PIC S9(3)           COMP-3.                  
013400*                                 LEVERANSPLANEORSAK                      
013500        05 KDLPSP            PIC S9              COMP-3.                  
013600*                                 LEVERANSPLANESPÄRR                      
013700        05 KDPLKOEP          PIC S9              COMP-3.                  
013800*                                 STATUS AVTALSKÖP (PLAN)                 
013900*                                 1=FÖRESLAGEN  2=GODKÄND                 
014000        05 KVBEST-PL         PIC S9(7)           COMP-3.                  
014100*                                 BESTÄLLNINGSKVANTITET PÅ PLAN           
014200        05 TIOMSPEC          PIC S9(5)           COMP-3.                  
014300*                                 OMSPECIFIKATIONSDATUM  (ÅÅVV)           
014400        05 TISPECST          PIC S9(5)           COMP-3.                  
014500*                                 SPECAD FR.O.M DATUM   (ÅÅVV)            
014600        05 TILPSP            PIC S9(5)           COMP-3.                  
014700*                                 DATUM LEVERANSPLAN-SPÄRR (ÅÅVV)         
014800        05 KDLEVPLF          PIC X.                                       
014900*                                 KOD FÖR LEVPLAN-GODKÄNNANDE             
015000        05 FLSKROT-WLC       PIC X.                                       
015100*                                 SISTA AVROP FÖRE SKROT                  
015200     03 FLRESEASON           PIC X.                                       
015300*                                 ALLMÄN FLAGGA                           
015400     03 FLASTERISK           PIC X.                                       
015500*                                 ALLMÄN FLAGGA                           
015600     03 KVSLUTKP             PIC S9(7)           COMP-3.                  
015700*                                 SLUTKÖPSSALDO                           
015800     03 KVUTRS               PIC S9(7)           COMP-3.                  
015900*                                 UTREDNINGSSALDO                         
016000     03 TIAVIDAT-SEN         PIC S9(7)           COMP-3.                  
016100*                                 SENASTE AVISERINGSDATUM  ÅÅMMDD         
016200     03 KDEFFMAN             PIC X.                                       
016300*                                 EMIL-KOD                                
016400     03 ADLAGOMR             PIC S9(3)           COMP-3.                  
016500*                                 LAGEROMRÅDE                             
016600     03 LEVDAGTAB.                                                        
016700        05 TILEVDAG          OCCURS 5 TIMES                               
016800                             PIC S9              COMP-3.                  
016900*                                 AVSÄNDNINGSDAG INOM VECKA               
017000     03 FLNYBER              PIC X.                                       
017100*                                 FLAGGA BERÄKN HEMTAGN NYTT SÄTT         
017200     03 KVULOAD              PIC S9(7)           COMP-3.                  
017300*                                 MIN ENHETSLAST FRÅN LEVERANTÖR          
017400     03 KVEOQ                PIC S9(7)           COMP-3.                  
017500*                                 BER. OPTIMAL HEMTAGNINGSKVANTIT         
017600*                                 ET                                      
017700     03 FLSKROT-BEV          PIC X.                                       
017800*                                 BEVAKNING 2 ÅR INFÖR SKROTNING          
017900     03 TISKPREL             PIC S9(5)           COMP-3.                  
018000*                                 PREL. SKROTNINGSDATUM (AAVV)            
018100*** END OF VILMAII-COPY LENGTH= 253 BYTES                                 
