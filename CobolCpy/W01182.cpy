000100 01  XLAG-W01182.                                                         
000200*                                 PART NO, IDDC, WDK722 DATA              
000300     03 XLAG-IDARTNR         PIC S9(9)           COMP-3.                  
000400*                                 ARTIKELNUMMER                           
000500*                                 PART NUMBER                             
000600     03 XLAG-IDDC            PIC X(2).                                    
000700*                                 IDENTIFIERARE LAGER                     
000800*                                 WAREHOUSE IDENTIFIER                    
000900     03 XLAG-WDK722.                                                      
001000*                                 ANSKAFFARINFO PER CN-DC                 
001100*                                 FYSISK NYCKEL KDSEGKEY                  
001200*                                  ALLTID = "1"                           
001300        05 XLAG-KDSEGKEY     PIC X.                                       
001400*                                 TEKNISK SEGMENT-NYCKEL                  
001500*                                 TECHNICAL SEGMENT KEY                   
001600        05 XLAG-DAPBPLAN     PIC 9(8).                                    
001700*                                 DATUM KVPB-PLAN GILTIG TOM              
001800*                                 DATE KVPB-PLAN VALID UNTIL              
001900        05 XLAG-DASEASON     PIC 9(8).                                    
002000*                                 DATUM RESEASON-LEDTID GILTIG TO         
002100*                                 M                                       
002200*                                 DATE RESEASON-LEDTID VALID UNTI         
002300*                                 L                                       
002400        05 XLAG-FLJIT        PIC X.                                       
002500*                                 JUST-IN-TIME FLAGGA                     
002600*                                 JUST-IN-TIME FLAG                       
002700        05 XLAG-IDANSK       PIC S9(3)           COMP-3.                  
002800*                                 ANSKAFFARNUMMER                         
002900*                                 PROCURER NO.                            
003000        05 XLAG-IDINK        PIC X(4).                                    
003100*                                 INKÖPARNUMMER                           
003200*                                 PURCHASE IDENTIFICATION NUMBER          
003300        05 XLAG-IDLEVNR-FRAM PIC X(5).                                    
003400*                                 FRAMTIDA LEVERANTÖRNUMMER               
003500*                                 THE SUPPLIER NAME IN THE FUTURE         
003600        05 XLAG-IDLEVNR-SHIP PIC X(5).                                    
003700*                                 SKEPPANDE LEVERANTÖR                    
003800*                                 SHIPPING SUPPLIER                       
003900        05 XLAG-IDPLANGR-AG  PIC S9              COMP-3.                  
004000*                                 PLANERINGSGRUPP ANSKAFFARE              
004100        05 XLAG-KDAVT        PIC S9              COMP-3.                  
004200*                                 AVTALSMÄRKNING                          
004300*                                 AGREEMENT CODE                          
004400        05 XLAG-KDLEVPLF     PIC X.                                       
004500*                                 KOD FÖR LEVPLAN-GODKÄNNANDE             
004600*                                 CODE FOR APPROVAL OF SCHEDULE P         
004700*                                 ROPOSAL                                 
004800        05 XLAG-KDLPSP       PIC S9              COMP-3.                  
004900*                                 LEVERANSPLANESPÄRR                      
005000        05 XLAG-FILLERX1     PIC X.                                       
005100        05 XLAG-KDOPPLAN     PIC X.                                       
005200*                                 OPTIMAL PLAN INOM FRYSTID               
005300*                                 OPTIMAL PLAN WITHIN FREEZTIME           
005400        05 XLAG-KVDAGAR-FFH  PIC S9(3)           COMP-3.                  
005500*                                 FRAMFÖRHÅLLNING   (ANTAL DAGAR)         
005600        05 XLAG-KVEOQ        PIC S9(7)           COMP-3.                  
005700*                                 BER. OPTIMAL HEMTAGNINGSKVANTIT         
005800*                                 ET                                      
005900        05 XLAG-KVPB-JUST1   PIC S9(6)V9(1)      COMP-3.                  
006000*                                 PERIODBEHOVSJUSTERING-1                 
006100*                                 PERIOD REQUIREMENTS-1                   
006200        05 XLAG-KVPB-JUST2   PIC S9(6)V9(1)      COMP-3.                  
006300*                                 PERIODBEHOVSJUSTERING-2                 
006400*                                 PERIOD REQUIREMENTS-2                   
006500        05 XLAG-KVPB-PLAN    PIC S9(6)V9(1)      COMP-3.                  
006600*                                 PLANERAT PERIODBEHOV                    
006700*                                 PLANNED PERIOD REQUIREMENTS             
006800        05 XLAG-KVPB-TREND   PIC S9(6)V9(1)      COMP-3.                  
006900*                                 PERIODTRENDVÄRDE                        
007000        05 XLAG-KVPALL       PIC S9(7)           COMP-3.                  
007100*                                 ANTAL I PALL                            
007200*                                 QUANTITY IN PALLET                      
007300        05 XLAG-KVSLAGER     PIC S9(7)           COMP-3.                  
007400*                                 SÄKERHETSLAGER                          
007500*                                 SAFETY STOCK                            
007600        05 XLAG-KVSLUTKP     PIC S9(7)           COMP-3.                  
007700*                                 SLUTKÖPSSALDO                           
007800        05 XLAG-KVSPANT      PIC S9(7)           COMP-3.                  
007900*                                 SPÄRRAT ANTAL                           
008000*                                 BLOCKED QTY                             
008100        05 XLAG-KVULOAD      PIC S9(7)           COMP-3.                  
008200*                                 MIN ENHETSLAST FRÅN LEVERANTÖR          
008300*                                 MIN LOAD FROM SUPPLIER                  
008400        05 XLAG-KVVECKOR-LT  PIC S9(3)           COMP-3.                  
008500*                                 ANTAL VECKOR LEDTID                     
008600        05 XLAG-KVVECKOR-FT  PIC S9(3)           COMP-3.                  
008700*                                 ANTAL VECKOR FRYSNINGSTID               
008800        05 XLAG-KVVECKOR-TREND                                            
008900                             PIC S9(3)           COMP-3.                  
009000*                                 ANTAL VECKOR TRENDVÄRDE                 
009100        05 XLAG-RESEASON-PLAN                                             
009200                             OCCURS 12 TIMES                              
009300                             PIC S9V9(2)         COMP-3.                  
009400*                                 SÄSONGSINDEX INKLUSIVE REFILL           
009500        05 XLAG-TIDATUM-TREND                                             
009600                             PIC S9(7)           COMP-3.                  
009700*                                 JUSTERAD TREND AAMMDD                   
009800*                                 LAST TREND CHANGE  YYMMDD               
009900        05 XLAG-TILPSP       PIC S9(5)           COMP-3.                  
010000*                                 DATUM LEVERANSPLAN-SPÄRR (ÅÅVV)         
010100        05 XLAG-TILEVDAG     OCCURS 5 TIMES                               
010200                             PIC S9              COMP-3.                  
010300*                                 AVSÄNDNINGSDAG INOM VECKA               
010400*                                 DELIVERY WEEK DAY                       
010500        05 XLAG-TILEVDAT     PIC S9(7)           COMP-3.                  
010600*                                 DATUM FRAMTIDA LEVERANTÖRNUMMER         
010700*                                 DATE FOR FUTURE SUPPLIER                
010800        05 XLAG-TIMANLED     PIC S9(7)           COMP-3.                  
010900*                                 SLUTDATUM MAN. LEDTID (ÅÅMMDD)          
011000*                                 END DATE MAN.LEAD TIME (YYMMDD)         
011100        05 XLAG-TIMANSEC     PIC S9(7)           COMP-3.                  
011200*                                 DATUM MANUELL SÄK-LAGER(ÅÅMMDD)         
011300*                                 DATE MAN. SAFETY STOCK (YYMMDD)         
011400        05 XLAG-TIOMSPEC     PIC S9(5)           COMP-3.                  
011500*                                 OMSPECIFIKATIONSDATUM  (ÅÅVV)           
011600        05 XLAG-TIREFSTO-LOC PIC S9(7)           COMP-3.                  
011700*                                 REF.STOPP T.O.M DATUM (NDC>LDC)         
011800*                                 STOPPED REF.UNTIL DATE(NDC>LDC)         
011900        05 XLAG-TISLUTKP     PIC S9(7)           COMP-3.                  
012000*                                 DÅ SLUTKÖPSDATUM BÖRJAR GÄLLA           
012100*                                 CALC OF ATR-BAL IS TO COMMENCE          
012200        05 XLAG-TIPBJUST-1   PIC S9(5)           COMP-3.                  
012300*                                 DATUM FÖRSTA PB-JUSTERING ÅÅVV          
012400*                                 FIRST PB-JUST DATE YYWW                 
012500        05 XLAG-TIPBJUST-2   PIC S9(5)           COMP-3.                  
012600*                                 DATUM ANDRA PB-JUSTERING ÅÅVV           
012700*                                 SECOND PB-JUST DATE YYWW                
012800        05 XLAG-IDLEVNR-SHIP-FRAM                                         
012900                             PIC X(5).                                    
013000*                                 FRAMTIDA SKEPP.LEVERANTÖRNUMMER         
013100*                                 THE SHIP.SUPPLIER IN THE FUTURE         
013200        05 XLAG-FLLARM-BUF   PIC X.                                       
013300*                                 BUFFERT-LARMRAPPORT UTFÄRDAD            
013400        05 XLAG-FILLER       PIC X(11).                                   
013500*** END OF VILMAII-COPY LENGTH= 177 BYTES                                 
