000100 01  XLAG-WDK722.                                                         
000200*                                 ANSKAFFARINFO PER CN-DC                 
000300*                                 FYSISK NYCKEL KDSEGKEY                  
000400*                                  ALLTID = "1"                           
000500     03 XLAG-KDSEGKEY        PIC X.                                       
000600*                                 TEKNISK SEGMENT-NYCKEL                  
000700*                                 TECHNICAL SEGMENT KEY                   
000800     03 XLAG-DAPBPLAN        PIC 9(8).                                    
000900*                                 DATUM KVPB-PLAN GILTIG TOM              
001000*                                 DATE KVPB-PLAN VALID UNTIL              
001100     03 XLAG-DASEASON        PIC 9(8).                                    
001200*                                 DATUM RESEASON-LEDTID GILTIG TO         
001300*                                 M                                       
001400*                                 DATE RESEASON-LEDTID VALID UNTI         
001500*                                 L                                       
001600     03 XLAG-FLJIT           PIC X.                                       
001700*                                 JUST-IN-TIME FLAGGA                     
001800*                                 JUST-IN-TIME FLAG                       
001900     03 XLAG-IDANSK          PIC S9(3)           COMP-3.                  
002000*                                 ANSKAFFARNUMMER                         
002100*                                 PROCURER NO.                            
002200     03 XLAG-IDINK           PIC X(4).                                    
002300*                                 INKÖPARNUMMER                           
002400*                                 PURCHASE IDENTIFICATION NUMBER          
002500     03 XLAG-IDLEVNR-FRAM    PIC X(5).                                    
002600*                                 FRAMTIDA LEVERANTÖRNUMMER               
002700*                                 THE SUPPLIER NAME IN THE FUTURE         
002800     03 XLAG-IDLEVNR-SHIP    PIC X(5).                                    
002900*                                 SKEPPANDE LEVERANTÖR                    
003000*                                 SHIPPING SUPPLIER                       
003100     03 XLAG-IDPLANGR-AG     PIC S9              COMP-3.                  
003200*                                 PLANERINGSGRUPP ANSKAFFARE              
003300     03 XLAG-KDAVT           PIC S9              COMP-3.                  
003400*                                 AVTALSMÄRKNING                          
003500*                                 AGREEMENT CODE                          
003600     03 XLAG-KDLEVPLF        PIC X.                                       
003700*                                 KOD FÖR LEVPLAN-GODKÄNNANDE             
003800*                                 CODE FOR APPROVAL OF SCHEDULE P         
003900*                                 ROPOSAL                                 
004000     03 XLAG-KDLPSP          PIC S9              COMP-3.                  
004100*                                 LEVERANSPLANESPÄRR                      
004200     03 XLAG-FILLERX1        PIC X.                                       
004300     03 XLAG-KDOPPLAN        PIC X.                                       
004400*                                 OPTIMAL PLAN INOM FRYSTID               
004500*                                 OPTIMAL PLAN WITHIN FREEZTIME           
004600     03 XLAG-KVDAGAR-FFH     PIC S9(3)           COMP-3.                  
004700*                                 FRAMFÖRHÅLLNING   (ANTAL DAGAR)         
004800     03 XLAG-KVEOQ           PIC S9(7)           COMP-3.                  
004900*                                 BER. OPTIMAL HEMTAGNINGSKVANTIT         
005000*                                 ET                                      
005100     03 XLAG-KVPB-JUST1      PIC S9(6)V9(1)      COMP-3.                  
005200*                                 PERIODBEHOVSJUSTERING-1                 
005300*                                 PERIOD REQUIREMENTS-1                   
005400     03 XLAG-KVPB-JUST2      PIC S9(6)V9(1)      COMP-3.                  
005500*                                 PERIODBEHOVSJUSTERING-2                 
005600*                                 PERIOD REQUIREMENTS-2                   
005700     03 XLAG-KVPB-PLAN       PIC S9(6)V9(1)      COMP-3.                  
005800*                                 PLANERAT PERIODBEHOV                    
005900*                                 PLANNED PERIOD REQUIREMENTS             
006000     03 XLAG-KVPB-TREND      PIC S9(6)V9(1)      COMP-3.                  
006100*                                 PERIODTRENDVÄRDE                        
006200     03 XLAG-KVPALL          PIC S9(7)           COMP-3.                  
006300*                                 ANTAL I PALL                            
006400*                                 QUANTITY IN PALLET                      
006500     03 XLAG-KVSLAGER        PIC S9(7)           COMP-3.                  
006600*                                 SÄKERHETSLAGER                          
006700*                                 SAFETY STOCK                            
006800     03 XLAG-KVSLUTKP        PIC S9(7)           COMP-3.                  
006900*                                 SLUTKÖPSSALDO                           
007000     03 XLAG-KVSPANT         PIC S9(7)           COMP-3.                  
007100*                                 SPÄRRAT ANTAL                           
007200*                                 BLOCKED QTY                             
007300     03 XLAG-KVULOAD         PIC S9(7)           COMP-3.                  
007400*                                 MIN ENHETSLAST FRÅN LEVERANTÖR          
007500*                                 MIN LOAD FROM SUPPLIER                  
007600     03 XLAG-KVVECKOR-LT     PIC S9(3)           COMP-3.                  
007700*                                 ANTAL VECKOR LEDTID                     
007800     03 XLAG-KVVECKOR-FT     PIC S9(3)           COMP-3.                  
007900*                                 ANTAL VECKOR FRYSNINGSTID               
008000     03 XLAG-KVVECKOR-TREND  PIC S9(3)           COMP-3.                  
008100*                                 ANTAL VECKOR TRENDVÄRDE                 
008200     03 XLAG-RESEASON-PLAN   OCCURS 12 TIMES                              
008300                             PIC S9V9(2)         COMP-3.                  
008400*                                 SÄSONGSINDEX INKLUSIVE REFILL           
008500     03 XLAG-TIDATUM-TREND   PIC S9(7)           COMP-3.                  
008600*                                 JUSTERAD TREND AAMMDD                   
008700*                                 LAST TREND CHANGE  YYMMDD               
008800     03 XLAG-TILPSP          PIC S9(5)           COMP-3.                  
008900*                                 DATUM LEVERANSPLAN-SPÄRR (ÅÅVV)         
009000     03 XLAG-TILEVDAG        OCCURS 5 TIMES                               
009100                             PIC S9              COMP-3.                  
009200*                                 AVSÄNDNINGSDAG INOM VECKA               
009300*                                 DELIVERY WEEK DAY                       
009400     03 XLAG-TILEVDAT        PIC S9(7)           COMP-3.                  
009500*                                 DATUM FRAMTIDA LEVERANTÖRNUMMER         
009600*                                 DATE FOR FUTURE SUPPLIER                
009700     03 XLAG-TIMANLED        PIC S9(7)           COMP-3.                  
009800*                                 SLUTDATUM MAN. LEDTID (ÅÅMMDD)          
009900*                                 END DATE MAN.LEAD TIME (YYMMDD)         
010000     03 XLAG-TIMANSEC        PIC S9(7)           COMP-3.                  
010100*                                 DATUM MANUELL SÄK-LAGER(ÅÅMMDD)         
010200*                                 DATE MAN. SAFETY STOCK (YYMMDD)         
010300     03 XLAG-TIOMSPEC        PIC S9(5)           COMP-3.                  
010400*                                 OMSPECIFIKATIONSDATUM  (ÅÅVV)           
010500     03 XLAG-TIREFSTO-LOC    PIC S9(7)           COMP-3.                  
010600*                                 REF.STOPP T.O.M DATUM (NDC>LDC)         
010700*                                 STOPPED REF.UNTIL DATE(NDC>LDC)         
010800     03 XLAG-TISLUTKP        PIC S9(7)           COMP-3.                  
010900*                                 DÅ SLUTKÖPSDATUM BÖRJAR GÄLLA           
011000*                                 CALC OF ATR-BAL IS TO COMMENCE          
011100     03 XLAG-TIPBJUST-1      PIC S9(5)           COMP-3.                  
011200*                                 DATUM FÖRSTA PB-JUSTERING ÅÅVV          
011300*                                 FIRST PB-JUST DATE YYWW                 
011400     03 XLAG-TIPBJUST-2      PIC S9(5)           COMP-3.                  
011500*                                 DATUM ANDRA PB-JUSTERING ÅÅVV           
011600*                                 SECOND PB-JUST DATE YYWW                
011700     03 XLAG-IDLEVNR-SHIP-FRAM                                            
011800                             PIC X(5).                                    
011900*                                 FRAMTIDA SKEPP.LEVERANTÖRNUMMER         
012000*                                 THE SHIP.SUPPLIER IN THE FUTURE         
012100     03 XLAG-FLLARM-BUF      PIC X.                                       
012200*                                 BUFFERT-LARMRAPPORT UTFÄRDAD            
012300     03 XLAG-FILLER          PIC X(11).                                   
012400*** END OF VILMAII-COPY LENGTH= 170 BYTES                                 
