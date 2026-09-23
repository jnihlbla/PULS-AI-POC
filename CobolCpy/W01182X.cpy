000100 01  XLAG-W01182X.                                                        
000200*                                 INFO FROM WDK722 - READABLE FOR         
000300*                                 MAT                                     
000400     03 XLAG-IDARTNR         PIC Z(7)9                                    
000500                             VALUE ZEROS.                                 
000600*                                 ARTIKELNUMMER                           
000700*                                 PART NUMBER                             
000800     03 XLAG-IDDC            PIC X(2)                                     
000900                             VALUE SPACES.                                
001000*                                 IDENTIFIERARE LAGER                     
001100*                                 WAREHOUSE IDENTIFIER                    
001200     03 XLAG-WDK722.                                                      
001300*                                 ANSKAFFARINFO PER CN-DC                 
001400*                                 FYSISK NYCKEL KDSEGKEY                  
001500*                                  ALLTID = "1"                           
001600        05 XLAG-KDSEGKEY     PIC X                                        
001700                             VALUE SPACE.                                 
001800*                                 TEKNISK SEGMENT-NYCKEL                  
001900*                                 TECHNICAL SEGMENT KEY                   
002000        05 XLAG-DAPBPLAN     PIC 9(8)                                     
002100                             VALUE ZEROS.                                 
002200*                                 DATUM KVPB-PLAN GILTIG TOM              
002300*                                 DATE KVPB-PLAN VALID UNTIL              
002400        05 XLAG-DASEASON     PIC 9(8)                                     
002500                             VALUE ZEROS.                                 
002600*                                 DATUM RESEASON-LEDTID GILTIG TO         
002700*                                 M                                       
002800*                                 DATE RESEASON-LEDTID VALID UNTI         
002900*                                 L                                       
003000        05 XLAG-FLJIT        PIC X                                        
003100                             VALUE SPACE.                                 
003200*                                 JUST-IN-TIME FLAGGA                     
003300*                                 JUST-IN-TIME FLAG                       
003400        05 XLAG-IDANSK       PIC Z(2)9                                    
003500                             VALUE ZEROS.                                 
003600*                                 ANSKAFFARNUMMER                         
003700*                                 PROCURER NO.                            
003800        05 XLAG-IDINK        PIC X(4)                                     
003900                             VALUE SPACES.                                
004000*                                 INKÖPARNUMMER                           
004100*                                 PURCHASE IDENTIFICATION NUMBER          
004200        05 XLAG-IDLEVNR-FRAM PIC X(5)                                     
004300                             VALUE SPACES.                                
004400*                                 FRAMTIDA LEVERANTÖRNUMMER               
004500*                                 THE SUPPLIER NAME IN THE FUTURE         
004600        05 XLAG-IDLEVNR-SHIP PIC X(5)                                     
004700                             VALUE SPACES.                                
004800*                                 SKEPPANDE LEVERANTÖR                    
004900*                                 SHIPPING SUPPLIER                       
005000        05 XLAG-IDPLANGR-AG  PIC 9                                        
005100                             VALUE ZERO.                                  
005200*                                 PLANERINGSGRUPP ANSKAFFARE              
005300        05 XLAG-KDAVT        PIC 9                                        
005400                             VALUE ZERO.                                  
005500*                                 AVTALSMÄRKNING                          
005600*                                 AGREEMENT CODE                          
005700        05 XLAG-KDLEVPLF     PIC X                                        
005800                             VALUE SPACE.                                 
005900*                                 KOD FÖR LEVPLAN-GODKÄNNANDE             
006000*                                 CODE FOR APPROVAL OF SCHEDULE P         
006100*                                 ROPOSAL                                 
006200        05 XLAG-KDLPSP       PIC 9                                        
006300                             VALUE ZERO.                                  
006400*                                 LEVERANSPLANESPÄRR                      
006500        05 XLAG-FILLERX1     PIC X                                        
006600                             VALUE SPACE.                                 
006700        05 XLAG-KDOPPLAN     PIC X                                        
006800                             VALUE SPACE.                                 
006900*                                 OPTIMAL PLAN INOM FRYSTID               
007000*                                 OPTIMAL PLAN WITHIN FREEZTIME           
007100        05 XLAG-KVDAGAR-FFH  PIC Z9                                       
007200                             VALUE ZEROS.                                 
007300*                                 FRAMFÖRHÅLLNING   (ANTAL DAGAR)         
007400        05 XLAG-KVEOQ        PIC Z(6)9                                    
007500                             VALUE ZEROS.                                 
007600*                                 BER. OPTIMAL HEMTAGNINGSKVANTIT         
007700*                                 ET                                      
007800        05 XLAG-KVPB-JUST1   PIC Z(5)9.9                                  
007900                             VALUE ZEROS.                                 
008000*                                 PERIODBEHOVSJUSTERING-1                 
008100*                                 PERIOD REQUIREMENTS-1                   
008200        05 XLAG-KVPB-JUST2   PIC Z(5)9.9                                  
008300                             VALUE ZEROS.                                 
008400*                                 PERIODBEHOVSJUSTERING-2                 
008500*                                 PERIOD REQUIREMENTS-2                   
008600        05 XLAG-KVPB-PLAN    PIC Z(5)9.9                                  
008700                             VALUE ZEROS.                                 
008800*                                 PLANERAT PERIODBEHOV                    
008900*                                 PLANNED PERIOD REQUIREMENTS             
009000        05 XLAG-KVPB-TREND   PIC Z(5)9.9                                  
009100                             VALUE ZEROS.                                 
009200*                                 PERIODTRENDVÄRDE                        
009300        05 XLAG-KVPALL       PIC Z(6)9                                    
009400                             VALUE ZEROS.                                 
009500*                                 ANTAL I PALL                            
009600*                                 QUANTITY IN PALLET                      
009700        05 XLAG-KVSLAGER     PIC Z(5)9                                    
009800                             VALUE ZEROS.                                 
009900*                                 SÄKERHETSLAGER                          
010000*                                 SAFETY STOCK                            
010100        05 XLAG-KVSLUTKP     PIC Z(6)9                                    
010200                             VALUE ZEROS.                                 
010300*                                 SLUTKÖPSSALDO                           
010400        05 XLAG-KVSPANT      PIC -(6)9                                    
010500                             VALUE ZEROS.                                 
010600*                                 SPÄRRAT ANTAL                           
010700*                                 BLOCKED QTY                             
010800        05 XLAG-KVULOAD      PIC Z(6)9                                    
010900                             VALUE ZEROS.                                 
011000*                                 MIN ENHETSLAST FRÅN LEVERANTÖR          
011100*                                 MIN LOAD FROM SUPPLIER                  
011200        05 XLAG-KVVECKOR-LT  PIC Z9                                       
011300                             VALUE ZEROS.                                 
011400*                                 ANTAL VECKOR LEDTID                     
011500        05 XLAG-KVVECKOR-FT  PIC Z9                                       
011600                             VALUE ZEROS.                                 
011700*                                 ANTAL VECKOR FRYSNINGSTID               
011800        05 XLAG-KVVECKOR-TREND                                            
011900                             PIC Z9                                       
012000                             VALUE ZEROS.                                 
012100*                                 ANTAL VECKOR TRENDVÄRDE                 
012200        05 XLAG-RESEASON-PLAN                                             
012300                             OCCURS 12 TIMES                              
012400                             PIC 9.9(2)                                   
012500                             VALUE ZEROS.                                 
012600*                                 SÄSONGSINDEX INKLUSIVE REFILL           
012700        05 XLAG-TIDATUM-TREND                                             
012800                             PIC 9(6)                                     
012900                             VALUE ZEROS.                                 
013000*                                 JUSTERAD TREND AAMMDD                   
013100*                                 LAST TREND CHANGE  YYMMDD               
013200        05 XLAG-TILPSP       PIC 9(4)                                     
013300                             VALUE ZEROS.                                 
013400*                                 DATUM LEVERANSPLAN-SPÄRR (ÅÅVV)         
013500        05 XLAG-TILEVDAG     OCCURS 5 TIMES                               
013600                             PIC 9                                        
013700                             VALUE ZERO.                                  
013800*                                 AVSÄNDNINGSDAG INOM VECKA               
013900*                                 DELIVERY WEEK DAY                       
014000        05 XLAG-TILEVDAT     PIC 9(6)                                     
014100                             VALUE ZEROS.                                 
014200*                                 DATUM FRAMTIDA LEVERANTÖRNUMMER         
014300*                                 DATE FOR FUTURE SUPPLIER                
014400        05 XLAG-TIMANLED     PIC 9(6)                                     
014500                             VALUE ZEROS.                                 
014600*                                 SLUTDATUM MAN. LEDTID (ÅÅMMDD)          
014700*                                 END DATE MAN.LEAD TIME (YYMMDD)         
014800        05 XLAG-TIMANSEC     PIC 9(6)                                     
014900                             VALUE ZEROS.                                 
015000*                                 DATUM MANUELL SÄK-LAGER(ÅÅMMDD)         
015100*                                 DATE MAN. SAFETY STOCK (YYMMDD)         
015200        05 XLAG-TIOMSPEC     PIC 9(4)                                     
015300                             VALUE ZEROS.                                 
015400*                                 OMSPECIFIKATIONSDATUM  (ÅÅVV)           
015500        05 XLAG-TIREFSTO-LOC PIC 9(6)                                     
015600                             VALUE ZEROS.                                 
015700*                                 REF.STOPP T.O.M DATUM (NDC>LDC)         
015800*                                 STOPPED REF.UNTIL DATE(NDC>LDC)         
015900        05 XLAG-TISLUTKP     PIC Z(6)                                     
016000                             VALUE ZEROS.                                 
016100*                                 DÅ SLUTKÖPSDATUM BÖRJAR GÄLLA           
016200*                                 CALC OF ATR-BAL IS TO COMMENCE          
016300        05 XLAG-TIPBJUST-1   PIC 9(4)                                     
016400                             VALUE ZEROS.                                 
016500*                                 DATUM FÖRSTA PB-JUSTERING ÅÅVV          
016600*                                 FIRST PB-JUST DATE YYWW                 
016700        05 XLAG-TIPBJUST-2   PIC 9(4)                                     
016800                             VALUE ZEROS.                                 
016900*                                 DATUM ANDRA PB-JUSTERING ÅÅVV           
017000*                                 SECOND PB-JUST DATE YYWW                
017100        05 XLAG-IDLEVNR-SHIP-FRAM                                         
017200                             PIC X(5)                                     
017300                             VALUE SPACES.                                
017400*                                 FRAMTIDA SKEPP.LEVERANTÖRNUMMER         
017500*                                 THE SHIP.SUPPLIER IN THE FUTURE         
017600        05 XLAG-FLLARM-BUF   PIC X                                        
017700                             VALUE SPACE.                                 
017800*                                 BUFFERT-LARMRAPPORT UTFÄRDAD            
017900        05 XLAG-FILLER       PIC X(11)                                    
018000                             VALUE SPACES.                                
018100*** END OF VILMAII-COPY LENGTH= 254 BYTES                                 
