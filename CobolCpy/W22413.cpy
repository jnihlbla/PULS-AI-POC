000100 01  UPD-W22413.                                                          
000200*                                 UPP-FIL FÖR LEVERANSPLANER              
000300*                                 POSTTYP = 001 UPPDATE WDK711            
000400*                                 KVREFBER, KVREFPKT, KVREFOVL            
000500*                                 TIREFPAF, TIREFPKT                      
000600*                                                                         
000700*                                 POSTTYP = 002 UPPDATE WDK722            
000800*                                 KVSLAGER, KVEOQ, TIMANSEC,              
000900*                                 KDLPSP, KVSLUTKP, TILPSP,               
001000*                                 KDLEVPLF, DAPBPLAN, KVPB-PLAN           
001100*                                 DASEASON, RESEASON-PLAN(1--12)          
001200*                                                                         
001300*                                 POSTTYP = 003 INSERT  WDD901            
001400*                                 IDARTNR, IDDC                           
001500*                                                                         
001600*                                 POSTTYP = 004 INSERT  WDD902            
001700*                                 IDARTNR, IDDC, IDLEVNR                  
001800*                                                                         
001900*                                 POSTTYP = 008 DELETE WDD901             
002000*                                 IDARTNR, IDDC                           
002100*                                                                         
002200*                                 POSTTYP = 009 DELETE WDD904             
002300*                                 IDARTNR, IDDC, IDLEVNR,                 
002400*                                                                         
002500*                                 POSTTYP = 010 DELETE WDD601             
002600*                                 IDARTNR, IDDC                           
002700*                                                                         
002800*                                 POSTTYP = 011 DELETE WDD905             
002900*                                 IDARTNR, IDDC, IDLEVNR,                 
003000*                                 DAAVROP-AVS, TILEVDAG                   
003100*                                                                         
003200*                                 POSTTYP = 012 REPLACE WDD905            
003300*                                 IDARTNR, IDDC, IDLEVNR,                 
003400*                                 DAAVROP-AVS, TILEVDAG, KVAVROP          
003500*                                                                         
003600*                                 POSTTYP = 013 INSERT WDD905             
003700*                                 IDARTNR, IDDC, IDLEVNR,                 
003800*                                 KDAVROP, DAAVROP-AVS, TILEVDAG,         
003900*                                 TIAVRDAT-INL, TIAVRDAT-DISP,            
004000*                                 KVAVROP                                 
004100*                                                                         
004200     03 UPD-IDPTYP           PIC X(3).                                    
004300*                                 POSTTYP                                 
004400*                                 RECORD TYPE                             
004500     03 UPD-IDARTNR          PIC S9(9)           COMP-3.                  
004600*                                 ARTIKELNUMMER                           
004700*                                 PART NUMBER                             
004800     03 UPD-IDDC             PIC X(2).                                    
004900*                                 IDENTIFIERARE LAGER                     
005000*                                 WAREHOUSE IDENTIFIER                    
005100     03 UPD-IDLEVNR          PIC X(5).                                    
005200*                                 LEVERANTÖRNUMMER                        
005300*                                 SUPPLIER NUMBER (VENDOR NUMBER)         
005400     03 UPD-DAAVROP-AVS      PIC 9(6).                                    
005500*                                 AVSÄNDNINGSVECKA (PLANERAD)             
005600*                                 (ÅÅÅÅVV)                                
005700     03 UPD-DAPBPLAN         PIC 9(8).                                    
005800*                                 DATUM KVPB-PLAN GILTIG TOM              
005900*                                 DATE KVPB-PLAN VALID UNTIL              
006000     03 UPD-DASEASON         PIC 9(8).                                    
006100*                                 DATUM RESEASON-LEDTID GILTIG TO         
006200*                                 M                                       
006300*                                 DATE RESEASON-LEDTID VALID UNTI         
006400*                                 L                                       
006500     03 UPD-IDANSK           PIC S9(3)           COMP-3.                  
006600*                                 ANSKAFFARNUMMER                         
006700*                                 PROCURER NO.                            
006800     03 UPD-KDAVROP          PIC S9              COMP-3.                  
006900*                                 AVROPSKOD                               
007000*                                 CALLED                                  
007100     03 UPD-KDLEVPLF         PIC X.                                       
007200*                                 KOD FÖR LEVPLAN-GODKÄNNANDE             
007300*                                 CODE FOR APPROVAL OF SCHEDULE P         
007400*                                 ROPOSAL                                 
007500     03 UPD-KDLPSP           PIC S9              COMP-3.                  
007600*                                 LEVERANSPLANESPÄRR                      
007700     03 UPD-KVAVROP          PIC S9(7)           COMP-3.                  
007800*                                 AVROPSKVANTITET                         
007900     03 UPD-KVEOQ            PIC S9(7)           COMP-3.                  
008000*                                 BER. OPTIMAL HEMTAGNINGSKVANTIT         
008100*                                 ET                                      
008200     03 UPD-KVPB-PLAN        PIC S9(6)V9(1)      COMP-3.                  
008300*                                 PLANERAT PERIODBEHOV                    
008400*                                 PLANNED PERIOD REQUIREMENTS             
008500     03 UPD-KVREFBER         PIC S9(7)           COMP-3.                  
008600*                                 BERÄKNAD REFILLINGKVANTITET             
008700*                                 CALCULATED REFILLING QUANTITY           
008800     03 UPD-KVREFOVL         PIC S9(7)           COMP-3.                  
008900*                                 BERÄKNAD ÖVERLAGERPUNKT                 
009000*                                 CALCULATED OVERSTOCK POINT              
009100     03 UPD-KVREFPKT         PIC S9(7)           COMP-3.                  
009200*                                 BERÄKNAD PÅFYLLNADSPUNKT                
009300*                                 CALCULATED REFILLING POINT              
009400     03 UPD-KVSLAGER         PIC S9(7)           COMP-3.                  
009500*                                 SÄKERHETSLAGER                          
009600*                                 SAFETY STOCK                            
009700     03 UPD-KVSLUTKP         PIC S9(7)           COMP-3.                  
009800*                                 SLUTKÖPSSALDO                           
009900     03 UPD-RESEASON-PLAN    OCCURS 12 TIMES                              
010000                             PIC S9V9(2)         COMP-3.                  
010100*                                 SÄSONGSINDEX INKLUSIVE REFILL           
010200     03 UPD-TIAVRDAT-INL     PIC S9(7)           COMP-3.                  
010300*                                 PLANERAT INLEVERANSDATUM                
010400     03 UPD-TIAVRDAT-DISP    PIC S9(7)           COMP-3.                  
010500*                                 PLANERAT DISPONIBLEDATUM                
010600     03 UPD-TILEVDAG         PIC S9              COMP-3.                  
010700*                                 AVSÄNDNINGSDAG INOM VECKA               
010800*                                 DELIVERY WEEK DAY                       
010900     03 UPD-TILPSP           PIC S9(5)           COMP-3.                  
011000*                                 DATUM LEVERANSPLAN-SPÄRR (ÅÅVV)         
011100     03 UPD-TIMANSEC         PIC S9(7)           COMP-3.                  
011200*                                 DATUM MANUELL SÄK-LAGER(ÅÅMMDD)         
011300*                                 DATE MAN. SAFETY STOCK (YYMMDD)         
011400     03 UPD-TIREFPAF         PIC S9(7)           COMP-3.                  
011500*                                 DATUM MANUELL PÅFYLLNADSKVANT           
011600*                                 DATE MANUAL REFILLING QTY               
011700     03 UPD-TIREFPKT         PIC S9(7)           COMP-3.                  
011800*                                 DATUM MANUELL REFILLPUNKT               
011900*                                 DATE MANUAL REFILLING POINT             
012000*** END OF VILMAII-COPY LENGTH= 122 BYTES                                 
