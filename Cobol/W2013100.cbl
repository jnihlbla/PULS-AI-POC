000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W2013100.                                                
000300 AUTHOR.         EWA ELIASSON.                                            
000400 DATE-WRITTEN.   AUGUSTI 1984.                                            
000500                                                                          
000600*                                                                         
000700*    FUNKTION.   UPPDATERING AV ANSKAFFNINGSDATA                          
000800*                UPPDATERAR WDG3 WLXXBJ11     LÄSER WDD3                  
000900*                                WLXXBI11           WDK6                  
001000*                           WDK6 WLARTC11           WDF1                  
001100*                           WDK7 WLARTS11           WDB6                  
001200*                                                                         
001300*    INDATA.                                                              
001400*        TRANSAKTION: W2T131                                              
001500*        MID:         W2I13101                                            
001600*                                                                         
001700*    UTDATA.                                                              
001800*        MOD:         W2O13101                                            
001900*                                                                         
002000*   ÄNDRINGAR:                                                            
002100*        04-09-20. E-TRACKER 1459688                                      
002200*                  LAGT TILL ENH.LAST I FÄLT DÄR REFILLART                
002300*                  TIDIGARE LÅG. REFILLART FLYTTAT TILL FÄLT KSP,         
002400*                  VILKET NU ÄR FLYTTAT TILL BILD 2132,                   
002500*                  PÅ UPPDRAG AV CDC TILLGÄNGLIG. //L.A.                  
002600*                                                                         
002700*        03-05-15. TILLAGT FUNKTION FÖR ATT BEGRÄNSA INFORMATION          
002800*                  FÖR USER VARS SEC-IDLEVNR PÅ USER-BASEN                
002900*                  INTE ÄR LIKA MED HUVUDLEVERANTÖREN.                    
003000*                  ( SEC-IDLEVNR = SPACE, FÅR SE ALLT )    /C.E.          
003100*                                                                         
003200*        15-04-22. ETRACKER 10130993                                      
003300*                  REDUCE NUMBER OF DELIVERY SCHEDULES                    
003400*                                                                         
003500*        17-09-25  JIRA PULS-842                                          
003600*                  ADD CONTROL ON SCREEN 2131WHEN UPDATING                
003700*                  CLAG-KVVECKOR-LT: MAX 52 VECKOR                        
003800*        21-07-20  2230520 /                                              
003900*                  ADD LOGIC TO VALIDATE AND CHANGE THE                   
004000*                  SUPPLIER.SWEDISH FORMAT IS REMOVED.                    
004100*                                                                         
004200     SKIP3                                                                
004300 ENVIRONMENT DIVISION.                                                    
004400     SKIP3                                                                
004500                                                                          
004600 DATA DIVISION.                                                           
004700     EJECT                                                                
004800 WORKING-STORAGE SECTION.                                                 
004900                                                                          
005000*    -- CHECKED BY WY2000                                                 
005100 77  PROGRAM-NAMN                PIC X(8)    VALUE 'W2013100'.            
005200 77  JA                          PIC X       VALUE 'J'.                   
005300 77  NEJ                         PIC X       VALUE 'N'.                   
005400 77  INDX                        PIC S9(9)   VALUE +1   COMP SYNC.        
005500 77  WS-CDC-SE                   PIC X(2)    VALUE '11'.                  
005600 77  IDARTNR-WS                  PIC X(9).                                
005700 77  WS-IDARTNR REDEFINES IDARTNR-WS  PIC 9(9).                           
005800 77  INDATA-OK                   PIC X(1)    VALUE SPACE.                 
005900 77  KOLLA-FAKTOR-DATUM-C1       PIC X(1)    VALUE SPACE.                 
006000 77  KOLLA-FAKTOR-DATUM-C2       PIC X(1)    VALUE SPACE.                 
006100 77  SW-2204                     PIC X(1)    VALUE SPACE.                 
006200 77  SW-2213                     PIC X(1)    VALUE SPACE.                 
006300 77  SW-IDANSK-UPPD              PIC X(1)    VALUE SPACE.                 
006400 77  SW-UPPDAT-ARTC12            PIC X(1)    VALUE SPACE.                 
006500 77  SW-UPPDAT-ARTC12-OK         PIC X(1)    VALUE SPACE.                 
006600 77  SW-UPPDAT-ARTC23            PIC X(1)    VALUE SPACE.                 
006700 77  SW-UPPDAT-ARTC91            PIC X(1)    VALUE SPACE.                 
006800 77  SW-UPPDAT-ARTS11            PIC X(1)    VALUE SPACE.                 
006900 77  TAECKNING                   PIC X(1)    VALUE SPACE.                 
007000 77  RANS-GRANS                  PIC S9(7)   COMP-3 VALUE +0.             
007100 77  DISPONIBELT                 PIC S9(7)   COMP-3 VALUE +0.             
007200 77  W-ART-KVLS                  PIC S9(7)   COMP-3 VALUE +0.             
007300 77  W-ART-KVUTRS                PIC S9(7)   COMP-3 VALUE +0.             
007400 77  W-ART-KVRESS                PIC S9(7)   COMP-3 VALUE +0.             
007500 77  W-ART-KVSPANT               PIC S9(7)   COMP-3 VALUE +0.             
007600 77  W-ART-KDERS                 PIC S9(3)   COMP-3 VALUE +0.             
007700 77  W-ART-KVSLAGER              PIC S9(7)   COMP-3 VALUE +0.             
007800 77  W-ART-KDLTK                 PIC S9      COMP-3 VALUE +0.             
007900 77  IDLEVNR-ALFA                PIC X(5)    VALUE SPACE.                 
008000 77  WS-IDLEVNR-NUM              PIC 9(5)    VALUE ZERO.                  
008100 77  W-TIAAVV                    PIC 9(4)    VALUE ZERO.                  
008200 77  WS-TIAAVV                   PIC S9(5)   VALUE ZERO COMP-3.           
008300 77  W-ANTAL-VECKOR              PIC 9(3)    VALUE ZERO COMP-3.           
008400 77  WS-SPAR-IDANSK              PIC 9(3)    VALUE ZERO.                  
008500 77  WS-IDLEVNR-8                PIC X(8)    VALUE SPACE.                 
008600 77  TEST-IDINK                  PIC 9(3)    VALUE ZERO.                  
008700 77  WS-EVENT-FOUND              PIC X(1)    VALUE SPACE.                 
008800     EJECT                                                                
008900 01  W-IDARTNR-8                 PIC 9(8)    VALUE ZERO.                  
009000 01  W-IDLOGLOP                  PIC S9(1) VALUE ZERO.                    
009100 01  SW-SUPPLIER-VAL-OK          PIC X(1)    VALUE SPACE.                 
009200 01  SW-MFG-SHIP-PRESENT         PIC X(1)    VALUE SPACE.                 
009300 01  SW-ERR-MSG-SUPPLIER         PIC X(1)    VALUE SPACE.                 
009400                                                                          
009500 01  DYNAMISKA-SUBPROGRAM.                                                
009600     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
009700     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
009800     03  W005INIT                PIC X(8)    VALUE 'W005INIT'.            
009900     03  WDATKONV                PIC X(8)    VALUE 'WDATKONV'.            
010000     03  W009VADD                PIC X(8)    VALUE 'W009VADD'.            
010100     03  WMEDKONV                PIC X(8)    VALUE 'WMEDKONV'.            
010200                                                                          
010300*    --- PARAMETRAR TILL SUBPROGRAM WMEDKONV                              
010400*01 -COPY WMEDAREA                                                        
010500                                                                          
010600     EJECT                                                                
010700*01  AREA   -COPY W092W001     -PRE W092-.                                
010800     EJECT                                                                
010900*01         -COPY A310TB65     -PRE A310-                                 
011000     EJECT                                                                
011100*                    ****   PARAMETRAR TILL W005INIT                      
011200*01  -COPY WMSGINIT                                                       
011300     EJECT                                                                
011400*    --- PARAMETRAR TILL DATKONV                                          
011500*                                                                         
011600*01  -COPY WDATAREA                                                       
011700     EJECT                                                                
011800*01  -COPY WWDC99                                                         
011900     EJECT                                                                
012000 01  FILLER                    PIC X(16) VALUE 'HJÄLP FÄLT I WS'.         
012100 01  SATS-SW                   PIC X.                                     
012200     88  INGAR-I-SATS                        VALUE 'J'.                   
012300     88  INGAR-EJ-I-SATS                     VALUE 'N'.                   
012400                                                                          
012500 77  SECURITY-SW                 PIC X       VALUE 'N'.                   
012600     88  PASSED-SECURITY-CHECK               VALUE 'J'.                   
012700     88  BLOCKED-SECURITY-CHECK              VALUE 'N'.                   
012800                                                                          
012900 01  FILLER.                                                              
013000     03  WS-RESLJUST             PIC 9(2)V9(1) VALUE ZERO.                
013100     03  FILLER REDEFINES WS-RESLJUST.                                    
013200         05  FILLER              PIC X.                                   
013300         05  WS-HELTAL           PIC 9.                                   
013400         05  WS-DECIMAL          PIC 9.                                   
013500                                                                          
013600     03  RESLJUST-WS.                                                     
013700         05  HELTAL              PIC 9(1) VALUE ZERO.                     
013800         05  PUNKT               PIC X    VALUE SPACE.                    
013900         05  DECIMAL             PIC 9(1) VALUE ZERO.                     
014000                                                                          
014100     03  NUM-RESLJUST.                                                    
014200         05  NUM-HELTAL          PIC 9(1) VALUE ZERO.                     
014300         05  NUM-DECIMAL         PIC 9(1) VALUE ZERO.                     
014400     03  FILLER REDEFINES NUM-RESLJUST.                                   
014500         05 N-TAL                PIC 9V9.                                 
014600                                                                          
014700     03  WS-RESLJUST-C1          PIC 9V9  VALUE ZERO.                     
014800     03  WS-RESLJUST-C2          PIC 9V9  VALUE ZERO.                     
014900     03  WS-TISLJUST-C1          PIC 9(4) VALUE ZERO.                     
015000     03  WS-TISLJUST-C2          PIC 9(4) VALUE ZERO.                     
015100     03  WS-IDANSK               PIC 9(3) VALUE ZERO.                     
015200     03  WS-KVSPANT              PIC 9(7) VALUE ZERO.                     
015300     03  WS-IDPLANGR-AG          PIC 9(1) VALUE ZERO.                     
015400     03  WS-IDPLANGR-LEV         PIC 9(1) VALUE ZERO.                     
015500     03  WS-KVVECKOR-LT          PIC 9(3) VALUE ZERO.                     
015600     03  WS-KVVECKOR-FT          PIC 9(3) VALUE ZERO.                     
015700     03  WS-TISLJUST             PIC 9(4) VALUE ZERO.                     
015800     03  WS-KVVECKOR-AT          PIC 9(3) VALUE ZERO.                     
015900     03  WS-KVQ                  PIC 9(7) VALUE ZERO.                     
016000     03  WS-KVBK                 PIC 9(7) VALUE ZERO.                     
016100     03  WS-KVPALL               PIC 9(7) VALUE ZERO.                     
016200     03  WS-KVSLAGER-C1          PIC 9(7) VALUE ZERO.                     
016300     03  WS-KVSLAGER-C2          PIC 9(7) VALUE ZERO.                     
016400     03  WS-KVQ-JUST             PIC 9(7) VALUE ZERO.                     
016500     03  WS-TIREFSTO             PIC 9(6) VALUE ZERO.                     
016600     03  WS-TIREFSTO-UT          PIC 9(7) VALUE ZERO.                     
016700     03  WS-TIQJUST              PIC 9(4) VALUE ZERO.                     
016800                                                                          
016900                                                                          
017000 01  WORK-VARIABLES.                                                      
017100     03  W-IDLEVNR           PIC X(5)    VALUE SPACE.                     
017200     03  W-IDLEVNR-NUM       PIC X(5).                                    
017300     03  W-IDLEVNR-SHIP      PIC X(5)    VALUE SPACE.                     
017400     03  W-IDLEVNR-MOTSV     PIC X(5)    VALUE SPACE.                     
017500     03  W-EVNT-IDLEVNR      PIC X(5)    VALUE SPACE.                     
017600     03  W-EVNT-IDLEVNR-SHIP PIC X(5)    VALUE SPACE.                     
017700     03  W-WDK601-IDLEVNR      PIC X(5)    VALUE SPACE.                   
017800     03  W-WDK611-IDLEVNR-SHIP PIC X(5)    VALUE SPACE.                   
017900     03  W-WDK611-IDDC-REF     PIC X(2)    VALUE SPACE.                   
018000     03  W-WDK727-IDDC-REF     PIC X(2)    VALUE SPACE.                   
018100     03  W-EXT-TO-REF       PIC X       VALUE SPACE.                      
018200     03  W-REF-TO-EXT       PIC X       VALUE SPACE.                      
018300     03  W-REF-TO-REF       PIC X       VALUE SPACE.                      
018400     03  W-REF-DC-OK        PIC X       VALUE 'N'.                        
018500     03  W-WDK611-ABSENT    PIC X       VALUE SPACE.                      
018600                                                                          
018700 01  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
018800 01  FILLER REDEFINES DAGENS-DATUM.                                       
018900     03  DAGENS-DATUM-AAR        PIC 9(2).                                
019000     03  DAGENS-DATUM-MAANAD     PIC 9(2).                                
019100     03  DAGENS-DATUM-DAG        PIC 9(2).                                
019200                                                                          
019300     EJECT                                                                
019400 01  W-IDAVTAL-RED               PIC 9(13).                               
019500 01  W-IDAVTAL REDEFINES W-IDAVTAL-RED.                                   
019600     03  FILLER                  PIC X.                                   
019700     03  W-PREFIX                PIC X(3).                                
019800     03  W-AVTALSNR              PIC X(6).                                
019900     03  W-SUFFIX                PIC X(3).                                
020000     SKIP2                                                                
020100*01 -COPY WWBYT03                                                         
020200     EJECT                                                                
020300*01 -COPY WWPRODSL                                                        
020400     EJECT                                                                
020500* - - - - - - - - - - - - - - - - - - -  NYCKLAR TILL DLI                 
020600 01  FILLER                      PIC X(16)  VALUE 'NYCKLAR-T-DLI'.        
020700 01  NYCKLAR-TILL-DLI.                                                    
020800                                                                          
020900   03  W-IDARTNR-KEY-X.                                                   
021000     05  W-IDARTNR-KEY           PIC S9(9)   VALUE ZERO  COMP-3.          
021100                                                                          
021200   03  W-IDLEVNR-KEY-X.                                                   
021300     05  W-IDLEVNR-KEY           PIC X(5)    VALUE SPACE.                 
021400                                                                          
021500   03  W-IDSKYLT-KEY-X.                                                   
021600     05  W-IDSKYLT-KEY           PIC X(3)    VALUE SPACE.                 
021700                                                                          
021800   03  W-2203-KEY-X.                                                      
021900     05  FILLER                  PIC X(4)    VALUE '2203'.                
022000     05  W-IDDC-2203             PIC X(2)    VALUE '11'.                  
022100     05  FILLER                  PIC X(24)   VALUE LOW-VALUE.             
022200                                                                          
022300   03  W-2213-KEY-X.                                                      
022400     05  FILLER                  PIC X(4)    VALUE '2213'.                
022500     05  W-IDDC-2213             PIC X(2)    VALUE '11'.                  
022600     05  FILLER                  PIC X(24)   VALUE LOW-VALUE.             
022700                                                                          
022800   03  W-WDGX-4505-KEY-X.                                                 
022900     05  W-IDHTYP-4505           PIC X(4)    VALUE '4505'.                
023000     05  W-IDDC-4505             PIC X(2)    VALUE '11'.                  
023100     05  FILLER                  PIC X(24)   VALUE LOW-VALUE.             
023200                                                                          
023300     03  W-IDDC-B6-X.                                                     
023400         05  W-IDDC-B6       PIC X(2)   VALUE SPACE.                      
023500     03  W-IDDC-REF-X.                                                    
023600         05  W-IDDC-REF          PIC X(2)    VALUE SPACE.                 
023700     03  W-WDB615KY-X.                                                    
023800         05  W-IDTRANS-B6        PIC X(4)    VALUE SPACE.                 
023900         05  W-IDDC-REF-B6       PIC X(2)    VALUE SPACE.                 
024000                                                                          
024100   03 W-IDDC-WDK7                PIC X(2) VALUE SPACES.                   
024200                                                                          
024300     EJECT                                                                
024400 01  MESSAGE-CODES.                                                       
024500     03  INF-REFILL-PART        PIC X(3)    VALUE '434'.                  
024600     EJECT                                                                
024700* - - - - - - - - - - - - - - - - - - -  MEDDELANDEN                      
024800 01  FILLER                      PIC X(16)   VALUE 'MEDDELANDEN'.         
024900 01  UPPDAT.                                                              
025000      03  FILLER                 PIC X(40)                                
025100          VALUE '  UPPDATERING GJORD                  '.                  
025200     SKIP2                                                                
025300 01  KVQFEL.                                                              
025400      03  FILLER                 PIC X(40)                                
025500          VALUE 'Q-JUST MÅSTE UPPD. NÄR DATQ-JUST UPPDAT'.                
025600     SKIP2                                                                
025700 01  TIQFEL.                                                              
025800      03  FILLER                 PIC X(40)                                
025900          VALUE 'DATQ-JUST MÅSTE UPPD. NÄR Q-JUST UPPDAT'.                
026000     SKIP2                                                                
026100 01  FEL.                                                                 
026200      03  FEL1                   PIC X(40)                                
026300          VALUE '  UPPLYSTA FÄLT FEL                  '.                  
026400     SKIP2                                                                
026500      03  FEL2                   PIC X(40)                                
026600          VALUE '  ARTIKELN SAKNAS                    '.                  
026700     SKIP2                                                                
026800      03  FEL3                   PIC X(40)                                
026900          VALUE '  ARTIKELNR EJ NUMERISKT             '.                  
027000                                                                          
027100     SKIP2                                                                
027200      03  FEL4                   PIC X(40)                                
027300          VALUE '  INVALID SUPPLIER NUMBER            '.                  
027400                                                                          
027500     SKIP2                                                                
027600      03  FEL5                   PIC X(40)                                
027700          VALUE '  INVALID SHIPPING NUMBER            '.                  
027800                                                                          
027900     SKIP2                                                                
028000      03  FEL7                   PIC X(40)                                
028100          VALUE ' UPDATE BOTH MFG & SHIP FIELDS '.                        
028200                                                                          
028300     SKIP2                                                                
028400      03  FEL8                   PIC X(40)                                
028500          VALUE '  INVALID SUPPLIER/CANNOT BE UPDATED '.                  
028600                                                                          
028700     SKIP2                                                                
028800      03  FEL9                   PIC X(40)                                
028900          VALUE '  PART EXPIRED                       '.                  
029000     SKIP2                                                                
029100      03  FEL10                  PIC X(40)                                
029200          VALUE 'SUPPLIER 1002 NOT VALID FOR THIS PART'.                  
029300     SKIP2                                                                
029400      03  FEL11                  PIC X(40)                                
029500          VALUE ' MFG & SHIP SUPPLIER HAS TO BE SAME  '.                  
029600     SKIP2                                                                
029700      03  FEL12                  PIC X(40)                                
029800          VALUE ' REFILL FLOW NOT AVAILABLE           '.                  
029810      03  FEL13                  PIC X(40)                                
029820          VALUE ' REFILL FLOW NOT VALID               '.                  
029830      03  FEL14                  PIC X(40)                                
029840          VALUE ' PART MISSING IN DC                  '.                  
029900     SKIP2                                                                
030000      03 W-FEL-6.                                                         
030100         05 FILLER     PIC X(19)   VALUE 'OBEHÖRIG ANVÄNDARE '.           
030200         05 FILLER     PIC X(19)   VALUE 'USER NOT AUTHORIZED'.           
030300      03 FILLER REDEFINES W-FEL-6.                                        
030400         05 FEL-6      PIC X(19) OCCURS 2.                                
030500                                                                          
030600     EJECT                                                                
030700* - - - - - - - - - - - - - - - - - - - - MID-AREA                        
030800 01  FILLER                      PIC X(16)   VALUE 'MID-AREA'.            
030900*01  MID -COPY W2I13101 -PRE MID-.                                        
031000     EJECT                                                                
031100* - - - - - - - - - - - - - - - - - - - - MSG-AREA                        
031200 01  FILLER                      PIC X(16)   VALUE 'MSG-AREA'.            
031300*01  -COPY WMSGAREA                                                       
031400     EJECT                                                                
031500*    03  MOD  -COPY W2O13101 -PRE MOD- -RED MSG-AREA.                     
031600     EJECT                                                                
031700* - - - - - - - - - - - - - - - - - - -  MFS-AREA                         
031800 01  FILLER                      PIC X(16)   VALUE 'MFS-AREA'.            
031900*01  -COPY WMFSAREA                                                       
032000     EJECT                                                                
032100* - - - - - - - - - - - - - - - - - - -  IMS-WS                           
032200 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
032300 01  IMS-WS.                                                              
032400*                        **** STATUS-KOD FRÅN IMS                         
032500   03  STATUS-WS                 PIC XX.                                  
032600     88  SEGMENT-FINNS                       VALUE '  '.                  
032700     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
032800     88  SEGMENT-GODK                        VALUE '  ',                  
032900                                                   'GA',                  
033000                                                   'GK'.                  
033100     88  BASEN-SLUT                          VALUE 'GB'.                  
033200     SKIP2                                                                
033300   03  GODK-STATUSKODER.                                                  
033400     05  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
033500     SKIP2                                                                
033600 01  SSA1                        PIC X(64).                               
033700 01  SSA2                        PIC X(64).                               
033800 01  SSA3                        PIC X(64).                               
033900     EJECT                                                                
034000                                                                          
034100*                            IMS FUNKTIONSKODER                           
034200*01    -COPY W0003                                                        
034300     EJECT                                                                
034400* - - - - - - - - - - - - - - - - - - -  DLI-IO-AREA                      
034500 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK601'.                      
034600 01  DLI-IO-WDK601.                                                       
034700*    03  -COPY WDK601                                                     
034800     EJECT                                                                
034900 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK611'.                      
035000 01  DLI-IO-WDK611.                                                       
035100*    03  -COPY WDK611                                                     
035200     EJECT                                                                
035300 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK623'.                      
035400 01  DLI-IO-WDK623.                                                       
035500*    03  -COPY WDK623                                                     
035600     EJECT                                                                
035700 01  FILLER         PIC X(16) VALUE 'DLI-IO-WLZZAC01'.                    
035800 01  DLI-IO-WLZZAC01.                                                     
035900*    03  -COPY WDGZ01 -PRE ZZAC-                                          
036000     EJECT                                                                
036100 01  DLI-IO-AREA.                                                         
036200     03  IO-AREA                 PIC X(200)  VALUE SPACE.                 
036300                                                                          
036400*    03  XXBJ01   -COPY WDGX01  -PRE XXBJ01- -RED IO-AREA.                
036500     SKIP3                                                                
036600*    03  XXBJ11   -COPY WDGX2204  -PRE XXBJ11-  -RED IO-AREA.             
036700     EJECT                                                                
036800*    03  XXBI01   -COPY WDGX01  -PRE XXBI01- -RED IO-AREA.                
036900     SKIP3                                                                
037000*    03  XXBI11   -COPY WDGX2214  -PRE XXBI11-  -RED IO-AREA.             
037100     SKIP3                                                                
037200*    03  WLBENA   -COPY WDD301  -PRE BENA-    -RED IO-AREA.               
037300     SKIP3                                                                
037400*    03  WLBENA   -COPY WDD311  -PRE BENA-    -RED IO-AREA.               
037500     EJECT                                                                
037600* - - - - - - - - - - - - - - - - - - -  DLI-IO-AREA2                     
037700 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA2'.        
037800 01  DLI-IO-AREA2.                                                        
037900     03  IO-AREA2                PIC X(100)  VALUE SPACE.                 
038000     SKIP3                                                                
038100*    03  WLLEVA01  -COPY WDF101 -PRE LEVA-    -RED IO-AREA2.              
038200     EJECT                                                                
038300* - - - - - - - - - - - - - - - - - - -  DLI-IO-AREA3                     
038400 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA3'.        
038500 01  DLI-IO-AREA3.                                                        
038600     03  IO-AREA3                PIC X(50)   VALUE SPACE.                 
038700     SKIP3                                                                
038800*    03  WL450501  -COPY WDGX4505 -PRE 4505-    -RED IO-AREA3.            
038900     EJECT                                                                
039000*    03  WL450511  -COPY WDGX4506                 -RED IO-AREA3.          
039100     EJECT                                                                
039200 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA4'.        
039300 01  DLI-IO-AREA4.                                                        
039400     03  IO-AREA4                PIC X(300)   VALUE SPACE.                
039500     SKIP3                                                                
039600*    03  WLARTS01  -COPY WDK701               -RED IO-AREA4.              
039700                                                                          
039800*    03  WLARTS11  -COPY WDK711               -RED IO-AREA4.              
039900     EJECT                                                                
039910 01  DLI-IO-K711-REF.                                                     
039920*    03  -COPY WDK711  -PRE REF-                                          
039930     EJECT                                                                
040000 01  FILLER               PIC X(16)   VALUE 'WDB601 AREA'.                
040100 01   DLI-IO-AREA-B601.                                                   
040200*     03  -COPY WDB601                                                    
040300                                                                          
040400 01  FILLER                  PIC X(16) VALUE 'DLI-IO-WDB615'.             
040500 01  DLI-IO-WDB615.                                                       
040600*    03  -COPY WDB615                                                     
040700                                                                          
040800 01  FILLER                  PIC X(16) VALUE 'DLI-IO-WDB616'.             
040900 01  DLI-IO-WDB616.                                                       
041000*    03  -COPY WDB616                                                     
041100                                                                          
041200     EJECT                                                                
041300 01  FILLER                      PIC X(16)   VALUE 'GDG9 AREA'.           
041400 01  DLI-IO-WDG901.                                                       
041500     03  IO-AREA-WDG901          PIC X(150)  VALUE SPACE.                 
041600     03  WDG901  REDEFINES IO-AREA-WDG901.                                
041700     SKIP3                                                                
041800*        05  -COPY WDG901   -PRE WDG901-                                  
041900     EJECT                                                                
042000     03  BAS-AREA REDEFINES IO-AREA-WDG901.                               
042100         05 FILLER               PIC X(29).                               
042200         05  DATA  -COPY W213R01  -PRE BAS-R01-                           
042300         05  BAS-R01-REST         PIC X(75).                              
042400     EJECT                                                                
042500 LINKAGE SECTION.                                                         
042600     SKIP2                                                                
042700*01  -COPY W0009    -PRE MSG-                                             
042800     EJECT                                                                
042900*01  -COPY W0008     -PRE USEA-                                           
043000     05  FILLER                  PIC X.                                   
043100     EJECT                                                                
043200*01  -COPY W0008     -PRE WLXXBJ-                                         
043300     05  FILLER                  PIC X.                                   
043400     EJECT                                                                
043500*01  -COPY W0008     -PRE WLXXBI-                                         
043600     05  FILLER                  PIC X.                                   
043700     EJECT                                                                
043800*01  -COPY W0008     -PRE WLLEVA-                                         
043900     05  FILLER                  PIC X.                                   
044000     EJECT                                                                
044100*01  -COPY W0008     -PRE WDK6-                                           
044200     05  FILLER                  PIC X.                                   
044300     EJECT                                                                
044400*01  -COPY W0008     -PRE ZZAC-                                           
044500         05  FILLER              PIC X.                                   
044600     EJECT                                                                
044700*01  -COPY W0008     -PRE WLBENA-                                         
044800     05  FILLER                  PIC X.                                   
044900     EJECT                                                                
045000*01  -COPY W0008     -PRE 4505-                                           
045100     05  FILLER                  PIC X.                                   
045200     EJECT                                                                
045300*01  -COPY W0008     -PRE ARTS-                                           
045400     05  FILLER                  PIC X.                                   
045500     EJECT                                                                
045600*01  -COPY W0008      -PRE WDB6-                                          
045700     05  FILLER                  PIC X.                                   
045800     EJECT                                                                
045900*01  -COPY W0008      -PRE WDG9-                                          
046000     05  FILLER                  PIC X.                                   
046100     EJECT                                                                
046200 PROCEDURE DIVISION USING MSG-PCB USEA-PCB                                
046300                       WLLEVA-PCB                                         
046400                       WLXXBJ-PCB WLXXBI-PCB                              
046500                       WDK6-PCB ZZAC-PCB                                  
046600                       WLBENA-PCB   4505-PCB                              
046700                       ARTS-PCB WDB6-PCB WDG9-PCB.                        
046800                                                                          
046900     ENTRY 'DLITCBL' USING MSG-PCB USEA-PCB                               
047000                       WLLEVA-PCB                                         
047100                       WLXXBJ-PCB WLXXBI-PCB                              
047200                       WDK6-PCB ZZAC-PCB                                  
047300                       WLBENA-PCB   4505-PCB                              
047400                       ARTS-PCB WDB6-PCB WDG9-PCB.                        
047500                                                                          
047600     PERFORM IMS-GET-MSG                                                  
047700     IF SEGMENT-FINNS                                                     
047800       PERFORM A-INIT                                                     
047900                                                                          
048000       IF IDARTNR-WS NOT NUMERIC                                          
048100          MOVE FEL3 TO MOD-MESSAGE-RAD1                                   
048200          PERFORM S01-CLOSE-SUPPLIER-UPD                                  
048300       ELSE                                                               
048400          MOVE IDARTNR-WS   TO W-IDARTNR-KEY                              
048500                                                                          
048600          PERFORM SEC-URITY                                               
048700          IF PASSED-SECURITY-CHECK                                        
048800*            --- OK, USER HAS NO RESTRICTIONS                             
048900             IF MFS-UPDATE OR MFS-UPD-V                                   
049000                PERFORM B-UPPDATERA                                       
049100             ELSE                                                         
049200                IF MID-IDARTNR-IN NOT = ALL '+'                           
049300                                                                          
049400                   PERFORM D-RENSA-FAELT-MOD                              
049500                                                                          
049600                END-IF                                                    
049700                                                                          
049800                PERFORM C-FRAGA                                           
049900             END-IF                                                       
050000                                                                          
050100          END-IF                                                          
050200       END-IF                                                             
050300                                                                          
050400       COMPUTE MSG-KVLL = LENGTH OF MOD-W2O13101 + 4                      
050500       PERFORM IMS-INSERT-MSG                                             
050600     END-IF                                                               
050700     MOVE ZERO TO RETURN-CODE                                             
050800     GOBACK                                                               
050900     .                                                                    
051000     EJECT                                                                
051100 A-INIT SECTION.                                                          
051200     SKIP2                                                                
051300                                                                          
051400     IF MSG-DUBBLA-TRANSKODER                                             
051500       MOVE MSG-INDATA-MINUS-2-TRANSKODER TO MID-W2I13101                 
051600       MOVE MSG-IDTRANS-2 TO MFS-IDTRANS                                  
051700       MOVE MSG-KDMFSFOR-2 TO MFS-KDMFSFOR                                
051800       MOVE MSG-KDTRTYP          TO MFS-KDTRTYP                           
051900     ELSE                                                                 
052000       MOVE MSG-INDATA-MINUS-1-TRANSKOD  TO MID-W2I13101                  
052100       MOVE MSG-IDTRANS-1 TO MFS-IDTRANS                                  
052200       MOVE MSG-KDMFSFOR-1 TO MFS-KDMFSFOR                                
052300       MOVE ' ' TO MFS-KDTRTYP                                            
052400     END-IF                                                               
052500                                                                          
052600     MOVE ALL '+' TO MSGI-WMSGINIT                                        
052700     MOVE '001'             TO MSGI-KDCALL                                
052800     MOVE MSG-SIGNON-USERID TO MSGI-IDUSER                                
052900     MOVE MSG-LTERM-NAME    TO MSGI-IDLTERM-USER                          
053000     MOVE '2131'            TO MSGI-IDTRANS                               
053100     IF MFS-IDTRANS = '2131'                                              
053200     OR (MID-IDARTNR-IN NUMERIC                                           
053300     AND MID-IDARTNR-IN > ZERO)                                           
053400         MOVE MID-IDARTNR-IN TO MSGI-IDARTNR                              
053500     END-IF                                                               
053600     CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                           
053700     MOVE MSGI-IDARTNR TO IDARTNR-WS                                      
053800     INSPECT IDARTNR-WS REPLACING ALL SPACE BY ZERO                       
053900                                                                          
054000     IF MID-IDARTNR-IN = SPACE OR ALL '+'                                 
054100        CONTINUE                                                          
054200     ELSE                                                                 
054300        MOVE ' ' TO MFS-KDTRTYP                                           
054400     END-IF                                                               
054500     IF MFS-IDTRANS NOT = '2131'                                          
054600        MOVE ' ' TO MFS-KDTRTYP                                           
054700     END-IF                                                               
054800                                                                          
054900                                                                          
055000     MOVE LOW-VALUE TO MSG-AREA                                           
055100     MOVE 'W2O131N1' TO MFS-IDMOD                                         
055200     MOVE '2131' TO MOD-IDTRANS                                           
055300                                                                          
055400     MOVE IDARTNR-WS TO MOD-IDARTNR-UT                                    
055500                                                                          
055600     INSPECT MOD-IDARTNR-UT REPLACING LEADING ZERO BY SPACE               
055700                                                                          
055800     PERFORM AA-ROER-EJ-FAELT-MOD-UT                                      
055900     PERFORM AB-RENSA-FAELT-MOD                                           
056000                                                                          
056100     MOVE MFS-RENSA-FAELT TO MOD-IDARTNR-IN                               
056200                             MOD-MESSAGE-RAD1                             
056300                             MOD-MESSAGE-RAD23                            
056400                                                                          
056500     ACCEPT DAGENS-DATUM  FROM DATE                                       
056600     .                                                                    
056700     EJECT                                                                
056800 AA-ROER-EJ-FAELT-MOD-UT SECTION.                                         
056900     SKIP2                                                                
057000     MOVE MFS-ROER-EJ-FAELT           TO MOD-IDANSK-UT                    
057100                                         MOD-KVSPANT-C1-UT                
057200                                         MOD-KVSPANT-C2-UT                
057300                                         MOD-IDPLANGR-AG-UT               
057400                                         MOD-IDPLANGR-LEV-UT              
057500                                         MOD-TIREFSTO-UT                  
057600                                         MOD-KVQ-JUST-UT                  
057700                                         MOD-TIQJUST-UT                   
057800                                         MOD-RESLJUST-C1-UT               
057900                                         MOD-RESLJUST-C2-UT               
058000                                         MOD-KVVECKOR-LT-UT               
058100                                         MOD-FLMANLT-UT                   
058200                                         MOD-TISLJUST-C1-UT               
058300                                         MOD-TISLJUST-C2-UT               
058400                                         MOD-KVVECKOR-AT-UT               
058500                                         MOD-FLMANAT-UT                   
058600                                         MOD-KVQ-UT                       
058700                                         MOD-FLMANQ-UT                    
058800                                         MOD-KVULOAD-UT                   
058900                                         MOD-FLREFILL-UT                  
059000                                         MOD-KVBK-UT                      
059100                                         MOD-FLMANBK-UT                   
059200                                         MOD-KVPALL-UT                    
059300                                         MOD-KVSLAGER-C1-UT               
059400                                         MOD-KVSLAGER-C2-UT               
059500                                         MOD-IDLEVNR-FRAM-UT              
059600                                         MOD-IDLEVNR-SHIP-FRAM-UT         
059700     .                                                                    
059800     EJECT                                                                
059900 AB-RENSA-FAELT-MOD SECTION.                                              
060000     SKIP2                                                                
060100     MOVE MFS-RENSA-FAELT             TO MOD-IDANSK-IN                    
060200                                         MOD-KVSPANT-C1-IN                
060300                                         MOD-KVSPANT-C2-IN                
060400                                         MOD-IDPLANGR-AG-IN               
060500                                         MOD-IDPLANGR-LEV-IN              
060600                                         MOD-TIREFSTO-IN                  
060700                                         MOD-KVQ-JUST-IN                  
060800                                         MOD-TIQJUST-IN                   
060900                                         MOD-RESLJUST-C1-IN               
061000                                         MOD-RESLJUST-C2-IN               
061100                                         MOD-KVVECKOR-LT-IN               
061200                                         MOD-FLMANLT-IN                   
061300                                         MOD-TISLJUST-C1-IN               
061400                                         MOD-TISLJUST-C2-IN               
061500                                         MOD-KVVECKOR-AT-IN               
061600                                         MOD-FLMANAT-IN                   
061700                                         MOD-KVQ-IN                       
061800                                         MOD-FLMANQ-IN                    
061900                                         MOD-KVULOAD-IN                   
062000                                         MOD-FLREFILL-IN                  
062100                                         MOD-KVBK-IN                      
062200                                         MOD-FLMANBK-IN                   
062300                                         MOD-KVPALL-IN                    
062400                                         MOD-KVSLAGER-C1-IN               
062500                                         MOD-KVSLAGER-C2-IN               
062600                                         MOD-IDLEVNR-FRAM-IN              
062700                                         MOD-IDLEVNR-SHIP-FRAM-IN         
062800     .                                                                    
062900     EJECT                                                                
063000 B-UPPDATERA SECTION.                                                     
063100     SKIP2                                                                
063200     PERFORM BA-KONTROLLERA-INDATA                                        
063300     MOVE MFS-ROER-EJ-FAELT  TO MOD-BEART-SVE                             
063400     IF INDATA-OK = JA                                                    
063500        PERFORM IMS-GET-WDK601                                            
063600        IF SEGMENT-FINNS                                                  
063700           IF ART-FLIART = 'J'                                            
063800              MOVE JA                  TO SATS-SW                         
063900           ELSE                                                           
064000              MOVE NEJ                 TO SATS-SW                         
064100           END-IF                                                         
064200           MOVE ART-IDLEVNR            TO W-IDLEVNR-KEY                   
064300           IF SW-UPPDAT-ARTC12 = JA                                       
064400              PERFORM IMS-GHNP-WDK611                                     
064500              IF SEGMENT-FINNS                                            
064600                 MOVE JA TO SW-UPPDAT-ARTC12-OK                           
064700                 PERFORM BB-UPPDATERA-ARTC12                              
064800                 IF SW-UPPDAT-ARTC12-OK = JA                              
064900                    PERFORM IMS-REPL-WDK611                               
065000                    MOVE UPPDAT TO MOD-MESSAGE-RAD23                      
065100                    PERFORM BBE-RENSA-ARTC12-FAELT                        
065200                 ELSE                                                     
065300                    MOVE FEL1 TO MOD-MESSAGE-RAD1                         
065400                    PERFORM BE-ROER-EJ-FAELT                              
065500                    MOVE NEJ TO SW-UPPDAT-ARTC23                          
065600                                SW-UPPDAT-ARTC91                          
065700                                SW-UPPDAT-ARTS11                          
065800                                SW-2204                                   
065900                                SW-2213                                   
066000                                SW-IDANSK-UPPD                            
066100                 END-IF                                                   
066200                 IF SW-2204 = JA                                          
066300                    PERFORM BGA-UPPDATERA-2204-WDG3                       
066400                 END-IF                                                   
066500                 IF SW-2213 = JA                                          
066600                    PERFORM BF-UPPDATERA-2213-WDG3                        
066700                 END-IF                                                   
066800                 IF SW-IDANSK-UPPD = JA                                   
066900                    PERFORM BGA-UPPDATERA-2204-WDG3                       
067000                 END-IF                                                   
067100              END-IF                                                      
067200           END-IF                                                         
067300                                                                          
067400           IF SW-UPPDAT-ARTC23 = JA                                       
067500              PERFORM IMS-GHNP-WDK611                                     
067600              IF SEGMENT-FINNS                                            
067700                 PERFORM BC-UPPDATERA-ARTC23                              
067800                 PERFORM IMS-REPL-WDK611                                  
067900                 MOVE UPPDAT TO MOD-MESSAGE-RAD23                         
068000                 PERFORM BCA-RENSA-ARTC23-FAELT                           
068100              END-IF                                                      
068200           END-IF                                                         
068300                                                                          
068400           IF SW-UPPDAT-ARTC91 = JA                                       
068500              PERFORM IMS-GHNP-WDK611                                     
068600              IF SEGMENT-FINNS                                            
068700                 PERFORM BD-UPPDATERA-ARTC91                              
068800                 PERFORM IMS-REPL-WDK611                                  
068900                 IF TAECKNING = JA                                        
069000                    PERFORM BH-TAECKNING                                  
069100                 END-IF                                                   
069200                 MOVE UPPDAT TO MOD-MESSAGE-RAD23                         
069300                 PERFORM BDA-RENSA-ARTC91-FAELT                           
069400              END-IF                                                      
069500           END-IF                                                         
069600                                                                          
069700           IF SW-UPPDAT-ARTS11 = JA                                       
069800              PERFORM BI-UPPDAT-ARTS11                                    
069900           END-IF                                                         
070000           IF SW-MFG-SHIP-PRESENT = JA AND                                
070100              SW-SUPPLIER-VAL-OK = JA                                     
070200              PERFORM BJ-UPDATE-MFG-SHP                                   
070300              MOVE UPPDAT             TO MOD-MESSAGE-RAD23                
070400           END-IF                                                         
070500                                                                          
070600        ELSE                                                              
070700           MOVE FEL2 TO MOD-MESSAGE-RAD1                                  
070800           PERFORM BE-ROER-EJ-FAELT                                       
070900        END-IF                                                            
071000     ELSE                                                                 
071100        IF SW-ERR-MSG-SUPPLIER = JA                                       
071200           CONTINUE                                                       
071300        ELSE                                                              
071400           MOVE FEL1 TO MOD-MESSAGE-RAD1                                  
071500        END-IF                                                            
071600        PERFORM BE-ROER-EJ-FAELT                                          
071700     END-IF                                                               
071800     .                                                                    
071900     EJECT                                                                
072000                                                                          
072100 BA-KONTROLLERA-INDATA SECTION.                                           
072200     SKIP2                                                                
072300     MOVE JA  TO INDATA-OK                                                
072400                                                                          
072500     PERFORM BAA-KOLLA-ARTC12                                             
072600                                                                          
072700     MOVE JA TO KOLLA-FAKTOR-DATUM-C1                                     
072800                KOLLA-FAKTOR-DATUM-C2                                     
072900     IF INDATA-OK = JA                                                    
073000       PERFORM BAB-KOLLA-ARTC23                                           
073100                                                                          
073200       PERFORM BAC-KOLLA-ARTC91                                           
073300     END-IF                                                               
073400     IF INDATA-OK = JA                                                    
073500       PERFORM BAD-KOLLA-MFG-SHP                                          
073600     END-IF                                                               
073700     .                                                                    
073800     EJECT                                                                
073900 BAA-KOLLA-ARTC12 SECTION.                                                
074000     SKIP2                                                                
074100     IF MID-IDANSK-IN NOT = ALL '+'                                       
074200       IF MID-IDANSK-IN NUMERIC                                           
074300           MOVE MFS-NUM-FAELT-RAETT   TO MOD-IDANSK-ATTR-IN               
074400           MOVE JA                    TO SW-UPPDAT-ARTC12                 
074500           MOVE MID-IDANSK-IN         TO WS-IDANSK                        
074600       ELSE                                                               
074700          MOVE NEJ                    TO INDATA-OK                        
074800          MOVE MFS-NUM-FAELT-FEL      TO MOD-IDANSK-ATTR-IN               
074900       END-IF                                                             
075000     ELSE                                                                 
075100        MOVE MID-IDANSK-UT            TO WS-IDANSK                        
075200     END-IF                                                               
075300                                                                          
075400     IF MID-IDPLANGR-AG-IN NOT = ALL '+'                                  
075500        IF MID-IDPLANGR-AG-IN NUMERIC                                     
075600           MOVE MFS-NUM-FAELT-RAETT   TO                                  
075700                                     MOD-IDPLANGR-AG-ATTR-IN              
075800           MOVE JA                    TO SW-UPPDAT-ARTC12                 
075900           MOVE MID-IDPLANGR-AG-IN    TO WS-IDPLANGR-AG                   
076000        ELSE                                                              
076100           MOVE NEJ                   TO INDATA-OK                        
076200           MOVE MFS-NUM-FAELT-FEL     TO MOD-IDPLANGR-AG-ATTR-IN          
076300        END-IF                                                            
076400     ELSE                                                                 
076500        MOVE MID-IDPLANGR-AG-UT       TO WS-IDPLANGR-AG                   
076600     END-IF                                                               
076700     IF MID-IDPLANGR-LEV-IN NOT = ALL '+'                                 
076800        IF MID-IDPLANGR-LEV-IN NUMERIC                                    
076900             MOVE MFS-NUM-FAELT-RAETT                                     
077000                                      TO MOD-IDPLANGR-LEV-ATTR-IN         
077100             MOVE JA                  TO SW-UPPDAT-ARTC12                 
077200        ELSE                                                              
077300           MOVE NEJ                   TO INDATA-OK                        
077400           MOVE MFS-NUM-FAELT-FEL     TO MOD-IDPLANGR-LEV-ATTR-IN         
077500        END-IF                                                            
077600     END-IF                                                               
077700                                                                          
077800                                                                          
077900     IF MID-KVVECKOR-LT-IN NOT = ALL '+'                                  
078000        IF MID-KVVECKOR-LT-IN NUMERIC                                     
078100          MOVE MID-KVVECKOR-LT-IN     TO WS-KVVECKOR-LT                   
078200          IF WS-KVVECKOR-LT > 052                                         
078300            MOVE NEJ                  TO INDATA-OK                        
078400            MOVE MFS-NUM-FAELT-FEL    TO MOD-KVVECKOR-LT-ATTR-IN          
078500          ELSE                                                            
078600             MOVE MFS-NUM-FAELT-RAETT   TO MOD-KVVECKOR-LT-ATTR-IN        
078700             MOVE JA                    TO SW-UPPDAT-ARTC12               
078800           END-IF                                                         
078900        ELSE                                                              
079000           MOVE NEJ                   TO INDATA-OK                        
079100           MOVE MFS-NUM-FAELT-FEL     TO MOD-KVVECKOR-LT-ATTR-IN          
079200        END-IF                                                            
079300     END-IF                                                               
079400                                                                          
079500     IF MID-FLMANLT-IN NOT =  ALL '+'                                     
079600        IF MID-FLMANLT-IN = 'J' OR 'N'                                    
079700           MOVE MFS-ALFA-FAELT-RAETT  TO MOD-FLMANLT-ATTR-IN              
079800           MOVE JA                    TO SW-UPPDAT-ARTC12                 
079900        ELSE                                                              
080000           MOVE NEJ                   TO INDATA-OK                        
080100           MOVE MFS-ALFA-FAELT-FEL    TO MOD-FLMANLT-ATTR-IN              
080200        END-IF                                                            
080300     END-IF                                                               
080400                                                                          
080500     IF MID-KVVECKOR-LT-IN NOT =  ALL '+'                                 
080600                        AND MID-FLMANLT-IN = 'N'                          
080700        MOVE NEJ                      TO INDATA-OK                        
080800        MOVE MFS-NUM-FAELT-FEL        TO MOD-KVVECKOR-LT-ATTR-IN          
080900        MOVE MFS-ALFA-FAELT-FEL       TO MOD-FLMANLT-ATTR-IN              
081000     END-IF                                                               
081100                                                                          
081200     IF MID-KVVECKOR-AT-IN NOT = ALL '+'                                  
081300        IF MID-KVVECKOR-AT-IN NUMERIC                                     
081400           MOVE MFS-NUM-FAELT-RAETT   TO MOD-KVVECKOR-AT-ATTR-IN          
081500           MOVE JA                    TO SW-UPPDAT-ARTC12                 
081600        ELSE                                                              
081700           MOVE NEJ                   TO INDATA-OK                        
081800           MOVE MFS-NUM-FAELT-FEL     TO MOD-KVVECKOR-AT-ATTR-IN          
081900        END-IF                                                            
082000     END-IF                                                               
082100                                                                          
082200     IF MID-FLMANAT-IN NOT = ALL '+'                                      
082300        IF MID-FLMANAT-IN = 'J' OR 'N'                                    
082400           MOVE MFS-ALFA-FAELT-RAETT  TO MOD-FLMANAT-ATTR-IN              
082500           MOVE JA                    TO SW-UPPDAT-ARTC12                 
082600        ELSE                                                              
082700           MOVE NEJ                   TO INDATA-OK                        
082800           MOVE MFS-ALFA-FAELT-FEL    TO MOD-FLMANAT-ATTR-IN              
082900        END-IF                                                            
083000     END-IF                                                               
083100                                                                          
083200     IF MID-KVVECKOR-AT-IN NOT = ALL '+'                                  
083300            AND MID-FLMANAT-IN = 'N'                                      
083400        MOVE NEJ                      TO INDATA-OK                        
083500        MOVE MFS-NUM-FAELT-FEL        TO MOD-KVVECKOR-AT-ATTR-IN          
083600        MOVE MFS-ALFA-FAELT-FEL       TO MOD-FLMANAT-ATTR-IN              
083700     END-IF                                                               
083800                                                                          
083900     IF MID-KVQ-IN NOT = ALL '+'                                          
084000        IF MID-KVQ-IN NUMERIC AND MFS-UPD-V                               
084100           MOVE MFS-NUM-FAELT-RAETT   TO MOD-KVQ-ATTR-IN                  
084200           MOVE JA                    TO SW-UPPDAT-ARTC12                 
084300        ELSE                                                              
084400           MOVE NEJ                   TO SW-UPPDAT-ARTC12                 
084500           MOVE NEJ                   TO INDATA-OK                        
084600           MOVE MFS-NUM-FAELT-FEL     TO MOD-KVQ-ATTR-IN                  
084700        END-IF                                                            
084800     END-IF                                                               
084900                                                                          
085000     IF MID-FLMANQ-IN NOT = ALL '+'                                       
085100        IF (MID-FLMANQ-IN = 'J' OR 'N') AND MFS-UPD-V                     
085200           MOVE MFS-ALFA-FAELT-RAETT  TO MOD-FLMANQ-ATTR-IN               
085300           MOVE JA                    TO SW-UPPDAT-ARTC12                 
085400        ELSE                                                              
085500           MOVE NEJ                   TO SW-UPPDAT-ARTC12                 
085600           MOVE NEJ                   TO INDATA-OK                        
085700           MOVE MFS-ALFA-FAELT-FEL    TO MOD-FLMANQ-ATTR-IN               
085800        END-IF                                                            
085900     END-IF                                                               
086000                                                                          
086100                                                                          
086200     IF MID-KVULOAD-IN NOT = ALL '+'                                      
086300        IF MID-KVULOAD-IN NUMERIC                                         
086400           MOVE MFS-NUM-FAELT-RAETT   TO MOD-KVULOAD-ATTR-IN              
086500           MOVE JA                    TO SW-UPPDAT-ARTC12                 
086600        ELSE                                                              
086700           MOVE MFS-NUM-FAELT-FEL     TO MOD-KVULOAD-ATTR-IN              
086800           MOVE NEJ                   TO INDATA-OK                        
086900        END-IF                                                            
087000        MOVE MFS-ROER-EJ-FAELT        TO MOD-KVULOAD-IN                   
087100     ELSE                                                                 
087200        MOVE MFS-RENSA-FAELT          TO MOD-KVULOAD-IN                   
087300     END-IF                                                               
087400                                                                          
087500     IF MID-FLREFILL-IN NOT = ALL '+'                                     
087600        IF MID-FLREFILL-IN = 'J' OR 'N'                                   
087700           IF MID-FLREFILL-IN = 'J'                                       
087800              PERFORM IMS-GET-WDK601                                      
087900              PERFORM IMS-GET-WDK611                                      
088000              MOVE IDARTNR-WS TO BYT03-IDARTNR                            
088100              MOVE ART-KDPRODSL  TO TEST-KDPRODSL                         
088200              IF (BYT03-OBJEKT AND NOT KDPRODSL-LOCAL-BYTES)              
088300              OR (KDPRODSL-BIMA)                                          
088400              OR (CLAG-FLLSRDEL = 'N')                                    
088500                 MOVE NEJ    TO INDATA-OK                                 
088600                 MOVE MFS-ALFA-FAELT-FEL TO                               
088700                                MOD-FLREFILL-ATTR-IN                      
088800              ELSE                                                        
088900                 MOVE MFS-ALFA-FAELT-RAETT  TO                            
089000                                 MOD-FLREFILL-ATTR-IN                     
089100                 MOVE JA      TO SW-UPPDAT-ARTC12                         
089200              END-IF                                                      
089300           END-IF                                                         
089400                                                                          
089500           IF MID-FLREFILL-IN = 'N'                                       
089600              MOVE JA                 TO SW-UPPDAT-ARTC12                 
089700              MOVE JA                 TO SW-UPPDAT-ARTS11                 
089800           END-IF                                                         
089900        ELSE                                                              
090000           MOVE NEJ                   TO INDATA-OK                        
090100           MOVE MFS-ALFA-FAELT-FEL    TO MOD-FLREFILL-ATTR-IN             
090200        END-IF                                                            
090300     END-IF                                                               
090400                                                                          
090500     IF MID-TIREFSTO-IN NOT = ALL '+'                                     
090600        MOVE MID-TIREFSTO-IN  TO WS-TIREFSTO                              
090700                                                                          
090800        MOVE 'AAMMDD'         TO DAT-KDDATFORM                            
090900        MOVE WS-TIREFSTO      TO DAT-I-TIDATUM                            
091000        CALL WDATKONV USING   DAT-KDDATFORM                               
091100                              DAT-I-TIDATUM                               
091200                              DAT-O-TIDATUM                               
091300                              DAT-KDSVAR                                  
091400                                                                          
091500        IF DAT-KDSVAR-OK                                                  
091600           MOVE MFS-ALFA-FAELT-RAETT  TO MOD-TIREFSTO-ATTR-IN             
091700           MOVE JA                    TO SW-UPPDAT-ARTC12                 
091800        ELSE                                                              
091900           MOVE NEJ                   TO INDATA-OK                        
092000           MOVE MFS-NUM-FAELT-FEL     TO MOD-TIREFSTO-ATTR-IN             
092100        END-IF                                                            
092200     END-IF                                                               
092300                                                                          
092400     IF MID-KVQ-IN NOT = ALL '+'                                          
092500            AND MID-FLMANQ-IN = 'N'                                       
092600        MOVE NEJ                      TO INDATA-OK                        
092700        MOVE MFS-NUM-FAELT-FEL        TO MOD-KVQ-ATTR-IN                  
092800        MOVE MFS-ALFA-FAELT-FEL       TO MOD-FLMANQ-ATTR-IN               
092900     END-IF                                                               
093000                                                                          
093100     IF MID-KVBK-IN NOT = ALL '+'                                         
093200        IF MID-KVBK-IN NUMERIC                                            
093300           MOVE MFS-NUM-FAELT-RAETT   TO MOD-KVBK-ATTR-IN                 
093400           MOVE JA                    TO SW-UPPDAT-ARTC12                 
093500        ELSE                                                              
093600           MOVE NEJ                   TO INDATA-OK                        
093700           MOVE MFS-NUM-FAELT-FEL     TO MOD-KVBK-ATTR-IN                 
093800        END-IF                                                            
093900     END-IF                                                               
094000                                                                          
094100     IF MID-FLMANBK-IN NOT = ALL '+'                                      
094200        IF MID-FLMANBK-IN = 'J' OR 'N'                                    
094300           MOVE MFS-ALFA-FAELT-RAETT  TO MOD-FLMANBK-ATTR-IN              
094400           MOVE JA                    TO SW-UPPDAT-ARTC12                 
094500        ELSE                                                              
094600           MOVE NEJ                   TO INDATA-OK                        
094700           MOVE MFS-ALFA-FAELT-FEL    TO MOD-FLMANBK-ATTR-IN              
094800        END-IF                                                            
094900     END-IF                                                               
095000                                                                          
095100     IF MID-KVBK-IN NOT = ALL '+'                                         
095200            AND MID-FLMANBK-IN = 'N'                                      
095300        MOVE NEJ                      TO INDATA-OK                        
095400        MOVE MFS-NUM-FAELT-FEL        TO MOD-KVBK-ATTR-IN                 
095500        MOVE MFS-ALFA-FAELT-FEL       TO MOD-FLMANBK-ATTR-IN              
095600     END-IF                                                               
095700                                                                          
095800     IF MID-KVPALL-IN NOT = ALL '+'                                       
095900        IF MID-KVPALL-IN NUMERIC                                          
096000           MOVE MFS-NUM-FAELT-RAETT   TO MOD-KVPALL-ATTR-IN               
096100           MOVE JA                    TO SW-UPPDAT-ARTC12                 
096200        ELSE                                                              
096300           MOVE NEJ                   TO INDATA-OK                        
096400           MOVE MFS-NUM-FAELT-FEL     TO MOD-KVPALL-ATTR-IN               
096500        END-IF                                                            
096600     END-IF                                                               
096700                                                                          
096800     IF MID-KVQ-JUST-IN NOT = ALL '+' OR                                  
096900        MID-TIQJUST-IN NOT = ALL '+'                                      
097000                                                                          
097100        IF MID-KVQ-JUST-IN NOT = ALL '+' AND                              
097200           MID-TIQJUST-IN NOT = ALL '+'                                   
097300           MOVE MID-KVQ-JUST-IN TO WS-KVQ-JUST                            
097400           MOVE MID-TIQJUST-IN  TO WS-TIQJUST                             
097500           IF WS-KVQ-JUST NUMERIC                                         
097600              MOVE MFS-NUM-FAELT-RAETT   TO MOD-KVQ-JUST-ATTR-IN          
097700              MOVE JA                    TO SW-UPPDAT-ARTC12              
097800           ELSE                                                           
097900              MOVE NEJ                   TO INDATA-OK                     
098000              MOVE MFS-NUM-FAELT-FEL     TO MOD-KVQ-JUST-ATTR-IN          
098100           END-IF                                                         
098200           IF WS-TIQJUST NUMERIC                                          
098300              MOVE MFS-NUM-FAELT-RAETT   TO MOD-TIQJUST-ATTR-IN           
098400              MOVE JA                    TO SW-UPPDAT-ARTC12              
098500           ELSE                                                           
098600              MOVE NEJ                   TO INDATA-OK                     
098700              MOVE MFS-NUM-FAELT-FEL     TO MOD-TIQJUST-ATTR-IN           
098800           END-IF                                                         
098900        ELSE                                                              
099000           IF MID-KVQ-JUST-IN NOT = ALL '+'                               
099100              MOVE MID-KVQ-JUST-IN TO WS-KVQ-JUST                         
099200              IF WS-KVQ-JUST NUMERIC                                      
099300                 MOVE MFS-NUM-FAELT-RAETT TO                              
099400                                     MOD-KVQ-JUST-ATTR-IN                 
099500                 MOVE JA              TO SW-UPPDAT-ARTC12                 
099600              ELSE                                                        
099700                 MOVE NEJ             TO INDATA-OK                        
099800                 MOVE MFS-NUM-FAELT-FEL TO MOD-KVQ-JUST-ATTR-IN           
099900              END-IF                                                      
100000              MOVE MFS-NUM-FAELT-FEL  TO MOD-TIQJUST-ATTR-IN              
100100              MOVE NEJ                TO INDATA-OK                        
100200              MOVE TIQFEL             TO MOD-MESSAGE-RAD23                
100300           ELSE                                                           
100400              MOVE MID-TIQJUST-IN TO WS-TIQJUST                           
100500              IF WS-TIQJUST NUMERIC                                       
100600                 MOVE MFS-NUM-FAELT-RAETT TO                              
100700                                    MOD-TIQJUST-ATTR-IN                   
100800                 MOVE JA              TO SW-UPPDAT-ARTC12                 
100900              ELSE                                                        
101000                 MOVE NEJ             TO INDATA-OK                        
101100                 MOVE MFS-NUM-FAELT-FEL TO                                
101200                                         MOD-TIQJUST-ATTR-IN              
101300              END-IF                                                      
101400              MOVE MFS-NUM-FAELT-FEL  TO MOD-KVQ-JUST-ATTR-IN             
101500              MOVE NEJ                TO INDATA-OK                        
101600              MOVE KVQFEL             TO MOD-MESSAGE-RAD23                
101700           END-IF                                                         
101800        END-IF                                                            
101900     END-IF                                                               
102000     .                                                                    
102100                                                                          
102200     EJECT                                                                
102300 BAB-KOLLA-ARTC23 SECTION.                                                
102400     SKIP2                                                                
102500                                                                          
102600     IF MID-RESLJUST-C1-IN NOT = ALL '+'                                  
102700        MOVE MID-RESLJUST-C1-IN TO RESLJUST-WS                            
102800        IF HELTAL NUMERIC AND PUNKT = '.' AND DECIMAL NUMERIC             
102900           MOVE MFS-NUM-FAELT-RAETT                                       
103000                                      TO MOD-RESLJUST-C1-ATTR-IN          
103100           MOVE JA                    TO SW-UPPDAT-ARTC23                 
103200           MOVE HELTAL                TO NUM-HELTAL                       
103300           MOVE DECIMAL               TO NUM-DECIMAL                      
103400           MOVE N-TAL                 TO WS-RESLJUST-C1                   
103500        ELSE                                                              
103600           MOVE NEJ                   TO INDATA-OK                        
103700           MOVE NEJ                   TO KOLLA-FAKTOR-DATUM-C1            
103800           MOVE MFS-NUM-FAELT-FEL     TO MOD-RESLJUST-C1-ATTR-IN          
103900        END-IF                                                            
104000     ELSE                                                                 
104100        MOVE MID-RESLJUST-C1-UT       TO RESLJUST-WS                      
104200        MOVE HELTAL                   TO NUM-HELTAL                       
104300        MOVE DECIMAL                  TO NUM-DECIMAL                      
104400        MOVE N-TAL                    TO WS-RESLJUST-C1                   
104500     END-IF                                                               
104600                                                                          
104700                                                                          
104800     IF MID-TISLJUST-C1-IN NOT = ALL '+'                                  
104900        IF MID-TISLJUST-C1-IN NUMERIC                                     
105000           MOVE MFS-NUM-FAELT-RAETT   TO MOD-TISLJUST-C1-ATTR-IN          
105100           MOVE JA                    TO SW-UPPDAT-ARTC23                 
105200           MOVE MID-TISLJUST-C1-IN    TO WS-TISLJUST-C1                   
105300        ELSE                                                              
105400           MOVE NEJ                   TO INDATA-OK                        
105500           MOVE MFS-NUM-FAELT-FEL     TO MOD-TISLJUST-C1-ATTR-IN          
105600        END-IF                                                            
105700     ELSE                                                                 
105800        MOVE MID-TISLJUST-C1-UT       TO WS-TISLJUST-C1                   
105900     END-IF                                                               
106000     SKIP2                                                                
106100*                      OM RESLJUST FINNS MÅSTE TISLJUST FINNAS.           
106200*                      OM RESLJUST UPPDATERAS MÅSTE TISLJUST              
106300*                         FINNAS ELLER UPPDATERAS.                        
106400*                      OM FAKTORN ÄR 0 KAN TISLJUST FINNAS ELLER          
106500*                         UPPDATERAS.                                     
106600*                                                                         
106700     IF KOLLA-FAKTOR-DATUM-C1 = JA                                        
106800       IF MID-RESLJUST-C1-IN NOT = ALL '+' OR                             
106900                  MID-TISLJUST-C1-IN NOT = ALL '+'                        
107000          IF WS-RESLJUST-C1 = ZERO                                        
107100             IF WS-TISLJUST-C1 = ZERO                                     
107200                MOVE MFS-NUM-FAELT-RAETT                                  
107300                                      TO MOD-RESLJUST-C1-ATTR-IN          
107400                MOVE MFS-NUM-FAELT-RAETT                                  
107500                                      TO MOD-TISLJUST-C1-ATTR-IN          
107600                MOVE JA               TO SW-UPPDAT-ARTC23                 
107700             ELSE                                                         
107800              IF WS-TISLJUST-C1 > ZERO                                    
107900                MOVE MFS-NUM-FAELT-RAETT                                  
108000                                      TO MOD-RESLJUST-C1-ATTR-IN          
108100                MOVE MFS-NUM-FAELT-RAETT                                  
108200                                      TO MOD-TISLJUST-C1-ATTR-IN          
108300                MOVE JA               TO SW-UPPDAT-ARTC23                 
108400               END-IF                                                     
108500             END-IF                                                       
108600          ELSE                                                            
108700            IF WS-RESLJUST-C1 > ZERO                                      
108800             IF WS-TISLJUST-C1 = ZERO                                     
108900                MOVE MFS-NUM-FAELT-FEL                                    
109000                                      TO MOD-RESLJUST-C1-ATTR-IN          
109100                MOVE MFS-NUM-FAELT-FEL                                    
109200                                      TO MOD-TISLJUST-C1-ATTR-IN          
109300                MOVE NEJ              TO INDATA-OK                        
109400             ELSE                                                         
109500              IF WS-TISLJUST-C1 > ZERO                                    
109600                MOVE MFS-NUM-FAELT-RAETT                                  
109700                                      TO MOD-RESLJUST-C1-ATTR-IN          
109800                MOVE MFS-NUM-FAELT-RAETT                                  
109900                                      TO MOD-TISLJUST-C1-ATTR-IN          
110000                MOVE JA               TO SW-UPPDAT-ARTC23                 
110100              END-IF                                                      
110200             END-IF                                                       
110300            END-IF                                                        
110400          END-IF                                                          
110500       END-IF                                                             
110600     END-IF                                                               
110700     SKIP3                                                                
110800     .                                                                    
110900     EJECT                                                                
111000 BAC-KOLLA-ARTC91 SECTION.                                                
111100     SKIP2                                                                
111200     IF MID-KVSPANT-C1-IN NOT = ALL '+'                                   
111300        IF MID-KVSPANT-C1-IN NUMERIC                                      
111400           MOVE MFS-NUM-FAELT-RAETT   TO MOD-KVSPANT-C1-ATTR-IN           
111500           MOVE JA                    TO SW-UPPDAT-ARTC91                 
111600        ELSE                                                              
111700           MOVE NEJ                   TO INDATA-OK                        
111800           MOVE MFS-NUM-FAELT-FEL     TO MOD-KVSPANT-C1-ATTR-IN           
111900        END-IF                                                            
112000     END-IF                                                               
112100                                                                          
112200     IF MID-KVSLAGER-IN-C1 NOT = ALL '+'                                  
112300        IF MID-KVSLAGER-IN-C1 NUMERIC                                     
112400           IF MID-TISLJUST-C1-IN > ZERO OR                                
112500              MID-TISLJUST-C1-UT > ZERO                                   
112600              IF MID-RESLJUST-C1-IN = '9.9' OR                            
112700                 MID-RESLJUST-C1-UT = '9.9'                               
112800                                                                          
112900                MOVE MFS-NUM-FAELT-RAETT TO                               
113000                                    MOD-KVSLAGER-C1-ATTR-IN               
113100                MOVE JA               TO SW-UPPDAT-ARTC91                 
113200              ELSE                                                        
113300                MOVE NEJ                   TO INDATA-OK                   
113400                MOVE MFS-NUM-FAELT-FEL                                    
113500                                    TO MOD-KVSLAGER-C1-ATTR-IN            
113600                                       MOD-RESLJUST-C1-ATTR-IN            
113700                MOVE MFS-ADD-LYS-UPP-FAELT TO                             
113800                                       MOD-RESLJUST-C1-ATTR-UT            
113900              END-IF                                                      
114000           ELSE                                                           
114100              MOVE NEJ               TO INDATA-OK                         
114200              MOVE MFS-NUM-FAELT-FEL TO MOD-KVSLAGER-C1-ATTR-IN           
114300                                        MOD-TISLJUST-C1-ATTR-IN           
114400              MOVE MFS-ADD-LYS-UPP-FAELT TO                               
114500                                     MOD-TISLJUST-C1-ATTR-UT              
114600           END-IF                                                         
114700        ELSE                                                              
114800           MOVE NEJ                   TO INDATA-OK                        
114900           MOVE MFS-NUM-FAELT-FEL     TO MOD-KVSLAGER-C1-ATTR-IN          
115000        END-IF                                                            
115100     END-IF                                                               
115200     .                                                                    
115300                                                                          
115400     EJECT                                                                
115500 BAD-KOLLA-MFG-SHP SECTION.                                               
115600                                                                          
115700     MOVE NEJ                         TO SW-SUPPLIER-VAL-OK               
115800                                         SW-MFG-SHIP-PRESENT              
115900     IF MID-IDLEVNR-FRAM-IN NOT = ALL '+'                                 
116000        IF MID-IDLEVNR-FRAM-IN(1:1) NOT = ' ' AND '+'                     
116100           MOVE JA                    TO INDATA-OK                        
116200           MOVE MFS-NUM-FAELT-RAETT   TO MOD-IDLEVNR-FRAM-ATTR-IN         
116300           MOVE JA                    TO SW-SUPPLIER-VAL-OK               
116400           MOVE MID-IDLEVNR-FRAM-IN   TO W-IDLEVNR                        
116500                                         W-IDLEVNR-KEY                    
116600        ELSE                                                              
116700           MOVE NEJ                   TO INDATA-OK                        
116800           MOVE MFS-NUM-FAELT-FEL     TO MOD-IDLEVNR-FRAM-ATTR-IN         
116900        END-IF                                                            
117000     END-IF                                                               
117100     IF INDATA-OK = JA                                                    
117200        IF MID-IDLEVNR-SHIP-FRAM-IN NOT = ALL '+'                         
117300           IF MID-IDLEVNR-SHIP-FRAM-IN(1:1) NOT = ' ' AND '+'             
117400              MOVE JA                 TO INDATA-OK                        
117500              MOVE MFS-NUM-FAELT-RAETT                                    
117600                                      TO                                  
117700                                    MOD-IDLEVNR-SHIP-FRAM-ATTR-IN         
117800              MOVE JA                 TO SW-SUPPLIER-VAL-OK               
117900              MOVE MID-IDLEVNR-SHIP-FRAM-IN                               
118000                                      TO W-IDLEVNR-SHIP                   
118100           ELSE                                                           
118200              MOVE NEJ                TO INDATA-OK                        
118300              MOVE MFS-NUM-FAELT-FEL  TO                                  
118400                                    MOD-IDLEVNR-SHIP-FRAM-ATTR-IN         
118500           END-IF                                                         
118600        END-IF                                                            
118700     END-IF                                                               
118800*BOTH MFG AND SHIPPING SUPPLIER FIELDS ARE MANDATORY                      
118900     IF INDATA-OK = JA                                                    
119000        IF (MID-IDLEVNR-FRAM-IN  = ALL '+'                                
119100        AND MID-IDLEVNR-SHIP-FRAM-IN NOT = ALL '+')                       
119200        OR (MID-IDLEVNR-FRAM-IN  NOT = ALL '+'                            
119300        AND MID-IDLEVNR-SHIP-FRAM-IN = ALL '+')                           
119400           MOVE NEJ                   TO INDATA-OK                        
119500                                         SW-SUPPLIER-VAL-OK               
119600           IF MID-IDLEVNR-FRAM-IN  = ALL '+'                              
119700               MOVE MFS-OEPPNA-ALFA-FAELT                                 
119800                                      TO MOD-IDLEVNR-FRAM-ATTR-IN         
119900           ELSE                                                           
120000               MOVE MFS-OEPPNA-ALFA-FAELT                                 
120100                                      TO                                  
120200                             MOD-IDLEVNR-SHIP-FRAM-ATTR-IN                
120300           END-IF                                                         
120400           MOVE FEL7                  TO MOD-MESSAGE-RAD1                 
120500           MOVE JA                    TO SW-ERR-MSG-SUPPLIER              
120600        ELSE                                                              
120700           IF MID-IDLEVNR-FRAM-IN  = ALL '+'                              
120800           AND MID-IDLEVNR-SHIP-FRAM-IN = ALL '+'                         
120900               CONTINUE                                                   
121000           ELSE                                                           
121100               MOVE JA                TO SW-SUPPLIER-VAL-OK               
121200                                         INDATA-OK                        
121300                                         SW-MFG-SHIP-PRESENT              
121400           END-IF                                                         
121500        END-IF                                                            
121600     END-IF                                                               
121700*GET SUPPLIER DETAILS FOR THE PART IN ART DATABASE                        
121800     IF INDATA-OK = JA AND                                                
121900        SW-MFG-SHIP-PRESENT = JA                                          
122000         PERFORM IMS-GET-WDK601                                           
122100         IF SEGMENT-FINNS                                                 
122200            MOVE ART-IDLEVNR          TO W-WDK601-IDLEVNR                 
122300            MOVE ART-KDPRODSL         TO TEST-KDPRODSL                    
122400            PERFORM IMS-GET-WDK611                                        
122500              IF SEGMENT-FINNS                                            
122600                 MOVE CLAG-IDLEVNR-SHIP                                   
122700                                      TO W-WDK611-IDLEVNR-SHIP            
122800                 MOVE CLAG-IDDC-REF   TO W-WDK611-IDDC-REF                
122900              ELSE                                                        
123000                 MOVE NEJ             TO INDATA-OK                        
123100              END-IF                                                      
123200         ELSE                                                             
123300            MOVE NEJ                  TO INDATA-OK                        
123400         END-IF                                                           
123500     END-IF                                                               
123600*CHK IF USER ENTERED MFG SUPPLIER IS VALID                                
123700     IF INDATA-OK = JA AND                                                
123800        SW-MFG-SHIP-PRESENT = JA                                          
123900         PERFORM IMS-GET-LEVA                                             
124000         IF SEGMENT-FINNS                                                 
124100             MOVE LEVA-LEV-IDLEVNR-MOTSV                                  
124200                                      TO W-IDLEVNR-MOTSV                  
124300         ELSE                                                             
124400           MOVE NEJ                   TO INDATA-OK                        
124500                                         SW-SUPPLIER-VAL-OK               
124600           MOVE JA                    TO SW-ERR-MSG-SUPPLIER              
124700           MOVE FEL4 TO MOD-MESSAGE-RAD1                                  
124800           MOVE MFS-NUM-FAELT-FEL     TO MOD-IDLEVNR-FRAM-ATTR-IN         
124900         END-IF                                                           
125000     END-IF                                                               
125100*CHK IF USER ENTERED SHIPPING SUPPLIER IS VALID                           
125200     IF INDATA-OK = JA AND                                                
125300       SW-MFG-SHIP-PRESENT = JA                                           
125400         MOVE W-IDLEVNR-SHIP          TO W-IDLEVNR-KEY                    
125500         PERFORM IMS-GET-LEVA                                             
125600         IF SEGMENT-FINNS                                                 
125700             CONTINUE                                                     
125800         ELSE                                                             
125900           MOVE NEJ                   TO INDATA-OK                        
126000                                         SW-SUPPLIER-VAL-OK               
126100           MOVE JA                    TO SW-ERR-MSG-SUPPLIER              
126200           MOVE FEL5                  TO MOD-MESSAGE-RAD1                 
126300           MOVE MFS-NUM-FAELT-FEL     TO                                  
126400                MOD-IDLEVNR-SHIP-FRAM-ATTR-IN                             
126500         END-IF                                                           
126600     END-IF                                                               
126700*IF A PART BELONGS TO PRODUCT GROUP 91,93 THRU 99, USER CANNOT            
126800*UPDATE SUPPLIER AS 1002                                                  
126900     IF INDATA-OK = JA AND                                                
127000        SW-MFG-SHIP-PRESENT = JA                                          
127100         IF (W-IDLEVNR         = '1002 ' AND                              
127200             KDPRODSL-LOCAL)                                              
127300           MOVE NEJ                   TO INDATA-OK                        
127400                                         SW-SUPPLIER-VAL-OK               
127500           MOVE JA                    TO SW-ERR-MSG-SUPPLIER              
127600           MOVE FEL10                 TO MOD-MESSAGE-RAD1                 
127700           MOVE MFS-NUM-FAELT-FEL     TO MOD-IDLEVNR-FRAM-ATTR-IN         
127800         END-IF                                                           
127900     END-IF                                                               
128000*CHK FOR A NUMERIC SUPPLIER ,THE MOTSV FIELD MUST BE SPACES               
128100     IF INDATA-OK = JA AND                                                
128200        SW-MFG-SHIP-PRESENT = JA                                          
128300         MOVE W-IDLEVNR               TO W-IDLEVNR-NUM                    
128400         INSPECT W-IDLEVNR-NUM REPLACING                                  
128500                                  ALL SPACE BY ZERO                       
128600         IF W-IDLEVNR-NUM NUMERIC                                         
128700           AND (W-IDLEVNR-MOTSV NOT = SPACE)                              
128800                                                                          
128900           MOVE NEJ                   TO INDATA-OK                        
129000                                         SW-SUPPLIER-VAL-OK               
129100           MOVE JA                    TO SW-ERR-MSG-SUPPLIER              
129200           MOVE FEL8 TO MOD-MESSAGE-RAD1                                  
129300           MOVE MFS-NUM-FAELT-FEL     TO MOD-IDLEVNR-FRAM-ATTR-IN         
129400         END-IF                                                           
129500     END-IF                                                               
129600                                                                          
129700     IF INDATA-OK = JA AND                                                
129800        SW-MFG-SHIP-PRESENT = JA                                          
129900         MOVE NEJ                     TO W-EXT-TO-REF                     
130000                                         W-REF-TO-EXT                     
130100                                         W-REF-TO-REF                     
130200                                         W-REF-DC-OK                      
130300         PERFORM BADA-GET-IDDC-DC-INFO                                    
130400*IF USER CHANGES A NON REFILL PART TO REFILL PART, THEN MFG AND           
130500*SHIP SUPPLIER HAS TO BE SAME                                             
130600         IF W-EXT-TO-REF = JA                                             
130700            IF MID-IDLEVNR-FRAM-IN NOT =                                  
130800               MID-IDLEVNR-SHIP-FRAM-IN                                   
130900               MOVE NEJ               TO INDATA-OK                        
131000                                         SW-SUPPLIER-VAL-OK               
131100               MOVE JA                TO SW-ERR-MSG-SUPPLIER              
131200               MOVE FEL11             TO MOD-MESSAGE-RAD1                 
131300               MOVE MFS-NUM-FAELT-FEL TO MOD-IDLEVNR-FRAM-ATTR-IN         
131400            ELSE                                                          
131500               MOVE WS-CDC-SE      TO W-IDDC-B6                           
131600               MOVE '4408'         TO W-IDTRANS-B6                        
131700               MOVE DCS-IDDC       TO W-IDDC-REF-B6                       
131800               PERFORM IMS-GU-WDB615                                      
131900               IF SEGMENT-FINNS                                           
132000                 IF DCS-IDDC             = WS-CDC-SE                      
132100                   MOVE MFS-ALFA-FAELT-FEL                                
132200                           TO MOD-IDLEVNR-FRAM-ATTR-IN                    
132300                   MOVE NEJ     TO INDATA-OK                              
132400                   MOVE JA      TO SW-ERR-MSG-SUPPLIER                    
132500                   MOVE FEL12   TO MOD-MESSAGE-RAD1                       
132600                 ELSE                                                     
132700*------------ DISTRICT DETAILS FOR OTHER DCS ARE STORED ON WDB616         
132800                   MOVE DCS-IDDC      TO W-IDDC-REF                       
132900                   PERFORM IMS-GU-WDB616                                  
133000                   IF  SEGMENT-FINNS                                      
133100                   AND REF-IDDISTR-REFILL > ZERO                          
133200                     MOVE MFS-ALFA-FAELT-RAETT                            
133300                             TO MOD-IDLEVNR-FRAM-ATTR-IN                  
133400                   ELSE                                                   
133500                     MOVE MFS-ALFA-FAELT-FEL                              
133600                             TO MOD-IDLEVNR-FRAM-ATTR-IN                  
133700                     MOVE NEJ     TO INDATA-OK                            
133800                     MOVE JA      TO SW-ERR-MSG-SUPPLIER                  
133900                     MOVE FEL12   TO MOD-MESSAGE-RAD1                     
134000                   END-IF                                                 
134100                 END-IF                                                   
134200               ELSE                                                       
134300                 MOVE MFS-ALFA-FAELT-FEL                                  
134400                            TO MOD-IDLEVNR-FRAM-ATTR-IN                   
134500                 MOVE NEJ        TO INDATA-OK                             
134600                 MOVE JA         TO SW-ERR-MSG-SUPPLIER                   
134700                 MOVE FEL12      TO MOD-MESSAGE-RAD1                      
134800               END-IF                                                     
134900            END-IF                                                        
134901                                                                          
134910            IF  INDATA-OK = JA                                            
134920               MOVE DCS-IDDC       TO W-IDDC-REF                          
134921               PERFORM IMS-GU-WDK711-REF                                  
134926               IF SEGMENT-FINNS                                           
134927                 IF (REF-SLAG-IDDC-REF = WS-CDC-SE)                       
134930                   MOVE NEJ        TO INDATA-OK                           
134931                   MOVE JA         TO SW-ERR-MSG-SUPPLIER                 
134932                   MOVE FEL13      TO MOD-MESSAGE-RAD1                    
134933                   MOVE MFS-ALFA-FAELT-FEL                                
134934                                   TO MOD-IDLEVNR-FRAM-ATTR-IN            
134935                 END-IF                                                   
134936               ELSE                                                       
134937                 MOVE NEJ          TO INDATA-OK                           
134938                 MOVE JA           TO SW-ERR-MSG-SUPPLIER                 
134939                 MOVE FEL14        TO MOD-MESSAGE-RAD1                    
134940                 MOVE MFS-ALFA-FAELT-FEL                                  
134941                                   TO MOD-IDLEVNR-FRAM-ATTR-IN            
134942               END-IF                                                     
134950            END-IF                                                        
135000         END-IF                                                           
135100                                                                          
135600         IF INDATA-OK = JA AND                                            
135700            SW-MFG-SHIP-PRESENT = JA                                      
135800             IF (W-REF-TO-REF = JA) OR                                    
135900                (W-EXT-TO-REF = JA AND W-REF-DC-OK = NEJ)                 
136000                MOVE NEJ              TO INDATA-OK                        
136100                                         SW-SUPPLIER-VAL-OK               
136200                MOVE JA               TO SW-ERR-MSG-SUPPLIER              
136300                MOVE FEL8             TO MOD-MESSAGE-RAD1                 
136400               MOVE MFS-NUM-FAELT-FEL TO MOD-IDLEVNR-FRAM-ATTR-IN         
136500             END-IF                                                       
136600         END-IF                                                           
136700     END-IF                                                               
136800     .                                                                    
136900                                                                          
137000     EJECT                                                                
137100*****************************************************************         
137200*W-EXT-TO-REF - WHEN CLAG-IDDC IS SPACES AND NEW SUPPLIER IDDC IS         
137300*               NOT SPACES,THEN USER IS CHANGING FROM NON REFILL          
137400*               TO REFILL PART.                                           
137500*W-REF-TO-REF - USER CHANGING REFILL SUPPLIER TO REFILL SUPPLIER.         
137600*W-REF-TO-EXT - USER CHANGING REFILL SUPPLIER TO NON REFILL.              
137700*               OLD SUPPLIER HAD VALUES IN CLAG-IDDC.NEW SUPPLIER         
137800*               DONT HAVE DATA IN WDB6 DB                                 
137900*****************************************************************         
138000 BADA-GET-IDDC-DC-INFO SECTION.                                           
138100                                                                          
138200     PERFORM IMS-GU-WDB601-LEVNR                                          
138300     IF SEGMENT-FINNS                                                     
138400         MOVE DCS-IDDC                TO WS-IDDC                          
138500                                                                          
138600         IF W-WDK611-IDDC-REF  = SPACE                                    
138700           MOVE JA                    TO W-EXT-TO-REF                     
138800         ELSE                                                             
138900           MOVE JA                    TO W-REF-TO-REF                     
139000         END-IF                                                           
139100                                                                          
139300         IF NDC                                                           
139400           MOVE JA                    TO W-REF-DC-OK                      
139500         END-IF                                                           
139600     ELSE                                                                 
139700         IF W-WDK611-IDDC-REF NOT = SPACE                                 
139800              MOVE JA                 TO W-REF-TO-EXT                     
139900         END-IF                                                           
140000     END-IF                                                               
140100     .                                                                    
140200                                                                          
140300     EJECT                                                                
140400 BB-UPPDATERA-ARTC12 SECTION.                                             
140500*                                                                         
140600*    TESTAR VILKET FÄLT SOM SKALL ÄNDRAS OCH I VISSA FALL                 
140700*    HÄMTAS INFORMATION FRÅN LEVERANTÖRSREGISTRET.                        
140800     SKIP2                                                                
140900     IF W-IDLEVNR-KEY = SPACE                                             
141000        CONTINUE                                                          
141100     ELSE                                                                 
141200        IF WS-IDPLANGR-AG = 0                                             
141300           MOVE NEJ                    TO SW-UPPDAT-ARTC12-OK             
141400           MOVE MFS-NUM-FAELT-FEL      TO MOD-IDANSK-ATTR-IN              
141500                                          MOD-IDPLANGR-AG-ATTR-IN         
141600        END-IF                                                            
141700     END-IF                                                               
141800     IF MID-IDANSK-IN NOT = ALL '+' AND SW-UPPDAT-ARTC12-OK               
141900        MOVE ZERO TO WS-SPAR-IDANSK                                       
142000        PERFORM BBA-UPPDAT-IDANSK                                         
142100     END-IF                                                               
142200     IF SW-UPPDAT-ARTC12-OK = JA                                          
142300                                                                          
142400        IF MID-IDPLANGR-AG-IN NOT = ALL '+'                               
142500           MOVE ZERO TO WS-SPAR-IDANSK                                    
142600           PERFORM BBD-UPPDAT-IDPLANGR-AG                                 
142700        END-IF                                                            
142800                                                                          
142900        IF MID-IDPLANGR-LEV-IN NOT =  ALL '+'                             
143000           MOVE MID-IDPLANGR-LEV-IN   TO WS-IDPLANGR-LEV                  
143100           MOVE WS-IDPLANGR-LEV       TO CLAG-IDPLANGR-LEV                
143200           MOVE MID-IDPLANGR-LEV-IN   TO MOD-IDPLANGR-LEV-UT              
143300           MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-IDPLANGR-LEV-ATTR-UT         
143400        END-IF                                                            
143500                                                                          
143600        IF MID-KVQ-JUST-IN NOT = ALL '+'                                  
143700           MOVE MID-KVQ-JUST-IN       TO CLAG-KVQ-JUST                    
143800                                         MOD-KVQ-JUST-UT                  
143900           MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-KVQ-JUST-ATTR-UT             
144000        END-IF                                                            
144100                                                                          
144200        IF MID-TIQJUST-IN NOT = ALL '+'                                   
144300           MOVE MID-TIQJUST-IN        TO WS-TIQJUST                       
144400           MOVE WS-TIQJUST            TO CLAG-TIQJUST                     
144500           MOVE MID-TIQJUST-IN        TO MOD-TIQJUST-UT                   
144600           MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-TIQJUST-ATTR-UT              
144700        END-IF                                                            
144800                                                                          
144900        IF MID-KVVECKOR-LT-IN NOT = ALL '+'                               
145000           MOVE JA                    TO CLAG-FLMANLT                     
145100                                         MOD-FLMANLT-UT                   
145200           MOVE MID-KVVECKOR-LT-IN    TO WS-KVVECKOR-LT                   
145300           MOVE WS-KVVECKOR-LT        TO CLAG-KVVECKOR-LT                 
145400           MOVE MID-KVVECKOR-LT-IN    TO MOD-KVVECKOR-LT-UT               
145500           MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-KVVECKOR-LT-ATTR-UT          
145600                                         MOD-FLMANLT-ATTR-UT              
145700           COMPUTE WS-KVVECKOR-FT ROUNDED =                               
145800                   WS-KVVECKOR-LT + (CLAG-KVDAGAR-FFH / 5)                
145900           MOVE WS-KVVECKOR-FT        TO CLAG-KVVECKOR-FT                 
146000                                                                          
146100           MOVE JA                    TO SW-2213                          
146200        END-IF                                                            
146300                                                                          
146400        IF MID-FLMANLT-IN NOT = ALL '+'                                   
146500           PERFORM BBC-UPPDAT-FLMANLT                                     
146600        END-IF                                                            
146700                                                                          
146800        IF MID-KVVECKOR-AT-IN NOT = ALL '+'                               
146900           MOVE JA                    TO CLAG-FLMANAT                     
147000                                         MOD-FLMANAT-UT                   
147100           MOVE MID-KVVECKOR-AT-IN    TO WS-KVVECKOR-AT                   
147200           MOVE WS-KVVECKOR-AT        TO CLAG-KVVECKOR-AT                 
147300           MOVE MID-KVVECKOR-AT-IN    TO MOD-KVVECKOR-AT-UT               
147400           MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-KVVECKOR-AT-ATTR-UT          
147500                                         MOD-FLMANAT-ATTR-UT              
147600           MOVE JA                    TO SW-2213                          
147700        END-IF                                                            
147800                                                                          
147900        IF MID-FLMANAT-IN NOT = ALL '+'                                   
148000           PERFORM BBB-UPPDAT-FLMANAT                                     
148100        END-IF                                                            
148200                                                                          
148300        IF (MID-KVQ-IN NOT = ALL '+') AND MFS-UPD-V                       
148400           MOVE JA                    TO CLAG-FLMANQ                      
148500                                         MOD-FLMANQ-UT                    
148600           MOVE MID-KVQ-IN            TO WS-KVQ                           
148700           MOVE WS-KVQ                TO CLAG-KVQ                         
148800           MOVE MID-KVQ-IN            TO MOD-KVQ-UT                       
148900           MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-KVQ-ATTR-UT                  
149000                                         MOD-FLMANQ-ATTR-UT               
149100           MOVE JA                    TO SW-2204                          
149200        END-IF                                                            
149300                                                                          
149400        IF (MID-FLMANQ-IN NOT = ALL '+') AND MFS-UPD-V                    
149500           MOVE MID-FLMANQ-IN         TO CLAG-FLMANQ                      
149600                                         MOD-FLMANQ-UT                    
149700           MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-FLMANQ-ATTR-UT               
149800        END-IF                                                            
149900                                                                          
150000        IF MID-KVULOAD-IN  NOT = ALL '+'                                  
150100           IF MID-KVULOAD-IN  = ZERO                                      
150200              MOVE ZERO              TO CLAG-KVULOAD                      
150300              MOVE MFS-RENSA-FAELT   TO MOD-KVULOAD-UT                    
150400              MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-KVULOAD-ATTR-UT           
150500           ELSE                                                           
150600              MOVE MID-KVULOAD-IN       TO CLAG-KVULOAD                   
150700                                           MOD-KVULOAD-UT                 
150800              MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-KVULOAD-ATTR-UT           
150900           END-IF                                                         
151000        END-IF                                                            
151100                                                                          
151200        IF MID-FLREFILL-IN NOT = ALL '+'                                  
151300           MOVE MID-FLREFILL-IN       TO CLAG-FLREFILL                    
151400                                         MOD-FLREFILL-UT                  
151500           MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-FLREFILL-ATTR-UT             
151600        END-IF                                                            
151700                                                                          
151800        IF MID-TIREFSTO-IN NOT = ALL '+'                                  
151900           MOVE WS-TIREFSTO          TO CLAG-TIREFSTO                     
152000                                         MOD-TIREFSTO-UT                  
152100           MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-TIREFSTO-ATTR-UT             
152200        END-IF                                                            
152300                                                                          
152400        IF MID-KVBK-IN NOT = ALL '+'                                      
152500          MOVE JA                     TO  CLAG-FLMANBK                    
152600                                         MOD-FLMANBK-UT                   
152700          MOVE MID-KVBK-IN            TO WS-KVBK                          
152800          MOVE WS-KVBK                TO CLAG-KVBK                        
152900          MOVE MID-KVBK-IN            TO MOD-KVBK-UT                      
153000          MOVE MFS-ADD-LYS-UPP-FAELT  TO MOD-KVBK-ATTR-UT                 
153100                                         MOD-FLMANBK-ATTR-UT              
153200        END-IF                                                            
153300                                                                          
153400        IF MID-FLMANBK-IN NOT = ALL '+'                                   
153500           MOVE MID-FLMANBK-IN        TO CLAG-FLMANBK                     
153600                                         MOD-FLMANBK-UT                   
153700           MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-FLMANBK-ATTR-UT              
153800        END-IF                                                            
153900                                                                          
154000        IF MID-KVPALL-IN NOT = ALL '+'                                    
154100           IF MID-KVPALL-IN NUMERIC                                       
154200              MOVE MID-KVPALL-IN TO WS-KVPALL                             
154300              MOVE WS-KVPALL TO CLAG-KVPALL                               
154400                                MOD-KVPALL-UT                             
154500              MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-KVPALL-ATTR-UT            
154600              IF CLAG-FLCDART = JA                                        
154700                MOVE 'OBS CD ARTIKEL'    TO MOD-MESSAGE-RAD1              
154800              END-IF                                                      
154900           END-IF                                                         
155000        END-IF                                                            
155100     END-IF                                                               
155200     .                                                                    
155300                                                                          
155400     EJECT                                                                
155500 BBA-UPPDAT-IDANSK SECTION.                                               
155600*                                                                         
155700*    KONTROLL OCH UPPDATERING AV IDANSK                                   
155800*    OM INGET VILLKOR UPPFYLLS SÄTTS SW-UPPDAT-CLAG-OK TILL NEJ           
155900                                                                          
156000                                                                          
156100     IF   CLAG-IDPLANGR-AG = 9                                            
156200          OR W-IDLEVNR-KEY = SPACE OR '9996' OR                           
156300                             '9997' OR '9998' OR '9999'                   
156400                                                                          
156500         MOVE CLAG-IDANSK         TO WS-SPAR-IDANSK                       
156600         MOVE MID-IDANSK-IN       TO WS-IDANSK                            
156700         MOVE WS-IDANSK           TO CLAG-IDANSK                          
156800         IF WS-SPAR-IDANSK = 701 OR 702 OR 703                            
156900            MOVE JA TO SW-IDANSK-UPPD                                     
157000         ELSE                                                             
157100            MOVE NEJ TO SW-IDANSK-UPPD                                    
157200         END-IF                                                           
157300         MOVE MID-IDANSK-IN       TO MOD-IDANSK-UT                        
157400         MOVE MFS-ADD-LYS-UPP-FAELT                                       
157500                                  TO MOD-IDANSK-ATTR-UT                   
157600                                                                          
157700     ELSE                                                                 
157800        MOVE NEJ                  TO SW-UPPDAT-ARTC12-OK                  
157900        MOVE MFS-NUM-FAELT-FEL    TO MOD-IDANSK-ATTR-IN                   
158000     END-IF                                                               
158100     .                                                                    
158200     EJECT                                                                
158300 BBB-UPPDAT-FLMANAT SECTION.                                              
158400*                                                                         
158500*    UPPDATERING AV FLMANAT. OM FLMANAT SÄTTS TILL NEJ UPPDATERAS         
158600*    KVVECKOR-AT FRÅN LEVERANTÖRSREGISTRET OCH                            
158700*     2213-POST SKRIVS PÅ HÄNDELSEREGISTRET                               
158800*                                                                         
158900                                                                          
159000     MOVE MID-FLMANAT-IN              TO CLAG-FLMANAT                     
159100                                         MOD-FLMANAT-UT                   
159200     MOVE MFS-ADD-LYS-UPP-FAELT       TO MOD-FLMANAT-ATTR-UT              
159300                                                                          
159400     IF CLAG-FLMANAT = NEJ                                                
159500        IF CLAG-KDHF > ZERO                                               
159600           MOVE 16                    TO CLAG-KVVECKOR-AT                 
159700                                         MOD-KVVECKOR-AT-UT               
159800           MOVE MFS-ADD-LYS-UPP-FAELT                                     
159900                                      TO MOD-KVVECKOR-AT-ATTR-UT          
160000           MOVE JA                    TO SW-2213                          
160100        ELSE                                                              
160200           MOVE ART-IDLEVNR           TO W-IDLEVNR-KEY                    
160300           PERFORM IMS-GET-LEVA                                           
160400           IF SEGMENT-FINNS                                               
160500              MOVE LEVA-LEV-KVVECKOR-AT   TO CLAG-KVVECKOR-AT             
160600              MOVE LEVA-LEV-KVVECKOR-AT   TO WS-KVVECKOR-AT               
160700              MOVE WS-KVVECKOR-AT     TO MOD-KVVECKOR-AT-UT               
160800              MOVE MFS-ADD-LYS-UPP-FAELT                                  
160900                                      TO MOD-KVVECKOR-AT-ATTR-UT          
161000              MOVE JA                 TO SW-2213                          
161100           END-IF                                                         
161200        END-IF                                                            
161300     END-IF                                                               
161400     .                                                                    
161500                                                                          
161600     EJECT                                                                
161700 BBC-UPPDAT-FLMANLT SECTION.                                              
161800*                                                                         
161900*    UPPDATERING AV FLMANLT. OM FLMANLT SÄTTS TILL NEJ UPPDATERAS         
162000*    KVVECKOR-LT FRÅN LEVERANTÖRSREGISTRET OCH                            
162100*    2213-POST SKRIVS PÅ HÄNDELSEREGISTRET                                
162200*                                                                         
162300     SKIP2                                                                
162400     MOVE MID-FLMANLT-IN              TO CLAG-FLMANLT                     
162500                                         MOD-FLMANLT-UT                   
162600     MOVE MFS-ADD-LYS-UPP-FAELT       TO MOD-FLMANLT-ATTR-UT              
162700                                                                          
162800     IF   CLAG-FLMANLT = NEJ                                              
162900        IF CLAG-KDHF > ZERO                                               
163000          IF CLAG-KDHF = 1                                                
163100             MOVE 6                   TO CLAG-KVVECKOR-LT                 
163200                                         MOD-KVVECKOR-LT-UT               
163300                                         WS-KVVECKOR-LT                   
163400          ELSE                                                            
163500             MOVE 8                   TO CLAG-KVVECKOR-LT                 
163600                                         MOD-KVVECKOR-LT-UT               
163700                                         WS-KVVECKOR-LT                   
163800          END-IF                                                          
163900          MOVE MFS-ADD-LYS-UPP-FAELT  TO MOD-KVVECKOR-LT-ATTR-UT          
164000          MOVE JA                     TO SW-2213                          
164100        ELSE                                                              
164200           MOVE ART-IDLEVNR           TO W-IDLEVNR-KEY                    
164300           PERFORM IMS-GET-LEVA                                           
164400           IF SEGMENT-FINNS                                               
164500              MOVE LEVA-LEV-KVVECKOR-LT   TO CLAG-KVVECKOR-LT             
164600              MOVE LEVA-LEV-KVVECKOR-LT   TO WS-KVVECKOR-LT               
164700              MOVE WS-KVVECKOR-LT     TO MOD-KVVECKOR-LT-UT               
164800              MOVE MFS-ADD-LYS-UPP-FAELT                                  
164900                                      TO MOD-KVVECKOR-LT-ATTR-UT          
165000              MOVE JA                 TO SW-2213                          
165100           END-IF                                                         
165200        END-IF                                                            
165300        COMPUTE WS-KVVECKOR-FT  ROUNDED =                                 
165400                WS-KVVECKOR-LT + (CLAG-KVDAGAR-FFH / 5)                   
165500        MOVE WS-KVVECKOR-FT        TO CLAG-KVVECKOR-FT                    
165600     END-IF                                                               
165700     .                                                                    
165800     EJECT                                                                
165900 BBD-UPPDAT-IDPLANGR-AG SECTION.                                          
166000*                                                                         
166100*    KONTROLL OCH ÄNDRING AV IDPLANGR-AG                                  
166200*    OM IDPLANGR-AG ÄR < 9 UPPDATERAS IDANSK FRÅN                         
166300*    LEVERANTÖRSREGISTRET                                                 
166400     SKIP2                                                                
166500      MOVE MID-IDPLANGR-AG-IN         TO WS-IDPLANGR-AG                   
166600      MOVE WS-IDPLANGR-AG             TO CLAG-IDPLANGR-AG                 
166700      MOVE MID-IDPLANGR-AG-IN         TO MOD-IDPLANGR-AG-UT               
166800      MOVE MFS-ADD-LYS-UPP-FAELT      TO MOD-IDPLANGR-AG-ATTR-UT          
166900                                                                          
167000      IF MID-IDPLANGR-AG-IN > ZERO                                        
167100                                                                          
167200         IF MID-IDPLANGR-AG-IN < '9'                                      
167300            PERFORM IMS-GET-LEVA                                          
167400                                                                          
167500            IF SEGMENT-FINNS                                              
167600                MOVE CLAG-IDANSK      TO WS-SPAR-IDANSK                   
167700                MOVE LEVA-LEV-IDANSK-PG (CLAG-IDPLANGR-AG)                
167800                                      TO CLAG-IDANSK                      
167900                MOVE LEVA-LEV-IDANSK-PG (CLAG-IDPLANGR-AG)                
168000                                      TO WS-IDANSK                        
168100                MOVE WS-IDANSK        TO MOD-IDANSK-UT                    
168200                IF WS-SPAR-IDANSK = 701 OR 702 OR 703                     
168300                   MOVE JA TO SW-IDANSK-UPPD                              
168400                ELSE                                                      
168500                   MOVE NEJ TO SW-IDANSK-UPPD                             
168600                END-IF                                                    
168700                MOVE MFS-ADD-LYS-UPP-FAELT                                
168800                                      TO MOD-IDANSK-ATTR-UT               
168900            END-IF                                                        
169000         END-IF                                                           
169100      END-IF                                                              
169200     .                                                                    
169300                                                                          
169400                                                                          
169500     EJECT                                                                
169600 BBE-RENSA-ARTC12-FAELT SECTION.                                          
169700     SKIP2                                                                
169800     MOVE MFS-RENSA-FAELT             TO MOD-IDANSK-IN                    
169900                                         MOD-IDPLANGR-AG-IN               
170000                                         MOD-IDPLANGR-LEV-IN              
170100                                         MOD-KVQ-JUST-IN                  
170200                                         MOD-TIQJUST-IN                   
170300                                         MOD-KVVECKOR-LT-IN               
170400                                         MOD-FLMANLT-IN                   
170500                                         MOD-KVVECKOR-AT-IN               
170600                                         MOD-FLMANAT-IN                   
170700                                         MOD-KVQ-IN                       
170800                                         MOD-FLMANQ-IN                    
170900                                         MOD-KVULOAD-IN                   
171000                                         MOD-FLREFILL-IN                  
171100                                         MOD-TIREFSTO-IN                  
171200                                         MOD-KVBK-IN                      
171300                                         MOD-FLMANBK-IN                   
171400                                         MOD-KVPALL-IN                    
171500                                         MOD-IDLEVNR-FRAM-IN              
171600                                         MOD-IDLEVNR-SHIP-FRAM-IN         
171700     .                                                                    
171800     EJECT                                                                
171900 BC-UPPDATERA-ARTC23 SECTION.                                             
172000     SKIP2                                                                
172100        IF MID-RESLJUST-C1-IN NOT =  ALL '+'                              
172200           MOVE WS-RESLJUST-C1        TO CLAG-RESLJUST                    
172300           MOVE MID-RESLJUST-C1-IN    TO MOD-RESLJUST-C1-UT               
172400           MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-RESLJUST-C1-ATTR-UT          
172500        END-IF                                                            
172600                                                                          
172700        IF MID-TISLJUST-C1-IN NOT = ALL '+'                               
172800           MOVE WS-TISLJUST-C1        TO CLAG-TISLJUST                    
172900           MOVE MID-TISLJUST-C1-IN    TO MOD-TISLJUST-C1-UT               
173000           MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-TISLJUST-C1-ATTR-UT          
173100        END-IF                                                            
173200                                                                          
173300     .                                                                    
173400     EJECT                                                                
173500 BCA-RENSA-ARTC23-FAELT SECTION.                                          
173600     SKIP2                                                                
173700        MOVE MFS-RENSA-FAELT          TO MOD-RESLJUST-C1-IN               
173800                                         MOD-TISLJUST-C1-IN               
173900        MOVE MFS-RENSA-FAELT          TO MOD-RESLJUST-C2-IN               
174000                                         MOD-TISLJUST-C2-IN               
174100     .                                                                    
174200     EJECT                                                                
174300 BD-UPPDATERA-ARTC91 SECTION.                                             
174400     SKIP2                                                                
174500     MOVE NEJ TO TAECKNING                                                
174600                                                                          
174700        IF MID-KVSPANT-C1-IN NOT =  ALL '+'                               
174800                                                                          
174900           MOVE MID-KVSPANT-C1-IN     TO WS-KVSPANT                       
175000           IF WS-KVSPANT < CLAG-KVSPANT                                   
175100              MOVE JA TO TAECKNING                                        
175200           END-IF                                                         
175300           MOVE WS-KVSPANT            TO CLAG-KVSPANT                     
175400           MOVE MID-KVSPANT-C1-IN     TO MOD-KVSPANT-C1-UT                
175500           MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-KVSPANT-C1-ATTR-UT           
175600                                                                          
175700        END-IF                                                            
175800                                                                          
175900        IF MID-KVSLAGER-IN-C1 NOT = ALL '+'                               
176000           IF MID-KVSLAGER-IN-C1 NUMERIC                                  
176100             MOVE MID-KVSLAGER-IN-C1  TO WS-KVSLAGER-C1                   
176200             MOVE WS-KVSLAGER-C1      TO CLAG-KVSLAGER                    
176300                                         MOD-KVSLAGER-C1-UT               
176400             MOVE MFS-ADD-LYS-UPP-FAELT                                   
176500                                  TO MOD-KVSLAGER-C1-ATTR-UT              
176600           END-IF                                                         
176700        END-IF                                                            
176800                                                                          
176900     .                                                                    
177000     EJECT                                                                
177100 BDA-RENSA-ARTC91-FAELT SECTION.                                          
177200     SKIP2                                                                
177300        MOVE MFS-RENSA-FAELT          TO MOD-KVSPANT-C1-IN                
177400                                         MOD-KVSLAGER-C1-IN               
177500        MOVE MFS-RENSA-FAELT          TO MOD-KVSPANT-C2-IN                
177600                                         MOD-KVSLAGER-C2-IN               
177700     .                                                                    
177800     EJECT                                                                
177900 BE-ROER-EJ-FAELT SECTION.                                                
178000     SKIP2                                                                
178100     MOVE MFS-ROER-EJ-FAELT           TO MOD-IDANSK-IN                    
178200                                         MOD-KVSPANT-C1-IN                
178300                                         MOD-KVSPANT-C2-IN                
178400                                         MOD-IDPLANGR-AG-IN               
178500                                         MOD-IDPLANGR-LEV-IN              
178600                                         MOD-TIREFSTO-IN                  
178700                                         MOD-KVQ-JUST-IN                  
178800                                         MOD-TIQJUST-IN                   
178900                                         MOD-RESLJUST-C1-IN               
179000                                         MOD-RESLJUST-C2-IN               
179100                                         MOD-KVVECKOR-LT-IN               
179200                                         MOD-FLMANLT-IN                   
179300                                         MOD-TISLJUST-C1-IN               
179400                                         MOD-TISLJUST-C2-IN               
179500                                         MOD-KVVECKOR-AT-IN               
179600                                         MOD-FLMANAT-IN                   
179700                                         MOD-KVQ-IN                       
179800                                         MOD-FLMANQ-IN                    
179900                                         MOD-KVULOAD-IN                   
180000                                         MOD-FLREFILL-IN                  
180100                                         MOD-KVBK-IN                      
180200                                         MOD-FLMANBK-IN                   
180300                                         MOD-KVPALL-IN                    
180400                                         MOD-KVSLAGER-C1-IN               
180500                                         MOD-KVSLAGER-C2-IN               
180600                                         MOD-IDLEVNR-FRAM-IN              
180700                                         MOD-IDLEVNR-SHIP-FRAM-IN         
180800     .                                                                    
180900     EJECT                                                                
181000 BF-UPPDATERA-2213-WDG3 SECTION.                                          
181100*                                                                         
181200*    SKRIVER 2213-POST PÅ HÄNDELSEREGISTRET                               
181300*                                                                         
181400     SKIP2                                                                
181500     MOVE IDARTNR-WS                  TO XXBI11-2214-IDARTNR              
181600                                                                          
181700     PERFORM IMS-ISRT-WLXXBI                                              
181800     .                                                                    
181900     SKIP3                                                                
182000 BGA-UPPDATERA-2204-WDG3 SECTION.                                         
182100*                                                                         
182200*    SKRIVER 2204-POST PÅ HÄNDELSEREGISTRET                               
182300*                                                                         
182400     SKIP2                                                                
182500     MOVE IDARTNR-WS                  TO XXBJ11-2204-IDARTNR              
182600     MOVE 15                          TO XXBJ11-2204-KDLPORS              
182700                                                                          
182800     PERFORM IMS-ISRT-WLXXBJ                                              
182900     .                                                                    
183000     SKIP3                                                                
183100                                                                          
183200     EJECT                                                                
183300 BH-TAECKNING SECTION.                                                    
183400     SKIP2                                                                
183500     IF CLAG-KVROS > 0                                                    
183600       MOVE CLAG-KVLS       TO W-ART-KVLS                                 
183700       MOVE CLAG-KVUTRS     TO W-ART-KVUTRS                               
183800       MOVE CLAG-KVRESS     TO W-ART-KVRESS                               
183900       MOVE CLAG-KVSPANT    TO W-ART-KVSPANT                              
184000       MOVE CLAG-KDERS      TO W-ART-KDERS                                
184100       MOVE CLAG-KVSLAGER TO W-ART-KVSLAGER                               
184200       PERFORM IMS-GET-WDK611                                             
184300       IF SEGMENT-FINNS                                                   
184400         MOVE CLAG-KDLTK   TO W-ART-KDLTK                                 
184500         MOVE JA TO TAECKNING                                             
184600            IF CLAG-KDLEVSP = 20 OR 21                                    
184700               MOVE NEJ TO TAECKNING                                      
184800            END-IF                                                        
184900         IF TAECKNING = JA                                                
185000               IF CLAG-PRARTSTD > 0                                       
185100                 MOVE ZERO TO DISPONIBELT                                 
185200                 COMPUTE DISPONIBELT =                                    
185300                         W-ART-KVLS   - W-ART-KVUTRS -                    
185400                         W-ART-KVRESS - W-ART-KVSPANT                     
185500                 IF DISPONIBELT > 0                                       
185600                      MOVE WS-CDC-SE      TO W-IDDC-4505                  
185700                      MOVE W-IDARTNR-KEY  TO 4506-IDARTNR                 
185800                      MOVE 13             TO 4506-KDTAKORS                
185900                      MOVE ZERO           TO 4506-KVANTMOT                
186000                      PERFORM IMS-ISRT-450511                             
186100                 END-IF                                                   
186200               END-IF                                                     
186300         END-IF                                                           
186400       END-IF                                                             
186500     END-IF                                                               
186600     .                                                                    
186700     EJECT                                                                
186800                                                                          
186900                                                                          
187000 BI-UPPDAT-ARTS11 SECTION.                                                
187100                                                                          
187200     PERFORM IMS-GU-ARTS01                                                
187300     IF SEGMENT-FINNS                                                     
187400        PERFORM IMS-GHNP-ARTS11                                           
187500        PERFORM UNTIL SEGMENT-SAKNAS                                      
187600           MOVE SLAG-IDDC TO W-IDDC-B6                                    
187700           PERFORM IMS-GU-WDB601                                          
187800           IF DCS-SDC                                                     
187900              MOVE 'P'            TO SLAG-KDREFSTA                        
188000              MOVE DAGENS-DATUM   TO SLAG-TIREFSTA                        
188100              MOVE ZERO           TO SLAG-KVPB-REF                        
188200                                     SLAG-KVPB-HIST                       
188300                                     SLAG-KVREFBER                        
188400                                     SLAG-KVREFOVL                        
188500                                     SLAG-KVREFPKT                        
188600              MOVE ZERO           TO SLAG-TIREFMPB                        
188700                                     SLAG-TIREFPAF                        
188800                                     SLAG-TIREFPKT                        
188900                                     SLAG-TIREFSTO                        
189000           END-IF                                                         
189100           PERFORM IMS-REPL-ARTS11                                        
189200           PERFORM IMS-GHNP-ARTS11                                        
189300        END-PERFORM                                                       
189400     END-IF                                                               
189500     .                                                                    
189600     EJECT                                                                
189700 BJ-UPDATE-MFG-SHP SECTION.                                               
189800                                                                          
189900     PERFORM IMS-GHN-WDG901                                               
190000     PERFORM UNTIL SEGMENT-SAKNAS OR BASEN-SLUT                           
190100          IF BAS-R01-IDPTYP  = 'R01' AND                                  
190200             BAS-R01-IDARTNR = W-IDARTNR-KEY                              
190300              PERFORM IMS-DLET-WDG901                                     
190400          END-IF                                                          
190500          PERFORM IMS-GHN-WDG901                                          
190600     END-PERFORM                                                          
190700     PERFORM BJA-INSERT-EVENT-DB                                          
190800     .                                                                    
190900     EJECT                                                                
191000                                                                          
191100 BJA-INSERT-EVENT-DB SECTION.                                             
191200                                                                          
191300     MOVE 'R01'                       TO BAS-R01-IDPTYP                   
191400     MOVE W-IDARTNR-KEY               TO BAS-R01-IDARTNR                  
191500     MOVE W-IDLEVNR                   TO BAS-R01-IDLEVNR                  
191600     MOVE W-IDLEVNR-SHIP              TO BAS-R01-IDLEVNR-SHIP             
191700     MOVE SPACE                       TO BAS-R01-IDSYSTEM                 
191800                                         BAS-R01-REST                     
191900                                                                          
192000                                                                          
192100     MOVE MSG-JULIAN-DATE             TO  WDG901-POST-TIREGDAT            
192200     MOVE MSG-TIME-OF-DAY             TO  WDG901-POST-TIKLOCK             
192300     MOVE MSG-SIGNON-USERID           TO  WDG901-POST-IDUSER              
192400     MOVE MSG-LTERM-NAME              TO  WDG901-POST-IDLTERM             
192500     MOVE ZERO                        TO  WDG901-POST-TIBORT              
192600                                                                          
192700     PERFORM IMS-ISRT-WDG901                                              
192800     MOVE W-IDLEVNR                   TO MOD-IDLEVNR-FRAM-UT              
192900     MOVE W-IDLEVNR-SHIP              TO MOD-IDLEVNR-SHIP-FRAM-UT         
193000     MOVE MFS-ADD-HILIGHT-FIELD       TO MOD-IDLEVNR-FRAM-ATTR-UT         
193100                                    MOD-IDLEVNR-SHIP-FRAM-ATTR-UT         
193200     .                                                                    
193300     EJECT                                                                
193400                                                                          
193500                                                                          
193600 C-FRAGA SECTION.                                                         
193700     SKIP2                                                                
193800     PERFORM IMS-GET-WDK601                                               
193900     IF SEGMENT-FINNS                                                     
194000        MOVE ART-IDLEVNR             TO W-IDLEVNR                         
194100        PERFORM IMS-GET-WDK611                                            
194200        IF SEGMENT-FINNS                                                  
194300           PERFORM CA-FLYTTA-ARTC12                                       
194400           PERFORM CAA-OEPPNA-ARTC12-FAELT                                
194500                                                                          
194600           PERFORM CB-FLYTTA-ARTC23-C1                                    
194700           PERFORM CBA-OEPPNA-ARTC23-C1                                   
194800           PERFORM CCB-RENSA-ARTC23-C2                                    
194900                                                                          
195000           MOVE CLAG-IDLEVNR-SHIP     TO W-IDLEVNR-SHIP                   
195100           MOVE CLAG-KVSPANT TO WS-KVSPANT                                
195200           MOVE WS-KVSPANT TO MOD-KVSPANT-C1-UT                           
195300           MOVE MFS-OEPPNA-NUM-FAELT TO MOD-KVSPANT-C1-ATTR-IN            
195400                                                                          
195500           MOVE CLAG-KVSLAGER TO WS-KVSLAGER-C1                           
195600           MOVE WS-KVSLAGER-C1 TO MOD-KVSLAGER-C1-UT                      
195700           MOVE MFS-OEPPNA-NUM-FAELT TO MOD-KVSLAGER-C1-ATTR-IN           
195800                                                                          
195900           MOVE MFS-RENSA-FAELT      TO MOD-KVSPANT-C2-UT                 
196000                                        MOD-KVSLAGER-C2-UT                
196100                                                                          
196200           IF CLAG-IDDC-REF NOT = SPACE                                   
196300              MOVE INF-REFILL-PART  TO MED-IDMFSFEL                       
196400              CALL WMEDKONV      USING MED-WMEDAREA                       
196500              MOVE MED-MFSFEL       TO MOD-MESSAGE-RAD1                   
196600              PERFORM CD-CLOSE-ARTC12-FAELT                               
196700           END-IF                                                         
196800        ELSE                                                              
196900           MOVE JA                   TO W-WDK611-ABSENT                   
197000        END-IF                                                            
197100                                                                          
197200        PERFORM IMS-GET-BENA01                                            
197300        IF SEGMENT-FINNS                                                  
197400           MOVE 'S  ' TO W-IDSKYLT-KEY                                    
197500           PERFORM IMS-GET-BENA11                                         
197600           IF SEGMENT-FINNS                                               
197700              MOVE BENA-TEXT-BEART TO MOD-BEART-SVE                       
197800           END-IF                                                         
197900        END-IF                                                            
198000                                                                          
198100        MOVE CLAG-IDDC-REF           TO W-IDDC-WDK7                       
198200        IF W-WDK611-ABSENT NOT = JA                                       
198300           PERFORM CE-GET-MFG-SHIP-DETAILS                                
198400        ELSE                                                              
198500           MOVE FEL9                 TO MOD-MESSAGE-RAD1                  
198600           PERFORM S01-CLOSE-SUPPLIER-UPD                                 
198700        END-IF                                                            
198800     ELSE                                                                 
198900        MOVE FEL2 TO MOD-MESSAGE-RAD1                                     
199000     END-IF                                                               
199100     .                                                                    
199200     EJECT                                                                
199300 CA-FLYTTA-ARTC12 SECTION.                                                
199400     SKIP2                                                                
199500     MOVE CLAG-IDANSK                 TO WS-IDANSK                        
199600     MOVE WS-IDANSK                   TO MOD-IDANSK-UT                    
199700     MOVE CLAG-IDPLANGR-AG            TO WS-IDPLANGR-AG                   
199800     MOVE WS-IDPLANGR-AG              TO MOD-IDPLANGR-AG-UT               
199900     MOVE CLAG-IDPLANGR-LEV           TO WS-IDPLANGR-LEV                  
200000     MOVE WS-IDPLANGR-LEV             TO MOD-IDPLANGR-LEV-UT              
200100     MOVE CLAG-KVQ-JUST               TO WS-KVQ-JUST                      
200200     MOVE WS-KVQ-JUST                 TO MOD-KVQ-JUST-UT                  
200300     MOVE CLAG-TIQJUST                TO WS-TIQJUST                       
200400     MOVE WS-TIQJUST                  TO MOD-TIQJUST-UT                   
200500     MOVE CLAG-KVVECKOR-LT            TO WS-KVVECKOR-LT                   
200600     MOVE WS-KVVECKOR-LT              TO MOD-KVVECKOR-LT-UT               
200700     MOVE CLAG-FLMANLT                TO MOD-FLMANLT-UT                   
200800     MOVE CLAG-KVVECKOR-AT            TO WS-KVVECKOR-AT                   
200900     MOVE WS-KVVECKOR-AT              TO MOD-KVVECKOR-AT-UT               
201000     MOVE CLAG-FLMANAT                TO MOD-FLMANAT-UT                   
201100     MOVE CLAG-KVQ                    TO WS-KVQ                           
201200     MOVE WS-KVQ                      TO MOD-KVQ-UT                       
201300     MOVE CLAG-FLMANQ                 TO MOD-FLMANQ-UT                    
201400     MOVE CLAG-KVULOAD                TO MOD-KVULOAD-UT                   
201500     MOVE CLAG-FLREFILL               TO MOD-FLREFILL-UT                  
201600     MOVE CLAG-TIREFSTO               TO WS-TIREFSTO-UT                   
201700     MOVE WS-TIREFSTO-UT              TO MOD-TIREFSTO-UT                  
201800     MOVE CLAG-KVBK                   TO WS-KVBK                          
201900     MOVE WS-KVBK                     TO MOD-KVBK-UT                      
202000     MOVE CLAG-FLMANBK                TO MOD-FLMANBK-UT                   
202100     MOVE CLAG-KVPALL                 TO WS-KVPALL                        
202200     MOVE WS-KVPALL                   TO MOD-KVPALL-UT                    
202300     .                                                                    
202400     EJECT                                                                
202500 CAA-OEPPNA-ARTC12-FAELT SECTION.                                         
202600     SKIP2                                                                
202700     MOVE MFS-OEPPNA-NUM-FAELT        TO MOD-IDANSK-ATTR-IN               
202800                                         MOD-IDPLANGR-AG-ATTR-IN          
202900                                         MOD-IDPLANGR-LEV-ATTR-IN         
203000                                         MOD-KVVECKOR-LT-ATTR-IN          
203100                                         MOD-KVVECKOR-AT-ATTR-IN          
203200                                         MOD-KVQ-ATTR-IN                  
203300                                         MOD-KVBK-ATTR-IN                 
203400                                         MOD-KVPALL-ATTR-IN               
203500                                         MOD-KVQ-JUST-ATTR-IN             
203600                                         MOD-TIQJUST-ATTR-IN              
203700                                         MOD-TIREFSTO-ATTR-IN             
203800                                         MOD-KVULOAD-ATTR-IN              
203900     MOVE MFS-OEPPNA-ALFA-FAELT       TO MOD-FLMANLT-ATTR-IN              
204000                                         MOD-FLMANAT-ATTR-IN              
204100                                         MOD-FLMANQ-ATTR-IN               
204200                                         MOD-FLREFILL-ATTR-IN             
204300                                         MOD-FLMANBK-ATTR-IN              
204400     .                                                                    
204500     EJECT                                                                
204600 CB-FLYTTA-ARTC23-C1 SECTION.                                             
204700     SKIP2                                                                
204800     MOVE CLAG-RESLJUST               TO WS-RESLJUST                      
204900     MOVE WS-HELTAL                   TO HELTAL                           
205000     MOVE WS-DECIMAL                  TO DECIMAL                          
205100     MOVE '.'                         TO PUNKT                            
205200     MOVE RESLJUST-WS                 TO MOD-RESLJUST-C1-UT               
205300                                                                          
205400     MOVE CLAG-TISLJUST               TO WS-TISLJUST                      
205500     MOVE WS-TISLJUST                 TO MOD-TISLJUST-C1-UT               
205600     .                                                                    
205700     EJECT                                                                
205800 CBA-OEPPNA-ARTC23-C1 SECTION.                                            
205900     SKIP2                                                                
206000                                                                          
206100     MOVE MFS-OEPPNA-NUM-FAELT  TO MOD-RESLJUST-C1-ATTR-IN                
206200                                   MOD-TISLJUST-C1-ATTR-IN                
206300     .                                                                    
206400     EJECT                                                                
206500 CCB-RENSA-ARTC23-C2 SECTION.                                             
206600     SKIP2                                                                
206700     MOVE MFS-RENSA-FAELT             TO MOD-RESLJUST-C2-UT               
206800                                         MOD-TISLJUST-C2-UT               
206900     .                                                                    
207000     EJECT                                                                
207100 CD-CLOSE-ARTC12-FAELT SECTION.                                           
207200     SKIP2                                                                
207300     MOVE MFS-CLOSE-FIELD             TO MOD-KVBK-ATTR-IN                 
207400                                         MOD-KVPALL-ATTR-IN               
207500                                         MOD-KVQ-JUST-ATTR-IN             
207600                                         MOD-TIQJUST-ATTR-IN              
207700                                         MOD-KVULOAD-ATTR-IN              
207800                                         MOD-FLMANQ-ATTR-IN               
207900                                         MOD-FLMANBK-ATTR-IN              
208000                                         MOD-KVSLAGER-C1-ATTR-IN          
208100                                         MOD-KVSLAGER-C2-ATTR-IN          
208200                                         MOD-KVSPANT-C1-ATTR-IN           
208300                                         MOD-KVSPANT-C2-ATTR-IN           
208400     .                                                                    
208500     EJECT                                                                
208600 CE-GET-MFG-SHIP-DETAILS SECTION.                                         
208700     SKIP2                                                                
208800                                                                          
208900     MOVE W-IDLEVNR                   TO MOD-IDLEVNR-FRAM-UT              
209000     MOVE W-IDLEVNR-SHIP              TO MOD-IDLEVNR-SHIP-FRAM-UT         
209100     MOVE NEJ                         TO WS-EVENT-FOUND                   
209200                                                                          
209300     PERFORM IMS-GN-WDG901                                                
209400     PERFORM UNTIL BASEN-SLUT OR WS-EVENT-FOUND = JA                      
209500        IF BAS-R01-IDPTYP  = 'R01' AND                                    
209600           BAS-R01-IDARTNR = W-IDARTNR-KEY                                
209700            MOVE JA                   TO WS-EVENT-FOUND                   
209800            MOVE BAS-R01-IDLEVNR      TO W-EVNT-IDLEVNR                   
209900            MOVE BAS-R01-IDLEVNR-SHIP TO W-EVNT-IDLEVNR-SHIP              
210000        END-IF                                                            
210100        PERFORM IMS-GN-WDG901                                             
210200     END-PERFORM                                                          
210300     IF WS-EVENT-FOUND = JA                                               
210400        MOVE W-EVNT-IDLEVNR           TO MOD-IDLEVNR-FRAM-UT              
210500        MOVE W-EVNT-IDLEVNR-SHIP      TO MOD-IDLEVNR-SHIP-FRAM-UT         
210600                                      MOD-IDLEVNR-SHIP-FRAM-UT            
210700        MOVE MFS-ADD-HILIGHT-FIELD    TO                                  
210800                                     MOD-IDLEVNR-FRAM-ATTR-UT             
210900                                     MOD-IDLEVNR-SHIP-FRAM-ATTR-UT        
211000     END-IF                                                               
211100     .                                                                    
211200     EJECT                                                                
211300 D-RENSA-FAELT-MOD SECTION.                                               
211400     SKIP2                                                                
211500     MOVE MFS-RENSA-FAELT             TO MOD-IDANSK-UT                    
211600                                         MOD-IDLEVNR-FRAM-UT              
211700                                         MOD-IDLEVNR-SHIP-FRAM-UT         
211800                                         MOD-KVSPANT-C1-UT                
211900                                         MOD-KVSPANT-C2-UT                
212000                                         MOD-IDPLANGR-AG-UT               
212100                                         MOD-IDPLANGR-LEV-UT              
212200                                         MOD-TIREFSTO-UT                  
212300                                         MOD-KVQ-JUST-UT                  
212400                                         MOD-TIQJUST-UT                   
212500                                         MOD-RESLJUST-C1-UT               
212600                                         MOD-RESLJUST-C2-UT               
212700                                         MOD-KVVECKOR-LT-UT               
212800                                         MOD-FLMANLT-UT                   
212900                                         MOD-TISLJUST-C1-UT               
213000                                         MOD-TISLJUST-C2-UT               
213100                                         MOD-KVVECKOR-AT-UT               
213200                                         MOD-FLMANAT-UT                   
213300                                         MOD-KVQ-UT                       
213400                                         MOD-FLMANQ-UT                    
213500                                         MOD-KVULOAD-UT                   
213600                                         MOD-FLREFILL-UT                  
213700                                         MOD-TIREFSTO-UT                  
213800                                         MOD-KVBK-UT                      
213900                                         MOD-FLMANBK-UT                   
214000                                         MOD-KVPALL-UT                    
214100                                         MOD-KVSLAGER-C1-UT               
214200                                         MOD-KVSLAGER-C2-UT               
214300     .                                                                    
214400     EJECT                                                                
214500 S01-CLOSE-SUPPLIER-UPD SECTION.                                          
214600     SKIP2                                                                
214700     MOVE MFS-CLOSE-FIELD-NOMOD       TO                                  
214800                                     MOD-IDLEVNR-FRAM-ATTR-IN             
214900                                     MOD-IDLEVNR-SHIP-FRAM-ATTR-IN        
215000     .                                                                    
215100     EJECT                                                                
215200 SEC-URITY   SECTION.                                                     
215300     SKIP2                                                                
215400*    --- CHECK IF USER IS GRANTED TO SEE PART-INFO                        
215500     PERFORM IMS-GET-WDK601                                               
215600     IF  SEGMENT-FINNS                                                    
215700        MOVE ART-IDLEVNR          TO WS-IDLEVNR-8                         
215800        IF MSGI-KDARBTYP-SEC-IDLEV = WS-IDLEVNR-8                         
215900        OR MSGI-KDARBTYP-SEC-IDLEV = SPACE OR LOW-VALUE                   
216000*          --- OK                                                         
216100           SET PASSED-SECURITY-CHECK TO TRUE                              
216200        ELSE                                                              
216300*          --- USER NOT AUTHORIZED                                        
216400           MOVE FEL-6 (1) TO MOD-MESSAGE-RAD1                             
216500           PERFORM D-RENSA-FAELT-MOD                                      
216600        END-IF                                                            
216700     ELSE                                                                 
216800        MOVE FEL2 TO MOD-MESSAGE-RAD1                                     
216900        PERFORM D-RENSA-FAELT-MOD                                         
217000        PERFORM S01-CLOSE-SUPPLIER-UPD                                    
217100     END-IF                                                               
217200     .                                                                    
217300     EJECT                                                                
217400                                                                          
217500* IMS SEKTIONER                                                           
217600     SKIP1                                                                
217700 IMS-GET-MSG SECTION.                                                     
217800     MOVE '  QC' TO GODK-STATUSKODER                                      
217900     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
218000     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
218100     PERFORM IMS-STATUSKONTROLL                                           
218200     .                                                                    
218300     SKIP3                                                                
218400 IMS-INSERT-MSG SECTION.                                                  
218500     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
218600     MOVE SPACE TO GODK-STATUSKODER                                       
218700     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
218800     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
218900     PERFORM IMS-STATUSKONTROLL                                           
219000     .                                                                    
219100     EJECT                                                                
219200 IMS-GET-LEVA SECTION.                                                    
219300     STRING 'WLLEVA01(IDLEVNR  =' W-IDLEVNR-KEY-X ')'                     
219400            DELIMITED BY SIZE INTO SSA1                                   
219500     MOVE '  GE' TO GODK-STATUSKODER                                      
219600     CALL CBLTDLI USING GU WLLEVA-PCB DLI-IO-AREA2 SSA1                   
219700     MOVE WLLEVA-STATUS-CODE TO STATUS-WS                                 
219800     PERFORM IMS-STATUSKONTROLL                                           
219900     .                                                                    
220000     EJECT                                                                
220100 IMS-ISRT-WLXXBJ SECTION.                                                 
220200     SKIP2                                                                
220300     STRING 'WLXXBJ01(WDG3KEY  =' W-2203-KEY-X ')'                        
220400            DELIMITED BY SIZE INTO SSA1                                   
220500     MOVE 'WLXXBJ11 ' TO SSA2                                             
220600     MOVE '  ' TO GODK-STATUSKODER                                        
220700     CALL CBLTDLI USING ISRT WLXXBJ-PCB DLI-IO-AREA SSA1 SSA2             
220800     MOVE WLXXBJ-STATUS-CODE TO STATUS-WS                                 
220900     PERFORM IMS-STATUSKONTROLL                                           
221000     .                                                                    
221100     SKIP3                                                                
221200 IMS-ISRT-WLXXBI SECTION.                                                 
221300     SKIP2                                                                
221400     STRING 'WLXXBI01(WDG3KEY  =' W-2213-KEY-X ')'                        
221500            DELIMITED BY SIZE INTO SSA1                                   
221600     MOVE 'WLXXBI11 ' TO SSA2                                             
221700     MOVE '  ' TO GODK-STATUSKODER                                        
221800     CALL CBLTDLI USING ISRT WLXXBI-PCB DLI-IO-AREA SSA1 SSA2             
221900     MOVE WLXXBI-STATUS-CODE TO STATUS-WS                                 
222000     PERFORM IMS-STATUSKONTROLL                                           
222100     .                                                                    
222200     EJECT                                                                
222300 IMS-GET-WDK601 SECTION.                                                  
222400     STRING 'WDK601  (IDARTNR  =' W-IDARTNR-KEY-X ')'                     
222500            DELIMITED BY SIZE INTO SSA1                                   
222600     MOVE '  GE' TO GODK-STATUSKODER                                      
222700     CALL CBLTDLI USING GU WDK6-PCB DLI-IO-WDK601 SSA1                    
222800     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
222900     PERFORM IMS-STATUSKONTROLL                                           
223000     .                                                                    
223100     EJECT                                                                
223200 IMS-GHNP-WDK611 SECTION.                                                 
223300     STRING 'WDK611  *F(KDSEGKEY =1)'                                     
223400            DELIMITED BY SIZE INTO SSA1                                   
223500     MOVE '  GE' TO GODK-STATUSKODER                                      
223600     CALL CBLTDLI USING GHNP WDK6-PCB DLI-IO-WDK611 SSA1                  
223700     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
223800     PERFORM IMS-STATUSKONTROLL                                           
223900     .                                                                    
224000     SKIP2                                                                
224100 IMS-GET-WDK611 SECTION.                                                  
224200     MOVE 'WDK611  *F ' TO SSA1                                           
224300     MOVE '  GE' TO GODK-STATUSKODER                                      
224400     CALL CBLTDLI USING GNP WDK6-PCB DLI-IO-WDK611 SSA1                   
224500     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
224600     PERFORM IMS-STATUSKONTROLL                                           
224700     .                                                                    
224800     SKIP2                                                                
224900 IMS-REPL-WDK611 SECTION.                                                 
225000     SKIP2                                                                
225100     MOVE '  ' TO GODK-STATUSKODER                                        
225200     CALL CBLTDLI USING REPL WDK6-PCB DLI-IO-WDK611                       
225300     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
225400     PERFORM IMS-STATUSKONTROLL                                           
225500     .                                                                    
225600     EJECT                                                                
225700 IMS-GET-BENA01 SECTION.                                                  
225800     STRING 'WLBENA01(WDD3BSEQ =' W-IDARTNR-KEY-X ')'                     
225900            DELIMITED BY SIZE INTO SSA1                                   
226000     MOVE '  GE' TO GODK-STATUSKODER                                      
226100     CALL CBLTDLI USING GU WLBENA-PCB DLI-IO-AREA SSA1                    
226200     MOVE WLBENA-STATUS-CODE TO STATUS-WS                                 
226300     PERFORM IMS-STATUSKONTROLL                                           
226400     .                                                                    
226500     SKIP2                                                                
226600 IMS-GET-BENA11 SECTION.                                                  
226700     STRING 'WLBENA11(IDSKYLT  =' W-IDSKYLT-KEY-X ')'                     
226800            DELIMITED BY SIZE INTO SSA1                                   
226900     MOVE '  GE' TO GODK-STATUSKODER                                      
227000     CALL CBLTDLI USING GNP WLBENA-PCB DLI-IO-AREA SSA1                   
227100     MOVE WLBENA-STATUS-CODE TO STATUS-WS                                 
227200     PERFORM IMS-STATUSKONTROLL                                           
227300     .                                                                    
227400     SKIP2                                                                
227500 IMS-ISRT-450511 SECTION.                                                 
227600     STRING 'WL450501(WDGXKEY  =' W-WDGX-4505-KEY-X ')'                   
227700            DELIMITED BY SIZE INTO SSA1                                   
227800     MOVE 'WL450511 '           TO SSA2                                   
227900     MOVE '  ' TO GODK-STATUSKODER                                        
228000     CALL CBLTDLI USING ISRT 4505-PCB DLI-IO-AREA3 SSA1 SSA2              
228100     MOVE 4505-STATUS-CODE TO STATUS-WS                                   
228200     PERFORM IMS-STATUSKONTROLL                                           
228300     .                                                                    
228400     EJECT                                                                
228500                                                                          
228600                                                                          
228700 IMS-GU-ARTS01 SECTION.                                                   
228800     STRING 'WLARTS01(IDARTNR  =' W-IDARTNR-KEY-X ')'                     
228900            DELIMITED BY SIZE INTO SSA1                                   
229000     MOVE '  GE' TO GODK-STATUSKODER                                      
229100     CALL CBLTDLI USING GU ARTS-PCB DLI-IO-AREA4 SSA1                     
229200     MOVE ARTS-STATUS-CODE TO STATUS-WS                                   
229300     PERFORM IMS-STATUSKONTROLL                                           
229400     .                                                                    
229500     EJECT                                                                
229600                                                                          
229700                                                                          
229800 IMS-GHNP-ARTS11 SECTION.                                                 
229900     MOVE  'WLARTS11 ' TO SSA1                                            
230000     MOVE '  GEGB' TO GODK-STATUSKODER                                    
230100     CALL CBLTDLI USING GHNP ARTS-PCB DLI-IO-AREA4 SSA1                   
230200     MOVE ARTS-STATUS-CODE TO STATUS-WS                                   
230300     PERFORM IMS-STATUSKONTROLL                                           
230400     .                                                                    
230500     EJECT                                                                
230600                                                                          
230700                                                                          
230800 IMS-REPL-ARTS11 SECTION.                                                 
230900                                                                          
231000     MOVE '  ' TO GODK-STATUSKODER                                        
231100     CALL CBLTDLI USING REPL ARTS-PCB DLI-IO-AREA4                        
231200     MOVE ARTS-STATUS-CODE TO STATUS-WS                                   
231300     PERFORM IMS-STATUSKONTROLL                                           
231400     .                                                                    
231500     EJECT                                                                
231501                                                                          
231510 IMS-GU-WDK711-REF SECTION.                                               
231550     STRING 'WLARTS01(IDARTNR  =' W-IDARTNR-KEY-X ')'                     
231560          DELIMITED BY SIZE INTO SSA1                                     
231570     STRING 'WLARTS11(IDDC     =' W-IDDC-REF-X ')'                        
231580          DELIMITED BY SIZE INTO SSA2                                     
231590     MOVE '  GE' TO GODK-STATUSKODER                                      
231591     CALL CBLTDLI USING GU ARTS-PCB DLI-IO-K711-REF SSA1 SSA2             
231592     MOVE ARTS-STATUS-CODE TO STATUS-WS                                   
231593     PERFORM IMS-STATUSKONTROLL                                           
231594     .                                                                    
231595     EJECT                                                                
231600                                                                          
231700 IMS-GU-WDB601    SECTION.                                                
231800     STRING 'WDB601  (IDDC     =' W-IDDC-B6-X ')'                         
231900          DELIMITED BY SIZE INTO SSA1                                     
232000     MOVE '  ' TO GODK-STATUSKODER                                        
232100     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-AREA-B601 SSA1                 
232200     MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
232300     PERFORM IMS-STATUSKONTROLL                                           
232400     .                                                                    
232500     EJECT                                                                
232600 IMS-GN-WDG901 SECTION.                                                   
232700     MOVE   'WDG901   ' TO SSA1                                           
232800     MOVE '  GB' TO GODK-STATUSKODER                                      
232900     CALL CBLTDLI USING GN WDG9-PCB DLI-IO-WDG901 SSA1                    
233000     MOVE WDG9-STATUS-CODE TO STATUS-WS                                   
233100     PERFORM IMS-STATUSKONTROLL                                           
233200     .                                                                    
233300     SKIP3                                                                
233400     EJECT                                                                
233500 IMS-GU-WDB601-LEVNR SECTION.                                             
233600     STRING 'WDB601  (IDLEVNDC =' W-IDLEVNR ')'                           
233700          DELIMITED BY SIZE INTO SSA1                                     
233800     MOVE '  GE' TO GODK-STATUSKODER                                      
233900     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-AREA-B601 SSA1                 
234000     MOVE WDB6-STATUS-CODE TO STATUS-WS                                   
234100     PERFORM IMS-STATUSKONTROLL                                           
234200     .                                                                    
234300     SKIP3                                                                
234400 IMS-GU-WDB615 SECTION.                                                   
234500                                                                          
234600     STRING 'WDB601  (IDDC     =' W-IDDC-B6-X ')'                         
234700          DELIMITED BY SIZE INTO SSA1                                     
234800     STRING 'WDB615  (WDB615KY =' W-WDB615KY-X ')'                        
234900          DELIMITED BY SIZE INTO SSA2                                     
235000     MOVE '  GE' TO GODK-STATUSKODER                                      
235100     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-WDB615 SSA1 SSA2               
235200     MOVE WDB6-STATUS-CODE TO STATUS-WS                                   
235300     PERFORM IMS-STATUSKONTROLL                                           
235400     .                                                                    
235500     SKIP3                                                                
235600 IMS-GU-WDB616 SECTION.                                                   
235700                                                                          
235800     STRING 'WDB601  (IDDC     =' W-IDDC-B6-X ')'                         
235900          DELIMITED BY SIZE INTO SSA1                                     
236000     STRING 'WDB616  (IDDCREF  =' W-IDDC-REF-X ')'                        
236100          DELIMITED BY SIZE INTO SSA2                                     
236200     MOVE '  GE' TO GODK-STATUSKODER                                      
236300     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-WDB616 SSA1 SSA2               
236400     MOVE WDB6-STATUS-CODE TO STATUS-WS                                   
236500     PERFORM IMS-STATUSKONTROLL                                           
236600     .                                                                    
236700     EJECT                                                                
236800 IMS-ISRT-WDG901 SECTION.                                                 
236900                                                                          
237000     MOVE 'WDG901   ' TO SSA1                                             
237100     MOVE '  '   TO GODK-STATUSKODER                                      
237200     CALL CBLTDLI USING ISRT WDG9-PCB DLI-IO-WDG901 SSA1                  
237300     MOVE WDG9-STATUS-CODE TO STATUS-WS                                   
237400     PERFORM IMS-STATUSKONTROLL                                           
237500     .                                                                    
237600     EJECT                                                                
237700 IMS-GHN-WDG901  SECTION.                                                 
237800     MOVE 'WDG901   '            TO SSA1                                  
237900     MOVE '  GEGB'               TO GODK-STATUSKODER                      
238000     CALL CBLTDLI USING GHN WDG9-PCB DLI-IO-WDG901 SSA1                   
238100     MOVE WDG9-STATUS-CODE       TO STATUS-WS                             
238200     PERFORM IMS-STATUSKONTROLL                                           
238300     .                                                                    
238400     EJECT                                                                
238500 IMS-DLET-WDG901 SECTION.                                                 
238600     MOVE '  '   TO GODK-STATUSKODER                                      
238700     CALL CBLTDLI USING DLET WDG9-PCB DLI-IO-WDG901                       
238800     MOVE WDG9-STATUS-CODE TO STATUS-WS                                   
238900     PERFORM IMS-STATUSKONTROLL                                           
239000     .                                                                    
239100     EJECT                                                                
239200 IMS-STATUSKONTROLL SECTION.                                              
239300     SET STATUS-IX TO 1                                                   
239400     SEARCH GODK-STATUS                                                   
239500       AT END                                                             
239600         CALL FELLOG                                                      
239700     WHEN GODK-STATUS (STATUS-IX) = STATUS-WS CONTINUE                    
239800     END-SEARCH                                                           
239900     .                                                                    
