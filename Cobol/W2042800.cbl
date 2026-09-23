000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W2042800.                                                
000300 AUTHOR.         ARCHANA BHAT.                                            
000400 DATE-WRITTEN.   12/11/28.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*                                                                         
000800*    FUNCTION:                                                            
000900*        SCREEN SHOWING THE CALCULATED DEMAND PER WEEK FOR THE            
001000*        REQUESTED PART. KEYS FOR ENTERING THE SCREEN ARE                 
001100*        PARTNUMBER ID AND  DC-ID.                                        
001200*                                                                         
001300*        THE PROGRAM READS     WDK6                                       
001400*        THE PROGRAM READS     WDK7                                       
001500*        THE PROGRAM READS     WDK9                                       
001600*        THE PROGRAM READS     WDR2(WDGX2502)                             
001700*                                                                         
001800*    INDATA.                                                              
001900*        TRANSACTION: W2T428                                              
002000*        MID:         W2I42801                                            
002100*                                                                         
002200*    OUTDATA.                                                             
002300*        MOD:         W2O428N1                                            
002400*                                                                         
002500*----------------------------------------------------------               
002600* PROGRAMÄNDRINGAR:                                                       
002700* 2015-07-06  ETRACKER 10209749    (WDD903)                               
002800*             ÄNDRA 2103/2403 ORSAK EXTRALEVERANSER OCH MERA.             
002900*                                                                         
003000* 2015-11-12  E'TRACKER 10243132  KINA EXPORT 2015                        
003100*                                                                         
003200* 2017-08-04  E'TRACKER 10299286  KINA LOCAL SOURCING USA.                
003300*                                 GEMENSAMT CCID:10302687 REFILL          
003400*                                                                         
003500                                                                          
003600     SKIP3                                                                
003700 ENVIRONMENT DIVISION.                                                    
003800                                                                          
003900 DATA DIVISION.                                                           
004000     EJECT                                                                
004100 WORKING-STORAGE SECTION.                                                 
004200*    -- CHECKED BY WY2000                                                 
004300     SKIP3                                                                
004400*    -COPY WY2000W3                                                       
004500 77  IDPGM                       PIC X(08)   VALUE 'W2042800'.            
004600                                                                          
004700*    --- WORK FIELDS FOR ERROR MESSAGES WHEN CALLING ABEND/FELLOG         
004800 77  ERROR-TEXT                  PIC X(80) VALUE SPACE.                   
004900                                                                          
005000 77  YES                         PIC X       VALUE 'J'.                   
005100 77  NOO                         PIC X       VALUE 'N'.                   
005200 77  CURRENT-SECTION             PIC X(30)   VALUE SPACE.                 
005300 77  DBS-SECTION                 PIC X(30)   VALUE SPACE.                 
005400                                                                          
005500*    --- WORK FIELDS FOR ACTUAL KEYVALUES OF SCREEN                       
005600                                                                          
005700                                                                          
005800 77  KEYS-SW                     PIC X       VALUE 'J'.                   
005900     88  KEYS-OK                             VALUE 'J'.                   
006000     88  KEYS-WRONG                          VALUE 'N'.                   
006100                                                                          
006200 77  INDATA-SW                   PIC X       VALUE 'J'.                   
006300     88  INDATA-OK                           VALUE 'J'.                   
006400     88  INDATA-FEL                          VALUE 'N'.                   
006500                                                                          
006600 77  WS-WDK711-SW                PIC X       VALUE 'N'.                   
006700     88  WS-WDK711-OK                        VALUE 'J'.                   
006800     88  WS-WDK711-FEL                       VALUE 'N'.                   
006900 77  DC-SW                       PIC X       VALUE 'J'.                   
007000     88  DC-OK                               VALUE 'J'.                   
007100     88  DC-WRONG                            VALUE 'N'.                   
007200                                                                          
007300 77  SECURITY-SW                 PIC X       VALUE 'N'.                   
007400     88  PASSED-SEC-CHECK                    VALUE 'J'.                   
007500                                                                          
007600 77  W-IDTRANS                   PIC X(4)    VALUE SPACE.                 
007700     88  OWN-MID                             VALUE '2428'.                
007800     88  GOOD-MID                            VALUE '2421' '2422'          
007900                                                   '2423' '2424'          
008000                                                   '2425' '2426'          
008100                                                   '2427' '2428'          
008200                                                   '2429'.                
008300     88  HELP-MID                            VALUE '0551'.                
008400     EJECT                                                                
008500*    --- WORKING STORAGE VARIABLES                                        
008600 01  WS-VARIABLES.                                                        
008700     03  WS-IDARTNR              PIC X(9)    VALUE SPACES.                
008800     03  WS-IDLEVNR-8            PIC X(8)    VALUE SPACES.                
008900     03  IDLEVNR-WS              PIC X(5)    VALUE SPACES.                
009000     03  WS-DASH                 PIC X(1)    VALUE '-'.                   
009100     03  IX                      PIC 9(3)    VALUE ZERO.                  
009200     03  MAX-IX                  PIC 9(3)    VALUE 156.                   
009300     03  INDX                    PIC 9(2)    VALUE ZERO.                  
009400     03  INDX1                   PIC 9(2)    VALUE ZERO.                  
009500     03  INDX-T                  PIC 9(2)    VALUE ZERO.                  
009600     03  MAX-INDX                PIC 9(2)    VALUE 14.                    
009700     03  MAX-KVVECKOR-TREND      PIC S9(3)   VALUE ZERO.                  
009800     03  W-ANTAL-VECKOR          PIC 9(3)    VALUE ZERO COMP-3.           
009900     03  W-TIAAVV-NUM            PIC 9(4)    VALUE ZERO.                  
010000     03  W-TIAAVV                PIC S9(5)   VALUE ZERO  COMP-3.          
010100     03  W-TIME                  PIC 9(8)    VALUE ZERO.                  
010200     03  W-VECKO-SEP-BEHOV  PIC  S9(7)V9(2)  VALUE ZERO  COMP-3.          
010300     03  W-DAG-SEP-BEHOV    PIC  S9(7)V9(2)  VALUE ZERO  COMP-3.          
010400     03  W-TIFINLV               PIC 9(8)    VALUE ZERO.                  
010500     03  FILLER REDEFINES W-TIFINLV.                                      
010600         05  W-TIFINLV-SS        PIC 9(2).                                
010700         05  W-TIFINLV-AAMMDD    PIC 9(6).                                
010800     03  W-KVDAGAR-KVAR          PIC 9(3)    VALUE ZERO COMP-3.           
010900     03  WS-KVPB-TREND-RAD1      PIC S9(6)V9(1) VALUE ZERO COMP-3.        
011000     03  WS-KVPB-TREND           PIC S9(6)V9.                             
011100     03  WS-KVPB-PLAN-WEEK       PIC S9(5)V9.                             
011200     03  SPAR-KVPB-TOT           PIC S9(8)V9(2) VALUE ZERO COMP-3.        
011300     03  SPAR-KVPB-REF           PIC S9(6)V9(1) VALUE ZERO COMP-3.        
011400     03  WS-KVPB-REF-RAD1        PIC S9(7)V9(2) VALUE ZERO COMP-3.        
011500                                                                          
011600     03  W-KVPB-TOT-TAB.                                                  
011700         05 W-KVPB-TOT           PIC S9(8)V9(2) COMP-3                    
011800                                             OCCURS 14.                   
011900                                                                          
012000     03  ENDAST-SEPARATBEHOV     PIC X(2)    VALUE '01'.                  
012100     03  ENDAST-XDCBEHOV         PIC X(2)    VALUE '02'.                  
012200     03  PB-TOTAL-SEP-LEV-XDC    PIC X(2)    VALUE '03'.                  
012300     03  ENDAST-CDCBEHOV         PIC X(2)    VALUE '04'.                  
012400     03  ENDAST-GLOBALBEHOV      PIC X(2)    VALUE '06'.                  
012500     03  ENDAST-LOKALBEHOV       PIC X(2)    VALUE '07'.                  
012600                                                                          
012700     03 WS-NOLLA-KVBEHOV-BHDC.                                            
012800        05 FILLER                   OCCURS 156                            
012900                              PIC S9(7)V9(2) COMP-3  VALUE ZERO.          
013000                                                                          
013100     03  WS-CURRENT-DATE.                                                 
013200         05  WS-DAGENS-TIAAAA    PIC 9(4)    VALUE ZERO.                  
013300         05  FILLER              PIC 9(4)    VALUE ZERO.                  
013400         05  FILLER              PIC 9(6)    VALUE ZERO.                  
013500                                                                          
013600     03  FILLER REDEFINES WS-CURRENT-DATE.                                
013700         05  WS-DAGENS-DATE      PIC 9(8).                                
013800         05  WS-DAGENS-TID.                                               
013900             07 WS-DAGENS-TIME   PIC 9(2).                                
014000             07 WS-DAGENS-MINUTE PIC 9(2).                                
014100             07 WS-DAGENS-SECOND PIC 9(2).                                
014200     EJECT                                                                
014300*    --- VALID IDDC CODES                                                 
014400*                                                                         
014500*01  -COPY WWDC99                                                         
014600     SKIP3                                                                
014700*    --- SUBPROGRAMS AND PARAMETER AREAS                                  
014800 01  GENERAL-SUBPROGRAMS.                                                 
014900     03  WMEDKONV                PIC X(8)    VALUE 'WMEDKONV'.            
015000     03  W005INIT                PIC X(8)    VALUE 'W005INIT'.            
015100     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
015200     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
015300     03  WDATKONV                PIC X(8)    VALUE 'WDATKONV'.            
015400     03  W009VADD                PIC X(8)    VALUE 'W009VADD'.            
015500     03  W222BHDC                PIC X(8)    VALUE 'W222BHDC'.            
015600     03  W222TILG                PIC X(8)    VALUE 'W222TILG'.            
015700     EJECT                                                                
015800*    --- PARAMETERS FOR SUBPROGRAM WMEDKONV                               
015900*01 -COPY WMEDAREA                                                        
016000     SKIP3                                                                
016100 01  MESSAGE-CODES.                                                       
016200     03  ERR-KEY-MISSING         PIC X(3)    VALUE '005'.                 
016300     03  ERR-PART-MISSING        PIC X(3)    VALUE '017'.                 
016400     03  ERR-WRONG-KEY           PIC X(3)    VALUE '401'.                 
016500     03  ERR-USER-NOT-AUTH       PIC X(3)    VALUE '405'.                 
016600     03  ERR-REFILL-PART         PIC X(3)    VALUE '434'.                 
016700     03  ERR-DC-INVALID          PIC X(3)    VALUE '440'.                 
016800     EJECT                                                                
016900*01  -COPY WDATAREA                                                       
017000     EJECT                                                                
017100* VARIABLES TO SUBPROGRAM W009VADD                                        
017200 01  DATUM-AAVV                  PIC S9(5)  COMP-3.                       
017300 01  ANTAL-VECKOR                PIC S9(3)  COMP-3.                       
017400     EJECT                                                                
017500*    --- PARAMETERS FOR SUB PROGRAM W005INIT                              
017600*                                                                         
017700 01  FILLER                      PIC X(16)   VALUE 'WMSGINIT'.            
017800     SKIP3                                                                
017900*01 -COPY WMSGINIT                                                        
018000     EJECT                                                                
018100*    --- PARAMETERS FOR SUB PROGRAM W222BHDC                              
018200 01  FILLER                      PIC X(16)   VALUE 'W222BHDC'.            
018300     SKIP3                                                                
018400*01  AREA  -COPY W222BHDC -PRE LINK-.                                     
018500     EJECT                                                                
018600*    --- PARAMETERS FOR SUB PROGRAM W222TILG                              
018700 01  FILLER                      PIC X(16)   VALUE 'W222TILG'.            
018800     SKIP3                                                                
018900*01  -COPY W222TILG                                                       
019000     EJECT                                                                
019100*    --- AREAS FOR MFS AND SCREEN MANAGEMENT                              
019200*                                                                         
019300 01  FILLER                      PIC X(16)   VALUE 'MID-AREA'.            
019400     SKIP3                                                                
019500*01  MID -COPY W2I42801                                                   
019600     EJECT                                                                
019700 01  FILLER                      PIC X(16)  VALUE 'MSG/MOD-AREA'.         
019800     SKIP3                                                                
019900*01  -COPY WMSGAREA                                                       
020000     EJECT                                                                
020100     03  MOD REDEFINES MSG-AREA.                                          
020200*      05  -COPY W2O42801                                                 
020300     EJECT                                                                
020400 01  FILLER                      PIC X(16)   VALUE 'MFS-AREA'.            
020500     SKIP3                                                                
020600*01  -COPY WMFSAREA                                                       
020700     EJECT                                                                
020800*    --- WORK-AREAS FOR IMS-SECTIONS                                      
020900*                                                                         
021000 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
021100     SKIP3                                                                
021200 01  KEYS-FOR-DLI.                                                        
021300     03  W-IDARTNR-X.                                                     
021400         05  W-IDARTNR           PIC S9(9)   VALUE ZERO COMP-3.           
021500     03  W-IDDC-X.                                                        
021600         05  W-IDDC              PIC X(2)    VALUE SPACE.                 
021700     03  W-IDDC-MIN-X.                                                    
021800         05  W-IDDC-MIN          PIC X(2)    VALUE SPACE.                 
021900     03  W-IDDC-MAX-X.                                                    
022000         05  W-IDDC-MAX          PIC X(2)    VALUE SPACE.                 
022100                                                                          
022200     03  W-IDDC-NON-X.                                                    
022300         05  W-IDDC-NON          PIC X(1)    VALUE SPACE.                 
022400                                                                          
022500     03  W-IDDC1-X.                                                       
022600         05  W-IDDC1             PIC X(1)    VALUE SPACE.                 
022700                                                                          
022800     03  W-IDDC-REF-X.                                                    
022900         05  W-IDDC-REF          PIC X(2)    VALUE SPACE.                 
023000                                                                          
023100     03  W-KDSEGKEY-K722-X.                                               
023200         05  W-KDSEGKEY          PIC X(1)    VALUE '1'.                   
023300     03  W-IDLAND-X.                                                      
023400         05  W-IDLAND            PIC X(2)    VALUE SPACE.                 
023500     SKIP2                                                                
023600*    --- STATUS CODES FROM IMS                                            
023700 01  STATUS-WS                   PIC XX.                                  
023800     88  SEGMENT-FOUND                       VALUE '  '.                  
023900     88  SEGMENT-FOUND-EXISTS                VALUE 'II'.                  
024000     88  SEGMENT-MISSING                     VALUE 'GE'.                  
024100     88  SEGMENT-END                         VALUE 'GB'.                  
024200     SKIP2                                                                
024300 01  GOOD-STATUSCODES.                                                    
024400     03  GOOD-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
024500     SKIP3                                                                
024600 01  SSA1                        PIC X(64).                               
024700 01  SSA2                        PIC X(64).                               
024800 01  SSA3                        PIC X(64).                               
024900     EJECT                                                                
025000*    --- IMS FUNCTION CODES                                               
025100*01  -COPY W0003                                                          
025200     EJECT                                                                
025300*    ---  DLI INPUT-OUTPUT AREA                                           
025400                                                                          
025500 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK601'.                      
025600 01  DLI-IO-WDK601.                                                       
025700*    03  -COPY WDK601                                                     
025800     EJECT                                                                
025900 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK611'.                      
026000 01  DLI-IO-WDK611.                                                       
026100*    03  -COPY WDK611                                                     
026200     EJECT                                                                
026300 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK701'.                      
026400 01  DLI-IO-WDK701.                                                       
026500*    03  -COPY WDK701                                                     
026600     EJECT                                                                
026700 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK711'.                      
026800 01  DLI-IO-WDK711.                                                       
026900*    03  -COPY WDK711                                                     
027000     EJECT                                                                
027100 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK712'.                      
027200 01  DLI-IO-WDK712.                                                       
027300*    03  -COPY WDK712                                                     
027400     EJECT                                                                
027500 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK722'.                      
027600 01  DLI-IO-WDK722.                                                       
027700*    03  -COPY WDK722                                                     
027800 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDB601'.                      
027900 01  DLI-IO-WDB601.                                                       
028000*    03  -COPY WDB601                                                     
028100     EJECT                                                                
028200 LINKAGE SECTION.                                                         
028300*01  -COPY W0009   -PRE MSG-                                              
028400*01  -COPY W0008   -PRE WDP7-                                             
028500     05  FILLER                   PIC X.                                  
028600                                                                          
028700*01  -COPY W0008  -PRE WDK6-                                              
028800     05  FILLER                   PIC X.                                  
028900                                                                          
029000*01  -COPY W0008  -PRE WDK7-                                              
029100     05  FILLER                   PIC X.                                  
029200                                                                          
029300*01  -COPY W0008  -PRE WDB6-                                              
029400     05  FILLER                   PIC X.                                  
029500                                                                          
029600 01  BHDC-WDK6-PCB                PIC X.                                  
029700 01  BHDC-WDK7-1-PCB              PIC X.                                  
029800 01  BHDC-WDB6-PCB                PIC X.                                  
029900 01  BHDC-WDR2-PCB                PIC X.                                  
030000 01  BHDC-WDD7-PCB                PIC X.                                  
030100 01  BHDC-WDK7-2-PCB              PIC X.                                  
030200 01  BHDC-WDD7-2-PCB              PIC X.                                  
030300 01  BHDC-WDK9-PCB                PIC X.                                  
030400 01  BHDC-REFL1-2501-PCB          PIC X.                                  
030500 01  BHDC-REFL1-WDB6-PCB          PIC X.                                  
030600 01  BHDC-REFL1-WDK7-PCB          PIC X.                                  
030700 01  BHDC-REFL1-UTIL-WDK6-PCB     PIC X.                                  
030800 01  BHDC-REFL1-UTIL-WDK7-PCB     PIC X.                                  
030900 01  BHDC-REFL1-UTIL-WDB6-PCB     PIC X.                                  
031000     EJECT                                                                
031100 01  BHDC-REFL2-2501-PCB          PIC X.                                  
031200 01  BHDC-REFL2-WDB6-PCB          PIC X.                                  
031300 01  BHDC-REFL2-UTIL-WDK6-PCB     PIC X.                                  
031400 01  BHDC-REFL2-UTIL-WDK7-PCB     PIC X.                                  
031500 01  BHDC-REFL2-UTIL-WDB6-PCB     PIC X.                                  
031600     EJECT                                                                
031700 01  BHDC-UTIL-WDK6-PCB           PIC X.                                  
031800 01  BHDC-UTIL-WDK7-PCB           PIC X.                                  
031900 01  BHDC-UTIL-WDB6-PCB           PIC X.                                  
032000     EJECT                                                                
032100 01  BHDC-W222-WDK6-PCB           PIC X.                                  
032200 01  BHDC-W222-WDK7-PCB           PIC X.                                  
032300 01  BHDC-W222-ARTM-PCB           PIC X.                                  
032400 01  BHDC-W222-2501-PCB           PIC X.                                  
032500 01  BHDC-W222-WDB6R-PCB          PIC X.                                  
032600 01  BHDC-W222-WDK7R-PCB          PIC X.                                  
032700 01  BHDC-W222-WDB6-PCB           PIC X.                                  
032800 01  BHDC-W222-WDD7-PCB           PIC X.                                  
032900 01  BHDC-W222-WDK7E-PCB          PIC X.                                  
033000 01  BHDC-W222-UTIL-WDK6-PCB      PIC X.                                  
033100 01  BHDC-W222-UTIL-WDK7-PCB      PIC X.                                  
033200 01  BHDC-W222-UTIL-WDB6-PCB      PIC X.                                  
033300 01  BHDC-W222-UTUP-WDK7-PCB      PIC X.                                  
033400 01  BHDC-W222-UTUP-WDB6-PCB      PIC X.                                  
033500 01  BHDC-W222-UTUP-UTIL-WDK6-PCB PIC X.                                  
033600 01  BHDC-W222-UTUP-UTIL-WDK7-PCB PIC X.                                  
033700 01  BHDC-W222-UTUP-UTIL-WDB6-PCB PIC X.                                  
033800     EJECT                                                                
033900 01  BHDC-UTUP-WDK7-PCB           PIC X.                                  
034000 01  BHDC-UTUP-WDB6-PCB           PIC X.                                  
034100 01  BHDC-UTUP-UTIL-WDK6-PCB      PIC X.                                  
034200 01  BHDC-UTUP-UTIL-WDK7-PCB      PIC X.                                  
034300 01  BHDC-UTUP-UTIL-WDB6-PCB      PIC X.                                  
034400     EJECT                                                                
034500 01  TILG-WDK7-PCB                PIC X.                                  
034600 01  TILG-WDL2-PCB                PIC X.                                  
034700 01  TILG-WDB6-PCB                PIC X.                                  
034800 01  TILG-WDD9-PCB                PIC X.                                  
034900 01  TILG-WDK6-PCB                PIC X.                                  
035000 01  TILG-WDK9-PCB                PIC X.                                  
035100     EJECT                                                                
035200 PROCEDURE DIVISION  USING MSG-PCB WDP7-PCB WDK6-PCB WDK7-PCB             
035300                                   WDB6-PCB                               
035500                                   BHDC-WDK6-PCB                          
035600                                   BHDC-WDK7-1-PCB                        
035700                                   BHDC-WDB6-PCB                          
035800                                   BHDC-WDR2-PCB                          
035900                                   BHDC-WDD7-PCB                          
036000                                   BHDC-WDK7-2-PCB                        
036100                                   BHDC-WDD7-2-PCB                        
036200                                   BHDC-WDK9-PCB                          
036300                                   BHDC-REFL1-2501-PCB                    
036400                                   BHDC-REFL1-WDB6-PCB                    
036500                                   BHDC-REFL1-WDK7-PCB                    
036600                                   BHDC-REFL1-UTIL-WDK6-PCB               
036700                                   BHDC-REFL1-UTIL-WDK7-PCB               
036800                                   BHDC-REFL1-UTIL-WDB6-PCB               
036900                                   BHDC-REFL2-2501-PCB                    
037000                                   BHDC-REFL2-WDB6-PCB                    
037100                                   BHDC-REFL2-UTIL-WDK6-PCB               
037200                                   BHDC-REFL2-UTIL-WDK7-PCB               
037300                                   BHDC-REFL2-UTIL-WDB6-PCB               
037400                                   BHDC-UTIL-WDK6-PCB                     
037500                                   BHDC-UTIL-WDK7-PCB                     
037600                                   BHDC-UTIL-WDB6-PCB                     
037700                                   BHDC-W222-WDK6-PCB                     
037800                                   BHDC-W222-WDK7-PCB                     
037900                                   BHDC-W222-ARTM-PCB                     
038000                                   BHDC-W222-2501-PCB                     
038100                                   BHDC-W222-WDB6R-PCB                    
038200                                   BHDC-W222-WDK7R-PCB                    
038300                                   BHDC-W222-WDB6-PCB                     
038400                                   BHDC-W222-WDD7-PCB                     
038500                                   BHDC-W222-WDK7E-PCB                    
038600                                   BHDC-W222-UTIL-WDK6-PCB                
038700                                   BHDC-W222-UTIL-WDK7-PCB                
038800                                   BHDC-W222-UTIL-WDB6-PCB                
038900                                   BHDC-W222-UTUP-WDK7-PCB                
039000                                   BHDC-W222-UTUP-WDB6-PCB                
039100                                   BHDC-W222-UTUP-UTIL-WDK6-PCB           
039200                                   BHDC-W222-UTUP-UTIL-WDK7-PCB           
039300                                   BHDC-W222-UTUP-UTIL-WDB6-PCB           
039400                                   BHDC-UTUP-WDK7-PCB                     
039500                                   BHDC-UTUP-WDB6-PCB                     
039600                                   BHDC-UTUP-UTIL-WDK6-PCB                
039700                                   BHDC-UTUP-UTIL-WDK7-PCB                
039800                                   BHDC-UTUP-UTIL-WDB6-PCB                
039900                                   TILG-WDK7-PCB                          
040000                                   TILG-WDL2-PCB                          
040100                                   TILG-WDB6-PCB                          
040200                                   TILG-WDD9-PCB                          
040300                                   TILG-WDK6-PCB                          
040400                                   TILG-WDK9-PCB                          
040500                                   .                                      
040600                                                                          
040700 MAIN SECTION.                                                            
040800     ENTRY 'DLITCBL' USING MSG-PCB WDP7-PCB WDK6-PCB WDK7-PCB             
040900                                   WDB6-PCB                               
041100                                   BHDC-WDK6-PCB                          
041200                                   BHDC-WDK7-1-PCB                        
041300                                   BHDC-WDB6-PCB                          
041400                                   BHDC-WDR2-PCB                          
041500                                   BHDC-WDD7-PCB                          
041600                                   BHDC-WDK7-2-PCB                        
041700                                   BHDC-WDD7-2-PCB                        
041800                                   BHDC-WDK9-PCB                          
041900                                   BHDC-REFL1-2501-PCB                    
042000                                   BHDC-REFL1-WDB6-PCB                    
042100                                   BHDC-REFL1-WDK7-PCB                    
042200                                   BHDC-REFL1-UTIL-WDK6-PCB               
042300                                   BHDC-REFL1-UTIL-WDK7-PCB               
042400                                   BHDC-REFL1-UTIL-WDB6-PCB               
042500                                   BHDC-REFL2-2501-PCB                    
042600                                   BHDC-REFL2-WDB6-PCB                    
042700                                   BHDC-REFL2-UTIL-WDK6-PCB               
042800                                   BHDC-REFL2-UTIL-WDK7-PCB               
042900                                   BHDC-REFL2-UTIL-WDB6-PCB               
043000                                   BHDC-UTIL-WDK6-PCB                     
043100                                   BHDC-UTIL-WDK7-PCB                     
043200                                   BHDC-UTIL-WDB6-PCB                     
043300                                   BHDC-W222-WDK6-PCB                     
043400                                   BHDC-W222-WDK7-PCB                     
043500                                   BHDC-W222-ARTM-PCB                     
043600                                   BHDC-W222-2501-PCB                     
043700                                   BHDC-W222-WDB6R-PCB                    
043800                                   BHDC-W222-WDK7R-PCB                    
043900                                   BHDC-W222-WDB6-PCB                     
044000                                   BHDC-W222-WDD7-PCB                     
044100                                   BHDC-W222-WDK7E-PCB                    
044200                                   BHDC-W222-UTIL-WDK6-PCB                
044300                                   BHDC-W222-UTIL-WDK7-PCB                
044400                                   BHDC-W222-UTIL-WDB6-PCB                
044500                                   BHDC-W222-UTUP-WDK7-PCB                
044600                                   BHDC-W222-UTUP-WDB6-PCB                
044700                                   BHDC-W222-UTUP-UTIL-WDK6-PCB           
044800                                   BHDC-W222-UTUP-UTIL-WDK7-PCB           
044900                                   BHDC-W222-UTUP-UTIL-WDB6-PCB           
045000                                   BHDC-UTUP-WDK7-PCB                     
045100                                   BHDC-UTUP-WDB6-PCB                     
045200                                   BHDC-UTUP-UTIL-WDK6-PCB                
045300                                   BHDC-UTUP-UTIL-WDK7-PCB                
045400                                   BHDC-UTUP-UTIL-WDB6-PCB                
045500                                   TILG-WDK7-PCB                          
045600                                   TILG-WDL2-PCB                          
045700                                   TILG-WDB6-PCB                          
045800                                   TILG-WDD9-PCB                          
045900                                   TILG-WDK6-PCB                          
046000                                   TILG-WDK9-PCB                          
046100                                   .                                      
046200                                                                          
046300     PERFORM IMS-GET-MSG                                                  
046400     IF SEGMENT-FOUND                                                     
046500       PERFORM A-INIT                                                     
046600       PERFORM B-CHECK-KEYS                                               
046700       IF KEYS-OK AND INDATA-OK                                           
046800         PERFORM S10-AUTH-USER-CHECK                                      
046900         IF PASSED-SEC-CHECK AND INDATA-OK                                
047000            PERFORM F-READ-SHOW-INFO                                      
047100*---                                                                      
047200*---        SAVE MFG SUPPLIER USING INIT-IO-AREA                          
047300            IF IDLEVNR-WS > SPACES                                        
047400               MOVE ALL '+'           TO MSGI-WMSGINIT                    
047500               MOVE '001'             TO MSGI-KDCALL                      
047600               MOVE MSG-LTERM-NAME    TO MSGI-IDLTERM-USER                
047700               MOVE MSG-SIGNON-USERID TO MSGI-IDUSER                      
047800               MOVE '2428'            TO MSGI-IDTRANS                     
047900               MOVE IDLEVNR-WS        TO MSGI-IDLEVNR                     
048000                                                                          
048100               CALL W005INIT USING MSGI-WMSGINIT WDP7-PCB                 
048200            END-IF                                                        
048300         END-IF                                                           
048400       END-IF                                                             
048500       COMPUTE MSG-KVLL = LENGTH OF MOD-W2O42801 + 4                      
048600       PERFORM IMS-INSERT-MSG                                             
048700     END-IF                                                               
048800                                                                          
048900     MOVE ZERO TO RETURN-CODE                                             
049000     GOBACK                                                               
049100     .                                                                    
049200     EJECT                                                                
049300 A-INIT SECTION.                                                          
049400                                                                          
049500     IF MSG-DOUBLE-TRANSACTIONS                                           
049600       MOVE MSG-INDATA-MINUS-2-TRANSACT   TO MID-W2I42801                 
049700       MOVE MSG-IDTRANS-2  TO MFS-IDTRANS                                 
049800       MOVE MSG-KDMFSFOR-2 TO MFS-KDMFSFOR                                
049900     ELSE                                                                 
050000       MOVE MSG-INDATA-MINUS-1-TRANSACT  TO MID-W2I42801                  
050100       MOVE MSG-IDTRANS-1  TO MFS-IDTRANS                                 
050200       MOVE MSG-KDMFSFOR-1 TO MFS-KDMFSFOR                                
050300     END-IF                                                               
050400                                                                          
050500     MOVE MSG-KDTRTYP TO MFS-KDTRTYP                                      
050600     MOVE MSG-IDPFK   TO MFS-IDPFK                                        
050700     MOVE MFS-IDTRANS TO W-IDTRANS                                        
050800                                                                          
050900     MOVE LOW-VALUE TO MSG-AREA                                           
051000     MOVE 'W2O428N1' TO MFS-IDMOD                                         
051100     MOVE '2428' TO MOD-IDTRANS                                           
051200     MOVE MFS-ERASE-FIELD TO MOD-TEMFSFEL MOD-TEMFSINF                    
051300                                                                          
051400     IF OWN-MID OR HELP-MID                                               
051500       CONTINUE                                                           
051600     ELSE                                                                 
051700       MOVE SPACE TO MFS-KDTRTYP                                          
051800       MOVE '7' TO MFS-IDPFK                                              
051900     END-IF                                                               
052000     .                                                                    
052100     EJECT                                                                
052200 B-CHECK-KEYS SECTION.                                                    
052300                                                                          
052400     MOVE ALL '+'           TO MSGI-WMSGINIT                              
052500     MOVE '001'             TO MSGI-KDCALL                                
052600     MOVE MSG-LTERM-NAME    TO MSGI-IDLTERM-USER                          
052700     MOVE MSG-SIGNON-USERID TO MSGI-IDUSER                                
052800     MOVE '2428'            TO MSGI-IDTRANS                               
052900     IF GOOD-MID                                                          
053000       MOVE MID-IDARTNR-IN  TO MSGI-IDARTNR                               
053100       MOVE MID-IDDC-IN     TO MSGI-IDDC-KEY                              
053200     ELSE                                                                 
053300       MOVE ALL '+'         TO MID-IDARTNR-IN                             
053400       MOVE ALL '+'         TO MID-IDDC-IN                                
053500     END-IF                                                               
053600     CALL W005INIT USING MSGI-WMSGINIT WDP7-PCB                           
053700*    MOVE MSGI-SPAR-AREA TO SAVE-AREA                                     
053800                                                                          
053900*    - LANGUAGE TO BE USED BY MEDKONV                                     
054000     MOVE MSGI-IDLAND-SPR TO MED-IDSKYLT                                  
054100                                                                          
054200     MOVE YES TO KEYS-SW                                                  
054300                                                                          
054400                                                                          
054500*    -- CHECK OF IDARTNR                                                  
054600     MOVE MFS-ERASE-FIELD TO MOD-IDARTNR-IN                               
054700                                                                          
054800     IF MID-IDARTNR-IN = ALL '+' OR SPACE                                 
054900       IF MSGI-IDARTNR = ALL '+' OR SPACE                                 
055000          MOVE 0              TO WS-IDARTNR                               
055100       ELSE                                                               
055200          MOVE MSGI-IDARTNR   TO WS-IDARTNR                               
055300       END-IF                                                             
055400     ELSE                                                                 
055500       MOVE '7'               TO MFS-IDPFK                                
055600       MOVE SPACE             TO MFS-KDTRTYP                              
055700       MOVE MID-IDARTNR-IN    TO WS-IDARTNR                               
055800     END-IF                                                               
055900                                                                          
056000     INSPECT WS-IDARTNR REPLACING LEADING SPACE BY ZERO                   
056100     IF WS-IDARTNR NUMERIC AND WS-IDARTNR > ZERO                          
056200        MOVE WS-IDARTNR       TO W-IDARTNR                                
056300     ELSE                                                                 
056400        MOVE NOO              TO KEYS-SW                                  
056500     END-IF                                                               
056600                                                                          
056700* GET THE CONTROL DIGIT TO THE PART NUMBER                                
056800     IF KEYS-OK                                                           
056900        PERFORM BA-GET-CNTRL-DIGIT                                        
057000     END-IF                                                               
057100                                                                          
057200*    -- CHECK OF IDDC                                                     
057300     MOVE MFS-ERASE-FIELD TO MOD-IDDC-IN                                  
057400                                                                          
057500     IF MID-IDDC-IN NOT = ALL '+'                                         
057600       MOVE '7'               TO MFS-IDPFK                                
057700       MOVE SPACE             TO MFS-KDTRTYP                              
057800     END-IF                                                               
057900     MOVE MSGI-IDDC-KEY  TO W-IDDC                                        
058000                                                                          
058100*** ONLY CHINA/USA NDC IS ALLOWED AS KEY INPUT                            
058200     PERFORM BB-CHECK-IDDC                                                
058300                                                                          
058400     IF GOOD-MID OR KEYS-OK                                               
058500       MOVE MSGI-IDDC-KEY      TO MOD-IDDC-UT                             
058600       MOVE WS-IDARTNR         TO MOD-IDARTNR-UT                          
058700       INSPECT MOD-IDARTNR-UT REPLACING LEADING ZERO BY SPACE             
058800       INSPECT MOD-IDDC-UT    REPLACING ALL '+' BY SPACE                  
058900     ELSE                                                                 
059000       MOVE MFS-ERASE-FIELD    TO MOD-IDDC-UT                             
059100                                  MOD-IDARTNR                             
059200     END-IF                                                               
059300                                                                          
059400     IF DC-WRONG                                                          
059500       MOVE ERR-DC-INVALID     TO MED-IDMFSFEL                            
059600       CALL WMEDKONV        USING MED-WMEDAREA                            
059700       MOVE MED-MFSFEL         TO MOD-TEMFSFEL                            
059800       PERFORM MFS-ERASE-FIELD-IN                                         
059900       PERFORM MFS-ERASE-FIELD-LINE-OUT                                   
060000     END-IF                                                               
060100                                                                          
060200     IF KEYS-WRONG                                                        
060300       MOVE ERR-WRONG-KEY      TO MED-IDMFSFEL                            
060400       CALL WMEDKONV USING MED-WMEDAREA                                   
060500       MOVE MED-MFSFEL         TO MOD-TEMFSFEL                            
060600       PERFORM MFS-ERASE-FIELD-IN                                         
060700       PERFORM MFS-ERASE-FIELD-LINE-OUT                                   
060800     END-IF                                                               
060900                                                                          
061000     IF KEYS-WRONG                                                        
061100     AND (WS-IDARTNR = ZEROS OR W-IDDC = SPACES)                          
061200       MOVE ERR-KEY-MISSING    TO MED-IDMFSFEL                            
061300       CALL WMEDKONV        USING MED-WMEDAREA                            
061400       MOVE MED-MFSFEL         TO MOD-TEMFSFEL                            
061500       MOVE MFS-ERASE-FIELD    TO MOD-IDARTNR                             
061600       PERFORM MFS-ERASE-FIELD-IN                                         
061700       PERFORM MFS-ERASE-FIELD-LINE-OUT                                   
061800     END-IF                                                               
061900     .                                                                    
062000     EJECT                                                                
062100 BA-GET-CNTRL-DIGIT SECTION.                                              
062200                                                                          
062300     PERFORM IMS-GU-WDK601                                                
062400     IF SEGMENT-FOUND                                                     
062500        MOVE ART-REKSIFFR      TO MOD-REKSIFFR                            
062600        MOVE WS-DASH           TO MOD-DASH-1                              
062700     ELSE                                                                 
062800        MOVE NOO               TO INDATA-SW                               
062900        MOVE ERR-PART-MISSING  TO MED-IDMFSFEL                            
063000        CALL WMEDKONV USING MED-WMEDAREA                                  
063100        MOVE MED-MFSFEL        TO MOD-TEMFSFEL                            
063200        PERFORM MFS-ERASE-FIELD-IN                                        
063300        PERFORM MFS-ERASE-FIELD-OUT                                       
063400     END-IF                                                               
063500     .                                                                    
063600     EJECT                                                                
063700 BB-CHECK-IDDC SECTION.                                                   
063800                                                                          
063900     IF W-IDDC NOT = SPACES                                               
064000        PERFORM IMS-GU-WDB601                                             
064100        IF SEGMENT-FOUND                                                  
064200           IF DCS-NDC-CN OR                                               
064300             (DCS-NDC-NA AND DCS-USA)                                     
064400              MOVE YES   TO DC-SW                                         
064500                                                                          
064600             MOVE DCS-IDLANDX2 TO W-IDLAND                                
064700                                                                          
064800             IF DCS-NDC-CN                                                
064900               MOVE '7A'  TO W-IDDC-MIN                                   
065000               MOVE '79'  TO W-IDDC-MAX                                   
065100             END-IF                                                       
065200             IF DCS-NDC-NA                                                
065300               MOVE '4A'  TO W-IDDC-MIN                                   
065400               MOVE '49'  TO W-IDDC-MAX                                   
065500             END-IF                                                       
065600           ELSE                                                           
065700              MOVE NOO   TO DC-SW                                         
065800                            INDATA-SW                                     
065900           END-IF                                                         
066000        ELSE                                                              
066100           MOVE NOO      TO DC-SW                                         
066200                            INDATA-SW                                     
066300        END-IF                                                            
066400     END-IF                                                               
066500     .                                                                    
066600     EJECT                                                                
066700 F-READ-SHOW-INFO SECTION.                                                
066800     MOVE 'F-READ-SHOW-INFO '  TO CURRENT-SECTION                         
066900                                                                          
067000     PERFORM FA-FETCH-WEEK                                                
067100     PERFORM FB-CALC-SEPARATE-DEMAND                                      
067200     PERFORM FC-CALC-REFILL-DC-DEMAND                                     
067300     PERFORM FG-CALC-REFILL-CDC-DEMAND                                    
067400     PERFORM FH-CALC-GLOBAL-DEMAND                                        
067500     PERFORM FD-CALC-TREND-DEMAND                                         
067600     PERFORM FE-CALC-TOTAL                                                
067700     PERFORM FF-CALC-PB-PLAN-DEMAND                                       
067800                                                                          
067900     .                                                                    
068000     EJECT                                                                
068100                                                                          
068200 FA-FETCH-WEEK SECTION.                                                   
068300     MOVE 'FA-FETCH-WEEK  '   TO CURRENT-SECTION                          
068400                                                                          
068500     MOVE FUNCTION CURRENT-DATE TO WS-CURRENT-DATE                        
068600                                                                          
068700     MOVE 'IDAG'                TO DAT-KDDATFORM                          
068800                                                                          
068900     CALL WDATKONV           USING DAT-KDDATFORM DAT-I-TIDATUM            
069000                                   DAT-O-TIDATUM DAT-KDSVAR               
069100                                                                          
069200     IF DAT-KDSVAR-OK                                                     
069300        MOVE DAT-TIAAVV-GRP      TO W-TIAAVV-NUM                          
069400        MOVE W-TIAAVV-NUM        TO LINK-TIAAVV-AKTUELL                   
069500                                    LINK-TIBEHOV-START                    
069600        MOVE 1                   TO W-ANTAL-VECKOR                        
069700        CALL W009VADD         USING LINK-TIBEHOV-START                    
069800                                    W-ANTAL-VECKOR                        
069900        MOVE DAT-TID             TO LINK-TID-AKTUELL                      
070000        MOVE W-IDDC              TO LINK-IDDC                             
070100     ELSE                                                                 
070200        CALL FELLOG                                                       
070300     END-IF                                                               
070400                                                                          
070500     MOVE +1                     TO INDX                                  
070600     PERFORM UNTIL INDX >  MAX-INDX                                       
070700        MOVE ZERO                TO W-KVPB-TOT(INDX)                      
070800        ADD +1                   TO INDX                                  
070900     END-PERFORM                                                          
071000                                                                          
071100     MOVE 1                      TO W-ANTAL-VECKOR                        
071200     MOVE +1                     TO INDX                                  
071300     MOVE W-TIAAVV-NUM           TO MOD-TIAAVV(INDX)                      
071400     ADD +1                      TO INDX                                  
071500     MOVE LINK-TIBEHOV-START     TO MOD-TIAAVV(INDX)                      
071600                                    W-TIAAVV                              
071700                                                                          
071800     ADD +1                      TO INDX                                  
071900     PERFORM UNTIL INDX        >  MAX-INDX                                
072000                                                                          
072100        CALL W009VADD USING W-TIAAVV W-ANTAL-VECKOR                       
072200        MOVE W-TIAAVV            TO MOD-TIAAVV(INDX)                      
072300        ADD +1                   TO INDX                                  
072400     END-PERFORM                                                          
072500                                                                          
072600     .                                                                    
072700     EJECT                                                                
072800 FB-CALC-SEPARATE-DEMAND SECTION.                                         
072900     MOVE 'FB-CALC-SEPARATE-DEMAND '  TO CURRENT-SECTION                  
073000**--                                                                      
073100**-- JAPAN OCH KINA HAR 6 ARBETSDAGAR.                                    
073200**--                                                                      
073300                                                                          
073400     MOVE FUNCTION CURRENT-DATE TO WS-CURRENT-DATE                        
073500                                                                          
073600     MOVE +1                     TO INDX                                  
073700     ACCEPT W-TIME               FROM TIME                                
073800     COMPUTE W-VECKO-SEP-BEHOV ROUNDED  =                                 
073900                           SPAR-KVPB-REF / 4.33                           
074000                                                                          
074100     MOVE W-IDDC  TO WS-IDDC                                              
074200     IF NDC-CN OR NDC-JP                                                  
074300       COMPUTE W-DAG-SEP-BEHOV   ROUNDED  =                               
074400                             W-VECKO-SEP-BEHOV / 6                        
074500     ELSE                                                                 
074600       COMPUTE W-DAG-SEP-BEHOV   ROUNDED  =                               
074700                             W-VECKO-SEP-BEHOV / 5                        
074800     END-IF                                                               
074900                                                                          
075000     PERFORM IMS-GU-WDK712                                                
075100     IF LART-DAPUBL > 0                                                   
075200        MOVE LART-DAPUBL       TO W-TIFINLV                               
075300     ELSE                                                                 
075400        MOVE ART-TIFINLV       TO DAT-I-TIDATUM                           
075500        MOVE 'AAVVD'           TO DAT-KDDATFORM                           
075600        CALL WDATKONV       USING DAT-KDDATFORM                           
075700                               DAT-I-TIDATUM                              
075800                               DAT-O-TIDATUM                              
075900                               DAT-KDSVAR                                 
076000        MOVE DAT-TISEKEL       TO W-TIFINLV-SS                            
076100        MOVE DAT-TIAAMMDD      TO W-TIFINLV-AAMMDD                        
076200     END-IF                                                               
076300                                                                          
076400     IF NDC-CN OR NDC-JP                                                  
076500       IF  LINK-TID-AKTUELL = 7 OR                                        
076600          (LINK-TID-AKTUELL = 6 AND W-TIME(1:4) > 1700)                   
076700       OR  (WS-DAGENS-DATE < W-TIFINLV)                                   
076800           MOVE ZERO              TO MOD-KVPB-REF(INDX)                   
076900       ELSE                                                               
077000           COMPUTE W-KVDAGAR-KVAR =  6 - LINK-TID-AKTUELL                 
077100           IF W-TIME(1:4)         <= 1700                                 
077200              ADD +1              TO W-KVDAGAR-KVAR                       
077300           END-IF                                                         
077400           COMPUTE MOD-KVPB-REF(INDX) = W-DAG-SEP-BEHOV *                 
077500                                        W-KVDAGAR-KVAR                    
077600                                                                          
077700           COMPUTE W-KVPB-TOT(INDX)   = (W-DAG-SEP-BEHOV *                
077800                                         W-KVDAGAR-KVAR) +                
077900                                         W-KVPB-TOT(INDX)                 
078000       END-IF                                                             
078100     ELSE                                                                 
078200       IF  LINK-TID-AKTUELL = 6 OR                                        
078300           LINK-TID-AKTUELL = 7 OR                                        
078400          (LINK-TID-AKTUELL = 5 AND W-TIME(1:4) > 1700)                   
078500       OR (WS-DAGENS-DATE < W-TIFINLV)                                    
078600           MOVE ZERO              TO MOD-KVPB-REF(INDX)                   
078700       ELSE                                                               
078800           COMPUTE W-KVDAGAR-KVAR =  5 - LINK-TID-AKTUELL                 
078900           IF W-TIME(1:4)         <= 1700                                 
079000              ADD +1              TO W-KVDAGAR-KVAR                       
079100           END-IF                                                         
079200           COMPUTE MOD-KVPB-REF(INDX) = W-DAG-SEP-BEHOV *                 
079300                                        W-KVDAGAR-KVAR                    
079400                                                                          
079500           COMPUTE W-KVPB-TOT(INDX)   = (W-DAG-SEP-BEHOV *                
079600                                         W-KVDAGAR-KVAR) +                
079700                                         W-KVPB-TOT(INDX)                 
079800       END-IF                                                             
079900     END-IF                                                               
080000                                                                          
080100     MOVE W-IDARTNR             TO LINK-IDARTNR                           
080200     MOVE 13                    TO LINK-KVVECKOR-BEHOV                    
080300     MOVE ZERO                  TO LINK-KVTILLG-TOT-CDC                   
080400     MOVE ENDAST-SEPARATBEHOV   TO LINK-KDBEHOV                           
080500                                                                          
080600     IF ART-KDERS-UTG = 0                                                 
080700       CALL W222BHDC         USING LINK-AREA                              
080800                                   BHDC-WDK6-PCB                          
080900                                   BHDC-WDK7-1-PCB                        
081000                                   BHDC-WDB6-PCB                          
081100                                   BHDC-WDR2-PCB                          
081200                                   BHDC-WDD7-PCB                          
081300                                   BHDC-WDK7-2-PCB                        
081400                                   BHDC-WDD7-2-PCB                        
081500                                   BHDC-WDK9-PCB                          
081600                                   BHDC-REFL1-2501-PCB                    
081700                                   BHDC-REFL1-WDB6-PCB                    
081800                                   BHDC-REFL1-WDK7-PCB                    
081900                                   BHDC-REFL1-UTIL-WDK6-PCB               
082000                                   BHDC-REFL1-UTIL-WDK7-PCB               
082100                                   BHDC-REFL1-UTIL-WDB6-PCB               
082200                                   BHDC-REFL2-2501-PCB                    
082300                                   BHDC-REFL2-WDB6-PCB                    
082400                                   BHDC-REFL2-UTIL-WDK6-PCB               
082500                                   BHDC-REFL2-UTIL-WDK7-PCB               
082600                                   BHDC-REFL2-UTIL-WDB6-PCB               
082700                                   BHDC-UTIL-WDK6-PCB                     
082800                                   BHDC-UTIL-WDK7-PCB                     
082900                                   BHDC-UTIL-WDB6-PCB                     
083000                                   BHDC-W222-WDK6-PCB                     
083100                                   BHDC-W222-WDK7-PCB                     
083200                                   BHDC-W222-ARTM-PCB                     
083300                                   BHDC-W222-2501-PCB                     
083400                                   BHDC-W222-WDB6R-PCB                    
083500                                   BHDC-W222-WDK7R-PCB                    
083600                                   BHDC-W222-WDB6-PCB                     
083700                                   BHDC-W222-WDD7-PCB                     
083800                                   BHDC-W222-WDK7E-PCB                    
083900                                   BHDC-W222-UTIL-WDK6-PCB                
084000                                   BHDC-W222-UTIL-WDK7-PCB                
084100                                   BHDC-W222-UTIL-WDB6-PCB                
084200                                   BHDC-W222-UTUP-WDK7-PCB                
084300                                   BHDC-W222-UTUP-WDB6-PCB                
084400                                   BHDC-W222-UTUP-UTIL-WDK6-PCB           
084500                                   BHDC-W222-UTUP-UTIL-WDK7-PCB           
084600                                   BHDC-W222-UTUP-UTIL-WDB6-PCB           
084700                                   BHDC-UTUP-WDK7-PCB                     
084800                                   BHDC-UTUP-WDB6-PCB                     
084900                                   BHDC-UTUP-UTIL-WDK6-PCB                
085000                                   BHDC-UTUP-UTIL-WDK7-PCB                
085100                                   BHDC-UTUP-UTIL-WDB6-PCB                
085200                                                                          
085300       IF LINK-FLJANEJ-ANROP = 'N'                                        
085400         PERFORM S20-NOLLA-BHDC-RESULTATFLT                               
085500       END-IF                                                             
085600     ELSE                                                                 
085700       PERFORM S20-NOLLA-BHDC-RESULTATFLT                                 
085800     END-IF                                                               
085900                                                                          
086000                                                                          
086100     MOVE +2                             TO INDX                          
086200     MOVE +1                             TO INDX1                         
086300     PERFORM UNTIL INDX                  >  MAX-INDX                      
086400        MOVE LINK-KVBEHOV-VECKA(INDX1)   TO MOD-KVPB-REF(INDX)            
086500        ADD  LINK-KVBEHOV-VECKA(INDX1)   TO W-KVPB-TOT(INDX)              
086600        ADD +1                           TO INDX                          
086700                                            INDX1                         
086800     END-PERFORM                                                          
086900     .                                                                    
087000     EJECT                                                                
087100                                                                          
087200 FC-CALC-REFILL-DC-DEMAND SECTION.                                        
087300     MOVE 'FC-CALC-REFILL-DC-DEMAND '  TO CURRENT-SECTION                 
087400***  ENDAST REFILL INOM LANDET, EJ GLOBALA NDC'R                          
087500                                                                          
087600     PERFORM IMS-GU-WDK701                                                
087700     IF SEGMENT-FOUND                                                     
087800        PERFORM IMS-GNP-WDK711-IDDCREF                                    
087900        IF SEGMENT-FOUND                                                  
088000           PERFORM FCA-CALC-DC-DEMAND                                     
088100        ELSE                                                              
088200           MOVE +1                    TO INDX                             
088300           PERFORM UNTIL INDX >  MAX-INDX                                 
088400             MOVE ZERO                TO MOD-KVPB-REFILL(INDX)            
088500             ADD +1                   TO INDX                             
088600           END-PERFORM                                                    
088700        END-IF                                                            
088800     END-IF                                                               
088900     .                                                                    
089000     EJECT                                                                
089100 FCA-CALC-DC-DEMAND SECTION.                                              
089200     MOVE ' FCA-CALC-DC-DEMAND '  TO CURRENT-SECTION                      
089300                                                                          
089400     MOVE 13                    TO LINK-KVVECKOR-BEHOV                    
089500     MOVE ZERO                  TO LINK-KVTILLG-TOT-CDC                   
089600     MOVE ENDAST-LOKALBEHOV     TO LINK-KDBEHOV                           
089700                                                                          
089800     IF ART-KDERS-UTG = 0                                                 
089900       CALL W222BHDC         USING LINK-AREA                              
090000                                   BHDC-WDK6-PCB                          
090100                                   BHDC-WDK7-1-PCB                        
090200                                   BHDC-WDB6-PCB                          
090300                                   BHDC-WDR2-PCB                          
090400                                   BHDC-WDD7-PCB                          
090500                                   BHDC-WDK7-2-PCB                        
090600                                   BHDC-WDD7-2-PCB                        
090700                                   BHDC-WDK9-PCB                          
090800                                   BHDC-REFL1-2501-PCB                    
090900                                   BHDC-REFL1-WDB6-PCB                    
091000                                   BHDC-REFL1-WDK7-PCB                    
091100                                   BHDC-REFL1-UTIL-WDK6-PCB               
091200                                   BHDC-REFL1-UTIL-WDK7-PCB               
091300                                   BHDC-REFL1-UTIL-WDB6-PCB               
091400                                   BHDC-REFL2-2501-PCB                    
091500                                   BHDC-REFL2-WDB6-PCB                    
091600                                   BHDC-REFL2-UTIL-WDK6-PCB               
091700                                   BHDC-REFL2-UTIL-WDK7-PCB               
091800                                   BHDC-REFL2-UTIL-WDB6-PCB               
091900                                   BHDC-UTIL-WDK6-PCB                     
092000                                   BHDC-UTIL-WDK7-PCB                     
092100                                   BHDC-UTIL-WDB6-PCB                     
092200                                   BHDC-W222-WDK6-PCB                     
092300                                   BHDC-W222-WDK7-PCB                     
092400                                   BHDC-W222-ARTM-PCB                     
092500                                   BHDC-W222-2501-PCB                     
092600                                   BHDC-W222-WDB6R-PCB                    
092700                                   BHDC-W222-WDK7R-PCB                    
092800                                   BHDC-W222-WDB6-PCB                     
092900                                   BHDC-W222-WDD7-PCB                     
093000                                   BHDC-W222-WDK7E-PCB                    
093100                                   BHDC-W222-UTIL-WDK6-PCB                
093200                                   BHDC-W222-UTIL-WDK7-PCB                
093300                                   BHDC-W222-UTIL-WDB6-PCB                
093400                                   BHDC-W222-UTUP-WDK7-PCB                
093500                                   BHDC-W222-UTUP-WDB6-PCB                
093600                                   BHDC-W222-UTUP-UTIL-WDK6-PCB           
093700                                   BHDC-W222-UTUP-UTIL-WDK7-PCB           
093800                                   BHDC-W222-UTUP-UTIL-WDB6-PCB           
093900                                   BHDC-UTUP-WDK7-PCB                     
094000                                   BHDC-UTUP-WDB6-PCB                     
094100                                   BHDC-UTUP-UTIL-WDK6-PCB                
094200                                   BHDC-UTUP-UTIL-WDK7-PCB                
094300                                   BHDC-UTUP-UTIL-WDB6-PCB                
094400                                                                          
094500                                                                          
094600       IF LINK-FLJANEJ-ANROP = 'N'                                        
094700         PERFORM S20-NOLLA-BHDC-RESULTATFLT                               
094800       END-IF                                                             
094900     ELSE                                                                 
095000       PERFORM S20-NOLLA-BHDC-RESULTATFLT                                 
095100     END-IF                                                               
095200                                                                          
095300     MOVE +1                     TO INDX                                  
095400     MOVE LINK-KVBEHOV-DESSUTOM  TO MOD-KVPB-REFILL(INDX)                 
095500     ADD  LINK-KVBEHOV-DESSUTOM  TO W-KVPB-TOT(INDX)                      
095600     MOVE +2                            TO INDX                           
095700     MOVE +1                            TO INDX1                          
095800     PERFORM UNTIL INDX                 >  MAX-INDX                       
095900        MOVE LINK-KVBEHOV-VECKA(INDX1)  TO MOD-KVPB-REFILL(INDX)          
096000        ADD  LINK-KVBEHOV-VECKA(INDX1)  TO W-KVPB-TOT(INDX)               
096100        ADD +1                          TO INDX                           
096200                                           INDX1                          
096300     END-PERFORM                                                          
096400     .                                                                    
096500     EJECT                                                                
096600                                                                          
096700 FD-CALC-TREND-DEMAND SECTION.                                            
096800     MOVE 'FD-CALC-TREND-DEMAND '  TO CURRENT-SECTION                     
096900                                                                          
097000     PERFORM IMS-GU-WDK722                                                
097100     IF SEGMENT-FOUND                                                     
097200        IF XLAG-KVPB-TREND NOT = ZERO                                     
097300           MOVE XLAG-KVVECKOR-TREND    TO MAX-KVVECKOR-TREND              
097400           MOVE +1                     TO INDX                            
097500           MOVE +1                     TO INDX-T                          
097600           PERFORM UNTIL INDX          >  MAX-INDX                        
097700              PERFORM FDA-CALC-TRENDS                                     
097800              COMPUTE SPAR-KVPB-TOT ROUNDED =                             
097900                 W-KVPB-TOT (INDX) / 2                                    
098000              ADD  WS-KVPB-TREND   TO W-KVPB-TOT (INDX)                   
098100*                  ÄR DET RÄTT TECKEN ?                                   
098200              IF W-KVPB-TOT (INDX) < SPAR-KVPB-TOT                        
098300                 MOVE SPAR-KVPB-TOT TO W-KVPB-TOT (INDX)                  
098400              END-IF                                                      
098500              ADD +1           TO INDX                                    
098600              ADD +1           TO INDX-T                                  
098700           END-PERFORM                                                    
098800        ELSE                                                              
098900           MOVE +1             TO INDX                                    
099000           PERFORM UNTIL INDX >  MAX-INDX                                 
099100              MOVE ZERO        TO MOD-KVPB-TREND (INDX)                   
099200              ADD +1           TO INDX                                    
099300           END-PERFORM                                                    
099400        END-IF                                                            
099500     ELSE                                                                 
099600        MOVE +1                TO INDX                                    
099700        PERFORM UNTIL INDX >  MAX-INDX                                    
099800          MOVE ZERO            TO MOD-KVPB-TREND (INDX)                   
099900          ADD +1               TO INDX                                    
100000        END-PERFORM                                                       
100100     END-IF                                                               
100200     .                                                                    
100300     EJECT                                                                
100400 FDA-CALC-TRENDS SECTION.                                                 
100500     MOVE 'FDA-CALC-TRENDS  '   TO CURRENT-SECTION                        
100600                                                                          
100700     IF LINK-TID-AKTUELL > 5                                              
100800     OR (LINK-TID-AKTUELL = 5 AND WS-DAGENS-TIME > 17)                    
100900*** IF TODAY'S DAY IS A WEEKEND                                           
101000        IF INDX-T NOT < MAX-KVVECKOR-TREND                                
101100           MOVE MAX-KVVECKOR-TREND TO INDX-T                              
101200        END-IF                                                            
101300        COMPUTE WS-KVPB-TREND ROUNDED =                                   
101400               INDX-T * XLAG-KVPB-TREND / 4.33                            
101500        END-COMPUTE                                                       
101600        MOVE    WS-KVPB-TREND       TO                                    
101700                MOD-KVPB-TREND (INDX)                                     
101800     ELSE                                                                 
101900*** IF TODAY'S DAY FALLS ON THE MIDDLE OF THE WEEK                        
102000        IF INDX-T NOT < MAX-KVVECKOR-TREND                                
102100           MOVE MAX-KVVECKOR-TREND TO INDX-T                              
102200        END-IF                                                            
102300        COMPUTE WS-KVPB-TREND ROUNDED =                                   
102400               INDX-T * XLAG-KVPB-TREND / 4.33                            
102500        END-COMPUTE                                                       
102600        IF INDX = 1                                                       
102700           IF LINK-TID-AKTUELL = 2                                        
102800              COMPUTE WS-KVPB-TREND ROUNDED =                             
102900                      WS-KVPB-TREND * 0.80                                
103000           END-IF                                                         
103100           IF LINK-TID-AKTUELL = 3                                        
103200              COMPUTE WS-KVPB-TREND ROUNDED =                             
103300                      WS-KVPB-TREND * 0.60                                
103400           END-IF                                                         
103500           IF LINK-TID-AKTUELL = 4                                        
103600              COMPUTE WS-KVPB-TREND ROUNDED =                             
103700                      WS-KVPB-TREND * 0.40                                
103800           END-IF                                                         
103900           IF LINK-TID-AKTUELL = 5                                        
104000              COMPUTE WS-KVPB-TREND ROUNDED =                             
104100                      WS-KVPB-TREND * 0.20                                
104200           END-IF                                                         
104300        END-IF                                                            
104400        MOVE    WS-KVPB-TREND   TO MOD-KVPB-TREND (INDX)                  
104500        IF INDX = 1                                                       
104600          MOVE  WS-KVPB-TREND   TO WS-KVPB-TREND-RAD1                     
104700        END-IF                                                            
104800     END-IF                                                               
104900     .                                                                    
105000     EJECT                                                                
105100 FE-CALC-TOTAL SECTION.                                                   
105200     MOVE 'FE-CALC-TOTAL '   TO CURRENT-SECTION                           
105300                                                                          
105400     MOVE +1                        TO INDX                               
105500     PERFORM UNTIL INDX             >  MAX-INDX                           
105600        IF W-KVPB-TOT(INDX) > ZERO                                        
105700           MOVE W-KVPB-TOT(INDX)    TO MOD-KVPB-TOT(INDX)                 
105800        ELSE                                                              
105900           MOVE ZERO                TO MOD-KVPB-TOT(INDX)                 
106000        END-IF                                                            
106100        ADD +1                      TO INDX                               
106200     END-PERFORM                                                          
106300     .                                                                    
106400     EJECT                                                                
106500 FF-CALC-PB-PLAN-DEMAND  SECTION.                                         
106600     MOVE 'FF-CALC-PB-PLAN-DEMAND '   TO CURRENT-SECTION                  
106700**--                                                                      
106800**-- JAPAN OCH KINA HAR 6 ARBETSDAGAR.                                    
106900**--                                                                      
107000                                                                          
107100     PERFORM IMS-GU-WDK722                                                
107200     IF SEGMENT-FOUND                                                     
107300     AND  (XLAG-DAPBPLAN >= WS-DAGENS-DATE OR                             
107400           XLAG-DASEASON >= WS-DAGENS-DATE)                               
107500                                                                          
107600        MOVE 13                    TO LINK-KVVECKOR-BEHOV                 
107700                                                                          
107800        MOVE ZERO                  TO LINK-KVTILLG-TOT-CDC                
107900        MOVE W-IDARTNR             TO TILG-IDARTNR                        
108000        CALL W222TILG        USING TILG-W222TILG                          
108100                                   TILG-WDK7-PCB TILG-WDL2-PCB            
108200                                   TILG-WDB6-PCB TILG-WDD9-PCB            
108300                                   TILG-WDK6-PCB TILG-WDK9-PCB            
108400                                                                          
108500        MOVE TILG-KVTILLG-TOT      TO LINK-KVTILLG-TOT-CDC                
108600                                                                          
108700        MOVE PB-TOTAL-SEP-LEV-XDC  TO LINK-KDBEHOV                        
108800                                                                          
108900        IF ART-KDERS-UTG = 0                                              
109000          CALL W222BHDC      USING LINK-AREA                              
109100                                   BHDC-WDK6-PCB                          
109200                                   BHDC-WDK7-1-PCB                        
109300                                   BHDC-WDB6-PCB                          
109400                                   BHDC-WDR2-PCB                          
109500                                   BHDC-WDD7-PCB                          
109600                                   BHDC-WDK7-2-PCB                        
109700                                   BHDC-WDD7-2-PCB                        
109800                                   BHDC-WDK9-PCB                          
109900                                   BHDC-REFL1-2501-PCB                    
110000                                   BHDC-REFL1-WDB6-PCB                    
110100                                   BHDC-REFL1-WDK7-PCB                    
110200                                   BHDC-REFL1-UTIL-WDK6-PCB               
110300                                   BHDC-REFL1-UTIL-WDK7-PCB               
110400                                   BHDC-REFL1-UTIL-WDB6-PCB               
110500                                   BHDC-REFL2-2501-PCB                    
110600                                   BHDC-REFL2-WDB6-PCB                    
110700                                   BHDC-REFL2-UTIL-WDK6-PCB               
110800                                   BHDC-REFL2-UTIL-WDK7-PCB               
110900                                   BHDC-REFL2-UTIL-WDB6-PCB               
111000                                   BHDC-UTIL-WDK6-PCB                     
111100                                   BHDC-UTIL-WDK7-PCB                     
111200                                   BHDC-UTIL-WDB6-PCB                     
111300                                   BHDC-W222-WDK6-PCB                     
111400                                   BHDC-W222-WDK7-PCB                     
111500                                   BHDC-W222-ARTM-PCB                     
111600                                   BHDC-W222-2501-PCB                     
111700                                   BHDC-W222-WDB6R-PCB                    
111800                                   BHDC-W222-WDK7R-PCB                    
111900                                   BHDC-W222-WDB6-PCB                     
112000                                   BHDC-W222-WDD7-PCB                     
112100                                   BHDC-W222-WDK7E-PCB                    
112200                                   BHDC-W222-UTIL-WDK6-PCB                
112300                                   BHDC-W222-UTIL-WDK7-PCB                
112400                                   BHDC-W222-UTIL-WDB6-PCB                
112500                                   BHDC-W222-UTUP-WDK7-PCB                
112600                                   BHDC-W222-UTUP-WDB6-PCB                
112700                                   BHDC-W222-UTUP-UTIL-WDK6-PCB           
112800                                   BHDC-W222-UTUP-UTIL-WDK7-PCB           
112900                                   BHDC-W222-UTUP-UTIL-WDB6-PCB           
113000                                   BHDC-UTUP-WDK7-PCB                     
113100                                   BHDC-UTUP-WDB6-PCB                     
113200                                   BHDC-UTUP-UTIL-WDK6-PCB                
113300                                   BHDC-UTUP-UTIL-WDK7-PCB                
113400                                   BHDC-UTUP-UTIL-WDB6-PCB                
113500                                                                          
113600                                                                          
113700          IF LINK-FLJANEJ-ANROP = 'N'                                     
113800            PERFORM S20-NOLLA-BHDC-RESULTATFLT                            
113900          END-IF                                                          
114000        ELSE                                                              
114100          PERFORM S20-NOLLA-BHDC-RESULTATFLT                              
114200        END-IF                                                            
114300                                                                          
114400        MOVE +1                     TO INDX                               
114500                                                                          
114600        ACCEPT W-TIME               FROM TIME                             
114700        COMPUTE W-VECKO-SEP-BEHOV ROUNDED = SPAR-KVPB-REF / 4.33          
114800                                                                          
114900        MOVE W-IDDC  TO WS-IDDC                                           
115000        IF NDC-CN OR NDC-JP                                               
115100          COMPUTE W-DAG-SEP-BEHOV ROUNDED =                               
115200                                  W-VECKO-SEP-BEHOV / 6                   
115300                                                                          
115400          IF LINK-TID-AKTUELL = 7 OR                                      
115500            (LINK-TID-AKTUELL = 6 AND W-TIME(1:4) > 1700)                 
115600          OR (WS-DAGENS-DATE < W-TIFINLV)                                 
115700                                                                          
115800             CONTINUE                                                     
115900          ELSE                                                            
116000             COMPUTE W-KVDAGAR-KVAR = 6 - LINK-TID-AKTUELL                
116100             IF W-TIME(1:4)      <= 1700                                  
116200                ADD +1           TO W-KVDAGAR-KVAR                        
116300             END-IF                                                       
116400                                                                          
116500             COMPUTE WS-KVPB-REF-RAD1  = W-DAG-SEP-BEHOV *                
116600                                         W-KVDAGAR-KVAR                   
116700                                                                          
116800             IF LINK-KVBEHOV-DESSUTOM = ZERO                              
116900               ADD  WS-KVPB-TREND-RAD1 TO                                 
117000                                      WS-KVPB-REF-RAD1                    
117100             END-IF                                                       
117200                                                                          
117300                                                                          
117400             COMPUTE LINK-KVBEHOV-DESSUTOM ROUNDED =                      
117500                         LINK-KVBEHOV-DESSUTOM + WS-KVPB-REF-RAD1         
117600                                                                          
117700          END-IF                                                          
117800        ELSE                                                              
117900          COMPUTE W-DAG-SEP-BEHOV ROUNDED =                               
118000                                  W-VECKO-SEP-BEHOV / 5                   
118100                                                                          
118200          IF LINK-TID-AKTUELL = 6 OR                                      
118300             LINK-TID-AKTUELL = 7 OR                                      
118400             (LINK-TID-AKTUELL = 5 AND W-TIME(1:4) > 1700)                
118500          OR (WS-DAGENS-DATE < W-TIFINLV)                                 
118600                                                                          
118700             CONTINUE                                                     
118800          ELSE                                                            
118900             COMPUTE W-KVDAGAR-KVAR = 5 - LINK-TID-AKTUELL                
119000             IF W-TIME(1:4)      <= 1700                                  
119100                ADD +1           TO W-KVDAGAR-KVAR                        
119200             END-IF                                                       
119300                                                                          
119400             COMPUTE WS-KVPB-REF-RAD1  = W-DAG-SEP-BEHOV *                
119500                                         W-KVDAGAR-KVAR                   
119600                                                                          
119700             IF LINK-KVBEHOV-DESSUTOM = ZERO                              
119800               ADD  WS-KVPB-TREND-RAD1 TO                                 
119900                                      WS-KVPB-REF-RAD1                    
120000             END-IF                                                       
120100                                                                          
120200             COMPUTE LINK-KVBEHOV-DESSUTOM ROUNDED =                      
120300                         LINK-KVBEHOV-DESSUTOM + WS-KVPB-REF-RAD1         
120400                                                                          
120500          END-IF                                                          
120600        END-IF                                                            
120700                                                                          
120800        MOVE LINK-KVBEHOV-DESSUTOM  TO MOD-KVPB-PLAN(INDX)                
120900                                                                          
121000        MOVE +2                            TO INDX                        
121100        MOVE +1                            TO INDX1                       
121200        PERFORM UNTIL INDX                 >  MAX-INDX                    
121300           MOVE LINK-KVBEHOV-VECKA(INDX1)  TO MOD-KVPB-PLAN(INDX)         
121400           ADD +1                          TO INDX                        
121500                                              INDX1                       
121600        END-PERFORM                                                       
121700                                                                          
121800     ELSE                                                                 
121900        MOVE +1                     TO INDX                               
122000        PERFORM UNTIL INDX          >  MAX-INDX                           
122100           MOVE ZERO                TO MOD-KVPB-PLAN(INDX)                
122200           ADD +1                   TO INDX                               
122300        END-PERFORM                                                       
122400     END-IF                                                               
122500     .                                                                    
122600     EJECT                                                                
122700                                                                          
122800*START NEW**** CHINA EXPORT DEMAND 72428                                  
122900 FG-CALC-REFILL-CDC-DEMAND SECTION.                                       
123000     MOVE 'FG-CALC-REFILL-CDC-DEMAND'  TO CURRENT-SECTION                 
123100                                                                          
123200     PERFORM IMS-GNP-WDK611                                               
123300     IF SEGMENT-FOUND                                                     
123400       IF CLAG-IDDC-REF = W-IDDC                                          
123500         PERFORM FGA-CALC-CDC-DEMAND                                      
123600       ELSE                                                               
123700         MOVE +1                   TO INDX                                
123800         PERFORM UNTIL INDX > MAX-INDX                                    
123900           MOVE ZERO               TO MOD-KVPB-CDC(INDX)                  
124000           ADD +1                  TO INDX                                
124100         END-PERFORM                                                      
124200       END-IF                                                             
124300     ELSE                                                                 
124400       MOVE +1                     TO INDX                                
124500       PERFORM UNTIL INDX > MAX-INDX                                      
124600         MOVE ZERO                 TO MOD-KVPB-CDC(INDX)                  
124700         ADD +1                    TO INDX                                
124800       END-PERFORM                                                        
124900     END-IF                                                               
125000     .                                                                    
125100     EJECT                                                                
125200 FGA-CALC-CDC-DEMAND    SECTION.                                          
125300     MOVE 'FGA-CALC-CDC-DEMAND '   TO CURRENT-SECTION                     
125400                                                                          
125500     MOVE 13                    TO LINK-KVVECKOR-BEHOV                    
125600                                                                          
125700     MOVE ZERO                  TO LINK-KVTILLG-TOT-CDC                   
125800     MOVE W-IDARTNR             TO TILG-IDARTNR                           
125900     CALL W222TILG        USING TILG-W222TILG                             
126000                                TILG-WDK7-PCB TILG-WDL2-PCB               
126100                                TILG-WDB6-PCB TILG-WDD9-PCB               
126200                                TILG-WDK6-PCB TILG-WDK9-PCB               
126300                                                                          
126400     MOVE TILG-KVTILLG-TOT      TO LINK-KVTILLG-TOT-CDC                   
126500                                                                          
126600     MOVE ENDAST-CDCBEHOV       TO LINK-KDBEHOV                           
126700                                                                          
126800     IF ART-KDERS-UTG = 0                                                 
126900       CALL W222BHDC         USING LINK-AREA                              
127000                                   BHDC-WDK6-PCB                          
127100                                   BHDC-WDK7-1-PCB                        
127200                                   BHDC-WDB6-PCB                          
127300                                   BHDC-WDR2-PCB                          
127400                                   BHDC-WDD7-PCB                          
127500                                   BHDC-WDK7-2-PCB                        
127600                                   BHDC-WDD7-2-PCB                        
127700                                   BHDC-WDK9-PCB                          
127800                                   BHDC-REFL1-2501-PCB                    
127900                                   BHDC-REFL1-WDB6-PCB                    
128000                                   BHDC-REFL1-WDK7-PCB                    
128100                                   BHDC-REFL1-UTIL-WDK6-PCB               
128200                                   BHDC-REFL1-UTIL-WDK7-PCB               
128300                                   BHDC-REFL1-UTIL-WDB6-PCB               
128400                                   BHDC-REFL2-2501-PCB                    
128500                                   BHDC-REFL2-WDB6-PCB                    
128600                                   BHDC-REFL2-UTIL-WDK6-PCB               
128700                                   BHDC-REFL2-UTIL-WDK7-PCB               
128800                                   BHDC-REFL2-UTIL-WDB6-PCB               
128900                                   BHDC-UTIL-WDK6-PCB                     
129000                                   BHDC-UTIL-WDK7-PCB                     
129100                                   BHDC-UTIL-WDB6-PCB                     
129200                                   BHDC-W222-WDK6-PCB                     
129300                                   BHDC-W222-WDK7-PCB                     
129400                                   BHDC-W222-ARTM-PCB                     
129500                                   BHDC-W222-2501-PCB                     
129600                                   BHDC-W222-WDB6R-PCB                    
129700                                   BHDC-W222-WDK7R-PCB                    
129800                                   BHDC-W222-WDB6-PCB                     
129900                                   BHDC-W222-WDD7-PCB                     
130000                                   BHDC-W222-WDK7E-PCB                    
130100                                   BHDC-W222-UTIL-WDK6-PCB                
130200                                   BHDC-W222-UTIL-WDK7-PCB                
130300                                   BHDC-W222-UTIL-WDB6-PCB                
130400                                   BHDC-W222-UTUP-WDK7-PCB                
130500                                   BHDC-W222-UTUP-WDB6-PCB                
130600                                   BHDC-W222-UTUP-UTIL-WDK6-PCB           
130700                                   BHDC-W222-UTUP-UTIL-WDK7-PCB           
130800                                   BHDC-W222-UTUP-UTIL-WDB6-PCB           
130900                                   BHDC-UTUP-WDK7-PCB                     
131000                                   BHDC-UTUP-WDB6-PCB                     
131100                                   BHDC-UTUP-UTIL-WDK6-PCB                
131200                                   BHDC-UTUP-UTIL-WDK7-PCB                
131300                                   BHDC-UTUP-UTIL-WDB6-PCB                
131400                                                                          
131500                                                                          
131600       IF LINK-FLJANEJ-ANROP = 'N'                                        
131700         PERFORM S20-NOLLA-BHDC-RESULTATFLT                               
131800       END-IF                                                             
131900     ELSE                                                                 
132000       PERFORM S20-NOLLA-BHDC-RESULTATFLT                                 
132100     END-IF                                                               
132200                                                                          
132300*                                                                         
132400*KOD FRÅN FC-CALC-REFILL-DC-DEMAND SECTION FÖR SUMMERING.                 
132500                                                                          
132600     MOVE +1                           TO INDX                            
132700     MOVE LINK-KVBEHOV-DESSUTOM        TO MOD-KVPB-CDC(INDX)              
132800     ADD  LINK-KVBEHOV-DESSUTOM        TO W-KVPB-TOT(INDX)                
132900     MOVE +2                           TO INDX                            
133000     MOVE +1                           TO INDX1                           
133100     PERFORM UNTIL INDX               >  MAX-INDX                         
133200        MOVE LINK-KVBEHOV-VECKA(INDX1) TO MOD-KVPB-CDC(INDX)              
133300        ADD LINK-KVBEHOV-VECKA(INDX1)  TO W-KVPB-TOT(INDX)                
133400        ADD +1                         TO INDX                            
133500                                          INDX1                           
133600     END-PERFORM                                                          
133700                                                                          
133800     .                                                                    
133900     EJECT                                                                
134000                                                                          
134100*END   NEW**** CHINA EXPORT DEMAND 72428                                  
134200                                                                          
134300 FH-CALC-GLOBAL-DEMAND SECTION.                                           
134400     MOVE 'FH-CALC-GLOBAL-DEMAND' TO CURRENT-SECTION                      
134500                                                                          
134600     PERFORM IMS-GU-WDK701                                                
134700     IF SEGMENT-FOUND                                                     
134800        MOVE W-IDDC(1:1) TO W-IDDC-NON                                    
134900        MOVE W-IDDC      TO W-IDDC-REF                                    
135000                                                                          
135100        PERFORM IMS-GNP-WDK711-DCREF-GLOBAL                               
135200        IF SEGMENT-FOUND                                                  
135300           PERFORM FHA-CALC-DC-DEMAND                                     
135400        ELSE                                                              
135500           MOVE +1                    TO INDX                             
135600           PERFORM UNTIL INDX >  MAX-INDX                                 
135700             MOVE ZERO                TO MOD-KVPB-GLOBAL(INDX)            
135800             ADD +1                   TO INDX                             
135900           END-PERFORM                                                    
136000        END-IF                                                            
136100     END-IF                                                               
136200     .                                                                    
136300     EJECT                                                                
136400 FHA-CALC-DC-DEMAND SECTION.                                              
136500     MOVE ' FHA-CALC-DC-DEMAND '  TO CURRENT-SECTION                      
136600                                                                          
136700     MOVE 13                    TO LINK-KVVECKOR-BEHOV                    
136800     MOVE ZERO                  TO LINK-KVTILLG-TOT-CDC                   
136900     MOVE ENDAST-GLOBALBEHOV    TO LINK-KDBEHOV                           
137000                                                                          
137100     IF ART-KDERS-UTG = 0                                                 
137200       CALL W222BHDC         USING LINK-AREA                              
137300                                   BHDC-WDK6-PCB                          
137400                                   BHDC-WDK7-1-PCB                        
137500                                   BHDC-WDB6-PCB                          
137600                                   BHDC-WDR2-PCB                          
137700                                   BHDC-WDD7-PCB                          
137800                                   BHDC-WDK7-2-PCB                        
137900                                   BHDC-WDD7-2-PCB                        
138000                                   BHDC-WDK9-PCB                          
138100                                   BHDC-REFL1-2501-PCB                    
138200                                   BHDC-REFL1-WDB6-PCB                    
138300                                   BHDC-REFL1-WDK7-PCB                    
138400                                   BHDC-REFL1-UTIL-WDK6-PCB               
138500                                   BHDC-REFL1-UTIL-WDK7-PCB               
138600                                   BHDC-REFL1-UTIL-WDB6-PCB               
138700                                   BHDC-REFL2-2501-PCB                    
138800                                   BHDC-REFL2-WDB6-PCB                    
138900                                   BHDC-REFL2-UTIL-WDK6-PCB               
139000                                   BHDC-REFL2-UTIL-WDK7-PCB               
139100                                   BHDC-REFL2-UTIL-WDB6-PCB               
139200                                   BHDC-UTIL-WDK6-PCB                     
139300                                   BHDC-UTIL-WDK7-PCB                     
139400                                   BHDC-UTIL-WDB6-PCB                     
139500                                   BHDC-W222-WDK6-PCB                     
139600                                   BHDC-W222-WDK7-PCB                     
139700                                   BHDC-W222-ARTM-PCB                     
139800                                   BHDC-W222-2501-PCB                     
139900                                   BHDC-W222-WDB6R-PCB                    
140000                                   BHDC-W222-WDK7R-PCB                    
140100                                   BHDC-W222-WDB6-PCB                     
140200                                   BHDC-W222-WDD7-PCB                     
140300                                   BHDC-W222-WDK7E-PCB                    
140400                                   BHDC-W222-UTIL-WDK6-PCB                
140500                                   BHDC-W222-UTIL-WDK7-PCB                
140600                                   BHDC-W222-UTIL-WDB6-PCB                
140700                                   BHDC-W222-UTUP-WDK7-PCB                
140800                                   BHDC-W222-UTUP-WDB6-PCB                
140900                                   BHDC-W222-UTUP-UTIL-WDK6-PCB           
141000                                   BHDC-W222-UTUP-UTIL-WDK7-PCB           
141100                                   BHDC-W222-UTUP-UTIL-WDB6-PCB           
141200                                   BHDC-UTUP-WDK7-PCB                     
141300                                   BHDC-UTUP-WDB6-PCB                     
141400                                   BHDC-UTUP-UTIL-WDK6-PCB                
141500                                   BHDC-UTUP-UTIL-WDK7-PCB                
141600                                   BHDC-UTUP-UTIL-WDB6-PCB                
141700                                                                          
141800                                                                          
141900       IF LINK-FLJANEJ-ANROP = 'N'                                        
142000         PERFORM S20-NOLLA-BHDC-RESULTATFLT                               
142100       END-IF                                                             
142200     ELSE                                                                 
142300       PERFORM S20-NOLLA-BHDC-RESULTATFLT                                 
142400     END-IF                                                               
142500                                                                          
142600     MOVE +1                     TO INDX                                  
142700     MOVE LINK-KVBEHOV-DESSUTOM  TO MOD-KVPB-GLOBAL(INDX)                 
142800     ADD  LINK-KVBEHOV-DESSUTOM  TO W-KVPB-TOT(INDX)                      
142900     MOVE +2                            TO INDX                           
143000     MOVE +1                            TO INDX1                          
143100     PERFORM UNTIL INDX                 >  MAX-INDX                       
143200        MOVE LINK-KVBEHOV-VECKA(INDX1)  TO MOD-KVPB-GLOBAL(INDX)          
143300        ADD  LINK-KVBEHOV-VECKA(INDX1)  TO W-KVPB-TOT(INDX)               
143400        ADD +1                          TO INDX                           
143500                                           INDX1                          
143600     END-PERFORM                                                          
143700     .                                                                    
143800     EJECT                                                                
143900                                                                          
144000 S10-AUTH-USER-CHECK SECTION.                                             
144100     MOVE 'S10-AUTH-USER-CHECK '  TO CURRENT-SECTION                      
144200                                                                          
144300     PERFORM IMS-GU-WDK711                                                
144400     IF SEGMENT-FOUND                                                     
144500        MOVE SLAG-KVPB-REF         TO SPAR-KVPB-REF                       
144600        MOVE SLAG-IDLEVNR          TO WS-IDLEVNR-8                        
144700                                      IDLEVNR-WS                          
144800        IF MSGI-KDARBTYP-SEC-IDLEV = WS-IDLEVNR-8                         
144900        OR MSGI-KDARBTYP-SEC-IDLEV = SPACE OR LOW-VALUE                   
145000*          --- AUTHORIZED USER                                            
145100           MOVE YES                TO SECURITY-SW                         
145200           IF SLAG-IDDC-REF NOT = SPACES                                  
145300            MOVE NOO               TO INDATA-SW                           
145400            MOVE ERR-REFILL-PART   TO MED-IDMFSFEL                        
145500            CALL WMEDKONV       USING MED-WMEDAREA                        
145600            MOVE MED-MFSFEL        TO MOD-TEMFSFEL                        
145700           END-IF                                                         
145800        ELSE                                                              
145900           MOVE NOO                TO SECURITY-SW                         
146000           MOVE ERR-USER-NOT-AUTH  TO MED-IDMFSFEL                        
146100           CALL WMEDKONV USING MED-WMEDAREA                               
146200           MOVE MED-MFSFEL         TO MOD-TEMFSFEL                        
146300        END-IF                                                            
146400     ELSE                                                                 
146500        MOVE NOO                   TO INDATA-SW                           
146600        MOVE ERR-PART-MISSING      TO MED-IDMFSFEL                        
146700        CALL WMEDKONV USING MED-WMEDAREA                                  
146800        MOVE MED-MFSFEL            TO MOD-TEMFSFEL                        
146900     END-IF                                                               
147000     .                                                                    
147100     EJECT                                                                
147200 S20-NOLLA-BHDC-RESULTATFLT  SECTION.                                     
147300     MOVE 'S20-NOLLA-BHDC-RESULTATFLT '  TO CURRENT-SECTION               
147400                                                                          
147500     MOVE ZERO                   TO LINK-KVBEHOV-SUMMA                    
147600     MOVE ZERO                   TO LINK-KVBEHOV-DESSUTOM                 
147700     MOVE ZERO                   TO LINK-TIBEHOV-FIRST                    
147800                                                                          
147900     MOVE 1   TO IX                                                       
148000     PERFORM UNTIL IX > MAX-IX                                            
148100       MOVE ZERO                 TO LINK-KVBEHOV-VECKA(IX)                
148200                                                                          
148300       ADD 1  TO IX                                                       
148400     END-PERFORM                                                          
148500     .                                                                    
148600     EJECT                                                                
148700 MFS-ERASE-FIELD-OUT SECTION.                                             
148800                                                                          
148900     MOVE MFS-ERASE-FIELD TO MOD-IDARTNR-UT                               
149000                             MOD-IDDC-UT                                  
149100     PERFORM MFS-ERASE-FIELD-LINE-OUT                                     
149200     .                                                                    
149300     SKIP3                                                                
149400 MFS-ERASE-FIELD-LINE-OUT SECTION.                                        
149500                                                                          
149600     MOVE +1                 TO INDX                                      
149700     PERFORM UNTIL INDX > MAX-INDX                                        
149800        MOVE MFS-ERASE-FIELD TO MOD-TIAAVV(INDX)                          
149900                                MOD-KVPB-REF(INDX)                        
150000                                MOD-KVPB-REFILL(INDX)                     
150100                                MOD-KVPB-CDC(INDX)                        
150200                                MOD-KVPB-TREND(INDX)                      
150300                                MOD-KVPB-TOT(INDX)                        
150400                                MOD-KVPB-PLAN(INDX)                       
150500        ADD +1               TO INDX                                      
150600     END-PERFORM                                                          
150700     .                                                                    
150800     EJECT                                                                
150900 MFS-ERASE-FIELD-IN SECTION.                                              
151000                                                                          
151100     MOVE MFS-ERASE-FIELD TO MOD-IDARTNR-IN                               
151200                             MOD-IDDC-IN                                  
151300     .                                                                    
151400     EJECT                                                                
151500                                                                          
151600* --- IMS SECTIONS ---                                                    
151700                                                                          
151800 IMS-GET-MSG SECTION.                                                     
151900                                                                          
152000     MOVE '  QC' TO GOOD-STATUSCODES                                      
152100     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
152200     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
152300     PERFORM IMS-STATUSCHECK                                              
152400     .                                                                    
152500     SKIP3                                                                
152600 IMS-INSERT-MSG SECTION.                                                  
152700                                                                          
152800     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
152900     MOVE SPACE TO GOOD-STATUSCODES                                       
153000     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
153100     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
153200     PERFORM IMS-STATUSCHECK                                              
153300     .                                                                    
153400     EJECT                                                                
153500 IMS-GU-WDK601 SECTION.                                                   
153600     MOVE 'IMS-GU-WDK601 '  TO DBS-SECTION                                
153700                                                                          
153800     STRING 'WDK601  (IDARTNR  =' W-IDARTNR-X ')'                         
153900          DELIMITED BY SIZE INTO SSA1                                     
154000     MOVE '  GE' TO GOOD-STATUSCODES                                      
154100     CALL CBLTDLI USING GU WDK6-PCB DLI-IO-WDK601 SSA1                    
154200     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
154300     PERFORM IMS-STATUSCHECK                                              
154400     .                                                                    
154500     EJECT                                                                
154600 IMS-GNP-WDK611 SECTION.                                                  
154700     MOVE 'IMS-GNP-WDK611 '  TO DBS-SECTION                               
154800                                                                          
154900     MOVE SPACES                TO SSA1                                   
155000     MOVE 'WDK611 '             TO SSA1                                   
155100     MOVE '  GE' TO GOOD-STATUSCODES                                      
155200     CALL CBLTDLI USING GU WDK6-PCB DLI-IO-WDK611 SSA1                    
155300     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
155400     PERFORM IMS-STATUSCHECK                                              
155500     .                                                                    
155600     EJECT                                                                
155700 IMS-GU-WDK711 SECTION.                                                   
155800     MOVE 'IMS-GU-WDK711 '  TO DBS-SECTION                                
155900                                                                          
156000     MOVE SPACES    TO SSA1                                               
156100                       SSA2                                               
156200     STRING 'WDK701  (IDARTNR  =' W-IDARTNR-X ')'                         
156300            DELIMITED BY SIZE INTO SSA1                                   
156400     STRING 'WDK711  (IDDC     =' W-IDDC-X ')'                            
156500            DELIMITED BY SIZE INTO SSA2                                   
156600     MOVE '  GE' TO GOOD-STATUSCODES                                      
156700     CALL CBLTDLI USING GU WDK7-PCB DLI-IO-WDK711 SSA1 SSA2               
156800     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
156900     PERFORM IMS-STATUSCHECK                                              
157000     .                                                                    
157100     EJECT                                                                
157200 IMS-GU-WDK701 SECTION.                                                   
157300     MOVE 'IMS-GU-WDK701 '  TO DBS-SECTION                                
157400                                                                          
157500     MOVE SPACES              TO SSA1                                     
157600     STRING 'WDK701  (IDARTNR  =' W-IDARTNR-X ')'                         
157700          DELIMITED BY SIZE INTO SSA1                                     
157800     MOVE '    ' TO GOOD-STATUSCODES                                      
157900     CALL CBLTDLI USING GU WDK7-PCB DLI-IO-WDK701 SSA1                    
158000     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
158100     PERFORM IMS-STATUSCHECK                                              
158200     .                                                                    
158300     EJECT                                                                
158400 IMS-GNP-WDK711-IDDCREF SECTION.                                          
158500     MOVE 'IMS-GNP-WDK711-IDDCREF '  TO DBS-SECTION                       
158600                                                                          
158700     MOVE SPACES              TO SSA1                                     
158800     STRING 'WDK711  (IDDC    >=' W-IDDC-MIN-X                            
158900                    '&IDDC    <=' W-IDDC-MAX-X                            
159000                    '&IDDCREF  =' W-IDDC-X ')'                            
159100          DELIMITED BY SIZE INTO SSA1                                     
159200     MOVE '  GEGB' TO GOOD-STATUSCODES                                    
159300     CALL CBLTDLI USING GNP WDK7-PCB DLI-IO-WDK711 SSA1                   
159400     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
159500     PERFORM IMS-STATUSCHECK                                              
159600     .                                                                    
159700     EJECT                                                                
159800 IMS-GNP-WDK711-DCREF-GLOBAL SECTION.                                     
159900     MOVE 'IMS-GNP-WDK711-DCREF-GLOBAL '  TO DBS-SECTION                  
160000                                                                          
160100     MOVE SPACES              TO SSA1                                     
160200     STRING 'WDK711  (IDDC1   NE' W-IDDC-NON-X                            
160300                    '&IDDCREF  =' W-IDDC-REF-X ')'                        
160400          DELIMITED BY SIZE INTO SSA1                                     
160500     MOVE '  GEGB' TO GOOD-STATUSCODES                                    
160600     CALL CBLTDLI USING GNP WDK7-PCB DLI-IO-WDK711 SSA1                   
160700     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
160800     PERFORM IMS-STATUSCHECK                                              
160900     .                                                                    
161000     EJECT                                                                
161100 IMS-GU-WDK712 SECTION.                                                   
161200     MOVE 'IMS-GU-WDK712 '  TO DBS-SECTION                                
161300                                                                          
161400     MOVE SPACES              TO SSA1                                     
161500                                 SSA2                                     
161600     STRING 'WDK701  (IDARTNR  =' W-IDARTNR-X ')'                         
161700          DELIMITED BY SIZE INTO SSA1                                     
161800     STRING 'WDK712  (IDLAND   =' W-IDLAND-X ')'                          
161900          DELIMITED BY SIZE INTO SSA2                                     
162000     MOVE '    ' TO GOOD-STATUSCODES                                      
162100     CALL CBLTDLI USING GU WDK7-PCB DLI-IO-WDK712 SSA1 SSA2               
162200     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
162300     PERFORM IMS-STATUSCHECK                                              
162400     .                                                                    
162500     EJECT                                                                
162600 IMS-GU-WDK722 SECTION.                                                   
162700     MOVE 'IMS-GU-WDK722 '  TO DBS-SECTION                                
162800                                                                          
162900     MOVE SPACES              TO SSA1                                     
163000                                 SSA2                                     
163100                                 SSA3                                     
163200     STRING 'WDK701  (IDARTNR  =' W-IDARTNR-X ')'                         
163300          DELIMITED BY SIZE INTO SSA1                                     
163400     STRING 'WDK711  (IDDC     =' W-IDDC-X ')'                            
163500          DELIMITED BY SIZE INTO SSA2                                     
163600     STRING 'WDK722  (KDSEGKEY =' W-KDSEGKEY-K722-X ')'                   
163700          DELIMITED BY SIZE INTO SSA3                                     
163800     MOVE '  GE'              TO GOOD-STATUSCODES                         
163900     CALL CBLTDLI USING GU WDK7-PCB DLI-IO-WDK722 SSA1 SSA2 SSA3          
164000     MOVE WDK7-STATUS-CODE    TO STATUS-WS                                
164100     PERFORM IMS-STATUSCHECK                                              
164200     .                                                                    
164300     EJECT                                                                
164400 IMS-GU-WDB601 SECTION.                                                   
164500     MOVE 'IMS-GU-WDB601 '  TO DBS-SECTION                                
164600                                                                          
164700     STRING 'WDB601  (IDDC     =' W-IDDC-X ')'                            
164800          DELIMITED BY SIZE INTO SSA1                                     
164900     MOVE '  GE' TO GOOD-STATUSCODES                                      
165000     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-WDB601 SSA1                    
165100     MOVE WDB6-STATUS-CODE TO STATUS-WS                                   
165200     PERFORM IMS-STATUSCHECK                                              
165300     .                                                                    
165400     EJECT                                                                
165500 IMS-STATUSCHECK SECTION.                                                 
165600                                                                          
165700     SET STATUS-IX TO 1                                                   
165800     SEARCH GOOD-STATUS                                                   
165900       AT END                                                             
166000         STRING ' INVALID STATUS CODE FROM IMS: ' STATUS-WS               
166100         DELIMITED BY SIZE INTO ERROR-TEXT                                
166200         CALL FELLOG                                                      
166300       WHEN GOOD-STATUS (STATUS-IX) = STATUS-WS                           
166400         CONTINUE                                                         
166500     END-SEARCH                                                           
166600     .                                                                    
