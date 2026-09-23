000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W2219B00.                                                
000300 AUTHOR.         STENING INGER.                                           
000400 DATE-WRITTEN.   15/08/26.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*    FUNCTION:                                                            
000800*    CREATE/DELETE A ALERT ON SCREEN 2471 WHEN TOTAL DEMAND               
000900*    WITHIN ANY WEEK DURING CURRENT WEEK + LEADTIME FOR THE MAIN          
001000*    SUPPLIER IS LARGER THAN THE TOTAL ASSETS ON THE DC UP                
001100*    UNTIL THAT WEEK.                                                     
001200*                                                                         
001300*                                                                         
001400*    ABENDCODES:                                                          
001500*        U0016 -  . . . .                                                 
001600*        U1000 -  . . . .                                                 
001700*                                                                         
001800                                                                          
001900     SKIP3                                                                
002000 ENVIRONMENT DIVISION.                                                    
002100     SKIP2                                                                
002200 INPUT-OUTPUT SECTION.                                                    
002300                                                                          
002400 FILE-CONTROL.                                                            
002500                                                                          
002600*          --- PART FROM ALERT FILE - DELETE                              
002700     SELECT W22191                     ASSIGN TO W2219BD1.                
002800*          --- PART FILE WDK7 AND WDK6                                    
002900     SELECT W22418                     ASSIGN TO W2219BD2.                
003000*          --- OUTPUT FILE TO UPDATE WDGX2223/24                          
003100     SELECT W2219B01                   ASSIGN TO W2219BD3.                
003200*          --- OUTPUT FILE TO DELETE WDGX2223/24                          
003300     SELECT W2219B02                   ASSIGN TO W2219BD4.                
003400*          --- OUTPUT FILE - KONTROLL FILE INTERNT                        
003500     SELECT W2219B03                   ASSIGN TO W2219BD5.                
003600     EJECT                                                                
003700 DATA DIVISION.                                                           
003800     SKIP2                                                                
003900 FILE SECTION.                                                            
004000     SKIP3                                                                
004100 FD  W22191                                                               
004200     RECORDING       F                                                    
004300     BLOCK CONTAINS  0.                                                   
004400                                                                          
004500*01  RECORD -COPY W22418   -PRE  IN1- -L.                                 
004600     EJECT                                                                
004700 FD  W22418                                                               
004800     RECORDING       F                                                    
004900     BLOCK CONTAINS  0.                                                   
005000                                                                          
005100*01  RECORD -COPY W22418   -PRE  IN-  -L.                                 
005200     EJECT                                                                
005300 FD  W2219B01                                                             
005400     RECORDING       F                                                    
005500     BLOCK CONTAINS  0.                                                   
005600                                                                          
005700*01  RECORD -COPY W2219401 -PRE  OUT1-  -L.                               
005800     EJECT                                                                
005900 FD  W2219B02                                                             
006000     RECORDING       F                                                    
006100     BLOCK CONTAINS  0.                                                   
006200                                                                          
006300*01  RECORD -COPY W2219401 -PRE  OUT2-  -L.                               
006400     EJECT                                                                
006500 FD  W2219B03                                                             
006600     RECORDING       F                                                    
006700     BLOCK CONTAINS  0.                                                   
006800                                                                          
006900*01  RECORD -COPY W2219402 -PRE  OUT3-  -L.                               
007000     EJECT                                                                
007100 WORKING-STORAGE SECTION.                                                 
007200*    -- CHECKED BY WY2000                                                 
007300     SKIP3                                                                
007400*    -COPY WY2000W3                                                       
007500                                                                          
007600 77  IDPGM                       PIC X(8)    VALUE 'W2219B00'.            
007700 77  CURRENT-SECTION             PIC X(80)   VALUE SPACE.                 
007800 77  DBS-SECTION                 PIC X(80)   VALUE SPACE.                 
007900 77  JA                          PIC X       VALUE 'J'.                   
008000 77  YES                         PIC X       VALUE 'Y'.                   
008100 77  NOO                         PIC X       VALUE 'N'.                   
008200 77  W222-IX                     PIC S9(4)   COMP SYNC VALUE ZERO.        
008300 77  TAB-IX                      PIC S9(4)   COMP SYNC VALUE ZERO.        
008400 77  TAB-IX-MAX                  PIC S9(4)   COMP SYNC VALUE +156.        
008500 77  TAB-IX-MAX-30               PIC S9(4)   COMP SYNC VALUE +30.         
008600                                                                          
008700 77  W-TILLG-BEHOV               PIC S9(07)V9(02) VALUE +0.               
008800 77  W-KVPB-REF                  PIC 9(6)V9(2) VALUE ZERO COMP-3.         
008900 77  W-KVAVROP-OLD               PIC S9(07)    VALUE ZERO COMP-3.         
009000 77  W-KVDAGAR-KVAR              PIC 9(3)      VALUE ZERO COMP-3.         
009100 77  W-VECKO-SEP-BEHOV          PIC S9(7)V9(2) VALUE ZERO COMP-3.         
009200 77  W-DAG-SEP-BEHOV            PIC S9(7)V9(2) VALUE ZERO COMP-3.         
009300 77  W-TIME                      PIC 9(8)    VALUE ZERO.                  
009400 77  W-TIFINLV-AAVV              PIC S9(5)               COMP-3.          
009500 77  W-DELETE-ALERT              PIC X(1)    VALUE SPACE.                 
009600 77  W-CREATE-ALERT              PIC X(1)    VALUE SPACE.                 
009700 77  W-CREATE-ALERT-FIRST-TIME   PIC X(1)    VALUE SPACE.                 
009800 77  W-GET-WDD924                PIC X(1)    VALUE 'N'.                   
009900 77  W-TILEVBSK-DISP-YYWW-LAST   PIC 9(04)   VALUE ZERO.                  
010000 77  W-NDCCN-KVVECKOR-LT         PIC S9(3)   COMP-3 VALUE +0.             
010100 77  W-TIBEHOV-FIRST             PIC 9(04)   VALUE ZERO.                  
010200                                                                          
010300 77  PB-TOTAL-SEP-LEV-XDC        PIC X(2)    VALUE '03'.                  
010400                                                                          
010500 01  W-YYWWD.                                                             
010600     03  W-YYWWD-NUM             PIC 9(05).                               
010700     03  W-YYWWD REDEFINES W-YYWWD-NUM.                                   
010800         05 W-YYWW               PIC 9(04).                               
010900         05 W-TODAYS-DAGNR       PIC 9(01).                               
011000                                                                          
011100 77  W22191-EOF-SW               PIC X       VALUE 'N'.                   
011200     88  END-OF-W22191                       VALUE 'J'.                   
011300                                                                          
011400 77  W22418-EOF-SW               PIC X       VALUE 'N'.                   
011500     88  END-OF-W22418                       VALUE 'J'.                   
011600                                                                          
011700 01  TAB-BEHOV-X.                                                         
011800    05  TAB-BEHOV OCCURS 156.                                             
011900        10 TAB-YYWW              PIC 9(04).                               
012000        10 FILLER REDEFINES TAB-YYWW.                                     
012100           15 TAB-YYWW-YY        PIC 9(02).                               
012200           15 TAB-YYWW-WW        PIC 9(02).                               
012300        10 TAB-KVBEHOV           PIC S9(7)V9(2) COMP-3.                   
012400        10 TAB-KVAVROP           PIC S9(07).                              
012500        10 TAB-SUDISPV           PIC S9(09)V9(2) COMP-3.                  
012600        10 TAB-FLLARM            PIC X(01).                               
012700                                                                          
012800 01  W-KVDISP                    PIC S9(09) VALUE +0.                     
012900 01  W-KVTILLG                   PIC S9(09)V9(02) VALUE +0.               
013000 01  W-KVAVIS-KVRAPP             PIC S9(09) VALUE +0.                     
013100 01  W-TOT-KVRAPP                PIC S9(09)       VALUE +0.               
013200     EJECT                                                                
013300*    --- VALID IDDC CODES                                                 
013400*01  -COPY WWDC99                                                         
013500     EJECT                                                                
013600 01  TODAYS-DATE                 PIC 9(06).                               
013700                                                                          
013800 01  TODAYS-DATE-YYWWD.                                                   
013900     03  TODAYS-YEAR-WEEK        PIC 9(04).                               
014000     03  FILLER REDEFINES TODAYS-YEAR-WEEK.                               
014100         05  TODAYS-YEAR         PIC 9(2).                                
014200         05  TODAYS-WEEK         PIC 9(2).                                
014300     03  TODAYS-DAGNR            PIC 9(1).                                
014400     EJECT                                                                
014500 01  GENERAL-SUBPROGRAMS.                                                 
014600*                                                                         
014700     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
014800     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
014900     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
015000     03  DATKORT                 PIC X(8)    VALUE 'DATKORT'.             
015100     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
015200     03  W222BHDC                PIC X(8)    VALUE 'W222BHDC'.            
015300     03  W009VADD                PIC X(8)    VALUE 'W009VADD'.            
015400     03  WDATKONV                PIC X(8)    VALUE 'WDATKONV'.            
015500     EJECT                                                                
015600*    --- PARAMETERS FOR SUB PROGRAM W222BHDC                              
015700 01  FILLER                      PIC X(16)   VALUE 'W222BHDC'.            
015800     SKIP3                                                                
015900*01  -COPY W222BHDC -PRE BHDC-                                            
016000     EJECT                                                                
016100*    --- PARAMETERS FOR SUBPROGRAM ABEND                                  
016200                                                                          
016300 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
016400 77  RKOD-ABEND-NO-DUMP          PIC S9(4)   COMP VALUE +16.              
016500 77  RKOD-ABEND-WITH-DUMP        PIC S9(4)   COMP VALUE +1000.            
016600     SKIP2                                                                
016700 01  ERROR-TEXT.                                                          
016800     03  FILLER                  PIC X(8)    VALUE 'ERR-TEXT'.            
016900     03  ERROR-TEXT-STR          PIC X(72)   VALUE SPACE.                 
017000     EJECT                                                                
017100*    --- PARAMETRAR TILL DATKORT                                          
017200*                                                                         
017300 01  PROGRAM-NAMN                PIC X(6)    VALUE 'W2219B'.              
017400     SKIP2                                                                
017500 01  DATUMKORT-ID                PIC X(6)    VALUE 'WDATUM'.              
017600     SKIP2                                                                
017700*01  -COPY WDATKORT                                                       
017800     EJECT                                                                
017900*01  -COPY WDATAREA                                                       
018000     EJECT                                                                
018100 01  W22418-AREA-START           PIC X(24)   VALUE                        
018200                                 'W22418-AREA-START  '.                   
018300                                                                          
018400*01  AREA -COPY W22418     -PRE IN-                                       
018500     EJECT                                                                
018600*    --- PARAMETRAR TILL POSTSUM                                          
018700*                                                                         
018800*01  -COPY W0005   -PRE  POSTSUM-                                         
018900     EJECT                                                                
019000* VARIABLES TO SUBPROGRAM W009VADD                                        
019100 01  W009-TIBEHOV-START          PIC S9(5)  COMP-3.                       
019200 01  W009-ANTAL-VECKOR           PIC S9(3)  COMP-3.                       
019300     EJECT                                                                
019400                                                                          
019500 01  WORK-AREA-START           PIC X(24)  VALUE 'WORK-AREA-START'.        
019600*01  -COPY W2219401     -PRE WORK-                                        
019700     EJECT                                                                
019800 01  OUT3-AREA-START           PIC X(24)  VALUE 'OUT3-AREA-START'.        
019900*01  -COPY W2219402     -PRE OUT3-                                        
020000     EJECT                                                                
020100                                                                          
020200*    --- AREAS FOR IMS-SECTIONS                                           
020300*                                                                         
020400     EJECT                                                                
020500 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
020600                                                                          
020700 01  KEYS-FOR-DLI.                                                        
020800     03  W-IDARTNR-X.                                                     
020900         05  W-IDARTNR           PIC S9(9) VALUE ZERO COMP-3.             
021000                                                                          
021100     03  W-IDLEVNR-X.                                                     
021200         05  W-IDLEVNR           PIC X(5)    VALUE SPACE.                 
021300                                                                          
021400     03  W-WDD901KY-X.                                                    
021500         05  W-IDARTNR-D9         PIC S9(9)  VALUE ZERO COMP-3.           
021600         05  W-IDDC-D9            PIC X(2)   VALUE SPACE.                 
021700                                                                          
021800     03  W-WDGX2231-X.                                                    
021900         05  W-IDHTYP-2231       PIC X(4)     VALUE '2231'.               
022000         05  FILLER              PIC X(26)    VALUE LOW-VALUE.            
022100                                                                          
022200     03  W-WDGX2232-X.                                                    
022300         05  W-IDANSK-2232       PIC S9(3)    VALUE ZERO COMP-3.          
022400         05  FILLER              PIC X(3)     VALUE LOW-VALUE.            
022500                                                                          
022600      03 W-DAINLEV-X.                                                     
022700         05  W-DAINLEV           PIC 9(16).                               
022800                                                                          
022900      03  W-IDPTYP-X.                                                     
023000         05  W-IDPTYP            PIC X(3)   VALUE '310'.                  
023100                                                                          
023200      03 W-KDAVROP-X.                                                     
023300         05  W-KDAVROP           PIC S9(1)   COMP-3 VALUE +2.             
023400                                                                          
023500     SKIP2                                                                
023600 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
023700*    --- STATUS-KOD FRÅN IMS                                              
023800 01  STATUS-WS                   PIC XX.                                  
023900     88  SEGMENT-FOUND                       VALUE '  '.                  
024000     88  SEGMENT-FOUND-EXISTS                VALUE 'II'.                  
024100     88  SEGMENT-MISSING                     VALUE 'GE'.                  
024200     88  SEGMENT-END                         VALUE 'GB'.                  
024300     SKIP2                                                                
024400 01  GOOD-STATUSCODES.                                                    
024500     03  GOOD-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
024600     SKIP3                                                                
024700 01  SSA1                        PIC X(64).                               
024800 01  SSA2                        PIC X(64).                               
024900 01  SSA3                        PIC X(64).                               
025000     EJECT                                                                
025100*    --- IMS FUNCTION CODES                                               
025200*01  -COPY W0003                                                          
025300     EJECT                                                                
025400*    ---  DLI INPUT-OUTPUT AREA                                           
025500 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDGX2232'.                    
025600 01  DLI-IO-WDGX2232.                                                     
025700*    03  -COPY WDGX2232                                                   
025800     EJECT                                                                
025900 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDD901'.                      
026000 01  DLI-IO-WDD901.                                                       
026100*    03  -COPY WDD901 -PRE WDD901-                                        
026200     EJECT                                                                
026300 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDD905'.                      
026400 01  DLI-IO-WDD905.                                                       
026500*    03  -COPY WDD905 -PRE WDD905-                                        
026600     EJECT                                                                
026700 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDD924'.                      
026800 01  DLI-IO-WDD924.                                                       
026900*    03  -COPY WDD924 -PRE WDD924-                                        
027000     EJECT                                                                
027100 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDL601'.                      
027200 01  DLI-IO-WDL601.                                                       
027300*    03  -COPY WDL601 -PRE L601-                                          
027400                                                                          
027500 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDL611'.                      
027600 01  DLI-IO-WDL611.                                                       
027700*    03  -COPY WDL611                                                     
027800                                                                          
027900 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDL621'.                      
028000 01  DLI-IO-WDL621.                                                       
028100*    03  -COPY WDL621                                                     
028200                                                                          
028300 LINKAGE SECTION.                                                         
028400                                                                          
028500*01  -COPY W0008  -PRE WDD9-                                              
028600     05  FILLER                   PIC X.                                  
028700     EJECT                                                                
028800*01  -COPY W0008  -PRE WDR2-                                              
028900     05  FILLER                   PIC X.                                  
029000     EJECT                                                                
029100*01  -COPY W0008  -PRE WDL6-                                              
029200     05  FILLER                   PIC X.                                  
029300     EJECT                                                                
029400*-BEHOVSMODULENS PCB:ER i W222BHDC                                        
029500 01  BHDC-WDK6-PCB                PIC X.                                  
029600 01  BHDC-WDK7-PCB                PIC X.                                  
029700 01  BHDC-WDB6-PCB                PIC X.                                  
029800 01  BHDC-WDR2-PCB                PIC X.                                  
029900 01  BHDC-WDD7A-PCB               PIC X.                                  
030000 01  BHDC-WDK7E-PCB               PIC X.                                  
030100 01  BHDC-WDD7-PCB                PIC X.                                  
030200 01  BHDC-WDK9-PCB                PIC X.                                  
030300 01  BHDC-REFL1-2501-PCB          PIC X.                                  
030400 01  BHDC-REFL1-WDB6-PCB          PIC X.                                  
030500 01  BHDC-REFL1-WDK7-PCB          PIC X.                                  
030600 01  BHDC-REFL1-UTIL-WDK6-PCB     PIC X.                                  
030700 01  BHDC-REFL1-UTIL-WDK7-PCB     PIC X.                                  
030800 01  BHDC-REFL1-UTIL-WDB6-PCB     PIC X.                                  
030900     EJECT                                                                
031000 01  BHDC-REFL2-2501-PCB          PIC X.                                  
031100 01  BHDC-REFL2-WDB6-PCB          PIC X.                                  
031200 01  BHDC-REFL2-UTIL-WDK6-PCB     PIC X.                                  
031300 01  BHDC-REFL2-UTIL-WDK7-PCB     PIC X.                                  
031400 01  BHDC-REFL2-UTIL-WDB6-PCB     PIC X.                                  
031500     EJECT                                                                
031600 01  BHDC-UTIL-WDK6-PCB           PIC X.                                  
031700 01  BHDC-UTIL-WDK7-PCB           PIC X.                                  
031800 01  BHDC-UTIL-WDB6-PCB           PIC X.                                  
031900     EJECT                                                                
032000 01  BHDC-W222-WDK6-PCB           PIC X.                                  
032100 01  BHDC-W222-WDK7-PCB           PIC X.                                  
032200 01  BHDC-W222-ARTM-PCB           PIC X.                                  
032300 01  BHDC-W222-2501-PCB           PIC X.                                  
032400 01  BHDC-W222-WDB6R-PCB          PIC X.                                  
032500 01  BHDC-W222-WDK7R-PCB          PIC X.                                  
032600 01  BHDC-W222-WDB6-PCB           PIC X.                                  
032700 01  BHDC-W222-WDD7-PCB           PIC X.                                  
032800 01  BHDC-W222-WDK7E-PCB          PIC X.                                  
032900 01  BHDC-W222-UTIL-WDK6-PCB      PIC X.                                  
033000 01  BHDC-W222-UTIL-WDK7-PCB      PIC X.                                  
033100 01  BHDC-W222-UTIL-WDB6-PCB      PIC X.                                  
033200 01  BHDC-W222-UTUP-WDK7-PCB      PIC X.                                  
033300 01  BHDC-W222-UTUP-WDB6-PCB      PIC X.                                  
033400 01  BHDC-W222-UTUP-UTIL-WDK6-PCB PIC X.                                  
033500 01  BHDC-W222-UTUP-UTIL-WDK7-PCB PIC X.                                  
033600 01  BHDC-W222-UTUP-UTIL-WDB6-PCB PIC X.                                  
033700     EJECT                                                                
033800 01  BHDC-UTUP-WDK7-PCB           PIC X.                                  
033900 01  BHDC-UTUP-WDB6-PCB           PIC X.                                  
034000 01  BHDC-UTUP-UTIL-WDK6-PCB      PIC X.                                  
034100 01  BHDC-UTUP-UTIL-WDK7-PCB      PIC X.                                  
034200 01  BHDC-UTUP-UTIL-WDB6-PCB      PIC X.                                  
034300     EJECT                                                                
034400                                                                          
034500 PROCEDURE DIVISION  USING WDD9-PCB WDR2-PCB WDL6-PCB                     
034600                           BHDC-WDK6-PCB  BHDC-WDK7-PCB                   
034700                           BHDC-WDB6-PCB  BHDC-WDR2-PCB                   
034800                           BHDC-WDD7A-PCB BHDC-WDK7E-PCB                  
034900                           BHDC-WDD7-PCB  BHDC-WDK9-PCB                   
035000                           BHDC-REFL1-2501-PCB                            
035100                           BHDC-REFL1-WDB6-PCB                            
035200                           BHDC-REFL1-WDK7-PCB                            
035300                           BHDC-REFL1-UTIL-WDK6-PCB                       
035400                           BHDC-REFL1-UTIL-WDK7-PCB                       
035500                           BHDC-REFL1-UTIL-WDB6-PCB                       
035600                           BHDC-REFL2-2501-PCB                            
035700                           BHDC-REFL2-WDB6-PCB                            
035800                           BHDC-REFL2-UTIL-WDK6-PCB                       
035900                           BHDC-REFL2-UTIL-WDK7-PCB                       
036000                           BHDC-REFL2-UTIL-WDB6-PCB                       
036100                           BHDC-UTIL-WDK6-PCB                             
036200                           BHDC-UTIL-WDK7-PCB                             
036300                           BHDC-UTIL-WDB6-PCB                             
036400                           BHDC-W222-WDK6-PCB                             
036500                           BHDC-W222-WDK7-PCB                             
036600                           BHDC-W222-ARTM-PCB                             
036700                           BHDC-W222-2501-PCB                             
036800                           BHDC-W222-WDB6R-PCB                            
036900                           BHDC-W222-WDK7R-PCB                            
037000                           BHDC-W222-WDB6-PCB                             
037100                           BHDC-W222-WDD7-PCB                             
037200                           BHDC-W222-WDK7E-PCB                            
037300                           BHDC-W222-UTIL-WDK6-PCB                        
037400                           BHDC-W222-UTIL-WDK7-PCB                        
037500                           BHDC-W222-UTIL-WDB6-PCB                        
037600                           BHDC-W222-UTUP-WDK7-PCB                        
037700                           BHDC-W222-UTUP-WDB6-PCB                        
037800                           BHDC-W222-UTUP-UTIL-WDK6-PCB                   
037900                           BHDC-W222-UTUP-UTIL-WDK7-PCB                   
038000                           BHDC-W222-UTUP-UTIL-WDB6-PCB                   
038100                           BHDC-UTUP-WDK7-PCB                             
038200                           BHDC-UTUP-WDB6-PCB                             
038300                           BHDC-UTUP-UTIL-WDK6-PCB                        
038400                           BHDC-UTUP-UTIL-WDK7-PCB                        
038500                           BHDC-UTUP-UTIL-WDB6-PCB                        
038600                           .                                              
038700 MAIN SECTION.                                                            
038800     ENTRY 'DLITCBL' USING WDD9-PCB WDR2-PCB WDL6-PCB                     
038900                           BHDC-WDK6-PCB  BHDC-WDK7-PCB                   
039000                           BHDC-WDB6-PCB  BHDC-WDR2-PCB                   
039100                           BHDC-WDD7A-PCB BHDC-WDK7E-PCB                  
039200                           BHDC-WDD7-PCB  BHDC-WDK9-PCB                   
039300                           BHDC-REFL1-2501-PCB                            
039400                           BHDC-REFL1-WDB6-PCB                            
039500                           BHDC-REFL1-WDK7-PCB                            
039600                           BHDC-REFL1-UTIL-WDK6-PCB                       
039700                           BHDC-REFL1-UTIL-WDK7-PCB                       
039800                           BHDC-REFL1-UTIL-WDB6-PCB                       
039900                           BHDC-REFL2-2501-PCB                            
040000                           BHDC-REFL2-WDB6-PCB                            
040100                           BHDC-REFL2-UTIL-WDK6-PCB                       
040200                           BHDC-REFL2-UTIL-WDK7-PCB                       
040300                           BHDC-REFL2-UTIL-WDB6-PCB                       
040400                           BHDC-UTIL-WDK6-PCB                             
040500                           BHDC-UTIL-WDK7-PCB                             
040600                           BHDC-UTIL-WDB6-PCB                             
040700                           BHDC-W222-WDK6-PCB                             
040800                           BHDC-W222-WDK7-PCB                             
040900                           BHDC-W222-ARTM-PCB                             
041000                           BHDC-W222-2501-PCB                             
041100                           BHDC-W222-WDB6R-PCB                            
041200                           BHDC-W222-WDK7R-PCB                            
041300                           BHDC-W222-WDB6-PCB                             
041400                           BHDC-W222-WDD7-PCB                             
041500                           BHDC-W222-WDK7E-PCB                            
041600                           BHDC-W222-UTIL-WDK6-PCB                        
041700                           BHDC-W222-UTIL-WDK7-PCB                        
041800                           BHDC-W222-UTIL-WDB6-PCB                        
041900                           BHDC-W222-UTUP-WDK7-PCB                        
042000                           BHDC-W222-UTUP-WDB6-PCB                        
042100                           BHDC-W222-UTUP-UTIL-WDK6-PCB                   
042200                           BHDC-W222-UTUP-UTIL-WDK7-PCB                   
042300                           BHDC-W222-UTUP-UTIL-WDB6-PCB                   
042400                           BHDC-UTUP-WDK7-PCB                             
042500                           BHDC-UTUP-WDB6-PCB                             
042600                           BHDC-UTUP-UTIL-WDK6-PCB                        
042700                           BHDC-UTUP-UTIL-WDK7-PCB                        
042800                           BHDC-UTUP-UTIL-WDB6-PCB                        
042900                           .                                              
043000                                                                          
043100     PERFORM A-INIT                                                       
043200                                                                          
043300* DELETE ALERT                                                            
043400     PERFORM S02-INIT-TAB-BEHOV                                           
043500                                                                          
043600     PERFORM S01-LAES-W22191                                              
043700     PERFORM UNTIL END-OF-W22191                                          
043800                                                                          
043900        PERFORM C-CREATE-ALERT                                            
044000                                                                          
044100        MOVE YES           TO W-DELETE-ALERT                              
044200        PERFORM D-WRITE-W2219B                                            
044300                                                                          
044400        PERFORM S02-INIT-TAB-BEHOV                                        
044500        MOVE +0            TO W-KVTILLG                                   
044600        MOVE +0            TO W-KVAVIS-KVRAPP                             
044700        MOVE +0            TO W-KVDISP                                    
044800                                                                          
044900        PERFORM S01-LAES-W22191                                           
045000     END-PERFORM                                                          
045100                                                                          
045200* INSERT/REPLACE ALERT                                                    
045300     PERFORM S02-INIT-TAB-BEHOV                                           
045400                                                                          
045500     PERFORM S01-LAES-W22418                                              
045600     PERFORM UNTIL END-OF-W22418                                          
045700                                                                          
045800        IF IN-NDCCN-KDERS         > +06                                   
045900        OR IN-NDCCN-IDLEVNR       = '9998'                                
046000        OR IN-NDCCN-REDIRLEV      = 1.0                                   
046100        OR IN-NDCCN-TISTODAT-LARM > TODAYS-DATE                           
046200        OR IN-NDCCN-TISTODAT-LARM = 999999                                
046300           CONTINUE                                                       
046400        ELSE                                                              
046500           PERFORM C-CREATE-ALERT                                         
046600        END-IF                                                            
046700                                                                          
046800        MOVE NOO           TO W-DELETE-ALERT                              
046900        PERFORM D-WRITE-W2219B                                            
047000                                                                          
047100        PERFORM S02-INIT-TAB-BEHOV                                        
047200        MOVE +0            TO W-KVTILLG                                   
047300        MOVE +0            TO W-KVAVIS-KVRAPP                             
047400        MOVE +0            TO W-KVDISP                                    
047500                                                                          
047600        PERFORM S01-LAES-W22418                                           
047700     END-PERFORM                                                          
047800                                                                          
047900     PERFORM Z-FINIT                                                      
048000                                                                          
048100     MOVE ZERO TO RETURN-CODE                                             
048200     GOBACK                                                               
048300     .                                                                    
048400     EJECT                                                                
048500 A-INIT SECTION.                                                          
048600                                                                          
048700     OPEN INPUT  W22418                                                   
048800                 W22191                                                   
048900          OUTPUT W2219B01                                                 
049000                 W2219B02                                                 
049100                 W2219B03                                                 
049200                                                                          
049300     ACCEPT TODAYS-DATE FROM DATE                                         
049400                                                                          
049500     CALL DATKORT USING PROGRAM-NAMN DATUMKORT-ID DATUMKORT               
049600     MOVE D-AAR            TO TODAYS-YEAR                                 
049700     MOVE D-VECKA          TO TODAYS-WEEK                                 
049800     MOVE D-DAGNR          TO TODAYS-DAGNR                                
049900                                                                          
050000     MOVE IDPGM            TO POSTSUM-PROGNAMN                            
050100     .                                                                    
050200     EJECT                                                                
050300 C-CREATE-ALERT SECTION.                                                  
050400     MOVE 'C-CREATE-ALERT      ' TO CURRENT-SECTION                       
050500                                                                          
050600*    INPUT FILE FROM PROGRAM W22418                                       
050700*    EXCLUDE KDSOFT=SW OR IDDC-REF NE SPACE                               
050800                                                                          
050900     COMPUTE W-NDCCN-KVVECKOR-LT = IN-NDCCN-KVVECKOR-LT + 1               
051000                                                                          
051100     PERFORM CA-CALL-W222BHDC                                             
051200                                                                          
051300     PERFORM CB-TILLG                                                     
051400     .                                                                    
051500     EJECT                                                                
051600 CA-CALL-W222BHDC SECTION.                                                
051700     MOVE 'CA-CALL-W222BHDC         ' TO CURRENT-SECTION                  
051800                                                                          
051900     IF IN-NDCCN-KDERS-UTG = 0                                            
052000       MOVE IN-NDCCN-IDARTNR        TO BHDC-IDARTNR                       
052100       MOVE IN-NDCCN-IDDC           TO BHDC-IDDC                          
052200       MOVE TODAYS-YEAR-WEEK        TO BHDC-TIAAVV-AKTUELL                
052300       MOVE TODAYS-DAGNR            TO BHDC-TID-AKTUELL                   
052400       MOVE TODAYS-YEAR-WEEK        TO W009-TIBEHOV-START                 
052500       MOVE 1                       TO W009-ANTAL-VECKOR                  
052600       CALL W009VADD USING W009-TIBEHOV-START W009-ANTAL-VECKOR           
052700       MOVE W009-TIBEHOV-START      TO BHDC-TIBEHOV-START                 
052800       MOVE W-NDCCN-KVVECKOR-LT     TO BHDC-KVVECKOR-BEHOV                
052900       MOVE IN-NDCCN-KVTILLG-TOT    TO BHDC-KVTILLG-TOT-CDC               
053000       MOVE PB-TOTAL-SEP-LEV-XDC    TO BHDC-KDBEHOV                       
053100                                                                          
053200       CALL W222BHDC USING BHDC-W222BHDC                                  
053300                           BHDC-WDK6-PCB  BHDC-WDK7-PCB                   
053400                           BHDC-WDB6-PCB  BHDC-WDR2-PCB                   
053500                           BHDC-WDD7A-PCB BHDC-WDK7E-PCB                  
053600                           BHDC-WDD7-PCB  BHDC-WDK9-PCB                   
053700                           BHDC-REFL1-2501-PCB                            
053800                           BHDC-REFL1-WDB6-PCB                            
053900                           BHDC-REFL1-WDK7-PCB                            
054000                           BHDC-REFL1-UTIL-WDK6-PCB                       
054100                           BHDC-REFL1-UTIL-WDK7-PCB                       
054200                           BHDC-REFL1-UTIL-WDB6-PCB                       
054300                           BHDC-REFL2-2501-PCB                            
054400                           BHDC-REFL2-WDB6-PCB                            
054500                           BHDC-REFL2-UTIL-WDK6-PCB                       
054600                           BHDC-REFL2-UTIL-WDK7-PCB                       
054700                           BHDC-REFL2-UTIL-WDB6-PCB                       
054800                           BHDC-UTIL-WDK6-PCB                             
054900                           BHDC-UTIL-WDK7-PCB                             
055000                           BHDC-UTIL-WDB6-PCB                             
055100                           BHDC-W222-WDK6-PCB                             
055200                           BHDC-W222-WDK7-PCB                             
055300                           BHDC-W222-ARTM-PCB                             
055400                           BHDC-W222-2501-PCB                             
055500                           BHDC-W222-WDB6R-PCB                            
055600                           BHDC-W222-WDK7R-PCB                            
055700                           BHDC-W222-WDB6-PCB                             
055800                           BHDC-W222-WDD7-PCB                             
055900                           BHDC-W222-WDK7E-PCB                            
056000                           BHDC-W222-UTIL-WDK6-PCB                        
056100                           BHDC-W222-UTIL-WDK7-PCB                        
056200                           BHDC-W222-UTIL-WDB6-PCB                        
056300                           BHDC-W222-UTUP-WDK7-PCB                        
056400                           BHDC-W222-UTUP-WDB6-PCB                        
056500                           BHDC-W222-UTUP-UTIL-WDK6-PCB                   
056600                           BHDC-W222-UTUP-UTIL-WDK7-PCB                   
056700                           BHDC-W222-UTUP-UTIL-WDB6-PCB                   
056800                           BHDC-UTUP-WDK7-PCB                             
056900                           BHDC-UTUP-WDB6-PCB                             
057000                           BHDC-UTUP-UTIL-WDK6-PCB                        
057100                           BHDC-UTUP-UTIL-WDK7-PCB                        
057200                           BHDC-UTUP-UTIL-WDB6-PCB                        
057300                                                                          
057400       IF BHDC-ANROP-FEL                                                  
057500          PERFORM S04-NOLLA-W222BHDC                                      
057600       END-IF                                                             
057700     ELSE                                                                 
057800       PERFORM S04-NOLLA-W222BHDC                                         
057900     END-IF                                                               
058000                                                                          
058100     MOVE +1                        TO TAB-IX                             
058200     MOVE TODAYS-YEAR-WEEK          TO TAB-YYWW   (TAB-IX)                
058300                                                                          
058400     PERFORM CAA-SEP-BEHOV-INNEV-VECKA                                    
058500     COMPUTE TAB-KVBEHOV(TAB-IX) ROUNDED =                                
058600             W-KVPB-REF + BHDC-KVBEHOV-DESSUTOM                           
058700                                                                          
058800     MOVE +0                        TO W222-IX                            
058900     PERFORM UNTIL TAB-IX = TAB-IX-MAX                                    
059000       ADD +1                       TO TAB-IX                             
059100                                       W222-IX                            
059200       MOVE TAB-YYWW-YY(TAB-IX - 1) TO TAB-YYWW-YY(TAB-IX)                
059300       COMPUTE TAB-YYWW-WW(TAB-IX) =                                      
059400               TAB-YYWW-WW(TAB-IX - 1) + 1                                
059500       IF TAB-YYWW-WW(TAB-IX) > +52                                       
059600           MOVE 'AAVV  '            TO DAT-KDDATFORM                      
059700           MOVE TAB-YYWW(TAB-IX)    TO DAT-I-TIDATUM                      
059800           CALL WDATKONV USING DAT-KDDATFORM                              
059900                DAT-I-TIDATUM DAT-O-TIDATUM DAT-KDSVAR                    
060000           IF DAT-KDSVAR-FEL                                              
060100              COMPUTE TAB-YYWW-YY(TAB-IX) =                               
060200                      TAB-YYWW-YY(TAB-IX - 1) + 1                         
060300              MOVE 1                TO TAB-YYWW-WW(TAB-IX)                
060400           END-IF                                                         
060500       END-IF                                                             
060600                                                                          
060700       MOVE BHDC-KVBEHOV-VECKA (W222-IX)                                  
060800                                    TO TAB-KVBEHOV(TAB-IX)                
060900     END-PERFORM                                                          
061000     .                                                                    
061100     EJECT                                                                
061200 CAA-SEP-BEHOV-INNEV-VECKA SECTION.                                       
061300     MOVE 'CAA-SEP-BEHOV-INNEV-VECKA  ' TO CURRENT-SECTION                
061400                                                                          
061500     ACCEPT W-TIME             FROM TIME                                  
061600     COMPUTE W-VECKO-SEP-BEHOV ROUNDED  =                                 
061700                               IN-NDCCN-KVPB-REF / 4.33                   
061800     COMPUTE W-DAG-SEP-BEHOV   ROUNDED  =  W-VECKO-SEP-BEHOV / 5          
061900     DIVIDE IN-NDCCN-TIFINLV BY 10 GIVING W-TIFINLV-AAVV                  
062000     MOVE TODAYS-YEAR-WEEK     TO TMP1-YYWW                               
062100     MOVE W-TIFINLV-AAVV       TO TMP2-YYWW                               
062200     PERFORM WY2000P3                                                     
062300                                                                          
062400     MOVE 'AAVVD '             TO DAT-KDDATFORM                           
062500     MOVE TAB-YYWW(TAB-IX)     TO W-YYWW                                  
062600     MOVE TODAYS-DAGNR         TO W-TODAYS-DAGNR                          
062700     MOVE W-YYWWD-NUM          TO DAT-I-TIDATUM                           
062800     CALL WDATKONV USING DAT-KDDATFORM                                    
062900                   DAT-I-TIDATUM DAT-O-TIDATUM DAT-KDSVAR                 
063000     IF DAT-KDSVAR-FEL                                                    
063100         MOVE ZERO              TO W-KVPB-REF                             
063200     ELSE                                                                 
063300        IF  DAT-TID = 6 OR                                                
063400            DAT-TID = 7 OR                                                
063500           (DAT-TID = 5 AND W-TIME(1:4) > 1700)                           
063600        OR  TMP1-YYWW < TMP2-YYWW                                         
063700            MOVE ZERO           TO W-KVPB-REF                             
063800        ELSE                                                              
063900            COMPUTE W-KVDAGAR-KVAR =  5 - DAT-TID                         
064000            IF W-TIME(1:4)         <= 1700                                
064100               ADD +1           TO W-KVDAGAR-KVAR                         
064200            END-IF                                                        
064300                                                                          
064400            COMPUTE W-KVPB-REF = W-DAG-SEP-BEHOV *                        
064500                                 W-KVDAGAR-KVAR                           
064600        END-IF                                                            
064700     END-IF                                                               
064800     .                                                                    
064900     EJECT                                                                
065000 CB-TILLG SECTION.                                                        
065100     MOVE 'CB-TILLG                    ' TO CURRENT-SECTION               
065200                                                                          
065300     COMPUTE W-KVTILLG = IN-NDCCN-KVLS                                    
065400                       + IN-NDCCN-KVAKS-SDC                               
065500                       - IN-NDCCN-KVROS-BULK                              
065600                       - IN-NDCCN-KVROS-DAG                               
065700                       - IN-NDCCN-KVRESS                                  
065800                       - IN-NDCCN-KVSPARR-KVAL                            
065900                       - IN-NDCCN-KVOKS-DAG                               
066000                       - IN-NDCCN-KVOKS-BULK                              
066100                                                                          
066200     MOVE W-KVTILLG             TO W-KVDISP                               
066300                                                                          
066400     PERFORM CBA-ADD-WDL6-TO-TILLG                                        
066500                                                                          
066600     PERFORM CBB-ADD-WDD9-TO-TAB-AVROP                                    
066700     .                                                                    
066800     EJECT                                                                
066900 CBA-ADD-WDL6-TO-TILLG SECTION.                                           
067000     MOVE 'CBA-ADD-WDL6-TO-TILLG' TO CURRENT-SECTION                      
067100                                                                          
067200     MOVE +0                      TO W-KVAVIS-KVRAPP                      
067300                                                                          
067400     MOVE IN-NDCCN-IDARTNR        TO W-IDARTNR                            
067500     PERFORM IMS-GU-WDL601                                                
067600     IF SEGMENT-FOUND                                                     
067700       PERFORM IMS-GNP-WDL611                                             
067800       PERFORM UNTIL SEGMENT-MISSING                                      
067900         IF INL-IDDC = IN-NDCCN-IDDC                                      
068000           IF INL-KDRT   = 7                                              
068100              MOVE +0 TO W-TOT-KVRAPP                                     
068200              MOVE INL-DAINLEV      TO W-DAINLEV                          
068300              PERFORM IMS-GNP-WDL621                                      
068400              PERFORM UNTIL SEGMENT-MISSING                               
068500                 ADD NDEL-KVRAPP TO W-TOT-KVRAPP                          
068600                 PERFORM IMS-GNP-WDL621                                   
068700              END-PERFORM                                                 
068800                                                                          
068900              COMPUTE W-KVAVIS-KVRAPP =                                   
069000                   W-KVAVIS-KVRAPP + (INL-KVAVIS - W-TOT-KVRAPP)          
069100           END-IF                                                         
069200         END-IF                                                           
069300                                                                          
069400         PERFORM IMS-GNP-WDL611                                           
069500                                                                          
069600       END-PERFORM                                                        
069700     END-IF                                                               
069800     COMPUTE W-KVTILLG = W-KVTILLG - W-KVAVIS-KVRAPP                      
069900     .                                                                    
070000     EJECT                                                                
070100 CBB-ADD-WDD9-TO-TAB-AVROP SECTION.                                       
070200     MOVE 'CBB-ADD-WDD9-TO-TAB-AVROP' TO CURRENT-SECTION                  
070300                                                                          
070400     MOVE +0                           TO TAB-IX                          
070500     MOVE +0                               TO W-KVAVROP-OLD               
070600                                                                          
070700     MOVE IN-NDCCN-IDARTNR             TO W-IDARTNR-D9                    
070800     MOVE IN-NDCCN-IDDC                TO W-IDDC-D9                       
070900     PERFORM IMS-GU-WDD901                                                
071000     IF SEGMENT-FOUND                                                     
071100        PERFORM IMS-GNP-WDD905                                            
071200        PERFORM UNTIL SEGMENT-MISSING                                     
071300          IF WDD905-KVAVROP > ZERO                                        
071400             MOVE 'AAMMDD'             TO DAT-KDDATFORM                   
071500             MOVE WDD905-TIAVRDAT-DISP TO DAT-I-TIDATUM                   
071600                                                                          
071700             CALL WDATKONV USING DAT-KDDATFORM                            
071800                  DAT-I-TIDATUM DAT-O-TIDATUM DAT-KDSVAR                  
071900             IF DAT-KDSVAR-OK                                             
072000                IF DAT-TIAAVV-GRP < TAB-YYWW (01)                         
072100                   ADD WDD905-KVAVROP      TO W-KVTILLG                   
072200                                              W-KVAVROP-OLD               
072300                END-IF                                                    
072400                                                                          
072500                MOVE +0                      TO TAB-IX                    
072600                PERFORM UNTIL W-NDCCN-KVVECKOR-LT = TAB-IX OR             
072700                                            TAB-IX = TAB-IX-MAX           
072800                  ADD +1                     TO TAB-IX                    
072900                  IF DAT-TIAAVV-GRP = TAB-YYWW (TAB-IX)                   
073000                     ADD WDD905-KVAVROP TO TAB-KVAVROP (TAB-IX)           
073100                  ELSE                                                    
073200                     IF DAT-TIAAVV-GRP >                                  
073300                        TAB-YYWW (W-NDCCN-KVVECKOR-LT)                    
073400                        MOVE TAB-IX-MAX         TO TAB-IX                 
073500                     END-IF                                               
073600                  END-IF                                                  
073700                END-PERFORM                                               
073800             END-IF                                                       
073900          END-IF                                                          
074000          PERFORM IMS-GNP-WDD905                                          
074100        END-PERFORM                                                       
074200     END-IF                                                               
074300     .                                                                    
074400     EJECT                                                                
074500 D-WRITE-W2219B SECTION.                                                  
074600     MOVE 'D-WRITE-W2219B         ' TO CURRENT-SECTION                    
074700                                                                          
074800     MOVE NOO                        TO W-GET-WDD924                      
074900     MOVE YES                        TO W-CREATE-ALERT-FIRST-TIME         
075000     MOVE +0                         TO TAB-IX                            
075100     MOVE ZERO                       TO W-TILEVBSK-DISP-YYWW-LAST         
075200     MOVE ZERO                       TO W-TIBEHOV-FIRST                   
075300     PERFORM UNTIL W-NDCCN-KVVECKOR-LT = TAB-IX                           
075400                                                                          
075500        ADD  +1                      TO TAB-IX                            
075600        COMPUTE W-KVTILLG = W-KVTILLG + TAB-KVAVROP(TAB-IX)               
075700                                                                          
075800        IF TAB-IX > +1                                                    
075900           SUBTRACT TAB-KVBEHOV(TAB-IX - 1) FROM W-KVTILLG                
076000        END-IF                                                            
076100                                                                          
076200        MOVE NOO                     TO TAB-FLLARM (TAB-IX)               
076300        IF TAB-KVBEHOV(TAB-IX) > W-KVTILLG                                
076400           MOVE YES                  TO TAB-FLLARM (TAB-IX)               
076500           IF W-GET-WDD924 = NOO                                          
076600              PERFORM DA-GET-WDD924-LAST                                  
076700              MOVE YES               TO W-GET-WDD924                      
076800           END-IF                                                         
076900           IF W-TILEVBSK-DISP-YYWW-LAST = TAB-YYWW (TAB-IX)               
077000           OR W-TILEVBSK-DISP-YYWW-LAST > TAB-YYWW (TAB-IX)               
077100              MOVE NOO               TO TAB-FLLARM (TAB-IX)               
077200           ELSE                                                           
077300              COMPUTE W-TILLG-BEHOV =                                     
077400                      W-KVTILLG - TAB-KVBEHOV(TAB-IX)                     
077500              IF W-TILLG-BEHOV > -0.99 AND W-TILLG-BEHOV < 0.00           
077600                 MOVE NOO            TO TAB-FLLARM (TAB-IX)               
077700              END-IF                                                      
077800           END-IF                                                         
077900                                                                          
078000           IF TAB-FLLARM (TAB-IX)       = YES                             
078100          AND W-CREATE-ALERT-FIRST-TIME = YES                             
078200              MOVE TAB-YYWW (TAB-IX) TO W-TIBEHOV-FIRST                   
078300              MOVE NOO               TO W-CREATE-ALERT-FIRST-TIME         
078400           END-IF                                                         
078500        END-IF                                                            
078600                                                                          
078700        MOVE W-KVTILLG               TO TAB-SUDISPV(TAB-IX)               
078800                                                                          
078900     END-PERFORM                                                          
079000                                                                          
079100     MOVE NOO                        TO W-CREATE-ALERT                    
079200     MOVE +0                         TO TAB-IX                            
079300     PERFORM UNTIL W-NDCCN-KVVECKOR-LT = TAB-IX                           
079400                OR W-CREATE-ALERT      = YES                              
079500       ADD +1                        TO TAB-IX                            
079600       IF TAB-FLLARM (TAB-IX) = YES                                       
079700          MOVE YES                   TO W-CREATE-ALERT                    
079800       END-IF                                                             
079900     END-PERFORM                                                          
080000                                                                          
080100     IF W-CREATE-ALERT = YES                                              
080200        IF W-DELETE-ALERT = NOO                                           
080300           PERFORM S11-MOVE-TO-WORK-AREA                                  
080400           PERFORM S11A-WRITE-W2219B01                                    
080500        END-IF                                                            
080600     ELSE                                                                 
080700        IF W-DELETE-ALERT = YES                                           
080800           PERFORM S11-MOVE-TO-WORK-AREA                                  
080900           PERFORM S11B-WRITE-W2219B02                                    
081000        END-IF                                                            
081100     END-IF                                                               
081200                                                                          
081300     IF W-DELETE-ALERT = NOO                                              
081400        PERFORM DB-WRITE-W2219403                                         
081500     END-IF                                                               
081600     .                                                                    
081700     EJECT                                                                
081800 DA-GET-WDD924-LAST SECTION.                                              
081900     MOVE 'DA-GET-WDD924-LAST '           TO CURRENT-SECTION              
082000                                                                          
082100     MOVE ZERO                       TO W-TILEVBSK-DISP-YYWW-LAST         
082200     MOVE IN-NDCCN-IDARTNR              TO W-IDARTNR                      
082300     PERFORM IMS-GU-WDD901                                                
082400     IF SEGMENT-FOUND                                                     
082500       MOVE IN-NDCCN-IDLEVNR            TO W-IDLEVNR                      
082600       PERFORM IMS-GNP-WDD924-LAST                                        
082700       IF SEGMENT-FOUND                                                   
082800          MOVE 'AAMMDD'                 TO DAT-KDDATFORM                  
082900          MOVE WDD924-LEV-TILEVBSK-DISP TO DAT-I-TIDATUM                  
083000          CALL WDATKONV USING DAT-KDDATFORM                               
083100                        DAT-I-TIDATUM DAT-O-TIDATUM DAT-KDSVAR            
083200          IF DAT-KDSVAR-OK                                                
083300             MOVE DAT-TIAAVV-GRP     TO W-TILEVBSK-DISP-YYWW-LAST         
083400          END-IF                                                          
083500       END-IF                                                             
083600     END-IF                                                               
083700     .                                                                    
083800     EJECT                                                                
083900 DB-WRITE-W2219403 SECTION.                                               
084000     MOVE 'DB-WRITE-W2219403  '      TO CURRENT-SECTION                   
084100                                                                          
084200     MOVE IN-NDCCN-IDANSK           TO OUT3-IDANSK                        
084300     MOVE 223                       TO OUT3-KDLARM                        
084400     MOVE IN-NDCCN-IDARTNR          TO OUT3-IDARTNR                       
084500     MOVE IN-NDCCN-IDDC             TO OUT3-IDDC                          
084600     MOVE IN-NDCCN-IDLEVNR          TO OUT3-IDLEVNR                       
084700     MOVE W-KVDISP                  TO OUT3-KVDISP                        
084800     MOVE W-KVAVIS-KVRAPP           TO OUT3-KVAVIS                        
084900     MOVE W-KVAVROP-OLD             TO OUT3-KVAVROP-OLD                   
085000     MOVE W-TILEVBSK-DISP-YYWW-LAST TO OUT3-TILEVBSK-DISP-LAST            
085100     MOVE W-CREATE-ALERT            TO OUT3-FLLARM-TOT                    
085200                                                                          
085300     MOVE +1                        TO TAB-IX                             
085400     PERFORM UNTIL TAB-IX > TAB-IX-MAX-30                                 
085500       MOVE TAB-YYWW       (TAB-IX) TO OUT3-TIAAVV  (TAB-IX)              
085600       MOVE TAB-KVBEHOV    (TAB-IX) TO OUT3-KVBEHOV (TAB-IX)              
085700       MOVE TAB-KVAVROP    (TAB-IX) TO OUT3-KVAVROP (TAB-IX)              
085800       MOVE TAB-SUDISPV    (TAB-IX) TO OUT3-SUDISPV (TAB-IX)              
085900       MOVE TAB-FLLARM     (TAB-IX) TO OUT3-FLLARM  (TAB-IX)              
086000       ADD +1                       TO TAB-IX                             
086100     END-PERFORM                                                          
086200                                                                          
086300     PERFORM S11C-WRITE-W2219B03                                          
086400     .                                                                    
086500     EJECT                                                                
086600*------------------------------                                           
086700 Z-FINIT SECTION.                                                         
086800     CLOSE W22418                                                         
086900           W22191                                                         
087000           W2219B01                                                       
087100           W2219B02                                                       
087200           W2219B03                                                       
087300     SKIP2                                                                
087400     MOVE 'S' TO POSTSUM-OPKOD                                            
087500     CALL POSTSUM USING POSTSUM-PARM                                      
087600     .                                                                    
087700     EJECT                                                                
087800 S01-LAES-W22418  SECTION.                                                
087900     MOVE 'S01-LAES-W22418        ' TO CURRENT-SECTION                    
088000                                                                          
088100     READ W22418 INTO IN-AREA                                             
088200     AT END                                                               
088300        SET END-OF-W22418 TO TRUE                                         
088400                                                                          
088500     NOT AT END                                                           
088600        MOVE 'W22418' TO POSTSUM-FDNAMN                                   
088700        MOVE 'W2219BD1' TO POSTSUM-DDNAMN2                                
088800        CALL POSTSUM USING POSTSUM-PARM                                   
088900     END-READ                                                             
089000     .                                                                    
089100     EJECT                                                                
089200                                                                          
089300 S01-LAES-W22191  SECTION.                                                
089400     MOVE 'S01-LAES-W22191        ' TO CURRENT-SECTION                    
089500                                                                          
089600     READ W22191 INTO IN-AREA                                             
089700     AT END                                                               
089800        SET END-OF-W22191 TO TRUE                                         
089900                                                                          
090000     NOT AT END                                                           
090100        MOVE 'W22191' TO POSTSUM-FDNAMN                                   
090200        MOVE 'W2219BD2' TO POSTSUM-DDNAMN2                                
090300        CALL POSTSUM USING POSTSUM-PARM                                   
090400     END-READ                                                             
090500     .                                                                    
090600     EJECT                                                                
090700                                                                          
090800 S02-INIT-TAB-BEHOV SECTION.                                              
090900     MOVE 'S02-INIT-TAB-BEHOV     ' TO CURRENT-SECTION                    
091000                                                                          
091100     MOVE +1         TO TAB-IX                                            
091200     PERFORM UNTIL TAB-IX > TAB-IX-MAX                                    
091300       MOVE ZERO     TO TAB-YYWW    (TAB-IX)                              
091400       MOVE +0       TO TAB-KVBEHOV (TAB-IX)                              
091500                        TAB-KVAVROP (TAB-IX)                              
091600                        TAB-SUDISPV (TAB-IX)                              
091700       MOVE SPACE    TO TAB-FLLARM  (TAB-IX)                              
091800       ADD +1        TO TAB-IX                                            
091900     END-PERFORM                                                          
092000                                                                          
092100     .                                                                    
092200     EJECT                                                                
092300 S04-NOLLA-W222BHDC SECTION.                                              
092400                                                                          
092500     MOVE +0                        TO BHDC-KVBEHOV-SUMMA                 
092600                                       BHDC-KVBEHOV-DESSUTOM              
092700                                       BHDC-TIBEHOV-FIRST                 
092800                                                                          
092900     MOVE +1                        TO TAB-IX                             
093000     PERFORM UNTIL TAB-IX > TAB-IX-MAX                                    
093100       MOVE +0                      TO BHDC-KVBEHOV-VECKA(TAB-IX)         
093200       ADD +1                       TO TAB-IX                             
093300     END-PERFORM                                                          
093400     .                                                                    
093500     EJECT                                                                
093600 S11-MOVE-TO-WORK-AREA SECTION.                                           
093700     MOVE 'S11-MOVE-TO-WORK-AREA   ' TO CURRENT-SECTION                   
093800                                                                          
093900     MOVE IN-NDCCN-IDANSK      TO W-IDANSK-2232                           
094000     PERFORM IMS-GU-WDR220                                                
094100     IF SEGMENT-FOUND                                                     
094200        MOVE 2232-IDANSK-LARM  TO WORK-IDANSK                             
094300     ELSE                                                                 
094400        MOVE ZERO              TO WORK-IDANSK                             
094500     END-IF                                                               
094600     MOVE 223                  TO WORK-KDLARM                             
094700     MOVE IN-NDCCN-IDARTNR TO WORK-IDARTNR                                
094800     MOVE IN-NDCCN-IDDC        TO WORK-IDDC                               
094900     MOVE JA                   TO WORK-FLNYLARM                           
095000     MOVE ZERO                 TO WORK-IDDISTR                            
095100     MOVE ZERO                 TO WORK-IDKUNDNR                           
095200     MOVE '0000000   '         TO WORK-IDKUNDRF                           
095300     MOVE 1                    TO WORK-IDLOPNR                            
095400     MOVE 'W221'               TO WORK-IDTRANS                            
095500     MOVE SPACE                TO WORK-KDMFSFOR                           
095600     MOVE ZERO                 TO WORK-IDKR                               
095700     MOVE IN-NDCCN-IDLEVNR     TO WORK-IDLEVNR                            
095800     MOVE W-TIBEHOV-FIRST      TO WORK-TIBEHOV-FIRST                      
095900     .                                                                    
096000     EJECT                                                                
096100                                                                          
096200 S11A-WRITE-W2219B01 SECTION.                                             
096300     MOVE 'S11A-WRITE-W2219B01     ' TO CURRENT-SECTION                   
096400                                                                          
096500     WRITE OUT1-RECORD FROM WORK-W2219401                                 
096600                                                                          
096700     MOVE SPACE      TO POSTSUM-TRANSTYP                                  
096800     MOVE 'INSERT'   TO POSTSUM-FDNAMN                                    
096900     MOVE 'W2219BD3' TO POSTSUM-DDNAMN2                                   
097000     CALL POSTSUM USING POSTSUM-PARM                                      
097100     .                                                                    
097200     EJECT                                                                
097300 S11B-WRITE-W2219B02 SECTION.                                             
097400     MOVE 'S11A-WRITE-W2219B02     ' TO CURRENT-SECTION                   
097500                                                                          
097600     WRITE OUT2-RECORD FROM WORK-W2219401                                 
097700                                                                          
097800     MOVE SPACE      TO POSTSUM-TRANSTYP                                  
097900     MOVE 'DELETE'   TO POSTSUM-FDNAMN                                    
098000     MOVE 'W2219BD4' TO POSTSUM-DDNAMN2                                   
098100     CALL POSTSUM USING POSTSUM-PARM                                      
098200     .                                                                    
098300     EJECT                                                                
098400 S11C-WRITE-W2219B03 SECTION.                                             
098500     MOVE 'S11C-WRITE-W2219B03     ' TO CURRENT-SECTION                   
098600                                                                          
098700     WRITE OUT3-RECORD FROM OUT3-W2219402                                 
098800                                                                          
098900     MOVE SPACE      TO POSTSUM-TRANSTYP                                  
099000     MOVE 'W2219B03' TO POSTSUM-FDNAMN                                    
099100     MOVE 'W2219BD5' TO POSTSUM-DDNAMN2                                   
099200     CALL POSTSUM USING POSTSUM-PARM                                      
099300     .                                                                    
099400     EJECT                                                                
099500 S99-ABEND SECTION.                                                       
099600                                                                          
099700     SKIP2                                                                
099800     MOVE 'S' TO POSTSUM-OPKOD                                            
099900     CALL POSTSUM USING POSTSUM-PARM                                      
100000     CALL ABEND USING RKOD-ABEND                                          
100100     .                                                                    
100200     EJECT                                                                
100300* --- IMS SECTIONS  ---                                                   
100400                                                                          
100500 IMS-GU-WDD901 SECTION.                                                   
100600     MOVE 'IMS-GU-WDD901 '   TO DBS-SECTION                               
100700                                                                          
100800     STRING 'WDD901  (WDD901KY =' W-WDD901KY-X ')'                        
100900          DELIMITED BY SIZE INTO SSA1                                     
101000     MOVE '  GE' TO GOOD-STATUSCODES                                      
101100     CALL CBLTDLI USING GU WDD9-PCB DLI-IO-WDD901 SSA1                    
101200     MOVE WDD9-STATUS-CODE TO STATUS-WS                                   
101300     PERFORM IMS-STATUSCHECK                                              
101400     .                                                                    
101500                                                                          
101600 IMS-GNP-WDD905 SECTION.                                                  
101700     MOVE 'IMS-GNP-WDD905 '   TO DBS-SECTION                              
101800                                                                          
101900     STRING 'WDD905  (KDAVROP  =' W-KDAVROP-X ')'                         
102000          DELIMITED BY SIZE   INTO SSA1                                   
102100     MOVE '  GE' TO GOOD-STATUSCODES                                      
102200     CALL CBLTDLI USING GNP WDD9-PCB DLI-IO-WDD905 SSA1                   
102300     MOVE WDD9-STATUS-CODE TO STATUS-WS                                   
102400     PERFORM IMS-STATUSCHECK                                              
102500     .                                                                    
102600                                                                          
102700 IMS-GNP-WDD924-LAST SECTION.                                             
102800     MOVE 'IMS-GNP-WDD924-LAST   ' TO DBS-SECTION                         
102900                                                                          
103000     STRING 'WDD902  (IDLEVNR  =' W-IDLEVNR-X ')'                         
103100          DELIMITED BY SIZE INTO SSA1                                     
103200     MOVE 'WDD924  *L ' TO SSA2                                           
103300     MOVE '  GE' TO GOOD-STATUSCODES                                      
103400     CALL CBLTDLI USING GNP WDD9-PCB DLI-IO-WDD924 SSA1 SSA2              
103500                                                                          
103600     MOVE WDD9-STATUS-CODE TO STATUS-WS                                   
103700     PERFORM IMS-STATUSCHECK                                              
103800     .                                                                    
103900     EJECT                                                                
104000 IMS-GU-WDR220 SECTION.                                                   
104100     MOVE 'IMS-GU-WDR220 '  TO DBS-SECTION                                
104200                                                                          
104300     STRING 'WDR201  (WDGXKEY  =' W-WDGX2231-X ')'                        
104400          DELIMITED BY SIZE INTO SSA1                                     
104500     STRING 'WDR220  (WDGXKEY  =' W-WDGX2232-X ')'                        
104600          DELIMITED BY SIZE INTO SSA2                                     
104700     MOVE '  GE' TO GOOD-STATUSCODES                                      
104800     CALL CBLTDLI USING GU WDR2-PCB DLI-IO-WDGX2232 SSA1 SSA2             
104900     MOVE WDR2-STATUS-CODE TO STATUS-WS                                   
105000     PERFORM IMS-STATUSCHECK                                              
105100     .                                                                    
105200     SKIP2                                                                
105300 IMS-GU-WDL601 SECTION.                                                   
105400     MOVE 'IMS-GU-WDL601 '  TO DBS-SECTION                                
105500                                                                          
105600     STRING 'WDL601  (IDARTNR  =' W-IDARTNR-X ')'                         
105700          DELIMITED BY SIZE INTO SSA1                                     
105800     MOVE '  GE'           TO GOOD-STATUSCODES                            
105900     CALL CBLTDLI USING GU WDL6-PCB DLI-IO-WDL601 SSA1                    
106000     MOVE WDL6-STATUS-CODE TO STATUS-WS                                   
106100     PERFORM IMS-STATUSCHECK                                              
106200     .                                                                    
106300                                                                          
106400 IMS-GNP-WDL611 SECTION.                                                  
106500     MOVE 'IMS-GNP-WDL611'  TO DBS-SECTION                                
106600                                                                          
106700     STRING 'WDL611  (IDPTYP   =' W-IDPTYP-X ')'                          
106800             DELIMITED BY SIZE INTO SSA1                                  
106900     MOVE '  GE'           TO GOOD-STATUSCODES                            
107000     CALL CBLTDLI USING GNP WDL6-PCB DLI-IO-WDL611 SSA1                   
107100     MOVE WDL6-STATUS-CODE TO STATUS-WS                                   
107200     PERFORM IMS-STATUSCHECK                                              
107300     .                                                                    
107400                                                                          
107500 IMS-GNP-WDL621 SECTION.                                                  
107600     MOVE 'IMS-GNP-WDL621'  TO DBS-SECTION                                
107700                                                                          
107800     STRING 'WDL611  (DAINLEV  =' W-DAINLEV-X ')'                         
107900             DELIMITED BY SIZE INTO SSA1                                  
108000     MOVE 'WDL621'         TO SSA2                                        
108100     MOVE '  GE' TO GOOD-STATUSCODES                                      
108200     CALL CBLTDLI USING GNP WDL6-PCB DLI-IO-WDL621 SSA1 SSA2              
108300     MOVE WDL6-STATUS-CODE TO STATUS-WS                                   
108400     PERFORM IMS-STATUSCHECK                                              
108500     .                                                                    
108600                                                                          
108700 IMS-STATUSCHECK SECTION.                                                 
108800                                                                          
108900     SET STATUS-IX TO 1                                                   
109000     SEARCH GOOD-STATUS                                                   
109100       AT END                                                             
109200         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
109300           DELIMITED BY SIZE INTO ERROR-TEXT                              
109400         DISPLAY ERROR-TEXT                                               
109500         CALL FELLOG                                                      
109600       WHEN GOOD-STATUS (STATUS-IX) = STATUS-WS                           
109700         CONTINUE                                                         
109800     END-SEARCH                                                           
109900     .                                                                    
110000*    -COPY WY2000P3                                                       
