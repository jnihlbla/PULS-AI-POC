000100 ID DIVISION.                                                             
000200     SKIP2                                                                
000300 PROGRAM-ID.     W2717800.                                                
000400*AUTHOR.         STEFAN ANDREASSON.                                       
000500*DATE-WRITTEN.   99/09/16.                                                
000600                                                                          
000700*    REMARKS.                                                             
000800*                                                                         
000900*                                                                         
001000*    FUNKTION:                                                            
001100*                                                                         
001200*        PROGRAMMET LÄSER      WDK7 MED SB                                
001300*                                                                         
001400*    ABENDKODER:                                                          
001500*        U0016 -  SVAR FRÅN WDATKONV EJ OK                                
001600*              -  "ÖVERSÄTTNING" AV LAGER TILL DC-INDX SAKNAS             
001700*        U1000 -  . . . .                                                 
001800*                                                                         
001900                                                                          
002000     SKIP3                                                                
002100 ENVIRONMENT DIVISION.                                                    
002200     SKIP2                                                                
002300 INPUT-OUTPUT SECTION.                                                    
002400                                                                          
002500 FILE-CONTROL.                                                            
002600     SKIP2                                                                
002700*                                                                         
002800                                                                          
002900     SELECT W27178                     ASSIGN TO W27178D1.                
003000*          --- UT-FIL                                                     
003100     EJECT                                                                
003200 DATA DIVISION.                                                           
003300     SKIP3                                                                
003400 FILE SECTION.                                                            
003500     SKIP3                                                                
003600                                                                          
003700                                                                          
003800 FD  W27178                                                               
003900     RECORDING       F                                                    
004000     BLOCK CONTAINS  0.                                                   
004100                                                                          
004200*01  POST -COPY W27178  -PRE UT-    -L.                                   
004300                                                                          
004400     EJECT                                                                
004500 WORKING-STORAGE SECTION.                                                 
004600     SKIP2                                                                
004700*    -COPY WY2000W1                                                       
004800     SKIP2                                                                
004900*    -COPY WY2000W3                                                       
005000     SKIP3                                                                
005100*    -COPY WY2000W2                                                       
005200     SKIP3                                                                
005300 77  IDPGM                       PIC X(8)    VALUE 'W2717800'.            
005400 77  JA                          PIC X       VALUE 'J'.                   
005500 77  NEJ                         PIC X       VALUE 'N'.                   
005600 77  AKTIV                       PIC X       VALUE 'A'.                   
005700                                                                          
005800*    --- INDEX SAMT MAX-INDEX                                             
005900 77  FILLER                      PIC X(16)   VALUE 'INDEX'.               
006000 77  INDX                        PIC 9(2)    VALUE ZERO.                  
006100                                                                          
006200*    --- SWITCHAR                                                         
006300 01  FILLER                      PIC X(16)   VALUE 'SWITCHAR'.            
006400                                                                          
006500 01  DC-POST.                                                             
006600     03  DC-PARAMETER        PIC X(4).                                    
006700         88 SAMTLIGA-DC      VALUE 'ALLA'.                                
006800         88 SAMTLIGA-SDC     VALUE 'EURO'.                                
006900         88 SAMTLIGA-NDC     VALUE 'AMER'.                                
007000         88 ENSTAKA-DC       VALUE 'DC21' 'DC22' 'DC23'                   
007100                                   'DC24' 'DC25' 'DC26'                   
007200                                   'DC3A'                                 
007300                                   'DC41' 'DC42' 'DC43' 'DC51'.           
007400                                                                          
007500     03  FILLER               PIC X(76).                                  
007600                                                                          
007700                                                                          
007800*      --- VALID IDDC CODES                                               
007900*                                                                         
008000*01    -COPY WWDC99                                                       
008100                                                                          
008200                                                                          
008300     EJECT                                                                
008400 01  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
008500 01  FILLER REDEFINES DAGENS-DATUM.                                       
008600     03  DAGENS-DATUM-AAR        PIC 9(2).                                
008700     03  DAGENS-DATUM-MAANAD     PIC 9(2).                                
008800     03  DAGENS-DATUM-DAG        PIC 9(2).                                
008900     EJECT                                                                
009000                                                                          
009100                                                                          
009200 01  DYNAMISKA-SUBPROGRAM.                                                
009300*                                                                         
009400     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
009500     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
009600     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
009700     03  DATKORT                 PIC X(8)    VALUE 'DATKORT'.             
009800     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
009900     03  WDATKONV                PIC X(8)    VALUE 'WDATKONV'.            
010000     SKIP2                                                                
010100*    --- PARAMETRAR TILL ABEND                                            
010200                                                                          
010300 77  RKOD-ABEND-UTAN-DUMP        PIC S9(4)   COMP VALUE +16.              
010400 77  RKOD-ABEND-MED-DUMP         PIC S9(4)   COMP VALUE +1000.            
010500     SKIP2                                                                
010600 01  FELTEXT.                                                             
010700     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
010800     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
010900     EJECT                                                                
011000*    --- PARAMETRAR TILL DATKORT                                          
011100*                                                                         
011200 01  PROGRAM-NAMN                PIC X(6)    VALUE 'W27178'.              
011300     SKIP2                                                                
011400 01  DATUMKORT-ID                PIC X(6)    VALUE 'WDATUM'.              
011500     SKIP2                                                                
011600*01  -COPY WDATKORT                                                       
011700     EJECT                                                                
011800*    --- PARAMETRAR TILL POSTSUM                                          
011900*                                                                         
012000*01  -COPY W0005   -PRE  POSTSUM-                                         
012100     EJECT                                                                
012200*    --- PARAMETRAR TILL WDATKONV                                         
012300*                                                                         
012400*01  -COPY WDATAREA                                                       
012500     EJECT                                                                
012600 01  UT-AREA-START              PIC X(24)   VALUE                         
012700                                 'UT-AREA-START  '.                       
012800     SKIP2                                                                
012900                                                                          
013000*01  AREA -COPY W27178     -PRE UT-                                       
013100     EJECT                                                                
013200*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
013300                                                                          
013400     SKIP3                                                                
013500 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
013600     SKIP3                                                                
013700 01  NYCKLAR-TILL-DLI.                                                    
013800     03  W-IDARTNR-X.                                                     
013900         05  W-IDARTNR           PIC S9(9)   VALUE ZERO COMP-3.           
014000                                                                          
014100     03  W-IDDC-X.                                                        
014200         05  W-IDDC              PIC X(2)    VALUE SPACE.                 
014300                                                                          
014400     03  W-KDSEGKEY-X.                                                    
014500         05  W-KDSEGKEY          PIC X       VALUE '1'.                   
014600                                                                          
014700*                                                                         
014800     SKIP2                                                                
014900*    --- STATUS-KOD FRÅN IMS                                              
015000 01  STATUS-WS                   PIC XX.                                  
015100     88  SEGMENT-FINNS                       VALUE '  '.                  
015200     88  SEGMENT-SAKNAS                      VALUE 'GB'.                  
015300     SKIP2                                                                
015400 01  GODK-STATUSKODER.                                                    
015500     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
015600     SKIP3                                                                
015700 01  SSA1                        PIC X(64).                               
015800 01  SSA2                        PIC X(64).                               
015900     EJECT                                                                
016000*    --- IMS FUNKTIONSKODER                                               
016100*01  -COPY W0003                                                          
016200     EJECT                                                                
016300*    ---  DLI INPUT-OUTPUT AREA                                           
016400                                                                          
016500                                                                          
016600 01  FILLER                    PIC X(16)   VALUE 'DLI-IO-AREA-K7'.        
016700     SKIP3                                                                
016800 01  DLI-IO-AREA-K7.                                                      
016900     03  IO-AREA-K7              PIC X(300)  VALUE SPACE.                 
017000     SKIP3                                                                
017100     03  WLARTS01 REDEFINES IO-AREA-K7.                                   
017200*        05  -COPY WDK701                                                 
017300     SKIP3                                                                
017400     03  WLARTS11 REDEFINES IO-AREA-K7.                                   
017500*        05  -COPY WDK711                                                 
017600     EJECT                                                                
017700 01  FILLER         PIC X(24) VALUE 'DLI-IO-WDK601'.                      
017800 01  DLI-IO-WDK601.                                                       
017900*    03  -COPY WDK601                                                     
018000     EJECT                                                                
018100                                                                          
018200 01  FILLER         PIC X(24) VALUE 'DLI-IO-WDK611'.                      
018300 01  DLI-IO-WDK611.                                                       
018400*    03  -COPY WDK611                                                     
018500     EJECT                                                                
018600                                                                          
018700                                                                          
018800 LINKAGE SECTION.                                                         
018900                                                                          
019000     EJECT                                                                
019100*01  -COPY W0008  -PRE WDK7-                                              
019200     05  WDK7-KEY-FB-AREA-IDARTNR       PIC S9(9) COMP-3.                 
019300     EJECT                                                                
019400*01  -COPY W0008  -PRE WDK6-                                              
019500     05  FILLER                  PIC X.                                   
019600     EJECT                                                                
019700 PROCEDURE DIVISION  USING WDK7-PCB WDK6-PCB.                             
019800     ENTRY 'DLITCBL' USING WDK7-PCB WDK6-PCB.                             
019900                                                                          
020000     PERFORM A-INIT                                                       
020100     PERFORM IMS-GN-WDK7                                                  
020200     PERFORM UNTIL SEGMENT-SAKNAS                                         
020300       EVALUATE WDK7-SEG-NAME-FB                                          
020400         WHEN 'WDK701  '                                                  
020500           MOVE WDK7-KEY-FB-AREA-IDARTNR TO W-IDARTNR                     
020600         WHEN 'WDK711  '                                                  
020700                                                                          
020800            MOVE SLAG-IDDC             TO W-IDDC                          
020900                                          WS-IDDC                         
021000            PERFORM C-BEHANDLA-ARTIKEL                                    
021100                                                                          
021200       END-EVALUATE                                                       
021300       PERFORM IMS-GN-WDK7                                                
021400     END-PERFORM                                                          
021500                                                                          
021600     PERFORM Z-FINIT                                                      
021700                                                                          
021800     MOVE ZERO TO RETURN-CODE                                             
021900     GOBACK                                                               
022000     .                                                                    
022100     EJECT                                                                
022200                                                                          
022300                                                                          
022400 A-INIT SECTION.                                                          
022500                                                                          
022600     OPEN OUTPUT W27178                                                   
022700     .                                                                    
022800     EJECT                                                                
022900                                                                          
023000                                                                          
023100 C-BEHANDLA-ARTIKEL SECTION.                                              
023200                                                                          
023300     MOVE W-IDARTNR          TO UT-IDARTNR                                
023400     MOVE SLAG-IDDC          TO UT-IDDC                                   
023500     MOVE SLAG-IDDC-REF      TO UT-IDDC-REF                               
023600     MOVE SLAG-IDLEVNR       TO UT-IDLEVNR                                
023700     MOVE SLAG-IDPERSON-BUY                                               
023800                             TO UT-IDPERSON-BUY                           
023900     MOVE SLAG-IDREFTAB      TO UT-IDREFTAB                               
024000     MOVE SLAG-FLFLYG        TO UT-FLFLYG                                 
024100     MOVE SLAG-ADLAGOMR      TO UT-ADLAGOMR                               
024110     MOVE SLAG-FLBUYUPD      TO UT-FLBUYUPD                               
024120     MOVE SLAG-FLTABUPD      TO UT-FLTABUPD                               
024200                                                                          
024201     COMPUTE UT-KVPB-REF-REOI-SUM  = SLAG-KVPB-REF                        
024202                                   + SLAG-KVPBREOI                        
024210                                                                          
024300     PERFORM S12-SKRIV-W27178                                             
024400     .                                                                    
024500     EJECT                                                                
024600                                                                          
024700                                                                          
024800 Z-FINIT SECTION.                                                         
024900                                                                          
025000     CLOSE W27178                                                         
025100                                                                          
025200     MOVE 'S' TO POSTSUM-OPKOD                                            
025300     CALL POSTSUM USING POSTSUM-PARM                                      
025400     .                                                                    
025500     EJECT                                                                
025600                                                                          
025700 S12-SKRIV-W27178 SECTION.                                                
025800                                                                          
025900     WRITE UT-POST FROM UT-AREA                                           
026000                                                                          
026100     MOVE 'W27178'   TO POSTSUM-FDNAMN                                    
026200     MOVE 'W27178D3' TO POSTSUM-DDNAMN2                                   
026300     CALL POSTSUM USING POSTSUM-PARM                                      
026400     .                                                                    
026500     EJECT                                                                
026600* --- IMS SEKTIONER ---                                                   
026700     SKIP3                                                                
026800     EJECT                                                                
026900 IMS-GN-WDK7 SECTION.                                                     
027000                                                                          
027100     CALL CBLTDLI USING GN WDK7-PCB DLI-IO-AREA-K7                        
027200     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
027300     MOVE '  GAGKGB' TO GODK-STATUSKODER                                  
027400     PERFORM IMS-STATUSKONTROLL                                           
027500     .                                                                    
027600     EJECT                                                                
027700                                                                          
027800 IMS-STATUSKONTROLL SECTION.                                              
027900                                                                          
028000     SET STATUS-IX TO 1                                                   
028100     SEARCH GODK-STATUS                                                   
028200       AT END                                                             
028300         STRING 'OTILLÅTEN RETURKOD FRÅN IMS: ' STATUS-WS                 
028400           DELIMITED BY SIZE INTO FELTEXT-STR                             
028500         DISPLAY FELTEXT                                                  
028600         CALL FELLOG                                                      
028700       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
028800         CONTINUE                                                         
028900     END-SEARCH                                                           
029000     .                                                                    
