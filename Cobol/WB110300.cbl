000100 PROCESS DYNAM                                                            
000200 ID DIVISION.                                                             
000300 PROGRAM-ID.     WB110300.                                                
000400 AUTHOR.         CONNY EGHOLT.                                            
000500 DATE-WRITTEN.   04/02/12.                                                
000600 DATE-COMPILED.                                                           
000700                                                                          
000800                                                                          
000900*    FUNKTION:                                                            
001000*        SKAPAR LISTFIL MED EXCEL-STUK                                    
001100*                                                                         
001200*        PROGRAMMET LÄSER      TABELL TB1ACCE                             
001300*        PROGRAMMET LÄSER      WDD2                                       
001400*        PROGRAMMET LÄSER      WDD9                                       
001500*        PROGRAMMET LÄSER      WDK6                                       
001510*        PROGRAMMET LÄSER      WDC1                                       
001520*        PROGRAMMET LÄSER      WDL2                                       
001600*                                                                         
001700*    ÄNDRINGAR:                                                           
001800*        TILLÄGG AV ANNULLERINGSFLAGGA I FÖRSTA POSITION                  
001900*      2007-SEPT:                                                         
002000*        STORA ÄNDRINGAR: SE PGM WB010100 FÖR INFO                        
002001*                                                                         
002010*      2007-NOV:                                                          
002020*        SMÅ ÄNDRINGAR:                                                   
002030*        TILLÄGG AV 6 NYA MANUELLT UPPDATERBARA NOTERINGSFÄLT.            
002031*        BORTTAG AV DE 6 "PSW-PLAN"-FÄLT, SOM KOM TILL 2007-SEP.          
002040*                                                                         
002050*    MARS-2008:     SCR 6488739. (EMERG)                                  
002060*        RÄTTA LÄSNING AV DATAKÄLLAN FÖR ATT VISA FLTPDWKPH1              
002070*        FRÅN ACCE-DATPDPH1  TILL  ACCE-DAPSWQP-1                         
002080*                                                                         
002090*    MARCH-2016: SCR 10219962.                                            
002091*        LOT OF CHANGES DUE TO NEW WAYS-OF-WORKING.                       
002092*                                                                         
002100                                                                          
002200     SKIP3                                                                
002300 ENVIRONMENT DIVISION.                                                    
002400     SKIP2                                                                
002500 INPUT-OUTPUT SECTION.                                                    
002600                                                                          
002700 FILE-CONTROL.                                                            
002800     SKIP2                                                                
002900*          --- LISTFILEN MED TABAVGRÄNSADE FÄLT (XLS)                     
003000     SELECT WB1103                     ASSIGN TO WB1103D1.                
003100     EJECT                                                                
003200 DATA DIVISION.                                                           
003300     SKIP3                                                                
003400 FILE SECTION.                                                            
003500     SKIP3                                                                
003600 FD  WB1103                                                               
003700     RECORDING       V                                                    
003800     BLOCK CONTAINS  0.                                                   
003900*01  POST -COPY WB1103R  -PRE  UT-RUB- -L.                                
003910*01  POST -COPY WB1103R2 -PRE  UT-R2- -L.                                 
004000*01  POST -COPY WB1103   -PRE  UT-XLS- -L.                                
004100     EJECT                                                                
004200                                                                          
004300 WORKING-STORAGE SECTION.                                                 
004400                                                                          
004500 77  IDPGM                       PIC X(8)    VALUE 'WB110300'.            
004600 77  JA                          PIC X       VALUE 'J'.                   
004700 77  YES                         PIC X       VALUE 'Y'.                   
004800 77  NEJ                         PIC X       VALUE 'N'.                   
004900 77  PROPOSED                    PIC X       VALUE 'P'.                   
004910 77  CONFIRMED                   PIC X       VALUE 'C'.                   
005000 77  OBSOLETE                    PIC X       VALUE 'O'.                   
005100 77  TAB                         PIC X       VALUE X'05'.                 
005110 77  ANT-SPACE                   PIC S9(3) COMP-3 VALUE 0.                
005121 77  START-POS                   PIC S9(3) COMP-3 VALUE 0.                
005130 77  WS-TEXT                     PIC X(100)  VALUE SPACE.                 
005200 77  RKOD-ABEND-DB2              PIC S9(4)   COMP VALUE +998.             
005210 77  RKOD-ABEND-MED-DUMP         PIC S9(4)   COMP VALUE +1000.            
005300     SKIP2                                                                
005310 77  SW-FIRST-TIME               PIC X       VALUE 'J'.                   
005320     88  FIRST-TIME                          VALUE 'J'.                   
005400                                                                          
005410*01  -COPY WWDCKONS                                                       
005420                                                                          
005500 77  SW-WDK601                   PIC X       VALUE 'N'.                   
005600     88  WDK601                              VALUE 'J'.                   
005700 77  SW-WDK611                   PIC X       VALUE 'N'.                   
005800     88  WDK611                              VALUE 'J'.                   
005900 77  SW-WDK613                   PIC X       VALUE 'N'.                   
006000     88  WDK613                              VALUE 'J'.                   
006010 77  SW-WDK623                   PIC X       VALUE 'N'.                   
006020     88  WDK623                              VALUE 'J'.                   
006100                                                                          
006200     SKIP2                                                                
006300 01  FELTEXT.                                                             
006400     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
006500     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
006600                                                                          
006700 77  FILLER                      PIC X(16) VALUE 'PERFORM-SEKT='.         
006800 77  PERFORM-SEKT                PIC X(80) VALUE SPACE.                   
006900 77  FILLER                      PIC X(16) VALUE 'DB2-SEKTION ='.         
007000 77  DB2-SEKTION                 PIC X(80) VALUE SPACE.                   
007100     EJECT                                                                
007200                                                                          
007300 01  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
007400 01  FILLER REDEFINES DAGENS-DATUM.                                       
007500     03  DAGENS-DATUM-AAR        PIC 9(2).                                
007600     03  DAGENS-DATUM-MAANAD     PIC 9(2).                                
007700     03  DAGENS-DATUM-DAG        PIC 9(2).                                
007800     EJECT                                                                
007900                                                                          
007901 01  WS-SULEVANT-AREA.                                                    
007902     03  WS-SULEVANT-ACK           PIC  9(5)    VALUE ZERO.               
007903     03  WS-SULEVANT-RED.                                                 
007904        05 WS-SULEVANT-OP          PIC  X(1)    VALUE SPACE.              
007905        05 WS-SULEVANT-NO          PIC  9(3)    VALUE ZERO.               
007910 01  WS-DAAVROP-AREA.                                                     
007920     03 WS-DAAVROP.                                                       
007930        05 WS-DAAVROP-SEK          PIC 9(2).                              
007940        05 WS-DAAVROP-AAVV         PIC 9(4).                              
007950                                                                          
007960 01  WS-TILEVBSK-AREA.                                                    
007970     03 WS-TILEVBSK.                                                      
007980        05 WS-TILEVBSK-SEK         PIC 9(2).                              
007990        05 WS-TILEVBSK-AAVVD       PIC 9(5).                              
007991                                                                          
007992 01  WS-TIAVRDAT-AREA.                                                    
007993     03 WS-TIAVRDAT.                                                      
007994        05 WS-TIAVRDAT-SEK         PIC 9(2).                              
007995        05 WS-TIAVRDAT-AAVVD       PIC 9(5).                              
007996                                                                          
007997 01  WS-TIAVIDAT-AREA.                                                    
007998     03 WS-TIAVIDAT.                                                      
007999        05 WS-TIAVIDAT-SEK         PIC 9(2).                              
008000        05 WS-TIAVIDAT-AAVVD       PIC 9(5).                              
008001     EJECT                                                                
008002                                                                          
008003 01  WS-DUMP             PIC X(20) VALUE 'DUMPAREA'.                      
008004 01  WS-DUMP-AREA.                                                        
008005     03 WS-KDAVROP-DUMP  PIC S9(1)      COMP-3 VALUE 0.                   
008006     03 WS-TILEVBSK-DUMP PIC S9(7)      COMP-3 VALUE 0.                   
008007     03 WS-TIAVRDAT-DUMP PIC S9(7)      COMP-3 VALUE 0.                   
008008     03 WS-PRARTBTO-DUMP PIC S9(7)V9(2) COMP-3 VALUE 0.                   
008009     03 WS-TIAVIDAT-DUMP PIC S9(7)      COMP-3 VALUE 0.                   
008010     03 WS-SULEVANT-DUMP PIC 9(5) VALUE 0.                                
008011                                                                          
008012 01  WS-DISPLAY              PIC X(20) VALUE 'DISPLAY '.                  
008013 01  WS-DISPLAY-AREA.                                                     
008014     03 WS-IDFKNGRP-DIS      PIC 9(4).                                    
008015     03 WS-IDAOTUTG-DIS      PIC 9(2).                                    
008016     03 WS-VKART-KDP-DIS     PIC 9(8).                                    
008017     03 WS-KVFOTO-DIS        PIC 9(2).                                    
008018     03 WS-IDKDPPOS-DIS      PIC 9(3).                                    
008019     03 WS-IDARTNR-OFARG-DIS PIC 9(8).                                    
008020     EJECT                                                                
008021                                                                          
008022 01  DYNAMISKA-SUBPROGRAM.                                                
008100*                                                                         
008200     03  ABEND                   PIC X(8)    VALUE 'ABEND   '.            
008300     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
008400     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
008500     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
008600     03  WDATKONV                PIC X(8)    VALUE 'WDATKONV'.            
008700     EJECT                                                                
008800*    --- PARAMETRAR TILL POSTSUM                                          
008900*                                                                         
009000*01  -COPY W0005   -PRE  POSTSUM-                                         
009100     EJECT                                                                
009200                                                                          
009300*01  -COPY WDATAREA                                                       
009400     EJECT                                                                
009500                                                                          
009600 01  SYSIN-PARM-START      PIC X(24) VALUE 'SYSIN-PARM-START'.            
009700     SKIP2                                                                
009800 01  SYSIN-AREA            PIC X(80) VALUE SPACE.                         
009900                                                                          
010000 01  PARM-AREA.                                                           
010100     03 PARM-XLSFIL        PIC X(34) VALUE SPACE.                         
010200     03 PARM-TIAOINF-FOM   PIC 9(6)  VALUE ZERO.                          
010300     03 PARM-TIAOINF-TOM   PIC 9(6)  VALUE ZERO.                          
010400     03 PARM-IDUSER        PIC X(8)  VALUE SPACE.                         
010500     03 PARM-IDUPPDSU      PIC X(8)  VALUE SPACE.                         
010600     03 PARM-IDUPPDKU      PIC X(8)  VALUE SPACE.                         
010700     03 PARM-CALL-TYPE     PIC X(3)  VALUE SPACE.                         
010800                                                                          
010900     EJECT                                                                
011000 01  WS-CALLDB2-AREA.                                                     
011100     03 CALL-TABELL.                                                      
011200        05 CALL-TIAOINF          PIC X     VALUE 'N'.                     
011300        05 CALL-IDUPPDKU         PIC X     VALUE 'N'.                     
011400        05 CALL-IDUPPDSU         PIC X     VALUE 'N'.                     
011500                                                                          
011600     03 WS-CALL-TYPE-DB2 REDEFINES CALL-TABELL PIC XXX.                   
011700*                     --  SU                                              
011800        88 CALL-TYPE-1                      VALUE 'NNJ'.                  
011900*                     --  KU                                              
012000        88 CALL-TYPE-2                      VALUE 'NJN'.                  
012100*                     --  KU + SU                                         
012200        88 CALL-TYPE-3                      VALUE 'NJJ'.                  
012300*                     --  ÄO                                              
012400        88 CALL-TYPE-4                      VALUE 'JNN'.                  
012500*                     --  ÄO + SU                                         
012600        88 CALL-TYPE-5                      VALUE 'JNJ'.                  
012700*                     --  ÄO + KU                                         
012800        88 CALL-TYPE-6                      VALUE 'JJN'.                  
012900*                     --  ÄO + KU + SU                                    
013000        88 CALL-TYPE-7                      VALUE 'JJJ'.                  
013100     EJECT                                                                
013200                                                                          
013300 77  W-TIAOINF-FOM               PIC S9(7) COMP-3 VALUE +0200101.         
013400 77  W-TIAOINF-TOM               PIC S9(7) COMP-3 VALUE +0201052.         
013500 77  WS-TIAOINF-X                PIC 9(6)  VALUE ZERO.                    
013600 77  WS-NUM-IDANSK               PIC 9(3)   VALUE ZERO.                   
013700 77  W-IDUPPDKU                  PIC X(8)   VALUE SPACE.                  
013800 77  W-IDUPPDSU                  PIC X(8)   VALUE SPACE.                  
013900 77  WS-KDFRPTYP                 PIC X      VALUE SPACE.                  
014000                                                                          
014100                                                                          
014200 01  UT-XLS-AREA-START     PIC X(24)   VALUE 'UT-XLS-AREA-START'.         
014300     SKIP2                                                                
014400*01  XLS-AREA -COPY WB1103     -PRE UT-                                   
014500     EJECT                                                                
014600 01  UT-RUB-AREA-START     PIC X(24)   VALUE 'UT-RUB-AREA-START'.         
014700     SKIP2                                                                
014800*01  XLS-RUB-AREA -COPY WB1103R     -PRE UT-                              
014900     EJECT                                                                
014910 01  UT-R2-AREA-START     PIC X(24)   VALUE 'UT-R2-AREA-START'.           
014920     SKIP2                                                                
014930*01  XLS-R2-AREA -COPY WB1103R2    -PRE UT-                               
014940     EJECT                                                                
015000                                                                          
015100 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
015200     SKIP3                                                                
015300 01  NYCKLAR-TILL-DLI.                                                    
015400     03  W-IDARTNR-X.                                                     
015500         05  W-IDARTNR           PIC S9(9)   VALUE ZERO COMP-3.           
015600     03  W-IDLEVNR-X.                                                     
015700         05  W-IDLEVNR           PIC X(5)    VALUE SPACE.                 
015800                                                                          
016120*    WDD9 KEYS                                                            
016130     03  W-WDD901KY-X.                                                    
016140         05  W-IDARTNR-D9        PIC S9(9)   VALUE ZERO COMP-3.           
016150         05  W-IDDC-D9           PIC X(2)    VALUE SPACE.                 
016160     03  W-WDD905KY-X.                                                    
016170         05  W-DAAVROP-X.                                                 
016180             07  W-DAAVROP       PIC  9(6)   VALUE ZERO.                  
016190         05  W-TILEVDAG-X.                                                
016191             07  W-TILEVDAG      PIC  S9     VALUE ZERO COMP-3.           
016192     03  W-KDAVROP-X.                                                     
016193         05  W-KDAVROP           PIC S9(1)   VALUE ZERO COMP-3.           
016194     03  W-DALEVBSK-AVS-X.                                                
016195         05 W-DALEVBSK-AVS       PIC  9(8)   VALUE ZERO.                  
016200                                                                          
016210*    WDK6 KEYS                                                            
016220     03  W-KDSEGKEY-X.                                                    
016230         05  W-KDSEGKEY          PIC X(1)    VALUE '1'.                   
016240                                                                          
016250*    WDC1 KEYS                                                            
016260     03  W-WDC101KY-X.                                                    
016270         05  W-IDARTNR-WDC       PIC S9(9)   VALUE ZERO COMP-3.           
016280         05  W-IDMARKBO-WDC      PIC X       VALUE SPACE.                 
016290*    WDL2 KEYS                                                            
016291     03  W-IDPTYP-X.                                                      
016292         05  W-IDPTYP            PIC X(3)    VALUE 'R32'.                 
019500     SKIP2                                                                
019600*    --- STATUS-KOD FRÅN DLI                                              
019700 01  STATUS-WS                   PIC XX.                                  
019800     88  SEGMENT-FINNS                       VALUE '  '.                  
019900     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
020000     88  SEGMENT-SLUT                        VALUE 'GB'.                  
020100     88  IMS-EJ-OK                           VALUE 'XD'.                  
020200     SKIP2                                                                
020300 01  GODK-STATUSKODER.                                                    
020400     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
020500     SKIP3                                                                
020600 01  SSA1                        PIC X(64).                               
020700 01  SSA2                        PIC X(64).                               
020800     EJECT                                                                
020900*    --- IMS FUNKTIONSKODER                                               
021000*01  -COPY W0003                                                          
021100     EJECT                                                                
021200 01  FILLER                      PIC X(16)  VALUE 'TB1ACCE-AREA'.         
021300*01  -COPY TB1ACCE -PRE ACCE-                                             
021400     EJECT                                                                
021500 01  FILLER                      PIC X(16)   VALUE 'SQLCA-AREA'.          
021600       EXEC SQL INCLUDE SQLCA END-EXEC.                                   
021700     EJECT                                                                
021800     EXEC SQL INCLUDE TB1ACCE END-EXEC.                                   
021900                                                                          
022000 01  FILLER                      PIC X(16)   VALUE 'SQLCODE-WS'.          
022100 01  DB2-WS.                                                              
022200     03  SQLCODE-WS              PIC 9(3)    VALUE 000.                   
022300         88  CURSOR-OK                       VALUE 000.                   
022400         88  RADER-FINNS                     VALUE 000.                   
022500         88  RADER-SAKNAS                    VALUE 100.                   
022600         88  ATKOMST-FEL                     VALUE 904.                   
022700     03  GODK-SQLCODEKODER.                                               
022800         05  GODK-SQLCODE OCCURS 5                                        
022900             INDEXED BY SQLCODE-IX PIC 9(3).                              
023000*    ---  DLI INPUT-OUTPUT AREA                                           
023100                                                                          
023200 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDD201'.                      
023300 01  DLI-IO-WDD201.                                                       
023400*    03  -COPY WDD201  -PRE WDD2-                                         
023500 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDD902'.                      
023600 01  DLI-IO-WDD902.                                                       
023700*    03  -COPY WDD902  -PRE WDD902-                                       
023800 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDD905'.                      
023900 01  DLI-IO-WDD905.                                                       
024000*    03  -COPY WDD905  -PRE WDD905-                                       
024010 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDD924'.                      
024020 01  DLI-IO-WDD924.                                                       
024030*    03  -COPY WDD924  -PRE WDD924-                                       
024100 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK601'.                      
024200 01  DLI-IO-WDK601.                                                       
024300*    03  -COPY WDK601  -PRE WDK6-                                         
024400     EJECT                                                                
024500 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK611'.                      
024600 01  DLI-IO-WDK611.                                                       
024700*    03  -COPY WDK611  -PRE WDK6-                                         
024800     EJECT                                                                
024900 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK613'.                      
025000 01  DLI-IO-WDK613.                                                       
025100*    03  -COPY WDK613  -PRE WDK6-                                         
025200     EJECT                                                                
025210 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK623'.                      
025220 01  DLI-IO-WDK623.                                                       
025230*    03  -COPY WDK623  -PRE WDK6-                                         
025240     EJECT                                                                
025250 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDC101'.                      
025260 01  DLI-IO-WDC101.                                                       
025270*    03  -COPY WDC101  -PRE WDC1-                                         
025280     EJECT                                                                
025290 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDL201'.                      
025291 01  DLI-IO-WDL201.                                                       
025292*    03  -COPY WDL201  -PRE WDL201-                                       
025293 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDL211'.                      
025294 01  DLI-IO-WDL211.                                                       
025295*    03  -COPY WDL211  -PRE WDL211-                                       
025296 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDL221'.                      
025297 01  DLI-IO-WDL221.                                                       
025298*    03  -COPY WDL221  -PRE WDL221-                                       
025299     EJECT                                                                
025300                                                                          
025400 LINKAGE SECTION.                                                         
025500                                                                          
025600*01  -COPY W0009   -PRE MSG-                                              
025700                                                                          
025800*01  -COPY W0008  -PRE WDD2-                                              
025900     05  FILLER                  PIC X.                                   
026000                                                                          
026100*01  -COPY W0008  -PRE WDD9-                                              
026200     05  FILLER                  PIC X.                                   
026300                                                                          
026400*01  -COPY W0008  -PRE WDK6-                                              
026500     05  FILLER                  PIC X.                                   
026600                                                                          
026610*01  -COPY W0008  -PRE WDC1-                                              
026620     05  FILLER                  PIC X.                                   
026630                                                                          
026640*01  -COPY W0008  -PRE WDL2-                                              
026650     05  FILLER                  PIC X.                                   
026660                                                                          
026700     EJECT                                                                
026800 PROCEDURE DIVISION  USING MSG-PCB WDD2-PCB WDD9-PCB WDK6-PCB             
026810                                   WDC1-PCB WDL2-PCB.                     
026900 MAIN SECTION.                                                            
027000     ENTRY 'DLITCBL' USING MSG-PCB WDD2-PCB WDD9-PCB  WDK6-PCB            
027010                                   WDC1-PCB WDL2-PCB.                     
027100     SKIP2                                                                
027200     PERFORM A-INIT                                                       
027300                                                                          
027400     IF RADER-FINNS                                                       
027500*        -- FETCH THE FIRST LINE IN SELECTION RANGE FROM DB2              
027600         EVALUATE TRUE                                                    
027700           WHEN CALL-TYPE-1 PERFORM DB2-FETCH-TB1ACCE-CRS-1               
027800           WHEN CALL-TYPE-2 PERFORM DB2-FETCH-TB1ACCE-CRS-2               
027900           WHEN CALL-TYPE-3 PERFORM DB2-FETCH-TB1ACCE-CRS-3               
028000           WHEN CALL-TYPE-4 PERFORM DB2-FETCH-TB1ACCE-CRS-4               
028100           WHEN CALL-TYPE-5 PERFORM DB2-FETCH-TB1ACCE-CRS-5               
028200           WHEN CALL-TYPE-6 PERFORM DB2-FETCH-TB1ACCE-CRS-6               
028300           WHEN CALL-TYPE-7 PERFORM DB2-FETCH-TB1ACCE-CRS-7               
028400           WHEN OTHER SET RADER-SAKNAS TO TRUE                            
028410                      DISPLAY 'INGEN CALL-TYPE ! I STYR'                  
028500         END-EVALUATE                                                     
028600         IF RADER-FINNS                                                   
028700             PERFORM F-PROCESS-AND-PUT-OUTPUT-DATA                        
028800         END-IF                                                           
028900     END-IF                                                               
029000                                                                          
029100     PERFORM Z-FINIT                                                      
029200                                                                          
029300     MOVE ZERO TO RETURN-CODE                                             
029400     GOBACK                                                               
029500     .                                                                    
029600     EJECT                                                                
029700                                                                          
029800 A-INIT SECTION.                                                          
029810     MOVE 'A-INIT     ' TO PERFORM-SEKT                                   
029900     SKIP2                                                                
029910     MOVE FUNCTION CURRENT-DATE(1:8) TO DAGENS-DATUM                      
030000     OPEN OUTPUT WB1103                                                   
030100                                                                          
030200     ACCEPT SYSIN-AREA FROM SYSIN                                         
030300                                                                          
030400     UNSTRING SYSIN-AREA DELIMITED BY ',' INTO                            
030500     PARM-XLSFIL                                                          
030600     PARM-TIAOINF-FOM                                                     
030700     PARM-TIAOINF-TOM                                                     
030800     PARM-IDUSER                                                          
030900     PARM-IDUPPDSU                                                        
031000     PARM-IDUPPDKU                                                        
031100     PARM-CALL-TYPE                                                       
031200                                                                          
031300     MOVE PARM-TIAOINF-FOM TO W-TIAOINF-FOM                               
031400     MOVE PARM-TIAOINF-TOM TO W-TIAOINF-TOM                               
031500     MOVE PARM-IDUPPDSU    TO W-IDUPPDSU                                  
031600     MOVE PARM-IDUPPDKU    TO W-IDUPPDKU                                  
031700                                                                          
031800     MOVE PARM-CALL-TYPE   TO CALL-TABELL                                 
031801             DISPLAY 'CALL-TABELL '                                       
031810                                 CALL-TIAOINF ','                         
031820                                 CALL-IDUPPDKU ','                        
031830                                 CALL-IDUPPDSU                            
032000             DISPLAY 'TIAOINF-FOM' PARM-TIAOINF-FOM                       
032100             DISPLAY 'TIAOINF-TOM' PARM-TIAOINF-TOM                       
032200             DISPLAY 'IDUPPDSU   ' PARM-IDUPPDSU                          
032300             DISPLAY 'IDUPPDKU   ' PARM-IDUPPDKU                          
032400                                                                          
032500     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
032600                                                                          
032700     INITIALIZE GODK-SQLCODEKODER                                         
032800     PERFORM AA-INITIALIZE-UT-XLS-AREA                                    
032900                                                                          
033000     EVALUATE TRUE                                                        
033100       WHEN CALL-TYPE-1  PERFORM DB2-DCL-OPN-TB1ACCE-CRS-1                
033200                        DISPLAY 'DB2-DCL-OPN-TB1ACCE-CRS-1'               
033300       WHEN CALL-TYPE-2  PERFORM DB2-DCL-OPN-TB1ACCE-CRS-2                
033400                        DISPLAY 'DB2-DCL-OPN-TB1ACCE-CRS-2'               
033500       WHEN CALL-TYPE-3  PERFORM DB2-DCL-OPN-TB1ACCE-CRS-3                
033600                        DISPLAY 'DB2-DCL-OPN-TB1ACCE-CRS-3'               
033700       WHEN CALL-TYPE-4  PERFORM DB2-DCL-OPN-TB1ACCE-CRS-4                
033800                        DISPLAY 'DB2-DCL-OPN-TB1ACCE-CRS-4'               
033900       WHEN CALL-TYPE-5  PERFORM DB2-DCL-OPN-TB1ACCE-CRS-5                
034000                        DISPLAY 'DB2-DCL-OPN-TB1ACCE-CRS-5'               
034100       WHEN CALL-TYPE-6  PERFORM DB2-DCL-OPN-TB1ACCE-CRS-6                
034200                        DISPLAY 'DB2-DCL-OPN-TB1ACCE-CRS-6'               
034300       WHEN CALL-TYPE-7  PERFORM DB2-DCL-OPN-TB1ACCE-CRS-7                
034400                        DISPLAY 'DB2-DCL-OPN-TB1ACCE-CRS-7'               
034500       WHEN OTHER                                                         
034600             SET RADER-SAKNAS TO TRUE                                     
034610             DISPLAY 'INGEN CALL-TYPE ! I A-INIT'                         
034700     END-EVALUATE                                                         
034800     .                                                                    
034900     EJECT                                                                
035000 AA-INITIALIZE-UT-XLS-AREA SECTION.                                       
035010     MOVE 'AA-INITIALIZE-UT-XLS-AREA  ' TO PERFORM-SEKT                   
035100     SKIP2                                                                
035200     INITIALIZE UT-XLS-AREA                                               
035300     MOVE TAB TO                                                          
035400     UT-XLS-TAB01  UT-XLS-TAB21  UT-XLS-TAB41  UT-XLS-TAB61               
035500     UT-XLS-TAB02  UT-XLS-TAB22  UT-XLS-TAB42  UT-XLS-TAB62               
035600     UT-XLS-TAB03  UT-XLS-TAB23  UT-XLS-TAB43  UT-XLS-TAB63               
035700     UT-XLS-TAB04  UT-XLS-TAB24  UT-XLS-TAB44  UT-XLS-TAB64               
035800     UT-XLS-TAB05  UT-XLS-TAB25  UT-XLS-TAB45  UT-XLS-TAB65               
035900     UT-XLS-TAB06  UT-XLS-TAB26  UT-XLS-TAB46  UT-XLS-TAB66               
036000     UT-XLS-TAB07  UT-XLS-TAB27  UT-XLS-TAB47  UT-XLS-TAB67               
036100     UT-XLS-TAB08  UT-XLS-TAB28  UT-XLS-TAB48  UT-XLS-TAB68               
036200     UT-XLS-TAB09  UT-XLS-TAB29  UT-XLS-TAB49  UT-XLS-TAB69               
036300     UT-XLS-TAB10  UT-XLS-TAB30  UT-XLS-TAB50  UT-XLS-TAB70               
036400     UT-XLS-TAB11  UT-XLS-TAB31  UT-XLS-TAB51  UT-XLS-TAB71               
036500     UT-XLS-TAB12  UT-XLS-TAB32  UT-XLS-TAB52                             
036600     UT-XLS-TAB13  UT-XLS-TAB33  UT-XLS-TAB53                             
036700     UT-XLS-TAB14  UT-XLS-TAB34  UT-XLS-TAB54                             
036800     UT-XLS-TAB15  UT-XLS-TAB35  UT-XLS-TAB55                             
036900     UT-XLS-TAB16  UT-XLS-TAB36  UT-XLS-TAB56                             
037000     UT-XLS-TAB17  UT-XLS-TAB37  UT-XLS-TAB57                             
037100     UT-XLS-TAB18  UT-XLS-TAB38  UT-XLS-TAB58                             
037200     UT-XLS-TAB19  UT-XLS-TAB39  UT-XLS-TAB59                             
037300     UT-XLS-TAB20  UT-XLS-TAB40  UT-XLS-TAB60                             
037400     .                                                                    
037500     EJECT                                                                
037600                                                                          
037700 F-PROCESS-AND-PUT-OUTPUT-DATA    SECTION.                                
037800     MOVE 'F-PROCESS-AND-PUT-OUTPUT-DATA' TO PERFORM-SEKT                 
037900     SKIP2                                                                
038000     MOVE ZERO TO W-IDARTNR                                               
038100                  W-TIAOINF-FOM                                           
038200                  W-TIAOINF-TOM                                           
038300     PERFORM FA-SKRIV-RUBRIK                                              
038400                                                                          
038500*    DISPLAY '1. RUBRIK SKRIVEN I FA-     '                               
038600                                                                          
038700     PERFORM UNTIL RADER-SAKNAS                                           
038800                                                                          
038900*      --- FÖRSTA DB2-FETCH ÄR GJORD I STYR-SEC                           
039000                                                                          
039100*      DISPLAY 'F. ACCE-IDARTNR ' ACCE-IDARTNR                            
039200                                                                          
039300       PERFORM FC1-CHECK-WDK6                                             
039400*      --- READS WDK601,    WDK611,     WDK613  AND WDK623                
039410                                                                          
040500       PERFORM FC2-FILL-THE-OUTPUT-AREA                                   
040600       PERFORM S11-SKRIV-OUTPUT-WB1103                                    
040700       EVALUATE TRUE                                                      
040800         WHEN CALL-TYPE-1 PERFORM DB2-FETCH-TB1ACCE-CRS-1                 
040900         WHEN CALL-TYPE-2 PERFORM DB2-FETCH-TB1ACCE-CRS-2                 
041000         WHEN CALL-TYPE-3 PERFORM DB2-FETCH-TB1ACCE-CRS-3                 
041100         WHEN CALL-TYPE-4 PERFORM DB2-FETCH-TB1ACCE-CRS-4                 
041200         WHEN CALL-TYPE-5 PERFORM DB2-FETCH-TB1ACCE-CRS-5                 
041300         WHEN CALL-TYPE-6 PERFORM DB2-FETCH-TB1ACCE-CRS-6                 
041400         WHEN CALL-TYPE-7 PERFORM DB2-FETCH-TB1ACCE-CRS-7                 
041500         WHEN OTHER                                                       
041600              SET RADER-SAKNAS TO TRUE                                    
041700       END-EVALUATE                                                       
041800     END-PERFORM                                                          
041900     .                                                                    
042000     EJECT                                                                
042100 FA-SKRIV-RUBRIK         SECTION.                                         
042200     MOVE 'FA-SKRIV-RUBRIK  ' TO PERFORM-SEKT                             
042300     SKIP2                                                                
042400     WRITE UT-RUB-POST FROM UT-XLS-RUB-AREA                               
042410     WRITE UT-R2-POST FROM UT-XLS-R2-AREA                                 
042500     .                                                                    
042600     EJECT                                                                
042700*                                                                         
042800 FC1-CHECK-WDK6    SECTION.                                               
042900     MOVE 'FC1-CHECK-WDK6    ' TO PERFORM-SEKT                            
043000     SKIP2                                                                
043100*    DISPLAY 'FC1. ACCE-IDARTNR ' ACCE-IDARTNR                            
043200     MOVE ACCE-IDARTNR TO W-IDARTNR                                       
043300                                                                          
043400     MOVE NEJ TO SW-WDK601 SW-WDK611 SW-WDK613 SW-WDK623                  
043500                                                                          
043600     PERFORM DLI-GU-WDK601                                                
043700     IF SEGMENT-FINNS                                                     
043800       SET WDK601 TO TRUE                                                 
043900                                                                          
044000       PERFORM DLI-GNP-WDK611                                             
044100       IF SEGMENT-FINNS                                                   
044200         SET WDK611 TO TRUE                                               
044210         PERFORM DLI-GNP-WDK623                                           
044220         IF SEGMENT-FINNS                                                 
044230           SET WDK623 TO TRUE                                             
044300         END-IF                                                           
044310       END-IF                                                             
044400       PERFORM DLI-GNP-WDK613                                             
044500       IF SEGMENT-FINNS                                                   
044600         SET WDK613 TO TRUE                                               
044700       END-IF                                                             
044800     END-IF                                                               
044900*    DISPLAY 'SW-WDK601 SW-WDK611 SW-WDK613 SW-WDK623'                    
045000*        SW-WDK601 ',' SW-WDK611 ',' SW-WDK613 ',' SW-WDK623              
045100     .                                                                    
045200     EJECT                                                                
045300*                                                                         
045400 FC2-FILL-THE-OUTPUT-AREA  SECTION.                                       
045500     MOVE 'FC2-FILL-THE-OUTPUT-AREA ' TO PERFORM-SEKT                     
045600     SKIP2                                                                
045700     PERFORM FCA-MOVE-CURRENT-DB2-RECORD                                  
045800*                                                                         
045900*    --- ADD UP WITH DATA FROM PULS DL1-BASES                             
046000     IF WDK601                                                            
046100       PERFORM FCB0-MOVE-CURRENT-WDK601-DATA                              
046200       IF WDK611                                                          
046300         PERFORM FCB1-MOVE-CURRENT-WDK611-DATA                            
046301                                                                          
046310         MOVE ZERO             TO UT-XLS-TIAVTAL                          
046320         IF WDK623                                                        
046330           IF  WDK6-CLAG-KDAVT = +1                                       
046331           AND WDK6-AVT-IDLEVNR-AVT = WDK6-ART-IDLEVNR                    
046340             PERFORM FCB2-MOVE-CURRENT-WDK623-DATA                        
046350           END-IF                                                         
046360         END-IF                                                           
046400       ELSE                                                               
046500*        --- CLEAR ALL WDK611-DATA                                        
046600         MOVE OBSOLETE TO UT-XLS-FLRPULS                                  
046610                                                                          
046701         MOVE SPACE           TO UT-XLS-IDINK                             
046900         MOVE ZERO     TO UT-XLS-IDANSK  WS-NUM-IDANSK                    
047000         MOVE ZERO     TO UT-XLS-KDEMBKOD-2                               
047100                          UT-XLS-KVLS                                     
047210                          UT-XLS-TIAVTAL                                  
047300       END-IF                                                             
047301                                                                          
047400*                                                                         
047500       IF WDK613                                                          
047600         IF WDK6-EMB-KDEMBKOD = +073                                      
047700           MOVE 'S'      TO UT-XLS-KDFRPTYP                               
047800         ELSE                                                             
047900           MOVE 'P'      TO UT-XLS-KDFRPTYP                               
048000         END-IF                                                           
048100       ELSE                                                               
048200***      --- UPPGIFT SAKNAS I PULS                                        
048300         IF WS-KDFRPTYP = SPACE                                           
048400           MOVE SPACE    TO UT-XLS-KDFRPTYP                               
048500         ELSE                                                             
048600***        --- MANUELLT INLAGD KDFRPTYP LÄGGS UT                          
048700           MOVE WS-KDFRPTYP TO UT-XLS-KDFRPTYP                            
048800         END-IF                                                           
048900       END-IF                                                             
049000     ELSE                                                                 
049100       PERFORM FCC-CLEAR-UT-WDK6-DATA                                     
049200       PERFORM FCG-CLEAR-UT-WDD9-DATA                                     
049300       MOVE ZERO TO UT-XLS-KVLS                                           
049400     END-IF                                                               
049500                                                                          
049501*    WDD9                                                                 
049510     MOVE W-IDARTNR  TO W-IDARTNR-D9                                      
049520     MOVE WC-CDC-SE  TO W-IDDC-D9                                         
049600     PERFORM DLI-GU-WDD902                                                
049700     IF SEGMENT-FINNS                                                     
049710*    WDD905:1                                                             
049800       PERFORM FCF-MOVE-WDD9-DATA-TO-XLS                                  
049900     ELSE                                                                 
050000       PERFORM FCG-CLEAR-UT-WDD9-DATA                                     
050100     END-IF                                                               
050200                                                                          
050210     MOVE W-IDARTNR  TO W-IDARTNR-D9                                      
050220     MOVE WC-CDC-SE  TO W-IDDC-D9                                         
050230     PERFORM DLI-GU-WDD902                                                
050240     IF SEGMENT-FINNS                                                     
050250*    WDD924                                                               
050260*    HÄMTA TILEVBSK-INL FRÅN AKTUELL LEVERANSPLAN                         
050270       PERFORM FCH-MOVE-WDD924-DATA-TO-RESP                               
050280       IF SEGMENT-SAKNAS                                                  
050290         MOVE W-IDARTNR  TO W-IDARTNR-D9                                  
050291         MOVE WC-CDC-SE  TO W-IDDC-D9                                     
050292         PERFORM DLI-GU-WDD902                                            
050293*    WDD905:2                                                             
050294*    HÄMTA TIAVRDAT-INL FÖR AKTUELLT GODKÄNT AVROP                        
050295         PERFORM FCI-MOVE-WDD905-DATA-TO-RESP                             
050296       END-IF                                                             
050297     END-IF                                                               
050298                                                                          
050299*    WDD2                                                                 
050400     PERFORM DLI-GU-WDD201                                                
050500     IF SEGMENT-FINNS                                                     
050600       PERFORM FCD-MOVE-CURRENT-WDD2-DATA                                 
050700     ELSE                                                                 
050800       PERFORM FCE-CLEAR-UT-WDD2-DATA                                     
050900     END-IF                                                               
050901                                                                          
050910*    WDC1                                                                 
050920     PERFORM FCK-CLEAR-RESP-WDC1-DATA                                     
050930     MOVE W-IDARTNR                TO W-IDARTNR-WDC                       
050940     MOVE 'B'                      TO W-IDMARKBO-WDC                      
050950*    HÄMTA PRARTBTO FRÅN WDC101                                           
050960     PERFORM DLI-GU-WDC101                                                
050970     IF SEGMENT-FINNS                                                     
050980       MOVE WDC1-ART-PRARTBTO-MARK TO WS-PRARTBTO-DUMP                    
050990       PERFORM FCJ-MOVE-CURRENT-WDC1-DATA                                 
050991     END-IF                                                               
050992                                                                          
050993*    WDL2                                                                 
050994     PERFORM FCN-CLEAR-RESP-WDL2-DATA                                     
050995*    HÄMTA SENASTE TIAVIDAT                                               
050996*    SUMMERA KVAVIS HISTORISKT TILLS > 99                                 
050997     PERFORM FCL-MOVE-CURRENT-WDL2-DATA                                   
051000     .                                                                    
051100     EJECT                                                                
051200 FCA-MOVE-CURRENT-DB2-RECORD  SECTION.                                    
051300     MOVE    'FCA-MOVE-CURRENT-DB2-RECORD'  TO PERFORM-SEKT               
051310*    DISPLAY 'FCA-MOVE-CURRENT-DB2-RECORD'                                
051400     SKIP2                                                                
051594     MOVE   ACCE-KDANNULL    TO UT-XLS-KDANNULL                           
051595     MOVE   UT-XLS-KDANNULL  TO WS-TEXT                                   
051596     PERFORM S99-INSPECT-REVERSE                                          
051607     MOVE   WS-TEXT          TO UT-XLS-KDANNULL                           
051608                                                                          
051610     MOVE   ACCE-IDARTNR     TO UT-XLS-IDARTNR                            
051700     MOVE   ACCE-KDARTTYP    TO UT-XLS-KDARTTYP                           
051905     MOVE   UT-XLS-KDARTTYP  TO WS-TEXT                                   
051906     PERFORM S99-INSPECT-REVERSE                                          
051940     MOVE   WS-TEXT          TO UT-XLS-KDARTTYP                           
051941                                                                          
051942     MOVE   ACCE-TEARTUTFG   TO UT-XLS-TEARTUTFG                          
051943     MOVE   UT-XLS-TEARTUTFG TO WS-TEXT                                   
051944     PERFORM S99-INSPECT-REVERSE                                          
051945     MOVE   WS-TEXT          TO UT-XLS-TEARTUTFG                          
051960                                                                          
051980     MOVE   ACCE-BEART       TO UT-XLS-BEART                              
051990     MOVE   UT-XLS-BEART     TO WS-TEXT                                   
052000     PERFORM S99-INSPECT-REVERSE                                          
052002     MOVE   WS-TEXT          TO UT-XLS-BEART                              
052003                                                                          
052040     MOVE   ACCE-TENOTE      TO UT-XLS-TENOTE                             
052050     MOVE   UT-XLS-TENOTE    TO WS-TEXT                                   
052060     PERFORM S99-INSPECT-REVERSE                                          
052097     MOVE   WS-TEXT          TO UT-XLS-TENOTE                             
052098                                                                          
052100     MOVE   ACCE-IDPRODGR    TO UT-XLS-IDPRODGR                           
052101     MOVE   UT-XLS-IDPRODGR  TO WS-TEXT                                   
052102     PERFORM S99-INSPECT-REVERSE                                          
052103     MOVE   WS-TEXT          TO UT-XLS-IDPRODGR                           
052210                                                                          
052300     IF ACCE-IDUPPDSU = '00000000'                                        
052400       MOVE SPACE            TO UT-XLS-IDUPPDSU                           
052401     ELSE                                                                 
052402       MOVE ACCE-IDUPPDSU    TO UT-XLS-IDUPPDSU                           
052403     END-IF                                                               
052404     MOVE   UT-XLS-IDUPPDSU  TO WS-TEXT                                   
052405     PERFORM S99-INSPECT-REVERSE                                          
052406     MOVE   WS-TEXT          TO UT-XLS-IDUPPDSU                           
052407                                                                          
052408     IF ACCE-IDUPPDKU = '00000000'                                        
052409       MOVE SPACE            TO UT-XLS-IDUPPDKU                           
052410     ELSE                                                                 
052420       MOVE ACCE-IDUPPDKU    TO UT-XLS-IDUPPDKU                           
052421     END-IF                                                               
052422     MOVE UT-XLS-IDUPPDKU    TO WS-TEXT                                   
052423     PERFORM S99-INSPECT-REVERSE                                          
052424     MOVE   WS-TEXT          TO UT-XLS-IDUPPDKU                           
052425                                                                          
052426     MOVE   ACCE-TESTATUPP   TO UT-XLS-TESTATUPP                          
052500     MOVE   ACCE-IDAOT       TO UT-XLS-IDAOT                              
052501     MOVE   UT-XLS-IDAOT     TO WS-TEXT                                   
052502     PERFORM S99-INSPECT-REVERSE                                          
052503     MOVE   WS-TEXT          TO UT-XLS-IDAOT                              
052504                                                                          
052600     IF ACCE-TIAOINF = ZERO                                               
052700       MOVE ZERO             TO UT-XLS-TIAOINF-AAVV                       
052701     ELSE                                                                 
052702       MOVE ACCE-TIAOINF     TO WS-TIAOINF-X                              
052703       MOVE WS-TIAOINF-X(3:) TO UT-XLS-TIAOINF-AAVV                       
052704     END-IF                                                               
052705     MOVE   UT-XLS-TIAOINF-AAVV                                           
052706                             TO WS-TEXT                                   
052707     PERFORM S99-INSPECT-REVERSE                                          
052708     MOVE   WS-TEXT          TO UT-XLS-TIAOINF-AAVV                       
052709                                                                          
052800     MOVE   ACCE-BEASSTYP    TO UT-XLS-BEASSTYP                           
052801     MOVE   UT-XLS-BEASSTYP  TO WS-TEXT                                   
052802     PERFORM S99-INSPECT-REVERSE                                          
052803     MOVE   WS-TEXT          TO UT-XLS-BEASSTYP                           
052804                                                                          
052900     MOVE   ACCE-KDFRPTYP    TO WS-KDFRPTYP                               
053300     IF ACCE-KDMDS = JA                                                   
053410       MOVE YES              TO UT-XLS-KDMDS                              
053411     ELSE                                                                 
053412       MOVE ACCE-KDMDS       TO UT-XLS-KDMDS                              
053500     END-IF                                                               
053501     MOVE   UT-XLS-KDMDS     TO WS-TEXT                                   
053502     PERFORM S99-INSPECT-REVERSE                                          
053503     MOVE   WS-TEXT          TO UT-XLS-KDMDS                              
053504                                                                          
053505     MOVE  ACCE-IDLEVNR-GSDB TO UT-XLS-IDLEVNR                            
053506                                   W-IDLEVNR                              
053507     IF ACCE-DAPSWQP-1 = SPACE                                            
053508       MOVE NEJ              TO UT-XLS-FLTPDWKPH1                         
053509     ELSE                                                                 
053510       MOVE YES              TO UT-XLS-FLTPDWKPH1                         
053520     END-IF                                                               
053521                                                                          
053522*    --- PSW-PLAN-FÄLTEN VISAS EJ EFTER ÄT 07:9                           
053600     MOVE ACCE-DAPSWQP-1     TO UT-XLS-DAPSWQP-1                          
054410     MOVE   UT-XLS-DAPSWQP-1 TO WS-TEXT                                   
054420     PERFORM S99-INSPECT-REVERSE                                          
054495     MOVE   WS-TEXT          TO UT-XLS-DAPSWQP-1                          
054496                                                                          
054497     MOVE   ACCE-DAPSWPP-2   TO UT-XLS-DAPSWPP-2                          
054498     MOVE   UT-XLS-DAPSWPP-2 TO WS-TEXT                                   
054499     PERFORM S99-INSPECT-REVERSE                                          
054500     MOVE   WS-TEXT          TO UT-XLS-DAPSWPP-2                          
054501                                                                          
054502     MOVE   ACCE-DAPSWCP-3   TO UT-XLS-DAPSWCP-3                          
054503     MOVE   UT-XLS-DAPSWCP-3 TO WS-TEXT                                   
054504     PERFORM S99-INSPECT-REVERSE                                          
054505     MOVE   WS-TEXT          TO UT-XLS-DAPSWCP-3                          
054506                                                                          
054507     MOVE   ACCE-KDPSWQP-1   TO UT-XLS-KDPSWQP-1                          
054508     MOVE   UT-XLS-KDPSWQP-1 TO WS-TEXT                                   
054509     PERFORM S99-INSPECT-REVERSE                                          
054510     MOVE   WS-TEXT          TO UT-XLS-KDPSWQP-1                          
054511                                                                          
054512     MOVE   ACCE-KDPSWPP-2   TO UT-XLS-KDPSWPP-2                          
054513     MOVE   UT-XLS-KDPSWPP-2 TO WS-TEXT                                   
054514     PERFORM S99-INSPECT-REVERSE                                          
054515     MOVE   WS-TEXT          TO UT-XLS-KDPSWPP-2                          
054516                                                                          
054517     MOVE   ACCE-KDPSWCP-3   TO UT-XLS-KDPSWCP-3                          
054518     MOVE   UT-XLS-KDPSWCP-3 TO WS-TEXT                                   
054519     PERFORM S99-INSPECT-REVERSE                                          
054520     MOVE   WS-TEXT          TO UT-XLS-KDPSWCP-3                          
054521                                                                          
054522     MOVE ACCE-DAPSWQA-1     TO UT-XLS-DAPSWQA-1                          
054523     MOVE UT-XLS-DAPSWQA-1   TO WS-TEXT                                   
054524     PERFORM S99-INSPECT-REVERSE                                          
054525     MOVE   WS-TEXT          TO UT-XLS-DAPSWQA-1                          
054526                                                                          
054527     MOVE ACCE-DAPSWPA-2     TO UT-XLS-DAPSWPA-2                          
054528     MOVE UT-XLS-DAPSWPA-2   TO WS-TEXT                                   
054529     PERFORM S99-INSPECT-REVERSE                                          
054530     MOVE   WS-TEXT          TO UT-XLS-DAPSWPA-2                          
054531                                                                          
054532     MOVE   ACCE-DAPSWCA-3   TO UT-XLS-DAPSWCA-3                          
054533     MOVE   UT-XLS-DAPSWCA-3 TO WS-TEXT                                   
054534     PERFORM S99-INSPECT-REVERSE                                          
054535     MOVE   WS-TEXT          TO UT-XLS-DAPSWCA-3                          
054536                                                                          
054537     MOVE   ACCE-KDPSWQA-1   TO UT-XLS-KDPSWQA-1                          
054538     MOVE   UT-XLS-KDPSWQA-1 TO WS-TEXT                                   
054539     PERFORM S99-INSPECT-REVERSE                                          
054540     MOVE   WS-TEXT          TO UT-XLS-KDPSWQA-1                          
054541                                                                          
054542     MOVE   ACCE-KDPSWPA-2   TO UT-XLS-KDPSWPA-2                          
054543     MOVE   UT-XLS-KDPSWPA-2 TO WS-TEXT                                   
054544     PERFORM S99-INSPECT-REVERSE                                          
054590     MOVE   WS-TEXT          TO UT-XLS-KDPSWPA-2                          
054591                                                                          
054593     MOVE   ACCE-KDPSWCA-3   TO UT-XLS-KDPSWCA-3                          
054594     MOVE   UT-XLS-KDPSWCA-3 TO WS-TEXT                                   
054595     PERFORM S99-INSPECT-REVERSE                                          
054650     MOVE   WS-TEXT          TO UT-XLS-KDPSWCA-3                          
054660                                                                          
054800     MOVE   ACCE-KVYVOL-INT  TO UT-XLS-KVYVOL-INT                         
054900     MOVE   ACCE-KVYVOL-B3   TO UT-XLS-KVYVOL-B3                          
055000     MOVE   ACCE-KVYVOL-B2   TO UT-XLS-KVYVOL-B2                          
055100     MOVE   ACCE-KVYVOL-B1   TO UT-XLS-KVYVOL-B1                          
055200     MOVE   ACCE-KVYVOL-ASS  TO UT-XLS-KVYVOL-ASS                         
055300     MOVE   ACCE-BEMAPP      TO UT-XLS-BEMAPP                             
055301     MOVE   UT-XLS-BEMAPP    TO WS-TEXT                                   
055302     PERFORM S99-INSPECT-REVERSE                                          
055303     MOVE   WS-TEXT          TO UT-XLS-BEMAPP                             
055304                                                                          
055400     MOVE ACCE-KVFOTO        TO WS-KVFOTO-DIS                             
055401     IF WS-KVFOTO-DIS = ZERO                                              
055402       MOVE ZERO             TO UT-XLS-KVFOTO                             
055403     ELSE                                                                 
055404       MOVE WS-KVFOTO-DIS    TO UT-XLS-KVFOTO                             
055405     END-IF                                                               
055406     MOVE   UT-XLS-KVFOTO    TO WS-TEXT                                   
055407     PERFORM S99-INSPECT-REVERSE                                          
055408     MOVE   WS-TEXT          TO UT-XLS-KVFOTO                             
055409                                                                          
055500     MOVE   ACCE-TIFOTO      TO UT-XLS-TIFOTO                             
055510     MOVE   UT-XLS-TIFOTO    TO WS-TEXT                                   
055511     PERFORM S99-INSPECT-REVERSE                                          
055512     MOVE   WS-TEXT          TO UT-XLS-TIFOTO                             
055513                                                                          
055514     MOVE   ACCE-TEVERKTYG   TO UT-XLS-TEVERKTYG                          
055515     MOVE   UT-XLS-TEVERKTYG TO WS-TEXT                                   
055516     PERFORM S99-INSPECT-REVERSE                                          
055517     MOVE   WS-TEXT          TO UT-XLS-TEVERKTYG                          
055518                                                                          
055520     MOVE   ACCE-TESTATXT    TO UT-XLS-TESTATXT                           
055521     MOVE   UT-XLS-TESTATXT  TO WS-TEXT                                   
055522     PERFORM S99-INSPECT-REVERSE                                          
055523     MOVE   WS-TEXT          TO UT-XLS-TESTATXT                           
055524                                                                          
055530     MOVE   ACCE-TEMATXT     TO UT-XLS-TEMATXT                            
055531     MOVE   UT-XLS-TEMATXT   TO WS-TEXT                                   
055532     PERFORM S99-INSPECT-REVERSE                                          
055533     MOVE   WS-TEXT          TO UT-XLS-TEMATXT                            
055534                                                                          
055540     MOVE   ACCE-TEINKTXT    TO UT-XLS-TEINKTXT                           
055541     MOVE   UT-XLS-TEINKTXT  TO WS-TEXT                                   
055542     PERFORM S99-INSPECT-REVERSE                                          
055543     MOVE   WS-TEXT          TO UT-XLS-TEINKTXT                           
055544                                                                          
055550     MOVE   ACCE-TEANSTXT    TO UT-XLS-TEANSTXT                           
055551     MOVE   UT-XLS-TEANSTXT  TO WS-TEXT                                   
055552     PERFORM S99-INSPECT-REVERSE                                          
055553     MOVE   WS-TEXT          TO UT-XLS-TEANSTXT                           
055554                                                                          
055560     MOVE   ACCE-TEAUXTXT    TO UT-XLS-TEAUXTXT                           
055561     MOVE   UT-XLS-TEAUXTXT  TO WS-TEXT                                   
055562     PERFORM S99-INSPECT-REVERSE                                          
055563     MOVE   WS-TEXT          TO UT-XLS-TEAUXTXT                           
055564                                                                          
055571     MOVE ACCE-IDARTNR-OFARG     TO WS-IDARTNR-OFARG-DIS                  
055593     IF WS-IDARTNR-OFARG-DIS = ZERO                                       
055594       MOVE ZERO                 TO UT-XLS-IDARTNR-OFARG                  
055595     ELSE                                                                 
055596       MOVE WS-IDARTNR-OFARG-DIS TO UT-XLS-IDARTNR-OFARG                  
055597     END-IF                                                               
055598     MOVE   UT-XLS-IDARTNR-OFARG                                          
055599                                 TO WS-TEXT                               
055600     PERFORM S99-INSPECT-REVERSE                                          
055700     MOVE   WS-TEXT              TO UT-XLS-IDARTNR-OFARG                  
055701                                                                          
055702     MOVE   ACCE-KDFARGST    TO UT-XLS-KDFARGST                           
055703     MOVE   UT-XLS-KDFARGST  TO WS-TEXT                                   
055704     PERFORM S99-INSPECT-REVERSE                                          
055705     MOVE   WS-TEXT          TO UT-XLS-KDFARGST                           
055706                                                                          
055707     MOVE   ACCE-IDPSLAG     TO UT-XLS-IDPSLAG                            
055708     MOVE   UT-XLS-IDPSLAG   TO WS-TEXT                                   
055709     PERFORM S99-INSPECT-REVERSE                                          
055710     MOVE   WS-TEXT          TO UT-XLS-IDPSLAG                            
055711                                                                          
055712     MOVE   ACCE-BETYP       TO UT-XLS-BETYP                              
055713     MOVE   UT-XLS-BETYP     TO WS-TEXT                                   
055714     PERFORM S99-INSPECT-REVERSE                                          
055715     MOVE   WS-TEXT          TO UT-XLS-BETYP                              
055716                                                                          
055717     MOVE ACCE-IDFKNGRP      TO WS-IDFKNGRP-DIS                           
055718     IF WS-IDFKNGRP-DIS = ZERO                                            
055719       MOVE ZERO             TO UT-XLS-IDFKNGRP                           
055720     ELSE                                                                 
055730       MOVE WS-IDFKNGRP-DIS  TO UT-XLS-IDFKNGRP                           
055731     END-IF                                                               
055732     MOVE   UT-XLS-IDFKNGRP  TO WS-TEXT                                   
055733     PERFORM S99-INSPECT-REVERSE                                          
055734     MOVE   WS-TEXT          TO UT-XLS-IDFKNGRP                           
055735                                                                          
055736     MOVE ACCE-IDKDPPOS      TO WS-IDKDPPOS-DIS                           
055737     IF WS-IDKDPPOS-DIS = ZERO                                            
055738       MOVE ZERO             TO UT-XLS-IDKDPPOS                           
055739     ELSE                                                                 
055740       MOVE WS-IDKDPPOS-DIS  TO UT-XLS-IDKDPPOS                           
055741     END-IF                                                               
055742     MOVE   UT-XLS-IDKDPPOS  TO WS-TEXT                                   
055743     PERFORM S99-INSPECT-REVERSE                                          
055744     MOVE   WS-TEXT          TO UT-XLS-IDKDPPOS                           
055745                                                                          
055746     MOVE ACCE-IDAOTUTG      TO WS-IDAOTUTG-DIS                           
055747     IF WS-IDAOTUTG-DIS = ZERO                                            
055748       MOVE ZERO             TO UT-XLS-IDAOTUTG                           
055749     ELSE                                                                 
055750       MOVE WS-IDAOTUTG-DIS  TO UT-XLS-IDAOTUTG                           
055751     END-IF                                                               
055752     MOVE   UT-XLS-IDAOTUTG  TO WS-TEXT                                   
055753     PERFORM S99-INSPECT-REVERSE                                          
055754     MOVE   WS-TEXT          TO UT-XLS-IDAOTUTG                           
055755                                                                          
055756     MOVE   ACCE-BEANST-KU   TO UT-XLS-BEANST-KU                          
055757     MOVE   UT-XLS-BEANST-KU TO WS-TEXT                                   
055758     PERFORM S99-INSPECT-REVERSE                                          
055759     MOVE   WS-TEXT          TO UT-XLS-BEANST-KU                          
055760                                                                          
055761     MOVE   ACCE-BEANST-SU   TO UT-XLS-BEANST-SU                          
055762     MOVE   UT-XLS-BEANST-SU TO WS-TEXT                                   
055763     PERFORM S99-INSPECT-REVERSE                                          
055764     MOVE   WS-TEXT          TO UT-XLS-BEANST-SU                          
055765                                                                          
055766     MOVE   ACCE-BEUPPDSU    TO UT-XLS-BEUPPDSU                           
055767     MOVE   UT-XLS-BEUPPDSU  TO WS-TEXT                                   
055768     PERFORM S99-INSPECT-REVERSE                                          
055769     MOVE   WS-TEXT          TO UT-XLS-BEUPPDSU                           
055770                                                                          
055771     MOVE   ACCE-IDPROJK     TO UT-XLS-IDPROJK                            
055772     MOVE   UT-XLS-IDPROJK   TO WS-TEXT                                   
055773     PERFORM S99-INSPECT-REVERSE                                          
055774     MOVE   WS-TEXT          TO UT-XLS-IDPROJK                            
055775                                                                          
055776     MOVE   ACCE-IDPSS       TO UT-XLS-IDPSS                              
055777     MOVE   UT-XLS-IDPSS     TO WS-TEXT                                   
055778     PERFORM S99-INSPECT-REVERSE                                          
055779     MOVE   WS-TEXT          TO UT-XLS-IDPSS                              
055780                                                                          
055781     MOVE ACCE-VKART-KDP     TO WS-VKART-KDP-DIS                          
055782     IF WS-VKART-KDP-DIS = ZERO                                           
055783       MOVE ZERO             TO UT-XLS-VKART-KDP                          
055784     ELSE                                                                 
055785       MOVE WS-VKART-KDP-DIS TO UT-XLS-VKART-KDP                          
055786     END-IF                                                               
055787     MOVE   UT-XLS-VKART-KDP TO WS-TEXT                                   
055788     PERFORM S99-INSPECT-REVERSE                                          
055789     MOVE   WS-TEXT          TO UT-XLS-VKART-KDP                          
055790     .                                                                    
055791     EJECT                                                                
055800 FCB0-MOVE-CURRENT-WDK601-DATA  SECTION.                                  
055810     MOVE 'FCB0-MOVE-CURRENT-WDK601-DATA ' TO PERFORM-SEKT                
055900     SKIP2                                                                
056000     MOVE YES                  TO UT-XLS-FLRPULS                          
056100     MOVE WDK6-ART-IDLEVNR     TO UT-XLS-IDLEVNR                          
056200                                       W-IDLEVNR                          
056300     .                                                                    
056400     EJECT                                                                
056500 FCB1-MOVE-CURRENT-WDK611-DATA  SECTION.                                  
056510     MOVE 'FCB1-MOVE-CURRENT-WDK611-DATA ' TO PERFORM-SEKT                
056600     SKIP2                                                                
057111     MOVE WDK6-CLAG-IDINK TO UT-XLS-IDINK                                 
057200     MOVE WDK6-CLAG-IDANSK     TO UT-XLS-IDANSK                           
057300                                  WS-NUM-IDANSK                           
057400     MOVE WDK6-CLAG-KDEMBKOD-2 TO UT-XLS-KDEMBKOD-2                       
057500     MOVE WDK6-CLAG-KVLS       TO UT-XLS-KVLS                             
057700     .                                                                    
057800     EJECT                                                                
057801                                                                          
057802 FCB2-MOVE-CURRENT-WDK623-DATA  SECTION.                                  
057803     SKIP2                                                                
057810     IF WDK6-AVT-TIAVTAL > ZERO                                           
057820         MOVE 'AAMMDD'           TO DAT-KDDATFORM                         
057830         MOVE WDK6-AVT-TIAVTAL   TO DAT-I-TIDATUM                         
057840         CALL WDATKONV        USING DAT-KDDATFORM DAT-I-TIDATUM           
057850                                    DAT-O-TIDATUM DAT-KDSVAR              
057860         IF DAT-KDSVAR-OK                                                 
057870           MOVE DAT-TIAAVVD(1:4) TO UT-XLS-TIAVTAL                        
057880         ELSE                                                             
057890           MOVE 9999             TO UT-XLS-TIAVTAL                        
057891         END-IF                                                           
057892     ELSE                                                                 
057893       MOVE ZERO                 TO UT-XLS-TIAVTAL                        
057894     END-IF                                                               
057895     .                                                                    
057896     EJECT                                                                
057900                                                                          
058000 FCC-CLEAR-UT-WDK6-DATA       SECTION.                                    
058010     MOVE 'FCC-CLEAR-UT-WDK6-DATA ' TO PERFORM-SEKT                       
058100     SKIP2                                                                
058200*    --- CLEAR WDK613                                                     
058300     MOVE SPACE  TO UT-XLS-KDFRPTYP                                       
058400                                                                          
058410*    --- CLEAR WDK623                                                     
058420     MOVE ZERO   TO UT-XLS-TIAVTAL                                        
058430                                                                          
058500*    --- CLEAR WDK611                                                     
058700     MOVE SPACE  TO UT-XLS-IDINK                                          
058800     MOVE ZERO   TO UT-XLS-IDANSK                                         
058810     MOVE ZERO   TO WS-NUM-IDANSK                                         
058900     MOVE ZERO   TO UT-XLS-KDEMBKOD-2                                     
059100                                                                          
059200*    --- AND WDK601                                                       
059300     IF ACCE-IDLEVNR-GSDB = SPACE                                         
059400       MOVE SPACE TO UT-XLS-IDLEVNR                                       
059500     END-IF                                                               
059600     MOVE NEJ     TO UT-XLS-FLRPULS                                       
059700     .                                                                    
059800     EJECT                                                                
059900 FCD-MOVE-CURRENT-WDD2-DATA  SECTION.                                     
059910      MOVE 'FCD-MOVE-CURRENT-WDD2-DATA  ' TO PERFORM-SEKT                 
060000     SKIP2                                                                
060100*    --- DON´T REPLACE WDK6 CORRESPONDING AVAILABLE FIELD DATA            
060200     IF UT-XLS-IDLEVNR      = SPACE                                       
060300       MOVE WDD2-ART-IDLEVNR  TO UT-XLS-IDLEVNR                           
060400     END-IF                                                               
060500     IF WS-NUM-IDANSK = ZERO                                              
060600       MOVE WDD2-ART-IDANSK   TO UT-XLS-IDANSK                            
060700     END-IF                                                               
060800     IF UT-XLS-IDINK   = SPACE                                            
060900       MOVE WDD2-ART-IDINK    TO UT-XLS-IDINK                             
061000     END-IF                                                               
061400                                                                          
061500*    --- MOVE ALL THE REMAINING FIELDS                                    
061600     IF UT-XLS-IDLEVNR   = '1002 '                                        
061700       MOVE  '1002 '  TO UT-XLS-BETEXT-OTP                                
061800     ELSE                                                                 
061900       IF WDD2-ART-KVLEVBEG > +0                                          
062100         MOVE YES     TO UT-XLS-BETEXT-OTP                                
062700       ELSE                                                               
062800         MOVE NEJ     TO UT-XLS-BETEXT-OTP                                
062900       END-IF                                                             
063000     END-IF                                                               
063001                                                                          
063400     MOVE   WDD2-ART-IDSTEKN  TO UT-XLS-IDSTEKN                           
064300********>> FLUPB "ISD" ERSÄTTS AV                                         
064400* TPD STATUS                                                              
064500     IF WDD2-ART-KDTPD = 'D' OR 'S' OR 'P' OR 'R' OR 'A'                  
064600       MOVE WDD2-ART-KDTPD       TO UT-XLS-KDTPD                          
064700     ELSE                                                                 
064800       MOVE SPACE                TO UT-XLS-KDTPD                          
064900     END-IF                                                               
070500*                                                                         
070600* TPD WEEK                                                                
070700     IF WDD2-ART-TITPD > ZERO                                             
070800         MOVE 'AAMMDD'            TO DAT-KDDATFORM                        
070900         MOVE WDD2-ART-TITPD      TO DAT-I-TIDATUM                        
071000         CALL WDATKONV      USING DAT-KDDATFORM DAT-I-TIDATUM             
071100                                  DAT-O-TIDATUM DAT-KDSVAR                
071200         IF DAT-KDSVAR-OK                                                 
071300           MOVE DAT-TIAAVVD(1:4)  TO UT-XLS-TITPD-AAVV                    
071400         ELSE                                                             
071500           MOVE 9999              TO UT-XLS-TITPD-AAVV                    
071600         END-IF                                                           
071700     ELSE                                                                 
071800       MOVE ZERO                  TO UT-XLS-TITPD-AAVV                    
071900     END-IF                                                               
072000     .                                                                    
072100     EJECT                                                                
072200                                                                          
072300 FCE-CLEAR-UT-WDD2-DATA      SECTION.                                     
072310     MOVE 'FCE-CLEAR-UT-WDD2-DATA  ' TO PERFORM-SEKT                      
072400     SKIP2                                                                
072500*    -- DONT CLEAR FIELDS CORRESPONDING WITH WDK6                         
072510                                                                          
072600     MOVE SPACE    TO UT-XLS-BETEXT-OTP                                   
072696                                                                          
072711     MOVE SPACE    TO UT-XLS-IDSTEKN                                      
072800     MOVE ZERO     TO UT-XLS-TITPD-AAVV                                   
072900     MOVE SPACE    TO UT-XLS-KDTPD                                        
074100     .                                                                    
074200     EJECT                                                                
074300                                                                          
074400 FCF-MOVE-WDD9-DATA-TO-XLS   SECTION.                                     
074410     MOVE 'FCF-MOVE-WDD9-DATA-TO-XLS  ' TO PERFORM-SEKT                   
074500     SKIP2                                                                
074600     MOVE +1 TO W-KDAVROP                                                 
074610                WS-KDAVROP-DUMP                                           
074700     PERFORM DLI-GNPF-WDD905                                              
074800     IF SEGMENT-FINNS                                                     
074900       MOVE PROPOSED    TO UT-XLS-KDLEVPST                                
075000     ELSE                                                                 
075100       MOVE +2 TO W-KDAVROP                                               
075110                  WS-KDAVROP-DUMP                                         
075200       PERFORM DLI-GNPF-WDD905                                            
075300       IF SEGMENT-FINNS                                                   
075400         MOVE CONFIRMED TO UT-XLS-KDLEVPST                                
075500       ELSE                                                               
075600         MOVE NEJ       TO UT-XLS-KDLEVPST                                
075700       END-IF                                                             
075800     END-IF                                                               
075900     .                                                                    
076000     EJECT                                                                
076100                                                                          
076200 FCG-CLEAR-UT-WDD9-DATA      SECTION.                                     
076210     MOVE 'FCG-CLEAR-UT-WDD9-DATA   '  TO PERFORM-SEKT                    
076300     SKIP2                                                                
076400     MOVE SPACE    TO UT-XLS-KDLEVPST                                     
076410     MOVE ZERO     TO UT-XLS-TILEVBSK                                     
076500                                                                          
076600     .                                                                    
076700     EJECT                                                                
076800                                                                          
076810 FCH-MOVE-WDD924-DATA-TO-RESP  SECTION.                                   
076820     MOVE DAGENS-DATUM TO W-DALEVBSK-AVS                                  
076830     PERFORM DLI-GNP-WDD924                                               
076840                                                                          
076850     IF SEGMENT-FINNS                                                     
076860       MOVE WDD924-LEV-TILEVBSK-INL TO DAT-I-TIDATUM                      
076870                                       WS-TILEVBSK-DUMP                   
076880       MOVE 'AAMMDD'                TO DAT-KDDATFORM                      
076890       CALL WDATKONV USING DAT-KDDATFORM DAT-I-TIDATUM                    
076891                           DAT-O-TIDATUM DAT-KDSVAR                       
076892       IF DAT-KDSVAR-OK                                                   
076893         MOVE DAT-TISEKEL           TO WS-TILEVBSK-SEK                    
076894         MOVE DAT-TIAAVVD           TO WS-TILEVBSK-AAVVD                  
076895       ELSE                                                               
076896         CALL ABEND USING RKOD-ABEND-MED-DUMP                             
076897       END-IF                                                             
076899                                                                          
076900       MOVE WS-TILEVBSK             TO UT-XLS-TILEVBSK                    
076902     END-IF                                                               
076903     .                                                                    
076904     EJECT                                                                
076905                                                                          
076906 FCI-MOVE-WDD905-DATA-TO-RESP  SECTION.                                   
076907     MOVE DAGENS-DATUM           TO DAT-I-TIDATUM                         
076908     MOVE 'AAMMDD'               TO DAT-KDDATFORM                         
076909     CALL WDATKONV USING DAT-KDDATFORM DAT-I-TIDATUM                      
076910                         DAT-O-TIDATUM DAT-KDSVAR                         
076911     IF DAT-KDSVAR-OK                                                     
076912       MOVE DAT-TISEKEL          TO WS-DAAVROP-SEK                        
076913       MOVE DAT-TIAAVV-GRP       TO WS-DAAVROP-AAVV                       
076914     ELSE                                                                 
076915       CALL ABEND USING RKOD-ABEND-MED-DUMP                               
076916     END-IF                                                               
076917                                                                          
076918     MOVE WS-DAAVROP             TO W-DAAVROP                             
076919     MOVE +2 TO W-KDAVROP                                                 
076920                WS-KDAVROP-DUMP                                           
076921     PERFORM DLI-GNP-WDD905                                               
076922                                                                          
076923     IF SEGMENT-FINNS                                                     
076924       MOVE WDD905-TIAVRDAT-INL  TO DAT-I-TIDATUM                         
076925                                    WS-TIAVRDAT-DUMP                      
076926       MOVE 'AAMMDD'             TO DAT-KDDATFORM                         
076927       CALL WDATKONV USING DAT-KDDATFORM DAT-I-TIDATUM                    
076928                           DAT-O-TIDATUM DAT-KDSVAR                       
076929       IF DAT-KDSVAR-OK                                                   
076930         MOVE DAT-TISEKEL        TO WS-TIAVRDAT-SEK                       
076931         MOVE DAT-TIAAVVD        TO WS-TIAVRDAT-AAVVD                     
076932       ELSE                                                               
076933         CALL ABEND USING RKOD-ABEND-MED-DUMP                             
076934       END-IF                                                             
076935                                                                          
076936       MOVE WS-TIAVRDAT          TO UT-XLS-TILEVBSK                       
076938     END-IF                                                               
076939     .                                                                    
076940     EJECT                                                                
076941                                                                          
076942 FCJ-MOVE-CURRENT-WDC1-DATA  SECTION.                                     
076943     IF WDC1-ART-PRARTBTO-MARK > 0                                        
076944       MOVE 'Y'                  TO UT-XLS-FLPULSPR                       
076945     ELSE                                                                 
076946       MOVE 'N'                  TO UT-XLS-FLPULSPR                       
076947     END-IF                                                               
076948     .                                                                    
076949     EJECT                                                                
076950                                                                          
076951 FCK-CLEAR-RESP-WDC1-DATA    SECTION.                                     
076952     MOVE 'N'                    TO UT-XLS-FLPULSPR                       
076953     .                                                                    
076954     EJECT                                                                
076955                                                                          
076956 FCL-MOVE-CURRENT-WDL2-DATA  SECTION.                                     
076957     MOVE 'J'                         TO SW-FIRST-TIME                    
076958     MOVE ZERO                        TO WS-SULEVANT-ACK                  
076959                                                                          
076960     PERFORM DLI-GU-WDL201                                                
076961     IF SEGMENT-FINNS                                                     
076962       PERFORM DLI-GNP-WDL221                                             
076970       PERFORM UNTIL SEGMENT-SAKNAS                                       
076980         IF WDL221-MOT-KDRT     = 0 AND                                   
076990            WDL221-MOT-IDDC     = WC-CDC-SE AND                           
076991            WDL221-MOT-KVANTMOT > 0                                       
077000           IF FIRST-TIME                                                  
077010             MOVE 'N'                 TO SW-FIRST-TIME                    
077011             MOVE WDL221-MOT-TIAVIDAT TO DAT-I-TIDATUM                    
077012                                         WS-TIAVIDAT-DUMP                 
077013             MOVE 'AAMMDD'            TO DAT-KDDATFORM                    
077014             CALL WDATKONV USING DAT-KDDATFORM DAT-I-TIDATUM              
077015                                 DAT-O-TIDATUM DAT-KDSVAR                 
077016             IF DAT-KDSVAR-OK                                             
077017               MOVE DAT-TISEKEL       TO WS-TIAVIDAT-SEK                  
077018               MOVE DAT-TIAAVVD       TO WS-TIAVIDAT-AAVVD                
077019             ELSE                                                         
077020               CALL ABEND USING RKOD-ABEND-MED-DUMP                       
077021             END-IF                                                       
077022                                                                          
077023             MOVE WS-TIAVIDAT-AAVVD   TO UT-XLS-TIAVIDAT                  
077024           END-IF                                                         
077025           COMPUTE WS-SULEVANT-ACK = WS-SULEVANT-ACK +                    
077026                                     WDL221-MOT-KVANTMOT                  
077027           END-COMPUTE                                                    
077028         END-IF                                                           
077029                                                                          
077030         IF WS-SULEVANT-ACK > 99                                          
077031           MOVE 'GE'               TO STATUS-WS                           
077032         ELSE                                                             
077033           PERFORM DLI-GNP-WDL221                                         
077034         END-IF                                                           
077035       END-PERFORM                                                        
077036       MOVE WS-SULEVANT-ACK        TO WS-SULEVANT-DUMP                    
077038     END-IF                                                               
077039                                                                          
077040     IF WS-SULEVANT-ACK > 99                                              
077041       MOVE '>'                    TO WS-SULEVANT-OP                      
077042       MOVE 99                     TO WS-SULEVANT-NO                      
077043     ELSE                                                                 
077044       IF WS-SULEVANT-ACK > 0                                             
077045         MOVE ' '                  TO WS-SULEVANT-OP                      
077046         MOVE WS-SULEVANT-ACK      TO WS-SULEVANT-NO                      
077047       ELSE                                                               
077048         MOVE ' '                  TO WS-SULEVANT-OP                      
077049         MOVE ZERO                 TO WS-SULEVANT-NO                      
077050       END-IF                                                             
077051     END-IF                                                               
077052                                                                          
077053     MOVE WS-SULEVANT-OP           TO UT-XLS-KDSIGN                       
077054     MOVE WS-SULEVANT-NO           TO UT-XLS-SULEVANT                     
077055     .                                                                    
077056     EJECT                                                                
077057                                                                          
077058 FCN-CLEAR-RESP-WDL2-DATA    SECTION.                                     
077059     MOVE ZERO                   TO UT-XLS-TIAVIDAT                       
077060     MOVE '000'                  TO UT-XLS-SULEVANT                       
077061     MOVE ' '                    TO UT-XLS-KDSIGN                         
077062     .                                                                    
077063     EJECT                                                                
077064                                                                          
077065 Z-FINIT SECTION.                                                         
077066      MOVE ' Z-FINIT  ' TO PERFORM-SEKT                                   
077070                                                                          
077100     CLOSE WB1103                                                         
077200     PERFORM DB2-CLOSE-TB1ACCE-CRS                                        
077300     SKIP2                                                                
077400     MOVE 'S' TO POSTSUM-OPKOD                                            
077500     CALL POSTSUM USING POSTSUM-PARM                                      
077600     .                                                                    
077700     EJECT                                                                
077800 S11-SKRIV-OUTPUT-WB1103 SECTION.                                         
077810     MOVE  'S11-SKRIV-OUTPUT-WB1103 ' TO PERFORM-SEKT                     
077900     SKIP2                                                                
078000     PERFORM S11A-MODIFY-FIELDS                                           
078010     WRITE UT-XLS-POST FROM UT-XLS-AREA                                   
078100                                                                          
078200     MOVE 'XLS' TO POSTSUM-TRANSTYP                                       
078300     MOVE 'WB1103 ' TO POSTSUM-FDNAMN                                     
078400     MOVE 'WB1103D1' TO POSTSUM-DDNAMN2                                   
078500     CALL POSTSUM USING POSTSUM-PARM                                      
078600     .                                                                    
078800*                                                                         
078810 S11A-MODIFY-FIELDS      SECTION.                                         
078820     MOVE  'S11A-MODIFY-FIELDS      ' TO PERFORM-SEKT                     
078830     SKIP2                                                                
078831     MOVE   UT-XLS-TIAVIDAT   TO WS-TEXT                                  
078832     PERFORM S99-INSPECT-REVERSE                                          
078833     MOVE   WS-TEXT           TO UT-XLS-TIAVIDAT                          
078834                                                                          
078835     MOVE   UT-XLS-DAPSWCA-3  TO WS-TEXT                                  
078836     PERFORM S99-INSPECT-REVERSE                                          
078837     MOVE   WS-TEXT           TO UT-XLS-DAPSWCA-3                         
078838                                                                          
078839     MOVE   UT-XLS-DAPSWCP-3  TO WS-TEXT                                  
078840     PERFORM S99-INSPECT-REVERSE                                          
078850     MOVE   WS-TEXT           TO UT-XLS-DAPSWCP-3                         
078851                                                                          
078852     MOVE   UT-XLS-DAPSWPP-2  TO WS-TEXT                                  
078853     PERFORM S99-INSPECT-REVERSE                                          
078854     MOVE   WS-TEXT           TO UT-XLS-DAPSWPP-2                         
078855                                                                          
078856     MOVE   UT-XLS-KDTPD      TO WS-TEXT                                  
078857     PERFORM S99-INSPECT-REVERSE                                          
078858     MOVE   WS-TEXT           TO UT-XLS-KDTPD                             
078859                                                                          
078860     MOVE   UT-XLS-TITPD-AAVV TO WS-TEXT                                  
078861     PERFORM S99-INSPECT-REVERSE                                          
078862     MOVE   WS-TEXT           TO UT-XLS-TITPD-AAVV                        
078863                                                                          
078864     MOVE   UT-XLS-TILEVBSK   TO WS-TEXT                                  
078865     PERFORM S99-INSPECT-REVERSE                                          
078866     MOVE   WS-TEXT           TO UT-XLS-TILEVBSK                          
078867                                                                          
078868     MOVE   UT-XLS-KDLEVPST   TO WS-TEXT                                  
078869     PERFORM S99-INSPECT-REVERSE                                          
078870     MOVE   WS-TEXT           TO UT-XLS-KDLEVPST                          
078871                                                                          
078872     MOVE   UT-XLS-TIAVTAL    TO WS-TEXT                                  
078873     PERFORM S99-INSPECT-REVERSE                                          
078874     MOVE   WS-TEXT           TO UT-XLS-TIAVTAL                           
078875                                                                          
078876     MOVE   UT-XLS-BETEXT-OTP TO WS-TEXT                                  
078877     PERFORM S99-INSPECT-REVERSE                                          
078878     MOVE   WS-TEXT           TO UT-XLS-BETEXT-OTP                        
078879                                                                          
078880     MOVE   UT-XLS-KDFRPTYP   TO WS-TEXT                                  
078881     PERFORM S99-INSPECT-REVERSE                                          
078882     MOVE   WS-TEXT           TO UT-XLS-KDFRPTYP                          
078883                                                                          
078884     MOVE   UT-XLS-IDANSK     TO WS-TEXT                                  
078885     PERFORM S99-INSPECT-REVERSE                                          
078886     MOVE   WS-TEXT           TO UT-XLS-IDANSK                            
078887                                                                          
078888     MOVE   UT-XLS-IDINK      TO WS-TEXT                                  
078889     PERFORM S99-INSPECT-REVERSE                                          
078890     MOVE   WS-TEXT           TO UT-XLS-IDINK                             
078891                                                                          
078892     MOVE   UT-XLS-IDSTEKN    TO WS-TEXT                                  
078893     PERFORM S99-INSPECT-REVERSE                                          
078896     MOVE   WS-TEXT           TO UT-XLS-IDSTEKN                           
078897                                                                          
078899     MOVE   UT-XLS-IDLEVNR    TO WS-TEXT                                  
078900     PERFORM S99-INSPECT-REVERSE                                          
078907     MOVE   WS-TEXT           TO UT-XLS-IDLEVNR                           
078912     .                                                                    
078913     EJECT                                                                
078914 S99-INSPECT-REVERSE     SECTION.                                         
078915     MOVE  'S99-INSPECT-REVERSE     ' TO PERFORM-SEKT                     
078916     SKIP2                                                                
078917     MOVE   ZERO             TO ANT-SPACE                                 
078918     INSPECT FUNCTION REVERSE(WS-TEXT)                                    
078919           TALLYING ANT-SPACE FOR LEADING SPACE                           
078920     COMPUTE START-POS = 100 - ANT-SPACE + 1                              
078921                                                                          
078922     IF START-POS > 100                                                   
078923       CONTINUE                                                           
078924     ELSE                                                                 
078925       INSPECT WS-TEXT( START-POS : )                                     
078926           REPLACING ALL SPACE BY LOW-VALUE                               
078927     END-IF                                                               
078928     .                                                                    
078929     EJECT                                                                
078930*                                                                         
084500* --- DLI SEKTIONER ---                                                   
084700     SKIP2                                                                
084800 DLI-GU-WDD201 SECTION.                                                   
084810     MOVE 'DLI-GU-WDD201 ' TO PERFORM-SEKT                                
084900                                                                          
085000     STRING 'WDD201  (IDARTNR  =' W-IDARTNR-X ')'                         
085100          DELIMITED BY SIZE INTO SSA1                                     
085200     MOVE '  GE' TO GODK-STATUSKODER                                      
085300     CALL CBLTDLI USING GU WDD2-PCB DLI-IO-WDD201 SSA1                    
085400     MOVE WDD2-STATUS-CODE TO STATUS-WS                                   
085500     PERFORM DLI-STATUS-CONTROL                                           
085600     .                                                                    
085700     EJECT                                                                
085800 DLI-GU-WDD902 SECTION.                                                   
085810     MOVE  'DLI-GU-WDD902 ' TO PERFORM-SEKT                               
085900                                                                          
086000     STRING 'WDD901  (WDD901KY =' W-WDD901KY-X ')'                        
086100          DELIMITED BY SIZE INTO SSA1                                     
086200     STRING 'WDD902  (IDLEVNR  =' W-IDLEVNR-X ')'                         
086300          DELIMITED BY SIZE INTO SSA2                                     
086400     MOVE '  GE' TO GODK-STATUSKODER                                      
086500     CALL CBLTDLI USING GU  WDD9-PCB DLI-IO-WDD902 SSA1 SSA2              
086600     MOVE WDD9-STATUS-CODE TO STATUS-WS                                   
086700     PERFORM DLI-STATUS-CONTROL                                           
086800     .                                                                    
086900     EJECT                                                                
087000 DLI-GNPF-WDD905 SECTION.                                                 
087010     MOVE 'DLI-GNPF-WDD905 ' TO PERFORM-SEKT                              
087100                                                                          
087200     STRING 'WDD905  *F(KDAVROP  =' W-KDAVROP-X ')'                       
087300          DELIMITED BY SIZE INTO SSA1                                     
087400     MOVE '  GE' TO GODK-STATUSKODER                                      
087500     CALL CBLTDLI USING GNP WDD9-PCB DLI-IO-WDD905 SSA1                   
087600     MOVE WDD9-STATUS-CODE TO STATUS-WS                                   
087700     PERFORM DLI-STATUS-CONTROL                                           
087800     .                                                                    
087900     EJECT                                                                
087910 DLI-GNP-WDD905 SECTION.                                                  
087911     MOVE 'DLI-GNP-WDD905  ' TO PERFORM-SEKT                              
087920                                                                          
087930     STRING 'WDD905  (WDD905KY>=' W-WDD905KY-X     '&'                    
087940                     'KDAVROP  =' W-KDAVROP-X ')'                         
087950          DELIMITED BY SIZE INTO SSA1                                     
087960     MOVE '  GE' TO GODK-STATUSKODER                                      
087970     CALL CBLTDLI USING GNP WDD9-PCB DLI-IO-WDD905 SSA1                   
087980     MOVE WDD9-STATUS-CODE TO STATUS-WS                                   
087990     PERFORM DLI-STATUS-CONTROL                                           
087991     .                                                                    
087992     SKIP2                                                                
087993 DLI-GNP-WDD924 SECTION.                                                  
087994     MOVE 'DLI-GNP-WDD924  ' TO PERFORM-SEKT                              
087995                                                                          
087996     STRING 'WDD924  (DALEVBSK>=' W-DALEVBSK-AVS-X ')'                    
087997          DELIMITED BY SIZE INTO SSA1                                     
087998     MOVE '  GE' TO GODK-STATUSKODER                                      
087999     CALL CBLTDLI USING GNP WDD9-PCB DLI-IO-WDD924 SSA1                   
088000     MOVE WDD9-STATUS-CODE TO STATUS-WS                                   
088001     PERFORM DLI-STATUS-CONTROL                                           
088002     .                                                                    
088003     EJECT                                                                
088004 DLI-GU-WDC101 SECTION.                                                   
088005     MOVE 'DLI-GU-WDC101   ' TO PERFORM-SEKT                              
088006                                                                          
088007     STRING 'WDC101  (WDC101KY =' W-WDC101KY-X ')'                        
088008          DELIMITED BY SIZE INTO SSA1                                     
088009     MOVE '  GE' TO GODK-STATUSKODER                                      
088010     CALL CBLTDLI USING GU WDC1-PCB DLI-IO-WDC101 SSA1                    
088011     MOVE WDC1-STATUS-CODE TO STATUS-WS                                   
088012     PERFORM DLI-STATUS-CONTROL                                           
088013     .                                                                    
088014     EJECT                                                                
088015 DLI-GU-WDL201 SECTION.                                                   
088016     MOVE 'DLI-GU-WDL201   ' TO PERFORM-SEKT                              
088017                                                                          
088018     STRING 'WDL201  (IDARTNR  =' W-IDARTNR-X ')'                         
088019          DELIMITED BY SIZE INTO SSA1                                     
088020     MOVE '  GE'           TO GODK-STATUSKODER                            
088021     CALL CBLTDLI USING GU WDL2-PCB DLI-IO-WDL201 SSA1                    
088022     MOVE WDL2-STATUS-CODE TO STATUS-WS                                   
088023     PERFORM DLI-STATUS-CONTROL                                           
088024     .                                                                    
088025     SKIP2                                                                
088026 DLI-GNP-WDL221 SECTION.                                                  
088027     MOVE 'DLI-GNP-WDL221  ' TO PERFORM-SEKT                              
088028                                                                          
088029     MOVE   'WDL211' TO SSA1                                              
088030     STRING 'WDL221  (IDPTYP   =' W-IDPTYP-X ')'                          
088031          DELIMITED BY SIZE INTO SSA2                                     
088032     MOVE '  GE' TO GODK-STATUSKODER                                      
088033     CALL CBLTDLI USING GNP WDL2-PCB DLI-IO-WDL221 SSA1 SSA2              
088034     MOVE WDL2-STATUS-CODE TO STATUS-WS                                   
088035     PERFORM DLI-STATUS-CONTROL                                           
088036     .                                                                    
088037     EJECT                                                                
088038 DLI-GU-WDK601 SECTION.                                                   
088040     MOVE 'DLI-GU-WDK601   ' TO PERFORM-SEKT                              
088100                                                                          
088200     STRING 'WDK601  (IDARTNR  =' W-IDARTNR-X ')'                         
088300          DELIMITED BY SIZE INTO SSA1                                     
088400     MOVE '  GE' TO GODK-STATUSKODER                                      
088500     CALL CBLTDLI USING GU WDK6-PCB DLI-IO-WDK601 SSA1                    
088600     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
088700     PERFORM DLI-STATUS-CONTROL                                           
088800     .                                                                    
088900     EJECT                                                                
089000 DLI-GNP-WDK611 SECTION.                                                  
089010     MOVE 'DLI-GNP-WDK611   ' TO PERFORM-SEKT                             
089100                                                                          
089200     MOVE 'WDK611    '      TO SSA1                                       
089300     MOVE '  GE' TO GODK-STATUSKODER                                      
089400     CALL CBLTDLI USING GNP WDK6-PCB DLI-IO-WDK611 SSA1                   
089500     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
089600     PERFORM DLI-STATUS-CONTROL                                           
089700     .                                                                    
089800     EJECT                                                                
089900 DLI-GNP-WDK613 SECTION.                                                  
089910     MOVE 'DLI-GNP-WDK613  ' TO PERFORM-SEKT                              
090000                                                                          
090100     MOVE 'WDK613  (KDEMBAL  =Q1 )'  TO SSA1                              
090200     MOVE '  GEGB' TO GODK-STATUSKODER                                    
090300     CALL CBLTDLI USING GNP WDK6-PCB DLI-IO-WDK613 SSA1                   
090400     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
090500     PERFORM DLI-STATUS-CONTROL                                           
090600     .                                                                    
090700     EJECT                                                                
090710 DLI-GNP-WDK623 SECTION.                                                  
090720     MOVE 'DLI-GNP-WDK623  ' TO PERFORM-SEKT                              
090730                                                                          
090740     MOVE 'WDK623     '  TO SSA1                                          
090750     MOVE '  GEGB' TO GODK-STATUSKODER                                    
090760     CALL CBLTDLI USING GNP WDK6-PCB DLI-IO-WDK623 SSA1                   
090770     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
090780     PERFORM DLI-STATUS-CONTROL                                           
090790     .                                                                    
090791     EJECT                                                                
095900 DLI-STATUS-CONTROL SECTION.                                              
096000                                                                          
096100     SET STATUS-IX TO 1                                                   
096200     SEARCH GODK-STATUS                                                   
096300       AT END                                                             
096400         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
096500         DELIMITED BY SIZE INTO FELTEXT                                   
096600         CALL FELLOG                                                      
096700       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
096800         CONTINUE                                                         
096900     END-SEARCH                                                           
097000     .                                                                    
097100     EJECT                                                                
097200 DB2-DCL-OPN-TB1ACCE-CRS-1  SECTION.                                      
097300     MOVE 'DB2-DCL-OPN-TB1ACCE-CRS-1' TO DB2-SEKTION                      
097400     EXEC SQL                                                             
097500          DECLARE TB1ACCE-CRS-1 CURSOR FOR SELECT                         
097600                                                                          
097700          IDARTNR                                                         
097800         ,BEART                                                           
097900         ,IDPRODGR                                                        
098000         ,IDUPPDSU                                                        
098100         ,IDUPPDKU                                                        
098200         ,IDAOT                                                           
098300         ,TIAOINF                                                         
098400         ,BEASSTYP                                                        
098500         ,KDARTTYP                                                        
098600         ,KDMDS                                                           
098700         ,KDFRPTYP                                                        
098800         ,TEARTUTFG                                                       
098900         ,TESTATUPP                                                       
099000         ,IDLEVNR_GSDB                                                    
099100         ,KDTPD_PH1                                                       
099200         ,DATPDPH1                                                        
099300         ,DAPSWQP_1                                                       
099500         ,DAPSWQA_1                                                       
099600         ,KDPSWQA_1                                                       
099900         ,DAPSWPA_2                                                       
100000         ,KDPSWPA_2                                                       
100300         ,DAPSWCA_3                                                       
100400         ,KDPSWCA_3                                                       
100500         ,KVYVOL_B3                                                       
100600         ,KVYVOL_B2                                                       
100700         ,KVYVOL_INT                                                      
100800         ,KVYVOL_B1                                                       
100900         ,KVYVOL_ASS                                                      
101000         ,BEMAPP                                                          
101100         ,KVFOTO                                                          
101200         ,TIFOTO                                                          
101300         ,TENOTE                                                          
101400         ,KDANNULL                                                        
101410         ,TEVERKTYG                                                       
101420         ,TESTATXT                                                        
101430         ,TEMATXT                                                         
101440         ,TEINKTXT                                                        
101450         ,TEANSTXT                                                        
101460         ,TEAUXTXT                                                        
101470         ,IDARTNR_OFARG                                                   
101480         ,KDFARGST                                                        
101490         ,IDPSLAG                                                         
101491         ,BETYP                                                           
101492         ,IDFKNGRP                                                        
101493         ,IDKDPPOS                                                        
101494         ,IDAOTUTG                                                        
101495         ,BEANST_KU                                                       
101496         ,BEANST_SU                                                       
101497         ,BEUPPDSU                                                        
101498         ,IDPROJK                                                         
101499         ,IDPSS                                                           
101500         ,VKART_KDP                                                       
101510                                                                          
101600          FROM   TB1ACCE                                                  
101700          WHERE  IDUPPDSU = :W-IDUPPDSU                                   
101800          ORDER BY IDARTNR                                                
101900          FOR FETCH ONLY                                                  
102000     END-EXEC                                                             
102100     MOVE 000100  TO GODK-SQLCODEKODER                                    
102200     EXEC SQL                                                             
102300        OPEN TB1ACCE-CRS-1                                                
102400     END-EXEC                                                             
102500     MOVE SQLCODE TO SQLCODE-WS                                           
102600     PERFORM DB2-STATUS-CONTROL                                           
102700     .                                                                    
102800     EJECT                                                                
102900 DB2-DCL-OPN-TB1ACCE-CRS-2  SECTION.                                      
103000     MOVE 'DB2-DCL-OPN-TB1ACCE-CRS-2' TO DB2-SEKTION                      
103100     EXEC SQL                                                             
103200          DECLARE TB1ACCE-CRS-2 CURSOR FOR SELECT                         
103300                                                                          
103400          IDARTNR                                                         
103500         ,BEART                                                           
103600         ,IDPRODGR                                                        
103700         ,IDUPPDSU                                                        
103800         ,IDUPPDKU                                                        
103900         ,IDAOT                                                           
104000         ,TIAOINF                                                         
104100         ,BEASSTYP                                                        
104200         ,KDARTTYP                                                        
104300         ,KDMDS                                                           
104400         ,KDFRPTYP                                                        
104500         ,TEARTUTFG                                                       
104600         ,TESTATUPP                                                       
104700         ,IDLEVNR_GSDB                                                    
104800         ,KDTPD_PH1                                                       
104900         ,DATPDPH1                                                        
105200         ,DAPSWQP_1                                                       
105210         ,DAPSWQA_1                                                       
105300         ,KDPSWQA_1                                                       
105600         ,DAPSWPA_2                                                       
105700         ,KDPSWPA_2                                                       
106000         ,DAPSWCA_3                                                       
106100         ,KDPSWCA_3                                                       
106200         ,KVYVOL_B3                                                       
106300         ,KVYVOL_B2                                                       
106400         ,KVYVOL_INT                                                      
106500         ,KVYVOL_B1                                                       
106600         ,KVYVOL_ASS                                                      
106700         ,BEMAPP                                                          
106800         ,KVFOTO                                                          
106900         ,TIFOTO                                                          
107000         ,TENOTE                                                          
107100         ,KDANNULL                                                        
107110         ,TEVERKTYG                                                       
107120         ,TESTATXT                                                        
107130         ,TEMATXT                                                         
107140         ,TEINKTXT                                                        
107150         ,TEANSTXT                                                        
107160         ,TEAUXTXT                                                        
107170         ,IDARTNR_OFARG                                                   
107180         ,KDFARGST                                                        
107190         ,IDPSLAG                                                         
107191         ,BETYP                                                           
107192         ,IDFKNGRP                                                        
107193         ,IDKDPPOS                                                        
107194         ,IDAOTUTG                                                        
107195         ,BEANST_KU                                                       
107196         ,BEANST_SU                                                       
107197         ,BEUPPDSU                                                        
107198         ,IDPROJK                                                         
107199         ,IDPSS                                                           
107200         ,VKART_KDP                                                       
107210                                                                          
107300          FROM   TB1ACCE                                                  
107400             WHERE IDUPPDKU = :W-IDUPPDKU                                 
107500          ORDER BY IDARTNR                                                
107600          FOR FETCH ONLY                                                  
107700     END-EXEC                                                             
107800     MOVE 000100  TO GODK-SQLCODEKODER                                    
107900     EXEC SQL                                                             
108000        OPEN TB1ACCE-CRS-2                                                
108100     END-EXEC                                                             
108200     MOVE SQLCODE TO SQLCODE-WS                                           
108300     PERFORM DB2-STATUS-CONTROL                                           
108400     .                                                                    
108500     EJECT                                                                
108600 DB2-DCL-OPN-TB1ACCE-CRS-3  SECTION.                                      
108700     MOVE 'DB2-DCL-OPN-TB1ACCE-CRS-3' TO DB2-SEKTION                      
108800     EXEC SQL                                                             
108900          DECLARE TB1ACCE-CRS-3 CURSOR FOR SELECT                         
109000                                                                          
109100          IDARTNR                                                         
109200         ,BEART                                                           
109300         ,IDPRODGR                                                        
109400         ,IDUPPDSU                                                        
109500         ,IDUPPDKU                                                        
109600         ,IDAOT                                                           
109700         ,TIAOINF                                                         
109800         ,BEASSTYP                                                        
109900         ,KDARTTYP                                                        
110000         ,KDMDS                                                           
110100         ,KDFRPTYP                                                        
110200         ,TEARTUTFG                                                       
110300         ,TESTATUPP                                                       
110400         ,IDLEVNR_GSDB                                                    
110500         ,KDTPD_PH1                                                       
110600         ,DATPDPH1                                                        
110900         ,DAPSWQP_1                                                       
110910         ,DAPSWQA_1                                                       
111000         ,KDPSWQA_1                                                       
111300         ,DAPSWPA_2                                                       
111400         ,KDPSWPA_2                                                       
111700         ,DAPSWCA_3                                                       
111800         ,KDPSWCA_3                                                       
111900         ,KVYVOL_B3                                                       
112000         ,KVYVOL_B2                                                       
112100         ,KVYVOL_INT                                                      
112200         ,KVYVOL_B1                                                       
112300         ,KVYVOL_ASS                                                      
112400         ,BEMAPP                                                          
112500         ,KVFOTO                                                          
112600         ,TIFOTO                                                          
112700         ,TENOTE                                                          
112800         ,KDANNULL                                                        
112810         ,TEVERKTYG                                                       
112820         ,TESTATXT                                                        
112830         ,TEMATXT                                                         
112840         ,TEINKTXT                                                        
112850         ,TEANSTXT                                                        
112860         ,TEAUXTXT                                                        
112870         ,IDARTNR_OFARG                                                   
112880         ,KDFARGST                                                        
112890         ,IDPSLAG                                                         
112891         ,BETYP                                                           
112892         ,IDFKNGRP                                                        
112893         ,IDKDPPOS                                                        
112894         ,IDAOTUTG                                                        
112895         ,BEANST_KU                                                       
112896         ,BEANST_SU                                                       
112897         ,BEUPPDSU                                                        
112898         ,IDPROJK                                                         
112899         ,IDPSS                                                           
112900         ,VKART_KDP                                                       
112910                                                                          
113000          FROM   TB1ACCE                                                  
113100             WHERE ( IDUPPDKU = :W-IDUPPDKU                               
113200               AND   IDUPPDSU = :W-IDUPPDSU )                             
113300          ORDER BY IDARTNR                                                
113400          FOR FETCH ONLY                                                  
113500     END-EXEC                                                             
113600     MOVE 000100  TO GODK-SQLCODEKODER                                    
113700     EXEC SQL                                                             
113800        OPEN TB1ACCE-CRS-3                                                
113900     END-EXEC                                                             
114000     MOVE SQLCODE TO SQLCODE-WS                                           
114100     PERFORM DB2-STATUS-CONTROL                                           
114200     .                                                                    
114300     EJECT                                                                
114400 DB2-DCL-OPN-TB1ACCE-CRS-4  SECTION.                                      
114500     MOVE 'DB2-DCL-OPN-TB1ACCE-CRS-4' TO DB2-SEKTION                      
114600     EXEC SQL                                                             
114700          DECLARE TB1ACCE-CRS-4 CURSOR FOR SELECT                         
114800                                                                          
114900          IDARTNR                                                         
115000         ,BEART                                                           
115100         ,IDPRODGR                                                        
115200         ,IDUPPDSU                                                        
115300         ,IDUPPDKU                                                        
115400         ,IDAOT                                                           
115500         ,TIAOINF                                                         
115600         ,BEASSTYP                                                        
115700         ,KDARTTYP                                                        
115800         ,KDMDS                                                           
115900         ,KDFRPTYP                                                        
116000         ,TEARTUTFG                                                       
116100         ,TESTATUPP                                                       
116200         ,IDLEVNR_GSDB                                                    
116300         ,KDTPD_PH1                                                       
116400         ,DATPDPH1                                                        
116700         ,DAPSWQP_1                                                       
116710         ,DAPSWQA_1                                                       
116800         ,KDPSWQA_1                                                       
117100         ,DAPSWPA_2                                                       
117200         ,KDPSWPA_2                                                       
117500         ,DAPSWCA_3                                                       
117600         ,KDPSWCA_3                                                       
117700         ,KVYVOL_B3                                                       
117800         ,KVYVOL_B2                                                       
117900         ,KVYVOL_INT                                                      
118000         ,KVYVOL_B1                                                       
118100         ,KVYVOL_ASS                                                      
118200         ,BEMAPP                                                          
118300         ,KVFOTO                                                          
118400         ,TIFOTO                                                          
118500         ,TENOTE                                                          
118600         ,KDANNULL                                                        
118610         ,TEVERKTYG                                                       
118620         ,TESTATXT                                                        
118630         ,TEMATXT                                                         
118640         ,TEINKTXT                                                        
118650         ,TEANSTXT                                                        
118660         ,TEAUXTXT                                                        
118670         ,IDARTNR_OFARG                                                   
118680         ,KDFARGST                                                        
118690         ,IDPSLAG                                                         
118691         ,BETYP                                                           
118692         ,IDFKNGRP                                                        
118693         ,IDKDPPOS                                                        
118694         ,IDAOTUTG                                                        
118695         ,BEANST_KU                                                       
118696         ,BEANST_SU                                                       
118697         ,BEUPPDSU                                                        
118698         ,IDPROJK                                                         
118699         ,IDPSS                                                           
118700         ,VKART_KDP                                                       
118710                                                                          
118800          FROM   TB1ACCE                                                  
118900          WHERE ( TIAOINF >= :W-TIAOINF-FOM                               
119000            AND   TIAOINF <= :W-TIAOINF-TOM )                             
119100          ORDER BY IDARTNR                                                
119200          FOR FETCH ONLY                                                  
119300     END-EXEC                                                             
119400     MOVE 000100  TO GODK-SQLCODEKODER                                    
119500     EXEC SQL                                                             
119600        OPEN TB1ACCE-CRS-4                                                
119700     END-EXEC                                                             
119800     MOVE SQLCODE TO SQLCODE-WS                                           
119900     PERFORM DB2-STATUS-CONTROL                                           
120000     .                                                                    
120100     EJECT                                                                
120200 DB2-DCL-OPN-TB1ACCE-CRS-5 SECTION.                                       
120300     MOVE 'DB2-DCL-OPN-TB1ACCE-CRS-5' TO DB2-SEKTION                      
120400     EXEC SQL                                                             
120500          DECLARE TB1ACCE-CRS-5 CURSOR FOR SELECT                         
120600                                                                          
120700          IDARTNR                                                         
120800         ,BEART                                                           
120900         ,IDPRODGR                                                        
121000         ,IDUPPDSU                                                        
121100         ,IDUPPDKU                                                        
121200         ,IDAOT                                                           
121300         ,TIAOINF                                                         
121400         ,BEASSTYP                                                        
121500         ,KDARTTYP                                                        
121600         ,KDMDS                                                           
121700         ,KDFRPTYP                                                        
121800         ,TEARTUTFG                                                       
121900         ,TESTATUPP                                                       
122000         ,IDLEVNR_GSDB                                                    
122100         ,KDTPD_PH1                                                       
122200         ,DATPDPH1                                                        
122500         ,DAPSWQP_1                                                       
122510         ,DAPSWQA_1                                                       
122600         ,KDPSWQA_1                                                       
122900         ,DAPSWPA_2                                                       
123000         ,KDPSWPA_2                                                       
123300         ,DAPSWCA_3                                                       
123400         ,KDPSWCA_3                                                       
123500         ,KVYVOL_B3                                                       
123600         ,KVYVOL_B2                                                       
123700         ,KVYVOL_INT                                                      
123800         ,KVYVOL_B1                                                       
123900         ,KVYVOL_ASS                                                      
124000         ,BEMAPP                                                          
124100         ,KVFOTO                                                          
124200         ,TIFOTO                                                          
124300         ,TENOTE                                                          
124400         ,KDANNULL                                                        
124410         ,TEVERKTYG                                                       
124420         ,TESTATXT                                                        
124430         ,TEMATXT                                                         
124440         ,TEINKTXT                                                        
124450         ,TEANSTXT                                                        
124460         ,TEAUXTXT                                                        
124470         ,IDARTNR_OFARG                                                   
124480         ,KDFARGST                                                        
124490         ,IDPSLAG                                                         
124491         ,BETYP                                                           
124492         ,IDFKNGRP                                                        
124493         ,IDKDPPOS                                                        
124494         ,IDAOTUTG                                                        
124495         ,BEANST_KU                                                       
124496         ,BEANST_SU                                                       
124497         ,BEUPPDSU                                                        
124498         ,IDPROJK                                                         
124499         ,IDPSS                                                           
124500         ,VKART_KDP                                                       
124510                                                                          
124600          FROM   TB1ACCE                                                  
124700             WHERE ( TIAOINF >= :W-TIAOINF-FOM                            
124800               AND   TIAOINF <= :W-TIAOINF-TOM                            
124900               AND   IDUPPDSU = :W-IDUPPDSU  )                            
125000          ORDER BY IDARTNR                                                
125100          FOR FETCH ONLY                                                  
125200     END-EXEC                                                             
125300     MOVE 000100  TO GODK-SQLCODEKODER                                    
125400     EXEC SQL                                                             
125500        OPEN TB1ACCE-CRS-5                                                
125600     END-EXEC                                                             
125700     MOVE SQLCODE TO SQLCODE-WS                                           
125800     PERFORM DB2-STATUS-CONTROL                                           
125900     .                                                                    
126000     EJECT                                                                
126100 DB2-DCL-OPN-TB1ACCE-CRS-6 SECTION.                                       
126200     MOVE 'DB2-DCL-OPN-TB1ACCE-CRS-6' TO DB2-SEKTION                      
126300     EXEC SQL                                                             
126400          DECLARE TB1ACCE-CRS-6 CURSOR FOR SELECT                         
126500                                                                          
126600          IDARTNR                                                         
126700         ,BEART                                                           
126800         ,IDPRODGR                                                        
126900         ,IDUPPDSU                                                        
127000         ,IDUPPDKU                                                        
127100         ,IDAOT                                                           
127200         ,TIAOINF                                                         
127300         ,BEASSTYP                                                        
127400         ,KDARTTYP                                                        
127500         ,KDMDS                                                           
127600         ,KDFRPTYP                                                        
127700         ,TEARTUTFG                                                       
127800         ,TESTATUPP                                                       
127900         ,IDLEVNR_GSDB                                                    
128000         ,KDTPD_PH1                                                       
128100         ,DATPDPH1                                                        
128400         ,DAPSWQP_1                                                       
128410         ,DAPSWQA_1                                                       
128500         ,KDPSWQA_1                                                       
128800         ,DAPSWPA_2                                                       
128900         ,KDPSWPA_2                                                       
129200         ,DAPSWCA_3                                                       
129300         ,KDPSWCA_3                                                       
129400         ,KVYVOL_B3                                                       
129500         ,KVYVOL_B2                                                       
129600         ,KVYVOL_INT                                                      
129700         ,KVYVOL_B1                                                       
129800         ,KVYVOL_ASS                                                      
129900         ,BEMAPP                                                          
130000         ,KVFOTO                                                          
130100         ,TIFOTO                                                          
130200         ,TENOTE                                                          
130300         ,KDANNULL                                                        
130310         ,TEVERKTYG                                                       
130320         ,TESTATXT                                                        
130330         ,TEMATXT                                                         
130340         ,TEINKTXT                                                        
130350         ,TEANSTXT                                                        
130360         ,TEAUXTXT                                                        
130370         ,IDARTNR_OFARG                                                   
130380         ,KDFARGST                                                        
130390         ,IDPSLAG                                                         
130391         ,BETYP                                                           
130392         ,IDFKNGRP                                                        
130393         ,IDKDPPOS                                                        
130394         ,IDAOTUTG                                                        
130395         ,BEANST_KU                                                       
130396         ,BEANST_SU                                                       
130397         ,BEUPPDSU                                                        
130398         ,IDPROJK                                                         
130399         ,IDPSS                                                           
130400         ,VKART_KDP                                                       
130410                                                                          
130500          FROM   TB1ACCE                                                  
130600             WHERE ( TIAOINF >= :W-TIAOINF-FOM                            
130700               AND   TIAOINF <= :W-TIAOINF-TOM                            
130800               AND   IDUPPDKU = :W-IDUPPDKU  )                            
130900          ORDER BY IDARTNR                                                
131000          FOR FETCH ONLY                                                  
131100     END-EXEC                                                             
131200     MOVE 000100  TO GODK-SQLCODEKODER                                    
131300     EXEC SQL                                                             
131400        OPEN TB1ACCE-CRS-6                                                
131500     END-EXEC                                                             
131600     MOVE SQLCODE TO SQLCODE-WS                                           
131700     PERFORM DB2-STATUS-CONTROL                                           
131800     .                                                                    
131900     EJECT                                                                
132000 DB2-DCL-OPN-TB1ACCE-CRS-7 SECTION.                                       
132100     MOVE 'DB2-DCL-OPN-TB1ACCE-CRS-7' TO DB2-SEKTION                      
132200     EXEC SQL                                                             
132300          DECLARE TB1ACCE-CRS-7 CURSOR FOR SELECT                         
132400                                                                          
132500          IDARTNR                                                         
132600         ,BEART                                                           
132700         ,IDPRODGR                                                        
132800         ,IDUPPDSU                                                        
132900         ,IDUPPDKU                                                        
133000         ,IDAOT                                                           
133100         ,TIAOINF                                                         
133200         ,BEASSTYP                                                        
133300         ,KDARTTYP                                                        
133400         ,KDMDS                                                           
133500         ,KDFRPTYP                                                        
133600         ,TEARTUTFG                                                       
133700         ,TESTATUPP                                                       
133800         ,IDLEVNR_GSDB                                                    
133900         ,KDTPD_PH1                                                       
134000         ,DATPDPH1                                                        
134300         ,DAPSWQP_1                                                       
134310         ,DAPSWQA_1                                                       
134400         ,KDPSWQA_1                                                       
134700         ,DAPSWPA_2                                                       
134800         ,KDPSWPA_2                                                       
135100         ,DAPSWCA_3                                                       
135200         ,KDPSWCA_3                                                       
135300         ,KVYVOL_B3                                                       
135400         ,KVYVOL_B2                                                       
135500         ,KVYVOL_INT                                                      
135600         ,KVYVOL_B1                                                       
135700         ,KVYVOL_ASS                                                      
135800         ,BEMAPP                                                          
135900         ,KVFOTO                                                          
136000         ,TIFOTO                                                          
136100         ,TENOTE                                                          
136200         ,KDANNULL                                                        
136210         ,TEVERKTYG                                                       
136220         ,TESTATXT                                                        
136230         ,TEMATXT                                                         
136240         ,TEINKTXT                                                        
136250         ,TEANSTXT                                                        
136260         ,TEAUXTXT                                                        
136270         ,IDARTNR_OFARG                                                   
136280         ,KDFARGST                                                        
136290         ,IDPSLAG                                                         
136291         ,BETYP                                                           
136292         ,IDFKNGRP                                                        
136293         ,IDKDPPOS                                                        
136294         ,IDAOTUTG                                                        
136295         ,BEANST_KU                                                       
136296         ,BEANST_SU                                                       
136297         ,BEUPPDSU                                                        
136298         ,IDPROJK                                                         
136299         ,IDPSS                                                           
136300         ,VKART_KDP                                                       
136310                                                                          
136400          FROM   TB1ACCE                                                  
136500             WHERE ( TIAOINF >= :W-TIAOINF-FOM                            
136600               AND   TIAOINF <= :W-TIAOINF-TOM                            
136700               AND   IDUPPDKU = :W-IDUPPDKU                               
136800               AND   IDUPPDSU = :W-IDUPPDSU  )                            
136900          ORDER BY IDARTNR                                                
137000          FOR FETCH ONLY                                                  
137100     END-EXEC                                                             
137200     MOVE 000100  TO GODK-SQLCODEKODER                                    
137300     EXEC SQL                                                             
137400        OPEN TB1ACCE-CRS-7                                                
137500     END-EXEC                                                             
137600     MOVE SQLCODE TO SQLCODE-WS                                           
137700     PERFORM DB2-STATUS-CONTROL                                           
137800     .                                                                    
137900     EJECT                                                                
138000 DB2-FETCH-TB1ACCE-CRS-1  SECTION.                                        
138100     SKIP2                                                                
138200     MOVE 'DB2-FETCH-TB1ACCE-CRS-1  ' TO DB2-SEKTION                      
138300     MOVE 000100  TO GODK-SQLCODEKODER                                    
138400     EXEC SQL                                                             
138500         FETCH TB1ACCE-CRS-1                                              
138600         INTO                                                             
138700         :ACCE-IDARTNR                                                    
138800        ,:ACCE-BEART                                                      
138900        ,:ACCE-IDPRODGR                                                   
139000        ,:ACCE-IDUPPDSU                                                   
139100        ,:ACCE-IDUPPDKU                                                   
139200        ,:ACCE-IDAOT                                                      
139300        ,:ACCE-TIAOINF                                                    
139400        ,:ACCE-BEASSTYP                                                   
139500        ,:ACCE-KDARTTYP                                                   
139600        ,:ACCE-KDMDS                                                      
139700        ,:ACCE-KDFRPTYP                                                   
139800        ,:ACCE-TEARTUTFG                                                  
139900        ,:ACCE-TESTATUPP                                                  
140000        ,:ACCE-IDLEVNR-GSDB                                               
140100        ,:ACCE-KDTPD-PH1                                                  
140200        ,:ACCE-DATPDPH1                                                   
140500        ,:ACCE-DAPSWQP-1                                                  
140510        ,:ACCE-DAPSWQA-1                                                  
140600        ,:ACCE-KDPSWQA-1                                                  
140900        ,:ACCE-DAPSWPA-2                                                  
141000        ,:ACCE-KDPSWPA-2                                                  
141300        ,:ACCE-DAPSWCA-3                                                  
141400        ,:ACCE-KDPSWCA-3                                                  
141500        ,:ACCE-KVYVOL-B3                                                  
141600        ,:ACCE-KVYVOL-B2                                                  
141700        ,:ACCE-KVYVOL-INT                                                 
141800        ,:ACCE-KVYVOL-B1                                                  
141900        ,:ACCE-KVYVOL-ASS                                                 
142000        ,:ACCE-BEMAPP                                                     
142100        ,:ACCE-KVFOTO                                                     
142200        ,:ACCE-TIFOTO                                                     
142300        ,:ACCE-TENOTE                                                     
142400        ,:ACCE-KDANNULL                                                   
142410        ,:ACCE-TEVERKTYG                                                  
142420        ,:ACCE-TESTATXT                                                   
142430        ,:ACCE-TEMATXT                                                    
142440        ,:ACCE-TEINKTXT                                                   
142450        ,:ACCE-TEANSTXT                                                   
142460        ,:ACCE-TEAUXTXT                                                   
142470        ,:ACCE-IDARTNR-OFARG                                              
142480        ,:ACCE-KDFARGST                                                   
142490        ,:ACCE-IDPSLAG                                                    
142491        ,:ACCE-BETYP                                                      
142492        ,:ACCE-IDFKNGRP                                                   
142493        ,:ACCE-IDKDPPOS                                                   
142494        ,:ACCE-IDAOTUTG                                                   
142495        ,:ACCE-BEANST-KU                                                  
142496        ,:ACCE-BEANST-SU                                                  
142497        ,:ACCE-BEUPPDSU                                                   
142498        ,:ACCE-IDPROJK                                                    
142499        ,:ACCE-IDPSS                                                      
142500        ,:ACCE-VKART-KDP                                                  
142510     END-EXEC                                                             
142600     MOVE SQLCODE TO SQLCODE-WS                                           
142700     PERFORM DB2-STATUS-CONTROL                                           
142800     .                                                                    
142900     EJECT                                                                
143000                                                                          
143100 DB2-FETCH-TB1ACCE-CRS-2  SECTION.                                        
143200     SKIP2                                                                
143300     MOVE 'DB2-FETCH-TB1ACCE-CRS-2  ' TO DB2-SEKTION                      
143400     MOVE 000100  TO GODK-SQLCODEKODER                                    
143500     EXEC SQL                                                             
143600         FETCH TB1ACCE-CRS-2                                              
143700         INTO                                                             
143800         :ACCE-IDARTNR                                                    
143900        ,:ACCE-BEART                                                      
144000        ,:ACCE-IDPRODGR                                                   
144100        ,:ACCE-IDUPPDSU                                                   
144200        ,:ACCE-IDUPPDKU                                                   
144300        ,:ACCE-IDAOT                                                      
144400        ,:ACCE-TIAOINF                                                    
144500        ,:ACCE-BEASSTYP                                                   
144600        ,:ACCE-KDARTTYP                                                   
144700        ,:ACCE-KDMDS                                                      
144800        ,:ACCE-KDFRPTYP                                                   
144900        ,:ACCE-TEARTUTFG                                                  
145000        ,:ACCE-TESTATUPP                                                  
145100        ,:ACCE-IDLEVNR-GSDB                                               
145200        ,:ACCE-KDTPD-PH1                                                  
145300        ,:ACCE-DATPDPH1                                                   
145600        ,:ACCE-DAPSWQP-1                                                  
145610        ,:ACCE-DAPSWQA-1                                                  
145700        ,:ACCE-KDPSWQA-1                                                  
146000        ,:ACCE-DAPSWPA-2                                                  
146100        ,:ACCE-KDPSWPA-2                                                  
146400        ,:ACCE-DAPSWCA-3                                                  
146500        ,:ACCE-KDPSWCA-3                                                  
146600        ,:ACCE-KVYVOL-B3                                                  
146700        ,:ACCE-KVYVOL-B2                                                  
146800        ,:ACCE-KVYVOL-INT                                                 
146900        ,:ACCE-KVYVOL-B1                                                  
147000        ,:ACCE-KVYVOL-ASS                                                 
147100        ,:ACCE-BEMAPP                                                     
147200        ,:ACCE-KVFOTO                                                     
147300        ,:ACCE-TIFOTO                                                     
147400        ,:ACCE-TENOTE                                                     
147500        ,:ACCE-KDANNULL                                                   
147510        ,:ACCE-TEVERKTYG                                                  
147520        ,:ACCE-TESTATXT                                                   
147530        ,:ACCE-TEMATXT                                                    
147540        ,:ACCE-TEINKTXT                                                   
147550        ,:ACCE-TEANSTXT                                                   
147560        ,:ACCE-TEAUXTXT                                                   
147570        ,:ACCE-IDARTNR-OFARG                                              
147580        ,:ACCE-KDFARGST                                                   
147590        ,:ACCE-IDPSLAG                                                    
147591        ,:ACCE-BETYP                                                      
147592        ,:ACCE-IDFKNGRP                                                   
147593        ,:ACCE-IDKDPPOS                                                   
147594        ,:ACCE-IDAOTUTG                                                   
147595        ,:ACCE-BEANST-KU                                                  
147596        ,:ACCE-BEANST-SU                                                  
147597        ,:ACCE-BEUPPDSU                                                   
147598        ,:ACCE-IDPROJK                                                    
147599        ,:ACCE-IDPSS                                                      
147600        ,:ACCE-VKART-KDP                                                  
147610     END-EXEC                                                             
147700     MOVE SQLCODE TO SQLCODE-WS                                           
147800     PERFORM DB2-STATUS-CONTROL                                           
147900     .                                                                    
148000     EJECT                                                                
148100                                                                          
148200 DB2-FETCH-TB1ACCE-CRS-3  SECTION.                                        
148300     SKIP2                                                                
148400     MOVE 'DB2-FETCH-TB1ACCE-CRS-3  ' TO DB2-SEKTION                      
148500     MOVE 000100  TO GODK-SQLCODEKODER                                    
148600     EXEC SQL                                                             
148700         FETCH TB1ACCE-CRS-3                                              
148800         INTO                                                             
148900         :ACCE-IDARTNR                                                    
149000        ,:ACCE-BEART                                                      
149100        ,:ACCE-IDPRODGR                                                   
149200        ,:ACCE-IDUPPDSU                                                   
149300        ,:ACCE-IDUPPDKU                                                   
149400        ,:ACCE-IDAOT                                                      
149500        ,:ACCE-TIAOINF                                                    
149600        ,:ACCE-BEASSTYP                                                   
149700        ,:ACCE-KDARTTYP                                                   
149800        ,:ACCE-KDMDS                                                      
149900        ,:ACCE-KDFRPTYP                                                   
150000        ,:ACCE-TEARTUTFG                                                  
150100        ,:ACCE-TESTATUPP                                                  
150200        ,:ACCE-IDLEVNR-GSDB                                               
150300        ,:ACCE-KDTPD-PH1                                                  
150400        ,:ACCE-DATPDPH1                                                   
150700        ,:ACCE-DAPSWQP-1                                                  
150710        ,:ACCE-DAPSWQA-1                                                  
150800        ,:ACCE-KDPSWQA-1                                                  
151100        ,:ACCE-DAPSWPA-2                                                  
151200        ,:ACCE-KDPSWPA-2                                                  
151500        ,:ACCE-DAPSWCA-3                                                  
151600        ,:ACCE-KDPSWCA-3                                                  
151700        ,:ACCE-KVYVOL-B3                                                  
151800        ,:ACCE-KVYVOL-B2                                                  
151900        ,:ACCE-KVYVOL-INT                                                 
152000        ,:ACCE-KVYVOL-B1                                                  
152100        ,:ACCE-KVYVOL-ASS                                                 
152200        ,:ACCE-BEMAPP                                                     
152300        ,:ACCE-KVFOTO                                                     
152400        ,:ACCE-TIFOTO                                                     
152500        ,:ACCE-TENOTE                                                     
152600        ,:ACCE-KDANNULL                                                   
152610        ,:ACCE-TEVERKTYG                                                  
152620        ,:ACCE-TESTATXT                                                   
152630        ,:ACCE-TEMATXT                                                    
152640        ,:ACCE-TEINKTXT                                                   
152650        ,:ACCE-TEANSTXT                                                   
152660        ,:ACCE-TEAUXTXT                                                   
152670        ,:ACCE-IDARTNR-OFARG                                              
152680        ,:ACCE-KDFARGST                                                   
152690        ,:ACCE-IDPSLAG                                                    
152691        ,:ACCE-BETYP                                                      
152692        ,:ACCE-IDFKNGRP                                                   
152693        ,:ACCE-IDKDPPOS                                                   
152694        ,:ACCE-IDAOTUTG                                                   
152695        ,:ACCE-BEANST-KU                                                  
152696        ,:ACCE-BEANST-SU                                                  
152697        ,:ACCE-BEUPPDSU                                                   
152698        ,:ACCE-IDPROJK                                                    
152699        ,:ACCE-IDPSS                                                      
152700        ,:ACCE-VKART-KDP                                                  
152710     END-EXEC                                                             
152800     MOVE SQLCODE TO SQLCODE-WS                                           
152900     PERFORM DB2-STATUS-CONTROL                                           
153000     .                                                                    
153100     EJECT                                                                
153200                                                                          
153300 DB2-FETCH-TB1ACCE-CRS-4  SECTION.                                        
153400     SKIP2                                                                
153500     MOVE 'DB2-FETCH-TB1ACCE-CRS-4  ' TO DB2-SEKTION                      
153600     MOVE 000100  TO GODK-SQLCODEKODER                                    
153700     EXEC SQL                                                             
153800         FETCH TB1ACCE-CRS-4                                              
153900         INTO                                                             
154000         :ACCE-IDARTNR                                                    
154100        ,:ACCE-BEART                                                      
154200        ,:ACCE-IDPRODGR                                                   
154300        ,:ACCE-IDUPPDSU                                                   
154400        ,:ACCE-IDUPPDKU                                                   
154500        ,:ACCE-IDAOT                                                      
154600        ,:ACCE-TIAOINF                                                    
154700        ,:ACCE-BEASSTYP                                                   
154800        ,:ACCE-KDARTTYP                                                   
154900        ,:ACCE-KDMDS                                                      
155000        ,:ACCE-KDFRPTYP                                                   
155100        ,:ACCE-TEARTUTFG                                                  
155200        ,:ACCE-TESTATUPP                                                  
155300        ,:ACCE-IDLEVNR-GSDB                                               
155400        ,:ACCE-KDTPD-PH1                                                  
155500        ,:ACCE-DATPDPH1                                                   
155800        ,:ACCE-DAPSWQP-1                                                  
155810        ,:ACCE-DAPSWQA-1                                                  
155900        ,:ACCE-KDPSWQA-1                                                  
156200        ,:ACCE-DAPSWPA-2                                                  
156300        ,:ACCE-KDPSWPA-2                                                  
156600        ,:ACCE-DAPSWCA-3                                                  
156700        ,:ACCE-KDPSWCA-3                                                  
156800        ,:ACCE-KVYVOL-B3                                                  
156900        ,:ACCE-KVYVOL-B2                                                  
157000        ,:ACCE-KVYVOL-INT                                                 
157100        ,:ACCE-KVYVOL-B1                                                  
157200        ,:ACCE-KVYVOL-ASS                                                 
157300        ,:ACCE-BEMAPP                                                     
157400        ,:ACCE-KVFOTO                                                     
157500        ,:ACCE-TIFOTO                                                     
157600        ,:ACCE-TENOTE                                                     
157700        ,:ACCE-KDANNULL                                                   
157710        ,:ACCE-TEVERKTYG                                                  
157720        ,:ACCE-TESTATXT                                                   
157730        ,:ACCE-TEMATXT                                                    
157740        ,:ACCE-TEINKTXT                                                   
157750        ,:ACCE-TEANSTXT                                                   
157760        ,:ACCE-TEAUXTXT                                                   
157770        ,:ACCE-IDARTNR-OFARG                                              
157780        ,:ACCE-KDFARGST                                                   
157790        ,:ACCE-IDPSLAG                                                    
157791        ,:ACCE-BETYP                                                      
157792        ,:ACCE-IDFKNGRP                                                   
157793        ,:ACCE-IDKDPPOS                                                   
157794        ,:ACCE-IDAOTUTG                                                   
157795        ,:ACCE-BEANST-KU                                                  
157796        ,:ACCE-BEANST-SU                                                  
157797        ,:ACCE-BEUPPDSU                                                   
157798        ,:ACCE-IDPROJK                                                    
157799        ,:ACCE-IDPSS                                                      
157800        ,:ACCE-VKART-KDP                                                  
157810     END-EXEC                                                             
157900     MOVE SQLCODE TO SQLCODE-WS                                           
158000     PERFORM DB2-STATUS-CONTROL                                           
158100     .                                                                    
158200     EJECT                                                                
158300                                                                          
158400 DB2-FETCH-TB1ACCE-CRS-5  SECTION.                                        
158500     SKIP2                                                                
158600     MOVE 'DB2-FETCH-TB1ACCE-CRS-5  ' TO DB2-SEKTION                      
158700     MOVE 000100  TO GODK-SQLCODEKODER                                    
158800     EXEC SQL                                                             
158900         FETCH TB1ACCE-CRS-5                                              
159000         INTO                                                             
159100         :ACCE-IDARTNR                                                    
159200        ,:ACCE-BEART                                                      
159300        ,:ACCE-IDPRODGR                                                   
159400        ,:ACCE-IDUPPDSU                                                   
159500        ,:ACCE-IDUPPDKU                                                   
159600        ,:ACCE-IDAOT                                                      
159700        ,:ACCE-TIAOINF                                                    
159800        ,:ACCE-BEASSTYP                                                   
159900        ,:ACCE-KDARTTYP                                                   
160000        ,:ACCE-KDMDS                                                      
160100        ,:ACCE-KDFRPTYP                                                   
160200        ,:ACCE-TEARTUTFG                                                  
160300        ,:ACCE-TESTATUPP                                                  
160400        ,:ACCE-IDLEVNR-GSDB                                               
160500        ,:ACCE-KDTPD-PH1                                                  
160600        ,:ACCE-DATPDPH1                                                   
160900        ,:ACCE-DAPSWQP-1                                                  
160910        ,:ACCE-DAPSWQA-1                                                  
161000        ,:ACCE-KDPSWQA-1                                                  
161300        ,:ACCE-DAPSWPA-2                                                  
161400        ,:ACCE-KDPSWPA-2                                                  
161700        ,:ACCE-DAPSWCA-3                                                  
161800        ,:ACCE-KDPSWCA-3                                                  
161900        ,:ACCE-KVYVOL-B3                                                  
162000        ,:ACCE-KVYVOL-B2                                                  
162100        ,:ACCE-KVYVOL-INT                                                 
162200        ,:ACCE-KVYVOL-B1                                                  
162300        ,:ACCE-KVYVOL-ASS                                                 
162400        ,:ACCE-BEMAPP                                                     
162500        ,:ACCE-KVFOTO                                                     
162600        ,:ACCE-TIFOTO                                                     
162700        ,:ACCE-TENOTE                                                     
162800        ,:ACCE-KDANNULL                                                   
162810        ,:ACCE-TEVERKTYG                                                  
162820        ,:ACCE-TESTATXT                                                   
162830        ,:ACCE-TEMATXT                                                    
162840        ,:ACCE-TEINKTXT                                                   
162850        ,:ACCE-TEANSTXT                                                   
162860        ,:ACCE-TEAUXTXT                                                   
162870        ,:ACCE-IDARTNR-OFARG                                              
162880        ,:ACCE-KDFARGST                                                   
162890        ,:ACCE-IDPSLAG                                                    
162891        ,:ACCE-BETYP                                                      
162892        ,:ACCE-IDFKNGRP                                                   
162893        ,:ACCE-IDKDPPOS                                                   
162894        ,:ACCE-IDAOTUTG                                                   
162895        ,:ACCE-BEANST-KU                                                  
162896        ,:ACCE-BEANST-SU                                                  
162897        ,:ACCE-BEUPPDSU                                                   
162898        ,:ACCE-IDPROJK                                                    
162899        ,:ACCE-IDPSS                                                      
162900        ,:ACCE-VKART-KDP                                                  
162910     END-EXEC                                                             
163000     MOVE SQLCODE TO SQLCODE-WS                                           
163100     PERFORM DB2-STATUS-CONTROL                                           
163200     .                                                                    
163300     EJECT                                                                
163400                                                                          
163500 DB2-FETCH-TB1ACCE-CRS-6  SECTION.                                        
163600     SKIP2                                                                
163700     MOVE 'DB2-FETCH-TB1ACCE-CRS-6  ' TO DB2-SEKTION                      
163800     MOVE 000100  TO GODK-SQLCODEKODER                                    
163900     EXEC SQL                                                             
164000         FETCH TB1ACCE-CRS-6                                              
164100         INTO                                                             
164200         :ACCE-IDARTNR                                                    
164300        ,:ACCE-BEART                                                      
164400        ,:ACCE-IDPRODGR                                                   
164500        ,:ACCE-IDUPPDSU                                                   
164600        ,:ACCE-IDUPPDKU                                                   
164700        ,:ACCE-IDAOT                                                      
164800        ,:ACCE-TIAOINF                                                    
164900        ,:ACCE-BEASSTYP                                                   
165000        ,:ACCE-KDARTTYP                                                   
165100        ,:ACCE-KDMDS                                                      
165200        ,:ACCE-KDFRPTYP                                                   
165300        ,:ACCE-TEARTUTFG                                                  
165400        ,:ACCE-TESTATUPP                                                  
165500        ,:ACCE-IDLEVNR-GSDB                                               
165600        ,:ACCE-KDTPD-PH1                                                  
165700        ,:ACCE-DATPDPH1                                                   
166000        ,:ACCE-DAPSWQP-1                                                  
166010        ,:ACCE-DAPSWQA-1                                                  
166100        ,:ACCE-KDPSWQA-1                                                  
166400        ,:ACCE-DAPSWPA-2                                                  
166500        ,:ACCE-KDPSWPA-2                                                  
166800        ,:ACCE-DAPSWCA-3                                                  
166900        ,:ACCE-KDPSWCA-3                                                  
167000        ,:ACCE-KVYVOL-B3                                                  
167100        ,:ACCE-KVYVOL-B2                                                  
167200        ,:ACCE-KVYVOL-INT                                                 
167300        ,:ACCE-KVYVOL-B1                                                  
167400        ,:ACCE-KVYVOL-ASS                                                 
167500        ,:ACCE-BEMAPP                                                     
167600        ,:ACCE-KVFOTO                                                     
167700        ,:ACCE-TIFOTO                                                     
167800        ,:ACCE-TENOTE                                                     
167900        ,:ACCE-KDANNULL                                                   
167910        ,:ACCE-TEVERKTYG                                                  
167920        ,:ACCE-TESTATXT                                                   
167930        ,:ACCE-TEMATXT                                                    
167940        ,:ACCE-TEINKTXT                                                   
167950        ,:ACCE-TEANSTXT                                                   
167960        ,:ACCE-TEAUXTXT                                                   
167970        ,:ACCE-IDARTNR-OFARG                                              
167980        ,:ACCE-KDFARGST                                                   
167990        ,:ACCE-IDPSLAG                                                    
167991        ,:ACCE-BETYP                                                      
167992        ,:ACCE-IDFKNGRP                                                   
167993        ,:ACCE-IDKDPPOS                                                   
167994        ,:ACCE-IDAOTUTG                                                   
167995        ,:ACCE-BEANST-KU                                                  
167996        ,:ACCE-BEANST-SU                                                  
167997        ,:ACCE-BEUPPDSU                                                   
167998        ,:ACCE-IDPROJK                                                    
167999        ,:ACCE-IDPSS                                                      
168000        ,:ACCE-VKART-KDP                                                  
168010     END-EXEC                                                             
168100     MOVE SQLCODE TO SQLCODE-WS                                           
168200     PERFORM DB2-STATUS-CONTROL                                           
168300     .                                                                    
168400     EJECT                                                                
168500                                                                          
168600 DB2-FETCH-TB1ACCE-CRS-7  SECTION.                                        
168700     SKIP2                                                                
168800     MOVE 'DB2-FETCH-TB1ACCE-CRS-7  ' TO DB2-SEKTION                      
168900     MOVE 000100  TO GODK-SQLCODEKODER                                    
169000     EXEC SQL                                                             
169100         FETCH TB1ACCE-CRS-7                                              
169200         INTO                                                             
169300         :ACCE-IDARTNR                                                    
169400        ,:ACCE-BEART                                                      
169500        ,:ACCE-IDPRODGR                                                   
169600        ,:ACCE-IDUPPDSU                                                   
169700        ,:ACCE-IDUPPDKU                                                   
169800        ,:ACCE-IDAOT                                                      
169900        ,:ACCE-TIAOINF                                                    
170000        ,:ACCE-BEASSTYP                                                   
170100        ,:ACCE-KDARTTYP                                                   
170200        ,:ACCE-KDMDS                                                      
170300        ,:ACCE-KDFRPTYP                                                   
170400        ,:ACCE-TEARTUTFG                                                  
170500        ,:ACCE-TESTATUPP                                                  
170600        ,:ACCE-IDLEVNR-GSDB                                               
170700        ,:ACCE-KDTPD-PH1                                                  
170800        ,:ACCE-DATPDPH1                                                   
171100        ,:ACCE-DAPSWQP-1                                                  
171110        ,:ACCE-DAPSWQA-1                                                  
171200        ,:ACCE-KDPSWQA-1                                                  
171500        ,:ACCE-DAPSWPA-2                                                  
171600        ,:ACCE-KDPSWPA-2                                                  
171900        ,:ACCE-DAPSWCA-3                                                  
172000        ,:ACCE-KDPSWCA-3                                                  
172100        ,:ACCE-KVYVOL-B3                                                  
172200        ,:ACCE-KVYVOL-B2                                                  
172300        ,:ACCE-KVYVOL-INT                                                 
172400        ,:ACCE-KVYVOL-B1                                                  
172500        ,:ACCE-KVYVOL-ASS                                                 
172600        ,:ACCE-BEMAPP                                                     
172700        ,:ACCE-KVFOTO                                                     
172800        ,:ACCE-TIFOTO                                                     
172900        ,:ACCE-TENOTE                                                     
173000        ,:ACCE-KDANNULL                                                   
173010        ,:ACCE-TEVERKTYG                                                  
173020        ,:ACCE-TESTATXT                                                   
173030        ,:ACCE-TEMATXT                                                    
173040        ,:ACCE-TEINKTXT                                                   
173050        ,:ACCE-TEANSTXT                                                   
173060        ,:ACCE-TEAUXTXT                                                   
173070        ,:ACCE-IDARTNR-OFARG                                              
173080        ,:ACCE-KDFARGST                                                   
173090        ,:ACCE-IDPSLAG                                                    
173091        ,:ACCE-BETYP                                                      
173092        ,:ACCE-IDFKNGRP                                                   
173093        ,:ACCE-IDKDPPOS                                                   
173094        ,:ACCE-IDAOTUTG                                                   
173095        ,:ACCE-BEANST-KU                                                  
173096        ,:ACCE-BEANST-SU                                                  
173097        ,:ACCE-BEUPPDSU                                                   
173098        ,:ACCE-IDPROJK                                                    
173099        ,:ACCE-IDPSS                                                      
173100        ,:ACCE-VKART-KDP                                                  
173110     END-EXEC                                                             
173200     MOVE SQLCODE TO SQLCODE-WS                                           
173300     PERFORM DB2-STATUS-CONTROL                                           
173400     .                                                                    
173500     EJECT                                                                
173600                                                                          
173700 DB2-CLOSE-TB1ACCE-CRS  SECTION.                                          
173800     SKIP2                                                                
173900     MOVE 'DB2-CLOSE-TB1ACCE-CRS        ' TO DB2-SEKTION                  
174000     EVALUATE TRUE                                                        
174100       WHEN CALL-TYPE-1                                                   
174200         EXEC SQL                                                         
174300             CLOSE TB1ACCE-CRS-1                                          
174400         END-EXEC                                                         
174500       WHEN CALL-TYPE-2                                                   
174600         EXEC SQL                                                         
174700             CLOSE TB1ACCE-CRS-2                                          
174800         END-EXEC                                                         
174900       WHEN CALL-TYPE-3                                                   
175000         EXEC SQL                                                         
175100             CLOSE TB1ACCE-CRS-2                                          
175200         END-EXEC                                                         
175300       WHEN CALL-TYPE-4                                                   
175400         EXEC SQL                                                         
175500             CLOSE TB1ACCE-CRS-4                                          
175600         END-EXEC                                                         
175700       WHEN CALL-TYPE-5                                                   
175800         EXEC SQL                                                         
175900             CLOSE TB1ACCE-CRS-5                                          
176000         END-EXEC                                                         
176100       WHEN CALL-TYPE-6                                                   
176200         EXEC SQL                                                         
176300             CLOSE TB1ACCE-CRS-6                                          
176400         END-EXEC                                                         
176500       WHEN CALL-TYPE-7                                                   
176600         EXEC SQL                                                         
176700             CLOSE TB1ACCE-CRS-7                                          
176800         END-EXEC                                                         
176900     END-EVALUATE                                                         
177000     .                                                                    
177100     EJECT                                                                
177200 DB2-STATUS-CONTROL   SECTION.                                            
177300                                                                          
177400     SET SQLCODE-IX TO 1                                                  
177500     SEARCH GODK-SQLCODE                                                  
177600       AT END                                                             
177700          STRING 'INVALID DB2 SQL STATUS CODE: ' SQLCODE-WS               
177800          DELIMITED BY SIZE INTO FELTEXT                                  
177900          CALL ABEND USING RKOD-ABEND-DB2                                 
178000       WHEN GODK-SQLCODE (SQLCODE-IX) = SQLCODE-WS CONTINUE               
178100     END-SEARCH                                                           
178200     .                                                                    
