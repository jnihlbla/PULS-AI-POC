000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W4122300.                                                
000300 AUTHOR.         PRIYASOPHIA GALBAO.                                      
000400 DATE-WRITTEN.   24/12/18.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700                                                                          
000800*    FUNCTION:                                                            
000900*        THIS IS A BMP PROGRAM THAT AUTOMATICALLY RELEASE WORKSHOP        
001000*        ORDERS USING A FILE GENERATED FROM W4122200 PROGRAM              
001100*        AND PERFORMS THE NEEDED IMS DB UPDATES.                          
001200*        THE FILE CONTAINS THE KEYS OF BACKORDER QUEUE(WDA5) THAT         
001300*        CAN BE RELEASED. IT RUNS EVERY HOUR AND CHECKS                   
001400*        ALL CLASS 3 ORDER WITH KDSTARAD AS 2 (RO).                       
001500*                                                                         
001600*        THE PROGRAM READS     WDA5                                       
001700*                                                                         
001800*        THE PROGRAM UPDATES   WDA5                                       
001900*                              WDK6                                       
002000*                                                                         
002100                                                                          
002200     SKIP3                                                                
002300 ENVIRONMENT DIVISION.                                                    
002400 CONFIGURATION SECTION.                                                   
002500 SPECIAL-NAMES.                                                           
002600     CLASS IDSYSAPIS IS 'LYNK' 'ECOM' 'VOUI' 'POLE' 'ACC '                
002700                        'APA' 'APB' 'APC' 'APD' 'APE' 'APF' 'APG'         
002800                        'APH' 'API' 'APJ'.                                
002900     SKIP2                                                                
003000 INPUT-OUTPUT SECTION.                                                    
003100                                                                          
003200 FILE-CONTROL.                                                            
003300*          --- RELEASE RECORDS                                            
003400     SELECT W41224                     ASSIGN TO W41223D1.                
003401*          --- OUTPUT FILE FOR W412D5 RTN                                 
003410     SELECT W4122B                     ASSIGN TO W41223D2.                
003500     EJECT                                                                
003600 DATA DIVISION.                                                           
003700     SKIP3                                                                
003800 FILE SECTION.                                                            
003900     SKIP3                                                                
004000 FD  W41224                                                               
004100     RECORDING       F                                                    
004200     BLOCK CONTAINS  0.                                                   
004300                                                                          
004400*01  POST -COPY W41224 -PRE IN-  -L.                                      
004500     EJECT                                                                
004510 FD  W4122B                                                               
004520     RECORDING V                                                          
004530     BLOCK CONTAINS 0.                                                    
004540 01  W4122B-POST                 PIC X(80).                               
004550     EJECT                                                                
004600 WORKING-STORAGE SECTION.                                                 
004700                                                                          
004800 77  IDPGM                       PIC X(8)    VALUE 'W4122300'.            
004900 77  CURR-SECTION                PIC X(16)   VALUE 'MAIN'.                
005000 77  CURR-IMS-SECTION            PIC X(16)   VALUE SPACE.                 
005100 01  CHKP-VAR.                                                            
005200     03 CHKP-MSG-IO-AREA-LENGTH  PIC S9(9)   VALUE +32 COMP SYNC.         
005300     03 CHKP-MSG-IO-AREA         PIC X(32)   VALUE SPACE.                 
005400     03 CHKP-AREA-LENGTH         PIC S9(9)   VALUE +32 COMP SYNC.         
005500     03 CHKP-AREA                PIC X(32)   VALUE SPACE.                 
005600     03 CHKP-ANT                 PIC S9(3)   VALUE +0   COMP-3.           
005700     03 CHKP-MAX                 PIC S9(3)   VALUE +500 COMP-3.           
005800 77  YES                         PIC X       VALUE 'J'.                   
005900 77  NOO                         PIC X       VALUE 'N'.                   
006000 77  RFS-IX                      PIC S9(4)   VALUE +1  COMP SYNC.         
006100 77  MAX-RFS-IX                  PIC S9(4)   VALUE +4  COMP SYNC.         
006200 77  SPAR-KVART                  PIC 9(7)    VALUE ZERO.                  
006300 77  SPARA-IDORDNR               PIC X(7)    VALUE SPACE.                 
006400 77  WS-TIRFS-ALLOC-DC           PIC 9(6)    VALUE ZERO.                  
006500 77  WS-IDDISTR                  PIC 9(4)    VALUE ZERO.                  
006600 77  WS-IDKUNDNR                 PIC 9(6)    VALUE ZERO.                  
006700 77  WS-KVART                    PIC 9(6)    VALUE ZERO.                  
006800     SKIP2                                                                
006900 01  WS-DAT                      PIC 9(6).                                
007000 01  FILLER REDEFINES WS-DAT.                                             
007100     03 WS-YEAR                  PIC 9(2).                                
007200     03 WS-MONTH                 PIC 9(2).                                
007300     03 WS-DAYS                  PIC 9(2).                                
007400                                                                          
007500 01  WS-ADBETRAD-1               PIC X(35) VALUE SPACE.                   
007600 01  WS-ADBETRAD-2               PIC X(35) VALUE SPACE.                   
007700 01  WS-BEBETRAD-1               PIC X(35) VALUE SPACE.                   
007800 01  WS-BEBETRAD-2               PIC X(35) VALUE SPACE.                   
007800 01  WS-BEGMT-RAD1               PIC X(35) VALUE SPACE.                   
007800 01  WS-BEGMT-RAD2               PIC X(35) VALUE SPACE.                   
007800 01  WS-ADGMT-GATA               PIC X(35) VALUE SPACE.                   
007800 01  WS-ADGMT-PADR               PIC X(35) VALUE SPACE.                   
007900                                                                          
007910*PRATRTNTO-LOC  CONVERSION                                                
007920 01  WS-PRARTNTO-LOC              PIC 9(7)V9(2).                          
007930 01  FILLER REDEFINES WS-PRARTNTO-LOC.                                    
007940    03  WS-PRARTNTO-LOC-HEL       PIC 9(7).                               
007950    03  WS-PRARTNTO-LOC-DEC       PIC 9(2).                               
007960                                                                          
007970 01 WS-PRARTNTO-LOC-X.                                                    
007980     03 WS-PRARTNTO-LOC-X-HEL      PIC X(7).                              
007990     03 FILLER                    PIC X(1)    VALUE '.'.                  
007991     03 WS-PRARTNTO-LOC-X-DEC      PIC X(2).                              
008000 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
008100 77  RKOD-ABEND-NO-DUMP          PIC S9(4)   COMP VALUE +16.              
008200 77  RKOD-ABEND-WITH-DUMP        PIC S9(4)   COMP VALUE +1000.            
008300                                                                          
008400 01  ERROR-TEXT.                                                          
008500     03  FILLER                  PIC X(8)    VALUE 'ERROR-TX'.            
008600     03  ERROR-TEXT-STR          PIC X(72)   VALUE SPACE.                 
008700 01  KONTROLL-SIFFRA.                                                     
008800     03  REK-IDARTNR             PIC 9(9)    VALUE 0.                     
008900     03  REK-LNGD                PIC 9(1)    VALUE 9.                     
009000     03  REK-REKSIFFR            PIC 9(1)    VALUE 0.                     
009100      EJECT                                                               
009200                                                                          
009300 77  W41224-EOF-SW               PIC X       VALUE 'N'.                   
009400     88  END-OF-W41224                       VALUE 'Y'.                   
009500     EJECT                                                                
009510 01  UT-AREA.                                                             
009520     03  FILLER              PIC X(02)    VALUE SPACE.                    
009530     03  UT-IDDISTR          PIC Z(3)9    VALUE ZERO.                     
009540     03  FILLER              PIC X(01)    VALUE ';'.                      
009550     03  UT-IDKUNDNR         PIC Z(5)9    VALUE ZERO.                     
009560     03  FILLER              PIC X(01)    VALUE ';'.                      
009570     03  UT-IDORDNR5         PIC Z(4)9    VALUE ZERO.                     
009580     03  FILLER              PIC X(01)    VALUE ';'.                      
009590     03  UT-IDORDNR-NEW      PIC X(7)     VALUE SPACE.                    
009591     03  FILLER              PIC X(01)    VALUE ';'.                      
009592     03  UT-IDARTNR          PIC Z(8)9    VALUE ZERO.                     
009593     03  FILLER              PIC X(01)    VALUE ';'.                      
009594     03  UT-RAD-KVART        PIC Z(6)9    VALUE ZERO.                     
009595     03  FILLER              PIC X(01)    VALUE ';'.                      
009596     03  UT-IDDC             PIC X(02)    VALUE SPACE.                    
009597     03  FILLER              PIC X(01)    VALUE ';'.                      
009605     EJECT                                                                
009610 01  GENERAL-SUBPROGRAMS.                                                 
009700*                                                                         
009800     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
009900     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
010000     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
010100     03  WORKDAY                 PIC X(8)    VALUE 'WORKDAY '.            
010200     03  W411ORDN                PIC X(8)    VALUE 'W411ORDN'.            
010300     03  W009KSIF                PIC X(8)    VALUE 'W009KSIF'.            
010400     03  W006KOM                 PIC X(8)    VALUE 'W006KOM '.            
010500     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
010600*01  -COPY W0005   -PRE  POSTSUM-                                         
010700     EJECT                                                                
010800 01  FILLER                      PIC X(16)   VALUE 'WORKDAY '.            
010900*    --- PARAMETRAR FOR SUBPROGRAM WORKDAY                                
011000*01 -COPY WORKAREA                                                        
011100 01  FILLER                      PIC X(16)   VALUE 'W411ORDN'.            
011200*    --- PARAMETRAR FOR SUBPROGRAM W411ORDN                               
011300*01 -COPY W411ORDN                                                        
011400 01  FILLER                      PIC X(16)   VALUE 'W009KSIF'.            
011500*    --- PARAMETRAR FOR SUBPROGRAM W009KSIF                               
011600*01 -COPY W009KSIF                                                        
011700 01  FILLER                      PIC X(16)   VALUE 'WWDCKONS'.            
011800*    --- PARAMETRAR FOR SUBPROGRAM WWDCKONS                               
011900*01 -COPY WWDCKONS                                                        
012000                                                                          
012100 01  W41224-AREA-START           PIC X(24)   VALUE                        
012200                                             'W41224-AREA-START'.         
012300*01  AREA -COPY W41224     -PRE IN-                                       
012400     EJECT                                                                
012500*    --- AREAS FOR IMS-SECTIONS                                           
012600*                                                                         
012700 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
012800                                                                          
012900     SKIP3                                                                
013000 01  KEYS-TILL-DLI.                                                       
013100     03  W-IDARTNR-X.                                                     
013200         05  W-IDARTNR           PIC S9(9)   VALUE ZERO COMP-3.           
013300     03  W-WDA5KEY-X.                                                     
013400         05  W-WDA5KEY           PIC X(24).                               
013500     03  W-IDGMT-X.                                                       
013600         05  W-IDDISTR           PIC S9(5)   VALUE ZERO COMP-3.           
013700         05  W-IDKUNDNR          PIC S9(7)   VALUE ZERO COMP-3.           
013800     03  W-IDGMTREF-X.                                                    
013900         05  W-IDDISTR-N9        PIC S9(5) COMP-3   VALUE ZERO.           
014000         05  W-IDKUNDNR-N9       PIC S9(7) COMP-3   VALUE ZERO.           
014100         05  W-IDKUNDRF-N9       PIC X(10) VALUE SPACE.                   
014200                                                                          
014300                                                                          
014400*    --- AREAS FOR SUB MODULES W006KOM                                    
014500                                                                          
014600 01  FILLER                    PIC X(16) VALUE 'MSG-KOM-WMSGKOM '.        
014700*01  -COPY WMSGKOM                                                        
014800                                                                          
014900 01  P-TO-P-SW.                                                           
015000   03  P-TO-P-KVLL           PIC S9(4)   COMP SYNC.                       
015100   03  P-TO-P-KDZ1           PIC X(1)    VALUE LOW-VALUE.                 
015200   03  P-TO-P-KDZ2           PIC X(1)    VALUE LOW-VALUE.                 
015300   03  P-TO-P-KDTRANS        PIC X(8).                                    
015400   03  P-TO-P-IDTRANS        PIC X(4).                                    
015500   03  P-TO-P-KDMFSFOR       PIC X(1).                                    
015600   03  P-TO-P-DATA           PIC X(1000).                                 
015700                                                                          
015800 01  FILLER                    PIC X(16) VALUE 'MSG-IO-AREA     '.        
015900*01  -COPY WMSGAREA                                                       
016000                                                                          
016100*    --- AREOR FÖR W006KOM SUBMODUL                                       
016200*                                                                         
016300 01  FILLER                      PIC X(16)   VALUE 'KOM-IO-AREA'.         
016400 01  KOM-IO-AREA.                                                         
016500   03  KOM-AREA                     PIC X(2500) VALUE SPACE.              
016600*03  FILLER  -COPY W4I25101 -PRE OHUV-  -RED KOM-AREA.                    
016700     EJECT                                                                
016800*03  FILLER  -COPY W4I25201 -PRE ORAD-  -RED KOM-AREA.                    
016900     EJECT                                                                
017000                                                                          
017100                                                                          
017200*    --- STATUS-KOD FRÅN IMS                                              
017300 01  STATUS-WS                   PIC XX.                                  
017400     88  SEGMENT-FOUND                       VALUE '  '.                  
017500     88  SEGMENT-FOUND-EXISTS                VALUE 'II'.                  
017600     88  SEGMENT-MISSING                     VALUE 'GE'.                  
017700     88  SEGMENT-NOMORE                      VALUE 'GB'.                  
017800     88  IMS-NOT-OK                          VALUE 'XD'.                  
017900     SKIP2                                                                
018000 01  GOOD-STATUSCODES.                                                    
018100     03  GOOD-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
018200     SKIP3                                                                
018300 01  SSA1                        PIC X(64).                               
018400 01  SSA2                        PIC X(64).                               
018500     EJECT                                                                
018600*    --- IMS FUNCTION CODES                                               
018700*01  -COPY W0003                                                          
018800     EJECT                                                                
018900*    ---  DLI INPUT-OUTPUT AREA                                           
019000                                                                          
019100*    ---  DLI INPUT-OUTPUT AREA                                           
019200 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDA501'.                      
019300 01  DLI-IO-WDA501.                                                       
019400*    03  -COPY WDA501                                                     
019500                                                                          
019600 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDB201'.                      
019700 01  DLI-IO-WDB201.                                                       
019800*    03  -COPY WDB201                                                     
019900                                                                          
020000 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDQ201'.                      
020100 01  DLI-IO-WDQ201.                                                       
020200*    03  -COPY WDQ201                                                     
020300                                                                          
020400 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK611'.                      
020500 01  DLI-IO-WDK611.                                                       
020600*    03  -COPY WDK611                                                     
020700     EJECT                                                                
020800 LINKAGE SECTION.                                                         
020900                                                                          
021000*01  -COPY W0009  -PRE MSG-                                               
021100*01  -COPY W0009  -PRE ALT-                                               
021200*01  -COPY W0009  -PRE WDP8-                                              
021300     05  FILLER                  PIC X.                                   
021400*01  -COPY W0008  -PRE WDA5-                                              
021500     05  FILLER                  PIC X.                                   
021600*01  -COPY W0008  -PRE WDB2-                                              
021700     05  FILLER                  PIC X.                                   
021800*01  -COPY W0008  -PRE WDK6-                                              
021900     05  FILLER                  PIC X.                                   
022000*01  -COPY W0008  -PRE WDQ2-                                              
022100     05  FILLER                  PIC X.                                   
022200 01  ORDN-XXKP-PCB               PIC X.                                   
022300 01  ORDN-ORQL-PCB               PIC X.                                   
022400 01  ORDN-PROC-PCB               PIC X.                                   
022500 01  ORDN-ORQI-PCB               PIC X.                                   
022600     EJECT                                                                
022700 PROCEDURE DIVISION  USING MSG-PCB                                        
022800                           ALT-PCB WDP8-PCB                               
022900                           WDA5-PCB WDB2-PCB                              
023000                           WDK6-PCB                                       
023100                           WDQ2-PCB                                       
023200                           ORDN-XXKP-PCB ORDN-ORQL-PCB                    
023300                           ORDN-PROC-PCB ORDN-ORQI-PCB.                   
023400 MAIN SECTION.                                                            
023500                                                                          
023600     SKIP2                                                                
023700     PERFORM A-INIT                                                       
023800                                                                          
023900     PERFORM S01-READ-W41224                                              
024000     PERFORM UNTIL END-OF-W41224                                          
024100       IF CHKP-ANT > CHKP-MAX                                             
024200         PERFORM IMS-CHECKPOINT                                           
024300       END-IF                                                             
024400       MOVE IN-AREA (1:24)             TO W-WDA5KEY                       
024500       PERFORM IMS-GHU-WDA501                                             
024600* CONTROL IN CASE OF RESTART AFTER AN ABEND                               
024700       IF RAD-KDSTARAD = '2'                                              
024800         PERFORM B-RELEASE-ORDER                                          
024810         PERFORM S02-WRITE-FILE                                           
024900       END-IF                                                             
025000                                                                          
025100       PERFORM S01-READ-W41224                                            
025200     END-PERFORM                                                          
025300                                                                          
025400                                                                          
025500     PERFORM Z-FINIT                                                      
025600                                                                          
025700     MOVE ZERO TO RETURN-CODE                                             
025800     GOBACK                                                               
025900     .                                                                    
026000     EJECT                                                                
026100 A-INIT SECTION.                                                          
026200     SKIP2                                                                
026300                                                                          
026400     PERFORM IMS-RESTART                                                  
026500                                                                          
026600     OPEN INPUT  W41224                                                   
026610          OUTPUT W4122B                                                   
026700                                                                          
026800     ACCEPT WS-DAT     FROM DATE                                          
026900                                                                          
027000     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
027001                                                                          
027010     MOVE SPACE            TO MSG-KOM-WMSGKOM                             
027011** INIT MSGKOM AREA WITH START VALUES                                     
027020     MOVE LENGTH OF MSG-KOM-WMSGKOM  TO MSG-KOM-KVLL                      
027030     MOVE LOW-VALUE        TO MSG-KOM-KDZ1                                
027040     MOVE LOW-VALUE        TO MSG-KOM-KDZ2                                
027050     MOVE SPACE            TO MSG-KOM-KDTRANS                             
027060     MOVE 'LDCREL'         TO MSG-KOM-IDSNDNOD                            
027070     MOVE 'W4122300'       TO MSG-KOM-IDSNDJOB                            
027080     MOVE FUNCTION CURRENT-DATE(3:6) TO MSG-KOM-TIREGDAT                  
027090*    -- TIKLOCK WILL BE INCREMENTENTED FOR EACH ORDER                     
027091*    -- THIS IS THE START VALUE                                           
027092     MOVE FUNCTION CURRENT-DATE(9:8) TO MSG-KOM-TIKLOCK                   
027093     MOVE SPACE            TO MSG-KOM-IDMFSMED                            
027094*    -- INITIALIZE TARGET TRANSACTION AREA WITH FIXED VALUES              
027095     MOVE LOW-VALUE                  TO MSG-KDZ1                          
027096     MOVE LOW-VALUE                  TO MSG-KDZ2                          
027100     .                                                                    
027200     EJECT                                                                
027300 B-RELEASE-ORDER SECTION.                                                 
027400                                                                          
027500     PERFORM BA-FETCH-NEW-ORDERNUMBER                                     
027700     PERFORM BC-CREATE-W40251-MID                                         
027800     PERFORM BD-CREATE-W40252-MID                                         
027900     PERFORM BE-UPDATE-WDA5                                               
028000                                                                          
028100     IF  RAD-IDDC = WC-CDC-SE                                             
028200         PERFORM BF-KVROS-WDK6                                            
028300     END-IF                                                               
028400     .                                                                    
028500     EJECT                                                                
028600                                                                          
028700 BA-FETCH-NEW-ORDERNUMBER   SECTION.                                      
028800                                                                          
028900     MOVE 'LDCB'                  TO ORDN-IDSYSTEM                        
029000     MOVE RAD-IDDISTR             TO ORDN-IDDISTR                         
029100     MOVE RAD-IDKUNDNR            TO ORDN-IDKUNDNR                        
029200     MOVE ZERO                    TO ORDN-IDORDNR-IN                      
029300                                                                          
029400     CALL W411ORDN USING ORDN-W411ORDN ORDN-XXKP-PCB ORDN-ORQL-PCB        
029500                                       ORDN-PROC-PCB ORDN-ORQI-PCB        
029600                                                                          
029700     MOVE ORDN-IDORDNR-UT         TO SPARA-IDORDNR                        
029800     .                                                                    
031800 BC-CREATE-W40251-MID  SECTION.                                           
031900                                                                          
032000     MOVE SPACE                   TO WS-ADBETRAD-1                        
032100     MOVE SPACE                   TO WS-ADBETRAD-2                        
032200     MOVE SPACE                   TO WS-BEBETRAD-1                        
032300     MOVE SPACE                   TO WS-BEBETRAD-2                        
032400                                                                          
032500     MOVE RAD-IDDISTR    TO W-IDDISTR                                     
032600     MOVE RAD-IDKUNDNR   TO W-IDKUNDNR                                    
032700     MOVE RAD-KVART      TO SPAR-KVART                                    
032800     PERFORM IMS-GU-WDB201                                                
032900                                                                          
033000*    correct address info from original order                             
033000*    *Read order to fetch adbet and bebet = contact info                  
033100*    for LYNK orders.                                                     
033200*    IF (RAD-IDSYSTEM = 'LYNK' OR 'ECOM' OR 'VOUI' OR 'TAD '              
033300*                              OR 'ACC ' OR 'APA'  OR 'APB'               
033400*                              OR 'APC ' OR 'APD'  OR 'APE'               
033500*                              OR 'ACF ' OR 'APG'  OR 'APH'               
033600*                              OR 'ACI ' OR 'APJ' )                       
033700       MOVE RAD-IDDISTR  TO W-IDDISTR-N9                                  
033800       MOVE RAD-IDKUNDNR TO W-IDKUNDNR-N9                                 
033900       MOVE '00'         TO W-IDKUNDRF-N9(1:2)                            
034000       MOVE RAD-IDKUNDRF TO W-IDKUNDRF-N9(3:5)                            
034100       PERFORM IMS-GU-WDQ201-CSEQ                                         
034200       IF SEGMENT-FOUND                                                   
034300          MOVE OHUV-ADBETRAD-1  TO WS-ADBETRAD-1                          
034400          MOVE OHUV-ADBETRAD-2  TO WS-ADBETRAD-2                          
034500          MOVE OHUV-BEBETRAD-1  TO WS-BEBETRAD-1                          
034600          MOVE OHUV-BEBETRAD-2  TO WS-BEBETRAD-2                          
034600          MOVE OHUV-BEGMT-RAD1  TO WS-BEGMT-RAD1                          
034600          MOVE OHUV-BEGMT-RAD2  TO WS-BEGMT-RAD2                          
034600          MOVE OHUV-ADGMT-GATA  TO WS-ADGMT-GATA                          
034600          MOVE OHUV-ADGMT-PADR  TO WS-ADGMT-PADR                          
034700       END-IF                                                             
034800*    END-IF                                                               
034900                                                                          
035000     MOVE 'W4I25101'             TO MSG-KOM-IDCPYTXT                      
035100     MOVE SPACE                  TO OHUV-MID-W4I25101                     
035200     MOVE RAD-IDSYSTEM           TO OHUV-MID-IDSYSTEM                     
035300     IF RAD-IDSYSTEM(1:4) IS IDSYSAPIS                                    
035400       MOVE 'B'                  TO OHUV-MID-IDSYSTEM(4:1)                
035500     ELSE                                                                 
035600       MOVE 'LDCB'               TO OHUV-MID-IDSYSTEM                     
035700     END-IF                                                               
035800     MOVE RAD-IDDISTR            TO WS-IDDISTR                            
035900     MOVE WS-IDDISTR             TO OHUV-MID-IDDISTR                      
036000     MOVE RAD-IDKUNDNR           TO WS-IDKUNDNR                           
036100     MOVE WS-IDKUNDNR            TO OHUV-MID-IDKUNDNR                     
036200     MOVE SPARA-IDORDNR          TO OHUV-MID-IDORDNR                      
036300     MOVE '3'                   TO OHUV-MID-KDORDKL                       
036400     MOVE IN-IDDC                TO OHUV-MID-IDDC                         
036500     MOVE RAD-IDORDNR5           TO OHUV-MID-BEKUNDRF                     
036600     MOVE NOO                    TO OHUV-MID-FLAUTPAC                     
036700     MOVE NOO                    TO OHUV-MID-FLAUTFAK                     
036800     MOVE NOO                    TO OHUV-MID-FLEMBORD                     
036900     MOVE NOO                    TO OHUV-MID-FLOVRLEV                     
037000     MOVE ZERO                   TO OHUV-MID-IDDEPT                       
037100     MOVE YES                 TO OHUV-MID-FLFORBI                         
037200     MOVE RAD-KDORDTYP-LDC       TO OHUV-MID-KDORDTYP-LDC                 
037300     PERFORM BCA-CREATE-RFSDATE                                           
037400     MOVE WS-TIRFS-ALLOC-DC      TO OHUV-MID-TIRFS                        
037500     MOVE RAD-TIREPDAT           TO OHUV-MID-TIREPDAT                     
037600     MOVE NOO                    TO OHUV-MID-FLORDTIL                     
037700     MOVE ZERO                   TO OHUV-MID-IDGROSS                      
037800*    *IF LYNK ORDER ADBET AND BEBET CONTAINS CONTACT INFO                 
037900     MOVE WS-ADBETRAD-1          TO OHUV-MID-ADBETRAD-1                   
038000     MOVE WS-ADBETRAD-2          TO OHUV-MID-ADBETRAD-2                   
038100     MOVE WS-BEBETRAD-1          TO OHUV-MID-BEBETRAD-1                   
038200     MOVE WS-BEBETRAD-2          TO OHUV-MID-BEBETRAD-2                   
038200     MOVE WS-BEGMT-RAD1          TO OHUV-MID-BEGMT-RAD1                   
038200     MOVE WS-BEGMT-RAD2          TO OHUV-MID-BEGMT-RAD2                   
038200     MOVE WS-ADGMT-GATA          TO OHUV-MID-ADGMT-GATA                   
038200     MOVE WS-ADGMT-PADR          TO OHUV-MID-ADGMT-PADR                   
038300                                                                          
038400                                                                          
038500     COMPUTE P-TO-P-KVLL = LENGTH OF OHUV-MID-W4I25101 + 17               
038600     MOVE 'W4T251X '       TO P-TO-P-KDTRANS                              
038700     MOVE '4251'           TO P-TO-P-IDTRANS                              
038800     MOVE '1'              TO P-TO-P-KDMFSFOR                             
038900                                                                          
038910     ADD +1                TO MSG-KOM-TIKLOCK                             
039000     MOVE KOM-AREA                TO P-TO-P-DATA                          
039100     CALL W006KOM USING MSG-PCB                                           
039200                        ALT-PCB                                           
039300                        WDP8-PCB                                          
039400                        MSG-KOM-WMSGKOM                                   
039500                        P-TO-P-SW                                         
039600     IF MSG-KOM-IDMFSMED NOT = SPACE                                      
039700*       FELAKTIG UPPDATERING PÅ KOMMUNIKATIONS-DB                         
039800*       FELAKTIG DATUM, TID EJ NUM FÅR EJ INTRÄFFS                        
039900        MOVE 'FELAKTIG PÅ INPUT TILL DISPATCHEN' TO ERROR-TEXT            
040000        CALL ABEND USING RKOD-ABEND-NO-DUMP                               
040100     END-IF                                                               
040200                                                                          
040300     MOVE SPACE TO KOM-AREA                                               
040400     .                                                                    
040500 BCA-CREATE-RFSDATE SECTION.                                              
040600                                                                          
040700     MOVE IN-IDDC                  TO WORK-IDDC                           
040800     MOVE +002                     TO WORK-KDCALL                         
040900     MOVE +001                     TO WORK-KVWORKD                        
041000     MOVE RAD-TIREPDAT          TO WORK-TIAAMMDD-FOM                      
041100     CALL WORKDAY                  USING WORK-KDCALL                      
041200                                         WORK-DATE-AREA                   
041300                                         WORK-KDSVAR                      
041400     IF WORK-KDSVAR-FEL                                                   
041500        MOVE 'SECT ECA-, DATE MISSING IN WORKDAY'                         
041600                                   TO ERROR-TEXT                          
041700        CALL ABEND                 USING RKOD-ABEND-NO-DUMP               
041800     ELSE                                                                 
041900       MOVE +003                   TO WORK-KDCALL                         
042000       MOVE GMT-KVDAGAR-RFS-DEF    TO WORK-KVWORKD                        
042100       PERFORM                                                            
042200       VARYING RFS-IX FROM 1 BY 1                                         
042300         UNTIL RFS-IX > MAX-RFS-IX                                        
042400         IF GMT-IDDC-RFS (RFS-IX) = WORK-IDDC                             
042500           MOVE GMT-KVDAGAR-RFS (RFS-IX)                                  
042600                                   TO WORK-KVWORKD                        
042700         END-IF                                                           
042800       END-PERFORM                                                        
042900       ADD +1  TO WORK-KVWORKD                                            
043000*      +1 FÖR ATT VARIABELN SKALL KUNNA INNEHÅLLA                         
043100*      ANTAL DAGAR FÖRE RFS.                                              
043200*      0 GER DÅ SAMMA DAG, 1 GER FÖRSTA ARBETSDAG FÖRE OSV...             
043300*      OM VI INTE ADDERAR +1 SKULLE VARIABELN SÄTTAS SÅ                   
043400*      1 GER SAMMA DAG, 2 FÖRSTA ARBETSDAG FÖRE OSV...                    
043500*                                                                         
043600                                                                          
043700       CALL WORKDAY                USING WORK-KDCALL                      
043800                                         WORK-DATE-AREA                   
043900                                         WORK-KDSVAR                      
044000       IF WORK-KDSVAR-FEL                                                 
044100          MOVE 'SECT ECA-2, DATE MISSING IN WORKDAY'                      
044200                                   TO ERROR-TEXT                          
044300          CALL ABEND               USING RKOD-ABEND-NO-DUMP               
044400       ELSE                                                               
044500         IF WORK-TIAAMMDD-FOM < WS-DAT                                    
044600           MOVE IN-IDDC            TO WORK-IDDC                           
044700           MOVE +002               TO WORK-KDCALL                         
044800           MOVE +001               TO WORK-KVWORKD                        
044900           MOVE WS-DAT             TO WORK-TIAAMMDD-FOM                   
045000           CALL WORKDAY            USING WORK-KDCALL                      
045100                                         WORK-DATE-AREA                   
045200                                         WORK-KDSVAR                      
045300           IF WORK-KDSVAR-FEL                                             
045400              MOVE 'SECT ECA-3, DATUM SAKNAS I WORKDAY'                   
045500                                   TO ERROR-TEXT                          
045600              CALL ABEND           USING RKOD-ABEND-NO-DUMP               
045700           ELSE                                                           
045800              MOVE WORK-TIAAMMDD-TOM TO WS-TIRFS-ALLOC-DC                 
045900           END-IF                                                         
046000         ELSE                                                             
046100           MOVE WORK-TIAAMMDD-FOM  TO WS-TIRFS-ALLOC-DC                   
046200         END-IF                                                           
046300       END-IF                                                             
046400     END-IF                                                               
046500     .                                                                    
046600 BD-CREATE-W40252-MID   SECTION.                                          
046700                                                                          
046900     MOVE SPACE            TO ORAD-MID-W4I25201                           
047000     MOVE RAD-IDSYSTEM     TO ORAD-MID-IDSYSTEM                           
047100     IF RAD-IDSYSTEM(1:4) IS IDSYSAPIS                                    
047200       MOVE 'B'                  TO ORAD-MID-IDSYSTEM(4:1)                
047300     ELSE                                                                 
047400       MOVE 'LDCB'               TO ORAD-MID-IDSYSTEM                     
047500     END-IF                                                               
047600     MOVE RAD-IDDISTR      TO WS-IDDISTR                                  
047700     MOVE WS-IDDISTR       TO ORAD-MID-IDDISTR                            
047800     MOVE RAD-IDKUNDNR     TO WS-IDKUNDNR                                 
047900     MOVE WS-IDKUNDNR      TO ORAD-MID-IDKUNDNR                           
048000     MOVE SPARA-IDORDNR    TO ORAD-MID-IDORDNR                            
048100     MOVE YES              TO ORAD-MID-FLSLUT                             
048110     MOVE '0000000   '     TO ORAD-MID-IDKUNDRF-RO                        
048120     MOVE RAD-IDORDNR5     TO ORAD-MID-IDKUNDRF-RO(3:5)                   
048200     MOVE RAD-IDARTNR      TO ORAD-MID-IDARTNR(1)                         
048300                              REK-IDARTNR                                 
048400     MOVE 9                TO REK-LNGD                                    
048500     MOVE 0                TO REK-REKSIFFR                                
048600     CALL W009KSIF      USING REK-IDARTNR                                 
048700                              REK-LNGD                                    
048800                              REK-REKSIFFR                                
048900     MOVE REK-REKSIFFR     TO ORAD-MID-REKSIFFR(1)                        
049000     MOVE RAD-KVART        TO WS-KVART                                    
049100     MOVE WS-KVART         TO ORAD-MID-KVBEART(1)                         
049200     MOVE RAD-IDKUNDRF-WIP TO ORAD-MID-IDKUNDRF-WIP(1)                    
049300     MOVE RAD-BERADREF     TO ORAD-MID-BERADREF(1)                        
049400                                                                          
049410     IF RAD-IDSYSTEM(1:3) = 'ECO'                                         
049420        MOVE RAD-PRARTNTO-LOC    TO WS-PRARTNTO-LOC                       
049430        MOVE WS-PRARTNTO-LOC-HEL TO WS-PRARTNTO-LOC-X-HEL                 
049440        MOVE WS-PRARTNTO-LOC-DEC TO WS-PRARTNTO-LOC-X-DEC                 
049460        MOVE WS-PRARTNTO-LOC-X   TO                                       
049470                                    ORAD-MID-PRARTNTO-LOC (1)             
049480        MOVE RAD-KDVALISO        TO ORAD-MID-KDVALISO (1)                 
049490     END-IF                                                               
049500     COMPUTE P-TO-P-KVLL = LENGTH OF ORAD-MID-W4I25201 + 17               
049600     MOVE 'W4T252X '       TO P-TO-P-KDTRANS                              
049700     MOVE '4252'           TO P-TO-P-IDTRANS                              
049800     MOVE '1'              TO P-TO-P-KDMFSFOR                             
049900                                                                          
050000     MOVE KOM-AREA                TO P-TO-P-DATA                          
050100     CALL W006KOM USING MSG-PCB                                           
050200                        ALT-PCB                                           
050300                        WDP8-PCB                                          
050400                        MSG-KOM-WMSGKOM                                   
050500                        P-TO-P-SW                                         
050600     IF MSG-KOM-IDMFSMED NOT = SPACE                                      
050700*       FELAKTIG UPPDATERING PÅ KOMMUNIKATIONS-DB                         
050800*       FELAKTIG DATUM, TID EJ NUM FÅR EJ INTRÄFFS                        
050900        MOVE 'FELAKTIG PÅ INPUT TILL DISPATCHEN' TO ERROR-TEXT            
051000        CALL ABEND USING RKOD-ABEND-NO-DUMP                               
051100     END-IF                                                               
051200                                                                          
051300     MOVE SPACE TO KOM-AREA                                               
051400     .                                                                    
051500     EJECT                                                                
051600                                                                          
051700 BE-UPDATE-WDA5    SECTION.                                               
051800                                                                          
051900     MOVE '4'                 TO RAD-KDSTARAD                             
052000     MOVE SPACE               TO RAD-IDKUNDRF-LEV                         
052100     MOVE SPARA-IDORDNR (3:5) TO RAD-IDKUNDRF-LEV (1:5)                   
052200     ACCEPT RAD-TIAVBOKN FROM DATE                                        
052300     PERFORM IMS-REPL-WDA501                                              
052400     ADD +1    TO CHKP-ANT                                                
052500     .                                                                    
052600     EJECT                                                                
052700                                                                          
052800 BF-KVROS-WDK6  SECTION.                                                  
052900                                                                          
053000     MOVE RAD-IDARTNR TO W-IDARTNR                                        
053100     PERFORM IMS-GHU-WDK611                                               
053200     COMPUTE CLAG-KVROS = CLAG-KVROS - SPAR-KVART                         
053300     PERFORM IMS-REPL-WDK611                                              
053400     ADD +1    TO CHKP-ANT                                                
053500     .                                                                    
053600     EJECT                                                                
053700                                                                          
053800 Z-FINIT SECTION.                                                         
053900                                                                          
054000                                                                          
054100     CLOSE W41224                                                         
054110           W4122B                                                         
054200     SKIP2                                                                
054300     MOVE 'S' TO POSTSUM-OPKOD                                            
054400     CALL POSTSUM USING POSTSUM-PARM                                      
054500     .                                                                    
054600     EJECT                                                                
054700 S01-READ-W41224  SECTION.                                                
054800     SKIP2                                                                
054900     READ W41224 INTO IN-AREA                                             
055000     AT END                                                               
055100        SET END-OF-W41224 TO TRUE                                         
055200                                                                          
055300     NOT AT END                                                           
055400        MOVE 'W41224' TO POSTSUM-FDNAMN                                   
055500        MOVE 'W41224D1' TO POSTSUM-DDNAMN2                                
055600        MOVE 'IN'      TO POSTSUM-TRANSTYP                                
055700        CALL POSTSUM USING POSTSUM-PARM                                   
055800                                                                          
055900     END-READ                                                             
056000     .                                                                    
056100     EJECT                                                                
056110  S02-WRITE-FILE SECTION.                                                 
056120                                                                          
056123     MOVE RAD-IDDISTR  TO UT-IDDISTR                                      
056124     MOVE RAD-IDKUNDNR TO UT-IDKUNDNR                                     
056125     MOVE RAD-IDORDNR5 TO UT-IDORDNR5                                     
056126     MOVE SPARA-IDORDNR TO UT-IDORDNR-NEW                                 
056127     MOVE RAD-IDARTNR  TO UT-IDARTNR                                      
056128     MOVE RAD-KVART    TO UT-RAD-KVART                                    
056129     MOVE IN-IDDC      TO UT-IDDC                                         
056130     WRITE W4122B-POST FROM UT-AREA AFTER 1                               
056140     .                                                                    
056150     EJECT                                                                
056200* --- IMS SECTIONS  ---                                                   
056300                                                                          
056400     EJECT                                                                
056500 IMS-GHU-WDA501 SECTION.                                                  
056600                                                                          
056700     STRING 'WDA501  (WDA501KY= ' W-WDA5KEY-X ')'                         
056800          DELIMITED BY SIZE INTO SSA1                                     
056900     MOVE '  GE' TO GOOD-STATUSCODES                                      
057000     CALL CBLTDLI USING GHU WDA5-PCB DLI-IO-WDA501 SSA1                   
057100     MOVE WDA5-STATUS-CODE TO STATUS-WS                                   
057200     PERFORM IMS-STATUSCHECK                                              
057300     .                                                                    
057400     EJECT                                                                
057500 IMS-REPL-WDA501 SECTION.                                                 
057600                                                                          
057700     MOVE '  ' TO GOOD-STATUSCODES                                        
057800                                                                          
057900     CALL CBLTDLI USING REPL WDA5-PCB DLI-IO-WDA501                       
058000     MOVE WDA5-STATUS-CODE TO STATUS-WS                                   
058100     PERFORM IMS-STATUSCHECK                                              
058200     .                                                                    
058300     EJECT                                                                
058400 IMS-GU-WDB201 SECTION.                                                   
058500                                                                          
058600     STRING 'WDB201  (IDGMT    =' W-IDGMT-X ')'                           
058700          DELIMITED BY SIZE INTO SSA1                                     
058800     MOVE '  ' TO GOOD-STATUSCODES                                        
058900     CALL CBLTDLI USING GU WDB2-PCB DLI-IO-WDB201 SSA1                    
059000     MOVE WDB2-STATUS-CODE TO STATUS-WS                                   
059100     PERFORM IMS-STATUSCHECK                                              
059200     .                                                                    
059300     EJECT                                                                
059400 IMS-GHU-WDK611 SECTION.                                                  
059500                                                                          
059600     STRING 'WDK601  (IDARTNR  =' W-IDARTNR-X ')'                         
059700            DELIMITED BY SIZE INTO SSA1                                   
059800     MOVE   'WDK611  '          TO SSA2                                   
059900     MOVE '  '                  TO GOOD-STATUSCODES                       
060000     CALL CBLTDLI USING GHU WDK6-PCB DLI-IO-WDK611 SSA1 SSA2              
060100     MOVE WDK6-STATUS-CODE      TO STATUS-WS                              
060200     PERFORM IMS-STATUSCHECK                                              
060300     .                                                                    
060400                                                                          
060500 IMS-REPL-WDK611 SECTION.                                                 
060600                                                                          
060700     MOVE '  ' TO GOOD-STATUSCODES                                        
060800                                                                          
060900     CALL CBLTDLI USING REPL WDK6-PCB DLI-IO-WDK611                       
061000     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
061100     PERFORM IMS-STATUSCHECK                                              
061200     .                                                                    
061300     EJECT                                                                
061400 IMS-GU-WDQ201-CSEQ SECTION.                                              
061500                                                                          
061600     STRING 'WDQ201  (WDQ2CSEQ =' W-IDGMTREF-X ')'                        
061700          DELIMITED BY SIZE INTO SSA1                                     
061800     MOVE '  ' TO GOOD-STATUSCODES                                        
061900     CALL CBLTDLI USING GU WDQ2-PCB DLI-IO-WDQ201 SSA1                    
062000     MOVE WDQ2-STATUS-CODE TO STATUS-WS                                   
062100     PERFORM IMS-STATUSCHECK                                              
062200     .                                                                    
062300     EJECT                                                                
062400 IMS-RESTART SECTION.                                                     
062500     SKIP2                                                                
062600     MOVE SPACE TO CHKP-MSG-IO-AREA                                       
062700     MOVE '  ' TO GOOD-STATUSCODES                                        
062800     CALL CBLTDLI USING XRST MSG-PCB                                      
062900                        CHKP-MSG-IO-AREA-LENGTH CHKP-MSG-IO-AREA          
063000                        CHKP-AREA-LENGTH CHKP-AREA                        
063100     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
063200     PERFORM IMS-STATUSCHECK                                              
063300     .                                                                    
063400     SKIP3                                                                
063500 IMS-CHECKPOINT SECTION.                                                  
063600     SKIP2                                                                
063700     MOVE SPACE TO CHKP-MSG-IO-AREA                                       
063800     MOVE '  XD' TO GOOD-STATUSCODES                                      
063900     CALL CBLTDLI USING CHKP MSG-PCB                                      
064000                        CHKP-MSG-IO-AREA-LENGTH CHKP-MSG-IO-AREA          
064100                        CHKP-AREA-LENGTH CHKP-AREA                        
064200     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
064300     PERFORM IMS-STATUSCHECK                                              
064400                                                                          
064500     IF IMS-NOT-OK                                                        
064600       MOVE 'IMS CONTROL REGION NOT ACCESSIBLE' TO ERROR-TEXT-STR         
064700       DISPLAY ERROR-TEXT                                                 
064800       CALL FELLOG                                                        
064900     END-IF                                                               
065000     .                                                                    
065100     EJECT                                                                
065200 IMS-STATUSCHECK SECTION.                                                 
065300     SKIP2                                                                
065400     SET STATUS-IX TO 1                                                   
065500     SEARCH GOOD-STATUS                                                   
065600       AT END                                                             
065700         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
065800           DELIMITED BY SIZE INTO ERROR-TEXT                              
065900         DISPLAY ERROR-TEXT                                               
066000         CALL FELLOG                                                      
066100       WHEN GOOD-STATUS (STATUS-IX) = STATUS-WS                           
066200         CONTINUE                                                         
066300     END-SEARCH                                                           
066400     .                                                                    
