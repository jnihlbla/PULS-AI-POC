000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W4285700.                                                
000300 AUTHOR.         SHILPA MADHURI G.                                        
000400 DATE-WRITTEN.   11/11/23.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700******************************************************************        
000800*                                                                         
000900*    FUNCTION:                                                            
001000*      PROGRAM WHICH TAKES THE W42850 FILE AND FETCHES VALUES             
001100*      INTO THE FIELDS BY READING THE DATABASES.                          
001200*        IT FETCHES CITY NAME FROM WDB6 DB                                
001300*        IT FETCHES STANDARD PRICE FROM WDK6                              
001400*        IT FETCHES AVG PRICE FROM WDK7                                   
001500*        IT CREATES 2 OUTPUT FILES WHICH WILL BE USED TO CREATE           
001600*      THE WEEKLY AND PERIODICAL MANAGEMENT REPORTS                       
001700*                                                                         
001710*                                                                         
001720*                                                                         
001730* 2012-11-02 E'TRACKER 10181786 WEB AUSTRALIEN FOLLOW-UP                  
001740*                                                                         
001750*                                                                         
001800******************************************************************        
001900                                                                          
002000     SKIP3                                                                
002100 ENVIRONMENT DIVISION.                                                    
002200     SKIP2                                                                
002300 INPUT-OUTPUT SECTION.                                                    
002400                                                                          
002500 FILE-CONTROL.                                                            
002600     SKIP2                                                                
002700*          --- W42850 FILE                                                
002800     SELECT W42850                     ASSIGN TO W42857D1.                
002900     SKIP2                                                                
003000*          --- W42851 FILE                                                
003100     SELECT W42851                     ASSIGN TO W42857D2.                
003200     SKIP2                                                                
003300*          --- W4285A FILE                                                
003400     SELECT W4285A                     ASSIGN TO W42857D3.                
003500     SKIP2                                                                
003600*          --- W4285B FILE                                                
003700     SELECT W4285B                     ASSIGN TO W42857D4.                
003800     EJECT                                                                
003900 DATA DIVISION.                                                           
004000     SKIP3                                                                
004100 FILE SECTION.                                                            
004200     SKIP3                                                                
004300 FD  W42850                                                               
004400     RECORDING       F                                                    
004500     BLOCK CONTAINS  0.                                                   
004600                                                                          
004700*01  -COPY W42850      -L.                                                
004800     SKIP3                                                                
004900 FD  W42851                                                               
005000     RECORDING       F                                                    
005100     BLOCK CONTAINS  0.                                                   
005200                                                                          
005300*01  -COPY W42851      -L.                                                
005400     SKIP3                                                                
005500 FD  W4285A                                                               
005600     RECORDING       F                                                    
005700     BLOCK CONTAINS  0.                                                   
005800                                                                          
005900*01  RECORD -COPY W42850 -PRE  W4285A-  -L.                               
006000     SKIP3                                                                
006100 FD  W4285B                                                               
006200     RECORDING       F                                                    
006300     BLOCK CONTAINS  0.                                                   
006400                                                                          
006500*01  RECORD -COPY W42851 -PRE  W4285B-  -L.                               
006600     EJECT                                                                
006700 WORKING-STORAGE SECTION.                                                 
006800                                                                          
006900 77  IDPGM                       PIC X(8)    VALUE 'W4285700'.            
007000 77  WS-PREV-IDDC                PIC X(2)    VALUE SPACES.                
007100 77  WS5A-SUARTSTD               PIC 9(8)V9(2) VALUE ZEROS.               
007200 77  WS5B-SUARTSTD               PIC 9(8)V9(2) VALUE ZEROS.               
007300 77  FELTEXT                     PIC X(80)   VALUE SPACE.                 
007400 77  WS-SURADER                  PIC 9(9)    VALUE ZEROS.                 
007500 77  W42850-EOF-SW               PIC X       VALUE 'N'.                   
007600     88  END-OF-W42850                       VALUE 'J'.                   
007700                                                                          
007800 77  W42851-EOF-SW               PIC X       VALUE 'N'.                   
007900     88  END-OF-W42851                       VALUE 'J'.                   
008000     EJECT                                                                
008100 01  TODAYS-DATE                 PIC 9(6)    VALUE ZERO.                  
008200 01  FILLER REDEFINES TODAYS-DATE.                                        
008300     03  TODAYS-DATE-YEAR        PIC 9(2).                                
008400     03  TODAYS-DATE-MONTH       PIC 9(2).                                
008500     03  TODAYS-DATE-DAY         PIC 9(2).                                
008600     EJECT                                                                
008700 01  GENERAL-SUBPROGRAMS.                                                 
008800*                                                                         
008900     03  ABEND                   PIC X(8)    VALUE 'ABEND   '.            
009000     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM '.            
009100     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
009200     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
009300     SKIP2                                                                
009400*    --- PARAMETERS TO ABEND                                              
009500                                                                          
009600 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
009700 77  RKOD-ABEND-NO-DUMP          PIC S9(4)   COMP VALUE +16.              
009800 77  RKOD-ABEND-WITH-DUMP        PIC S9(4)   COMP VALUE +1000.            
009900     SKIP2                                                                
013900 01  NYCKLAR-TILL-DLI.                                                    
014000     03  W-IDARTNR-X.                                                     
014100         05  W-IDARTNR           PIC S9(9)   VALUE ZERO COMP-3.           
014200     03  W-IDDC-X.                                                        
014300         05  W-IDDC              PIC X(2)    VALUE SPACE.                 
014400     03  W-IDDC-B6-X.                                                     
014500         05  W-IDDC-B6           PIC X(2)    VALUE SPACE.                 
014600                                                                          
017200 01  STATUS-WS                   PIC XX.                                  
017300     88  SEGMENT-FINNS                       VALUE '  '.                  
017400     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
017500     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
017600     SKIP2                                                                
017700 01  GODK-STATUSKODER.                                                    
017800     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
017900 01  SSA1                        PIC X(64).                               
018000 01  SSA2                        PIC X(64).                               
018100*    --- PARAMETRAR TILL POSTSUM                                          
018200*                                                                         
018300*01  -COPY W0005   -PRE  POSTSUM-                                         
018400*    --- COPYBOOKS DECLARATION                                            
018600*01  -COPY W0003                                                          
018700*                                                                         
018800     EJECT                                                                
018900 01  W42850-AREA-START           PIC X(24)   VALUE                        
019000                                 'W42850-AREA-START  '.                   
019100     SKIP2                                                                
019200                                                                          
019300*01  AREA -COPY W42850     -PRE W42850-                                   
019400     EJECT                                                                
019500 01  W42851-AREA-START           PIC X(24)   VALUE                        
019600                                 'W42851-AREA-START  '.                   
019700     SKIP2                                                                
019800                                                                          
019900*01  AREA -COPY W42851     -PRE W42851-                                   
020000     EJECT                                                                
020100 01  W4285A-AREA-START           PIC X(24)   VALUE                        
020200                                 'W4285A-AREA-START  '.                   
020300     SKIP2                                                                
020400                                                                          
020500*01  AREA -COPY W42850     -PRE W4285A-                                   
020600     EJECT                                                                
020700 01  W4285B-AREA-START           PIC X(24)   VALUE                        
020800                                 'W4285B-AREA-START  '.                   
020900     SKIP2                                                                
021000                                                                          
021100*01  AREA -COPY W42851     -PRE W4285B-                                   
021200     EJECT                                                                
021300                                                                          
021400*    ---  DLI INPUT-OUTPUT AREA                                           
021401                                                                          
021402 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK601'.                      
021403 01  DLI-IO-WDK601.                                                       
021404*    03  -COPY WDK601                                                     
021405     EJECT                                                                
021406 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK611'.                      
021407 01  DLI-IO-WDK611.                                                       
021408*    03  -COPY WDK611                                                     
021409     EJECT                                                                
021410                                                                          
021411 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK701'.                      
021412 01  DLI-IO-WDK701.                                                       
021413*    03  -COPY WDK701                                                     
021414     EJECT                                                                
021415 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK711'.                      
021416 01  DLI-IO-WDK711.                                                       
021417*    03  -COPY WDK711                                                     
021418     EJECT                                                                
021419                                                                          
022000 01  FILLER         PIC X(16)   VALUE 'DLI-IO-WDB601'.                    
022100 01   DLI-IO-WDB601.                                                      
022200*     03  -COPY WDB601                                                    
022300                                                                          
022400 LINKAGE SECTION.                                                         
022500*01  -COPY W0008   -PRE WDK6-                                             
022600     05  FILLER                  PIC X.                                   
022700     EJECT                                                                
022800*01  -COPY W0008   -PRE WDK7-                                             
022900     05  FILLER                  PIC X.                                   
023000     EJECT                                                                
023100*01  -COPY W0008   -PRE WDB6-                                             
023200     05  FILLER                  PIC X.                                   
023300     EJECT                                                                
023400 PROCEDURE DIVISION USING   WDK6-PCB WDK7-PCB WDB6-PCB.                   
023500 MAIN SECTION.                                                            
023600     ENTRY 'DLITCBL' USING  WDK6-PCB WDK7-PCB WDB6-PCB.                   
023700                                                                          
023800     PERFORM A-INIT                                                       
023900     PERFORM S01-READ-W42850                                              
024000     PERFORM S02-READ-W42851                                              
024100     PERFORM UNTIL END-OF-W42850                                          
024200      PERFORM B-PROCESS-W42850                                            
024300      PERFORM S01-READ-W42850                                             
024400     END-PERFORM                                                          
024500                                                                          
024600     PERFORM UNTIL END-OF-W42851                                          
024700      PERFORM D-PROCESS-W42851                                            
024800      PERFORM S02-READ-W42851                                             
024900     END-PERFORM                                                          
025000                                                                          
025100                                                                          
025200                                                                          
025300     PERFORM Z-FINIT                                                      
025400                                                                          
025500     MOVE ZERO TO RETURN-CODE                                             
025600     GOBACK                                                               
025700     .                                                                    
025800     EJECT                                                                
025900 A-INIT SECTION.                                                          
026000                                                                          
026100     OPEN INPUT  W42850                                                   
026200                 W42851                                                   
026300                                                                          
026400     OPEN OUTPUT W4285A                                                   
026500                 W4285B                                                   
026600     SKIP2                                                                
026700     ACCEPT TODAYS-DATE  FROM DATE                                        
026800     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
026900     .                                                                    
027000     EJECT                                                                
027100 B-PROCESS-W42850 SECTION.                                                
027200                                                                          
027300     MOVE W42850-IDARTNR   TO W-IDARTNR                                   
027400     MOVE W42850-IDDC-RET  TO W-IDDC                                      
027500     MOVE W42850-AREA      TO W4285A-AREA                                 
027510     IF W42850-IDDC-RET NOT = DCS-IDDC                                    
027511       MOVE W42850-IDDC-RET  TO W-IDDC-B6                                 
027520       PERFORM IMS-GU-WDB601                                              
027521     END-IF                                                               
027540     MOVE DCS-ADGMT-PADR(11:20) TO W4285A-ADCITY                          
027550                                                                          
027600     IF DCS-IDLANDX2 = 'CN'                                               
027601       PERFORM IMS-GU-WDK711                                              
027602       IF SEGMENT-FINNS                                                   
027603         COMPUTE WS5A-SUARTSTD =                                          
027604         (W42850-KVRETINL + W42850-KVRETINL-SKR) * SLAG-PRAVCOST          
027605       END-IF                                                             
027610     ELSE                                                                 
027700       PERFORM IMS-GU-WDK611                                              
027800       IF SEGMENT-FINNS                                                   
027900         COMPUTE WS5A-SUARTSTD =                                          
028000         (W42850-KVRETINL + W42850-KVRETINL-SKR) * CLAG-PRARTSTD          
028100       END-IF                                                             
029000     END-IF                                                               
029100       IF W42850-KVRETINL > 0                                             
029200         MOVE 1                   TO W4285A-KVRETINL                      
029300       END-IF                                                             
029400       IF W42850-KVAVV-KVANT > 0                                          
029500         MOVE 1                   TO W4285A-KVAVV-KVANT                   
029600       END-IF                                                             
029700       IF W42850-KVRETINL-SKR > 0                                         
029800         MOVE 1                   TO W4285A-KVRETINL-SKR                  
029900       END-IF                                                             
030000       IF W42850-KVAVV-KVAL   > 0                                         
030100         MOVE 1                   TO W4285A-KVAVV-KVAL                    
030200       END-IF                                                             
030300       MOVE W42850-KVDAGAR-INL    TO W4285A-KVDAGAR-INL                   
030400       MOVE W42850-TISAAPP-INLINL TO W4285A-TISAAPP-INLINL                
030500       MOVE W42850-TISAAVV-INLINL TO W4285A-TISAAVV-INLINL                
030600       MOVE W42850-TIINLINL       TO W4285A-TIINLINL                      
030710       MOVE W42850-KDMFUP         TO W4285A-KDMFUP                        
030800       MOVE W42850-IDARTNR        TO W4285A-IDARTNR                       
030900       MOVE W42850-DARETANK       TO W4285A-DARETANK                      
031000       MOVE W42850-KDANMORS       TO W4285A-KDANMORS                      
031100       MOVE W42850-IDRAPPNR       TO W4285A-IDRAPPNR                      
031200       MOVE W42850-IDKUNDNR       TO W4285A-IDKUNDNR                      
031300       MOVE W42850-IDDISTR        TO W4285A-IDDISTR                       
031400       MOVE W42850-IDDC-RET       TO W4285A-IDDC-RET                      
031500       MOVE WS5A-SUARTSTD         TO W4285A-SUARTSTD                      
031600                                                                          
032100     PERFORM S11-WRITE-W4285A                                             
032200     INITIALIZE WS5A-SUARTSTD                                             
032300     .                                                                    
032400     EJECT                                                                
032500                                                                          
032600 D-PROCESS-W42851 SECTION.                                                
032700                                                                          
032800     IF WS-PREV-IDDC = SPACES                                             
032900       ADD 1                      TO WS-SURADER                           
033000       CONTINUE                                                           
033100     ELSE                                                                 
033200      IF W42851-IDDC-RET NOT EQUAL TO WS-PREV-IDDC                        
033300       MOVE ZERO                  TO W4285B-IDARTNR                       
033400       MOVE ZERO                  TO W4285B-KVLEVANM-BEKR                 
033500       ADD WS5B-SUARTSTD          TO W4285B-SUARTSTD                      
033600       MOVE WS-PREV-IDDC          TO W4285B-IDDC-RET                      
033700       MOVE WS-SURADER            TO W4285B-SURADER                       
033800       PERFORM S12-WRITE-W4285B                                           
033900       MOVE W4285B-IDDC-RET       TO WS-PREV-IDDC                         
034000       INITIALIZE WS5B-SUARTSTD W4285B-AREA WS-SURADER                    
034100       ADD 1                      TO WS-SURADER                           
034200      ELSE                                                                
034300       ADD 1                      TO WS-SURADER                           
034400      END-IF                                                              
034500     END-IF                                                               
034600       MOVE W42851-IDARTNR   TO W-IDARTNR                                 
034700       MOVE W42851-AREA      TO W4285B-AREA                               
034800       MOVE W42851-IDDC-RET  TO WS-PREV-IDDC                              
034900       MOVE W42851-IDDC-RET  TO W-IDDC                                    
034901                                                                          
034910     IF W42851-IDDC-RET NOT = DCS-IDDC                                    
034911       MOVE W42851-IDDC-RET  TO W-IDDC-B6                                 
034920       PERFORM IMS-GU-WDB601                                              
034930     END-IF                                                               
035000     IF DCS-IDLANDX2 = 'CN'                                               
035002        PERFORM IMS-GU-WDK711                                             
035003        IF SEGMENT-FINNS                                                  
035004          COMPUTE WS5B-SUARTSTD =                                         
035005          (W42851-KVLEVANM-BEKR * SLAG-PRAVCOST)                          
035006        END-IF                                                            
035012     ELSE                                                                 
035100       PERFORM IMS-GU-WDK611                                              
035200       IF SEGMENT-FINNS                                                   
035300         COMPUTE WS5B-SUARTSTD =                                          
035400         (W42851-KVLEVANM-BEKR * CLAG-PRARTSTD)                           
035500       END-IF                                                             
035600     END-IF                                                               
036500     .                                                                    
036600     EJECT                                                                
036700 S01-READ-W42850  SECTION.                                                
036800     READ W42850 INTO W42850-AREA                                         
036900     AT END                                                               
037000        MOVE HIGH-VALUE TO W42850-AREA                                    
037100        SET END-OF-W42850 TO TRUE                                         
037200                                                                          
037300     NOT AT END                                                           
037400        MOVE 'W42850' TO POSTSUM-FDNAMN                                   
037500        MOVE 'W42857D1' TO POSTSUM-DDNAMN2                                
037600        MOVE SPACES        TO POSTSUM-TRANSTYP                            
037700        CALL POSTSUM USING POSTSUM-PARM                                   
037800     END-READ                                                             
037900     .                                                                    
038000     EJECT                                                                
038100 S02-READ-W42851  SECTION.                                                
038200     READ W42851 INTO W42851-AREA                                         
038300     AT END                                                               
038400        MOVE HIGH-VALUE TO W42851-AREA                                    
038500        SET END-OF-W42851 TO TRUE                                         
038600                                                                          
038700     NOT AT END                                                           
038800        MOVE 'W42851' TO POSTSUM-FDNAMN                                   
038900        MOVE 'W42857D2' TO POSTSUM-DDNAMN2                                
039000        MOVE SPACES        TO POSTSUM-TRANSTYP                            
039100        CALL POSTSUM USING POSTSUM-PARM                                   
039200     END-READ                                                             
039300     .                                                                    
039400     EJECT                                                                
039500 S11-WRITE-W4285A SECTION.                                                
039600                                                                          
039700     WRITE W4285A-RECORD FROM W4285A-AREA                                 
039800                                                                          
039900     MOVE SPACES        TO POSTSUM-TRANSTYP                               
040000     MOVE 'W4285A' TO POSTSUM-FDNAMN                                      
040100     MOVE 'W42857D3' TO POSTSUM-DDNAMN2                                   
040200     CALL POSTSUM USING POSTSUM-PARM                                      
040300     .                                                                    
040400     EJECT                                                                
040500 S12-WRITE-W4285B SECTION.                                                
040600                                                                          
040700     WRITE W4285B-RECORD FROM W4285B-AREA                                 
040800                                                                          
040900     MOVE SPACES        TO POSTSUM-TRANSTYP                               
041000     MOVE 'W4285B' TO POSTSUM-FDNAMN                                      
041100     MOVE 'W42857D4' TO POSTSUM-DDNAMN2                                   
041200     CALL POSTSUM USING POSTSUM-PARM                                      
041300     .                                                                    
041400     EJECT                                                                
041500 Z-FINIT SECTION.                                                         
041600                                                                          
041700     MOVE ZERO                  TO W4285B-IDARTNR                         
041800     MOVE ZERO                  TO W4285B-KVLEVANM-BEKR                   
041900     ADD WS5B-SUARTSTD          TO W4285B-SUARTSTD                        
042000     MOVE WS-PREV-IDDC          TO W4285B-IDDC-RET                        
042100     MOVE WS-SURADER            TO W4285B-SURADER                         
042200     PERFORM S12-WRITE-W4285B                                             
042300     CLOSE W42850                                                         
042400           W42851                                                         
042500           W4285A                                                         
042600           W4285B                                                         
042700     MOVE 'S' TO POSTSUM-OPKOD                                            
042800     CALL POSTSUM USING POSTSUM-PARM                                      
042900     .                                                                    
043000     EJECT                                                                
128400 IMS-GU-WDK611 SECTION.                                                   
128500                                                                          
128600     STRING 'WDK601  (IDARTNR  =' W-IDARTNR-X ')'                         
128700          DELIMITED BY SIZE INTO SSA1                                     
128800     MOVE 'WDK611   ' TO SSA2                                             
128900     MOVE '  GE' TO GODK-STATUSKODER                                      
129000     CALL CBLTDLI USING GU WDK6-PCB DLI-IO-WDK611  SSA1 SSA2              
129100     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
129200     PERFORM IMS-STATUSKONTROLL                                           
129300     .                                                                    
129400     EJECT                                                                
129500 IMS-GU-WDK711 SECTION.                                                   
129600                                                                          
129700     STRING 'WDK701  (IDARTNR  =' W-IDARTNR-X ')'                         
129800          DELIMITED BY SIZE INTO SSA1                                     
129900     STRING 'WDK711  (IDDC     =' W-IDDC-X ')'                            
130000          DELIMITED BY SIZE INTO SSA2                                     
130100     MOVE '  GE' TO GODK-STATUSKODER                                      
130200     CALL CBLTDLI USING GU WDK7-PCB DLI-IO-WDK711  SSA1 SSA2              
130300     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
130400     PERFORM IMS-STATUSKONTROLL                                           
130500     .                                                                    
130600     EJECT                                                                
131800 IMS-GU-WDB601    SECTION.                                                
131900     STRING 'WDB601  (IDDC     =' W-IDDC-B6-X ')'                         
132000          DELIMITED BY SIZE INTO SSA1                                     
132100     MOVE '    ' TO GODK-STATUSKODER                                      
132200     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-WDB601    SSA1                 
132300     MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
132400     PERFORM IMS-STATUSKONTROLL                                           
132800     .                                                                    
138700 IMS-STATUSKONTROLL SECTION.                                              
138800                                                                          
138900     SET STATUS-IX TO 1                                                   
139000     SEARCH GODK-STATUS                                                   
139100       AT END                                                             
139200         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
139300         DELIMITED BY SIZE INTO FELTEXT                                   
139400         CALL FELLOG                                                      
139500       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
139600         CONTINUE                                                         
139700     END-SEARCH                                                           
139800     .                                                                    
139900 S99-ABEND SECTION.                                                       
140000                                                                          
140100     SKIP2                                                                
140200     MOVE 'S' TO POSTSUM-OPKOD                                            
140300     CALL POSTSUM USING POSTSUM-PARM                                      
140400     CALL ABEND USING RKOD-ABEND                                          
140500     .                                                                    
140600     EJECT                                                                
