000100 PROCESS DYNAM                                                            
000200 ID DIVISION.                                                             
000300     SKIP2                                                                
000400 PROGRAM-ID.     W2711200.                                                
000500*AUTHOR.         STEFAN KIHLBERG                                          
000600*DATE-WRITTEN.   94/12/01.                                                
000700                                                                          
000800*    REMARKS                                                              
000900*                                                                         
001000*    FUNKTION:                                                            
001100*        REFILLRELEASE - ALLA ORDER PÅ WDE3 MED ANGIVET IDDC LÄSES        
001200*        IGENOM.                                                          
001300*        FILER SKAPAS:                                                    
001400*          - FIL MED ORDRAR, GODKÄNDA FÖRSLAG , RETURER, GODKÄNDA         
001500*            FLYGFÖRSLAG OCH TRANSFERORDRAR (W41210)                      
001600*          - FIL MED SAMTLIGA ORDRAR OCH RETURER SOM SKA                  
001700*            RENSAS (W27113)                                              
001800*          - FIL MED SAMTLIGA ORDER PÅ LOKALA ARTIKLAR (W27115)           
001900*                                                                         
002000*        SLÄPPDAGR FÖR ORDRAR ÄR ENLIGT KALENDER(INTE SOP-DATUM)          
002100*        PROGRAMMET LÄSER      WLORDL (WDE3)                              
002200*                                      WDQ2                               
002300*                   UPPDATERAR         WDK6                               
002400*                                                                         
002500*                                                                         
002600*    ABENDKODER:                                                          
002700*        U0016 -  . . . .                                                 
002800*        U1000 -  . . . .                                                 
002900*                                                                         
003000     SKIP3                                                                
003100 ENVIRONMENT DIVISION.                                                    
003200     SKIP2                                                                
003300 INPUT-OUTPUT SECTION.                                                    
003400                                                                          
003500 FILE-CONTROL.                                                            
003600     SKIP2                                                                
003700*          --- FIL MED REFILL                                             
003800     SELECT W41210                     ASSIGN TO W27112D1.                
003900     SKIP2                                                                
004000*          --- RENSNINGS-FIL                                              
004100     SELECT W27113                     ASSIGN TO W27112D2.                
004200     SKIP2                                                                
004300*          --- FIL MED LOKALA ARTIKLAR USA                                
004400     SELECT W27115                     ASSIGN TO W27112D3.                
004500     SKIP2                                                                
004600*          --- IN-FIL MED IDDC                                            
004700     SELECT W271DC                    ASSIGN TO W27112D4.                 
004800     EJECT                                                                
004900 DATA DIVISION.                                                           
005000     SKIP3                                                                
005100 FILE SECTION.                                                            
005200     SKIP3                                                                
005300 FD  W41210                                                               
005400     RECORDING       V                                                    
005500     BLOCK CONTAINS  0.                                                   
005600                                                                          
005700 01  REFILL-POST          PIC X(160).                                     
005800                                                                          
005900                                                                          
006000 FD  W27113                                                               
006100     RECORDING       F                                                    
006200     BLOCK CONTAINS  0.                                                   
006300                                                                          
006400*01  POST -COPY W27113 -PRE RENS-     -L.                                 
006500                                                                          
006600 FD  W27115                                                               
006700     RECORDING       F                                                    
006800     BLOCK CONTAINS  0.                                                   
006900                                                                          
007000*01  POST -COPY W27115 -PRE LOKAL-    -L.                                 
007100                                                                          
007200                                                                          
007300 FD  W271DC                                                               
007400     LABEL RECORD STANDARD                                                
007500     RECORDING F                                                          
007600     BLOCK CONTAINS 0.                                                    
007700                                                                          
007800 01  FILLER                  PIC X(80).                                   
007900                                                                          
008000                                                                          
008100     EJECT                                                                
008200 WORKING-STORAGE SECTION.                                                 
008300     SKIP2                                                                
008400*    -COPY WY2000W1                                                       
008500     SKIP3                                                                
008600 77  IDPGM                       PIC X(8)    VALUE 'W2711200'.            
008700*                                                                         
008800 77  JA                          PIC X       VALUE 'J'.                   
008900 77  NEJ                         PIC X       VALUE 'N'.                   
009000 77  IX                          PIC 9(3)    VALUE ZERO.                  
009010 77  SUB                         PIC 9(1)    VALUE ZERO.                  
009100*                                                                         
009200 77  KDFARLIG-SW                 PIC X       VALUE 'N'.                   
009300     88  DANGEROUSGOODS                      VALUE 'J'.                   
009400     88  NOTDANGEROUS                        VALUE 'N'.                   
009500                                                                          
009501 77  WEEK-SW                     PIC X   VALUE ' '.                       
009502     88  WEEK-EVEN                       VALUE 'J'.                       
009503     88  WEEK-ODD                        VALUE 'N'.                       
009504                                                                          
009505 77  RELEASEDAY-SW               PIC X   VALUE 'N'.                       
009506     88  RELEASEDAY                      VALUE 'J'.                       
009507                                                                          
010000     SKIP2                                                                
010100 01  FELTEXT.                                                             
010200     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
010300     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
010400     EJECT                                                                
010500                                                                          
010600 01  ARBETSFALT.                                                          
010700     03 BEORDRAT-DC             PIC X(2) VALUE SPACE.                     
010800                                                                          
010900 01  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
011000 01  FILLER REDEFINES DAGENS-DATUM.                                       
011100     03  DAGENS-DATUM-AAR        PIC 9(2).                                
011200     03  DAGENS-DATUM-MAANAD     PIC 9(2).                                
011300     03  DAGENS-DATUM-DAG        PIC 9(2).                                
011400                                                                          
011500     EJECT                                                                
011600 01  ARBETSFAELT.                                                         
011700     03  WS-TID                  PIC 9(8).                                
011800     03  WS-IDORDNR              PIC 9(7).                                
011900     03  WS-IDORDNR-FLYG         PIC 9(7).                                
012000     03  WS-IDORDNR-FLYG-17-AUS  PIC 9(7).                                
012100     03  WS-IDORDNR-KDFRAKT-41   PIC 9(7).                                
012200     03  WS-TID-FLYG             PIC 9(2).                                
012300     03  WS-TID-FLYG-17-AUS      PIC 9(2).                                
012400     03  WS-TID-EVENING          PIC 9(2).                                
012500     03  WS-TID-TIMME            PIC 9(2).                                
012600*                                                                         
012700     03  WS-IDKUNDNR             PIC 9(6).                                
012800     03  WS-NEW-CUSTID.                                                   
012900         05 FILLER               PIC 9(2)    VALUE ZERO.                  
013000         05 WS-CUSTID-A          PIC 9(2)    VALUE ZERO.                  
013100         05 WS-CUSTID-B          PIC 9(2)    VALUE ZERO.                  
013200*                                                                         
013300     03  WS-KDFRAKT              PIC 9(2).                                
013301     03  WS-TIREFBAT             PIC 9(2).                                
013310     03  WS-FLFRAKDG             PIC X       VALUE SPACE.                 
013400*                                                                         
013500     03  WS-KDORDKL              PIC 9.                                   
013600     03  WS-HELTAL               PIC S9(7)   VALUE ZERO COMP-3.           
013700*                                                                         
013800     03 WS-IDDISTR               PIC S9(5)   COMP-3.                      
013900     03 WS-KDARBTYP              PIC X(8)    VALUE SPACE.                 
014000     03 WS-IDDISTR-JMF           PIC X(4)    VALUE SPACE.                 
014100     03 WS-IDKUNDNR-JMF          PIC X(6)    VALUE SPACE.                 
014200     03 WS-IDDISTR-JMF-NUM       PIC 9(4)    VALUE ZERO.                  
014300     03 WS-IDKUNDNR-JMF-NUM      PIC 9(6)    VALUE ZERO.                  
014400     03 WS-IDLEVNR-JMF           PIC X(5)    VALUE SPACE.                 
014500                                                                          
014501     03 WS-WEEKBY2-QUOTIENT      PIC 9(2)   VALUE ZERO.                   
014502     03 WS-WEEKBY2-REMAINDER     PIC 9(2)   VALUE ZERO.                   
014503                                                                          
014600     03  WS-ADLAGOMR-9      PIC  9(2)     VALUE ZERO.                     
014700                                                                          
014800     03 WS-ADART-X.                                                       
014900        05 WS-ADLAGOMR-X    PIC  9(2).                                    
015000        05 WS-ADGANG-X      PIC  9(2).                                    
015100        05 WS-ADPLATS-X     PIC  X(5)     VALUE SPACE.                    
015200        05 FILLER           PIC  X(1)     VALUE SPACE.                    
015300* OM ÄNDRING AV LAYOUT PÅ WS-ADART-X GÖRS MÅSTE MOTSVARANDE               
015400* ÄNDRING GÖRAS I PGM W40376.                                             
015500                                                                          
015600     03 WS-ADART-CD-X.                                                    
015700        05 WS-ADLAGOMR-CD-X PIC  9(2).                                    
015800        05 WS-ADGANG-CD-X   PIC  9(2).                                    
015900        05 WS-ADPLATS-CD-X  PIC  X(5)     VALUE SPACE.                    
016000        05 FILLER           PIC  X(1)     VALUE SPACE.                    
016100* OM ÄNDRING AV LAYOUT PÅ WS-ADART-CD-X GÖRS MÅSTE MOTSVARANDE            
016200* ÄNDRING GÖRAS I PGM W4120100                                            
016300                                                                          
016400* BELOW FLAGS SHOULD BE RETAINED UNTIL PROPOSAL REVIEWED                  
016500* VALUES IN FIELD KDREFTXT IN WDE3                                        
016600     03 WS-CHECK-FLAG       PIC 9(02).                                    
016700        88 RETAIN-FLAG      VALUE 02 18 19 21 22 40 45 76.                
016800     EJECT                                                                
016900                                                                          
017000*01  -COPY WWDIST35                                                       
017100     EJECT                                                                
017200                                                                          
017300 01  DYNAMISKA-SUBPROGRAM.                                                
017400*                                                                         
017500     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
017600     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
017700     03  WDATKONV                PIC X(8)    VALUE 'WDATKONV'.            
017800     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
017900     03  ABEND                   PIC X(8)    VALUE 'ABEND   '.            
018000     EJECT                                                                
018100*    --- PARAMETRAR TILL DATKONV                                          
018200*                                                                         
018300*01  -COPY WDATAREA                                                       
018400     EJECT                                                                
018500*    --- PARAMETRAR TILL POSTSUM                                          
018600*                                                                         
018700*01  -COPY W0005   -PRE  POSTSUM-                                         
018800     EJECT                                                                
018900 01  REFILL-AREA-START           PIC X(24)   VALUE                        
019000                                             'REFILL-AREA-START'.         
019100     SKIP2                                                                
019200                                                                          
019300 01  REFILL-AREA.                                                         
019400     03 -COPY W412RX3     -PRE REFILL-.                                   
019500     03 FILLER              PIC X(80).                                    
019600                                                                          
019700     EJECT                                                                
019800 01  RENS-AREA-START             PIC X(24)   VALUE                        
019900                                             'RENS-AREA-START'.           
020000     SKIP2                                                                
020100                                                                          
020200*01  AREA -COPY W27113      -PRE RENS-                                    
020300*                                                                         
020400     EJECT                                                                
020500 01  LOKAL-AREA-START             PIC X(24)   VALUE                       
020600                                             'LOKAL-AREA-START'.          
020700     SKIP2                                                                
020800                                                                          
020900*01  AREA -COPY W27115      -PRE LOKAL-                                   
021000*                                                                         
021100     EJECT                                                                
021200 01  IN-AREA-START               PIC X(24)   VALUE                        
021300                                             'IN-AREA-START'.             
021400     SKIP2                                                                
021500                                                                          
021600                                                                          
022300                                                                          
022500 01  DC-POST.                                                             
022600     03  DC-PARAMETER-TIME    PIC X(2).                                   
022700     03  FILLER               PIC X(78).                                  
022800                                                                          
022900 77  SW-SAMTLIGA-DC             PIC X   VALUE 'N'.                        
023000     88  SAMTLIGA-DC                    VALUE 'J'.                        
023100                                                                          
023200 77  SW-SAMTLIGA-BEST           PIC X   VALUE 'N'.                        
023300     88  SAMTLIGA-BEST                  VALUE 'J'.                        
023400                                                                          
023500 77  SW-PROCESS-LPO             PIC X   VALUE 'N'.                        
023600     88  PROCESS-LPO                    VALUE 'J'.                        
023700                                                                          
023800 77  SW-REFWAY-OK               PIC X   VALUE 'N'.                        
023900     88  REFWAY-OK                      VALUE 'J'.                        
024000                                                                          
024100                                                                          
024200 01  PARM-SYSIN.                                                          
024300     03  PARM-KDREFTYP        PIC X(1)  VALUE SPACE.                      
024400     03  PARM-IDDC            PIC X(2)  VALUE SPACE.                      
024500     03  PARM-IDDISTR         PIC X(4)  VALUE SPACE.                      
024600     03  PARM-IDKUNDNR        PIC X(6)  VALUE SPACE.                      
024700     03  PARM-IDLEVNR         PIC X(5)  VALUE SPACE.                      
024800     03  FILLER               PIC X(62).                                  
024900                                                                          
025000*      --- VALID IDDC CODES                                               
025100*                                                                         
025200*01    -COPY WWDC99                                                       
025300       EJECT                                                              
025400*                                                                         
025500                                                                          
025600     EJECT                                                                
025700 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
025800     SKIP3                                                                
025900 01  NYCKLAR-TILL-DLI.                                                    
026000     03 W-WDE301-X.                                                       
026100         05  W-IDDC-301          PIC X(2)  VALUE SPACE.                   
026200         05  W-IDPERSON-BUY      PIC S9(3) VALUE ZERO COMP-3.             
026300         05  W-KDREFTYP          PIC X     VALUE SPACE.                   
026400         05  W-IDARTNR-301       PIC S9(9) VALUE ZERO COMP-3.             
026500         05  W-IDDISTR           PIC S9(5) VALUE ZERO COMP-3.             
026600                                                                          
026700     03 W-WDE301KY-MIN-X.                                                 
026800         05  W-IDDC-MIN          PIC X(2)  VALUE SPACE.                   
026900         05  W-IDPERSON-BUY-MIN  PIC S9(3) VALUE ZERO COMP-3.             
027000         05  W-KDREFTYP-MIN      PIC X     VALUE SPACE.                   
027100         05  W-IDARTNR-MIN       PIC S9(9) VALUE ZERO COMP-3.             
027200         05  W-IDDISTR-MIN       PIC S9(5) VALUE ZERO COMP-3.             
027300                                                                          
027400     03 W-WDE301KY-MAX-X.                                                 
027500         05  W-IDDC-MAX          PIC X(2)  VALUE SPACE.                   
027600         05  W-IDPERSON-BUY-MAX  PIC S9(3) VALUE ZERO COMP-3.             
027700         05  W-KDREFTYP-MAX      PIC X     VALUE SPACE.                   
027800         05  W-IDARTNR-MAX       PIC S9(9)                                
027900                                         VALUE +999999999 COMP-3.         
028000         05  W-IDDISTR-MIN       PIC S9(5) VALUE +99999 COMP-3.           
028100                                                                          
028200     03 W-WDQ2C1KY-X.                                                     
028300        05  W-SEQC-IDDISTR      PIC S9(5)   VALUE +0 COMP-3.              
028400        05  W-SEQC-IDKUNDNR     PIC S9(7)   VALUE +0 COMP-3.              
028500        05  W-SEQC-IDKUNDRF.                                              
028600          07  W-SEQC-IDORDNR7   PIC 9(7)    VALUE ZERO.                   
028700          07  FILLER            PIC X(3)    VALUE SPACE.                  
028800                                                                          
028900     03  W-IDARTNR-X.                                                     
029000         05  W-IDARTNR           PIC S9(9)   VALUE ZERO COMP-3.           
029100                                                                          
029200     03  W-IDDC-B6-X.                                                     
029300         05  W-IDDC-B6       PIC X(2)   VALUE SPACE.                      
029400                                                                          
029500     03  W-IDDC-X.                                                        
029600         05  W-IDDC          PIC X(2)   VALUE SPACE.                      
029700                                                                          
029800     03  W-IDDC-B616-X.                                                   
029900         05  W-IDDC-B616       PIC X(2)   VALUE SPACE.                    
030000                                                                          
030100     03  W-KDSEGKEY-X.                                                    
030200         05  W-KDSEGKEY          PIC X       VALUE '1'.                   
030300                                                                          
030400     SKIP2                                                                
030500*    --- STATUS-KOD FRÅN IMS                                              
030600 01  STATUS-WS                   PIC XX.                                  
030700     88  SEGMENT-FINNS                       VALUE '  '.                  
030800     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
030900     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
031000     88  SEGMENT-SLUT                        VALUE 'GB'.                  
031100     88  IMS-EJ-OK                           VALUE 'XD'.                  
031200     SKIP2                                                                
031300 01  GODK-STATUSKODER.                                                    
031400     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
031500     SKIP3                                                                
031600 01  SSA1                        PIC X(128).                              
031700 01  SSA2                        PIC X(64).                               
031800     EJECT                                                                
031900*    --- IMS FUNKTIONSKODER                                               
032000*01  -COPY W0003                                                          
032100     EJECT                                                                
032200*    ---  DLI INPUT-OUTPUT AREA                                           
032300 01  FILLER                      PIC X(16)   VALUE 'MMI-IO-AREA'.         
032400     SKIP3                                                                
032500 01  DLI-IO-AREA-E301.                                                    
032600*        05  -COPY WDE301                                                 
032700     EJECT                                                                
032800 01  FILLER            PIC X(16)    VALUE 'DLI-IO-WDQ2C1'.                
032900 01  DLI-IO-WDQ2C1.                                                       
033000*    03  WDQ2C1 -COPY WDQ2C1                                              
033100     EJECT                                                                
033200 01  FILLER                      PIC X(16) VALUE 'DLI-IO-K601'.           
033300     SKIP3                                                                
033400 01  DLI-IO-AREA-K601.                                                    
033500*    03  -COPY WDK601                                                     
033600 01  FILLER                      PIC X(16) VALUE 'DLI-IO-K611'.           
033700     SKIP3                                                                
033800 01  DLI-IO-AREA-K611.                                                    
033900*    03  -COPY WDK611                                                     
034000     EJECT                                                                
034100 01  FILLER                      PIC X(16) VALUE 'DLI-IO-K701'.           
034200 01  DLI-IO-AREA-K701.                                                    
034300*    03  -COPY WDK701                                                     
034400 01  FILLER                      PIC X(16) VALUE 'DLI-IO-K711'.           
034500     SKIP3                                                                
034600 01  DLI-IO-AREA-K711.                                                    
034700*    03  -COPY WDK711                                                     
034800     EJECT                                                                
034900 01  FILLER               PIC X(16)   VALUE 'WDB601 AREA'.                
035000 01   DLI-IO-AREA-B601.                                                   
035100*     03  -COPY WDB601                                                    
035200     EJECT                                                                
035300 01  FILLER               PIC X(16)   VALUE 'WDB616 AREA'.                
035400 01   DLI-IO-AREA-B616.                                                   
035500*     03  -COPY WDB616       -PRE B6-                                     
035600     EJECT                                                                
035700 LINKAGE SECTION.                                                         
035800                                                                          
035900*01  -COPY W0009   -PRE MSG-                                              
036000     EJECT                                                                
036100*01  -COPY W0008  -PRE WDE3-                                              
036200     05  FILLER                  PIC X.                                   
036300     EJECT                                                                
036400*01  -COPY W0008  -PRE WDQ2C-                                             
036500     05  FILLER                  PIC X.                                   
036600     EJECT                                                                
036700*01  -COPY W0008  -PRE WDK6-                                              
036800     05  FILLER                  PIC X.                                   
036900     EJECT                                                                
037000*01  -COPY W0008      -PRE WDB6-                                          
037100     05  FILLER                  PIC X.                                   
037200     EJECT                                                                
037300*01  -COPY W0008      -PRE WDK7-                                          
037400     05  FILLER                  PIC X.                                   
037500     EJECT                                                                
037600 PROCEDURE DIVISION  USING MSG-PCB WDE3-PCB WDQ2C-PCB WDK6-PCB            
037700                           WDB6-PCB WDK7-PCB.                             
037800     ENTRY 'DLITCBL' USING MSG-PCB WDE3-PCB WDQ2C-PCB WDK6-PCB            
037900                           WDB6-PCB WDK7-PCB.                             
038000                                                                          
038100     PERFORM A-INIT                                                       
038200     PERFORM IMS-GU-E301                                                  
038300     PERFORM UNTIL SEGMENT-SAKNAS                                         
038400           MOVE NEJ          TO SW-SAMTLIGA-DC                            
038500                                SW-SAMTLIGA-BEST                          
038600                                SW-PROCESS-LPO                            
038700                                SW-REFWAY-OK                              
038720                                WS-FLFRAKDG                               
038900           MOVE ZERO         TO WS-TIREFBAT                               
038901                                                                          
038910           MOVE REF-IDDC     TO WS-IDDC                                   
039000                                W-IDDC-B6                                 
039100                                W-IDDC                                    
039200           MOVE SPACES       TO W-IDDC-B616                               
039300                                                                          
039400           MOVE REF-IDDISTR  TO WS-IDDISTR-JMF-NUM                        
039500                                DIST35-IDDISTR                            
039600           MOVE REF-IDKUNDNR TO WS-IDKUNDNR-JMF-NUM                       
039700           MOVE WS-IDKUNDNR-JMF-NUM TO WS-IDKUNDNR-JMF                    
039800           MOVE WS-IDDISTR-JMF-NUM  TO WS-IDDISTR-JMF                     
039900           INSPECT WS-IDKUNDNR-JMF      REPLACING                         
040000                LEADING SPACE BY ZERO                                     
040100           INSPECT WS-IDDISTR-JMF       REPLACING                         
040200                LEADING SPACE BY ZERO                                     
040300           MOVE REF-IDLEVNR         TO WS-IDLEVNR-JMF                     
040400*                                                                         
040500           MOVE REF-IDARTNR         TO W-IDARTNR                          
040520           PERFORM IMS-GU-K601                                            
040521           PERFORM IMS-GN-K611                                            
040530           MOVE NEJ TO KDFARLIG-SW                                        
040560           IF CLAG-KDFARLIG = 4                                           
040570             MOVE JA           TO KDFARLIG-SW                             
040580           END-IF                                                         
040590           IF REF-KDREFTYP = 'T'                                          
040591           AND NDC                                                        
040592             IF  WS-IDKUNDNR-JMF-NUM > 509                                
040593             AND WS-IDKUNDNR-JMF-NUM < 520                                
040594               MOVE '51'                 TO W-IDDC-B616                   
040595             ELSE                                                         
040596               MOVE WS-IDKUNDNR-JMF(5:2) TO W-IDDC-B616                   
040597             END-IF                                                       
040598           ELSE                                                           
040599             IF REF-KDREFTYP = 'T'                                        
040600*************TRANSFERS BETWEEN LDC/SDC ARE PERFORMED                      
040601*************EVERY DAY                                                    
040603*              MOVE JA      TO SW-REFWAY-OK                               
040604               MOVE 2       TO WS-TIREFBAT                                
040610             ELSE                                                         
040700               IF CDC-SE                                                  
040800                  IF CLAG-IDDC-REF > SPACES                               
040900                     MOVE CLAG-IDDC-REF TO W-IDDC-B616                    
041000                  END-IF                                                  
041100               ELSE                                                       
041200                  PERFORM IMS-GU-K711                                     
041300                  IF SLAG-IDDC-REF > SPACES                               
041400                     MOVE SLAG-IDDC-REF TO W-IDDC-B616                    
041500                  END-IF                                                  
041600               END-IF                                                     
041601             END-IF                                                       
041610           END-IF                                                         
041700                                                                          
041800           PERFORM IMS-GU-WDB601                                          
041810                                                                          
041811********LOCAL PO IS IN W271DB EVERY DAY*********                          
041820           IF ((DCS-FLLPO = JA) AND                                       
041830               (SLAG-IDDC-REF = SPACE))                                   
041831             MOVE 5         TO WS-TIREFBAT                                
041841             MOVE JA TO SW-PROCESS-LPO                                    
041850           END-IF                                                         
041860                                                                          
041900           IF W-IDDC-B616   > SPACES                                      
042000              PERFORM IMS-GU-WDB616                                       
042100              IF SEGMENT-FINNS                                            
042200                MOVE B6-REF-FLFRAKDG   TO WS-FLFRAKDG                     
042213                MOVE B6-REF-TIREFBAT TO WS-TIREFBAT                       
042300              ELSE                                                        
042301** SET TO BATCH W271D2                                                    
042310                MOVE 2                 TO WS-TIREFBAT                     
042600              END-IF                                                      
042700           END-IF                                                         
042800*                                                                         
042900****************W271V2   DC-PARAMETER = 99***********************         
043000****************W271D2   DC-PARAMETER = 02(RUNS AROUND 02.00)****         
043100****************W271DB   DC-PARAMETER = 05(RUNS AROUND 05.00)****         
043200****************W271B1   DC-PARAMETER = 89(ORDERED FRM 2343 )****         
043300***PARAMETER(TIREFBAT IS SET ON 4408 SCREEN TO TELL WHAT BATCH***         
043400           IF ((DC-PARAMETER-TIME    = '99')                              
043500           OR (DC-PARAMETER-TIME    = WS-TIREFBAT))                       
043600              MOVE JA      TO SW-REFWAY-OK                                
043700              IF DC-PARAMETER-TIME  = '99'                                
043800                 MOVE JA   TO SW-SAMTLIGA-DC                              
043900              END-IF                                                      
044700           ELSE                                                           
044800              IF DC-PARAMETER-TIME  = '89'                                
044900*****ORDERED FROM 2343                                                    
045000                 MOVE JA   TO SW-SAMTLIGA-BEST                            
045100              END-IF                                                      
045200           END-IF                                                         
045300*RETURER                                                                  
045400           IF REF-KDREFTYP = 'R'                                          
045500*************W271V2                                                       
045600             IF (SAMTLIGA-DC                                              
045700             AND GOOD-DC)                                                 
045800*************W271D2                                                       
045900             OR (REFWAY-OK                                                
046000             AND (DCS-SDC                                                 
046100             OR  DCS-NDC-CN                                               
046110             OR  DCS-NDC-SA                                               
046200             OR  DCS-NDC-OTHERS))                                         
046900*************W271DB                                                       
047000             OR (REFWAY-OK                                                
047100             AND (DCS-NDC-NA                                              
047200             OR   DCS-NDC-PF))                                            
047700*************SLÄPP FRÅN 2343                                              
047900             OR (SAMTLIGA-BEST                                            
048000             AND PARM-IDDC         = REF-IDDC                             
048100             AND PARM-KDREFTYP     = REF-KDREFTYP)                        
048300             OR (SAMTLIGA-BEST                                            
048400             AND PARM-IDDISTR      = WS-IDDISTR-JMF                       
048500             AND PARM-IDKUNDNR     = WS-IDKUNDNR-JMF                      
048600             AND PARM-KDREFTYP     = REF-KDREFTYP)                        
048700               PERFORM B-RETUR                                            
048800             END-IF                                                       
048900           ELSE                                                           
049000***           TRANSFERS                                                   
049100             IF REF-KDREFTYP = 'T'                                        
049200*            AND (REF-IDKUNDNR = 7000 OR 7001 OR 7002                     
049300*                             OR 7003 OR 7004 OR 7005                     
049400*                             OR 7006 OR 7007 OR 71                       
049500*                             OR 72   OR 73 OR 74                         
049600*                             OR 41   OR 43 OR 44                         
049700*                             OR 45   OR 46 OR 511                        
049800*                             OR 512  OR 513 OR 514                       
049900*                             OR 515  OR 516))                            
050000***            VECKOKÖRNING W271V2                                        
050100*              IF (SAMTLIGA-DC                                            
050200*              AND GOOD-DC)                                               
050300***            SDC BATCH W271D2 FÖR KINA                                  
050500*              OR (REFWAY-OK                                              
050600*              AND (REF-IDKUNDNR = 7000 OR 7001 OR 7002                   
050700*                               OR 7003 OR 7004 OR 7005                   
050800*                               OR 7006 OR 7007 OR 71                     
050900*                               OR 72   OR 73 OR 74))                     
051000************** W271DB                                                     
051200*              OR (REFWAY-OK                                              
051300*              AND (REF-IDKUNDNR =  41  OR 43   OR 44                     
051400*                                       OR 45   OR 46   OR 511            
051500*                                       OR 512  OR 513  OR 514            
051600*                                       OR 515  OR 516))                  
051700***            SLÄPP FRÅN 2343                                            
051900*              OR (SAMTLIGA-BEST                                          
051901               IF REFWAY-OK                                               
051910               OR (SAMTLIGA-BEST                                          
052000               AND PARM-IDDC       = REF-IDDC                             
052100               AND PARM-KDREFTYP   = REF-KDREFTYP)                        
052300               OR (SAMTLIGA-BEST                                          
052400               AND PARM-IDDISTR    = WS-IDDISTR-JMF                       
052500               AND PARM-IDKUNDNR   = WS-IDKUNDNR-JMF                      
052600               AND PARM-KDREFTYP   = REF-KDREFTYP)                        
052700                   PERFORM C-ORD-FORSL-LOK-TRA                            
052800               END-IF                                                     
052900             ELSE                                                         
053000*ORDER                                                                    
053100               IF REFWAY-OK                                               
053200               AND GOOD-DC                                                
053300***************W271V2                                                     
053400***************ALLT                                                       
053700***************W271D2                                                     
053800****LDC/SDC EUROPA + INTERNREFILL KINA + REFILL TILL CDC(CN+US)           
053900****REFILL USA TILL KINA                                                  
055100***************W271DB                                                     
055200****ALLA NDC-NA, NDC-PF, NDC/LDC-CN SOM FYLLS PÅ FRÅN CDC                 
055300****REFILL KINA TILL USA                                                  
056300***************SLÄPP FRÅN 2343                                            
056500               OR (SAMTLIGA-BEST                                          
056600               AND (PARM-IDDC       = REF-IDDC                            
056700               AND (PARM-KDREFTYP   = REF-KDREFTYP                        
056800               OR    (PARM-KDREFTYP = 'B'                                 
056900               AND    REF-KDREFTYP  = 'O')))                              
057200               OR  (PARM-IDDISTR    = WS-IDDISTR-JMF                      
057300               AND  PARM-IDLEVNR    = WS-IDLEVNR-JMF                      
057400               AND (PARM-KDREFTYP   = REF-KDREFTYP                        
057500               OR    (PARM-KDREFTYP = 'B'                                 
057600               AND    REF-KDREFTYP  = 'O')))                              
057700****FÖR TRANSFERS                                                         
058000               OR  (PARM-IDDISTR    = WS-IDDISTR-JMF                      
058100               AND  PARM-IDKUNDNR   = WS-IDKUNDNR-JMF                     
058200               AND  PARM-KDREFTYP   = REF-KDREFTYP))                      
058300                   PERFORM C-ORD-FORSL-LOK-TRA                            
058400               END-IF                                                     
058500             END-IF                                                       
058600           END-IF                                                         
058700       PERFORM IMS-GN-E301                                                
058800     END-PERFORM                                                          
058900                                                                          
059000     PERFORM Z-FINIT                                                      
059100                                                                          
059200     MOVE ZERO TO RETURN-CODE                                             
059300     GOBACK                                                               
059400     .                                                                    
059500     EJECT                                                                
059600 A-INIT SECTION.                                                          
059700     SKIP2                                                                
059800                                                                          
059900     OPEN INPUT W271DC                                                    
060000                                                                          
060100     PERFORM S20-LAES-W271DC                                              
060200                                                                          
060300     DISPLAY 'DC-PARAMETER : ' DC-PARAMETER-TIME                          
060400                                                                          
060600     IF DC-PARAMETER-TIME = '89'                                          
060700        ACCEPT PARM-SYSIN FROM SYSIN                                      
060800        UNSTRING PARM-SYSIN DELIMITED BY ','                              
060900          INTO PARM-KDREFTYP                                              
061000               PARM-IDDC                                                  
061100               PARM-IDDISTR                                               
061200               PARM-IDKUNDNR                                              
061300               PARM-IDLEVNR                                               
061400                                                                          
061500     END-IF                                                               
061600                                                                          
061700     DISPLAY 'PARM-IDDC     : ' PARM-IDDC                                 
061800     DISPLAY 'PARM-KDREFTYP : ' PARM-KDREFTYP                             
061900     DISPLAY 'PARM-IDDISTR  : ' PARM-IDDISTR                              
062000     DISPLAY 'PARM-IDKUNDNR : ' PARM-IDKUNDNR                             
062100     DISPLAY 'PARM-IDLEVNR  : ' PARM-IDLEVNR                              
062200                                                                          
062300     CLOSE W271DC                                                         
062400                                                                          
062500     OPEN OUTPUT W41210                                                   
062600                 W27113                                                   
062700                 W27115                                                   
062800                                                                          
062900     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
063000                                                                          
063100     ACCEPT DAGENS-DATUM FROM DATE                                        
063200     DISPLAY 'DAGENS-DATUM : ' DAGENS-DATUM                               
063300                                                                          
063400     MOVE 'AAMMDD'     TO DAT-KDDATFORM                                   
063500                                                                          
063600     MOVE DAGENS-DATUM TO DAT-I-TIDATUM                                   
063700                                                                          
063800     CALL WDATKONV USING DAT-KDDATFORM DAT-I-TIDATUM                      
063900                         DAT-O-TIDATUM DAT-KDSVAR                         
064000                                                                          
064010***************FOR TEST***************                                    
064011*    MOVE 7 TO DAT-TID                                                    
064020****************************************                                  
064100     IF DAT-KDSVAR-OK                                                     
064200       MOVE ZERO          TO WS-IDORDNR(1:2)                              
064300                             WS-IDORDNR-FLYG(1:2)                         
064400                             WS-IDORDNR-FLYG-17-AUS(1:2)                  
064500                             WS-IDORDNR-KDFRAKT-41(1:2)                   
064600       MOVE DAT-TIVV      TO WS-IDORDNR(3:2)                              
064700                             WS-IDORDNR-FLYG(3:2)                         
064800                             WS-IDORDNR-FLYG-17-AUS(3:2)                  
064900                             WS-IDORDNR-KDFRAKT-41(3:2)                   
065000       MOVE DAT-TID       TO WS-IDORDNR(5:1)                              
065100                             WS-IDORDNR-FLYG(5:1)                         
065200                             WS-IDORDNR-FLYG-17-AUS(5:1)                  
065300                             WS-IDORDNR-KDFRAKT-41(5:1)                   
065400                                                                          
065500       ACCEPT WS-TID FROM TIME                                            
065501       DIVIDE 2 INTO DAT-TIVV      GIVING WS-WEEKBY2-QUOTIENT             
065502                                REMAINDER WS-WEEKBY2-REMAINDER            
065503       IF DAT-TIVV = 1 OR WS-WEEKBY2-REMAINDER > 0                        
065504         SET WEEK-ODD        TO TRUE                                      
065505       ELSE                                                               
065506         SET WEEK-EVEN       TO TRUE                                      
065507       END-IF                                                             
065600       MOVE WS-TID(1:2)       TO WS-IDORDNR(6:2)                          
065700                                 WS-TID-TIMME                             
065800       COMPUTE WS-TID-EVENING = WS-TID-TIMME + 24                         
065900       MOVE WS-TID-EVENING    TO WS-IDORDNR-KDFRAKT-41(6:2)               
066000       COMPUTE WS-TID-FLYG = WS-TID-TIMME + 48                            
066100       MOVE WS-TID-FLYG       TO WS-IDORDNR-FLYG(6:2)                     
066200       COMPUTE WS-TID-FLYG-17-AUS = WS-TID-TIMME + 72                     
066300       MOVE WS-TID-FLYG-17-AUS                                            
066400                              TO WS-IDORDNR-FLYG-17-AUS(6:2)              
066500       DISPLAY 'WS-IDORDNR             : ' WS-IDORDNR                     
066600       DISPLAY 'WS-IDORDNR-FLYG        : ' WS-IDORDNR-FLYG                
066700       DISPLAY 'WS-IDORDNR-FLYG-17-AUS : ' WS-IDORDNR-FLYG-17-AUS         
066800       DISPLAY 'WS-IDORDNR-KDFRAKT-41  : ' WS-IDORDNR-KDFRAKT-41          
066900                                                                          
067000     ELSE                                                                 
067100       MOVE 'FEL I DATKONV1' TO FELTEXT-STR                               
067200       DISPLAY FELTEXT                                                    
067300       CALL FELLOG                                                        
067400     END-IF                                                               
067500     MOVE LOW-VALUE          TO W-WDE301KY-MIN-X                          
067600     MOVE HIGH-VALUE         TO W-WDE301KY-MAX-X                          
067700     .                                                                    
067800     EJECT                                                                
067900                                                                          
068000                                                                          
068100 B-RETUR SECTION.                                                         
068200                                                                          
068300     PERFORM S30-KUNDNUMMER-FRAKTKOD-RETUR                                
068400     PERFORM S77-SKAPA-LAGERPLATS                                         
068500     PERFORM S31-FLYTTA-TILL-RETURPOST                                    
068600     PERFORM S10-KOLLA-ORDERNR                                            
068700     PERFORM S11-SKRIV-W41210                                             
068800     PERFORM S60-FLYTTA-TILL-RENSPOST                                     
068900     PERFORM S13-SKRIV-W27113                                             
069000     .                                                                    
069100     EJECT                                                                
069200                                                                          
069300 C-ORD-FORSL-LOK-TRA SECTION.                                             
069400                                                                          
069500*       MASKINELLA ORDRAR ELLER                                           
069600*       GODKÄNDA MASKINELLA FÖRSLAG ELLER MANUELLA ORDRAR                 
069700*       PÅ CDC-ARTIKLAR ELLER LOKALA ARTIKLAR                             
069800*                                                                         
069900*                                                                         
070000*       (MANUELLA ORDRAR HAR KDREFTYP = B FÖR BÅTFÖRSLAG                  
070100*                                     = A FÖR FLYGFÖRSLAG                 
070200*                                     = C FÖR KRITISKT FLYGFÖRSLAG        
070300*                                     = L LOKALA ARTIKLAR N AM            
070400*                                     = T TRANSFER INOM N AM              
070500     IF   REF-KDREFTYP = 'O'                                              
070600     OR                                                                   
070700          REF-KDREFTYP = 'T'                                              
070800     OR                                                                   
070900       (( REF-KDREFTYP = 'B'     OR                                       
071000          REF-KDREFTYP = 'A'     OR                                       
071100          REF-KDREFTYP = 'C'     OR                                       
071200          REF-KDREFTYP = 'L' )                                            
071300                                         AND                              
071400          REF-KDREFORS = 'O')                                             
071500                                                                          
071600        IF REF-KVBEART = ZERO                                             
071700**        THE SUPERCEEDING MSGS ON 2372 AND 2382 SHOULD REMAIN            
071800**        UNTIL THE PROPOSALS ARE REVIEWED AND HENCE DO NOT               
071900**        DELETE FROM WDE3 HERE FOR                                       
072000**        KDREFTXT = 02,18,19,21,22,40,45,76.                             
072100          MOVE REF-KDREFTXT    TO WS-CHECK-FLAG                           
072200          IF RETAIN-FLAG                                                  
072300            CONTINUE                                                      
072400          ELSE                                                            
072500            MOVE NEJ TO RENS-FLREFNYO                                     
072600            PERFORM CA-EJ-GODKANDA-FORSLAG                                
072700          END-IF                                                          
072800        ELSE                                                              
072900           MOVE NEJ TO RENS-FLREFNYO                                      
072901           MOVE NEJ TO RELEASEDAY-SW                                      
072902           IF  REF-KDREFORS = 'O'                                         
072903           OR  REF-KDREFTYP = 'O'                                         
072905*          AND (REF-KDREFTYP = 'O' OR 'B' OR 'T' OR 'A' OR                
072906*                              'C' OR 'L')                                
072907             PERFORM CE-CHECK-RELEASEDAY                                  
072920           END-IF                                                         
073000           EVALUATE REF-KDREFTYP                                          
073100              WHEN 'O'                                                    
073200                 PERFORM CB-ORDER-GODK-FORSLAG-TRANSF                     
073300              WHEN 'B'                                                    
073400                 PERFORM CB-ORDER-GODK-FORSLAG-TRANSF                     
073500              WHEN 'T'                                                    
073600                 PERFORM CB-ORDER-GODK-FORSLAG-TRANSF                     
073700              WHEN 'L'                                                    
073800                 PERFORM CC-GODK-FORSLAG-LOKAL-ARTIKEL                    
073900              WHEN 'A'                                                    
074000                 PERFORM CD-GODK-FLYGORDER-NATT                           
074100              WHEN 'C'                                                    
074200                 PERFORM CD-GODK-FLYGORDER-NATT                           
074300           END-EVALUATE                                                   
074400        END-IF                                                            
074500                                                                          
074600     END-IF                                                               
074700     .                                                                    
074800     EJECT                                                                
074900                                                                          
075000                                                                          
075100 CA-EJ-GODKANDA-FORSLAG SECTION.                                          
075200                                                                          
075300     PERFORM S60-FLYTTA-TILL-RENSPOST                                     
075400     PERFORM S13-SKRIV-W27113                                             
075500     .                                                                    
075600     EJECT                                                                
075700                                                                          
075800                                                                          
075900 CB-ORDER-GODK-FORSLAG-TRANSF SECTION.                                    
076000                                                                          
077200                                                                          
077400     IF SAMTLIGA-BEST                                                     
077500     AND (PARM-IDDC = '21'                                                
077600     OR   PARM-IDDC = '23'                                                
077700     OR   PARM-IDDC = '24'                                                
077800     OR   PARM-IDDC = '25'                                                
077900     OR   PARM-IDDC = '26')                                               
078000****                                                                      
078100*          SNABBORDER FÖR SDC                                             
078200****                                                                      
078300       PERFORM S44-SNABBORDER-SDC                                         
078400     ELSE                                                                 
078500       PERFORM S40-KUNDNUMMER-REFILL                                      
078600       PERFORM S41-FRAKTKOD-REFILL                                        
078700       PERFORM S43-ORDERKLASS-REFILL                                      
078800     END-IF                                                               
078900                                                                          
079000     PERFORM S77-SKAPA-LAGERPLATS                                         
079100     PERFORM S42-FLYTTA-TILL-REFILLPOST                                   
079400                                                                          
079600     IF SAMTLIGA-BEST                                                     
079601     OR (REF-KDREFTYP    = 'O' AND RELEASEDAY)                            
079602     OR ( REF-KDREFORS = 'O' AND RELEASEDAY)                              
098601      IF DANGEROUSGOODS AND (WS-FLFRAKDG = 'Y' OR                         
098602                             WS-FLFRAKDG = 'J')                           
098603        MOVE WS-IDORDNR-KDFRAKT-41 TO REFILL-IDORDNR7                     
098604        MOVE '41'                    TO REFILL-KDFRAKT                    
098605      END-IF                                                              
098606      PERFORM S10-KOLLA-ORDERNR                                           
098700      PERFORM S11-SKRIV-W41210                                            
098800      PERFORM S60-FLYTTA-TILL-RENSPOST                                    
098900      PERFORM S13-SKRIV-W27113                                            
099000     END-IF                                                               
099100     .                                                                    
099200     EJECT                                                                
099300                                                                          
099400                                                                          
099500 CC-GODK-FORSLAG-LOKAL-ARTIKEL SECTION.                                   
099600                                                                          
099700     IF PROCESS-LPO                                                       
099800     OR SAMTLIGA-BEST                                                     
099900        MOVE REF-IDARTNR   TO LOKAL-IDARTNR                               
100000        MOVE REF-IDDC      TO LOKAL-IDDC                                  
100100        MOVE REF-IDLEVNR   TO LOKAL-IDLEVNR                               
100200        MOVE REF-KVBEART   TO LOKAL-KVBEART                               
100300        MOVE ZERO          TO LOKAL-KVDAGAR                               
100400        PERFORM S15-SKRIV-W27115                                          
100500                                                                          
100600        PERFORM S60-FLYTTA-TILL-RENSPOST                                  
100700        PERFORM S13-SKRIV-W27113                                          
100800     END-IF                                                               
100900     .                                                                    
101000     EJECT                                                                
101100                                                                          
101200                                                                          
101300 CD-GODK-FLYGORDER-NATT SECTION.                                          
101400                                                                          
101500     PERFORM S50-KUNDNUMMER-FLYG                                          
101600     PERFORM S51-FRAKTKOD-FLYG                                            
101700     PERFORM S77-SKAPA-LAGERPLATS                                         
101800     PERFORM S52-FLYTTA-TILL-FLYGPOST                                     
101900*  IF NOT ENSTAKA-DC GÖR SÅ ATT ORDRAR SLÄPPS DIREKT FRÅN                 
102000*                    2343 UTAN BEROENDE AV DAG                            
102204     IF (REF-KDREFORS = 'O' AND RELEASEDAY )                              
102205     OR SAMTLIGA-BEST                                                     
105300       PERFORM S10-KOLLA-ORDERNR                                          
105400       PERFORM S11-SKRIV-W41210                                           
105500       PERFORM S60-FLYTTA-TILL-RENSPOST                                   
105600       PERFORM S13-SKRIV-W27113                                           
105700     END-IF                                                               
105800     .                                                                    
105900     EJECT                                                                
106000                                                                          
106100                                                                          
106101 CE-CHECK-RELEASEDAY SECTION.                                             
106102                                                                          
106103     MOVE DAT-TID             TO SUB                                      
106104                                                                          
106105     IF SAMTLIGA-BEST                                                     
106106       MOVE 'Y'    TO RELEASEDAY-SW                                       
106107*******IF RELEASED FROM 2343 ALWAYS SET FLAG TO Y                         
106108     ELSE                                                                 
106109       EVALUATE TRUE                                                      
106110       WHEN REF-KDREFTYP = 'L'                                            
106111         SET RELEASEDAY       TO TRUE                                     
106112       WHEN (REF-KDREFTYP = 'A' OR                                        
106113             REF-KDREFTYP = 'C')                                          
106114        AND B6-REF-FLREFDAY(SUB) = 'J'                                    
106116         SET RELEASEDAY       TO TRUE                                     
106117       WHEN (REF-KDREFTYP = 'O' OR                                        
106118             REF-KDREFTYP = 'B' OR                                        
106119             REF-KDREFTYP = 'T')                                          
106120        AND NOTDANGEROUS                                                  
106121        AND B6-REF-FLREFBLK(SUB) = 'J'                                    
106123         SET RELEASEDAY       TO TRUE                                     
106124       WHEN (REF-KDREFTYP = 'O' OR                                        
106125             REF-KDREFTYP = 'B' OR                                        
106126             REF-KDREFTYP = 'T')                                          
106127        AND DANGEROUSGOODS                                                
106128        AND ( (B6-REF-KDREFDG(SUB) = 'J')                                 
106129            OR (B6-REF-KDREFDG(SUB) = 'E' AND WEEK-EVEN)                  
106130            OR (B6-REF-KDREFDG(SUB) = 'O' AND WEEK-ODD) )                 
106131         SET RELEASEDAY       TO TRUE                                     
106132       WHEN REF-KDREFTYP = 'T'                                            
106133        AND (SDC OR LDC)                                                  
106134         SET RELEASEDAY       TO TRUE                                     
106135       END-EVALUATE                                                       
106136     END-IF                                                               
106137     .                                                                    
106138     EJECT                                                                
106139                                                                          
106140                                                                          
106200 Z-FINIT SECTION.                                                         
106300                                                                          
106400                                                                          
106500     CLOSE W41210                                                         
106600           W27113                                                         
106700           W27115                                                         
106800     SKIP2                                                                
106900     MOVE 'S' TO POSTSUM-OPKOD                                            
107000     CALL POSTSUM USING POSTSUM-PARM                                      
107100     .                                                                    
107200     EJECT                                                                
107300                                                                          
107400 S10-KOLLA-ORDERNR SECTION.                                               
107500     MOVE REFILL-IDDISTR   TO W-SEQC-IDDISTR                              
107600     MOVE REFILL-IDKUNDNR  TO W-SEQC-IDKUNDNR                             
107700     MOVE REFILL-IDORDNR7  TO W-SEQC-IDORDNR7                             
107800     PERFORM IMS-GET-WDQ2C                                                
107900     PERFORM UNTIL SEGMENT-SAKNAS                                         
108000        ADD +1 TO W-SEQC-IDORDNR7                                         
108100                  REFILL-IDORDNR7                                         
108200        PERFORM IMS-GET-WDQ2C                                             
108300     END-PERFORM                                                          
108400     .                                                                    
108500     EJECT                                                                
108600                                                                          
108700 S11-SKRIV-W41210 SECTION.                                                
108800                                                                          
108900     IF REF-KVBEART-CD = ZERO                                             
109000***                                                                       
109100*** ORDERRAD MOT NORMAL LAGERPLATS (ENBART)                               
109200***                                                                       
109300       MOVE ZERO           TO REFILL-ADLAGOMR-CD                          
109400                              REFILL-ADGANG-CD                            
109500                              REFILL-ADPLATS-CD                           
109600       PERFORM S11A-SKRIV-W41210                                          
109700                                                                          
109800     ELSE                                                                 
109900***                                                                       
110000*** CROSS DOCKING STYRNING                                                
110100***                                                                       
110200                                                                          
110300       MOVE REF-IDARTNR      TO W-IDARTNR                                 
110400       PERFORM IMS-GHU-K611                                               
110500                                                                          
110600       MOVE 1                TO IX                                        
110700       PERFORM UNTIL IX > 4                                               
110800       OR REF-ADLAGOMR-CD = CLAG-ADLAGOMR-CD (IX)                         
110900         ADD 1               TO IX                                        
111000       END-PERFORM                                                        
111100                                                                          
111200       IF IX > 4                                                          
111300         MOVE 'FEL CROSS DOCKING LAGOMR I INFILEN'                        
111400                             TO FELTEXT-STR                               
111500         DISPLAY FELTEXT                                                  
111600         CALL FELLOG                                                      
111700       END-IF                                                             
111800                                                                          
111900       IF REF-KVBEART-CD > CLAG-KVLS-CD (IX)                              
112000                                                                          
112100**DOESN'T HAVE TO BE ADJUSTED FOR LOCAL KVQPACK-3                         
112200**UNTIL WE DEVELOPE CROSS DOCKING FROM OTHER DC'S                         
112300**THEN CDC /JOHAN 180423                                                  
112400         COMPUTE WS-HELTAL = CLAG-KVLS-CD (IX) /                          
112500                             CLAG-KVQPACK-3                               
112600                  ON SIZE ERROR                                           
112700                      MOVE ZERO   TO WS-HELTAL                            
112800         END-COMPUTE                                                      
112900         COMPUTE REFILL-KVBEART = CLAG-KVQPACK-3 * WS-HELTAL              
113000       ELSE                                                               
113100         MOVE REF-KVBEART-CD TO REFILL-KVBEART                            
113200       END-IF                                                             
113300                                                                          
113400       IF REFILL-KVBEART > ZERO                                           
113500         PERFORM S11A-SKRIV-W41210                                        
113600       END-IF                                                             
113700***                                                                       
113800*** NEDBOKNING AV RESERVERAT ANTAL CROSS DOCKING                          
113900*** AV VAD SOM HAR RESERVERATS                                            
114000*** OM (CD) SALDOT UNDERSTIGER BESTÄLLT/RESERVERAT ANTAL                  
114100*** HAR KVBEART-CD OCH KVBEART-CD-RES OLIKA VÄRDEN                        
114200***                                                                       
114300                                                                          
114400       COMPUTE CLAG-KVRESS-CD (IX) =                                      
114500               CLAG-KVRESS-CD (IX) - REF-KVBEART-CD                       
114600                                                                          
114700       COMPUTE CLAG-KVLS-CD (IX) =                                        
114800               CLAG-KVLS-CD (IX) - REFILL-KVBEART                         
114900                                                                          
115000       PERFORM IMS-REPL-K611                                              
115100                                                                          
115200       COMPUTE REFILL-KVBEART = REF-KVBEART - REFILL-KVBEART              
115300       IF REFILL-KVBEART > ZERO                                           
115400***                                                                       
115500*** ORDERRAD MOT NORMAL LAGERPLATS                                        
115600***                                                                       
115700         MOVE ZERO         TO REFILL-ADLAGOMR-CD                          
115800                              REFILL-ADGANG-CD                            
115900                              REFILL-ADPLATS-CD                           
116000         PERFORM S11A-SKRIV-W41210                                        
116100       END-IF                                                             
116200     END-IF                                                               
116300     IF CDC-SE                                                            
116400       CONTINUE                                                           
116500     ELSE                                                                 
116600       PERFORM IMS-GU-K711                                                
116700       IF SEGMENT-FINNS AND SLAG-TIDATUM-CROSS > 0                        
116800         MOVE ZERO         TO SLAG-TIDATUM-CROSS                          
116900         PERFORM IMS-REPL-K711                                            
117000       END-IF                                                             
117100     END-IF                                                               
117200     .                                                                    
117300     EJECT                                                                
117400                                                                          
117500                                                                          
117600 S11A-SKRIV-W41210 SECTION.                                               
117700                                                                          
117800     WRITE REFILL-POST FROM REFILL-AREA                                   
117900     EVALUATE REF-KDREFTYP                                                
118000        WHEN 'A'                                                          
118100           MOVE 'FLYG' TO POSTSUM-TRANSTYP                                
118200        WHEN 'C'                                                          
118300           MOVE 'CRIT' TO POSTSUM-TRANSTYP                                
118400        WHEN 'B'                                                          
118500           MOVE 'BOAT' TO POSTSUM-TRANSTYP                                
118600        WHEN 'L'                                                          
118700           MOVE 'TRSF' TO POSTSUM-TRANSTYP                                
118800        WHEN 'O'                                                          
118900           IF NDC                                                         
119000              MOVE 'NDCO' TO POSTSUM-TRANSTYP                             
119100           END-IF                                                         
119200           IF DCS-SDC                                                     
119300              MOVE 'SDCO' TO POSTSUM-TRANSTYP                             
119400           END-IF                                                         
119500        END-EVALUATE                                                      
119600     MOVE 'W41210 ' TO POSTSUM-FDNAMN                                     
119700     MOVE 'W27112D1' TO POSTSUM-DDNAMN2                                   
119800     CALL POSTSUM USING POSTSUM-PARM                                      
119900     .                                                                    
120000     EJECT                                                                
120100                                                                          
120200                                                                          
120300 S13-SKRIV-W27113 SECTION.                                                
120400                                                                          
120500     WRITE RENS-POST FROM RENS-AREA                                       
120600                                                                          
120700     MOVE  'RENS '   TO POSTSUM-TRANSTYP                                  
120800     MOVE 'W27113 '  TO POSTSUM-FDNAMN                                    
120900     MOVE 'W27112D2' TO POSTSUM-DDNAMN2                                   
121000     CALL POSTSUM USING POSTSUM-PARM                                      
121100     .                                                                    
121200     EJECT                                                                
121300                                                                          
121400                                                                          
121500 S15-SKRIV-W27115 SECTION.                                                
121600                                                                          
121700     WRITE LOKAL-POST FROM LOKAL-AREA                                     
121800                                                                          
121900     MOVE 'LOKAL'    TO POSTSUM-TRANSTYP                                  
122000     MOVE 'W27115 '  TO POSTSUM-FDNAMN                                    
122100     MOVE 'W27112D3' TO POSTSUM-DDNAMN2                                   
122200     CALL POSTSUM USING POSTSUM-PARM                                      
122300     .                                                                    
122400     EJECT                                                                
122500                                                                          
122600                                                                          
122700 S20-LAES-W271DC SECTION.                                                 
122800                                                                          
122900      READ W271DC   INTO DC-POST                                          
123000     .                                                                    
123100     EJECT                                                                
123200                                                                          
123300                                                                          
123400 S30-KUNDNUMMER-FRAKTKOD-RETUR SECTION.                                   
123500                                                                          
123600     MOVE REF-IDARTNR        TO W-IDARTNR                                 
123700     MOVE REF-IDKUNDNR     TO     WS-IDKUNDNR                             
123800     PERFORM IMS-GU-K711                                                  
123900     IF SLAG-IDDC-REF = '11'                                              
124000     AND REF-IDKUNDNR = ZERO                                              
124100        IF REF-KDREFORS = 'T' OR 'U'                                      
124200           MOVE DCS-IDKUNDNR-TRETUR                                       
124300                           TO WS-IDKUNDNR                                 
124400        END-IF                                                            
124500        IF REF-KDREFORS = 'N'                                             
124600           MOVE DCS-IDKUNDNR-RETUR                                        
124700                           TO WS-IDKUNDNR                                 
124800        END-IF                                                            
124900     ELSE                                                                 
125000       IF REF-IDKUNDNR = ZERO                                             
125100         MOVE SLAG-IDDC-REF TO W-IDDC-B616                                
125200         PERFORM IMS-GU-WDB616                                            
125300         IF SEGMENT-FINNS                                                 
125400           IF REF-KDREFORS = 'T' OR 'U'                                   
125500              MOVE B6-REF-IDKUNDNR-TRETUR                                 
125600                              TO WS-IDKUNDNR                              
125700           END-IF                                                         
125800           IF REF-KDREFORS = 'N'                                          
125900              MOVE B6-REF-IDKUNDNR-RETUR                                  
126000                              TO WS-IDKUNDNR                              
126100           END-IF                                                         
126200         END-IF                                                           
126300       END-IF                                                             
126400     END-IF                                                               
126500     .                                                                    
126600     EJECT                                                                
126700                                                                          
126800                                                                          
126900 S31-FLYTTA-TILL-RETURPOST SECTION.                                       
127000                                                                          
127100*RETUR SKRIVS SOM REFILLPOST                                              
127200                                                                          
127300     INITIALIZE               REFILL-W412RX3                              
127400                                                                          
127500     MOVE 'RX3'            TO REFILL-IDTYP                                
127600     MOVE REF-IDDISTR      TO REFILL-IDDISTR                              
127700     MOVE  WS-IDKUNDNR     TO REFILL-IDKUNDNR                             
127800     MOVE WS-IDORDNR       TO REFILL-IDORDNR7                             
127900     MOVE 1                TO REFILL-KDCLAGER                             
128000     IF (DCS-SDC AND NOT DCS-CHINA)                                       
128100         MOVE 1            TO REFILL-KDORDKL                              
128200     ELSE                                                                 
128300         MOVE 3            TO REFILL-KDORDKL                              
128400     END-IF                                                               
128500     IF SLAG-IDDC-REF NOT = '11'                                          
128600       MOVE SLAG-IDDC-REF       TO W-IDDC                                 
128700       PERFORM IMS-GU-K711                                                
128800       COMPUTE SLAG-KVBEART = SLAG-KVBEART + REF-KVBEART                  
128900       PERFORM IMS-REPL-K711                                              
129000     END-IF                                                               
129100     MOVE SPACE            TO REFILL-KDFAKTYP                             
129200     MOVE REF-KDFRAKT      TO REFILL-KDFRAKT                              
129300     MOVE 0                TO REFILL-KDTPOTYP                             
129400     MOVE 0                TO REFILL-TITPO                                
129500     MOVE REF-IDARTNR      TO REFILL-IDARTNR                              
129600     MOVE 0                TO REFILL-REKSIFFR                             
129700     MOVE REF-KVBEART      TO REFILL-KVBEART                              
129800***OM KDREFORS ÄR U(AUTORETUR OMR 98) TILLÅT KVANTBRYTNING*****           
129900     IF REF-KDREFORS = 'U'                                                
130000       MOVE 1              TO REFILL-KDKVBRYT                             
130100     ELSE                                                                 
130200       MOVE 0              TO REFILL-KDKVBRYT                             
130300     END-IF                                                               
130400     MOVE WS-ADART-X       TO REFILL-BERADREF                             
130500                                                                          
130600     IF REF-KDREFORS = 'T' OR 'U'                                         
130700       MOVE 'COMPLETE' TO REFILL-BEKUNDRF                                 
130800     ELSE                                                                 
130900       MOVE SPACE      TO REFILL-BEKUNDRF                                 
131000     END-IF                                                               
131100     .                                                                    
131200     EJECT                                                                
131300                                                                          
131400                                                                          
131500 S40-KUNDNUMMER-REFILL SECTION.                                           
131600                                                                          
131700     IF REF-KDREFTYP = 'T'                                                
131800        MOVE REF-IDKUNDNR TO WS-IDKUNDNR                                  
131900     ELSE                                                                 
132000*        EVALUATE TRUE                                                    
132100*                                                                         
132200*          WHEN SDC-NL        ERSATT AV IF DCS-KDDCSTYR-KUND = 1          
132300*                                                                         
132400       IF NOT ( DIST35-REFILL         OR                                  
132500                DIST35-REFILL-INOM-JP OR                                  
132600                DIST35-CA-USA-REFILL  OR                                  
132700                DIST35-USA-CA-REFILL )                                    
132800             MOVE 99                  TO WS-IDKUNDNR                      
132900       ELSE                                                               
133000         IF DCS-KDDCSTYR-KUND = 1                                         
133100                                                                          
133200            EVALUATE TRUE                                                 
133300              WHEN REF-ADLAGOMR-SDC = 10 OR 40                            
133400                MOVE 10 TO WS-IDKUNDNR                                    
133500              WHEN REF-ADLAGOMR-SDC = 12                                  
133600                MOVE 12 TO WS-IDKUNDNR                                    
133700              WHEN REF-ADLAGOMR-SDC = 11 OR 15                            
133800                MOVE 11 TO WS-IDKUNDNR                                    
133900              WHEN REF-ADLAGOMR-SDC = 20 OR 21 OR 22 OR                   
134000                23 OR 27 OR 70 OR 30 OR 31 OR 32 OR 34 OR 36 OR           
134100                37                                                        
134200                MOVE 20 TO WS-IDKUNDNR                                    
134300              WHEN REF-ADLAGOMR-SDC = 25 OR 35                            
134400                MOVE 30 TO WS-IDKUNDNR                                    
134500              WHEN OTHER                                                  
134600                MOVE 99 TO WS-IDKUNDNR                                    
134700            END-EVALUATE                                                  
134800         END-IF                                                           
134900                                                                          
135000                                                                          
135100*                                                                         
135200*        WHEN SDC-GB          ERSATT AV IF DCS-KDDCSTYR-KUND = 2          
135300*        WHEN SDC-GB-3A       ERSATT AV IF DCS-KDDCSTYR-KUND = 2          
135400*                                                                         
135500                                                                          
135600         IF DCS-KDDCSTYR-KUND = 2                                         
135700                                                                          
135800                                                                          
135900            EVALUATE TRUE                                                 
136000              WHEN REF-ADLAGOMR-SDC = 10                                  
136100                MOVE 10 TO WS-IDKUNDNR                                    
136200              WHEN REF-ADLAGOMR-SDC = 11                                  
136300                MOVE 11 TO WS-IDKUNDNR                                    
136400              WHEN REF-ADLAGOMR-SDC = 12                                  
136500                MOVE 12 TO WS-IDKUNDNR                                    
136600              WHEN REF-ADLAGOMR-SDC = 20                                  
136700                MOVE 20 TO WS-IDKUNDNR                                    
136800              WHEN REF-ADLAGOMR-SDC = 21                                  
136900                MOVE 21 TO WS-IDKUNDNR                                    
137000              WHEN REF-ADLAGOMR-SDC = 19 OR 30 OR 31 OR 33                
137100                MOVE 30 TO WS-IDKUNDNR                                    
137200              WHEN REF-ADLAGOMR-SDC = 18 OR 32 OR                         
137300                                      33 OR 34 OR 35 OR 46                
137400                MOVE 30 TO WS-IDKUNDNR                                    
137500              WHEN REF-ADLAGOMR-SDC = 40                                  
137600                MOVE 40 TO WS-IDKUNDNR                                    
137700              WHEN REF-ADLAGOMR-SDC = 41                                  
137800                MOVE 41 TO WS-IDKUNDNR                                    
137900              WHEN OTHER                                                  
138000                MOVE 99 TO WS-IDKUNDNR                                    
138100            END-EVALUATE                                                  
138200         END-IF                                                           
138300                                                                          
138400                                                                          
138500*                                                                         
138600*        WHEN SDC-ES          ERSATT AV IF DCS-KDDCSTYR-KUND = 3          
138700*                                                                         
138800                                                                          
138900         IF DCS-KDDCSTYR-KUND = 3                                         
139000                                                                          
139100            EVALUATE TRUE                                                 
139200              WHEN REF-ADLAGOMR-SDC = 10 OR 11                            
139300                MOVE 10 TO WS-IDKUNDNR                                    
139400              WHEN REF-ADLAGOMR-SDC = 15                                  
139500                MOVE 15 TO WS-IDKUNDNR                                    
139600              WHEN REF-ADLAGOMR-SDC = 20 OR 60                            
139700                MOVE 20 TO WS-IDKUNDNR                                    
139800              WHEN REF-ADLAGOMR-SDC = 21 OR 31 OR 51 OR 61                
139900                MOVE 32 TO WS-IDKUNDNR                                    
140000              WHEN REF-ADLAGOMR-SDC = 30                                  
140100                MOVE 20 TO WS-IDKUNDNR                                    
140200              WHEN REF-ADLAGOMR-SDC = 40 OR 41                            
140300                MOVE 40 TO WS-IDKUNDNR                                    
140400              WHEN REF-ADLAGOMR-SDC = 50                                  
140500                MOVE 30 TO WS-IDKUNDNR                                    
140600              WHEN OTHER                                                  
140700                MOVE 99 TO WS-IDKUNDNR                                    
140800            END-EVALUATE                                                  
140900                                                                          
141000         END-IF                                                           
141100                                                                          
141200                                                                          
141300*                                                                         
141400*        WHEN SDC-IT          ERSATT AV IF DCS-KDDCSTYR-KUND = 4          
141500*                                                                         
141600                                                                          
141700         IF DCS-KDDCSTYR-KUND = 4                                         
141800                                                                          
141900            EVALUATE TRUE                                                 
142000              WHEN REF-ADLAGOMR-SDC = 10 OR 12                            
142100                MOVE 10 TO WS-IDKUNDNR                                    
142200              WHEN REF-ADLAGOMR-SDC = 11 OR 13                            
142300                MOVE 11 TO WS-IDKUNDNR                                    
142400              WHEN REF-ADLAGOMR-SDC = 15                                  
142500                MOVE 14 TO WS-IDKUNDNR                                    
142600              WHEN REF-ADLAGOMR-SDC = 20 OR 25                            
142700                MOVE 20 TO WS-IDKUNDNR                                    
142800              WHEN REF-ADLAGOMR-SDC = 30 OR 40                            
142900                MOVE 30 TO WS-IDKUNDNR                                    
143000              WHEN REF-ADLAGOMR-SDC = 90                                  
143100                MOVE 99 TO WS-IDKUNDNR                                    
143200              WHEN OTHER                                                  
143300                MOVE 99 TO WS-IDKUNDNR                                    
143400            END-EVALUATE                                                  
143500         END-IF                                                           
143600                                                                          
143700*                                                                         
143800*        WHEN SDC-AT          ERSATT AV IF DCS-KDDCSTYR-KUND = 5          
143900*                                                                         
144000                                                                          
144100         IF DCS-KDDCSTYR-KUND = 5                                         
144200                                                                          
144300            EVALUATE TRUE                                                 
144400              WHEN REF-ADLAGOMR-SDC = 10                                  
144500                MOVE 10 TO WS-IDKUNDNR                                    
144600              WHEN REF-ADLAGOMR-SDC = 21 OR 22 OR 23 OR 24                
144700                MOVE 20 TO WS-IDKUNDNR                                    
144800              WHEN REF-ADLAGOMR-SDC = 30 OR 32                            
144900                MOVE 30 TO WS-IDKUNDNR                                    
145000              WHEN REF-ADLAGOMR-SDC = 40 OR 41 OR 42                      
145100                MOVE 40 TO WS-IDKUNDNR                                    
145200              WHEN OTHER                                                  
145300                MOVE 99 TO WS-IDKUNDNR                                    
145400            END-EVALUATE                                                  
145500         END-IF                                                           
145600                                                                          
145700                                                                          
145800*                                                                         
145900*        WHEN NDC-US-RU       ERSATT AV IF DCS-KDDCSTYR-KUND = 6          
146000*                                                                         
146100                                                                          
146200         IF DCS-KDDCSTYR-KUND = 6                                         
146300            IF NOT DIST35-REFILL                                          
146700              IF DIST35-CA-USA-REFILL                                     
146800                IF DIST35-CA-NDC41-REFILL                                 
146900                  MOVE 511 TO WS-IDKUNDNR                                 
147000                END-IF                                                    
147100                IF DIST35-CA-NDC43-REFILL                                 
147200                  MOVE 513 TO WS-IDKUNDNR                                 
147300                END-IF                                                    
147400                IF DIST35-CA-NDC44-REFILL                                 
147500                  MOVE 514 TO WS-IDKUNDNR                                 
147600                END-IF                                                    
147700                IF DIST35-CA-NDC45-REFILL                                 
147800                  MOVE 515 TO WS-IDKUNDNR                                 
147900                END-IF                                                    
148000                IF DIST35-CA-NDC46-REFILL                                 
148100                  MOVE 516 TO WS-IDKUNDNR                                 
148200                END-IF                                                    
148201                IF DIST35-CA-NDC47-REFILL                                 
148202                  MOVE 517 TO WS-IDKUNDNR                                 
148203                END-IF                                                    
148300              ELSE                                                        
148400                MOVE 99       TO WS-IDKUNDNR                              
148500              END-IF                                                      
148600            ELSE                                                          
148700             IF ART-IDFKNGRP = 8616                                       
148800               MOVE 50       TO WS-IDKUNDNR                               
148900             ELSE                                                         
149000              IF REF-ADLAGOMR-CDC = 10                                    
149100                EVALUATE TRUE                                             
149200                   WHEN REF-ADLAGOMR-SDC = 10 OR 11 OR 12 OR 13           
149300                      MOVE 10 TO WS-IDKUNDNR                              
149400                   WHEN REF-ADLAGOMR-SDC = 15 OR 16                       
149500                      MOVE 15 TO WS-IDKUNDNR                              
149600                   WHEN OTHER                                             
149700                      MOVE 19 TO WS-IDKUNDNR                              
149800                END-EVALUATE                                              
149900              ELSE                                                        
150000                EVALUATE TRUE                                             
150100                   WHEN REF-ADLAGOMR-SDC = 20 OR 21 OR 22 OR 25 OR        
150200                                           30 OR 31 OR 35 OR 40 OR        
150300                                           73                             
150400                      MOVE 20 TO WS-IDKUNDNR                              
150500                   WHEN OTHER                                             
150600                      MOVE 19 TO WS-IDKUNDNR                              
150700                END-EVALUATE                                              
150800              END-IF                                                      
150900             END-IF                                                       
151000            END-IF                                                        
151100         END-IF                                                           
151200                                                                          
151300                                                                          
151400*                                                                         
151500*        WHEN NDC-US-AT       ERSATT AV IF DCS-KDDCSTYR-KUND = 7          
151600*                                                                         
151700                                                                          
153800                                                                          
153900*                                                                         
154000*        WHEN NDC-CA          ERSATT AV IF DCS-KDDCSTYR-KUND = 9          
154100*                                                                         
154200                                                                          
154300         IF DCS-KDDCSTYR-KUND = 9                                         
154400          IF DIST35-USA-CA-REFILL                                         
154500            MOVE REF-IDARTNR TO W-IDARTNR                                 
154600            PERFORM IMS-GU-K711                                           
154700            MOVE SLAG-IDDC-REF TO WS-IDKUNDNR                             
154800          ELSE                                                            
154900            IF ART-IDFKNGRP = 8616                                        
155000              MOVE 50        TO WS-IDKUNDNR                               
155100            ELSE                                                          
155200              IF REF-ADLAGOMR-CDC = 10                                    
155300                EVALUATE TRUE                                             
155400                   WHEN REF-ADLAGOMR-SDC = 10                             
155500                      MOVE 10 TO WS-IDKUNDNR                              
155600                   WHEN REF-ADLAGOMR-SDC = 15                             
155700                      MOVE 15 TO WS-IDKUNDNR                              
155800                   WHEN OTHER                                             
155900                      MOVE 19 TO WS-IDKUNDNR                              
156000                END-EVALUATE                                              
156100              ELSE                                                        
156200                EVALUATE TRUE                                             
156300                   WHEN REF-ADLAGOMR-SDC = 20 OR 21 OR 30 OR              
156400                                            40                            
156500                      MOVE 20 TO WS-IDKUNDNR                              
156600                   WHEN OTHER                                             
156700                      MOVE 19 TO WS-IDKUNDNR                              
156800                END-EVALUATE                                              
156900              END-IF                                                      
157000            END-IF                                                        
157100          END-IF                                                          
157200         END-IF                                                           
157300                                                                          
157400*                                                                         
157500*        WHEN NDC-JP          ERSATT AV IF DCS-KDDCSTYR-KUND = 10         
157600*                                                                         
157700                                                                          
157800         IF DCS-KDDCSTYR-KUND = 10                                        
157900                                                                          
158000            EVALUATE TRUE                                                 
158100              WHEN REF-ADLAGOMR-SDC = 10 OR 11                            
158200                MOVE 10 TO WS-IDKUNDNR                                    
158300              WHEN REF-ADLAGOMR-SDC = 15 OR 16 OR 17                      
158400                MOVE 15 TO WS-IDKUNDNR                                    
158500              WHEN REF-ADLAGOMR-SDC = 20                                  
158600                MOVE 20 TO WS-IDKUNDNR                                    
158700              WHEN REF-ADLAGOMR-SDC = 25                                  
158800                MOVE 25 TO WS-IDKUNDNR                                    
158900              WHEN REF-ADLAGOMR-SDC = 30                                  
159000                MOVE 30 TO WS-IDKUNDNR                                    
159100              WHEN REF-ADLAGOMR-SDC = 31                                  
159200                MOVE 50 TO WS-IDKUNDNR                                    
159300              WHEN REF-ADLAGOMR-SDC = 40 OR 41                            
159400                MOVE 40 TO WS-IDKUNDNR                                    
159500              WHEN REF-ADLAGOMR-SDC = 50                                  
159600                MOVE 50 TO WS-IDKUNDNR                                    
159700              WHEN REF-ADLAGOMR-SDC = 51 OR 52 OR 53                      
159800                MOVE 50 TO WS-IDKUNDNR                                    
159900              WHEN OTHER                                                  
160000                MOVE 99 TO WS-IDKUNDNR                                    
160100            END-EVALUATE                                                  
160200         END-IF                                                           
160300                                                                          
160400                                                                          
160500*                                                                         
160600*        WHEN NDC-AU          ERSATT AV IF DCS-KDDCSTYR-KUND = 11         
160700*                                                                         
160800                                                                          
160900         IF DCS-KDDCSTYR-KUND = 11                                        
161000                                                                          
161100            EVALUATE TRUE                                                 
161200              WHEN REF-ADLAGOMR-SDC = 10 OR 15                            
161300                MOVE 101 TO WS-IDKUNDNR                                   
161400              WHEN REF-ADLAGOMR-SDC = 11 OR 20 OR 21                      
161500                MOVE 201 TO WS-IDKUNDNR                                   
161600              WHEN REF-ADLAGOMR-SDC = 30 OR 31 OR 49                      
161700                MOVE 301 TO WS-IDKUNDNR                                   
161800              WHEN OTHER                                                  
161900                MOVE 991 TO WS-IDKUNDNR                                   
162000            END-EVALUATE                                                  
162100                                                                          
162200         END-IF                                                           
162300                                                                          
162400***???*****WHEN LDC-SE-1A                                                 
162500*        WHEN LDC                                                         
162600*         IF LDC-GB-2A                                                    
162700*                           ERSATT AV IF DCS-KDDCSTYR-KUND = 12           
162800*                                                                         
162900                                                                          
163000         IF DCS-KDDCSTYR-KUND = 12                                        
163100            EVALUATE TRUE                                                 
163200              WHEN REF-ADLAGOMR-SDC = 10 OR 11                            
163300                MOVE 10 TO WS-IDKUNDNR                                    
163400              WHEN REF-ADLAGOMR-SDC = 20 OR 21                            
163500                MOVE 10 TO WS-IDKUNDNR                                    
163600              WHEN REF-ADLAGOMR-SDC = 30 OR 31                            
163700                MOVE 10    TO WS-IDKUNDNR                                 
163800              WHEN REF-ADLAGOMR-SDC = 40 OR 42                            
163900                MOVE 40    TO WS-IDKUNDNR                                 
164000              WHEN OTHER                                                  
164100                MOVE 99    TO WS-IDKUNDNR                                 
164200            END-EVALUATE                                                  
164300         END-IF                                                           
164400*         ELSE                                                            
164500*  ÖVRIGA LDC'ER              ERSATT AV IF DCS-KDDCSTYR-KUND = 13         
164600*  DVS EJ 2A                                                              
164700*                                                                         
164800                                                                          
164900         IF DCS-KDDCSTYR-KUND = 13                                        
165000            EVALUATE TRUE                                                 
165100              WHEN REF-ADLAGOMR-SDC = 9                                   
165200                MOVE    9 TO WS-IDKUNDNR                                  
165300              WHEN REF-ADLAGOMR-SDC = 10 OR 11                            
165400                MOVE 10 TO WS-IDKUNDNR                                    
165500              WHEN REF-ADLAGOMR-SDC = 20 OR 21 OR 22 OR 30 OR 31          
165600                MOVE 20 TO WS-IDKUNDNR                                    
165700              WHEN REF-ADLAGOMR-SDC = 40 OR 42                            
165800                MOVE 40    TO WS-IDKUNDNR                                 
165900              WHEN OTHER                                                  
166000                MOVE 99    TO WS-IDKUNDNR                                 
166100            END-EVALUATE                                                  
166200         END-IF                                                           
166300*                                                                         
166400*        WHEN NDC-CN                                                      
166500**** KOPIA PÅ REGELVERK 10 ENL PATRIK L                                   
166600                                                                          
166700         IF DCS-KDDCSTYR-KUND = 14                                        
166800                                                                          
166900            IF NOT DIST35-REFILL                                          
167200                  MOVE 99 TO WS-IDKUNDNR                                  
167300            ELSE                                                          
167400              EVALUATE TRUE                                               
167500                WHEN REF-ADLAGOMR-SDC = 10 OR 11 OR 12                    
167600                  MOVE 10 TO WS-IDKUNDNR                                  
167700                WHEN REF-ADLAGOMR-SDC = 15 OR 16 OR 17                    
167800                  MOVE 15 TO WS-IDKUNDNR                                  
167900                WHEN REF-ADLAGOMR-SDC = 20 OR 21 OR 22 OR 23 OR 24        
168000                  MOVE 20 TO WS-IDKUNDNR                                  
168100                WHEN REF-ADLAGOMR-SDC = 25                                
168200                  MOVE 25 TO WS-IDKUNDNR                                  
168300                WHEN REF-ADLAGOMR-SDC = 30 OR 31                          
168400                  MOVE 30 TO WS-IDKUNDNR                                  
168500                WHEN REF-ADLAGOMR-SDC = 40                                
168600                  MOVE 40 TO WS-IDKUNDNR                                  
168700                WHEN REF-ADLAGOMR-SDC = 41 OR 42                          
168800                  MOVE 50 TO WS-IDKUNDNR                                  
168900                WHEN REF-ADLAGOMR-SDC = 50                                
169000                  MOVE 50 TO WS-IDKUNDNR                                  
169100                WHEN REF-ADLAGOMR-SDC = 51 OR 52 OR 53                    
169200                  MOVE 50 TO WS-IDKUNDNR                                  
169300                WHEN REF-ADLAGOMR-SDC = 65                                
169400                  MOVE 60 TO WS-IDKUNDNR                                  
169500                WHEN OTHER                                                
169600                  MOVE 99 TO WS-IDKUNDNR                                  
169700              END-EVALUATE                                                
169800            END-IF                                                        
169900         END-IF                                                           
170000*                                                                         
170100*  LDC-CN                                                                 
170200**** KOPIA PÅ REGELVERK 13 ENL PATRIK L                                   
170300*                                                                         
170400                                                                          
171700*                                                                         
171800                                                                          
171900         IF DCS-KDDCSTYR-KUND = 16                                        
172000            MOVE 99          TO WS-IDKUNDNR                               
172100         END-IF                                                           
172200       END-IF                                                             
172300                                                                          
172400                                                                          
172500       IF NOT ( DIST35-REFILL         OR                                  
172700                DIST35-CA-USA-REFILL  OR                                  
172800                DIST35-USA-CA-REFILL )                                    
173300         MOVE REF-IDARTNR TO W-IDARTNR                                    
173400         IF W-IDDC = '11'                                                 
173500           PERFORM IMS-GHU-K611                                           
173600           IF SEGMENT-FINNS                                               
173700             MOVE CLAG-IDDC-REF    TO WS-CUSTID-A                         
173800             MOVE WS-IDKUNDNR      TO WS-CUSTID-B                         
173900             MOVE ZERO             TO WS-IDKUNDNR                         
174000             MOVE WS-NEW-CUSTID    TO WS-IDKUNDNR                         
177400           END-IF                                                         
177500         ELSE                                                             
177600           PERFORM IMS-GU-K711                                            
177700           IF SEGMENT-FINNS                                               
177800             MOVE SLAG-IDDC-REF    TO WS-CUSTID-A                         
177900             MOVE WS-IDKUNDNR      TO WS-CUSTID-B                         
178000             MOVE ZERO             TO WS-IDKUNDNR                         
178100             MOVE WS-NEW-CUSTID    TO WS-IDKUNDNR                         
181500           END-IF                                                         
181600         END-IF                                                           
181700       END-IF                                                             
181800     END-IF                                                               
181900     .                                                                    
182000     EJECT                                                                
182100                                                                          
182200                                                                          
182300 S41-FRAKTKOD-REFILL SECTION.                                             
182400                                                                          
182800     MOVE REF-KDFRAKT      TO WS-KDFRAKT                                  
183000     .                                                                    
183100     EJECT                                                                
183200                                                                          
183300                                                                          
183400 S42-FLYTTA-TILL-REFILLPOST SECTION.                                      
183500                                                                          
183600                                                                          
183700     INITIALIZE               REFILL-W412RX3                              
183800                                                                          
183900     MOVE 'RX3'            TO REFILL-IDTYP                                
184000     MOVE REF-IDDISTR      TO REFILL-IDDISTR                              
184100     MOVE WS-IDKUNDNR      TO REFILL-IDKUNDNR                             
184200     MOVE WS-IDORDNR       TO REFILL-IDORDNR7                             
184300     MOVE 1                TO REFILL-KDCLAGER                             
184400     MOVE WS-KDORDKL       TO REFILL-KDORDKL                              
184500     MOVE SPACE            TO REFILL-KDFAKTYP                             
184600     MOVE WS-KDFRAKT       TO REFILL-KDFRAKT                              
184700     MOVE 0                TO REFILL-KDTPOTYP                             
184800     MOVE SPACE            TO REFILL-BEKUNDRF                             
184900     MOVE 0                TO REFILL-TITPO                                
185000     MOVE REF-IDARTNR      TO REFILL-IDARTNR                              
185100     MOVE 0                TO REFILL-REKSIFFR                             
185200     MOVE REF-KVBEART      TO REFILL-KVBEART                              
185300     MOVE 0                TO REFILL-KDKVBRYT                             
185400     MOVE WS-ADART-X       TO REFILL-BERADREF                             
185500     MOVE WS-ADLAGOMR-CD-X TO REFILL-ADLAGOMR-CD                          
185600     MOVE WS-ADGANG-CD-X   TO REFILL-ADGANG-CD                            
185700     MOVE WS-ADPLATS-CD-X  TO REFILL-ADPLATS-CD                           
185800     .                                                                    
185900     EJECT                                                                
186000                                                                          
186100                                                                          
186200 S43-ORDERKLASS-REFILL SECTION.                                           
186300                                                                          
186400     IF REF-KDFRAKT = 43                                                  
186500**DETTA FÖR ATT NÄR MAN SLÄPPER FÖRDRÖJD REFILLORDER PGA CROSS DOC        
186600       MOVE 1              TO WS-KDORDKL                                  
186700       MOVE REF-IDARTNR    TO W-IDARTNR                                   
186800       MOVE REF-IDDC       TO W-IDDC                                      
186900     ELSE                                                                 
187000       IF REF-KDREFTYP = 'T'                                              
187100          MOVE 3           TO WS-KDORDKL                                  
187200       ELSE                                                               
187300          MOVE 4           TO WS-KDORDKL                                  
187400       END-IF                                                             
187500     END-IF                                                               
187600     .                                                                    
187700     EJECT                                                                
187800                                                                          
187900                                                                          
188000 S44-SNABBORDER-SDC SECTION.                                              
188100                                                                          
188200     MOVE 31            TO WS-KDFRAKT                                     
188300                                                                          
188400     MOVE 1                TO WS-KDORDKL                                  
188500     IF REF-KDREFTYP = 'T'                                                
188600       MOVE REF-IDKUNDNR  TO WS-IDKUNDNR                                  
188700     ELSE                                                                 
188800       MOVE 2              TO WS-IDKUNDNR                                 
188900     END-IF                                                               
189000     .                                                                    
189100     EJECT                                                                
189200                                                                          
189300                                                                          
189400 S50-KUNDNUMMER-FLYG SECTION.                                             
189500                                                                          
189600     MOVE REF-IDARTNR TO W-IDARTNR                                        
189700     IF CDC-SE                                                            
189800       PERFORM IMS-GU-WDK611                                              
189900       MOVE CLAG-IDDC-REF TO W-IDDC-B616                                  
190000       PERFORM IMS-GU-WDB616                                              
190100       IF SEGMENT-FINNS                                                   
190200         MOVE B6-REF-IDKUNDNR-SORD TO WS-IDKUNDNR                         
190300       END-IF                                                             
190400     ELSE                                                                 
190500       PERFORM IMS-GU-K711                                                
190600       IF SEGMENT-FINNS                                                   
190700         MOVE SLAG-IDDC-REF TO W-IDDC-B616                                
190800         IF SLAG-IDDC-REF = '11'                                          
190900           MOVE DCS-IDKUNDNR-SORD TO WS-IDKUNDNR                          
191000         ELSE                                                             
191100           PERFORM IMS-GU-WDB616                                          
191200           IF SEGMENT-FINNS                                               
191300             MOVE B6-REF-IDKUNDNR-SORD TO WS-IDKUNDNR                     
191400           END-IF                                                         
191500         END-IF                                                           
191600       END-IF                                                             
191700     END-IF                                                               
191800                                                                          
191900     .                                                                    
192000     EJECT                                                                
192100                                                                          
192200                                                                          
192300 S51-FRAKTKOD-FLYG SECTION.                                               
192400                                                                          
192500     MOVE REF-KDFRAKT        TO WS-KDFRAKT                                
192600     .                                                                    
192700     EJECT                                                                
192800                                                                          
192900                                                                          
193000 S52-FLYTTA-TILL-FLYGPOST SECTION.                                        
193100                                                                          
193200     INITIALIZE               REFILL-W412RX3                              
193300                                                                          
193400     MOVE 'RX3'            TO REFILL-IDTYP                                
193500     MOVE REF-IDDISTR      TO REFILL-IDDISTR                              
193600     MOVE WS-IDKUNDNR      TO REFILL-IDKUNDNR                             
193700     MOVE WS-IDORDNR-FLYG  TO REFILL-IDORDNR7                             
193800     MOVE 1                TO REFILL-KDCLAGER                             
193900     MOVE 1                TO REFILL-KDORDKL                              
194000     MOVE SPACE            TO REFILL-KDFAKTYP                             
194100     MOVE WS-KDFRAKT       TO REFILL-KDFRAKT                              
194200     MOVE 0                TO REFILL-KDTPOTYP                             
194300     MOVE SPACE            TO REFILL-BEKUNDRF                             
194400     MOVE 0                TO REFILL-TITPO                                
194500     MOVE REF-IDARTNR      TO REFILL-IDARTNR                              
194600     MOVE 0                TO REFILL-REKSIFFR                             
194700     MOVE REF-KVBEART      TO REFILL-KVBEART                              
194800     MOVE 0                TO REFILL-KDKVBRYT                             
194900     MOVE WS-ADART-X       TO REFILL-BERADREF                             
195000     .                                                                    
195100     EJECT                                                                
195200                                                                          
195300                                                                          
195400 S60-FLYTTA-TILL-RENSPOST SECTION.                                        
195500                                                                          
195600     MOVE REF-IDDC          TO RENS-IDDC                                  
195700     MOVE REF-IDPERSON-BUY  TO RENS-IDPERSON-BUY                          
195800     MOVE REF-KDREFTYP      TO RENS-KDREFTYP                              
195900     MOVE REF-IDARTNR       TO RENS-IDARTNR                               
196000     MOVE REF-IDDISTR       TO RENS-IDDISTR                               
196100     MOVE NEJ               TO RENS-FLREFNYO                              
196200     .                                                                    
196300     EJECT                                                                
196400                                                                          
196500                                                                          
196600 S77-SKAPA-LAGERPLATS SECTION.                                            
196700                                                                          
196800     MOVE REF-ADLAGOMR-SDC TO WS-ADLAGOMR-X                               
196900     MOVE REF-ADGANG-SDC TO WS-ADGANG-X                                   
197000     MOVE REF-ADPLATS-SDC TO WS-ADPLATS-X                                 
197100     MOVE REF-ADLAGOMR-CD TO WS-ADLAGOMR-CD-X                             
197200     MOVE REF-ADGANG-CD    TO WS-ADGANG-CD-X                              
197300     MOVE REF-ADPLATS-CD TO WS-ADPLATS-CD-X                               
197400     .                                                                    
197500     EJECT                                                                
197600                                                                          
197700                                                                          
197800* --- IMS SEKTIONER ---                                                   
197900                                                                          
198000                                                                          
198100 IMS-GU-E301 SECTION.                                                     
198200                                                                          
198300     STRING 'WDE301  (WDE301KY=>' W-WDE301KY-MIN-X                        
198400                    '&WDE301KY<=' W-WDE301KY-MAX-X ')'                    
198500          DELIMITED BY SIZE INTO SSA1                                     
198600     MOVE '  GE' TO GODK-STATUSKODER                                      
198700     CALL CBLTDLI USING GU WDE3-PCB DLI-IO-AREA-E301 SSA1                 
198800     MOVE WDE3-STATUS-CODE TO STATUS-WS                                   
198900     PERFORM IMS-STATUSKONTROLL                                           
199000     .                                                                    
199100     EJECT                                                                
199200                                                                          
199300                                                                          
199400 IMS-GN-E301 SECTION.                                                     
199500                                                                          
199600     STRING 'WDE301  (WDE301KY=>' W-WDE301KY-MIN-X                        
199700                    '&WDE301KY<=' W-WDE301KY-MAX-X ')'                    
199800          DELIMITED BY SIZE INTO SSA1                                     
199900     MOVE '  GE' TO GODK-STATUSKODER                                      
200000     CALL CBLTDLI USING GN WDE3-PCB DLI-IO-AREA-E301 SSA1                 
200100     MOVE WDE3-STATUS-CODE TO STATUS-WS                                   
200200     PERFORM IMS-STATUSKONTROLL                                           
200300     .                                                                    
200400     EJECT                                                                
200500                                                                          
200600                                                                          
200700                                                                          
200800 IMS-GET-WDQ2C SECTION.                                                   
200900     STRING 'WLORQL01(WDQ2C1KY =' W-WDQ2C1KY-X ')'                        
201000          DELIMITED BY SIZE INTO SSA1                                     
201100     MOVE '  GE' TO GODK-STATUSKODER                                      
201200     CALL CBLTDLI USING GU WDQ2C-PCB DLI-IO-WDQ2C1 SSA1                   
201300     MOVE WDQ2C-STATUS-CODE TO STATUS-WS                                  
201400     PERFORM IMS-STATUSKONTROLL                                           
201500     .                                                                    
201600     SKIP3                                                                
201700 IMS-GU-K601 SECTION.                                                     
201800     STRING 'WDK601  (IDARTNR  =' W-IDARTNR-X ')'                         
201900          DELIMITED BY SIZE INTO SSA1                                     
202000     MOVE '  ' TO GODK-STATUSKODER                                        
202100     CALL CBLTDLI USING GU WDK6-PCB DLI-IO-AREA-K601 SSA1                 
202200     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
202300     PERFORM IMS-STATUSKONTROLL                                           
202400     .                                                                    
202500     SKIP3                                                                
202600 IMS-GN-K611 SECTION.                                                     
202700                                                                          
202800     STRING 'WDK611  (KDSEGKEY =' W-KDSEGKEY-X ')'                        
202900          DELIMITED BY SIZE INTO SSA1                                     
203000     MOVE '  ' TO GODK-STATUSKODER                                        
203100     CALL CBLTDLI USING GNP WDK6-PCB DLI-IO-AREA-K611 SSA1                
203200     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
203300     PERFORM IMS-STATUSKONTROLL                                           
203400     .                                                                    
203500     EJECT                                                                
203600 IMS-GHU-K611 SECTION.                                                    
203700     STRING 'WDK601  (IDARTNR  =' W-IDARTNR-X ')'                         
203800          DELIMITED BY SIZE INTO SSA1                                     
203900     MOVE 'WDK611  '       TO SSA2                                        
204000     MOVE '  ' TO GODK-STATUSKODER                                        
204100     CALL CBLTDLI USING GHU WDK6-PCB DLI-IO-AREA-K611 SSA1 SSA2           
204200     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
204300     PERFORM IMS-STATUSKONTROLL                                           
204400     .                                                                    
204500     SKIP3                                                                
204600 IMS-GU-WDK611 SECTION.                                                   
204700     STRING 'WDK601  (IDARTNR  =' W-IDARTNR-X ')'                         
204800          DELIMITED BY SIZE INTO SSA1                                     
204900     MOVE 'WDK611  '       TO SSA2                                        
205000     MOVE '  ' TO GODK-STATUSKODER                                        
205100     CALL CBLTDLI USING GU WDK6-PCB DLI-IO-AREA-K611 SSA1 SSA2            
205200     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
205300     PERFORM IMS-STATUSKONTROLL                                           
205400     .                                                                    
205500     SKIP3                                                                
205600 IMS-REPL-K611 SECTION.                                                   
205700     SKIP2                                                                
205800     MOVE '  ' TO GODK-STATUSKODER                                        
205900     CALL CBLTDLI USING REPL WDK6-PCB DLI-IO-AREA-K611                    
206000     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
206100     PERFORM IMS-STATUSKONTROLL                                           
206200     .                                                                    
206300     EJECT                                                                
206400 IMS-GU-K711 SECTION.                                                     
206500     STRING 'WDK701  (IDARTNR  =' W-IDARTNR-X ')'                         
206600          DELIMITED BY SIZE INTO SSA1                                     
206700     STRING 'WDK711  (IDDC     =' W-IDDC-X ')'                            
206800          DELIMITED BY SIZE INTO SSA2                                     
206900     MOVE '  ' TO GODK-STATUSKODER                                        
207000     CALL CBLTDLI USING GHU WDK7-PCB DLI-IO-AREA-K711 SSA1 SSA2           
207100     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
207200     PERFORM IMS-STATUSKONTROLL                                           
207300     .                                                                    
207400     SKIP3                                                                
207500                                                                          
207600 IMS-REPL-K711 SECTION.                                                   
207700     SKIP2                                                                
207800     MOVE '  ' TO GODK-STATUSKODER                                        
207900     CALL CBLTDLI USING REPL WDK7-PCB DLI-IO-AREA-K711                    
208000     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
208100     PERFORM IMS-STATUSKONTROLL                                           
208200     .                                                                    
208300     EJECT                                                                
208400                                                                          
208500 IMS-GU-WDB601    SECTION.                                                
208600     STRING 'WDB601  (IDDC     =' W-IDDC-B6-X ')'                         
208700          DELIMITED BY SIZE INTO SSA1                                     
208800     MOVE '  ' TO GODK-STATUSKODER                                        
208900     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-AREA-B601 SSA1                 
209000     MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
209100     PERFORM IMS-STATUSKONTROLL                                           
209200     .                                                                    
209300     EJECT                                                                
209400                                                                          
209500 IMS-GU-WDB616    SECTION.                                                
209600     STRING 'WDB601  (IDDC     =' W-IDDC-B6-X ')'                         
209700          DELIMITED BY SIZE INTO SSA1                                     
209800     STRING 'WDB616  (IDDCREF  =' W-IDDC-B616-X ')'                       
209900          DELIMITED BY SIZE INTO SSA2                                     
210000     MOVE '  GE' TO GODK-STATUSKODER                                      
210100     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-AREA-B616 SSA1 SSA2            
210200     MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
210300     PERFORM IMS-STATUSKONTROLL                                           
210400     .                                                                    
210500     EJECT                                                                
210600                                                                          
210700 IMS-STATUSKONTROLL SECTION.                                              
210800     SKIP2                                                                
210900     SET STATUS-IX TO 1                                                   
211000     SEARCH GODK-STATUS                                                   
211100       AT END                                                             
211200         MOVE 'FEL STATUSKOD' TO FELTEXT-STR                              
211300         DISPLAY FELTEXT                                                  
211400         CALL FELLOG                                                      
211500       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
211600         CONTINUE                                                         
211700     END-SEARCH                                                           
211800     .                                                                    
211900     EJECT                                                                
212000*    -COPY WY2000P1                                                       
