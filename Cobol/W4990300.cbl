000102 ID DIVISION.                                                             
000202     SKIP2                                                                
000302 PROGRAM-ID.     W4990300.                                                
000402*AUTHOR.         LASSI OLGRENER.                                          
000502*DATE-WRITTEN.   92/11/25.                                                
000602                                                                          
000702*    REMARKS.                                                             
000802*                                                                         
000902*    FUNKTION:                                                            
001002*        STÄMMER AV Q4 > Q3 > Q2                                          
001102*        I FÖLJANDE FALL SKRIVS W49903-FIL:                               
001202*        OM  ANTAL RADER PÅ Q4 <> KVRADER-Q3                              
001302*        OM Q4 SAKNAR Q3 OCH PÅ Q2 RENSAD ARBTAB. ELLER TIRFS > 0         
001402*                                                                         
001502*        PROGRAMMET LÄSER      WDQ4 MED SB                                
001602*                              WDQ3 MED DL/I                              
001702*                              WDQ2 MED DL/I                              
001802                                                                          
001902 ENVIRONMENT DIVISION.                                                    
002002                                                                          
002102 INPUT-OUTPUT SECTION.                                                    
002202                                                                          
002302 FILE-CONTROL.                                                            
002402                                                                          
002502*          --- Q3-KVRAD <> Q4-RADER/RENSAD Q212/TRPKAT B+C                
002602     SELECT W49903                     ASSIGN TO W49903D1.                
002702     SELECT W49903F                    ASSIGN TO W49903D2.                
002802     EJECT                                                                
002902 DATA DIVISION.                                                           
003002                                                                          
003102 FILE SECTION.                                                            
003202                                                                          
003302 FD  W49903                                                               
003402     RECORDING       F                                                    
003502     BLOCK CONTAINS  0.                                                   
003602     SKIP2                                                                
003702 01  UT-POST            PIC  X(80).                                       
003802     EJECT                                                                
003902 FD  W49903F                                                              
004002     RECORDING       F                                                    
004102     BLOCK CONTAINS  0.                                                   
004202     SKIP2                                                                
004302 01  UT2-POST            PIC  X(40).                                      
004402     EJECT                                                                
004502 WORKING-STORAGE SECTION.                                                 
004602     SKIP2                                                                
004702 77  IDPGM                       PIC X(8)    VALUE 'W4990300'.            
004802 77  CURRENT-SECTION             PIC X(16)   VALUE SPACE.                 
004902 77  FELTEXT                     PIC X(64)   VALUE SPACE.                 
005002 77  Q4-ANT-RADER                PIC S9(5)   COMP-3.                      
005102 77  Q3-ANT-RADER                PIC S9(5)   COMP-3.                      
005202 77  RKOD-ABEND-MED-DUMP         PIC S9(4)  VALUE +1000 COMP SYNC.        
005302 77  IX                          PIC S9(3)   COMP-3.                      
005402     SKIP2                                                                
005502 01  UT-AREA.                                                             
005602   03 UT-IDORDER                 PIC X(7).                                
005702   03 FILLER                     PIC X(1).                                
005802   03 UT-IDDISTR                 PIC X(5).                                
005902   03 FILLER                     PIC X(1).                                
006002   03 UT-IDKUNDNR                PIC X(7).                                
006102   03 FILLER                     PIC X(1).                                
006202   03 UT-IDORDNR                 PIC X(7).                                
006302   03 FILLER                     PIC X(1).                                
006402   03 UT-IX                      PIC 9(3).                                
006500   03 UT-IDPRC                   PIC X(4).                                
006600   03 FILLER                     PIC X(1).                                
006700   03 UT-KVRADER                 PIC 9(5).                                
006800   03 FILLER                     PIC X(2).                                
006900   03 UT-TIREGDAT                PIC 9(6).                                
007000   03 FILLER                     PIC X(2).                                
007100   03 UT-IDSYSTEM                PIC X(4).                                
007200                                                                          
007300 01  UT2-AREA.                                                            
007400   03 UT2-IDTRANS                PIC X(8) VALUE 'W4T298X '.               
007500   03 UT2-IDSYSTEM               PIC X(5) VALUE 'FIXA1'.                  
007600   03 UT2-IDDISTR                PIC 9(4) VALUE ZERO.                     
007700   03 UT2-IDKUNDNR               PIC 9(6) VALUE ZERO.                     
007800   03 UT2-IDKUNDRF               PIC X(10) VALUE SPACE.                   
007900   03 UT2-IDORDER                PIC 9(7) VALUE ZERO.                     
008000                                                                          
008100 01  FILLER                    PIC X(16)   VALUE 'SEND-CONTROL'.          
008200                                                                          
008300 01  WS-IDCOM           PIC S9(9)  VALUE ZERO  COMP-3.                    
008400                                                                          
008500 01  FILLER                      PIC X(16)   VALUE 'SEND-AREA'.           
008600 01  SEND-AREA.                                                           
008700     03 SEND-KVLL                PIC S9(4) COMP SYNC VALUE 44.            
008800     03 SEND-KDZ                 PIC X(2) VALUE LOW-VALUE.                
008900     03 SEND-IDTRANS             PIC X(8) VALUE 'W4T298X '.               
009000     03 SEND-IDSYSTEM            PIC X(5) VALUE 'FIXA1'.                  
009100     03 SEND-ID.                                                          
009200         05 SEND-IDDISTR         PIC 9(4) VALUE ZERO.                     
009300         05 SEND-IDKUNDNR        PIC 9(6) VALUE ZERO.                     
009402         05 SEND-IDKUNDRF        PIC X(10) VALUE SPACE.                   
009502         05 SEND-IDORDER         PIC 9(7) VALUE ZERO.                     
009602                                                                          
009702 01 LAST-SEND-ID.                                                         
009802     03 LAST-SEND-IDDISTR        PIC 9(4) VALUE ZERO.                     
009902     03 LAST-SEND-IDKUNDNR       PIC 9(6) VALUE ZERO.                     
010002     03 LAST-SEND-IDKUNDRF       PIC X(10) VALUE SPACE.                   
010102     03 LAST-SEND-IDORDER        PIC 9(7) VALUE ZERO.                     
010202                                                                          
010300                                                                          
010400 01  FILLER.                                                              
010500     03  PRC-TABELL             OCCURS 20                                 
010600                                 INDEXED BY PRC-IX.                       
010700         05  TAB-IDPRC           PIC X(4).                                
010800         05  TAB-KVRADER         PIC 9(5).                                
010900                                                                          
011000 01  DYNAMISKA-SUBPROGRAM.                                                
011100*                                                                         
011200     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
011300     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
011402     03  ABEND                   PIC X(8)    VALUE 'ABEND   '.            
011502                                                                          
011602 01  SPAR-IDORDER                PIC S9(7)   VALUE ZERO COMP-3.           
011702 01  SPAR-TIREGDAT               PIC S9(7)   VALUE ZERO COMP-3.           
011802 01  SPAR-TIREGTID               PIC S9(7)   VALUE ZERO COMP-3.           
011902 01  SPAR-IDGMTREF.                                                       
012002    03  SPAR-IDDISTR             PIC S9(5)   VALUE ZERO COMP-3.           
012102    03  SPAR-IDKUNDNR            PIC S9(7)   VALUE ZERO COMP-3.           
012202    03  SPAR-IDKUNDRF            PIC X(10)   VALUE SPACE.                 
012302 01  SPAR-IDKUNDRF-RO            PIC X(10)   VALUE SPACE.                 
012400     EJECT                                                                
012500*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
012600*                                                                         
012700 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
012800                                                                          
012900 01  NYCKLAR-TILL-DLI.                                                    
013000     03  W-WDQ3KEY-MIN-X.                                                 
013100         05  W-IDORDER-MIN       PIC S9(7)   VALUE ZERO COMP-3.           
013200         05  W-IDDC-MIN          PIC  X(2)   VALUE ZERO.                  
013300         05  FILLER              PIC X(06)   VALUE LOW-VALUE.             
013400     03  W-WDQ3KEY-MAX-X.                                                 
013500         05  W-IDORDER-MAX       PIC S9(7)   VALUE ZERO COMP-3.           
013600         05  W-IDDC-MAX          PIC  X(2)   VALUE ZERO.                  
013700         05  FILLER              PIC X(06)   VALUE HIGH-VALUE.            
013800     SKIP2                                                                
013900     03  W-IDORDER-X.                                                     
014000         05  W-IDORDER           PIC S9(7)   VALUE ZERO COMP-3.           
014100     03  W-IDDC-X.                                                        
014200         05  W-IDDC              PIC  X(2)   VALUE ZERO.                  
014300   03    W-KDODELST-X.                                                    
014400     05    W-KDODELST            PIC  X(1)   VALUE 'R'.                   
014500*    --- STATUS-KOD FRÅN IMS                                              
014600 01  STATUS-Q4                   PIC XX.                                  
014700     88  Q4-SLUT                             VALUE 'GB'.                  
014800                                                                          
014900 01  STATUS-Q3                   PIC XX.                                  
015000     88  Q3-FINNS                            VALUE '  '.                  
015100     88  Q3-SAKNAS                           VALUE 'GE'.                  
015200                                                                          
015300 01  STATUS-WS                   PIC XX.                                  
015400     88  SEGMENT-FINNS                       VALUE '  '.                  
015500     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
015600     88  BASEN-SLUT                          VALUE 'GB'.                  
015700     SKIP2                                                                
015800 01  GODK-STATUSKODER.                                                    
015900     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
016000     SKIP3                                                                
016100 01  SSA1                        PIC X(96).                               
016200 01  SSA2                        PIC X(32).                               
016300     EJECT                                                                
016400*    --- IMS FUNKTIONSKODER                                               
016500*01  -COPY W0003                                                          
016600     EJECT                                                                
016700*    ---  DLI INPUT-OUTPUT AREA                                           
016800 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA'.         
016900                                                                          
017000 01  DLI-IO-AREA.                                                         
017100                                                                          
017200     03  Q4-IO-AREA   -COPY WDQ401                                        
017300     EJECT                                                                
017400     03  Q3-IO-AREA   -COPY WDQ301                                        
017500     EJECT                                                                
017600     03  Q201-IO-AREA -COPY WDQ201                                        
017700     EJECT                                                                
017800     03  Q212-IO-AREA -COPY WDQ212                                        
017900     EJECT                                                                
018000     03  Q221-IO-AREA -COPY WDQ221                                        
018100     EJECT                                                                
018200 LINKAGE SECTION.                                                         
018300                                                                          
018400*01  -COPY W0009   -PRE MSG-                                              
018500                                                                          
018600*01  -COPY W0009   -PRE 4298-                                             
018700     SKIP2                                                                
018800*01  -COPY W0008  -PRE Q4-                                                
018900     05  FILLER                  PIC X.                                   
019000*01  -COPY W0008  -PRE Q3-                                                
019100     05  FILLER                  PIC X.                                   
019200     EJECT                                                                
019300*01  -COPY W0008  -PRE Q2-                                                
019400     05  FILLER                  PIC X.                                   
019500     EJECT                                                                
019600 PROCEDURE DIVISION  USING MSG-PCB 4298-PCB Q4-PCB Q3-PCB Q2-PCB.         
019700     ENTRY 'DLITCBL' USING MSG-PCB 4298-PCB Q4-PCB Q3-PCB Q2-PCB.         
019800                                                                          
019900     OPEN OUTPUT W49903                                                   
020000                 W49903F                                                  
020100                                                                          
020200     PERFORM IMS-GN-Q4                                                    
020300     PERFORM UNTIL Q4-SLUT                                                
020400                                                                          
020500        PERFORM 20 TIMES                                                  
020600        SET PRC-IX TO 1                                                   
020700           MOVE SPACE TO TAB-IDPRC(PRC-IX)                                
020800           MOVE ZERO  TO TAB-KVRADER(PRC-IX)                              
020900           SET PRC-IX UP BY 1                                             
021000        END-PERFORM                                                       
021100                                                                          
021200        MOVE ORAD-IDGMTREF    TO SPAR-IDGMTREF                            
021300        MOVE ORAD-IDKUNDRF-RO TO SPAR-IDKUNDRF-RO                         
021402        MOVE ORAD-TIREGTID    TO SPAR-TIREGTID                            
021500        MOVE ORAD-IDORDER  TO W-IDORDER-MIN                               
021600                              W-IDORDER-MAX                               
021700                              W-IDORDER                                   
021800                              SPAR-IDORDER                                
021900        MOVE ORAD-TIREGDAT TO SPAR-TIREGDAT                               
022000        MOVE ORAD-IDDC     TO W-IDDC-MIN                                  
022100                              W-IDDC-MAX                                  
022200                              W-IDDC                                      
022300        MOVE +0            TO Q4-ANT-RADER                                
022400        PERFORM UNTIL Q4-SLUT                                             
022500                   OR (ORAD-IDORDER NOT = W-IDORDER)                      
022600                   OR (ORAD-IDDC NOT = W-IDDC)                            
022700           ADD +1          TO Q4-ANT-RADER                                
022800           PERFORM IMS-GN-Q4                                              
022900        END-PERFORM                                                       
023000                                                                          
023100        PERFORM IMS-GU-Q3                                                 
023200          SET PRC-IX TO 1                                                 
023300          PERFORM UNTIL SEGMENT-SAKNAS OR BASEN-SLUT                      
023400             MOVE ODEL-IDPRC TO TAB-IDPRC(PRC-IX)                         
023500             MOVE ODEL-KVRADER TO TAB-KVRADER(PRC-IX)                     
023600             SET PRC-IX UP BY 1                                           
023700             PERFORM IMS-GN-Q3                                            
023800          END-PERFORM                                                     
023900                                                                          
024000          PERFORM A-SKRIV-FILER                                           
024100     END-PERFORM                                                          
024200                                                                          
024300     CLOSE W49903                                                         
024400           W49903F                                                        
024500     MOVE ZERO TO RETURN-CODE                                             
024600     GOBACK                                                               
024700     .                                                                    
024800     EJECT                                                                
024900 A-SKRIV-FILER SECTION.                                                   
025000                                                                          
025100     MOVE SPAR-IDORDER     TO UT-IDORDER                                  
025200                              UT2-IDORDER                                 
025300                              SEND-IDORDER                                
025400     MOVE SPAR-IDDISTR     TO UT-IDDISTR                                  
025500                              UT2-IDDISTR                                 
025600                              SEND-IDDISTR                                
025700     MOVE SPAR-IDKUNDNR    TO UT-IDKUNDNR                                 
025800                              UT2-IDKUNDNR                                
025902                              SEND-IDKUNDNR                               
026002     MOVE SPAR-IDKUNDRF    TO UT-IDORDNR                                  
026102                              UT2-IDKUNDRF                                
026202                              SEND-IDKUNDRF                               
026302     MOVE SPAR-TIREGDAT    TO UT-TIREGDAT                                 
026402                                                                          
026502     IF Q3-SAKNAS                                                         
026602        PERFORM IMS-GU-Q201                                               
026702        IF SEGMENT-FINNS                                                  
026800          IF OHUV-IDSYSTEM NOT = '4231'                                   
026900            MOVE OHUV-IDSYSTEM TO UT-IDSYSTEM                             
027000            PERFORM IMS-GU-Q212                                           
027100            IF SEGMENT-FINNS                                              
027200               IF ARB-TIRFS > +0                                          
027300                 PERFORM IMS-GNP-Q221                                     
027400                 PERFORM UNTIL SEGMENT-SAKNAS                             
027500                    SET PRC-IX TO 1                                       
027600                    SEARCH PRC-TABELL                                     
027700                    AT END                                                
027800                      MOVE LOR-ADLAGOMR    TO UT-IX                       
027900                      MOVE LOR-IDPRC       TO UT-IDPRC                    
028000                      MOVE LOR-KVRADER     TO UT-KVRADER                  
028100                                                                          
028200                      WRITE UT-POST        FROM UT-AREA                   
028300                      WRITE UT2-POST       FROM UT2-AREA                  
028402*                                                                         
028502* ADDING DISPLAYS TO CHECK IN SPOOL MAYBE HELPS TO FIND BUG?              
028503          DISPLAY 'ORDER ID>> ' OHUV-IDDISTR '/' OHUV-IDKUNDNR '/'        
028504                             OHUV-IDORDNR7                                
028600          DISPLAY 'Q2 DATA >> '                                           
028702                  'IDORDER= ' OHUV-IDORDER ' / '                          
028802                  'REGDAT= ' OHUV-TIREGDAT ' / '                          
028902                  'REGTID= ' OHUV-TIREGTID ' / '                          
029002                  'IDPRC=' LOR-IDPRC                                      
030202          DISPLAY 'Q4 DATA >> '                                           
030300                  'RF-RO= ' SPAR-IDKUNDRF-RO ' / '                        
030402                  'REGDAT= ' SPAR-TIREGDAT ' / '                          
030502                  'REGTID= ' SPAR-TIREGTID                                
030503          DISPLAY '--------------------------------------'                
030600                                                                          
030700                      IF SEND-ID NOT = LAST-SEND-ID                       
030800                         PERFORM IMS-PURG-4298-MSG-4298                   
030900                         MOVE SEND-ID TO LAST-SEND-ID                     
031000                      END-IF                                              
031100                                                                          
031200                    WHEN TAB-IDPRC (PRC-IX) = LOR-IDPRC     AND           
031300                         TAB-KVRADER (PRC-IX) = LOR-KVRADER               
031400                      CONTINUE                                            
031500                    END-SEARCH                                            
031600                    PERFORM IMS-GNP-Q221                                  
031700                 END-PERFORM                                              
031800               END-IF                                                     
031900            END-IF                                                        
032000          END-IF                                                          
032100        ELSE                                                              
032200          MOVE 'BORT'       TO UT-IDSYSTEM                                
032300          MOVE 'BORT'       TO UT-IDPRC                                   
032400          MOVE 0            TO UT-KVRADER                                 
032500          WRITE UT-POST FROM UT-AREA                                      
032600        END-IF                                                            
032700     END-IF                                                               
032800     .                                                                    
032900     EJECT                                                                
033000 IMS-PURG-4298-MSG-4298  SECTION.                                         
033100     MOVE SPACE TO GODK-STATUSKODER                                       
033200     CALL  CBLTDLI  USING PURG 4298-PCB SEND-AREA                         
033302     MOVE 4298-STATUS-CODE TO STATUS-WS                                   
033402     PERFORM IMS-STATUSKONTROLL                                           
033502     .                                                                    
033602 IMS-GN-Q4   SECTION.                                                     
033702                                                                          
033802     CALL CBLTDLI USING GN Q4-PCB Q4-IO-AREA                              
033902     MOVE Q4-STATUS-CODE TO STATUS-WS STATUS-Q4                           
034002     MOVE '  GAGKGB' TO GODK-STATUSKODER                                  
034102     PERFORM IMS-STATUSKONTROLL                                           
034200     .                                                                    
034300     SKIP2                                                                
034400 IMS-GU-Q3   SECTION.                                                     
034500                                                                          
034600     STRING 'WDQ301  (WDQ301KY>=' W-WDQ3KEY-MIN-X                         
034700                    '&WDQ301KY<=' W-WDQ3KEY-MAX-X                         
034800                    '&KDODELST =' W-KDODELST-X ')'                        
034900          DELIMITED BY SIZE     INTO SSA1                                 
035000     CALL CBLTDLI USING GU Q3-PCB Q3-IO-AREA SSA1                         
035100     MOVE Q3-STATUS-CODE TO STATUS-WS STATUS-Q3                           
035200     MOVE '  GE' TO GODK-STATUSKODER                                      
035300     PERFORM IMS-STATUSKONTROLL                                           
035400     .                                                                    
035500     SKIP2                                                                
035600 IMS-GN-Q3   SECTION.                                                     
035700                                                                          
035800     STRING 'WDQ301  (WDQ301KY>=' W-WDQ3KEY-MIN-X                         
035900                    '&WDQ301KY<=' W-WDQ3KEY-MAX-X                         
036000                    '&KDODELST =' W-KDODELST-X ')'                        
036100          DELIMITED BY SIZE     INTO SSA1                                 
036200     CALL CBLTDLI USING GN Q3-PCB Q3-IO-AREA SSA1                         
036300     MOVE Q3-STATUS-CODE TO STATUS-WS                                     
036400     MOVE '  GEGB' TO GODK-STATUSKODER                                    
036500     PERFORM IMS-STATUSKONTROLL                                           
036600     .                                                                    
036700     EJECT                                                                
036800 IMS-GU-Q201 SECTION.                                                     
036900                                                                          
037000     STRING 'WDQ201  (IDORDER  =' W-IDORDER-X ')'                         
037100          DELIMITED BY SIZE     INTO SSA1                                 
037200     CALL CBLTDLI USING GU Q2-PCB Q201-IO-AREA SSA1                       
037300     MOVE Q2-STATUS-CODE TO STATUS-WS                                     
037400     MOVE '  GE' TO GODK-STATUSKODER                                      
037500     PERFORM IMS-STATUSKONTROLL                                           
037600     .                                                                    
037700     SKIP2                                                                
037800 IMS-GU-Q212 SECTION.                                                     
037900                                                                          
038000     STRING 'WDQ201  (IDORDER  =' W-IDORDER-X ')'                         
038100          DELIMITED BY SIZE     INTO SSA1                                 
038200     STRING 'WDQ212  (IDDC     =' W-IDDC-X ')'                            
038300          DELIMITED BY SIZE     INTO SSA2                                 
038400     CALL CBLTDLI USING GU Q2-PCB Q212-IO-AREA SSA1 SSA2                  
038500     MOVE Q2-STATUS-CODE TO STATUS-WS                                     
038600     MOVE '  GE' TO GODK-STATUSKODER                                      
038700     PERFORM IMS-STATUSKONTROLL                                           
038800     .                                                                    
038901     SKIP2                                                                
039002 IMS-GNP-Q221 SECTION.                                                    
039102                                                                          
039202     MOVE 'WDQ221'  TO SSA1                                               
039302     CALL CBLTDLI USING GNP Q2-PCB Q221-IO-AREA SSA1                      
039402     MOVE Q2-STATUS-CODE TO STATUS-WS                                     
039502     MOVE '  GE' TO GODK-STATUSKODER                                      
039602     PERFORM IMS-STATUSKONTROLL                                           
039702     .                                                                    
039802     SKIP2                                                                
039902 IMS-STATUSKONTROLL SECTION.                                              
040002     SKIP2                                                                
040102     SET STATUS-IX TO 1                                                   
040202     SEARCH GODK-STATUS                                                   
040302       AT END                                                             
040402         CALL FELLOG                                                      
040502       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
040602         CONTINUE                                                         
040700     END-SEARCH                                                           
040800     .                                                                    
