000100 ID DIVISION.                                                             
000201 PROGRAM-ID.     W5613300.                                                
000301 AUTHOR.         DADHICH PRERNA.                                          
000401 DATE-WRITTEN.   18/05/04.                                                
000501 DATE-COMPILED.                                                           
000601                                                                          
000701*    FUNCTION:                                                            
000801*        IT CREATES A .CSV FILE FOR RECEIVED GOODS FOR US AND CN          
000901*                                                                         
001001*                                                                         
001101*    ABENDCODES:                                                          
001201*        U0016 -  . . .                                                   
001301*        U1000 -  . . . .                                                 
001401*                                                                         
001501                                                                          
001601     SKIP3                                                                
001701 ENVIRONMENT DIVISION.                                                    
001801     SKIP2                                                                
001901 INPUT-OUTPUT SECTION.                                                    
002001                                                                          
002101 FILE-CONTROL.                                                            
002201     SKIP2                                                                
002301*          --- INPUT FILE FROM W56131 EXTRACT OF WDL6                     
002401     SELECT W56131               ASSIGN TO W56133D1.                      
002501     SKIP2                                                                
002601*          --- INPUT FILE - WDK7                                          
002701     SELECT W01184A              ASSIGN TO W56133D2.                      
002801     SKIP2                                                                
002901*          --- PREVIOUS VERSION - WDK7                                    
003001     SELECT W01184B              ASSIGN TO W56133D3.                      
003101     SKIP2                                                                
003201*          --- OUTPUT FILE WITH GOODS RECEIVED                            
003301     SELECT W56134               ASSIGN TO W56133D4.                      
003401     SKIP2                                                                
003501     EJECT                                                                
003601 DATA DIVISION.                                                           
003701     SKIP2                                                                
003801 FILE SECTION.                                                            
003901     SKIP3                                                                
004001 FD  W56131                                                               
004101     RECORDING       F                                                    
004201     BLOCK CONTAINS  0.                                                   
004301                                                                          
004401*01  -COPY W56131      -L.                                                
004501     SKIP3                                                                
004601                                                                          
004701 FD  W01184A                                                              
004801     RECORDING F                                                          
004901     BLOCK CONTAINS 0.                                                    
005001*01  -COPY W01184      -L                                                 
005101                                                                          
005201 FD  W01184B                                                              
005301     RECORDING F                                                          
005401     BLOCK CONTAINS 0.                                                    
005501*01  -COPY W01184      -L                                                 
005601                                                                          
005701 FD  W56134                                                               
005801     RECORDING       V                                                    
005901     BLOCK CONTAINS  0.                                                   
006001 01  UT-RECORD                   PIC X(217).                              
006101     EJECT                                                                
006201 WORKING-STORAGE SECTION.                                                 
006301                                                                          
006401 77  IDPGM                       PIC X(8)    VALUE 'W5613300'.            
006501 77  YES                         PIC X       VALUE 'J'.                   
006601 77  NOO                         PIC X       VALUE 'N'.                   
006701 77  WS-PREV-IDLANDX2            PIC X(02)   VALUE SPACES.                
006801 77  IX                          PIC S9(4)   BINARY.                      
006901 77  IX-MARKUP                   PIC S9(4)   BINARY.                      
007000 77  WS-INDX                     PIC S9(4)   VALUE +0   COMP SYNC.        
007100 77  WS-IDARTNR                  PIC S9(9)   VALUE ZERO COMP-3.           
007200 77  WS-KVLS                     PIC S9(7)   VALUE +0   COMP-3.           
007300 77  WS-KVEFRS                   PIC S9(7)   VALUE +0   COMP-3.           
007400 77  WS-BEG-STOCK-AVIL           PIC S9(7)   VALUE +0   COMP-3.           
007500 77  WS-END-STOCK-AVIL           PIC S9(7)   VALUE +0   COMP-3.           
007600 77  WS-BEG-INVENTORY            PIC S9(8)V99                             
007700                                             VALUE +0   COMP-3.           
007800 77  WS-END-INVENTORY            PIC S9(8)V99                             
007900                                             VALUE +0   COMP-3.           
008000 77  WS-PRARTBEL-PR              PIC S9(8)V9(5) VALUE +0  COMP-3.         
008100 77  WS-PRARTBES-PR              PIC S9(8)V9(5) VALUE +0  COMP-3.         
008300 77  WS-EXTD-COST                PIC S9(7)V9(2) VALUE +0  COMP-3.         
008500 77  WS-MARKUP                   PIC 9V9(2)  VALUE ZERO.                  
008800 77  WS-EXTD-LANDED-COST         PIC S9(10)V9(2)                          
008900                                             VALUE +0   COMP-3.           
009000                                                                          
009100 77  W-DAPRLIST-9KOMPL           PIC 9(8).                                
009110 77  DAGENS-AAMMDD               PIC 9(06).                               
009200                                                                          
009900 77  WS-KDVALISO                 PIC X(3)       VALUE SPACE.              
010001 77  W-REVALUTA                  PIC S9(5)      VALUE +0 COMP-3.          
010100 77  W-RETULF                    PIC S9(3)V9(4) VALUE +0.                 
010200 77  W-PRKURS                    PIC S9(6)V9(5) VALUE ZERO.               
010300 77  W-PRKURS2                   PIC S9(6)V9(5) VALUE ZERO.               
010400 77  W-PRARTBEL                  PIC S9(8)V9(3) VALUE ZERO.               
010500 77  W-FIRST-PRKURS              PIC S9(6)V9(5) VALUE ZERO.               
010600 77  W-VALUTA-FOUND              PIC X       VALUE 'J'.                   
010610 77  W-DATE-AAMM                 PIC 9(4)    VALUE ZERO.                  
010620 77  WS-KDVALISO-HUV             PIC X(3)    VALUE 'SEK'.                 
010700                                                                          
010800 77  W56131-EOF-SW               PIC X       VALUE 'N'.                   
010900     88  END-OF-W56131                       VALUE 'J'.                   
011000                                                                          
011100 77  W01184A-EOF-SW               PIC X      VALUE 'N'.                   
011200     88  END-OF-W01184A                      VALUE 'J'.                   
011300                                                                          
011400 77  W01184B-EOF-SW               PIC X      VALUE 'N'.                   
011500     88  END-OF-W01184B                      VALUE 'J'.                   
011600                                                                          
011700 77  WS-PROD-SW                  PIC X       VALUE 'N'.                   
011800     88 PRODKOD-MISSING                      VALUE 'N'.                   
011900     88 PRODKOD-FOUND                        VALUE 'J'.                   
012000                                                                          
012100 77  WS-PRICE-SW                 PIC X       VALUE 'N'.                   
012200     88 PRICE-FOUND                          VALUE 'J'.                   
012300     EJECT                                                                
012400 01  TODAYS-DATE                 PIC 9(6)    VALUE ZERO.                  
012500 01  FILLER REDEFINES TODAYS-DATE.                                        
012600     03  TODAYS-DATE-YEAR        PIC 9(2).                                
012700     03  TODAYS-DATE-MONTH       PIC 9(2).                                
012800     03  TODAYS-DATE-DAY         PIC 9(2).                                
012900     EJECT                                                                
013000 01  LINE-NO                     PIC S9(4)   BINARY VALUE ZERO.           
013100                                                                          
013200*01    -COPY WWMARKUP                                                     
013301*01    -COPY WWDCKONS                                                     
013400 01  GENERAL-SUBPROGRAMS.                                                 
013500*                                                                         
013600     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
013700     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
013800     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
013900     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
014000     03  W335PRIS                PIC X(8)    VALUE 'W335PRIS'.            
014010     03  W510CURR                PIC X(8)    VALUE 'W510CURR'.            
014100     SKIP2                                                                
014200*    --- PARAMETERS FOR SUBPROGRAM ABEND                                  
014300                                                                          
014400 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
014500 77  RKOD-ABEND-NO-DUMP          PIC S9(4)   COMP VALUE +16.              
014600 77  RKOD-ABEND-WITH-DUMP        PIC S9(4)   COMP VALUE +1000.            
014700     SKIP2                                                                
014800 01  ERROR-TEXT.                                                          
014900     03  FILLER                  PIC X(10)   VALUE 'ERROR-TEXT'.          
015000     03  ERROR-TEXT-STR          PIC X(72)   VALUE SPACE.                 
015100     EJECT                                                                
015200*01  -COPY W335PRIS                                                       
015300     EJECT                                                                
015310*01  -COPY W510CURR                                                       
015320     EJECT                                                                
015400*    --- PARAMETRAR TILL POSTSUM                                          
015500*                                                                         
015600*01  -COPY W0005   -PRE  POSTSUM-                                         
015700     EJECT                                                                
015800 01  IN1-AREA-START              PIC X(24)   VALUE                        
015900                                 'IN1-AREA-START  '.                      
016000     SKIP2                                                                
016100                                                                          
016200*01  AREA -COPY W56131     -PRE IN1-                                      
016300     EJECT                                                                
016400 01  IN2-AREA-START              PIC X(24)   VALUE                        
016500                                 'IN2-AREA-START  '.                      
016600     SKIP2                                                                
016700                                                                          
016800*01  AREA -COPY W01184     -PRE IN2-                                      
016900     EJECT                                                                
017000 01  IN3-AREA-START              PIC X(24)   VALUE                        
017100                                 'IN3-AREA-START  '.                      
017200     SKIP2                                                                
017300                                                                          
017400*01  AREA -COPY W01184     -PRE IN3-                                      
017500     EJECT                                                                
017600 01  UT-AREA-START               PIC X(24)   VALUE                        
017700                                 'UT-AREA-START  '.                       
017800     SKIP2                                                                
017900                                                                          
018000 01  UT-AREA                     PIC X(213)  VALUE SPACE.                 
018100                                                                          
018200 01  UT-CONTROL-REC1.                                                     
018300     03  FILLER                  PIC X(15)   VALUE                        
018400                                 ' ¤DAPW56133-001'.                       
018500     EJECT                                                                
018600 01  UT-CONTROL-REC2.                                                     
018700     03  FILLER                  PIC X(5)    VALUE ' ¤DAP'.               
018800     03  UT-CTL-IDLANDX2         PIC X(2)    VALUE SPACE.                 
018900     EJECT                                                                
019000 01  UT-HEADER.                                                           
019100     03  UT-H-IDDC               PIC X(02)   VALUE 'DC'.                  
019200     03  FILLER                  PIC X(01)   VALUE ';'.                   
019300     03  UT-H-KDPRODSL           PIC X(08)   VALUE 'PROD GRP'.            
019400     03  FILLER                  PIC X(01)   VALUE ';'.                   
019500     03  UT-H-KDPSLLOC           PIC X(03)   VALUE 'LPC'.                 
019600     03  FILLER                  PIC X(01)   VALUE ';'.                   
019700     03  UT-H-IDARTNR            PIC X(07)   VALUE 'PART NO'.             
019800     03  FILLER                  PIC X(01)   VALUE ';'.                   
019900     03  UT-H-BEG-STOCK          PIC X(15)   VALUE                        
020000                                             'BEG STOCK AVAIL'.           
020100     03  FILLER                  PIC X(01)   VALUE ';'.                   
020200     03  UT-H-END-STOCK          PIC X(15)   VALUE                        
020300                                             'END STOCK AVAIL'.           
020400     03  FILLER                  PIC X(01)   VALUE ';'.                   
020500     03  UT-H-BEG-PRAVCOST       PIC X(12)   VALUE                        
020600                                             'BEG AVG COST'.              
020700     03  FILLER                  PIC X(01)   VALUE ';'.                   
020800     03  UT-H-END-PRAVCOST       PIC X(12)   VALUE                        
020900                                             'END AVG COST'.              
021000     03  FILLER                  PIC X(01)   VALUE ';'.                   
021100     03  UT-H-BEG-INVENTORY      PIC X(19)   VALUE                        
021200                                           'BEG INVENTORY VALUE'.         
021300     03  FILLER                  PIC X(01)   VALUE ';'.                   
021400     03  UT-H-END-INVENTORY      PIC X(19)   VALUE                        
021500                                           'END INVENTORY VALUE'.         
021600     03  FILLER                  PIC X(01)   VALUE ';'.                   
021700     03  UT-H-IDKUNDRF           PIC X(05)   VALUE 'ORDER'.               
021800     03  FILLER                  PIC X(01)   VALUE ';'.                   
021900     03  UT-H-IDLOPNRM           PIC X(07)   VALUE 'INVOICE'.             
022000     03  FILLER                  PIC X(01)   VALUE ';'.                   
022100     03  UT-H-KVAVIS             PIC X(03)   VALUE 'QTY'.                 
022200     03  FILLER                  PIC X(01)   VALUE ';'.                   
022300     03  UT-H-PRARTBEL-PR        PIC X(09)   VALUE 'UNIT COST'.           
022400     03  FILLER                  PIC X(01)   VALUE ';'.                   
022500     03  UT-H-EXTD-COST          PIC X(13)   VALUE                        
022600                                                'EXTENDED COST'.          
022700     03  FILLER                  PIC X(01)   VALUE ';'.                   
022800     03  UT-H-LANDING-COST       PIC X(11)   VALUE                        
022900                                             'LANDED COST'.               
023000     03  FILLER                  PIC X(01)   VALUE ';'.                   
023100     03  UT-H-EXT-LANDING-COST   PIC X(20)   VALUE                        
023200                                          'EXTENDED LANDED COST'.         
023300     03  FILLER                  PIC X(01)   VALUE ';'.                   
023400     03  UT-H-KDVALISO           PIC X(08)   VALUE 'CURRENCY'.            
023500     03  FILLER                  PIC X(01)   VALUE ';'.                   
023600     03  UT-H-SOURCE             PIC X(06)   VALUE 'SOURCE'.              
023700     03  FILLER                  PIC X(01)   VALUE ';'.                   
023800                                                                          
023900 01  UT-RAD.                                                              
024000     03  UT-IDDC                 PIC X(02)   VALUE SPACE.                 
024100     03  FILLER                  PIC X(01)   VALUE ';'.                   
024200     03  UT-KDPRODSL             PIC Z9      VALUE ZERO.                  
024300     03  FILLER                  PIC X(01)   VALUE ';'.                   
024400     03  UT-KDPSLLOC             PIC Z9      VALUE ZERO.                  
024500     03  FILLER                  PIC X(01)   VALUE ';'.                   
024600     03  UT-IDARTNR              PIC Z(8)9   VALUE ZERO.                  
024700     03  FILLER                  PIC X(01)   VALUE ';'.                   
024800     03  UT-BEG-STOCK-AVIL       PIC Z(6)9   VALUE ZERO.                  
024900     03  FILLER                  PIC X(01)   VALUE ';'.                   
025000     03  UT-END-STOCK-AVIL       PIC Z(6)9   VALUE ZERO.                  
025100     03  FILLER                  PIC X(01)   VALUE ';'.                   
025200     03  UT-BEG-PRAVCOST         PIC Z(6)9.9(2)                           
025300                                             VALUE ZERO.                  
025400     03  FILLER                  PIC X(01)   VALUE ';'.                   
025500     03  UT-END-PRAVCOST         PIC Z(6)9.9(2)                           
025600                                             VALUE ZERO.                  
025700     03  FILLER                  PIC X(01)   VALUE ';'.                   
025800     03  UT-BEG-INVENTORY        PIC Z(8)9.9(2)                           
025900                                             VALUE ZERO.                  
026000     03  FILLER                  PIC X(01)   VALUE ';'.                   
026100     03  UT-END-INVENTORY        PIC Z(8)9.9(2)                           
026200                                             VALUE ZERO.                  
026300     03  FILLER                  PIC X(01)   VALUE ';'.                   
026400     03  UT-IDKUNDRF             PIC X(10)   VALUE SPACE.                 
026500     03  FILLER                  PIC X(01)   VALUE ';'.                   
026600     03  UT-IDLOPNRM             PIC Z(8)9   VALUE ZERO.                  
026700     03  FILLER                  PIC X(01)   VALUE ';'.                   
026800     03  UT-KVAVIS               PIC Z(6)9   VALUE ZERO.                  
026900     03  FILLER                  PIC X(01)   VALUE ';'.                   
027000     03  UT-PRARTBEL-PR          PIC Z(7)9.9(2) VALUE ZERO.               
027100     03  FILLER                  PIC X(01)   VALUE ';'.                   
027200     03  UT-EXTD-COST            PIC Z(9)9.9(2) VALUE ZERO.               
027300     03  FILLER                  PIC X(01)   VALUE ';'.                   
027400     03  UT-LANDING-COST         PIC Z(9)9.9(2) VALUE ZERO.               
027500     03  FILLER                  PIC X(01)   VALUE ';'.                   
027600     03  UT-EXT-LANDING-COST     PIC Z(9)9.9(2) VALUE ZERO.               
027700     03  FILLER                  PIC X(01)   VALUE ';'.                   
027800     03  UT-KDVALISO             PIC X(03)   VALUE SPACE.                 
027900     03  FILLER                  PIC X(01)   VALUE ';'.                   
028000     03  UT-SOURCE               PIC X(05)   VALUE SPACE.                 
028100     03  FILLER                  PIC X(01)   VALUE ';'.                   
028200                                                                          
028300*    --- AREAS FOR IMS-SECTIONS                                           
028400*                                                                         
028500     EJECT                                                                
028600 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
028700     SKIP3                                                                
028800 01  KEYS-FOR-DLI.                                                        
028900     03  W-IDDC-X.                                                        
029000         05  W-IDDC              PIC X(2)    VALUE SPACE.                 
029100     03  W-IDLEVNR-X.                                                     
029200         05  W-IDLEVNR           PIC X(5)    VALUE SPACE.                 
029300     03  W-IDLAND-X.                                                      
029400         05  W-IDLAND            PIC X(2)    VALUE SPACE.                 
029500     03  W-IDARTNR-X.                                                     
029600         05  W-IDARTNR           PIC S9(9)   VALUE ZERO COMP-3.           
031401     SKIP2                                                                
031501*    --- STATUS-KOD FRÅN IMS                                              
031601 01  STATUS-WS                   PIC XX.                                  
031701     88  SEGMENT-FOUND                       VALUE '  '.                  
031801     88  SEGMENT-FOUND-EXISTS                VALUE 'II'.                  
031901     88  SEGMENT-MISSING                     VALUE 'GE'.                  
032001     SKIP2                                                                
032101 01  GOOD-STATUSCODES.                                                    
032201     03  GOOD-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
032301     SKIP3                                                                
032401 01  SSA1                        PIC X(64).                               
032501 01  SSA2                        PIC X(64).                               
032601 01  SSA3                        PIC X(64).                               
032701     EJECT                                                                
032801*    --- IMS FUNCTION CODES                                               
032901*01  -COPY W0003                                                          
033001     EJECT                                                                
033101*    ---  DLI INPUT-OUTPUT AREA                                           
033201 01  FILLER                      PIC X(16) VALUE 'DLI-IO-WDK601'.         
033301 01  DLI-IO-WDK601.                                                       
033401*    03  -COPY WDK601                                                     
033501 01  FILLER                      PIC X(16) VALUE 'DLI-IO-WDK611'.         
033601 01  DLI-IO-WDK611.                                                       
033701*    03  -COPY WDK611                                                     
033801 01  FILLER                      PIC X(16) VALUE 'DLI-IO-WDK621'.         
033901 01  DLI-IO-WDK621.                                                       
034001*    03  -COPY WDK621                                                     
034101 01  FILLER                      PIC X(16) VALUE 'DLI-IO-WDK701'.         
034201 01  DLI-IO-WDK701.                                                       
034301*    03  -COPY WDK701                                                     
034401 01  FILLER                      PIC X(16) VALUE 'DLI-IO-WDK711'.         
034501 01  DLI-IO-WDK711.                                                       
034601*    03  -COPY WDK711                                                     
034701 01  FILLER                      PIC X(16) VALUE 'DLI-IO-WDK724'.         
034801 01  DLI-IO-WDK724.                                                       
034901*    03  -COPY WDK724                                                     
035001 01  FILLER                      PIC X(16) VALUE 'DLI-IO-WDB601'.         
035101 01  DLI-IO-WDB601.                                                       
035201*    03  -COPY WDB601                                                     
036201 01  FILLER                      PIC X(16) VALUE 'DLI-IO-WDF101'.         
036301 01  DLI-IO-WDF101.                                                       
036401*    03  -COPY WDF101                                                     
036501 01  FILLER                      PIC X(16) VALUE 'DLI-IO-WDF102'.         
036601 01  DLI-IO-WDF102.                                                       
036701*    03  -COPY WDF102 -PRE LEV-                                           
036801                                                                          
036901     EJECT                                                                
037001 LINKAGE SECTION.                                                         
037101                                                                          
037201*01  -COPY W0008   -PRE WDK6-                                             
037301     05  FILLER                  PIC X.                                   
037401                                                                          
037501*01  -COPY W0008   -PRE WDK7-                                             
037601     05  FILLER                  PIC X.                                   
037701                                                                          
037801*01  -COPY W0008   -PRE WDB6-                                             
037901     05  FILLER                  PIC X.                                   
038001                                                                          
038101*01  -COPY W0008   -PRE WDF1-                                             
038201     05  FILLER                  PIC X.                                   
038301                                                                          
038401*01  -COPY W0008   -PRE WDG2-                                             
038501     05  FILLER                  PIC X.                                   
038601                                                                          
039001 01  ARTC-P                      PIC X.                                   
039101 01  WDK7-P                      PIC X.                                   
039201 01  GMTA-P                      PIC X.                                   
039301 01  BETA-P                      PIC X.                                   
039401 01  PRIA-P                      PIC X.                                   
039501 01  PRIB-P                      PIC X.                                   
039601 01  PRIS-COST-WDK6-PCB          PIC X.                                   
039701 01  PRIS-COST-WDK7-PCB          PIC X.                                   
039801 01  PRIS-COST-WDF1-PCB          PIC X.                                   
039901 01  PRIS-COST-9305-PCB          PIC X.                                   
040001 01  PRIS-COST-WDK72-PCB         PIC X.                                   
040101 01  PRIS-COST-WDB6-PCB          PIC X.                                   
040301 PROCEDURE DIVISION  USING  WDK6-PCB WDK7-PCB WDB6-PCB                    
040401                     WDF1-PCB WDG2-PCB                                    
040501                     ARTC-P WDK7-P GMTA-P BETA-P PRIA-P PRIB-P            
040601                     PRIS-COST-WDK6-PCB                                   
040701                     PRIS-COST-WDK7-PCB                                   
040801                     PRIS-COST-WDF1-PCB                                   
040901                     PRIS-COST-9305-PCB                                   
041001                     PRIS-COST-WDK72-PCB                                  
041101                     PRIS-COST-WDB6-PCB.                                  
041301 MAIN SECTION.                                                            
041401     ENTRY 'DLITCBL' USING  WDK6-PCB WDK7-PCB WDB6-PCB                    
041501                     WDF1-PCB WDG2-PCB                                    
041601                     ARTC-P WDK7-P GMTA-P BETA-P PRIA-P PRIB-P            
041701                     PRIS-COST-WDK6-PCB                                   
041801                     PRIS-COST-WDK7-PCB                                   
041901                     PRIS-COST-WDF1-PCB                                   
042001                     PRIS-COST-9305-PCB                                   
042101                     PRIS-COST-WDK72-PCB                                  
042201                     PRIS-COST-WDB6-PCB.                                  
042401                                                                          
042501     PERFORM A-INIT                                                       
042601                                                                          
042701     PERFORM S01-READ-W56131                                              
042801     PERFORM S02-READ-W01184A                                             
042901     PERFORM S03-READ-W01184B                                             
043001                                                                          
043101     PERFORM UNTIL END-OF-W56131                                          
043201       PERFORM B-PROCESS                                                  
043301       PERFORM S01-READ-W56131                                            
043401     END-PERFORM                                                          
043501                                                                          
043601     PERFORM Z-FINIT                                                      
043701                                                                          
043801     MOVE ZERO  TO RETURN-CODE                                            
043901     GOBACK                                                               
044001     .                                                                    
044101     EJECT                                                                
044201                                                                          
044301 A-INIT SECTION.                                                          
044401                                                                          
044501     OPEN INPUT  W56131                                                   
044601                 W01184A                                                  
044701                 W01184B                                                  
044801                                                                          
044901     OPEN OUTPUT W56134                                                   
045001                                                                          
045101     ACCEPT TODAYS-DATE          FROM DATE                                
045201     MOVE IDPGM                  TO  POSTSUM-PROGNAMN                     
045301                                                                          
045401     INITIALIZE UT-RAD                                                    
045501     .                                                                    
045601     EJECT                                                                
045701                                                                          
045801 B-PROCESS SECTION.                                                       
045901                                                                          
046001     PERFORM BA-FETCH-WDK6-LPC                                            
046101                                                                          
046201     PERFORM UNTIL END-OF-W01184A OR                                      
046301                   (IN1-IDARTNR <= IN2-SLAG-IDARTNR AND                   
046401                    IN1-IDDC    <= IN2-SLAG-IDDC)                         
046501       PERFORM S02-READ-W01184A                                           
046601     END-PERFORM                                                          
046701                                                                          
046801     PERFORM UNTIL END-OF-W01184B OR                                      
046901                   (IN1-IDARTNR <= IN3-SLAG-IDARTNR AND                   
047001                    IN1-IDDC    <= IN3-SLAG-IDDC)                         
047101       PERFORM S03-READ-W01184B                                           
047201     END-PERFORM                                                          
047301                                                                          
047401     IF (IN1-IDARTNR = IN2-SLAG-IDARTNR AND                               
047501         IN1-IDDC    = IN2-SLAG-IDDC)                                     
047601       PERFORM BC-FETCH-CURR-STOCK                                        
047701     END-IF                                                               
047801                                                                          
047901     IF (IN1-IDARTNR = IN3-SLAG-IDARTNR AND                               
048001         IN1-IDDC    = IN3-SLAG-IDDC)                                     
048101       PERFORM BB-FETCH-PREV-STOCK                                        
048201     END-IF                                                               
048301                                                                          
048401     PERFORM BD-CALCULATE-UNIT-COST                                       
048501                                                                          
048601     IF IN1-IDLANDX2 = WS-PREV-IDLANDX2                                   
048701       CONTINUE                                                           
048801     ELSE                                                                 
048901       MOVE UT-CONTROL-REC1      TO UT-AREA                               
049001       PERFORM S11-WRITE-W56134                                           
049101       MOVE IN1-IDLANDX2         TO UT-CTL-IDLANDX2                       
049201                                    WS-PREV-IDLANDX2                      
049301       MOVE UT-CONTROL-REC2      TO UT-AREA                               
049401       PERFORM S11-WRITE-W56134                                           
049501       MOVE UT-HEADER            TO UT-AREA                               
049601       PERFORM S11-WRITE-W56134                                           
049701     END-IF                                                               
049801                                                                          
049901     MOVE UT-RAD                 TO UT-AREA                               
050001     PERFORM S11-WRITE-W56134                                             
050101     INITIALIZE UT-RAD                                                    
050201     .                                                                    
050301     EJECT                                                                
050401                                                                          
050501 BA-FETCH-WDK6-LPC SECTION.                                               
050601                                                                          
050701     MOVE IN1-IDDC               TO W-IDDC                                
050801                                    UT-IDDC                               
050901     MOVE IN1-IDARTNR            TO W-IDARTNR                             
051001                                    UT-IDARTNR                            
051101     MOVE IN1-IDKUNDRF           TO UT-IDKUNDRF                           
051201     MOVE IN1-IDLOPNRM           TO UT-IDLOPNRM                           
051301     MOVE IN1-KVAVIS             TO UT-KVAVIS                             
051401                                                                          
051501     PERFORM IMS-GU-WDK601                                                
051601     MOVE ART-KDPRODSL           TO UT-KDPRODSL                           
051701                                                                          
051801     PERFORM IMS-GNP-WDK611                                               
051901     IF SEGMENT-FOUND                                                     
052001       MOVE CLAG-KDPSLLOC        TO UT-KDPSLLOC                           
052101     END-IF                                                               
052201                                                                          
052301     MOVE 1                      TO WS-MARKUP                             
052401     MOVE NOO                    TO WS-PROD-SW                            
052501                                                                          
052601     PERFORM IMS-GU-WDB601                                                
052701                                                                          
052801     PERFORM                                                              
052901     VARYING IX FROM 1 BY 1                                               
053001       UNTIL IX > MARKUP-TAB-MAX OR                                       
053101             PRODKOD-FOUND                                                
053201       IF MARKUP-LPC (IX) = CLAG-KDPSLLOC                                 
053301         SET PRODKOD-FOUND       TO TRUE                                  
053401         MOVE IX                 TO IX-MARKUP                             
053501       END-IF                                                             
053601     END-PERFORM                                                          
053701                                                                          
053801     IF PRODKOD-FOUND                                                     
053901       IF DCS-NDC-NA AND DCS-USA                                          
054001         MOVE MARKUP-FAKTOR-USA (IX-MARKUP)                               
054101                                 TO WS-MARKUP                             
054201       END-IF                                                             
054301     END-IF                                                               
054401     .                                                                    
054501     EJECT                                                                
054601 BB-FETCH-PREV-STOCK SECTION.                                             
054701                                                                          
054801******************************************************************        
054901* CHECKING BEGNING STOCK IN PREVIOUS FILE W01184B*                        
055001******************************************************************        
055101     MOVE IN3-SLAG-KVLS          TO WS-KVLS                               
055201     MOVE IN3-SLAG-KVEFRS        TO WS-KVEFRS                             
055301                                                                          
055401* CALCULATING BEG STOCK VALUE *                                           
055501     COMPUTE WS-BEG-STOCK-AVIL = WS-KVLS +  WS-KVEFRS                     
055601                                                                          
055701     MOVE WS-BEG-STOCK-AVIL      TO UT-BEG-STOCK-AVIL                     
055801     MOVE IN3-SLAG-PRAVCOST      TO UT-BEG-PRAVCOST                       
055901                                                                          
056001* CALCULATING INVENTORY VALUE *                                           
056101     COMPUTE WS-BEG-INVENTORY ROUNDED = WS-BEG-STOCK-AVIL *               
056201                                        IN3-SLAG-PRAVCOST                 
056301     MOVE WS-BEG-INVENTORY       TO UT-BEG-INVENTORY                      
056401     .                                                                    
056501     EJECT                                                                
056601                                                                          
056701 BC-FETCH-CURR-STOCK SECTION.                                             
056801******************************************************************        
056901* CHECKING END STOCK IN CURRENT  FILE W01184A*                            
057001******************************************************************        
057101     MOVE IN2-SLAG-KVLS          TO WS-KVLS                               
057201     MOVE IN2-SLAG-KVEFRS        TO WS-KVEFRS                             
057301* CALCULATING END STOCK VALUE *                                           
057401     COMPUTE WS-END-STOCK-AVIL  = WS-KVLS +  WS-KVEFRS                    
057501     MOVE WS-END-STOCK-AVIL      TO UT-END-STOCK-AVIL                     
057601     MOVE IN2-SLAG-PRAVCOST      TO UT-END-PRAVCOST                       
057701                                                                          
057801* CALCULATING INVENTORY VALUE *                                           
057901     COMPUTE WS-END-INVENTORY ROUNDED = WS-END-STOCK-AVIL *               
058001                                        IN2-SLAG-PRAVCOST                 
058101     MOVE WS-END-INVENTORY       TO UT-END-INVENTORY                      
058201                                                                          
058301     .                                                                    
058401     EJECT                                                                
058501                                                                          
058601 BD-CALCULATE-UNIT-COST SECTION.                                          
058701************************************************************              
058801* CALCULATION FOR UNIT COST AND EXTENDED COST *                           
058901************************************************************              
059001                                                                          
059101     MOVE IN1-IDDC               TO W-IDDC                                
059201     MOVE IN1-IDARTNR            TO W-IDARTNR                             
059301                                                                          
059401     MOVE ZEROES                 TO WS-PRARTBEL-PR                        
059501                                    WS-PRARTBES-PR                        
059601                                                                          
059701     PERFORM IMS-GU-WDK711                                                
059801     IF SEGMENT-FOUND                                                     
059901       IF SLAG-IDDC-REF = SPACES                                          
060001         PERFORM IMS-GNP-WDK724                                           
060101                                                                          
060201         PERFORM                                                          
060301           UNTIL SEGMENT-MISSING OR                                       
060401                 IN1-IDLEVNR = SPRL-IDLEVNR-PR                            
060501           PERFORM IMS-GNP-WDK724                                         
060601         END-PERFORM                                                      
060701                                                                          
060801         IF SEGMENT-MISSING                                               
060901           CONTINUE                                                       
061001         ELSE                                                             
061101           MOVE SPRL-KDVALISO    TO UT-KDVALISO                           
061201           COMPUTE WS-PRARTBEL-PR ROUNDED = SPRL-PRARTBEL-PR              
061301           MOVE WS-PRARTBEL-PR   TO UT-PRARTBEL-PR                        
061401                                                                          
061501           COMPUTE WS-PRARTBES-PR ROUNDED = SPRL-PRARTBES-PR              
061601           MOVE WS-PRARTBES-PR   TO UT-LANDING-COST                       
061701         END-IF                                                           
061801                                                                          
061901         MOVE IN1-IDLEVNR        TO UT-SOURCE                             
062001       ELSE                                                               
062101         PERFORM BDA-GET-COST-LOCAL-CURR                                  
062201         COMPUTE WS-PRARTBES-PR  ROUNDED = WS-PRARTBEL-PR                 
062301                                         * WS-MARKUP                      
062401         MOVE WS-PRARTBES-PR     TO UT-LANDING-COST                       
062501         MOVE IN1-IDLEVNR        TO UT-SOURCE                             
062601       END-IF                                                             
062701                                                                          
062801       COMPUTE WS-EXTD-COST ROUNDED =                                     
062901                                    IN1-KVAVIS * WS-PRARTBEL-PR           
063001       MOVE WS-EXTD-COST         TO UT-EXTD-COST                          
063101                                                                          
063201       COMPUTE WS-EXTD-LANDED-COST ROUNDED =                              
063301                                    IN1-KVAVIS * WS-PRARTBES-PR           
063401       MOVE WS-EXTD-LANDED-COST  TO UT-EXT-LANDING-COST                   
063501     END-IF                                                               
063601     .                                                                    
063701     EJECT                                                                
063801                                                                          
063901******************************************************************        
064001*      GET COST LOCAL CURR FOR NOT LOCALLY SOURCED PARTS                  
064101******************************************************************        
064201 BDA-GET-COST-LOCAL-CURR  SECTION.                                        
064301                                                                          
064401     MOVE ZEROES                 TO W-PRARTBEL                            
064501                                                                          
064601     PERFORM BDAA-GET-WDK724-PRICE                                        
064701     IF PRICE-FOUND                                                       
064801       CONTINUE                                                           
064901     ELSE                                                                 
065001       MOVE SLAG-IDLEVNR         TO W-IDLEVNR                             
065101       IF W-IDLEVNR = '1441 ' OR 'BP2TW'                                  
065201         MOVE WC-CDC-SE          TO PRIS-IDDC                             
065301         MOVE DCS-IDDISTR-REFILL                                          
065401                                 TO PRIS-IDDISTR                          
065501         PERFORM BDAB-CALL-W335PRIS                                       
065601         MOVE PRIS-PRARTNTO      TO W-PRARTBEL                            
065701       ELSE                                                               
065801         PERFORM BDAC-WDK621-PRIS                                         
065901         IF SEGMENT-FOUND                                                 
066001           MOVE PRL-PRARTBEL-PR  TO W-PRARTBEL                            
066101         END-IF                                                           
066201       END-IF                                                             
066301       IF W-IDLEVNR = '1441 ' OR 'BP2TW' OR                               
066401          PRL-KDVALISO = 'SEK'                                            
066501         MOVE DCS-KDVALISO       TO CURR-KDVALISO-ROW                     
066601       ELSE                                                               
066701         MOVE PRL-KDVALISO       TO CURR-KDVALISO-ROW                     
066801       END-IF                                                             
066901       PERFORM BDAD-READ-VALUTA                                           
067001       IF W-VALUTA-FOUND = YES                                            
067101         MOVE CURR-PRKURS-NEW    TO W-PRKURS                              
067201         IF W-IDLEVNR = '1441 ' OR 'BP2TW' OR                             
067301            PRL-KDVALISO = 'SEK'                                          
067401           MOVE W-PRKURS         TO W-PRKURS2                             
067501         ELSE                                                             
067601           MOVE W-PRKURS         TO W-FIRST-PRKURS                        
067701           MOVE DCS-KDVALISO     TO CURR-KDVALISO-ROW                     
067801           PERFORM BDAD-READ-VALUTA                                       
067901           IF W-VALUTA-FOUND = YES                                        
068001             COMPUTE W-PRKURS2 ROUNDED =                                  
068101                     W-PRKURS / W-FIRST-PRKURS                            
068201           ELSE                                                           
068301             MOVE ZERO           TO W-PRKURS2                             
068401           END-IF                                                         
068501         END-IF                                                           
068601         IF W-PRKURS2 = ZERO                                              
068701           CONTINUE                                                       
068801         ELSE                                                             
068901           MOVE CURR-KDVALISO-ROW TO UT-KDVALISO                          
069001           COMPUTE WS-PRARTBEL-PR ROUNDED =                               
069101                   W-PRARTBEL / W-PRKURS2                                 
069201           MOVE WS-PRARTBEL-PR    TO UT-PRARTBEL-PR                       
069301         END-IF                                                           
069401       END-IF                                                             
069501     END-IF                                                               
069601     .                                                                    
069701     EJECT                                                                
069801                                                                          
069901 BDAA-GET-WDK724-PRICE    SECTION.                                        
070001                                                                          
070101     MOVE NOO                    TO WS-PRICE-SW                           
070201* GET FIRST OCCURRENCE OF WDK724 WHERE SPRL-SUINLEV-PR IS                 
070301* GREATER THAN 0                                                          
070401     PERFORM IMS-GNP-WDK724                                               
070501     PERFORM                                                              
070601       UNTIL SEGMENT-MISSING OR PRICE-FOUND                               
070701       IF SEGMENT-FOUND                                                   
070801         IF SPRL-SUINLEV-PR > 0                                           
070901           MOVE YES              TO WS-PRICE-SW                           
071001         END-IF                                                           
071101       END-IF                                                             
071201       PERFORM IMS-GNP-WDK724                                             
071301     END-PERFORM                                                          
071401                                                                          
071501* IF NOT FOUND, GET FIRST OCCURRENCE OF WDK724                            
071601     IF PRICE-FOUND                                                       
071701        CONTINUE                                                          
071801     ELSE                                                                 
071901        PERFORM IMS-GNP-WDK724-FIRST                                      
072001        IF SEGMENT-FOUND                                                  
072101           MOVE YES              TO WS-PRICE-SW                           
072201        END-IF                                                            
072301     END-IF                                                               
072401                                                                          
072501     IF PRICE-FOUND                                                       
072601       MOVE SPRL-IDLEVNR-PR      TO W-IDLEVNR                             
072701       MOVE SPRL-PRARTBEL-PR     TO W-PRARTBEL                            
072801       PERFORM BDAAA-READ-VALUTA                                          
072901     END-IF                                                               
073001     .                                                                    
073101     EJECT                                                                
073201                                                                          
073301 BDAAA-READ-VALUTA  SECTION.                                              
073401                                                                          
073501     MOVE DCS-KDVALISO           TO CURR-KDVALISO-HUV                     
073601     MOVE SPRL-KDVALISO          TO WS-KDVALISO                           
073701     IF CURR-KDVALISO-HUV = WS-KDVALISO                                   
073801       MOVE 1                    TO W-PRKURS                              
073901       MOVE 1                    TO W-REVALUTA                            
074001     ELSE                                                                 
074101       MOVE WS-KDVALISO          TO CURR-KDVALISO-ROW                     
074102       MOVE W-DATE-AAMM          TO CURR-TIAAMM                           
074401       CALL W510CURR USING CURR-W510CURR WDG2-PCB                         
074501       IF CURR-KDSVAR = ' '                                               
074901         MOVE CURR-PRKURS-NEW    TO W-PRKURS                              
075001         MOVE CURR-REVALUTA-TO   TO W-REVALUTA                            
075002       ELSE                                                               
075003         MOVE 1                  TO W-PRKURS                              
075004         MOVE 1                  TO W-REVALUTA                            
075101       END-IF                                                             
075201     END-IF                                                               
075301     MOVE SPRL-IDLEVNR-PR        TO W-IDLEVNR                             
075401     PERFORM IMS-GU-WDF101                                                
075501     IF SEGMENT-MISSING                                                   
075601       MOVE 1                    TO W-RETULF                              
075701     ELSE                                                                 
075801       MOVE DCS-IDLANDX2         TO W-IDLAND                              
075901       PERFORM IMS-GNP-WDF102                                             
076001       IF SEGMENT-FOUND                                                   
076101         IF LEV-TULL-TITULF < DAGENS-AAMMDD                               
076201           MOVE LEV-TULL-RETULF-1 TO W-RETULF                             
076301         ELSE                                                             
076401           MOVE LEV-TULL-RETULF-2 TO W-RETULF                             
076501         END-IF                                                           
076601       ELSE                                                               
076701         MOVE 1                   TO W-RETULF                             
076801       END-IF                                                             
076901     END-IF                                                               
077001     COMPUTE WS-PRARTBEL-PR ROUNDED = W-PRARTBEL * W-RETULF               
077101                                    * W-PRKURS / W-REVALUTA               
077201     MOVE WS-PRARTBEL-PR          TO UT-PRARTBEL-PR                       
077301     .                                                                    
077401     EJECT                                                                
077501                                                                          
077601 BDAB-CALL-W335PRIS SECTION.                                              
077701                                                                          
077801     MOVE 1                           TO PRIS-KDCALL                      
077901     MOVE 1                           TO PRIS-KVBEART                     
078001     MOVE 0                           TO PRIS-IDKUNDNR                    
078101     MOVE 4                           TO PRIS-KDORDKL                     
078201     MOVE 'W5613300'                  TO PRIS-IDPGM                       
078301     MOVE SPACE                       TO PRIS-FLINVEST                    
078401     MOVE IN1-IDARTNR                 TO PRIS-IDARTNR                     
078501                                                                          
078601     CALL W335PRIS                 USING PRIS-W335PRIS                    
078701                                         ARTC-P WDK7-P                    
078801                                         GMTA-P BETA-P                    
078901                                         PRIA-P PRIB-P                    
079001                                         PRIS-COST-WDK6-PCB               
079101                                         PRIS-COST-WDK7-PCB               
079201                                         PRIS-COST-WDF1-PCB               
079301                                         PRIS-COST-9305-PCB               
079401                                         PRIS-COST-WDK72-PCB              
079501                                         PRIS-COST-WDB6-PCB               
079701     .                                                                    
079801     EJECT                                                                
079901                                                                          
080001 BDAC-WDK621-PRIS SECTION.                                                
080101     PERFORM IMS-GU-WDK601                                                
080201     PERFORM IMS-GNP-WDK611                                               
080301     IF SEGMENT-FOUND                                                     
080401        PERFORM IMS-GNP-WDK621                                            
080501     END-IF                                                               
080601     IF SEGMENT-FOUND                                                     
080701       PERFORM                                                            
080801         UNTIL (PRL-IDLEVNR = SLAG-IDLEVNR AND                            
080901                PRL-KDSTATUS-PR > 0 AND                                   
081001                W-DAPRLIST-9KOMPL <= PRL-DAPRLIST-9KOMPL) OR              
081101               SEGMENT-MISSING                                            
081201         PERFORM IMS-GNP-WDK621                                           
081301       END-PERFORM                                                        
081401     END-IF                                                               
081501     .                                                                    
081601     EJECT                                                                
081701                                                                          
081801 BDAD-READ-VALUTA SECTION.                                                
081901                                                                          
082001     MOVE FUNCTION CURRENT-DATE(3:2)  TO W-DATE-AAMM(1:2)                 
082002     MOVE FUNCTION CURRENT-DATE(5:2)  TO W-DATE-AAMM(3:2)                 
082301     MOVE YES                         TO W-VALUTA-FOUND                   
082302     MOVE W-DATE-AAMM                 TO CURR-TIAAMM                      
082303     MOVE WS-KDVALISO-HUV             TO CURR-KDVALISO-HUV                
082305     MOVE 'M'                         TO CURR-KDVALTYP                    
082401     CALL W510CURR USING CURR-W510CURR WDG2-PCB                           
082501     IF CURR-KDSVAR = ' '                                                 
082502       MOVE CURR-PRKURS-NEW           TO W-PRKURS                         
082503     ELSE                                                                 
082601       MOVE NOO                       TO W-VALUTA-FOUND                   
082701       MOVE ZERO                      TO W-PRKURS                         
083001     END-IF                                                               
083101     .                                                                    
083201     EJECT                                                                
083301                                                                          
083401 Z-FINIT SECTION.                                                         
083501     CLOSE W56131                                                         
083601           W01184A                                                        
083701           W01184B                                                        
083801           W56134                                                         
083901     SKIP2                                                                
084001     MOVE 'S'                    TO POSTSUM-OPKOD                         
084101     CALL POSTSUM             USING POSTSUM-PARM                          
084201     .                                                                    
084301     EJECT                                                                
084401                                                                          
084501 S01-READ-W56131  SECTION.                                                
084601     READ W56131               INTO IN1-AREA                              
084701       AT END                                                             
084801         MOVE HIGH-VALUE         TO IN1-AREA                              
084901         MOVE ZERO               TO IN1-IDARTNR                           
085001         SET END-OF-W56131       TO TRUE                                  
085101                                                                          
085201     NOT AT END                                                           
085301       MOVE 'W56131'             TO POSTSUM-FDNAMN                        
085401       MOVE 'W56131D1'           TO POSTSUM-DDNAMN2                       
085501       MOVE SPACES               TO POSTSUM-TRANSTYP                      
085601       CALL POSTSUM           USING POSTSUM-PARM                          
085701     END-READ                                                             
085801     .                                                                    
085901     EJECT                                                                
086001                                                                          
086101 S02-READ-W01184A SECTION.                                                
086201                                                                          
086301     READ W01184A              INTO IN2-AREA                              
086401       AT END                                                             
086501         MOVE HIGH-VALUE         TO IN2-AREA                              
086601         MOVE ZERO               TO IN2-SLAG-IDARTNR                      
086701         SET END-OF-W01184A      TO TRUE                                  
086801                                                                          
086901     NOT AT END                                                           
087001       MOVE 'W01184A'            TO POSTSUM-FDNAMN                        
087101       MOVE 'W56133D2'           TO POSTSUM-DDNAMN2                       
087201       MOVE SPACES               TO POSTSUM-TRANSTYP                      
087301       CALL POSTSUM           USING POSTSUM-PARM                          
087401     END-READ                                                             
087501     .                                                                    
087601     EJECT                                                                
087701                                                                          
087801 S03-READ-W01184B SECTION.                                                
087901                                                                          
088001     READ W01184B              INTO IN3-AREA                              
088101       AT END                                                             
088201         MOVE HIGH-VALUE         TO IN3-AREA                              
088301         MOVE ZERO               TO IN3-SLAG-IDARTNR                      
088401         SET END-OF-W01184B      TO TRUE                                  
088501                                                                          
088601     NOT AT END                                                           
088701       MOVE 'W01184B'            TO POSTSUM-FDNAMN                        
088801       MOVE 'W56133D3'           TO POSTSUM-DDNAMN2                       
088901       MOVE SPACES               TO POSTSUM-TRANSTYP                      
089001       CALL POSTSUM           USING POSTSUM-PARM                          
089101     END-READ                                                             
089201     .                                                                    
089301     EJECT                                                                
089401                                                                          
089501 S11-WRITE-W56134 SECTION.                                                
089601                                                                          
089701     WRITE UT-RECORD           FROM UT-AREA                               
089801     MOVE SPACES                 TO UT-AREA                               
089901                                                                          
090001     MOVE SPACES                 TO POSTSUM-TRANSTYP                      
090101     MOVE 'W56134'               TO POSTSUM-FDNAMN                        
090201     MOVE 'W56133D3'             TO POSTSUM-DDNAMN2                       
090301     CALL POSTSUM             USING POSTSUM-PARM                          
090401     .                                                                    
090501     EJECT                                                                
090601                                                                          
090701 S99-ABEND SECTION.                                                       
090801                                                                          
090901     SKIP2                                                                
091001     MOVE 'S'                    TO POSTSUM-OPKOD                         
091101     CALL POSTSUM             USING POSTSUM-PARM                          
091201     CALL ABEND               USING RKOD-ABEND                            
091301     .                                                                    
091401     EJECT                                                                
091501* --- IMS SECTIONS  ---                                                   
091601                                                                          
091701 IMS-GU-WDB601    SECTION.                                                
091801     STRING 'WDB601  (IDDC     =' W-IDDC-X ')'                            
091901             DELIMITED BY SIZE INTO SSA1                                  
092001     MOVE '  GE'                 TO GOOD-STATUSCODES                      
092101     CALL CBLTDLI             USING GU                                    
092201                                    WDB6-PCB                              
092301                                    DLI-IO-WDB601                         
092401                                    SSA1                                  
092501     MOVE WDB6-STATUS-CODE       TO STATUS-WS                             
092601     PERFORM IMS-STATUSCHECK                                              
092701     .                                                                    
092801     EJECT                                                                
092901 IMS-GU-WDK601 SECTION.                                                   
093001                                                                          
093101     STRING 'WDK601  (IDARTNR  =' W-IDARTNR-X ')'                         
093201             DELIMITED BY SIZE INTO SSA1                                  
093301     MOVE '  '                   TO GOOD-STATUSCODES                      
093401     CALL CBLTDLI             USING GU                                    
093501                                    WDK6-PCB                              
093601                                    DLI-IO-WDK601                         
093701                                    SSA1                                  
093801     MOVE WDK6-STATUS-CODE       TO STATUS-WS                             
093901     PERFORM IMS-STATUSCHECK                                              
094001     .                                                                    
094101     EJECT                                                                
094201                                                                          
094301 IMS-GNP-WDK611 SECTION.                                                  
094401                                                                          
094501     MOVE 'WDK611             '  TO SSA1                                  
094601     MOVE '  GE'                 TO GOOD-STATUSCODES                      
094701     CALL CBLTDLI             USING GNP                                   
094801                                    WDK6-PCB                              
094901                                    DLI-IO-WDK611                         
095001                                    SSA1                                  
095101     MOVE WDK6-STATUS-CODE       TO STATUS-WS                             
095201     PERFORM IMS-STATUSCHECK                                              
095301     .                                                                    
095401     EJECT                                                                
095501                                                                          
095601 IMS-GNP-WDK621 SECTION.                                                  
095701                                                                          
095801     MOVE 'WDK621             '  TO SSA1                                  
095901     MOVE '  GE'                 TO GOOD-STATUSCODES                      
096001     CALL CBLTDLI             USING GNP                                   
096101                                    WDK6-PCB                              
096201                                    DLI-IO-WDK621                         
096301                                    SSA1                                  
096401     MOVE WDK6-STATUS-CODE       TO STATUS-WS                             
096501     PERFORM IMS-STATUSCHECK                                              
096601     .                                                                    
096701     EJECT                                                                
096801                                                                          
096901 IMS-GU-WDK711 SECTION.                                                   
097001                                                                          
097101     STRING 'WDK701  (IDARTNR  =' W-IDARTNR-X ')'                         
097201             DELIMITED BY SIZE INTO SSA1                                  
097301     STRING 'WDK711  (IDDC     =' W-IDDC-X ')'                            
097401             DELIMITED BY SIZE INTO SSA2                                  
097501     MOVE '  GE'                 TO GOOD-STATUSCODES                      
097601     CALL CBLTDLI             USING GU                                    
097701                                    WDK7-PCB                              
097801                                    DLI-IO-WDK711                         
097901                                    SSA1 SSA2                             
098001     MOVE WDK7-STATUS-CODE       TO STATUS-WS                             
098101     PERFORM IMS-STATUSCHECK                                              
098201     .                                                                    
098301     EJECT                                                                
098401 IMS-GNP-WDK724-FIRST SECTION.                                            
098501                                                                          
098601     MOVE 'WDK724  *F         '  TO SSA1                                  
098701     MOVE '  GE'                 TO GOOD-STATUSCODES                      
098801     CALL CBLTDLI             USING GNP                                   
098901                                    WDK7-PCB                              
099001                                    DLI-IO-WDK724                         
099101                                    SSA1                                  
099201     MOVE WDK7-STATUS-CODE       TO STATUS-WS                             
099301     PERFORM IMS-STATUSCHECK                                              
099401     .                                                                    
099501     EJECT                                                                
099601 IMS-GNP-WDK724 SECTION.                                                  
099701                                                                          
099801     MOVE 'WDK724             '  TO SSA1                                  
099901     MOVE '  GE'                 TO GOOD-STATUSCODES                      
100001     CALL CBLTDLI             USING GNP                                   
100101                                    WDK7-PCB                              
100201                                    DLI-IO-WDK724                         
100301                                    SSA1                                  
100401     MOVE WDK7-STATUS-CODE       TO STATUS-WS                             
100501     PERFORM IMS-STATUSCHECK                                              
100601     .                                                                    
100701     EJECT                                                                
100801                                                                          
100901 IMS-GU-WDF101   SECTION.                                                 
101001     STRING 'WDF101  (IDLEVNR  =' W-IDLEVNR-X ')'                         
101101             DELIMITED BY SIZE INTO SSA1                                  
101201     MOVE '  GE'                 TO GOOD-STATUSCODES                      
101301     CALL CBLTDLI             USING GU                                    
101401                                    WDF1-PCB                              
101501                                    DLI-IO-WDF101                         
101601                                    SSA1                                  
101701     MOVE WDF1-STATUS-CODE       TO STATUS-WS                             
101801     PERFORM IMS-STATUSCHECK                                              
101901     .                                                                    
102001     SKIP3                                                                
102101                                                                          
102201 IMS-GNP-WDF102   SECTION.                                                
102301     STRING 'WDF102  (IDLAND   =' W-IDLAND-X ')'                          
102401             DELIMITED BY SIZE INTO SSA1                                  
102501     MOVE '  GE'                 TO GOOD-STATUSCODES                      
102601     CALL CBLTDLI             USING GNP                                   
102701                                    WDF1-PCB                              
102801                                    DLI-IO-WDF102                         
102901                                    SSA1                                  
103001     MOVE WDF1-STATUS-CODE       TO STATUS-WS                             
103101     PERFORM IMS-STATUSCHECK                                              
103201     .                                                                    
103301     EJECT                                                                
106201                                                                          
106301 IMS-STATUSCHECK SECTION.                                                 
106401                                                                          
106501     SET STATUS-IX               TO 1                                     
106601     SEARCH GOOD-STATUS                                                   
106701       AT END                                                             
106801         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
106901           DELIMITED BY SIZE INTO ERROR-TEXT                              
107001         DISPLAY ERROR-TEXT                                               
107101         CALL FELLOG                                                      
107201       WHEN GOOD-STATUS (STATUS-IX) = STATUS-WS                           
107301         CONTINUE                                                         
107401     END-SEARCH                                                           
107501     .                                                                    
