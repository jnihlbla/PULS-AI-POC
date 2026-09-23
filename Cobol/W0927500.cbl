000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W0927500.                                                
000300 AUTHOR.         HELGEGREN PER-ANDERS.                                    
000400 DATE-WRITTEN.   06/04/07.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700                                                                          
000800*    FUNKTION:                                                            
000900*        SKAPAR LARM OM STUNDANDE ANNULLATION I NAP                       
001000*        PGA 6 VECKOR KVAR TILL SLUTTID                                   
001100*                                                                         
001200*        PROGRAMMET LÄSER      WDK6                                       
001300*        PROGRAMMET LÄSER      WDR2                                       
001400*        PROGRAMMET UPPDATERAR WDR5                                       
001500*                                                                         
001600                                                                          
001700     SKIP3                                                                
001800 ENVIRONMENT DIVISION.                                                    
001900     SKIP2                                                                
002000 INPUT-OUTPUT SECTION.                                                    
002100                                                                          
002200 FILE-CONTROL.                                                            
002300     SKIP2                                                                
002400*          --- AVTALSPOSTER MED 6 VECKOR KVAR FRÅN NAP                    
002500     SELECT W09277                     ASSIGN TO W09275D1.                
002600     EJECT                                                                
002700 DATA DIVISION.                                                           
002800     SKIP3                                                                
002900 FILE SECTION.                                                            
003000     SKIP3                                                                
003100 FD  W09277                                                               
003200     RECORDING       F                                                    
003300     BLOCK CONTAINS  0.                                                   
003400                                                                          
003500*01  -COPY NAPIN         -L.                                              
003600     SKIP3                                                                
003700     EJECT                                                                
003800 WORKING-STORAGE SECTION.                                                 
003900                                                                          
004000 77  IDPGM                       PIC X(8)    VALUE 'W0927500'.            
004100 01  CHKP-VAR.                                                            
004200     03 CHKP-MSG-IO-AREA-LENGTH  PIC S9(9)   VALUE +32 COMP SYNC.         
004300     03 CHKP-MSG-IO-AREA         PIC X(32)   VALUE SPACE.                 
004400     03 CHKP-AREA-LENGTH         PIC S9(9)   VALUE +32 COMP SYNC.         
004500     03 CHKP-AREA                PIC X(32)   VALUE SPACE.                 
004600     03 CHKP-ANT                 PIC S9(3)   VALUE +0   COMP-3.           
004700     03 CHKP-MAX                 PIC S9(3)   VALUE +100 COMP-3.           
004800 77  JA                          PIC X       VALUE 'J'.                   
004900 77  NEJ                         PIC X       VALUE 'N'.                   
005000     SKIP2                                                                
005100 01  FELTEXT.                                                             
005200     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
005300     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
005400                                                                          
005500 77  W09277-EOF-SW               PIC X       VALUE 'N'.                   
005600     88  END-OF-W09277                       VALUE 'J'.                   
005700     EJECT                                                                
005800 01  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
005900 01  FILLER REDEFINES DAGENS-DATUM.                                       
006000     03  DAGENS-DATUM-AAR        PIC 9(2).                                
006100     03  DAGENS-DATUM-MAANAD     PIC 9(2).                                
006200     03  DAGENS-DATUM-DAG        PIC 9(2).                                
006300     SKIP3                                                                
006400 01  ARBAREOR.                                                            
006500     03  WS-LARM-RETUR-X         PIC 9(3)  VALUE 700.                     
006600     03  FILLER REDEFINES WS-LARM-RETUR-X.                                
006700         05  WS-LARM1            PIC 9.                                   
006800         05  WS-RETUR            PIC 9(2).                                
006900     03  WS-LARM-RETUR REDEFINES WS-LARM-RETUR-X  PIC 9(3).               
007000                                                                          
007100     03  WS-ORDERED-PROD         PIC X(20).                               
007200     03  FILLER  REDEFINES WS-ORDERED-PROD.                               
007300         05  FILLER              PIC X(11).                               
007400         05  WS-IDARTNR          PIC X(9).                                
007500     03  FILLER  REDEFINES WS-ORDERED-PROD.                               
007600         05  FILLER              PIC X(11).                               
007700         05  WS-IDARTNR-NUM      PIC 9(9).                                
007710*                                                                         
007720*01  -COPY WWDCKONS                                                       
007800                                                                          
007900     EJECT                                                                
008000 01  DYNAMISKA-SUBPROGRAM.                                                
008100*                                                                         
008200     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
008300     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
008400     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
008500     03  WDATKONV                PIC X(8)    VALUE 'WDATKONV'.            
008600     EJECT                                                                
008700*    --- PARAMETRAR TILL POSTSUM                                          
008800*                                                                         
008900*01  -COPY W0005   -PRE  POSTSUM-                                         
009000     EJECT                                                                
009100*01  -COPY WDATAREA                                                       
009200     EJECT                                                                
009300 01  IN-AREA-START               PIC X(24)   VALUE                        
009400                                             'IN-AREA-START'.             
009500     SKIP2                                                                
009600                                                                          
009700*01  AREA -COPY NAPIN        -PRE IN-                                     
009800*                                                                         
009900     EJECT                                                                
010000 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
010100     SKIP3                                                                
010200 01  NYCKLAR-TILL-DLI.                                                    
010300     03  W-IDARTNR-X.                                                     
010400         05  W-IDARTNR           PIC S9(9)   VALUE ZERO COMP-3.           
010500     03  W-KDSEGKEY-X.                                                    
010600         05  W-KDSEGKEY          PIC X(1)    VALUE '1'.                   
010700                                                                          
010800     03  W-WDGX2223-X.                                                    
010900         05  W-IDHTYP-2223       PIC X(4)     VALUE '2223'.               
011000         05  W-IDANSK-2223       PIC S9(3)    VALUE ZERO COMP-3.          
011100         05  W-VALFRI-2225       PIC X(24)    VALUE LOW-VALUE.            
011200                                                                          
011300     03  W-WDGX2224-X.                                                    
011400         05  W-TISENBEK-DAG-2224 PIC S9(7)    VALUE ZERO COMP-3.          
011500         05  W-TISENBEK-KL-2224  PIC S9(7)    VALUE ZERO COMP-3.          
011600         05  W-KDLARM-2224       PIC S9(3)    VALUE ZERO COMP-3.          
011700                                                                          
011800     03  W-WDGX2231-X.                                                    
011900         05  W-IDHTYP-2231       PIC X(4)     VALUE '2231'.               
012000         05  W-VALFRI-2231       PIC X(26)    VALUE LOW-VALUE.            
012100                                                                          
012200     03  W-WDGX2232-X.                                                    
012300         05  W-IDANSK-2232       PIC S9(3)    VALUE ZERO COMP-3.          
012400         05  W-LOW-VALUE-2232    PIC X(3)     VALUE LOW-VALUE.            
012500                                                                          
012600     SKIP2                                                                
012700*    --- STATUS-KOD FRÅN IMS                                              
012800 01  STATUS-WS                   PIC XX.                                  
012900     88  SEGMENT-FINNS                       VALUE '  '.                  
013000     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
013100     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
013200     88  SEGMENT-SLUT                        VALUE 'GB'.                  
013300     88  IMS-EJ-OK                           VALUE 'XD'.                  
013400     SKIP2                                                                
013500 01  GODK-STATUSKODER.                                                    
013600     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
013700     SKIP3                                                                
013800 01  SSA1                        PIC X(64).                               
013900 01  SSA2                        PIC X(64).                               
014000     EJECT                                                                
014100*    --- IMS FUNKTIONSKODER                                               
014200*01  -COPY W0003                                                          
014300     EJECT                                                                
014400*    ---  DLI INPUT-OUTPUT AREA                                           
014500                                                                          
014600 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK601'.                      
014700 01  DLI-IO-WDK601.                                                       
014800*    03  -COPY WDK601                                                     
014900     SKIP3                                                                
015000 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK611'.                      
015100 01  DLI-IO-WDK611.                                                       
015200*    03  -COPY WDK611                                                     
015300     EJECT                                                                
015400 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDGX01'.                      
015500 01  DLI-IO-WDGX01.                                                       
015600*    03  -COPY WDGX01                                                     
015700     SKIP3                                                                
015800 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDR220'.                      
015900 01  DLI-IO-WDR220.                                                       
016000*    03  -COPY WDGX2232   -PRE WDR220-                                    
016100     EJECT                                                                
016200 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDR501'.                      
016300 01  DLI-IO-WDR501.                                                       
016400*    03  -COPY WDGX2223   -PRE WDR501-                                    
016500     SKIP3                                                                
016600 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDR550'.                      
016700 01  DLI-IO-WDR550.                                                       
016800*    03  -COPY WDGX2224   -PRE WDR550-                                    
017000                                                                          
017100     EJECT                                                                
017200 LINKAGE SECTION.                                                         
017300                                                                          
017400*01  -COPY W0009   -PRE MSG-                                              
017500                                                                          
017600*01  -COPY W0008  -PRE WDK6-                                              
017700     05  FILLER                  PIC X.                                   
017800                                                                          
017900*01  -COPY W0008  -PRE WDR2-                                              
018000     05  FILLER                  PIC X.                                   
018100                                                                          
018200*01  -COPY W0008  -PRE WDR5-                                              
018300     05  FILLER                  PIC X.                                   
018400     EJECT                                                                
018500 PROCEDURE DIVISION  USING MSG-PCB WDK6-PCB WDR2-PCB                      
018600     WDR5-PCB.                                                            
018700 MAIN SECTION.                                                            
018800     ENTRY 'DLITCBL' USING MSG-PCB WDK6-PCB WDR2-PCB                      
018900     WDR5-PCB.                                                            
019000                                                                          
019100     PERFORM A-INIT                                                       
019200     PERFORM S01-LAES-W09277                                              
019300     PERFORM UNTIL END-OF-W09277                                          
019400       IF CHKP-ANT > CHKP-MAX                                             
019500         PERFORM X-TAG-CHECKPOINT                                         
019600       END-IF                                                             
019700*      FIXA IDARTNR                                                       
019800       MOVE IN-NAP-ORDERED-PROD TO WS-ORDERED-PROD                        
019900       INSPECT WS-IDARTNR REPLACING LEADING SPACE BY ZERO                 
020000       MOVE WS-IDARTNR-NUM TO W-IDARTNR                                   
020100                                                                          
020200       PERFORM IMS-GET-WDK611                                             
020300       IF SEGMENT-FINNS                                                   
020400          PERFORM B-SKAPA-LARM                                            
020500       END-IF                                                             
020600                                                                          
020700       PERFORM S01-LAES-W09277                                            
020800     END-PERFORM                                                          
020900                                                                          
021000                                                                          
021100     PERFORM Z-FINIT                                                      
021200                                                                          
021300     MOVE ZERO TO RETURN-CODE                                             
021400     GOBACK                                                               
021500     .                                                                    
021600     EJECT                                                                
021700 A-INIT SECTION.                                                          
021800     SKIP2                                                                
021900                                                                          
022000     PERFORM IMS-RESTART                                                  
022100                                                                          
022200     OPEN INPUT W09277                                                    
022300                                                                          
022400     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
022500     .                                                                    
022600     EJECT                                                                
022700 B-SKAPA-LARM SECTION.                                                    
022800     SKIP2                                                                
022900     MOVE CLAG-IDANSK TO W-IDANSK-2232                                    
023000     PERFORM IMS-GET-WDR220                                               
023100     IF SEGMENT-FINNS                                                     
023200       MOVE WDR220-2232-IDANSK-LARM TO W-IDANSK-2223                      
023300     ELSE                                                                 
023400       MOVE ZERO TO W-IDANSK-2223                                         
023500     END-IF                                                               
023600     MOVE '2223'              TO WDR501-2223-IDHTYP                       
023700     MOVE W-IDANSK-2223       TO WDR501-2223-IDANSK                       
023800     MOVE LOW-VALUE           TO WDR501-2223-LOW-VALUE                    
023900     PERFORM IMS-ISRT-WDR501                                              
024000     PERFORM IMS-GET-WDR501                                               
024100     MOVE FUNCTION CURRENT-DATE(3:6)                                      
024200                              TO WDR550-2224-TISENBEK-DAG                 
024300     MOVE FUNCTION CURRENT-DATE(11:6)                                     
024400                              TO WDR550-2224-TISENBEK-KL                  
024500     MOVE W-IDARTNR           TO WDR550-2224-IDARTNR                      
024510     MOVE WC-CDC-SE           TO WDR550-2224-IDDC                         
024600     MOVE JA                  TO WDR550-2224-FLNYLARM                     
024700     MOVE ZERO                TO WDR550-2224-IDDISTR                      
024800                                 WDR550-2224-IDKUNDNR                     
024900     MOVE '0000000   '        TO WDR550-2224-IDKUNDRF                     
025000     MOVE 1                   TO WDR550-2224-IDLOPNR                      
025100     MOVE IN-NAP-VPER-END     TO WDR550-2224-TIREGDAT                     
025300     MOVE SPACE               TO WDR550-2224-IDTRANS                      
025400                                 WDR550-2224-KDMFSFOR                     
025500     MOVE ZERO                TO WDR550-2224-IDKR                         
025600     MOVE 777                 TO WDR550-2224-KDLARM                       
025610     MOVE SPACE               TO WDR550-2224-IDLEVNR                      
025700                                                                          
025800     PERFORM IMS-ISRT-WDR550                                              
025900                                                                          
026000     .                                                                    
026100     EJECT                                                                
026200 Z-FINIT SECTION.                                                         
026300                                                                          
026400                                                                          
026500     CLOSE W09277                                                         
026600                                                                          
026700                                                                          
026800     MOVE 'S' TO POSTSUM-OPKOD                                            
026900     CALL POSTSUM USING POSTSUM-PARM                                      
027000     .                                                                    
027100     EJECT                                                                
027200 S01-LAES-W09277  SECTION.                                                
027300     SKIP2                                                                
027400     READ W09277 INTO IN-AREA                                             
027500     AT END                                                               
027600        SET END-OF-W09277 TO TRUE                                         
027700                                                                          
027800     NOT AT END                                                           
027900        MOVE 'W09277'        TO POSTSUM-FDNAMN                            
028000        MOVE 'W09275D1'      TO POSTSUM-DDNAMN2                           
028100        MOVE 'LARM'          TO POSTSUM-TRANSTYP                          
028200        CALL POSTSUM      USING POSTSUM-PARM                              
028300                                                                          
028400     END-READ                                                             
028500     .                                                                    
028600     EJECT                                                                
028700 X-TAG-CHECKPOINT   SECTION.                                              
028800                                                                          
028900     PERFORM IMS-CHECKPOINT                                               
029000     MOVE ZERO TO CHKP-ANT                                                
029100     .                                                                    
029200     EJECT                                                                
029300* --- IMS SEKTIONER ---                                                   
029400                                                                          
029500 IMS-GET-WDK611 SECTION.                                                  
029600                                                                          
029700     STRING 'WDK601  (IDARTNR  =' W-IDARTNR-X ')'                         
029800          DELIMITED BY SIZE INTO SSA1                                     
029900     STRING 'WDK611  (KDSEGKEY =' W-KDSEGKEY-X ')'                        
030000          DELIMITED BY SIZE INTO SSA2                                     
030100     MOVE '  GE' TO GODK-STATUSKODER                                      
030200     CALL CBLTDLI USING GU  WDK6-PCB DLI-IO-WDK611 SSA1 SSA2              
030300     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
030400     PERFORM IMS-STATUSKONTROLL                                           
030500     .                                                                    
030600     EJECT                                                                
030700 IMS-GET-WDR220 SECTION.                                                  
030800                                                                          
030900     STRING 'WDR201  (WDGXKEY  =' W-WDGX2231-X ')'                        
031000          DELIMITED BY SIZE INTO SSA1                                     
031100     STRING 'WDR220  (WDGXKEY  =' W-WDGX2232-X ')'                        
031200          DELIMITED BY SIZE INTO SSA2                                     
031300     MOVE '  GE' TO GODK-STATUSKODER                                      
031400     CALL CBLTDLI USING GU  WDR2-PCB DLI-IO-WDR220 SSA1 SSA2              
031500     MOVE WDR2-STATUS-CODE TO STATUS-WS                                   
031600     PERFORM IMS-STATUSKONTROLL                                           
031700     .                                                                    
031800     EJECT                                                                
031900 IMS-GET-WDR501 SECTION.                                                  
032000                                                                          
032100     STRING 'WDR501  (WDGXKEY  =' W-WDGX2223-X ')'                        
032200          DELIMITED BY SIZE INTO SSA1                                     
032300     MOVE '  GE' TO GODK-STATUSKODER                                      
032400     CALL CBLTDLI USING GHU WDR5-PCB DLI-IO-WDR501 SSA1                   
032500     MOVE WDR5-STATUS-CODE TO STATUS-WS                                   
032600     PERFORM IMS-STATUSKONTROLL                                           
032700     .                                                                    
032800     SKIP3                                                                
032900 IMS-ISRT-WDR501 SECTION.                                                 
033000                                                                          
033100     MOVE 'WDR501   ' TO SSA1                                             
033200     MOVE '  II' TO GODK-STATUSKODER                                      
033300     CALL CBLTDLI USING ISRT WDR5-PCB DLI-IO-WDR501 SSA1                  
033400     MOVE WDR5-STATUS-CODE TO STATUS-WS                                   
033500     PERFORM IMS-STATUSKONTROLL                                           
033600     ADD +1 TO CHKP-ANT                                                   
033700     .                                                                    
033800     EJECT                                                                
033900*IMS-GET-WDR550 SECTION.                                                  
034000*                                                                         
034100*    STRING 'WDR550  (WDGXKEY  =' W-WDGXKEY-X ')'                         
034200*         DELIMITED BY SIZE INTO SSA1                                     
034300*    MOVE '  GE' TO GODK-STATUSKODER                                      
034400*    CALL CBLTDLI USING GHNP WDR5-PCB DLI-IO-WDR550 SSA1                  
034500*    MOVE WDR5-STATUS-CODE TO STATUS-WS                                   
034600*    PERFORM IMS-STATUSKONTROLL                                           
034700*    .                                                                    
034800     SKIP3                                                                
034900 IMS-ISRT-WDR550 SECTION.                                                 
035000                                                                          
035100     MOVE 'WDR550   ' TO SSA1                                             
035200     MOVE '  II' TO GODK-STATUSKODER                                      
035300     CALL CBLTDLI USING ISRT WDR5-PCB DLI-IO-WDR550 SSA1                  
035400     MOVE WDR5-STATUS-CODE TO STATUS-WS                                   
035500     PERFORM IMS-STATUSKONTROLL                                           
035600     ADD +1 TO CHKP-ANT                                                   
035700     .                                                                    
035800     EJECT                                                                
035900 IMS-RESTART SECTION.                                                     
036000     SKIP2                                                                
036100     MOVE SPACE TO CHKP-MSG-IO-AREA                                       
036200     MOVE '  ' TO GODK-STATUSKODER                                        
036300     CALL CBLTDLI USING XRST MSG-PCB                                      
036400                        CHKP-MSG-IO-AREA-LENGTH CHKP-MSG-IO-AREA          
036500                        CHKP-AREA-LENGTH CHKP-AREA                        
036600     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
036700     PERFORM IMS-STATUSKONTROLL                                           
036800     .                                                                    
036900     SKIP3                                                                
037000 IMS-CHECKPOINT SECTION.                                                  
037100     SKIP2                                                                
037200     MOVE SPACE TO CHKP-MSG-IO-AREA                                       
037300     MOVE '  XD' TO GODK-STATUSKODER                                      
037400     CALL CBLTDLI USING CHKP MSG-PCB                                      
037500                        CHKP-MSG-IO-AREA-LENGTH CHKP-MSG-IO-AREA          
037600                        CHKP-AREA-LENGTH CHKP-AREA                        
037700     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
037800     PERFORM IMS-STATUSKONTROLL                                           
037900                                                                          
038000     IF IMS-EJ-OK                                                         
038100       MOVE 'IMS-KONTROLLREGION EJ TILLGÄNGLIG' TO FELTEXT-STR            
038200       DISPLAY FELTEXT                                                    
038300       CALL FELLOG                                                        
038400     END-IF                                                               
038500     .                                                                    
038600     EJECT                                                                
038700 IMS-STATUSKONTROLL SECTION.                                              
038800     SKIP2                                                                
038900     SET STATUS-IX TO 1                                                   
039000     SEARCH GODK-STATUS                                                   
039100       AT END                                                             
039200         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
039300           DELIMITED BY SIZE INTO FELTEXT                                 
039400         DISPLAY FELTEXT                                                  
039500         CALL FELLOG                                                      
039600       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
039700         CONTINUE                                                         
039800     END-SEARCH                                                           
039900     .                                                                    
