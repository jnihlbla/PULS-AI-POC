000100 ID DIVISION.                                                             
000200                                                                          
000300 PROGRAM-ID.     W6116A00.                                                
000400*AUTHOR.         ANDERS HENRIKSSON.                                       
000500*DATE-WRITTEN.   2007-03-14.                                              
000600*                                                                         
000700*    FUNKTION:                                                            
000800*        PROGRAMMET LÄSER FIL MED ARTIKLAR MED SENASTE                    
000900*        HUVUDLEVERANTÖR OCH MATCHAR MED NÄST SENASTE VERSIONEN           
001000*        AV SAMMA FIL.                                                    
001100*                                                                         
001200*        PROGRAMMET UPPDATERAR WDK611                                     
001300*        PROGRAMMET UPPDATERAR WDT311                                     
001400*                                                                         
001500     EJECT                                                                
001600 ENVIRONMENT DIVISION.                                                    
001700 INPUT-OUTPUT SECTION.                                                    
001800 FILE-CONTROL.                                                            
001900*          --- ARTIKLAR, HUVUDLEVERANTÖR, SENASTE VERSION                 
002000     SELECT W6116A           ASSIGN TO W6116AD1.                          
002100                                                                          
002200*          --- ARTIKLAR, HUVUDLEVERANTÖR, NÄST SENASTE VERSION            
002300     SELECT W6116B           ASSIGN TO W6116AD2.                          
002400                                                                          
002500     EJECT                                                                
002600 DATA DIVISION.                                                           
002700 FILE SECTION.                                                            
002800     SKIP3                                                                
002900 FD  W6116A                                                               
003000     RECORDING       F                                                    
003100     BLOCK CONTAINS  0.                                                   
003200*01  -COPY W61167 -L.                                                     
003300                                                                          
003400 FD  W6116B                                                               
003500     RECORDING       F                                                    
003600     BLOCK CONTAINS  0.                                                   
003700*01  -COPY W61167 -L.                                                     
003800                                                                          
003900 WORKING-STORAGE SECTION.                                                 
004000                                                                          
004100 77  IDPGM                       PIC X(8)    VALUE 'W6116A00'.            
004200 77  JA                          PIC X       VALUE 'J'.                   
004300 77  NEJ                         PIC X       VALUE 'N'.                   
004400 77  W6116A-EOF                  PIC X       VALUE 'N'.                   
004500 77  W6116B-EOF                  PIC X       VALUE 'N'.                   
004600                                                                          
004700 01  WS-DAREGDAT                 PIC 9(8).                                
004800 01  WS-TID                      PIC S9(9).                               
004900 01  WS-DAPRLIST-9KOMPL-NEW      PIC S9(9).                               
005000 01  WS-DAPRLIST-9KOMPL-OLD      PIC S9(9).                               
005100                                                                          
005200 01  FELTEXT.                                                             
005300     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
005400     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
005500                                                                          
005600 01  CHKP-VAR.                                                            
005700   03  CHKP-MSG-IO-AREA-LENGTH   PIC S9(9)   VALUE +32 COMP SYNC.         
005800   03  CHKP-MSG-IO-AREA          PIC X(32)   VALUE SPACE.                 
005900   03  CHKP-AREA-LENGTH          PIC S9(9)   VALUE +32 COMP SYNC.         
006000   03  CHKP-AREA                 PIC X(32)   VALUE SPACE.                 
006100   03  CHKP-ANT                  PIC S9(3)   VALUE +0.                    
006200   03  CHKP-MAX                  PIC S9(3)   VALUE +50.                   
006300                                                                          
006400 01  DYNAMISKA-SUBPROGRAM.                                                
006500*                                                                         
006600     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
006700     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
006800     03  DATKORT                 PIC X(8)    VALUE 'DATKORT'.             
006900     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
007000                                                                          
007100     EJECT                                                                
007200*    --- VALID IDDC CODES                                                 
007300*                                                                         
007400*01  -COPY W0005   -PRE  POSTSUM-                                         
007500     EJECT                                                                
007600                                                                          
007700 01  IN-AREA-START               PIC X(24)   VALUE                        
007800                                 'IN-AREA-START  '.                       
007900*01  AREA -COPY W61167     -PRE NEW-                                      
008000     EJECT                                                                
008100                                                                          
008200 01  IN-AREA2-START              PIC X(24)   VALUE                        
008300                                 'IN-AREA2-START '.                       
008400*01  AREA -COPY W61167     -PRE OLD-                                      
008500     EJECT                                                                
008600                                                                          
008700 01  FILLER                    PIC X(16) VALUE 'IMS-WS'.                  
008800                                                                          
008900 01  W-WDK601.                                                            
009000     03  W-IDARTNR-X.                                                     
009100         05 W-IDARTNR            PIC S9(9)  VALUE ZERO COMP-3.            
009200     SKIP2                                                                
009300                                                                          
009310 01  W-WDT311.                                                            
009320     03  W-IDLAND-X.                                                      
009330         05 W-IDLAND             PIC X(2)   VALUE SPACE.                  
009340     SKIP2                                                                
009350                                                                          
009400*    --- STATUS-KOD FRÅN IMS                                              
009500 01  STATUS-WS                   PIC XX.                                  
009600     88  SEGMENT-FINNS                       VALUE '  '.                  
009700     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
009800     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
009900     88  SEGMENT-SLUT                        VALUE 'GB'.                  
010000     88  IMS-EJ-OK                           VALUE 'XD'.                  
010100     SKIP2                                                                
010200 01  GODK-STATUSKODER.                                                    
010300     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
010400     SKIP3                                                                
010500 01  SSA1                        PIC X(128).                              
010600 01  SSA2                        PIC X(128).                              
010700 01  SSA3                        PIC X(128).                              
010800                                                                          
010801 01  -COPY WWPRODSL                                                       
010802     EJECT                                                                
010900                                                                          
011000*    --- IMS FUNKTIONSKODER                                               
011100*01  -COPY W0003                                                          
011200     EJECT                                                                
011300*    ---  DLI INPUT-OUTPUT AREA                                           
011400 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA'.         
011500     SKIP3                                                                
011600 01  DLI-IO-AREA.                                                         
011700     03  IO-AREA                 PIC X(900)  VALUE SPACE.                 
011800     SKIP3                                                                
011900     03  WDK601 REDEFINES IO-AREA.                                        
012000*      05  -COPY WDK601                                                   
012100     03  WDK611 REDEFINES IO-AREA.                                        
012200*      05  -COPY WDK611                                                   
012300     EJECT                                                                
012400 01  DLI-IO-WDT301.                                                       
012700*    03  -COPY WDT301                                                     
012710 01  DLI-IO-WDT311.                                                       
012720*    03  -COPY WDT311                                                     
012800     EJECT                                                                
012900 LINKAGE SECTION.                                                         
013000                                                                          
013100*01  -COPY W0009  -PRE MSG-                                               
013200                                                                          
013300*01  -COPY W0008  -PRE WDK6-                                              
013301     05  FILLER                  PIC X.                                   
013310                                                                          
013320*01  -COPY W0008  -PRE WDT3-                                              
013400     05  FILLER                  PIC X.                                   
013500     EJECT                                                                
013600 PROCEDURE DIVISION  USING MSG-PCB WDK6-PCB WDT3-PCB.                     
013700 MAIN SECTION.                                                            
013800     ENTRY 'DLITCBL' USING MSG-PCB WDK6-PCB WDT3-PCB.                     
013900                                                                          
014000     PERFORM A-INIT                                                       
014100                                                                          
014200     PERFORM S01-LAES-W6116A                                              
014300     PERFORM S01-LAES-W6116B                                              
014400     PERFORM UNTIL W6116A-EOF  = JA                                       
014500       IF NEW-IDARTNR < OLD-IDARTNR                                       
014600         PERFORM B-BEHANDLA-NEW-IDARTNR                                   
014700         PERFORM S01-LAES-W6116A                                          
014800       ELSE                                                               
014900         IF NEW-IDARTNR > OLD-IDARTNR                                     
015000**** WE DON'T HANDLE DELETED ITEMS                                        
015100           PERFORM S01-LAES-W6116B                                        
015200         ELSE                                                             
015300           IF NEW-IDARTNR = OLD-IDARTNR                                   
015310             IF NEW-IDLEVNR = OLD-IDLEVNR                                 
015400               PERFORM C-BEHANDLA-SAME-IDARTNR                            
015410             ELSE                                                         
015411               PERFORM D-BEHANDLA-NEW-IDLEVNR                             
015420             END-IF                                                       
015500           END-IF                                                         
015600           PERFORM S01-LAES-W6116A                                        
015700           PERFORM S01-LAES-W6116B                                        
015800         END-IF                                                           
015900       END-IF                                                             
016000       IF CHKP-ANT > CHKP-MAX                                             
016100         PERFORM X-TAG-CHECKPOINT                                         
016200       END-IF                                                             
016300     END-PERFORM                                                          
016400                                                                          
016500     PERFORM Z-FINIT                                                      
016600                                                                          
016700     MOVE ZERO TO RETURN-CODE                                             
016800     GOBACK                                                               
016900     .                                                                    
017000     EJECT                                                                
017100 A-INIT SECTION.                                                          
017200     PERFORM IMS-RESTART                                                  
017300                                                                          
017400     OPEN INPUT  W6116A                                                   
017500     OPEN INPUT  W6116B                                                   
017600                                                                          
017700     MOVE ZERO TO CHKP-ANT                                                
017800     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
017900     .                                                                    
018000     EJECT                                                                
018100                                                                          
018200 B-BEHANDLA-NEW-IDARTNR  SECTION.                                         
018300     PERFORM S05-READ-WDK6                                                
018400     IF SEGMENT-FINNS                                                     
018500       IF NEW-KDFPKPRI = 'Y'                                              
018600         PERFORM S07-UPDATE-WDK6-WDT3                                     
018700       END-IF                                                             
018800       IF NEW-KDFPKPRI = 'N'                                              
018900         PERFORM S06-UPDATE-WDK6-WDT3                                     
019000       END-IF                                                             
019100     END-IF                                                               
019200     .                                                                    
019300     EJECT                                                                
019400                                                                          
019500 C-BEHANDLA-SAME-IDARTNR SECTION.                                         
019600     MOVE NEW-DAPRLIST-9KOMPL TO WS-DAPRLIST-9KOMPL-NEW                   
019700     MOVE OLD-DAPRLIST-9KOMPL TO WS-DAPRLIST-9KOMPL-OLD                   
019800     IF WS-DAPRLIST-9KOMPL-NEW < WS-DAPRLIST-9KOMPL-OLD                   
019900       PERFORM S05-READ-WDK6                                              
019910       IF SEGMENT-FINNS                                                   
020000         IF OLD-KDFPKPRI = 'Y' AND NEW-KDFPKPRI = 'Y'                     
020100           CONTINUE                                                       
020200         END-IF                                                           
020300         IF OLD-KDFPKPRI = 'Y' AND NEW-KDFPKPRI = 'N'                     
020400           PERFORM S06-UPDATE-WDK6-WDT3                                   
020500         END-IF                                                           
020600         IF OLD-KDFPKPRI = 'N' AND NEW-KDFPKPRI = 'Y'                     
020700           PERFORM S07-UPDATE-WDK6-WDT3                                   
020800         END-IF                                                           
020900         IF OLD-KDFPKPRI = 'N' AND NEW-KDFPKPRI = 'N'                     
021000           CONTINUE                                                       
021100         END-IF                                                           
021200         IF OLD-KDFPKPRI = '?' AND NEW-KDFPKPRI = 'Y'                     
021300           PERFORM S07-UPDATE-WDK6-WDT3                                   
021400         END-IF                                                           
021500         IF OLD-KDFPKPRI = '?' AND NEW-KDFPKPRI = 'N'                     
021600           PERFORM S06-UPDATE-WDK6-WDT3                                   
021700         END-IF                                                           
021800         IF OLD-KDFPKPRI = '?' AND NEW-KDFPKPRI = '?'                     
021900           CONTINUE                                                       
022000         END-IF                                                           
022100         IF OLD-KDFPKPRI = ' ' AND NEW-KDFPKPRI = 'Y'                     
022200           PERFORM S07-UPDATE-WDK6-WDT3                                   
022300         END-IF                                                           
022400         IF OLD-KDFPKPRI = ' ' AND NEW-KDFPKPRI = 'N'                     
022500           PERFORM S06-UPDATE-WDK6-WDT3                                   
022600         END-IF                                                           
022700         IF OLD-KDFPKPRI = ' ' AND NEW-KDFPKPRI = '?'                     
022800           CONTINUE                                                       
022900         END-IF                                                           
023000         IF OLD-KDFPKPRI = ' ' AND NEW-KDFPKPRI = ' '                     
023100           CONTINUE                                                       
023200         END-IF                                                           
023210       END-IF                                                             
023300     ELSE                                                                 
023400****   WE DON'T HANDLE DELETED ITEMS OR UNCHANGED ITEMS                   
023500       CONTINUE                                                           
023600     END-IF                                                               
023700     .                                                                    
023800     EJECT                                                                
023900                                                                          
024000 D-BEHANDLA-NEW-IDLEVNR SECTION.                                          
024001*    MOVE NEW-DAPRLIST-9KOMPL TO WS-DAPRLIST-9KOMPL-NEW                   
024002*    MOVE OLD-DAPRLIST-9KOMPL TO WS-DAPRLIST-9KOMPL-OLD                   
024003*    IF WS-DAPRLIST-9KOMPL-NEW < WS-DAPRLIST-9KOMPL-OLD                   
024004       PERFORM S05-READ-WDK6                                              
024005       IF SEGMENT-FINNS                                                   
024021         IF NEW-KDFPKPRI = 'Y'                                            
024022           PERFORM S07-UPDATE-WDK6-WDT3                                   
024023         END-IF                                                           
024036         IF NEW-KDFPKPRI = 'N'                                            
024037           PERFORM S06-UPDATE-WDK6-WDT3                                   
024038         END-IF                                                           
024042         IF NEW-KDFPKPRI = '?'                                            
024043           PERFORM S07-UPDATE-WDK6-WDT3                                   
024044         END-IF                                                           
024051         IF NEW-KDFPKPRI = ' '                                            
024052           PERFORM S07-UPDATE-WDK6-WDT3                                   
024054         END-IF                                                           
024055       END-IF                                                             
024056*    ELSE                                                                 
024057**** WE DON'T HANDLE DELETED ITEMS OR UNCHANGED ITEMS                     
024058*      CONTINUE                                                           
024059*    END-IF                                                               
024060     .                                                                    
024061     EJECT                                                                
024062                                                                          
024070 Z-FINIT SECTION.                                                         
024100     CLOSE W6116A                                                         
024200     CLOSE W6116B                                                         
024300     MOVE 'S' TO POSTSUM-OPKOD                                            
024400     CALL POSTSUM USING POSTSUM-PARM                                      
024500     .                                                                    
024600     EJECT                                                                
024700                                                                          
024800 S01-LAES-W6116A  SECTION.                                                
024900     READ W6116A INTO NEW-AREA                                            
025000     AT END                                                               
025100        MOVE JA          TO W6116A-EOF                                    
025200     NOT AT END                                                           
025300        MOVE 'IN  '      TO POSTSUM-TRANSTYP                              
025400        MOVE 'W6116A'    TO POSTSUM-FDNAMN                                
025500        MOVE 'W6116AD1'  TO POSTSUM-DDNAMN2                               
025600        CALL POSTSUM USING POSTSUM-PARM                                   
025700     END-READ                                                             
025800     .                                                                    
025900     EJECT                                                                
026000                                                                          
026100 S01-LAES-W6116B  SECTION.                                                
026200     READ W6116B INTO OLD-AREA                                            
026300     AT END                                                               
026400        MOVE +999999999  TO OLD-IDARTNR                                   
026500        MOVE JA          TO W6116B-EOF                                    
026600     NOT AT END                                                           
026700        MOVE 'IN  '      TO POSTSUM-TRANSTYP                              
026800        MOVE 'W6116B'    TO POSTSUM-FDNAMN                                
026900        MOVE 'W6116AD2'  TO POSTSUM-DDNAMN2                               
027000        CALL POSTSUM USING POSTSUM-PARM                                   
027100     END-READ                                                             
027200     .                                                                    
027300     EJECT                                                                
027400                                                                          
027500 S05-READ-WDK6    SECTION.                                                
027600     MOVE NEW-IDARTNR TO W-IDARTNR-X                                      
027700     PERFORM IMS-GU-WDK601                                                
027710     IF SEGMENT-FINNS                                                     
027800       PERFORM IMS-GNP-WDK611                                             
027810     END-IF                                                               
027900     .                                                                    
028000     EJECT                                                                
028100                                                                          
028200 S06-UPDATE-WDK6-WDT3  SECTION.                                           
028300**** UPDATE WDK611                                                        
028400     MOVE NEW-IDARTNR TO W-IDARTNR-X                                      
028500     PERFORM IMS-GU-WDK601                                                
028601     MOVE ART-KDPRODSL TO TEST-KDPRODSL                                   
028602**** IGNORE CHANGE TO 06 FOR LYNK                                         
028603     IF NOT KDPRODSL-LYNK                                                 
028604       PERFORM IMS-GHNP-WDK611                                            
028700       IF SEGMENT-FINNS                                                   
028800         MOVE 06 TO CLAG-BEFT                                             
028810         MOVE ZERO TO CLAG-KDFORP                                         
028900         PERFORM IMS-REPL-WDK611                                          
029000         ADD +1 TO CHKP-ANT                                               
029100       END-IF                                                             
029110                                                                          
029120       PERFORM IMS-GU-WDT301                                              
029130       IF SEGMENT-SAKNAS                                                  
029140          MOVE W-IDARTNR TO FART-IDARTNR                                  
029150          PERFORM IMS-ISRT-WDT301                                         
029160       END-IF                                                             
029200**** INSERT WDT311                                                        
029210       MOVE 'SE'     TO W-IDLAND                                          
029300       PERFORM IMS-GU-WDT311                                              
029400       MOVE 'W6116A' TO FPCK-IDUSER                                       
029500       MOVE 06       TO FPCK-BEFT                                         
029510       MOVE ZERO     TO FPCK-KDFORP                                       
029600       IF NEW-KDFPKPRI = 'Y'                                              
029700         MOVE 'PACKING INCLUDED IN PRICE = Y'  TO FPCK-TEBEFT(1)          
029800       ELSE                                                               
029900         MOVE 'PACKING INCLUDED IN PRICE = N'  TO FPCK-TEBEFT(1)          
030000       END-IF                                                             
030100       MOVE SPACE TO FPCK-TEBEFT(2)                                       
030200       MOVE SPACE TO FPCK-TEBEFT(3)                                       
030210       MOVE SPACE TO FPCK-TEBEFT(4)                                       
030220       MOVE SPACE TO FPCK-TEBEFT(5)                                       
030300       PERFORM S08-CONVERT-TO-9KOMPL                                      
030310       MOVE 'SE'  TO FPCK-IDLANDX2                                        
030400       PERFORM IMS-ISRT-WDT311                                            
030500       ADD +1 TO CHKP-ANT                                                 
030510     END-IF                                                               
030600     .                                                                    
030700     EJECT                                                                
030800                                                                          
030900 S07-UPDATE-WDK6-WDT3   SECTION.                                          
031000**** UPDATE WDK611                                                        
031100     MOVE NEW-IDARTNR TO W-IDARTNR-X                                      
031200     PERFORM IMS-GU-WDK601                                                
031300     MOVE ART-KDPRODSL TO TEST-KDPRODSL                                   
031310**** IGNORE CHANGE TO 07 FOR LYNK                                         
031400     IF NOT KDPRODSL-LYNK                                                 
031500       PERFORM IMS-GHNP-WDK611                                            
031600       IF SEGMENT-FINNS                                                   
031700         MOVE 07 TO CLAG-BEFT                                             
031800         MOVE ZERO TO CLAG-KDFORP                                         
031900         PERFORM IMS-REPL-WDK611                                          
032000         ADD +1 TO CHKP-ANT                                               
032100       END-IF                                                             
032200                                                                          
032300       PERFORM IMS-GU-WDT301                                              
032400       IF SEGMENT-SAKNAS                                                  
032500          MOVE W-IDARTNR TO FART-IDARTNR                                  
032600          PERFORM IMS-ISRT-WDT301                                         
032700       END-IF                                                             
032800**** INSERT WDT311                                                        
032900       MOVE 'SE'     TO W-IDLAND                                          
033000       PERFORM IMS-GU-WDT311                                              
033100       MOVE 'W6116A' TO FPCK-IDUSER                                       
033200       MOVE 07       TO FPCK-BEFT                                         
033300       MOVE ZERO     TO FPCK-KDFORP                                       
033400       IF NEW-KDFPKPRI = 'Y'                                              
033500         MOVE 'PACKING INCLUDED IN PRICE = Y'  TO FPCK-TEBEFT(1)          
033600       ELSE                                                               
033700         MOVE 'PACKING INCLUDED IN PRICE = N'  TO FPCK-TEBEFT(1)          
033800       END-IF                                                             
033900       MOVE SPACE TO FPCK-TEBEFT(2)                                       
034000       MOVE SPACE TO FPCK-TEBEFT(3)                                       
034100       MOVE SPACE TO FPCK-TEBEFT(4)                                       
034200       MOVE SPACE TO FPCK-TEBEFT(5)                                       
034300       PERFORM S08-CONVERT-TO-9KOMPL                                      
034400       MOVE 'SE'  TO FPCK-IDLANDX2                                        
034500       PERFORM IMS-ISRT-WDT311                                            
034600       ADD +1 TO CHKP-ANT                                                 
034601     END-IF                                                               
034602     .                                                                    
034603     EJECT                                                                
034604                                                                          
034605 S08-CONVERT-TO-9KOMPL SECTION.                                           
034606     MOVE FUNCTION CURRENT-DATE(1:8) TO WS-DAREGDAT                       
034607     COMPUTE FPCK-DAREGDAT-9KOMPL = 999999999 - WS-DAREGDAT               
034608     ACCEPT WS-TID FROM TIME                                              
034609     COMPUTE FPCK-TIKLOCK-9KOMPL = 999999999 - WS-TID                     
034610     .                                                                    
034611     EJECT                                                                
034612                                                                          
034613 X-TAG-CHECKPOINT   SECTION.                                              
034614     PERFORM IMS-CHECKPOINT                                               
034615     MOVE ZERO TO CHKP-ANT                                                
034700     .                                                                    
034800     EJECT                                                                
034900                                                                          
035000* --- IMS SEKTIONER ---                                                   
035100     SKIP3                                                                
035200                                                                          
035300 IMS-GU-WDK601 SECTION.                                                   
035400     STRING 'WDK601  (IDARTNR  =' W-IDARTNR-X ')'                         
035500            DELIMITED BY SIZE INTO SSA1                                   
035600     MOVE '  GE' TO GODK-STATUSKODER                                      
035700     CALL CBLTDLI USING GU WDK6-PCB DLI-IO-AREA SSA1                      
035800     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
035900     PERFORM IMS-STATUSKONTROLL                                           
036000     .                                                                    
036100     SKIP3                                                                
036200                                                                          
036300 IMS-GNP-WDK611 SECTION.                                                  
036400     MOVE 'WDK611  (KDSEGKEY =1)' TO SSA1                                 
036500     MOVE '  GE' TO GODK-STATUSKODER                                      
036600     CALL CBLTDLI USING GNP WDK6-PCB DLI-IO-AREA SSA1                     
036700     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
036800     PERFORM IMS-STATUSKONTROLL                                           
036900     .                                                                    
037000     SKIP3                                                                
037100                                                                          
037200 IMS-GHNP-WDK611 SECTION.                                                 
037300     MOVE 'WDK611  (KDSEGKEY =1)' TO SSA1                                 
037400     MOVE '  GE' TO GODK-STATUSKODER                                      
037500     CALL CBLTDLI USING GHNP WDK6-PCB DLI-IO-AREA SSA1                    
037600     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
037700     PERFORM IMS-STATUSKONTROLL                                           
037800     SKIP3                                                                
037900     .                                                                    
038000     EJECT                                                                
038100                                                                          
038200 IMS-REPL-WDK611 SECTION.                                                 
038300     MOVE '  ' TO GODK-STATUSKODER                                        
038400     CALL CBLTDLI USING REPL WDK6-PCB DLI-IO-AREA                         
038500     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
038600     PERFORM IMS-STATUSKONTROLL                                           
038700     .                                                                    
038800     EJECT                                                                
038900                                                                          
039000 IMS-GU-WDT311          SECTION.                                          
039100     STRING 'WDT301  (IDARTNR  =' W-IDARTNR-X ')'                         
039200          DELIMITED BY SIZE INTO SSA1                                     
039210     STRING 'WDT311  (IDLAND   =' W-IDLAND-X ')'                          
039220          DELIMITED BY SIZE INTO SSA2                                     
039400     MOVE '  GE' TO GODK-STATUSKODER                                      
039500     CALL CBLTDLI USING GU  WDT3-PCB DLI-IO-WDT311 SSA1 SSA2              
039600     MOVE WDT3-STATUS-CODE TO STATUS-WS                                   
039700     PERFORM IMS-STATUSKONTROLL                                           
039800     .                                                                    
039900     SKIP3                                                                
040000                                                                          
040010 IMS-GU-WDT301          SECTION.                                          
040020     STRING 'WDT301  (IDARTNR  =' W-IDARTNR-X ')'                         
040030          DELIMITED BY SIZE INTO SSA1                                     
040060     MOVE '  GE' TO GODK-STATUSKODER                                      
040070     CALL CBLTDLI USING GU  WDT3-PCB DLI-IO-WDT301 SSA1                   
040080     MOVE WDT3-STATUS-CODE TO STATUS-WS                                   
040090     PERFORM IMS-STATUSKONTROLL                                           
040091     .                                                                    
040092     SKIP3                                                                
040093                                                                          
040100 IMS-ISRT-WDT301 SECTION.                                                 
040400     MOVE 'WDT301' TO SSA1                                                
040500     MOVE '    ' TO GODK-STATUSKODER                                      
040600     CALL CBLTDLI USING ISRT WDT3-PCB DLI-IO-WDT301 SSA1                  
040700     MOVE WDT3-STATUS-CODE TO STATUS-WS                                   
040800     PERFORM IMS-STATUSKONTROLL                                           
040900     .                                                                    
041000     EJECT                                                                
041100                                                                          
041110 IMS-ISRT-WDT311 SECTION.                                                 
041120     STRING 'WDT301  (IDARTNR  =' W-IDARTNR-X ')'                         
041130          DELIMITED BY SIZE INTO SSA1                                     
041140     MOVE 'WDT311' TO SSA2                                                
041150     MOVE '    ' TO GODK-STATUSKODER                                      
041160     CALL CBLTDLI USING ISRT WDT3-PCB DLI-IO-WDT311 SSA1 SSA2             
041170     MOVE WDT3-STATUS-CODE TO STATUS-WS                                   
041180     PERFORM IMS-STATUSKONTROLL                                           
041190     .                                                                    
041191     EJECT                                                                
041192                                                                          
041200 IMS-RESTART SECTION.                                                     
041300     MOVE SPACE TO CHKP-MSG-IO-AREA                                       
041400     MOVE '  ' TO GODK-STATUSKODER                                        
041500     CALL CBLTDLI USING XRST MSG-PCB                                      
041600                        CHKP-MSG-IO-AREA-LENGTH CHKP-MSG-IO-AREA          
041700                        CHKP-AREA-LENGTH CHKP-AREA                        
041800     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
041900     PERFORM IMS-STATUSKONTROLL                                           
042000     .                                                                    
042100     SKIP3                                                                
042200                                                                          
042300 IMS-CHECKPOINT SECTION.                                                  
042400     MOVE SPACE TO CHKP-MSG-IO-AREA                                       
042500     MOVE '  XD' TO GODK-STATUSKODER                                      
042600     CALL CBLTDLI USING CHKP MSG-PCB                                      
042700                        CHKP-MSG-IO-AREA-LENGTH CHKP-MSG-IO-AREA          
042800                        CHKP-AREA-LENGTH CHKP-AREA                        
042900     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
043000     PERFORM IMS-STATUSKONTROLL                                           
043100                                                                          
043200     IF IMS-EJ-OK                                                         
043300       MOVE 'IMS-KONTROLLREGION EJ TILLGÄNGLIG' TO FELTEXT-STR            
043400       DISPLAY FELTEXT                                                    
043500       CALL FELLOG                                                        
043600     END-IF                                                               
043700     .                                                                    
043800     SKIP3                                                                
043900                                                                          
044000 IMS-STATUSKONTROLL SECTION.                                              
044100     SET STATUS-IX TO 1                                                   
044200     SEARCH GODK-STATUS                                                   
044300       AT END                                                             
044400         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
044500           DELIMITED BY SIZE INTO FELTEXT                                 
044600         DISPLAY FELTEXT                                                  
044700         CALL FELLOG                                                      
044800       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
044900         CONTINUE                                                         
045000     END-SEARCH                                                           
045100     .                                                                    
