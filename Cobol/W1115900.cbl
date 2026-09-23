000100 ID DIVISION.                                                             
000200                                                                          
000300 PROGRAM-ID.     W1115900.                                                
000400 AUTHOR.         JOHAN NIHLBLAD                                           
000500 DATE-WRITTEN.   OKT 09                                                   
000600                                                                          
000700*    FUNKTION:                                                            
000800*                                                                         
000900*        LÄSER FIL FÖR UPPDATERING FRÅN SVHC OCH KOMPLETTERAR             
001000*        DENNA MED INFO.                                                  
001100*                                                                         
001200*    INDATA .                                                             
001300*                                                                         
001400*        FIL FRÅN SVHC                                                    
001500*                                                                         
001600*    UTDATA .                                                             
001700*                                                                         
001800*        UTFIL W11159  MED IDARTNR KDERS TIERSDAT LAGERSALDON             
001900                                                                          
002000     EJECT                                                                
002100 ENVIRONMENT DIVISION.                                                    
002200     SKIP2                                                                
002300 INPUT-OUTPUT SECTION.                                                    
002400                                                                          
002500 FILE-CONTROL.                                                            
002600                                                                          
002700*                         INFILL MED ARTIKELNUMMER                        
002800     SELECT W11158                     ASSIGN TO W11159D1.                
002900*                                                                         
003000*                         UTFIL KOMPLETTERAD MED DATA                     
003100     SELECT W11159                     ASSIGN TO W11159D2.                
003200     EJECT                                                                
003300 DATA DIVISION.                                                           
003400     SKIP3                                                                
003500 FILE SECTION.                                                            
003600     SKIP2                                                                
003700 FD  W11158                                                               
003800     RECORDING       V                                                    
003900     BLOCK CONTAINS  0.                                                   
004000     SKIP2                                                                
004100 01  INFIL                       PIC X(252).                              
004200     SKIP2                                                                
004300 FD  W11159                                                               
004400     RECORDING       V                                                    
004500     BLOCK CONTAINS  0.                                                   
004600     SKIP2                                                                
004700 01  UT-POST                     PIC X(153).                              
004800     EJECT                                                                
004900 WORKING-STORAGE SECTION.                                                 
005000                                                                          
005100                                                                          
005200*    -- CHECKED BY WY2000                                                 
005300 77  IDPGM                       PIC X(8)    VALUE 'W1115900'.            
005400 77  W-CHKP-MAX                  PIC S9(3)   VALUE +200  COMP-3.          
005500 77  W-CHKP-RAEKNARE             PIC S9(3)   VALUE +0    COMP-3.          
005600 77  W-MSG-IO-AREA-LENGTH        PIC S9(9)   VALUE +32  COMP SYNC.        
005700 77  W-MSG-IO-AREA               PIC X(32)   VALUE SPACE.                 
005800 77  W-CHKP-AREA-1-LENGTH        PIC S9(9)   VALUE +32  COMP SYNC.        
005900 77  W-CHKP-AREA-1               PIC X(32)   VALUE SPACE.                 
006000 77  JA                          PIC X       VALUE 'J'.                   
006100 77  NEJ                         PIC X       VALUE 'N'.                   
006200 77  INPUT-RAETT                 PIC X       VALUE SPACE.                 
006300 77  WS-TIUPPDAT                 PIC S9(7) COMP-3 VALUE ZERO.             
006400 77  WS-TIUPPTID                 PIC S9(9) COMP-3 VALUE ZERO.             
006500                                                                          
006600 77  W11158-EOF-SW               PIC X       VALUE 'N'.                   
006700     88  END-OF-W11158                       VALUE 'J'.                   
006800                                                                          
006900 77  INDATA-SW                   PIC X       VALUE 'J'.                   
007000     88  INDATA-OK                           VALUE 'J'.                   
007100     88  INDATA-FEL                          VALUE 'N'.                   
007200                                                                          
007300 01  WS-IDARTNR                  PIC X(9)  VALUE SPACE.                   
007400*01  IDARTNR-WS REDEFINES WS-IDARTNR PIC 9(9).                            
007500                                                                          
007600 01  WS-KVLS-JP                  PIC 9(9)    VALUE ZERO.                  
007700 01  WS-KVLS-AU                  PIC 9(9)    VALUE ZERO.                  
007800 01  WS-KVLS-CA                  PIC 9(9)    VALUE ZERO.                  
007900 01  WS-KVLS-USA                 PIC 9(9)    VALUE ZERO.                  
008000 01  WS-KVLS-EU                  PIC 9(9)    VALUE ZERO.                  
008100 01  WS-KVLS-CN                  PIC 9(9)    VALUE ZERO.                  
008200 01  WS-KVLS-IN                  PIC 9(9)    VALUE ZERO.                  
008210 01  WS-KVLS-KR                  PIC 9(9)    VALUE ZERO.                  
008220 01  WS-KVLS-AE                  PIC 9(9)    VALUE ZERO.                  
008230 01  WS-KVLS-TR                  PIC 9(9)    VALUE ZERO.                  
008231 01  WS-KVLS-BR                  PIC 9(9)    VALUE ZERO.                  
008232 01  WS-KVLS-MX                  PIC 9(9)    VALUE ZERO.                  
008240 01  WS-KVLS-MY                  PIC 9(9)    VALUE ZERO.                  
008250 01  WS-KVLS-TH                  PIC 9(9)    VALUE ZERO.                  
008260 01  WS-KVLS-TW                  PIC 9(9)    VALUE ZERO.                  
008270 01  WS-KVLS-ZA                  PIC 9(9)    VALUE ZERO.                  
008300 01  FELTEXT.                                                             
008400     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
008500     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
008600     EJECT                                                                
008700 01  FILLER                      PIC X(8)    VALUE 'FIELDS  '.            
008800 01  FIELD-LENGTHS.                                                       
008900     03 LIDARTNR                 PIC S9(4)   BINARY.                      
009000*      --- VALID IDDC CODES                                               
009100*                                                                         
009200*01    -COPY WWDC99                                                       
009300       EJECT                                                              
009400*                                                                         
009500                                                                          
009600 01  DYNAMISKA-SUBPROGRAM.                                                
009700*                                                                         
009800     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
009900     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
010000     03  W009CIA                 PIC X(8)    VALUE 'W009CIA '.            
010100     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
010200     03  W006KOM                 PIC X(8)    VALUE 'W006KOM'.             
010300     EJECT                                                                
010400*    --- PARAMETRAR TILL POSTSUM                                          
010500*                                                                         
010600*01  -COPY W009CIA                                                        
010700     EJECT                                                                
010800*01  -COPY W0005 -PRE  POSTSUM-                                           
010900     EJECT                                                                
011000 01  IN-AREA-START               PIC X(24)   VALUE                        
011100                                             'IN-AREA-START'.             
011200     SKIP2                                                                
011300*                                                                         
011400 01  IN-AREA.                                                             
011500     03  FILLER               PIC X(256)     VALUE SPACE.                 
011600                                                                          
011700     EJECT                                                                
011800 01  UT-AREA-START               PIC X(24)   VALUE                        
011900                                             'UT-AREA-START'.             
012000     SKIP2                                                                
012100*                                                                         
012200 01  UT-AREA.                                                             
012300     03  UT-IDARTNR           PIC 9(9)       VALUE ZERO.                  
012400     03  FILLER               PIC X          VALUE ';'.                   
012500     03  UT-KDERS             PIC 9(2)       VALUE ZERO.                  
012600     03  FILLER               PIC X          VALUE ';'.                   
012700     03  UT-TIERSDAT          PIC 9(5)       VALUE ZERO.                  
012800     03  FILLER               PIC X          VALUE ';'.                   
012900     03  UT-KVLS-CDC          PIC 9(9)       VALUE ZERO.                  
013000     03  FILLER               PIC X          VALUE ';'.                   
013100     03  UT-KVLS-EU           PIC 9(9)       VALUE ZERO.                  
013200     03  FILLER               PIC X          VALUE ';'.                   
013300     03  UT-KVLS-JP           PIC 9(9)       VALUE ZERO.                  
013400     03  FILLER               PIC X          VALUE ';'.                   
013500     03  UT-KVLS-AU           PIC 9(9)       VALUE ZERO.                  
013600     03  FILLER               PIC X          VALUE ';'.                   
013700     03  UT-KVLS-USA          PIC 9(9)       VALUE ZERO.                  
013800     03  FILLER               PIC X          VALUE ';'.                   
013900     03  UT-KVLS-CA           PIC 9(9)       VALUE ZERO.                  
014000     03  FILLER               PIC X          VALUE ';'.                   
014100     03  UT-KVLS-CN           PIC 9(9)       VALUE ZERO.                  
014200     03  FILLER               PIC X          VALUE ';'.                   
014300     03  UT-KVLS-IN           PIC 9(9)       VALUE ZERO.                  
014310     03  FILLER               PIC X          VALUE ';'.                   
014320     03  UT-KVLS-KR           PIC 9(9)       VALUE ZERO.                  
014330     03  FILLER               PIC X          VALUE ';'.                   
014340     03  UT-KVLS-AE           PIC 9(9)       VALUE ZERO.                  
014350     03  FILLER               PIC X          VALUE ';'.                   
014360     03  UT-KVLS-TR           PIC 9(9)       VALUE ZERO.                  
014370     03  FILLER               PIC X          VALUE ';'.                   
014380     03  UT-KVLS-MY           PIC 9(9)       VALUE ZERO.                  
014390     03  FILLER               PIC X          VALUE ';'.                   
014391     03  UT-KVLS-TH           PIC 9(9)       VALUE ZERO.                  
014392     03  FILLER               PIC X          VALUE ';'.                   
014393     03  UT-KVLS-TW           PIC 9(9)       VALUE ZERO.                  
014394     03  FILLER               PIC X          VALUE ';'.                   
014395     03  UT-KVLS-BR           PIC 9(9)       VALUE ZERO.                  
014396     03  FILLER               PIC X          VALUE ';'.                   
014397     03  UT-KVLS-MX           PIC 9(9)       VALUE ZERO.                  
014398     03  FILLER               PIC X          VALUE ';'.                   
014399     03  UT-KVLS-ZA           PIC 9(9)       VALUE ZERO.                  
014400     EJECT                                                                
014500 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
014600                                                                          
014700 01  NYCKLAR-TILL-DLI.                                                    
014800     03  W-IDARTNR-X.                                                     
014900         05  W-IDARTNR           PIC S9(9) VALUE ZERO COMP-3.             
015000                                                                          
015100     03  W-KDSEGKEY-X.                                                    
015200         05  W-KDSEGKEY          PIC X       VALUE '1'.                   
015300                                                                          
015400     03  W-IDDC-X.                                                        
015500         05  W-IDDC              PIC X(2)    VALUE SPACE.                 
015600                                                                          
015700     SKIP3                                                                
015800*    --- STATUS-KOD FRÅN IMS                                              
015900 01  STATUS-WS                   PIC XX.                                  
016000     88  SEGMENT-FINNS                       VALUE '  '.                  
016100     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
016200     88  SEGMENT-SLUT                        VALUE 'GB'.                  
016300     88  IMS-EJ-OK                           VALUE 'XD'.                  
016400                                                                          
016500 01  GODK-STATUSKODER.                                                    
016600     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
016700                                                                          
016800 01  SSA1                        PIC X(64).                               
016900 01  SSA2                        PIC X(64).                               
017000     EJECT                                                                
017100*    --- IMS FUNKTIONSKODER                                               
017200*01  -COPY W0003                                                          
017300     EJECT                                                                
017400                                                                          
017500 01  FILLER         PIC X(24) VALUE 'DLI-IO-WDK601'.                      
017600 01  DLI-IO-WDK601.                                                       
017700*    03  -COPY WDK601                                                     
017800     EJECT                                                                
017900                                                                          
018000 01  FILLER         PIC X(24) VALUE 'DLI-IO-WDK611'.                      
018100 01  DLI-IO-WDK611.                                                       
018200*    03  -COPY WDK611                                                     
018300     EJECT                                                                
018400                                                                          
018500 01  FILLER         PIC X(24) VALUE 'DLI-IO-WDK701'.                      
018600 01  DLI-IO-WDK701.                                                       
018700*    03  -COPY WDK701                                                     
018800     EJECT                                                                
018900                                                                          
019000 01  FILLER         PIC X(24) VALUE 'DLI-IO-WDK711'.                      
019100 01  DLI-IO-WDK711.                                                       
019200*    03  -COPY WDK711                                                     
019300     EJECT                                                                
019400                                                                          
019500 01  FILLER         PIC X(16)  VALUE 'DLI-IO-WDD701'.                     
019600     SKIP3                                                                
019700 01  DLI-IO-WDD701.                                                       
019800*    03  -COPY WDD701  -PRE WDD701-                                       
019900     EJECT                                                                
020000 01  FILLER         PIC X(16)  VALUE 'DLI-IO-WDD704'.                     
020100     SKIP3                                                                
020200 01  DLI-IO-WDD704.                                                       
020300*    03  -COPY WDD704  -PRE WDD704-                                       
020400     EJECT                                                                
020500                                                                          
020600 LINKAGE SECTION.                                                         
020700                                                                          
020800*01  -COPY W0008 -PRE WDK6-                                               
020900     05  FILLER      PIC X.                                               
021000     EJECT                                                                
021100*01  -COPY W0008 -PRE WDK7-                                               
021200     05  FILLER      PIC X.                                               
021300     EJECT                                                                
021400*01  -COPY W0008 -PRE WDD7-                                               
021500     05  FILLER      PIC X.                                               
021600     EJECT                                                                
021700 PROCEDURE DIVISION  USING WDK6-PCB WDK7-PCB                              
021800                           WDD7-PCB.                                      
021900     ENTRY 'DLITCBL' USING WDK6-PCB WDK7-PCB                              
022000                           WDD7-PCB.                                      
022100                                                                          
022200     PERFORM A-INIT                                                       
022300     PERFORM S01-LAES-W11158                                              
022400                                                                          
022500     PERFORM UNTIL END-OF-W11158                                          
022600       PERFORM B-KOLLA-INDATA                                             
022700       IF INDATA-OK                                                       
022800         PERFORM C-SKAPA-UTDATA                                           
022900       END-IF                                                             
023000       PERFORM S01-LAES-W11158                                            
023100     END-PERFORM                                                          
023200                                                                          
023300     PERFORM Z-FINIT                                                      
023400                                                                          
023500     MOVE ZERO TO RETURN-CODE                                             
023600     GOBACK                                                               
023700     .                                                                    
023800     EJECT                                                                
023900 A-INIT SECTION.                                                          
024000                                                                          
024100     OPEN INPUT W11158                                                    
024200         OUTPUT W11159                                                    
024300     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
024400     .                                                                    
024500     EJECT                                                                
024600                                                                          
024700 B-KOLLA-INDATA SECTION.                                                  
024800                                                                          
024900     MOVE JA   TO INDATA-SW                                               
025000     UNSTRING IN-AREA DELIMITED BY SPACE INTO WS-IDARTNR                  
025100       COUNT IN LIDARTNR                                                  
025200     END-UNSTRING                                                         
025300     IF LIDARTNR > LENGTH OF WS-IDARTNR                                   
025400       MOVE NEJ TO INDATA-SW                                              
025500     ELSE                                                                 
025600       IF WS-IDARTNR(1:LIDARTNR) NUMERIC                                  
025700         MOVE WS-IDARTNR(1:LIDARTNR)     TO W-IDARTNR                     
025800       ELSE                                                               
025900         MOVE NEJ TO INDATA-SW                                            
026000       END-IF                                                             
026100     END-IF                                                               
026200     .                                                                    
026300     EJECT                                                                
026400                                                                          
026500 C-SKAPA-UTDATA SECTION.                                                  
026600                                                                          
026700     PERFORM IMS-GU-WDK601                                                
026800     IF SEGMENT-FINNS                                                     
026900       PERFORM IMS-GN-WDK611                                              
027000       MOVE CLAG-KVLS     TO UT-KVLS-CDC                                  
027100       MOVE CLAG-KDERS    TO UT-KDERS                                     
027200       IF CLAG-KDERS > 20                                                 
027300         MOVE ART-TIERSDAT TO UT-TIERSDAT                                 
027400       ELSE                                                               
027500         PERFORM IMS-GU-WDD704                                            
027600         IF SEGMENT-FINNS                                                 
027700           MOVE WDD704-TIERSDAT-REG TO UT-TIERSDAT                        
027800         ELSE                                                             
027900           MOVE ZERO                  TO UT-TIERSDAT                      
028000         END-IF                                                           
028100       END-IF                                                             
028200       MOVE ZERO TO WS-KVLS-JP                                            
028300                    WS-KVLS-AU                                            
028400                    WS-KVLS-CA                                            
028500                    WS-KVLS-USA                                           
028600                    WS-KVLS-EU                                            
028700                    WS-KVLS-CN                                            
028800                    WS-KVLS-IN                                            
028810                    WS-KVLS-KR                                            
028820                    WS-KVLS-AE                                            
028830                    WS-KVLS-TR                                            
028840                    WS-KVLS-MY                                            
028850                    WS-KVLS-TH                                            
028860                    WS-KVLS-TW                                            
028861                    WS-KVLS-BR                                            
028862                    WS-KVLS-MX                                            
028863                    WS-KVLS-ZA                                            
028900       PERFORM IMS-GU-WDK701                                              
029000       IF SEGMENT-FINNS                                                   
029100         PERFORM IMS-GNP-WDK711                                           
029200         PERFORM UNTIL SEGMENT-SAKNAS                                     
029300           MOVE SLAG-IDDC TO WS-IDDC                                      
029400           IF NDC-JP                                                      
029500             ADD SLAG-KVLS            TO WS-KVLS-JP                       
029600           ELSE                                                           
029700             IF NDC-AU                                                    
029800               ADD SLAG-KVLS          TO WS-KVLS-AU                       
029900             ELSE                                                         
030000               IF NDC-CA                                                  
030100                 ADD SLAG-KVLS        TO WS-KVLS-CA                       
030200               ELSE                                                       
030300                 IF NDC-US                                                
030400                   ADD SLAG-KVLS      TO WS-KVLS-USA                      
030500                 ELSE                                                     
030600                   IF NDC-CN                                              
030700                     ADD SLAG-KVLS    TO WS-KVLS-CN                       
030800                   ELSE                                                   
030900                     IF (LDC OR SDC)                                      
031000                       ADD SLAG-KVLS  TO WS-KVLS-EU                       
031100                     ELSE                                                 
031200                       IF NDC-IN                                          
031300                         ADD SLAG-KVLS TO WS-KVLS-IN                      
031400                       ELSE                                               
031401                         IF NDC-KR                                        
031402                           ADD SLAG-KVLS TO WS-KVLS-KR                    
031410                         ELSE                                             
031411                           IF NDC-AE                                      
031412                             ADD SLAG-KVLS TO WS-KVLS-AE                  
031414                           ELSE                                           
031415                            IF NDC-TR                                     
031416                               ADD SLAG-KVLS TO WS-KVLS-TR                
031417                            ELSE                                          
031418                             IF NDC-BR                                    
031419                                ADD SLAG-KVLS TO WS-KVLS-BR               
031420                             ELSE                                         
031421                              IF NDC-MX                                   
031422                                 ADD SLAG-KVLS TO WS-KVLS-MX              
031423                              ELSE                                        
031424                                IF NDC-MY                                 
031425                                 ADD SLAG-KVLS TO WS-KVLS-MY              
031426                               ELSE                                       
031427                                 IF NDC-TH                                
031428                                   ADD SLAG-KVLS TO WS-KVLS-TH            
031429                                 ELSE                                     
031430                                   IF NDC-TW                              
031431                                     ADD SLAG-KVLS TO WS-KVLS-TW          
031432                                   ELSE                                   
031433                                    IF NDC-ZA                             
031434                                     ADD SLAG-KVLS TO WS-KVLS-ZA          
031435                                    END-IF                                
031436                                   END-IF                                 
031437                                 END-IF                                   
031438                               END-IF                                     
031439                              END-IF                                      
031440                             END-IF                                       
031441                            END-IF                                        
031442                           END-IF                                         
031443                         END-IF                                           
031450                       END-IF                                             
031500                     END-IF                                               
031600                   END-IF                                                 
031700                 END-IF                                                   
031800               END-IF                                                     
031900             END-IF                                                       
032000           END-IF                                                         
032100           PERFORM IMS-GNP-WDK711                                         
032200         END-PERFORM                                                      
032300       ELSE                                                               
032400         MOVE ZERO    TO WS-KVLS-EU                                       
032500                         WS-KVLS-JP                                       
032600                         WS-KVLS-AU                                       
032700                         WS-KVLS-USA                                      
032800                         WS-KVLS-CA                                       
032900                         WS-KVLS-CN                                       
033000                         WS-KVLS-IN                                       
033010                         WS-KVLS-KR                                       
033020                         WS-KVLS-AE                                       
033030                         WS-KVLS-TR                                       
033040                         WS-KVLS-MY                                       
033050                         WS-KVLS-TH                                       
033060                         WS-KVLS-TW                                       
033061                         WS-KVLS-BR                                       
033062                         WS-KVLS-MX                                       
033063                         WS-KVLS-ZA                                       
033100       END-IF                                                             
033200       MOVE WS-KVLS-EU    TO UT-KVLS-EU                                   
033300       MOVE WS-KVLS-JP    TO UT-KVLS-JP                                   
033400       MOVE WS-KVLS-AU    TO UT-KVLS-AU                                   
033500       MOVE WS-KVLS-USA   TO UT-KVLS-USA                                  
033600       MOVE WS-KVLS-CA    TO UT-KVLS-CA                                   
033700       MOVE WS-KVLS-CN    TO UT-KVLS-CN                                   
033800       MOVE WS-KVLS-IN    TO UT-KVLS-IN                                   
033810       MOVE WS-KVLS-KR    TO UT-KVLS-KR                                   
033820       MOVE WS-KVLS-AE    TO UT-KVLS-AE                                   
033830       MOVE WS-KVLS-TR    TO UT-KVLS-TR                                   
033840       MOVE WS-KVLS-MY    TO UT-KVLS-MY                                   
033850       MOVE WS-KVLS-TH    TO UT-KVLS-TH                                   
033860       MOVE WS-KVLS-TW    TO UT-KVLS-TW                                   
033861       MOVE WS-KVLS-BR    TO UT-KVLS-BR                                   
033862       MOVE WS-KVLS-MX    TO UT-KVLS-MX                                   
033863       MOVE WS-KVLS-ZA    TO UT-KVLS-ZA                                   
033900     ELSE                                                                 
034000       MOVE ZERO      TO UT-KDERS                                         
034100                         UT-TIERSDAT                                      
034200                         UT-KVLS-CDC                                      
034300                         UT-KVLS-EU                                       
034400                         UT-KVLS-JP                                       
034500                         UT-KVLS-AU                                       
034600                         UT-KVLS-USA                                      
034700                         UT-KVLS-CA                                       
034800                         UT-KVLS-CN                                       
034900                         UT-KVLS-IN                                       
034910                         UT-KVLS-KR                                       
034920                         UT-KVLS-AE                                       
034930                         UT-KVLS-TR                                       
034940                         UT-KVLS-MY                                       
034950                         UT-KVLS-TH                                       
034960                         UT-KVLS-TW                                       
034961                         UT-KVLS-BR                                       
034962                         UT-KVLS-MX                                       
034963                         UT-KVLS-ZA                                       
035000     END-IF                                                               
035100     MOVE W-IDARTNR   TO UT-IDARTNR                                       
035200     PERFORM S02-SKRIV-W11159                                             
035300     .                                                                    
035400     EJECT                                                                
035500 Z-FINIT SECTION.                                                         
035600                                                                          
035700     CLOSE W11158                                                         
035800           W11159                                                         
035900                                                                          
036000     MOVE 'S' TO POSTSUM-OPKOD                                            
036100     CALL POSTSUM USING POSTSUM-PARM                                      
036200     .                                                                    
036300     EJECT                                                                
036400 S01-LAES-W11158  SECTION.                                                
036500                                                                          
036600     READ W11158 INTO IN-AREA                                             
036700     AT END                                                               
036800        SET END-OF-W11158 TO TRUE                                         
036900                                                                          
037000     NOT AT END                                                           
037100        MOVE 'W11158'       TO POSTSUM-FDNAMN                             
037200        MOVE 'W11159D1'     TO POSTSUM-DDNAMN2                            
037300        MOVE 'IN'           TO POSTSUM-TRANSTYP                           
037400        CALL POSTSUM USING POSTSUM-PARM                                   
037500     END-READ                                                             
037600     .                                                                    
037700     EJECT                                                                
037800 S02-SKRIV-W11159  SECTION.                                               
037900                                                                          
038000     WRITE UT-POST FROM UT-AREA                                           
038100                                                                          
038200        MOVE 'W11159'       TO POSTSUM-FDNAMN                             
038300        MOVE 'W11159D2'     TO POSTSUM-DDNAMN2                            
038400        MOVE 'UT'           TO POSTSUM-TRANSTYP                           
038500        CALL POSTSUM USING POSTSUM-PARM                                   
038600     .                                                                    
038700     EJECT                                                                
038800* IMS SECTIONER                                                           
038900                                                                          
039000 IMS-GU-WDK601 SECTION.                                                   
039100     STRING 'WDK601  (IDARTNR  =' W-IDARTNR-X ')'                         
039200          DELIMITED BY SIZE INTO SSA1                                     
039300     MOVE '  GE' TO GODK-STATUSKODER                                      
039400     CALL CBLTDLI USING GU WDK6-PCB DLI-IO-WDK601 SSA1                    
039500     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
039600     PERFORM IMS-STATUSKONTROLL                                           
039700     .                                                                    
039800     SKIP3                                                                
039900                                                                          
040000 IMS-GN-WDK611 SECTION.                                                   
040100                                                                          
040200     STRING 'WDK611  (KDSEGKEY =' W-KDSEGKEY-X ')'                        
040300          DELIMITED BY SIZE INTO SSA1                                     
040400     MOVE '  GE' TO GODK-STATUSKODER                                      
040500     CALL CBLTDLI USING GNP WDK6-PCB DLI-IO-WDK611 SSA1                   
040600     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
040700     PERFORM IMS-STATUSKONTROLL                                           
040800     .                                                                    
040900     EJECT                                                                
041000 IMS-GU-WDD704 SECTION.                                                   
041100     STRING 'WDD701  (IDARTNR  =' W-IDARTNR-X ')'                         
041200            DELIMITED BY SIZE INTO SSA1                                   
041300     MOVE 'WDD704   ' TO SSA2                                             
041400     MOVE '  GE' TO GODK-STATUSKODER                                      
041500     CALL CBLTDLI USING GU WDD7-PCB DLI-IO-WDD704 SSA1 SSA2               
041600     MOVE WDD7-STATUS-CODE TO STATUS-WS                                   
041700     PERFORM IMS-STATUSKONTROLL                                           
041800     .                                                                    
041900     EJECT                                                                
042000 IMS-GU-WDK701 SECTION.                                                   
042100     STRING 'WDK701  (IDARTNR = ' W-IDARTNR-X ')'                         
042200          DELIMITED BY SIZE INTO SSA1                                     
042300     MOVE '  GE' TO GODK-STATUSKODER                                      
042400     CALL CBLTDLI USING GU WDK7-PCB DLI-IO-WDK701  SSA1                   
042500     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
042600     PERFORM IMS-STATUSKONTROLL                                           
042700     .                                                                    
042800     SKIP3                                                                
042900 IMS-GNP-WDK711 SECTION.                                                  
043000     MOVE 'WDK711   ' TO SSA1                                             
043100     MOVE '  GE' TO GODK-STATUSKODER                                      
043200     CALL CBLTDLI USING GNP WDK7-PCB DLI-IO-WDK711  SSA1                  
043300     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
043400     PERFORM IMS-STATUSKONTROLL                                           
043500     .                                                                    
043600     EJECT                                                                
043700                                                                          
043800 IMS-STATUSKONTROLL SECTION.                                              
043900                                                                          
044000     SET STATUS-IX TO 1                                                   
044100     SEARCH GODK-STATUS                                                   
044200       AT END                                                             
044300         MOVE 'IMS RETURKOD : ' TO FELTEXT-STR                            
044400         DISPLAY FELTEXT STATUS-WS                                        
044500         CALL FELLOG                                                      
044600       WHEN GODK-STATUS(STATUS-IX) = STATUS-WS                            
044700         CONTINUE                                                         
044800     END-SEARCH                                                           
044900     .                                                                    
