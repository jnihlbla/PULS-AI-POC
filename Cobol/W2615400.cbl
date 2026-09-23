000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W2615400.                                                
000300 AUTHOR.         CONNY EGHOLT.                                            
000400 DATE-WRITTEN.   01/10/08.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*    FUNKTION:                                                            
000800*        KOMPLETTERAR INFIL MED ANSKAFFARID OCH ANSK.GRUPP-INTERV.        
000900*        SKRIVER ENDAST UT POSTER SOM TILLHÖR DISTRIKT 81                 
001000*                                         OCH KUNDERNA 11 OCH 111         
001100*                                                                         
001200*        PROGRAMMET LÄSER      WDK6                                       
001300*                                                                         
001400*    ABENDKODER:                                                          
001500*        U0016 -  . . . .                                                 
001600*        U1000 -  . . . .                                                 
001700*                                                                         
001800     SKIP3                                                                
001900 ENVIRONMENT DIVISION.                                                    
002000     SKIP2                                                                
002100 INPUT-OUTPUT SECTION.                                                    
002200                                                                          
002300 FILE-CONTROL.                                                            
002400     SKIP2                                                                
002500*          --- ALLA RADER PÅ WDQ4                                         
002600     SELECT W26155                     ASSIGN TO W26154D1.                
002700     SKIP2                                                                
002800*          --- KOMPLETTERAD EFR-FIL MED ANSK PÅ DISTR 81                  
002900     SELECT W26154                     ASSIGN TO W26154D2.                
003000     EJECT                                                                
003100 DATA DIVISION.                                                           
003200     SKIP2                                                                
003300 FILE SECTION.                                                            
003400     SKIP3                                                                
003500 FD  W26155                                                               
003600     RECORDING       F                                                    
003700     BLOCK CONTAINS  0.                                                   
003800*01  -COPY WDQ401       -L.                                               
003900                                                                          
004000     SKIP3                                                                
004100 FD  W26154                                                               
004200     RECORDING       F                                                    
004300     BLOCK CONTAINS  0.                                                   
004400*01  POST -COPY W26154 -PRE  UT-  -L.                                     
004500                                                                          
004600     EJECT                                                                
004700 WORKING-STORAGE SECTION.                                                 
004800                                                                          
004900 77  IDPGM                       PIC X(8)    VALUE 'W2615400'.            
005000 77  JA                          PIC X       VALUE 'J'.                   
005100 77  NEJ                         PIC X       VALUE 'N'.                   
005200                                                                          
005300 77  W26155-EOF-SW               PIC X       VALUE 'N'.                   
005400     88  END-OF-W26155                       VALUE 'J'.                   
005500     EJECT                                                                
005600 01  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
005700 01  FILLER REDEFINES DAGENS-DATUM.                                       
005800     03  DAGENS-DATUM-AAR        PIC 9(2).                                
005900     03  DAGENS-DATUM-MAANAD     PIC 9(2).                                
006000     03  DAGENS-DATUM-DAG        PIC 9(2).                                
006100     EJECT                                                                
006200 01  DYNAMISKA-SUBPROGRAM.                                                
006300*                                                                         
006400     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
006500     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
006600     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
006700     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
006800     03  W200ANSK                PIC X(8)    VALUE 'W200ANSK'.            
006900     SKIP2                                                                
007000*    --- PARAMETRAR TILL ABEND                                            
007100                                                                          
007200 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
007300 77  RKOD-ABEND-UTAN-DUMP        PIC S9(4)   COMP VALUE +16.              
007400 77  RKOD-ABEND-MED-DUMP         PIC S9(4)   COMP VALUE +1000.            
007500     SKIP2                                                                
007600 01  FELTEXT.                                                             
007700     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
007800     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
007900     EJECT                                                                
008000*    --- PARAMETRAR TILL POSTSUM                                          
008100*                                                                         
008200*01  -COPY W0005   -PRE  POSTSUM-                                         
008300     EJECT                                                                
008400*01  -COPY W009W42   -PRE L-                                              
008500     EJECT                                                                
008600 01  IN-AREA-START       PIC X(24)   VALUE 'IN-AREA-START  '.             
008700*01  AREA -COPY WDQ401      -PRE IN-                                      
008800     EJECT                                                                
008900                                                                          
009000                                                                          
009100 01  UT-AREA-START       PIC X(24)   VALUE 'UT-AREA-START  '.             
009200*01  AREA -COPY W26154     -PRE UT-                                       
009300                                                                          
009400     EJECT                                                                
009500*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
009600*                                                                         
009700     EJECT                                                                
009800 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
009900     SKIP3                                                                
010000 01  NYCKLAR-TILL-DLI.                                                    
010100     03  W-IDARTNR-X.                                                     
010200         05  W-IDARTNR           PIC S9(9)   VALUE ZERO COMP-3.           
010210     03  W-IDORDER-X.                                                     
010220         05  W-IDORDER           PIC S9(7)   VALUE ZERO COMP-3.           
010300     SKIP2                                                                
010400*    --- STATUS-KOD FRÅN IMS                                              
010500 01  STATUS-WS                   PIC XX.                                  
010600     88  SEGMENT-FINNS                       VALUE '  '.                  
010700     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
010800     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
010900     SKIP2                                                                
011000 01  GODK-STATUSKODER.                                                    
011100     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
011200     SKIP3                                                                
011300 01  SSA1                        PIC X(64).                               
011400 01  SSA2                        PIC X(64).                               
011500     EJECT                                                                
011600*    --- IMS FUNKTIONSKODER                                               
011700*01  -COPY W0003                                                          
011800     EJECT                                                                
011900*    ---  DLI INPUT-OUTPUT AREA                                           
012000 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK601'.                      
012100 01  DLI-IO-WDK601.                                                       
012200*    03  -COPY WDK601                                                     
012300     EJECT                                                                
012400 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK611'.                      
012500 01  DLI-IO-WDK611.                                                       
012600*    03  -COPY WDK611                                                     
012700     EJECT                                                                
012710 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDQ201'.                      
012720 01  DLI-IO-WDQ201.                                                       
012730*    03  -COPY WDQ201                                                     
012740     EJECT                                                                
012800 LINKAGE SECTION.                                                         
012900                                                                          
013000*01  -COPY W0008  -PRE WDK6-                                              
013100     05  FILLER                  PIC X.                                   
013200     EJECT                                                                
013210*01  -COPY W0008  -PRE WDQ2-                                              
013220     05  FILLER                  PIC X.                                   
013230     EJECT                                                                
013300 PROCEDURE DIVISION  USING WDK6-PCB WDQ2-PCB.                             
013400 MAIN SECTION.                                                            
013500     ENTRY 'DLITCBL' USING WDK6-PCB WDQ2-PCB.                             
013600*------------------------                                                 
013700     PERFORM A-INIT                                                       
013800                                                                          
013900     PERFORM S01-LAES-W26155                                              
014000     PERFORM UNTIL END-OF-W26155                                          
014100                                                                          
014200       IF IN-ORAD-IDDISTR = +81                                           
014300       AND ( IN-ORAD-IDKUNDNR = +11 OR +111 )                             
014400         MOVE IN-AREA TO UT-AREA                                          
014500         MOVE ZERO    TO UT-ORAD-GRP-INT                                  
014600                         UT-ORAD-IDANSK                                   
014700*      --- KOMPLETTERA POSTER MED DIST=81 FÖR KUND 11 OCH 111             
014800                                                                          
014900         MOVE IN-ORAD-IDARTNR TO W-IDARTNR                                
015000         PERFORM IMS-GET-WDK601                                           
015100         IF SEGMENT-FINNS                                                 
015200           PERFORM IMS-GET-WDK611                                         
015300           IF SEGMENT-FINNS                                               
015400             MOVE CLAG-FLSKROT-AUTO TO UT-ORAD-FLSKROT-AUTO               
015500             MOVE CLAG-IDANSK       TO L-IDANSK                           
015600                                                                          
015700             CALL W200ANSK USING L-W009W42                                
015800                                                                          
015900             MOVE L-IDANSK          TO UT-ORAD-IDANSK                     
016000             MOVE L-GRUPP-INTERVALL TO UT-ORAD-GRP-INT                    
016100           END-IF                                                         
016200         END-IF                                                           
016201                                                                          
016210         MOVE IN-ORAD-IDORDER TO W-IDORDER                                
016220         PERFORM IMS-GU-WDQ201                                            
016230         IF SEGMENT-FINNS AND OHUV-IDKONTO = 481180                       
016231***         DESSA SKALL EJ MED                                            
016240            CONTINUE                                                      
016250         ELSE                                                             
016300            PERFORM S11-SKRIV-W26154                                      
016310         END-IF                                                           
016400       END-IF                                                             
016500                                                                          
016600       PERFORM S01-LAES-W26155                                            
016700     END-PERFORM                                                          
016800                                                                          
016900     PERFORM Z-FINIT                                                      
017000                                                                          
017100     MOVE ZERO TO RETURN-CODE                                             
017200     GOBACK                                                               
017300     .                                                                    
017400     EJECT                                                                
017500 A-INIT SECTION.                                                          
017600                                                                          
017700     OPEN INPUT  W26155                                                   
017800                                                                          
017900     OPEN OUTPUT W26154                                                   
018000                                                                          
018100     ACCEPT DAGENS-DATUM  FROM DATE                                       
018200     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
018300     .                                                                    
018400     EJECT                                                                
018500 Z-FINIT SECTION.                                                         
018600     CLOSE W26155                                                         
018700           W26154                                                         
018800     SKIP2                                                                
018900     MOVE 'S' TO POSTSUM-OPKOD                                            
019000     CALL POSTSUM USING POSTSUM-PARM                                      
019100     .                                                                    
019200     EJECT                                                                
019300 S01-LAES-W26155  SECTION.                                                
019400     READ W26155 INTO IN-AREA                                             
019500     AT END                                                               
019600        MOVE HIGH-VALUE TO IN-AREA                                        
019700        SET END-OF-W26155 TO TRUE                                         
019800                                                                          
019900     NOT AT END                                                           
020000        MOVE 'IN'      TO POSTSUM-TRANSTYP                                
020100        MOVE 'W26155' TO POSTSUM-FDNAMN                                   
020200        MOVE 'W26154D1' TO POSTSUM-DDNAMN2                                
020300        CALL POSTSUM USING POSTSUM-PARM                                   
020400     END-READ                                                             
020500     .                                                                    
020600     EJECT                                                                
020700 S11-SKRIV-W26154 SECTION.                                                
020800                                                                          
020900     WRITE UT-POST FROM UT-AREA                                           
021000                                                                          
021100     MOVE 'UT'      TO POSTSUM-TRANSTYP                                   
021200     MOVE 'W26154' TO POSTSUM-FDNAMN                                      
021300     MOVE 'W26154D2' TO POSTSUM-DDNAMN2                                   
021400     CALL POSTSUM USING POSTSUM-PARM                                      
021500     .                                                                    
021600     EJECT                                                                
021700* --- IMS SEKTIONER ---                                                   
021800                                                                          
022000 IMS-GET-WDK601 SECTION.                                                  
022100                                                                          
022200     STRING 'WDK601  (IDARTNR  =' W-IDARTNR-X ')'                         
022300          DELIMITED BY SIZE INTO SSA1                                     
022400     MOVE '  GE' TO GODK-STATUSKODER                                      
022500     CALL CBLTDLI USING GU WDK6-PCB DLI-IO-WDK601 SSA1                    
022600     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
022700     PERFORM IMS-STATUSKONTROLL                                           
022800     .                                                                    
022900     EJECT                                                                
023000 IMS-GET-WDK611 SECTION.                                                  
023100                                                                          
023200     MOVE   'WDK611  '        TO SSA1                                     
023300     MOVE '  GE' TO GODK-STATUSKODER                                      
023400     CALL CBLTDLI USING GNP WDK6-PCB DLI-IO-WDK611 SSA1                   
023500     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
023600     PERFORM IMS-STATUSKONTROLL                                           
023700     .                                                                    
023800     EJECT                                                                
023810 IMS-GU-WDQ201 SECTION.                                                   
023820                                                                          
023830     STRING 'WDQ201  (IDORDER  =' W-IDORDER-X ')'                         
023840          DELIMITED BY SIZE INTO SSA1                                     
023850     MOVE '  GE' TO GODK-STATUSKODER                                      
023860     CALL CBLTDLI USING GU WDQ2-PCB DLI-IO-WDQ201 SSA1                    
023870     MOVE WDQ2-STATUS-CODE TO STATUS-WS                                   
023880     PERFORM IMS-STATUSKONTROLL                                           
023890     .                                                                    
023891     EJECT                                                                
023900 IMS-STATUSKONTROLL SECTION.                                              
024000                                                                          
024100     SET STATUS-IX TO 1                                                   
024200     SEARCH GODK-STATUS                                                   
024300       AT END                                                             
024400         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
024500           DELIMITED BY SIZE INTO FELTEXT                                 
024600         DISPLAY FELTEXT                                                  
024700         CALL FELLOG                                                      
024800       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
024900         CONTINUE                                                         
025000     END-SEARCH                                                           
025100     .                                                                    
