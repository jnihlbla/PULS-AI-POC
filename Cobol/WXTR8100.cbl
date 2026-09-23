000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     WXTR8100.                                                
000300 AUTHOR.         NIHLBLAD JOHAN.                                          
000400 DATE-WRITTEN.   10/02/01.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*    FUNCTION:                                                            
000800*        PGM TO CREATE EXTARCT FROM WDF8                                  
000900*                                                                         
001000*        THE PROGRAM READS     WDF8                                       
001100*                                                                         
001200*    ABENDCODES:                                                          
001300*        U0016 -  . . . .                                                 
001400*        U1000 -  . . . .                                                 
001500*                                                                         
001600                                                                          
001700     SKIP3                                                                
001800 ENVIRONMENT DIVISION.                                                    
001900     SKIP2                                                                
002000 INPUT-OUTPUT SECTION.                                                    
002100                                                                          
002200 FILE-CONTROL.                                                            
002300     SKIP2                                                                
002400*          --- UTFIL                                                      
002500     SELECT WXTR81                     ASSIGN TO WXTR81D1.                
002600     EJECT                                                                
002700 DATA DIVISION.                                                           
002800     SKIP2                                                                
002900 FILE SECTION.                                                            
003000     SKIP3                                                                
003100 FD  WXTR81                                                               
003200     RECORDING       F                                                    
003300     BLOCK CONTAINS  0.                                                   
003400                                                                          
003500*01  RECORD -COPY WXTR81 -PRE  UT-  -L.                                   
003600     EJECT                                                                
003700 WORKING-STORAGE SECTION.                                                 
003800                                                                          
003900 77  IDPGM                       PIC X(8)    VALUE 'WXTR8100'.            
004000 77  YES                         PIC X       VALUE 'J'.                   
004100 77  NOO                         PIC X       VALUE 'N'.                   
004200     EJECT                                                                
004300 01  TODAYS-DATE                 PIC 9(6)    VALUE ZERO.                  
004400 01  FILLER REDEFINES TODAYS-DATE.                                        
004500     03  TODAYS-DATE-YEAR        PIC 9(2).                                
004600     03  TODAYS-DATE-MONTH       PIC 9(2).                                
004700     03  TODAYS-DATE-DAY         PIC 9(2).                                
004800     EJECT                                                                
006000 01  GENERAL-SUBPROGRAMS.                                                 
006100*                                                                         
006200     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
006300     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
006400     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
006500     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
006600     SKIP2                                                                
006700*    --- PARAMETERS FOR SUBPROGRAM ABEND                                  
006800                                                                          
006900 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
007000 77  RKOD-ABEND-NO-DUMP          PIC S9(4)   COMP VALUE +16.              
007100 77  RKOD-ABEND-WITH-DUMP        PIC S9(4)   COMP VALUE +1000.            
007200     SKIP2                                                                
007300 01  FELTEXT.                                                             
007400     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
007500     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
007600                                                                          
007700*    --- PARAMETRAR TILL POSTSUM                                          
007800*                                                                         
007900*01  -COPY W0005   -PRE  POSTSUM-                                         
008000     EJECT                                                                
008100 01  UT-AREA-START               PIC X(24)   VALUE                        
008200                                 'UT-AREA-START  '.                       
008300     SKIP2                                                                
008400                                                                          
008500*01  AREA -COPY WXTR81     -PRE UT-                                       
008600     EJECT                                                                
008700*    --- AREAS FOR IMS-SECTIONS                                           
008800*                                                                         
008900     EJECT                                                                
009000 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
009100     SKIP3                                                                
009200 01  KEYS-FOR-DLI.                                                        
009300     03  W-IDARTNR-X.                                                     
009400         05  W-IDARTNR           PIC S9(9)   VALUE ZERO COMP-3.           
009500                                                                          
009900     SKIP2                                                                
010000*    --- STATUS-KOD FRÅN IMS                                              
010100 01  STATUS-WS                   PIC XX.                                  
010200     88  SEGMENT-FOUND                       VALUE '  '.                  
010300     88  SEGMENT-FOUND-EXISTS                VALUE 'II'.                  
010400     88  SEGMENT-MISSING                     VALUE 'GE'.                  
010500     88  END-OF-REGISTER                     VALUE 'GB'.                  
010600     SKIP2                                                                
010700 01  GOOD-STATUSCODES.                                                    
010800     03  GOOD-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
010900     SKIP3                                                                
011000 01  SSA1                        PIC X(64).                               
011100     EJECT                                                                
011200*    --- IMS FUNCTION CODES                                               
011300*01  -COPY W0003                                                          
011400     EJECT                                                                
011500*    ---  DLI INPUT-OUTPUT AREA                                           
011600 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDF801'.                      
011700 01  DLI-IO-WDF801.                                                       
011800*    03  -COPY WDF801                                                     
011900     EJECT                                                                
012000 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDF811'.                      
012100 01  DLI-IO-WDF811.                                                       
012200*    03  -COPY WDF811                                                     
012300     EJECT                                                                
012400 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDF812'.                      
012500 01  DLI-IO-WDF812.                                                       
012600*    03  -COPY WDF812                                                     
012700     EJECT                                                                
012800 LINKAGE SECTION.                                                         
012900                                                                          
013000                                                                          
013100*01  -COPY W0008  -PRE WDF8-                                              
013200     05  FILLER                  PIC X.                                   
013300     EJECT                                                                
013400 PROCEDURE DIVISION  USING WDF8-PCB.                                      
013500 MAIN SECTION.                                                            
013600     ENTRY 'DLITCBL' USING WDF8-PCB.                                      
013700                                                                          
013800                                                                          
013900     PERFORM A-INIT                                                       
014000                                                                          
014100     PERFORM IMS-GU-WDF801                                                
014200     PERFORM UNTIL SEGMENT-MISSING OR END-OF-REGISTER                     
014300       PERFORM IMS-GNP-WDF812                                             
014310       IF SEGMENT-FOUND                                                   
014400         PERFORM UNTIL SEGMENT-MISSING                                    
014500           MOVE ASPR-IDARTNR     TO W-IDARTNR                             
014600           PERFORM IMS-GNP-WDF811-FIRST                                   
014610           IF SEGMENT-FOUND                                               
014700             PERFORM UNTIL SEGMENT-MISSING                                
014800               PERFORM B-WRITE-OUTFILE-ALL                                
014900               PERFORM IMS-GNP-WDF811                                     
015000             END-PERFORM                                                  
015010           ELSE                                                           
015020             PERFORM B-WRITE-OUTFILE-812                                  
015030           END-IF                                                         
015100           PERFORM IMS-GNP-WDF812                                         
015200         END-PERFORM                                                      
015210       ELSE                                                               
015211         PERFORM IMS-GNP-WDF811-FIRST                                     
015212         IF SEGMENT-FOUND                                                 
015213           PERFORM UNTIL SEGMENT-MISSING                                  
015214             PERFORM B-WRITE-OUTFILE-811                                  
015215             PERFORM IMS-GNP-WDF811                                       
015216           END-PERFORM                                                    
015217         ELSE                                                             
015218           PERFORM B-WRITE-OUTFILE-801                                    
015219         END-IF                                                           
015220       END-IF                                                             
015230       MOVE ZERO                TO W-IDARTNR                              
015300       PERFORM IMS-GN-WDF801                                              
015400     END-PERFORM                                                          
015500                                                                          
015600                                                                          
015700     PERFORM Z-FINIT                                                      
015800                                                                          
015900     MOVE ZERO TO RETURN-CODE                                             
016000     GOBACK                                                               
016100     .                                                                    
016200     EJECT                                                                
016300 A-INIT SECTION.                                                          
016400                                                                          
016500     OPEN OUTPUT WXTR81                                                   
016600                                                                          
016700     ACCEPT TODAYS-DATE  FROM DATE                                        
016800     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
016900     .                                                                    
017000     EJECT                                                                
017010                                                                          
017100 B-WRITE-OUTFILE-ALL SECTION.                                             
017101                                                                          
017102     MOVE GSPR-IDSPRGRP       TO UT-IDDIRGRP                              
017103     MOVE GSPR-TISTADAT       TO UT-DASTADAT-GRP                          
017104     MOVE GSPR-KDMARKBLK      TO UT-KDMARKBLK-GRP                         
017105     MOVE ASPR-IDARTNR        TO UT-IDARTNR                               
017106     MOVE ASPR-TISTADAT       TO UT-DASTADAT-ART                          
017107     MOVE DSPR-IDDISTR-FOM    TO UT-IDDISTR-FOM                           
017108     MOVE DSPR-IDDISTR-TOM    TO UT-IDDISTR-TOM                           
017110     MOVE DSPR-IDKUNDNR-FOM   TO UT-IDKUNDNR-FOM                          
017111     MOVE DSPR-IDKUNDNR-TOM   TO UT-IDKUNDNR-TOM                          
017112     MOVE DSPR-TISTADAT(1)    TO UT-TISTADAT-0                            
017113     MOVE DSPR-TISTADAT(2)    TO UT-TISTADAT-1                            
017114     MOVE DSPR-TISTADAT(3)    TO UT-TISTADAT-2                            
017115     MOVE DSPR-TISTADAT(4)    TO UT-TISTADAT-3                            
017116     MOVE DSPR-TISTADAT(5)    TO UT-TISTADAT-4                            
017117     PERFORM S11-WRITE-WXTR81                                             
017118     .                                                                    
017120     EJECT                                                                
017130                                                                          
017140 B-WRITE-OUTFILE-801 SECTION.                                             
017150                                                                          
017160     MOVE GSPR-IDSPRGRP       TO UT-IDDIRGRP                              
017170     MOVE GSPR-TISTADAT       TO UT-DASTADAT-GRP                          
017180     MOVE GSPR-KDMARKBLK      TO UT-KDMARKBLK-GRP                         
017190     MOVE ZERO                TO UT-IDARTNR                               
017191                                 UT-DASTADAT-ART                          
017192                                 UT-IDDISTR-FOM                           
017193                                 UT-IDDISTR-TOM                           
017194                                 UT-IDKUNDNR-FOM                          
017195                                 UT-IDKUNDNR-TOM                          
017196                                 UT-TISTADAT-0                            
017197                                 UT-TISTADAT-1                            
017198                                 UT-TISTADAT-2                            
017199                                 UT-TISTADAT-3                            
017200                                 UT-TISTADAT-4                            
017201     PERFORM S11-WRITE-WXTR81                                             
017202     .                                                                    
017203     EJECT                                                                
017204                                                                          
017205 B-WRITE-OUTFILE-811 SECTION.                                             
017206                                                                          
017207     MOVE GSPR-IDSPRGRP       TO UT-IDDIRGRP                              
017208     MOVE GSPR-TISTADAT       TO UT-DASTADAT-GRP                          
017209     MOVE GSPR-KDMARKBLK      TO UT-KDMARKBLK-GRP                         
017210     MOVE ZERO                TO UT-IDARTNR                               
017211                                 UT-DASTADAT-ART                          
017212     MOVE DSPR-IDDISTR-FOM    TO UT-IDDISTR-FOM                           
017213     MOVE DSPR-IDDISTR-TOM    TO UT-IDDISTR-TOM                           
017214     MOVE DSPR-IDKUNDNR-FOM   TO UT-IDKUNDNR-FOM                          
017215     MOVE DSPR-IDKUNDNR-TOM   TO UT-IDKUNDNR-TOM                          
017216     MOVE DSPR-TISTADAT(1)    TO UT-TISTADAT-0                            
017217     MOVE DSPR-TISTADAT(2)    TO UT-TISTADAT-1                            
017218     MOVE DSPR-TISTADAT(3)    TO UT-TISTADAT-2                            
017219     MOVE DSPR-TISTADAT(4)    TO UT-TISTADAT-3                            
017220     MOVE DSPR-TISTADAT(5)    TO UT-TISTADAT-4                            
017221     PERFORM S11-WRITE-WXTR81                                             
017222     .                                                                    
017223     EJECT                                                                
017224                                                                          
017225 B-WRITE-OUTFILE-812 SECTION.                                             
017226                                                                          
017227     MOVE GSPR-IDSPRGRP       TO UT-IDDIRGRP                              
017228     MOVE GSPR-TISTADAT       TO UT-DASTADAT-GRP                          
017229     MOVE GSPR-KDMARKBLK      TO UT-KDMARKBLK-GRP                         
017230     MOVE ASPR-IDARTNR        TO UT-IDARTNR                               
017231     MOVE ASPR-TISTADAT       TO UT-DASTADAT-ART                          
017232     MOVE ZERO                TO UT-IDDISTR-FOM                           
017233                                 UT-IDDISTR-TOM                           
017234                                 UT-IDKUNDNR-FOM                          
017235                                 UT-IDKUNDNR-TOM                          
017236                                 UT-TISTADAT-0                            
017237                                 UT-TISTADAT-1                            
017238                                 UT-TISTADAT-2                            
017239                                 UT-TISTADAT-3                            
017240                                 UT-TISTADAT-4                            
017241     PERFORM S11-WRITE-WXTR81                                             
017242     .                                                                    
017243     EJECT                                                                
017244                                                                          
017250 Z-FINIT SECTION.                                                         
017300     CLOSE WXTR81                                                         
017400     SKIP2                                                                
017500     MOVE 'S' TO POSTSUM-OPKOD                                            
017600     CALL POSTSUM USING POSTSUM-PARM                                      
017700     .                                                                    
017800     EJECT                                                                
017900 S11-WRITE-WXTR81 SECTION.                                                
018000                                                                          
018100     WRITE UT-RECORD    FROM UT-AREA                                      
018200                                                                          
018400     MOVE 'WXTR81' TO POSTSUM-FDNAMN                                      
018500     MOVE 'WXTR81D1' TO POSTSUM-DDNAMN2                                   
018600     CALL POSTSUM USING POSTSUM-PARM                                      
018700     .                                                                    
018800     EJECT                                                                
018900 S99-ABEND SECTION.                                                       
019000                                                                          
019100     SKIP2                                                                
019200     MOVE 'S' TO POSTSUM-OPKOD                                            
019300     CALL POSTSUM USING POSTSUM-PARM                                      
019400     CALL ABEND USING RKOD-ABEND                                          
019500     .                                                                    
019600     EJECT                                                                
019700* --- IMS SECTIONS  ---                                                   
019800                                                                          
019900     EJECT                                                                
020000 IMS-GU-WDF801 SECTION.                                                   
020100                                                                          
020200     MOVE 'WDF801  '       TO SSA1                                        
020400     MOVE '  GE'           TO GOOD-STATUSCODES                            
020500     CALL CBLTDLI USING GU WDF8-PCB DLI-IO-WDF801 SSA1                    
020600     MOVE WDF8-STATUS-CODE TO STATUS-WS                                   
020700     PERFORM IMS-STATUSCHECK                                              
020800     .                                                                    
020900     EJECT                                                                
021000 IMS-GN-WDF801 SECTION.                                                   
021100                                                                          
021110     MOVE 'WDF801  '       TO SSA1                                        
021400     MOVE '  GEGB'         TO GOOD-STATUSCODES                            
021500     CALL CBLTDLI USING GN WDF8-PCB DLI-IO-WDF801 SSA1                    
021600     MOVE WDF8-STATUS-CODE TO STATUS-WS                                   
021700     PERFORM IMS-STATUSCHECK                                              
021800     .                                                                    
021900     EJECT                                                                
022000 IMS-GNP-WDF811 SECTION.                                                  
022100                                                                          
022310     MOVE   'WDF811'     TO SSA1                                          
022400     MOVE '  GE' TO GOOD-STATUSCODES                                      
022500     CALL CBLTDLI USING GNP WDF8-PCB DLI-IO-WDF811 SSA1                   
022600     MOVE WDF8-STATUS-CODE TO STATUS-WS                                   
022700     PERFORM IMS-STATUSCHECK                                              
022800     .                                                                    
022900     EJECT                                                                
023000 IMS-GNP-WDF811-FIRST SECTION.                                            
023100                                                                          
023200     MOVE   'WDF811  *F' TO SSA1                                          
023300     MOVE '  GE' TO GOOD-STATUSCODES                                      
023400     CALL CBLTDLI USING GNP WDF8-PCB DLI-IO-WDF811 SSA1                   
023500     MOVE WDF8-STATUS-CODE TO STATUS-WS                                   
023600     PERFORM IMS-STATUSCHECK                                              
023700     .                                                                    
023800     EJECT                                                                
023900 IMS-GNP-WDF812 SECTION.                                                  
024000                                                                          
024100     STRING 'WDF812  (IDARTNR  >' W-IDARTNR-X ')'                         
024200          DELIMITED BY SIZE INTO SSA1                                     
024300     MOVE '  GE' TO GOOD-STATUSCODES                                      
024400     CALL CBLTDLI USING GNP WDF8-PCB DLI-IO-WDF812 SSA1                   
024500     MOVE WDF8-STATUS-CODE TO STATUS-WS                                   
024600     PERFORM IMS-STATUSCHECK                                              
024700     .                                                                    
024800     EJECT                                                                
024900 IMS-STATUSCHECK SECTION.                                                 
025000                                                                          
025100     SET STATUS-IX TO 1                                                   
025200     SEARCH GOOD-STATUS                                                   
025300       AT END                                                             
025310         MOVE 'FEL STATUSKOD' TO FELTEXT-STR                              
025320         DISPLAY FELTEXT                                                  
025700         CALL FELLOG                                                      
025800       WHEN GOOD-STATUS (STATUS-IX) = STATUS-WS                           
025900         CONTINUE                                                         
026000     END-SEARCH                                                           
026100     .                                                                    
