000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W2151100.                                                
000300 AUTHOR.         ANDERSSON BERT.                                          
000400 DATE-WRITTEN.   20/11/06.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700                                                                          
000800*    FUNCTION:                                                            
000900*        LOAD WDJ4 WITH POSTA FROM FILE W21505                            
001000*                                                                         
001100*        THE PROGRAM UPDATES   WDJ4                                       
001300*                                                                         
001400                                                                          
001500     SKIP3                                                                
001600 ENVIRONMENT DIVISION.                                                    
001700     SKIP2                                                                
001800 INPUT-OUTPUT SECTION.                                                    
001900                                                                          
002000 FILE-CONTROL.                                                            
002100     SKIP2                                                                
002200*          --- INFILE                                                     
002300     SELECT W21505                     ASSIGN TO W21511D1.                
002400     EJECT                                                                
002500 DATA DIVISION.                                                           
002600     SKIP3                                                                
002700 FILE SECTION.                                                            
002800     SKIP3                                                                
002900 FD  W21505                                                               
003000     RECORDING       F                                                    
003100     BLOCK CONTAINS  0.                                                   
003200                                                                          
003300 01  IN-POST        PIC X(85).                                            
003400     EJECT                                                                
003500 WORKING-STORAGE SECTION.                                                 
003600                                                                          
003700 77  IDPGM                       PIC X(8)    VALUE 'W2151100'.            
003800 77  FILLER                      PIC X(8)    VALUE 'PGMPOS:'.             
003900 77  WS-PGM-POS                  PIC X(40)   VALUE SPACE.                 
004000 77  FILLER                      PIC X(8)    VALUE 'IMSPOS:'.             
004100 77  WS-PGM-IMS-POS              PIC X(40)   VALUE SPACE.                 
004200 77  WS-IDARTNR-OVER             PIC 9(08)   VALUE ZERO.                  
004300                                                                          
004400 01  CHKP-VAR.                                                            
004500     03 CHKP-MSG-IO-AREA-LENGTH  PIC S9(9)   VALUE +32 COMP SYNC.         
004600     03 CHKP-MSG-IO-AREA         PIC X(32)   VALUE SPACE.                 
004700     03 CHKP-AREA-LENGTH         PIC S9(9)   VALUE +32 COMP SYNC.         
004800     03 CHKP-AREA                PIC X(32)   VALUE SPACE.                 
004900     03 CHKP-ANT                 PIC S9(5)   VALUE +0   COMP-3.           
005000     03 CHKP-MAX                 PIC S9(5)   VALUE +2000 COMP-3.          
005100 77  YES                         PIC X       VALUE 'J'.                   
005200 77  NOO                         PIC X       VALUE 'N'.                   
005300     SKIP2                                                                
005400 01  ERROR-TEXT.                                                          
005500     03  FILLER                  PIC X(16)   VALUE 'ERROR-TEXT'.          
005600     03  ERROR-TEXT-STR          PIC X(72)   VALUE SPACE.                 
005700                                                                          
005800 77  W21505-EOF-SW               PIC X       VALUE 'N'.                   
005900     88  END-OF-W21505                       VALUE 'Y'.                   
006000     EJECT                                                                
006100 01  TODAYS-DATE                 PIC 9(6)    VALUE ZERO.                  
006200 01  FILLER REDEFINES TODAYS-DATE.                                        
006300     03  TODAYS-DATE-YEAR        PIC 9(2).                                
006400     03  TODAYS-DATE-MONTH       PIC 9(2).                                
006500     03  TODAYS-DATE-DAY         PIC 9(2).                                
006600     EJECT                                                                
006700                                                                          
006800 77  REPLACE-SW                  PIC X   VALUE 'J'.                       
006900     88  REPLACE-PART                    VALUE 'J'.                       
007000                                                                          
007100 01  GENERAL-SUBPROGRAMS.                                                 
007200*                                                                         
007300     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
007400     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
007500     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
007600     EJECT                                                                
007700*    --- PARAMETRAR TILL POSTSUM                                          
007800*                                                                         
007900*01  -COPY W0005   -PRE  POSTSUM-                                         
008000     EJECT                                                                
008100 01  IN-AREA-START               PIC X(24)   VALUE                        
008200                                             'IN-AREA-START'.             
008300     SKIP2                                                                
008400 01  IN-AREA.                                                             
008500     03 IN-POST-TYPE           PIC X(2).                                  
008600     03 IN-FILLER1             PIC X(1).                                  
008700     03 IN-ARTNR-OVER          PIC X(8).                                  
008800     03 IN-FILLER2             PIC X(1).                                  
008900     03 IN-ARTNR-ING           PIC X(8).                                  
009000     03 IN-FILLER3             PIC X(1).                                  
009100     03 IN-NO-OF               PIC X(5).                                  
009200     03 IN-FILLER4             PIC X(1).                                  
009300     03 IN-T-AOINFTID6         PIC X(6).                                  
009400     03 IN-FILLER5             PIC X(1).                                  
009500     03 IN-U-AOINFTID6         PIC X(6).                                  
009600     03 IN-FILLER6             PIC X(1).                                  
009700     03 IN-ARTTYP              PIC X(4).                                  
009800     03 IN-FILLER7             PIC X(1).                                  
009900     03 IN-LEVEL               PIC X(2).                                  
010000     03 IN-FILLER8             PIC X(1).                                  
010100     03 IN-ARTBEN              PIC X(25).                                 
010200     03 IN-FILLER9             PIC X(1).                                  
010300     03 IN-RESMARK             PIC X(1).                                  
010400     03 IN-FILLER9             PIC X(1).                                  
010500     03 IN-ARTNR-SATS          PIC X(8).                                  
010600*                                                                         
010700     EJECT                                                                
010800 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
010900     SKIP3                                                                
011000 01  KEYS-TILL-DLI.                                                       
011100*WDJ401                                                                   
011200     03  W-IDARTNR-SATS-X.                                                
011300         05  W-IDARTNR-SATS      PIC S9(9)   VALUE ZERO COMP-3.           
011400*WDJ411                                                                   
011500     03  W-WDJ411-X.                                                      
011600         05  W-IDARTNR-J411      PIC S9(9)   VALUE ZERO COMP-3.           
012400     SKIP2                                                                
012500*    --- STATUS-KOD FRÅN IMS                                              
012600 01  STATUS-WS                   PIC XX.                                  
012700     88  SEGMENT-FOUND                       VALUE '  '.                  
012800     88  SEGMENT-FOUND-EXISTS                VALUE 'II'.                  
012900     88  SEGMENT-MISSING                     VALUE 'GE'.                  
013000     88  SEGMENT-NOMORE                      VALUE 'GB'.                  
013100     88  IMS-NOT-OK                          VALUE 'XD'.                  
013200     SKIP2                                                                
013300 01  GOOD-STATUSCODES.                                                    
013400     03  GOOD-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
013500     SKIP3                                                                
013600 01  SSA1                        PIC X(64).                               
013700 01  SSA2                        PIC X(64).                               
013800     EJECT                                                                
013900*    --- IMS FUNCTION CODES                                               
014000*01  -COPY W0003                                                          
014100     EJECT                                                                
014200*    ---  DLI INPUT-OUTPUT AREA                                           
014300                                                                          
014400**   KDP KIT PART                                                         
014500 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDJ401'.                      
014600 01  DLI-IO-WDJ401.                                                       
014700*    03  -COPY WDJ401                                                     
014800     EJECT                                                                
014900 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDJ411'.                      
015000 01  DLI-IO-WDJ411.                                                       
015100*    03  -COPY WDJ411                                                     
015200     EJECT                                                                
016300 LINKAGE SECTION.                                                         
016400                                                                          
016500*01  -COPY W0009   -PRE MSG-                                              
016600                                                                          
016700*01  -COPY W0008  -PRE WDJ4-                                              
016800     05  FILLER                  PIC X.                                   
016900                                                                          
017200     EJECT                                                                
017300 PROCEDURE DIVISION  USING MSG-PCB WDJ4-PCB.                              
017400 MAIN SECTION.                                                            
017500     ENTRY 'DLITCBL' USING MSG-PCB WDJ4-PCB.                              
017600                                                                          
017700     SKIP2                                                                
017800     PERFORM A-INIT                                                       
017900                                                                          
018000     PERFORM S01-READ-W21505                                              
018100     PERFORM UNTIL END-OF-W21505                                          
018200                                                                          
018300       MOVE IN-ARTNR-OVER           TO W-IDARTNR-SATS                     
018500       MOVE IN-ARTNR-OVER           TO WS-IDARTNR-OVER                    
018700                                                                          
018800       EVALUATE IN-POST-TYPE                                              
018900         WHEN '10'                                                        
018910           IF CHKP-ANT > CHKP-MAX                                         
018921             PERFORM IMS-CHECKPOINT                                       
018922             MOVE ZERO TO CHKP-ANT                                        
018930           END-IF                                                         
018940                                                                          
019000           PERFORM B-LOAD-WDJ401                                          
019100         WHEN '20'                                                        
019200           PERFORM C-LOAD-WDJ411                                          
019300       END-EVALUATE                                                       
019400                                                                          
019900       PERFORM S01-READ-W21505                                            
020000     END-PERFORM                                                          
020100                                                                          
020200     PERFORM Z-FINIT                                                      
020300                                                                          
020400     MOVE ZERO TO RETURN-CODE                                             
020500     GOBACK                                                               
020600     .                                                                    
020700     EJECT                                                                
020800 A-INIT SECTION.                                                          
020900     MOVE 'A-INIT             '   TO WS-PGM-POS                           
021000     SKIP2                                                                
021100                                                                          
021200     PERFORM IMS-RESTART                                                  
021300                                                                          
021400     OPEN INPUT W21505                                                    
021500                                                                          
021600     MOVE NOO                     TO REPLACE-SW                           
021700                                                                          
021800     MOVE IDPGM                   TO POSTSUM-PROGNAMN                     
021900     .                                                                    
022000     EJECT                                                                
022100                                                                          
022200 B-LOAD-WDJ401    SECTION.                                                
022300     MOVE 'B-LOAD-WDJ4     '      TO WS-PGM-POS                           
022400                                                                          
022500     PERFORM IMS-GHU-WDJ401                                               
022510                                                                          
022520     MOVE IN-ARTNR-OVER              TO KSART-IDARTNR-SATS                
022530     MOVE IN-ARTBEN                  TO KSART-BEART                       
022540     MOVE IN-NO-OF                   TO KSART-KVANTAL                     
022600                                                                          
022700     IF SEGMENT-FOUND                                                     
022800*REPLACE                                                                  
023000       PERFORM IMS-REPL-WDJ401                                            
023100       ADD +1                    TO CHKP-ANT                              
023200       MOVE YES                  TO REPLACE-SW                            
023300     ELSE                                                                 
023400*INSERT                                                                   
023600       PERFORM IMS-ISRT-WDJ401                                            
023700       ADD +1                    TO CHKP-ANT                              
023800       MOVE NOO                  TO REPLACE-SW                            
023900     END-IF                                                               
024000                                                                          
024100     .                                                                    
024200     EJECT                                                                
024300                                                                          
025300 C-LOAD-WDJ411       SECTION.                                             
025400     MOVE 'C-LOAD-WDJ411      '      TO WS-PGM-POS                        
025500                                                                          
025700     IF IN-ARTNR-OVER = IN-ARTNR-SATS                                     
025900                                                                          
025910*TEST IF PART IS IN MAIN KIT OR WITHIN INGOING KIT PART.                  
026000       IF REPLACE-PART                                                    
026100*REPLACE                                                                  
026110                                                                          
026120         MOVE IN-ARTNR-SATS          TO W-IDARTNR-SATS                    
026130         MOVE IN-ARTNR-ING           TO W-IDARTNR-J411                    
026140                                                                          
026150         PERFORM IMS-GHU-WDJ401                                           
026160                                                                          
026500         PERFORM IMS-GHNP-WDJ411                                          
026600         PERFORM CA-MOVE-TO-WDJ411                                        
026700         IF SEGMENT-FOUND                                                 
026900           PERFORM IMS-REPL-WDJ411                                        
027000           ADD +1                TO CHKP-ANT                              
027100         ELSE                                                             
027300           PERFORM IMS-ISRT-WDJ411                                        
027400           ADD +1                TO CHKP-ANT                              
027500         END-IF                                                           
027600       ELSE                                                               
027700*INSERT                                                                   
027800         MOVE WS-IDARTNR-OVER       TO W-IDARTNR-SATS                     
028200         PERFORM CA-MOVE-TO-WDJ411                                        
028400         PERFORM IMS-ISRT-WDJ411                                          
028500         ADD +1                  TO CHKP-ANT                              
028600       END-IF                                                             
028700     ELSE                                                                 
028800       PERFORM CB-LOAD-WDJ401                                             
028900       PERFORM CC-LOAD-WDJ411                                             
029100     END-IF                                                               
029200     .                                                                    
029300     EJECT                                                                
029400                                                                          
029500 CA-MOVE-TO-WDJ411     SECTION.                                           
029600     MOVE 'CA-MOVE-TO-WDJ411  '      TO WS-PGM-POS                        
029700                                                                          
029800     MOVE IN-ARTNR-ING               TO KSRAD-IDARTNR                     
029900     MOVE IN-ARTBEN                  TO KSRAD-BEART                       
030000     MOVE IN-ARTTYP                  TO KSRAD-KDARTTYP                    
030100     MOVE IN-RESMARK                 TO KSRAD-KDKITKDP                    
030200     MOVE IN-NO-OF                   TO KSRAD-KVANTAL                     
030300     MOVE IN-T-AOINFTID6             TO KSRAD-TIAAVV-FOM                  
030400     MOVE IN-U-AOINFTID6             TO KSRAD-TIAAVV-TOM                  
030500     .                                                                    
030600     EJECT                                                                
030700                                                                          
030800 CB-LOAD-WDJ401       SECTION.                                            
030900     MOVE 'CB-LOAD-WDJ401  '      TO WS-PGM-POS                           
031000                                                                          
031100     MOVE IN-ARTNR-SATS              TO W-IDARTNR-SATS                    
031200     MOVE IN-ARTNR-ING               TO W-IDARTNR-J411                    
031300                                                                          
031400     PERFORM IMS-GHU-WDJ401                                               
031500                                                                          
031600     MOVE IN-ARTNR-SATS              TO KSART-IDARTNR-SATS                
031700     MOVE IN-ARTBEN                  TO KSART-BEART                       
031800     MOVE IN-NO-OF                   TO KSART-KVANTAL                     
031900     IF SEGMENT-FOUND                                                     
032000*REPLACE                                                                  
032200       PERFORM IMS-REPL-WDJ401                                            
032300       ADD +1                    TO CHKP-ANT                              
032400       MOVE YES                  TO REPLACE-SW                            
032500     ELSE                                                                 
032600*INSERT                                                                   
032800       PERFORM IMS-ISRT-WDJ401                                            
032900       ADD +1                    TO CHKP-ANT                              
033000       MOVE NOO                  TO REPLACE-SW                            
033100     END-IF                                                               
033200                                                                          
033300     .                                                                    
033400     EJECT                                                                
033500                                                                          
033600 CC-LOAD-WDJ411       SECTION.                                            
033700     MOVE 'CC-LOAD-WDJ411  '      TO WS-PGM-POS                           
033800                                                                          
033820     MOVE IN-ARTNR-SATS              TO W-IDARTNR-SATS                    
033830     MOVE IN-ARTNR-ING               TO W-IDARTNR-J411                    
033840                                                                          
033850     PERFORM IMS-GHU-WDJ401                                               
033860                                                                          
034110     PERFORM CCA-MOVE-TO-WDJ411                                           
034200     IF REPLACE-PART                                                      
034300*REPLACE                                                                  
034400       PERFORM IMS-GHNP-WDJ411                                            
034500       IF SEGMENT-FOUND                                                   
034700         PERFORM IMS-REPL-WDJ411                                          
034800         ADD +1                  TO CHKP-ANT                              
034900       ELSE                                                               
035100         PERFORM IMS-ISRT-WDJ411                                          
035200         ADD +1                  TO CHKP-ANT                              
035300       END-IF                                                             
035400     ELSE                                                                 
035500*INSERT                                                                   
035800       PERFORM IMS-ISRT-WDJ411                                            
035900       ADD +1                    TO CHKP-ANT                              
036000     END-IF                                                               
036100     .                                                                    
036200     EJECT                                                                
036300                                                                          
036400 CCA-MOVE-TO-WDJ411      SECTION.                                         
036500     MOVE 'CCA-MOVE-TO-WDJ411 '      TO WS-PGM-POS                        
036510                                                                          
036600     MOVE IN-ARTNR-ING               TO KSRAD-IDARTNR                     
036800     MOVE IN-ARTBEN                  TO KSRAD-BEART                       
036900     MOVE IN-ARTTYP                  TO KSRAD-KDARTTYP                    
037000     MOVE IN-RESMARK                 TO KSRAD-KDKITKDP                    
037100     MOVE IN-NO-OF                   TO KSRAD-KVANTAL                     
037200     MOVE IN-T-AOINFTID6             TO KSRAD-TIAAVV-FOM                  
037300     MOVE IN-U-AOINFTID6             TO KSRAD-TIAAVV-TOM                  
037400     .                                                                    
037500     EJECT                                                                
037600                                                                          
037700 Z-FINIT SECTION.                                                         
037800     MOVE 'Z-FINIT            '      TO WS-PGM-POS                        
037900                                                                          
038000     CLOSE W21505                                                         
038100     SKIP2                                                                
038200     MOVE 'S' TO POSTSUM-OPKOD                                            
038300     CALL POSTSUM USING POSTSUM-PARM                                      
038400     .                                                                    
038500     EJECT                                                                
038600                                                                          
038700 S01-READ-W21505  SECTION.                                                
038800     MOVE 'S01-READ-W21505    '      TO WS-PGM-POS                        
038900     SKIP2                                                                
039000     READ W21505 INTO IN-AREA                                             
039100     AT END                                                               
039200****    MOVE HIGH-VALUE TO IN-ID                                          
039300        SET END-OF-W21505 TO TRUE                                         
039400                                                                          
039500     NOT AT END                                                           
039600        MOVE 'W21505' TO POSTSUM-FDNAMN                                   
039700        MOVE 'W21511D1' TO POSTSUM-DDNAMN2                                
039800        MOVE IN-AREA       TO POSTSUM-TRANSTYP                            
039900***     MOVE IN-IDPTYP TO POSTSUM-TRANSTYP                                
040000        CALL POSTSUM USING POSTSUM-PARM                                   
040100                                                                          
040200***     ADD 1 TO W-W21505-KVPOST-IN                                       
040300     END-READ                                                             
040400     .                                                                    
040500     EJECT                                                                
041700* --- IMS SECTIONS  ---                                                   
041800                                                                          
041900     EJECT                                                                
042000 IMS-GHU-WDJ401 SECTION.                                                  
042100     MOVE 'IMS-GHU-WDJ401  '         TO WS-PGM-IMS-POS                    
042200                                                                          
042300     STRING 'WDJ401  (IDARTNRS =' W-IDARTNR-SATS-X ')'                    
042400          DELIMITED BY SIZE INTO SSA1                                     
042500     MOVE '  GE' TO GOOD-STATUSCODES                                      
042600     CALL CBLTDLI USING GHU WDJ4-PCB DLI-IO-WDJ401 SSA1                   
042700     MOVE WDJ4-STATUS-CODE TO STATUS-WS                                   
042800     PERFORM IMS-STATUSCHECK                                              
042900     .                                                                    
043000     SKIP3                                                                
043100 IMS-ISRT-WDJ401 SECTION.                                                 
043200     MOVE 'IMS-ISRT-WDJ401 '         TO WS-PGM-IMS-POS                    
043300                                                                          
043400     MOVE 'WDJ401 ' TO SSA1                                               
043500     MOVE '  II' TO GOOD-STATUSCODES                                      
043600     CALL CBLTDLI USING ISRT WDJ4-PCB DLI-IO-WDJ401 SSA1                  
043700     MOVE WDJ4-STATUS-CODE TO STATUS-WS                                   
043800     PERFORM IMS-STATUSCHECK                                              
043900     .                                                                    
044000     SKIP3                                                                
044100 IMS-REPL-WDJ401 SECTION.                                                 
044200     MOVE 'IMS-REPL-WDJ401 '         TO WS-PGM-IMS-POS                    
044300                                                                          
044400     MOVE '  ' TO GOOD-STATUSCODES                                        
044500     CALL CBLTDLI USING REPL WDJ4-PCB DLI-IO-WDJ401                       
044600     MOVE WDJ4-STATUS-CODE TO STATUS-WS                                   
044700     PERFORM IMS-STATUSCHECK                                              
044800     .                                                                    
044900     EJECT                                                                
045000 IMS-GHNP-WDJ411 SECTION.                                                 
045100     MOVE 'IMS-GHNP-WDJ411 '         TO WS-PGM-IMS-POS                    
045200                                                                          
045300     STRING 'WDJ401  (IDARTNRS =' W-IDARTNR-SATS-X ')'                    
045400          DELIMITED BY SIZE INTO SSA1                                     
045500     STRING 'WDJ411  (IDARTNR  =' W-WDJ411-X ')'                          
045600          DELIMITED BY SIZE INTO SSA2                                     
045700     MOVE '  GE' TO GOOD-STATUSCODES                                      
045800     CALL CBLTDLI USING GHNP WDJ4-PCB DLI-IO-WDJ411 SSA1 SSA2             
045900     MOVE WDJ4-STATUS-CODE TO STATUS-WS                                   
046000     PERFORM IMS-STATUSCHECK                                              
046100     .                                                                    
046200     SKIP3                                                                
046300 IMS-ISRT-WDJ411 SECTION.                                                 
046400     MOVE 'IMS-ISRT-WDJ411 '         TO WS-PGM-IMS-POS                    
046500                                                                          
046600     STRING 'WDJ401  (IDARTNRS =' W-IDARTNR-SATS-X ')'                    
046700          DELIMITED BY SIZE INTO SSA1                                     
046800     MOVE 'WDJ411 ' TO SSA2                                               
046900     MOVE '  II' TO GOOD-STATUSCODES                                      
047000     CALL CBLTDLI USING ISRT WDJ4-PCB DLI-IO-WDJ411 SSA1 SSA2             
047100     MOVE WDJ4-STATUS-CODE TO STATUS-WS                                   
047200     PERFORM IMS-STATUSCHECK                                              
047300     .                                                                    
047400     SKIP3                                                                
047500 IMS-REPL-WDJ411 SECTION.                                                 
047600     MOVE 'IMS-REPL-WDJ411 '         TO WS-PGM-IMS-POS                    
047700                                                                          
047800     MOVE '  ' TO GOOD-STATUSCODES                                        
047900     CALL CBLTDLI USING REPL WDJ4-PCB DLI-IO-WDJ411                       
048000     MOVE WDJ4-STATUS-CODE TO STATUS-WS                                   
048100     PERFORM IMS-STATUSCHECK                                              
048200     .                                                                    
048300     EJECT                                                                
050600 IMS-RESTART SECTION.                                                     
050700     MOVE 'IMS-RESTART     '         TO WS-PGM-IMS-POS                    
050800     SKIP2                                                                
050900     MOVE SPACE TO CHKP-MSG-IO-AREA                                       
051000     MOVE '  ' TO GOOD-STATUSCODES                                        
051100     CALL CBLTDLI USING XRST MSG-PCB                                      
051200                        CHKP-MSG-IO-AREA-LENGTH CHKP-MSG-IO-AREA          
051300                        CHKP-AREA-LENGTH CHKP-AREA                        
051400     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
051500     PERFORM IMS-STATUSCHECK                                              
051600     .                                                                    
051700     SKIP3                                                                
051800 IMS-CHECKPOINT SECTION.                                                  
051900     MOVE 'IMS-CHECKPOINT  '         TO WS-PGM-IMS-POS                    
052000     SKIP2                                                                
052100     MOVE SPACE TO CHKP-MSG-IO-AREA                                       
052200     MOVE '  XD' TO GOOD-STATUSCODES                                      
052300     CALL CBLTDLI USING CHKP MSG-PCB                                      
052400                        CHKP-MSG-IO-AREA-LENGTH CHKP-MSG-IO-AREA          
052500                        CHKP-AREA-LENGTH CHKP-AREA                        
052600     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
052700     PERFORM IMS-STATUSCHECK                                              
052800                                                                          
052900     IF IMS-NOT-OK                                                        
053000       MOVE 'IMS CONTROL REGION IS NOT ACCESSIBLE'                        
053100         TO ERROR-TEXT-STR                                                
053200       DISPLAY ERROR-TEXT                                                 
053300       CALL FELLOG                                                        
053400     END-IF                                                               
053500     .                                                                    
053600     EJECT                                                                
053700 IMS-STATUSCHECK SECTION.                                                 
053800*    MOVE 'IMS-STATUSCHECK '         TO WS-PGM-IMS-POS                    
053900     SKIP2                                                                
054000     SET STATUS-IX TO 1                                                   
054100     SEARCH GOOD-STATUS                                                   
054200       AT END                                                             
054300         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
054400           DELIMITED BY SIZE INTO ERROR-TEXT                              
054500         DISPLAY ERROR-TEXT                                               
054600         CALL FELLOG                                                      
054700       WHEN GOOD-STATUS (STATUS-IX) = STATUS-WS                           
054800         CONTINUE                                                         
054900     END-SEARCH                                                           
055000     .                                                                    
