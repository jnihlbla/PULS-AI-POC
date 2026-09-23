000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W2212F00.                                                
000300 AUTHOR.         OLSSON SUSANNE.                                          
000400 DATE-WRITTEN.   16/11/18.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700                                                                          
000800*    FUNKTION:                                                            
000900*        PROGRAMMET UPPDATERAR "*" FLAGGAN PÅ NYA 220-LARM FÖR            
001000*        ARTIKLAR DÄR LEVERANTÖREN HAR FÅTT AUTOMATISKA MAIL.             
001100*        SE BILD 2172/2472                                                
001200*        TEXTFÄLTET PÅ BILD 2106/2406 LEVERANSBESKED UPPDATERAS.          
001300*                                                                         
001400*        PROGRAMMET UPPDATERAR WDD9                                       
001500*        PROGRAMMET UPPDATERAR WDD4                                       
001600*                                                                         
001700                                                                          
001800     SKIP3                                                                
001900 ENVIRONMENT DIVISION.                                                    
002000     SKIP2                                                                
002100 INPUT-OUTPUT SECTION.                                                    
002200                                                                          
002300 FILE-CONTROL.                                                            
002400     SKIP2                                                                
002500*          --- UPPDATERINGSPOSTER FRÅN W2212E00                           
002600     SELECT W2212F                     ASSIGN TO W2212FD1.                
002700     EJECT                                                                
002800 DATA DIVISION.                                                           
002900     SKIP3                                                                
003000 FILE SECTION.                                                            
003100     SKIP3                                                                
003200 FD  W2212F                                                               
003300     RECORDING       F                                                    
003400     BLOCK CONTAINS  0.                                                   
003500                                                                          
003600*01  -COPY W2212F      -L.                                                
003700     EJECT                                                                
003800 WORKING-STORAGE SECTION.                                                 
003900                                                                          
004000 77  IDPGM                       PIC X(8)    VALUE 'W2212F00'.            
004100 77  CURRENT-SECTION             PIC X(30)   VALUE SPACE.                 
004200 77  DBS-SECTION                 PIC X(30)   VALUE SPACE.                 
004300 77  WS-TIBORT                   PIC 9(6)    VALUE ZERO.                  
004400 77  WS-TIBORT-8                 PIC 9(8)    VALUE ZERO.                  
004500 77  WS-TIBORT-UPD               PIC 9(6)    VALUE ZERO.                  
004600 77  WS-TIBORT-UPD-YYYYMMDD      PIC 9(8)    VALUE ZERO.                  
004610 77  SW-UPPDAT-TIBORT            PIC X       VALUE 'N'.                   
004620 77  SPAR-TIBORT-UPD             PIC S9(7)   VALUE ZERO COMP-3.           
004700                                                                          
004800 01  W-W2212F-KVPOST-IN          PIC S9(5)   VALUE ZERO COMP-3.           
004900                                                                          
005000 01  CHKP-VAR.                                                            
005100     03 CHKP-MSG-IO-AREA-LENGTH  PIC S9(9)   VALUE +32 COMP SYNC.         
005200     03 CHKP-MSG-IO-AREA         PIC X(32)   VALUE SPACE.                 
005300     03 CHKP-AREA-LENGTH         PIC S9(9)   VALUE +32 COMP SYNC.         
005400     03 CHKP-AREA                PIC X(32)   VALUE SPACE.                 
005500     03 CHKP-ANT                 PIC S9(3)   VALUE +0   COMP-3.           
005600     03 CHKP-MAX                 PIC S9(3)   VALUE +500 COMP-3.           
005700 77  JA                          PIC X       VALUE 'J'.                   
005800 77  NEJ                         PIC X       VALUE 'N'.                   
005900     SKIP2                                                                
006000 01  FELTEXT.                                                             
006100     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
006200     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
006300                                                                          
006400 77  W2212F-EOF-SW               PIC X       VALUE 'N'.                   
006500     88  END-OF-W2212F                       VALUE 'J'.                   
006600                                                                          
006700 01  SW-TEXT-UPPD                PIC X       VALUE 'N'.                   
006800     88  TEXT-UPPD                           VALUE 'J'.                   
006900     88  NO-TEXT-UPPD                        VALUE 'N'.                   
007000                                                                          
007500     EJECT                                                                
007700 01  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
007800 01  FILLER REDEFINES DAGENS-DATUM.                                       
007900     03  DAGENS-DATUM-AAR        PIC 9(2).                                
008000     03  DAGENS-DATUM-MAANAD     PIC 9(2).                                
008100     03  DAGENS-DATUM-DAG        PIC 9(2).                                
008200                                                                          
008300                                                                          
008400 01  DAGENS-TID                  PIC 9(8)    VALUE ZERO.                  
008500 01  FILLER    REDEFINES DAGENS-TID.                                      
008600     03  DAGENS-HHMMSS PIC 9(6).                                          
008700     03  FILLER        PIC 9(2).                                          
008800                                                                          
008900     EJECT                                                                
009000 01  DYNAMISKA-SUBPROGRAM.                                                
009100*                                                                         
009200     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
009300     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
009400     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
009500     03  WZ20DAYS                PIC X(8)    VALUE 'WZ20DAYS'.            
009600     EJECT                                                                
009700*    --- PARAMETRAR TILL POSTSUM                                          
009800*                                                                         
009900*01  -COPY W0005   -PRE  POSTSUM-                                         
010000     EJECT                                                                
010100*    --- PARAMETRAR TILL WZ20DAYS                                         
010200 01  FILLER                      PIC X(16)   VALUE 'WZ20DAYS'.            
010300*01  -COPY WZ20DAYS                                                       
010400     EJECT                                                                
010500 01  IN-AREA-START               PIC X(24)   VALUE                        
010600                                             'IN-AREA-START'.             
010700     SKIP2                                                                
010800                                                                          
010900*01  AREA -COPY W2212F     -PRE IN-                                       
011000*                                                                         
011100     EJECT                                                                
011200 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
011300     SKIP3                                                                
011400 01  NYCKLAR-TILL-DLI.                                                    
011500     03  W-WDD901KY-X.                                                    
011600         05 W-IDARTNR            PIC S9(9)   VALUE ZERO COMP-3.           
011700         05 W-IDDC               PIC X(2)    VALUE SPACE.                 
011800                                                                          
011900     03  W-IDLEVNR-X.                                                     
012000         05  W-IDLEVNR           PIC X(5)    VALUE SPACE.                 
012100                                                                          
012200     03  W-IDLEVBSK-X.                                                    
012300         05  W-IDLEVBSK          PIC S9      VALUE ZERO COMP-3.           
012400                                                                          
012500     03  W-WDD401KY-X.                                                    
012600         05  W-DAREGDAT-9KOMPL   PIC 9(8)    VALUE ZERO.                  
012700         05  W-TIKLOCK-9KOMPL    PIC S9(9)   VALUE ZERO COMP-3.           
012800     SKIP2                                                                
012900*    --- STATUS-KOD FRÅN IMS                                              
013000 01  STATUS-WS                   PIC XX.                                  
013100     88  SEGMENT-FINNS                       VALUE '  '.                  
013200     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
013300     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
013400     88  SEGMENT-SLUT                        VALUE 'GB'.                  
013500     88  IMS-EJ-OK                           VALUE 'XD'.                  
013600     SKIP2                                                                
013700 01  GODK-STATUSKODER.                                                    
013800     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
013900     SKIP3                                                                
014000 01  SSA1                        PIC X(64).                               
014100 01  SSA2                        PIC X(64).                               
014110 01  SSA3                        PIC X(64).                               
014200     EJECT                                                                
014300*    --- IMS FUNKTIONSKODER                                               
014400*01  -COPY W0003                                                          
014500     EJECT                                                                
014600*    ---  DLI INPUT-OUTPUT AREA                                           
014700                                                                          
014800 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDD902'.                      
014900 01  DLI-IO-WDD902.                                                       
015000*    03  -COPY WDD902                                                     
015100     EJECT                                                                
015200 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDD924'.                      
015210 01  DLI-IO-WDD924.                                                       
015220*    03  -COPY WDD924                                                     
015221     EJECT                                                                
015222 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDD925'.                      
015223 01  DLI-IO-WDD925.                                                       
015224*    03  -COPY WDD925                                                     
015230     EJECT                                                                
015300 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDD401'.                      
015400 01  DLI-IO-WDD401.                                                       
015500*    03  -COPY WDD401                                                     
015600                                                                          
015700     EJECT                                                                
015800 LINKAGE SECTION.                                                         
015900                                                                          
016000*01  -COPY W0009   -PRE MSG-                                              
016100                                                                          
016200*01  -COPY W0008  -PRE WDD9-                                              
016300     05  FILLER                  PIC X.                                   
016400                                                                          
016500*01  -COPY W0008  -PRE WDD4-                                              
016600     05  FILLER                  PIC X.                                   
016700     EJECT                                                                
016800 PROCEDURE DIVISION  USING MSG-PCB WDD9-PCB WDD4-PCB.                     
016900 MAIN SECTION.                                                            
017000     ENTRY 'DLITCBL' USING MSG-PCB WDD9-PCB WDD4-PCB.                     
017100                                                                          
017200     SKIP2                                                                
017300     PERFORM A-INIT                                                       
017400     PERFORM S01-LAES-W2212F                                              
017500     PERFORM UNTIL END-OF-W2212F                                          
017600       IF CHKP-ANT > CHKP-MAX                                             
017700         PERFORM X-TAG-CHECKPOINT                                         
017800       END-IF                                                             
017900                                                                          
018000       PERFORM B-BEHANDLA-LARM-2172                                       
018100                                                                          
018300       PERFORM C-HANDLE-TEXT-2106                                         
018390                                                                          
018400       PERFORM S01-LAES-W2212F                                            
018500     END-PERFORM                                                          
018600                                                                          
018800     PERFORM Z-FINIT                                                      
018900                                                                          
019000     MOVE ZERO TO RETURN-CODE                                             
019100     GOBACK                                                               
019200     .                                                                    
019300     EJECT                                                                
019400 A-INIT SECTION.                                                          
019500     SKIP2                                                                
019600                                                                          
019700     PERFORM IMS-RESTART                                                  
019800                                                                          
019900     OPEN INPUT W2212F                                                    
020000                                                                          
020400     ACCEPT DAGENS-DATUM FROM DATE                                        
020500     ACCEPT DAGENS-TID   FROM TIME                                        
020600                                                                          
020700     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
020800                                                                          
020900     PERFORM AA-HAEMTA-TIBORT-UPD                                         
021000                                                                          
021100     .                                                                    
021200     EJECT                                                                
021300 AA-HAEMTA-TIBORT-UPD  SECTION.                                           
021400     MOVE 'AA-HAEMTA-TIBORT-UPD '  TO CURRENT-SECTION                     
021500                                                                          
021600     MOVE DAGENS-DATUM        TO DAYS-TIDATE1                             
021700     MOVE 'YYMMDD'            TO DAYS-KDDATFMT1                           
021800     MOVE 'YYYYMMDD'          TO DAYS-KDDATFMT2                           
021900     MOVE +7                  TO DAYS-KVDAYS                              
022000     MOVE SPACE               TO DAYS-TIDATE2                             
022100                                 DAYS-IDCALEND                            
022200                                                                          
022300     CALL WZ20DAYS USING DAYS-WZ20DAYS                                    
022400                                                                          
022500     IF DAYS-KDRC = 8                                                     
022600       MOVE 'FEL VID ANROP TILL WZ20DAYS 1 ' TO FELTEXT                   
022700                                                                          
022800       CALL FELLOG                                                        
022900     ELSE                                                                 
023000       MOVE DAYS-TIDATE2(1:8) TO WS-TIBORT-UPD-YYYYMMDD                   
023100       MOVE DAYS-TIDATE2(3:6) TO WS-TIBORT-UPD                            
023200     END-IF                                                               
023300     .                                                                    
023400     EJECT                                                                
023410******************************************************************        
023411*WHEN MAILS ARE SENT TO SUPPLIER FOR PREADVICE DEVIATION,DELETE           
023412*THE ALERT FROM THE SCREEN 2172/2472(B4 THIS CHANGE, THE REC STILL        
023413*APPEARS IN THE 2172/2472.ONLY THE FLNYLARM FIELD IS SPACED OUT.          
023414*PURCHASE PLANNER HAS TO MANUALLY DELETE THE RECORD.)                     
023420******************************************************************        
023500 B-BEHANDLA-LARM-2172  SECTION.                                           
023600     MOVE 'B-BEHANDLA-LARM-2172 '  TO CURRENT-SECTION                     
023700                                                                          
023800     MOVE IN-DAREGDAT-9KOMPL   TO W-DAREGDAT-9KOMPL                       
023900     MOVE IN-TIKLOCK-9KOMPL    TO W-TIKLOCK-9KOMPL                        
024000                                                                          
024100     PERFORM IMS-GHU-WDD401                                               
024200     IF SEGMENT-FINNS                                                     
025010       PERFORM IMS-DLET-WDD401                                            
025020       ADD +1 TO CHKP-ANT                                                 
025030     END-IF                                                               
025100     .                                                                    
025200     EJECT                                                                
025300******************************************************************        
025400**           HANDLING THE TEXT IN WDD925                        **        
025500*WHEN PRE ADVICE DELIVERY IS PRESENT:1)NO TEXT INSERTED.2)TIBORT *        
025600*DATE UPDATED IF NECESSARY.                                               
025610*WHEN PRE ADVICE DELIVERY IS MISSING:1)CHECK IF FIRST LINE("2")  *        
025700*IS PRESENT.IF YES UPDATE TIBORT DATE IN ALL WDD925 SEGMENTS.    *        
025710*2)IF FIRST LINE("2") MISSING,INSERT TEXT FOR SEGMENT "2".       *        
025720*3)IF USER UPATES LINES 2 OR 3 OR 4 IN 2106 & LINE 1 IS EMPTY,   *        
025730*INSERT "WAITING FOR DELIVERY INFO IN LINE 1("2").               *        
025800******************************************************************        
032413 C-HANDLE-TEXT-2106   SECTION.                                            
032414     MOVE 'C-HANDLE-TEXT-2106   '  TO CURRENT-SECTION                     
032415                                                                          
032416     MOVE NEJ                      TO SW-TEXT-UPPD                        
032417     MOVE NEJ                      TO SW-UPPDAT-TIBORT                    
032418     MOVE IN-IDARTNR               TO W-IDARTNR                           
032419     MOVE IN-IDDC                  TO W-IDDC                              
032420     MOVE IN-IDLEVNR               TO W-IDLEVNR                           
032421                                                                          
032422     PERFORM IMS-GU-WDD902                                                
032423     IF SEGMENT-FINNS                                                     
032425        PERFORM IMS-GNP-WDD924                                            
032426        IF SEGMENT-FINNS                                                  
032428           PERFORM CA-BEHANDLA-TIBORT                                     
032429           MOVE '2'                TO W-IDLEVBSK                          
032430           PERFORM IMS-GHU-WDD925                                         
032431           IF SEGMENT-FINNS AND SW-UPPDAT-TIBORT = JA                     
032433              PERFORM S02-REPL-TIBORT-WDD925                              
032434           END-IF                                                         
032435           MOVE '4'             TO W-IDLEVBSK                             
032436           PERFORM IMS-GHU-WDD925                                         
032437           IF SEGMENT-FINNS AND SW-UPPDAT-TIBORT = JA                     
032439              PERFORM S02-REPL-TIBORT-WDD925                              
032440           END-IF                                                         
032441           MOVE '5'                TO W-IDLEVBSK                          
032442           PERFORM IMS-GHU-WDD925                                         
032443           IF SEGMENT-FINNS AND SW-UPPDAT-TIBORT = JA                     
032445              PERFORM S02-REPL-TIBORT-WDD925                              
032446           END-IF                                                         
032447           MOVE '6'             TO W-IDLEVBSK                             
032448           PERFORM IMS-GHU-WDD925                                         
032449           IF SEGMENT-FINNS AND SW-UPPDAT-TIBORT = JA                     
032451              PERFORM S02-REPL-TIBORT-WDD925                              
032452           END-IF                                                         
032458        ELSE                                                              
032460           PERFORM CA-BEHANDLA-TIBORT                                     
032461           IF SEGMENT-FINNS                                               
032463              MOVE '2'           TO W-IDLEVBSK                            
032464              PERFORM IMS-GHU-WDD925                                      
032465              IF SEGMENT-FINNS                                            
032466                 IF SW-UPPDAT-TIBORT = JA                                 
032467                   PERFORM S02-REPL-TIBORT-WDD925                         
032468                 END-IF                                                   
032469              ELSE                                                        
032470                 PERFORM CB-INSERT-WDD925                                 
032471              END-IF                                                      
032479              MOVE '4'           TO W-IDLEVBSK                            
032480              PERFORM IMS-GHU-WDD925                                      
032481              IF SEGMENT-FINNS AND SW-UPPDAT-TIBORT = JA                  
032482                 PERFORM S02-REPL-TIBORT-WDD925                           
032483              END-IF                                                      
032484              MOVE '5'           TO W-IDLEVBSK                            
032485              PERFORM IMS-GHU-WDD925                                      
032486              IF SEGMENT-FINNS AND SW-UPPDAT-TIBORT = JA                  
032487                 PERFORM S02-REPL-TIBORT-WDD925                           
032488              END-IF                                                      
032489              MOVE '6'           TO W-IDLEVBSK                            
032490              PERFORM IMS-GHU-WDD925                                      
032491              IF SEGMENT-FINNS AND SW-UPPDAT-TIBORT = JA                  
032492                 PERFORM S02-REPL-TIBORT-WDD925                           
032493              END-IF                                                      
032494           ELSE                                                           
032495              PERFORM CB-INSERT-WDD925                                    
032506           END-IF                                                         
032507        END-IF                                                            
032508     END-IF                                                               
032509     .                                                                    
032510     EJECT                                                                
032511 CA-BEHANDLA-TIBORT   SECTION.                                            
032512     MOVE 'CA-BEHANDLA-TIBORT '  TO CURRENT-SECTION                       
032513                                                                          
032514     MOVE ZERO                  TO SPAR-TIBORT-UPD                        
032515                                                                          
032516     PERFORM IMS-GNP-WDD925                                               
032517     IF SEGMENT-FINNS                                                     
032519       IF INFO-TIBORT = ZERO                                              
032520         MOVE JA                TO SW-UPPDAT-TIBORT                       
032521       ELSE                                                               
032522         MOVE INFO-TIBORT       TO WS-TIBORT                              
032523         MOVE WS-TIBORT         TO DAYS-TIDATE1                           
032524         MOVE 'YYMMDD'          TO DAYS-KDDATFMT1                         
032525         MOVE 'YYYYMMDD'        TO DAYS-KDDATFMT2                         
032526         MOVE 0                 TO DAYS-KVDAYS                            
032527         MOVE SPACE             TO DAYS-TIDATE2                           
032528                                     DAYS-IDCALEND                        
032529                                                                          
032530         CALL WZ20DAYS USING DAYS-WZ20DAYS                                
032531                                                                          
032532         IF DAYS-KDRC = 8                                                 
032533           MOVE 'FEL VID ANROP TILL WZ20DAYS 2 '                          
032534                                TO FELTEXT                                
032535           CALL FELLOG                                                    
032536         ELSE                                                             
032537           MOVE DAYS-TIDATE2(1:8) TO WS-TIBORT-8                          
032538           IF WS-TIBORT-8 > WS-TIBORT-UPD-YYYYMMDD                        
032539             MOVE INFO-TIBORT     TO SPAR-TIBORT-UPD                      
032540           ELSE                                                           
032541             IF WS-TIBORT-8 = WS-TIBORT-UPD-YYYYMMDD                      
032542               CONTINUE                                                   
032543             ELSE                                                         
032544               MOVE JA            TO SW-UPPDAT-TIBORT                     
032545             END-IF                                                       
032546           END-IF                                                         
032547         END-IF                                                           
032548       END-IF                                                             
032549     ELSE                                                                 
032550       MOVE JA              TO SW-UPPDAT-TIBORT                           
032551     END-IF                                                               
032552     .                                                                    
032553     EJECT                                                                
032554 CB-INSERT-WDD925   SECTION.                                              
032555     MOVE 'CB-INSERT-WDD925     '  TO CURRENT-SECTION                     
032556                                                                          
032558     MOVE '2'           TO INFO-IDLEVBSK                                  
032559     MOVE 'No eta available. Pushing supplier for info.'                  
032560                        TO INFO-TELEVBSK                                  
032561     MOVE WS-TIBORT-UPD     TO INFO-TIBORT                                
032562     MOVE DAGENS-DATUM      TO INFO-TIREGDAT                              
032563     MOVE DAGENS-HHMMSS     TO INFO-TIREGTID                              
032564                                                                          
032565     PERFORM IMS-ISRT-WDD925                                              
032566     ADD +1 TO CHKP-ANT                                                   
032567     .                                                                    
032568     EJECT                                                                
032569 Z-FINIT SECTION.                                                         
032570                                                                          
032571                                                                          
032580     CLOSE W2212F                                                         
032600     SKIP2                                                                
032700     MOVE 'S' TO POSTSUM-OPKOD                                            
032800     CALL POSTSUM USING POSTSUM-PARM                                      
032900     .                                                                    
033000     EJECT                                                                
033100 S01-LAES-W2212F  SECTION.                                                
033200     SKIP2                                                                
033300     READ W2212F INTO IN-AREA                                             
033400     AT END                                                               
033500        MOVE HIGH-VALUE   TO IN-AREA                                      
033600        SET END-OF-W2212F TO TRUE                                         
033700                                                                          
033800     NOT AT END                                                           
033900        MOVE 'W2212F'   TO POSTSUM-FDNAMN                                 
034000        MOVE 'W2212FD1' TO POSTSUM-DDNAMN2                                
034100        MOVE 'IN  '     TO POSTSUM-TRANSTYP                               
034200        CALL POSTSUM USING POSTSUM-PARM                                   
034300                                                                          
034400        ADD 1 TO W-W2212F-KVPOST-IN                                       
034500     END-READ                                                             
034600     .                                                                    
034700     EJECT                                                                
034800 S02-REPL-TIBORT-WDD925 SECTION.                                          
034900                                                                          
034901     MOVE 'S02-REPL-TIBORT-WDD925' TO CURRENT-SECTION                     
034902                                                                          
034903     MOVE WS-TIBORT-UPD   TO INFO-TIBORT                                  
034904     PERFORM IMS-REPL-WDD925                                              
034905     ADD +1 TO CHKP-ANT                                                   
034910     .                                                                    
034920     EJECT                                                                
034930 X-TAG-CHECKPOINT   SECTION.                                              
034940                                                                          
035000* --- VID CHECKPOINTTAGGNING SÅ TAPPAR MAN GN-POSITION I BASEN            
035100* --- SPARA DATABASNYCKLAR OM DET BEHÖVS                                  
035200     PERFORM IMS-CHECKPOINT                                               
035300     MOVE ZERO TO CHKP-ANT                                                
035400* --- LÄS OM DATABAS OM DET BEHÖVS                                        
035500     .                                                                    
035600     EJECT                                                                
035700* --- IMS SEKTIONER ---                                                   
035800                                                                          
035900     EJECT                                                                
036000 IMS-GU-WDD902  SECTION.                                                  
036100     MOVE 'IMS-GU-WDD902 '  TO DBS-SECTION                                
036200                                                                          
036300     MOVE SPACE  TO SSA1 SSA2                                             
036400     STRING 'WDD901  (WDD901KY =' W-WDD901KY-X ')'                        
036500          DELIMITED BY SIZE INTO SSA1                                     
036600     STRING 'WDD902  (IDLEVNR  =' W-IDLEVNR-X ')'                         
036700          DELIMITED BY SIZE INTO SSA2                                     
036800     CALL CBLTDLI USING GU WDD9-PCB DLI-IO-WDD902 SSA1 SSA2               
036900     MOVE WDD9-STATUS-CODE TO STATUS-WS                                   
037000     PERFORM IMS-STATUSKONTROLL                                           
037100     .                                                                    
037200     EJECT                                                                
037300 IMS-GNP-WDD924 SECTION.                                                  
037400     MOVE 'IMS-GNP-WDD924 '  TO DBS-SECTION                               
037500                                                                          
037600     MOVE SPACE  TO SSA1                                                  
037910     MOVE 'WDD924  *F ' TO SSA1                                           
038000     MOVE '  GE' TO GODK-STATUSKODER                                      
038100     CALL CBLTDLI USING GNP WDD9-PCB DLI-IO-WDD924 SSA1                   
038200     MOVE WDD9-STATUS-CODE TO STATUS-WS                                   
038300     PERFORM IMS-STATUSKONTROLL                                           
038400     .                                                                    
038500     SKIP3                                                                
038501 IMS-GNP-WDD925 SECTION.                                                  
038502     MOVE 'IMS-GNP-WDD925 '  TO DBS-SECTION                               
038503                                                                          
038504     MOVE SPACE  TO SSA1                                                  
038505     MOVE 'WDD925  '  TO SSA1                                             
038506     MOVE '  GE' TO GODK-STATUSKODER                                      
038507     CALL CBLTDLI USING GNP WDD9-PCB DLI-IO-WDD925 SSA1                   
038508     MOVE WDD9-STATUS-CODE TO STATUS-WS                                   
038510     PERFORM IMS-STATUSKONTROLL                                           
038511     .                                                                    
038512     SKIP3                                                                
038513 IMS-GHU-WDD925 SECTION.                                                  
038520     MOVE 'IMS-GHU-WDD925 '  TO DBS-SECTION                               
038530                                                                          
038541     MOVE SPACE  TO SSA1 SSA2 SSA3                                        
038542     STRING 'WDD901  (WDD901KY =' W-WDD901KY-X ')'                        
038543          DELIMITED BY SIZE INTO SSA1                                     
038544     STRING 'WDD902  (IDLEVNR  =' W-IDLEVNR-X ')'                         
038545          DELIMITED BY SIZE INTO SSA2                                     
038550     STRING 'WDD925  (IDLEVBSK =' W-IDLEVBSK-X ')'                        
038560          DELIMITED BY SIZE INTO SSA3                                     
038580     MOVE '  GE' TO GODK-STATUSKODER                                      
038590     CALL CBLTDLI USING GHU WDD9-PCB DLI-IO-WDD925 SSA1 SSA2 SSA3         
038591     MOVE WDD9-STATUS-CODE TO STATUS-WS                                   
038592     PERFORM IMS-STATUSKONTROLL                                           
038593     .                                                                    
038594     SKIP3                                                                
038600 IMS-ISRT-WDD925 SECTION.                                                 
038700     MOVE 'IMS-ISRT-WDD925 '  TO DBS-SECTION                              
038800                                                                          
038900     MOVE SPACE  TO SSA1 SSA2 SSA3                                        
039000     STRING 'WDD901  (WDD901KY =' W-WDD901KY-X ')'                        
039100          DELIMITED BY SIZE INTO SSA1                                     
039200     STRING 'WDD902  (IDLEVNR  =' W-IDLEVNR-X ')'                         
039300          DELIMITED BY SIZE INTO SSA2                                     
039400     MOVE 'WDD925 ' TO SSA3                                               
039600     MOVE '    ' TO GODK-STATUSKODER                                      
039700     CALL CBLTDLI USING ISRT WDD9-PCB DLI-IO-WDD925 SSA1 SSA2 SSA3        
039800     MOVE WDD9-STATUS-CODE TO STATUS-WS                                   
039900     PERFORM IMS-STATUSKONTROLL                                           
040000     .                                                                    
040100     SKIP3                                                                
040200 IMS-REPL-WDD925 SECTION.                                                 
040300     MOVE 'IMS-REPL-WDD925 '  TO DBS-SECTION                              
040400                                                                          
040500     MOVE '  ' TO GODK-STATUSKODER                                        
040600     CALL CBLTDLI USING REPL WDD9-PCB DLI-IO-WDD925                       
040700     MOVE WDD9-STATUS-CODE TO STATUS-WS                                   
040800     PERFORM IMS-STATUSKONTROLL                                           
040900     .                                                                    
041000     EJECT                                                                
041100 IMS-GHU-WDD401 SECTION.                                                  
041200     MOVE 'IMS-GHU-WDD401 '   TO DBS-SECTION                              
041300                                                                          
041400     MOVE SPACE  TO SSA1                                                  
041500     STRING 'WDD401  (WDD401KY =' W-WDD401KY-X ')'                        
041600          DELIMITED BY SIZE INTO SSA1                                     
041700     MOVE '  GE' TO GODK-STATUSKODER                                      
041800     CALL CBLTDLI USING GHU WDD4-PCB DLI-IO-WDD401 SSA1                   
041900     MOVE WDD4-STATUS-CODE TO STATUS-WS                                   
042000     PERFORM IMS-STATUSKONTROLL                                           
042100     .                                                                    
042200     SKIP3                                                                
043110 IMS-DLET-WDD401 SECTION.                                                 
043120     MOVE 'IMS-DLET-WDD401 '  TO DBS-SECTION                              
043130                                                                          
043140     MOVE '  ' TO GODK-STATUSKODER                                        
043150     CALL CBLTDLI USING DLET WDD4-PCB DLI-IO-WDD401                       
043160     MOVE WDD4-STATUS-CODE TO STATUS-WS                                   
043170     PERFORM IMS-STATUSKONTROLL                                           
043180     .                                                                    
043190     EJECT                                                                
043200 IMS-RESTART SECTION.                                                     
043300     SKIP2                                                                
043400     MOVE SPACE TO CHKP-MSG-IO-AREA                                       
043500     MOVE '  ' TO GODK-STATUSKODER                                        
043600     CALL CBLTDLI USING XRST MSG-PCB                                      
043700                        CHKP-MSG-IO-AREA-LENGTH CHKP-MSG-IO-AREA          
043800                        CHKP-AREA-LENGTH CHKP-AREA                        
043900     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
044000     PERFORM IMS-STATUSKONTROLL                                           
044100     .                                                                    
044200     SKIP3                                                                
044300 IMS-CHECKPOINT SECTION.                                                  
044400     SKIP2                                                                
044500     MOVE SPACE TO CHKP-MSG-IO-AREA                                       
044600     MOVE '  XD' TO GODK-STATUSKODER                                      
044700     CALL CBLTDLI USING CHKP MSG-PCB                                      
044800                        CHKP-MSG-IO-AREA-LENGTH CHKP-MSG-IO-AREA          
044900                        CHKP-AREA-LENGTH CHKP-AREA                        
045000     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
045100     PERFORM IMS-STATUSKONTROLL                                           
045200                                                                          
045300     IF IMS-EJ-OK                                                         
045400       MOVE 'IMS-KONTROLLREGION EJ TILLGÄNGLIG' TO FELTEXT-STR            
045500       DISPLAY FELTEXT                                                    
045600       CALL FELLOG                                                        
045700     END-IF                                                               
045800     .                                                                    
045900     EJECT                                                                
046000 IMS-STATUSKONTROLL SECTION.                                              
046100     SKIP2                                                                
046200     SET STATUS-IX TO 1                                                   
046300     SEARCH GODK-STATUS                                                   
046400       AT END                                                             
046500         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
046600           DELIMITED BY SIZE INTO FELTEXT                                 
046700         DISPLAY FELTEXT                                                  
046800         CALL FELLOG                                                      
046900       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
047000         CONTINUE                                                         
047100     END-SEARCH                                                           
047200     .                                                                    
