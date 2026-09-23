000900 ID DIVISION.                                                             
001000 PROGRAM-ID.     W2151000.                                                
001100 AUTHOR.         ANDERSSON BERT.                                          
001200 DATE-WRITTEN.   20/12/17.                                                
001300 DATE-COMPILED.                                                           
001400                                                                          
001500                                                                          
001600*    FUNCTION:                                                            
001700*        DIFF BETWEEN W21501 GENERATION +0 AND -1                         
001800*        OUTFILE W21505 FOR UPDATE OF WDJ4                                
001900*                                                                         
002000                                                                          
002100     SKIP3                                                                
002200 ENVIRONMENT DIVISION.                                                    
002300     SKIP2                                                                
002400 INPUT-OUTPUT SECTION.                                                    
002500                                                                          
002600 FILE-CONTROL.                                                            
002700     SKIP2                                                                
002800*          --- W21501(+0)                                                 
002900     SELECT W21501A                    ASSIGN TO W21510D1.                
003000     SKIP2                                                                
003100*          --- W21501(-1)                                                 
003200     SELECT W21501B                    ASSIGN TO W21510D2.                
003300     SKIP2                                                                
003400*          --- W21505(+0)                                                 
003500     SELECT W21505                     ASSIGN TO W21510D3.                
003600     EJECT                                                                
003700 DATA DIVISION.                                                           
003800     SKIP3                                                                
003900 FILE SECTION.                                                            
004000     SKIP3                                                                
004100 FD  W21501A                                                              
004200     RECORDING       F                                                    
004300     BLOCK CONTAINS  0.                                                   
004500 01  IN1-POST.                                                            
004510     03 INA-POST-TYPE          PIC X(2).                                  
004520     03 INA-FILLER1            PIC X(1).                                  
004530     03 INA-ARTNR-OVER         PIC X(8).                                  
004540     03 INA-FILLER2            PIC X(1).                                  
004550     03 INA-ARTNR-ING          PIC X(8).                                  
004560     03 INA-FILLER3            PIC X(1).                                  
004570     03 INA-NO-OF              PIC X(5).                                  
004580     03 INA-FILLER4            PIC X(1).                                  
004590     03 INA-T-AOINFTID6        PIC X(6).                                  
004591     03 INA-FILLER5            PIC X(1).                                  
004592     03 INA-U-AOINFTID6        PIC X(6).                                  
004593     03 INA-FILLER6            PIC X(1).                                  
004594     03 INA-ARTTYP             PIC X(4).                                  
004595     03 INA-FILLER7            PIC X(1).                                  
004596     03 INA-LEVEL              PIC X(2).                                  
004597     03 INA-FILLER8            PIC X(1).                                  
004598     03 INA-ARTBEN             PIC X(25).                                 
004599     03 INA-FILLER9            PIC X(1).                                  
004600     03 INA-RESMARK            PIC X(1).                                  
004601     03 INA-FILLER10           PIC X(1).                                  
004602     03 INA-ARTNR-SATS         PIC X(8).                                  
004604     03 INA-FILLER11           PIC X(40).                                 
004605*                                                                         
004606     SKIP2                                                                
004800 FD  W21501B                                                              
004900     RECORDING       F                                                    
005000     BLOCK CONTAINS  0.                                                   
005200 01  IN2-POST.                                                            
005210     03 INB-POST-TYPE          PIC X(2).                                  
005220     03 INB-FILLER1            PIC X(1).                                  
005230     03 INB-ARTNR-OVER         PIC X(8).                                  
005240     03 INB-FILLER2            PIC X(1).                                  
005250     03 INB-ARTNR-ING          PIC X(8).                                  
005260     03 INB-FILLER3            PIC X(1).                                  
005270     03 INB-NO-OF              PIC X(5).                                  
005280     03 INB-FILLER4            PIC X(1).                                  
005290     03 INB-T-AOINFTID6        PIC X(6).                                  
005291     03 INB-FILLER5            PIC X(1).                                  
005292     03 INB-U-AOINFTID6        PIC X(6).                                  
005293     03 INB-FILLER6            PIC X(1).                                  
005294     03 INB-ARTTYP             PIC X(4).                                  
005295     03 INB-FILLER7            PIC X(1).                                  
005296     03 INB-LEVEL              PIC X(2).                                  
005297     03 INB-FILLER8            PIC X(1).                                  
005298     03 INB-ARTBEN             PIC X(25).                                 
005299     03 INB-FILLER9            PIC X(1).                                  
005300     03 INB-RESMARK            PIC X(1).                                  
005301     03 INB-FILLER10           PIC X(1).                                  
005302     03 INB-ARTNR-SATS         PIC X(8).                                  
005304     03 INB-FILLER11           PIC X(40).                                 
005305*                                                                         
005400     SKIP3                                                                
005440                                                                          
005500 FD  W21505                                                               
005600     RECORDING       F                                                    
005700     BLOCK CONTAINS  0.                                                   
005900*01 RECORD -COPY W215090 -PRE  OUT-  -L.                                  
006003*                                                                         
006030     EJECT                                                                
006100 WORKING-STORAGE SECTION.                                                 
006200                                                                          
006300 77  IDPGM                       PIC X(8)    VALUE 'W2151000'.            
006400 77  FILLER                      PIC X(8)    VALUE 'PGMPOS:'.             
006500 77  WS-PGM-POS                  PIC X(40)   VALUE SPACE.                 
006600 77  FILLER                      PIC X(8)    VALUE 'IMSPOS:'.             
006700 77  WS-PGM-IMS-POS              PIC X(40)   VALUE SPACE.                 
006800 77  WS-IDARTNR-OVER             PIC 9(08)   VALUE ZERO.                  
006900 77  YES                         PIC X       VALUE 'J'.                   
007000 77  NOO                         PIC X       VALUE 'N'.                   
007100                                                                          
007200     SKIP2                                                                
007300 01  ERROR-TEXT.                                                          
007400     03  FILLER                  PIC X(16)   VALUE 'ERROR-TEXT'.          
007500     03  ERROR-TEXT-STR          PIC X(72)   VALUE SPACE.                 
007600                                                                          
007700 77  W21501A-EOF-SW              PIC X       VALUE 'N'.                   
007800     88  END-OF-W21501A                      VALUE 'J'.                   
007900                                                                          
008000 77  W21501B-EOF-SW              PIC X       VALUE 'N'.                   
008100     88  END-OF-W21501B                      VALUE 'J'.                   
008200                                                                          
008300 77  WRITE-HEAD-SW               PIC X       VALUE 'N'.                   
008400     88  WRITE-HEAD                          VALUE 'J'.                   
008500     EJECT                                                                
008600 01  WS-POST.                                                             
008610     03 WS-POST-TYPE        PIC X(2).                                     
008620     03 FILLER1             PIC X(1)   VALUE SPACE.                       
008700     03 WS-ARTNR-OVER       PIC X(8).                                     
008701     03 FILLER2             PIC X(1)   VALUE SPACE.                       
008800     03 WS-ARTNR-ING        PIC X(8).                                     
008810     03 FILLER3             PIC X(1)   VALUE SPACE.                       
008900     03 WS-NO-OF            PIC X(5).                                     
008910     03 FILLER4             PIC X(1)   VALUE SPACE.                       
009000     03 WS-T-AOINFTID6      PIC X(6).                                     
009010     03 FILLER5             PIC X(1)   VALUE SPACE.                       
009100     03 WS-U-AOINFTID6      PIC X(6).                                     
009110     03 FILLER6             PIC X(1)   VALUE SPACE.                       
009200     03 WS-ARTTYP           PIC X(4).                                     
009210     03 FILLER7             PIC X(1)   VALUE SPACE.                       
009300     03 WS-LEVEL            PIC X(2).                                     
009310     03 FILLER8             PIC X(1)   VALUE SPACE.                       
009400     03 WS-ARTBEN           PIC X(25).                                    
009410     03 FILLER9             PIC X(1)   VALUE SPACE.                       
009500     03 WS-FILLER9          PIC X(1).                                     
009510     03 FILLER10            PIC X(1)   VALUE SPACE.                       
009600     03 WS-RESMARK          PIC X(1).                                     
009610     03 FILLER11            PIC X(1)   VALUE SPACE.                       
009700     03 WS-ARTNR-SATS       PIC X(8).                                     
009800                                                                          
009810                                                                          
009900 01  TODAYS-DATE                 PIC 9(6)    VALUE ZERO.                  
010000 01  FILLER REDEFINES TODAYS-DATE.                                        
010100     03  TODAYS-DATE-YEAR        PIC 9(2).                                
010200     03  TODAYS-DATE-MONTH       PIC 9(2).                                
010300     03  TODAYS-DATE-DAY         PIC 9(2).                                
010400     EJECT                                                                
010500 01  GENERAL-SUBPROGRAMS.                                                 
010600*                                                                         
010700     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
010800     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
010900     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
011000     EJECT                                                                
011100*    --- PARAMETRAR TILL POSTSUM                                          
011200*                                                                         
011300*01  -COPY W0005   -PRE  POSTSUM-                                         
011400     EJECT                                                                
011500 01  IN1-AREA-START              PIC X(24)   VALUE                        
011600                                             'IN1-AREA-START'.            
011700 01  IN1-AREA.                                                            
011800     03 IN1-POST-TYPE          PIC X(2).                                  
011900     03 IN1-FILLER1            PIC X(1).                                  
012000     03 IN1-ARTNR-OVER         PIC X(8).                                  
012100     03 IN1-FILLER2            PIC X(1).                                  
012200     03 IN1-ARTNR-ING          PIC X(8).                                  
012300     03 IN1-FILLER3            PIC X(1).                                  
012400     03 IN1-NO-OF              PIC X(5).                                  
012500     03 IN1-FILLER4            PIC X(1).                                  
012600     03 IN1-T-AOINFTID6        PIC X(6).                                  
012700     03 IN1-FILLER5            PIC X(1).                                  
012800     03 IN1-U-AOINFTID6        PIC X(6).                                  
012900     03 IN1-FILLER6            PIC X(1).                                  
013000     03 IN1-ARTTYP             PIC X(4).                                  
013100     03 IN1-FILLER7            PIC X(1).                                  
013200     03 IN1-LEVEL              PIC X(2).                                  
013300     03 IN1-FILLER8            PIC X(1).                                  
013400     03 IN1-ARTBEN             PIC X(25).                                 
013500     03 IN1-FILLER9            PIC X(1).                                  
013600     03 IN1-RESMARK            PIC X(1).                                  
013700     03 IN1-FILLER10           PIC X(1).                                  
013800     03 IN1-ARTNR-SATS         PIC X(8).                                  
013900     03 IN1-FILLER11           PIC X(40).                                 
014000*                                                                         
014100     SKIP2                                                                
014200                                                                          
014300 01  IN2-AREA-START              PIC X(24)   VALUE                        
014400                                             'IN2-AREA-START'.            
014500 01  IN2-AREA.                                                            
014600     03 IN2-POST-TYPE          PIC X(2).                                  
014700     03 IN2-FILLER1            PIC X(1).                                  
014800     03 IN2-ARTNR-OVER         PIC X(8).                                  
014900     03 IN2-FILLER2            PIC X(1).                                  
015000     03 IN2-ARTNR-ING          PIC X(8).                                  
015100     03 IN2-FILLER3            PIC X(1).                                  
015200     03 IN2-NO-OF              PIC X(5).                                  
015300     03 IN2-FILLER4            PIC X(1).                                  
015400     03 IN2-T-AOINFTID6        PIC X(6).                                  
015500     03 IN2-FILLER5            PIC X(1).                                  
015600     03 IN2-U-AOINFTID6        PIC X(6).                                  
015700     03 IN2-FILLER6            PIC X(1).                                  
015800     03 IN2-ARTTYP             PIC X(4).                                  
015900     03 IN2-FILLER7            PIC X(1).                                  
016000     03 IN2-LEVEL              PIC X(2).                                  
016100     03 IN2-FILLER8            PIC X(1).                                  
016200     03 IN2-ARTBEN             PIC X(25).                                 
016300     03 IN2-FILLER9            PIC X(1).                                  
016400     03 IN2-RESMARK            PIC X(1).                                  
016500     03 IN2-FILLER10           PIC X(1).                                  
016600     03 IN2-ARTNR-SATS         PIC X(8).                                  
016700     03 IN2-FILLER11           PIC X(40).                                 
016800*                                                                         
016900     SKIP2                                                                
017000                                                                          
017100 01  OUT-AREA-START              PIC X(24)   VALUE 'OUT-AREA'.            
017200     SKIP2                                                                
017310 01  OUT-AREA.                                                            
017320     03 OUT-POST-TYPE          PIC X(2).                                  
017330     03 OUT-FILLER1            PIC X(1).                                  
017340     03 OUT-ARTNR-OVER         PIC X(8).                                  
017350     03 OUT-FILLER2            PIC X(1).                                  
017360     03 OUT-ARTNR-ING          PIC X(8).                                  
017370     03 OUT-FILLER3            PIC X(1).                                  
017380     03 OUT-NO-OF              PIC X(5).                                  
017390     03 OUT-FILLER4            PIC X(1).                                  
017391     03 OUT-T-AOINFTID6        PIC X(6).                                  
017392     03 OUT-FILLER5            PIC X(1).                                  
017393     03 OUT-U-AOINFTID6        PIC X(6).                                  
017394     03 OUT-FILLER6            PIC X(1).                                  
017395     03 OUT-ARTTYP             PIC X(4).                                  
017396     03 OUT-FILLER7            PIC X(1).                                  
017397     03 OUT-LEVEL              PIC X(2).                                  
017398     03 OUT-FILLER8            PIC X(1).                                  
017399     03 OUT-ARTBEN             PIC X(25).                                 
017400     03 OUT-FILLER9            PIC X(1).                                  
017401     03 OUT-RESMARK            PIC X(1).                                  
017402     03 OUT-FILLER10           PIC X(1).                                  
017403     03 OUT-ARTNR-SATS         PIC X(8).                                  
017410*                                                                         
017500     EJECT                                                                
017600 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
017700*    --- STATUS-KOD FRÅN IMS                                              
017800 01  STATUS-WS                   PIC XX.                                  
017900     88  SEGMENT-FOUND                       VALUE '  '.                  
018000     88  SEGMENT-FOUND-EXISTS                VALUE 'II'.                  
018100     88  SEGMENT-MISSING                     VALUE 'GE'.                  
018200     88  SEGMENT-NOMORE                      VALUE 'GB'.                  
018300     88  IMS-NOT-OK                          VALUE 'XD'.                  
018400     SKIP2                                                                
018500 01  GOOD-STATUSCODES.                                                    
018600     03  GOOD-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
018700     SKIP3                                                                
018800 01  SSA1                        PIC X(64).                               
018900 01  SSA2                        PIC X(64).                               
019000     EJECT                                                                
019100*    --- IMS FUNCTION CODES                                               
019200*01  -COPY W0003                                                          
019300     EJECT                                                                
019400*    ---  DLI INPUT-OUTPUT AREA                                           
019500                                                                          
019600     EJECT                                                                
019700 LINKAGE SECTION.                                                         
019800                                                                          
019900*01  -COPY W0009   -PRE MSG-                                              
020000     EJECT                                                                
020100 PROCEDURE DIVISION  USING MSG-PCB.                                       
020200 MAIN SECTION.                                                            
020300     ENTRY 'DLITCBL' USING MSG-PCB.                                       
020400                                                                          
020500     SKIP2                                                                
020600     PERFORM A-INIT                                                       
020700     PERFORM S01-READ-W21501A                                             
020800     PERFORM S02-READ-W21501B                                             
020900          DISPLAY 'W21510 START'                                          
020920                                                                          
021000     PERFORM UNTIL END-OF-W21501A                                         
021100       IF IN1-POST-TYPE = '10'                                            
021200         IF IN1-ARTTYP = 'S   '                                           
021210*          SAVE KIT PART HEAD                                             
021300           PERFORM C-SAVE-TO-WS                                           
021400           MOVE 'J'          TO WRITE-HEAD-SW                             
021600         END-IF                                                           
021700       END-IF                                                             
021800       IF IN1-ARTNR-OVER = IN2-ARTNR-OVER                                 
021900                                                                          
021920         IF  IN1-ARTBEN  NOT = IN2-ARTBEN                                 
021921         OR  IN1-RESMARK NOT = IN2-RESMARK                                
021924           PERFORM D-WS-HEAD-TO-OUT                                       
021925           PERFORM S11-WRITE-W21505                                       
021926           MOVE 'N'          TO WRITE-HEAD-SW                             
021927         END-IF                                                           
021928                                                                          
021930*        READ ALL 02 LEVEL                                                
022000         PERFORM S01-READ-W21501A                                         
022100         PERFORM S02-READ-W21501B                                         
022102                                                                          
022200         PERFORM UNTIL IN1-POST-TYPE NOT = '20'                           
022201         OR END-OF-W21501B                                                
022300           IF IN1-POST-TYPE = '20' AND IN2-POST-TYPE = '20'               
022400             IF  IN1-ARTNR-ING   = IN2-ARTNR-ING                          
022500             AND IN1-NO-OF       = IN2-NO-OF                              
022600             AND IN1-T-AOINFTID6 = IN2-T-AOINFTID6                        
022700             AND IN1-U-AOINFTID6 = IN2-U-AOINFTID6                        
022800             AND IN1-ARTTYP      = IN2-ARTTYP                             
022900             AND IN1-LEVEL       = IN2-LEVEL                              
023000             AND IN1-ARTBEN      = IN2-ARTBEN                             
023100             AND IN1-RESMARK     = IN2-RESMARK                            
023200               CONTINUE                                                   
023400*              NO DIFF READ NEXT                                          
023500             ELSE                                                         
023600               IF WRITE-HEAD-SW = 'J'                                     
023800                 PERFORM D-WS-HEAD-TO-OUT                                 
023900                 PERFORM S11-WRITE-W21505                                 
024000                 MOVE 'N'  TO  WRITE-HEAD-SW                              
024100               END-IF                                                     
024200                                                                          
024300*              WRITE IN1 UNMATCHED                                        
024330*            END-IF                                                       
024400               PERFORM B-MOVE-TO-W21510D3                                 
024500               PERFORM S11-WRITE-W21505                                   
024600             END-IF                                                       
024700                                                                          
024800             PERFORM S01-READ-W21501A                                     
024900             IF IN2-POST-TYPE = '20'                                      
025000               PERFORM S02-READ-W21501B                                   
025100             END-IF                                                       
025200*                                                                         
025300           ELSE                                                           
025310             IF IN1-POST-TYPE < IN2-POST-TYPE                             
025311*              READ  IN1 UNMATCHED                                        
025314               PERFORM S01-READ-W21501A                                   
025320             ELSE                                                         
025321               IF IN1-POST-TYPE > IN2-POST-TYPE                           
025322                 PERFORM S02-READ-W21501B                                 
025330               END-IF                                                     
025331             END-IF                                                       
026010           END-IF                                                         
026100         END-PERFORM                                                      
026200       ELSE                                                               
026300         IF IN1-ARTNR-OVER < IN2-ARTNR-OVER                               
026400*          WRITE IN1 UNMATCHED                                            
026500           PERFORM B-MOVE-TO-W21510D3                                     
026510           PERFORM S11-WRITE-W21505                                       
026600           PERFORM S01-READ-W21501A                                       
026700         ELSE                                                             
026800           IF IN1-ARTNR-OVER > IN2-ARTNR-OVER                             
026900             IF END-OF-W21501B                                            
027000               CONTINUE                                                   
027010           DISPLAY '* W21501B-EOF-SW *' W21501B-EOF-SW                    
027020               MOVE '99999999' TO IN2-ARTNR-OVER                          
027100             ELSE                                                         
027101               PERFORM S02-READ-W21501B                                   
027110             END-IF                                                       
027120           END-IF                                                         
027200         END-IF                                                           
027300                                                                          
027400       END-IF                                                             
027500     END-PERFORM                                                          
027700                                                                          
027800     PERFORM Z-FINIT                                                      
027900                                                                          
028000     MOVE ZERO TO RETURN-CODE                                             
028100     GOBACK                                                               
028200     .                                                                    
028300     EJECT                                                                
028400 A-INIT SECTION.                                                          
028500     MOVE 'A-INIT             '   TO WS-PGM-POS                           
028600     SKIP2                                                                
028700                                                                          
028800     OPEN INPUT  W21501A                                                  
028900     OPEN INPUT  W21501B                                                  
029000     OPEN OUTPUT W21505                                                   
029100                                                                          
029110     MOVE  SPACE           TO OUT-AREA                                    
029200                                                                          
029300     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
029400     .                                                                    
029500     EJECT                                                                
029600                                                                          
029700 B-MOVE-TO-W21510D3      SECTION.                                         
029800     MOVE 'B-MOVE-TO-W21510D3 '   TO WS-PGM-POS                           
029900                                                                          
030000*    MOVE  SPACE           TO WS-POST                                     
030010*    MOVE  IN1-ARTNR-OVER  TO WS-ARTNR-OVER                               
030100*    MOVE  IN1-ARTNR-ING   TO WS-ARTNR-ING                                
030200*    MOVE  IN1-POST-TYPE   TO WS-POST-TYPE                                
030300*    MOVE  IN1-ARTNR-ING   TO WS-ARTNR-ING                                
030400*    MOVE  IN1-NO-OF       TO WS-NO-OF                                    
030500*    MOVE  IN1-T-AOINFTID6 TO WS-T-AOINFTID6                              
030600*    MOVE  IN1-U-AOINFTID6 TO WS-U-AOINFTID6                              
030700*    MOVE  IN1-ARTTYP      TO WS-ARTTYP                                   
030800*    MOVE  IN1-LEVEL       TO WS-LEVEL                                    
030900*    MOVE  IN1-ARTBEN      TO WS-ARTBEN                                   
031000*    MOVE  IN1-RESMARK     TO WS-RESMARK                                  
031010*    MOVE  IN1-ARTNR-SATS  TO WS-ARTNR-SATS                               
031020     MOVE  SPACE           TO OUT-AREA                                    
031030     MOVE  IN1-ARTNR-OVER  TO OUT-ARTNR-OVER                              
031040     MOVE  IN1-ARTNR-ING   TO OUT-ARTNR-ING                               
031050     MOVE  IN1-POST-TYPE   TO OUT-POST-TYPE                               
031060     MOVE  IN1-ARTNR-ING   TO OUT-ARTNR-ING                               
031070     MOVE  IN1-NO-OF       TO OUT-NO-OF                                   
031080     MOVE  IN1-T-AOINFTID6 TO OUT-T-AOINFTID6                             
031090     MOVE  IN1-U-AOINFTID6 TO OUT-U-AOINFTID6                             
031091     MOVE  IN1-ARTTYP      TO OUT-ARTTYP                                  
031092     MOVE  IN1-LEVEL       TO OUT-LEVEL                                   
031093     MOVE  IN1-ARTBEN      TO OUT-ARTBEN                                  
031094     MOVE  IN1-RESMARK     TO OUT-RESMARK                                 
031095     MOVE  IN1-ARTNR-SATS  TO OUT-ARTNR-SATS                              
031100     .                                                                    
031200                                                                          
031300 C-SAVE-TO-WS    SECTION.                                                 
031400     MOVE 'C-SAVE-TO-WS       '   TO WS-PGM-POS                           
031410*          SAVE KIT PART HEAD                                             
031500                                                                          
031600     MOVE IN1-ARTNR-OVER  TO WS-ARTNR-OVER                                
031700     MOVE IN1-ARTNR-ING   TO WS-ARTNR-ING                                 
031800     MOVE IN1-POST-TYPE   TO WS-POST-TYPE                                 
032000     MOVE IN1-NO-OF       TO WS-NO-OF                                     
032100     MOVE IN1-T-AOINFTID6 TO WS-T-AOINFTID6                               
032200     MOVE IN1-U-AOINFTID6 TO WS-U-AOINFTID6                               
032300     MOVE IN1-ARTTYP      TO WS-ARTTYP                                    
032400     MOVE IN1-LEVEL       TO WS-LEVEL                                     
032500     MOVE IN1-ARTBEN      TO WS-ARTBEN                                    
032600     MOVE IN1-RESMARK     TO WS-RESMARK                                   
032610     MOVE IN1-ARTNR-SATS  TO WS-ARTNR-SATS                                
032700     .                                                                    
032800***                                                                       
032900                                                                          
033000 D-WS-HEAD-TO-OUT     SECTION.                                            
033100     MOVE 'D-WS-HEAD-TO-OUT   '   TO WS-PGM-POS                           
033200                                                                          
035020     MOVE SPACE          TO OUT-AREA                                      
035030     MOVE WS-POST-TYPE   TO OUT-POST-TYPE                                 
035040     MOVE WS-ARTNR-OVER  TO OUT-ARTNR-OVER                                
035050     MOVE WS-ARTNR-ING   TO OUT-ARTNR-ING                                 
035060     MOVE WS-POST-TYPE   TO OUT-POST-TYPE                                 
035070     MOVE WS-NO-OF       TO OUT-NO-OF                                     
035080     MOVE WS-T-AOINFTID6 TO OUT-T-AOINFTID6                               
035090     MOVE WS-U-AOINFTID6 TO OUT-U-AOINFTID6                               
035091     MOVE WS-ARTTYP      TO OUT-ARTTYP                                    
035092     MOVE WS-LEVEL       TO OUT-LEVEL                                     
035093     MOVE WS-ARTBEN      TO OUT-ARTBEN                                    
035094     MOVE WS-RESMARK     TO OUT-RESMARK                                   
035095     MOVE WS-ARTNR-SATS  TO OUT-ARTNR-SATS                                
035100     .                                                                    
035200***                                                                       
035300 Z-FINIT SECTION.                                                         
035400     MOVE 'Z-FINIT            '   TO WS-PGM-POS                           
035500                                                                          
035600     CLOSE W21501A                                                        
035700     CLOSE W21501B                                                        
035800     CLOSE W21505                                                         
035900     SKIP2                                                                
036000     MOVE 'S' TO POSTSUM-OPKOD                                            
036100     CALL POSTSUM USING POSTSUM-PARM                                      
036200     .                                                                    
036300     EJECT                                                                
036400 S01-READ-W21501A SECTION.                                                
036500     MOVE 'S01-READ-W21501A +0'   TO WS-PGM-POS                           
036600     SKIP2                                                                
036700     READ W21501A INTO IN1-AREA                                           
036800     AT END                                                               
036900*       MOVE HIGH-VALUE TO IN1-POST-TYPE                                  
037000*       SET END-OF-W21501A TO TRUE                                        
037010        MOVE YES           TO W21501A-EOF-SW                              
037100                                                                          
037200     NOT AT END                                                           
037300        MOVE 'W21501A' TO POSTSUM-FDNAMN                                  
037400        MOVE 'W21510D1' TO POSTSUM-DDNAMN2                                
037500*       MOVE IN1-POST-TYPE TO POSTSUM-TRANSTYP                            
037600        CALL POSTSUM USING POSTSUM-PARM                                   
037700     END-READ                                                             
037800     .                                                                    
037900     EJECT                                                                
038000 S02-READ-W21501B SECTION.                                                
038100     MOVE 'S02-READ-W21501B -1'   TO WS-PGM-POS                           
038200     SKIP2                                                                
038300     READ W21501B INTO IN2-AREA                                           
038400     AT END                                                               
038500        CONTINUE                                                          
038600*       MOVE HIGH-VALUE TO IN2-POST-TYPE                                  
038700*       SET END-OF-W21501B TO TRUE                                        
038710        MOVE YES           TO W21501B-EOF-SW                              
038800                                                                          
038900     NOT AT END                                                           
039000        MOVE 'W21501B' TO POSTSUM-FDNAMN                                  
039100        MOVE 'W21510D2' TO POSTSUM-DDNAMN2                                
039200*       MOVE IN2-POST-TYPE TO POSTSUM-TRANSTYP                            
039300        CALL POSTSUM USING POSTSUM-PARM                                   
039400     END-READ                                                             
039500     .                                                                    
039600     EJECT                                                                
039700 S11-WRITE-W21505 SECTION.                                                
039800     MOVE 'S11-WRITE-W21505   '   TO WS-PGM-POS                           
039900     SKIP2                                                                
040000*    WRITE OUT-RECORD  FROM W21505-POST                                   
040010     WRITE OUT-RECORD  FROM OUT-AREA                                      
040100                                                                          
040200*    MOVE OUT-POST-TYPE TO POSTSUM-TRANSTYP                               
040300     MOVE 'W21505 ' TO POSTSUM-FDNAMN                                     
040400     MOVE 'W21510D3' TO POSTSUM-DDNAMN2                                   
040500     CALL POSTSUM USING POSTSUM-PARM                                      
040600     .                                                                    
040700     EJECT                                                                
040800* --- IMS SECTIONS  ---                                                   
040900                                                                          
041000     EJECT                                                                
041100 IMS-STATUSCHECK SECTION.                                                 
041200     SKIP2                                                                
041300     SET STATUS-IX TO 1                                                   
041400     SEARCH GOOD-STATUS                                                   
041500       AT END                                                             
041600         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
041700           DELIMITED BY SIZE INTO ERROR-TEXT                              
041800         DISPLAY ERROR-TEXT                                               
041900         CALL FELLOG                                                      
042000       WHEN GOOD-STATUS (STATUS-IX) = STATUS-WS                           
042100         CONTINUE                                                         
042200     END-SEARCH                                                           
042300     .                                                                    
