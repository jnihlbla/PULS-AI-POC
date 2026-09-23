000100 ID DIVISION.                                                             
000200 PROGRAM-ID.        W1225000.                                             
000300 AUTHOR.            P DAHLÖF.                                             
000400 DATE-WRITTEN.      SEP 1987.                                             
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*    FUNKTION:                                                            
000800*            SKAPA FIL W12251 PÅ ALLA B-MÄRKTA ARTIKLAR SAMT              
000900*            RENSA DESSA ARTIKLAR FRÅN WDK6.                              
001000*            FIL W12251 KOMPLETTERAS MED FOB.GROSS PRIS.                  
001100*            FÖR S-MÄRKTA ARTIKLAR SKALL ALLA SEGMENT UTOM ROT            
001200*            SKALAS PÅ WDK6.                                              
001210*            PÅ WDT3 DELETAS SAMTLIGA SEGMENT FÖR ARTIKELN.               
001300*                                                                         
001400*    MÄRKNING SKER:                                                       
001500*            I PROGRAM W12204                                             
001600                                                                          
001700                                                                          
001800     EJECT                                                                
001900 ENVIRONMENT DIVISION.                                                    
002000 INPUT-OUTPUT SECTION.                                                    
002100 FILE-CONTROL.                                                            
002200     SKIP2                                                                
002300*- - - - - - - - - - - - - - INFIL:                                       
002400     SELECT W12225         ASSIGN TO     W12250D1.                        
002500*- - - - - - - - - - - - - - UTFIL:                                       
002600     SELECT W12251         ASSIGN TO     W12250D2.                        
002700     EJECT                                                                
002800 DATA DIVISION.                                                           
002900 FILE SECTION.                                                            
003000     SKIP3                                                                
003100 FD   W12225                                                              
003200      RECORDING F                                                         
003300      BLOCK CONTAINS 0.                                                   
003400*01   POST -COPY W12225   -PRE W12225-    -L.                             
003500     EJECT                                                                
003600                                                                          
003700 FD   W12251                                                              
003800      RECORDING F                                                         
003900      BLOCK CONTAINS 0.                                                   
004000*01   POST -COPY W12251   -PRE W12251-    -L.                             
004100     EJECT                                                                
004200                                                                          
004300 WORKING-STORAGE SECTION.                                                 
004400                                                                          
004500*    -- CHECKED BY WY2000                                                 
004600 77  IDPGM                       PIC X(8)    VALUE 'W1225000'.            
004700 01  GENERELLA-KONSTANTER.                                                
004800   03  JA                        PIC X       VALUE 'J'.                   
004900   03  NEJ                       PIC X       VALUE 'N'.                   
005000   77  W12225-EOF                PIC X       VALUE 'N'.                   
005100   77  W-KDERS                   PIC S9(3)   VALUE ZERO.                  
005110   77  W-REPL-WDK601             PIC 9(7)    VALUE ZERO.                  
005120   77  W-DLET-WDK601             PIC 9(7)    VALUE ZERO.                  
005130   77  W-DLET-WDK611             PIC 9(7)    VALUE ZERO.                  
005140   77  W-DLET-WDK613             PIC 9(7)    VALUE ZERO.                  
005150   77  W-DLET-WDT301             PIC 9(7)    VALUE ZERO.                  
005160   77  W-ISRT-WDK401             PIC 9(7)    VALUE ZERO.                  
005200   77  INDX                      PIC S9(3)   VALUE ZERO.                  
005300   77  WS-DAGENS-AAMMDD          PIC 9(6)    VALUE ZERO.                  
005400     SKIP3                                                                
005500 01  DYNAMISKA-SUBPROGRAM.                                                
005600   03  POSTSUM                   PIC X(8)    VALUE 'POSTSUM '.            
005700   03  ABEND                     PIC X(8)    VALUE 'ABEND   '.            
005800   03  CBLTDLI                   PIC X(8)    VALUE 'CBLTDLI '.            
005900   03  FELLOG                    PIC X(8)    VALUE 'FELLOG  '.            
006000   03  WDATKONV                  PIC X(8)    VALUE 'WDATKONV'.            
006100     EJECT                                                                
006200*- - - - - - - - - - - - - -  PARAMETRAR TILL POSTSUM                     
006300*01  -COPY W0005     -PRE  POSTSUM-.                                      
006400     EJECT                                                                
006500*- - - - - - - - - - - - - -  PARAMETRAR TILL DATKONV                     
006600*01  -COPY WDATAREA                                                       
006700     EJECT                                                                
006800*- - - - - - - - - - - - - -  ARBETSAREAR FÖR INFIL                       
006900 01  FILLER                      PIC X(24)    VALUE 'INFIL'.              
007000                                                                          
007100*01  AREA    -COPY W12225   -PRE W-IN-.                                   
007200     EJECT                                                                
007300*- - - - - - - - - - - - - -  ARBETSAREOR FÖR UTFIL                       
007400 01  FILLER                      PIC X(24)    VALUE 'UTFIL'.              
007500                                                                          
007600*01  AREA    -COPY W12251   -PRE W-UT-.                                   
007700     EJECT                                                                
007800*- - - - - - - - -PARAMETER-AREOR TILL IMS-SUBPGM-SEKTIONER.              
007900 01  FILLER                      PIC X(16) VALUE 'IMS-WS'.                
008000     SKIP3                                                                
008100*- - - - - - - - -STATUSKOD FRÅN IMS                                      
008200 01  STATUS-WS                   PIC X(2).                                
008300   88  SEGMENT-FINNS         VALUE '  '.                                  
008400   88  SEGMENT-SAKNAS        VALUE 'GE'.                                  
008500   88  SEGMENT-FINNS-REDAN   VALUE 'II'.                                  
008600   SKIP3                                                                  
008700 01  GODK-STATUSKODER.                                                    
008800    03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                 
008900 01  SSA1                        PIC X(32).                               
009000 01  SSA2                        PIC X(32).                               
009100 01  SSA3                        PIC X(32).                               
009200*- - - - - - - - - - - - - - NYCKLAR TILL DLI.                            
009300 01  NYCKLAR-TILL-DLI.                                                    
009400   03   W-IDARTNR-X.                                                      
009500     05 W-IDARTNR                PIC S9(9)  COMP-3.                       
009600   03   W-KDSEGKEY-X.                                                     
009700     05 W-KDSEGKEY               PIC X      VALUE '1'.                    
009800   03   W-KDNOTTYP-X.                                                     
009900     05 W-KDNOTTYP               PIC S9(1)  VALUE ZERO  COMP-3.           
010000     EJECT                                                                
010100*01   -COPY W0003                                                         
010200     EJECT                                                                
010300 01  FILLER                      PIC X(16)    VALUE                       
010400                                              'DLI-IO-AREA'.              
010500 01  DLI-IO-AREA.                                                         
010600   03  IO-AREA                   PIC X(900) VALUE SPACE.                  
010700*03  WLARTC01  -COPY WDK601   -RED IO-AREA.                               
010800     EJECT                                                                
010900*03  WLARTC11  -COPY WDK611   -RED IO-AREA.                               
011000     EJECT                                                                
011300*03  WLARTC13  -COPY WDK613   -RED IO-AREA.                               
011400     EJECT                                                                
011500*03  WLARTC25  -COPY WDK625   -RED IO-AREA.                               
011600     EJECT                                                                
011700 01  DLI-IO-AREA2.                                                        
011800   03  IO-AREA2                  PIC X(800) VALUE SPACE.                  
011900*03  WDK401    -COPY WDK401   -RED IO-AREA2.                              
011910     EJECT                                                                
011920 01  DLI-IO-WDT301.                                                       
011940*    03  -COPY WDT301                                                     
011941     EJECT                                                                
012200 LINKAGE SECTION.                                                         
012300*01  -COPY W0008       -PRE WDK6-.                                        
012400 05  FILLER             PIC X.                                            
012500     EJECT                                                                
012600*01  -COPY W0008       -PRE WDK4-.                                        
012700 05  FILLER             PIC X.                                            
012800     EJECT                                                                
012810*01  -COPY W0008       -PRE WDT3-.                                        
012820 05  FILLER             PIC X.                                            
012830     EJECT                                                                
012900 PROCEDURE DIVISION USING WDK6-PCB WDK4-PCB WDT3-PCB.                     
013000 MAIN SECTION.                                                            
013100     ENTRY 'DLITCBL' USING WDK6-PCB WDK4-PCB WDT3-PCB.                    
013200                                                                          
013300     PERFORM A-INIT                                                       
013400     PERFORM S11-LAES-INFIL                                               
013500     PERFORM UNTIL W12225-EOF = JA                                        
013600       MOVE W-IN-IDARTNR TO W-IDARTNR                                     
013700       IF W-IN-UTFIL-TYP = 'B'                                            
013800         PERFORM B-SKRIV-OCH-DELETA-ARTC                                  
013900       ELSE                                                               
014000         PERFORM C-SKALA-ARTC-POST                                        
014100       END-IF                                                             
014110       PERFORM D-DELETA-WDT3                                              
014200       PERFORM S11-LAES-INFIL                                             
014300     END-PERFORM                                                          
014400                                                                          
014500     PERFORM Z-FINIT                                                      
014600     MOVE ZERO TO RETURN-CODE                                             
014700     GOBACK                                                               
014800     .                                                                    
014900     EJECT                                                                
015000 A-INIT SECTION.                                                          
015100                                                                          
015200     OPEN  INPUT W12225                                                   
015300     OPEN OUTPUT W12251                                                   
015400     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
015500                                                                          
015600     MOVE 'IDAG  ' TO DAT-KDDATFORM                                       
015700     CALL WDATKONV USING DAT-KDDATFORM                                    
015800                         DAT-I-TIDATUM                                    
015900                         DAT-O-TIDATUM                                    
016000                         DAT-KDSVAR                                       
016100     MOVE DAT-TIAAMMDD TO WS-DAGENS-AAMMDD                                
016200     PERFORM S01-NOLLSTALL                                                
016300     .                                                                    
016400     EJECT                                                                
016500 B-SKRIV-OCH-DELETA-ARTC SECTION.                                         
016600                                                                          
016700     PERFORM IMS-GHU-ARTC01                                               
016800     PERFORM BA-FLYTTA-ARTC01-TILL-UT                                     
016900     MOVE +1 TO INDX                                                      
017000     PERFORM UNTIL INDX > 5                                               
017100       IF INDX = 1                                                        
017200         MOVE ART-IDAO(INDX)   TO W-UT-IDAO-1                             
017300       ELSE                                                               
017400         EVALUATE TRUE                                                    
017500         WHEN INDX = 2                                                    
017600           MOVE ART-IDAO(INDX) TO W-UT-IDAO-2                             
017700         WHEN INDX = 3                                                    
017800           MOVE ART-IDAO(INDX) TO W-UT-IDAO-3                             
017900         WHEN INDX = 4                                                    
018000           MOVE ART-IDAO(INDX) TO W-UT-IDAO-4                             
018100         WHEN INDX = 5                                                    
018200           MOVE ART-IDAO(INDX) TO W-UT-IDAO-5                             
018300         END-EVALUATE                                                     
018400       END-IF                                                             
018500       ADD +1 TO INDX                                                     
018600     END-PERFORM                                                          
018700                                                                          
018800     IF ART-KDERS-UTG = ZERO                                              
018900       PERFORM IMS-GHNP-ARTC11                                            
019000       IF SEGMENT-FINNS                                                   
019100         MOVE CLAG-KDERS TO W-UT-KDERS-UTG                                
019200       ELSE                                                               
019300         CONTINUE                                                         
019400       END-IF                                                             
019500       PERFORM IMS-GHU-ARTC01                                             
019600     END-IF                                                               
019700                                                                          
019800     MOVE 0 TO W-UT-PRARTBTO-EXP                                          
019900     PERFORM S22-SKRIV-UTFIL                                              
020000***  DISPLAY 'DLET'  W-IDARTNR                                            
020100     PERFORM IMS-DELETE-ARTC                                              
020110     ADD 1 TO W-DLET-WDK601                                               
020200     .                                                                    
020300     EJECT                                                                
020400 C-SKALA-ARTC-POST SECTION.                                               
020500                                                                          
020600     PERFORM IMS-GHU-ARTC01                                               
020700     PERFORM IMS-GHNP-ARTC11                                              
020800     IF SEGMENT-FINNS                                                     
020900       MOVE CLAG-KDERS TO W-KDERS                                         
021000                                                                          
021100       PERFORM IMS-GHU-ARTC01                                             
021200       MOVE W-KDERS TO ART-KDERS-UTG                                      
021300***    DISPLAY 'REPL'  W-IDARTNR                                          
021400       PERFORM IMS-REPL-ARTC                                              
021410       ADD 1 TO W-REPL-WDK601                                             
021500                                                                          
021600       PERFORM IMS-GHU-ARTC01                                             
021700       PERFORM IMS-GHNP-ARTC11                                            
021800                                                                          
021900       MOVE W-IDARTNR TO RENS-IDARTNR                                     
022000       MOVE WS-DAGENS-AAMMDD TO RENS-TIBORT                               
022100       MOVE +3 TO W-KDNOTTYP                                              
022200       PERFORM IMS-GET-ARTC25                                             
022300       IF SEGMENT-FINNS                                                   
022400          MOVE NOT-TEARTNOT TO RENS-TEARTNOT-BER                          
022500       ELSE                                                               
022600          MOVE SPACE TO RENS-TEARTNOT-BER                                 
022700       END-IF                                                             
022800       MOVE +7 TO W-KDNOTTYP                                              
022900       PERFORM IMS-GET-ARTC25                                             
023000       IF SEGMENT-FINNS                                                   
023100          MOVE NOT-TEARTNOT TO RENS-TEARTNOT-BERIMP                       
023200       ELSE                                                               
023300          MOVE SPACE TO RENS-TEARTNOT-BERIMP                              
023400       END-IF                                                             
023500       PERFORM IMS-ISRT-WDK401                                            
023510       ADD 1 TO W-ISRT-WDK401                                             
023600                                                                          
023700     END-IF                                                               
023800                                                                          
023900     PERFORM IMS-GHU-ARTC01                                               
024000     PERFORM IMS-GHNP-ARTC11                                              
024100     IF SEGMENT-FINNS                                                     
024200       PERFORM IMS-DELETE-ARTC                                            
024210       ADD 1 TO W-DLET-WDK611                                             
024300     END-IF                                                               
025100                                                                          
025200     PERFORM IMS-GHU-ARTC01                                               
025300     PERFORM IMS-GET-WDK613                                               
025400     PERFORM UNTIL SEGMENT-SAKNAS                                         
025500       PERFORM IMS-DELETE-ARTC                                            
025510       ADD 1 TO W-DLET-WDK613                                             
025600       PERFORM IMS-GET-WDK613                                             
025700     END-PERFORM                                                          
025800     .                                                                    
025900     EJECT                                                                
026000 BA-FLYTTA-ARTC01-TILL-UT SECTION.                                        
026100                                                                          
026200     MOVE W-IDARTNR        TO W-UT-IDARTNR                                
026300     MOVE ART-REKSIFFR     TO W-UT-REKSIFFR                               
026400     MOVE ART-FLERS        TO W-UT-FLERS                                  
026500     MOVE ART-KDERS-UTG    TO W-UT-KDERS-UTG                              
026600     MOVE ART-TIERSDAT     TO W-UT-TIERSDAT                               
026700     MOVE ART-TIFINLV      TO W-UT-TIFINLV                                
026800     MOVE ART-TIREGDAT     TO W-UT-TIREGDAT                               
026900     MOVE ART-IDLEVNR      TO W-UT-IDLEVNR                                
027000     MOVE ART-KDSORT       TO W-UT-KDSORT                                 
027100     MOVE W-IN-BEART       TO W-UT-BEART                                  
027200     MOVE W-IN-KDHOMONYM   TO W-UT-KDHOMONYM                              
027300     .                                                                    
027400     EJECT                                                                
027500 D-DELETA-WDT3 SECTION.                                                   
027600                                                                          
027601     PERFORM IMS-GHU-WDT301                                               
027602     IF SEGMENT-FINNS                                                     
027603       PERFORM IMS-DELETE-WDT3                                            
027604       ADD 1 TO W-DLET-WDT301                                             
027605     END-IF                                                               
027610     .                                                                    
027620     EJECT                                                                
027630 S01-NOLLSTALL SECTION.                                                   
027640                                                                          
027700     MOVE ZERO TO RENS-IDARTNR                                            
027800                  RENS-TIBORT                                             
027900     MOVE SPACE TO RENS-TEARTNOT-BER                                      
028000                   RENS-TEARTNOT-BERIMP                                   
028100     .                                                                    
028200     EJECT                                                                
028300 S11-LAES-INFIL SECTION.                                                  
028400                                                                          
028500     READ W12225 INTO W-IN-AREA                                           
028600         AT END MOVE JA    TO W12225-EOF                                  
028700     END-READ                                                             
028800     IF W12225-EOF = NEJ                                                  
028900       MOVE 'W12250D1'   TO POSTSUM-DDNAMN2                               
029000       MOVE 'W12225'     TO POSTSUM-FDNAMN                                
029100       CALL POSTSUM USING POSTSUM-PARM                                    
029200     END-IF                                                               
029300     .                                                                    
029400     EJECT                                                                
029500 S22-SKRIV-UTFIL SECTION.                                                 
029600                                                                          
029700     WRITE W12251-POST FROM W-UT-AREA                                     
029800     MOVE 'W12250D2' TO POSTSUM-DDNAMN2                                   
029900     MOVE 'W12251  ' TO POSTSUM-FDNAMN                                    
030000     MOVE 'BORT'     TO POSTSUM-TRANSTYP                                  
030100     CALL POSTSUM USING POSTSUM-PARM                                      
030200     .                                                                    
030300     EJECT                                                                
030400 Z-FINIT SECTION.                                                         
030500                                                                          
030600                                                                          
           DISPLAY 'ANTAL ÄNDRADE WDK601: ' W-REPL-WDK601                       
           DISPLAY 'ANTAL BORTTAGNA WDK601: ' W-DLET-WDK601                     
           DISPLAY 'ANTAL BORTTAGNA WDK611: ' W-DLET-WDK611                     
           DISPLAY 'ANTAL BORTTAGNA WDK613: ' W-DLET-WDK613                     
           DISPLAY 'ANTAL BORTTAGNA WDT301: ' W-DLET-WDT301                     
           DISPLAY 'ANTAL NYA WDK401: ' W-ISRT-WDK401                           
030600     CLOSE W12225                                                         
030700     CLOSE W12251                                                         
030800     MOVE 'S' TO POSTSUM-OPKOD                                            
030900     CALL POSTSUM USING POSTSUM-PARM                                      
031000     .                                                                    
031100     EJECT                                                                
031200*- - - - - - - - - - - - - - - - - - - -IMS-SECTIONER.                    
031300 IMS-GHU-ARTC01 SECTION.                                                  
031400     STRING 'WDK601  (IDARTNR  =' W-IDARTNR-X ')'                         
031500            DELIMITED BY SIZE INTO SSA1                                   
031600     MOVE '  ' TO GODK-STATUSKODER                                        
031700     CALL CBLTDLI USING GHU WDK6-PCB DLI-IO-AREA SSA1                     
031800     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
031900     PERFORM IMS-STATUSKONTROLL                                           
032000     .                                                                    
032100     SKIP3                                                                
032200 IMS-GHNP-ARTC11 SECTION.                                                 
032300     STRING 'WDK611  (KDSEGKEY =' W-KDSEGKEY-X ')'                        
032400            DELIMITED BY SIZE INTO SSA1                                   
032500     MOVE '  GE' TO GODK-STATUSKODER                                      
032600     CALL CBLTDLI USING GHNP WDK6-PCB DLI-IO-AREA SSA1                    
032700     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
032800     PERFORM IMS-STATUSKONTROLL                                           
032900     .                                                                    
033000     SKIP3                                                                
033100 IMS-GET-ARTC25 SECTION.                                                  
033200     STRING 'WDK625  *F(KDNOTTYP =' W-KDNOTTYP-X ')'                      
033300            DELIMITED BY SIZE INTO SSA1                                   
033400     MOVE '  GE' TO GODK-STATUSKODER                                      
033500     CALL CBLTDLI USING GNP WDK6-PCB DLI-IO-AREA SSA1                     
033600     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
033700     PERFORM IMS-STATUSKONTROLL                                           
033800     .                                                                    
033900     SKIP3                                                                
034000 IMS-DELETE-ARTC SECTION.                                                 
034100     MOVE '  ' TO GODK-STATUSKODER                                        
034200     CALL CBLTDLI USING DLET WDK6-PCB DLI-IO-AREA                         
034300     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
034400     PERFORM IMS-STATUSKONTROLL                                           
034500     .                                                                    
034600     SKIP3                                                                
034700 IMS-REPL-ARTC SECTION.                                                   
034800     MOVE '  ' TO GODK-STATUSKODER                                        
034900     CALL CBLTDLI USING REPL WDK6-PCB DLI-IO-AREA                         
035000     MOVE WDK6-STATUS-CODE  TO STATUS-WS                                  
035100     PERFORM IMS-STATUSKONTROLL                                           
035200     .                                                                    
035300     SKIP3                                                                
035400 IMS-ISRT-WDK401 SECTION.                                                 
035500     MOVE 'WDK401  ' TO SSA1                                              
035600     MOVE '  ' TO GODK-STATUSKODER                                        
035700     CALL CBLTDLI USING ISRT WDK4-PCB DLI-IO-AREA2 SSA1                   
035800     MOVE WDK4-STATUS-CODE TO STATUS-WS                                   
035900     PERFORM IMS-STATUSKONTROLL                                           
036000     .                                                                    
036100     SKIP3                                                                
037000 IMS-GET-WDK613 SECTION.                                                  
037100     MOVE 'WDK613  *F' TO SSA1                                            
037200     MOVE '  GE' TO GODK-STATUSKODER                                      
037300     CALL CBLTDLI USING GHNP WDK6-PCB DLI-IO-AREA SSA1                    
037400     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
037500     PERFORM IMS-STATUSKONTROLL                                           
037600     .                                                                    
037700     SKIP3                                                                
037710 IMS-GHU-WDT301 SECTION.                                                  
037720     STRING 'WDT301  (IDARTNR  =' W-IDARTNR-X ')'                         
037730            DELIMITED BY SIZE INTO SSA1                                   
037740     MOVE '  GE' TO GODK-STATUSKODER                                      
037750     CALL CBLTDLI USING GHU WDT3-PCB DLI-IO-WDT301 SSA1                   
037760     MOVE WDT3-STATUS-CODE TO STATUS-WS                                   
037770     PERFORM IMS-STATUSKONTROLL                                           
037780     .                                                                    
037790     SKIP3                                                                
037791 IMS-DELETE-WDT3 SECTION.                                                 
037792     MOVE '  ' TO GODK-STATUSKODER                                        
037793     CALL CBLTDLI USING DLET WDT3-PCB DLI-IO-WDT301                       
037794     MOVE WDT3-STATUS-CODE TO STATUS-WS                                   
037795     PERFORM IMS-STATUSKONTROLL                                           
037796     .                                                                    
037797     SKIP3                                                                
037800 IMS-STATUSKONTROLL SECTION.                                              
037900     SET STATUS-IX TO 1                                                   
038000     SEARCH GODK-STATUS                                                   
038100       AT END                                                             
038200         CALL FELLOG                                                      
038300       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
038400         CONTINUE                                                         
038500     END-SEARCH                                                           
038600     .                                                                    
