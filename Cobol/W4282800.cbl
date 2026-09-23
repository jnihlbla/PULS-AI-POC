000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W4282800.                                                
000300 AUTHOR.         UMESH JAIN.                                              
000400 DATE-WRITTEN.   08/09/10.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*    FUNKTION:                                                            
000800*        THIS PROGRAM IS A COPY OF W4282300                               
000900*                                                                         
001000*        PROGRAMMET TAR FIL W42828 SORTERAD PÅ IDLANDX2 OCH DC.           
001100*        LEVERANSANMÄRKNINGAR OCH FAKUTERARDE RADER PER VECKA OCH         
001200*        HITTILLS I ÅR.                                                   
001300*        RÄKNAR UT ÄVEN % PÅ SKEPPADER ARTIKLAR PER VECKA OCH             
001400*        HITTILLS I ÅR. SKICKAR TILL DISTR. & PRINT.                      
001500*                                                                         
001510* 2012-11-09 E'TRACKER 10181786 WEB AUSTRALIEN FOLLOW-UP                  
001520*                                                                         
001600 ENVIRONMENT DIVISION.                                                    
001800 INPUT-OUTPUT SECTION.                                                    
001900                                                                          
002000 FILE-CONTROL.                                                            
002200*          --- VECKANS RADER LEVERANSANM OCH FAKUTURERAT.                 
002300     SELECT W42828                     ASSIGN TO W42828D1.                
002400*                                                                         
002500 DATA DIVISION.                                                           
002700 FILE SECTION.                                                            
002900 FD  W42828                                                               
003000     RECORDING       F                                                    
003100     BLOCK CONTAINS  0.                                                   
003200                                                                          
003300*01  -COPY W4282801    -L.                                                
003400     EJECT                                                                
003500 WORKING-STORAGE SECTION.                                                 
003600                                                                          
003700 77  IDPGM                       PIC X(8)    VALUE 'W4282800'.            
003800 77  JA                          PIC X       VALUE 'J'.                   
003900 77  NEJ                         PIC X       VALUE 'N'.                   
004000 77  SPAR-IDDC                   PIC X(2)    VALUE SPACE.                 
004100 77  SPAR-IDLANDX2               PIC X(2)    VALUE SPACE.                 
004110 77  SPAR-KDANMORS-001           PIC X(2)    VALUE SPACE.                 
004200 77  SPAR-DOC-TIAAPP             PIC 9(4)    VALUE ZERO.                  
004300 77  W-TOTAL-SUART-PERIOD        PIC 9(5)    VALUE ZERO.                  
004400 77  W-TOTAL-SURADER-PERIOD      PIC 9(9)    VALUE ZERO.                  
004410 77  W-TOTAL-SURADER-PERIOD-KD   PIC 9(9)    VALUE ZERO.                  
004500 77  W-TOTAL-SUART-YEAR          PIC 9(5)    VALUE ZERO.                  
004600 77  W-TOTAL-SURADER-YEAR        PIC 9(9)    VALUE ZERO.                  
004601 77  W-KVRETINL-YEAR-KD          PIC 9(5)    VALUE ZERO.                  
004602 77  W-KVRETINL-PERIOD-KD        PIC 9(5)    VALUE ZERO.                  
004603 77  W-REPROCENT-YEAR            PIC 9(3)V9(3) VALUE ZERO.                
004610 77  W-REPROCENT-PERIOD          PIC 9(3)V9(3) VALUE ZERO.                
004620 77  W-REPROCENT-YEAR-KD         PIC 9(3)V9(3) VALUE ZERO.                
004630 77  W-REPROCENT-PERIOD-KD       PIC 9(3)V9(3) VALUE ZERO.                
004700 77  WS-YYMMDDHHMM               PIC 9(10)   VALUE ZERO.                  
004800 77  W-KVRETINL-SUM              PIC 9(7)    VALUE ZERO.                  
004900 77  W-FIRST-TIME                PIC 9(1)    VALUE ZERO.                  
005100 77  ERROR-TEXT                  PIC X(80)   VALUE SPACE.                 
005200 77  KDRC-DISPLAY                PIC Z(5).                                
005300*                                                                         
005400 01  FELTEXT.                                                             
005500     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
005600     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
005700                                                                          
005800 77  W42828-EOF-SW               PIC X       VALUE 'N'.                   
005900     88  END-OF-W42828                       VALUE 'J'.                   
006000*                                                                         
006010 77  WS-REC-STATUS               PIC X       VALUE 'N'.                   
006020     88  WS-LATEST-REC                       VALUE 'J'.                   
006030*                                                                         
006100 01  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
006200 01  FILLER REDEFINES DAGENS-DATUM.                                       
006300     03  DAGENS-DATUM-AAR        PIC 9(2).                                
006400     03  DAGENS-DATUM-MAANAD     PIC 9(2).                                
006500     03  DAGENS-DATUM-DAG        PIC 9(2).                                
006600*                                                                         
006700 01  WS-RUBRIK                   PIC X(30)   VALUE SPACE.                 
006800 01  FILLER REDEFINES WS-RUBRIK.                                          
006900     03  WS-IDDC                 PIC X(2).                                
007000     03  WS-BINDESTRECK          PIC X(3).                                
007100     03  WS-RADRUBRIK            PIC X(25).                               
007200*                                                                         
007300 01  DYNAMISKA-SUBPROGRAM.                                                
007400*                                                                         
007500     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
007600     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
007700     03  ABEND                   PIC X(8)    VALUE 'ABEND   '.            
007800     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
007900     03  WZ01SEND                PIC X(8)    VALUE 'WZ01SEND'.            
007910     03  WL10WBDC                PIC X(8)    VALUE 'WL10WBDC'.            
008000*                                                                         
008100*    --- PARAMETRAR TILL POSTSUM                                          
008200*                                                                         
008300*01  -COPY W0005   -PRE  POSTSUM-                                         
008400     EJECT                                                                
008500*    --- PARAMETERS TO ABEND                                              
008600 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
008700 77  RKOD-ABEND-NO-DUMP          PIC S9(4)   COMP VALUE +16.              
008800 77  RKOD-ABEND-WITH-DUMP        PIC S9(4)   COMP VALUE +1000.            
008900*                                                                         
009000 01  FILLER                      PIC X(16)   VALUE 'SEND-CONTROL'.        
009100*01  -COPY WZ01SEND                                                       
009200     EJECT                                                                
009201                                                                          
009210*    --- PARAMETRAR TILL WL10WBDC                                         
009220*01  -COPY WL10WBDC                                                       
009300     EJECT                                                                
009400 01  FILLER                      PIC X(16)   VALUE 'HDR-AREA'.            
009500 01  HDR-AREA.                                                            
009600*    03  -COPY WZ01REQU  -PRE HDR-                                        
009700*    03  -COPY WZ04HDR                                                    
009800*                                                                         
009900 01  FILLER                      PIC X(16)   VALUE 'DOC-AREA'.            
010000 01  DOC-AREA.                                                            
010100*    03  -COPY W428281                                                    
010200 01  IN-AREA-START               PIC X(24)   VALUE                        
010300                                             'IN-AREA-START'.             
010400*01  AREA -COPY W4282801   -PRE IN-                                       
010500*                                                                         
010600 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
010700     SKIP3                                                                
010800*    --- STATUS-KOD FRÅN IMS                                              
010900 01  STATUS-WS                   PIC XX.                                  
011000     88  SEGMENT-FINNS                       VALUE '  '.                  
012000     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
012100     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
012200     88  SEGMENT-SLUT                        VALUE 'GB'.                  
012300     88  IMS-EJ-OK                           VALUE 'XD'.                  
012400     SKIP2                                                                
012500 01  GODK-STATUSKODER.                                                    
012600     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
012700     SKIP3                                                                
012800 01  SSA1                        PIC X(64).                               
012900 01  SSA2                        PIC X(64).                               
013000     EJECT                                                                
013100*    --- IMS FUNKTIONSKODER                                               
013200*01  -COPY W0003                                                          
013300     EJECT                                                                
013400 LINKAGE SECTION.                                                         
013500                                                                          
013600*01  -COPY W0009   -PRE MSG-                                              
013700     EJECT                                                                
013800 01  DISTRDOC-PCB                PIC X.                                   
013900     EJECT                                                                
014000 PROCEDURE DIVISION  USING MSG-PCB DISTRDOC-PCB.                          
015000 MAIN SECTION.                                                            
015100     ENTRY 'DLITCBL' USING MSG-PCB DISTRDOC-PCB.                          
015200                                                                          
015300     PERFORM A-INIT                                                       
015400     PERFORM S01-LAES-W42828                                              
015407*                                                                         
015600     PERFORM UNTIL END-OF-W42828                                          
015700       MOVE IN-IDLANDX2     TO SPAR-IDLANDX2                              
015800       MOVE IN-TIAAPP       TO SPAR-DOC-TIAAPP                            
016000       PERFORM S05-OPEN-DAP-SEND                                          
016100       PERFORM S02-FLYTTA-HEADER-DATA                                     
016200       PERFORM S06-PUT-DAP-HEADER                                         
017100*                                                                         
017200       PERFORM UNTIL END-OF-W42828 OR                                     
017300            IN-IDLANDX2 NOT = SPAR-IDLANDX2                               
017400         MOVE ZERO           TO W-TOTAL-SUART-PERIOD                      
017500                                W-TOTAL-SURADER-PERIOD                    
017600                                W-TOTAL-SUART-YEAR                        
017700                                W-TOTAL-SURADER-YEAR                      
017800         MOVE 1              TO W-FIRST-TIME                              
017900         MOVE IN-IDDC        TO SPAR-IDDC                                 
018000*                                                                         
018100         PERFORM UNTIL END-OF-W42828 OR IN-IDDC NOT = SPAR-IDDC           
018200           IF W-FIRST-TIME = 1                                            
018300             MOVE IN-IDDC    TO  DOC-IDDC                                 
018400             MOVE IN-ADCITY  TO  DOC-ADCITY                               
018500             MOVE 'LINE'     TO  DOC-IDAFPRCD                             
018600             MOVE ZERO       TO  W-FIRST-TIME                             
018700             PERFORM S07-PUT-DOC                                          
018800             MOVE ALL '+'    TO  DOC-AREA                                 
018900             MOVE 'LINE'     TO  DOC-IDAFPRCD                             
019000             PERFORM S07-PUT-DOC                                          
019100           END-IF                                                         
019200*                                                                         
019201           MOVE ZERO            TO W-KVRETINL-PERIOD-KD                   
019202                                   W-KVRETINL-YEAR-KD                     
019203                                   W-REPROCENT-YEAR-KD                    
019204                                   W-REPROCENT-PERIOD-KD                  
019205                                   W-TOTAL-SURADER-PERIOD-KD              
019206           MOVE IN-KDANMORS-001 TO SPAR-KDANMORS-001                      
019207           MOVE 'J'             TO WS-REC-STATUS                          
019208*                                                                         
019210           PERFORM UNTIL END-OF-W42828 OR                                 
019220                   IN-KDANMORS-001 NOT = SPAR-KDANMORS-001                
019300             PERFORM B-BEHANDLA-FLYTTA-DATA                               
019400             PERFORM S01-LAES-W42828                                      
019800           END-PERFORM                                                    
019801*                                                                         
019802           MOVE W-KVRETINL-PERIOD-KD  TO DOC-KVRETINL-PERIOD              
019803*                                                                         
019808*          IF W-KVRETINL-PERIOD-KD > 0                                    
019809           IF W-TOTAL-SURADER-PERIOD-KD > 0                               
019810              COMPUTE W-REPROCENT-PERIOD-KD =                             
019811                 W-KVRETINL-PERIOD-KD / W-TOTAL-SURADER-PERIOD-KD         
019813           ELSE                                                           
019814              COMPUTE W-REPROCENT-PERIOD-KD = 0                           
019816           END-IF                                                         
019817           MOVE W-REPROCENT-PERIOD-KD TO DOC-REPROCENT-PERIOD             
019818*                                                                         
019822           PERFORM S07-PUT-DOC                                            
019823*                                                                         
019824           IF IN-IDDC NOT = SPAR-IDDC                                     
019825             IF W-TOTAL-SURADER-YEAR > 0                                  
019826               COMPUTE DOC-REPROCENT-YEAR ROUNDED =                       
019827                       W-TOTAL-SUART-YEAR / W-TOTAL-SURADER-YEAR          
019828             ELSE                                                         
019829               COMPUTE DOC-REPROCENT-YEAR = 0                             
019830             END-IF                                                       
019831             PERFORM C-BEHANDLA-TOTALRAD                                  
019832           END-IF                                                         
019840         END-PERFORM                                                      
019900       END-PERFORM                                                        
020000       PERFORM S08-CLOSE-DAP-SEND                                         
020100     END-PERFORM                                                          
020200*                                                                         
020300     PERFORM Z-FINIT                                                      
020400     MOVE ZERO TO RETURN-CODE                                             
020500     GOBACK                                                               
020600     .                                                                    
020700 A-INIT SECTION.                                                          
020800     OPEN INPUT W42828                                                    
020900     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
021000     .                                                                    
022000 B-BEHANDLA-FLYTTA-DATA  SECTION.                                         
023000     MOVE 'LINE1'             TO DOC-IDAFPRCD                             
024000     MOVE SPAR-DOC-TIAAPP     TO DOC-TIAAPP                               
024100*                                                                         
024200     EVALUATE IN-KDANMORS-001                                             
024300       WHEN '00'                                                          
024400         MOVE '00 - SHORTAGE'               TO DOC-RUBRIK                 
024500       WHEN '11'                                                          
024600         MOVE '11 - OVERAGE'                TO DOC-RUBRIK                 
024700       WHEN '2X'                                                          
024800         MOVE '2X - WRONG PART (MISS PICK)' TO DOC-RUBRIK                 
025000       WHEN '4X'                                                          
025100         MOVE '4X - PACKING-DAMAGE'         TO DOC-RUBRIK                 
025300       WHEN '6X'                                                          
025400         MOVE '6X - TRANSPORT DAMAGE/LOST'  TO DOC-RUBRIK                 
025600     END-EVALUATE                                                         
026100*                                                                         
026102     COMPUTE W-KVRETINL-PERIOD-KD = W-KVRETINL-PERIOD-KD +                
026103                                    IN-SUART-PERIOD                       
026104     COMPUTE W-TOTAL-SURADER-PERIOD-KD =                                  
026105         W-TOTAL-SURADER-PERIOD-KD +     IN-SURADER-PERIOD                
026200                                                                          
027000*                                                                         
027001     IF WS-LATEST-REC                                                     
027010       COMPUTE DOC-KVRETINL-YEAR = IN-SUART-YEAR                          
027011       COMPUTE W-TOTAL-SUART-YEAR = W-TOTAL-SUART-YEAR +                  
027012                                    IN-SUART-YEAR                         
027013       COMPUTE W-TOTAL-SURADER-YEAR = W-TOTAL-SURADER-YEAR +              
027014                                      IN-SURADER-YEAR                     
027020                                                                          
027200       IF IN-SURADER-YEAR > 0                                             
027300         COMPUTE DOC-REPROCENT-YEAR ROUNDED =                             
027400                 IN-SUART-YEAR / IN-SURADER-YEAR                          
027600       ELSE                                                               
027700         MOVE ZERO  TO DOC-REPROCENT-YEAR                                 
027800       END-IF                                                             
027801       MOVE 'N' TO WS-REC-STATUS                                          
027810     END-IF                                                               
027900*                                                                         
028000     COMPUTE W-TOTAL-SUART-PERIOD = W-TOTAL-SUART-PERIOD +                
028100                                    IN-SUART-PERIOD                       
028300                                                                          
028400     COMPUTE W-TOTAL-SURADER-PERIOD = W-TOTAL-SURADER-PERIOD +            
028500                                      IN-SURADER-PERIOD                   
029800     .                                                                    
029900 C-BEHANDLA-TOTALRAD SECTION.                                             
030100     MOVE 'LINE1'              TO DOC-IDAFPRCD                            
030200     MOVE 'TOTAL'              TO DOC-RUBRIK                              
030300     MOVE SPAR-DOC-TIAAPP      TO DOC-TIAAPP                              
030400     MOVE W-TOTAL-SUART-PERIOD TO DOC-KVRETINL-PERIOD                     
030500     MOVE W-TOTAL-SUART-YEAR   TO DOC-KVRETINL-YEAR                       
030600                                                                          
030700     IF W-TOTAL-SURADER-PERIOD > 0                                        
030800       COMPUTE DOC-REPROCENT-PERIOD ROUNDED                               
030900               = W-TOTAL-SUART-PERIOD / W-TOTAL-SURADER-PERIOD            
031100     ELSE                                                                 
031200       MOVE ZERO TO DOC-REPROCENT-PERIOD                                  
031300     END-IF                                                               
031400                                                                          
031500     IF W-TOTAL-SURADER-YEAR > 0                                          
031600       COMPUTE DOC-REPROCENT-YEAR ROUNDED                                 
031700                       = W-TOTAL-SUART-YEAR / W-TOTAL-SURADER-YEAR        
031900     ELSE                                                                 
032000       MOVE ZERO TO DOC-REPROCENT-YEAR                                    
032100     END-IF                                                               
032200*                                                                         
032300     PERFORM S07-PUT-DOC                                                  
032400     MOVE ALL '+'             TO DOC-AREA                                 
032500     MOVE 'LINE'              TO DOC-IDAFPRCD                             
032600     PERFORM S07-PUT-DOC                                                  
032700     .                                                                    
032800 Z-FINIT SECTION.                                                         
032900     CLOSE W42828                                                         
033000     MOVE 'S' TO POSTSUM-OPKOD                                            
033100     CALL POSTSUM USING POSTSUM-PARM                                      
033200     .                                                                    
033300 S01-LAES-W42828  SECTION.                                                
033400     READ W42828 INTO IN-AREA                                             
033500     AT END                                                               
033600        MOVE HIGH-VALUE TO IN-W42828                                      
033700        SET END-OF-W42828 TO TRUE                                         
033800                                                                          
033900     NOT AT END                                                           
034000        MOVE 'W42828'   TO POSTSUM-FDNAMN                                 
034100        MOVE 'W42828D1' TO POSTSUM-DDNAMN2                                
034200        MOVE SPACE      TO POSTSUM-TRANSTYP                               
034300        CALL POSTSUM USING POSTSUM-PARM                                   
034400     END-READ                                                             
034500     .                                                                    
034600 S02-FLYTTA-HEADER-DATA  SECTION.                                         
034610                                                                          
034611*--- COMPUTE WHICH MANAGEMENT FOLLOW UP GROUP THE DC BELONGS TO.          
034612     MOVE IN-IDDC             TO WBDC-IDDC                                
034613     CALL WL10WBDC USING WBDC-AREA                                        
034614                                                                          
034700     MOVE 001                 TO HDR-REQU-IDMSGVER                        
034800     MOVE SPACE               TO HDR-REQU-KDPGMACT                        
034900     MOVE 'W42828'            TO HDR-REQU-IDUSER                          
035000     MOVE 'MANDISCDEVPER'     TO HDR-IDOUTTYPE                            
035100     MOVE SPACE               TO HDR-IDOUTREC                             
035300     MOVE WBDC-KDMFUP         TO HDR-IDOUTREC(1:2)                        
035400     MOVE IN-IDLANDX2         TO HDR-IDOUTREC(3:2)                        
035500     MOVE FUNCTION CURRENT-DATE (3:10) TO WS-YYMMDDHHMM                   
035600     MOVE WS-YYMMDDHHMM       TO HDR-IDLIST                               
035700     .                                                                    
035800 S05-OPEN-DAP-SEND SECTION.                                               
035900     MOVE 'CARPARTS.DAP.DISTRDOCWEB' TO SEND-ADDISPABS                    
036000     MOVE 'OPEN'                     TO SEND-KDFUNC                       
036100     CALL WZ01SEND USING SEND-CONTROL-AREA                                
036200                         SEND-OPEN-AREA                                   
036300     IF SEND-KDRC > ZERO                                                  
036400       MOVE SEND-KDRC                     TO KDRC-DISPLAY                 
036500       STRING 'WZ01SEND OPEN ERROR RC=' KDRC-DISPLAY                      
036600       DELIMITED BY SIZE INTO ERROR-TEXT                                  
036700       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
036800     END-IF                                                               
036900     .                                                                    
037000 S06-PUT-DAP-HEADER SECTION.                                              
037100     MOVE 'PUT'                           TO SEND-KDFUNC                  
037200     MOVE LENGTH OF HDR-AREA              TO SEND-KVDLEN                  
037300     CALL WZ01SEND USING SEND-CONTROL-AREA                                
037400                         SEND-KVDLEN                                      
037500                         HDR-AREA                                         
037600     IF SEND-KDRC > ZERO                                                  
037700       MOVE SEND-KDRC                     TO KDRC-DISPLAY                 
037800       STRING 'WZ01SEND PUT ERROR RC=' KDRC-DISPLAY                       
037900       DELIMITED BY SIZE INTO ERROR-TEXT                                  
038000       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
038100     END-IF                                                               
038200     .                                                                    
038300 S07-PUT-DOC      SECTION.                                                
038400     MOVE 'PUT'                           TO SEND-KDFUNC                  
038500     MOVE LENGTH OF DOC-AREA              TO SEND-KVDLEN                  
038600     CALL WZ01SEND USING SEND-CONTROL-AREA                                
038700                         SEND-KVDLEN                                      
038800                         DOC-AREA                                         
038900     IF SEND-KDRC > ZERO                                                  
039000       MOVE SEND-KDRC                     TO KDRC-DISPLAY                 
039100       STRING 'WZ01SEND PUT ERROR RC=' KDRC-DISPLAY                       
039200       DELIMITED BY SIZE INTO ERROR-TEXT                                  
039300       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
039400     END-IF                                                               
039500     .                                                                    
039600 S08-CLOSE-DAP-SEND SECTION.                                              
039700     MOVE 'CLOSE'                    TO SEND-KDFUNC                       
039800     CALL WZ01SEND USING SEND-CONTROL-AREA                                
039900                                                                          
040000     IF SEND-KDRC > 0                                                     
040100       MOVE SEND-KDRC TO KDRC-DISPLAY                                     
040200       STRING 'WZ01SEND CLOSE ERROR RC=' KDRC-DISPLAY                     
040300       DELIMITED BY SIZE INTO ERROR-TEXT                                  
040400       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
040500     END-IF                                                               
040600     .                                                                    
040700* --- IMS SEKTIONER ---                                                   
040800 IMS-STATUSKONTROLL SECTION.                                              
040900     SKIP2                                                                
041000     SET STATUS-IX TO 1                                                   
041100     SEARCH GODK-STATUS                                                   
041200       AT END                                                             
041300         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
041400           DELIMITED BY SIZE INTO FELTEXT                                 
041500         DISPLAY FELTEXT                                                  
041600         CALL FELLOG                                                      
041700       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
041800         CONTINUE                                                         
041900     END-SEARCH                                                           
042000     .                                                                    
