000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W4282300.                                                
000300 AUTHOR.         ELEONOR ÖSTRÖM.                                          
000400 DATE-WRITTEN.   08/09/10.                                                
000500 DATE-COMPILED.                                                           
000700                                                                          
000800*    FUNKTION:                                                            
000900*        PROGRAMMET TAR FIL W42823 SORTERAD PÅ IDLANDX2 OCH DC.           
001000*        LEVERANSANMÄRKNINGAR OCH FAKUTERARDE RADER PER VECKA OCH         
001100*        HITTILLS I ÅR.                                                   
001200*        RÄKNAR UT ÄVEN % PÅ SKEPPADER ARTIKLAR PER VECKA OCH             
001300*        HITTILLS I ÅR. SKICKAR TILL DISTR. & PRINT.                      
001600*                                                                         
001700*        E'TRACKER. 6785206  DATED 2008-05-16                             
001900*                                                                         
002200 ENVIRONMENT DIVISION.                                                    
002300     SKIP2                                                                
002400 INPUT-OUTPUT SECTION.                                                    
002500                                                                          
002600 FILE-CONTROL.                                                            
002700     SKIP2                                                                
002800*          --- VECKANS RADER LEVERANSANM OCH FAKUTURERAT.                 
002900     SELECT W42823                     ASSIGN TO W42823D1.                
003000     EJECT                                                                
003100 DATA DIVISION.                                                           
003200     SKIP3                                                                
003300 FILE SECTION.                                                            
003400     SKIP3                                                                
003500 FD  W42823                                                               
003600     RECORDING       F                                                    
003700     BLOCK CONTAINS  0.                                                   
003800                                                                          
003900*01  -COPY W4282301    -L.                                                
004000     EJECT                                                                
004100 WORKING-STORAGE SECTION.                                                 
004200                                                                          
004300 77  IDPGM                       PIC X(8)    VALUE 'W4282300'.            
004400 77  JA                          PIC X       VALUE 'J'.                   
004500 77  NEJ                         PIC X       VALUE 'N'.                   
004900 77  SPAR-IDDC                   PIC X(2)    VALUE SPACE.                 
004910 77  SPAR-IDLANDX2               PIC X(2)    VALUE SPACE.                 
005000 77  SPAR-DOC-TIAAVV             PIC 9(4)    VALUE ZERO.                  
005100 77  W-TOTAL-SUART-WEEK          PIC 9(5)    VALUE ZERO.                  
005200 77  W-TOTAL-SURADER-WEEK        PIC 9(9)    VALUE ZERO.                  
005300 77  W-TOTAL-SUART-YEAR          PIC 9(5)    VALUE ZERO.                  
005400 77  W-TOTAL-SURADER-YEAR        PIC 9(9)    VALUE ZERO.                  
005500 77  WS-YYMMDDHHMM               PIC 9(10)   VALUE ZERO.                  
005600 77  W-KVRETINL-SUM              PIC 9(7)    VALUE ZERO.                  
005610 77  W-FIRST-TIME                PIC 9(1)    VALUE ZERO.                  
005700                                                                          
005800 77  ERROR-TEXT                  PIC X(80)   VALUE SPACE.                 
005900 77  KDRC-DISPLAY                PIC Z(5).                                
006000     SKIP2                                                                
006100 01  FELTEXT.                                                             
006200     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
006300     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
006400                                                                          
006500 77  W42823-EOF-SW               PIC X       VALUE 'N'.                   
006600     88  END-OF-W42823                       VALUE 'J'.                   
006700     EJECT                                                                
006800 01  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
006900 01  FILLER REDEFINES DAGENS-DATUM.                                       
007000     03  DAGENS-DATUM-AAR        PIC 9(2).                                
007100     03  DAGENS-DATUM-MAANAD     PIC 9(2).                                
007200     03  DAGENS-DATUM-DAG        PIC 9(2).                                
007300     EJECT                                                                
007310 01  WS-RUBRIK                   PIC X(30)   VALUE SPACE.                 
007320 01  FILLER REDEFINES WS-RUBRIK.                                          
007330     03  WS-IDDC                 PIC X(2).                                
007340     03  WS-BINDESTRECK          PIC X(3).                                
007350     03  WS-RADRUBRIK            PIC X(25).                               
007360     EJECT                                                                
007400 01  DYNAMISKA-SUBPROGRAM.                                                
007500*                                                                         
007600     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
007700     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
007800     03  ABEND                   PIC X(8)    VALUE 'ABEND   '.            
007900     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
008000     03  WZ01SEND                PIC X(8)    VALUE 'WZ01SEND'.            
008100     EJECT                                                                
008200*    --- PARAMETRAR TILL POSTSUM                                          
008300*                                                                         
008400*01  -COPY W0005   -PRE  POSTSUM-                                         
008500     EJECT                                                                
008600*    --- PARAMETERS TO ABEND                                              
008800 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
008900 77  RKOD-ABEND-NO-DUMP          PIC S9(4)   COMP VALUE +16.              
009000 77  RKOD-ABEND-WITH-DUMP        PIC S9(4)   COMP VALUE +1000.            
009100*                                                                         
009400 01  FILLER                      PIC X(16)   VALUE 'SEND-CONTROL'.        
009500*01  -COPY WZ01SEND                                                       
009600                                                                          
009700     EJECT                                                                
009800 01  FILLER                      PIC X(16)   VALUE 'HDR-AREA'.            
009900 01  HDR-AREA.                                                            
010000*    03  -COPY WZ01REQU  -PRE HDR-                                        
010100*    03  -COPY WZ04HDR                                                    
010200*                                                                         
010300 01  FILLER                      PIC X(16)   VALUE 'DOC-AREA'.            
010400 01  DOC-AREA.                                                            
010500*    03  -COPY W428231                                                    
010900 01  IN-AREA-START               PIC X(24)   VALUE                        
011000                                             'IN-AREA-START'.             
011300*01  AREA -COPY W4282301   -PRE IN-                                       
011400*                                                                         
011600 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
011700     SKIP3                                                                
012200*    --- STATUS-KOD FRÅN IMS                                              
012300 01  STATUS-WS                   PIC XX.                                  
012400     88  SEGMENT-FINNS                       VALUE '  '.                  
012500     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
012600     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
012700     88  SEGMENT-SLUT                        VALUE 'GB'.                  
012800     88  IMS-EJ-OK                           VALUE 'XD'.                  
012900     SKIP2                                                                
013000 01  GODK-STATUSKODER.                                                    
013100     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
013200     SKIP3                                                                
013300 01  SSA1                        PIC X(64).                               
013400 01  SSA2                        PIC X(64).                               
013500     EJECT                                                                
013600*    --- IMS FUNKTIONSKODER                                               
013700*01  -COPY W0003                                                          
013800     EJECT                                                                
014500 LINKAGE SECTION.                                                         
014600                                                                          
014700*01  -COPY W0009   -PRE MSG-                                              
014800     EJECT                                                                
014900 01  DISTRDOC-PCB                PIC X.                                   
015000     EJECT                                                                
015500 PROCEDURE DIVISION  USING MSG-PCB DISTRDOC-PCB.                          
015600 MAIN SECTION.                                                            
015700     ENTRY 'DLITCBL' USING MSG-PCB DISTRDOC-PCB.                          
015800                                                                          
016000     PERFORM A-INIT                                                       
016100     PERFORM S01-LAES-W42823                                              
016220                                                                          
016300     PERFORM UNTIL END-OF-W42823                                          
016310       MOVE IN-IDLANDX2   TO SPAR-IDLANDX2                                
016350       MOVE IN-TIAAVV     TO SPAR-DOC-TIAAVV                              
017200*                                                                         
017700       PERFORM UNTIL END-OF-W42823 OR                                     
017800            IN-IDLANDX2 NOT = SPAR-IDLANDX2                               
017811         MOVE ZERO           TO W-TOTAL-SUART-WEEK                        
017812                                W-TOTAL-SURADER-WEEK                      
017813                                W-TOTAL-SUART-YEAR                        
017814                                W-TOTAL-SURADER-YEAR                      
017816         MOVE 1              TO W-FIRST-TIME                              
017817         MOVE IN-IDDC        TO SPAR-IDDC                                 
017818*                                                                         
017820         PERFORM UNTIL END-OF-W42823 OR IN-IDDC NOT = SPAR-IDDC           
017900           IF W-FIRST-TIME = 1                                            
017901             PERFORM S05-OPEN-DAP-SEND                                    
017902             PERFORM S02-FLYTTA-HEADER-DATA                               
017903             PERFORM S06-PUT-DAP-HEADER                                   
017910             MOVE IN-IDDC    TO  DOC-IDDC                                 
017911             MOVE IN-ADCITY  TO  DOC-ADCITY                               
017912             MOVE 'LINE'     TO  DOC-IDAFPRCD                             
017913             MOVE ZERO       TO W-FIRST-TIME                              
017914             PERFORM S07-PUT-DOC                                          
017915             MOVE ALL '+'    TO  DOC-AREA                                 
017916             MOVE 'LINE'     TO  DOC-IDAFPRCD                             
017917             PERFORM S07-PUT-DOC                                          
017920           END-IF                                                         
017930*                                                                         
018000           PERFORM B-BEHANDLA-FLYTTA-DATA                                 
018010           PERFORM S01-LAES-W42823                                        
018020           IF IN-IDDC NOT = SPAR-IDDC                                     
018030             PERFORM C-BEHANDLA-TOTALRAD                                  
018040             PERFORM S08-CLOSE-DAP-SEND                                   
018100           END-IF                                                         
018110         END-PERFORM                                                      
018200       END-PERFORM                                                        
018600     END-PERFORM                                                          
018700*                                                                         
018800     PERFORM Z-FINIT                                                      
019000     MOVE ZERO TO RETURN-CODE                                             
019100     GOBACK                                                               
019200     .                                                                    
019400 A-INIT SECTION.                                                          
019700     OPEN INPUT W42823                                                    
019900     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
020000     .                                                                    
023300 B-BEHANDLA-FLYTTA-DATA  SECTION.                                         
023500     MOVE 'LINE1'             TO DOC-IDAFPRCD                             
023700     MOVE SPAR-DOC-TIAAVV     TO DOC-TIAAVV                               
024900                                                                          
024910     IF IN-KDANMORS-001 = '00'                                            
024920       MOVE '00 - SHORTAGE'   TO DOC-RUBRIK                               
024930     ELSE                                                                 
024940       IF IN-KDANMORS-001 = '11'                                          
024950         MOVE '11 - OVERAGE'  TO DOC-RUBRIK                               
024960       ELSE                                                               
024970         IF IN-KDANMORS-001 = '2X'                                        
024980           MOVE '2X - WRONG PART (MISS PICK)' TO DOC-RUBRIK               
024990         ELSE                                                             
024991           IF IN-KDANMORS-001 = '4X'                                      
024992             MOVE '4X - PACKING-DAMAGE' TO DOC-RUBRIK                     
024993           ELSE                                                           
024994             IF IN-KDANMORS-001 = '6X'                                    
024995               MOVE '6X - TRANSPORT DAMAGE/LOST' TO DOC-RUBRIK            
024996             END-IF                                                       
024997           END-IF                                                         
024998         END-IF                                                           
024999       END-IF                                                             
025000     END-IF                                                               
025001                                                                          
025002     MOVE IN-SUART-WEEK       TO DOC-KVRETINL-WEEK                        
025003     IF IN-SURADER-WEEK > 0                                               
025010       COMPUTE DOC-REPROCENT-WEEK ROUNDED                                 
025011                       = IN-SUART-WEEK / IN-SURADER-WEEK                  
025020       END-COMPUTE                                                        
025021     ELSE                                                                 
025022       MOVE ZERO TO DOC-REPROCENT-WEEK                                    
025023     END-IF                                                               
025030                                                                          
025040     MOVE IN-SUART-YEAR       TO DOC-KVRETINL-YEAR                        
025041     IF IN-SURADER-YEAR > 0                                               
025050       COMPUTE DOC-REPROCENT-YEAR ROUNDED                                 
025051                       = IN-SUART-YEAR / IN-SURADER-YEAR                  
025070       END-COMPUTE                                                        
025071     ELSE                                                                 
025072       MOVE ZERO TO DOC-REPROCENT-YEAR                                    
025073     END-IF                                                               
025080                                                                          
025600     COMPUTE W-TOTAL-SUART-WEEK = W-TOTAL-SUART-WEEK +                    
025610                                   IN-SUART-WEEK                          
025620     END-COMPUTE                                                          
025630                                                                          
025640     COMPUTE W-TOTAL-SURADER-WEEK = W-TOTAL-SURADER-WEEK +                
025650                                   IN-SURADER-WEEK                        
025660     END-COMPUTE                                                          
025661                                                                          
025662     COMPUTE W-TOTAL-SUART-YEAR = W-TOTAL-SUART-YEAR +                    
025663                                   IN-SUART-YEAR                          
025664     END-COMPUTE                                                          
025665                                                                          
025666     COMPUTE W-TOTAL-SURADER-YEAR = W-TOTAL-SURADER-YEAR +                
025667                                   IN-SURADER-YEAR                        
025668     END-COMPUTE                                                          
025670                                                                          
026000                                                                          
026100     PERFORM S07-PUT-DOC                                                  
026200     .                                                                    
026310 C-BEHANDLA-TOTALRAD SECTION.                                             
026320                                                                          
026330     MOVE 'LINE1'             TO DOC-IDAFPRCD                             
026333     MOVE 'TOTAL'             TO DOC-RUBRIK                               
026350     MOVE SPAR-DOC-TIAAVV     TO DOC-TIAAVV                               
026360     MOVE W-TOTAL-SUART-WEEK  TO DOC-KVRETINL-WEEK                        
026370     MOVE W-TOTAL-SUART-YEAR  TO DOC-KVRETINL-YEAR                        
026380                                                                          
026381     IF W-TOTAL-SURADER-WEEK > 0                                          
026390       COMPUTE DOC-REPROCENT-WEEK ROUNDED                                 
026391                       = W-TOTAL-SUART-WEEK / W-TOTAL-SURADER-WEEK        
026394       END-COMPUTE                                                        
026395     ELSE                                                                 
026396       MOVE ZERO TO DOC-REPROCENT-WEEK                                    
026397     END-IF                                                               
026398                                                                          
026399     IF W-TOTAL-SURADER-YEAR > 0                                          
026400       COMPUTE DOC-REPROCENT-YEAR ROUNDED                                 
026401                       = W-TOTAL-SUART-YEAR / W-TOTAL-SURADER-YEAR        
026403       END-COMPUTE                                                        
026404     ELSE                                                                 
026405       MOVE ZERO TO DOC-REPROCENT-YEAR                                    
026406     END-IF                                                               
026407                                                                          
026408     PERFORM S07-PUT-DOC                                                  
026409     MOVE ALL '+'             TO DOC-AREA                                 
026410     MOVE 'LINE'              TO DOC-IDAFPRCD                             
026411     PERFORM S07-PUT-DOC                                                  
026412     .                                                                    
026420 Z-FINIT SECTION.                                                         
026700     CLOSE W42823                                                         
026900     MOVE 'S' TO POSTSUM-OPKOD                                            
027000     CALL POSTSUM USING POSTSUM-PARM                                      
027100     .                                                                    
027300 S01-LAES-W42823  SECTION.                                                
027500     READ W42823 INTO IN-AREA                                             
027600     AT END                                                               
027700        MOVE HIGH-VALUE TO IN-W42823                                      
027800        SET END-OF-W42823 TO TRUE                                         
027900                                                                          
028000     NOT AT END                                                           
028100        MOVE 'W42823'   TO POSTSUM-FDNAMN                                 
028200        MOVE 'W42823D1' TO POSTSUM-DDNAMN2                                
028300        MOVE SPACE      TO POSTSUM-TRANSTYP                               
028400        CALL POSTSUM USING POSTSUM-PARM                                   
028500     END-READ                                                             
028600     .                                                                    
028800 S02-FLYTTA-HEADER-DATA  SECTION.                                         
029000     MOVE 001                 TO HDR-REQU-IDMSGVER                        
029100     MOVE SPACE               TO HDR-REQU-KDPGMACT                        
029200     MOVE 'W42823'            TO HDR-REQU-IDUSER                          
029400     MOVE 'DISCDEVWEEK '      TO HDR-IDOUTTYPE                            
029500     MOVE SPACE               TO HDR-IDOUTREC                             
029600     MOVE IN-IDDC             TO HDR-IDOUTREC(1:2)                        
029900     MOVE FUNCTION CURRENT-DATE (3:10) TO WS-YYMMDDHHMM                   
030000     MOVE WS-YYMMDDHHMM       TO HDR-IDLIST                               
030100     .                                                                    
030300 S05-OPEN-DAP-SEND SECTION.                                               
030600     MOVE 'CARPARTS.DAP.DISTRDOCWEB' TO SEND-ADDISPABS                    
030700     MOVE 'OPEN'                     TO SEND-KDFUNC                       
030800     CALL WZ01SEND USING SEND-CONTROL-AREA                                
030900                         SEND-OPEN-AREA                                   
031000     IF SEND-KDRC > ZERO                                                  
031100       MOVE SEND-KDRC                     TO KDRC-DISPLAY                 
031200       STRING 'WZ01SEND OPEN ERROR RC=' KDRC-DISPLAY                      
031300       DELIMITED BY SIZE INTO ERROR-TEXT                                  
031400       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
031500     END-IF                                                               
031600     .                                                                    
031900 S06-PUT-DAP-HEADER SECTION.                                              
032200     MOVE 'PUT'                           TO SEND-KDFUNC                  
032300     MOVE LENGTH OF HDR-AREA              TO SEND-KVDLEN                  
032400     CALL WZ01SEND USING SEND-CONTROL-AREA                                
032500                         SEND-KVDLEN                                      
032600                         HDR-AREA                                         
032700     IF SEND-KDRC > ZERO                                                  
032800       MOVE SEND-KDRC                     TO KDRC-DISPLAY                 
032900       STRING 'WZ01SEND PUT ERROR RC=' KDRC-DISPLAY                       
033000       DELIMITED BY SIZE INTO ERROR-TEXT                                  
033100       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
033200     END-IF                                                               
033300     .                                                                    
033400 S07-PUT-DOC      SECTION.                                                
033700     MOVE 'PUT'                           TO SEND-KDFUNC                  
033800     MOVE LENGTH OF DOC-AREA              TO SEND-KVDLEN                  
033900     CALL WZ01SEND USING SEND-CONTROL-AREA                                
034000                         SEND-KVDLEN                                      
034100                         DOC-AREA                                         
034200     IF SEND-KDRC > ZERO                                                  
034300       MOVE SEND-KDRC                     TO KDRC-DISPLAY                 
034400       STRING 'WZ01SEND PUT ERROR RC=' KDRC-DISPLAY                       
034500       DELIMITED BY SIZE INTO ERROR-TEXT                                  
034600       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
034700     END-IF                                                               
034800     .                                                                    
035000 S08-CLOSE-DAP-SEND SECTION.                                              
035300     MOVE 'CLOSE'                    TO SEND-KDFUNC                       
035400     CALL WZ01SEND USING SEND-CONTROL-AREA                                
035500                                                                          
035600     IF SEND-KDRC > 0                                                     
035700       MOVE SEND-KDRC TO KDRC-DISPLAY                                     
035800       STRING 'WZ01SEND CLOSE ERROR RC=' KDRC-DISPLAY                     
035900       DELIMITED BY SIZE INTO ERROR-TEXT                                  
036000       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
036100     END-IF                                                               
036200     .                                                                    
036500* --- IMS SEKTIONER ---                                                   
037800 IMS-STATUSKONTROLL SECTION.                                              
037900     SKIP2                                                                
038000     SET STATUS-IX TO 1                                                   
038100     SEARCH GODK-STATUS                                                   
038200       AT END                                                             
038300         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
038400           DELIMITED BY SIZE INTO FELTEXT                                 
038500         DISPLAY FELTEXT                                                  
038600         CALL FELLOG                                                      
038700       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
038800         CONTINUE                                                         
038900     END-SEARCH                                                           
039000     .                                                                    
