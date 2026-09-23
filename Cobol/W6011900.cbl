000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W6011900.                                                
000300 AUTHOR.         GUNNAR LARSSON IDK.                                      
000400 DATE-WRITTEN.   92/08/26.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*    FUNKTION:                                                            
000800*        MAKULERA PARTI/BACKA R32(PF23)                                   
000900*                                                                         
001000*        THIS IS A DRIVER PGM FOR TRANSACTIONS W6T119, W6T119U            
002000*                                                                         
002100*        IT TAKES CARE OF ALL TECHNICAL DETAILS RELATED TO WHELP          
002200*        AND 3270 FORMATS AND CALLS SUBPROGRAM W6011910 WHICH             
002300*        CONTAINS ALL BUSINESS LOGIC FOR THESE TRANSACTIONS.              
002400*                                                                         
002500*        A SIMILAR DRIVER PROGRAM FOR INVOCATIONS FROM THE WEB            
002600*        EXISTS - W6W11900 (TRANSACTIONS W6W119T, W6T119U)                
002700*                                                                         
002800*                                                                         
002900     SKIP3                                                                
003000 ENVIRONMENT DIVISION.                                                    
003100     EJECT                                                                
003200 DATA DIVISION.                                                           
003300 WORKING-STORAGE SECTION.                                                 
003400*    -COPY WY2000W9                                                       
003500     SKIP3                                                                
003600 77  IDPGM                       PIC X(08)   VALUE 'W6011900'.            
003700                                                                          
003800*    --- ARBETSFÄLT FÖR FELMEDDELANDEN VID CALL ABEND/FELLOG              
003900 77  FELTEXT                     PIC X(80) VALUE SPACE.                   
004000 77  FILLER                      PIC X(16) VALUE '*ANTAL UPP.'.           
004100 77  JA                          PIC X       VALUE 'J'.                   
004200 77  YES                         PIC X       VALUE 'Y'.                   
004300 77  NEJ                         PIC X       VALUE 'N'.                   
004400                                                                          
004500 77  WS-IDDC-WDB6                PIC X(2)    VALUE SPACE.                 
004600                                                                          
004700*01  -COPY WWDCKONS                                                       
004800                                                                          
004900 77  MAX-MOD-LAENGD              PIC S9(4)  VALUE +241  COMP SYNC.        
005000*    --- ARBETSFÄLT FÖR AKTUELLA NYCKELVÄRDEN FRÅN SKÄRMEN                
006000 77  WS-IDLOPNRM                 PIC X(8)    VALUE SPACE.                 
007000                                                                          
008000 77  INDATA-SW                   PIC X       VALUE 'J'.                   
009000     88  INDATA-OK                           VALUE 'J'.                   
010000     88  INDATA-FEL                          VALUE 'N'.                   
011000                                                                          
011100 77  W-IDTRANS                   PIC X(4)    VALUE SPACE.                 
011200     88  EGEN-MID                            VALUE '6119'.                
011300     88  GODK-MID                            VALUE '6111' '6112'          
011400                                                   '6113' '6114'          
011500                                                   '6115' '6116'          
011600                                                   '6118' '6119'.         
011700     88  HELP-MID                            VALUE '0551'.                
011800     EJECT                                                                
011900*    --- SUBPROGRAM OCH PARAMETERAREOR                                    
012000 01  GENERELLA-SUBPROGRAM.                                                
013000     03  WMEDKONV                PIC X(8)    VALUE 'WMEDKONV'.            
014000     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
015000     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
016000     03  WDATKONV                PIC X(8)    VALUE 'WDATKONV'.            
017000     03  W005INIT                PIC X(8)    VALUE 'W005INIT'.            
018000     03  W6011910                PIC X(8)    VALUE 'W6011910'.            
019000     EJECT                                                                
020000*01 -COPY WMSGINIT                                                        
021000     SKIP3                                                                
022000*    --- PARAMETRAR TILL SUBPROGRAM WMEDKONV                              
023000*01 -COPY WMEDAREA                                                        
024000     SKIP3                                                                
025000 01 WS-IDMSG-ERROR         PIC X(3).                                      
025100    88 WRONG-KEY           VALUE '022'.                                   
025200    88 CORR-HILITE-FLDS    VALUE '020'.                                   
025300    88 007-OTILLATEN-UPD   VALUE '007'.                                   
025400    88 010-NOT-IN-REG      VALUE '025'.                                   
025500    88 QTY-QUAL-IR-EXIST   VALUE '351'.                                   
025600    88 TECH-QUAL-IR-EXIST  VALUE '352'.                                   
025700    88 PF11-AND-NO-DATA    VALUE '014'.                                   
025800    88 QUANT-TOO-BIG       VALUE '298'.                                   
025900    88 SPL-ADV-QTY         VALUE '353'.                                   
026000                                                                          
027000 01 WS-IDMSG-INFO          PIC X(3).                                      
027100    88 PRESS-PF11          VALUE '013'.                                   
027200    88 UPDATE-DONE         VALUE '001'.                                   
027300                                                                          
027400 01  MESSAGE-CODES.                                                       
027500     03  ERR-CORR-HILITE-FLDS    PIC X(3)    VALUE '001'.                 
027600     03  INF-PRESS-PF11          PIC X(3)    VALUE '003'.                 
027700     03  ERR-007-OTILLATEN-UPD   PIC X(3)    VALUE '007'.                 
027800     03  ERR-010-NOT-IN-REG      PIC X(3)    VALUE '010'.                 
027900     03  ERR-PF11-AND-NO-DATA    PIC X(3)    VALUE '011'.                 
028000     03  INF-UPDATE-DONE         PIC X(3)    VALUE '101'.                 
028100     03  ERR-QTY-QUAL-IR-EXIST   PIC X(3)    VALUE '224'.                 
028200     03  ERR-TECH-QUAL-IR-EXIST  PIC X(3)    VALUE '225'.                 
028300     03  ERR-WRONG-KEY           PIC X(3)    VALUE '401'.                 
028400     03  ERR-QUANT-TOO-BIG       PIC X(3)    VALUE '725'.                 
028500     03  ERR-SPL-ADV-QTY         PIC X(3)    VALUE '286'.                 
028600     EJECT                                                                
028700*    --- AREA FÖR WDATKONV                                                
028800 01  FILLER                    PIC X(16) VALUE 'WDATAREA********'.        
028900                                                                          
029000*01  -COPY WDATAREA                                                       
029100     EJECT                                                                
029200                                                                          
029300*    --- AREOR FÖR MFS OCH SKÄRMHANTERING                                 
029400*                                                                         
029500 01  FILLER                      PIC X(16)   VALUE 'MID-AREA'.            
029600     SKIP3                                                                
029700*01  MID -COPY W6I11901                                                   
029800     EJECT                                                                
029900 01  FILLER                      PIC X(16)  VALUE 'MSG/MOD-AREA'.         
030000     SKIP3                                                                
030100*01  -COPY WMSGAREA                                                       
030200     EJECT                                                                
030300     03  MOD REDEFINES MSG-AREA.                                          
030400*      05  -COPY W6O11901                                                 
030500     EJECT                                                                
030600 01  FILLER                      PIC X(16)   VALUE 'MFS-AREA'.            
030700     SKIP3                                                                
030800*01  -COPY WMFSAREA                                                       
030900     EJECT                                                                
031000*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
031100*                                                                         
031200     SKIP2                                                                
031300 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
031400     SKIP2                                                                
031500******************************************************************        
031600*                                                                         
031700*                AREOR FÖR ANROP TILL W6011910                            
031800*                                                                         
031900 01  FILLER                    PIC X(16) VALUE 'REQU-AREA'.               
032000 01  REQU-AREA.                                                           
033000     03 -COPY WZ01REQU                                                    
034000     03 -COPY W60119I1                                                    
035000                                                                          
036000 01  FILLER                    PIC X(16) VALUE 'RESP-AREA'.               
037000 01  RESP-AREA.                                                           
038000     03 -COPY WZ01RESP                                                    
039000     03 -COPY W60119O1                                                    
040000                                                                          
041000*    --- STATUS-KOD FRÅN IMS                                              
042000 01  STATUS-WS                   PIC XX.                                  
042100     88  SEGMENT-FINNS                       VALUE '  '.                  
042200     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
042300     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
042400     88  BASEN-SLUT                          VALUE 'GE'.                  
042500     SKIP2                                                                
042600 01  GODK-STATUSKODER.                                                    
042700     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
042800     SKIP3                                                                
042900 01  SSA1                        PIC X(128).                              
043000 01  SSA2                        PIC X(64).                               
043100 01  SSA3                        PIC X(64).                               
043200     EJECT                                                                
043300*    --- IMS FUNKTIONSKODER                                               
043400*01  -COPY W0003                                                          
043500     EJECT                                                                
043600     EJECT                                                                
043700 LINKAGE SECTION.                                                         
043800*01  -COPY W0009  -PRE MSG-                                               
043900*01  -COPY W0008  -PRE USEA-                                              
044000     05  FILLER             PIC X.                                        
045000 01  INLA-PCB               PIC X.                                        
046000 01  INLC-PCB               PIC X.                                        
047000 01  ARTC-PCB               PIC X.                                        
048000 01  INLB-PCB               PIC X.                                        
049000 01  INLE-PCB               PIC X.                                        
050000 01  ZZAC-PCB               PIC X.                                        
060000 01  LASA-PCB               PIC X.                                        
061000 01  KVAE-PCB               PIC X.                                        
062000 01  ARTS-PCB               PIC X.                                        
063000 01  INLC-INL-PCB           PIC X.                                        
064000 01  LOGA-PCB               PIC X.                                        
065000 01  FILB-PCB               PIC X.                                        
065100 01  SAPA-PCB               PIC X.                                        
065200 01  WDG2-PCB               PIC X.                                        
065210 01  9305-AVG-PCB           PIC X.                                        
065220 01  AVG-WDB6-PCB           PIC X.                                        
065300 01  WDB6-LEV-PCB           PIC X.                                        
065400 01  WDB6-PCB               PIC X.                                        
065410 01  WDK7-PCB               PIC X.                                        
065420 01  WDF1-PCB               PIC X.                                        
065430 01  9305-PCB               PIC X.                                        
065500     EJECT                                                                
065600 PROCEDURE DIVISION  USING                                                
065700                     MSG-PCB  USEA-PCB INLC-PCB                           
065800                     INLA-PCB ARTC-PCB INLB-PCB                           
065900                     INLE-PCB ZZAC-PCB                                    
066000                     LASA-PCB KVAE-PCB ARTS-PCB                           
067000                     INLC-INL-PCB LOGA-PCB                                
067100                     FILB-PCB SAPA-PCB WDG2-PCB 9305-AVG-PCB              
067200                     AVG-WDB6-PCB WDB6-LEV-PCB WDB6-PCB WDK7-PCB          
067210                     WDF1-PCB 9305-PCB.                                   
067300     ENTRY 'DLITCBL' USING                                                
067400                     MSG-PCB  USEA-PCB INLC-PCB                           
067500                     INLA-PCB ARTC-PCB INLB-PCB                           
067600                     INLE-PCB ZZAC-PCB                                    
067700                     LASA-PCB KVAE-PCB ARTS-PCB                           
067800                     INLC-INL-PCB LOGA-PCB                                
067900                     FILB-PCB SAPA-PCB WDG2-PCB 9305-AVG-PCB              
068000                     AVG-WDB6-PCB WDB6-LEV-PCB WDB6-PCB WDK7-PCB          
068010                     WDF1-PCB 9305-PCB.                                   
068100                                                                          
068200     PERFORM IMS-GET-MSG                                                  
068300     IF SEGMENT-FINNS                                                     
068400       PERFORM A-INIT                                                     
068500       PERFORM B-INIT-KEYS                                                
068600       MOVE ALL '+' TO REQU-INPUT                                         
068700       IF MFS-UPDATE                                                      
068800         MOVE MID-FLMAK  TO REQU-FLMAK                                    
068900         MOVE MID-FLBACK TO REQU-FLBACK                                   
069000         SET REQU-UPDATE TO TRUE                                          
069100       ELSE                                                               
069200         IF MFS-UPD-V                                                     
069300           MOVE MID-FLMAK  TO REQU-FLMAK                                  
069400           MOVE MID-FLBACK TO REQU-FLBACK                                 
069500           SET REQU-UPD-V TO TRUE                                         
069600         ELSE                                                             
069700           IF MFS-FIRST                                                   
069800             SET REQU-FIRST TO TRUE                                       
069900           ELSE                                                           
070000             SET REQU-QUERY TO TRUE                                       
070100           END-IF                                                         
070200         END-IF                                                           
070300       END-IF                                                             
070400       PERFORM F-BUSINESS-LOGIC-W6011910                                  
070500*                                                                         
070600       MOVE MAX-MOD-LAENGD TO MSG-KVLL                                    
070700       PERFORM IMS-INSERT-MSG                                             
070800     END-IF                                                               
070900                                                                          
071000     MOVE ZERO TO RETURN-CODE                                             
071100     GOBACK                                                               
071200                                                                          
071300     .                                                                    
071400     EJECT                                                                
071500 A-INIT SECTION.                                                          
071600                                                                          
071700     IF MSG-DUBBLA-TRANSKODER                                             
071800       MOVE MSG-INDATA-MINUS-2-TRANSKODER TO MID-W6I11901                 
071900       MOVE MSG-IDTRANS-2 TO MFS-IDTRANS                                  
072000       MOVE MSG-KDMFSFOR-2 TO MFS-KDMFSFOR                                
073000     ELSE                                                                 
074000       MOVE MSG-INDATA-MINUS-1-TRANSKOD  TO MID-W6I11901                  
075000       MOVE MSG-IDTRANS-1 TO MFS-IDTRANS                                  
076000       MOVE MSG-KDMFSFOR-1 TO MFS-KDMFSFOR                                
076100     END-IF                                                               
076200                                                                          
076300     MOVE MSG-KDTRTYP TO MFS-KDTRTYP                                      
076400     MOVE MSG-IDPFK TO MFS-IDPFK                                          
076500     MOVE MFS-IDTRANS TO W-IDTRANS                                        
076600                                                                          
076700     MOVE LOW-VALUE TO MSG-AREA                                           
076800     MOVE 'W6O119N1' TO MFS-IDMOD                                         
076900     MOVE '6119' TO MOD-IDTRANS                                           
077000     MOVE MFS-RENSA-FAELT TO MOD-TEMFSFEL MOD-TEMFSINF                    
077100                                                                          
077200     IF EGEN-MID OR HELP-MID                                              
077300       CONTINUE                                                           
077400     ELSE                                                                 
077500       MOVE SPACE TO MFS-KDTRTYP                                          
077600       MOVE '7' TO MFS-IDPFK                                              
077700     END-IF                                                               
077800                                                                          
077900     PERFORM AA-INIT-NYCKLAR                                              
078000                                                                          
079000     IF MSGI-IDLAND-SPR = 'GB'                                            
080000       MOVE 'GB ' TO MED-IDSKYLT                                          
080100     ELSE                                                                 
080200       MOVE 'S  ' TO MED-IDSKYLT                                          
080300     END-IF                                                               
080400     .                                                                    
080500     EJECT                                                                
080600*----------------------------------------------------------------*        
080700 AA-INIT-NYCKLAR SECTION.                                                 
080800                                                                          
080900     MOVE ALL '+' TO MSGI-WMSGINIT                                        
081000     MOVE '001'                  TO MSGI-KDCALL                           
081100     MOVE MSG-SIGNON-USERID      TO MSGI-IDUSER                           
081200     MOVE MSG-LTERM-NAME         TO MSGI-IDLTERM-USER                     
081300     MOVE '6119'                 TO MSGI-IDTRANS                          
081400     CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                           
081500     .                                                                    
081600     EJECT                                                                
081700 B-INIT-KEYS     SECTION.                                                 
081800                                                                          
081900     PERFORM BA-KONTROLL-AV-IDLOPNRM                                      
082000     PERFORM BB-KONTROLL-AV-IDDC                                          
082100                                                                          
082200     IF GODK-MID                                                          
082300       MOVE WS-IDLOPNRM     TO MOD-IDLOPNRM-UT                            
082400       INSPECT MOD-IDLOPNRM-UT                                            
082500               REPLACING LEADING ZERO BY SPACE                            
082600       MOVE REQU-IDDC-KEY   TO MOD-IDDC-UT                                
082900     ELSE                                                                 
083000       MOVE MFS-RENSA-FAELT TO MOD-IDLOPNRM-UT                            
083100                               MOD-IDARTNR                                
083200                               MOD-KVAVIS                                 
084000                               MOD-BEART                                  
084100                               MOD-IDLEVNR                                
084200                               MOD-IDFS                                   
084300                               MOD-TIAVIDAT                               
084400                               MOD-IDDC-UT                                
084600     END-IF                                                               
084800                                                                          
084900     .                                                                    
085000     EJECT                                                                
085100 BA-KONTROLL-AV-IDLOPNRM    SECTION.                                      
085200     SKIP2                                                                
086000     MOVE MFS-RENSA-FAELT TO MOD-IDLOPNRM-IN                              
086100                                                                          
086200     IF MID-IDLOPNRM-IN = ALL '+'                                         
086300       MOVE MID-IDLOPNRM-UT TO WS-IDLOPNRM                                
086400       INSPECT WS-IDLOPNRM REPLACING LEADING SPACE BY ZERO                
086500       MOVE WS-IDLOPNRM     TO REQU-IDLOPNRM-KEY                          
086600     ELSE                                                                 
086700       MOVE MID-IDLOPNRM-IN TO WS-IDLOPNRM                                
086800                               REQU-IDLOPNRM-KEY                          
086900       MOVE '7'             TO MFS-IDPFK                                  
087000       MOVE SPACE           TO MFS-KDTRTYP                                
088000     END-IF                                                               
088100     .                                                                    
088200     EJECT                                                                
088300 BB-KONTROLL-AV-IDDC        SECTION.                                      
088400     SKIP2                                                                
088500*    MOVE MFS-RENSA-FAELT TO MOD-IDDC-UT                                  
088600     MOVE MFS-RENSA-FAELT TO MOD-IDDC-IN                                  
088700                                                                          
088800     IF EGEN-MID OR HELP-MID                                              
088900       IF MID-IDDC-IN = ALL '+'                                           
089000         MOVE MSGI-IDDC   TO REQU-IDDC-KEY                                
089100       ELSE                                                               
089200         MOVE MID-IDDC-IN TO REQU-IDDC-KEY                                
089300         MOVE    SPACE    TO MFS-KDTRTYP                                  
089400         MOVE     '7'     TO MFS-IDPFK                                    
089500       END-IF                                                             
089600     ELSE                                                                 
089700       IF GODK-MID                                                        
089800         MOVE MSGI-IDDC   TO REQU-IDDC-KEY                                
089900       ELSE                                                               
090000         MOVE SPACE       TO REQU-IDDC-KEY                                
090100       END-IF                                                             
091000     END-IF                                                               
091100     .                                                                    
091200     EJECT                                                                
091300 F-BUSINESS-LOGIC-W6011910    SECTION.                                    
091400                                                                          
091500     CALL W6011910 USING                                                  
091600          REQU-AREA RESP-AREA                                             
091700          MSG-PCB           INLC-PCB                                      
091800          INLA-PCB ARTC-PCB INLB-PCB                                      
091900          INLE-PCB ZZAC-PCB                                               
092000          LASA-PCB KVAE-PCB ARTS-PCB                                      
093000          INLC-INL-PCB LOGA-PCB                                           
094000          FILB-PCB SAPA-PCB WDG2-PCB 9305-AVG-PCB AVG-WDB6-PCB            
095000          WDB6-LEV-PCB WDB6-PCB WDK7-PCB WDF1-PCB                         
096000          9305-PCB                                                        
097000     PERFORM FA-SET-MSG-AND-HILIGHT                                       
098000     PERFORM FB-MOVE-RESP-TO-MOD                                          
099000     .                                                                    
100000     EJECT                                                                
110000                                                                          
120000 FA-SET-MSG-AND-HILIGHT   SECTION.                                        
130000                                                                          
140000     MOVE RESP-IDMSG-ERROR TO WS-IDMSG-ERROR                              
150000     MOVE RESP-IDMSG-INFO  TO WS-IDMSG-INFO                               
160000                                                                          
170000     IF WRONG-KEY                                                         
180000       MOVE NEJ                   TO INDATA-SW                            
190000       MOVE ERR-WRONG-KEY         TO MED-IDMFSFEL                         
200000     END-IF                                                               
210000                                                                          
220000     IF PF11-AND-NO-DATA                                                  
230000       MOVE ERR-PF11-AND-NO-DATA  TO MED-IDMFSFEL                         
240000     END-IF                                                               
250000                                                                          
260000     IF SPL-ADV-QTY                                                       
270000       MOVE ERR-SPL-ADV-QTY       TO MED-IDMFSFEL                         
280000     END-IF                                                               
290000                                                                          
300000     IF CORR-HILITE-FLDS                                                  
310000       MOVE ERR-CORR-HILITE-FLDS  TO MED-IDMFSFEL                         
320000     END-IF                                                               
330000                                                                          
340000     IF CORR-HILITE-FLDS                                                  
350000       MOVE ERR-CORR-HILITE-FLDS  TO MED-IDMFSFEL                         
360000     END-IF                                                               
370000                                                                          
380000     IF PF11-AND-NO-DATA                                                  
390000       MOVE ERR-PF11-AND-NO-DATA  TO MED-IDMFSFEL                         
400000     END-IF                                                               
401000                                                                          
402000     IF QUANT-TOO-BIG                                                     
403000       MOVE ERR-QUANT-TOO-BIG     TO MED-IDMFSFEL                         
404000     END-IF                                                               
405000                                                                          
406000     IF 007-OTILLATEN-UPD                                                 
406100       MOVE ERR-007-OTILLATEN-UPD TO MED-IDMFSFEL                         
406200     END-IF                                                               
406300                                                                          
406400     IF 010-NOT-IN-REG                                                    
406500       MOVE ERR-010-NOT-IN-REG    TO MED-IDMFSFEL                         
406600     END-IF                                                               
406700                                                                          
406800     IF QTY-QUAL-IR-EXIST                                                 
406900       MOVE ERR-QTY-QUAL-IR-EXIST TO MED-IDMFSFEL                         
407000     END-IF                                                               
407100                                                                          
407200     IF TECH-QUAL-IR-EXIST                                                
407300       MOVE ERR-TECH-QUAL-IR-EXIST TO MED-IDMFSFEL                        
407400     END-IF                                                               
407500                                                                          
407600     CALL WMEDKONV USING MED-WMEDAREA                                     
407700     MOVE MED-TEMFSFEL    TO MOD-TEMFSFEL                                 
407800     PERFORM MFS-ROER-EJ-FAELT-UT                                         
407900     PERFORM MFS-ROER-EJ-FAELT-IN                                         
408000*                                                                         
409000     IF PRESS-PF11                                                        
410000       MOVE INF-PRESS-PF11      TO MED-IDMFSINF                           
410100     END-IF                                                               
410200                                                                          
410300     IF UPDATE-DONE                                                       
410400       MOVE INF-UPDATE-DONE     TO MED-IDMFSINF                           
410500     END-IF                                                               
410600                                                                          
410700     CALL WMEDKONV USING MED-WMEDAREA                                     
410800     MOVE MED-TEMFSINF  TO MOD-TEMFSINF                                   
410900     .                                                                    
411000     EJECT                                                                
412000                                                                          
412100 FB-MOVE-RESP-TO-MOD      SECTION.                                        
412200     IF NOT WRONG-KEY                                                     
412300*                                                                         
412400     IF RESP-IDARTNR = ALL '+'                                            
412500       MOVE MFS-ROER-EJ-FAELT  TO MOD-IDARTNR                             
412600     ELSE                                                                 
412700       MOVE RESP-IDARTNR       TO MOD-IDARTNR                             
412800     END-IF                                                               
412900*                                                                         
413000     IF RESP-KVAVIS = ALL '+'                                             
414000       MOVE MFS-ROER-EJ-FAELT  TO MOD-KVAVIS                              
414100     ELSE                                                                 
414200       MOVE RESP-KVAVIS        TO MOD-KVAVIS                              
414300     END-IF                                                               
414400*                                                                         
414500     IF RESP-BEART = ALL '+'                                              
414600       MOVE MFS-ROER-EJ-FAELT  TO MOD-BEART                               
414700     ELSE                                                                 
414800       MOVE RESP-BEART         TO MOD-BEART                               
414900     END-IF                                                               
415000*                                                                         
415100     IF RESP-IDLEVNR = ALL '+'                                            
415200       MOVE MFS-ROER-EJ-FAELT  TO MOD-IDLEVNR                             
415300     ELSE                                                                 
415400       MOVE RESP-IDLEVNR       TO MOD-IDLEVNR                             
415500     END-IF                                                               
415600*                                                                         
415700     IF RESP-IDFS  = ALL '+'                                              
415800       MOVE MFS-ROER-EJ-FAELT  TO MOD-IDFS                                
415900     ELSE                                                                 
416000       MOVE RESP-IDFS          TO MOD-IDFS                                
416100     END-IF                                                               
416200*                                                                         
416300     IF RESP-TIAVIDAT = ALL '+'                                           
416400       MOVE MFS-ROER-EJ-FAELT  TO MOD-TIAVIDAT                            
416500     ELSE                                                                 
416600       MOVE RESP-TIAVIDAT      TO MOD-TIAVIDAT                            
416700     END-IF                                                               
416800*                                                                         
416900     MOVE RESP-FLMAK-ATTR      TO MOD-FLMAK-ATTR                          
417000     IF RESP-FLMAK = ALL '+'                                              
417100       MOVE MFS-ROER-EJ-FAELT  TO MOD-FLMAK                               
417200     ELSE                                                                 
417300       MOVE RESP-FLMAK         TO MOD-FLMAK                               
417400     END-IF                                                               
417500*                                                                         
417600     MOVE RESP-FLBACK-ATTR     TO MOD-FLBACK-ATTR                         
417700     IF RESP-FLBACK = ALL '+'                                             
417800       MOVE MFS-ROER-EJ-FAELT  TO MOD-FLBACK                              
417900     ELSE                                                                 
418000       MOVE RESP-FLBACK        TO MOD-FLBACK                              
418100     END-IF                                                               
418300*                                                                         
418400     END-IF                                                               
418500*                                                                         
418600     IF UPDATE-DONE                                                       
418700       PERFORM MFS-FORM-ATTR                                              
418800       PERFORM MFS-RENSA-FAELT-IN                                         
418900     END-IF                                                               
419000     .                                                                    
419100     EJECT                                                                
420000                                                                          
430000 MFS-RENSA-FAELT-UT SECTION.                                              
431000                                                                          
432000*    --- ALLA UTDATA-FÄLT                                                 
433000     MOVE MFS-RENSA-FAELT TO MOD-IDARTNR                                  
434000                             MOD-KVAVIS                                   
435000                             MOD-BEART                                    
436000                             MOD-IDLEVNR                                  
436100                             MOD-IDFS                                     
436200                             MOD-TIAVIDAT                                 
436300                             MOD-IDDC-UT                                  
436400     .                                                                    
436500     SKIP2                                                                
436600 MFS-RENSA-FAELT-IN SECTION.                                              
436700                                                                          
436800*    --- ALLA INDATA-FÄLT                                                 
436900     MOVE MFS-RENSA-FAELT TO MOD-FLMAK                                    
437000                             MOD-FLBACK                                   
437100     .                                                                    
437200     EJECT                                                                
437300 MFS-ROER-EJ-FAELT-UT  SECTION.                                           
437400                                                                          
437500*    --- ALLA UTDATA-FÄLT                                                 
437600     MOVE MFS-ROER-EJ-FAELT TO MOD-IDARTNR                                
437700                             MOD-KVAVIS                                   
437800                             MOD-BEART                                    
437900                             MOD-IDLEVNR                                  
438000                             MOD-IDFS                                     
438100                             MOD-TIAVIDAT                                 
438200**                           MOD-IDDC-UT                                  
438300     .                                                                    
438400     SKIP2                                                                
438500 MFS-ROER-EJ-FAELT-IN  SECTION.                                           
438600                                                                          
438700*    --- ALLA INDATA-FÄLT                                                 
438800     MOVE MFS-ROER-EJ-FAELT TO MOD-FLMAK                                  
438900                               MOD-FLBACK                                 
439000     .                                                                    
439100     EJECT                                                                
439200 MFS-FORM-ATTR SECTION.                                                   
439300                                                                          
439400*    --- ALLA INDATA-FÄLT                                                 
439500     MOVE MFS-FORMATETS-ATTR TO MOD-FLMAK-ATTR                            
439600                                MOD-FLBACK-ATTR                           
439700     .                                                                    
439800     EJECT                                                                
439900* --- IMS SEKTIONER ---                                                   
440000     SKIP3                                                                
440100 IMS-GET-MSG SECTION.                                                     
440200                                                                          
440300     MOVE '  QC' TO GODK-STATUSKODER                                      
440400     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
440500     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
440600     PERFORM IMS-STATUSKONTROLL                                           
440700     .                                                                    
440800     SKIP3                                                                
440900 IMS-INSERT-MSG SECTION.                                                  
441000                                                                          
441100     IF MSGI-IDLAND-SPR NOT = 'GB'                                        
441200       MOVE '0' TO MFS-KDHUVOMR                                           
441300     END-IF                                                               
441400     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
441500     MOVE SPACE TO GODK-STATUSKODER                                       
441600     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
441700     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
441800     PERFORM IMS-STATUSKONTROLL                                           
441900     .                                                                    
442000     EJECT                                                                
443000 IMS-STATUSKONTROLL SECTION.                                              
444000                                                                          
445000     SET STATUS-IX TO 1                                                   
446000     SEARCH GODK-STATUS                                                   
447000       AT END                                                             
448000         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
449000         DELIMITED BY SIZE INTO FELTEXT                                   
450000         CALL FELLOG                                                      
451000       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
452000         CONTINUE                                                         
453000     END-SEARCH                                                           
454000     .                                                                    
455000     EJECT                                                                
456000*    -COPY WY2000P9                                                       
