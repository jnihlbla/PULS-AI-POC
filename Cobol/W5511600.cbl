000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W5511600.                                                
000300 AUTHOR.         MARKUS ASPFJÄLL.                                         
000400 DATE-WRITTEN.   00/11/03.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700                                                                          
000800*    FUNKTION:                                                            
000900*        LÄSER IN FIL IFRÅN EXCEL OCH UPPDATERAR WDF1                     
001000*        MED NY TULLFAKTOR                                                
001100*                                                                         
001200*        PROGRAMMET UPPDATERAR WDF1                                       
001300*                                                                         
001400                                                                          
001500     SKIP3                                                                
001600 ENVIRONMENT DIVISION.                                                    
001700     SKIP2                                                                
001800 INPUT-OUTPUT SECTION.                                                    
001900                                                                          
002000 FILE-CONTROL.                                                            
002100     SKIP2                                                                
002200*          --- EXCELFIL                                                   
002300     SELECT W55144                     ASSIGN TO W55116D1.                
002400     SKIP2                                                                
002500*          --- FELFIL MED FELPOSTER                                       
002600     SELECT W5514X                     ASSIGN TO W55116D2.                
002700     EJECT                                                                
002800 DATA DIVISION.                                                           
002900     SKIP3                                                                
003000 FILE SECTION.                                                            
003100     SKIP3                                                                
003200 FD  W55144                                                               
003300     RECORDING       F                                                    
003400     BLOCK CONTAINS  0.                                                   
003500 01  FILLER        PIC X(250).                                            
003600     SKIP3                                                                
003700 FD  W5514X                                                               
003800     RECORDING       V                                                    
003900     BLOCK CONTAINS  0.                                                   
004000 01  W5514X00    PIC X(250).                                              
004100     EJECT                                                                
004200 WORKING-STORAGE SECTION.                                                 
004300                                                                          
004400                                                                          
004500*    -- CHECKED BY WY2000                                                 
004600 77  IDPGM                       PIC X(8)    VALUE 'W5511600'.            
004700 01  CHKP-VAR.                                                            
004800     03 CHKP-MSG-IO-AREA-LENGTH  PIC S9(9)   VALUE +32 COMP SYNC.         
004900     03 CHKP-MSG-IO-AREA         PIC X(32)   VALUE SPACE.                 
005000     03 CHKP-AREA-LENGTH         PIC S9(9)   VALUE +32 COMP SYNC.         
005100     03 CHKP-AREA                PIC X(32)   VALUE SPACE.                 
005200     03 CHKP-ANT                 PIC S9(3)   VALUE +0   COMP-3.           
005300     03 CHKP-MAX                 PIC S9(3)   VALUE +500 COMP-3.           
005400 77  JA                          PIC X       VALUE 'J'.                   
005500 77  NEJ                         PIC X       VALUE 'N'.                   
005600 77  OK-SW                       PIC X       VALUE 'N'.                   
005700     88 OK                                   VALUE 'J'.                   
005800     SKIP2                                                                
005900 01  FELTEXT.                                                             
006000     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
006100     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
006200                                                                          
006300 77  W55144-EOF-SW               PIC X       VALUE 'N'.                   
006400     88  END-OF-W55144                       VALUE 'J'.                   
006500     EJECT                                                                
006600 01  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
006700 01  FILLER REDEFINES DAGENS-DATUM.                                       
006800     03  DAGENS-DATUM-AAR        PIC 9(2).                                
006900     03  DAGENS-DATUM-MAANAD     PIC 9(2).                                
007000     03  DAGENS-DATUM-DAG        PIC 9(2).                                
007100                                                                          
007200 01  NEXT-AAR                    PIC 99     VALUE ZERO.                   
007300                                                                          
007400 01  INFIL-DATUM                PIC 9(6)    VALUE ZERO.                   
007500 01  FILLER REDEFINES INFIL-DATUM.                                        
007600     03  INFIL-DATUM-AAR        PIC 9(2).                                 
007700     03  INFIL-DATUM-MAANAD     PIC 9(2).                                 
007800     03  INFIL-DATUM-DAG        PIC 9(2).                                 
007900                                                                          
008000     EJECT                                                                
008100 01  WS-OLD-TULLFAKT             PIC 9(7).                                
008200 01  FILLER REDEFINES WS-OLD-TULLFAKT.                                    
008300     03 WS-OLD-TULLTAL           PIC 9(3).                                
008400     03 WS-OLD-TULLDEC           PIC 9(4).                                
008500                                                                          
008600 01  WS-NEW-TULLFAKT             PIC 9(7).                                
008700 01  FILLER REDEFINES WS-NEW-TULLFAKT.                                    
008800     03 WS-NEW-TULLTAL           PIC 9(3).                                
008900     03 WS-NEW-TULLDEC           PIC 9(4).                                
009000                                                                          
009100 01  WS-DEC-OLDTULL              PIC 9(3)V9(4).                           
009200 01  FILLER REDEFINES WS-DEC-OLDTULL.                                     
009300     03  WS-DEC-OLDTULL-TAL      PIC 9(3).                                
009400     03  WS-DEC-OLDTULL-DEC      PIC 9(4).                                
009500                                                                          
009600 01  WS-DEC-NEWTULL              PIC 9(3)V9(4).                           
009700 01  FILLER REDEFINES WS-DEC-NEWTULL.                                     
009800     03  WS-DEC-NEWTULL-TAL      PIC 9(3).                                
009900     03  WS-DEC-NEWTULL-DEC      PIC 9(4).                                
010000                                                                          
010100     EJECT                                                                
010200                                                                          
010300 01  DYNAMISKA-SUBPROGRAM.                                                
010400*                                                                         
010500     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
010600     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
010700     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
010800     EJECT                                                                
010900*    --- PARAMETRAR TILL POSTSUM                                          
011000*                                                                         
011100*01  -COPY W0005   -PRE  POSTSUM-                                         
011200     EJECT                                                                
011300 01  IN-AREA-START               PIC X(24)  VALUE                         
011400                                             'IN-AREA-START'.             
011500 01  IN-R18-AREA                 PIC X(250).                              
011600 01  FILLER REDEFINES IN-R18-AREA.                                        
011700     03 IN-R18-COUNTRY           PIC X(2).                                
011710     03 IN-R18-TYP               PIC X(3).                                
011800     03 IN-R18-LEVNR             PIC X(5).                                
011900     03 IN-R18-KOD               PIC 9.                                   
012000     03 IN-R18-DATUM             PIC 9(6).                                
012100     03 IN-R18-NEW-TULL          PIC 9(7).                                
012200     03 IN-R18-OLD-TULL          PIC 9(7).                                
012300     03 IN-R18-VALUTAKOD         PIC 9(3).                                
012400                                                                          
012500     SKIP2                                                                
012600     EJECT                                                                
012700 01  UT-AREA-START               PIC X(24)  VALUE                         
012800                                             'UT-AREA-START'.             
012900 01  UT-R18-AREA                 PIC X(250).                              
013000 01  FILLER REDEFINES UT-R18-AREA.                                        
013100     03 UT-R18-COUNTRY           PIC X(2).                                
013110     03 UT-R18-TYP               PIC X(3).                                
013200     03 UT-R18-LEVNR             PIC X(5).                                
013300     03 UT-R18-KOD               PIC 9.                                   
013400     03 UT-R18-DATUM             PIC 9(6).                                
013500     03 UT-R18-NEW-TULL          PIC 9(7).                                
013600     03 UT-R18-OLD-TULL          PIC 9(7).                                
013700     03 UT-R18-VALUTAKOD         PIC 9(3).                                
013800     03 UT-R18-TEXT              PIC X(20).                               
013900                                                                          
014000                                                                          
014100     SKIP2                                                                
014200*                                                                         
014300     EJECT                                                                
014400 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
014500     SKIP3                                                                
014600 01  NYCKLAR-TILL-DLI.                                                    
014700     03  W-IDLEVNR-X.                                                     
014800         05  W-IDLEVNR           PIC X(5)   VALUE SPACE.                  
014900     03  W-IDLAND-X.                                                      
015000         05  W-IDLAND            PIC X(2)   VALUE SPACE.                  
015100     03  W-KDVALLEV-X.                                                    
015200         05  W-KDVALLEV          PIC S9(3)   VALUE ZERO COMP-3.           
015300     SKIP2                                                                
015400*    --- STATUS-KOD FRÅN IMS                                              
015500 01  STATUS-WS                   PIC XX.                                  
015600     88  SEGMENT-FINNS                       VALUE '  '.                  
015700     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
015800     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
015900     88  SEGMENT-SLUT                        VALUE 'GB'.                  
016000     88  IMS-EJ-OK                           VALUE 'XD'.                  
016100     SKIP2                                                                
016200 01  GODK-STATUSKODER.                                                    
016300     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
016400     SKIP3                                                                
016500 01  SSA1                        PIC X(64).                               
016600 01  SSA2                        PIC X(64).                               
016700     EJECT                                                                
016800*    --- IMS FUNKTIONSKODER                                               
016900*01  -COPY W0003                                                          
017000     EJECT                                                                
017100*    ---  DLI INPUT-OUTPUT AREA                                           
017200                                                                          
017300 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDF101'.                      
017400 01  DLI-IO-WDF101.                                                       
017500*    03  -COPY WDF101                                                     
017600     EJECT                                                                
017700 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDF102'.                      
017800 01  DLI-IO-WDF102.                                                       
017900*    03  -COPY WDF102                                                     
018000                                                                          
018100     EJECT                                                                
018200 LINKAGE SECTION.                                                         
018300                                                                          
018400*01  -COPY W0009   -PRE MSG-                                              
018500                                                                          
018600*01  -COPY W0008  -PRE WDF1-                                              
018700     05  FILLER                  PIC X.                                   
018800     EJECT                                                                
018900 PROCEDURE DIVISION  USING MSG-PCB WDF1-PCB.                              
019000 MAIN SECTION.                                                            
019100     ENTRY 'DLITCBL' USING MSG-PCB WDF1-PCB.                              
019200                                                                          
019300     SKIP2                                                                
019400     PERFORM A-INIT                                                       
019500     PERFORM S01-LAES-W55144                                              
019600     PERFORM UNTIL END-OF-W55144                                          
019700       IF CHKP-ANT > CHKP-MAX                                             
019800         PERFORM X-TAG-CHECKPOINT                                         
019900       END-IF                                                             
020000       PERFORM B-FLYTTA-VARDEN                                            
020100       IF OK                                                              
020200         PERFORM IMS-GET-WDF101                                           
020300         IF SEGMENT-FINNS                                                 
020400           MOVE IN-R18-COUNTRY TO W-IDLAND                                
020500           PERFORM IMS-GET-WDF102                                         
020600           IF SEGMENT-FINNS                                               
020800             IF OK                                                        
020900**** FÖR ATT KÖRA IN PRISER EN ANDRA GÅNG INNAN ÅRSSKIFTET                
021000               IF TULL-TITULF  = IN-R18-DATUM                             
021100                 MOVE IN-R18-DATUM    TO TULL-TITULF                      
021200                 MOVE WS-DEC-NEWTULL  TO TULL-RETULF-1                    
021300                 PERFORM IMS-REPL-WDF102                                  
021400                 MOVE +1 TO CHKP-ANT                                      
021500**** SLUT FIXEN FÖR ATT KÖRA IN PRISER FLERA GÅNGER UNDER ÅRET            
021600               ELSE                                                       
021700                 MOVE IN-R18-DATUM    TO TULL-TITULF                      
021800                 MOVE TULL-RETULF-1   TO TULL-RETULF-2                    
021900                 MOVE WS-DEC-NEWTULL  TO TULL-RETULF-1                    
022000                 PERFORM IMS-REPL-WDF102                                  
022100                 MOVE +1 TO CHKP-ANT                                      
022200               END-IF                                                     
022300             ELSE                                                         
022400               MOVE 'DATUM FEL PÅ FIL' TO UT-R18-TEXT                     
022500               PERFORM S11-SKRIV-W5514X                                   
022600             END-IF                                                       
022700           ELSE                                                           
022800             MOVE 'SEGMENT 02 SAKNAS' TO UT-R18-TEXT                      
022900             PERFORM S11-SKRIV-W5514X                                     
023000           END-IF                                                         
023100         ELSE                                                             
023200           MOVE 'LEVNR SAKNAS PÅ BAS' TO UT-R18-TEXT                      
023300           PERFORM S11-SKRIV-W5514X                                       
023400         END-IF                                                           
023500       ELSE                                                               
023600         PERFORM S11-SKRIV-W5514X                                         
023700       END-IF                                                             
023800       PERFORM S01-LAES-W55144                                            
023900     END-PERFORM                                                          
024000                                                                          
024100                                                                          
024200     PERFORM Z-FINIT                                                      
024300                                                                          
024400     MOVE ZERO TO RETURN-CODE                                             
024500     GOBACK                                                               
024600     .                                                                    
024700     EJECT                                                                
024800 A-INIT SECTION.                                                          
024900     SKIP2                                                                
025000                                                                          
025100     PERFORM IMS-RESTART                                                  
025200                                                                          
025300     OPEN INPUT W55144                                                    
025400                                                                          
025500     OPEN OUTPUT W5514X                                                   
025600                                                                          
025700     ACCEPT DAGENS-DATUM FROM DATE                                        
025800*    COMPUTE NEXT-AAR = DAGENS-DATUM-AAR + 1                              
025900     DISPLAY 'DAGENS-DATUM ' DAGENS-DATUM ' NÄSTA-ÅR ' NEXT-AAR           
026000     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
026100     .                                                                    
026200     EJECT                                                                
026300 B-FLYTTA-VARDEN SECTION.                                                 
026400     MOVE IN-R18-NEW-TULL      TO WS-NEW-TULLFAKT                         
026500     MOVE WS-NEW-TULLTAL       TO WS-DEC-NEWTULL-TAL                      
026600     MOVE WS-NEW-TULLDEC       TO WS-DEC-NEWTULL-DEC                      
026700                                                                          
026800     MOVE IN-R18-OLD-TULL      TO WS-OLD-TULLFAKT                         
026900     MOVE WS-OLD-TULLTAL       TO WS-DEC-OLDTULL-TAL                      
027000     MOVE WS-OLD-TULLDEC       TO WS-DEC-OLDTULL-DEC                      
027100                                                                          
027200     MOVE IN-R18-LEVNR         TO W-IDLEVNR                               
027300     PERFORM BA-KONTROLLERA-TULLFAKT                                      
027400     .                                                                    
027500     EJECT                                                                
027600 BA-KONTROLLERA-TULLFAKT SECTION.                                         
027700     MOVE NEJ                       TO OK-SW                              
027800     IF WS-DEC-NEWTULL NUMERIC                                            
027900       IF WS-DEC-NEWTULL < 2 AND > 0.99                                   
028000         IF IN-R18-KOD = 2                                                
028100           MOVE JA                  TO OK-SW                              
028200         ELSE                                                             
028300           MOVE 'KOD INTE 2     '   TO UT-R18-TEXT                        
028400           MOVE NEJ                 TO OK-SW                              
028500         END-IF                                                           
028600       ELSE                                                               
028700         MOVE 'TULLFAKT > 2 OR < 1 '  TO UT-R18-TEXT                      
028800         MOVE NEJ                   TO OK-SW                              
028900       END-IF                                                             
029000     ELSE                                                                 
029100       MOVE 'TULLFAKT INTE NUM'     TO UT-R18-TEXT                        
029200       MOVE NEJ                     TO OK-SW                              
029300     END-IF                                                               
029400                                                                          
029500     .                                                                    
029600     EJECT                                                                
029700 C-KOLLA-DATUM SECTION.                                                   
029800     MOVE IN-R18-DATUM   TO INFIL-DATUM                                   
029900**** FÖR ATT KÖRA EN ANDRA GÅNG INNAN ÅRSSKIFTET                          
030000**   IF NEXT-AAR = INFIL-DATUM-AAR                                        
030100**     IF TITULF  = IN-R18-DATUM                                          
030200**       MOVE NEJ        TO OK-SW                                         
030300**     ELSE                                                               
030400*        MOVE JA         TO OK-SW                                         
030500**     END-IF                                                             
030600**   ELSE                                                                 
030700**     MOVE NEJ          TO OK-SW                                         
030800**   END-IF                                                               
030900****                                                                      
031000                                                                          
031100     IF NEXT-AAR = INFIL-DATUM-AAR                                        
031200       IF TULL-TITULF  = IN-R18-DATUM                                     
031300**** OM MAN INTE VILL TILLÅTA FLERA UPPDATERINGAR INNAN                   
031400**** ÅRSSKIFTET SÅ SKALL DET VARA NEJ HÄR                                 
031500         MOVE JA         TO OK-SW                                         
031600       ELSE                                                               
031700         MOVE JA         TO OK-SW                                         
031800       END-IF                                                             
031900     ELSE                                                                 
032000       MOVE NEJ          TO OK-SW                                         
032100     END-IF                                                               
032200     .                                                                    
032300     EJECT                                                                
032400                                                                          
032500 Z-FINIT SECTION.                                                         
032600                                                                          
032700                                                                          
032800     CLOSE W55144                                                         
032900                                                                          
033000           W5514X                                                         
033100     SKIP2                                                                
033200     MOVE 'S' TO POSTSUM-OPKOD                                            
033300     CALL POSTSUM USING POSTSUM-PARM                                      
033400     .                                                                    
033500     EJECT                                                                
033600 S01-LAES-W55144  SECTION.                                                
033700     SKIP2                                                                
033800     READ W55144    INTO IN-R18-AREA                                      
033900     AT END                                                               
034000        SET END-OF-W55144 TO TRUE                                         
034100                                                                          
034200     NOT AT END                                                           
034300        MOVE 'W55144' TO POSTSUM-FDNAMN                                   
034400        MOVE 'W55116D1' TO POSTSUM-DDNAMN2                                
034500        MOVE 'IN-'     TO POSTSUM-TRANSTYP                                
034600        CALL POSTSUM USING POSTSUM-PARM                                   
034700                                                                          
034800     END-READ                                                             
034900     MOVE IN-R18-AREA TO UT-R18-AREA                                      
035000     .                                                                    
035100     EJECT                                                                
035200 S11-SKRIV-W5514X SECTION.                                                
035300     SKIP2                                                                
035400                                                                          
035500     WRITE W5514X00    FROM UT-R18-AREA                                   
035600                                                                          
035700     MOVE 'UT-'     TO POSTSUM-TRANSTYP                                   
035800     MOVE 'W5514X ' TO POSTSUM-FDNAMN                                     
035900     MOVE 'W55116D2' TO POSTSUM-DDNAMN2                                   
036000     CALL POSTSUM USING POSTSUM-PARM                                      
036100     .                                                                    
036200     EJECT                                                                
036300 X-TAG-CHECKPOINT   SECTION.                                              
036400                                                                          
036500* --- VID CHECKPOINTTAGGNING SÅ TAPPAR MAN GN-POSITION I BASEN            
036600* --- SPARA DATABASNYCKLAR OM DET BEHÖVS                                  
036700     PERFORM IMS-CHECKPOINT                                               
036800     MOVE ZERO TO CHKP-ANT                                                
036900* --- LÄS OM DATABAS OM DET BEHÖVS                                        
037000     .                                                                    
037100     EJECT                                                                
037200* --- IMS SEKTIONER ---                                                   
037300                                                                          
037400     EJECT                                                                
037500 IMS-GET-WDF101 SECTION.                                                  
037600                                                                          
037700     STRING 'WDF101  (IDLEVNR  =' W-IDLEVNR-X ')'                         
037800          DELIMITED BY SIZE INTO SSA1                                     
037900     MOVE '  GE' TO GODK-STATUSKODER                                      
038000     CALL CBLTDLI USING GU WDF1-PCB DLI-IO-WDF101 SSA1                    
038100     MOVE WDF1-STATUS-CODE TO STATUS-WS                                   
038200     PERFORM IMS-STATUSKONTROLL                                           
038300     .                                                                    
038400     EJECT                                                                
038500 IMS-GET-WDF102 SECTION.                                                  
038600                                                                          
038700     STRING 'WDF102  (IDLAND   =' W-IDLAND-X ')'                          
038800          DELIMITED BY SIZE INTO SSA1                                     
038900     MOVE '  GE' TO GODK-STATUSKODER                                      
039000     CALL CBLTDLI USING GHNP WDF1-PCB DLI-IO-WDF102 SSA1                  
039100     MOVE WDF1-STATUS-CODE TO STATUS-WS                                   
039200     PERFORM IMS-STATUSKONTROLL                                           
039300     .                                                                    
039400     SKIP3                                                                
039500 IMS-REPL-WDF102 SECTION.                                                 
039600                                                                          
039700     MOVE '  ' TO GODK-STATUSKODER                                        
039800     CALL CBLTDLI USING REPL WDF1-PCB DLI-IO-WDF102                       
039900     MOVE WDF1-STATUS-CODE TO STATUS-WS                                   
040000     PERFORM IMS-STATUSKONTROLL                                           
040100     MOVE 'UPP'     TO POSTSUM-TRANSTYP                                   
040200     MOVE 'W55116 ' TO POSTSUM-FDNAMN                                     
040300     MOVE 'W55116UP' TO POSTSUM-DDNAMN2                                   
040400     CALL POSTSUM USING POSTSUM-PARM                                      
040500     .                                                                    
040600     EJECT                                                                
040700 IMS-RESTART SECTION.                                                     
040800     SKIP2                                                                
040900     MOVE SPACE TO CHKP-MSG-IO-AREA                                       
041000     MOVE '  ' TO GODK-STATUSKODER                                        
041100     CALL CBLTDLI USING XRST MSG-PCB                                      
041200                        CHKP-MSG-IO-AREA-LENGTH CHKP-MSG-IO-AREA          
041300                        CHKP-AREA-LENGTH CHKP-AREA                        
041400     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
041500     PERFORM IMS-STATUSKONTROLL                                           
041600     .                                                                    
041700     SKIP3                                                                
041800 IMS-CHECKPOINT SECTION.                                                  
041900     SKIP2                                                                
042000     MOVE SPACE TO CHKP-MSG-IO-AREA                                       
042100     MOVE '  XD' TO GODK-STATUSKODER                                      
042200     CALL CBLTDLI USING CHKP MSG-PCB                                      
042300                        CHKP-MSG-IO-AREA-LENGTH CHKP-MSG-IO-AREA          
042400                        CHKP-AREA-LENGTH CHKP-AREA                        
042500     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
042600     PERFORM IMS-STATUSKONTROLL                                           
042700                                                                          
042800     IF IMS-EJ-OK                                                         
042900       MOVE 'IMS-KONTROLLREGION EJ TILLGÄNGLIG' TO FELTEXT-STR            
043000       DISPLAY FELTEXT                                                    
043100       CALL FELLOG                                                        
043200     END-IF                                                               
043300     .                                                                    
043400     EJECT                                                                
043500 IMS-STATUSKONTROLL SECTION.                                              
043600     SKIP2                                                                
043700     SET STATUS-IX TO 1                                                   
043800     SEARCH GODK-STATUS                                                   
043900       AT END                                                             
044000         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
044100           DELIMITED BY SIZE INTO FELTEXT                                 
044200         DISPLAY FELTEXT                                                  
044300         CALL FELLOG                                                      
044400       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
044500         CONTINUE                                                         
044600     END-SEARCH                                                           
044700     .                                                                    
