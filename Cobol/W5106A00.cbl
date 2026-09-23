000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W5106A00.                                                
000300 AUTHOR.         LENA SKOGLUND GUIDE KONSULT AB                           
000400 DATE-WRITTEN.   2003/06/02.                                              
000500 DATE-COMPILED.                                                           
000600                                                                          
000700                                                                          
000800*    FUNKTION:                                                            
000900*        PROGRAMMET ÄR EN BMP SOM TAR EMOT TRANSAR FRÅN BILLIT,           
001000*        VIA WZ01-MODULEN. SKAPAR EN UTFIL MED DATA SOM SKALL             
001100*        VIDARE TILL PGM W??????? FÖR BEARBETNING.                        
001200*                                                                         
001300*    INDATA.                                                              
001400*        TRANSAKTION: W5106A                                              
001500*        MID:         WF2106 (VIA WZ01)                                   
001600*                                                                         
001700                                                                          
001800     SKIP3                                                                
001900 ENVIRONMENT DIVISION.                                                    
002000     SKIP2                                                                
002100 INPUT-OUTPUT SECTION.                                                    
002200                                                                          
002300 FILE-CONTROL.                                                            
002400     SKIP2                                                                
002500*          --- RADER FRÅN BILL-IT                                         
002600     SELECT W5106A                     ASSIGN TO W5106AD1.                
002700     EJECT                                                                
002800 DATA DIVISION.                                                           
002900     SKIP3                                                                
003000 FILE SECTION.                                                            
003100     SKIP3                                                                
003200 FD  W5106A                                                               
003300     RECORDING       F                                                    
003400     BLOCK CONTAINS  0.                                                   
003500                                                                          
003600 01  W5106A-POST.                                                         
003700*    03   -COPY WDR901    -L.                                             
003800     03 FILLER                   PIC X(6).                                
003900     EJECT                                                                
004000 WORKING-STORAGE SECTION.                                                 
004100                                                                          
004200 77  IDPGM                       PIC X(8)    VALUE 'W5106A00'.            
004300 77  JA                          PIC X       VALUE 'J'.                   
004400 77  NEJ                         PIC X       VALUE 'N'.                   
004500 77  W-ANT                       PIC S9(3)   VALUE ZERO COMP-3.           
004600 77  WS-RETRY                    PIC S9(3)   VALUE ZERO COMP-3.           
004700                                                                          
004800 01  SPAR-IDSYSMOT               PIC X(6)    VALUE SPACE.                 
004900 01  KDRC-DISPLAY                PIC Z(5).                                
005000                                                                          
005100 01  FELTEXT.                                                             
005200     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
005300     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
005400                                                                          
005500 01  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
005600 01  FILLER REDEFINES DAGENS-DATUM.                                       
005700     03  DAGENS-DATUM-AAR        PIC 9(2).                                
005800     03  DAGENS-DATUM-MAANAD     PIC 9(2).                                
005900     03  DAGENS-DATUM-DAG        PIC 9(2).                                
006000                                                                          
006100 01 WS-DATUM-8                   PIC 9(8)    VALUE ZERO.                  
006200 01  FILLER REDEFINES WS-DATUM-8.                                         
006300     03  WS-SEKEL                PIC 9(2).                                
006400     03  WS-TIAAMMDD             PIC 9(6).                                
006500                                                                          
006600 01  DYNAMISKA-SUBPROGRAM.                                                
006700     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
006800     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
006900     03  WZ01RECV                PIC X(8)    VALUE 'WZ01RECV'.            
007000     03  W009WAIT                PIC X(8)    VALUE 'W009WAIT'.            
007100                                                                          
007200*    --- PARAMETERS TO W009WAIT                                           
007300 77  WAIT-TIME                   PIC S9(9)   COMP VALUE +0.               
007400                                                                          
007500 01  FILLER                      PIC X(16)   VALUE 'WWIDFTG '.            
007600*01  -COPY WWIDFTG                                                        
007700                                                                          
007800*    --- AREOR FÖR KOMMUNIKATION                                          
007900 01  FILLER                      PIC X(16)   VALUE 'RECEIVE-AREA'.        
008000*01  -COPY WZ01RECV                                                       
008100                                                                          
008200 01  FILLER                      PIC X(16)   VALUE 'IN-DATA*****'.        
008300 01  IN-DATA.                                                             
008400*    03  -COPY WZ01REQU                                                   
008500*    03  -COPY WF2106     -PRE IN-                                        
008600*      05  -COPY WDR901   -PRE IN- -RED   IN-FIL-WF2106-DATA              
008700*       09 -COPY W510EKHA -PRE IN- -RED   IN-FIL-WDR901-DATA              
008800                                                                          
008900 01  UT-AREA-START               PIC X(24)   VALUE                        
009000                                                'UT-AREA-START**'.        
009100*01  AREA -COPY WDR901     -PRE UT-                                       
009200*    05   -COPY W510EKHA   -PRE UT- -RED UT-FIL-WDR901-DATA               
009300     05   UT-EKH-IDSYSMOT        PIC X(6).                                
009400     EJECT                                                                
009500*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
009600 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
009700     SKIP3                                                                
009800 01  NYCKLAR-TILL-DLI.                                                    
009900     03  W-WDH501KY-X.                                                    
010000         05  W-IDFTG             PIC 9(2)        VALUE ZERO.              
010100         05  W-KDEKHHT           PIC X(3)        VALUE SPACE.             
010200     03  W-KDEKSHT-X.                                                     
010300         05  W-KDEKSHT           PIC X(3)        VALUE SPACE.             
010400     03  W-KDEKNIVA-X.                                                    
010500         05  W-KDEKNIVA          PIC X(5)        VALUE SPACE.             
010600     SKIP2                                                                
010700*    --- STATUS-KOD FRÅN IMS                                              
010800 01  STATUS-WS                   PIC XX.                                  
010900     88  SEGMENT-FINNS                       VALUE '  '.                  
011000     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
011100     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
011200     SKIP2                                                                
011300 01  GODK-STATUSKODER.                                                    
011400     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
011500     SKIP3                                                                
011600 01  SSA1                        PIC X(64).                               
011700 01  SSA2                        PIC X(64).                               
011800 01  SSA3                        PIC X(64).                               
011900     EJECT                                                                
012000*    --- IMS FUNKTIONSKODER                                               
012100*01  -COPY W0003                                                          
012200     EJECT                                                                
012300*    ---  DLI INPUT-OUTPUT AREA                                           
012400 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDH501'.                      
012500 01  DLI-IO-WDH501.                                                       
012600*    03  -COPY WDH501                                                     
012700     EJECT                                                                
012800 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDH511'.                      
012900 01  DLI-IO-WDH511.                                                       
013000*    03  -COPY WDH511                                                     
013100     EJECT                                                                
013200 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDH521'.                      
013300 01  DLI-IO-WDH521.                                                       
013400*    03  -COPY WDH521                                                     
013500     EJECT                                                                
013600 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDH531'.                      
013700 01  DLI-IO-WDH531.                                                       
013800*    03  -COPY WDH531                                                     
013900     EJECT                                                                
014000 LINKAGE SECTION.                                                         
014100*01  -COPY W0009  -PRE MSG-                                               
014200     SKIP2                                                                
014300*01  -COPY W0008  -PRE WDH5-                                              
014400     05  FILLER                  PIC X.                                   
014500                                                                          
014600 PROCEDURE DIVISION  USING MSG-PCB WDH5-PCB.                              
014700                                                                          
014800 MAIN SECTION.                                                            
014900     ENTRY 'DLITCBL' USING MSG-PCB WDH5-PCB.                              
015000                                                                          
015100     PERFORM A-INIT                                                       
015200     PERFORM S02-RECV-MESSAGE                                             
015300     PERFORM UNTIL RECV-KDRC > ZERO                                       
015400       PERFORM B-MATCHA-POST                                              
015500       PERFORM S02-RECV-MESSAGE                                           
015600     END-PERFORM                                                          
015700                                                                          
015800     PERFORM Z-FINIT                                                      
015900                                                                          
016000     MOVE ZERO                   TO RETURN-CODE                           
016100     GOBACK                                                               
016200     .                                                                    
016300     EJECT                                                                
016400 A-INIT SECTION.                                                          
016500                                                                          
016600     PERFORM S01-RECV-OPEN                                                
016700     OPEN OUTPUT W5106A                                                   
016800     .                                                                    
016900     EJECT                                                                
017000 B-MATCHA-POST SECTION.                                                   
017100                                                                          
017200     MOVE SPACE                  TO SPAR-IDSYSMOT                         
017300     PERFORM BA-FLYTTA-NYCKEL                                             
017400     PERFORM IMS-GET-WDH5-ALL                                             
017500     IF SEGMENT-FINNS                                                     
017600       PERFORM IMS-GNP-WDH531                                             
017700       PERFORM UNTIL SEGMENT-SAKNAS                                       
017800         IF  SYST-IDSYSMOT NOT = SPACE                                    
017900         AND SYST-IDSYSMOT NOT = SPAR-IDSYSMOT                            
018000********** SKAPA EN POST FÖR VARJE MOTTAGARE                              
018100           MOVE SYST-IDSYSMOT    TO SPAR-IDSYSMOT                         
018200           PERFORM BB-SKAPA-POST                                          
018300         END-IF                                                           
018400         PERFORM IMS-GNP-WDH531                                           
018500       END-PERFORM                                                        
018600     END-IF                                                               
018700     .                                                                    
018800     EJECT                                                                
018900 BA-FLYTTA-NYCKEL SECTION.                                                
019000                                                                          
019100     MOVE WC-IDFTG-PV            TO W-IDFTG                               
019200     MOVE IN-EKH-KDEKHHT         TO W-KDEKHHT                             
019300     MOVE IN-EKH-KDEKSHT         TO W-KDEKSHT                             
019400     MOVE IN-EKH-KDEKNIVA        TO W-KDEKNIVA                            
019500     .                                                                    
019600     EJECT                                                                
019700 BB-SKAPA-POST SECTION.                                                   
019800                                                                          
019900     MOVE IN-FIL-WDR901          TO UT-FIL-WDR901                         
020000     MOVE SPAR-IDSYSMOT          TO UT-EKH-IDSYSMOT                       
020100     PERFORM S11-SKRIV-W5106A                                             
020200     .                                                                    
020300     EJECT                                                                
020400 Z-FINIT SECTION.                                                         
020500     CLOSE W5106A                                                         
020600     PERFORM S03-RECV-CLOSE                                               
020700     .                                                                    
020800     EJECT                                                                
020900 S01-RECV-OPEN SECTION.                                                   
021000     MOVE 'OPEN'                 TO RECV-KDFUNC                           
021100     MOVE 'CARPARTS.PULS.RECGL'                                           
021200                                 TO RECV-ADDISPABS                        
021300                                                                          
021400     CALL WZ01RECV USING            RECV-CONTROL-AREA                     
021500                                    RECV-OPEN-AREA                        
021600                                                                          
021700*    Sometimes, it happens that this routine is triggered before          
021800*    the input message is complete and released by IMS. So we do          
021900*    an incremental retry of upto 3 times before abending the             
022000*    program with no message.                                             
022100                                                                          
022200     IF RECV-KDRC = 20                                                    
022300       PERFORM                                                            
022400       VARYING WS-RETRY FROM 1 BY 1                                       
022500         UNTIL WS-RETRY > 3                                               
022600            OR RECV-KDRC NOT = 20                                         
022700         COMPUTE WAIT-TIME = WS-RETRY * 100                               
022800         CALL W009WAIT        USING WAIT-TIME                             
022900         MOVE 'OPEN'             TO RECV-KDFUNC                           
023000         MOVE 'CARPARTS.PULS.RECGL'                                       
023100                                 TO RECV-ADDISPABS                        
023200         CALL WZ01RECV USING        RECV-CONTROL-AREA                     
023300                                    RECV-OPEN-AREA                        
023400       END-PERFORM                                                        
023500     END-IF                                                               
023600                                                                          
023700     IF RECV-KDRC > 0                                                     
023800       MOVE RECV-KDRC            TO KDRC-DISPLAY                          
023900       STRING 'WZ01RECV OPEN ERROR RC= ' KDRC-DISPLAY                     
024000       DELIMITED BY SIZE INTO FELTEXT-STR                                 
024100       DISPLAY FELTEXT                                                    
024200       CALL FELLOG                                                        
024300     END-IF                                                               
024400     .                                                                    
024500 S02-RECV-MESSAGE SECTION.                                                
024600                                                                          
024700     MOVE 'GET'                  TO RECV-KDFUNC                           
024800     MOVE LENGTH OF IN-DATA      TO RECV-KVDLEN                           
024900     CALL WZ01RECV            USING RECV-CONTROL-AREA                     
025000                                    RECV-KVDLEN                           
025100                                    IN-DATA                               
025200     IF RECV-KDRC > 1                                                     
025300       MOVE RECV-KDRC            TO KDRC-DISPLAY                          
025400       STRING 'WZ01RECV GET  ERROR RC= ' KDRC-DISPLAY                     
025500       DELIMITED BY SIZE       INTO FELTEXT-STR                           
025600       DISPLAY FELTEXT                                                    
025700       CALL FELLOG                                                        
025800     END-IF                                                               
025900     .                                                                    
026000 S03-RECV-CLOSE SECTION.                                                  
026100                                                                          
026200     MOVE 'CLOSE'                TO RECV-KDFUNC                           
026300     CALL WZ01RECV            USING RECV-CONTROL-AREA                     
026400                                                                          
026500     IF RECV-KDRC > 0                                                     
026600       MOVE RECV-KDRC            TO KDRC-DISPLAY                          
026700       STRING 'WZ01RECV CLOSE ERROR RC= ' KDRC-DISPLAY                    
026800       DELIMITED BY SIZE       INTO FELTEXT-STR                           
026900       DISPLAY FELTEXT                                                    
027000       CALL FELLOG                                                        
027100     END-IF                                                               
027200     .                                                                    
027300 S11-SKRIV-W5106A SECTION.                                                
027400     SKIP2                                                                
027500     WRITE W5106A-POST         FROM UT-AREA                               
027600     .                                                                    
027700     EJECT                                                                
027800* --- IMS SEKTIONER ---                                                   
027900 IMS-GET-WDH5-ALL SECTION.                                                
028000                                                                          
028100     STRING 'WDH501  (WDH501KY =' W-WDH501KY-X ')'                        
028200          DELIMITED BY SIZE INTO SSA1                                     
028300     STRING 'WDH511  (KDEKSHT  =' W-KDEKSHT-X ')'                         
028400          DELIMITED BY SIZE INTO SSA2                                     
028500     STRING 'WDH521  (KDEKNIVA =' W-KDEKNIVA-X ')'                        
028600          DELIMITED BY SIZE INTO SSA3                                     
028700     MOVE '  GE'                 TO GODK-STATUSKODER                      
028800     CALL CBLTDLI USING GU WDH5-PCB DLI-IO-WDH521 SSA1 SSA2 SSA3          
028900     MOVE WDH5-STATUS-CODE       TO STATUS-WS                             
029000     PERFORM IMS-STATUSKONTROLL                                           
029100     .                                                                    
029200     EJECT                                                                
029300 IMS-GNP-WDH531   SECTION.                                                
029400                                                                          
029500     STRING 'WDH531   '                                                   
029600          DELIMITED BY SIZE INTO SSA1                                     
029700     MOVE '  GE'                 TO GODK-STATUSKODER                      
029800     CALL CBLTDLI USING GNP WDH5-PCB DLI-IO-WDH531 SSA1                   
029900     MOVE WDH5-STATUS-CODE       TO STATUS-WS                             
030000     PERFORM IMS-STATUSKONTROLL                                           
030100     .                                                                    
030200     EJECT                                                                
030300                                                                          
030400 IMS-STATUSKONTROLL SECTION.                                              
030500                                                                          
030600     SET STATUS-IX TO 1                                                   
030700     SEARCH GODK-STATUS                                                   
030800       AT END                                                             
030900         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
031000           DELIMITED BY SIZE INTO FELTEXT                                 
031100         DISPLAY FELTEXT                                                  
031200         CALL FELLOG                                                      
031300       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
031400         CONTINUE                                                         
031500     END-SEARCH                                                           
031600     .                                                                    
