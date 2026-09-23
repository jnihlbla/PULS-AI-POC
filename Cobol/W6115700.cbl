000100************************************                                      
000200 ID DIVISION.                                                             
000300                                                                          
000400 PROGRAM-ID.     W6115700.                                                
000500 AUTHOR.         KENT JEBSEN.                                             
000600 DATE-WRITTEN.   97/05/21.                                                
000700 DATE-COMPILED.                                                           
000800                                                                          
000900                                                                          
001000*    FUNKTION:                                                            
001100*        BYTER FÖR NDC:ERNA UT STANDARDPRIS MOT BESTÄLLNINGSPRIS          
001200*        SOM HÄMTAS FRÅN WDK621 SAMT HÄMTAR VOLYM FRÅN WDK6 FÖR           
001210*        ALLA ARTIKLAR.                                                   
001300*                                                                         
001400*        PROGRAMMET LÄSER      W6INLA (W6D1)                              
001500*        PROGRAMMET LÄSER      WLARTC (WDK6)                              
001600*                                                                         
001700*    ABENDKODER:                                                          
001800*        U0016 -  . . . .                                                 
001900*        U1000 -  . . . .                                                 
002000*                                                                         
002100*    CHANGES:                                                             
002200*        MARCH-2008.  ETRACKER 6442199.                                   
002300*        FILE W61140B EXTENDED WITH CLAG-VLARTNTO.   /C.E.                
002400*                                                                         
002500                                                                          
002600     SKIP3                                                                
002700 ENVIRONMENT DIVISION.                                                    
002800     SKIP2                                                                
002900 INPUT-OUTPUT SECTION.                                                    
003000                                                                          
003100 FILE-CONTROL.                                                            
003200     SKIP2                                                                
003300*          --- SB AV W6G3 (FRÅN W61149)                                   
003400     SELECT W61140                     ASSIGN TO W61157D1.                
003500     SKIP2                                                                
003600*          --- STDPRIS UTBYTT MOT BEST-PRIS FÖR NDC                       
003700     SELECT W61140B                    ASSIGN TO W61157D2.                
003800     EJECT                                                                
003900 DATA DIVISION.                                                           
004000     SKIP3                                                                
004100 FILE SECTION.                                                            
004200     SKIP3                                                                
004300 FD  W61140                                                               
004400     RECORDING       F                                                    
004500     BLOCK CONTAINS  0.                                                   
004600                                                                          
004700*01  -COPY W6114001      -L.                                              
004800     SKIP3                                                                
004900 FD  W61140B                                                              
005000     RECORDING       F                                                    
005100     BLOCK CONTAINS  0.                                                   
005200                                                                          
005300*01  POST -COPY W6114001 -PRE  UT-  -L.                                   
005400     EJECT                                                                
005500 WORKING-STORAGE SECTION.                                                 
005600     SKIP2                                                                
005700                                                                          
005800                                                                          
005900*    -- CHECKED BY WY2000                                                 
006000 77  IDPGM                       PIC X(8)    VALUE 'W6114900'.            
006100 77  JA                          PIC X       VALUE 'J'.                   
006200 77  NEJ                         PIC X       VALUE 'N'.                   
006300     SKIP2                                                                
006400 01  FELTEXT.                                                             
006500     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
006600     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
006700                                                                          
006800 77  W61140-EOF-SW               PIC X       VALUE 'N'.                   
006900     88  END-OF-W61140                       VALUE 'J'.                   
007000     EJECT                                                                
007100 01  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
007200 01  FILLER REDEFINES DAGENS-DATUM.                                       
007300     03  DAGENS-DATUM-AAR        PIC 9(2).                                
007400     03  DAGENS-DATUM-MAANAD     PIC 9(2).                                
007500     03  DAGENS-DATUM-DAG        PIC 9(2).                                
007600     EJECT                                                                
007700*      --- VALID IDDC CODES                                               
007800*                                                                         
007900*01    -COPY WWDC99                                                       
008000       EJECT                                                              
008100 01  DYNAMISKA-SUBPROGRAM.                                                
008200*                                                                         
008300     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
008400     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
008500     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
008600     EJECT                                                                
008700*    --- PARAMETRAR TILL POSTSUM                                          
008800*                                                                         
008900*01  -COPY W0005   -PRE  POSTSUM-                                         
009000     EJECT                                                                
009100 01  IN-AREA-START               PIC X(24)   VALUE                        
009200                                             'IN-AREA-START'.             
009300     SKIP2                                                                
009400                                                                          
009500*01  AREA -COPY W6114001     -PRE IN-                                     
009600     EJECT                                                                
009700 01  UT-AREA-START               PIC X(24)   VALUE                        
009800                                             'UT-AREA-START'.             
009900     SKIP2                                                                
010000                                                                          
010100*01  AREA -COPY W6114001     -PRE UT-                                     
010200*                                                                         
010300     EJECT                                                                
010400 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
010500     SKIP3                                                                
010600 01  NYCKLAR-TILL-DLI.                                                    
010700*--------SEQ NKL TILL INLB                                                
010800     03  W1-IDLOPNRM-X.                                                   
010900         05  W1-IDLOPNRM         PIC S9(9)   VALUE ZERO COMP-3.           
011000     03  W-IDARTNR-X.                                                     
011100         05  W-IDARTNR           PIC S9(9)   VALUE ZERO COMP-3.           
011200     03  W-WDK621KY-X.                                                    
011300         05  W-DAPRLIST-9KOMPL   PIC 9(8)    VALUE ZERO.                  
011400         05  W-IDLEVNR-21        PIC X(5)    VALUE LOW-VALUE.             
           03  W-IDDC-X.                                                        
               05  W-IDDC              PIC X(2)    VALUE SPACE.                 
011500     SKIP2                                                                
011600*    --- STATUS-KOD FRÅN IMS                                              
011700 01  STATUS-WS                   PIC XX.                                  
011800     88  SEGMENT-FINNS                       VALUE '  '.                  
011900     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
012000     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
012100     88  SEGMENT-SLUT                        VALUE 'GB'.                  
012200     88  IMS-EJ-OK                           VALUE 'XD'.                  
012300     SKIP2                                                                
012400 01  GODK-STATUSKODER.                                                    
012500     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
012600     SKIP3                                                                
012700 01  SSA1                        PIC X(64).                               
012800 01  SSA2                        PIC X(64).                               
012900     EJECT                                                                
013000*    --- IMS FUNKTIONSKODER                                               
013100*01  -COPY W0003                                                          
013200     EJECT                                                                
013300*    ---  DLI INPUT-OUTPUT AREA                                           
013400                                                                          
013500 01  DLI-IO-AREA.                                                         
013600     03  IO-AREA                 PIC X(900)  VALUE SPACE.                 
013700     SKIP3                                                                
013800                                                                          
013900     03  W6INLA11 REDEFINES IO-AREA.                                      
014000*        05  -COPY W6D111                                                 
014100     SKIP3                                                                
014200                                                                          
014300     03  W6INLB11 REDEFINES IO-AREA.                                      
014400*        05  -COPY W6D1B1                                                 
014500     SKIP3                                                                
014600                                                                          
014700     03  WLARTC01 REDEFINES IO-AREA.                                      
014800*        05  -COPY WDK601  -PRE ARTC-                                     
014900     EJECT                                                                
015000     03  WLARTC11 REDEFINES IO-AREA.                                      
015100*        05  -COPY WDK611  -PRE ARTC-                                     
015200     EJECT                                                                
015300     03  WLARTC21 REDEFINES IO-AREA.                                      
015400*        05  -COPY WDK621  -PRE ARTC-                                     
015300     03  WDK711   REDEFINES IO-AREA.                                      
015400*        05  -COPY WDK711  -PRE WDK7-                                     
015500     EJECT                                                                
015600 LINKAGE SECTION.                                                         
015700                                                                          
015800*01  -COPY W0008  -PRE INLA-                                              
015900     05  FILLER                  PIC X.                                   
016000     EJECT                                                                
016100                                                                          
016200*01  -COPY W0008  -PRE INLB-                                              
016300     05  FILLER                  PIC X.                                   
016400     EJECT                                                                
016500                                                                          
016600*01  -COPY W0008   -PRE ARTC-                                             
016700     05  FILLER                  PIC X.                                   
016800     EJECT                                                                
016500                                                                          
016600*01  -COPY W0008   -PRE WDK7-                                             
016700     05  FILLER                  PIC X.                                   
016800     EJECT                                                                
016900 PROCEDURE DIVISION  USING INLB-PCB ARTC-PCB WDK7-PCB.                    
017000 MAIN SECTION.                                                            
017100     ENTRY 'DLITCBL' USING INLB-PCB ARTC-PCB WDK7-PCB.                    
017200                                                                          
017300     SKIP2                                                                
017400     PERFORM A-INIT                                                       
017500     PERFORM S01-LAES-W61140                                              
017600     PERFORM UNTIL END-OF-W61140                                          
017700       MOVE IN-AREA  TO UT-AREA                                           
017800       MOVE IN-IDDC  TO WS-IDDC                                           
                              W-IDDC                                            
017801                                                                          
017810       PERFORM B-HAMTA-ARTNR                                              
017820       IF SEGMENT-FINNS                                                   
017821         PERFORM D-HAMTA-VOLYM                                            
               IF NDC-CN                                                        
                 PERFORM E-GET-PRARTSTD                                         
               END-IF                                                           
017830       END-IF                                                             
017840                                                                          
017900       IF NDC-NA                                                          
018300         PERFORM C-HAMTA-BEST-PRIS                                        
018500       END-IF                                                             
018600       PERFORM S11-SKRIV-W61140B                                          
018700       PERFORM S01-LAES-W61140                                            
018800     END-PERFORM                                                          
018900                                                                          
019000     PERFORM Z-FINIT                                                      
019100                                                                          
019200     MOVE ZERO TO RETURN-CODE                                             
019300     GOBACK                                                               
019400     .                                                                    
019500     EJECT                                                                
019600 A-INIT SECTION.                                                          
019700     SKIP2                                                                
019800                                                                          
019900     OPEN INPUT W61140                                                    
020000                                                                          
020100     OPEN OUTPUT W61140B                                                  
020200                                                                          
020300                                                                          
020400     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
020500     .                                                                    
020600     EJECT                                                                
020700 B-HAMTA-ARTNR SECTION.                                                   
020800     SKIP2                                                                
020900                                                                          
021000     MOVE IN-IDLOPNRM TO W1-IDLOPNRM                                      
021100     PERFORM IMS-GU-INLB-D111                                             
021200     .                                                                    
021300     EJECT                                                                
021400 C-HAMTA-BEST-PRIS SECTION.                                               
021500                                                                          
021600     PERFORM IMS-GU-ARTC11                                                
021700     IF SEGMENT-FINNS                                                     
021900       PERFORM IMS-GET-ARTC-PRL                                           
022000       IF SEGMENT-FINNS                                                   
022100         MOVE ARTC-PRL-PRARTBES-PR TO UT-PRARTSTD                         
022200       END-IF                                                             
022300     END-IF                                                               
022400     .                                                                    
022500     EJECT                                                                
022510 D-HAMTA-VOLYM     SECTION.                                               
022520                                                                          
022521     MOVE ART-IDARTNR TO W-IDARTNR                                        
022530     PERFORM IMS-GU-ARTC11                                                
022540     IF SEGMENT-FINNS                                                     
022550       MOVE ARTC-CLAG-VLARTNTO     TO UT-VLARTNTO                         
022591     END-IF                                                               
022592     .                                                                    
022593     EJECT                                                                
022510 E-GET-PRARTSTD    SECTION.                                               
022520                                                                          
022530     PERFORM IMS-GU-WDK7-SLAG                                             
022540     IF SEGMENT-FINNS                                                     
022550       MOVE WDK7-SLAG-PRAVCOST     TO UT-PRARTSTD                         
022591     END-IF                                                               
022592     .                                                                    
022593     EJECT                                                                
022600 Z-FINIT SECTION.                                                         
022700                                                                          
022800                                                                          
022900     CLOSE W61140                                                         
023000                                                                          
023100           W61140B                                                        
023200     SKIP2                                                                
023300     MOVE 'S' TO POSTSUM-OPKOD                                            
023400     CALL POSTSUM USING POSTSUM-PARM                                      
023500     .                                                                    
023600     EJECT                                                                
023700 S01-LAES-W61140  SECTION.                                                
023800     SKIP2                                                                
023900     READ W61140 INTO IN-AREA                                             
024000     AT END                                                               
024100        SET END-OF-W61140 TO TRUE                                         
024200                                                                          
024300     NOT AT END                                                           
024400        MOVE 'W61140'   TO POSTSUM-FDNAMN                                 
024500        MOVE 'W61157D1' TO POSTSUM-DDNAMN2                                
024600        CALL POSTSUM    USING POSTSUM-PARM                                
024700     END-READ                                                             
024800     .                                                                    
024900     EJECT                                                                
025000 S11-SKRIV-W61140B SECTION.                                               
025100     SKIP2                                                                
025200     WRITE UT-POST   FROM UT-AREA                                         
025300                                                                          
025400     MOVE 'W61140B'     TO POSTSUM-FDNAMN                                 
025500     MOVE 'W61157D2'    TO POSTSUM-DDNAMN2                                
025600     CALL POSTSUM       USING POSTSUM-PARM                                
025700     .                                                                    
025800     EJECT                                                                
025900* --- IMS SEKTIONER ---                                                   
026000     SKIP3                                                                
026100     EJECT                                                                
026200 IMS-GU-INLB-D111  SECTION.                                               
026300     STRING 'W6INLA11(W6D1BSEQ =' W1-IDLOPNRM-X ')'                       
026400             DELIMITED BY SIZE INTO SSA1                                  
026500     MOVE '  GE'                 TO GODK-STATUSKODER                      
026600     CALL CBLTDLI USING GU       INLB-PCB                                 
026700                                 DLI-IO-AREA                              
026800                                 SSA1                                     
026900     MOVE INLB-STATUS-CODE       TO STATUS-WS                             
027000     PERFORM IMS-STATUSKONTROLL                                           
027100     .                                                                    
027200     EJECT                                                                
027300     SKIP3                                                                
027400 IMS-GU-ARTC11 SECTION.                                                   
027500     STRING 'WLARTC01(IDARTNR  =' W-IDARTNR-X ')'                         
027600          DELIMITED BY SIZE INTO SSA1                                     
027700     MOVE 'WLARTC11'       TO SSA2                                        
027800     MOVE '  GE' TO GODK-STATUSKODER                                      
027900     CALL CBLTDLI USING GU ARTC-PCB DLI-IO-AREA SSA1 SSA2                 
028000     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
028100     PERFORM IMS-STATUSKONTROLL                                           
028200     .                                                                    
028300     EJECT                                                                
028400 IMS-GET-ARTC-PRL SECTION.                                                
028500                                                                          
028600     STRING 'WLARTC21(WDK621KY>=' W-WDK621KY-X ')'                        
028700          DELIMITED BY SIZE INTO SSA1                                     
028800     MOVE '  GE' TO GODK-STATUSKODER                                      
028900     CALL CBLTDLI USING GNP ARTC-PCB DLI-IO-AREA SSA1                     
029000     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
029100     PERFORM IMS-STATUSKONTROLL                                           
029200     .                                                                    
029300     EJECT                                                                
       IMS-GU-WDK7-SLAG SECTION.                                                
                                                                                
           STRING 'WDK701  (IDARTNR  =' W-IDARTNR-X ')'                         
                DELIMITED BY SIZE INTO SSA1                                     
           STRING 'WDK711  (IDDC     =' W-IDDC-X ')'                            
                DELIMITED BY SIZE INTO SSA2                                     
           MOVE '  GE' TO GODK-STATUSKODER                                      
           CALL CBLTDLI USING GU WDK7-PCB DLI-IO-AREA SSA1 SSA2                 
           MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
           PERFORM IMS-STATUSKONTROLL                                           
           .                                                                    
029300     EJECT                                                                
029400 IMS-STATUSKONTROLL SECTION.                                              
029500     SKIP2                                                                
029600     SET STATUS-IX TO 1                                                   
029700     SEARCH GODK-STATUS                                                   
029800       AT END                                                             
029900         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
030000           DELIMITED BY SIZE INTO FELTEXT                                 
030100         DISPLAY FELTEXT                                                  
030200         CALL FELLOG                                                      
030300       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
030400         CONTINUE                                                         
030500     END-SEARCH                                                           
030600     .                                                                    
