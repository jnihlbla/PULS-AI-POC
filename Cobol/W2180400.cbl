000100 ID DIVISION.                                                             
000200     SKIP2                                                                
000300 PROGRAM-ID.     W2180400.                                                
000400*AUTHOR.         STEFAN KIHLBERG.                                         
000500*DATE-WRITTEN.   92/03/31.                                                
000600                                                                          
000700*    REMARKS.                                                             
000800*                                                                         
000900*    FUNKTION:                                                            
001000*        LÄSER HÄNDELSEBASEN WDR5 SEGMENT 530 HÄNDELSE 2228               
001100*        FÖR ARTIKLAR DÄR "TIDISPIN" SKALL OMRÄKNAS.                      
001200*        PROGRAMMET SKICKAR ARTIKELN VIDARE TILL                          
001300*        SUBPROGRAMMET W218DISP, SOM BERÄKNAR "TIDISPIN"                  
001400*        OCH UPPDATERAR WDK611.                                           
001500*                                                                         
001600*    ABENDKODER:                                                          
001700*        U0016 -  . . . .                                                 
001800*        U1000 -  . . . .                                                 
001900*                                                                         
002000                                                                          
002100     SKIP3                                                                
002200 ENVIRONMENT DIVISION.                                                    
002300     SKIP2                                                                
002400 INPUT-OUTPUT SECTION.                                                    
002500                                                                          
002600 FILE-CONTROL.                                                            
002700     EJECT                                                                
002800 DATA DIVISION.                                                           
002900     SKIP3                                                                
003000 FILE SECTION.                                                            
003100     EJECT                                                                
003200 WORKING-STORAGE SECTION.                                                 
003300     SKIP2                                                                
003301                                                                          
003310*    -- CHECKED BY WY2000                                                 
003400 77  IDPGM                       PIC X(8)    VALUE 'W2180400'.            
003500 01  CHKP-VAR.                                                            
003600 03  CHKP-MSG-IO-AREA-LENGTH     PIC S9(9)   VALUE +32 COMP SYNC.         
003700 03  CHKP-MSG-IO-AREA            PIC X(32)   VALUE SPACE.                 
003800 03  CHKP-AREA-LENGTH            PIC S9(9)   VALUE +32 COMP SYNC.         
003900 03  CHKP-AREA                   PIC X(32)   VALUE SPACE.                 
004000 03  CHKP-ANT                    PIC S9(3)   VALUE +0.                    
004100 03  CHKP-MAX                    PIC S9(3)   VALUE +010.                  
004200                                                                          
004300 77  JA                          PIC X       VALUE 'J'.                   
004400 77  NEJ                         PIC X       VALUE 'N'.                   
004500                                                                          
004600 77  WS-IDARTNR                  PIC S9(9)   VALUE ZERO COMP-3.           
004700                                                                          
004800 77  WS-ART-SAKNAS-ANT           PIC 9(9)    VALUE ZERO.                  
004900 77  WS-UTG-STORRE-ANT           PIC 9(9)    VALUE ZERO.                  
005000 77  WS-UPPDAT-ANT               PIC 9(9)    VALUE ZERO.                  
005100                                                                          
005200 77  2228-SEGMENT-SAKNAS-SW      PIC X       VALUE 'N'.                   
005300     88 2228-SEGMENT-SAKNAS                  VALUE 'J'.                   
005400     SKIP2                                                                
005500 01  FELTEXT.                                                             
005600     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
005700     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
005800     EJECT                                                                
005900 01  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
006000 01  FILLER REDEFINES DAGENS-DATUM.                                       
006100     03  DAGENS-DATUM-AAR        PIC 9(2).                                
006200     03  DAGENS-DATUM-MAANAD     PIC 9(2).                                
006300     03  DAGENS-DATUM-DAG        PIC 9(2).                                
006400     EJECT                                                                
006500 01  DYNAMISKA-SUBPROGRAM.                                                
006600*                                                                         
006700     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
006800     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
006900     03  W218DISP                PIC X(8)    VALUE 'W218DISP'.            
007000     EJECT                                                                
007100*01  -COPY W218DISP                                                       
007200*                                                                         
007300     EJECT                                                                
007400 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
007500     SKIP3                                                                
007600******************************************************************        
007700*         NYCKLAR OCH SÖKFÄLT TILL DLI                                    
007800******************************************************************        
007900                                                                          
008000 01  FILLER                      PIC X(16) VALUE 'DLI-NYCKLAR'.           
008100                                                                          
008200 01  NYCKLAR-TILL-DLI.                                                    
008300*                                H-TYP 2227                               
008400     03  W-2227KEY-X.                                                     
008500       05  W-IDHTYP         PIC X(4)    VALUE '2227'.                     
008600       05  FILLER           PIC X(26)   VALUE LOW-VALUE.                  
008700                                                                          
008800*                                  H-TYP 2228                             
008900     03  W-2228KEY-X.                                                     
009000       05  W-2228-IDARTNR          PIC S9(9)  COMP-3.                     
009200       05  W-LOW-VALUE        PIC X(5) VALUE LOW-VALUE.                   
009300     SKIP2                                                                
009400*    --- STATUS-KOD FRÅN IMS                                              
009500 01  STATUS-WS                   PIC XX.                                  
009600     88  SEGMENT-FINNS                       VALUE '  '.                  
009700     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
009800     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
009900     88  SEGMENT-SLUT                        VALUE 'GB'.                  
010000     88  IMS-EJ-OK                           VALUE 'XD'.                  
010100     SKIP2                                                                
010200 01  GODK-STATUSKODER.                                                    
010300     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
010400     SKIP3                                                                
010500 01  SSA1                        PIC X(64).                               
010600 01  SSA2                        PIC X(64).                               
010700     EJECT                                                                
010800*    --- IMS FUNKTIONSKODER                                               
010900*01  -COPY W0003                                                          
011000     EJECT                                                                
011100*    ---  DLI INPUT-OUTPUT AREA                                           
011200 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA'.         
011300     SKIP3                                                                
011400 01  DLI-IO-AREA.                                                         
011500     03  IO-AREA                 PIC X(150)  VALUE SPACE.                 
011600*    ---  DLI INPUT-OUTPUT AREA                                           
011700 01  FILLER                    PIC X(16)   VALUE 'DLI-IO-AREA-2'.         
011800     SKIP3                                                                
011900 01  DLI-IO-AREA-2.                                                       
012000*    03  -COPY WDGX2228                                                   
012100     EJECT                                                                
012200 LINKAGE SECTION.                                                         
012300                                                                          
012400*01  -COPY W0009   -PRE MSG-                                              
012500     EJECT                                                                
012600*01  -COPY W0008   -PRE XXBW-                                             
012700       05 FILLER             PIC X.                                       
012800     EJECT                                                                
012900**PCB FÖR SUBPROGRAM W218DISP                                             
013000                                                                          
013100 01  ARTC-PCB                 PIC X.                                      
013110 01  ARTS-PCB                 PIC X.                                      
013120 01  OIGA-PCB                 PIC X.                                      
013200 01  INLB-PCB                 PIC X.                                      
013300 01  XXCT-PCB                 PIC X.                                      
013400                                                                          
013500 PROCEDURE DIVISION  USING MSG-PCB                                        
013600                           XXBW-PCB                                       
013601                           ARTC-PCB                                       
013610                           ARTS-PCB                                       
013611                           OIGA-PCB                                       
013612                           INLB-PCB                                       
013613                           XXCT-PCB.                                      
013700     ENTRY 'DLITCBL' USING MSG-PCB                                        
013800                           XXBW-PCB                                       
013801                           ARTC-PCB                                       
013802                           ARTS-PCB                                       
013803                           OIGA-PCB                                       
013804                           INLB-PCB                                       
013805                           XXCT-PCB.                                      
013900                                                                          
014000     PERFORM A-INIT                                                       
014100     PERFORM IMS-GET-2227                                                 
014200     PERFORM IMS-GHNP-2228                                                
014300     IF SEGMENT-FINNS                                                     
014400        PERFORM IMS-DLET-2228                                             
014500        PERFORM UNTIL 2228-SEGMENT-SAKNAS                                 
014600           MOVE 2228-IDARTNR TO WS-IDARTNR                                
014700           PERFORM UNTIL 2228-SEGMENT-SAKNAS OR                           
014800                         2228-IDARTNR NOT = WS-IDARTNR                    
014900              PERFORM IMS-GHNP-2228                                       
015000              IF SEGMENT-FINNS                                            
015100                 PERFORM IMS-DLET-2228                                    
015200              ELSE                                                        
015300                 SET 2228-SEGMENT-SAKNAS TO TRUE                          
015400              END-IF                                                      
015500           END-PERFORM                                                    
015600           MOVE WS-IDARTNR  TO DISP-IDARTNR                               
015700           MOVE +0          TO DISP-KDSVAR                                
015800           MOVE IDPGM       TO DISP-IDPGM                                 
015900           CALL W218DISP USING DISP-W218DISP                              
015910                               ARTC-PCB                                   
015911                               ARTS-PCB                                   
015912                               OIGA-PCB                                   
015920                               INLB-PCB                                   
016000           ADD +1 TO CHKP-ANT                                             
016100           PERFORM Y-RAKNA-UPPDAT                                         
016200           IF CHKP-ANT >= CHKP-MAX                                        
016300              PERFORM X-TAG-CHECKPOINT                                    
016400              PERFORM IMS-GET-2227                                        
016500              MOVE +1 TO CHKP-ANT                                         
016600           END-IF                                                         
016700        END-PERFORM                                                       
016800     END-IF                                                               
016900     DISPLAY 'ARTIKEL-SAKNAS = ' WS-ART-SAKNAS-ANT                        
017000     DISPLAY 'KDERS-UTG > 0  = ' WS-UTG-STORRE-ANT                        
017100     DISPLAY 'UPPDAT         = ' WS-UPPDAT-ANT                            
017200                                                                          
017300     MOVE ZERO TO RETURN-CODE                                             
017400     GOBACK                                                               
017500     .                                                                    
017600                                                                          
017700 A-INIT SECTION.                                                          
017800     SKIP2                                                                
017900                                                                          
018000     PERFORM IMS-RESTART                                                  
018100     MOVE +1 TO CHKP-ANT                                                  
018200                                                                          
018300     .                                                                    
018400                                                                          
018500     EJECT                                                                
018600                                                                          
018700 Y-RAKNA-UPPDAT SECTION.                                                  
018800                                                                          
018900     IF DISP-KDSVAR = '1'                                                 
019000        ADD +1 TO WS-ART-SAKNAS-ANT                                       
019100     END-IF                                                               
019200     IF DISP-KDSVAR = '2'                                                 
019300        ADD 1 TO WS-UTG-STORRE-ANT                                        
019400     END-IF                                                               
019500     IF DISP-KDSVAR = 3                                                   
019600        ADD 1 TO WS-UPPDAT-ANT                                            
019700     END-IF                                                               
019800     .                                                                    
019900     EJECT                                                                
020000                                                                          
020100                                                                          
020200 X-TAG-CHECKPOINT SECTION.                                                
020300                                                                          
020400     PERFORM IMS-CHECKPOINT                                               
020500     .                                                                    
020600     EJECT                                                                
020700* --- IMS SEKTIONER ---                                                   
020800     SKIP3                                                                
020900     EJECT                                                                
021000                                                                          
021100******************************************************************        
021200* IMS SEKTIONER                                                           
021300******************************************************************        
021400                                                                          
021500                                                                          
021600 IMS-GET-2227 SECTION.                                                    
021700                                                                          
021800     STRING 'WLXXBW01(WDGXKEY  =' W-2227KEY-X  ')'                        
021900            DELIMITED BY SIZE INTO SSA1                                   
022000     MOVE '  ' TO GODK-STATUSKODER                                        
022100     CALL CBLTDLI USING GU XXBW-PCB DLI-IO-AREA SSA1                      
022200     MOVE XXBW-STATUS-CODE TO STATUS-WS                                   
022300     PERFORM IMS-STATUSKONTROLL                                           
022400     .                                                                    
022500     EJECT                                                                
022600                                                                          
022700                                                                          
022800 IMS-GHNP-2228 SECTION.                                                   
022900                                                                          
023000     MOVE   'WLXXBW11  '          TO SSA1                                 
023100     MOVE '  GE' TO GODK-STATUSKODER                                      
023200     CALL CBLTDLI USING GHNP XXBW-PCB DLI-IO-AREA-2 SSA1                  
023300     MOVE XXBW-STATUS-CODE TO STATUS-WS                                   
023400     PERFORM IMS-STATUSKONTROLL                                           
023500     .                                                                    
023600     EJECT                                                                
023700                                                                          
023800 IMS-DLET-2228 SECTION.                                                   
023900                                                                          
024000     MOVE '  ' TO GODK-STATUSKODER                                        
024100     CALL CBLTDLI USING DLET XXBW-PCB DLI-IO-AREA-2 SSA1                  
024200     MOVE XXBW-STATUS-CODE TO STATUS-WS                                   
024300     PERFORM IMS-STATUSKONTROLL                                           
024400     .                                                                    
024500 IMS-RESTART SECTION.                                                     
024600     SKIP2                                                                
024700     MOVE SPACE TO CHKP-MSG-IO-AREA                                       
024800     MOVE '  ' TO GODK-STATUSKODER                                        
024900     CALL CBLTDLI USING XRST MSG-PCB                                      
025000                        CHKP-MSG-IO-AREA-LENGTH CHKP-MSG-IO-AREA          
025100                        CHKP-AREA-LENGTH CHKP-AREA                        
025200     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
025300     PERFORM IMS-STATUSKONTROLL                                           
025400     .                                                                    
025500     EJECT                                                                
025600 IMS-CHECKPOINT SECTION.                                                  
025700     SKIP2                                                                
025800     MOVE SPACE TO CHKP-MSG-IO-AREA                                       
025900     MOVE '  XD' TO GODK-STATUSKODER                                      
026000     CALL CBLTDLI USING CHKP MSG-PCB                                      
026100                        CHKP-MSG-IO-AREA-LENGTH CHKP-MSG-IO-AREA          
026200                        CHKP-AREA-LENGTH CHKP-AREA                        
026300     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
026400     PERFORM IMS-STATUSKONTROLL                                           
026500                                                                          
026600     IF IMS-EJ-OK                                                         
026700       MOVE 'IMS-KONTROLLREGION EJ TILLGÄNGLIG' TO FELTEXT-STR            
026800       DISPLAY FELTEXT                                                    
026900       CALL FELLOG                                                        
027000     END-IF                                                               
027100     .                                                                    
027200     EJECT                                                                
027300 IMS-STATUSKONTROLL SECTION.                                              
027400     SKIP2                                                                
027500     SET STATUS-IX TO 1                                                   
027600     SEARCH GODK-STATUS                                                   
027700       AT END                                                             
027800         MOVE 'XXXXXXXXXX' TO FELTEXT-STR                                 
027900         DISPLAY FELTEXT                                                  
028000         CALL FELLOG                                                      
028100       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
028200         CONTINUE                                                         
028300     END-SEARCH                                                           
028400     .                                                                    
