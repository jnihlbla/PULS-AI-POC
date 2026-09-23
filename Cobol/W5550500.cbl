000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W5550500.                                                
000300 AUTHOR.         MARKUS ASPFJÄLL.                                         
000400 DATE-WRITTEN.   98/02/03.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700                                                                          
000800*    FUNKTION:                                                            
000900*        LÄSER INFIL W55507 OCH UPPDATERAR WDL9/WLLOGA BASEN              
001000*                                                                         
001100*        PROGRAMMET UPPDATERAR WLLOGA (WDL9)                              
001200*                                                                         
001300                                                                          
001400     SKIP3                                                                
001500 ENVIRONMENT DIVISION.                                                    
001600     SKIP2                                                                
001700 INPUT-OUTPUT SECTION.                                                    
001800                                                                          
001900 FILE-CONTROL.                                                            
002000     SKIP2                                                                
002100*          --- INFIL-W55507                                               
002200     SELECT W55507                     ASSIGN TO W55505D1.                
002300     EJECT                                                                
002400 DATA DIVISION.                                                           
002500     SKIP3                                                                
002600 FILE SECTION.                                                            
002700     SKIP3                                                                
002800 FD  W55507                                                               
002900     RECORDING       F                                                    
003000     BLOCK CONTAINS  0.                                                   
003100                                                                          
003200*01  -COPY W55507      -L.                                                
003300     EJECT                                                                
003400 WORKING-STORAGE SECTION.                                                 
003500                                                                          
003600*    --CHECKED BY WY2000                                                  
003700     SKIP3                                                                
003800 77  IDPGM                       PIC X(8)    VALUE 'W5550500'.            
003900 01  CHKP-VAR.                                                            
004000     03 CHKP-MSG-IO-AREA-LENGTH  PIC S9(9)   VALUE +32 COMP SYNC.         
004100     03 CHKP-MSG-IO-AREA         PIC X(32)   VALUE SPACE.                 
004200     03 CHKP-AREA-LENGTH         PIC S9(9)   VALUE +32 COMP SYNC.         
004300     03 CHKP-AREA                PIC X(32)   VALUE SPACE.                 
004400     03 CHKP-ANT                 PIC S9(5)   VALUE +0   COMP-3.           
004500     03 CHKP-MAX                 PIC S9(5)   VALUE +3000 COMP-3.          
004600 77  JA                          PIC X       VALUE 'J'.                   
004700 77  NEJ                         PIC X       VALUE 'N'.                   
004800     SKIP2                                                                
004900 01  FELTEXT.                                                             
005000     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
005100     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
005200 01  W-W55507-KVPOST-IN          PIC S9(9)   VALUE ZERO.                  
005300                                                                          
005400 77  W55507-EOF-SW               PIC X       VALUE 'N'.                   
005500     88  END-OF-W55507                       VALUE 'J'.                   
005600     SKIP2                                                                
005700 01  WS-DAGENS-DATUM             PIC 9(8).                                
005800     EJECT                                                                
005900 01  DYNAMISKA-SUBPROGRAM.                                                
006000*                                                                         
006100     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
006200     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
006300     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
006400     EJECT                                                                
006500*    --- PARAMETRAR TILL POSTSUM                                          
006600*                                                                         
006700*01  -COPY W0005   -PRE  POSTSUM-                                         
006800     EJECT                                                                
006900 01  IN-AREA-START               PIC X(24)   VALUE                        
007000                                             'IN-AREA-START'.             
007100*01  AREA -COPY W55507     -PRE IN-                                       
007200*                                                                         
007300     EJECT                                                                
007400*                                                                         
007500 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
007600     SKIP3                                                                
007700                                                                          
007800                                                                          
007900 01  W-WDL901KY-X.                                                        
008000     03  W-IDARTNR               PIC S9(9)        COMP-3.                 
008100     03  W-DAREGDAT              PIC  9(8).                               
008200     03  W-TIKLOCK               PIC S9(9)        COMP-3.                 
008300     03  W-IDSEKVNR              PIC S9(3)        COMP-3.                 
008400                                                                          
008500                                                                          
008600     SKIP2                                                                
008700*    --- STATUS-KOD FRÅN IMS                                              
008800 01  STATUS-WS                   PIC XX.                                  
008900     88  SEGMENT-FINNS                       VALUE '  '.                  
009000     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
009100     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
009200     88  SEGMENT-SLUT                        VALUE 'GB'.                  
009300     88  IMS-EJ-OK                           VALUE 'XD'.                  
009400     SKIP2                                                                
009500 01  GODK-STATUSKODER.                                                    
009600     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
009700     SKIP3                                                                
009800 01  SSA1                        PIC X(64).                               
009900 01  SSA2                        PIC X(64).                               
010000     EJECT                                                                
010100*    --- IMS FUNKTIONSKODER                                               
010200*01  -COPY W0003                                                          
010300     EJECT                                                                
010400*    ---  DLI INPUT-OUTPUT AREA                                           
010500                                                                          
010600 01  FILLER         PIC X(16) VALUE 'DLI-IO-WLLOGA01'.                    
010700 01  DLI-IO-WLLOGA01.                                                     
010800*   03  WLLOGA01 -COPY WDL901                                             
010900                                                                          
011000     EJECT                                                                
011100 LINKAGE SECTION.                                                         
011200                                                                          
011300*01  -COPY W0009   -PRE MSG-                                              
011400                                                                          
011500*01  -COPY W0008  -PRE LOGA-                                              
011600     05  FILLER                  PIC X.                                   
011700     EJECT                                                                
011800 PROCEDURE DIVISION  USING MSG-PCB LOGA-PCB.                              
011900 MAIN SECTION.                                                            
012000     ENTRY 'DLITCBL' USING MSG-PCB LOGA-PCB.                              
012100                                                                          
012200     PERFORM A-INIT                                                       
012300                                                                          
012400     PERFORM S01-LAES-W55507                                              
012500     PERFORM UNTIL END-OF-W55507                                          
012600       IF CHKP-ANT > CHKP-MAX                                             
012700         PERFORM X-TAG-CHECKPOINT                                         
012800       END-IF                                                             
012900                                                                          
013000       PERFORM C-KOLLA-UPPDATERA-BAS                                      
013100                                                                          
013200       PERFORM S01-LAES-W55507                                            
013300       ADD +1 TO CHKP-ANT                                                 
013400     END-PERFORM                                                          
013500                                                                          
013600                                                                          
013700     PERFORM Z-FINIT                                                      
013800                                                                          
013900     MOVE ZERO TO RETURN-CODE                                             
014000     GOBACK                                                               
014100     .                                                                    
014200     EJECT                                                                
014300 A-INIT SECTION.                                                          
014400                                                                          
014500     PERFORM IMS-RESTART                                                  
014600                                                                          
014700     OPEN INPUT W55507                                                    
014800     MOVE FUNCTION CURRENT-DATE(1:8) TO WS-DAGENS-DATUM                   
014900     DISPLAY 'DAGENS-DATUM ' WS-DAGENS-DATUM                              
015000                                                                          
015100     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
015200     .                                                                    
015300     EJECT                                                                
015400 C-KOLLA-UPPDATERA-BAS SECTION.                                           
015500                                                                          
015600     MOVE IN-URV-IDARTNR         TO W-IDARTNR                             
015700     MOVE IN-URV-DAREGDAT-9KOMPL TO W-DAREGDAT                            
015800     MOVE IN-URV-TIKLOCK-9KOMPL  TO W-TIKLOCK                             
015900     MOVE IN-URV-IDSEKVNR        TO W-IDSEKVNR                            
016000                                                                          
016100     PERFORM IMS-GET-LOGA                                                 
016200     IF SEGMENT-FINNS                                                     
016300        IF LOGG-DAREGDAT-LADD < WS-DAGENS-DATUM OR                        
016400          (LOGG-IDPGM NOT = IN-URV-IDPGM)                                 
016500* VILLLKOR 1 ÄR TILL FÖR GAMLA SOL-HÄNDELSER, UPPD ENDAST EN GÅNG         
016600* VILLLKOR 2 ÄR TILL FÖR DAGLIGA SOLSTÄDNINGEN W555D2                     
016700                                                                          
016800          IF LOGG-IDPGM = IN-URV-IDPGM                                    
016900            MOVE WS-DAGENS-DATUM TO IN-URV-DAREGDAT-LADD                  
017000          END-IF                                                          
017100          PERFORM IMS-DLET-LOGA                                           
017200          MOVE IN-AREA           TO WLLOGA01                              
017300          PERFORM IMS-ISRT-LOGA                                           
017400                                                                          
017500        END-IF                                                            
017600     ELSE                                                                 
017700       MOVE WS-DAGENS-DATUM TO IN-URV-DAREGDAT-LADD                       
017800       MOVE IN-AREA TO WLLOGA01                                           
017900       PERFORM IMS-ISRT-LOGA                                              
018000     END-IF                                                               
018100     .                                                                    
018200     EJECT                                                                
018300 Z-FINIT SECTION.                                                         
018400                                                                          
018500     CLOSE W55507                                                         
018600                                                                          
018700     MOVE 'S' TO POSTSUM-OPKOD                                            
018800     CALL POSTSUM USING POSTSUM-PARM                                      
018900     .                                                                    
019000     EJECT                                                                
019100 S01-LAES-W55507  SECTION.                                                
019200                                                                          
019300     READ W55507 INTO IN-AREA                                             
019400     AT END                                                               
019500                                                                          
019600        SET END-OF-W55507 TO TRUE                                         
019700                                                                          
019800     NOT AT END                                                           
019900        MOVE 'W55507' TO POSTSUM-FDNAMN                                   
020000        MOVE 'W55505D1' TO POSTSUM-DDNAMN2                                
020100        MOVE 'INFIL  ' TO POSTSUM-TRANSTYP                                
020200        CALL POSTSUM USING POSTSUM-PARM                                   
020300                                                                          
020400        ADD 1 TO W-W55507-KVPOST-IN                                       
020500     END-READ                                                             
020600     .                                                                    
020700     EJECT                                                                
020800 X-TAG-CHECKPOINT   SECTION.                                              
020900                                                                          
021000     PERFORM IMS-CHECKPOINT                                               
021100     MOVE ZERO TO CHKP-ANT                                                
021200     .                                                                    
021300     EJECT                                                                
021400* --- IMS SEKTIONER ---                                                   
021500                                                                          
021600     EJECT                                                                
021700 IMS-GET-LOGA SECTION.                                                    
021800                                                                          
021900     STRING 'WLLOGA01(WDL901KY =' W-WDL901KY-X ')'                        
022000            DELIMITED BY SIZE INTO SSA1                                   
022100     MOVE '  GE' TO GODK-STATUSKODER                                      
022200     CALL CBLTDLI USING GHU LOGA-PCB DLI-IO-WLLOGA01 SSA1                 
022300     MOVE LOGA-STATUS-CODE TO STATUS-WS                                   
022400     PERFORM IMS-STATUSKONTROLL                                           
022500     .                                                                    
022600     SKIP3                                                                
022700 IMS-ISRT-LOGA SECTION.                                                   
022800                                                                          
022900     MOVE 'WLLOGA01 ' TO SSA1                                             
023000     MOVE '  II' TO GODK-STATUSKODER                                      
023100     CALL CBLTDLI USING ISRT LOGA-PCB DLI-IO-WLLOGA01 SSA1                
023200     MOVE LOGA-STATUS-CODE TO STATUS-WS                                   
023300     PERFORM IMS-STATUSKONTROLL                                           
023400     .                                                                    
023500     EJECT                                                                
023600 IMS-DLET-LOGA SECTION.                                                   
023700                                                                          
023800     MOVE '  ' TO GODK-STATUSKODER                                        
023900     CALL CBLTDLI USING DLET LOGA-PCB DLI-IO-WLLOGA01                     
024000     MOVE LOGA-STATUS-CODE TO STATUS-WS                                   
024100     PERFORM IMS-STATUSKONTROLL                                           
024200     .                                                                    
024300     EJECT                                                                
024400 IMS-RESTART SECTION.                                                     
024500     SKIP2                                                                
024600     MOVE SPACE TO CHKP-MSG-IO-AREA                                       
024700     MOVE '  ' TO GODK-STATUSKODER                                        
024800     CALL CBLTDLI USING XRST MSG-PCB                                      
024900                        CHKP-MSG-IO-AREA-LENGTH CHKP-MSG-IO-AREA          
025000                        CHKP-AREA-LENGTH CHKP-AREA                        
025100     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
025200     PERFORM IMS-STATUSKONTROLL                                           
025300     .                                                                    
025400     SKIP3                                                                
025500 IMS-CHECKPOINT SECTION.                                                  
025600     SKIP2                                                                
025700     MOVE SPACE TO CHKP-MSG-IO-AREA                                       
025800     MOVE '  XD' TO GODK-STATUSKODER                                      
025900     CALL CBLTDLI USING CHKP MSG-PCB                                      
026000                        CHKP-MSG-IO-AREA-LENGTH CHKP-MSG-IO-AREA          
026100                        CHKP-AREA-LENGTH CHKP-AREA                        
026200     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
026300     PERFORM IMS-STATUSKONTROLL                                           
026400                                                                          
026500     IF IMS-EJ-OK                                                         
026600       MOVE 'IMS-KONTROLLREGION EJ TILLGÄNGLIG' TO FELTEXT-STR            
026700       DISPLAY FELTEXT                                                    
026800       CALL FELLOG                                                        
026900     END-IF                                                               
027000     .                                                                    
027100     EJECT                                                                
027200 IMS-STATUSKONTROLL SECTION.                                              
027300     SKIP2                                                                
027400     SET STATUS-IX TO 1                                                   
027500     SEARCH GODK-STATUS                                                   
027600       AT END                                                             
027700         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
027800           DELIMITED BY SIZE INTO FELTEXT                                 
027900         DISPLAY FELTEXT                                                  
028000         CALL FELLOG                                                      
028100       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
028200         CONTINUE                                                         
028300     END-SEARCH                                                           
028400     .                                                                    
