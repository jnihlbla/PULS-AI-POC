000100 ID DIVISION.                                                             
000300 PROGRAM-ID.     W3351100.                                                
000400 AUTHOR.         RONNY STENHOLM.                                          
000500 DATE-WRITTEN.   93/10/14.                                                
000510 DATE-COMPILED.                                                           
000600                                                                          
000900*    FUNKTION:                                                            
001100*        RENSAR PRIS/RABATT REGISTRET FRÅN INAKTUELLA                     
001200*        RABATTSTRUKTURER I NORMALPRISSÄTTNINGEN.                         
001300*                                                                         
001400*        PROGRAMMET UPPDATERAR WLPRIB (WDC2)                              
001500*                                                                         
002000                                                                          
002100     SKIP3                                                                
002200 ENVIRONMENT DIVISION.                                                    
002300     SKIP2                                                                
002400 INPUT-OUTPUT SECTION.                                                    
002500                                                                          
002600 FILE-CONTROL.                                                            
002700     SKIP2                                                                
002800 DATA DIVISION.                                                           
002900     EJECT                                                                
003000 WORKING-STORAGE SECTION.                                                 
003100*    -COPY WY2000W1                                                       
003110                                                                          
003200 77  IDPGM                       PIC X(8)    VALUE 'W3351100'.            
003300 77  JA                          PIC X       VALUE 'J'.                   
003400 77  NEJ                         PIC X       VALUE 'N'.                   
003500 77  IX                          PIC 9(2)    COMP SYNC.                   
003600 77  SPAR-IDMARKBO               PIC X(1)    VALUE SPACE.                 
003700 77  SPAR-DASTADAT               PIC 9(8)    VALUE ZERO.                  
003800 77  SPAR-IDPROMR                PIC X(3)    VALUE SPACE.                 
003900     SKIP2                                                                
004000 01  FELTEXT.                                                             
004100     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
004200     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
004300                                                                          
004400 77  RKOD-ABEND-UTAN-DUMP        PIC S9(4)   COMP VALUE +16.              
004500                                                                          
004600 77  NY-TABELL-SW                PIC X       VALUE 'N'.                   
004700     88  NY-TABELL                           VALUE 'J'.                   
004800     EJECT                                                                
004900 01  CHKP-VAR.                                                            
005000 03  CHKP-MSG-IO-AREA-LENGTH     PIC S9(9)   VALUE +32 COMP SYNC.         
005100 03  CHKP-MSG-IO-AREA            PIC X(32)   VALUE SPACE.                 
005200 03  CHKP-AREA-LENGTH            PIC S9(9)   VALUE +32 COMP SYNC.         
005300 03  CHKP-AREA                   PIC X(32)   VALUE SPACE.                 
005400 03  CHKP-ANT                    PIC S9(3)   VALUE +0.                    
005500 03  CHKP-MAX                    PIC S9(3)   VALUE +100.                  
005600 01  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
005700 01  FILLER REDEFINES DAGENS-DATUM.                                       
005800     03  DAGENS-DATUM-AAR        PIC 9(2).                                
005900     03  DAGENS-DATUM-MAANAD     PIC 9(2).                                
006000     03  DAGENS-DATUM-DAG        PIC 9(2).                                
006100     EJECT                                                                
006200 01  DYNAMISKA-SUBPROGRAM.                                                
006400     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
006500     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
006600     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
006700     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
006800     EJECT                                                                
006900*    --- PARAMETRAR TILL POSTSUM                                          
007000*                                                                         
007100*01  -COPY W0005   -PRE  POSTSUM-                                         
007200     EJECT                                                                
007300 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
007400     SKIP3                                                                
007500 01  NYCKLAR-TILL-DLI.                                                    
007600     03  W-IDPROMR-X.                                                     
007700         05  W-IDPROMR           PIC X(3)    VALUE SPACE.                 
007800     03  W-DASTADAT-X.                                                    
007900         05  W-DASTADAT          PIC 9(8)   VALUE ZERO.                   
008000     SKIP2                                                                
008100*    --- STATUS-KOD FRÅN IMS                                              
008200 01  STATUS-WS                   PIC XX.                                  
008300     88  SEGMENT-FINNS                       VALUE '  '.                  
008400     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
008500     88  SEGMENT-SLUT                        VALUE 'GB'.                  
008600     88  IMS-EJ-OK                           VALUE 'XD'.                  
008700     SKIP2                                                                
008800 01  GODK-STATUSKODER.                                                    
008900     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
009000     SKIP3                                                                
009100 01  SSA1                        PIC X(64).                               
009200 01  SSA2                        PIC X(64).                               
009300     EJECT                                                                
009400*    --- IMS FUNKTIONSKODER                                               
009500*01  -COPY W0003                                                          
009600     EJECT                                                                
009700*    ---  DLI INPUT-OUTPUT AREA                                           
009800 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA'.         
009900                                                                          
010000 01  DLI-IO-PRIB01.                                                       
010400*  03  -COPY WDC201  -PRE PRIB-                                           
010500     EJECT                                                                
010510 01  DLI-IO-PRIB13.                                                       
010700*  03  -COPY WDC213  -PRE NORM-                                           
010800     EJECT                                                                
010900 LINKAGE SECTION.                                                         
011100*01  -COPY W0009   -PRE MSG-                                              
011200     EJECT                                                                
011300*01  -COPY W0008  -PRE PRIB-                                              
011400     05  FILLER                  PIC X.                                   
011500     EJECT                                                                
011600 PROCEDURE DIVISION  USING MSG-PCB  PRIB-PCB.                             
011610 MAIN SECTION.                                                            
011700     ENTRY 'DLITCBL' USING MSG-PCB  PRIB-PCB.                             
011800                                                                          
012000     PERFORM A-INIT                                                       
012100                                                                          
012200     PERFORM B-RENSA-PRIB-NORM                                            
012300                                                                          
012400     MOVE ZERO TO RETURN-CODE                                             
012500     GOBACK                                                               
012600     .                                                                    
012700     EJECT                                                                
012800 A-INIT SECTION.                                                          
013000                                                                          
013100     ACCEPT DAGENS-DATUM FROM DATE                                        
013200     PERFORM IMS-RESTART                                                  
013300                                                                          
013400     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
013500     .                                                                    
013600     EJECT                                                                
013700 B-RENSA-PRIB-NORM SECTION.                                               
013800                                                                          
013900     PERFORM IMS-GN-PRIB-PRIB                                             
014000     PERFORM UNTIL SEGMENT-SAKNAS OR SEGMENT-SLUT                         
014100       IF CHKP-ANT > CHKP-MAX                                             
014200         PERFORM X-TAG-CHECKPOINT                                         
014300       END-IF                                                             
014400       MOVE PRIB-PRO-IDPROMR TO SPAR-IDPROMR                              
014500*                            LÄS NORMSEGMENT SPARA DATUM(NYCKELN)         
014600       PERFORM IMS-GN-PRIB-NORM                                           
014700       IF SEGMENT-FINNS                                                   
014800         MOVE NORM-RAB-DASTADAT TO SPAR-DASTADAT                          
014900*                                          LÄS NÄSTA NORM SEGMENT         
015000         PERFORM IMS-GN-PRIB-NORM                                         
015100         IF SEGMENT-FINNS                                                 
015200*                              OM "NÄSTA" NORMSEG =< DAGENS DATUM         
015210           MOVE NORM-RAB-DASTADAT(3:6) TO TMP1-YYMMDD                     
015220           MOVE DAGENS-DATUM       TO TMP2-YYMMDD                         
015230           PERFORM WY2000P1                                               
015240                                                                          
015300           IF TMP1-YYMMDD <= TMP2-YYMMDD                                  
015400             PERFORM BA-TAG-BORT-GAMMAL-RABATT                            
015500             ADD +1 TO CHKP-ANT                                           
015600           END-IF                                                         
015700         END-IF                                                           
015800       END-IF                                                             
015900       PERFORM IMS-GN-PRIB-PRIB                                           
016000     END-PERFORM                                                          
016100     .                                                                    
016200     EJECT                                                                
016300 BA-TAG-BORT-GAMMAL-RABATT SECTION.                                       
016400                                                                          
016500*                                TAG BORT DET FÖRSTA NORMSEGMENTET        
016600*                                      MED DEN SPARADE NYCKELN            
016700     MOVE SPAR-IDPROMR    TO W-IDPROMR-X                                  
016800     MOVE SPAR-DASTADAT TO W-DASTADAT                                     
016900     PERFORM IMS-GHU-NORM                                                 
017000     IF SEGMENT-FINNS                                                     
017100       PERFORM IMS-DLET-PRIB                                              
017400     END-IF                                                               
017500     .                                                                    
017600     EJECT                                                                
017620 X-TAG-CHECKPOINT   SECTION.                                              
017630                                                                          
017660     PERFORM IMS-CHECKPOINT                                               
017670     MOVE ZERO TO CHKP-ANT                                                
017690     .                                                                    
017691     EJECT                                                                
017700* --- IMS SEKTIONER ---                                                   
017900                                                                          
018000 IMS-GN-PRIB-PRIB SECTION.                                                
018100     MOVE 'WLPRIB01 ' TO SSA1                                             
018200     MOVE '  GEGB' TO GODK-STATUSKODER                                    
018300     CALL CBLTDLI USING GN PRIB-PCB DLI-IO-PRIB01 SSA1                    
018400     MOVE PRIB-STATUS-CODE TO STATUS-WS                                   
018500     PERFORM IMS-STATUSKONTROLL                                           
018600     .                                                                    
018700     SKIP3                                                                
018800 IMS-GHU-NORM SECTION.                                                    
018900     STRING 'WLPRIB01(IDPROMR  =' W-IDPROMR-X ')'                         
019000          DELIMITED BY SIZE INTO SSA1                                     
019100     STRING 'WLPRIB13(DASTADAT =' W-DASTADAT-X ')'                        
019200          DELIMITED BY SIZE INTO SSA2                                     
019300     MOVE '  GE' TO GODK-STATUSKODER                                      
019400     CALL CBLTDLI USING GHU PRIB-PCB DLI-IO-PRIB13 SSA1 SSA2              
019500     MOVE PRIB-STATUS-CODE TO STATUS-WS                                   
019600     PERFORM IMS-STATUSKONTROLL                                           
019700     .                                                                    
019800     SKIP3                                                                
019900 IMS-GN-PRIB-NORM SECTION.                                                
020000     MOVE 'WLPRIB13 ' TO SSA1                                             
020100     MOVE '  GEGB' TO GODK-STATUSKODER                                    
020200     CALL CBLTDLI USING GHNP PRIB-PCB DLI-IO-PRIB13 SSA1                  
020300     MOVE PRIB-STATUS-CODE TO STATUS-WS                                   
020400     PERFORM IMS-STATUSKONTROLL                                           
020500     .                                                                    
020600     SKIP3                                                                
020700 IMS-DLET-PRIB SECTION.                                                   
020800                                                                          
020900     MOVE '  ' TO GODK-STATUSKODER                                        
021000     CALL CBLTDLI USING DLET PRIB-PCB DLI-IO-PRIB13                       
021100     MOVE PRIB-STATUS-CODE TO STATUS-WS                                   
021200     PERFORM IMS-STATUSKONTROLL                                           
021300     .                                                                    
021400     EJECT                                                                
022800 IMS-RESTART SECTION.                                                     
022900                                                                          
023000     MOVE SPACE TO CHKP-MSG-IO-AREA                                       
023100     MOVE '  ' TO GODK-STATUSKODER                                        
023200     CALL CBLTDLI USING XRST MSG-PCB                                      
023300                        CHKP-MSG-IO-AREA-LENGTH CHKP-MSG-IO-AREA          
023400                        CHKP-AREA-LENGTH CHKP-AREA                        
023500     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
023600     PERFORM IMS-STATUSKONTROLL                                           
023700     .                                                                    
023800     SKIP3                                                                
023900 IMS-CHECKPOINT SECTION.                                                  
024000                                                                          
024100     MOVE SPACE TO CHKP-MSG-IO-AREA                                       
024200     MOVE '  XD' TO GODK-STATUSKODER                                      
024300     CALL CBLTDLI USING CHKP MSG-PCB                                      
024400                        CHKP-MSG-IO-AREA-LENGTH CHKP-MSG-IO-AREA          
024500                        CHKP-AREA-LENGTH CHKP-AREA                        
024600     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
024700     PERFORM IMS-STATUSKONTROLL                                           
024800                                                                          
024900     IF IMS-EJ-OK                                                         
025000       MOVE 'IMS-KONTROLLREGION EJ TILLGÄNGLIG' TO FELTEXT-STR            
025100       DISPLAY FELTEXT                                                    
025200       CALL FELLOG                                                        
025300     END-IF                                                               
025400     .                                                                    
025500     EJECT                                                                
025700 IMS-STATUSKONTROLL SECTION.                                              
025800                                                                          
025900     SET STATUS-IX TO 1                                                   
026000     SEARCH GODK-STATUS                                                   
026100       AT END                                                             
026200         MOVE 'XXXXXXXXXX' TO FELTEXT-STR                                 
026300         DISPLAY FELTEXT                                                  
026400         CALL FELLOG                                                      
026500       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
026600         CONTINUE                                                         
026700     END-SEARCH                                                           
026800     .                                                                    
026900*    -COPY WY2000P1                                                       
027000                                                                          
