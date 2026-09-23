000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W6141400.                                                
000300 AUTHOR.         BODIL LINDAHL.                                           
000400 DATE-WRITTEN.   01/02/22.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700                                                                          
000800*    FUNKTION:                                                            
000900*        LADDAR WDGX2404 MED ARTIKEL-DATA                                 
001000*        KVALITETS SPÄRRADE ARTIKLAR.                                     
001100*                                                                         
001200*        PROGRAMMET UPPDATERAR WDR5                                       
001300*                                                                         
001400                                                                          
001500     SKIP3                                                                
001600 ENVIRONMENT DIVISION.                                                    
001700     SKIP2                                                                
001800 INPUT-OUTPUT SECTION.                                                    
001900                                                                          
002000 FILE-CONTROL.                                                            
002100     SKIP2                                                                
002200*          --- ARTIKELDATA                                                
002300     SELECT W61410                     ASSIGN TO W61414D1.                
002400     EJECT                                                                
002500 DATA DIVISION.                                                           
002600     SKIP3                                                                
002700 FILE SECTION.                                                            
002800     SKIP3                                                                
002900 FD  W61410                                                               
003000     RECORDING       F                                                    
003100     BLOCK CONTAINS  0.                                                   
003200                                                                          
003300*01  -COPY W61410      -L.                                                
003400     EJECT                                                                
003500 WORKING-STORAGE SECTION.                                                 
003600                                                                          
003700                                                                          
003800*    -- CHECKED BY WY2000                                                 
003900 77  IDPGM                       PIC X(8)    VALUE 'W6141400'.            
004000 01  CHKP-VAR.                                                            
004100     03 CHKP-MSG-IO-AREA-LENGTH  PIC S9(9)   VALUE +32 COMP SYNC.         
004200     03 CHKP-MSG-IO-AREA         PIC X(32)   VALUE SPACE.                 
004300     03 CHKP-AREA-LENGTH         PIC S9(9)   VALUE +32 COMP SYNC.         
004400     03 CHKP-AREA                PIC X(32)   VALUE SPACE.                 
004500     03 CHKP-ANT                 PIC S9(3)   VALUE +0   COMP-3.           
004600     03 CHKP-MAX                 PIC S9(3)   VALUE +500 COMP-3.           
004700 77  JA                          PIC X       VALUE 'J'.                   
004800 77  NEJ                         PIC X       VALUE 'N'.                   
004900                                                                          
005000 01  FELTEXT.                                                             
005100     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
005200     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
005300                                                                          
005400 77  W61410-EOF-SW               PIC X       VALUE 'N'.                   
005500     88  END-OF-W61410                       VALUE 'J'.                   
005600                                                                          
005700 01  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
005800 01  FILLER REDEFINES DAGENS-DATUM.                                       
005900     03  DAGENS-DATUM-AAR        PIC 9(2).                                
006000     03  DAGENS-DATUM-MAANAD     PIC 9(2).                                
006100     03  DAGENS-DATUM-DAG        PIC 9(2).                                
006200     EJECT                                                                
006300 01  DYNAMISKA-SUBPROGRAM.                                                
006400*                                                                         
006500     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
006600     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
006700     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
006800     EJECT                                                                
006900*    --- PARAMETRAR TILL POSTSUM                                          
007000*01  -COPY W0005   -PRE  POSTSUM-                                         
007100     EJECT                                                                
007200 01  IN-AREA-START               PIC X(24)   VALUE                        
007300                                             'IN-AREA-START'.             
007400*01  AREA -COPY W61410     -PRE IN-                                       
007500*                                                                         
007600     EJECT                                                                
007700 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
007800     SKIP3                                                                
007900 01  NYCKLAR-TILL-DLI.                                                    
008000     03  W-2403KEY-X.                                                     
008100         05  W-2403-IDHTYP      PIC X(4)     VALUE '2403'.                
008200         05  FILLER             PIC X(26)    VALUE LOW-VALUE.             
008300     03  W-2404KEY-X.                                                     
008400         05  W-IDARTNR          PIC S9(9)    VALUE ZERO COMP-3.           
008500         05  W-IDDC             PIC X(2)     VALUE LOW-VALUE.             
008600         05  W-IDFKNGRP         PIC S9(5)    VALUE ZERO.                  
008700     SKIP2                                                                
008800*    --- STATUS-KOD FRÅN IMS                                              
008900 01  STATUS-WS                   PIC XX.                                  
009000     88  SEGMENT-FINNS                       VALUE '  '.                  
009100     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
009200     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
009300     88  SEGMENT-SLUT                        VALUE 'GB'.                  
009400     88  IMS-EJ-OK                           VALUE 'XD'.                  
009500     SKIP2                                                                
009600 01  GODK-STATUSKODER.                                                    
009700     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
009800     SKIP3                                                                
009900 01  SSA1                        PIC X(64).                               
010000 01  SSA2                        PIC X(64).                               
010100     EJECT                                                                
010200*    --- IMS FUNKTIONSKODER                                               
010300*01  -COPY W0003                                                          
010400     EJECT                                                                
010500*    ---  DLI INPUT-OUTPUT AREA                                           
010600                                                                          
010700 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDR501'.                      
010800 01  DLI-IO-WDR501.                                                       
010900*    03  -COPY WDGX01  -PRE 2403-                                         
011000     EJECT                                                                
011100 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDGX2404'.                    
011200 01  DLI-IO-WDGX2404.                                                     
011300*    03  -COPY WDGX2404                                                   
011400                                                                          
011500     EJECT                                                                
011600 LINKAGE SECTION.                                                         
011700                                                                          
011800*01  -COPY W0009   -PRE MSG-                                              
011900     EJECT                                                                
012000*01  -COPY W0008  -PRE 2403-                                              
012100     05  FILLER                  PIC X.                                   
012200     EJECT                                                                
012300 PROCEDURE DIVISION  USING MSG-PCB 2403-PCB.                              
012400 MAIN SECTION.                                                            
012500     ENTRY 'DLITCBL' USING MSG-PCB 2403-PCB.                              
012600                                                                          
012700     PERFORM A-INIT                                                       
012800     PERFORM B-BORT-GAMMAL-INFO                                           
012900     MOVE '2403'    TO 2403-IDHTYP                                        
013000     MOVE LOW-VALUE TO 2403-NYCKEL-VALFRI                                 
013100     PERFORM IMS-ISRT-2403                                                
013200                                                                          
013300     PERFORM S01-LAES-W61410                                              
013400     PERFORM UNTIL END-OF-W61410                                          
013500       IF CHKP-ANT > CHKP-MAX                                             
013600         PERFORM X-TAG-CHECKPOINT                                         
013700       END-IF                                                             
013800       PERFORM C-ISRT-WDGX2404                                            
013900       PERFORM S01-LAES-W61410                                            
014000     END-PERFORM                                                          
014100                                                                          
014200     PERFORM Z-FINIT                                                      
014300     MOVE ZERO TO RETURN-CODE                                             
014400     GOBACK                                                               
014500     .                                                                    
014600     EJECT                                                                
014700 A-INIT SECTION.                                                          
014800                                                                          
014900     PERFORM IMS-RESTART                                                  
015000     OPEN INPUT W61410                                                    
015100     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
015200     .                                                                    
015300     EJECT                                                                
015400 B-BORT-GAMMAL-INFO SECTION.                                              
015500                                                                          
015600     MOVE '2403' TO W-2403-IDHTYP                                         
015700     PERFORM IMS-GET-2403                                                 
015800     IF SEGMENT-FINNS                                                     
015900        PERFORM IMS-DLET-2403                                             
016000     END-IF                                                               
016100     .                                                                    
016200     EJECT                                                                
016300 C-ISRT-WDGX2404 SECTION.                                                 
016400                                                                          
016500     MOVE IN-IDARTNR       TO 2404-IDARTNR                                
016600     MOVE IN-IDDC          TO 2404-IDDC                                   
016700     MOVE IN-IDFKNGRP      TO 2404-IDFKNGRP                               
016800     MOVE IN-KDLEVSP       TO 2404-KDLEVSP                                
016900     MOVE IN-KVSPARR-KVAL  TO 2404-KVSPARR-KVAL                           
017000     MOVE IN-IDUSER-SPKVAL TO 2404-IDUSER-SPKVAL                          
017100     MOVE IN-KVLS          TO 2404-KVLS                                   
017200     MOVE IN-KVROS         TO 2404-KVROS                                  
017300     MOVE IN-PRARTSTD      TO 2404-PRARTSTD                               
017400     MOVE IN-TISPARR-KVAL  TO 2404-TISPARR-KVAL                           
017500     MOVE IN-BEART-ENG     TO 2404-BEART-ENG                              
017600     MOVE IN-BEART-SVE     TO 2404-BEART-SVE                              
017610     MOVE IN-IDLEVNR       TO 2404-IDLEVNR                                
017700                                                                          
017800     PERFORM IMS-ISRT-WDGX2404                                            
017900     ADD +1 TO CHKP-ANT                                                   
018000     .                                                                    
018100     EJECT                                                                
018200 Z-FINIT SECTION.                                                         
018300                                                                          
018400     CLOSE W61410                                                         
018500     MOVE 'S' TO POSTSUM-OPKOD                                            
018600     CALL POSTSUM USING POSTSUM-PARM                                      
018700     .                                                                    
018800     EJECT                                                                
018900 S01-LAES-W61410  SECTION.                                                
019000                                                                          
019100     READ W61410 INTO IN-AREA                                             
019200     AT END                                                               
019300        SET END-OF-W61410 TO TRUE                                         
019400     NOT AT END                                                           
019500        MOVE 'W61410'   TO POSTSUM-FDNAMN                                 
019600        MOVE 'W61414D1' TO POSTSUM-DDNAMN2                                
019700        MOVE SPACE      TO POSTSUM-TRANSTYP                               
019800        CALL POSTSUM USING POSTSUM-PARM                                   
019900     END-READ                                                             
020000     .                                                                    
020100     EJECT                                                                
020200 X-TAG-CHECKPOINT   SECTION.                                              
020300                                                                          
020400     PERFORM IMS-CHECKPOINT                                               
020500     MOVE ZERO TO CHKP-ANT                                                
020600     MOVE '2403' TO W-2403-IDHTYP                                         
020700     PERFORM IMS-GET-2403                                                 
020800     .                                                                    
020900     EJECT                                                                
021000* --- IMS SEKTIONER ---                                                   
021100                                                                          
021200     EJECT                                                                
021300 IMS-GET-2403 SECTION.                                                    
021400     STRING 'WDR501  (WDGXKEY  =' W-2403KEY-X ')'                         
021500          DELIMITED BY SIZE INTO SSA1                                     
021600     MOVE '  GE' TO GODK-STATUSKODER                                      
021700     CALL CBLTDLI USING GHU 2403-PCB DLI-IO-WDR501 SSA1                   
021800     MOVE 2403-STATUS-CODE TO STATUS-WS                                   
021900     PERFORM IMS-STATUSKONTROLL                                           
022000     .                                                                    
022100     SKIP3                                                                
022200 IMS-ISRT-2403 SECTION.                                                   
022300     MOVE 'WDR501 ' TO SSA1                                               
022400     MOVE '  ' TO GODK-STATUSKODER                                        
022500     CALL CBLTDLI USING ISRT 2403-PCB DLI-IO-WDR501 SSA1                  
022600     MOVE 2403-STATUS-CODE TO STATUS-WS                                   
022700     PERFORM IMS-STATUSKONTROLL                                           
022800     .                                                                    
022900     SKIP3                                                                
023000 IMS-DLET-2403 SECTION.                                                   
023100     MOVE '  ' TO GODK-STATUSKODER                                        
023200     CALL CBLTDLI USING DLET 2403-PCB DLI-IO-WDR501                       
023300     MOVE 2403-STATUS-CODE TO STATUS-WS                                   
023400     PERFORM IMS-STATUSKONTROLL                                           
023500     .                                                                    
023600     EJECT                                                                
023700 IMS-ISRT-WDGX2404 SECTION.                                               
023800     STRING 'WDR501  (WDGXKEY  =' W-2403KEY-X ')'                         
023900          DELIMITED BY SIZE INTO SSA1                                     
024000     MOVE 'WDGX2404' TO SSA2                                              
024100     MOVE '  ' TO GODK-STATUSKODER                                        
024200     CALL CBLTDLI USING ISRT 2403-PCB DLI-IO-WDGX2404 SSA1 SSA2           
024300     MOVE 2403-STATUS-CODE TO STATUS-WS                                   
024400     PERFORM IMS-STATUSKONTROLL                                           
024500     .                                                                    
024600     EJECT                                                                
024700 IMS-RESTART SECTION.                                                     
024800     MOVE SPACE TO CHKP-MSG-IO-AREA                                       
024900     MOVE '  ' TO GODK-STATUSKODER                                        
025000     CALL CBLTDLI USING XRST MSG-PCB                                      
025100                        CHKP-MSG-IO-AREA-LENGTH CHKP-MSG-IO-AREA          
025200                        CHKP-AREA-LENGTH CHKP-AREA                        
025300     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
025400     PERFORM IMS-STATUSKONTROLL                                           
025500     .                                                                    
025600     SKIP3                                                                
025700 IMS-CHECKPOINT SECTION.                                                  
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
027800         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
027900           DELIMITED BY SIZE INTO FELTEXT                                 
028000         DISPLAY FELTEXT                                                  
028100         CALL FELLOG                                                      
028200       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
028300         CONTINUE                                                         
028400     END-SEARCH                                                           
028500     .                                                                    
