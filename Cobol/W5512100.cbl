000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W5512100.                                                
000300 AUTHOR.         SARASWATHY S.                                            
000400 DATE-WRITTEN.   18/09/05.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700                                                                          
000800*    FUNCTION:                                                            
000900*        STD PRICE YEARLY PROCESS                                         
001000*        PROGRAM READS FILE W01185 TO GET WDK24 VALUES                    
001200*        PROGRAM READS WDK6 (W510PRTR SUBPROGRAM)                         
001300                                                                          
001400     SKIP3                                                                
001500 ENVIRONMENT DIVISION.                                                    
001600     SKIP2                                                                
001700 INPUT-OUTPUT SECTION.                                                    
001800                                                                          
001900 FILE-CONTROL.                                                            
002000     SKIP2                                                                
002100*          --- IN FILE - W01185                                           
002200     SELECT W01185                     ASSIGN TO W55121D1.                
002300*          --- IN FILE - W55147                                           
002400     SELECT W55147                     ASSIGN TO W55121D2.                
002500*          --- UT FILE - STANDARD PRICE                                   
002600     SELECT W55121                     ASSIGN TO W55121D3.                
002700     EJECT                                                                
002800 DATA DIVISION.                                                           
002900     SKIP3                                                                
003000 FILE SECTION.                                                            
003100     SKIP3                                                                
003200 FD  W01185                                                               
003300     RECORDING       F                                                    
003400     BLOCK CONTAINS  0.                                                   
003500*01  W01185-POST  -COPY W01185   -L                                       
003600 FD  W55147                                                               
003700     RECORDING       F                                                    
003800     BLOCK CONTAINS  0.                                                   
003900*01  W55147-POST  -COPY W55147   -L                                       
004000 FD  W55121                                                               
004100     RECORDING       F                                                    
004200     BLOCK CONTAINS  0.                                                   
004300*01  W55121-POST  -COPY W55147   -L                                       
004400     EJECT                                                                
004500 WORKING-STORAGE SECTION.                                                 
004600                                                                          
004700 77  IDPGM                       PIC X(8)    VALUE 'W5512100'.            
005000 77  W01185-EOF-SW               PIC X       VALUE 'N'.                   
005100     88  END-OF-W01185                       VALUE 'J'.                   
005200 77  W55147-EOF-SW               PIC X       VALUE 'N'.                   
005300     88  END-OF-W55147                       VALUE 'J'.                   
005400     SKIP2                                                                
005710 01  WS-IDARTNR       PIC S9(9) COMP-3 VALUE 0.                           
007600     EJECT                                                                
007700 01  GENERAL-SUBPROGRAMS.                                                 
007800*                                                                         
008100     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
008300     03  W510PRTR                PIC X(8)    VALUE 'W510PRTR'.            
008400     EJECT                                                                
008500*    --- PARAMETRAR TILL POSTSUM                                          
008600*                                                                         
008700*01  -COPY W0005   -PRE  POSTSUM-                                         
008800     EJECT                                                                
008900*01  -COPY WWDC99                                                         
009000     EJECT                                                                
009100 01  FILLER                    PIC X(8)    VALUE 'W510PRTR'.              
009200*01       -COPY W510PRTR                                                  
009300     EJECT                                                                
009400*01  -COPY WDATAREA                                                       
009500     EJECT                                                                
009600 01  IN-AREA-START               PIC X(24)   VALUE                        
009700                                             'IN-AREA-START'.             
009800*01  AREA -COPY W01185     -PRE IN1-                                      
009900     EJECT                                                                
010000*                                                                         
010100*01  AREA -COPY W55147     -PRE IN2-                                      
010200     EJECT                                                                
010300*                                                                         
010400 01  UT-AREA-START               PIC X(24)   VALUE                        
010500                                             'UT-AREA-START'.             
010600*01  AREA -COPY W55147     -PRE UT-                                       
010700     EJECT                                                                
010800*                                                                         
010900     SKIP2                                                                
011000*                                                                         
011100     EJECT                                                                
011200 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
011300     SKIP3                                                                
011400 LINKAGE SECTION.                                                         
011500                                                                          
011600*01  -COPY W0008  -PRE WDK6-                                              
011700     05 FILLER                   PIC X(13).                               
011800     EJECT                                                                
011900 PROCEDURE DIVISION  USING WDK6-PCB.                                      
012000 MAIN SECTION.                                                            
012100     ENTRY 'DLITCBL' USING WDK6-PCB.                                      
012200                                                                          
012300     PERFORM A-INIT                                                       
012400                                                                          
012500     PERFORM F-READ-SHOW-INFO                                             
012600                                                                          
012700     PERFORM Z-FINIT                                                      
012800                                                                          
012900     MOVE ZERO TO RETURN-CODE                                             
013000     GOBACK                                                               
013100     .                                                                    
013200     EJECT                                                                
013300 A-INIT SECTION.                                                          
013400     SKIP2                                                                
013500     OPEN INPUT  W01185                                                   
013600                 W55147                                                   
013700          OUTPUT W55121                                                   
013800     INITIALIZE  IN1-AREA                                                 
013810     INITIALIZE  IN2-AREA                                                 
013900     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
014000     .                                                                    
014100     EJECT                                                                
014200*                                                                         
014300 F-READ-SHOW-INFO SECTION.                                                
014400     PERFORM S01-READ-W01185                                              
014500     PERFORM UNTIL END-OF-W01185                                          
014510       IF IN1-SPRL-IDARTNR NOT = WS-IDARTNR                               
014600        MOVE IN1-SPRL-IDDC    TO WS-IDDC                                  
014700        MOVE IN1-SPRL-IDARTNR TO WS-IDARTNR                               
014800        IF NDC-US OR NDC-CN                                               
014804         PERFORM UNTIL END-OF-W55147 OR                                   
014802                      (IN1-SPRL-IDARTNR <= IN2-IDARTNR)                   
014806          PERFORM S01-READ-W55147                                         
014808         END-PERFORM                                                      
015100          IF IN1-SPRL-IDARTNR = IN2-IDARTNR                               
015110           PERFORM FA-PRICE-CHECK                                         
015120           IF PRTR-KDSVAR = '1'                                           
015200              PERFORM FB-MOVE-FILE-DATA                                   
015300              PERFORM S11-WRITE-W55121                                    
015310           END-IF                                                         
015400          END-IF                                                          
015700        END-IF                                                            
015810       END-IF                                                             
015820       PERFORM S01-READ-W01185                                            
015900     END-PERFORM                                                          
016000     .                                                                    
016100     EJECT                                                                
016200*                                                                         
016210 FA-PRICE-CHECK SECTION.                                                  
016220     MOVE IN1-SPRL-IDDC     TO PRTR-IDDC                                  
016230     MOVE IN1-SPRL-IDARTNR  TO PRTR-IDARTNR                               
016240     MOVE 030               TO PRTR-KDCALL                                
016250     CALL W510PRTR USING PRTR-W510PRTR WDK6-PCB                           
016260     .                                                                    
016270     EJECT                                                                
016300 FB-MOVE-FILE-DATA SECTION.                                               
016400                                                                          
016500     MOVE IN1-SPRL-IDARTNR     TO UT-IDARTNR                              
016800     MOVE IN1-SPRL-IDLEVNR-PR  TO UT-IDLEVNR                              
016900     MOVE IN1-SPRL-KDVALISO    TO UT-KDVALISO                             
017000     MOVE IN1-SPRL-PRARTBEL-PR TO UT-PRARTBEL-PR                          
017100     MOVE IN2-FLAPC            TO UT-FLAPC                                
017200     MOVE IN2-FLIART           TO UT-FLIART                               
017300     MOVE IN2-FLPRFIL          TO UT-FLPRFIL                              
017400     MOVE IN2-IDFKNGRP         TO UT-IDFKNGRP                             
017500     MOVE IN2-IDLEVNR-HUV      TO UT-IDLEVNR-HUV                          
017600     MOVE IN2-IDPRANSV         TO UT-IDPRANSV                             
017700     MOVE IN2-KDHF             TO UT-KDHF                                 
017800     MOVE IN2-KDPRBEH          TO UT-KDPRBEH                              
017800     MOVE IN2-KDPRODSL         TO UT-KDPRODSL                             
017810     MOVE IN2-KDSTASPIS        TO UT-KDSTASPIS                            
017820     MOVE IN2-KVBEHOVAR        TO UT-KVBEHOVAR                            
017830     MOVE IN2-KVDISP-SPIS      TO UT-KVDISP-SPIS                          
017840     MOVE IN2-PRARTBES         TO UT-PRARTBES                             
017850     MOVE IN2-PRARTSJK         TO UT-PRARTSJK                             
017860     MOVE IN2-PRDIRLON-AKT     TO UT-PRDIRLON-AKT                         
017870     MOVE IN2-PRDIRLON-KOM     TO UT-PRDIRLON-KOM                         
017880     MOVE IN2-PRDMTRL-AKT      TO UT-PRDMTRL-AKT                          
017890     MOVE IN2-PRDMTRL-KOM      TO UT-PRDMTRL-KOM                          
017891     MOVE IN2-PRINK-AKT        TO UT-PRINK-AKT                            
017892     MOVE IN2-PRINK-KOM        TO UT-PRINK-KOM                            
017893     MOVE IN2-PRKURS           TO UT-PRKURS                               
017894     MOVE IN2-PROVRPAL-AKT     TO UT-PROVRPAL-AKT                         
017895     MOVE IN2-PROVRPAL-KOM     TO UT-PROVRPAL-KOM                         
017896     MOVE IN2-REAENDR          TO UT-REAENDR                              
017897     MOVE IN2-REDIRLEV         TO UT-REDIRLEV                             
017898     MOVE IN2-RETULF           TO UT-RETULF                               
017899     MOVE IN2-TEARTNOT         TO UT-TEARTNOT                             
017900     MOVE IN2-TIPRLIST         TO UT-TIPRLIST                             
017901     MOVE IN2-IDNAMN           TO UT-IDNAMN                               
017902     MOVE IN2-IDINK            TO UT-IDINK                                
017903     MOVE IN2-IDMAIL           TO UT-IDMAIL                               
017910     .                                                                    
018000     EJECT                                                                
018100*                                                                         
022400 Z-FINIT SECTION.                                                         
022500     CLOSE W01185                                                         
022600           W55147                                                         
022610           W55121                                                         
022700     SKIP2                                                                
022800     MOVE 'S' TO POSTSUM-OPKOD                                            
022900     CALL POSTSUM USING POSTSUM-PARM                                      
023000     .                                                                    
023100     EJECT                                                                
023200 S01-READ-W01185  SECTION.                                                
023300     READ W01185 INTO IN1-AREA                                            
023400     AT END                                                               
023500        SET END-OF-W01185 TO TRUE                                         
023600                                                                          
023700     NOT AT END                                                           
023800        MOVE 'IN'         TO POSTSUM-TRANSTYP                             
023900        MOVE 'W01185'     TO POSTSUM-FDNAMN                               
024000        MOVE 'W55121D1'   TO POSTSUM-DDNAMN2                              
024100        CALL POSTSUM USING POSTSUM-PARM                                   
024200     END-READ                                                             
024300     .                                                                    
024400     EJECT                                                                
024410 S01-READ-W55147  SECTION.                                                
024420     READ W55147 INTO IN2-AREA                                            
024430     AT END                                                               
024440        SET END-OF-W55147 TO TRUE                                         
024460     NOT AT END                                                           
024470        MOVE 'IN'         TO POSTSUM-TRANSTYP                             
024480        MOVE 'W55147'     TO POSTSUM-FDNAMN                               
024490        MOVE 'W55121D2'   TO POSTSUM-DDNAMN2                              
024491        CALL POSTSUM USING POSTSUM-PARM                                   
024492     END-READ                                                             
024493     .                                                                    
024494     EJECT                                                                
024500                                                                          
024600 S11-WRITE-W55121 SECTION.                                                
024700     SKIP2                                                                
024800     WRITE W55121-POST FROM UT-AREA                                       
024900                                                                          
025000     MOVE SPACE     TO POSTSUM-TRANSTYP                                   
025100     MOVE 'W55121 ' TO POSTSUM-FDNAMN                                     
025200     MOVE 'W55121D3' TO POSTSUM-DDNAMN2                                   
025300     CALL POSTSUM USING POSTSUM-PARM                                      
025400     .                                                                    
025500     EJECT                                                                
