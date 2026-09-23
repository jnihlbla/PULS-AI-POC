001100 ID DIVISION.                                                             
001200     SKIP2                                                                
001300 PROGRAM-ID.     W2711400.                                                
001400*AUTHOR.         STEFAN ÅSGÅRDEN.                                         
001500*DATE-WRITTEN.   SEP 2002.                                                
001600                                                                          
001700*    REMARKS                                                              
001800*                                                                         
001900*    FUNKTION:                                                            
002000*        BOKAR NER RESERVERAT ANTAL FÖR ETT CROSS DOCKING OMRÅDE          
002200*                                                                         
002301*        PROGRAMMET UPPDATERAR WDK6                                       
002400*                                                                         
002500*    ABENDKODER:                                                          
002600*        U0016 -  . . . .                                                 
002700*        U1000 -  . . . .                                                 
002800*                                                                         
002900                                                                          
003000     SKIP3                                                                
003100 ENVIRONMENT DIVISION.                                                    
003200     SKIP2                                                                
003300 INPUT-OUTPUT SECTION.                                                    
003400                                                                          
003500 FILE-CONTROL.                                                            
003601     SKIP2                                                                
003602*          --- INFIL MED RENS-POSTER                                      
003610     SELECT W27114                     ASSIGN TO W27114D1.                
003800     EJECT                                                                
003900 DATA DIVISION.                                                           
004000     SKIP3                                                                
004100 FILE SECTION.                                                            
004201     SKIP3                                                                
004202 FD  W27114                                                               
004203     RECORDING       F                                                    
004204     BLOCK CONTAINS  0.                                                   
004205                                                                          
004210*01  -COPY W27114      -L.                                                
004300     EJECT                                                                
004400 WORKING-STORAGE SECTION.                                                 
004500     SKIP2                                                                
004501                                                                          
004510*    -- CHECKED BY WY2000                                                 
004600 77  IDPGM                       PIC X(8)    VALUE 'W2711400'.            
004700 01  CHKP-VAR.                                                            
004800 03  CHKP-MSG-IO-AREA-LENGTH     PIC S9(9)   VALUE +32 COMP SYNC.         
004900 03  CHKP-MSG-IO-AREA            PIC X(32)   VALUE SPACE.                 
005000 03  CHKP-AREA-LENGTH            PIC S9(9)   VALUE +32 COMP SYNC.         
005100 03  CHKP-AREA                   PIC X(32)   VALUE SPACE.                 
005200 03  CHKP-ANT                    PIC S9(3)   VALUE +0.                    
005300 03  CHKP-MAX                    PIC S9(3)   VALUE +100.                  
005400 77  JA                          PIC X       VALUE 'J'.                   
005500 77  NEJ                         PIC X       VALUE 'N'.                   
005510 01  IX                          PIC S9(9)   VALUE ZERO COMP-3.           
005600     SKIP2                                                                
005700 01  FELTEXT.                                                             
005800     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
005900     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
006101                                                                          
006102 77  W27114-EOF-SW               PIC X       VALUE 'N'.                   
006110     88  END-OF-W27114                       VALUE 'J'.                   
006400     EJECT                                                                
006500 01  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
006600                                                                          
007000     EJECT                                                                
007010                                                                          
007020                                                                          
007100 01  DYNAMISKA-SUBPROGRAM.                                                
007200*                                                                         
007300     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
007400     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
007601     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
007610     03  WDATKONV                PIC X(8)    VALUE 'WDATKONV'.            
007620     03  ABEND                   PIC X(8)    VALUE 'ABEND   '.            
007700     EJECT                                                                
007800 77  RKOD-ABEND-UTAN-DUMP        PIC S9(4)   COMP VALUE +16.              
007900 77  RKOD-ABEND-MED-DUMP         PIC S9(4)   COMP VALUE +1000.            
008000     EJECT                                                                
008502*    --- PARAMETRAR TILL POSTSUM                                          
008503*                                                                         
008510*01  -COPY W0005   -PRE  POSTSUM-                                         
008701     EJECT                                                                
008710*01  -COPY WDATAREA                                                       
008801     EJECT                                                                
008802 01  IN-AREA-START               PIC X(24)   VALUE                        
008803                                             'IN-AREA-START'.             
008804     SKIP2                                                                
008805                                                                          
008810*01  AREA -COPY W27114     -PRE IN-                                       
008900*                                                                         
009000     EJECT                                                                
009100 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
009200     SKIP3                                                                
009300 01  NYCKLAR-TILL-DLI.                                                    
009422     03  W-IDARTNR-X.                                                     
009423         05  W-IDARTNR           PIC S9(9)   VALUE ZERO COMP-3.           
009500     SKIP2                                                                
009600*    --- STATUS-KOD FRÅN IMS                                              
009700 01  STATUS-WS                   PIC XX.                                  
009800     88  SEGMENT-FINNS                       VALUE '  '.                  
009900     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
010000     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
010100     88  SEGMENT-SLUT                        VALUE 'GB'.                  
010200     88  IMS-EJ-OK                           VALUE 'XD'.                  
010300     SKIP2                                                                
010400 01  GODK-STATUSKODER.                                                    
010500     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
010600     SKIP3                                                                
010700 01  SSA1                        PIC X(128).                              
010800 01  SSA2                        PIC X(64).                               
010900     EJECT                                                                
011000*    --- IMS FUNKTIONSKODER                                               
011100*01  -COPY W0003                                                          
011200     EJECT                                                                
011400*    ---  DLI INPUT-OUTPUT AREA                                           
011410 01  FILLER                      PIC X(16) VALUE 'DLI-IO-K611'.           
011420     SKIP3                                                                
011430 01  DLI-IO-AREA-K611.                                                    
011440*    03  -COPY WDK611                                                     
011450     EJECT                                                                
012994                                                                          
013000 LINKAGE SECTION.                                                         
013100                                                                          
013200*01  -COPY W0009   -PRE MSG-                                              
013301     EJECT                                                                
013302*01  -COPY W0008  -PRE WDK6-                                              
013303     05  FILLER                  PIC X.                                   
013702     EJECT                                                                
013703 PROCEDURE DIVISION  USING MSG-PCB WDK6-PCB.                              
013710     ENTRY 'DLITCBL' USING MSG-PCB WDK6-PCB.                              
013800                                                                          
014100     PERFORM A-INIT                                                       
014200                                                                          
014210     PERFORM S01-LAES-W27114                                              
014220                                                                          
014300     PERFORM UNTIL END-OF-W27114                                          
014310                                                                          
014400       IF CHKP-ANT > CHKP-MAX                                             
014500         PERFORM X-TAG-CHECKPOINT                                         
014600       END-IF                                                             
014700                                                                          
015000       MOVE IN-IDARTNR       TO W-IDARTNR                                 
015100       PERFORM IMS-GHU-K611                                               
015210                                                                          
015300       MOVE 1                TO IX                                        
015308       PERFORM UNTIL IX > 4                                               
015309       OR IN-ADLAGOMR-CD = CLAG-ADLAGOMR-CD (IX)                          
015310         ADD 1               TO IX                                        
015311       END-PERFORM                                                        
015312                                                                          
015313       IF IX > 4                                                          
015314        MOVE 'FEL ADLAGOMR I INFILEN'                                     
015315                             TO FELTEXT-STR                               
015316        DISPLAY FELTEXT                                                   
015317        PERFORM S99-ABEND                                                 
015318       END-IF                                                             
015319                                                                          
015320       COMPUTE CLAG-KVRESS-CD (IX) =                                      
015321               CLAG-KVRESS-CD (IX) - IN-KVBEART-CD-RES                    
015322                                                                          
015323       COMPUTE CLAG-KVLS-CD (IX) =                                        
015324               CLAG-KVLS-CD (IX) - IN-KVBEART-CD                          
015325                                                                          
015330       PERFORM IMS-REPL-K611                                              
015340       ADD +1 TO CHKP-ANT                                                 
015361                                                                          
015370       PERFORM S01-LAES-W27114                                            
015400     END-PERFORM                                                          
015500                                                                          
015600                                                                          
015700     PERFORM Z-FINIT                                                      
015800                                                                          
015900     MOVE ZERO TO RETURN-CODE                                             
016000     GOBACK                                                               
016100     .                                                                    
016200     EJECT                                                                
016300 A-INIT SECTION.                                                          
016400     SKIP2                                                                
016500                                                                          
016600     PERFORM IMS-RESTART                                                  
016801                                                                          
016810     OPEN INPUT W27114                                                    
017500                                                                          
017600     ACCEPT DAGENS-DATUM FROM DATE                                        
018010     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
018300     .                                                                    
018500     EJECT                                                                
018600 Z-FINIT SECTION.                                                         
019101                                                                          
019110     CLOSE W27114                                                         
019301     SKIP2                                                                
019302     MOVE 'S' TO POSTSUM-OPKOD                                            
019310     CALL POSTSUM USING POSTSUM-PARM                                      
019500     .                                                                    
019601     EJECT                                                                
019602 S01-LAES-W27114  SECTION.                                                
019603     SKIP2                                                                
019604     READ W27114 INTO IN-AREA                                             
019605     AT END                                                               
019607        SET END-OF-W27114 TO TRUE                                         
019608                                                                          
019609     NOT AT END                                                           
019610        MOVE 'W27114' TO POSTSUM-FDNAMN                                   
019611        MOVE 'W27114D1' TO POSTSUM-DDNAMN2                                
019613        CALL POSTSUM USING POSTSUM-PARM                                   
019614                                                                          
019616     END-READ                                                             
019620     .                                                                    
019900     EJECT                                                                
019910 S99-ABEND SECTION.                                                       
019920                                                                          
019930     SKIP2                                                                
019940     CALL ABEND USING RKOD-ABEND-UTAN-DUMP                                
019950     .                                                                    
019960     EJECT                                                                
020000 X-TAG-CHECKPOINT   SECTION.                                              
020100                                                                          
020700     PERFORM IMS-CHECKPOINT                                               
020800     MOVE ZERO TO CHKP-ANT                                                
021000     .                                                                    
021100     EJECT                                                                
021200* --- IMS SEKTIONER ---                                                   
021300     SKIP3                                                                
021401     EJECT                                                                
021600 IMS-GHU-K611 SECTION.                                                    
021601     STRING 'WDK601  (IDARTNR  =' W-IDARTNR-X ')'                         
021602          DELIMITED BY SIZE INTO SSA1                                     
021603     MOVE 'WDK611  '       TO SSA2                                        
021604     MOVE '  ' TO GODK-STATUSKODER                                        
021605     CALL CBLTDLI USING GHU WDK6-PCB DLI-IO-AREA-K611 SSA1 SSA2           
021606     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
021607     PERFORM IMS-STATUSKONTROLL                                           
021608     .                                                                    
021609     SKIP3                                                                
021610 IMS-REPL-K611 SECTION.                                                   
021611     SKIP2                                                                
021612     MOVE '  ' TO GODK-STATUSKODER                                        
021613     CALL CBLTDLI USING REPL WDK6-PCB DLI-IO-AREA-K611                    
021614     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
021615     PERFORM IMS-STATUSKONTROLL                                           
021616     .                                                                    
021617     EJECT                                                                
021628                                                                          
021629                                                                          
021630 IMS-RESTART SECTION.                                                     
021700     SKIP2                                                                
021800     MOVE SPACE TO CHKP-MSG-IO-AREA                                       
021900     MOVE '  ' TO GODK-STATUSKODER                                        
022000     CALL CBLTDLI USING XRST MSG-PCB                                      
022100                        CHKP-MSG-IO-AREA-LENGTH CHKP-MSG-IO-AREA          
022200                        CHKP-AREA-LENGTH CHKP-AREA                        
022300     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
022400     PERFORM IMS-STATUSKONTROLL                                           
022500     .                                                                    
022600     EJECT                                                                
022700 IMS-CHECKPOINT SECTION.                                                  
022800     SKIP2                                                                
022900     MOVE SPACE TO CHKP-MSG-IO-AREA                                       
023000     MOVE '  XD' TO GODK-STATUSKODER                                      
023100     CALL CBLTDLI USING CHKP MSG-PCB                                      
023200                        CHKP-MSG-IO-AREA-LENGTH CHKP-MSG-IO-AREA          
023300                        CHKP-AREA-LENGTH CHKP-AREA                        
023400     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
023500     PERFORM IMS-STATUSKONTROLL                                           
023600                                                                          
023700     IF IMS-EJ-OK                                                         
023800       MOVE 'IMS-KONTROLLREGION EJ TILLGÄNGLIG' TO FELTEXT-STR            
023900       DISPLAY FELTEXT                                                    
024000       CALL FELLOG                                                        
024100     END-IF                                                               
024200     .                                                                    
024300     EJECT                                                                
024400 IMS-STATUSKONTROLL SECTION.                                              
024500     SKIP2                                                                
024600     SET STATUS-IX TO 1                                                   
024700     SEARCH GODK-STATUS                                                   
024800       AT END                                                             
024900         MOVE 'XXXXXXXXXX' TO FELTEXT-STR                                 
025000         DISPLAY FELTEXT                                                  
025100         CALL FELLOG                                                      
025200       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
025300         CONTINUE                                                         
025400     END-SEARCH                                                           
025500     .                                                                    
