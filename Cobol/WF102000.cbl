001200 ID DIVISION.                                                             
001300 PROGRAM-ID.     WF102000.                                                
001400 AUTHOR.         BO HAMMARIN.                                             
001500 DATE-WRITTEN.   OCTOBER 2002.                                            
001600 DATE-COMPILED.                                                           
001800                                                                          
001900*    FUNCTION:                                                            
002000*        THE PGM                                                          
002100*        - READS PRM-DATA FROM SYSIN                                      
002110*        - READS FILE WITH NEW/CHANGED/DELETED VAT RECORDS                
002140*        - SENDS VAT DATA TO VIPS FOR                                     
002160*          . NETHERLANDS                                                  
002170*            BY USING WZ01SEND (CARPARTS.VIPS.RECVATNL)                   
002500*          DEPENDING ON SYMBOLIC PRM                                      
002510*                                                                         
002600                                                                          
002800 ENVIRONMENT DIVISION.                                                    
002900                                                                          
003000 INPUT-OUTPUT SECTION.                                                    
003200 FILE-CONTROL.                                                            
003300*          --- SYSIN FROM JCL                                             
003310     SELECT INDATA                     ASSIGN TO SYSIN.                   
003320                                                                          
003380*          --- VAT-RECORDS                                                
003390     SELECT WF1017                     ASSIGN TO WF1020D1.                
003500     EJECT                                                                
003510                                                                          
003600 DATA DIVISION.                                                           
003700                                                                          
003800 FILE SECTION.                                                            
003900 FD  INDATA                                                               
004000     LABEL RECORD STANDARD                                                
004010     RECORDING  F                                                         
004020     BLOCK CONTAINS 0.                                                    
004030 01  INPOST                      PIC X(80).                               
004040                                                                          
004097 FD  WF1017                                                               
004098     RECORDING       F                                                    
004099     BLOCK CONTAINS  0.                                                   
004100                                                                          
004110*01  -COPY WF10M17     -L.                                                
004120     EJECT                                                                
004121                                                                          
004200 WORKING-STORAGE SECTION.                                                 
004300 77  IDPGM                       PIC X(8)    VALUE 'WF102000'.            
004570 77  SYSIN-EOF                   PIC X       VALUE 'N'.                   
004581 77  OK-SW                       PIC X       VALUE 'N'.                   
004582 77  WF1017-EOF-SW               PIC X       VALUE 'N'.                   
004583     88  END-OF-WF1017                       VALUE 'J'.                   
004584 77  FIRST-TIME-SW               PIC X       VALUE 'J'.                   
004585     88  FIRST-TIME                          VALUE 'J'.                   
004590     EJECT                                                                
004600                                                                          
005100 01  WS-TABELL.                                                           
005200     03 WS-COUNTRY-VALUES.                                                
005300       05  FILLER PIC X(2)  VALUE 'NL'.                                   
005402       05  FILLER PIC X(1)  VALUE 'J'.                                    
005403       05  FILLER PIC X(50) VALUE 'CARPARTS.VIPS.RECVATNL'.               
005404       05  FILLER PIC X(2)  VALUE SPACE.                                  
005406       05  FILLER PIC X(1)  VALUE SPACE.                                  
005407       05  FILLER PIC X(50) VALUE SPACE.                                  
005408                                                                          
005409     03 WS-COUNTRY-IND REDEFINES WS-COUNTRY-VALUES OCCURS 2               
005410                       INDEXED BY IX.                                     
005411       05 TAB-IDLAND  PIC X(2).                                           
005412       05 TAB-FLEU    PIC X(1).                                           
005413       05 TAB-ADDRESS PIC X(50).                                          
005415     EJECT                                                                
005416                                                                          
005417 01  ERRTEXT.                                                             
005418     03  FILLER                  PIC X(8)    VALUE 'ERRTEXT'.             
005419     03  ERRTEXT-STR             PIC X(72)   VALUE SPACE.                 
005420 01  KDRC-DISPLAY                PIC Z(5).                                
005421     EJECT                                                                
005430                                                                          
005500 01  TODAYS-DATE                 PIC 9(6)    VALUE ZERO.                  
005600 01  FILLER REDEFINES TODAYS-DATE.                                        
005700     03  TODAYS-DATE-YEAR        PIC 9(2).                                
005800     03  TODAYS-DATE-MONTH       PIC 9(2).                                
005900     03  TODAYS-DATE-DAY         PIC 9(2).                                
006000     EJECT                                                                
006010                                                                          
006100 01  GENERAL-SUBPROGRAMS.                                                 
006400     03  ABEND                   PIC X(8)    VALUE 'ABEND   '.            
006401     03  WZ01SEND                PIC X(8)    VALUE 'WZ01SEND'.            
006402     EJECT                                                                
006410                                                                          
006420*    --- PARAMETERS FOR SUBPROGRAM ABEND                                  
006430                                                                          
006440 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
006450 77  RKOD-ABEND-NO-DUMP          PIC S9(4)   COMP VALUE +16.              
006460 77  RKOD-ABEND-WITH-DUMP        PIC S9(4)   COMP VALUE +1000.            
006601     EJECT                                                                
006603                                                                          
006627*    --- AREOR FÖR KOMMUNIKATION                                          
006628 01  FILLER                      PIC X(16)   VALUE 'SEND-CONTROL'.        
006629*01  -COPY WZ01SEND                                                       
006630     EJECT                                                                
009070                                                                          
009071 01  FILLER                      PIC X(10)   VALUE 'SYSIN '.              
009072 01  INAREA.                                                              
009073     03  PRM-IDLAND              PIC X(2).                                
009074     03  FILLER                  PIC X(78).                               
009075     EJECT                                                                
009076                                                                          
009080 01  UT17-AREA-START             PIC X(24)   VALUE                        
009090                                             'UT17-AREA-START'.           
009091 01  UT17-AREA.                                                           
009100*    03  -COPY WF10M17          -PRE UT17-                                
009200*                                                                         
009300     EJECT                                                                
009400                                                                          
010100 LINKAGE SECTION.                                                         
010300*01  -COPY W0009   -PRE MSG-                                              
010700     EJECT                                                                
010800                                                                          
010801 PROCEDURE DIVISION  USING MSG-PCB.                                       
010802                                                                          
010803 MAIN SECTION.                                                            
010810     ENTRY 'DLITCBL' USING MSG-PCB.                                       
010900                                                                          
011200     PERFORM A-INIT                                                       
011201                                                                          
011210     PERFORM B-EXECUTE                                                    
011212                                                                          
012510     PERFORM Z-FINIT                                                      
012700     MOVE ZERO TO RETURN-CODE                                             
012800     GOBACK                                                               
012900     .                                                                    
013000     EJECT                                                                
013010                                                                          
013100 A-INIT SECTION.                                                          
013110     OPEN INPUT INDATA                                                    
013120     READ INDATA NEXT RECORD INTO INAREA                                  
013130       AT END MOVE 'J'              TO SYSIN-EOF                          
013140     END-READ                                                             
013150     CLOSE INDATA                                                         
013151     DISPLAY PRM-IDLAND                                                   
013160                                                                          
013200     OPEN INPUT WF1017                                                    
014200     .                                                                    
014350     EJECT                                                                
014360                                                                          
014370 B-EXECUTE SECTION.                                                       
014391     PERFORM S01-READ-WF1017                                              
014392                                                                          
014394     PERFORM UNTIL END-OF-WF1017                                          
014395       MOVE 'N'                TO   OK-SW                                 
014396       PERFORM UNTIL OK-SW = 'J' OR                                       
014397                     TAB-IDLAND(IX) = SPACE                               
014398         IF TAB-IDLAND(IX) = PRM-IDLAND                                   
014401           MOVE 'J'            TO   OK-SW                                 
014402         END-IF                                                           
014405       END-PERFORM                                                        
014406       IF OK-SW = 'J'                                                     
014409         IF (UT17-IDLAND = 'EU' AND TAB-FLEU(IX) = 'J') OR                
014410            (UT17-IDLAND = PRM-IDLAND)                                    
014411           IF FIRST-TIME                                                  
014412             PERFORM S11-VAT-OPEN                                         
014413             MOVE 'N'          TO FIRST-TIME-SW                           
014414           END-IF                                                         
014415           PERFORM S12-VAT-PUT                                            
014416           PERFORM S01-READ-WF1017                                        
014417         ELSE                                                             
014418           PERFORM S01-READ-WF1017                                        
014419         END-IF                                                           
014420       ELSE                                                               
014421         CALL ABEND USING RKOD-ABEND-WITH-DUMP                            
014422       END-IF                                                             
014423     END-PERFORM                                                          
014424     .                                                                    
014425     EJECT                                                                
014426                                                                          
014500 Z-FINIT SECTION.                                                         
014600     IF NOT FIRST-TIME                                                    
014601       PERFORM S13-VAT-CLOSE                                              
014602     END-IF                                                               
014610                                                                          
014700     CLOSE WF1017                                                         
015000     .                                                                    
015101     EJECT                                                                
015102                                                                          
015438 S01-READ-WF1017  SECTION.                                                
015439     READ WF1017          INTO UT17-WF10M17                               
015440     AT END                                                               
015441        MOVE HIGH-VALUE   TO   UT17-WF10M17                               
015442        SET END-OF-WF1017 TO   TRUE                                       
015443     END-READ                                                             
015444     .                                                                    
015445                                                                          
015455 S11-VAT-OPEN SECTION.                                                    
015463     MOVE 'N'                TO OK-SW                                     
015464     PERFORM UNTIL OK-SW = 'J' OR                                         
015465                   TAB-IDLAND(IX) = SPACE                                 
015466       IF TAB-IDLAND(IX) = PRM-IDLAND                                     
015467         MOVE 'J'            TO   OK-SW                                   
015468       END-IF                                                             
015469     END-PERFORM                                                          
015470                                                                          
015471     IF OK-SW = 'J'                                                       
015474       MOVE TAB-ADDRESS(IX)  TO SEND-ADDISPABS                            
015475       MOVE 'OPEN'           TO SEND-KDFUNC                               
015476       CALL WZ01SEND USING SEND-CONTROL-AREA                              
015477                           SEND-OPEN-AREA                                 
015478       IF SEND-KDRC > ZERO                                                
015479         MOVE SEND-KDRC      TO KDRC-DISPLAY                              
015480         STRING 'WZ01SEND OPEN ERROR RC=' KDRC-DISPLAY                    
015481         DELIMITED BY SIZE INTO ERRTEXT-STR                               
015482         CALL ABEND USING RKOD-ABEND-WITH-DUMP                            
015483       END-IF                                                             
015485     ELSE                                                                 
015486       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
015487     END-IF                                                               
015488     .                                                                    
015490                                                                          
015514 S12-VAT-PUT SECTION.                                                     
015515     MOVE 'PUT'                           TO SEND-KDFUNC                  
015516     MOVE LENGTH OF UT17-AREA             TO SEND-KVDLEN                  
015517     CALL WZ01SEND USING SEND-CONTROL-AREA                                
015518                         SEND-KVDLEN                                      
015519                         UT17-AREA                                        
015520     IF SEND-KDRC > 1                                                     
015521       MOVE SEND-KDRC                     TO KDRC-DISPLAY                 
015522       STRING 'WZ01SEND PUT ERROR RC=' KDRC-DISPLAY                       
015523       DELIMITED BY SIZE INTO ERRTEXT-STR                                 
015524       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
015525     END-IF                                                               
015526     .                                                                    
015528                                                                          
015529 S13-VAT-CLOSE SECTION.                                                   
015530     MOVE 'CLOSE'                         TO SEND-KDFUNC                  
015531     CALL WZ01SEND USING SEND-CONTROL-AREA                                
015532     .                                                                    
