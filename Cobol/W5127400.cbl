001100 ID DIVISION.                                                             
001200 PROGRAM-ID.     W5127400.                                                
001300 AUTHOR.         SARASWATHY S.                                            
001400 DATE-WRITTEN.   19/06/14.                                                
001500 DATE-COMPILED.                                                           
001600                                                                          
001700*    FUNCTION:                                                            
001800*        CREATES FILE W51274A WITH KVRESS FROM W01160                     
001900*        CREATES FILE W51274B WITH KVRESS FROM W01184                     
002000*                                                                         
002200*                                                                         
002300*    ABENDCODES:                                                          
002400*        U0016 -  . . . .                                                 
002500*        U1000 -  . . . .                                                 
002600*                                                                         
002700                                                                          
002800     SKIP3                                                                
002900 ENVIRONMENT DIVISION.                                                    
003000     SKIP2                                                                
003100 INPUT-OUTPUT SECTION.                                                    
003200                                                                          
003300 FILE-CONTROL.                                                            
003401     SKIP2                                                                
003402*          --- WDK7 EXTRACT FILE W01184                                   
003403     SELECT W01184                     ASSIGN TO W51274D1.                
003404     SKIP2                                                                
003405*          --- WDK6 EXTRACT FILE W01160                                   
003406     SELECT W01160                     ASSIGN TO W51274D2.                
003407     SKIP2                                                                
003408*          --- KVRESS FROM W01184 K7 EXTRACT FILE                         
003409     SELECT W51274A                    ASSIGN TO W51274D3.                
003410     SKIP2                                                                
003411*          --- KVRESS FROM K6 EXTRACT FILE W01160                         
003420     SELECT W51274B                    ASSIGN TO W51274D4.                
003600     EJECT                                                                
003700 DATA DIVISION.                                                           
003800     SKIP2                                                                
003900 FILE SECTION.                                                            
004001     SKIP3                                                                
004002 FD  W01184                                                               
004003     RECORDING       F                                                    
004004     BLOCK CONTAINS  0.                                                   
004005                                                                          
004006*01  -COPY W01184      -L.                                                
004007     SKIP3                                                                
004008 FD  W01160                                                               
004009     RECORDING       F                                                    
004010     BLOCK CONTAINS  0.                                                   
004011                                                                          
004012*01  -COPY W01160      -L.                                                
004013     SKIP3                                                                
004014 FD  W51274A                                                              
004015     RECORDING       F                                                    
004016     BLOCK CONTAINS  0.                                                   
004017                                                                          
004018*01  RECORD -COPY W51274 -PRE  UT1-  -L.                                  
004019     SKIP3                                                                
004020 FD  W51274B                                                              
004021     RECORDING       F                                                    
004022     BLOCK CONTAINS  0.                                                   
004023                                                                          
004030*01  RECORD -COPY W51274 -PRE  UT2-  -L.                                  
004100     EJECT                                                                
004200 WORKING-STORAGE SECTION.                                                 
004300                                                                          
004400 77  IDPGM                       PIC X(8)    VALUE 'W5127400'.            
004500 77  YES                         PIC X       VALUE 'J'.                   
004600 77  NOO                         PIC X       VALUE 'N'.                   
004801                                                                          
004802 77  W01184-EOF-SW               PIC X       VALUE 'N'.                   
004803     88  END-OF-W01184                       VALUE 'J'.                   
004804                                                                          
004805 77  W01160-EOF-SW               PIC X       VALUE 'N'.                   
004810     88  END-OF-W01160                       VALUE 'J'.                   
004900     EJECT                                                                
005000 01  TODAYS-DATE                 PIC 9(6)    VALUE ZERO.                  
005100 01  FILLER REDEFINES TODAYS-DATE.                                        
005200     03  TODAYS-DATE-YEAR        PIC 9(2).                                
005300     03  TODAYS-DATE-MONTH       PIC 9(2).                                
005400     03  TODAYS-DATE-DAY         PIC 9(2).                                
005500     EJECT                                                                
005600 01  GENERAL-SUBPROGRAMS.                                                 
005700*                                                                         
005800     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
005900     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
006000     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
006110     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
006200     SKIP2                                                                
006300*    --- PARAMETERS FOR SUBPROGRAM ABEND                                  
006400                                                                          
006500 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
006600 77  RKOD-ABEND-NO-DUMP          PIC S9(4)   COMP VALUE +16.              
006700 77  RKOD-ABEND-WITH-DUMP        PIC S9(4)   COMP VALUE +1000.            
006800     SKIP2                                                                
006900 01  ERROR-TEXT.                                                          
007000     03  FILLER                  PIC X(10)   VALUE 'ERROR-TEXT'.          
007100     03  ERROR-TEXT-STR          PIC X(72)   VALUE SPACE.                 
007201     EJECT                                                                
007202*01  -COPY WWDC99                                                         
007203*01  -COPY WWDCKONS                                                       
007204*    --- PARAMETRAR TILL POSTSUM                                          
007205*                                                                         
007210*01  -COPY W0005   -PRE  POSTSUM-                                         
007401     EJECT                                                                
007402 01  IN1-AREA-START              PIC X(24)   VALUE                        
007403                                 'IN1-AREA-START  '.                      
007404     SKIP2                                                                
007405                                                                          
007406*01  AREA -COPY W01184     -PRE IN1-                                      
007407     EJECT                                                                
007408 01  IN2-AREA-START              PIC X(24)   VALUE                        
007409                                 'IN2-AREA-START  '.                      
007410     SKIP2                                                                
007411                                                                          
007412*01  AREA -COPY W01160     -PRE IN2-                                      
007413     EJECT                                                                
007414 01  UT1-AREA-START              PIC X(24)   VALUE                        
007415                                 'UT1-AREA-START  '.                      
007416     SKIP2                                                                
007417                                                                          
007418*01  AREA -COPY W51274      -PRE UT1-                                     
007419     EJECT                                                                
007420 01  UT2-AREA-START              PIC X(24)   VALUE                        
007421                                 'UT2-AREA-START  '.                      
007422     SKIP2                                                                
007423                                                                          
007430*01  AREA -COPY W51274      -PRE UT2-                                     
007500     EJECT                                                                
010500 LINKAGE SECTION.                                                         
010600                                                                          
010800     EJECT                                                                
010901 PROCEDURE DIVISION.                                                      
010902 MAIN SECTION.                                                            
011200                                                                          
011300     PERFORM A-INIT                                                       
011400                                                                          
011501     PERFORM S01-READ-W01184                                              
011600     PERFORM UNTIL END-OF-W01184                                          
011700       PERFORM B-PROCESS-W01184                                           
012301       PERFORM S01-READ-W01184                                            
012400     END-PERFORM                                                          
012500                                                                          
012510     PERFORM S02-READ-W01160                                              
012511     PERFORM UNTIL END-OF-W01160                                          
012512       PERFORM B-PROCESS-W01160                                           
012520       PERFORM S02-READ-W01160                                            
012530     END-PERFORM                                                          
012600                                                                          
012700     PERFORM Z-FINIT                                                      
012800                                                                          
012900     MOVE ZERO TO RETURN-CODE                                             
013000     GOBACK                                                               
013100     .                                                                    
013200     EJECT                                                                
013300 A-INIT SECTION.                                                          
013401                                                                          
013402     OPEN INPUT  W01184                                                   
013410                 W01160                                                   
013501                                                                          
013502     OPEN OUTPUT W51274A                                                  
013510                 W51274B                                                  
013600                                                                          
013700     ACCEPT TODAYS-DATE  FROM DATE                                        
013810     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
014000     .                                                                    
014100     EJECT                                                                
014110 B-PROCESS-W01184 SECTION.                                                
014111     MOVE IN1-SLAG-IDDC TO WS-IDDC                                        
014112     IF NOT NDC-NA AND                                                    
014114        NOT XDC-NON-VCC-OWNED AND                                         
014115        NOT LDC-CN                                                        
014128         MOVE IN1-SLAG-IDARTNR TO UT1-IDARTNR                             
014129         MOVE IN1-SLAG-IDDC    TO UT1-IDDC                                
014130         MOVE IN1-SLAG-KVRESS  TO UT1-KVRESS                              
014131         MOVE ZEROS            TO UT1-PRARTSTD                            
014132     END-IF                                                               
014133     IF IN1-SLAG-KVRESS NOT = 0                                           
014134         PERFORM S11-WRITE-W51274A                                        
014135     END-IF                                                               
014136     .                                                                    
014137     EJECT                                                                
014138                                                                          
014140 B-PROCESS-W01160 SECTION.                                                
014150     MOVE IN2-CLAG-IDARTNR     TO UT2-IDARTNR                             
014160     MOVE WC-CDC-SE            TO UT2-IDDC                                
014170     MOVE IN2-CLAG-KVRESS      TO UT2-KVRESS                              
014180     MOVE IN2-CLAG-PRARTSTD    TO UT2-PRARTSTD                            
014191     IF IN2-CLAG-KVRESS NOT = 0                                           
014192         PERFORM S12-WRITE-W51274B                                        
014193     END-IF                                                               
014194     .                                                                    
014195     EJECT                                                                
014200 Z-FINIT SECTION.                                                         
014301     CLOSE W01184                                                         
014302           W01160                                                         
014303           W51274A                                                        
014310           W51274B                                                        
014401     SKIP2                                                                
014402     MOVE 'S' TO POSTSUM-OPKOD                                            
014410     CALL POSTSUM USING POSTSUM-PARM                                      
014500     .                                                                    
014601     EJECT                                                                
014602 S01-READ-W01184  SECTION.                                                
014603     READ W01184 INTO IN1-AREA                                            
014604     AT END                                                               
014605        MOVE HIGH-VALUE TO IN1-AREA                                       
014606        SET END-OF-W01184 TO TRUE                                         
014607                                                                          
014608     NOT AT END                                                           
014609        MOVE 'W01184'   TO POSTSUM-FDNAMN                                 
014610        MOVE 'W51274D1' TO POSTSUM-DDNAMN2                                
014612        MOVE SPACE      TO POSTSUM-TRANSTYP                               
014613        CALL POSTSUM USING POSTSUM-PARM                                   
014614     END-READ                                                             
014615     .                                                                    
014616     EJECT                                                                
014617 S02-READ-W01160  SECTION.                                                
014618     READ W01160 INTO IN2-AREA                                            
014619     AT END                                                               
014620        MOVE HIGH-VALUE TO IN2-AREA                                       
014621        SET END-OF-W01160 TO TRUE                                         
014622                                                                          
014623     NOT AT END                                                           
014624        MOVE 'W01160'   TO POSTSUM-FDNAMN                                 
014625        MOVE 'W51274D2' TO POSTSUM-DDNAMN2                                
014627        MOVE SPACE      TO POSTSUM-TRANSTYP                               
014628        CALL POSTSUM USING POSTSUM-PARM                                   
014629     END-READ                                                             
014630     .                                                                    
014701     EJECT                                                                
014702 S11-WRITE-W51274A SECTION.                                               
014703                                                                          
014704     WRITE UT1-RECORD FROM UT1-AREA                                       
014705                                                                          
014706     MOVE SPACE      TO POSTSUM-TRANSTYP                                  
014707     MOVE 'W51274A'  TO POSTSUM-FDNAMN                                    
014708     MOVE 'W51274D3' TO POSTSUM-DDNAMN2                                   
014709     CALL POSTSUM USING POSTSUM-PARM                                      
014710     .                                                                    
014711     EJECT                                                                
014712 S12-WRITE-W51274B SECTION.                                               
014713                                                                          
014714     WRITE UT2-RECORD FROM UT2-AREA                                       
014715                                                                          
014716     MOVE SPACE      TO POSTSUM-TRANSTYP                                  
014717     MOVE 'W51274B'  TO POSTSUM-FDNAMN                                    
014718     MOVE 'W51274D4' TO POSTSUM-DDNAMN2                                   
014719     CALL POSTSUM USING POSTSUM-PARM                                      
014720     .                                                                    
014900     EJECT                                                                
015000 S99-ABEND SECTION.                                                       
015100                                                                          
015201     SKIP2                                                                
015202     MOVE 'S' TO POSTSUM-OPKOD                                            
015210     CALL POSTSUM USING POSTSUM-PARM                                      
015300     CALL ABEND USING RKOD-ABEND                                          
015400     .                                                                    
015500     EJECT                                                                
