000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W5614200.                                                
000300 AUTHOR.         DADHICH PRERNA.                                          
000400 DATE-WRITTEN.   18/06/15.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*                                                                         
000800*    FUNCTION:                                                            
000900*        READS FILE CREATED IN THE PROGRAM W56141 AND CREATES             
001000*        WEEKLY DATA  WITH NEW FIELD INVENTORY VALUE                      
001100*                                                                         
001200*                                                                         
001300*    ABENDCODES:                                                          
001400*        U0016 -  . . . .                                                 
001500*        U1000 -  . . . .                                                 
001600*                                                                         
001700                                                                          
001800     SKIP3                                                                
001900 ENVIRONMENT DIVISION.                                                    
002000     SKIP2                                                                
002100 INPUT-OUTPUT SECTION.                                                    
002200                                                                          
002300 FILE-CONTROL.                                                            
002400     SKIP2                                                                
002500*          --- FILE FROM  W56141                                          
002600     SELECT W56141               ASSIGN TO W56142D1.                      
002700     SKIP2                                                                
002800*          --- OUTPUT .CSV FILE                                           
002900     SELECT W56142               ASSIGN TO W56142D2.                      
003000     SKIP2                                                                
003100                                                                          
003300     EJECT                                                                
003400 DATA DIVISION.                                                           
003500     SKIP3                                                                
003600 FILE SECTION.                                                            
003700     SKIP3                                                                
003800 FD  W56141                                                               
003900     RECORDING       F                                                    
004000     BLOCK CONTAINS  0.                                                   
004100*01  -COPY W56141       -L.                                               
004200     SKIP3                                                                
004800 FD  W56142                                                               
004900     RECORDING       V                                                    
005000     BLOCK CONTAINS  0.                                                   
005100 01  UT-RECORD                   PIC X(60).                               
005200     EJECT                                                                
005300 WORKING-STORAGE SECTION.                                                 
005400                                                                          
005500 77  IDPGM                       PIC X(8)    VALUE 'W5614200'.            
005600 77  YES                         PIC X       VALUE 'J'.                   
005700 77  NOO                         PIC X       VALUE 'N'.                   
005800 77  SW-FIRST-READ               PIC X       VALUE 'Y'.                   
005801 77  SW-READ                     PIC X       VALUE 'N'.                   
005810 77  WS-PREV-IDLANDX2            PIC X(02)   VALUE SPACES.                
005820 77  WS-IDLANDX2                 PIC X(02)   VALUE SPACES.                
005900                                                                          
006000 77  W56141-EOF-SW               PIC X       VALUE 'N'.                   
006100     88  END-OF-W56141                       VALUE 'J'.                   
006200                                                                          
006600 77  WS-PREVIOUS-INVOC-VAL       PIC S9(10)V9(2) COMP-3                   
006700                                             VALUE ZERO.                  
007000 77  WS-INVOICE-VALUE            PIC S9(10)V9(2) COMP-3                   
007100                                             VALUE ZERO.                  
007101 77  WS-LANDING-COST             PIC S9(10)V9(2) COMP-3                   
007102                                             VALUE ZERO.                  
007103                                                                          
007104 77  WS-PRARTBEL-PR              PIC S9(8)V9(2) COMP-3.                   
007110                                                                          
007200     EJECT                                                                
007300 01  TODAYS-DATE                 PIC 9(6)    VALUE ZERO.                  
007400 01  FILLER REDEFINES TODAYS-DATE.                                        
007500     03  TODAYS-DATE-YEAR        PIC 9(2).                                
007600     03  TODAYS-DATE-MONTH       PIC 9(2).                                
007700     03  TODAYS-DATE-DAY         PIC 9(2).                                
007800     EJECT                                                                
007810 01  WS-PREV-KEY.                                                         
007820     03  WS-IDDC                 PIC X(02)   VALUE SPACE.                 
007840     03  WS-IDLOPNRM             PIC S9(9)   COMP-3                       
007850                                             VALUE ZERO.                  
007880     03  WS-KDPSLLOC             PIC 9(2)    VALUE ZERO.                  
007890     03  WS-KDPRODSL             PIC 9(3)    VALUE ZERO.                  
007900 01  GENERAL-SUBPROGRAMS.                                                 
008000*                                                                         
008100     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
008200     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
008300     SKIP2                                                                
008400*    --- PARAMETERS TO ABEND                                              
008500                                                                          
008600 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
008700 77  RKOD-ABEND-NO-DUMP          PIC S9(4)   COMP VALUE +16.              
008800 77  RKOD-ABEND-WITH-DUMP        PIC S9(4)   COMP VALUE +1000.            
008900     SKIP2                                                                
009000 01  ERROR-TEXT.                                                          
009100     03  FILLER                  PIC X(10)    VALUE 'ERROR-TEXT'.         
009200     03  ERROR-TEXT-STR          PIC X(72)   VALUE SPACE.                 
009300     EJECT                                                                
009400*    --- PARAMETRAR TILL POSTSUM                                          
009500*                                                                         
009600*01  -COPY W0005   -PRE  POSTSUM-                                         
009700     EJECT                                                                
009800 01  IN-AREA-START               PIC X(24)   VALUE                        
009900                                 'IN-AREA-START  '.                       
010000     SKIP2                                                                
010100                                                                          
010200*01  AREA -COPY W56141     -PRE IN-                                       
010300     EJECT                                                                
010400 01  UT-AREA-START               PIC X(24)   VALUE                        
010500                                 'UT-AREA-START  '.                       
010600     SKIP2                                                                
010710 01  UT-AREA                     PIC X(56)  VALUE SPACE.                  
010720                                                                          
010730 01  UT-CONTROL-REC1.                                                     
010740     03  FILLER                  PIC X(15)   VALUE                        
010750                                 ' ¤DAPW56141-001'.                       
010760     EJECT                                                                
010770 01  UT-CONTROL-REC2.                                                     
010780     03  FILLER                  PIC X(5)    VALUE ' ¤DAP'.               
010790     03  UT-CTL-IDLANDX2         PIC X(2)    VALUE SPACE.                 
010791     EJECT                                                                
010800                                                                          
015000 01  UT-HEADER.                                                           
015300     03  UT-H-IDDC               PIC X(02)   VALUE 'DC'.                  
015400     03  FILLER                  PIC X(01)   VALUE ';'.                   
015500     03  UT-H-IDLOPNRM           PIC X(07)   VALUE 'INVOICE'.             
015600     03  FILLER                  PIC X(01)   VALUE ';'.                   
015700     03  UT-H-KDPRODSL           PIC X(08)   VALUE 'PROD GRP'.            
015800     03  FILLER                  PIC X(01)   VALUE ';'.                   
015900     03  UT-H-KDPSLLOC           PIC X(14)   VALUE                        
016000                                             'LOCAL PROD GRP'.            
016100     03  FILLER                  PIC X(01)   VALUE ';'.                   
016200     03  UT-H-INVOICE-VALUE      PIC X(13)   VALUE                        
016300                                             'INVOICE VALUE'.             
016400     03  FILLER                  PIC X(01)   VALUE ';'.                   
016500                                                                          
016600 01  UT-RAD.                                                              
016900     03  UT-IDDC                 PIC X(02)   VALUE SPACE.                 
017000     03  FILLER                  PIC X(01)   VALUE ';'.                   
017100     03  UT-IDLOPNRM             PIC Z(8)9   VALUE ZERO.                  
017200     03  FILLER                  PIC X(01)   VALUE ';'.                   
017300     03  UT-KDPRODSL             PIC Z(2)9   VALUE ZERO.                  
017400     03  FILLER                  PIC X(01)   VALUE ';'.                   
017500     03  UT-KDPSLLOC             PIC Z9      VALUE ZERO.                  
017600     03  FILLER                  PIC X(01)   VALUE ';'.                   
017700     03  UT-INVOICE-VALUE        PIC Z(9)9.9(2) VALUE ZERO.               
017800     03  FILLER                  PIC X(01)   VALUE ';'.                   
017900     EJECT                                                                
018000                                                                          
018100 PROCEDURE DIVISION.                                                      
018200 MAIN SECTION.                                                            
018300     SKIP2                                                                
018400                                                                          
018500     PERFORM A-INIT                                                       
018800                                                                          
018810     PERFORM S01-READ-W56141                                              
018900     PERFORM UNTIL END-OF-W56141                                          
019000       MOVE 'YES' TO SW-READ                                              
022500       PERFORM B-PROCESS                                                  
022510       PERFORM S01-READ-W56141                                            
022600     END-PERFORM                                                          
022601     PERFORM BA-PROCESS-EOF                                               
022602                                                                          
022700     PERFORM Z-FINIT                                                      
022800                                                                          
022900     MOVE ZERO                   TO    RETURN-CODE                        
023000     GOBACK                                                               
023100     .                                                                    
023200     EJECT                                                                
023300 A-INIT SECTION.                                                          
023400                                                                          
023500     OPEN INPUT  W56141                                                   
023800     OPEN OUTPUT W56142                                                   
024000                                                                          
024010     ACCEPT TODAYS-DATE          FROM DATE                                
024100     MOVE IDPGM                  TO        POSTSUM-PROGNAMN               
024200     INITIALIZE UT-RAD                                                    
024300     .                                                                    
024400     EJECT                                                                
024500                                                                          
024501 B-PROCESS SECTION.                                                       
024508                                                                          
024510     COMPUTE WS-PRARTBEL-PR      ROUNDED = IN-PRARTBEL-PR                 
024511     COMPUTE WS-LANDING-COST     ROUNDED = IN-KVANTMOT *                  
024512                                           WS-PRARTBEL-PR                 
024513                                                                          
024515* CHECK FOR THE KEY VALUE *                                               
024516     IF( IN-IDDC    = WS-IDDC      AND                                    
024517         IN-IDLOPNRM= WS-IDLOPNRM  AND                                    
024518         IN-KDPSLLOC= WS-KDPSLLOC )                                       
024519       MOVE WS-LANDING-COST      TO                                       
024520                                       WS-PREVIOUS-INVOC-VAL              
024521       COMPUTE WS-INVOICE-VALUE    ROUNDED                                
024522                                     = WS-PREVIOUS-INVOC-VAL              
024523                                     + WS-INVOICE-VALUE                   
024524                                                                          
024525       MOVE WS-INVOICE-VALUE     TO    WS-LANDING-COST                    
024526     ELSE                                                                 
024527       IF (SW-FIRST-READ = 'Y')                                           
024528         MOVE 'NOO'              TO    SW-FIRST-READ                      
024529         CONTINUE                                                         
024530       ELSE                                                               
024531         MOVE WS-IDDC            TO    UT-IDDC                            
024532         MOVE WS-IDLOPNRM        TO    UT-IDLOPNRM                        
024533         MOVE WS-KDPSLLOC        TO    UT-KDPSLLOC                        
024534         MOVE WS-KDPRODSL        TO    UT-KDPRODSL                        
024535         MOVE WS-INVOICE-VALUE   TO    UT-INVOICE-VALUE                   
024537                                                                          
024538         IF WS-IDLANDX2 = WS-PREV-IDLANDX2                                
024542            CONTINUE                                                      
024543         ELSE                                                             
024544            MOVE UT-CONTROL-REC1                                          
024545                                 TO    UT-AREA                            
024546            PERFORM S11-WRITE-W56142                                      
024547            MOVE WS-IDLANDX2                                              
024548                                 TO    UT-CTL-IDLANDX2                    
024549                                       WS-PREV-IDLANDX2                   
024550            MOVE UT-CONTROL-REC2                                          
024551                                 TO    UT-AREA                            
024552            PERFORM S11-WRITE-W56142                                      
024553            MOVE UT-HEADER                                                
024554                                 TO    UT-AREA                            
024555            PERFORM S11-WRITE-W56142                                      
024556         END-IF                                                           
024557         MOVE UT-RAD             TO    UT-AREA                            
024558         PERFORM S11-WRITE-W56142                                         
024559         INITIALIZE UT-RAD                                                
024563         INITIALIZE WS-PREV-KEY                                           
024564         MOVE ZEROS              TO                                       
024565                                       WS-PREVIOUS-INVOC-VAL              
024566         MOVE ZEROS              TO                                       
024567                                       WS-INVOICE-VALUE                   
024568       END-IF                                                             
024569     END-IF                                                               
024570     MOVE IN-IDDC                TO    WS-IDDC                            
024571     MOVE IN-IDLOPNRM            TO    WS-IDLOPNRM                        
024572     MOVE IN-KDPSLLOC            TO    WS-KDPSLLOC                        
024573     MOVE IN-KDPRODSL            TO    WS-KDPRODSL                        
024574     MOVE WS-LANDING-COST        TO    WS-INVOICE-VALUE                   
024575     MOVE IN-IDLANDX2            TO    WS-IDLANDX2                        
024642                                                                          
024646     .                                                                    
024647     EJECT                                                                
024648                                                                          
024649 BA-PROCESS-EOF SECTION.                                                  
024650     IF(END-OF-W56141 AND SW-READ ='Y')                                   
024651        MOVE WS-IDDC             TO    UT-IDDC                            
024652        MOVE WS-IDLOPNRM         TO    UT-IDLOPNRM                        
024653        MOVE WS-KDPSLLOC         TO    UT-KDPSLLOC                        
024654        MOVE WS-KDPRODSL         TO    UT-KDPRODSL                        
024655        MOVE WS-LANDING-COST     TO    UT-INVOICE-VALUE                   
024656                                                                          
024657        IF WS-IDLANDX2 = WS-PREV-IDLANDX2                                 
024658          CONTINUE                                                        
024659        ELSE                                                              
024660          MOVE UT-CONTROL-REC1                                            
024661                                 TO    UT-AREA                            
024662          PERFORM S11-WRITE-W56142                                        
024663          MOVE WS-IDLANDX2                                                
024664                                 TO    UT-CTL-IDLANDX2                    
024665                                       WS-PREV-IDLANDX2                   
024666          MOVE UT-CONTROL-REC2                                            
024667                                 TO    UT-AREA                            
024668          PERFORM S11-WRITE-W56142                                        
024669          MOVE UT-HEADER                                                  
024670                                 TO    UT-AREA                            
024671          PERFORM S11-WRITE-W56142                                        
024672        END-IF                                                            
024673                                                                          
024674        MOVE UT-RAD              TO    UT-AREA                            
024675        PERFORM S11-WRITE-W56142                                          
024678     END-IF                                                               
024679     .                                                                    
024680     EJECT                                                                
024681                                                                          
024682 Z-FINIT SECTION.                                                         
024690                                                                          
024700     CLOSE W56141                                                         
024800           W56142                                                         
024900     SKIP2                                                                
025000     MOVE 'S'                    TO    POSTSUM-OPKOD                      
025100     CALL POSTSUM             USING    POSTSUM-PARM                       
025200     .                                                                    
025300     EJECT                                                                
025310                                                                          
025400 S01-READ-W56141  SECTION.                                                
025500     READ W56141                 INTO  IN-AREA                            
025600     AT END                                                               
025700        MOVE HIGH-VALUE          TO    IN-AREA                            
025800        SET END-OF-W56141        TO TRUE                                  
025900                                                                          
026000     NOT AT END                                                           
026100        MOVE 'W56141'            TO    POSTSUM-FDNAMN                     
026200        MOVE 'W56142D1'          TO    POSTSUM-DDNAMN2                    
026300        MOVE SPACES              TO    POSTSUM-TRANSTYP                   
026400        CALL POSTSUM          USING    POSTSUM-PARM                       
026500     END-READ                                                             
026600     .                                                                    
026700     EJECT                                                                
026800                                                                          
028700                                                                          
028800 S11-WRITE-W56142 SECTION.                                                
028900                                                                          
029000     WRITE UT-RECORD             FROM  UT-AREA                            
029010     MOVE SPACES                 TO    UT-AREA                            
029100                                                                          
029200     MOVE SPACES                 TO    POSTSUM-TRANSTYP                   
029300     MOVE 'W56142'               TO    POSTSUM-FDNAMN                     
029400     MOVE 'W56142D2'             TO    POSTSUM-DDNAMN2                    
029500     CALL POSTSUM             USING    POSTSUM-PARM                       
029700     .                                                                    
029800     EJECT                                                                
029810                                                                          
029900 S99-ABEND SECTION.                                                       
030000                                                                          
030100     SKIP2                                                                
030200     MOVE 'S'                    TO    POSTSUM-OPKOD                      
030300     CALL POSTSUM             USING    POSTSUM-PARM                       
030400     CALL ABEND               USING    RKOD-ABEND                         
030500     .                                                                    
