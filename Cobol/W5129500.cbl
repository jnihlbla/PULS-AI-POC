000200 ID DIVISION.                                                             
000300 PROGRAM-ID.     W5129500.                                                
000400 AUTHOR.         BARSHARANI BISHOYE.                                      
000500 DATE-WRITTEN.   20/04/07.                                                
000600 DATE-COMPILED.                                                           
000700                                                                          
000800*                                                                         
000900*    FUNCTION:                                                            
001000*        CHANGE APLHANUMERIC PART NO TO NUMERIC PART NO                   
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
002401     SKIP2                                                                
002402*          --- VCCS RECORDS AND QUANTITY AS PER DC                        
002403     SELECT W51293D                    ASSIGN TO W51295D1.                
002404     SKIP2                                                                
002405*          --- NON-VCCS RECORDS AND QUANTITY AS PER DC                    
002406     SELECT W51294D                    ASSIGN TO W51295D2.                
002407     SKIP2                                                                
002408*          --- VCCS RECORDS AND QUANTITY AS PER COMPANY                   
002409     SELECT W51295D                    ASSIGN TO W51295D3.                
002410     SKIP2                                                                
002411*          --- NON-VCCS RECORDS AND QUANTITY AS PER COMPANY               
002412     SELECT W51296D                    ASSIGN TO W51295D4.                
002413     SKIP2                                                                
002414*          --- VCCS RECORD AND QUANTITY AS PER DC                         
002415     SELECT W51293B                    ASSIGN TO W51295D5.                
002416     SKIP2                                                                
002417*          --- NON-VCCS RECORDS AND QUANTITY AS PER DC                    
002418     SELECT W51294B                    ASSIGN TO W51295D6.                
002419     SKIP2                                                                
002420*          --- VCCS RECORDS AND QUANTITY AS PER COMPANY                   
002421     SELECT W51295B                    ASSIGN TO W51295D7.                
002422     SKIP2                                                                
002423*          --- NON-VCCS RECORDS AND QUANTITY AS PER COMPANY               
002430     SELECT W51296B                    ASSIGN TO W51295D8.                
002600     EJECT                                                                
002700 DATA DIVISION.                                                           
002800     SKIP3                                                                
002900 FILE SECTION.                                                            
003001     SKIP3                                                                
003002 FD  W51293D                                                              
003003     RECORDING       F                                                    
003004     BLOCK CONTAINS  0.                                                   
003005                                                                          
003006*01  -COPY W51293      -L.                                                
003007     SKIP3                                                                
003008 FD  W51294D                                                              
003009     RECORDING       F                                                    
003010     BLOCK CONTAINS  0.                                                   
003011                                                                          
003012*01  -COPY W51293      -L.                                                
003013     SKIP3                                                                
003014 FD  W51295D                                                              
003015     RECORDING       F                                                    
003016     BLOCK CONTAINS  0.                                                   
003017                                                                          
003018*01  -COPY W51293      -L.                                                
003019     SKIP3                                                                
003020 FD  W51296D                                                              
003021     RECORDING       F                                                    
003022     BLOCK CONTAINS  0.                                                   
003023                                                                          
003024*01  -COPY W51293      -L.                                                
003025     SKIP3                                                                
003026 FD  W51293B                                                              
003027     RECORDING       F                                                    
003028     BLOCK CONTAINS  0.                                                   
003029                                                                          
003030*01  RECORD -COPY W51295 -PRE  UT1-  -L.                                  
003031     SKIP3                                                                
003032 FD  W51294B                                                              
003033     RECORDING       F                                                    
003034     BLOCK CONTAINS  0.                                                   
003035                                                                          
003036*01  RECORD -COPY W51295 -PRE  UT2-  -L.                                  
003037     SKIP3                                                                
003038 FD  W51295B                                                              
003039     RECORDING       F                                                    
003040     BLOCK CONTAINS  0.                                                   
003041                                                                          
003042*01  RECORD -COPY W51295 -PRE  UT3-  -L.                                  
003043     SKIP3                                                                
003044 FD  W51296B                                                              
003045     RECORDING       F                                                    
003046     BLOCK CONTAINS  0.                                                   
003047                                                                          
003050*01  RECORD -COPY W51295 -PRE  UT4-  -L.                                  
003100     EJECT                                                                
003200 WORKING-STORAGE SECTION.                                                 
003300                                                                          
003400 77  IDPGM                       PIC X(8)    VALUE 'W5129500'.            
003500 77  YES                         PIC X       VALUE 'J'.                   
003600 77  NOO                         PIC X       VALUE 'N'.                   
003801                                                                          
003802 77  W51293D-EOF-SW               PIC X       VALUE 'N'.                  
003803     88  END-OF-W51293D                       VALUE 'J'.                  
003804                                                                          
003805 77  W51294D-EOF-SW               PIC X       VALUE 'N'.                  
003806     88  END-OF-W51294D                       VALUE 'J'.                  
003807                                                                          
003808 77  W51295D-EOF-SW               PIC X       VALUE 'N'.                  
003809     88  END-OF-W51295D                       VALUE 'J'.                  
003810                                                                          
003811 77  W51296S-EOF-SW               PIC X       VALUE 'N'.                  
003820     88  END-OF-W51296D                       VALUE 'J'.                  
003900     EJECT                                                                
004000 01  TODAYS-DATE                 PIC 9(6)    VALUE ZERO.                  
004100 01  FILLER REDEFINES TODAYS-DATE.                                        
004200     03  TODAYS-DATE-YEAR        PIC 9(2).                                
004300     03  TODAYS-DATE-MONTH       PIC 9(2).                                
004400     03  TODAYS-DATE-DAY         PIC 9(2).                                
004500     EJECT                                                                
004510 01  W-IDARTNR-NUM1         PIC 9(9).                                     
004520 01  W-IDARTNR-ALFA1        PIC X(9).                                     
004530 01  W-IDARTNR-NUM2         PIC 9(9).                                     
004540 01  W-IDARTNR-ALFA2        PIC X(9).                                     
004550 01  W-IDARTNR-NUM3         PIC 9(9).                                     
004560 01  W-IDARTNR-ALFA3        PIC X(9).                                     
004570 01  W-IDARTNR-NUM4         PIC 9(9).                                     
004580 01  W-IDARTNR-ALFA4        PIC X(9).                                     
004600 01  GENERAL-SUBPROGRAMS.                                                 
004700*                                                                         
004800     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
004910     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
005000     SKIP2                                                                
005100*    --- PARAMETERS TO ABEND                                              
005200                                                                          
005300 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
005400 77  RKOD-ABEND-NO-DUMP          PIC S9(4)   COMP VALUE +16.              
005500 77  RKOD-ABEND-WITH-DUMP        PIC S9(4)   COMP VALUE +1000.            
005600     SKIP2                                                                
005700 01  ERROR-TEXT.                                                          
005800     03  FILLER                  PIC X(10)   VALUE 'ERROR-TEXT'.          
005900     03  ERROR-TEXT-STR          PIC X(72)   VALUE SPACE.                 
006001     EJECT                                                                
006002*    --- PARAMETRAR TILL POSTSUM                                          
006003*                                                                         
006010*01  -COPY W0005   -PRE  POSTSUM-                                         
006201     EJECT                                                                
006202 01  IN1-AREA-START              PIC X(24)   VALUE                        
006203                                 'IN1-AREA-START  '.                      
006204     SKIP2                                                                
006205                                                                          
006206*01  AREA -COPY W51293     -PRE IN1-                                      
006207     EJECT                                                                
006208 01  IN2-AREA-START              PIC X(24)   VALUE                        
006209                                 'IN2-AREA-START  '.                      
006210     SKIP2                                                                
006211                                                                          
006212*01  AREA -COPY W51293     -PRE IN2-                                      
006213     EJECT                                                                
006214 01  IN3-AREA-START              PIC X(24)   VALUE                        
006215                                 'IN3-AREA-START  '.                      
006216     SKIP2                                                                
006217                                                                          
006218*01  AREA -COPY W51293     -PRE IN3-                                      
006219     EJECT                                                                
006220 01  IN4-AREA-START              PIC X(24)   VALUE                        
006221                                 'IN4-AREA-START  '.                      
006222     SKIP2                                                                
006223                                                                          
006224*01  AREA -COPY W51293     -PRE IN4-                                      
006225     EJECT                                                                
006226 01  UT1-AREA-START              PIC X(24)   VALUE                        
006227                                 'UT1-AREA-START  '.                      
006228     SKIP2                                                                
006229                                                                          
006230*01  AREA -COPY W51295     -PRE UT1-                                      
006231     EJECT                                                                
006232 01  UT2-AREA-START              PIC X(24)   VALUE                        
006233                                 'UT2-AREA-START  '.                      
006234     SKIP2                                                                
006235                                                                          
006236*01  AREA -COPY W51295     -PRE UT2-                                      
006237     EJECT                                                                
006238 01  UT3-AREA-START              PIC X(24)   VALUE                        
006239                                 'UT3-AREA-START  '.                      
006240     SKIP2                                                                
006241                                                                          
006242*01  AREA -COPY W51295     -PRE UT3-                                      
006243     EJECT                                                                
006244 01  UT4-AREA-START              PIC X(24)   VALUE                        
006245                                 'UT4-AREA-START  '.                      
006246     SKIP2                                                                
006247                                                                          
006250*01  AREA -COPY W51295     -PRE UT4-                                      
006300     EJECT                                                                
006400 PROCEDURE DIVISION.                                                      
006500 MAIN SECTION.                                                            
006700     SKIP2                                                                
006800                                                                          
006900     PERFORM A-INIT                                                       
006910                                                                          
007000     PERFORM B-INSPECT-PART                                               
008100                                                                          
008200     PERFORM Z-FINIT                                                      
008300                                                                          
008400     MOVE ZERO TO RETURN-CODE                                             
008500     GOBACK                                                               
008600     .                                                                    
008700     EJECT                                                                
008800 A-INIT SECTION.                                                          
008901                                                                          
008902     OPEN INPUT  W51293D                                                  
008903                 W51294D                                                  
008904                 W51295D                                                  
008910                 W51296D                                                  
009001                                                                          
009002     OPEN OUTPUT W51293B                                                  
009003                 W51294B                                                  
009004                 W51295B                                                  
009010                 W51296B                                                  
009100     SKIP2                                                                
009200     ACCEPT TODAYS-DATE  FROM DATE                                        
009310     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
009400     .                                                                    
009500     EJECT                                                                
009515                                                                          
009516 B-INSPECT-PART SECTION.                                                  
009517                                                                          
009518     PERFORM S01-READ-W51293D                                             
009519     PERFORM UNTIL END-OF-W51293D                                         
009521     MOVE IN1-IDARTNR-FINANCE  TO W-IDARTNR-ALFA1                         
009522     MOVE ZERO TO TALLY                                                   
009523     INSPECT W-IDARTNR-ALFA1 TALLYING TALLY                               
009524                 FOR CHARACTERS BEFORE INITIAL SPACE                      
009525     IF TALLY = ZERO                                                      
009526       MOVE ZERO                TO W-IDARTNR-NUM1                         
009527     ELSE                                                                 
009528       MOVE W-IDARTNR-ALFA1(1:TALLY)                                      
009529                                TO W-IDARTNR-NUM1                         
009530     END-IF                                                               
009531     PERFORM C-MOVE-OUTPUT-W51293B                                        
009533       PERFORM S11-WRITE-W51293B                                          
009537     PERFORM S01-READ-W51293D                                             
009538     END-PERFORM                                                          
009539                                                                          
009540     PERFORM S02-READ-W51294D                                             
009541     PERFORM UNTIL END-OF-W51294D                                         
009542     MOVE IN2-IDARTNR-FINANCE  TO W-IDARTNR-ALFA2                         
009543                                                                          
009544     MOVE ZERO TO TALLY                                                   
009545     INSPECT W-IDARTNR-ALFA2 TALLYING TALLY                               
009546                 FOR CHARACTERS BEFORE INITIAL SPACE                      
009547     IF TALLY = ZERO                                                      
009548       MOVE ZERO                TO W-IDARTNR-NUM2                         
009549     ELSE                                                                 
009550       MOVE W-IDARTNR-ALFA2(1:TALLY)                                      
009551                                TO W-IDARTNR-NUM2                         
009552     END-IF                                                               
009553     PERFORM C-MOVE-OUTPUT-W51294B                                        
009554     PERFORM S12-WRITE-W51294B                                            
009555     PERFORM S02-READ-W51294D                                             
009556     END-PERFORM                                                          
009557                                                                          
009558     PERFORM S03-READ-W51295D                                             
009559     PERFORM UNTIL END-OF-W51295D                                         
009560     MOVE IN3-IDARTNR-FINANCE  TO W-IDARTNR-ALFA3                         
009561                                                                          
009562     MOVE ZERO TO TALLY                                                   
009563     INSPECT W-IDARTNR-ALFA3 TALLYING TALLY                               
009564                 FOR CHARACTERS BEFORE INITIAL SPACE                      
009565     IF TALLY = ZERO                                                      
009566       MOVE ZERO                TO W-IDARTNR-NUM3                         
009567     ELSE                                                                 
009568       MOVE W-IDARTNR-ALFA3(1:TALLY)                                      
009569                                TO W-IDARTNR-NUM3                         
009570     END-IF                                                               
009571     PERFORM C-MOVE-OUTPUT-W51295B                                        
009572     PERFORM S13-WRITE-W51295B                                            
009573     PERFORM S03-READ-W51295D                                             
009574     END-PERFORM                                                          
009575                                                                          
009576     PERFORM S04-READ-W51296D                                             
009577     PERFORM UNTIL END-OF-W51296D                                         
009578     MOVE IN4-IDARTNR-FINANCE  TO W-IDARTNR-ALFA4                         
009579                                                                          
009580     MOVE ZERO TO TALLY                                                   
009581     INSPECT W-IDARTNR-ALFA4 TALLYING TALLY                               
009582                 FOR CHARACTERS BEFORE INITIAL SPACE                      
009583     IF TALLY = ZERO                                                      
009584       MOVE ZERO                TO W-IDARTNR-NUM4                         
009585     ELSE                                                                 
009586       MOVE W-IDARTNR-ALFA4(1:TALLY)                                      
009587                                TO W-IDARTNR-NUM4                         
009588     END-IF                                                               
009589     PERFORM C-MOVE-OUTPUT-W51296B                                        
009590     PERFORM S14-WRITE-W51296B                                            
009591     PERFORM S04-READ-W51296D                                             
009592     END-PERFORM                                                          
009594     .                                                                    
009595     EJECT                                                                
009596 C-MOVE-OUTPUT-W51293B SECTION.                                           
009597     MOVE W-IDARTNR-NUM1      TO UT1-IDARTNR                              
009598     MOVE IN1-IDLEGSEL        TO UT1-IDLEGSEL                             
009599     MOVE IN1-IDDC            TO UT1-IDDC                                 
009600     MOVE IN1-SULEVANT        TO UT1-SULEVANT                             
009601     .                                                                    
009610     EJECT                                                                
009620 C-MOVE-OUTPUT-W51294B SECTION.                                           
009630     MOVE W-IDARTNR-NUM2    TO UT2-IDARTNR                                
009640     MOVE IN2-IDLEGSEL        TO UT2-IDLEGSEL                             
009650     MOVE IN2-IDDC            TO UT2-IDDC                                 
009660     MOVE IN2-SULEVANT        TO UT2-SULEVANT                             
009670     .                                                                    
009680     EJECT                                                                
009690 C-MOVE-OUTPUT-W51295B SECTION.                                           
009691     MOVE W-IDARTNR-NUM3      TO UT3-IDARTNR                              
009692     MOVE IN3-IDLEGSEL        TO UT3-IDLEGSEL                             
009693     MOVE IN3-IDDC            TO UT3-IDDC                                 
009694     MOVE IN3-SULEVANT        TO UT3-SULEVANT                             
009695     .                                                                    
009696     EJECT                                                                
009697 C-MOVE-OUTPUT-W51296B SECTION.                                           
009698     MOVE W-IDARTNR-NUM4      TO UT4-IDARTNR                              
009699     MOVE IN4-IDLEGSEL        TO UT4-IDLEGSEL                             
009700     MOVE IN4-IDDC            TO UT4-IDDC                                 
009701     MOVE IN4-SULEVANT        TO UT4-SULEVANT                             
009702     .                                                                    
009703     EJECT                                                                
009704 Z-FINIT SECTION.                                                         
009705     CLOSE W51293D                                                        
009706           W51294D                                                        
009707           W51295D                                                        
009708           W51296D                                                        
009709           W51293B                                                        
009710           W51294B                                                        
009711           W51295B                                                        
009720           W51296B                                                        
009801     SKIP2                                                                
009802     MOVE 'S' TO POSTSUM-OPKOD                                            
009810     CALL POSTSUM USING POSTSUM-PARM                                      
009900     .                                                                    
010001     EJECT                                                                
010002 S01-READ-W51293D SECTION.                                                
010003     READ W51293D INTO IN1-AREA                                           
010004     AT END                                                               
010005        MOVE HIGH-VALUE TO IN1-AREA                                       
010006        SET END-OF-W51293D TO TRUE                                        
010007                                                                          
010008     NOT AT END                                                           
010009        MOVE 'W51293D' TO POSTSUM-FDNAMN                                  
010010        MOVE 'W51295D1' TO POSTSUM-DDNAMN2                                
010012        MOVE 'IN1' TO POSTSUM-TRANSTYP                                    
010013        CALL POSTSUM USING POSTSUM-PARM                                   
010014     END-READ                                                             
010015     .                                                                    
010016     EJECT                                                                
010017 S02-READ-W51294D  SECTION.                                               
010018     READ W51294D INTO IN2-AREA                                           
010019     AT END                                                               
010020        MOVE HIGH-VALUE TO IN2-AREA                                       
010021        SET END-OF-W51294D TO TRUE                                        
010022                                                                          
010023     NOT AT END                                                           
010024        MOVE 'W51294D' TO POSTSUM-FDNAMN                                  
010025        MOVE 'W51295D2' TO POSTSUM-DDNAMN2                                
010027        MOVE 'IN2' TO POSTSUM-TRANSTYP                                    
010028        CALL POSTSUM USING POSTSUM-PARM                                   
010029     END-READ                                                             
010030     .                                                                    
010031     EJECT                                                                
010032 S03-READ-W51295D  SECTION.                                               
010033     READ W51295D INTO IN3-AREA                                           
010034     AT END                                                               
010035        MOVE HIGH-VALUE TO IN3-AREA                                       
010036        SET END-OF-W51295D TO TRUE                                        
010037                                                                          
010038     NOT AT END                                                           
010039        MOVE 'W51295D' TO POSTSUM-FDNAMN                                  
010040        MOVE 'W51295D3' TO POSTSUM-DDNAMN2                                
010042        MOVE 'IN3' TO POSTSUM-TRANSTYP                                    
010043        CALL POSTSUM USING POSTSUM-PARM                                   
010044     END-READ                                                             
010045     .                                                                    
010046     EJECT                                                                
010047 S04-READ-W51296D  SECTION.                                               
010048     READ W51296D INTO IN4-AREA                                           
010049     AT END                                                               
010050        MOVE HIGH-VALUE TO IN4-AREA                                       
010051        SET END-OF-W51296D TO TRUE                                        
010052                                                                          
010053     NOT AT END                                                           
010054        MOVE 'W51296D' TO POSTSUM-FDNAMN                                  
010055        MOVE 'W51295D4' TO POSTSUM-DDNAMN2                                
010057        MOVE 'IN4' TO POSTSUM-TRANSTYP                                    
010058        CALL POSTSUM USING POSTSUM-PARM                                   
010059     END-READ                                                             
010060     .                                                                    
010101     EJECT                                                                
010102 S11-WRITE-W51293B SECTION.                                               
010103                                                                          
010104     WRITE UT1-RECORD FROM UT1-AREA                                       
010105                                                                          
010106     MOVE 'UT1' TO POSTSUM-TRANSTYP                                       
010107     MOVE 'W51293B' TO POSTSUM-FDNAMN                                     
010108     MOVE 'W51295D5' TO POSTSUM-DDNAMN2                                   
010109     CALL POSTSUM USING POSTSUM-PARM                                      
010110     .                                                                    
010111     EJECT                                                                
010112 S12-WRITE-W51294B SECTION.                                               
010113                                                                          
010114     WRITE UT2-RECORD FROM UT2-AREA                                       
010115                                                                          
010116     MOVE 'UT2' TO POSTSUM-TRANSTYP                                       
010117     MOVE 'W51294B' TO POSTSUM-FDNAMN                                     
010118     MOVE 'W51295D6' TO POSTSUM-DDNAMN2                                   
010119     CALL POSTSUM USING POSTSUM-PARM                                      
010120     .                                                                    
010121     EJECT                                                                
010122 S13-WRITE-W51295B SECTION.                                               
010123                                                                          
010124     WRITE UT3-RECORD FROM UT3-AREA                                       
010125                                                                          
010126     MOVE 'UT3' TO POSTSUM-TRANSTYP                                       
010127     MOVE 'W51295B' TO POSTSUM-FDNAMN                                     
010128     MOVE 'W51295D7' TO POSTSUM-DDNAMN2                                   
010129     CALL POSTSUM USING POSTSUM-PARM                                      
010130     .                                                                    
010131     EJECT                                                                
010132 S14-WRITE-W51296B SECTION.                                               
010133                                                                          
010134     WRITE UT4-RECORD FROM UT4-AREA                                       
010135                                                                          
010136     MOVE 'UT4' TO POSTSUM-TRANSTYP                                       
010137     MOVE 'W51296B' TO POSTSUM-FDNAMN                                     
010138     MOVE 'W51295D8' TO POSTSUM-DDNAMN2                                   
010139     CALL POSTSUM USING POSTSUM-PARM                                      
010140     .                                                                    
010300     EJECT                                                                
010400 S99-ABEND SECTION.                                                       
010500                                                                          
010601     SKIP2                                                                
010602     MOVE 'S' TO POSTSUM-OPKOD                                            
010610     CALL POSTSUM USING POSTSUM-PARM                                      
010700     CALL ABEND USING RKOD-ABEND                                          
010800     .                                                                    
