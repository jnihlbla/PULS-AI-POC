000200 ID DIVISION.                                                             
000300 PROGRAM-ID.     W5222200.                                                
000400 AUTHOR.         NILSSON LINDA.                                           
000500 DATE-WRITTEN.   03/02/20.                                                
000600 DATE-COMPILED.                                                           
000700                                                                          
000830*                                                                         
000831*    FUNCTION:                                                            
000850*        SKICKAR V.A.T. TRANSAR TILL ONDEMAND                             
000860*        FRÅN INFILEN W52221 (W5222100).                                  
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
002405*          --- V.A.T. TILL ON DEMAND - INFIL                              
002406     SELECT W52221                     ASSIGN TO W52222D1.                
002407     SKIP2                                                                
002411*          --- V.A.T. TILL ON DEMAND - LISTA                              
002420     SELECT W52222-001                 ASSIGN TO W52222D2.                
002600     EJECT                                                                
002700 DATA DIVISION.                                                           
002800     SKIP3                                                                
002900 FILE SECTION.                                                            
003001     SKIP3                                                                
003008 FD  W52221                                                               
003009     RECORDING       F                                                    
003010     BLOCK CONTAINS  0.                                                   
003011                                                                          
003012*01  -COPY W52221      -L.                                                
003013     SKIP3                                                                
003014 FD  W52222-001                                                           
003015     RECORDING       F                                                    
003016     BLOCK CONTAINS  0.                                                   
003017     SKIP2                                                                
003018 01  W52222-001-LINE             PIC X(121).                              
003100     EJECT                                                                
003200 WORKING-STORAGE SECTION.                                                 
003300                                                                          
003400 77  IDPGM                       PIC X(8)    VALUE 'W5222200'.            
003500 77  YES                         PIC X       VALUE 'J'.                   
003600 77  NOO                         PIC X       VALUE 'N'.                   
003801                                                                          
003805 77  W52221-EOF-SW               PIC X       VALUE 'N'.                   
003810     88  END-OF-W52221                       VALUE 'Y'.                   
003900     EJECT                                                                
004000 01  TODAYS-DATE                 PIC 9(6)    VALUE ZERO.                  
004100 01  FILLER REDEFINES TODAYS-DATE.                                        
004200     03  TODAYS-DATE-YEAR        PIC 9(2).                                
004300     03  TODAYS-DATE-MONTH       PIC 9(2).                                
004400     03  TODAYS-DATE-DAY         PIC 9(2).                                
004500     EJECT                                                                
004600 01  GENERAL-SUBPROGRAMS.                                                 
004700*                                                                         
004800     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
004910     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
005000     SKIP2                                                                
005100*    --- SPAR-AREA FÖR RADBRYTNING                                        
005101 01  WS-SPAR.                                                             
005102     03  SPAR-TIFAKT             PIC 9(6)  VALUE ZERO.                    
005103     03  SPAR-IDFAKT             PIC 9(7)  VALUE ZERO.                    
005110*    --- PARAMETERS TO ABEND                                              
005200                                                                          
005300 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
005400 77  RKOD-ABEND-NO-DUMP          PIC S9(4)   COMP VALUE +16.              
005500 77  RKOD-ABEND-WITH-DUMP        PIC S9(4)   COMP VALUE +1000.            
005600     SKIP2                                                                
005700 01  ERRTEXT.                                                             
005800     03  FILLER                  PIC X(8)    VALUE 'ERRTEXT'.             
005900     03  ERRTEXT-STR             PIC X(72)   VALUE SPACE.                 
006001     EJECT                                                                
006002*    --- PARAMETRAR TILL POSTSUM                                          
006003*                                                                         
006010*01  -COPY W0005   -PRE  POSTSUM-                                         
006201     EJECT                                                                
006202 01  W52221-AREA-START              PIC X(24)   VALUE                     
006203                                 'W52221-AREA-START  '.                   
006204     SKIP2                                                                
006205 01  W52221-AREA.                                                         
006206*    03  -COPY W52221     -PRE VAT-                                       
006207     EJECT                                                                
006213     EJECT                                                                
006214 01  W001-AREA-START             PIC X(24)   VALUE                        
006215                                 'W001-AREA-START  '.                     
006216     SKIP2                                                                
006217 01  W001-HELPAREAS.                                                      
006218*                                                                         
006219     03  W001-SKIP               PIC 9(3) COMP-3  VALUE 1.                
006220     03  W001-MAX-LINES-PER-PAGE                                          
006221                                 PIC 9(3)    VALUE 42.                    
006222     03  W001-LISTNR             PIC X(11)   VALUE 'W52222-001'.          
006223     03  W001-PAGECOUNTER        PIC S9(5)   COMP-3 VALUE ZERO.           
006225     EJECT                                                                
006226 01  W001-LINE.                                                           
006228     03  FILLER                  PIC X     VALUE SPACE.                   
006229     03  W001-TIAA               PIC 9(2)  VALUE ZERO.                    
006232     03  W001-TIRP               PIC 9(2)  VALUE ZERO.                    
006234     03  FILLER                  PIC X     VALUE SPACE.                   
006236     03  W001-IDLANDX3-SEND      PIC X(3)  VALUE SPACE.                   
006237     03  FILLER                  PIC X     VALUE SPACE.                   
006240     03  W001-IDLANDX3-BET       PIC X(3)  VALUE SPACE.                   
006242     03  FILLER                  PIC X     VALUE SPACE.                   
006246     03  W001-IDVAT-SEND         PIC X(17) VALUE SPACE.                   
006247     03  FILLER                  PIC X     VALUE SPACE.                   
006248     03  W001-IDVAT-REC          PIC X(17) VALUE SPACE.                   
006250     03  FILLER                  PIC X     VALUE SPACE.                   
006251     03  W001-IDDISTR            PIC 9(4)  VALUE ZERO.                    
006252     03  FILLER                  PIC X     VALUE SPACE.                   
006253     03  W001-IDKUNDNR           PIC 9(6)  VALUE ZERO.                    
006254     03  FILLER                  PIC X     VALUE SPACE.                   
006256     03  W001-TIFAKT             PIC 9(6)  VALUE ZERO.                    
006258     03  FILLER                  PIC X     VALUE SPACE.                   
006260     03  W001-IDFAKT             PIC 9(7)  VALUE ZERO.                    
006262     03  FILLER                  PIC X     VALUE SPACE.                   
006264     03  W001-SUFKTTOT-LOC       PIC +9(10)V9(2) VALUE ZERO.              
006266     03  FILLER                  PIC X     VALUE SPACE.                   
006267     03  W001-SUVAT-FAKT-LOC     PIC +9(10)V9(2) VALUE ZERO.              
006272     EJECT                                                                
006273 01  W001-HEADER1.                                                        
006274*                                                                         
006275     03  FILLER                  PIC X(3).                                
006276     03  FILLER                  PIC X(27) VALUE                          
006277                                    'VOLVO CAR CUSTOMER SERVICE '.        
006278     03  FILLER                  PIC X(65)                                
006279                                 VALUE '  V.A.T.           '.             
006280     03  FILLER                  PIC X(13) VALUE SPACE.                   
006282     03  FILLER                  PIC X(6) VALUE ' PAGE '.                 
006283     03  W001-PAGE               PIC Z(4)9.                               
006284     SKIP2                                                                
006285 01  W001-HEADER2.                                                        
006286     03  FILLER                  PIC X(5)  VALUE ' PER'.                  
006289     03  FILLER                  PIC X(4)  VALUE ' SND'.                  
006290     03  FILLER                  PIC X(4)  VALUE ' REC'.                  
006295     03  FILLER                  PIC X(18) VALUE                          
006296                                            ' VAT.REG.NO. SND  '.         
006297     03  FILLER                  PIC X(18) VALUE                          
006298                                            ' VAT.REG.NO. REC  '.         
006299     03  FILLER                  PIC X(5)  VALUE ' DIST'.                 
006300     03  FILLER                  PIC X(7)  VALUE ' DEALER'.               
006301     03  FILLER                  PIC X(15) VALUE                          
006302                                               ' DOC. DAT & ID '.         
006303     03  FILLER                  PIC X(14) VALUE ' NET AMOUNT   '.        
006304     03  FILLER                  PIC X(14) VALUE ' VAT AMOUNT   '.        
006308                                                                          
006309     03  FILLER                  PIC X(121) VALUE SPACE.                  
006310     EJECT                                                                
006320                                                                          
006400 PROCEDURE DIVISION.                                                      
006500 MAIN SECTION.                                                            
006700     SKIP2                                                                
006800                                                                          
006900     PERFORM A-INIT                                                       
007001     PERFORM S01-READ-W52221                                              
007100     PERFORM UNTIL END-OF-W52221                                          
007110       MOVE VAT-TIAAAA (3:2)   TO W001-TIAA                               
007120       MOVE VAT-TIRP           TO W001-TIRP                               
007130       MOVE VAT-IDLANDX3-SEND  TO W001-IDLANDX3-SEND                      
007140       MOVE VAT-IDLANDX3-BET   TO W001-IDLANDX3-BET                       
007150       MOVE VAT-IDVAT-SEND     TO W001-IDVAT-SEND                         
007151       MOVE VAT-IDVAT-REC      TO W001-IDVAT-REC                          
007160       MOVE VAT-IDDISTR        TO W001-IDDISTR                            
007170       MOVE VAT-IDKUNDNR       TO W001-IDKUNDNR                           
007180       MOVE VAT-TIFAKT         TO W001-TIFAKT                             
007190       MOVE VAT-IDFAKT         TO W001-IDFAKT                             
007193       MOVE VAT-SUFKTTOT-LOC   TO W001-SUFKTTOT-LOC                       
007194       MOVE VAT-SUVAT-FAKT-LOC TO W001-SUVAT-FAKT-LOC                     
007195       PERFORM S21-WRITE-W52222-001                                       
007197       MOVE VAT-TIFAKT         TO SPAR-TIFAKT                             
007198       MOVE VAT-IDFAKT         TO SPAR-IDFAKT                             
007801       PERFORM S01-READ-W52221                                            
007900     END-PERFORM                                                          
008100                                                                          
008200     PERFORM Z-FINIT                                                      
008300                                                                          
008400     MOVE ZERO TO RETURN-CODE                                             
008500     GOBACK                                                               
008600     .                                                                    
008700     EJECT                                                                
008800 A-INIT SECTION.                                                          
008901                                                                          
008902     OPEN INPUT  W52221                                                   
009002     OPEN OUTPUT W52222-001                                               
009100     SKIP2                                                                
009200*    ACCEPT TODAYS-DATE  FROM DATE                                        
009300*    MOVE TODAYS-DATE TO W001-DATE                                        
009310     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
009400     .                                                                    
009500     EJECT                                                                
009600 Z-FINIT SECTION.                                                         
009701     CLOSE W52221                                                         
009703           W52222-001                                                     
009801     SKIP2                                                                
009802     MOVE 'S' TO POSTSUM-OPKOD                                            
009810     CALL POSTSUM USING POSTSUM-PARM                                      
009900     .                                                                    
010001     EJECT                                                                
010002 S01-READ-W52221  SECTION.                                                
010003     READ W52221 INTO W52221-AREA                                         
010004     AT END                                                               
010005        MOVE HIGH-VALUE TO W52221-AREA                                    
010006        SET END-OF-W52221 TO TRUE                                         
010007                                                                          
010008     NOT AT END                                                           
010009        MOVE 'W52221' TO POSTSUM-FDNAMN                                   
010010        MOVE 'W52222D1' TO POSTSUM-DDNAMN2                                
010011        MOVE 'V.A.T.' TO POSTSUM-TRANSTYP                                 
010012        CALL POSTSUM USING POSTSUM-PARM                                   
010013     END-READ                                                             
010014     .                                                                    
010015     EJECT                                                                
010202 S21-WRITE-W52222-001  SECTION.                                           
010203                                                                          
010219     IF SPAR-TIFAKT NOT = W001-TIFAKT OR                                  
010220        SPAR-IDFAKT NOT = W001-IDFAKT                                     
010221       PERFORM S21A-WRITE-HEADERS                                         
010222     END-IF                                                               
010223     MOVE 1 TO W001-SKIP                                                  
010226     WRITE W52222-001-LINE FROM W001-LINE AFTER W001-SKIP                 
010230     SKIP2                                                                
010231     MOVE SPACE TO W001-LINE                                              
010233     .                                                                    
010234     EJECT                                                                
010235 S21A-WRITE-HEADERS SECTION.                                              
010236                                                                          
010237     ADD +1 TO W001-PAGECOUNTER                                           
010238     MOVE W001-PAGECOUNTER TO W001-PAGE                                   
010239     WRITE W52222-001-LINE FROM W001-HEADER1 AFTER PAGE                   
010240     WRITE W52222-001-LINE FROM W001-HEADER2 AFTER 2                      
010242     SKIP2                                                                
010243     MOVE 3 TO W001-SKIP                                                  
010244     .                                                                    
010250     EJECT                                                                
