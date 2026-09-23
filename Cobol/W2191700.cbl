001100 ID DIVISION.                                                             
001200 PROGRAM-ID.     W2191700.                                                
001300 AUTHOR.         STENING INGER.                                           
001400 DATE-WRITTEN.   16/11/14.                                                
001500 DATE-COMPILED.                                                           
001600                                                                          
001700*    FUNCTION:                                                            
001800*        REPORT TO LIST ALL CAMPAIGNS WITH RESERVED                       
001900*        CAMPAIGN MATERIAL                                                
002000*                                                                         
002110*        THE PROGRAM READS     WDK6                                       
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
003402*          --- PARTS FROM CAMPAIGN DATABASE WDM211                        
003404     SELECT W21901                     ASSIGN TO W21917D1.                
003405     SKIP2                                                                
003406*          --- CAMPAIGNS WITH RESERVED CAMPAIGN MATERIAL                  
003410     SELECT W21917                     ASSIGN TO W21917D2.                
003600     EJECT                                                                
003700 DATA DIVISION.                                                           
003800     SKIP2                                                                
003900 FILE SECTION.                                                            
004001     SKIP3                                                                
004002 FD  W21901                                                               
004003     RECORDING       F                                                    
004004     BLOCK CONTAINS  0.                                                   
004005                                                                          
004006*01  -COPY W219001      -L.                                               
004007     SKIP3                                                                
004008 FD  W21917                                                               
004009     RECORDING       V                                                    
004010     BLOCK CONTAINS  0.                                                   
004011                                                                          
004020 01  OUT-RECORD                  PIC X(80).                               
004100     EJECT                                                                
004200 WORKING-STORAGE SECTION.                                                 
004300                                                                          
004400 77  IDPGM                       PIC X(8)    VALUE 'W2191700'.            
004700 77  CURRENT-SECTION             PIC X(80)   VALUE SPACE.                 
004800 77  DBS-SECTION                 PIC X(80)   VALUE SPACE.                 
004801 77  YES                         PIC X       VALUE 'J'.                   
004802 77  NOO                         PIC X       VALUE 'N'.                   
004803                                                                          
004804 77  W21901-EOF-SW               PIC X       VALUE 'N'.                   
004810     88  END-OF-W21901                       VALUE 'J'.                   
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
007000     03  FILLER                  PIC X(8)    VALUE 'ERR-TEXT'.            
007100     03  ERROR-TEXT-STR          PIC X(72)   VALUE SPACE.                 
007201     EJECT                                                                
007202*    --- PARAMETRAR TILL POSTSUM                                          
007203*                                                                         
007210*01  -COPY W0005   -PRE  POSTSUM-                                         
007401     EJECT                                                                
007402 01  IN-AREA-START               PIC X(24)   VALUE                        
007403                                 'IN-AREA-START  '.                       
007404     SKIP2                                                                
007405                                                                          
007406*01  AREA -COPY W219001     -PRE IN-                                      
007407     EJECT                                                                
007417 01  OUT-AREA-HEAD-X             PIC X(24)   VALUE                        
007418                                 'OUT-AREA-HEAD'.                         
007419 01  OUT-AREA-HEAD.                                                       
007420     03  OUT-IDDC-HEAD           PIC X(02) VALUE 'DC'.                    
007421     03  FILLER                  PIC X(1)  VALUE ';'.                     
007422     03  OUT-IDKAMPRF-HEAD       PIC X(08) VALUE 'CAMP-REF'.              
007423     03  FILLER                  PIC X(1)  VALUE ';'.                     
007424     03  OUT-IDARTNR-HEAD        PIC X(07) VALUE 'PART.NO'.               
007425     03  FILLER                  PIC X(1)  VALUE ';'.                     
007426     03  OUT-IDANSK-HEAD         PIC X(04) VALUE 'PROC'.                  
007427     03  FILLER                  PIC X(1)  VALUE ';'.                     
007428     03  OUT-KVRESS-KAMP-HEAD    PIC X(07) VALUE 'RES-QTY'.               
007429     03  FILLER                  PIC X(1)  VALUE ';'.                     
007430     03  OUT-KVRESS-REMAIN-HEAD  PIC X(10)                                
007431                                           VALUE 'REMAIN-RES'.            
007432     03  FILLER                  PIC X(1)  VALUE ';'.                     
007433     03  OUT-TISTADAT-HEAD       PIC X(05) VALUE 'START'.                 
007434     03  FILLER                  PIC X(1)  VALUE ';'.                     
007435     03  OUT-TISTODAT-HEAD       PIC X(04) VALUE 'STOP'.                  
007436                                                                          
007437 01  OUT-AREA-LINE-X             PIC X(24)   VALUE                        
007438                                 'OUT-AREA-LINE'.                         
007439 01  OUT-AREA-LINE.                                                       
007440     03  OUT-IDDC                PIC X(02).                               
007441     03  FILLER                  PIC X(1)  VALUE ';'.                     
007442     03  OUT-IDKAMPRF            PIC Z(6)9.                               
007443     03  FILLER                  PIC X(1)  VALUE ';'.                     
007444     03  OUT-IDARTNR             PIC Z(8)9.                               
007445     03  FILLER                  PIC X(1)  VALUE ';'.                     
007446     03  OUT-IDANSK              PIC Z(2)9.                               
007447     03  FILLER                  PIC X(1)  VALUE ';'.                     
007448     03  OUT-KVRESS-KAMP         PIC Z(6)9.                               
007449     03  FILLER                  PIC X(1)  VALUE ';'.                     
007450     03  OUT-KVRESS-REMAIN       PIC -(7)9.                               
007451     03  FILLER                  PIC X(1)  VALUE ';'.                     
007452     03  OUT-TISTADAT            PIC Z(06).                               
007453     03  FILLER                  PIC X(1)  VALUE ';'.                     
007454     03  OUT-TISTODAT            PIC Z(06).                               
007460                                                                          
007600*    --- AREAS FOR IMS-SECTIONS                                           
007700*                                                                         
007800     EJECT                                                                
007900 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
008000     SKIP3                                                                
008100 01  KEYS-FOR-DLI.                                                        
008201     03  W-IDARTNR-X.                                                     
008210         05  W-IDARTNR           PIC S9(9)   VALUE ZERO COMP-3.           
008300     SKIP2                                                                
008400*    --- STATUS-KOD FRÅN IMS                                              
008500 01  STATUS-WS                   PIC XX.                                  
008600     88  SEGMENT-FOUND                       VALUE '  '.                  
008700     88  SEGMENT-FOUND-EXISTS                VALUE 'II'.                  
008800     88  SEGMENT-MISSING                     VALUE 'GE'.                  
008900     SKIP2                                                                
009000 01  GOOD-STATUSCODES.                                                    
009100     03  GOOD-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
009200     SKIP3                                                                
009300 01  SSA1                        PIC X(64).                               
009400 01  SSA2                        PIC X(64).                               
009500     EJECT                                                                
009600*    --- IMS FUNCTION CODES                                               
009700*01  -COPY W0003                                                          
009800     EJECT                                                                
010000*    ---  DLI INPUT-OUTPUT AREA                                           
010101 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK611'.                      
010102 01  DLI-IO-WDK611.                                                       
010110*    03  -COPY WDK611                                                     
010400     EJECT                                                                
010500 LINKAGE SECTION.                                                         
010701                                                                          
010702*01  -COPY W0008  -PRE WDK6-                                              
010710     05  FILLER                  PIC X.                                   
010800     EJECT                                                                
010901 PROCEDURE DIVISION  USING WDK6-PCB.                                      
010902 MAIN SECTION.                                                            
010910     ENTRY 'DLITCBL' USING WDK6-PCB.                                      
011200                                                                          
011300     PERFORM A-INIT                                                       
011400                                                                          
011510     PERFORM S01-READ-W21901                                              
011520     IF NOT END-OF-W21901                                                 
011530        PERFORM S11-WRITE-W21917-HEAD                                     
011540     END-IF                                                               
011550                                                                          
011600     PERFORM UNTIL END-OF-W21901                                          
011700                                                                          
011800       PERFORM B-CREATE-RESERVED-CAMPAIGN                                 
012200                                                                          
012310       PERFORM S01-READ-W21901                                            
012400     END-PERFORM                                                          
012500                                                                          
012600                                                                          
012700     PERFORM Z-FINIT                                                      
012800                                                                          
012900     MOVE ZERO TO RETURN-CODE                                             
013000     GOBACK                                                               
013100     .                                                                    
013200     EJECT                                                                
013300 A-INIT SECTION.                                                          
013401                                                                          
013410     OPEN INPUT  W21901                                                   
013501                                                                          
013510     OPEN OUTPUT W21917                                                   
013600                                                                          
013700     ACCEPT TODAYS-DATE  FROM DATE                                        
013810     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
014000     .                                                                    
014100     EJECT                                                                
014110 B-CREATE-RESERVED-CAMPAIGN SECTION.                                      
014126                                                                          
014130     IF (IN-KVRESS-KAMP - IN-KVBEART-KUND) > +0                           
014133        MOVE IN-IDDC              TO OUT-IDDC                             
014134        MOVE IN-IDKAMPRF          TO OUT-IDKAMPRF                         
014135        MOVE IN-IDARTNR           TO OUT-IDARTNR                          
014136        MOVE IN-IDARTNR           TO W-IDARTNR                            
014137        PERFORM IMS-GU-WDK611                                             
014138        IF SEGMENT-FOUND                                                  
014139           MOVE CLAG-IDANSK       TO OUT-IDANSK                           
014140        ELSE                                                              
014141           MOVE +0                TO OUT-IDANSK                           
014142        END-IF                                                            
014143        MOVE IN-KVRESS-KAMP       TO OUT-KVRESS-KAMP                      
014144        COMPUTE OUT-KVRESS-REMAIN =                                       
014145                IN-KVRESS-KAMP - IN-KVBEART-KUND                          
014147        MOVE IN-TISTADAT          TO OUT-TISTADAT                         
014148        MOVE IN-TISTODAT          TO OUT-TISTODAT                         
014149                                                                          
014150        PERFORM S11-WRITE-W21917-LINE                                     
014151     END-IF                                                               
014152     .                                                                    
014160     EJECT                                                                
014200 Z-FINIT SECTION.                                                         
014301     CLOSE W21901                                                         
014310           W21917                                                         
014401     SKIP2                                                                
014402     MOVE 'S' TO POSTSUM-OPKOD                                            
014410     CALL POSTSUM USING POSTSUM-PARM                                      
014500     .                                                                    
014601     EJECT                                                                
014602 S01-READ-W21901  SECTION.                                                
014603                                                                          
014604     READ W21901 INTO IN-AREA                                             
014605     AT END                                                               
014606        MOVE HIGH-VALUE TO IN-AREA                                        
014607        SET END-OF-W21901 TO TRUE                                         
014608                                                                          
014609     NOT AT END                                                           
014610        MOVE 'W21901'   TO POSTSUM-FDNAMN                                 
014611        MOVE 'W21917D1' TO POSTSUM-DDNAMN2                                
014613        MOVE SPACE      TO POSTSUM-TRANSTYP                               
014614        CALL POSTSUM USING POSTSUM-PARM                                   
014615     END-READ                                                             
014620     .                                                                    
014701     EJECT                                                                
014702 S11-WRITE-W21917-HEAD SECTION.                                           
014703                                                                          
014704     WRITE OUT-RECORD FROM OUT-AREA-HEAD                                  
014705                                                                          
014706     MOVE SPACE      TO POSTSUM-TRANSTYP                                  
014707     MOVE 'W21917'   TO POSTSUM-FDNAMN                                    
014708     MOVE 'W21917D2' TO POSTSUM-DDNAMN2                                   
014709     CALL POSTSUM USING POSTSUM-PARM                                      
014710     .                                                                    
014900     EJECT                                                                
014910 S11-WRITE-W21917-LINE SECTION.                                           
014920                                                                          
014930     WRITE OUT-RECORD FROM OUT-AREA-LINE                                  
014940                                                                          
014950     MOVE SPACE      TO POSTSUM-TRANSTYP                                  
014960     MOVE 'W21917'   TO POSTSUM-FDNAMN                                    
014970     MOVE 'W21917D2' TO POSTSUM-DDNAMN2                                   
014980     CALL POSTSUM USING POSTSUM-PARM                                      
014990     .                                                                    
014991     EJECT                                                                
015000 S99-ABEND SECTION.                                                       
015100                                                                          
015201     SKIP2                                                                
015202     MOVE 'S' TO POSTSUM-OPKOD                                            
015210     CALL POSTSUM USING POSTSUM-PARM                                      
015300     CALL ABEND USING RKOD-ABEND                                          
015400     .                                                                    
015500     EJECT                                                                
015600* --- IMS SECTIONS  ---                                                   
015700                                                                          
015801     EJECT                                                                
015802 IMS-GU-WDK611   SECTION.                                                 
015803     MOVE 'IMS-GU-WDK611      ' TO DBS-SECTION                            
015804                                                                          
015805     STRING 'WDK601  (IDARTNR  =' W-IDARTNR-X ')'                         
015806          DELIMITED BY SIZE INTO SSA1                                     
015807     MOVE 'WDK611'         TO SSA2                                        
015808     MOVE '  '             TO GOOD-STATUSCODES                            
015809     CALL CBLTDLI USING GU WDK6-PCB DLI-IO-WDK611 SSA1 SSA2               
015810     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
015811     PERFORM IMS-STATUSCHECK                                              
015812     .                                                                    
015900     EJECT                                                                
016000 IMS-STATUSCHECK SECTION.                                                 
016100                                                                          
016200     SET STATUS-IX TO 1                                                   
016300     SEARCH GOOD-STATUS                                                   
016400       AT END                                                             
016500         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
016600           DELIMITED BY SIZE INTO ERROR-TEXT                              
016700         DISPLAY ERROR-TEXT                                               
016800         CALL FELLOG                                                      
016900       WHEN GOOD-STATUS (STATUS-IX) = STATUS-WS                           
017000         CONTINUE                                                         
017100     END-SEARCH                                                           
017200     .                                                                    
