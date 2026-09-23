000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W4281700.                                                
000300 AUTHOR.         ÖSTRÖM ELEONOR.                                          
000400 DATE-WRITTEN.   08/08/19.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700                                                                          
000800*    FUNKTION:                                                            
000900*        PROGRAMMET SORTERAR INFIL FRÅN W4281600 SÅ ATT UTSKRIFT          
001000*        SKER SORTERAT PÅ DC OCH LAND.                                    
001100*        PROGRAMMET INGÅR I RUTIN W428V1.VECKANS RADER.                   
001200*        LÄSER IN W42811-FILEN FRÅN VECKORUTIN W428V1 OCH                 
001300*        SKICKAR RADER TILL DISTR. OCH PRINT VIA WZ01.                    
001400*                                                                         
001500*        E'TRACKER. 6785206  DATED 2008-06-18                             
001600*        E-TRACKER 10143271 CHINA WAREHOUSE PROJECT-1 2011-12-08          
001610*                                                                         
001700*                                                                         
001800                                                                          
001900     SKIP3                                                                
002000 ENVIRONMENT DIVISION.                                                    
002100     SKIP2                                                                
002200 INPUT-OUTPUT SECTION.                                                    
002300                                                                          
002400 FILE-CONTROL.                                                            
002500     SKIP2                                                                
002600*          --- PERIODENS INLAGDA RADER KOD 72 HOS LDC                     
002700     SELECT W42816                     ASSIGN TO W42817D1.                
002800     EJECT                                                                
002900 DATA DIVISION.                                                           
003000     SKIP3                                                                
003100 FILE SECTION.                                                            
003200     SKIP3                                                                
003300 FD  W42816                                                               
003400     RECORDING       F                                                    
003500     BLOCK CONTAINS  0.                                                   
003600                                                                          
003700*01  -COPY W42816      -L.                                                
003800     EJECT                                                                
003900 WORKING-STORAGE SECTION.                                                 
004000                                                                          
004100 77  IDPGM                       PIC X(8)    VALUE 'W4281500'.            
004200 77  JA                          PIC X       VALUE 'J'.                   
004300 77  NEJ                         PIC X       VALUE 'N'.                   
004400 77  SPAR-IDLANDX2               PIC X(2)    VALUE SPACE.                 
004410 77  SPAR-IDFTG                  PIC 9(2)    VALUE ZERO.                  
004500 77  WS-YYMMDDHHMM               PIC 9(10)   VALUE ZERO.                  
004600                                                                          
004700 77  ERROR-TEXT                  PIC X(80)   VALUE SPACE.                 
004800 77  KDRC-DISPLAY                PIC Z(5).                                
004900     SKIP2                                                                
005000 01  FELTEXT.                                                             
006000     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
006100     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
006200                                                                          
006300 77  W42816-EOF-SW               PIC X       VALUE 'N'.                   
006400     88  END-OF-W42816                       VALUE 'J'.                   
006500     EJECT                                                                
006600 01  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
006700 01  FILLER REDEFINES DAGENS-DATUM.                                       
006800     03  DAGENS-DATUM-AAR        PIC 9(2).                                
006900     03  DAGENS-DATUM-MAANAD     PIC 9(2).                                
007000     03  DAGENS-DATUM-DAG        PIC 9(2).                                
007100     EJECT                                                                
007101                                                                          
007102*01  -COPY WWIDFTG                                                        
007110     EJECT                                                                
007120                                                                          
007200 01  DYNAMISKA-SUBPROGRAM.                                                
007300*                                                                         
007400     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
007500     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
007600     03  ABEND                   PIC X(8)    VALUE 'ABEND   '.            
007700     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
007800     03  WZ01SEND                PIC X(8)    VALUE 'WZ01SEND'.            
007900     EJECT                                                                
008000*    --- PARAMETRAR TILL POSTSUM                                          
008100*                                                                         
008200*01  -COPY W0005   -PRE  POSTSUM-                                         
008300     EJECT                                                                
008400*    --- PARAMETERS TO ABEND                                              
008500                                                                          
008600 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
008700 77  RKOD-ABEND-NO-DUMP          PIC S9(4)   COMP VALUE +16.              
008800 77  RKOD-ABEND-WITH-DUMP        PIC S9(4)   COMP VALUE +1000.            
008900                                                                          
009000     EJECT                                                                
009100                                                                          
009200 01  FILLER                      PIC X(16)   VALUE 'SEND-CONTROL'.        
009300*01  -COPY WZ01SEND                                                       
009400                                                                          
009500     EJECT                                                                
009600 01  FILLER                      PIC X(16)   VALUE 'HDR-AREA'.            
009700 01  HDR-AREA.                                                            
009800*    03  -COPY WZ01REQU  -PRE HDR-                                        
009900*    03  -COPY WZ04HDR                                                    
010000*                                                                         
010100 01  FILLER                      PIC X(16)   VALUE 'DOC-AREA'.            
010200 01  DOC-AREA.                                                            
010300*    03  -COPY W428121                                                    
010400*                                                                         
010500     EJECT                                                                
010700 01  IN-AREA-START               PIC X(24)   VALUE                        
010800                                             'IN-AREA-START'.             
010900     SKIP2                                                                
011000                                                                          
011100*01  AREA -COPY W42816     -PRE IN-                                       
011200*                                                                         
011300     EJECT                                                                
011400 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
011500     SKIP3                                                                
011600*    --- STATUS-KOD FRÅN IMS                                              
011700 01  STATUS-WS                   PIC XX.                                  
011800     88  SEGMENT-FINNS                       VALUE '  '.                  
011900     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
012000     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
012100     88  SEGMENT-SLUT                        VALUE 'GB'.                  
012200     88  IMS-EJ-OK                           VALUE 'XD'.                  
012300     SKIP2                                                                
012400 01  GODK-STATUSKODER.                                                    
012500     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
012600     SKIP3                                                                
012700 01  SSA1                        PIC X(64).                               
012800 01  SSA2                        PIC X(64).                               
012900     EJECT                                                                
013000     EJECT                                                                
013100 LINKAGE SECTION.                                                         
013200                                                                          
013300*01  -COPY W0009   -PRE MSG-                                              
013400     EJECT                                                                
013500 01  DISTRDOC-PCB                PIC X.                                   
013600     EJECT                                                                
013700                                                                          
013800 PROCEDURE DIVISION  USING MSG-PCB DISTRDOC-PCB.                          
013900 MAIN SECTION.                                                            
014000     ENTRY 'DLITCBL' USING MSG-PCB DISTRDOC-PCB.                          
014100                                                                          
014200     SKIP2                                                                
014300     PERFORM A-INIT                                                       
014400                                                                          
014500     PERFORM S01-LAES-W42816                                              
014600                                                                          
014700     PERFORM UNTIL END-OF-W42816                                          
014800       PERFORM S05-OPEN-DAP-SEND                                          
014801       MOVE IN-IDFTG TO WS-IDFTG                                          
014810       IF IDFTG-PV                                                        
014820       OR IDFTG-CN                                                        
014830       OR IDFTG-IN                                                        
014840       OR IDFTG-KR                                                        
014850       OR IDFTG-TR                                                        
014860       OR IDFTG-BR                                                        
014870       OR IDFTG-MX                                                        
014880       OR IDFTG-ZA                                                        
014890       OR IDFTG-MY                                                        
014891       OR IDFTG-TH                                                        
014892       OR IDFTG-TW                                                        
014900         PERFORM S02-FLYTTA-HEADER-DATA                                   
014970       END-IF                                                             
015000       PERFORM S06-PUT-DAP-HEADER                                         
015100                                                                          
015200       MOVE IN-IDLANDX2   TO SPAR-IDLANDX2                                
015210       MOVE IN-IDFTG      TO SPAR-IDFTG                                   
015300                                                                          
015400       PERFORM UNTIL END-OF-W42816 OR (IN-IDFTG NOT = SPAR-IDFTG)         
015500                                                                          
015600           IF IN-IDLANDX2 NOT = SPAR-IDLANDX2                             
015700              MOVE ALL '+'      TO DOC-AREA                               
015800              MOVE 'LINE'       TO DOC-IDAFPRCD                           
015900              PERFORM S07-PUT-DOC                                         
016000              MOVE IN-IDLANDX2  TO SPAR-IDLANDX2                          
016100           END-IF                                                         
016200                                                                          
016300           PERFORM C-FLYTTA-DATA                                          
016400           PERFORM S07-PUT-DOC                                            
016500                                                                          
016600           PERFORM S01-LAES-W42816                                        
016700                                                                          
016800       END-PERFORM                                                        
016900                                                                          
017000       PERFORM S08-CLOSE-DAP-SEND                                         
017100                                                                          
017200     END-PERFORM                                                          
017300     PERFORM Z-FINIT                                                      
017400                                                                          
017500     MOVE ZERO TO RETURN-CODE                                             
017600     GOBACK                                                               
017700     .                                                                    
017800     EJECT                                                                
017900 A-INIT SECTION.                                                          
018000     SKIP2                                                                
018100                                                                          
018200     OPEN INPUT W42816                                                    
018300     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
018400                                                                          
018500     .                                                                    
018600     EJECT                                                                
018700 C-FLYTTA-DATA  SECTION.                                                  
018800                                                                          
018900     MOVE 'LINE'              TO DOC-IDAFPRCD                             
019000     MOVE IN-IDDC-RET         TO DOC-IDDC                                 
019100     MOVE IN-TIAAVV           TO DOC-TIAAVV                               
019200     MOVE IN-ADCITY           TO DOC-ADCITY                               
019300     MOVE IN-KVRETINL         TO DOC-KVRETINL                             
019400     MOVE IN-KVRETINL-SKR     TO DOC-KVRETINL-SKR                         
019500     MOVE IN-KVAVV-KVANT      TO DOC-KVAVV-KVANT                          
019600     MOVE IN-KVDAGDEC         TO DOC-KVDAGDEC                             
019700                                                                          
019800     .                                                                    
019900     EJECT                                                                
020000 Z-FINIT SECTION.                                                         
020100                                                                          
020200                                                                          
020300     CLOSE W42816                                                         
020400     SKIP2                                                                
020500     MOVE 'S' TO POSTSUM-OPKOD                                            
020600     CALL POSTSUM USING POSTSUM-PARM                                      
020700     .                                                                    
020800     EJECT                                                                
020900 S01-LAES-W42816  SECTION.                                                
021000     SKIP2                                                                
021100     READ W42816 INTO IN-AREA                                             
021200     AT END                                                               
021300        MOVE HIGH-VALUE TO IN-W42816                                      
021400        SET END-OF-W42816 TO TRUE                                         
021500                                                                          
021600     NOT AT END                                                           
021700        MOVE 'W42816'   TO POSTSUM-FDNAMN                                 
021800        MOVE 'W42817D1' TO POSTSUM-DDNAMN2                                
021900        MOVE SPACE      TO POSTSUM-TRANSTYP                               
022000        CALL POSTSUM USING POSTSUM-PARM                                   
022100     END-READ                                                             
022200     .                                                                    
022300     EJECT                                                                
022400 S02-FLYTTA-HEADER-DATA  SECTION.                                         
022500                                                                          
022600     MOVE 001             TO HDR-REQU-IDMSGVER                            
022700     MOVE SPACE           TO HDR-REQU-KDPGMACT                            
022800     MOVE 'W42817'        TO HDR-REQU-IDUSER                              
022900                                                                          
023000     MOVE 'MANDISC72WEEK  '   TO HDR-IDOUTTYPE                            
023100     MOVE SPACE               TO HDR-IDOUTREC                             
023300     MOVE 'EUW42817'          TO HDR-IDOUTREC                             
023310     EVALUATE TRUE                                                        
023320     WHEN IDFTG-CN                                                        
023330         MOVE 'CN'            TO HDR-IDOUTREC(1:2)                        
023340     WHEN IDFTG-IN                                                        
023350         MOVE 'AS'            TO HDR-IDOUTREC(1:2)                        
023360     WHEN IDFTG-KR                                                        
023370         MOVE 'KR'            TO HDR-IDOUTREC(1:2)                        
023380     WHEN IDFTG-TR                                                        
023390         MOVE 'TR'            TO HDR-IDOUTREC(1:2)                        
023391     WHEN IDFTG-BR                                                        
023392         MOVE 'BR'            TO HDR-IDOUTREC(1:2)                        
023393     WHEN IDFTG-MX                                                        
023394         MOVE 'MX'            TO HDR-IDOUTREC(1:2)                        
023395     WHEN IDFTG-ZA                                                        
023396         MOVE 'ZA'            TO HDR-IDOUTREC(1:2)                        
023397     WHEN IDFTG-MY                                                        
023398         MOVE 'MY'            TO HDR-IDOUTREC(1:2)                        
023399     WHEN IDFTG-TH                                                        
023400         MOVE 'TH'            TO HDR-IDOUTREC(1:2)                        
023401     WHEN IDFTG-TW                                                        
023402         MOVE 'TW'            TO HDR-IDOUTREC(1:2)                        
023403     END-EVALUATE                                                         
023410                                                                          
023500     MOVE FUNCTION CURRENT-DATE (3:10) TO WS-YYMMDDHHMM                   
023600     MOVE WS-YYMMDDHHMM       TO HDR-IDLIST                               
023700     .                                                                    
023800     EJECT                                                                
023810 S03-FLYTTA-HEADER-DATA-CN  SECTION.                                      
023820                                                                          
023830     MOVE 001             TO HDR-REQU-IDMSGVER                            
023840     MOVE SPACE           TO HDR-REQU-KDPGMACT                            
023850     MOVE 'W42817'        TO HDR-REQU-IDUSER                              
023860                                                                          
023870     MOVE 'MANDISC72WEEK  '   TO HDR-IDOUTTYPE                            
023880     MOVE SPACE               TO HDR-IDOUTREC                             
023890     MOVE 'CNW42817'          TO HDR-IDOUTREC                             
023891                                                                          
023892     MOVE FUNCTION CURRENT-DATE (3:10) TO WS-YYMMDDHHMM                   
023893     MOVE WS-YYMMDDHHMM       TO HDR-IDLIST                               
023894     .                                                                    
023895     EJECT                                                                
023896 S04-FLYTTA-HEADER-DATA-IN  SECTION.                                      
023897                                                                          
023898     MOVE 001             TO HDR-REQU-IDMSGVER                            
023899     MOVE SPACE           TO HDR-REQU-KDPGMACT                            
023900     MOVE 'W42817'        TO HDR-REQU-IDUSER                              
023901                                                                          
023902     MOVE 'MANDISC72WEEK  '   TO HDR-IDOUTTYPE                            
023903     MOVE SPACE               TO HDR-IDOUTREC                             
023904     MOVE 'ASW42817'          TO HDR-IDOUTREC                             
023905                                                                          
023906     MOVE FUNCTION CURRENT-DATE (3:10) TO WS-YYMMDDHHMM                   
023907     MOVE WS-YYMMDDHHMM       TO HDR-IDLIST                               
023908     .                                                                    
023909     EJECT                                                                
023910 S05-OPEN-DAP-SEND SECTION.                                               
024000*    MOVE 'S05-OPEN-DAP-S' TO CURR-SECTION                                
024100                                                                          
024200     MOVE 'CARPARTS.DAP.DISTRDOCWEB' TO SEND-ADDISPABS                    
024300     MOVE 'OPEN'                     TO SEND-KDFUNC                       
024400     CALL WZ01SEND USING SEND-CONTROL-AREA                                
024500                         SEND-OPEN-AREA                                   
024600     IF SEND-KDRC > ZERO                                                  
024700       MOVE SEND-KDRC                     TO KDRC-DISPLAY                 
024800       STRING 'WZ01SEND OPEN ERROR RC=' KDRC-DISPLAY                      
024900       DELIMITED BY SIZE INTO ERROR-TEXT                                  
025000       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
025100     END-IF                                                               
025200     .                                                                    
025300     EJECT                                                                
025400                                                                          
025500 S06-PUT-DAP-HEADER SECTION.                                              
025600*    MOVE 'S06-PUT-DAP-HE' TO CURR-SECTION                                
025700                                                                          
025800     MOVE 'PUT'                           TO SEND-KDFUNC                  
025900     MOVE LENGTH OF HDR-AREA              TO SEND-KVDLEN                  
026000     CALL WZ01SEND USING SEND-CONTROL-AREA                                
026100                         SEND-KVDLEN                                      
026200                         HDR-AREA                                         
026300     IF SEND-KDRC > ZERO                                                  
026400       MOVE SEND-KDRC                     TO KDRC-DISPLAY                 
026500       STRING 'WZ01SEND PUT ERROR RC=' KDRC-DISPLAY                       
026600       DELIMITED BY SIZE INTO ERROR-TEXT                                  
026700       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
026800     END-IF                                                               
026900     .                                                                    
027000 S07-PUT-DOC      SECTION.                                                
027100*    MOVE 'S07-PUT-DOC ' TO CURR-SECTION                                  
027200                                                                          
027300     MOVE 'PUT'                           TO SEND-KDFUNC                  
027400     MOVE LENGTH OF DOC-AREA              TO SEND-KVDLEN                  
027500     CALL WZ01SEND USING SEND-CONTROL-AREA                                
027600                         SEND-KVDLEN                                      
027700                         DOC-AREA                                         
027800     IF SEND-KDRC > ZERO                                                  
027900       MOVE SEND-KDRC                     TO KDRC-DISPLAY                 
028000       STRING 'WZ01SEND PUT ERROR RC=' KDRC-DISPLAY                       
028100       DELIMITED BY SIZE INTO ERROR-TEXT                                  
028200       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
028300     END-IF                                                               
028400     .                                                                    
028500     EJECT                                                                
028600 S08-CLOSE-DAP-SEND SECTION.                                              
028700*    MOVE 'S08-CLOSE-DAP-' TO CURR-SECTION                                
028800                                                                          
028900     MOVE 'CLOSE'                    TO SEND-KDFUNC                       
029000     CALL WZ01SEND USING SEND-CONTROL-AREA                                
029100                                                                          
029200     IF SEND-KDRC > 0                                                     
029300       MOVE SEND-KDRC TO KDRC-DISPLAY                                     
029400       STRING 'WZ01SEND CLOSE ERROR RC=' KDRC-DISPLAY                     
029500       DELIMITED BY SIZE INTO ERROR-TEXT                                  
029600       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
029700     END-IF                                                               
029800     .                                                                    
029900     EJECT                                                                
030000                                                                          
030010 S09-FLYTTA-HEADER-DATA-KR  SECTION.                                      
030020                                                                          
030030     MOVE 001             TO HDR-REQU-IDMSGVER                            
030040     MOVE SPACE           TO HDR-REQU-KDPGMACT                            
030050     MOVE 'W42817'        TO HDR-REQU-IDUSER                              
030060                                                                          
030070     MOVE 'MANDISC72WEEK  '   TO HDR-IDOUTTYPE                            
030080     MOVE SPACE               TO HDR-IDOUTREC                             
030090     MOVE 'KRW42817'          TO HDR-IDOUTREC                             
030091                                                                          
030092     MOVE FUNCTION CURRENT-DATE (3:10) TO WS-YYMMDDHHMM                   
030093     MOVE WS-YYMMDDHHMM       TO HDR-IDLIST                               
030094     .                                                                    
030095     EJECT                                                                
030096 S10-FLYTTA-HEADER-DATA-TR  SECTION.                                      
030097                                                                          
030098     MOVE 001             TO HDR-REQU-IDMSGVER                            
030099     MOVE SPACE           TO HDR-REQU-KDPGMACT                            
030100     MOVE 'W42817'        TO HDR-REQU-IDUSER                              
030101                                                                          
030102     MOVE 'MANDISC72WEEK'     TO HDR-IDOUTTYPE                            
030103     MOVE SPACE               TO HDR-IDOUTREC                             
030104     MOVE 'TRW42817'          TO HDR-IDOUTREC                             
030105                                                                          
030106     MOVE FUNCTION CURRENT-DATE (3:10) TO WS-YYMMDDHHMM                   
030107     MOVE WS-YYMMDDHHMM       TO HDR-IDLIST                               
030108     .                                                                    
030109     EJECT                                                                
030110 S11-FLYTTA-HEADER-DATA-BR  SECTION.                                      
030111                                                                          
030112     MOVE 001             TO HDR-REQU-IDMSGVER                            
030113     MOVE SPACE           TO HDR-REQU-KDPGMACT                            
030114     MOVE 'W42817'        TO HDR-REQU-IDUSER                              
030115                                                                          
030116     MOVE 'MANDISC72WEEK'     TO HDR-IDOUTTYPE                            
030117     MOVE SPACE               TO HDR-IDOUTREC                             
030118     MOVE 'BRW42817'          TO HDR-IDOUTREC                             
030119                                                                          
030120     MOVE FUNCTION CURRENT-DATE (3:10) TO WS-YYMMDDHHMM                   
030121     MOVE WS-YYMMDDHHMM       TO HDR-IDLIST                               
030122     .                                                                    
030123     EJECT                                                                
030124 S12-FLYTTA-HEADER-DATA-MX  SECTION.                                      
030125                                                                          
030126     MOVE 001             TO HDR-REQU-IDMSGVER                            
030127     MOVE SPACE           TO HDR-REQU-KDPGMACT                            
030128     MOVE 'W42817'        TO HDR-REQU-IDUSER                              
030129                                                                          
030130     MOVE 'MANDISC72WEEK'     TO HDR-IDOUTTYPE                            
030131     MOVE SPACE               TO HDR-IDOUTREC                             
030132     MOVE 'MXW42817'          TO HDR-IDOUTREC                             
030133                                                                          
030134     MOVE FUNCTION CURRENT-DATE (3:10) TO WS-YYMMDDHHMM                   
030135     MOVE WS-YYMMDDHHMM       TO HDR-IDLIST                               
030136     .                                                                    
030137     EJECT                                                                
030138 S14-FLYTTA-HEADER-DATA-ZA  SECTION.                                      
030139                                                                          
030140     MOVE 001             TO HDR-REQU-IDMSGVER                            
030141     MOVE SPACE           TO HDR-REQU-KDPGMACT                            
030142     MOVE 'W42817'        TO HDR-REQU-IDUSER                              
030143                                                                          
030144     MOVE 'MANDISC72WEEK'     TO HDR-IDOUTTYPE                            
030145     MOVE SPACE               TO HDR-IDOUTREC                             
030146     MOVE 'ZAW42817'          TO HDR-IDOUTREC                             
030147                                                                          
030148     MOVE FUNCTION CURRENT-DATE (3:10) TO WS-YYMMDDHHMM                   
030149     MOVE WS-YYMMDDHHMM       TO HDR-IDLIST                               
030150     .                                                                    
030151     EJECT                                                                
030152 S15-FLYTTA-HEADER-DATA-MY  SECTION.                                      
030153                                                                          
030154     MOVE 001             TO HDR-REQU-IDMSGVER                            
030155     MOVE SPACE           TO HDR-REQU-KDPGMACT                            
030156     MOVE 'W42817'        TO HDR-REQU-IDUSER                              
030157                                                                          
030158     MOVE 'MANDISC72WEEK'     TO HDR-IDOUTTYPE                            
030159     MOVE SPACE               TO HDR-IDOUTREC                             
030160     MOVE 'MYW42817'          TO HDR-IDOUTREC                             
030161                                                                          
030162     MOVE FUNCTION CURRENT-DATE (3:10) TO WS-YYMMDDHHMM                   
030163     MOVE WS-YYMMDDHHMM       TO HDR-IDLIST                               
030164     .                                                                    
030165     EJECT                                                                
030170 IMS-STATUSKONTROLL SECTION.                                              
030200     SKIP2                                                                
030300     SET STATUS-IX TO 1                                                   
030400     SEARCH GODK-STATUS                                                   
030500       AT END                                                             
030600         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
030700           DELIMITED BY SIZE INTO FELTEXT                                 
030800         DISPLAY FELTEXT                                                  
030900         CALL FELLOG                                                      
031000       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
031100         CONTINUE                                                         
031200     END-SEARCH                                                           
031300     .                                                                    
