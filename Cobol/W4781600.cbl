000900*********************************************                             
001100 ID DIVISION.                                                             
001200 PROGRAM-ID.     W4781600.                                                
001300 AUTHOR.         SRINADH NADIMPALLI.                                      
001400 DATE-WRITTEN.   25/02/18.                                                
001500 DATE-COMPILED.                                                           
001600                                                                          
001700*    FUNCTION:                                                            
001800*        40581 SCREEN DATA TO DATALAKE                                    
001900*                                                                         
002010*        THE PROGRAM READS     WDR1 WDGX4431                              
002020*        THE PROGRAM READS     WDR1 WDGX4432                              
002030*        THE PROGRAM READS     WDR1 WDGX4433                              
002040*        THE PROGRAM READS     WDR1 WDGX4434                              
002100*                                                                         
002200*    ABENDCODES:                                                          
002300*        U0016 -  . . . .                                                 
002400*        U1000 -  . . . .                                                 
002500*                                                                         
002600                                                                          
002700     SKIP3                                                                
002800 ENVIRONMENT DIVISION.                                                    
002900     SKIP2                                                                
003000 INPUT-OUTPUT SECTION.                                                    
003100                                                                          
003200 FILE-CONTROL.                                                            
003301     SKIP2                                                                
003302*          --- EXTRACT OF WDGX4432 AND WDGX4434                           
003310     SELECT W47816                     ASSIGN TO W47816D1.                
003500     EJECT                                                                
003600 DATA DIVISION.                                                           
003700     SKIP2                                                                
003800 FILE SECTION.                                                            
003901     SKIP3                                                                
003902 FD  W47816                                                               
003903     RECORDING       F                                                    
003904     BLOCK CONTAINS  0.                                                   
003905 01  UT-AREA PIC X(94).                                                   
004000     EJECT                                                                
004100 WORKING-STORAGE SECTION.                                                 
004200                                                                          
004240                                                                          
004300 77  IDPGM                       PIC X(8)    VALUE 'W4781600'.            
004400 77  YES                         PIC X       VALUE 'J'.                   
004500 77  NOO                         PIC X       VALUE 'N'.                   
004800     EJECT                                                                
004900 01  TODAYS-DATE                 PIC 9(6)    VALUE ZERO.                  
005000 01  FILLER REDEFINES TODAYS-DATE.                                        
005100     03  TODAYS-DATE-YEAR        PIC 9(2).                                
005200     03  TODAYS-DATE-MONTH       PIC 9(2).                                
005300     03  TODAYS-DATE-DAY         PIC 9(2).                                
005400     EJECT                                                                
005500 01  GENERAL-SUBPROGRAMS.                                                 
005600*                                                                         
005700     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
005800     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
005900     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
006010     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
006100     SKIP2                                                                
006110 01  TRP-AREA.                                                            
006120     03 UT-IDHTYP PIC X(4).                                               
006130     03 UT-TAB01  PIC X      VALUE X'05'.                                 
006140     03 UT-IDDC   PIC X(2).                                               
006150     03 UT-TAB02  PIC X      VALUE X'05'.                                 
006160     03 UT-IDTRP  PIC X(5).                                               
006170     03 UT-TAB03  PIC X      VALUE X'05'.                                 
006180     03 UT-BETRPDST PIC X(15).                                            
006190     03 UT-TAB04  PIC X      VALUE X'05'.                                 
006191     03 UT-KVLASTTI PIC 9(3).9(2).                                        
006192     03 UT-TAB05  PIC X      VALUE X'05'.                                 
006193     03 UT-KVADMFL  PIC 9(3).9(2).                                        
006194     03 UT-TAB06  PIC X      VALUE X'05'.                                 
006195     03 UT-KVADMEL  PIC 9(3).9(2).                                        
006196     03 UT-TAB06  PIC X      VALUE X'05'.                                 
006197     03 UT1-IDHTYP PIC X(4).                                              
006198     03 UT1-TAB01  PIC X      VALUE X'05'.                                
006199     03 UT1-IDDC   PIC X(2).                                              
006200     03 UT1-TAB02  PIC X      VALUE X'05'.                                
006201     03 UT1-IDTRP  PIC X(5).                                              
006202     03 UT1-TAB03  PIC X      VALUE X'05'.                                
006203     03 UT1-TITRPAVG PIC 9(7).                                            
006204     03 UT1-TAB04  PIC X      VALUE X'05'.                                
006205     03 UT1-BETRPFIR PIC X(15).                                           
006206     03 UT1-TAB05  PIC X      VALUE X'05'.                                
006207     03 UT1-KDFARLIG PIC 9.                                               
006208     03 UT1-TAB06  PIC X      VALUE X'05'.                                
006209     03 UT1-VLTRPMIN PIC 9(3).                                            
006210*    --- PARAMETERS FOR SUBPROGRAM ABEND                                  
006300                                                                          
006400 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
006500 77  RKOD-ABEND-NO-DUMP          PIC S9(4)   COMP VALUE +16.              
006600 77  RKOD-ABEND-WITH-DUMP        PIC S9(4)   COMP VALUE +1000.            
006700     SKIP2                                                                
006800 01  ERROR-TEXT.                                                          
006900     03  FILLER                  PIC X(9)    VALUE 'ERRORTEXT'.           
007000     03  ERROR-TEXT-STR          PIC X(71)   VALUE SPACE.                 
007101     EJECT                                                                
007102*    --- PARAMETRAR TILL POSTSUM                                          
007103*                                                                         
007110*01  -COPY W0005   -PRE  POSTSUM-                                         
007301     EJECT                                                                
007500*    --- AREAS FOR IMS-SECTIONS                                           
007600*                                                                         
007700     EJECT                                                                
007800 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
007900     SKIP3                                                                
008000 01  KEYS-FOR-DLI.                                                        
008100     03  W-4431-IDHTYP       PIC X(4)    VALUE '4431'.                    
008200     SKIP2                                                                
008210     03  W-WDGXKEY-4433-X.                                                
008220         05  W-4433-IDHTYP       PIC  X(4)        VALUE '4433'.           
008230         05  W-4433-IDDC         PIC  X(2)        VALUE SPACE.            
008240         05  FILLER              PIC  X(24)       VALUE LOW-VALUE.        
008250                                                                          
008260     03  W-WDGXKEY-4434-MIN-X.                                            
008270         05  W-4434-IDTRP-MIN    PIC X(5)    VALUE SPACE.                 
008290         05  W-4434-LOW-VALUE    PIC X(5)    VALUE LOW-VALUE.             
008291                                                                          
008292     03  W-WDGXKEY-4434-MAX-X.                                            
008293         05  W-4434-IDTRP-MAX    PIC X(5)    VALUE SPACE.                 
008295         05  W-4434-HIGH-VALUE   PIC X(5)    VALUE HIGH-VALUE.            
008300*    --- STATUS-KOD FRÅN IMS                                              
008400 01  STATUS-WS                   PIC XX.                                  
008500     88  SEGMENT-FOUND                       VALUE '  '.                  
008600     88  SEGMENT-FOUND-EXISTS                VALUE 'II'.                  
008700     88  SEGMENT-MISSING                     VALUE 'GE'.                  
008710     88  SEGMENT-END                         VALUE 'GB'.                  
008800     SKIP2                                                                
008900 01  GOOD-STATUSCODES.                                                    
009000     03  GOOD-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
009100     SKIP3                                                                
009200 01  SSA1                        PIC X(64).                               
009300 01  SSA2                        PIC X(256).                              
009400     EJECT                                                                
009500*    --- IMS FUNCTION CODES                                               
009600*01  -COPY W0003                                                          
009700     EJECT                                                                
009900*    ---  DLI INPUT-OUTPUT AREA                                           
009910 01  FILLER               PIC X(16)   VALUE '4431 AREA'.                  
009920 01   DLI-IO-AREA-4431.                                                   
009930*     03  -COPY WDGX4431                                                  
009950 01  FILLER               PIC X(16)   VALUE '4432 AREA'.                  
009960 01   DLI-IO-AREA-4432.                                                   
009970*     03  -COPY WDGX4432                                                  
009980 01  FILLER               PIC X(16)   VALUE '4433 AREA'.                  
009990 01   DLI-IO-AREA-4433.                                                   
010000*     03  -COPY WDGX4433                                                  
010200 01  FILLER               PIC X(16)   VALUE '4434 AREA'.                  
010300 01   DLI-IO-AREA-4434.                                                   
010310*     03  -COPY WDGX4434                                                  
010400 LINKAGE SECTION.                                                         
010602*01  -COPY W0008  -PRE 4431-                                              
010611     05  4431-KFB-IDHTYP         PIC X(4).                                
010612     05  4431-KFB-IDDC           PIC X(2).                                
010620*01  -COPY W0008  -PRE 4433-                                              
010640     05  4433-KFB-IDHTYP         PIC X(4).                                
010650     05  4433-KFB-IDDC           PIC X(2).                                
010700     EJECT                                                                
010801 PROCEDURE DIVISION  USING 4431-PCB 4433-PCB.                             
010802 MAIN SECTION.                                                            
010810     ENTRY 'DLITCBL' USING 4431-PCB 4433-PCB.                             
010900                                                                          
011100     PERFORM A-INIT                                                       
011110     PERFORM B-FETCH-WDGX4432                                             
012600     PERFORM Z-FINIT                                                      
012700                                                                          
012800     MOVE ZERO TO RETURN-CODE                                             
012900     GOBACK                                                               
013000     .                                                                    
013100     EJECT                                                                
013200 A-INIT SECTION.                                                          
013401                                                                          
013410     OPEN OUTPUT W47816                                                   
013500                                                                          
013600     ACCEPT TODAYS-DATE  FROM DATE                                        
013710     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
013900     .                                                                    
014000     EJECT                                                                
014001 B-FETCH-WDGX4432 SECTION.                                                
014002     PERFORM IMS-GN-WDR101-WDGX4431                                       
014003     PERFORM UNTIL SEGMENT-MISSING OR SEGMENT-END                         
014004      PERFORM IMS-GNP-WDR110-WDGX4432                                     
014005      PERFORM UNTIL SEGMENT-MISSING                                       
014006       PERFORM BA-FETCH-WDGX4434                                          
014009       PERFORM IMS-GNP-WDR110-WDGX4432                                    
014010      END-PERFORM                                                         
014011      PERFORM IMS-GN-WDR101-WDGX4431                                      
014012     END-PERFORM                                                          
014013     .                                                                    
014014     EJECT                                                                
014015 BA-FETCH-WDGX4434 SECTION.                                               
014016     MOVE 4431-KFB-IDDC TO W-4433-IDDC                                    
014017     PERFORM IMS-GU-WDR101-WDGX4433                                       
014019      MOVE 4432-IDTRP TO W-4434-IDTRP-MIN                                 
014020                         W-4434-IDTRP-MAX                                 
014021      PERFORM IMS-GNP-WDR130-WDGX4434                                     
014022      PERFORM UNTIL SEGMENT-MISSING OR SEGMENT-END                        
014023       PERFORM BAA-MOVE-DATA                                              
014024       PERFORM S11-WRITE-TRP                                              
014025       PERFORM IMS-GNP-WDR130-WDGX4434                                    
014026      END-PERFORM                                                         
014029     .                                                                    
014030     EJECT                                                                
014041 BAA-MOVE-DATA SECTION.                                                   
014042     MOVE 4431-KFB-IDHTYP    TO UT-IDHTYP                                 
014043     MOVE 4431-KFB-IDDC      TO UT-IDDC                                   
014044     MOVE 4432-IDTRP         TO UT-IDTRP                                  
014045     MOVE 4432-BETRPDST      TO UT-BETRPDST                               
014046     MOVE 4432-KVLASTTI      TO UT-KVLASTTI                               
014047     MOVE 4432-KVADMFL       TO UT-KVADMFL                                
014048     MOVE 4432-KVADMEL       TO UT-KVADMEL                                
014050     MOVE 4433-KFB-IDHTYP    TO UT1-IDHTYP                                
014060     MOVE 4433-KFB-IDDC      TO UT1-IDDC                                  
014070     MOVE 4434-IDTRP         TO UT1-IDTRP                                 
014080     MOVE 4434-BETRPFIR      TO UT1-BETRPFIR                              
014090     MOVE 4434-KDFARLIG      TO UT1-KDFARLIG                              
014091     MOVE 4434-VLTRPMIN      TO UT1-VLTRPMIN                              
014092     MOVE 4434-TITRPAVG      TO UT1-TITRPAVG                              
014093     .                                                                    
014094     EJECT                                                                
014100 Z-FINIT SECTION.                                                         
014210     CLOSE W47816                                                         
014301     SKIP2                                                                
014302     MOVE 'S' TO POSTSUM-OPKOD                                            
014310     CALL POSTSUM USING POSTSUM-PARM                                      
014400     .                                                                    
014601     EJECT                                                                
014602 S11-WRITE-TRP SECTION.                                                   
014603                                                                          
014604     WRITE UT-AREA FROM TRP-AREA.                                         
014605                                                                          
014606**   MOVE TRP-IDPTYP TO POSTSUM-TRANSTYP                                  
014607     MOVE 'W47816' TO POSTSUM-FDNAMN                                      
014608     MOVE 'W47816D1' TO POSTSUM-DDNAMN2                                   
014609     CALL POSTSUM USING POSTSUM-PARM                                      
014610     .                                                                    
014800     EJECT                                                                
014900 S99-ABEND SECTION.                                                       
015000                                                                          
015101     SKIP2                                                                
015102     MOVE 'S' TO POSTSUM-OPKOD                                            
015110     CALL POSTSUM USING POSTSUM-PARM                                      
015200     CALL ABEND USING RKOD-ABEND                                          
015300     .                                                                    
015400     EJECT                                                                
015500* --- IMS SECTIONS  ---                                                   
015600                                                                          
015701     EJECT                                                                
015702 IMS-GN-WDR101-WDGX4431 SECTION.                                          
015703                                                                          
015704     STRING 'WDR101  (IDHTYP   =' W-4431-IDHTYP ')'                       
015705          DELIMITED BY SIZE INTO SSA1                                     
015706     MOVE '  GEGB' TO GOOD-STATUSCODES                                    
015707     CALL CBLTDLI USING GN 4431-PCB DLI-IO-AREA-4431 SSA1                 
015708     MOVE 4431-STATUS-CODE TO STATUS-WS                                   
015709     PERFORM IMS-STATUSCHECK                                              
015710     .                                                                    
015800     EJECT                                                                
015892 IMS-GNP-WDR110-WDGX4432 SECTION.                                         
015893                                                                          
015897     MOVE 'WDR110  '        TO SSA1                                       
015899     MOVE '  GE' TO GOOD-STATUSCODES                                      
015900     CALL CBLTDLI USING GNP 4431-PCB DLI-IO-AREA-4432 SSA1                
015901     MOVE 4431-STATUS-CODE TO STATUS-WS                                   
015902     PERFORM IMS-STATUSCHECK                                              
015903     .                                                                    
015904     EJECT                                                                
015905 IMS-GU-WDR101-WDGX4433 SECTION.                                          
015906                                                                          
015907     STRING 'WDR101  (WDGXKEY  =' W-WDGXKEY-4433-X ')'                    
015908          DELIMITED BY SIZE INTO SSA2                                     
015909     MOVE '  GE' TO GOOD-STATUSCODES                                      
015910     CALL CBLTDLI USING GU 4433-PCB DLI-IO-AREA-4433 SSA2                 
015911     MOVE 4433-STATUS-CODE TO STATUS-WS                                   
015912     PERFORM IMS-STATUSCHECK                                              
015913     .                                                                    
015914     EJECT                                                                
015915 IMS-GNP-WDR130-WDGX4434 SECTION.                                         
015916                                                                          
015917     STRING 'WDR130  (WDGXKEY >=' W-WDGXKEY-4434-MIN-X                    
015918                    '&WDGXKEY <=' W-WDGXKEY-4434-MAX-X ')'                
015919             DELIMITED BY SIZE INTO SSA2                                  
015921     MOVE '  GE' TO GOOD-STATUSCODES                                      
015922     CALL CBLTDLI USING GNP 4433-PCB DLI-IO-AREA-4434 SSA2                
015923     MOVE 4433-STATUS-CODE TO STATUS-WS                                   
015924     PERFORM IMS-STATUSCHECK                                              
015925     .                                                                    
015926     EJECT                                                                
015930 IMS-STATUSCHECK SECTION.                                                 
016000                                                                          
016100     SET STATUS-IX TO 1                                                   
016200     SEARCH GOOD-STATUS                                                   
016300       AT END                                                             
016400         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
016500           DELIMITED BY SIZE INTO ERROR-TEXT                              
016600         DISPLAY ERROR-TEXT                                               
016700         CALL FELLOG                                                      
016800       WHEN GOOD-STATUS (STATUS-IX) = STATUS-WS                           
016900         CONTINUE                                                         
017000     END-SEARCH                                                           
017100     .                                                                    
