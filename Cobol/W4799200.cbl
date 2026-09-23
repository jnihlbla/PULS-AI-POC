000100 ID DIVISION.                                                             
000200     SKIP2                                                                
000300 PROGRAM-ID.     W4799200.                                                
000400*AUTHOR.         STEFANO GIOBBI.                                          
000500*DATE-WRITTEN.   92/04/30.                                                
000600                                                                          
000700*    REMARKS.                                                             
000800*                                                                         
000900*    FUNCTION:                                                            
001000*        TAR IN NEDLÄST WDQ2 OCH SELEKTERAR UT ORDERHUVUDEN MED           
001100*        ARBETSTABELLER.                                                  
001200*                                                                         
001300     SKIP3                                                                
001400 ENVIRONMENT DIVISION.                                                    
001500     SKIP2                                                                
001600 INPUT-OUTPUT SECTION.                                                    
001700                                                                          
001800 FILE-CONTROL.                                                            
001900     SKIP2                                                                
002000     SELECT W47997                     ASSIGN TO W47992D1.                
002100*          --- WDQ2 NEDLÄST                                               
002200                                                                          
002300     SELECT W47992                     ASSIGN TO W47992D2.                
002400*          --- CDC-INFO FRÅN NEDLÄST WDQ2                                 
002500     EJECT                                                                
002600 DATA DIVISION.                                                           
002700     SKIP3                                                                
002800 FILE SECTION.                                                            
002900     SKIP3                                                                
003000 FD  W47997                                                               
003100     RECORDING       V                                                    
003200     BLOCK CONTAINS  0.                                                   
003300     SKIP2                                                                
003400*01  -COPY W479973     -L.                                                
003500     SKIP3                                                                
003600 FD  W47992                                                               
003700     RECORDING       F                                                    
003800     BLOCK CONTAINS  0.                                                   
003900     SKIP2                                                                
004000*01  POST -COPY W47992 -PRE  U92-  -L.                                    
004100     EJECT                                                                
004200 WORKING-STORAGE SECTION.                                                 
004300     SKIP2                                                                
004301                                                                          
004310*    -- CHECKED BY WY2000                                                 
004400 77  IDPGM                       PIC X(8)    VALUE 'W4799200'.            
004500 77  JA                          PIC X       VALUE 'J'.                   
004600 77  NEJ                         PIC X       VALUE 'N'.                   
004700     EJECT                                                                
004800 01  TODAYS-DATE                 PIC 9(6)    VALUE ZERO.                  
004900 01  FILLER REDEFINES TODAYS-DATE.                                        
005000     03  TODAYS-DATE-YEAR        PIC 9(2).                                
005100     03  TODAYS-DATE-MONTH       PIC 9(2).                                
005200     03  TODAYS-DATE-DAY         PIC 9(2).                                
005300     EJECT                                                                
005400 01  GENERAL-SUBPROGRAM.                                                  
005500*                                                                         
005600     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
005700     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
005800     SKIP2                                                                
005900*    --- PARAMETERS TO ABEND                                              
006000                                                                          
006100 77  RKOD-ABEND-NO-DUMP          PIC S9(4)   COMP VALUE +16.              
006200 77  RKOD-ABEND-WITH-DUMP        PIC S9(4)   COMP VALUE +1000.            
006300     SKIP2                                                                
006400 01  ERRTEXT.                                                             
006500     03  FILLER                  PIC  X(8)   VALUE 'ERRTEXT '.            
006600     03  ERRTEXT-STR             PIC  X(72)  VALUE SPACE.                 
006700     SKIP2                                                                
006800 01  SWITCH-AREA.                                                         
006900     03  FILLER                  PIC  X(8)   VALUE 'SWITCHAR'.            
007000     03  SW-EOF-W47997           PIC  X(1)   VALUE 'N'.                   
007100     SKIP2                                                                
007200 01  KONSTANT-AREA.                                                       
007300     03  FILLER                  PIC  X(8)   VALUE 'KONSTANT'.            
007400     03  K-IDSEGM-WDQ201         PIC  X(6)   VALUE 'WDQ201'.              
007500     03  K-IDSEGM-WDQ212         PIC  X(6)   VALUE 'WDQ212'.              
007600     SKIP2                                                                
007700 01  SPAR-AREA.                                                           
007800     03  FILLER                  PIC  X(8)   VALUE 'SPARAREA'.            
007900     03  SPAR-I97Q201-FLKLAR     PIC  X(1)   VALUE SPACE.                 
008000     EJECT                                                                
008100*    --- PARAMETRAR TILL POSTSUM                                          
008200*                                                                         
008300*01  -COPY W0005   -PRE  POSTSUM-                                         
008400     EJECT                                                                
008500 01  I97-AREA-START              PIC X(24)   VALUE                        
008600                                 'I97-AREA-START   '.                     
008700     SKIP2                                                                
008800 01  I97-AREA.                                                            
008900                                                                          
009000     03  I97-IDSEGM            PIC X(6)   VALUE SPACE.                    
009100     03  FILLER                PIC X(500) VALUE SPACE.                    
009200     SKIP2                                                                
009300*01  FILLER   -PRE I97Q201- -COPY W479971 -RED I97-AREA.                  
009400     EJECT                                                                
009500*01  FILLER   -PRE I97Q212- -COPY W479973 -RED I97-AREA.                  
009600     EJECT                                                                
009700 01  U92-AREA-START              PIC X(24)   VALUE                        
009800                                 'U92-AREA-START  '.                      
009900     SKIP2                                                                
010000                                                                          
010100*01  AREA -COPY W47992     -PRE U92-                                      
010200     EJECT                                                                
010300 PROCEDURE DIVISION.                                                      
010400                                                                          
010500     PERFORM A-INIT                                                       
010600     PERFORM S01-READ-W47997                                              
010700     PERFORM UNTIL SW-EOF-W47997 = JA                                     
010800                                                                          
010900       EVALUATE I97-IDSEGM                                                
011000                                                                          
011100         WHEN K-IDSEGM-WDQ201                                             
011200           PERFORM B-FLYTTA-WDQ201-INFO                                   
011300           MOVE    I97Q201-FLKLAR TO SPAR-I97Q201-FLKLAR                  
011400         WHEN K-IDSEGM-WDQ212                                             
011500           IF SPAR-I97Q201-FLKLAR = JA                                    
011600             PERFORM D-BEHANDLA-WDQ212-INFO                               
011700             PERFORM S11-WRITE-W47992                                     
011800           END-IF                                                         
011900       END-EVALUATE                                                       
012000                                                                          
012100       PERFORM S01-READ-W47997                                            
012200     END-PERFORM                                                          
012300                                                                          
012400     PERFORM Z-FINIT                                                      
012500                                                                          
012600     MOVE ZERO TO RETURN-CODE                                             
012700     GOBACK                                                               
012800     .                                                                    
012900     EJECT                                                                
013000 A-INIT SECTION.                                                          
013100                                                                          
013200     OPEN INPUT                                                           
013300          W47997                                                          
013400                                                                          
013500     OPEN OUTPUT                                                          
013600          W47992                                                          
013700                                                                          
013800*    ACCEPT TODAYS-DATE FROM DATE                                         
013900     MOVE   IDPGM       TO   POSTSUM-PROGNAMN                             
014000     .                                                                    
014100     EJECT                                                                
014200 B-FLYTTA-WDQ201-INFO SECTION.                                            
014300                                                                          
014400     MOVE I97Q201-IDORDER TO U92-IDORDER                                  
014500     MOVE I97Q201-KDORDKL TO U92-KDORDKL                                  
014600     .                                                                    
014700     EJECT                                                                
014800 D-BEHANDLA-WDQ212-INFO SECTION.                                          
014900                                                                          
015000     MOVE I97Q212-IDDC      TO U92-IDDC                                   
015100     MOVE I97Q212-KDFRAKT   TO U92-KDFRAKT                                
015200     MOVE I97Q212-IDPRC-GRP TO U92-IDPRC-GRP                              
015300     .                                                                    
015400     EJECT                                                                
015500 Z-FINIT SECTION.                                                         
015600                                                                          
015700     CLOSE W47997                                                         
015800           W47992                                                         
015900                                                                          
016000     MOVE 'S'     TO    POSTSUM-OPKOD                                     
016100     CALL POSTSUM USING POSTSUM-PARM                                      
016200     .                                                                    
016300     EJECT                                                                
016400 S01-READ-W47997 SECTION.                                                 
016500                                                                          
016600     READ W47997      INTO I97-AREA                                       
016700     AT END                                                               
016800        MOVE JA         TO    SW-EOF-W47997                               
016900                                                                          
017000     NOT AT END                                                           
017100        MOVE 'W47997'   TO    POSTSUM-FDNAMN                              
017200        MOVE 'W47992D1' TO    POSTSUM-DDNAMN2                             
017300        MOVE 'I97'      TO    POSTSUM-TRANSTYP                            
017400        CALL POSTSUM    USING POSTSUM-PARM                                
017500     END-READ                                                             
017600     .                                                                    
017700     EJECT                                                                
017800 S11-WRITE-W47992 SECTION.                                                
017900                                                                          
018000     WRITE U92-POST  FROM  U92-AREA                                       
018100                                                                          
018200     MOVE 'U92'      TO    POSTSUM-TRANSTYP                               
018300     MOVE 'W47992'   TO    POSTSUM-FDNAMN                                 
018400     MOVE 'W47992D2' TO    POSTSUM-DDNAMN2                                
018500     CALL POSTSUM    USING POSTSUM-PARM                                   
018600     .                                                                    
018700     EJECT                                                                
018800 S99-ABEND SECTION.                                                       
018900                                                                          
019000     MOVE 'S'     TO    POSTSUM-OPKOD                                     
019100     CALL POSTSUM USING POSTSUM-PARM                                      
019200     CALL ABEND   USING RKOD-ABEND-NO-DUMP                                
019300     .                                                                    
