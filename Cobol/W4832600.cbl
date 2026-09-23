000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W4832600.                                                
000300 AUTHOR.         STENING INGER.                                           
000400 DATE-WRITTEN.   21/04/07.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*                                                                         
000800*    FUNCTION: CREATE FILE TO AZURE IN DISPLAY FORMAT                     
000900*                                                                         
001000*    ABENDCODES:                                                          
001100*                                                                         
001200                                                                          
001300     SKIP3                                                                
001400 ENVIRONMENT DIVISION.                                                    
001500     SKIP2                                                                
001600 INPUT-OUTPUT SECTION.                                                    
001700                                                                          
001800 FILE-CONTROL.                                                            
001900     SKIP2                                                                
002000*          --- INFIL                                                      
002100     SELECT W48325                     ASSIGN TO W48326D1.                
002200*          --- UTFIL                                                      
002300     SELECT W48325X                    ASSIGN TO W48326D2.                
002400     EJECT                                                                
002500 DATA DIVISION.                                                           
002600     SKIP3                                                                
002700 FILE SECTION.                                                            
002800     SKIP3                                                                
002900 FD  W48325                                                               
003000     RECORDING       F                                                    
003100     BLOCK CONTAINS  0.                                                   
003200                                                                          
003300*01  -COPY W4832501       -PRE  IN-   -L.                                 
003400     SKIP3                                                                
003500 FD  W48325X                                                              
003600     RECORDING       F                                                    
003700     BLOCK CONTAINS  0.                                                   
003800                                                                          
003900*01  RECORD -COPY W48325X -PRE  OUTX- -L.                                 
004000     EJECT                                                                
004100 WORKING-STORAGE SECTION.                                                 
004200                                                                          
004300 77  IDPGM                       PIC X(8)    VALUE 'W4832600'.            
004400 77  YES                         PIC X       VALUE 'J'.                   
004500 77  NOO                         PIC X       VALUE 'N'.                   
004600 77  IX1                         PIC S9(9)   VALUE +0 COMP SYNC.          
004700                                                                          
004800 77  W48325-EOF-SW               PIC X       VALUE 'N'.                   
004900     88  END-OF-W48325                       VALUE 'J'.                   
005000     EJECT                                                                
005100 01  GENERAL-SUBPROGRAMS.                                                 
005200*                                                                         
005300     03  WDATKONV                PIC X(8)    VALUE 'WDATKONV'.            
005400     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
005500     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
005600     SKIP2                                                                
005700*    --- PARAMETERS TO ABEND                                              
005800                                                                          
005900 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
006000 77  RKOD-ABEND-NO-DUMP          PIC S9(4)   COMP VALUE +16.              
006100 77  RKOD-ABEND-WITH-DUMP        PIC S9(4)   COMP VALUE +1000.            
006200     SKIP2                                                                
006300 01  ERROR-TEXT.                                                          
006400     03  FILLER                  PIC X(8)    VALUE 'ERR-TEXT'.            
006500     03  ERROR-TEXT-STR          PIC X(72)   VALUE SPACE.                 
006600     EJECT                                                                
006700*    --- PARAMETRAR TILL POSTSUM                                          
006800*                                                                         
006900*01  -COPY W0005   -PRE  POSTSUM-                                         
007000     EJECT                                                                
007100 01  IN-AREA-START               PIC X(24)   VALUE                        
007200                                 'IN-AREA-START  '.                       
007300                                                                          
007400*01  AREA -COPY W4832501     -PRE IN-                                     
007500     EJECT                                                                
007600 01  OUT1-AREA-START             PIC X(24)   VALUE                        
007700                                 'OUT1-AREA-START  '.                     
007800                                                                          
007900*01  AREA -COPY W48325X    -PRE OUTX-                                     
008000     EJECT                                                                
008100 PROCEDURE DIVISION.                                                      
008200 MAIN SECTION.                                                            
008300     SKIP2                                                                
008400                                                                          
008500     PERFORM A-INIT                                                       
008600     PERFORM S01-READ-W48325                                              
008700     PERFORM UNTIL END-OF-W48325                                          
008800       PERFORM S11-WRITE-W48325X                                          
008900                                                                          
009000       PERFORM S01-READ-W48325                                            
009100     END-PERFORM                                                          
009200                                                                          
009300     PERFORM Z-FINIT                                                      
009400                                                                          
009500     MOVE ZERO TO RETURN-CODE                                             
009600     GOBACK                                                               
009700     .                                                                    
009800     EJECT                                                                
009900 A-INIT SECTION.                                                          
010000                                                                          
010100     OPEN INPUT  W48325                                                   
010200                                                                          
010300          OUTPUT W48325X                                                  
010400                                                                          
010500     .                                                                    
010600     EJECT                                                                
010700 Z-FINIT SECTION.                                                         
010800     CLOSE W48325                                                         
010900           W48325X                                                        
011000     SKIP2                                                                
011100     MOVE 'S' TO POSTSUM-OPKOD                                            
011200     CALL POSTSUM USING POSTSUM-PARM                                      
011300     .                                                                    
011400     EJECT                                                                
011500 S01-READ-W48325  SECTION.                                                
011600                                                                          
011700     READ W48325 INTO IN-AREA                                             
011800     AT END                                                               
011900        MOVE HIGH-VALUE TO IN-AREA                                        
012000        SET END-OF-W48325 TO TRUE                                         
012100                                                                          
012200     NOT AT END                                                           
012300        MOVE 'W48325'   TO POSTSUM-FDNAMN                                 
012400        MOVE 'W48326D1' TO POSTSUM-DDNAMN2                                
012500        MOVE SPACE      TO POSTSUM-TRANSTYP                               
012600        CALL POSTSUM USING POSTSUM-PARM                                   
012700     END-READ                                                             
012800     .                                                                    
012900     EJECT                                                                
013000 S11-WRITE-W48325X SECTION.                                               
013100                                                                          
013200     MOVE IN-IDPRODNR           TO OUTX-IDPRODNR                          
013300     MOVE IN-IDKOLLI            TO OUTX-IDKOLLI                           
013400     MOVE IN-IDDC               TO OUTX-IDDC                              
013500     MOVE IN-IDDISTR            TO OUTX-IDDISTR                           
013600     MOVE IN-IDKUNDNR           TO OUTX-IDKUNDNR                          
013700     MOVE IN-KDFAKTYP           TO OUTX-KDFAKTYP                          
013800     MOVE IN-IDFAKT             TO OUTX-IDFAKT                            
013900     MOVE IN-TIFAKT             TO OUTX-TIFAKT                            
014000     MOVE IN-TIFAKTID           TO OUTX-TIFAKTID                          
014100     MOVE IN-KDORDKL            TO OUTX-KDORDKL                           
014200     MOVE IN-KDFRAKT            TO OUTX-KDFRAKT                           
014300     MOVE IN-FLDIRLEV           TO OUTX-FLDIRLEV                          
014400     MOVE IN-IDLEVNR            TO OUTX-IDLEVNR                           
014500     MOVE IN-KDVIA              TO OUTX-KDVIA                             
014600     MOVE IN-DASUPREF           TO OUTX-DASUPREF                          
014700     MOVE IN-TISUPTID           TO OUTX-TISUPTID                          
014800     MOVE IN-TIUTSKR            TO OUTX-TIUTSKR                           
014900     MOVE IN-TIUTSTID           TO OUTX-TIUTSTID                          
015000     MOVE IN-TILASTN            TO OUTX-TILASTN                           
015100     MOVE IN-TILASTID           TO OUTX-TILASTID                          
015200     MOVE IN-IDTRPTNR           TO OUTX-IDTRPTNR                          
015300     MOVE IN-IDLBBET            TO OUTX-IDLBBET                           
015400     MOVE IN-IDSHIPM            TO OUTX-IDSHIPM                           
015500     MOVE IN-IDLASTN            TO OUTX-IDLASTN                           
015600     MOVE IN-DIKOLLIL           TO OUTX-DIKOLLIL                          
015700     MOVE IN-DIKOLLIB           TO OUTX-DIKOLLIB                          
015800     MOVE IN-DIKOLLIH           TO OUTX-DIKOLLIH                          
015900     MOVE IN-KDKOLLI            TO OUTX-KDKOLLI                           
016000     MOVE IN-KDFARLIG-KOLLI     TO OUTX-KDFARLIG-KOLLI                    
016100     MOVE IN-KVFLAMP-KOLLI      TO OUTX-KVFLAMP-KOLLI                     
016200     MOVE +1                    TO IX1                                    
016300     PERFORM UNTIL IX1 > 10                                               
016400       MOVE IN-IDPSN    (IX1)   TO OUTX-IDPSN (IX1)                       
016500       MOVE IN-VKART-FG (IX1)   TO OUTX-VKART-FG(IX1)                     
016600       MOVE    IN-VLFG  (IX1)   TO OUTX-VLFG (IX1)                        
016700       ADD +1                   TO IX1                                    
016800     END-PERFORM                                                          
016900     MOVE IN-SUEQFG             TO OUTX-SUEQFG                            
017000     MOVE IN-KVFALRAD           TO OUTX-KVFALRAD                          
017100     MOVE IN-KVORDRAD           TO OUTX-KVORDRAD                          
017200     MOVE IN-SUORDV-KOLLI       TO OUTX-SUORDV-KOLLI                      
017300     MOVE IN-VKORDNTO-KOLLI     TO OUTX-VKORDNTO-KOLLI                    
017400     MOVE IN-VKORDBTO-KOLLI     TO OUTX-VKORDBTO-KOLLI                    
017500     MOVE IN-VLORDBTO-KOLLI     TO OUTX-VLORDBTO-KOLLI                    
017600     MOVE IN-FLLDCKND           TO OUTX-FLLDCKND                          
017700     MOVE IN-IDLANDX2           TO OUTX-IDLANDX2                          
017800     MOVE IN-TIAAVV-LASTN       TO OUTX-TIAAVV-LASTN                      
017900     MOVE IN-TIAAMM-LASTN       TO OUTX-TIAAMM-LASTN                      
018000     MOVE IN-TIAARP-LASTN       TO OUTX-TIAARP-LASTN                      
018100     MOVE IN-TIAA-LASTN         TO OUTX-TIAA-LASTN                        
018200                                                                          
018300     PERFORM S11A-WRITE-W48324X                                           
018400     .                                                                    
018500     EJECT                                                                
018600 S11A-WRITE-W48324X SECTION.                                              
018700                                                                          
018800     WRITE OUTX-RECORD FROM OUTX-AREA                                     
018900                                                                          
019000     MOVE SPACE      TO POSTSUM-TRANSTYP                                  
019100     MOVE 'W48326'   TO POSTSUM-FDNAMN                                    
019200     MOVE 'W48326D2' TO POSTSUM-DDNAMN2                                   
019300     CALL POSTSUM USING POSTSUM-PARM                                      
019400     .                                                                    
019500     EJECT                                                                
019600 S99-ABEND SECTION.                                                       
019700                                                                          
019800     SKIP2                                                                
019900     MOVE 'S' TO POSTSUM-OPKOD                                            
020000     CALL POSTSUM USING POSTSUM-PARM                                      
020100     CALL ABEND USING RKOD-ABEND                                          
020200     .                                                                    
