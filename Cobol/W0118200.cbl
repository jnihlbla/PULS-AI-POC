000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W0118200.                                                
000300 AUTHOR.         SATHISH THIRUVENGADAM.                                   
000400 DATE-WRITTEN.   22/06/24.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*                                                                         
000800*    FUNCTION: CREATE FILES TO AZURE DATALAKE IN DISPLAY FORMAT           
000900*              -NDC PROCUREMENT DATA (WDK722,WDK723 EXTRACT)              
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
002000*          --- INPUT FILE FROM W01184 - WDK722                            
002100     SELECT W01182                     ASSIGN TO W01182D1.                
002110*          --- INPUT FILE FROM W01184 - WDK723                            
002120     SELECT W01183                     ASSIGN TO W01182D2.                
002200*          --- OUTPUT FILE TO AZURE DATALAKE - WDK722                     
002300     SELECT W01182X                    ASSIGN TO W01182D3.                
002310*          --- OUTPUT FILE TO AZURE DATALAKE - WDK723                     
002320     SELECT W01183X                    ASSIGN TO W01182D4.                
002400     EJECT                                                                
002500 DATA DIVISION.                                                           
002600     SKIP3                                                                
002700 FILE SECTION.                                                            
002800     SKIP3                                                                
002900 FD  W01182                                                               
003000     RECORDING       F                                                    
003100     BLOCK CONTAINS  0.                                                   
003200                                                                          
003300*01  -COPY W01182          -PRE  IN1-   -L.                               
003301                                                                          
003310 FD  W01183                                                               
003320     RECORDING       F                                                    
003330     BLOCK CONTAINS  0.                                                   
003340                                                                          
003350*01  -COPY W01183          -PRE  IN2-   -L.                               
003400     SKIP3                                                                
003500 FD  W01182X                                                              
003600     RECORDING       F                                                    
003700     BLOCK CONTAINS  0.                                                   
003800                                                                          
003900*01  RECORD -COPY W01182X   -PRE  OUTX1- -L.                              
004000     SKIP3                                                                
004010 FD  W01183X                                                              
004020     RECORDING       F                                                    
004030     BLOCK CONTAINS  0.                                                   
004040                                                                          
004050*01  RECORD -COPY W01183X   -PRE  OUTX2- -L.                              
004060     EJECT                                                                
004100 WORKING-STORAGE SECTION.                                                 
004200                                                                          
004300 77  IDPGM                       PIC X(8)    VALUE 'W0118200'.            
004400 77  YES                         PIC X       VALUE 'J'.                   
004500 77  NOO                         PIC X       VALUE 'N'.                   
004600 77  IX1                         PIC S9(9)   VALUE +0 COMP SYNC.          
004700                                                                          
004800 77  W01182-EOF-SW               PIC X       VALUE 'N'.                   
004900     88  END-OF-W01182                       VALUE 'J'.                   
004910                                                                          
004920 77  W01183-EOF-SW               PIC X       VALUE 'N'.                   
004930     88  END-OF-W01183                       VALUE 'J'.                   
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
007100 01  IN1-AREA-START               PIC X(24)   VALUE                       
007200                                 'IN1-AREA-START  '.                      
007300                                                                          
007400*01  AREA -COPY W01182      -PRE IN1-                                     
007500     EJECT                                                                
007510 01  IN2-AREA-START               PIC X(24)   VALUE                       
007520                                 'IN2-AREA-START  '.                      
007530                                                                          
007540*01  AREA -COPY W01183      -PRE IN2-                                     
007550     EJECT                                                                
007600 01  OUT1-AREA-START             PIC X(24)   VALUE                        
007700                                 'OUT1-AREA-START  '.                     
007800                                                                          
007900*01  AREA -COPY W01182X     -PRE OUTX1-                                   
008000     EJECT                                                                
008010 01  OUT2-AREA-START             PIC X(24)   VALUE                        
008020                                 'OUT2-AREA-START  '.                     
008030                                                                          
008040*01  AREA -COPY W01183X     -PRE OUTX2-                                   
008050     EJECT                                                                
008100 PROCEDURE DIVISION.                                                      
008200 MAIN SECTION.                                                            
008300     SKIP2                                                                
008400                                                                          
008500     PERFORM A-INIT                                                       
008510*CREATES WDK722 FILE IN READBLE FORMAT                                    
008600     PERFORM S01-READ-W01182                                              
008700     PERFORM UNTIL END-OF-W01182                                          
008800       PERFORM S11-WRITE-W01182X                                          
008900                                                                          
009000       PERFORM S01-READ-W01182                                            
009100     END-PERFORM                                                          
009120*CREATES WDK723 FILE IN READBLE FORMAT                                    
009130     PERFORM S02-READ-W01183                                              
009140     PERFORM UNTIL END-OF-W01183                                          
009150       PERFORM S21-WRITE-W01183X                                          
009160                                                                          
009170       PERFORM S02-READ-W01183                                            
009180     END-PERFORM                                                          
009190*                                                                         
009200                                                                          
009300     PERFORM Z-FINIT                                                      
009400                                                                          
009500     MOVE ZERO TO RETURN-CODE                                             
009600     GOBACK                                                               
009700     .                                                                    
009800     EJECT                                                                
009900 A-INIT SECTION.                                                          
010000                                                                          
010100     OPEN INPUT  W01182                                                   
010200                 W01183                                                   
010300          OUTPUT W01182X                                                  
010400                 W01183X                                                  
010500     .                                                                    
010600     EJECT                                                                
010700 Z-FINIT SECTION.                                                         
010800     CLOSE W01182                                                         
010900           W01183                                                         
010910           W01182X                                                        
010920           W01183X                                                        
011000     SKIP2                                                                
011100     MOVE 'S' TO POSTSUM-OPKOD                                            
011200     CALL POSTSUM USING POSTSUM-PARM                                      
011300     .                                                                    
011400     EJECT                                                                
011500 S01-READ-W01182   SECTION.                                               
011600                                                                          
011700     READ W01182  INTO IN1-AREA                                           
011800     AT END                                                               
011900        MOVE HIGH-VALUE TO IN1-AREA                                       
012000        SET END-OF-W01182  TO TRUE                                        
012100                                                                          
012200     NOT AT END                                                           
012300        MOVE 'W01182'   TO POSTSUM-FDNAMN                                 
012400        MOVE 'W01182D1' TO POSTSUM-DDNAMN2                                
012500        MOVE SPACE      TO POSTSUM-TRANSTYP                               
012600        CALL POSTSUM USING POSTSUM-PARM                                   
012700     END-READ                                                             
012800     .                                                                    
012900     EJECT                                                                
012910 S02-READ-W01183   SECTION.                                               
012920                                                                          
012930     READ W01183  INTO IN2-AREA                                           
012940     AT END                                                               
012950        MOVE HIGH-VALUE TO IN2-AREA                                       
012960        SET END-OF-W01183  TO TRUE                                        
012970                                                                          
012980     NOT AT END                                                           
012990        MOVE 'W01183'   TO POSTSUM-FDNAMN                                 
012991        MOVE 'W01182D2' TO POSTSUM-DDNAMN2                                
012992        MOVE SPACE      TO POSTSUM-TRANSTYP                               
012993        CALL POSTSUM USING POSTSUM-PARM                                   
012994     END-READ                                                             
012995     .                                                                    
012996     EJECT                                                                
013000 S11-WRITE-W01182X SECTION.                                               
013100                                                                          
013200*WDK701                                                                   
013300     MOVE IN1-XLAG-IDARTNR             TO OUTX1-XLAG-IDARTNR              
013400     MOVE IN1-XLAG-IDDC                TO OUTX1-XLAG-IDDC                 
013500*WDK722                                                                   
013600     MOVE IN1-XLAG-KDSEGKEY            TO OUTX1-XLAG-KDSEGKEY             
013700     MOVE IN1-XLAG-DAPBPLAN            TO OUTX1-XLAG-DAPBPLAN             
013800     MOVE IN1-XLAG-DASEASON            TO OUTX1-XLAG-DASEASON             
013900     MOVE IN1-XLAG-FLJIT               TO OUTX1-XLAG-FLJIT                
014000     MOVE IN1-XLAG-IDANSK              TO OUTX1-XLAG-IDANSK               
014100     MOVE IN1-XLAG-IDINK               TO OUTX1-XLAG-IDINK                
014200     MOVE IN1-XLAG-IDLEVNR-FRAM        TO OUTX1-XLAG-IDLEVNR-FRAM         
014300     MOVE IN1-XLAG-IDLEVNR-SHIP        TO OUTX1-XLAG-IDLEVNR-SHIP         
014400     MOVE IN1-XLAG-IDPLANGR-AG         TO OUTX1-XLAG-IDPLANGR-AG          
014500     MOVE IN1-XLAG-KDAVT               TO OUTX1-XLAG-KDAVT                
014600     MOVE IN1-XLAG-KDLEVPLF            TO OUTX1-XLAG-KDLEVPLF             
014700     MOVE IN1-XLAG-KDLPSP              TO OUTX1-XLAG-KDLPSP               
014800     MOVE IN1-XLAG-FILLERX1            TO OUTX1-XLAG-FILLERX1             
014900     MOVE IN1-XLAG-KDOPPLAN            TO OUTX1-XLAG-KDOPPLAN             
015000     MOVE IN1-XLAG-KVDAGAR-FFH         TO OUTX1-XLAG-KVDAGAR-FFH          
015100     MOVE IN1-XLAG-KVEOQ               TO OUTX1-XLAG-KVEOQ                
015200     MOVE IN1-XLAG-KVPB-JUST1          TO OUTX1-XLAG-KVPB-JUST1           
015300     MOVE IN1-XLAG-KVPB-JUST2          TO OUTX1-XLAG-KVPB-JUST2           
015400     MOVE IN1-XLAG-KVPB-PLAN           TO OUTX1-XLAG-KVPB-PLAN            
015500     MOVE IN1-XLAG-KVPB-TREND          TO OUTX1-XLAG-KVPB-TREND           
015600     MOVE IN1-XLAG-KVPALL              TO OUTX1-XLAG-KVPALL               
015700     MOVE IN1-XLAG-KVSLAGER            TO OUTX1-XLAG-KVSLAGER             
015800     MOVE IN1-XLAG-KVSLUTKP            TO OUTX1-XLAG-KVSLUTKP             
015900     MOVE IN1-XLAG-KVSPANT             TO OUTX1-XLAG-KVSPANT              
016000     MOVE IN1-XLAG-KVULOAD             TO OUTX1-XLAG-KVULOAD              
016100     MOVE IN1-XLAG-KVVECKOR-LT         TO OUTX1-XLAG-KVVECKOR-LT          
016200     MOVE IN1-XLAG-KVVECKOR-FT         TO OUTX1-XLAG-KVVECKOR-FT          
016300     MOVE IN1-XLAG-KVVECKOR-TREND      TO                                 
016310                                         OUTX1-XLAG-KVVECKOR-TREND        
016400                                                                          
016500     MOVE +1                          TO IX1                              
016600     PERFORM UNTIL IX1 > 12                                               
016700       MOVE IN1-XLAG-RESEASON-PLAN(IX1) TO                                
016800                                    OUTX1-XLAG-RESEASON-PLAN(IX1)         
016900       ADD +1                         TO IX1                              
017000     END-PERFORM                                                          
017100                                                                          
017200     MOVE IN1-XLAG-TIDATUM-TREND       TO OUTX1-XLAG-TIDATUM-TREND        
017300     MOVE IN1-XLAG-TILPSP              TO OUTX1-XLAG-TILPSP               
017400                                                                          
017500     MOVE +1                          TO IX1                              
017600     PERFORM UNTIL IX1 > 5                                                
017700       MOVE IN1-XLAG-TILEVDAG(IX1)     TO OUTX1-XLAG-TILEVDAG(IX1)        
017800       ADD +1                         TO IX1                              
017900     END-PERFORM                                                          
018000                                                                          
018100     MOVE IN1-XLAG-TILEVDAT            TO OUTX1-XLAG-TILEVDAT             
018200     MOVE IN1-XLAG-TIMANLED            TO OUTX1-XLAG-TIMANLED             
018300     MOVE IN1-XLAG-TIMANSEC            TO OUTX1-XLAG-TIMANSEC             
018400     MOVE IN1-XLAG-TIOMSPEC            TO OUTX1-XLAG-TIOMSPEC             
018500     MOVE IN1-XLAG-TIREFSTO-LOC        TO OUTX1-XLAG-TIREFSTO-LOC         
018600     MOVE IN1-XLAG-TISLUTKP            TO OUTX1-XLAG-TISLUTKP             
018700     MOVE IN1-XLAG-TIPBJUST-1          TO OUTX1-XLAG-TIPBJUST-1           
018800     MOVE IN1-XLAG-TIPBJUST-2          TO OUTX1-XLAG-TIPBJUST-2           
018900     MOVE IN1-XLAG-IDLEVNR-SHIP-FRAM   TO                                 
019000                                      OUTX1-XLAG-IDLEVNR-SHIP-FRAM        
019010     MOVE IN1-XLAG-FLLARM-BUF          TO OUTX1-XLAG-FLLARM-BUF           
019100     PERFORM S11A-WRITE-W01182X                                           
019200     .                                                                    
019300     EJECT                                                                
019400 S11A-WRITE-W01182X SECTION.                                              
019500                                                                          
019600     WRITE OUTX1-RECORD FROM OUTX1-AREA                                   
019700                                                                          
019800     MOVE SPACE      TO POSTSUM-TRANSTYP                                  
019900     MOVE 'W01182X'  TO POSTSUM-FDNAMN                                    
020000     MOVE 'W01182D3' TO POSTSUM-DDNAMN2                                   
020100     CALL POSTSUM USING POSTSUM-PARM                                      
020200     .                                                                    
020300     EJECT                                                                
020310 S21-WRITE-W01183X SECTION.                                               
020320                                                                          
020330*WDK701                                                                   
020340     MOVE IN2-SAVT-IDARTNR             TO OUTX2-SAVT-IDARTNR              
020350     MOVE IN2-SAVT-IDDC                TO OUTX2-SAVT-IDDC                 
020360*WDK723                                                                   
020370     MOVE IN2-SAVT-IDAVTAL             TO OUTX2-SAVT-IDAVTAL              
020380     MOVE IN2-SAVT-IDLEVNR-AVT         TO OUTX2-SAVT-IDLEVNR-AVT          
020390     MOVE IN2-SAVT-IDLEVNR-SHIP        TO OUTX2-SAVT-IDLEVNR-SHIP         
020400     MOVE IN2-SAVT-TIAVTAL             TO OUTX2-SAVT-TIAVTAL              
020410                                                                          
020420     PERFORM S21A-WRITE-W01183X                                           
020445     .                                                                    
020446     EJECT                                                                
020447 S21A-WRITE-W01183X SECTION.                                              
020448                                                                          
020449     WRITE OUTX2-RECORD FROM OUTX2-AREA                                   
020450                                                                          
020451     MOVE SPACE      TO POSTSUM-TRANSTYP                                  
020452     MOVE 'W01183X'  TO POSTSUM-FDNAMN                                    
020453     MOVE 'W01182D4' TO POSTSUM-DDNAMN2                                   
020454     CALL POSTSUM USING POSTSUM-PARM                                      
020455     .                                                                    
020456     EJECT                                                                
020460 S99-ABEND SECTION.                                                       
020500                                                                          
020600     SKIP2                                                                
020700     MOVE 'S' TO POSTSUM-OPKOD                                            
020800     CALL POSTSUM USING POSTSUM-PARM                                      
020900     CALL ABEND USING RKOD-ABEND                                          
021000     .                                                                    
