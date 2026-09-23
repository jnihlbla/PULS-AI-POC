000100 ID DIVISION.                                                             
000200 PROGRAM-ID.                 W4406800.                                    
000300 AUTHOR.                     STEFANO GIOBBI.                              
000400     DATE-WRITTEN.           JUN 1991.                                    
000500*                                                                         
000600     REMARKS.                                                             
000700*                                                                         
000800*    FUNKTION.                                                            
000900*                                                                         
001000*    LÄSER ORDERKÖN (WDQ1) MED SB.                                        
001100*    SALDOINFORMATION LISTAS PÅ FIL.                                      
001200*                                                                         
001300     EJECT                                                                
001400 ENVIRONMENT DIVISION.                                                    
001500 INPUT-OUTPUT SECTION.                                                    
001600 FILE-CONTROL.                                                            
001700     SKIP2                                                                
001800                                                                          
001900     SELECT W44068           ASSIGN TO      W44068D1.                     
002000     EJECT                                                                
002100 DATA DIVISION.                                                           
002200 FILE SECTION.                                                            
002300     SKIP2                                                                
002400 FD  W44068                                                               
002500     LABEL RECORD STANDARD                                                
002600     RECORDING F                                                          
002700     BLOCK CONTAINS 0.                                                    
002800*01  POST -COPY W440068 -PRE W44068-  -L.                                 
002900     EJECT                                                                
003000 WORKING-STORAGE SECTION.                                                 
003100     SKIP2                                                                
003200                                                                          
003300*    -- CHECKED BY WY2000                                                 
003400*    ---- GENERELLA KONSTANTER                                            
003500 77  JA                          PIC X       VALUE 'J'.                   
003600 77  NEJ                         PIC X       VALUE 'N'.                   
003700                                                                          
003800 01  DYNAMISKA-SUBPROGRAM.                                                
003900   03  POSTSUM                   PIC X(8)    VALUE 'POSTSUM'.             
004000   03  CBLTDLI                   PIC X(8)    VALUE 'CBLTDLI'.             
004100   03  FELLOG                    PIC X(8)    VALUE 'FELLOG '.             
004200     EJECT                                                                
004300*    ---- PARAMETRAR TILL POSTSUM                                         
004400                                                                          
004500*01  -COPY W0005      -PRE POSTSUM-.                                      
004600     EJECT                                                                
004700*    ---- UTAREA FÖR W44068-POST                                          
004800                                                                          
004900 01  FILLER                      PIC X(16)   VALUE                        
005000                                             'W-W44068-POST'.             
005100     SKIP3                                                                
005200*01  AREA -COPY W440068    -PRE UT-.                                      
005300     EJECT                                                                
005400                                                                          
005500*    ---- ARBETS-AREOR FÖR IMS-SEKTIONERNA.                               
005600                                                                          
005700 01  FILLER                      PIC X(8)   VALUE 'IMS-WS  '.             
005800                                                                          
005900*    ---- STATUSKOD FRÅN IMS                                              
006000                                                                          
006100 01  STATUS-WS                   PIC XX.                                  
006200     88  SEGMENT-FINNS                      VALUE '  '.                   
006300     88  SEGMENT-SLUT                       VALUE 'GB'.                   
006400                                                                          
006500 01  GODK-STATUSKODER.                                                    
006600   03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                  
006700                                                                          
006800 01  SSA1                        PIC X(40).                               
006900     EJECT                                                                
007000*01      -COPY W0003.                                                     
007100     EJECT                                                                
007200 01  FILLER                      PIC X(16)  VALUE                         
007300                                            'DLI-IO-AREA'.                
007400 01  DLI-IO-AREA.                                                         
007500*                                                                         
007600*  03  WLORQM01 -COPY WDQ101                                              
007700     EJECT                                                                
007800 LINKAGE SECTION.                                                         
007900     SKIP2                                                                
008000*    -COPY W0008 -PRE WDQ1-.                                              
008100    05  FILLER                   PIC XX.                                  
008200     EJECT                                                                
008300 PROCEDURE DIVISION  USING WDQ1-PCB.                                      
008400     ENTRY 'DLITCBL' USING WDQ1-PCB.                                      
008500                                                                          
008600 STYR SECTION.                                                            
008700                                                                          
008800     PERFORM A-INIT                                                       
008900     PERFORM IMS-GET-WDQ1                                                 
009000     PERFORM UNTIL SEGMENT-SLUT                                           
009100       PERFORM B-SKAPA-UTPOST                                             
009200       PERFORM IMS-GET-WDQ1                                               
009300     END-PERFORM                                                          
009400                                                                          
009500     PERFORM Z-FINIT                                                      
009600     MOVE    ZERO TO RETURN-CODE                                          
009700     GOBACK                                                               
009800     .                                                                    
009900     EJECT                                                                
010000 A-INIT SECTION.                                                          
010100                                                                          
010200     OPEN OUTPUT W44068                                                   
010300     MOVE 'W4406800'         TO POSTSUM-PROGNAMN                          
010400     MOVE 'W44068D1'         TO POSTSUM-DDNAMN2                           
010500     MOVE 'W44068  '         TO POSTSUM-FDNAMN                            
010600     .                                                                    
010700     EJECT                                                                
010800 B-SKAPA-UTPOST SECTION.                                                  
010900                                                                          
011000     IF OBKR-FLOBOK   = NEJ AND OBKR-IDSYSTEM NOT = 'PROF'                
011100         EVALUATE TRUE                                                    
011200             WHEN  OBKR-KDORDBEK = 10                                     
011300                  PERFORM BA-REDIGERA                                     
011400                                                                          
011500             WHEN (OBKR-KDORDBEK = 15  OR                                 
011600                                   16  OR                                 
011700                                   43  OR                                 
011800                                   44  OR                                 
011900                                   92  OR 99)                             
012000                                       AND                                
012100                  (OBKR-KVPREAVB > ZERO OR OBKR-KVPRERO > ZERO)           
012200                  PERFORM BA-REDIGERA                                     
012300                                                                          
012400             WHEN (OBKR-KDORDBEK = 41)                                    
012500                                       AND                                
012600                  (OBKR-IDARTNR-TILLK > +0)                               
012700                                       AND                                
012800                  (OBKR-KVPREAVB > ZERO OR OBKR-KVPRERO > ZERO)           
012900                                                                          
013000                  PERFORM BA-REDIGERA                                     
013100         END-EVALUATE                                                     
013200     END-IF                                                               
013300     .                                                                    
013400 BA-REDIGERA SECTION.                                                     
013500                                                                          
013600     IF OBKR-IDARTNR-TILLK > +0                                           
013700         MOVE OBKR-IDARTNR-TILLK   TO    UT-IDARTNR                       
013800     ELSE                                                                 
013900         MOVE  OBKR-IDARTNR        TO    UT-IDARTNR                       
014000     END-IF                                                               
014100     MOVE  OBKR-IDDC               TO    UT-IDDC                          
014200     MOVE  OBKR-KDORDKL            TO    UT-KDORDKL                       
014300     IF OBKR-KDORDBEK = 92                                                
014400        COMPUTE UT-KVBEART-Q = OBKR-KVPREAVB + OBKR-KVPRERO               
014500     ELSE                                                                 
014600        MOVE  OBKR-KVBEART-Q       TO    UT-KVBEART-Q                     
014700     END-IF                                                               
014800                                                                          
014900     IF OBKR-IDKAMPRF > +0                                                
015000         MOVE +0                   TO    UT-KVBEART-Q                     
015100     END-IF                                                               
015200                                                                          
015300     IF OBKR-KDORDBEK = 10                                                
015400       IF OBKR-TIRODAT = ZERO                                             
015500         MOVE ZERO                 TO    UT-KVPREAVB                      
015600                                         UT-KVPRERO                       
015700         PERFORM S01-SKRIV                                                
015800       END-IF                                                             
015900     ELSE                                                                 
016000       IF OBKR-IDLEVNR NOT = SPACE                                        
016100         MOVE ZERO                 TO    UT-KVPREAVB                      
016200       ELSE                                                               
016300         MOVE OBKR-KVPREAVB        TO    UT-KVPREAVB                      
016400       END-IF                                                             
016500                                                                          
016600       MOVE  OBKR-KVPRERO          TO    UT-KVPRERO                       
016700                                                                          
016800       PERFORM S01-SKRIV                                                  
016900     END-IF                                                               
017000     .                                                                    
017100     EJECT                                                                
017200 S01-SKRIV SECTION.                                                       
017300                                                                          
017400         WRITE W44068-POST     FROM  UT-AREA                              
017500         MOVE 'OBKR'           TO    POSTSUM-TRANSTYP                     
017600         CALL  POSTSUM         USING POSTSUM-PARM                         
017700     .                                                                    
017800     SKIP2                                                                
017900 Z-FINIT SECTION.                                                         
018000                                                                          
018100     CLOSE W44068                                                         
018200     MOVE  'S'     TO    POSTSUM-OPKOD                                    
018300     CALL  POSTSUM USING POSTSUM-PARM                                     
018400     .                                                                    
018500     EJECT                                                                
018600*                                                                         
018700 IMS-GET-WDQ1 SECTION.                                                    
018800                                                                          
018900     MOVE    '  GAGKGB'       TO    GODK-STATUSKODER                      
019000     CALL    CBLTDLI          USING GN WDQ1-PCB DLI-IO-AREA               
019100     MOVE    WDQ1-STATUS-CODE TO    STATUS-WS                             
019200     PERFORM IMS-STATUSKONTROLL                                           
019300     .                                                                    
019400     SKIP3                                                                
019500 IMS-STATUSKONTROLL SECTION.                                              
019600                                                                          
019700     SET    STATUS-IX TO 1                                                
019800     SEARCH GODK-STATUS                                                   
019900       AT END CALL FELLOG                                                 
020000       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
020100         CONTINUE                                                         
020200     END-SEARCH                                                           
020300     .                                                                    
