000400 ID DIVISION.                                                             
000500*                                                                         
000600 PROGRAM-ID.             W4747000.                                        
001000*AUTHOR.                 IDK.                                             
001100*DATE-WRITTEN.           FEB   81.                                        
001200                                                                          
001400                                                                          
001500*    FUNKTION:  REGISTERRENSNING  LASTNINGSREGISTER                       
001600                                                                          
001900     EJECT                                                                
002000 ENVIRONMENT DIVISION.                                                    
002100     SKIP2                                                                
002200 DATA DIVISION.                                                           
002300     EJECT                                                                
002400 WORKING-STORAGE SECTION.                                                 
002410                                                                          
002420*    -COPY WY2000W9                                                       
002500 77  IDPGM                   PIC X(8)    VALUE 'W4747000'.                
003400     SKIP2                                                                
003500*- - - - - - - - - - - - - -  GENERELLA KONSTANTER                        
003600                                                                          
003700 77  JA                      PIC X       VALUE 'J'.                       
003800 77  NEJ                     PIC X       VALUE 'N'.                       
003900 77  LASTNING-AVSLUTAD       PIC S9(1)   VALUE +9    COMP-3.              
004000                                                                          
004100*- - - - - - - - - - - - - - - - - ARBETSFÄLT                             
004200 01  W.                                                                   
004300                                                                          
004400     03  W-DATUM             PIC 9(6).                                    
004500                                                                          
004600     03  W-PARAMETRAR-TILL-DATKONV.                                       
004700         05  W-DATKONV-KONVTYP    PIC X.                                  
004800         05  W-DATKONV-DATUM      PIC X(6).                               
004900         05  W-DATKONV-RESULTAT   PIC X(5).                               
005000                                                                          
005100     03  W-TILASTN-ATER-AAVVD.                                            
005200         05  W-TILASTN-ATER-AA    PIC 9(2).                               
005300         05  W-TILASTN-ATER-VV    PIC 9(2).                               
005400         05  W-TILASTN-ATER-D     PIC 9.                                  
005410                                                                          
005420     03  ANROP-OK-SW              PIC X  VALUE 'J'.                       
005421         88  ANROP-OK                    VALUE 'J'.                       
005422         88  ANROP-FEL                   VALUE 'N'.                       
005430                                                                          
005500     EJECT                                                                
005600 01  DYNAMISKA-SUBPROGRAM.                                                
005800     03  DATKORT             PIC X(8)    VALUE 'DATKORT '.                
005900     03  DATKONV             PIC X(8)    VALUE 'DATKONV '.                
005910     03  CBLTDLI             PIC X(8)    VALUE 'CBLTDLI '.                
005920     03  FELLOG              PIC X(8)    VALUE 'FELLOG  '.                
006000     03  ABEND               PIC X(8)    VALUE 'ABEND   '.                
006100     SKIP3                                                                
006200 01  RETURKODER.                                                          
006300     03  RKOD-ABEND-UTAN-DUMP    PIC S9(4)  VALUE +16  COMP  SYNC.        
006400     EJECT                                                                
006500 01  DATUMKORT-ID            PIC X(6)    VALUE 'WDATUM'.                  
006600                                                                          
006700*01  -COPY WDATKORTC0                                                     
006900     EJECT                                                                
007000 01  IMS-WS.                                                              
007100   03  FILLER                    PIC X(8)    VALUE 'IMS-WS  '.            
007200     SKIP3                                                                
007300*                            *** STATUSKOD FRÅN IMS                       
007400   03  STATUS-WS                 PIC XX.                                  
007500     88  SEGMENT-FINNS                       VALUE '  '.                  
007600     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
007700     88  BASEN-SLUT                          VALUE 'GB'.                  
007800     SKIP3                                                                
007900   03  GODK-STATUSKODER.                                                  
008000     05  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
008100     SKIP3                                                                
008200   03  SSA1                      PIC X(64).                               
008300   03  SSA2                      PIC X(64).                               
008310   03  SSA3                      PIC X(64).                               
008320     EJECT                                                                
008330*01  -COPY W0003                                                          
008340     EJECT                                                                
008350 01  DLI-IO-AREA.                                                         
008360   03  IO-AREA1                  PIC X(400)  VALUE SPACE.                 
008370     SKIP3                                                                
008380*  03  WLLASA01 -COPY WDM101  -RED IO-AREA1.                              
008390     EJECT                                                                
008400 LINKAGE SECTION.                                                         
008500     SKIP3                                                                
008610*01      -COPY W0008     -PRE LASTN-                                      
008620      05 FILLER          PIC X.                                           
008700     EJECT                                                                
008800 PROCEDURE DIVISION USING LASTN-PCB .                                     
008900                                                                          
009000     ENTRY 'DLITCBL' USING LASTN-PCB .                                    
009100     SKIP3                                                                
009200     PERFORM A-INIT                                                       
009300                                                                          
009400     PERFORM S01-LASTN-LAS-LASTNSEG                                       
009500     PERFORM UNTIL                                                        
009600                   ANROP-FEL                                              
009610*    FIX (IF-SATS)                                                        
009611       IF  LASTN-IDLASTN = 682299 OR 671652                               
009612         CONTINUE                                                         
009613       ELSE                                                               
009614*    FIX (IF-SATS)                                                        
009700       IF  LASTN-KDLASSTA = LASTNING-AVSLUTAD                             
009800          MOVE LASTN-TILASTN-ATER TO W-DATUM                              
009900          MOVE W-DATUM            TO W-DATKONV-DATUM                      
010000          MOVE '3'                TO W-DATKONV-KONVTYP                    
010100         CALL DATKONV USING W-DATKONV-KONVTYP                             
010200                            W-DATKONV-DATUM                               
010300                            W-DATKONV-RESULTAT                            
010400                                                                          
010500         IF  W-DATKONV-KONVTYP NOT = '0'                                  
010600           PERFORM X-ABEND                                                
010700         END-IF                                                           
010800          MOVE W-DATKONV-RESULTAT TO W-TILASTN-ATER-AAVVD                 
010810          MOVE W-TILASTN-ATER-AA  TO TMP1-YY                              
010820          MOVE D-AAR              TO TMP2-YY                              
010830          PERFORM WY2000P9                                                
011000         IF  W-TILASTN-ATER-VV < D-VECKA OR                               
011100         TMP1-YY < TMP2-YY                                                
011200           PERFORM S02-LASTN-DLET                                         
011300         END-IF                                                           
011400       END-IF                                                             
011401*    FIX (END-IF)                                                         
011410       END-IF                                                             
011420*    FIX (END-IF)                                                         
011500       PERFORM S01-LASTN-LAS-LASTNSEG                                     
011600     END-PERFORM                                                          
011700     MOVE ZERO TO RETURN-CODE                                             
011800     GOBACK                                                               
011900     CONTINUE.                                                            
012000     EJECT                                                                
012100 A-INIT SECTION.                                                          
012200     SKIP3                                                                
012300     CALL DATKORT USING IDPGM                                             
012400                        DATUMKORT-ID                                      
012500                        DATUMKORT                                         
012600     CONTINUE.                                                            
012700     EJECT                                                                
012800 X-ABEND  SECTION.                                                        
012900     SKIP1                                                                
013000     DISPLAY                                                              
013100     '***** W4747000  : FEL STATUS FRÅN DATKONV , RKOD : '                
013200             W-DATKONV-KONVTYP '  DATUM : ' W-DATKONV-DATUM               
013300             '   IDLASTNR  :'  LASTN-IDLASTN                              
013400                                                                          
013500     CALL ABEND USING RKOD-ABEND-UTAN-DUMP                                
013600     CONTINUE.                                                            
013700     EJECT                                                                
013800 S01-LASTN-LAS-LASTNSEG SECTION.                                          
013900     SKIP1                                                                
014210     PERFORM IMS-LASTN-GET-LASTNSEG                                       
014220                                                                          
014230     IF  SEGMENT-FINNS                                                    
014270        MOVE 'J'                TO ANROP-OK-SW                            
014280     ELSE                                                                 
014281        MOVE 'N'                TO ANROP-OK-SW                            
014291     END-IF                                                               
014292     CONTINUE.                                                            
014300     SKIP3                                                                
014400 S02-LASTN-DLET SECTION.                                                  
014500     SKIP1                                                                
014810     PERFORM IMS-LASTN-DLET                                               
014820     CONTINUE.                                                            
014900                                                                          
014910*                                                                         
015000*    -COPY WY2000P9                                                       
015010*                                                                         
015100* IMS SECTIONER                                                           
015200     SKIP3                                                                
015300 IMS-LASTN-GET-LASTNSEG SECTION.                                          
015400     SKIP1                                                                
015500     MOVE 'WLLASA01'        TO SSA1                                       
015600     MOVE '  GEGB'          TO GODK-STATUSKODER                           
015700     CALL CBLTDLI USING GHN LASTN-PCB DLI-IO-AREA SSA1                    
015800     MOVE LASTN-STATUS-CODE TO STATUS-WS                                  
015900     PERFORM IMS-STATUSKONTROLL                                           
016000     CONTINUE.                                                            
016100     SKIP3                                                                
016200 IMS-LASTN-DLET SECTION.                                                  
016300     SKIP1                                                                
016400     MOVE SPACE             TO GODK-STATUSKODER                           
016500     CALL CBLTDLI USING DLET LASTN-PCB DLI-IO-AREA SSA1                   
016600     MOVE LASTN-STATUS-CODE TO STATUS-WS                                  
016700     PERFORM IMS-STATUSKONTROLL                                           
016800     CONTINUE.                                                            
016900     EJECT                                                                
017000 IMS-STATUSKONTROLL SECTION.                                              
017100     SET STATUS-IX TO 1                                                   
017200     SEARCH GODK-STATUS AT END CALL FELLOG                                
017300     WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                             
017400     CONTINUE                                                             
017500     END-SEARCH                                                           
017600     CONTINUE                                                             
017700            CONTINUE.                                                     
017800 IMS-EXIT. EXIT.                                                          
017900     CONTINUE.                                                            
