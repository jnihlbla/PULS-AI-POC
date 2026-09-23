000100 ID DIVISION.                                                             
000200 PROGRAM-ID.         W2150400.                                            
000300 AUTHOR.             IDK, GÖTEBORG.                                       
000410 DATE-WRITTEN.       NOV 1978.                                            
000500                                                                          
000700*    FUNCTION.                                                            
000800*        PROGRAMMET SKRIVER UT LISTA MED FELMEDDELANDEN FÖR               
000900*        SATSER TILL ANSKAFFARE. INFORMATIONSFILEN (W21507)               
001000*        ÄR SKAPAD I PROGRAM W21502.                                      
001100*        PGM. OMSKRIVET JAN -88 FRÅN REPORT WRITER TILL COBOL             
001200*    RETURKODER:                                                          
001300*            +20             FEL VID SORTERING                            
001400*    SUBPROGRAM:                                                          
001500*            ABEND                                                        
001600*            POSTSUM                                                      
001700*            DATUMKORT                                                    
001800     EJECT                                                                
001900 ENVIRONMENT DIVISION.                                                    
002000 INPUT-OUTPUT SECTION.                                                    
002100 FILE-CONTROL.                                                            
002200     SKIP2                                                                
002300*                            *** SORTFIL                       ***        
002400*                            *** FÖR W21507                    ***        
002500     SELECT SORTFIL ASSIGN W21504DS.                                      
002600*                            *** INFORMATION OM SATSER         ***        
002700*                            *** INPUT                         ***        
002800     SELECT W21507 ASSIGN W21504D1.                                       
002900*                            *** LISTA                         ***        
003000*                            *** SATS-INFORMATION              ***        
003100     SELECT LISTA  ASSIGN W21504D2.                                       
003200     EJECT                                                                
003300 DATA DIVISION.                                                           
003400 FILE SECTION.                                                            
003500 SD  SORTFIL                                                              
003600     RECORDING V.                                                         
003700*01  POST   -COPY W215LI02    -PRE SORT-.                                 
003900     EJECT                                                                
004000 FD  W21507                                                               
004100     RECORDING V                                                          
004200     BLOCK 0                                                              
004300     LABEL RECORD STANDARD.                                               
004400*01         -COPY W215LI02    -L.                                         
004600     EJECT                                                                
004700 FD  LISTA                                                                
004800     RECORDING F                                                          
004900     BLOCK 0                                                              
005000     LABEL RECORD STANDARD.                                               
005100 01  LISTPOST      PIC X(121).                                            
005200     EJECT                                                                
005300 WORKING-STORAGE SECTION.                                                 
005400     SKIP2                                                                
005401                                                                          
005410*    -- CHECKED BY WY2000                                                 
005500*********RÄKNARE**************************************************        
005600 01  SIDRAK                  PIC 9(5)    COMP-3 VALUE ZERO.               
005700 01  RADRAK                  PIC 9(2)    COMP-3 VALUE ZERO.               
005800******************************************************************        
005900 01  IDANSKJAMFOR            PIC S9(3)   COMP-3 VALUE ZERO.               
005910 01  IDARTJAMFOR             PIC S9(9)   COMP-3 VALUE ZERO.               
006000 01  RKOD                    PIC S9(4)   VALUE +0    COMP SYNC.           
006100 01  KONSTANTER.                                                          
006200     03  JA                  PIC X       VALUE 'J'.                       
006300     03  NEJ                 PIC X       VALUE 'N'.                       
006400     SKIP3                                                                
006500 01  EOF-SWITCHAR.                                                        
006600     03  SORTFIL-EOF         PIC X   VALUE 'N'.                           
006700 01  DYNAMISKA-SUBPROGRAM.                                                
006800     03  DATKORT             PIC X(8)    VALUE 'DATKORT'.                 
006900     03  POSTSUM             PIC X(8)    VALUE 'POSTSUM'.                 
007000     03  ABEND               PIC X(8)    VALUE 'ABEND  '.                 
007200     EJECT                                                                
007300*                            *** PARAMETRAR TILL DATKORT                  
007400 01  PROGRAM-NAMN            PIC X(6)    VALUE 'R     '.                  
007500 01  DATUMKORT-ID            PIC X(6)    VALUE 'WDATUM'.                  
007600*01  -COPY WDATKORT                                                       
007800*                            *** PARAMETRAR TILL POSTSUM                  
007900*01  -COPY W0005       -PRE POSTSUM-.                                     
008100     EJECT                                                                
008200*                            *************************************        
008300*                            **  AREA FÖR W21507                **        
008400*                            **                                 **        
008500*                            *************************************        
008600*01  AREA  -COPY W215LI02   -PRE S00LI2-.                                 
008800     EJECT                                                                
008900******************* - RUBRIKER-*********************************          
009000 01  RUB1.                                                                
009100         03  FILLER          PIC X(21)   VALUE '  VOLVO PARTS'.           
009200         03  FILLER          PIC X(12)   VALUE 'W21504-001'.              
009300         03  FILLER          PIC X(51)   VALUE                            
009400                                      'FELLISTA SATSBEORDRING'.           
009500         03  FILLER          PIC X(5)    VALUE 'DATUM'.                   
009600         03  AAR             PIC Z99     VALUE ZERO.                      
009700         03  MAANAD          PIC Z99     VALUE ZERO.                      
009800         03  DAG             PIC Z99     VALUE ZERO.                      
009900         03  FILLER          PIC X(8)    VALUE '    SIDA'.                
010000         03  SIDMARKERING    PIC Z(4)9   VALUE ZERO.                      
010100     SKIP2                                                                
010200 01  RUB2.                                                                
010300         03  FILLER          PIC X(6)    VALUE SPACE.                     
010400         03  FILLER          PIC X(15)   VALUE 'ANSKAFFARE'.              
010500         03  FILLER          PIC X(12)   VALUE 'SATSNR'.                  
010600         03  FILLER          PIC X(10)   VALUE 'FELORSAK'.                
010700     SKIP2                                                                
010800 01  FELANM-RAD.                                                          
010900         03  FILLER          PIC X(13)   VALUE SPACE.                     
011000         03  IDANSK-FEL      PIC Z(3)    VALUE ZERO.                      
011100         03  FILLER          PIC X(2)    VALUE SPACE.                     
011200         03  IDARTNR-FEL     PIC Z(9)    VALUE ZERO.                      
011300         03  FILLER          PIC X(6)    VALUE SPACE.                     
011400         03  FELMEDDELANDE   PIC X(30)   VALUE SPACE.                     
011500     EJECT                                                                
011600 PROCEDURE DIVISION.                                                      
011700                                                                          
011800                                                                          
011900     PERFORM A-INITIERA                                                   
012000                                                                          
012100     SORT SORTFIL                                                         
012200     ASCENDING SORT-IDANSK SORT-IDARTNR-SATS                              
012300     USING W21507                                                         
012400     OUTPUT PROCEDURE B-SATSINFO-LISTA                                    
012500                                                                          
012600     PERFORM Z-AVSLUTA                                                    
012700                                                                          
012800     IF SORT-RETURN GREATER ZERO                                          
012900       DISPLAY '*** W2150400 FEL VID SORTERING'                           
013000       MOVE 20 TO RKOD                                                    
013100       CALL ABEND USING RKOD                                              
013200     ELSE                                                                 
013300       MOVE ZERO TO RETURN-CODE                                           
013400       GOBACK                                                             
013500     END-IF                                                               
013600     .                                                                    
013700     EJECT                                                                
013800 A-INITIERA SECTION.                                                      
013900                                                                          
014000     OPEN OUTPUT LISTA                                                    
014100                                                                          
014200     CALL DATKORT USING PROGRAM-NAMN DATUMKORT-ID DATUMKORT               
014300                                                                          
014400     .                                                                    
014500     EJECT                                                                
014600 B-SATSINFO-LISTA SECTION.                                                
014700                                                                          
014800     PERFORM BA-LAES-SORTFIL-W21507                                       
014810     IF SORTFIL-EOF = NEJ                                                 
014900       PERFORM BB-INITIERA-LISTA                                          
015000       PERFORM UNTIL SORTFIL-EOF = JA OR RADRAK > ZERO                    
015100                                                                          
015200          PERFORM BC-SIDRUB                                               
015300                                                                          
015400             PERFORM UNTIL RADRAK >= 42 OR                                
015500                           S00LI2-IDANSK NOT = IDANSKJAMFOR               
015600                                                                          
015610                IF S00LI2-IDARTNR-SATS = IDARTJAMFOR                      
015620                   CONTINUE                                               
015630                ELSE                                                      
015700                   PERFORM BD-FEL-RAD                                     
015701                   MOVE S00LI2-IDARTNR-SATS TO IDARTJAMFOR                
015710                END-IF                                                    
015800                PERFORM BA-LAES-SORTFIL-W21507                            
015900                                                                          
016000             END-PERFORM                                                  
016100                                                                          
016200             MOVE ZERO TO RADRAK                                          
016210                          IDARTJAMFOR                                     
016300             MOVE S00LI2-IDANSK TO IDANSKJAMFOR                           
016400                                                                          
016500       END-PERFORM                                                        
016510     END-IF                                                               
016600     .                                                                    
016700     EJECT                                                                
016800 BA-LAES-SORTFIL-W21507 SECTION.                                          
016900                                                                          
017000     RETURN SORTFIL INTO S00LI2-AREA                                      
017100       AT END                                                             
017200         MOVE 43 TO RADRAK                                                
017300         MOVE JA TO SORTFIL-EOF                                           
017400     END-RETURN.                                                          
017500                                                                          
017600     IF SORTFIL-EOF = NEJ                                                 
017700       MOVE 'W21507' TO POSTSUM-FDNAMN                                    
017800       MOVE 'W21504DS' TO POSTSUM-DDNAMN2                                 
017900       MOVE S00LI2-SORTHELP TO POSTSUM-TRANSTYP                           
018000       CALL POSTSUM USING POSTSUM-PARM                                    
018100     END-IF                                                               
018200*                                                                         
018300     .                                                                    
018400 BB-INITIERA-LISTA SECTION.                                               
018500*                                                                         
018600     MOVE ZERO TO RADRAK                                                  
018700     MOVE S00LI2-IDANSK TO IDANSKJAMFOR                                   
018710     MOVE ZERO          TO IDARTJAMFOR                                    
018800     MOVE D-AAR TO AAR                                                    
018900     MOVE D-MAANAD TO MAANAD                                              
019000     MOVE D-DAG TO DAG                                                    
019100     .                                                                    
019200     EJECT                                                                
019300 BC-SIDRUB SECTION.                                                       
019400*                                                                         
019500     ADD 1 TO SIDRAK                                                      
019600     MOVE SIDRAK TO SIDMARKERING                                          
019700     WRITE LISTPOST FROM RUB1 AFTER PAGE                                  
019800     ADD 4 TO RADRAK                                                      
019900     WRITE LISTPOST FROM RUB2 AFTER 2                                     
020000     ADD 2 TO RADRAK                                                      
020100     .                                                                    
020200     SKIP3                                                                
020300 BD-FEL-RAD SECTION.                                                      
020400*                                                                         
020500     IF RADRAK < 7                                                        
020600       MOVE S00LI2-IDANSK TO IDANSK-FEL                                   
020700     ELSE                                                                 
020800       MOVE ZERO TO IDANSK-FEL                                            
020900     END-IF                                                               
021000     MOVE S00LI2-IDARTNR-SATS TO IDARTNR-FEL                              
021100     MOVE S00LI2-FELMEDDELANDE TO FELMEDDELANDE                           
021200     WRITE LISTPOST FROM FELANM-RAD AFTER 2                               
021300     ADD 2 TO RADRAK                                                      
021400     .                                                                    
021500     EJECT                                                                
021600 Z-AVSLUTA SECTION.                                                       
021700                                                                          
021800     CLOSE LISTA                                                          
021900                                                                          
022000     MOVE 'S' TO POSTSUM-OPKOD                                            
022100     CALL POSTSUM USING POSTSUM-PARM                                      
022200     .                                                                    
