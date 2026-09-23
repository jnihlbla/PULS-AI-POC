000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W5128800.                                                
000300 AUTHOR.         BARSHARANI BISHOYE.                                      
000400 DATE-WRITTEN.   13/11/19.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700                                                                          
000800*    FUNCTION:                                                            
000900*        STOCK BALANCE MONTHLY REPORT                                     
001000*        STOCK BALANCE FROM THE FILE W51284 IS USED TO CREATE THE         
001100*        REPORT.                                                          
001500*                                                                         
001600*        THE PROGRAM READS   WDB6                                         
001700*                                                                         
001800                                                                          
001900     SKIP3                                                                
002000 ENVIRONMENT DIVISION.                                                    
002100     SKIP2                                                                
002200 INPUT-OUTPUT SECTION.                                                    
002300                                                                          
002400 FILE-CONTROL.                                                            
002500     SKIP2                                                                
002600*          --- INPUT FILE                                                 
002700     SELECT W51284                     ASSIGN TO W51288D1.                
002800     SKIP2                                                                
002900*          --- OUTPUT REPORT FILE                                         
003000     SELECT W51288                     ASSIGN TO W51288D2.                
003100     EJECT                                                                
003200 DATA DIVISION.                                                           
003300     SKIP3                                                                
003400 FILE SECTION.                                                            
003500     SKIP3                                                                
003600 FD  W51284                                                               
003700     RECORDING       F                                                    
003800     BLOCK CONTAINS  0.                                                   
003900*01  RECORD -COPY W51284 -PRE  IN-  -L.                                   
004000     EJECT                                                                
004100     SKIP3                                                                
004200 FD  W51288                                                               
004300     RECORDING       V                                                    
004400     BLOCK CONTAINS  0.                                                   
004401                                                                          
004410 01  LIST-POST           PIC X(45).                                       
004700     EJECT                                                                
004800 WORKING-STORAGE SECTION.                                                 
004900                                                                          
005000 77  IDPGM                       PIC X(8)    VALUE 'W5128800'.            
005100 77  YES                         PIC X       VALUE 'J'.                   
005200 77  NOO                         PIC X       VALUE 'N'.                   
005300     SKIP2                                                                
005400 01  ERROR-TEXT.                                                          
005500     03  ERROR-TEXT-STR          PIC X(72)   VALUE SPACE.                 
005600                                                                          
005700 77  W51284-EOF-SW               PIC X       VALUE 'N'.                   
005800     88  END-OF-W51284                       VALUE 'Y'.                   
005810 77  WS-KDTRADP                  PIC X(4)    VALUE SPACE.                 
005900     EJECT                                                                
006000 01  GENERAL-SUBPROGRAMS.                                                 
006100*                                                                         
006200     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
006300     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
006400     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
006401                                                                          
006410 01  CURR-KDTRADP        PIC X(4)    VALUE SPACE.                         
006420                                                                          
006430 01  W001-DAP.                                                            
006440     03  FILLER                  PIC X(165)  VALUE SPACE.                 
006500     EJECT                                                                
006510*01  -COPY WWDC99                                                         
006520     EJECT                                                                
006600*    --- PARAMETRAR TILL POSTSUM                                          
006700*                                                                         
006800*01  -COPY W0005   -PRE  POSTSUM-                                         
006900     EJECT                                                                
007000 01  IN-AREA-START               PIC X(24)   VALUE                        
007100                                             'IN-AREA-START'.             
007200 01  IN-AREA.                                                             
007300*    03  -COPY W51284  -PRE  IN-                                          
007400     SKIP2                                                                
007500                                                                          
007600     EJECT                                                                
007700 01  TEXT-AREA.                                                           
007900     03  UT-HEADER.                                                       
008000         05  FILLER           PIC X(11) VALUE 'PART NUMBER'.              
008100         05  FILLER           PIC X(1)  VALUE ';'.                        
008200         05  FILLER           PIC X(2)  VALUE 'DC'.                       
008300         05  FILLER           PIC X(1)  VALUE ';'.                        
008600         05  FILLER           PIC X(13) VALUE 'STOCK BALANCE'.            
008700         05  FILLER           PIC X(1)  VALUE ';'.                        
008800         05  FILLER           PIC X(8)  VALUE 'AVG COST'.                 
008900         05  FILLER           PIC X(1)  VALUE ';'.                        
009000         05  FILLER           PIC X(2)  VALUE 'PG'.                       
009100                                                                          
009200     03 UT-AREA.                                                          
009210         05 UT-IDARTNR        PIC Z(7)9.                                  
009211         05 FILLER            PIC X(1)    VALUE ';'.                      
009220         05 UT-IDDC           PIC X(2).                                   
009221         05 FILLER            PIC X(1)    VALUE ';'.                      
009230         05 UT-KVLS           PIC -(7)9.                                  
009231         05 FILLER            PIC X(1)    VALUE ';'.                      
009240         05 UT-PRAVCOST       PIC Z(6)9.9(2).                             
009241         05 FILLER            PIC X(1)    VALUE ';'.                      
009250         05 UT-KDPSLLOC       PIC 9(2).                                   
009260                                                                          
009700 01  KEYS-TILL-DLI.                                                       
010110     03  W-IDDC-X.                                                        
010120         05  W-IDDC              PIC X(2)    VALUE SPACE.                 
010200     SKIP2                                                                
010300*    --- STATUS-KOD FRÅN IMS                                              
010400 01  STATUS-WS                   PIC XX.                                  
010500     88  SEGMENT-FOUND                       VALUE '  '.                  
010600     88  SEGMENT-FOUND-EXISTS                VALUE 'II'.                  
010700     88  SEGMENT-MISSING                     VALUE 'GE'.                  
010800     88  SEGMENT-NOMORE                      VALUE 'GB'.                  
010900     88  IMS-NOT-OK                          VALUE 'XD'.                  
011000     SKIP2                                                                
011100 01  GOOD-STATUSCODES.                                                    
011200     03  GOOD-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
011300     SKIP3                                                                
011400 01  SSA1                        PIC X(64).                               
011600     EJECT                                                                
011700*    --- IMS FUNCTION CODES                                               
011800*01  -COPY W0003                                                          
011900     EJECT                                                                
012000*    ---  DLI INPUT-OUTPUT AREA                                           
012100                                                                          
012910 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDB601'.                      
012920 01  DLI-IO-WDB601.                                                       
012930*    03  -COPY WDB601                                                     
012940     EJECT                                                                
013000 LINKAGE SECTION.                                                         
013100                                                                          
013410*01  -COPY W0008  -PRE WDB6-                                              
013420     05  FILLER                  PIC X.                                   
013430     EJECT                                                                
013500 PROCEDURE DIVISION  USING WDB6-PCB.                                      
013600 MAIN SECTION.                                                            
013700     ENTRY 'DLITCBL' USING WDB6-PCB.                                      
013800                                                                          
013900     SKIP2                                                                
014000     PERFORM A-INIT                                                       
014100     PERFORM S01-READ-W51284                                              
014600     PERFORM UNTIL END-OF-W51284                                          
015305       MOVE IN-IDDC  TO W-IDDC                                            
015310       PERFORM IMS-GU-WDB601                                              
015320       IF SEGMENT-FOUND                                                   
015330         MOVE DCS-KDTRADP  TO WS-KDTRADP                                  
015340       ELSE                                                               
015350         MOVE SPACE        TO WS-KDTRADP                                  
015360       END-IF                                                             
015363       PERFORM B-MOVE-DATA                                                
015364       PERFORM S01-READ-W51284                                            
015800     END-PERFORM                                                          
015900                                                                          
016000                                                                          
016100     PERFORM Z-FINIT                                                      
016200                                                                          
016300     MOVE ZERO TO RETURN-CODE                                             
016400     GOBACK                                                               
016500     .                                                                    
016600     EJECT                                                                
016700 A-INIT SECTION.                                                          
016800     SKIP2                                                                
016900                                                                          
017000     OPEN INPUT W51284                                                    
017100                                                                          
017200     OPEN OUTPUT W51288                                                   
017300                                                                          
017400                                                                          
017500     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
017600     .                                                                    
017700     EJECT                                                                
017800 B-MOVE-DATA SECTION.                                                     
017802                                                                          
017804     MOVE IN-IDDC      TO WS-IDDC                                         
017805     IF XDC-NON-VCC-OWNED                                                 
017806     OR NDC-NA                                                            
017807       IF WS-KDTRADP NOT = CURR-KDTRADP                                   
017808          MOVE WS-KDTRADP  TO CURR-KDTRADP                                
017810       PERFORM S10-SKRIV-DAP1                                             
017820       PERFORM S11-SKRIV-DAP2                                             
017830       WRITE LIST-POST   FROM UT-HEADER                                   
017840       END-IF                                                             
017842     END-IF                                                               
017860     IF IN-KVLS-TOT <     ZERO                                            
017870     OR IN-KVLS-TOT >     ZERO                                            
017900       MOVE IN-IDARTNR    TO UT-IDARTNR                                   
018000       MOVE IN-IDDC       TO UT-IDDC                                      
018100       MOVE IN-KVLS-TOT   TO UT-KVLS                                      
018200       MOVE IN-PRAVCOST   TO UT-PRAVCOST                                  
018300       MOVE IN-KDPSLLOC   TO UT-KDPSLLOC                                  
018801       WRITE LIST-POST   FROM UT-AREA                                     
018803       PERFORM S11-WRITE-W51288                                           
018810     END-IF                                                               
018900     .                                                                    
019000     EJECT                                                                
019100 Z-FINIT SECTION.                                                         
019200                                                                          
019300                                                                          
019400     CLOSE W51284                                                         
019500                                                                          
019600           W51288                                                         
019700     SKIP2                                                                
019800     MOVE 'S' TO POSTSUM-OPKOD                                            
019900     CALL POSTSUM USING POSTSUM-PARM                                      
020000     .                                                                    
020100     EJECT                                                                
020200 S01-READ-W51284  SECTION.                                                
020300     SKIP2                                                                
020400     READ W51284 INTO IN-AREA                                             
020500     AT END                                                               
020600        SET END-OF-W51284 TO TRUE                                         
020700                                                                          
020800     NOT AT END                                                           
020810        MOVE IN-IDDC  TO W-IDDC                                           
020820                          WS-IDDC                                         
020900        MOVE 'W51284' TO POSTSUM-FDNAMN                                   
021000        MOVE 'W51288D1' TO POSTSUM-DDNAMN2                                
021100        CALL POSTSUM USING POSTSUM-PARM                                   
021200     END-READ                                                             
021300     .                                                                    
021400     EJECT                                                                
021500 S11-WRITE-W51288 SECTION.                                                
021600     SKIP2                                                                
021700                                                                          
021800     MOVE 'W51288 ' TO POSTSUM-FDNAMN                                     
021900     MOVE 'W51288D2' TO POSTSUM-DDNAMN2                                   
022000     CALL POSTSUM USING POSTSUM-PARM                                      
022100     .                                                                    
022200     EJECT                                                                
022300* --- IMS SECTIONS  ---                                                   
022400                                                                          
022500     EJECT                                                                
022600 S10-SKRIV-DAP1 SECTION.                                                  
022610                                                                          
022620     MOVE ' ¤DAPW51288' TO W001-DAP                                       
022630     WRITE LIST-POST FROM W001-DAP                                        
022640                                                                          
022650     MOVE SPACE TO W001-DAP                                               
022660     .                                                                    
022670                                                                          
022680 S11-SKRIV-DAP2 SECTION.                                                  
022690                                                                          
022691     STRING ' ¤DAP' CURR-KDTRADP                                          
022692            DELIMITED BY SIZE INTO W001-DAP                               
022693     WRITE LIST-POST FROM W001-DAP                                        
022694                                                                          
022695     MOVE SPACE TO W001-DAP                                               
022696     .                                                                    
023810 IMS-GU-WDB601 SECTION.                                                   
023820                                                                          
023830     STRING 'WDB601  (IDDC     =' W-IDDC-X ')'                            
023840          DELIMITED BY SIZE INTO SSA1                                     
023850     MOVE '  GE' TO GOOD-STATUSCODES                                      
023860     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-WDB601 SSA1                    
023870     MOVE WDB6-STATUS-CODE TO STATUS-WS                                   
023880     PERFORM IMS-STATUSCHECK                                              
023890     .                                                                    
023891     SKIP3                                                                
023900 IMS-STATUSCHECK SECTION.                                                 
024000     SKIP2                                                                
024100     SET STATUS-IX TO 1                                                   
024200     SEARCH GOOD-STATUS                                                   
024300       AT END                                                             
024400         STRING ' STATUS CODE FROM IMS: ' STATUS-WS                       
024500           DELIMITED BY SIZE INTO ERROR-TEXT                              
024700         CALL FELLOG                                                      
024800       WHEN GOOD-STATUS (STATUS-IX) = STATUS-WS                           
024900         CONTINUE                                                         
025000     END-SEARCH                                                           
025100     .                                                                    
