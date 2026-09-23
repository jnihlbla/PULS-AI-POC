000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W5221200.                                                
000300 AUTHOR.         NILSSON LINDA.                                           
000400 DATE-WRITTEN.   02/06/04.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700                                                                          
000800*                                                                         
000900*    FUNCTION:                                                            
001000*                                                                         
001100*        THE PROGRAM READS FILE W52201 AND CREATES A                      
001200*        INTRASTAT TRANSACTION FOR VOLVO TRANSPORT AND                    
001300*        A REQUEST FILE FOR MISCELLANEOUS CLIENTS.                        
001400*                                                                         
001500                                                                          
001600     SKIP3                                                                
001700 ENVIRONMENT DIVISION.                                                    
001800     SKIP2                                                                
001900 INPUT-OUTPUT SECTION.                                                    
002000                                                                          
002100 FILE-CONTROL.                                                            
002200     SKIP2                                                                
002300*          --- SELECTED INTRASTAT-DATA FROM IVW-TABLE -INPUT              
002400     SELECT W52201                     ASSIGN TO W52212D1.                
002500     SKIP2                                                                
002600*          --- INTRASTAT TRANSACTIONS VOLVO TRANSPORT -OUTPUT             
002700     SELECT W52212                     ASSIGN TO W52212D2.                
002800     SKIP2                                                                
002900 DATA DIVISION.                                                           
003000     SKIP3                                                                
003100 FILE SECTION.                                                            
003200     SKIP3                                                                
003300 FD  W52201                                                               
003400     RECORDING       F                                                    
003500     BLOCK CONTAINS  0.                                                   
003600                                                                          
003700 01  IN-POST.                                                             
003800*    03  -COPY W522INT   -L.                                              
003900     SKIP3                                                                
004000                                                                          
004100 FD  W52212                                                               
004200     RECORDING       F                                                    
004300     BLOCK CONTAINS  0.                                                   
004400                                                                          
004500 01  W52212-POST.                                                         
004600*    03  -COPY W475INT   -L.                                              
004700     SKIP3                                                                
004800                                                                          
004900 WORKING-STORAGE SECTION.                                                 
005000                                                                          
005100 77  IDPGM                       PIC X(8)    VALUE 'W5221200'.            
005200 77  YES                         PIC X       VALUE 'J'.                   
005300 77  NOO                         PIC X       VALUE 'N'.                   
005400     SKIP2                                                                
005500 01  IDPTYP.                                                              
005600     03  WS-IDPTYP               PIC X(3)    VALUE SPACE.                 
005700     03  WS-IDPTYP-HEAD          PIC X(3)    VALUE SPACE.                 
005800                                                                          
005900 01  WS-BEFORMS                  PIC X(15)   VALUE SPACE.                 
006000                                                                          
006100 01  WS-IDLEVNR                  PIC X(5)    VALUE '00000'.               
006200                                                                          
006300 01  WS-IDLANDX3-REC-OLD         PIC X(3)    VALUE SPACE.                 
006400                                                                          
006500 01  IDARTNR-UNSTRING.                                                    
006600     03  WS-IDARTNR-UNSTR        PIC X(9)    VALUE SPACE.                 
006700                                                                          
006800 01  CALCULATE-AREA.                                                      
006900     03 WS-SEK                   PIC S9(13)  COMP-3.                      
007000     03 WS-SUNTO                 PIC S9(13)  COMP-3.                      
007100     03 WS-SUFKTBEL              PIC S9(11)  COMP-3.                      
007200     SKIP2                                                                
007300 77  W52201-EOF-SW               PIC X       VALUE 'N'.                   
007400     88  END-OF-W52201                       VALUE 'J'.                   
007500     EJECT                                                                
007600 77  IDLANDX3-REC-SW             PIC X       VALUE 'N'.                   
007700     88  IDLANDX3-REC-FIRST                  VALUE 'J'.                   
007800     EJECT                                                                
007900 01  ERRTEXT.                                                             
008000     03  FILLER                  PIC X(8)    VALUE 'ERRTEXT'.             
008100     03  ERRTEXT-STR             PIC X(72)   VALUE SPACE.                 
008200 01  KDRC-DISPLAY                PIC Z(5).                                
008300                                                                          
008400 01  GENERAL-SUBPROGRAMS.                                                 
008500     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
008600     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
008700     03  ABEND                   PIC X(8)    VALUE 'ABEND   '.            
008800     EJECT                                                                
008900*    --- PARAMETRAR TILL ABEND                                            
009000 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
009100 77  RKOD-ABEND-NO-DUMP          PIC S9(4)   COMP VALUE +16.              
009200 77  RKOD-ABEND-WITH-DUMP        PIC S9(4)   COMP VALUE +1000.            
009300     EJECT                                                                
009400                                                                          
009500*    --- INFIL                                                            
009600 01  IN-AREA-START               PIC X(24)   VALUE                        
009700                                                 'IN-AREA-START'.         
009800 01  IN-AREA.                                                             
009900*    03  -COPY W522INT     -PRE IN-                                       
010000     EJECT                                                                
010100                                                                          
010200*    --- UTFIL W52212                                                     
010300*    --- FIL VOLVO TRANSPORT                                              
010400 01  W52212-AREA-START              PIC X(24)   VALUE                     
010500                                    'W52212-AREA-START       '.           
010600 01  W52212-AREA.                                                         
010700*    03  -COPY W475INT     -PRE W52212-                                   
010800     EJECT                                                                
010900                                                                          
011000 LINKAGE SECTION.                                                         
011100                                                                          
011200*01  -COPY W0009   -PRE MSG-                                              
011300     EJECT                                                                
011400 PROCEDURE DIVISION  USING MSG-PCB.                                       
011500 MAIN SECTION.                                                            
011600     ENTRY 'DLITCBL' USING MSG-PCB.                                       
011700                                                                          
011800                                                                          
011900     SKIP2                                                                
012000     PERFORM A-INIT                                                       
012100     PERFORM S01-READ-W52201                                              
012200     PERFORM UNTIL END-OF-W52201                                          
012300*    --- TESTAR SÄNDANDE LAND MOT LAND09-INT-UT                           
012400       PERFORM B-EXECUTE                                                  
012500       PERFORM S01-READ-W52201                                            
012600     END-PERFORM                                                          
012700                                                                          
012800     PERFORM Z-FINIT                                                      
012900                                                                          
013000     MOVE ZERO TO RETURN-CODE                                             
013100     GOBACK                                                               
013200     .                                                                    
013300     EJECT                                                                
013400 A-INIT SECTION.                                                          
013500                                                                          
013600     OPEN INPUT  W52201                                                   
013700     OPEN OUTPUT W52212                                                   
013800     .                                                                    
013900     EJECT                                                                
014000 B-EXECUTE SECTION.                                                       
014100                                                                          
014200     IF IN-KDFINDOC = 'ECO'                                               
014210       MOVE +99          TO W52212-KDINTTYP                               
014300       IF IN-IDLANDX3-SEND > SPACE                                        
014400         IF IN-IDLANDX3-SEND = 'SE'                                       
014500           PERFORM BA-CREATE-W52212                                       
014600           PERFORM S11-WRITE-W52212                                       
014700         END-IF                                                           
014800       ELSE                                                               
014900         IF IN-IDLANDX3-REC > SPACE                                       
015000           IF IN-IDLANDX3-REC = 'SE'                                      
015100             PERFORM BA-CREATE-W52212                                     
015200             PERFORM S11-WRITE-W52212                                     
015300           END-IF                                                         
015400         END-IF                                                           
015500       END-IF                                                             
015600     END-IF                                                               
015700                                                                          
015800     IF IN-KDFINDOC = 'EXC'                                               
015810       MOVE +41        TO W52212-KDINTTYP                                 
015900       IF IN-IDLANDX3-SEND = 'SE'                                         
016000         PERFORM BA-CREATE-W52212                                         
016100         PERFORM S11-WRITE-W52212                                         
016200       END-IF                                                             
016300                                                                          
016400       IF IN-IDLANDX3-REC = 'SE'                                          
016500         PERFORM BA-CREATE-W52212                                         
016600         PERFORM S11-WRITE-W52212                                         
016700       END-IF                                                             
016800     END-IF                                                               
016900                                                                          
017000     IF IN-KDFINDOC NOT = 'ECO' AND 'EXC'                                 
017001       IF IN-KDFINDOC = 'INV'                                             
017002       OR IN-KDFINDOC = 'INL'                                             
017010         MOVE +11    TO W52212-KDINTTYP                                   
017020       ELSE                                                               
017021         IF IN-KDFINDOC = 'CR'                                            
017022           MOVE +21  TO W52212-KDINTTYP                                   
017023         ELSE                                                             
017024           MOVE +99  TO W52212-KDINTTYP                                   
017025         END-IF                                                           
017030       END-IF                                                             
017100       IF IN-IDLANDX3-SEND = 'SE'                                         
017200         PERFORM BA-CREATE-W52212                                         
017300         PERFORM S11-WRITE-W52212                                         
017400       END-IF                                                             
017500     END-IF                                                               
017600     .                                                                    
017700     EJECT                                                                
017800 BA-CREATE-W52212 SECTION.                                                
017900                                                                          
018000*    --- BYGG UPP POST W52212                                             
018100     IF IN-KDFINDOC = 'INV'                                               
018200       MOVE 'FAK'             TO W52212-IDPTYP                            
018300     ELSE                                                                 
018400       MOVE 'INT'             TO W52212-IDPTYP                            
018500     END-IF                                                               
018510     MOVE IN-IDLANDX3-BET     TO W52212-IDLANDX2                          
018600     IF IN-KDFINDOC = 'ECO' OR 'INL'                                      
018700       IF IN-IDLANDX3-BET = SPACE                                         
018800         MOVE IN-IDLANDX3-REC TO W52212-IDLANDX2                          
018900       END-IF                                                             
019200     END-IF                                                               
019300     MOVE IN-DAFINDOC         TO W52212-TIFAKT                            
019400     MOVE IN-IDARTNR          TO WS-IDARTNR-UNSTR                         
019500     INSPECT WS-IDARTNR-UNSTR REPLACING LEADING SPACE BY ZERO             
019600     MOVE WS-IDARTNR-UNSTR    TO W52212-IDARTNR                           
019700     MOVE IN-KVLEVART         TO W52212-KVLEVART                          
019800     MOVE IN-IDSTATNR         TO W52212-IDSTATNR                          
019900     MOVE IN-KDARTURS         TO W52212-KDARTURS                          
019910     MOVE IN-IDVAT-BET        TO W52212-IDVAT                             
019920     IF IN-IDLANDX3-BET = SPACE                                           
019930       MOVE IN-IDVAT-AGENT    TO W52212-IDVAT                             
019940     END-IF                                                               
020000     IF IN-KDVALISO      NOT = 'SEK'                                      
020100*    --- BERÄKNA BELOPP I SEK                                             
020200       COMPUTE WS-SUFKTBEL = IN-SUNTO * IN-PRKURS                         
020300       MOVE WS-SUFKTBEL       TO W52212-SUFKTBEL                          
020400     ELSE                                                                 
020500       MOVE IN-SUNTO          TO W52212-SUFKTBEL                          
020600     END-IF                                                               
020700     IF IN-KDFINDOC = 'CR'                                                
020710       MOVE IN-IDVAT-RESP     TO W52212-IDVAT                             
020800       COMPUTE W52212-KVLEVART = W52212-KVLEVART * -1                     
020900       END-COMPUTE                                                        
021000       COMPUTE W52212-SUFKTBEL = W52212-SUFKTBEL * -1                     
021100       END-COMPUTE                                                        
021200     END-IF                                                               
021210     IF W52212-IDVAT = SPACE                                              
021220       MOVE 'QV999999999999' TO W52212-IDVAT                              
021230     END-IF                                                               
021300     .                                                                    
021400     EJECT                                                                
021500 Z-FINIT SECTION.                                                         
021600                                                                          
021700     CLOSE W52201                                                         
021800           W52212                                                         
021900     .                                                                    
022000     EJECT                                                                
022100*    --- READ AND WRITE SECTIONS                                          
022200 S01-READ-W52201 SECTION.                                                 
022300     SKIP2                                                                
022400     READ                                                                 
022500       W52201 INTO IN-AREA                                                
022600     AT END MOVE YES TO W52201-EOF-SW                                     
022700     END-READ                                                             
022800     .                                                                    
022900     EJECT                                                                
023000 S11-WRITE-W52212 SECTION.                                                
023100                                                                          
023200     WRITE W52212-POST FROM W52212-AREA                                   
023300     .                                                                    
023400     EJECT                                                                
