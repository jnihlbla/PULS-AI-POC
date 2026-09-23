000100 ID DIVISION.                                                             
000200     SKIP2                                                                
000300 PROGRAM-ID.     W0081200.                                                
000400 AUTHOR.         CARINA VIKTORSSON.                                       
000500 DATE-WRITTEN.   MAJ 1988.                                                
000600                                                                          
000700     REMARKS.                                                             
000800*                                                                         
000900*    FUNKTION.                                                            
001000*        RESTORDERSTYRNING 2                                              
001100*        VISA PRIORITETSRADER (MAX 25 STYCKEN)                            
001200*    INDATA.                                                              
001300*        TRANSAKTION: W0T812                                              
001400                                                                          
001500     SKIP3                                                                
001600 ENVIRONMENT DIVISION.                                                    
001700     SKIP3                                                                
001800 DATA DIVISION.                                                           
001900     EJECT                                                                
002000 WORKING-STORAGE SECTION.                                                 
002001                                                                          
002010*    -- CHECKED BY WY2000                                                 
002100 77  PROGRAM-NAMN                PIC X(08)   VALUE 'W0081200'.            
002200 77  JA                          PIC X       VALUE 'J'.                   
002300 77  NEJ                         PIC X       VALUE 'N'.                   
002400 77  SPRAK-IX                    PIC S9(9)   VALUE +0   COMP SYNC.        
002500 77  INDX                        PIC S9(9)   VALUE +0   COMP SYNC.        
002600 77  MAX-MOD-LAENGD              PIC S9(4)   VALUE +437 COMP SYNC.        
002700 01  VISA-SPEC-SW                PIC X.                                   
002800   88  VISA-JA                               VALUE 'J'.                   
002900   88  VISA-NEJ                              VALUE 'N'.                   
003000     SKIP3                                                                
003001 01  GENERELLA-SUBPROGRAM.                                                
003003     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
003004     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
003010     EJECT                                                                
003100 01  NYCKLAR-TILL-DLI.                                                    
003200   03  W-IDHTYP-4501-X.                                                   
003300       05  W-IDHTYP-4501         PIC X(4)    VALUE '4501'.                
003400       05  W-LOWVALUE-4501       PIC X(26)   VALUE LOW-VALUE.             
003500   03  W-WDGXKEY-X.                                                       
003600     05  W-KDRAPRIO              PIC S9(3)   VALUE ZERO  COMP-3.          
003700     05  W-LOWVALUE              PIC X(3)    VALUE LOW-VALUE.             
003800     EJECT                                                                
003900*    -COPY WWTEXT01                                                       
004100     EJECT                                                                
004200******************************************************************        
004300*                                                                         
004400*                AREOR FÖR MFS OCH SKÄRMHANTERING                         
004500*                                                                         
004600 01  FILLER                      PIC X(16)   VALUE 'MFS-WS'.              
004700     SKIP3                                                                
004800*    -COPY W0I81201                                                       
005000     EJECT                                                                
005100*01  -COPY WMSGAREA                                                       
005300     EJECT                                                                
005400*  03 W0O81201 -COPY W0O81201     -RED MSG-AREA                           
005600     EJECT                                                                
005700*01  -COPY WMFSAREA                                                       
005900     EJECT                                                                
006000******************************************************************        
006100*                                                                         
006200*        ARBETS-AREOR TILL IMS-SEKTIONERNA                                
006300*                                                                         
006400 01  IMS-WS.                                                              
006500   03  FILLER                    PIC X(16)   VALUE 'IMS-WS     '.         
006600     SKIP3                                                                
006700*                        **** STATUS-KOD FRÅN IMS                         
006800   03  STATUS-WS                 PIC XX.                                  
006900     88  SEGMENT-FINNS                       VALUE '  '.                  
007000     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
007100     SKIP3                                                                
007200   03  GODK-STATUSKODER.                                                  
007300     05  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
007400     SKIP3                                                                
007500 01    SSA1                      PIC X(64).                               
007600     EJECT                                                                
007700*                            IMS FUNKTIONSKODER                           
007800*01    -COPY W0003                                                        
008000     EJECT                                                                
008100 01  FILLER                PIC X(16) VALUE 'DLI-IO-AREA '.                
008200 01  DLI-IO-AREA.                                                         
008300   03  IO-AREA                   PIC X(100)  VALUE SPACE.                 
008400     SKIP3                                                                
008500*  03  WLXXJM01 -COPY WDGX01      -RED IO-AREA -PRE 4501-                 
008700     EJECT                                                                
008800*  03  WLXXJM11 -COPY WDGX4502    -RED IO-AREA                            
009000     EJECT                                                                
009100 LINKAGE SECTION.                                                         
009200*01  -COPY W0009     -PRE MSG-                                            
009400     EJECT                                                                
009500*01  -COPY W0008     -PRE XXJM-                                           
009700     05  FILLER                  PIC X.                                   
009800     EJECT                                                                
009900 PROCEDURE DIVISION USING MSG-PCB XXJM-PCB.                               
010000     ENTRY 'DLITCBL' USING MSG-PCB XXJM-PCB.                              
010100                                                                          
010200     PERFORM IMS-GET-MSG                                                  
010300     IF SEGMENT-FINNS                                                     
010400        PERFORM A-INIT                                                    
010500        PERFORM IMS-GU-WLXXJM01                                           
010600         IF MFS-IDPFK = '7'                                               
010700            MOVE ZERO TO W-KDRAPRIO                                       
010800            MOVE TEXT-0410(SPRAK-IX) TO MOD-TEMFSINF                      
010900            MOVE NEJ TO VISA-SPEC-SW                                      
011000         ELSE                                                             
011100            IF MFS-IDPFK = '8'                                            
011200               MOVE MID-KDRAPRIO-PF8  TO W-KDRAPRIO                       
011300               MOVE JA TO VISA-SPEC-SW                                    
011400            ELSE                                                          
011500               MOVE MID-KDRAPRIO-ENTER TO W-KDRAPRIO                      
011600               MOVE JA TO VISA-SPEC-SW                                    
011700            END-IF                                                        
011800            IF W-KDRAPRIO = ZERO                                          
011900               MOVE '7' TO MFS-IDPFK                                      
012000               MOVE TEXT-0410(SPRAK-IX) TO MOD-TEMFSINF                   
012100               MOVE NEJ TO VISA-SPEC-SW                                   
012200            END-IF                                                        
012300         END-IF                                                           
012400         PERFORM C-LAES-VISA-INFO                                         
012500       MOVE MAX-MOD-LAENGD TO MSG-KVLL                                    
012600       PERFORM IMS-INSERT-MSG                                             
012700     END-IF                                                               
012800                                                                          
012900     MOVE ZERO TO RETURN-CODE                                             
013000     GOBACK                                                               
013100     .                                                                    
013200     EJECT                                                                
013300 A-INIT SECTION.                                                          
013400                                                                          
013500     IF MSG-DUBBLA-TRANSKODER                                             
013600       MOVE MSG-INDATA-MINUS-2-TRANSKODER TO MID-W0I81201                 
013700       MOVE MSG-IDTRANS-2 TO MFS-IDTRANS                                  
013800       MOVE MSG-KDMFSFOR-2 TO MFS-KDMFSFOR                                
013900       MOVE MSG-KDTRTYP          TO MFS-KDTRTYP                           
014000       MOVE MSG-IDPFK TO MFS-IDPFK                                        
014100     ELSE                                                                 
014200       MOVE MSG-INDATA-MINUS-1-TRANSKOD  TO MID-W0I81201                  
014300       MOVE MSG-IDTRANS-1 TO MFS-IDTRANS                                  
014400       MOVE MSG-KDMFSFOR-1 TO MFS-KDMFSFOR                                
014500       MOVE SPACE TO MFS-KDTRTYP        MFS-IDPFK                         
014600     END-IF                                                               
014700                                                                          
014800     MOVE LOW-VALUE TO MSG-AREA                                           
014900     MOVE 'W0O81201' TO MFS-IDMOD                                         
015000     MOVE '0812' TO MOD-IDTRANS                                           
015100     MOVE MFS-RENSA-FAELT TO MOD-TEMFSFEL MOD-TEMFSINF                    
015200                                                                          
015300     IF MFS-IDTRANS NOT = '0812'                                          
015400       MOVE SPACE TO MFS-KDTRTYP                                          
015500       MOVE '7' TO MFS-IDPFK                                              
015600     END-IF                                                               
015700     IF MFS-KDMFSFOR = '2'                                                
015800       MOVE +2 TO SPRAK-IX                                                
015900     ELSE                                                                 
016000       MOVE +1 TO SPRAK-IX                                                
016100     END-IF                                                               
016200     .                                                                    
016300     EJECT                                                                
016400 C-LAES-VISA-INFO SECTION.                                                
016500                                                                          
016600     IF VISA-JA                                                           
016700        PERFORM IMS-GNP-WLXXJM11-KVAL                                     
016800     ELSE                                                                 
016900        PERFORM IMS-GNP-WLXXJM11                                          
017000     END-IF                                                               
017100     IF SEGMENT-FINNS                                                     
017200        MOVE 4502-KDRAPRIO        TO MOD-KDRAPRIO-ENTER                   
017300     ELSE                                                                 
017400        MOVE ZERO                 TO MOD-KDRAPRIO-ENTER                   
017500     END-IF                                                               
017600     MOVE +1 TO INDX                                                      
017700     PERFORM UNTIL INDX > +14                                             
017800        IF SEGMENT-FINNS                                                  
017900           MOVE 4502-KDRAPRIO        TO MOD-KDRAPRIO(INDX)                
018000           MOVE 4502-BERAPRIO        TO MOD-BERAPRIO(INDX)                
018100           MOVE 4502-FLPRIO          TO MOD-FLPRIO(INDX)                  
018200           MOVE 4502-REROFORD        TO MOD-REROFORD(INDX)                
018300           MOVE 4502-KVVECKOR-TECK   TO MOD-KVVECKOR-TECK(INDX)           
018400           MOVE 4502-RELEVFOR        TO MOD-RELEVFOR(INDX)                
018500           PERFORM IMS-GNP-WLXXJM11                                       
018600        ELSE                                                              
018700           MOVE MFS-RENSA-FAELT      TO MOD-KDRAPRIO(INDX)                
018800                                        MOD-BERAPRIO(INDX)                
018900                                        MOD-FLPRIO(INDX)                  
019000                                        MOD-REROFORD(INDX)                
019100                                        MOD-KVVECKOR-TECK(INDX)           
019200                                        MOD-RELEVFOR(INDX)                
019300        END-IF                                                            
019400        ADD +1 TO INDX                                                    
019500     END-PERFORM                                                          
019600     IF SEGMENT-FINNS                                                     
019700        MOVE 4502-KDRAPRIO        TO MOD-KDRAPRIO-PF8                     
019800        MOVE TEXT-0402(SPRAK-IX)  TO MOD-TEMFSINF                         
019900     ELSE                                                                 
020000        MOVE ZERO                 TO MOD-KDRAPRIO-PF8                     
020100     END-IF                                                               
020200     .                                                                    
020300     EJECT                                                                
020400* IMS SEKTIONER                                                           
020500     SKIP3                                                                
020600 IMS-GET-MSG SECTION.                                                     
020700                                                                          
020800     MOVE '  QC' TO GODK-STATUSKODER                                      
020900     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
021000     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
021100     PERFORM IMS-STATUSKONTROLL                                           
021200     SKIP3                                                                
021300     .                                                                    
021400 IMS-INSERT-MSG SECTION.                                                  
021500                                                                          
021600     IF ENGLISH-TEXT                                                      
021700       MOVE 'N' TO MFS-KDHUVOMR                                           
021800     END-IF                                                               
021900     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
022000     MOVE SPACE TO GODK-STATUSKODER                                       
022100     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
022200     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
022300     PERFORM IMS-STATUSKONTROLL                                           
022400     .                                                                    
022500     EJECT                                                                
022600 IMS-GU-WLXXJM01 SECTION.                                                 
022700                                                                          
022800     STRING 'WLXXJM01(WDGXKEY  =' W-IDHTYP-4501-X ')'                     
022900          DELIMITED BY SIZE INTO SSA1                                     
023000     MOVE '  ' TO GODK-STATUSKODER                                        
023100     CALL CBLTDLI USING GU XXJM-PCB DLI-IO-AREA SSA1                      
023200     MOVE XXJM-STATUS-CODE TO STATUS-WS                                   
023300     PERFORM IMS-STATUSKONTROLL                                           
023400     .                                                                    
023500     SKIP1                                                                
023600 IMS-GNP-WLXXJM11-KVAL SECTION.                                           
023700                                                                          
023800     STRING 'WLXXJM11(WDGXKEY  =' W-WDGXKEY-X ')'                         
023900          DELIMITED BY SIZE INTO SSA1                                     
024000     MOVE '  GE' TO GODK-STATUSKODER                                      
024100     CALL CBLTDLI USING GNP XXJM-PCB DLI-IO-AREA SSA1                     
024200     MOVE XXJM-STATUS-CODE TO STATUS-WS                                   
024300     PERFORM IMS-STATUSKONTROLL                                           
024400     .                                                                    
024500     SKIP1                                                                
024600 IMS-GNP-WLXXJM11 SECTION.                                                
024700                                                                          
024800     MOVE '  GE' TO GODK-STATUSKODER                                      
024900     CALL CBLTDLI USING GNP XXJM-PCB DLI-IO-AREA                          
025000     MOVE XXJM-STATUS-CODE TO STATUS-WS                                   
025100     PERFORM IMS-STATUSKONTROLL                                           
025200     .                                                                    
025300     SKIP1                                                                
025400 IMS-STATUSKONTROLL SECTION.                                              
025500                                                                          
025600     SET STATUS-IX TO 1                                                   
025700     SEARCH GODK-STATUS AT END CALL FELLOG                                
025800       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS CONTINUE                  
025900     END-SEARCH                                                           
026000     .                                                                    
