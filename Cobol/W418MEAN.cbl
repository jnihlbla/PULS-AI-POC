000100*COMPOPT STDSUB=YES                                                       
000200 ID DIVISION.                                                             
000300 PROGRAM-ID.     W418MEAN.                                                
000400 AUTHOR.         LARS THELL CAP PROGRAMATOR                               
000500 DATE-WRITTEN.   JULI 1995.                                               
000600 DATE-COMPILED.                                                           
000700                                                                          
000800*    FUNKTION:                                                            
000900*        PROGRAMMET SKAPAR MAIL TILL LEVERANSANMÄRKNINGSANSVARIG          
001000*        MED UNDERKÄNDA RADER                                             
001100*                                                                         
001110*    E'TRACKER 8687963  2010-10-29                                        
001120*                                                                         
001200                                                                          
001300 ENVIRONMENT DIVISION.                                                    
001400                                                                          
001500 DATA DIVISION.                                                           
001600                                                                          
001700     EJECT                                                                
001800 WORKING-STORAGE SECTION.                                                 
001900 77  IDPGM                   PIC X(8) VALUE 'W418MEAN'.                   
002000 77  JA                      PIC X       VALUE 'J'.                       
002100 77  NEJ                     PIC X       VALUE 'N'.                       
002200 77  INDX                    PIC S9(4)   VALUE ZERO COMP SYNC.            
002300 77  IX2                     PIC S9(4)   VALUE ZERO COMP SYNC.            
002400 77  MAX-IX2                 PIC S9(4)   VALUE +13  COMP SYNC.            
002500                                                                          
002600 01  FILLER.                                                              
002700   03  COUNTER               PIC 99  VALUE ZERO.                          
002800                                                                          
002900 01  DYNAMISKA-SUBPROGRAM.                                                
003000   03  CBLTDLI               PIC X(8)    VALUE 'CBLTDLI '.                
003100   03  FELLOG                PIC X(8)    VALUE 'FELLOG  '.                
003200                                                                          
003300     EJECT                                                                
003400*  MAILRADER                                                              
003500                                                                          
003600 01  LIST-HRAD1.                                                          
003700     03   FILLER                  PIC X(1)  VALUE SPACE.                  
003800     03   FILLER                  PIC X(12) VALUE                         
003900                                        'W418MEAN-001'.                   
004000     03   FILLER                  PIC X(4)  VALUE SPACE.                  
004100     03   FILLER                  PIC X(35)  VALUE                        
004200                        'REJECTED DISCREPANCY LINES         '.            
004300                                                                          
004400 01  LIST-HRAD2.                                                          
004500     03   FILLER                  PIC X(1)  VALUE SPACE.                  
004600     03   FILLER                  PIC X(9)  VALUE 'DISTRICT '.            
004700     03   FILLER                  PIC X(3)  VALUE SPACE.                  
004800     03   FILLER                  PIC X(5)  VALUE 'CUST '.                
004900     03   FILLER                  PIC X(3)  VALUE SPACE.                  
005000     03   FILLER                  PIC X(9)  VALUE 'REPORT.NO'.            
005100     03   FILLER                  PIC X(3)  VALUE SPACE.                  
005200     03   FILLER                  PIC X(10) VALUE 'ARTICLE.NO'.           
005300     03   FILLER                  PIC X(2)  VALUE SPACE.                  
005400     03   FILLER                  PIC X(5)  VALUE 'LINE '.                
005500     03   FILLER                  PIC X(3)  VALUE SPACE.                  
005600     03   FILLER                  PIC X(10) VALUE 'TREAT CODE'.           
005700                                                                          
005800 01  LIST-LRAD.                                                           
005900     03   FILLER                  PIC X(5)  VALUE SPACE.                  
006000     03   LRAD-IDDISTR            PIC Z(4)  VALUE ZERO.                   
006100     03   FILLER                  PIC X(2)  VALUE SPACE.                  
006200     03   LRAD-IDKUNDNR           PIC Z(6)  VALUE ZERO.                   
006300     03   FILLER                  PIC X(6)  VALUE SPACE.                  
006400     03   LRAD-IDRAPPNR           PIC Z(7)  VALUE ZERO.                   
006500     03   FILLER                  PIC X(3)  VALUE SPACE.                  
006600     03   LRAD-IDARTNR            PIC Z(8)9.                              
006700     03   FILLER                  PIC X(3)  VALUE SPACE.                  
006800     03   LRAD-IDRADNR            PIC Z(5)  VALUE ZERO.                   
006900     03   FILLER                  PIC X(3)  VALUE SPACE.                  
007000     03   LRAD-KDKREBEH           PIC X(3)  VALUE SPACE.                  
007100                                                                          
007200 01  LIST-LRAD-1.                                                         
007300     03   FILLER                  PIC X(5)  VALUE SPACE.                  
007400     03   TXT-TEANMNOT-ADM-1      PIC X(70) VALUE SPACE.                  
007500                                                                          
007600 01  LIST-LRAD-2.                                                         
007700     03   FILLER                  PIC X(5)  VALUE SPACE.                  
007800     03   TXT-TEANMNOT-ADM-2      PIC X(70) VALUE SPACE.                  
007900                                                                          
008000 01  LIST-LRAD-3.                                                         
008100     03   FILLER                  PIC X(5)  VALUE SPACE.                  
008200     03   TXT-TEANMNOT-ADM-3      PIC X(70) VALUE SPACE.                  
008300                                                                          
008400 01  LIST-LRAD-4.                                                         
008500     03   FILLER                  PIC X(5)  VALUE SPACE.                  
008600     03   TXT-TEANMNOT-REM-1      PIC X(70) VALUE SPACE.                  
008700                                                                          
008800 01  LIST-LRAD-5.                                                         
008900     03   FILLER                  PIC X(5)  VALUE SPACE.                  
009000     03   TXT-TEANMNOT-REM-2      PIC X(70) VALUE SPACE.                  
009100                                                                          
009200 01  LIST-LRAD-6.                                                         
009300     03   FILLER                  PIC X(5)  VALUE SPACE.                  
009400     03   TXT-TEANMNOT-REM-3      PIC X(70) VALUE SPACE.                  
009500                                                                          
009600 01  LIST-BLANKRAD.                                                       
009700     03   FILLER                  PIC X(80) VALUE SPACE.                  
009800                                                                          
009900     EJECT                                                                
010000*    --- STATUS-KOD FRÅN IMS                                              
010100 01  STATUS-WS                   PIC XX.                                  
010200     88  STATUS-OK                           VALUE '  '.                  
010300                                                                          
010400*    --- IMS FUNKTIONSKODER                                               
010500*01  -COPY W0003                                                          
010600                                                                          
010700     EJECT                                                                
010800*01  -COPY WMSGAREA                                                       
010900                                                                          
011000     EJECT                                                                
011100*   --- PARAMETRAR TILL PROGRAM W0541X                                    
011200                                                                          
011300 01  FILLER                     PIC X(16)   VALUE 'WMSGMAIL-AREA'.        
011400*01  -COPY WMSGMAIL                                                       
011500                                                                          
011600     EJECT                                                                
011700*    ---- ARBETS-AREOR FÖR IMS-SEKTIONERNA                                
011800                                                                          
011900 01  GODK-STATUSKODER.                                                    
012000   03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                  
012100                                                                          
012200 01  SSA1                    PIC X(32).                                   
012300                                                                          
012400     EJECT                                                                
012500 LINKAGE SECTION.                                                         
012600                                                                          
012700*01  -COPY W418MEAN                                                       
012800     EJECT                                                                
012900*01  -COPY W0009      -PRE  MAIL-                                         
013000                                                                          
013100     EJECT                                                                
013200 PROCEDURE DIVISION  USING  MEAN-W418MEAN MAIL-PCB.                       
013300 STYR SECTION.                                                            
013400                                                                          
013500     PERFORM A-INIT                                                       
013600                                                                          
013700     PERFORM B-SKAPA-MAIL                                                 
013800                                                                          
013900     GOBACK                                                               
014000     .                                                                    
014100                                                                          
014200     EJECT                                                                
014300 A-INIT        SECTION.                                                   
014400                                                                          
014500     MOVE '4722'                TO MAIL-IDTRANS                           
014600     MOVE '1'                   TO MAIL-KDMFSFOR                          
014700     MOVE 'UNDERKÄNDA RADER'    TO MAIL-IDMAILTTL                         
014800     MOVE +1                    TO INDX                                   
014900     MOVE LIST-HRAD1            TO MAIL-TEMAIL (INDX)                     
015000     ADD +1                     TO INDX                                   
015100                                                                          
015200     MOVE ZERO TO COUNTER                                                 
015300     INSPECT MEAN-IDMAIL TALLYING COUNTER FOR ALL '@'                     
015400     IF COUNTER = 0                                                       
015500       MOVE 'SSAMUEL2@VOLVOCARS.COM'                                      
015600                                TO MAIL-IDMAIL                            
015700     ELSE                                                                 
015800       MOVE MEAN-IDMAIL         TO MAIL-IDMAIL                            
015900     END-IF                                                               
016000                                                                          
016100     .                                                                    
016200     EJECT                                                                
016300 B-SKAPA-MAIL  SECTION.                                                   
016400                                                                          
016500     PERFORM BA-REDIGERA-HUVUD                                            
016600     PERFORM BB-REDIGERA-RADER                                            
016700                                                                          
016800     MOVE INDX                TO MAIL-KVMAILLN                            
680100                                                                          
710099     PERFORM IMS-PURGE-TRANS0541X-MID                                     
720033     .                                                                    
730099                                                                          
740033     EJECT                                                                
750055 BA-REDIGERA-HUVUD  SECTION.                                              
760055                                                                          
770066     ADD +1                     TO INDX                                   
780099     MOVE LIST-HRAD2            TO MAIL-TEMAIL (INDX)                     
790055                                                                          
800068     ADD +1                     TO INDX                                   
810099     MOVE LIST-BLANKRAD         TO MAIL-TEMAIL (INDX)                     
820055     .                                                                    
830099                                                                          
840055     EJECT                                                                
850055 BB-REDIGERA-RADER  SECTION.                                              
860055                                                                          
870085     MOVE +1                    TO IX2                                    
880084     PERFORM UNTIL IX2          >  MAX-IX2 OR                             
890084             MEAN-IDDISTR(IX2)  =  ZERO                                   
900088        ADD +1                  TO INDX                                   
910084        MOVE MEAN-IDDISTR (IX2) TO LRAD-IDDISTR                           
920084        MOVE MEAN-IDKUNDNR(IX2) TO LRAD-IDKUNDNR                          
930084        MOVE MEAN-IDRAPPNR(IX2) TO LRAD-IDRAPPNR                          
940084        MOVE MEAN-IDARTNR (IX2) TO LRAD-IDARTNR                           
950084        MOVE MEAN-IDRADNR (IX2) TO LRAD-IDRADNR                           
960097        MOVE MEAN-KDKREBEH(IX2) TO LRAD-KDKREBEH                          
970099        MOVE LIST-LRAD          TO MAIL-TEMAIL (INDX)                     
980099                                                                          
990099        MOVE MEAN-TEANMNOT-ADM (IX2, 1) TO TXT-TEANMNOT-ADM-1             
000099        MOVE MEAN-TEANMNOT-ADM (IX2, 2) TO TXT-TEANMNOT-ADM-2             
010099        MOVE MEAN-TEANMNOT-ADM (IX2, 3) TO TXT-TEANMNOT-ADM-3             
020099                                                                          
030099        ADD +1                  TO INDX                                   
040099        MOVE LIST-LRAD-1        TO MAIL-TEMAIL (INDX)                     
050099                                                                          
060099        ADD +1                  TO INDX                                   
070099        MOVE LIST-LRAD-2        TO MAIL-TEMAIL (INDX)                     
080099                                                                          
090099        ADD +1                  TO INDX                                   
100099        MOVE LIST-LRAD-3        TO MAIL-TEMAIL (INDX)                     
110099                                                                          
120099        MOVE MEAN-TEANMNOT-REM (IX2, 1) TO TXT-TEANMNOT-REM-1             
130099        MOVE MEAN-TEANMNOT-REM (IX2, 2) TO TXT-TEANMNOT-REM-2             
140099        MOVE MEAN-TEANMNOT-REM (IX2, 3) TO TXT-TEANMNOT-REM-3             
150099                                                                          
160099        ADD +1                  TO INDX                                   
170099        MOVE LIST-LRAD-4        TO MAIL-TEMAIL (INDX)                     
180099                                   MAIL-TEMAIL (INDX)                     
190099                                                                          
200099        ADD +1                  TO INDX                                   
210099        MOVE LIST-LRAD-5        TO MAIL-TEMAIL (INDX)                     
220099                                   MAIL-TEMAIL (INDX)                     
230099                                                                          
240099        ADD +1                  TO INDX                                   
250099        MOVE LIST-LRAD-6        TO MAIL-TEMAIL (INDX)                     
260099                                   MAIL-TEMAIL (INDX)                     
270099                                                                          
280099        ADD +1                  TO IX2                                    
290084     END-PERFORM                                                          
300055     .                                                                    
310099                                                                          
320055     EJECT                                                                
330099 IMS-PURGE-TRANS0541X-MID SECTION.                                        
340092                                                                          
350092     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
360092     MOVE '  '                  TO GODK-STATUSKODER                       
370099     CALL CBLTDLI USING PURG MAIL-PCB MAIL-WMSGMAIL                       
380099     MOVE MAIL-STATUS-CODE       TO STATUS-WS                             
390092     PERFORM IMS-STATUSKONTROLL                                           
400092     .                                                                    
410099                                                                          
420099                                                                          
430000 IMS-STATUSKONTROLL SECTION.                                              
440000                                                                          
450000     SET STATUS-IX TO 1                                                   
460000     SEARCH GODK-STATUS                                                   
470000       AT END CALL FELLOG                                                 
480000       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS CONTINUE                  
490000     END-SEARCH                                                           
500000     .                                                                    
