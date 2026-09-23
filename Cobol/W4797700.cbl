000100 ID DIVISION.                                                             
000200 PROGRAM-ID.      W4797700.                                               
000300 AUTHOR.          GÖRAN KJELLSON  GUIDE DATAKONSULT  AB                   
000400 DATE-WRITTEN.    DEC  1990.                                              
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*    FUNKTION.                                                            
000800*                                                                         
000900*        RENSNING AV WLORDM (WDL1)                                        
001000*       11-SEGMENTEN TAS BORT OM                                          
001100*          SVENSKT DISTRIKT OCH ÄLDRE ÄN TVÅ ÅR                           
001200*          ELLER                                                          
001300*          UTLÄNDSKT DISTRIKT OCH ÄLDRE ÄN ETT ÅR                         
001400*                                                                         
001500*       12-SEGMENTEN TAS BORT OM                                          
001600*          SVENSKT DISTRIKT OCH ÄLDRE ÄN ETT ÅR                           
001700*          ELLER                                                          
001800*          UTLÄNDSKT DISTRIKT OCH ÄLDRE ÄN TVÅ ÅR                         
001900*                                                                         
002000*       01-SEGMENT TAS BORT OM                                            
002100*          VARKEN 11- ELLER 12-SEGMENT FINNS KVAR                         
002200*                                                                         
002300                                                                          
002400 ENVIRONMENT DIVISION.                                                    
002500     SKIP2                                                                
002600 INPUT-OUTPUT SECTION.                                                    
002700                                                                          
002800 FILE-CONTROL.                                                            
002900     SKIP2                                                                
003000                                                                          
003100     SELECT W47977                     ASSIGN TO W47977D1.                
003200     EJECT                                                                
003300 DATA DIVISION.                                                           
003400                                                                          
003500 FILE SECTION.                                                            
003600     SKIP2                                                                
003700 FD  W47977                                                               
003800     RECORDING       F                                                    
003900     BLOCK CONTAINS  0.                                                   
004000                                                                          
004100 01  W47977-POST                PIC X(45).                                
004200     EJECT                                                                
004300 WORKING-STORAGE SECTION.                                                 
004400*  CHECKED BY WY2000                                                      
004500                                                                          
004600 77  IDPGM                     PIC X(08)   VALUE 'W4797700'.              
004700 77  JA                        PIC X(1)    VALUE 'J'.                     
004800 77  NEJ                       PIC X(1)    VALUE 'N'.                     
005000 77  W-ANT-11                  PIC S9(7)   VALUE ZERO  COMP-3.            
005100 77  W-ANT-12                  PIC S9(7)   VALUE ZERO  COMP-3.            
005200 77  W-DEL-01                  PIC S9(7)   VALUE ZERO  COMP-3.            
005300 77  W-DEL-11                  PIC S9(7)   VALUE ZERO  COMP-3.            
005400 77  W-DEL-12                  PIC S9(7)   VALUE ZERO  COMP-3.            
005500                                                                          
005600 77  SW-DEL-ROT                PIC X(1)    VALUE 'N'.                     
005700                                                                          
005800 01  W-DATE                    PIC 9(08).                                 
005900                                                                          
006000 01  W-DAT-1                   PIC 9(8)    VALUE ZERO.                    
006100                                                                          
006200 01  W-DAT-2                   PIC 9(8)    VALUE ZERO.                    
006300                                                                          
006400 01  DYNAMISKA-SUBPROGRAM.                                                
006500   03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.              
006600   03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.              
006610   03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.               
006700                                                                          
006701 01 UT-AREA.                                                              
006720   03 UT-SEGM              PIC X(8)    VALUE SPACES.                      
006721   03 UT-ORD-IDDISTR       PIC S9(5)   VALUE ZERO COMP-3.                 
006722   03 UT-ORD-IDKUNDNR      PIC S9(7)   VALUE ZERO COMP-3.                 
006723   03 UT-ORD-IDKUNDRF      PIC X(10)   VALUE SPACES.                      
006724   03 UT-ORD-IDDC          PIC X(2)    VALUE SPACES.                      
006725   03 UT-DAT-DAFAKT        PIC 9(8)    VALUE ZERO.                        
006726   03 UT-DAT-DAHISTOB      PIC 9(8)    VALUE ZERO.                        
006800     EJECT                                                                
006810*    --- PARAMETRAR TILL POSTSUM                                          
006820*                                                                         
006830*01  -COPY W0005   -PRE  POSTSUM-                                         
006840     EJECT                                                                
006900*    ---- ARBETS-AREOR FÖR IMS-SEKTIONERNA.                               
007000                                                                          
007100 01  FILLER                    PIC X(8)   VALUE 'IMS-WS  '.               
007200                                                                          
007300 01  NYCKLAR-TILL-DLI.                                                    
007400   03  W-WDL101KY-X.                                                      
007500     05  W-IDDISTR             PIC S9(5)  VALUE ZERO COMP-3.              
007600     05  W-IDKUNDNR            PIC S9(7)  VALUE ZERO COMP-3.              
007700     05  W-IDKUNDRF            PIC X(10)  VALUE SPACE.                    
007800     05  W-IDDC                PIC X(2)   VALUE '00'.                     
007900*    ---- STATUSKOD FRÅN IMS                                              
008000                                                                          
008100 01  STATUS-WS                 PIC XX.                                    
008200     88  SEGMENT-FINNS                    VALUE '  '.                     
008300     88  SEGMENT-SAKNAS                   VALUE 'GE'.                     
008400     88  BASEN-SLUT                       VALUE 'GB'.                     
008500                                                                          
008600 01  GODK-STATUSKODER.                                                    
008700   03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                  
008800                                                                          
008900 01  SSA1                      PIC X(64).                                 
009000 01  SSA2                      PIC X(64).                                 
009100     EJECT                                                                
009200*01      -COPY W0003.                                                     
009300     EJECT                                                                
009400 01  FILLER                    PIC X(16)  VALUE 'DLI-IO-AREA'.            
009500                                                                          
009600 01  DLI-IO-AREA-1.                                                       
009700   03  IO-AREA                 PIC X(50).                                 
009800                                                                          
009900   03  WDL101 REDEFINES IO-AREA.                                          
010000*    05  -COPY WDL101                                                     
010100     EJECT                                                                
010200   03  WDL111 REDEFINES IO-AREA.                                          
010300*    05  -COPY WDL111                                                     
010400     SKIP3                                                                
010500   03  WDL112 REDEFINES IO-AREA.                                          
010600*    05  -COPY WDL112                                                     
010700     EJECT                                                                
011100 LINKAGE SECTION.                                                         
011500*    -COPY W0008 -PRE WDL1-.                                              
011600    05  FILLER                 PIC X(22).                                 
011700     EJECT                                                                
011800 PROCEDURE DIVISION  USING WDL1-PCB.                                      
011900 MAIN SECTION.                                                            
012000     ENTRY 'DLITCBL' USING WDL1-PCB.                                      
012100                                                                          
012200     PERFORM A-INIT                                                       
012300                                                                          
012400     PERFORM IMS-GN-WDL1                                                  
012500     PERFORM UNTIL BASEN-SLUT                                             
012600       IF WDL1-SEG-NAME-FB = 'WDL101  '                                   
012800         IF SW-DEL-ROT = JA                                               
012810           MOVE 'WDL101  '        TO UT-SEGM                              
012900           PERFORM S01-WRITE-W47977                                       
013200           ADD +1 TO W-DEL-01                                             
013300         END-IF                                                           
013710         MOVE ORD-IDDISTR       TO UT-ORD-IDDISTR                         
013720         MOVE ORD-IDKUNDNR      TO UT-ORD-IDKUNDNR                        
013730         MOVE ORD-IDKUNDRF      TO UT-ORD-IDKUNDRF                        
013740         MOVE ORD-IDDC          TO UT-ORD-IDDC                            
013800         MOVE JA TO SW-DEL-ROT                                            
013900       ELSE                                                               
014000         IF WDL1-SEG-NAME-FB = 'WDL111  '                                 
014100           ADD +1 TO W-ANT-11                                             
014200           PERFORM B-KOLLA-OCH-RENSA-WDL111-SEGM                          
014300         ELSE                                                             
014400           IF WDL1-SEG-NAME-FB = 'WDL112  '                               
014500             ADD +1 TO W-ANT-12                                           
014600             PERFORM C-KOLLA-OCH-RENSA-WDL112-SEGM                        
014700           END-IF                                                         
014800         END-IF                                                           
014900       END-IF                                                             
015000       PERFORM IMS-GN-WDL1                                                
015100     END-PERFORM                                                          
015200                                                                          
015300     PERFORM Z-FINIT                                                      
015400                                                                          
015500     MOVE ZERO TO RETURN-CODE                                             
015600     GOBACK.                                                              
015700                                                                          
015800     EJECT                                                                
015900 A-INIT SECTION.                                                          
016010     OPEN OUTPUT W47977                                                   
016020                                                                          
016100     MOVE FUNCTION CURRENT-DATE (1:8) TO W-DATE                           
016200     MOVE W-DATE TO W-DAT-1 W-DAT-2                                       
016300     SUBTRACT 10000 FROM W-DAT-1                                          
016400     SUBTRACT 20000 FROM W-DAT-2                                          
016500     DISPLAY 'ETT ÅR  '  W-DAT-1                                          
016600     DISPLAY 'TVÅ ÅR  '  W-DAT-2                                          
016700     .                                                                    
016800     EJECT                                                                
016900 B-KOLLA-OCH-RENSA-WDL111-SEGM SECTION.                                   
017100     IF W-IDDISTR < 800                                                   
017200       IF W-DAT-2 > DAT-DAFAKT                                            
017220         MOVE 'WDL111  '        TO UT-SEGM                                
017221         MOVE DAT-DAFAKT        TO UT-DAT-DAFAKT                          
017230         PERFORM S01-WRITE-W47977                                         
017400         ADD +1 TO W-DEL-11                                               
017500       ELSE                                                               
017600         MOVE NEJ TO SW-DEL-ROT                                           
017700       END-IF                                                             
017800     ELSE                                                                 
017900       IF W-DAT-1 > DAT-DAFAKT                                            
017902         MOVE 'WDL111  '        TO UT-SEGM                                
017903         MOVE DAT-DAFAKT        TO UT-DAT-DAFAKT                          
017904         PERFORM S01-WRITE-W47977                                         
018100         ADD +1 TO W-DEL-11                                               
018200       ELSE                                                               
018300         MOVE NEJ TO SW-DEL-ROT                                           
018400       END-IF                                                             
018500     END-IF                                                               
018600     .                                                                    
018700     SKIP3                                                                
018800 C-KOLLA-OCH-RENSA-WDL112-SEGM SECTION.                                   
018900                                                                          
019000     IF W-IDDISTR < 800                                                   
019100       IF W-DAT-1 > DAT-DAHISTOB                                          
019102         MOVE 'WDL112  '        TO UT-SEGM                                
019103         MOVE DAT-DAHISTOB      TO UT-DAT-DAHISTOB                        
019104         PERFORM S01-WRITE-W47977                                         
019300         ADD +1 TO W-DEL-12                                               
019400       ELSE                                                               
019500         MOVE NEJ TO SW-DEL-ROT                                           
019600       END-IF                                                             
019700     ELSE                                                                 
019800       IF W-DAT-2 > DAT-DAHISTOB                                          
019802         MOVE 'WDL112  '        TO UT-SEGM                                
019803         MOVE DAT-DAHISTOB      TO UT-DAT-DAHISTOB                        
019804         PERFORM S01-WRITE-W47977                                         
020000         ADD +1 TO W-DEL-12                                               
020100       ELSE                                                               
020200         MOVE NEJ TO SW-DEL-ROT                                           
020300       END-IF                                                             
020400     END-IF                                                               
020500     .                                                                    
020600                                                                          
020700     EJECT                                                                
020800 Z-FINIT SECTION.                                                         
020900                                                                          
020910     CLOSE W47977                                                         
020920                                                                          
021100     DISPLAY 'W-ANT-11 '  W-ANT-11                                        
021200     DISPLAY 'W-ANT-12 '  W-ANT-12                                        
021300     DISPLAY 'W-DEL-01 '  W-DEL-01                                        
021400     DISPLAY 'W-DEL-11 '  W-DEL-11                                        
021500     DISPLAY 'W-DEL-12 '  W-DEL-12                                        
021600     .                                                                    
021700     EJECT                                                                
021710 S01-WRITE-W47977 SECTION.                                                
021720     WRITE W47977-POST FROM UT-AREA                                       
021730                                                                          
021740     MOVE ' UT '       TO POSTSUM-TRANSTYP                                
021750     MOVE 'W47977'     TO POSTSUM-FDNAMN                                  
021760     MOVE 'W47977D1'   TO POSTSUM-DDNAMN2                                 
021770     CALL POSTSUM   USING POSTSUM-PARM                                    
021780     .                                                                    
021790     EJECT                                                                
023700 IMS-GN-WDL1 SECTION.                                                     
023900     MOVE '  GAGKGB'             TO GODK-STATUSKODER                      
024000     CALL CBLTDLI USING GN WDL1-PCB DLI-IO-AREA-1                         
024100     MOVE WDL1-STATUS-CODE      TO STATUS-WS                              
024200     PERFORM IMS-STATUSKONTROLL                                           
024300     .                                                                    
024400                                                                          
025300 IMS-STATUSKONTROLL SECTION.                                              
025400                                                                          
025500     SET STATUS-IX TO 1                                                   
025600     SEARCH GODK-STATUS                                                   
025700       AT END                                                             
025800         DISPLAY 'FEL STATUSKOD FRÅN IMS ' STATUS-WS                      
025900         CALL FELLOG                                                      
026000       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
026100         CONTINUE                                                         
026200      END-SEARCH                                                          
026300      .                                                                   
