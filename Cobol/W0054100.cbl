000100 ID DIVISION.                                                             
000200     SKIP2                                                                
000300 PROGRAM-ID.     W0054100.                                                
000400 AUTHOR.         LASSI.                                                   
000500 DATE-WRITTEN.   JULI 2007.                                               
000600 DATE-COMPILED.                                                           
000700                                                                          
000800*    FUNKTION.                                                            
000900*     PROGRAMMET SÄNDER MAIL TILL VALFRITT MAIL-ID VIA W411OUTM.          
001000*     ANROP KAN SKE FRÅN - MPP:ER MED KLASSISK P-TO-P                     
001100*                        - WZ01SEND (FRÅN NPP:ER)                         
001200*     ANTAL RADER SOM KAN SÄNDAS TILL MAIL ÄR MAX 99                      
001300*                                                                         
001400*    INDATA.                                                              
001500*        TRANSAKTION: W0T541X                                             
001600*        MID: WMSGMAIL (DET FINNS INGEN 0541-MID)                         
001700*             WZ01MAIL-VID ANROP FRÅN WZ01SEND (EJ MED I WS HÄR)          
001800*                                                                         
001900     SKIP2                                                                
002000 ENVIRONMENT DIVISION.                                                    
002100     SKIP2                                                                
002200 DATA DIVISION.                                                           
002300     EJECT                                                                
002400 WORKING-STORAGE SECTION.                                                 
002500*    -- CHECKED BY WY2000                                                 
002600 77  IDPGM                     PIC X(8)    VALUE 'W0054100'.              
002700 01  ERRTEXT.                                                             
002800     03  FILLER                PIC X(8)    VALUE 'ERRTEXT'.               
002900     03  ERRTEXT-STR           PIC X(72)   VALUE SPACE.                   
003000 77  KDRC-DISPLAY              PIC Z(3)9.                                 
003100                                                                          
003200 77  TEMP-MIDAREA              PIC X(8813) VALUE SPACE.                   
003300 77  IX                        PIC S9(9)   VALUE +0    COMP SYNC.         
003400 77  RKOD-ABEND-WITH-DUMP      PIC S9(4)   COMP VALUE +1000.              
003500 01  WDLEN-X.                                                             
003600   03  WDLEN                   PIC S9(4)   BINARY.                        
003700                                                                          
003800     EJECT                                                                
003900 01  GENERELLA-SUBPROGRAM.                                                
004000   03  WZ11OUTM                PIC X(8)    VALUE 'WZ11OUTM'.              
004100   03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.              
004200   03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.              
004300   03  ABEND                   PIC X(8)    VALUE 'ABEND   '.              
004400     EJECT                                                                
004500*                                                                         
004600 01  FILLER                      PIC X(16)   VALUE 'OUTM-AREA'.           
004700*                                                                         
004800 01  OUTM-AREA.                                                           
004900     03 -COPY WZ11OUTM                                                    
005000                                                                          
005100******************************************************************        
005200*                                                                         
005300*        ARBETS-AREOR TILL IMS-SEKTIONERNA                                
005400*                                                                         
005500 01  IMS-WS.                                                              
005600   03  FILLER                    PIC X(16)   VALUE 'IMS-WS     '.         
005700     SKIP3                                                                
005800*                        **** STATUS-KOD FRÅN IMS                         
005900   03  STATUS-WS                 PIC XX.                                  
006000     88  SEGMENT-FINNS                       VALUE '  '.                  
006100     SKIP3                                                                
006200   03  GODK-STATUSKODER.                                                  
006300     05  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
006400     SKIP3                                                                
006500*                            IMS FUNKTIONSKODER                           
006600*01    -COPY W0003                                                        
006700     EJECT                                                                
006800*                                                                         
006900 01  FILLER                    PIC X(16)   VALUE 'WMSGMAIL'.              
007000 01  MSG-IO-MAIL-AREA.                                                    
007100*  03  -COPY WMSGMAIL  -PRE MID-.                                         
007200                                                                          
007300     EJECT                                                                
007400 01  FILLER                    PIC X(16)   VALUE 'MOD-WS     '.           
007500*                                                                         
007600 01  MSG-IO-MOD-AREA.                                                     
007700   03  MSG-KVLL                PIC S9(4)   BINARY.                        
007800   03  MSG-KDZZ                PIC XX.                                    
007900   03  AREA -COPY WZ01RESP -PRE MOD-                                      
008000                                                                          
008100     EJECT                                                                
008200 LINKAGE SECTION.                                                         
008300*01  -COPY W0009     -PRE MSG-                                            
008400     EJECT                                                                
008500 PROCEDURE DIVISION USING MSG-PCB.                                        
008600 STYR SECTION.                                                            
008700     ENTRY 'DLITCBL' USING MSG-PCB.                                       
008800                                                                          
008900     PERFORM IMS-GET-MSG                                                  
009000     IF SEGMENT-FINNS                                                     
009100       PERFORM A-INIT                                                     
009200       PERFORM S01-SEND-OPEN                                              
009300                                                                          
009400       MOVE +1 TO IX                                                      
009500       PERFORM UNTIL IX > MID-MAIL-KVMAILLN                               
009600         PERFORM S02-SEND-MAIL-LINE                                       
009700         ADD +1 TO IX                                                     
009800       END-PERFORM                                                        
009900*      CALL FELLOG                                                        
010000       PERFORM S03-SEND-CLOSE                                             
010100     END-IF                                                               
010200                                                                          
010300     IF MID-MAIL-KDTRANS = 'W0T541U '                                     
010400*      -- ANROP FRÅN WEBBEN - RETURNERA ETT SVAR                          
010500       PERFORM S04-RETURN-MSG                                             
010600     END-IF                                                               
010700                                                                          
010800     MOVE ZERO TO RETURN-CODE                                             
010900     GOBACK                                                               
011000     .                                                                    
011100     EJECT                                                                
011200                                                                          
011300 A-INIT SECTION.                                                          
011400                                                                          
011500*    -- KOLLA OM DET ÄR SÄNT VIA WZ01SEND.                                
011600*    -- DÅ FINNS DET ETT BINÄRT LÄNGDFÄLT EFTER TRANSKODEN                
011700                                                                          
011800     MOVE MID-MAIL-IDTRANS(1:2) TO WDLEN-X                                
011900     IF WDLEN > 0 AND < 10000                                             
012000*      -- JA, TA BORT LÄNGDFÄLTET                                         
012100       MOVE SPACE TO TEMP-MIDAREA                                         
012200       STRING MSG-IO-MAIL-AREA(1:12)                                      
012300              MSG-IO-MAIL-AREA(15:)                                       
012400         DELIMITED BY SIZE INTO TEMP-MIDAREA                              
012500       MOVE TEMP-MIDAREA     TO MSG-IO-MAIL-AREA                          
012600     END-IF                                                               
012700                                                                          
012800*    --SKRIV VARNING PÅ FÖRSTA TEXTRADEN OM DET SAKNAS TEXT               
012900     IF MID-MAIL-KVMAILLN < +1                                            
013000       MOVE +1 TO MID-MAIL-KVMAILLN                                       
013100       MOVE 'TEXT MISSING IN TEXT-FIELDS' TO MID-MAIL-TEMAIL (1)          
013200     END-IF                                                               
013300     .                                                                    
013400     EJECT                                                                
013500 S01-SEND-OPEN SECTION.                                                   
013600                                                                          
013700     MOVE SPACE              TO OUTM-WZ11OUT                              
013800                                                                          
013900     MOVE 1                      TO OUTM-IDCALL                           
014000     MOVE 'OPEN'                 TO OUTM-KDFUNC                           
014100     MOVE ZERO                   TO OUTM-KDRC                             
014200     MOVE MID-MAIL-IDMAIL        TO OUTM-IDOUTDEST                        
014300     MOVE MID-MAIL-IDPFDEF       TO OUTM-IDPFDEF                          
014400     MOVE MID-MAIL-FLCARRCNTL    TO OUTM-FLCARRCNTL                       
014500     MOVE MID-MAIL-IDMAIL-SENDER TO OUTM-IDMAIL-SENDER                    
014600     MOVE MID-MAIL-IDMAILTTL     TO OUTM-IDMAILTTL                        
014700                                                                          
014800*    -- INITIALIZE ADDL INFO FIELDS. THESE ARE NOT USED                   
014900*    -- AS RESTART IS NOT AVAILABLE HERE.                                 
015000     MOVE SPACES                 TO OUTM-IDOUTTYPE                        
015100     MOVE SPACES                 TO OUTM-IDOUTREC                         
015200     MOVE SPACES                 TO OUTM-IDLIST                           
015300     MOVE ZEROES                 TO OUTM-TIREGDAT                         
015400     MOVE ZEROES                 TO OUTM-TIKLOCK                          
015500                                                                          
015600     CALL WZ11OUTM USING OUTM-WZ11OUT                                     
015700                                                                          
015800     IF OUTM-KDRC > 0                                                     
015900       MOVE OUTM-KDRC TO KDRC-DISPLAY                                     
016000       STRING 'WZ11OUTM OPEN ERROR RC=' KDRC-DISPLAY                      
016100       DELIMITED BY SIZE INTO ERRTEXT                                     
016200       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
016300     END-IF                                                               
016400     .                                                                    
016500     SKIP3                                                                
016600 S02-SEND-MAIL-LINE SECTION.                                              
016700                                                                          
016800     MOVE 'PUT'               TO OUTM-KDFUNC                              
016900     MOVE 85                  TO OUTM-TEOUTDATA-L                         
017000     MOVE MID-MAIL-TEMAIL(IX) TO OUTM-TEOUTDATA                           
017100                                                                          
017200     CALL WZ11OUTM USING OUTM-WZ11OUT                                     
017300                                                                          
017400     IF OUTM-KDRC > 0                                                     
017500       MOVE OUTM-KDRC TO KDRC-DISPLAY                                     
017600       STRING 'WZ11OUTM PUT ERROR RC=' KDRC-DISPLAY                       
017700       DELIMITED BY SIZE INTO ERRTEXT                                     
017800       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
017900     END-IF                                                               
018000     .                                                                    
018100     SKIP3                                                                
018200 S03-SEND-CLOSE SECTION.                                                  
018300                                                                          
018400     MOVE 'CLOSE'          TO OUTM-KDFUNC                                 
018500     CALL WZ11OUTM USING OUTM-WZ11OUT                                     
018600                                                                          
018700     IF OUTM-KDRC > 0                                                     
018800       MOVE OUTM-KDRC TO KDRC-DISPLAY                                     
018900       STRING 'WZ11OUTM CLOSE ERROR RC=' KDRC-DISPLAY                     
019000       DELIMITED BY SIZE INTO ERRTEXT                                     
019100       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
019200     END-IF                                                               
019300     .                                                                    
019400     SKIP3                                                                
019500 S04-RETURN-MSG SECTION.                                                  
019600                                                                          
019700     MOVE ALL '+'  TO MOD-AREA                                            
019800     MOVE 001 TO MOD-RESP-IDMSGVER                                        
019900                                                                          
020000     MOVE LENGTH OF MSG-IO-MOD-AREA TO MSG-KVLL                           
020100     PERFORM IMS-INSERT-MSG                                               
020200     .                                                                    
020300     SKIP3                                                                
020400     EJECT                                                                
020500* IMS SEKTIONER                                                           
020600                                                                          
020700 IMS-GET-MSG SECTION.                                                     
020800                                                                          
020900     MOVE '  QC' TO GODK-STATUSKODER                                      
021000     CALL CBLTDLI USING GU MSG-PCB MSG-IO-MAIL-AREA                       
021100     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
021200     PERFORM IMS-STATUSKONTROLL                                           
021300     .                                                                    
021400     SKIP2                                                                
021500 IMS-INSERT-MSG SECTION.                                                  
021600     MOVE LOW-VALUE TO MSG-KDZZ                                           
021700     MOVE SPACE TO GODK-STATUSKODER                                       
021800*    -- OBS: INGET MODNAMN BEHÖVS VIDA NROP FRÅN WEB                      
021900     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-MOD-AREA                      
022000     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
022100     PERFORM IMS-STATUSKONTROLL                                           
022200     .                                                                    
022300     EJECT                                                                
022400 IMS-STATUSKONTROLL SECTION.                                              
022500                                                                          
022600     SET STATUS-IX TO 1                                                   
022700     SEARCH GODK-STATUS                                                   
022800       AT END                                                             
022900         CALL FELLOG                                                      
023000       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
023100         CONTINUE                                                         
023200     END-SEARCH                                                           
023300     .                                                                    
