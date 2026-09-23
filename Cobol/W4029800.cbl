000100 ID DIVISION.                                                             
000200     SKIP2                                                                
000300 PROGRAM-ID.     W4029800.                                                
000400 AUTHOR.         GUNNAR LARSSON, IDK.                                     
000500 DATE-WRITTEN.   AUG 1991.                                                
000600                                                                          
000700     REMARKS.                                                             
000800*                                                                         
000900*    FUNKTION:                                                            
001000*        BAKGRUNDS-MPP FÖR UPPDATERING VID ORDERAVSLUT.                   
001100*                                                                         
001200*        - MSG LÄSES. FELAKTIG IDTRANS MEDFÖR FELLOG.                     
001300*        - W413AVSO ANROPAS.                                              
001400*          ALL UPPDATERING SKER I W413AVSO.                               
001500*                                                                         
001600*        - SIST MEN INTE MINST SKER START AV W40293(TRANS.SKAPARE)        
001700*                                                                         
001800*        - ETRACKER 856434. LAGT TILL EN LÄSNING AV WDQ2.                 
001900*             TINA 050214.                                                
002000*                                                                         
002100*    INDATA:                                                              
002200*        TRANSAKTION: W4T298X  MID: W4I29801                              
002300*                                                                         
002400*    UTDATA:                                                              
002500*        TRANSAKTION: W4T293X                                             
002600*                                                                         
002700*    ÖVRIGT:                                                              
002800*        OM YTTERLIGARE IDTRANS SKALL TILLÅTAS, SÅ MÅSTE                  
002900*        FÖLJANDE ÄNDRAS:                                                 
003000*          - 88-NIVÅN GODK-MID.                                           
003100*                                                                         
003200* HÖSTEN 2004 GÖRAN KJELLSON                                              
003300* ETRACKER 887753                                                         
003400*                                                                         
003500     EJECT                                                                
003600 ENVIRONMENT DIVISION.                                                    
003700     SKIP2                                                                
003800 DATA DIVISION.                                                           
003900     SKIP2                                                                
004000 WORKING-STORAGE SECTION.                                                 
004100*    -- CHECKED BY WY2000                                                 
004200     SKIP3                                                                
004300*                                                                         
004400 77  IDPGM                   PIC  X(8)           VALUE 'W4029800'.        
004500 77  FELTEXT                 PIC  X(80)          VALUE SPACE.             
004600 77  JA                      PIC  X(1)           VALUE 'J'.               
004700 77  NEJ                     PIC  X(1)           VALUE 'N'.               
004800 77  SPRAK-IX                PIC S9(9) COMP SYNC VALUE +0.                
004900*                                                                         
005000 77  W-IDTRANS               PIC  X(4)           VALUE SPACE.             
005100     88  GODK-MID                                VALUE '4203'             
005200                                                       '4213'             
005300                                                       '4223'             
005400                                                       '4233'             
005500                                                       '4243'             
005600                                                       '4252'             
005700                                                       '4255'             
005800                                                       '4257'             
005810                                                       '4258'             
005900                                                       '4299'             
005910                                                       'FIXA'.            
006000     EJECT                                                                
006300*                                                                         
006400 01  FILLER                  PIC X(16)   VALUE 'GEN-SUBPGM   '.           
006500 01  GENERELLA-SUBPROGRAM.                                                
006600*                                                                         
006700     03  CBLTDLI             PIC  X(8)           VALUE 'CBLTDLI '.        
006800     03  FELLOG              PIC  X(8)           VALUE 'FELLOG  '.        
006900     03  W413AVSO            PIC  X(8)           VALUE 'W413AVSO'.        
007000     EJECT                                                                
007100 01  FILLER                  PIC X(16)   VALUE 'AVSO-W413AVSO  '.         
007200     -COPY  W413AVSO                                                      
007300     EJECT                                                                
007400 01  FILLER                  PIC X(16)   VALUE 'W4I29801       '.         
007500******************************************************************        
007600*    MSG - INDATA TILL W4029800 FRÅN 'SÄNDANDE' PROGRAM.                  
007700******************************************************************        
007800     -COPY  W4I29801                                                      
007900     EJECT                                                                
008000 01  FILLER                  PIC X(16)   VALUE 'MSG-IO-AREA'.             
008100******************************************************************        
008200*    MSG-IO-AREA.                                                *        
008300******************************************************************        
008400     SKIP2                                                                
008500 01  -COPY WMSGAREA                                                       
008600     EJECT                                                                
008700 01  FILLER                  PIC X(16)   VALUE '4293-MID   '.             
008800     -COPY W4I29301  -PRE 4293-                                           
008900     EJECT                                                                
009000*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
009100*                                                                         
009200 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
009300     SKIP3                                                                
009400 01  NYCKLAR-TILL-DLI.                                                    
009500     03  W-IDORDER-X.                                                     
009600         05  W-IDORDER           PIC S9(7)   VALUE ZERO COMP-3.           
009700                                                                          
009800     03  W-IDDC-B6-X.                                                     
009900         05 W-IDDC-B6                  PIC X(2).                          
010000                                                                          
010100     SKIP2                                                                
010200*    --- STATUS-KOD FRÅN IMS                                              
010300 01  STATUS-WS                   PIC XX.                                  
010400     88  SEGMENT-FINNS                       VALUE '  '.                  
010500     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
010600     SKIP2                                                                
010700 01  GODK-STATUSKODER.                                                    
010800     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
010900     SKIP3                                                                
011000 01  SSA1                        PIC X(64).                               
011100     EJECT                                                                
011200*    --- IMS FUNKTIONSKODER                                               
011300*01  -COPY W0003                                                          
011400     EJECT                                                                
011500*    ---  DLI INPUT-OUTPUT AREA                                           
011600                                                                          
011700 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDQ201'.                      
011800 01  DLI-IO-WDQ201.                                                       
011900*    03  -COPY WDQ201                                                     
012000                                                                          
012100 01  FILLER               PIC X(16)   VALUE 'WDB601 AREA'.                
012200 01   DLI-IO-AREA-B601.                                                   
012300*     03  -COPY WDB601                                                    
012400                                                                          
012500     EJECT                                                                
012600 LINKAGE SECTION.                                                         
012700*01  -COPY W0009  -PRE MSG-                                               
012800*01  -COPY W0009  -PRE ALT-                                               
012900*01  -COPY W0008  -PRE WDQ2-                                              
013000     05  FILLER                  PIC X.                                   
013100*01  -COPY W0008  -PRE WDB6-                                              
013200     05  FILLER                  PIC X.                                   
013300     EJECT                                                                
013400 01  AVSO-WDE6-PCB               PIC X.                                   
013500 01  AVSO-ORQA-PCB               PIC X.                                   
013600 01  AVSO-WDQ2-PCB               PIC X.                                   
013700 01  AVSO-GMTB-PCB               PIC X.                                   
013800 01  AVSO-XXKA-PCB               PIC X.                                   
013900 01  AVSO-4437-PCB               PIC X.                                   
014000 01  AVSO-XXKE-PCB               PIC X.                                   
014100 01  AVSO-XXKF-PCB               PIC X.                                   
014200 01  AVSO-XXKG-PCB               PIC X.                                   
014300 01  AVSO-XXKH-PCB               PIC X.                                   
014400 01  AVSO-XXKI-PCB               PIC X.                                   
014500 01  AVSO-XXKP-PCB               PIC X.                                   
014510 01  AVSO-WDB2-PCB               PIC X.                                   
014520 01  AVSO-WDB6-PCB               PIC X.                                   
014600 01  AVSO-WDP7-PCB               PIC X.                                   
014700 01  TRAN-XXKB-PCB               PIC X.                                   
014800 01  ORDN-ORQL-PCB               PIC X.                                   
014900 01  ORDN-PROC-PCB               PIC X.                                   
015000 01  ORDN-ORQI-PCB               PIC X.                                   
015100 01  ORDN-WDQ3-PCB               PIC X.                                   
015200     EJECT                                                                
015300 PROCEDURE DIVISION USING  MSG-PCB ALT-PCB WDQ2-PCB WDB6-PCB              
015400      AVSO-WDE6-PCB AVSO-ORQA-PCB AVSO-WDQ2-PCB                           
015500      AVSO-GMTB-PCB AVSO-XXKA-PCB AVSO-4437-PCB AVSO-XXKE-PCB             
015600      AVSO-XXKF-PCB AVSO-XXKG-PCB AVSO-XXKH-PCB AVSO-XXKI-PCB             
015700      AVSO-XXKP-PCB AVSO-WDB2-PCB AVSO-WDB6-PCB AVSO-WDP7-PCB             
015710      TRAN-XXKB-PCB                                                       
015800      ORDN-ORQL-PCB ORDN-PROC-PCB ORDN-ORQI-PCB ORDN-WDQ3-PCB.            
015900                                                                          
016000     ENTRY 'DLITCBL' USING MSG-PCB ALT-PCB WDQ2-PCB WDB6-PCB              
016100      AVSO-WDE6-PCB AVSO-ORQA-PCB AVSO-WDQ2-PCB                           
016200      AVSO-GMTB-PCB AVSO-XXKA-PCB AVSO-4437-PCB AVSO-XXKE-PCB             
016300      AVSO-XXKF-PCB AVSO-XXKG-PCB AVSO-XXKH-PCB AVSO-XXKI-PCB             
016400      AVSO-XXKP-PCB AVSO-WDB2-PCB AVSO-WDB6-PCB AVSO-WDP7-PCB             
016410      TRAN-XXKB-PCB                                                       
016500      ORDN-ORQL-PCB ORDN-PROC-PCB ORDN-ORQI-PCB ORDN-WDQ3-PCB.            
016600     SKIP2                                                                
016700     PERFORM IMS-GET-MSG                                                  
016800                                                                          
016900     IF SEGMENT-FINNS                                                     
017000         PERFORM A-INIT                                                   
017100                                                                          
017200         PERFORM B-ANROPA-WOPS                                            
017300                                                                          
019100         PERFORM C-STARTA-TRANSPGM-4293                                   
019400     END-IF                                                               
019500                                                                          
019600     MOVE ZERO TO RETURN-CODE                                             
019700     GOBACK                                                               
019800     .                                                                    
019900     EJECT                                                                
020000 A-INIT SECTION.                                                          
020100                                                                          
020200     MOVE MSG-INDATA-MINUS-1-TRANSKOD  TO MID-W4I29801                    
020300     MOVE MSG-IDTRANS-1                TO W-IDTRANS                       
020400                                                                          
020500     IF NOT GODK-MID                                                      
020600       STRING 'FELAKTIG IDTRANS: ' W-IDTRANS                              
020700         DELIMITED BY SIZE INTO FELTEXT                                   
020800       CALL FELLOG                                                        
020900     END-IF                                                               
021000     .                                                                    
021100     EJECT                                                                
021200 B-ANROPA-WOPS SECTION.                                                   
021300                                                                          
021400     MOVE MID-IDDISTR          TO AVSO-IDDISTR                            
021500     MOVE MID-IDKUNDNR         TO AVSO-IDKUNDNR                           
021600     MOVE MID-IDKUNDRF         TO AVSO-IDKUNDRF                           
021700     MOVE MID-IDORDER          TO AVSO-IDORDER                            
021800     MOVE +0                   TO AVSO-TIRFS                              
021900                                  AVSO-TIAAMMDD                           
022000                                  AVSO-TIHHMM                             
022100     MOVE SPACE                TO AVSO-IDDC                               
022200     MOVE W-IDTRANS            TO AVSO-IDTRANS                            
022300*FIX                                                                      
022400*    IF MID-IDORDER = 554531                                              
022500*       MOVE WC-CDC-SE  TO AVSO-IDDC                                      
022600*    END-IF                                                               
022700* FIX SLUT                                                                
022800                                                                          
022900     CALL W413AVSO USING AVSO-W413AVSO AVSO-WDE6-PCB AVSO-ORQA-PCB        
023000           AVSO-WDQ2-PCB AVSO-GMTB-PCB AVSO-XXKA-PCB                      
023100           AVSO-4437-PCB AVSO-XXKE-PCB AVSO-XXKF-PCB AVSO-XXKG-PCB        
023200           AVSO-XXKH-PCB AVSO-XXKI-PCB AVSO-XXKP-PCB                      
023201           AVSO-WDB2-PCB AVSO-WDB6-PCB                                    
023210           AVSO-WDP7-PCB                                                  
023300           TRAN-XXKB-PCB ORDN-ORQL-PCB ORDN-PROC-PCB                      
023400           ORDN-ORQI-PCB ORDN-WDQ3-PCB                                    
023500     .                                                                    
023600     EJECT                                                                
023700 C-STARTA-TRANSPGM-4293 SECTION.                                          
023800                                                                          
023900     COMPUTE MSG-KVLL = LENGTH OF 4293-MID-W4I29301 + 17                  
024000     MOVE 'W4T293X '         TO MSG-KDTRANS-1                             
024100     MOVE '4298'             TO MSG-IDTRANS-1                             
024200                                                                          
024300     MOVE SPACE              TO 4293-MID-W4I29301                         
024400* LÄS WDQ2 FÖR ATT TA REDA PÅ OM ORDERN ÄR EN VERKSTADSORDER              
024500* DÄR MAN REDAN HAR BERÄKNAT ORDERINGÅNGEN.                               
024600     MOVE MID-IDORDER TO W-IDORDER                                        
024700     PERFORM IMS-GET-WDQ201                                               
024800     MOVE OHUV-IDDC-PRIM     TO W-IDDC-B6                                 
024900     PERFORM IMS-GU-WDB601                                                
025000     IF OHUV-IDSYSTEM = ('LDC ' OR 'TACD')  AND                           
025100                         W-IDTRANS = '4255' AND                           
025200                        DCS-SDC AND DCS-IDLANDX2 = 'SE'                   
025300       MOVE JA TO 4293-MID-FLORDING                                       
025400     END-IF                                                               
025500***                                                                       
025600     MOVE MID-IDORDER        TO 4293-MID-IDORDER                          
025700     MOVE MID-IDDISTR        TO 4293-MID-IDDISTR                          
025800     MOVE MID-IDKUNDNR       TO 4293-MID-IDKUNDNR                         
025900     MOVE MID-IDKUNDRF       TO 4293-MID-IDKUNDRF                         
026000                                                                          
026100     MOVE 4293-MID-W4I29301  TO MSG-MID-OUT                               
026200                                                                          
026300     PERFORM IMS-INSERT-ALT-MSG-4293                                      
026400     .                                                                    
026500     EJECT                                                                
026600 IMS-GET-MSG SECTION.                                                     
026700                                                                          
026800     MOVE    '  QC'          TO    GODK-STATUSKODER                       
026900     CALL    CBLTDLI         USING GU   MSG-PCB MSG-IO-AREA               
027000     MOVE    MSG-STATUS-CODE TO    STATUS-WS                              
027100     PERFORM IMS-STATUSKONTROLL                                           
027200     .                                                                    
027300     SKIP2                                                                
027400 IMS-INSERT-ALT-MSG-4293 SECTION.                                         
027500                                                                          
027600     MOVE    '  '            TO    GODK-STATUSKODER                       
027700     CALL    CBLTDLI         USING ISRT ALT-PCB MSG-IO-AREA               
027800     MOVE    ALT-STATUS-CODE TO    STATUS-WS                              
027900     PERFORM IMS-STATUSKONTROLL                                           
028000     .                                                                    
028100     SKIP2                                                                
028200 IMS-GET-WDQ201 SECTION.                                                  
028300                                                                          
028400     STRING 'WDQ201  (IDORDER  =' W-IDORDER-X ')'                         
028500          DELIMITED BY SIZE INTO SSA1                                     
028600     MOVE '    ' TO GODK-STATUSKODER                                      
028700     CALL CBLTDLI USING GU WDQ2-PCB DLI-IO-WDQ201 SSA1                    
028800     MOVE WDQ2-STATUS-CODE TO STATUS-WS                                   
028900     PERFORM IMS-STATUSKONTROLL                                           
029000     .                                                                    
029100     EJECT                                                                
029200 IMS-GU-WDB601    SECTION.                                                
029300     STRING 'WDB601  (IDDC     =' W-IDDC-B6-X ')'                         
029400          DELIMITED BY SIZE INTO SSA1                                     
029500     MOVE '  GE' TO GODK-STATUSKODER                                      
029600     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-AREA-B601 SSA1                 
029700     MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
029800     PERFORM IMS-STATUSKONTROLL                                           
029900     IF SEGMENT-SAKNAS                                                    
030000         MOVE SPACE TO DCS-KDDC                                           
030100     END-IF                                                               
030200     .                                                                    
030300     EJECT                                                                
030400 IMS-STATUSKONTROLL SECTION.                                              
030500                                                                          
030600     SET STATUS-IX TO 1                                                   
030700     SEARCH GODK-STATUS                                                   
030800       AT END                                                             
030900         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
031000         DELIMITED BY SIZE INTO FELTEXT                                   
031100         CALL FELLOG                                                      
031200       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
031300         CONTINUE                                                         
031400     END-SEARCH                                                           
031500     .                                                                    
