000100******************************************************************        
000200*     THIS PROGRAM ALSO HAS A WEB-LDC VERSION CALLED WL0144      *        
000300******************************************************************        
000400 ID DIVISION.                                                             
000500 PROGRAM-ID.     W4050900.                                                
000600 AUTHOR.         LARS THELL      (MG).                                    
000700 DATE-WRITTEN.   90/10/23.                                                
000800                                                                          
000900     REMARKS.                                                             
001000*    FUNKTION:                                                            
001100*        PROGRAMMET HANTERAR BILDEN;                                      
001200*        FRÅGA PÅ DISTRIKT - ARTIKEL                                      
001300*                                                                         
001400*        PROGRAMMET LÄSER      WLORQI (WDQ2)                              
001500*        PROGRAMMET LÄSER      WLORQF (WDQ4)                              
001600*        PROGRAMMET LÄSER      WLORQH (WDQ4)                              
001700*        PROGRAMMET LÄSER      WDE4C                                      
001800*        PROGRAMMET LÄSER      WDE6                                       
001900*                                                                         
002000*    INDATA.                                                              
002100*        TRANSAKTION: W4T509                                              
002200*        MID:         W4I50901                                            
002300*                                                                         
002400*    UTDATA.                                                              
002500*        MOD:         W4O50901                                            
002600                                                                          
002700     SKIP3                                                                
002800 ENVIRONMENT DIVISION.                                                    
002900     EJECT                                                                
003000 DATA DIVISION.                                                           
003100 WORKING-STORAGE SECTION.                                                 
003200*    -- CHECKED BY WY2000                                                 
003300     SKIP3                                                                
003400*    -COPY WY2000W1                                                       
003500     SKIP3                                                                
003600 77  IDPGM                       PIC X(08)   VALUE 'W4050900'.            
003700                                                                          
003800*    --- ARBETSFÄLT FÖR FELMEDDELANDEN VID CALL ABEND/FELLOG              
003900 77  FELTEXT                     PIC X(80) VALUE SPACE.                   
004000                                                                          
004100 77  JA                          PIC X       VALUE 'J'.                   
004200 77  NEJ                         PIC X       VALUE 'N'.                   
004300                                                                          
004400*    --- INDEX FÖR BLÄDDRINGSRADER                                        
004500 77  INDX                        PIC S9(4)  VALUE +0    COMP SYNC.        
004600 77  MAX-RAD                     PIC S9(4)  VALUE +14   COMP SYNC.        
004700 77  SPRAK-IX                    PIC S9(9)  VALUE +0    COMP SYNC.        
004800                                                                          
004900*    --- ARBETSFÄLT FÖR AKTUELLA NYCKELVÄRDEN FRÅN SKÄRMEN                
005000 77  WS-IDDC                     PIC X(2)    VALUE SPACE.                 
005100 77  IDDC-WS                     PIC X(2)    VALUE SPACE.                 
005200 77  WS-IDORDNR7                 PIC X(7)    VALUE SPACE.                 
005300 77  WS-IDKOLLI                  PIC X(5)    VALUE SPACE.                 
005400 77  WS-IDPRODNR                 PIC X(7)    VALUE SPACE.                 
005500                                                                          
005600 01  W-SPAR-IDKUNDRF.                                                     
005700     03  W-SPAR-IDORDNR7         PIC X(7)    VALUE '+++++++'.             
005800     03  FILLER                  PIC X(3)    VALUE '+++'.                 
005900                                                                          
006000 77  W-IDDB                      PIC X(6)    VALUE SPACE.                 
006100 77  W-TIAAMMDD                  PIC 9(6)    VALUE ZERO.                  
006200 77  WS-IDPURAD                  PIC S9(5)   VALUE ZERO  COMP-3.          
006300                                                                          
006400 77  NYCKLAR-SW                  PIC X       VALUE 'J'.                   
006500     88  NYCKLAR-OK                          VALUE 'J'.                   
006600     88  NYCKLAR-FEL                         VALUE 'N'.                   
006700                                                                          
006800 77  ALLT-SW                     PIC X       VALUE 'J'.                   
006900     88  ALLT-OK                             VALUE 'J'.                   
007000                                                                          
007100 77  IDDC-SW                     PIC X       VALUE 'N'.                   
007200     88  IDDC-FINNS                          VALUE 'J'.                   
007300     88  IDDC-SAKNAS                         VALUE 'N'.                   
007400                                                                          
007500 77  FLER-KOLLI-SW               PIC X       VALUE 'N'.                   
007600     88  FLER-KOLLI-FINNS                    VALUE 'J'.                   
007700                                                                          
007800*      --- VALID IDDC CODES                                               
007900*                                                                         
008000*01    -COPY WWDCKONS                                                     
008100       EJECT                                                              
008200 77  W-IDTRANS                   PIC X(4)    VALUE SPACE.                 
008300     88  EGEN-MID                            VALUE '4509'.                
008400     88  GODK-MID                            VALUE '4501' '4502'          
008500                                                   '4503' '4504'          
008600                                                   '4505' '4506'          
008700                                                   '4507' '4508'          
008800                                                   '4509'.                
008900     EJECT                                                                
009000*    --- SUBPROGRAM OCH PARAMETERAREOR                                    
009100 01  GENERELLA-SUBPROGRAM.                                                
009200     03  WMEDKONV                PIC X(8)    VALUE 'WMEDKONV'.            
009300     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
009400     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
009500     03  W005INIT                PIC X(8)    VALUE 'W005INIT'.            
009600     EJECT                                                                
009700*    --- PARAMETRAR TILL SUBPROGRAM W005INIT                              
009800*01 -COPY WMSGINIT                                                        
009900     EJECT                                                                
010000 01  HELP-AREOR.                                                          
010100*                                                                         
010200     03  FILLER            PIC X(16)         VALUE 'HELP-AREOR'.          
010300     SKIP2                                                                
010400 01  SPAR-AREOR.                                                          
010500*                                                                         
010600     03  FILLER            PIC X(16)         VALUE 'SPAR-AREOR'.          
010700     03  SPAR-IDORDER      PIC S9(7)  COMP-3 VALUE +0.                    
010800     03  SPAR-IDDC         PIC  X(2)         VALUE SPACE.                 
010900     EJECT                                                                
011000*    --- PARAMETRAR TILL SUBPROGRAM WMEDKONV                              
011100*   -COPY WMEDAREA                                                        
011200     SKIP3                                                                
011300 01  MESSAGE-CODES.                                                       
011400     03  INF-FIRST-PAGE-006      PIC X(3)    VALUE '006'.                 
011500     03  INF-MORE-INFO-105       PIC X(3)    VALUE '105'.                 
011600     03  ERR-WRONG-KEY-401       PIC X(3)    VALUE '401'.                 
011700     EJECT                                                                
011800*    --- DISTRIKTSCOPYTEXTER                                              
011900 01  TEST-IDDISTR              PIC S9(5) COMP-3.                          
012000*01  FILLER -COPY WWDIST03 -RED TEST-IDDISTR.                             
012100     EJECT                                                                
012200*01  FILLER -COPY WWDIST19 -RED TEST-IDDISTR.                             
012300     EJECT                                                                
012400*01  FILLER -COPY WWDIST88 -RED TEST-IDDISTR.                             
012500     EJECT                                                                
012600*    --- AREOR FÖR MFS OCH SKÄRMHANTERING                                 
012700*                                                                         
012800 01  FILLER                      PIC X(16)   VALUE 'MID-AREA'.            
012900     SKIP3                                                                
013000*01  MID -COPY W4I50901                                                   
013100     EJECT                                                                
013200 01  FILLER                      PIC X(16)  VALUE 'MSG/MOD-AREA'.         
013300     SKIP3                                                                
013400*01  -COPY WMSGAREA                                                       
013500     EJECT                                                                
013600     03  MOD REDEFINES MSG-AREA.                                          
013700*      05  -COPY W4O50901                                                 
013800     EJECT                                                                
013900 01  FILLER                      PIC X(16)   VALUE 'MFS-AREA'.            
014000     SKIP3                                                                
014100*01  -COPY WMFSAREA                                                       
014200     EJECT                                                                
014300*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
014400*                                                                         
014500 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
014600     SKIP3                                                                
014700 01  NYCKLAR-TILL-DLI.                                                    
014800     SKIP2                                                                
014900   03    W-WDQ401KY-X.                                                    
015000     05    W-Q401KY-IDORDER      PIC S9(7)   VALUE ZERO  COMP-3.          
015100     05    W-Q401KY-IDDC         PIC  X(2)   VALUE SPACE.                 
015200     05    W-Q401KY-ADLAGOMR     PIC S9(3)   VALUE ZERO  COMP-3.          
015300     05    W-Q401KY-ADGANG       PIC S9(3)   VALUE ZERO  COMP-3.          
015400     05    W-Q401KY-ADPLATS      PIC S9(5)   VALUE ZERO  COMP-3.          
015500     05    W-Q401KY-IDARTNR      PIC S9(9)   VALUE ZERO  COMP-3.          
015600     05    W-Q401KY-IDLOPNR      PIC S9(3)   VALUE ZERO  COMP-3.          
015700     SKIP2                                                                
015800   03    W-WDQ4B1KY-X.                                                    
015900     05    W-Q4B1KY-IDARTNR      PIC S9(9)   VALUE ZERO  COMP-3.          
016000     05    W-Q4B1KY-IDLOPNR      PIC S9(3)   VALUE ZERO  COMP-3.          
016100     05    W-Q4B1KY-IDGMTREF.                                             
016200       07  W-Q4B1KY-IDDISTR      PIC S9(5)   VALUE ZERO  COMP-3.          
016300       07  W-Q4B1KY-IDKUNDNR     PIC S9(7)   VALUE ZERO  COMP-3.          
016400       07  W-Q4B1KY-IDKUNDRF.                                             
016500        09 W-Q4B1KY-IDORDNR7     PIC 9(07)   VALUE ZERO.                  
016600        09 FILLER                PIC X(03)   VALUE SPACE.                 
016700     05    W-Q4B1KY-IDORDER      PIC S9(7)   VALUE ZERO  COMP-3.          
016800     05    W-Q4B1KY-IDDC         PIC  X(2)   VALUE SPACE.                 
016900     05    W-Q4B1KY-ADLAGOMR     PIC S9(3)   VALUE ZERO  COMP-3.          
017000     05    W-Q4B1KY-ADGANG       PIC S9(3)   VALUE ZERO  COMP-3.          
017100     05    W-Q4B1KY-ADPLATS      PIC S9(5)   VALUE ZERO  COMP-3.          
017200     SKIP2                                                                
017300   03    W-WDQ4B1KY-MIN-X.                                                
017400     05    W-Q4B1KY-MIN-IDARTNR  PIC S9(9)   VALUE ZERO  COMP-3.          
017500     05    W-Q4B1KY-MIN-IDLOPNR  PIC S9(3)   VALUE ZERO  COMP-3.          
017600     05    W-Q4B1KY-MIN-IDGMTREF.                                         
017700       07  W-Q4B1KY-MIN-IDDISTR  PIC S9(5)   VALUE ZERO  COMP-3.          
017800       07  W-Q4B1KY-MIN-IDKUNDNR PIC S9(7)   VALUE ZERO  COMP-3.          
017900       07  W-Q4B1KY-MIN-IDKUNDRF.                                         
018000        09 W-Q4B1KY-MIN-IDORDNR7 PIC 9(07)   VALUE ZERO.                  
018100        09 FILLER                PIC X(03)   VALUE SPACE.                 
018200     05    W-Q4B1KY-MIN-IDORDER  PIC S9(7)   VALUE ZERO  COMP-3.          
018300     05    W-Q4B1KY-MIN-IDDC     PIC  X(2)   VALUE SPACE.                 
018400     05    W-Q4B1KY-MIN-ADLAGOMR PIC S9(3)   VALUE ZERO  COMP-3.          
018500     05    W-Q4B1KY-MIN-ADGANG   PIC S9(3)   VALUE ZERO  COMP-3.          
018600     05    W-Q4B1KY-MIN-ADPLATS  PIC S9(5)   VALUE ZERO  COMP-3.          
018700     SKIP2                                                                
018800   03    W-WDQ4B1KY-MAX-X.                                                
018900     05    W-Q4B1KY-MAX-IDARTNR  PIC S9(9)   VALUE ZERO  COMP-3.          
019000     05    W-Q4B1KY-MAX-IDLOPNR  PIC S9(3)   VALUE ZERO  COMP-3.          
019100     05    W-Q4B1KY-MAX-IDGMTREF.                                         
019200       07  W-Q4B1KY-MAX-IDDISTR  PIC S9(5)   VALUE ZERO  COMP-3.          
019300       07  W-Q4B1KY-MAX-IDKUNDNR PIC S9(7)   VALUE ZERO  COMP-3.          
019400       07  W-Q4B1KY-MAX-IDKUNDRF.                                         
019500        09 W-Q4B1KY-MAX-IDORDNR7 PIC 9(07)   VALUE ZERO.                  
019600        09 FILLER                PIC X(03)   VALUE SPACE.                 
019700     05    W-Q4B1KY-MAX-IDORDER  PIC S9(7)   VALUE ZERO  COMP-3.          
019800     05    W-Q4B1KY-MAX-IDDC     PIC  X(2)   VALUE SPACE.                 
019900     05    W-Q4B1KY-MAX-ADLAGOMR PIC S9(3)   VALUE ZERO  COMP-3.          
020000     05    W-Q4B1KY-MAX-ADGANG   PIC S9(3)   VALUE ZERO  COMP-3.          
020100     05    W-Q4B1KY-MAX-ADPLATS  PIC S9(5)   VALUE ZERO  COMP-3.          
020200     SKIP2                                                                
020300   03    W-WDE4KEY-X.                                                     
020400     05    W-E401KY-IDGMTREF.                                             
020500       07  W-E401KY-IDDISTR      PIC S9(5)   VALUE ZERO  COMP-3.          
020600       07  W-E401KY-IDKUNDNR     PIC S9(7)   VALUE ZERO  COMP-3.          
020700       07  W-E401KY-IDKUNDRF.                                             
020800        09 W-E401KY-IDORDNR5     PIC 9(05)   VALUE ZERO.                  
020900        09 FILLER                PIC X(05)   VALUE SPACE.                 
021000     05    W-E401KY-IDPRODNR     PIC S9(7)   VALUE ZERO  COMP-3.          
021100     05    W-E401KY-IDPLKLST     PIC S9(3)   VALUE ZERO  COMP-3.          
021200     SKIP2                                                                
021300   03    W-WDE4C1KY-X.                                                    
021400     05    W-E4C1KY-IDARTNR      PIC S9(9)   VALUE ZERO  COMP-3.          
021500     05    W-E4C1KY-IDPRODNR     PIC S9(7)   VALUE ZERO  COMP-3.          
021600     05    W-E4C1KY-IDPURAD      PIC S9(5)   VALUE ZERO  COMP-3.          
021700     SKIP2                                                                
021800   03    W-WDE4C1KY-MIN-X.                                                
021900     05    W-E4C1KY-MIN-IDARTNR  PIC S9(9)   VALUE ZERO  COMP-3.          
022000     05    W-E4C1KY-MIN-IDPRODNR PIC S9(7)   VALUE ZERO  COMP-3.          
022100     05    W-E4C1KY-MIN-IDPURAD  PIC S9(5)   VALUE ZERO  COMP-3.          
022200     SKIP2                                                                
022300   03    W-WDE4C1KY-MAX-X.                                                
022400     05    W-E4C1KY-MAX-IDARTNR  PIC S9(9)   VALUE ZERO  COMP-3.          
022500     05    W-E4C1KY-MAX-IDPRODNR PIC S9(7)   VALUE ZERO  COMP-3.          
022600     05    W-E4C1KY-MAX-IDPURAD  PIC S9(5)   VALUE ZERO  COMP-3.          
022700     SKIP2                                                                
022800   03    W-WDE421KY-X.                                                    
022900     05    W-E421KY-IDPRODNR     PIC S9(7)   VALUE ZERO  COMP-3.          
023000     05    W-E421KY-IDKOLLI      PIC S9(5)   VALUE ZERO  COMP-3.          
023100     SKIP2                                                                
023200   03 W-IDPRODNR-X.                                                       
023300       05  W-IDPRODNR-WDE611     PIC S9(7)   VALUE ZERO  COMP-3.          
023400   03 W-IDKOLLI-X.                                                        
023500       05  W-IDKOLLI-WDE611      PIC S9(5)   VALUE ZERO  COMP-3.          
023600     SKIP2                                                                
023700   03    W-IDORDER-X.                                                     
023800     05    W-IDORDER             PIC S9(7)   VALUE ZERO  COMP-3.          
023900     SKIP2                                                                
024000   03    W-IDARTNR-X.                                                     
024100     05    W-IDARTNR             PIC S9(9)   VALUE ZERO  COMP-3.          
024200     SKIP2                                                                
024300   03    W-IDDISTR-X.                                                     
024400     05    W-IDDISTR             PIC S9(5)   VALUE ZERO  COMP-3.          
024500     SKIP2                                                                
024600   03    W-IDKUNDNR-MIN-X.                                                
024700     05    W-IDKUNDNR-MIN        PIC S9(7)   VALUE ZERO  COMP-3.          
024800     SKIP2                                                                
024900   03    W-IDKUNDNR-MAX-X.                                                
025000     05    W-IDKUNDNR-MAX        PIC S9(7)   VALUE ZERO  COMP-3.          
025100     SKIP2                                                                
025200   03    W-IDDC-MIN-X.                                                    
025300     05    W-IDDC-MIN            PIC  X(2)   VALUE SPACE.                 
025400     SKIP2                                                                
025500   03    W-IDDC-MAX-X.                                                    
025600     05    W-IDDC-MAX            PIC  X(2)   VALUE SPACE.                 
025700     SKIP2                                                                
025800   03    W-IDDC-X.                                                        
025900     05    W-IDDC                PIC  X(2)   VALUE SPACE.                 
026000     SKIP2                                                                
026100   03    W-IDPURAD-X.                                                     
026200     05    W-IDPURAD             PIC S9(5)   VALUE ZERO  COMP-3.          
026300                                                                          
026400   03  W-IDDC-B6-X.                                                       
026500       05 W-IDDC-B6                  PIC X(2).                            
026600                                                                          
026700     SKIP2                                                                
026800*    --- STATUS-KOD FRÅN IMS                                              
026900 01  STATUS-WS                   PIC XX.                                  
027000     88  SEGMENT-FINNS                       VALUE '  '.                  
027100     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
027200     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
027300     88  SEGMENT-SLUT                        VALUE 'GB'.                  
027400     SKIP2                                                                
027500 01  GODK-STATUSKODER.                                                    
027600     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
027700     SKIP3                                                                
027800 01  SSA1                        PIC X(192).                              
027900 01  SSA2                        PIC X(64).                               
028000 01  SSA3                        PIC X(64).                               
028100 01  SSA4                        PIC X(64).                               
028200     EJECT                                                                
028300*    --- IMS FUNKTIONSKODER                                               
028400*01  -COPY W0003                                                          
028500     EJECT                                                                
028600*    ---  DLI INPUT-OUTPUT AREA                                           
028700 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA'.         
028800     SKIP3                                                                
028900 01  DLI-IO-AREA1.                                                        
029000*        05  -COPY WDQ201                                                 
029100     EJECT                                                                
029200 01  DLI-IO-AREA2.                                                        
029300     03  IO-AREA2                PIC X(4000)  VALUE SPACE.                
029400     SKIP3                                                                
029500     03  WLORQI12 REDEFINES IO-AREA2.                                     
029600*        05  -COPY WDQ212                                                 
029700     EJECT                                                                
029800 01  DLI-IO-AREA3.                                                        
029900     03  IO-AREA3                PIC X(3000)  VALUE SPACE.                
030000     SKIP3                                                                
030100     03  WLORQF01 REDEFINES IO-AREA3.                                     
030200*        05  -COPY WDQ401                                                 
030300     EJECT                                                                
030400     03  WLORQH01 REDEFINES IO-AREA3.                                     
030500*        05  -COPY WDQ4B1                                                 
030600     EJECT                                                                
030700     03  WDE4C1   REDEFINES IO-AREA3.                                     
030800*        05  -COPY WDE4C1                                                 
030900     EJECT                                                                
031000 01  FILLER                  PIC X(16)   VALUE 'DLI-IO-E421'.             
031100 01  DLI-IO-E421.                                                         
031200*    03  -COPY WDE421                                                     
031300     EJECT                                                                
031400 01  FILLER                  PIC X(16)   VALUE 'DLI-IO-E401-11'.          
031500 01  DLI-IO-E401-11.                                                      
031600     SKIP3                                                                
031700*    03      -COPY WDE401                                                 
031800     EJECT                                                                
031900*    03      -COPY WDE411                                                 
032000     EJECT                                                                
032100 01  DLI-IO-AREA7.                                                        
032200     03  IO-AREA7                PIC X(300)  VALUE SPACE.                 
032300                                                                          
032400     03  WDE401   REDEFINES IO-AREA7.                                     
032500*        05  -COPY WDE401 -PRE ARE7-                                      
032600     EJECT                                                                
032700 01  DLI-IO-AREA8.                                                        
032800*    05  -COPY WDE411 -PRE AREA8-                                         
032900 01  DLI-IOAREA-WDE611.                                                   
033000     03     -COPY WDE611                                                  
033100                                                                          
033200 01  FILLER               PIC X(16)   VALUE 'WDB601 AREA'.                
033300 01   DLI-IO-AREA-B601.                                                   
033400*     03  -COPY WDB601                                                    
033500                                                                          
033600     EJECT                                                                
033700 LINKAGE SECTION.                                                         
033800                                                                          
033900*01  -COPY W0009      -PRE MSG-                                           
034000     EJECT                                                                
034100*01  -COPY W0008      -PRE USEA-                                          
034200     05  FILLER                  PIC X.                                   
034300     EJECT                                                                
034400*01  -COPY W0008      -PRE ORQI-                                          
034500     05  FILLER                  PIC X.                                   
034600     EJECT                                                                
034700*01  -COPY W0008      -PRE ORQF-                                          
034800     05  FILLER                  PIC X.                                   
034900     EJECT                                                                
035000*01  -COPY W0008      -PRE ORQH-                                          
035100     05  FILLER                  PIC X.                                   
035200     EJECT                                                                
035300*01  -COPY W0008      -PRE WDE4C-                                         
035400     05  FILLER                  PIC X.                                   
035500     EJECT                                                                
035600*01  -COPY W0008      -PRE WDE4-                                          
035700     05  FILLER                  PIC X.                                   
035800     EJECT                                                                
035900*01  -COPY W0008      -PRE WDE6-                                          
036000     05  FILLER                  PIC X.                                   
036100     EJECT                                                                
036200*01  -COPY W0008      -PRE WDB6-                                          
036300     05  FILLER                  PIC X.                                   
036400     EJECT                                                                
036500 PROCEDURE DIVISION  USING MSG-PCB USEA-PCB ORQI-PCB ORQF-PCB             
036600                          ORQH-PCB WDE4C-PCB WDE4-PCB WDE6-PCB            
036700                          WDB6-PCB.                                       
036800     ENTRY 'DLITCBL' USING MSG-PCB USEA-PCB ORQI-PCB ORQF-PCB             
036900                          ORQH-PCB WDE4C-PCB WDE4-PCB WDE6-PCB            
037000                          WDB6-PCB.                                       
037100                                                                          
037200     PERFORM IMS-GET-MSG                                                  
037300     IF SEGMENT-FINNS                                                     
037400       PERFORM A-INIT                                                     
037500       PERFORM B-KOLLA-NYCKLAR                                            
037600       IF NYCKLAR-OK                                                      
037700           IF MFS-FIRST                                                   
037800             PERFORM C-FOERSTA-SIDA                                       
037900           ELSE                                                           
038000             IF MFS-NEXT                                                  
038100               PERFORM D-NAESTA-SIDA                                      
038200             ELSE                                                         
038300               PERFORM E-SAMMA-SIDA                                       
038400             END-IF                                                       
038500           END-IF                                                         
038600           IF ALLT-OK                                                     
038700               PERFORM F-LAES-VISA-INFO                                   
038800           END-IF                                                         
038900       END-IF                                                             
039000                                                                          
039100       COMPUTE MSG-KVLL = LENGTH OF MOD-W4O50901 + 4                      
039200       PERFORM IMS-INSERT-MSG                                             
039300     END-IF                                                               
039400                                                                          
039500     MOVE ZERO TO RETURN-CODE                                             
039600     GOBACK                                                               
039700     .                                                                    
039800     EJECT                                                                
039900 A-INIT SECTION.                                                          
040000                                                                          
040100     IF MSG-DUBBLA-TRANSKODER                                             
040200       MOVE MSG-INDATA-MINUS-2-TRANSKODER TO MID-W4I50901                 
040300       MOVE MSG-IDTRANS-2                 TO MFS-IDTRANS                  
040400       MOVE MSG-KDMFSFOR-2                TO MFS-KDMFSFOR                 
040500     ELSE                                                                 
040600       MOVE MSG-INDATA-MINUS-1-TRANSKOD   TO MID-W4I50901                 
040700       MOVE MSG-IDTRANS-1                 TO MFS-IDTRANS                  
040800       MOVE MSG-KDMFSFOR-1                TO MFS-KDMFSFOR                 
040900     END-IF                                                               
041000                                                                          
041100     MOVE MSG-KDTRTYP                     TO MFS-KDTRTYP                  
041200     MOVE MSG-IDPFK                       TO MFS-IDPFK                    
041300     MOVE MFS-IDTRANS                     TO W-IDTRANS                    
041400                                                                          
041500     MOVE LOW-VALUE                       TO MSG-AREA                     
041600     MOVE 'W4O509N1'                      TO MFS-IDMOD                    
041700     MOVE '4509'                          TO MOD-IDTRANS                  
041800     MOVE MFS-RENSA-FAELT                 TO MOD-TEMFSFEL                 
041900                                             MOD-TEMFSINF                 
042000                                                                          
042100     IF NOT EGEN-MID                                                      
042200       MOVE SPACE              TO MFS-KDTRTYP                             
042300       MOVE '7'                TO MFS-IDPFK                               
042400       IF GODK-MID                                                        
042500         CONTINUE                                                         
042600       ELSE                                                               
042700         PERFORM AA-NOLLSTAELL-MID-MOD                                    
042800       END-IF                                                             
042900     END-IF                                                               
043000                                                                          
043100     MOVE NEJ                  TO IDDC-SW                                 
043200                                  FLER-KOLLI-SW                           
043300     MOVE SPACE                TO W-IDDB                                  
043400     MOVE ZERO                 TO MOD-IDKOLLI-ENTER                       
043500                                  MOD-IDKOLLI-NEXT                        
043600                                  SPAR-IDORDER                            
043700                                  SPAR-IDDC                               
043800     .                                                                    
043900     EJECT                                                                
044000 AA-NOLLSTAELL-MID-MOD      SECTION.                                      
044100                                                                          
044200     MOVE ALL '+'           TO MID-W4I50901                               
044300     MOVE SPACE             TO MOD-IDDISTR-IN                             
044400                               MOD-IDDISTR-UT                             
044500                               MOD-IDKUNDNR-IN                            
044600                               MOD-IDKUNDNR-UT                            
044700                               MOD-IDARTNR-IN                             
044800                               MOD-IDARTNR-UT                             
044900                               MOD-IDDC-IN                                
045000                               MOD-IDDC-UT                                
045100                               MOD-IDORDNR7-IN                            
045200                               MOD-IDORDNR7-UT                            
045300                               MOD-IDKOLLI-IN                             
045400                               MOD-IDKOLLI-UT                             
045500                               MOD-IDPRODNR-IN                            
045600                               MOD-IDPRODNR-UT                            
045700     .                                                                    
045800     EJECT                                                                
045900 B-KOLLA-NYCKLAR SECTION.                                                 
046000                                                                          
046100     MOVE ALL '+'           TO MSGI-WMSGINIT                              
046200     MOVE '001'             TO MSGI-KDCALL                                
046300     MOVE MSG-SIGNON-USERID TO MSGI-IDUSER                                
046400     MOVE '4509'            TO MSGI-IDTRANS                               
046500     MOVE MSG-LTERM-NAME    TO MSGI-IDLTERM-USER                          
046600     IF EGEN-MID                                                          
046700        MOVE MID-IDPRODNR-IN    TO MSGI-IDPRODNR                          
046800        MOVE MID-IDDISTR-IN     TO MSGI-IDDISTR                           
046900        MOVE MID-IDKUNDNR-IN    TO MSGI-IDKUNDNR                          
047000        IF MID-IDORDNR7-IN      NOT = ALL '+'                             
047100           MOVE MID-IDORDNR7-IN TO W-SPAR-IDORDNR7                        
047200           MOVE W-SPAR-IDKUNDRF TO MSGI-IDKUNDRF                          
047300        END-IF                                                            
047400        MOVE MID-IDARTNR-IN     TO MSGI-IDARTNR                           
047500        MOVE MID-IDKOLLI-IN     TO MSGI-IDKOLLI                           
047600     END-IF                                                               
047700     CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                           
047800     MOVE MSGI-IDLAND-SPR   TO MED-IDSKYLT                                
047900                                                                          
048000     MOVE JA                   TO NYCKLAR-SW                              
048100                                                                          
048200     MOVE LOW-VALUE            TO W-WDQ4B1KY-MIN-X                        
048300                                  W-WDE4C1KY-MIN-X                        
048400                                  W-IDKUNDNR-MIN-X                        
048500                                  W-IDDC-MIN-X                            
048600                                                                          
048700     MOVE HIGH-VALUE           TO W-WDQ4B1KY-MAX-X                        
048800                                  W-WDE4C1KY-MAX-X                        
048900                                  W-IDKUNDNR-MAX-X                        
049000                                  W-IDDC-MAX-X                            
049100     PERFORM BA-KOLLA-IDDISTR                                             
049200     PERFORM BB-KOLLA-IDKUNDNR                                            
049300     PERFORM BC-KOLLA-IDARTNR                                             
049400     PERFORM BD-KOLLA-IDDC                                                
049500     IF IDDC-WS = ZERO                                                    
049600       MOVE    ' 0'            TO MOD-IDDC-UT                             
049700     ELSE                                                                 
049800       MOVE    WS-IDDC         TO MOD-IDDC-UT                             
049900     END-IF                                                               
050000     IF NYCKLAR-OK                                                        
050100       PERFORM BE-FLYTTA-OVRIGA-NYCKLAR                                   
050200       MOVE    MSGI-IDDISTR    TO MOD-IDDISTR-UT                          
050300       INSPECT MOD-IDDISTR-UT  REPLACING LEADING ZERO BY SPACE            
050400       MOVE    MSGI-IDKUNDNR   TO MOD-IDKUNDNR-UT                         
050500       INSPECT MOD-IDKUNDNR-UT REPLACING LEADING ZERO BY SPACE            
050600       MOVE    MSGI-IDARTNR    TO MOD-IDARTNR-UT                          
050700       INSPECT MOD-IDARTNR-UT  REPLACING LEADING ZERO BY SPACE            
050800      ELSE                                                                
050900       MOVE MFS-RENSA-FAELT    TO MOD-IDORDNR7-IN                         
051000                                  MOD-IDORDNR7-UT                         
051100                                  MOD-IDKOLLI-IN                          
051200                                  MOD-IDKOLLI-UT                          
051300                                  MOD-IDPRODNR-IN                         
051400                                  MOD-IDPRODNR-UT                         
051500                                  MOD-IDDISTR-UT                          
051600                                  MOD-IDKUNDNR-UT                         
051700                                  MOD-IDARTNR-UT                          
051800     END-IF                                                               
051900                                                                          
052000     IF GODK-MID                                                          
052100        CONTINUE                                                          
052200     ELSE                                                                 
052300       MOVE MFS-RENSA-FAELT    TO MOD-IDORDNR7-UT                         
052400                                  MOD-IDKOLLI-UT                          
052500                                  MOD-IDPRODNR-UT                         
052600                                                                          
052700     END-IF                                                               
052800                                                                          
052900     IF NYCKLAR-FEL                                                       
053000       MOVE ERR-WRONG-KEY-401  TO MED-IDMFSFEL                            
053100       CALL WMEDKONV USING     MED-WMEDAREA                               
053200       MOVE MED-MFSFEL         TO MOD-TEMFSFEL                            
053300       PERFORM MFS-RENSA-FAELT-IN                                         
053400       PERFORM MFS-RENSA-FAELT-UT                                         
053500     END-IF                                                               
053600     .                                                                    
053700     EJECT                                                                
053800 BA-KOLLA-IDDISTR     SECTION.                                            
053900                                                                          
054000*    -- KONTROLL AV IDDISTR                                               
054100     MOVE MFS-RENSA-FAELT      TO MOD-IDDISTR-IN                          
054200                                                                          
054300     IF MID-IDDISTR-IN         NOT = ALL '+'                              
054400       MOVE '7'                TO MFS-IDPFK                               
054500       MOVE SPACE              TO MFS-KDTRTYP                             
054600     END-IF                                                               
054700     IF MSGI-IDDISTR NUMERIC AND MSGI-IDDISTR > ZERO                      
054800       MOVE MSGI-IDDISTR       TO W-IDDISTR                               
054900                                  W-Q4B1KY-IDDISTR                        
055000                                  W-Q4B1KY-MIN-IDDISTR                    
055100                                  W-Q4B1KY-MAX-IDDISTR                    
055200                                  W-E401KY-IDDISTR                        
055300                                  TEST-IDDISTR                            
055400     ELSE                                                                 
055500*      MOVE NEJ                TO NYCKLAR-SW                              
055600       MOVE LOW-VALUE          TO W-Q4B1KY-MIN-IDGMTREF                   
055700       MOVE HIGH-VALUE         TO W-Q4B1KY-MAX-IDGMTREF                   
055800*                                                                         
055900     END-IF                                                               
056000     .                                                                    
056100     EJECT                                                                
056200                                                                          
056300 BB-KOLLA-IDKUNDNR    SECTION.                                            
056400                                                                          
056500*    -- KONTROLL AV IDKUNDNR                                              
056600     MOVE MFS-RENSA-FAELT      TO MOD-IDKUNDNR-IN                         
056700                                                                          
056800     IF MID-IDKUNDNR-IN        NOT = ALL '+'                              
056900       MOVE '7'                TO MFS-IDPFK                               
057000       MOVE SPACE              TO MFS-KDTRTYP                             
057100     END-IF                                                               
057200                                                                          
057300     IF MSGI-IDKUNDNR NUMERIC                                             
057400       IF MSGI-IDKUNDNR          =  ZERO                                  
057500           CONTINUE                                                       
057600        ELSE                                                              
057700           MOVE MSGI-IDKUNDNR  TO W-Q4B1KY-MIN-IDKUNDNR                   
057800                                  W-Q4B1KY-MAX-IDKUNDNR                   
057900                                  W-E401KY-IDKUNDNR                       
058000                                  W-IDKUNDNR-MIN                          
058100                                  W-IDKUNDNR-MAX                          
058200       END-IF                                                             
058300     END-IF                                                               
058400     .                                                                    
058500     EJECT                                                                
058600 BC-KOLLA-IDARTNR     SECTION.                                            
058700                                                                          
058800*    -- KONTROLL AV IDARTNR                                               
058900     MOVE MFS-RENSA-FAELT      TO MOD-IDARTNR-IN                          
059000                                                                          
059100     IF MID-IDARTNR-IN         NOT = ALL '+'                              
059200       MOVE '7'                TO MFS-IDPFK                               
059300       MOVE SPACE              TO MFS-KDTRTYP                             
059400     END-IF                                                               
059500     IF MSGI-IDARTNR  NUMERIC AND MSGI-IDARTNR  > ZERO                    
059600       MOVE MSGI-IDARTNR       TO W-IDARTNR                               
059700                                  W-Q401KY-IDARTNR                        
059800                                  W-Q4B1KY-IDARTNR                        
059900                                  W-Q4B1KY-MIN-IDARTNR                    
060000                                  W-Q4B1KY-MAX-IDARTNR                    
060100                                  W-E4C1KY-IDARTNR                        
060200                                  W-E4C1KY-MIN-IDARTNR                    
060300                                  W-E4C1KY-MAX-IDARTNR                    
060400     ELSE                                                                 
060500       MOVE NEJ                TO NYCKLAR-SW                              
060600     END-IF                                                               
060700     .                                                                    
060800     EJECT                                                                
060900 BD-KOLLA-IDDC SECTION.                                                   
061000                                                                          
061100     MOVE MFS-RENSA-FAELT      TO MOD-IDDC-IN                             
061200     IF EGEN-MID                                                          
061300        IF MID-IDDC-IN     = ALL '+'                                      
061400          MOVE MID-IDDC-UT TO WS-IDDC                                     
061500        ELSE                                                              
061600          MOVE MID-IDDC-IN TO WS-IDDC                                     
061700          MOVE '7'        TO MFS-IDPFK                                    
061800          MOVE SPACE      TO MFS-KDTRTYP                                  
061900        END-IF                                                            
062000     ELSE                                                                 
062100        MOVE MSGI-IDDC       TO WS-IDDC                                   
062200     END-IF                                                               
062300                                                                          
062400     MOVE WS-IDDC               TO IDDC-WS                                
062500     INSPECT IDDC-WS REPLACING ALL SPACES BY ZEROS                        
062600     IF DCS-IDDC NOT = WS-IDDC                                            
062700        MOVE WS-IDDC TO W-IDDC-B6                                         
062800        PERFORM IMS-GU-WDB601                                             
062900     END-IF                                                               
063000     IF DCS-KDDC = SPACE OR DCS-CDC-TR                                    
063100       IF IDDC-WS = ZERO                                                  
063200         CONTINUE                                                         
063300       ELSE                                                               
063400          MOVE MSGI-IDDC       TO WS-IDDC                                 
063500                                  W-IDDC                                  
063600                                  W-Q401KY-IDDC                           
063700                                  W-Q4B1KY-IDDC                           
063800                                  W-IDDC-MAX                              
063900                                  W-IDDC-MIN                              
064000       END-IF                                                             
064100     ELSE                                                                 
064200       IF DCS-DDC                                                         
064300         MOVE WC-CDC-SE        TO W-IDDC                                  
064400       ELSE                                                               
064500         MOVE WS-IDDC          TO W-IDDC                                  
064600       END-IF                                                             
064700       MOVE WS-IDDC            TO W-Q401KY-IDDC                           
064800                                  W-Q4B1KY-IDDC                           
064900                                  W-IDDC-MAX                              
065000                                  W-IDDC-MIN                              
065100     END-IF                                                               
065200                                                                          
065300*    KOD SOM ERSATTS AV WDB6-LÄSNINGEN OVAN                               
065400*    IF (GOOD-DC AND NOT CDC-TR) OR GOOD-DDC                              
065500*      IF GOOD-DDC                                                        
065600*        MOVE WC-CDC-SE        TO W-IDDC                                  
065700*      ELSE                                                               
065800*        MOVE WS-IDDC          TO W-IDDC                                  
065900*      END-IF                                                             
066000*      MOVE WS-IDDC            TO W-Q401KY-IDDC                           
066100*                                 W-Q4B1KY-IDDC                           
066200*                                 W-IDDC-MAX                              
066300*                                 W-IDDC-MIN                              
066400*    ELSE                                                                 
066500*      IF IDDC-WS = ZERO                                                  
066600*        CONTINUE                                                         
066700*      ELSE                                                               
066800*         MOVE MSGI-IDDC       TO WS-IDDC                                 
066900*                                 W-IDDC                                  
067000*                                 W-Q401KY-IDDC                           
067100*                                 W-Q4B1KY-IDDC                           
067200*                                 W-IDDC-MAX                              
067300*                                 W-IDDC-MIN                              
067400*      END-IF                                                             
067500*    END-IF                                                               
067600     .                                                                    
067700     EJECT                                                                
067800 BE-FLYTTA-OVRIGA-NYCKLAR SECTION.                                        
067900                                                                          
068000*    -- FLYTTA IDORDNR                                                    
068100     MOVE MFS-RENSA-FAELT      TO MOD-IDORDNR7-IN                         
068200                                                                          
068300     IF MID-IDORDNR7-IN        = ALL '+'                                  
068400       MOVE MID-IDORDNR7-UT    TO WS-IDORDNR7                             
068500     ELSE                                                                 
068600       MOVE MID-IDORDNR7-IN    TO WS-IDORDNR7                             
068700       INSPECT WS-IDORDNR7 REPLACING LEADING ZERO BY SPACE                
068800     END-IF                                                               
068900                                                                          
069000     MOVE WS-IDORDNR7          TO MOD-IDORDNR7-UT                         
069100                                                                          
069200*    -- FLYTTA IDKOLLI                                                    
069300     MOVE MFS-RENSA-FAELT      TO MOD-IDKOLLI-IN                          
069400                                                                          
069500     IF MID-IDKOLLI-IN         = ALL '+'                                  
069600       MOVE MID-IDKOLLI-UT     TO WS-IDKOLLI                              
069700     ELSE                                                                 
069800       MOVE MID-IDKOLLI-IN     TO WS-IDKOLLI                              
069900       INSPECT WS-IDKOLLI  REPLACING LEADING ZERO BY SPACE                
070000     END-IF                                                               
070100                                                                          
070200     MOVE WS-IDKOLLI           TO MOD-IDKOLLI-UT                          
070300                                                                          
070400*    -- FLYTTA IDPRODNR                                                   
070500     MOVE MFS-RENSA-FAELT      TO MOD-IDPRODNR-IN                         
070600                                                                          
070700     IF MID-IDPRODNR-IN        = ALL '+'                                  
070800       MOVE MID-IDPRODNR-UT    TO WS-IDPRODNR                             
070900     ELSE                                                                 
071000       MOVE MID-IDPRODNR-IN    TO WS-IDPRODNR                             
071100       INSPECT WS-IDPRODNR   REPLACING LEADING ZERO BY SPACE              
071200     END-IF                                                               
071300                                                                          
071400     MOVE WS-IDPRODNR          TO MOD-IDPRODNR-UT                         
071500                                                                          
071600     .                                                                    
071700     EJECT                                                                
071800 C-FOERSTA-SIDA SECTION.                                                  
071900                                                                          
072000     MOVE INF-FIRST-PAGE-006   TO MED-IDMFSFEL                            
072100     CALL WMEDKONV USING       MED-WMEDAREA                               
072200     MOVE MED-MFSFEL           TO MOD-TEMFSFEL                            
072300                                                                          
072400     .                                                                    
072500     EJECT                                                                
072600 D-NAESTA-SIDA SECTION.                                                   
072700                                                                          
072800     MOVE MID-IDDB-NEXT          TO W-IDDB                                
072900     IF MID-IDDB-NEXT            =  'WLORQH'                              
073000         MOVE MID-IDRADNR-NEXT   TO W-Q4B1KY-IDLOPNR                      
073100         MOVE MID-IDDISTR-NEXT   TO W-Q4B1KY-IDDISTR                      
073200         MOVE MID-IDKUNDNR-NEXT  TO W-Q4B1KY-IDKUNDNR                     
073300         MOVE MID-IDORDNR7-NEXT  TO W-Q4B1KY-IDORDNR7                     
073400         MOVE MID-IDORDER-NEXT   TO W-Q4B1KY-IDORDER                      
073500         MOVE MID-IDDC-NEXT      TO W-Q4B1KY-IDDC                         
073600         MOVE MID-ADLAGOMR-NEXT  TO W-Q4B1KY-ADLAGOMR                     
073700         MOVE MID-ADGANG-NEXT    TO W-Q4B1KY-ADGANG                       
073800         MOVE MID-ADPLATS-NEXT   TO W-Q4B1KY-ADPLATS                      
073900     ELSE                                                                 
074000         MOVE MID-IDRADNR-NEXT   TO W-E4C1KY-IDPURAD                      
074100         MOVE MID-IDPRODNR-NEXT  TO W-E4C1KY-IDPRODNR                     
074200                                    W-E421KY-IDPRODNR                     
074300         MOVE MID-IDKOLLI-NEXT   TO W-E421KY-IDKOLLI                      
074400     END-IF                                                               
074500     MOVE JA                   TO ALLT-SW                                 
074600     .                                                                    
074700     EJECT                                                                
074800 E-SAMMA-SIDA SECTION.                                                    
074900                                                                          
075000     MOVE MID-IDDB-ENTER             TO W-IDDB                            
075100     IF MID-IDDB-ENTER               =  'WLORQH'                          
075200         MOVE MID-IDRADNR-ENTER      TO W-Q4B1KY-IDLOPNR                  
075300         MOVE MID-IDDISTR-ENTER      TO W-Q4B1KY-IDDISTR                  
075400         MOVE MID-IDKUNDNR-ENTER     TO W-Q4B1KY-IDKUNDNR                 
075500         MOVE MID-IDORDNR7-ENTER     TO W-Q4B1KY-IDORDNR7                 
075600         MOVE MID-IDORDER-ENTER      TO W-Q4B1KY-IDORDER                  
075700         MOVE MID-IDDC-ENTER         TO W-Q4B1KY-IDDC                     
075800         MOVE MID-ADLAGOMR-ENTER     TO W-Q4B1KY-ADLAGOMR                 
075900         MOVE MID-ADGANG-ENTER       TO W-Q4B1KY-ADGANG                   
076000         MOVE MID-ADPLATS-ENTER      TO W-Q4B1KY-ADPLATS                  
076100     ELSE                                                                 
076200         MOVE MID-IDRADNR-ENTER      TO W-E4C1KY-IDPURAD                  
076300         MOVE MID-IDPRODNR-ENTER     TO W-E4C1KY-IDPRODNR                 
076400                                        W-E421KY-IDPRODNR                 
076500         MOVE MID-IDKOLLI-ENTER      TO W-E421KY-IDKOLLI                  
076600     END-IF                                                               
076700     MOVE JA                         TO ALLT-SW                           
076800     .                                                                    
076900     EJECT                                                                
077000 F-LAES-VISA-INFO SECTION.                                                
077100                                                                          
077200     MOVE +1                   TO INDX                                    
077300                                                                          
077400     EVALUATE TRUE                                                        
077500     WHEN W-IDDB               =  'WLORQH' OR SPACE                       
077600         IF DIST19-SATS                                                   
077700            CONTINUE                                                      
077800         ELSE                                                             
077900            PERFORM FA-BEHANDLA-ORDERRADKO                                
078000         END-IF                                                           
078100         IF INDX               >  MAX-RAD AND SEGMENT-FINNS OR            
078200         W-IDDISTR             =  ZERO                                    
078300             CONTINUE                                                     
078400          ELSE                                                            
078500             PERFORM FB-BEHANDLA-WDE4-ORDER                               
078600         END-IF                                                           
078700                                                                          
078800     WHEN W-IDDB               =  'WDE4C '                                
078900         PERFORM FB-BEHANDLA-WDE4-ORDER                                   
079000                                                                          
079100     END-EVALUATE                                                         
079200                                                                          
079300     IF INDX                   = +1                                       
079400         IF MFS-NEXT                                                      
079500             MOVE '115'        TO MED-IDMFSFEL                            
079600             CALL WMEDKONV     USING MED-WMEDAREA                         
079700             MOVE MED-MFSFEL   TO MOD-TEMFSFEL                            
079800          ELSE                                                            
079900             MOVE '005'        TO MED-IDMFSFEL                            
080000             CALL WMEDKONV     USING MED-WMEDAREA                         
080100             MOVE MED-MFSFEL   TO MOD-TEMFSFEL                            
080200             PERFORM MFS-RENSA-FAELT-UT                                   
080300         END-IF                                                           
080400     END-IF                                                               
080500     .                                                                    
080600     EJECT                                                                
080700 FA-BEHANDLA-ORDERRADKO SECTION.                                          
080800                                                                          
080900     IF MFS-FIRST                                                         
081000       PERFORM IMS-GET-ORQH01-OKVAL                                       
081100     ELSE                                                                 
081200       PERFORM IMS-GET-ORQH01-KVAL                                        
081300     END-IF                                                               
081400     PERFORM FAA-SKAPA-WDQ4-NYCKLAR-ENTER                                 
081500     PERFORM UNTIL SEGMENT-SAKNAS  OR                                     
081600                   SEGMENT-SLUT    OR                                     
081700                   INDX > MAX-RAD                                         
081800       PERFORM FAB-SKAPA-WDQ401-NYCKEL                                    
081900       PERFORM IMS-GET-ORQF01-KVAL                                        
082000       PERFORM FAC-LAS-OHUV-ARBTAB                                        
082100       PERFORM FAD-REDIGERA-ORDERRAD                                      
082200       ADD     +1 TO INDX                                                 
082300       PERFORM IMS-GET-ORQH01-OKVAL                                       
082400     END-PERFORM                                                          
082500                                                                          
082600     IF INDX > MAX-RAD AND                                                
082700        SEGMENT-FINNS                                                     
082800       PERFORM FAE-SPARA-WDQ4-NYCKLAR-NEXT                                
082900     END-IF                                                               
083000     .                                                                    
083100     EJECT                                                                
083200 FAA-SKAPA-WDQ4-NYCKLAR-ENTER   SECTION.                                  
083300                                                                          
083400     IF SEGMENT-FINNS                                                     
083500         MOVE SEQB-IDLOPNR     TO MOD-IDRADNR-ENTER                       
083600         MOVE SEQB-IDKUNDNR    TO MOD-IDKUNDNR-ENTER                      
083700         MOVE SEQB-IDDISTR     TO MOD-IDDISTR-ENTER                       
083800         MOVE SEQB-IDORDNR7    TO MOD-IDORDNR7-ENTER                      
083900         MOVE SEQB-IDORDER     TO MOD-IDORDER-ENTER                       
084000         MOVE SEQB-IDDC        TO MOD-IDDC-ENTER                          
084100         MOVE SEQB-ADLAGOMR    TO MOD-ADLAGOMR-ENTER                      
084200         MOVE SEQB-ADGANG      TO MOD-ADGANG-ENTER                        
084300         MOVE SEQB-ADPLATS     TO MOD-ADPLATS-ENTER                       
084400         MOVE 'WLORQH '        TO MOD-IDDB-ENTER                          
084500       ELSE                                                               
084600         MOVE ZERO             TO MOD-IDRADNR-ENTER                       
084700                                  MOD-IDKUNDNR-ENTER                      
084800                                  MOD-IDORDNR7-ENTER                      
084900                                  MOD-IDORDER-ENTER                       
085000                                  MOD-IDDC-ENTER                          
085100                                  MOD-ADLAGOMR-ENTER                      
085200                                  MOD-ADGANG-ENTER                        
085300                                  MOD-ADPLATS-ENTER                       
085400         MOVE SPACE            TO MOD-IDDB-ENTER                          
085500     END-IF                                                               
085600     .                                                                    
085700     EJECT                                                                
085800 FAB-SKAPA-WDQ401-NYCKEL   SECTION.                                       
085900                                                                          
086000     MOVE SEQB-IDORDER         TO W-Q401KY-IDORDER                        
086100     MOVE SEQB-IDDC            TO W-Q401KY-IDDC                           
086200     MOVE SEQB-ADLAGOMR        TO W-Q401KY-ADLAGOMR                       
086300     MOVE SEQB-ADGANG          TO W-Q401KY-ADGANG                         
086400     MOVE SEQB-ADPLATS         TO W-Q401KY-ADPLATS                        
086500     MOVE SEQB-IDARTNR         TO W-Q401KY-IDARTNR                        
086600     MOVE SEQB-IDLOPNR         TO W-Q401KY-IDLOPNR                        
086700     .                                                                    
086800     EJECT                                                                
086900 FAC-LAS-OHUV-ARBTAB       SECTION.                                       
087000                                                                          
087100     MOVE ORAD-IDORDER         TO W-IDORDER                               
087200                               IN W-IDORDER-X                             
087300     PERFORM IMS-GET-ORQI01-KVAL                                          
087400     IF SEGMENT-FINNS                                                     
087500        MOVE ORAD-IDDC         IN ORAD-WDQ401                             
087600                               TO WS-IDDC                                 
087700        IF DCS-IDDC NOT = WS-IDDC                                         
087800           MOVE WS-IDDC TO W-IDDC-B6                                      
087900           PERFORM IMS-GU-WDB601                                          
088000        END-IF                                                            
088100        IF DCS-DDC                                                        
088200          MOVE WC-CDC-SE       TO W-IDDC                                  
088300        ELSE                                                              
088400          MOVE WS-IDDC         TO W-IDDC                                  
088500        END-IF                                                            
088600        PERFORM IMS-GET-ORQI12-KVAL                                       
088700     ELSE                                                                 
088800        IF DIST19-SATS                                                    
088900           CONTINUE                                                       
089000        ELSE                                                              
089100           MOVE 'FEL STATUSKOD FRÅN IMS I FAC-LAS-OHUV-ARBTAB'            
089200                               TO FELTEXT                                 
089300           CALL FELLOG                                                    
089400        END-IF                                                            
089500     END-IF                                                               
089600     .                                                                    
089700     EJECT                                                                
089800 FAD-REDIGERA-ORDERRAD     SECTION.                                       
089900                                                                          
090000     IF DIST19-SATS                                                       
090100        CONTINUE                                                          
090200     ELSE                                                                 
090300        IF OHUV-FLKLAR         =  NEJ                                     
090400            MOVE 'E '          TO MOD-KDORDSTA-RAD     (INDX)             
090500         ELSE                                                             
090600            IF ARB-KDTRPKAT    =  'A'                                     
090700                MOVE 'R '      TO MOD-KDORDSTA-RAD     (INDX)             
090800             ELSE                                                         
090900                MOVE ARB-KDTRPKAT TO MOD-KDORDSTA-RAD  (INDX)             
091000            END-IF                                                        
091100        END-IF                                                            
091200     END-IF                                                               
091300     MOVE OHUV-IDDISTR         TO MOD-IDDISTR-RAD      (INDX)             
091400     MOVE OHUV-IDKUNDNR        TO MOD-IDKUNDNR-RAD     (INDX)             
091500     MOVE ORAD-IDORDNR7          IN ORAD-WDQ401                           
091600                               TO MOD-IDORDNR7-RAD     (INDX)             
091700     MOVE ORAD-IDDC              IN ORAD-WDQ401                           
091800                               TO MOD-IDDC-RAD         (INDX)             
091900     MOVE ARB-KDFRAKT          TO MOD-KDFRAKT-RAD      (INDX)             
092000     MOVE OHUV-KDORDKL         TO MOD-KDORDKL-RAD      (INDX)             
092100     MOVE ORAD-KVBEART-Q         IN ORAD-WDQ401                           
092200                               TO MOD-KVBEART-Q-RAD    (INDX)             
092300     MOVE OHUV-TIREGDAT        TO W-TIAAMMDD                              
092400     MOVE W-TIAAMMDD           TO MOD-TIREGDAT-RAD     (INDX)             
092500     MOVE ORAD-IDLEVNR           IN ORAD-WDQ401                           
092600                               TO MOD-IDLEVNR-RAD      (INDX)             
092700     MOVE ORAD-IDKUNDRF-RO       IN ORAD-WDQ401  (1:7)                    
092800                               TO MOD-IDORDNR7-URS-RAD (INDX)             
092900     IF ORAD-IDBIL               IN ORAD-WDQ401                           
093000                                 > SPACE         OR                       
093100        ORAD-IDVIN               IN ORAD-WDQ401                           
093200                                 > SPACE                                  
093300       IF DIST88-VDI OR DIST03-SVERIGE-2                                  
093400         MOVE ORAD-IDBIL         IN ORAD-WDQ401                           
093500                               TO MOD-IDVIN-RAD (INDX)                    
093600       ELSE                                                               
093700         MOVE ORAD-IDVIN         IN ORAD-WDQ401                           
093800                               TO MOD-IDVIN-RAD (INDX)                    
093900       END-IF                                                             
094000     ELSE                                                                 
094100         MOVE ORAD-BERADREF      IN ORAD-WDQ401                           
094200                                 TO MOD-IDVIN-RAD (INDX)                  
094300     END-IF                                                               
094400     .                                                                    
094500     EJECT                                                                
094600 FAE-SPARA-WDQ4-NYCKLAR-NEXT   SECTION.                                   
094700                                                                          
094800     MOVE SEQB-IDLOPNR         TO MOD-IDRADNR-NEXT                        
094900     MOVE SEQB-IDDISTR         TO MOD-IDDISTR-NEXT                        
095000     MOVE SEQB-IDKUNDNR        TO MOD-IDKUNDNR-NEXT                       
095100     MOVE SEQB-IDORDNR7        TO MOD-IDORDNR7-NEXT                       
095200     MOVE SEQB-IDORDER         TO MOD-IDORDER-NEXT                        
095300     MOVE SEQB-IDDC            TO MOD-IDDC-NEXT                           
095400     MOVE SEQB-ADLAGOMR        TO MOD-ADLAGOMR-NEXT                       
095500     MOVE SEQB-ADGANG          TO MOD-ADGANG-NEXT                         
095600     MOVE SEQB-ADPLATS         TO MOD-ADPLATS-NEXT                        
095700     MOVE 'WLORQH '            TO MOD-IDDB-NEXT                           
095800     MOVE INF-MORE-INFO-105    TO MED-IDMFSINF                            
095900     CALL WMEDKONV USING MED-WMEDAREA                                     
096000     MOVE MED-TEMFSINF         TO MOD-TEMFSINF                            
096100     .                                                                    
096200     EJECT                                                                
096300 FB-BEHANDLA-WDE4-ORDER SECTION.                                          
096400                                                                          
096500     IF MFS-FIRST OR (MFS-ENTER AND W-IDDB = 'WLORQH') OR                 
096600                     (MFS-NEXT  AND W-IDDB = 'WLORQH')                    
096700       PERFORM IMS-GET-WDE4C1-OKVAL                                       
096800     ELSE                                                                 
096900       PERFORM IMS-GET-WDE4C1-KVAL                                        
097000     END-IF                                                               
097100                                                                          
097200     IF SEGMENT-FINNS AND INDX =  +1                                      
097300       PERFORM FBA-SKAPA-WDE4-NYCKEL-ENTER                                
097400     END-IF                                                               
097500                                                                          
097600     PERFORM UNTIL SEGMENT-SAKNAS  OR                                     
097700                   SEGMENT-SLUT    OR                                     
097800                   INDX > MAX-RAD                                         
097900                                                                          
098000       PERFORM S01-SKAPA-WDE401-WDE411-NYCKEL                             
098100       PERFORM IMS-GU-WDE401-11                                           
098200                                                                          
098300       IF KORD-IDDC  =  WS-IDDC OR                                        
098400          IDDC-WS = ZERO                                                  
098500         IF KORD-IDORDER  =  SPAR-IDORDER AND                             
098600            KORD-IDDC     =  SPAR-IDDC                                    
098700           CONTINUE                                                       
098800         ELSE                                                             
098900           PERFORM FBC-LAS-OHUV                                           
099000           MOVE KORD-IDORDER TO SPAR-IDORDER                              
099100           MOVE KORD-IDDC    TO SPAR-IDDC                                 
099200         END-IF                                                           
099300         IF SEGMENT-SAKNAS                                                
099400           IF DIST19-SATS                                                 
099500             PERFORM FBD-REDIGERA-PO-RAD                                  
099600           ELSE                                                           
099700             CONTINUE                                                     
099800           END-IF                                                         
099900         ELSE                                                             
100000           PERFORM FBD-REDIGERA-PO-RAD                                    
100100           END-IF                                                         
100200       END-IF                                                             
100300                                                                          
100400       IF FLER-KOLLI-FINNS                                                
100500*            DET FINNS FLER KOLLI SEGMENT OCH SIDAN ÄR FULL               
100600*            DÅ SKALL MAN EJ LÄSA NÄSTA ARTIKEL SEGMENT                   
100700         CONTINUE                                                         
100800       ELSE                                                               
100900         PERFORM IMS-GET-WDE4C1-OKVAL                                     
101000       END-IF                                                             
101100     END-PERFORM                                                          
101200                                                                          
101300     IF INDX > MAX-RAD AND                                                
101400       SEGMENT-FINNS                                                      
101500       IF IDDC-WS = ZERO   OR                                             
101600          FLER-KOLLI-FINNS                                                
101700         MOVE    SEQC-IDPURAD TO WS-IDPURAD                               
101800         PERFORM FBF-SPARA-PACK-ORDER-NYCKLAR                             
101900       ELSE                                                               
102000         PERFORM FBE-KOLLA-IDDC-FINNS                                     
102100         IF IDDC-FINNS                                                    
102200           MOVE    SEQC-IDPURAD       TO W-IDPURAD                        
102300           PERFORM IMS-GNP-WDE411                                         
102400           MOVE    AREA8-ORAD-IDPURAD TO WS-IDPURAD                       
102500           PERFORM FBF-SPARA-PACK-ORDER-NYCKLAR                           
102600         END-IF                                                           
102700       END-IF                                                             
102800     END-IF                                                               
102900     .                                                                    
103000     EJECT                                                                
103100 FBA-SKAPA-WDE4-NYCKEL-ENTER SECTION.                                     
103200                                                                          
103300     IF SEGMENT-FINNS                                                     
103400         MOVE SEQC-IDPRODNR    TO MOD-IDPRODNR-ENTER                      
103500         MOVE SEQC-IDPURAD     TO MOD-IDRADNR-ENTER                       
103600         MOVE 'WDE4C  '        TO MOD-IDDB-ENTER                          
103700      ELSE                                                                
103800         MOVE ZERO             TO MOD-IDPRODNR-ENTER                      
103900                                  MOD-IDRADNR-ENTER                       
104000         MOVE SPACE            TO MOD-IDDB-ENTER                          
104100     END-IF                                                               
104200     .                                                                    
104300     EJECT                                                                
104400 FBC-LAS-OHUV              SECTION.                                       
104500                                                                          
104600     MOVE KORD-IDORDER         TO W-IDORDER                               
104700                               IN W-IDORDER-X                             
104800     PERFORM IMS-GET-ORQI01-KVAL                                          
104900     IF SEGMENT-FINNS                                                     
105000        CONTINUE                                                          
105100     ELSE                                                                 
105200*       IF DIST19-SATS                                                    
105300           CONTINUE                                                       
105400*       ELSE                                                              
105500*          MOVE 'FEL STATUSKOD FRÅN IMS I FBC-LAS-OHUV'                   
105600*                              TO FELTEXT                                 
105700*          CALL FELLOG                                                    
105800*       END-IF                                                            
105900     END-IF                                                               
106000     .                                                                    
106100     EJECT                                                                
106200 FBD-REDIGERA-PO-RAD     SECTION.                                         
106300                                                                          
106400     IF ORAD-KDRADSTA          <  +4  OR                                  
106500        ORAD-KVLEVART          =  ZERO                                    
106600         IF DIST19-SATS                                                   
106700            MOVE KORD-IDDISTR  TO MOD-IDDISTR-RAD   (INDX)                
106800            MOVE KORD-IDKUNDNR TO MOD-IDKUNDNR-RAD  (INDX)                
106900            MOVE KORD-KDFRAKT  TO MOD-KDFRAKT-RAD   (INDX)                
107000            MOVE ORAD-KDORDKL  IN ORAD-WDE411                             
107100                               TO MOD-KDORDKL-RAD   (INDX)                
107200         ELSE                                                             
107300            MOVE OHUV-IDDISTR  TO MOD-IDDISTR-RAD   (INDX)                
107400            MOVE OHUV-IDKUNDNR TO MOD-IDKUNDNR-RAD  (INDX)                
107500            MOVE KORD-KDFRAKT  TO MOD-KDFRAKT-RAD   (INDX)                
107600            MOVE OHUV-KDORDKL  TO MOD-KDORDKL-RAD   (INDX)                
107700         END-IF                                                           
107800                                                                          
107900         MOVE KORD-IDORDNR5    TO MOD-IDORDNR7-RAD  (INDX)                
108000         MOVE KORD-IDDC        TO MOD-IDDC-RAD (INDX)                     
108100         MOVE ORAD-IDLEVNR     IN ORAD-WDE411                             
108200                               TO MOD-IDLEVNR-RAD   (INDX)                
108300         IF ORAD-KVLEVART      = ZERO AND                                 
108400            ORAD-KDRADSTA      > 3                                        
108500             IF ORAD-KVAVBART IN ORAD-WDE411 = ZERO                       
108600* * * INNEBÄR ATT RADEN ÄR NOLLAD VID PACKNINGSRAPPORTERINGEN,            
108700* * * 'N' SOM STATUS SKALL DÅ STÅ FÖR NOLLNINGEN.                         
108800               MOVE 'N'        TO MOD-KDORDSTA-RAD  (INDX)                
108900             ELSE                                                         
109000               MOVE 'P'        TO MOD-KDORDSTA-RAD  (INDX)                
109100             END-IF                                                       
109200          ELSE                                                            
109300             MOVE 'U'          TO MOD-KDORDSTA-RAD  (INDX)                
109400         END-IF                                                           
109500         MOVE ORAD-KVAVBART    IN ORAD-WDE411                             
109600                               TO MOD-KVBEART-Q-RAD (INDX)                
109700         MOVE ORAD-TIUTSKR     TO MOD-TIREGDAT-RAD  (INDX)                
109800                                                                          
109900         MOVE ORAD-IDKUNDRF-RO      IN ORAD-WDE411 (1:5)                  
110000                               TO MOD-IDORDNR7-URS-RAD (INDX)             
110100         IF ORAD-IDBIL           IN ORAD-WDE411                           
110200                                     > SPACE     OR                       
110300            ORAD-IDVIN           IN ORAD-WDE411                           
110400                                     > SPACE                              
110500           IF (DIST88-VDI OR DIST03-SVERIGE-2) AND                        
110600             ORAD-IDSYSTEM IN ORAD-WDE411 = 'VDI '                        
110700             MOVE ORAD-IDBIL     IN ORAD-WDE411                           
110800                                   TO MOD-IDVIN-RAD (INDX)                
110900           ELSE                                                           
111000             MOVE ORAD-IDVIN     IN ORAD-WDE411                           
111100                                   TO MOD-IDVIN-RAD (INDX)                
111200           END-IF                                                         
111300         ELSE                                                             
111400             MOVE ORAD-BERADREF  IN ORAD-WDE411                           
111500                                     TO MOD-IDVIN-RAD (INDX)              
111600         END-IF                                                           
111700         ADD +1                TO INDX                                    
111800     ELSE                                                                 
111900         IF MFS-FIRST OR (MFS-ENTER AND W-IDDB = 'WLORQH')                
112000             PERFORM IMS-GN-WDE401-11-21                                  
112100         ELSE                                                             
112200             IF (MFS-ENTER AND MID-IDKOLLI-ENTER > ZERO AND               
112300                 W-IDDB = 'WDE4C ' AND INDX = +1)           OR            
112400                (MFS-NEXT  AND MID-IDKOLLI-NEXT  > ZERO AND               
112500                 W-IDDB = 'WDE4C ' AND INDX = +1)                         
112600                 PERFORM IMS-GU-WDE401-11-21                              
112700              ELSE                                                        
112800                 PERFORM IMS-GN-WDE401-11-21                              
112900             END-IF                                                       
113000         END-IF                                                           
113100                                                                          
113200         IF SEGMENT-FINNS AND  INDX = +1                                  
113300             MOVE KKOLLI-IDKOLLI TO MOD-IDKOLLI-ENTER                     
113400         END-IF                                                           
113500         MOVE NEJ              TO  FLER-KOLLI-SW                          
113600         PERFORM UNTIL         SEGMENT-SAKNAS  OR                         
113700                               SEGMENT-SLUT    OR                         
113800                               INDX > MAX-RAD                             
113900             IF DIST19-SATS                                               
114000                MOVE KORD-IDDISTR   TO MOD-IDDISTR-RAD   (INDX)           
114100                MOVE KORD-IDKUNDNR  TO MOD-IDKUNDNR-RAD  (INDX)           
114200                MOVE KORD-KDFRAKT   TO MOD-KDFRAKT-RAD   (INDX)           
114300                MOVE KORD-KDORDKL   TO MOD-KDORDKL-RAD   (INDX)           
114400             ELSE                                                         
114500                MOVE OHUV-IDDISTR   TO MOD-IDDISTR-RAD   (INDX)           
114600                MOVE OHUV-IDKUNDNR  TO MOD-IDKUNDNR-RAD  (INDX)           
114700                MOVE KORD-KDFRAKT   TO MOD-KDFRAKT-RAD   (INDX)           
114800                MOVE OHUV-KDORDKL   TO MOD-KDORDKL-RAD   (INDX)           
114900             END-IF                                                       
115000                                                                          
115100             MOVE KORD-IDORDNR5     TO MOD-IDORDNR7-RAD  (INDX)           
115200             MOVE KORD-IDDC         TO MOD-IDDC-RAD      (INDX)           
115300             MOVE ORAD-IDLEVNR      IN ORAD-WDE411                        
115400                                    TO MOD-IDLEVNR-RAD   (INDX)           
115500                                                                          
115600             MOVE ORAD-IDKUNDRF-RO      IN ORAD-WDE411 (1:5)              
115700                                    TO MOD-IDORDNR7-URS-RAD (INDX)        
115800             IF ORAD-IDBIL              IN ORAD-WDE411                    
115900                                        > SPACE         OR                
116000                ORAD-IDVIN              IN ORAD-WDE411                    
116100                                        > SPACE                           
116200               IF (DIST88-VDI OR DIST03-SVERIGE-2) AND                    
116300                 ORAD-IDSYSTEM IN ORAD-WDE411 = 'VDI '                    
116400                 MOVE ORAD-IDBIL        IN ORAD-WDE411                    
116500                                    TO MOD-IDVIN-RAD (INDX)               
116600               ELSE                                                       
116700                 MOVE ORAD-IDVIN        IN ORAD-WDE411                    
116800                                    TO MOD-IDVIN-RAD (INDX)               
116900               END-IF                                                     
117000             ELSE                                                         
117100                 MOVE ORAD-BERADREF IN ORAD-WDE411                        
117200                                         TO MOD-IDVIN-RAD (INDX)          
117300             END-IF                                                       
117400             PERFORM FBDA-BESTAM-STAT                                     
117500                                                                          
117600             ADD +1                 TO INDX                               
117700             PERFORM IMS-GN-WDE401-11-21                                  
117800         END-PERFORM                                                      
117900                                                                          
118000         IF SEGMENT-FINNS  AND                                            
118100            INDX > MAX-RAD                                                
118200             MOVE JA                TO FLER-KOLLI-SW                      
118300             MOVE KKOLLI-IDKOLLI    TO MOD-IDKOLLI-NEXT                   
118400         END-IF                                                           
118500     END-IF                                                               
118600     .                                                                    
118700     EJECT                                                                
118800                                                                          
118900 FBDA-BESTAM-STAT    SECTION.                                             
119000                                                                          
119100                                                                          
119200     MOVE KKOLLI-IDPRODNR TO W-IDPRODNR-WDE611                            
119300     MOVE KKOLLI-IDKOLLI  TO W-IDKOLLI-WDE611                             
119400     PERFORM IMS-GU-WDE611                                                
119500     IF SEGMENT-FINNS                                                     
119600     MOVE KKOLLI-KVLEVART       TO MOD-KVBEART-Q-RAD (INDX)               
119700                                                                          
119800     EVALUATE TRUE                                                        
119900                                                                          
120000     WHEN KOLLI-KDKOLSTA       =  ZERO OR 1                               
120100         MOVE 'P '             TO MOD-KDORDSTA-RAD  (INDX)                
120200         MOVE KOLLI-TIPACKN    TO MOD-TIREGDAT-RAD  (INDX)                
120300                                                                          
120400     WHEN KOLLI-KDKOLSTA       = 2 OR 3                                   
120500         MOVE 'L '             TO MOD-KDORDSTA-RAD (INDX)                 
120600         MOVE KOLLI-TILASTN    TO MOD-TIREGDAT-RAD (INDX)                 
120700                                                                          
120800     WHEN KOLLI-KDKOLSTA       = 4                                        
120900         MOVE 'LF'             TO MOD-KDORDSTA-RAD (INDX)                 
121000         MOVE KOLLI-TILASTN   TO TMP1-YYMMDD                              
121100         MOVE KOLLI-TIFAKT    TO TMP2-YYMMDD                              
121200         PERFORM WY2000P1                                                 
121300         IF TMP1-YYMMDD > TMP2-YYMMDD                                     
121400           MOVE KOLLI-TILASTN  TO MOD-TIREGDAT-RAD (INDX)                 
121500         ELSE                                                             
121600           MOVE KOLLI-TIFAKT   TO MOD-TIREGDAT-RAD (INDX)                 
121700         END-IF                                                           
121800                                                                          
121900     WHEN KOLLI-KDKOLSTA       = 7                                        
122000         MOVE KOLLI-TILASTN    TO MOD-TIREGDAT-RAD (INDX)                 
122100         MOVE 'S '             TO MOD-KDORDSTA-RAD (INDX)                 
122200                                                                          
122300     WHEN KOLLI-KDKOLSTA       = 9                                        
122400         MOVE KOLLI-TIFAKT     TO MOD-TIREGDAT-RAD (INDX)                 
122500         MOVE 'SF'             TO MOD-KDORDSTA-RAD (INDX)                 
122600                                                                          
122700     WHEN KOLLI-KDKOLSTA       = 6 OR 7                                   
122800         MOVE KOLLI-TIFAKT     TO MOD-TIREGDAT-RAD (INDX)                 
122900         MOVE 'F '             TO MOD-KDORDSTA-RAD (INDX)                 
123000                                                                          
123100     WHEN KOLLI-KDKOLSTA       = 8 OR 9                                   
123200         MOVE 'FL'             TO MOD-KDORDSTA-RAD (INDX)                 
123300         MOVE KOLLI-TILASTN   TO TMP1-YYMMDD                              
123400         MOVE KOLLI-TIFAKT    TO TMP2-YYMMDD                              
123500         PERFORM WY2000P1                                                 
123600         IF TMP1-YYMMDD > TMP2-YYMMDD                                     
123700             MOVE KOLLI-TILASTN TO MOD-TIREGDAT-RAD (INDX)                
123800          ELSE                                                            
123900             MOVE KOLLI-TIFAKT TO MOD-TIREGDAT-RAD (INDX)                 
124000         END-IF                                                           
124100     END-EVALUATE                                                         
124110     END-IF                                                               
124200     .                                                                    
124300     EJECT                                                                
124400                                                                          
124500 FBE-KOLLA-IDDC-FINNS SECTION.                                            
124600                                                                          
124700*    OM ALLA IDDC EJ ÄR VALT PÅ BILDEN MÅSTE MAN KOLLA SÅ                 
124800*    ATT DET FINNS NÅGOT WDE411 SEGMENT MED RÄTT IDDC                     
124900*    VID BLÄDDRING.                                                       
125000                                                                          
125100     MOVE    SPACE          TO  W-E401KY-IDKUNDRF                         
125200     MOVE    SEQC-IDDISTR   TO  W-E401KY-IDDISTR                          
125300     MOVE    SEQC-IDKUNDNR  TO  W-E401KY-IDKUNDNR                         
125400     MOVE    SEQC-IDORDNR5  TO  W-E401KY-IDORDNR5                         
125500     MOVE    SEQC-IDPRODNR  TO  W-E401KY-IDPRODNR                         
125600     MOVE    SEQC-IDPLKLST  TO  W-E401KY-IDPLKLST                         
125700     PERFORM IMS-GU-WDE401                                                
125800                                                                          
125900     IF ARE7-KORD-IDDC = WS-IDDC                                          
126000       MOVE JA TO IDDC-SW                                                 
126100     ELSE                                                                 
126200       PERFORM IMS-GET-WDE4C1-OKVAL                                       
126300       PERFORM UNTIL IDDC-FINNS  OR                                       
126400            SEGMENT-SAKNAS OR                                             
126500            SEGMENT-SLUT                                                  
126600           MOVE SPACE         TO  W-E401KY-IDKUNDRF                       
126700           MOVE SEQC-IDDISTR  TO W-E401KY-IDDISTR                         
126800           MOVE SEQC-IDKUNDNR TO W-E401KY-IDKUNDNR                        
126900           MOVE SEQC-IDORDNR5 TO W-E401KY-IDORDNR5                        
127000           MOVE SEQC-IDPRODNR TO W-E401KY-IDPRODNR                        
127100           MOVE SEQC-IDPLKLST TO W-E401KY-IDPLKLST                        
127200           PERFORM IMS-GU-WDE401                                          
127300           IF ARE7-KORD-IDDC = WS-IDDC                                    
127400             MOVE    JA TO  IDDC-SW                                       
127500           ELSE                                                           
127600             PERFORM IMS-GET-WDE4C1-OKVAL                                 
127700           END-IF                                                         
127800       END-PERFORM                                                        
127900     END-IF                                                               
128000     .                                                                    
128100     EJECT                                                                
128200 FBF-SPARA-PACK-ORDER-NYCKLAR SECTION.                                    
128300                                                                          
128400     MOVE SEQC-IDPRODNR        TO MOD-IDPRODNR-NEXT                       
128500     MOVE WS-IDPURAD           TO MOD-IDRADNR-NEXT                        
128600     MOVE 'WDE4C '             TO MOD-IDDB-NEXT                           
128700     MOVE INF-MORE-INFO-105    TO MED-IDMFSINF                            
128800     CALL WMEDKONV USING MED-WMEDAREA                                     
128900     MOVE MED-TEMFSINF         TO MOD-TEMFSINF                            
129000                                                                          
129100     .                                                                    
129200     EJECT                                                                
129300 S01-SKAPA-WDE401-WDE411-NYCKEL   SECTION.                                
129400                                                                          
129500*    MOVE WS-IDDISTR           TO  W-E401KY-IDDISTR                       
129600*    MOVE WS-IDKUNDNR          TO  W-E401KY-IDKUNDNR                      
129700     MOVE SPACE                TO  W-E401KY-IDKUNDRF                      
129800     MOVE SEQC-IDDISTR         TO  W-E401KY-IDDISTR                       
129900     MOVE SEQC-IDKUNDNR        TO  W-E401KY-IDKUNDNR                      
130000     MOVE SEQC-IDORDNR5        TO  W-E401KY-IDORDNR5                      
130100     MOVE SEQC-IDPRODNR        TO  W-E401KY-IDPRODNR                      
130200     MOVE SEQC-IDPLKLST        TO  W-E401KY-IDPLKLST                      
130300     MOVE SEQC-IDPURAD         TO  W-IDPURAD                              
130400     .                                                                    
130500     EJECT                                                                
130600 MFS-RENSA-FAELT-UT SECTION.                                              
130700                                                                          
130800*    --- ALLA UTDATA-FÄLT                                                 
130900*    --- INKL. BLÄDDRINGSNYCKLAR                                          
131000     MOVE MFS-RENSA-FAELT      TO MOD-IDDC-ENTER                          
131100                                  MOD-IDDC-NEXT                           
131200                                  MOD-IDORDER-ENTER                       
131300                                  MOD-IDORDER-NEXT                        
131400                                  MOD-IDDISTR-ENTER                       
131500                                  MOD-IDDISTR-NEXT                        
131600                                  MOD-IDKUNDNR-ENTER                      
131700                                  MOD-IDKUNDNR-NEXT                       
131800                                  MOD-ADLAGOMR-ENTER                      
131900                                  MOD-ADLAGOMR-NEXT                       
132000                                  MOD-ADGANG-ENTER                        
132100                                  MOD-ADGANG-NEXT                         
132200                                  MOD-ADPLATS-ENTER                       
132300                                  MOD-ADPLATS-NEXT                        
132400                                  MOD-IDPRODNR-ENTER                      
132500                                  MOD-IDPRODNR-NEXT                       
132600                                  MOD-IDDB-ENTER                          
132700                                  MOD-IDDB-NEXT                           
132800                                  MOD-IDRADNR-ENTER                       
132900                                  MOD-IDRADNR-NEXT                        
133000                                                                          
133100     MOVE +1                   TO INDX                                    
133200     PERFORM UNTIL INDX        >  MAX-RAD                                 
133300         PERFORM MFS-RENSA-RAD-FAELT-UT                                   
133400         ADD +1                TO INDX                                    
133500     END-PERFORM                                                          
133600     .                                                                    
133700     SKIP2                                                                
133800 MFS-RENSA-RAD-FAELT-UT SECTION.                                          
133900                                                                          
134000*    --- UTDATA-FÄLT PÅ BLÄDDRINGSRADER                                   
134100     MOVE MFS-RENSA-FAELT      TO MOD-IDDISTR-RAD      (INDX)             
134200                                  MOD-IDKUNDNR-RAD     (INDX)             
134300                                  MOD-IDORDNR7-RAD     (INDX)             
134400                                  MOD-IDDC-RAD         (INDX)             
134500                                  MOD-KDFRAKT-RAD      (INDX)             
134600                                  MOD-KDORDKL-RAD      (INDX)             
134700                                  MOD-KDORDSTA-RAD     (INDX)             
134800                                  MOD-KVBEART-Q-RAD    (INDX)             
134900                                  MOD-TIREGDAT-RAD     (INDX)             
135000                                  MOD-IDLEVNR-RAD      (INDX)             
135100                                  MOD-IDORDNR7-URS-RAD (INDX)             
135200                                  MOD-IDVIN-RAD        (INDX)             
135300     .                                                                    
135400     SKIP2                                                                
135500 MFS-RENSA-FAELT-IN SECTION.                                              
135600                                                                          
135700*    --- ALLA INDATA-FÄLT                                                 
135800     MOVE MFS-RENSA-FAELT      TO MOD-IDDISTR-IN                          
135900                                  MOD-IDKUNDNR-IN                         
136000                                  MOD-IDARTNR-IN                          
136100                                  MOD-IDDC-IN                             
136200     .                                                                    
136300     EJECT                                                                
136400* --- IMS SEKTIONER ---                                                   
136500     SKIP3                                                                
136600 IMS-GET-MSG SECTION.                                                     
136700                                                                          
136800     MOVE '  QC' TO GODK-STATUSKODER                                      
136900     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
137000     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
137100     PERFORM IMS-STATUSKONTROLL                                           
137200     .                                                                    
137300     SKIP3                                                                
137400 IMS-INSERT-MSG SECTION.                                                  
137500                                                                          
137600     IF MSGI-IDLAND-SPR NOT = 'GB'                                        
137700       MOVE '0' TO MFS-KDHUVOMR                                           
137800     END-IF                                                               
137900     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
138000     MOVE SPACE TO GODK-STATUSKODER                                       
138100     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
138200     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
138300     PERFORM IMS-STATUSKONTROLL                                           
138400     .                                                                    
138500     EJECT                                                                
138600 IMS-GET-ORQI01-KVAL SECTION.                                             
138700                                                                          
138800     STRING 'WLORQI01(IDORDER  =' W-IDORDER-X ')'                         
138900          DELIMITED BY SIZE INTO SSA1                                     
139000     MOVE '  GE' TO GODK-STATUSKODER                                      
139100     CALL CBLTDLI USING GU ORQI-PCB DLI-IO-AREA1 SSA1                     
139200     MOVE ORQI-STATUS-CODE TO STATUS-WS                                   
139300     PERFORM IMS-STATUSKONTROLL                                           
139400     .                                                                    
139500     EJECT                                                                
139600 IMS-GET-ORQI12-KVAL     SECTION.                                         
139700                                                                          
139800     STRING 'WLORQI01(IDORDER  =' W-IDORDER-X ')'                         
139900          DELIMITED BY SIZE INTO SSA1                                     
140000     STRING 'WLORQI12(IDDC     =' W-IDDC-X    ')'                         
140100          DELIMITED BY SIZE INTO SSA2                                     
140200     MOVE '  ' TO GODK-STATUSKODER                                        
140300     CALL CBLTDLI USING GU ORQI-PCB DLI-IO-AREA2 SSA1 SSA2                
140400     MOVE ORQI-STATUS-CODE TO STATUS-WS                                   
140500     PERFORM IMS-STATUSKONTROLL                                           
140600     .                                                                    
140700     EJECT                                                                
140800 IMS-GET-ORQF01-KVAL  SECTION.                                            
140900                                                                          
141000     STRING 'WLORQF01(WDQ401KY =' W-WDQ401KY-X ')'                        
141100          DELIMITED BY SIZE INTO SSA1                                     
141200     MOVE '  GE' TO GODK-STATUSKODER                                      
141300     CALL CBLTDLI USING GU ORQF-PCB DLI-IO-AREA3 SSA1                     
141400     MOVE ORQF-STATUS-CODE TO STATUS-WS                                   
141500     PERFORM IMS-STATUSKONTROLL                                           
141600     .                                                                    
141700     EJECT                                                                
141800 IMS-GET-ORQH01-KVAL    SECTION.                                          
141900                                                                          
142000     STRING 'WLORQH01(WDQ4B1KY =' W-WDQ4B1KY-X ')'                        
142100          DELIMITED BY SIZE INTO SSA1                                     
142200     MOVE '  GE' TO GODK-STATUSKODER                                      
142300     CALL CBLTDLI USING GU ORQH-PCB DLI-IO-AREA3 SSA1                     
142400     MOVE ORQH-STATUS-CODE TO STATUS-WS                                   
142500     PERFORM IMS-STATUSKONTROLL                                           
142600     .                                                                    
142700     SKIP3                                                                
142800 IMS-GET-ORQH01-OKVAL   SECTION.                                          
142900                                                                          
143000     STRING 'WLORQH01(WDQ4B1KY>=' W-WDQ4B1KY-MIN-X                        
143100                    '&WDQ4B1KY<=' W-WDQ4B1KY-MAX-X                        
143200                    '&IDGMTREF>=' W-Q4B1KY-MIN-IDGMTREF                   
143300                    '&IDGMTREF<=' W-Q4B1KY-MAX-IDGMTREF                   
143400                    '&IDDC    >=' W-IDDC-MIN-X                            
143500                    '&IDDC    <=' W-IDDC-MAX-X ')'                        
143600          DELIMITED BY SIZE INTO SSA1                                     
143700     MOVE '  GE' TO GODK-STATUSKODER                                      
143800     CALL CBLTDLI USING GN ORQH-PCB DLI-IO-AREA3 SSA1                     
143900     MOVE ORQH-STATUS-CODE TO STATUS-WS                                   
144000     PERFORM IMS-STATUSKONTROLL                                           
144100     .                                                                    
144200     EJECT                                                                
144300 IMS-GET-WDE4C1-KVAL   SECTION.                                           
144400                                                                          
144500     STRING 'WDE4C1  (WDE4C1KY =' W-WDE4C1KY-X ')'                        
144600          DELIMITED BY SIZE INTO SSA1                                     
144700     MOVE '  GE' TO GODK-STATUSKODER                                      
144800     CALL CBLTDLI USING GU WDE4C-PCB DLI-IO-AREA3 SSA1                    
144900     MOVE WDE4C-STATUS-CODE TO STATUS-WS                                  
145000     PERFORM IMS-STATUSKONTROLL                                           
145100     .                                                                    
145200     SKIP3                                                                
145300 IMS-GET-WDE4C1-OKVAL  SECTION.                                           
145400                                                                          
145500     STRING 'WDE4C1  (WDE4C1KY>=' W-WDE4C1KY-MIN-X                        
145600                    '&WDE4C1KY<=' W-WDE4C1KY-MAX-X                        
145700                    '&IDARTNR  =' W-IDARTNR-X                             
145800                    '&IDDISTR  =' W-IDDISTR-X                             
145900                    '&IDKUNDNR>=' W-IDKUNDNR-MIN-X                        
146000                    '&IDKUNDNR<=' W-IDKUNDNR-MAX-X ')'                    
146100          DELIMITED BY SIZE INTO SSA1                                     
146200     MOVE '  GE' TO GODK-STATUSKODER                                      
146300     CALL CBLTDLI USING GN WDE4C-PCB DLI-IO-AREA3 SSA1                    
146400     MOVE WDE4C-STATUS-CODE TO STATUS-WS                                  
146500     PERFORM IMS-STATUSKONTROLL                                           
146600     .                                                                    
146700     EJECT                                                                
146800 IMS-GU-WDE401-11 SECTION.                                                
146900                                                                          
147000     STRING 'WDE401  *D(WDE401KY =' W-WDE4KEY-X ')'                       
147100          DELIMITED BY SIZE INTO SSA1                                     
147200     STRING 'WDE411  (IDPURAD  =' W-IDPURAD-X ')'                         
147300          DELIMITED BY SIZE INTO SSA2                                     
147400     MOVE '    ' TO GODK-STATUSKODER                                      
147500     CALL CBLTDLI USING GU WDE4-PCB DLI-IO-E401-11 SSA1 SSA2              
147600     MOVE WDE4-STATUS-CODE TO STATUS-WS                                   
147700     PERFORM IMS-STATUSKONTROLL                                           
147800     .                                                                    
147900     SKIP2                                                                
148000 IMS-GU-WDE401 SECTION.                                                   
148100                                                                          
148200     STRING 'WDE401  (WDE401KY =' W-WDE4KEY-X ')'                         
148300          DELIMITED BY SIZE INTO SSA1                                     
148400     MOVE '  '   TO GODK-STATUSKODER                                      
148500     CALL CBLTDLI USING GU WDE4-PCB DLI-IO-AREA7 SSA1                     
148600     MOVE WDE4-STATUS-CODE TO STATUS-WS                                   
148700     PERFORM IMS-STATUSKONTROLL                                           
148800     .                                                                    
148900     EJECT                                                                
149000 IMS-GNP-WDE411 SECTION.                                                  
149100                                                                          
149200     STRING 'WDE411  (IDPURAD  =' W-IDPURAD-X ')'                         
149300          DELIMITED BY SIZE INTO SSA1                                     
149400     MOVE '  '   TO GODK-STATUSKODER                                      
149500     CALL CBLTDLI USING GNP WDE4-PCB DLI-IO-AREA8 SSA1                    
149600     MOVE WDE4-STATUS-CODE TO STATUS-WS                                   
149700     PERFORM IMS-STATUSKONTROLL                                           
149800     .                                                                    
149900     EJECT                                                                
150000 IMS-GU-WDE401-11-21      SECTION.                                        
150100                                                                          
150200     STRING 'WDE401  (WDE401KY =' W-WDE4KEY-X ')'                         
150300          DELIMITED BY SIZE INTO SSA1                                     
150400     STRING 'WDE411  (IDPURAD  =' W-IDPURAD-X ')'                         
150500          DELIMITED BY SIZE INTO SSA2                                     
150600     STRING 'WDE421  (WDE421KY =' W-WDE421KY-X ')'                        
150700          DELIMITED BY SIZE INTO SSA3                                     
150800     MOVE '  GE' TO GODK-STATUSKODER                                      
150900     CALL CBLTDLI USING GU  WDE4-PCB DLI-IO-E421 SSA1 SSA2 SSA3           
151000     MOVE WDE4-STATUS-CODE TO STATUS-WS                                   
151100     PERFORM IMS-STATUSKONTROLL                                           
151200     .                                                                    
151300     EJECT                                                                
151400 IMS-GN-WDE401-11-21      SECTION.                                        
151500     STRING 'WDE401  (WDE401KY =' W-WDE4KEY-X ')'                         
151600          DELIMITED BY SIZE INTO SSA1                                     
151700     STRING 'WDE411  (IDPURAD  =' W-IDPURAD-X ')'                         
151800          DELIMITED BY SIZE INTO SSA2                                     
151900     MOVE  'WDE421  '          TO SSA3                                    
152000     MOVE '  GEGB' TO GODK-STATUSKODER                                    
152100     CALL CBLTDLI USING GN  WDE4-PCB DLI-IO-E421 SSA1 SSA2 SSA3           
152200     MOVE WDE4-STATUS-CODE TO STATUS-WS                                   
152300     PERFORM IMS-STATUSKONTROLL                                           
152400     .                                                                    
152500     EJECT                                                                
152600 IMS-GU-WDE611 SECTION.                                                   
152700                                                                          
152800       STRING 'WDE601  (IDPRODNR =' W-IDPRODNR-X ')'                      
152900            DELIMITED BY SIZE INTO SSA1                                   
153000       STRING 'WDE611  (IDKOLLI  =' W-IDKOLLI-X ')'                       
153100            DELIMITED BY SIZE INTO SSA2                                   
153200       MOVE 'GE  ' TO GODK-STATUSKODER                                    
153300       CALL CBLTDLI USING GU WDE6-PCB DLI-IOAREA-WDE611 SSA1 SSA2         
153400       MOVE WDE6-STATUS-CODE TO STATUS-WS                                 
153500       PERFORM IMS-STATUSKONTROLL                                         
153600         .                                                                
153700     EJECT                                                                
153800 IMS-GU-WDB601    SECTION.                                                
153900     STRING 'WDB601  (IDDC     =' W-IDDC-B6-X ')'                         
154000          DELIMITED BY SIZE INTO SSA1                                     
154100     MOVE '  GE' TO GODK-STATUSKODER                                      
154200     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-AREA-B601 SSA1                 
154300     MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
154400     PERFORM IMS-STATUSKONTROLL                                           
154500     IF SEGMENT-SAKNAS                                                    
154600         MOVE SPACE TO DCS-KDDC                                           
154700     END-IF                                                               
154800     .                                                                    
154900 IMS-STATUSKONTROLL SECTION.                                              
155000                                                                          
155100     SET STATUS-IX TO 1                                                   
155200     SEARCH GODK-STATUS                                                   
155300       AT END                                                             
155400         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
155500         DELIMITED BY SIZE INTO FELTEXT                                   
155600         CALL FELLOG                                                      
155700       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS CONTINUE                  
155800     END-SEARCH                                                           
155900     .                                                                    
156000     EJECT                                                                
156100*    -COPY WY2000P1                                                       
