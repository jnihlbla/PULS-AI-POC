000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     WL014400.                                                
000300 AUTHOR.         SUBBARAO PARUCHURI V.                                    
000400 DATE-WRITTEN.   04/07/15.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*    NAME:       'CARPARTS.LDC.ORDERQUERYPART2'                           
000800*                                                                         
000900*    FUNCTION:                                                            
001000*        FUNKTION:                                                        
001100*        PROGRAMMET HANTERAR BILDEN;                                      
001200*        FRÅGA PÅ DISTRIKT - ARTIKEL                                      
001300*        PROGRAMMET LÄSER      WLORQI (WDQ2)                              
001400*        PROGRAMMET LÄSER      WLORQF (WDQ4)                              
001500*        PROGRAMMET LÄSER      WLORQH (WDQ4)                              
001600*        PROGRAMMET LÄSER      WDE4C                                      
001700*        PROGRAMMET LÄSER      WDE6                                       
001800*                                                                         
001900*        WL014400 PROGRAM IS A REPLICA OF W4050900 PROGRAM                
002000*        AND CUSTOMIZED FOR WEB-LDC REQUIREMENTS                          
002100*                                                                         
002200*    INDATA.                                                              
002300*        TRANSACTION: WL0144T                                             
002400*        REQUEST:     WL0144I1                                            
002500*                                                                         
002600*    OUTDATA.                                                             
002700*        RESPONSE:    WL0144O1                                            
002800                                                                          
002900     SKIP3                                                                
003000 ENVIRONMENT DIVISION.                                                    
003100     SKIP2                                                                
003200 INPUT-OUTPUT SECTION.                                                    
003300                                                                          
003400 FILE-CONTROL.                                                            
003500     EJECT                                                                
003600 DATA DIVISION.                                                           
003700     SKIP3                                                                
003800 FILE SECTION.                                                            
003900     EJECT                                                                
004000 WORKING-STORAGE SECTION.                                                 
004100*    -- CHECKED BY WY2000                                                 
004200     SKIP3                                                                
004300*    -COPY WY2000W1                                                       
004400     SKIP3                                                                
004500 77  IDPGM                       PIC X(08)   VALUE 'WL014400'.            
004600                                                                          
004700*    --- WORK FIELDS FOR ERROR MESSAGES WHEN CALLING ABEND/FELLOG         
004800 77  ERROR-TEXT                  PIC X(80) VALUE SPACE.                   
004900 77  KDRC-DISPLAY                PIC Z(5).                                
005000*    --- ARBETSFÄLT FÖR FELMEDDELANDEN VID CALL ABEND/FELLOG              
005100 77  FELTEXT                     PIC X(80) VALUE SPACE.                   
005200                                                                          
005300 77  JA                          PIC X       VALUE 'J'.                   
005400 77  NEJ                         PIC X       VALUE 'N'.                   
005500                                                                          
005600*    -COPY WWDCKONS                                                       
005700                                                                          
005800*    --- INDEX FÖR BLÄDDRINGSRADER                                        
005900 77  INDX                        PIC S9(4)  VALUE +0    COMP SYNC.        
006000 77  MAX-RAD                     PIC S9(4)  VALUE +500  COMP SYNC.        
006100 77  SPRAK-IX                    PIC S9(9)  VALUE +0    COMP SYNC.        
006200                                                                          
006300*    --- ARBETSFÄLT FÖR AKTUELLA NYCKELVÄRDEN FRÅN SKÄRMEN                
006400 77  IDDC-WS                     PIC X(2)    VALUE SPACE.                 
006500 77  WS-IDORDNR7                 PIC X(7)    VALUE SPACE.                 
006600 77  WS-IDKOLLI                  PIC X(5)    VALUE SPACE.                 
006700 77  WS-IDPRODNR                 PIC X(7)    VALUE SPACE.                 
006800 77  WS-IDELMT-ERROR             PIC X(16).                               
006900 77  WS-IDMSG-ERROR              PIC X(03).                               
007000 77  WS-IDMSG-INFO               PIC X(03).                               
007100                                                                          
007200 01  W-SPAR-IDKUNDRF.                                                     
007300     03  W-SPAR-IDORDNR7         PIC X(7)    VALUE '+++++++'.             
007400     03  FILLER                  PIC X(3)    VALUE '+++'.                 
007500                                                                          
007600 77  W-IDDB                      PIC X(6)    VALUE SPACE.                 
007700 77  W-TIAAMMDD                  PIC 9(6)    VALUE ZERO.                  
007800 77  WS-IDPURAD                  PIC S9(5)   VALUE ZERO  COMP-3.          
007900                                                                          
008000 77  NYCKLAR-SW                  PIC X       VALUE 'J'.                   
008100     88  NYCKLAR-OK                          VALUE 'J'.                   
008200     88  NYCKLAR-FEL                         VALUE 'N'.                   
008300                                                                          
008400 77  ALLT-SW                     PIC X       VALUE 'J'.                   
008500     88  ALLT-OK                             VALUE 'J'.                   
008600                                                                          
008700 77  IDDC-SW                     PIC X       VALUE 'N'.                   
008800     88  IDDC-FINNS                          VALUE 'J'.                   
008900     88  IDDC-SAKNAS                         VALUE 'N'.                   
009000                                                                          
009100 77  WS-COUNT                    PIC 9(3)    VALUE ZERO.                  
009200 77  FLER-KOLLI-SW               PIC X       VALUE 'N'.                   
009300     88  FLER-KOLLI-FINNS                    VALUE 'J'.                   
009400                                                                          
009500*      --- VALID IDDC CODES                                               
009600*                                                                         
009700*    --- SUBPROGRAMS AND PARAMETER AREAS                                  
009800 01  GENERAL-SUBPROGRAMS.                                                 
009900     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
010000     03  ABEND                   PIC X(8)    VALUE 'ABEND   '.            
010100     03  WZ01SUB                 PIC X(8)    VALUE 'WZ01SUB '.            
010200     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
010300     SKIP3                                                                
010400*    --- PARAMETERS TO ABEND                                              
010500                                                                          
010600 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
010700 77  RKOD-ABEND-NO-DUMP          PIC S9(4)   COMP VALUE +16.              
010800 77  RKOD-ABEND-WITH-DUMP        PIC S9(4)   COMP VALUE +1000.            
010900     SKIP3                                                                
011000 01  MESSAGE-CODES.                                                       
011100     03  ERR-WRONG-KEY           PIC X(3)    VALUE '043'.                 
011200     EJECT                                                                
011300*                                                                         
011400 01  FILLER                      PIC X(16)   VALUE 'SUB-CONTROL'.         
011500     SKIP3                                                                
011600*01  -COPY WZ01SUB                                                        
011700     EJECT                                                                
011800 01  FILLER                      PIC X(16)   VALUE 'REQU-AREA'.           
011900     SKIP3                                                                
012000 01  REQU-AREA.                                                           
012100*    03  -COPY WZ01REQU                                                   
012200*    03  -COPY WL0144I1                                                   
012300     EJECT                                                                
012400 01  FILLER                      PIC X(16)   VALUE 'RESP-AREA'.           
012500     SKIP3                                                                
012600 01  RESP-AREA.                                                           
012700*    03  -COPY WZ01RESP                                                   
012800*    03  -COPY WL0144O1                                                   
012900     EJECT                                                                
013000 01  HELP-AREOR.                                                          
013100*                                                                         
013200     03  FILLER            PIC X(16)         VALUE 'HELP-AREOR'.          
013300     SKIP2                                                                
013400 01  SPAR-AREOR.                                                          
013500*                                                                         
013600     03  FILLER            PIC X(16)         VALUE 'SPAR-AREOR'.          
013700     03  SPAR-IDORDER      PIC S9(7)  COMP-3 VALUE +0.                    
013800     03  SPAR-IDDC         PIC  X(2)         VALUE SPACE.                 
013900     EJECT                                                                
014000*    --- PARAMETRAR TILL SUBPROGRAM WMEDKONV                              
014100*   -COPY WMEDAREA                                                        
014200     SKIP3                                                                
014300 01  MESSAGE-CODES.                                                       
014400     03  ERR-WRONG-KEY-401       PIC X(3)    VALUE '043'.                 
014500     03  SYSTEM-ERROR            PIC X(3)    VALUE '099'.                 
014600     03  FEL-TO-MANY             PIC X(3)    VALUE '028'.                 
014700     EJECT                                                                
014800*    --- DISTRIKTSCOPYTEXTER                                              
014900 01  TEST-IDDISTR              PIC S9(5) COMP-3.                          
015000*01  FILLER -COPY WWDIST03 -RED TEST-IDDISTR.                             
015100     EJECT                                                                
015200*01  FILLER -COPY WWDIST19 -RED TEST-IDDISTR.                             
015300     EJECT                                                                
015400*01  FILLER -COPY WWDIST88 -RED TEST-IDDISTR.                             
015500     EJECT                                                                
015600*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
015700*                                                                         
015800 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
015900     SKIP3                                                                
016000 01  NYCKLAR-TILL-DLI.                                                    
016100     SKIP2                                                                
016200   03    W-WDQ401KY-X.                                                    
016300     05    W-Q401KY-IDORDER      PIC S9(7)   VALUE ZERO  COMP-3.          
016400     05    W-Q401KY-IDDC         PIC  X(2)   VALUE SPACE.                 
016500     05    W-Q401KY-ADLAGOMR     PIC S9(3)   VALUE ZERO  COMP-3.          
016600     05    W-Q401KY-ADGANG       PIC S9(3)   VALUE ZERO  COMP-3.          
016700     05    W-Q401KY-ADPLATS      PIC S9(5)   VALUE ZERO  COMP-3.          
016800     05    W-Q401KY-IDARTNR      PIC S9(9)   VALUE ZERO  COMP-3.          
016900     05    W-Q401KY-IDLOPNR      PIC S9(3)   VALUE ZERO  COMP-3.          
017000     SKIP2                                                                
017100   03    W-WDQ4B1KY-X.                                                    
017200     05    W-Q4B1KY-IDARTNR      PIC S9(9)   VALUE ZERO  COMP-3.          
017300     05    W-Q4B1KY-IDLOPNR      PIC S9(3)   VALUE ZERO  COMP-3.          
017400     05    W-Q4B1KY-IDGMTREF.                                             
017500       07  W-Q4B1KY-IDDISTR      PIC S9(5)   VALUE ZERO  COMP-3.          
017600       07  W-Q4B1KY-IDKUNDNR     PIC S9(7)   VALUE ZERO  COMP-3.          
017700       07  W-Q4B1KY-IDKUNDRF.                                             
017800        09 W-Q4B1KY-IDORDNR7     PIC 9(07)   VALUE ZERO.                  
017900        09 FILLER                PIC X(03)   VALUE SPACE.                 
018000     05    W-Q4B1KY-IDORDER      PIC S9(7)   VALUE ZERO  COMP-3.          
018100     05    W-Q4B1KY-IDDC         PIC  X(2)   VALUE SPACE.                 
018200     05    W-Q4B1KY-ADLAGOMR     PIC S9(3)   VALUE ZERO  COMP-3.          
018300     05    W-Q4B1KY-ADGANG       PIC S9(3)   VALUE ZERO  COMP-3.          
018400     05    W-Q4B1KY-ADPLATS      PIC S9(5)   VALUE ZERO  COMP-3.          
018500     SKIP2                                                                
018600   03    W-WDQ4B1KY-MIN-X.                                                
018700     05    W-Q4B1KY-MIN-IDARTNR  PIC S9(9)   VALUE ZERO  COMP-3.          
018800     05    W-Q4B1KY-MIN-IDLOPNR  PIC S9(3)   VALUE ZERO  COMP-3.          
018900     05    W-Q4B1KY-MIN-IDGMTREF.                                         
019000       07  W-Q4B1KY-MIN-IDDISTR  PIC S9(5)   VALUE ZERO  COMP-3.          
019100       07  W-Q4B1KY-MIN-IDKUNDNR PIC S9(7)   VALUE ZERO  COMP-3.          
019200       07  W-Q4B1KY-MIN-IDKUNDRF.                                         
019300        09 W-Q4B1KY-MIN-IDORDNR7 PIC 9(07)   VALUE ZERO.                  
019400        09 FILLER                PIC X(03)   VALUE SPACE.                 
019500     05    W-Q4B1KY-MIN-IDORDER  PIC S9(7)   VALUE ZERO  COMP-3.          
019600     05    W-Q4B1KY-MIN-IDDC     PIC  X(2)   VALUE SPACE.                 
019700     05    W-Q4B1KY-MIN-ADLAGOMR PIC S9(3)   VALUE ZERO  COMP-3.          
019800     05    W-Q4B1KY-MIN-ADGANG   PIC S9(3)   VALUE ZERO  COMP-3.          
019900     05    W-Q4B1KY-MIN-ADPLATS  PIC S9(5)   VALUE ZERO  COMP-3.          
020000     SKIP2                                                                
020100   03    W-WDQ4B1KY-MAX-X.                                                
020200     05    W-Q4B1KY-MAX-IDARTNR  PIC S9(9)   VALUE ZERO  COMP-3.          
020300     05    W-Q4B1KY-MAX-IDLOPNR  PIC S9(3)   VALUE ZERO  COMP-3.          
020400     05    W-Q4B1KY-MAX-IDGMTREF.                                         
020500       07  W-Q4B1KY-MAX-IDDISTR  PIC S9(5)   VALUE ZERO  COMP-3.          
020600       07  W-Q4B1KY-MAX-IDKUNDNR PIC S9(7)   VALUE ZERO  COMP-3.          
020700       07  W-Q4B1KY-MAX-IDKUNDRF.                                         
020800        09 W-Q4B1KY-MAX-IDORDNR7 PIC 9(07)   VALUE ZERO.                  
020900        09 FILLER                PIC X(03)   VALUE SPACE.                 
021000     05    W-Q4B1KY-MAX-IDORDER  PIC S9(7)   VALUE ZERO  COMP-3.          
021100     05    W-Q4B1KY-MAX-IDDC     PIC  X(2)   VALUE SPACE.                 
021200     05    W-Q4B1KY-MAX-ADLAGOMR PIC S9(3)   VALUE ZERO  COMP-3.          
021300     05    W-Q4B1KY-MAX-ADGANG   PIC S9(3)   VALUE ZERO  COMP-3.          
021400     05    W-Q4B1KY-MAX-ADPLATS  PIC S9(5)   VALUE ZERO  COMP-3.          
021500     SKIP2                                                                
021600   03    W-WDE4KEY-X.                                                     
021700     05    W-E401KY-IDGMTREF.                                             
021800       07  W-E401KY-IDDISTR      PIC S9(5)   VALUE ZERO  COMP-3.          
021900       07  W-E401KY-IDKUNDNR     PIC S9(7)   VALUE ZERO  COMP-3.          
022000       07  W-E401KY-IDKUNDRF.                                             
022100        09 W-E401KY-IDORDNR5     PIC 9(05)   VALUE ZERO.                  
022200        09 FILLER                PIC X(05)   VALUE SPACE.                 
022300     05    W-E401KY-IDPRODNR     PIC S9(7)   VALUE ZERO  COMP-3.          
022400     05    W-E401KY-IDPLKLST     PIC S9(3)   VALUE ZERO  COMP-3.          
022500     SKIP2                                                                
022600   03    W-WDE4C1KY-X.                                                    
022700     05    W-E4C1KY-IDARTNR      PIC S9(9)   VALUE ZERO  COMP-3.          
022800     05    W-E4C1KY-IDPRODNR     PIC S9(7)   VALUE ZERO  COMP-3.          
022900     05    W-E4C1KY-IDPURAD      PIC S9(5)   VALUE ZERO  COMP-3.          
023000     SKIP2                                                                
023100   03    W-WDE4C1KY-MIN-X.                                                
023200     05    W-E4C1KY-MIN-IDARTNR  PIC S9(9)   VALUE ZERO  COMP-3.          
023300     05    W-E4C1KY-MIN-IDPRODNR PIC S9(7)   VALUE ZERO  COMP-3.          
023400     05    W-E4C1KY-MIN-IDPURAD  PIC S9(5)   VALUE ZERO  COMP-3.          
023500     SKIP2                                                                
023600   03    W-WDE4C1KY-MAX-X.                                                
023700     05    W-E4C1KY-MAX-IDARTNR  PIC S9(9)   VALUE ZERO  COMP-3.          
023800     05    W-E4C1KY-MAX-IDPRODNR PIC S9(7)   VALUE ZERO  COMP-3.          
023900     05    W-E4C1KY-MAX-IDPURAD  PIC S9(5)   VALUE ZERO  COMP-3.          
024000     SKIP2                                                                
024100   03    W-WDE421KY-X.                                                    
024200     05    W-E421KY-IDPRODNR     PIC S9(7)   VALUE ZERO  COMP-3.          
024300     05    W-E421KY-IDKOLLI      PIC S9(5)   VALUE ZERO  COMP-3.          
024400     SKIP2                                                                
024500   03 W-IDPRODNR-X.                                                       
024600       05  W-IDPRODNR-WDE611     PIC S9(7)   VALUE ZERO  COMP-3.          
024700   03 W-IDKOLLI-X.                                                        
024800       05  W-IDKOLLI-WDE611      PIC S9(5)   VALUE ZERO  COMP-3.          
024900     SKIP2                                                                
025000   03    W-IDORDER-X.                                                     
025100     05    W-IDORDER             PIC S9(7)   VALUE ZERO  COMP-3.          
025200     SKIP2                                                                
025300   03    W-IDARTNR-X.                                                     
025400     05    W-IDARTNR             PIC S9(9)   VALUE ZERO  COMP-3.          
025500     SKIP2                                                                
025600   03    W-IDDISTR-X.                                                     
025700     05    W-IDDISTR             PIC S9(5)   VALUE ZERO  COMP-3.          
025800     SKIP2                                                                
025900   03    W-IDKUNDNR-MIN-X.                                                
026000     05    W-IDKUNDNR-MIN        PIC S9(7)   VALUE ZERO  COMP-3.          
026100     SKIP2                                                                
026200   03    W-IDKUNDNR-MAX-X.                                                
026300     05    W-IDKUNDNR-MAX        PIC S9(7)   VALUE ZERO  COMP-3.          
026400     SKIP2                                                                
026500   03    W-IDDC-MIN-X.                                                    
026600     05    W-IDDC-MIN            PIC  X(2)   VALUE SPACE.                 
026700     SKIP2                                                                
026800   03    W-IDDC-MAX-X.                                                    
026900     05    W-IDDC-MAX            PIC  X(2)   VALUE SPACE.                 
027000     SKIP2                                                                
027100   03    W-IDDC-X.                                                        
027200     05    W-IDDC                PIC  X(2)   VALUE SPACE.                 
027300     SKIP2                                                                
027400   03    W-IDPURAD-X.                                                     
027500     05    W-IDPURAD             PIC S9(5)   VALUE ZERO  COMP-3.          
027600     SKIP2                                                                
027700                                                                          
027800   03  W-IDDC-B6-X.                                                       
027900       05 W-IDDC-B6                  PIC X(2).                            
028000*    --- STATUS-KOD FRÅN IMS                                              
028100 01  STATUS-WS                   PIC XX.                                  
028200     88  SEGMENT-FINNS                       VALUE '  '.                  
028300     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
028400     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
028500     88  SEGMENT-SLUT                        VALUE 'GB'.                  
028600     SKIP2                                                                
028700 01  GODK-STATUSKODER.                                                    
028800     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
028900     SKIP3                                                                
029000 01  SSA1                        PIC X(192).                              
029100 01  SSA2                        PIC X(64).                               
029200 01  SSA3                        PIC X(64).                               
029300 01  SSA4                        PIC X(64).                               
029400     EJECT                                                                
029500*    --- IMS FUNKTIONSKODER                                               
029600*01  -COPY W0003                                                          
029700     EJECT                                                                
029800*    ---  DLI INPUT-OUTPUT AREA                                           
029900 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA'.         
030000     SKIP3                                                                
030100 01  DLI-IO-AREA1.                                                        
030200*        05  -COPY WDQ201                                                 
030300     EJECT                                                                
030400 01  DLI-IO-AREA2.                                                        
030500*    03  -COPY WDQ212                                                     
030600     EJECT                                                                
030700 01  DLI-IO-AREA3.                                                        
030800     03  IO-AREA3                PIC X(3000)  VALUE SPACE.                
030900     SKIP3                                                                
031000     03  WLORQF01 REDEFINES IO-AREA3.                                     
031100*        05  -COPY WDQ401                                                 
031200     EJECT                                                                
031300     03  WLORQH01 REDEFINES IO-AREA3.                                     
031400*        05  -COPY WDQ4B1                                                 
031500     EJECT                                                                
031600     03  WDE4C1   REDEFINES IO-AREA3.                                     
031700*        05  -COPY WDE4C1                                                 
031800     EJECT                                                                
031900 01  FILLER                  PIC X(16)   VALUE 'DLI-IO-E421'.             
032000 01  DLI-IO-E421.                                                         
032100*    03  -COPY WDE421                                                     
032200     EJECT                                                                
032300 01  FILLER                  PIC X(16)   VALUE 'DLI-IO-E401-11'.          
032400 01  DLI-IO-E401-11.                                                      
032500     SKIP3                                                                
032600*    03      -COPY WDE401                                                 
032700     EJECT                                                                
032800*    03      -COPY WDE411                                                 
032900     EJECT                                                                
033000 01  DLI-IO-AREA7.                                                        
033100*    03  -COPY WDE401 -PRE ARE7-                                          
033200     EJECT                                                                
033300 01  DLI-IO-AREA8.                                                        
033400*        05  -COPY WDE411 -PRE AREA8-                                     
033500 01  DLI-IOAREA-WDE611.                                                   
033600     03     -COPY WDE611                                                  
033700                                                                          
033800 01  FILLER               PIC X(16)   VALUE 'WDB601 AREA'.                
033900 01   DLI-IO-AREA-B601.                                                   
034000*     03  -COPY WDB601                                                    
034100     EJECT                                                                
034200 LINKAGE SECTION.                                                         
034300 01  MSG-PCB                     PIC X.                                   
034400     EJECT                                                                
034500*01  -COPY W0008      -PRE ORQI-                                          
034600     05  FILLER                  PIC X.                                   
034700     EJECT                                                                
034800*01  -COPY W0008      -PRE ORQF-                                          
034900     05  FILLER                  PIC X.                                   
035000     EJECT                                                                
035100*01  -COPY W0008      -PRE ORQH-                                          
035200     05  FILLER                  PIC X.                                   
035300     EJECT                                                                
035400*01  -COPY W0008      -PRE WDE4C-                                         
035500     05  FILLER                  PIC X.                                   
035600     EJECT                                                                
035700*01  -COPY W0008      -PRE WDE4-                                          
035800     05  FILLER                  PIC X.                                   
035900     EJECT                                                                
036000*01  -COPY W0008      -PRE WDE6-                                          
036100     05  FILLER                  PIC X.                                   
036200     EJECT                                                                
036300*01  -COPY W0008      -PRE WDB6-                                          
036400     05  FILLER                  PIC X.                                   
036500     EJECT                                                                
036600 PROCEDURE DIVISION  USING MSG-PCB ORQI-PCB ORQF-PCB                      
036700                          ORQH-PCB WDE4C-PCB WDE4-PCB WDE6-PCB            
036800                          WDB6-PCB.                                       
036900 MAIN SECTION.                                                            
037000     ENTRY 'DLITCBL' USING MSG-PCB ORQI-PCB ORQF-PCB                      
037100                          ORQH-PCB WDE4C-PCB WDE4-PCB WDE6-PCB            
037200                          WDB6-PCB.                                       
037300                                                                          
037400     PERFORM S01-FETCH-REQUEST-ARGUMENT                                   
037510                                                                          
037600     IF SUB-KDRC = 0                                                      
037700      IF  REQU-KDPGMACT  = 'S'                                            
037800       PERFORM A-INIT                                                     
037900       PERFORM B-KOLLA-NYCKLAR                                            
038000       IF NYCKLAR-OK                                                      
038100               PERFORM F-LAES-VISA-INFO                                   
038200               MOVE WS-COUNT          TO RESP-KVRADER                     
038300               IF WS-COUNT <  501                                         
038400                  CONTINUE                                                
038500               ELSE                                                       
038600                  MOVE  FEL-TO-MANY   TO RESP-IDMSG-ERROR                 
038700               END-IF                                                     
038800       END-IF                                                             
038900      ELSE                                                                
039000        MOVE SYSTEM-ERROR               TO RESP-IDMSG-ERROR               
039100      END-IF                                                              
039200       MOVE RESP-IDMSG-INFO    TO WS-IDMSG-INFO                           
039300       MOVE RESP-IDMSG-ERROR   TO WS-IDMSG-ERROR                          
039400       MOVE RESP-IDELMT-ERROR  TO WS-IDELMT-ERROR                         
039500       IF WS-IDMSG-ERROR NOT = SPACE                                      
039600           MOVE ALL '+' TO RESP-AREA                                      
039700           MOVE WS-IDMSG-ERROR   TO RESP-IDMSG-ERROR                      
039800           MOVE WS-IDELMT-ERROR  TO RESP-IDELMT-ERROR                     
039900           MOVE WS-IDMSG-INFO    TO RESP-IDMSG-INFO                       
040000           MOVE 001              TO RESP-IDMSGVER                         
040100           MOVE ZERO             TO RESP-KVRADER                          
040200       END-IF                                                             
040300                                                                          
040400       PERFORM S02-RETURN-RESPONSE                                        
040500     END-IF                                                               
040600                                                                          
040700     MOVE ZERO TO RETURN-CODE                                             
040800     GOBACK                                                               
040900     .                                                                    
041000     EJECT                                                                
041100 A-INIT SECTION.                                                          
041200                                                                          
041300         PERFORM AA-NOLLSTAELL-MID-MOD                                    
041400                                                                          
041500     MOVE NEJ                  TO FLER-KOLLI-SW                           
041600                                                                          
041700     MOVE SPACE                TO W-IDDB                                  
041800     MOVE ZERO                 TO SPAR-IDORDER                            
041900                                  SPAR-IDDC                               
042000     .                                                                    
042100     EJECT                                                                
042200 AA-NOLLSTAELL-MID-MOD      SECTION.                                      
042300                                                                          
042400     MOVE ALL '+'              TO RESP-AREA                               
042500     MOVE SPACE                TO RESP-IDMSG-ERROR                        
042600                                  RESP-IDMSG-INFO                         
042700                                  RESP-IDELMT-ERROR                       
042800     MOVE 001                  TO RESP-IDMSGVER                           
042900     MOVE ZERO                 TO RESP-KVRADER                            
043000     .                                                                    
043100     EJECT                                                                
043200 B-KOLLA-NYCKLAR SECTION.                                                 
043300                                                                          
043400     MOVE JA                   TO NYCKLAR-SW                              
043500                                                                          
043600     MOVE LOW-VALUE            TO W-WDQ4B1KY-MIN-X                        
043700                                  W-WDE4C1KY-MIN-X                        
043800                                  W-IDKUNDNR-MIN-X                        
043900                                  W-IDDC-MIN-X                            
044000                                                                          
044100     MOVE HIGH-VALUE           TO W-WDQ4B1KY-MAX-X                        
044200                                  W-WDE4C1KY-MAX-X                        
044300                                  W-IDKUNDNR-MAX-X                        
044400                                  W-IDDC-MAX-X                            
044500     PERFORM BA-KOLLA-IDDISTR                                             
044600     PERFORM BB-KOLLA-IDKUNDNR                                            
044700     PERFORM BC-KOLLA-IDARTNR                                             
044800     PERFORM BD-KOLLA-IDDC                                                
044900     PERFORM BE-FLYTTA-OVRIGA-NYCKLAR                                     
045000                                                                          
045100     IF REQU-IDDISTR-KEY NUMERIC                                          
045200       MOVE    REQU-IDDISTR-KEY TO RESP-IDDISTR-KEY                       
045300       INSPECT RESP-IDDISTR-KEY  REPLACING LEADING ZERO BY SPACE          
045400     END-IF                                                               
045500                                                                          
045600     IF REQU-IDKUNDNR-KEY  NUMERIC                                        
045700       MOVE    REQU-IDKUNDNR-KEY TO RESP-IDKUNDNR-KEY                     
045800       INSPECT RESP-IDKUNDNR-KEY REPLACING LEADING ZERO BY SPACE          
045900     END-IF                                                               
046000                                                                          
046100     IF REQU-IDARTNR-KEY  NUMERIC                                         
046200       MOVE    REQU-IDARTNR-KEY TO RESP-IDARTNR-KEY                       
046300       INSPECT RESP-IDARTNR-KEY REPLACING LEADING ZERO BY SPACE           
046400     END-IF                                                               
046500                                                                          
046600     IF REQU-IDORDNR7-KEY  NUMERIC OR                                     
046700        REQU-IDORDNR7-KEY = ALL '+'                                       
046800        CONTINUE                                                          
046900     ELSE                                                                 
047000       MOVE NEJ        TO NYCKLAR-SW                                      
047100     END-IF                                                               
047200                                                                          
047300     IF REQU-IDKOLLI-KEY   NUMERIC OR                                     
047400        REQU-IDKOLLI-KEY = ALL '+'                                        
047500        CONTINUE                                                          
047600     ELSE                                                                 
047700       MOVE NEJ        TO NYCKLAR-SW                                      
047800     END-IF                                                               
047900                                                                          
048000     IF REQU-IDPRODNR-KEY  NUMERIC OR                                     
048100        REQU-IDPRODNR-KEY = ALL '+'                                       
048200        CONTINUE                                                          
048300     ELSE                                                                 
048400       MOVE NEJ        TO NYCKLAR-SW                                      
048500     END-IF                                                               
048600                                                                          
048700     IF NYCKLAR-FEL                                                       
048800       MOVE ERR-WRONG-KEY-401  TO RESP-IDMSG-ERROR                        
048900     END-IF                                                               
049000     .                                                                    
049100     EJECT                                                                
049200 BA-KOLLA-IDDISTR     SECTION.                                            
049300                                                                          
049400*    -- KONTROLL AV IDDISTR                                               
049500                                                                          
049600     IF REQU-IDDISTR-KEY NUMERIC AND REQU-IDDISTR-KEY > ZERO              
049700       MOVE REQU-IDDISTR-KEY   TO W-IDDISTR                               
049800                                  W-Q4B1KY-IDDISTR                        
049900                                  W-Q4B1KY-MIN-IDDISTR                    
050000                                  W-Q4B1KY-MAX-IDDISTR                    
050100                                  W-E401KY-IDDISTR                        
050200                                  TEST-IDDISTR                            
050300     ELSE                                                                 
050400       MOVE LOW-VALUE          TO W-Q4B1KY-MIN-IDGMTREF                   
050500       MOVE HIGH-VALUE         TO W-Q4B1KY-MAX-IDGMTREF                   
050600     END-IF                                                               
050700     .                                                                    
050800     EJECT                                                                
050900                                                                          
051000 BB-KOLLA-IDKUNDNR    SECTION.                                            
051100                                                                          
051200*    -- KONTROLL AV IDKUNDNR                                              
051300                                                                          
051400     IF REQU-IDKUNDNR-KEY NUMERIC                                         
051500       IF REQU-IDKUNDNR-KEY      =  ZERO                                  
051600           CONTINUE                                                       
051700        ELSE                                                              
051800           MOVE REQU-IDKUNDNR-KEY TO W-Q4B1KY-MIN-IDKUNDNR                
051900                                  W-Q4B1KY-MAX-IDKUNDNR                   
052000                                  W-E401KY-IDKUNDNR                       
052100                                  W-IDKUNDNR-MIN                          
052200                                  W-IDKUNDNR-MAX                          
052300       END-IF                                                             
052400     END-IF                                                               
052500     .                                                                    
052600     EJECT                                                                
052700 BC-KOLLA-IDARTNR     SECTION.                                            
052800                                                                          
052900*    -- KONTROLL AV IDARTNR                                               
053000     IF REQU-IDARTNR-KEY NUMERIC AND REQU-IDARTNR-KEY > ZERO              
053100       MOVE REQU-IDARTNR-KEY   TO W-IDARTNR                               
053200                                  W-Q401KY-IDARTNR                        
053300                                  W-Q4B1KY-IDARTNR                        
053400                                  W-Q4B1KY-MIN-IDARTNR                    
053500                                  W-Q4B1KY-MAX-IDARTNR                    
053600                                  W-E4C1KY-IDARTNR                        
053700                                  W-E4C1KY-MIN-IDARTNR                    
053800                                  W-E4C1KY-MAX-IDARTNR                    
053900     ELSE                                                                 
054000       MOVE NEJ                TO NYCKLAR-SW                              
054100     END-IF                                                               
054200     .                                                                    
054300     EJECT                                                                
054400 BD-KOLLA-IDDC SECTION.                                                   
054500                                                                          
054600     MOVE REQU-IDDC-KEY   TO IDDC-WS                                      
054700                             W-IDDC                                       
054800                             W-IDDC-B6                                    
054900                             W-Q401KY-IDDC                                
055000                             W-Q4B1KY-IDDC                                
055100                             W-IDDC-MAX                                   
055200                             W-IDDC-MIN                                   
055300                             RESP-IDDC-KEY                                
055400                                                                          
055500     PERFORM IMS-GU-WDB601                                                
055600     .                                                                    
055700     EJECT                                                                
055800 BE-FLYTTA-OVRIGA-NYCKLAR SECTION.                                        
055900                                                                          
056000*    -- FLYTTA IDORDNR                                                    
056100       MOVE REQU-IDORDNR7-KEY  TO WS-IDORDNR7                             
056200       INSPECT WS-IDORDNR7 REPLACING LEADING ZERO BY SPACE                
056300                                                                          
056400     IF WS-IDORDNR7 NUMERIC                                               
056500     MOVE WS-IDORDNR7          TO RESP-IDORDNR7-KEY                       
056600     END-IF                                                               
056700                                                                          
056800*    -- FLYTTA IDKOLLI                                                    
056900                                                                          
057000     MOVE REQU-IDKOLLI-KEY   TO WS-IDKOLLI                                
057100     INSPECT WS-IDKOLLI  REPLACING LEADING ZERO BY SPACE                  
057200                                                                          
057300     IF WS-IDKOLLI  NUMERIC                                               
057400     MOVE WS-IDKOLLI           TO RESP-IDKOLLI-KEY                        
057500     END-IF                                                               
057600                                                                          
057700*    -- FLYTTA IDPRODNR                                                   
057800                                                                          
057900     MOVE REQU-IDPRODNR-KEY  TO WS-IDPRODNR                               
058000     INSPECT WS-IDPRODNR   REPLACING LEADING ZERO BY SPACE                
058100                                                                          
058200     IF WS-IDPRODNR NUMERIC                                               
058300     MOVE WS-IDPRODNR          TO RESP-IDPRODNR-KEY                       
058400     END-IF                                                               
058500                                                                          
058501*    -- FLYTTA FLAGGAN FLALLDC                                            
058502                                                                          
058510     MOVE REQU-FLALLDC         TO RESP-FLALLDC                            
058520                                                                          
058600     .                                                                    
058700     EJECT                                                                
058800 F-LAES-VISA-INFO SECTION.                                                
058900                                                                          
059000     MOVE +1                   TO INDX                                    
059100     MOVE ALL ' '              TO REQU-IDDB-ENTER                         
059200     MOVE  REQU-IDDB-ENTER     TO W-IDDB                                  
059300                                                                          
059400     IF DIST19-SATS                                                       
059500        CONTINUE                                                          
059600     ELSE                                                                 
059700        PERFORM FA-BEHANDLA-ORDERRADKO                                    
059800     END-IF                                                               
059900     IF INDX               >  MAX-RAD AND SEGMENT-FINNS OR                
060000        W-IDDISTR             =  ZERO                                     
060100        CONTINUE                                                          
060200     ELSE                                                                 
060300        PERFORM FB-BEHANDLA-WDE4-ORDER                                    
060400     END-IF                                                               
060500*                                                                         
060600*    EVALUATE TRUE                                                        
060700*    WHEN W-IDDB               =  'WLORQH' OR SPACE                       
060800*        IF DIST19-SATS                                                   
060900*           CONTINUE                                                      
061000*        ELSE                                                             
061100*           PERFORM FA-BEHANDLA-ORDERRADKO                                
061200*        END-IF                                                           
061300*        IF INDX               >  MAX-RAD AND SEGMENT-FINNS OR            
061400*        W-IDDISTR             =  ZERO                                    
061500*            CONTINUE                                                     
061600*         ELSE                                                            
061700*            PERFORM FB-BEHANDLA-WDE4-ORDER                               
061800*        END-IF                                                           
061900*                                                                         
062000*    WHEN W-IDDB               =  'WDE4C '                                
062100*        PERFORM FB-BEHANDLA-WDE4-ORDER                                   
062200*                                                                         
062300*    END-EVALUATE                                                         
062400     .                                                                    
062500     EJECT                                                                
062600 FA-BEHANDLA-ORDERRADKO SECTION.                                          
062700                                                                          
062800     PERFORM IMS-GET-ORQH01-OKVAL                                         
062900     PERFORM FAA-SKAPA-WDQ4-NYCKLAR-ENTER                                 
063000     PERFORM UNTIL SEGMENT-SAKNAS  OR                                     
063100                   SEGMENT-SLUT    OR                                     
063200                   INDX > MAX-RAD                                         
063300       PERFORM FAB-SKAPA-WDQ401-NYCKEL                                    
063400       PERFORM IMS-GET-ORQF01-KVAL                                        
063500       PERFORM FAC-LAS-OHUV-ARBTAB                                        
063600       PERFORM FAD-REDIGERA-ORDERRAD                                      
063700       ADD     +1 TO INDX                                                 
063800       PERFORM IMS-GET-ORQH01-OKVAL                                       
063900     END-PERFORM                                                          
064000                                                                          
064100     IF INDX > MAX-RAD AND                                                
064200        SEGMENT-FINNS                                                     
064300        CONTINUE                                                          
064400     END-IF                                                               
064500     .                                                                    
064600     EJECT                                                                
064700 FAA-SKAPA-WDQ4-NYCKLAR-ENTER   SECTION.                                  
064800                                                                          
064900     IF SEGMENT-FINNS                                                     
065000         MOVE 'WLORQH '        TO RESP-IDDB-ENTER                         
065100       ELSE                                                               
065200         MOVE SPACE            TO RESP-IDDB-ENTER                         
065300     END-IF                                                               
065400     .                                                                    
065500     EJECT                                                                
065600 FAB-SKAPA-WDQ401-NYCKEL   SECTION.                                       
065700                                                                          
065800     MOVE SEQB-IDORDER         TO W-Q401KY-IDORDER                        
065900     MOVE SEQB-IDDC            TO W-Q401KY-IDDC                           
066000     MOVE SEQB-ADLAGOMR        TO W-Q401KY-ADLAGOMR                       
066100     MOVE SEQB-ADGANG          TO W-Q401KY-ADGANG                         
066200     MOVE SEQB-ADPLATS         TO W-Q401KY-ADPLATS                        
066300     MOVE SEQB-IDARTNR         TO W-Q401KY-IDARTNR                        
066400     MOVE SEQB-IDLOPNR         TO W-Q401KY-IDLOPNR                        
066500     .                                                                    
066600     EJECT                                                                
066700 FAC-LAS-OHUV-ARBTAB       SECTION.                                       
066800                                                                          
066900     MOVE ORAD-IDORDER         TO W-IDORDER                               
067000                               IN W-IDORDER-X                             
067100     PERFORM IMS-GET-ORQI01-KVAL                                          
067200     IF SEGMENT-FINNS                                                     
067300        IF ORAD-IDDC NOT = DCS-IDDC                                       
067400           MOVE ORAD-IDDC     IN ORAD-WDQ401                              
067500                              TO W-IDDC-B6                                
067600           PERFORM IMS-GU-WDB601                                          
067700        END-IF                                                            
067800        IF DCS-DDC                                                        
067900           MOVE WC-CDC-SE      TO W-IDDC                                  
068000        ELSE                                                              
068100           MOVE ORAD-IDDC      IN ORAD-WDQ401                             
068200                               TO W-IDDC                                  
068300        END-IF                                                            
068400        PERFORM IMS-GET-ORQI12-KVAL                                       
068500     ELSE                                                                 
068600        IF DIST19-SATS                                                    
068700           CONTINUE                                                       
068800        ELSE                                                              
068900           MOVE 'FEL STATUSKOD FRÅN IMS I FAC-LAS-OHUV-ARBTAB'            
069000                               TO FELTEXT                                 
069100           CALL FELLOG                                                    
069200        END-IF                                                            
069300     END-IF                                                               
069400     .                                                                    
069500     EJECT                                                                
069600 FAD-REDIGERA-ORDERRAD     SECTION.                                       
069700                                                                          
069800     IF DIST19-SATS                                                       
069900        CONTINUE                                                          
070000     ELSE                                                                 
070100        IF OHUV-FLKLAR         =  NEJ                                     
070200            MOVE 'E '          TO RESP-KDORDSTA-RAD    (INDX)             
070300         ELSE                                                             
070400            IF ARB-KDTRPKAT    =  'A'                                     
070500                MOVE 'R '      TO RESP-KDORDSTA-RAD    (INDX)             
070600             ELSE                                                         
070700                MOVE ARB-KDTRPKAT TO RESP-KDORDSTA-RAD (INDX)             
070800            END-IF                                                        
070900        END-IF                                                            
071000     END-IF                                                               
071100     ADD      +1               TO WS-COUNT                                
071200     MOVE OHUV-IDDISTR         TO RESP-IDDISTR-RAD     (INDX)             
071300     MOVE OHUV-IDKUNDNR        TO RESP-IDKUNDNR-RAD    (INDX)             
071400     MOVE ORAD-IDORDNR7          IN ORAD-WDQ401                           
071500                               TO RESP-IDORDNR7-RAD    (INDX)             
071530                                                                          
071600     MOVE ORAD-IDDC              IN ORAD-WDQ401                           
071700                               TO RESP-IDDC-RAD        (INDX)             
071800     MOVE ARB-KDFRAKT          TO RESP-KDFRAKT-RAD     (INDX)             
071900     MOVE OHUV-KDORDKL         TO RESP-KDORDKL-RAD     (INDX)             
072000     MOVE ORAD-KVBEART-Q         IN ORAD-WDQ401                           
072100                               TO RESP-KVBEART-Q-RAD   (INDX)             
072200     MOVE OHUV-TIREGDAT        TO W-TIAAMMDD                              
072300     MOVE W-TIAAMMDD           TO RESP-TIREGDAT-RAD    (INDX)             
072400     MOVE ORAD-IDLEVNR           IN ORAD-WDQ401                           
072500                               TO RESP-IDLEVNR-RAD     (INDX)             
072600     MOVE ORAD-IDKUNDRF-RO       IN ORAD-WDQ401  (1:7)                    
072700                               TO RESP-IDORDNR7-URS-RAD (INDX)            
072800     IF ORAD-IDBIL               IN ORAD-WDQ401                           
072900                                 > SPACE         OR                       
073000        ORAD-IDVIN               IN ORAD-WDQ401                           
073100                                 > SPACE                                  
073200       IF DIST88-VDI OR DIST03-SVERIGE-2                                  
073300         MOVE ORAD-IDBIL         IN ORAD-WDQ401                           
073400                               TO RESP-IDVIN-RAD (INDX)                   
073500       ELSE                                                               
073600         MOVE ORAD-IDVIN         IN ORAD-WDQ401                           
073700                               TO RESP-IDVIN-RAD (INDX)                   
073800       END-IF                                                             
073900     ELSE                                                                 
074000       IF REQU-IDUSER = 'V066269 '                                        
074100         MOVE ORAD-IDSYSTEM      IN ORAD-WDQ401                           
074200                                 TO RESP-IDVIN-RAD (INDX) (1:4)           
074300         MOVE ORAD-BERADREF      IN ORAD-WDQ401                           
074400                                 TO RESP-IDVIN-RAD (INDX) (6:12)          
074500       ELSE                                                               
074600         MOVE ORAD-BERADREF      IN ORAD-WDQ401                           
074700                                 TO RESP-IDVIN-RAD (INDX)                 
074800       END-IF                                                             
074900     END-IF                                                               
075000     .                                                                    
075100     EJECT                                                                
075200 FB-BEHANDLA-WDE4-ORDER SECTION.                                          
075201                                                                          
075300                                                                          
075400     IF W-IDDB = 'WLORQH' OR SPACE                                        
075500       PERFORM IMS-GET-WDE4C1-OKVAL                                       
075600     ELSE                                                                 
075700       PERFORM IMS-GET-WDE4C1-KVAL                                        
075800     END-IF                                                               
075900                                                                          
076000     IF SEGMENT-FINNS AND INDX =  +1                                      
076100       PERFORM FBA-SKAPA-WDE4-NYCKEL-ENTER                                
076200     END-IF                                                               
076300                                                                          
076400     PERFORM UNTIL SEGMENT-SAKNAS  OR                                     
076500                   SEGMENT-SLUT    OR                                     
076600                   INDX > MAX-RAD                                         
076700                                                                          
076800       PERFORM S01-SKAPA-WDE401-WDE411-NYCKEL                             
076900       PERFORM IMS-GU-WDE401-11                                           
077000                                                                          
077010       IF KORD-IDDC = IDDC-WS OR                                          
077020          REQU-FLALLDC = JA                                               
077100         IF KORD-IDORDER = SPAR-IDORDER AND                               
077200            KORD-IDDC =    SPAR-IDDC                                      
077300            CONTINUE                                                      
077400         ELSE                                                             
077500            PERFORM FBC-LAS-OHUV                                          
077600            MOVE KORD-IDORDER TO SPAR-IDORDER                             
077700            MOVE KORD-IDDC TO SPAR-IDDC                                   
077800         END-IF                                                           
077900         IF SEGMENT-SAKNAS                                                
078000            IF DIST19-SATS                                                
078100               PERFORM FBD-REDIGERA-PO-RAD                                
078200            ELSE                                                          
078300               CONTINUE                                                   
078400            END-IF                                                        
078500         ELSE                                                             
078600            PERFORM FBD-REDIGERA-PO-RAD                                   
078700         END-IF                                                           
078800       END-IF                                                             
078810                                                                          
078900       IF FLER-KOLLI-FINNS                                                
079000*            DET FINNS FLER KOLLI SEGMENT OCH SIDAN ÄR FULL               
079100*            DÅ SKALL MAN EJ LÄSA NÄSTA ARTIKEL SEGMENT                   
079200         CONTINUE                                                         
079300       ELSE                                                               
079400         PERFORM IMS-GET-WDE4C1-OKVAL                                     
079500       END-IF                                                             
079600     END-PERFORM                                                          
079700                                                                          
079800     IF INDX > MAX-RAD AND                                                
079900       SEGMENT-FINNS                                                      
080000       IF IDDC-WS = ZERO   OR                                             
080100          FLER-KOLLI-FINNS                                                
080200          MOVE    SEQC-IDPURAD TO WS-IDPURAD                              
080300       ELSE                                                               
080400         PERFORM FBE-KOLLA-IDDC-FINNS                                     
080500         IF IDDC-FINNS                                                    
080600           MOVE    SEQC-IDPURAD       TO W-IDPURAD                        
080700           PERFORM IMS-GNP-WDE411                                         
080800           MOVE    AREA8-ORAD-IDPURAD TO WS-IDPURAD                       
080900         END-IF                                                           
081000       END-IF                                                             
081100     END-IF                                                               
081200     .                                                                    
081300     EJECT                                                                
081400 FBA-SKAPA-WDE4-NYCKEL-ENTER SECTION.                                     
081500                                                                          
081600     IF SEGMENT-FINNS                                                     
081700         MOVE 'WDE4C  '        TO RESP-IDDB-ENTER                         
081800      ELSE                                                                
081900         MOVE SPACE            TO RESP-IDDB-ENTER                         
082000     END-IF                                                               
082100     .                                                                    
082200     EJECT                                                                
082300 FBC-LAS-OHUV              SECTION.                                       
082400                                                                          
082500     MOVE KORD-IDORDER         TO W-IDORDER                               
082600                               IN W-IDORDER-X                             
082700     PERFORM IMS-GET-ORQI01-KVAL                                          
082800     IF SEGMENT-FINNS                                                     
082900        CONTINUE                                                          
083000     ELSE                                                                 
083100           CONTINUE                                                       
083200     END-IF                                                               
083300     .                                                                    
083400     EJECT                                                                
083500 FBD-REDIGERA-PO-RAD     SECTION.                                         
083600                                                                          
083700     IF ORAD-KDRADSTA          <  +4  OR                                  
083800        ORAD-KVLEVART          =  ZERO                                    
083900         IF DIST19-SATS                                                   
084000            MOVE KORD-IDDISTR  TO RESP-IDDISTR-RAD  (INDX)                
084100            MOVE KORD-IDKUNDNR TO RESP-IDKUNDNR-RAD (INDX)                
084200            MOVE KORD-KDFRAKT  TO RESP-KDFRAKT-RAD  (INDX)                
084300            MOVE ORAD-KDORDKL  IN ORAD-WDE411                             
084400                               TO RESP-KDORDKL-RAD  (INDX)                
084500         ELSE                                                             
084600            MOVE OHUV-IDDISTR  TO RESP-IDDISTR-RAD  (INDX)                
084700            MOVE OHUV-IDKUNDNR TO RESP-IDKUNDNR-RAD (INDX)                
084800            MOVE KORD-KDFRAKT  TO RESP-KDFRAKT-RAD  (INDX)                
084900            MOVE OHUV-KDORDKL  TO RESP-KDORDKL-RAD  (INDX)                
085000         END-IF                                                           
085100                                                                          
085200         MOVE KORD-IDORDNR5    TO RESP-IDORDNR7-RAD (INDX)                
085210                                                                          
085300         MOVE KORD-IDDC        TO RESP-IDDC-RAD (INDX)                    
085400         MOVE ORAD-IDLEVNR     IN ORAD-WDE411                             
085500                               TO RESP-IDLEVNR-RAD  (INDX)                
085600         IF ORAD-KVLEVART      = ZERO AND                                 
085700            ORAD-KDRADSTA      > 3                                        
085800             IF ORAD-KVAVBART IN ORAD-WDE411 = ZERO                       
085900* * * INNEBÄR ATT RADEN ÄR NOLLAD VID PACKNINGSRAPPORTERINGEN,            
086000* * * 'N' SOM STATUS SKALL DÅ STÅ FÖR NOLLNINGEN.                         
086100               MOVE 'N'        TO RESP-KDORDSTA-RAD (INDX)                
086200             ELSE                                                         
086300               MOVE 'P'        TO RESP-KDORDSTA-RAD (INDX)                
086400             END-IF                                                       
086500          ELSE                                                            
086600             MOVE 'U'          TO RESP-KDORDSTA-RAD (INDX)                
086700         END-IF                                                           
086800         MOVE ORAD-KVAVBART    IN ORAD-WDE411                             
086900                               TO RESP-KVBEART-Q-RAD (INDX)               
087000         MOVE ORAD-TIUTSKR     TO RESP-TIREGDAT-RAD (INDX)                
087100                                                                          
087200         MOVE ORAD-IDKUNDRF-RO      IN ORAD-WDE411 (1:5)                  
087300                               TO RESP-IDORDNR7-URS-RAD (INDX)            
087400         IF ORAD-IDBIL           IN ORAD-WDE411                           
087500                                     > SPACE     OR                       
087600            ORAD-IDVIN           IN ORAD-WDE411                           
087700                                     > SPACE                              
087800           IF (DIST88-VDI OR DIST03-SVERIGE-2) AND                        
087900             ORAD-IDSYSTEM IN ORAD-WDE411 = 'VDI '                        
088000             MOVE ORAD-IDBIL     IN ORAD-WDE411                           
088100                                   TO RESP-IDVIN-RAD (INDX)               
088200           ELSE                                                           
088300             MOVE ORAD-IDVIN     IN ORAD-WDE411                           
088400                                   TO RESP-IDVIN-RAD (INDX)               
088500           END-IF                                                         
088600         ELSE                                                             
088700           IF REQU-IDUSER = 'V066269 '                                    
088800             MOVE ORAD-IDSYSTEM  IN ORAD-WDE411                           
088900                                    TO RESP-IDVIN-RAD (INDX) (1:4)        
089000             MOVE ORAD-BERADREF  IN ORAD-WDE411                           
089100                                  TO RESP-IDVIN-RAD (INDX) (6:12)         
089200           ELSE                                                           
089300             MOVE ORAD-BERADREF  IN ORAD-WDE411                           
089400                                   TO RESP-IDVIN-RAD (INDX)               
089500           END-IF                                                         
089600         END-IF                                                           
089700         ADD +1                TO INDX                                    
089800         ADD +1                TO WS-COUNT                                
089900     ELSE                                                                 
090000         IF W-IDDB = 'WLORQH'                                             
090100             PERFORM IMS-GN-WDE401-11-21                                  
090200         ELSE                                                             
090300             IF (W-IDDB = 'WDE4C ' AND INDX = +1)                         
090400                 PERFORM IMS-GU-WDE401-11-21                              
090500              ELSE                                                        
090600                 PERFORM IMS-GN-WDE401-11-21                              
090700             END-IF                                                       
090800         END-IF                                                           
090900                                                                          
091000         MOVE NEJ              TO  FLER-KOLLI-SW                          
091100         PERFORM UNTIL         SEGMENT-SAKNAS  OR                         
091200                               SEGMENT-SLUT    OR                         
091300                               INDX > MAX-RAD                             
091400             IF DIST19-SATS                                               
091500                MOVE KORD-IDDISTR   TO RESP-IDDISTR-RAD  (INDX)           
091600                MOVE KORD-IDKUNDNR  TO RESP-IDKUNDNR-RAD (INDX)           
091700                MOVE KORD-KDFRAKT   TO RESP-KDFRAKT-RAD  (INDX)           
091800                MOVE KORD-KDORDKL   TO RESP-KDORDKL-RAD  (INDX)           
091900             ELSE                                                         
092000                MOVE OHUV-IDDISTR   TO RESP-IDDISTR-RAD  (INDX)           
092100                MOVE OHUV-IDKUNDNR  TO RESP-IDKUNDNR-RAD (INDX)           
092200                MOVE KORD-KDFRAKT   TO RESP-KDFRAKT-RAD  (INDX)           
092300                MOVE OHUV-KDORDKL   TO RESP-KDORDKL-RAD  (INDX)           
092400             END-IF                                                       
092500                                                                          
092600             MOVE KORD-IDORDNR5     TO RESP-IDORDNR7-RAD (INDX)           
092630                                                                          
092700             MOVE KORD-IDDC         TO RESP-IDDC-RAD     (INDX)           
092800             MOVE ORAD-IDLEVNR      IN ORAD-WDE411                        
092900                                    TO RESP-IDLEVNR-RAD  (INDX)           
093000                                                                          
093100             MOVE ORAD-IDKUNDRF-RO      IN ORAD-WDE411 (1:5)              
093200                                  TO RESP-IDORDNR7-URS-RAD (INDX)         
093300             IF ORAD-IDBIL              IN ORAD-WDE411                    
093400                                        > SPACE         OR                
093500                ORAD-IDVIN              IN ORAD-WDE411                    
093600                                        > SPACE                           
093700               IF (DIST88-VDI OR DIST03-SVERIGE-2) AND                    
093800                 ORAD-IDSYSTEM IN ORAD-WDE411 = 'VDI '                    
093900                 MOVE ORAD-IDBIL        IN ORAD-WDE411                    
094000                                    TO RESP-IDVIN-RAD (INDX)              
094100               ELSE                                                       
094200                 MOVE ORAD-IDVIN        IN ORAD-WDE411                    
094300                                    TO RESP-IDVIN-RAD (INDX)              
094400               END-IF                                                     
094500             ELSE                                                         
094600               IF REQU-IDUSER = 'V066269 '                                
094700                 MOVE ORAD-IDSYSTEM IN ORAD-WDE411                        
094800                                    TO RESP-IDVIN-RAD (INDX) (1:4)        
094900                 MOVE ORAD-BERADREF IN ORAD-WDE411                        
095000                                 TO RESP-IDVIN-RAD (INDX) (6:12)          
095100               ELSE                                                       
095200                 MOVE ORAD-BERADREF IN ORAD-WDE411                        
095300                                      TO RESP-IDVIN-RAD (INDX)            
095400               END-IF                                                     
095500             END-IF                                                       
095600             PERFORM FBDA-BESTAM-STAT                                     
095700                                                                          
095800             ADD +1                 TO INDX                               
095900             ADD +1                 TO WS-COUNT                           
096000             PERFORM IMS-GN-WDE401-11-21                                  
096100         END-PERFORM                                                      
096200                                                                          
096300         IF SEGMENT-FINNS  AND                                            
096400            INDX > MAX-RAD                                                
096500             MOVE JA                TO FLER-KOLLI-SW                      
096600         END-IF                                                           
096700     END-IF                                                               
096800                                                                          
096900                                                                          
097000     .                                                                    
097100     EJECT                                                                
097200                                                                          
097300 FBDA-BESTAM-STAT    SECTION.                                             
097400                                                                          
097500                                                                          
097600     MOVE KKOLLI-IDPRODNR TO W-IDPRODNR-WDE611                            
097700     MOVE KKOLLI-IDKOLLI  TO W-IDKOLLI-WDE611                             
097800     PERFORM IMS-GU-WDE611                                                
097810     IF SEGMENT-FINNS                                                     
097900     MOVE KKOLLI-KVLEVART       TO RESP-KVBEART-Q-RAD (INDX)              
098000                                                                          
098100     EVALUATE TRUE                                                        
098200                                                                          
098300     WHEN KOLLI-KDKOLSTA       =  ZERO OR 1                               
098400         MOVE 'P '             TO RESP-KDORDSTA-RAD (INDX)                
098500         MOVE KOLLI-TIPACKN    TO RESP-TIREGDAT-RAD (INDX)                
098600                                                                          
098700     WHEN KOLLI-KDKOLSTA       = 2 OR 3                                   
098800         MOVE 'L '             TO RESP-KDORDSTA-RAD (INDX)                
098900         MOVE KOLLI-TILASTN    TO RESP-TIREGDAT-RAD (INDX)                
099000                                                                          
099100     WHEN KOLLI-KDKOLSTA       = 4                                        
099200         MOVE 'LF'             TO RESP-KDORDSTA-RAD (INDX)                
099300         MOVE KOLLI-TILASTN   TO TMP1-YYMMDD                              
099400         MOVE KOLLI-TIFAKT    TO TMP2-YYMMDD                              
099500         PERFORM WY2000P1                                                 
099600         IF TMP1-YYMMDD > TMP2-YYMMDD                                     
099700           MOVE KOLLI-TILASTN  TO RESP-TIREGDAT-RAD (INDX)                
099800         ELSE                                                             
099900           MOVE KOLLI-TIFAKT   TO RESP-TIREGDAT-RAD (INDX)                
100000         END-IF                                                           
100100                                                                          
100200     WHEN KOLLI-KDKOLSTA       = 7                                        
100300         MOVE KOLLI-TILASTN    TO RESP-TIREGDAT-RAD (INDX)                
100400         MOVE 'S '             TO RESP-KDORDSTA-RAD (INDX)                
100500                                                                          
100600     WHEN KOLLI-KDKOLSTA       = 9                                        
100700         MOVE KOLLI-TIFAKT     TO RESP-TIREGDAT-RAD (INDX)                
100800         MOVE 'SF'             TO RESP-KDORDSTA-RAD (INDX)                
100900                                                                          
101000     WHEN KOLLI-KDKOLSTA       = 6 OR 7                                   
101100         MOVE KOLLI-TIFAKT     TO RESP-TIREGDAT-RAD (INDX)                
101200         MOVE 'F '             TO RESP-KDORDSTA-RAD (INDX)                
101300                                                                          
101400     WHEN KOLLI-KDKOLSTA       = 8 OR 9                                   
101500         MOVE 'FL'             TO RESP-KDORDSTA-RAD (INDX)                
101600         MOVE KOLLI-TILASTN   TO TMP1-YYMMDD                              
101700         MOVE KOLLI-TIFAKT    TO TMP2-YYMMDD                              
101800         PERFORM WY2000P1                                                 
101900         IF TMP1-YYMMDD > TMP2-YYMMDD                                     
102000             MOVE KOLLI-TILASTN TO RESP-TIREGDAT-RAD (INDX)               
102100          ELSE                                                            
102200             MOVE KOLLI-TIFAKT TO RESP-TIREGDAT-RAD (INDX)                
102300         END-IF                                                           
102400     END-EVALUATE                                                         
102410     END-IF                                                               
102500     .                                                                    
102600     EJECT                                                                
102700 FBE-KOLLA-IDDC-FINNS SECTION.                                            
102800                                                                          
102900*    OM ALLA IDDC EJ ÄR VALT PÅ BILDEN MÅSTE MAN KOLLA SÅ                 
103000*    ATT DET FINNS NÅGOT WDE411 SEGMENT MED RÄTT IDDC                     
103100*    VID BLÄDDRING.                                                       
103200                                                                          
103300     MOVE    SPACE          TO  W-E401KY-IDKUNDRF                         
103400     MOVE    SEQC-IDDISTR   TO  W-E401KY-IDDISTR                          
103500     MOVE    SEQC-IDKUNDNR  TO  W-E401KY-IDKUNDNR                         
103600     MOVE    SEQC-IDORDNR5  TO  W-E401KY-IDORDNR5                         
103700     MOVE    SEQC-IDPRODNR  TO  W-E401KY-IDPRODNR                         
103800     MOVE    SEQC-IDPLKLST  TO  W-E401KY-IDPLKLST                         
103900     PERFORM IMS-GU-WDE401                                                
104000                                                                          
104100     IF ARE7-KORD-IDDC = DCS-IDDC                                         
104200       MOVE JA TO IDDC-SW                                                 
104300     ELSE                                                                 
104400       PERFORM IMS-GET-WDE4C1-OKVAL                                       
104500       PERFORM UNTIL IDDC-FINNS  OR                                       
104600            SEGMENT-SAKNAS OR                                             
104700            SEGMENT-SLUT                                                  
104800           MOVE SPACE         TO  W-E401KY-IDKUNDRF                       
104900           MOVE SEQC-IDDISTR  TO W-E401KY-IDDISTR                         
105000           MOVE SEQC-IDKUNDNR TO W-E401KY-IDKUNDNR                        
105100           MOVE SEQC-IDORDNR5 TO W-E401KY-IDORDNR5                        
105200           MOVE SEQC-IDPRODNR TO W-E401KY-IDPRODNR                        
105300           MOVE SEQC-IDPLKLST TO W-E401KY-IDPLKLST                        
105400           PERFORM IMS-GU-WDE401                                          
105500           IF ARE7-KORD-IDDC = DCS-IDDC                                   
105600             MOVE    JA TO  IDDC-SW                                       
105700           ELSE                                                           
105800             PERFORM IMS-GET-WDE4C1-OKVAL                                 
105900           END-IF                                                         
106000       END-PERFORM                                                        
106100     END-IF                                                               
106200     .                                                                    
106300     EJECT                                                                
106400                                                                          
106500 S01-SKAPA-WDE401-WDE411-NYCKEL   SECTION.                                
106600                                                                          
106700     MOVE SPACE                TO  W-E401KY-IDKUNDRF                      
106800     MOVE SEQC-IDDISTR         TO  W-E401KY-IDDISTR                       
106900     MOVE SEQC-IDKUNDNR        TO  W-E401KY-IDKUNDNR                      
107000     MOVE SEQC-IDORDNR5        TO  W-E401KY-IDORDNR5                      
107100     MOVE SEQC-IDPRODNR        TO  W-E401KY-IDPRODNR                      
107200     MOVE SEQC-IDPLKLST        TO  W-E401KY-IDPLKLST                      
107300     MOVE SEQC-IDPURAD         TO  W-IDPURAD                              
107400     .                                                                    
107500     EJECT                                                                
107600*    --- DISPATCHER SECTIONS                                              
107700 S01-FETCH-REQUEST-ARGUMENT SECTION.                                      
107800                                                                          
107900     MOVE 'GETARG'               TO SUB-KDFUNC                            
108000     MOVE 'CARPARTS.LDC.ORDERQUERYPART2'     TO SUB-ADDISPABS             
108100     MOVE LENGTH OF REQU-AREA    TO SUB-KVDLEN                            
108200                                                                          
108300     CALL WZ01SUB USING SUB-CONTROL-AREA SUB-KVDLEN REQU-AREA             
108400                                                                          
108500     IF SUB-KDRC > 0                                                      
108600       MOVE SUB-KDRC TO KDRC-DISPLAY                                      
108700       STRING 'WZ01SUB GETARG ERROR RC=' KDRC-DISPLAY                     
108800       DELIMITED BY SIZE INTO ERROR-TEXT                                  
108900       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
109000     END-IF                                                               
109100     .                                                                    
109200     SKIP3                                                                
109300 S02-RETURN-RESPONSE SECTION.                                             
109400                                                                          
109500     MOVE 'RETURN'                   TO SUB-KDFUNC                        
109600     MOVE LENGTH OF RESP-AREA        TO SUB-KVDLEN                        
109700                                                                          
109800     CALL WZ01SUB USING SUB-CONTROL-AREA SUB-KVDLEN RESP-AREA             
109900                                                                          
110000     IF SUB-KDRC > 0                                                      
110100       MOVE SUB-KDRC TO KDRC-DISPLAY                                      
110200       STRING 'WZ01SUB RETURN ERROR RC=' KDRC-DISPLAY                     
110300       DELIMITED BY SIZE INTO ERROR-TEXT                                  
110400       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
110500     END-IF                                                               
110600     .                                                                    
110700     EJECT                                                                
110800* --- IMS SEKTIONER ---                                                   
110900 IMS-GET-ORQI01-KVAL SECTION.                                             
111000                                                                          
111100     STRING 'WLORQI01(IDORDER  =' W-IDORDER-X ')'                         
111200          DELIMITED BY SIZE INTO SSA1                                     
111300     MOVE '  GE' TO GODK-STATUSKODER                                      
111400     CALL CBLTDLI USING GU ORQI-PCB DLI-IO-AREA1 SSA1                     
111500     MOVE ORQI-STATUS-CODE TO STATUS-WS                                   
111600     PERFORM IMS-STATUSKONTROLL                                           
111700     .                                                                    
111800     EJECT                                                                
111900 IMS-GET-ORQI12-KVAL     SECTION.                                         
112000                                                                          
112100     STRING 'WLORQI01(IDORDER  =' W-IDORDER-X ')'                         
112200          DELIMITED BY SIZE INTO SSA1                                     
112300     STRING 'WLORQI12(IDDC     =' W-IDDC-X    ')'                         
112400          DELIMITED BY SIZE INTO SSA2                                     
112500     MOVE '  ' TO GODK-STATUSKODER                                        
112600     CALL CBLTDLI USING GU ORQI-PCB DLI-IO-AREA2 SSA1 SSA2                
112700     MOVE ORQI-STATUS-CODE TO STATUS-WS                                   
112800     PERFORM IMS-STATUSKONTROLL                                           
112900     .                                                                    
113000     EJECT                                                                
113100 IMS-GET-ORQF01-KVAL  SECTION.                                            
113101                                                                          
113200                                                                          
113300     STRING 'WLORQF01(WDQ401KY =' W-WDQ401KY-X ')'                        
113400          DELIMITED BY SIZE INTO SSA1                                     
113500     MOVE '  GE' TO GODK-STATUSKODER                                      
113600     CALL CBLTDLI USING GU ORQF-PCB DLI-IO-AREA3 SSA1                     
113700     MOVE ORQF-STATUS-CODE TO STATUS-WS                                   
113800     PERFORM IMS-STATUSKONTROLL                                           
113900     .                                                                    
114000     EJECT                                                                
114100 IMS-GET-ORQH01-OKVAL   SECTION.                                          
114101                                                                          
114200                                                                          
114300     STRING 'WLORQH01(WDQ4B1KY>=' W-WDQ4B1KY-MIN-X                        
114400                    '&WDQ4B1KY<=' W-WDQ4B1KY-MAX-X                        
114500                    '&IDGMTREF>=' W-Q4B1KY-MIN-IDGMTREF                   
114600                    '&IDGMTREF<=' W-Q4B1KY-MAX-IDGMTREF                   
114700                    '&IDDC    >=' W-IDDC-MIN-X                            
114800                    '&IDDC    <=' W-IDDC-MAX-X ')'                        
114900          DELIMITED BY SIZE INTO SSA1                                     
115000     MOVE '  GE' TO GODK-STATUSKODER                                      
115100     CALL CBLTDLI USING GN ORQH-PCB DLI-IO-AREA3 SSA1                     
115200     MOVE ORQH-STATUS-CODE TO STATUS-WS                                   
115300     PERFORM IMS-STATUSKONTROLL                                           
115400     .                                                                    
115500     EJECT                                                                
115600 IMS-GET-WDE4C1-KVAL   SECTION.                                           
115700                                                                          
115730                                                                          
115800     STRING 'WDE4C1  (WDE4C1KY =' W-WDE4C1KY-X ')'                        
115900          DELIMITED BY SIZE INTO SSA1                                     
116000     MOVE '  GE' TO GODK-STATUSKODER                                      
116100     CALL CBLTDLI USING GU WDE4C-PCB DLI-IO-AREA3 SSA1                    
116200     MOVE WDE4C-STATUS-CODE TO STATUS-WS                                  
116300     PERFORM IMS-STATUSKONTROLL                                           
116400     .                                                                    
116500     SKIP3                                                                
116600 IMS-GET-WDE4C1-OKVAL  SECTION.                                           
116601                                                                          
116800     STRING 'WDE4C1  (WDE4C1KY>=' W-WDE4C1KY-MIN-X                        
116900                    '&WDE4C1KY<=' W-WDE4C1KY-MAX-X                        
117000                    '&IDARTNR  =' W-IDARTNR-X                             
117100                    '&IDDISTR  =' W-IDDISTR-X                             
117200                    '&IDKUNDNR>=' W-IDKUNDNR-MIN-X                        
117300                    '&IDKUNDNR<=' W-IDKUNDNR-MAX-X ')'                    
117400          DELIMITED BY SIZE INTO SSA1                                     
117500     MOVE '  GE' TO GODK-STATUSKODER                                      
117600     CALL CBLTDLI USING GN WDE4C-PCB DLI-IO-AREA3 SSA1                    
117700     MOVE WDE4C-STATUS-CODE TO STATUS-WS                                  
117800     PERFORM IMS-STATUSKONTROLL                                           
117900     .                                                                    
118000     EJECT                                                                
118100 IMS-GU-WDE401-11 SECTION.                                                
118200                                                                          
118300     STRING 'WDE401  *D(WDE401KY =' W-WDE4KEY-X ')'                       
118400          DELIMITED BY SIZE INTO SSA1                                     
118500     STRING 'WDE411  (IDPURAD  =' W-IDPURAD-X ')'                         
118600          DELIMITED BY SIZE INTO SSA2                                     
118700     MOVE '    ' TO GODK-STATUSKODER                                      
118800     CALL CBLTDLI USING GU WDE4-PCB DLI-IO-E401-11 SSA1 SSA2              
118900     MOVE WDE4-STATUS-CODE TO STATUS-WS                                   
119000     PERFORM IMS-STATUSKONTROLL                                           
119100     .                                                                    
119200     SKIP2                                                                
119300 IMS-GU-WDE401 SECTION.                                                   
119301                                                                          
119500     STRING 'WDE401  (WDE401KY =' W-WDE4KEY-X ')'                         
119600          DELIMITED BY SIZE INTO SSA1                                     
119700     MOVE '  '   TO GODK-STATUSKODER                                      
119800     CALL CBLTDLI USING GU WDE4-PCB DLI-IO-AREA7 SSA1                     
119900     MOVE WDE4-STATUS-CODE TO STATUS-WS                                   
120000     PERFORM IMS-STATUSKONTROLL                                           
120100     .                                                                    
120200     EJECT                                                                
120300 IMS-GNP-WDE411 SECTION.                                                  
120400                                                                          
120440                                                                          
120500     STRING 'WDE411  (IDPURAD  =' W-IDPURAD-X ')'                         
120600          DELIMITED BY SIZE INTO SSA1                                     
120700     MOVE '  '   TO GODK-STATUSKODER                                      
120800     CALL CBLTDLI USING GNP WDE4-PCB DLI-IO-AREA8 SSA1                    
120900     MOVE WDE4-STATUS-CODE TO STATUS-WS                                   
121000     PERFORM IMS-STATUSKONTROLL                                           
121100     .                                                                    
121200     EJECT                                                                
121300 IMS-GU-WDE401-11-21      SECTION.                                        
121400                                                                          
121500     STRING 'WDE401  (WDE401KY =' W-WDE4KEY-X ')'                         
121600          DELIMITED BY SIZE INTO SSA1                                     
121700     STRING 'WDE411  (IDPURAD  =' W-IDPURAD-X ')'                         
121800          DELIMITED BY SIZE INTO SSA2                                     
121900     STRING 'WDE421  (WDE421KY =' W-WDE421KY-X ')'                        
122000          DELIMITED BY SIZE INTO SSA3                                     
122100     MOVE '  GE' TO GODK-STATUSKODER                                      
122200     CALL CBLTDLI USING GU  WDE4-PCB DLI-IO-E421 SSA1 SSA2 SSA3           
122300     MOVE WDE4-STATUS-CODE TO STATUS-WS                                   
122400     PERFORM IMS-STATUSKONTROLL                                           
122500     .                                                                    
122600     EJECT                                                                
122700 IMS-GN-WDE401-11-21      SECTION.                                        
122780                                                                          
122800     STRING 'WDE401  (WDE401KY =' W-WDE4KEY-X ')'                         
122900          DELIMITED BY SIZE INTO SSA1                                     
123000     STRING 'WDE411  (IDPURAD  =' W-IDPURAD-X ')'                         
123100          DELIMITED BY SIZE INTO SSA2                                     
123200     MOVE  'WDE421  '          TO SSA3                                    
123300     MOVE '  GEGB' TO GODK-STATUSKODER                                    
123400     CALL CBLTDLI USING GN  WDE4-PCB DLI-IO-E421 SSA1 SSA2 SSA3           
123500     MOVE WDE4-STATUS-CODE TO STATUS-WS                                   
123600     PERFORM IMS-STATUSKONTROLL                                           
123700     .                                                                    
123800     EJECT                                                                
123900 IMS-GU-WDE611 SECTION.                                                   
124000                                                                          
124100       STRING 'WDE601  (IDPRODNR =' W-IDPRODNR-X ')'                      
124200            DELIMITED BY SIZE INTO SSA1                                   
124300       STRING 'WDE611  (IDKOLLI  =' W-IDKOLLI-X ')'                       
124400            DELIMITED BY SIZE INTO SSA2                                   
124500       MOVE 'GE  ' TO GODK-STATUSKODER                                    
124600       CALL CBLTDLI USING GU WDE6-PCB DLI-IOAREA-WDE611 SSA1 SSA2         
124700       MOVE WDE6-STATUS-CODE TO STATUS-WS                                 
124800       PERFORM IMS-STATUSKONTROLL                                         
124900         .                                                                
125000     EJECT                                                                
125100                                                                          
125200 IMS-GU-WDB601    SECTION.                                                
125230                                                                          
125300     STRING 'WDB601  (IDDC     =' W-IDDC-B6-X ')'                         
125400          DELIMITED BY SIZE INTO SSA1                                     
125500     MOVE '  GE' TO GODK-STATUSKODER                                      
125600     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-AREA-B601 SSA1                 
125700     MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
125800     PERFORM IMS-STATUSKONTROLL                                           
125900     IF SEGMENT-SAKNAS                                                    
126000         MOVE SPACE TO DCS-KDDC                                           
126100     END-IF                                                               
126200     .                                                                    
126300 IMS-STATUSKONTROLL SECTION.                                              
126400                                                                          
126500     SET STATUS-IX TO 1                                                   
126600     SEARCH GODK-STATUS                                                   
126700       AT END                                                             
126800         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
126900         DELIMITED BY SIZE INTO FELTEXT                                   
127000         CALL FELLOG                                                      
127100       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS CONTINUE                  
127200     END-SEARCH                                                           
127300     .                                                                    
127400     EJECT                                                                
127500*    -COPY WY2000P1                                                       
