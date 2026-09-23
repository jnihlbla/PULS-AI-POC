000100**********************************************************                
000200 ID DIVISION.                                                             
000300 PROGRAM-ID.     W4063800.                                                
000400 AUTHOR.         MOGREN STINA.                                            
000500 DATE-WRITTEN.   02/05/13.                                                
000600 DATE-COMPILED.                                                           
000700*                                                                         
000800*    FUNKTION:                                                            
000900*      PROGRAMMET ÄR EN BAKGRUNDS-MPP I ADD-IT/BUILD-IT MODULEN           
001000*      SOM LETAR EFTER EJ PRISSATTA  WDE2- RADER                          
001100*      RAD-SEGMENT UPPDATERAS MED PRISER O SUORDV RÄKNAS OM               
001200*      ÄR BLIR KOLLITS ALLA RADER PRISSATTA SKICKAS                       
001300*      EN 'POST' TILL BILLIT                                              
001400*                                                                         
001500*      PROGRAMMET STARTAS AV W00507   (TID)                               
001600*      SAMT STARTAR UPP W4637 I BUILD-IT                                  
001700*                                                                         
001800*                                                                         
001900*        PROGRAMMET SKRIVER    WDE2  TRANSPORTRELREG                      
002000*                                    FAKTURA - BILL-IT                    
002100*                              WDE1  TRANSPORTRELEASEREG.                 
002200*                              WDE4  ORDERRADER                           
002300*                              WDE6  KOLLIREG                             
002400*        PROGRAMMET LÄSER      -------------------                        
002500*                              WDC7  DDI PRIS FRÅGA                       
002600*                              WDB6  DC-REGISTER                          
002700*                                                                         
002800*    INDATA.                                                              
002900*                                                                         
003000*    E-TRACKER 1365139   FÖR ATT FÅ KORREKT PERIODSLUT                    
003100*                        NU ÄR PERIOD = MÅNAD                             
003200*                                                                         
003300*             OM A7 MOT W40637 FÖR MYCKET MATERIAL                        
003400*             ANVÄND TOT-SEND RÄKNARE  ( 2 STÄLLEN)                       
003500*             OCH KÖR FRÅN IMSQVC ETT ANTAL GGR                           
003600                                                                          
003700     SKIP3                                                                
003800 ENVIRONMENT DIVISION.                                                    
003900                                                                          
004000 DATA DIVISION.                                                           
004100     EJECT                                                                
004200 WORKING-STORAGE SECTION.                                                 
004300 77  IDPGM                       PIC X(08)   VALUE 'W4063800'.            
004400                                                                          
004500*    --- ARBETSFÄLT FÖR FELMEDDELANDEN VID CALL ABEND/FELLOG              
004600 77  FELTEXT                     PIC X(80) VALUE SPACE.                   
004700                                                                          
004800                                                                          
004900 77  JA                          PIC X       VALUE 'J'.                   
005000 77  YES                         PIC X       VALUE 'Y'.                   
005100 77  NEJ                         PIC X       VALUE 'N'.                   
005200                                                                          
005300*    --- ARBETSFÄLT FÖR AKTUELLA NYCKELVÄRDEN FRÅN SKÄRMEN                
005400                                                                          
005500                                                                          
005600 77  NYCKLAR-SW                  PIC X       VALUE 'J'.                   
005700     88  NYCKLAR-OK                          VALUE 'J'.                   
005800     88  NYCKLAR-FEL                         VALUE 'N'.                   
005900                                                                          
006000 77  ALLT-SW                     PIC X       VALUE 'J'.                   
006100     88  ALLT-OK                             VALUE 'J'.                   
006200     88  ALLT-NEJ                            VALUE 'N'.                   
006300                                                                          
006400 77  WDE1-SW                     PIC X       VALUE 'N'.                   
006500     88  WDE1-FINNS                          VALUE 'J'.                   
006600                                                                          
006700 77  FL-UPPDAT                   PIC X       VALUE 'N'.                   
006800     88  UPPDAT-EJ                           VALUE 'N'.                   
006900     88  UPPDAT-JA                           VALUE 'J'.                   
007000                                                                          
007100 77  FL-C711                     PIC X       VALUE 'N'.                   
007200     88  C711-POST                           VALUE 'J'.                   
007300                                                                          
007400 77  DELA-SW                     PIC X       VALUE 'N'.                   
007500     88  DELA-JA                             VALUE 'J'.                   
007600     88  DELA-NEJ                            VALUE 'N'.                   
007700                                                                          
007800 77  ANT-SEND                    PIC S9(3)   VALUE ZERO.                  
007900 77  MAX-SEND                    PIC S9(3)   VALUE +5   COMP-3.           
008000 77  TOT-SEND                    PIC S9(5)   VALUE +0   COMP-3.           
008100                                                                          
008200 77  W-SVAR                      PIC X       VALUE SPACE.                 
008300 77  WS-CDC-11                   PIC X(2)    VALUE '11'.                  
008400                                                                          
008500                                                                          
008600 77  RKOD-ABEND-MED-DUMP         PIC S9(4)   VALUE +33  COMP-3.           
008700 77  KDRC-DISP                   PIC 9(4)    VALUE ZERO.                  
008800                                                                          
008900 77  IX                          PIC S9(3)   VALUE ZERO COMP-3.           
009000                                                                          
009100 01  E2-STATUS                   PIC XX      VALUE SPACE.                 
009200 01  E4-STATUS                   PIC XX      VALUE SPACE.                 
009300                                                                          
009400 01  FILLER                      PIC X(16)   VALUE 'WS-SEKTION'.          
009500 01  WS-SEKTION                  PIC X(30)   VALUE SPACE.                 
009600                                                                          
009700 01  WS-PGM                      PIC 9(1)    VALUE ZERO.                  
009800                                                                          
009900 01  WS-IDCOM                    PIC S9(9)   VALUE ZERO COMP-3.           
010000 01  WS-IDKOLLI                  PIC S9(5)   COMP-3.                      
010100                                                                          
010200 01  WS-TIKLOCK                  PIC S9(9)   VALUE ZERO COMP-3.           
010300 01  WWS-TIKLOCK                 PIC 9(8)    VALUE ZERO.                  
010400 01  FILLER                      REDEFINES WWS-TIKLOCK.                   
010500   03  WWTID                     PIC 9(6).                                
010600   03  WWFILL                    PIC 99.                                  
010700 01  WX-KLOCKA                   PIC S9(7) VALUE 220000 COMP-3.           
010800                                                                          
010900 01  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
011000 01  W-DAG                       PIC 9(1)    VALUE ZERO.                  
011100 01  W-TIAARP                    PIC 9(4)    VALUE ZERO.                  
011200 01  W-MANDAG                    PIC 9(6)    VALUE ZERO.                  
011300 01  W-TIAARP-MANDAG             PIC 9(4)    VALUE ZERO.                  
011400 01  W-PSLUT                     PIC X(1)    VALUE 'N'.                   
011500   88  PERIODSLUT                VALUE 'J'.                               
011600                                                                          
011700 01  WS-DATUM                    PIC 9(8).                                
011800 01  FILLER                      REDEFINES WS-DATUM.                      
011900     03  WS-SEKEL                PIC 9(2).                                
012000     03  WS-AAMMDD               PIC 9(6).                                
012100                                                                          
012200 01  WC-DATUM                    PIC 9(6).                                
012300 01  FILLER                      REDEFINES WC-DATUM.                      
012400     03  WC-AAMM                 PIC 9(4).                                
012500     03  WC-DD                   PIC 99.                                  
012600                                                                          
012700 01  WC-KDMOMSIN                 PIC S9(1)   VALUE ZERO COMP-3.           
012800 01  WC-IDDC                     PIC X(2)    VALUE SPACE.                 
012900                                                                          
013000 01  W-DDD1                      PIC S9(5)      VALUE ZERO COMP-3.        
013100 01  W-DDD2                      PIC S9(5)      VALUE ZERO COMP-3.        
013200 01  W-DDDX                      PIC S9(5)      VALUE ZERO COMP-3.        
013300                                                                          
013400 01  WS-PRARTNTO-LOC             PIC S9(7)V9(2) VALUE ZERO COMP-3.        
013500 01  WS-PRARTNTO-LOCPREL         PIC S9(7)V9(2) VALUE ZERO COMP-3.        
013600 01  WS-SUORDV-LOC               PIC S9(9)V9(2) VALUE ZERO COMP-3.        
013700 01  WS-SUORDV-LOCPREL           PIC S9(9)V9(2) VALUE ZERO COMP-3.        
013800 01  WS-SUORDV-AVB-LOC           PIC S9(9)V9(2) VALUE ZERO COMP-3.        
013900 01  WS-SUORDV-AVB-LOCPREL       PIC S9(9)V9(2) VALUE ZERO COMP-3.        
014000 01  WS-SUORDV-LEV-LOC           PIC S9(9)V9(2) VALUE ZERO COMP-3.        
014100 01  WS-SUORDV-LEV-LOCPREL       PIC S9(9)V9(2) VALUE ZERO COMP-3.        
014200                                                                          
014300 01  TEST-IDDISTR                PIC 9(5)    COMP-3 VALUE ZERO.           
014400*01  FILLER   -COPY WWDIST03    -RED TEST-IDDISTR.                        
014500     EJECT                                                                
014600*01  FILLER   -COPY WWDIST34    -RED TEST-IDDISTR.                        
014700     EJECT                                                                
014800*01  FILLER   -COPY WWDIST35    -RED TEST-IDDISTR.                        
014900     EJECT                                                                
015000*01  FILLER   -COPY WWDIST42    -RED TEST-IDDISTR.                        
015100     EJECT                                                                
015200*01  FILLER   -COPY WWDIST28    -RED TEST-IDDISTR.                        
015300     EJECT                                                                
015400*01  -COPY  WWDC99                                                        
015500     EJECT                                                                
015600*01  -COPY WWLANDX2                                                       
015700                                                                          
015800*    --- SUBPROGRAM OCH PARAMETERAREOR                                    
015900 01  GENERELLA-SUBPROGRAM.                                                
016000     03  WMEDKONV                PIC X(8)    VALUE 'WMEDKONV'.            
016100     03  W005INIT                PIC X(8)    VALUE 'W005INIT'.            
016200     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
016300     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
016400     03  ABEND                   PIC X(8)    VALUE 'ABEND   '.            
016500     03  WDATKONV                PIC X(8)    VALUE 'WDATKONV'.            
016600     03  WDAGKONV                PIC X(8)    VALUE 'WDAGKONV'.            
016700     03  W411EXCH                PIC X(8)    VALUE 'W411EXCH'.            
016800     03  WZ01SEND                PIC X(8)    VALUE 'WZ01SEND'.            
016900     03  VIMSID                  PIC X(8)    VALUE 'VIMSID '.             
017000     EJECT                                                                
017100                                                                          
017200*    --- PARAMETRAR TILL SUBPROGRAM WMEDKONV                              
017300*01 -COPY WMEDAREA                                                        
017400     SKIP3                                                                
017500 01  MESSAGE-CODES.                                                       
017600     03  ERR-WRONG-KEY           PIC X(3)    VALUE '401'.                 
017700     EJECT                                                                
017800                                                                          
017900*    --- PARAMETERS TO VIMSID                                             
018000 01  VIMSID-PARM.                                                         
018100   03  IMSID4                    PIC X(4)    VALUE SPACE.                 
018200   03  FILLER                    PIC X(4)    VALUE SPACE.                 
018300     EJECT                                                                
018400                                                                          
018500*01  -COPY WDATAREA                                                       
018600     EJECT                                                                
018700*01  -COPY WDAGAREA                                                       
018800     EJECT                                                                
018900*    --- AREOR TILL GEMENSAMMA SUBPROGRAM                                 
019000*                                                                         
019100 01 FILLER                       PIC X(8) VALUE  'W411EXCH'.              
019200*   -COPY W411EXCH                                                        
019300     EJECT                                                                
019400*    --- PARAMETRAR TILL SUBPROGRAM W005INIT                              
019500*                                                                         
019600 01  FILLER                      PIC X(16)   VALUE 'WMSGINIT'.            
019700     SKIP3                                                                
019800*01 -COPY WMSGINIT                                                        
019900     EJECT                                                                
020000 01  FILLER              PIC X(16)   VALUE '  WMSGSOP-AREA'.              
020100*01      -COPY WMSGSOP                                                    
020200     EJECT                                                                
020300*01      -COPY WMSGAREA                                                   
020400                                                                          
020500*                                                                         
020600*    --- AREOR FÖR ANROP TILL WZ01  ------                                
020700 01  FILLER                      PIC X(16)   VALUE 'WZ01-AREAUT'.         
020800*01  -COPY WZ01SEND                                                       
020900                                                                          
021000 01  FILLER                      PIC X(16)   VALUE  'P-TO-P-AREA'.        
021100 01  P-TO-P-SW.                                                           
021200                                                                          
021300     02  P-TO-P-KVLL             PIC S9(4)           COMP SYNC.           
021400     02  P-TO-P-KDZ1             PIC X(1)  VALUE LOW-VALUE.               
021500     02  P-TO-P-KDZ2             PIC X(1)  VALUE LOW-VALUE.               
021600     02  P-TO-P-KDTRANS          PIC X(8).                                
021700     02  P-TO-P-IDTRANS          PIC X(4).                                
021800     02  P-TO-P-KDMFSFOR         PIC X(1).                                
021900     02  P-TO-P-DATA             PIC X(1000).                             
022000     EJECT                                                                
022100                                                                          
022200                                                                          
022300*    --- AREOR FÖR MFS OCH SKÄRMHANTERING                                 
022400*                                                                         
022500 01  FILLER                      PIC X(16)   VALUE 'MID-AREA'.            
022600     SKIP3                                                                
022700*01  MID -COPY W40637I1                                                   
022800     EJECT                                                                
022900                                                                          
023000*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
023100*                                                                         
023200 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
023300     SKIP3                                                                
023400 01  NYCKLAR-TILL-DLI.                                                    
023500     03  W-IDSHIPM-X.                                                     
023600         05  W-IDSHIPM           PIC  9(7)   VALUE ZERO.                  
023700     03  W-WDE111KY-X.                                                    
023800         05  W-IDDISTR           PIC S9(5)   VALUE ZERO COMP-3.           
023900         05  W-IDKUNDNR          PIC S9(7)   VALUE ZERO COMP-3.           
024000     03  W-WDE121KY-X.                                                    
024100         05  W-IDPRODNR          PIC S9(7)   VALUE ZERO COMP-3.           
024200         05  W-IDKOLLI           PIC S9(5)   VALUE ZERO COMP-3.           
024300     03  W-IDPURAD-X.                                                     
024400         05  W-IDPURAD           PIC S9(5)   VALUE ZERO COMP-3.           
024500     03  W-WDE2ASEQ-MIN-X.                                                
024600         05  W-IDPRODNR-MIN      PIC S9(7)   VALUE ZERO COMP-3.           
024700         05  W-IDKOLLI-MIN       PIC S9(5)   VALUE ZERO COMP-3.           
024800     03  W-WDE2ASEQ-MAX-X.                                                
024900         05  W-IDPRODNR-MAX      PIC S9(7)   VALUE ZERO COMP-3.           
025000         05  W-IDKOLLI-MAX       PIC S9(5)   VALUE ZERO COMP-3.           
025100     03  W-IDARTNR-X.                                                     
025200         05  W-IDARTNR           PIC S9(9)   VALUE ZERO COMP-3.           
025300     03  W-WDB101KY-X.                                                    
025400       05  W-WDB1-IDPARTNR       PIC X(9)    VALUE SPACE.                 
025500       05  W-WDB1-IDFTG          PIC 9(2)    VALUE ZERO.                  
025600     03  W-IDGMT-X.                                                       
025700       05  W-IDDISTR-WDB2        PIC S9(5)   VALUE ZERO COMP-3.           
025800       05  W-IDKUNDNR-WDB2       PIC S9(7)   VALUE ZERO COMP-3.           
025900*                                                                         
026000     03  W-WDC701KY-X.                                                    
026100         05  W-IDDISTR-C7        PIC 9(4)    VALUE ZERO.                  
026200         05  W-IDKUNDNR-C7       PIC 9(7)    VALUE ZERO.                  
026300         05  W-IDBUNDLE-C7       PIC X(15)   VALUE SPACE.                 
026400         05  FILLER              REDEFINES W-IDBUNDLE-C7.                 
026500           07  W-IDORDER-C7      PIC 9(7).                                
026600           07  FILLER            PIC X(8).                                
026700     03  W-IDPRQUES-X.                                                    
026800         05  W-IDPRQUES          PIC 9(7)    VALUE ZERO.                  
026900*                                                                         
027000     03  W-WDE401KY-X.                                                    
027100         05  W-IDDISTR-E4        PIC S9(5)   VALUE ZERO COMP-3.           
027200         05  W-IDKUNDNR-E4       PIC S9(7)   VALUE ZERO COMP-3.           
027300         05  W-IDKUNDRF-E4       PIC X(10)   VALUE SPACE.                 
027400         05  W-IDPRODNR-E4       PIC S9(7)   VALUE ZERO COMP-3.           
027500         05  W-IDPLKLST-E4       PIC S9(3)   VALUE ZERO COMP-3.           
027600*                                                                         
027700     03  W-WDE4BSEQ-X.                                                    
027800         05  W-IDPRODNR-BSEQ     PIC S9(7)   VALUE ZERO COMP-3.           
027900         05  W-IDPURAD-BSEQ      PIC S9(5)   VALUE ZERO COMP-3.           
028000*                                                                         
028100     03  W-IDPRODNR-E6-X.                                                 
028200         05  W-IDPRODNR-E6       PIC S9(7)   VALUE ZERO COMP-3.           
028300     03  W-IDKOLLI-X.                                                     
028400         05  W-IDKOLLI-E6        PIC S9(5)   VALUE ZERO COMP-3.           
028500*                                                                         
028600     03  W-WDQ4ASEQ-X.                                                    
028700         05  W-IDORDNR-ASEQ      PIC S9(7)   VALUE ZERO COMP-3.           
028800         05  W-IDARTNR-ASEQ      PIC S9(9)   VALUE ZERO COMP-3.           
028900         05  W-IDLOPNR-ASEQ      PIC S9(3)   VALUE ZERO COMP-3.           
029000*                                                                         
029100     03  W-IDORDER-X.                                                     
029200         05  W-IDORDER           PIC S9(7)   VALUE ZERO COMP-3.           
029300*                                                                         
029400     03  W-WDQ211KY-X.                                                    
029500         05  W-IDDC              PIC X(2)    VALUE SPACE.                 
029600         05  W-IDLEVNR           PIC X(5)    VALUE SPACE.                 
029700*                                                                         
029800     03  W-IDDC-Q212-X.                                                   
029900         05  W-IDDC-Q212         PIC X(2)    VALUE SPACE.                 
030000*                                                                         
030100     03  W-IDDC-B6-X.                                                     
030200         05 W-IDDC-B6                  PIC X(2).                          
030300*                                                                         
030400     03   W-WDB301KY-X.                                                   
030500         05  W-IDDC-B3           PIC X(2)    VALUE SPACE.                 
030600         05  W-IDDISTR-B3        PIC S9(5)   VALUE ZERO COMP-3.           
030700         05  W-IDKUNDNR-B3       PIC S9(7)   VALUE ZERO COMP-3.           
030800*                                                                         
030900    03  W-WDB301KY-DEF-X.                                                 
031000        05  W-IDDC-B3-DEF        PIC X(2)    VALUE SPACE.                 
031100        05  W-IDDISTR-B3-DEF     PIC S9(5)   VALUE ZERO COMP-3.           
031200        05  W-IDKUNDNR-B3-DEF    PIC S9(7) VALUE +9999999 COMP-3.         
031300                                                                          
031400*                                                                         
031500     SKIP2                                                                
031600                                                                          
031700*    --- STATUS-KOD FRÅN IMS                                              
031800 01  STATUS-WS                   PIC XX.                                  
031900     88  SEGMENT-FINNS                       VALUE '  '.                  
032000     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
032100     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
032200     88  SEGMENT-SLUT                        VALUE 'GB'.                  
032300     SKIP2                                                                
032400 01  GODK-STATUSKODER.                                                    
032500     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
032600     SKIP3                                                                
032700 01  SSA1                        PIC X(64).                               
032800 01  SSA2                        PIC X(64).                               
032900 01  SSA3                        PIC X(64).                               
033000 01  SSA4                        PIC X(64).                               
033100     EJECT                                                                
033200*    --- IMS FUNKTIONSKODER                                               
033300*01  -COPY W0003                                                          
033400     EJECT                                                                
033500                                                                          
033600*    ---  DLI INPUT-OUTPUT AREA                                           
033700                                                                          
033800 01  FILLER                    PIC X(16) VALUE 'DLI-IO-WDE101'.           
033900 01  DLI-IO-WDE101.                                                       
034000*    03  -COPY WDE101                                                     
034100     EJECT                                                                
034200 01  FILLER                    PIC X(16) VALUE 'DLI-IO-WDE111'.           
034300 01  DLI-IO-WDE111.                                                       
034400*    03  -COPY WDE111                                                     
034500     EJECT                                                                
034600 01  FILLER                    PIC X(16) VALUE 'DLI-IO-WDE121'.           
034700 01  DLI-IO-WDE121.                                                       
034800*    03  -COPY WDE121                                                     
034900     EJECT                                                                
035000 01  FILLER                    PIC X(16) VALUE 'DLI-IO-WDE131'.           
035100 01  DLI-IO-WDE131.                                                       
035200*    03  -COPY WDE131                                                     
035300     EJECT                                                                
035400                                                                          
035500 01  FILLER                    PIC X(16) VALUE 'DLI-IO-WDE201'.           
035600 01  DLI-IO-WDE201.                                                       
035700*    03  -COPY WDE201                                                     
035800     EJECT                                                                
035900 01  FILLER                    PIC X(16) VALUE 'DLI-IO-WDE211'.           
036000 01  DLI-IO-WDE211.                                                       
036100*    03  -COPY WDE211                                                     
036200     EJECT                                                                
036300 01  FILLER                    PIC X(16) VALUE 'DLI-IO-WDE221'.           
036400 01  DLI-IO-WDE221.                                                       
036500*    03  -COPY WDE221                                                     
036600     EJECT                                                                
036700 01  FILLER                    PIC X(16) VALUE 'DLI-IO-WDE231'.           
036800 01  DLI-IO-WDE231.                                                       
036900*    03  -COPY WDE231                                                     
037000     EJECT                                                                
037100                                                                          
037200 01  FILLER                    PIC X(16) VALUE 'DLI-IO-WDE401'.           
037300 01  DLI-IO-WDE401.                                                       
037400*    03  -COPY WDE401                                                     
037500     EJECT                                                                
037600 01  FILLER                    PIC X(16) VALUE 'DLI-IO-WDE411'.           
037700 01  DLI-IO-WDE411.                                                       
037800*    03  -COPY WDE411                                                     
037900     EJECT                                                                
038000                                                                          
038100 01  FILLER                    PIC X(16) VALUE 'DLI-IO-WDE601'.           
038200 01  DLI-IO-WDE601.                                                       
038300*    03  -COPY WDE601                                                     
038400     EJECT                                                                
038500 01  FILLER                    PIC X(16) VALUE 'DLI-IO-WDE611'.           
038600 01  DLI-IO-WDE611.                                                       
038700*    03  -COPY WDE611                                                     
038800     EJECT                                                                
038900                                                                          
039000 01  FILLER                    PIC X(16) VALUE 'DLI-IO-WDC701'.           
039100 01  DLI-IO-WDC701.                                                       
039200*    03  -COPY WDC701                                                     
039300     EJECT                                                                
039400 01  FILLER                    PIC X(16) VALUE 'DLI-IO-WDC711'.           
039500 01  DLI-IO-WDC711.                                                       
039600*    03  -COPY WDC711                                                     
039700     EJECT                                                                
039800 01  FILLER               PIC X(16)   VALUE 'WDB601 AREA'.                
039900 01   DLI-IO-AREA-B601.                                                   
040000*     03  -COPY WDB601                                                    
040100     EJECT                                                                
040200 01  FILLER                   PIC X(16) VALUE 'DLI-IO-WDB301'.            
040300 01  DLI-IO-WDB301.                                                       
040400*     03  -COPY WDB301.                                                   
040500     EJECT                                                                
040600 01  FILLER                   PIC X(16) VALUE 'DLI-IO-WDB101'.            
040700 01  DLI-IO-WDB101.                                                       
040800*     03  -COPY WDB101.                                                   
040900     EJECT                                                                
041000 01  FILLER                   PIC X(16) VALUE 'DLI-IO-WDB201'.            
041100 01  DLI-IO-WDB201.                                                       
041200*     03  -COPY WDB201.                                                   
041300     EJECT                                                                
041400                                                                          
041500 LINKAGE SECTION.                                                         
041600                                                                          
041700*01  -COPY W0009  -PRE  MSG-                                              
041800     EJECT                                                                
041900*01  -COPY W0009  -PRE ALT0606-                                           
042000     EJECT                                                                
042100*01  -COPY W0009  -PRE  BU37-                                             
042200     EJECT                                                                
042300*01  -COPY W0009  -PRE  4638-                                             
042400     EJECT                                                                
042500*01  -COPY W0008  -PRE WDE1-                                              
042600     05  FILLER                  PIC X.                                   
042700*01  -COPY W0008  -PRE WDE2-                                              
042800     05  FILLER                  PIC X.                                   
042900*01  -COPY W0008  -PRE WDE4-                                              
043000     05  FILLER                  PIC X.                                   
043100*01  -COPY W0008  -PRE WDE6-                                              
043200     05  FILLER                  PIC X.                                   
043300*01  -COPY W0008  -PRE WDC7-                                              
043400     05  FILLER                  PIC X.                                   
043500*01  -COPY W0008  -PRE WDQ2-                                              
043600     05  FILLER                  PIC X.                                   
043700     EJECT                                                                
043800*01  -COPY W0008  -PRE WDB6-                                              
043900     05  FILLER                  PIC X.                                   
044000*01  -COPY W0008  -PRE WDB3-                                              
044100     05  FILLER                  PIC X.                                   
044200     EJECT                                                                
044300*01  -COPY W0008  -PRE WDB1-                                              
044400     05  FILLER                  PIC X.                                   
044500     EJECT                                                                
044600*01  -COPY W0008  -PRE WDB2-                                              
044700     05  FILLER                  PIC X.                                   
044800     EJECT                                                                
044900*                                                                         
045000 PROCEDURE DIVISION  USING         MSG-PCB                                
045100                                   BU37-PCB                               
045200                                   4638-PCB                               
045300                                   WDE1-PCB WDE2-PCB                      
045400                                   WDE4-PCB WDE6-PCB                      
045500                                   WDC7-PCB                               
045600                                   WDQ2-PCB WDB6-PCB                      
045700                                   WDB3-PCB WDB1-PCB WDB2-PCB.            
045800 MAIN SECTION.                                                            
045900     ENTRY 'DLITCBL' USING         MSG-PCB                                
046000                                   BU37-PCB                               
046100                                   4638-PCB                               
046200                                   WDE1-PCB WDE2-PCB                      
046300                                   WDE4-PCB WDE6-PCB                      
046400                                   WDC7-PCB                               
046500                                   WDQ2-PCB WDB6-PCB                      
046600                                   WDB3-PCB WDB1-PCB WDB2-PCB.            
046700     PERFORM IMS-GET-MSG                                                  
046800     IF SEGMENT-FINNS                                                     
046900       PERFORM A-INIT                                                     
047000*      E221-KOLLIN MED KDPRSTA=R OCH NÅGON RAD UTAN PRIS                  
047100       PERFORM IMS-GHN-WDE2ASEQ                                           
047200                                                                          
047300                                                                          
047400       PERFORM UNTIL SEGMENT-SAKNAS OR SEGMENT-SLUT                       
047500                                                                          
047600         MOVE JA                   TO ALLT-SW                             
047700         MOVE BKOLLI-IDPRODNR      TO W-IDPRODNR                          
047800         MOVE BKOLLI-IDKOLLI       TO W-IDKOLLI                           
047900         PERFORM IMS-GNP-WDE211                                           
048000         IF SEGMENT-FINNS                                                 
048100           MOVE BGMT-IDDISTR       TO W-IDDISTR                           
048200           MOVE BGMT-IDKUNDNR      TO W-IDKUNDNR                          
048300           PERFORM IMS-GNP-WDE201                                         
048400           IF SEGMENT-FINNS                                               
048500             MOVE BILL-IDSHIPM     TO W-IDSHIPM                           
048600             IF SEGMENT-FINNS                                             
048700               PERFORM IMS-GHNP-WDE231                                    
048800             END-IF                                                       
048900           END-IF                                                         
049000         END-IF                                                           
049100         MOVE STATUS-WS            TO E2-STATUS                           
049200                                                                          
049300         MOVE NEJ                  TO WDE1-SW                             
049400         PERFORM IMS-GU-WDE111                                            
049500         IF SEGMENT-FINNS                                                 
049600           MOVE JA                 TO WDE1-SW                             
049700         END-IF                                                           
049800         MOVE E2-STATUS            TO STATUS-WS                           
049900         PERFORM D-NOLLA                                                  
050000*                                                                         
050100         PERFORM UNTIL SEGMENT-SAKNAS                                     
050200           MOVE BRAD-IDPURAD       TO W-IDPURAD                           
050300           IF BRAD-PRARTNTO-LOC = ZERO                                    
050400             PERFORM E-KOMPLETTERA-WDE231                                 
050500             IF WDE1-FINNS                                                
050600               PERFORM F-KOMPLETTERA-WDE131                               
050700             END-IF                                                       
050800           END-IF                                                         
050900                                                                          
051000           PERFORM IMS-GHNP-WDE231                                        
051100                                                                          
051200         END-PERFORM                                                      
051300                                                                          
051400         PERFORM G-UPPDATERA-STATUS-KOLLI                                 
051500         IF WS-SUORDV-LOC          = ZERO AND                             
051600            WS-SUORDV-LOCPREL      = ZERO AND                             
051700            WS-SUORDV-AVB-LOC      = ZERO AND                             
051800            WS-SUORDV-AVB-LOCPREL  = ZERO AND                             
051900            WS-SUORDV-LEV-LOC      = ZERO AND                             
052000            WS-SUORDV-LEV-LOCPREL  = ZERO                                 
052100            CONTINUE                                                      
052200         ELSE                                                             
052300            PERFORM H-UPPDAT-SUMMOR                                       
052400         END-IF                                                           
052500                                                                          
052600         PERFORM J-AVSLUTA-KOLLI                                          
052700                                                                          
052800         PERFORM IMS-GHN-WDE2ASEQ                                         
052900         IF TOT-SEND > 100                                                
053000           MOVE 'GE'               TO STATUS-WS                           
053100         END-IF                                                           
053200       END-PERFORM                                                        
053300       PERFORM Z-FINIT                                                    
053400     END-IF                                                               
053500                                                                          
053600     MOVE ZERO TO RETURN-CODE                                             
053700     GOBACK                                                               
053800     .                                                                    
053900     EJECT                                                                
054000 A-INIT SECTION.                                                          
054100     MOVE 'A-INIT'              TO WS-SEKTION                             
054200                                                                          
054300                                                                          
054400     ACCEPT DAGENS-DATUM    FROM DATE                                     
054500     ACCEPT WS-TIKLOCK      FROM TIME                                     
054600     MOVE WS-TIKLOCK        TO WWS-TIKLOCK                                
054700     MOVE WWTID             TO WS-TIKLOCK                                 
054800*                           TTMMSS                                        
054900***  MOVE    40830    TO DAGENS-DATUM                                     
055000                                                                          
055100     MOVE 'AAMMDD'          TO DAT-KDDATFORM                              
055200     MOVE DAGENS-DATUM      TO DAT-I-TIDATUM                              
055300     CALL WDATKONV USING  DAT-KDDATFORM  DAT-I-TIDATUM                    
055400                          DAT-O-TIDATUM  DAT-KDSVAR                       
055500     IF DAT-KDSVAR-OK                                                     
055600       MOVE DAT-TIDDD       TO W-DDD1                                     
055700     END-IF                                                               
055800                                                                          
055900     IF DAT-KDSVAR-OK                                                     
056000       MOVE DAT-TID         TO W-DAG                                      
056100     END-IF                                                               
056200                                                                          
056300     IF W-DAG = 6                                                         
056400*                SISTA KÖRNINGEN PÅ LÖRDAG ÄR KL 16.01                    
056500       MOVE 160000          TO WX-KLOCKA                                  
056600     END-IF                                                               
056700                                                                          
056800     MOVE DAGENS-DATUM    TO WC-DATUM                                     
056900     MOVE WC-AAMM         TO W-TIAARP                                     
057000                                                                          
057100     MOVE 002             TO DAG-KDCALL                                   
057200     MOVE 20              TO DAG-TISEKEL-FOM                              
057300                             DAG-TISEKEL-TOM                              
057400     MOVE DAGENS-DATUM    TO DAG-TIAAMMDD-FOM                             
057500     MOVE 2               TO DAG-KVKALDAG                                 
057600     IF W-DAG = 6                                                         
057700       MOVE 3             TO DAG-KVKALDAG                                 
057800     END-IF                                                               
057900     CALL WDAGKONV USING  DAG-KDCALL  DAG-DATUM-AREA                      
058000                          DAG-KDSVAR                                      
058100     IF DAG-KDSVAR = SPACE                                                
058200       MOVE DAG-TIAAMMDD-TOM   TO W-MANDAG                                
058300                                  WC-DATUM                                
058400       MOVE WC-AAMM            TO W-TIAARP-MANDAG                         
058500     END-IF                                                               
058600                                                                          
058700                                                                          
058800     IF W-TIAARP NOT = W-TIAARP-MANDAG                                    
058900       MOVE JA               TO W-PSLUT                                   
059000     END-IF                                                               
059100                                                                          
059200     PERFORM S01-OPEN-WZ01                                                
059300                                                                          
059400     MOVE LOW-VALUE            TO W-WDE2ASEQ-MIN-X                        
059500     MOVE HIGH-VALUE           TO W-WDE2ASEQ-MAX-X                        
059600     .                                                                    
059700     EJECT                                                                
059800                                                                          
059900                                                                          
060000 D-NOLLA   SECTION.                                                       
060100     MOVE 'D-NOLLA     '       TO WS-SEKTION                              
060200                                                                          
060300                                                                          
060400     MOVE ZERO                 TO WS-SUORDV-LOC                           
060500                                  WS-SUORDV-LOCPREL                       
060600                                  WS-SUORDV-AVB-LOC                       
060700                                  WS-SUORDV-AVB-LOCPREL                   
060800                                  WS-SUORDV-LEV-LOC                       
060900                                  WS-SUORDV-LEV-LOCPREL                   
061000     MOVE JA                   TO ALLT-SW                                 
061100     MOVE SPACE                TO W-SVAR                                  
061200     .                                                                    
061300     EJECT                                                                
061400 E-KOMPLETTERA-WDE231   SECTION.                                          
061500     MOVE 'E-KOMPLETTERA-WDE231'  TO WS-SEKTION                           
061600                                                                          
061700     MOVE NEJ                        TO FL-C711                           
061800                                        DELA-SW                           
061900     MOVE 'GE'                       TO E4-STATUS                         
062000     IF BRAD-PRARTNTO-LOC = 0                                             
062100       MOVE NEJ                      TO FL-UPPDAT                         
062200       IF BRAD-IDPRQUES NOT = ZERO                                        
062300         MOVE BKOLLI-IDDISTR         TO W-IDDISTR-C7                      
062400         MOVE BKOLLI-IDKUNDNR        TO W-IDKUNDNR-C7                     
062500         MOVE BKOLLI-IDKUNDRF-GRP    TO W-IDBUNDLE-C7                     
062600         MOVE BKOLLI-IDPRODNR        TO W-IDPRODNR-BSEQ                   
062700         MOVE BRAD-IDPURAD           TO W-IDPURAD-BSEQ                    
062800         MOVE BRAD-IDPRQUES          TO W-IDPRQUES                        
062900         PERFORM IMS-GHU-WDE411-BSEQ                                      
063000         MOVE STATUS-WS              TO E4-STATUS                         
063100         IF ORAD-IDKUNDRF-RO NOT = '00000     '                           
063200           MOVE ORAD-IDKUNDRF-RO(1:5) TO W-IDORDER-C7(3:5)                
063300           PERFORM IMS-GHU-WDC711                                         
063400         ELSE                                                             
063500           PERFORM IMS-GHU-WDC711                                         
063600         END-IF                                                           
063700*                                                                         
063800         IF SEGMENT-FINNS                                                 
063900           MOVE JA                   TO FL-C711                           
064000           IF  LPRQ-KDPRSTA = 'A' OR 'M'                                  
064100                                                                          
064200             ADD BRAD-KVLEVART      TO LPRQ-KVANTAL-AVBOK                 
064300             IF LPRQ-KVANTAL-AVBOK >=  LPRQ-KVBEART                       
064400                MOVE 'Y'            TO LPRQ-FLALL                         
064500             END-IF                                                       
064600             PERFORM IMS-REPL-WDC711                                      
064700                                                                          
064800             PERFORM EB-KOMPLETTERA-WDE411                                
064900                                                                          
065000             MOVE BRAD-PRARTNTO-LOC     TO WS-PRARTNTO-LOC                
065100             MOVE BRAD-PRARTNTO-LOCPREL TO WS-PRARTNTO-LOCPREL            
065200*------- UPDATERA PRARTBTO/NTO FRÅN C711 TILL E231                        
065300**           PERFORM EC-RAKNA-OM-PRISFRAGA                                
065400             MOVE LPRQ-PRARTBTO-LOC     TO BRAD-PRARTBTO-LOC              
065500             IF ORAD-KDPRTYP NOT = 'P'                                    
065600              MOVE LPRQ-PRARTNTO-LOC    TO BRAD-PRARTNTO-LOC              
065700             END-IF                                                       
065800             MOVE LPRQ-KDVALISO         TO BRAD-KDVALISO                  
065900             PERFORM S20-VATCODE                                          
066000             MOVE LPRQ-REARTRAB         TO BRAD-RERAB                     
066100             MOVE LPRQ-KDRAB            TO BRAD-KDRAB                     
066200             MOVE LPRQ-BEART-VIPS       TO BRAD-BEART-VIPS                
066300                                                                          
066400             IF BRAD-PRARTBTO-LOC > 0 AND                                 
066500                BRAD-PRARTNTO-LOCPREL > 0                                 
066600               MOVE ZERO             TO BRAD-PRARTNTO-LOCPREL             
066700             END-IF                                                       
066800             PERFORM IMS-REPL-WDE231                                      
066900                                                                          
067000             MOVE JA                 TO FL-UPPDAT                         
067100                                                                          
067200*-------  SUORDV FÖR E401, E121, E6..,                                    
067300             COMPUTE WS-SUORDV-LOC =                                      
067400                 WS-SUORDV-LOC +                                          
067500                 BRAD-KVLEVART * BRAD-PRARTNTO-LOC -                      
067600                 BRAD-KVLEVART * WS-PRARTNTO-LOC                          
067700             COMPUTE WS-SUORDV-LOCPREL =                                  
067800                 WS-SUORDV-LOCPREL +                                      
067900                 BRAD-KVLEVART * BRAD-PRARTNTO-LOCPREL -                  
068000                 BRAD-KVLEVART * WS-PRARTNTO-LOCPREL                      
068100                                                                          
068200             COMPUTE WS-SUORDV-AVB-LOC =                                  
068300                 WS-SUORDV-AVB-LOC +                                      
068400                 BRAD-KVLEVART * BRAD-PRARTNTO-LOC -                      
068500                 BRAD-KVLEVART * WS-PRARTNTO-LOC                          
068600**               ORAD-KVAVBART * BRAD-PRARTNTO-LOC -                      
068700**               ORAD-KVAVBART * WS-PRARTNTO-LOC                          
068800             COMPUTE WS-SUORDV-AVB-LOCPREL =                              
068900                 WS-SUORDV-AVB-LOCPREL +                                  
069000                 BRAD-KVLEVART * BRAD-PRARTNTO-LOCPREL -                  
069100                 BRAD-KVLEVART * WS-PRARTNTO-LOCPREL                      
069200**               ORAD-KVAVBART * BRAD-PRARTNTO-LOCPREL -                  
069300**               ORAD-KVAVBART * WS-PRARTNTO-LOCPREL                      
069400                                                                          
069500             COMPUTE WS-SUORDV-LEV-LOC =                                  
069600                 WS-SUORDV-LEV-LOC +                                      
069700                 BRAD-KVLEVART * BRAD-PRARTNTO-LOC -                      
069800                 BRAD-KVLEVART * WS-PRARTNTO-LOC                          
069900**               ORAD-KVLEVART * BRAD-PRARTNTO-LOC -                      
070000**               ORAD-KVLEVART * WS-PRARTNTO-LOC                          
070100             COMPUTE WS-SUORDV-LEV-LOCPREL =                              
070200                 WS-SUORDV-LEV-LOCPREL +                                  
070300                 BRAD-KVLEVART * BRAD-PRARTNTO-LOCPREL -                  
070400                 BRAD-KVLEVART * WS-PRARTNTO-LOCPREL                      
070500**               ORAD-KVLEVART * BRAD-PRARTNTO-LOCPREL -                  
070600**               ORAD-KVLEVART * WS-PRARTNTO-LOCPREL                      
070700                                                                          
070800           ELSE                                                           
070900             MOVE NEJ               TO ALLT-SW                            
071000             MOVE LPRQ-KDPRSTA      TO W-SVAR                             
071100           END-IF                                                         
071200         ELSE                                                             
071300           MOVE NEJ                 TO ALLT-SW                            
071400         END-IF                                                           
071500       END-IF                                                             
071600                                                                          
071700*      -- FIND OUT WHICH IMS SYSTEM WE ARE USING                          
071800       CALL VIMSID USING VIMSID-PARM                                      
071900                                                                          
072000       EVALUATE IMSID4                                                    
072100         WHEN 'IMG0'                                                      
072200           IF UPPDAT-EJ                                                   
072300**           KOLLA OM 24 TIMMAR GÅTT     ELLER                            
072400**                    SISTA DAG I PERIOD O KL ÖVER 22:00                  
072500             MOVE 'AAMMDD'          TO DAT-KDDATFORM                      
072600             MOVE BILL-TISKEPPN     TO DAT-I-TIDATUM                      
072700             CALL WDATKONV USING  DAT-KDDATFORM  DAT-I-TIDATUM            
072800                                  DAT-O-TIDATUM  DAT-KDSVAR               
072900             IF DAT-KDSVAR-OK                                             
073000               MOVE DAT-TIDDD       TO W-DDD2                             
073100             END-IF                                                       
073200             COMPUTE W-DDDX = W-DDD1 - W-DDD2                             
073300             IF (BILL-TISKEPPN < DAGENS-DATUM  AND                        
073400                 BILL-TISKPTID < WS-TIKLOCK)   OR                         
073500                W-DDDX > 3  OR                                            
073600**              ANTAL DAGAR >/= 1  I TEST-MILJÖERNA                       
073700                (PERIODSLUT AND WS-TIKLOCK > WX-KLOCKA)                   
073800                PERFORM EA-LOCPREL-TILL-LOCPRISER                         
073900               MOVE JA                     TO ALLT-SW                     
074000             END-IF                                                       
074100           END-IF                                                         
074200         WHEN OTHER                                                       
074300           PERFORM EA-LOCPREL-TILL-LOCPRISER                              
074400       END-EVALUATE                                                       
074500     END-IF                                                               
074600     .                                                                    
074700     EJECT                                                                
074800 EA-LOCPREL-TILL-LOCPRISER  SECTION.                                      
074900     MOVE 'EA-LOCPREL-TILL-LOCPRISER'  TO WS-SEKTION                      
075000                                                                          
075100     MOVE BRAD-PRARTNTO-LOC            TO WS-PRARTNTO-LOC                 
075200     MOVE BRAD-PRARTNTO-LOCPREL        TO WS-PRARTNTO-LOCPREL             
075300*------- UPDATERA PRARTBTO/NTO PÅ E231                                    
075400     MOVE BRAD-PRARTNTO-LOCPREL        TO BRAD-PRARTNTO-LOC               
075500                                                                          
075600     MOVE ZERO                         TO BRAD-PRARTNTO-LOCPREL           
075700     IF BRAD-PRARTBTO-LOC = ZERO                                          
075800        MOVE BRAD-PRARTNTO-LOC         TO BRAD-PRARTBTO-LOC               
075900        MOVE '00'                      TO BRAD-KDRAB                      
076000     END-IF                                                               
076100     MOVE 'T'                          TO BRAD-KDPRTYP                    
076200     PERFORM IMS-REPL-WDE231                                              
076300*                                                                         
076400     MOVE NEJ                          TO DELA-SW                         
076500     IF C711-POST                                                         
076600        ADD BRAD-KVLEVART              TO LPRQ-KVANTAL-AVBOK              
076700        IF LPRQ-KVANTAL-AVBOK >=  LPRQ-KVBEART                            
076800           MOVE 'Y'            TO LPRQ-FLALL                              
076900        END-IF                                                            
077000        PERFORM IMS-REPL-WDC711                                           
077100     END-IF                                                               
077200*                                                                         
077300     MOVE NEJ                          TO FL-C711                         
077400*-------  SUORDV FÖR E401, E121, E6..,                                    
077500     COMPUTE WS-SUORDV-LOC = WS-SUORDV-LOC +                              
077600                 BRAD-KVLEVART * BRAD-PRARTNTO-LOC -                      
077700                 BRAD-KVLEVART * WS-PRARTNTO-LOC                          
077800     COMPUTE WS-SUORDV-LOCPREL = WS-SUORDV-LOCPREL +                      
077900                 BRAD-KVLEVART * BRAD-PRARTNTO-LOCPREL -                  
078000                 BRAD-KVLEVART * WS-PRARTNTO-LOCPREL                      
078100                                                                          
078200     MOVE BKOLLI-IDPRODNR      TO W-IDPRODNR-BSEQ                         
078300     MOVE BRAD-IDPURAD         TO W-IDPURAD-BSEQ                          
078400*    PERFORM IMS-GHU-WDE411-BSEQ                                          
078500     IF WS-PRARTNTO-LOC = ZERO AND                                        
078600        WS-PRARTNTO-LOCPREL = ZERO                                        
078700        MOVE 'GE'              TO E4-STATUS                               
078800     END-IF                                                               
078900     MOVE E4-STATUS            TO STATUS-WS                               
079000     IF SEGMENT-FINNS                                                     
079100*------- UPDATERA PRARTNTO/NTO PÅ E411                                    
079200        IF ORAD-PRARTNTO-LOCPREL NOT = ZERO                               
079300          MOVE ORAD-PRARTNTO-LOCPREL  TO ORAD-PRARTNTO-LOC                
079400        END-IF                                                            
079500        IF ORAD-IDPRQUES NOT = ZERO                                       
079600          IF LPRQ-FLALL = 'Y'                                             
079700            MOVE ZERO               TO ORAD-PRARTNTO-LOCPREL              
079800          END-IF                                                          
079900        ELSE                                                              
080000          MOVE ZERO                 TO ORAD-PRARTNTO-LOCPREL              
080100        END-IF                                                            
080200                                                                          
080300        PERFORM IMS-REPL-WDE411                                           
080400                                                                          
080500        COMPUTE WS-SUORDV-AVB-LOC =                                       
080600                WS-SUORDV-AVB-LOC +                                       
080700                BRAD-KVLEVART * BRAD-PRARTNTO-LOC -                       
080800                BRAD-KVLEVART * WS-PRARTNTO-LOC                           
080900**              ORAD-KVAVBART * BRAD-PRARTNTO-LOC -                       
081000**              ORAD-KVAVBART * WS-PRARTNTO-LOC                           
081100        COMPUTE WS-SUORDV-AVB-LOCPREL =                                   
081200                WS-SUORDV-AVB-LOCPREL +                                   
081300                BRAD-KVLEVART * BRAD-PRARTNTO-LOCPREL -                   
081400                BRAD-KVLEVART * WS-PRARTNTO-LOCPREL                       
081500**              ORAD-KVAVBART * BRAD-PRARTNTO-LOCPREL -                   
081600**              ORAD-KVAVBART * WS-PRARTNTO-LOCPREL                       
081700                                                                          
081800        COMPUTE WS-SUORDV-LEV-LOC =                                       
081900                WS-SUORDV-LEV-LOC +                                       
082000                BRAD-KVLEVART * BRAD-PRARTNTO-LOC -                       
082100                BRAD-KVLEVART * WS-PRARTNTO-LOC                           
082200**              ORAD-KVLEVART * BRAD-PRARTNTO-LOC -                       
082300**              ORAD-KVLEVART * WS-PRARTNTO-LOC                           
082400        COMPUTE WS-SUORDV-LEV-LOCPREL =                                   
082500                WS-SUORDV-LEV-LOCPREL +                                   
082600                BRAD-KVLEVART * BRAD-PRARTNTO-LOCPREL -                   
082700                BRAD-KVLEVART * WS-PRARTNTO-LOCPREL                       
082800**              ORAD-KVLEVART * BRAD-PRARTNTO-LOCPREL -                   
082900**              ORAD-KVLEVART * WS-PRARTNTO-LOCPREL                       
083000                                                                          
083100     END-IF                                                               
083200     .                                                                    
083300     EJECT                                                                
083400 EB-KOMPLETTERA-WDE411   SECTION.                                         
083500     MOVE 'EB-KOMPLETTERA-WDE411'  TO WS-SEKTION                          
083600                                                                          
083700     MOVE BKOLLI-IDPRODNR      TO W-IDPRODNR-BSEQ                         
083800     MOVE BRAD-IDPURAD         TO W-IDPURAD-BSEQ                          
083900**   PERFORM IMS-GHU-WDE411-BSEQ                                          
084000     MOVE E4-STATUS            TO STATUS-WS                               
084100     IF SEGMENT-FINNS                                                     
084200      IF  LPRQ-KDPRSTA = 'A' OR 'M'                                       
084300*------- UPDATERA PRARTBTO/NTO FRÅN C711 TILL E411                        
084400**      PERFORM EC-RAKNA-OM-PRISFRAGA                                     
084500        MOVE LPRQ-PRARTBTO-LOC  TO ORAD-PRARTBTO-LOC                      
084600        IF ORAD-KDPRTYP NOT = 'P'                                         
084700          MOVE LPRQ-PRARTNTO-LOC  TO ORAD-PRARTNTO-LOC                    
084800        END-IF                                                            
084900        MOVE LPRQ-KDVALISO      TO ORAD-KDVALISO                          
085000        MOVE LPRQ-KDVAT         TO ORAD-KDVAT                             
085100        MOVE LPRQ-REARTRAB      TO ORAD-RERAB                             
085200        MOVE LPRQ-KDRAB         TO ORAD-KDRAB                             
085300        MOVE LPRQ-BEART-VIPS    TO ORAD-BEART-VIPS                        
085400                                                                          
085500        IF ORAD-PRARTNTO-LOC     NOT = ZERO AND                           
085600           ORAD-PRARTNTO-LOCPREL NOT = ZERO                               
085700           MOVE JA              TO DELA-SW                                
085800        END-IF                                                            
085900                                                                          
086000        IF ORAD-PRARTNTO-LOC > 0 AND                                      
086100           ORAD-PRARTNTO-LOCPREL > 0 AND LPRQ-FLALL = 'Y'                 
086200           MOVE ZERO            TO ORAD-PRARTNTO-LOCPREL                  
086300        END-IF                                                            
086400*       IF ORAD-KDPRTYP NOT = 'P'                                         
086500*         MOVE 'A'              TO ORAD-KDPRTYP                           
086600*       END-IF                                                            
086700      END-IF                                                              
086800      PERFORM IMS-REPL-WDE411                                             
086900     END-IF                                                               
087000     .                                                                    
087100     EJECT                                                                
087200*EC-RAKNA-OM-PRISFRAGA  SECTION.                                          
087300*    MOVE 'EC-RAKNA-OM-PRISFRAGA' TO WS-SEKTION                           
087400                                                                          
087500****                                                                      
087600*      HELA SECTIONEN ÄR EN FIX FÖR  XTRA-MILJÖN                          
087700****                                                                      
087800*    MOVE BGMT-IDDISTR           TO TEST-IDDISTR                          
087900*    IF  DIST03-SVERIGE                                                   
088000*     IF LPRQ-KDVALISO NOT = BRAD-KDVALISO                                
088100*      MOVE LPRQ-KDVALISO        TO W-KDVALISO-A                          
088200*      PERFORM IMS-GET-VALUTA-A                                           
088300**           RÄKNA OM TILL SAMMA VALUTA SOM KUNDEN                        
088400*     IF BRAD-KDVALISO = 'SEK'  OR SPACE                                  
088500*        MOVE X-9304-PRKURS      TO EXCH-PRKURS                           
088600**           +1 KDCALL = LOKAL VALUTA TILL SEK                            
088700*        MOVE +1                 TO EXCH-KDCALL                           
088800*        MOVE +0                 TO EXCH-SUORDV-IN                        
088900*        MOVE LPRQ-PRARTBTO-LOC  TO EXCH-PRARTNTO-IN                      
089000*        CALL  W411EXCH           USING EXCH-W411EXCH                     
089100*        MOVE EXCH-PRARTNTO-UT   TO LPRQ-PRARTBTO-LOC                     
089200*        MOVE LPRQ-PRARTNTO-LOC  TO EXCH-PRARTNTO-IN                      
089300*        CALL  W411EXCH           USING EXCH-W411EXCH                     
089400*        MOVE EXCH-PRARTNTO-UT   TO LPRQ-PRARTNTO-LOC                     
089500*        MOVE BRAD-KDVALISO      TO LPRQ-KDVALISO                         
089600*      END-IF                                                             
089700*     END-IF                                                              
089800*    END-IF                                                               
089900*    .                                                                    
090000*    EJECT                                                                
090100 F-KOMPLETTERA-WDE131   SECTION.                                          
090200     MOVE 'F-KOMPLETTERA-WDE131'  TO WS-SEKTION                           
090300                                                                          
090400     MOVE BKOLLI-IDPRODNR         TO W-IDPRODNR                           
090500     MOVE BKOLLI-IDKOLLI          TO W-IDKOLLI                            
090600     PERFORM IMS-GHU-WDE131                                               
090700     IF SEGMENT-FINNS                                                     
090800       MOVE BRAD-PRARTNTO-LOC     TO SRAD-PRARTNTO-LOC                    
090900       MOVE BRAD-PRARTNTO-LOCPREL TO SRAD-PRARTNTO-LOCPREL                
091000       PERFORM IMS-REPL-WDE131                                            
091100     END-IF                                                               
091200     .                                                                    
091300     EJECT                                                                
091400 G-UPPDATERA-STATUS-KOLLI SECTION.                                        
091500     MOVE 'G-UPPDATERA-STATUS-KOLLI' TO WS-SEKTION                        
091600                                                                          
091700     MOVE BGMT-IDDISTR            TO TEST-IDDISTR                         
091800     IF (ALLT-OK )                                                        
091900*       UPPDATERA KDPRTYP  I WDE221 OM ALLA RADER HAR PRIS                
092000       PERFORM IMS-GHU-WDE221-ASEQ                                        
092100       MOVE 'A'                  TO BKOLLI-KDPRSTA                        
092200       PERFORM IMS-REPL-WDE221                                            
092300     END-IF                                                               
092400     .                                                                    
092500     EJECT                                                                
092600 H-UPPDAT-SUMMOR  SECTION.                                                
092700     MOVE 'H-UPPDAT-SUMMOR'       TO WS-SEKTION                           
092800                                                                          
092900     IF WDE1-FINNS                                                        
093000       PERFORM HA-SUMMERA-WDE121                                          
093100     END-IF                                                               
093200     PERFORM HB-SUMMERA-WDE401                                            
093300     PERFORM HC-SUMMERA-WDE601                                            
093400     PERFORM HD-SUMMERA-WDE611                                            
093500                                                                          
093600     MOVE ZERO                    TO WS-SUORDV-LOC                        
093700                                     WS-SUORDV-LOCPREL                    
093800                                     WS-SUORDV-AVB-LOC                    
093900                                     WS-SUORDV-AVB-LOCPREL                
094000                                     WS-SUORDV-LEV-LOC                    
094100                                     WS-SUORDV-LEV-LOCPREL                
094200     .                                                                    
094300     EJECT                                                                
094400 HA-SUMMERA-WDE121   SECTION.                                             
094500     MOVE 'HA-SUMMERA-WDE121'  TO WS-SEKTION                              
094600                                                                          
094700     PERFORM IMS-GHU-WDE121                                               
094800     IF SEGMENT-FINNS                                                     
094900       COMPUTE SKOLLI-SUORDV-LOC     = SKOLLI-SUORDV-LOC +                
095000               WS-SUORDV-LOC                                              
095100       COMPUTE SKOLLI-SUORDV-LOCPREL = SKOLLI-SUORDV-LOCPREL +            
095200               WS-SUORDV-LOCPREL                                          
095300       PERFORM IMS-REPL-WDE121                                            
095400     END-IF                                                               
095500     .                                                                    
095600     EJECT                                                                
095700 HB-SUMMERA-WDE401   SECTION.                                             
095800     MOVE 'HB-SUMMERA-WDE401'  TO WS-SEKTION                              
095900                                                                          
096000     PERFORM IMS-GHNP-WDE401                                              
096100     IF SEGMENT-FINNS                                                     
096200       MOVE BILL-IDDC                TO W-IDDC-B6                         
096300       PERFORM IMS-GU-WDB601                                              
096400       IF DCS-DDC AND ORAD-FLDIRLEV = JA                                  
096500         COMPUTE KORD-SUORDV-LEVPL-LOC  =                                 
096600               KORD-SUORDV-LEVPL-LOC    +                                 
096700               WS-SUORDV-AVB-LOC                                          
096800         COMPUTE KORD-SUORDV-LEVPL-LOCPREL =                              
096900               KORD-SUORDV-LEVPL-LOCPREL   +                              
097000               WS-SUORDV-AVB-LOCPREL                                      
097100       ELSE                                                               
097200         COMPUTE KORD-SUORDV-LOC     = KORD-SUORDV-LOC +                  
097300               WS-SUORDV-AVB-LOC                                          
097400         COMPUTE KORD-SUORDV-LOCPREL = KORD-SUORDV-LOCPREL +              
097500               WS-SUORDV-AVB-LOCPREL                                      
097600       END-IF                                                             
097700       PERFORM IMS-REPL-WDE401                                            
097800     END-IF                                                               
097900     .                                                                    
098000     EJECT                                                                
098100 HC-SUMMERA-WDE601   SECTION.                                             
098200     MOVE 'HC-SUMMERA-WDE601'  TO WS-SEKTION                              
098300                                                                          
098400     MOVE BKOLLI-IDPRODNR         TO W-IDPRODNR-E6                        
098500     MOVE BKOLLI-IDKOLLI          TO W-IDKOLLI-E6                         
098600     PERFORM IMS-GHU-WDE601                                               
098700     IF SEGMENT-FINNS                                                     
098800       COMPUTE VORD-SUORDV-LOC     = VORD-SUORDV-LOC +                    
098900               WS-SUORDV-AVB-LOC                                          
099000       COMPUTE VORD-SUORDV-LOCPREL = VORD-SUORDV-LOCPREL +                
099100               WS-SUORDV-AVB-LOCPREL                                      
099200       COMPUTE VORD-SUORDV-PACK-LOC = VORD-SUORDV-PACK-LOC +              
099300               WS-SUORDV-LEV-LOC                                          
099400       COMPUTE VORD-SUORDV-PACK-LOCPREL =                                 
099500               VORD-SUORDV-PACK-LOCPREL +                                 
099600               WS-SUORDV-LEV-LOCPREL                                      
099700       COMPUTE VORD-SUORDV-FL-LOC = VORD-SUORDV-FL-LOC +                  
099800               WS-SUORDV-LEV-LOC                                          
099900       COMPUTE VORD-SUORDV-FL-LOCPREL =                                   
100000               VORD-SUORDV-FL-LOCPREL +                                   
100100               WS-SUORDV-LEV-LOCPREL                                      
100200       PERFORM IMS-REPL-WDE601                                            
100300     END-IF                                                               
100400     .                                                                    
100500     EJECT                                                                
100600 HD-SUMMERA-WDE611   SECTION.                                             
100700     MOVE 'HD-SUMMERA-WDE611'  TO WS-SEKTION                              
100800                                                                          
100900     PERFORM IMS-GHNP-WDE611                                              
101000     IF SEGMENT-FINNS                                                     
101100       COMPUTE KOLLI-SUORDV-LOC     = KOLLI-SUORDV-LOC +                  
101200               WS-SUORDV-LOC                                              
101300       COMPUTE KOLLI-SUORDV-LOCPREL = KOLLI-SUORDV-LOCPREL +              
101400               WS-SUORDV-LOCPREL                                          
101500                                                                          
101600                                                                          
101700       PERFORM IMS-REPL-WDE611                                            
101800                                                                          
101900     END-IF                                                               
102000     .                                                                    
102100     EJECT                                                                
102200                                                                          
102300 J-AVSLUTA-KOLLI SECTION.                                                 
102400     MOVE 'J-AVSLUTA-KOLLI'    TO WS-SEKTION                              
102500                                                                          
102600     IF ALLT-OK                                                           
102700       MOVE BILL-IDSHIPM         TO MID-IDSHIPM                           
102800       MOVE BKOLLI-IDKOLLI       TO MID-IDKOLLI                           
102900       MOVE BKOLLI-IDKUNDNR      TO MID-IDKUNDNR                          
103000       MOVE BKOLLI-IDDISTR       TO MID-IDDISTR                           
103100       MOVE BKOLLI-IDPRODNR      TO MID-IDPRODNR                          
103200       PERFORM S02-SEND-WZ01                                              
103300       ADD 1                     TO TOT-SEND                              
103400     END-IF                                                               
103500                                                                          
103600     .                                                                    
103700     EJECT                                                                
103800 Z-FINIT  SECTION.                                                        
103900     MOVE 'Z-FINIT'             TO WS-SEKTION                             
104000                                                                          
104100*           STARTAR UPP W4637 (BUILDIT)                                   
104200     PERFORM S03-CLOSE-WZ01                                               
104300*    PERFORM IMS-PURGE-ALTMSG-0606                                        
104400     IF TOT-SEND > 100                                                    
104500       PERFORM S10-STARTA-W40638                                          
104600     END-IF                                                               
104700                                                                          
104800     .                                                                    
104900     EJECT                                                                
105000 S01-OPEN-WZ01 SECTION.                                                   
105100     MOVE 'S01-OPEN-WZ01'       TO WS-SEKTION                             
105200                                                                          
105300     MOVE 'OPEN'                     TO SEND-KDFUNC                       
105400     MOVE 'CARPARTS.PULS.BUILDIT '   TO SEND-ADDISPABS                    
105500     MOVE SPACE                      TO SEND-ADDISPABS-RETURN             
105600                                                                          
105700     CALL WZ01SEND   USING      SEND-CONTROL-AREA                         
105800                                SEND-OPEN-AREA                            
105900     IF SEND-KDRC > 0                                                     
106000       MOVE SEND-KDRC           TO KDRC-DISP                              
106100       STRING 'WZ01SEND-OPEN RC-ERR = ' KDRC-DISP                         
106200            DELIMITED BY SIZE INTO FELTEXT                                
106300       CALL FELLOG                                                        
106400     ELSE                                                                 
106500       MOVE SEND-IDCOM               TO WS-IDCOM                          
106600     END-IF                                                               
106700     .                                                                    
106800     EJECT                                                                
106900 S02-SEND-WZ01 SECTION.                                                   
107000     MOVE 'S02-SEND-WZ01'    TO WS-SEKTION                                
107100                                                                          
107200     MOVE 'PUT'                      TO SEND-KDFUNC                       
107300     COMPUTE SEND-KVDLEN = LENGTH OF MID-W40637I1                         
107400                                                                          
107500     CALL WZ01SEND   USING      SEND-CONTROL-AREA                         
107600                                SEND-KVDLEN                               
107700                                MID-W40637I1                              
107800     IF SEND-KDRC > 0                                                     
107900       MOVE SEND-KDRC           TO KDRC-DISP                              
108000       STRING 'WZ01SEND-PUT RC-ERR = ' KDRC-DISP                          
108100            DELIMITED BY SIZE INTO FELTEXT                                
108200       CALL FELLOG                                                        
108300     END-IF                                                               
108400                                                                          
108500     ADD +1                  TO ANT-SEND                                  
108600     .                                                                    
108700     EJECT                                                                
108800 S03-CLOSE-WZ01  SECTION.                                                 
108900     MOVE 'S03-CLOSE-WZ01'      TO WS-SEKTION                             
109000                                                                          
109100     MOVE 'CLOSE'               TO SEND-KDFUNC                            
109200                                                                          
109300     CALL WZ01SEND   USING      SEND-CONTROL-AREA                         
109400     IF SEND-KDRC > 0                                                     
109500       MOVE SEND-KDRC           TO KDRC-DISP                              
109600       STRING 'WZ01SEND-CLOSE RC-ERR = ' KDRC-DISP                        
109700            DELIMITED BY SIZE INTO FELTEXT                                
109800       CALL FELLOG                                                        
109900     END-IF                                                               
110000     .                                                                    
110100     EJECT                                                                
110200 S10-STARTA-W40638  SECTION.                                              
110300*      STARTA OM W40638                                                   
110400     MOVE LOW-VALUE            TO P-TO-P-KDZ1                             
110500     MOVE LOW-VALUE            TO P-TO-P-KDZ2                             
110600                                                                          
110700     MOVE 'W40638X '           TO P-TO-P-KDTRANS                          
110800     MOVE '4638'               TO P-TO-P-IDTRANS                          
110900     MOVE '1'                  TO P-TO-P-KDMFSFOR                         
111000     COMPUTE P-TO-P-KVLL = 17                                             
111100                                                                          
111200     MOVE SPACE                TO P-TO-P-DATA                             
111300     PERFORM IMS-ISRT-ALT-MSG-4638                                        
111400     .                                                                    
111500 S20-VATCODE  SECTION.                                                    
111600*    ÄNDRAD AV BOSSE H. 20021129                                          
111700     MOVE BGMT-IDDISTR     TO TEST-IDDISTR                                
111800     PERFORM S20A-LAS-WDB3                                                
111900     MOVE WC-IDDC              TO WS-IDDC                                 
112000     IF WS-IDDC NOT = W-IDDC-B6                                           
112100        MOVE WS-IDDC TO W-IDDC-B6                                         
112200        PERFORM IMS-GU-WDB601                                             
112300     END-IF                                                               
112400     IF WC-KDMOMSIN    = +2                                               
112500*                MOMSFRITT                                                
112600       IF DCS-SDC AND DCS-ITALY                                           
112700         IF BGMT-IDDISTR = 1558                                           
112800         OR BGMT-IDDISTR = 1578                                           
112900           MOVE 'ID'           TO BRAD-KDVAT                              
113000         ELSE                                                             
113100           MOVE 'IC'           TO BRAD-KDVAT                              
113200         END-IF                                                           
113300       ELSE                                                               
113400        IF DCS-SDC AND DCS-HOLLAND                                        
113500         IF BGMT-IDDISTR    =  1619 OR 1620 OR 1622 OR 1628 OR            
113600                             1678                                         
113700           MOVE 'NZ'           TO BRAD-KDVAT                              
113800         ELSE                                                             
113900           IF DIST42-EU                                                   
114000             MOVE 'NI'         TO BRAD-KDVAT                              
114100           ELSE                                                           
114200             MOVE 'NJ'         TO BRAD-KDVAT                              
114300           END-IF                                                         
114400         END-IF                                                           
114500        ELSE                                                              
114600         IF DCS-DDC AND DCS-GERMANY                                       
114700         OR DCS-SDC AND DCS-GERMANY                                       
114800           IF DIST42-EU                                                   
114900             MOVE 'VI'         TO BRAD-KDVAT                              
115000           ELSE                                                           
115100             MOVE 'VJ'         TO BRAD-KDVAT                              
115200           END-IF                                                         
115300         ELSE                                                             
115400           IF DCS-DDC AND DCS-BELGIUM                                     
115500           OR DCS-SDC AND DCS-BELGIUM                                     
115600             IF DIST34-BELGIEN-DDC                                        
115700               MOVE 'BD'       TO BRAD-KDVAT                              
115800             ELSE                                                         
115900               IF DIST42-EU                                               
116000                 MOVE 'BQ'     TO BRAD-KDVAT                              
116100               ELSE                                                       
116200                 MOVE 'BX'     TO BRAD-KDVAT                              
116300               END-IF                                                     
116400             END-IF                                                       
116500           ELSE                                                           
116600            IF DCS-DDC AND DCS-FRANCE                                     
116700              IF DIST34-FRANKRIKE-DDC                                     
116800                MOVE 'F3'       TO BRAD-KDVAT                             
116900              ELSE                                                        
117000                IF DIST42-EU                                              
117100                  MOVE 'F4'     TO BRAD-KDVAT                             
117200                ELSE                                                      
117300                  MOVE 'F5'     TO BRAD-KDVAT                             
117400                END-IF                                                    
117500              END-IF                                                      
117600            ELSE                                                          
117700             IF LDC-CH                                                    
117800               MOVE 'XX'       TO BRAD-KDVAT                              
117900             ELSE                                                         
118000              IF DCS-DDC AND DCS-FINLAND                                  
118100              OR DCS-SDC AND DCS-FINLAND                                  
118200                IF DIST34-FINLAND-DDC                                     
118300                OR DIST34-FINLAND-LDC                                     
118400                  MOVE '48'      TO BRAD-KDVAT                            
118500                ELSE                                                      
118600                  IF DIST42-EU                                            
118700                    MOVE '70'    TO BRAD-KDVAT                            
118800                  ELSE                                                    
118900                    MOVE '90'    TO BRAD-KDVAT                            
119000                  END-IF                                                  
119100                END-IF                                                    
119200              ELSE                                                        
119300               IF DCS-DDC AND DCS-POLAND                                  
119400               OR DCS-SDC AND DCS-POLAND                                  
119500                 IF DIST34-POLAND-DDC                                     
119600                 OR DIST34-POLAND-LDC                                     
119700                   MOVE 'P2'      TO BRAD-KDVAT                           
119800                 ELSE                                                     
119900                   IF DIST42-EU                                           
120000                     MOVE 'PC'    TO BRAD-KDVAT                           
120100                   ELSE                                                   
120200                     MOVE 'PD'    TO BRAD-KDVAT                           
120300                   END-IF                                                 
120400                 END-IF                                                   
120500               ELSE                                                       
120600                IF DCS-DDC AND DCS-AUSTRALIA                              
120700                 IF DIST34-AUSTRALIA-DDC                                  
120800                   MOVE '90'     TO BRAD-KDVAT                            
120900                 ELSE                                                     
121000                   IF DIST42-EU                                           
121100                     MOVE '90' TO BRAD-KDVAT                              
121200                   ELSE                                                   
121300                     MOVE '90' TO BRAD-KDVAT                              
121400                   END-IF                                                 
121500                 END-IF                                                   
121600                ELSE                                                      
121700                  IF DCS-IDLANDX2 = 'GB'                                  
121800                    IF DCS-DDC AND DCS-ENGLAND                            
121900                    OR DCS-SDC AND DCS-ENGLAND                            
122000                      IF DIST34-ENGLAND-DDC                               
122100                      OR DIST34-ENGLAND-LDC                               
122200                      OR DIST34-ENGLAND-SDC                               
122300                        MOVE 'G7' TO BRAD-KDVAT                           
122400                      ELSE                                                
122500*** SENDING FROM GB TO EU                                                 
122600                       IF DIST42-EU                                       
122700                         MOVE 'G9' TO BRAD-KDVAT                          
122800                       ELSE                                               
122900                         MOVE '90' TO BRAD-KDVAT                          
123000                       END-IF                                             
123100                     END-IF                                               
123200                    END-IF                                                
123300                  ELSE                                                    
123400                    IF DIST42-EU                                          
123500                      IF DIST03-SVERIGE                                   
123600                        MOVE '21' TO BRAD-KDVAT                           
123700                      ELSE                                                
123800                        MOVE '70' TO BRAD-KDVAT                           
123900                      END-IF                                              
124000                    ELSE                                                  
124100                      MOVE '90'     TO BRAD-KDVAT                         
124200                      IF DCS-NDC-PF                                       
124300                        MOVE 'XX' TO BRAD-KDVAT                           
124400                        IF DCS-AUSTRALIA AND DIST34-JAPAN-NDC             
124500                        OR                                                
124600                        DCS-JAPAN     AND DIST34-AUSTRALIA-NDC            
124700                        OR                                                
124800                        DIST35-PACIFIC-TRANSFER                           
124900                        OR                                                
125000                        DIST35-REFILL-INOM-JP                             
125100                          MOVE '90' TO BRAD-KDVAT                         
125200                      END-IF                                              
125300                      IF DCS-THAILAND                                     
125400                        MOVE 'XZ' TO BRAD-KDVAT                           
125500                      END-IF                                              
125600                    END-IF                                                
125700                  END-IF                                                  
125800                 END-IF                                                   
125900                END-IF                                                    
126000               END-IF                                                     
126100              END-IF                                                      
126200             END-IF                                                       
126300            END-IF                                                        
126400           END-IF                                                         
126500         END-IF                                                           
126600       END-IF                                                             
126700      END-IF                                                              
126800     ELSE                                                                 
126900       EVALUATE TRUE                                                      
127000       WHEN DCS-CDC OR DCS-SDC AND DCS-SWEDEN                             
127100          MOVE '21'          TO BRAD-KDVAT                                
127200       WHEN DCS-SDC AND DCS-HOLLAND                                       
127300          MOVE 'NZ'          TO BRAD-KDVAT                                
127400       WHEN DCS-DDC AND DCS-ENGLAND                                       
127500          MOVE 'G7'          TO BRAD-KDVAT                                
127600       WHEN DCS-SDC AND DCS-ENGLAND                                       
127700          MOVE 'G7'          TO BRAD-KDVAT                                
127800       WHEN DCS-SDC AND DCS-SPAIN                                         
127900          MOVE 'S4'          TO BRAD-KDVAT                                
128000       WHEN DCS-SDC AND DCS-ITALY                                         
128100          MOVE 'IC'          TO BRAD-KDVAT                                
128200       WHEN DCS-SDC AND DCS-AUSTRIA                                       
128300          MOVE 'A2'          TO BRAD-KDVAT                                
128400       WHEN DCS-NDC-PF AND DCS-JAPAN                                      
128500          MOVE 'J4'          TO BRAD-KDVAT                                
128600       WHEN DCS-NDC-PF AND DCS-AUSTRALIA                                  
128700          MOVE '90'          TO BRAD-KDVAT                                
128800       WHEN DCS-DDC AND DCS-SWEDEN                                        
128900          MOVE '21'          TO BRAD-KDVAT                                
129000       WHEN DCS-DDC AND DCS-NORWAY                                        
129100          MOVE 'Y1'          TO BRAD-KDVAT                                
129200       WHEN DCS-SDC AND DCS-NORWAY                                        
129300          MOVE 'Y1'          TO BRAD-KDVAT                                
129400       WHEN DCS-DDC AND DCS-BELGIUM                                       
129500          MOVE 'BD'          TO BRAD-KDVAT                                
129600       WHEN DCS-SDC AND DCS-BELGIUM                                       
129700          MOVE 'BD'          TO BRAD-KDVAT                                
129800       WHEN DCS-DDC AND DCS-GERMANY                                       
129900          MOVE 'VE'          TO BRAD-KDVAT                                
130000       WHEN DCS-SDC AND DCS-GERMANY                                       
130100          MOVE 'VE'          TO BRAD-KDVAT                                
130200       WHEN DCS-DDC AND DCS-FRANCE                                        
130300          MOVE 'F3'          TO BRAD-KDVAT                                
130400       WHEN DCS-SDC AND DCS-FRANCE                                        
130500          MOVE 'F3'          TO BRAD-KDVAT                                
130600       WHEN DCS-DDC AND DCS-FINLAND                                       
130700          MOVE '48'          TO BRAD-KDVAT                                
130800       WHEN DCS-SDC AND DCS-FINLAND                                       
130900          MOVE '48'          TO BRAD-KDVAT                                
131000       WHEN DCS-DDC AND DCS-AUSTRALIA                                     
131100          MOVE '90'          TO BRAD-KDVAT                                
131200*      WHEN DCS-DDC AND DCS-HUNGARY                                       
131300*         MOVE '??'          TO BRAD-KDVAT                                
131400       WHEN DCS-DDC AND DCS-POLAND                                        
131500          MOVE 'P2'          TO BRAD-KDVAT                                
131600       WHEN DCS-SDC AND DCS-POLAND                                        
131700          MOVE 'P2'          TO BRAD-KDVAT                                
131800       WHEN LDC-CH                                                        
131900          MOVE 'XX'          TO BRAD-KDVAT                                
132000       END-EVALUATE                                                       
132100       IF BGMT-IDDISTR = 1558                                             
132200       OR BGMT-IDDISTR = 1578                                             
132300        MOVE 'ID'            TO BRAD-KDVAT                                
132400       END-IF                                                             
132500     END-IF                                                               
132600     PERFORM S20B-VAT-ADAPTION                                            
132700     .                                                                    
132800                                                                          
132900 S20A-LAS-WDB3  SECTION.                                                  
133000     MOVE 'S20A-LAS-WDB3'  TO WS-SEKTION.                                 
133100                                                                          
133200**   MOVE BGMT-IDDISTR         TO W-IDDISTR                               
133300     MOVE BGMT-IDDISTR         TO W-IDDISTR-B3                            
133400                                  W-IDDISTR-B3-DEF                        
133500                                  TEST-IDDISTR                            
133600***  MOVE BGMT-IDKUNDNR        TO W-IDKUNDNR                              
133700     MOVE BGMT-IDKUNDNR        TO W-IDKUNDNR-B3                           
133800     MOVE BILL-IDDC            TO W-IDDC-B3                               
133900     MOVE BILL-IDDC            TO W-IDDC-B3-DEF                           
134000     PERFORM IMS-GU-WDB301                                                
134100     IF SEGMENT-SAKNAS                                                    
134200***     MOVE ZERO              TO DC-KDFORSKN                             
134300***                               DC-KDSPFKTK                             
134400        MOVE ZERO              TO DC-KDMOMSIN                             
134500        MOVE BILL-IDDC         TO WC-IDDC                                 
134600                                  WS-IDDC                                 
134700        IF WS-IDDC NOT = W-IDDC-B6                                        
134800           MOVE WS-IDDC TO W-IDDC-B6                                      
134900           PERFORM IMS-GU-WDB601                                          
135000        END-IF                                                            
135100                                                                          
135200        IF DCS-DDC                                                        
135300          MOVE WS-CDC-11       TO W-IDDC-B3                               
135400                                  W-IDDC-B3-DEF                           
135500          PERFORM IMS-GU-WDB301                                           
135600        END-IF                                                            
135700        PERFORM S20AA-KDMOMSIN                                            
135800     ELSE                                                                 
135900        MOVE DC-IDDC           TO WC-IDDC                                 
136000        MOVE DC-KDMOMSIN       TO WC-KDMOMSIN                             
136100     END-IF                                                               
136200     .                                                                    
136300                                                                          
136400 S20B-VAT-ADAPTION SECTION.                                               
136500**** TEMPORARY SOLN FOR D25 COMPOUND - GB, BE, ES                         
136600     MOVE BGMT-IDDISTR       TO TEST-IDDISTR                              
136700     IF DIST28-CDC-GB25 OR DIST28-CDC-NO25                                
136800       MOVE '90'             TO BRAD-KDVAT                                
136900     END-IF                                                               
137000     IF DIST28-CDC-BE25 OR DIST28-CDC-ES25                                
137100       MOVE '70'             TO BRAD-KDVAT                                
137200     END-IF                                                               
137300                                                                          
137400**** SHOULD ONLY BE FOR DDGS                                              
137500     IF DCS-DDC                                                           
137600       MOVE BGMT-IDDISTR       TO W-IDDISTR-WDB2                          
137700       MOVE BGMT-IDKUNDNR      TO W-IDKUNDNR-WDB2                         
137800       PERFORM IMS-GU-WDB201                                              
137900       MOVE GMT-IDPARTNR TO W-WDB1-IDPARTNR                               
138000       MOVE GMT-IDFTG    TO W-WDB1-IDFTG                                  
138100       PERFORM IMS-GU-WDB101                                              
138200**** RECEIVER COUNTRY SHOULD BE IN EUROPE                                 
138300       MOVE BET-IDLANDX2       TO LANDX2-IDLANDX2                         
138400       IF LANDX2-EU-IDLANDX2                                              
138500**** SENDING COUNTRY SHOULD BE IN EUROPE                                  
138600         MOVE DCS-IDLANDX2     TO LANDX2-IDLANDX2                         
138700         IF LANDX2-EU-IDLANDX2                                            
138800           IF BET-FLDIRVAT = 'J'                                          
138900             IF BET-IDLANDX2 = 'AT'                                       
139000               MOVE 'A2' TO BRAD-KDVAT                                    
139100             END-IF                                                       
139200             IF BET-IDLANDX2 = 'BE'                                       
139300               MOVE 'BD' TO BRAD-KDVAT                                    
139400             END-IF                                                       
139500             IF BET-IDLANDX2 = 'DE'                                       
139600               MOVE 'VE' TO BRAD-KDVAT                                    
139700             END-IF                                                       
139800             IF BET-IDLANDX2 = 'ES'                                       
139900               MOVE 'S4' TO BRAD-KDVAT                                    
140000             END-IF                                                       
140100             IF BET-IDLANDX2 = 'FI'                                       
140200               MOVE '48' TO BRAD-KDVAT                                    
140300             END-IF                                                       
140400             IF BET-IDLANDX2 = 'FR'                                       
140500               MOVE 'F3' TO BRAD-KDVAT                                    
140600             END-IF                                                       
140700             IF BET-IDLANDX2 = 'IT'                                       
140800               MOVE 'IC' TO BRAD-KDVAT                                    
140900             END-IF                                                       
141000             IF BET-IDLANDX2 = 'NL'                                       
141100               MOVE 'NZ' TO BRAD-KDVAT                                    
141200             END-IF                                                       
141300             IF BET-IDLANDX2 = 'PL'                                       
141400               MOVE 'P2' TO BRAD-KDVAT                                    
141500             END-IF                                                       
141600           END-IF                                                         
141700         END-IF                                                           
141800       END-IF                                                             
141900     END-IF                                                               
142000     .                                                                    
142100                                                                          
142200 S20AA-KDMOMSIN   SECTION.                                                
142300     MOVE 'S20AA-KDMOMSIN'        TO WS-SEKTION                           
142400                                                                          
142500     MOVE BILL-IDDC                  TO WS-IDDC                           
142600     MOVE BGMT-IDDISTR               TO TEST-IDDISTR                      
142700     MOVE +2                         TO WC-KDMOMSIN                       
142800     EVALUATE TRUE                                                        
142900        WHEN DCS-DDC AND DCS-SWEDEN                                       
143000           IF DIST34-SVERIGE-DDC                                          
143100              MOVE +1                TO WC-KDMOMSIN                       
143200           END-IF                                                         
143300        WHEN DCS-DDC AND DCS-NORWAY                                       
143400           IF DIST34-NORGE-DDC                                            
143500              MOVE +1                TO WC-KDMOMSIN                       
143600           END-IF                                                         
143700        WHEN DCS-DDC AND DCS-FINLAND                                      
143800           IF DIST34-FINLAND-DDC                                          
143900              MOVE +1                TO WC-KDMOMSIN                       
144000           END-IF                                                         
144100        WHEN DCS-DDC AND DCS-KOREA                                        
144200           IF DIST34-KOREA-DDC                                            
144300              MOVE +1                TO WC-KDMOMSIN                       
144400           END-IF                                                         
144500        WHEN DCS-DDC AND DCS-BELGIUM                                      
144600           IF DIST34-BELGIEN-DDC                                          
144700              MOVE +1                TO WC-KDMOMSIN                       
144800           END-IF                                                         
144900        WHEN DCS-DDC AND DCS-GERMANY                                      
145000           IF DIST34-TYSKLAND-DDC                                         
145100              MOVE +1                TO WC-KDMOMSIN                       
145200           END-IF                                                         
145300        WHEN DCS-DDC AND DCS-FRANCE                                       
145400           IF DIST34-FRANKRIKE-DDC                                        
145500              MOVE +1                TO WC-KDMOMSIN                       
145600           END-IF                                                         
145700        WHEN DCS-DDC AND DCS-POLAND                                       
145800           IF DIST34-POLAND-DDC                                           
145900              MOVE +1                TO WC-KDMOMSIN                       
146000           END-IF                                                         
146100        WHEN DCS-DDC AND DCS-ENGLAND                                      
146200           IF DIST34-ENGLAND-DDC                                          
146300              MOVE +1                TO WC-KDMOMSIN                       
146400           END-IF                                                         
146500*       WHEN DCS-DDC AND DCS-TURKEY                                       
146600*          IF DIST34-TURKEY-DDC                                           
146700*             MOVE +1                TO WC-KDMOMSIN                       
146800*          END-IF                                                         
146900*       WHEN DCS-DDC AND DCS-HUNGARY                                      
147000*          IF DIST34-HUNGARY-DDC                                          
147100*             MOVE +1                TO WC-KDMOMSIN                       
147200*          END-IF                                                         
147300*       WHEN DCS-DDC AND DCS-MAROCKO                                      
147400*          IF DIST34-MAROCKO-DDC                                          
147500*             MOVE +1                TO WC-KDMOMSIN                       
147600*          END-IF                                                         
147700     END-EVALUATE                                                         
147800     .                                                                    
147900     EJECT                                                                
148000* --- IMS SEKTIONER ---                                                   
148100 IMS-GET-MSG  SECTION.                                                    
148200     MOVE    '  QC'          TO    GODK-STATUSKODER                       
148300     CALL    CBLTDLI         USING GU   MSG-PCB MSG-IO-AREA               
148400     MOVE    MSG-STATUS-CODE TO    STATUS-WS                              
148500     PERFORM IMS-STATUSKONTROLL                                           
148600     .                                                                    
148700     SKIP2                                                                
148800                                                                          
148900*IMS-PURGE-ALTMSG-0606 SECTION.                                           
149000*    MOVE SPACE TO GODK-STATUSKODER                                       
149100*    CALL CBLTDLI USING PURG ALT0606-PCB MSGSOP-WMSGSOP                   
149200*    MOVE ALT0606-STATUS-CODE TO STATUS-WS                                
149300*    PERFORM IMS-STATUSKONTROLL                                           
149400*    .                                                                    
149500*    SKIP3                                                                
149600 IMS-ISRT-ALT-MSG-4638 SECTION.                                           
149700                                                                          
149800     MOVE SPACE TO GODK-STATUSKODER                                       
149900     CALL CBLTDLI USING ISRT 4638-PCB P-TO-P-SW                           
150000     MOVE 4638-STATUS-CODE TO STATUS-WS                                   
150100     PERFORM IMS-STATUSKONTROLL                                           
150200     .                                                                    
150300     SKIP2                                                                
150400     EJECT                                                                
150500 IMS-GU-WDB301 SECTION.                                                   
150600     MOVE 'IMS-GU-WDB301'   TO WS-SEKTION                                 
150700                                                                          
150800     STRING 'WDB301  (WDB301KY =' W-WDB301KY-X                            
150900                    '+WDB301KY =' W-WDB301KY-DEF-X ')'                    
151000          DELIMITED BY SIZE INTO SSA1                                     
151100     MOVE '  GE' TO GODK-STATUSKODER                                      
151200     CALL CBLTDLI USING GU WDB3-PCB DLI-IO-WDB301 SSA1                    
151300     MOVE WDB3-STATUS-CODE TO STATUS-WS                                   
151400     PERFORM IMS-STATUSKONTROLL                                           
151500     .                                                                    
151600 IMS-GU-WDE111 SECTION.                                                   
151700     MOVE 'IMS-GU-WDE111'      TO WS-SEKTION                              
151800                                                                          
151900     STRING 'WDE101  (IDSHIPM  =' W-IDSHIPM-X ')'                         
152000          DELIMITED BY SIZE INTO SSA1                                     
152100     STRING 'WDE111  (WDE111KY =' W-WDE111KY-X ')'                        
152200          DELIMITED BY SIZE INTO SSA2                                     
152300     MOVE '  GE' TO GODK-STATUSKODER                                      
152400     CALL CBLTDLI USING GU WDE1-PCB DLI-IO-WDE111 SSA1 SSA2               
152500     MOVE WDE1-STATUS-CODE TO STATUS-WS                                   
152600     PERFORM IMS-STATUSKONTROLL                                           
152700     .                                                                    
152800     EJECT                                                                
152900 IMS-GHU-WDE121 SECTION.                                                  
153000     MOVE 'IMS-GHU-WDE121'      TO WS-SEKTION                             
153100                                                                          
153200     STRING 'WDE101  (IDSHIPM  =' W-IDSHIPM-X ')'                         
153300          DELIMITED BY SIZE INTO SSA1                                     
153400     STRING 'WDE111  (WDE111KY =' W-WDE111KY-X ')'                        
153500          DELIMITED BY SIZE INTO SSA2                                     
153600     STRING 'WDE121  (WDE121KY =' W-WDE121KY-X ')'                        
153700          DELIMITED BY SIZE INTO SSA3                                     
153800     MOVE '  GE' TO GODK-STATUSKODER                                      
153900     CALL CBLTDLI USING GHU WDE1-PCB DLI-IO-WDE121 SSA1 SSA2 SSA3         
154000     MOVE WDE1-STATUS-CODE TO STATUS-WS                                   
154100     PERFORM IMS-STATUSKONTROLL                                           
154200     .                                                                    
154300     EJECT                                                                
154400 IMS-REPL-WDE121 SECTION.                                                 
154500     MOVE 'IMS-REPL-WDE121'      TO WS-SEKTION                            
154600                                                                          
154700     MOVE '    '   TO GODK-STATUSKODER                                    
154800     CALL CBLTDLI USING REPL WDE1-PCB DLI-IO-WDE121                       
154900     MOVE WDE1-STATUS-CODE TO STATUS-WS                                   
155000     PERFORM IMS-STATUSKONTROLL                                           
155100     .                                                                    
155200     EJECT                                                                
155300 IMS-GHU-WDE131 SECTION.                                                  
155400     MOVE 'IMS-GHU-WDE131'      TO WS-SEKTION                             
155500                                                                          
155600     STRING 'WDE101  (IDSHIPM  =' W-IDSHIPM-X ')'                         
155700          DELIMITED BY SIZE INTO SSA1                                     
155800     STRING 'WDE111  (WDE111KY =' W-WDE111KY-X ')'                        
155900          DELIMITED BY SIZE INTO SSA2                                     
156000     STRING 'WDE121  (WDE121KY =' W-WDE121KY-X ')'                        
156100          DELIMITED BY SIZE INTO SSA3                                     
156200     STRING 'WDE131  (IDPURAD  =' W-IDPURAD-X ')'                         
156300          DELIMITED BY SIZE INTO SSA4                                     
156400     MOVE '  GE'    TO GODK-STATUSKODER                                   
156500     CALL CBLTDLI USING GHU WDE1-PCB DLI-IO-WDE131 SSA1 SSA2              
156600                                                   SSA3 SSA4              
156700     MOVE WDE1-STATUS-CODE TO STATUS-WS                                   
156800     PERFORM IMS-STATUSKONTROLL                                           
156900     .                                                                    
157000     EJECT                                                                
157100 IMS-REPL-WDE131 SECTION.                                                 
157200     MOVE 'IMS-REPL-WDE131'      TO WS-SEKTION                            
157300                                                                          
157400     MOVE '    '   TO GODK-STATUSKODER                                    
157500     CALL CBLTDLI USING REPL WDE1-PCB DLI-IO-WDE131                       
157600     MOVE WDE1-STATUS-CODE TO STATUS-WS                                   
157700     PERFORM IMS-STATUSKONTROLL                                           
157800     .                                                                    
157900     EJECT                                                                
158000 IMS-GNP-WDE201 SECTION.                                                  
158100     MOVE 'IMS-GNP-WDE201'      TO WS-SEKTION                             
158200                                                                          
158300     STRING 'WDE221  (WDE221KY =' W-WDE121KY-X ')'                        
158400          DELIMITED BY SIZE INTO SSA1                                     
158500     STRING 'WDE211  (WDE211KY =' W-WDE111KY-X ')'                        
158600          DELIMITED BY SIZE INTO SSA2                                     
158700     MOVE 'WDE201  '            TO SSA3                                   
158800     MOVE '  GE'     TO GODK-STATUSKODER                                  
158900     CALL CBLTDLI USING GHNP WDE2-PCB DLI-IO-WDE201 SSA1 SSA2 SSA3        
159000     MOVE WDE2-STATUS-CODE TO STATUS-WS                                   
159100     PERFORM IMS-STATUSKONTROLL                                           
159200     .                                                                    
159300     EJECT                                                                
159400 IMS-GNP-WDE211 SECTION.                                                  
159500     MOVE 'IMS-GNP-WDE211'      TO WS-SEKTION                             
159600                                                                          
159700     STRING 'WDE221  (WDE221KY =' W-WDE121KY-X ')'                        
159800          DELIMITED BY SIZE INTO SSA1                                     
159900     MOVE 'WDE211  '            TO SSA2                                   
160000     MOVE '  GE' TO GODK-STATUSKODER                                      
160100     CALL CBLTDLI USING GNP WDE2-PCB DLI-IO-WDE211 SSA1 SSA2              
160200     MOVE WDE2-STATUS-CODE TO STATUS-WS                                   
160300     PERFORM IMS-STATUSKONTROLL                                           
160400     .                                                                    
160500     EJECT                                                                
160600 IMS-GHU-WDE221-ASEQ  SECTION.                                            
160700     MOVE 'IMS-GHU-WDE221-ASEQ'  TO WS-SEKTION                            
160800                                                                          
160900     STRING 'WDE221  (WDE2ASEQ =' W-WDE121KY-X ')'                        
161000          DELIMITED BY SIZE INTO SSA1                                     
161100     MOVE '  GE' TO GODK-STATUSKODER                                      
161200     CALL CBLTDLI USING GHU WDE2-PCB DLI-IO-WDE221 SSA1                   
161300     MOVE WDE2-STATUS-CODE TO STATUS-WS                                   
161400     PERFORM IMS-STATUSKONTROLL                                           
161500     .                                                                    
161600     EJECT                                                                
161700 IMS-REPL-WDE221 SECTION.                                                 
161800     MOVE 'IMS-REPL-WDE221'      TO WS-SEKTION                            
161900                                                                          
162000     MOVE '    '   TO GODK-STATUSKODER                                    
162100     CALL CBLTDLI USING REPL WDE2-PCB DLI-IO-WDE221                       
162200     MOVE WDE2-STATUS-CODE TO STATUS-WS                                   
162300     PERFORM IMS-STATUSKONTROLL                                           
162400     .                                                                    
162500     EJECT                                                                
162600 IMS-GHN-WDE2ASEQ SECTION.                                                
162700     MOVE 'IMS-GN-WDE2ASEQ'     TO WS-SEKTION                             
162800                                                                          
162900     STRING 'WDE221  (WDE2ASEQ>=' W-WDE2ASEQ-MIN-X                        
163000                    '&WDE2ASEQ<=' W-WDE2ASEQ-MAX-X ')'                    
163100          DELIMITED BY SIZE INTO SSA1                                     
163200     MOVE '  GEGB'   TO GODK-STATUSKODER                                  
163300     CALL CBLTDLI USING GHN WDE2-PCB DLI-IO-WDE221 SSA1                   
163400     MOVE WDE2-STATUS-CODE TO STATUS-WS                                   
163500     PERFORM IMS-STATUSKONTROLL                                           
163600     .                                                                    
163700     EJECT                                                                
163800 IMS-REPL-WDE231 SECTION.                                                 
163900     MOVE 'IMS-REPL-WDE231'      TO WS-SEKTION                            
164000                                                                          
164100     MOVE '    '   TO GODK-STATUSKODER                                    
164200     CALL CBLTDLI USING REPL WDE2-PCB DLI-IO-WDE231                       
164300     MOVE WDE2-STATUS-CODE TO STATUS-WS                                   
164400     PERFORM IMS-STATUSKONTROLL                                           
164500     .                                                                    
164600     EJECT                                                                
164700 IMS-GHNP-WDE231 SECTION.                                                 
164800     MOVE 'IMS-GHNP-WDE231'      TO WS-SEKTION                            
164900                                                                          
165000     STRING 'WDE221  (WDE221KY =' W-WDE121KY-X ')'                        
165100          DELIMITED BY SIZE INTO SSA1                                     
165200     MOVE 'WDE231  '            TO SSA2                                   
165300     MOVE '  GE' TO GODK-STATUSKODER                                      
165400     CALL CBLTDLI USING GHNP WDE2-PCB DLI-IO-WDE231 SSA1 SSA2             
165500     MOVE WDE2-STATUS-CODE TO STATUS-WS                                   
165600     PERFORM IMS-STATUSKONTROLL                                           
165700     .                                                                    
165800     EJECT                                                                
165900 IMS-GHNP-WDE401  SECTION.                                                
166000     MOVE 'IMS-GHNP-WDE401'      TO WS-SEKTION                            
166100                                                                          
166200     MOVE 'WDE401'        TO SSA1                                         
166300     MOVE '    ' TO GODK-STATUSKODER                                      
166400     CALL CBLTDLI USING GHNP WDE4-PCB DLI-IO-WDE401 SSA1                  
166500     MOVE WDE4-STATUS-CODE TO STATUS-WS                                   
166600     PERFORM IMS-STATUSKONTROLL                                           
166700     .                                                                    
166800     SKIP3                                                                
166900 IMS-REPL-WDE401 SECTION.                                                 
167000     MOVE 'IMS-REPL-WDE401'      TO WS-SEKTION                            
167100                                                                          
167200     MOVE '    '   TO GODK-STATUSKODER                                    
167300     CALL CBLTDLI USING REPL WDE4-PCB DLI-IO-WDE401                       
167400     MOVE WDE4-STATUS-CODE TO STATUS-WS                                   
167500     PERFORM IMS-STATUSKONTROLL                                           
167600     .                                                                    
167700     EJECT                                                                
167800 IMS-GHU-WDE411-BSEQ  SECTION.                                            
167900     MOVE 'IMS-GHU-WDE411-BSEQ'  TO WS-SEKTION                            
168000                                                                          
168100     STRING 'WDE411  (WDE4BSEQ =' W-WDE4BSEQ-X ')'                        
168200          DELIMITED BY SIZE INTO SSA1                                     
168300     MOVE '  GE' TO GODK-STATUSKODER                                      
168400     CALL CBLTDLI USING GHU WDE4-PCB DLI-IO-WDE411 SSA1                   
168500     MOVE WDE4-STATUS-CODE TO STATUS-WS                                   
168600     PERFORM IMS-STATUSKONTROLL                                           
168700     .                                                                    
168800     SKIP3                                                                
168900 IMS-REPL-WDE411 SECTION.                                                 
169000     MOVE 'IMS-REPL-WDE411'      TO WS-SEKTION                            
169100                                                                          
169200     MOVE '    '   TO GODK-STATUSKODER                                    
169300     CALL CBLTDLI USING REPL WDE4-PCB DLI-IO-WDE411                       
169400     MOVE WDE4-STATUS-CODE TO STATUS-WS                                   
169500     PERFORM IMS-STATUSKONTROLL                                           
169600     .                                                                    
169700     EJECT                                                                
169800 IMS-GHU-WDE601  SECTION.                                                 
169900                                                                          
170000     STRING 'WDE601  (IDPRODNR =' W-IDPRODNR-E6-X ')'                     
170100          DELIMITED BY SIZE INTO SSA1                                     
170200     MOVE '  GE' TO GODK-STATUSKODER                                      
170300     CALL CBLTDLI USING GHU WDE6-PCB DLI-IO-WDE601 SSA1                   
170400     MOVE WDE6-STATUS-CODE TO STATUS-WS                                   
170500     PERFORM IMS-STATUSKONTROLL                                           
170600     .                                                                    
170700     SKIP3                                                                
170800 IMS-REPL-WDE601  SECTION.                                                
170900                                                                          
171000     MOVE '    ' TO GODK-STATUSKODER                                      
171100     CALL CBLTDLI USING REPL WDE6-PCB DLI-IO-WDE601                       
171200     MOVE WDE6-STATUS-CODE TO STATUS-WS                                   
171300     PERFORM IMS-STATUSKONTROLL                                           
171400     .                                                                    
171500     SKIP3                                                                
171600 IMS-GHNP-WDE611  SECTION.                                                
171700     MOVE 'IMS-GHNP-WDE611'      TO WS-SEKTION                            
171800                                                                          
171900     STRING 'WDE611  (IDKOLLI  =' W-IDKOLLI-X ')'                         
172000          DELIMITED BY SIZE INTO SSA1                                     
172100     MOVE '  GE' TO GODK-STATUSKODER                                      
172200     CALL CBLTDLI USING GHNP WDE6-PCB DLI-IO-WDE611 SSA1                  
172300     MOVE WDE6-STATUS-CODE TO STATUS-WS                                   
172400     PERFORM IMS-STATUSKONTROLL                                           
172500     .                                                                    
172600     SKIP3                                                                
172700 IMS-REPL-WDE611  SECTION.                                                
172800                                                                          
172900     MOVE '    ' TO GODK-STATUSKODER                                      
173000     CALL CBLTDLI USING REPL WDE6-PCB DLI-IO-WDE611                       
173100     MOVE WDE6-STATUS-CODE TO STATUS-WS                                   
173200     PERFORM IMS-STATUSKONTROLL                                           
173300     .                                                                    
173400     SKIP3                                                                
173500 IMS-GHU-WDC711 SECTION.                                                  
173600     MOVE 'IMS-GHU-WDC711'      TO WS-SEKTION                             
173700                                                                          
173800     STRING 'WDC701  (WDC701KY =' W-WDC701KY-X ')'                        
173900          DELIMITED BY SIZE INTO SSA1                                     
174000     STRING 'WDC711  (IDPRQUES =' W-IDPRQUES-X ')'                        
174100          DELIMITED BY SIZE INTO SSA2                                     
174200     MOVE '  GE' TO GODK-STATUSKODER                                      
174300     CALL CBLTDLI USING GHU WDC7-PCB DLI-IO-WDC711 SSA1 SSA2              
174400     MOVE WDC7-STATUS-CODE TO STATUS-WS                                   
174500     PERFORM IMS-STATUSKONTROLL                                           
174600     .                                                                    
174700     SKIP3                                                                
174800 IMS-REPL-WDC711 SECTION.                                                 
174900                                                                          
175000     MOVE '  ' TO GODK-STATUSKODER                                        
175100     CALL CBLTDLI USING REPL WDC7-PCB DLI-IO-WDC711                       
175200     MOVE WDC7-STATUS-CODE TO STATUS-WS                                   
175300     PERFORM IMS-STATUSKONTROLL                                           
175400     .                                                                    
175500     EJECT                                                                
175600  IMS-GU-WDB601    SECTION.                                               
175700      STRING 'WDB601  (IDDC     =' W-IDDC-B6-X ')'                        
175800           DELIMITED BY SIZE INTO SSA1                                    
175900      MOVE '  GE' TO GODK-STATUSKODER                                     
176000      CALL CBLTDLI USING GU WDB6-PCB DLI-IO-AREA-B601 SSA1                
176100      MOVE WDB6-STATUS-CODE    TO STATUS-WS                               
176200      PERFORM IMS-STATUSKONTROLL                                          
176300      IF SEGMENT-SAKNAS                                                   
176400          MOVE SPACE TO DCS-KDDC                                          
176500      END-IF                                                              
176600      .                                                                   
176700                                                                          
176800 IMS-GU-WDB101              SECTION.                                      
176900     MOVE 'IMS-GU-WDB101'   TO WS-SEKTION                                 
177000                                                                          
177100     STRING 'WDB101  (WDB101KY =' W-WDB101KY-X ')'                        
177200          DELIMITED BY SIZE INTO SSA1                                     
177300     MOVE '    '              TO GODK-STATUSKODER                         
177400                                                                          
177500     CALL CBLTDLI USING GU WDB1-PCB DLI-IO-WDB101 SSA1                    
177600     MOVE WDB1-STATUS-CODE    TO STATUS-WS                                
177700     PERFORM IMS-STATUSKONTROLL                                           
177800     .                                                                    
177900     EJECT                                                                
178000                                                                          
178100 IMS-GU-WDB201              SECTION.                                      
178200     MOVE 'IMS-GU-WDB201'   TO WS-SEKTION                                 
178300                                                                          
178400     STRING 'WDB201  (IDGMT    =' W-IDGMT-X ')'                           
178500          DELIMITED BY SIZE INTO SSA1                                     
178600     MOVE '  GE'              TO GODK-STATUSKODER                         
178700                                                                          
178800     CALL CBLTDLI USING GU WDB2-PCB DLI-IO-WDB201 SSA1                    
178900     MOVE WDB2-STATUS-CODE    TO STATUS-WS                                
179000     PERFORM IMS-STATUSKONTROLL                                           
179100     .                                                                    
179200     EJECT                                                                
179300                                                                          
179400 IMS-STATUSKONTROLL SECTION.                                              
179500                                                                          
179600     SET STATUS-IX TO 1                                                   
179700     SEARCH GODK-STATUS                                                   
179800       AT END                                                             
179900         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
180000         DELIMITED BY SIZE INTO FELTEXT                                   
180100         CALL FELLOG                                                      
180200       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
180300         CONTINUE                                                         
180400     END-SEARCH                                                           
180500     .                                                                    
