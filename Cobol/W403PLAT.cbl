000100*COMPOPT STDSUB=YES                                                       
000200 ID DIVISION.                                                             
000300                                                                          
000400 PROGRAM-ID.     W403PLAT.                                                
000500 AUTHOR.         MARGARETA GABRIELSON.                                    
000600 DATE-WRITTEN.   96/05/06.                                                
000700 DATE-COMPILED.                                                           
000800                                                                          
000900*    FUNCTION:                                                            
001000*        SUB PROGRAM HANDLING TRANSPORT NUMBERS AND ADDRESSES             
001100*        THREE DIFFERENT CASES: BOOKING      KDCALL = 4                   
001200*                               SEARCHING    KDCALL = 0,1,2,6             
001300*                               UNBOOKING    KDCALL = 3,5                 
001400*                                                                         
001500*        THE PROGRAM READS     WLXXDM (WDR1)                              
001600*        THE PROGRAM UPDATES   WLXXDN (WDR1)                              
001700*        THE PROGRAM UPDATES   WLXXDP (WDR1)                              
001800*        THE PROGRAM UPDATES   WLXXDO (WDR1)                              
001900*        THE PROGRAM READS     WDE6C                                      
002000*        THE PROGRAM READS     WLGMTC (WDB5)                              
002100*        THE PROGRAM READS             WDB6                               
002200*                                                                         
002300*                                                                         
002400*                                                                         
002500     EJECT                                                                
002600 DATA DIVISION.                                                           
002700     SKIP2                                                                
002800 FILE SECTION.                                                            
002900     EJECT                                                                
003000 WORKING-STORAGE SECTION.                                                 
003100                                                                          
003200*    -- CHECKED BY WY2000                                                 
003300     SKIP3                                                                
003400 77  IDPGM                       PIC X(8)    VALUE 'W403PLAT'.            
003500 77  YES                         PIC X       VALUE 'Y'.                   
003600 77  NOO                         PIC X       VALUE 'N'.                   
003700 77  JA                          PIC X       VALUE 'J'.                   
003800 77  NEJ                         PIC X       VALUE 'N'.                   
003900 77  FEL                         PIC X       VALUE 'F'.                   
004000 77  OK                          PIC X       VALUE SPACE.                 
004100 77  WS-SDC-23                   PIC X(2)    VALUE '23'.                  
004200     SKIP2                                                                
004300*                                                                         
004400 01  TODAYS-DATE                 PIC 9(6)    VALUE ZERO.                  
004500 01  FILLER REDEFINES TODAYS-DATE.                                        
004600     03  TODAYS-DATE-YEAR        PIC 9(2).                                
004700     03  TODAYS-DATE-MONTH       PIC 9(2).                                
004800     03  TODAYS-DATE-DAY         PIC 9(2).                                
004900     EJECT                                                                
005000 01  GENERAL-SUBPROGRAM.                                                  
005100*                                                                         
005200     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
005300     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
005400     03  WADRBER                 PIC X(8)    VALUE 'WADRBER '.            
005500     SKIP2                                                                
005600 01  FILLER       PIC X(24) VALUE 'PARAMETRAR TILL WADRBER '.             
005700                                                                          
005800*01  -COPY WADRAREA                                                       
005900     EJECT                                                                
006000 01  ERRTEXT.                                                             
006100     03  FILLER                  PIC X(8)    VALUE 'ERRTEXT'.             
006200     03  ERRTEXT-STR             PIC X(72)   VALUE SPACE.                 
006300     EJECT                                                                
006400     SKIP3                                                                
006500*      --- VALID IDDC CODES                                               
006600*                                                                         
006700*01    -COPY WWDC99                                                       
006800*                                                                         
006900 01  TEST-IDDISTR                PIC S9(5)  COMP-3.                       
007000                                                                          
007100*01  FILLER -COPY WWDIST97          -RED TEST-IDDISTR.                    
007200     EJECT                                                                
007300 01  RAC-AREA-SLUT.                                                       
007400     03  RAC-ADCLGEO.                                                     
007500         05  RAC-IDDC             PIC X(2).                               
007600         05  RAC-ADFLGEO          PIC X(3)          VALUE 'RAC'.          
007700     03  RAC-ADFLOMR              PIC S9(3)  COMP-3 VALUE +999.           
007800     03  RAC-ADRUTNIV             PIC S9(3)  COMP-3 VALUE +1.             
007900     SKIP3                                                                
008000 01  DAG-AREA-SLASK.                                                      
008100     03  DAG-ADCLGEO.                                                     
008200         05  DAG-IDDC             PIC X(2).                               
008300         05  DAG-ADFLGEO          PIC X(3)          VALUE 'DAG'.          
008400     03  DAG-ADFLOMR              PIC S9(3)  COMP-3 VALUE +900.           
008500     03  DAG-ADRUTNIV             PIC S9(3)  COMP-3 VALUE +1.             
008600     SKIP3                                                                
008700 01  KVANT-AREA-SLASK.                                                    
008800     03  KVANT-ADCLGEO.                                                   
008900         05  KVANT-IDDC           PIC X(2).                               
009000         05  KVANT-ADFLGEO        PIC X(3)          VALUE 'RAC'.          
009100     03  KVANT-ADFLOMR            PIC S9(3)  COMP-3 VALUE +999.           
009200     03  KVANT-ADRUTNIV           PIC S9(3)  COMP-3 VALUE +1.             
009300     SKIP3                                                                
009400*                                                                         
009500 01  NDC-AREA-DEFAULT.                                                    
009600     03  NDC-ADCLGEO.                                                     
009700         05  NDC-IDDC          PIC X(2).                                  
009800         05  NDC-ADFLGEO       PIC X(3)          VALUE 'NDC'.             
009900     03  NDC-ADFLOMR           PIC S9(3)  COMP-3 VALUE ZERO.              
010000     03  NDC-ADRUTNIV          PIC S9(3)  COMP-3 VALUE ZERO.              
010100     SKIP3                                                                
010200 01  BILLIT-AREA-DEFAULT.                                                 
010300     03  BILLIT-IDTRPTNR          PIC S9(3)  COMP-3 VALUE +998.           
010400     03  BILLIT-ADCLGEO.                                                  
010500         05  BILLIT-IDDC          PIC X(2).                               
010600         05  BILLIT-ADFLGEO       PIC X(3)          VALUE 'BIL'.          
010700     03  BILLIT-ADFLOMR           PIC S9(3)  COMP-3 VALUE ZERO.           
010800     03  BILLIT-ADRUTNIV          PIC S9(3)  COMP-3 VALUE ZERO.           
010900     SKIP3                                                                
011000 01  FARLIG-ADFLOMR            PIC S9(3)  COMP-3 VALUE +950.              
011100     EJECT                                                                
011200******************************************************************        
011300**                                                              **        
011400**      ARBETSFÄLT                                              **        
011500**                                                              **        
011600******************************************************************        
011700 01  FILLER       PIC X(24) VALUE 'ARBETS-FÄLT PLATSSÖKNING'.             
011800                                                                          
011900 01  W-DIHMODUL                  PIC S9(3)   COMP-3.                      
012000 01  W-DIDMODUL                  PIC S9(3)   COMP-3.                      
012100 01  W-DIBMODUL                  PIC S9(3)   COMP-3.                      
012200     SKIP3                                                                
012300*- - - - - - - - - - - - - - BERÄKNINGSFÄLT                               
012400                                                                          
012500 01  W-DIMODUL                           PIC 9(5).                        
012600 01  BER-DIMODUL REDEFINES W-DIMODUL.                                     
012700     03  FILLER                          PIC 9(2).                        
012800     03  BER-HUNDRATAL-DIKOLLI           PIC 9.                           
012900     03  BER-TIOTAL-DIKOLLI              PIC 9.                           
013000     03  BER-ENTAL-DIKOLLI               PIC 9.                           
013100     SKIP3                                                                
013200 01  SPAR-AREA.                                                           
013300     03  SPAR-IDTRPTNR           PIC S9(3)  COMP-3  VALUE ZERO.           
013400     03  SPAR-IDDC               PIC X(2)           VALUE SPACE.          
013500     03  SPAR-ADVMODUL           PIC S9(3)  COMP-3  VALUE ZERO.           
013600     03  SPAR-ADHMODUL           PIC S9(3)  COMP-3  VALUE ZERO.           
013700     03  SPAR-TESPAERR           PIC X(20)          VALUE SPACE.          
013800     03  SPAR-TBSPAERR           PIC X(50)          VALUE SPACE.          
013900     SKIP3                                                                
014000 01  W-RUTA-TESPAERR.                                                     
014100     03  W-TRPT                  PIC X(4).                                
014200     03  FILLER                  PIC X.                                   
014300     03  W-TRPTNR                PIC X(3).                                
014400     03  FILLER                  PIC X.                                   
014500     03  W-TRPTDATUM             PIC X(6).                                
014600     03  FILLER                  PIC X.                                   
014700     03  W-FULL                  PIC X(4).                                
014800     EJECT                                                                
014900 01  FILLER       PIC X(24) VALUE 'ARBETS-FÄLT PLATSBOKNING'.             
015000                                                                          
015100 01  STAELL-SLUT-TEXT.                                                    
015200     03  STAELL-SLUT             PIC X(20) VALUE 'STAELL SLUT'.           
015300     EJECT                                                                
015400 01  W-INDEXAR.                                                           
015500     03  IX                      PIC S9(3) COMP-3.                        
015600     03  TAB-IX-PLUS-ETT         PIC S9(3) COMP-3.                        
015700     03  MAX-IX-PLUS-ETT         PIC S9(3) COMP-3  VALUE +21.             
015800     SKIP3                                                                
015900 01  WADRBER-TABELL.                                                      
016000     03  TABELL        OCCURS 20.                                         
016100         05  TAB-ADVMODUL        PIC S9(3)  COMP-3.                       
016200         05  TAB-ADHMODUL        PIC S9(3)  COMP-3.                       
016300     SKIP3                                                                
016400 01  FILLER                  PIC X(16) VALUE 'IMS-WS'.                    
016500     SKIP2                                                                
016600*    ---- STATUSKOD FRÅN IMS                                              
016700                                                                          
016800 01  STATUS-WS               PIC XX.                                      
016900     88  SEGMENT-FINNS                    VALUE '  '.                     
017000     88  SEGMENT-SAKNAS                   VALUE 'GE'.                     
017100     88  SEGMENT-SLUT                     VALUE 'GB'.                     
017200     SKIP2                                                                
017300 01  GODK-STATUSKODER.                                                    
017400   03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                  
017500     SKIP2                                                                
017600 01  SSA1                    PIC X(192).                                  
017700 01  SSA2                    PIC X(64).                                   
017800     EJECT                                                                
017900*                            IMS FUNKTIONSKODER                           
018000*01  -COPY W0003                                                          
018100     EJECT                                                                
018200 01  FILLER          PIC X(24)                                            
018300     EJECT                                                                
018400                     VALUE 'DLI-NYCKLAR'.                                 
018500*                                                                         
018600 01  NYCKLAR-TILL-DLI.                                                    
018700     03  W-4417-WDGXKEY-X.                                                
018800         05  W-4417-IDHTYP            PIC X(4)  VALUE '4417'.             
018900         05  W-4417-ADCLGEO.                                              
019000             07  W-4417-IDDC          PIC X(2).                           
019100             07  W-4417-ADFLGEO       PIC X(3)  VALUE 'RAC'.              
019200         05  W-4417-LOWVALUE          PIC X(21) VALUE LOW-VALUE.          
019300                                                                          
019400     03  W-4418-WDGXKEY-X.                                                
019500         05  W-4418-ADFLOMR           PIC S9(3) COMP-3.                   
019600         05  W-4418-ADRUTNIV          PIC S9(3) COMP-3.                   
019700         05  W-4418-DIHMODUL          PIC S9(3) COMP-3.                   
019800         05  W-4418-DIDMODUL          PIC S9(3) COMP-3.                   
019900         05  W-4418-LOWVALUE          PIC X(2)  VALUE LOW-VALUE.          
020000                                                                          
020100     03  W-4420-WDGXKEY-X.                                                
020200         05  W-4420-ADVMODUL          PIC S9(3) COMP-3.                   
020300         05  W-4420-LOWVALUE          PIC X(8)  VALUE LOW-VALUE.          
020400                                                                          
020500     03  W-4411-WDGXKEY-X.                                                
020600         05  W-4411-IDHTYP            PIC X(4)  VALUE '4411'.             
020700         05  W-4411-ADCLGEO.                                              
020800             07  W-4411-IDDC          PIC X(2).                           
020900             07  W-4411-ADFLGEO       PIC X(3).                           
021000         05  W-4411-LOWVALUE          PIC X(21) VALUE LOW-VALUE.          
021100                                                                          
021200     03  W-4412-WDGXKEY-X.                                                
021300         05  W-4412-ADFLOMR           PIC S9(3) COMP-3.                   
021400         05  W-4412-LOWVALUE          PIC X(8)  VALUE LOW-VALUE.          
021500                                                                          
021600     03  W-4414-WDGXKEY-X.                                                
021700         05  W-4414-ADRUTNIV          PIC S9(3) COMP-3.                   
021800         05  W-4414-LOWVALUE          PIC X(8)  VALUE LOW-VALUE.          
021900                                                                          
022000     03  W-4402-KY4402-X.                                                 
022100         05  W-4402-IDDC              PIC X(2).                           
022200         05  W-4402-IDDISTR           PIC S9(5) COMP-3.                   
022300         05  W-4402-IDKUNDNR          PIC S9(7) COMP-3.                   
022400         05  W-4402-KDFRAKT           PIC S9(3) COMP-3.                   
022500         05  W-4402-KDORDKLX          PIC X.                              
022600                                                                          
022700     03  W-4402-KY4402-MIN-X.                                             
022800         05  W-4402-IDDC-MIN          PIC X(2).                           
022900         05  W-4402-IDDISTR-MIN       PIC S9(5) COMP-3.                   
023000         05  FILLER                   PIC X(07) VALUE LOW-VALUE.          
023100                                                                          
023200     03  W-4402-KY4402-MAX-X.                                             
023300         05  W-4402-IDDC-MAX          PIC X(2).                           
023400         05  W-4402-IDDISTR-MAX       PIC S9(5) COMP-3.                   
023500         05  FILLER                   PIC X(07) VALUE HIGH-VALUE.         
023600                                                                          
023700     03  W-4406-WDGXKEY-X.                                                
023800         05  W-4406-IDTRPTNR         PIC S9(3)  COMP-3.                   
023900         05  W-4406-IDDC             PIC X(2).                            
024000         05  W-4406-LOWVALUE         PIC X(6)   VALUE LOW-VALUE.          
024100                                                                          
024200     03  W-4408-WDGXKEY-X.                                                
024300         05  W-4408-ADCLGEO.                                              
024400             07  W-4408-IDDC         PIC X(2).                            
024500             07  W-4408-ADFLGEO      PIC X(3).                            
024600         05  W-4408-ADFLOMR          PIC S9(3)  COMP-3.                   
024700         05  W-4408-ADRUTNIV         PIC S9(3)  COMP-3.                   
024800         05  W-4408-LOWVALUE         PIC X(1)   VALUE LOW-VALUE.          
024900     EJECT                                                                
025000     03  W-WDE6C1KY-X-MIN.                                                
025100         05  W-SEK-IDTRPTNR-MIN      PIC S9(3)  COMP-3.                   
025200         05  W-SEK-DARFS-MIN         PIC 9(12).                           
025300         05  W-SEK-ADCLGEO-MIN.                                           
025400             07 W-SEK-IDDC-MIN       PIC X(2).                            
025500             07 W-SEK-ADFLGEO-MIN    PIC X(3).                            
025600         05  W-SEK-ADFLOMR-MIN       PIC S9(3)  COMP-3.                   
025700         05  W-SEK-ADRUTNIV-MIN      PIC S9(3)  COMP-3.                   
025800         05  W-SEK-ADVMODUL-MIN      PIC S9(3)  COMP-3.                   
025900         05  W-SEK-IDDISTR-MIN       PIC S9(5)  COMP-3.                   
026000         05  W-SEK-IDKUNDNR-MIN      PIC S9(7)  COMP-3.                   
026100         05  W-SEK-IDPRODNR-MIN      PIC S9(7)  COMP-3.                   
026200         05  W-SEK-IDKOLLI-FLER-MIN PIC S9(5)   COMP-3.                   
026300         05  W-SEK-IDKOLLI-MIN       PIC S9(5)  COMP-3.                   
026400                                                                          
026500     03  W-WDE6C1KY-X-MAX.                                                
026600         05  W-SEK-IDTRPTNR-MAX      PIC S9(3)  COMP-3.                   
026700         05  W-SEK-DARFS-MAX         PIC 9(12).                           
026800         05  W-SEK-ADCLGEO-MAX.                                           
026900             07 W-SEK-IDDC-MAX       PIC X(2).                            
027000             07 W-SEK-ADFLGEO-MAX    PIC X(3).                            
027100         05  W-SEK-ADFLOMR-MAX       PIC S9(3)  COMP-3.                   
027200         05  W-SEK-ADRUTNIV-MAX      PIC S9(3)  COMP-3.                   
027300         05  W-SEK-ADVMODUL-MAX      PIC S9(3)  COMP-3.                   
027400         05  W-SEK-IDDISTR-MAX       PIC S9(5)  COMP-3.                   
027500         05  W-SEK-IDKUNDNR-MAX      PIC S9(7)  COMP-3.                   
027600         05  W-SEK-IDPRODNR-MAX      PIC S9(7)  COMP-3.                   
027700         05  W-SEK-IDKOLLI-FLER-MAX  PIC S9(5)  COMP-3.                   
027800         05  W-SEK-IDKOLLI-MAX       PIC S9(5)  COMP-3.                   
027900                                                                          
028000     03 W-SEK-ADCLGEO-X.                                                  
028100        05 W-SEK-IDDC                PIC X(2).                            
028200        05 W-SEK-ADFLGEO             PIC X(3).                            
028300     03  W-SEK-ADFLOMR-X.                                                 
028400        05  W-SEK-ADFLOMR            PIC S9(3)  COMP-3.                   
028500     03 W-SEK-ADRUTNIV-X.                                                 
028600        05  W-SEK-ADRUTNIV           PIC S9(3)  COMP-3.                   
028700     03  W-SEK-ADHMODUL-X.                                                
028800        05  W-SEK-ADHMODUL           PIC S9(3)  COMP-3.                   
028900                                                                          
029000    03 W-WDB501KY-X.                                                      
029100      05 W-KUND-IDDC               PIC X(2).                              
029200      05 W-KUND-KDFRAKT            PIC S9(3)   VALUE ZERO  COMP-3.        
029300      05     W-IDGMT-WDB5.                                                
029400        07 W-KUND-IDDISTR          PIC S9(5)   VALUE ZERO  COMP-3.        
029500        07 W-KUND-IDKUNDNR         PIC S9(7)   VALUE ZERO  COMP-3.        
029600*                                                                         
029700    03 W-WDB501KY-DEFAULT-X.                                              
029800       05 W-KUND-IDDC-DEFAULT      PIC X(2).                              
029900       05 W-KUND-KDFRAKT-DEFAULT   PIC S9(3) VALUE ZERO COMP-3.           
030000       05    W-IDGMT-WDB5-DEFAULT.                                        
030100        07 W-KUND-IDDISTR-DEFAULT  PIC S9(5) VALUE ZERO COMP-3.           
030200        07 W-KUND-IDKUNDNR-DEFAULT PIC S9(7) VALUE 9999999 COMP-3.        
030300*                                                                         
030400    03  W-IDDC-B6-X.                                                      
030500        05 W-IDDC-B6                  PIC X(2).                           
030600                                                                          
030700     EJECT                                                                
030800*01  -COPY WDGX01                                                         
030900     EJECT                                                                
031000 01  FILLER       PIC X(24) VALUE 'DLI I/O  PLATSSÖKNING   '.             
031100                                                                          
031200 01  DLI-IO-AREA.                                                         
031300   03  IO-AREA                  PIC X(150) VALUE SPACE.                   
031400*                                                                         
031500*  03  WLXXDP01  -COPY WDGX4417        -RED IO-AREA.                      
031600     EJECT                                                                
031700*  03  WLXXDP11  -COPY WDGX4418        -RED IO-AREA.                      
031800     EJECT                                                                
031900*  03  WLXXDP21  -COPY WDGX4420        -RED IO-AREA.                      
032000     EJECT                                                                
032100*  03  WLXXDO01  -COPY WDGX4411        -RED IO-AREA.                      
032200     EJECT                                                                
032300*  03  WLXXDO11  -COPY WDGX4412        -RED IO-AREA.                      
032400     EJECT                                                                
032500*  03  WLXXDO21  -COPY WDGX4414        -RED IO-AREA.                      
032600     EJECT                                                                
032700*  03  WLXXDM11  -COPY WDGX4402        -RED IO-AREA.                      
032800     EJECT                                                                
032900*  03  WLXXDN11  -COPY WDGX4406        -RED IO-AREA.                      
033000     EJECT                                                                
033100*  03  WLXXDN21  -COPY WDGX4408        -RED IO-AREA.                      
033200     EJECT                                                                
033300*  03  WDE6C1    -COPY WDE6C1          -RED IO-AREA.                      
033400     EJECT                                                                
033500*  03  WLGMTC01  -COPY WDB501          -RED IO-AREA.                      
033600     EJECT                                                                
033700 01  FILLER               PIC X(16)   VALUE 'WDB601 AREA'.                
033800 01   DLI-IO-AREA-B601.                                                   
033900*     03  -COPY WDB601                                                    
034000                                                                          
034100 LINKAGE SECTION.                                                         
034200                                                                          
034300*01  -COPY W403PLAT                                                       
034400     EJECT                                                                
034500*01  -COPY W0008  -PRE XXDM-                                              
034600     05  FILLER                  PIC X.                                   
034700     EJECT                                                                
034800*01  -COPY W0008  -PRE XXDN-                                              
034900     05  FILLER                  PIC X.                                   
035000     EJECT                                                                
035100*01  -COPY W0008  -PRE XXDP-                                              
035200     05  FILLER                  PIC X.                                   
035300     EJECT                                                                
035400*01  -COPY W0008  -PRE XXDO-                                              
035500     05  FILLER                  PIC X.                                   
035600     EJECT                                                                
035700*01  -COPY W0008  -PRE WDE6C-                                             
035800     05  FILLER                  PIC X.                                   
035900     EJECT                                                                
036000*01  -COPY W0008  -PRE GMTC-                                              
036100     05  FILLER                  PIC X.                                   
036200     EJECT                                                                
036300*01  -COPY W0008  -PRE WDB6-                                              
036400     05  FILLER                  PIC X.                                   
036500     EJECT                                                                
036600 PROCEDURE DIVISION  USING PLATS-W403PLAT                                 
036700                           XXDM-PCB                                       
036800                           XXDN-PCB                                       
036900                           XXDP-PCB                                       
037000                           XXDO-PCB                                       
037100                           WDE6C-PCB                                      
037200                           GMTC-PCB                                       
037300                           WDB6-PCB.                                      
037400                                                                          
037500 STYR SECTION.                                                            
037600                                                                          
037700     IF PLATS-KDCALL = 0 OR 1 OR 2 OR 6                                   
037800       PERFORM A-PLATSSOEKNING                                            
037900     ELSE                                                                 
038000       IF PLATS-KDCALL = 4                                                
038100         PERFORM B-PLATSBOKNING                                           
038200       ELSE                                                               
038300         IF PLATS-KDCALL = 3 OR 5                                         
038400           PERFORM C-PLATSAVBOKNING                                       
038500         END-IF                                                           
038600       END-IF                                                             
038700     END-IF                                                               
038800                                                                          
038900     GOBACK                                                               
039000     .                                                                    
039100     EJECT                                                                
039200 A-PLATSSOEKNING SECTION.                                                 
039300                                                                          
039400     MOVE PLATS-IDDC TO W-IDDC-B6                                         
039500     PERFORM IMS-GU-WDB601                                                
039900       IF PLATS-ADFLGEO = SPACE                                           
040000         PERFORM AA-TRANSPORT                                             
040100         IF SPAR-IDTRPTNR > ZERO                                          
040200           PERFORM AB-RUT-ADRESS                                          
040300         ELSE                                                             
040400*   OM TRANSPORT SAKNAS PÅ KOLLIT FÖR 'BILLIT-KOLLIN EJ NDC-NA'           
040500           PERFORM AH-HAEMTA-TRANSPORT-BILLIT                             
040600         END-IF                                                           
040700       ELSE                                                               
040800         PERFORM AF-KOLLA-GIVEN-ADRESS                                    
040900       END-IF                                                             
041100     .                                                                    
041200     EJECT                                                                
041300 AA-TRANSPORT SECTION.                                                    
041400                                                                          
041500     MOVE PLATS-IDDC        TO WS-IDDC                                    
041600     MOVE ZERO              TO SPAR-IDTRPTNR                              
041700                               SPAR-IDDC                                  
041800     PERFORM AAA-LAS-TRANSP-TAB                                           
041900     IF SEGMENT-FINNS                                                     
042000       MOVE PLATS-IDDISTR   TO TEST-IDDISTR                               
042100                                                                          
042200       IF DIST97-STYRNING                                                 
042300         IF DCS-FLKNDVAL = JA                                             
042400           MOVE PLATS-IDKUNDNR TO W-4402-IDKUNDNR                         
042500         ELSE                                                             
042603           IF (DIST97-STYRNING-2635 AND (CDC-SE OR LDC-FI))               
042702                                    OR                                    
042803              (DIST97-STYRNING-2638 AND (CDC-SE OR LDC-FI))               
042902             MOVE PLATS-IDKUNDNR TO W-4402-IDKUNDNR                       
043002           ELSE                                                           
043102             MOVE ZERO      TO W-4402-IDKUNDNR                            
043202           END-IF                                                         
043302         END-IF                                                           
043402                                                                          
043502       ELSE                                                               
043603         IF (DIST97-STYRNING-878     AND                                  
043703            (LDC-SE-1C OR LDC-NO-3J))                                     
043802                       OR                                                 
043902            (DIST97-STYRNING-1090 AND LDC-FI)                             
044001           MOVE PLATS-IDKUNDNR TO W-4402-IDKUNDNR                         
044101                                                                          
044201         ELSE                                                             
045101           IF (DIST97-STYRNING-878 OR DIST97-STYRNING-1090)               
045301                       AND                                                
045401              (DCS-KDDC = 'C ' OR 'D ')                                   
045501             MOVE ZERO           TO W-4402-IDKUNDNR                       
045601                                                                          
045701           ELSE                                                           
045801             IF DIST97-UNDANTAG-VOR AND (DCS-KDDC NOT = 'C ')             
045901               MOVE ZERO         TO W-4402-IDKUNDNR                       
046001             ELSE                                                         
046101               MOVE PLATS-IDKUNDNR TO W-4402-IDKUNDNR                     
046201             END-IF                                                       
046301                                                                          
046401           END-IF                                                         
046601         END-IF                                                           
046701       END-IF                                                             
046801                                                                          
046802       MOVE PLATS-IDDC      TO W-4402-IDDC                                
046803       MOVE PLATS-IDDISTR   TO W-4402-IDDISTR                             
046804       MOVE PLATS-KDFRAKT   TO W-4402-KDFRAKT                             
046805       MOVE PLATS-KDORDKLX  TO W-4402-KDORDKLX                            
046806       PERFORM IMS-LAS-TRANSP-TAB-FIRST                                   
046807       IF SEGMENT-FINNS                                                   
046808         MOVE 4402-IDTRPTNR      TO SPAR-IDTRPTNR                         
046809         MOVE 4402-IDDC          TO SPAR-IDDC                             
046810         MOVE 4402-IDDC-CROSS TO PLATS-IDDC-CROSS                         
046820       ELSE                                                               
046830         MOVE SPACE                    TO W-4402-KDORDKLX                 
046840         PERFORM IMS-LAS-TRANSP-TAB-FIRST                                 
046850         IF SEGMENT-FINNS                                                 
046860           MOVE 4402-IDTRPTNR          TO SPAR-IDTRPTNR                   
046870           MOVE 4402-IDDC              TO SPAR-IDDC                       
046880           MOVE 4402-IDDC-CROSS TO PLATS-IDDC-CROSS                       
046890         ELSE                                                             
046900           MOVE ZERO                   TO W-4402-KDFRAKT                  
047000           PERFORM IMS-LAS-TRANSP-TAB-FIRST                               
047100           IF SEGMENT-FINNS                                               
047200             MOVE 4402-IDTRPTNR        TO SPAR-IDTRPTNR                   
047300             MOVE 4402-IDDC            TO SPAR-IDDC                       
047400             MOVE 4402-IDDC-CROSS TO PLATS-IDDC-CROSS                     
047500           ELSE                                                           
047600             MOVE ZERO                 TO W-4402-IDKUNDNR                 
047700             PERFORM IMS-LAS-TRANSP-TAB-FIRST                             
047800             IF SEGMENT-FINNS                                             
047900               MOVE 4402-IDTRPTNR TO SPAR-IDTRPTNR                        
048000               MOVE 4402-IDDC          TO SPAR-IDDC                       
048100               MOVE 4402-IDDC-CROSS TO PLATS-IDDC-CROSS                   
048200             END-IF                                                       
048300           END-IF                                                         
048400         END-IF                                                           
048500       END-IF                                                             
048600     END-IF                                                               
051101     .                                                                    
051201     EJECT                                                                
051301 AAA-LAS-TRANSP-TAB SECTION.                                              
051401                                                                          
051501     MOVE '4401'          TO IDHTYP                                       
051601     PERFORM IMS-LAS-TRANSP-4401-ROT                                      
051701                                                                          
051801     MOVE PLATS-IDDC      TO W-4402-IDDC-MIN                              
051901     MOVE PLATS-IDDISTR   TO W-4402-IDDISTR-MIN                           
052001     SKIP2                                                                
052101     MOVE PLATS-IDDC      TO W-4402-IDDC-MAX                              
052201     MOVE PLATS-IDDISTR   TO W-4402-IDDISTR-MAX                           
052301     PERFORM IMS-LAS-TRANSP-TAB-GNP                                       
052401     .                                                                    
052501     EJECT                                                                
052601 AB-RUT-ADRESS SECTION.                                                   
052701                                                                          
052801     PERFORM ABA-LAS-TRANSP-ADR-SPAR                                      
052901     IF SEGMENT-FINNS                                                     
053001       MOVE TRPT-IDTRPTNR TO PLATS-IDTRPTNR                               
053101       MOVE TRPT-FLUTLAST TO PLATS-FLUTLAST                               
053201       PERFORM IMS-LAS-TRANSP-RUTA-OKVAL                                  
053301       PERFORM UNTIL SEGMENT-SAKNAS OR                                    
053401             TRPTRUT-FLRUTFUL NOT = JA                                    
053501         PERFORM IMS-LAS-TRANSP-RUTA-OKVAL                                
053601       END-PERFORM                                                        
053701       MOVE OK                    TO PLATS-KDSVAR                         
053801       MOVE TRPTRUT-ADCLGEO       TO PLATS-ADCLGEO                        
053901       IF PLATS-KDCALL = +6                                               
054001         MOVE FARLIG-ADFLOMR      TO PLATS-ADFLOMR                        
054101       ELSE                                                               
054201         MOVE TRPTRUT-ADFLOMR     TO PLATS-ADFLOMR                        
054301       END-IF                                                             
054401       MOVE TRPTRUT-ADRUTNIV      TO PLATS-ADRUTNIV                       
054501       MOVE ZERO                  TO PLATS-DIHMODUL                       
054601       MOVE ZERO                  TO PLATS-DIDMODUL                       
054701       MOVE ZERO                  TO PLATS-ADVMODUL                       
054801       MOVE ZERO                  TO PLATS-ADHMODUL                       
054901     ELSE                                                                 
055001       MOVE FEL                   TO PLATS-KDSVAR                         
055101     END-IF                                                               
055201     .                                                                    
055301     SKIP3                                                                
055401 ABA-LAS-TRANSP-ADR-SPAR SECTION.                                         
055501                                                                          
055601     MOVE '4405'             TO IDHTYP                                    
055701     PERFORM IMS-LAS-TRANSP-4405-ROT                                      
055801                                                                          
055901     MOVE SPAR-IDTRPTNR      TO W-4406-IDTRPTNR                           
056001     MOVE SPAR-IDDC          TO W-4406-IDDC                               
056101                                                                          
056201     PERFORM IMS-LAS-TRANSP-ADR-OMR                                       
056301     .                                                                    
056401     EJECT                                                                
056501 AF-KOLLA-GIVEN-ADRESS SECTION.                                           
056601     SKIP2                                                                
056701     IF PLATS-ADFLOMR < +500 OR > +899                                    
056801       PERFORM AFA-LAS-DEF-RUTA                                           
056901       IF SEGMENT-FINNS                                                   
057001         MOVE RUTA-TESPAERR  TO W-RUTA-TESPAERR                           
057101         IF W-TRPT = 'TRPT'                                               
057201           MOVE W-TRPTNR        TO PLATS-IDTRPTNR                         
057301         ELSE                                                             
057401           MOVE BILLIT-IDTRPTNR TO PLATS-IDTRPTNR                         
057501         END-IF                                                           
057601       ELSE                                                               
057701         MOVE BILLIT-IDTRPTNR TO PLATS-IDTRPTNR                           
057801       END-IF                                                             
057901                                                                          
058001       MOVE ZERO             TO PLATS-DIHMODUL                            
058101       MOVE ZERO             TO PLATS-DIDMODUL                            
058201       MOVE ZERO             TO PLATS-ADVMODUL                            
058301       MOVE ZERO             TO PLATS-ADHMODUL                            
058401       MOVE JA               TO PLATS-FLUTLAST                            
058501       MOVE OK               TO PLATS-KDSVAR                              
058601     ELSE                                                                 
058701       MOVE FEL              TO PLATS-KDSVAR                              
058801     END-IF                                                               
058901     .                                                                    
059001     EJECT                                                                
059101 AFA-LAS-DEF-RUTA SECTION.                                                
059201     SKIP2                                                                
059301     MOVE PLATS-ADCLGEO      TO W-4411-ADCLGEO                            
059401     PERFORM IMS-LAS-4411-ROT                                             
059501     IF SEGMENT-FINNS                                                     
059601       MOVE PLATS-ADFLOMR    TO W-4412-ADFLOMR                            
059701                                                                          
059801       MOVE PLATS-ADRUTNIV   TO W-4414-ADRUTNIV                           
059901       PERFORM IMS-LAS-RUTA-RUTA                                          
060001     END-IF                                                               
060101     .                                                                    
060201     EJECT                                                                
060301 AG-HAEMTA-KUND-TRANSPORT SECTION.                                        
060401     SKIP2                                                                
060501                                                                          
060601     MOVE PLATS-IDDC         TO W-KUND-IDDC                               
060701                                W-KUND-IDDC-DEFAULT                       
060801                                NDC-IDDC                                  
060901     MOVE PLATS-KDFRAKT      TO W-KUND-KDFRAKT                            
061001                                W-KUND-KDFRAKT-DEFAULT                    
061101     MOVE PLATS-IDDISTR      TO W-KUND-IDDISTR                            
061201                                W-KUND-IDDISTR-DEFAULT                    
061301     MOVE PLATS-IDKUNDNR     TO W-KUND-IDKUNDNR                           
061401     PERFORM IMS-GU-GMTC01-WDB501                                         
061501     IF SEGMENT-FINNS                                                     
061601       EVALUATE PLATS-KDORDKLX                                            
061701         WHEN '0'                                                         
061801           MOVE FK-IDTRPLOS-0 TO PLATS-IDTRPTNR                           
061901         WHEN '1'                                                         
062001           MOVE FK-IDTRPLOS-1 TO PLATS-IDTRPTNR                           
062101         WHEN '2'                                                         
062201           MOVE FK-IDTRPLOS-2 TO PLATS-IDTRPTNR                           
062301         WHEN '3'                                                         
062401           MOVE FK-IDTRPLOS-3 TO PLATS-IDTRPTNR                           
062501         WHEN '4'                                                         
062601           MOVE FK-IDTRPLOS-4 TO PLATS-IDTRPTNR                           
062701         WHEN OTHER                                                       
062801           MOVE FK-IDTRPLOS-3 TO PLATS-IDTRPTNR                           
062901       END-EVALUATE                                                       
063001       MOVE JA                     TO PLATS-FLUTLAST                      
063101       MOVE OK                     TO PLATS-KDSVAR                        
063201       MOVE NDC-ADCLGEO            TO PLATS-ADCLGEO                       
063301       MOVE NDC-ADFLOMR            TO PLATS-ADFLOMR                       
063401       MOVE NDC-ADRUTNIV           TO PLATS-ADRUTNIV                      
063501       MOVE ZERO                   TO PLATS-DIHMODUL                      
063601                                      PLATS-DIDMODUL                      
063701                                      PLATS-ADHMODUL                      
063801                                      PLATS-ADVMODUL                      
063901       MOVE PLATS-IDDISTR   TO TEST-IDDISTR                               
064001       IF PLATS-IDTRPTNR = ZERO                                           
064101         MOVE BILLIT-IDTRPTNR      TO PLATS-IDTRPTNR                      
064201       END-IF                                                             
064301     ELSE                                                                 
064401       MOVE FEL                    TO PLATS-KDSVAR                        
064501     END-IF                                                               
064601     .                                                                    
064602     EJECT                                                                
064701 AH-HAEMTA-TRANSPORT-BILLIT    SECTION.                                   
064801     SKIP2                                                                
064901     MOVE PLATS-IDDC             TO BILLIT-IDDC                           
065001     MOVE BILLIT-IDTRPTNR        TO PLATS-IDTRPTNR                        
065101     MOVE JA                     TO PLATS-FLUTLAST                        
065201     MOVE OK                     TO PLATS-KDSVAR                          
065301     MOVE BILLIT-ADCLGEO         TO PLATS-ADCLGEO                         
065401     MOVE BILLIT-ADFLOMR         TO PLATS-ADFLOMR                         
065501     MOVE BILLIT-ADRUTNIV        TO PLATS-ADRUTNIV                        
065601     MOVE ZERO                   TO PLATS-DIHMODUL                        
065701                                    PLATS-DIDMODUL                        
065801                                    PLATS-ADHMODUL                        
065901                                    PLATS-ADVMODUL                        
066001     .                                                                    
066101     EJECT                                                                
066201 B-PLATSBOKNING SECTION.                                                  
066301     SKIP2                                                                
066401     EVALUATE TRUE                                                        
066501       WHEN PLATS-ADFLOMR > +700 AND < +900                               
066601         PERFORM BA-STALL                                                 
066701       WHEN PLATS-ADFLOMR < +701 OR > +899                                
066801         PERFORM BB-RUTA                                                  
066901     END-EVALUATE                                                         
067001     .                                                                    
067101     EJECT                                                                
067201 BA-STALL SECTION.                                                        
067301     SKIP2                                                                
067401     PERFORM BS02-LAS-STALL-NIVA                                          
067501     IF SEGMENT-FINNS                                                     
067601       MOVE PLATS-ADVMODUL   TO SPAR-ADVMODUL                             
067701       MOVE PLATS-ADHMODUL   TO SPAR-ADHMODUL                             
067801       PERFORM BS01-ANROPA-WADRBER                                        
067901       IF ADR-KDSVAR NOT = SPACE                                          
068001         MOVE FEL            TO PLATS-KDSVAR                              
068101       ELSE                                                               
068201                                                                          
068301         MOVE ADR-TBSPAERR   TO STAELL-TBSPAERR                           
068401         PERFORM IMS-REPLACE-STALL                                        
068501         PERFORM IMS-LAS-STALL-SPARR-GHNP                                 
068601                                                                          
068701         PERFORM UNTIL SEGMENT-SAKNAS OR SPAERR-ADHMODUL                  
068801                       NOT < PLATS-ADVMODUL                               
068901           PERFORM IMS-LAS-STALL-SPARR-GHNP                               
069001         END-PERFORM                                                      
069101                                                                          
069201         PERFORM UNTIL SEGMENT-SAKNAS OR SPAERR-ADVMODUL                  
069301                       > PLATS-ADHMODUL                                   
069401           MOVE SPAERR-ADHMODUL TO SPAR-ADHMODUL                          
069501           IF SPAERR-ADVMODUL < PLATS-ADVMODUL                            
069601             COMPUTE SPAERR-ADHMODUL = PLATS-ADVMODUL - 1                 
069701             PERFORM IMS-REPLACE-STALL                                    
069801           ELSE                                                           
069901             PERFORM IMS-DELETE-STALL                                     
070001           END-IF                                                         
070101           IF SPAR-ADHMODUL > PLATS-ADHMODUL                              
070201             COMPUTE SPAERR-ADVMODUL = PLATS-ADHMODUL + 1                 
070301             MOVE SPAR-ADHMODUL TO SPAERR-ADHMODUL                        
070401             MOVE LOW-VALUE     TO SPAERR-LOWVALUE                        
070501             PERFORM IMS-INSERT-STALL-SPARR                               
070601           END-IF                                                         
070701           PERFORM IMS-LAS-STALL-SPARR-GHNP                               
070801         END-PERFORM                                                      
070901                                                                          
071001         MOVE PLATS-ADVMODUL TO SPAERR-ADVMODUL                           
071101         MOVE PLATS-ADHMODUL TO SPAERR-ADHMODUL                           
071201         MOVE PLATS-TESPAERR TO SPAERR-TESPAERR                           
071301         MOVE LOW-VALUE      TO SPAERR-LOWVALUE                           
071401         PERFORM IMS-INSERT-STALL-SPARR                                   
071501                                                                          
071601         MOVE OK             TO PLATS-KDSVAR                              
071701       END-IF                                                             
071801     ELSE                                                                 
071901       PERFORM BAA-LAGG-TILL-STALL                                        
072001       MOVE OK               TO PLATS-KDSVAR                              
072101     END-IF                                                               
072201     .                                                                    
072301     EJECT                                                                
072401 BAA-LAGG-TILL-STALL SECTION.                                             
072501     SKIP2                                                                
072601     MOVE PLATS-ADCLGEO      TO W-4417-ADCLGEO                            
072701     PERFORM IMS-LAS-STALL-ROT                                            
072801                                                                          
072901     IF SEGMENT-SAKNAS                                                    
073001       MOVE W-4417-IDHTYP    TO 4417-IDHTYP                               
073101       MOVE PLATS-ADCLGEO    TO 4417-ADCLGEO                              
073201       MOVE LOW-VALUE        TO 4417-LOWVALUE                             
073301       PERFORM IMS-INSERT-STALL-ROT                                       
073401     END-IF                                                               
073501                                                                          
073601     MOVE PLATS-ADFLOMR      TO STAELL-ADFLOMR                            
073701     MOVE PLATS-ADRUTNIV     TO STAELL-ADRUTNIV                           
073801     MOVE PLATS-DIHMODUL     TO STAELL-DIHMODUL                           
073901     MOVE PLATS-DIDMODUL     TO STAELL-DIDMODUL                           
074001     MOVE LOW-VALUE          TO STAELL-TBSPAERR                           
074101                                                                          
074201     IF PLATS-TESPAERR NOT = SPACE                                        
074301       MOVE PLATS-ADVMODUL   TO SPAR-ADVMODUL                             
074401       MOVE PLATS-ADHMODUL   TO SPAR-ADHMODUL                             
074501       PERFORM BS01-ANROPA-WADRBER                                        
074601       IF ADR-KDSVAR = SPACE                                              
074701         MOVE ADR-TBSPAERR   TO STAELL-TBSPAERR                           
074801       END-IF                                                             
074901     END-IF                                                               
075001                                                                          
075101     IF PLATS-ADHMODUL < +400                                             
075201       COMPUTE SPAR-ADVMODUL = PLATS-ADHMODUL + 1                         
075301       MOVE +400             TO SPAR-ADHMODUL                             
075401       PERFORM BS01-ANROPA-WADRBER                                        
075501       IF ADR-KDSVAR = SPACE                                              
075601         MOVE ADR-TBSPAERR   TO STAELL-TBSPAERR                           
075701       END-IF                                                             
075801     END-IF                                                               
075901                                                                          
076001     MOVE PLATS-VLRUTNIV     TO STAELL-VLRUTNIV                           
076101     MOVE LOW-VALUE          TO STAELL-LOWVALUE                           
076201     PERFORM IMS-INSERT-STALL-NIVA                                        
076301                                                                          
076401     IF PLATS-TESPAERR NOT = SPACE                                        
076501       MOVE PLATS-ADVMODUL   TO SPAERR-ADVMODUL                           
076601       MOVE PLATS-ADHMODUL   TO SPAERR-ADHMODUL                           
076701       MOVE PLATS-TESPAERR   TO SPAERR-TESPAERR                           
076801       MOVE LOW-VALUE        TO SPAERR-LOWVALUE                           
076901       PERFORM IMS-INSERT-STALL-SPARR                                     
077001     END-IF                                                               
077101                                                                          
077201     IF PLATS-ADHMODUL < 400                                              
077301       COMPUTE SPAERR-ADVMODUL = PLATS-ADHMODUL + 1                       
077401       MOVE +400             TO SPAERR-ADHMODUL                           
077501       MOVE STAELL-SLUT      TO SPAERR-TESPAERR                           
077601       MOVE LOW-VALUE        TO SPAERR-LOWVALUE                           
077701       PERFORM IMS-INSERT-STALL-SPARR                                     
077801     END-IF                                                               
077901     .                                                                    
078001     EJECT                                                                
078101 BB-RUTA SECTION.                                                         
078201     SKIP2                                                                
078301     MOVE OK                 TO PLATS-KDSVAR                              
078401                                                                          
078501     MOVE PLATS-ADCLGEO      TO W-4411-ADCLGEO                            
078601     PERFORM IMS-LAS-4411-ROT                                             
078701                                                                          
078801     IF SEGMENT-FINNS                                                     
078901       MOVE PLATS-ADFLOMR    TO W-4412-ADFLOMR                            
079001       PERFORM IMS-LAS-RUTA-LASTOMR                                       
079101                                                                          
079201       IF SEGMENT-FINNS                                                   
079301         MOVE PLATS-ADRUTNIV TO W-4414-ADRUTNIV                           
079401         PERFORM IMS-LAS-RUTA-RUTA-GHNP                                   
079501                                                                          
079601         IF SEGMENT-FINNS                                                 
079701           MOVE RUTA-TESPAERR TO W-RUTA-TESPAERR                          
079801                                                                          
079901           IF W-TRPT NOT = 'TRPT'                                         
080001             MOVE PLATS-TESPAERR TO RUTA-TESPAERR                         
080101             PERFORM IMS-REPLACE-RUTA                                     
080201           ELSE                                                           
080301             MOVE FEL           TO PLATS-KDSVAR                           
080401           END-IF                                                         
080501                                                                          
080601         ELSE                                                             
080701           MOVE PLATS-ADRUTNIV TO RUTA-ADRUTNIV                           
080801           MOVE PLATS-VLRUTNIV TO RUTA-VLRUTNIV                           
080901           MOVE PLATS-TESPAERR TO RUTA-TESPAERR                           
081001           MOVE LOW-VALUE     TO RUTA-LOWVALUE                            
081101           PERFORM IMS-INSERT-RUTA-RUTA                                   
081201         END-IF                                                           
081301                                                                          
081401       ELSE                                                               
081501         MOVE PLATS-ADFLOMR   TO LASTOMR-ADFLOMR                          
081601         MOVE LOW-VALUE       TO LASTOMR-LOWVALUE                         
081701         PERFORM IMS-INSERT-RUTA-LASTOMR                                  
081801         MOVE PLATS-ADRUTNIV  TO RUTA-ADRUTNIV                            
081901         MOVE PLATS-VLRUTNIV  TO RUTA-VLRUTNIV                            
082001         MOVE PLATS-TESPAERR  TO RUTA-TESPAERR                            
082101         MOVE LOW-VALUE       TO RUTA-LOWVALUE                            
082201         PERFORM IMS-INSERT-RUTA-RUTA                                     
082301       END-IF                                                             
082401                                                                          
082501     ELSE                                                                 
082601       MOVE W-4411-IDHTYP    TO 4411-IDHTYP                               
082701       MOVE PLATS-ADCLGEO    TO 4411-ADCLGEO                              
082801       MOVE LOW-VALUE        TO 4411-LOWVALUE                             
082901       PERFORM IMS-INSERT-RUTA-ROT                                        
083001       MOVE PLATS-ADFLOMR    TO LASTOMR-ADFLOMR                           
083101       MOVE LOW-VALUE        TO LASTOMR-LOWVALUE                          
083201       PERFORM IMS-INSERT-RUTA-LASTOMR                                    
083301       MOVE PLATS-ADRUTNIV   TO RUTA-ADRUTNIV                             
083401       MOVE PLATS-VLRUTNIV   TO RUTA-VLRUTNIV                             
083501       MOVE PLATS-TESPAERR   TO RUTA-TESPAERR                             
083601       MOVE LOW-VALUE        TO RUTA-LOWVALUE                             
083701       PERFORM IMS-INSERT-RUTA-RUTA                                       
083801     END-IF                                                               
083901     .                                                                    
084001     EJECT                                                                
084101 BS01-ANROPA-WADRBER SECTION.                                             
084201     SKIP2                                                                
084301     MOVE +2                 TO ADR-KDCALL                                
084401     MOVE STAELL-TBSPAERR    TO ADR-TBSPAERR                              
084501     MOVE SPAR-ADVMODUL      TO ADR-ADVMODUL                              
084601     MOVE SPAR-ADHMODUL      TO ADR-ADHMODUL                              
084701     CALL WADRBER USING ADR-WADRAREA                                      
084801     .                                                                    
084901     EJECT                                                                
085001 BS02-LAS-STALL-NIVA SECTION.                                             
085101     SKIP2                                                                
085201     MOVE PLATS-ADCLGEO      TO W-4417-ADCLGEO                            
085301     PERFORM IMS-LAS-STALL-ROT                                            
085401                                                                          
085501     IF SEGMENT-FINNS                                                     
085601       MOVE PLATS-ADFLOMR    TO W-4418-ADFLOMR                            
085701       MOVE PLATS-ADRUTNIV   TO W-4418-ADRUTNIV                           
085801       MOVE PLATS-DIHMODUL   TO W-4418-DIHMODUL                           
085901       MOVE PLATS-DIDMODUL   TO W-4418-DIDMODUL                           
086001                                                                          
086101       PERFORM IMS-LAS-STALL-NIVA-KVAL                                    
086201     END-IF                                                               
086301     .                                                                    
086401     EJECT                                                                
086501 C-PLATSAVBOKNING SECTION.                                                
086601                                                                          
086701     EVALUATE TRUE                                                        
086801       WHEN PLATS-ADFLOMR > +700 AND < +900                               
086901         PERFORM CS01-NOLLSTALL                                           
087001         IF PLATS-KDCALL = +3                                             
087101           PERFORM CA-STALL-AVBOKA                                        
087201         ELSE                                                             
087301           PERFORM CB-STALL-SPARR-BORT                                    
087401         END-IF                                                           
087501                                                                          
087601       WHEN PLATS-ADFLOMR < +701 OR > +899                                
087701         IF PLATS-KDCALL = +3                                             
087801           PERFORM CC-RUTA-AVBOKA                                         
087901         ELSE                                                             
088001           PERFORM CD-RUTA-SPARR-BORT                                     
088101         END-IF                                                           
088201                                                                          
088301       WHEN OTHER                                                         
088401         MOVE FEL TO PLATS-KDSVAR                                         
088501     END-EVALUATE                                                         
088601     .                                                                    
088701     EJECT                                                                
088801 CA-STALL-AVBOKA SECTION.                                                 
088901                                                                          
089001     MOVE FEL TO PLATS-KDSVAR                                             
089101                                                                          
089201     PERFORM CAA-SKAPA-TABELL-AVBOKA                                      
089301     PERFORM CS02-LAS-STALL-NIVA                                          
089401     MOVE +1 TO IX                                                        
089501                                                                          
089601     EVALUATE TRUE                                                        
089701       WHEN SEGMENT-FINNS AND TAB-ADVMODUL (IX) > ZERO                    
089801                                                                          
089901         PERFORM CS03-WADRBER                                             
090001                                                                          
090101         IF ADR-KDSVAR = SPACE                                            
090201           MOVE ADR-TBSPAERR TO STAELL-TBSPAERR                           
090301           PERFORM IMS-REPLACE-STALL                                      
090401         END-IF                                                           
090501                                                                          
090601         MOVE ADR-KDSVAR TO PLATS-KDSVAR                                  
090701                                                                          
090801       WHEN SEGMENT-FINNS AND TAB-ADVMODUL (IX) = ZERO                    
090901         MOVE OK TO PLATS-KDSVAR                                          
091001     END-EVALUATE                                                         
091101     .                                                                    
091201     EJECT                                                                
091301 CAA-SKAPA-TABELL-AVBOKA SECTION.                                         
091401                                                                          
091501     MOVE +0 TO IX                                                        
091601     PERFORM CS04-LAS-STALL-SPARR                                         
091701     PERFORM UNTIL SEGMENT-SAKNAS  OR                                     
091801             SPAERR-ADHMODUL NOT < PLATS-ADVMODUL                         
091901       PERFORM IMS-LAS-STALL-SPARR-GHNP                                   
092001     END-PERFORM                                                          
092101                                                                          
092201     IF SEGMENT-SAKNAS OR                                                 
092301       (SEGMENT-FINNS AND                                                 
092401        SPAERR-ADVMODUL > PLATS-ADHMODUL)                                 
092501       MOVE +1 TO IX                                                      
092601       MOVE PLATS-ADVMODUL TO TAB-ADVMODUL (IX)                           
092701       MOVE PLATS-ADHMODUL TO TAB-ADHMODUL (IX)                           
092801     ELSE                                                                 
092901       MOVE PLATS-ADVMODUL TO SPAR-ADVMODUL                               
093001       PERFORM UNTIL SEGMENT-SAKNAS OR                                    
093101               SPAERR-ADVMODUL NOT < PLATS-ADHMODUL                       
093201         IF SPAERR-ADVMODUL > PLATS-ADVMODUL                              
093301           ADD +1 TO IX                                                   
093401           MOVE SPAR-ADVMODUL TO                                          
093501               TAB-ADVMODUL (IX)                                          
093601           COMPUTE TAB-ADHMODUL (IX) =                                    
093701           SPAERR-ADVMODUL - 1                                            
093801         END-IF                                                           
093901         IF SPAERR-ADHMODUL < PLATS-ADHMODUL                              
094001           ADD +1 TO IX                                                   
094101           COMPUTE TAB-ADVMODUL (IX) =                                    
094201           SPAERR-ADHMODUL + 1                                            
094301           PERFORM IMS-LAS-STALL-SPARR-GHNP                               
094401           IF SEGMENT-FINNS AND                                           
094501             SPAERR-ADVMODUL < PLATS-ADHMODUL                             
094601             COMPUTE TAB-ADHMODUL (IX) =                                  
094701             SPAERR-ADVMODUL - 1                                          
094801                                                                          
094901             COMPUTE SPAR-ADVMODUL =                                      
095001             SPAERR-ADHMODUL + 1                                          
095101           ELSE                                                           
095201            MOVE PLATS-ADHMODUL TO TAB-ADHMODUL (IX)                      
095301           END-IF                                                         
095401         END-IF                                                           
095501         PERFORM IMS-LAS-STALL-SPARR-GHNP                                 
095601       END-PERFORM                                                        
095701     END-IF                                                               
095801                                                                          
095901     COMPUTE TAB-IX-PLUS-ETT = IX + 1                                     
096001     .                                                                    
096101     EJECT                                                                
096201 CB-STALL-SPARR-BORT SECTION.                                             
096301                                                                          
096401     PERFORM CS05-LAS-KOLLI-SEK                                           
096501                                                                          
096601     IF SEGMENT-FINNS                                                     
096701       PERFORM CBA-SKAPA-TAB-SPARR-BORT                                   
096801     ELSE                                                                 
096901       MOVE +1 TO IX                                                      
097001       MOVE +2 TO TAB-IX-PLUS-ETT                                         
097101       MOVE PLATS-ADVMODUL TO TAB-ADVMODUL (IX)                           
097201       MOVE PLATS-ADHMODUL TO TAB-ADHMODUL (IX)                           
097301     END-IF                                                               
097401                                                                          
097501     MOVE FEL TO PLATS-KDSVAR                                             
097601     PERFORM CS02-LAS-STALL-NIVA                                          
097701     MOVE +1 TO IX                                                        
097801                                                                          
097901     IF SEGMENT-FINNS AND TAB-ADVMODUL (IX) > ZERO                        
098001       PERFORM CS03-WADRBER                                               
098101                                                                          
098201       IF ADR-KDSVAR = SPACE                                              
098301         MOVE ADR-TBSPAERR TO STAELL-TBSPAERR                             
098401         PERFORM IMS-REPLACE-STALL                                        
098501         PERFORM CBB-LAS-TA-BORT-SPARR                                    
098601       END-IF                                                             
098701       MOVE ADR-KDSVAR TO PLATS-KDSVAR                                    
098801     END-IF                                                               
098901     .                                                                    
099001     EJECT                                                                
099101 CBA-SKAPA-TAB-SPARR-BORT SECTION.                                        
099201                                                                          
099301     MOVE +0 TO IX                                                        
099401     MOVE PLATS-ADVMODUL TO SPAR-ADVMODUL                                 
099501                                                                          
099601     PERFORM UNTIL SEGMENT-SAKNAS                                         
099701       IF SEQC-ADVMODUL > PLATS-ADVMODUL                                  
099801         ADD +1 TO IX                                                     
099901         MOVE SPAR-ADVMODUL TO TAB-ADVMODUL (IX)                          
100001         COMPUTE TAB-ADHMODUL (IX) =                                      
100101         SEQC-ADVMODUL - 1                                                
100201       END-IF                                                             
100301       IF SEQC-ADHMODUL < PLATS-ADHMODUL                                  
100401         ADD +1 TO IX                                                     
100501         COMPUTE TAB-ADVMODUL (IX) =                                      
100601         SEQC-ADHMODUL + 1                                                
100701         PERFORM IMS-LAS-KOLLI-SEK-KVAL                                   
100801         IF SEGMENT-FINNS                                                 
100901           COMPUTE TAB-ADHMODUL (IX) =                                    
101001             SEQC-ADVMODUL - 1                                            
101101                                                                          
101201           COMPUTE SPAR-ADVMODUL =                                        
101301           SEQC-ADHMODUL + 1                                              
101401         ELSE                                                             
101501          MOVE PLATS-ADHMODUL TO TAB-ADHMODUL (IX)                        
101601         END-IF                                                           
101701       END-IF                                                             
101801       PERFORM IMS-LAS-KOLLI-SEK-KVAL                                     
101901     END-PERFORM                                                          
102001                                                                          
102101     COMPUTE TAB-IX-PLUS-ETT = IX + 1                                     
102201     .                                                                    
102301     EJECT                                                                
102401 CBB-LAS-TA-BORT-SPARR SECTION.                                           
102501                                                                          
102601     MOVE PLATS-ADVMODUL TO W-4420-ADVMODUL                               
102701     PERFORM IMS-LAS-STALL-SPARR-KVAL                                     
102801     IF SEGMENT-FINNS                                                     
102901       PERFORM IMS-DELETE-STALL                                           
103001     END-IF                                                               
103101     .                                                                    
103201     EJECT                                                                
103301 CC-RUTA-AVBOKA SECTION.                                                  
103401                                                                          
103501     MOVE OK TO PLATS-KDSVAR                                              
103601                                                                          
103701     PERFORM CS06-LAS-KOLLI-SEK-RUTA                                      
103801     IF SEGMENT-SAKNAS                                                    
103901       PERFORM CS07-LAS-TRANSP-ADR                                        
104001       IF SEGMENT-FINNS                                                   
104101         IF TRPTRUT-FLRUTFUL = 'J'                                        
104201           PERFORM IMS-DELETE-TRANSP-ADR                                  
104301           PERFORM CS08-LAS-RUTA                                          
104401           IF SEGMENT-FINNS                                               
104501             MOVE RUTA-TESPAERR TO W-RUTA-TESPAERR                        
104601             IF W-TRPT = 'TRPT'                                           
104701               MOVE SPACE TO RUTA-TESPAERR                                
104801               PERFORM IMS-REPLACE-RUTA                                   
104901             END-IF                                                       
105001           ELSE                                                           
105101             MOVE FEL TO PLATS-KDSVAR                                     
105201           END-IF                                                         
105301         END-IF                                                           
105401       END-IF                                                             
105501     END-IF                                                               
105601     .                                                                    
105701     EJECT                                                                
105801 CD-RUTA-SPARR-BORT SECTION.                                              
105901                                                                          
106001     MOVE FEL TO PLATS-KDSVAR                                             
106101     PERFORM CS08-LAS-RUTA                                                
106201     IF SEGMENT-FINNS                                                     
106301       MOVE SPACE TO RUTA-TESPAERR                                        
106401       PERFORM IMS-REPLACE-RUTA                                           
106501       MOVE OK TO PLATS-KDSVAR                                            
106601     END-IF                                                               
106701     .                                                                    
106801     EJECT                                                                
106901 CS01-NOLLSTALL SECTION.                                                  
107001                                                                          
107101     MOVE ZERO TO SPAR-ADVMODUL                                           
107201                                                                          
107301     MOVE +1 TO IX                                                        
107401     PERFORM UNTIL IX NOT < MAX-IX-PLUS-ETT                               
107501       MOVE ZERO TO TAB-ADVMODUL (IX)                                     
107601       MOVE ZERO TO TAB-ADHMODUL (IX)                                     
107701                                                                          
107801       ADD +1 TO IX                                                       
107901     END-PERFORM                                                          
108001     .                                                                    
108101     EJECT                                                                
108201 CS02-LAS-STALL-NIVA SECTION.                                             
108301                                                                          
108401     MOVE PLATS-ADCLGEO TO W-4417-ADCLGEO                                 
108501     PERFORM IMS-LAS-STALL-ROT                                            
108601                                                                          
108701     IF SEGMENT-FINNS                                                     
108801       MOVE PLATS-ADFLOMR  TO W-4418-ADFLOMR                              
108901       MOVE PLATS-ADRUTNIV TO W-4418-ADRUTNIV                             
109001       MOVE PLATS-DIHMODUL TO W-4418-DIHMODUL                             
109101       MOVE PLATS-DIDMODUL TO W-4418-DIDMODUL                             
109201       PERFORM IMS-LAS-STALL-NIVA-KVAL                                    
109301     END-IF                                                               
109401     .                                                                    
109501     EJECT                                                                
109601 CS03-WADRBER SECTION.                                                    
109701                                                                          
109801     MOVE +3 TO ADR-KDCALL                                                
109901     MOVE STAELL-TBSPAERR TO ADR-TBSPAERR                                 
110001                                                                          
110101     MOVE +1 TO IX                                                        
110201                                                                          
110301     PERFORM UNTIL IX NOT < TAB-IX-PLUS-ETT                               
110401       MOVE TAB-ADVMODUL (IX) TO ADR-ADVMODUL                             
110501       MOVE TAB-ADHMODUL (IX) TO ADR-ADHMODUL                             
110601                                                                          
110701       CALL WADRBER USING ADR-WADRAREA                                    
110801                                                                          
110901       ADD +1 TO IX                                                       
111001     END-PERFORM                                                          
111101     .                                                                    
111201     EJECT                                                                
111301 CS04-LAS-STALL-SPARR SECTION.                                            
111401                                                                          
111501     MOVE PLATS-ADCLGEO TO W-4417-ADCLGEO                                 
111601     PERFORM IMS-LAS-STALL-ROT                                            
111701                                                                          
111801     IF SEGMENT-FINNS                                                     
111901       MOVE PLATS-ADFLOMR  TO W-4418-ADFLOMR                              
112001       MOVE PLATS-ADRUTNIV TO W-4418-ADRUTNIV                             
112101       MOVE PLATS-DIHMODUL TO W-4418-DIHMODUL                             
112201       MOVE PLATS-DIDMODUL TO W-4418-DIDMODUL                             
112301                                                                          
112401       PERFORM IMS-LAS-STALL-SPARR-GHNP                                   
112501     END-IF                                                               
112601     .                                                                    
112701     EJECT                                                                
112801 CS05-LAS-KOLLI-SEK SECTION.                                              
112901                                                                          
113001     MOVE LOW-VALUE TO W-WDE6C1KY-X-MIN                                   
113101                                                                          
113201     MOVE PLATS-IDTRPTNR TO W-SEK-IDTRPTNR-MIN                            
113301     MOVE PLATS-TIRFS    TO W-SEK-DARFS-MIN                               
113401     IF PLATS-TIRFS NOT = ZERO                                            
113501       IF PLATS-TIRFS < 5000000000                                        
113601         MOVE 20         TO W-SEK-DARFS-MIN (1:2)                         
113701       ELSE                                                               
113801         IF PLATS-TIRFS < 9999999999                                      
113901           MOVE 19       TO W-SEK-DARFS-MIN (1:2)                         
114001         ELSE                                                             
114101           MOVE 999999999999 TO W-SEK-DARFS-MIN                           
114201         END-IF                                                           
114301       END-IF                                                             
114401     END-IF                                                               
114501     MOVE PLATS-ADCLGEO  TO W-SEK-ADCLGEO-MIN                             
114601     MOVE PLATS-ADFLOMR  TO W-SEK-ADFLOMR-MIN                             
114701     MOVE PLATS-ADRUTNIV TO W-SEK-ADRUTNIV-MIN                            
114801     MOVE PLATS-ADHMODUL TO W-SEK-ADVMODUL-MIN                            
114901                                                                          
115001     MOVE PLATS-ADVMODUL TO W-SEK-ADHMODUL                                
115101                                                                          
115201     PERFORM IMS-LAS-KOLLI-SEK-KVAL                                       
115301     .                                                                    
115401     SKIP3                                                                
115501 CS06-LAS-KOLLI-SEK-RUTA SECTION.                                         
115601                                                                          
115701     MOVE LOW-VALUE TO W-WDE6C1KY-X-MIN                                   
115801     MOVE HIGH-VALUE TO W-WDE6C1KY-X-MAX                                  
115901                                                                          
116001     MOVE PLATS-IDTRPTNR TO W-SEK-IDTRPTNR-MIN                            
116101                            W-SEK-IDTRPTNR-MAX                            
116201     MOVE PLATS-IDDC     TO W-SEK-IDDC                                    
116301     MOVE PLATS-ADFLGEO  TO W-SEK-ADFLGEO                                 
116401     MOVE PLATS-ADFLOMR  TO W-SEK-ADFLOMR                                 
116501     MOVE PLATS-ADRUTNIV TO W-SEK-ADRUTNIV                                
116601                                                                          
116701     PERFORM IMS-LAS-KOLLI-SEK-RUTA                                       
116801     .                                                                    
116901     EJECT                                                                
117001 CS07-LAS-TRANSP-ADR SECTION.                                             
117101                                                                          
117201     MOVE '4405' TO IDHTYP                                                
117301     PERFORM IMS-LAS-TRANSP-4405-ROT                                      
117401                                                                          
117501     MOVE PLATS-IDTRPTNR TO W-4406-IDTRPTNR                               
117601     MOVE PLATS-IDDC     TO W-4406-IDDC                                   
117701                                                                          
117801     MOVE PLATS-ADCLGEO  TO W-4408-ADCLGEO                                
117901     MOVE PLATS-ADFLOMR  TO W-4408-ADFLOMR                                
118001     MOVE PLATS-ADRUTNIV TO W-4408-ADRUTNIV                               
118101     PERFORM IMS-LAS-TRANSP-ADR-RUTA                                      
118201     .                                                                    
118301     EJECT                                                                
118401 CS08-LAS-RUTA SECTION.                                                   
118501                                                                          
118601     MOVE PLATS-ADCLGEO TO W-4411-ADCLGEO                                 
118701     PERFORM IMS-LAS-4411-ROT                                             
118801                                                                          
118901     IF SEGMENT-FINNS                                                     
119001       MOVE PLATS-ADFLOMR TO W-4412-ADFLOMR                               
119101                                                                          
119201       MOVE PLATS-ADRUTNIV TO W-4414-ADRUTNIV                             
119301       PERFORM IMS-LAS-RUTA-RUTA-GHNP                                     
119401     END-IF                                                               
119501     .                                                                    
119601     EJECT                                                                
119701***  IMS-SEKTIONER ***                                                    
119801                                                                          
119901 IMS-LAS-TRANSP-4401-ROT SECTION.                                         
120001                                                                          
120101     STRING 'WLXXDM01(WDGXKEY  =' WDGX01 ')'                              
120201            DELIMITED BY SIZE INTO SSA1                                   
120301     MOVE '  ' TO GODK-STATUSKODER                                        
120401     CALL CBLTDLI USING GU                                                
120501     XXDM-PCB DLI-IO-AREA SSA1                                            
120601     MOVE XXDM-STATUS-CODE TO STATUS-WS                                   
120701     PERFORM IMS-STATUSKONTROLL                                           
120801     .                                                                    
120901     SKIP3                                                                
121001 IMS-LAS-TRANSP-TAB-GNP SECTION.                                          
121101                                                                          
121201     STRING 'WLXXDM11(KY4402  >=' W-4402-KY4402-MIN-X                     
121301                    '&KY4402  <=' W-4402-KY4402-MAX-X ')'                 
121401            DELIMITED BY SIZE INTO SSA1                                   
121501     MOVE '  GE' TO GODK-STATUSKODER                                      
121601     CALL CBLTDLI USING GNP                                               
121701     XXDM-PCB DLI-IO-AREA SSA1                                            
121801     MOVE XXDM-STATUS-CODE TO STATUS-WS                                   
121901     PERFORM IMS-STATUSKONTROLL                                           
122001     .                                                                    
122101     SKIP2                                                                
122201 IMS-LAS-TRANSP-TAB-FIRST SECTION.                                        
122301     SKIP2                                                                
122401     STRING 'WLXXDM11*F(KY4402   =' W-4402-KY4402-X ')'                   
122501            DELIMITED BY SIZE INTO SSA1                                   
122601     MOVE '  GE' TO GODK-STATUSKODER                                      
122701     CALL CBLTDLI USING GNP                                               
122801     XXDM-PCB DLI-IO-AREA SSA1                                            
122901     MOVE XXDM-STATUS-CODE TO STATUS-WS                                   
123001     PERFORM IMS-STATUSKONTROLL                                           
123101     .                                                                    
123201     EJECT                                                                
123301 IMS-LAS-TRANSP-4405-ROT SECTION.                                         
123401                                                                          
123501     STRING 'WLXXDN01(WDGXKEY  =' WDGX01 ')'                              
123601            DELIMITED BY SIZE INTO SSA1                                   
123701     MOVE '  ' TO GODK-STATUSKODER                                        
123801     CALL CBLTDLI USING GU                                                
123901     XXDN-PCB DLI-IO-AREA SSA1                                            
124001     MOVE XXDN-STATUS-CODE TO STATUS-WS                                   
124101     PERFORM IMS-STATUSKONTROLL                                           
124201     .                                                                    
124301     SKIP2                                                                
124401 IMS-LAS-TRANSP-ADR-OMR SECTION.                                          
124501                                                                          
124601     STRING 'WLXXDN11(WDGXKEY  =' W-4406-WDGXKEY-X ')'                    
124701            DELIMITED BY SIZE INTO SSA1                                   
124801     MOVE '  GE' TO GODK-STATUSKODER                                      
124901     CALL CBLTDLI USING GNP                                               
125001     XXDN-PCB DLI-IO-AREA SSA1                                            
125101     MOVE XXDN-STATUS-CODE TO STATUS-WS                                   
125201     PERFORM IMS-STATUSKONTROLL                                           
125301     .                                                                    
125401                                                                          
125501 IMS-LAS-TRANSP-ADR-RUTA SECTION.                                         
125601     SKIP2                                                                
125701     STRING 'WLXXDN11(WDGXKEY  =' W-4406-WDGXKEY-X ')'                    
125801            DELIMITED BY SIZE INTO SSA1                                   
125901     STRING 'WLXXDN21(WDGXKEY  =' W-4408-WDGXKEY-X ')'                    
126001            DELIMITED BY SIZE INTO SSA2                                   
126101     MOVE '  GE' TO GODK-STATUSKODER                                      
126201     CALL CBLTDLI USING GHNP                                              
126301     XXDN-PCB DLI-IO-AREA SSA1 SSA2                                       
126401     MOVE XXDN-STATUS-CODE TO STATUS-WS                                   
126501     PERFORM IMS-STATUSKONTROLL                                           
126601     .                                                                    
126701     SKIP2                                                                
126801 IMS-DELETE-TRANSP-ADR SECTION.                                           
126901                                                                          
127001     MOVE '  ' TO GODK-STATUSKODER                                        
127101     CALL CBLTDLI USING DLET XXDN-PCB DLI-IO-AREA                         
127201     MOVE XXDN-STATUS-CODE TO STATUS-WS                                   
127301     PERFORM IMS-STATUSKONTROLL                                           
127401     .                                                                    
127501     SKIP3                                                                
127601 IMS-LAS-TRANSP-RUTA-OKVAL SECTION.                                       
127701                                                                          
127801     MOVE 'WLXXDN21 ' TO SSA1                                             
127901     MOVE '  GE' TO GODK-STATUSKODER                                      
128001     CALL CBLTDLI USING GNP                                               
128101     XXDN-PCB DLI-IO-AREA SSA1                                            
128201     MOVE XXDN-STATUS-CODE TO STATUS-WS                                   
128301     PERFORM IMS-STATUSKONTROLL                                           
128401     .                                                                    
128501     EJECT                                                                
128601 IMS-LAS-STALL-ROT SECTION.                                               
128701                                                                          
128801     STRING 'WLXXDP01(WDGXKEY  =' W-4417-WDGXKEY-X ')'                    
128901            DELIMITED BY SIZE INTO SSA1                                   
129001     MOVE '  GE' TO GODK-STATUSKODER                                      
129101     CALL CBLTDLI USING GU                                                
129201     XXDP-PCB DLI-IO-AREA SSA1                                            
129301     MOVE XXDP-STATUS-CODE TO STATUS-WS                                   
129401     PERFORM IMS-STATUSKONTROLL                                           
129501     .                                                                    
129601     SKIP2                                                                
129701 IMS-LAS-STALL-NIVA-KVAL SECTION.                                         
129801                                                                          
129901     STRING 'WLXXDP11(WDGXKEY  =' W-4418-WDGXKEY-X ')'                    
130001            DELIMITED BY SIZE INTO SSA1                                   
130101     MOVE '  GE' TO GODK-STATUSKODER                                      
130201     CALL CBLTDLI USING GHNP                                              
130301     XXDP-PCB DLI-IO-AREA SSA1                                            
130401     MOVE XXDP-STATUS-CODE TO STATUS-WS                                   
130501     PERFORM IMS-STATUSKONTROLL                                           
130601     .                                                                    
130701     EJECT                                                                
130801 IMS-LAS-STALL-SPARR-GHNP SECTION.                                        
130901                                                                          
131001     STRING 'WLXXDP11(WDGXKEY  =' W-4418-WDGXKEY-X ')'                    
131101            DELIMITED BY SIZE INTO SSA1                                   
131201     MOVE 'WLXXDP21 ' TO SSA2                                             
131301     MOVE '  GE' TO GODK-STATUSKODER                                      
131401     CALL CBLTDLI USING GHNP                                              
131501     XXDP-PCB DLI-IO-AREA SSA1 SSA2                                       
131601     MOVE XXDP-STATUS-CODE TO STATUS-WS                                   
131701     PERFORM IMS-STATUSKONTROLL                                           
131801     SKIP3                                                                
131901     .                                                                    
132001 IMS-LAS-STALL-SPARR-KVAL SECTION.                                        
132101                                                                          
132201     STRING 'WLXXDP11(WDGXKEY  =' W-4418-WDGXKEY-X ')'                    
132301            DELIMITED BY SIZE INTO SSA1                                   
132401     STRING 'WLXXDP21(WDGXKEY  =' W-4420-WDGXKEY-X ')'                    
132501            DELIMITED BY SIZE INTO SSA2                                   
132601     MOVE '  GE' TO GODK-STATUSKODER                                      
132701     CALL CBLTDLI USING GHNP                                              
132801     XXDP-PCB DLI-IO-AREA SSA1 SSA2                                       
132901     MOVE XXDP-STATUS-CODE TO STATUS-WS                                   
133001     PERFORM IMS-STATUSKONTROLL                                           
133101     .                                                                    
133201     SKIP3                                                                
133301 IMS-DELETE-STALL SECTION.                                                
133401                                                                          
133501     MOVE '  ' TO GODK-STATUSKODER                                        
133601     CALL CBLTDLI USING DLET XXDP-PCB DLI-IO-AREA                         
133701     MOVE XXDP-STATUS-CODE TO STATUS-WS                                   
133801     PERFORM IMS-STATUSKONTROLL                                           
133901     .                                                                    
134001     EJECT                                                                
134101 IMS-INSERT-STALL-ROT SECTION.                                            
134201                                                                          
134301     MOVE 'WLXXDP01 ' TO SSA1                                             
134401     MOVE '  ' TO GODK-STATUSKODER                                        
134501     CALL CBLTDLI USING ISRT                                              
134601     XXDP-PCB DLI-IO-AREA SSA1                                            
134701     MOVE XXDP-STATUS-CODE TO STATUS-WS                                   
134801     PERFORM IMS-STATUSKONTROLL                                           
134901     SKIP2                                                                
135001     .                                                                    
135101 IMS-INSERT-STALL-NIVA SECTION.                                           
135201     SKIP2                                                                
135301     MOVE 'WLXXDP11 ' TO SSA1                                             
135401     MOVE '  ' TO GODK-STATUSKODER                                        
135501     CALL CBLTDLI USING ISRT                                              
135601     XXDP-PCB DLI-IO-AREA SSA1                                            
135701     MOVE XXDP-STATUS-CODE TO STATUS-WS                                   
135801     PERFORM IMS-STATUSKONTROLL                                           
135901     SKIP2                                                                
136001     .                                                                    
136101 IMS-INSERT-STALL-SPARR SECTION.                                          
136201     SKIP2                                                                
136301     MOVE 'WLXXDP21 ' TO SSA1                                             
136401     MOVE '  ' TO GODK-STATUSKODER                                        
136501     CALL CBLTDLI USING ISRT                                              
136601     XXDP-PCB DLI-IO-AREA SSA1                                            
136701     MOVE XXDP-STATUS-CODE TO STATUS-WS                                   
136801     PERFORM IMS-STATUSKONTROLL                                           
136901     .                                                                    
137001     EJECT                                                                
137101 IMS-REPLACE-STALL SECTION.                                               
137201                                                                          
137301     MOVE '  ' TO GODK-STATUSKODER                                        
137401     CALL CBLTDLI USING REPL                                              
137501     XXDP-PCB DLI-IO-AREA                                                 
137601     MOVE XXDP-STATUS-CODE TO STATUS-WS                                   
137701     PERFORM IMS-STATUSKONTROLL                                           
137801     .                                                                    
137901     EJECT                                                                
138001 IMS-LAS-4411-ROT SECTION.                                                
138101                                                                          
138201     STRING 'WLXXDO01(WDGXKEY  =' W-4411-WDGXKEY-X ')'                    
138301            DELIMITED BY SIZE INTO SSA1                                   
138401     MOVE '  GE' TO GODK-STATUSKODER                                      
138501     CALL CBLTDLI USING GU                                                
138601     XXDO-PCB DLI-IO-AREA SSA1                                            
138701     MOVE XXDO-STATUS-CODE TO STATUS-WS                                   
138801     PERFORM IMS-STATUSKONTROLL                                           
138901     .                                                                    
139001     SKIP3                                                                
139101 IMS-LAS-RUTA-RUTA SECTION.                                               
139201                                                                          
139301     STRING 'WLXXDO11(WDGXKEY  =' W-4412-WDGXKEY-X ')'                    
139401            DELIMITED BY SIZE INTO SSA1                                   
139501     STRING 'WLXXDO21(WDGXKEY  =' W-4414-WDGXKEY-X ')'                    
139601            DELIMITED BY SIZE INTO SSA2                                   
139701     MOVE '  GE' TO GODK-STATUSKODER                                      
139801     CALL CBLTDLI USING GNP                                               
139901     XXDO-PCB DLI-IO-AREA SSA1 SSA2                                       
140001     MOVE XXDO-STATUS-CODE TO STATUS-WS                                   
140101     PERFORM IMS-STATUSKONTROLL                                           
140201     .                                                                    
140301     SKIP2                                                                
140401 IMS-LAS-RUTA-RUTA-GHNP SECTION.                                          
140501                                                                          
140601     STRING 'WLXXDO11(WDGXKEY  =' W-4412-WDGXKEY-X ')'                    
140701            DELIMITED BY SIZE INTO SSA1                                   
140801     STRING 'WLXXDO21(WDGXKEY  =' W-4414-WDGXKEY-X ')'                    
140901            DELIMITED BY SIZE INTO SSA2                                   
141001     MOVE '  GE' TO GODK-STATUSKODER                                      
141101     CALL CBLTDLI USING GHNP                                              
141201     XXDO-PCB DLI-IO-AREA SSA1 SSA2                                       
141301     MOVE XXDO-STATUS-CODE TO STATUS-WS                                   
141401     PERFORM IMS-STATUSKONTROLL                                           
141501     .                                                                    
141601     EJECT                                                                
141701 IMS-LAS-RUTA-LASTOMR SECTION.                                            
141801                                                                          
141901     STRING 'WLXXDO11(WDGXKEY  =' W-4412-WDGXKEY-X ')'                    
142001            DELIMITED BY SIZE INTO SSA1                                   
142101     MOVE '  GE' TO GODK-STATUSKODER                                      
142201     CALL CBLTDLI USING GNP                                               
142301     XXDO-PCB DLI-IO-AREA SSA1                                            
142401     MOVE XXDO-STATUS-CODE TO STATUS-WS                                   
142501     PERFORM IMS-STATUSKONTROLL                                           
142601     SKIP3                                                                
142701     .                                                                    
142801 IMS-REPLACE-RUTA SECTION.                                                
142901                                                                          
143001     MOVE '  ' TO GODK-STATUSKODER                                        
143101     CALL CBLTDLI USING REPL XXDO-PCB DLI-IO-AREA                         
143201     MOVE XXDO-STATUS-CODE TO STATUS-WS                                   
143301     PERFORM IMS-STATUSKONTROLL                                           
143401     SKIP3                                                                
143501     .                                                                    
143601 IMS-INSERT-RUTA-ROT SECTION.                                             
143701                                                                          
143801     MOVE 'WLXXDO01 ' TO SSA1                                             
143901     MOVE '  ' TO GODK-STATUSKODER                                        
144001     CALL CBLTDLI USING ISRT                                              
144101     XXDO-PCB DLI-IO-AREA SSA1                                            
144201     MOVE XXDO-STATUS-CODE TO STATUS-WS                                   
144301     PERFORM IMS-STATUSKONTROLL                                           
144401     SKIP3                                                                
144501     .                                                                    
144601 IMS-INSERT-RUTA-LASTOMR SECTION.                                         
144701                                                                          
144801     MOVE 'WLXXDO11 ' TO SSA1                                             
144901     MOVE '  ' TO GODK-STATUSKODER                                        
145001     CALL CBLTDLI USING ISRT                                              
145101     XXDO-PCB DLI-IO-AREA SSA1                                            
145201     MOVE XXDO-STATUS-CODE TO STATUS-WS                                   
145301     PERFORM IMS-STATUSKONTROLL                                           
145401     SKIP3                                                                
145501     .                                                                    
145601 IMS-INSERT-RUTA-RUTA SECTION.                                            
145701                                                                          
145801     MOVE 'WLXXDO21 ' TO SSA1                                             
145901     MOVE '  ' TO GODK-STATUSKODER                                        
146001     CALL CBLTDLI USING ISRT                                              
146101     XXDO-PCB DLI-IO-AREA SSA1                                            
146201     MOVE XXDO-STATUS-CODE TO STATUS-WS                                   
146301     PERFORM IMS-STATUSKONTROLL                                           
146401     .                                                                    
146501     EJECT                                                                
146601 IMS-LAS-KOLLI-SEK-KVAL SECTION.                                          
146701                                                                          
146801     STRING 'WDE6C1  (WDE6C1KY<=' W-WDE6C1KY-X-MIN                        
146901                    '&ADHMODUL>=' W-SEK-ADHMODUL-X ')'                    
147001            DELIMITED BY SIZE INTO SSA1                                   
147101     MOVE '  GEGB' TO GODK-STATUSKODER                                    
147201     CALL CBLTDLI USING GN WDE6C-PCB DLI-IO-AREA SSA1                     
147301     MOVE WDE6C-STATUS-CODE TO STATUS-WS                                  
147401     PERFORM IMS-STATUSKONTROLL                                           
147501     .                                                                    
147601     SKIP3                                                                
147701 IMS-LAS-KOLLI-SEK-RUTA SECTION.                                          
147801                                                                          
147901     STRING 'WDE6C1  (WDE6C1KY>=' W-WDE6C1KY-X-MIN                        
148001                    '&WDE6C1KY<=' W-WDE6C1KY-X-MAX                        
148101                    '&ADCLGEO  =' W-SEK-ADCLGEO-X                         
148201                    '&ADFLOMR  =' W-SEK-ADFLOMR-X                         
148301                    '&ADRUTNIV =' W-SEK-ADRUTNIV-X ')'                    
148401            DELIMITED BY SIZE INTO SSA1                                   
148501     MOVE '  GE' TO GODK-STATUSKODER                                      
148601     CALL CBLTDLI USING GU WDE6C-PCB DLI-IO-AREA SSA1                     
148701     MOVE WDE6C-STATUS-CODE TO STATUS-WS                                  
148801     PERFORM IMS-STATUSKONTROLL                                           
148901     .                                                                    
149001     EJECT                                                                
149101 IMS-GU-GMTC01-WDB501      SECTION.                                       
149201     STRING 'WLGMTC01(WDB501KY =' W-WDB501KY-X                            
149301                    '!WDB501KY =' W-WDB501KY-DEFAULT-X ')'                
149401            DELIMITED BY SIZE INTO SSA1                                   
149501     MOVE '  GE' TO GODK-STATUSKODER                                      
149601     CALL CBLTDLI USING GU GMTC-PCB DLI-IO-AREA SSA1                      
149701     MOVE GMTC-STATUS-CODE TO STATUS-WS                                   
149801     PERFORM IMS-STATUSKONTROLL                                           
149901     SKIP3                                                                
150001     .                                                                    
150101 IMS-GU-WDB601    SECTION.                                                
150201     STRING 'WDB601  (IDDC     =' W-IDDC-B6-X ')'                         
150301          DELIMITED BY SIZE INTO SSA1                                     
150401     MOVE '  GE' TO GODK-STATUSKODER                                      
150501     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-AREA-B601 SSA1                 
150601     MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
150701     PERFORM IMS-STATUSKONTROLL                                           
150801     IF SEGMENT-SAKNAS                                                    
150901         MOVE SPACE TO DCS-KDDC                                           
151001     END-IF                                                               
151101     .                                                                    
151201 IMS-STATUSKONTROLL SECTION.                                              
151301                                                                          
151401     SET STATUS-IX TO 1                                                   
151501     SEARCH GODK-STATUS AT END CALL FELLOG                                
151601     WHEN GODK-STATUS (STATUS-IX) = STATUS-WS CONTINUE                    
151701     END-SEARCH                                                           
151801     .                                                                    
152001     EJECT                                                                
