000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W4034600.                                                
000300 AUTHOR.         MARTIEN HOMPES.                                          
000400 DATE-WRITTEN.   96/06/19.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*PROGRAM FOR USE TO PRINT ON MARKPOINT WITH LABEL SIZE A6                 
000800*    FUNCTION:                                                            
000900*        PRINT COLLO LABEL BEFORE COLLO IS CREATED                        
001000*                                                                         
001100*        THE PROGRAM READS     WDE4                                       
001200*        THE PROGRAM READS     WLGMTA (WDB2)                              
001300*                                                                         
001400*    INDATA.                                                              
001500*        TRANSACTION: W4T346                                              
001600*        MID:         W4I34601                                            
001700*                                                                         
001800*    OUTDATA.                                                             
001900*        MOD:         W4O34601                                            
002000                                                                          
002100     SKIP3                                                                
002200 ENVIRONMENT DIVISION.                                                    
002300 DATA DIVISION.                                                           
002400                                                                          
002500     EJECT                                                                
002600 WORKING-STORAGE SECTION.                                                 
002700                                                                          
002800*    -- CHECKED BY WY2000                                                 
002900 77  IDPGM                       PIC X(08)   VALUE 'W4034600'.            
003000                                                                          
003100*    --- WORK FIELDS FOR ERROR MESSAGES WHEN CALLING ABEND/FELLOG         
003200 77  ERROR-TEXT                          PIC X(80) VALUE SPACE.           
003300                                                                          
003400 77  YES                         PIC X       VALUE 'Y'.                   
003500 77  NOO                         PIC X       VALUE 'N'.                   
003600                                                                          
003700*    --- WORK FIELDS FOR ACTUAL KEYVALUES OF SCREEN                       
003800 77  INDX                        PIC S9(4)  VALUE +0    COMP SYNC.        
003900 77  MAX-INDX                    PIC S9(4)  VALUE +5    COMP SYNC.        
004000                                                                          
004100                                                                          
004200 77  KEYS-SW                     PIC X       VALUE 'J'.                   
004300     88  KEYS-OK                             VALUE 'Y'.                   
004400     88  KEYS-WRONG                          VALUE 'N'.                   
004500                                                                          
004600 77  DUMMY-AREA                  PIC X(50)   VALUE SPACE.                 
004700 77  RAETT                       PIC X       VALUE 'R'.                   
004800 77  SPRAK-IX                    PIC S9(9)   VALUE +0   COMP SYNC.        
004900                                                                          
005000 77  W-IDTRANS                   PIC X(4)    VALUE SPACE.                 
005100     88  OWN-MID                             VALUE '4346'.                
005200     88  GOOD-MID                            VALUE '4346'.                
005300     88  HELP-MID                            VALUE '0551'.                
005400                                                                          
005500 77  LISTVAL                     PIC X(8)    VALUE SPACE.                 
005600     EJECT                                                                
005700 01  WORKFIELDS.                                                          
005800   03  WS-IDDISTR                           PIC 9(4).                     
005900   03  WS-IDKUNDNR                          PIC 9(6).                     
006000   03  WS-IDORDNR                           PIC X(5).                     
006100   03  WS-IDPRODNR                          PIC X(7).                     
006200   03  WS-IDKOLLI1                          PIC X(5).                     
006300   03  WS-IDKOLLI2                          PIC X(5).                     
006400   03  WS-IDKOLLI3                          PIC X(5).                     
006500   03  WS-IDKOLLI4                          PIC X(5).                     
006600   03  WS-IDKOLLI5                          PIC X(5).                     
006700   03  WS-ROUTE-PLATFORM                    PIC 9(002).                   
006800   03  WS-ROUTE-TOURNR                      PIC 9(003).                   
006900   03  WS-KDPRTVAL                          PIC X(002).                   
007000   03  WS-KDFRAKT                PIC S9(3)   VALUE ZERO.                  
007100*                                                                         
007200*      --- VALID IDDC CODES                                               
007300*                                                                         
007400*01    -COPY WWDC99                                                       
007500       EJECT                                                              
007600*                                                                         
007700 01  WS-ADGODSM-RAD2.                                                     
007800  03  WS-ADGODSM-RAD2-1.                                                  
007900   05  WS-ADGODSM-PLATFORM       PIC X(02) VALUE '00'.                    
008000   05  WS-ADGODSM-TRANSNR        PIC X(03) VALUE '000'.                   
008100  03  WS-ADGODSM-RAD2-2          PIC X(22) VALUE SPACE.                   
008200                                                                          
008300 01  WS-IDPRTLST.                                                         
008400     03 WS-SYSTDEL               PIC X(1).                                
008500     03 WS-LISTTYP               PIC X(2).                                
008600     03 WS-DC                    PIC X(2).                                
008700     03 WS-KDPRT                 PIC X(3).                                
008800                                                                          
008900     EJECT                                                                
009000*    --- SUBPROGRAM OCH PARAMETERAREOR                                    
009100 01  GENERAL-SUBPROGRAM.                                                  
009200     03  W006PRS1                PIC X(8)    VALUE 'W006PRS1'.            
009300     03  W006PRT                 PIC X(8)    VALUE 'W006PRT '.            
009400     03  WMEDKONV                PIC X(8)    VALUE 'WMEDKONV'.            
009500     03  W005INIT                PIC X(8)    VALUE 'W005INIT'.            
009600     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
009700     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
009800     EJECT                                                                
009900*    --- PARAMETERS FOR SUBPROGRAM WMEDKONV                               
010000*01 -COPY WMEDAREA                                                        
010100     SKIP3                                                                
010200 01  MESSAGE-CODES.                                                       
010300     03  DC-MISSING              PIC X(3)    VALUE '026'.                 
010400     03  ORDER-MISSING           PIC X(3)    VALUE '054'.                 
010500     03  ERR-WRONG-KEY           PIC X(3)    VALUE '401'.                 
010600     03  WRONG-PRINTER           PIC X(3)    VALUE '772'.                 
010700     03  CASE-LABEL-PRINTED      PIC X(3)    VALUE '790'.                 
010800     EJECT                                                                
010900*    --- PARAMETERS FOR SUB PROGRAM W005INIT                              
011000*                                                                         
011100 01  FILLER                      PIC X(16)   VALUE 'WMSGINIT'.            
011200     SKIP3                                                                
011300*01 -COPY WMSGINIT                                                        
011400     SKIP3                                                                
011500*    --- PARAMETERS FOR SUB PROGRAM W006PRT                               
011600*                                                                         
011700 01  FILLER                      PIC X(16)  VALUE 'W006PRT  '.            
011800*   -COPY W006PRT                                                         
011900     SKIP3                                                                
012000     EJECT                                                                
012100*                                                                         
012200*    --- PRINT LINES FOR PRINTER                                          
012300*                                                                         
012400 01  FILLER               PIC X(16)  VALUE 'DATA PRINTLINE  '.            
012500*    --- GENERAL PRINT DATA FOR PRINT LINE INFO                           
012600 01  PRINT-DATA.                                                          
012700     03 PRINT-IDDISTR            PIC 9(004).                              
012800     03 PRINT-IDKUNDNR           PIC 9(006).                              
012900     03 PRINT-IDORDNR            PIC 9(007).                              
013000     03 PRINT-IDPRODNR           PIC 9(007).                              
013100     03 PRINT-KDFRAKT            PIC 9(003).                              
013200     03 PRINT-IDKOLLI OCCURS 5   PIC 9(005).                              
013300     03 PRINT-BEGMT-RAD1         PIC X(035).                              
013400     03 PRINT-BEGMT-RAD2         PIC X(035).                              
013500     03 PRINT-ABGMT-GATA         PIC X(035).                              
013600     03 PRINT-ABGMT-PADR         PIC X(035).                              
013700     03 PRINT-ROUTE-PLATFORM     PIC 9(002).                              
013800     03 PRINT-ROUTE-TOURNR       PIC 9(003).                              
013900                                                                          
014000     EJECT                                                                
014100                                                                          
014200 01  FILLER          PIC X(24)  VALUE 'MARKP PRINT MAASTRICHT'.           
014300*    --- CASE LABEL A6 FORMAT FOR MARKPOINT PRINTER MAASTRICHT            
014400*                                                                         
014500*****************************************************************         
014600*    AREA MED STYRTECKEN FÖR MARKPOINT TERMO SKRIVARE.          *         
014700*    ANV. FÖR ATT SKRIVA KOLLIFLAGGA I A6 FORMAT I SDC-21,      *         
014800*    SDC-23 OCH SDC-26.                                         *         
014900*****************************************************************         
015000 01  FILLER           PIC X(24)  VALUE 'KOLLI-FLA6 TERMO'.                
015100*    STYRTECKEN ENLIGT MANUAL: MARKPOINT THERMAL PRINTER                  
015200*                              LABELPOINT                                 
015300 01  CASE-LABEL-THERMO-A6.                                                
015400   03  A6-RAD        PIC X(132)  VALUE SPACE.                             
015500                                                                          
015600   03  A6-STYR-01.                                                        
015700     05  FILLER      PIC X(3)  VALUE '!CÅ'.                               
015800                                                                          
015900   03  A6-STYR-91.                                                        
016000     05  FILLER      PIC X(3)  VALUE '!PÅ'.                               
016100                                                                          
016200   03  A6-RUB-DISTRICT.                                                   
016300     05  FILLER      PIC X(25) VALUE '!F T N   30  300 L 1 1 3 '.         
016400     05  FILLER      PIC X(11) VALUE '"DISTRICT"Å'.                       
016500                                                                          
016600   03  A6-RUB-CUSTOMER.                                                   
016700     05  FILLER      PIC X(25) VALUE '!F T N   30  800 L 1 1 3 '.         
016800     05  FILLER      PIC X(11) VALUE '"CUSTOMER"Å'.                       
016900                                                                          
017000   03  A6-RUB-ORDERNUMBER.                                                
017100     05  FILLER      PIC X(25) VALUE '!F T N   30 1280 L 1 1 3 '.         
017200     05  FILLER      PIC X(15) VALUE '"ORDER NUMBER"Å'.                   
017300                                                                          
017400   03  A6-RUB-ADDRESS.                                                    
017500     05  FILLER      PIC X(25) VALUE '!F T N  210   80 L 1 1 3 '.         
017600     05  FILLER      PIC X(10) VALUE '"ADDRESS"Å'.                        
017700                                                                          
017800   03  A6-RUB-CASE.                                                       
017900     05  FILLER      PIC X(25) VALUE '!F T N  210 1400 L 1 1 3 '.         
018000     05  FILLER      PIC X(7)  VALUE '"CASE"Å'.                           
018100                                                                          
018200   03  A6-RUB-FREIGHTCODE.                                                
018300     05  FILLER      PIC X(25) VALUE '!F T N  360 1280 L 1 1 3 '.         
018400     05  FILLER      PIC X(15) VALUE '"FREIGHT CODE"Å'.                   
018500                                                                          
018600   03  A6-RUB-WEIGHT-KG.                                                  
018700     05  FILLER      PIC X(25) VALUE '!F T N  560 1300 L 1 1 3 '.         
018800     05  FILLER      PIC X(12)  VALUE '"WEIGHT KG"Å'.                     
018900                                                                          
019000   03  A6-RUB-IDPRODNR.                                                   
019100     05  FILLER      PIC X(25) VALUE '!F T N  740 1180 L 1 1 3 '.         
019200     05  FILLER      PIC X(20) VALUE '"PRODUCTION NUMBER"Å'.              
019300                                                                          
019400   03  A6-RUB-PLATFORM.                                                   
019500     05  FILLER      PIC X(25) VALUE '!F T N  840  180 L 1 1 3 '.         
019600     05  FILLER      PIC X(11) VALUE '"PLATFORM"Å'.                       
019700                                                                          
019800   03  A6-DATA-IDDISTR.                                                   
019900     05  FILLER      PIC X(25) VALUE '!F T N  160  600 R 2 2 6 '.         
020000     05  FILLER      PIC X(1)  VALUE '"'.                                 
020100     05  A6-IDDISTR  PIC Z(4)  VALUE ZERO.                                
020200     05  FILLER      PIC X(2)  VALUE '"Å'.                                
020300                                                                          
020400   03  A6-DATA-IDKUNDNR.                                                  
020500     05  FILLER      PIC X(25) VALUE '!F T N  160 1100 R 2 2 6 '.         
020600     05  FILLER      PIC X(1)  VALUE '"'.                                 
020700     05  A6-IDKUNDNR PIC Z(5)9 VALUE ZERO.                                
020800     05  FILLER      PIC X(2)  VALUE '"Å'.                                
020900                                                                          
021000   03  A6-DATA-IDORDNR.                                                   
021100     05  FILLER      PIC X(25) VALUE '!F T N  160 1600 R 2 2 6 '.         
021200     05  FILLER      PIC X(1)  VALUE '"'.                                 
021300     05  A6-IDORDNR  PIC Z(5)9 VALUE SPACE.                               
021400     05  FILLER      PIC X(2)  VALUE '"Å'.                                
021500                                                                          
024410   03  A6-DATA-ADRESS1.                                                   
024420     05  FILLER      PIC X(25) VALUE '!F T N  320  80  L 8 3 1 '.         
024430     05  FILLER      PIC X(1)  VALUE '"'.                                 
024440     05  A6-ADRESS-1 PIC X(30) VALUE SPACE.                               
024450     05  FILLER      PIC X(2)  VALUE '"Å'.                                
024460                                                                          
024470   03  A6-DATA-ADRESS2.                                                   
024480     05  FILLER      PIC X(25) VALUE '!F T N  440  80  L 8 3 1 '.         
024490     05  FILLER      PIC X(1)  VALUE '"'.                                 
024491     05  A6-ADRESS-2 PIC X(30) VALUE SPACE.                               
024492     05  FILLER      PIC X(2)  VALUE '"Å'.                                
024493                                                                          
024494   03  A6-DATA-ADRESS3.                                                   
024495     05  FILLER      PIC X(25) VALUE '!F T N  560  80  L 8 3 1 '.         
024496     05  FILLER      PIC X(1)  VALUE '"'.                                 
024497     05  A6-ADRESS-3 PIC X(30) VALUE SPACE.                               
024498     05  FILLER      PIC X(2)  VALUE '"Å'.                                
024499                                                                          
024500   03  A6-DATA-ADRESS4.                                                   
024501     05  FILLER      PIC X(25) VALUE '!F T N  680  80  L 8 3 1 '.         
024502     05  FILLER      PIC X(1)  VALUE '"'.                                 
024503     05  A6-ADRESS-4 PIC X(30) VALUE SPACE.                               
024504     05  FILLER      PIC X(2)  VALUE '"Å'.                                
024505                                                                          
024506   03  A6-DATA-ADRESS5.                                                   
024507     05  FILLER      PIC X(25) VALUE '!F T N  800  80  L 8 3 1 '.         
024508     05  FILLER      PIC X(1)  VALUE '"'.                                 
024509     05  A6-ADRESS-5 PIC X(30) VALUE SPACE.                               
024510     05  FILLER      PIC X(2)  VALUE '"Å'.                                
024520                                                                          
024600   03  A6-DATA-IDKOLLI.                                                   
024700     05  FILLER      PIC X(25) VALUE '!F T N  330 1600 R 2 2 6 '.         
024800     05  FILLER      PIC X(1)  VALUE '"'.                                 
024900     05  A6-IDKOLLI  PIC Z(5)  VALUE ZERO.                                
025000     05  FILLER      PIC X(2)  VALUE '"Å'.                                
025100                                                                          
025200   03  A6-DATA-KDFRAKT.                                                   
025300     05  FILLER      PIC X(25) VALUE '!F T N  500 1600 R 2 2 6 '.         
025400     05  FILLER      PIC X(1)  VALUE '"'.                                 
025500     05  A6-KDFRAKT  PIC Z9    VALUE ZERO.                                
025600     05  FILLER      PIC X(2)  VALUE '"Å'.                                
025700                                                                          
025800   03  A6-DATA-WEIGHT.                                                    
025900     05  FILLER      PIC X(25) VALUE '!F T N  690 1590 R 2 2 6 '.         
026000     05  FILLER      PIC X(1)  VALUE '"'.                                 
026100     05  A6-VKORDBTO PIC Z(5)  VALUE ZERO.                                
026200     05  FILLER      PIC X(2)  VALUE '"Å'.                                
026300                                                                          
026400   03  A6-DATA-WEIGHT-KILO-HEKTO.                                         
026500     05  FILLER      PIC X(25) VALUE '!F T N  720 1590 R 2 2 6 '.         
026600     05  FILLER      PIC X(1)  VALUE '"'.                                 
026700     05  A6-KILO     PIC Z(5)  VALUE ZERO.                                
026800     05  A6-PUNKT    PIC X     VALUE '.'.                                 
026900     05  A6-HEKTO    PIC 9     VALUE ZERO.                                
027000     05  FILLER      PIC X(2)  VALUE '"Å'.                                
027100                                                                          
027200   03  A6-DATA-IDPRODNR.                                                  
027300     05  FILLER      PIC X(25) VALUE '!F T N  880 1590 R 2 2 6 '.         
027400     05  FILLER      PIC X(1)  VALUE '"'.                                 
027500     05  A6-IDPRODNR PIC Z(7)  VALUE ZERO.                                
027600     05  FILLER      PIC X(2)  VALUE '"Å'.                                
027700                                                                          
027800   03  A6-TEXT-VOR.                                                       
027900     05  FILLER      PIC X(25) VALUE '!F T N  860  700 R 2 2 6 '.         
028000     05  FILLER      PIC X(1)  VALUE '"'.                                 
028100     05  A6-KDORDKL-TXT PIC X(3) VALUE SPACE.                             
028200     05  FILLER      PIC X(2)  VALUE '"Å'.                                
028300                                                                          
028400   03  A6-TEXT-INT.                                                       
028500     05  FILLER      PIC X(25) VALUE '!F T N  860  700 R 2 2 6 '.         
028600     05  FILLER      PIC X(1)  VALUE '"'.                                 
028700     05  A6-IDPRCVAR-TXT PIC X(3) VALUE SPACE.                            
028800     05  FILLER      PIC X(2)  VALUE '"Å'.                                
028900                                                                          
029000   03  A6-DATA-PLATFORM.                                                  
029100     05  FILLER      PIC X(25) VALUE '!F T N  860  700 R 2 2 6 '.         
029200     05  A6-PLATFORM PIC 9(02) VALUE ZERO.                                
029300     05  A6-SLASH    PIC X(01) VALUE '/'.                                 
029400     05  A6-TRANS    PIC 9(03) VALUE ZERO.                                
029500     05  FILLER      PIC X(2)  VALUE '"Å'.                                
029600                                                                          
029700   03  A6-BARCODE-SDC21.                                                  
029800     05  FILLER    PIC X(30) VALUE '!F C N 1040 200 L 140 2 12 '.         
029900     05  FILLER      PIC X(1)  VALUE '"'.                                 
030100     05  A6-DISTR    PIC 9(4)  VALUE ZERO.                                
030300     05  A6-KUNDNR   PIC 9(6)  VALUE ZERO.                                
030500     05  A6-ORDNR    PIC 9(7)  VALUE ZERO.                                
030700     05  A6-KOLLI    PIC 9(5)  VALUE ZERO.                                
030800     05  FILLER      PIC X(2)  VALUE '"Å'.                                
030900                                                                          
031000   03  A6-BARCODE-TXT-SDC21.                                              
031100     05  FILLER    PIC X(30) VALUE '!F T N 1080  600 L  2 1 13 '.         
031200     05  FILLER      PIC X(1)  VALUE '"'.                                 
031400     05  A6-DISTR-TXT PIC 9(4) VALUE ZERO.                                
031600     05  A6-KUNDNR-TXT PIC 9(6) VALUE ZERO.                               
031800     05  A6-ORDNR-TXT  PIC 9(7) VALUE ZERO.                               
032000     05  A6-KOLLI-TXT  PIC 9(5) VALUE ZERO.                               
032100     05  FILLER        PIC X(2) VALUE '"Å'.                               
032200                                                                          
032900   03  A6-DATA-PRINT-DATE.                                                
033000     05  FILLER      PIC X(25) VALUE '!F T N 1000 1380 L 1 1 3 '.         
033100     05  FILLER      PIC X(1)  VALUE '"'.                                 
033110     05  FILLER      PIC X(5)  VALUE 'DATE:'.                             
033200     05  A6-DATE     PIC 9(6)  VALUE ZERO.                                
033300     05  FILLER      PIC X(2)  VALUE '"Å'.                                
033400                                                                          
033410   03  A6-DATA-PRINT-TIME.                                                
033420     05  FILLER      PIC X(25) VALUE '!F T N 1050 1380 L 1 1 3 '.         
033430     05  FILLER      PIC X(1)  VALUE '"'.                                 
033431     05  FILLER      PIC X(5)  VALUE 'TIME:'.                             
033440     05  A6-TIME     PIC 9(8)  VALUE ZERO.                                
033450     05  FILLER      PIC X(2)  VALUE '"Å'.                                
033460                                                                          
033500     EJECT                                                                
033600******************************************************************        
033700*    --- AREAS FOR MFS AND SCREEN MANAGEMENT                              
033800*                                                                         
033900 01  FILLER                      PIC X(16)   VALUE 'MID-AREA'.            
034000                                                                          
034100*01  MID -COPY W4I34601                                                   
034200     EJECT                                                                
034300 01  FILLER                      PIC X(16)  VALUE 'MSG/MOD-AREA'.         
034400                                                                          
034500*01  -COPY WMSGAREA                                                       
034600     EJECT                                                                
034700     03  MOD REDEFINES MSG-AREA.                                          
034800*      05  -COPY W4O34601                                                 
034900     EJECT                                                                
035000*01  -COPY W006PRAR                                                       
035100     EJECT                                                                
035200 01  FILLER                      PIC X(16)   VALUE 'MFS-AREA'.            
035300                                                                          
035400*01  -COPY WMFSAREA                                                       
035500     EJECT                                                                
035600*    --- WORK-AREAS FOR IMS-SECTIONS                                      
035700*                                                                         
035800     EJECT                                                                
035900 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
036000                                                                          
036100 01  KEYS-TO-DLI.                                                         
036200   03  W-IDPROD-X.                                                        
036300     05  W-IDPROD                PIC S9(7)   VALUE ZERO COMP-3.           
036400   03  W-IDGMT-X.                                                         
036500     05  W-IDDISTR               PIC S9(5)   VALUE ZERO COMP-3.           
036600     05  W-IDKUNDNR              PIC S9(7)   VALUE ZERO COMP-3.           
036700   03  W-WDE4B-KEYSEQ-MIN-X.                                              
036800     05  W-420-IDPRODNR-MIN      PIC S9(7)   VALUE ZERO  COMP-3.          
036900     05  W-420-IDPURAD-MIN       PIC S9(5)   VALUE ZERO  COMP-3.          
037000*                                                                         
037100   03  W-WDE4B-KEYSEQ-MAX-X.                                              
037200     05  W-420-IDPRODNR-MAX      PIC S9(7)   VALUE ZERO  COMP-3.          
037300     05  W-420-IDPURAD-MAX       PIC S9(5)   VALUE ZERO  COMP-3.          
037400*                                                                         
037500   03  W-WDE4B-KEYSEQ-X.                                                  
037600     05  W-420-IDPRODNR          PIC S9(7)   VALUE ZERO  COMP-3.          
037700     05  W-420-IDPURAD           PIC S9(5)   VALUE ZERO  COMP-3.          
037800     SKIP2                                                                
037900 01    MEDDELANDE.                                                        
038000*                                                                         
038100   03    FEL-02.                                                          
038200     05  FILLER                  PIC X(40)   VALUE                        
038300        ' PRODUCTION NUMBER DOESNT BELONG TO DC '.                        
038400     05  FILLER                  PIC X(40)   VALUE                        
038500        ' PRODUCTION NUMBER DOESNT BELONG TO DC '.                        
038600   03    FILLER  REDEFINES  FEL-02.                                       
038700     05  FEL-796     OCCURS 2    PIC X(40).                               
038800     SKIP2                                                                
038900     EJECT                                                                
039000*    --- STATUS-KOD FRÅN IMS                                              
039100 01  STATUS-WS                   PIC XX.                                  
039200     88  SEGMENT-FOUND                       VALUE '  '.                  
039300     88  SEGMENT-FOUND-EXISTS                VALUE 'II'.                  
039400     88  SEGMENT-MISSING                     VALUE 'GE'.                  
039500     SKIP2                                                                
039600 01  GOOD-STATUSCODES.                                                    
039700     03  GOOD-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
039800     SKIP3                                                                
039900 01  SSA1                        PIC X(64).                               
040000 01  SSA2                        PIC X(64).                               
040100     EJECT                                                                
040200*    --- IMS FUNCTION CODES                                               
040300*01  -COPY W0003                                                          
040400     EJECT                                                                
040500*    ---  DLI INPUT-OUTPUT AREA                                           
040600 01  FILLER                  PIC X(16)   VALUE 'DLI-IO-WDE401'.           
040700 01  DLI-IO-WDE401.                                                       
040800*  03  -COPY WDE401.                                                      
040900                                                                          
041000     EJECT                                                                
041100 01  FILLER                  PIC X(16)   VALUE 'DLI-IO-GMTA01'.           
041200 01  DLI-IO-GMTA01.                                                       
041300*  03  -COPY WDB201.                                                      
041400                                                                          
041500     EJECT                                                                
041600 LINKAGE SECTION.                                                         
041700*01  -COPY W0009  -PRE MSG-                                               
041800     EJECT                                                                
041900*01  -COPY W0009  -PRE ALT-                                               
042000     EJECT                                                                
042100*01  -COPY W0008  -PRE USEA-                                              
042200     05  FILLER                  PIC X.                                   
042300     EJECT                                                                
042400*01  -COPY W0008  -PRE WDE4-                                              
042500     05  FILLER                  PIC X.                                   
042600     EJECT                                                                
042700*01  -COPY W0008  -PRE GMTA-                                              
042800     05  FILLER                  PIC X.                                   
042900     EJECT                                                                
043000 PROCEDURE DIVISION  USING MSG-PCB ALT-PCB  USEA-PCB                      
043100                                   WDE4-PCB GMTA-PCB.                     
043200 MAIN SECTION.                                                            
043300     ENTRY 'DLITCBL' USING MSG-PCB ALT-PCB  USEA-PCB                      
043400                                   WDE4-PCB GMTA-PCB.                     
043500                                                                          
043600     PERFORM IMS-GET-MSG                                                  
043700     IF SEGMENT-FOUND                                                     
043800       PERFORM A-INIT                                                     
043900       IF GOOD-MID OR HELP-MID                                            
044000         PERFORM B-CHECK-KEYS                                             
044100         IF KEYS-OK                                                       
044200           PERFORM C-CHECK-KEYS                                           
044300           IF KEYS-OK                                                     
044400             PERFORM D-GET-PRINT-INFO                                     
044500             PERFORM E-PRINT-LABEL                                        
044600           END-IF                                                         
044700         END-IF                                                           
044800       END-IF                                                             
044900       COMPUTE MSG-KVLL = LENGTH OF MOD-W4O34601 + 4                      
045000       PERFORM IMS-INSERT-MSG                                             
045100     END-IF                                                               
045200                                                                          
045300     MOVE ZERO TO RETURN-CODE                                             
045400     GOBACK                                                               
045500     .                                                                    
045600                                                                          
045700     EJECT                                                                
045800 A-INIT SECTION.                                                          
045900                                                                          
046000     IF MSG-DOUBLE-TRANSACTIONS                                           
046100       MOVE MSG-INDATA-MINUS-2-TRANSACT   TO MID-W4I34601                 
046200       MOVE MSG-IDTRANS-2  TO MFS-IDTRANS                                 
046300       MOVE MSG-KDMFSFOR-2 TO MFS-KDMFSFOR                                
046400     ELSE                                                                 
046500       MOVE MSG-INDATA-MINUS-1-TRANSACT  TO MID-W4I34601                  
046600       MOVE MSG-IDTRANS-1  TO MFS-IDTRANS                                 
046700       MOVE MSG-KDMFSFOR-1 TO MFS-KDMFSFOR                                
046800     END-IF                                                               
046900                                                                          
047000     MOVE MSG-KDTRTYP TO MFS-KDTRTYP                                      
047100     MOVE MSG-IDPFK   TO MFS-IDPFK                                        
047200     MOVE MFS-IDTRANS TO W-IDTRANS                                        
047300                                                                          
047400     MOVE LOW-VALUE TO MSG-AREA                                           
047500     MOVE 'W4O346N1' TO MFS-IDMOD                                         
047600     MOVE '4346' TO MOD-IDTRANS                                           
047700     MOVE MFS-ERASE-FIELD TO MOD-TEMFSFEL MOD-TEMFSINF                    
047800                                                                          
047900     IF OWN-MID OR HELP-MID                                               
048000       CONTINUE                                                           
048100     ELSE                                                                 
048200       MOVE SPACE TO MFS-KDTRTYP                                          
048300       MOVE '7' TO MFS-IDPFK                                              
048400       PERFORM MFS-ERASE-FIELD-IN                                         
048500     END-IF                                                               
048600     .                                                                    
048700     EJECT                                                                
048800 B-CHECK-KEYS SECTION.                                                    
048900                                                                          
049000     MOVE ALL '+'           TO MSGI-WMSGINIT                              
049100     MOVE '001'             TO MSGI-KDCALL                                
049200     MOVE MSG-LTERM-NAME    TO MSGI-IDLTERM-USER                          
049300     MOVE MSG-SIGNON-USERID TO MSGI-IDUSER                                
049400     MOVE '4346'            TO MSGI-IDTRANS                               
049500                                                                          
049600     CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                           
049700                                                                          
049800     MOVE YES TO KEYS-SW                                                  
049900     MOVE MSGI-IDLAND-SPR   TO MED-IDSKYLT                                
050000                                                                          
050100     IF MSGI-IDLAND-SPR = 'GB'                                            
050200       MOVE +2 TO SPRAK-IX                                                
050300     ELSE                                                                 
050400       MOVE +1 TO SPRAK-IX                                                
050500     END-IF                                                               
050600                                                                          
050700*    -- CHECK OF IDPROD                                                   
050800     MOVE MFS-ERASE-FIELD TO MOD-IDPRODNR-IN                              
050900                                                                          
051000     IF MID-IDPRODNR-IN = ALL '+'                                         
051100       MOVE MID-IDPRODNR-UT             TO   MOD-IDPRODNR-UT              
051200       MOVE MID-IDPRODNR-UT             TO   WS-IDPRODNR                  
051300       INSPECT WS-IDPRODNR REPLACING LEADING SPACE BY ZERO                
051400     ELSE                                                                 
051500       MOVE MID-IDPRODNR-IN             TO   WS-IDPRODNR                  
051600                                             MOD-IDPRODNR-UT              
051700     END-IF                                                               
051710     INSPECT MOD-IDPRODNR-UT REPLACING LEADING ZERO BY SPACE              
051800                                                                          
051900     MOVE MFS-ERASE-FIELD TO MOD-IDKOLLI-1                                
052000     IF MID-IDKOLLI-1 = ALL '+'                                           
052100       MOVE ZEROES                       TO   WS-IDKOLLI1                 
052200     ELSE                                                                 
052300       IF MID-IDKOLLI-1 NUMERIC                                           
052400         MOVE MID-IDKOLLI-1               TO   WS-IDKOLLI1                
052500       ELSE                                                               
052600         MOVE NOO                         TO   KEYS-SW                    
052700       END-IF                                                             
052800     END-IF                                                               
052900                                                                          
053000     MOVE MFS-ERASE-FIELD TO MOD-IDKOLLI-2                                
053100     IF MID-IDKOLLI-2 = ALL '+'                                           
053200        MOVE ZEROES       TO   WS-IDKOLLI2                                
053300     ELSE                                                                 
053400       IF MID-IDKOLLI-2 NUMERIC                                           
053500         MOVE MID-IDKOLLI-2  TO   WS-IDKOLLI2                             
053600       ELSE                                                               
053700         MOVE NOO            TO   KEYS-SW                                 
053800       END-IF                                                             
053900     END-IF                                                               
054000*                                                                         
054100     MOVE MFS-ERASE-FIELD TO MOD-IDKOLLI-3                                
054200     IF MID-IDKOLLI-3 = ALL '+'                                           
054300       MOVE ZEROES           TO   WS-IDKOLLI3                             
054400     ELSE                                                                 
054500       IF MID-IDKOLLI-3 NUMERIC                                           
054600         MOVE MID-IDKOLLI-3  TO   WS-IDKOLLI3                             
054700       ELSE                                                               
054800         MOVE NOO            TO   KEYS-SW                                 
054900       END-IF                                                             
055000     END-IF                                                               
055100*                                                                         
055200     MOVE MFS-ERASE-FIELD TO MOD-IDKOLLI-4                                
055300     IF MID-IDKOLLI-4 = ALL '+'                                           
055400        MOVE ZEROES           TO   WS-IDKOLLI4                            
055500     ELSE                                                                 
055600       IF MID-IDKOLLI-4 NUMERIC                                           
055700         MOVE MID-IDKOLLI-4   TO   WS-IDKOLLI4                            
055800       ELSE                                                               
055900         MOVE NOO             TO   KEYS-SW                                
056000       END-IF                                                             
056100     END-IF                                                               
056200*                                                                         
056300     MOVE MFS-ERASE-FIELD TO MOD-IDKOLLI-5                                
056400     IF MID-IDKOLLI-5 = ALL '+'                                           
056500        MOVE ZEROES           TO   WS-IDKOLLI5                            
056600     ELSE                                                                 
056700       IF MID-IDKOLLI-5 NUMERIC                                           
056800         MOVE MID-IDKOLLI-5   TO   WS-IDKOLLI5                            
056900       ELSE                                                               
057000         MOVE NOO             TO   KEYS-SW                                
057100       END-IF                                                             
057200     END-IF                                                               
057300*                                                                         
057400     MOVE MFS-ERASE-FIELD TO MOD-PRTVAL-ADRESSFL-IN                       
057500     IF MID-PRTVAL-IN = ALL '+'                                           
057600       MOVE MID-PRTVAL-UT TO MOD-PRTVAL-ADRESSFL-UT                       
057700                             WS-KDPRTVAL                                  
057800     ELSE                                                                 
057900       MOVE MID-PRTVAL-IN TO MOD-PRTVAL-ADRESSFL-UT                       
058000                             WS-KDPRTVAL                                  
058100     END-IF                                                               
058200*                                                                         
058300*    -- FILL  IDDC                                                        
058400     MOVE MFS-ERASE-FIELD TO MOD-IDDC-IN                                  
058500                                                                          
058600     MOVE MSGI-IDDC                   TO WS-IDDC                          
058700                                                                          
058800     IF WS-IDPRODNR NOT NUMERIC                                           
058900       MOVE NOO                       TO   KEYS-SW                        
059000     END-IF                                                               
059100                                                                          
059200     IF WS-IDDC IS > SPACE                                                
059300       MOVE WS-IDDC                   TO MOD-IDDC-UT                      
059400     ELSE                                                                 
059500       MOVE NOO                       TO   KEYS-SW                        
059600     END-IF                                                               
059700                                                                          
059800     IF KEYS-WRONG                                                        
059900       MOVE ERR-WRONG-KEY TO MED-IDMFSFEL                                 
060000       CALL WMEDKONV USING MED-WMEDAREA                                   
060100       MOVE MED-MFSFEL TO MOD-TEMFSFEL                                    
060200     END-IF                                                               
060300     .                                                                    
060400     EJECT                                                                
060500 C-CHECK-KEYS SECTION.                                                    
060600                                                                          
060700     MOVE WS-IDPRODNR          TO W-420-IDPRODNR-MIN                      
060800                                  W-420-IDPRODNR-MAX                      
060900                                  W-420-IDPRODNR                          
061000     MOVE 1                    TO W-420-IDPURAD-MIN                       
061100                                  W-420-IDPURAD                           
061200     MOVE 99999                TO W-420-IDPURAD-MAX                       
061300     PERFORM IMS-GU-KUNDORDER-SEK-INV                                     
061400     IF SEGMENT-FOUND                                                     
061500       IF KORD-IDDC = WS-IDDC                                             
061600         MOVE KORD-IDDISTR     TO WS-IDDISTR                              
061700         MOVE KORD-IDKUNDNR    TO WS-IDKUNDNR                             
061800         MOVE KORD-IDORDNR5    TO WS-IDORDNR                              
061900         MOVE KORD-KDFRAKT     TO WS-KDFRAKT                              
062000       ELSE                                                               
062100         MOVE NOO              TO KEYS-SW                                 
062200         MOVE FEL-796 (SPRAK-IX) TO MOD-TEMFSFEL                          
062300       END-IF                                                             
062400     ELSE                                                                 
062500       MOVE NOO                TO   KEYS-SW                               
062600       MOVE ORDER-MISSING      TO MED-IDMFSFEL                            
062700       CALL WMEDKONV USING MED-WMEDAREA                                   
062800       MOVE MED-MFSFEL         TO MOD-TEMFSFEL                            
062900     END-IF                                                               
063000                                                                          
063100     IF KEYS-OK                                                           
063200       MOVE '4'                TO WS-SYSTDEL                              
063300       MOVE 'KF'               TO WS-LISTTYP                              
063400       MOVE WS-IDDC            TO WS-DC                                   
063500       MOVE WS-KDPRTVAL        TO WS-KDPRT                                
063600                                                                          
063700       MOVE 001                TO PRT-KDCALL                              
063800       MOVE WS-IDPRTLST        TO PRT-IDPRTLST                            
063900                                                                          
064000       CALL W006PRT USING PRT-W006PRT                                     
064100                                                                          
064200       IF PRT-KDSVAR NOT = RAETT                                          
064300         MOVE NOO              TO KEYS-SW                                 
064400         MOVE WRONG-PRINTER    TO MED-IDMFSFEL                            
064500         CALL WMEDKONV USING MED-WMEDAREA                                 
064600         MOVE MED-MFSFEL       TO MOD-TEMFSFEL                            
064700       END-IF                                                             
064800     END-IF                                                               
064900     .                                                                    
065000     EJECT                                                                
065100 D-GET-PRINT-INFO SECTION.                                                
065200                                                                          
065300     ACCEPT  A6-DATE           FROM DATE                                  
065400     ACCEPT  A6-TIME           FROM TIME                                  
065500                                                                          
065600     MOVE WS-IDDISTR           TO PRINT-IDDISTR                           
065700                                  W-IDDISTR                               
065800     MOVE WS-IDKUNDNR          TO PRINT-IDKUNDNR                          
065900                                  W-IDKUNDNR                              
066000     MOVE WS-IDORDNR           TO PRINT-IDORDNR                           
066100     MOVE WS-IDPRODNR          TO PRINT-IDPRODNR                          
066200     MOVE WS-KDFRAKT           TO PRINT-KDFRAKT                           
066300     PERFORM  IMS-GU-GMTA01-WDB201                                        
066400     IF SEGMENT-FOUND                                                     
066500       MOVE GMT-BEGMT-RAD1         TO PRINT-BEGMT-RAD1                    
066600*START FIX P.G.A. ATT KUNDREGISTRET (SE BILD 4411) KAN VARA IFYLLD        
066700*MED GATA PÅ BEGMT-RAD2, POSTADR. PÅ ADGMT-GATA OCH LAND PÅ PADR.         
066800       IF GMT-ADGMT-LAND = ALL SPACE                                      
066900         MOVE GMT-BEGMT-RAD2       TO PRINT-BEGMT-RAD2                    
067000         MOVE GMT-ADGMT-GATA       TO PRINT-ABGMT-GATA                    
067100         MOVE GMT-ADGMT-PADR       TO PRINT-ABGMT-PADR                    
067200                                      WS-ADGODSM-RAD2                     
067300       ELSE                                                               
067400         MOVE GMT-ADGMT-GATA       TO PRINT-BEGMT-RAD2                    
067500         MOVE GMT-ADGMT-PADR       TO PRINT-ABGMT-GATA                    
067600         MOVE GMT-ADGMT-LAND       TO PRINT-ABGMT-PADR                    
067700                                      WS-ADGODSM-RAD2                     
067800       END-IF                                                             
067900*END FIX                                                                  
068000       IF W-IDDISTR = 2278 AND WS-ADGODSM-PLATFORM NUMERIC AND            
068100         WS-ADGODSM-PLATFORM NUMERIC                                      
068200         MOVE WS-ADGODSM-PLATFORM TO PRINT-ROUTE-PLATFORM                 
068300         MOVE WS-ADGODSM-TRANSNR  TO PRINT-ROUTE-TOURNR                   
068400       ELSE                                                               
068500         MOVE ZEROES              TO PRINT-ROUTE-PLATFORM                 
068600                                        PRINT-ROUTE-TOURNR                
068700       END-IF                                                             
068800     ELSE                                                                 
068900       MOVE SPACES              TO PRINT-BEGMT-RAD1                       
069000                                   PRINT-BEGMT-RAD2                       
069100                                   PRINT-ABGMT-GATA                       
069200                                   PRINT-ABGMT-PADR                       
069300       MOVE ZEROES              TO PRINT-ROUTE-PLATFORM                   
069400                                   PRINT-ROUTE-TOURNR                     
069500     END-IF                                                               
069600     MOVE WS-IDKOLLI1             TO PRINT-IDKOLLI(1)                     
069700     MOVE WS-IDKOLLI2             TO PRINT-IDKOLLI(2)                     
069800     MOVE WS-IDKOLLI3             TO PRINT-IDKOLLI(3)                     
069900     MOVE WS-IDKOLLI4             TO PRINT-IDKOLLI(4)                     
070000     MOVE WS-IDKOLLI5             TO PRINT-IDKOLLI(5)                     
070100     .                                                                    
070200                                                                          
070300     EJECT                                                                
070400 E-PRINT-LABEL             SECTION.                                       
070500                                                                          
070600     MOVE PRT-IDPRTLST    TO LISTVAL                                      
070700                                                                          
070800     EVALUATE TRUE                                                        
070900       WHEN SDC-NL                                                        
071000         MOVE +1               TO INDX                                    
071100         PERFORM UNTIL INDX > MAX-INDX                                    
071200           IF PRINT-IDKOLLI(INDX) > 0                                     
071300             PERFORM S01-PRT-MARKPOINT-TERMO-A6                           
071400           END-IF                                                         
071500           ADD 1      TO    INDX                                          
071600         END-PERFORM                                                      
071700       WHEN OTHER                                                         
071800         CONTINUE                                                         
071900     END-EVALUATE                                                         
072000     MOVE CASE-LABEL-PRINTED       TO MED-IDMFSFEL                        
072100     CALL WMEDKONV USING MED-WMEDAREA                                     
072200     MOVE MED-MFSFEL               TO MOD-TEMFSINF                        
072300     .                                                                    
072400                                                                          
072500     EJECT                                                                
072600 S01-PRT-MARKPOINT-TERMO-A6               SECTION.                        
072700                                                                          
072800     MOVE PRINT-IDDISTR    TO A6-IDDISTR                                  
072900                              A6-DISTR                                    
073000                              A6-DISTR-TXT                                
073100     MOVE PRINT-IDKUNDNR   TO A6-IDKUNDNR                                 
073200                              A6-KUNDNR                                   
073300                              A6-KUNDNR-TXT                               
073400     MOVE PRINT-IDORDNR    TO A6-IDORDNR                                  
073500                              A6-ORDNR                                    
073600                              A6-ORDNR-TXT                                
073700     MOVE PRINT-IDPRODNR   TO A6-IDPRODNR                                 
073800     MOVE PRINT-IDKOLLI(INDX)  TO A6-IDKOLLI                              
073900                              A6-KOLLI                                    
074000                              A6-KOLLI-TXT                                
074100     MOVE PRINT-KDFRAKT    TO A6-KDFRAKT                                  
074200*    MOVE WS-KDORDKL-TXT   TO A6-KDORDKL-TXT                              
074300*    MOVE WS-IDPRCVAR-TXT  TO A6-IDPRCVAR-TXT                             
074400                                                                          
074500     MOVE PRINT-BEGMT-RAD1 TO A6-ADRESS-1                                 
074600     MOVE PRINT-BEGMT-RAD2 TO A6-ADRESS-2                                 
074700     MOVE PRINT-ABGMT-GATA TO A6-ADRESS-3                                 
074800     MOVE PRINT-ABGMT-PADR TO A6-ADRESS-4                                 
074900                                                                          
075000*    MOVE PRINT-ROUTE-PLATFORM  TO A6-PLATFORM                            
075100*    MOVE PRINT-ROUTE-TOURNR    TO A6-TRANS                               
075200                                                                          
075300     CALL W006PRS1 USING PRT-SPOOL-A4S PRT-OPEN LISTVAL                   
075400                         ALT-PCB DUMMY-AREA DUMMY-AREA                    
075500                                                                          
075600     MOVE SPACE                     TO A6-RAD                             
075700     MOVE A6-STYR-01                TO A6-RAD                             
075800     CALL W006PRS1 USING PRT-SPOOL-A4S PRT-WRITE LISTVAL                  
075900                         ALT-PCB PRT-NYSIDA-RAD1 A6-RAD                   
076000                                                                          
076100     MOVE A6-STYR-01                TO A6-RAD                             
076200     CALL W006PRS1 USING PRT-SPOOL-A4S PRT-WRITE LISTVAL                  
076300                         ALT-PCB PRT-AFTER-1 A6-RAD                       
076400                                                                          
076500     MOVE A6-RUB-DISTRICT           TO A6-RAD                             
076600     CALL W006PRS1 USING PRT-SPOOL-A4S PRT-WRITE LISTVAL                  
076700                         ALT-PCB PRT-AFTER-1 A6-RAD                       
076800                                                                          
076900     MOVE A6-RUB-CUSTOMER      TO A6-RAD                                  
077000     CALL W006PRS1 USING PRT-SPOOL-A4S PRT-WRITE LISTVAL                  
077100                         ALT-PCB PRT-AFTER-1 A6-RAD                       
077200                                                                          
077300     MOVE A6-RUB-ORDERNUMBER        TO A6-RAD                             
077400     CALL W006PRS1 USING PRT-SPOOL-A4S PRT-WRITE LISTVAL                  
077500                         ALT-PCB PRT-AFTER-1 A6-RAD                       
077600                                                                          
077700     MOVE A6-RUB-FREIGHTCODE        TO A6-RAD                             
077800     CALL W006PRS1 USING PRT-SPOOL-A4S PRT-WRITE LISTVAL                  
077900                         ALT-PCB PRT-AFTER-1 A6-RAD                       
078000                                                                          
078100     MOVE A6-RUB-IDPRODNR           TO A6-RAD                             
078200     CALL W006PRS1 USING PRT-SPOOL-A4S PRT-WRITE LISTVAL                  
078300                         ALT-PCB PRT-AFTER-1 A6-RAD                       
078400                                                                          
078500     MOVE A6-RUB-ADDRESS            TO A6-RAD                             
078600     CALL W006PRS1 USING PRT-SPOOL-A4S PRT-WRITE LISTVAL                  
078700                         ALT-PCB PRT-AFTER-1 A6-RAD                       
078800                                                                          
078900     MOVE A6-RUB-CASE               TO A6-RAD                             
079000     CALL W006PRS1 USING PRT-SPOOL-A4S PRT-WRITE LISTVAL                  
079100                         ALT-PCB PRT-AFTER-1 A6-RAD                       
079200                                                                          
079300*    MOVE A6-RUB-PLATFORM               TO A6-RAD                         
079400*    CALL W006PRS1 USING PRT-SPOOL-A4S PRT-WRITE LISTVAL                  
079500*                        ALT-PCB PRT-AFTER-1 A6-RAD                       
079600                                                                          
079700     MOVE A6-DATA-IDDISTR           TO A6-RAD                             
079800     CALL W006PRS1 USING PRT-SPOOL-A4S PRT-WRITE LISTVAL                  
079900                         ALT-PCB PRT-AFTER-1 A6-RAD                       
080000                                                                          
080100     MOVE A6-DATA-IDKUNDNR          TO A6-RAD                             
080200     CALL W006PRS1 USING PRT-SPOOL-A4S PRT-WRITE LISTVAL                  
080300                         ALT-PCB PRT-AFTER-1 A6-RAD                       
080400                                                                          
080500     MOVE A6-DATA-IDORDNR           TO A6-RAD                             
080600     CALL W006PRS1 USING PRT-SPOOL-A4S PRT-WRITE LISTVAL                  
080700                         ALT-PCB PRT-AFTER-1 A6-RAD                       
080800                                                                          
080900     MOVE A6-DATA-IDKOLLI           TO A6-RAD                             
081000     CALL W006PRS1 USING PRT-SPOOL-A4S PRT-WRITE LISTVAL                  
081100                         ALT-PCB PRT-AFTER-1 A6-RAD                       
081200                                                                          
081300     MOVE A6-DATA-ADRESS1           TO A6-RAD                             
081400     CALL W006PRS1 USING PRT-SPOOL-A4S PRT-WRITE LISTVAL                  
081500                         ALT-PCB PRT-AFTER-1 A6-RAD                       
081600                                                                          
081700     MOVE A6-DATA-ADRESS2           TO A6-RAD                             
081800     CALL W006PRS1 USING PRT-SPOOL-A4S PRT-WRITE LISTVAL                  
081900                         ALT-PCB PRT-AFTER-1 A6-RAD                       
082000                                                                          
082100     MOVE A6-DATA-ADRESS3           TO A6-RAD                             
082200     CALL W006PRS1 USING PRT-SPOOL-A4S PRT-WRITE LISTVAL                  
082300                         ALT-PCB PRT-AFTER-1 A6-RAD                       
082400                                                                          
082500     MOVE A6-DATA-ADRESS4           TO A6-RAD                             
082600     CALL W006PRS1 USING PRT-SPOOL-A4S PRT-WRITE LISTVAL                  
082700                         ALT-PCB PRT-AFTER-1 A6-RAD                       
082800                                                                          
082900     MOVE A6-DATA-KDFRAKT           TO A6-RAD                             
083000     CALL W006PRS1 USING PRT-SPOOL-A4S PRT-WRITE LISTVAL                  
083100                         ALT-PCB PRT-AFTER-1 A6-RAD                       
083200                                                                          
083300     MOVE A6-DATA-IDPRODNR          TO A6-RAD                             
083400     CALL W006PRS1 USING PRT-SPOOL-A4S PRT-WRITE LISTVAL                  
083500                         ALT-PCB PRT-AFTER-1 A6-RAD                       
083600                                                                          
083700*    MOVE A6-DATA-PLATFORM          TO A6-RAD                             
083800*    CALL W006PRS1 USING PRT-SPOOL-A4S PRT-WRITE LISTVAL                  
083900*                        ALT-PCB PRT-AFTER-1 A6-RAD                       
084000*                                                                         
084100*    MOVE A6-TEXT-VOR               TO A6-RAD                             
084200*    CALL W006PRS1 USING PRT-SPOOL-A4S PRT-WRITE LISTVAL                  
084300*                        ALT-PCB PRT-AFTER-1 A6-RAD                       
084400*                                                                         
084500*    MOVE A6-TEXT-INT               TO A6-RAD                             
084600*    CALL W006PRS1 USING PRT-SPOOL-A4S PRT-WRITE LISTVAL                  
084700*                        ALT-PCB PRT-AFTER-1 A6-RAD                       
084800                                                                          
084900     MOVE A6-BARCODE-SDC21                 TO A6-RAD                      
085000     CALL W006PRS1 USING PRT-SPOOL-A4S PRT-WRITE LISTVAL                  
085100                         ALT-PCB PRT-AFTER-1 A6-RAD                       
085200                                                                          
085300     MOVE A6-BARCODE-TXT-SDC21             TO A6-RAD                      
085400     CALL W006PRS1 USING PRT-SPOOL-A4S PRT-WRITE LISTVAL                  
085500                         ALT-PCB PRT-AFTER-1 A6-RAD                       
085600                                                                          
085700     MOVE  A6-DATA-PRINT-TIME            TO A6-RAD                        
085800     CALL W006PRS1 USING PRT-SPOOL-A4S PRT-WRITE LISTVAL                  
085900                         ALT-PCB PRT-AFTER-1 A6-RAD                       
086000                                                                          
086100     MOVE  A6-DATA-PRINT-DATE            TO A6-RAD                        
086200     CALL W006PRS1 USING PRT-SPOOL-A4S PRT-WRITE LISTVAL                  
086300                         ALT-PCB PRT-AFTER-1 A6-RAD                       
086400                                                                          
086500     MOVE A6-STYR-91 TO A6-RAD                                            
086600     CALL W006PRS1 USING PRT-SPOOL-A4S PRT-WRITE LISTVAL                  
086700                         ALT-PCB PRT-AFTER-1 A6-RAD                       
086800                                                                          
086900     CALL W006PRS1 USING PRT-SPOOL-A4S PRT-CLOSE LISTVAL                  
087000                         ALT-PCB DUMMY-AREA DUMMY-AREA                    
087100     .                                                                    
087200                                                                          
087300     EJECT                                                                
087400 MFS-ERASE-FIELD-IN SECTION.                                              
087500                                                                          
087600*    --- ALLA INDATA-FÄLT                                                 
087700                                                                          
087800     MOVE MFS-ERASE-FIELD TO MOD-IDPRODNR-UT                              
087900                             MOD-IDPRODNR-IN                              
088000                             MOD-PRTVAL-ADRESSFL-UT                       
088100                             MOD-PRTVAL-ADRESSFL-IN                       
088200                             MOD-IDKOLLI-1                                
088300                             MOD-IDKOLLI-2                                
088400                             MOD-IDKOLLI-3                                
088500                             MOD-IDKOLLI-4                                
088600                             MOD-IDKOLLI-5                                
088700     .                                                                    
088800                                                                          
088900     EJECT                                                                
089000* --- IMS SECTIONS ---                                                    
089100                                                                          
089200                                                                          
089300 IMS-GET-MSG SECTION.                                                     
089400                                                                          
089500     MOVE '  QC' TO GOOD-STATUSCODES                                      
089600     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
089700     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
089800     PERFORM IMS-STATUSCHECK                                              
089900     .                                                                    
090000                                                                          
090100                                                                          
090200 IMS-INSERT-MSG SECTION.                                                  
090300                                                                          
090400     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
090500     MOVE SPACE TO GOOD-STATUSCODES                                       
090600     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
090700     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
090800     PERFORM IMS-STATUSCHECK                                              
090900     .                                                                    
091000                                                                          
091100     EJECT                                                                
091200 IMS-GU-GMTA01-WDB201 SECTION.                                            
091300                                                                          
091400     STRING 'WLGMTA01(IDGMT    =' W-IDGMT-X ')'                           
091500            DELIMITED BY SIZE INTO SSA1                                   
091600     MOVE '      ' TO GOOD-STATUSCODES                                    
091700     CALL CBLTDLI USING GU GMTA-PCB DLI-IO-GMTA01 SSA1                    
091800     MOVE GMTA-STATUS-CODE TO STATUS-WS                                   
091900     PERFORM IMS-STATUSCHECK                                              
092000                                                                          
092100                                                                          
092200     .                                                                    
092300 IMS-GU-KUNDORDER-SEK-INV SECTION.                                        
092400     STRING 'WDE411  (WDE4BSEQ>=' W-WDE4B-KEYSEQ-MIN-X                    
092500                    '&WDE4BSEQ<=' W-WDE4B-KEYSEQ-MAX-X ')'                
092600            DELIMITED BY SIZE INTO SSA1                                   
092700     MOVE 'WDE401   ' TO SSA2                                             
092800     MOVE '  GEGB' TO GOOD-STATUSCODES                                    
092900     CALL CBLTDLI USING GU   WDE4-PCB DLI-IO-WDE401 SSA1 SSA2             
093000     MOVE WDE4-STATUS-CODE TO STATUS-WS                                   
093100     PERFORM IMS-STATUSCHECK                                              
093200     .                                                                    
093300                                                                          
093400     EJECT                                                                
093500 IMS-STATUSCHECK SECTION.                                                 
093600                                                                          
093700     SET STATUS-IX TO 1                                                   
093800     SEARCH GOOD-STATUS                                                   
093900       AT END                                                             
094000         STRING ' INVALID STATUS CODE FROM IMS: ' STATUS-WS               
094100         DELIMITED BY SIZE INTO ERROR-TEXT                                
094200         CALL FELLOG                                                      
094300       WHEN GOOD-STATUS (STATUS-IX) = STATUS-WS                           
094400         CONTINUE                                                         
094500     END-SEARCH                                                           
094600     .                                                                    
