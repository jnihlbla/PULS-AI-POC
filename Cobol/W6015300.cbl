000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W6015300.                                                
000300 AUTHOR.         STEFAN ÅSGÅRDEN.                                         
000400 DATE-WRITTEN.   6 FEB 2002.                                              
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*    FUNKTION:                                                            
000800*        LOSSNING I CDC ELLER SVS                                         
000900*                                                                         
001000*        KOPIERAT PRINTNING FRÅN W6019800                                 
001100*                 SORTERING FRÅN W2035300                                 
001200*                                                                         
001300*                                                                         
001400*                                                                         
001500*        PROGRAMMET LÄSER      WDK6                                       
001600*                              WDM5                                       
001700*                              WDP7                                       
001800*                                                                         
001900*        PROGRAMMET UPPDATERAR WDM5 (ARBETSBAS)                           
002000*                              WDM9 (HISTORIKBAS)                         
002100*                              WDP7 (USERDATABASEN)                       
002200*                                                                         
002300*    INDATA.                                                              
002400*        TRANSAKTION: W6T153                                              
002500*        MID:         W6I15301                                            
002600*                                                                         
002700*    UTDATA.                                                              
002800*        MOD:         W6O15301                                            
002900                                                                          
003000     SKIP3                                                                
003100 ENVIRONMENT DIVISION.                                                    
003200     EJECT                                                                
003300 DATA DIVISION.                                                           
003400 WORKING-STORAGE SECTION.                                                 
003500                                                                          
003600*    -- CHECKED BY WY2000                                                 
003700 77  IDPGM                       PIC X(08)   VALUE 'W6015300'.            
003800                                                                          
003900*    --- ARBETSFÄLT FÖR FELMEDDELANDEN VID CALL ABEND/FELLOG              
004000 77  FELTEXT                     PIC X(80) VALUE SPACE.                   
004100                                                                          
004200 77  JA                          PIC X       VALUE 'J'.                   
004300 77  NEJ                         PIC X       VALUE 'N'.                   
004310 77  FOERSTA-RADEN               PIC X       VALUE 'N'.                   
004400                                                                          
005000 77  DAGENS-DATUM                PIC X(6)    VALUE SPACE.                 
005100                                                                          
005200*    --- INDEX FÖR BLÄDDRINGSRADER                                        
005300 77  INDX                        PIC S9(4)  VALUE +0    COMP SYNC.        
005310 77  RAD-MAX                     PIC S9(4)  VALUE +13   COMP SYNC.        
005320 77  RAD-IX                      PIC S9(3)  VALUE ZERO  COMP-3.           
005330 77  IDSID-RAKN                  PIC S9(3)  VALUE ZERO.                   
005500                                                                          
005510     EJECT                                                                
005520 01  FILLER                      PIC X(16)   VALUE 'TABELL'.              
005530     SKIP3                                                                
005600 01  TAB-MAX                     PIC S9(9) COMP VALUE 200.                
005700     EJECT                                                                
005800*    --- TABELL SOM SORTERAS AV WINTSOR                                   
005900 01  TABELL.                                                              
006000     03  TAB-POST  OCCURS 200.                                            
006100       04  TAB-RAD.                                                       
006200         05  TAB-IDARTNR         PIC 9(9)    BLANK WHEN ZERO.             
006300         05  TAB-KVANTAL         PIC 9(6)    BLANK WHEN ZERO.             
006400         05  TAB-ADART.                                                   
006500           07  TAB-ADLAGOMR      PIC 9(2)    BLANK WHEN ZERO.             
006600           07  TAB-ADGANG        PIC 9(2)    BLANK WHEN ZERO.             
006700           07  TAB-ADPLATS       PIC 9(5)    BLANK WHEN ZERO.             
006800         05  TAB-TETRPMED        PIC X(20).                               
006900       04  TAB-SORT.                                                      
007000           07  TAB-ADLAGOMR-SORT PIC 9(2)    BLANK WHEN ZERO.             
007100           07  TAB-ADGANG-SORT   PIC 9(2)    BLANK WHEN ZERO.             
007200           07  TAB-ADPLATS-SORT  PIC 9(5)    BLANK WHEN ZERO.             
007300 77  INDATA-SW                   PIC X       VALUE 'J'.                   
007400     88  INDATA-OK                           VALUE 'J'.                   
007500     88  INDATA-FEL                          VALUE 'N'.                   
007600                                                                          
007700 77  NYCKLAR-SW                  PIC X       VALUE 'J'.                   
007800     88  NYCKLAR-OK                          VALUE 'J'.                   
007900     88  NYCKLAR-FEL                         VALUE 'N'.                   
008000                                                                          
008100 77  UPD-RAD-SW                  PIC X       VALUE 'N'.                   
008200     88  UPD-RAD-JA                          VALUE 'J'.                   
008300     88  UPD-RAD-NEJ                         VALUE 'N'.                   
008400                                                                          
008500 77  LOSSNING-KLAR-SW            PIC X       VALUE 'N'.                   
008600     88  LOSSNING-KLAR-JA                    VALUE 'J'.                   
008700     88  LOSSNING-KLAR-NEJ                   VALUE 'N'.                   
008800                                                                          
008900 77  PRINT-RAD-SW                PIC X       VALUE 'N'.                   
009000     88  PRINT-RAD-JA                        VALUE 'J'.                   
009100     88  PRINT-RAD-NEJ                       VALUE 'N'.                   
009200                                                                          
009300 77  PRINTER-VAL-SW              PIC X       VALUE 'N'.                   
009400     88  PRINTER-VAL-JA                      VALUE 'J'.                   
009500     88  PRINTER-VAL-NEJ                     VALUE 'N'.                   
009600                                                                          
009700 77  PRINTER-OK                  PIC X       VALUE 'J'.                   
009710                                                                          
009720 77  M521-FINNS-SW               PIC X       VALUE 'N'.                   
009730     88  M521-FINNS-JA                       VALUE 'J'.                   
009731     88  M521-FINNS-NEJ                      VALUE 'N'.                   
009800                                                                          
009900                                                                          
010000 77  W-IDTRANS                   PIC X(4)    VALUE SPACE.                 
010100     88  EGEN-MID                            VALUE '6153'.                
010200     88  GODK-MID                            VALUE '6153'.                
010300     88  HELP-MID                            VALUE '0551'.                
010400                                                                          
010500*    --- GENERELLA ARBETSAREAOR.                                          
010600                                                                          
010610     EJECT                                                                
010620 01  FILLER                      PIC X(16)   VALUE 'WS'.                  
010630     SKIP3                                                                
010700 01  WS.                                                                  
010800*********************************************************                 
010900*    WS-MSGI-AREA-6153                                                    
011000*           ANVÄNDS FÖR ATT SPARA PÅ NYCKELDATABASEN WDP7                 
011100*           (I MSGI-SPAR-AREA)                                            
011200*********************************************************                 
011300  05 WS-MSGI-AREA-6153.                                                   
011400    10 WS-MSGI-IDTRANS-6153      PIC X(4)    VALUE '6153'.                
011500    10 WS-MSGI-SSA-KEY-ENTER.                                             
011600      15  WS-ENTER-IDTRPTNR      PIC S9(5)   COMP-3 VALUE ZERO.           
011700      15  WS-ENTER-DADATTID      PIC 9(14)   VALUE ZERO.                  
011800                                                                          
011900    10 WS-MSGI-SSA-KEY-NEXT.                                              
012000      15  WS-NEXT-IDTRPTNR       PIC S9(5)   COMP-3 VALUE ZERO.           
012100      15  WS-NEXT-DADATTID       PIC 9(14)   VALUE ZERO.                  
012200                                                                          
012300    10 WS-MSGI-RAD-KEY           OCCURS 13.                               
012400      15  WS-RAD-IDTRPTNR        PIC S9(5)   COMP-3 VALUE ZERO.           
012500      15  WS-RAD-DADATTID        PIC 9(14)   VALUE ZERO.                  
012600                                                                          
012700    10 FILLER                    PIC X(741)  VALUE SPACE.                 
012800                                                                          
012810  05 WS-ADTRDEST                 PIC X(3)    VALUE SPACE.                 
012820  05 WS-KDTRPSTA                 PIC X       VALUE SPACE.                 
012830  05 WS-DATUM-14                 PIC 9(14)   VALUE ZERO.                  
012840  05 WS-IDTRPTNR                 PIC 9(5)    VALUE ZERO.                  
012841  05 FILLER                      PIC X(16)   VALUE 'WS-SECTION'.          
012842  05 WS-SECTION                  PIC X(26)   VALUE SPACE.                 
013010                                                                          
013020       EJECT                                                              
013100     EJECT                                                                
013200*    --- SUBPROGRAM OCH PARAMETERAREOR                                    
013300 01  GENERELLA-SUBPROGRAM.                                                
013400     03  W005INIT                PIC X(8)    VALUE 'W005INIT'.            
013500     03  WMEDKONV                PIC X(8)    VALUE 'WMEDKONV'.            
013600     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
013700     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
013800     03  WDATKONV                PIC X(8)    VALUE 'WDATKONV'.            
013900     03  W006KOM                 PIC X(8)    VALUE 'W006KOM '.            
014000     03  W009KSIF                PIC X(8)    VALUE 'W009KSIF'.            
014100     03  W006PRT                 PIC X(8)    VALUE 'W006PRT '.            
014200     03  W006PRS1                PIC X(8)    VALUE 'W006PRS1'.            
014300     03  WINTSOR                 PIC X(8)    VALUE 'WINTSOR'.             
014400     EJECT                                                                
014500*01 -COPY W006PRT                                                         
014600     EJECT                                                                
014700*    ---AREA FÖR SUBPGM W006PRS1                                          
014800 01 FILLER                      PIC X(16)   VALUE 'W006PRS1'.             
014900                                                                          
015000*01 -COPY W006PRAR                                                        
015100                                                                          
015200 01 WS-PRINTER-PARM.                                                      
015300     03 WS-LIST-PRINTER          PIC X(8).                                
015400     03 WS-RAD.                                                           
015500       05 WS-FILLER              PIC X(1).                                
015600       05 WS-LISTRAD             PIC X(120).                              
015700     03 WS-DUMMY                 PIC X(1).                                
015800     EJECT                                                                
015900                                                                          
016000*    --- PARAMETRAR TILL SUBPROGRAM WINTSOR                               
016100 01  TABENTRY-PARM.                                                       
016200     03  STEGLANGD               PIC S9(9) COMP  VALUE 53.                
016300     03  ANTAL                   PIC S9(9) COMP.                          
016400     03  NYCKELLANGD             PIC S9(9) COMP  VALUE 9.                 
016500                                                                          
016600     EJECT                                                                
016700*    --- PARAMETRAR TILL SUBPROGRAM W005INIT                              
016800*01 -COPY WMSGINIT                                                        
016900     EJECT                                                                
017000*    --- PARAMETRAR TILL SUBPROGRAM WMEDKONV                              
017100*01 -COPY WMEDAREA                                                        
017200     SKIP3                                                                
017300 01  MESSAGE-CODES.                                                       
017400     03  ERR-CORR-HILITE-FLDS    PIC X(3)    VALUE '001'.                 
017410     03  CONFLICT                PIC X(3)    VALUE '002'.                 
017500     03  INF-PRESS-PF11          PIC X(3)    VALUE '003'.                 
017600     03  INF-URVAL-SAKNAS        PIC X(3)    VALUE '005'.                 
017700     03  INF-FIRST-PAGE          PIC X(3)    VALUE '006'.                 
017800     03  ERR-PF11-AND-NO-DATA    PIC X(3)    VALUE '011'.                 
017900     03  TOM-RAD                 PIC X(3)    VALUE '080'.                 
018000     03  INF-UPDATE-DONE         PIC X(3)    VALUE '101'.                 
018100     03  INF-MORE-INFO-EXISTS    PIC X(3)    VALUE '105'.                 
018200     03  INF-SISTA-SIDAN         PIC X(3)    VALUE '115'.                 
018300     03  INF-PRINT-BEGAERD       PIC X(3)    VALUE '118'.                 
018400     03  INF-PRINT-START         PIC X(3)    VALUE '202'.                 
018500     03  ERR-WRONG-KEY           PIC X(3)    VALUE '401'.                 
018600     03  AREA-MISSING            PIC X(3)    VALUE '705'.                 
018700     03  ERR-FEL-PRINTER         PIC X(3)    VALUE '772'.                 
018710     03  UPDATING-NOT-ALLOWED    PIC X(3)    VALUE '777'.                 
018720     SKIP3                                                                
018730 01  FELTEXTER.                                                           
018740     03  MED-1.                                                           
018750       05 FILLER                 PIC X(55)                                
018760         VALUE 'ANGE TRPID FÖR ATT KUNNA LOSSA'.                          
018770     03  MED-2.                                                           
018780       05 FILLER                 PIC X(55)                                
018790         VALUE 'LPL ÄR OBLIGATORISKT VID LOSSNING'.                       
018800     EJECT                                                                
018900 01  FILLER                      PIC X(16)   VALUE 'DAT-AREA'.            
019000     SKIP3                                                                
019100 01  DAT-IO-AREA.                                                         
019200*    03  -COPY WDATAREA                                                   
019300     EJECT                                                                
019400                                                                          
019500*    --- AREOR FÖR MFS OCH SKÄRMHANTERING                                 
019600*                                                                         
019700 01  FILLER                      PIC X(16)   VALUE 'MID-AREA'.            
019800     SKIP3                                                                
019900*01  MID -COPY W6I15301                                                   
020000     EJECT                                                                
020100 01  FILLER                      PIC X(16)  VALUE 'MSG/MOD-AREA'.         
020200     SKIP3                                                                
020300*01  -COPY WMSGAREA                                                       
020400     EJECT                                                                
020500     03  MOD REDEFINES MSG-AREA.                                          
020600*      05  -COPY W6O15301                                                 
020700     EJECT                                                                
020800*    --- AREOR FÖR W006KOM SUBMODUL                                       
020900*                                                                         
021000 01  FILLER                      PIC X(16)   VALUE 'MSG-KOM-AREA'.        
021100*01  -COPY WMSGKOM                                                        
021200     EJECT                                                                
021300 01  FILLER                      PIC X(16)   VALUE 'MFS-AREA'.            
021400     SKIP3                                                                
021500*01  -COPY WMFSAREA                                                       
021600     EJECT                                                                
021700*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
021800*                                                                         
021900     EJECT                                                                
022000 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
022100     SKIP3                                                                
022200 01  NYCKLAR-TILL-DLI.                                                    
022300     03  W-IDARTNR-X.                                                     
022400         05  W-IDARTNR           PIC S9(9)              COMP-3.           
022500     03  W-DADATTID-X.                                                    
022600         05  W-DADATTID          PIC 9(14)   VALUE ZERO.                  
022601     03  W-DADATTID-9KOMPL-X.                                             
022602         05  W-DADATTID-9KOMPL   PIC 9(14)   VALUE ZERO.                  
022610     03  W-ADTRDEST              PIC X(3)    VALUE SPACE.                 
022620     03  W-IDTRPTNR-X.                                                    
022630         05  W-IDTRPTNR          PIC S9(5)              COMP-3.           
022700                                                                          
022800     EJECT                                                                
022900*    --- STATUS-KOD FRÅN IMS                                              
023000 01  STATUS-WS                   PIC XX.                                  
023100     88  SEGMENT-FINNS                       VALUE '  '.                  
023200     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
023300     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
023400     SKIP2                                                                
023500 01  GODK-STATUSKODER.                                                    
023600     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
023710     EJECT                                                                
023720 01  FILLER                      PIC X(16)   VALUE 'SSA'.                 
023730     SKIP3                                                                
023800 01  SSA1                        PIC X(160).                              
023900 01  SSA2                        PIC X(128).                              
024000 01  SSA3                        PIC X(128).                              
024100     EJECT                                                                
024200***********************************************                           
024300*  PRINTRADER                                 *                           
024400***********************************************                           
024500                                                                          
024600                                                                          
024700 01  L1-RAD1.                                                             
024800     03 FILLER          PIC X(23) VALUE 'VOLVO CUSTOMER SERVICE'.         
024900     03 FILLER          PIC X(18) VALUE SPACE.                            
025000     03 FILLER          PIC X(10) VALUE 'W60153-001'.                     
025100     03 FILLER          PIC X(10) VALUE SPACE.                            
025200     03 FILLER          PIC X(14) VALUE 'LASTNINGSLISTA'.                 
025300     03 FILLER          PIC X(15) VALUE SPACE.                            
025400     03 FILLER          PIC X(5)  VALUE 'DATE '.                          
025500     03 L1-RAD1-DATUM   PIC X(6).                                         
025600     03 FILLER          PIC X(8)  VALUE '   SID  '.                       
025700     03 L1-RAD1-IDSID        PIC ZZ9.                                     
025800                                                                          
025900 01  L1-RAD2.                                                             
026000     03 FILLER               PIC X(121) VALUE SPACE.                      
026100                                                                          
026200 01  L1-RAD3.                                                             
026300     03 FILLER               PIC X(121) VALUE SPACE.                      
026400                                                                          
026500 01  L1-RAD4.                                                             
026600     03 FILLER               PIC X(121) VALUE SPACE.                      
026700                                                                          
026800 01  L1-RAD5.                                                             
026900     03 FILLER               PIC X(121) VALUE SPACE.                      
027000                                                                          
027100 01  L1-RAD6.                                                             
027200     03 FILLER               PIC X(121) VALUE SPACE.                      
027300                                                                          
027400 01  L1-RAD7.                                                             
027500     03 FILLER               PIC X(4)   VALUE '    '.                     
027600     03 FILLER               PIC X(5)   VALUE 'ARTNR'.                    
027700     03 FILLER               PIC X(3)   VALUE SPACE.                      
027800     03 FILLER               PIC X(5)   VALUE 'ANTAL'.                    
027900     03 FILLER               PIC X(2)   VALUE SPACE.                      
028000     03 FILLER               PIC X(11)  VALUE 'PLATS'.                    
028100     03 FILLER               PIC X(2)   VALUE SPACE.                      
028200     03 FILLER               PIC X(3)   VALUE 'FT'.                       
028300     03 FILLER               PIC X(2)   VALUE SPACE.                      
028400     03 FILLER               PIC X(8)   VALUE 'NOTERING'.                 
028500                                                                          
028600 01  L1-RAD8.                                                             
028700     03 L1-RAD8-IDARTNR      PIC Z(8)9.                                   
028800     03 FILLER               PIC X(2)   VALUE SPACE.                      
028900     03 L1-RAD8-KVANTAL      PIC Z(5)9.                                   
029000     03 FILLER               PIC X(2)   VALUE SPACE.                      
029100     03 L1-RAD8-ADLAGOMR     PIC Z9.                                      
029200     03 FILLER               PIC X      VALUE SPACE.                      
029300     03 L1-RAD8-ADGANG       PIC Z9.                                      
029400     03 FILLER               PIC X      VALUE SPACE.                      
029500     03 L1-RAD8-ADPLATS      PIC Z(4)9.                                   
029600     03 FILLER               PIC X(2)   VALUE SPACE.                      
029700     03 L1-RAD8-BEFT         PIC Z9.                                      
029800     03 FILLER               PIC X(2)   VALUE SPACE.                      
029900     03 L1-RAD8-TETRPMED     PIC X(20)  VALUE SPACE.                      
030000                                                                          
030100                                                                          
030200                                                                          
030300 01  L2-RAD1.                                                             
030400     03 FILLER          PIC X(23) VALUE 'VOLVO CUSTOMER SERVICE'.         
030500     03 FILLER          PIC X(18) VALUE SPACE.                            
030600     03 FILLER          PIC X(10) VALUE 'W60153-001'.                     
030700     03 FILLER          PIC X(10) VALUE SPACE.                            
030800     03 FILLER          PIC X(14) VALUE 'TRANSPORTLISTA'.                 
030900     03 FILLER          PIC X(15) VALUE SPACE.                            
031000     03 FILLER          PIC X(5)  VALUE 'DATE '.                          
031100     03 L2-RAD1-DATUM   PIC X(6).                                         
031200     03 FILLER          PIC X(8)  VALUE '   SID  '.                       
031300     03 L2-RAD1-IDSID        PIC ZZ9.                                     
031400                                                                          
031500 01  L2-RAD2.                                                             
031600     03 FILLER               PIC X(121) VALUE SPACE.                      
031700                                                                          
031800 01  L2-RAD3.                                                             
031900     03 FILLER               PIC X(121) VALUE SPACE.                      
032000                                                                          
032100 01  L2-RAD4.                                                             
032400     03 FILLER               PIC X(12) VALUE 'TRANSPORTID'.               
032500     03 L2-RAD4-TRPTNR       PIC Z(5)9.                                   
032600     03 FILLER               PIC X(103) VALUE SPACE.                      
032700                                                                          
032800 01  L2-RAD5.                                                             
032900     03 FILLER               PIC X(121) VALUE SPACE.                      
033000                                                                          
033100 01  L2-RAD6.                                                             
033200     03 FILLER               PIC X(121) VALUE SPACE.                      
033300                                                                          
033400 01  L2-RAD7.                                                             
033500     03 FILLER               PIC X(4)   VALUE '    '.                     
033600     03 FILLER               PIC X(5)   VALUE 'ARTNR'.                    
033700     03 FILLER               PIC X(3)   VALUE SPACE.                      
033800     03 FILLER               PIC X(5)   VALUE 'ANTAL'.                    
033900     03 FILLER               PIC X(2)   VALUE SPACE.                      
034000     03 FILLER               PIC X(11)  VALUE 'PLATS'.                    
034100     03 FILLER               PIC X(2)   VALUE SPACE.                      
034200     03 FILLER               PIC X(8)   VALUE 'NOTERING'.                 
034300                                                                          
034400 01  L2-RAD8.                                                             
034500     03 L2-RAD8-IDARTNR      PIC Z(8)9.                                   
034600     03 FILLER               PIC X(2)   VALUE SPACE.                      
034700     03 L2-RAD8-KVANTAL      PIC Z(5)9.                                   
034800     03 FILLER               PIC X(2)   VALUE SPACE.                      
034900     03 L2-RAD8-ADLAGOMR     PIC Z9.                                      
035000     03 FILLER               PIC X      VALUE SPACE.                      
035100     03 L2-RAD8-ADGANG       PIC Z9.                                      
035200     03 FILLER               PIC X      VALUE SPACE.                      
035300     03 L2-RAD8-ADPLATS      PIC Z(4)9.                                   
035400     03 FILLER               PIC X(2)   VALUE SPACE.                      
035500     03 L2-RAD8-TETRPMED     PIC X(20)  VALUE SPACE.                      
035600                                                                          
035700*    --- IMS FUNKTIONSKODER                                               
035800*01  -COPY W0003                                                          
035900     EJECT                                                                
036000*    ---  DLI INPUT-OUTPUT AREA                                           
036100     EJECT                                                                
036200                                                                          
036300 01  FILLER         PIC X(24) VALUE 'DLI-IO-WDK601'.                      
036400 01  DLI-IO-WDK601.                                                       
036500*    03  -COPY WDK601                                                     
036600     EJECT                                                                
036700                                                                          
036800 01  FILLER         PIC X(24) VALUE 'DLI-IO-WDK611'.                      
036900 01  DLI-IO-WDK611.                                                       
037000*    03  -COPY WDK611                                                     
037100     EJECT                                                                
037200                                                                          
037300 01  FILLER         PIC X(24) VALUE 'DLI-IO-WDM501'.                      
037400 01  DLI-IO-WDM501.                                                       
037500*    03  -COPY WDM501                                                     
037600     EJECT                                                                
037700                                                                          
037800 01  FILLER         PIC X(24) VALUE 'DLI-IO-WDM511'.                      
037900 01  DLI-IO-WDM511.                                                       
038000*    03  -COPY WDM511                                                     
038100     EJECT                                                                
038200                                                                          
038300 01  FILLER         PIC X(24) VALUE 'DLI-IO-WDM521'.                      
038400 01  DLI-IO-WDM521.                                                       
038500*    03  -COPY WDM521                                                     
038600     EJECT                                                                
038700                                                                          
038800 01  FILLER         PIC X(24) VALUE 'DLI-IO-WDM901'.                      
038900 01  DLI-IO-WDM901.                                                       
039000*    03  -COPY WDM901                                                     
039100     EJECT                                                                
039200                                                                          
039300 01  FILLER         PIC X(24) VALUE 'DLI-IO-WDM911'.                      
039400 01  DLI-IO-WDM911.                                                       
039500*    03  -COPY WDM911                                                     
039600     EJECT                                                                
039700                                                                          
039800 LINKAGE SECTION.                                                         
039900                                                                          
040000*01  -COPY W0009   -PRE MSG-                                              
040100     EJECT                                                                
040200*01  -COPY W0009   -PRE ALT-                                              
040300     EJECT                                                                
040400*01  -COPY W0008   -PRE WDP7-                                             
040500     05  FILLER                  PIC X.                                   
040600     EJECT                                                                
040700*01  -COPY W0008  -PRE WDM5-                                              
040800     05  FILLER                  PIC X.                                   
040900     EJECT                                                                
041000*01  -COPY W0008  -PRE WDM5-2-                                            
041100     05  FILLER                  PIC X.                                   
041200     EJECT                                                                
041300*01  -COPY W0008  -PRE WDK6-                                              
041400     05  FILLER                  PIC X.                                   
041500     EJECT                                                                
041600*01  -COPY W0008  -PRE WDM9-                                              
041700     05  FILLER                  PIC X.                                   
041800     EJECT                                                                
041900 PROCEDURE DIVISION  USING MSG-PCB ALT-PCB WDP7-PCB                       
042020                           WDM5-PCB WDM5-2-PCB WDM9-PCB WDK6-PCB.         
042100 MAIN SECTION.                                                            
042200     ENTRY 'DLITCBL' USING MSG-PCB ALT-PCB WDP7-PCB                       
042210                           WDM5-PCB WDM5-2-PCB WDM9-PCB WDK6-PCB.         
042400                                                                          
042500     PERFORM IMS-GET-MSG                                                  
042600                                                                          
042700     IF SEGMENT-FINNS                                                     
042800        PERFORM A-INIT                                                    
042900        PERFORM B-KOLLA-NYCKLAR                                           
043000                                                                          
043100        IF NYCKLAR-OK                                                     
043200                                                                          
043300           IF MFS-UPDATE                                                  
043400              PERFORM G-KOLLA-INPUT                                       
043500                                                                          
043600              IF INDATA-OK                                                
043700                 PERFORM H-UPPDATERA                                      
043800              END-IF                                                      
043900                                                                          
044000           ELSE                                                           
044100              IF MFS-FIRST                                                
044200                 PERFORM C-FOERSTA-SIDA                                   
044300              ELSE                                                        
044400                 IF MFS-NEXT                                              
044500                    PERFORM D-NAESTA-SIDA                                 
044600                 ELSE                                                     
044700                    PERFORM E-SAMMA-SIDA                                  
044800                 END-IF                                                   
044900              END-IF                                                      
045000           END-IF                                                         
045200           PERFORM F-LAES-VISA-INFO                                       
045300                                                                          
045400* ---    UPPDATERA MSGI-SPAR-AREA                                         
045500           MOVE '002'           TO MSGI-KDCALL                            
045600           MOVE MSG-LTERM-NAME  TO MSGI-IDLTERM-USER                      
045700           MOVE MSG-SIGNON-USERID                                         
045800                                TO MSGI-IDUSER                            
045900           MOVE '6153'          TO MSGI-IDTRANS                           
046000           MOVE WS-MSGI-AREA-6153                                         
046100                                TO MSGI-SPAR-AREA                         
046200           CALL W005INIT USING MSGI-WMSGINIT WDP7-PCB                     
046300                                                                          
046400        END-IF                                                            
046500*                                                                         
046600        COMPUTE MSG-KVLL = LENGTH OF MOD-W6O15301 + 4                     
046700        PERFORM IMS-INSERT-MSG                                            
046800     END-IF                                                               
046900                                                                          
047000     MOVE ZERO TO RETURN-CODE                                             
047100     GOBACK                                                               
047200     .                                                                    
047300     EJECT                                                                
047400 A-INIT SECTION.                                                          
047410     MOVE 'A-INIT             ' TO WS-SECTION                             
047500                                                                          
047600     IF MSG-DUBBLA-TRANSKODER                                             
047700       MOVE MSG-INDATA-MINUS-2-TRANSKODER TO MID-W6I15301                 
047800       MOVE MSG-IDTRANS-2  TO MFS-IDTRANS                                 
047900       MOVE MSG-KDMFSFOR-2 TO MFS-KDMFSFOR                                
048000     ELSE                                                                 
048100       MOVE MSG-INDATA-MINUS-1-TRANSKOD  TO MID-W6I15301                  
048200       MOVE MSG-IDTRANS-1  TO MFS-IDTRANS                                 
048300       MOVE MSG-KDMFSFOR-1 TO MFS-KDMFSFOR                                
048400     END-IF                                                               
048500                                                                          
048600     MOVE MSG-KDTRTYP TO MFS-KDTRTYP                                      
048700     MOVE MSG-IDPFK   TO MFS-IDPFK                                        
048800     MOVE MFS-IDTRANS TO W-IDTRANS                                        
048900                                                                          
049000     MOVE LOW-VALUE TO MSG-AREA                                           
049100     MOVE 'W6O15301' TO MFS-IDMOD                                         
049200     MOVE '6153' TO MOD-IDTRANS                                           
049300     MOVE MFS-RENSA-FAELT TO MOD-TEMFSFEL MOD-TEMFSINF                    
049400                                                                          
049500     IF EGEN-MID OR HELP-MID                                              
049600        CONTINUE                                                          
049700     ELSE                                                                 
049800        MOVE SPACE TO MFS-KDTRTYP                                         
049900        MOVE '7'   TO MFS-IDPFK                                           
050000     END-IF                                                               
050100     ACCEPT DAGENS-DATUM FROM DATE                                        
050110     MOVE 'SE '           TO MED-IDSKYLT                                  
050200     .                                                                    
050300     EJECT                                                                
050400 B-KOLLA-NYCKLAR SECTION.                                                 
050410     MOVE 'B-KOLLA-NYCKLAR    ' TO WS-SECTION                             
050500                                                                          
050600     MOVE SPACE              TO WS-MSGI-SSA-KEY-ENTER                     
050700                                WS-MSGI-SSA-KEY-NEXT                      
050800                                                                          
050900******   UPPDATERING AV MSGI-BLÄDDRINGSNYCKLAR SKER                       
051000******   I SLUTET AV PROGRAMMET                                           
051100     MOVE ALL '+'            TO MSGI-WMSGINIT                             
051200     MOVE '001'              TO MSGI-KDCALL                               
051300     MOVE MSG-LTERM-NAME     TO MSGI-IDLTERM-USER                         
051400     MOVE MSG-SIGNON-USERID  TO MSGI-IDUSER                               
051500     MOVE '6153'             TO MSGI-IDTRANS                              
051600     CALL W005INIT USING MSGI-WMSGINIT WDP7-PCB                           
051700                                                                          
051800     IF EGEN-MID                                                          
051900     AND MSGI-SPAR-AREA(1:4) = '6153'                                     
052000       MOVE MSGI-SPAR-AREA   TO WS-MSGI-AREA-6153                         
052001     ELSE                                                                 
052010       MOVE ZERO             TO MID-IDTRPTNR-IN                           
052100     END-IF                                                               
052200                                                                          
052300                                                                          
052400     MOVE JA TO NYCKLAR-SW                                                
052500     MOVE MFS-RENSA-FAELT    TO MOD-ADTRDEST-IN                           
052600                                                                          
052700     IF MID-ADTRDEST-IN = ALL '+'                                         
052800       MOVE MID-ADTRDEST-UT  TO WS-ADTRDEST                               
052810       INSPECT WS-ADTRDEST  REPLACING LEADING '+' BY SPACE                
052900     ELSE                                                                 
053000       MOVE MID-ADTRDEST-IN  TO WS-ADTRDEST                               
053100       MOVE '7'              TO MFS-IDPFK                                 
053200       MOVE SPACE            TO MFS-KDTRTYP                               
053201                                WS-MSGI-SSA-KEY-ENTER                     
053202                                WS-MSGI-SSA-KEY-NEXT                      
053300     END-IF                                                               
053400                                                                          
053500     IF WS-ADTRDEST = 'SVS'                                               
053600     OR WS-ADTRDEST = 'CDC'                                               
053700       MOVE WS-ADTRDEST      TO MOD-ADTRDEST-UT                           
053800     ELSE                                                                 
053900       MOVE NEJ TO NYCKLAR-SW                                             
054000     END-IF                                                               
054100                                                                          
054200     MOVE MFS-RENSA-FAELT    TO MOD-IDTRPTNR-IN                           
054300                                                                          
054400     IF MID-IDTRPTNR-IN = ALL '+'                                         
054420       INSPECT MID-IDTRPTNR-UT REPLACING LEADING '+' BY SPACE             
054510       MOVE MID-IDTRPTNR-UT  TO WS-IDTRPTNR                               
054600     ELSE                                                                 
054700       MOVE MID-IDTRPTNR-IN  TO WS-IDTRPTNR                               
054800       MOVE '7'              TO MFS-IDPFK                                 
054900       MOVE SPACE            TO MFS-KDTRTYP                               
054910                                WS-MSGI-SSA-KEY-ENTER                     
054920                                WS-MSGI-SSA-KEY-NEXT                      
055000     END-IF                                                               
055010     INSPECT WS-IDTRPTNR     REPLACING LEADING SPACE BY ZERO              
055020     MOVE WS-IDTRPTNR        TO MOD-IDTRPTNR-UT                           
055021     INSPECT MOD-IDTRPTNR-UT REPLACING LEADING ZERO BY SPACE              
055100                                                                          
055200     IF NYCKLAR-FEL                                                       
055300        MOVE ERR-WRONG-KEY TO MED-IDMFSFEL                                
055400        CALL WMEDKONV USING MED-WMEDAREA                                  
055500                                                                          
055600        MOVE MED-TEMFSFEL TO MOD-TEMFSFEL                                 
055700        PERFORM MFS-RENSA-FAELT-IN                                        
055800        PERFORM MFS-RENSA-FAELT-UT                                        
055900     END-IF                                                               
056000     .                                                                    
056100     EJECT                                                                
056200 C-FOERSTA-SIDA SECTION.                                                  
056210     MOVE 'C-FOERSTA-SIDA     ' TO WS-SECTION                             
056300                                                                          
056400     MOVE 'SE '           TO MED-IDSKYLT                                  
056500     MOVE INF-FIRST-PAGE TO MED-IDMFSFEL                                  
056600     CALL WMEDKONV USING MED-WMEDAREA                                     
056700     MOVE MED-TEMFSFEL   TO MOD-TEMFSFEL                                  
056800                                                                          
056900*    --- BLANKA/NOLLA UT BLÄDDRINGSNYCKEL                                 
057000     MOVE SPACE              TO WS-MSGI-SSA-KEY-ENTER                     
057100                                WS-MSGI-SSA-KEY-NEXT                      
057200     PERFORM MFS-RENSA-FAELT-IN                                           
057300     .                                                                    
057400     EJECT                                                                
057500 D-NAESTA-SIDA SECTION.                                                   
057502     MOVE 'D-NAESTA-SIDA      ' TO WS-SECTION                             
057600                                                                          
058200     MOVE SPACE              TO WS-MSGI-SSA-KEY-ENTER                     
058400     .                                                                    
058500     EJECT                                                                
058600 E-SAMMA-SIDA SECTION.                                                    
058610     MOVE 'E-SAMMA-SIDA       ' TO WS-SECTION                             
058700                                                                          
058800     IF MID-INPUT = ALL '+'                                               
058900       PERFORM MFS-RENSA-FAELT-IN                                         
059000     ELSE                                                                 
059100       MOVE INF-PRESS-PF11 TO MED-IDMFSINF                                
059200       CALL WMEDKONV USING MED-WMEDAREA                                   
059300       MOVE MED-TEMFSINF TO MOD-TEMFSINF                                  
059400       PERFORM EA-MID-INDATA-TILL-MOD                                     
059500     END-IF                                                               
059600     .                                                                    
059700     EJECT                                                                
059800 EA-MID-INDATA-TILL-MOD SECTION.                                          
059810     MOVE 'EA-MID-INDATA-TILL-MOD ' TO WS-SECTION                         
059900                                                                          
060000     MOVE +1 TO INDX                                                      
060100                                                                          
060200     PERFORM UNTIL INDX > RAD-MAX                                         
060300        IF MID-CMD (INDX)         = ALL '+'                               
060400           MOVE MFS-RENSA-FAELT         TO MOD-CMD (INDX)                 
060500        ELSE                                                              
060600           MOVE MID-CMD (INDX)                                            
060700                             TO MOD-CMD (INDX)                            
061000        END-IF                                                            
061100        ADD +1     TO INDX                                                
061200                                                                          
061300     END-PERFORM                                                          
061400*                                                                         
061500     IF MID-LOSSNING-KLAR     = ALL '+'                                   
061600        MOVE MFS-RENSA-FAELT         TO MOD-LOSSNING-KLAR                 
061700     ELSE                                                                 
061800        MOVE MID-LOSSNING-KLAR       TO MOD-LOSSNING-KLAR                 
062000     END-IF                                                               
062100                                                                          
062200     IF MID-ADINLOMR-LPL = ALL '+'                                        
062300        MOVE MFS-RENSA-FAELT         TO MOD-ADINLOMR-LPL                  
062400     ELSE                                                                 
062500        MOVE MID-ADINLOMR-LPL        TO MOD-ADINLOMR-LPL                  
062700     END-IF                                                               
062800                                                                          
062900     IF MID-PRINTER          = ALL '+'                                    
063000        MOVE MFS-RENSA-FAELT         TO MOD-PRINTER                       
063100     ELSE                                                                 
063200        MOVE MID-PRINTER             TO MOD-PRINTER                       
063400     END-IF                                                               
063500     .                                                                    
063600     EJECT                                                                
063700 F-LAES-VISA-INFO SECTION.                                                
063710     MOVE 'F-LAES-VISA-INFO' TO WS-SECTION                                
063800                                                                          
063900     MOVE WS-ADTRDEST        TO W-ADTRDEST                                
063910     PERFORM IMS-GU-M501                                                  
064000                                                                          
064100     IF WS-IDTRPTNR = ZERO                                                
064200                                                                          
064230       IF WS-MSGI-SSA-KEY-ENTER NOT = SPACE                               
064232         MOVE WS-ENTER-IDTRPTNR                                           
064233                             TO W-IDTRPTNR                                
064234         MOVE WS-ENTER-DADATTID                                           
064235                             TO W-DADATTID                                
064282         PERFORM IMS-GNP-M511-KVAL                                        
064283         PERFORM IMS-GNP-M521-KVAL                                        
064290                                                                          
064291       ELSE                                                               
064292         IF WS-MSGI-SSA-KEY-NEXT NOT = SPACE                              
064294           MOVE WS-NEXT-IDTRPTNR                                          
064295                               TO W-IDTRPTNR                              
064296           MOVE WS-NEXT-DADATTID                                          
064297                               TO W-DADATTID                              
064300           PERFORM IMS-GNP-M511-KVAL                                      
064301           PERFORM IMS-GNP-M521-KVAL                                      
064303         ELSE                                                             
064305                                                                          
064400           PERFORM IMS-GNP-M511                                           
064402           IF SEGMENT-FINNS                                               
064406             MOVE TRAN-IDTRPTNR                                           
064407                             TO W-IDTRPTNR                                
064409             PERFORM IMS-GNP-M521                                         
064410             IF SEGMENT-SAKNAS                                            
064411               PERFORM FA-LAES-GILTIG-POST                                
064412             END-IF                                                       
064416           END-IF                                                         
064417         END-IF                                                           
064420       END-IF                                                             
064430       MOVE SPACE            TO WS-MSGI-SSA-KEY-ENTER                     
064440                                WS-MSGI-SSA-KEY-NEXT                      
064500                                                                          
064600       MOVE 1                TO INDX                                      
064700       PERFORM UNTIL INDX > RAD-MAX                                       
064800       OR SEGMENT-SAKNAS                                                  
064900       OR TRAN-IDTRPTNR = 99999                                           
065100                                                                          
065200         PERFORM UNTIL INDX > RAD-MAX                                     
065300         OR SEGMENT-SAKNAS                                                
065400                                                                          
065500           MOVE TRAN-IDTRPTNR                                             
065600                             TO MOD-IDTRPTNR (INDX)                       
065900           MOVE AVG-DADATTID(3:6)                                         
066000                             TO MOD-TIDATUM (INDX)                        
066100           MOVE AVG-IDARTNR  TO MOD-IDARTNR (INDX)                        
066110                                W-IDARTNR                                 
066200           MOVE AVG-KVANTAL  TO MOD-KVANTAL (INDX)                        
066300           MOVE AVG-TETRPMED TO MOD-TETRPMED (INDX)                       
066400           MOVE TRAN-IDTRPTNR                                             
066500                             TO WS-RAD-IDTRPTNR (INDX)                    
066600           MOVE AVG-DADATTID TO WS-RAD-DADATTID (INDX)                    
066800                                                                          
066900           PERFORM IMS-GU-K611                                            
067000                                                                          
067100           IF WS-ADTRDEST = 'CDC'                                         
067200             MOVE CLAG-ADLAGOMR                                           
067300                               TO MOD-ADLAGOMR (INDX)                     
067400             MOVE CLAG-ADGANG TO MOD-ADGANG (INDX)                        
067500             MOVE CLAG-ADPLATS TO MOD-ADPLATS (INDX)                      
067600           END-IF                                                         
067700                                                                          
067800           IF WS-ADTRDEST = 'SVS'                                         
067900             MOVE CLAG-ADLAGOMR-SVS                                       
068000                               TO MOD-ADLAGOMR (INDX)                     
068100             MOVE CLAG-ADGANG-SVS                                         
068200                               TO MOD-ADGANG (INDX)                       
068300             MOVE CLAG-ADPLATS-SVS                                        
068400                               TO MOD-ADPLATS (INDX)                      
068500           END-IF                                                         
068501                                                                          
068510           IF INDX = 1                                                    
068541             MOVE TRAN-IDTRPTNR                                           
068542                               TO WS-ENTER-IDTRPTNR                       
068543             MOVE AVG-DADATTID TO WS-ENTER-DADATTID                       
068550           END-IF                                                         
068560                                                                          
068600           ADD 1             TO INDX                                      
068700                                                                          
068800           PERFORM IMS-GNP-M521                                           
068900                                                                          
069000         END-PERFORM                                                      
069100                                                                          
069200         IF SEGMENT-SAKNAS                                                
069300*                                                                         
069400*  LÄS EJ OM SIDAN ÄR FYLLD                                               
069500*                                                                         
069600           PERFORM IMS-GNP-M511                                           
069602           IF SEGMENT-FINNS                                               
069603             MOVE TRAN-IDTRPTNR                                           
069604                             TO W-IDTRPTNR                                
069605             PERFORM IMS-GNP-M521                                         
069606             IF SEGMENT-SAKNAS                                            
069607               PERFORM FA-LAES-GILTIG-POST                                
069608             END-IF                                                       
069700           END-IF                                                         
069710         END-IF                                                           
069800                                                                          
069900       END-PERFORM                                                        
069997                                                                          
069998       IF SEGMENT-FINNS                                                   
069999       AND TRAN-IDTRPTNR NOT = 99999                                      
070000          MOVE AVG-DADATTID  TO WS-NEXT-DADATTID                          
070002          MOVE TRAN-IDTRPTNR TO WS-NEXT-IDTRPTNR                          
070003          MOVE 'SE '         TO MED-IDSKYLT                               
070004          MOVE INF-MORE-INFO-EXISTS                                       
070005                               TO MED-IDMFSFEL                            
070006          CALL WMEDKONV USING MED-WMEDAREA                                
070007          MOVE MED-TEMFSFEL  TO MOD-TEMFSFEL                              
070008       ELSE                                                               
070009          MOVE SPACE         TO WS-MSGI-SSA-KEY-NEXT                      
070010       END-IF                                                             
070020                                                                          
070100     ELSE                                                                 
070200                                                                          
070300*                                                                         
070400*  IDTRPTNR HAR VALTS                                                     
070500*                                                                         
070610                                                                          
070620       IF WS-MSGI-SSA-KEY-ENTER NOT = SPACE                               
070630         MOVE WS-ENTER-IDTRPTNR                                           
070640                             TO W-IDTRPTNR                                
070650         MOVE WS-ENTER-DADATTID                                           
070660                             TO W-DADATTID                                
070671         PERFORM IMS-GNP-M511-KVAL                                        
070672         PERFORM IMS-GNP-M521-KVAL                                        
070680                                                                          
070690       ELSE                                                               
070691         IF WS-MSGI-SSA-KEY-NEXT NOT = SPACE                              
070692           MOVE WS-NEXT-IDTRPTNR                                          
070693                               TO W-IDTRPTNR                              
070694           MOVE WS-NEXT-DADATTID                                          
070695                               TO W-DADATTID                              
070696           PERFORM IMS-GNP-M511-KVAL                                      
070697           PERFORM IMS-GNP-M521-KVAL                                      
070699         ELSE                                                             
070700           MOVE WS-IDTRPTNR  TO W-IDTRPTNR                                
070702           PERFORM IMS-GNP-M511-KVAL                                      
070703           IF SEGMENT-FINNS                                               
070705             PERFORM IMS-GNP-M521                                         
070706           END-IF                                                         
070707         END-IF                                                           
070708       END-IF                                                             
070709       MOVE SPACE            TO WS-MSGI-SSA-KEY-ENTER                     
070710                                WS-MSGI-SSA-KEY-NEXT                      
071000                                                                          
071100       MOVE 1                TO INDX                                      
071200       PERFORM UNTIL INDX > RAD-MAX                                       
071300       OR SEGMENT-SAKNAS                                                  
071310       OR TRAN-IDTRPTNR NOT = W-IDTRPTNR                                  
071400                                                                          
071500           MOVE TRAN-IDTRPTNR                                             
071600                             TO MOD-IDTRPTNR (INDX)                       
071900           MOVE AVG-DADATTID(3:6)                                         
072000                             TO MOD-TIDATUM (INDX)                        
072100           MOVE AVG-IDARTNR  TO MOD-IDARTNR (INDX)                        
072110                                W-IDARTNR                                 
072200           MOVE AVG-KVANTAL  TO MOD-KVANTAL (INDX)                        
072300           MOVE AVG-TETRPMED TO MOD-TETRPMED (INDX)                       
072400           MOVE TRAN-IDTRPTNR                                             
072500                             TO WS-RAD-IDTRPTNR (INDX)                    
072600           MOVE AVG-DADATTID TO WS-RAD-DADATTID (INDX)                    
072800                                                                          
072900           PERFORM IMS-GU-K611                                            
073000                                                                          
073100           IF WS-ADTRDEST = 'CDC'                                         
073200             MOVE CLAG-ADLAGOMR                                           
073300                               TO MOD-ADLAGOMR (INDX)                     
073400             MOVE CLAG-ADGANG TO MOD-ADGANG (INDX)                        
073500             MOVE CLAG-ADPLATS TO MOD-ADPLATS (INDX)                      
073600           END-IF                                                         
073700                                                                          
073800           IF WS-ADTRDEST = 'SVS'                                         
073900             MOVE CLAG-ADLAGOMR-SVS                                       
074000                               TO MOD-ADLAGOMR (INDX)                     
074100             MOVE CLAG-ADGANG-SVS                                         
074200                               TO MOD-ADGANG (INDX)                       
074300             MOVE CLAG-ADPLATS-SVS                                        
074400                               TO MOD-ADPLATS (INDX)                      
074500           END-IF                                                         
074510                                                                          
074520           IF INDX = 1                                                    
074530             MOVE TRAN-IDTRPTNR                                           
074540                               TO WS-ENTER-IDTRPTNR                       
074550             MOVE AVG-DADATTID TO WS-ENTER-DADATTID                       
074570           END-IF                                                         
074580                                                                          
074600           ADD 1             TO INDX                                      
074700                                                                          
074800           PERFORM IMS-GNP-M521                                           
074900                                                                          
075000       END-PERFORM                                                        
075010                                                                          
075020       IF SEGMENT-FINNS                                                   
075030       AND TRAN-IDTRPTNR = W-IDTRPTNR                                     
075040          MOVE AVG-DADATTID TO WS-NEXT-DADATTID                           
075060          MOVE TRAN-IDTRPTNR TO WS-NEXT-IDTRPTNR                          
075070          MOVE 'SE '         TO MED-IDSKYLT                               
075080          MOVE INF-MORE-INFO-EXISTS                                       
075090                               TO MED-IDMFSFEL                            
075091          CALL WMEDKONV USING MED-WMEDAREA                                
075092          MOVE MED-TEMFSFEL  TO MOD-TEMFSFEL                              
075093       ELSE                                                               
075094          MOVE SPACE         TO WS-MSGI-SSA-KEY-NEXT                      
075095       END-IF                                                             
075100                                                                          
075200     END-IF                                                               
075300                                                                          
076300     PERFORM UNTIL INDX > RAD-MAX                                         
076400                                                                          
076500        MOVE MFS-STAENG-FAELT                                             
076600                             TO MOD-CMD-ATTR (INDX)                       
076700        MOVE MFS-RENSA-FAELT TO MOD-CMD (INDX)                            
076800                                MOD-IDTRPTNR (INDX)                       
076900                                MOD-TIDATUM (INDX)                        
077000                                MOD-IDARTNR (INDX)                        
077100                                MOD-KVANTAL (INDX)                        
077200                                MOD-ADLAGOMR (INDX)                       
077300                                MOD-ADGANG (INDX)                         
077400                                MOD-ADPLATS (INDX)                        
077500                                MOD-TETRPMED (INDX)                       
077600        MOVE ZERO            TO WS-RAD-IDTRPTNR (INDX)                    
077700                                WS-RAD-DADATTID (INDX)                    
077800                                                                          
077900       ADD 1                 TO INDX                                      
078000                                                                          
078100     END-PERFORM                                                          
103500     .                                                                    
103600     EJECT                                                                
103620 FA-LAES-GILTIG-POST SECTION.                                             
103621     MOVE 'FA-LAES-GILTIG-POST' TO WS-SECTION                             
103630                                                                          
103712     MOVE NEJ                TO M521-FINNS-SW                             
103713     PERFORM IMS-GNP-M511                                                 
103714     PERFORM UNTIL  SEGMENT-SAKNAS                                        
103715     OR             TRAN-IDTRPTNR = 99999                                 
103716     OR             M521-FINNS-JA                                         
103717       MOVE TRAN-IDTRPTNR    TO W-IDTRPTNR                                
103718       PERFORM IMS-GNP-M521                                               
103719       IF SEGMENT-FINNS                                                   
103720         MOVE JA             TO M521-FINNS-SW                             
103721       ELSE                                                               
103730         PERFORM IMS-GNP-M511                                             
103733       END-IF                                                             
103734     END-PERFORM                                                          
103928     .                                                                    
103929     EJECT                                                                
103930 G-KOLLA-INPUT SECTION.                                                   
103932     MOVE 'G-KOLLA-INPUT      ' TO WS-SECTION                             
103933                                                                          
103940                                                                          
104000                                                                          
104100     MOVE JA                 TO INDATA-SW                                 
104200     MOVE NEJ                TO UPD-RAD-SW                                
104300                                LOSSNING-KLAR-SW                          
104400                                PRINT-RAD-SW                              
104500                                PRINTER-VAL-SW                            
104600                                                                          
104700     IF  MID-INPUT      = ALL '+'                                         
104800        MOVE ERR-PF11-AND-NO-DATA TO MED-IDMFSFEL                         
104900        MOVE NEJ TO INDATA-SW                                             
105000     ELSE                                                                 
105100                                                                          
105200        MOVE +1              TO INDX                                      
105300        PERFORM UNTIL INDX   > RAD-MAX                                    
105400                                                                          
105500            IF  MID-CMD (INDX)  NOT = ALL '+'                             
105600            AND MID-CMD (INDX)  NOT = SPACE                               
105700                                                                          
105800                IF   MID-CMD (INDX) = 'P'                                 
106000                OR   MID-CMD (INDX) = 'B'                                 
106100                                                                          
106500                  MOVE JA    TO UPD-RAD-SW                                
106600                  MOVE MFS-ALFA-FAELT-RAETT                               
106700                             TO MOD-CMD-ATTR(INDX)                        
106800                  IF MID-CMD (INDX) = 'P'                                 
106900                    MOVE JA  TO PRINT-RAD-SW                              
107000                  END-IF                                                  
107100                ELSE                                                      
107200                    MOVE MFS-ALFA-FAELT-FEL                               
107300                             TO MOD-CMD-ATTR (INDX)                       
107400                    MOVE ERR-CORR-HILITE-FLDS                             
107500                             TO MED-IDMFSFEL                              
107600                    MOVE NEJ TO INDATA-SW                                 
107700                END-IF                                                    
107800            ELSE                                                          
107900                MOVE MFS-RENSA-FAELT                                      
108000                             TO MOD-CMD-ATTR (INDX)                       
108100            END-IF                                                        
108200                                                                          
108300            ADD 1 TO INDX                                                 
108400         END-PERFORM                                                      
108500                                                                          
108600         IF MID-LOSSNING-KLAR = ALL '+'                                   
108601         OR MID-LOSSNING-KLAR = SPACE                                     
108700            CONTINUE                                                      
108800         ELSE                                                             
108900           IF MID-LOSSNING-KLAR = JA                                      
108901           AND NOT (MID-ADINLOMR-LPL = ALL '+'                            
108902           OR       MID-ADINLOMR-LPL = SPACE)                             
109000               MOVE JA       TO LOSSNING-KLAR-SW                          
109100               MOVE MFS-ALFA-FAELT-RAETT                                  
109200                             TO MOD-LOSSNING-KLAR-ATTR                    
109300           ELSE                                                           
109320               MOVE MFS-ALFA-FAELT-FEL                                    
109500                             TO MOD-LOSSNING-KLAR-ATTR                    
109600               MOVE ERR-CORR-HILITE-FLDS                                  
109700                             TO MED-IDMFSFEL                              
109710               MOVE MED-2    TO MOD-TEMFSINF                              
109800               MOVE NEJ TO INDATA-SW                                      
109900           END-IF                                                         
109920           IF WS-IDTRPTNR = ZERO                                          
109970               MOVE MED-1    TO MOD-TEMFSINF                              
109993               MOVE NEJ TO INDATA-SW                                      
109994           END-IF                                                         
110000         END-IF                                                           
110100                                                                          
110200         IF MID-PRINTER = ALL '+'                                         
110201         OR MID-PRINTER = SPACE                                           
110300            CONTINUE                                                      
110400         ELSE                                                             
110401           MOVE JA           TO PRINTER-VAL-SW                            
110410           PERFORM GA-KOLLA-PRINTER                                       
111600         END-IF                                                           
111700     END-IF                                                               
111800                                                                          
111900     IF INDATA-FEL                                                        
112000        CALL WMEDKONV USING MED-WMEDAREA                                  
112100        MOVE MED-TEMFSFEL TO MOD-TEMFSFEL                                 
112200        PERFORM MFS-ROER-EJ-FAELT-UT                                      
112300        PERFORM MFS-ROER-EJ-FAELT-IN                                      
112400     ELSE                                                                 
112500                                                                          
112600         IF UPD-RAD-JA                                                    
112700         AND LOSSNING-KLAR-JA                                             
112800*****                                                                     
112900*****     INTE MÖJLIGT ATT UPPDATERA BÅDE ENSKILD RAD                     
113000*****     OCH LOSSNING SAMTIDIGT                                          
113100*****                                                                     
113200             MOVE NEJ     TO INDATA-SW                                    
113300             MOVE CONFLICT TO MED-IDMFSFEL                                
113400             CALL WMEDKONV USING MED-WMEDAREA                             
113500             MOVE MED-TEMFSFEL TO MOD-TEMFSFEL                            
113600             PERFORM MFS-ROER-EJ-FAELT-IN                                 
113700             PERFORM MFS-ROER-EJ-FAELT-UT                                 
113800         ELSE                                                             
113900                                                                          
113910           IF (PRINT-RAD-JA                                               
113930           AND PRINTER-VAL-NEJ)                                           
113940           OR (LOSSNING-KLAR-JA                                           
113950           AND PRINTER-VAL-JA)                                            
113960           OR (PRINT-RAD-NEJ                                              
113970           AND PRINTER-VAL-JA                                             
113980           AND WS-IDTRPTNR = ZERO)                                        
115000*****                                                                     
115100*****       MÅSTE ANGE PRINTER OM RADEN ÄR MARKERAD FÖR PRINTNING         
115300*****                                                                     
115400               MOVE NEJ   TO INDATA-SW                                    
115500               MOVE CONFLICT TO MED-IDMFSFEL                              
115600               CALL WMEDKONV USING MED-WMEDAREA                           
115700               MOVE MED-TEMFSFEL TO MOD-TEMFSFEL                          
115800               PERFORM MFS-ROER-EJ-FAELT-IN                               
115900               PERFORM MFS-ROER-EJ-FAELT-UT                               
116000           END-IF                                                         
116100         END-IF                                                           
116200     END-IF                                                               
116600     .                                                                    
116700     EJECT                                                                
116800 GA-KOLLA-PRINTER SECTION.                                                
116900                                                                          
117000     MOVE 001                TO PRT-KDCALL                                
117100     MOVE '6M'               TO PRT-IDPRTLST(1:2)                         
117200     MOVE MID-PRINTER        TO PRT-IDPRTLST(3:6)                         
117300     CALL W006PRT USING PRT-W006PRT                                       
117400     IF PRT-KDSVAR = 'F'                                                  
117500       MOVE MFS-ALFA-FAELT-FEL                                            
117600                             TO MOD-PRINTER-ATTR                          
117700       MOVE NEJ              TO PRINTER-OK                                
117710       MOVE NEJ              TO INDATA-SW                                 
117800       MOVE ERR-FEL-PRINTER  TO MED-IDMFSFEL                              
117900       CALL WMEDKONV USING MED-WMEDAREA                                   
118000       MOVE MED-TEMFSFEL     TO MOD-TEMFSFEL                              
118100       PERFORM MFS-ROER-EJ-FAELT-UT                                       
118200       PERFORM MFS-ROER-EJ-FAELT-IN                                       
118300     ELSE                                                                 
118400       MOVE PRT-BEPRTLST     TO MOD-TEMFSFEL                              
118500       MOVE PRT-IDPRTLST     TO WS-LIST-PRINTER                           
118600       MOVE MID-PRINTER      TO MOD-PRINTER                               
118700       MOVE JA               TO PRINTER-OK                                
118710       MOVE MFS-ALFA-FAELT-RAETT                                          
118720                             TO MOD-PRINTER-ATTR                          
118800     END-IF                                                               
118900                                                                          
119000     .                                                                    
119100     EJECT                                                                
119200 H-UPPDATERA SECTION.                                                     
119210     MOVE 'H-UPPDATERA        ' TO WS-SECTION                             
119300                                                                          
119400     IF PRINT-RAD-JA                                                      
119500       PERFORM S01-PRT-OPEN                                               
119600       PERFORM HE-SKRIV-L1-RAD1-7                                         
119700     END-IF                                                               
119800                                                                          
119900     MOVE +1 TO INDX                                                      
120000     PERFORM UNTIL INDX > RAD-MAX                                         
120100        IF MID-CMD (INDX) = 'P'                                           
120210           PERFORM HF-SKRIV-RAD8-A                                        
120300        END-IF                                                            
121000        IF MID-CMD (INDX) = 'B'                                           
121100           PERFORM HD-BORTTAG                                             
121200        END-IF                                                            
121300        ADD 1 TO INDX                                                     
121400     END-PERFORM                                                          
121500                                                                          
121600     IF PRINT-RAD-JA                                                      
121700                                                                          
121800       MOVE INF-PRINT-START TO MED-IDMFSINF                               
121900       CALL WMEDKONV USING MED-WMEDAREA                                   
122000       MOVE MED-TEMFSINF TO MOD-TEMFSINF                                  
122100                                                                          
122200       PERFORM S03-PRT-CLOSE                                              
122300     END-IF                                                               
122310                                                                          
122311     IF  PRINT-RAD-NEJ                                                    
122320     AND PRINTER-VAL-JA                                                   
122400                                                                          
122402       MOVE WS-ADTRDEST      TO W-ADTRDEST                                
122411       PERFORM IMS-GU-M501                                                
122420                                                                          
122421       MOVE WS-IDTRPTNR      TO W-IDTRPTNR                                
122430       IF SEGMENT-FINNS                                                   
122441         PERFORM IMS-GNP-M521                                             
122442       END-IF                                                             
122443                                                                          
122444       MOVE ZERO             TO INDX                                      
122445                                                                          
122446       PERFORM UNTIL SEGMENT-SAKNAS                                       
122447       OR INDX > TAB-MAX                                                  
122450                                                                          
122451           ADD 1             TO INDX                                      
122452           MOVE AVG-IDARTNR  TO TAB-IDARTNR  (INDX)                       
122453                                W-IDARTNR                                 
122454           MOVE AVG-KVANTAL  TO TAB-KVANTAL  (INDX)                       
122455           PERFORM IMS-GU-K611                                            
122456           IF WS-ADTRDEST = 'CDC'                                         
122457             MOVE CLAG-ADLAGOMR                                           
122458                             TO TAB-ADLAGOMR (INDX)                       
122459                                TAB-ADLAGOMR-SORT (INDX)                  
122460             MOVE CLAG-ADGANG                                             
122461                             TO TAB-ADGANG  (INDX)                        
122462                                TAB-ADGANG-SORT (INDX)                    
122463             MOVE CLAG-ADPLATS                                            
122464                             TO TAB-ADPLATS (INDX)                        
122465                                TAB-ADPLATS-SORT (INDX)                   
122466           END-IF                                                         
122467           IF WS-ADTRDEST = 'SVS'                                         
122468             MOVE CLAG-ADLAGOMR-SVS                                       
122469                             TO TAB-ADLAGOMR (INDX)                       
122470                                TAB-ADLAGOMR-SORT (INDX)                  
122471             MOVE CLAG-ADGANG-SVS                                         
122472                             TO TAB-ADGANG (INDX)                         
122473                                TAB-ADGANG-SORT (INDX)                    
122474             MOVE CLAG-ADPLATS-SVS                                        
122475                             TO TAB-ADPLATS (INDX)                        
122476                                TAB-ADPLATS-SORT (INDX)                   
122477           END-IF                                                         
122478           MOVE AVG-TETRPMED TO TAB-TETRPMED (INDX)                       
122479                                                                          
122480           PERFORM IMS-GNP-M521                                           
122481       END-PERFORM                                                        
122482                                                                          
122483       MOVE INDX             TO TAB-MAX                                   
122484                                ANTAL                                     
122485*                                                                         
122486       CALL WINTSOR USING TABELL STEGLANGD ANTAL                          
122487                    TAB-SORT (1) NYCKELLANGD                              
122488                                                                          
122489       PERFORM S01-PRT-OPEN                                               
122490       PERFORM HG-SKRIV-L2-RAD1-7                                         
122491       MOVE 1                TO INDX                                      
122492       PERFORM UNTIL INDX > TAB-MAX                                       
122493         PERFORM HH-SKRIV-RAD8-B                                          
122494         ADD 1               TO INDX                                      
122495       END-PERFORM                                                        
122496       PERFORM S03-PRT-CLOSE                                              
122497     END-IF                                                               
122503                                                                          
122510     IF MID-LOSSNING-KLAR = JA                                            
122600        PERFORM HA-LOSSNING-KLAR                                          
122700     END-IF                                                               
122800                                                                          
122900     MOVE INF-UPDATE-DONE TO MED-IDMFSINF                                 
123000     CALL WMEDKONV USING MED-WMEDAREA                                     
123100     MOVE MED-TEMFSINF TO MOD-TEMFSINF                                    
123200     PERFORM MFS-RENSA-FAELT-IN                                           
123300     .                                                                    
123400     EJECT                                                                
123500 HA-LOSSNING-KLAR SECTION.                                                
123510     MOVE 'HA-LOSSNING-KLAR   ' TO WS-SECTION                             
123600                                                                          
123700     MOVE WS-ADTRDEST        TO W-ADTRDEST                                
123800     MOVE WS-IDTRPTNR        TO W-IDTRPTNR                                
123900                                                                          
123910     PERFORM IMS-GU-M501                                                  
123920     PERFORM IMS-GNP-M511-KVAL                                            
124800                                                                          
124900     IF SEGMENT-FINNS                                                     
125000       PERFORM IMS-GHNP-M521                                              
125010     END-IF                                                               
125100                                                                          
125200     PERFORM UNTIL SEGMENT-SAKNAS                                         
127600                                                                          
127601       MOVE AVG-IDARTNR      TO W-IDARTNR                                 
127602       MOVE AVG-DADATTID     TO W-DADATTID                                
127603       COMPUTE W-DADATTID-9KOMPL = 99999999999999 - AVG-DADATTID          
127606                                                                          
127610       PERFORM IMS-GHU-M911                                               
127700       MOVE DAGENS-DATUM     TO TRP-TITRPMOT                              
127800       MOVE MSGI-IDUSER      TO TRP-IDUSER-TRP                            
127801       IF MID-ADINLOMR-LPL NOT = ALL '+'                                  
127810         MOVE MID-ADINLOMR-LPL                                            
127820                             TO TRP-ADINLOMR-LPL                          
127830       END-IF                                                             
127900       PERFORM IMS-REPL-M911                                              
127901       IF AVG-FLEJBOK = NEJ                                               
127910         PERFORM IMS-GHU-K611                                             
127911         IF WS-ADTRDEST = 'SVS'                                           
127912           ADD TRP-KVANTAL     TO CLAG-KVLS-SVS                           
127913         ELSE                                                             
127914           SUBTRACT TRP-KVANTAL                                           
127915                               FROM CLAG-KVLS-SVS                         
127916           IF CLAG-KVLS-SVS < 0                                           
127917             MOVE 0 TO CLAG-KVLS-SVS                                      
127918           END-IF                                                         
127919         END-IF                                                           
127930         PERFORM IMS-REPL-WDK611                                          
127940       END-IF                                                             
128000       PERFORM IMS-DLET-M521                                              
128200       PERFORM IMS-GHNP-M521                                              
128300     END-PERFORM                                                          
128310     PERFORM IMS-GHU-M511                                                 
128311     IF SEGMENT-FINNS                                                     
128320       PERFORM IMS-DLET-M511                                              
128330     END-IF                                                               
128400     MOVE SPACE              TO WS-MSGI-SSA-KEY-ENTER                     
129800     .                                                                    
129900     EJECT                                                                
134200 HD-BORTTAG SECTION.                                                      
134210     MOVE 'HD-BORTTAG         ' TO WS-SECTION                             
134300                                                                          
134400     MOVE WS-ADTRDEST        TO W-ADTRDEST                                
134610     MOVE WS-RAD-DADATTID (INDX)                                          
134700                             TO W-DADATTID                                
134710     MOVE WS-RAD-IDTRPTNR (INDX)                                          
134711                             TO W-IDTRPTNR                                
134800     PERFORM IMS-GHU-M521                                                 
134900     PERFORM IMS-DLET-M521                                                
134912     MOVE AVG-IDARTNR        TO W-IDARTNR                                 
134913     MOVE AVG-DADATTID       TO W-DADATTID                                
134914     COMPUTE W-DADATTID-9KOMPL = 99999999999999 - AVG-DADATTID            
134917     PERFORM IMS-GHU-M911                                                 
134918     MOVE ' SAK'              TO TRP-TETRPMED (17:4)                      
134919     PERFORM IMS-REPL-M911                                                
134920     MOVE 99999              TO TRAN-IDTRPTNR                             
134921                                W-IDTRPTNR                                
134922     PERFORM IMS-ISRT-M511-PCB2                                           
134923     ADD 1                   TO AVG-DADATTID                              
134924     MOVE SPACE              TO AVG-KDTRPSTA                              
134930     PERFORM IMS-ISRT-M521-PCB2                                           
134932     PERFORM UNTIL SEGMENT-FINNS                                          
134933       ADD +1 TO AVG-DADATTID                                             
134934       PERFORM IMS-ISRT-M521-PCB2                                         
134936     END-PERFORM                                                          
134950     IF INDX = 1                                                          
134960       MOVE SPACE            TO WS-MSGI-SSA-KEY-ENTER                     
134970     END-IF                                                               
135000     .                                                                    
135100     EJECT                                                                
135200                                                                          
135300 HE-SKRIV-L1-RAD1-7 SECTION.                                              
135310     MOVE 'HE-SKRIV-L1-RAD1-7 ' TO WS-SECTION                             
135400                                                                          
135500     IF IDSID-RAKN = +0                                                   
135600        MOVE +1 TO IDSID-RAKN                                             
135700     END-IF                                                               
135800                                                                          
135900     MOVE DAGENS-DATUM       TO L1-RAD1-DATUM                             
136000     MOVE IDSID-RAKN         TO L1-RAD1-IDSID                             
136100     MOVE L1-RAD1            TO WS-LISTRAD                                
136200     MOVE PRT-NYSIDA-RAD4    TO PRT-RADSKIP                               
136300     PERFORM S02-SKRIV-RAD                                                
136400                                                                          
136500     MOVE L1-RAD2            TO WS-LISTRAD                                
136600     MOVE PRT-AFTER-1        TO PRT-RADSKIP                               
136700     PERFORM S02-SKRIV-RAD                                                
136800                                                                          
136900     MOVE L1-RAD3            TO WS-LISTRAD                                
137000     MOVE PRT-AFTER-1        TO PRT-RADSKIP                               
137100     PERFORM S02-SKRIV-RAD                                                
137200                                                                          
137300     MOVE L1-RAD4            TO WS-LISTRAD                                
137400     MOVE PRT-AFTER-1        TO PRT-RADSKIP                               
137500     PERFORM S02-SKRIV-RAD                                                
137600                                                                          
137700     MOVE L1-RAD5            TO WS-LISTRAD                                
137800     MOVE PRT-AFTER-1        TO PRT-RADSKIP                               
137900     PERFORM S02-SKRIV-RAD                                                
138000                                                                          
138100     MOVE L1-RAD6            TO WS-LISTRAD                                
138200     MOVE PRT-AFTER-1        TO PRT-RADSKIP                               
138300     PERFORM S02-SKRIV-RAD                                                
138400                                                                          
138500     MOVE L1-RAD7            TO WS-LISTRAD                                
138600     MOVE PRT-AFTER-1        TO PRT-RADSKIP                               
138700     PERFORM S02-SKRIV-RAD                                                
138800     .                                                                    
138900     EJECT                                                                
139000                                                                          
139110 HF-SKRIV-RAD8-A SECTION.                                                 
139120     MOVE 'HF-SKRIV-RAD8-A    ' TO WS-SECTION                             
139200                                                                          
139300     MOVE WS-ADTRDEST        TO W-ADTRDEST                                
139310     MOVE WS-RAD-IDTRPTNR (INDX)                                          
139400                             TO W-IDTRPTNR                                
139500     MOVE WS-RAD-DADATTID (INDX)                                          
139600                             TO W-DADATTID                                
139700     PERFORM IMS-GHU-M521                                                 
139900     MOVE DAGENS-DATUM       TO AVG-TIUPPDAT                              
140000     MOVE MSGI-IDUSER        TO AVG-IDUSER-TRP                            
140100     PERFORM IMS-REPL-M521                                                
140200                                                                          
140300     IF RAD-IX = +0                                                       
140400       ADD +1 TO RAD-IX                                                   
140500       MOVE JA TO FOERSTA-RADEN                                           
140600     ELSE                                                                 
140700       MOVE NEJ TO FOERSTA-RADEN                                          
140800     END-IF                                                               
140900                                                                          
141000     MOVE AVG-IDARTNR        TO  L1-RAD8-IDARTNR                          
141100                                 W-IDARTNR                                
141200     MOVE AVG-KVANTAL        TO  L1-RAD8-KVANTAL                          
141300     PERFORM IMS-GU-K611                                                  
141400                                                                          
141500     IF WS-ADTRDEST = 'CDC'                                               
141600       MOVE CLAG-ADLAGOMR-SVS                                             
141700                             TO L1-RAD8-ADLAGOMR                          
141800       MOVE CLAG-ADGANG-SVS  TO L1-RAD8-ADGANG                            
141900       MOVE CLAG-ADPLATS-SVS TO L1-RAD8-ADPLATS                           
142000     END-IF                                                               
142100     IF WS-ADTRDEST = 'SVS'                                               
142200       MOVE CLAG-ADLAGOMR    TO L1-RAD8-ADLAGOMR                          
142300       MOVE CLAG-ADGANG      TO L1-RAD8-ADGANG                            
142400       MOVE CLAG-ADPLATS     TO L1-RAD8-ADPLATS                           
142500     END-IF                                                               
142600     MOVE CLAG-BEFT          TO L1-RAD8-BEFT                              
142700     MOVE AVG-TETRPMED       TO L1-RAD8-TETRPMED                          
143410     MOVE PRT-AFTER-2      TO PRT-RADSKIP                                 
143420     ADD +2                TO RAD-IX                                      
143500                                                                          
143600     MOVE L1-RAD8          TO WS-LISTRAD                                  
143700     PERFORM S02-SKRIV-RAD                                                
143800     .                                                                    
144000     EJECT                                                                
144100 HG-SKRIV-L2-RAD1-7 SECTION.                                              
144200     MOVE 'HG-SKRIV-L2-RAD1-7 ' TO WS-SECTION                             
144300                                                                          
144400     IF IDSID-RAKN = +0                                                   
144500        MOVE +1 TO IDSID-RAKN                                             
144600     END-IF                                                               
144700                                                                          
144800     MOVE DAGENS-DATUM       TO L2-RAD1-DATUM                             
144900     MOVE IDSID-RAKN         TO L2-RAD1-IDSID                             
145000     MOVE L2-RAD1            TO WS-LISTRAD                                
145100     MOVE PRT-NYSIDA-RAD4    TO PRT-RADSKIP                               
145200     PERFORM S02-SKRIV-RAD                                                
145300                                                                          
145400     MOVE L2-RAD2            TO WS-LISTRAD                                
145500     MOVE PRT-AFTER-1        TO PRT-RADSKIP                               
145600     PERFORM S02-SKRIV-RAD                                                
145700                                                                          
145800     MOVE L2-RAD3            TO WS-LISTRAD                                
145900     MOVE PRT-AFTER-1        TO PRT-RADSKIP                               
146000     PERFORM S02-SKRIV-RAD                                                
146100                                                                          
146110     MOVE WS-IDTRPTNR        TO L2-RAD4-TRPTNR                            
146200     MOVE L2-RAD4            TO WS-LISTRAD                                
146300     MOVE PRT-AFTER-1        TO PRT-RADSKIP                               
146400     PERFORM S02-SKRIV-RAD                                                
146500                                                                          
146600     MOVE L2-RAD5            TO WS-LISTRAD                                
146700     MOVE PRT-AFTER-1        TO PRT-RADSKIP                               
146800     PERFORM S02-SKRIV-RAD                                                
146900                                                                          
147000     MOVE L2-RAD6            TO WS-LISTRAD                                
147100     MOVE PRT-AFTER-1        TO PRT-RADSKIP                               
147200     PERFORM S02-SKRIV-RAD                                                
147300                                                                          
147400     MOVE L2-RAD7            TO WS-LISTRAD                                
147500     MOVE PRT-AFTER-1        TO PRT-RADSKIP                               
147600     PERFORM S02-SKRIV-RAD                                                
147700     .                                                                    
147800     EJECT                                                                
147900                                                                          
148000 HH-SKRIV-RAD8-B SECTION.                                                 
148100     MOVE 'HH-SKRIV-RAD8-B    ' TO WS-SECTION                             
148200                                                                          
148300     IF RAD-IX = +0                                                       
148400       ADD +1 TO RAD-IX                                                   
148500       MOVE JA TO FOERSTA-RADEN                                           
148600     ELSE                                                                 
148700       MOVE NEJ TO FOERSTA-RADEN                                          
148800     END-IF                                                               
148900                                                                          
149000     MOVE TAB-IDARTNR (INDX) TO  L2-RAD8-IDARTNR                          
149100     MOVE TAB-KVANTAL (INDX) TO  L2-RAD8-KVANTAL                          
149200     MOVE TAB-ADLAGOMR (INDX)                                             
149300                             TO L2-RAD8-ADLAGOMR                          
149400     MOVE TAB-ADGANG (INDX)  TO L2-RAD8-ADGANG                            
149500     MOVE TAB-ADPLATS (INDX) TO L2-RAD8-ADPLATS                           
149600     MOVE TAB-TETRPMED (INDX)                                             
149700                             TO L2-RAD8-TETRPMED                          
149710     MOVE PRT-AFTER-2      TO PRT-RADSKIP                                 
149720     ADD +2                TO RAD-IX                                      
150500                                                                          
150600     MOVE L2-RAD8          TO WS-LISTRAD                                  
150610     PERFORM S02-SKRIV-RAD                                                
150620     .                                                                    
150630     EJECT                                                                
150700 MFS-RENSA-FAELT-UT SECTION.                                              
150800                                                                          
150900*    --- ALLA UTDATA-FÄLT                                                 
151000     MOVE MFS-RENSA-FAELT    TO MOD-ADTRDEST-UT                           
151100                                MOD-IDTRPTNR-UT                           
151200     MOVE +1 TO INDX                                                      
151300     PERFORM UNTIL INDX > RAD-MAX                                         
151400       MOVE MFS-RENSA-FAELT                                               
151500                             TO MOD-IDTRPTNR (INDX)                       
151501                                MOD-TIDATUM (INDX)                        
151510                                MOD-IDARTNR (INDX)                        
151600                                MOD-KVANTAL (INDX)                        
151700                                MOD-ADLAGOMR(INDX)                        
151800                                MOD-ADGANG (INDX)                         
151900                                MOD-ADPLATS (INDX)                        
152400                                MOD-TETRPMED(INDX)                        
152500       ADD +1 TO INDX                                                     
152600     END-PERFORM                                                          
152700     .                                                                    
152800     EJECT                                                                
152900 MFS-RENSA-FAELT-IN SECTION.                                              
153000                                                                          
153100*    --- ALLA INDATA-FÄLT                                                 
153200     MOVE MFS-RENSA-FAELT    TO MOD-ADTRDEST-IN                           
153300                                MOD-IDTRPTNR-IN                           
153400                                MOD-LOSSNING-KLAR                         
153410                                MOD-ADINLOMR-LPL                          
153500                                MOD-PRINTER                               
153600     MOVE +1 TO INDX                                                      
153700     PERFORM UNTIL INDX > RAD-MAX                                         
153800       MOVE MFS-RENSA-FAELT  TO MOD-CMD (INDX)                            
153900       ADD +1                TO INDX                                      
154000     END-PERFORM                                                          
154100     .                                                                    
154200     EJECT                                                                
154300 MFS-ROER-EJ-FAELT-UT  SECTION.                                           
154400                                                                          
154500*    --- ALLA UTDATA-FÄLT                                                 
154600     MOVE MFS-ROER-EJ-FAELT  TO MOD-ADTRDEST-UT                           
154700                                MOD-IDTRPTNR-UT                           
154800     MOVE +1 TO INDX                                                      
154900     PERFORM UNTIL INDX > RAD-MAX                                         
155000       MOVE MFS-ROER-EJ-FAELT                                             
155100                             TO MOD-IDTRPTNR (INDX)                       
155101                                MOD-TIDATUM (INDX)                        
155110                                MOD-IDARTNR (INDX)                        
155200                                MOD-KVANTAL (INDX)                        
155300                                MOD-ADLAGOMR(INDX)                        
155400                                MOD-ADGANG (INDX)                         
155500                                MOD-ADPLATS (INDX)                        
156000                                MOD-TETRPMED(INDX)                        
156100       ADD +1 TO INDX                                                     
156200     END-PERFORM                                                          
156300     .                                                                    
156400     EJECT                                                                
156500 MFS-ROER-EJ-FAELT-IN  SECTION.                                           
156600                                                                          
156700*    --- ALLA INDATA-FÄLT                                                 
156800     MOVE MFS-ROER-EJ-FAELT  TO MOD-ADTRDEST-IN                           
156900                                MOD-IDTRPTNR-IN                           
157000                                MOD-LOSSNING-KLAR                         
157100                                MOD-ADINLOMR-LPL                          
157110                                MOD-PRINTER                               
157200     MOVE +1 TO INDX                                                      
157300     PERFORM UNTIL INDX > RAD-MAX                                         
157400       MOVE MFS-ROER-EJ-FAELT                                             
157500                             TO MOD-CMD (INDX)                            
157600       ADD +1                TO INDX                                      
157700     END-PERFORM                                                          
157800     .                                                                    
157900     EJECT                                                                
158000 S01-PRT-OPEN SECTION.                                                    
158100                                                                          
158200     CALL W006PRS1  USING PRT-SPOOL-OVR                                   
158300                          PRT-OPEN                                        
158400                          WS-LIST-PRINTER                                 
158500                          ALT-PCB                                         
158600                          WS-DUMMY                                        
158700                          WS-DUMMY                                        
158800     .                                                                    
158900     EJECT                                                                
159000 S02-SKRIV-RAD SECTION.                                                   
159100                                                                          
159200     CALL W006PRS1  USING PRT-SPOOL-OVR                                   
159300                          PRT-WRITE                                       
159400                          WS-LIST-PRINTER                                 
159500                          ALT-PCB                                         
159600                          PRT-RADSKIP                                     
159700                          WS-RAD                                          
159800     .                                                                    
159900     EJECT                                                                
160000 S03-PRT-CLOSE SECTION.                                                   
160100                                                                          
160200     CALL W006PRS1  USING PRT-SPOOL-OVR                                   
160300                          PRT-CLOSE                                       
160400                          WS-LIST-PRINTER                                 
160500                          ALT-PCB                                         
160600                          WS-DUMMY                                        
160700                          WS-DUMMY                                        
160800     .                                                                    
160900     EJECT                                                                
161000* --- IMS SEKTIONER ---                                                   
161100     SKIP3                                                                
161200 IMS-GET-MSG SECTION.                                                     
161300     MOVE '  QC' TO GODK-STATUSKODER                                      
161400     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
161500     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
161600     PERFORM IMS-STATUSKONTROLL                                           
161700     .                                                                    
161800     SKIP2                                                                
161900 IMS-INSERT-MSG SECTION.                                                  
162000     IF SWEDISH-TEXT                                                      
162100        IF MSGI-IDLAND-SPR NOT = 'GB'                                     
162200           MOVE '0' TO MFS-KDHUVOMR                                       
162300        END-IF                                                            
162400     END-IF                                                               
162500     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
162600     MOVE SPACE TO GODK-STATUSKODER                                       
162700     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
162800     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
162900     PERFORM IMS-STATUSKONTROLL                                           
163000     .                                                                    
163800     EJECT                                                                
163900 IMS-GU-K611 SECTION.                                                     
164000                                                                          
164100     STRING 'WDK601  (IDARTNR  =' W-IDARTNR-X ')'                         
164200          DELIMITED BY SIZE INTO SSA1                                     
164300     MOVE 'WDK611  '         TO SSA2                                      
164400     MOVE '  ' TO GODK-STATUSKODER                                        
164500     CALL CBLTDLI USING GU WDK6-PCB DLI-IO-WDK611 SSA1 SSA2               
164600     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
164700     PERFORM IMS-STATUSKONTROLL                                           
164800     .                                                                    
164810     EJECT                                                                
164820 IMS-GHU-K611 SECTION.                                                    
164830                                                                          
164840     STRING 'WDK601  (IDARTNR  =' W-IDARTNR-X ')'                         
164850          DELIMITED BY SIZE INTO SSA1                                     
164860     MOVE 'WDK611  '         TO SSA2                                      
164870     MOVE '  ' TO GODK-STATUSKODER                                        
164880     CALL CBLTDLI USING GHU WDK6-PCB DLI-IO-WDK611 SSA1 SSA2              
164890     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
164891     PERFORM IMS-STATUSKONTROLL                                           
164892     .                                                                    
164893     EJECT                                                                
164895 IMS-REPL-WDK611 SECTION.                                                 
164896     SKIP2                                                                
164897     MOVE '  ' TO GODK-STATUSKODER                                        
164898     CALL CBLTDLI USING REPL WDK6-PCB DLI-IO-WDK611                       
164899     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
164900     PERFORM IMS-STATUSKONTROLL                                           
164901     .                                                                    
164910     EJECT                                                                
165000 IMS-GU-M501   SECTION.                                                   
165100                                                                          
165200     STRING 'WDM501  (ADTRDEST =' W-ADTRDEST ')'                          
165300          DELIMITED BY SIZE INTO SSA1                                     
165400     MOVE SPACE  TO GODK-STATUSKODER                                      
165500     CALL CBLTDLI USING GU  WDM5-PCB DLI-IO-WDM501 SSA1                   
165600     MOVE WDM5-STATUS-CODE TO STATUS-WS                                   
165700     PERFORM IMS-STATUSKONTROLL                                           
165800     .                                                                    
165900     EJECT                                                                
166000 IMS-GNP-M511 SECTION.                                                    
166100                                                                          
166200     STRING 'WDM511    '                                                  
166300          DELIMITED BY SIZE INTO SSA1                                     
166400     MOVE '  GE' TO GODK-STATUSKODER                                      
166500     CALL CBLTDLI USING GNP WDM5-PCB DLI-IO-WDM511 SSA1                   
166600     MOVE WDM5-STATUS-CODE TO STATUS-WS                                   
166700     PERFORM IMS-STATUSKONTROLL                                           
166800     .                                                                    
166900     EJECT                                                                
166920 IMS-GNP-M511-KVAL SECTION.                                               
166930                                                                          
166951     STRING 'WDM511  (IDTRPTNR =' W-IDTRPTNR-X ')'                        
166952          DELIMITED BY SIZE INTO SSA1                                     
166960     MOVE '  GE' TO GODK-STATUSKODER                                      
166970     CALL CBLTDLI USING GNP WDM5-PCB DLI-IO-WDM511 SSA1                   
166980     MOVE WDM5-STATUS-CODE TO STATUS-WS                                   
166990     PERFORM IMS-STATUSKONTROLL                                           
166991     .                                                                    
166992     EJECT                                                                
168120 IMS-GHU-M511   SECTION.                                                  
168130                                                                          
168140     STRING 'WDM501  (ADTRDEST =' W-ADTRDEST ')'                          
168150          DELIMITED BY SIZE INTO SSA1                                     
168160     STRING 'WDM511  (IDTRPTNR =' W-IDTRPTNR-X ')'                        
168170          DELIMITED BY SIZE INTO SSA2                                     
168171     MOVE '  GE' TO GODK-STATUSKODER                                      
168190     CALL CBLTDLI USING GHU  WDM5-PCB DLI-IO-WDM511 SSA1 SSA2             
168191     MOVE WDM5-STATUS-CODE TO STATUS-WS                                   
168192     PERFORM IMS-STATUSKONTROLL                                           
168193     .                                                                    
168194     EJECT                                                                
168200 IMS-ISRT-M511-PCB2 SECTION.                                              
168300                                                                          
168400     STRING 'WDM501  (ADTRDEST =' W-ADTRDEST ')'                          
168500          DELIMITED BY SIZE INTO SSA1                                     
168600     STRING 'WDM511    '                                                  
168700          DELIMITED BY SIZE INTO SSA2                                     
168900     MOVE '  II' TO GODK-STATUSKODER                                      
168920     CALL CBLTDLI USING ISRT WDM5-2-PCB                                   
169000                             DLI-IO-WDM511 SSA1 SSA2                      
169100     MOVE WDM5-2-STATUS-CODE TO STATUS-WS                                 
169200     PERFORM IMS-STATUSKONTROLL                                           
169300     .                                                                    
169400     EJECT                                                                
169500 IMS-ISRT-M521-PCB2 SECTION.                                              
169600                                                                          
169700     STRING 'WDM501  (ADTRDEST =' W-ADTRDEST ')'                          
169800          DELIMITED BY SIZE INTO SSA1                                     
169900     STRING 'WDM511  (IDTRPTNR =' W-IDTRPTNR-X ')'                        
170000          DELIMITED BY SIZE INTO SSA2                                     
170100     STRING 'WDM521     '                                                 
170200          DELIMITED BY SIZE INTO SSA3                                     
170300     MOVE '  II' TO GODK-STATUSKODER                                      
170410     CALL CBLTDLI USING ISRT WDM5-2-PCB                                   
170500                             DLI-IO-WDM521 SSA1 SSA2 SSA3                 
170510     MOVE WDM5-2-STATUS-CODE TO STATUS-WS                                 
170700     PERFORM IMS-STATUSKONTROLL                                           
170800     .                                                                    
170900     EJECT                                                                
171000 IMS-GNP-M521 SECTION.                                                    
171100                                                                          
171110     STRING 'WDM511  (IDTRPTNR =' W-IDTRPTNR-X ')'                        
171120          DELIMITED BY SIZE INTO SSA1                                     
171200     STRING 'WDM521    '                                                  
171300          DELIMITED BY SIZE INTO SSA2                                     
171400     MOVE '  GE' TO GODK-STATUSKODER                                      
171500     CALL CBLTDLI USING GNP WDM5-PCB DLI-IO-WDM521 SSA1 SSA2              
171600     MOVE WDM5-STATUS-CODE TO STATUS-WS                                   
171700     PERFORM IMS-STATUSKONTROLL                                           
171800     .                                                                    
171810     EJECT                                                                
171820 IMS-GHNP-M521 SECTION.                                                   
171830                                                                          
171831     STRING 'WDM511  (IDTRPTNR =' W-IDTRPTNR-X ')'                        
171832          DELIMITED BY SIZE INTO SSA1                                     
171840     STRING 'WDM521    '                                                  
171850          DELIMITED BY SIZE INTO SSA2                                     
171860     MOVE '  GE' TO GODK-STATUSKODER                                      
171870     CALL CBLTDLI USING GHNP WDM5-PCB DLI-IO-WDM521 SSA1 SSA2             
171880     MOVE WDM5-STATUS-CODE TO STATUS-WS                                   
171890     PERFORM IMS-STATUSKONTROLL                                           
171891     .                                                                    
171900     EJECT                                                                
173320 IMS-GNP-M521-KVAL   SECTION.                                             
173330                                                                          
173331     STRING 'WDM511  (IDTRPTNR =' W-IDTRPTNR-X ')'                        
173332          DELIMITED BY SIZE INTO SSA1                                     
173340     STRING 'WDM521  (DADATTID =' W-DADATTID-X ')'                        
173350          DELIMITED BY SIZE INTO SSA2                                     
173351     MOVE '  GE' TO GODK-STATUSKODER                                      
173370     CALL CBLTDLI USING GNP WDM5-PCB DLI-IO-WDM521 SSA1 SSA2              
173380     MOVE WDM5-STATUS-CODE TO STATUS-WS                                   
173390     PERFORM IMS-STATUSKONTROLL                                           
173391     .                                                                    
173392     EJECT                                                                
173400 IMS-GHU-M521   SECTION.                                                  
173500                                                                          
173600     STRING 'WDM501  (ADTRDEST =' W-ADTRDEST ')'                          
173700          DELIMITED BY SIZE INTO SSA1                                     
173800     STRING 'WDM511  (IDTRPTNR =' W-IDTRPTNR-X ')'                        
173900          DELIMITED BY SIZE INTO SSA2                                     
174000     STRING 'WDM521  (DADATTID =' W-DADATTID-X ')'                        
174100          DELIMITED BY SIZE INTO SSA3                                     
174200     MOVE SPACE  TO GODK-STATUSKODER                                      
174300     CALL CBLTDLI USING GHU WDM5-PCB DLI-IO-WDM521 SSA1 SSA2 SSA3         
174400     MOVE WDM5-STATUS-CODE TO STATUS-WS                                   
174500     PERFORM IMS-STATUSKONTROLL                                           
174600     .                                                                    
174700     EJECT                                                                
174800 IMS-REPL-M521 SECTION.                                                   
174900                                                                          
175000     MOVE '  ' TO GODK-STATUSKODER                                        
175100     CALL CBLTDLI USING REPL WDM5-PCB DLI-IO-WDM521                       
175200     MOVE WDM5-STATUS-CODE TO STATUS-WS                                   
175300     PERFORM IMS-STATUSKONTROLL                                           
175400     .                                                                    
175500     EJECT                                                                
175600 IMS-DLET-M521 SECTION.                                                   
175700                                                                          
175800     MOVE '  ' TO GODK-STATUSKODER                                        
175900     CALL CBLTDLI USING DLET WDM5-PCB DLI-IO-WDM521                       
176000     MOVE WDM5-STATUS-CODE TO STATUS-WS                                   
176100     PERFORM IMS-STATUSKONTROLL                                           
176200     .                                                                    
176300     EJECT                                                                
176500 IMS-DLET-M511 SECTION.                                                   
176600                                                                          
176700     MOVE '  ' TO GODK-STATUSKODER                                        
176800     CALL CBLTDLI USING DLET WDM5-PCB DLI-IO-WDM511                       
176900     MOVE WDM5-STATUS-CODE TO STATUS-WS                                   
177000     PERFORM IMS-STATUSKONTROLL                                           
177100     .                                                                    
177200     EJECT                                                                
178620 IMS-GHU-M911 SECTION.                                                    
178630                                                                          
178640     STRING 'WDM901  (IDARTNR  =' W-IDARTNR-X ')'                         
178650          DELIMITED BY SIZE INTO SSA1                                     
178661     STRING 'WDM911  (DADATTI9 =' W-DADATTID-9KOMPL-X ')'                 
178670          DELIMITED BY SIZE INTO SSA2                                     
178680     MOVE '  ' TO GODK-STATUSKODER                                        
178690     CALL CBLTDLI USING GHU WDM9-PCB                                      
178691                             DLI-IO-WDM911 SSA1 SSA2                      
178692     MOVE WDM9-STATUS-CODE TO STATUS-WS                                   
178693     PERFORM IMS-STATUSKONTROLL                                           
178694     .                                                                    
178695     EJECT                                                                
178696 IMS-REPL-M911 SECTION.                                                   
178697                                                                          
178698     MOVE '  ' TO GODK-STATUSKODER                                        
178699     CALL CBLTDLI USING REPL WDM9-PCB DLI-IO-WDM911                       
178700     MOVE WDM9-STATUS-CODE TO STATUS-WS                                   
178701     PERFORM IMS-STATUSKONTROLL                                           
178702     .                                                                    
178710     EJECT                                                                
178800 IMS-STATUSKONTROLL SECTION.                                              
178900     SET STATUS-IX TO 1                                                   
179000     SEARCH GODK-STATUS                                                   
179100       AT END                                                             
179200         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
179300         DELIMITED BY SIZE INTO FELTEXT                                   
179400         CALL FELLOG                                                      
179500       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
179600         CONTINUE                                                         
179700     END-SEARCH                                                           
179800     .                                                                    
