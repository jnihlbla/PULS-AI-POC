000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W5108000.                                                
000300 AUTHOR.         BO HAMMARIN.                                             
000400 DATE-WRITTEN.   AUGUSTI-1998.                                            
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*    FUNKTION:                                                            
000800*       -PGM LÄSER EKONOMISKA HÄNDELSETRANSAKTIONER FRÅN WDR8             
000900*        OCH MATCHAR DESSA MOT EKONOMISKA STYRPARAMETRAR FÖR              
001000*        1 EKONOMISKA STYRPARAMETRAR                                      
001100*                                                                         
001200*        FÖR ATT I SLUTÄNDEN PRODUCERA                                    
001300*        1 POSTER TILL LEVA1                                              
001400*        2 POSTER TILL AVSTÄMNING OCH LAGERVÄRDERING                      
001500*                                                                         
001600*       -PROGRAMMET LÄSER     WDH5 REGELVERKET SAMT                       
001700*        REGISTER FÖR ÅRSKURS WDG2 (HTYP 9305)                            
001800*                                                                         
001900*    ABENDKODER:                                                          
002000*        U0016 -  . . . .                                                 
002100*        U1000 -  . . . .                                                 
002200*                                                                         
002300     SKIP3                                                                
002400 ENVIRONMENT DIVISION.                                                    
002500     SKIP2                                                                
002600 INPUT-OUTPUT SECTION.                                                    
002700                                                                          
002800 FILE-CONTROL.                                                            
002900     SKIP2                                                                
003000*          --- HÄNDELSETRANSAKTIONER                                      
003100     SELECT W5103A                     ASSIGN TO W51080D1.                
003200     SKIP2                                                                
003300*          --- LEVA1 INTERFACE                                            
003400     SELECT W51080                     ASSIGN TO W51080D2.                
003500     SKIP2                                                                
003600*          --- AVSTÄMNINGSPOSTER                                          
003700     SELECT W51081                     ASSIGN TO W51080D3.                
003800     EJECT                                                                
003900 DATA DIVISION.                                                           
004000     SKIP2                                                                
004100 FILE SECTION.                                                            
004200     SKIP3                                                                
004300 FD  W5103A                                                               
004400     RECORDING       F                                                    
004500     BLOCK CONTAINS  0.                                                   
004600                                                                          
004700*01  -COPY W51080        -L.                                              
004800                                                                          
004900     SKIP3                                                                
005000 FD  W51080                                                               
005100     RECORDING       F                                                    
005200     BLOCK CONTAINS  0.                                                   
005300                                                                          
005400*01  LEVA-POST -COPY A432GSDB -L.                                         
005500     SKIP3                                                                
005600 FD  W51081                                                               
005700     RECORDING       F                                                    
005800     BLOCK CONTAINS  0.                                                   
005900                                                                          
006000*01  AVST-POST -COPY W51068   -L.                                         
006100     EJECT                                                                
006200 WORKING-STORAGE SECTION.                                                 
006300                                                                          
006400 77  IDPGM                       PIC X(8)    VALUE 'W5108000'.            
006500 77  JA                          PIC X       VALUE 'J'.                   
006600 77  NEJ                         PIC X       VALUE 'N'.                   
006700 77  INDX                        PIC S9(2)   VALUE +0 COMP SYNC.          
006800 77  W-IDINK                     PIC 9(3)    VALUE ZERO.                  
006810 77  W-DATE-AAMM                 PIC 9(4)    VALUE ZERO.                  
006900                                                                          
006910 77  WS-FOUND-SW                 PIC X       VALUE 'N'.                   
006920     88  WS-FOUND                            VALUE 'J'.                   
006930     88  WS-NOT-FOUND                        VALUE 'N'.                   
006940                                                                          
007000 77  W5103A-EOF-SW               PIC X       VALUE 'N'.                   
007100     88  END-OF-W5103A                       VALUE 'J'.                   
007200                                                                          
007300 01   IN-IX                       PIC 9(1)    VALUE ZERO.                 
007400 01   UT-IX                       PIC 9(1)    VALUE ZERO.                 
007500 01   WS-IDDISTR                  PIC 9(5).                               
007510 01   W-IDFS                      PIC 9(8).                               
007600 01   W-PRARTBEL-NUM              PIC 9(13).                              
007700 01   WS-IN-PRARTBEL-PR           PIC S9(8)V9(5)      COMP-3.             
007800 01   WS-BEL-BEST                 PIC S9(8)V9(9)      COMP-3.             
007900                                                                          
008000 01  WORKAREA.                                                            
008100     03  W-PRKURS                 PIC S9(6)V9(5) COMP-3  VALUE +0.        
008200     03  W-REVALUTA               PIC S9(3)      COMP-3  VALUE +0.        
008300                                                                          
008800 01  WS-DATUM.                                                            
008900     03  WS-AKTDATUM             PIC 9(6).                                
009000                                                                          
009100 01  WS-R3-ACCOUNT.                                                       
009200     03  WS-R3-ACCOUNT-ALFA.                                              
009300         05 FILLER               PIC X(4).                                
009400         05 WS-R3-ACCOUNT-6      PIC X(6).                                
009500     03  WS-R3-ACCOUNT-DISP REDEFINES WS-R3-ACCOUNT-ALFA.                 
009600         05 WS-R3-ACCOUNT-10     PIC 9(10).                               
009700                                                                          
009800     EJECT                                                                
009900 01  DYNAMISKA-SUBPROGRAM.                                                
010000*                                                                         
010100     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
010200     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
010300     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
010400     03  DATKORT                 PIC X(8)    VALUE 'DATKORT'.             
010500     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
010510     03  W510CURR                PIC X(8)    VALUE 'W510CURR'.            
010600     SKIP2                                                                
010700*    --- PARAMETRAR TILL ABEND                                            
010800                                                                          
010900 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
011000 77  RKOD-ABEND-UTAN-DUMP        PIC S9(4)   COMP VALUE +16.              
011100 77  RKOD-ABEND-MED-DUMP         PIC S9(4)   COMP VALUE +1000.            
011200     EJECT                                                                
011300*    --- PARAMETRAR TILL DATKORT                                          
011400*                                                                         
011500 01  DATUMKORT-ID                PIC X(6)    VALUE 'WDATUM'.              
011600     SKIP2                                                                
011700*01  -COPY WDATKORT                                                       
011800     EJECT                                                                
011900*    --- VALID IDDC CODES                                                 
012000*                                                                         
012100*01  -COPY WWDC99                                                         
012200                                                                          
012300 01  FILLER                      PIC X(16)   VALUE 'WWIDFTG '.            
012400*01  -COPY WWIDFTG                                                        
012401                                                                          
012402*    --- PARAMETRAR TILL POSTSUM                                          
012403*                                                                         
012404*01  -COPY W0005   -PRE  POSTSUM-                                         
012405*                                                                         
012500     EJECT                                                                
012600*01  -COPY W510CURR                                                       
012700*                                                                         
012800*                                                                         
012900*01  -COPY WWLANDX2                                                       
012910*                                                                         
013000 01  IN-AREA-START               PIC X(24)   VALUE                        
013100                                 'IN-AREA-START  '.                       
013200                                                                          
013300*01  AREA -COPY W51080     -PRE IN-                                       
013400                                                                          
013500     EJECT                                                                
013600 01  UT-AREA-START               PIC X(24)   VALUE                        
013700                                 'UT-AREA-START  '.                       
013800     SKIP2                                                                
013900                                                                          
014000 01  LEVA-AREA.                                                           
014100*    03  -COPY A432GSDB -PRE MR-.                                         
014200     EJECT                                                                
014300 01  FILLER                      PIC X(24)   VALUE 'AVST-AREA'.           
014400                                                                          
014500 01  AVST-AREA.                                                           
014600*    03  -COPY W51068 -PRE AVST-                                          
014700     EJECT                                                                
014800*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
014900*                                                                         
015000     EJECT                                                                
015100 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
015200     SKIP3                                                                
015300 01  NYCKLAR-TILL-DLI.                                                    
015400     03  W-WDH501KY-X.                                                    
015410         05  W-IDFTG             PIC 9(2)    VALUE ZERO.                  
015500         05  W-KDEKHHT           PIC X(3)    VALUE SPACE.                 
015600     03  W-KDEKSHT-X.                                                     
015700         05  W-KDEKSHT           PIC X(3)    VALUE SPACE.                 
015800     03  W-KDEKNIVA-X.                                                    
015900         05  W-KDEKNIVA          PIC X(5)    VALUE SPACE.                 
016000     03  W-WDH531KY-X.                                                    
016100         05  W-IDSYSMOT          PIC X(6)    VALUE SPACE.                 
016200         05  W-IDPTYP            PIC X(3)    VALUE SPACE.                 
016300         05  W-IDSEKVNR          PIC S9(3)   VALUE ZERO COMP-3.           
016400     03  W-IDARTNR-X.                                                     
016500         05  W-IDARTNR           PIC S9(9)   VALUE ZERO COMP-3.           
016600     03  W-DAINLEV-X.                                                     
016700         05  W-DAINLEV           PIC 9(16)   VALUE ZERO.                  
016800     03  W-IDGMT-X.                                                       
016900         05 W-IDDISTR            PIC S9(5)   VALUE ZERO COMP-3.           
017000         05 W-IDKUNDNR           PIC S9(7)   VALUE ZERO COMP-3.           
017010     03  W-WDB101KY-X.                                                    
017020         05  W-IDPARTNR          PIC X(9)    VALUE SPACE.                 
017030         05  W-WDB1-IDFTG        PIC 9(2)    VALUE ZERO.                  
017040                                                                          
017100                                                                          
017200*    --- STATUS-KOD FRÅN IMS                                              
017300 01  STATUS-WS                   PIC XX.                                  
017400     88  SEGMENT-FINNS                       VALUE '  '.                  
017500     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
017600     SKIP2                                                                
017700 01  GODK-STATUSKODER.                                                    
017800     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
017900     SKIP3                                                                
018000 01  SSA1                        PIC X(64).                               
018100 01  SSA2                        PIC X(64).                               
018200 01  SSA3                        PIC X(64).                               
018300 01  SSA4                        PIC X(64).                               
018400     EJECT                                                                
018500*    --- IMS FUNKTIONSKODER                                               
018600*01  -COPY W0003                                                          
018700     EJECT                                                                
018800*    ---  DLI INPUT-OUTPUT AREA                                           
018900 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDH501'.                      
019000 01  DLI-IO-WDH501.                                                       
019100*    03  -COPY WDH501                                                     
019200     EJECT                                                                
019300 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDH511'.                      
019400 01  DLI-IO-WDH511.                                                       
019500*    03  -COPY WDH511                                                     
019600     EJECT                                                                
019700 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDH521'.                      
019800 01  DLI-IO-WDH521.                                                       
019900*    03  -COPY WDH521                                                     
020000     EJECT                                                                
020100 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDH531'.                      
020200 01  DLI-IO-WDH531.                                                       
020300*    03  -COPY WDH531                                                     
020400     EJECT                                                                
020500 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDL201'.                      
020600 01  DLI-IO-WDL201.                                                       
020700*    03  -COPY WDL201                                                     
020800     EJECT                                                                
020900 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDL211'.                      
020910 01  DLI-IO-WDL211.                                                       
020920*    03  -COPY WDL211                                                     
020930     EJECT                                                                
020940 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDL222'.                      
020950 01  DLI-IO-WDL222.                                                       
020960*    03  -COPY WDL222                                                     
020970     EJECT                                                                
020980 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDB201'.                      
020990 01  DLI-IO-WDB201.                                                       
020991*    03  -COPY WDB201                                                     
020992     EJECT                                                                
020993 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDB101'.                      
020994 01  DLI-IO-WDB101.                                                       
020995*    03  -COPY WDB101                                                     
020996     EJECT                                                                
021000 LINKAGE SECTION.                                                         
021100                                                                          
021200*01  -COPY W0008  -PRE WDH5-                                              
021300     05  FILLER                  PIC X.                                   
021400                                                                          
021500*01  -COPY W0008  -PRE 9305-                                              
021600     05  FILLER                  PIC X.                                   
021610*01  -COPY W0008  -PRE WDL2-                                              
021620     05  FILLER                  PIC X.                                   
021630     EJECT                                                                
021640*01  -COPY W0008  -PRE WDB2-                                              
021650     05  FILLER                  PIC X.                                   
021660     EJECT                                                                
021670*01  -COPY W0008  -PRE WDB1-                                              
021680     05  FILLER                  PIC X.                                   
021690     EJECT                                                                
021700     EJECT                                                                
022010 PROCEDURE DIVISION  USING WDH5-PCB 9305-PCB WDL2-PCB WDB2-PCB            
022020                           WDB1-PCB.                                      
022030 MAIN SECTION.                                                            
022040     ENTRY 'DLITCBL' USING WDH5-PCB 9305-PCB WDL2-PCB WDB2-PCB            
022050                           WDB1-PCB.                                      
022060                                                                          
022100                                                                          
022200     PERFORM A-INIT                                                       
022300                                                                          
022400     PERFORM S01-LAES-W5103A                                              
022500     PERFORM UNTIL END-OF-W5103A                                          
022600       MOVE SPACE              TO LEVA-AREA                               
022700       PERFORM B-KOMPL-LEVA1-MED-REGELBAS                                 
022800       PERFORM C-SKAPA-RESTEN-LEVA1-TRANS                                 
022810       IF IN-IDFTG = 57                                                   
022900         PERFORM D-SKAPA-AVST-POST                                        
022910       END-IF                                                             
023000       PERFORM S01-LAES-W5103A                                            
023100     END-PERFORM                                                          
023200                                                                          
023300     PERFORM Z-FINIT                                                      
023400                                                                          
023500     MOVE ZERO TO RETURN-CODE                                             
023600     GOBACK                                                               
023700     .                                                                    
023800     EJECT                                                                
023810                                                                          
023900 A-INIT SECTION.                                                          
024100     OPEN INPUT  W5103A                                                   
024200                                                                          
024300     OPEN OUTPUT W51080 W51081                                            
024400                                                                          
024500     CALL DATKORT USING IDPGM DATUMKORT-ID DATUMKORT                      
024600     MOVE D-AAR        TO WS-AKTDATUM(1:2)                                
024700     MOVE D-MAANAD     TO WS-AKTDATUM(3:2)                                
024800     MOVE D-DAG        TO WS-AKTDATUM(5:2)                                
024900     MOVE IDPGM        TO POSTSUM-PROGNAMN                                
025000                                                                          
025100     INITIALIZE MR-A432GSDB                                               
025200     .                                                                    
025300     EJECT                                                                
025310                                                                          
025400 B-KOMPL-LEVA1-MED-REGELBAS SECTION.                                      
025501     IF IN-IDFTG = 57                                                     
025510       MOVE WC-IDFTG-PV      TO W-IDFTG                                   
025600       MOVE '001'            TO W-KDEKHHT                                 
025700       MOVE '001'            TO W-KDEKSHT                                 
025800       MOVE 'DET'            TO W-KDEKNIVA                                
025900       MOVE 'LEVA1'          TO W-IDSYSMOT                                
026000       MOVE '000'            TO W-IDPTYP                                  
026100       MOVE 2                TO W-IDSEKVNR                                
026200       PERFORM IMS-GU-WDH531                                              
026300       MOVE 'O'              TO MR-FLAGGA1F                               
026400       MOVE SYST-IDANALYS    TO MR-COSTCTR-FREIGHT                        
026500                                                                          
026510       MOVE WC-IDFTG-PV      TO W-IDFTG                                   
026600       MOVE '001'            TO W-KDEKHHT                                 
026700       MOVE '001'            TO W-KDEKSHT                                 
026800       MOVE 'DET'            TO W-KDEKNIVA                                
026900       MOVE 'LEVA1'          TO W-IDSYSMOT                                
027000       MOVE '000'            TO W-IDPTYP                                  
027100       MOVE 3                TO W-IDSEKVNR                                
027200       PERFORM IMS-GU-WDH531                                              
027300       MOVE 'O'              TO MR-FLAGGA3VD                              
027400       MOVE SYST-IDANALYS    TO MR-COSTCTR-EXCH-DIFF                      
027500                                                                          
027510       MOVE WC-IDFTG-PV      TO W-IDFTG                                   
027600       MOVE '001'            TO W-KDEKHHT                                 
027700       MOVE '001'            TO W-KDEKSHT                                 
027800       MOVE 'DET'            TO W-KDEKNIVA                                
027900       MOVE 'LEVA1'          TO W-IDSYSMOT                                
028000       MOVE '000'            TO W-IDPTYP                                  
028100                                                                          
028200**   ENDAST DE TRE FÖRSTA TECKNEN I IDINK ÄR ANVÄNDBARA                   
028300       MOVE ZERO             TO W-IDINK                                   
028400       IF IN-IDINK(1:3) NUMERIC                                           
028500         MOVE IN-IDINK(1:3)  TO W-IDINK                                   
028600       ELSE                                                               
028700         IF IN-IDINK(1:2) NUMERIC                                         
028800           MOVE IN-IDINK(1:2) TO W-IDINK                                  
028900         ELSE                                                             
029000           IF IN-IDINK(1:1) NUMERIC                                       
029100            MOVE IN-IDINK(1:1) TO W-IDINK                                 
029200           END-IF                                                         
029300         END-IF                                                           
029400       END-IF                                                             
029500                                                                          
030000       IF W-IDINK = 251                                                   
030100       OR W-IDINK = 253                                                   
030200       OR W-IDINK = 256                                                   
030210       OR W-IDINK = 264                                                   
030220       OR W-IDINK = 274                                                   
030230       OR W-IDINK = 572                                                   
030240       OR W-IDINK = 930                                                   
030250       OR W-IDINK = 931                                                   
030260       OR W-IDINK = 932                                                   
030270       OR W-IDINK = 933                                                   
030280         MOVE 5              TO W-IDSEKVNR                                
030400       ELSE                                                               
030500         MOVE 4              TO W-IDSEKVNR                                
030600       END-IF                                                             
030700       PERFORM IMS-GU-WDH531                                              
030800       MOVE 'O'              TO MR-FLAGGA2D                               
030900       MOVE SYST-IDANALYS    TO MR-COSTCTR-PRICE-DIFF                     
031000     ELSE                                                                 
031001       MOVE SPACE            TO MR-FLAGGA1F                               
031002       MOVE SPACE            TO MR-COSTCTR-FREIGHT                        
031003       MOVE SPACE            TO MR-FLAGGA3VD                              
031004       MOVE SPACE            TO MR-COSTCTR-EXCH-DIFF                      
031005       MOVE SPACE            TO MR-FLAGGA2D                               
031006       MOVE SPACE            TO MR-COSTCTR-PRICE-DIFF                     
031010     END-IF                                                               
031100     .                                                                    
031200     EJECT                                                                
031210                                                                          
031300 C-SKAPA-RESTEN-LEVA1-TRANS SECTION.                                      
031410     MOVE IN-IDDC            TO WS-IDDC                                   
031500     MOVE '500'              TO MR-PTYP                                   
031510     IF IN-IDFTG = 57                                                     
031600       MOVE '244918'         TO MR-HKTO                                   
031610       IF GOOD-DDC                                                        
031620         PERFORM CB-CHECK-DIRECT-DELIVERIES                               
031630       ELSE                                                               
031640         MOVE SPACE          TO MR-UKTO(1:2)                              
031650         MOVE IN-IDDC        TO MR-UKTO(3:2)                              
031660       END-IF                                                             
031900       MOVE 'A10VCP'         TO MR-PRCTR                                  
031901       MOVE W-IDINK          TO MR-BESTPREF                               
031902       MOVE +57              TO MR-FTAG                                   
031903       IF CDC-SE                                                          
031904          MOVE 20            TO MR-SYSTKOD                                
031905       ELSE                                                               
031906          MOVE 21            TO MR-SYSTKOD                                
031907       END-IF                                                             
031908       IF IN-FLLSBOK = 'N'                                                
031909         MOVE IN-IDDISTR     TO MR-GODSMOT                                
031910       ELSE                                                               
031911         IF NDC-JP                                                        
031912***        JAPAN SOM GODSMOTTAGARE                                        
031913           MOVE 15230        TO MR-GODSMOT                                
031914         ELSE                                                             
031915           IF NDC-AU                                                      
031916***   AUSTRALIEN SOM GODSMOTTAGARE                                        
031917             MOVE 07844      TO MR-GODSMOT                                
031918           ELSE                                                           
031919             IF CDC-SE                                                    
031920***   VCCS SOM GODSMOTTAGARE                                              
031921               MOVE 01441    TO MR-GODSMOT                                
031922             ELSE                                                         
031923               IF CDC-TR                                                  
031924***   SUPPLY TERMINAL SOM GODSMOTTAGARE                                   
031925                 MOVE 13426  TO MR-GODSMOT                                
031926               ELSE                                                       
031927***   OKÄNDA GODSMOTTAGARE                                                
031928                 MOVE 0      TO MR-GODSMOT                                
031929               END-IF                                                     
031930             END-IF                                                       
031931           END-IF                                                         
031932         END-IF                                                           
031933       END-IF                                                             
031934       IF IN-KDVALISO = 'XXX'                                             
031935         MOVE 'SEK'          TO IN-KDVALISO                               
031936         MOVE 0              TO MR-URSPPRIS-BEST                          
031937         COMPUTE MR-BEL-STD = 0.1 * IN-KVAVIS                             
031938       ELSE                                                               
031939         COMPUTE MR-BEL-STD = (IN-PRINK - IN-PRHEMTAG) * IN-KVAVIS        
031940       END-IF                                                             
031941                                                                          
031942       PERFORM CA-LAS-AAR-VALUTAKURS                                      
031943       COMPUTE WS-BEL-BEST ROUNDED = IN-PRARTBEL-PR * W-PRKURS            
031944                               / W-REVALUTA                               
031945       COMPUTE MR-BEL-BEST ROUNDED = WS-BEL-BEST                          
031946       COMPUTE MR-BEL-BEST ROUNDED = MR-BEL-BEST * IN-KVAVIS              
031947     ELSE                                                                 
031948       MOVE '288100'         TO MR-HKTO                                   
031949       MOVE '1'              TO MR-UKTO(1:1)                              
031950       MOVE SPACE            TO MR-UKTO(2:1)                              
031951       MOVE IN-IDDC          TO MR-UKTO(3:2)                              
031952       MOVE SPACE            TO MR-PRCTR                                  
031953       IF IN-IDINK(1:3) NUMERIC                                           
031954         MOVE IN-IDINK(1:3)  TO W-IDINK                                   
031955       ELSE                                                               
031956         IF IN-IDINK(1:2) NUMERIC                                         
031957           MOVE IN-IDINK(1:2) TO W-IDINK                                  
031958         ELSE                                                             
031959           IF IN-IDINK(1:1) NUMERIC                                       
031960            MOVE IN-IDINK(1:1) TO W-IDINK                                 
031961           END-IF                                                         
031962         END-IF                                                           
031963       END-IF                                                             
031964       MOVE W-IDINK          TO MR-BESTPREF                               
031965       MOVE IN-IDFTG         TO MR-FTAG                                   
031966       MOVE 21               TO MR-SYSTKOD                                
031967       IF NDC-US                                                          
031970         MOVE 04113          TO MR-GODSMOT                                
031971         IF IN-KDVALISO = 'XXX'                                           
031972           MOVE 'USD'        TO IN-KDVALISO                               
031973           MOVE 0            TO MR-URSPPRIS-BEST                          
031974           COMPUTE MR-BEL-STD = 0.1 * IN-KVAVIS                           
031975         ELSE                                                             
031976           IF IN-KDVALISO = 'USD'                                         
031977             COMPUTE MR-BEL-STD = IN-PRARTBES * IN-KVAVIS                 
031978           ELSE                                                           
031979             PERFORM CC-LAS-USD-VALUTAKURS                                
031980             COMPUTE MR-BEL-STD ROUNDED = IN-PRARTBEL-PR *                
031981                                IN-KVAVIS * W-PRKURS / W-REVALUTA         
031982           END-IF                                                         
031983         END-IF                                                           
031984         COMPUTE MR-BEL-BEST ROUNDED = IN-PRARTBEL-PR * IN-KVAVIS         
031985       END-IF                                                             
031986       IF NDC-CN                                                          
031987**** CHINA SUPPLIER IS NOT NUMMERIC 5 POSITION                            
031988         MOVE 35234          TO MR-GODSMOT                                
031989         IF IN-KDVALISO = 'XXX'                                           
031990           MOVE 'CNY'        TO IN-KDVALISO                               
031991           MOVE 0            TO MR-URSPPRIS-BEST                          
031992           COMPUTE MR-BEL-STD = 0.1 * IN-KVAVIS                           
031993         ELSE                                                             
031994           IF IN-KDVALISO = 'CNY'                                         
031995             COMPUTE MR-BEL-STD = IN-PRARTBES * IN-KVAVIS                 
031996           ELSE                                                           
031997             PERFORM CD-LAS-CNY-VALUTAKURS                                
031998             COMPUTE MR-BEL-STD ROUNDED = IN-PRARTBEL-PR *                
031999                                IN-KVAVIS * W-PRKURS / W-REVALUTA         
032000           END-IF                                                         
032001         END-IF                                                           
032002         COMPUTE MR-BEL-BEST ROUNDED = IN-PRARTBEL-PR * IN-KVAVIS         
032003       END-IF                                                             
032004     END-IF                                                               
032010     MOVE IN-KDPRODSL        TO MR-PRODKOD                                
032100     MOVE IN-IDLOPNRM        TO MR-MRNR                                   
032200     MOVE IN-IDARTNR         TO MR-ARTNR                                  
032300     MOVE IN-IDLEVNR         TO MR-GSDB                                   
032500     MOVE ZERO               TO MR-BESTLOPNR                              
032600                                MR-BESTSUFF                               
032700                                W-IDFS                                    
032800** IN-IDFS ÄR ALFANUMERISKT 8 BYTE OCH VÄNSTERSTÄLLT, BLANKUTFYLLT        
032900** W-IDFS SKALL VARA NUMERISKT 6 BYTE OCH HÖGERSTÄLLT                     
033000** HÄR GÖRS DETTA OCH TAR ÄVEN BORT ALLA BLANKA OCH EJ NUMERISKA          
033100** TECKEN FÖR TECKEN FRÅN HÖGER TILL VÄNSTER                              
033200     MOVE 8                  TO IN-IX                                     
033300     MOVE 8                  TO UT-IX                                     
033400     PERFORM UNTIL IN-IX = 0 OR UT-IX = 0                                 
033500       IF IN-IDFS(IN-IX:1) NUMERIC                                        
033600         MOVE IN-IDFS(IN-IX:1) TO W-IDFS(UT-IX:1)                         
033700         SUBTRACT 1 FROM IN-IX                                            
033800         SUBTRACT 1 FROM UT-IX                                            
033900       ELSE                                                               
034000         SUBTRACT 1 FROM IN-IX                                            
034100       END-IF                                                             
034200     END-PERFORM                                                          
034300***  MR-PACKNR ÄR ENDAST 6 TECKEN LÅNGT                                   
034400     MOVE W-IDFS(3:6)        TO MR-PACKNR                                 
034500     MOVE W-IDFS             TO MR-PACKNRTOT                              
034600                                                                          
034700     MOVE IN-TIAVIDAT        TO MR-DATUM-AVS(2:7)                         
034800     IF MR-DATUM-AVS(3:2) > 50                                            
034900       MOVE 19               TO MR-DATUM-AVS(1:2)                         
035000     ELSE                                                                 
035100       MOVE 20               TO MR-DATUM-AVS(1:2)                         
035200     END-IF                                                               
035300     MOVE IN-KVAVIS          TO MR-ANTAL                                  
035400     EVALUATE IN-KDSORT                                                   
035500       WHEN 'M'                                                           
035600           MOVE 4            TO MR-SORT1                                  
035700       WHEN 'KG'                                                          
035800           MOVE 5            TO MR-SORT1                                  
035900       WHEN 'M2'                                                          
036000           MOVE 6            TO MR-SORT1                                  
036100       WHEN 'L'                                                           
036200           MOVE 9            TO MR-SORT1                                  
036300       WHEN OTHER                                                         
036400           MOVE 1            TO MR-SORT1                                  
036500     END-EVALUATE                                                         
036600                                                                          
036700     MOVE IN-PRARTBES        TO MR-PRIS-BEST                              
036800                                                                          
036900     MOVE IN-PRARTBEL-PR     TO WS-IN-PRARTBEL-PR                         
037000     IF IN-KDVALISO = 'GBP'                                               
037100       COMPUTE WS-IN-PRARTBEL-PR = 10 * IN-PRARTBEL-PR                    
037200***  LEVA1 TOLKAR GBP-PRISET SOM TIO GÅNGER STÖRRE                        
037300     END-IF                                                               
037400                                                                          
037500*** W-PRARTBEL-NUM ÄR ENDAST FÖR ATT TESTA AV ANTALET RELE-               
037600*** VANTA DECIMALER. URSPRIS-BEST I LEVA1-POSTEN ÄR HELTALS-              
037700*** FÄLT MEN TOLKAS I LEVA1 SOM OM DEN HAR TVÅ DECIMALER                  
037800                                                                          
037900     COMPUTE  W-PRARTBEL-NUM = 100000 * WS-IN-PRARTBEL-PR                 
038000     IF W-PRARTBEL-NUM(11:3) = 000                                        
038100       COMPUTE MR-URSPPRIS-BEST = 100 * WS-IN-PRARTBEL-PR                 
038200       MOVE '1'              TO MR-ENHET-PRIS                             
038300***  HÄR FINNS ENDAST TVÅ RELEVANTA DECIMALER                             
038400***  SORT ('1') ÄR PRIS PER STYCK                                         
038500     ELSE                                                                 
038600       IF W-PRARTBEL-NUM(13:1) = 0                                        
038700         COMPUTE MR-URSPPRIS-BEST = 10000 * WS-IN-PRARTBEL-PR             
038800         MOVE '2'            TO MR-ENHET-PRIS                             
038900***  HÄR FINNS FYRA RELEVANTA DECIMALER                                   
039000***  SORT ('2') ÄR PRIS PER 100 STYCK                                     
039100       ELSE                                                               
039200         COMPUTE MR-URSPPRIS-BEST = 100000 * WS-IN-PRARTBEL-PR            
039300         MOVE '3'            TO MR-ENHET-PRIS                             
039400***  HÄR FINNS FEM RELEVANTA DECIMALER                                    
039500***  SORT ('3') ÄR PRIS PER 1000 STYCK                                    
039600       END-IF                                                             
039700     END-IF                                                               
039800                                                                          
039900     IF MR-URSPPRIS-BEST = +0                                             
040000       MOVE 1                TO MR-URSPPRIS-BEST                          
040100     END-IF                                                               
040200                                                                          
040300     IF IN-KDTIPPR = 1 OR 3                                               
040400       MOVE '0'              TO MR-TIPPAT                                 
040500     ELSE                                                                 
040600       MOVE '1'              TO MR-TIPPAT                                 
040700     END-IF                                                               
040800                                                                          
042800                                                                          
043100     MOVE IN-KDVALISO        TO MR-KDVALISO                               
043700                                                                          
043800     MOVE ZERO               TO MR-TULLKURS                               
043900     MOVE +1                 TO MR-TULLFAKT                               
044000                                                                          
046700     MOVE ZERO               TO MR-LOPNRSEKVFROM1                         
046800                                MR-LOPNRSEKVTOM1                          
046900                                MR-LOPNRSEKVFROM2                         
047000                                MR-LOPNRSEKVTOM2                          
047100     MOVE IN-FLAVVINL        TO MR-AVVIKELSE                              
047200     MOVE IN-KDINLAVV        TO MR-RATTKOD                                
047300                                                                          
047400     IF MR-ANTAL NOT = 0                                                  
047500       PERFORM S02-SKRIV-W51080                                           
047600     END-IF                                                               
047700     .                                                                    
047800     EJECT                                                                
047810                                                                          
047900 CA-LAS-AAR-VALUTAKURS SECTION.                                           
048100     MOVE IN-KDVALISO        TO CURR-KDVALISO-ROW                         
048110     MOVE 'SEK'              TO CURR-KDVALISO-HUV                         
048200     MOVE FUNCTION CURRENT-DATE(3:2) TO W-DATE-AAMM(1:2)                  
048210     MOVE 01                         TO W-DATE-AAMM(3:2)                  
048300     MOVE W-DATE-AAMM         TO CURR-TIAAMM                              
048310     MOVE 'A'                 TO CURR-KDVALTYP                            
048320     CALL W510CURR USING CURR-W510CURR 9305-PCB                           
048330     IF CURR-KDSVAR = ' '                                                 
048500       MOVE CURR-PRKURS-NEW  TO W-PRKURS                                  
048600       MOVE CURR-REVALUTA-TO TO W-REVALUTA                                
048700     ELSE                                                                 
048800       MOVE +1               TO W-PRKURS                                  
048900                                W-REVALUTA                                
049000     END-IF                                                               
049100     .                                                                    
049200     EJECT                                                                
049201                                                                          
049210 CB-CHECK-DIRECT-DELIVERIES SECTION.                                      
049220     MOVE NEJ                TO WS-FOUND-SW                               
049230     MOVE IN-IDARTNR         TO W-IDARTNR                                 
049240     PERFORM IMS-GU-WDL201                                                
049250     IF SEGMENT-FINNS                                                     
049260       PERFORM IMS-GNP-WDL211                                             
049270       PERFORM UNTIL SEGMENT-SAKNAS OR WS-FOUND                           
049280         IF SEGMENT-FINNS                                                 
049290           MOVE INL-DAINLEV TO W-DAINLEV                                  
049291           PERFORM IMS-GNP-WDL222                                         
049292           IF SEGMENT-FINNS                                               
049293             MOVE DIR-IDDISTR        TO W-IDDISTR                         
049294             MOVE DIR-IDKUNDNR       TO W-IDKUNDNR                        
049295             IF W-IDDISTR = IN-IDDISTR                                    
049296             AND DIR-IDLOPNRM = IN-IDLOPNRM                               
049297               MOVE JA               TO WS-FOUND-SW                       
049298               PERFORM IMS-GU-WDB201                                      
049299               IF SEGMENT-FINNS                                           
049300                 MOVE GMT-IDPARTNR   TO W-IDPARTNR                        
049301                 MOVE GMT-IDFTG      TO W-WDB1-IDFTG                      
049302                 PERFORM IMS-GU-WDB101                                    
049303                 IF SEGMENT-FINNS                                         
049304                   MOVE BET-IDLANDX2      TO LANDX2-IDLANDX2              
049305                   IF LANDX2-EU-IDLANDX2                                  
049306                     MOVE IN-IDDC         TO LANDX2-IDLANDX2              
049307                     IF LANDX2-EU-IDLANDX2                                
049308                       IF BET-FLDIRVAT = 'J'                              
049309                         MOVE 'DD'          TO MR-UKTO(1:2)               
049310                         MOVE BET-IDLANDX2  TO MR-UKTO(3:2)               
049311                       ELSE                                               
049312                         MOVE 'DD'          TO MR-UKTO(1:2)               
049313                         MOVE DIR-IDDC      TO MR-UKTO(3:2)               
049314                       END-IF                                             
049315                     ELSE                                                 
049316                       MOVE SPACE           TO MR-UKTO(1:2)               
049317                       MOVE IN-IDDC         TO MR-UKTO(3:2)               
049318                     END-IF                                               
049319                   ELSE                                                   
049320                     MOVE SPACE           TO MR-UKTO(1:2)                 
049321                     MOVE IN-IDDC         TO MR-UKTO(3:2)                 
049322                   END-IF                                                 
049323                 ELSE                                                     
049324                   CONTINUE                                               
049325                 END-IF                                                   
049326               ELSE                                                       
049327                 CONTINUE                                                 
049328               END-IF                                                     
049329             ELSE                                                         
049330               CONTINUE                                                   
049331             END-IF                                                       
049332           ELSE                                                           
049333             CONTINUE                                                     
049334           END-IF                                                         
049335         ELSE                                                             
049336           CONTINUE                                                       
049337         END-IF                                                           
049338         PERFORM IMS-GNP-WDL211                                           
049339       END-PERFORM                                                        
049340     ELSE                                                                 
049341       CONTINUE                                                           
049342     END-IF                                                               
049343     .                                                                    
049344     EJECT                                                                
049345                                                                          
049346 CC-LAS-USD-VALUTAKURS SECTION.                                           
049347**** MONTHLY RATE SHOULD BE USED FROM THE DATE THE BINNING IS DONE        
049348     MOVE IN-KDVALISO        TO CURR-KDVALISO-ROW                         
049349     MOVE 'USD'              TO CURR-KDVALISO-HUV                         
049350     MOVE FUNCTION CURRENT-DATE(3:2) TO W-DATE-AAMM(1:2)                  
049351     MOVE FUNCTION CURRENT-DATE(5:2) TO W-DATE-AAMM(3:2)                  
049352     MOVE W-DATE-AAMM         TO CURR-TIAAMM                              
049353     MOVE 'M'                 TO CURR-KDVALTYP                            
049354     CALL W510CURR USING CURR-W510CURR 9305-PCB                           
049355     IF CURR-KDSVAR = ' '                                                 
049356       MOVE CURR-PRKURS-NEW  TO W-PRKURS                                  
049357       MOVE CURR-REVALUTA-TO TO W-REVALUTA                                
049358     ELSE                                                                 
049359       MOVE +1               TO W-PRKURS                                  
049360                                W-REVALUTA                                
049361     END-IF                                                               
049362     .                                                                    
049363     EJECT                                                                
049364                                                                          
049365 CD-LAS-CNY-VALUTAKURS SECTION.                                           
049366**** MONTHLY RATE SHOULD BE USED FROM THE DATE THE BINNING IS DONE        
049367     MOVE IN-KDVALISO        TO CURR-KDVALISO-ROW                         
049368     MOVE 'CNY'              TO CURR-KDVALISO-HUV                         
049369     MOVE FUNCTION CURRENT-DATE(3:2) TO W-DATE-AAMM(1:2)                  
049370     MOVE FUNCTION CURRENT-DATE(5:2) TO W-DATE-AAMM(3:2)                  
049371     MOVE W-DATE-AAMM         TO CURR-TIAAMM                              
049372     MOVE 'M'                 TO CURR-KDVALTYP                            
049380     CALL W510CURR USING CURR-W510CURR 9305-PCB                           
049390     IF CURR-KDSVAR = ' '                                                 
049400       MOVE CURR-PRKURS-NEW  TO W-PRKURS                                  
049410       MOVE CURR-REVALUTA-TO TO W-REVALUTA                                
049420     ELSE                                                                 
049421       MOVE +1               TO W-PRKURS                                  
049422                                W-REVALUTA                                
049423     END-IF                                                               
049424     .                                                                    
049425     EJECT                                                                
049426                                                                          
049427 D-SKAPA-AVST-POST SECTION.                                               
049500     MOVE IN-DAREGDAT        TO AVST-DAREGDAT                             
049600     MOVE IN-TIKLOCK         TO AVST-TIKLOCK                              
049700     MOVE ZERO               TO AVST-DAVERDAT                             
049800     MOVE '001'              TO AVST-KDEKHHT                              
049900     MOVE '001'              TO AVST-KDEKSHT                              
050000     MOVE 'DET'              TO AVST-KDEKNIVA                             
050100     MOVE IN-IDDC            TO AVST-IDDC                                 
050200     MOVE ZERO               TO AVST-IDVERGL                              
050300     MOVE W-IDFS             TO AVST-IDVERGL(3:8)                         
050400     MOVE IN-IDARTNR         TO AVST-IDARTNR                              
050500     MOVE IN-KDPRODSL        TO AVST-KDPRODSL                             
050600     MOVE IN-IDKONTO         TO AVST-IDKONTO                              
050700     MOVE IN-KVAVIS          TO AVST-KVANTAL                              
050800     COMPUTE AVST-SUBEL = IN-KVAVIS * IN-PRINK                            
050900     MOVE IN-FLLSBOK         TO AVST-FLLSBOK                              
051000     MOVE ZERO               TO AVST-PRARTSTD                             
051100     MOVE IN-PRINK           TO AVST-PRINK                                
051200                                                                          
051300     IF AVST-KVANTAL NOT = 0                                              
051400       PERFORM S03-SKRIV-W51081                                           
051500     END-IF                                                               
051600     .                                                                    
051700     EJECT                                                                
051710                                                                          
051800 Z-FINIT SECTION.                                                         
052000     CLOSE W5103A                                                         
052100           W51080                                                         
052200           W51081                                                         
052300                                                                          
052400     MOVE 'S' TO POSTSUM-OPKOD                                            
052500     CALL POSTSUM USING POSTSUM-PARM                                      
052600     .                                                                    
052700     EJECT                                                                
052710                                                                          
052800 S01-LAES-W5103A  SECTION.                                                
053000     READ W5103A INTO IN-AREA                                             
053100     AT END                                                               
053200       MOVE JA           TO W5103A-EOF-SW                                 
053300     NOT AT END                                                           
053400       MOVE 'W5103A'     TO POSTSUM-FDNAMN                                
053500       MOVE 'W51080D1'   TO POSTSUM-DDNAMN2                               
053600       MOVE SPACE        TO POSTSUM-TRANSTYP                              
053700       CALL POSTSUM USING POSTSUM-PARM                                    
053800     END-READ                                                             
053900     .                                                                    
054000     SKIP2                                                                
054010                                                                          
054100 S02-SKRIV-W51080 SECTION.                                                
054300     WRITE LEVA-POST  FROM LEVA-AREA                                      
054400                                                                          
054500     MOVE SPACE       TO POSTSUM-TRANSTYP                                 
054600     MOVE 'W51080'    TO POSTSUM-FDNAMN                                   
054700     MOVE 'W51080D2'  TO POSTSUM-DDNAMN2                                  
054800     CALL POSTSUM USING POSTSUM-PARM                                      
054900     .                                                                    
055000     SKIP2                                                                
055010                                                                          
055100 S03-SKRIV-W51081 SECTION.                                                
055300     WRITE AVST-POST   FROM AVST-AREA                                     
055400                                                                          
055500     MOVE SPACE       TO POSTSUM-TRANSTYP                                 
055600     MOVE 'W51081'    TO POSTSUM-FDNAMN                                   
055700     MOVE 'W51080D3'  TO POSTSUM-DDNAMN2                                  
055800     CALL POSTSUM USING POSTSUM-PARM                                      
055900     .                                                                    
056000     EJECT                                                                
056100* --- IMS SEKTIONER ---                                                   
056200                                                                          
056300 IMS-GU-WDH531 SECTION.                                                   
056500     STRING 'WDH501  (WDH501KY =' W-WDH501KY-X ')'                        
056600          DELIMITED BY SIZE INTO SSA1                                     
056700     STRING 'WDH511  (KDEKSHT  =' W-KDEKSHT-X ')'                         
056800          DELIMITED BY SIZE INTO SSA2                                     
056900     STRING 'WDH521  (KDEKNIVA =' W-KDEKNIVA-X ')'                        
057000          DELIMITED BY SIZE INTO SSA3                                     
057100     STRING 'WDH531  (WDH531KY =' W-WDH531KY-X ')'                        
057200          DELIMITED BY SIZE INTO SSA4                                     
057300     MOVE '  '              TO GODK-STATUSKODER                           
057400     CALL CBLTDLI USING GU  WDH5-PCB DLI-IO-WDH531 SSA1                   
057500                                                   SSA2                   
057600                                                   SSA3                   
057700                                                   SSA4                   
057800     MOVE WDH5-STATUS-CODE  TO STATUS-WS                                  
057900                                                                          
058000     PERFORM IMS-STATUSKONTROLL                                           
058100     .                                                                    
058200     EJECT                                                                
058210                                                                          
058300 IMS-GU-WDL201 SECTION.                                                   
058500     STRING 'WDL201  (IDARTNR  =' W-IDARTNR-X ')'                         
058600          DELIMITED BY SIZE INTO SSA1                                     
058700     MOVE '  GE' TO GODK-STATUSKODER                                      
058800     CALL CBLTDLI USING GU WDL2-PCB DLI-IO-WDL201 SSA1                    
058900     MOVE WDL2-STATUS-CODE TO STATUS-WS                                   
059000     PERFORM IMS-STATUSKONTROLL                                           
059100     .                                                                    
059200     EJECT                                                                
059210                                                                          
059300 IMS-GNP-WDL211         SECTION.                                          
059400     MOVE 'WDL211  ' TO SSA1                                              
059410     MOVE '  GE' TO GODK-STATUSKODER                                      
059420     CALL CBLTDLI USING GNP WDL2-PCB DLI-IO-WDL211 SSA1                   
059430     MOVE WDL2-STATUS-CODE TO STATUS-WS                                   
059440     PERFORM IMS-STATUSKONTROLL                                           
059450     .                                                                    
059451                                                                          
059460 IMS-GNP-WDL222         SECTION.                                          
059470     STRING 'WDL211  (DAINLEV  =' W-DAINLEV-X ')'                         
059480             DELIMITED BY SIZE INTO SSA1                                  
059490     MOVE 'WDL222  ' TO SSA2                                              
059491                                                                          
059492     MOVE '  GE' TO GODK-STATUSKODER                                      
059493     CALL CBLTDLI USING GNP WDL2-PCB DLI-IO-WDL222 SSA1 SSA2              
059494                                                                          
059495     MOVE WDL2-STATUS-CODE TO STATUS-WS                                   
059496     PERFORM IMS-STATUSKONTROLL                                           
059497     .                                                                    
059498                                                                          
059499 IMS-GU-WDB201 SECTION.                                                   
059501     STRING 'WDB201  (IDGMT    =' W-IDGMT-X ')'                           
059502            DELIMITED BY SIZE INTO SSA1                                   
059503                                                                          
059504     MOVE '  GE' TO GODK-STATUSKODER                                      
059505     CALL CBLTDLI USING GU WDB2-PCB DLI-IO-WDB201 SSA1                    
059506     MOVE WDB2-STATUS-CODE TO STATUS-WS                                   
059507     PERFORM IMS-STATUSKONTROLL                                           
059508     .                                                                    
059509     EJECT                                                                
059510                                                                          
059511 IMS-GU-WDB101 SECTION.                                                   
059512     STRING 'WDB101  (WDB101KY =' W-WDB101KY-X ')'                        
059513          DELIMITED BY SIZE INTO SSA1                                     
059514     MOVE '  GE' TO GODK-STATUSKODER                                      
059515     CALL CBLTDLI USING GU WDB1-PCB DLI-IO-WDB101 SSA1                    
059516     MOVE WDB1-STATUS-CODE TO STATUS-WS                                   
059517     PERFORM IMS-STATUSKONTROLL                                           
059518     .                                                                    
059519     EJECT                                                                
059520                                                                          
059530 IMS-STATUSKONTROLL SECTION.                                              
059700     SET STATUS-IX TO 1                                                   
059800     SEARCH GODK-STATUS AT END CALL FELLOG                                
059900       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS CONTINUE                  
060000     END-SEARCH                                                           
060100     .                                                                    
