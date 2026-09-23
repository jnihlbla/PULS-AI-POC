000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W5156200.                                                
000300 AUTHOR.         HÅKAN BOHLIN.                                            
000400 DATE-WRITTEN.   20170822.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*                                                                         
000800*    FUNKTION:                                                            
000900*        LÄSER IN 1 FIL, POSTER IFRÅN WDR8-PEDALBASEN.                    
001000*                                                                         
001100*                                                                         
001200*        KONTROLLERAR EKONOMISKA HÄNDELSER OCH AVVISAR                    
001300*        FELAKTIGA POSTER                                                 
001400*                                                                         
001500                                                                          
001600     SKIP3                                                                
001700 ENVIRONMENT DIVISION.                                                    
001800     SKIP2                                                                
001900 INPUT-OUTPUT SECTION.                                                    
002000                                                                          
002100 FILE-CONTROL.                                                            
002200     SKIP2                                                                
002300*          --- INFIL1-HÄNDELSEPOSTER                                      
002400     SELECT W51562                     ASSIGN TO W51562D1.                
002500     SKIP2                                                                
002600*          --- UTFIL1-KORREKTAPOSTER                                      
002700     SELECT W51564                     ASSIGN TO W51562D2.                
002800     SKIP2                                                                
002900*          --- UTFIL2-FELPOSTER                                           
003000     SELECT W51565                     ASSIGN TO W51562D3.                
003100     EJECT                                                                
003200 DATA DIVISION.                                                           
003300     SKIP3                                                                
003400 FILE SECTION.                                                            
003500     SKIP3                                                                
003600 FD  W51562                                                               
003700     RECORDING       F                                                    
003800     BLOCK CONTAINS  0.                                                   
003900                                                                          
004000*01  -COPY WDR801      -L.                                                
004100     SKIP3                                                                
004200                                                                          
004300 FD  W51564                                                               
004400     RECORDING       F                                                    
004500     BLOCK CONTAINS  0.                                                   
004600                                                                          
004700*01  POST -COPY WDR801 -PRE  RATT- -L.                                    
004800     SKIP3                                                                
004900                                                                          
005000 FD  W51565                                                               
005100     RECORDING       F                                                    
005200     BLOCK CONTAINS  0.                                                   
005300                                                                          
005400*01  POST -COPY WDR801 -PRE  FEL-  -L.                                    
005500     EJECT                                                                
005600 WORKING-STORAGE SECTION.                                                 
005700                                                                          
005800*    -- CHECKED BY WY2000                                                 
005900 77  IDPGM                       PIC X(8)    VALUE 'W5156200'.            
006000 77  JA                          PIC X       VALUE 'J'.                   
006100 77  NEJ                         PIC X       VALUE 'N'.                   
006200                                                                          
006300 77  W51561-EOF-SW               PIC X       VALUE 'N'.                   
006400     88  END-OF-W51562                       VALUE 'J'.                   
006500                                                                          
006600 77  WS-TOT-AMOUNT-DDI           PIC S9(9)V99  COMP-3 VALUE ZERO.         
006700 77  WS-LINE-AMOUNT-DDI          PIC S9(9)V99  COMP-3 VALUE ZERO.         
006800 77  WS-DIFF-AMOUNT-DDI          PIC S9(9)V99  COMP-3 VALUE ZERO.         
006900 77  WS-IDVERGL                  PIC X(10)   VALUE '          '.          
007000 77  WS-IDKUNDNR                 PIC S9(7)   COMP-3 VALUE ZERO.           
007010 77  W-DATE-AAMM                 PIC 9(4)    VALUE ZERO.                  
007020 77  WS-KDVALISO-HUV             PIC X(3)    VALUE 'SEK'.                 
007100     EJECT                                                                
007200                                                                          
007300 01  FILLER                      PIC X(16)   VALUE 'WWIDFTG '.            
007400*01  -COPY WWIDFTG                                                        
007500     EJECT                                                                
007600                                                                          
007700 01  TEST-IDDISTR                PIC 9(5)    COMP-3.                      
007800*01  FILLER  -COPY WWDIST19   -RED TEST-IDDISTR.                          
008000     EJECT                                                                
008100                                                                          
008200 01  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
008300 01  FILLER REDEFINES DAGENS-DATUM.                                       
008400     03  DAGENS-DATUM-AAR        PIC 9(2).                                
008500     03  DAGENS-DATUM-MAANAD     PIC 9(2).                                
008600     03  DAGENS-DATUM-DAG        PIC 9(2).                                
008700 01  W-AAAAMMDD                  PIC 9(8).                                
008800 01  WS-DAGENS-DATUM             PIC 9(8).                                
008900 01  WS-TIREGDAT-TOT             PIC 9(8) VALUE ZERO.                     
009000 01  FILLER REDEFINES WS-TIREGDAT-TOT.                                    
009100     03  WS-YY                   PIC 9(2).                                
009200     03  WS-TIREGDAT             PIC 9(6).                                
009300                                                                          
009400 01  RETURKOD                    PIC S9(4) COMP SYNC VALUE +0.            
009500 01  SPAR-KDVALISO               PIC X(3)       VALUE SPACE.              
009600 01  SPAR-BEFEL                  PIC X(20)      VALUE SPACE.              
009700 01  WS-KDVALISO                 PIC X(3)       VALUE SPACE.              
010000                                                                          
010100     EJECT                                                                
010200 01  FEL-TEXTER.                                                          
010300     03 W-FEL-1                 PIC X(20)   VALUE                         
010400     'SYSTEM ERROR 1'.                                                    
010500     03 W-FEL-3                 PIC X(20)   VALUE                         
010600     'RECEIVING WAREHOUSE'.                                               
010700     03 W-FEL-4                 PIC X(20)   VALUE                         
010800     'SENDING WAREHOUSE'.                                                 
010900     03 W-FEL-5                 PIC X(20)   VALUE                         
011000     'STOCKUPD MANDATORY'.                                                
011100     03 W-FEL-7                 PIC X(20)   VALUE                         
011200     'REGISTER DATE'.                                                     
011300     03 W-FEL-8                 PIC X(20)   VALUE                         
011400     'VERIFICATION DATE 1'.                                               
011500     03 W-FEL-81                PIC X(20)   VALUE                         
011600     'VERIFICATION DATE 2'.                                               
011700     03 W-FEL-9                 PIC X(20)   VALUE                         
011800     'SUM AMOUNT IS ZERO'.                                                
011900     03 W-FEL-10                PIC X(20)   VALUE                         
012000     'QUANTITY NUMBER 1'.                                                 
012100     03 W-FEL-11                PIC X(20)   VALUE                         
012200     'PARTNUMBER IS ZERO 1'.                                              
012300     03 W-FEL-12                PIC X(20)   VALUE                         
012400     'SUM AMOUNT NOT ZERO'.                                               
012500     03 W-FEL-13                PIC X(20)   VALUE                         
012600     'QUANTITY NUMBER 2'.                                                 
012700     03 W-FEL-14                PIC X(20)   VALUE                         
012800     'PARTNUMBER NOT ZERO'.                                               
012900     03 W-FEL-15                PIC X(20)   VALUE                         
013000     'MAIN EVENT NOT ADDED'.                                              
013100     03 W-FEL-16                PIC X(20)   VALUE                         
013200     'LEVEL NOT ADDED'.                                                   
013300     03 W-FEL-17                PIC X(20)   VALUE                         
013400     'WRONG CURRENCY CODE'.                                               
013500     03 W-FEL-18                PIC X(20)   VALUE                         
013600     'SUB EVENT NOT ADDED'.                                               
013700     03 W-FEL-19                PIC X(20)   VALUE                         
013800     'NO PRODUCT GROUP'.                                                  
013900     03 W-FEL-20                PIC X(20)   VALUE                         
014000     'NO LOC PRODUCT GROUP'.                                              
014100     03 W-FEL-21                PIC X(20)   VALUE                         
014200     'NO NET PRICE'.                                                      
014300     03 W-FEL-22                PIC X(20)   VALUE                         
014400     'NO LANDING COST'.                                                   
014500     03 W-FEL-23                PIC X(20)   VALUE                         
014600     'NO COST OF SALES'.                                                  
014700     03 W-FEL-24                PIC X(20)   VALUE                         
014800     'NO STANDARD PRICE'.                                                 
014900     03 W-FEL-25                PIC X(20)   VALUE                         
015000     'NO PURCHASE PRICE'.                                                 
015100     03 W-FEL-26                PIC X(20)   VALUE                         
015200     'NO SURCHARGE COST'.                                                 
015300     03 W-FEL-27                PIC X(20)   VALUE                         
015400     'NO SURCHARGE PACKING'.                                              
015500     03 W-FEL-28                PIC X(20)   VALUE                         
015600     'NO OVERHEAD SURCHARG'.                                              
015700     03 W-FEL-29                PIC X(20)   VALUE                         
015800     'SYSTEM ERROR 2'.                                                    
015900     03 W-FEL-30                PIC X(20)   VALUE                         
016000     'LEVEL NOT ADDED'.                                                   
016100     03 W-FEL-31                PIC X(20)   VALUE                         
016200     'CLIENT NOT ADDED'.                                                  
016300     03 W-FEL-32                PIC X(20)   VALUE                         
016400     'CURRENCY RATE WRONG'.                                               
016500     03 W-FEL-34                PIC X(20)   VALUE                         
016600     'NET PRICE NE SUM'.                                                  
016700     03 W-FEL-35                PIC X(20)   VALUE                         
016800     'NO TRANSPORT COST'.                                                 
016900     03 W-FEL-36                PIC X(20)   VALUE                         
017000     'PARTNUMBER IS ZERO 2'.                                              
017010     03 W-FEL-37                PIC X(20)   VALUE                         
017020     'NO AVERAGE COST'.                                                   
017100                                                                          
017200                                                                          
017300     EJECT                                                                
017400 01  TRANSAR                      PIC X(4).                               
017500     88 GODK-TRANS                           VALUE '5106' '5108'          
017600                                                   '5109' '5116'          
017700                                                   '5151' '6302'          
017800                                                   '6303' '6309'          
017900                                                   '6193' '6119'          
018000                                                   '6192' '6193'          
018100                                                   '611C' '611D'          
018200                                                   '6115' '6144'          
018300                                                   '4731' '4737'          
018400                                                   '5108' '6203'          
018500                                                   '6117' '6115'          
018600                                                   '6148' '6133'          
018700                                                   '6147'                 
018800                                                   '4738' '6100'.         
018900                                                                          
019000     EJECT                                                                
019100 01  FLLSBOK                     PIC X.                                   
019200     88 GODK-FLLSBOK                        VALUE 'Y' 'N' 'J' ' '.        
019300                                                                          
019400     EJECT                                                                
019500 01  POST-SW                     PIC X      VALUE 'J'.                    
019600     88 POST-OK                             VALUE 'J'.                    
019700     88 POST-FEL                            VALUE 'N'.                    
019800                                                                          
019900     EJECT                                                                
020000 01  DYNAMISKA-SUBPROGRAM.                                                
020100*                                                                         
020200     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
020300     03  DATKORT                 PIC X(8)    VALUE 'DATKORT'.             
020400     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
020500     03  WDATKONV                PIC X(8)    VALUE 'WDATKONV'.            
020600     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
020700     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
020710     03  W510CURR                PIC X(8)    VALUE 'W510CURR'.            
020800     SKIP2                                                                
020900*    --- PARAMETRAR TILL ABEND                                            
021000                                                                          
021100 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
021200 77  RKOD-ABEND-UTAN-DUMP        PIC S9(4)   COMP VALUE +16.              
021300 77  RKOD-ABEND-MED-DUMP         PIC S9(4)   COMP VALUE +1000.            
021400     SKIP2                                                                
021500 01  FELTEXT.                                                             
021600     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
021700     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
021800     EJECT                                                                
021900*    --- PARAMETRAR TILL DATKORT                                          
022000*                                                                         
022100 01  PROGRAM-NAMN                PIC X(6)    VALUE 'W51562'.              
022200     SKIP2                                                                
022300 01  DATUMKORT-ID                PIC X(6)    VALUE 'WDATUM'.              
022400     SKIP2                                                                
022500*01  -COPY WDATKORT                                                       
022600     EJECT                                                                
022700*    --- PARAMETRAR TILL POSTSUM                                          
022800*                                                                         
022900*01  -COPY W0005   -PRE  POSTSUM-                                         
023000     EJECT                                                                
023100*01  -COPY WDATAREA                                                       
023200     EJECT                                                                
023210*01  -COPY W510CURR                                                       
023220     EJECT                                                                
023300 01  FILLER                      PIC X(16)      VALUE 'IMS'.              
023400                                                                          
023500 01  NYCKLAR-TILL-DLI.                                                    
023600     03  W-WDH501KY-X.                                                    
023700         05  W-IDFTG             PIC 9(2)        VALUE ZERO.              
023800         05  W-KDEKHHT           PIC X(3)        VALUE SPACE.             
023900     03  W-KDEKSHT-X.                                                     
024000         05  W-KDEKSHT           PIC X(3)        VALUE SPACE.             
024100     03  W-KDEKNIVA-X.                                                    
024200         05  W-KDEKNIVA          PIC X(5)        VALUE SPACE.             
024400     03  W-IDSYSMOT-X.                                                    
024500         05  W-IDSYSMOT          PIC X(4)        VALUE SPACE.             
024600     03  W-KDSEGKY-X.                                                     
024700         05  W-KDSEGKEY          PIC X           VALUE SPACE.             
025800     03  W-IDDC-B6-X.                                                     
025900         05 W-IDDC-B6            PIC X(2).                                
026000                                                                          
026100 01  STATUS-WS                   PIC XX.                                  
026200     88  SEGMENT-FINNS                      VALUE '  '.                   
026300     88  SEGMENT-FINNS-REDAN                VALUE 'II'.                   
026400     88  SEGMENT-SAKNAS                     VALUE 'GE'.                   
026500     88  SEGMENT-SLUT                       VALUE 'GB'.                   
026600     SKIP2                                                                
026700 01  GODK-STATUSKODER.                                                    
026800     03 GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                 
026900                                                                          
027000 01  SSA1                        PIC X(64).                               
027100 01  SSA2                        PIC X(64).                               
027200 01  SSA3                        PIC X(64).                               
027300                                                                          
027400* ---IMS FUNKTIONSKODER----                                               
027500*01  -COPY W0003                                                          
027600     EJECT                                                                
027700                                                                          
027800*----DLI INPUT OCH OUTPUT AREA ------                                     
027900                                                                          
028000                                                                          
028100 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDH501'.                      
028200 01  DLI-IO-WDH501.                                                       
028300*    03  -COPY WDH501                                                     
028400     EJECT                                                                
028500 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDH511'.                      
028600 01  DLI-IO-WDH511.                                                       
028700*    03  -COPY WDH511                                                     
028800     EJECT                                                                
028900 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDH521'.                      
029000 01  DLI-IO-WDH521.                                                       
029100*    03  -COPY WDH521                                                     
029200     EJECT                                                                
029300 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDH531'.                      
029400 01  DLI-IO-WDH531.                                                       
029500*    03  -COPY WDH531                                                     
029600     EJECT                                                                
029700                                                                          
029800 01  IN1-AREA-START              PIC X(24)   VALUE                        
029900                                 'IN1-AREA-START  '.                      
030000     SKIP2                                                                
030100                                                                          
030200*01  AREA -COPY WDR801   -PRE IN1-                                        
030300*    05   -COPY W510EKHA -PRE IN1- -RED IN1-FIL-WDR801-DATA               
030400     EJECT                                                                
030500 01  RATT-AREA-START             PIC X(24)   VALUE                        
030600                                 'RATT-AREA-START  '.                     
030700     SKIP2                                                                
030800                                                                          
030900*01  AREA -COPY WDR801   -PRE RATT-                                       
031000*    05   -COPY W510EKHA -PRE RATT- -RED RATT-FIL-WDR801-DATA             
031100     EJECT                                                                
031200 01  FEL-AREA-START              PIC X(24)   VALUE                        
031300                                 'FEL-AREA-START  '.                      
031400     SKIP2                                                                
031500                                                                          
031600*01  AREA -COPY WDR801   -PRE FEL-                                        
031700*    05   -COPY W510EKHA -PRE FEL- -RED FEL-FIL-WDR801-DATA               
031800                                                                          
032300 01  FILLER               PIC X(16)   VALUE 'WDB601 AREA'.                
032400 01   DLI-IO-AREA-B601.                                                   
032500*     03  -COPY WDB601                                                    
032600                                                                          
032700     EJECT                                                                
032800 LINKAGE SECTION.                                                         
032900                                                                          
033000*01  -COPY W0008     -PRE WDH5-                                           
033100     05 FILLER              PIC X.                                        
033200                                                                          
033300*01  -COPY W0008     -PRE WDG2-                                           
033400     05 FILLER              PIC X.                                        
033500                                                                          
033600*01  -COPY W0008     -PRE WDB6-                                           
033700     05 FILLER              PIC X.                                        
033800                                                                          
033900 PROCEDURE DIVISION USING   WDH5-PCB WDG2-PCB WDB6-PCB.                   
034000                                                                          
034100 MAIN SECTION.                                                            
034200     ENTRY 'DLITCBL' USING  WDH5-PCB WDG2-PCB WDB6-PCB.                   
034300                                                                          
034400     PERFORM A-INIT                                                       
034500     PERFORM S01-LAES-W51562                                              
034600     PERFORM UNTIL END-OF-W51562                                          
034700       PERFORM B-KONTROLLERA-MED-REGELVERK                                
034800       PERFORM S01-LAES-W51562                                            
034900     END-PERFORM                                                          
035000                                                                          
035100     PERFORM Z-FINIT                                                      
035200                                                                          
035300     MOVE ZERO TO  RETURN-CODE                                            
035400     GOBACK                                                               
035500     .                                                                    
035600     EJECT                                                                
035700                                                                          
035800 A-INIT SECTION.                                                          
035900     OPEN INPUT  W51562                                                   
036000                                                                          
036100     OPEN OUTPUT W51564                                                   
036200                 W51565                                                   
036300     SKIP2                                                                
036400     CALL DATKORT USING PROGRAM-NAMN DATUMKORT-ID DATUMKORT               
036500     MOVE D-AAR     TO  DAGENS-DATUM-AAR                                  
036510                        W-DATE-AAMM(1:2)                                  
036600     MOVE D-MAANAD  TO  DAGENS-DATUM-MAANAD                               
036610                        W-DATE-AAMM(3:2)                                  
036700     MOVE D-DAG     TO  DAGENS-DATUM-DAG                                  
036800     MOVE FUNCTION CURRENT-DATE(1:8) TO WS-DAGENS-DATUM                   
036900                                                                          
037000     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
037100     .                                                                    
037200     EJECT                                                                
037300                                                                          
037400 B-KONTROLLERA-MED-REGELVERK SECTION.                                     
037500     IF IN1-EKH-IDVERGL NOT = WS-IDVERGL                                  
037600       MOVE IN1-EKH-IDVERGL  TO WS-IDVERGL                                
037700     END-IF                                                               
037800                                                                          
037900**** FIX PROBLEM MED SAMFAKTURERING DÄR KUNDNR 0 ANVÄNDS FÖR              
038000**** SUMMA OCH TILLÄGG                                                    
038100     IF  IN1-EKH-KDEKHHT  = '102'                                         
038200     AND IN1-EKH-KDEKSHT  = '120'                                         
038300     AND IN1-EKH-KDEKNIVA = 'DET'                                         
038400       MOVE IN1-EKH-IDKUNDNR TO WS-IDKUNDNR                               
038500     END-IF                                                               
038600     IF  IN1-EKH-KDEKHHT  = '102'                                         
038700     AND IN1-EKH-KDEKSHT  = '120'                                         
038800     AND IN1-EKH-KDEKNIVA = 'SUM'                                         
038900       MOVE WS-IDKUNDNR TO IN1-EKH-IDKUNDNR                               
039000     END-IF                                                               
039100****                                                                      
039200                                                                          
039300     MOVE JA TO POST-SW                                                   
039400     MOVE SPACE TO SPAR-BEFEL                                             
039500     MOVE IN1-EKH-KDEKHHT  TO W-KDEKHHT                                   
039600     MOVE IN1-EKH-KDEKSHT  TO W-KDEKSHT                                   
039700     MOVE IN1-EKH-KDEKNIVA TO W-KDEKNIVA                                  
039810     MOVE WC-IDFTG-IN      TO W-IDFTG                                     
039900     PERFORM IMS-GET-WDH5-ALL                                             
040000     IF SEGMENT-FINNS                                                     
040100       IF NIVA-FLPRODSL = 'Y' AND IN1-EKH-KDPRODSL NOT > 0                
040200         MOVE W-FEL-19     TO SPAR-BEFEL                                  
040300         MOVE NEJ TO POST-SW                                              
040400       END-IF                                                             
040500       IF NIVA-FLPSLLOC = 'Y' AND POST-OK                                 
040600         IF IN1-EKH-KDPSLLOC NOT > 0                                      
040700           MOVE W-FEL-20   TO SPAR-BEFEL                                  
040800           MOVE NEJ        TO POST-SW                                     
040900         END-IF                                                           
041000       END-IF                                                             
041100* INGA PRIS KONTROLLER PÅ HHT=501 SHT=501, NOLL KAN FÖREKOMMA I           
041200* PRISFÄLTEN                                                              
041400       IF (IN1-EKH-KDEKHHT = '204' AND IN1-EKH-KDEKSHT = '201')           
041500       OR (IN1-EKH-KDEKHHT = '204' AND IN1-EKH-KDEKSHT = '301')           
041700           CONTINUE                                                       
041800       ELSE                                                               
041900         IF NIVA-FLARTNTO = 'Y' AND POST-OK                               
042000           IF IN1-EKH-PRARTNTO NOT > 0                                    
042100             MOVE W-FEL-21   TO SPAR-BEFEL                                
042200             MOVE NEJ        TO POST-SW                                   
042300           END-IF                                                         
042400         END-IF                                                           
042500         IF NIVA-FLARTSJK = 'Y' AND POST-OK                               
042600           IF IN1-EKH-PRARTSJK = 0                                        
042700             MOVE W-FEL-23   TO SPAR-BEFEL                                
042800             MOVE NEJ        TO POST-SW                                   
042900           END-IF                                                         
043000         END-IF                                                           
043100         IF NIVA-FLARTSTD = 'Y' AND POST-OK                               
043200           IF IN1-EKH-PRARTSTD  = 0                                       
043300             MOVE W-FEL-24   TO SPAR-BEFEL                                
043400             MOVE NEJ        TO POST-SW                                   
043500           END-IF                                                         
043600         END-IF                                                           
043700       END-IF                                                             
043800       IF NIVA-FLAVCOST = 'Y' AND POST-OK                                 
043900         IF IN1-EKH-PRLANDCO NOT > 0                                      
044000           MOVE W-FEL-22   TO SPAR-BEFEL                                  
044100           MOVE NEJ        TO POST-SW                                     
044200         END-IF                                                           
044300       END-IF                                                             
044400       IF NIVA-FLINK    = 'Y' AND POST-OK                                 
044500         IF IN1-EKH-PRINK NOT > 0                                         
044600           MOVE W-FEL-25   TO SPAR-BEFEL                                  
044700           MOVE NEJ        TO POST-SW                                     
044800         END-IF                                                           
044900       END-IF                                                             
045000       IF NIVA-FLDIRLON = 'Y' AND POST-OK                                 
045100         IF IN1-EKH-PRDIRLON NOT > 0                                      
045200           MOVE W-FEL-26   TO SPAR-BEFEL                                  
045300           MOVE NEJ        TO POST-SW                                     
045400         END-IF                                                           
045500       END-IF                                                             
045600       IF NIVA-FLDMTRL  = 'Y' AND POST-OK                                 
045700         IF IN1-EKH-PRDMTRL NOT > 0                                       
045800           MOVE W-FEL-27   TO SPAR-BEFEL                                  
045900           MOVE NEJ        TO POST-SW                                     
046000         END-IF                                                           
046100       END-IF                                                             
046200       IF NIVA-FLOVRPAL = 'Y' AND POST-OK                                 
046300         IF IN1-EKH-PROVRPAL NOT > 0                                      
046400             MOVE W-FEL-28   TO SPAR-BEFEL                                
046500             MOVE NEJ        TO POST-SW                                   
046600         END-IF                                                           
046700       END-IF                                                             
046800       IF NIVA-FLHEMTAG = 'Y' AND POST-OK                                 
046900         IF IN1-EKH-PRHEMTAG NOT > 0                                      
047000             MOVE W-FEL-35   TO SPAR-BEFEL                                
047100             MOVE NEJ        TO POST-SW                                   
047200         END-IF                                                           
047300       END-IF                                                             
047400       IF IN1-EKH-KDEKNIVA = 'DET'                                        
047500         IF IN1-EKH-FLLSBOK = SPACE                                       
047600           IF NIVA-FLLSBOK = 'Y' AND POST-OK                              
047700             MOVE 'Y'      TO IN1-EKH-FLLSBOK                             
047800           END-IF                                                         
047900         END-IF                                                           
048000       END-IF                                                             
048100       IF POST-OK                                                         
048200         PERFORM IMS-GNP-WDH5                                             
048300         IF SEGMENT-SAKNAS                                                
048400           IF W-KDEKNIVA  = 'SUM' OR 'MOMS'                               
048500             CONTINUE                                                     
048600           ELSE                                                           
048700             MOVE W-FEL-30 TO SPAR-BEFEL                                  
048800             MOVE NEJ TO POST-SW                                          
048900           END-IF                                                         
049000         ELSE                                                             
049100           IF SYST-IDSYSMOT = SPACE                                       
049200             MOVE W-FEL-31 TO SPAR-BEFEL                                  
049300             MOVE NEJ TO POST-SW                                          
049400           END-IF                                                         
049500         END-IF                                                           
049600       END-IF                                                             
049700       IF POST-OK                                                         
049800       MOVE IN1-EKH-KDVALISO TO SPAR-KDVALISO                             
049900         MOVE IN1-EKH-KDVALISO TO SPAR-KDVALISO                           
050000         PERFORM S03-KONTROLLERA-KDVALISO                                 
050100         IF WS-KDVALISO = SPACE                                           
050200           MOVE W-FEL-17 TO SPAR-BEFEL                                    
050300         ELSE                                                             
050400           PERFORM BA-KONTROLLERA-POST                                    
050500         END-IF                                                           
050600       END-IF                                                             
050700     ELSE                                                                 
050800       PERFORM IMS-GET-WDH5-HHT                                           
050900       IF SEGMENT-SAKNAS                                                  
051000         MOVE W-FEL-15        TO SPAR-BEFEL                               
051100       ELSE                                                               
051200         PERFORM IMS-GET-WDH5-SHT                                         
051300         IF SEGMENT-SAKNAS                                                
051400           MOVE W-FEL-18      TO SPAR-BEFEL                               
051500         ELSE                                                             
051600           PERFORM IMS-GET-WDH5-NIVA                                      
051700           IF SEGMENT-SAKNAS                                              
051800               MOVE W-FEL-16    TO SPAR-BEFEL                             
051900           END-IF                                                         
052000         END-IF                                                           
052100       END-IF                                                             
052200     END-IF                                                               
052300     IF SPAR-BEFEL NOT = SPACE                                            
052400       PERFORM BC-SKICKA-FELPOST                                          
052500     END-IF                                                               
052600     .                                                                    
052700     EJECT                                                                
052800                                                                          
052900 BA-KONTROLLERA-POST SECTION.                                             
053000     IF IN1-FIL-IDPGM(1:1) NOT = 'W'                                      
053100       MOVE W-FEL-1 TO SPAR-BEFEL                                         
053200     ELSE                                                                 
053300       IF SPAR-BEFEL = SPACE                                              
053400         MOVE IN1-EKH-IDDC-SEND TO W-IDDC-B6                              
053500         PERFORM IMS-GU-WDB601                                            
053600         IF DCS-KDDC = SPACE AND IN1-EKH-IDDC-SEND NOT = SPACE            
053700           MOVE W-FEL-4 TO SPAR-BEFEL                                     
053800         ELSE                                                             
053900           MOVE IN1-EKH-IDDC-REC TO W-IDDC-B6                             
054000           PERFORM IMS-GU-WDB601                                          
054100           IF DCS-KDDC = SPACE AND IN1-EKH-IDDC-REC NOT = SPACE           
054200             MOVE W-FEL-3 TO SPAR-BEFEL                                   
054300           ELSE                                                           
054400             MOVE IN1-EKH-FLLSBOK TO FLLSBOK                              
054500             IF IN1-EKH-FLLSBOK = 'J'                                     
054600               MOVE 'Y' TO IN1-EKH-FLLSBOK                                
054700             END-IF                                                       
054800             IF NOT GODK-FLLSBOK                                          
054900               MOVE W-FEL-5 TO SPAR-BEFEL                                 
055000             ELSE                                                         
055100                 MOVE 'AAMMDD' TO DAT-KDDATFORM                           
055200                 MOVE DAGENS-DATUM TO DAT-I-TIDATUM                       
055300                                                                          
055400                 CALL WDATKONV USING DAT-KDDATFORM                        
055500                                     DAT-I-TIDATUM                        
055600                                     DAT-O-TIDATUM                        
055700                                     DAT-KDSVAR                           
055800                                                                          
055900                 IF DAT-KDSVAR-OK                                         
056000                   MOVE DAT-TIAAMMDD TO W-AAAAMMDD                        
056100                   MOVE DAT-TISEKEL  TO W-AAAAMMDD(1:2)                   
056200                 ELSE                                                     
056300                   MOVE +1000        TO RETURKOD                          
056400                   CALL ABEND USING RETURKOD                              
056500                 END-IF                                                   
056600                 MOVE IN1-FIL-TIREGDAT TO WS-TIREGDAT                     
056700                 MOVE 20               TO WS-YY                           
056800                 IF WS-TIREGDAT-TOT > WS-DAGENS-DATUM                     
056900                   MOVE W-FEL-7 TO SPAR-BEFEL                             
057000                 ELSE                                                     
057100                   IF IN1-EKH-DAVERDAT >  WS-DAGENS-DATUM                 
057200                     MOVE W-FEL-8 TO SPAR-BEFEL                           
057300                   ELSE                                                   
057400                     IF IN1-EKH-DAVERDAT > WS-TIREGDAT-TOT                
057500                       MOVE W-FEL-81 TO SPAR-BEFEL                        
057600                     ELSE                                                 
057700                       IF IN1-EKH-KDEKNIVA NOT = 'DET'                    
057800                         IF IN1-EKH-SUBEL = ZERO                          
057900* SUMMABELOPP FÅR VARA NOLL NÄR DET ÄR EN 404-401 POST, SKROT             
058000                          IF IN1-EKH-KDEKHHT = '404' AND                  
058100                             IN1-EKH-KDEKSHT = '401'                      
058200                            CONTINUE                                      
058300                          ELSE                                            
058800                            MOVE W-FEL-9 TO SPAR-BEFEL                    
059000                          END-IF                                          
059100                         ELSE                                             
059200                           IF IN1-EKH-KVANTAL NOT = ZERO                  
059300                             MOVE W-FEL-10 TO SPAR-BEFEL                  
060910                           END-IF                                         
061000                         END-IF                                           
061100                       ELSE                                               
061200                         IF IN1-EKH-KDEKNIVA = 'DET'                      
061300                           IF IN1-EKH-SUBEL NOT = ZERO                    
061400                             MOVE W-FEL-12 TO SPAR-BEFEL                  
061500                           ELSE                                           
061600                             IF IN1-EKH-IDARTNR = ZERO                    
062100                                MOVE W-FEL-36 TO SPAR-BEFEL               
062300                             ELSE                                         
062310                               IF IN1-EKH-KDEKHHT = '303' AND             
062320                                  IN1-EKH-KDEKSHT = '311'                 
062330                                 IF IN1-EKH-PRARTSTD NOT > 0              
062340                                  MOVE W-FEL-37   TO SPAR-BEFEL           
062350                                 END-IF                                   
062360                               END-IF                                     
062400                               IF IN1-EKH-KDEKHHT(1:1) = '2' OR           
062500                                  IN1-EKH-KDEKHHT = '303'                 
062600                                 CONTINUE                                 
062700                               ELSE                                       
062800                                 IF IN1-EKH-KVANTAL = ZERO                
062900* DET KAN KOMMA POSTER MED NOLL I ANTAL                                   
063000* UNDANTAGET GÄLLER BARA SKROTNING, 404-401 POSTER                        
063100* ALLA ANDRA POSTER BLIR DET EN FELPOST UTAV                              
063200                                   IF IN1-EKH-KDEKHHT = '404' AND         
063300                                      IN1-EKH-KDEKSHT = '401'             
063400                                     CONTINUE                             
063500                                   ELSE                                   
063600                                     MOVE W-FEL-13 TO SPAR-BEFEL          
063700                                   END-IF                                 
063800                                 END-IF                                   
063900                               END-IF                                     
064000                             END-IF                                       
064100                           END-IF                                         
064200                         END-IF                                           
064300                       END-IF                                             
064400                     END-IF                                               
064500                   END-IF                                                 
064600                 END-IF                                                   
064700             END-IF                                                       
064800           END-IF                                                         
064900         END-IF                                                           
065000       END-IF                                                             
065100     END-IF                                                               
065200     IF SPAR-BEFEL = SPACE                                                
065300       IF IN1-EKH-IDDISTR  > 0                                            
065400       OR IN1-EKH-IDKUNDNR > 0                                            
065500       OR IN1-EKH-IDVERGL  > 0                                            
065600         CONTINUE                                                         
065700       ELSE                                                               
065800         MOVE W-FEL-29 TO SPAR-BEFEL                                      
065900       END-IF                                                             
066000       IF IN1-EKH-PRKURS = 0 AND IN1-EKH-KDEKNIVA = 'DET'                 
066100         MOVE W-FEL-32 TO SPAR-BEFEL                                      
066200       END-IF                                                             
066300     END-IF                                                               
066400                                                                          
066500     IF SPAR-BEFEL = SPACE                                                
066600       IF IN1-FIL-IDPGM = 'W4183000' OR 'W4263400' OR 'W4263500'          
066700       OR 'W5402000' OR 'W4183C00'                                        
066800* SYSTEM W426-KRF, W54020, W41830 M. FL. SKALL GÅ DEN GAMLA VÄGEN         
066900         PERFORM BD-SKICKA-RATT-POST                                      
067000       ELSE                                                               
067400* HÄR GENERERAS KURSDIFF-POSTER FÖR DEALER-NET/DDI MARKNADER              
067500         IF IN1-EKH-KDEKNIVA = 'DET'                                      
067600           IF  IN1-EKH-PRARTNTO = ZERO                                    
067700           AND IN1-EKH-PRARTSTD = ZERO                                    
067800             PERFORM BC-SKICKA-FELPOST                                    
067900           ELSE                                                           
068000             PERFORM BD-SKICKA-RATT-POST                                  
068100           END-IF                                                         
068200         ELSE                                                             
068300           IF IN1-EKH-KDEKNIVA = 'SUM'                                    
068400             PERFORM BD-SKICKA-RATT-POST                                  
068500           ELSE                                                           
068600             IF IN1-EKH-SUBEL = ZERO                                      
068700               CONTINUE                                                   
068800             ELSE                                                         
068900               PERFORM BD-SKICKA-RATT-POST                                
069000             END-IF                                                       
069100           END-IF                                                         
069200         END-IF                                                           
069300       END-IF                                                             
069400                                                                          
069500     ELSE                                                                 
069600       PERFORM BC-SKICKA-FELPOST                                          
069700     END-IF                                                               
069800     .                                                                    
069900     EJECT                                                                
070000                                                                          
070100 BC-SKICKA-FELPOST SECTION.                                               
070200     IF IN1-EKH-KDEKHHT = '2??' AND IN1-EKH-KDEKSHT = '2??'               
070300       CONTINUE                                                           
070400     ELSE                                                                 
070500       MOVE IN1-AREA TO FEL-AREA                                          
070600       MOVE SPAR-BEFEL       TO FEL-EKH-BEFELSAP                          
070700       MOVE 'W515EKFA'       TO FEL-FIL-IDCPYTXT                          
070800       PERFORM S12-SKRIV-FEL-POST                                         
070900     END-IF                                                               
071000                                                                          
071100     .                                                                    
071200     EJECT                                                                
071300                                                                          
071400 BD-SKICKA-RATT-POST SECTION.                                             
071500     MOVE IN1-AREA TO RATT-AREA                                           
071600     PERFORM S11-SKRIV-RATT-POST                                          
071700     .                                                                    
071800     EJECT                                                                
071900                                                                          
072000 Z-FINIT SECTION.                                                         
072100     CLOSE W51562                                                         
072200                                                                          
072300           W51564                                                         
072400           W51565                                                         
072500     SKIP2                                                                
072600     MOVE 'S' TO POSTSUM-OPKOD                                            
072700     CALL POSTSUM USING POSTSUM-PARM                                      
072800     .                                                                    
072900     EJECT                                                                
073000                                                                          
073100 S01-LAES-W51562  SECTION.                                                
073200     READ W51562 INTO IN1-AREA                                            
073300     AT END                                                               
073400        MOVE HIGH-VALUE TO IN1-AREA                                       
073500        SET END-OF-W51562 TO TRUE                                         
073600                                                                          
073700     NOT AT END                                                           
073800        MOVE 'W51562' TO POSTSUM-FDNAMN                                   
073900        MOVE 'W51562D1' TO POSTSUM-DDNAMN2                                
074000        MOVE 'INPOST'   TO POSTSUM-TRANSTYP                               
074100        CALL POSTSUM USING POSTSUM-PARM                                   
074200     END-READ                                                             
074300     .                                                                    
074400     EJECT                                                                
074500                                                                          
074600 S03-KONTROLLERA-KDVALISO SECTION.                                        
074700     MOVE SPAR-KDVALISO            TO CURR-KDVALISO-ROW                   
074900                                                                          
075000     MOVE W-DATE-AAMM              TO CURR-TIAAMM                         
075010     MOVE WS-KDVALISO-HUV          TO CURR-KDVALISO-HUV                   
075030     MOVE 'M'                      TO CURR-KDVALTYP                       
075040                                                                          
075050     CALL W510CURR USING CURR-W510CURR WDG2-PCB                           
075060     IF CURR-KDSVAR = ' '                                                 
075200       MOVE SPAR-KDVALISO          TO WS-KDVALISO                         
075400     ELSE                                                                 
075500       MOVE SPACE                  TO WS-KDVALISO                         
075600     END-IF                                                               
075700                                                                          
075800     .                                                                    
075900     EJECT                                                                
076000                                                                          
076100 S11-SKRIV-RATT-POST SECTION.                                             
076200     WRITE RATT-POST FROM RATT-AREA                                       
076300                                                                          
076400     MOVE 'GODK-POST' TO POSTSUM-TRANSTYP                                 
076500     MOVE 'W51564' TO POSTSUM-FDNAMN                                      
076600     MOVE 'W51562D2' TO POSTSUM-DDNAMN2                                   
076700     CALL POSTSUM USING POSTSUM-PARM                                      
076800     .                                                                    
076900     EJECT                                                                
077000                                                                          
077100 S12-SKRIV-FEL-POST SECTION.                                              
077200     WRITE FEL-POST FROM FEL-AREA                                         
077300                                                                          
077400     MOVE 'FEL-POST' TO POSTSUM-TRANSTYP                                  
077500     MOVE 'W51565' TO POSTSUM-FDNAMN                                      
077600     MOVE 'W51565D5' TO POSTSUM-DDNAMN2                                   
077700     CALL POSTSUM USING POSTSUM-PARM                                      
077800     MOVE SPACE TO SPAR-BEFEL                                             
077900     .                                                                    
078000     EJECT                                                                
078100                                                                          
078200 IMS-GET-WDH5-HHT SECTION.                                                
078300     STRING 'WDH501  (WDH501KY =' W-WDH501KY-X ')'                        
078400          DELIMITED BY SIZE INTO SSA1                                     
078500     MOVE '  GE' TO GODK-STATUSKODER                                      
078600     CALL CBLTDLI USING GU WDH5-PCB DLI-IO-WDH501 SSA1                    
078700     MOVE WDH5-STATUS-CODE TO STATUS-WS                                   
078800     PERFORM IMS-STATUSKONTROLL                                           
078900     .                                                                    
079000     EJECT                                                                
079100                                                                          
079200 IMS-GET-WDH5-SHT SECTION.                                                
079300     STRING 'WDH511  (KDEKSHT  =' W-KDEKSHT-X ')'                         
079400          DELIMITED BY SIZE INTO SSA1                                     
079500     MOVE '  GE' TO GODK-STATUSKODER                                      
079600     CALL CBLTDLI USING GNP WDH5-PCB DLI-IO-WDH511 SSA1                   
079700     MOVE WDH5-STATUS-CODE TO STATUS-WS                                   
079800     PERFORM IMS-STATUSKONTROLL                                           
079900     .                                                                    
080000     EJECT                                                                
080100                                                                          
080200 IMS-GET-WDH5-NIVA SECTION.                                               
080300     STRING 'WDH521  (KDEKNIVA =' W-KDEKNIVA-X ')'                        
080400          DELIMITED BY SIZE INTO SSA1                                     
080500     MOVE '  GE' TO GODK-STATUSKODER                                      
080600     CALL CBLTDLI USING GNP WDH5-PCB DLI-IO-WDH521 SSA1                   
080700     MOVE WDH5-STATUS-CODE TO STATUS-WS                                   
080800     PERFORM IMS-STATUSKONTROLL                                           
080900     .                                                                    
081000     EJECT                                                                
081100 IMS-GET-WDH5-ALL SECTION.                                                
081200                                                                          
081300     STRING 'WDH501  (WDH501KY =' W-WDH501KY-X ')'                        
081400          DELIMITED BY SIZE INTO SSA1                                     
081500     STRING 'WDH511  (KDEKSHT  =' W-KDEKSHT-X ')'                         
081600          DELIMITED BY SIZE INTO SSA2                                     
081700     STRING 'WDH521  (KDEKNIVA =' W-KDEKNIVA-X ')'                        
081800          DELIMITED BY SIZE INTO SSA3                                     
081900     MOVE '  GE' TO GODK-STATUSKODER                                      
082000     CALL CBLTDLI USING GU WDH5-PCB DLI-IO-WDH521 SSA1 SSA2 SSA3          
082100     MOVE WDH5-STATUS-CODE TO STATUS-WS                                   
082200     PERFORM IMS-STATUSKONTROLL                                           
082300     .                                                                    
082400     EJECT                                                                
082500                                                                          
082600 IMS-GNP-WDH5      SECTION.                                               
082700     STRING 'WDH531   '                                                   
082800          DELIMITED BY SIZE INTO SSA1                                     
082900     MOVE '  GE' TO GODK-STATUSKODER                                      
083000     CALL CBLTDLI USING GNP WDH5-PCB DLI-IO-WDH531 SSA1                   
083100     MOVE WDH5-STATUS-CODE TO STATUS-WS                                   
083200     PERFORM IMS-STATUSKONTROLL                                           
083300     .                                                                    
083400     EJECT                                                                
083500                                                                          
084800 IMS-GU-WDB601    SECTION.                                                
084900     STRING 'WDB601  (IDDC     =' W-IDDC-B6-X ')'                         
085000          DELIMITED BY SIZE INTO SSA1                                     
085100     MOVE '  GE' TO GODK-STATUSKODER                                      
085200     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-AREA-B601 SSA1                 
085300     MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
085400     PERFORM IMS-STATUSKONTROLL                                           
085500     IF SEGMENT-SAKNAS                                                    
085600         MOVE SPACE TO DCS-KDDC                                           
085700     END-IF                                                               
085800     .                                                                    
085900                                                                          
086000 IMS-STATUSKONTROLL SECTION.                                              
086100     SET STATUS-IX TO 1                                                   
086200     SEARCH GODK-STATUS                                                   
086300       AT END CALL FELLOG                                                 
086400       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS CONTINUE                  
086500     END-SEARCH                                                           
086600     .                                                                    
