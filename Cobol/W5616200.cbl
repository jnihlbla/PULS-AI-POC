000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W5616200.                                                
000300 AUTHOR.         ARCHANA BHAT                                             
000400 DATE-WRITTEN.   20171101                                                 
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
002300                                                                          
002400     SKIP3                                                                
002500 ENVIRONMENT DIVISION.                                                    
002600     SKIP2                                                                
002700 INPUT-OUTPUT SECTION.                                                    
002800                                                                          
002900 FILE-CONTROL.                                                            
003000     SKIP2                                                                
003100*          --- INFIL1-HÄNDELSEPOSTER                                      
003200     SELECT W56162                     ASSIGN TO W56162D1.                
003300     SKIP2                                                                
003400*          --- UTFIL1-KORREKTAPOSTER                                      
003500     SELECT W56164                     ASSIGN TO W56162D2.                
003600     SKIP2                                                                
003700*          --- UTFIL2-FELPOSTER                                           
003800     SELECT W56165                     ASSIGN TO W56162D3.                
003900     EJECT                                                                
004000 DATA DIVISION.                                                           
004100     SKIP3                                                                
004200 FILE SECTION.                                                            
004300     SKIP3                                                                
004400 FD  W56162                                                               
004500     RECORDING       F                                                    
004600     BLOCK CONTAINS  0.                                                   
004700                                                                          
004800*01  -COPY WDR801      -L.                                                
004900     SKIP3                                                                
005000                                                                          
005100 FD  W56164                                                               
005200     RECORDING       F                                                    
005300     BLOCK CONTAINS  0.                                                   
005400                                                                          
005500*01  POST -COPY WDR801 -PRE  RATT- -L.                                    
005600     SKIP3                                                                
005700                                                                          
005800 FD  W56165                                                               
005900     RECORDING       F                                                    
006000     BLOCK CONTAINS  0.                                                   
006100                                                                          
006200*01  POST -COPY WDR801 -PRE  FEL-  -L.                                    
006300     EJECT                                                                
006400 WORKING-STORAGE SECTION.                                                 
006500                                                                          
006600*    -- CHECKED BY WY2000                                                 
006700 77  IDPGM                       PIC X(8)    VALUE 'W5616200'.            
006800 77  JA                          PIC X       VALUE 'J'.                   
006900 77  NEJ                         PIC X       VALUE 'N'.                   
007000                                                                          
007100 77  W56161-EOF-SW               PIC X       VALUE 'N'.                   
007200     88  END-OF-W56162                       VALUE 'J'.                   
007300                                                                          
007400 77  WS-TOT-AMOUNT-DDI           PIC S9(9)V99  COMP-3 VALUE ZERO.         
007500 77  WS-LINE-AMOUNT-DDI          PIC S9(9)V99  COMP-3 VALUE ZERO.         
007600 77  WS-DIFF-AMOUNT-DDI          PIC S9(9)V99  COMP-3 VALUE ZERO.         
007700 77  WS-IDVERGL                  PIC X(10)   VALUE '          '.          
007730 77  WS-IDKUNDNR                 PIC S9(7)   COMP-3 VALUE ZERO.           
007740 77  W-DATE-AAMM                 PIC 9(4)    VALUE ZERO.                  
007750 77  WS-KDVALISO-HUV             PIC X(3)    VALUE 'SEK'.                 
007800     EJECT                                                                
007900                                                                          
008000 01  FILLER                      PIC X(16)   VALUE 'WWIDFTG '.            
008100*01  -COPY WWIDFTG                                                        
008200     EJECT                                                                
008300                                                                          
008400 01  TEST-IDDISTR                PIC 9(5)    COMP-3.                      
008500*01  FILLER  -COPY WWDIST19   -RED TEST-IDDISTR.                          
008600*01  FILLER  -COPY WWDIS134   -RED TEST-IDDISTR.                          
008700     EJECT                                                                
008800                                                                          
008900 01  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
009000 01  FILLER REDEFINES DAGENS-DATUM.                                       
009100     03  DAGENS-DATUM-AAR        PIC 9(2).                                
009200     03  DAGENS-DATUM-MAANAD     PIC 9(2).                                
009300     03  DAGENS-DATUM-DAG        PIC 9(2).                                
009400 01  W-AAAAMMDD                  PIC 9(8).                                
009500 01  WS-DAGENS-DATUM             PIC 9(8).                                
009510 01  WS-TIREGDAT-TOT             PIC 9(8) VALUE ZERO.                     
009520 01  FILLER REDEFINES WS-TIREGDAT-TOT.                                    
009530     03  WS-YY                   PIC 9(2).                                
009540     03  WS-TIREGDAT             PIC 9(6).                                
009600                                                                          
009700 01  RETURKOD                    PIC S9(4) COMP SYNC VALUE +0.            
009800 01  SPAR-KDVALISO               PIC X(3)       VALUE SPACE.              
009900 01  SPAR-BEFEL                  PIC X(20)      VALUE SPACE.              
010000 01  WS-KDVALISO                 PIC X(3)       VALUE SPACE.              
010100                                                                          
010200     EJECT                                                                
010300 01  FEL-TEXTER.                                                          
010400     03 W-FEL-1                 PIC X(20)   VALUE                         
010500     'SYSTEM ERROR 1'.                                                    
010800     03 W-FEL-3                 PIC X(20)   VALUE                         
010900     'RECEIVING WAREHOUSE'.                                               
011000     03 W-FEL-4                 PIC X(20)   VALUE                         
011100     'SENDING WAREHOUSE'.                                                 
011200     03 W-FEL-5                 PIC X(20)   VALUE                         
011300     'STOCKUPD MANDATORY'.                                                
011600     03 W-FEL-7                 PIC X(20)   VALUE                         
011700     'REGISTER DATE'.                                                     
011800     03 W-FEL-8                 PIC X(20)   VALUE                         
011900     'VERIFICATION DATE 1'.                                               
012000     03 W-FEL-81                PIC X(20)   VALUE                         
012100     'VERIFICATION DATE 2'.                                               
012200     03 W-FEL-9                 PIC X(20)   VALUE                         
012300     'SUM AMOUNT IS ZERO'.                                                
012400     03 W-FEL-10                PIC X(20)   VALUE                         
012500     'QUANTITY NUMBER 1'.                                                 
012600     03 W-FEL-11                PIC X(20)   VALUE                         
012700     'PARTNUMBER IS ZERO 1'.                                              
012800     03 W-FEL-12                PIC X(20)   VALUE                         
012900     'SUM AMOUNT NOT ZERO'.                                               
013000     03 W-FEL-13                PIC X(20)   VALUE                         
013100     'QUANTITY NUMBER 2'.                                                 
013200     03 W-FEL-14                PIC X(20)   VALUE                         
013300     'PARTNUMBER NOT ZERO'.                                               
013400     03 W-FEL-15                PIC X(20)   VALUE                         
013500     'MAIN EVENT NOT ADDED'.                                              
013600     03 W-FEL-16                PIC X(20)   VALUE                         
013700     'LEVEL NOT ADDED'.                                                   
013800     03 W-FEL-17                PIC X(20)   VALUE                         
013900     'WRONG CURRENCY CODE'.                                               
014000     03 W-FEL-18                PIC X(20)   VALUE                         
014100     'SUB EVENT NOT ADDED'.                                               
014200     03 W-FEL-19                PIC X(20)   VALUE                         
014300     'NO PRODUCT GROUP'.                                                  
014400     03 W-FEL-20                PIC X(20)   VALUE                         
014500     'NO LOC PRODUCT GROUP'.                                              
014600     03 W-FEL-21                PIC X(20)   VALUE                         
014700     'NO NET PRICE'.                                                      
014800     03 W-FEL-22                PIC X(20)   VALUE                         
014900     'NO LANDING COST'.                                                   
015000     03 W-FEL-23                PIC X(20)   VALUE                         
015100     'NO COST OF SALES'.                                                  
015200     03 W-FEL-24                PIC X(20)   VALUE                         
015300     'NO STANDARD PRICE'.                                                 
015400     03 W-FEL-25                PIC X(20)   VALUE                         
015500     'NO PURCHASE PRICE'.                                                 
015600     03 W-FEL-26                PIC X(20)   VALUE                         
015700     'NO SURCHARGE COST'.                                                 
015800     03 W-FEL-27                PIC X(20)   VALUE                         
015900     'NO SURCHARGE PACKING'.                                              
016000     03 W-FEL-28                PIC X(20)   VALUE                         
016100     'NO OVERHEAD SURCHARG'.                                              
016200     03 W-FEL-29                PIC X(20)   VALUE                         
016300     'SYSTEM ERROR 2'.                                                    
016400     03 W-FEL-30                PIC X(20)   VALUE                         
016500     'LEVEL NOT ADDED'.                                                   
016600     03 W-FEL-31                PIC X(20)   VALUE                         
016700     'CLIENT NOT ADDED'.                                                  
016800     03 W-FEL-32                PIC X(20)   VALUE                         
016900     'CURRENCY RATE WRONG'.                                               
017200     03 W-FEL-34                PIC X(20)   VALUE                         
017300     'NET PRICE NE SUM'.                                                  
017400     03 W-FEL-35                PIC X(20)   VALUE                         
017500     'NO TRANSPORT COST'.                                                 
017610     03 W-FEL-36                PIC X(20)   VALUE                         
017620     'PARTNUMBER IS ZERO 2'.                                              
017621     03 W-FEL-37                PIC X(20)   VALUE                         
017622     'NO AVERAGE COST'.                                                   
017630                                                                          
017700                                                                          
017800     EJECT                                                                
017900 01  TRANSAR                      PIC X(4).                               
018000     88 GODK-TRANS                           VALUE '5106' '5108'          
018100                                                   '5109' '5116'          
018200                                                   '5151' '6302'          
018300                                                   '6303' '6309'          
018400                                                   '6193' '6119'          
018500                                                   '6192' '6193'          
018600                                                   '611C' '611D'          
018700                                                   '6115' '6144'          
018800                                                   '4731' '4737'          
018900                                                   '5108' '6203'          
019000                                                   '6117' '6115'          
019100                                                   '6148' '6133'          
019200                                                   '6147'                 
019300                                                   '4738' '6100'.         
019400                                                                          
019500     EJECT                                                                
019600 01  FLLSBOK                     PIC X.                                   
019700     88 GODK-FLLSBOK                        VALUE 'Y' 'N' 'J' ' '.        
019800                                                                          
019900     EJECT                                                                
020000 01  POST-SW                     PIC X      VALUE 'J'.                    
020100     88 POST-OK                             VALUE 'J'.                    
020200     88 POST-FEL                            VALUE 'N'.                    
020300                                                                          
020400     EJECT                                                                
020500 01  DYNAMISKA-SUBPROGRAM.                                                
020600*                                                                         
020700     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
020800     03  DATKORT                 PIC X(8)    VALUE 'DATKORT'.             
020900     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
021000     03  WDATKONV                PIC X(8)    VALUE 'WDATKONV'.            
021100     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
021200     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
021210     03  W510CURR                PIC X(8)    VALUE 'W510CURR'.            
021300     SKIP2                                                                
021400*    --- PARAMETRAR TILL ABEND                                            
021500                                                                          
021600 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
021700 77  RKOD-ABEND-UTAN-DUMP        PIC S9(4)   COMP VALUE +16.              
021800 77  RKOD-ABEND-MED-DUMP         PIC S9(4)   COMP VALUE +1000.            
021900     SKIP2                                                                
022000 01  FELTEXT.                                                             
022100     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
022200     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
022300     EJECT                                                                
022400*    --- PARAMETRAR TILL DATKORT                                          
022500*                                                                         
022600 01  PROGRAM-NAMN                PIC X(6)    VALUE 'W56162'.              
022700     SKIP2                                                                
022800 01  DATUMKORT-ID                PIC X(6)    VALUE 'WDATUM'.              
022900     SKIP2                                                                
023000*01  -COPY WDATKORT                                                       
023100     EJECT                                                                
023110*01  -COPY W510CURR                                                       
023120     EJECT                                                                
023200*    --- PARAMETRAR TILL POSTSUM                                          
023300*                                                                         
023400*01  -COPY W0005   -PRE  POSTSUM-                                         
023500     EJECT                                                                
023600*01  -COPY WDATAREA                                                       
023700     EJECT                                                                
023800 01  FILLER                      PIC X(16)      VALUE 'IMS'.              
023900                                                                          
024000 01  NYCKLAR-TILL-DLI.                                                    
024100     03  W-WDH501KY-X.                                                    
024200         05  W-IDFTG             PIC 9(2)        VALUE ZERO.              
024300         05  W-KDEKHHT           PIC X(3)        VALUE SPACE.             
024400     03  W-KDEKSHT-X.                                                     
024500         05  W-KDEKSHT           PIC X(3)        VALUE SPACE.             
024600     03  W-KDEKNIVA-X.                                                    
024700         05  W-KDEKNIVA          PIC X(5)        VALUE SPACE.             
024900     03  W-IDSYSMOT-X.                                                    
025000         05  W-IDSYSMOT          PIC X(4)        VALUE SPACE.             
025100     03  W-KDSEGKY-X.                                                     
025200         05  W-KDSEGKEY          PIC X           VALUE SPACE.             
026300     03  W-IDDC-B6-X.                                                     
026400         05 W-IDDC-B6            PIC X(2).                                
026500                                                                          
026600 01  STATUS-WS                   PIC XX.                                  
026700     88  SEGMENT-FINNS                      VALUE '  '.                   
026800     88  SEGMENT-FINNS-REDAN                VALUE 'II'.                   
026900     88  SEGMENT-SAKNAS                     VALUE 'GE'.                   
027000     88  SEGMENT-SLUT                       VALUE 'GB'.                   
027100     SKIP2                                                                
027200 01  GODK-STATUSKODER.                                                    
027300     03 GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                 
027400                                                                          
027500 01  SSA1                        PIC X(64).                               
027600 01  SSA2                        PIC X(64).                               
027700 01  SSA3                        PIC X(64).                               
027800                                                                          
027900* ---IMS FUNKTIONSKODER----                                               
028000*01  -COPY W0003                                                          
028100     EJECT                                                                
028200                                                                          
028300*----DLI INPUT OCH OUTPUT AREA ------                                     
028400                                                                          
028500                                                                          
028600 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDH501'.                      
028700 01  DLI-IO-WDH501.                                                       
028800*    03  -COPY WDH501                                                     
028900     EJECT                                                                
029000 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDH511'.                      
029100 01  DLI-IO-WDH511.                                                       
029200*    03  -COPY WDH511                                                     
029300     EJECT                                                                
029400 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDH521'.                      
029500 01  DLI-IO-WDH521.                                                       
029600*    03  -COPY WDH521                                                     
029700     EJECT                                                                
029800 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDH531'.                      
029900 01  DLI-IO-WDH531.                                                       
030000*    03  -COPY WDH531                                                     
030100     EJECT                                                                
030200                                                                          
030300 01  IN1-AREA-START              PIC X(24)   VALUE                        
030400                                 'IN1-AREA-START  '.                      
030500     SKIP2                                                                
030600                                                                          
030700*01  AREA -COPY WDR801   -PRE IN1-                                        
030800*    05   -COPY W510EKHA -PRE IN1- -RED IN1-FIL-WDR801-DATA               
030900     EJECT                                                                
031000 01  RATT-AREA-START             PIC X(24)   VALUE                        
031100                                 'RATT-AREA-START  '.                     
031200     SKIP2                                                                
031300                                                                          
031400*01  AREA -COPY WDR801   -PRE RATT-                                       
031500*    05   -COPY W510EKHA -PRE RATT- -RED RATT-FIL-WDR801-DATA             
031600     EJECT                                                                
031700 01  FEL-AREA-START              PIC X(24)   VALUE                        
031800                                 'FEL-AREA-START  '.                      
031900     SKIP2                                                                
032000                                                                          
032100*01  AREA -COPY WDR801   -PRE FEL-                                        
032200*    05   -COPY W510EKHA -PRE FEL- -RED FEL-FIL-WDR801-DATA               
032300                                                                          
032800 01  FILLER               PIC X(16)   VALUE 'WDB601 AREA'.                
032900 01   DLI-IO-AREA-B601.                                                   
033000*     03  -COPY WDB601                                                    
033100                                                                          
033200     EJECT                                                                
033300 LINKAGE SECTION.                                                         
033400                                                                          
033500*01  -COPY W0008     -PRE WDH5-                                           
033600     05 FILLER              PIC X.                                        
033700                                                                          
033800*01  -COPY W0008     -PRE WDG2-                                           
033900     05 FILLER              PIC X.                                        
034000                                                                          
034100*01  -COPY W0008     -PRE WDB6-                                           
034200     05 FILLER              PIC X.                                        
034300                                                                          
034400 PROCEDURE DIVISION USING   WDH5-PCB WDG2-PCB WDB6-PCB.                   
034500                                                                          
034600 MAIN SECTION.                                                            
034700     ENTRY 'DLITCBL' USING  WDH5-PCB WDG2-PCB WDB6-PCB.                   
034800                                                                          
035100     PERFORM A-INIT                                                       
035200     PERFORM S01-LAES-W56162                                              
035300     PERFORM UNTIL END-OF-W56162                                          
035400       PERFORM B-KONTROLLERA-MED-REGELVERK                                
035500       PERFORM S01-LAES-W56162                                            
035600     END-PERFORM                                                          
035700                                                                          
035800     PERFORM Z-FINIT                                                      
035900                                                                          
036000     MOVE ZERO TO  RETURN-CODE                                            
036100     GOBACK                                                               
036200     .                                                                    
036300     EJECT                                                                
036310                                                                          
036400 A-INIT SECTION.                                                          
036600     OPEN INPUT  W56162                                                   
036700                                                                          
036800     OPEN OUTPUT W56164                                                   
036900                 W56165                                                   
037000     SKIP2                                                                
037100     CALL DATKORT USING PROGRAM-NAMN DATUMKORT-ID DATUMKORT               
037200     MOVE D-AAR     TO  DAGENS-DATUM-AAR                                  
037210                        W-DATE-AAMM(1:2)                                  
037300     MOVE D-MAANAD  TO  DAGENS-DATUM-MAANAD                               
037310                        W-DATE-AAMM(3:2)                                  
037400     MOVE D-DAG     TO  DAGENS-DATUM-DAG                                  
037500     MOVE FUNCTION CURRENT-DATE(1:8) TO WS-DAGENS-DATUM                   
037600                                                                          
037700     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
037800     .                                                                    
037900     EJECT                                                                
037910                                                                          
038000 B-KONTROLLERA-MED-REGELVERK SECTION.                                     
038200     IF IN1-EKH-IDVERGL NOT = WS-IDVERGL                                  
038300       MOVE IN1-EKH-IDVERGL  TO WS-IDVERGL                                
038700     END-IF                                                               
038800                                                                          
038801**** FIX PROBLEM MED SAMFAKTURERING DÄR KUNDNR 0 ANVÄNDS FÖR              
038802**** SUMMA OCH TILLÄGG                                                    
038810     IF  IN1-EKH-KDEKHHT  = '102'                                         
038820     AND IN1-EKH-KDEKSHT  = '120'                                         
038821     AND IN1-EKH-KDEKNIVA = 'DET'                                         
038822       MOVE IN1-EKH-IDKUNDNR TO WS-IDKUNDNR                               
038830     END-IF                                                               
038840     IF  IN1-EKH-KDEKHHT  = '102'                                         
038850     AND IN1-EKH-KDEKSHT  = '120'                                         
038851     AND IN1-EKH-KDEKNIVA = 'SUM'                                         
038860       MOVE WS-IDKUNDNR TO IN1-EKH-IDKUNDNR                               
038870     END-IF                                                               
038871****                                                                      
038880                                                                          
038900     MOVE JA TO POST-SW                                                   
039000     MOVE SPACE TO SPAR-BEFEL                                             
039200     MOVE IN1-EKH-KDEKHHT  TO W-KDEKHHT                                   
039300     MOVE IN1-EKH-KDEKSHT  TO W-KDEKSHT                                   
039400     MOVE IN1-EKH-KDEKNIVA TO W-KDEKNIVA                                  
039410     MOVE WC-IDFTG-US      TO W-IDFTG                                     
039500     PERFORM IMS-GET-WDH5-ALL                                             
039600     IF SEGMENT-FINNS                                                     
039700       IF NIVA-FLPRODSL = 'Y' AND IN1-EKH-KDPRODSL NOT > 0                
039800         MOVE W-FEL-19     TO SPAR-BEFEL                                  
039900         MOVE NEJ TO POST-SW                                              
040000       END-IF                                                             
040100       IF NIVA-FLPSLLOC = 'Y' AND POST-OK                                 
040200         IF IN1-EKH-KDPSLLOC NOT > 0                                      
040300           MOVE W-FEL-20   TO SPAR-BEFEL                                  
040400           MOVE NEJ        TO POST-SW                                     
040500         END-IF                                                           
040600       END-IF                                                             
040700* INGA PRIS KONTROLLER PÅ HHT=501 SHT=501, NOLL KAN FÖREKOMMA I           
040800* PRISFÄLTEN                                                              
041000       IF (IN1-EKH-KDEKHHT = '501' AND IN1-EKH-KDEKSHT = '501')           
041100       OR (IN1-EKH-KDEKHHT = '204' AND IN1-EKH-KDEKSHT = '201')           
041200       OR (IN1-EKH-KDEKHHT = '204' AND IN1-EKH-KDEKSHT = '204')           
041300       OR (IN1-EKH-KDEKHHT = '204' AND IN1-EKH-KDEKSHT = '251')           
041310       OR (IN1-EKH-KDEKHHT = '204' AND IN1-EKH-KDEKSHT = '301')           
041400           CONTINUE                                                       
041500       ELSE                                                               
041600         IF NIVA-FLARTNTO = 'Y' AND POST-OK                               
041700           IF IN1-EKH-PRARTNTO NOT > 0                                    
041800             MOVE W-FEL-21   TO SPAR-BEFEL                                
041900             MOVE NEJ        TO POST-SW                                   
042000           END-IF                                                         
042100         END-IF                                                           
042200         IF NIVA-FLARTSJK = 'Y' AND POST-OK                               
042300           IF IN1-EKH-PRARTSJK = 0                                        
042400             MOVE W-FEL-23   TO SPAR-BEFEL                                
042500             MOVE NEJ        TO POST-SW                                   
042600           END-IF                                                         
042700         END-IF                                                           
042800         IF NIVA-FLARTSTD = 'Y' AND POST-OK                               
042900           IF IN1-EKH-PRARTSTD  = 0                                       
043000             MOVE W-FEL-24   TO SPAR-BEFEL                                
043100             MOVE NEJ        TO POST-SW                                   
043200           END-IF                                                         
043300         END-IF                                                           
043400       END-IF                                                             
043500       IF NIVA-FLAVCOST = 'Y' AND POST-OK                                 
043600         IF IN1-EKH-PRLANDCO NOT > 0                                      
043700           MOVE W-FEL-22   TO SPAR-BEFEL                                  
043800           MOVE NEJ        TO POST-SW                                     
043900         END-IF                                                           
044000       END-IF                                                             
044100       IF NIVA-FLINK    = 'Y' AND POST-OK                                 
044200         IF IN1-EKH-PRINK NOT > 0                                         
044300           MOVE W-FEL-25   TO SPAR-BEFEL                                  
044400           MOVE NEJ        TO POST-SW                                     
044500         END-IF                                                           
044600       END-IF                                                             
044700       IF NIVA-FLDIRLON = 'Y' AND POST-OK                                 
044800         IF IN1-EKH-PRDIRLON NOT > 0                                      
044900           MOVE W-FEL-26   TO SPAR-BEFEL                                  
045000           MOVE NEJ        TO POST-SW                                     
045100         END-IF                                                           
045200       END-IF                                                             
045300       IF NIVA-FLDMTRL  = 'Y' AND POST-OK                                 
045400         IF IN1-EKH-PRDMTRL NOT > 0                                       
045500           MOVE W-FEL-27   TO SPAR-BEFEL                                  
045600           MOVE NEJ        TO POST-SW                                     
045700         END-IF                                                           
045800       END-IF                                                             
045900       IF NIVA-FLOVRPAL = 'Y' AND POST-OK                                 
046000         IF IN1-EKH-PROVRPAL NOT > 0                                      
046100             MOVE W-FEL-28   TO SPAR-BEFEL                                
046200             MOVE NEJ        TO POST-SW                                   
046300         END-IF                                                           
046400       END-IF                                                             
046500       IF NIVA-FLHEMTAG = 'Y' AND POST-OK                                 
046600         IF IN1-EKH-PRHEMTAG NOT > 0                                      
046700             MOVE W-FEL-35   TO SPAR-BEFEL                                
046800             MOVE NEJ        TO POST-SW                                   
046900         END-IF                                                           
047000       END-IF                                                             
047100       IF IN1-EKH-KDEKNIVA = 'DET'                                        
047200         IF IN1-EKH-FLLSBOK = SPACE                                       
047300           IF NIVA-FLLSBOK = 'Y' AND POST-OK                              
047400             MOVE 'Y'      TO IN1-EKH-FLLSBOK                             
047500           END-IF                                                         
047600         END-IF                                                           
047700       END-IF                                                             
048500       IF POST-OK                                                         
048600         PERFORM IMS-GNP-WDH5                                             
048700         IF SEGMENT-SAKNAS                                                
048800           IF W-KDEKNIVA  = 'SUM' OR 'MOMS'                               
048900             CONTINUE                                                     
049000           ELSE                                                           
049100             MOVE W-FEL-30 TO SPAR-BEFEL                                  
049200             MOVE NEJ TO POST-SW                                          
049300           END-IF                                                         
049400         ELSE                                                             
049500           IF SYST-IDSYSMOT = SPACE                                       
049600             MOVE W-FEL-31 TO SPAR-BEFEL                                  
049700             MOVE NEJ TO POST-SW                                          
049800           END-IF                                                         
049900         END-IF                                                           
050000       END-IF                                                             
050100       IF POST-OK                                                         
050200       MOVE IN1-EKH-KDVALISO TO SPAR-KDVALISO                             
051100         MOVE IN1-EKH-KDVALISO TO SPAR-KDVALISO                           
051200         PERFORM S03-KONTROLLERA-KDVALISO                                 
051300         IF WS-KDVALISO = SPACE                                           
051400           MOVE W-FEL-17 TO SPAR-BEFEL                                    
051500         ELSE                                                             
051600           PERFORM BA-KONTROLLERA-POST                                    
051700         END-IF                                                           
051800       END-IF                                                             
051900     ELSE                                                                 
052000       PERFORM IMS-GET-WDH5-HHT                                           
052100       IF SEGMENT-SAKNAS                                                  
052200         MOVE W-FEL-15        TO SPAR-BEFEL                               
052300       ELSE                                                               
052400         PERFORM IMS-GET-WDH5-SHT                                         
052500         IF SEGMENT-SAKNAS                                                
052600           MOVE W-FEL-18      TO SPAR-BEFEL                               
052700         ELSE                                                             
052800           PERFORM IMS-GET-WDH5-NIVA                                      
052900           IF SEGMENT-SAKNAS                                              
053000               MOVE W-FEL-16    TO SPAR-BEFEL                             
053100           END-IF                                                         
053200         END-IF                                                           
053300       END-IF                                                             
053400     END-IF                                                               
053500     IF SPAR-BEFEL NOT = SPACE                                            
053600       PERFORM BC-SKICKA-FELPOST                                          
053700     END-IF                                                               
053800     .                                                                    
053900     EJECT                                                                
053910                                                                          
054000 BA-KONTROLLERA-POST SECTION.                                             
054100     IF IN1-FIL-IDPGM(1:1) NOT = 'W'                                      
054200       MOVE W-FEL-1 TO SPAR-BEFEL                                         
054300     ELSE                                                                 
055100       IF SPAR-BEFEL = SPACE                                              
055200         MOVE IN1-EKH-IDDC-SEND TO W-IDDC-B6                              
055300         PERFORM IMS-GU-WDB601                                            
055400         IF DCS-KDDC = SPACE AND IN1-EKH-IDDC-SEND NOT = SPACE            
055500           MOVE W-FEL-4 TO SPAR-BEFEL                                     
055600         ELSE                                                             
055700           MOVE IN1-EKH-IDDC-REC TO W-IDDC-B6                             
055800           PERFORM IMS-GU-WDB601                                          
055900           IF DCS-KDDC = SPACE AND IN1-EKH-IDDC-REC NOT = SPACE           
056000             MOVE W-FEL-3 TO SPAR-BEFEL                                   
056100           ELSE                                                           
056200             MOVE IN1-EKH-FLLSBOK TO FLLSBOK                              
056300             IF IN1-EKH-FLLSBOK = 'J'                                     
056400               MOVE 'Y' TO IN1-EKH-FLLSBOK                                
056500             END-IF                                                       
056600             IF NOT GODK-FLLSBOK                                          
056700               MOVE W-FEL-5 TO SPAR-BEFEL                                 
056800             ELSE                                                         
057300                 MOVE 'AAMMDD' TO DAT-KDDATFORM                           
057400                 MOVE DAGENS-DATUM TO DAT-I-TIDATUM                       
057500                                                                          
057600                 CALL WDATKONV USING DAT-KDDATFORM                        
057700                                     DAT-I-TIDATUM                        
057800                                     DAT-O-TIDATUM                        
057900                                     DAT-KDSVAR                           
058000                                                                          
058100                 IF DAT-KDSVAR-OK                                         
058200                   MOVE DAT-TIAAMMDD TO W-AAAAMMDD                        
058300                   MOVE DAT-TISEKEL  TO W-AAAAMMDD(1:2)                   
058400                 ELSE                                                     
058500                   MOVE +1000        TO RETURKOD                          
058600                   CALL ABEND USING RETURKOD                              
058700                 END-IF                                                   
058710                 MOVE IN1-FIL-TIREGDAT TO WS-TIREGDAT                     
058720                 MOVE 20               TO WS-YY                           
058810                 IF WS-TIREGDAT-TOT > WS-DAGENS-DATUM                     
058900                   MOVE W-FEL-7 TO SPAR-BEFEL                             
059000                 ELSE                                                     
059100                   IF IN1-EKH-DAVERDAT >  WS-DAGENS-DATUM                 
059200                     MOVE W-FEL-8 TO SPAR-BEFEL                           
059300                   ELSE                                                   
059410                     IF IN1-EKH-DAVERDAT > WS-TIREGDAT-TOT                
059500                       MOVE W-FEL-81 TO SPAR-BEFEL                        
059600                     ELSE                                                 
059700                       IF IN1-EKH-KDEKNIVA NOT = 'DET'                    
059800                         IF IN1-EKH-SUBEL = ZERO                          
059900* SUMMABELOPP FÅR VARA NOLL NÄR DET ÄR EN 404-401 POST, SKROT             
060000                          IF IN1-EKH-KDEKHHT = '404' AND                  
060100                             IN1-EKH-KDEKSHT = '401'                      
060200                            CONTINUE                                      
060300                          ELSE                                            
060400                            IF IN1-EKH-KDEKHHT = '204' AND                
060500                               IN1-EKH-KDEKSHT = '204'                    
060600                              CONTINUE                                    
060700                            ELSE                                          
060800                              MOVE W-FEL-9 TO SPAR-BEFEL                  
060900                            END-IF                                        
061000                          END-IF                                          
061100                         ELSE                                             
061200                           IF IN1-EKH-KVANTAL NOT = ZERO                  
061300                             MOVE W-FEL-10 TO SPAR-BEFEL                  
061400                           ELSE                                           
061500                             IF IN1-EKH-KDEKHHT = '301'                   
061600                               IF IN1-EKH-KDEKSHT = '301' OR              
061700                                             '302' OR '303'               
061800                                 IF IN1-EKH-IDARTNR = ZERO                
061900                                   MOVE W-FEL-11 TO                       
062000                                        SPAR-BEFEL                        
062100                                 END-IF                                   
062200                               ELSE                                       
062300                                 IF IN1-EKH-IDARTNR NOT = ZERO            
062400                                   MOVE W-FEL-14 TO                       
062500                                        SPAR-BEFEL                        
062610                                 END-IF                                   
062700                               END-IF                                     
062800                             END-IF                                       
062900                           END-IF                                         
063000                         END-IF                                           
063100                       ELSE                                               
063200                         IF IN1-EKH-KDEKNIVA = 'DET'                      
063300                           IF IN1-EKH-SUBEL NOT = ZERO                    
063400                             MOVE W-FEL-12 TO SPAR-BEFEL                  
063500                           ELSE                                           
063600                             IF IN1-EKH-IDARTNR = ZERO                    
063700                               IF IN1-EKH-KDEKHHT = '204' AND             
063800                                  IN1-EKH-KDEKSHT = '204'                 
063900                                 CONTINUE                                 
064000                               ELSE                                       
064100                                 MOVE W-FEL-36 TO SPAR-BEFEL              
064200                               END-IF                                     
064300                             ELSE                                         
064310                               IF  IN1-EKH-KDEKHHT = '303'                
064320                               AND IN1-EKH-KDEKSHT = '311'                
064330                                 IF IN1-EKH-PRARTNTO = 0                  
064340                                   MOVE W-FEL-37   TO SPAR-BEFEL          
064350                                 END-IF                                   
064360                               END-IF                                     
064400                               IF IN1-EKH-KDEKHHT(1:1) = '2' OR           
064500                                  IN1-EKH-KDEKHHT = '303'                 
064600                                 CONTINUE                                 
064700                               ELSE                                       
064800                                 IF IN1-EKH-KVANTAL = ZERO                
064900* DET KAN KOMMA POSTER MED NOLL I ANTAL                                   
065000* UNDANTAGET GÄLLER BARA SKROTNING, 404-401 POSTER                        
065100* ALLA ANDRA POSTER BLIR DET EN FELPOST UTAV                              
065200                                   IF IN1-EKH-KDEKHHT = '404' AND         
065300                                      IN1-EKH-KDEKSHT = '401'             
065400                                     CONTINUE                             
065500                                   ELSE                                   
065600                                     MOVE W-FEL-13 TO SPAR-BEFEL          
065700                                   END-IF                                 
065800                                 END-IF                                   
065900                               END-IF                                     
066000                             END-IF                                       
066100                           END-IF                                         
066200                         END-IF                                           
066300                       END-IF                                             
066400                     END-IF                                               
066500                   END-IF                                                 
066600                 END-IF                                                   
066800             END-IF                                                       
066900           END-IF                                                         
067000         END-IF                                                           
067100       END-IF                                                             
067200     END-IF                                                               
067300     IF SPAR-BEFEL = SPACE                                                
067400       IF IN1-EKH-IDDISTR  > 0                                            
067500       OR IN1-EKH-IDKUNDNR > 0                                            
067600       OR IN1-EKH-IDVERGL  > 0                                            
067700         CONTINUE                                                         
067800       ELSE                                                               
067900         MOVE W-FEL-29 TO SPAR-BEFEL                                      
068000       END-IF                                                             
068100       IF IN1-EKH-PRKURS = 0 AND IN1-EKH-KDEKNIVA = 'DET'                 
068200         MOVE W-FEL-32 TO SPAR-BEFEL                                      
068300       END-IF                                                             
068400     END-IF                                                               
068500                                                                          
068600     IF SPAR-BEFEL = SPACE                                                
068800       IF IN1-FIL-IDPGM = 'W4183000' OR 'W4263400' OR 'W4263500'          
068900       OR 'W5402000' OR 'W4183C00'                                        
069000* SYSTEM W426-KRF, W54020, W41830 M. FL. SKALL GÅ DEN GAMLA VÄGEN         
069200         PERFORM BD-SKICKA-RATT-POST                                      
069300       ELSE                                                               
069400* HÄR GENERERAS KURSDIFF-POSTER FÖR DEALER-NET/DDI MARKNADER              
069500         IF IN1-EKH-KDEKNIVA = 'DET'                                      
069510           IF  IN1-EKH-PRARTNTO = ZERO                                    
069511           AND IN1-EKH-PRARTSTD = ZERO                                    
069521             PERFORM BC-SKICKA-FELPOST                                    
069530           ELSE                                                           
070410             PERFORM BD-SKICKA-RATT-POST                                  
070420           END-IF                                                         
070500         ELSE                                                             
070600           IF IN1-EKH-KDEKNIVA = 'SUM'                                    
071300             PERFORM BD-SKICKA-RATT-POST                                  
074100           ELSE                                                           
074110             IF IN1-EKH-SUBEL = ZERO                                      
074120               CONTINUE                                                   
074130             ELSE                                                         
074800               PERFORM BD-SKICKA-RATT-POST                                
074900             END-IF                                                       
074910           END-IF                                                         
075000         END-IF                                                           
075100       END-IF                                                             
075200                                                                          
075300     ELSE                                                                 
075400       PERFORM BC-SKICKA-FELPOST                                          
075500     END-IF                                                               
075600     .                                                                    
075700     EJECT                                                                
075710                                                                          
075800 BC-SKICKA-FELPOST SECTION.                                               
075900     IF IN1-EKH-KDEKHHT = '2??' AND IN1-EKH-KDEKSHT = '2??'               
076000       CONTINUE                                                           
076100     ELSE                                                                 
076200       MOVE IN1-AREA TO FEL-AREA                                          
076300       MOVE SPAR-BEFEL       TO FEL-EKH-BEFELSAP                          
076400       MOVE 'W561EKFA'       TO FEL-FIL-IDCPYTXT                          
076500       PERFORM S12-SKRIV-FEL-POST                                         
076600     END-IF                                                               
076700                                                                          
076800     .                                                                    
076900     EJECT                                                                
076910                                                                          
077000 BD-SKICKA-RATT-POST SECTION.                                             
077100     MOVE IN1-AREA TO RATT-AREA                                           
077200     PERFORM S11-SKRIV-RATT-POST                                          
077300     .                                                                    
077400     EJECT                                                                
077410                                                                          
077500 Z-FINIT SECTION.                                                         
077600     CLOSE W56162                                                         
077700                                                                          
077800           W56164                                                         
077900           W56165                                                         
078000     SKIP2                                                                
078100     MOVE 'S' TO POSTSUM-OPKOD                                            
078200     CALL POSTSUM USING POSTSUM-PARM                                      
078300     .                                                                    
078400     EJECT                                                                
078410                                                                          
078500 S01-LAES-W56162  SECTION.                                                
078700     READ W56162 INTO IN1-AREA                                            
078800     AT END                                                               
078900        MOVE HIGH-VALUE TO IN1-AREA                                       
079000        SET END-OF-W56162 TO TRUE                                         
079100                                                                          
079200     NOT AT END                                                           
079300        MOVE 'W56162' TO POSTSUM-FDNAMN                                   
079400        MOVE 'W56162D1' TO POSTSUM-DDNAMN2                                
079500        MOVE 'INPOST'   TO POSTSUM-TRANSTYP                               
079600        CALL POSTSUM USING POSTSUM-PARM                                   
079700     END-READ                                                             
079800     .                                                                    
079900     EJECT                                                                
079910                                                                          
080000 S03-KONTROLLERA-KDVALISO SECTION.                                        
080200     MOVE SPAR-KDVALISO            TO CURR-KDVALISO-ROW                   
080400     MOVE W-DATE-AAMM              TO CURR-TIAAMM                         
080410     MOVE WS-KDVALISO-HUV          TO CURR-KDVALISO-HUV                   
080420     MOVE 'M'                      TO CURR-KDVALTYP                       
080430                                                                          
080440     CALL W510CURR USING CURR-W510CURR WDG2-PCB                           
080450     IF CURR-KDSVAR = ' '                                                 
080460                                                                          
080700       MOVE SPAR-KDVALISO          TO WS-KDVALISO                         
080800     ELSE                                                                 
080900       MOVE SPACE                  TO WS-KDVALISO                         
081000     END-IF                                                               
081100                                                                          
081200     .                                                                    
081300     EJECT                                                                
081310                                                                          
081400 S11-SKRIV-RATT-POST SECTION.                                             
081500     WRITE RATT-POST FROM RATT-AREA                                       
081600                                                                          
081700     MOVE 'GODK-POST' TO POSTSUM-TRANSTYP                                 
081800     MOVE 'W56164' TO POSTSUM-FDNAMN                                      
081900     MOVE 'W56162D2' TO POSTSUM-DDNAMN2                                   
082000     CALL POSTSUM USING POSTSUM-PARM                                      
082100     .                                                                    
082200     EJECT                                                                
082210                                                                          
082300 S12-SKRIV-FEL-POST SECTION.                                              
082500     WRITE FEL-POST FROM FEL-AREA                                         
082600                                                                          
082700     MOVE 'FEL-POST' TO POSTSUM-TRANSTYP                                  
082800     MOVE 'W56165' TO POSTSUM-FDNAMN                                      
082900     MOVE 'W56165D5' TO POSTSUM-DDNAMN2                                   
083000     CALL POSTSUM USING POSTSUM-PARM                                      
083100     MOVE SPACE TO SPAR-BEFEL                                             
083200     .                                                                    
083300     EJECT                                                                
083310                                                                          
083400 IMS-GET-WDH5-HHT SECTION.                                                
083600     STRING 'WDH501  (WDH501KY =' W-WDH501KY-X ')'                        
083700          DELIMITED BY SIZE INTO SSA1                                     
083800     MOVE '  GE' TO GODK-STATUSKODER                                      
083900     CALL CBLTDLI USING GU WDH5-PCB DLI-IO-WDH501 SSA1                    
084000     MOVE WDH5-STATUS-CODE TO STATUS-WS                                   
084100     PERFORM IMS-STATUSKONTROLL                                           
084200     .                                                                    
084300     EJECT                                                                
084310                                                                          
084400 IMS-GET-WDH5-SHT SECTION.                                                
084600     STRING 'WDH511  (KDEKSHT  =' W-KDEKSHT-X ')'                         
084700          DELIMITED BY SIZE INTO SSA1                                     
084800     MOVE '  GE' TO GODK-STATUSKODER                                      
084900     CALL CBLTDLI USING GNP WDH5-PCB DLI-IO-WDH511 SSA1                   
085000     MOVE WDH5-STATUS-CODE TO STATUS-WS                                   
085100     PERFORM IMS-STATUSKONTROLL                                           
085200     .                                                                    
085300     EJECT                                                                
085310                                                                          
085400 IMS-GET-WDH5-NIVA SECTION.                                               
085600     STRING 'WDH521  (KDEKNIVA =' W-KDEKNIVA-X ')'                        
085700          DELIMITED BY SIZE INTO SSA1                                     
085800     MOVE '  GE' TO GODK-STATUSKODER                                      
085900     CALL CBLTDLI USING GNP WDH5-PCB DLI-IO-WDH521 SSA1                   
086000     MOVE WDH5-STATUS-CODE TO STATUS-WS                                   
086100     PERFORM IMS-STATUSKONTROLL                                           
086200     .                                                                    
086300     EJECT                                                                
086400 IMS-GET-WDH5-ALL SECTION.                                                
086500                                                                          
086600     STRING 'WDH501  (WDH501KY =' W-WDH501KY-X ')'                        
086700          DELIMITED BY SIZE INTO SSA1                                     
086800     STRING 'WDH511  (KDEKSHT  =' W-KDEKSHT-X ')'                         
086900          DELIMITED BY SIZE INTO SSA2                                     
087000     STRING 'WDH521  (KDEKNIVA =' W-KDEKNIVA-X ')'                        
087100          DELIMITED BY SIZE INTO SSA3                                     
087200     MOVE '  GE' TO GODK-STATUSKODER                                      
087300     CALL CBLTDLI USING GU WDH5-PCB DLI-IO-WDH521 SSA1 SSA2 SSA3          
087400     MOVE WDH5-STATUS-CODE TO STATUS-WS                                   
087500     PERFORM IMS-STATUSKONTROLL                                           
087600     .                                                                    
087700     EJECT                                                                
087710                                                                          
087800 IMS-GNP-WDH5      SECTION.                                               
087900     STRING 'WDH531   '                                                   
088000          DELIMITED BY SIZE INTO SSA1                                     
088100     MOVE '  GE' TO GODK-STATUSKODER                                      
088200     CALL CBLTDLI USING GNP WDH5-PCB DLI-IO-WDH531 SSA1                   
088300     MOVE WDH5-STATUS-CODE TO STATUS-WS                                   
088400     PERFORM IMS-STATUSKONTROLL                                           
088500     .                                                                    
088600     EJECT                                                                
088610                                                                          
089900 IMS-GU-WDB601    SECTION.                                                
090000     STRING 'WDB601  (IDDC     =' W-IDDC-B6-X ')'                         
090100          DELIMITED BY SIZE INTO SSA1                                     
090200     MOVE '  GE' TO GODK-STATUSKODER                                      
090300     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-AREA-B601 SSA1                 
090400     MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
090500     PERFORM IMS-STATUSKONTROLL                                           
090600     IF SEGMENT-SAKNAS                                                    
090700         MOVE SPACE TO DCS-KDDC                                           
090800     END-IF                                                               
090900     .                                                                    
090910                                                                          
091000 IMS-STATUSKONTROLL SECTION.                                              
091200     SET STATUS-IX TO 1                                                   
091300     SEARCH GODK-STATUS                                                   
091400       AT END CALL FELLOG                                                 
091500       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS CONTINUE                  
091600     END-SEARCH                                                           
091700     .                                                                    
