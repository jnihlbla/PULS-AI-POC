000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W5106300.                                                
000300 AUTHOR.         MARKUS ASPFJÄLL.                                         
000400 DATE-WRITTEN.   98/05/28.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*                                                                         
000800*    FUNKTION:                                                            
000900*        LÄSER IN 1 FIL, POSTER IFRÅN WDR9-PEDALBASEN.                    
001000*                                                                         
001100*                                                                         
001200*        KONTROLLERAR EKONOMISKA HÄNDELSER OCH AVVISAR                    
001300*        FELAKTIGA POSTER                                                 
001400*                                                                         
001500*    FEBRUARI - 2003:                                                     
001600*    KURSDIFF-POSTER SKAPAS I FÖREKOMMANDE FALL FÖR                       
001700*    DEALER-NET/DDI MARKNADER (SÖK PÅ "DDI")                              
001800*                                                                         
001900*    ABENDKODER:                                                          
002000*        U0016 -  . . . .                                                 
002100*        U1000 -  . . . .                                                 
002200*                                                                         
002300                                                                          
002400     SKIP3                                                                
002500 ENVIRONMENT DIVISION.                                                    
002600     SKIP2                                                                
002700 INPUT-OUTPUT SECTION.                                                    
002800                                                                          
002900 FILE-CONTROL.                                                            
003000     SKIP2                                                                
003100*          --- INFIL1-HÄNDELSEPOSTER                                      
003200     SELECT W51061                     ASSIGN TO W51063D1.                
003300     SKIP2                                                                
003400*          --- UTFIL1-KORREKTAPOSTER                                      
003500     SELECT W51063                     ASSIGN TO W51063D2.                
003600     SKIP2                                                                
003700*          --- UTFIL2-FELPOSTER                                           
003800     SELECT W51062                     ASSIGN TO W51063D3.                
003900     EJECT                                                                
004000 DATA DIVISION.                                                           
004100     SKIP3                                                                
004200 FILE SECTION.                                                            
004300     SKIP3                                                                
004400 FD  W51061                                                               
004500     RECORDING       F                                                    
004600     BLOCK CONTAINS  0.                                                   
004700                                                                          
004800*01  -COPY WDR901      -L.                                                
004900     SKIP3                                                                
005000 FD  W51063                                                               
005100     RECORDING       F                                                    
005200     BLOCK CONTAINS  0.                                                   
005300                                                                          
005400*01  POST -COPY WDR901 -PRE  RATT- -L.                                    
005500     SKIP3                                                                
005600 FD  W51062                                                               
005700     RECORDING       F                                                    
005800     BLOCK CONTAINS  0.                                                   
005900                                                                          
006000*01  POST -COPY WDR901 -PRE  FEL-  -L.                                    
006100     EJECT                                                                
006200 WORKING-STORAGE SECTION.                                                 
006300                                                                          
006400*    -- CHECKED BY WY2000                                                 
006500 77  IDPGM                       PIC X(8)    VALUE 'W5106300'.            
006600 77  JA                          PIC X       VALUE 'J'.                   
006700 77  NEJ                         PIC X       VALUE 'N'.                   
006800                                                                          
006900 77  W51061-EOF-SW               PIC X       VALUE 'N'.                   
007000     88  END-OF-W51061                       VALUE 'J'.                   
007100                                                                          
007200 77  WS-TOT-AMOUNT-DDI           PIC S9(9)V99  COMP-3 VALUE ZERO.         
007300 77  WS-LINE-AMOUNT-DDI          PIC S9(9)V99  COMP-3 VALUE ZERO.         
007400 77  WS-DIFF-AMOUNT-DDI          PIC S9(9)V99  COMP-3 VALUE ZERO.         
007500 77  WS-EKH-PRARTNTO           PIC S9(7)V9(2)  COMP-3 VALUE ZERO.         
007600 77  WS-EKH-SUBEL              PIC S9(9)V9(2)  COMP-3 VALUE ZERO.         
007700 77  WS-EKH-SUVAT              PIC S9(11)V9(2) COMP-3 VALUE ZERO.         
007800 77  WS-IDVERGL                PIC X(10)   VALUE '          '.            
007810 77  W-DATE-AAMM               PIC 9(4)    VALUE ZERO.                    
007820 77  WS-KDVALISO-HUV           PIC X(3)    VALUE 'SEK'.                   
007900     EJECT                                                                
008000                                                                          
008100 01  FILLER                      PIC X(16)   VALUE 'WWIDFTG '.            
008200*01  -COPY WWIDFTG                                                        
008300     EJECT                                                                
008400                                                                          
008500 01  TEST-IDDISTR                PIC 9(5)    COMP-3.                      
008600*01  FILLER  -COPY WWDIST19   -RED TEST-IDDISTR.                          
008700*01  FILLER  -COPY WWDIS134   -RED TEST-IDDISTR.                          
008800     EJECT                                                                
008900                                                                          
009000 01  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
009100 01  FILLER REDEFINES DAGENS-DATUM.                                       
009200     03  DAGENS-DATUM-AAR        PIC 9(2).                                
009300     03  DAGENS-DATUM-MAANAD     PIC 9(2).                                
009400     03  DAGENS-DATUM-DAG        PIC 9(2).                                
009500 01  W-AAAAMMDD                  PIC 9(8).                                
009600 01  WS-DAGENS-DATUM             PIC 9(8).                                
009700                                                                          
009800 01  RETURKOD                    PIC S9(4) COMP SYNC VALUE +0.            
009900 01  SPAR-KDVALISO               PIC X(3)       VALUE SPACE.              
010000 01  SPAR-BEFEL                  PIC X(20)      VALUE SPACE.              
010100 01  WS-KDVALISO                 PIC X(3)       VALUE SPACE.              
010200                                                                          
010300     EJECT                                                                
010400 01  FEL-TEXTER.                                                          
010500     03 W-FEL-1                 PIC X(20)   VALUE                         
010600     'INGET PROGRAM NUMMER'.                                              
010700     03 W-FEL-2                 PIC X(20)   VALUE                         
010800     'FEL INGEN TRAN KOD'.                                                
010900     03 W-FEL-3                 PIC X(20)   VALUE                         
011000     'FEL REC IDDC'.                                                      
011100     03 W-FEL-4                 PIC X(20)   VALUE                         
011200     'FEL SEND IDDC'.                                                     
011300     03 W-FEL-5                 PIC X(20)   VALUE                         
011400     'FEL LAGAVB EJ JA/NEJ'.                                              
011500     03 W-FEL-6                 PIC X(20)   VALUE                         
011600     'FEL IDUSER SAKNAS'.                                                 
011700     03 W-FEL-7                 PIC X(20)   VALUE                         
011800     'FEL REGISTRERINGSDAT'.                                              
011900     03 W-FEL-8                 PIC X(20)   VALUE                         
012000     'FEL VERDAT > DAGENS '.                                              
012100     03 W-FEL-81                PIC X(20)   VALUE                         
012200     'FEL VERDAT > REGDAT'.                                               
012300     03 W-FEL-9                 PIC X(20)   VALUE                         
012400     'FEL SUMMA ÄR NOLL'.                                                 
012500     03 W-FEL-10                PIC X(20)   VALUE                         
012600     'FEL ANTAL INTE NOLL'.                                               
012700     03 W-FEL-11                PIC X(20)   VALUE                         
012800     'FEL ARTIKELNR SAKNAS'.                                              
012900     03 W-FEL-12                PIC X(20)   VALUE                         
013000     'FEL SUMMA EJ NOLL'.                                                 
013100     03 W-FEL-13                PIC X(20)   VALUE                         
013200     'FEL ANTAL SAKNAS'.                                                  
013300     03 W-FEL-14                PIC X(20)   VALUE                         
013400     'FEL ARTNR EJ NOLL'.                                                 
013500     03 W-FEL-15                PIC X(20)   VALUE                         
013600     'FEL HÄNDELSETYP HUV'.                                               
013700     03 W-FEL-16                PIC X(20)   VALUE                         
013800     'FEL EKON HÄND.NIVÅ'.                                                
013900     03 W-FEL-17                PIC X(20)   VALUE                         
014000     'FEL ISO VALUTAKOD'.                                                 
014100     03 W-FEL-18                PIC X(20)   VALUE                         
014200     'FEL HÄNDELSETYP SHT'.                                               
014300     03 W-FEL-19                PIC X(20)   VALUE                         
014400     'INGET PRODUKTSLAG'.                                                 
014500     03 W-FEL-20                PIC X(20)   VALUE                         
014600     'INGET PRODSL LOKALT'.                                               
014700     03 W-FEL-21                PIC X(20)   VALUE                         
014800     'INGET ARTPRIS NETTO'.                                               
014900     03 W-FEL-22                PIC X(20)   VALUE                         
015000     'INGEN AVERAGE COST'.                                                
015100     03 W-FEL-23                PIC X(20)   VALUE                         
015200     'INGEN SJÄLVKOST'.                                                   
015300     03 W-FEL-24                PIC X(20)   VALUE                         
015400     'INGET STANDARDPRIS'.                                                
015500     03 W-FEL-25                PIC X(20)   VALUE                         
015600     'INGET INKÖPSPRIS'.                                                  
015700     03 W-FEL-26                PIC X(20)   VALUE                         
015800     'INGEN DIREKT LÖN'.                                                  
015900     03 W-FEL-27                PIC X(20)   VALUE                         
016000     'INGET DIR. MATERIAL'.                                               
016100     03 W-FEL-28                PIC X(20)   VALUE                         
016200     'INGET ÖVRIGT PÅLÄGG'.                                               
016300     03 W-FEL-29                PIC X(20)   VALUE                         
016400     'FEL IDNYCKLAR'.                                                     
016500     03 W-FEL-30                PIC X(20)   VALUE                         
016600     'INGET 31-SEGMENT'.                                                  
016700     03 W-FEL-31                PIC X(20)   VALUE                         
016800     'INGEN IDSYSMOT'.                                                    
016900     03 W-FEL-32                PIC X(20)   VALUE                         
017000     'KURS SAKNAS'.                                                       
017100     03 W-FEL-33                PIC X(20)   VALUE                         
017200     'INGEN LON MTRL OVRP'.                                               
017300     03 W-FEL-34                PIC X(20)   VALUE                         
017400     'SUMMA RADER EJ = HUV'.                                              
017500     03 W-FEL-35                PIC X(20)   VALUE                         
017600     'INGEN HEMT.FAKTOR'.                                                 
017700                                                                          
017800                                                                          
017900     EJECT                                                                
018000 01  TRANSAR                      PIC X(4).                               
018100     88 GODK-TRANS                           VALUE '5106' '5108'          
018200                                                   '5109' '5116'          
018300                                                   '5151' '6302'          
018400                                                   '6303' '6309'          
018500                                                   '6193' '6119'          
018600                                                   '6192' '6193'          
018700                                                   '611C' '611D'          
018800                                                   '6115' '6144'          
018900                                                   '4731' '4737'          
019000                                                   '5108' '6203'          
019100                                                   '6117' '6115'          
019200                                                   '6148' '6133'          
019300                                                   '6147'                 
019400                                                   '4738' '6100'.         
019500                                                                          
019600     EJECT                                                                
019700 01  FLLSBOK                     PIC X.                                   
019800     88 GODK-FLLSBOK                        VALUE 'Y' 'N' 'J' ' '.        
019900                                                                          
020000     EJECT                                                                
020100 01  POST-SW                     PIC X      VALUE 'J'.                    
020200     88 POST-OK                             VALUE 'J'.                    
020300     88 POST-FEL                            VALUE 'N'.                    
020400     EJECT                                                                
020600 01  DYNAMISKA-SUBPROGRAM.                                                
020700*                                                                         
020800     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
020900     03  DATKORT                 PIC X(8)    VALUE 'DATKORT'.             
021000     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
021100     03  WDATKONV                PIC X(8)    VALUE 'WDATKONV'.            
021200     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
021300     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
021310     03  W510CURR                PIC X(8)    VALUE 'W510CURR'.            
021400     SKIP2                                                                
021500*    --- PARAMETRAR TILL ABEND                                            
021600                                                                          
021700 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
021800 77  RKOD-ABEND-UTAN-DUMP        PIC S9(4)   COMP VALUE +16.              
021900 77  RKOD-ABEND-MED-DUMP         PIC S9(4)   COMP VALUE +1000.            
022000     SKIP2                                                                
022100 01  FELTEXT.                                                             
022200     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
022300     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
022400     EJECT                                                                
022500*    --- PARAMETRAR TILL DATKORT                                          
022600*                                                                         
022700 01  PROGRAM-NAMN                PIC X(6)    VALUE 'W51063'.              
022800     SKIP2                                                                
022900 01  DATUMKORT-ID                PIC X(6)    VALUE 'WDATUM'.              
023000     SKIP2                                                                
023100*01  -COPY WDATKORT                                                       
023200     EJECT                                                                
023300*    --- PARAMETRAR TILL POSTSUM                                          
023400*                                                                         
023500*01  -COPY W0005   -PRE  POSTSUM-                                         
023600     EJECT                                                                
023700*01  -COPY WDATAREA                                                       
023800     EJECT                                                                
023810*01  -COPY W510CURR                                                       
023820     EJECT                                                                
023900 01  FILLER                      PIC X(16)      VALUE 'IMS'.              
024000                                                                          
024100 01  NYCKLAR-TILL-DLI.                                                    
024200     03  W-WDH501KY-X.                                                    
024300         05  W-IDFTG             PIC 9(2)        VALUE ZERO.              
024400         05  W-KDEKHHT           PIC X(3)        VALUE SPACE.             
024500     03  W-KDEKSHT-X.                                                     
024600         05  W-KDEKSHT           PIC X(3)        VALUE SPACE.             
024700     03  W-KDEKNIVA-X.                                                    
024800         05  W-KDEKNIVA          PIC X(5)        VALUE SPACE.             
024900*        05  W-FLLSBOK           PIC X           VALUE SPACE.             
025000     03  W-IDSYSMOT-X.                                                    
025100         05  W-IDSYSMOT          PIC X(4)        VALUE SPACE.             
025200     03  W-KDSEGKY-X.                                                     
025300         05  W-KDSEGKEY          PIC X           VALUE SPACE.             
026400     03  W-IDDC-B6-X.                                                     
026500         05 W-IDDC-B6            PIC X(2).                                
026600                                                                          
026700 01  STATUS-WS                   PIC XX.                                  
026800     88  SEGMENT-FINNS                      VALUE '  '.                   
026900     88  SEGMENT-FINNS-REDAN                VALUE 'II'.                   
027000     88  SEGMENT-SAKNAS                     VALUE 'GE'.                   
027100     88  SEGMENT-SLUT                       VALUE 'GB'.                   
027200     SKIP2                                                                
027300 01  GODK-STATUSKODER.                                                    
027400     03 GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                 
027500                                                                          
027600 01  SSA1                        PIC X(64).                               
027700 01  SSA2                        PIC X(64).                               
027800 01  SSA3                        PIC X(64).                               
027900                                                                          
028000* ---IMS FUNKTIONSKODER----                                               
028100*01  -COPY W0003                                                          
028200     EJECT                                                                
028300                                                                          
028400*----DLI INPUT OCH OUTPUT AREA ------                                     
028500                                                                          
028600                                                                          
028700 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDH501'.                      
028800 01  DLI-IO-WDH501.                                                       
028900*    03  -COPY WDH501                                                     
029000     EJECT                                                                
029100 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDH511'.                      
029200 01  DLI-IO-WDH511.                                                       
029300*    03  -COPY WDH511                                                     
029400     EJECT                                                                
029500 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDH521'.                      
029600 01  DLI-IO-WDH521.                                                       
029700*    03  -COPY WDH521                                                     
029800     EJECT                                                                
029900 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDH531'.                      
030000 01  DLI-IO-WDH531.                                                       
030100*    03  -COPY WDH531                                                     
030200     EJECT                                                                
030300                                                                          
030400 01  IN1-AREA-START              PIC X(24)   VALUE                        
030500                                 'IN1-AREA-START  '.                      
030600     SKIP2                                                                
030700                                                                          
030800*01  AREA -COPY WDR901   -PRE IN1-                                        
030900*    05   -COPY W510EKHA -PRE IN1- -RED IN1-FIL-WDR901-DATA               
031000     EJECT                                                                
031100 01  RATT-AREA-START             PIC X(24)   VALUE                        
031200                                 'RATT-AREA-START  '.                     
031300     SKIP2                                                                
031400                                                                          
031500*01  AREA -COPY WDR901   -PRE RATT-                                       
031600*    05   -COPY W510EKHA -PRE RATT- -RED RATT-FIL-WDR901-DATA             
031700     EJECT                                                                
031800 01  FEL-AREA-START              PIC X(24)   VALUE                        
031900                                 'FEL-AREA-START  '.                      
032000     SKIP2                                                                
032100                                                                          
032200*01  AREA -COPY WDR901   -PRE FEL-                                        
032300*    05   -COPY W510EKHA -PRE FEL- -RED FEL-FIL-WDR901-DATA               
032400                                                                          
032900 01  FILLER               PIC X(16)   VALUE 'WDB601 AREA'.                
033000 01   DLI-IO-AREA-B601.                                                   
033100*     03  -COPY WDB601                                                    
033200                                                                          
033300     EJECT                                                                
033400 LINKAGE SECTION.                                                         
033500                                                                          
033600*01  -COPY W0008     -PRE WDH5-                                           
033700     05 FILLER              PIC X.                                        
033800                                                                          
033900*01  -COPY W0008     -PRE WDG2-                                           
034000     05 FILLER              PIC X.                                        
034100                                                                          
034200*01  -COPY W0008     -PRE WDB6-                                           
034300     05 FILLER              PIC X.                                        
034400                                                                          
034500 PROCEDURE DIVISION USING   WDH5-PCB WDG2-PCB WDB6-PCB.                   
034600                                                                          
034700 MAIN SECTION.                                                            
034800     ENTRY 'DLITCBL' USING  WDH5-PCB WDG2-PCB WDB6-PCB.                   
034900                                                                          
035000     SKIP2                                                                
035100                                                                          
035200     PERFORM A-INIT                                                       
035300     PERFORM S01-LAES-W51061                                              
035400     PERFORM UNTIL END-OF-W51061                                          
035500       PERFORM B-KONTROLLERA-MED-REGELVERK                                
035600       PERFORM S01-LAES-W51061                                            
035700     END-PERFORM                                                          
035800                                                                          
035900     PERFORM Z-FINIT                                                      
036000                                                                          
036100     MOVE ZERO TO  RETURN-CODE                                            
036200     GOBACK                                                               
036300     .                                                                    
036400     EJECT                                                                
036500 A-INIT SECTION.                                                          
036600                                                                          
036700     OPEN INPUT  W51061                                                   
036800                                                                          
036900     OPEN OUTPUT W51063                                                   
037000                 W51062                                                   
037100     SKIP2                                                                
037200     CALL DATKORT USING PROGRAM-NAMN DATUMKORT-ID DATUMKORT               
037300     MOVE D-AAR     TO  DAGENS-DATUM-AAR                                  
037400     MOVE D-MAANAD  TO  DAGENS-DATUM-MAANAD                               
037500     MOVE D-DAG     TO  DAGENS-DATUM-DAG                                  
037600     MOVE FUNCTION CURRENT-DATE(1:8) TO WS-DAGENS-DATUM                   
037700                                                                          
037800     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
037900     .                                                                    
038000     EJECT                                                                
038100 B-KONTROLLERA-MED-REGELVERK SECTION.                                     
038200                                                                          
038300     IF IN1-EKH-IDVERGL NOT = WS-IDVERGL                                  
038400       MOVE IN1-EKH-IDVERGL  TO WS-IDVERGL                                
038500       MOVE ZERO             TO WS-TOT-AMOUNT-DDI                         
038600                                WS-LINE-AMOUNT-DDI                        
038700                                WS-DIFF-AMOUNT-DDI                        
038800     END-IF                                                               
038900                                                                          
039000     MOVE JA TO POST-SW                                                   
039100     MOVE SPACE TO SPAR-BEFEL                                             
039200     MOVE WC-IDFTG-PV      TO W-IDFTG                                     
039300     MOVE IN1-EKH-KDEKHHT  TO W-KDEKHHT                                   
039400     MOVE IN1-EKH-KDEKSHT  TO W-KDEKSHT                                   
039500     MOVE IN1-EKH-KDEKNIVA TO W-KDEKNIVA                                  
039600     PERFORM IMS-GET-WDH5-ALL                                             
039700     IF SEGMENT-FINNS                                                     
039800       IF NIVA-FLPRODSL = 'Y' AND IN1-EKH-KDPRODSL NOT > 0                
039900         MOVE W-FEL-19     TO SPAR-BEFEL                                  
040000         MOVE NEJ TO POST-SW                                              
040100       END-IF                                                             
040200       IF NIVA-FLPSLLOC = 'Y' AND POST-OK                                 
040300         IF IN1-EKH-KDPSLLOC NOT > 0                                      
040400           MOVE W-FEL-20   TO SPAR-BEFEL                                  
040500           MOVE NEJ        TO POST-SW                                     
040600         END-IF                                                           
040700       END-IF                                                             
040800* INGA PRIS KONTROLLER PÅ HHT=501 SHT=501, NOLL KAN FÖREKOMMA I           
040900* PRISFÄLTEN, MARKUS ASPFJÄLL 19990105                                    
041000* KOMPLETTERAT AV BO HAMMARIN 19990116                                    
041100       IF (IN1-EKH-KDEKHHT = '501' AND IN1-EKH-KDEKSHT = '501') OR        
041200          (IN1-EKH-KDEKHHT = '204' AND IN1-EKH-KDEKSHT = '201') OR        
041300          (IN1-EKH-KDEKHHT = '204' AND IN1-EKH-KDEKSHT = '204') OR        
041400          (IN1-EKH-KDEKHHT = '204' AND IN1-EKH-KDEKSHT = '251')           
041500           CONTINUE                                                       
041600       ELSE                                                               
041700         IF NIVA-FLARTNTO = 'Y' AND POST-OK                               
041800           IF IN1-EKH-PRARTNTO NOT > 0                                    
041900             MOVE W-FEL-21   TO SPAR-BEFEL                                
042000             MOVE NEJ        TO POST-SW                                   
042100           END-IF                                                         
042200         END-IF                                                           
042300         IF NIVA-FLARTSJK = 'Y' AND POST-OK                               
042400           IF IN1-EKH-PRARTSJK = 0                                        
042500             MOVE W-FEL-23   TO SPAR-BEFEL                                
042600             MOVE NEJ        TO POST-SW                                   
042700           END-IF                                                         
042800         END-IF                                                           
042900         IF NIVA-FLARTSTD = 'Y' AND POST-OK                               
043000           IF IN1-EKH-PRARTSTD  = 0                                       
043100             MOVE W-FEL-24   TO SPAR-BEFEL                                
043200             MOVE NEJ        TO POST-SW                                   
043300           END-IF                                                         
043400         END-IF                                                           
043500       END-IF                                                             
043600       IF NIVA-FLAVCOST = 'Y' AND POST-OK                                 
043700         IF IN1-EKH-PRLANDCO NOT > 0                                      
043800           MOVE W-FEL-22   TO SPAR-BEFEL                                  
043900           MOVE NEJ        TO POST-SW                                     
044000         END-IF                                                           
044100       END-IF                                                             
044200       IF NIVA-FLINK    = 'Y' AND POST-OK                                 
044300         IF IN1-EKH-PRINK NOT > 0                                         
044400           MOVE W-FEL-25   TO SPAR-BEFEL                                  
044500           MOVE NEJ        TO POST-SW                                     
044600         END-IF                                                           
044700       END-IF                                                             
044800       IF NIVA-FLDIRLON = 'Y' AND POST-OK                                 
044900         IF IN1-EKH-PRDIRLON NOT > 0                                      
045000           MOVE W-FEL-26   TO SPAR-BEFEL                                  
045100           MOVE NEJ        TO POST-SW                                     
045200         END-IF                                                           
045300       END-IF                                                             
045400       IF NIVA-FLDMTRL  = 'Y' AND POST-OK                                 
045500         IF IN1-EKH-PRDMTRL NOT > 0                                       
045600           MOVE W-FEL-27   TO SPAR-BEFEL                                  
045700           MOVE NEJ        TO POST-SW                                     
045800         END-IF                                                           
045900       END-IF                                                             
046000       IF NIVA-FLOVRPAL = 'Y' AND POST-OK                                 
046100         IF IN1-EKH-PROVRPAL NOT > 0                                      
046200             MOVE W-FEL-28   TO SPAR-BEFEL                                
046300             MOVE NEJ        TO POST-SW                                   
046400         END-IF                                                           
046500       END-IF                                                             
046600       IF NIVA-FLHEMTAG = 'Y' AND POST-OK                                 
046700         IF IN1-EKH-PRHEMTAG NOT > 0                                      
046800             MOVE W-FEL-35   TO SPAR-BEFEL                                
046900             MOVE NEJ        TO POST-SW                                   
047000         END-IF                                                           
047100       END-IF                                                             
047200       IF IN1-EKH-KDEKNIVA = 'DET'                                        
047300         IF IN1-EKH-FLLSBOK = SPACE                                       
047400           IF NIVA-FLLSBOK = 'Y' AND POST-OK                              
047500             MOVE 'Y'      TO IN1-EKH-FLLSBOK                             
047600           END-IF                                                         
047700         END-IF                                                           
047800       END-IF                                                             
047900       IF IN1-EKH-KDEKHHT = '103' AND IN1-EKH-KDEKSHT = '101'             
048000         IF IN1-EKH-PRDIRLON = 0 AND IN1-EKH-PRDMTRL = 0 AND              
048100            IN1-EKH-PROVRPAL = 0                                          
048200          MOVE W-FEL-33           TO SPAR-BEFEL                           
048300          MOVE NEJ                TO POST-SW                              
048400         END-IF                                                           
048500       END-IF                                                             
048600       IF POST-OK                                                         
048700         PERFORM IMS-GNP-WDH5                                             
048800         IF SEGMENT-SAKNAS                                                
048900           IF W-KDEKNIVA  = 'SUM' OR 'MOMS'                               
049000             CONTINUE                                                     
049100           ELSE                                                           
049200             MOVE W-FEL-30 TO SPAR-BEFEL                                  
049300             MOVE NEJ TO POST-SW                                          
049400           END-IF                                                         
049500         ELSE                                                             
049600           IF SYST-IDSYSMOT = SPACE                                       
049700             MOVE W-FEL-31 TO SPAR-BEFEL                                  
049800             MOVE NEJ TO POST-SW                                          
049900           END-IF                                                         
050000         END-IF                                                           
050100       END-IF                                                             
050200       IF POST-OK                                                         
050300       MOVE IN1-EKH-KDVALISO TO SPAR-KDVALISO                             
050400*** SPANSKA PESETAS FINNS I BÄGGE VARIANTERNA ESP OCH ESB                 
050500         IF SPAR-KDVALISO = 'ESB'                                         
050600           MOVE 'ESP'                  TO IN1-EKH-KDVALISO                
050700         END-IF                                                           
050800*** BELGISKA FRANC  FINNS I BÄGGE VARIANTERNA BEF OCH BEC                 
050900         IF SPAR-KDVALISO = 'BEC'                                         
051000           MOVE 'BEF'                  TO IN1-EKH-KDVALISO                
051100         END-IF                                                           
051200         MOVE IN1-EKH-KDVALISO TO SPAR-KDVALISO                           
051300         PERFORM S03-KONTROLLERA-KDVALISO                                 
051400         IF WS-KDVALISO = SPACE                                           
051500           MOVE W-FEL-17 TO SPAR-BEFEL                                    
051600         ELSE                                                             
051700           PERFORM BA-KONTROLLERA-POST                                    
051800         END-IF                                                           
051900       END-IF                                                             
052000     ELSE                                                                 
052100       PERFORM IMS-GET-WDH5-HHT                                           
052200       IF SEGMENT-SAKNAS                                                  
052300         MOVE W-FEL-15        TO SPAR-BEFEL                               
052400       ELSE                                                               
052500         PERFORM IMS-GET-WDH5-SHT                                         
052600         IF SEGMENT-SAKNAS                                                
052700           MOVE W-FEL-18      TO SPAR-BEFEL                               
052800         ELSE                                                             
052900           PERFORM IMS-GET-WDH5-NIVA                                      
053000           IF SEGMENT-SAKNAS                                              
053100               MOVE W-FEL-16    TO SPAR-BEFEL                             
053200           END-IF                                                         
053300         END-IF                                                           
053400       END-IF                                                             
053500     END-IF                                                               
053600     IF SPAR-BEFEL NOT = SPACE                                            
053700       PERFORM BC-SKICKA-FELPOST                                          
053800     END-IF                                                               
053900     .                                                                    
054000     EJECT                                                                
054100 BA-KONTROLLERA-POST SECTION.                                             
054200     IF IN1-FIL-IDPGM(1:1) NOT = 'W'                                      
054300       MOVE W-FEL-1 TO SPAR-BEFEL                                         
054400     ELSE                                                                 
054500* ALLA TRANSKODER ÄR OK 19990105-MARKUS ASPFJÄLL *****                    
054600*      IF IN1-FIL-IDPGM(3:1)    = '0'                                     
054700*        MOVE IN1-EKH-IDTRANS TO TRANSAR                                  
054800*        IF NOT GODK-TRANS                                                
054900*          MOVE W-FEL-2 TO SPAR-BEFEL                                     
055000*        END-IF                                                           
055100*      END-IF                                                             
055200       IF SPAR-BEFEL = SPACE                                              
055300         MOVE IN1-EKH-IDDC-SEND TO W-IDDC-B6                              
055400         PERFORM IMS-GU-WDB601                                            
055500         IF DCS-KDDC = SPACE AND IN1-EKH-IDDC-SEND NOT = SPACE            
055600           MOVE W-FEL-4 TO SPAR-BEFEL                                     
055700         ELSE                                                             
055800           MOVE IN1-EKH-IDDC-REC TO W-IDDC-B6                             
055900           PERFORM IMS-GU-WDB601                                          
056000           IF DCS-KDDC = SPACE AND IN1-EKH-IDDC-REC NOT = SPACE           
056100             MOVE W-FEL-3 TO SPAR-BEFEL                                   
056200           ELSE                                                           
056300             MOVE IN1-EKH-FLLSBOK TO FLLSBOK                              
056400             IF IN1-EKH-FLLSBOK = 'J'                                     
056500               MOVE 'Y' TO IN1-EKH-FLLSBOK                                
056600             END-IF                                                       
056700             IF NOT GODK-FLLSBOK                                          
056800               MOVE W-FEL-5 TO SPAR-BEFEL                                 
056900             ELSE                                                         
057000*              IF IN1-FIL-IDUSER = SPACE                                  
057100*                MOVE W-FEL-6 TO SPAR-BEFEL                               
057200*              ELSE                                                       
057300*---- FLYTTA SUBPROGRAM CALL TILL RÄTT STÄLLE --                          
057400                 MOVE 'AAMMDD' TO DAT-KDDATFORM                           
057500                 MOVE DAGENS-DATUM TO DAT-I-TIDATUM                       
057600                                                                          
057700                 CALL WDATKONV USING DAT-KDDATFORM                        
057800                                     DAT-I-TIDATUM                        
057900                                     DAT-O-TIDATUM                        
058000                                     DAT-KDSVAR                           
058100                                                                          
058200                 IF DAT-KDSVAR-OK                                         
058300                   MOVE DAT-TIAAMMDD TO W-AAAAMMDD                        
058400                   MOVE DAT-TISEKEL  TO W-AAAAMMDD(1:2)                   
058500                 ELSE                                                     
058600                   MOVE +1000        TO RETURKOD                          
058700                   CALL ABEND USING RETURKOD                              
058800                 END-IF                                                   
058900                 IF IN1-FIL-DAREGDAT >  WS-DAGENS-DATUM                   
059000                   MOVE W-FEL-7 TO SPAR-BEFEL                             
059100                 ELSE                                                     
059200                   IF IN1-EKH-DAVERDAT >  WS-DAGENS-DATUM                 
059300                     MOVE W-FEL-8 TO SPAR-BEFEL                           
059400                   ELSE                                                   
059500                     IF IN1-EKH-DAVERDAT >  IN1-FIL-DAREGDAT              
059600                       MOVE W-FEL-81 TO SPAR-BEFEL                        
059700                     ELSE                                                 
059800                       IF IN1-EKH-KDEKNIVA NOT = 'DET'                    
059900                         IF IN1-EKH-SUBEL = ZERO                          
060000* SUMMABELOPP FÅR VARA NOLL NÄR DET ÄR EN 404-401 POST, SKROT             
060100                          IF IN1-EKH-KDEKHHT = '404' AND                  
060200                             IN1-EKH-KDEKSHT = '401'                      
060300                            CONTINUE                                      
060400                          ELSE                                            
060500                            IF IN1-EKH-KDEKHHT = '204' AND                
060600                               IN1-EKH-KDEKSHT = '204'                    
060700                              CONTINUE                                    
060701                            ELSE                                          
060900                                MOVE W-FEL-9 TO SPAR-BEFEL                
061060                            END-IF                                        
061100                          END-IF                                          
061200                         ELSE                                             
061300                           IF IN1-EKH-KVANTAL NOT = ZERO                  
061400                             MOVE W-FEL-10 TO SPAR-BEFEL                  
061500                           ELSE                                           
061600                             IF IN1-EKH-KDEKHHT = '301'                   
061700                               IF IN1-EKH-KDEKSHT = '301' OR              
061800                                           '302' OR '303'                 
061900                                 IF IN1-EKH-IDARTNR = ZERO                
062000                                   MOVE W-FEL-11 TO                       
062100                                        SPAR-BEFEL                        
062200                                 END-IF                                   
062300                               ELSE                                       
062400                                 IF IN1-EKH-IDARTNR NOT = ZERO            
062500                                   MOVE W-FEL-14 TO                       
062600                                      SPAR-BEFEL                          
062700                                 END-IF                                   
062710                               END-IF                                     
062800                             END-IF                                       
062900                           END-IF                                         
063100                         END-IF                                           
063200                       ELSE                                               
063300                         IF IN1-EKH-KDEKNIVA = 'DET'                      
063400                           IF IN1-EKH-SUBEL NOT = ZERO                    
063500                             MOVE W-FEL-12 TO SPAR-BEFEL                  
063600                           ELSE                                           
063700                             IF IN1-EKH-IDARTNR = ZERO                    
063800                               IF IN1-EKH-KDEKHHT = '204' AND             
063900                                  IN1-EKH-KDEKSHT = '204'                 
064000                                 CONTINUE                                 
064100                               ELSE                                       
064200                                 MOVE W-FEL-11 TO SPAR-BEFEL              
064300                               END-IF                                     
064400                             ELSE                                         
064410                              IF  IN1-EKH-KDEKHHT = '303'                 
064420                              AND IN1-EKH-KDEKSHT = '311'                 
064430                                IF IN1-EKH-PRARTSTD = 0                   
064440                                  MOVE W-FEL-24   TO SPAR-BEFEL           
064450                                END-IF                                    
064460                              END-IF                                      
064500                               IF IN1-EKH-KDEKHHT(1:1) = '2' OR           
064600                                  IN1-EKH-KDEKHHT = '303'                 
064700                                 CONTINUE                                 
064800                               ELSE                                       
064900                                 IF IN1-EKH-KVANTAL = ZERO                
065000* DET KAN KOMMA POSTER MED NOLL I ANTAL, MARKUS ASPFJÄLL 19990108         
065100* UNDANTAGET GÄLLER BARA SKROTNING, 404-401 POSTER                        
065200* ALLA ANDRA POSTER BLIR DET EN FELPOST UTAV                              
065300                                   IF IN1-EKH-KDEKHHT = '404' AND         
065400                                      IN1-EKH-KDEKSHT = '401'             
065500                                     CONTINUE                             
065600                                   ELSE                                   
065700                                     MOVE W-FEL-13 TO SPAR-BEFEL          
065800                                   END-IF                                 
065900                                 END-IF                                   
066000                               END-IF                                     
066100                             END-IF                                       
066200                           END-IF                                         
066300                         END-IF                                           
066400                       END-IF                                             
066500                     END-IF                                               
066600                   END-IF                                                 
066700                 END-IF                                                   
066800*              END-IF                                                     
066900             END-IF                                                       
067000           END-IF                                                         
067100         END-IF                                                           
067200       END-IF                                                             
067300     END-IF                                                               
067400     IF SPAR-BEFEL = SPACE                                                
067500       IF IN1-EKH-IDDISTR  > 0 OR                                         
067600          IN1-EKH-IDKUNDNR > 0 OR                                         
067700          IN1-EKH-IDVERGL  > 0                                            
067800         CONTINUE                                                         
067900       ELSE                                                               
068000         MOVE W-FEL-29 TO SPAR-BEFEL                                      
068100       END-IF                                                             
068200       IF IN1-EKH-PRKURS = 0 AND IN1-EKH-KDEKNIVA = 'DET'                 
068300         MOVE W-FEL-32 TO SPAR-BEFEL                                      
068400       END-IF                                                             
068500     END-IF                                                               
068600                                                                          
068700     IF SPAR-BEFEL = SPACE                                                
068800       MOVE IN1-EKH-IDDISTR TO TEST-IDDISTR                               
068900       IF IN1-FIL-IDPGM = 'W4183000' OR 'W4263400' OR 'W4263500'          
069000          OR 'W5402000' OR 'W4183C00'                                     
069100          OR DIST19-SATS OR DIS134-BYTESRENOV                             
069200* SYSTEM W426-KRF, W54020, W41830 M. FL. SKALL GÅ DEN GAMLA VÄGEN         
069320          PERFORM BD-SKICKA-RATT-POST                                     
069400       ELSE                                                               
069500* HÄR GENERERAS KURSDIFF-POSTER FÖR DEALER-NET/DDI MARKNADER              
069600         IF IN1-EKH-KDEKNIVA = 'DET'                                      
069700           PERFORM BD-SKICKA-RATT-POST                                    
069800         ELSE                                                             
069900           IF IN1-EKH-KDEKNIVA = 'SUM'                                    
070000             PERFORM BD-SKICKA-RATT-POST                                  
070100           ELSE                                                           
070200             PERFORM BD-SKICKA-RATT-POST                                  
070300           END-IF                                                         
070400         END-IF                                                           
070500       END-IF                                                             
070600     ELSE                                                                 
070700       PERFORM BC-SKICKA-FELPOST                                          
070800     END-IF                                                               
070900     .                                                                    
071000     EJECT                                                                
071100 BC-SKICKA-FELPOST SECTION.                                               
071200     IF IN1-EKH-KDEKHHT = '2??' AND IN1-EKH-KDEKSHT = '2??'               
071300       CONTINUE                                                           
071400     ELSE                                                                 
071500       MOVE IN1-AREA TO FEL-AREA                                          
071600       MOVE SPAR-BEFEL       TO FEL-EKH-BEFELSAP                          
071700       MOVE 'W510EKFA'       TO FEL-FIL-IDCPYTXT                          
071800       PERFORM S12-SKRIV-FEL-POST                                         
071900     END-IF                                                               
072000                                                                          
072100     .                                                                    
072200     EJECT                                                                
072300 BD-SKICKA-RATT-POST SECTION.                                             
072400     MOVE IN1-AREA TO RATT-AREA                                           
072500     PERFORM S11-SKRIV-RATT-POST                                          
072600     .                                                                    
072700     EJECT                                                                
072800 Z-FINIT SECTION.                                                         
072900     CLOSE W51061                                                         
073000                                                                          
073100           W51063                                                         
073200           W51062                                                         
073300     SKIP2                                                                
073400     MOVE 'S' TO POSTSUM-OPKOD                                            
073500     CALL POSTSUM USING POSTSUM-PARM                                      
073600     .                                                                    
073700     EJECT                                                                
073800 S01-LAES-W51061  SECTION.                                                
073900                                                                          
074000     READ W51061 INTO IN1-AREA                                            
074100     AT END                                                               
074200        MOVE HIGH-VALUE TO IN1-AREA                                       
074300        SET END-OF-W51061 TO TRUE                                         
074400                                                                          
074500     NOT AT END                                                           
074600        MOVE 'W51061' TO POSTSUM-FDNAMN                                   
074700        MOVE 'W51063D1' TO POSTSUM-DDNAMN2                                
074800        MOVE 'INPOST'   TO POSTSUM-TRANSTYP                               
074900        CALL POSTSUM USING POSTSUM-PARM                                   
075000     END-READ                                                             
075100     .                                                                    
075200     EJECT                                                                
075300 S03-KONTROLLERA-KDVALISO SECTION.                                        
075400                                                                          
075500     MOVE SPAR-KDVALISO            TO CURR-KDVALISO-ROW                   
075600     MOVE DAGENS-DATUM-AAR         TO W-DATE-AAMM(1:2)                    
075610     MOVE DAGENS-DATUM-MAANAD      TO W-DATE-AAMM(3:2)                    
075620                                                                          
075700     MOVE W-DATE-AAMM              TO CURR-TIAAMM                         
075710     MOVE WS-KDVALISO-HUV          TO CURR-KDVALISO-HUV                   
075720     MOVE 'M'                      TO CURR-KDVALTYP                       
075740                                                                          
075800     CALL W510CURR USING CURR-W510CURR WDG2-PCB                           
075900     IF CURR-KDSVAR = ' '                                                 
076000       MOVE SPAR-KDVALISO          TO WS-KDVALISO                         
076100     ELSE                                                                 
076200       MOVE SPACE                  TO WS-KDVALISO                         
076300     END-IF                                                               
076400                                                                          
076500     .                                                                    
076600     EJECT                                                                
076700 S11-SKRIV-RATT-POST SECTION.                                             
076800     WRITE RATT-POST FROM RATT-AREA                                       
076900                                                                          
077000     MOVE 'GODK-POST' TO POSTSUM-TRANSTYP                                 
077100     MOVE 'W5106B' TO POSTSUM-FDNAMN                                      
077200     MOVE 'W51063D3' TO POSTSUM-DDNAMN2                                   
077300     CALL POSTSUM USING POSTSUM-PARM                                      
077400     .                                                                    
077500     EJECT                                                                
077600 S12-SKRIV-FEL-POST SECTION.                                              
077700                                                                          
077800     WRITE FEL-POST FROM FEL-AREA                                         
077900                                                                          
078000     MOVE 'FEL-POST' TO POSTSUM-TRANSTYP                                  
078100     MOVE 'W5106A' TO POSTSUM-FDNAMN                                      
078200     MOVE 'W51063D4' TO POSTSUM-DDNAMN2                                   
078300     CALL POSTSUM USING POSTSUM-PARM                                      
078400     MOVE SPACE TO SPAR-BEFEL                                             
078500     .                                                                    
078600     EJECT                                                                
078700 IMS-GET-WDH5-HHT SECTION.                                                
078800                                                                          
078900     STRING 'WDH501  (WDH501KY =' W-WDH501KY-X ')'                        
079000          DELIMITED BY SIZE INTO SSA1                                     
079100     MOVE '  GE' TO GODK-STATUSKODER                                      
079200     CALL CBLTDLI USING GU WDH5-PCB DLI-IO-WDH501 SSA1                    
079300     MOVE WDH5-STATUS-CODE TO STATUS-WS                                   
079400     PERFORM IMS-STATUSKONTROLL                                           
079500     .                                                                    
079600     EJECT                                                                
079700 IMS-GET-WDH5-SHT SECTION.                                                
079800                                                                          
079900     STRING 'WDH511  (KDEKSHT  =' W-KDEKSHT-X ')'                         
080000          DELIMITED BY SIZE INTO SSA1                                     
080100     MOVE '  GE' TO GODK-STATUSKODER                                      
080200     CALL CBLTDLI USING GNP WDH5-PCB DLI-IO-WDH511 SSA1                   
080300     MOVE WDH5-STATUS-CODE TO STATUS-WS                                   
080400     PERFORM IMS-STATUSKONTROLL                                           
080500     .                                                                    
080600     EJECT                                                                
080700 IMS-GET-WDH5-NIVA SECTION.                                               
080800                                                                          
080900     STRING 'WDH521  (KDEKNIVA =' W-KDEKNIVA-X ')'                        
081000          DELIMITED BY SIZE INTO SSA1                                     
081100     MOVE '  GE' TO GODK-STATUSKODER                                      
081200     CALL CBLTDLI USING GNP WDH5-PCB DLI-IO-WDH521 SSA1                   
081300     MOVE WDH5-STATUS-CODE TO STATUS-WS                                   
081400     PERFORM IMS-STATUSKONTROLL                                           
081500     .                                                                    
081600     EJECT                                                                
081700 IMS-GET-WDH5-ALL SECTION.                                                
081800                                                                          
081900     STRING 'WDH501  (WDH501KY =' W-WDH501KY-X ')'                        
082000          DELIMITED BY SIZE INTO SSA1                                     
082100     STRING 'WDH511  (KDEKSHT  =' W-KDEKSHT-X ')'                         
082200          DELIMITED BY SIZE INTO SSA2                                     
082300     STRING 'WDH521  (KDEKNIVA =' W-KDEKNIVA-X ')'                        
082400          DELIMITED BY SIZE INTO SSA3                                     
082500     MOVE '  GE' TO GODK-STATUSKODER                                      
082600     CALL CBLTDLI USING GU WDH5-PCB DLI-IO-WDH521 SSA1 SSA2 SSA3          
082700     MOVE WDH5-STATUS-CODE TO STATUS-WS                                   
082800     PERFORM IMS-STATUSKONTROLL                                           
082900     .                                                                    
083000     EJECT                                                                
083100 IMS-GNP-WDH5      SECTION.                                               
083200     STRING 'WDH531   '                                                   
083300          DELIMITED BY SIZE INTO SSA1                                     
083400     MOVE '  GE' TO GODK-STATUSKODER                                      
083500     CALL CBLTDLI USING GNP WDH5-PCB DLI-IO-WDH531 SSA1                   
083600     MOVE WDH5-STATUS-CODE TO STATUS-WS                                   
083700     PERFORM IMS-STATUSKONTROLL                                           
083800     .                                                                    
083900     EJECT                                                                
085200 IMS-GU-WDB601    SECTION.                                                
085300     STRING 'WDB601  (IDDC     =' W-IDDC-B6-X ')'                         
085400          DELIMITED BY SIZE INTO SSA1                                     
085500     MOVE '  GE' TO GODK-STATUSKODER                                      
085600     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-AREA-B601 SSA1                 
085700     MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
085800     PERFORM IMS-STATUSKONTROLL                                           
085900     IF SEGMENT-SAKNAS                                                    
086000         MOVE SPACE TO DCS-KDDC                                           
086100     END-IF                                                               
086200     .                                                                    
086300 IMS-STATUSKONTROLL SECTION.                                              
086400                                                                          
086500     SET STATUS-IX TO 1                                                   
086600     SEARCH GODK-STATUS                                                   
086700       AT END CALL FELLOG                                                 
086800       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS CONTINUE                  
086900     END-SEARCH                                                           
087000     .                                                                    
