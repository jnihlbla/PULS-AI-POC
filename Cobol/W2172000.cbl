000100 PROCESS DYNAM                                                            
000200 ID DIVISION.                                                             
000300     SKIP2                                                                
000400 PROGRAM-ID.     W2172000.                                                
000500*AUTHOR.         INGER STENING.                                           
000600*DATE-WRITTEN.   12/11/02.                                                
000700                                                                          
000800*    REMARKS.                                                             
000900*        NOTE ! OBS ! NOTE ! OBS ! NOTE ! OBS ! NOTE ! OBS !              
001000*                                                                         
001100*        AT COMPILATION, CHANGE TO LINES=75 or rather LINES=80            
001200*        IN JCL JOB-CARD TO AVOID ANNOYING ABENDS.                        
001300*                                                                         
001400*    FUNKTION:                                                            
001500*        DETTA PGM SKRIVER UT ARTIKEL-INFORMATION                         
001600*        MED HJÄLP AV ETT URVAL                                           
001700*        SOM VIA SOP-PARAMETRAR FRÅN BILD 2422.                           
001800*        OCH SORTERINGS-ALTERNATIV KOMMER OCKSÅ DENNA VÄG.                
001900*                                                                         
002000*        PROGRAMMET LÄSER      WDD9                                       
002100*        PROGRAMMET LÄSER      WDF5                                       
002200*        PROGRAMMET LÄSER      WDK6                                       
002300*        PROGRAMMET LÄSER      WDK7 (VIA SUBPGM W222BHDC)                 
002400*        PROGRAMMET LÄSER      WDR2                                       
002500*        PROGRAMMET LÄSER      W6D1                                       
002600*        PROGRAMMET LÄSER      W6H7 med W6H7B                             
002700*        PROGRAMMET LÄSER      WDA5                                       
002800*                                                                         
002900*    ÄNDRING:                                                             
003000*                                                                         
003100*    ABENDKODER:                                                          
003200*        U0016 -  . . . .                                                 
003300*        U1000 -  . . . .                                                 
003400*                                                                         
003500                                                                          
003600     EJECT                                                                
003700 ENVIRONMENT DIVISION.                                                    
003800     SKIP2                                                                
003900 INPUT-OUTPUT SECTION.                                                    
004000                                                                          
004100 FILE-CONTROL.                                                            
004200     SKIP2                                                                
004300*          --- PARAMETRAR FRÅN BILD 2422                                  
004400     Select W2172010                   Assign To W21720D1.                
004500     SKIP2                                                                
004600*          --- PULS FILE FROM WDK7, WDK6, WDL7 and WDD3                   
004700     Select W2172020                   Assign To W21720D2.                
004800     SKIP2                                                                
004900*          --- LISTA ARTIKEL-INFORMATION EXCELFIL                         
005000     Select W21720-001                 Assign To W21720D3.                
005100     SKIP2                                                                
005200*          --- SORTERINGSFIL                                              
005300     Select SORTFIL                    Assign To W21720DS.                
005400     EJECT                                                                
005500 DATA DIVISION.                                                           
005600     SKIP2                                                                
005700 FILE SECTION.                                                            
005800     SKIP2                                                                
005900 FD  W2172010                                                             
006000     RECORDING       F                                                    
006100     BLOCK CONTAINS  0.                                                   
006200     SKIP2                                                                
006300*01  -COPY W2172010    -L.                                                
006400     SKIP3                                                                
006500 FD  W2172020                                                             
006600     RECORDING       F                                                    
006700     BLOCK CONTAINS  0.                                                   
006800     SKIP2                                                                
006900*01  -COPY W2172020    -L.                                                
007000     SKIP3                                                                
007100 FD  W21720-001                                                           
007200     RECORDING       V                                                    
007300     BLOCK CONTAINS  0                                                    
007400     Record Is Varying From 1 To 1183 Depending On LAENGD.                
007500*    LINAGE W001-MAX-RADER-PER-SIDA.                                      
007600 01  W21720-001-RAD              PIC X(1183).                             
007700     EJECT                                                                
007800 SD  SORTFIL.                                                             
007900     SKIP2                                                                
008000*01  POST -COPY W21720S     -PRE SORT-                                    
008100     EJECT                                                                
008200 WORKING-STORAGE SECTION.                                                 
008300     SKIP2                                                                
008400                                                                          
008500*    -COPY WY2000W1                                                       
008600                                                                          
008700*    -COPY WY2000W2                                                       
008800                                                                          
008900*    -- CHECKED BY WY2000                                                 
009000 77  IDPGM                       PIC X(8)    Value 'W2172000'.            
009100 77  FILLER                      PIC X(24)  VALUE 'IMS-SEKTION ='.        
009200 77  IMS-SEKTION                 PIC X(30).                               
009300 77  CURRENT-SECTION             PIC X(30)   VALUE SPACE.                 
009400 77  TEST1-YYMMDD                PIC 9(6)    VALUE ZERO.                  
009500 77  TEST2-YYMMDD                PIC 9(6)    VALUE ZERO.                  
009600 77  IX                          PIC S9(4) COMP SYNC.                     
009700 77  IX1                         PIC S9(4) COMP SYNC.                     
009800 77  IX2                         PIC S9(4) COMP SYNC.                     
009900 77  IX1-TILEVDAG                PIC S9(4) COMP SYNC.                     
010000 77  INDX-VOR                    PIC S9(4) COMP SYNC.                     
010100 77  MAX-INDX-VOR                PIC S9(4) COMP SYNC VALUE +500.          
010200 77  TAB-TECKEN                  PIC X       VALUE x'05'.                 
010300 77  ANT-LB-POST                 PIC S9(9) COMP SYNC Value Zero.          
010400 77  YES                         PIC X       Value 'Y'.                   
010500 77  JA                          PIC X       Value 'J'.                   
010600 77  NOO                         PIC X       Value 'N'.                   
010700 77  LAENGD                      PIC 9(4)    Value 1183.                  
010800 77  BEART-DIFAELT               PIC S9(4)   Value Zero comp.             
010900 77  BEART-BESORD                PIC X(50)   Value Space.                 
011000 77  5-Space                     PIC X(5)    Value Space.                 
011100 77  ENDAST-XDCBEHOV             PIC X(2)    VALUE '02'.                  
011200 77  PB-TOTAL-SEP-LEV-XDC        PIC X(2)    VALUE '03'.                  
011300 77  WS-PART-IDPERSON            PIC 9(3)    VALUE ZERO.                  
011400                                                                          
011500 77  W2172010-EOF-SW             PIC X       Value 'N'.                   
011600     88  END-OF-W2172010                     Value 'J'.                   
011700                                                                          
011800 77  W2172020-EOF-SW             PIC X       Value 'N'.                   
011900     88  END-OF-W2172020                     Value 'J'.                   
012000                                                                          
012100 77  SORTFIL-EOF-SW              PIC X       VALUE 'N'.                   
012200     88  END-OF-SORTFIL                      VALUE 'J'.                   
012300                                                                          
012400 77  URVAL-SW                    PIC X       Value 'N'.                   
012500     88  URVAL-OK                            Value 'Y'.                   
012600                                                                          
012700 77  AVBRYT-SW                   PIC X       Value 'N'.                   
012800     88  AVBRYTES-EJ                         Value 'N'.                   
012900     88  AVBRYT                              Value 'J'.                   
013000                                                                          
013100 77  TRAEFF-SW                   PIC X       Value 'N'.                   
013200     88  TRAEFF-OK                           Value 'Y'.                   
013300     88  TRAEFF-NOO                          Value 'N'.                   
013400                                                                          
013500 77  KR-SKRIVEN-SW               PIC X       Value 'N'.                   
013600     88  ARTIKEL-MED-KR-SKRIVEN              Value 'J'.                   
013700     88  ARTIKEL-MED-KR-EJ-SKRIVEN           Value 'N'.                   
013800*                                                                         
013900 01  FILLER          PIC X(24) Value 'DATUMBERÄKNINGSAREOR'.              
014000                                                                          
014100 01  DAGENS-DADATUM              PIC 9(8)    Value Zero.                  
014200 01  FILLER REDEFINES DAGENS-DADATUM.                                     
014300     03  DAGENS-DADATUM-SEKEL    PIC 9(2).                                
014400     03  DAGENS-TIDATUM          PIC 9(6).                                
014500                                                                          
014600 01 DAGENS-AAAAVVD               PIC 9(7)    Value Zero.                  
014700 01 FILLER REDEFINES DAGENS-AAAAVVD.                                      
014800     03 DAGENS-SEKEL             PIC 9(2).                                
014900     03 DAGENS-AA                PIC 9(2).                                
015000     03 DAGENS-VVD               PIC X(3).                                
015100 01 FILLER REDEFINES DAGENS-AAAAVVD.                                      
015200     03 DAGENS-DAAVROP           PIC 9(6).                                
015300     03 DAGENS-TILEVDAG          PIC 9(1).                                
015400 01 FILLER REDEFINES DAGENS-AAAAVVD.                                      
015500     03 FILLER                   PIC 9(2).                                
015600     03 DAGENS-TIAAVVD           PIC 9(5).                                
015700 01 FILLER REDEFINES DAGENS-AAAAVVD.                                      
015800     03 FILLER                   PIC 9(2).                                
015900     03 DAGENS-TIAAVV            PIC 9(4).                                
016000     03 DAGENS-DAGNR             PIC 9(1).                                
016100                                                                          
016200* Jämförelsefält för avropsdag mot DAGENS-AAAAVVD                         
016300 01 WS-AVROP-DAAVROP-AVS         PIC 9(7).                                
016400 01 FILLER REDEFINES WS-AVROP-DAAVROP-AVS.                                
016500     03 AVROP-DAAVROP-AVS       PIC 9(6).                                 
016600     03 AVROP-TILEVDAG          PIC 9(1).                                 
016700                                                                          
016800 01 WS-ANTAL-URVALSDAGAR         PIC 9(3) Value Zero.                     
016900                                                                          
017000 01 DAAVROP-SLUTVECKA            PIC 9(6).                                
017100 01 FILLER REDEFINES DAAVROP-SLUTVECKA.                                   
017200     03 SLUT-SEKEL               PIC 9(2).                                
017300     03 SLUT-AAVV                PIC 9(4).                                
017400*    03 SLUT-DAG                 PIC 9   .                                
017500                                                                          
017600 01 TMP1-YYWW                    PIC 9(4) Value Zero.                     
017700 01 TMP2-YYWW                    PIC 9(4) Value Zero.                     
017800*                                                                         
017900 01 TILEVBSK-DATUM-KONV.                                                  
018000   03  WS-DALEVBSK-AVS   PIC 9(8).                                        
018100   03  FILLER  REDEFINES WS-DALEVBSK-AVS.                                 
018200       05  WS-DALEVBSK-SS     PIC 9(2).                                   
018300       05  WS-DALEVBSK-AAMMDD PIC 9(6).                                   
018400*                                                                         
018500*                                                                         
018600 01  WS-YEAR                     PIC 9(4).                                
018700 01  OI-ARTAL-0                  PIC 9(4).                                
018800 01  OI-ARTAL-1                  PIC 9(4).                                
018900 01  OI-ARTAL-2                  PIC 9(4).                                
019000 01  OI-ARTAL-3                  PIC 9(4).                                
019100 01  OI-ARTAL-4                  PIC 9(4).                                
019200 01  OI-ARTAL-5                  PIC 9(4).                                
019300                                                                          
019400     EJECT                                                                
019500*      --- VALID IDDC CODES                                               
019600*01    -COPY WWDCKONS                                                     
019700*01    -COPY WWDC99                                                       
019800     EJECT                                                                
019900 01  DYNAMISKA-SUBPROGRAM.                                                
020000*                                                                         
020100     03  ABEND                   PIC X(8)    Value 'ABEND'.               
020200     03  POSTSUM                 PIC X(8)    Value 'POSTSUM'.             
020300     SKIP2                                                                
020400*    --- PARAMETRAR TILL ABEND                                            
020500                                                                          
020600 77  RKOD-ABEND-UTAN-DUMP        PIC S9(4)   COMP Value +16.              
020700 77  RKOD-ABEND-MED-DUMP         PIC S9(4)   COMP Value +1000.            
020800     SKIP2                                                                
020900 01  FELTEXT.                                                             
021000     03  FILLER                  PIC X(8)    Value 'FELTEXT'.             
021100     03  FELTEXT-STR             PIC X(72)   Value Space.                 
021200     EJECT                                                                
021300*    --- PARAMETRAR TILL POSTSUM                                          
021400*                                                                         
021500*01  -COPY W0005   -PRE  POSTSUM-                                         
021600     EJECT                                                                
021700     SKIP2                                                                
021800                                                                          
021900 01  WS-AREA-PARM1.                                                       
022000     03  WS-IDUSER              PIC X(8).                                 
022100     03  WS-LISTA Occurs 45.                                              
022200         05  WS-FLAGGA-LISTA    PIC X.                                    
022300     03  FILLER                 PIC X(25) Value Space.                    
022400*                          SUMMA = 80 TECKEN                              
022500                                                                          
022600     EJECT                                                                
022700 01 WS20-AREA.                                                            
022800     03 WS20-IDDC           PIC X(2).                                     
022900*2                                                                        
023000     03 WS20-IDPERSON-FOM   PIC 9(3).                                     
023100*5                                                                        
023200     03 WS20-IDPERSON-TOM   PIC 9(3).                                     
023300*8                                                                        
023400     03 WS20-IDPERSON       PIC 9(3)  Occurs 5 Times.                     
023500*23                                                                       
023600     03 WS20-FL-IDBERED      PIC X(1).                                    
023700*24                                                                       
023800     03 WS20-FL-IDLEVNR-SHIP PIC X(1).                                    
023900*25                                                                       
024000     03 WS20-IDLEVNR        PIC X(5)  Occurs 5 Times.                     
024100*50                                                                       
024200     03 WS20-KDERS-FOM      PIC 9(2).                                     
024300*52                                                                       
024400     03 WS20-KDERS-TOM      PIC 9(2).                                     
024500*54                                                                       
024600     03 WS20-KDERS          PIC 9(2)  Occurs 5 Times.                     
024700*64                                                                       
024800     03 WS20-FLERS-VIPS     PIC X(1).                                     
024900*65                                                                       
025000     03  FILLER             PIC X(15).                                    
025100*                          SUMMA = 80 TECKEN                              
025200                                                                          
025300 01 FILLER           PIC X(24) Value 'SUMMERINGAR           '.            
025400 01  SUMMERINGAR.                                                         
025500                                                                          
025600     03  BRYTBEGREPP.                                                     
025700         05  OLD-IDANSK          PIC S9(3) COMP-3 Value Zero.             
025800         05  OLD-IDLEVNR         PIC X(5)  Value Space.                   
025900                                                                          
026000     03  RAKNARE.                                                         
026100         05  W-TOTSUM-MB-EXCEL    PIC S9(9) COMP-3 Value Zero.            
026200                                                                          
026300     03  W-KVART-TOT-C1          PIC S9(9) Value Zero COMP-3.             
026400*                                                                         
026500 01 FILLER           PIC X(24) Value 'ARBETSFÄLT KVPB-SUM-VV'.            
026600 01 ARBETSFAELT-KVPB-SUM-VV.                                              
026700     03  W-TIME             PIC   9(8)       Value Zero.                  
026800     03  W-VECKO-SEP-BEHOV  PIC  S9(7)V9(2)  Value Zero COMP-3.           
026900     03  W-DAG-SEP-BEHOV    PIC  S9(7)V9(2)  Value Zero COMP-3.           
027000     03  W-KVDAGAR-KVAR     PIC   9(3)       Value Zero COMP-3.           
027100     03  W-TIFINLV-AAVV     PIC  S9(5)                  COMP-3.           
027200     EJECT                                                                
027300**********************************************************                
027400*    O B S   ÄNDRAS DET HÄR SKALL DET EV. ÄNDRAS I                        
027500*            BA-INITIERA-RUBRIK   OCKSÅ.                                  
027600**********************************************************                
027700                                                                          
027800 01  W-ARBETS-AREOR.                                                      
027900*      ---- Fälten visas i denna ordning på listan                        
028000     03  WS-IDLEVNR              PIC X(5).                                
028100     03  WS-IDLEVNR-SHIP         PIC X(5).                                
028200     03  WS-IDARTNR              PIC Z(9).                                
028300     03  WS-TILEVBSK             PIC 9(5).                                
028400     03  W-KVAKS-SDC-PAV         PIC -(7).                                
028500     03  W-KVSLAGER              PIC -(7).                                
028600     03  W-TIMANSEC              PIC 9(6).                                
028700     03  W-KVREFOVL              PIC -(6)9.                               
028800     03  W-KVROS-BULK-DAG        PIC -(7).                                
028900     03  W-KVRORAD               PIC -(5).                                
029000     03  W-KVVORKO               PIC -(9).                                
029100     03  W-KVSLAP-SUM            PIC -(7).                                
029200     03  W-KVAVIS-NOT-REC        PIC -(7).                                
029300*         KVAVIS-NOT-REC är summan av alla ej mottagna avrop (R31)        
029400*                med senare TIAVIDAT än dagens datum (WDL221)             
029500*    AAVVD TILEVBSK-AVS (AAVVD) FRÅN WDD924 LEV-DALEVBSK-AVS              
029600                                                                          
029700     03  W-TIBORT-INFO           PIC 9(6).                                
029800*     AAVVD  Date on 2106     från WDD925   INFO-TIBORT                   
029900     03  W-KDPRODSL              PIC Z(2).                                
030000     03  W-IDFKNGRP              PIC Z(4).                                
030100     03  W-KVAVIS-FORAVIS        PIC -(8).                                
030200     03  W-LATASTE-INLEV.                                                 
030300        05 W-TIAVIDAT-LATE       PIC 9(6) Value Zero.                     
030400        05 FILLER                PIC X(1)  Value Space.                   
030500        05 W-IDKUNDRF-LATE       PIC X(10) Value Space.                   
030600        05 FILLER                PIC X(1)  Value Space.                   
030700        05 W-KVANTAL-LATE        PIC -(5)9 Value Zero.                    
030800*                       -"-  SUM LTH = 25                                 
030900     03  W-KVPB-REF              PIC -(6)9v,9.                            
031000     03  W-KVPB-PLAN             PIC -(6)9v,9.                            
031100     03  W-DAPBPLAN              PIC -(8).                                
031200     03  W-ADVICED               PIC -(8)9v,9.                            
031300     03  W-KVOIRULL              PIC Z(7)9.                               
031400     03  W-KVOI-YEAR-0           PIC Z(7)9.                               
031500     03  W-KVOI-YEAR-1           PIC Z(7)9.                               
031600     03  W-KVOI-YEAR-2           PIC Z(7)9.                               
031700     03  W-KVOI-YEAR-3           PIC z(7)9.                               
031800     03  W-KVOI-YEAR-4           PIC z(7)9.                               
031900     03  W-KVOI-YEAR-5           PIC z(7)9.                               
032000     03  W-KVVECKOR-LT           PIC Z(2).                                
032100     03  W-KVLS-KVRESS           PIC Z(06)9.                              
032200     03  W-PRMATRL               PIC -(6)9v,99.                           
032300     03  W-PRAVCOST              PIC -(6)9v,99.                           
032400     03  W-TIREFPAF              PIC -(7).                                
032500     03  W-KDLEVPLF              PIC X(1).                                
032600     03  W-FLJIT                 PIC X(1).                                
032700     03  W-TIFINLV               PIC 9(5).                                
032800     03  W-TIURPROD              PIC 9(5).                                
032900     03  W-DAPUBL                PIC -(8).                                
033000     03  W-TIREFSTO-GRP.                                                  
033100        05 W-TIREFSTO            PIC 9(6).                                
033200     03  W-KVPB-SUM-VV           PIC -(6)9v,9.                            
033300     03  W-KVVECKOR-RED          PIC Z9.                                  
033400     03  W-KVAVROP-SUM-VV        PIC -(8).                                
033500* -----NEDAN 4 FÄLT FRÅN VAL Styrparam. -----------                       
033600     03  W-KDPRISKL              PIC X(1).                                
033700     03  W-KDFREKKL              PIC X(1).                                
033800     03  W-IDREFTAB              PIC X(01).                               
033900* -----NEDAN 3 FÄLT FRÅN VAL Service. -----------                         
034000     03  W-SUINKORD              PIC -(18)9.                              
034100     03  W-SUAVBRP               PIC -(5)9v,99.                           
034200     03  W-RESERVG-NTO           PIC -(3)9v,99.                           
034300     03  W-KDSORT                PIC X(2).                                
034400* -----NEDAN 8 FÄLT FRÅN VAL Kvanter. -----------                         
034500*           KVEOQ       FRÅN CLAG-KVEOQ (läs wdk611 här)                  
034600     03  W-KVEOQ                 PIC -(7).                                
034700     03  W-KVREFBER              PIC -(7).                                
034800     03  W-KVPALL                PIC -(7).                                
034900     03  W-KVULOAD               PIC -(7).                                
035000* -----NEDAN 9 FÄLT FRÅN VAL Förp.Info.----------                         
035100     03  W-IDARTNR-EMBQ0         PIC Z(8).                                
035200     03  W-IDARTNR-EMBQ1         PIC Z(8).                                
035300     03  W-IDARTNR-EMBQ2         PIC Z(8).                                
035400     03  W-KDFORP                PIC -(5).                                
035500     03  W-BEFT                  PIC -(3).                                
035600     03  W-KVSPANT               PIC -(5)9.                               
035700     03  W-KDKRSTA               PIC 9.                                   
035800     03  W-IDKR                  PIC Z(4)9.                               
035900     03  W-KDERS                 PIC 99.                                  
036000     03  W-ADART-RED.                                                     
036100        05 W-ADLAGOMR            PIC Z9.                                  
036200        05 FILLER                PIC X(1) Value Space.                    
036300        05 W-ADGANG              PIC Z9.                                  
036400        05 FILLER                PIC X(1) Value Space.                    
036500        05 W-ADPLATS             PIC Z(04)9(1).                           
036600     03  W-IDINK                 PIC X(4).                                
036700     03  W-IDANSK                PIC Z(3).                                
036800     03  W-IDBERED               PIC Z(3).                                
036900     03  W-TILEVDAG-TAB.                                                  
037000         05 W-TILEVDAG-T OCCURS 5.                                        
037100           07 W-TILEVDAG         PIC -Z(1).                               
037200           07 W-TILEVDAG-SEM     PIC X(01).                               
037300     03  W-KVPB-TREND            PIC +(6)9.9.                             
037400     03  W-KVVECKOR-TREND        PIC 9(02).                               
037500     03  W-TIDATUM-TREND         PIC 9(06).                               
037600     03  W-VKART                 PIC Z(06)9.                              
037700     03  W-VLARTNTO              PIC Z(07)9.9.                            
037800     03  W-DASEASON              PIC Z(8).                                
037900                                                                          
038000     03  W-REASON-PLAN-TAB.                                               
038100         05 W-REASON-PLAN-T OCCURS 12.                                    
038200            07 W-RESEASON-PLAN   PIC Z(2)V,Z(2).                          
038300            07 W-RESEASON-SEM    PIC X(01).                               
038400                                                                          
038500     03  W-SUM-KVVORKO           PIC S9(9)   Value Zero COMP-3.           
038600     03  VOR-QUEUE-TAB.                                                   
038700         05 TAB-VOR-QUEUE OCCURS 500.                                     
038800            07 TAB-VOR-IDARTNR   PIC S9(09) COMP-3.                       
038900            07 TAB-VOR-KVVORKO   PIC S9(09) COMP-3.                       
039000     EJECT                                                                
039100                                                                          
039200 01  IN20-AREA-START    PIC X(24)   Value 'IN20-AREA-START  '.            
039300*    PARAMETRAR IN , FRÅN BILD 2422 VIA SOP                               
039400*    --- Här ligger SYSIN-data-rad-3 när Pgm-slingan körs.                
039500*01  AREA -COPY W2172010   -PRE IN20-                                     
039600                                                                          
039700     EJECT                                                                
039800                                                                          
039900 01  IN-PART-AREA-START PIC X(24)   Value 'IN-PART-AREA-START  '.         
040000*    DAGLAGERFILEN, HÄRIFRÅN HÄMTAS ARTIKELINFO TILL LISTAN               
040100                                                                          
040200*01  AREA -COPY W2172020   -PRE IN-PART-                                  
040300                                                                          
040400     EJECT                                                                
040500                                                                          
040600 01  W001-AREA-START    PIC X(24)   Value 'W001-AREA-START  '.            
040700 01  W001-HJALPAREOR.                                                     
040800*                                                                         
040900     03  W001-SKIP               PIC 9(3) COMP-3  Value 1.                
041000     SKIP2                                                                
041100*                                                                         
041200 01  W001-URVAL-RAD1.                                                     
041300     03  FILLER          PIC X(3) Value Space.                            
041400     03  FILLER          PIC X(18) Value 'Purch.Pl.interval:'.            
041500     03  FILLER          PIC X(2)  Value Space.                           
041600     03  W001-IDPERSON-FOM PIC Z(3) Value Zero.                           
041700     03  FILLER          PIC X     Value '-'.                             
041800     03  FILLER          PIC X     Value Space.                           
041900     03  W001-IDPERSON-TOM PIC Z(3) Value Zero.                           
042000     03  FILLER          PIC X(6)  Value Space.                           
042100*        -- Explicita val av AnskaffarID                                  
042200     03  FILLER          PIC X(15) Value 'Purch.Pl.list: '.               
042300     03  FILLER Occurs 5.                                                 
042400         05  W001-IDPERSON    PIC Z(2)9 Value Zero.                       
042500         05  W001-IDANSK-SEP PIC X(2) Value ', '.                         
042600     03  FILLER          PIC X(9)  Value Space.                           
042700     03  FILLER          PIC X(10) Value 'SORT.ALT  '.                    
042800     03  W001-SORTERING  PIC X(16) Value Space.                           
042900     03  FILLER          PIC X(12) Value Space.                           
043000*                        SUMMA 120 TKN                                    
043100                                                                          
043200                                                                          
043300 01  W001-URVAL-RAD2.                                                     
043400     03  FILLER           PIC X(37) Value Space.                          
043500     03  FILLER           PIC X(14) Value 'Suppl.ID list:'.               
043600     03  FILLER           PIC X(2) Value Space.                           
043700     03  FILLER  Occurs 9.                                                
043800         05  W001-IDLEVNR     PIC X(5)  Value '    -'.                    
043900         05  W001-IDLEVNR-SEP PIC X(2)  Value ', '.                       
044000     03  FILLER           PIC X(4) Value Space.                           
044100*                        SUMMA 120 TKN                                    
044200                                                                          
044300 01  W001-URVAL-RAD3.                                                     
044400     03  FILLER             PIC X(3)  Value Space.                        
044500     03  FILLER             PIC X(18) Value 'Sup.Code interval:'.         
044600     03  FILLER             PIC X(3)  Value Space.                        
044700     03  W001-KDERS-FOM     PIC Z(2)  Value Zero.                         
044800     03  FILLER             PIC X     Value '-'.                          
044900     03  FILLER             PIC X(2)  Value Space.                        
045000     03  W001-KDERS-TOM     PIC Z(2)  Value Zero.                         
045100     03  FILLER             PIC X(6)  Value Space.                        
045200*        -- Explicita val av ErsKod                                       
045300     03  FILLER             PIC X(16) Value 'Sup.Code list:'.             
045400     03  FILLER             PIC X     Value Space.                        
045500     03  FILLER Occurs 5.                                                 
045600         05  W001-KDERS         PIC 9(2)  Value Zero.                     
045700         05  W001-KDERS-SEP     PIC X(4)  Value ',   '.                   
045800     03  FILLER             PIC X(37) Value Space.                        
045900*                        SUMMA 120 TKN                                    
046000                                                                          
046100 01  W001-URVAL-RAD4.                                                     
046200     03  FILLER             PIC X(3) Value Space.                         
046300     03  FILLER             PIC X(18) Value 'Func.Grp.interval:'.         
046400     03  FILLER             PIC X     Value Space.                        
046500     03  W001-IDFKNGRP-FOM  PIC Z(4)  Value Zero.                         
046600     03  FILLER             PIC X     Value '-'.                          
046700     03  W001-IDFKNGRP-TOM  PIC Z(4)  Value Zero.                         
046800     03  FILLER             PIC X(6)  Value Space.                        
046900*        -- Explicita val av FunktionsgruppID                             
047000     03  FILLER             PIC X(15) Value 'Func.Grp list:'.             
047100     03  FILLER Occurs 4.                                                 
047200         05  W001-IDFKNGRP      PIC Z(3)9 Value Zero.                     
047300         05  W001-IDFKNGRP-SEP  PIC X(2)  Value ', '.                     
047400     03  FILLER             PIC X(51) Value Space.                        
047500*                        SUMMA 120 TKN                                    
047600                                                                          
047700                                                                          
047800 01  W001-URVAL-RAD5.                                                     
047900     03  FILLER             PIC X(3) Value Space.                         
048000     03  FILLER             PIC X(15) Value 'Pack.Type list:'.            
048100     03  FILLER             PIC X(1)  Value Space.                        
048200*        -- Explicita val av FörpackningsID                               
048300     03  FILLER Occurs 4.                                                 
048400         05  W001-BEFT          PIC 9(2)  Value Zero.                     
048500         05  W001-BEFT-SEP      PIC X(2)  Value ', '.                     
048600     03  FILLER             PIC X(2)  Value Space.                        
048700     03  FILLER             PIC X(15) Value 'Search(S)Desc: '.            
048800     03  W001-BEART-SOEK    PIC X(25) Value Space.                        
048900     03  FILLER             PIC X(2)  Value Space.                        
049000     03  FILLER             PIC X(7)  Value 'Calls  '.                    
049100     03  W001-VECKOR-AVROP  PIC Z(2)  Value Zero.                         
049200     03  FILLER             PIC X(6)  Value ' v.   '.                     
049300     03  FILLER             PIC X(11) Value 'Tot.Demand '.                
049400     03  W001-VECKOR-BEHOV  PIC Z(2)  Value Zero.                         
049500     03  FILLER             PIC X(3)  Value ' v.'.                        
049600     03  FILLER             PIC X(6) Value Space.                         
049700*                        SUMMA 120 TKN                                    
049800                                                                          
049900 01  W001-URVAL-RAD6.                                                     
050000     03  FILLER             PIC X(3) Value Space.                         
050100     03  FILLER             PIC X(18) Value 'Prod.Grp.interval:'.         
050200     03  FILLER             PIC X     Value Space.                        
050300     03  W001-KDPRODSL-FOM  PIC Z(4)  Value Zero.                         
050400     03  FILLER             PIC X     Value '-'.                          
050500     03  W001-KDPRODSL-TOM  PIC Z(4)  Value Zero.                         
050600     03  FILLER             PIC X(6)  Value Space.                        
050700*        -- Val av Parts Projekt-ID                                       
050800     03  FILLER             PIC X(14) Value 'Parts Project:'.             
050900     03  W001-IDPROJ-URV    PIC X(4)  Value Space.                        
051000     03  FILLER             PIC X(65) Value Space.                        
051100*                        SUMMA 120 TKN                                    
051200                                                                          
051300     EJECT                                                                
051400                                                                          
051500 01  W001-DETALJ.                                                         
051600     03  FILLER       PIC X(1)   Value Space.                             
051700     03  W001-ART-RAD PIC X(120) Value Space.                             
051800     EJECT                                                                
051900                                                                          
052000 01  W002-DETALJ.                                                         
052100     03  FILLER                  PIC X(01)   Value Space.                 
052200     03  W002-ART-RAD            PIC X(1174) Value Space.                 
052300     EJECT                                                                
052400                                                                          
052500 01  W-WRITE-PART-ON-EXCEL       PIC X(01)   Value Space.                 
052600*                                                                         
052700     SKIP2                                                                
052800 01  SORTWS-AREA-START           PIC X(24)   Value                        
052900                                  'SORTWS-AREA-START  '.                  
053000*01  AREA -COPY W21720S     -PRE SORTWS-                                  
053100 01  SORT-RETURN-X               PIC X(4)  Value Space.                   
053200     EJECT                                                                
053300                                                                          
053400 01  GENERELLA-SUBPROGRAM.                                                
053500     03  WDATKONV                PIC X(8)    Value 'WDATKONV'.            
053600     03  WZ20DAYS                PIC X(8)    Value 'WZ20DAYS'.            
053700     03  W222BHDC                PIC X(8)    Value 'W222BHDC'.            
053800     03  W271REFL                PIC X(8)    Value 'W271REFL'.            
053900     03  CBLTDLI                 PIC X(8)    Value 'CBLTDLI '.            
054000     03  FELLOG                  PIC X(8)    Value 'FELLOG  '.            
054100*                                                                         
054200*    --- PARAMETRAR TILL SUBPROGRAM WDATKONV "KONVERTERA DATUM"           
054300 01  FILLER                      PIC X(16)   VALUE 'WDATAREA'.            
054400*01 -COPY WDATAREA                                                        
054500     EJECT                                                                
054600*    --- PARAMETRAR TILL SUBPROGRAM WZ20DAYS "ADDERA DAGAR DATUM"         
054700 01  FILLER                      PIC X(16)   VALUE 'WZ20DAYS'.            
054800*01 -COPY WZ20DAYS                                                        
054900     EJECT                                                                
055000*    --- PARAMETRAR TILL W271REFL                                         
055100*01 -COPY W271REFL                                                        
055200     EJECT                                                                
055300*    *************************************                                
055400*    **  LINK-AREA  BEHOVSTABELL        **                                
055500*    *************************************                                
055600 01  FILLER                      PIC X(16)   VALUE 'W222L222'.            
055700*01  AREA  -COPY W222BHDC   -PRE LINK-.                                   
055800     EJECT                                                                
055900                                                                          
056000 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
056100     SKIP3                                                                
056200 01  FILLER                      PIC X(16)   VALUE 'CHKP'.                
056300 01  CHKP-VAR.                                                            
056400     03 CHKP-MSG-IO-AREA-LENGTH  PIC S9(9)   VALUE +32 COMP SYNC.         
056500     03 CHKP-MSG-IO-AREA         PIC X(32)   VALUE SPACE.                 
056600     03 CHKP-AREA-LENGTH         PIC S9(9)   VALUE +32 COMP SYNC.         
056700     03 CHKP-AREA                PIC X(32)   VALUE SPACE.                 
056800     03 CHKP-ANT                 PIC S9(3)   VALUE +0   COMP-3.           
056900     03 CHKP-MAX                 PIC S9(3)   VALUE +100 COMP-3.           
057000                                                                          
057100                                                                          
057200 01  NYCKLAR-TILL-DLI.                                                    
057300                                                                          
057400     03  W-WDF5KEY-MIN-X.                                                 
057500         05  W-IDLEVNR-MIN       PIC X(5)    Value Space.                 
057600         05  W-IDBENR-MIN        PIC X       Value LOW-VALUE.             
057700                                                                          
057800     03  W-WDF5KEY-MAX-X.                                                 
057900         05  W-IDLEVNR-MAX       PIC X(5)    Value Space.                 
058000         05  W-IDBENR-MAX        PIC X       Value HIGH-VALUE.            
058100                                                                          
058200     03  W-IDARTNR-X.                                                     
058300         05  W-IDARTNR           PIC S9(9)   Value Zero COMP-3.           
058400                                                                          
058500     03  W-IDDC-X.                                                        
058600         05  W-IDDC              PIC X(02)   Value Space.                 
058700                                                                          
058800     03  W-WDD901KY-X.                                                    
058900         05  W-IDARTNR-D9        PIC S9(9)   Value Zero COMP-3.           
059000         05  W-IDDC-D9           PIC X(2)    Value Space.                 
059100                                                                          
059200     03  W-IDLEVNR-X.                                                     
059300         05  W-IDLEVNR           PIC X(5)    Value Space.                 
059400                                                                          
059500     03  W-IDLEVBSK-X.                                                    
059600         05 W-IDLEVBSK           PIC S9(1)   VALUE ZERO  COMP-3.          
059700                                                                          
059800     03  W-KDAVROP-X.                                                     
059900         05  W-KDAVROP           PIC S9(1)   Value 2 COMP-3.              
060000                                                                          
060100     03  W-W6D1HSEQ-X.                                                    
060200         05  W-IDARTNR-HSEQ        PIC S9(9)   Value Zero  COMP-3.        
060300                                                                          
060400     03  W-W6H7B1KY-MIN-X.                                                
060500         05  W-IDARTNR-H7-MIN      PIC S9(9)   Value Zero COMP-3.         
060600         05  W-DAREGDAT-9KOMPL-MIN PIC  9(8)   Value Zeroes.              
060700         05  W-IDLEVNR-H7-MIN      PIC  X(5)   Value LOW-Value.           
060800         05  W-KVKRKNTR-MIN        PIC S9(1)   Value Zero COMP-3.         
060900*                                     =19 BYTES                           
061000*        05  FILLER                PIC  9(5)   Value Zeroes.              
061100*                                     =24 BYTES                           
061200     03  W-W6H7B1KY-MAX-X.                                                
061300         05  W-IDARTNR-H7-MAX      PIC S9(9)   Value Zero COMP-3.         
061400         05  W-DAREGDAT-9KOMPL-MAX PIC  9(8)   Value 99999999.            
061500         05  W-IDLEVNR-H7-MAX      PIC  X(5)   Value HIGH-Value.          
061600         05  W-KVKRKNTR-MAX        PIC S9(1)   Value +9 COMP-3.           
061700*                                     =19 BYTES                           
061800*        05  FILLER                PIC  9(5)   Value 99999.               
061900*                                     =24 BYTES                           
062000                                                                          
062100     03  W-KDKRSTA-MIN             PIC X(1)    Value '1'.                 
062200     03  W-KDKRSTA-MAX             PIC X(1)    Value '9'.                 
062300                                                                          
062400     03  W-WDA5A1KY-MIN.                                                  
062500         05  W-IDARTNR-MIN        PIC S9(9) COMP-3   VALUE ZERO.          
062600         05  W-IDDC-MIN           PIC X(2)  Value Space.                  
062700         05  W-FILLER-MIN         PIC X(33) VALUE LOW-VALUE.              
062800     03  W-KDSTARAD-MIN           PIC X     Value Space.                  
062900                                                                          
063000     03  W-WDA5A1KY-MAX.                                                  
063100         05  W-IDARTNR-MAX        PIC S9(9) COMP-3   VALUE ZERO.          
063200         05  W-IDDC-MAX           PIC X(2)  Value Space.                  
063300         05  W-FILLER-MAX         PIC X(33) VALUE HIGH-VALUE.             
063400     03  W-KDSTARAD-MAX           PIC X     Value Space.                  
063500                                                                          
063600     03  W-IDHTYP-X.                                                      
063700         05  W-IDHTYP            PIC X(4)     VALUE '4541'.               
063800         05  FILLER              PIC X(26)    VALUE LOW-VALUE.            
063900                                                                          
064000     03  W-KDVORATG-X.                                                    
064100         05  W-KDVORATG          PIC X       VALUE '2'.                   
064200*    --- IMS FUNKTIONSKODER                                               
064300*01  -COPY W0003                                                          
064400     EJECT                                                                
064500*    ---  DLI INPUT-OUTPUT AREA                                           
064600 01  FILLER                  PIC X(16) Value 'DLI-IO-WDF502'.             
064700     SKIP3                                                                
064800 01  DLI-IO-AREA-WDF5.                                                    
064900*        05  -COPY WDF502                                                 
065000     EJECT                                                                
065100 01  FILLER                  PIC X(16) Value 'DLI-IO-WDD9'.               
065200     SKIP3                                                                
065300 01  DLI-IO-AREA-WDD9.                                                    
065400     03  IO-AREA-WDD9        PIC X(100) Value Space.                      
065500     SKIP3                                                                
065600*    03  -COPY WDD902 -PRE WDD902- -RED IO-AREA-WDD9.                     
065700*    03  -COPY WDD905              -RED IO-AREA-WDD9.                     
065800*    03  -COPY WDD924              -RED IO-AREA-WDD9.                     
065900*    03  -COPY WDD925              -RED IO-AREA-WDD9.                     
066000     EJECT                                                                
066100 01  FILLER         PIC X(16) Value 'DLI-IO-WDK601'.                      
066200 01  DLI-IO-WDK601.                                                       
066300*    03  -COPY WDK601                                                     
066400     EJECT                                                                
066500 01  FILLER         PIC X(16) Value 'DLI-IO-WDK611'.                      
066600 01  DLI-IO-WDK611.                                                       
066700*    03  -COPY WDK611                                                     
066800     EJECT                                                                
066900 01  FILLER         PIC X(16) Value 'DLI-IO-WDK621'.                      
067000 01  DLI-IO-WDK621.                                                       
067100*    03  -COPY WDK621                                                     
067200     EJECT                                                                
067300 01  FILLER         PIC X(16) Value 'DLI-IO-W6D111'.                      
067400 01  DLI-IO-W6D111.                                                       
067500*    03  -COPY W6D111   -PRE W6D1-.                                       
067600     EJECT                                                                
067700 01  FILLER         PIC X(16) Value 'DLI-IO-W6H701'.                      
067800 01  DLI-IO-W6H701.                                                       
067900*    03  -COPY W6H701   -PRE W6H7-.                                       
068000     EJECT                                                                
068100 01  FILLER         PIC X(16) Value 'DLI-IO-WDA501'.                      
068200 01  DLI-IO-WDA5A.                                                        
068300*        05  -COPY WDA5A1                                                 
068400     EJECT                                                                
068500 01  DLI-IO-WDA501.                                                       
068600*        05  -COPY WDA501                                                 
068700     EJECT                                                                
068800 01  FILLER         PIC X(16)   VALUE 'DLI-IO-WDL601'.                    
068900*                                                                         
069000 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDL601'.                      
069100 01  DLI-IO-WDL601.                                                       
069200*    03  -COPY WDL601                                                     
069300*                                                                         
069400 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDL611'.                      
069500 01  DLI-IO-WDL611.                                                       
069600*    03  -COPY WDL611                                                     
069700*                                                                         
069800 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDGX4542'.                    
069900 01  DLI-IO-WDGX4542.                                                     
070000*    03  -COPY WDGX4542                                                   
070100     EJECT                                                                
070200                                                                          
070300*    --- STATUS-KOD FRÅN IMS                                              
070400 01  STATUS-WS                   PIC XX.                                  
070500     88  SEGMENT-FINNS                       Value '  '.                  
070600     88  SEGMENT-SLUT                        VALUE 'GB'.                  
070700     88  SEGMENT-FINNS-REDAN                 Value 'II'.                  
070800     88  IMS-EJ-OK                           VALUE 'XD'.                  
070900     88  SEGMENT-SAKNAS            Values ARE 'GE' 'GB'.                  
071000     SKIP2                                                                
071100 01  GODK-STATUSKODER.                                                    
071200     03  GODK-STATUS Occurs 5 Indexed By STATUS-IX PIC XX.                
071300     SKIP3                                                                
071400 01  SSA1                        PIC X(160).                              
071500 01  SSA2                        PIC X(128).                              
071600 01  SSA3                        PIC X(128).                              
071700     EJECT                                                                
071800 LINKAGE SECTION.                                                         
071900                                                                          
072000*01  -COPY W0009   -PRE MSG-                                              
072100     EJECT                                                                
072200*01  -COPY W0008  -PRE WDF5-                                              
072300     05  FILLER                   PIC X.                                  
072400     EJECT                                                                
072500*01  -COPY W0008  -PRE WDD9-                                              
072600     05  FILLER                   PIC X.                                  
072700     EJECT                                                                
072800*01  -COPY W0008  -PRE W6D1-                                              
072900     05  FILLER                   PIC X.                                  
073000     EJECT                                                                
073100*01  -COPY W0008  -PRE W6H7-                                              
073200     05  FILLER                   PIC X.                                  
073300     EJECT                                                                
073400*01  -COPY W0008  -PRE WDA5A-                                             
073500     05  FILLER                   PIC X.                                  
073600     EJECT                                                                
073700*-BEHOVSMODULENS PCB:ER i W222BHDC                                        
073800 01  BHDC-WDK6-PCB                PIC X.                                  
073900 01  BHDC-WDK7-PCB                PIC X.                                  
074000 01  BHDC-WDB6-PCB                PIC X.                                  
074100 01  BHDC-WDR2-PCB                PIC X.                                  
074200 01  BHDC-WDD7-PCB                PIC X.                                  
074300 01  BHDC-WDK7E-PCB               PIC X.                                  
074400 01  BHDC-WDD7-2-PCB              PIC X.                                  
074500 01  BHDC-WDK9-PCB                PIC X.                                  
074600 01  BHDC-REFL1-2501-PCB          PIC X.                                  
074700 01  BHDC-REFL1-WDB6-PCB          PIC X.                                  
074800 01  BHDC-REFL1-WDK7-PCB          PIC X.                                  
074900 01  BHDC-REFL1-UTIL-WDK6-PCB     PIC X.                                  
075000 01  BHDC-REFL1-UTIL-WDK7-PCB     PIC X.                                  
075100 01  BHDC-REFL1-UTIL-WDB6-PCB     PIC X.                                  
075200     EJECT                                                                
075300 01  BHDC-REFL2-2501-PCB          PIC X.                                  
075400 01  BHDC-REFL2-WDB6-PCB          PIC X.                                  
075500 01  BHDC-REFL2-UTIL-WDK6-PCB     PIC X.                                  
075600 01  BHDC-REFL2-UTIL-WDK7-PCB     PIC X.                                  
075700 01  BHDC-REFL2-UTIL-WDB6-PCB     PIC X.                                  
075800     EJECT                                                                
075900 01  BHDC-UTIL-WDK6-PCB           PIC X.                                  
076000 01  BHDC-UTIL-WDK7-PCB           PIC X.                                  
076100 01  BHDC-UTIL-WDB6-PCB           PIC X.                                  
076200     EJECT                                                                
076300 01  BHDC-W222-WDK6-PCB           PIC X.                                  
076400 01  BHDC-W222-WDK7-PCB           PIC X.                                  
076500 01  BHDC-W222-ARTM-PCB           PIC X.                                  
076600 01  BHDC-W222-2501-PCB           PIC X.                                  
076700 01  BHDC-W222-WDB6R-PCB          PIC X.                                  
076800 01  BHDC-W222-WDK7R-PCB          PIC X.                                  
076900 01  BHDC-W222-WDB6-PCB           PIC X.                                  
077000 01  BHDC-W222-WDD7-PCB           PIC X.                                  
077100 01  BHDC-W222-WDK7E-PCB          PIC X.                                  
077200 01  BHDC-W222-UTIL-WDK6-PCB      PIC X.                                  
077300 01  BHDC-W222-UTIL-WDK7-PCB      PIC X.                                  
077400 01  BHDC-W222-UTIL-WDB6-PCB      PIC X.                                  
077500 01  BHDC-W222-UTUP-WDK7-PCB      PIC X.                                  
077600 01  BHDC-W222-UTUP-WDB6-PCB      PIC X.                                  
077700 01  BHDC-W222-UTUP-UTIL-WDK6-PCB PIC X.                                  
077800 01  BHDC-W222-UTUP-UTIL-WDK7-PCB PIC X.                                  
077900 01  BHDC-W222-UTUP-UTIL-WDB6-PCB PIC X.                                  
078000     EJECT                                                                
078100 01  BHDC-UTUP-WDK7-PCB           PIC X.                                  
078200 01  BHDC-UTUP-WDB6-PCB           PIC X.                                  
078300 01  BHDC-UTUP-UTIL-WDK6-PCB      PIC X.                                  
078400 01  BHDC-UTUP-UTIL-WDK7-PCB      PIC X.                                  
078500 01  BHDC-UTUP-UTIL-WDB6-PCB      PIC X.                                  
078600     EJECT                                                                
078700* LATEST DEL                                                              
078800*01  -COPY W0008  -PRE WDL6-.                                             
078900     05  FILLER                   PIC X.                                  
079000     EJECT                                                                
079100* VOR QUEUE                                                               
079200*01  -COPY W0008  -PRE 4541-.                                             
079300     05  FILLER                   PIC X.                                  
079400     EJECT                                                                
079500                                                                          
079600* W271REFL                                                                
079700 01  REFL-2501-PCB                PIC X.                                  
079800 01  REFL-WDB6-PCB                PIC X.                                  
079900 01  REFL-WDK7-PCB                PIC X.                                  
080000     EJECT                                                                
080100 01  UTIL-WDK6-PCB                PIC X.                                  
080200 01  UTIL-WDK7-PCB                PIC X.                                  
080300 01  UTIL-WDB6-PCB                PIC X.                                  
080400                                                                          
080500 PROCEDURE DIVISION Using  MSG-PCB                                        
080600      WDF5-PCB WDD9-PCB W6D1-PCB W6H7-PCB WDA5A-PCB                       
080700      BHDC-WDK6-PCB   BHDC-WDK7-PCB                                       
080800      BHDC-WDB6-PCB   BHDC-WDR2-PCB                                       
080900      BHDC-WDD7-PCB   BHDC-WDK7E-PCB                                      
081000      BHDC-WDD7-2-PCB BHDC-WDK9-PCB                                       
081100      BHDC-REFL1-2501-PCB                                                 
081200      BHDC-REFL1-WDB6-PCB                                                 
081300      BHDC-REFL1-WDK7-PCB                                                 
081400      BHDC-REFL1-UTIL-WDK6-PCB                                            
081500      BHDC-REFL1-UTIL-WDK7-PCB                                            
081600      BHDC-REFL1-UTIL-WDB6-PCB                                            
081700      BHDC-REFL2-2501-PCB                                                 
081800      BHDC-REFL2-WDB6-PCB                                                 
081900      BHDC-REFL2-UTIL-WDK6-PCB                                            
082000      BHDC-REFL2-UTIL-WDK7-PCB                                            
082100      BHDC-REFL2-UTIL-WDB6-PCB                                            
082200      BHDC-UTIL-WDK6-PCB                                                  
082300      BHDC-UTIL-WDK7-PCB                                                  
082400      BHDC-UTIL-WDB6-PCB                                                  
082500      BHDC-W222-WDK6-PCB                                                  
082600      BHDC-W222-WDK7-PCB                                                  
082700      BHDC-W222-ARTM-PCB                                                  
082800      BHDC-W222-2501-PCB                                                  
082900      BHDC-W222-WDB6R-PCB                                                 
083000      BHDC-W222-WDK7R-PCB                                                 
083100      BHDC-W222-WDB6-PCB                                                  
083200      BHDC-W222-WDD7-PCB                                                  
083300      BHDC-W222-WDK7E-PCB                                                 
083400      BHDC-W222-UTIL-WDK6-PCB                                             
083500      BHDC-W222-UTIL-WDK7-PCB                                             
083600      BHDC-W222-UTIL-WDB6-PCB                                             
083700      BHDC-W222-UTUP-WDK7-PCB                                             
083800      BHDC-W222-UTUP-WDB6-PCB                                             
083900      BHDC-W222-UTUP-UTIL-WDK6-PCB                                        
084000      BHDC-W222-UTUP-UTIL-WDK7-PCB                                        
084100      BHDC-W222-UTUP-UTIL-WDB6-PCB                                        
084200      BHDC-UTUP-WDK7-PCB                                                  
084300      BHDC-UTUP-WDB6-PCB                                                  
084400      BHDC-UTUP-UTIL-WDK6-PCB                                             
084500      BHDC-UTUP-UTIL-WDK7-PCB                                             
084600      BHDC-UTUP-UTIL-WDB6-PCB                                             
084700      WDL6-PCB                                                            
084800      4541-PCB                                                            
084900      REFL-2501-PCB                                                       
085000      REFL-WDB6-PCB                                                       
085100      REFL-WDK7-PCB                                                       
085200      UTIL-WDK6-PCB                                                       
085300      UTIL-WDK7-PCB                                                       
085400      UTIL-WDB6-PCB.                                                      
085500                                                                          
085600     ENTRY 'DLITCBL' Using MSG-PCB                                        
085700      WDF5-PCB WDD9-PCB W6D1-PCB W6H7-PCB WDA5A-PCB                       
085800      BHDC-WDK6-PCB   BHDC-WDK7-PCB                                       
085900      BHDC-WDB6-PCB   BHDC-WDR2-PCB                                       
086000      BHDC-WDD7-PCB   BHDC-WDK7E-PCB                                      
086100      BHDC-WDD7-2-PCB BHDC-WDK9-PCB                                       
086200      BHDC-REFL1-2501-PCB                                                 
086300      BHDC-REFL1-WDB6-PCB                                                 
086400      BHDC-REFL1-WDK7-PCB                                                 
086500      BHDC-REFL1-UTIL-WDK6-PCB                                            
086600      BHDC-REFL1-UTIL-WDK7-PCB                                            
086700      BHDC-REFL1-UTIL-WDB6-PCB                                            
086800      BHDC-REFL2-2501-PCB                                                 
086900      BHDC-REFL2-WDB6-PCB                                                 
087000      BHDC-REFL2-UTIL-WDK6-PCB                                            
087100      BHDC-REFL2-UTIL-WDK7-PCB                                            
087200      BHDC-REFL2-UTIL-WDB6-PCB                                            
087300      BHDC-UTIL-WDK6-PCB                                                  
087400      BHDC-UTIL-WDK7-PCB                                                  
087500      BHDC-UTIL-WDB6-PCB                                                  
087600      BHDC-W222-WDK6-PCB                                                  
087700      BHDC-W222-WDK7-PCB                                                  
087800      BHDC-W222-ARTM-PCB                                                  
087900      BHDC-W222-2501-PCB                                                  
088000      BHDC-W222-WDB6R-PCB                                                 
088100      BHDC-W222-WDK7R-PCB                                                 
088200      BHDC-W222-WDB6-PCB                                                  
088300      BHDC-W222-WDD7-PCB                                                  
088400      BHDC-W222-WDK7E-PCB                                                 
088500      BHDC-W222-UTIL-WDK6-PCB                                             
088600      BHDC-W222-UTIL-WDK7-PCB                                             
088700      BHDC-W222-UTIL-WDB6-PCB                                             
088800      BHDC-W222-UTUP-WDK7-PCB                                             
088900      BHDC-W222-UTUP-WDB6-PCB                                             
089000      BHDC-W222-UTUP-UTIL-WDK6-PCB                                        
089100      BHDC-W222-UTUP-UTIL-WDK7-PCB                                        
089200      BHDC-W222-UTUP-UTIL-WDB6-PCB                                        
089300      BHDC-UTUP-WDK7-PCB                                                  
089400      BHDC-UTUP-WDB6-PCB                                                  
089500      BHDC-UTUP-UTIL-WDK6-PCB                                             
089600      BHDC-UTUP-UTIL-WDK7-PCB                                             
089700      BHDC-UTUP-UTIL-WDB6-PCB                                             
089800      WDL6-PCB                                                            
089900      4541-PCB                                                            
090000      REFL-2501-PCB                                                       
090100      REFL-WDB6-PCB                                                       
090200      REFL-WDK7-PCB                                                       
090300      UTIL-WDK6-PCB                                                       
090400      UTIL-WDK7-PCB                                                       
090500      UTIL-WDB6-PCB.                                                      
090600                                                                          
090700     Perform A-INIT                                                       
090800                                                                          
090900     Sort SORTFIL ASCENDING KEY SORT-IDARTNR                              
091000                  INPUT  PROCEDURE C-SORT-INPUT                           
091100                  OUTPUT PROCEDURE D-SORT-OUTPUT                          
091200                                                                          
091300     If SORT-RETURN Not = 0 And AVBRYTES-EJ                               
091400       move SORT-RETURN To SORT-RETURN-X                                  
091500       String 'RETURKOD ' SORT-RETURN-X ' FRÅN SORT'                      
091600       Delimited By Size Into FELTEXT-STR                                 
091700       Display FELTEXT                                                    
091800       Perform S99-ABEND                                                  
091900     Else                                                                 
092000       Perform Z-FINIT                                                    
092100                                                                          
092200       Move Zero To RETURN-CODE                                           
092300       GOBACK                                                             
092400     End-If                                                               
092500     .                                                                    
092600     EJECT                                                                
092700 A-INIT SECTION.                                                          
092800     MOVE 'A-INIT              '     TO CURRENT-SECTION                   
092900                                                                          
093000     OPEN INPUT  W2172010                                                 
093100                 W2172020                                                 
093200     OPEN OUTPUT W21720-001                                               
093300                                                                          
093400     Move "IDAG  "                To DAT-KDDATFORM                        
093500     Call WDATKONV Using DAT-KDDATFORM DAT-I-TIDATUM                      
093600                         DAT-O-TIDATUM DAT-KDSVAR                         
093700     If DAT-KDSVAR-OK                                                     
093800        Move DAT-TIAAMMDD         To DAGENS-TIDATUM                       
093900        Move DAT-TISEKEL          To DAGENS-SEKEL                         
094000                                     DAGENS-DADATUM-SEKEL                 
094100        Move DAT-TIAA             To DAGENS-AA                            
094200        Move DAT-TIAAVVD-GRP(3:3) To DAGENS-VVD                           
094300        Move DAT-TID              To LINK-TID-AKTUELL                     
094400     Else                                                                 
094500        Move 'FEL FRÅN DATKONV - DAGENS DATUM' To FELTEXT                 
094600        Call FELLOG                                                       
094700     End-If                                                               
094800     Move IDPGM                   To POSTSUM-PROGNAMN                     
094900                                                                          
095000     Move FUNCTION CURRENT-DATE (1:4) To WS-YEAR                          
095100     Compute OI-ARTAL-0 = WS-YEAR - 0                                     
095200     Compute OI-ARTAL-1 = WS-YEAR - 1                                     
095300     Compute OI-ARTAL-2 = WS-YEAR - 2                                     
095400     Compute OI-ARTAL-3 = WS-YEAR - 3                                     
095500     Compute OI-ARTAL-4 = WS-YEAR - 4                                     
095600     Compute OI-ARTAL-5 = WS-YEAR - 5                                     
095700                                                                          
095800**********************************************                            
095900*    HÄR LÄSES PARAMETRARNA FRÅN BILD 2422 IN                             
096000**********************************************                            
096100                                                                          
096200*    --- LÄS INDATA RAD 1 FRÅN 2422                                       
096300     Perform S01-LAES-W2172010                                            
096400     If Not END-OF-W2172010                                               
096500                                                                          
096600       Move IN20-RAD1 To WS-AREA-PARM1                                    
096700     Else                                                                 
096800       Move 'PARAMETRAR FRÅN SOP SAKNAS '                                 
096900             To FELTEXT-STR                                               
097000       Display FELTEXT                                                    
097100       Perform S99-ABEND                                                  
097200     End-If                                                               
097300                                                                          
097400*    --- LÄS INDATA RAD 2 FRÅN 2422  = URVAL1                             
097500     Perform S01-LAES-W2172010                                            
097600     If Not END-OF-W2172010                                               
097700                                                                          
097800       Move IN20-RAD2 To WS20-AREA                                        
097900                                                                          
098000     Else                                                                 
098100       Move 'PARAMETERRAD 2 OCH 3 FRÅN SOP SAKNAS '                       
098200             To FELTEXT-STR                                               
098300       Display FELTEXT                                                    
098400       Perform S99-ABEND                                                  
098500     End-If                                                               
098600                                                                          
098700     Display '*********** PARAMETRAR:   '                                 
098800     Display WS-AREA-PARM1                                                
098900     Display IN20-RAD2                                                    
099000                                                                          
099100*    --- LÄS INDATA RAD 3 FRÅN 2422                                       
099200     Perform S01-LAES-W2172010                                            
099300     If Not END-OF-W2172010                                               
099400*                                                                         
099500*      Rad 3 ligger nu kvar i IN20-arean                                  
099600*                                                                         
099700       If IN20-BEART-SOEK > Space                                         
099800         Move IN20-BEART-SOEK       To BEART-BESORD                       
099900         Move Zero                  To BEART-DIFAELT                      
100000                                                                          
100100         Inspect BEART-BESORD Tallying BEART-DIFAELT                      
100200                For Characters Before Initial 5-Space                     
100300       Else                                                               
100400         Move Space                 To BEART-BESORD                       
100500       End-If                                                             
100600     Else                                                                 
100700       Move 'PARAMETERRAD 3 FRÅN SOP SAKNAS '                             
100800             To FELTEXT-STR                                               
100900       Display FELTEXT                                                    
101000       Perform S99-ABEND                                                  
101100                                                                          
101200     End-If                                                               
101300                                                                          
101400     Display IN20-RAD3                                                    
101500     Display '*********** '                                               
101600     Move Space To W001-IDANSK-SEP (5)                                    
101700                   W001-IDLEVNR-SEP (9)                                   
101800                   W001-KDERS-SEP (5)                                     
101900                   W001-IDFKNGRP-SEP (4)                                  
102000                   W001-BEFT-SEP (4)                                      
102100                                                                          
102200     If IN20-KVVECKOR-AVROP > Zero                                        
102300       Perform AA-BERAKNA-SLUTVECKA-AVROP                                 
102400     End-If                                                               
102500     If IN20-KVVECKOR-KVPB  > Zero                                        
102600       Perform AB-BERAKNA-STARTVECKA-KVPB                                 
102700     End-If                                                               
102800*     -- Initierar BMP                                                    
102900     PERFORM IMS-RESTART                                                  
103000     .                                                                    
103100     EJECT                                                                
103200                                                                          
103300 AA-BERAKNA-SLUTVECKA-AVROP   SECTION.                                    
103400     MOVE 'AA-BERAKNA-SLUTVECKA-AVROP' TO CURRENT-SECTION                 
103500                                                                          
103600*                                                                         
103700*  Räkna ut antal dagar i kommande veckor, + de i denna veckan            
103800     Compute WS-ANTAL-URVALSDAGAR =                                       
103900     (( IN20-KVVECKOR-AVROP - 1 ) * 7 ) + 6 - DAGENS-TILEVDAG             
104000*                                                                         
104100                                                                          
104200*  Bestäm sedan vilken slutvecka det blir efter alla dessa dagar.         
104300     Move "YYMMDD"             To DAYS-KDDATFMT1                          
104400     Move DAGENS-TIDATUM       To DAYS-TIDATE1                            
104500                                                                          
104600     Move "YYWW"               To DAYS-KDDATFMT2                          
104700     Move Space                To DAYS-IDCALEND                           
104800                                  DAYS-TIDATE2                            
104900     Move WS-ANTAL-URVALSDAGAR To DAYS-KVDAYS                             
105000                                                                          
105100     Call WZ20DAYS Using DAYS-WZ20DAYS                                    
105200*                                                                         
105300     If DAYS-KDRC = +0                                                    
105400       Move 20                 To SLUT-SEKEL                              
105500       Move DAYS-TIDATE2(1:4)  To SLUT-AAVV                               
105600*      Move 5                  To SLUT-DAG                                
105700*                                                                         
105800     Else                                                                 
105900       DISPLAY 'FELKOD: ' DAYS-KDRC                                       
106000       String 'FEL I WZ20DAYS, ' DAGENS-TIDATUM ' + '                     
106100              IN20-KVVECKOR-AVROP  ' VECKOR (= '                          
106200              WS-ANTAL-URVALSDAGAR ' DAGAR)'                              
106300              Delimited By Size Into FELTEXT-STR                          
106400              Display FELTEXT                                             
106500              Perform S99-ABEND                                           
106600     End-If                                                               
106700                                                                          
106800     .                                                                    
106900     EJECT                                                                
107000                                                                          
107100 AB-BERAKNA-STARTVECKA-KVPB   SECTION.                                    
107200     MOVE 'AB-BERAKNA-STARTVECKA-KVPB' TO CURRENT-SECTION                 
107300                                                                          
107400     Move "YYMMDD"             To DAYS-KDDATFMT1                          
107500     Move DAGENS-TIDATUM       To DAYS-TIDATE1                            
107600     Move "YYWW"               To DAYS-KDDATFMT2                          
107700     Move 7                    To DAYS-KVDAYS                             
107800     Move Space                To DAYS-TIDATE2                            
107900     Call WZ20DAYS Using DAYS-WZ20DAYS                                    
108000     If DAYS-KDRC = Zero                                                  
108100       Move DAYS-TIDATE2(1:4)  To LINK-TIBEHOV-START                      
108200                                  LINK-TIAAVV-AKTUELL                     
108300     Else                                                                 
108400       String 'FEL I WZ20DAYS, ' DAGENS-TIDATUM ' + 7 dagar'              
108500              Delimited By Size Into FELTEXT-STR                          
108600     End-If                                                               
108700     .                                                                    
108800     EJECT                                                                
108900                                                                          
109000 C-SORT-INPUT  SECTION.                                                   
109100     MOVE 'C-SORT-INPUT              ' TO CURRENT-SECTION                 
109200                                                                          
109300****************************************************************          
109400*    HÄR LÄSES DAGLAGERBANDET IN                                          
109500*        OM ART UPPFYLLER URVALET SÅ SKAPAS SORTFIL                       
109600*    Här läses och skrives också sortposter för multipla                  
109700*    förkomster av KR-info samt Lev.besked                                
109800*    för vilka de vill ha en rad i output för varje                       
109900*    förekomst.                                                           
110000****************************************************************          
110100     Perform S02-LAES-W2172020                                            
110200     Perform S03-LAES-VOR-QUEUE                                           
110300     Perform Until END-OF-W2172020                                        
110400       IF IN-PART-IDDC = WS20-IDDC                                        
110500      AND IN-PART-IDDC-REF = Space                                        
110600         Perform CA-KOLLA-MOT-URVALET                                     
110700         If URVAL-OK                                                      
110800           Move IN-PART-IDARTNR To SORTWS-IDARTNR                         
110900           Perform CC-SKAPA-SORTFIL                                       
111000*          -- Nu kontrolleras val-fält som kan ge multipla rader          
111100*          -- Ifall inget av de är valda,skrivs SORT-posten direkt        
111200           Move Zero  To SORTWS-IDKR                                      
111300                         SORTWS-TILEVBSK-AVS                              
111400                         SORTWS-KVAVIS-BSKKVAR                            
111500                         SORTWS-TILEVBSK-INL-DC                           
111600                         SORTWS-TILEVBSK-DISP-DC                          
111700                         SORTWS-TIBORT-INFO                               
111800           Move Space To SORTWS-KDKRSTA                                   
111900           If  WS-FLAGGA-LISTA(08) = Space                                
112000           And WS-FLAGGA-LISTA(45) = Space                                
112100*            lista 08 = Del.Prom                                          
112200*            lista 45 = IR-status                                         
112300             Perform S31-SORT-RELEASE                                     
112400           Else                                                           
112500             If WS-FLAGGA-LISTA(08) > Space                               
112600               Perform C1-BEH-MULT-LEVBESK                                
112700*              Lev.Besk (multipla rader möjliga)                          
112800*              SORT-RELEASE görs här minst en gång                        
112900             Else                                                         
113000               If WS-FLAGGA-LISTA(45) > Space                             
113100                 Perform C2-BEH-MULT-KR-STATUS                            
113200*                KR (multipla rader möjliga)                              
113300*                SORT-RELEASE görs här minst en gång                      
113400               End-If                                                     
113500             End-If                                                       
113600           End-If                                                         
113700         End-If                                                           
113800       End-If                                                             
113900       Perform S02-LAES-W2172020                                          
114000     End-Perform                                                          
114100     .                                                                    
114200     EJECT                                                                
114300*                                                                         
114400 C1-BEH-MULT-LEVBESK  SECTION.                                            
114500     MOVE 'C1-BEH-MULT-LEVBESK       ' TO CURRENT-SECTION                 
114600                                                                          
114700     Move IN-PART-IDARTNR            To W-IDARTNR-D9                      
114800     Move IN-PART-IDDC               To W-IDDC-D9                         
114900     IF WS20-FL-IDLEVNR-SHIP = 'X'                                        
115000       MOVE IN-PART-IDLEVNR-SHIP     To W-IDLEVNR                         
115100     ELSE                                                                 
115200       Move IN-PART-IDLEVNR          To W-IDLEVNR                         
115300     END-IF                                                               
115400                                                                          
115500     Perform IMS-GU-WDD902                                                
115600*      -- Hämta Ext-datum för något av IDLEVBSK +2 +4 +5 +6               
115700     If SEGMENT-FINNS                                                     
115800                                                                          
115900       PERFORM S2-LAES-OCH-BEH-EXTINFO                                    
116000       PERFORM S4-LAES-OCH-BEH-EXTINFO2                                   
116100       PERFORM S5-LAES-OCH-BEH-EXTINFO3                                   
116200       PERFORM S6-LAES-OCH-BEH-EXTINFO4                                   
116300                                                                          
116400       Perform IMS-GU-WDD902                                              
116500*      -- Hämta Leverans-info                                             
116600       Perform IMS-GNP-WDD924                                             
116700       If SEGMENT-FINNS                                                   
116800         Perform Until SEGMENT-SAKNAS                                     
116900           Move LEV-DALEVBSK-AVS     To WS-DALEVBSK-AVS                   
117000           Move WS-DALEVBSK-AAMMDD   To DAT-I-TIDATUM                     
117100           Perform S22-DATUMKONV-TILL-AAVVD                               
117200           If DAT-KDSVAR-OK                                               
117300              Move DAT-TIAAVVD       To SORTWS-TILEVBSK-AVS               
117400           Else                                                           
117500              Move Zero              To SORTWS-TILEVBSK-AVS               
117600           End-If                                                         
117700                                                                          
117800           Move LEV-KVAVIS-BSKKVAR   To SORTWS-KVAVIS-BSKKVAR             
117900                                                                          
118000           If LEV-TILEVBSK-INL > 0                                        
118100              Move LEV-TILEVBSK-INL  To DAT-I-TIDATUM                     
118200              Perform S22-DATUMKONV-TILL-AAVVD                            
118300              If DAT-KDSVAR-OK                                            
118400                 Move DAT-TIAAVVD    To SORTWS-TILEVBSK-INL-DC            
118500              Else                                                        
118600                 Move 99999          To SORTWS-TILEVBSK-INL-DC            
118700              End-If                                                      
118800           Else                                                           
118900              Move Zero              To SORTWS-TILEVBSK-INL-DC            
119000           End-If                                                         
119100                                                                          
119200           If LEV-TILEVBSK-DISP > 0                                       
119300              Move LEV-TILEVBSK-DISP To DAT-I-TIDATUM                     
119400              Perform S22-DATUMKONV-TILL-AAVVD                            
119500              If DAT-KDSVAR-OK                                            
119600                 Move DAT-TIAAVVD    To SORTWS-TILEVBSK-DISP-DC           
119700              Else                                                        
119800                 Move 99999          To SORTWS-TILEVBSK-DISP-DC           
119900              End-If                                                      
120000           Else                                                           
120100              Move Zero              To SORTWS-TILEVBSK-DISP-DC           
120200           End-If                                                         
120300                                                                          
120400           Perform S31-SORT-RELEASE                                       
120500                                                                          
120600           Perform IMS-GNP-WDD924                                         
120700         End-Perform                                                      
120800       Else                                                               
120900*        -- Inget Lev.besk.  Skriv övr.info utan Lev-Besk-info            
121000         Perform S31-SORT-RELEASE                                         
121100       End-If                                                             
121200     Else                                                                 
121300*      -- Ej träff på rätt Lev. Skriv övr.info utan Lev-Besk-info         
121400       Perform S31-SORT-RELEASE                                           
121500     End-If                                                               
121600     .                                                                    
121700     EJECT                                                                
121800 C2-BEH-MULT-KR-STATUS  SECTION.                                          
121900     MOVE 'C2-BEH-MULT-KR-STATUS     ' TO CURRENT-SECTION                 
122000                                                                          
122100*    -- Kompl. med IDKR och KDKRSTA och "releasa" SORTWS                  
122200*    -- för varje IDKR enligt urval och per art. och lev.                 
122300     If WS-FLAGGA-LISTA(45) Numeric                                       
122400*      -- Beställaren vill ha en speciell KR status                       
122500       Move WS-FLAGGA-LISTA(45) To                                        
122600                 W-KDKRSTA-MIN W-KDKRSTA-MAX SORTWS-KDKRSTA               
122700     Else                                                                 
122800*      -- Beställaren ska ha alla KR status för ART och LEV               
122900       Move '1' To W-KDKRSTA-MIN                                          
123000       Move '9' To W-KDKRSTA-MAX                                          
123100     End-If                                                               
123200*    -- Sök med Art. och Lev. som gäller för nuv. post                    
123300     Move SORTWS-IDARTNR        To W-IDARTNR-H7-MIN                       
123400                                   W-IDARTNR-H7-MAX                       
123500     IF WS20-FL-IDLEVNR-SHIP = 'X'                                        
123600       Move SORTWS-IDLEVNR-SHIP To W-IDLEVNR-H7-MIN                       
123700                                   W-IDLEVNR-H7-MAX                       
123800     ELSE                                                                 
123900       Move SORTWS-IDLEVNR      To W-IDLEVNR-H7-MIN                       
124000                                   W-IDLEVNR-H7-MAX                       
124100     END-IF                                                               
124200                                                                          
124300     Perform IMS-GU-W6H701-SEQB                                           
124400                                                                          
124500     Set ARTIKEL-MED-KR-EJ-SKRIVEN To True                                
124600     Perform Until SEGMENT-SAKNAS                                         
124700*      -- KR-träff. Kolla först om Urval och DC är rätt                   
124800       If W6H7-KR-KDKRSTA  >= W-KDKRSTA-MIN                               
124900       And W6H7-KR-KDKRSTA <= W-KDKRSTA-MAX                               
125000       And W6H7-KR-IDDC     = IN-PART-IDDC                                
125100         Move W6H7-KR-IDKR    To SORTWS-IDKR                              
125200         Move W6H7-KR-KDKRSTA To SORTWS-KDKRSTA                           
125300         Perform S31-SORT-RELEASE                                         
125400         Set ARTIKEL-MED-KR-SKRIVEN To True                               
125500       End-If                                                             
125600       Perform IMS-GN-W6H701-SEQB                                         
125700     End-Perform                                                          
125800                                                                          
125900     If ARTIKEL-MED-KR-EJ-SKRIVEN                                         
126000*      -- Ingen KR-träff. Skriv ut raden utan KR                          
126100       Perform S31-SORT-RELEASE                                           
126200     End-If                                                               
126300     .                                                                    
126400     EJECT                                                                
126500                                                                          
126600 CA-KOLLA-MOT-URVALET    SECTION.                                         
126700     MOVE 'CA-KOLLA-MOT-URVALET      ' TO CURRENT-SECTION                 
126800                                                                          
126900     Move YES To URVAL-SW                                                 
127000* KOLLA IDPROJ-URVAL                                                      
127100     If URVAL-OK                                                          
127200       If IN20-IDPROJ-URV > Space                                         
127300         If IN-PART-IDPROJ = IN20-IDPROJ-URV                              
127400            Continue                                                      
127500         Else                                                             
127600            Move NOO To URVAL-SW                                          
127700         End-If                                                           
127800       End-If                                                             
127900     End-If                                                               
128000                                                                          
128100* KOLLA PRODUKTSLAGSURVAL                                                 
128200     If URVAL-OK                                                          
128300       If IN20-KDPRODSL-FOM > Zero                                        
128400         If ((IN-PART-KDPRODSL Not < IN20-KDPRODSL-FOM )                  
128500         And (IN-PART-KDPRODSL Not > IN20-KDPRODSL-TOM ))                 
128600            Continue                                                      
128700         Else                                                             
128800            Move NOO To URVAL-SW                                          
128900         End-If                                                           
129000       End-If                                                             
129100     End-If                                                               
129200                                                                          
129300* KOLLA URVAL ANSKAFFARID/BEREDARID                                       
129400     If WS20-FL-IDBERED = 'X'                                             
129500        MOVE IN-PART-IDBERED TO WS-PART-IDPERSON                          
129600     ELSE                                                                 
129700        MOVE IN-PART-IDANSK  TO WS-PART-IDPERSON                          
129800     END-IF                                                               
129900     If WS20-IDPERSON-FOM > Zero                                          
130000        If WS-PART-IDPERSON Not < WS20-IDPERSON-FOM And                   
130100           WS-PART-IDPERSON Not > WS20-IDPERSON-TOM                       
130200           Continue                                                       
130300        Else                                                              
130400           Move NOO To URVAL-SW                                           
130500        End-If                                                            
130600     Else                                                                 
130700       If WS20-IDPERSON (1) > Zero Or WS20-IDPERSON (2) > Zero Or         
130800          WS20-IDPERSON (3) > Zero Or WS20-IDPERSON (4) > Zero Or         
130900          WS20-IDPERSON (5) > Zero                                        
131000           Move +1 To IX                                                  
131100           Move NOO To TRAEFF-SW                                          
131200           Perform Until IX > 5 Or TRAEFF-OK                              
131300              If WS-PART-IDPERSON = WS20-IDPERSON (IX) And                
131400                 WS20-IDPERSON (IX) > Zero                                
131500                 Move YES To TRAEFF-SW                                    
131600              End-If                                                      
131700              Add +1 To IX                                                
131800           End-Perform                                                    
131900           If TRAEFF-NOO                                                  
132000              Move NOO To URVAL-SW                                        
132100           End-If                                                         
132200       End-If                                                             
132300     End-If                                                               
132400                                                                          
132500* KOLLA URVAL LEVERANTÖRSID                                               
132600     If URVAL-OK                                                          
132700       If WS20-IDLEVNR (1) > Space Or WS20-IDLEVNR (2) > Space            
132800       Or WS20-IDLEVNR (3) > Space Or WS20-IDLEVNR (4) > Space            
132900       Or WS20-IDLEVNR (5) > Space                                        
133000           Move +1 To IX                                                  
133100           Move NOO To TRAEFF-SW                                          
133200           Perform Until IX > 5  Or TRAEFF-OK                             
133300             IF WS20-FL-IDLEVNR-SHIP = 'X'                                
133400               If IN-PART-IDLEVNR-SHIP = WS20-IDLEVNR (IX) And            
133500                  WS20-IDLEVNR (IX) > Space                               
133600                  Move YES To TRAEFF-SW                                   
133700               End-If                                                     
133800             ELSE                                                         
133900               If IN-PART-IDLEVNR = WS20-IDLEVNR (IX) And                 
134000                  WS20-IDLEVNR (IX) > Space                               
134100                  Move YES To TRAEFF-SW                                   
134200               End-If                                                     
134300             End-If                                                       
134400             Add +1 To IX                                                 
134500           End-Perform                                                    
134600           If TRAEFF-NOO                                                  
134700              Move NOO To URVAL-SW                                        
134800           End-If                                                         
134900       End-If                                                             
135000     End-If                                                               
135100                                                                          
135200* KOLLA ERSÄTTNINGSKOD URVAL                                              
135300     If URVAL-OK                                                          
135400       If WS20-KDERS-FOM < 99                                             
135500          If IN-PART-KDERS Not < WS20-KDERS-FOM And                       
135600             IN-PART-KDERS Not > WS20-KDERS-TOM                           
135700              Continue                                                    
135800          Else                                                            
135900             Move NOO To URVAL-SW                                         
136000          End-If                                                          
136100       Else                                                               
136200         If WS20-KDERS (1) < 99 Or WS20-KDERS (2) < 99 Or                 
136300            WS20-KDERS (3) < 99 Or WS20-KDERS (4) < 99 Or                 
136400            WS20-KDERS (5) < 99                                           
136500             Move +1 To IX                                                
136600             Move NOO To TRAEFF-SW                                        
136700             Perform Until IX > 5 Or TRAEFF-OK                            
136800                If IN-PART-KDERS = WS20-KDERS (IX)                        
136900                   Move YES To TRAEFF-SW                                  
137000                End-If                                                    
137100                Add +1 To IX                                              
137200             End-Perform                                                  
137300             If TRAEFF-NOO                                                
137400                Move NOO To URVAL-SW                                      
137500             End-If                                                       
137600         End-If                                                           
137700       End-If                                                             
137800     End-If                                                               
137900                                                                          
138000* KOLLA SS-CODE LOC                                                       
138100     If URVAL-OK                                                          
138200       If WS20-FLERS-VIPS = YES OR JA                                     
138300          IF IN-PART-TIERSDAT-VIPS = +0                                   
138400             Move NOO To URVAL-SW                                         
138500          End-If                                                          
138600       Else                                                               
138700          If WS20-FLERS-VIPS = NOO                                        
138800             IF IN-PART-TIERSDAT-VIPS > +0                                
138900                Move NOO To URVAL-SW                                      
139000             End-If                                                       
139100          End-If                                                          
139200       End-If                                                             
139300     End-If                                                               
139400                                                                          
139500* KOLLA FUNKTIONSGRUPPSID URVAL                                           
139600     If URVAL-OK                                                          
139700       If IN20-IDFKNGRP-FOM > Zero                                        
139800          If IN-PART-IDFKNGRP Not < IN20-IDFKNGRP-FOM And                 
139900             IN-PART-IDFKNGRP Not > IN20-IDFKNGRP-TOM                     
140000             Continue                                                     
140100          Else                                                            
140200             Move NOO To URVAL-SW                                         
140300          End-If                                                          
140400       Else                                                               
140500         If IN20-IDFKNGRP(1) > Zero Or IN20-IDFKNGRP(2) > Zero Or         
140600            IN20-IDFKNGRP(3) > Zero Or IN20-IDFKNGRP(4) > Zero            
140700             Move +1 To IX                                                
140800             Move NOO To TRAEFF-SW                                        
140900                                                                          
141000             Perform Until IX > 4 Or TRAEFF-OK                            
141100                If IN-PART-IDFKNGRP = IN20-IDFKNGRP(IX) And               
141200                   IN20-IDFKNGRP(IX) > Zero                               
141300                   Move YES To TRAEFF-SW                                  
141400                End-If                                                    
141500                Add +1 To IX                                              
141600             End-Perform                                                  
141700                                                                          
141800             If TRAEFF-NOO                                                
141900                Move NOO To URVAL-SW                                      
142000             End-If                                                       
142100         End-If                                                           
142200       End-If                                                             
142300     End-If                                                               
142400                                                                          
142500* KOLLA FÖRPACKNINGSTYP URVAL                                             
142600     If URVAL-OK                                                          
142700       If IN20-BEFT (1) > Zero  Or IN20-BEFT (2) > Zero                   
142800       Or IN20-BEFT (3) > Zero  Or IN20-BEFT (4) > Zero                   
142900           Move +1 To IX                                                  
143000           Move NOO To TRAEFF-SW                                          
143100           Perform Until IX > 4  Or TRAEFF-OK                             
143200              If (IN-PART-BEFT-SLAG = IN20-BEFT (IX) And                  
143300                  IN20-BEFT (IX) > Zero)                                  
143400              Or (IN-PART-BEFT-CLAG = IN20-BEFT (IX) And                  
143500                  IN20-BEFT (IX) > Zero)                                  
143600                 Move YES To TRAEFF-SW                                    
143700              End-If                                                      
143800              Add +1 To IX                                                
143900           End-Perform                                                    
144000           If TRAEFF-NOO                                                  
144100              Move NOO To URVAL-SW                                        
144200           End-If                                                         
144300       End-If                                                             
144400     End-If                                                               
144500                                                                          
144600* KOLLA SÖKORD, BENÄMNING                                                 
144700     If URVAL-OK                                                          
144800*      -- IN20-BEART-SOEK är behandlad i A-INIT                           
144900       If BEART-BESORD > Space                                            
145000         Move Zero To IX                                                  
145100         Inspect IN-PART-BEART Tallying IX                                
145200         For All BEART-BESORD(1:BEART-DIFAELT)                            
145300                                                                          
145400         If IX = Zero                                                     
145500           Move NOO To URVAL-SW                                           
145600         End-If                                                           
145700       End-If                                                             
145800     End-If                                                               
145900                                                                          
146000* KOLLA URVAL ADLAGOMR + ADGANG                                           
146100     If URVAL-OK                                                          
146200        If IN20-ADLAGOMR > Zero                                           
146300                                                                          
146400           If ((IN-PART-ADLAGOMR   = IN20-ADLAGOMR)    and                
146500              ((IN-PART-ADGANG not < IN20-ADGANG-FOM)  and                
146600               (IN-PART-ADGANG not > IN20-ADGANG-TOM)))                   
146700                Continue                                                  
146800           Else                                                           
146900                Move NOO To URVAL-SW                                      
147000           End-if                                                         
147100        End-if                                                            
147200     End-if                                                               
147300     .                                                                    
147400     EJECT                                                                
147500                                                                          
147600 CC-SKAPA-SORTFIL SECTION.                                                
147700     MOVE 'CC-SKAPA-SORTFIL          ' TO CURRENT-SECTION                 
147800                                                                          
147900     Move IN-PART-IDARTNR        To SORTWS-IDARTNR                        
148000     Move IN-PART-IDDC           To SORTWS-IDDC                           
148100     Move IN-PART-IDLEVNR        To SORTWS-IDLEVNR                        
148200     Move IN-PART-IDLEVNR-SHIP   To SORTWS-IDLEVNR-SHIP                   
148300     If IN-PART-BEFT-SLAG > 0                                             
148400        Move IN-PART-BEFT-SLAG   To SORTWS-BEFT                           
148500     Else                                                                 
148600        Move IN-PART-BEFT-CLAG   To SORTWS-BEFT                           
148700     End-If                                                               
148800     Move IN-PART-BEART          To SORTWS-BEART                          
148900     Move IN-PART-KDFORP         To SORTWS-KDFORP                         
149000     Move IN-PART-KVLS           To SORTWS-KVLS                           
149100     Move IN-PART-KVRESS         To SORTWS-KVRESS                         
149200     Move IN-PART-KVAKS-PAV      To SORTWS-KVAKS-PAV                      
149300     Move IN-PART-KVAKS-SDC      To SORTWS-KVAKS-SDC                      
149400     Move IN-PART-KVROS-BULK     To SORTWS-KVROS-BULK                     
149500     Move IN-PART-KVROS-DAG      To SORTWS-KVROS-DAG                      
149600     Move IN-PART-KVVECKOR-LT    To SORTWS-KVVECKOR-LT                    
149700     Move IN-PART-IDANSK         To SORTWS-IDANSK                         
149800     Move IN-PART-IDBERED        To SORTWS-IDBERED                        
149900     Move +0                     to IX1-TILEVDAG                          
150000     PERFORM UNTIL IX1-TILEVDAG = +5                                      
150100       ADD +1                    To IX1-TILEVDAG                          
150200       MOVE IN-PART-TILEVDAG(IX1-TILEVDAG) TO SORTWS-TILEVDAG             
150300                                              (IX1-TILEVDAG)              
150400     END-PERFORM                                                          
150500     Move IN-PART-IDINK          To SORTWS-IDINK                          
150600     If IN-PART-KDAVT = +0                                                
150700        Move Noo                 To SORTWS-FLAVT                          
150800     Else                                                                 
150900        Move Yes                 To SORTWS-FLAVT                          
151000     End-If                                                               
151100     Move IN-PART-KVPB-REF       To SORTWS-KVPB-REF                       
151200     Move IN-PART-KVPBREOI       To SORTWS-KVPBREOI                       
151300     Move IN-PART-KVPB-PLAN      To SORTWS-KVPB-PLAN                      
151400     Move IN-PART-DAPBPLAN       To SORTWS-DAPBPLAN                       
151500     Move IN-PART-KVPB-TREND     To SORTWS-KVPB-TREND                     
151600     Move IN-PART-TIDATUM-TREND  To SORTWS-TIDATUM-TREND                  
151700     Move IN-PART-KVVECKOR-TREND To SORTWS-KVVECKOR-TREND                 
151800     Move IN-PART-DASEASON       To SORTWS-DASEASON                       
151900     Move +0                     To IX                                    
152000     Perform until IX = +12                                               
152100       Add +1                    To IX                                    
152200       Move IN-PART-RESEASON-PLAN (IX)                                    
152300                                 To SORTWS-RESEASON-PLAN (IX)             
152400     End-Perform                                                          
152500     Move IN-PART-KVOI-YEAR-0    To SORTWS-KVOI-YEAR-0                    
152600     Move IN-PART-KVOI-YEAR-1    To SORTWS-KVOI-YEAR-1                    
152700     Move IN-PART-KVOI-YEAR-2    To SORTWS-KVOI-YEAR-2                    
152800     Move IN-PART-KVOI-YEAR-3    To SORTWS-KVOI-YEAR-3                    
152900     Move IN-PART-KVOI-YEAR-4    To SORTWS-KVOI-YEAR-4                    
153000     Move IN-PART-KVOI-YEAR-5    To SORTWS-KVOI-YEAR-5                    
153100     Move IN-PART-KVOI-12-RULL   To SORTWS-KVOI-12-RULL                   
153200                                                                          
153300     Move IN-PART-KDERS          To SORTWS-KDERS                          
153400     If IN-PART-TIERSDAT-VIPS = Zero                                      
153500        Move Noo                 To SORTWS-FLERSDAT-VIPS                  
153600     Else                                                                 
153700        Move Yes                 To SORTWS-FLERSDAT-VIPS                  
153800     End-If                                                               
153900     Move IN-PART-PRMATRL        To SORTWS-PRMATRL                        
154000     Move IN-PART-PRAVCOST       To SORTWS-PRAVCOST                       
154100     Move IN-PART-KDFPKPRI       To SORTWS-KDFPKPRI                       
154200     Move IN-PART-KDSORT         To SORTWS-KDSORT                         
154300     Move IN-PART-TIREFSTO-LOC   To SORTWS-TIREFSTO-LOC                   
154400     If IN-PART-VKART-SLAG > 0                                            
154500        Move IN-PART-VKART-SLAG  To SORTWS-VKART                          
154600     Else                                                                 
154700        Move IN-PART-VKART-CLAG  To SORTWS-VKART                          
154800     End-if                                                               
154900     If IN-PART-VLARTNTO-SLAG > 0                                         
155000        Move IN-PART-VLARTNTO-SLAG  To SORTWS-VLARTNTO                    
155100     Else                                                                 
155200        Move IN-PART-VLARTNTO-CLAG  To SORTWS-VLARTNTO                    
155300     End-if                                                               
155400     Move IN-PART-KVTILLG-TOT    To SORTWS-KVTILLG-TOT                    
155500     Move IN-PART-TIFINLV        To SORTWS-TIFINLV                        
155600     Move IN-PART-DAPUBL         To SORTWS-DAPUBL                         
155700     Move IN-PART-TIURPROD       To SORTWS-TIURPROD                       
155800     Move IN-PART-KDPRODSL       To SORTWS-KDPRODSL                       
155900     Move IN-PART-IDFKNGRP       To SORTWS-IDFKNGRP                       
156000     Move IN-PART-KVREFBER       To SORTWS-KVREFBER                       
156100     Move IN-PART-TIREFPAF       To SORTWS-TIREFPAF                       
156200     Move IN-PART-KVEOQ          To SORTWS-KVEOQ                          
156300     Move IN-PART-KVPALL         To SORTWS-KVPALL                         
156400     Move IN-PART-KVULOAD        To SORTWS-KVULOAD                        
156500     If IN-PART-IDARTNR-EMBQ0-SLAG > 0                                    
156600       Move IN-PART-IDARTNR-EMBQ0-SLAG To SORTWS-IDARTNR-EMBQ0            
156700     Else                                                                 
156800       Move IN-PART-IDARTNR-EMBQ0-CLAG To SORTWS-IDARTNR-EMBQ0            
156900     End-If                                                               
157000     If IN-PART-IDARTNR-EMBQ1-SLAG > 0                                    
157100       Move IN-PART-IDARTNR-EMBQ1-SLAG To SORTWS-IDARTNR-EMBQ1            
157200     Else                                                                 
157300       Move IN-PART-IDARTNR-EMBQ1-CLAG To SORTWS-IDARTNR-EMBQ1            
157400     End-If                                                               
157500     If IN-PART-IDARTNR-EMBQ2-SLAG > 0                                    
157600       Move IN-PART-IDARTNR-EMBQ2-SLAG To SORTWS-IDARTNR-EMBQ2            
157700     Else                                                                 
157800       Move IN-PART-IDARTNR-EMBQ2-CLAG To SORTWS-IDARTNR-EMBQ2            
157900     End-If                                                               
158000     Move IN-PART-KVREFOVL       To SORTWS-KVREFOVL                       
158100     Move IN-PART-KVSPANT        To SORTWS-KVSPANT                        
158200     Move IN-PART-KVSLAGER       To SORTWS-KVSLAGER                       
158300     Move IN-PART-TIMANSEC       To SORTWS-TIMANSEC                       
158400     If IN-PART-KDARTURS-SLAG = SPACE                                     
158500       Move IN-PART-KDARTURS-CLAG  To SORTWS-KDARTURS                     
158600     Else                                                                 
158700       Move IN-PART-KDARTURS-SLAG  To SORTWS-KDARTURS                     
158800     End-If                                                               
158900     Move IN-PART-ADLAGOMR       To SORTWS-ADLAGOMR                       
159000     Move IN-PART-ADGANG         To SORTWS-ADGANG                         
159100     Move IN-PART-ADPLATS        To SORTWS-ADPLATS                        
159200     Move IN-PART-FLJIT          To SORTWS-FLJIT                          
159300     Move IN-PART-KDLEVPLF       To SORTWS-KDLEVPLF                       
159400     Move IN-PART-IDPROJ         To SORTWS-IDPROJ                         
159500     Move 1 To IX                                                         
159600     Perform Until IX > 3                                                 
159700        Move IN-PART-IDKAT(IX)   To SORTWS-IDKAT(IX)                      
159800        Add 1 To IX                                                       
159900     End-Perform                                                          
160000     Move IN-PART-FLLSRDEL       To SORTWS-FLLSRDEL                       
160100     Move IN-PART-SUINKORD       To SORTWS-SUINKORD                       
160200     Move IN-PART-SUAVBRP        To SORTWS-SUAVBRP                        
160300     Move IN-PART-RESERVG-NTO    To SORTWS-RESERVG-NTO                    
160400                                                                          
160500     If WS-FLAGGA-LISTA(02) > Space                                       
160600        Perform CCB-HAMTA-BELEV                                           
160700     End-If                                                               
160800                                                                          
160900     If WS-FLAGGA-LISTA(06) > Space                                       
161000*       -- BO                                                             
161100        Perform CCK-HAMTA-WDA5A-RORADER                                   
161200        Perform CCL-HAMTA-VOR-QUEUE                                       
161300     End-If                                                               
161400                                                                          
161500     If WS-FLAGGA-LISTA(07) > Space                                       
161600        Perform CCA-HAMTA-ARREARS-O-NOT-REC                               
161700     End-If                                                               
161800                                                                          
161900     If WS-FLAGGA-LISTA(09) > Space                                       
162000        Perform CCC-HAMTA-FORAVIS-FRAN-W6D1                               
162100     End-If                                                               
162200                                                                          
162300     If IN20-KVVECKOR-AVROP > Zero                                        
162400        Perform CCD-HAMTA-AVROP-2-WDD905                                  
162500     Else                                                                 
162600        Move +0                  To SORTWS-KVAVROP-SUM-VV                 
162700     End-If                                                               
162800                                                                          
162900     If IN20-KVVECKOR-KVPB  > Zeroes                                      
163000        Perform CCE-HAMTA-KVPB-SUM-VV                                     
163100     End-If                                                               
163200                                                                          
163300     If WS-FLAGGA-LISTA (13) > Space                                      
163400        PERFORM CCF-GET-LATESTDEL-WDL611                                  
163500     End-If                                                               
163600                                                                          
163700     If WS-FLAGGA-LISTA (34) > Space                                      
163800        PERFORM CCG-GET-PARAMETERS                                        
163900     End-If                                                               
164000                                                                          
164100     .                                                                    
164200     EJECT                                                                
164300                                                                          
164400 CCA-HAMTA-ARREARS-O-NOT-REC SECTION.                                     
164500     MOVE 'CCA-HAMTA-ARREARS-O-NOT-REC  ' TO CURRENT-SECTION              
164600                                                                          
164700     Move Zero                To SORTWS-KVSLAP-SUM                        
164800     Move Zero                To SORTWS-KVAVIS-NOT-REC                    
164900                                                                          
165000     Move IN-PART-IDARTNR     To W-IDARTNR                                
165100                                 W-IDARTNR-D9                             
165200     Move WS20-IDDC           To W-IDDC-D9                                
165300*                                                                         
165400     IF WS20-FL-IDLEVNR-SHIP = 'X'                                        
165500       MOVE IN-PART-IDLEVNR-SHIP To W-IDLEVNR                             
165600     ELSE                                                                 
165700       Move IN-PART-IDLEVNR      To W-IDLEVNR                             
165800     END-IF                                                               
165900*                                                                         
166000     Perform IMS-GU-WDD902                                                
166100     If SEGMENT-FINNS                                                     
166200       Perform IMS-GNP-WDD905                                             
166300       Perform Until SEGMENT-SAKNAS                                       
166400         If TIAVRDAT-INL < DAGENS-TIDATUM                                 
166500*          --- Arrears                                                    
166600           Compute SORTWS-KVSLAP-SUM                                      
166700                 = SORTWS-KVSLAP-SUM + KVAVROP                            
166800         Else                                                             
166900           If TIAVRDAT-INL >= DAGENS-TIDATUM                              
167000*            --- Not Received                                             
167100             Move DAAVROP-AVS To AVROP-DAAVROP-AVS                        
167200             Move TILEVDAG    To AVROP-TILEVDAG                           
167300                                                                          
167400             If WS-AVROP-DAAVROP-AVS < DAGENS-AAAAVVD                     
167500             And TIAVRDAT-INL >= DAGENS-TIDATUM                           
167600                 Compute SORTWS-KVAVIS-NOT-REC                            
167700                       = SORTWS-KVAVIS-NOT-REC + KVAVROP                  
167800             End-If                                                       
167900           End-If                                                         
168000         End-If                                                           
168100         Perform IMS-GNP-WDD905                                           
168200       End-Perform                                                        
168300     End-If                                                               
168400     .                                                                    
168500     EJECT                                                                
168600 CCF-GET-LATESTDEL-WDL611 SECTION.                                        
168700     MOVE 'CCF-GET-LATESTDEL-WDL611  ' TO CURRENT-SECTION                 
168800                                                                          
168900     Initialize                 SORTWS-LATESTDEL (1)                      
169000     Initialize                 SORTWS-LATESTDEL (2)                      
169100     Initialize                 SORTWS-LATESTDEL (3)                      
169200     Initialize                 SORTWS-LATESTDEL (4)                      
169300     Initialize                 SORTWS-LATESTDEL (5)                      
169400                                                                          
169500     Move IN-PART-IDARTNR  To W-IDARTNR                                   
169600     Perform IMS-GU-WDL601                                                
169700     If SEGMENT-FINNS                                                     
169800       Move +1 To IX                                                      
169900       Perform Until IX > +5                                              
170000         Perform IMS-GNP-WDL611                                           
170100         If SEGMENT-FINNS                                                 
170200           PERFORM UNTIL SEGMENT-SAKNAS OR IX > 5                         
170300             IF INL-IDLEVNR   = IN-PART-IDLEVNR AND                       
170400                INL-KDRT      = 0               AND                       
170500                INL-IDDC      = IN-PART-IDDC    AND                       
170600                INL-FLMAKUL   = NOO             AND                       
170700               (INL-IDPTYP    = 'R31' OR 'R32')                           
170800                 MOVE INL-TIAVIDAT   TO SORTWS-TIAVIDAT-LATE (IX)         
170900                 MOVE INL-IDKUNDRF   TO SORTWS-IDKUNDRF-LATE (IX)         
171000                 IF INL-IDPTYP = 'R31'                                    
171100                    ADD INL-KVAVIS   TO SORTWS-KVANTAL-LATE  (IX)         
171200                 ELSE                                                     
171300                    ADD INL-KVANTMOT TO SORTWS-KVANTAL-LATE  (IX)         
171400                 END-IF                                                   
171500             END-IF                                                       
171600             PERFORM IMS-GNP-WDL611                                       
171700             Add +1 To IX                                                 
171800           END-PERFORM                                                    
171900         Else                                                             
172000           Add +5 To IX                                                   
172100         End-If                                                           
172200       End-Perform                                                        
172300     End-If                                                               
172400     .                                                                    
172500 CCB-HAMTA-BELEV SECTION.                                                 
172600     MOVE 'CCB-HAMTA-BELEV           ' TO CURRENT-SECTION                 
172700                                                                          
172800     Move Space   To SORTWS-BELEV                                         
172900                                                                          
173000     Move IN-PART-IDARTNR To W-IDARTNR                                    
173100*                                                                         
173200     IF WS20-FL-IDLEVNR-SHIP = 'X'                                        
173300       MOVE IN-PART-IDLEVNR-SHIP To W-IDLEVNR W-IDLEVNR-MIN               
173400                                              W-IDLEVNR-MAX               
173500     ELSE                                                                 
173600       Move IN-PART-IDLEVNR      To W-IDLEVNR W-IDLEVNR-MIN               
173700                                              W-IDLEVNR-MAX               
173800     END-IF                                                               
173900*                                                                         
174000     PERFORM IMS-GU-WDF501                                                
174100     If SEGMENT-FINNS                                                     
174200       PERFORM IMS-GNP-WDF502                                             
174300       Perform Until SEGMENT-SAKNAS                                       
174400         Move XLEV-BELEVART      To SORTWS-BELEV                          
174500         PERFORM IMS-GNP-WDF502                                           
174600       End-Perform                                                        
174700     End-If                                                               
174800     .                                                                    
174900     EJECT                                                                
175000 CCC-HAMTA-FORAVIS-FRAN-W6D1 SECTION.                                     
175100     MOVE 'CCC-HAMTA-FORAVIS-FRAN-W6D1 ' TO CURRENT-SECTION               
175200                                                                          
175300     Move Zero                  To W-KVART-TOT-C1                         
175400                                                                          
175500     Move SORTWS-IDARTNR        To W-IDARTNR-HSEQ                         
175600                                                                          
175700     Perform IMS-GN-W6D111-W6D1SEQ                                        
175800     Perform Until SEGMENT-SAKNAS                                         
175900       If W6D1-ART-FLFEL = NOO                                            
176000      And W6D1-ART-IDLOPNRM = Zero                                        
176100          If IN-PART-IDDC = W6D1-ART-IDDC                                 
176200            Add W6D1-ART-KVAVIS    To W-KVART-TOT-C1                      
176300          End-If                                                          
176400       End-If                                                             
176500       Perform IMS-GN-W6D111-W6D1SEQ                                      
176600     End-Perform                                                          
176700     Move W-KVART-TOT-C1        To SORTWS-KVAVIS-FORAVIS                  
176800     .                                                                    
176900     EJECT                                                                
177000                                                                          
177100 CCD-HAMTA-AVROP-2-WDD905  SECTION.                                       
177200     MOVE 'CCD-HAMTA-AVROP-2-WDD905  ' TO CURRENT-SECTION                 
177300                                                                          
177400*  Gällande avrop hämtas för det antal veckor som är beställt.            
177500*  Första veckan är innevarande, med början idag.                         
177600*  Slutvecka och  dagar i kommande veckor + de i denna veckan             
177700*  är beräknat i AA- section.                                             
177800*  Addera alla KVAVROP mot denna leverantör på aktuell artikel            
177900                                                                          
178000     Move Zero To SORTWS-KVAVROP-SUM-VV                                   
178100                                                                          
178200     Move SORTWS-IDARTNR        To W-IDARTNR-D9                           
178300     Move SORTWS-IDDC           To W-IDDC-D9                              
178400     IF WS20-FL-IDLEVNR-SHIP = 'X'                                        
178500       MOVE SORTWS-IDLEVNR-SHIP To W-IDLEVNR                              
178600     ELSE                                                                 
178700       Move SORTWS-IDLEVNR      To W-IDLEVNR                              
178800     END-IF                                                               
178900                                                                          
179000     Perform IMS-GU-WDD902                                                
179100                                                                          
179200     If SEGMENT-FINNS                                                     
179300       Perform IMS-GNP-WDD905                                             
179400       Perform Until SEGMENT-SAKNAS                                       
179500                  Or  DAAVROP-AVS > DAAVROP-SLUTVECKA                     
179600                                                                          
179700         If ( DAAVROP-AVS   = DAGENS-DAAVROP                              
179800              And TILEVDAG >= DAGENS-TILEVDAG )                           
179900         Or   DAAVROP-AVS  >  DAGENS-DAAVROP                              
180000              Add  KVAVROP      To SORTWS-KVAVROP-SUM-VV                  
180100         End-If                                                           
180200         Perform IMS-GNP-WDD905                                           
180300       End-Perform                                                        
180400     End-If                                                               
180500     .                                                                    
180600     EJECT                                                                
180700                                                                          
180800 CCE-HAMTA-KVPB-SUM-VV SECTION.                                           
180900     MOVE 'CCE-HAMTA-KVPB-SUM-VV   ' TO CURRENT-SECTION                   
181000                                                                          
181100     Move Zero                 To SORTWS-KVPB-SUM-VV                      
181200                                                                          
181300*    Addera behovet i den innevarande veckan                              
181400     Perform CCEA-SEP-BEHOV-INNEV-VECKA                                   
181500                                                                          
181600     If IN20-KVVECKOR-KVPB > 1                                            
181700*      Summera sedan behovet i de resterande veckorna                     
181800*      LINK-TIBEHOV-START-VECKA (Veckan efter denna) är beräknad i        
181900*      AB- section.                                                       
182000       Move SORTWS-IDARTNR     To LINK-IDARTNR                            
182100       Move SORTWS-IDDC        To LINK-IDDC                               
182200       Move SORTWS-KVTILLG-TOT To LINK-KVTILLG-TOT-CDC                    
182300                                                                          
182400       If IN-PART-KDERS-UTG = +0                                          
182500          Perform CCEB-SEP-BEHOV-OVRIGA-VECKOR                            
182600          Perform CCEC-XDC-BEHOV                                          
182700       End-if                                                             
182800     End-If                                                               
182900     .                                                                    
183000     EJECT                                                                
183100                                                                          
183200 CCEA-SEP-BEHOV-INNEV-VECKA SECTION.                                      
183300     MOVE 'CCEA-SEP-BEHOV-INNEV-VECKA' TO CURRENT-SECTION                 
183400                                                                          
183500     ACCEPT W-TIME             From TIME                                  
183600     IF IN-PART-KVPB-REF > +0                                             
183700       Compute W-VECKO-SEP-BEHOV Rounded = IN-PART-KVPB-REF / 4.33        
183800     Else                                                                 
183900       Compute W-VECKO-SEP-BEHOV Rounded = IN-PART-KVPB-SEP / 4.33        
184000     End-If                                                               
184100     Compute W-DAG-SEP-BEHOV   Rounded  =  W-VECKO-SEP-BEHOV / 5          
184200     If IN-PART-DAPUBL > Zero                                             
184300        Divide IN-PART-DAPUBL By 10 Giving W-TIFINLV-AAVV                 
184400     Else                                                                 
184500        Divide IN-PART-TIFINLV By 10 Giving W-TIFINLV-AAVV                
184600     End-If                                                               
184700     Move DAGENS-TIAAVV        To TMP1-YYWW                               
184800     Move W-TIFINLV-AAVV       To TMP2-YYWW                               
184900     Perform WY2000P3                                                     
185000                                                                          
185100     If  DAGENS-DAGNR = 6 Or                                              
185200         DAGENS-DAGNR = 7 Or                                              
185300        (DAGENS-DAGNR = 5 And W-TIME(1:4) > 1700)                         
185400     Or  TMP1-YYWW < TMP2-YYWW                                            
185500*        --- NOLL i bidrag till KVPB-SUM-VV                               
185600         Continue                                                         
185700     Else                                                                 
185800         Compute W-KVDAGAR-KVAR =  5 - DAGENS-DAGNR                       
185900         If W-TIME(1:4) <= 1700                                           
186000            Add +1             To W-KVDAGAR-KVAR                          
186100         End-If                                                           
186200     End-If                                                               
186300     .                                                                    
186400     EJECT                                                                
186500                                                                          
186600 CCEB-SEP-BEHOV-OVRIGA-VECKOR SECTION.                                    
186700     MOVE 'CCEB-SEP-BEHOV-OVRIGA-VECKOR' TO CURRENT-SECTION               
186800                                                                          
186900     Compute LINK-KVVECKOR-BEHOV = IN20-KVVECKOR-KVPB - 1                 
187000     Set  LINK-PB-TOTAL-SEP-LEV-XDC To True                               
187100                                                                          
187200     Call W222BHDC Using LINK-AREA BHDC-WDK6-PCB   BHDC-WDK7-PCB          
187300                                   BHDC-WDB6-PCB   BHDC-WDR2-PCB          
187400                                   BHDC-WDD7-PCB   BHDC-WDK7E-PCB         
187500                                   BHDC-WDD7-2-PCB BHDC-WDK9-PCB          
187600                                   BHDC-REFL1-2501-PCB                    
187700                                   BHDC-REFL1-WDB6-PCB                    
187800                                   BHDC-REFL1-WDK7-PCB                    
187900                                   BHDC-REFL1-UTIL-WDK6-PCB               
188000                                   BHDC-REFL1-UTIL-WDK7-PCB               
188100                                   BHDC-REFL1-UTIL-WDB6-PCB               
188200                                   BHDC-REFL2-2501-PCB                    
188300                                   BHDC-REFL2-WDB6-PCB                    
188400                                   BHDC-REFL2-UTIL-WDK6-PCB               
188500                                   BHDC-REFL2-UTIL-WDK7-PCB               
188600                                   BHDC-REFL2-UTIL-WDB6-PCB               
188700                                   BHDC-UTIL-WDK6-PCB                     
188800                                   BHDC-UTIL-WDK7-PCB                     
188900                                   BHDC-UTIL-WDB6-PCB                     
189000                                   BHDC-W222-WDK6-PCB                     
189100                                   BHDC-W222-WDK7-PCB                     
189200                                   BHDC-W222-ARTM-PCB                     
189300                                   BHDC-W222-2501-PCB                     
189400                                   BHDC-W222-WDB6R-PCB                    
189500                                   BHDC-W222-WDK7R-PCB                    
189600                                   BHDC-W222-WDB6-PCB                     
189700                                   BHDC-W222-WDD7-PCB                     
189800                                   BHDC-W222-WDK7E-PCB                    
189900                                   BHDC-W222-UTIL-WDK6-PCB                
190000                                   BHDC-W222-UTIL-WDK7-PCB                
190100                                   BHDC-W222-UTIL-WDB6-PCB                
190200                                   BHDC-W222-UTUP-WDK7-PCB                
190300                                   BHDC-W222-UTUP-WDB6-PCB                
190400                                   BHDC-W222-UTUP-UTIL-WDK6-PCB           
190500                                   BHDC-W222-UTUP-UTIL-WDK7-PCB           
190600                                   BHDC-W222-UTUP-UTIL-WDB6-PCB           
190700                                   BHDC-UTUP-WDK7-PCB                     
190800                                   BHDC-UTUP-WDB6-PCB                     
190900                                   BHDC-UTUP-UTIL-WDK6-PCB                
191000                                   BHDC-UTUP-UTIL-WDK7-PCB                
191100                                   BHDC-UTUP-UTIL-WDB6-PCB                
191200                                                                          
191300     If LINK-ANROP-OK                                                     
191400       Move +1 To IX                                                      
191500       Perform Until IX > LINK-KVVECKOR-BEHOV                             
191600          Add LINK-KVBEHOV-VECKA(IX) To SORTWS-KVPB-SUM-VV                
191700          Add +1 To IX                                                    
191800       End-Perform                                                        
191900     Else                                                                 
192000       String 'FEL i anrop FRÅN CCEB-SEP-BEHOV-OVRIGA-VECKOR'             
192100              ' till W222BHDC(behovsmodulen)'                             
192200              Delimited By Size Into FELTEXT-STR                          
192300       Display FELTEXT-STR ' ' SORTWS-IDARTNR                             
192400     End-If                                                               
192500     .                                                                    
192600     EJECT                                                                
192700                                                                          
192800 CCEC-XDC-BEHOV SECTION.                                                  
192900     MOVE 'CCEC-XDC-BEHOV ' TO CURRENT-SECTION                            
193000                                                                          
193100     Compute LINK-KVVECKOR-BEHOV = IN20-KVVECKOR-KVPB - 1                 
193200                                                                          
193300     Set LINK-ENDAST-XDCBEHOV To True                                     
193400                                                                          
193500     Call W222BHDC Using LINK-AREA BHDC-WDK6-PCB   BHDC-WDK7-PCB          
193600                                   BHDC-WDB6-PCB   BHDC-WDR2-PCB          
193700                                   BHDC-WDD7-PCB   BHDC-WDK7E-PCB         
193800                                   BHDC-WDD7-2-PCB BHDC-WDK9-PCB          
193900                                   BHDC-REFL1-2501-PCB                    
194000                                   BHDC-REFL1-WDB6-PCB                    
194100                                   BHDC-REFL1-WDK7-PCB                    
194200                                   BHDC-REFL1-UTIL-WDK6-PCB               
194300                                   BHDC-REFL1-UTIL-WDK7-PCB               
194400                                   BHDC-REFL1-UTIL-WDB6-PCB               
194500                                   BHDC-REFL2-2501-PCB                    
194600                                   BHDC-REFL2-WDB6-PCB                    
194700                                   BHDC-REFL2-UTIL-WDK6-PCB               
194800                                   BHDC-REFL2-UTIL-WDK7-PCB               
194900                                   BHDC-REFL2-UTIL-WDB6-PCB               
195000                                   BHDC-UTIL-WDK6-PCB                     
195100                                   BHDC-UTIL-WDK7-PCB                     
195200                                   BHDC-UTIL-WDB6-PCB                     
195300                                   BHDC-W222-WDK6-PCB                     
195400                                   BHDC-W222-WDK7-PCB                     
195500                                   BHDC-W222-ARTM-PCB                     
195600                                   BHDC-W222-2501-PCB                     
195700                                   BHDC-W222-WDB6R-PCB                    
195800                                   BHDC-W222-WDK7R-PCB                    
195900                                   BHDC-W222-WDB6-PCB                     
196000                                   BHDC-W222-WDD7-PCB                     
196100                                   BHDC-W222-WDK7E-PCB                    
196200                                   BHDC-W222-UTIL-WDK6-PCB                
196300                                   BHDC-W222-UTIL-WDK7-PCB                
196400                                   BHDC-W222-UTIL-WDB6-PCB                
196500                                   BHDC-W222-UTUP-WDK7-PCB                
196600                                   BHDC-W222-UTUP-WDB6-PCB                
196700                                   BHDC-W222-UTUP-UTIL-WDK6-PCB           
196800                                   BHDC-W222-UTUP-UTIL-WDK7-PCB           
196900                                   BHDC-W222-UTUP-UTIL-WDB6-PCB           
197000                                   BHDC-UTUP-WDK7-PCB                     
197100                                   BHDC-UTUP-WDB6-PCB                     
197200                                   BHDC-UTUP-UTIL-WDK6-PCB                
197300                                   BHDC-UTUP-UTIL-WDK7-PCB                
197400                                   BHDC-UTUP-UTIL-WDB6-PCB                
197500     If LINK-ANROP-OK                                                     
197600       Add LINK-KVBEHOV-DESSUTOM To SORTWS-KVPB-SUM-VV                    
197700       Move +1 To IX                                                      
197800       Perform Until IX > LINK-KVVECKOR-BEHOV                             
197900         Add LINK-KVBEHOV-VECKA(IX) To SORTWS-KVPB-SUM-VV                 
198000         Add +1 To IX                                                     
198100       End-Perform                                                        
198200     Else                                                                 
198300       String 'FEL i anrop FRÅN CCEC-LDC-BEHOV '                          
198400              ' till W222BHDC(behovsmodulen)'                             
198500              Delimited By Size Into FELTEXT-STR                          
198600       Display FELTEXT-STR ' ' SORTWS-IDARTNR                             
198700     End-If                                                               
198800     .                                                                    
198900     EJECT                                                                
199000                                                                          
199100 CCG-GET-PARAMETERS SECTION.                                              
199200     MOVE 'CCG-GET-PARAMETERS          ' TO CURRENT-SECTION               
199300                                                                          
199400     INITIALIZE REFL-W271REFL                                             
199500                                                                          
199600     MOVE IN-PART-IDDC               TO REFL-IDDC                         
199700     MOVE IN-PART-IDARTNR            TO REFL-IDARTNR                      
199800     MOVE SPACE                      TO REFL-IDDC-REF                     
199900     MOVE IN-PART-IDREFTAB           TO REFL-IDREFTAB                     
200000     MOVE IN-PART-FLWILSON           TO REFL-FLWILSON                     
200100     MOVE IN-PART-PRMATRL            TO REFL-PRARTBES                     
200200                                                                          
200300     MOVE IN-PART-TIREFPKT           TO TEST1-YYMMDD                      
200400     MOVE DAGENS-TIDATUM             TO TEST2-YYMMDD                      
200500     IF TEST1-YYMMDD >= TEST2-YYMMDD                                      
200600         MOVE IN-PART-KVREFPKT       TO REFL-IN-KVREFPKT                  
200700     ELSE                                                                 
200800         MOVE ZERO                   TO REFL-IN-KVREFPKT                  
200900     END-IF                                                               
201000                                                                          
201100     MOVE IN-PART-TIREFPAF           TO TEST1-YYMMDD                      
201200     MOVE DAGENS-TIDATUM             TO TEST2-YYMMDD                      
201300     IF TEST1-YYMMDD >= TEST2-YYMMDD                                      
201400        MOVE IN-PART-KVREFBER        TO REFL-IN-KVREFBER                  
201500        MOVE IN-PART-TIREFPAF        TO TEST1-YYMMDD                      
201600     ELSE                                                                 
201700        MOVE ZERO                    TO REFL-IN-KVREFBER                  
201800        MOVE ZERO                    TO TEST1-YYMMDD                      
201900     END-IF                                                               
202000                                                                          
202100     MOVE IN-PART-IDLEVNR            TO REFL-IN-IDLEVNR-DC                
202200     MOVE ZERO                       TO REFL-NDC-KVDAGAR-TBT-DC           
202300                                                                          
202400     MOVE +1                         TO IX                                
202500     PERFORM UNTIL IX > +12                                               
202600       MOVE IN-PART-RESEASON-PLAN (IX)                                    
202700                                     TO REFL-RESEASON(IX)                 
202800       ADD +1                        TO IX                                
202900     END-PERFORM                                                          
203000                                                                          
203100     MOVE IN-PART-FLFLYG             TO REFL-FLFLYG                       
203200                                                                          
203300     CALL W271REFL USING REFL-W271REFL                                    
203400                         REFL-2501-PCB                                    
203500                         REFL-WDB6-PCB                                    
203600                         REFL-WDK7-PCB                                    
203700                         UTIL-WDK6-PCB                                    
203800                         UTIL-WDK7-PCB                                    
203900                         UTIL-WDB6-PCB                                    
204000                                                                          
204100     MOVE REFL-KLASS (2:1)           TO SORTWS-KDPRISKL                   
204200     MOVE REFL-KLASS (3:1)           TO SORTWS-KDFREKKL                   
204300     MOVE IN-PART-IDREFTAB           TO SORTWS-IDREFTAB                   
204400     .                                                                    
204500     EJECT                                                                
204600 CCK-HAMTA-WDA5A-RORADER SECTION.                                         
204700     MOVE 'CCK-HAMTA-WDA5A-RORADER ' TO CURRENT-SECTION                   
204800     SKIP2                                                                
204900*    "RO" LISTA 7                                                         
205000     MOVE LOW-VALUE                  TO W-WDA5A1KY-MIN                    
205100     Move HIGH-VALUE                 To W-WDA5A1KY-MAX                    
205200     MOVE IN-PART-IDARTNR            To W-IDARTNR-MIN                     
205300                                        W-IDARTNR-MAX                     
205400     Move IN-PART-IDDC               To W-IDDC-MIN                        
205500                                        W-IDDC-MAX                        
205600     Move '2'                        To W-KDSTARAD-MIN                    
205700                                        W-KDSTARAD-MAX                    
205800     Move +0                         To SORTWS-KVRORAD                    
205900                                                                          
206000     Perform IMS-GN-WDA5A                                                 
206100     Perform UNTIL SEGMENT-SAKNAS                                         
206200        Add +1                       To SORTWS-KVRORAD                    
206300        Perform IMS-GN-WDA5A                                              
206400     END-Perform                                                          
206500     .                                                                    
206600     EJECT                                                                
206700 CCL-HAMTA-VOR-QUEUE SECTION.                                             
206800     MOVE 'CCL-HAMTA-VOR-QUEUE     ' To CURRENT-SECTION                   
206900                                                                          
207000     MOVE +0                            TO W-SUM-KVVORKO                  
207100     MOVE +1                            TO INDX-VOR                       
207200     PERFORM UNTIL TAB-VOR-IDARTNR (INDX-VOR) = +0                        
207300       IF IN-PART-IDARTNR = TAB-VOR-IDARTNR (INDX-VOR)                    
207400          ADD TAB-VOR-KVVORKO(INDX-VOR) TO W-SUM-KVVORKO                  
207500       END-IF                                                             
207600       ADD  +1                          TO INDX-VOR                       
207700     END-PERFORM                                                          
207800                                                                          
207900     MOVE W-SUM-KVVORKO                 TO SORTWS-KVVORKO                 
208000     .                                                                    
208100     EJECT                                                                
208200 D-SORT-OUTPUT SECTION.                                                   
208300     MOVE 'D-SORT-OUTPUT           ' To CURRENT-SECTION                   
208400                                                                          
208500     Perform S32-SORT-RETURN                                              
208600                                                                          
208700     If END-OF-SORTFIL                                                    
208800        Move 1183 To LAENGD                                               
208900        Move '******* NO PARTS MATCHED SELECTION *******'                 
209000                              To W001-DETALJ W002-DETALJ                  
209100        Perform S21-SKRIV-W21720-001                                      
209200     Else                                                                 
209300        Move SORTWS-IDANSK    To OLD-IDANSK                               
209400        Move SORTWS-IDLEVNR   To OLD-IDLEVNR                              
209500     End-If                                                               
209600                                                                          
209700     Perform DD-SKAPA-EXCEL-RUBRIK                                        
209800                                                                          
209900     Perform Until END-OF-SORTFIL                                         
210000                                                                          
210100       Perform DE-SKAPA-EXCELRAD                                          
210200                                                                          
210300       Perform S21-SKRIV-W21720-001                                       
210400                                                                          
210500       Perform DF-KOLLA-STORLEK                                           
210600                                                                          
210700       Perform S32-SORT-RETURN                                            
210800     End-Perform                                                          
210900     .                                                                    
211000     EJECT                                                                
211100 DD-SKAPA-EXCEL-RUBRIK SECTION.                                           
211200     MOVE 'DD-SKAPA-EXCEL-RUBRIK       ' TO CURRENT-SECTION               
211300                                                                          
211400***************************************************************           
211500* RAD 1 from 2422                                                         
211600***************************************************************           
211700     Move 1 To LAENGD                                                     
211800     Move 'Part no  '                  To W002-ART-RAD(LAENGD:9)          
211900     Add  9                            To LAENGD                          
212000     Move TAB-TECKEN                   To W002-ART-RAD(LAENGD:1)          
212100     Add  1                            To LAENGD                          
212200* Suppl                                                                   
212300     If WS-FLAGGA-LISTA (1) > Space                                       
212400       Move 'Suppl'                    To W002-ART-RAD(LAENGD:5)          
212500       Add  5                          To LAENGD                          
212600       Move TAB-TECKEN                 To W002-ART-RAD(LAENGD:1)          
212700       Add  1                          To LAENGD                          
212800                                                                          
212900       Move 'SHP  '                    To W002-ART-RAD(LAENGD:5)          
213000       Add  5                          To LAENGD                          
213100       Move TAB-TECKEN                 To W002-ART-RAD(LAENGD:1)          
213200       Add  1                          To LAENGD                          
213300     End-If                                                               
213400* SupDescr                                                                
213500     If WS-FLAGGA-LISTA (2) > Space                                       
213600       Move 'SupDescription           ' To W002-ART-RAD(LAENGD:25)        
213700       Add  25                         To LAENGD                          
213800       Move TAB-TECKEN                 To W002-ART-RAD(LAENGD:1)          
213900       Add  1                          To LAENGD                          
214000     End-If                                                               
214100* Descr                                                                   
214200     If WS-FLAGGA-LISTA (3) > Space                                       
214300      Move 'Description              ' To W002-ART-RAD(LAENGD:25)         
214400      Add  25                          To LAENGD                          
214500       Move TAB-TECKEN                 To W002-ART-RAD(LAENGD:1)          
214600       Add 1                           To LAENGD                          
214700     End-If                                                               
214800* Stock                                                                   
214900     If WS-FLAGGA-LISTA (4) > Space                                       
215000       Move 'Stock'                    To W002-ART-RAD(LAENGD:5)          
215100       Add  5                          To LAENGD                          
215200       Move TAB-TECKEN                 To W002-ART-RAD(LAENGD:1)          
215300       Add  1                          To LAENGD                          
215400     End-If                                                               
215500* AK                                                                      
215600     If WS-FLAGGA-LISTA (5) > Space                                       
215700       Move 'AK     '                  To W002-ART-RAD(LAENGD:7)          
215800       Add  7                          To LAENGD                          
215900       Move TAB-TECKEN                 To W002-ART-RAD(LAENGD:1)          
216000       Add  1                          To LAENGD                          
216100     End-If                                                               
216200* BO/VOR                                                                  
216300     If WS-FLAGGA-LISTA (6) > Space                                       
216400       Move 'BO Qty '                  To W002-ART-RAD(LAENGD:7)          
216500       Add  7                          To LAENGD                          
216600       Move TAB-TECKEN                 To W002-ART-RAD(LAENGD:1)          
216700       Add  1                          To LAENGD                          
216800       Move 'BO Lines'                 To W002-ART-RAD(LAENGD:8)          
216900       Add  8                          To LAENGD                          
217000       Move TAB-TECKEN                 To W002-ART-RAD(LAENGD:1)          
217100       Add  1                          To LAENGD                          
217200       Move 'VOR Qty '                 To W002-ART-RAD(LAENGD:8)          
217300       Add  8                          To LAENGD                          
217400       Move TAB-TECKEN                 To W002-ART-RAD(LAENGD:1)          
217500       Add  1                          To LAENGD                          
217600     End-If                                                               
217700                                                                          
217800***************************************************************           
217900* RAD 2 from 2422                                                         
218000***************************************************************           
218100* Arrears                                                                 
218200     If WS-FLAGGA-LISTA (7) > Space                                       
218300       Move 'Arrears'                  To W002-ART-RAD(LAENGD:7)          
218400       Add  7                          To LAENGD                          
218500       Move TAB-TECKEN                 To W002-ART-RAD(LAENGD:1)          
218600       Add  1                          To LAENGD                          
218700       Move 'Not Received'             To W002-ART-RAD(LAENGD:12)         
218800       Add  12                         To LAENGD                          
218900       Move TAB-TECKEN                 To W002-ART-RAD(LAENGD:1)          
219000       Add  1                          To LAENGD                          
219100     End-If                                                               
219200* Del.prom                                                                
219300     If WS-FLAGGA-LISTA (8) > Space                                       
219400       Move 'DISPATCH Week'            To W002-ART-RAD(LAENGD:13)         
219500       Add  13                         To LAENGD                          
219600       Move TAB-TECKEN                 To W002-ART-RAD(LAENGD:1)          
219700       Add  1                          To LAENGD                          
219800       Move 'DISPATCH Qty'             To W002-ART-RAD(LAENGD:12)         
219900       Add  12                         To LAENGD                          
220000       Move TAB-TECKEN                 To W002-ART-RAD(LAENGD:1)          
220100       Add  1                          To LAENGD                          
220200       Move 'CD Plan.Inc.Wk'           To W002-ART-RAD(LAENGD:14)         
220300       Add  14                         To LAENGD                          
220400       Move TAB-TECKEN                 To W002-ART-RAD(LAENGD:1)          
220500       Add  1                          To LAENGD                          
220600       Move 'AVAIL Date'               To W002-ART-RAD(LAENGD:10)         
220700       Add  10                         To LAENGD                          
220800       Move TAB-TECKEN                 To W002-ART-RAD(LAENGD:1)          
220900       Add  1                          To LAENGD                          
221000       Move 'X-info Date'              To W002-ART-RAD(LAENGD:11)         
221100       Add  11                         To LAENGD                          
221200       Move TAB-TECKEN                 To W002-ART-RAD(LAENGD:1)          
221300       Add  1                          To LAENGD                          
221400     End-If                                                               
221500* Pre.Adv.                                                                
221600     If WS-FLAGGA-LISTA (09) > Space                                      
221700       Move 'ADVICED Qty'              To W002-ART-RAD(LAENGD:11)         
221800       Add  11                         To LAENGD                          
221900       Move TAB-TECKEN                 To W002-ART-RAD(LAENGD:1)          
222000       Add  1                          To LAENGD                          
222100     End-If                                                               
222200* LeadTime                                                                
222300     If WS-FLAGGA-LISTA (10) > Space                                      
222400       Move 'LEAD TIME'                To W002-ART-RAD(LAENGD:9)          
222500       Add  9                          To LAENGD                          
222600       Move TAB-TECKEN                 To W002-ART-RAD(LAENGD:1)          
222700       Add 1                           To LAENGD                          
222800     End-If                                                               
222900                                                                          
223000***************************************************************           
223100* RAD 3 from 2422                                                         
223200***************************************************************           
223300* Proc                                                                    
223400     If WS-FLAGGA-LISTA (11) > Space                                      
223500       Move 'PURCH.PL'              To W002-ART-RAD(LAENGD:8)             
223600       Add  8                          To LAENGD                          
223700       Move TAB-TECKEN                 To W002-ART-RAD(LAENGD:1)          
223800       Add  1                          To LAENGD                          
223900                                                                          
224000       Move 'PLANNER'                  To W002-ART-RAD(LAENGD:7)          
224100       Add  7                          To LAENGD                          
224200       Move TAB-TECKEN                 To W002-ART-RAD(LAENGD:1)          
224300       Add  1                          To LAENGD                          
224400     End-If                                                               
224500* SendDay                                                                 
224600     If WS-FLAGGA-LISTA (12) > Space                                      
224700       Move 'Send day'                 To W002-ART-RAD(LAENGD:8)          
224800       Add  8                          To LAENGD                          
224900       Move TAB-TECKEN                 To W002-ART-RAD(LAENGD:1)          
225000       Add  1                          To LAENGD                          
225100     End-If                                                               
225200* LatestDel new layout                                                    
225300     If WS-FLAGGA-LISTA (13) > Space                                      
225400       Move 'Latest Del.1'             To W002-ART-RAD(LAENGD:12)         
225500       Add  12                         To LAENGD                          
225600       Move TAB-TECKEN                 To W002-ART-RAD(LAENGD:1)          
225700       Add  1                          To LAENGD                          
225800       Move 'Latest Del.2'             To W002-ART-RAD(LAENGD:12)         
225900       Add  12                         To LAENGD                          
226000       Move TAB-TECKEN                 To W002-ART-RAD(LAENGD:1)          
226100       Add  1                          To LAENGD                          
226200       Move 'Latest Del.3'             To W002-ART-RAD(LAENGD:12)         
226300       Add  12                         To LAENGD                          
226400       Move TAB-TECKEN                 To W002-ART-RAD(LAENGD:1)          
226500       Add  1                          To LAENGD                          
226600       Move 'Latest Del.4'             To W002-ART-RAD(LAENGD:12)         
226700       Add  12                         To LAENGD                          
226800       Move TAB-TECKEN                 To W002-ART-RAD(LAENGD:1)          
226900       Add  1                          To LAENGD                          
227000       Move 'Latest Del.5'             To W002-ART-RAD(LAENGD:12)         
227100       Add  12                         To LAENGD                          
227200       Move TAB-TECKEN                 To W002-ART-RAD(LAENGD:1)          
227300       Add  1                          To LAENGD                          
227400     End-If                                                               
227500                                                                          
227600* PurchID                                                                 
227700     If WS-FLAGGA-LISTA (14) > Space                                      
227800       Move 'Purchaser'                To W002-ART-RAD(LAENGD:9)          
227900       Add  9                          To LAENGD                          
228000       Move TAB-TECKEN                 To W002-ART-RAD(LAENGD:1)          
228100       Add  1                          To LAENGD                          
228200     End-If                                                               
228300* Agreem - avtal                                                          
228400     If WS-FLAGGA-LISTA (15) > Space                                      
228500       Move 'Agreem'                   To W002-ART-RAD(LAENGD:7)          
228600       Add  7                          To LAENGD                          
228700       Move TAB-TECKEN                 To W002-ART-RAD(LAENGD:1)          
228800       Add  1                          To LAENGD                          
228900     End-If                                                               
229000* PerDem (PB)                                                             
229100     If WS-FLAGGA-LISTA (16) > Space                                      
229200       Move 'Forecast SEP'             To W002-ART-RAD(LAENGD:12)         
229300       Add  12                         To LAENGD                          
229400       Move TAB-TECKEN                 To W002-ART-RAD(LAENGD:1)          
229500       Add  1                          To LAENGD                          
229600                                                                          
229700       Move 'Forecast TOT'             To W002-ART-RAD(LAENGD:12)         
229800       Add  14                         To LAENGD                          
229900       Move TAB-TECKEN                 To W002-ART-RAD(LAENGD:1)          
230000       Add  1                          To LAENGD                          
230100                                                                          
230200       Move 'Forecast PLAN'            To W002-ART-RAD(LAENGD:13)         
230300       Add  13                         To LAENGD                          
230400       Move TAB-TECKEN                 To W002-ART-RAD(LAENGD:1)          
230500       Add  1                          To LAENGD                          
230600                                                                          
230700       Move 'Fc PLAN ToDate'           To W002-ART-RAD(LAENGD:14)         
230800       Add  14                         To LAENGD                          
230900       Move TAB-TECKEN                 To W002-ART-RAD(LAENGD:1)          
231000       Add  1                          To LAENGD                          
231100     End-If                                                               
231200* TREND 17                                                                
231300     If WS-FLAGGA-LISTA (17) > Space                                      
231400       Move 'Trend'                    To W002-ART-RAD(LAENGD:5)          
231500       Add  5                          To LAENGD                          
231600       Move TAB-TECKEN                 To W002-ART-RAD(LAENGD:1)          
231700       Add  1                          To LAENGD                          
231800                                                                          
231900       Move 'Trend weeks'              To W002-ART-RAD(LAENGD:11)         
232000       Add  11                         To LAENGD                          
232100       Move TAB-TECKEN                 To W002-ART-RAD(LAENGD:1)          
232200       Add  1                          To LAENGD                          
232300                                                                          
232400       Move 'Trend updated'            To W002-ART-RAD(LAENGD:13)         
232500       Add  13                         To LAENGD                          
232600       Move TAB-TECKEN                 To W002-ART-RAD(LAENGD:1)          
232700       Add  1                          To LAENGD                          
232800     End-If                                                               
232900* Seas                                                                    
233000     If WS-FLAGGA-LISTA (18) > Space                                      
233100       Move 'Season Locked To'         To W002-ART-RAD(LAENGD:16)         
233200       Add  16                         To LAENGD                          
233300       Move TAB-TECKEN                 To W002-ART-RAD(LAENGD:1)          
233400       Add  1                          To LAENGD                          
233500       Move 'Season'                   To W002-ART-RAD(LAENGD:6)          
233600       Add  6                          To LAENGD                          
233700       Move TAB-TECKEN                 To W002-ART-RAD(LAENGD:1)          
233800       Add  1                          To LAENGD                          
233900     End-If                                                               
234000                                                                          
234100* DemHist - Oi ru 12                                                      
234200     If WS-FLAGGA-LISTA (19) > Space                                      
234300       Move 'IO LAST 12'               To W002-ART-RAD(LAENGD:10)         
234400       Add  10                         To LAENGD                          
234500       Move TAB-TECKEN                 To W002-ART-RAD(LAENGD:1)          
234600       Add  1                          To LAENGD                          
234700     End-If                                                               
234800                                                                          
234900* DemHist Year - Oi(iår+5)                                                
235000     If WS-FLAGGA-LISTA (20) > Space                                      
235100       Move 'IO '                      To W002-ART-RAD(LAENGD:3)          
235200       Add  3                          To LAENGD                          
235300       Move OI-ARTAL-0                 To W002-ART-RAD(LAENGD:4)          
235400       Add  4                          To LAENGD                          
235500       Move TAB-TECKEN                 To W002-ART-RAD(LAENGD:1)          
235600       Add  1                          To LAENGD                          
235700       Move 'IO '                      To W002-ART-RAD(LAENGD:3)          
235800       Add  3                          To LAENGD                          
235900       Move OI-ARTAL-1                 To W002-ART-RAD(LAENGD:4)          
236000       Add  4                          To LAENGD                          
236100       Move TAB-TECKEN                 To W002-ART-RAD(LAENGD:1)          
236200       Add  1                          To LAENGD                          
236300       Move 'IO '                      To W002-ART-RAD(LAENGD:3)          
236400       Add  3                          To LAENGD                          
236500       Move OI-ARTAL-2                 To W002-ART-RAD(LAENGD:4)          
236600       Add  4                          To LAENGD                          
236700       Move TAB-TECKEN                 To W002-ART-RAD(LAENGD:1)          
236800       Add  1                          To LAENGD                          
236900       Move 'IO '                      To W002-ART-RAD(LAENGD:3)          
237000       Add  3                          To LAENGD                          
237100       Move OI-ARTAL-3                 To W002-ART-RAD(LAENGD:4)          
237200       Add  4                          To LAENGD                          
237300       Move TAB-TECKEN                 To W002-ART-RAD(LAENGD:1)          
237400       Add  1                          To LAENGD                          
237500       Move 'IO '                      To W002-ART-RAD(LAENGD:3)          
237600       Add  3                          To LAENGD                          
237700       Move OI-ARTAL-4                 To W002-ART-RAD(LAENGD:4)          
237800       Add  4                          To LAENGD                          
237900       Move TAB-TECKEN                 To W002-ART-RAD(LAENGD:1)          
238000       Add  1                          To LAENGD                          
238100       Move 'IO '                      To W002-ART-RAD(LAENGD:3)          
238200       Add  3                          To LAENGD                          
238300       Move OI-ARTAL-5                 To W002-ART-RAD(LAENGD:4)          
238400       Add  4                          To LAENGD                          
238500       Move TAB-TECKEN                 To W002-ART-RAD(LAENGD:1)          
238600       Add  1                          To LAENGD                          
238700     End-If                                                               
238800                                                                          
238900***************************************************************           
239000* RAD 4 from 2422                                                         
239100***************************************************************           
239200* SS CDC - Erskod                                                         
239300     If WS-FLAGGA-LISTA (21) > Space                                      
239400       Move 'SS CDC'                   To W002-ART-RAD(LAENGD:6)          
239500       Add  6                          To LAENGD                          
239600       Move TAB-TECKEN                 To W002-ART-RAD(LAENGD:1)          
239700       Add  1                          To LAENGD                          
239800     End-If                                                               
239900                                                                          
240000* SS local                                                                
240100     If WS-FLAGGA-LISTA (22) > Space                                      
240200       Move 'SS local'                 To W002-ART-RAD(LAENGD:8)          
240300       Add  8                          To LAENGD                          
240400       Move TAB-TECKEN                 To W002-ART-RAD(LAENGD:1)          
240500       Add  1                          To LAENGD                          
240600     End-If                                                               
240700                                                                          
240800* Price                                                                   
240900     If WS-FLAGGA-LISTA (23) > Space                                      
241000       Move 'Matrial price'            To W002-ART-RAD(LAENGD:13)         
241100       Add  13                         To LAENGD                          
241200       Move TAB-TECKEN                 To W002-ART-RAD(LAENGD:1)          
241300       Add  1                          To LAENGD                          
241400       Move 'Aver.Cost'                To W002-ART-RAD(LAENGD:9)          
241500       Add  9                          To LAENGD                          
241600       Move TAB-TECKEN                 To W002-ART-RAD(LAENGD:1)          
241700       Add  1                          To LAENGD                          
241800     End-If                                                               
241900* PackFL - Förp.flag                                                      
242000     If WS-FLAGGA-LISTA (24) > Space                                      
242100       Move 'PackFL'                   To W002-ART-RAD(LAENGD:6)          
242200       Add  6                          To LAENGD                          
242300       Move TAB-TECKEN                 To W002-ART-RAD(LAENGD:1)          
242400       Add 1                           To LAENGD                          
242500     End-If                                                               
242600* Sort                                                                    
242700     If WS-FLAGGA-LISTA (25) > Space                                      
242800       Move 'Sort'                     To W002-ART-RAD(LAENGD:4)          
242900       Add  4                          To LAENGD                          
243000       Move TAB-TECKEN                 To W002-ART-RAD(LAENGD:1)          
243100       Add  1                          To LAENGD                          
243200     End-If                                                               
243300* RefStop - RefillSt.                                                     
243400     If WS-FLAGGA-LISTA (26) > Space                                      
243500       Move 'RefStop'                  To W002-ART-RAD(LAENGD:11)         
243600       Add  11                         To LAENGD                          
243700       Move TAB-TECKEN                 To W002-ART-RAD(LAENGD:1)          
243800       Add  1                          To LAENGD                          
243900     End-If                                                               
244000* Weight/Vol - Vikt/Volym                                                 
244100     If WS-FLAGGA-LISTA (27) > Space                                      
244200       Move 'Weight'                   To W002-ART-RAD(LAENGD:6)          
244300       Add  6                          To LAENGD                          
244400       Move TAB-TECKEN                 To W002-ART-RAD(LAENGD:1)          
244500       Add  1                          To LAENGD                          
244600       Move 'Volume'                   To W002-ART-RAD(LAENGD:6)          
244700       Add  6                          To LAENGD                          
244800       Move TAB-TECKEN                 To W002-ART-RAD(LAENGD:1)          
244900       Add  1                          To LAENGD                          
245000     End-If                                                               
245100                                                                          
245200***************************************************************           
245300* RAD 5 from 2422                                                         
245400***************************************************************           
245500* PubWeek - 1:a inlev                                                     
245600     If WS-FLAGGA-LISTA (28) > Space                                      
245700       Move 'PUBL.Week'                To W002-ART-RAD(LAENGD:9)          
245800       Add  9                          To LAENGD                          
245900       Move TAB-TECKEN                 To W002-ART-RAD(LAENGD:1)          
246000       Add  1                          To LAENGD                          
246100     End-If                                                               
246200* PubWeek Loc                                                             
246300     If WS-FLAGGA-LISTA (29) > Space                                      
246400       Move 'PUBL.Week Loc'            To W002-ART-RAD(LAENGD:13)         
246500       Add  13                         To LAENGD                          
246600       Move TAB-TECKEN                 To W002-ART-RAD(LAENGD:1)          
246700       Add  1                          To LAENGD                          
246800     End-If                                                               
246900* OutProd - Utg prod                                                      
247000     If WS-FLAGGA-LISTA (30) > Space                                      
247100       Move 'PROD Stop'                To W002-ART-RAD(LAENGD:9)          
247200       Add  9                          To LAENGD                          
247300       Move TAB-TECKEN                 To W002-ART-RAD(LAENGD:1)          
247400       Add  1                          To LAENGD                          
247500     End-If                                                               
247600* Prodgrp - Prodsl                                                        
247700     If WS-FLAGGA-LISTA (31) > Space                                      
247800       Move 'PGRP'                     To W002-ART-RAD(LAENGD:4)          
247900       Add  4                          To LAENGD                          
248000       Move TAB-TECKEN                 To W002-ART-RAD(LAENGD:1)          
248100       Add  1                          To LAENGD                          
248200     End-If                                                               
248300* Fcngrp - Funkgrp                                                        
248400     If WS-FLAGGA-LISTA (32) > Space                                      
248500       Move 'FCNGRP'                   To W002-ART-RAD(LAENGD:6)          
248600       Add  6                          To LAENGD                          
248700       Move TAB-TECKEN                 To W002-ART-RAD(LAENGD:1)          
248800       Add  1                          To LAENGD                          
248900     End-If                                                               
249000                                                                          
249100* Tot.dem                                                                 
249200*    --- SEPARAT VALFÄLT "TOT.BEH" (utanför  "LISTA" )                    
249300     If IN20-KVVECKOR-KVPB  > Zero                                        
249400       Move IN20-KVVECKOR-KVPB To W-KVVECKOR-RED                          
249500       String 'TOTAL DEM.' W-KVVECKOR-RED ' WEEKS'                        
249600                           TAB-TECKEN                                     
249700        Delimited By Size            Into W002-ART-RAD(LAENGD:19)         
249800       Add  19                         To LAENGD                          
249900     End-If                                                               
250000                                                                          
250100* CallOffs                                                                
250200*    --- SEPARAT VALFÄLT "K.AVROP" (utanför  "LISTA" )                    
250300     If IN20-KVVECKOR-AVROP > Zero                                        
250400       Move IN20-KVVECKOR-AVROP To W-KVVECKOR-RED                         
250500       String 'CALLS ' W-KVVECKOR-RED ' WEEKS'                            
250600                       TAB-TECKEN                                         
250700        Delimited By Size            Into W002-ART-RAD(LAENGD:15)         
250800       Add  15                         To LAENGD                          
250900     End-If                                                               
251000                                                                          
251100***************************************************************           
251200* RAD 6 from 2422                                                         
251300***************************************************************           
251400* Service                                                                 
251500     If WS-FLAGGA-LISTA (33) > Space                                      
251600       Move 'IO LINES DC'              To W002-ART-RAD(LAENGD:20)         
251700       Add  20                         To LAENGD                          
251800       Move TAB-TECKEN                 To W002-ART-RAD(LAENGD:1)          
251900       Add  1                          To LAENGD                          
252000                                                                          
252100       Move 'BOOKED DC'                To W002-ART-RAD(LAENGD:10)         
252200       Add  10                         To LAENGD                          
252300       Move TAB-TECKEN                 To W002-ART-RAD(LAENGD:1)          
252400       Add  1                          To LAENGD                          
252500                                                                          
252600       Move 'SERVICE DC'               To W002-ART-RAD(LAENGD:10)         
252700       Add  10                         To LAENGD                          
252800       Move TAB-TECKEN                 To W002-ART-RAD(LAENGD:1)          
252900       Add  1                          To LAENGD                          
253000                                                                          
253100       Move 'Sort'                     To W002-ART-RAD(LAENGD:4)          
253200       Add  4                          To LAENGD                          
253300       Move TAB-TECKEN                 To W002-ART-RAD(LAENGD:1)          
253400       Add  1                          To LAENGD                          
253500     End-If                                                               
253600* Parameters - Styrparm stämmer antligen inte - TBD Susanne               
253700     If WS-FLAGGA-LISTA (34) > Space                                      
253800       Move 'PRICE CL'                 To W002-ART-RAD(LAENGD:8)          
253900       Add  8                          To LAENGD                          
254000       Move TAB-TECKEN                 To W002-ART-RAD(LAENGD:1)          
254100       Add  1                          To LAENGD                          
254200       Move 'FREQ.CLASS'               To W002-ART-RAD(LAENGD:10)         
254300       Add  10                         To LAENGD                          
254400       Move TAB-TECKEN                 To W002-ART-RAD(LAENGD:1)          
254500       Add  1                          To LAENGD                          
254600       Move 'TABLE'                    To W002-ART-RAD(LAENGD:5)          
254700       Add  5                          To LAENGD                          
254800       Move TAB-TECKEN                 To W002-ART-RAD(LAENGD:1)          
254900       Add  1                          To LAENGD                          
255000     End-If                                                               
255100                                                                          
255200* Quant's - Kvanter stämmer förmodligen inte                              
255300     If WS-FLAGGA-LISTA (35) > Space                                      
255400       Move 'EOQ    '                  To W002-ART-RAD(LAENGD:7)          
255500       Add  7                          To LAENGD                          
255600       Move TAB-TECKEN                 To W002-ART-RAD(LAENGD:1)          
255700       Add  1                          To LAENGD                          
255800       Move 'MIN QTY'                  To W002-ART-RAD(LAENGD:7)          
255900       Add  7                          To LAENGD                          
256000       Move TAB-TECKEN                 To W002-ART-RAD(LAENGD:1)          
256100       Add  1                          To LAENGD                          
256200       Move 'Q-QTY  '                  To W002-ART-RAD(LAENGD:10)         
256300       Add  10                         To LAENGD                          
256400       Move TAB-TECKEN                 To W002-ART-RAD(LAENGD:1)          
256500       Add  1                          To LAENGD                          
256600       Move 'Date Q-qty'               To W002-ART-RAD(LAENGD:10)         
256700       Add  10                         To LAENGD                          
256800       Move TAB-TECKEN                 To W002-ART-RAD(LAENGD:1)          
256900       Add  1                          To LAENGD                          
257000       Move 'MIN.LOAD'                 To W002-ART-RAD(LAENGD:8)          
257100       Add  8                          To LAENGD                          
257200       Move TAB-TECKEN                 To W002-ART-RAD(LAENGD:1)          
257300       Add  1                          To LAENGD                          
257400     End-If                                                               
257500                                                                          
257600* Packinfo - Förp.Info                                                    
257700     If WS-FLAGGA-LISTA (36) > Space                                      
257800       Move '  EMBQ-0'                 To W002-ART-RAD(LAENGD:8 )         
257900       Add  8                          To LAENGD                          
258000       Move TAB-TECKEN                 To W002-ART-RAD(LAENGD:1)          
258100       Add  1                          To LAENGD                          
258200       Move '  EMBQ-1'                 To W002-ART-RAD(LAENGD:8)          
258300       Add  8                          To LAENGD                          
258400       Move TAB-TECKEN                 To W002-ART-RAD(LAENGD:1)          
258500       Add  1                          To LAENGD                          
258600       Move '  EMBQ-2'                 To W002-ART-RAD(LAENGD:8)          
258700       Add  8                          To LAENGD                          
258800       Move TAB-TECKEN                 To W002-ART-RAD(LAENGD:1)          
258900       Add  1                          To LAENGD                          
259000       Move 'PACK CODE'                To W002-ART-RAD(LAENGD:9)          
259100       Add  9                          To LAENGD                          
259200       Move TAB-TECKEN                 To W002-ART-RAD(LAENGD:1)          
259300       Add  1                          To LAENGD                          
259400       Move 'PACK TYPE'                To W002-ART-RAD(LAENGD:9)          
259500       Add  9                          To LAENGD                          
259600       Move TAB-TECKEN                 To W002-ART-RAD(LAENGD:1)          
259700       Add  1                          To LAENGD                          
259800     End-If                                                               
259900* MaxPt                                                                   
260000     If WS-FLAGGA-LISTA (37) > Space                                      
260100       Move 'Maxpoint'                 To W002-ART-RAD(LAENGD:8)          
260200       Add  8                          To LAENGD                          
260300       Move TAB-TECKEN                 To W002-ART-RAD(LAENGD:1)          
260400       Add  1                          To LAENGD                          
260500     End-If                                                               
260600* Blocked - Spärrade                                                      
260700     If WS-FLAGGA-LISTA (38) > Space                                      
260800       Move 'BLOCKED QTY'              To W002-ART-RAD(LAENGD:11)         
260900       Add  11                         To LAENGD                          
261000       Move TAB-TECKEN                 To W002-ART-RAD(LAENGD:1)          
261100       Add  1                          To LAENGD                          
261200     End-If                                                               
261300* Safety Stock - S-lager                                                  
261400     If WS-FLAGGA-LISTA (39) > Space                                      
261500       Move 'Safety Stock'             To W002-ART-RAD(LAENGD:12)         
261600       Add  12                         To LAENGD                          
261700       Move TAB-TECKEN                 To W002-ART-RAD(LAENGD:1)          
261800       Add  1                          To LAENGD                          
261900       Move 'Safety stock date'        To W002-ART-RAD(LAENGD:17)         
262000       Add  17                         To LAENGD                          
262100       Move TAB-TECKEN                 To W002-ART-RAD(LAENGD:1)          
262200       Add  1                          To LAENGD                          
262300     End-If                                                               
262400                                                                          
262500***************************************************************           
262600* RAD 7 from 2422                                                         
262700***************************************************************           
262800* Origin - ursprung                                                       
262900     If WS-FLAGGA-LISTA (40) > Space                                      
263000       Move 'Cntry of origin'          To W002-ART-RAD(LAENGD:15)         
263100       Add  15                         To LAENGD                          
263200       Move TAB-TECKEN                 To W002-ART-RAD(LAENGD:1)          
263300       Add  1                          To LAENGD                          
263400     End-If                                                               
263500* W-H Area - Lag.omr                                                      
263600     If WS-FLAGGA-LISTA (41) > Space                                      
263700       Move 'W-H Area'                 To W002-ART-RAD(LAENGD:8)          
263800       Add  8                          To LAENGD                          
263900       Move TAB-TECKEN                 To W002-ART-RAD(LAENGD:1)          
264000       Add  1                          To LAENGD                          
264100     End-If                                                               
264200* Aut/JIT                                                                 
264300     If WS-FLAGGA-LISTA (42) > Space                                      
264400       Move 'Aut'                      To W002-ART-RAD(LAENGD:3)          
264500       Add  3                          To LAENGD                          
264600       Move TAB-TECKEN                 To W002-ART-RAD(LAENGD:1)          
264700       Add  1                          To LAENGD                          
264800       Move 'Jit'                      To W002-ART-RAD(LAENGD:3)          
264900       Add  3                          To LAENGD                          
265000       Move TAB-TECKEN                 To W002-ART-RAD(LAENGD:1)          
265100       Add  1                          To LAENGD                          
265200     End-If                                                               
265300* Proj/Mod.                                                               
265400     If WS-FLAGGA-LISTA (43) > Space                                      
265500       Move 'Project'                  To W002-ART-RAD(LAENGD:7)          
265600       Add  7                          To LAENGD                          
265700       Move TAB-TECKEN                 To W002-ART-RAD(LAENGD:1)          
265800       Add  1                          To LAENGD                          
265900       Move 'Model-1'                  To W002-ART-RAD(LAENGD:7)          
266000       Add  7                          To LAENGD                          
266100       Move TAB-TECKEN                 To W002-ART-RAD(LAENGD:1)          
266200       Add  1                          To LAENGD                          
266300       Move 'Model-2'                  To W002-ART-RAD(LAENGD:7)          
266400       Add  7                          To LAENGD                          
266500       Move TAB-TECKEN                 To W002-ART-RAD(LAENGD:1)          
266600       Add  1                          To LAENGD                          
266700       Move 'Model-3'                  To W002-ART-RAD(LAENGD:7)          
266800       Add  7                          To LAENGD                          
266900       Move TAB-TECKEN                 To W002-ART-RAD(LAENGD:1)          
267000       Add  1                          To LAENGD                          
267100     End-If                                                               
267200* S-part - Uart/LSR                                                       
267300     If WS-FLAGGA-LISTA (44) > Space                                      
267400       Move 'S-Part'                   To W002-ART-RAD(LAENGD:6)          
267500       Add  6                          To LAENGD                          
267600       Move TAB-TECKEN                 To W002-ART-RAD(LAENGD:1)          
267700       Add  1                          To LAENGD                          
267800     End-If                                                               
267900* IT-status - KR-status                                                   
268000     If WS-FLAGGA-LISTA (45) > Space                                      
268100       Move 'IR ST'                    To W002-ART-RAD(LAENGD:5)          
268200       Add  5                          To LAENGD                          
268300       Move TAB-TECKEN                 To W002-ART-RAD(LAENGD:1)          
268400       Add  1                          To LAENGD                          
268500       Move 'IR NO'                    To W002-ART-RAD(LAENGD:5)          
268600       Add  5                          To LAENGD                          
268700       Move TAB-TECKEN                 To W002-ART-RAD(LAENGD:1)          
268800       Add  1                          To LAENGD                          
268900     End-If                                                               
269000                                                                          
269100     Move Space                        To W-WRITE-PART-ON-EXCEL           
269200     Perform S21-SKRIV-W21720-001                                         
269300     .                                                                    
269400                                                                          
269500 DE-SKAPA-EXCELRAD SECTION.                                               
269600     MOVE 'DE-SKAPA-EXCELRAD           ' TO CURRENT-SECTION               
269700                                                                          
269800     Move Space               To W-WRITE-PART-ON-EXCEL                    
269900                                                                          
270000     Move 1 To LAENGD                                                     
270100                                                                          
270200***************************************************************           
270300* RAD 1 from 2422                                                         
270400***************************************************************           
270500     Move SORTWS-IDARTNR      To WS-IDARTNR                               
270600     Move WS-IDARTNR          To W002-ART-RAD (LAENGD:9)                  
270700     Add 9                    To LAENGD                                   
270800     Move TAB-TECKEN          To W002-ART-RAD(LAENGD:1)                   
270900     Add 1                    To LAENGD                                   
271000                                                                          
271100* Suppl - Lev.                                                            
271200     If WS-FLAGGA-LISTA (01) > Space                                      
271300        Move SORTWS-IDLEVNR   To WS-IDLEVNR                               
271400        Move WS-IDLEVNR       To W002-ART-RAD (LAENGD:5)                  
271500        Add 5                 To LAENGD                                   
271600        Move TAB-TECKEN       To W002-ART-RAD(LAENGD:1)                   
271700        Add 1                 To LAENGD                                   
271800                                                                          
271900        Move SORTWS-IDLEVNR-SHIP To WS-IDLEVNR-SHIP                       
272000        Move WS-IDLEVNR-SHIP  To W002-ART-RAD (LAENGD:5)                  
272100        Add 5                 To LAENGD                                   
272200        Move TAB-TECKEN       To W002-ART-RAD(LAENGD:1)                   
272300        Add 1                 To LAENGD                                   
272400     End-If                                                               
272500                                                                          
272600* SupDescr - Lev.bet                                                      
272700     If WS-FLAGGA-LISTA (02) > Space                                      
272800*       -- Leverantörens Art-Beteckning                                   
272900        Move SORTWS-BELEV     To W002-ART-RAD (LAENGD:25)                 
273000        Add 25                To LAENGD                                   
273100        Move TAB-TECKEN       To W002-ART-RAD(LAENGD:1)                   
273200        Add 1                 To LAENGD                                   
273300     End-If                                                               
273400                                                                          
273500* Descr - Ben.(GB)                                                        
273600     If WS-FLAGGA-LISTA (03) > Space                                      
273700        Move SORTWS-BEART     To W002-ART-RAD (LAENGD:25)                 
273800        Add 25                To LAENGD                                   
273900        Move TAB-TECKEN       To W002-ART-RAD(LAENGD:1)                   
274000        Add 1                 To LAENGD                                   
274100     End-If                                                               
274200                                                                          
274300* Stock                                                                   
274400     If WS-FLAGGA-LISTA (04) > Space                                      
274500        COMPUTE W-KVLS-KVRESS = SORTWS-KVLS - SORTWS-KVRESS               
274600        Move W-KVLS-KVRESS    To W002-ART-RAD (LAENGD:7)                  
274700        Add 7                 To LAENGD                                   
274800        Move TAB-TECKEN       To W002-ART-RAD(LAENGD:1)                   
274900        Add 1                 To LAENGD                                   
275000     End-If                                                               
275100                                                                          
275200* AK                                                                      
275300     If WS-FLAGGA-LISTA (05) > Space                                      
275400*       -- AK                                                             
275500        Compute W-KVAKS-SDC-PAV =                                         
275600                SORTWS-KVAKS-SDC + SORTWS-KVAKS-PAV                       
275700        Move W-KVAKS-SDC-PAV   tO W002-ART-RAD (LAENGD:7)                 
275800        Add 7                  To LAENGD                                  
275900        Move TAB-TECKEN        To W002-ART-RAD(LAENGD:1)                  
276000        Add 1                  To LAENGD                                  
276100     End-If                                                               
276200* VOR/RO                                                                  
276300     If WS-FLAGGA-LISTA (06) > Space                                      
276400*       -- BO                                                             
276500*       1. --  RO-saldo                                                   
276600        Compute W-KVROS-BULK-DAG =                                        
276700                SORTWS-KVROS-BULK + SORTWS-KVROS-DAG                      
276800        Move W-KVROS-BULK-DAG  To W002-ART-RAD (LAENGD:7)                 
276900        Add 7                  To LAENGD                                  
277000        Move TAB-TECKEN        To W002-ART-RAD(LAENGD:1)                  
277100        Add 1                  To LAENGD                                  
277200*       2. --  RO-rader                                                   
277300        Move SORTWS-KVRORAD    To W-KVRORAD                               
277400        Move W-KVRORAD         To W002-ART-RAD (LAENGD:5)                 
277500        Add 5                  To LAENGD                                  
277600        Move TAB-TECKEN        To W002-ART-RAD(LAENGD:1)                  
277700        Add 1                  To LAENGD                                  
277800                                                                          
277900*       2. --  VOR-rader                                                  
278000        Move SORTWS-KVVORKO    To W-KVVORKO                               
278100        Move W-KVVORKO         To W002-ART-RAD (LAENGD:9)                 
278200        Add 9                  To LAENGD                                  
278300        Move TAB-TECKEN        To W002-ART-RAD(LAENGD:1)                  
278400        Add 1                  To LAENGD                                  
278500                                                                          
278600        If WS-FLAGGA-LISTA (06) = 'S'                                     
278700           If SORTWS-KVROS-BULK = 0                                       
278800          And SORTWS-KVROS-DAG  = 0                                       
278900          And SORTWS-KVRORAD    = 0                                       
279000              If W-WRITE-PART-ON-EXCEL not = YES                          
279100                 Move NOO      To W-WRITE-PART-ON-EXCEL                   
279200              End-if                                                      
279300           Else                                                           
279400              Move YES         To W-WRITE-PART-ON-EXCEL                   
279500           End-if                                                         
279600        End-if                                                            
279700     End-If                                                               
279800                                                                          
279900***************************************************************           
280000* RAD 2 from 2422                                                         
280100***************************************************************           
280200* ARREARS                                                                 
280300     If WS-FLAGGA-LISTA (07) > Space                                      
280400*       1. ARREARS                                                        
280500        Move SORTWS-KVSLAP-SUM   To W-KVSLAP-SUM                          
280600        Move W-KVSLAP-SUM        To W002-ART-RAD (LAENGD:7)               
280700        Add 7                    To LAENGD                                
280800        Move TAB-TECKEN          To W002-ART-RAD(LAENGD:1)                
280900        Add 1                    To LAENGD                                
281000                                                                          
281100*       2.  NOT RECEIVED                                                  
281200        Move SORTWS-KVAVIS-NOT-REC To W-KVAVIS-NOT-REC                    
281300        Move W-KVAVIS-NOT-REC To W002-ART-RAD (LAENGD:7)                  
281400        Add 7                 To LAENGD                                   
281500        Move TAB-TECKEN       To W002-ART-RAD(LAENGD:1)                   
281600        Add 1                 To LAENGD                                   
281700                                                                          
281800        If WS-FLAGGA-LISTA (06) = 'S'                                     
281900           If SORTWS-KVSLAP-SUM     = Zero                                
282000          And SORTWS-KVAVIS-NOT-REC = Zero                                
282100              If W-WRITE-PART-ON-EXCEL not = YES                          
282200                 Move NOO      To W-WRITE-PART-ON-EXCEL                   
282300              End-if                                                      
282400           Else                                                           
282500              Move YES         To W-WRITE-PART-ON-EXCEL                   
282600           End-if                                                         
282700        End-if                                                            
282800     End-If                                                               
282900                                                                          
283000* Del.Prom                                                                
283100     If WS-FLAGGA-LISTA (08) > Space                                      
283200*       -- Lev.Besk.                                                      
283300*       1--- Dispatch Week (WDD924)                                       
283400        Move SORTWS-TILEVBSK-AVS To W002-ART-RAD (LAENGD:6)               
283500        Add 6                 To LAENGD                                   
283600        Move TAB-TECKEN       To W002-ART-RAD(LAENGD:1)                   
283700        Add 1                 To LAENGD                                   
283800*       2--- Dispatch Qty (WDD924)                                        
283900        Move SORTWS-KVAVIS-BSKKVAR To W002-ART-RAD (LAENGD:7)             
284000        Add 7                 To LAENGD                                   
284100        Move TAB-TECKEN       To W002-ART-RAD(LAENGD:1)                   
284200        Add 1                 To LAENGD                                   
284300*       3--- CD plan incom. deliv. (WDD924)                               
284400        Move SORTWS-TILEVBSK-INL-DC To WS-TILEVBSK                        
284500        Move WS-TILEVBSK      To W002-ART-RAD (LAENGD:5)                  
284600        Add 5                 To LAENGD                                   
284700        Move TAB-TECKEN       To W002-ART-RAD(LAENGD:1)                   
284800        Add 1                 To LAENGD                                   
284900*       4--- CD avail. Week (2106) (WDD924)                               
285000        Move SORTWS-TILEVBSK-DISP-DC To WS-TILEVBSK                       
285100        Move WS-TILEVBSK      To W002-ART-RAD (LAENGD:5)                  
285200        Add 5                 To LAENGD                                   
285300        Move TAB-TECKEN       To W002-ART-RAD(LAENGD:1)                   
285400        Add 1                 To LAENGD                                   
285500*       5--- X-info Date (EXT on 2106) (WDD925)                           
285600        Move SORTWS-TIBORT-INFO To W-TIBORT-INFO                          
285700        If W-TIBORT-INFO > Zero                                           
285800          Move W-TIBORT-INFO  To W002-ART-RAD (LAENGD:6)                  
285900        End-If                                                            
286000        Add 6                 To LAENGD                                   
286100        Move TAB-TECKEN       To W002-ART-RAD(LAENGD:1)                   
286200        Add 1                 To LAENGD                                   
286300     End-If                                                               
286400                                                                          
286500* Pre.Adv.                                                                
286600     If WS-FLAGGA-LISTA (09) > Space                                      
286700        Move SORTWS-KVAVIS-FORAVIS To W-KVAVIS-FORAVIS                    
286800        Move W-KVAVIS-FORAVIS To W002-ART-RAD (LAENGD:8)                  
286900        Add 8                 To LAENGD                                   
287000        Move TAB-TECKEN       To W002-ART-RAD(LAENGD:1)                   
287100        Add 1                 To LAENGD                                   
287200     End-If                                                               
287300                                                                          
287400* Lead Time                                                               
287500     If WS-FLAGGA-LISTA (10) > Space                                      
287600        Move SORTWS-KVVECKOR-LT To W-KVVECKOR-LT                          
287700        Move W-KVVECKOR-LT    To W002-ART-RAD (LAENGD:3)                  
287800        Add 3                 To LAENGD                                   
287900        Move TAB-TECKEN       To W002-ART-RAD(LAENGD:1)                   
288000        Add 1                 To LAENGD                                   
288100     End-If                                                               
288200                                                                          
288300* Proc                                                                    
288400     If WS-FLAGGA-LISTA (11) > Space                                      
288500        Move SORTWS-IDANSK    To W-IDANSK                                 
288600        Move W-IDANSK         To W002-ART-RAD (LAENGD:3)                  
288700        Add 3                 To LAENGD                                   
288800        Move TAB-TECKEN       To W002-ART-RAD(LAENGD:1)                   
288900        Add 1                 To LAENGD                                   
289000                                                                          
289100        Move SORTWS-IDBERED   To W-IDBERED                                
289200        Move W-IDBERED        To W002-ART-RAD (LAENGD:3)                  
289300        Add 3                 To LAENGD                                   
289400        Move TAB-TECKEN       To W002-ART-RAD(LAENGD:1)                   
289500        Add 1                 To LAENGD                                   
289600     End-If                                                               
289700                                                                          
289800* Send Day - Avs.dag                                                      
289900     If WS-FLAGGA-LISTA (12) > Space                                      
290000        MOVE +0               To IX1                                      
290100        MOVE +0               To IX2                                      
290200        Perform until IX1 = +5                                            
290300          Add +1              To IX1                                      
290400          If SORTWS-TILEVDAG (IX1) not = +0                               
290500             Add +1           To IX2                                      
290600             Move SORTWS-TILEVDAG (IX1) To W-TILEVDAG     (IX2)           
290700             Move ';'                   To W-TILEVDAG-SEM (IX2)           
290800          End-if                                                          
290900        End-Perform                                                       
291000                                                                          
291100        Perform until IX2 = +5                                            
291200          Add +1              To IX2                                      
291300          Move Zero           To W-TILEVDAG     (IX2)                     
291400          Move Space          To W-TILEVDAG-SEM (IX2)                     
291500        End-Perform                                                       
291600                                                                          
291700        Move W-TILEVDAG-TAB   To                                          
291800                              W002-ART-RAD(LAENGD:15)                     
291900        Add 15                To LAENGD                                   
292000        Move TAB-TECKEN       To W002-ART-RAD(LAENGD:1)                   
292100        Add 1                 To LAENGD                                   
292200     End-If                                                               
292300                                                                          
292400* LatestDel - new layout Susanne                                          
292500     If WS-FLAGGA-LISTA (13) > Space                                      
292600        Move +1 To IX                                                     
292700        Perform Until IX > +5                                             
292800          Initialize    W-LATASTE-INLEV                                   
292900                                                                          
293000          If SORTWS-TIAVIDAT-LATE (IX) > Zero                             
293100          Or SORTWS-KVANTAL-LATE  (IX) > Zero                             
293200            Move SORTWS-TIAVIDAT-LATE (IX) To W-TIAVIDAT-LATE             
293300            MOVE SORTWS-IDKUNDRF-LATE (IX) To W-IDKUNDRF-LATE             
293400            Move SORTWS-KVANTAL-LATE  (IX) To W-KVANTAL-LATE              
293500          Else                                                            
293600            If W-TIAVIDAT-LATE = Zero                                     
293700              Inspect W-TIAVIDAT-LATE Replacing All '0' By ' '            
293800            End-If                                                        
293900          End-If                                                          
294000                                                                          
294100          Move W-LATASTE-INLEV To W002-ART-RAD (LAENGD:25)                
294200                                                                          
294300          Add 25               To LAENGD                                  
294400          Move TAB-TECKEN      To W002-ART-RAD(LAENGD:1)                  
294500          Add 1                To LAENGD                                  
294600                                                                          
294700          Add +1 To IX                                                    
294800        End-Perform                                                       
294900     End-If                                                               
295000                                                                          
295100***************************************************************           
295200* RAD 3 from 2422                                                         
295300***************************************************************           
295400* PurchID                                                                 
295500     If WS-FLAGGA-LISTA (14) > Space                                      
295600        Move SORTWS-IDINK     To W-IDINK                                  
295700        Move W-IDINK          To W002-ART-RAD (LAENGD:4)                  
295800        Add 4                 To LAENGD                                   
295900        Move TAB-TECKEN       To W002-ART-RAD(LAENGD:1)                   
296000        Add 1                 To LAENGD                                   
296100     End-If                                                               
296200                                                                          
296300* Agreem - Avtal                                                          
296400     If WS-FLAGGA-LISTA (15) > Space                                      
296500        Move SORTWS-FLAVT     To W002-ART-RAD (LAENGD:1)                  
296600        Add 1                 To LAENGD                                   
296700        Move TAB-TECKEN       To W002-ART-RAD(LAENGD:1)                   
296800        Add 1                 To LAENGD                                   
296900     End-If                                                               
297000                                                                          
297100* PerDem                                                                  
297200     If WS-FLAGGA-LISTA (16) > Space                                      
297300        Add 1                  To LAENGD                                  
297400        Move SORTWS-KVPB-REF   To W-KVPB-REF                              
297500        Move W-KVPB-REF        To W002-ART-RAD (LAENGD:9)                 
297600        Add 9                  To LAENGD                                  
297700        Move TAB-TECKEN        To W002-ART-RAD(LAENGD:1)                  
297800                                                                          
297900        Add 1                  To LAENGD                                  
298000        Compute W-ADVICED = SORTWS-KVPB-REF +                             
298100                            SORTWS-KVPBREOI                               
298200        Move W-ADVICED         To W002-ART-RAD (LAENGD:11)                
298300        Add 11                 To LAENGD                                  
298400        Move TAB-TECKEN        To W002-ART-RAD(LAENGD:1)                  
298500                                                                          
298600        Add 1                  To LAENGD                                  
298700        Move SORTWS-KVPB-PLAN  To W-KVPB-PLAN                             
298800        Move W-KVPB-PLAN       To W002-ART-RAD (LAENGD:9)                 
298900        Add 9                  To LAENGD                                  
299000        Move TAB-TECKEN        To W002-ART-RAD(LAENGD:1)                  
299100        Add 1                  To LAENGD                                  
299200                                                                          
299300        Move ZERO              To W-DAPBPLAN                              
299400        IF SORTWS-DAPBPLAN > 0                                            
299500           If SORTWS-DAPBPLAN < DAGENS-DADATUM                            
299600              Continue                                                    
299700           Else                                                           
299800              Move SORTWS-DAPBPLAN TO W-DAPBPLAN                          
299900           End-if                                                         
300000        End-If                                                            
300100        Move W-DAPBPLAN        To W002-ART-RAD (LAENGD:8)                 
300200        Add 8                  To LAENGD                                  
300300        Move TAB-TECKEN        To W002-ART-RAD(LAENGD:1)                  
300400        Add 1                  To LAENGD                                  
300500     End-If                                                               
300600                                                                          
300700* Trend                                                                   
300800     If WS-FLAGGA-LISTA (17) > Space                                      
300900        Move SORTWS-KVPB-TREND To W-KVPB-TREND                            
301000        Move W-KVPB-TREND     To W002-ART-RAD (LAENGD:10)                 
301100        Add 10                To LAENGD                                   
301200        Move TAB-TECKEN       To W002-ART-RAD(LAENGD:1)                   
301300        Add 1                 To LAENGD                                   
301400                                                                          
301500        Move SORTWS-KVVECKOR-TREND TO W-KVVECKOR-TREND                    
301600        Move W-KVVECKOR-TREND To W002-ART-RAD (LAENGD:2)                  
301700        Add 2                 To LAENGD                                   
301800        Move TAB-TECKEN       To W002-ART-RAD(LAENGD:1)                   
301900        Add 1                 To LAENGD                                   
302000        Move SORTWS-TIDATUM-TREND TO W-TIDATUM-TREND                      
302100        Move W-TIDATUM-TREND  To W002-ART-RAD (LAENGD:6)                  
302200        Add 6                 To LAENGD                                   
302300        Move TAB-TECKEN       To W002-ART-RAD(LAENGD:1)                   
302400        Add 1                 To LAENGD                                   
302500                                                                          
302600        If WS-FLAGGA-LISTA (17) = 'S'                                     
302700           If SORTWS-KVPB-TREND     = 0                                   
302800          And SORTWS-KVVECKOR-TREND = 0                                   
302900          And SORTWS-TIDATUM-TREND  = 0                                   
303000              If W-WRITE-PART-ON-EXCEL Not = YES                          
303100                 Move NOO     To W-WRITE-PART-ON-EXCEL                    
303200              End-if                                                      
303300           Else                                                           
303400              Move YES        To W-WRITE-PART-ON-EXCEL                    
303500           End-if                                                         
303600        End-if                                                            
303700     End-If                                                               
303800                                                                          
303900* Seas.                                                                   
304000     If WS-FLAGGA-LISTA (18) > Space                                      
304100        Move SORTWS-DASEASON  To W-DASEASON                               
304200        Move W-DASEASON       To W002-ART-RAD (LAENGD:8)                  
304300        Add 8                 To LAENGD                                   
304400        Move TAB-TECKEN       To W002-ART-RAD(LAENGD:1)                   
304500        Add 1                 To LAENGD                                   
304600                                                                          
304700        MOVE +0               To IX1                                      
304800        MOVE +0               To IX2                                      
304900        Perform until IX1 = +12                                           
305000          Add +1              To IX1                                      
305100          If SORTWS-RESEASON-PLAN (IX1) not = 0                           
305200             Add +1           To IX2                                      
305300             Move SORTWS-RESEASON-PLAN (IX1)                              
305400                              To W-RESEASON-PLAN (IX2)                    
305500             Move ';'         To W-RESEASON-SEM  (IX2)                    
305600          End-if                                                          
305700        End-Perform                                                       
305800                                                                          
305900        Perform until IX2 = +12                                           
306000          Add +1              To IX2                                      
306100          Move Zero           To W-RESEASON-PLAN (IX2)                    
306200          Move Space       To W-RESEASON-SEM  (IX2)                       
306300        End-Perform                                                       
306400        Move W-REASON-PLAN-TAB To                                         
306500                              W002-ART-RAD(LAENGD:72)                     
306600        Add 72                To LAENGD                                   
306700        Move TAB-TECKEN       To W002-ART-RAD(LAENGD:1)                   
306800        Add 1                 To LAENGD                                   
306900                                                                          
307000        If WS-FLAGGA-LISTA (18) = 'S'                                     
307100           If W-WRITE-PART-ON-EXCEL Not = YES                             
307200              Move NOO        To W-WRITE-PART-ON-EXCEL                    
307300           End-if                                                         
307400        End-if                                                            
307500     End-If                                                               
307600                                                                          
307700* DemHist - Oi ru 12                                                      
307800     If WS-FLAGGA-LISTA (19) > Space                                      
307900        Move SORTWS-KVOI-12-RULL To W-KVOIRULL                            
308000        Move W-KVOIRULL       To W002-ART-RAD (LAENGD:8)                  
308100        Add 8                 To LAENGD                                   
308200        Move TAB-TECKEN       To W002-ART-RAD(LAENGD:1)                   
308300        Add 1                 To LAENGD                                   
308400     End-If                                                               
308500                                                                          
308600* DemHist Year - Oi(iår+5)                                                
308700     If WS-FLAGGA-LISTA (20) > Space                                      
308800*       -- Oi(iår+5)                                                      
308900        Move SORTWS-KVOI-YEAR-0 To W-KVOI-YEAR-0                          
309000        Move SORTWS-KVOI-YEAR-1 To W-KVOI-YEAR-1                          
309100        Move SORTWS-KVOI-YEAR-2 To W-KVOI-YEAR-2                          
309200        Move SORTWS-KVOI-YEAR-3 To W-KVOI-YEAR-3                          
309300        Move SORTWS-KVOI-YEAR-4 To W-KVOI-YEAR-4                          
309400        Move SORTWS-KVOI-YEAR-5 To W-KVOI-YEAR-5                          
309500*       1. OI iår                                                         
309600        Move W-KVOI-YEAR-0    To W002-ART-RAD (LAENGD:8)                  
309700        Add 8                 To LAENGD                                   
309800        Move TAB-TECKEN       To W002-ART-RAD(LAENGD:1)                   
309900        Add 1                 To LAENGD                                   
310000*       2. OI förra året                                                  
310100        Move W-KVOI-YEAR-1    To W002-ART-RAD (LAENGD:8)                  
310200        Add 8                 To LAENGD                                   
310300        Move TAB-TECKEN       To W002-ART-RAD(LAENGD:1)                   
310400        Add 1                 To LAENGD                                   
310500*       3. OI iår minus 2                                                 
310600        Move W-KVOI-YEAR-2    To W002-ART-RAD (LAENGD:8)                  
310700        Add 8                 To LAENGD                                   
310800        Move TAB-TECKEN       To W002-ART-RAD(LAENGD:1)                   
310900        Add 1                 To LAENGD                                   
311000*       4. OI iår minus 3                                                 
311100        Move W-KVOI-YEAR-3    To W002-ART-RAD (LAENGD:8)                  
311200        Add 8                 To LAENGD                                   
311300        Move TAB-TECKEN       To W002-ART-RAD(LAENGD:1)                   
311400        Add 1                 To LAENGD                                   
311500*       5. OI iår minus 4                                                 
311600        Move W-KVOI-YEAR-4    To W002-ART-RAD (LAENGD:8)                  
311700        Add 8                 To LAENGD                                   
311800        Move TAB-TECKEN       To W002-ART-RAD(LAENGD:1)                   
311900        Add 1                 To LAENGD                                   
312000*       6. OI iår minus 5                                                 
312100        Move W-KVOI-YEAR-5    To W002-ART-RAD (LAENGD:8)                  
312200        Add 8                 To LAENGD                                   
312300        Move TAB-TECKEN       To W002-ART-RAD(LAENGD:1)                   
312400        Add 1                 To LAENGD                                   
312500     End-If                                                               
312600                                                                          
312700***************************************************************           
312800* RAD 4 from 2422                                                         
312900***************************************************************           
313000* SS CDC - Erskod                                                         
313100     If WS-FLAGGA-LISTA (21) > Space                                      
313200        Move SORTWS-KDERS     To W-KDERS                                  
313300        Move W-KDERS          To W002-ART-RAD (LAENGD:2)                  
313400        Add 2                 To LAENGD                                   
313500        Move TAB-TECKEN       To W002-ART-RAD(LAENGD:1)                   
313600        Add 1                 To LAENGD                                   
313700     End-If                                                               
313800*                                                                         
313900* SS lOCAL                                                                
314000     If WS-FLAGGA-LISTA (22) > Space                                      
314100        Move SORTWS-FLERSDAT-VIPS  To W002-ART-RAD (LAENGD:1)             
314200        Add 1                      To LAENGD                              
314300        Move TAB-TECKEN            To W002-ART-RAD(LAENGD:1)              
314400        Add 1                      To LAENGD                              
314500     End-If                                                               
314600*                                                                         
314700* Price                                                                   
314800     If WS-FLAGGA-LISTA (23) > Space                                      
314900        Move SORTWS-PRMATRL        To W-PRMATRL                           
315000        Move W-PRMATRL             To W002-ART-RAD (LAENGD:10)            
315100        Add 10                     To LAENGD                              
315200        Move TAB-TECKEN            To W002-ART-RAD(LAENGD:1)              
315300        Add 1                      To LAENGD                              
315400        Move SORTWS-PRAVCOST       To W-PRAVCOST                          
315500        Move W-PRAVCOST            To W002-ART-RAD (LAENGD:10)            
315600        Add 10                     To LAENGD                              
315700        Move TAB-TECKEN            To W002-ART-RAD(LAENGD:1)              
315800        Add 1                      To LAENGD                              
315900     End-If                                                               
316000                                                                          
316100* PackFL                                                                  
316200     If WS-FLAGGA-LISTA (24) > Space                                      
316300        Move SORTWS-KDFPKPRI       To W002-ART-RAD (LAENGD:1)             
316400        Add 1                      To LAENGD                              
316500        Move TAB-TECKEN            To W002-ART-RAD(LAENGD:1)              
316600        Add 1                      To LAENGD                              
316700     End-If                                                               
316800                                                                          
316900* Sort                                                                    
317000     If WS-FLAGGA-LISTA (25) > Space                                      
317100        Move SORTWS-KDSORT    To W-KDSORT                                 
317200        Move W-KDSORT         To W002-ART-RAD (LAENGD:2)                  
317300        Add 2                 To LAENGD                                   
317400        Move TAB-TECKEN       To W002-ART-RAD(LAENGD:1)                   
317500        Add 1                 To LAENGD                                   
317600     End-If                                                               
317700                                                                          
317800* RefStop - RefillSt.                                                     
317900     If WS-FLAGGA-LISTA (26) > Space                                      
318000*       -- Refillst.                                                      
318100        Move SORTWS-TIREFSTO-LOC To W-TIREFSTO                            
318200        If W-TIREFSTO < DAGENS-TIDATUM                                    
318300          Move Space To W-TIREFSTO-GRP                                    
318400        End-If                                                            
318500        Move W-TIREFSTO-GRP   To W002-ART-RAD (LAENGD:6)                  
318600        Add 6                 To LAENGD                                   
318700        Move TAB-TECKEN       To W002-ART-RAD(LAENGD:1)                   
318800        Add 1                 To LAENGD                                   
318900     End-If                                                               
319000                                                                          
319100* Weight/Vol - Vikt/Volym                                                 
319200     If WS-FLAGGA-LISTA (27) > Space                                      
319300*       -- Vikt                                                           
319400        Move SORTWS-VKART     To W-VKART                                  
319500        Move W-VKART          To W002-ART-RAD (LAENGD:7)                  
319600        Add 7                 To LAENGD                                   
319700        Move TAB-TECKEN       To W002-ART-RAD(LAENGD:1)                   
319800        Add 1                 To LAENGD                                   
319900*       -- Volym                                                          
320000        Move SORTWS-VLARTNTO To W-VLARTNTO                                
320100        Move W-VLARTNTO       To W002-ART-RAD (LAENGD:10)                 
320200        Add 10                To LAENGD                                   
320300        Move TAB-TECKEN       To W002-ART-RAD(LAENGD:1)                   
320400        Add 1                 To LAENGD                                   
320500     End-If                                                               
320600                                                                          
320700***************************************************************           
320800* RAD 5 from 2422                                                         
320900***************************************************************           
321000* PubWeek - 1:a inlev                                                     
321100     If WS-FLAGGA-LISTA (28) > Space                                      
321200        Move SORTWS-TIFINLV   To W-TIFINLV                                
321300        Move W-TIFINLV        To W002-ART-RAD (LAENGD:5)                  
321400        Add 5                 To LAENGD                                   
321500        Move TAB-TECKEN       To W002-ART-RAD(LAENGD:1)                   
321600        Add 1                 To LAENGD                                   
321700     End-If                                                               
321800                                                                          
321900* PubWeekLoc                                                              
322000     If WS-FLAGGA-LISTA (29) > Space                                      
322100        Move SORTWS-DAPUBL    To W-DAPUBL                                 
322200        Move W-DAPUBL         To W002-ART-RAD (LAENGD:8)                  
322300        Add 8                 To LAENGD                                   
322400        Move TAB-TECKEN       To W002-ART-RAD(LAENGD:1)                   
322500        Add 1                 To LAENGD                                   
322600     End-If                                                               
322700                                                                          
322800* OutProd                                                                 
322900     If WS-FLAGGA-LISTA (30) > Space                                      
323000        Move SORTWS-TIURPROD To W-TIURPROD                                
323100        Move W-TIURPROD       To W002-ART-RAD (LAENGD:5)                  
323200        Add 5                 To LAENGD                                   
323300        Move TAB-TECKEN       To W002-ART-RAD(LAENGD:1)                   
323400        Add 1                 To LAENGD                                   
323500     End-If                                                               
323600                                                                          
323700* Prodgrp                                                                 
323800     If WS-FLAGGA-LISTA (31) > Space                                      
323900        Move SORTWS-KDPRODSL To W-KDPRODSL                                
324000        Move W-KDPRODSL       To W002-ART-RAD (LAENGD:4)                  
324100        Add 4                 To LAENGD                                   
324200        Move TAB-TECKEN       To W002-ART-RAD(LAENGD:1)                   
324300        Add 1                 To LAENGD                                   
324400     End-If                                                               
324500                                                                          
324600* Funkgrp                                                                 
324700     If WS-FLAGGA-LISTA (32) > Space                                      
324800        Move SORTWS-IDFKNGRP To W-IDFKNGRP                                
324900        Move W-IDFKNGRP       To W002-ART-RAD (LAENGD:4)                  
325000        Add 4                 To LAENGD                                   
325100        Move TAB-TECKEN       To W002-ART-RAD(LAENGD:1)                   
325200        Add 1                 To LAENGD                                   
325300     End-If                                                               
325400                                                                          
325500* Tot.dem                                                                 
325600*    --- SEPARAT VALFÄLT "TOT.BEHOV" (utanför  "LISTA" )                  
325700     If IN20-KVVECKOR-KVPB  > Zero                                        
325800        Move SORTWS-KVPB-SUM-VV To W-KVPB-SUM-VV                          
325900        Move W-KVPB-SUM-VV    To W002-ART-RAD (LAENGD:9)                  
326000        Add 9                 To LAENGD                                   
326100        Move TAB-TECKEN       To W002-ART-RAD(LAENGD:1)                   
326200        Add 1                 To LAENGD                                   
326300     End-If                                                               
326400*                                                                         
326500* CallOffs                                                                
326600*    --- SEPARAT VALFÄLT "K.AVROP" (utanför  "LISTA" )                    
326700     If IN20-KVVECKOR-AVROP  > Zeroes                                     
326800        Move SORTWS-KVAVROP-SUM-VV To W-KVAVROP-SUM-VV                    
326900        Move W-KVAVROP-SUM-VV To W002-ART-RAD (LAENGD:8)                  
327000        Add 8                 To LAENGD                                   
327100        Move TAB-TECKEN       To W002-ART-RAD(LAENGD:1)                   
327200        Add 1                 To LAENGD                                   
327300     End-If                                                               
327400                                                                          
327500***************************************************************           
327600* RAD 6 from 2422                                                         
327700***************************************************************           
327800* Service                                                                 
327900     If WS-FLAGGA-LISTA (33) > Space                                      
328000*       -- Service                                                        
328100*       1. "Inkommande rader DC"                                          
328200        Move SORTWS-SUINKORD  To W-SUINKORD                               
328300        Move W-SUINKORD       To W002-ART-RAD (LAENGD:20)                 
328400        Add 20                To LAENGD                                   
328500        Move TAB-TECKEN       To W002-ART-RAD(LAENGD:1)                   
328600        Add 1                 To LAENGD                                   
328700*       2. "Avbokade rader DC"                                            
328800        Move SORTWS-SUAVBRP   To W-SUAVBRP                                
328900        Move W-SUAVBRP        To W002-ART-RAD (LAENGD:10)                 
329000        Add 10                To LAENGD                                   
329100        Move TAB-TECKEN       To W002-ART-RAD(LAENGD:1)                   
329200        Add 1                 To LAENGD                                   
329300*       3. "ServiceGrad"                                                  
329400        Move SORTWS-RESERVG-NTO To W-RESERVG-NTO                          
329500        Move W-RESERVG-NTO    To W002-ART-RAD (LAENGD:8)                  
329600        Add 8                 To LAENGD                                   
329700        Move TAB-TECKEN       To W002-ART-RAD(LAENGD:1)                   
329800        Add 1                 To LAENGD                                   
329900                                                                          
330000*       4. "Sort code"                                                    
330100        Move SORTWS-KDSORT    To W-KDSORT                                 
330200        Move W-KDSORT         To W002-ART-RAD (LAENGD:2)                  
330300        Add 2                 To LAENGD                                   
330400        Move TAB-TECKEN       To W002-ART-RAD(LAENGD:1)                   
330500        Add 1                 To LAENGD                                   
330600     End-If                                                               
330700*                                                                         
330800* Parameters - Styrparam                                                  
330900     If WS-FLAGGA-LISTA (34) > Space                                      
331000*       -- Styrparam                                                      
331100*       1. "Prisklass                                                     
331200        Move SORTWS-KDPRISKL  To W-KDPRISKL                               
331300        Move W-KDPRISKL       To W002-ART-RAD (LAENGD:1)                  
331400        Add 1                 To LAENGD                                   
331500        Move TAB-TECKEN       To W002-ART-RAD(LAENGD:1)                   
331600        Add 1                 To LAENGD                                   
331700*       2. "Frekvensklass"                                                
331800        Move SORTWS-KDFREKKL To W-KDFREKKL                                
331900        Move W-KDFREKKL       To W002-ART-RAD (LAENGD:1)                  
332000        Add 1                 To LAENGD                                   
332100        Move TAB-TECKEN       To W002-ART-RAD(LAENGD:1)                   
332200        Add 1                 To LAENGD                                   
332300*       3. "TABLE"                                                        
332400        Move SORTWS-IDREFTAB  To W-IDREFTAB                               
332500        Move W-IDREFTAB       To W002-ART-RAD (LAENGD:1)                  
332600        Add 1                 To LAENGD                                   
332700        Move TAB-TECKEN       To W002-ART-RAD(LAENGD:1)                   
332800        Add 1                 To LAENGD                                   
332900     End-If                                                               
333000                                                                          
333100* Quant's - Kvanter stämmer förmodligen inte 41                           
333200     If WS-FLAGGA-LISTA (35) > Space                                      
333300*       -- KVANTER 9 fält                                                 
333400**      1--- EOQ , Q-opt                                                  
333500        Move SORTWS-KVEOQ     To W-KVEOQ                                  
333600        Move W-KVEOQ          To W002-ART-RAD (LAENGD:7)                  
333700        Add 7                 To LAENGD                                   
333800        Move TAB-TECKEN       To W002-ART-RAD(LAENGD:1)                   
333900        Add 1                 To LAENGD                                   
334000*       2--- MIN QTY                                                      
334100        Move SORTWS-KVPALL    To W-KVPALL                                 
334200        Move W-KVPALL         To W002-ART-RAD (LAENGD:7)                  
334300        Add 7                 To LAENGD                                   
334400        Move TAB-TECKEN       To W002-ART-RAD(LAENGD:1)                   
334500        Add 1                 To LAENGD                                   
334600*       3--- Q-kvant                                                      
334700        Move SORTWS-KVREFBER  To W-KVREFBER                               
334800        Move W-KVREFBER       To W002-ART-RAD (LAENGD:7)                  
334900        Add 7                 To LAENGD                                   
335000        Move TAB-TECKEN       To W002-ART-RAD(LAENGD:1)                   
335100        Add 1                 To LAENGD                                   
335200*       4--- Q-Date                                                       
335300        Move SORTWS-TIREFPAF  To W-TIREFPAF                               
335400        Move W-TIREFPAF       To W002-ART-RAD (LAENGD:7)                  
335500        Add 7                 To LAENGD                                   
335600        Move TAB-TECKEN       To W002-ART-RAD(LAENGD:1)                   
335700        Add 1                 To LAENGD                                   
335800**      5--- Min.Kvant                                                    
335900        Move SORTWS-KVULOAD   To W-KVULOAD                                
336000        Move W-KVULOAD        To W002-ART-RAD (LAENGD:7)                  
336100        Add 7                 To LAENGD                                   
336200        Move TAB-TECKEN       To W002-ART-RAD(LAENGD:1)                   
336300        Add 1                 To LAENGD                                   
336400     End-If                                                               
336500                                                                          
336600* PackInfo - Förp.Info                                                    
336700     If WS-FLAGGA-LISTA (36) > Space                                      
336800*       -- Förp.Info 9 fält                                               
336900*       1 till 5 -- EMBQ                                                  
337000        Move SORTWS-IDARTNR-EMBQ0 To W-IDARTNR-EMBQ0                      
337100        Move W-IDARTNR-EMBQ0  To W002-ART-RAD (LAENGD:8)                  
337200        Add 8                 To LAENGD                                   
337300        Move TAB-TECKEN       To W002-ART-RAD(LAENGD:1)                   
337400        Add 1                 To LAENGD                                   
337500        Move SORTWS-IDARTNR-EMBQ1 To W-IDARTNR-EMBQ1                      
337600        Move W-IDARTNR-EMBQ1  To W002-ART-RAD (LAENGD:8)                  
337700        Add 8                 To LAENGD                                   
337800        Move TAB-TECKEN       To W002-ART-RAD(LAENGD:1)                   
337900        Add 1                 To LAENGD                                   
338000        Move SORTWS-IDARTNR-EMBQ2 To W-IDARTNR-EMBQ2                      
338100        Move W-IDARTNR-EMBQ2  To W002-ART-RAD (LAENGD:8)                  
338200        Add 8                 To LAENGD                                   
338300        Move TAB-TECKEN       To W002-ART-RAD(LAENGD:1)                   
338400        Add 1                 To LAENGD                                   
338500                                                                          
338600*       6--- Pack.code                                                    
338700        Move SORTWS-KDFORP    To W-KDFORP                                 
338800        Move W-KDFORP         To W002-ART-RAD (LAENGD:5)                  
338900        Add 5                 To LAENGD                                   
339000        Move TAB-TECKEN       To W002-ART-RAD(LAENGD:1)                   
339100        Add 1                 To LAENGD                                   
339200                                                                          
339300*       7--- Pack.type                                                    
339400        Move SORTWS-BEFT      To W-BEFT                                   
339500        Move W-BEFT           To W002-ART-RAD (LAENGD:3)                  
339600        Add 3                 To LAENGD                                   
339700        Move TAB-TECKEN       To W002-ART-RAD(LAENGD:1)                   
339800        Add 1                 To LAENGD                                   
339900     End-If                                                               
340000                                                                          
340100* MaxPt                                                                   
340200     If WS-FLAGGA-LISTA (37) > Space                                      
340300        Move SORTWS-KVREFOVL To W-KVREFOVL                                
340400        Move W-KVREFOVL       To W002-ART-RAD (LAENGD:7)                  
340500        Add 7                 To LAENGD                                   
340600        Move TAB-TECKEN       To W002-ART-RAD(LAENGD:1)                   
340700        Add 1                 To LAENGD                                   
340800     End-If                                                               
340900                                                                          
341000* Blocked - Spärrade                                                      
341100     If WS-FLAGGA-LISTA (38) > Space                                      
341200*       -- Blocked Qty.                                                   
341300        Move SORTWS-KVSPANT   To W-KVSPANT                                
341400        Move W-KVSPANT        To W002-ART-RAD (LAENGD:6)                  
341500        Add 6                 To LAENGD                                   
341600        Move TAB-TECKEN       To W002-ART-RAD(LAENGD:1)                   
341700        Add 1                 To LAENGD                                   
341800                                                                          
341900        If WS-FLAGGA-LISTA (38) = 'S'                                     
342000           If SORTWS-KVSPANT = Zero                                       
342100              If W-WRITE-PART-ON-EXCEL Not = YES                          
342200                 Move NOO     To W-WRITE-PART-ON-EXCEL                    
342300              End-if                                                      
342400           Else                                                           
342500              Move YES        To W-WRITE-PART-ON-EXCEL                    
342600           End-if                                                         
342700        End-if                                                            
342800     End-If                                                               
342900                                                                          
343000* Safety Stock - S-lager                                                  
343100     If WS-FLAGGA-LISTA (39) > Space                                      
343200*       -- S-lager                                                        
343300*       1. - Safty Stock                                                  
343400        Move SORTWS-KVSLAGER To W-KVSLAGER                                
343500        Move W-KVSLAGER       To W002-ART-RAD (LAENGD:7)                  
343600        Add 7                 To LAENGD                                   
343700        Move TAB-TECKEN       To W002-ART-RAD(LAENGD:1)                   
343800        Add 1                 To LAENGD                                   
343900*       2. - Safty Stock date                                             
344000        Move SORTWS-TIMANSEC  To W-TIMANSEC                               
344100        Move W-TIMANSEC       To W002-ART-RAD (LAENGD:6)                  
344200        Add 6                 To LAENGD                                   
344300        Move TAB-TECKEN       To W002-ART-RAD(LAENGD:1)                   
344400        Add 1                 To LAENGD                                   
344500     End-if                                                               
344600                                                                          
344700***************************************************************           
344800* RAD 7 from 2422                                                         
344900***************************************************************           
345000* Origin - Ursprung                                                       
345100     If WS-FLAGGA-LISTA (40) > Space                                      
345200        Move SORTWS-KDARTURS To W002-ART-RAD (LAENGD:2)                   
345300        Add 2                 To LAENGD                                   
345400        Move TAB-TECKEN       To W002-ART-RAD(LAENGD:1)                   
345500        Add 1                 To LAENGD                                   
345600     End-If                                                               
345700                                                                          
345800* W-H Area - Lag.omr                                                      
345900     If WS-FLAGGA-LISTA (41) > Space                                      
346000*       --LAG.OMR.                                                        
346100        Move SORTWS-ADLAGOMR To W-ADLAGOMR                                
346200        Move SORTWS-ADGANG    To W-ADGANG                                 
346300        Move SORTWS-ADPLATS   To W-ADPLATS                                
346400        Move W-ADART-RED      To W002-ART-RAD (LAENGD:11)                 
346500        Add 11                To LAENGD                                   
346600        Move TAB-TECKEN       To W002-ART-RAD(LAENGD:1)                   
346700        Add 1                 To LAENGD                                   
346800     End-If                                                               
346900                                                                          
347000* Aut/JIT                                                                 
347100     If WS-FLAGGA-LISTA (42) > Space                                      
347200*       -- kdlevplf                                                       
347300        Move SORTWS-KDLEVPLF To W-KDLEVPLF                                
347400        Move W-KDLEVPLF       To W002-ART-RAD (LAENGD:1)                  
347500        Add 1                 To LAENGD                                   
347600        Move TAB-TECKEN       To W002-ART-RAD(LAENGD:1)                   
347700        Add 1                 To LAENGD                                   
347800*       -- fljit                                                          
347900        Move SORTWS-FLJIT     To W-FLJIT                                  
348000        Move W-FLJIT          To W002-ART-RAD (LAENGD:1)                  
348100        Add 1                 To LAENGD                                   
348200        Move TAB-TECKEN       To W002-ART-RAD(LAENGD:1)                   
348300        Add 1                 To LAENGD                                   
348400     End-If                                                               
348500                                                                          
348600* Proj/Mod.                                                               
348700     If WS-FLAGGA-LISTA (43) > Space                                      
348800*       -- Project                                                        
348900        Move SORTWS-IDPROJ    To W002-ART-RAD (LAENGD:4)                  
349000        Add 4                 To LAENGD                                   
349100        Move TAB-TECKEN       To W002-ART-RAD(LAENGD:1)                   
349200        Add 1                 To LAENGD                                   
349300*       -- Model-1                                                        
349400        Move SORTWS-IDKAT(01) To W002-ART-RAD (LAENGD:5)                  
349500        Add 5                 To LAENGD                                   
349600        Move TAB-TECKEN       To W002-ART-RAD(LAENGD:1)                   
349700        Add 1                 To LAENGD                                   
349800*       -- Model-2                                                        
349900        Move SORTWS-IDKAT(02) To W002-ART-RAD (LAENGD:5)                  
350000        Add 5                 To LAENGD                                   
350100        Move TAB-TECKEN       To W002-ART-RAD(LAENGD:1)                   
350200        Add 1                 To LAENGD                                   
350300*       -- Model-3                                                        
350400        Move SORTWS-IDKAT(03) To W002-ART-RAD (LAENGD:5)                  
350500        Add 5                 To LAENGD                                   
350600        Move TAB-TECKEN       To W002-ART-RAD(LAENGD:1)                   
350700        Add 1                 To LAENGD                                   
350800     End-If                                                               
350900                                                                          
351000* S-part - Uart/LSR                                                       
351100     If WS-FLAGGA-LISTA (44) > Space                                      
351200*       -- LSR                                                            
351300        Move SORTWS-FLLSRDEL To W002-ART-RAD (LAENGD:1)                   
351400        Add 1                 To LAENGD                                   
351500        Move TAB-TECKEN       To W002-ART-RAD(LAENGD:1)                   
351600        Add 1                 To LAENGD                                   
351700     End-If                                                               
351800                                                                          
351900* IR-status - KR-status                                                   
352000     If WS-FLAGGA-LISTA (45) > Space                                      
352100*       * KR * (TVÅ KOLUMNER)                                             
352200        Move SORTWS-KDKRSTA   To W-KDKRSTA                                
352300        Move W-KDKRSTA        To W002-ART-RAD (LAENGD:1)                  
352400        Add 1                 To LAENGD                                   
352500        Move TAB-TECKEN       To W002-ART-RAD(LAENGD:1)                   
352600        Add 1                 To LAENGD                                   
352700        Move SORTWS-IDKR      To W-IDKR                                   
352800        Move W-IDKR           To W002-ART-RAD (LAENGD:5)                  
352900        Add 5                 To LAENGD                                   
353000        Move TAB-TECKEN       To W002-ART-RAD(LAENGD:1)                   
353100        Add 1                 To LAENGD                                   
353200     End-If                                                               
353300     .                                                                    
353400                                                                          
353500 DF-KOLLA-STORLEK SECTION.                                                
353600     MOVE 'DF-KOLLA-STORLEK            ' TO CURRENT-SECTION               
353700*    Endast vid EXCEL, andra listor använder ej denna section.            
353800                                                                          
353900     Add LAENGD  TO W-TOTSUM-MB-EXCEL                                     
354000                                                                          
354100     If W-TOTSUM-MB-EXCEL > 4990000                                       
354200        Perform DFA-STOPPA-LISTAN                                         
354300        Set END-OF-SORTFIL To True                                        
354400        Set AVBRYT         To True                                        
354500     End-If                                                               
354600     .                                                                    
354700     EJECT                                                                
354800                                                                          
354900 DFA-STOPPA-LISTAN SECTION.                                               
355000     MOVE 'DFA-STOPPA-LISTAN           ' TO CURRENT-SECTION               
355100                                                                          
355200*******************************************************                   
355300*    HÄR AVBRYTER VI EXCEL-FILEN                                          
355400*    MAX 5 MB TILLÅTES SKRIVAS UT                                         
355500*******************************************************                   
355600                                                                          
355700     Display '*************************************'                      
355800     Display 'OBS  EXCEL-fil för stor: PGM:et avbryts'                    
355900     Display '*************************************'                      
356000                                                                          
356100     Move '*************************************'                         
356200          To W001-ART-RAD                                                 
356300     Perform S21-SKRIV-W21720-001                                         
356400     Move '*************************************'                         
356500          To W001-ART-RAD                                                 
356600     Perform S21-SKRIV-W21720-001                                         
356700     Move 'O B S    O B S   O B S   O B S   O B S'                        
356800          To W001-ART-RAD                                                 
356900     Perform S21-SKRIV-W21720-001                                         
357000     Move 'För stort urval -> för stor EXCEL-fil '                        
357100          To W001-ART-RAD                                                 
357200     Perform S21-SKRIV-W21720-001                                         
357300     Move 'Bearbetningen avbryts, filen ej komplett. '                    
357400          To W001-ART-RAD                                                 
357500     Perform S21-SKRIV-W21720-001                                         
357600     Move '*************************************'                         
357700          To W001-ART-RAD                                                 
357800     Perform S21-SKRIV-W21720-001                                         
357900     Move '*************************************'                         
358000          To W001-ART-RAD                                                 
358100     Perform S21-SKRIV-W21720-001                                         
358200     .                                                                    
358300     EJECT                                                                
358400                                                                          
358500 Z-FINIT SECTION.                                                         
358600     MOVE 'Z-FINIT                     ' TO CURRENT-SECTION               
358700                                                                          
358800     CLOSE W2172010                                                       
358900           W2172020                                                       
359000           W21720-001                                                     
359100                                                                          
359200     Move 'T'         To POSTSUM-OPKOD                                    
359300     Move 'W2172020'  To POSTSUM-FDNAMN                                   
359400     Move 'W21720D2'  To POSTSUM-DDNAMN2                                  
359500     Move 'IN'        To POSTSUM-TRANSTYP                                 
359600     Move ANT-LB-POST To POSTSUM-TOTTRANS                                 
359700     Call POSTSUM Using POSTSUM-PARM                                      
359800                                                                          
359900     Move 'S' To POSTSUM-OPKOD                                            
360000     Call POSTSUM Using POSTSUM-PARM                                      
360100     .                                                                    
360200     EJECT                                                                
360300 S01-LAES-W2172010 SECTION.                                               
360400     MOVE 'S01-LAES-W2172010             ' TO CURRENT-SECTION             
360500                                                                          
360600     Read W2172010 Into IN20-AREA                                         
360700     At End                                                               
360800        Set END-OF-W2172010 To True                                       
360900                                                                          
361000     Not At End                                                           
361100        Move 'W21720' To POSTSUM-FDNAMN                                   
361200        Move 'W21720D1' To POSTSUM-DDNAMN2                                
361300        Move 'PARM'     To POSTSUM-TRANSTYP                               
361400        Call POSTSUM Using POSTSUM-PARM                                   
361500     End-READ                                                             
361600     .                                                                    
361700     EJECT                                                                
361800 S02-LAES-W2172020 SECTION.                                               
361900     MOVE 'S02-LAES-W2172020             ' TO CURRENT-SECTION             
362000                                                                          
362100     Read W2172020 Into IN-PART-AREA                                      
362200     At End                                                               
362300        Set END-OF-W2172020 To True                                       
362400                                                                          
362500     Not At End                                                           
362600        Add +1 To ANT-LB-POST                                             
362700     End-READ                                                             
362800     .                                                                    
362900     EJECT                                                                
363000 S03-LAES-VOR-QUEUE  SECTION.                                             
363100     MOVE 'S03-LAES-VOR-QUEUE          ' TO CURRENT-SECTION               
363200                                                                          
363300     MOVE WS20-IDDC             TO W-IDDC                                 
363400     PERFORM IMS-GU-WDGX4541                                              
363500     IF SEGMENT-FINNS                                                     
363600        MOVE +1                 TO INDX-VOR                               
363700        PERFORM IMS-GNP-WDGX4542                                          
363800        PERFORM UNTIL SEGMENT-SAKNAS                                      
363900                   OR INDX-VOR > MAX-INDX-VOR                             
364000           MOVE 4542-IDARTNR    TO TAB-VOR-IDARTNR (INDX-VOR)             
364100           MOVE 4542-KVBEART-Q  TO TAB-VOR-KVVORKO (INDX-VOR)             
364200           PERFORM IMS-GNP-WDGX4542                                       
364300           ADD +1               TO INDX-VOR                               
364400        END-PERFORM                                                       
364500     END-IF                                                               
364600                                                                          
364700     IF INDX-VOR > MAX-INDX-VOR                                           
364800        MOVE 'CHANGE MAX-INDX-VOR' TO FELTEXT-STR                         
364900        Display FELTEXT                                                   
365000        Perform S99-ABEND                                                 
365100     ELSE                                                                 
365200        PERFORM UNTIL INDX-VOR > MAX-INDX-VOR                             
365300          MOVE +0               TO TAB-VOR-IDARTNR (INDX-VOR)             
365400          MOVE +0               TO TAB-VOR-KVVORKO (INDX-VOR)             
365500          ADD +1                TO INDX-VOR                               
365600        END-PERFORM                                                       
365700     END-IF                                                               
365800     .                                                                    
365900     EJECT                                                                
366000 S21-SKRIV-W21720-001  SECTION.                                           
366100     MOVE 'S21-SKRIV-W21720-001        ' TO CURRENT-SECTION               
366200                                                                          
366300*    -- SMTP klarar högst 1020 byts långa poster.                         
366400*    -- Bättre att trunkera än abenda.                                    
366500                                                                          
366600     If LAENGD > 1020                                                     
366700        Move 1020 To LAENGD                                               
366800     End-If                                                               
366900                                                                          
367000     If W-WRITE-PART-ON-EXCEL = YES or Space                              
367100        Write W21720-001-RAD From W002-DETALJ(1:LAENGD)                   
367200                             After W001-SKIP                              
367300        Move 'LISTA '   To POSTSUM-FDNAMN                                 
367400        Move 'W21720D3' To POSTSUM-DDNAMN2                                
367500        Move 'RAD'      To POSTSUM-TRANSTYP                               
367600        Call POSTSUM Using POSTSUM-PARM                                   
367700     End-if                                                               
367800                                                                          
367900     Move Space To W002-DETALJ                                            
368000     Move 1     To W001-SKIP                                              
368100                                                                          
368200     .                                                                    
368300     EJECT                                                                
368400 S22-DATUMKONV-TILL-AAVVD  SECTION.                                       
368500     MOVE 'S22-DATUMKONV-TILL-AAVVD    ' TO CURRENT-SECTION               
368600                                                                          
368700     MOVE 'AAMMDD'          TO DAT-KDDATFORM                              
368800     CALL WDATKONV USING DAT-KDDATFORM,                                   
368900                         DAT-I-TIDATUM,                                   
369000                         DAT-O-TIDATUM,                                   
369100                         DAT-KDSVAR                                       
369200     .                                                                    
369300     EJECT                                                                
369400                                                                          
369500 S2-LAES-OCH-BEH-EXTINFO SECTION.                                         
369600     MOVE 'S2-LAES-OCH-BEH-EXTINFO     ' TO CURRENT-SECTION               
369700                                                                          
369800     PERFORM IMS-GU-WDD902                                                
369900     IF SEGMENT-FINNS                                                     
370000        MOVE +2 TO W-IDLEVBSK                                             
370100        PERFORM IMS-GNP-WDD925-KVAL                                       
370200        IF SEGMENT-FINNS                                                  
370300           MOVE DAGENS-TIDATUM TO TMP1-YYMMDD                             
370400           MOVE INFO-TIBORT    TO TMP2-YYMMDD                             
370500           PERFORM WY2000P1                                               
370600           IF TMP1-YYMMDD > TMP2-YYMMDD                                   
370700              continue                                                    
370800           ELSE                                                           
370900              MOVE INFO-TIBORT   TO DAT-I-TIDATUM                         
371000              PERFORM S22-DATUMKONV-TILL-AAVVD                            
371100              IF DAT-KDSVAR-OK                                            
371200                 MOVE DAT-TIAAVVD TO SORTWS-TIBORT-INFO                   
371300              END-IF                                                      
371400           END-IF                                                         
371500        END-IF                                                            
371600     END-IF                                                               
371700     .                                                                    
371800     EJECT                                                                
371900                                                                          
372000 S4-LAES-OCH-BEH-EXTINFO2 SECTION.                                        
372100     MOVE 'S4-LAES-OCH-BEH-EXTINFO2    ' TO CURRENT-SECTION               
372200                                                                          
372300     PERFORM IMS-GU-WDD902                                                
372400     IF SEGMENT-FINNS                                                     
372500        MOVE +4 TO W-IDLEVBSK                                             
372600        PERFORM IMS-GNP-WDD925-KVAL                                       
372700        IF SEGMENT-FINNS                                                  
372800           MOVE DAGENS-TIDATUM TO TMP1-YYMMDD                             
372900           MOVE INFO-TIBORT    TO TMP2-YYMMDD                             
373000           PERFORM WY2000P1                                               
373100           IF TMP1-YYMMDD > TMP2-YYMMDD                                   
373200              continue                                                    
373300           ELSE                                                           
373400              MOVE INFO-TIBORT   TO DAT-I-TIDATUM                         
373500              PERFORM S22-DATUMKONV-TILL-AAVVD                            
373600              IF DAT-KDSVAR-OK                                            
373700                 IF SORTWS-TIBORT-INFO NUMERIC                            
373800                 AND SORTWS-TIBORT-INFO > ZERO                            
373900                    MOVE SORTWS-TIBORT-INFO TO TMP1-YYWWD                 
374000                    MOVE DAT-TIAAVVD        TO TMP2-YYWWD                 
374100                    PERFORM WY2000P2                                      
374200                    IF TMP1-YYWWD > TMP2-YYWWD                            
374300                       MOVE DAT-TIAAVVD TO SORTWS-TIBORT-INFO             
374400                    END-IF                                                
374500                 ELSE                                                     
374600                    MOVE DAT-TIAAVVD TO SORTWS-TIBORT-INFO                
374700                 END-IF                                                   
374800              END-IF                                                      
374900           END-IF                                                         
375000        END-IF                                                            
375100     END-IF                                                               
375200     .                                                                    
375300     EJECT                                                                
375400                                                                          
375500 S5-LAES-OCH-BEH-EXTINFO3 SECTION.                                        
375600     MOVE 'S5-LAES-OCH-BEH-EXTINFO3    ' TO CURRENT-SECTION               
375700                                                                          
375800     PERFORM IMS-GU-WDD902                                                
375900     IF SEGMENT-FINNS                                                     
376000        MOVE +5 TO W-IDLEVBSK                                             
376100        PERFORM IMS-GNP-WDD925-KVAL                                       
376200        IF SEGMENT-FINNS                                                  
376300           MOVE DAGENS-TIDATUM  TO TMP1-YYMMDD                            
376400           MOVE INFO-TIBORT   TO TMP2-YYMMDD                              
376500           PERFORM WY2000P1                                               
376600           IF TMP1-YYMMDD > TMP2-YYMMDD                                   
376700              CONTINUE                                                    
376800           ELSE                                                           
376900              MOVE INFO-TIBORT   TO DAT-I-TIDATUM                         
377000              PERFORM S22-DATUMKONV-TILL-AAVVD                            
377100              IF DAT-KDSVAR-OK                                            
377200                 IF SORTWS-TIBORT-INFO NUMERIC                            
377300                 AND SORTWS-TIBORT-INFO > ZERO                            
377400                    MOVE SORTWS-TIBORT-INFO TO TMP1-YYWWD                 
377500                    MOVE DAT-TIAAVVD        TO TMP2-YYWWD                 
377600                    PERFORM WY2000P2                                      
377700                    IF TMP1-YYWWD > TMP2-YYWWD                            
377800                       MOVE DAT-TIAAVVD TO SORTWS-TIBORT-INFO             
377900                    END-IF                                                
378000                 ELSE                                                     
378100                    MOVE DAT-TIAAVVD TO SORTWS-TIBORT-INFO                
378200                 END-IF                                                   
378300              END-IF                                                      
378400           END-IF                                                         
378500        END-IF                                                            
378600     END-IF                                                               
378700     .                                                                    
378800     EJECT                                                                
378900                                                                          
379000 S6-LAES-OCH-BEH-EXTINFO4 SECTION.                                        
379100     MOVE 'S6-LAES-OCH-BEH-EXTINFO4    ' TO CURRENT-SECTION               
379200                                                                          
379300     PERFORM IMS-GU-WDD902                                                
379400     IF SEGMENT-FINNS                                                     
379500        MOVE +6 TO W-IDLEVBSK                                             
379600        PERFORM IMS-GNP-WDD925-KVAL                                       
379700        IF SEGMENT-FINNS                                                  
379800           MOVE DAGENS-TIDATUM TO TMP1-YYMMDD                             
379900           MOVE INFO-TIBORT    TO TMP2-YYMMDD                             
380000           PERFORM WY2000P1                                               
380100           IF TMP1-YYMMDD > TMP2-YYMMDD                                   
380200              CONTINUE                                                    
380300           ELSE                                                           
380400              MOVE INFO-TIBORT   TO DAT-I-TIDATUM                         
380500              PERFORM S22-DATUMKONV-TILL-AAVVD                            
380600              IF DAT-KDSVAR-OK                                            
380700                 IF SORTWS-TIBORT-INFO NUMERIC                            
380800                 AND SORTWS-TIBORT-INFO > ZERO                            
380900                    MOVE SORTWS-TIBORT-INFO TO TMP1-YYWWD                 
381000                    MOVE DAT-TIAAVVD        TO TMP2-YYWWD                 
381100                    PERFORM WY2000P2                                      
381200                    IF TMP1-YYWWD > TMP2-YYWWD                            
381300                       MOVE DAT-TIAAVVD TO SORTWS-TIBORT-INFO             
381400                    END-IF                                                
381500                 ELSE                                                     
381600                    MOVE DAT-TIAAVVD TO SORTWS-TIBORT-INFO                
381700                 END-IF                                                   
381800              END-IF                                                      
381900           END-IF                                                         
382000        END-IF                                                            
382100     END-IF                                                               
382200     .                                                                    
382300     EJECT                                                                
382400                                                                          
382500 S31-SORT-RELEASE  SECTION.                                               
382600     MOVE 'S31-SORT-RELEASE            ' TO CURRENT-SECTION               
382700                                                                          
382800     RELEASE SORT-POST From SORTWS-AREA                                   
382900     .                                                                    
383000     EJECT                                                                
383100                                                                          
383200 S32-SORT-RETURN  SECTION.                                                
383300     MOVE 'S32-SORT-RETURN             ' TO CURRENT-SECTION               
383400                                                                          
383500     Return SORTFIL Into SORTWS-AREA                                      
383600     At End                                                               
383700         Set END-OF-SORTFIL To True                                       
383800        Move 999              To SORTWS-IDANSK                            
383900        Move '99999'          To SORTWS-IDLEVNR                           
384000     End-Return                                                           
384100     .                                                                    
384200     EJECT                                                                
384300                                                                          
384400 S99-ABEND SECTION.                                                       
384500     MOVE 'S99-ABEND                   ' TO CURRENT-SECTION               
384600                                                                          
384700     Move 'T'         To POSTSUM-OPKOD                                    
384800     Move 'W2172020'  To POSTSUM-FDNAMN                                   
384900     Move 'W21720D2'  To POSTSUM-DDNAMN2                                  
385000     Move 'IN'        To POSTSUM-TRANSTYP                                 
385100     Move ANT-LB-POST To POSTSUM-TOTTRANS                                 
385200     Call POSTSUM Using POSTSUM-PARM                                      
385300                                                                          
385400     Move 'S' To POSTSUM-OPKOD                                            
385500     Call POSTSUM Using POSTSUM-PARM                                      
385600     Call ABEND Using RKOD-ABEnd-UTAN-DUMP                                
385700     .                                                                    
385800     EJECT                                                                
385900                                                                          
386000** IMS-sections  ****                                                     
386100                                                                          
386200 IMS-RESTART SECTION.                                                     
386300     SKIP2                                                                
386400     MOVE SPACE TO CHKP-MSG-IO-AREA                                       
386500     MOVE '  ' TO GODK-STATUSKODER                                        
386600     CALL CBLTDLI USING XRST MSG-PCB                                      
386700                        CHKP-MSG-IO-AREA-LENGTH CHKP-MSG-IO-AREA          
386800                        CHKP-AREA-LENGTH CHKP-AREA                        
386900     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
387000     PERFORM IMS-STATUSKONTROLL                                           
387100     .                                                                    
387200     SKIP3                                                                
387300                                                                          
387400 IMS-GU-WDF501 SECTION.                                                   
387500     Move 'IMS-GU-WDF501        ' To IMS-SEKTION                          
387600     String 'WDF501  (IDARTNR  =' W-IDARTNR-X ')'                         
387700          Delimited By Size Into SSA1                                     
387800     Move '  GE' To GODK-STATUSKODER                                      
387900     Call CBLTDLI Using GU WDF5-PCB DLI-IO-AREA-WDF5 SSA1                 
388000     Move WDF5-STATUS-CODE To STATUS-WS                                   
388100     Perform IMS-STATUSKONTROLL                                           
388200     .                                                                    
388300     EJECT                                                                
388400 IMS-GNP-WDF502 SECTION.                                                  
388500     Move 'IMS-GNP-WDF502       ' To IMS-SEKTION                          
388600     String 'WDF502  (WDF5KEY >=' W-WDF5KEY-MIN-X                         
388700                    '&WDF5KEY <=' W-WDF5KEY-MAX-X ')'                     
388800          Delimited By Size Into SSA1                                     
388900     Move '  GE' To GODK-STATUSKODER                                      
389000     Call CBLTDLI Using GNP WDF5-PCB DLI-IO-AREA-WDF5 SSA1                
389100     Move WDF5-STATUS-CODE To STATUS-WS                                   
389200     Perform IMS-STATUSKONTROLL                                           
389300     .                                                                    
389400     EJECT                                                                
389500                                                                          
389600 IMS-GU-WDD902 SECTION.                                                   
389700     Move 'IMS-GU-WDD902        ' To IMS-SEKTION                          
389800     STRING 'WDD901  (WDD901KY =' W-WDD901KY-X ')'                        
389900          DELIMITED BY SIZE   INTO SSA1                                   
390000     String 'WDD902  (IDLEVNR  =' W-IDLEVNR-X ')'                         
390100          Delimited By Size Into SSA2                                     
390200     Move '  GE' To GODK-STATUSKODER                                      
390300     Call CBLTDLI Using GU WDD9-PCB IO-AREA-WDD9 SSA1 SSA2                
390400     Move WDD9-STATUS-CODE To STATUS-WS                                   
390500     Perform IMS-STATUSKONTROLL                                           
390600     .                                                                    
390700     SKIP3                                                                
390800 IMS-GNP-WDD905 SECTION.                                                  
390900     Move 'IMS-GNP-WDD905       ' To IMS-SEKTION                          
391000     String 'WDD905  (KDAVROP  =' W-KDAVROP-X ')'                         
391100          Delimited By Size Into SSA1                                     
391200     Move '  GE' To GODK-STATUSKODER                                      
391300     Call CBLTDLI Using GNP WDD9-PCB IO-AREA-WDD9 SSA1                    
391400     Move WDD9-STATUS-CODE To STATUS-WS                                   
391500     Perform IMS-STATUSKONTROLL                                           
391600     .                                                                    
391700     EJECT                                                                
391800 IMS-GNP-WDD924 SECTION.                                                  
391900     Move 'IMS-GNP-WDD924        ' To IMS-SEKTION                         
392000     String 'WDD901  (WDD901KY =' W-WDD901KY-X ')'                        
392100          Delimited By Size Into SSA1                                     
392200     String 'WDD902  (IDLEVNR  =' W-IDLEVNR-X ')'                         
392300          Delimited By Size Into SSA2                                     
392400     Move 'WDD924   ' To SSA3                                             
392500     Move '  GE' To GODK-STATUSKODER                                      
392600     Call CBLTDLI Using GNP WDD9-PCB IO-AREA-WDD9 SSA1 SSA2 SSA3          
392700     Move WDD9-STATUS-CODE To STATUS-WS                                   
392800     Perform IMS-STATUSKONTROLL                                           
392900     .                                                                    
393000     SKIP3                                                                
393100 IMS-GNP-WDD925-KVAL    SECTION.                                          
393200     Move 'IMS-GNP-WDD925-KVAL   ' To IMS-SEKTION                         
393300                                                                          
393400     STRING 'WDD925  (IDLEVBSK =' W-IDLEVBSK-X ')'                        
393500            DELIMITED BY SIZE INTO SSA1                                   
393600     MOVE '  GE' TO GODK-STATUSKODER                                      
393700     CALL CBLTDLI USING GNP WDD9-PCB IO-AREA-WDD9 SSA1                    
393800     MOVE WDD9-STATUS-CODE TO STATUS-WS                                   
393900     PERFORM IMS-STATUSKONTROLL                                           
394000     .                                                                    
394100 IMS-GN-W6D111-W6D1SEQ SECTION.                                           
394200     Move 'IMS-GN-W6D111-W6D1SEQ' To IMS-SEKTION                          
394300                                                                          
394400     String 'W6D111  (W6D1HSEQ =' W-W6D1HSEQ-X ')'                        
394500            Delimited By Size Into SSA1                                   
394600     Move '  GEGB' To GODK-STATUSKODER                                    
394700     Call CBLTDLI Using GN W6D1-PCB DLI-IO-W6D111 SSA1                    
394800     Move W6D1-STATUS-CODE To STATUS-WS                                   
394900     Perform IMS-STATUSKONTROLL                                           
395000     .                                                                    
395100     EJECT                                                                
395200                                                                          
395300 IMS-GU-W6H701-SEQB SECTION.                                              
395400     Move 'IMS-GU-W6H701-SEQB   ' To IMS-SEKTION                          
395500                                                                          
395600     String 'W6H701  (W6H7BSEQ>=' W-W6H7B1KY-MIN-X                        
395700                    '&W6H7BSEQ<=' W-W6H7B1KY-MAX-X ')'                    
395800          Delimited By Size Into SSA1                                     
395900     Move '  GBGE' To GODK-STATUSKODER                                    
396000     Call CBLTDLI Using GU W6H7-PCB DLI-IO-W6H701 SSA1                    
396100     Move W6H7-STATUS-CODE To STATUS-WS                                   
396200     Perform IMS-STATUSKONTROLL                                           
396300     .                                                                    
396400     EJECT                                                                
396500 IMS-GN-W6H701-SEQB SECTION.                                              
396600     Move 'IMS-GN-W6H701-SEQB   ' To IMS-SEKTION                          
396700                                                                          
396800     String 'W6H701  (W6H7BSEQ >' W-W6H7B1KY-MIN-X                        
396900                    '&W6H7BSEQ<=' W-W6H7B1KY-MAX-X ')'                    
397000          Delimited By Size Into SSA1                                     
397100     Move '  GBGE' To GODK-STATUSKODER                                    
397200     Call CBLTDLI Using GN W6H7-PCB DLI-IO-W6H701 SSA1                    
397300     Move W6H7-STATUS-CODE To STATUS-WS                                   
397400     Perform IMS-STATUSKONTROLL                                           
397500     .                                                                    
397600     EJECT                                                                
397700 IMS-GN-WDA5A  SECTION.                                                   
397800     Move 'IMS-GN-WDA5A         ' To IMS-SEKTION                          
397900     STRING 'WDA5A1  (WDA5A1KY>=' W-WDA5A1KY-MIN                          
398000                    '&WDA5A1KY<=' W-WDA5A1KY-MAX                          
398100                    '&KDSTARAD>=' W-KDSTARAD-MIN                          
398200                    '&KDSTARAD<=' W-KDSTARAD-MAX ')'                      
398300            DELIMITED BY SIZE INTO SSA1                                   
398400     MOVE '  GEGB' TO GODK-STATUSKODER                                    
398500     CALL CBLTDLI USING GN WDA5A-PCB DLI-IO-WDA5A SSA1                    
398600     MOVE WDA5A-STATUS-CODE TO STATUS-WS                                  
398700     PERFORM IMS-STATUSKONTROLL                                           
398800     .                                                                    
398900     EJECT                                                                
399000                                                                          
399100 IMS-GU-WDL601 SECTION.                                                   
399200     STRING 'WDL601  (IDARTNR  =' W-IDARTNR-X ')'                         
399300          DELIMITED BY SIZE INTO SSA1                                     
399400     MOVE '  GE' TO GODK-STATUSKODER                                      
399500     CALL CBLTDLI USING GU WDL6-PCB DLI-IO-WDL601 SSA1                    
399600     MOVE WDL6-STATUS-CODE TO STATUS-WS                                   
399700     PERFORM IMS-STATUSKONTROLL                                           
399800     .                                                                    
399900                                                                          
400000 IMS-GNP-WDL611 SECTION.                                                  
400100     STRING 'WDL611     '                                                 
400200          DELIMITED BY SIZE INTO SSA1                                     
400300     MOVE '  GE' TO GODK-STATUSKODER                                      
400400     CALL CBLTDLI USING GNP WDL6-PCB DLI-IO-WDL611 SSA1                   
400500     MOVE WDL6-STATUS-CODE TO STATUS-WS                                   
400600     PERFORM IMS-STATUSKONTROLL                                           
400700     .                                                                    
400800                                                                          
400900 IMS-GU-WDGX4541 SECTION.                                                 
401000                                                                          
401100     STRING 'WL454101(WDGXKEY  =' W-IDHTYP-X ')'                          
401200          DELIMITED BY SIZE INTO SSA1                                     
401300     MOVE '    ' TO GODK-STATUSKODER                                      
401400     CALL CBLTDLI USING GU  4541-PCB DLI-IO-WDGX4542 SSA1                 
401500     MOVE 4541-STATUS-CODE TO STATUS-WS                                   
401600     PERFORM IMS-STATUSKONTROLL                                           
401700     .                                                                    
401800                                                                          
401900 IMS-GNP-WDGX4542  SECTION.                                               
402000                                                                          
402100     STRING 'WL454111(IDDC     =' W-IDDC-X                                
402200                    '&KDVORATG <' W-KDVORATG-X ')'                        
402300            DELIMITED BY SIZE INTO SSA1                                   
402400     MOVE '  GE'                TO GODK-STATUSKODER                       
402500     CALL CBLTDLI USING GNP 4541-PCB DLI-IO-WDGX4542 SSA1                 
402600     MOVE 4541-STATUS-CODE      TO STATUS-WS                              
402700     PERFORM IMS-STATUSKONTROLL                                           
402800     .                                                                    
402900                                                                          
403000 IMS-STATUSKONTROLL SECTION.                                              
403100                                                                          
403200     Set STATUS-IX To 1                                                   
403300     Search GODK-STATUS                                                   
403400       At End                                                             
403500         String ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
403600         Delimited By Size Into FELTEXT                                   
403700         Call FELLOG                                                      
403800       When GODK-STATUS (STATUS-IX) = STATUS-WS                           
403900         Continue                                                         
404000     End-Search                                                           
404100     .                                                                    
404200                                                                          
404300*    -COPY WY2000P3                                                       
404400     EJECT                                                                
404500*    -COPY WY2000P1                                                       
404600     EJECT                                                                
404700*    -COPY WY2000P2                                                       
404800     EJECT                                                                
