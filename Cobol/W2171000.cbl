000100 PROCESS DYNAM                                                            
000200 ID DIVISION.                                                             
000300     SKIP2                                                                
000400 PROGRAM-ID.     W2171000.                                                
000500*AUTHOR.         PER-ANDERS HELGEGREN.                                    
000600*DATE-WRITTEN.   91/10/18.                                                
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
001700*        SOM VIA SOP-PARAMETRAR                                           
001800*        KOMMIT FRÅN BILD 2322.                                           
001900*        IDUSER FÖR LISTADRESSER                                          
002000*        PRINTER FÖR EV VAL AV PRINTER                                    
002100*        OCH SORTERINGS-ALTERNATIV KOMMER OCKSÅ DENNA VÄG.                
002200*                                                                         
002300*        PROGRAMMET LÄSER      WDD9                                       
002400*        PROGRAMMET LÄSER      WDF1                                       
002500*        PROGRAMMET LÄSER      WDF5                                       
002600*        PROGRAMMET LÄSER      WDK6                                       
002700*        PROGRAMMET LÄSER      WDK7 (VIA SUBPGM W22222)                   
002800*        PROGRAMMET LÄSER      WDK9                                       
002900*        PROGRAMMET LÄSER      WDL2                                       
003000*        PROGRAMMET LÄSER      WDR2                                       
003100*        PROGRAMMET LÄSER      W6D1                                       
003200*        PROGRAMMET LÄSER      W6H7 med W6H7B                             
003300*        PROGRAMMET LÄSER      WDL8 (VIA SUBPGM W222SEAS)                 
003400*        PROGRAMMET LÄSER DB2  TP1ARTK                                    
003500*                MED "JOIN" AV TP1KAMP                                    
003600*        PROGRAMMET LÄSER      WDA5                                       
003700*                                                                         
003800*    ÄNDRING:                                                             
003900*        2003 MARS (CONNY E.)                                             
004000*        SPECIELL SLÄPLISTA UTGÅR (TILLKOMMER I ANNAN SYST.LÖSN)          
004100*        FLER & ÄNDRADE URVALSPARAMETRAR FRÅN 2322. (CTX W21710)          
004200*        NYTT = BL.A. ATT VISSA VAL I FLAGGA-LISTA GENERERAR FLER         
004300*        KOLUMNER ÄN EN, SAMT ATT TVÅ AV LIST-VALEN PÅ 2322 HAR           
004400*        TVÅSIFFRIG INPUT OCH MÅSTE DÄRFÖR LIGGA UTANFÖR LISTAN.          
004500*        (STORA ÄNDRINGAR I B- OCH BA-)                                   
004600*                                                                         
004700*        2006 Maj/Juni  (CONNY E.)                                        
004800*                  Ändring enligt eTracker 855881.                        
004900*                  Tillägg av urvalsfält: Prodsl, Projkod.                
005000*                  Borttag, Ändring och Tillägg av                        
005100*                  listdatafält.  Planerad rel: 06:6                      
005200*        2008 April  (C.E./   )                                           
005300*                  Bug-fix enligt eTracker 6598565.                       
005400*                  i C4-BEH-MULT-KAMPANJ                                  
005500*                                                                         
005600*        2011 November  (Inger Stening)                                   
005700*                  Ändring enligt eTracker 5578283.                       
005800*                  Tagit bort val av skrivare, word, sortering.           
005900*                  Endast excel ark kvar.                                 
006000*                  Ändrat ordningen på urval.                             
006100*                  Nytt sök begrepp: Lag.omr + Gång                       
006200*                  Nya fält: TPO, Dir.lev, Avs.dag, Trend,                
006300*                            Vikt/Volym, Ursprung                         
006400*                  Förändrade fält: VOR/RO, Släp, Säsong, AUT/JIT,        
006500*                            Pris, Proj/Mod, Spärrade, MaxPunkt,          
006600*                            Uart/LSR                                     
006700*                                                                         
006800*        2012 Februari  (Inger Stening)                                   
006900*                  Bug-fix plus ändring enligt eTracker 10164169.         
007000*                  Markert kampanj plus ett antal urval.                  
007100*                  Visa artiklar för de markerade urvalen även om         
007200*                  kampanj inte finns.                                    
007300*                                                                         
007400*        2012 Augusti   (Göran Kjellson)                                  
007500*                  Nyckel WDD901 utökad med IDDC                          
007600*                                                                         
007700*    ABENDKODER:                                                          
007800*        U0016 -  . . . .                                                 
007900*        U1000 -  . . . .                                                 
008000*                                                                         
008100                                                                          
008200     EJECT                                                                
008300 ENVIRONMENT DIVISION.                                                    
008400     SKIP2                                                                
008500 INPUT-OUTPUT SECTION.                                                    
008600                                                                          
008700 FILE-CONTROL.                                                            
008800     SKIP2                                                                
008900*          --- PARAMETRAR FRÅN BILD 2322                                  
009000     Select W21710                     Assign To W21710D1.                
009100     SKIP2                                                                
009200*          --- PULS DAGLAGERFIL FRÅN W2170900                             
009300     Select W21709                     Assign To W21710D2.                
009400     SKIP2                                                                
009500*          --- LISTA ARTIKEL-INFORMATION PAPPERSLISTA                     
009600     Select W21710-001                 Assign To W21710D3.                
009700     EJECT                                                                
009800 DATA DIVISION.                                                           
009900     SKIP2                                                                
010000 FILE SECTION.                                                            
010100     SKIP2                                                                
010200 FD  W21710                                                               
010300     RECORDING       F                                                    
010400     BLOCK CONTAINS  0.                                                   
010500     SKIP2                                                                
010600*01  -COPY W21710      -L.                                                
010700     SKIP3                                                                
010800 FD  W21709                                                               
010900     RECORDING       F                                                    
011000     BLOCK CONTAINS  0.                                                   
011100     SKIP2                                                                
011200*01  -COPY W21709      -L.                                                
011300     SKIP3                                                                
011400 FD  W21710-001                                                           
011500     RECORDING       V                                                    
011600     BLOCK CONTAINS  0                                                    
011700     Record Is Varying From 1 To 1200 Depending On LAENGD-FD.             
011800 01  W21710-001-RAD              PIC X(1200).                             
011900     EJECT                                                                
012000 WORKING-STORAGE SECTION.                                                 
012100     SKIP2                                                                
012200                                                                          
012300*    -COPY WY2000W1                                                       
012400                                                                          
012500*    -COPY WY2000W2                                                       
012600                                                                          
012700*    -- CHECKED BY WY2000                                                 
012800 77  IDPGM                       PIC X(8)    Value 'W2171000'.            
012900 01  FILLER                      PIC X(24) VALUE 'IMS-SEKTION ='.         
013000 77  IMS-SEKTION                 PIC X(30).                               
013100 77  CURRENT-SECTION             PIC X(30)  VALUE SPACE.                  
013200 77  WS-TEXT                     PIC X(30).                               
013300 77  IX-TAB                      PIC S9(4) COMP SYNC.                     
013400 77  IX                          PIC S9(4) COMP SYNC.                     
013500 77  IX1                         PIC S9(4) COMP SYNC.                     
013600 77  IX2                         PIC S9(4) COMP SYNC.                     
013700 77  IX1-TILEVDAG                PIC S9(4) COMP SYNC.                     
013800 77  TIX                         PIC S9(4) COMP SYNC.                     
013900 77  TAB-TECKEN                  PIC X     VALUE x'05'.                   
014000 77  W-POS                       PIC S9(4) COMP SYNC.                     
014100 77  START-POS                   PIC S9(4) COMP SYNC Value Zero.          
014200 77  ANT-LB-POST                 PIC S9(9) COMP SYNC Value Zero.          
014300 77  YES                         PIC X       Value 'Y'.                   
014400 77  JA                          PIC X       Value 'J'.                   
014500 77  NEJ                         PIC X       Value 'N'.                   
014600 77  LAENGD                      PIC 9(4)    Value 0.                     
014700 77  LAENGD-FD                   PIC 9(4)    Value 0.                     
014800 77  LAENGD-MAX                  PIC 9(4)    Value 1200.                  
014900 77  LAENGD-TILEVDAG             PIC S9(4)   Value Zero comp.             
015000 77  BEART-DIFAELT               PIC S9(4)   Value Zero comp.             
015100 77  BEART-BESORD                PIC X(50)   Value Space.                 
015200 77  5-Space                     PIC X(5)    Value Space.                 
015300 77  FLAGGA                      PIC X       Value 'N'.                   
015400 77  FOERSTA-MAIL                PIC X       Value 'J'.                   
015500 77  W-TPO                       PIC X       Value Space.                 
015600 77  W-KVAARLF                   PIC 9(02)   Value Zero.                  
015700 77  WS-PART-IDRESP              PIC 9(3)    VALUE ZERO.                  
015800                                                                          
015900 77  MAX-TAB-IX                  PIC S9(4) COMP SYNC.                     
016000 77  TAB-IX                      PIC S9(4) COMP SYNC.                     
016100 01  WDGX1144-TAB.                                                        
016200     03  TAB-WDGX1144 OCCURS 5000.                                        
016300         05 TAB-IDFKNGRP-FOM     PIC 9(04).                               
016400         05 TAB-IDFKNGRP-TOM     PIC 9(04).                               
016500         05 TAB-KVAARLF          PIC 9(02).                               
016600                                                                          
016700 77  W21710-EOF-SW               PIC X       Value 'N'.                   
016800     88  END-OF-W21710                       Value 'J'.                   
016900                                                                          
017000 77  W21709-EOF-SW               PIC X       Value 'N'.                   
017100     88  END-OF-W21709                       Value 'J'.                   
017200                                                                          
017300 77  SORTFIL-EOF-SW              PIC X       VALUE 'N'.                   
017400     88  END-OF-SORTFIL                      VALUE 'J'.                   
017500                                                                          
017600 77  FIRST-REC-SW                PIC X       VALUE 'J'.                   
017700     88  FIRST-REC                           VALUE 'J'.                   
017800     88  NOT-FIRST-REC                       VALUE 'N'.                   
017900                                                                          
018000 77  WRITE-OUTPUT-SW             PIC X       VALUE 'N'.                   
018100     88  WRITE-OUTPUT                        VALUE 'J'.                   
018200                                                                          
018300 77  URVAL-SW                    PIC X       Value 'N'.                   
018400     88  URVAL-OK                            Value 'J'.                   
018500                                                                          
018600 77  TRAEFF-SW                   PIC X       Value 'N'.                   
018700     88  TRAEFF-OK                           Value 'J'.                   
018800     88  TRAEFF-NEJ                          Value 'N'.                   
018900                                                                          
019000 77  KR-SKRIVEN-SW               PIC X       Value 'N'.                   
019100     88  ARTIKEL-MED-KR-SKRIVEN              Value 'J'.                   
019200     88  ARTIKEL-MED-KR-EJ-SKRIVEN           Value 'N'.                   
019300                                                                          
019400 77  KAMP-SKRIVEN-SW             PIC X       Value 'N'.                   
019500     88  ARTIKEL-MED-KAMP-SKRIVEN            Value 'J'.                   
019600     88  ARTIKEL-MED-KAMP-EJ-SKRIVEN         Value 'N'.                   
019700                                                                          
019800*                                                                         
019900*                                                                         
020000*                                                                         
020100 01  FILLER          PIC X(24) Value 'DATUMBERÄKNINGSAREOR'.              
020200                                                                          
020300 01  DAGENS-DADATUM              PIC 9(8)    Value Zero.                  
020400 01  FILLER REDEFINES DAGENS-DADATUM.                                     
020500     03  DAGENS-DADATUM-SEKEL    PIC 9(2).                                
020600     03  DAGENS-TIDATUM          PIC 9(6).                                
020700                                                                          
020800 01 DAGENS-AAAAVVD               PIC 9(7)    Value Zero.                  
020900 01 FILLER REDEFINES DAGENS-AAAAVVD.                                      
021000     03 DAGENS-SEKEL             PIC 9(2).                                
021100     03 DAGENS-AA                PIC 9(2).                                
021200     03 DAGENS-VVD               PIC X(3).                                
021300 01 FILLER REDEFINES DAGENS-AAAAVVD.                                      
021400     03 DAGENS-DAAVROP           PIC 9(6).                                
021500     03 DAGENS-TILEVDAG          PIC 9(1).                                
021600 01 FILLER REDEFINES DAGENS-AAAAVVD.                                      
021700     03 FILLER                   PIC 9(2).                                
021800     03 DAGENS-TIAAVVD           PIC 9(5).                                
021900 01 FILLER REDEFINES DAGENS-AAAAVVD.                                      
022000     03 FILLER                   PIC 9(2).                                
022100     03 DAGENS-TIAAVV            PIC 9(4).                                
022200     03 DAGENS-DAGNR             PIC 9(1).                                
022300                                                                          
022400* Jämförelsefält för avropsdag mot DAGENS-AAAAVVD                         
022500 01 WS-AVROP-DAAVROP-AVS         PIC 9(7).                                
022600 01 FILLER REDEFINES WS-AVROP-DAAVROP-AVS.                                
022700     03 AVROP-DAAVROP-AVS       PIC 9(6).                                 
022800     03 AVROP-TILEVDAG          PIC 9(1).                                 
022900                                                                          
023000                                                                          
023100 01 WS-ANTAL-URVALSDAGAR         PIC 9(3) Value Zero.                     
023200                                                                          
023300 01 DAAVROP-SLUTVECKA            PIC 9(6).                                
023400 01 FILLER REDEFINES DAAVROP-SLUTVECKA.                                   
023500     03 SLUT-SEKEL               PIC 9(2).                                
023600     03 SLUT-AAVV                PIC 9(4).                                
023700*    03 SLUT-DAG                 PIC 9   .                                
023800                                                                          
023900 01 TMP1-YYWW                    PIC 9(4) Value Zero.                     
024000 01 TMP2-YYWW                    PIC 9(4) Value Zero.                     
024100*                                                                         
024200 01 TILEVBSK-DATUM-KONV.                                                  
024300   03  WS-DALEVBSK-AVS   PIC 9(8).                                        
024400   03  FILLER  REDEFINES WS-DALEVBSK-AVS.                                 
024500       05  WS-DALEVBSK-SS     PIC 9(2).                                   
024600       05  WS-DALEVBSK-AAMMDD PIC 9(6).                                   
024700*                                                                         
024800*                                                                         
024900*                                                                         
025000 01  WS-YEAR                     PIC 9(4).                                
025100 01  OI-ARTAL-0                  PIC 9(4).                                
025200 01  OI-ARTAL-1                  PIC 9(4).                                
025300 01  OI-ARTAL-2                  PIC 9(4).                                
025400 01  OI-ARTAL-3                  PIC 9(4).                                
025500 01  OI-ARTAL-4                  PIC 9(4).                                
025600 01  OI-ARTAL-5                  PIC 9(4).                                
025700                                                                          
025800                                                                          
025900     EJECT                                                                
026000*      --- VALID IDDC CODES                                               
026100*01    -COPY WWDCKONS                                                     
026200*01    -COPY WWDC99                                                       
026300     EJECT                                                                
026400 01  DYNAMISKA-SUBPROGRAM.                                                
026500*                                                                         
026600     03  ABEND                   PIC X(8)    Value 'ABEND'.               
026700     03  POSTSUM                 PIC X(8)    Value 'POSTSUM'.             
026800     SKIP2                                                                
026900*    --- PARAMETRAR TILL ABEND                                            
027000                                                                          
027100 77  RKOD-ABEND-UTAN-DUMP        PIC S9(4)   COMP Value +16.              
027200 77  RKOD-ABEND-MED-DUMP         PIC S9(4)   COMP Value +1000.            
027300     SKIP2                                                                
027400 01  FELTEXT.                                                             
027500     03  FILLER                  PIC X(8)    Value 'FELTEXT'.             
027600     03  FELTEXT-STR             PIC X(72)   Value Space.                 
027700     EJECT                                                                
027800*    --- PARAMETRAR TILL POSTSUM                                          
027900*                                                                         
028000*01  -COPY W0005   -PRE  POSTSUM-                                         
028100     EJECT                                                                
028200     SKIP2                                                                
028300*          TABELL                                                         
028400*    FÖR EVENTUELLA VARIABLER UT PÅ LISTAN                                
028500*    OM START > 0 SKALL DENNA MED, OCH ANGER KOLUMN-POSITION.             
028600*    LÄNGD ANGER VARIABELNS LÄNGD PÅ LISTAN (BA-INIT).                    
028700*    RUB1 = VARIABELNS RUBRIKRAD 1                                        
028800*    RUB2 = VARIABELNS RUBRIKRAD 2                                        
028900*                                                                         
029000                                                                          
029100 01  TABELL.                                                              
029200     03  TABELL-RAD  Occurs 108.                                          
029300*        --- DENNA TABELL INNEHÅLLER ALLA KOLUMNER SOM KAN SKAPAS         
029400*        --- D.V.S. ÄVEN "MULTIPLA" såsom: FörpInfo, SenInlev,            
029500*        --- BestRest, Kampanj, Kvanter, LevBesk, KVOI, PB,               
029600*        --- Service, S-Lager, Släp, Styrparam, Säsong, VOR-RO.           
029700*        --- TAB-ELEMENT INDEXERAS MED TAB-IX                             
029800         05  TAB-START           PIC S9(3) COMP-3  Value Zero.            
029900         05  TAB-LAENGD          PIC S9(3) COMP-3  Value Zero.            
030000         05  TAB-RUB1            PIC X(25) Value Space.                   
030100         05  TAB-RUB2            PIC X(25) Value Space.                   
030200     SKIP2                                                                
030300 01  WS-AREA-PARM1.                                                       
030400     03  WS-IDUSER              PIC X(8).                                 
030500     03  WS-LISTA Occurs 52.                                              
030600         05  WS-FLAGGA-LISTA    PIC X.                                    
030700     03  FILLER                 PIC X(21).                                
030800*                          SUMMA = 80 TECKEN                              
030900                                                                          
031000     EJECT                                                                
031100 01 WS10-AREA.                                                            
031200     03 WS10-IDANSK-FOM     PIC 9(3).                                     
031300* 3                                                                       
031400     03 WS10-IDANSK-TOM     PIC 9(3).                                     
031500* 6                                                                       
031600     03 WS10-IDANSK         PIC 9(3)  Occurs 5 Times.                     
031700*21                                                                       
031800     03 WS10-IDLEVNR        PIC X(5)  Occurs 9 Times.                     
031900*66                                                                       
032000     03 WS10-KVVECKOR-AVROP PIC 9(2).                                     
032100*68                                                                       
032200     03 WS10-KVVECKOR-KVPB  PIC 9(2).                                     
032300*70                                                                       
032400     03 WS10-KDPRODSL-FOM   PIC 9(2).                                     
032500*72                                                                       
032600     03 WS10-KDPRODSL-TOM   PIC 9(2).                                     
032700*74                                                                       
032800     03 WS10-IDPROJ-URV     PIC X(4).                                     
032900*78                                                                       
033000     03 WS10-FL-IDLEVNR-SHIP PIC X(1).                                    
033100*79                                                                       
033200     03 WS10-FL-IDBERED      PIC X(1).                                    
033300*                          SUMMA = 80 TECKEN                              
033400                                                                          
033500 01 FILLER           PIC X(24) Value 'SUMMERINGAR           '.            
033600 01  SUMMERINGAR.                                                         
033700                                                                          
033800     03  BRYTBEGREPP.                                                     
033900         05  OLD-IDANSK          PIC S9(3) COMP-3 Value Zero.             
034000         05  OLD-IDLEVNR         PIC X(5)  Value Space.                   
034100                                                                          
034200     03  RAKNARE.                                                         
034300         05  W-TOTSUM-ANTAL       PIC S9(7) COMP-3 Value Zero.            
034400         05  W-TOTSUM-MB-EXCEL    PIC S9(9) COMP-3 Value Zero.            
034500         05  TOTSUM-ANTAL         PIC Z(5)9        Value Zero.            
034600                                                                          
034700     03  W-KVART-TOT-C1          PIC S9(9) Value Zero COMP-3.             
034800*                                                                         
034900 01 FILLER           PIC X(24) Value 'ARBETSFÄLT KVPB-SUM-VV'.            
035000 01 ARBETSFAELT-KVPB-SUM-VV.                                              
035100     03  W-TIME             PIC   9(8)       Value Zero.                  
035200     03  W-VECKO-SEP-BEHOV  PIC  S9(7)V9(2)  Value Zero COMP-3.           
035300     03  W-DAG-SEP-BEHOV    PIC  S9(7)V9(2)  Value Zero COMP-3.           
035400     03  W-ANTAL-VECKOR     PIC   9(3)       Value Zero COMP-3.           
035500     03  W-KVDAGAR-KVAR     PIC   9(3)       Value Zero COMP-3.           
035600     03  W-TIFINLV-AAVV     PIC  S9(5)                  COMP-3.           
035700     03  W-FAKTOR           PIC  S9(1)V9(3)             COMP-3.           
035800     03  W-KVPB-SEP-SUM     PIC  S9(7)V9(2)  Value Zero COMP-3.           
035900     03  W-KVPB-SATS-SUM    PIC  S9(7)V9(2)  Value Zero COMP-3.           
036000     03  W-KVPB-TPO-SUM     PIC  S9(7)V9(2)  Value Zero COMP-3.           
036100     03  W-KVPB-SDC-SUM     PIC  S9(7)V9(2)  Value Zero COMP-3.           
036200     03  W-KVPB-NDC-SUM     PIC  S9(7)V9(2)  Value Zero COMP-3.           
036300     EJECT                                                                
036400**********************************************************                
036500*    O B S   ÄNDRAS DET HÄR SKALL DET EV. ÄNDRAS I                        
036600*            BA-INITIERA-RUBRIK   OCKSÅ.                                  
036700**********************************************************                
036800                                                                          
036900 01  W-ARBETS-AREOR.                                                      
037000*      ---- Fälten visas i denna ordning på listan                        
037100     03  WS-IDLEVNR              PIC X(5).                                
037200     03  WS-IDLEVNR-SHIP         PIC X(5).                                
037300     03  WS-IDARTNR              PIC Z(9).                                
037400     03  W-LEVBET                PIC X(25).                               
037500     03  W-BEART-S               PIC X(25).                               
037600     03  W-BEART-GB              PIC X(25).                               
037700     03  W-ST-ON-HND-CDC         PIC -(7).                                
037800     03  W-ST-ON-HND-SDC-LDC     PIC -(7).                                
037900     03  W-ST-ON-HND-NDC         PIC -(7).                                
038000     03  W-KVAKS                 PIC -(7).                                
038100     03  W-KVSLAGER              PIC -(7).                                
038200     03  W-RESLJUST              PIC -(1)9v,9.                            
038300     03  W-TISLJUST              PIC -(5).                                
038400     03  W-KVLS-MAXCORE          PIC -(7).                                
038500     03  W-KVROS                 PIC -(7).                                
038600     03  W-KVRORAD               PIC -(5).                                
038700*   KVRORAD=rader på WDA5A1                                               
038800     03  W-KVVORKO               PIC -(7).                                
038900     03  W-KVSLAP-SUM            PIC -(7).                                
039000                                                                          
039100     03  W-KVAVIS-NOT-REC        PIC -(7).                                
039200*         KVAVIS-NOT-REC är summan av alla ej mottagna avrop (R31)        
039300*                med senare TIAVIDAT än dagens datum (WDL221)             
039400     03  W-TILEVBSK-AVS          PIC -(6).                                
039500*    AAVVD TILEVBSK-AVS (AAVVD) FRÅN WDD924 LEV-DALEVBSK-AVS              
039600                                                                          
039700     03  W-KVAVIS-BSKKVAR        PIC -(6).                                
039800*          KVAVIS-BSKKVAR     FRÅN WDD924  LEV-KVAVIS-BSKKVAR             
039900                                                                          
040000     03  W-TILEVBSK-INL-C1       PIC -(6).                                
040100*     AAVVD TILEVBSK-INL-C1    från WDD924  LEV-TILEVBSK-INL              
040200                                                                          
040300     03  W-TILEVBSK-DISP-C1      PIC -(6).                                
040400*     AAVVD TILEVBSK-DISP-C1 "Disp-va" från LEV-TILEVBSK-DISP             
040500                                                                          
040600     03  W-TIBORT-INFO           PIC 9(6).                                
040700*     AAVVD  Date on 2106     från WDD925   INFO-TIBORT                   
040800     03  W-KDPRODSL              PIC Z(2).                                
040900     03  W-IDFKNGRP              PIC Z(4).                                
041000     03  W-KVAVIS-FORAVIS        PIC -(8).                                
041100     03  W-SENASTE-INLEV.                                                 
041200        05 W-TIAVIDAT-SEN        PIC 9(6) Value Zero.                     
041300        05 FILLER                PIC X(1) Value Space.                    
041400        05 W-IDFS-SEN            PIC X(8) Value Space.                    
041500        05 FILLER                PIC X(1) Value Space.                    
041600        05 W-KVANTAL-SEN         PIC -(5)9 Value Zero.                    
041700*                       -"-  SUM LTH = 23                                 
041800     03  W-KVPB-SEP              PIC -(6)9v,9.                            
041900     03  W-KVPB-TOT              PIC -(6)9v,9.                            
042000     03  W-KVPB-SATS             PIC -(6)9v,9.                            
042100     03  W-KVPB-PLAN             PIC -(6)9v,9.                            
042200     03  W-TIPBPLAN              PIC Z(6).                                
042300     03  W-KVOIRULL              PIC Z(7)9.                               
042400     03  W-KVOI-YEAR-0           PIC Z(7)9.                               
042500     03  W-KVOI-YEAR-1           PIC Z(7)9.                               
042600     03  W-KVOI-YEAR-2           PIC Z(7)9.                               
042700     03  W-KVOI-YEAR-3           PIC z(7)9.                               
042800     03  W-KVOI-YEAR-4           PIC z(7)9.                               
042900     03  W-KVOI-YEAR-5           PIC z(7)9.                               
043000     03  W-KVVECKOR-LT           PIC Z(2).                                
043100     03  W-PRARTBES              PIC -(6)9v,99.                           
043200     03  W-PRARTSTD              PIC -(6)9v,99.                           
043300     03  WS-KVBR-VALID-LEV       PIC 9(7).                                
043400     03  W-KVBR-VALID-LEV        PIC -(7).                                
043500     03  WS-KVBR-OVR-LEV         PIC 9(7).                                
043600     03  W-KVBR-OVR-LEV          PIC -(7).                                
043700     03  W-KDAVT                 PIC Z(1).                                
043800     03  W-KDLEVPLF              PIC X(1).                                
043900     03  W-FLJIT                 PIC X(1).                                
044000     03  W-IDPROJ                PIC X(4).                                
044100     03  W-IDKAT                 PIC X(17).                               
044200     03 FILLER REDEFINES W-IDKAT.                                         
044300        05 W-IDKAT1              PIC X(5).                                
044400        05 FILLER                PIC X(1).                                
044500        05 W-IDKAT2              PIC X(5).                                
044600        05 FILLER                PIC X(1).                                
044700        05 W-IDKAT3              PIC X(5).                                
044800     03  W-TIFINLV               PIC 9(5).                                
044900     03  W-TIURPROD              PIC 9(5).                                
045000* -----NEDAN 3 FÄLT FRÅN VAL Säsong.---------------                       
045100     03  W-DASPSEA               PIC -(8).                                
045200     03  W-SEASON-ARTIKEL        PIC X(1).                                
045300     03  W-OSAKERHET             PIC zz9v,9.                              
045400     03  W-TIREFSTO-GRP.                                                  
045500        05 W-TIREFSTO            PIC 9(6).                                
045600     03  W-FLIART                PIC X(1).                                
045700     03  W-KVPB-SUM-VV           PIC -(6)9v,9.                            
045800     03  W-KVVECKOR-RED          PIC Z9.                                  
045900     03  W-KVAVROP-SUM-VV        PIC -(8).                                
046000* -----NEDAN 4 FÄLT FRÅN VAL Styrparam. -----------                       
046100     03  W-KDVVKL                PIC X(1).                                
046200*             KVMAD-SEP FRÅN IN09-KVMAD-SEP                               
046300     03  W-KVMAD-SEP             PIC -(5)9v,9.                            
046400*             KDPRISKL  FRÅN IN09-KDPRISKL                                
046500     03  W-KDPRISKL              PIC X(1).                                
046600*             KDFREKKL  FRÅN IN09-KDFREKLL                                
046700     03  W-KDFREKKL              PIC X(1).                                
046800* -----NEDAN 3 FÄLT FRÅN VAL Service. -----------                         
046900     03  W-KVINORD               PIC -(6).                                
047000     03  W-KVAVBRAD              PIC -(6)9v,99.                           
047100     03  W-SERVG                 PIC -(6)9v,99.                           
047200     03  W-KDSORT                PIC X(2).                                
047300* -----NEDAN 8 FÄLT FRÅN VAL Kvanter. -----------                         
047400*           KVEOQ       FRÅN CLAG-KVEOQ (läs wdk611 här)                  
047500     03  W-KVEOQ                 PIC -(7).                                
047600     03  W-KVPALL                PIC -(7).                                
047700*           KVQ = Q-kvant                                                 
047800     03  W-KVQ                   PIC -(7).                                
047900*           FLMANQ = Q-spärr (2131)                                       
048000     03  W-FLMANQ                PIC X.                                   
048100     03  W-KVQPACK-0             PIC -(5).                                
048200     03  W-KVQPACK-1             PIC -(5).                                
048300     03  W-KVQPACK-2             PIC -(5).                                
048400     03  W-KVQPACK-3             PIC -(5).                                
048500     03  W-KVQPACK-4             PIC -(5).                                
048600     03  W-KVULOAD               PIC -(7).                                
048700*           KVSPARR   FRÅN IN09-KVSPARR-KVAL  "KvalSpärr" (6308)          
048800     03  W-KVSPARR-KVAL          PIC -(7).                                
048900*           FLNYBER   FRÅN CLAG-FLNYBER  (läs wdk611 här)                 
049000     03  W-FLNYBER               PIC X.                                   
049100* -----NEDAN 9 FÄLT FRÅN VAL Förp.Info.----------                         
049200     03  W-IDARTNR-EMBQ0         PIC Z(8).                                
049300     03  W-IDARTNR-EMBQ1         PIC Z(8).                                
049400     03  W-IDARTNR-EMBQ2         PIC Z(8).                                
049500     03  W-IDARTNR-EMBQ3         PIC Z(8).                                
049600     03  W-IDARTNR-EMBQ4         PIC Z(8).                                
049700     03  W-KDFORP                PIC -(5).                                
049800     03  WS-KDFORP               PIC 9(4).                                
049900     03  W-BEFT                  PIC -(3).                                
050000     03  W-PRDIRLON              PIC -(3)9v,999.                          
050100     03  W-PRDMTRL               PIC -(5)9v,999.                          
050200     03  W-KVSPANT               PIC -(5)9.                               
050300     03  W-KDKRSTA               PIC 9.                                   
050400     03  W-IDKR                  PIC Z(4)9.                               
050500     03  W-KDERS                 PIC 99.                                  
050600* -----NEDAN 5 FÄLT FRÅN VAL Kampanj. -----------                         
050700     03  W-IDKAMP                PIC X(7).                                
050800     03  W-IDKAMP-GRP            PIC X(7).                                
050900     03  W-TISTADAT-KAMP         PIC 9(6).                                
051000     03  W-TISTODAT-KAMP         PIC 9(6).                                
051100     03  W-KDKAMP                PIC X(1).                                
051200     03  W-ADART-RED.                                                     
051300        05 W-ADLAGOMR            PIC 99.                                  
051400        05 FILLER                PIC X(1) Value Space.                    
051500        05 W-ADGANG              PIC 99.                                  
051600        05 FILLER                PIC X(1) Value Space.                    
051700        05 W-ADPLATS             PIC 9(5).                                
051800     03  W-ADINPORT              PIC X(8).                                
051900     03  W-IDINK                 PIC x(4).                                
052000     03  W-IDANSK                PIC Z(3).                                
052100     03  W-IDBERED               PIC Z(3).                                
052200     03  W-KDUART                PIC X.                                   
052300     03  W-KDARTURS              PIC X.                                   
052400     03  W-KVART                 PIC -(7).                                
052500     03  W-REDIRLEV              PIC 9(1).9(2).                           
052600     03  W-TILEVDAG-TAB.                                                  
052700         05 W-TILEVDAG    OCCURS 5 PIC -Z(1).                             
052800         05 FILLER               PIC X(1) VALUE SPACE.                    
052900     03  W-KDTECKEN-TREND        PIC X(01).                               
053000     03  W-KVPB-TREND            PIC +(6)9.9.                             
053100     03  W-KVVECKOR-TREND        PIC 9(02).                               
053200     03  W-TIDATUM-TREND         PIC 9(06).                               
053300     03  W-VKART                 PIC Z(06)9.                              
053400     03  W-VLARTNTO              PIC Z(07)9.9.                            
053500     03  W-KVMP                  PIC Z(06)9.                              
053600     03  W-KDFPKPRI              PIC X(1).                                
053700     03  W-KDOTFREK              PIC X(1).                                
053800     EJECT                                                                
053900                                                                          
054000 01  IN10-AREA-START    PIC X(24)   Value 'IN10-AREA-START  '.            
054100*    PARAMETRAR IN , FRÅN BILD 2322 VIA SOP                               
054200*    --- Här ligger SYSIN-data-rad-3 när Pgm-slingan körs.                
054300*01  AREA -COPY W21710     -PRE IN10-                                     
054400                                                                          
054500     EJECT                                                                
054600                                                                          
054700 01  IN09-AREA-START    PIC X(24)   Value 'IN09-AREA-START  '.            
054800*    DAGLAGERFILEN, HÄRIFRÅN HÄMTAS ARTIKELINFO TILL LISTAN               
054900                                                                          
055000*01  AREA -COPY W21709     -PRE IN09-                                     
055100                                                                          
055200     EJECT                                                                
055300                                                                          
055400 01  W002-AREA-START    PIC X(24)   Value 'W002-AREA-START  '.            
055500                                                                          
055600 01  W002-HJALPAREOR.                                                     
055700     03  W002-SKIP               PIC 9(3) COMP-3  Value 1.                
055800                                                                          
055900 01  W002-DETALJ.                                                         
056000     03  FILLER                  PIC X(01)   Value Space.                 
056100     03  W002-ART-RAD            PIC X(2000) Value Space.                 
056200     EJECT                                                                
056300                                                                          
056400 01  W-WRITE-PART-ON-EXCEL       PIC X(01)   Value Space.                 
056500*                                                                         
056600     SKIP2                                                                
056700 01  TEMPWS-AREA-START           PIC X(24)   Value                        
056800                                  'TEMPWS-AREA-START  '.                  
056900*01  AREA -COPY W21710S     -PRE TEMPWS-                                  
057000     EJECT                                                                
057100                                                                          
057200 01  GENERELLA-SUBPROGRAM.                                                
057300     03  WDATKONV                PIC X(8)    Value 'WDATKONV'.            
057400     03  WZ20DAYS                PIC X(8)    Value 'WZ20DAYS'.            
057500     03  W22222                  PIC X(8)    Value 'W22222'.              
057600     03  W222SEAS                PIC X(8)    Value 'W222SEAS'.            
057700     03  CBLTDLI                 PIC X(8)    Value 'CBLTDLI '.            
057800     03  FELLOG                  PIC X(8)    Value 'FELLOG  '.            
057900                                                                          
058000     03 FILLER             PIC X(16)   VALUE 'WS-DB2-SEKTION'.            
058100     03 WS-DB2-SEKTION           PIC X(24)   VALUE SPACE.                 
058200*                                                                         
058300*                                                                         
058400*    --- PARAMETRAR TILL SUBPROGRAM WDATKONV "KONVERTERA DATUM"           
058500 01  FILLER                      PIC X(16)   VALUE 'WDATAREA'.            
058600*01 -COPY WDATAREA                                                        
058700     EJECT                                                                
058800*    --- PARAMETRAR TILL SUBPROGRAM WZ20DAYS "ADDERA DAGAR DATUM"         
058900 01  FILLER                      PIC X(16)   VALUE 'WZ20DAYS'.            
059000*01 -COPY WZ20DAYS                                                        
059100     EJECT                                                                
059200*    *************************************                                
059300*    **  LINK-AREA  BEHOVSTABELL        **                                
059400*    *************************************                                
059500 01  FILLER                      PIC X(16)   VALUE 'W222L222'.            
059600*01  AREA  -COPY W222L222   -PRE LINK-.                                   
059700     EJECT                                                                
059800                                                                          
059900*    *************************************                                
060000*    **  LINKS-AREA SÄSONGS-MODULEN    **                                 
060100*    *************************************                                
060200 01  FILLER                      PIC X(16)   VALUE 'W222SEAS'.            
060300*01  AREA  -COPY W222SEAS   -PRE LINKS-.                                  
060400     EJECT                                                                
060500 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
060600     SKIP3                                                                
060700 01  FILLER                      PIC X(16)   VALUE 'CHKP'.                
060800 01  CHKP-VAR.                                                            
060900     03 CHKP-MSG-IO-AREA-LENGTH  PIC S9(9)   VALUE +32 COMP SYNC.         
061000     03 CHKP-MSG-IO-AREA         PIC X(32)   VALUE SPACE.                 
061100     03 CHKP-AREA-LENGTH         PIC S9(9)   VALUE +32 COMP SYNC.         
061200     03 CHKP-AREA                PIC X(32)   VALUE SPACE.                 
061300     03 CHKP-ANT                 PIC S9(3)   VALUE +0   COMP-3.           
061400     03 CHKP-MAX                 PIC S9(3)   VALUE +100 COMP-3.           
061500                                                                          
061600                                                                          
061700 01  NYCKLAR-TILL-DLI.                                                    
061800                                                                          
061900     03  W-WDF5KEY-MIN-X.                                                 
062000         05  W-IDLEVNR-MIN       PIC X(5)    Value Space.                 
062100         05  W-IDBENR-MIN        PIC X       Value LOW-VALUE.             
062200     03  W-WDF5KEY-MAX-X.                                                 
062300         05  W-IDLEVNR-MAX       PIC X(5)    Value Space.                 
062400         05  W-IDBENR-MAX        PIC X       Value HIGH-VALUE.            
062500                                                                          
062600     03  W-IDARTNR-X.                                                     
062700         05  W-IDARTNR           PIC S9(9)   Value Zero COMP-3.           
062800                                                                          
062900     03  W-WDD901KY-X.                                                    
063000         05  W-IDARTNR-D9        PIC S9(9)   Value Zero COMP-3.           
063100         05  W-IDDC-D9           PIC X(2)    Value Space.                 
063200                                                                          
063300     03  W-IDLEVNR-X.                                                     
063400         05  W-IDLEVNR           PIC X(5)    Value Space.                 
063500                                                                          
063600     03    W-IDLEVBSK-X.                                                  
063700         05 W-IDLEVBSK           PIC S9(1)   VALUE ZERO  COMP-3.          
063800                                                                          
063900     03  W-WDGXKEY-ROT.                                                   
064000          05  FILLER             PIC X(04)   Value '2215'.                
064100          05  FILLER             PIC X(26)   Value LOW-Value.             
064200     03  W-KDAVROP-X.                                                     
064300         05  W-KDAVROP           PIC S9(1)   Value 2 COMP-3.              
064400     03  W-KDSEGKEY-X.                                                    
064500         05 W-KDSEGKEY           PIC X(1)    Value '1'.                   
064600     03  W-W6D1HSEQ-X.                                                    
064700         05  W-IDARTNR-HSEQ      PIC S9(9)   Value Zero  COMP-3.          
064800                                                                          
064900     03  W-W6H7B1KY-MIN-X.                                                
065000         05  W-IDARTNR-H7-MIN      PIC S9(9)   Value Zero COMP-3.         
065100         05  W-DAREGDAT-9KOMPL-MIN PIC  9(8)   Value Zeroes.              
065200         05  W-IDLEVNR-H7-MIN      PIC  X(5)   Value LOW-Value.           
065300         05  W-KVKRKNTR-MIN        PIC S9(1)   Value Zero COMP-3.         
065400*                                     =19 BYTES                           
065500*        05  FILLER                PIC  9(5)   Value Zeroes.              
065600*                                     =24 BYTES                           
065700     03  W-W6H7B1KY-MAX-X.                                                
065800         05  W-IDARTNR-H7-MAX      PIC S9(9)   Value Zero COMP-3.         
065900         05  W-DAREGDAT-9KOMPL-MAX PIC  9(8)   Value 99999999.            
066000         05  W-IDLEVNR-H7-MAX      PIC  X(5)   Value HIGH-Value.          
066100         05  W-KVKRKNTR-MAX        PIC S9(1)   Value +9 COMP-3.           
066200*                                     =19 BYTES                           
066300*        05  FILLER                PIC  9(5)   Value 99999.               
066400*                                     =24 BYTES                           
066500                                                                          
066600     03  W-KDKRSTA-MIN             PIC X(1)    Value '1'.                 
066700     03  W-KDKRSTA-MAX             PIC X(1)    Value '9'.                 
066800                                                                          
066900     03  W-WDA5A1KY-MIN.                                                  
067000         05  W-IDARTNR-MIN        PIC S9(9) COMP-3   VALUE ZERO.          
067100         05  W-IDDC-MIN           PIC X(2)  VALUE SPACE.                  
067200         05  W-FILLER-MIN         PIC X(33) VALUE LOW-VALUE.              
067300     03  W-KDSTARAD-MIN           PIC X     Value Space.                  
067400                                                                          
067500     03  W-WDA5A1KY-MAX.                                                  
067600         05  W-IDARTNR-MAX        PIC S9(9) COMP-3   VALUE ZERO.          
067700         05  W-IDDC-MAX           PIC X(2)  VALUE SPACE.                  
067800         05  W-FILLER-MAX         PIC X(33) VALUE HIGH-VALUE.             
067900     03  W-KDSTARAD-MAX           PIC X     Value Space.                  
068000                                                                          
068100     03  W-WDA501KY.                                                      
068200         05  W-IDDISTR-N2         PIC S9(5) COMP-3   VALUE ZERO.          
068300         05  W-IDKUNDNR-N2        PIC S9(7) COMP-3   VALUE ZERO.          
068400         05  W-IDKUNDRF-N2.                                               
068500             07  W-IDORDNR-N2     PIC 9(5)           VALUE ZERO.          
068600             07  FILLER           PIC X(5)           VALUE SPACE.         
068700         05  W-IDARTNR-N2         PIC S9(9) COMP-3   VALUE ZERO.          
068800         05  W-IDLOPNR-N2         PIC S9(3) COMP-3   VALUE ZERO.          
068900                                                                          
069000     03  W-KDTPOTYP-MIN-X.                                                
069100         05  W-KDTPOTYP-MIN       PIC S9(1)  COMP-3 VALUE ZERO.           
069200     03  W-KDTPOTYP-MAX-X.                                                
069300         05  W-KDTPOTYP-MAX       PIC S9(1)  COMP-3 VALUE ZERO.           
069400                                                                          
069500     03  W-WDGXKEY-1143-X.                                                
069600         05  W-IDHTYP-1143       PIC X(4)    VALUE '1143'.                
069700         05  FILLER              PIC X(26)   VALUE LOW-VALUE.             
069800*    --- IMS FUNKTIONSKODER                                               
069900*01  -COPY W0003                                                          
070000     EJECT                                                                
070100*    ---  DLI INPUT-OUTPUT AREA                                           
070200 01  FILLER                  PIC X(16) Value 'DLI-IO-WDF502'.             
070300     SKIP3                                                                
070400 01  DLI-IO-AREA-WDF5.                                                    
070500*        05  -COPY WDF502                                                 
070600     EJECT                                                                
070700 01  FILLER                  PIC X(16) Value 'DLI-IO-WDD9'.               
070800     SKIP3                                                                
070900 01  DLI-IO-AREA-WDD9.                                                    
071000     03  IO-AREA-WDD9       PIC X(100) Value Space.                       
071100     SKIP3                                                                
071200*    03  -COPY WDD902 -PRE WDD902- -RED IO-AREA-WDD9.                     
071300*    03  -COPY WDD905              -RED IO-AREA-WDD9.                     
071400*    03  -COPY WDD924              -RED IO-AREA-WDD9.                     
071500*    03  -COPY WDD925              -RED IO-AREA-WDD9.                     
071600     EJECT                                                                
071700 01  FILLER         PIC X(16) Value 'DLI-IO-WDL2'.                        
071800 01  DLI-IO-WDL2.                                                         
071900*    03  -COPY WDL221                                                     
072000     EJECT                                                                
072100 01  FILLER         PIC X(16) Value 'DLI-IO-WDK601'.                      
072200 01  DLI-IO-WDK601.                                                       
072300*    03  -COPY WDK601                                                     
072400     EJECT                                                                
072500 01  FILLER         PIC X(16) Value 'DLI-IO-WDK611'.                      
072600 01  DLI-IO-WDK611.                                                       
072700*    03  -COPY WDK611                                                     
072800     EJECT                                                                
072900 01  FILLER         PIC X(16) Value 'DLI-IO-WDK621'.                      
073000 01  DLI-IO-WDK621.                                                       
073100*    03  -COPY WDK621                                                     
073200     EJECT                                                                
073300 01  FILLER         PIC X(16) Value 'DLI-IO-WDK626'.                      
073400 01  DLI-IO-WDK626.                                                       
073500*    03  -COPY WDK626                                                     
073600     EJECT                                                                
073700 01  FILLER         PIC X(16) Value 'DLI-IO-W6D111'.                      
073800 01  DLI-IO-W6D111.                                                       
073900*    03  -COPY W6D111   -PRE W6D1-.                                       
074000     EJECT                                                                
074100 01  FILLER         PIC X(16) Value 'DLI-IO-W6H701'.                      
074200 01  DLI-IO-W6H701.                                                       
074300*    03  -COPY W6H701   -PRE W6H7-.                                       
074400     EJECT                                                                
074500 01  FILLER         PIC X(16) Value 'DLI-IO-WDA501'.                      
074600 01  DLI-IO-WDA5A.                                                        
074700*        05  -COPY WDA5A1                                                 
074800     EJECT                                                                
074900 01  DLI-IO-WDA501.                                                       
075000*        05  -COPY WDA501                                                 
075100     EJECT                                                                
075200 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDGX01'.                      
075300 01  DLI-IO-WDGX01.                                                       
075400*    03  -COPY WDGX01                                                     
075500                                                                          
075600 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDGX1144'.                    
075700 01  DLI-IO-WDGX1144.                                                     
075800*    03  -COPY WDGX1144                                                   
075900                                                                          
076000*    --- STATUS-KOD FRÅN IMS                                              
076100 01  STATUS-WS                   PIC XX.                                  
076200     88  SEGMENT-FINNS                       Value '  '.                  
076300     88  SEGMENT-SLUT                        VALUE 'GB'.                  
076400     88  SEGMENT-FINNS-REDAN                 Value 'II'.                  
076500     88  IMS-EJ-OK                           VALUE 'XD'.                  
076600     88  SEGMENT-SAKNAS            Values ARE 'GE' 'GB'.                  
076700     SKIP2                                                                
076800 01  GODK-STATUSKODER.                                                    
076900     03  GODK-STATUS Occurs 5 Indexed By STATUS-IX PIC XX.                
077000     SKIP3                                                                
077100 01  SSA1                        PIC X(160).                              
077200 01  SSA2                        PIC X(128).                              
077300 01  SSA3                        PIC X(128).                              
077400     EJECT                                                                
077500 01  FILLER                      PIC X(16)   VALUE 'SQLCA-AREA'.          
077600       EXEC SQL INCLUDE SQLCA END-EXEC.                                   
077700                                                                          
077800 01  FILLER                      PIC X(16)   VALUE 'SQLCODE-WS'.          
077900 01  DB2-WS.                                                              
078000     03  SQLCODE-WS              PIC  9(3)   VALUE ZERO.                  
078100         88  CURSOR-OK                       VALUE  000.                  
078200         88  ROW-FOUND                       VALUE  000.                  
078300         88  ROW-NOTFOUND                    VALUE  100.                  
078400         88  ROW-DUPLICATE                   VALUE  803.                  
078500         88  ROW-SEVERAL                     VALUE  811.                  
078600         88  RESOURCE-WRONG                  VALUE  904.                  
078700     03  GOOD-SQLCODECODES.                                               
078800         05  GOOD-SQLCODE OCCURS 5                                        
078900             INDEXED BY SQLCODE-IX PIC 9(3).                              
079000*                                                                         
079100     EJECT                                                                
079200*    ---  DB2 HOST-COPYTEXTER                                             
079300 01  FILLER                      PIC X(16)  VALUE 'TP1ARTK-AREA'.         
079400*01  -COPY TP1ARTK -PRE TP1ARTK-                                          
079500 01  FILLER                      PIC X(16)  VALUE 'TP1KAMP-AREA'.         
079600*01  -COPY TP1KAMP -PRE TP1KAMP-                                          
079700 01  FILLER                      PIC X(16)  VALUE 'BYART-AREA'.           
079800*01  -COPY BYART   -PRE BYART-                                            
079900     EJECT                                                                
080000                                                                          
080100*    ---  DB2 DCL                                                         
080200 01  FILLER                      PIC X(16)   VALUE 'TP1ARTK DCL '.        
080300     EXEC SQL INCLUDE TP1ARTK END-EXEC.                                   
080400 01  FILLER                      PIC X(16)   VALUE 'TP1KAMP DCL '.        
080500     EXEC SQL INCLUDE TP1KAMP END-EXEC.                                   
080600 01  FILLER                      PIC X(16)   VALUE 'BYART DCL '.          
080700     EXEC SQL INCLUDE BYART   END-EXEC.                                   
080800     EJECT                                                                
080900 LINKAGE SECTION.                                                         
081000                                                                          
081100*01  -COPY W0008  -PRE WDF5-                                              
081200     05  FILLER                  PIC X.                                   
081300     EJECT                                                                
081400*01  -COPY W0008  -PRE WDD9-                                              
081500     05  FILLER                  PIC X.                                   
081600     EJECT                                                                
081700*01  -COPY W0008  -PRE WDR2-                                              
081800     05  FILLER                  PIC X.                                   
081900     EJECT                                                                
082000*01  -COPY W0008  -PRE WDK9-                                              
082100     05  FILLER                  PIC X.                                   
082200     EJECT                                                                
082300*01  -COPY W0008  -PRE WDF1-                                              
082400     05  FILLER                  PIC X.                                   
082500     EJECT                                                                
082600*01  -COPY W0008  -PRE WDL2-                                              
082700     05  FILLER                  PIC X.                                   
082800     EJECT                                                                
082900*01  -COPY W0008  -PRE WDK6-                                              
083000     05  FILLER                  PIC X.                                   
083100     EJECT                                                                
083200*01  -COPY W0008  -PRE WDK61-                                             
083300     05  FILLER                  PIC X.                                   
083400     EJECT                                                                
083500*01  -COPY W0008  -PRE W6D1-                                              
083600     05  FILLER                  PIC X.                                   
083700     EJECT                                                                
083800*01  -COPY W0008  -PRE W6H7-                                              
083900     05  FILLER                  PIC X.                                   
084000     EJECT                                                                
084100*01  -COPY W0008  -PRE WDG2-                                              
084200     05  FILLER                  PIC X.                                   
084300     EJECT                                                                
084400*01  -COPY W0008  -PRE WDA5A-                                             
084500     05  FILLER                  PIC X.                                   
084600     EJECT                                                                
084700* TPO                                                                     
084800*01  -COPY W0008  -PRE WDA5A1-                                            
084900     05  FILLER                  PIC X.                                   
085000     EJECT                                                                
085100*01  -COPY W0008  -PRE WDA5-                                              
085200     05  FILLER                  PIC X.                                   
085300     EJECT                                                                
085400*-BEHOVSMODULENS PCB:ER i W2222200                                        
085500 01  W222-WDK6-PCB               PIC X.                                   
085600 01  W222-WDK7-PCB               PIC X.                                   
085700 01  W222-ARTM-PCB               PIC X.                                   
085800 01  W222-2501-PCB               PIC X.                                   
085900 01  W222-WDB6R-PCB              PIC X.                                   
086000 01  W222-WDK7R-PCB              PIC X.                                   
086100 01  W222-WDB6-PCB               PIC X.                                   
086200 01  W222-WDD7-PCB               PIC X.                                   
086300 01  W222-WDK7E-PCB              PIC X.                                   
086400 01  W222-UTIL-WDK6-PCB          PIC X.                                   
086500 01  W222-UTIL-WDK7-PCB          PIC X.                                   
086600 01  W222-UTIL-WDB6-PCB          PIC X.                                   
086700 01  W222-UTUP-WDK7-PCB          PIC X.                                   
086800 01  W222-UTUP-WDB6-PCB          PIC X.                                   
086900 01  W222-UTUP-UTIL-WDK6-PCB     PIC X.                                   
087000 01  W222-UTUP-UTIL-WDK7-PCB     PIC X.                                   
087100 01  W222-UTUP-UTIL-WDB6-PCB     PIC X.                                   
087200     EJECT                                                                
087300                                                                          
087400* SÄSONGSMODULENS PCB i W222SEAS                                          
087500*01  -COPY W0008  -PRE WDL8-.                                             
087600     05  FILLER                  PIC X.                                   
087700     EJECT                                                                
087800                                                                          
087900 PROCEDURE DIVISION USING                                                 
088000      WDF5-PCB  WDD9-PCB   WDL2-PCB WDK6-PCB                              
088100      WDK61-PCB W6D1-PCB   W6H7-PCB WDG2-PCB                              
088200      WDA5A-PCB WDA5A1-PCB WDA5-PCB                                       
088300      W222-WDK6-PCB W222-WDK7-PCB   W222-ARTM-PCB                         
088400      W222-2501-PCB W222-WDB6R-PCB  W222-WDK7R-PCB                        
088500      W222-WDB6-PCB W222-WDD7-PCB   W222-WDK7E-PCB                        
088600      W222-UTIL-WDK6-PCB                                                  
088700      W222-UTIL-WDK7-PCB                                                  
088800      W222-UTIL-WDB6-PCB                                                  
088900      W222-UTUP-WDK7-PCB                                                  
089000      W222-UTUP-WDB6-PCB                                                  
089100      W222-UTUP-UTIL-WDK6-PCB                                             
089200      W222-UTUP-UTIL-WDK7-PCB                                             
089300      W222-UTUP-UTIL-WDB6-PCB                                             
089400      WDL8-PCB.                                                           
089500                                                                          
089600     ENTRY 'DLITCBL' USING                                                
089700      WDF5-PCB  WDD9-PCB   WDL2-PCB WDK6-PCB                              
089800      WDK61-PCB W6D1-PCB   W6H7-PCB WDG2-PCB                              
089900      WDA5A-PCB WDA5A1-PCB WDA5-PCB                                       
090000      W222-WDK6-PCB W222-WDK7-PCB   W222-ARTM-PCB                         
090100      W222-2501-PCB W222-WDB6R-PCB  W222-WDK7R-PCB                        
090200      W222-WDB6-PCB W222-WDD7-PCB   W222-WDK7E-PCB                        
090300      W222-UTIL-WDK6-PCB                                                  
090400      W222-UTIL-WDK7-PCB                                                  
090500      W222-UTIL-WDB6-PCB                                                  
090600      W222-UTUP-WDK7-PCB                                                  
090700      W222-UTUP-WDB6-PCB                                                  
090800      W222-UTUP-UTIL-WDK6-PCB                                             
090900      W222-UTUP-UTIL-WDK7-PCB                                             
091000      W222-UTUP-UTIL-WDB6-PCB                                             
091100      WDL8-PCB.                                                           
091200                                                                          
091300     PERFORM A-INIT                                                       
091400                                                                          
091500     PERFORM S02-LAES-W21709                                              
091600     PERFORM B-PRERPARE-UT-EXCEL                                          
091700     PERFORM UNTIL END-OF-W21709                                          
091800       PERFORM C-PROCESS-INPUT                                            
091900       PERFORM S02-LAES-W21709                                            
092000     END-PERFORM                                                          
092100                                                                          
092200     PERFORM Z-FINIT                                                      
092300                                                                          
092400     MOVE ZERO TO RETURN-CODE                                             
092500     GOBACK                                                               
092600     .                                                                    
092700     EJECT                                                                
092800 A-INIT SECTION.                                                          
092900     MOVE 'A-INIT              '     TO CURRENT-SECTION                   
093000                                                                          
093100     OPEN INPUT  W21710                                                   
093200                 W21709                                                   
093300     OPEN OUTPUT W21710-001                                               
093400                                                                          
093500     MOVE "IDAG  " TO DAT-KDDATFORM                                       
093600     CALL WDATKONV USING DAT-KDDATFORM DAT-I-TIDATUM                      
093700                         DAT-O-TIDATUM DAT-KDSVAR                         
093800     IF DAT-KDSVAR-OK                                                     
093900        MOVE DAT-TIAAMMDD         TO DAGENS-TIDATUM                       
094000        MOVE DAT-TISEKEL          TO DAGENS-SEKEL                         
094100                                     DAGENS-DADATUM-SEKEL                 
094200        MOVE DAT-TIAA             TO DAGENS-AA                            
094300        MOVE DAT-TIAAVVD-GRP(3:3) TO DAGENS-VVD                           
094400        MOVE DAT-TID              TO LINK-TID-AKTUELL                     
094500     ELSE                                                                 
094600        MOVE 'FEL FRÅN DATKONV - DAGENS DATUM' TO FELTEXT                 
094700        CALL FELLOG                                                       
094800     END-IF                                                               
094900     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
095000                                                                          
095100     MOVE FUNCTION CURRENT-DATE (1:4) TO WS-YEAR                          
095200     COMPUTE OI-ARTAL-0 = WS-YEAR - 0                                     
095300     COMPUTE OI-ARTAL-1 = WS-YEAR - 1                                     
095400     COMPUTE OI-ARTAL-2 = WS-YEAR - 2                                     
095500     COMPUTE OI-ARTAL-3 = WS-YEAR - 3                                     
095600     COMPUTE OI-ARTAL-4 = WS-YEAR - 4                                     
095700     COMPUTE OI-ARTAL-5 = WS-YEAR - 5                                     
095800                                                                          
095900**********************************************                            
096000*    HÄR LÄSES PARAMETRARNA FRÅN BILD 2322 IN                             
096100**********************************************                            
096200                                                                          
096300*    --- LÄS INDATA RAD 1 FRÅN 2322                                       
096400     PERFORM S01-LAES-W21710                                              
096500     IF NOT END-OF-W21710                                                 
096600                                                                          
096700       MOVE IN10-RAD1 TO WS-AREA-PARM1                                    
096800     ELSE                                                                 
096900       MOVE 'PARAMETRAR FRÅN SOP SAKNAS '                                 
097000             TO FELTEXT-STR                                               
097100       DISPLAY FELTEXT                                                    
097200       PERFORM S99-ABEND                                                  
097300     END-IF                                                               
097400                                                                          
097500*    --- LÄS INDATA RAD 2 FRÅN 2322  = URVAL1                             
097600     PERFORM S01-LAES-W21710                                              
097700     IF NOT END-OF-W21710                                                 
097800                                                                          
097900       MOVE IN10-RAD2 TO WS10-AREA                                        
098000                                                                          
098100     ELSE                                                                 
098200       MOVE 'PARAMETERRAD 2 OCH 3 FRÅN SOP SAKNAS '                       
098300             TO FELTEXT-STR                                               
098400       DISPLAY FELTEXT                                                    
098500       PERFORM S99-ABEND                                                  
098600     END-IF                                                               
098700                                                                          
098800     DISPLAY '*********** PARAMETRAR:   '                                 
098900     DISPLAY WS-AREA-PARM1                                                
099000     DISPLAY IN10-RAD2                                                    
099100                                                                          
099200*    --- LÄS INDATA RAD 3 FRÅN 2322                                       
099300     PERFORM S01-LAES-W21710                                              
099400     IF NOT END-OF-W21710                                                 
099500*                                                                         
099600*      RAD 3 LIGGER NU KVAR I IN10-AREAN                                  
099700*                                                                         
099800       IF IN10-BEART-SOEK > SPACE                                         
099900         MOVE IN10-BEART-SOEK       TO BEART-BESORD                       
100000         MOVE ZERO                  TO BEART-DIFAELT                      
100100                                                                          
100200         INSPECT BEART-BESORD TALLYING BEART-DIFAELT                      
100300                For Characters Before Initial 5-Space                     
100400       Else                                                               
100500         Move Space                 To BEART-BESORD                       
100600       End-If                                                             
100700     Else                                                                 
100800       Move 'PARAMETERRAD 3 FRÅN SOP SAKNAS '                             
100900             To FELTEXT-STR                                               
101000       Display FELTEXT                                                    
101100       Perform S99-ABEND                                                  
101200                                                                          
101300     End-If                                                               
101400                                                                          
101500     Display IN10-RAD3                                                    
101600     Display '*********** '                                               
101700                                                                          
101800     If WS10-KVVECKOR-AVROP > Zero                                        
101900       Perform AA-BERAKNA-SLUTVECKA-AVROP                                 
102000     End-If                                                               
102100     If WS10-KVVECKOR-KVPB  > Zero                                        
102200       Perform AB-BERAKNA-STARTVECKA-KVPB                                 
102300     End-If                                                               
102400     PERFORM AC-TAB-WDGX1144                                              
102500     .                                                                    
102600     EJECT                                                                
102700                                                                          
102800 AA-BERAKNA-SLUTVECKA-AVROP   SECTION.                                    
102900     MOVE 'AA-BERAKNA-SLUTVECKA-AVROP' TO CURRENT-SECTION                 
103000                                                                          
103100*                                                                         
103200*  Räkna ut antal dagar i kommande veckor, + de i denna veckan            
103300     Compute WS-ANTAL-URVALSDAGAR =                                       
103400     (( WS10-KVVECKOR-AVROP - 1 ) * 7 ) + 6 - DAGENS-TILEVDAG             
103500*                                                                         
103600                                                                          
103700*  Bestäm sedan vilken slutvecka det blir efter alla dessa dagar.         
103800     Move "YYMMDD"             To DAYS-KDDATFMT1                          
103900     Move DAGENS-TIDATUM       To DAYS-TIDATE1                            
104000                                                                          
104100     Move "YYWW"               To DAYS-KDDATFMT2                          
104200     Move Space                To DAYS-IDCALEND                           
104300                                  DAYS-TIDATE2                            
104400     Move WS-ANTAL-URVALSDAGAR To DAYS-KVDAYS                             
104500                                                                          
104600     Call WZ20DAYS Using DAYS-WZ20DAYS                                    
104700*                                                                         
104800     If DAYS-KDRC = +0                                                    
104900       Move 20                 To SLUT-SEKEL                              
105000       Move DAYS-TIDATE2(1:4)  To SLUT-AAVV                               
105100*      Move 5                  To SLUT-DAG                                
105200*                                                                         
105300     Else                                                                 
105400       DISPLAY 'FELKOD: ' DAYS-KDRC                                       
105500       String 'FEL I WZ20DAYS, ' DAGENS-TIDATUM ' + '                     
105600              WS10-KVVECKOR-AVROP  ' VECKOR (= '                          
105700              WS-ANTAL-URVALSDAGAR ' DAGAR)'                              
105800              Delimited By Size Into FELTEXT-STR                          
105900              Display FELTEXT                                             
106000              Perform S99-ABEND                                           
106100     End-If                                                               
106200                                                                          
106300     .                                                                    
106400     EJECT                                                                
106500                                                                          
106600                                                                          
106700 AB-BERAKNA-STARTVECKA-KVPB   SECTION.                                    
106800     MOVE 'AB-BERAKNA-STARTVECKA-KVPB' TO CURRENT-SECTION                 
106900                                                                          
107000     Move "YYMMDD"             To DAYS-KDDATFMT1                          
107100     Move DAGENS-TIDATUM       To DAYS-TIDATE1                            
107200     Move "YYWW"               To DAYS-KDDATFMT2                          
107300     Move 7                    To DAYS-KVDAYS                             
107400     Move Space                To DAYS-TIDATE2                            
107500     Call WZ20DAYS Using DAYS-WZ20DAYS                                    
107600     If DAYS-KDRC = Zero                                                  
107700       Move DAYS-TIDATE2(1:4)  To LINK-TIBEHOV-START                      
107800                                  LINK-TIAAVV-AKTUELL                     
107900     Else                                                                 
108000       String 'FEL I WZ20DAYS, ' DAGENS-TIDATUM ' + 7 dagar'              
108100              Delimited By Size Into FELTEXT-STR                          
108200     End-If                                                               
108300     .                                                                    
108400     EJECT                                                                
108500 AC-TAB-WDGX1144 SECTION.                                                 
108600     MOVE 'AC-TAB-WDGX1144             ' TO CURRENT-SECTION               
108700                                                                          
108800     MOVE +0                        TO TAB-IX                             
108900     PERFORM IMS-GU-WDGX1143                                              
109000                                                                          
109100     PERFORM IMS-GNP-WDGX1144                                             
109200     PERFORM UNTIL SEGMENT-SAKNAS                                         
109300        ADD +1                      TO TAB-IX                             
109400        MOVE 1144-IDFKNGRP-FOM      TO TAB-IDFKNGRP-FOM (TAB-IX)          
109500        MOVE 1144-IDFKNGRP-TOM      TO TAB-IDFKNGRP-TOM (TAB-IX)          
109600        MOVE 1144-KVAARLF           TO TAB-KVAARLF      (TAB-IX)          
109700                                                                          
109800        PERFORM IMS-GNP-WDGX1144                                          
109900                                                                          
110000     END-PERFORM                                                          
110100     Move TAB-IX                    TO MAX-TAB-IX                         
110200     .                                                                    
110300     EJECT                                                                
110400 B-PRERPARE-UT-EXCEL    SECTION.                                          
110500     MOVE 'B-PRERPARE-UT-EXCEL       ' TO CURRENT-SECTION                 
110600                                                                          
110700     IF END-OF-W21709                                                     
110800        MOVE LAENGD-MAX       TO LAENGD                                   
110900        MOVE '*******  INGA ARTIKLAR UPPFYLLDE URVALET ***'               
111000                              TO W002-DETALJ                              
111100        PERFORM S21-SKRIV-W21710-001                                      
111200        PERFORM S7-SKAPA-EXCEL-RUBRIK                                     
111300     End-If                                                               
111400     .                                                                    
111500     EJECT                                                                
111600                                                                          
111700 C-PROCESS-INPUT SECTION.                                                 
111800     MOVE 'C-PROCESS-INPUT           ' TO CURRENT-SECTION                 
111900                                                                          
112000****************************************************************          
112100*    HÄR LÄSES DAGLAGERBANDET IN                                          
112200*        OM ART UPPFYLLER URVALET SÅ SKAPAS SORTFIL                       
112300*    Här läses och skrives också sortposter för multipla                  
112400*    förkomster av KR-info, KAMPANJ-info samt Lev.besked                  
112500*    för vilka de vill ha en rad i output för varje                       
112600*    förekomst.                                                           
112700****************************************************************          
112800     PERFORM CA-KOLLA-MOT-URVALET                                         
112900     IF URVAL-OK                                                          
113000       MOVE IN09-IDARTNR    TO TEMPWS-IDARTNR                             
113100       PERFORM CC-SKAPA-SORTFIL                                           
113200*      -- NU KONTROLLERAS VAL-FÄLT SOM KAN GE MULTIPLA RADER              
113300*      -- IFALL INGET AV DE ÄR VALDA,                                     
113400*      -- SKRIVS SORT-POSTEN DIREKT.                                      
113500       MOVE ZERO  TO TEMPWS-IDKR                                          
113600                     TEMPWS-IDKAMP-GRP                                    
113700                     TEMPWS-TISTADAT-KAMP                                 
113800                     TEMPWS-TISTODAT-KAMP                                 
113900                     TEMPWS-TILEVBSK-AVS                                  
114000                     TEMPWS-KVAVIS-BSKKVAR                                
114100                     TEMPWS-TILEVBSK-INL-C1                               
114200                     TEMPWS-TILEVBSK-DISP-C1                              
114300                     TEMPWS-TIBORT-INFO                                   
114400                     TEMPWS-TITPO                                         
114500                     TEMPWS-KVART                                         
114600       MOVE SPACE TO TEMPWS-KDKRSTA                                       
114700                     TEMPWS-IDKAMP                                        
114800                     TEMPWS-KDKAMP                                        
114900       IF  WS-FLAGGA-LISTA(09) = SPACE                                    
115000       AND WS-FLAGGA-LISTA(11) = SPACE                                    
115100       AND WS-FLAGGA-LISTA(46) = SPACE                                    
115200       AND WS-FLAGGA-LISTA(47) = SPACE                                    
115300*        LISTA 09 = LEV.BESK.                                             
115400*        LISTA 11 = TPO     .                                             
115500*        LISTA 46 = KR-STATUS                                             
115600*        LISTA 47 = KAMPANJ                                               
115700         PERFORM CH-PROCESS-OUTPUT                                        
115800       ELSE                                                               
115900         IF WS-FLAGGA-LISTA(09) > SPACE                                   
116000           PERFORM C1-BEH-MULT-LEVBESK                                    
116100*          LEV.BESK (MULTIPLA RADER MÖJLIGA)                              
116200*          SORT-RELEASE GÖRS HÄR MINST EN GÅNG                            
116300         ELSE                                                             
116400           IF WS-FLAGGA-LISTA(11) > SPACE                                 
116500             PERFORM C2-BEH-MULT-TPO                                      
116600*            TPO (MULTIPLA RADER MÖJLIGA)                                 
116700*            SORT-RELEASE GÖRS HÄR MINST EN GÅNG                          
116800           ELSE                                                           
116900             IF WS-FLAGGA-LISTA(46) > SPACE                               
117000               PERFORM C3-BEH-MULT-KR-STATUS                              
117100*              KR (MULTIPLA RADER MÖJLIGA)                                
117200*              SORT-RELEASE GÖRS HÄR MINST EN GÅNG                        
117300             ELSE                                                         
117400               IF WS-FLAGGA-LISTA(47) > SPACE                             
117500                 PERFORM C4-BEH-MULT-KAMPANJ                              
117600*                KAMPANJ (MULTIPLA RADER MÖJLIGA)                         
117700*                SORT-RELEASE GÖRS HÄR MINST EN GÅNG                      
117800               END-IF                                                     
117900             End-If                                                       
118000           End-If                                                         
118100         End-If                                                           
118200       End-If                                                             
118300     End-If                                                               
118400     .                                                                    
118500     EJECT                                                                
118600 C1-BEH-MULT-LEVBESK  SECTION.                                            
118700     MOVE 'C1-BEH-MULT-LEVBESK       ' TO CURRENT-SECTION                 
118800                                                                          
118900     Move IN09-IDARTNR To W-IDARTNR                                       
119000                          W-IDARTNR-D9                                    
119100     MOVE WC-CDC-SE    To W-IDDC-D9                                       
119200     IF WS10-FL-IDLEVNR-SHIP = 'X'                                        
119300       MOVE IN09-IDLEVNR-SHIP To W-IDLEVNR                                
119400     ELSE                                                                 
119500       Move IN09-IDLEVNR      To W-IDLEVNR                                
119600     END-IF                                                               
119700                                                                          
119800     Perform IMS-GU-WDD902                                                
119900*      -- Hämta Ext-datum för något av IDLEVBSK +2 +4 +5 +6               
120000     If SEGMENT-FINNS                                                     
120100                                                                          
120200       PERFORM S2-LAES-OCH-BEH-EXTINFO                                    
120300       PERFORM S4-LAES-OCH-BEH-EXTINFO2                                   
120400       PERFORM S5-LAES-OCH-BEH-EXTINFO3                                   
120500       PERFORM S6-LAES-OCH-BEH-EXTINFO4                                   
120600                                                                          
120700       Perform IMS-GU-WDD902                                              
120800*      -- Hämta Leverans-info                                             
120900       Perform IMS-GNP-WDD924                                             
121000       If SEGMENT-FINNS                                                   
121100         Perform Until SEGMENT-SAKNAS                                     
121200           Move LEV-DALEVBSK-AVS  To WS-DALEVBSK-AVS                      
121300           Move WS-DALEVBSK-AAMMDD To DAT-I-TIDATUM                       
121400           Perform S22-DATUMKONV-TILL-AAVVD                               
121500           If DAT-KDSVAR-OK                                               
121600              Move DAT-TIAAVVD To TEMPWS-TILEVBSK-AVS                     
121700           Else                                                           
121800              Move Zero      To TEMPWS-TILEVBSK-AVS                       
121900           End-If                                                         
122000                                                                          
122100           Move LEV-KVAVIS-BSKKVAR To TEMPWS-KVAVIS-BSKKVAR               
122200                                                                          
122300           If LEV-TILEVBSK-INL > 0                                        
122400              Move LEV-TILEVBSK-INL To DAT-I-TIDATUM                      
122500              Perform S22-DATUMKONV-TILL-AAVVD                            
122600              If DAT-KDSVAR-OK                                            
122700                 Move DAT-TIAAVVD To TEMPWS-TILEVBSK-INL-C1               
122800              Else                                                        
122900                 Move 99999     To TEMPWS-TILEVBSK-INL-C1                 
123000              End-If                                                      
123100           Else                                                           
123200              Move Zero         To TEMPWS-TILEVBSK-INL-C1                 
123300           End-If                                                         
123400                                                                          
123500           If LEV-TILEVBSK-DISP > 0                                       
123600              Move LEV-TILEVBSK-DISP To DAT-I-TIDATUM                     
123700              Perform S22-DATUMKONV-TILL-AAVVD                            
123800              If DAT-KDSVAR-OK                                            
123900                 Move DAT-TIAAVVD To TEMPWS-TILEVBSK-DISP-C1              
124000              Else                                                        
124100                 Move 99999     To TEMPWS-TILEVBSK-DISP-C1                
124200              End-If                                                      
124300           Else                                                           
124400              Move Zero         To TEMPWS-TILEVBSK-DISP-C1                
124500           End-If                                                         
124600                                                                          
124700           PERFORM CH-PROCESS-OUTPUT                                      
124800                                                                          
124900           Perform IMS-GNP-WDD924                                         
125000         End-Perform                                                      
125100       Else                                                               
125200*        -- Inget Lev.besk.  Skriv övr.info utan Lev-Besk-info            
125300         PERFORM CH-PROCESS-OUTPUT                                        
125400       End-If                                                             
125500     Else                                                                 
125600*      -- Ej träff på rätt Lev. Skriv övr.info utan Lev-Besk-info         
125700       PERFORM CH-PROCESS-OUTPUT                                          
125800     End-If                                                               
125900     .                                                                    
126000     EJECT                                                                
126100 C2-BEH-MULT-TPO SECTION.                                                 
126200     MOVE 'C2-BEH-MULT-TPO           ' TO CURRENT-SECTION                 
126300                                                                          
126400     Move NEJ                   To W-TPO                                  
126500     MOVE LOW-VALUE             TO W-WDA5A1KY-MIN                         
126600     MOVE HIGH-VALUE            TO W-WDA5A1KY-MAX                         
126700     MOVE IN09-IDARTNR          TO W-IDARTNR-MIN                          
126800                                   W-IDARTNR-MAX                          
126900     Move '1'                   TO W-KDTPOTYP-MIN                         
127000     Move '6'                   TO W-KDTPOTYP-MAX                         
127100     PERFORM IMS-GN-WDA5A-TITPO                                           
127200     PERFORM UNTIL SEGMENT-SAKNAS OR                                      
127300                   SEGMENT-SLUT                                           
127400        MOVE SEQA-IDDISTR       TO  W-IDDISTR-N2                          
127500        MOVE SEQA-IDKUNDNR      TO  W-IDKUNDNR-N2                         
127600        MOVE SEQA-IDKUNDRF      TO  W-IDKUNDRF-N2                         
127700        MOVE SEQA-IDARTNR       TO  W-IDARTNR-N2                          
127800        MOVE SEQA-IDLOPNR       TO  W-IDLOPNR-N2                          
127900                                                                          
128000        PERFORM IMS-GU-WDA501                                             
128100        IF SEGMENT-FINNS                                                  
128200           IF RAD-KDSTARAD = '1'                                          
128300              IF RAD-KDTPOTYP NOT = 0                                     
128400                 MOVE RAD-TITPO  To TEMPWS-TITPO                          
128500                 MOVE RAD-KVART  To TEMPWS-KVART                          
128600                 PERFORM CH-PROCESS-OUTPUT                                
128700                 Move JA         To W-TPO                                 
128800              End-If                                                      
128900           End-If                                                         
129000        End-If                                                            
129100        PERFORM IMS-GN-WDA5A-TITPO                                        
129200     End-Perform                                                          
129300                                                                          
129400     If W-TPO = NEJ                                                       
129500        MOVE +0                 To TEMPWS-TITPO                           
129600        MOVE +0                 To TEMPWS-KVART                           
129700        PERFORM CH-PROCESS-OUTPUT                                         
129800     End-If                                                               
129900     .                                                                    
130000     EJECT                                                                
130100 C3-BEH-MULT-KR-STATUS  SECTION.                                          
130200     MOVE 'C3-BEH-MULT-KR-STATUS     ' TO CURRENT-SECTION                 
130300                                                                          
130400*    -- Kompl. med IDKR och KDKRSTA och "releasa" SORTWS                  
130500*    -- för varje IDKR enligt urval och per art. och lev.                 
130600     If WS-FLAGGA-LISTA(46) Numeric                                       
130700*      -- Beställaren vill ha en speciell KR status                       
130800       Move WS-FLAGGA-LISTA(46) To                                        
130900                 W-KDKRSTA-MIN W-KDKRSTA-MAX TEMPWS-KDKRSTA               
131000     Else                                                                 
131100*      -- Beställaren ska ha alla KR status för ART och LEV               
131200       Move '1' To W-KDKRSTA-MIN                                          
131300       Move '9' To W-KDKRSTA-MAX                                          
131400     End-If                                                               
131500*    -- Sök med Art. och Lev. som gäller för nuv. post                    
131600     Move TEMPWS-IDARTNR To W-IDARTNR-H7-MIN                              
131700                            W-IDARTNR-H7-MAX                              
131800**** Move TEMPWS-IDLEVNR To W-IDLEVNR-H7-MIN                              
131900****                        W-IDLEVNR-H7-MAX                              
132000     IF WS10-FL-IDLEVNR-SHIP = 'X'                                        
132100       MOVE TEMPWS-IDLEVNR-SHIP To W-IDLEVNR-H7-MIN                       
132200                                   W-IDLEVNR-H7-MAX                       
132300     ELSE                                                                 
132400       Move TEMPWS-IDLEVNR      To W-IDLEVNR-H7-MIN                       
132500                                   W-IDLEVNR-H7-MAX                       
132600     END-IF                                                               
132700                                                                          
132800     Perform IMS-GU-W6H701-SEQB                                           
132900                                                                          
133000     Set ARTIKEL-MED-KR-EJ-SKRIVEN To True                                
133100                                                                          
133200     Perform Until SEGMENT-SAKNAS                                         
133300*      -- KR-träff. Kolla först om Urval och DC är rätt                   
133400       If W6H7-KR-KDKRSTA  >= W-KDKRSTA-MIN                               
133500       And W6H7-KR-KDKRSTA <= W-KDKRSTA-MAX                               
133600       And W6H7-KR-IDDC = '11'                                            
133700         Move W6H7-KR-IDKR    To TEMPWS-IDKR                              
133800         Move W6H7-KR-KDKRSTA To TEMPWS-KDKRSTA                           
133900         PERFORM CH-PROCESS-OUTPUT                                        
134000         SET ARTIKEL-MED-KR-SKRIVEN TO TRUE                               
134100       End-If                                                             
134200       Perform IMS-GN-W6H701-SEQB                                         
134300     End-Perform                                                          
134400                                                                          
134500     If ARTIKEL-MED-KR-EJ-SKRIVEN                                         
134600*      -- Ingen KR-träff. Skriv ut raden utan KR                          
134700       PERFORM CH-PROCESS-OUTPUT                                          
134800     End-If                                                               
134900     .                                                                    
135000     EJECT                                                                
135100                                                                          
135200 C4-BEH-MULT-KAMPANJ  SECTION.                                            
135300     MOVE 'C4-BEH-MULT-KAMPANJ       ' TO CURRENT-SECTION                 
135400                                                                          
135500*    Lista 47 = Kampanj. Läser DB2 baserna                                
135600*                        TP1ARTK och TP1KAMP "joinat".                    
135700                                                                          
135800     Set ARTIKEL-MED-KAMP-EJ-SKRIVEN To True                              
135900                                                                          
136000     Move IN09-IDARTNR To W-IDARTNR                                       
136100                                                                          
136200     Perform DB2-OPEN-KMPDATA                                             
136300     If CURSOR-OK                                                         
136400       Perform DB2-FETCH-KMPDATA-CRS1                                     
136500       If ROW-FOUND                                                       
136600         Perform Until ROW-NOTFOUND                                       
136700           Move TP1ARTK-IDKAMP        To TEMPWS-IDKAMP                    
136800           Move TP1KAMP-IDKAMP-GRP    To TEMPWS-IDKAMP-GRP                
136900           Move TP1KAMP-TISTADAT-KAMP To TEMPWS-TISTADAT-KAMP             
137000           Move TP1KAMP-TISTODAT-KAMP To TEMPWS-TISTODAT-KAMP             
137100           Move TP1KAMP-KDKAMP        To TEMPWS-KDKAMP                    
137200           PERFORM CH-PROCESS-OUTPUT                                      
137300           Perform DB2-FETCH-KMPDATA-CRS1                                 
137400           Set ARTIKEL-MED-KAMP-SKRIVEN To True                           
137500         End-Perform                                                      
137600       End-If                                                             
137700     End-If                                                               
137800     Perform DB2-CLOSE-KMPDATA-CRS1                                       
137900                                                                          
138000     If ARTIKEL-MED-KAMP-EJ-SKRIVEN                                       
138100*      -- Ingen KAMPANJ-träff. Skriv ut raden utan KAMPANJ                
138200       PERFORM CH-PROCESS-OUTPUT                                          
138300     End-If                                                               
138400     .                                                                    
138500     EJECT                                                                
138600                                                                          
138700 CA-KOLLA-MOT-URVALET    SECTION.                                         
138800     MOVE 'CA-KOLLA-MOT-URVALET      ' TO CURRENT-SECTION                 
138900                                                                          
139000     MOVE JA  TO URVAL-SW                                                 
139100* KOLLA IDPROJ-URVAL                                                      
139200     IF URVAL-OK                                                          
139300       IF WS10-IDPROJ-URV > SPACE                                         
139400         IF  IN09-IDPROJ = WS10-IDPROJ-URV                                
139500            CONTINUE                                                      
139600         ELSE                                                             
139700            MOVE NEJ TO URVAL-SW                                          
139800         END-IF                                                           
139900       END-IF                                                             
140000     END-IF                                                               
140100                                                                          
140200* KOLLA PRODUKTSLAGSURVAL                                                 
140300     IF URVAL-OK                                                          
140400       IF WS10-KDPRODSL-FOM > ZERO                                        
140500         IF ((IN09-KDPRODSL NOT < WS10-KDPRODSL-FOM )                     
140600         AND (IN09-KDPRODSL NOT > WS10-KDPRODSL-TOM ))                    
140700            CONTINUE                                                      
140800         ELSE                                                             
140900            MOVE NEJ TO URVAL-SW                                          
141000         END-IF                                                           
141100       END-IF                                                             
141200     END-IF                                                               
141300                                                                          
141400* KOLLA URVAL ANSKAFFARID/BEREDARE                                        
141500     IF WS10-FL-IDBERED = 'X'                                             
141600        MOVE IN09-IDBERED    TO WS-PART-IDRESP                            
141700     ELSE                                                                 
141800        MOVE IN09-IDANSK     TO WS-PART-IDRESP                            
141900     END-IF                                                               
142000     IF WS10-IDANSK-FOM > ZERO                                            
142100        IF WS-PART-IDRESP NOT < WS10-IDANSK-FOM AND                       
142200           WS-PART-IDRESP NOT > WS10-IDANSK-TOM                           
142300           CONTINUE                                                       
142400        ELSE                                                              
142500           MOVE NEJ TO URVAL-SW                                           
142600        END-IF                                                            
142700     ELSE                                                                 
142800       IF WS10-IDANSK (1) > ZERO OR WS10-IDANSK (2) > ZERO OR             
142900          WS10-IDANSK (3) > ZERO OR WS10-IDANSK (4) > ZERO OR             
143000          WS10-IDANSK (5) > ZERO                                          
143100           MOVE +1 TO IX                                                  
143200           MOVE NEJ TO TRAEFF-SW                                          
143300           PERFORM UNTIL IX > 5 OR TRAEFF-OK                              
143400              IF WS-PART-IDRESP = WS10-IDANSK (IX) AND                    
143500                 WS10-IDANSK (IX) > ZERO                                  
143600                 MOVE JA TO TRAEFF-SW                                     
143700              END-IF                                                      
143800              ADD +1 TO IX                                                
143900           END-PERFORM                                                    
144000           IF TRAEFF-NEJ                                                  
144100              MOVE NEJ TO URVAL-SW                                        
144200           END-IF                                                         
144300       END-IF                                                             
144400     END-IF                                                               
144500                                                                          
144600* KOLLA URVAL LEVERANTÖRSID                                               
144700     IF URVAL-OK                                                          
144800       IF WS10-IDLEVNR (1) > SPACE OR WS10-IDLEVNR (2) > SPACE            
144900       OR WS10-IDLEVNR (3) > SPACE OR WS10-IDLEVNR (4) > SPACE            
145000       OR WS10-IDLEVNR (5) > SPACE OR WS10-IDLEVNR (6) > SPACE            
145100       OR WS10-IDLEVNR (7) > SPACE OR WS10-IDLEVNR (8) > SPACE            
145200       OR WS10-IDLEVNR (9) > SPACE                                        
145300           MOVE +1 TO IX                                                  
145400           MOVE NEJ TO TRAEFF-SW                                          
145500           PERFORM UNTIL IX > 9  OR TRAEFF-OK                             
145600             IF WS10-FL-IDLEVNR-SHIP = 'X'                                
145700               IF IN09-IDLEVNR-SHIP = WS10-IDLEVNR (IX) AND               
145800                  WS10-IDLEVNR (IX) > SPACE                               
145900                  MOVE JA TO TRAEFF-SW                                    
146000               END-IF                                                     
146100             ELSE                                                         
146200               IF IN09-IDLEVNR = WS10-IDLEVNR (IX) AND                    
146300                  WS10-IDLEVNR (IX) > SPACE                               
146400                  MOVE JA TO TRAEFF-SW                                    
146500               END-IF                                                     
146600             END-IF                                                       
146700             ADD +1 TO IX                                                 
146800           END-PERFORM                                                    
146900           IF TRAEFF-NEJ                                                  
147000              MOVE NEJ TO URVAL-SW                                        
147100           END-IF                                                         
147200       END-IF                                                             
147300     END-IF                                                               
147400                                                                          
147500* KOLLA ERSÄTTNINGSKOD URVAL                                              
147600     IF URVAL-OK                                                          
147700       IF IN10-KDERS-FOM < 99                                             
147800          IF IN09-KDERS NOT < IN10-KDERS-FOM AND                          
147900             IN09-KDERS NOT > IN10-KDERS-TOM                              
148000             CONTINUE                                                     
148100          ELSE                                                            
148200             MOVE NEJ TO URVAL-SW                                         
148300          END-IF                                                          
148400       ELSE                                                               
148500         IF IN10-KDERS (1) < 99 OR IN10-KDERS (2) < 99 OR                 
148600            IN10-KDERS (3) < 99 OR IN10-KDERS (4) < 99 OR                 
148700            IN10-KDERS (5) < 99                                           
148800             MOVE +1 TO IX                                                
148900             MOVE NEJ TO TRAEFF-SW                                        
149000             PERFORM UNTIL IX > 5 OR TRAEFF-OK                            
149100                IF IN09-KDERS = IN10-KDERS (IX)                           
149200                   MOVE JA TO TRAEFF-SW                                   
149300                END-IF                                                    
149400                ADD +1 TO IX                                              
149500             END-PERFORM                                                  
149600             IF TRAEFF-NEJ                                                
149700                MOVE NEJ TO URVAL-SW                                      
149800             END-IF                                                       
149900         END-IF                                                           
150000       END-IF                                                             
150100     END-IF                                                               
150200                                                                          
150300* KOLLA KDOTFREK-URVAL                                                    
150400     IF URVAL-OK                                                          
150500       IF IN10-KDOTFREK > SPACE                                           
150600         IF  IN09-KDOTFREK = IN10-KDOTFREK                                
150700            CONTINUE                                                      
150800         ELSE                                                             
150900            MOVE NEJ TO URVAL-SW                                          
151000         END-IF                                                           
151100       END-IF                                                             
151200     END-IF                                                               
151300                                                                          
151400* KOLLA FUNKTIONSGRUPPSID URVAL                                           
151500     IF URVAL-OK                                                          
151600       IF IN10-IDFKNGRP-FOM > ZERO                                        
151700          IF IN09-IDFKNGRP NOT < IN10-IDFKNGRP-FOM AND                    
151800             IN09-IDFKNGRP NOT > IN10-IDFKNGRP-TOM                        
151900             CONTINUE                                                     
152000          ELSE                                                            
152100             MOVE NEJ TO URVAL-SW                                         
152200          END-IF                                                          
152300       ELSE                                                               
152400         IF IN10-IDFKNGRP(1) > ZERO OR IN10-IDFKNGRP(2) > ZERO OR         
152500            IN10-IDFKNGRP(3) > ZERO OR IN10-IDFKNGRP(4) > ZERO            
152600             MOVE +1 TO IX                                                
152700             MOVE NEJ TO TRAEFF-SW                                        
152800                                                                          
152900             PERFORM UNTIL IX > 4 OR TRAEFF-OK                            
153000                IF IN09-IDFKNGRP = IN10-IDFKNGRP(IX) AND                  
153100                   IN10-IDFKNGRP(IX) > ZERO                               
153200                   MOVE JA TO TRAEFF-SW                                   
153300                END-IF                                                    
153400                ADD +1 TO IX                                              
153500             END-PERFORM                                                  
153600                                                                          
153700             IF TRAEFF-NEJ                                                
153800                MOVE NEJ TO URVAL-SW                                      
153900             END-IF                                                       
154000         END-IF                                                           
154100       END-IF                                                             
154200     END-IF                                                               
154300                                                                          
154400* KOLLA FÖRPACKNINGSTYP URVAL                                             
154500     IF URVAL-OK                                                          
154600       IF IN10-BEFT (1) > ZERO  OR IN10-BEFT (2) > ZERO                   
154700       OR IN10-BEFT (3) > ZERO  OR IN10-BEFT (4) > ZERO                   
154800           MOVE +1 TO IX                                                  
154900           MOVE NEJ TO TRAEFF-SW                                          
155000           PERFORM UNTIL IX > 4  OR TRAEFF-OK                             
155100              IF IN09-BEFT = IN10-BEFT (IX) AND                           
155200                 IN10-BEFT (IX) > ZERO                                    
155300                 MOVE JA TO TRAEFF-SW                                     
155400              END-IF                                                      
155500              ADD +1 TO IX                                                
155600           END-PERFORM                                                    
155700           IF TRAEFF-NEJ                                                  
155800              MOVE NEJ TO URVAL-SW                                        
155900           END-IF                                                         
156000       END-IF                                                             
156100     END-IF                                                               
156200                                                                          
156300* KOLLA SÖKORD, BENÄMNING                                                 
156400     IF URVAL-OK                                                          
156500*      -- IN10-BEART-SOEK ÄR BEHANDLAD I A-INIT                           
156600       IF BEART-BESORD > SPACE                                            
156700         MOVE ZERO TO IX                                                  
156800         INSPECT IN09-BEART-SVE TALLYING IX                               
156900         FOR ALL BEART-BESORD(1:BEART-DIFAELT)                            
157000                                                                          
157100         IF IX = ZERO                                                     
157200           MOVE NEJ TO URVAL-SW                                           
157300         END-IF                                                           
157400       END-IF                                                             
157500     END-IF                                                               
157600                                                                          
157700* KOLLA URVAL ADLAGOMR + ADGANG                                           
157800     IF URVAL-OK                                                          
157900        IF IN10-ADLAGOMR > ZERO                                           
158000                                                                          
158100           IF ((IN10-ADLAGOMR        = IN09-ADLAGOMR-SVS)  AND            
158200              ((IN09-ADGANG-SVS  NOT < IN10-ADGANG-FOM)    AND            
158300               (IN09-ADGANG-SVS  NOT > IN10-ADGANG-TOM)))                 
158400           OR                                                             
158500              ((IN10-ADLAGOMR        = IN09-ADLAGOMR)      AND            
158600              ((IN09-ADGANG      NOT < IN10-ADGANG-FOM)    AND            
158700               (IN09-ADGANG      NOT > IN10-ADGANG-TOM)))                 
158800                CONTINUE                                                  
158900           ELSE                                                           
159000                MOVE NEJ TO URVAL-SW                                      
159100           END-IF                                                         
159200        END-IF                                                            
159300     End-if                                                               
159400     .                                                                    
159500     EJECT                                                                
159600                                                                          
159700 CC-SKAPA-SORTFIL SECTION.                                                
159800     MOVE 'CC-SKAPA-SORTFIL          ' TO CURRENT-SECTION                 
159900                                                                          
160000     Move IN09-ADGANG        To TEMPWS-ADGANG                             
160100     Move IN09-ADINPORT      To TEMPWS-ADINPORT                           
160200     Move IN09-ADLAGOMR      To TEMPWS-ADLAGOMR                           
160300     Move IN09-ADPLATS       To TEMPWS-ADPLATS                            
160400     Move IN09-BEART-ENG     To TEMPWS-BEART-ENG                          
160500     Move IN09-BEART-SVE     To TEMPWS-BEART-SVE                          
160600     Move IN09-BEFT          To TEMPWS-BEFT                               
160700     Move IN09-DAPBPLAN      To TEMPWS-DAPBPLAN                           
160800     Move IN09-FLIART        To TEMPWS-FLIART                             
160900     Move IN09-FLJIT         To TEMPWS-FLJIT                              
161000     Move IN09-FLMANQ        To TEMPWS-FLMANQ                             
161100     Move IN09-FLLSRDEL      To TEMPWS-FLLSRDEL                           
161200     Move IN09-IDANSK        To TEMPWS-IDANSK                             
161300     Move IN09-IDBERED       To TEMPWS-IDBERED                            
161400     Move IN09-IDARTNR       To TEMPWS-IDARTNR                            
161500     Move IN09-IDARTNR-EMBQ0 To TEMPWS-IDARTNR-EMBQ0                      
161600     Move IN09-IDARTNR-EMBQ1 To TEMPWS-IDARTNR-EMBQ1                      
161700     Move IN09-IDARTNR-EMBQ2 To TEMPWS-IDARTNR-EMBQ2                      
161800     Move IN09-IDARTNR-EMBQ3 To TEMPWS-IDARTNR-EMBQ3                      
161900     Move IN09-IDARTNR-EMBQ4 To TEMPWS-IDARTNR-EMBQ4                      
162000     Move IN09-IDFKNGRP      To TEMPWS-IDFKNGRP                           
162100     Move 1 To IX                                                         
162200     Perform Until IX > 3                                                 
162300        Move IN09-IDKAT(IX)  To TEMPWS-IDKAT(IX)                          
162400        Add 1 To IX                                                       
162500     End-Perform                                                          
162600     Move IN09-IDINK         To TEMPWS-IDINK                              
162700     Move IN09-IDLEVNR       To TEMPWS-IDLEVNR                            
162800     Move IN09-IDLEVNR-SHIP  To TEMPWS-IDLEVNR-SHIP                       
162900     Move IN09-IDPROJ        To TEMPWS-IDPROJ                             
163000     Move IN09-KDAVT         To TEMPWS-KDAVT                              
163100     Move IN09-KDERS         To TEMPWS-KDERS                              
163200     Move IN09-KDFORP        To WS-KDFORP                                 
163300     Move WS-KDFORP          To TEMPWS-KDFORP                             
163400     Move IN09-KDFREKKL      To TEMPWS-KDFREKKL                           
163500     Move IN09-KDLEVPLF      To TEMPWS-KDLEVPLF                           
163600     Move IN09-KDOTFREK      To TEMPWS-KDOTFREK                           
163700     Move IN09-KDPRISKL      To TEMPWS-KDPRISKL                           
163800     Move IN09-KDPRODSL      To TEMPWS-KDPRODSL                           
163900     Move IN09-KDSORT        To TEMPWS-KDSORT                             
164000     Move IN09-KDUART        To TEMPWS-KDUART                             
164100     Move IN09-KDVVKL        To TEMPWS-KDVVKL                             
164200*    Move IN09-KVBR-TOT      To TEMPWS-KVBR-TOT                           
164300     Compute TEMPWS-KVAKS = IN09-KVAKS-CDC + IN09-KVAKS-PAV               
164400     Move IN09-KVPB-SEP      To TEMPWS-KVPB-SEP                           
164500     Compute TEMPWS-KVPB-TOT = (IN09-KVPB-SEP +                           
164600              IN09-KVPB-SATS + IN09-KVPB-REF + IN09-KVPB-TPO)             
164700     Move IN09-KVAVBRAD      To TEMPWS-KVAVBRAD                           
164800     Move IN09-KVINORD       To TEMPWS-KVINORD                            
164900     If TEMPWS-KVINORD  > 0                                               
165000       Compute TEMPWS-SERVG Rounded =                                     
165100                       TEMPWS-KVAVBRAD / TEMPWS-KVINORD                   
165200     Else                                                                 
165300       Move Zero To TEMPWS-SERVG                                          
165400     End-If                                                               
165500     Move IN09-KVLS-CDC      To TEMPWS-KVLS-CDC                           
165600     Move IN09-KVLS-NDC      To TEMPWS-KVLS-NDC                           
165700     Move IN09-KVLS-SDC-LDC  To TEMPWS-KVLS-SDC-LDC                       
165800     Move IN09-KVMAD-SEP     To TEMPWS-KVMAD-SEP                          
165900     Move IN09-KVMP          To TEMPWS-KVMP                               
166000     Move IN09-KVOI-YEAR-0   To TEMPWS-KVOI-YEAR-0                        
166100     Move IN09-KVOI-YEAR-1   To TEMPWS-KVOI-YEAR-1                        
166200     Move IN09-KVOI-YEAR-2   To TEMPWS-KVOI-YEAR-2                        
166300     Move IN09-KVOI-YEAR-3   To TEMPWS-KVOI-YEAR-3                        
166400     Move IN09-KVOI-YEAR-4   To TEMPWS-KVOI-YEAR-4                        
166500     Move IN09-KVOI-YEAR-5   To TEMPWS-KVOI-YEAR-5                        
166600     Move IN09-KVOI-12-RULL  To TEMPWS-KVOI-12-RULL                       
166700     Move IN09-KVPALL        To TEMPWS-KVPALL                             
166800     Move IN09-KVPB-PLAN     To TEMPWS-KVPB-PLAN                          
166900     Move IN09-KVPB-SATS     To TEMPWS-KVPB-SATS                          
167000     Move IN09-KVQ           To TEMPWS-KVQ                                
167100     Move IN09-KVQPACK-0     To TEMPWS-KVQPACK-0                          
167200     Move IN09-KVQPACK-1     To TEMPWS-KVQPACK-1                          
167300     Move IN09-KVQPACK-2     To TEMPWS-KVQPACK-2                          
167400     Move IN09-KVQPACK-3     To TEMPWS-KVQPACK-3                          
167500     Move IN09-KVQPACK-4     To TEMPWS-KVQPACK-4                          
167600     Move IN09-KVRESS-CDC     To TEMPWS-KVRESS-CDC                        
167700     Move IN09-KVRESS-NDC     To TEMPWS-KVRESS-NDC                        
167800     Move IN09-KVRESS-SDC-LDC To TEMPWS-KVRESS-SDC-LDC                    
167900     Move IN09-KVROS         To TEMPWS-KVROS                              
168000     Move IN09-KVSLAGER      To TEMPWS-KVSLAGER                           
168100     Move IN09-KVSPANT       To TEMPWS-KVSPANT                            
168200     Move IN09-KVSPARR-KVAL-CDC To TEMPWS-KVSPARR-KVAL-CDC                
168300     Move IN09-KVVECKOR-LT   To TEMPWS-KVVECKOR-LT                        
168400     Move IN09-PRARTBES      To TEMPWS-PRARTBES                           
168500     Move IN09-PRARTSTD      To TEMPWS-PRARTSTD                           
168600     Move IN09-PRDIRLON      To TEMPWS-PRDIRLON                           
168700     Move IN09-PRDMTRL       To TEMPWS-PRDMTRL                            
168800     Move IN09-REDIRLEV      To TEMPWS-REDIRLEV                           
168900     Move IN09-RESLJUST      To TEMPWS-RESLJUST                           
169000*    Move IN09-TIDISPIN      To TEMPWS-TIDISPIN                           
169100     Move IN09-TIFINLV       To TEMPWS-TIFINLV                            
169200     Move IN09-TISLJUST      To TEMPWS-TISLJUST                           
169300     Move IN09-TIURPROD      To TEMPWS-TIURPROD                           
169400     Move IN09-TIREFSTO      TO TEMPWS-TIREFSTO                           
169500     Move IN09-VKART         TO TEMPWS-VKART                              
169600     Move IN09-VLARTNTO      TO TEMPWS-VLARTNTO                           
169700     Move IN09-KVEOQ         TO TEMPWS-KVEOQ                              
169800     Move IN09-KVULOAD       TO TEMPWS-KVULOAD                            
169900     Move IN09-FLNYBER       TO TEMPWS-FLNYBER                            
170000     Move IN09-KVVORKO       TO TEMPWS-KVVORKO                            
170100     Move IN09-KDARTURS      TO TEMPWS-KDARTURS                           
170200     Move IN09-KVPB-TREND    TO TEMPWS-KVPB-TREND                         
170300     Move IN09-KVVECKOR-TREND TO TEMPWS-KVVECKOR-TREND                    
170400     Move IN09-TIDATUM-TREND TO TEMPWS-TIDATUM-TREND                      
170500     Move +0                 TO IX1-TILEVDAG                              
170600     PERFORM UNTIL IX1-TILEVDAG = +5                                      
170700       ADD +1                TO IX1-TILEVDAG                              
170800       Move IN09-TILEVDAG(IX1-TILEVDAG) TO TEMPWS-TILEVDAG                
170900                                             (IX1-TILEVDAG)               
171000     END-PERFORM                                                          
171100                                                                          
171200     If WS-FLAGGA-LISTA(02) > Space                                       
171300        Perform CCB-HAMTA-BELEV                                           
171400     End-If                                                               
171500                                                                          
171600     If WS-FLAGGA-LISTA(07) > Space                                       
171700*       -- VOR/RO                                                         
171800        Perform CCK-HAMTA-WDA5A-RORADER                                   
171900     End-If                                                               
172000                                                                          
172100     If WS-FLAGGA-LISTA(08) > Space                                       
172200        Perform CCA-HAMTA-SLAP-O-NOT-REC                                  
172300     End-If                                                               
172400                                                                          
172500     If WS-FLAGGA-LISTA(09) > Space                                       
172600*      Lev.Besk. data hämtas i C- före TEMPWS-RELEASE                     
172700        Continue                                                          
172800     End-If                                                               
172900                                                                          
173000     If WS-FLAGGA-LISTA(10) > Space                                       
173100        Perform CCF-HAMTA-FORAVIS-FRAN-W6D1                               
173200     End-If                                                               
173300                                                                          
173400     If WS-FLAGGA-LISTA(19) > Space                                       
173500        Perform CCC-HAMTA-SEN-INLEV                                       
173600     End-If                                                               
173700                                                                          
173800     If WS-FLAGGA-LISTA(26) > Space                                       
173900        Perform CCH-HAMTA-KVBR-VALID-OCH-OVR                              
174000     End-If                                                               
174100                                                                          
174200     If WS-FLAGGA-LISTA(18) > Space                                       
174300*      SÄSONG-fälten hämtas både från WDK626 och W222SEAS                 
174400*      nya indexet för SÄSONG = 18                                        
174500       Perform CCI-HAMTA-SEASONG                                          
174600     End-If                                                               
174700                                                                          
174800     If WS10-KVVECKOR-AVROP > Zeroes                                      
174900        Perform CCD-HAMTA-AVROP-2-WDD905                                  
175000     End-If                                                               
175100                                                                          
175200     If WS10-KVVECKOR-KVPB  > Zeroes                                      
175300        Perform CCE-HAMTA-KVPB-SUM-VV                                     
175400     End-If                                                               
175500                                                                          
175600     If WS-FLAGGA-LISTA(46) > Space                                       
175700*      KR-status och KR-id hämtas i C- före TEMPWS-RELEASE                
175800*      En release görs för varje IDKR man hittar för artikeln.            
175900       Continue                                                           
176000     End-If                                                               
176100     .                                                                    
176200     EJECT                                                                
176300                                                                          
176400 CCA-HAMTA-SLAP-O-NOT-REC SECTION.                                        
176500     MOVE 'CCA-HAMTA-SLAP-O-NOT-REC  ' TO CURRENT-SECTION                 
176600                                                                          
176700     Move Zero  To TEMPWS-KVSLAP-SUM                                      
176800     Move Zero  To TEMPWS-KVAVIS-NOT-REC                                  
176900                                                                          
177000     Move IN09-IDARTNR To W-IDARTNR                                       
177100                          W-IDARTNR-D9                                    
177200     Move WC-CDC-SE    To W-IDDC-D9                                       
177300*                                                                         
177400     IF WS10-FL-IDLEVNR-SHIP = 'X'                                        
177500       MOVE IN09-IDLEVNR-SHIP To W-IDLEVNR                                
177600     ELSE                                                                 
177700       Move IN09-IDLEVNR      To W-IDLEVNR                                
177800     END-IF                                                               
177900*                                                                         
178000     Perform IMS-GU-WDD902                                                
178100     If SEGMENT-FINNS                                                     
178200       Perform IMS-GNP-WDD905                                             
178300       Perform Until SEGMENT-SAKNAS                                       
178400         If TIAVRDAT-INL < DAGENS-TIDATUM                                 
178500*          --- Släp                                                       
178600           Compute TEMPWS-KVSLAP-SUM                                      
178700                 = TEMPWS-KVSLAP-SUM + KVAVROP                            
178800         Else                                                             
178900           If TIAVRDAT-INL >= DAGENS-TIDATUM                              
179000*            --- Not Received                                             
179100             Move DAAVROP-AVS To AVROP-DAAVROP-AVS                        
179200             Move TILEVDAG    To AVROP-TILEVDAG                           
179300                                                                          
179400             If WS-AVROP-DAAVROP-AVS < DAGENS-AAAAVVD                     
179500             And TIAVRDAT-INL >= DAGENS-TIDATUM                           
179600                 Compute TEMPWS-KVAVIS-NOT-REC                            
179700                       = TEMPWS-KVAVIS-NOT-REC + KVAVROP                  
179800             End-If                                                       
179900           End-If                                                         
180000         End-If                                                           
180100         Perform IMS-GNP-WDD905                                           
180200       End-Perform                                                        
180300     End-If                                                               
180400     .                                                                    
180500     EJECT                                                                
180600 CCB-HAMTA-BELEV SECTION.                                                 
180700     MOVE 'CCB-HAMTA-BELEV           ' TO CURRENT-SECTION                 
180800                                                                          
180900     Move Space   To TEMPWS-BELEV                                         
181000                                                                          
181100     Move IN09-IDARTNR To W-IDARTNR                                       
181200*                                                                         
181300     IF WS10-FL-IDLEVNR-SHIP = 'X'                                        
181400       MOVE IN09-IDLEVNR-SHIP To W-IDLEVNR W-IDLEVNR-MIN                  
181500                                           W-IDLEVNR-MAX                  
181600     ELSE                                                                 
181700       Move IN09-IDLEVNR      To W-IDLEVNR W-IDLEVNR-MIN                  
181800                                           W-IDLEVNR-MAX                  
181900     END-IF                                                               
182000*                                                                         
182100     PERFORM IMS-GU-WDF501                                                
182200     If SEGMENT-FINNS                                                     
182300       PERFORM IMS-GNP-WDF502                                             
182400       Perform Until SEGMENT-SAKNAS                                       
182500         Move XLEV-BELEVART To TEMPWS-BELEV                               
182600         PERFORM IMS-GNP-WDF502                                           
182700       End-Perform                                                        
182800     End-If                                                               
182900     .                                                                    
183000     EJECT                                                                
183100 CCC-HAMTA-SEN-INLEV   SECTION.                                           
183200     MOVE 'CCC-HAMTA-SEN-INLEV       ' TO CURRENT-SECTION                 
183300                                                                          
183400     Initialize                 TEMPWS-SENASTEINLEV (1)                   
183500     Initialize                 TEMPWS-SENASTEINLEV (2)                   
183600     Initialize                 TEMPWS-SENASTEINLEV (3)                   
183700     Initialize                 TEMPWS-SENASTEINLEV (4)                   
183800     Initialize                 TEMPWS-SENASTEINLEV (5)                   
183900                                                                          
184000     Move TEMPWS-IDARTNR   To W-IDARTNR                                   
184100**** Move TEMPWS-IDLEVNR   To W-IDLEVNR                                   
184200     IF WS10-FL-IDLEVNR-SHIP = 'X'                                        
184300       MOVE TEMPWS-IDLEVNR-SHIP To W-IDLEVNR                              
184400     ELSE                                                                 
184500       Move TEMPWS-IDLEVNR      To W-IDLEVNR                              
184600     END-IF                                                               
184700     Perform IMS-GU-WDL201                                                
184800     If SEGMENT-FINNS                                                     
184900       Move +1 To IX                                                      
185000       Perform Until IX > +5                                              
185100         Perform IMS-GNP-WDL221                                           
185200         If SEGMENT-FINNS                                                 
185300           If MOT-KDRT = 0                                                
185400             If MOT-IDPTYP = 'R31' Or 'R32'                               
185500               If MOT-TIAVIDAT Not Numeric                                
185600               or MOT-IDAVINR  Not Numeric                                
185700                 Move 'TIAVIDAT eller IDAIVINR är onum på basen'          
185800                            To FELTEXT-STR                                
185900                 Display FELTEXT                                          
186000                 display 'MOT-TIAVIDAT=' MOT-TIAVIDAT                     
186100                 display 'MOT-IDAVINR =' MOT-IDAVINR                      
186200                 Perform S99-ABEND                                        
186300               End-if                                                     
186400               Move MOT-TIAVIDAT To TEMPWS-TIAVIDAT-SEN (IX)              
186500               Move MOT-IDAVINR  To TEMPWS-IDFS-SEN  (IX)                 
186600               If MOT-IDPTYP = 'R31'                                      
186700                 Move MOT-KVAVIS   To TEMPWS-KVANTAL-SEN (IX)             
186800               Else                                                       
186900                 Move MOT-KVANTMOT To TEMPWS-KVANTAL-SEN (IX)             
187000               End-If                                                     
187100               Add +1 To IX                                               
187200             End-If                                                       
187300           End-If                                                         
187400         Else                                                             
187500           Add +5 To IX                                                   
187600         End-If                                                           
187700       End-Perform                                                        
187800     End-If                                                               
187900     .                                                                    
188000     EJECT                                                                
188100 CCD-HAMTA-AVROP-2-WDD905  SECTION.                                       
188200     MOVE 'CCD-HAMTA-AVROP-2-WDD905  ' TO CURRENT-SECTION                 
188300                                                                          
188400*  Gällande avrop hämtas för det antal veckor som är beställt.            
188500*  Första veckan är innevarande, med början idag.                         
188600*  Slutvecka och  dagar i kommande veckor + de i denna veckan             
188700*  är beräknat i AA- section.                                             
188800*  Addera alla KVAVROP mot denna leverantör på aktuell artikel            
188900                                                                          
189000     Move Zero To TEMPWS-KVAVROP-SUM-VV                                   
189100                                                                          
189200     Move TEMPWS-IDARTNR   To W-IDARTNR                                   
189300                              W-IDARTNR-D9                                
189400     Move WC-CDC-SE        To W-IDDC-D9                                   
189500**** Move TEMPWS-IDLEVNR   To W-IDLEVNR                                   
189600     IF WS10-FL-IDLEVNR-SHIP = 'X'                                        
189700       MOVE TEMPWS-IDLEVNR-SHIP To W-IDLEVNR                              
189800     ELSE                                                                 
189900       Move TEMPWS-IDLEVNR      To W-IDLEVNR                              
190000     END-IF                                                               
190100                                                                          
190200     Perform IMS-GU-WDD902                                                
190300                                                                          
190400     If SEGMENT-FINNS                                                     
190500       Perform IMS-GNP-WDD905                                             
190600       Perform Until SEGMENT-SAKNAS                                       
190700                  Or  DAAVROP-AVS > DAAVROP-SLUTVECKA                     
190800                                                                          
190900         If ( DAAVROP-AVS   = DAGENS-DAAVROP                              
191000              And TILEVDAG >= DAGENS-TILEVDAG )                           
191100         Or   DAAVROP-AVS  >  DAGENS-DAAVROP                              
191200                                                                          
191300              Add KVAVROP To TEMPWS-KVAVROP-SUM-VV                        
191400                                                                          
191500         End-If                                                           
191600         Perform IMS-GNP-WDD905                                           
191700       End-Perform                                                        
191800     End-If                                                               
191900     .                                                                    
192000     EJECT                                                                
192100                                                                          
192200 CCE-HAMTA-KVPB-SUM-VV    SECTION.                                        
192300     MOVE 'CCE-HAMTA-KVPB-SUM-VV     ' TO CURRENT-SECTION                 
192400                                                                          
192500     Move Zero To TEMPWS-KVPB-SUM-VV                                      
192600                                                                          
192700*    Addera behovet i den innevarande veckan                              
192800     Perform CCEA-SEP-BEHOV-INNEV-VECKA                                   
192900                                                                          
193000     If WS10-KVVECKOR-KVPB > 1                                            
193100*      Summera sedan behovet i de resterande veckorna                     
193200*      LINK-TIBEHOV-START-VECKA (Veckan efter denna) är beräknad i        
193300*      AB- section.                                                       
193400       Move TEMPWS-IDARTNR To LINK-IDARTNR                                
193500       Move SPACE          To LINK-IDDC                                   
193600                                                                          
193700       Perform CCEC-SEP-BEHOV-OVRIGA-VECKOR                               
193800       Perform CCED-SATS-BEHOV                                            
193900       Perform CCEE-TPO-BEHOV                                             
194000       Perform CCEF-SDC-BEHOV                                             
194100       Perform CCEG-NDC-BEHOV                                             
194200     End-If                                                               
194300     .                                                                    
194400     EJECT                                                                
194500                                                                          
194600 CCEA-SEP-BEHOV-INNEV-VECKA  SECTION.                                     
194700     SKIP2                                                                
194800     ACCEPT W-TIME             From TIME                                  
194900     Compute W-VECKO-SEP-BEHOV Rounded  =  IN09-KVPB-SEP / 4.33           
195000     Compute W-DAG-SEP-BEHOV   Rounded  =  W-VECKO-SEP-BEHOV / 5          
195100     Divide IN09-TIFINLV By 10 Giving W-TIFINLV-AAVV                      
195200     Move DAGENS-TIAAVV        To TMP1-YYWW                               
195300     Move W-TIFINLV-AAVV       To TMP2-YYWW                               
195400     Perform WY2000P3                                                     
195500                                                                          
195600     If  DAGENS-DAGNR = 6 Or                                              
195700         DAGENS-DAGNR = 7 Or                                              
195800        (DAGENS-DAGNR = 5 And W-TIME(1:4) > 1700)                         
195900     Or  TMP1-YYWW < TMP2-YYWW                                            
196000*        --- NOLL i bidrag till KVPB-SUM-VV                               
196100         Continue                                                         
196200     Else                                                                 
196300         Compute W-KVDAGAR-KVAR =  5 - DAGENS-DAGNR                       
196400         If W-TIME(1:4) <= 1700                                           
196500            Add +1 To W-KVDAGAR-KVAR                                      
196600         End-If                                                           
196700         Move +1 To W-FAKTOR                                              
196800         Subtract IN09-REDIRLEV From W-FAKTOR                             
196900         Compute W-KVPB-SEP-SUM = W-DAG-SEP-BEHOV                         
197000                                * W-KVDAGAR-KVAR                          
197100                                * W-FAKTOR                                
197200         Add W-KVPB-SEP-SUM To TEMPWS-KVPB-SUM-VV                         
197300     End-If                                                               
197400     .                                                                    
197500     EJECT                                                                
197600                                                                          
197700                                                                          
197800 CCEC-SEP-BEHOV-OVRIGA-VECKOR SECTION.                                    
197900     MOVE 'CCEC-SEP-BEHOV-OVRIGA-VECKOR' TO CURRENT-SECTION               
198000                                                                          
198100     Compute LINK-KVVECKOR-BEHOV = WS10-KVVECKOR-KVPB - 1                 
198200     Set  LINK-ENDAST-SEPARATBEHOV To True                                
198300     Move NEJ To LINK-FLINKLDIRLEV                                        
198400                                                                          
198500     Call W22222 Using LINK-AREA W222-WDK6-PCB  W222-WDK7-PCB             
198600                                 W222-ARTM-PCB  W222-2501-PCB             
198700                                 W222-WDB6R-PCB W222-WDK7R-PCB            
198800                                 W222-WDB6-PCB  W222-WDD7-PCB             
198900                                 W222-WDK7E-PCB                           
199000                                 W222-UTIL-WDK6-PCB                       
199100                                 W222-UTIL-WDK7-PCB                       
199200                                 W222-UTIL-WDB6-PCB                       
199300                                 W222-UTUP-WDK7-PCB                       
199400                                 W222-UTUP-WDB6-PCB                       
199500                                 W222-UTUP-UTIL-WDK6-PCB                  
199600                                 W222-UTUP-UTIL-WDK7-PCB                  
199700                                 W222-UTUP-UTIL-WDB6-PCB                  
199800                                                                          
199900     If LINK-ANROP-OK                                                     
200000       Move +1 To IX                                                      
200100       Perform Until IX > LINK-KVVECKOR-BEHOV                             
200200          Add LINK-KVBEHOV-VECKA(IX) To TEMPWS-KVPB-SUM-VV                
200300          Add +1 To IX                                                    
200400       End-Perform                                                        
200500     Else                                                                 
200600       String 'FEL i anrop FRÅN CCEC-SEP-BEHOV-OVRIGA-VECKOR'             
200700              ' till W22222(behovsmodulen)'                               
200800              Delimited By Size Into FELTEXT-STR                          
200900       Display FELTEXT-STR ' ' TEMPWS-IDARTNR                             
201000     End-If                                                               
201100     .                                                                    
201200     EJECT                                                                
201300                                                                          
201400 CCED-SATS-BEHOV  SECTION.                                                
201500     MOVE 'CCED-SATS-BEHOV             ' TO CURRENT-SECTION               
201600                                                                          
201700     If IN09-FLIART = JA                                                  
201800       Compute LINK-KVVECKOR-BEHOV = WS10-KVVECKOR-KVPB - 1               
201900                                                                          
202000       Set  LINK-ENDAST-SATSBEHOV To True                                 
202100       Move NEJ To LINK-FLINKLDIRLEV                                      
202200                                                                          
202300       Call W22222 Using LINK-AREA W222-WDK6-PCB  W222-WDK7-PCB           
202400                                   W222-ARTM-PCB  W222-2501-PCB           
202500                                   W222-WDB6R-PCB W222-WDK7R-PCB          
202600                                   W222-WDB6-PCB  W222-WDD7-PCB           
202700                                   W222-WDK7E-PCB                         
202800                                   W222-UTIL-WDK6-PCB                     
202900                                   W222-UTIL-WDK7-PCB                     
203000                                   W222-UTIL-WDB6-PCB                     
203100                                   W222-UTUP-WDK7-PCB                     
203200                                   W222-UTUP-WDB6-PCB                     
203300                                   W222-UTUP-UTIL-WDK6-PCB                
203400                                   W222-UTUP-UTIL-WDK7-PCB                
203500                                   W222-UTUP-UTIL-WDB6-PCB                
203600                                                                          
203700       If LINK-ANROP-OK                                                   
203800         Add LINK-KVBEHOV-DESSUTOM To TEMPWS-KVPB-SUM-VV                  
203900         Move +1 To IX                                                    
204000         Perform Until IX > LINK-KVVECKOR-BEHOV                           
204100            Add LINK-KVBEHOV-VECKA(IX) To TEMPWS-KVPB-SUM-VV              
204200            Add +1 To IX                                                  
204300         End-Perform                                                      
204400       Else                                                               
204500         String 'FEL i anrop FRÅN CCED-SATS-BEHOV '                       
204600                ' till W22222(behovsmodulen)'                             
204700                Delimited By Size Into FELTEXT-STR                        
204800         Display FELTEXT-STR ' ' TEMPWS-IDARTNR                           
204900       End-If                                                             
205000     Else                                                                 
205100       Continue                                                           
205200*       -- Inget SATS-behov att summera                                   
205300     End-If                                                               
205400     .                                                                    
205500     EJECT                                                                
205600                                                                          
205700 CCEE-TPO-BEHOV  SECTION.                                                 
205800     MOVE 'CCEE-TPO-BEHOV              ' TO CURRENT-SECTION               
205900                                                                          
206000     Compute LINK-KVVECKOR-BEHOV = WS10-KVVECKOR-KVPB - 1                 
206100                                                                          
206200     Set  LINK-ENDAST-LEVBEHOV To True                                    
206300     Move NEJ To LINK-FLINKLDIRLEV                                        
206400                                                                          
206500       Call W22222 Using LINK-AREA W222-WDK6-PCB  W222-WDK7-PCB           
206600                                   W222-ARTM-PCB  W222-2501-PCB           
206700                                   W222-WDB6R-PCB W222-WDK7R-PCB          
206800                                   W222-WDB6-PCB  W222-WDD7-PCB           
206900                                   W222-WDK7E-PCB                         
207000                                   W222-UTIL-WDK6-PCB                     
207100                                   W222-UTIL-WDK7-PCB                     
207200                                   W222-UTIL-WDB6-PCB                     
207300                                   W222-UTUP-WDK7-PCB                     
207400                                   W222-UTUP-WDB6-PCB                     
207500                                   W222-UTUP-UTIL-WDK6-PCB                
207600                                   W222-UTUP-UTIL-WDK7-PCB                
207700                                   W222-UTUP-UTIL-WDB6-PCB                
207800                                                                          
207900     If LINK-ANROP-OK                                                     
208000       Add LINK-KVBEHOV-DESSUTOM To TEMPWS-KVPB-SUM-VV                    
208100       Move +1 To IX                                                      
208200       Perform Until IX > LINK-KVVECKOR-BEHOV                             
208300          Add LINK-KVBEHOV-VECKA(IX) To TEMPWS-KVPB-SUM-VV                
208400          Add +1 To IX                                                    
208500       End-Perform                                                        
208600     Else                                                                 
208700       String 'FEL i anrop FRÅN CCEE-TPO-BEHOV '                          
208800              ' till W22222(behovsmodulen)'                               
208900              Delimited By Size Into FELTEXT-STR                          
209000       Display FELTEXT-STR ' ' TEMPWS-IDARTNR                             
209100     End-If                                                               
209200     .                                                                    
209300     EJECT                                                                
209400                                                                          
209500 CCEF-SDC-BEHOV  SECTION.                                                 
209600     MOVE 'CCEF-SDC-BEHOV              ' TO CURRENT-SECTION               
209700                                                                          
209800     If IN09-FLREFILL = JA                                                
209900       Compute LINK-KVVECKOR-BEHOV = WS10-KVVECKOR-KVPB - 1               
210000                                                                          
210100       Set LINK-ENDAST-SDCBEHOV To True                                   
210200       Move NEJ To LINK-FLINKLDIRLEV                                      
210300                                                                          
210400       Call W22222 Using LINK-AREA W222-WDK6-PCB  W222-WDK7-PCB           
210500                                   W222-ARTM-PCB  W222-2501-PCB           
210600                                   W222-WDB6R-PCB W222-WDK7R-PCB          
210700                                   W222-WDB6-PCB  W222-WDD7-PCB           
210800                                   W222-WDK7E-PCB                         
210900                                   W222-UTIL-WDK6-PCB                     
211000                                   W222-UTIL-WDK7-PCB                     
211100                                   W222-UTIL-WDB6-PCB                     
211200                                   W222-UTUP-WDK7-PCB                     
211300                                   W222-UTUP-WDB6-PCB                     
211400                                   W222-UTUP-UTIL-WDK6-PCB                
211500                                   W222-UTUP-UTIL-WDK7-PCB                
211600                                   W222-UTUP-UTIL-WDB6-PCB                
211700                                                                          
211800       If LINK-ANROP-OK                                                   
211900         Add LINK-KVBEHOV-DESSUTOM To TEMPWS-KVPB-SUM-VV                  
212000         Move +1 To IX                                                    
212100         Perform Until IX > LINK-KVVECKOR-BEHOV                           
212200           Add LINK-KVBEHOV-VECKA(IX) To TEMPWS-KVPB-SUM-VV               
212300           Add +1 To IX                                                   
212400         End-Perform                                                      
212500       Else                                                               
212600         String 'FEL i anrop FRÅN CCEF-SDC-BEHOV '                        
212700                ' till W22222(behovsmodulen)'                             
212800                Delimited By Size Into FELTEXT-STR                        
212900         Display FELTEXT-STR ' ' TEMPWS-IDARTNR                           
213000       End-If                                                             
213100     End-If                                                               
213200     .                                                                    
213300     EJECT                                                                
213400                                                                          
213500 CCEG-NDC-BEHOV  SECTION.                                                 
213600     MOVE 'CCEG-NDC-BEHOV              ' TO CURRENT-SECTION               
213700                                                                          
213800     Compute LINK-KVVECKOR-BEHOV = WS10-KVVECKOR-KVPB - 1                 
213900                                                                          
214000     Set LINK-ENDAST-NDCBEHOV To True                                     
214100     Move NEJ To LINK-FLINKLDIRLEV                                        
214200                                                                          
214300     Call W22222 Using LINK-AREA W222-WDK6-PCB  W222-WDK7-PCB             
214400                                 W222-ARTM-PCB  W222-2501-PCB             
214500                                 W222-WDB6R-PCB W222-WDK7R-PCB            
214600                                 W222-WDB6-PCB  W222-WDD7-PCB             
214700                                 W222-WDK7E-PCB                           
214800                                 W222-UTIL-WDK6-PCB                       
214900                                 W222-UTIL-WDK7-PCB                       
215000                                 W222-UTIL-WDB6-PCB                       
215100                                 W222-UTUP-WDK7-PCB                       
215200                                 W222-UTUP-WDB6-PCB                       
215300                                 W222-UTUP-UTIL-WDK6-PCB                  
215400                                 W222-UTUP-UTIL-WDK7-PCB                  
215500                                 W222-UTUP-UTIL-WDB6-PCB                  
215600                                                                          
215700     If LINK-ANROP-OK                                                     
215800       Add LINK-KVBEHOV-DESSUTOM To TEMPWS-KVPB-SUM-VV                    
215900       Move +1 To IX                                                      
216000       Perform Until IX > LINK-KVVECKOR-BEHOV                             
216100         Add LINK-KVBEHOV-VECKA(IX) To TEMPWS-KVPB-SUM-VV                 
216200         Add +1 To IX                                                     
216300       End-Perform                                                        
216400     Else                                                                 
216500       String 'FEL i anrop FRÅN CCEG-NDC-BEHOV '                          
216600              ' till W22222(behovsmodulen)'                               
216700              Delimited By Size Into FELTEXT-STR                          
216800       Display FELTEXT-STR ' ' TEMPWS-IDARTNR                             
216900     End-If                                                               
217000     .                                                                    
217100     EJECT                                                                
217200                                                                          
217300 CCF-HAMTA-FORAVIS-FRAN-W6D1 SECTION.                                     
217400     MOVE 'CCF-HAMTA-FORAVIS-FRAN-W6D1 ' TO CURRENT-SECTION               
217500*                               kopierat från w2010200  /c.e.             
217600     Move Zero To W-KVART-TOT-C1                                          
217700                                                                          
217800     Move TEMPWS-IDARTNR   To W-IDARTNR-HSEQ                              
217900                                                                          
218000     Perform IMS-GN-W6D111-W6D1SEQ                                        
218100     Perform Until SEGMENT-SAKNAS                                         
218200       Move W6D1-ART-IDDC To WS-IDDC                                      
218300       If (CDC-SE Or CDC-TR) And W6D1-ART-IDLOPNRM = Zero                 
218400         If W6D1-ART-FLFEL = NEJ                                          
218500           Add W6D1-ART-KVAVIS To W-KVART-TOT-C1                          
218600         End-If                                                           
218700       End-If                                                             
218800       Perform IMS-GN-W6D111-W6D1SEQ                                      
218900     End-Perform                                                          
219000                                                                          
219100     Move W-KVART-TOT-C1 To TEMPWS-KVAVIS-FORAVIS                         
219200     .                                                                    
219300     EJECT                                                                
219400                                                                          
219500 CCH-HAMTA-KVBR-VALID-OCH-OVR SECTION.                                    
219600     MOVE 'CCH-HAMTA-KVBR-VALID-OCH-OVR' TO CURRENT-SECTION               
219700                                                                          
219800     Move Zero  To TEMPWS-KVBR-VALID-LEV                                  
219900                   WS-KVBR-VALID-LEV                                      
220000                   TEMPWS-KVBR-OVR-LEV                                    
220100                   WS-KVBR-OVR-LEV                                        
220200     Move IN09-IDARTNR To W-IDARTNR                                       
220300                          W-IDARTNR-D9                                    
220400     Move WC-CDC-SE    To W-IDDC-D9                                       
220500*                                                                         
220600     IF WS10-FL-IDLEVNR-SHIP = 'X'                                        
220700       MOVE IN09-IDLEVNR-SHIP To W-IDLEVNR                                
220800     ELSE                                                                 
220900       Move IN09-IDLEVNR      To W-IDLEVNR                                
221000     END-IF                                                               
221100*                                                                         
221200     Perform IMS-GU-WDD901                                                
221300     If SEGMENT-FINNS                                                     
221400       Perform IMS-GNP-WDD902                                             
221500       Perform Until SEGMENT-SAKNAS                                       
221600         If WDD902-IDLEVNR = W-IDLEVNR                                    
221700            Add WDD902-KVBR To WS-KVBR-VALID-LEV                          
221800         Else                                                             
221900            Add WDD902-KVBR To WS-KVBR-OVR-LEV                            
222000         End-If                                                           
222100         Perform IMS-GNP-WDD902                                           
222200       End-Perform                                                        
222300       Move WS-KVBR-VALID-LEV To TEMPWS-KVBR-VALID-LEV                    
222400       Move WS-KVBR-OVR-LEV   To TEMPWS-KVBR-OVR-LEV                      
222500     End-If                                                               
222600     .                                                                    
222700     EJECT                                                                
222800 CCI-HAMTA-SEASONG  SECTION.                                              
222900     MOVE 'CCI-HAMTA-SEASONG           ' TO CURRENT-SECTION               
223000                                                                          
223100     Move Zero            To TEMPWS-DASPSEA                               
223200                             TEMPWS-OSAKERHET                             
223300     Move Space           To TEMPWS-SEASON-ARTIKEL                        
223400     Move TEMPWS-IDARTNR  To LINKS-SEAS-IDARTNR                           
223500     Move NEJ             To LINKS-SEAS-FLKVARTAL                         
223600     Move TEMPWS-IDFKNGRP To LINKS-SEAS-IDFKNGRP                          
223700                                                                          
223800     Move TEMPWS-IDARTNR  To W-IDARTNR                                    
223900     Perform IMS-GU-WDK626                                                
224000     If SEGMENT-FINNS                                                     
224100       Move JUST-DASPSEA        To TEMPWS-DASPSEA                         
224200     End-If                                                               
224300                                                                          
224400     Call W222SEAS Using LINKS-SEAS-W222SEAS WDL8-PCB                     
224500     If LINKS-SEAS-KDSVAR = Space                                         
224600       Move LINKS-SEAS-OSAKERHET To TEMPWS-OSAKERHET                      
224700       If LINKS-SEAS-SEASON-ARTIKEL = JA                                  
224800         Move 'Y' To TEMPWS-SEASON-ARTIKEL                                
224900       Else                                                               
225000         Move 'N' To TEMPWS-SEASON-ARTIKEL                                
225100       End-If                                                             
225200     End-If                                                               
225300     .                                                                    
225400     EJECT                                                                
225500 CCK-HAMTA-WDA5A-RORADER SECTION.                                         
225600     MOVE 'CCK-HAMTA-WDA5A-RORADER     ' TO CURRENT-SECTION               
225700     SKIP2                                                                
225800*    "VOR/RO" Lista 7                                                     
225900     Move LOW-VALUE            TO W-WDA5A1KY-MIN                          
226000     Move HIGH-VALUE           TO W-WDA5A1KY-MAX                          
226100     MOVE IN09-IDARTNR         TO W-IDARTNR-MIN                           
226200                                  W-IDARTNR-MAX                           
226300     Move '11'                 TO W-IDDC-MIN                              
226400                                  W-IDDC-MAX                              
226500     Move '2'                  TO W-KDSTARAD-MIN                          
226600                                  W-KDSTARAD-MAX                          
226700     Move +0 To TEMPWS-KVRORAD                                            
226800                                                                          
226900     Perform IMS-GN-WDA5A                                                 
227000     Perform UNTIL SEGMENT-SAKNAS                                         
227100        Add +1 To TEMPWS-KVRORAD                                          
227200        Perform IMS-GN-WDA5A                                              
227300     END-Perform                                                          
227400     .                                                                    
227500     EJECT                                                                
227600 CH-PROCESS-OUTPUT SECTION.                                               
227700     MOVE 'CH-PROCESS-OUTPUT           ' TO CURRENT-SECTION               
227800                                                                          
227900     MOVE TEMPWS-IDANSK       TO OLD-IDANSK                               
228000     MOVE TEMPWS-IDLEVNR      TO OLD-IDLEVNR                              
228100                                                                          
228200     IF FIRST-REC                                                         
228300        PERFORM S7-SKAPA-EXCEL-RUBRIK                                     
228400        MOVE NEJ              TO FIRST-REC-SW                             
228500     END-IF                                                               
228600                                                                          
228700     PERFORM CHA-SKAPA-EXCELRAD                                           
228800                                                                          
228900     PERFORM S21-SKRIV-W21710-001                                         
229000     .                                                                    
229100     EJECT                                                                
229200                                                                          
229300 CHA-SKAPA-EXCELRAD SECTION.                                              
229400     MOVE 'DE-SKAPA-EXCELRAD           ' TO CURRENT-SECTION               
229500                                                                          
229600     Move Space               To W-WRITE-PART-ON-EXCEL                    
229700                                                                          
229800     Move 1 To LAENGD                                                     
229900                                                                          
230000     Move TEMPWS-IDARTNR      To WS-IDARTNR                               
230100     Move WS-IDARTNR          To W002-ART-RAD (LAENGD:9)                  
230200     Add 9                    To LAENGD                                   
230300     Move TAB-TECKEN          To W002-ART-RAD(LAENGD:1)                   
230400     Add 1                    To LAENGD                                   
230500                                                                          
230600* Lev.                                                                    
230700     Move TEMPWS-IDLEVNR      To WS-IDLEVNR                               
230800     Move TEMPWS-IDLEVNR-SHIP To WS-IDLEVNR-SHIP                          
230900     If WS-FLAGGA-LISTA (1) > Space                                       
231000        Move WS-IDLEVNR       To W002-ART-RAD (LAENGD:5)                  
231100        Add 5                 To LAENGD                                   
231200        Move TAB-TECKEN       To W002-ART-RAD(LAENGD:1)                   
231300        Add 1                 To LAENGD                                   
231400                                                                          
231500        Move WS-IDLEVNR-SHIP  To W002-ART-RAD (LAENGD:5)                  
231600        Add 5                 To LAENGD                                   
231700        Move TAB-TECKEN       To W002-ART-RAD(LAENGD:1)                   
231800        Add 1                 To LAENGD                                   
231900     End-If                                                               
232000                                                                          
232100* Lev.bet                                                                 
232200     If WS-FLAGGA-LISTA (2) > Space                                       
232300*       -- Leverantörens Art-Beteckning                                   
232400        Move TEMPWS-BELEV     To W-LEVBET                                 
232500        Move W-LEVBET         To W002-ART-RAD (LAENGD:25)                 
232600        Add 25                To LAENGD                                   
232700        Move TAB-TECKEN       To W002-ART-RAD(LAENGD:1)                   
232800        Add 1                 To LAENGD                                   
232900     End-If                                                               
233000                                                                          
233100* Ben.(S)                                                                 
233200     If WS-FLAGGA-LISTA (3) > Space                                       
233300        Move TEMPWS-BEART-SVE To W-BEART-S                                
233400        Move W-BEART-S        To W002-ART-RAD (LAENGD:25)                 
233500        Add 25                To LAENGD                                   
233600        Move TAB-TECKEN       To W002-ART-RAD(LAENGD:1)                   
233700        Add 1                 To LAENGD                                   
233800     End-If                                                               
233900                                                                          
234000* Ben.(GB)                                                                
234100     If WS-FLAGGA-LISTA (4) > Space                                       
234200        Move TEMPWS-BEART-ENG To W-BEART-GB                               
234300        Move W-BEART-GB       To W002-ART-RAD (LAENGD:25)                 
234400        Add 25                To LAENGD                                   
234500        Move TAB-TECKEN       To W002-ART-RAD(LAENGD:1)                   
234600        Add 1                 To LAENGD                                   
234700     End-If                                                               
234800                                                                          
234900* St.on hnd                                                               
235000     If WS-FLAGGA-LISTA (5) > Space                                       
235100* CDC                                                                     
235200        Compute W-ST-ON-HND-CDC Rounded =                                 
235300                TEMPWS-KVLS-CDC - TEMPWS-KVRESS-CDC                       
235400        Move W-ST-ON-HND-CDC To W002-ART-RAD (LAENGD:7)                   
235500        Add 7                 To LAENGD                                   
235600        Move TAB-TECKEN       To W002-ART-RAD(LAENGD:1)                   
235700        Add 1                 To LAENGD                                   
235800* SDC+LDC                                                                 
235900        Compute W-ST-ON-HND-SDC-LDC Rounded =                             
236000                TEMPWS-KVLS-SDC-LDC - TEMPWS-KVRESS-SDC-LDC               
236100        Move W-ST-ON-HND-SDC-LDC To W002-ART-RAD (LAENGD:7)               
236200        Add 7                 To LAENGD                                   
236300        Move TAB-TECKEN       To W002-ART-RAD(LAENGD:1)                   
236400        Add 1                 To LAENGD                                   
236500* NDC                                                                     
236600        Compute W-ST-ON-HND-NDC Rounded =                                 
236700                TEMPWS-KVLS-NDC     - TEMPWS-KVRESS-NDC                   
236800        Move W-ST-ON-HND-NDC To W002-ART-RAD (LAENGD:7)                   
236900        Add 7                 To LAENGD                                   
237000        Move TAB-TECKEN       To W002-ART-RAD(LAENGD:1)                   
237100        Add 1                 To LAENGD                                   
237200     End-If                                                               
237300                                                                          
237400* AK saldo                                                                
237500     If WS-FLAGGA-LISTA (6) > Space                                       
237600*       -- AK                                                             
237700        Move TEMPWS-KVAKS     To W-KVAKS                                  
237800        Move W-KVAKS          To W002-ART-RAD (LAENGD:7)                  
237900        Add 7                 To LAENGD                                   
238000        Move TAB-TECKEN       To W002-ART-RAD(LAENGD:1)                   
238100        Add 1                 To LAENGD                                   
238200     End-If                                                               
238300* VOR/RO                                                                  
238400     If WS-FLAGGA-LISTA (07) > Space                                      
238500*       -- VOR / RO                                                       
238600*       1. --  RO-saldo                                                   
238700        Move TEMPWS-KVROS     To W-KVROS                                  
238800        Move W-KVROS          To W002-ART-RAD (LAENGD:7)                  
238900        Add 7                 To LAENGD                                   
239000        Move TAB-TECKEN       To W002-ART-RAD(LAENGD:1)                   
239100        Add 1                 To LAENGD                                   
239200*       2. --  RO-rader                                                   
239300        Move TEMPWS-KVRORAD   To W-KVRORAD                                
239400        Move W-KVRORAD        To W002-ART-RAD (LAENGD:5)                  
239500        Add 5                 To LAENGD                                   
239600        Move TAB-TECKEN       To W002-ART-RAD(LAENGD:1)                   
239700        Add 1                 To LAENGD                                   
239800*       3. --  VOR-saldo                                                  
239900        Move TEMPWS-KVVORKO   To W-KVVORKO                                
240000        Move W-KVVORKO        To W002-ART-RAD (LAENGD:7)                  
240100        Add 7                 To LAENGD                                   
240200        Move TAB-TECKEN       To W002-ART-RAD(LAENGD:1)                   
240300        Add 1                 To LAENGD                                   
240400                                                                          
240500        If WS-FLAGGA-LISTA (07) = 'S'                                     
240600           If TEMPWS-KVROS   = 0                                          
240700          And TEMPWS-KVRORAD = 0                                          
240800          And TEMPWS-KVVORKO = 0                                          
240900              If W-WRITE-PART-ON-EXCEL not = JA                           
241000                 Move NEJ     To W-WRITE-PART-ON-EXCEL                    
241100              End-if                                                      
241200           Else                                                           
241300              Move JA         To W-WRITE-PART-ON-EXCEL                    
241400           End-if                                                         
241500        End-if                                                            
241600     End-If                                                               
241700                                                                          
241800* Släp                                                                    
241900     If WS-FLAGGA-LISTA (08) > Space                                      
242000*       * Släp                                                            
242100*       1. ARREARS                                                        
242200        Move TEMPWS-KVSLAP-SUM To W-KVSLAP-SUM                            
242300        Move W-KVSLAP-SUM     To W002-ART-RAD (LAENGD:7)                  
242400        Add 7                 To LAENGD                                   
242500        Move TAB-TECKEN       To W002-ART-RAD(LAENGD:1)                   
242600        Add 1                 To LAENGD                                   
242700*       2.  NOT RECEIVED                                                  
242800        Move TEMPWS-KVAVIS-NOT-REC To W-KVAVIS-NOT-REC                    
242900        Move W-KVAVIS-NOT-REC To W002-ART-RAD (LAENGD:7)                  
243000        Add 7                 To LAENGD                                   
243100        Move TAB-TECKEN       To W002-ART-RAD(LAENGD:1)                   
243200        Add 1                 To LAENGD                                   
243300                                                                          
243400        If WS-FLAGGA-LISTA (08) = 'S'                                     
243500           If TEMPWS-KVSLAP-SUM     = 0                                   
243600          And TEMPWS-KVAVIS-NOT-REC = 0                                   
243700              If W-WRITE-PART-ON-EXCEL Not = JA                           
243800                 Move NEJ     To W-WRITE-PART-ON-EXCEL                    
243900              End-if                                                      
244000           Else                                                           
244100              Move JA         To W-WRITE-PART-ON-EXCEL                    
244200           End-if                                                         
244300        End-if                                                            
244400     End-If                                                               
244500                                                                          
244600* Lev.Besk                                                                
244700     If WS-FLAGGA-LISTA (09) > Space                                      
244800*       -- Lev.Besk.                                                      
244900*       1--- Dispatch Week (WDD924)                                       
245000        Move TEMPWS-TILEVBSK-AVS To W-TILEVBSK-AVS                        
245100        Move W-TILEVBSK-AVS      To W002-ART-RAD (LAENGD:6)               
245200        Add 6                 To LAENGD                                   
245300        Move TAB-TECKEN       To W002-ART-RAD(LAENGD:1)                   
245400        Add 1                 To LAENGD                                   
245500*       2--- Dispatch Qty (WDD924)                                        
245600        Move TEMPWS-KVAVIS-BSKKVAR To W-KVAVIS-BSKKVAR                    
245700        Move W-KVAVIS-BSKKVAR To W002-ART-RAD (LAENGD:6)                  
245800        Add 6                 To LAENGD                                   
245900        Move TAB-TECKEN       To W002-ART-RAD(LAENGD:1)                   
246000        Add 1                 To LAENGD                                   
246100*       3--- CD plan incom. deliv. (WDD924)                               
246200        Move TEMPWS-TILEVBSK-INL-C1 To W-TILEVBSK-INL-C1                  
246300        Move W-TILEVBSK-INL-C1      To W002-ART-RAD (LAENGD:6)            
246400        Add 6                 To LAENGD                                   
246500        Move TAB-TECKEN       To W002-ART-RAD(LAENGD:1)                   
246600        Add 1                 To LAENGD                                   
246700*       4--- CD avail. Week (2106) (WDD924)                               
246800        Move TEMPWS-TILEVBSK-DISP-C1 To W-TILEVBSK-DISP-C1                
246900        Move W-TILEVBSK-DISP-C1      To W002-ART-RAD (LAENGD:6)           
247000        Add 6                 To LAENGD                                   
247100        Move TAB-TECKEN       To W002-ART-RAD(LAENGD:1)                   
247200        Add 1                 To LAENGD                                   
247300*       5--- X-info Date (EXT on 2106) (WDD925)                           
247400        Move TEMPWS-TIBORT-INFO To W-TIBORT-INFO                          
247500        If W-TIBORT-INFO > Zero                                           
247600          Move W-TIBORT-INFO  To W002-ART-RAD (LAENGD:6)                  
247700        End-If                                                            
247800        Add 6                 To LAENGD                                   
247900        Move TAB-TECKEN       To W002-ART-RAD(LAENGD:1)                   
248000        Add 1                 To LAENGD                                   
248100     End-If                                                               
248200                                                                          
248300* FörAvis.                                                                
248400     If WS-FLAGGA-LISTA (10) > Space                                      
248500        Move TEMPWS-KVAVIS-FORAVIS To W-KVAVIS-FORAVIS                    
248600        Move W-KVAVIS-FORAVIS To W002-ART-RAD (LAENGD:8)                  
248700                                                                          
248800        Add 8                 To LAENGD                                   
248900        Move TAB-TECKEN       To W002-ART-RAD(LAENGD:1)                   
249000        Add 1                 To LAENGD                                   
249100     End-If                                                               
249200                                                                          
249300* TPO                                                                     
249400     If WS-FLAGGA-LISTA (11) > Space                                      
249500        If TEMPWS-TITPO > 0                                               
249600           MOVE TEMPWS-TITPO  TO DAT-I-TIDATUM                            
249700           MOVE 'AAMMDD'      TO DAT-KDDATFORM                            
249800           CALL WDATKONV   USING DAT-KDDATFORM                            
249900                                 DAT-I-TIDATUM                            
250000                                 DAT-O-TIDATUM                            
250100                                 DAT-KDSVAR                               
250200           MOVE DAT-TIAAVVD   TO W002-ART-RAD (LAENGD:5)                  
250300        Else                                                              
250400           MOVE SPACE         TO W002-ART-RAD (LAENGD:5)                  
250500        End-if                                                            
250600                                                                          
250700        Add 5                 To LAENGD                                   
250800        Move TAB-TECKEN       To W002-ART-RAD(LAENGD:1)                   
250900        Add 1                 To LAENGD                                   
251000                                                                          
251100        Move TEMPWS-KVART     To W-KVART                                  
251200        Move W-KVART          To W002-ART-RAD (LAENGD:7)                  
251300        Add 7                 To LAENGD                                   
251400        Move TAB-TECKEN       To W002-ART-RAD(LAENGD:1)                   
251500        Add 1                 To LAENGD                                   
251600                                                                          
251700        If WS-FLAGGA-LISTA (11) = 'S'                                     
251800           If TEMPWS-TITPO = 0                                            
251900          And TEMPWS-KVART = 0                                            
252000              If W-WRITE-PART-ON-EXCEL Not = JA                           
252100                 Move NEJ     To W-WRITE-PART-ON-EXCEL                    
252200              End-if                                                      
252300           Else                                                           
252400              Move JA         To W-WRITE-PART-ON-EXCEL                    
252500           End-if                                                         
252600        End-if                                                            
252700     End-If                                                               
252800                                                                          
252900* Ledtid                                                                  
253000     If WS-FLAGGA-LISTA (12) > Space                                      
253100*       -- Ledtid                                                         
253200        Move TEMPWS-KVVECKOR-LT To W-KVVECKOR-LT                          
253300        Move W-KVVECKOR-LT    To W002-ART-RAD (LAENGD:3)                  
253400        Add 3                 To LAENGD                                   
253500        Move TAB-TECKEN       To W002-ART-RAD(LAENGD:1)                   
253600        Add 1                 To LAENGD                                   
253700     End-If                                                               
253800                                                                          
253900* Erskod                                                                  
254000     If WS-FLAGGA-LISTA (13) > Space                                      
254100        Move TEMPWS-KDERS     To W-KDERS                                  
254200        Move W-KDERS          To W002-ART-RAD (LAENGD:2)                  
254300        Add 2                 To LAENGD                                   
254400        Move TAB-TECKEN       To W002-ART-RAD(LAENGD:1)                   
254500        Add 1                 To LAENGD                                   
254600     End-If                                                               
254700                                                                          
254800* Ansk/Ber                                                                
254900     If WS-FLAGGA-LISTA (14) > Space                                      
255000*       -- Anskaffare                                                     
255100        Move TEMPWS-IDANSK    To W-IDANSK                                 
255200        Move W-IDANSK         To W002-ART-RAD (LAENGD:3)                  
255300        Add 3                 To LAENGD                                   
255400        Move TAB-TECKEN       To W002-ART-RAD(LAENGD:1)                   
255500        Add 1                 To LAENGD                                   
255600                                                                          
255700        Move TEMPWS-IDBERED   To W-IDBERED                                
255800        Move W-IDBERED        To W002-ART-RAD (LAENGD:3)                  
255900        Add 3                 To LAENGD                                   
256000        Move TAB-TECKEN       To W002-ART-RAD(LAENGD:1)                   
256100        Add 1                 To LAENGD                                   
256200     End-If                                                               
256300                                                                          
256400* Dir.lev                                                                 
256500     If WS-FLAGGA-LISTA (15) > Space                                      
256600*       -- DIRLEV                                                         
256700        Move TEMPWS-REDIRLEV  To W-REDIRLEV                               
256800        Move W-REDIRLEV       To W002-ART-RAD (LAENGD:5)                  
256900        Add 5                 To LAENGD                                   
257000        Move TAB-TECKEN       To W002-ART-RAD(LAENGD:1)                   
257100        Add 1                 To LAENGD                                   
257200     End-If                                                               
257300                                                                          
257400* Avs.dag                                                                 
257500     If WS-FLAGGA-LISTA (16) > Space                                      
257600*       -- Avsdag                                                         
257700        MOVE +0               To IX1                                      
257800        MOVE +0               To IX2                                      
257900        Perform until IX1 = 5                                             
258000          Add +1              To IX1                                      
258100          If TEMPWS-TILEVDAG (IX1) not = 0                                
258200             Add +1           To IX2                                      
258300             Move TEMPWS-TILEVDAG (IX1) To W-TILEVDAG (IX2)               
258400          End-if                                                          
258500        End-Perform                                                       
258600                                                                          
258700        Perform until IX2 = 5                                             
258800          Add +1              To IX2                                      
258900          Move Zero           To W-TILEVDAG (IX2)                         
259000        End-Perform                                                       
259100        Move W-TILEVDAG-TAB   To                                          
259200                              W002-ART-RAD(LAENGD:15)                     
259300        Add 15                To LAENGD                                   
259400        Move TAB-TECKEN       To W002-ART-RAD(LAENGD:1)                   
259500        Add 1                 To LAENGD                                   
259600     End-If                                                               
259700                                                                          
259800* Trend                                                                   
259900     If WS-FLAGGA-LISTA (17) > Space                                      
260000*       -- Trend                                                          
260100        Move TEMPWS-KVPB-TREND To W-KVPB-TREND                            
260200        Move W-KVPB-TREND     To W002-ART-RAD (LAENGD:10)                 
260300        Add 10                To LAENGD                                   
260400        Move TAB-TECKEN       To W002-ART-RAD(LAENGD:1)                   
260500        Add 1                 To LAENGD                                   
260600                                                                          
260700        Move TEMPWS-KVVECKOR-TREND TO W-KVVECKOR-TREND                    
260800        Move W-KVVECKOR-TREND To W002-ART-RAD (LAENGD:2)                  
260900        Add 2                 To LAENGD                                   
261000        Move TAB-TECKEN       To W002-ART-RAD(LAENGD:1)                   
261100        Add 1                 To LAENGD                                   
261200        Move TEMPWS-TIDATUM-TREND TO W-TIDATUM-TREND                      
261300        Move W-TIDATUM-TREND  To W002-ART-RAD (LAENGD:6)                  
261400        Add 6                 To LAENGD                                   
261500        Move TAB-TECKEN       To W002-ART-RAD(LAENGD:1)                   
261600        Add 1                 To LAENGD                                   
261700                                                                          
261800        If WS-FLAGGA-LISTA (17) = 'S'                                     
261900           If TEMPWS-KVPB-TREND     = 0                                   
262000          And TEMPWS-KVVECKOR-TREND = 0                                   
262100          And TEMPWS-TIDATUM-TREND  = 0                                   
262200              If W-WRITE-PART-ON-EXCEL Not = JA                           
262300                 Move NEJ     To W-WRITE-PART-ON-EXCEL                    
262400              End-if                                                      
262500           Else                                                           
262600              Move JA         To W-WRITE-PART-ON-EXCEL                    
262700           End-if                                                         
262800        End-if                                                            
262900     End-If                                                               
263000                                                                          
263100* Säsong                                                                  
263200     If WS-FLAGGA-LISTA (18) > Space                                      
263300*       -- Säsong                                                         
263400        Move TEMPWS-DASPSEA   To W-DASPSEA                                
263500        Move W-DASPSEA        To W002-ART-RAD (LAENGD:8)                  
263600        Add 8                 To LAENGD                                   
263700        Move TAB-TECKEN       To W002-ART-RAD(LAENGD:1)                   
263800        Add 1                 To LAENGD                                   
263900                                                                          
264000        Move TEMPWS-SEASON-ARTIKEL To W-SEASON-ARTIKEL                    
264100        Move W-SEASON-ARTIKEL To W002-ART-RAD (LAENGD:2)                  
264200        Add 2                 To LAENGD                                   
264300        Move TAB-TECKEN       To W002-ART-RAD(LAENGD:1)                   
264400        Add 1                 To LAENGD                                   
264500                                                                          
264600        Move TEMPWS-OSAKERHET To W-OSAKERHET                              
264700        Move W-OSAKERHET      To W002-ART-RAD (LAENGD:6)                  
264800        Add 6                 To LAENGD                                   
264900        Move TAB-TECKEN       To W002-ART-RAD(LAENGD:1)                   
265000        Add 1                 To LAENGD                                   
265100        If WS-FLAGGA-LISTA (18) = 'S'                                     
265200           If TEMPWS-SEASON-ARTIKEL = NEJ                                 
265300              If W-WRITE-PART-ON-EXCEL Not = JA                           
265400                 Move NEJ     To W-WRITE-PART-ON-EXCEL                    
265500              End-if                                                      
265600           Else                                                           
265700              Move JA         To W-WRITE-PART-ON-EXCEL                    
265800           End-if                                                         
265900        End-if                                                            
266000     End-If                                                               
266100                                                                          
266200* Sen.inlev                                                               
266300     If WS-FLAGGA-LISTA (19) > Space                                      
266400        Move +1 To IX                                                     
266500        Perform Until IX > +5                                             
266600          Initialize    W-SENASTE-INLEV                                   
266700                                                                          
266800          If TEMPWS-TIAVIDAT-SEN (IX) > Zero                              
266900          Or TEMPWS-KVANTAL-SEN  (IX) > Zero                              
267000            Move TEMPWS-TIAVIDAT-SEN (IX) To W-TIAVIDAT-SEN               
267100            Move TEMPWS-IDFS-SEN     (IX) To W-IDFS-SEN                   
267200            Move TEMPWS-KVANTAL-SEN  (IX) To W-KVANTAL-SEN                
267300          Else                                                            
267400            If W-TIAVIDAT-SEN = Zero                                      
267500              Inspect W-TIAVIDAT-SEN Replacing All '0' By ' '             
267600            End-If                                                        
267700          End-If                                                          
267800                                                                          
267900          Move W-SENASTE-INLEV To W002-ART-RAD (LAENGD:23)                
268000                                                                          
268100          Add 23               To LAENGD                                  
268200          Move TAB-TECKEN      To W002-ART-RAD(LAENGD:1)                  
268300          Add 1                To LAENGD                                  
268400                                                                          
268500          Add +1 To IX                                                    
268600        End-Perform                                                       
268700     End-If                                                               
268800                                                                          
268900* PB                                                                      
269000     If WS-FLAGGA-LISTA (20) > Space                                      
269100*       -- PB                                                             
269200*       1. PB-SEP                                                         
269300        Move TEMPWS-KVPB-SEP  To W-KVPB-SEP                               
269400        If TEMPWS-KVPB-SEP > Zero                                         
269500          Move W-KVPB-SEP     To W002-ART-RAD (LAENGD:9)                  
269600        End-If                                                            
269700        Add 9                 To LAENGD                                   
269800        Move TAB-TECKEN       To W002-ART-RAD(LAENGD:1)                   
269900        Add 1                 To LAENGD                                   
270000                                                                          
270100*       2. PB-tot                                                         
270200        Move TEMPWS-KVPB-TOT  To W-KVPB-TOT                               
270300        If TEMPWS-KVPB-TOT > Zero                                         
270400          Move W-KVPB-TOT     To W002-ART-RAD (LAENGD:9)                  
270500        End-If                                                            
270600        Add 9                 To LAENGD                                   
270700        Move TAB-TECKEN       To W002-ART-RAD(LAENGD:1)                   
270800        Add 1                 To LAENGD                                   
270900                                                                          
271000*       3. PB-sats                                                        
271100        Move TEMPWS-KVPB-SATS To W-KVPB-SATS                              
271200        If TEMPWS-KVPB-SATS > Zero                                        
271300          Move W-KVPB-SATS    To W002-ART-RAD (LAENGD:9)                  
271400        End-If                                                            
271500        Add 9                 To LAENGD                                   
271600        Move TAB-TECKEN       To W002-ART-RAD(LAENGD:1)                   
271700        Add 1                 To LAENGD                                   
271800                                                                          
271900*       4. PB-plan                                                        
272000        Move TEMPWS-KVPB-PLAN To W-KVPB-PLAN                              
272100        If TEMPWS-KVPB-PLAN > Zero                                        
272200          Move W-KVPB-PLAN    To W002-ART-RAD (LAENGD:9)                  
272300        End-If                                                            
272400        Add 9                 To LAENGD                                   
272500        Move TAB-TECKEN       To W002-ART-RAD(LAENGD:1)                   
272600        Add 1                 To LAENGD                                   
272700*       5. Datum PB-plan till                                             
272800        If   TEMPWS-DAPBPLAN > DAGENS-DADATUM                             
272900          Move TEMPWS-DAPBPLAN      To W-TIPBPLAN                         
273000        Else                                                              
273100          Move Zero                 To W-TIPBPLAN                         
273200        End-If                                                            
273300        Move W-TIPBPLAN        To W002-ART-RAD (LAENGD:6)                 
273400        Add 6                  To LAENGD                                  
273500        Move TAB-TECKEN        To W002-ART-RAD(LAENGD:1)                  
273600        Add 1                  To LAENGD                                  
273700     End-If                                                               
273800                                                                          
273900* Oi ru 12                                                                
274000     If WS-FLAGGA-LISTA (21) > Space                                      
274100        Move TEMPWS-KVOI-12-RULL To W-KVOIRULL                            
274200        Move W-KVOIRULL       To W002-ART-RAD (LAENGD:8)                  
274300        Add 8                 To LAENGD                                   
274400        Move TAB-TECKEN       To W002-ART-RAD(LAENGD:1)                   
274500        Add 1                 To LAENGD                                   
274600     End-If                                                               
274700                                                                          
274800* Oi(iår+5)                                                               
274900     If WS-FLAGGA-LISTA (22) > Space                                      
275000*       -- Oi(iår+5)                                                      
275100        Move TEMPWS-KVOI-YEAR-0 To W-KVOI-YEAR-0                          
275200        Move TEMPWS-KVOI-YEAR-1 To W-KVOI-YEAR-1                          
275300        Move TEMPWS-KVOI-YEAR-2 To W-KVOI-YEAR-2                          
275400        Move TEMPWS-KVOI-YEAR-3 To W-KVOI-YEAR-3                          
275500        Move TEMPWS-KVOI-YEAR-4 To W-KVOI-YEAR-4                          
275600        Move TEMPWS-KVOI-YEAR-5 To W-KVOI-YEAR-5                          
275700*       1. OI iår                                                         
275800        Move W-KVOI-YEAR-0    To W002-ART-RAD (LAENGD:8)                  
275900        Add 8                 To LAENGD                                   
276000        Move TAB-TECKEN       To W002-ART-RAD(LAENGD:1)                   
276100        Add 1                 To LAENGD                                   
276200*       2. OI förra året                                                  
276300        Move W-KVOI-YEAR-1    To W002-ART-RAD (LAENGD:8)                  
276400        Add 8                 To LAENGD                                   
276500        Move TAB-TECKEN       To W002-ART-RAD(LAENGD:1)                   
276600        Add 1                 To LAENGD                                   
276700*       3. OI iår minus 2                                                 
276800        Move W-KVOI-YEAR-2    To W002-ART-RAD (LAENGD:8)                  
276900        Add 8                 To LAENGD                                   
277000        Move TAB-TECKEN       To W002-ART-RAD(LAENGD:1)                   
277100        Add 1                 To LAENGD                                   
277200*       4. OI iår minus 3                                                 
277300        Move W-KVOI-YEAR-3    To W002-ART-RAD (LAENGD:8)                  
277400        Add 8                 To LAENGD                                   
277500        Move TAB-TECKEN       To W002-ART-RAD(LAENGD:1)                   
277600        Add 1                 To LAENGD                                   
277700*       5. OI iår minus 4                                                 
277800        Move W-KVOI-YEAR-4    To W002-ART-RAD (LAENGD:8)                  
277900        Add 8                 To LAENGD                                   
278000        Move TAB-TECKEN       To W002-ART-RAD(LAENGD:1)                   
278100        Add 1                 To LAENGD                                   
278200*       6. OI iår minus 5                                                 
278300        Move W-KVOI-YEAR-5    To W002-ART-RAD (LAENGD:8)                  
278400        Add 8                 To LAENGD                                   
278500        Move TAB-TECKEN       To W002-ART-RAD(LAENGD:1)                   
278600        Add 1                 To LAENGD                                   
278700     End-If                                                               
278800                                                                          
278900* Aut/JIT                                                                 
279000     If WS-FLAGGA-LISTA (23) > Space                                      
279100*       -- kdlevplf                                                       
279200        Move TEMPWS-KDLEVPLF  To W-KDLEVPLF                               
279300        Move W-KDLEVPLF       To W002-ART-RAD (LAENGD:1)                  
279400        Add 1                 To LAENGD                                   
279500        Move TAB-TECKEN       To W002-ART-RAD(LAENGD:1)                   
279600        Add 1                 To LAENGD                                   
279700*       -- fljit                                                          
279800        Move TEMPWS-FLJIT     To W-FLJIT                                  
279900        Move W-FLJIT          To W002-ART-RAD (LAENGD:1)                  
280000        Add 1                 To LAENGD                                   
280100        Move TAB-TECKEN       To W002-ART-RAD(LAENGD:1)                   
280200        Add 1                 To LAENGD                                   
280300     End-If                                                               
280400                                                                          
280500* Inköpare                                                                
280600     If WS-FLAGGA-LISTA (24) > Space                                      
280700*       -- Inköpare                                                       
280800        Move TEMPWS-IDINK     To W-IDINK                                  
280900        Move W-IDINK          To W002-ART-RAD (LAENGD:4)                  
281000        Add 4                 To LAENGD                                   
281100        Move TAB-TECKEN       To W002-ART-RAD(LAENGD:1)                   
281200        Add 1                 To LAENGD                                   
281300     End-If                                                               
281400                                                                          
281500* Pris                                                                    
281600     If WS-FLAGGA-LISTA (25) > Space                                      
281700        Move TEMPWS-PRARTBES  To W-PRARTBES                               
281800        Move W-PRARTBES       To W002-ART-RAD (LAENGD:10)                 
281900        Add 10                To LAENGD                                   
282000        Move TAB-TECKEN       To W002-ART-RAD(LAENGD:1)                   
282100        Add 1                 To LAENGD                                   
282200                                                                          
282300        Move TEMPWS-PRARTSTD  To W-PRARTSTD                               
282400        Move W-PRARTSTD       To W002-ART-RAD (LAENGD:10)                 
282500        Add 10                To LAENGD                                   
282600        Move TAB-TECKEN       To W002-ART-RAD(LAENGD:1)                   
282700        Add 1                 To LAENGD                                   
282800     End-If                                                               
282900                                                                          
283000* Best.pris                                                               
283100     If WS-FLAGGA-LISTA (26) > Space                                      
283200*       -- Best.rest                                                      
283300*       1. Best.Rest.Valid leverantör                                     
283400        Move TEMPWS-KVBR-VALID-LEV To W-KVBR-VALID-LEV                    
283500        Move W-KVBR-VALID-LEV    To W002-ART-RAD (LAENGD:7)               
283600        Add 7                    To LAENGD                                
283700        Move TAB-TECKEN          To W002-ART-RAD(LAENGD:1)                
283800        Add 1                    To  LAENGD                               
283900*       2. Best.Rest.Övriga leverantörer                                  
284000        Move TEMPWS-KVBR-OVR-LEV To W-KVBR-OVR-LEV                        
284100        Move W-KVBR-OVR-LEV      To W002-ART-RAD (LAENGD:7)               
284200        Add 7                    To LAENGD                                
284300        Move TAB-TECKEN          To W002-ART-RAD(LAENGD:1)                
284400        Add 1                    To LAENGD                                
284500     End-If                                                               
284600                                                                          
284700* Avtal                                                                   
284800     If WS-FLAGGA-LISTA (27) > Space                                      
284900        Move TEMPWS-KDAVT     To W-KDAVT                                  
285000        Move W-KDAVT          To W002-ART-RAD (LAENGD:1)                  
285100        Add 1                 To LAENGD                                   
285200        Move TAB-TECKEN       To W002-ART-RAD(LAENGD:1)                   
285300        Add 1                 To LAENGD                                   
285400     End-If                                                               
285500                                                                          
285600* Förp.flag                                                               
285700     If WS-FLAGGA-LISTA (28) > Space                                      
285800        MOVE SPACE                  To W-KDFPKPRI                         
285900        IF TEMPWS-IDLEVNR NOT = SPACE                                     
286000           MOVE TEMPWS-IDARTNR      To W-IDARTNR                          
286100           MOVE TEMPWS-IDLEVNR      To W-IDLEVNR                          
286200           PERFORM IMS-GET-WDK621                                         
286300           IF SEGMENT-FINNS                                               
286400              MOVE PRL-KDFPKPRI     To W-KDFPKPRI                         
286500           END-IF                                                         
286600        END-IF                                                            
286700        Move W-KDFPKPRI       To W002-ART-RAD (LAENGD:1)                  
286800        Add 1                 To LAENGD                                   
286900        Move TAB-TECKEN       To W002-ART-RAD(LAENGD:1)                   
287000        Add 1                 To LAENGD                                   
287100     End-If                                                               
287200                                                                          
287300* Vikt/Volym                                                              
287400     If WS-FLAGGA-LISTA (29) > Space                                      
287500*       -- Vikt                                                           
287600        Move TEMPWS-VKART     To W-VKART                                  
287700        Move W-VKART          To W002-ART-RAD (LAENGD:7)                  
287800        Add 7                 To LAENGD                                   
287900        Move TAB-TECKEN       To W002-ART-RAD(LAENGD:1)                   
288000        Add 1                 To LAENGD                                   
288100*       -- Volym                                                          
288200        Move TEMPWS-VLARTNTO  To W-VLARTNTO                               
288300        Move W-VLARTNTO       To W002-ART-RAD (LAENGD:10)                 
288400        Add 10                To LAENGD                                   
288500        Move TAB-TECKEN       To W002-ART-RAD(LAENGD:1)                   
288600        Add 1                 To LAENGD                                   
288700     End-If                                                               
288800                                                                          
288900* RefillSt.                                                               
289000     If WS-FLAGGA-LISTA (30) > Space                                      
289100*       -- Refillst.                                                      
289200        Move TEMPWS-TIREFSTO  To W-TIREFSTO                               
289300        If W-TIREFSTO < DAGENS-TIDATUM                                    
289400          Move Space To W-TIREFSTO-GRP                                    
289500        End-If                                                            
289600        Move W-TIREFSTO-GRP   To W002-ART-RAD (LAENGD:6)                  
289700        Add 6                 To LAENGD                                   
289800        Move TAB-TECKEN       To W002-ART-RAD(LAENGD:1)                   
289900        Add 1                 To LAENGD                                   
290000     End-If                                                               
290100                                                                          
290200* Proj/Mod.                                                               
290300     If WS-FLAGGA-LISTA (31) > Space                                      
290400*       -- Project                                                        
290500        Move TEMPWS-IDPROJ    To W-IDPROJ                                 
290600        Move W-IDPROJ         To W002-ART-RAD (LAENGD:4)                  
290700        Add 4                 To LAENGD                                   
290800        Move TAB-TECKEN       To W002-ART-RAD(LAENGD:1)                   
290900        Add 1                 To LAENGD                                   
291000*       -- Model                                                          
291100        Move Space            To W-IDKAT                                  
291200        Move TEMPWS-IDKAT(01) To W-IDKAT1                                 
291300        Move TEMPWS-IDKAT(02) To W-IDKAT2                                 
291400        Move TEMPWS-IDKAT(03) To W-IDKAT3                                 
291500        Move W-IDKAT          To W002-ART-RAD (LAENGD:17)                 
291600        Add 17                To LAENGD                                   
291700        Move TAB-TECKEN       To W002-ART-RAD(LAENGD:1)                   
291800        Add 1                 To LAENGD                                   
291900     End-If                                                               
292000                                                                          
292100* 1:a inlev                                                               
292200     If WS-FLAGGA-LISTA (32) > Space                                      
292300        Move TEMPWS-TIFINLV   To W-TIFINLV                                
292400        Move W-TIFINLV        To W002-ART-RAD (LAENGD:5)                  
292500        Add 5                 To LAENGD                                   
292600        Move TAB-TECKEN       To W002-ART-RAD(LAENGD:1)                   
292700        Add 1                 To LAENGD                                   
292800     End-If                                                               
292900                                                                          
293000* Utg.prod                                                                
293100     If WS-FLAGGA-LISTA (33) > Space                                      
293200        Move TEMPWS-TIURPROD  To W-TIURPROD                               
293300        Move W-TIURPROD       To W002-ART-RAD (LAENGD:5)                  
293400        Add 5                 To LAENGD                                   
293500        Move TAB-TECKEN       To W002-ART-RAD(LAENGD:1)                   
293600        Add 1                 To LAENGD                                   
293700     End-If                                                               
293800*                                                                         
293900* R.Pol                                                                   
294000     If WS-FLAGGA-LISTA (34) > Space                                      
294100        Move 15               To W-KVAARLF                                
294200        Move +1               To Tab-ix                                   
294300        Perform until Tab-ix > Max-Tab-ix                                 
294400          If TEMPWS-IDFKNGRP >= TAB-IDFKNGRP-FOM (Tab-ix)                 
294500         And TEMPWS-IDFKNGRP <= TAB-IDFKNGRP-TOM (Tab-ix)                 
294600             Move TAB-KVAARLF(Tab-ix)                                     
294700                              To W-KVAARLF                                
294800             Move +5000       To Tab-ix                                   
294900          End-If                                                          
295000          Add +1              To Tab-ix                                   
295100        End-Perform                                                       
295200        Move W-KVAARLF        To W002-ART-RAD (LAENGD:2)                  
295300        Add 2                 To LAENGD                                   
295400        Move TAB-TECKEN       To W002-ART-RAD(LAENGD:1)                   
295500        Add 1                 To LAENGD                                   
295600     End-If                                                               
295700                                                                          
295800* Prodsl                                                                  
295900     If WS-FLAGGA-LISTA (35) > Space                                      
296000        Move TEMPWS-KDPRODSL  To W-KDPRODSL                               
296100        Move W-KDPRODSL       To W002-ART-RAD (LAENGD:2)                  
296200        Add 2                 To LAENGD                                   
296300        Move TAB-TECKEN       To W002-ART-RAD(LAENGD:1)                   
296400        Add 1                 To LAENGD                                   
296500     End-If                                                               
296600                                                                          
296700* Funkgrp                                                                 
296800     If WS-FLAGGA-LISTA (36) > Space                                      
296900        Move TEMPWS-IDFKNGRP  To W-IDFKNGRP                               
297000        Move W-IDFKNGRP       To W002-ART-RAD (LAENGD:4)                  
297100        Add 4                 To LAENGD                                   
297200        Move TAB-TECKEN       To W002-ART-RAD(LAENGD:1)                   
297300        Add 1                 To LAENGD                                   
297400     End-If                                                               
297500                                                                          
297600* I sats                                                                  
297700     If WS-FLAGGA-LISTA (37) > Space                                      
297800*       -- I Sats                                                         
297900        Move TEMPWS-FLIART    To W-FLIART                                 
298000        Move W-FLIART         To W002-ART-RAD (LAENGD:1)                  
298100        Add 1                 To LAENGD                                   
298200        Move TAB-TECKEN       To W002-ART-RAD(LAENGD:1)                   
298300        Add 1                 To LAENGD                                   
298400     End-If                                                               
298500                                                                          
298600*    --- SEPARAT VALFÄLT "TOT.BEHOV" (utanför  "LISTA" )                  
298700     If WS10-KVVECKOR-KVPB  > Zero                                        
298800        Move TEMPWS-KVPB-SUM-VV To W-KVPB-SUM-VV                          
298900        Move W-KVPB-SUM-VV    To W002-ART-RAD (LAENGD:9)                  
299000        Add 9                 To LAENGD                                   
299100        Move TAB-TECKEN       To W002-ART-RAD(LAENGD:1)                   
299200        Add 1                 To LAENGD                                   
299300     End-If                                                               
299400                                                                          
299500*    --- SEPARAT VALFÄLT "K.AVROP" (utanför  "LISTA" )                    
299600     If WS10-KVVECKOR-AVROP  > Zeroes                                     
299700        Move TEMPWS-KVAVROP-SUM-VV To W-KVAVROP-SUM-VV                    
299800        Move W-KVAVROP-SUM-VV To W002-ART-RAD (LAENGD:8)                  
299900        Add 8                 To LAENGD                                   
300000        Move TAB-TECKEN       To W002-ART-RAD(LAENGD:1)                   
300100        Add 1                 To LAENGD                                   
300200     End-If                                                               
300300                                                                          
300400* Styrparam                                                               
300500     If WS-FLAGGA-LISTA (38) > Space                                      
300600*       -- Styrparam                                                      
300700*       1. "Vvkl".                                                        
300800        Move TEMPWS-KDVVKL    To W-KDVVKL                                 
300900        Move W-KDVVKL         To W002-ART-RAD (LAENGD:1)                  
301000        Add 1                 To LAENGD                                   
301100        Move TAB-TECKEN       To W002-ART-RAD(LAENGD:1)                   
301200        Add 1                 To LAENGD                                   
301300*       2. "MAD Sep"                                                      
301400        Move TEMPWS-KVMAD-SEP To W-KVMAD-SEP                              
301500        Move W-KVMAD-SEP      To W002-ART-RAD (LAENGD:8)                  
301600        Add 8                 To LAENGD                                   
301700        Move TAB-TECKEN       To W002-ART-RAD(LAENGD:1)                   
301800        Add 1                 To LAENGD                                   
301900*       3. "Prisklass                                                     
302000        Move TEMPWS-KDPRISKL  To W-KDPRISKL                               
302100        Move W-KDPRISKL       To W002-ART-RAD (LAENGD:1)                  
302200        Add 1                 To LAENGD                                   
302300        Move TAB-TECKEN       To W002-ART-RAD(LAENGD:1)                   
302400        Add 1                 To LAENGD                                   
302500*       4. "Frekvensklass"                                                
302600        Move TEMPWS-KDFREKKL  To W-KDFREKKL                               
302700        Move W-KDFREKKL       To W002-ART-RAD (LAENGD:1)                  
302800        Add 1                 To LAENGD                                   
302900        Move TAB-TECKEN       To W002-ART-RAD(LAENGD:1)                   
303000        Add 1                 To LAENGD                                   
303100     End-If                                                               
303200                                                                          
303300* Service                                                                 
303400     If WS-FLAGGA-LISTA (39) > Space                                      
303500*       -- Service                                                        
303600*       1. "Inkommande rader FV"                                          
303700        Move TEMPWS-KVINORD   To W-KVINORD                                
303800        Move W-KVINORD        To W002-ART-RAD (LAENGD:6)                  
303900        Add 6                 To LAENGD                                   
304000        Move TAB-TECKEN       To W002-ART-RAD(LAENGD:1)                   
304100        Add 1                 To LAENGD                                   
304200*       2. "Avbokade rader FV"                                            
304300        Move TEMPWS-KVAVBRAD  To W-KVAVBRAD                               
304400        Move W-KVAVBRAD       To W002-ART-RAD (LAENGD:10)                 
304500        Add 10                To LAENGD                                   
304600        Move TAB-TECKEN       To W002-ART-RAD(LAENGD:1)                   
304700        Add 1                 To LAENGD                                   
304800*       3. "ServiceGrad"                                                  
304900        Move TEMPWS-SERVG     To W-SERVG                                  
305000        Move W-SERVG          To W002-ART-RAD (LAENGD:10)                 
305100        Add 10                To LAENGD                                   
305200        Move TAB-TECKEN       To W002-ART-RAD(LAENGD:1)                   
305300        Add 1                 To LAENGD                                   
305400     End-If                                                               
305500                                                                          
305600* Sort                                                                    
305700     If WS-FLAGGA-LISTA (40) > Space                                      
305800*       -- Sort                                                           
305900        Move TEMPWS-KDSORT    To W-KDSORT                                 
306000        Move W-KDSORT         To W002-ART-RAD (LAENGD:2)                  
306100        Add 2                 To LAENGD                                   
306200        Move TAB-TECKEN       To W002-ART-RAD(LAENGD:1)                   
306300        Add 1                 To LAENGD                                   
306400     End-If                                                               
306500                                                                          
306600* Kvanter                                                                 
306700     If WS-FLAGGA-LISTA (41) > Space                                      
306800*       -- KVANTER 12 fält                                                
306900*       1--- EOQ , Q-opt                                                  
307000        Move TEMPWS-KVEOQ     To W-KVEOQ                                  
307100        Move W-KVEOQ          To W002-ART-RAD (LAENGD:7)                  
307200        Add 7                 To LAENGD                                   
307300        Move TAB-TECKEN       To W002-ART-RAD(LAENGD:1)                   
307400        Add 1                 To LAENGD                                   
307500*       2--- Min.Kvant                                                    
307600        Move TEMPWS-KVPALL    To W-KVPALL                                 
307700        Move W-KVPALL         To W002-ART-RAD (LAENGD:7)                  
307800        Add 7                 To LAENGD                                   
307900        Move TAB-TECKEN       To W002-ART-RAD(LAENGD:1)                   
308000        Add 1                 To LAENGD                                   
308100*       3--- Q-kvant                                                      
308200        Move TEMPWS-KVQ       To W-KVQ                                    
308300        Move W-KVQ            To W002-ART-RAD (LAENGD:7)                  
308400        Add 7                 To LAENGD                                   
308500        Move TAB-TECKEN       To W002-ART-RAD(LAENGD:1)                   
308600        Add 1                 To LAENGD                                   
308700*       4--- Q-spärr                                                      
308800        Move TEMPWS-FLMANQ    To W-FLMANQ                                 
308900        Move W-FLMANQ         To W002-ART-RAD (LAENGD:1)                  
309000        Add 1                 To LAENGD                                   
309100        Move TAB-TECKEN       To W002-ART-RAD(LAENGD:1)                   
309200        Add 1                 To LAENGD                                   
309300*       5 till 9 ---  Q0- Q4                                              
309400        Move TEMPWS-KVQPACK-0  To W-KVQPACK-0                             
309500        Move TEMPWS-KVQPACK-1  To W-KVQPACK-1                             
309600        Move TEMPWS-KVQPACK-2  To W-KVQPACK-2                             
309700        Move TEMPWS-KVQPACK-3  To W-KVQPACK-3                             
309800        Move TEMPWS-KVQPACK-4  To W-KVQPACK-4                             
309900        Move W-KVQPACK-0      To W002-ART-RAD (LAENGD:5)                  
310000        Add 5                 To LAENGD                                   
310100        Move TAB-TECKEN       To W002-ART-RAD(LAENGD:1)                   
310200        Add 1                 To LAENGD                                   
310300        Move W-KVQPACK-1      To W002-ART-RAD (LAENGD:5)                  
310400        Add 5                 To LAENGD                                   
310500        Move TAB-TECKEN       To W002-ART-RAD(LAENGD:1)                   
310600        Add 1                 To LAENGD                                   
310700        Move W-KVQPACK-2      To W002-ART-RAD (LAENGD:5)                  
310800        Add 5                 To LAENGD                                   
310900        Move TAB-TECKEN       To W002-ART-RAD(LAENGD:1)                   
311000        Add 1                 To LAENGD                                   
311100        Move W-KVQPACK-3      To W002-ART-RAD (LAENGD:5)                  
311200        Add 5                 To LAENGD                                   
311300        Move TAB-TECKEN       To W002-ART-RAD(LAENGD:1)                   
311400        Add 1                 To LAENGD                                   
311500        Move W-KVQPACK-4      To W002-ART-RAD (LAENGD:5)                  
311600        Add 5                 To LAENGD                                   
311700        Move TAB-TECKEN       To W002-ART-RAD(LAENGD:1)                   
311800        Add 1                 To LAENGD                                   
311900*       10-- Enhetslast                                                   
312000        Move TEMPWS-KVULOAD   To W-KVULOAD                                
312100        Move W-KVULOAD        To W002-ART-RAD (LAENGD:7)                  
312200        Add 7                 To LAENGD                                   
312300        Move TAB-TECKEN       To W002-ART-RAD(LAENGD:1)                   
312400        Add 1                 To LAENGD                                   
312500*       11-- Kval.Spärr CDC (6308)                                        
312600        Move TEMPWS-KVSPARR-KVAL-CDC To W-KVSPARR-KVAL                    
312700        Move W-KVSPARR-KVAL   To W002-ART-RAD (LAENGD:7)                  
312800        Add 7                 To LAENGD                                   
312900        Move TAB-TECKEN       To W002-ART-RAD(LAENGD:1)                   
313000        Add 1                 To LAENGD                                   
313100*       12-- Nyberäkning                                                  
313200        Move TEMPWS-FLNYBER   To W-FLNYBER                                
313300        Move W-FLNYBER        To W002-ART-RAD (LAENGD:1)                  
313400        Add 1                 To LAENGD                                   
313500        Move TAB-TECKEN       To W002-ART-RAD(LAENGD:1)                   
313600        Add 1                 To LAENGD                                   
313700     End-If                                                               
313800                                                                          
313900* Förp.Info                                                               
314000     If WS-FLAGGA-LISTA (42) > Space                                      
314100*       -- Förp.Info 9 fält                                               
314200*       1 till 5 -- EMBQ                                                  
314300        Move TEMPWS-IDARTNR-EMBQ0 To W-IDARTNR-EMBQ0                      
314400        Move W-IDARTNR-EMBQ0  To W002-ART-RAD (LAENGD:8)                  
314500        Add 8                 To LAENGD                                   
314600        Move TAB-TECKEN       To W002-ART-RAD(LAENGD:1)                   
314700        Add 1                 To LAENGD                                   
314800        Move TEMPWS-IDARTNR-EMBQ1 To W-IDARTNR-EMBQ1                      
314900        Move W-IDARTNR-EMBQ1  To W002-ART-RAD (LAENGD:8)                  
315000        Add 8                 To LAENGD                                   
315100        Move TAB-TECKEN       To W002-ART-RAD(LAENGD:1)                   
315200        Add 1                 To LAENGD                                   
315300        Move TEMPWS-IDARTNR-EMBQ2 To W-IDARTNR-EMBQ2                      
315400        Move W-IDARTNR-EMBQ2  To W002-ART-RAD (LAENGD:8)                  
315500        Add 8                 To LAENGD                                   
315600        Move TAB-TECKEN       To W002-ART-RAD(LAENGD:1)                   
315700        Add 1                 To LAENGD                                   
315800        Move TEMPWS-IDARTNR-EMBQ3 To W-IDARTNR-EMBQ3                      
315900        Move W-IDARTNR-EMBQ3  To W002-ART-RAD (LAENGD:8)                  
316000        Add 8                 To LAENGD                                   
316100        Move TAB-TECKEN       To W002-ART-RAD(LAENGD:1)                   
316200        Add 1                 To LAENGD                                   
316300        Move TEMPWS-IDARTNR-EMBQ4 To W-IDARTNR-EMBQ4                      
316400        Move W-IDARTNR-EMBQ4  To W002-ART-RAD (LAENGD:8)                  
316500        Add 8                 To LAENGD                                   
316600        Move TAB-TECKEN       To W002-ART-RAD(LAENGD:1)                   
316700        Add 1                 To LAENGD                                   
316800                                                                          
316900*       6--- Pack.code                                                    
317000        Move TEMPWS-KDFORP    To W-KDFORP                                 
317100        Move W-KDFORP         To W002-ART-RAD (LAENGD:5)                  
317200        Add 5                 To LAENGD                                   
317300        Move TAB-TECKEN       To W002-ART-RAD(LAENGD:1)                   
317400        Add 1                 To LAENGD                                   
317500                                                                          
317600*       7--- Pack.type                                                    
317700        Move TEMPWS-BEFT      To W-BEFT                                   
317800        Move W-BEFT           To W002-ART-RAD (LAENGD:3)                  
317900        Add 3                 To LAENGD                                   
318000        Move TAB-TECKEN       To W002-ART-RAD(LAENGD:1)                   
318100        Add 1                 To LAENGD                                   
318200*       8--- Direct wages                                                 
318300        Move TEMPWS-PRDIRLON  To W-PRDIRLON                               
318400        Move W-PRDIRLON       To W002-ART-RAD (LAENGD:7)                  
318500        Add 7                 To LAENGD                                   
318600        Move TAB-TECKEN       To W002-ART-RAD(LAENGD:1)                   
318700        Add 1                 To LAENGD                                   
318800*       9--- Surcharge Pack Material                                      
318900        Move TEMPWS-PRDMTRL   To W-PRDMTRL                                
319000        Move W-PRDMTRL        To W002-ART-RAD (LAENGD:10)                 
319100        Add 10                To LAENGD                                   
319200        Move TAB-TECKEN       To W002-ART-RAD(LAENGD:1)                   
319300        Add 1                 To LAENGD                                   
319400     End-If                                                               
319500                                                                          
319600* Spärrade                                                                
319700     If WS-FLAGGA-LISTA (43) > Space                                      
319800*       -- Blocked Qty.                                                   
319900        Move TEMPWS-KVSPANT   To W-KVSPANT                                
320000        Move W-KVSPANT        To W002-ART-RAD (LAENGD:6)                  
320100        Add 6                 To LAENGD                                   
320200        Move TAB-TECKEN       To W002-ART-RAD(LAENGD:1)                   
320300        Add 1                 To LAENGD                                   
320400                                                                          
320500        If WS-FLAGGA-LISTA (43) = 'S'                                     
320600           If TEMPWS-KVSPANT = Zero                                       
320700              If W-WRITE-PART-ON-EXCEL Not = JA                           
320800                 Move NEJ     To W-WRITE-PART-ON-EXCEL                    
320900              End-if                                                      
321000           Else                                                           
321100              Move JA         To W-WRITE-PART-ON-EXCEL                    
321200           End-if                                                         
321300        End-if                                                            
321400     End-If                                                               
321500                                                                          
321600* S-lager                                                                 
321700     If WS-FLAGGA-LISTA (44) > Space                                      
321800*       -- S-lager                                                        
321900*       1. - Säk.Lager Antal                                              
322000        Move TEMPWS-KVSLAGER  To W-KVSLAGER                               
322100        Move W-KVSLAGER       To W002-ART-RAD (LAENGD:7)                  
322200        Add 7                 To LAENGD                                   
322300        Move TAB-TECKEN       To W002-ART-RAD(LAENGD:1)                   
322400        Add 1                 To LAENGD                                   
322500*       2. - Säk.Lager Justeringsfaktor                                   
322600        Move TEMPWS-RESLJUST  To W-RESLJUST                               
322700        Move W-RESLJUST       To W002-ART-RAD (LAENGD:4)                  
322800        Add 4                 To LAENGD                                   
322900        Move TAB-TECKEN       To W002-ART-RAD(LAENGD:1)                   
323000        Add 1                 To LAENGD                                   
323100*       3. - S.L. Justeringsfaktor till datum                             
323200        Move TEMPWS-TISLJUST  To W-TISLJUST                               
323300        Move W-TISLJUST       To W002-ART-RAD (LAENGD:5)                  
323400        Add 5                 To LAENGD                                   
323500        Move TAB-TECKEN       To W002-ART-RAD(LAENGD:1)                   
323600        Add 1                 To LAENGD                                   
323700     End-if                                                               
323800                                                                          
323900* MaxPunkt                                                                
324000     If WS-FLAGGA-LISTA (45) > Space                                      
324100*       -- MaxPunkt                                                       
324200        Move TEMPWS-KVMP      To W-KVMP                                   
324300        Move W-KVMP           To W002-ART-RAD (LAENGD:7)                  
324400        Add 7                 To LAENGD                                   
324500        Move TAB-TECKEN       To W002-ART-RAD(LAENGD:1)                   
324600        Add 1                 To LAENGD                                   
324700*       -- MaxLS                                                          
324800        MOVE TEMPWS-IDARTNR   To BYART-IDARTNR-BYT                        
324900        PERFORM DB2-SELECT-BYART                                          
325000        IF ROW-FOUND                                                      
325100           Move  BYART-KVLS-MAXCORE To W-KVLS-MAXCORE                     
325200        ELSE                                                              
325300           Move  ZERO         To W-KVLS-MAXCORE                           
325400        END-IF                                                            
325500        Move W-KVLS-MAXCORE   To W002-ART-RAD (LAENGD:7)                  
325600        Add 7                 To LAENGD                                   
325700        Move TAB-TECKEN       To W002-ART-RAD(LAENGD:1)                   
325800        Add 1                 To LAENGD                                   
325900     End-If                                                               
326000                                                                          
326100* KR-status                                                               
326200     If WS-FLAGGA-LISTA (46) > Space                                      
326300*       * KR * (TVÅ KOLUMNER)                                             
326400        Move TEMPWS-KDKRSTA   To W-KDKRSTA                                
326500        Move W-KDKRSTA        To W002-ART-RAD (LAENGD:1)                  
326600        Add 1                 To LAENGD                                   
326700        Move TAB-TECKEN       To W002-ART-RAD(LAENGD:1)                   
326800        Add 1                 To LAENGD                                   
326900        Move TEMPWS-IDKR      To W-IDKR                                   
327000        Move W-IDKR           To W002-ART-RAD (LAENGD:5)                  
327100        Add 5                 To LAENGD                                   
327200        Move TAB-TECKEN       To W002-ART-RAD(LAENGD:1)                   
327300        Add 1                 To LAENGD                                   
327400     End-If                                                               
327500                                                                          
327600* Kampanj                                                                 
327700     If WS-FLAGGA-LISTA (47) > Space                                      
327800*       -- KAMPANJ 5 fält                                                 
327900*       1--- Kampanj-ID                                                   
328000        Move TEMPWS-IDKAMP    To W-IDKAMP                                 
328100        Move W-IDKAMP         To W002-ART-RAD (LAENGD:7)                  
328200        Add 7                 To LAENGD                                   
328300        Move TAB-TECKEN       To W002-ART-RAD(LAENGD:1)                   
328400        Add 1                 To LAENGD                                   
328500*       2--- Kampanj-Grupp                                                
328600        Move TEMPWS-IDKAMP-GRP To W-IDKAMP-GRP                            
328700        Move W-IDKAMP-GRP      To W002-ART-RAD (LAENGD:7)                 
328800        Add 7                  To LAENGD                                  
328900        Move TAB-TECKEN        To W002-ART-RAD(LAENGD:1)                  
329000        Add 1                  To LAENGD                                  
329100*       3--- Kampanj-Startdatum                                           
329200        Move TEMPWS-TISTADAT-KAMP To W-TISTADAT-KAMP                      
329300        Move W-TISTADAT-KAMP  To W002-ART-RAD (LAENGD:6)                  
329400        Add 6                 To LAENGD                                   
329500        Move TAB-TECKEN       To W002-ART-RAD(LAENGD:1)                   
329600        Add 1                 To LAENGD                                   
329700*       4--- Kampanj-Stoppdatum                                           
329800        Move TEMPWS-TISTODAT-KAMP To W-TISTODAT-KAMP                      
329900        Move W-TISTODAT-KAMP  To W002-ART-RAD (LAENGD:6)                  
330000        Add 6                 To LAENGD                                   
330100        Move TAB-TECKEN       To W002-ART-RAD(LAENGD:1)                   
330200        Add 1                 To LAENGD                                   
330300*       5--- Kampanj-Kod                                                  
330400        Move TEMPWS-KDKAMP    To W-KDKAMP                                 
330500        Move W-KDKAMP         To W002-ART-RAD (LAENGD:1)                  
330600        Add 1                 To LAENGD                                   
330700        Move TAB-TECKEN       To W002-ART-RAD(LAENGD:1)                   
330800        Add 1                 To LAENGD                                   
330900                                                                          
331000        If WS-FLAGGA-LISTA (47) = 'S' or 'X'                              
331100           If TEMPWS-idkamp = Space                                       
331200              If W-WRITE-PART-ON-EXCEL Not = JA                           
331300                 Move NEJ     To W-WRITE-PART-ON-EXCEL                    
331400              End-if                                                      
331500           Else                                                           
331600              Move JA         To W-WRITE-PART-ON-EXCEL                    
331700           End-if                                                         
331800        End-if                                                            
331900     End-If                                                               
332000                                                                          
332100* Lag.omr                                                                 
332200     If WS-FLAGGA-LISTA (48) > Space                                      
332300*       --LAG.OMR.                                                        
332400        Move TEMPWS-ADLAGOMR  To W-ADLAGOMR                               
332500        Move TEMPWS-ADGANG    To W-ADGANG                                 
332600        Move TEMPWS-ADPLATS   To W-ADPLATS                                
332700        Move W-ADART-RED      To W002-ART-RAD (LAENGD:11)                 
332800        Add 11                To LAENGD                                   
332900        Move TAB-TECKEN       To W002-ART-RAD(LAENGD:1)                   
333000        Add 1                 To LAENGD                                   
333100     End-If                                                               
333200                                                                          
333300* Gate                                                                    
333400     If WS-FLAGGA-LISTA (49) > Space                                      
333500*       -- Gate                                                           
333600        Move TEMPWS-ADINPORT  To W-ADINPORT                               
333700        Move W-ADINPORT       To W002-ART-RAD (LAENGD:8)                  
333800        Add 8                 To LAENGD                                   
333900        Move TAB-TECKEN       To W002-ART-RAD(LAENGD:1)                   
334000        Add 1                 To LAENGD                                   
334100     End-If                                                               
334200                                                                          
334300* Uart/LSR                                                                
334400     If WS-FLAGGA-LISTA (50) > Space                                      
334500*       -- Uart                                                           
334600        Move TEMPWS-KDUART    To W002-ART-RAD (LAENGD:1)                  
334700        Add 1                 To LAENGD                                   
334800        Move TAB-TECKEN       To W002-ART-RAD(LAENGD:1)                   
334900        Add 1                 To LAENGD                                   
335000*       -- LSR                                                            
335100        Move TEMPWS-FLLSRDEL  To W002-ART-RAD (LAENGD:1)                  
335200        Add 1                 To LAENGD                                   
335300        Move TAB-TECKEN       To W002-ART-RAD(LAENGD:1)                   
335400        Add 1                 To LAENGD                                   
335500     End-If                                                               
335600                                                                          
335700* Ursprung                                                                
335800     If WS-FLAGGA-LISTA (51) > Space                                      
335900        Move TEMPWS-KDARTURS  To W002-ART-RAD (LAENGD:2)                  
336000        Add 2                 To LAENGD                                   
336100        Move TAB-TECKEN       To W002-ART-RAD(LAENGD:1)                   
336200        Add 1                 To LAENGD                                   
336300     End-If                                                               
336400                                                                          
336500* KDOTFREK                                                                
336600     If WS-FLAGGA-LISTA (52) > Space                                      
336700        Move TEMPWS-KDOTFREK  To W002-ART-RAD (LAENGD:2)                  
336800        Add 1                 To LAENGD                                   
336900        Move TAB-TECKEN       To W002-ART-RAD(LAENGD:1)                   
337000        Add 1                 To LAENGD                                   
337100     End-If                                                               
337200     .                                                                    
337300     EJECT                                                                
337400                                                                          
337500 Z-FINIT SECTION.                                                         
337600     MOVE 'Z-FINIT                     ' TO CURRENT-SECTION               
337700                                                                          
337800     CLOSE W21710                                                         
337900           W21709                                                         
338000           W21710-001                                                     
338100                                                                          
338200     Move 'T'         To POSTSUM-OPKOD                                    
338300     Move 'W21709'    To POSTSUM-FDNAMN                                   
338400     Move 'W21710D2'  To POSTSUM-DDNAMN2                                  
338500     Move 'IN'        To POSTSUM-TRANSTYP                                 
338600     Move ANT-LB-POST To POSTSUM-TOTTRANS                                 
338700     Call POSTSUM Using POSTSUM-PARM                                      
338800                                                                          
338900     Move 'S' To POSTSUM-OPKOD                                            
339000     Call POSTSUM Using POSTSUM-PARM                                      
339100     .                                                                    
339200     EJECT                                                                
339300 S01-LAES-W21710  SECTION.                                                
339400     MOVE 'S01-LAES-W21710             ' TO CURRENT-SECTION               
339500                                                                          
339600     Read W21710 Into IN10-AREA                                           
339700     At End                                                               
339800        Set END-OF-W21710 To True                                         
339900                                                                          
340000     Not At End                                                           
340100        Move 'W21710' To POSTSUM-FDNAMN                                   
340200        Move 'W21710D1' To POSTSUM-DDNAMN2                                
340300        Move 'PARM'     To POSTSUM-TRANSTYP                               
340400        Call POSTSUM Using POSTSUM-PARM                                   
340500     End-READ                                                             
340600     .                                                                    
340700     EJECT                                                                
340800 S02-LAES-W21709  SECTION.                                                
340900     MOVE 'S02-LAES-W21709             ' TO CURRENT-SECTION               
341000                                                                          
341100     READ W21709 INTO IN09-AREA                                           
341200     AT END                                                               
341300        SET END-OF-W21709     TO TRUE                                     
341400        MOVE 999              TO TEMPWS-IDANSK                            
341500        MOVE '99999'          TO TEMPWS-IDLEVNR                           
341600                                                                          
341700     NOT AT END                                                           
341800        ADD +1                TO ANT-LB-POST                              
341900     END-READ                                                             
342000     .                                                                    
342100     EJECT                                                                
342200 S21-SKRIV-W21710-001  SECTION.                                           
342300     MOVE 'S21-SKRIV-W21710-001        ' TO CURRENT-SECTION               
342400                                                                          
342500     If LAENGD > LAENGD-MAX                                               
342600        Move LAENGD-MAX To LAENGD-FD                                      
342700     ELSE                                                                 
342800        Move LAENGD     To LAENGD-FD                                      
342900     End-If                                                               
343000                                                                          
343100     If W-WRITE-PART-ON-EXCEL = JA or Space                               
343200        Write W21710-001-RAD From W002-DETALJ(1:LAENGD)                   
343300                             After W002-SKIP                              
343400        Move 'LISTA '   To POSTSUM-FDNAMN                                 
343500        Move 'W21710D3' To POSTSUM-DDNAMN2                                
343600        Move 'RAD'      To POSTSUM-TRANSTYP                               
343700        Call POSTSUM Using POSTSUM-PARM                                   
343800     End-if                                                               
343900                                                                          
344000     Move Space To W002-DETALJ                                            
344100     Move 1     To W002-SKIP                                              
344200                                                                          
344300     .                                                                    
344400     EJECT                                                                
344500 S22-DATUMKONV-TILL-AAVVD  SECTION.                                       
344600     MOVE 'S22-DATUMKONV-TILL-AAVVD    ' TO CURRENT-SECTION               
344700                                                                          
344800     MOVE 'AAMMDD'          TO DAT-KDDATFORM                              
344900     CALL WDATKONV USING DAT-KDDATFORM,                                   
345000                         DAT-I-TIDATUM,                                   
345100                         DAT-O-TIDATUM,                                   
345200                         DAT-KDSVAR                                       
345300     .                                                                    
345400     EJECT                                                                
345500                                                                          
345600 S2-LAES-OCH-BEH-EXTINFO SECTION.                                         
345700     MOVE 'S2-LAES-OCH-BEH-EXTINFO     ' TO CURRENT-SECTION               
345800                                                                          
345900     PERFORM IMS-GU-WDD902                                                
346000     IF SEGMENT-FINNS                                                     
346100        MOVE +2 TO W-IDLEVBSK                                             
346200        PERFORM IMS-GNP-WDD925-KVAL                                       
346300        IF SEGMENT-FINNS                                                  
346400           MOVE DAGENS-TIDATUM TO TMP1-YYMMDD                             
346500           MOVE INFO-TIBORT    TO TMP2-YYMMDD                             
346600           PERFORM WY2000P1                                               
346700           IF TMP1-YYMMDD > TMP2-YYMMDD                                   
346800              continue                                                    
346900           ELSE                                                           
347000              MOVE INFO-TIBORT   TO DAT-I-TIDATUM                         
347100              PERFORM S22-DATUMKONV-TILL-AAVVD                            
347200              IF DAT-KDSVAR-OK                                            
347300                 MOVE DAT-TIAAVVD TO TEMPWS-TIBORT-INFO                   
347400              END-IF                                                      
347500           END-IF                                                         
347600        END-IF                                                            
347700     END-IF                                                               
347800     .                                                                    
347900     EJECT                                                                
348000                                                                          
348100 S4-LAES-OCH-BEH-EXTINFO2 SECTION.                                        
348200     MOVE 'S4-LAES-OCH-BEH-EXTINFO2    ' TO CURRENT-SECTION               
348300                                                                          
348400     PERFORM IMS-GU-WDD902                                                
348500     IF SEGMENT-FINNS                                                     
348600        MOVE +4 TO W-IDLEVBSK                                             
348700        PERFORM IMS-GNP-WDD925-KVAL                                       
348800        IF SEGMENT-FINNS                                                  
348900           MOVE DAGENS-TIDATUM TO TMP1-YYMMDD                             
349000           MOVE INFO-TIBORT    TO TMP2-YYMMDD                             
349100           PERFORM WY2000P1                                               
349200           IF TMP1-YYMMDD > TMP2-YYMMDD                                   
349300              continue                                                    
349400           ELSE                                                           
349500              MOVE INFO-TIBORT   TO DAT-I-TIDATUM                         
349600              PERFORM S22-DATUMKONV-TILL-AAVVD                            
349700              IF DAT-KDSVAR-OK                                            
349800                 IF TEMPWS-TIBORT-INFO NUMERIC                            
349900                 AND TEMPWS-TIBORT-INFO > ZERO                            
350000                    MOVE TEMPWS-TIBORT-INFO TO TMP1-YYWWD                 
350100                    MOVE DAT-TIAAVVD        TO TMP2-YYWWD                 
350200                    PERFORM WY2000P2                                      
350300                    IF TMP1-YYWWD > TMP2-YYWWD                            
350400                       MOVE DAT-TIAAVVD TO TEMPWS-TIBORT-INFO             
350500                    END-IF                                                
350600                 ELSE                                                     
350700                    MOVE DAT-TIAAVVD TO TEMPWS-TIBORT-INFO                
350800                 END-IF                                                   
350900              END-IF                                                      
351000           END-IF                                                         
351100        END-IF                                                            
351200     END-IF                                                               
351300     .                                                                    
351400     EJECT                                                                
351500                                                                          
351600 S5-LAES-OCH-BEH-EXTINFO3 SECTION.                                        
351700     MOVE 'S5-LAES-OCH-BEH-EXTINFO3    ' TO CURRENT-SECTION               
351800                                                                          
351900     PERFORM IMS-GU-WDD902                                                
352000     IF SEGMENT-FINNS                                                     
352100        MOVE +5 TO W-IDLEVBSK                                             
352200        PERFORM IMS-GNP-WDD925-KVAL                                       
352300        IF SEGMENT-FINNS                                                  
352400           MOVE DAGENS-TIDATUM  TO TMP1-YYMMDD                            
352500           MOVE INFO-TIBORT   TO TMP2-YYMMDD                              
352600           PERFORM WY2000P1                                               
352700           IF TMP1-YYMMDD > TMP2-YYMMDD                                   
352800              CONTINUE                                                    
352900           ELSE                                                           
353000              MOVE INFO-TIBORT   TO DAT-I-TIDATUM                         
353100              PERFORM S22-DATUMKONV-TILL-AAVVD                            
353200              IF DAT-KDSVAR-OK                                            
353300                 IF TEMPWS-TIBORT-INFO NUMERIC                            
353400                 AND TEMPWS-TIBORT-INFO > ZERO                            
353500                    MOVE TEMPWS-TIBORT-INFO TO TMP1-YYWWD                 
353600                    MOVE DAT-TIAAVVD        TO TMP2-YYWWD                 
353700                    PERFORM WY2000P2                                      
353800                    IF TMP1-YYWWD > TMP2-YYWWD                            
353900                       MOVE DAT-TIAAVVD TO TEMPWS-TIBORT-INFO             
354000                    END-IF                                                
354100                 ELSE                                                     
354200                    MOVE DAT-TIAAVVD TO TEMPWS-TIBORT-INFO                
354300                 END-IF                                                   
354400              END-IF                                                      
354500           END-IF                                                         
354600        END-IF                                                            
354700     END-IF                                                               
354800     .                                                                    
354900     EJECT                                                                
355000                                                                          
355100 S6-LAES-OCH-BEH-EXTINFO4 SECTION.                                        
355200     MOVE 'S6-LAES-OCH-BEH-EXTINFO4    ' TO CURRENT-SECTION               
355300                                                                          
355400     PERFORM IMS-GU-WDD902                                                
355500     IF SEGMENT-FINNS                                                     
355600        MOVE +6 TO W-IDLEVBSK                                             
355700        PERFORM IMS-GNP-WDD925-KVAL                                       
355800        IF SEGMENT-FINNS                                                  
355900           MOVE DAGENS-TIDATUM TO TMP1-YYMMDD                             
356000           MOVE INFO-TIBORT    TO TMP2-YYMMDD                             
356100           PERFORM WY2000P1                                               
356200           IF TMP1-YYMMDD > TMP2-YYMMDD                                   
356300              CONTINUE                                                    
356400           ELSE                                                           
356500              MOVE INFO-TIBORT   TO DAT-I-TIDATUM                         
356600              PERFORM S22-DATUMKONV-TILL-AAVVD                            
356700              IF DAT-KDSVAR-OK                                            
356800                 IF TEMPWS-TIBORT-INFO NUMERIC                            
356900                 AND TEMPWS-TIBORT-INFO > ZERO                            
357000                    MOVE TEMPWS-TIBORT-INFO TO TMP1-YYWWD                 
357100                    MOVE DAT-TIAAVVD        TO TMP2-YYWWD                 
357200                    PERFORM WY2000P2                                      
357300                    IF TMP1-YYWWD > TMP2-YYWWD                            
357400                       MOVE DAT-TIAAVVD TO TEMPWS-TIBORT-INFO             
357500                    END-IF                                                
357600                 ELSE                                                     
357700                    MOVE DAT-TIAAVVD TO TEMPWS-TIBORT-INFO                
357800                 END-IF                                                   
357900              END-IF                                                      
358000           END-IF                                                         
358100        END-IF                                                            
358200     END-IF                                                               
358300     .                                                                    
358400     EJECT                                                                
358500                                                                          
358600 S7-SKAPA-EXCEL-RUBRIK SECTION.                                           
358700     MOVE 'DD-SKAPA-EXCEL-RUBRIK       ' TO CURRENT-SECTION               
358800                                                                          
358900     Move 1 To LAENGD                                                     
359000     Move 'PART NO  '                  To W002-ART-RAD(LAENGD:9)          
359100     Add  9                            To LAENGD                          
359200     Move TAB-TECKEN                   To W002-ART-RAD(LAENGD:1)          
359300     Add  1                            To LAENGD                          
359400* Lev                                                                     
359500     If WS-FLAGGA-LISTA (1) > Space                                       
359600       Move 'SUPPL'                    To W002-ART-RAD(LAENGD:5)          
359700       Add  5                          To LAENGD                          
359800       Move TAB-TECKEN                 To W002-ART-RAD(LAENGD:1)          
359900       Add  1                          To LAENGD                          
360000                                                                          
360100       Move 'SHP  '                    To W002-ART-RAD(LAENGD:5)          
360200       Add  5                          To LAENGD                          
360300       Move TAB-TECKEN                 To W002-ART-RAD(LAENGD:1)          
360400       Add  1                          To LAENGD                          
360500     End-If                                                               
360600* Lev.bet                                                                 
360700     If WS-FLAGGA-LISTA (2) > Space                                       
360800      Move 'SUPPL PART NO            ' To W002-ART-RAD(LAENGD:25)         
360900       Add  25                         To LAENGD                          
361000       Move TAB-TECKEN                 To W002-ART-RAD(LAENGD:1)          
361100       Add  1                          To LAENGD                          
361200     End-If                                                               
361300* Ben.(S)                                                                 
361400     IF WS-FLAGGA-LISTA (3) > SPACE                                       
361500      MOVE 'DESCRIPTION.S            ' TO W002-ART-RAD(LAENGD:25)         
361600      ADD  25                          TO LAENGD                          
361700       MOVE TAB-TECKEN                 TO W002-ART-RAD(LAENGD:1)          
361800       ADD 1                           TO LAENGD                          
361900     END-IF                                                               
362000* Ben.(GB)                                                                
362100     IF WS-FLAGGA-LISTA (4) > SPACE                                       
362200      MOVE 'DESCRIPTION.GB           ' TO W002-ART-RAD(LAENGD:25)         
362300      ADD  25                          TO LAENGD                          
362400       MOVE TAB-TECKEN                 TO W002-ART-RAD(LAENGD:1)          
362500       ADD 1                           TO LAENGD                          
362600     END-IF                                                               
362700* St.on hnd                                                               
362800     IF WS-FLAGGA-LISTA (5) > SPACE                                       
362900       MOVE 'STOCK CDC'                TO W002-ART-RAD(LAENGD:9)          
363000       ADD  9                          TO LAENGD                          
363100       MOVE TAB-TECKEN                 TO W002-ART-RAD(LAENGD:1)          
363200       ADD  1                          TO LAENGD                          
363300       MOVE 'ST.SDC/LDC'               TO W002-ART-RAD(LAENGD:10)         
363400       ADD  10                         TO LAENGD                          
363500       MOVE TAB-TECKEN                 TO W002-ART-RAD(LAENGD:1)          
363600       ADD  1                          TO LAENGD                          
363700       MOVE 'STOCK NDC'                TO W002-ART-RAD(LAENGD:9)          
363800       ADD  9                          TO LAENGD                          
363900       MOVE TAB-TECKEN                 TO W002-ART-RAD(LAENGD:1)          
364000       ADD  1                          TO LAENGD                          
364100     END-IF                                                               
364200* AK saldo                                                                
364300     IF WS-FLAGGA-LISTA (6) > SPACE                                       
364400       MOVE 'AK     '                  TO W002-ART-RAD(LAENGD:7)          
364500       ADD  7                          TO LAENGD                          
364600       MOVE TAB-TECKEN                 TO W002-ART-RAD(LAENGD:1)          
364700       ADD  1                          TO LAENGD                          
364800     END-IF                                                               
364900* VOR/RO                                                                  
365000     IF WS-FLAGGA-LISTA (7) > SPACE                                       
365100       Move 'BO Qty '                  To W002-ART-RAD(LAENGD:7)          
365200       ADD  7                          TO LAENGD                          
365300       MOVE TAB-TECKEN                 TO W002-ART-RAD(LAENGD:1)          
365400       ADD  1                          TO LAENGD                          
365500       Move 'BO Lines'                 To W002-ART-RAD(LAENGD:8)          
365600       Add  8                          To LAENGD                          
365700       Move TAB-TECKEN                 To W002-ART-RAD(LAENGD:1)          
365800       Add  1                          To LAENGD                          
365900       Move 'VOR Qty'                  To W002-ART-RAD(LAENGD:7)          
366000       Add  7                          To LAENGD                          
366100       Move TAB-TECKEN                 To W002-ART-RAD(LAENGD:1)          
366200       Add  1                          To LAENGD                          
366300     End-If                                                               
366400* Släp                                                                    
366500     If WS-FLAGGA-LISTA (8) > Space                                       
366600       Move 'ARREARS'                  To W002-ART-RAD(LAENGD:7)          
366700       Add  7                          To LAENGD                          
366800       Move TAB-TECKEN                 To W002-ART-RAD(LAENGD:1)          
366900       Add  1                          To LAENGD                          
367000       Move 'NOT RECEIVED'             To W002-ART-RAD(LAENGD:12)         
367100       Add  12                         To LAENGD                          
367200       Move TAB-TECKEN                 To W002-ART-RAD(LAENGD:1)          
367300       Add  1                          To LAENGD                          
367400     End-If                                                               
367500* Lev.Besk                                                                
367600     If WS-FLAGGA-LISTA (9) > Space                                       
367700       Move 'DISPATCH Week'            To W002-ART-RAD(LAENGD:13)         
367800       Add  13                         To LAENGD                          
367900       Move TAB-TECKEN                 To W002-ART-RAD(LAENGD:1)          
368000       Add  1                          To LAENGD                          
368100       Move 'DISPATCH Qty'             To W002-ART-RAD(LAENGD:12)         
368200       Add  12                         To LAENGD                          
368300       Move TAB-TECKEN                 To W002-ART-RAD(LAENGD:1)          
368400       Add  1                          To LAENGD                          
368500       Move 'CD Plan.Inc.Wk'           To W002-ART-RAD(LAENGD:14)         
368600       Add  14                         To LAENGD                          
368700       Move TAB-TECKEN                 To W002-ART-RAD(LAENGD:1)          
368800       Add  1                          To LAENGD                          
368900       Move 'AVAIL Date'               To W002-ART-RAD(LAENGD:10)         
369000       Add  10                         To LAENGD                          
369100       Move TAB-TECKEN                 To W002-ART-RAD(LAENGD:1)          
369200       Add  1                          To LAENGD                          
369300       Move 'X-info Date'              To W002-ART-RAD(LAENGD:11)         
369400       Add  11                         To LAENGD                          
369500       Move TAB-TECKEN                 To W002-ART-RAD(LAENGD:1)          
369600       Add  1                          To LAENGD                          
369700     End-If                                                               
369800* FörAvis.                                                                
369900     If WS-FLAGGA-LISTA (10) > Space                                      
370000       Move 'ADVICED QTY.'             To W002-ART-RAD(LAENGD:12)         
370100       Add  12                         To LAENGD                          
370200       Move TAB-TECKEN                 To W002-ART-RAD(LAENGD:1)          
370300       Add  1                          To LAENGD                          
370400     End-If                                                               
370500* TPO                                                                     
370600     If WS-FLAGGA-LISTA (11) > Space                                      
370700       Move 'TPO date'                 To W002-ART-RAD(LAENGD:8)          
370800       Add  8                          To LAENGD                          
370900       Move TAB-TECKEN                 To W002-ART-RAD(LAENGD:1)          
371000       Add  1                          To LAENGD                          
371100                                                                          
371200       Move 'TPO Qty'                  To W002-ART-RAD(LAENGD:7)          
371300       Add  7                          To LAENGD                          
371400       Move TAB-TECKEN                 To W002-ART-RAD(LAENGD:1)          
371500       Add  1                          To LAENGD                          
371600     End-If                                                               
371700* Ledtid                                                                  
371800     If WS-FLAGGA-LISTA (12) > Space                                      
371900       Move 'LEAD TIME'                To W002-ART-RAD(LAENGD:9)          
372000       Add  9                          To LAENGD                          
372100       Move TAB-TECKEN                 To W002-ART-RAD(LAENGD:1)          
372200       Add 1                           To LAENGD                          
372300     End-If                                                               
372400* Erskod                                                                  
372500     If WS-FLAGGA-LISTA (13) > Space                                      
372600       Move 'SUPERSESSION'             To W002-ART-RAD(LAENGD:12)         
372700       Add  12                         To LAENGD                          
372800       Move TAB-TECKEN                 To W002-ART-RAD(LAENGD:1)          
372900       Add  1                          To LAENGD                          
373000     End-If                                                               
373100* Ansk/Ber                                                                
373200     If WS-FLAGGA-LISTA (14) > Space                                      
373300       Move 'PURCH.PL'                 To W002-ART-RAD(LAENGD:8)          
373400       Add  8                          To LAENGD                          
373500       Move TAB-TECKEN                 To W002-ART-RAD(LAENGD:1)          
373600       Add  1                          To LAENGD                          
373700                                                                          
373800       Move 'PLANNER'                  To W002-ART-RAD(LAENGD:7)          
373900       Add  7                          To LAENGD                          
374000       Move TAB-TECKEN                 To W002-ART-RAD(LAENGD:1)          
374100       Add  1                          To LAENGD                          
374200     End-If                                                               
374300* Dir.lev                                                                 
374400     If WS-FLAGGA-LISTA (15) > Space                                      
374500       Move 'Dlev-andel'               To W002-ART-RAD(LAENGD:10)         
374600       Add  10                         To LAENGD                          
374700       Move TAB-TECKEN                 To W002-ART-RAD(LAENGD:1)          
374800       Add  1                          To LAENGD                          
374900     End-If                                                               
375000* Avs.dag                                                                 
375100     If WS-FLAGGA-LISTA (16) > Space                                      
375200       Move 'Send day'                 To W002-ART-RAD(LAENGD:8)          
375300       Add  8                          To LAENGD                          
375400       Move TAB-TECKEN                 To W002-ART-RAD(LAENGD:1)          
375500       Add  1                          To LAENGD                          
375600     End-If                                                               
375700* TREND                                                                   
375800     If WS-FLAGGA-LISTA (17) > Space                                      
375900       Move 'Trend'                    To W002-ART-RAD(LAENGD:5)          
376000       Add  5                          To LAENGD                          
376100       Move TAB-TECKEN                 To W002-ART-RAD(LAENGD:1)          
376200       Add  1                          To LAENGD                          
376300                                                                          
376400       Move 'Trend weeks'              To W002-ART-RAD(LAENGD:11)         
376500       Add  11                         To LAENGD                          
376600       Move TAB-TECKEN                 To W002-ART-RAD(LAENGD:1)          
376700       Add  1                          To LAENGD                          
376800                                                                          
376900       Move 'Trend updated'            To W002-ART-RAD(LAENGD:13)         
377000       Add  13                         To LAENGD                          
377100       Move TAB-TECKEN                 To W002-ART-RAD(LAENGD:1)          
377200       Add  1                          To LAENGD                          
377300     End-If                                                               
377400* Säsong                                                                  
377500     If WS-FLAGGA-LISTA (18) > Space                                      
377600       Move 'SEASON LOCKED TO'         To W002-ART-RAD(LAENGD:16)         
377700       Add  16                         To LAENGD                          
377800       Move TAB-TECKEN                 To W002-ART-RAD(LAENGD:1)          
377900       Add  1                          To LAENGD                          
378000       Move 'SEASON'                   To W002-ART-RAD(LAENGD:6)          
378100       Add  6                          To LAENGD                          
378200       Move TAB-TECKEN                 To W002-ART-RAD(LAENGD:1)          
378300       Add  1                          To LAENGD                          
378400       Move 'INSTAB.FACTOR'            To W002-ART-RAD(LAENGD:13)         
378500       Add  13                         To LAENGD                          
378600       Move TAB-TECKEN                 To W002-ART-RAD(LAENGD:1)          
378700       Add  1                          To LAENGD                          
378800     End-If                                                               
378900* Sen.inlev                                                               
379000     If WS-FLAGGA-LISTA (19) > Space                                      
379100       Move 'LATEST DEL.1'             To W002-ART-RAD(LAENGD:12)         
379200       Add  12                         To LAENGD                          
379300       Move TAB-TECKEN                 To W002-ART-RAD(LAENGD:1)          
379400       Add  1                          To LAENGD                          
379500       Move 'LATEST DEL.2'             To W002-ART-RAD(LAENGD:12)         
379600       Add  12                         To LAENGD                          
379700       Move TAB-TECKEN                 To W002-ART-RAD(LAENGD:1)          
379800       Add  1                          To LAENGD                          
379900       Move 'LATEST DEL.3'             To W002-ART-RAD(LAENGD:12)         
380000       Add  12                         To LAENGD                          
380100       Move TAB-TECKEN                 To W002-ART-RAD(LAENGD:1)          
380200       Add  1                          To LAENGD                          
380300       Move 'LATEST DEL.4'             To W002-ART-RAD(LAENGD:12)         
380400       Add  12                         To LAENGD                          
380500       Move TAB-TECKEN                 To W002-ART-RAD(LAENGD:1)          
380600       Add  1                          To LAENGD                          
380700       Move 'LATEST DEL.5'             To W002-ART-RAD(LAENGD:12)         
380800       Add  12                         To LAENGD                          
380900       Move TAB-TECKEN                 To W002-ART-RAD(LAENGD:1)          
381000       Add  1                          To LAENGD                          
381100     End-If                                                               
381200* PB                                                                      
381300     If WS-FLAGGA-LISTA (20) > Space                                      
381400       Move 'FORECAST SEP'             To W002-ART-RAD(LAENGD:12)         
381500       Add  12                         To LAENGD                          
381600       Move TAB-TECKEN                 To W002-ART-RAD(LAENGD:1)          
381700       Add  1                          To LAENGD                          
381800       Move 'FORECAST TOT'             To W002-ART-RAD(LAENGD:12)         
381900       Add  12                         To LAENGD                          
382000       Move TAB-TECKEN                 To W002-ART-RAD(LAENGD:1)          
382100       Add  1                          To LAENGD                          
382200       Move 'FORECAST KIT'             To W002-ART-RAD(LAENGD:12)         
382300       Add  12                         To LAENGD                          
382400       Move TAB-TECKEN                 To W002-ART-RAD(LAENGD:1)          
382500       Add  1                          To LAENGD                          
382600       Move 'FORECAST PLAN'            To W002-ART-RAD(LAENGD:13)         
382700       Add  13                         To LAENGD                          
382800       Move TAB-TECKEN                 To W002-ART-RAD(LAENGD:1)          
382900       Add  1                          To LAENGD                          
383000       Move 'FC PL ToDate'             To W002-ART-RAD(LAENGD:12)         
383100       Add  12                         To LAENGD                          
383200       Move TAB-TECKEN                 To W002-ART-RAD(LAENGD:1)          
383300       Add  1                          To LAENGD                          
383400     End-If                                                               
383500* Oi ru 12                                                                
383600     If WS-FLAGGA-LISTA (21) > Space                                      
383700       Move 'IO LAST 12'               To W002-ART-RAD(LAENGD:10)         
383800       Add  10                         To LAENGD                          
383900       Move TAB-TECKEN                 To W002-ART-RAD(LAENGD:1)          
384000       Add  1                          To LAENGD                          
384100     End-If                                                               
384200* Oi(iår+5)                                                               
384300     If WS-FLAGGA-LISTA (22) > Space                                      
384400       Move 'IO '                      To W002-ART-RAD(LAENGD:3)          
384500       Add  3                          To LAENGD                          
384600       Move OI-ARTAL-0                 To W002-ART-RAD(LAENGD:4)          
384700       Add  4                          To LAENGD                          
384800       Move TAB-TECKEN                 To W002-ART-RAD(LAENGD:1)          
384900       Add  1                          To LAENGD                          
385000       Move 'IO '                      To W002-ART-RAD(LAENGD:3)          
385100       Add  3                          To LAENGD                          
385200       Move OI-ARTAL-1                 To W002-ART-RAD(LAENGD:4)          
385300       Add  4                          To LAENGD                          
385400       Move TAB-TECKEN                 To W002-ART-RAD(LAENGD:1)          
385500       Add  1                          To LAENGD                          
385600       Move 'IO '                      To W002-ART-RAD(LAENGD:3)          
385700       Add  3                          To LAENGD                          
385800       Move OI-ARTAL-2                 To W002-ART-RAD(LAENGD:4)          
385900       Add  4                          To LAENGD                          
386000       Move TAB-TECKEN                 To W002-ART-RAD(LAENGD:1)          
386100       Add  1                          To LAENGD                          
386200       Move 'IO '                      To W002-ART-RAD(LAENGD:3)          
386300       Add  3                          To LAENGD                          
386400       Move OI-ARTAL-3                 To W002-ART-RAD(LAENGD:4)          
386500       Add  4                          To LAENGD                          
386600       Move TAB-TECKEN                 To W002-ART-RAD(LAENGD:1)          
386700       Add  1                          To LAENGD                          
386800       Move 'IO '                      To W002-ART-RAD(LAENGD:3)          
386900       Add  3                          To LAENGD                          
387000       Move OI-ARTAL-4                 To W002-ART-RAD(LAENGD:4)          
387100       Add  4                          To LAENGD                          
387200       Move TAB-TECKEN                 To W002-ART-RAD(LAENGD:1)          
387300       Add  1                          To LAENGD                          
387400       Move 'IO '                      To W002-ART-RAD(LAENGD:3)          
387500       Add  3                          To LAENGD                          
387600       Move OI-ARTAL-5                 To W002-ART-RAD(LAENGD:4)          
387700       Add  4                          To LAENGD                          
387800       Move TAB-TECKEN                 To W002-ART-RAD(LAENGD:1)          
387900       Add  1                          To LAENGD                          
388000     End-If                                                               
388100* Aut/JIT                                                                 
388200     If WS-FLAGGA-LISTA (23) > Space                                      
388300       Move 'AUT.SCHEDULE'             To W002-ART-RAD(LAENGD:12)         
388400       Add  12                         To LAENGD                          
388500       Move TAB-TECKEN                 To W002-ART-RAD(LAENGD:1)          
388600       Add  1                          To LAENGD                          
388700       Move 'JIT'                      To W002-ART-RAD(LAENGD:3)          
388800       Add  3                          To LAENGD                          
388900       Move TAB-TECKEN                 To W002-ART-RAD(LAENGD:1)          
389000       Add  1                          To LAENGD                          
389100     End-If                                                               
389200* Inköpare                                                                
389300     If WS-FLAGGA-LISTA (24) > Space                                      
389400       Move 'PURCHASER'                To W002-ART-RAD(LAENGD:9)          
389500       Add  9                          To LAENGD                          
389600       Move TAB-TECKEN                 To W002-ART-RAD(LAENGD:1)          
389700       Add  1                          To LAENGD                          
389800     End-If                                                               
389900* Pris                                                                    
390000     If WS-FLAGGA-LISTA (25) > Space                                      
390100       Move 'ORDER PRICE'              To W002-ART-RAD(LAENGD:11)         
390200       Add  11                         To LAENGD                          
390300       Move TAB-TECKEN                 To W002-ART-RAD(LAENGD:1)          
390400       Add  1                          To LAENGD                          
390500       Move 'STD. PRICE'               To W002-ART-RAD(LAENGD:10)         
390600       Add  10                         To LAENGD                          
390700       Move TAB-TECKEN                 To W002-ART-RAD(LAENGD:1)          
390800       Add  1                          To LAENGD                          
390900     End-If                                                               
391000* Best.pris                                                               
391100     If WS-FLAGGA-LISTA (26) > Space                                      
391200       Move 'ORDER BAL.VAL'            To W002-ART-RAD(LAENGD:13)         
391300       Add  13                         To LAENGD                          
391400       Move TAB-TECKEN                 To W002-ART-RAD(LAENGD:1)          
391500       Add  1                          To LAENGD                          
391600       Move 'ORDER BAL.OTH'            To W002-ART-RAD(LAENGD:13)         
391700       Add  13                         To LAENGD                          
391800       Move TAB-TECKEN                 To W002-ART-RAD(LAENGD:1)          
391900       Add  1                          To LAENGD                          
392000     End-If                                                               
392100* AVTAL                                                                   
392200     If WS-FLAGGA-LISTA (27) > Space                                      
392300       Move 'OPEN ORDER'               To W002-ART-RAD(LAENGD:10)         
392400       Add  10                         To LAENGD                          
392500       Move TAB-TECKEN                 To W002-ART-RAD(LAENGD:1)          
392600       Add  1                          To LAENGD                          
392700     End-If                                                               
392800* Förp.flag                                                               
392900     If WS-FLAGGA-LISTA (28) > Space                                      
393000       Move 'PACK FLAG'                To W002-ART-RAD(LAENGD:9)          
393100       Add  9                          To LAENGD                          
393200       Move TAB-TECKEN                 To W002-ART-RAD(LAENGD:1)          
393300       Add 1                           To LAENGD                          
393400     End-If                                                               
393500* Vikt/Volym                                                              
393600     If WS-FLAGGA-LISTA (29) > Space                                      
393700       Move 'Weight'                   To W002-ART-RAD(LAENGD:6)          
393800       Add  6                          To LAENGD                          
393900       Move TAB-TECKEN                 To W002-ART-RAD(LAENGD:1)          
394000       Add  1                          To LAENGD                          
394100       Move 'Volume'                   To W002-ART-RAD(LAENGD:6)          
394200       Add  6                          To LAENGD                          
394300       Move TAB-TECKEN                 To W002-ART-RAD(LAENGD:1)          
394400       Add  1                          To LAENGD                          
394500     End-If                                                               
394600* RefillSt.                                                               
394700     If WS-FLAGGA-LISTA (30) > Space                                      
394800       Move 'REFILL STOP'              To W002-ART-RAD(LAENGD:11)         
394900       Add  11                         To LAENGD                          
395000       Move TAB-TECKEN                 To W002-ART-RAD(LAENGD:1)          
395100       Add  1                          To LAENGD                          
395200     End-If                                                               
395300* Proj/Mod.                                                               
395400     If WS-FLAGGA-LISTA (31) > Space                                      
395500       Move 'PROJECT'                  To W002-ART-RAD(LAENGD:7)          
395600       Add  7                          To LAENGD                          
395700       Move TAB-TECKEN                 To W002-ART-RAD(LAENGD:1)          
395800       Add  1                          To LAENGD                          
395900       Move 'MODEL'                    To W002-ART-RAD(LAENGD:5)          
396000       Add  5                          To LAENGD                          
396100       Move TAB-TECKEN                 To W002-ART-RAD(LAENGD:1)          
396200       Add  1                          To LAENGD                          
396300     End-If                                                               
396400* 1:a inlev                                                               
396500     If WS-FLAGGA-LISTA (32) > Space                                      
396600       Move 'PUBL.WEEK'                To W002-ART-RAD(LAENGD:9)          
396700       Add  9                          To LAENGD                          
396800       Move TAB-TECKEN                 To W002-ART-RAD(LAENGD:1)          
396900       Add  1                          To LAENGD                          
397000     End-If                                                               
397100* Utg prod                                                                
397200     If WS-FLAGGA-LISTA (33) > Space                                      
397300       Move 'PROD STOP'                To W002-ART-RAD(LAENGD:9)          
397400       Add  9                          To LAENGD                          
397500       Move TAB-TECKEN                 To W002-ART-RAD(LAENGD:1)          
397600       Add  1                          To LAENGD                          
397700     End-If                                                               
397800* År i lgr                                                                
397900     If WS-FLAGGA-LISTA (34) > Space                                      
398000       Move 'S.Pol'                    To W002-ART-RAD(LAENGD:5)          
398100       Add  5                          To LAENGD                          
398200       Move TAB-TECKEN                 To W002-ART-RAD(LAENGD:1)          
398300       Add  1                          To LAENGD                          
398400     End-If                                                               
398500* Prodsl                                                                  
398600     If WS-FLAGGA-LISTA (35) > Space                                      
398700       Move 'PGRP'                     To W002-ART-RAD(LAENGD:4)          
398800       Add  4                          To LAENGD                          
398900       Move TAB-TECKEN                 To W002-ART-RAD(LAENGD:1)          
399000       Add  1                          To LAENGD                          
399100     End-If                                                               
399200* Funkgrp                                                                 
399300     If WS-FLAGGA-LISTA (36) > Space                                      
399400       Move 'FGRP'                     To W002-ART-RAD(LAENGD:4)          
399500       Add  4                          To LAENGD                          
399600       Move TAB-TECKEN                 To W002-ART-RAD(LAENGD:1)          
399700       Add  1                          To LAENGD                          
399800     End-If                                                               
399900* I sats                                                                  
400000     If WS-FLAGGA-LISTA (37) > Space                                      
400100       Move 'IN KIT'                   To W002-ART-RAD(LAENGD:6)          
400200       Add  6                          To LAENGD                          
400300       Move TAB-TECKEN                 To W002-ART-RAD(LAENGD:1)          
400400       Add  1                          To LAENGD                          
400500     End-If                                                               
400600                                                                          
400700*    --- SEPARAT VALFÄLT "TOT.BEH" (utanför  "LISTA" )                    
400800     If WS10-KVVECKOR-KVPB  > Zero                                        
400900       Move WS10-KVVECKOR-KVPB To W-KVVECKOR-RED                          
401000       String 'TOTAL DEM.' W-KVVECKOR-RED ' WEEKS'                        
401100                           TAB-TECKEN                                     
401200        Delimited By Size            Into W002-ART-RAD(LAENGD:19)         
401300       Add  19                         To LAENGD                          
401400     End-If                                                               
401500                                                                          
401600*    --- SEPARAT VALFÄLT "K.AVROP" (utanför  "LISTA" )                    
401700     If WS10-KVVECKOR-AVROP > Zero                                        
401800       Move WS10-KVVECKOR-AVROP To W-KVVECKOR-RED                         
401900       String 'CALLS ' W-KVVECKOR-RED ' WEEKS'                            
402000                       TAB-TECKEN                                         
402100        Delimited By Size            Into W002-ART-RAD(LAENGD:15)         
402200       Add  15                         To LAENGD                          
402300     End-If                                                               
402400* Styrparm                                                                
402500     If WS-FLAGGA-LISTA (38) > Space                                      
402600       Move 'VVCL'                     To W002-ART-RAD(LAENGD:4)          
402700       Add  4                          To LAENGD                          
402800       Move TAB-TECKEN                 To W002-ART-RAD(LAENGD:1)          
402900       Add  1                          To LAENGD                          
403000       Move 'MAD SEP'                  To W002-ART-RAD(LAENGD:8)          
403100       Add  8                          To LAENGD                          
403200       Move TAB-TECKEN                 To W002-ART-RAD(LAENGD:1)          
403300       Add  1                          To LAENGD                          
403400       Move 'PRICE CL'                 To W002-ART-RAD(LAENGD:8)          
403500       Add  8                          To LAENGD                          
403600       Move TAB-TECKEN                 To W002-ART-RAD(LAENGD:1)          
403700       Add  1                          To LAENGD                          
403800       Move 'FREQ.CLASS'               To W002-ART-RAD(LAENGD:9)          
403900       Add  9                          To LAENGD                          
404000       Move TAB-TECKEN                 To W002-ART-RAD(LAENGD:1)          
404100       Add  1                          To LAENGD                          
404200     End-If                                                               
404300* Service                                                                 
404400     If WS-FLAGGA-LISTA (39) > Space                                      
404500       Move 'IO LINES LW'              To W002-ART-RAD(LAENGD:11)         
404600       Add  11                         To LAENGD                          
404700       Move TAB-TECKEN                 To W002-ART-RAD(LAENGD:1)          
404800       Add  1                          To LAENGD                          
404900       Move 'BOOKED LW '               To W002-ART-RAD(LAENGD:10)         
405000       Add  10                         To LAENGD                          
405100       Move TAB-TECKEN                 To W002-ART-RAD(LAENGD:1)          
405200       Add  1                          To LAENGD                          
405300       Move 'SERVICE LW'               To W002-ART-RAD(LAENGD:10)         
405400       Add  10                         To LAENGD                          
405500       Move TAB-TECKEN                 To W002-ART-RAD(LAENGD:1)          
405600       Add  1                          To LAENGD                          
405700     End-If                                                               
405800* Sort                                                                    
405900     If WS-FLAGGA-LISTA (40) > Space                                      
406000       Move 'SORT'                     To W002-ART-RAD(LAENGD:4)          
406100       Add  4                          To LAENGD                          
406200       Move TAB-TECKEN                 To W002-ART-RAD(LAENGD:1)          
406300       Add  1                          To LAENGD                          
406400     End-If                                                               
406500* Kvanter                                                                 
406600     If WS-FLAGGA-LISTA (41) > Space                                      
406700       Move 'Q-opt,EOQ'                To W002-ART-RAD(LAENGD:9)          
406800       Add  9                          To LAENGD                          
406900       Move TAB-TECKEN                 To W002-ART-RAD(LAENGD:1)          
407000       Add  1                          To LAENGD                          
407100       Move 'MIN QTY'                  To W002-ART-RAD(LAENGD:7)          
407200       Add  7                          To LAENGD                          
407300       Move TAB-TECKEN                 To W002-ART-RAD(LAENGD:1)          
407400       Add  1                          To LAENGD                          
407500       Move 'Q-QTY  '                  To W002-ART-RAD(LAENGD:7)          
407600       Add  7                          To LAENGD                          
407700       Move TAB-TECKEN                 To W002-ART-RAD(LAENGD:1)          
407800       Add  1                          To LAENGD                          
407900       Move 'MAN.Q, Q-BL'              To W002-ART-RAD(LAENGD:11)         
408000       Add  11                         To LAENGD                          
408100       Move TAB-TECKEN                 To W002-ART-RAD(LAENGD:1)          
408200       Add  1                          To LAENGD                          
408300       Move '   Q0'                    To W002-ART-RAD(LAENGD:5)          
408400       Add  5                          To LAENGD                          
408500       Move TAB-TECKEN                 To W002-ART-RAD(LAENGD:1)          
408600       Add  1                          To LAENGD                          
408700       Move '   Q1'                    To W002-ART-RAD(LAENGD:5)          
408800       Add  5                          To LAENGD                          
408900       Move TAB-TECKEN                 To W002-ART-RAD(LAENGD:1)          
409000       Add  1                          To LAENGD                          
409100       Move '   Q2'                    To W002-ART-RAD(LAENGD:5)          
409200       Add  5                          To LAENGD                          
409300       Move TAB-TECKEN                 To W002-ART-RAD(LAENGD:1)          
409400       Add  1                          To LAENGD                          
409500       Move '   Q3'                    To W002-ART-RAD(LAENGD:5)          
409600       Add  5                          To LAENGD                          
409700       Move TAB-TECKEN                 To W002-ART-RAD(LAENGD:1)          
409800       Add  1                          To LAENGD                          
409900       Move '   Q4'                    To W002-ART-RAD(LAENGD:5)          
410000       Add  5                          To LAENGD                          
410100       Move TAB-TECKEN                 To W002-ART-RAD(LAENGD:1)          
410200       Add  1                          To LAENGD                          
410300       Move 'MIN.LOAD'                 To W002-ART-RAD(LAENGD:8)          
410400       Add  8                          To LAENGD                          
410500       Move TAB-TECKEN                 To W002-ART-RAD(LAENGD:1)          
410600       Add  1                          To LAENGD                          
410700       Move 'QUAL.BL.CDC'              To W002-ART-RAD(LAENGD:11)         
410800       Add  11                         To LAENGD                          
410900       Move TAB-TECKEN                 To W002-ART-RAD(LAENGD:1)          
411000       Add  1                          To LAENGD                          
411100       Move 'NEW CALC'                 To W002-ART-RAD(LAENGD:8)          
411200       Add  8                          To LAENGD                          
411300       Move TAB-TECKEN                 To W002-ART-RAD(LAENGD:1)          
411400       Add  1                          To LAENGD                          
411500     End-If                                                               
411600* Förp.Info                                                               
411700     If WS-FLAGGA-LISTA (42) > Space                                      
411800       Move '  EMBQ-0'                 To W002-ART-RAD(LAENGD:8 )         
411900       Add  8                          To LAENGD                          
412000       Move TAB-TECKEN                 To W002-ART-RAD(LAENGD:1)          
412100       Add  1                          To LAENGD                          
412200       Move '  EMBQ-1'                 To W002-ART-RAD(LAENGD:8)          
412300       Add  8                          To LAENGD                          
412400       Move TAB-TECKEN                 To W002-ART-RAD(LAENGD:1)          
412500       Add  1                          To LAENGD                          
412600       Move '  EMBQ-2'                 To W002-ART-RAD(LAENGD:8)          
412700       Add  8                          To LAENGD                          
412800       Move TAB-TECKEN                 To W002-ART-RAD(LAENGD:1)          
412900       Add  1                          To LAENGD                          
413000       Move '  EMBQ-3'                 To W002-ART-RAD(LAENGD:8)          
413100       Add  8                          To LAENGD                          
413200       Move TAB-TECKEN                 To W002-ART-RAD(LAENGD:1)          
413300       Add  1                          To LAENGD                          
413400       Move '  EMBQ-4'                 To W002-ART-RAD(LAENGD:8)          
413500       Add  8                          To LAENGD                          
413600       Move TAB-TECKEN                 To W002-ART-RAD(LAENGD:1)          
413700       Add  1                          To LAENGD                          
413800       Move 'PACK CODE'                To W002-ART-RAD(LAENGD:9)          
413900       Add  9                          To LAENGD                          
414000       Move TAB-TECKEN                 To W002-ART-RAD(LAENGD:1)          
414100       Add  1                          To LAENGD                          
414200       Move 'PACK TYPE'                To W002-ART-RAD(LAENGD:9)          
414300       Add  9                          To LAENGD                          
414400       Move TAB-TECKEN                 To W002-ART-RAD(LAENGD:1)          
414500       Add  1                          To LAENGD                          
414600       Move 'DIRECT WAGES'             To W002-ART-RAD(LAENGD:12)         
414700       Add  12                         To LAENGD                          
414800       Move TAB-TECKEN                 To W002-ART-RAD(LAENGD:1)          
414900       Add 1                           To LAENGD                          
415000       Move 'SURCHARGE PACK.MTRL'      To W002-ART-RAD(LAENGD:19)         
415100       Add  19                         To LAENGD                          
415200       Move TAB-TECKEN                 To W002-ART-RAD(LAENGD:1)          
415300       Add  1                          To LAENGD                          
415400     End-If                                                               
415500* Spärrade                                                                
415600     If WS-FLAGGA-LISTA (43) > Space                                      
415700       Move 'BLOCKED QTY'              To W002-ART-RAD(LAENGD:11)         
415800       Add  11                         To LAENGD                          
415900       Move TAB-TECKEN                 To W002-ART-RAD(LAENGD:1)          
416000       Add  1                          To LAENGD                          
416100     End-If                                                               
416200* S-lager                                                                 
416300     If WS-FLAGGA-LISTA (44) > Space                                      
416400       Move 'SECURITY STOCK'           To W002-ART-RAD(LAENGD:14)         
416500       Add  14                         To LAENGD                          
416600       Move TAB-TECKEN                 To W002-ART-RAD(LAENGD:1)          
416700       Add  1                          To LAENGD                          
416800       Move 'SECURITY FACTOR'          To W002-ART-RAD(LAENGD:15)         
416900       Add  15                         To LAENGD                          
417000       Move TAB-TECKEN                 To W002-ART-RAD(LAENGD:1)          
417100       Add  1                          To LAENGD                          
417200       Move 'SEC.FACT.DATE'            To W002-ART-RAD(LAENGD:13)         
417300       Add  13                         To LAENGD                          
417400       Move TAB-TECKEN                 To W002-ART-RAD(LAENGD:1)          
417500       Add  1                          To LAENGD                          
417600     End-If                                                               
417700* MaxPunkt                                                                
417800     If WS-FLAGGA-LISTA (45) > Space                                      
417900       Move 'MAX POINT'                To W002-ART-RAD(LAENGD:9)          
418000       Add  9                          To LAENGD                          
418100       Move TAB-TECKEN                 To W002-ART-RAD(LAENGD:1)          
418200       Add  1                          To LAENGD                          
418300       Move 'MAX-LS'                   To W002-ART-RAD(LAENGD:6)          
418400       Add  6                          To LAENGD                          
418500       Move TAB-TECKEN                 To W002-ART-RAD(LAENGD:1)          
418600       Add  1                          To LAENGD                          
418700     End-If                                                               
418800* KR-status                                                               
418900     If WS-FLAGGA-LISTA (46) > Space                                      
419000       Move 'IR ST'                    To W002-ART-RAD(LAENGD:5)          
419100       Add  5                          To LAENGD                          
419200       Move TAB-TECKEN                 To W002-ART-RAD(LAENGD:1)          
419300       Add  1                          To LAENGD                          
419400       Move 'IR NO'                    To W002-ART-RAD(LAENGD:5)          
419500       Add  5                          To LAENGD                          
419600       Move TAB-TECKEN                 To W002-ART-RAD(LAENGD:1)          
419700       Add  1                          To LAENGD                          
419800     End-If                                                               
419900* Kampanj                                                                 
420000     If WS-FLAGGA-LISTA (47) > Space                                      
420100       Move 'CAMPAIGN ID'              To W002-ART-RAD(LAENGD:11)         
420200       Add  11                         To LAENGD                          
420300       Move TAB-TECKEN                 To W002-ART-RAD(LAENGD:1)          
420400       Add  1                          To LAENGD                          
420500       Move 'CAMPAIGN GRP'             To W002-ART-RAD(LAENGD:12)         
420600       Add  12                         To LAENGD                          
420700       Move TAB-TECKEN                 To W002-ART-RAD(LAENGD:1)          
420800       Add  1                          To LAENGD                          
420900       Move 'CAMPAIGN START'           To W002-ART-RAD(LAENGD:14)         
421000       Add  14                         To LAENGD                          
421100       Move TAB-TECKEN                 To W002-ART-RAD(LAENGD:1)          
421200       Add  1                          To LAENGD                          
421300       Move 'CAMPAIGN STOP'            To W002-ART-RAD(LAENGD:13)         
421400       Add  13                         To LAENGD                          
421500       Move TAB-TECKEN                 To W002-ART-RAD(LAENGD:1)          
421600       Add  1                          To LAENGD                          
421700       Move 'CAMPAIGN CODE'            To W002-ART-RAD(LAENGD:13)         
421800       Add  13                         To LAENGD                          
421900       Move TAB-TECKEN                 To W002-ART-RAD(LAENGD:1)          
422000       Add  1                          To LAENGD                          
422100     End-If                                                               
422200* Lag.omr                                                                 
422300     If WS-FLAGGA-LISTA (48) > Space                                      
422400       Move 'CDC PART ADDRESS'         To W002-ART-RAD(LAENGD:16)         
422500       Add  16                         To LAENGD                          
422600       Move TAB-TECKEN                 To W002-ART-RAD(LAENGD:1)          
422700       Add  1                          To LAENGD                          
422800     End-If                                                               
422900* Gate                                                                    
423000     If WS-FLAGGA-LISTA (49) > Space                                      
423100       Move 'GATE    '                 To W002-ART-RAD(LAENGD:8)          
423200       Add  8                          To LAENGD                          
423300       Move TAB-TECKEN                 To W002-ART-RAD(LAENGD:1)          
423400       Add  1                          To LAENGD                          
423500     End-If                                                               
423600* Uart/LSR                                                                
423700     If WS-FLAGGA-LISTA (50) > Space                                      
423800       Move 'EXC.PART'                 To W002-ART-RAD(LAENGD:8)          
423900       Add  8                          To LAENGD                          
424000       Move TAB-TECKEN                 To W002-ART-RAD(LAENGD:1)          
424100       Add  1                          To LAENGD                          
424200       Move 'LSR'                      To W002-ART-RAD(LAENGD:3)          
424300       Add  3                          To LAENGD                          
424400       Move TAB-TECKEN                 To W002-ART-RAD(LAENGD:1)          
424500       Add  1                          To LAENGD                          
424600     End-If                                                               
424700* 51 - Ursprung                                                           
424800     If WS-FLAGGA-LISTA (51) > Space                                      
424900       Move 'Origin'                   To W002-ART-RAD(LAENGD:6)          
425000       Add  6                          To LAENGD                          
425100       Move TAB-TECKEN                 To W002-ART-RAD(LAENGD:1)          
425200       Add  1                          To LAENGD                          
425300     End-If                                                               
425400* 52 - KDOTFREK                                                           
425500     If WS-FLAGGA-LISTA (52) > Space                                      
425600       Move 'OHF'                      To W002-ART-RAD(LAENGD:6)          
425700       Add  3                          To LAENGD                          
425800       Move TAB-TECKEN                 To W002-ART-RAD(LAENGD:1)          
425900       Add  1                          To LAENGD                          
426000     End-If                                                               
426100                                                                          
426200     MOVE SPACE                       TO W-WRITE-PART-ON-EXCEL            
426300     PERFORM S21-SKRIV-W21710-001                                         
426400     .                                                                    
426500     EJECT                                                                
426600                                                                          
426700 S99-ABEND SECTION.                                                       
426800     MOVE 'S99-ABEND                   ' TO CURRENT-SECTION               
426900                                                                          
427000     Move 'T'         To POSTSUM-OPKOD                                    
427100     Move 'W21709'    To POSTSUM-FDNAMN                                   
427200     Move 'W21710D2'  To POSTSUM-DDNAMN2                                  
427300     Move 'IN'        To POSTSUM-TRANSTYP                                 
427400     Move ANT-LB-POST To POSTSUM-TOTTRANS                                 
427500     Call POSTSUM Using POSTSUM-PARM                                      
427600                                                                          
427700     Move 'S' To POSTSUM-OPKOD                                            
427800     Call POSTSUM Using POSTSUM-PARM                                      
427900     Call ABEND Using RKOD-ABEnd-UTAN-DUMP                                
428000     .                                                                    
428100     EJECT                                                                
428200                                                                          
428300** DB2-sections  ****                                                     
428400                                                                          
428500 DB2-OPEN-KMPDATA   SECTION.                                              
428600     MOVE 'DB2-OPEN-KMPDATA   ' TO  WS-DB2-SEKTION                        
428700                                                                          
428800     MOVE 000100 TO GOOD-SQLCODECODES                                     
428900                                                                          
429000     EXEC SQL DECLARE KMPDATA-CRS CURSOR FOR                              
429100           SELECT  A.IDARTNR                                              
429200                  ,A.IDKAMP                                               
429300                  ,K.TISTADAT_KAMP                                        
429400                  ,K.TISTODAT_KAMP                                        
429500                  ,K.KDKAMP                                               
429600                  ,K.IDKAMP_GRP                                           
429700                                                                          
429800           FROM    TP1ARTK A, TP1KAMP K                                   
429900                                                                          
430000           WHERE   A.IDARTNR = :W-IDARTNR                                 
430100           AND     A.IDKAMP  = K.IDKAMP                                   
430200           ORDER BY K.IDKAMP                                              
430300                                                                          
430400     END-EXEC                                                             
430500     MOVE 000100 TO GOOD-SQLCODECODES                                     
430600     EXEC SQL OPEN KMPDATA-CRS END-EXEC                                   
430700                                                                          
430800     MOVE SQLCODE TO SQLCODE-WS                                           
430900     PERFORM DB2-STATUS-KONTROLL                                          
431000     .                                                                    
431100     EJECT                                                                
431200 DB2-FETCH-KMPDATA-CRS1 SECTION.                                          
431300     MOVE 'DB2-FETCH-KMPDATA-CRS1' TO  WS-DB2-SEKTION                     
431400                                                                          
431500     MOVE 000100  TO GOOD-SQLCODECODES                                    
431600     EXEC SQL FETCH KMPDATA-CRS INTO                                      
431700                  :TP1ARTK-IDARTNR                                        
431800                 ,:TP1ARTK-IDKAMP                                         
431900                 ,:TP1KAMP-TISTADAT-KAMP                                  
432000                 ,:TP1KAMP-TISTODAT-KAMP                                  
432100                 ,:TP1KAMP-KDKAMP                                         
432200                 ,:TP1KAMP-IDKAMP-GRP                                     
432300     END-EXEC                                                             
432400                                                                          
432500     MOVE SQLCODE TO SQLCODE-WS                                           
432600     PERFORM DB2-STATUS-KONTROLL                                          
432700     .                                                                    
432800     EJECT                                                                
432900 DB2-CLOSE-KMPDATA-CRS1 SECTION.                                          
433000     MOVE 'DB2-CLOSE-KMPDATA-CRS1' TO  WS-DB2-SEKTION                     
433100     EXEC SQL CLOSE KMPDATA-CRS END-EXEC                                  
433200     .                                                                    
433300                                                                          
433400 DB2-SELECT-BYART SECTION.                                                
433500     MOVE 'DB2-SELECT-BYART'       TO  WS-DB2-SEKTION                     
433600                                                                          
433700     MOVE 000100 TO GOOD-SQLCODECODES                                     
433800                                                                          
433900     EXEC SQL SELECT                                                      
434000                  IDARTNR_BYT,                                            
434100                  KVLS_MAXCORE                                            
434200              INTO                                                        
434300                  :BYART-IDARTNR-BYT,                                     
434400                  :BYART-KVLS-MAXCORE                                     
434500            FROM BYART                                                    
434600            WHERE IDARTNR_BYT = :BYART-IDARTNR-BYT                        
434700     END-EXEC                                                             
434800     MOVE SQLCODE           TO SQLCODE-WS                                 
434900     PERFORM DB2-STATUS-KONTROLL                                          
435000     .                                                                    
435100** IMS-sections  ****                                                     
435200                                                                          
435300 IMS-GU-WDF501 SECTION.                                                   
435400     Move 'IMS-GU-WDF501        ' To IMS-SEKTION                          
435500     String 'WDF501  (IDARTNR  =' W-IDARTNR-X ')'                         
435600          Delimited By Size Into SSA1                                     
435700     Move '  GE' To GODK-STATUSKODER                                      
435800     Call CBLTDLI Using GU WDF5-PCB DLI-IO-AREA-WDF5 SSA1                 
435900     Move WDF5-STATUS-CODE To STATUS-WS                                   
436000     Perform IMS-STATUSKONTROLL                                           
436100     .                                                                    
436200     EJECT                                                                
436300 IMS-GNP-WDF502 SECTION.                                                  
436400     Move 'IMS-GNP-WDF502       ' To IMS-SEKTION                          
436500     String 'WDF502  (WDF5KEY >=' W-WDF5KEY-MIN-X                         
436600                    '&WDF5KEY <=' W-WDF5KEY-MAX-X ')'                     
436700          Delimited By Size Into SSA1                                     
436800     Move '  GE' To GODK-STATUSKODER                                      
436900     Call CBLTDLI Using GNP WDF5-PCB DLI-IO-AREA-WDF5 SSA1                
437000     Move WDF5-STATUS-CODE To STATUS-WS                                   
437100     Perform IMS-STATUSKONTROLL                                           
437200     .                                                                    
437300     EJECT                                                                
437400                                                                          
437500 IMS-GU-WDD901 SECTION.                                                   
437600     Move 'IMS-GU-WDD901        ' To IMS-SEKTION                          
437700     String 'WDD901  (WDD901KY =' W-WDD901KY-X ')'                        
437800          Delimited By Size Into SSA1                                     
437900     Move '  GE' To GODK-STATUSKODER                                      
438000     Call CBLTDLI Using GU WDD9-PCB IO-AREA-WDD9 SSA1                     
438100     Move WDD9-STATUS-CODE To STATUS-WS                                   
438200     Perform IMS-STATUSKONTROLL                                           
438300     .                                                                    
438400     SKIP3                                                                
438500 IMS-GNP-WDD902 SECTION.                                                  
438600     Move 'IMS-GNP-WDD902       ' To IMS-SEKTION                          
438700     String 'WDD901  (WDD901KY =' W-WDD901KY-X ')'                        
438800          Delimited By Size Into SSA1                                     
438900     Move   'WDD902  '        To SSA2                                     
439000     Move '  GE' To GODK-STATUSKODER                                      
439100     Call CBLTDLI Using GNP WDD9-PCB IO-AREA-WDD9 SSA1 SSA2               
439200     Move WDD9-STATUS-CODE To STATUS-WS                                   
439300     Perform IMS-STATUSKONTROLL                                           
439400     .                                                                    
439500     SKIP3                                                                
439600 IMS-GU-WDD902 SECTION.                                                   
439700     Move 'IMS-GU-WDD902        ' To IMS-SEKTION                          
439800     String 'WDD901  (WDD901KY =' W-WDD901KY-X ')'                        
439900          Delimited By Size Into SSA1                                     
440000     String 'WDD902  (IDLEVNR  =' W-IDLEVNR-X ')'                         
440100          Delimited By Size Into SSA2                                     
440200     Move '  GE' To GODK-STATUSKODER                                      
440300     Call CBLTDLI Using GU WDD9-PCB IO-AREA-WDD9 SSA1 SSA2                
440400     Move WDD9-STATUS-CODE To STATUS-WS                                   
440500     Perform IMS-STATUSKONTROLL                                           
440600     .                                                                    
440700     SKIP3                                                                
440800 IMS-GNP-WDD905 SECTION.                                                  
440900     Move 'IMS-GNP-WDD905       ' To IMS-SEKTION                          
441000     String 'WDD905  (KDAVROP  =' W-KDAVROP-X ')'                         
441100          Delimited By Size Into SSA1                                     
441200     Move '  GE' To GODK-STATUSKODER                                      
441300     Call CBLTDLI Using GNP WDD9-PCB IO-AREA-WDD9 SSA1                    
441400     Move WDD9-STATUS-CODE To STATUS-WS                                   
441500     Perform IMS-STATUSKONTROLL                                           
441600     .                                                                    
441700     EJECT                                                                
441800 IMS-GNP-WDD924 SECTION.                                                  
441900     Move 'IMS-GNP-WDD924        ' To IMS-SEKTION                         
442000     String 'WDD901  (WDD901KY =' W-WDD901KY-X ')'                        
442100          Delimited By Size Into SSA1                                     
442200     String 'WDD902  (IDLEVNR  =' W-IDLEVNR-X ')'                         
442300          Delimited By Size Into SSA2                                     
442400     Move 'WDD924   ' To SSA3                                             
442500     Move '  GE' To GODK-STATUSKODER                                      
442600     Call CBLTDLI Using GNP WDD9-PCB IO-AREA-WDD9 SSA1 SSA2 SSA3          
442700     Move WDD9-STATUS-CODE To STATUS-WS                                   
442800     Perform IMS-STATUSKONTROLL                                           
442900     .                                                                    
443000     SKIP3                                                                
443100 IMS-GNP-WDD925 SECTION.                                                  
443200     Move 'IMS-GNP-WDD925        ' To IMS-SEKTION                         
443300                                                                          
443400     String 'WDD901  (WDD901KY =' W-WDD901KY-X ')'                        
443500          Delimited By Size Into SSA1                                     
443600     String 'WDD902  (IDLEVNR  =' W-IDLEVNR-X ')'                         
443700          Delimited By Size Into SSA2                                     
443800     Move 'WDD925   ' To SSA3                                             
443900     Move '  GE' To GODK-STATUSKODER                                      
444000     Call CBLTDLI Using GNP WDD9-PCB IO-AREA-WDD9 SSA1 SSA2 SSA3          
444100     Move WDD9-STATUS-CODE To STATUS-WS                                   
444200     Perform IMS-STATUSKONTROLL                                           
444300     .                                                                    
444400     SKIP3                                                                
444500 IMS-GNP-WDD925-KVAL    SECTION.                                          
444600     Move 'IMS-GNP-WDD925-KVAL   ' To IMS-SEKTION                         
444700                                                                          
444800     STRING 'WDD925  (IDLEVBSK =' W-IDLEVBSK-X ')'                        
444900            DELIMITED BY SIZE INTO SSA1                                   
445000     MOVE '  GE' TO GODK-STATUSKODER                                      
445100     CALL CBLTDLI USING GNP WDD9-PCB IO-AREA-WDD9 SSA1                    
445200     MOVE WDD9-STATUS-CODE TO STATUS-WS                                   
445300     PERFORM IMS-STATUSKONTROLL                                           
445400     .                                                                    
445500 IMS-GU-WDL201 SECTION.                                                   
445600     Move 'IMS-GNP-WDL221       ' To IMS-SEKTION                          
445700                                                                          
445800     String 'WDL201  (IDARTNR  =' W-IDARTNR-X ')'                         
445900            Delimited By Size Into SSA1                                   
446000     Move '  GE'                  To GODK-STATUSKODER                     
446100     Call CBLTDLI Using GU WDL2-PCB DLI-IO-WDL2 SSA1                      
446200     Move WDL2-STATUS-CODE To STATUS-WS                                   
446300     Perform IMS-STATUSKONTROLL                                           
446400     .                                                                    
446500     SKIP3                                                                
446600 IMS-GNP-WDL221 SECTION.                                                  
446700     Move 'IMS-GNP-WDL221       ' To IMS-SEKTION                          
446800                                                                          
446900     Move   'WDL211  '         To SSA1                                    
447000     String 'WDL221  (IDLEVNR  =' W-IDLEVNR-X ')'                         
447100            Delimited By Size Into SSA2                                   
447200     Move '  GE'                To GODK-STATUSKODER                       
447300     Call CBLTDLI Using GNP WDL2-PCB DLI-IO-WDL2 SSA1 SSA2                
447400     Move WDL2-STATUS-CODE      To STATUS-WS                              
447500     Perform IMS-STATUSKONTROLL                                           
447600     .                                                                    
447700     SKIP3                                                                
447800 IMS-GU-WDK626 SECTION.                                                   
447900     Move 'IMS-GU-WDK626        ' To IMS-SEKTION                          
448000                                                                          
448100     String 'WDK601  (IDARTNR  =' W-IDARTNR-X ')'                         
448200          DELIMITED BY SIZE INTO SSA1                                     
448300     Move 'WDK611  (KDSEGKEY =1)' TO SSA2                                 
448400     Move 'WDK626   ' TO SSA3                                             
448500     Move '  GE' TO GODK-STATUSKODER                                      
448600     Call CBLTDLI Using GU WDK6-PCB DLI-IO-WDK626 SSA1 SSA2 SSA3          
448700     Move WDK6-STATUS-CODE TO STATUS-WS                                   
448800     Perform IMS-STATUSKONTROLL                                           
448900     .                                                                    
449000     EJECT                                                                
449100 IMS-GN-W6D111-W6D1SEQ SECTION.                                           
449200     Move 'IMS-GN-W6D111-W6D1SEQ' To IMS-SEKTION                          
449300                                                                          
449400     String 'W6D111  (W6D1HSEQ =' W-W6D1HSEQ-X ')'                        
449500            Delimited By Size Into SSA1                                   
449600     Move '  GEGB' To GODK-STATUSKODER                                    
449700     Call CBLTDLI Using GN W6D1-PCB DLI-IO-W6D111 SSA1                    
449800     Move W6D1-STATUS-CODE To STATUS-WS                                   
449900     Perform IMS-STATUSKONTROLL                                           
450000     .                                                                    
450100     EJECT                                                                
450200                                                                          
450300 IMS-GU-W6H701-SEQB SECTION.                                              
450400     Move 'IMS-GU-W6H701-SEQB   ' To IMS-SEKTION                          
450500                                                                          
450600     String 'W6H701  (W6H7BSEQ>=' W-W6H7B1KY-MIN-X                        
450700                    '&W6H7BSEQ<=' W-W6H7B1KY-MAX-X ')'                    
450800          Delimited By Size Into SSA1                                     
450900     Move '  GBGE' To GODK-STATUSKODER                                    
451000     Call CBLTDLI Using GU W6H7-PCB DLI-IO-W6H701 SSA1                    
451100     Move W6H7-STATUS-CODE To STATUS-WS                                   
451200     Perform IMS-STATUSKONTROLL                                           
451300     .                                                                    
451400     EJECT                                                                
451500 IMS-GN-W6H701-SEQB SECTION.                                              
451600     Move 'IMS-GN-W6H701-SEQB   ' To IMS-SEKTION                          
451700                                                                          
451800     String 'W6H701  (W6H7BSEQ >' W-W6H7B1KY-MIN-X                        
451900                    '&W6H7BSEQ<=' W-W6H7B1KY-MAX-X ')'                    
452000          Delimited By Size Into SSA1                                     
452100     Move '  GBGE' To GODK-STATUSKODER                                    
452200     Call CBLTDLI Using GN W6H7-PCB DLI-IO-W6H701 SSA1                    
452300     Move W6H7-STATUS-CODE To STATUS-WS                                   
452400     Perform IMS-STATUSKONTROLL                                           
452500     .                                                                    
452600     EJECT                                                                
452700 IMS-GN-WDA5A  SECTION.                                                   
452800     Move 'IMS-GN-WDA5A         ' To IMS-SEKTION                          
452900     STRING 'WDA5A1  (WDA5A1KY>=' W-WDA5A1KY-MIN                          
453000                    '&WDA5A1KY<=' W-WDA5A1KY-MAX                          
453100                    '&KDSTARAD>=' W-KDSTARAD-MIN                          
453200                    '&KDSTARAD<=' W-KDSTARAD-MAX ')'                      
453300            DELIMITED BY SIZE INTO SSA1                                   
453400     MOVE '  GEGB' TO GODK-STATUSKODER                                    
453500     CALL CBLTDLI USING GN WDA5A-PCB DLI-IO-WDA5A SSA1                    
453600     MOVE WDA5A-STATUS-CODE TO STATUS-WS                                  
453700     PERFORM IMS-STATUSKONTROLL                                           
453800     .                                                                    
453900     EJECT                                                                
454000                                                                          
454100 IMS-GET-WDK621 SECTION.                                                  
454200                                                                          
454300     STRING 'WDK601  (IDARTNR  =' W-IDARTNR-X ')'                         
454400          DELIMITED BY SIZE INTO SSA1                                     
454500     STRING 'WDK611  (KDSEGKEY =1)'                                       
454600          DELIMITED BY SIZE INTO SSA2                                     
454700     STRING 'WDK621  (IDLEVNR  =' W-IDLEVNR-X ')'                         
454800          DELIMITED BY SIZE INTO SSA3                                     
454900     MOVE '  GE' TO GODK-STATUSKODER                                      
455000     CALL CBLTDLI USING GU WDK61-PCB DLI-IO-WDK621 SSA1 SSA2 SSA3         
455100     MOVE WDK61-STATUS-CODE TO STATUS-WS                                  
455200     PERFORM IMS-STATUSKONTROLL                                           
455300     .                                                                    
455400                                                                          
455500 IMS-GN-WDA5A-TITPO SECTION.                                              
455600     Move 'IMS-GN-WDA5A-TITPO   ' To IMS-SEKTION                          
455700                                                                          
455800     STRING 'WDA5A1  (WDA5A1KY>=' W-WDA5A1KY-MIN                          
455900                    '&WDA5A1KY<=' W-WDA5A1KY-MAX                          
456000                    '&KDTPOTYP>=' W-KDTPOTYP-MIN-X                        
456100                    '&KDTPOTYP<=' W-KDTPOTYP-MAX-X')'                     
456200            DELIMITED BY SIZE INTO SSA1                                   
456300     MOVE '  GEGB' TO GODK-STATUSKODER                                    
456400     CALL CBLTDLI USING GN WDA5A1-PCB DLI-IO-WDA5A SSA1                   
456500     MOVE WDA5A1-STATUS-CODE TO STATUS-WS                                 
456600     PERFORM IMS-STATUSKONTROLL                                           
456700     .                                                                    
456800     EJECT                                                                
456900 IMS-GU-WDA501    SECTION.                                                
457000     Move 'IMS-GU-WDA501        ' To IMS-SEKTION                          
457100                                                                          
457200     STRING 'WDA501  (WDA501KY =' W-WDA501KY ')'                          
457300            DELIMITED BY SIZE INTO SSA1                                   
457400     MOVE '  GE' TO GODK-STATUSKODER                                      
457500     CALL CBLTDLI USING GU WDA5-PCB DLI-IO-WDA501 SSA1                    
457600     MOVE WDA5-STATUS-CODE TO STATUS-WS                                   
457700     PERFORM IMS-STATUSKONTROLL                                           
457800     .                                                                    
457900     SKIP2                                                                
458000 IMS-GU-WDGX1143 SECTION.                                                 
458100     MOVE 'IMS-GU-WDGX1143              ' TO IMS-SEKTION                  
458200                                                                          
458300     STRING 'WDG201  (WDGXKEY  =' W-WDGXKEY-1143-X ')'                    
458400            DELIMITED BY SIZE INTO SSA1                                   
458500     MOVE '    '                TO GODK-STATUSKODER                       
458600     CALL CBLTDLI USING GU WDG2-PCB DLI-IO-WDGX01 SSA1                    
458700     MOVE WDG2-STATUS-CODE      TO STATUS-WS                              
458800     PERFORM IMS-STATUSKONTROLL                                           
458900     .                                                                    
459000                                                                          
459100 IMS-GNP-WDGX1144  SECTION.                                               
459200     MOVE 'IMS-GNP-WDGX1144             ' TO IMS-SEKTION                  
459300                                                                          
459400     MOVE 'WDGX1144 '          TO SSA1                                    
459500     MOVE '  GE'               TO GODK-STATUSKODER                        
459600     CALL CBLTDLI USING GNP WDG2-PCB DLI-IO-WDGX1144 SSA1                 
459700     MOVE WDG2-STATUS-CODE     TO STATUS-WS                               
459800     PERFORM IMS-STATUSKONTROLL                                           
459900     .                                                                    
460000     EJECT                                                                
460100 IMS-STATUSKONTROLL SECTION.                                              
460200                                                                          
460300     Set STATUS-IX To 1                                                   
460400     Search GODK-STATUS                                                   
460500       At End                                                             
460600         String ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
460700         Delimited By Size Into FELTEXT                                   
460800         Call FELLOG                                                      
460900       When GODK-STATUS (STATUS-IX) = STATUS-WS                           
461000         Continue                                                         
461100     End-Search                                                           
461200     .                                                                    
461300                                                                          
461400 DB2-STATUS-KONTROLL       SECTION.                                       
461500                                                                          
461600     SET SQLCODE-IX TO 1                                                  
461700     SEARCH GOOD-SQLCODE                                                  
461800       AT END CALL FELLOG                                                 
461900       WHEN GOOD-SQLCODE (SQLCODE-IX) = SQLCODE-WS CONTINUE               
462000     END-SEARCH                                                           
462100     .                                                                    
462200*    -COPY WY2000P3                                                       
462300     EJECT                                                                
462400*    -COPY WY2000P1                                                       
462500     EJECT                                                                
462600*    -COPY WY2000P2                                                       
462700     EJECT                                                                
