000100 ID DIVISION.                                                             
000210 PROGRAM-ID.         W2016300.                                            
000300 AUTHOR.             IDK, GÖTEBORG.                                       
000400 DATE-WRITTEN.       FEBR 1979.                                           
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*    FUNKTION.                                                            
000800*            TP-PROGRAM FÖR ANSKAFFNING (LEVERANSPLAN)                    
000900*            PROGRAMMET SKÖTER UPPDATERING AV REGISTER                    
001000*            (FRÅGEDELEN UTFÖRES I PROGRAM W2010300)                      
001100*            UPPDATERINGSPROGRAMMET OMFATTAR FÖLJANDE                     
001200*            DELAR:                                                       
001300*                - KONTROLL AV UPPDATERINGSFÄLT                           
001400*                - UPPDATERING AV REGISTER                                
001500*                - OM KDOMSPEC ÄR 3 STARTAS W21035 SOM GÖR NY             
001600*                  PUNKTBERÄKNING OCH OMPEC AV LEVERANSPLANEN             
001700*                  ON-LINE. SEDAN STARTAS 2103 UPP FÖR ATT VISA           
001800*                  DEN NYA LEVERANSPLANEN.                                
001900*                                                                         
002000*            PROGRAMMET LÄSER    WDG3 (WDGX2258/WDGX2260)                 
002100*                                      HTYP 2257                          
002200*                                                                         
002300*    INDATA.                                                              
002400*        TRANSAKTION: W2T163     UPPDATERING                              
002500*        TRANSAKTION: W2T163X    UPPDATERING-DISPATCHERN                  
002600*        MID:         W2I10301                                            
002700*    UTDATA.                                                              
002800*        MOD:         W2O10301                                            
002900*                     WMSGKOM   (DISPATCHERN)                             
003000*    SUBPROGRAM.                                                          
003100*        WDATKONV                                                         
003200*        W221LPAD                                                         
003210*        FELLOG                                                           
003300*                                                                         
003400*    ÄNDRING.                                                             
003500*            FEB-07: TILLÄGG AV ADM.(DLET) AV SEGM PÅ WDD6                
003600*            (LEV.PLANEFÖRSLAGSKÖN), VID GODKÄNNANDE AV FÖRSLAG           
003700*            OCH FÖRKASTANDE AV FÖRSLAG. KOMKOD=1 OCH 2  /C.E.            
003800*                                                                         
003900*            MAJ-12: NYCKEL WDD901 UTÖKAD MED IDDC                        
004000*                                                                         
004100*    14-03-21 LAGT TILL LÄSNING MOT WDG3/HTYP 2257 (BILD 2149)            
004200*             BLOCKERADE AVROPSVECKOR FÖR EN SHIP-LEVERANTÖR.             
004300*             E'TRACKER 8403120.                                          
004400*                                                                         
004500*    15-04-22  ETRACKER 10130993                                          
004600*              REDUCE NUMBER OF DELIVERY SCHEDULES                        
004700*                                                                         
004800*    15-05-20  ETRACKER 10258214                                          
004900*              Rätta felkod tillbaka till disp. 0622 vid                  
005000*              automatgodkända levplaner från W221V1/W2215400.            
005100*                                                                         
005110*    17-06-19  ETRACKER 10185995                                          
005120*              Take away possibility to take away arrears                 
005130*              for kit parts                                              
005140*                                                                         
005150*    17-06-19  ETRACKER 10223668                                          
005160*              Not possible to send schedules if setup                    
005170*              missing on screen 2114.                                    
005200     SKIP3                                                                
005300 ENVIRONMENT DIVISION.                                                    
005400     SKIP3                                                                
005500 CONFIGURATION SECTION.                                                   
005600     SKIP2                                                                
005700 DATA DIVISION.                                                           
005800     EJECT                                                                
005900 WORKING-STORAGE SECTION.                                                 
006000                                                                          
006100*    -COPY WY2000W3                                                       
006200     SKIP3                                                                
006300*    -COPY WY2000W1                                                       
006400     SKIP3                                                                
006500*    -COPY WY2000W9                                                       
006600     SKIP3                                                                
006700 77  FELTEXT                 PIC X(80) VALUE SPACE.                       
006800 77  CURRENT-SECTION         PIC X(30) VALUE SPACE.                       
006900 77  DBS-SECTION             PIC X(30) VALUE SPACE.                       
007000                                                                          
007100 77  IDARTNR-WS              PIC X(9).                                    
007200 77  IDLEVNR-WS              PIC X(5).                                    
007300 77  KDBEHX-PLAN-WS          PIC X(1).                                    
007310 77  W-KIT-EXIST             PIC X(1)    VALUE 'N'.                       
007400 77  JA                      PIC X(1)    VALUE 'J'.                       
007500 77  YES                     PIC X(1)    VALUE 'Y'.                       
007600 77  OCH                     PIC X(1)    VALUE '&'.                       
007700 77  NEJ                     PIC X(1)    VALUE 'N'.                       
007800 77  W-2216-SEG-FINNS        PIC X(1)    VALUE 'J'.                       
007810 77  FL-PRARTBES             PIC X(1)    VALUE 'N'.                       
007900 77  WS-PRARTBES-PR          PIC S9(7)V99 COMP-3.                         
007910 77  WS-ANT-WDK622           PIC S9(3)   VALUE ZERO COMP-3.               
008000 77  ELLER                   PIC X(1)    VALUE '!'.                       
008100 77  FEL                     PIC X(1)    VALUE 'F'.                       
008200 77  GALLANDE                PIC X(1)    VALUE 'G'.                       
008300 77  FORSLAG                 PIC X(1)    VALUE 'F'.                       
008400 77  RAETT                   PIC X(1)    VALUE 'R'.                       
008500 77  NUM                     PIC X(1)    VALUE 'N'.                       
008600 77  EJ-IFYLLD               PIC X(1)    VALUE 'E'.                       
008700 77  AAVV                    PIC X(4)    VALUE 'ÅÅVV'.                    
008800 77  YYWW                    PIC X(4)    VALUE 'YYWW'.                    
008900 77  STRECK                  PIC X(1)    VALUE '-'.                       
009000 77  ASTERISK                PIC X(1)    VALUE '*'.                       
009100 77  PARENTES                PIC X(1)    VALUE ')'.                       
009200 77  KOLON                   PIC X(1)    VALUE ':'.                       
009300 77  ORSAK-KOEP              PIC 9(2)    VALUE 01.                        
009400 77  ORSAK-BEGAERD           PIC 9(2)    VALUE 17.                        
009500 77  ORSAK-BEG-OPT           PIC 9(2)    VALUE 18.                        
009600 77  TRAFF                   PIC X(1)    VALUE SPACE.                     
009700 77  DAGENS-AAAAMMDD         PIC 9(8)    VALUE ZERO.                      
009800 77  DEFINITIV               PIC S9      COMP-3 VALUE +1.                 
009810 77  WS-DAYS-TIDATE1-AAVVD   PIC 9(5)    VALUE ZERO.                      
009900                                                                          
010000 77  MAX-ANT-ORSAKSKODER     PIC S9(9)   VALUE +26   COMP SYNC.           
010100 77  MAX-ANT-PERIODER        PIC S9(9)   VALUE +12   COMP SYNC.           
010200 77  MAX-ANT-VECKOR-I-TAB    PIC S9(9)   VALUE +5    COMP SYNC.           
010300 77  MAX-ANT-PERIODER-I-TAB  PIC S9(9)   VALUE +10   COMP SYNC.           
010400 77  MAX-ANT-GAMLA-AVROP     PIC S9(9)   VALUE +5    COMP SYNC.           
010500 77  MAX-ANT-AENDR-AVROP     PIC S9(9)   VALUE +4    COMP SYNC.           
010600 77  MAX-ANT-INDATA-FLT      PIC S9(9)   VALUE +13   COMP SYNC.           
010700 77  MAX-MOD-LENGD           PIC S9(9)   VALUE +1290 COMP SYNC.           
010800 77  LNG-P-TO-P-PREFIX       PIC S9(4)    VALUE +17  COMP SYNC.           
010900     EJECT                                                                
011000*01  -COPY WWDCKONS                                                       
011100                                                                          
011200 01  OLIKA-LEV               PIC X(5).                                    
011300     88 VOLKSWAGEN-LEVNR                 VALUE '6453'                     
011400                                               'Q09EB'.                   
011500     88 LV-LEVNR                         VALUE '14489'                    
011600                                               'DL7YA'.                   
011700*                                         NYCKELFÄLT FÖR LÄSNING          
011800*                                          AV DATABASER                   
011900 01  W-WDD901KY-X.                                                        
012000     03  W-IDARTNR-D9        PIC S9(9)  VALUE ZERO   COMP-3.              
012100     03  W-IDDC-D9           PIC X(2)   VALUE SPACE.                      
012200 01  W-IDLEVNR-DC-X.                                                      
012300     03  W-IDLEVNR-DC        PIC X(5)   VALUE SPACE.                      
012400 01  W-IDARTNR-X.                                                         
012500     03  W-IDARTNR           PIC S9(9)               COMP-3.              
012600 01  W-KDNOTTYP-X.                                                        
012700     03  W-KDNOTTYP          PIC S9(01) VALUE ZERO   COMP-3.              
012800 01  W-IDLEVNR-X.                                                         
012900     03  W-IDLEVNR           PIC X(5).                                    
013000 01  W-IDLEVNR-SHIP-X.                                                    
013100     03  W-IDLEVNR-SHIP      PIC X(5).                                    
013200 01  W-DAPRLIST-X.                                                        
013300     03  W-DAPRLIST      PIC   9(8)  VALUE ZERO.                          
013400 01  W-KDSEGKEY-X.                                                        
013500     03  W-KDSEGKEY          PIC X(1)    VALUE '1'.                       
013600 01  W-WDGXKEY-IDLEVNR.                                                   
013700     03  X-IDLEVNR           PIC X(5).                                    
013800                                                                          
013900 01  W-WDD601KY-MIN-X.                                                    
014000     03 W-IDDC-MIN           PIC X(2)  Value Space.                       
014100     03 W-IDLEVNR-MIN        PIC X(5)  Value 'A<B<C'.                     
014200     03 W-IDARTNR-MIN        PIC S9(9) Value Zero Comp-3.                 
014300     03 W-IDANSK-MIN-X.                                                   
014400        05 W-IDANSK-MIN      PIC S9(3) Value +000 COMP-3.                 
014500                                                                          
014600 01  W-WDD601KY-MAX-X.                                                    
014700     03 W-IDDC-MAX           PIC X(2)  Value '99'.                        
014800     03 W-IDLEVNR-MAX        PIC X(5)  Value 'Z>Y>X'.                     
014900     03 W-IDARTNR-MAX        PIC S9(9) Value Zero Comp-3.                 
015000     03 W-IDANSK-MAX-X.                                                   
015100        05 W-IDANSK-MAX      PIC S9(3) Value +999 COMP-3.                 
015200                                                                          
015410                                                                          
015420 01  W-WDD905KY-X.                                                        
015500     03  W-DAAVROP-X.                                                     
015600         05  W-DAAVROP       PIC  9(6)    VALUE ZERO.                     
015700         05  FILLER REDEFINES W-DAAVROP.                                  
015800             07  W-DAAVROP-SS    PIC  9(2).                               
015900             07  W-DAAVROP-AAVV  PIC  9(4).                               
016000     03  W-TILEVDAG-X.                                                    
016100         05  W-TILEVDAG      PIC  S9      VALUE ZERO COMP-3.              
016110 01  W-WDJ2CSEQ-MIN-X.                                                    
016120     03  W-J2CSEQ-MIN-IDARTNR   PIC S9(9)  VALUE ZERO COMP-3.             
016130     03  W-J2CSEQ-MIN-DAREGDAT  PIC  9(8)  VALUE zero.                    
016140     SKIP2                                                                
016150 01  W-WDJ2CSEQ-MAX-X.                                                    
016160     03  W-J2CSEQ-MAX-IDARTNR   PIC S9(9)  VALUE ZERO COMP-3.             
016170     03  W-J2CSEQ-MAX-DAREGDAT  PIC  9(8)  VALUE ZERO.                    
016300 01  W-IDBESTNR-X.                                                        
016400     03  FILLER          PIC X       VALUE '0'.                           
016500     03  W-BEST-IDINK    PIC 9(3).                                        
016600     03  FILLER          PIC X       VALUE '9'.                           
016700     03  W-BEST-IDLEVNR  PIC X(5)    VALUE SPACE.                         
016800     03  W-BEST-SUFFIX   PIC 9(3)    VALUE ZERO.                          
016900 01  FILLER              REDEFINES W-IDBESTNR-X.                          
017000     03  W-IDBESTNR      PIC 9(13).                                       
017010                                                                          
017100 01  W-KDAVROP-X.                                                         
017200     03  W-KDAVROP           PIC S9(1)               COMP-3.              
017300 01  W-IDORDNSB-X.                                                        
017400     03  W-IDORDNSB          PIC S9(5)               COMP-3.              
017500 01  W-2203-KEY.                                                          
017600     03  W-2203-IDHTYP       PIC X(4) VALUE '2203'.                       
017700     03  W-2203-IDDC         PIC X(2) value '11'.                         
017800     03  FILLER              PIC X(24) VALUE LOW-VALUE.                   
017900 01  W-WDGXKEY-ROT.                                                       
018000     03  W-WDGX-KEY          PIC X(4)    VALUE ZERO.                      
018100     03  FILLER              PIC X(26)   VALUE LOW-VALUE.                 
018200 01  W-WDGXKEY-PERIOD-ROT.                                                
018300     03  FILLER              PIC X(4)    VALUE '2217'.                    
018400     03  FILLER              PIC X(1)    VALUE 'J'.                       
018500     03  FILLER              PIC X(25)   VALUE LOW-VALUE.                 
018600 01  W-KVBEST-PL-X.                                                       
018700     03  W-KVBEST-PL         PIC S9(7)               COMP-3.              
018800 01  W-KDOMSPEC-X.                                                        
018900     03  W-KDOMSPEC          PIC S9(1)               COMP-3.              
019000 01  W-TILPSP-X.                                                          
019100     03  W-TILPSP            PIC S9(5)               COMP-3.              
019200 01  W-KDANTEX-X.                                                         
019300     03  W-KDANTEX           PIC 9(1).                                    
019400 01  W-KDANTEX-NUM           PIC 9     VALUE 2.                           
019500 01  W-2227KEY-X.                                                         
019600     03 W-IDHTYP        PIC X(4)    VALUE '2227'.                         
019700     03 FILLER          PIC X(26)   VALUE LOW-VALUE.                      
019800 01  W-WDGXKEY-2235-X.                                                    
019900     03 W-IDHTYP        PIC X(4)    VALUE '2235'.                         
020000     03 FILLER          PIC X(26)   VALUE LOW-VALUE.                      
020100 01  W-WDGXKEY-2236-X.                                                    
020200     03 W-IDARTNR-H     PIC S9(9)   VALUE ZERO       COMP-3.              
020300     03 W-TIBEHOV-H     PIC S9(5)   VALUE ZERO       COMP-3.              
020400     03 FILLER          PIC X(2)    VALUE LOW-VALUE.                      
020500 01  W-WDGXKEY-2245-X.                                                    
020600     03 W-IDHTYP-2245   PIC X(4)    VALUE '2245'.                         
020700     03 FILLER          PIC X(26)   VALUE LOW-VALUE.                      
020800 01  WDF3.                                                                
020900     03  W-WDF301KY-X.                                                    
021000         05  W-IDLANDX2      PIC X(2)    VALUE SPACE.                     
021100         05  W-DADATUM-HELG  PIC 9(8)    VALUE ZERO.                      
021200         05  FILLER  REDEFINES W-DADATUM-HELG.                            
021300             07  W-DADATUM-HELG-SS       PIC 9(2).                        
021400             07  W-DADATUM-HELG-AAMMDD   PIC 9(6).                        
021500                                                                          
021600*------                                                                   
021700 01  W-WDGXKEY-2257-X.                                                    
021800     03 W-IDHTYP-2257        PIC X(4)    VALUE '2257'.                    
021900     03 FILLER               PIC X(26)   VALUE LOW-VALUE.                 
022000                                                                          
022100 01  W-WDGXKEY-2258-X.                                                    
022200     03 W-IDLEVNR-SHIP-2258  PIC X(5)    VALUE SPACE.                     
022300                                                                          
022400 01  W-DAAVROP-2260-X.                                                    
022500     03 W-DAAVROP-2260       PIC 9(6)    VALUE ZERO.                      
022600                                                                          
022700 01  W-IDANSK-2260-X.                                                     
022800     03 W-IDANSK-2260        PIC S9(3)   VALUE ZERO COMP-3.               
022900*------                                                                   
022910 01  W-WDGXKEY-2223-X.                                                    
022920     03  W-IDHTYP-2223       PIC X(4)    VALUE '2223'.                    
022930     03  W-IDANSK-2223       PIC S9(3)   COMP-3 VALUE ZERO.               
022940     03  FILLER              PIC X(24)   VALUE LOW-VALUE.                 
022950 01  W-IDDC-X.                                                            
022960     03  W-IDDC              PIC X(2)   VALUE SPACE.                      
022970 01  W-KDLARM-X.                                                          
022980     03  W-KDLARM            PIC S9(3)   COMP-3 VALUE ZERO.               
022981                                                                          
022982 01  W-WDGX2231-X.                                                        
022983     03  W-IDHTYP-2231       PIC X(4)     VALUE '2231'.                   
022984     03  FILLER              PIC X(26)    VALUE LOW-VALUE.                
022985                                                                          
022990 01  W-WDGX2232-X.                                                        
022991     03  W-IDANSK-2232       PIC S9(3)    VALUE ZERO COMP-3.              
022992     03  FILLER              PIC X(3)     VALUE LOW-VALUE.                
022993                                                                          
023000                                                                          
023100     EJECT                                                                
023200*                                          INMATNINGSFAELT                
023300 01  W-INMATNINGSFAELT.                                                   
023400     03  W-KDBEHX-PLAN       PIC X.                                       
023500     03  W-KOMKOD            PIC S9(1)    COMP-3  VALUE ZERO.             
023600     03  W-KVAVROP           PIC S9(7)    COMP-3  VALUE ZERO.             
023700                                                                          
023800     03  W-FLJIT             PIC  X(1)            VALUE SPACE.            
023900     SKIP3                                                                
024000 01  SWITCHAR.                                                            
024100     03  SW-LEVSEGM-FINNS    PIC X(1)    VALUE 'N'.                       
024200     03  SW-UTSKRIFT-BEGAERD                                              
024300                             PIC X(1)    VALUE 'N'.                       
024310     03  SW-UPDATE-WDK6-WDD9                                              
024320                             PIC X(1)    VALUE 'N'.                       
024400     03  SW-UTSKR-BK         PIC X(1)    VALUE 'N'.                       
024500     03  SW-FAELT-IFYLLT     PIC X(1)    VALUE 'N'.                       
024600     03  SW-HUVUDLEVERANTOER PIC X(1)    VALUE 'N'.                       
024700     03  SW-AVROP-AENDRAT    PIC X(1)    VALUE 'N'.                       
024800     03  SW-KOEP-AENDRAT     PIC X(1)    VALUE 'N'.                       
024900     03  SW-OMSPEC-BEG       PIC X(1)    VALUE 'N'.                       
025000     03  FLUPPD-2228         PIC X(1)    VALUE 'N'.                       
025100                                                                          
025200 01  SW-UPPDAT-CLAG          PIC X(1).                                    
025300     88  UPPDAT-OK-CLAG                  VALUE 'J'.                       
025400     88  UPPDAT-EJ-CLAG                  VALUE 'N'.                       
025500                                                                          
025600 01  SW-INDATA               PIC X(1).                                    
025700     88  INDATA-OK                       VALUE 'J'.                       
025800     88  INDATA-FEL                      VALUE 'N'.                       
025900                                                                          
026000 01  SW-SATSBEORDR-FINNS     PIC X(1)    VALUE SPACE.                     
026100     88  SATSBEORDR-FINNS                VALUE 'J'.                       
026200                                                                          
026300 01  SW-TRAFF-DAG            PIC X(1)    VALUE SPACE.                     
026400     88  FORSTA-LEV-DAG                  VALUE 'J'.                       
026500                                                                          
026600 01  SW-FLJIT-UPPD           PIC X(1)    VALUE SPACE.                     
026700     88  FLJIT-UPPD                      VALUE 'J'.                       
026800     EJECT                                                                
026900 01  ARBETSAREOR.                                                         
027000                                                                          
027100     03  IX                  PIC S9(9)               COMP SYNC.           
027200     03  IX-DAG              PIC S9(3)   VALUE +0    COMP SYNC.           
027300     03  IX-AVROP            PIC S9(9)               COMP SYNC.           
027400     03  IX-PLUS-1           PIC S9(9)               COMP SYNC.           
027500     03  IX-PLUS-2           PIC S9(9)               COMP SYNC.           
027600     03  IX-TOT              PIC S9(9)               COMP SYNC.           
027700     03  IX-GAM              PIC S9(9)               COMP SYNC.           
027800     03  IX-VECKA            PIC S9(9)               COMP SYNC.           
027900     03  IY                  PIC S9(9)               COMP SYNC.           
028000     03  IY-PERIOD           PIC S9(9)               COMP SYNC.           
028100     03  IX-PP               PIC S9(9)               COMP SYNC.           
028200     03  IX-AAPP             PIC  9(2).                                   
028300     03  IX-KOLL             PIC  9(2).                                   
028400     03  PIX                 PIC  9(2).                                   
028500     03  WS-KVPALL           PIC S9(7)   VALUE ZERO COMP-3.               
028600     03  BEORDR-AVROP OCCURS 4 PIC S9(5) VALUE ZERO COMP-3.               
028700     03  W-TIFINLV           PIC 9(5)    VALUE ZERO.                      
028800     03  FILLER  REDEFINES W-TIFINLV.                                     
028900         05  W-TIFINLV-AA    PIC 9(2).                                    
029000         05  W-TIFINLV-VV    PIC 9(2).                                    
029100         05  FILLER          PIC 9.                                       
029200     03  W-TIAVROP-AVS       PIC 9(5)    VALUE ZERO.                      
029300     03  FILLER  REDEFINES W-TIAVROP-AVS.                                 
029400         05  FILLER          PIC 9.                                       
029500         05  W-TIAVROP-AA    PIC 9(2).                                    
029600         05  W-TIAVROP-VV    PIC 9(2).                                    
029700     03  W-TIFINLV-VVVV      PIC S9(5) VALUE ZERO COMP-3.                 
029800     03  W-TIAVROP-VVVV      PIC S9(5) VALUE ZERO COMP-3.                 
029900     03  FILLER              PIC X(8)       VALUE 'ARSBEH'.               
030000     03  W-ARSBEH            PIC S9(9)      VALUE ZERO  COMP-3.           
030100     03  FILLER              PIC X(8)       VALUE 'ARSOMS'.               
030200     03  W-ARSOMS            PIC S9(9)V9(2) VALUE ZERO  COMP-3.           
030300     03  FILLER              PIC X(8)       VALUE 'ARSOMS'.               
030400     03  W-ARSOMS-80000      PIC S9(9)V9(2) VALUE 80000 COMP-3.           
030500     03  FILLER              PIC X(8)       VALUE 'ARSOMS'.               
030600     03  W-KVPB-SDC          PIC S9(6)V9(1) VALUE ZERO  COMP-3.           
030700     SKIP3                                                                
030800     03 FILLER               PIC X(16)   VALUE 'W-AVROP-TAB-RAD'.         
030900     03 W-AVROP-TAB-RAD      OCCURS 11.                                   
031000         05  W-PERIOD-TAB-RAD  PIC 9(2).                                  
031100         05  W-AVROP-TAB-KOL OCCURS 5.                                    
031200             10  W-AVROP-TAB-AENDRAT                                      
031300                             PIC X.                                       
031400             10  W-KVAVROP-TAB                                            
031500                             PIC S9(7)               COMP-3.              
031600             10  W-TIAVROP-AVS-TAB                                        
031700                             PIC S9(3)               COMP-3.              
031800                                                                          
031900     03  W-AVROP-GAMLA       OCCURS 5.                                    
032000         05  W-AVROP-GAM-AENDRAT                                          
032100                             PIC X.                                       
032200         05  W-KVAVROP-GAM   PIC S9(7)               COMP-3.              
032300         05  W-TIAVROP-AVS-GAM                                            
032400                             PIC  9(4).                                   
032500                                                                          
032600     03 FILLER               PIC X(16)   VALUE 'W-AVROP-TAB-RAD'.         
032700     03  W-ORSKAS-TAB        OCCURS 3.                                    
032800         05  W-ORSAK-AENDRAD PIC X.                                       
032900         05  W-ORSAK-TAB-KOD PIC 9(2).                                    
033000     SKIP3                                                                
033100     03  W-ORSAKS-KOD        PIC 9(2).                                    
033200     03  W-ANTAL-VECKOR      PIC S9(3)               COMP-3.              
033300                                                                          
033400     03  W-DATUM-AKTUELLT    PIC S9(5)               COMP-3.              
033500     03  W-TID-AKTUELLT      PIC 9(1).                                    
033600     03  W-DATUM-AKT-PLUS59  PIC S9(5)               COMP-3.              
033700                                                                          
033800     03  W-DATUM-AAVV        PIC 9(4).                                    
033900     03  FILLER              REDEFINES W-DATUM-AAVV.                      
034000         05  W-DATUM-AA      PIC 9(2).                                    
034100         05  W-DATUM-VV      PIC 9(2).                                    
034200     03  W-DATUM-SATS        PIC 9(4).                                    
034201                                                                          
034210     03  NEXT-KLOCKSLAG.                                                  
034220       05  WS-NXT-KL                 PIC 9(6).                            
034230       05  FILLER                    REDEFINES WS-NXT-KL.                 
034240         07  WS-NXT-KL-TT            PIC 9(2).                            
034250         07  WS-NXT-KL-MM            PIC 9(2).                            
034260         07  WS-NXT-KL-SS            PIC 9(2).                            
034270                                                                          
034400     03  W-IDLOGLOP          PIC 9.                                       
034500     03  W-IDARTNR-N         PIC 9(8).                                    
034600     03  W-IDLEVNR-N         PIC X(5).                                    
034700     03  W-SKILJETECKEN      PIC X.                                       
034800     03  W-KVAVROP-ACC       PIC S9(7)               COMP-3.              
034900     03  W-KVBR              PIC S9(7)               COMP-3.              
035000     03  W-DAGENS-DAT-PLUS-LT PIC S9(5)              COMP-3.              
035100     03  W-DATUM-AKT-5VV     PIC S9(5)               COMP-3.              
035200     03  W-KVDAGAR-FFH       PIC S9(3)               COMP-3.              
035300     03  WS-TILPSP           PIC  9(4).                                   
035400     03  WS-IDORDNSB         PIC  9(5).                                   
035500                                                                          
035600     03  W-PER-PP            PIC 99   VALUE ZERO.                         
035700     03  W-PERIOD-AAPP       PIC 9(4).                                    
035800     03  FILLER              REDEFINES W-PERIOD-AAPP.                     
035900         05  W-PERIOD-AA     PIC 9(2).                                    
036000         05  W-PERIOD-PP     PIC 9(2).                                    
036100     03  W-PERIOD-AAPP-START PIC 9(4).                                    
036200                                                                          
036300     03  W-DAXLEVSP          PIC 9(6).                                    
036400     03  FILLER  REDEFINES W-DAXLEVSP.                                    
036500         05  W-DAXLEVSP-SS   PIC 9(2).                                    
036600         05  W-DAXLEVSP-AAVV PIC 9(4).                                    
036700     03  W-DASPECST          PIC 9(6).                                    
036800     03  FILLER  REDEFINES W-DASPECST.                                    
036900         05  W-DASPECST-SS   PIC 9(2).                                    
037000         05  W-DASPECST-AAVV PIC 9(4).                                    
037100     03  W-DAAVROP-AVS       PIC 9(6).                                    
037200     03  FILLER  REDEFINES W-DAAVROP-AVS.                                 
037300         05  W-DAAVROP-AVS-SS    PIC 9(2).                                
037400         05  W-DAAVROP-AVS-AAVV  PIC 9(4).                                
037500                                                                          
037600     03  W-PER-AA            PIC S9(3)               COMP-3.              
037700     03  W-BER-PERIOD        PIC S9(5)               COMP-3.              
037800     03  W-START-PER-AAPP    PIC S9(5)               COMP-3.              
037900     03  W-START-PER-AA      PIC S9(3)               COMP-3.              
038000     03  W-START-PER-PP      PIC S9(3)               COMP-3.              
038100     03  WS-AAPP             PIC 9(4)    VALUE ZERO.                      
038200     03  FILLER REDEFINES    WS-AAPP.                                     
038300         05 WS-AA            PIC 9(2).                                    
038400         05 WS-PP            PIC 9(2).                                    
038500     03  TIDISPIN-AAMMDD     PIC 9(6) VALUE ZERO.                         
038600                                                                          
038700     03  WS-TIAAVVD-GRP.                                                  
038800         05 WS-TIAAVV-GRP.                                                
038900            07 WS-TIAA-VECKA         PIC S9(2).                           
039000            07 WS-TIIVV              PIC S9(2).                           
039100         05 WS-TIAAVV                REDEFINES WS-TIAAVV-GRP              
039200                                     PIC 9(4).                            
039300         05 WS-TID                   PIC 9.                               
039400     03  WS-TIAAVVD-GRP-R            REDEFINES WS-TIAAVVD-GRP             
039500                                     PIC 9(5).                            
039600                                                                          
039700     03  WS-DAGENS-DATUM             PIC 9(6).                            
039800     03  W-ANT-ARTIKLAR-I-SATS                                            
039900                             PIC S9(5)               COMP-3.              
040000     03  WS-TILEVDAG  OCCURS 5    PIC 9.                                  
040100     03  WS-KVAVROP   OCCURS 5    PIC S9(7) COMP-3.                       
040200     03  ANT-LEVDAG               PIC 9(3).                               
040300     03  W-TIAAMMDD-AVS           PIC 9(6).                               
040400     03  WS-KVAVROP-DIFF          PIC S9(7) COMP-3.                       
040500     03  PASS-TILEVDAG            PIC 9.                                  
040600     03  WS-IDLANDX2              PIC X(2)    VALUE SPACE.                
040700                                                                          
040800*    03  WDGX2216    -COPY WDGX2216 -PRE W-.                              
040900     EJECT                                                                
041000*                                          REG-INFO FÖR KONTOLL AV        
041100*                                          UPPDAT.DATA REDIGERING         
041200*                                          AV UPPD. BILD                  
041300 01  W-REGISTER-INFO.                                                     
041400     03  W-MATINFO-KVDAGAR-TT                                             
041500                             PIC S9(3)               COMP-3.              
041600     03  W-MATINFO-KVVECKOR-LT                                            
041700                             PIC S9(3)               COMP-3.              
041800     03  W-MATINFO-KDVVKL    PIC S9(1)               COMP-3.              
041900     03  W-MATINFO-TILPSP    PIC S9(5)               COMP-3.              
042000     03  W-MATINFO-IDANSK    PIC S9(3)               COMP-3.              
042100     03  W-MATINFO-ADLAGOMR  PIC S9(3)               COMP-3.              
042200     03  W-MATINFO-KDEFFMAN  PIC X.                                       
042300     03  W-MATINFO-KVDAGAR-INLEV                                          
042400                             PIC S9(3)               COMP-3.              
042500     03  W-MATINFO-KDAVT     PIC S9(3)               COMP-3.              
042600     03  W-MATINFO-KDLPSP    PIC S9(1)               COMP-3.              
042700     03  W-MATINFO-KDHF      PIC S9(1)               COMP-3.              
042800     03  W-MATINFO-KDLEVPLF  PIC X.                                       
042900     03  W-EKINFO-PRARTSTD   PIC S9(7)V9(2)          COMP-3.              
043000     03  W-LEVNR-KVBR        PIC S9(7)               COMP-3.              
043100     03  W-OMSPEC-TISPECST   PIC S9(7)               COMP-3.              
043200     03  W-OMSPEC-KVBEST-PL  PIC S9(7)               COMP-3.              
043300     03  W-OMSPEC-KDPLKOEP   PIC S9(1)               COMP-3.              
043400     SKIP3                                                                
043500 01  DYNAMISKA-SUBPROGRAM.                                                
043600     03  CBLTDLI             PIC X(8)   VALUE 'CBLTDLI '.                 
043700     03  FELLOG              PIC X(8)   VALUE 'FELLOG  '.                 
043800     03  WDATKONV            PIC X(8)   VALUE 'WDATKONV'.                 
043810     03  WZ20DAYS            PIC X(8)   VALUE 'WZ20DAYS'.                 
043900     03  ABEND               PIC X(8)   VALUE 'ABEND   '.                 
044000     03  WORKDAY             PIC X(8)   VALUE 'WORKDAY '.                 
044100     03  W005INIT            PIC X(8)   VALUE 'W005INIT'.                 
044200     03  WMEDKONV            PIC X(8)   VALUE 'WMEDKONV'.                 
044300     03  W221LPAD            PIC X(8)   VALUE 'W221LPAD'.                 
044400     EJECT                                                                
044500*01  -COPY WORKAREA                                                       
044600     EJECT                                                                
044602                                                                          
044603 01  FILLER                  PIC X(16)   VALUE 'WZ20DAYS   '.             
044604*   -COPY WZ20DAYS                                                        
044610     EJECT                                                                
044700*      PARAMETRAR TILL ABEND                                              
044800 01 RKOD-ABEND               PIC S9(4)  VALUE +33 COMP SYNC.              
044900 01 RKOD-ABEND-66            PIC S9(4)  VALUE +66 COMP SYNC.              
045000     EJECT                                                                
045100*    --- VARIABLER TILL SUBPROGRAM W221LPAD                               
045200 01  W-W221LP-CTX            PIC X(08) VALUE 'W221LP01'.                  
045300 01  W-KDLPORS-GRP.                                                       
045400     03 W-KDLPORS-TAB OCCURS 4 PIC 9(3).                                  
045500     SKIP3                                                                
045600*    --- PARAMETRAR TILL SUBPROGRAM WMEDKONV                              
045700*01 -COPY WMEDAREA                                                        
045800 01  MESSAGE-CODES.                                                       
045900     03  INF-SUPPL-BLOCKED       PIC X(3)    VALUE '438'.                 
046000     EJECT                                                                
046100*---------------------------------------------------------------          
046200 01      FELMEDDELANDE.                                                   
046300   03    FEL-2           PIC X(40)                                        
046400                VALUE 'VOLKSWAGEN - AVS.VECKA 4-8-12-16 O.S.V. '.         
046500   03    FEL-3           PIC X(20)  VALUE 'KÖP SAKNAS          '.         
046600   03    FEL-4           PIC X(40)                                        
046700                VALUE 'NYCKLAR IFYLLDA, INGEN UPPDATERING      '.         
046800   03    FEL-5           PIC X(20)  VALUE 'UPPLYSTA FÄLT FEL   '.         
046900   03    FEL-6           PIC X(20)  VALUE 'LP-FÖRSLAG SAKNAS   '.         
047000   03    FEL-7           PIC X(20)  VALUE 'LEVERANSPLAN SAKNAS '.         
047100   03    FEL-8           PIC X(20)  VALUE 'GÄLLANDE PLAN SAKNAS'.         
047200   03    FEL-9           PIC X(20)  VALUE 'REGISTER UPPDATERADE'.         
047300   03    FEL-10          PIC X(40)                                        
047400                VALUE 'ARTIKEL SAKNAS I DATABASEN              '.         
047500   03    FEL-11          PIC X(20) VALUE 'STANDARDPRIS SAKNAS '.          
047600   03    FEL-12          PIC X(38) VALUE                                  
047700                       'AVROP BEORDRAT - ÄNDRING VIA BILD 2303'.          
047800   03    FEL-13          PIC X(32) VALUE                                  
047900                       'INNEHÅLLER FÄRRE ÄN TVÅ ARTIKLAR'.                
048000   03    FEL-14          PIC X(32) VALUE                                  
048100                       'ARTIKEL SAKNAS I RASA'.                           
048400   03    FEL-16          PIC X(32) VALUE                                  
048500                       'ARTIKELN HAR UTGÅTT  '.                           
048600   03    FEL-17          PIC X(32) VALUE                                  
048700                       'FEL PAPPKOD          '.                           
048800   03    FEL-18          PIC X(32) VALUE                                  
048900                       'LEVERANTÖRNUMMER FELAKTIGT'.                      
049000   03    FEL-19          PIC X(55) VALUE                                  
049100           'EJ TILLÅTIT ATT TA BORT SLÄP NÄR SATS ORDER FINNS'.           
049200   03    FEL-20          PIC X(32) VALUE                                  
049300                       'LEVERANTÖR SAKNAS PÅ BILD 2114'.                  
049400   03    MED-3           PIC X(30)                                        
049500                         VALUE 'AVROP PÅ LEVERANTÖRS HELGDAG'.            
049600*  ENGELSK TEXT                                                           
049700   03    FEL-102         PIC X(40)                                        
049800                VALUE 'VOLKSWAGEN - SEND.WEEK 4-8-12-16 ETC.   '.         
049900   03    FEL-103         PIC X(20)  VALUE 'PURCHASE MISSING    '.         
050000   03    FEL-104         PIC X(40)                                        
050100                VALUE 'KEYS SPECIFIED ,    NO UPDATE           '.         
050200   03    FEL-105         PIC X(20)  VALUE 'HIGHLIGHT FIELD ERR.'.         
050300   03    FEL-106         PIC X(20)  VALUE 'DP-PROPOSAL MISSING '.         
050400   03    FEL-107         PIC X(20)  VALUE 'DELIVERYPLAN MISSING'.         
050500   03    FEL-108         PIC X(20)  VALUE 'EXISTING PLAN MISS. '.         
050600   03    FEL-109         PIC X(20)  VALUE 'DATA BASES UPDATED  '.         
050700   03    FEL-110         PIC X(40)                                        
050800                VALUE 'PART NO. MISSING IN THE DATA BASE       '.         
050900   03    FEL-111         PIC X(20) VALUE 'STANDARD PRICE MISS.'.          
051000   03    FEL-112         PIC X(38) VALUE                                  
051100                       'ARREA ORDERED  - CHANGE IN SCREEN 2303'.          
051200   03    FEL-113         PIC X(32) VALUE                                  
051300                       'CONTAINS LESS THAN TWO PARTS    '.                
051400   03    FEL-114         PIC X(32) VALUE                                  
051500                       'PART MISSING IN  RASA'.                           
051800   03    FEL-116         PIC X(32) VALUE                                  
051900                       'PART NO. EXPIRED     '.                           
052000   03    FEL-117         PIC X(32) VALUE                                  
052100                       'INVALID PAPPKOD      '.                           
052200   03    FEL-118         PIC X(32) VALUE                                  
052300                       'ERRONEOUS SUPPLIER NO.'.                          
052400   03    FEL-119         PIC X(55) VALUE                                  
052500           'NOT ALLOWED TO REMOVE ARREAR WHEN KIT ORDER EXIST'.           
052600   03    FEL-120         PIC X(32) VALUE                                  
052700                       'SUPPLIER MISSING ON SCREEN 2114'.                 
052800   03    MED-13          PIC X(30)                                        
052900                         VALUE 'CALL ON SUPPLIER HOLLIDAY   '.            
053000     EJECT                                                                
053100*    -COPY W200EMAB.                                                      
053200     EJECT                                                                
053300*01  -COPY W221W005.                                                      
053400     EJECT                                                                
053500*                                          START-VECKA I PER 1-12         
053600*                                          ING  1-12 - AKTUELLT ÅR        
053700*                                          ING 13-24 - NÄSTA ÅR           
053800 01  PERIODINDELNING.                                                     
053900     03  PERIOD-TAB          OCCURS 24.                                   
054000         05  PER-START-VV                                                 
054100                             PIC  9(3)               COMP-3.              
054200                                                                          
054300         05  PER-SLUT-VV                                                  
054400                             PIC S9(3)               COMP-3.              
054500                                                                          
054600         05  PER-ANT-VV                                                   
054700                             PIC  9(1).                                   
054800                                                                          
054900         05  PER-AAPP        PIC 9(4).                                    
055000         05  FILLER          REDEFINES PER-AAPP.                          
055100             07  PER-AAPP-AA PIC 9(2).                                    
055200             07  PER-AAPP-PP PIC 9(2).                                    
055300     EJECT                                                                
055400*                            **                                  *        
055500*                            **  FÄLTMARKERINGS-AREA             *        
055600*                            **                                  *        
055700*                                                                         
055800     03  FILLER              PIC X(16)   VALUE 'FAELTMARKERINGAR'.        
055900     03  FAELTTAB.                                                        
056000         05  KOMKOD          PIC X.                                       
056100         05  FILLER          PIC X       VALUE 'N'.                       
056200         05  TIAVROP-AVS1    PIC X.                                       
056300         05  FILLER          PIC X       VALUE 'N'.                       
056400         05  KVAVROP1        PIC X.                                       
056500         05  FILLER          PIC X       VALUE 'N'.                       
056600         05  TIAVROP-AVS2    PIC X.                                       
056700         05  FILLER          PIC X       VALUE 'N'.                       
056800         05  KVAVROP2        PIC X.                                       
056900         05  FILLER          PIC X       VALUE 'N'.                       
057000         05  TIAVROP-AVS3    PIC X.                                       
057100         05  FILLER          PIC X       VALUE 'N'.                       
057200         05  KVAVROP3        PIC X.                                       
057300         05  FILLER          PIC X       VALUE 'N'.                       
057400         05  TIAVROP-AVS4    PIC X.                                       
057500         05  FILLER          PIC X       VALUE 'N'.                       
057600         05  KVAVROP4        PIC X.                                       
057700         05  FILLER          PIC X       VALUE 'N'.                       
057800         05  KVBEST-PL       PIC X.                                       
057900         05  FILLER          PIC X       VALUE 'N'.                       
058000         05  KDOMSPEC        PIC X.                                       
058100         05  FILLER          PIC X       VALUE 'N'.                       
058200         05  TILPSP          PIC X.                                       
058300         05  FILLER          PIC X       VALUE 'N'.                       
058400         05  KDLEVPLF        PIC X.                                       
058500         05  FILLER          PIC X       VALUE SPACE.                     
058800         05  FLJIT           PIC X.                                       
058900         05  FILLER          PIC X       VALUE SPACE.                     
059000*                                                                         
059100     03  FILLER              REDEFINES FAELTTAB.                          
059200         05  FAELTGRUPP      OCCURS  14.                                  
059300             07  FAELTTAB-FAELT                                           
059400                             PIC X.                                       
059500             07  FAELTTAB-KDDATTYP                                        
059600                             PIC X.                                       
059700*                                                                         
059800     03  FAELTTAB-IX         PIC S9(9)   VALUE +0    COMP SYNC.           
059900     03  FAELTTAB-IX-MAX     PIC S9(9)   VALUE +14   COMP SYNC.           
060000                                                                          
060100     EJECT                                                                
060200*    -COPY W221FLEV                                                       
060300     EJECT                                                                
060400*01      -COPY WDATAREA.                                                  
060500     EJECT                                                                
060600 01  TP-WS.                                                               
060700   03    FILLER              PIC X(16)   VALUE '   TP - AREAOR  '.        
060800     SKIP3                                                                
060900*01  MID       -COPY W2I10301  -PRE MID-.                                 
061000     EJECT                                                                
061100 01  FILLER                      PIC X(16)   VALUE 'WMSGINIT'.            
061200*01 -COPY WMSGINIT                                                        
061300     EJECT                                                                
061400 01  FILLER                      PIC X(16) VALUE '2147-SPAR-AREA'.        
061500*      *****  SPAR-AREA i MSGI-USEA från 2147-transen, som                
061600*      *****  håller SPAR-värden för bläddring på 2103.                   
061700 01  -COPY WW20147S.                                                      
061800     EJECT                                                                
061900 01  FILLER                      PIC X(16)  VALUE 'MSG-KOM-AREA'.         
062000     SKIP3                                                                
062100*01  -COPY WMSGKOM                                                        
062200     EJECT                                                                
062300*01  -COPY WMSGAREA.                                                      
062400     EJECT                                                                
062500*    03  MOD   -COPY W2O10301  -PRE MOD-  -RED MSG-AREA.                  
062600     EJECT                                                                
062700*01  -COPY WMFSAREA.                                                      
062800     EJECT                                                                
062900 01  FILLER                      PIC X(16)   VALUE 'P-TO-P-AREA'.         
063000 01      P-TO-P-SW.                                                       
063100                                                                          
063200  02     P-TO-P-KVLL             PIC S9(4)           COMP SYNC.           
063300  02     P-TO-P-KDZ1             PIC X(1)  VALUE LOW-VALUE.               
063400  02     P-TO-P-KDZ2             PIC X(1)  VALUE LOW-VALUE.               
063500  02     P-TO-P-KDTRANS          PIC X(8).                                
063600  02     P-TO-P-IDTRANS          PIC X(4).                                
063700  02     P-TO-P-KDMFSFOR         PIC X(1).                                
063800  02     P-TO-P-DATA             PIC X(1000).                             
063900     EJECT                                                                
064000*01  -COPY W2I13501   -PRE MOD2135-MID-                                   
064100*                            **                                  *        
064200*                            **  ARBETSAREOR TILL F IMS-KTIONER  *        
064300*                            **                                  *        
064400 01  IMS-WS.                                                              
064500   03    FILLER              PIC X(16)   VALUE '     IMS-WS     '.        
064600     SKIP3                                                                
064700*                                          STATUSKOD FRÅN IMS             
064800   03    STATUS-WS           PIC X(2).                                    
064900         88  SEGMENT-FINNS               VALUE '  '.                      
065000         88  SEGMENT-SAKNAS              VALUE 'GE'.                      
065100         88  SEGMENT-FINNS-REDAN         VALUE 'II'.                      
065200     SKIP3                                                                
065300   03    GODK-STATUSKODER.                                                
065400     05  GODK-STATUS OCCURS 3 INDEXED BY STATUS-IX PIC X(2).              
065500     SKIP3                                                                
065600 01      SSA1                PIC X(128).                                  
065700 01      SSA2                PIC X(64).                                   
065800 01      SSA3                PIC X(64).                                   
065900 01      SSA4                PIC X(64).                                   
066000 01      SSA5                PIC X(64).                                   
066100     SKIP3                                                                
066200*                                          IMS FUNKTIONSKODER             
066300*01  -COPY W0003                                                          
066400     EJECT                                                                
066500 01  FILLER                 PIC X(16)   VALUE 'DLI-IO-WDK601'.            
066600 01  DLI-IO-WDK601.                                                       
066900*    03  -COPY WDK601                                                     
067000     EJECT                                                                
067010 01  FILLER                 PIC X(16)   VALUE 'DLI-IO-WDK611'.            
067100 01  DLI-IO-WDK611.                                                       
067400*    03  -COPY WDK611                                                     
067500     EJECT                                                                
067501 01  FILLER                 PIC X(16)   VALUE 'DLI-IO-WDK622'.            
067502 01  DLI-IO-WDK622.                                                       
067503*    03  -COPY WDK622                                                     
067504     EJECT                                                                
067510 01  FILLER                 PIC X(16)   VALUE 'DLI-IO-WDK621'.            
067520 01  DLI-IO-WDK621.                                                       
067700*    03  -COPY WDK621                                                     
067800     EJECT                                                                
067810 01  FILLER                 PIC X(16)   VALUE 'DLI-IO-WDK623'.            
067900 01  DLI-IO-WDK623.                                                       
068200*    03  -COPY WDK623                                                     
068300     EJECT                                                                
068310 01  FILLER                 PIC X(16)   VALUE 'DLI-IO-WDK625'.            
068400 01  DLI-IO-WDK625.                                                       
068700*    03  -COPY WDK625                                                     
068800     EJECT                                                                
068900 01  DLI-IO-AREA.                                                         
069000     03  IO-AREA            PIC X(200)  VALUE SPACE.                      
069100     SKIP3                                                                
069200*    03  WLINLB01  -COPY WDD901 -PRE WDD901-   -RED IO-AREA.              
069300     EJECT                                                                
069400*    03  WLINLB11  -COPY WDD902 -PRE LEVNR-    -RED IO-AREA.              
069500     EJECT                                                                
069600*    03  WLINLB22  -COPY WDD904 -PRE OMSPEC-   -RED IO-AREA.              
069700     EJECT                                                                
069800*    03  WLINLB23  -COPY WDD905 -PRE AVROP-    -RED IO-AREA.              
069900     EJECT                                                                
070000*    03  WLINLB31  -COPY WDD906 -PRE WDD906-   -RED IO-AREA.              
070100     EJECT                                                                
070200*    03  WLINLB32  -COPY WDD907 -PRE BEORD-    -RED IO-AREA.              
070300     EJECT                                                                
070600*    03  WDGX2216  -COPY WDGX2216              -RED IO-AREA.              
070700     EJECT                                                                
070800*    03  WDGX2218  -COPY WDGX2218              -RED IO-AREA.              
070900     EJECT                                                                
071000*    03  WLXXBJ01  -COPY WDG32201 -PRE 2201-   -RED IO-AREA.              
071100     EJECT                                                                
071200*    03  WLXXBJ11  -COPY WDGX2204              -RED IO-AREA.              
071300     EJECT                                                                
071400 01  FILLER                 PIC X(16)   VALUE 'IO-AREA2'.                 
071500 01  DLI-IO-AREA2.                                                        
071600     03  IO-AREA2           PIC X(40)  VALUE SPACE.                       
071700     SKIP3                                                                
071800*    03 WLXXBW01 INGEN COPYTEXT   TOM ROT                                 
071900*    03  WLXXBW11  -COPY WDGX2228              -RED IO-AREA2.             
072000     EJECT                                                                
072100 01  FILLER                 PIC X(16)   VALUE 'IO-AREA3'.                 
072200 01  DLI-IO-AREA3.                                                        
072300     03  IO-AREA3           PIC X(40)  VALUE SPACE.                       
072400     SKIP3                                                                
072500*    03  WLXXCZ11 -COPY WDGX2246 -PRE XXCZ-   -RED IO-AREA3.              
072600     EJECT                                                                
072700 01  DLI-IO-AREA4        PIC X(400)  VALUE SPACE.                         
072800     SKIP3                                                                
072900*01  WLSATB01 -COPY WDJ101   -PRE SATB-     -RED DLI-IO-AREA4.            
073000     EJECT                                                                
073100*01  WLSATB11 -COPY WDJ111   -PRE SATB-     -RED DLI-IO-AREA4.            
073200     EJECT                                                                
073300 01  FILLER                 PIC X(16)   VALUE 'IO-AREA9'.                 
073400 01  DLI-IO-AREA9.                                                        
073500     03  IO-AREA9           PIC X(40)  VALUE SPACE.                       
073600     SKIP3                                                                
073700*    03  WLINLB23  -COPY WDD905 -PRE SLASK-    -RED IO-AREA9.             
073800     EJECT                                                                
073900                                                                          
074202 01  FILLER                  PIC X(16)  VALUE 'DLI-IO-WDD902'.            
074203 01  DLI-IO-WDD902.                                                       
074204*    03  -COPY WDD902 -PRE 902-LEVNR-                                     
074205     EJECT                                                                
074206 01  FILLER                  PIC X(16)  VALUE 'DLI-IO-WDD904'.            
074207 01  DLI-IO-WDD904.                                                       
074208*    03  -COPY WDD904 -PRE 904-OMSPEC-                                    
074209     EJECT                                                                
074210 01  FILLER                 PIC X(16)   VALUE 'IO-AREA906'.               
074220 01  DLI-IO-AREA906.                                                      
074230     03  -COPY WDD906                                                     
074240     EJECT                                                                
074300 01  FILLER                 PIC X(16)   VALUE 'IO-AREA907'.               
074400 01  DLI-IO-AREA907.                                                      
074500     03  -COPY WDD907                                                     
074600     EJECT                                                                
074700 01  DLI-IO-AREA-F1          PIC X(100).                                  
074800     SKIP2                                                                
074900*01  WLLEVA01 -COPY WDF101     -PRE F1-       -RED DLI-IO-AREA-F1         
075000     EJECT                                                                
075100 01  FILLER                  PIC X(16)  VALUE 'DLI-IO-AREA-F106'.         
075200     SKIP3                                                                
075300 01  DLI-IO-AREA-F106.                                                    
075400     SKIP2                                                                
075500*    03  WLLEVA14 -COPY WDF106                                            
075600     EJECT                                                                
075700 01  DLI-IO-AREA-K7          PIC X(300).                                  
075800     SKIP2                                                                
075900*01  WLARTS01 -COPY WDK701     -PRE K7-       -RED DLI-IO-AREA-K7         
076000     EJECT                                                                
076100*01  WLARTS11 -COPY WDK711                    -RED DLI-IO-AREA-K7         
076200     EJECT                                                                
076300 01  FILLER                  PIC X(16)  VALUE 'DLI-IO-AREA-F301'.         
076400     SKIP3                                                                
076500 01  DLI-IO-AREA-F301.                                                    
076600     SKIP2                                                                
076700*    03  -COPY WDF301                                                     
076800     EJECT                                                                
077500 01  FILLER                  PIC X(16)  VALUE 'DLI-IO-WDD601'.            
077600 01  DLI-IO-WDD601.                                                       
077700*    03  -COPY WDD601                                                     
077800     EJECT                                                                
078400 01  FILLER                  PIC X(16)  VALUE 'DLI-IO-WDGX2258'.          
078500 01  DLI-IO-WDGX2258.                                                     
078600*    03 -COPY WDGX2258                                                    
078700     EJECT                                                                
078800 01  FILLER                  PIC X(16)  VALUE 'DLI-IO-WDGX2260'.          
078900 01  DLI-IO-WDGX2260.                                                     
079000*    03 -COPY WDGX2260                                                    
079100     EJECT                                                                
079200                                                                          
079300 01  FILLER                  PIC X(16) VALUE 'DLI-IO-WDB601'.             
079400 01  DLI-IO-WDB601.                                                       
079500*    03  -COPY WDB601                                                     
079600     EJECT                                                                
080100                                                                          
080110 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDGX2223'.                    
080120 01  DLI-IO-WDGX2223.                                                     
080130*    03  -COPY WDGX2223                                                   
080140     EJECT                                                                
080150                                                                          
080160 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDGX2224'.                    
080170 01  DLI-IO-WDGX2224.                                                     
080180*    03  -COPY WDGX2224                                                   
080190     EJECT                                                                
080195 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDGX2232'.                    
080196 01  DLI-IO-WDGX2232.                                                     
080197*    03  -COPY WDGX2232                                                   
080198     EJECT                                                                
080200 01  FILLER                  PIC X(16) VALUE 'DLI-IO-WDJ201'.             
080201 01  DLI-IO-WDJ201.                                                       
080202*    03  -COPY WDJ201                                                     
080203     EJECT                                                                
080210                                                                          
080300 LINKAGE SECTION.                                                         
080400*01  -COPY W0009 -PRE MSG-                                                
080500     EJECT                                                                
080600*01  -COPY W0009 -PRE MSGKOM-                                             
080700                                                                          
080800*01  -COPY W0009   -PRE ALT1-                                             
080900     EJECT                                                                
081000*01  -COPY W0008 -PRE WDK6-                                               
081100     05  FILLER                PIC X.                                     
081200                                                                          
081300*01  -COPY W0008 -PRE INLB-                                               
081400     05  FILLER                PIC X.                                     
081500     EJECT                                                                
081600*01  -COPY W0008 -PRE XXBJ-.                                              
081700     05  FILLER              PIC X.                                       
081800                                                                          
081900*01  -COPY W0008 -PRE XXBL-.                                              
082000     05  FILLER              PIC X.                                       
082100     EJECT                                                                
082200*01  -COPY W0008 -PRE XXBK-.                                              
082300     05  FILLER              PIC X.                                       
082400                                                                          
082500*01  -COPY W0008 -PRE XXBW-.                                              
082600     05  FILLER              PIC X.                                       
082700     EJECT                                                                
082800*01  -COPY W0008 -PRE SATB-.                                              
082900     05  FILLER              PIC X.                                       
083000     EJECT                                                                
083400*01  -COPY W0008 -PRE XXCZ-.                                              
083500     05  FILLER              PIC X.                                       
083600                                                                          
083700*01  -COPY W0008 -PRE WDF1-.                                              
083800     05  FILLER              PIC X.                                       
083900                                                                          
084000*01  -COPY W0008 -PRE WDK7-.                                              
084100     05  FILLER              PIC X.                                       
084200     EJECT                                                                
084300*01  -COPY W0008  -PRE WDF3-.                                             
084400     05  FILLER                  PIC X.                                   
084500     EJECT                                                                
084600*01  -COPY W0008  -PRE WDD6-                                              
084700     05  FILLER                  PIC X.                                   
084800     EJECT                                                                
084900*01  -COPY W0008  -PRE WDP7-                                              
085000     05  FILLER                  PIC X.                                   
085100     EJECT                                                                
085200*01  -COPY W0008  -PRE WDD9-                                              
085300     05  FILLER                  PIC X.                                   
085400     EJECT                                                                
085500*01  -COPY W0008  -PRE XXBM-                                              
085600     05  FILLER                  PIC X.                                   
085700     EJECT                                                                
085800*01  -COPY W0008  -PRE 2257-                                              
085900     05  FILLER                  PIC X.                                   
086000     EJECT                                                                
086100*01  -COPY W0008  -PRE WDB6-                                              
086200     05  FILLER                  PIC X.                                   
086300     EJECT                                                                
086310*01  -COPY W0008  -PRE WDJ2-                                              
086320     05  FILLER                  PIC X.                                   
086330     EJECT                                                                
086400*01  -COPY W0008  -PRE WDR5-                                              
086500     05  FILLER                  PIC X.                                   
086600     EJECT                                                                
086610*01  -COPY W0008  -PRE WDR2-                                              
086620     05  FILLER                  PIC X.                                   
086630     EJECT                                                                
086700 PROCEDURE DIVISION USING MSG-PCB MSGKOM-PCB ALT1-PCB WDK6-PCB            
086800           INLB-PCB XXBJ-PCB XXBL-PCB XXBK-PCB XXBW-PCB                   
086910           SATB-PCB XXCZ-PCB WDF1-PCB WDK7-PCB                            
087000           WDF3-PCB WDD6-PCB WDP7-PCB WDD9-PCB XXBM-PCB                   
087100           2257-PCB WDB6-PCB WDJ2-PCB WDR5-PCB WDR2-PCB.                  
087200 MAIN SECTION.                                                            
087300     ENTRY 'DLITCBL' USING MSG-PCB MSGKOM-PCB ALT1-PCB WDK6-PCB           
087400            INLB-PCB XXBJ-PCB XXBL-PCB XXBK-PCB XXBW-PCB                  
087510            SATB-PCB XXCZ-PCB WDF1-PCB WDK7-PCB                           
087600            WDF3-PCB WDD6-PCB WDP7-PCB WDD9-PCB XXBM-PCB                  
087700            2257-PCB WDB6-PCB WDJ2-PCB WDR5-PCB WDR2-PCB.                 
087800                                                                          
087900     PERFORM IMS-GET-MSG                                                  
088000     IF SEGMENT-FINNS                                                     
088100       PERFORM IMS-GET-WMSGKOM                                            
088200       PERFORM A-KONTROLL-NYCKLAR-OCH-INIT                                
088300       PERFORM MFS-ROER-EJ-UTDATA-FAELT                                   
088400       IF INDATA-OK                                                       
088500         MOVE IDARTNR-WS TO W-IDARTNR     W-IDARTNR-H                     
088600                            W-IDARTNR-MIN W-IDARTNR-MAX                   
088700         MOVE IDLEVNR-WS TO W-IDLEVNR                                     
088800                            W-IDLEVNR-MIN W-IDLEVNR-MAX                   
088900         MOVE KDBEHX-PLAN-WS TO W-KDBEHX-PLAN                             
089000         MOVE MFS-ADD-SAETT-CURSOR TO MOD-IDARTNR-IN-ATTR                 
089100         PERFORM B-KONTROLL-INDATA-FAELT                                  
089200         IF INDATA-OK                                                     
089300           IF SW-OMSPEC-BEG    = NEJ                                      
089400              PERFORM C-UPPDATERA-REGISTER-BILD                           
089500                                                                          
089600              IF FLUPPD-2228 = JA                                         
089700                PERFORM S19-UPPDATERA-WDR5                                
089800              END-IF                                                      
089900                                                                          
090000              IF MOD-MESSAGE = SPACE                                      
090200                IF ENGLISH-TEXT                                           
090300                   MOVE FEL-109  TO MOD-MESSAGE                           
090400                ELSE                                                      
090500                   MOVE FEL-9    TO MOD-MESSAGE                           
090600                END-IF                                                    
090700                MOVE '101'       TO MSG-KOM-IDMFSMED                      
090800              END-IF                                                      
090900           ELSE                                                           
091000              PERFORM E-STARTA-OMSPEC-2135-TRANS                          
091100           END-IF                                                         
091200         END-IF                                                           
091300       ELSE                                                               
091400         PERFORM MFS-ROER-EJ-INDATA-FAELT                                 
091500       END-IF                                                             
091600       IF MFS-UPD-X                                                       
091700         IF MSG-KOM-IDMFSMED = SPACE                                      
091800            MOVE '101' TO MSG-KOM-IDMFSMED                                
091900         END-IF                                                           
092000         PERFORM IMS-INSERT-WMSGKOM                                       
092100       ELSE                                                               
092200          IF SW-OMSPEC-BEG    = NEJ OR INDATA-FEL                         
092300             PERFORM IMS-INSERT-MSG                                       
092400          END-IF                                                          
092500       END-IF                                                             
092600     END-IF                                                               
092700     MOVE ZERO TO RETURN-CODE                                             
092800     GOBACK                                                               
092900     .                                                                    
093000     EJECT                                                                
093100 A-KONTROLL-NYCKLAR-OCH-INIT SECTION.                                     
093110     MOVE 'A-KONTROLL-NYCKLAR-OCH-INIT' TO CURRENT-SECTION                
093200                                                                          
093300     MOVE JA TO SW-INDATA                                                 
093400     IF MSG-DUBBLA-TRANSKODER                                             
093500       MOVE MSG-INDATA-MINUS-2-TRANSKODER TO MID-W2I10301                 
093600       MOVE MSG-IDTRANS-2                 TO MFS-IDTRANS                  
093700       MOVE MSG-KDMFSFOR-2                TO MFS-KDMFSFOR                 
093800     ELSE                                                                 
093900       MOVE MSG-INDATA-MINUS-1-TRANSKOD TO MID-W2I10301                   
094000       MOVE MSG-IDTRANS-1               TO MFS-IDTRANS                    
094100       MOVE MSG-KDMFSFOR-1              TO MFS-KDMFSFOR                   
094200     END-IF                                                               
094300     MOVE   MSG-KDTRTYP                 TO MFS-KDTRTYP                    
094400                                                                          
094500     MOVE SPACE                         TO MSG-KOM-IDMFSMED               
094600                                                                          
094700     MOVE ZERO TO W-IDLOGLOP                                              
094800     MOVE LOW-VALUE  TO MOD-W2O10301                                      
094900     MOVE '2103'     TO MOD-IDTRANS                                       
095000     MOVE 'W2O10301' TO MFS-IDMOD                                         
095100     MOVE MAX-MOD-LENGD TO MSG-KVLL                                       
095200                                                                          
095300     MOVE MFS-RENSA-FAELT TO MOD-IDARTNR-IN                               
095400                             MOD-IDLEVNR-IN                               
095500                             MOD-KDBEHX-PLAN-IN                           
095710     MOVE SPACE        TO    MOD-MESSAGE                                  
095720                             MOD-MESSAGE-BOTTOM                           
095800                                                                          
095900     IF MID-IDARTNR-IN = ALL '+'                                          
096000       MOVE MID-IDARTNR-UT TO IDARTNR-WS                                  
096100       INSPECT IDARTNR-WS REPLACING LEADING SPACE BY ZERO                 
096200     ELSE                                                                 
096300       MOVE MID-IDARTNR-IN TO IDARTNR-WS                                  
096400       MOVE NEJ TO SW-INDATA                                              
096500     END-IF                                                               
096600     IF MID-IDLEVNR-IN = ALL '+'                                          
096700       MOVE MID-IDLEVNR-UT TO IDLEVNR-WS                                  
096800     ELSE                                                                 
096900       MOVE MID-IDLEVNR-IN TO IDLEVNR-WS                                  
097000       MOVE NEJ TO SW-INDATA                                              
097100     END-IF                                                               
097200                                                                          
097300     IF MID-KDBEHX-PLAN-IN = 'E'                                          
097400        MOVE 'G'               TO MID-KDBEHX-PLAN-IN                      
097500     END-IF                                                               
097600     IF MID-KDBEHX-PLAN-IN = 'P'                                          
097700        MOVE 'F'               TO MID-KDBEHX-PLAN-IN                      
097800     END-IF                                                               
097900     IF MID-KDBEHX-PLAN-UT = 'E'                                          
098000        MOVE 'G'               TO MID-KDBEHX-PLAN-UT                      
098100     END-IF                                                               
098200     IF MID-KDBEHX-PLAN-UT = 'P'                                          
098300        MOVE 'F'               TO MID-KDBEHX-PLAN-UT                      
098400     END-IF                                                               
098500                                                                          
098600     IF MID-KDBEHX-PLAN-IN = ALL '+'                                      
098700       MOVE MID-KDBEHX-PLAN-UT TO KDBEHX-PLAN-WS                          
098800     ELSE                                                                 
098900       MOVE MID-KDBEHX-PLAN-IN TO KDBEHX-PLAN-WS                          
099000       MOVE NEJ TO SW-INDATA                                              
099100     END-IF                                                               
099200                                                                          
099300     IF INDATA-FEL                                                        
099400       IF ENGLISH-TEXT                                                    
099500          MOVE FEL-104  TO MOD-MESSAGE                                    
099600       ELSE                                                               
099700          MOVE FEL-4    TO MOD-MESSAGE                                    
099800       END-IF                                                             
099900       MOVE '401' TO MSG-KOM-IDMFSMED                                     
100000       MOVE '2'   TO MSG-KOM-KDSVAR                                       
100100     END-IF                                                               
100200                                                                          
100300     IF MFS-IDTRANS NOT = '2103'                                          
100400       MOVE NEJ TO SW-INDATA                                              
100500       MOVE '034'        TO MSG-KOM-IDMFSMED                              
100600       MOVE '2'          TO MSG-KOM-KDSVAR                                
100700     END-IF                                                               
100800                                                                          
100900     PERFORM AA-DAGENS-DATUM                                              
101000     PERFORM AB-PERIODINDELNING-TAB                                       
101100                                                                          
101200     MOVE IDARTNR-WS TO MOD-IDARTNR-UT                                    
101300     INSPECT MOD-IDARTNR-UT REPLACING LEADING ZERO BY SPACE               
101400     MOVE IDLEVNR-WS TO MOD-IDLEVNR-UT                                    
101500                        MOD-IDLEVNR-SHIP-UT                               
101600     MOVE KDBEHX-PLAN-WS TO MOD-KDBEHX-PLAN-UT                            
101700                                                                          
101800     MOVE WC-CDC-SE      TO W-IDDC-MIN                                    
101900                            W-IDDC-MAX                                    
102000     .                                                                    
102100     EJECT                                                                
102200 AA-DAGENS-DATUM SECTION.                                                 
102210     MOVE 'AA-DAGENS-DATUM            ' TO CURRENT-SECTION                
102300                                                                          
102400     MOVE 'IDAG  ' TO DAT-KDDATFORM                                       
102500                                                                          
102600     CALL WDATKONV USING DAT-KDDATFORM                                    
102700                         DAT-I-TIDATUM                                    
102800                         DAT-O-TIDATUM                                    
102900                         DAT-KDSVAR                                       
103000                                                                          
103100     MOVE DAT-TIAAMMDD   TO WS-DAGENS-DATUM                               
103200     MOVE DAT-TIAA TO W-DATUM-AA                                          
103300                      W-START-PER-AA                                      
103400     MOVE DAT-TIRP TO W-START-PER-PP                                      
103500     MOVE DAT-TIVV TO W-DATUM-VV                                          
103600     MOVE DAT-TIAARP TO W-PERIOD-AAPP                                     
103700                       W-START-PER-AAPP                                   
103800     MOVE W-DATUM-AAVV TO W-DATUM-AKTUELLT                                
103900     MOVE DAT-TID      TO W-TID-AKTUELLT                                  
104000                                                                          
104100     ADD 1 TO W-DATUM-AA                                                  
104200     ADD 7 TO W-DATUM-VV                                                  
104300     IF  W-DATUM-VV > 52                                                  
104400       ADD 1 TO W-DATUM-AA                                                
104500       SUBTRACT 52 FROM W-DATUM-VV                                        
104600     END-IF                                                               
104700     MOVE W-DATUM-AAVV TO W-DATUM-AKT-PLUS59                              
104800     .                                                                    
104900     EJECT                                                                
105000 AB-PERIODINDELNING-TAB SECTION.                                          
105010     MOVE 'AB-PERIODINDELNING-TAB     ' TO CURRENT-SECTION                
105100                                                                          
105200*    --- FYLL I VECKONR FÖR PERIODERNA                                    
105300     MOVE W-START-PER-AA     TO WS-AA                                     
105400     MOVE 01                 TO WS-PP                                     
105500     MOVE 'AARP'             TO DAT-KDDATFORM                             
105600     MOVE +1                 TO IX-PP                                     
105700                                                                          
105800     PERFORM UNTIL IX-PP      >  24                                       
105900       IF WS-PP = 13                                                      
106000          MOVE +1             TO WS-PP                                    
106100          ADD  +1             TO WS-AA                                    
106200       END-IF                                                             
106300       MOVE WS-AAPP           TO DAT-I-TIDATUM                            
106400       CALL WDATKONV USING DAT-KDDATFORM DAT-I-TIDATUM                    
106500                             DAT-O-TIDATUM DAT-KDSVAR                     
106600       IF DAT-KDSVAR-OK                                                   
106700          MOVE DAT-TIVV         TO PER-START-VV (IX-PP)                   
106800          MOVE DAT-KVVIPER      TO PER-ANT-VV   (IX-PP)                   
106900          MOVE DAT-TIAARP       TO PER-AAPP     (IX-PP)                   
106901                                                                          
107000          COMPUTE PER-SLUT-VV (IX-PP) =                                   
107100                  PER-START-VV (IX-PP) + DAT-KVVIPER - 1                  
107110****************fix för period 12 2020                                    
107120          IF PER-AAPP(12) = 2012                                          
107130            MOVE 53   TO PER-SLUT-VV(12)                                  
107140          END-IF                                                          
107150**************************************                                    
107200       ELSE                                                               
107300          MOVE 'FELAKTIGT DATUM - DATKONV3' TO FELTEXT                    
107400          CALL FELLOG                                                     
107500       END-IF                                                             
107600       ADD +1                   TO WS-PP IX-PP                            
107700     END-PERFORM                                                          
107800                                                                          
107900     MOVE W-START-PER-AA TO W-PERIOD-AA                                   
108000     MOVE W-START-PER-PP TO W-PERIOD-PP                                   
108100     MOVE W-PERIOD-AAPP  TO W-PERIOD-AAPP-START                           
108200     MOVE 1 TO IY                                                         
108300                                                                          
108400     PERFORM UNTIL IY > MAX-ANT-PERIODER-I-TAB                            
108500       MOVE W-PERIOD-PP TO W-PERIOD-TAB-RAD (IY)                          
108600       IF W-PERIOD-PP = 12                                                
108700         ADD 1  TO W-PERIOD-AA                                            
108800         MOVE 1 TO W-PERIOD-PP                                            
108900       ELSE                                                               
109000         ADD 1  TO W-PERIOD-PP                                            
109100       END-IF                                                             
109200       ADD 1 TO IY                                                        
109300     END-PERFORM                                                          
109400     .                                                                    
109500     EJECT                                                                
109600 B-KONTROLL-INDATA-FAELT SECTION.                                         
109610     MOVE 'B-KONTROLL-INDATA-FAELT    ' TO CURRENT-SECTION                
109700                                                                          
109800     MOVE 1 TO FAELTTAB-IX                                                
109900     PERFORM UNTIL FAELTTAB-IX > FAELTTAB-IX-MAX                          
110000       MOVE EJ-IFYLLD TO FAELTTAB-FAELT (FAELTTAB-IX)                     
110100       ADD 1 TO FAELTTAB-IX                                               
110200     END-PERFORM                                                          
110300                                                                          
110400     PERFORM IMS-GU-WDK601                                                
110500     IF SEGMENT-FINNS                                                     
110600       IF ART-KDERS-UTG > ZERO                                            
110700         IF ART-KDERS-UTG > 20                                            
110800*        AND W-IDLEVNR = '1002'                                           
110900           CONTINUE                                                       
111000         ELSE                                                             
111100           MOVE NEJ TO SW-INDATA                                          
111200           IF ENGLISH-TEXT                                                
111300             MOVE FEL-116  TO MOD-MESSAGE                                 
111400           ELSE                                                           
111500             MOVE FEL-16   TO MOD-MESSAGE                                 
111600           END-IF                                                         
111700           MOVE '018' TO MSG-KOM-IDMFSMED                                 
111800           MOVE '2'   TO MSG-KOM-KDSVAR                                   
111900         END-IF                                                           
112000       ELSE                                                               
112100         PERFORM BA-LAES-REGINFO-FOR-KONTROLL                             
112200         PERFORM BN-KONTROLL-LEVNR                                        
112300         IF INDATA-OK                                                     
112400           IF W-IDLEVNR = '1002'                                          
112500             PERFORM BI-KONTROLL-SATSART                                  
112600           END-IF                                                         
112700         END-IF                                                           
112800       END-IF                                                             
112900       IF INDATA-OK                                                       
113000         PERFORM BB-KONTROLL-KOMKOD                                       
113100         PERFORM BC-KONTROLL-AVROPSDATA                                   
113200         PERFORM BE-KONTROLL-KOEP                                         
113300         PERFORM BF-KONTROLL-OMSPEC                                       
113400         PERFORM BG-KONTROLL-LPSP                                         
114100         PERFORM BL-KONTROLL-KDLEVPLF                                     
114300         PERFORM BJ-KONTROLL-FLJIT                                        
114310         PERFORM BH-KONTROLL-FAELTTAB                                     
114400                                                                          
114500         IF INDATA-FEL AND SW-FAELT-IFYLLT = JA                           
114600           IF SW-LEVSEGM-FINNS = JA                                       
114700             PERFORM MFS-SET-ATTR-FAELT-FEL                               
114800           ELSE                                                           
114900             IF  SW-HUVUDLEVERANTOER = JA                                 
115000             AND W-MATINFO-KDAVT > ZERO                                   
115100               MOVE MFS-ROER-EJ-FAELT TO MOD-KVBEST-PL-IN                 
115200               MOVE MFS-NUM-FAELT-FEL TO MOD-KVBEST-PL-IN-ATTR            
115300             END-IF                                                       
115400           END-IF                                                         
115500         ELSE                                                             
115600           IF SW-LEVSEGM-FINNS = NEJ AND SW-FAELT-IFYLLT = NEJ            
115700             IF  SW-HUVUDLEVERANTOER = JA                                 
115800             AND W-MATINFO-KDAVT > ZERO                                   
115900             AND W-KDBEHX-PLAN = GALLANDE                                 
116000               MOVE MFS-RENSA-FAELT TO MOD-KVBEST-PL-IN                   
116100               MOVE MFS-OEPPNA-NUM-FAELT TO MOD-KVBEST-PL-IN-ATTR         
116200             END-IF                                                       
116300           ELSE                                                           
116400             PERFORM MFS-OEPPNA-INIT-UPPDAT-FAELT                         
116500           END-IF                                                         
116600         END-IF                                                           
116700                                                                          
116800         IF INDATA-FEL                                                    
116900           MOVE MFS-ALFA-FAELT-RAETT  TO MOD-TEARTNOT1-IN-ATTR            
117000                                         MOD-TEARTNOT2-IN-ATTR            
117100           MOVE MFS-ROER-EJ-FAELT     TO MOD-TEARTNOT1-IN                 
117200                                         MOD-TEARTNOT2-IN                 
117210           IF MOD-MESSAGE = SPACE                                         
117300             IF ENGLISH-TEXT                                              
117400                MOVE FEL-105          TO MOD-MESSAGE                      
117500             ELSE                                                         
117600                MOVE FEL-5            TO MOD-MESSAGE                      
117700             END-IF                                                       
117701           END-IF                                                         
117720           MOVE '001'                 TO MSG-KOM-IDMFSMED                 
117730           MOVE '2'                   TO MSG-KOM-KDSVAR                   
118000         END-IF                                                           
118100       END-IF                                                             
118200     ELSE                                                                 
118300       MOVE NEJ TO SW-INDATA                                              
118400       IF ENGLISH-TEXT                                                    
118500          MOVE FEL-110  TO MOD-MESSAGE                                    
118600       ELSE                                                               
118700          MOVE FEL-10   TO MOD-MESSAGE                                    
118800       END-IF                                                             
118900       MOVE '010' TO MSG-KOM-IDMFSMED                                     
119000       MOVE '2'   TO MSG-KOM-KDSVAR                                       
119100     END-IF                                                               
119200     .                                                                    
119300     EJECT                                                                
119400 BN-KONTROLL-LEVNR SECTION.                                               
119410     MOVE 'BN-KONTROLL-LEVNR          ' TO CURRENT-SECTION                
119500                                                                          
119600     IF W-IDLEVNR = ART-IDLEVNR                                           
119700       IF CLAG-IDDC-REF NOT = SPACE                                       
119800         MOVE NEJ           TO SW-INDATA                                  
119900         IF ENGLISH-TEXT                                                  
120000           MOVE FEL-118     TO MOD-MESSAGE                                
120100         ELSE                                                             
120200           MOVE FEL-18      TO MOD-MESSAGE                                
120300         END-IF                                                           
120400         MOVE '092'         TO MSG-KOM-IDMFSMED                           
120500         MOVE '2'           TO MSG-KOM-KDSVAR                             
120600         PERFORM MFS-ROER-EJ-UTDATA-FAELT                                 
120700       END-IF                                                             
120800     ELSE                                                                 
120900       MOVE W-IDLEVNR     TO W-IDLEVNR-DC                                 
121000       PERFORM IMS-GU-WDB601-LEV                                          
121100       IF SEGMENT-FINNS                                                   
121200         IF DCS-NDC-CN                                                    
121300           MOVE NEJ       TO SW-INDATA                                    
121400           IF ENGLISH-TEXT                                                
121500             MOVE FEL-118 TO MOD-MESSAGE                                  
121600           ELSE                                                           
121700             MOVE FEL-18  TO MOD-MESSAGE                                  
121800           END-IF                                                         
121900           MOVE '092'     TO MSG-KOM-IDMFSMED                             
122000           MOVE '2'       TO MSG-KOM-KDSVAR                               
122100           PERFORM MFS-ROER-EJ-UTDATA-FAELT                               
122200         END-IF                                                           
122300       END-IF                                                             
122400     END-IF                                                               
122500     .                                                                    
122600     EJECT                                                                
122700 BA-LAES-REGINFO-FOR-KONTROLL SECTION.                                    
122710     MOVE 'BA-LAES-REGINFO-FOR-KONTROLL' TO CURRENT-SECTION               
122800                                                                          
122900*    DE SEGMENT LÄSES FRÅN REGISTRET SOM KRÄVS FÖR               *        
123000*    KONTROLL AV UPPDATERINGS-INDATA OCH FÖR REDIGERING AV       *        
123100*    BILD MED UPPDATERINGAR                                      *        
123200                                                                          
123300     IF ART-IDLEVNR = W-IDLEVNR                                           
123400       MOVE JA TO SW-HUVUDLEVERANTOER                                     
123500     ELSE                                                                 
123600       MOVE NEJ TO SW-HUVUDLEVERANTOER                                    
123700     END-IF                                                               
123800     PERFORM IMS-GHNP-WDK611                                              
123900     PERFORM BAC-GET-WDK621                                               
124000*                                                                         
124100     MOVE SPACE               TO W-IDLEVNR-SHIP-2258                      
124200     MOVE ZERO                TO W-IDANSK-2260                            
124300                                                                          
124400     IF W-IDLEVNR = ART-IDLEVNR                                           
124500       MOVE CLAG-IDLEVNR-SHIP TO MOD-IDLEVNR-SHIP-UT                      
124600                                 W-IDLEVNR-SHIP                           
124700                                 W-IDLEVNR-SHIP-2258                      
124800       MOVE CLAG-IDANSK       TO W-IDANSK-2260                            
124900     ELSE                                                                 
125000       PERFORM IMS-GNP-WDK623                                             
125100       MOVE NEJ TO TRAFF                                                  
125200       PERFORM UNTIL SEGMENT-SAKNAS OR TRAFF = JA                         
125300         IF W-IDLEVNR = AVT-IDLEVNR-AVT                                   
125400           MOVE AVT-IDLEVNR-SHIP TO MOD-IDLEVNR-SHIP-UT                   
125500                                    W-IDLEVNR-SHIP                        
125600           MOVE JA TO TRAFF                                               
125700         END-IF                                                           
125800         PERFORM IMS-GNP-WDK623                                           
125900       END-PERFORM                                                        
126000       IF TRAFF = NEJ                                                     
126100         MOVE W-IDLEVNR         TO MOD-IDLEVNR-SHIP-UT                    
126200                                   W-IDLEVNR-SHIP                         
126300       END-IF                                                             
126400     END-IF                                                               
126500*                                                                         
126600     MOVE CLAG-KVDAGAR-TT     TO W-MATINFO-KVDAGAR-TT                     
126700     MOVE CLAG-KVDAGAR-INLEV  TO W-MATINFO-KVDAGAR-INLEV                  
126800     MOVE CLAG-KVVECKOR-LT    TO W-MATINFO-KVVECKOR-LT                    
126900     MOVE CLAG-IDANSK         TO W-MATINFO-IDANSK                         
127000     MOVE CLAG-KDEFFMAN       TO W-MATINFO-KDEFFMAN                       
127100     MOVE CLAG-ADLAGOMR       TO W-MATINFO-ADLAGOMR                       
127200     MOVE CLAG-KDAVT          TO W-MATINFO-KDAVT                          
127300     MOVE CLAG-KDLPSP         TO W-MATINFO-KDLPSP                         
127400     MOVE CLAG-KDVVKL         TO W-MATINFO-KDVVKL                         
127500     MOVE CLAG-TILPSP         TO W-MATINFO-TILPSP                         
127600     MOVE CLAG-KDHF           TO W-MATINFO-KDHF                           
127700     MOVE CLAG-KDLEVPLF       TO W-MATINFO-KDLEVPLF                       
127800     MOVE CLAG-FLJIT          TO W-FLJIT                                  
127900*                                                                         
128000     PERFORM BAB-LAS-WDK7                                                 
128100     MOVE W-IDLEVNR           TO WS-IDLEVNR-EMIL                          
128200     IF (CLAG-KDEFFMAN = 'E' OR 'B') OR                                   
128300        (NOT EJ-GODK-EMIL-LEVNR)                                          
128400        COMPUTE W-ARSOMS = 12                                             
128500                * ( CLAG-KVPB-SEP                                         
128600                  + CLAG-KVPB-SATS                                        
128700                  + CLAG-KVPB-TPO                                         
128800                  + W-KVPB-SDC    )                                       
128900                *   WS-PRARTBES-PR                                        
129000     END-IF                                                               
129100     IF (CLAG-KVQ > ZERO)                                                 
129200        COMPUTE W-ARSBEH = 12                                             
129300                * ( CLAG-KVPB-SEP                                         
129400                  + CLAG-KVPB-SATS                                        
129500                  + CLAG-KVPB-TPO                                         
129600                  + W-KVPB-SDC    )                                       
129700     END-IF                                                               
129800*                                                                         
129900*    IF CLAG-KDERS > 20                                                   
130000*     IF (MID-KOMKOD NUMERIC AND                                          
130100*        MID-KOMKOD = '5') OR                                             
130200*        (MID-KOMKOD NUMERIC AND                                          
130300*        MID-KOMKOD = '1')                                                
130400*        OR (W-IDLEVNR NOT = '1002')                                      
130500*        CONTINUE                                                         
130600*     ELSE                                                                
130700*       MOVE NEJ TO SW-INDATA                                             
130800*       IF ENGLISH-TEXT                                                   
130900*          MOVE FEL-116  TO MOD-MESSAGE                                   
131000*       ELSE                                                              
131100*          MOVE FEL-16   TO MOD-MESSAGE                                   
131200*       END-IF                                                            
131300*     END-IF                                                              
131400*    END-IF                                                               
131500*                                                                         
131600     IF W-IDLEVNR = '1002'                                                
131700       MOVE CLAG-PRARTSTD     TO W-EKINFO-PRARTSTD                        
131800     END-IF                                                               
131900*                                                                         
132000     PERFORM S14-NOLLSTAELL-AVROPSTABELLER                                
132100     MOVE NEJ TO SW-LEVSEGM-FINNS                                         
132200     MOVE IDARTNR-WS  TO W-IDARTNR-D9                                     
132300     MOVE WC-CDC-SE   TO W-IDDC-D9                                        
132400     PERFORM IMS-GU-LEVART                                                
132500     IF SEGMENT-SAKNAS                                                    
132600       MOVE ZERO TO W-LEVNR-KVBR                                          
132700                    W-OMSPEC-TISPECST                                     
132800                    W-OMSPEC-KDPLKOEP                                     
132900                    W-ORSAK-TAB-KOD (1)                                   
133000                    W-ORSAK-TAB-KOD (2)                                   
133100                    W-ORSAK-TAB-KOD (3)                                   
133200                    W-OMSPEC-KVBEST-PL                                    
133300     ELSE                                                                 
133400       PERFORM IMS-GNP-LEVERANTOER-SEG-KVAL                               
133500       IF SEGMENT-SAKNAS                                                  
133600         MOVE ZERO TO W-LEVNR-KVBR                                        
133700                      W-OMSPEC-TISPECST                                   
133800                      W-OMSPEC-KDPLKOEP                                   
133900                      W-ORSAK-TAB-KOD (1)                                 
134000                      W-ORSAK-TAB-KOD (2)                                 
134100                      W-ORSAK-TAB-KOD (3)                                 
134200                      W-OMSPEC-KVBEST-PL                                  
134300       ELSE                                                               
134400         MOVE JA TO SW-LEVSEGM-FINNS                                      
134500         MOVE LEVNR-KVBR TO W-LEVNR-KVBR                                  
134600         PERFORM IMS-GHNP-OMSPEC-SEG                                      
134700         IF SEGMENT-SAKNAS                                                
134800           MOVE ZERO TO W-OMSPEC-TISPECST                                 
134900                        W-OMSPEC-KVBEST-PL                                
135000                        W-OMSPEC-KDPLKOEP                                 
135100                        W-ORSAK-TAB-KOD (1)                               
135200                        W-ORSAK-TAB-KOD (2)                               
135300                        W-ORSAK-TAB-KOD (3)                               
135400         ELSE                                                             
135500           MOVE OMSPEC-DASPECST      TO W-DASPECST                        
135600           MOVE W-DASPECST-AAVV      TO W-OMSPEC-TISPECST                 
135700           MOVE OMSPEC-KVBEST-PL     TO W-OMSPEC-KVBEST-PL                
135800           MOVE OMSPEC-KDPLKOEP      TO W-OMSPEC-KDPLKOEP                 
135900           MOVE OMSPEC-KDLPORS-TAB (1) TO W-ORSAK-TAB-KOD (1)             
136000           MOVE OMSPEC-KDLPORS-TAB (2) TO W-ORSAK-TAB-KOD (2)             
136100           MOVE OMSPEC-KDLPORS-TAB (3) TO W-ORSAK-TAB-KOD (3)             
136200         END-IF                                                           
136300         PERFORM BAA-LAES-AVROP-TILL-TABELLER                             
136400       END-IF                                                             
136500     END-IF                                                               
136600     .                                                                    
136700     EJECT                                                                
136800 BAA-LAES-AVROP-TILL-TABELLER SECTION.                                    
136810     MOVE 'BAA-LAES-AVROP-TILL-TABELLER' TO CURRENT-SECTION               
136900                                                                          
137000     IF W-KDBEHX-PLAN = GALLANDE                                          
137100       MOVE 2  TO W-KDAVROP                                               
137200     ELSE                                                                 
137300       MOVE 1  TO W-KDAVROP                                               
137400     END-IF                                                               
137500     MOVE ZERO TO IX-GAM                                                  
137600     PERFORM IMS-GHNP-AVROP-OKVAL-NEXT                                    
137700     PERFORM UNTIL SEGMENT-SAKNAS                                         
137800       MOVE AVROP-DAAVROP-AVS   TO W-DAAVROP-AVS                          
137900       MOVE W-DAAVROP-AVS-AAVV  TO TMP1-YYWW                              
138000       MOVE W-OMSPEC-TISPECST   TO TMP2-YYWW                              
138100       PERFORM WY2000P3                                                   
138200       IF (TMP1-YYWW < TMP2-YYWW   AND                                    
138300           AVROP-KDAVROP = 2)                                             
138400       OR (TMP1-YYWW >= TMP2-YYWW     AND                                 
138500           W-KDBEHX-PLAN = FORSLAG    AND                                 
138600           AVROP-KDAVROP = 1)                                             
138700       OR (TMP1-YYWW >= TMP2-YYWW     AND                                 
138800           W-KDBEHX-PLAN = GALLANDE   AND                                 
138900           AVROP-KDAVROP = 2)                                             
139000                                                                          
139100         MOVE AVROP-DAAVROP-AVS   TO W-DAAVROP-AVS                        
139200         MOVE W-DAAVROP-AVS-AAVV  TO TMP1-YYWW                            
139300         MOVE W-DATUM-AKTUELLT    TO TMP2-YYWW                            
139400         PERFORM WY2000P3                                                 
139500         IF TMP1-YYWW < TMP2-YYWW                                         
139600           PERFORM S12-GAMMALT-AVROP-TILL-TAB                             
139700         ELSE                                                             
139800           PERFORM S08-NYA-AVROP-TILL-TAB                                 
139900         END-IF                                                           
140000       END-IF                                                             
140100       PERFORM IMS-GHNP-AVROP-OKVAL-NEXT                                  
140200     END-PERFORM                                                          
140300     .                                                                    
140400     EJECT                                                                
140500 BAB-LAS-WDK7            SECTION.                                         
140510     MOVE 'BAB-LAS-WDK7                ' TO CURRENT-SECTION               
140600                                                                          
140700     PERFORM IMS-GU-ARTS01-WDK701                                         
140800     MOVE ZERO                 TO W-KVPB-SDC                              
140900                                                                          
141000     IF SEGMENT-FINNS                                                     
141100        PERFORM IMS-GNP-ARTS11-WDK711                                     
141200        PERFORM UNTIL SEGMENT-SAKNAS                                      
141300           ADD SLAG-KVPB-REF   TO W-KVPB-SDC                              
141400           PERFORM IMS-GNP-ARTS11-WDK711                                  
141500        END-PERFORM                                                       
141600     END-IF                                                               
141700     .                                                                    
141800     EJECT                                                                
141900 BAC-GET-WDK621          SECTION.                                         
141910     MOVE 'BAC-GET-WDK621              ' TO CURRENT-SECTION               
142000                                                                          
142100     MOVE FUNCTION CURRENT-DATE(1:8) TO DAGENS-AAAAMMDD                   
142200     COMPUTE W-DAPRLIST = 99999999 - DAGENS-AAAAMMDD                      
142300     PERFORM IMS-GNP-WDK621                                               
142400     IF SEGMENT-SAKNAS                                                    
142500       MOVE CLAG-PRARTSTD       TO WS-PRARTBES-PR                         
142600     ELSE                                                                 
142700       MOVE NEJ                 TO FL-PRARTBES                            
142800       PERFORM UNTIL  SEGMENT-SAKNAS                                      
142900         IF PRL-SUINLEV-PR > ZERO                                         
143000           MOVE PRL-PRARTBES-PR  TO WS-PRARTBES-PR                        
143100           SET SEGMENT-SAKNAS TO TRUE                                     
143200         ELSE                                                             
143300           IF FL-PRARTBES = NEJ                                           
143400             MOVE PRL-PRARTBES-PR TO WS-PRARTBES-PR                       
143500             MOVE JA              TO FL-PRARTBES                          
143600           END-IF                                                         
143700           PERFORM IMS-GNP-WDK621                                         
143800         END-IF                                                           
143900       END-PERFORM                                                        
144000     END-IF                                                               
144100     .                                                                    
144200     EJECT                                                                
144300 BB-KONTROLL-KOMKOD     SECTION.                                          
144310     MOVE 'BB-KONTROLL-KOMKOD          ' TO CURRENT-SECTION               
144400                                                                          
144500     IF MID-KOMKOD NOT = SPACE                                            
144600       MOVE RAETT TO KOMKOD                                               
144700       IF MID-KOMKOD NUMERIC                                              
144800       AND (MID-KOMKOD = '1'                                              
144810         OR MID-KOMKOD = '2'                                              
144820         OR MID-KOMKOD = '5')                                             
145100         MOVE MID-KOMKOD TO W-KOMKOD                                      
145200         IF W-KOMKOD = 1 AND W-KDBEHX-PLAN = GALLANDE                     
145300           IF ENGLISH-TEXT                                                
145400             MOVE FEL-106  TO MOD-MESSAGE-BOTTOM                          
145500           ELSE                                                           
145600             MOVE FEL-6    TO MOD-MESSAGE-BOTTOM                          
145700           END-IF                                                         
145800           MOVE FEL TO KOMKOD                                             
145900         END-IF                                                           
145910         IF W-KOMKOD = 2                                                  
145920           IF W-KDBEHX-PLAN = SPACE                                       
145930             IF ENGLISH-TEXT                                              
145940               MOVE FEL-107 TO MOD-MESSAGE-BOTTOM                         
145950             ELSE                                                         
145960               MOVE FEL-7   TO MOD-MESSAGE-BOTTOM                         
145970             END-IF                                                       
145980             MOVE FEL       TO KOMKOD                                     
145990           ELSE                                                           
145991             PERFORM BBC-KONTROLL-LEVINFO                                 
145992           END-IF                                                         
145993         END-IF                                                           
145994                                                                          
145995         IF W-KOMKOD = 5                                                  
145996           PERFORM BBC-KONTROLL-LEVINFO                                   
145997           IF W-2216-SEG-FINNS = JA                                       
145998             IF W-KDBEHX-PLAN = FORSLAG OR W-KDBEHX-PLAN = SPACE          
145999               IF ENGLISH-TEXT                                            
146000                  MOVE FEL-108            TO MOD-MESSAGE-BOTTOM           
146001               ELSE                                                       
146002                  MOVE FEL-8              TO MOD-MESSAGE-BOTTOM           
146010               END-IF                                                     
146020               MOVE FEL                   TO KOMKOD                       
146030             ELSE                                                         
146040               IF W-IDLEVNR = '1002'                                      
146050              AND CLAG-KDERS > 20                                         
146060                 MOVE IDARTNR-WS          TO W-IDARTNR-D9                 
146070                 MOVE WC-CDC-SE           TO W-IDDC-D9                    
146080                 PERFORM IMS-GU-LEVART                                    
146090                 PERFORM IMS-GHNP-AVROP-FIRST                             
146091                 MOVE NEJ                 TO W-KIT-EXIST                  
146092                 PERFORM UNTIL SEGMENT-SAKNAS                             
146093                            OR W-KIT-EXIST = JA                           
146094                   MOVE AVROP-DAAVROP-AVS TO W-DAAVROP                    
146095                   MOVE AVROP-TILEVDAG    TO W-TILEVDAG                   
146096                   MOVE +2                TO W-KDAVROP                    
146097                   PERFORM IMS-GU-WDD907                                  
146098                   IF SEGMENT-FINNS                                       
146099                     MOVE LOW-VALUE       TO W-WDJ2CSEQ-MIN-X             
146100                     MOVE HIGH-VALUE      TO W-WDJ2CSEQ-MAX-X             
146110                     MOVE W-IDARTNR       TO W-J2CSEQ-MIN-IDARTNR         
146120                                             W-J2CSEQ-MAX-IDARTNR         
146130                     MOVE NEJ             TO W-KIT-EXIST                  
146140                     PERFORM IMS-GU-WDJ201-OKVAL                          
146150                     PERFORM UNTIL SEGMENT-SAKNAS                         
146160                                OR W-KIT-EXIST = JA                       
146170                       IF SHUV-IDORDNSB = IDORDNSB                        
146180                          MOVE JA         TO W-KIT-EXIST                  
146190                       ELSE                                               
146191                          PERFORM IMS-GN-WDJ201-OKVAL                     
146192                       END-IF                                             
146193                     END-PERFORM                                          
146194                     PERFORM IMS-GHNP-AVROP-NEXT                          
146195                   END-IF                                                 
146196                 END-PERFORM                                              
146197                                                                          
146198                 IF W-KIT-EXIST = JA                                      
146199                   MOVE FEL               TO KOMKOD                       
146201                   IF ENGLISH-TEXT                                        
146210                     MOVE FEL-112         TO MOD-MESSAGE-BOTTOM           
146220                   ELSE                                                   
146230                     MOVE FEL-12          TO MOD-MESSAGE-BOTTOM           
146240                   END-IF                                                 
146250                 END-IF                                                   
146260               END-IF                                                     
146270             END-IF                                                       
146271           END-IF                                                         
146280         END-IF                                                           
146290                                                                          
147800         IF W-KOMKOD = 2                                                  
147900           MOVE JA TO FLUPPD-2228                                         
148000         END-IF                                                           
148100                                                                          
148101       ELSE                                                               
148110         MOVE FEL TO KOMKOD                                               
148200       END-IF                                                             
148300     END-IF                                                               
148400                                                                          
148500     IF MID-KOMKOD = SPACE                                                
148600     AND W-KDBEHX-PLAN = GALLANDE                                         
148700       MOVE RAETT TO KOMKOD                                               
148800     END-IF                                                               
148900     .                                                                    
149000     EJECT                                                                
149010 BBC-KONTROLL-LEVINFO    SECTION.                                         
149020     MOVE 'BBC-KONTROLL-LEVINFO         ' TO CURRENT-SECTION              
149030     MOVE JA              TO W-2216-SEG-FINNS                             
149040                                                                          
149050     MOVE SPACE           TO DLI-IO-AREA                                  
149060     MOVE '2215'          TO W-WDGX-KEY                                   
149070     MOVE W-IDLEVNR       TO X-IDLEVNR                                    
149080     PERFORM IMS-GU-2216-SEG                                              
149090     IF SEGMENT-SAKNAS                                                    
149091        MOVE NEJ          TO W-2216-SEG-FINNS                             
149092        IF MFS-UPD-X                                                      
149093          PERFORM BBCA-CREATE-ALERT-761                                   
149094        ELSE                                                              
149095          IF ENGLISH-TEXT                                                 
149096             MOVE FEL-120 TO MOD-MESSAGE                                  
149097          ELSE                                                            
149098             MOVE FEL-20  TO MOD-MESSAGE                                  
149099          END-IF                                                          
149108          MOVE FEL        TO KOMKOD                                       
149109        END-IF                                                            
149110                                                                          
149111     END-IF                                                               
149112     .                                                                    
149113     EJECT                                                                
149114                                                                          
149115 BBCA-CREATE-ALERT-761 SECTION.                                           
149116     MOVE 'BBCA-CREATE-ALERT-761       ' TO CURRENT-SECTION               
149117                                                                          
149118     MOVE W-MATINFO-IDANSK         TO W-IDANSK-2232                       
149119     PERFORM IMS-GU-WDR220                                                
149120     IF SEGMENT-FINNS                                                     
149121        MOVE 2232-IDANSK-LARM      TO W-IDANSK-2223                       
149123     ELSE                                                                 
149124        MOVE ZERO                  TO W-IDANSK-2223                       
149125     END-IF                                                               
149126     PERFORM IMS-GU-WDGX2223                                              
149127     IF SEGMENT-SAKNAS                                                    
149128       MOVE '2223'                 TO 2223-IDHTYP                         
149129       MOVE W-IDANSK-2223          TO 2223-IDANSK                         
149130       MOVE LOW-VALUE              TO 2223-LOW-VALUE                      
149131       PERFORM IMS-ISRT-WDGX2223                                          
149132       PERFORM IMS-GU-WDGX2223                                            
149140     END-IF                                                               
149150                                                                          
149151     MOVE '761'                    TO W-KDLARM                            
149152     MOVE WC-CDC-SE                TO W-IDDC                              
149153     PERFORM IMS-GHNP-WDGX2224                                            
149154     IF SEGMENT-SAKNAS                                                    
149156       PERFORM BBCAA-BUILD-WDGX2224                                       
149157       PERFORM IMS-ISRT-WDGX2224                                          
149158       PERFORM UNTIL (NOT SEGMENT-FINNS-REDAN)                            
149159         MOVE 2224-TISENBEK-KL     TO WS-NXT-KL                           
149160         PERFORM BBCAB-NXT-SEKUND                                         
149161         MOVE WS-NXT-KL            TO 2224-TISENBEK-KL                    
149163         PERFORM IMS-ISRT-WDGX2224                                        
149164       END-PERFORM                                                        
149165     ELSE                                                                 
149166       MOVE WS-DAGENS-DATUM        TO 2224-TIREGDAT                       
149167       PERFORM IMS-REPL-WDGX2224                                          
149169     END-IF                                                               
149170     .                                                                    
149171     EJECT                                                                
149172                                                                          
149173 BBCAA-BUILD-WDGX2224 SECTION.                                            
149174     MOVE 'BBCAA-BUILD-WDGX2224  ' TO CURRENT-SECTION                     
149175                                                                          
149176     MOVE FUNCTION CURRENT-DATE(3:6)  TO  2224-TISENBEK-DAG               
149177     MOVE FUNCTION CURRENT-DATE(11:6) TO  2224-TISENBEK-KL                
149178                                                                          
149179     MOVE '761'               TO 2224-KDLARM                              
149180     MOVE W-IDARTNR           TO 2224-IDARTNR                             
149181     MOVE WC-CDC-SE           TO 2224-IDDC                                
149182     MOVE JA                  TO 2224-FLNYLARM                            
149183     MOVE ZERO                TO 2224-IDDISTR                             
149184     MOVE ZERO                TO 2224-IDKUNDNR                            
149185     MOVE '0000000   '        TO 2224-IDKUNDRF                            
149186     MOVE 1                   TO 2224-IDLOPNR                             
149187     MOVE WS-DAGENS-DATUM     TO 2224-TIREGDAT                            
149188     MOVE 'W221'              TO 2224-IDTRANS                             
149189     MOVE SPACE               TO 2224-KDMFSFOR                            
149190     MOVE ZERO                TO 2224-IDKR                                
149191     MOVE W-IDLEVNR           TO 2224-IDLEVNR                             
149192                                                                          
149193     .                                                                    
149194     EJECT                                                                
149195 BBCAB-NXT-SEKUND SECTION.                                                
149196     MOVE 'BBCAB-NXT-SEKUND      ' TO CURRENT-SECTION                     
149197*                                                                         
149198*    RÄKNAR UPP TILL NÄSTA SEKUND.                                        
149199*    GÅR ALDRIG ÖVER DYGNS-GRÄNS.                                         
149200*    NÄSTA SEKUND EFTER 23.59.59 GER 00.00.00 INOM SAMMA DYGN.            
149201*                                                                         
149202     ADD 1                   TO WS-NXT-KL-SS                              
149203     IF  WS-NXT-KL-SS > 59                                                
149204       MOVE ZERO             TO WS-NXT-KL-SS                              
149205       ADD 1                 TO WS-NXT-KL-MM                              
149206       IF  WS-NXT-KL-MM > 59                                              
149207         MOVE ZERO           TO WS-NXT-KL-MM                              
149208         ADD 1               TO WS-NXT-KL-TT                              
149209         IF  WS-NXT-KL-TT > 23                                            
149210           MOVE ZERO         TO WS-NXT-KL-TT                              
149211         END-IF                                                           
149212       END-IF                                                             
149213     END-IF                                                               
149214     .                                                                    
149215     EJECT                                                                
149216 BC-KONTROLL-AVROPSDATA  SECTION.                                         
149217     MOVE 'BC-KONTROLL-AVROPSDATA      ' TO CURRENT-SECTION               
149220                                                                          
149300     MOVE 1 TO IX                                                         
149400     MOVE 2 TO IX-PLUS-1                                                  
149500     MOVE 3 TO IX-PLUS-2                                                  
149600                                                                          
149700     PERFORM UNTIL IX > MAX-ANT-AENDR-AVROP                               
149800                                                                          
149900       IF (MID-TIAVROP-AVS (IX) NOT = AAVV AND                            
150000           MID-TIAVROP-AVS (IX) NOT = YYWW AND                            
150100           MID-TIAVROP-AVS (IX) NOT = SPACE)                              
150200       OR  MID-KVAVROP (IX) NOT = SPACE                                   
150300                                                                          
150400         MOVE RAETT TO FAELTTAB-FAELT (IX-PLUS-1)                         
150500         IF MID-TIAVROP-AVS (IX) NOT NUMERIC                              
150600           MOVE FEL TO FAELTTAB-FAELT (IX-PLUS-1)                         
150700         ELSE                                                             
150800           MOVE MID-TIAVROP-AVS (IX) TO W-DAAVROP-AAVV                    
150900           PERFORM S25-SEKELJUSTERA                                       
151000           PERFORM S07-KONTROLL-DATUM                                     
151100                                                                          
151200           MOVE W-DAAVROP-AAVV     TO TMP1-YYWW                           
151300           MOVE W-DATUM-AKTUELLT   TO TMP2-YYWW                           
151400           MOVE W-DATUM-AKT-PLUS59 TO TMP3-YYWW                           
151500           PERFORM WY2000Q3                                               
151600           IF  FAELTTAB-FAELT (IX-PLUS-1) = RAETT                         
151700           AND (TMP1-YYWW < TMP2-YYWW  OR                                 
151800                TMP1-YYWW NOT < TMP3-YYWW)                                
151900           AND W-DAAVROP-AAVV NOT = 9999                                  
152000                                                                          
152100             IF  W-DAAVROP-AAVV NOT = W-TIAVROP-AVS-GAM (1)               
152200             AND W-DAAVROP-AAVV NOT = W-TIAVROP-AVS-GAM (2)               
152300             AND W-DAAVROP-AAVV NOT = W-TIAVROP-AVS-GAM (3)               
152400             AND W-DAAVROP-AAVV NOT = W-TIAVROP-AVS-GAM (4)               
152500             AND W-DAAVROP-AAVV NOT = W-TIAVROP-AVS-GAM (5)               
152600               MOVE FEL TO FAELTTAB-FAELT (IX-PLUS-1)                     
152700             END-IF                                                       
152800           END-IF                                                         
152900                                                                          
153000           MOVE W-IDLEVNR      TO OLIKA-LEV                               
153100           IF VOLKSWAGEN-LEVNR                                            
153200             MOVE W-DAAVROP-AAVV TO W-DATUM-AAVV                          
153300             DIVIDE W-DATUM-VV BY 4 GIVING W-ANTAL-VECKOR                 
153400             MULTIPLY 4 BY W-ANTAL-VECKOR                                 
153500             IF W-DATUM-VV NOT = W-ANTAL-VECKOR                           
153600               IF ENGLISH-TEXT                                            
153700                  MOVE FEL-102  TO MOD-MESSAGE-BOTTOM                     
153800               ELSE                                                       
153900                  MOVE FEL-2    TO MOD-MESSAGE-BOTTOM                     
154000               END-IF                                                     
154100             END-IF                                                       
154200           END-IF                                                         
154300                                                                          
154400           MOVE W-DAAVROP-AAVV TO W-DATUM-AAVV                            
154500           COMPUTE W-ANTAL-VECKOR ROUNDED =                               
154600                                W-MATINFO-KVDAGAR-TT / 5                  
154700           ADD W-DATUM-VV TO W-ANTAL-VECKOR                               
154800                                                                          
154900           IF W-DATUM-AA = 9 AND W-ANTAL-VECKOR > 53                      
155000              ADD 1 TO W-DATUM-AA                                         
155100              SUBTRACT 53 FROM W-ANTAL-VECKOR                             
155200           END-IF                                                         
155300           IF W-DATUM-AA = 9 AND W-ANTAL-VECKOR = 53                      
155400             CONTINUE                                                     
155500           ELSE                                                           
155600             PERFORM UNTIL W-ANTAL-VECKOR <= 52                           
155700               ADD 1 TO W-DATUM-AA                                        
155800               SUBTRACT 52 FROM W-ANTAL-VECKOR                            
155900             END-PERFORM                                                  
156000           END-IF                                                         
156100           MOVE W-ANTAL-VECKOR TO W-DATUM-VV                              
156200                                                                          
156300           IF  W-DAAVROP-AAVV = 9999                                      
156400           AND W-TIAVROP-AVS-GAM (1) NOT = 9999                           
156500             MOVE FEL TO FAELTTAB-FAELT (IX-PLUS-1)                       
156600           END-IF                                                         
156700                                                                          
156800           IF MID-KOMKOD = '1' AND W-KDBEHX-PLAN = FORSLAG                
156900             MOVE FEL TO FAELTTAB-FAELT (IX-PLUS-1)                       
157000           END-IF                                                         
157100                                                                          
157200           IF W-IDLEVNR = '1002'                                          
157300             IF W-DAAVROP-AAVV = 9999                                     
157400               MOVE FEL TO FAELTTAB-FAELT (IX-PLUS-1)                     
157500               MOVE MFS-ALFA-FAELT-FEL TO                                 
157600                              MOD-TIAVROP-AVS-IN-ATTR(IX)                 
157700               IF ENGLISH-TEXT                                            
157800                  MOVE FEL-112  TO MOD-MESSAGE-BOTTOM                     
157900               ELSE                                                       
158000                  MOVE FEL-12   TO MOD-MESSAGE-BOTTOM                     
158100               END-IF                                                     
158200             ELSE                                                         
158300               MOVE +2                  TO W-KDAVROP                      
158400               MOVE IDARTNR-WS          TO W-IDARTNR-D9                   
158500               MOVE WC-CDC-SE           TO W-IDDC-D9                      
158600               PERFORM IMS-GU-AVROP-KVAL                                  
158700               IF SEGMENT-FINNS                                           
158800                 MOVE AVROP-TILEVDAG TO W-TILEVDAG                        
158900                 PERFORM IMS-GNP-SATSBEORDR                               
159000                 IF SEGMENT-FINNS                                         
159100                     MOVE FEL TO FAELTTAB-FAELT (IX-PLUS-1)               
159200                     MOVE MFS-ALFA-FAELT-FEL TO                           
159300                                MOD-TIAVROP-AVS-IN-ATTR(IX)               
159400                     IF ENGLISH-TEXT                                      
159500                        MOVE FEL-112  TO MOD-MESSAGE-BOTTOM               
159600                     ELSE                                                 
159700                        MOVE FEL-12   TO MOD-MESSAGE-BOTTOM               
159800                     END-IF                                               
159900                 END-IF                                                   
160000               END-IF                                                     
160100             END-IF                                                       
160200           END-IF                                                         
160300         END-IF                                                           
160400       END-IF                                                             
160500                                                                          
160600       IF MID-KVAVROP (IX) NOT = SPACE                                    
160700       OR (MID-TIAVROP-AVS (IX) NOT = AAVV AND                            
160800           MID-TIAVROP-AVS (IX) NOT = YYWW AND                            
160900           MID-TIAVROP-AVS (IX) NOT = SPACE)                              
161000         MOVE RAETT TO FAELTTAB-FAELT (IX-PLUS-2)                         
161100                                                                          
161200         IF MID-KVAVROP (IX) NOT NUMERIC                                  
161300           MOVE FEL TO FAELTTAB-FAELT (IX-PLUS-2)                         
161400         ELSE                                                             
161500           MOVE MID-KVAVROP (IX) TO W-KVAVROP                             
161600         END-IF                                                           
161700         IF  MID-TIAVROP-AVS (IX) = '9999'                                
161800         AND (W-KVAVROP > W-KVAVROP-GAM (1) OR                            
161900              W-KVAVROP = ZERO)                                           
162000           MOVE FEL TO FAELTTAB-FAELT (IX-PLUS-2)                         
162100         END-IF                                                           
162200         IF MID-KOMKOD = '1' AND W-KDBEHX-PLAN = FORSLAG                  
162300           MOVE FEL TO FAELTTAB-FAELT (IX-PLUS-1)                         
162400         END-IF                                                           
162410       END-IF                                                             
162500                                                                          
165200       ADD 1 TO IX                                                        
165300       ADD 2 TO IX-PLUS-1                                                 
165400                IX-PLUS-2                                                 
165500     END-PERFORM                                                          
165600     .                                                                    
165700     EJECT                                                                
165800 BE-KONTROLL-KOEP SECTION.                                                
165810     MOVE 'BE-KONTROLL-KOEP            ' TO CURRENT-SECTION               
165900                                                                          
166000     IF MID-KVBEST-PL NOT = SPACE                                         
166100       MOVE RAETT TO KVBEST-PL                                            
166200       IF MID-KVBEST-PL NOT NUMERIC OR W-MATINFO-KDAVT = ZERO             
166300       OR SW-HUVUDLEVERANTOER = NEJ                                       
166400         MOVE FEL TO KVBEST-PL                                            
166500       ELSE                                                               
166600         MOVE MID-KVBEST-PL TO W-KVBEST-PL                                
166700         IF W-KVBEST-PL = ZERO AND W-OMSPEC-KVBEST-PL = ZERO              
166800           IF ENGLISH-TEXT                                                
166900              MOVE FEL-103  TO MOD-MESSAGE-BOTTOM                         
167000           ELSE                                                           
167100              MOVE FEL-3    TO MOD-MESSAGE-BOTTOM                         
167200           END-IF                                                         
167300           MOVE FEL TO KVBEST-PL                                          
167400         END-IF                                                           
167500         IF W-KDBEHX-PLAN = FORSLAG AND MID-KOMKOD NOT = '2'              
167600           MOVE FEL TO KVBEST-PL                                          
167700         END-IF                                                           
167800       END-IF                                                             
167900     END-IF                                                               
168000     .                                                                    
168100     EJECT                                                                
168200 BF-KONTROLL-OMSPEC      SECTION.                                         
168210     MOVE 'BF-KONTROLL-OMSPEC         ' TO CURRENT-SECTION                
168300                                                                          
168400     MOVE NEJ              TO SW-OMSPEC-BEG                               
168500     IF MID-KDOMSPEC NOT = SPACE                                          
168600       MOVE RAETT TO KDOMSPEC                                             
168700       IF MID-KDOMSPEC NOT NUMERIC OR MID-KDOMSPEC < '1'                  
168800       OR MID-KDOMSPEC > '3'                                              
168900         MOVE FEL TO KDOMSPEC                                             
169000       ELSE                                                               
169100         IF MID-KDOMSPEC          =  3                                    
169200            IF SW-LEVSEGM-FINNS   = JA AND                                
169300               SW-HUVUDLEVERANTOER = JA                                   
169400               MOVE JA            TO SW-OMSPEC-BEG                        
169500               MOVE MID-KDOMSPEC  TO W-KDOMSPEC                           
169600            ELSE                                                          
169700               MOVE FEL           TO KDOMSPEC                             
169800            END-IF                                                        
169900         ELSE                                                             
170000            MOVE MID-KDOMSPEC     TO W-KDOMSPEC                           
170100         END-IF                                                           
170200       END-IF                                                             
170300     END-IF                                                               
170400     .                                                                    
170500     EJECT                                                                
170600 BG-KONTROLL-LPSP       SECTION.                                          
170610     MOVE 'BG-KONTROLL-LPSP            ' TO CURRENT-SECTION               
170700                                                                          
170800     IF MID-TILPSP NOT = AAVV AND MID-TILPSP NOT = SPACE AND              
170900        MID-TILPSP NOT = YYWW                                             
171000       MOVE RAETT TO TILPSP                                               
171100       IF  MID-TILPSP NOT NUMERIC                                         
171200       OR (W-MATINFO-KDLPSP NOT = ZERO AND                                
171300           W-MATINFO-KDLPSP NOT = 3    AND                                
171400           W-MATINFO-KDLPSP NOT = 6)                                      
171500         MOVE FEL TO TILPSP                                               
171600       ELSE                                                               
171700         MOVE MID-TILPSP TO W-TILPSP                                      
171800         MOVE W-TILPSP TO W-DATUM-AAVV                                    
171900         MOVE W-TILPSP           TO TMP1-YYWW                             
172000         MOVE W-DATUM-AKTUELLT   TO TMP2-YYWW                             
172100         PERFORM WY2000P3                                                 
172200         IF W-DATUM-AA = 20                                               
172300           IF TMP1-YYWW < TMP2-YYWW                                       
172400           OR W-DATUM-VV < 01 OR W-DATUM-VV > 53                          
172500             MOVE FEL TO TILPSP                                           
172600           END-IF                                                         
172700         ELSE                                                             
172800           IF TMP1-YYWW < TMP2-YYWW                                       
172900           OR W-DATUM-VV < 01 OR W-DATUM-VV > 52                          
173000             MOVE FEL TO TILPSP                                           
173100           END-IF                                                         
173200         END-IF                                                           
173300       END-IF                                                             
173400     END-IF                                                               
173500     .                                                                    
173600     EJECT                                                                
173700 BH-KONTROLL-FAELTTAB SECTION.                                            
173710     MOVE 'BH-KONTROLL-FAELTTAB        ' TO CURRENT-SECTION               
173800                                                                          
173900     MOVE JA TO SW-INDATA                                                 
174000     MOVE NEJ TO SW-FAELT-IFYLLT                                          
174100     MOVE 1 TO FAELTTAB-IX                                                
174200                                                                          
174300     PERFORM UNTIL FAELTTAB-IX > FAELTTAB-IX-MAX                          
174400       IF FAELTTAB-FAELT (FAELTTAB-IX) = RAETT                            
174500         MOVE JA TO SW-FAELT-IFYLLT                                       
174600       ELSE                                                               
174700         IF  FAELTTAB-FAELT (FAELTTAB-IX) = FEL                           
174800           MOVE NEJ TO SW-INDATA                                          
174900           MOVE JA TO SW-FAELT-IFYLLT                                     
175000         END-IF                                                           
175100       END-IF                                                             
175200       ADD 1 TO FAELTTAB-IX                                               
175300     END-PERFORM                                                          
175400     .                                                                    
175500     EJECT                                                                
175600 BI-KONTROLL-SATSART SECTION.                                             
175610     MOVE 'BI-KONTROLL-SATSART         ' TO CURRENT-SECTION               
175700                                                                          
175800     IF W-EKINFO-PRARTSTD = 0                                             
175900       MOVE NEJ TO SW-INDATA                                              
176000       IF ENGLISH-TEXT                                                    
176100          MOVE FEL-111  TO MOD-MESSAGE                                    
176200       ELSE                                                               
176300          MOVE FEL-11   TO MOD-MESSAGE                                    
176400       END-IF                                                             
176500       MOVE '301' TO MSG-KOM-IDMFSMED                                     
176600       MOVE '2'   TO MSG-KOM-KDSVAR                                       
176700     ELSE                                                                 
176800       PERFORM IMS-GU-SATS-ROT                                            
176900       IF SEGMENT-FINNS                                                   
177000         PERFORM IMS-GNP-ING-SATS-ART                                     
177100         IF SEGMENT-FINNS                                                 
177200            MOVE ZERO TO W-ANT-ARTIKLAR-I-SATS                            
177300            PERFORM UNTIL SEGMENT-SAKNAS                                  
177400               MOVE SATB-RAD-TISTADAT TO TMP1-YYMMDD                      
177500               MOVE SATB-RAD-TISTODAT TO TMP2-YYMMDD                      
177600               MOVE WS-DAGENS-DATUM   TO TMP3-YYMMDD                      
177700               PERFORM WY2000Q1                                           
177800               IF TMP1-YYMMDD <= TMP3-YYMMDD AND                          
177900                  TMP2-YYMMDD >  TMP3-YYMMDD                              
178000                  ADD +1 TO W-ANT-ARTIKLAR-I-SATS                         
178100               END-IF                                                     
178200               PERFORM IMS-GNP-ING-SATS-ART                               
178300            END-PERFORM                                                   
178400            IF W-ANT-ARTIKLAR-I-SATS < 2                                  
178500              MOVE NEJ TO SW-INDATA                                       
178600              IF ENGLISH-TEXT                                             
178700                 MOVE FEL-113  TO MOD-MESSAGE                             
178800              ELSE                                                        
178900                 MOVE FEL-13   TO MOD-MESSAGE                             
179000              END-IF                                                      
179100            END-IF                                                        
179200         ELSE                                                             
179300           MOVE NEJ TO SW-INDATA                                          
179400           IF ENGLISH-TEXT                                                
179500              MOVE FEL-113  TO MOD-MESSAGE                                
179600           ELSE                                                           
179700              MOVE FEL-13   TO MOD-MESSAGE                                
179800           END-IF                                                         
179900         END-IF                                                           
180000       ELSE                                                               
180100         MOVE NEJ TO SW-INDATA                                            
180200         IF ENGLISH-TEXT                                                  
180300            MOVE FEL-114  TO MOD-MESSAGE                                  
180400         ELSE                                                             
180500            MOVE FEL-14   TO MOD-MESSAGE                                  
180600         END-IF                                                           
180700       END-IF                                                             
180800       IF INDATA-FEL                                                      
180900         MOVE '007' TO MSG-KOM-IDMFSMED                                   
181000         MOVE '2'   TO MSG-KOM-KDSVAR                                     
181100       END-IF                                                             
181200     END-IF                                                               
181300     .                                                                    
181400     EJECT                                                                
181500 BJ-KONTROLL-FLJIT      SECTION.                                          
181510     MOVE 'BJ-KONTROLL-FLJIT           ' TO CURRENT-SECTION               
181600                                                                          
181700*    KONTROLL AV KOD FLJIT                                                
181810                                                                          
181900     IF MID-FLJIT = YES                                                   
182000        MOVE JA               TO MID-FLJIT                                
182100     END-IF                                                               
182200                                                                          
182300     IF MID-FLJIT = W-FLJIT                                               
182400        MOVE RAETT            TO FLJIT                                    
182500     ELSE                                                                 
182600        IF MID-FLJIT NOT = SPACE AND MID-FLJIT NOT = '+'                  
182700              IF MID-FLJIT    = 'J' OR 'N'                                
182800                 MOVE RAETT   TO FLJIT                                    
182900                 MOVE JA      TO SW-FLJIT-UPPD                            
183000              ELSE                                                        
183100                 MOVE FEL     TO FLJIT                                    
183110                 MOVE NEJ     TO SW-INDATA                                
183200              END-IF                                                      
183300        ELSE                                                              
183400              MOVE RAETT      TO FLJIT                                    
183500        END-IF                                                            
183600     END-IF                                                               
183700     .                                                                    
183800     EJECT                                                                
183900 BL-KONTROLL-KDLEVPLF   SECTION.                                          
183910     MOVE 'BL-KONTROLL-KDLEVPLF        ' TO CURRENT-SECTION               
184000                                                                          
184100*    KONTROLL AV KOD FÖR AUT. GODKÄNNANDE AV LEVERANSPLANEFÖRSLAG         
184200                                                                          
184300     IF MID-KDLEVPLF = W-MATINFO-KDLEVPLF                                 
184400        MOVE RAETT         TO KDLEVPLF                                    
184500     ELSE                                                                 
184600        IF MID-KDLEVPLF NOT = SPACE                                       
184700           IF W-MATINFO-KDLEVPLF = 'G' OR                                 
184800             (W-MATINFO-KDLEVPLF = 'P' AND                                
184900             (MID-KDLEVPLF NOT = 'J'   AND                                
185000              MID-KDLEVPLF NOT = 'S'))                                    
185100              MOVE FEL        TO KDLEVPLF                                 
185200           ELSE                                                           
185300              IF MID-KDLEVPLF = 'J' OR 'N'                                
185400                 MOVE RAETT   TO KDLEVPLF                                 
185500              ELSE                                                        
185600                 IF MID-KDLEVPLF = 'S'                                    
185700                    MOVE RAETT   TO KDLEVPLF                              
185800                 ELSE                                                     
185900                    MOVE FEL     TO KDLEVPLF                              
186000                 END-IF                                                   
186100              END-IF                                                      
186200           END-IF                                                         
186300        END-IF                                                            
186400     END-IF                                                               
186500     .                                                                    
186600     EJECT                                                                
193100 C-UPPDATERA-REGISTER-BILD SECTION.                                       
193200     MOVE 'C-UPPDATERA-REGISTER-BILD '  TO CURRENT-SECTION                
193300                                                                          
193400     PERFORM IMS-GU-WDK601                                                
193500     PERFORM IMS-GHNP-WDK611                                              
193600     MOVE NEJ TO SW-UPPDAT-CLAG                                           
193700                                                                          
193800     MOVE CLAG-TIDISPIN TO TIDISPIN-AAMMDD                                
193900                                                                          
194000     MOVE NEJ TO SW-UTSKRIFT-BEGAERD                                      
194100                 SW-AVROP-AENDRAT                                         
194200                 SW-KOEP-AENDRAT                                          
194210                 SW-UPDATE-WDK6-WDD9                                      
194300     IF  SW-LEVSEGM-FINNS = NEJ                                           
194400       PERFORM CA-SKAPA-LEVERANTOER                                       
194500     END-IF                                                               
194600     IF  W-IDLEVNR = '1002'                                               
194700       PERFORM CB-KOLL-SATSART-AENDRAD                                    
194800       PERFORM S22-BERAKNA-VV-PLUS-LT                                     
194900     END-IF                                                               
195000                                                                          
195100     MOVE IDARTNR-WS  TO W-IDARTNR-D9                                     
195200     MOVE WC-CDC-SE   TO W-IDDC-D9                                        
195300     PERFORM IMS-GU-LEVART                                                
195400                                                                          
195500     IF  MID-KOMKOD NOT = SPACE                                           
195600       PERFORM CC-KOMKOD-AENDRAD                                          
195700     END-IF                                                               
195800     IF (MID-KOMKOD = SPACE AND KDBEHX-PLAN-WS = GALLANDE)                
195900       PERFORM CC-KOMKOD-AENDRAD                                          
196000     END-IF                                                               
196100                                                                          
196200     MOVE 1 TO IX-AVROP                                                   
196300     PERFORM UNTIL IX-AVROP > MAX-ANT-AENDR-AVROP                         
196400       IF  MID-TIAVROP-AVS (IX-AVROP) NOT = AAVV                          
196500       AND MID-TIAVROP-AVS (IX-AVROP) NOT = YYWW                          
196600       AND MID-TIAVROP-AVS (IX-AVROP) NOT = SPACE                         
196700         MOVE MID-TIAVROP-AVS (IX-AVROP)  TO W-DAAVROP-AAVV               
196800         MOVE MID-KVAVROP     (IX-AVROP)  TO W-KVAVROP                    
196900         PERFORM S25-SEKELJUSTERA                                         
197000         PERFORM CD-AVROP-AENDRAT                                         
197100       END-IF                                                             
197200       ADD 1 TO IX-AVROP                                                  
197300     END-PERFORM                                                          
197400                                                                          
197500     IF  MID-KVBEST-PL NOT = SPACE                                        
197600       PERFORM CF-KOEP-AENDRAT                                            
197700     END-IF                                                               
197800                                                                          
197900     PERFORM CG-INITIERA-TABELL                                           
198000                                                                          
198100     IF  MID-KDOMSPEC NOT = SPACE                                         
198200       PERFORM S02-BEGAERAN-OMSPEC                                        
198300     END-IF                                                               
198400                                                                          
198500     IF (MID-TILPSP NOT = AAVV AND                                        
198600         MID-TILPSP NOT = YYWW AND MID-TILPSP NOT = SPACE ) OR            
198700        (MID-KDLEVPLF NOT = SPACE  AND                                    
198800         MID-KDLEVPLF NOT = W-MATINFO-KDLEVPLF)                           
198900           IF MID-TILPSP NOT = AAVV AND MID-TILPSP NOT = SPACE AND        
199000              MID-TILPSP NOT = YYWW                                       
199100              MOVE W-TILPSP TO CLAG-TILPSP                                
199200                               WS-TILPSP                                  
199300              IF WS-TILPSP > ZERO                                         
199400                 MOVE WS-TILPSP TO MOD-TILPSP                             
199500              ELSE                                                        
199600                 MOVE SPACE     TO MOD-TILPSP                             
199700              END-IF                                                      
199800              MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-TILPSP-ATTR               
199900              IF  CLAG-KDLPSP = ZERO                                      
200000                  MOVE 3 TO CLAG-KDLPSP                                   
200100                            MOD-KDLPSP                                    
200200                  MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-KDLPSP-ATTR           
200300              END-IF                                                      
200400           END-IF                                                         
200500           IF MID-KDLEVPLF NOT = SPACE  AND                               
200600              MID-KDLEVPLF NOT = W-MATINFO-KDLEVPLF                       
200700              MOVE MID-KDLEVPLF TO CLAG-KDLEVPLF                          
200800                                   MOD-KDLEVPLF-IN                        
200900           END-IF                                                         
201000           MOVE JA TO SW-UPPDAT-CLAG                                      
201100     END-IF                                                               
201200                                                                          
201300     IF FLJIT-UPPD                                                        
201400        MOVE MID-FLJIT    TO CLAG-FLJIT                                   
201500                             MOD-FLJIT-IN                                 
201600        MOVE JA           TO SW-UPPDAT-CLAG                               
201700     END-IF                                                               
201800                                                                          
201900     IF UPPDAT-OK-CLAG                                                    
202000        PERFORM IMS-REPL-WDK611                                           
202100     END-IF                                                               
202101                                                                          
202120     IF SW-UPDATE-WDK6-WDD9 = JA                                          
202130        PERFORM CH-UPDATE-WDK6-WDD9                                       
202140     END-IF                                                               
202200                                                                          
202300     PERFORM CE-UPDATE-TEARTNOT                                           
202400                                                                          
202500     IF MID-KOMKOD    NOT = SPACE                                         
202600     OR MID-KVBEST-PL NOT = SPACE                                         
202700     OR MID-KDOMSPEC  NOT = SPACE                                         
202800       PERFORM S10-ORSAKSTEXTER-TILL-MOD                                  
202900     END-IF                                                               
203000                                                                          
203100     IF SW-AVROP-AENDRAT = JA OR SW-KOEP-AENDRAT = JA                     
203200       PERFORM S11-GAM-AVROP-I-TAB-TILL-MOD                               
203300       PERFORM S13-NYA-AVROP-I-TAB-TILL-MOD                               
203400     END-IF                                                               
203500                                                                          
203600     IF MID-KOMKOD = '1' OR MID-KOMKOD = '2'                              
203700     OR MID-KVBEST-PL NOT = SPACE                                         
203800       MOVE W-LEVNR-KVBR TO W-KVBR                                        
203900       IF (W-KDBEHX-PLAN = GALLANDE AND W-OMSPEC-KDPLKOEP = 2)            
204000       OR (W-KDBEHX-PLAN = FORSLAG AND                                    
204100           W-OMSPEC-KDPLKOEP NOT = 2)                                     
204200          MOVE W-OMSPEC-KVBEST-PL TO MOD-KVBEST-PL                        
204300          ADD W-OMSPEC-KVBEST-PL TO W-KVBR                                
204400       ELSE                                                               
204500          MOVE ZERO TO MOD-KVBEST-PL                                      
204600       END-IF                                                             
204700       MOVE W-KVBR TO MOD-KVBR                                            
204800     END-IF                                                               
204900     .                                                                    
205000     EJECT                                                                
205100 CA-SKAPA-LEVERANTOER SECTION.                                            
205200     MOVE 'CA-SKAPA-LEVERANTOER '  TO CURRENT-SECTION                     
205300                                                                          
205400     MOVE W-IDARTNR TO WDD901-IDARTNR                                     
205500                       W-IDARTNR-D9                                       
205600     MOVE WC-CDC-SE TO WDD901-IDDC                                        
205700                       W-IDDC-D9                                          
205800     PERFORM IMS-INSERT-ART-SEG                                           
205900                                                                          
206000     MOVE W-IDLEVNR TO LEVNR-IDLEVNR                                      
206100     MOVE ZERO TO LEVNR-KVBR                                              
206200                  LEVNR-TILEVPL                                           
206300     PERFORM IMS-INSERT-LEVERANTOER-SEG                                   
206400     .                                                                    
206500     EJECT                                                                
206600 CB-KOLL-SATSART-AENDRAD SECTION.                                         
206700     MOVE 'CB-KOLL-SATSART-AENDRAD '   TO CURRENT-SECTION                 
206800                                                                          
206900     IF  W-KDBEHX-PLAN = FORSLAG AND MID-KOMKOD = '2'                     
207000       PERFORM S04-SATSART-AENDRAD                                        
207100     END-IF                                                               
207200     IF  W-KDBEHX-PLAN = GALLANDE AND MID-KOMKOD = '5'                    
207300       PERFORM S04-SATSART-AENDRAD                                        
207400     END-IF                                                               
207500     IF  MID-KVAVROP (1) NOT = SPACE                                      
207600     OR  MID-KVAVROP (2) NOT = SPACE                                      
207700     OR  MID-KVAVROP (3) NOT = SPACE                                      
207800     OR  MID-KVAVROP (4) NOT = SPACE                                      
207900       PERFORM S04-SATSART-AENDRAD                                        
208000     END-IF                                                               
208100                                                                          
208200     .                                                                    
208300     EJECT                                                                
208400 CC-KOMKOD-AENDRAD SECTION.                                               
208500     MOVE 'CC-KOMKOD-AENDRAD '  TO CURRENT-SECTION                        
208600                                                                          
208700*    KOD= 1 OCH FÖRSLAG  : FÖRSLAG FÖRKASTAS,GÄLLANDE PLAN VISAS *        
208800*    KOD= 2 OCH FÖRSLAG  : FÖRSLAG GODKÄNNES                     *        
208900*    KOD= 1 OCH GÄLLANDE : UTSKRIFT AV GÄLLANDE PLAN             *        
209000*    KOD= 5 OCH GÄLLANDE : GÄLLANDE PLAN RADERAS EFTER AKTUELL   *        
209100*                          VECKA                                 *        
209200                                                                          
209300     IF W-KOMKOD = 1 AND W-KDBEHX-PLAN = FORSLAG                          
209400       MOVE JA TO SW-AVROP-AENDRAT                                        
209500                  SW-KOEP-AENDRAT                                         
209600       IF ENGLISH-TEXT                                                    
209700          MOVE 'EXISTING' TO MOD-TEXT-PLANTYP                             
209800       ELSE                                                               
209900          MOVE 'GÄLLANDE' TO MOD-TEXT-PLANTYP                             
210000       END-IF                                                             
210100       MOVE GALLANDE TO MOD-KDBEHX-PLAN-UT                                
210200                        W-KDBEHX-PLAN                                     
210300       PERFORM CCA-UPPDAT-MATINFO                                         
210400                                                                          
210500       PERFORM IMS-GHNP-OMSPEC-SEG                                        
210600       IF  SEGMENT-FINNS                                                  
210700         PERFORM IMS-DELETE-WDD9                                          
210800       END-IF                                                             
210900       PERFORM CCB-AVSLAG-FORSLAG                                         
211000                                                                          
211100       IF W-OMSPEC-KVBEST-PL > ZERO                                       
211200         MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-DATA-GRP2-ATTR                 
211300       END-IF                                                             
211400       MOVE ZERO TO MOD-KDLPSP                                            
211500                    MOD-KVBEST-PL                                         
211600                    W-MATINFO-KDLPSP                                      
211700                    W-OMSPEC-KVBEST-PL                                    
211800                                                                          
211900       MOVE 1 TO IX                                                       
212000       PERFORM UNTIL IX > 3                                               
212100         MOVE ZERO TO W-ORSAK-TAB-KOD (IX)                                
212200         MOVE NEJ TO W-ORSAK-AENDRAD (IX)                                 
212300         ADD 1 TO IX                                                      
212400       END-PERFORM                                                        
212500       MOVE W-LEVNR-KVBR TO MOD-KVBR                                      
212600       MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-TEXT-PLANTYP-ATTR                
212700                                     MOD-KDLPSP-ATTR                      
212800       PERFORM S14-NOLLSTAELL-AVROPSTABELLER                              
212900       PERFORM CCC-LAES-RED-GALLANDE-PLAN                                 
213000*      -- Förkastat förslag tas bort från förslagskö                      
213100       Perform CCE-TAG-BORT-FRAN-FORSLAGSKOE                              
213200     END-IF                                                               
213300                                                                          
213400     IF W-KOMKOD = 2 AND W-KDBEHX-PLAN = FORSLAG                          
213500       MOVE ZERO TO MOD-KDLPSP                                            
213600                    W-MATINFO-KDLPSP                                      
213700       IF ENGLISH-TEXT                                                    
213800          MOVE 'EXISTING' TO MOD-TEXT-PLANTYP                             
213900       ELSE                                                               
214000          MOVE 'GÄLLANDE' TO MOD-TEXT-PLANTYP                             
214100       END-IF                                                             
214200       MOVE GALLANDE TO MOD-KDBEHX-PLAN-UT                                
214300                        W-KDBEHX-PLAN                                     
214400                                                                          
214500       IF W-OMSPEC-KVBEST-PL > ZERO                                       
214600         MOVE 2 TO W-OMSPEC-KDPLKOEP                                      
214700         MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-DATA-GRP2-ATTR                 
214800       END-IF                                                             
214900       MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-TEXT-PLANTYP-ATTR                
215000                                     MOD-KDLPSP-ATTR                      
215100                                                                          
215200       PERFORM CCA-UPPDAT-MATINFO                                         
215300                                                                          
215400       PERFORM IMS-GHNP-OMSPEC-SEG                                        
215500       IF  SEGMENT-FINNS AND OMSPEC-KVBEST-PL > ZERO                      
215600         MOVE 2 TO OMSPEC-KDPLKOEP                                        
215700         PERFORM IMS-REPLACE-WDD9                                         
215800       END-IF                                                             
215900       PERFORM CCD-GODKANN-FORSLAG                                        
216000       PERFORM S01-GEN-UTSKRIFTSBEGAERAN                                  
216100*      -- Godkänt förslag tas bort från förslagskö                        
216200       Perform CCE-TAG-BORT-FRAN-FORSLAGSKOE                              
216300     END-IF                                                               
216400                                                                          
216500     IF ((W-KOMKOD = 2 OR 5) AND KDBEHX-PLAN-WS = GALLANDE) OR            
216600        (MID-KOMKOD = SPACE  AND KDBEHX-PLAN-WS = GALLANDE)               
216700       PERFORM S01-GEN-UTSKRIFTSBEGAERAN                                  
216800     END-IF                                                               
216900                                                                          
217000     IF W-KOMKOD = 5 AND W-KDBEHX-PLAN = GALLANDE                         
217100       MOVE 2 TO W-KDAVROP                                                
217200       MOVE W-DATUM-AKTUELLT TO W-DAAVROP-AAVV                            
217300                                W-DATUM-AAVV                              
217400       PERFORM S25-SEKELJUSTERA                                           
217500       IF W-DATUM-AA = 20                                                 
217600         IF W-DATUM-VV >= 53                                              
217700           ADD 1 TO W-DATUM-AA                                            
217800           MOVE 1 TO W-DATUM-VV                                           
217900         ELSE                                                             
218000           ADD 1 TO W-DATUM-AAVV                                          
218100         END-IF                                                           
218200       ELSE                                                               
218300         IF W-DATUM-VV >= 52                                              
218400           ADD 1 TO W-DATUM-AA                                            
218500           MOVE 1 TO W-DATUM-VV                                           
218600         ELSE                                                             
218700           ADD 1 TO W-DATUM-AAVV                                          
218800         END-IF                                                           
218900       END-IF                                                             
219000       IF W-IDLEVNR = '1002'                                              
219100         PERFORM IMS-GHNP-AVROP-GREATER-DAT                               
219200         PERFORM UNTIL SEGMENT-SAKNAS                                     
219300           MOVE AVROP-DAAVROP-AVS TO W-DAAVROP                            
219400           MOVE AVROP-TILEVDAG    TO W-TILEVDAG                           
219500           PERFORM IMS-GNP-SATSBEORDR                                     
219600           IF SEGMENT-FINNS                                               
219700             MOVE JA TO SW-SATSBEORDR-FINNS                               
219800             MOVE W-DAAVROP-AAVV TO W-DATUM-SATS                          
219900           ELSE                                                           
220000             PERFORM IMS-GHNP-AVROP-KVAL-KEY-FIRST                        
220100             PERFORM IMS-DELETE-WDD9                                      
220200           END-IF                                                         
220300           PERFORM IMS-GHNP-AVROP-NEXT                                    
220400         END-PERFORM                                                      
220500                                                                          
220600         IF SATSBEORDR-FINNS                                              
220700           PERFORM S23-LYS-UPP-AVROP-I-TAB                                
220800           MOVE W-DATUM-SATS TO W-DATUM-AAVV                              
220900           IF W-DATUM-AA = 20                                             
221000             IF W-DATUM-VV >= 53                                          
221100               ADD 1 TO W-DATUM-AA                                        
221200               MOVE 1 TO W-DATUM-VV                                       
221300             ELSE                                                         
221400               ADD 1 TO W-DATUM-AAVV                                      
221500             END-IF                                                       
221600           ELSE                                                           
221700             IF W-DATUM-VV >= 52                                          
221800               ADD 1 TO W-DATUM-AA                                        
221900               MOVE 1 TO W-DATUM-VV                                       
222000             ELSE                                                         
222100               ADD 1 TO W-DATUM-AAVV                                      
222200             END-IF                                                       
222300           END-IF                                                         
222400           IF ENGLISH-TEXT                                                
222500              MOVE FEL-112  TO MOD-MESSAGE                                
222600           ELSE                                                           
222700              MOVE FEL-12   TO MOD-MESSAGE                                
222800           END-IF                                                         
222900         END-IF                                                           
223000                                                                          
223100         IF MID-KOMKOD = '5'                                              
223200         AND CLAG-KDERS > 20                                              
223300           MOVE IDARTNR-WS TO W-IDARTNR-D9                                
223400           MOVE WC-CDC-SE  TO W-IDDC-D9                                   
223500           PERFORM IMS-GU-LEVART                                          
223600           PERFORM IMS-GHNP-AVROP-FIRST                                   
223700           PERFORM UNTIL SEGMENT-SAKNAS                                   
223800             MOVE AVROP-DAAVROP-AVS TO W-DAAVROP                          
223900             MOVE AVROP-TILEVDAG    TO W-TILEVDAG                         
224000             MOVE +2                TO W-KDAVROP                          
224100             PERFORM IMS-GU-WDD906                                        
224200             IF SEGMENT-FINNS                                             
224300               CONTINUE                                                   
224400             ELSE                                                         
224500               PERFORM IMS-GU-WDD907                                      
224600               IF SEGMENT-FINNS                                           
224700                 MOVE BEORD-IDORDNSB TO WS-IDORDNSB                       
224800                 MOVE WS-IDORDNSB    TO W-IDORDNSB                        
224900                 PERFORM IMS-GHU-WDD907                                   
225000                 IF SEGMENT-FINNS                                         
225100                   PERFORM IMS-DELETE-WDD907                              
225200                 END-IF                                                   
225300               END-IF                                                     
225400               PERFORM IMS-GHU-AVROP-KVAL-KEY2                            
225500               IF SEGMENT-FINNS                                           
225600                 PERFORM IMS-DELETE-AVROP                                 
225700                 PERFORM IMS-GU-LEVART                                    
225800               END-IF                                                     
225900             END-IF                                                       
226000             PERFORM IMS-GHNP-AVROP-NEXT                                  
226100           END-PERFORM                                                    
226200         END-IF                                                           
226300                                                                          
226400         PERFORM S16-RADERA-I-AVROPSTAB                                   
226500       ELSE                                                               
226600         PERFORM S16-RADERA-I-AVROPSTAB                                   
226700         PERFORM IMS-GHNP-AVROP-GREATER-DAT                               
226800                                                                          
226900         PERFORM UNTIL SEGMENT-SAKNAS                                     
227000           PERFORM IMS-DELETE-WDD9                                        
227100           PERFORM IMS-GHNP-AVROP-NEXT                                    
227200         END-PERFORM                                                      
227300       END-IF                                                             
227400       MOVE JA TO SW-AVROP-AENDRAT                                        
227500     END-IF                                                               
227600     .                                                                    
227700     EJECT                                                                
227800 CCA-UPPDAT-MATINFO SECTION.                                              
227900     MOVE 'CCA-UPPDAT-MATINFO '  TO CURRENT-SECTION                       
228000                                                                          
228100*    OM FÖRSLAG GODKÄNNES JUSTERAS ÄVEN LEVERANSSPÄRR            *        
228200                                                                          
228300       MOVE ZERO TO CLAG-KDLPSP                                           
228400       IF  W-KOMKOD = 2                                                   
228500         MOVE ZERO TO W-ANTAL-VECKOR                                      
228600                                                                          
228700         EVALUATE W-MATINFO-KDVVKL                                        
228800            WHEN 1                                                        
228900              MOVE 3 TO W-ANTAL-VECKOR                                    
229000            WHEN 2                                                        
229100              MOVE 3 TO W-ANTAL-VECKOR                                    
229200            WHEN 3                                                        
229300              MOVE 3 TO W-ANTAL-VECKOR                                    
229400            WHEN 4                                                        
229500              MOVE 3 TO W-ANTAL-VECKOR                                    
229600            WHEN 5                                                        
229700              MOVE 2 TO W-ANTAL-VECKOR                                    
229800         END-EVALUATE                                                     
229900                                                                          
230000         IF  W-ANTAL-VECKOR > ZERO                                        
230100           MOVE W-DATUM-AKTUELLT TO W-DATUM-AAVV                          
230200           ADD W-DATUM-VV TO W-ANTAL-VECKOR                               
230300           IF W-DATUM-AA = 20 AND W-ANTAL-VECKOR > 53                     
230400             ADD 1 TO W-DATUM-AA                                          
230500             SUBTRACT 53 FROM W-ANTAL-VECKOR                              
230600           END-IF                                                         
230700           IF W-DATUM-AA = 20 AND W-ANTAL-VECKOR = 53                     
230800             CONTINUE                                                     
230900           ELSE                                                           
231000             PERFORM UNTIL W-ANTAL-VECKOR <= 52                           
231100               ADD 1 TO W-DATUM-AA                                        
231200               SUBTRACT 52 FROM W-ANTAL-VECKOR                            
231300             END-PERFORM                                                  
231400           END-IF                                                         
231500           MOVE W-ANTAL-VECKOR TO W-DATUM-VV                              
231600           MOVE W-DATUM-AAVV TO W-MATINFO-TILPSP                          
231700                                WS-TILPSP                                 
231800                                CLAG-TILPSP                               
231900           IF WS-TILPSP > ZERO                                            
232000              MOVE WS-TILPSP TO MOD-TILPSP                                
232100           ELSE                                                           
232200              MOVE SPACE     TO MOD-TILPSP                                
232300           END-IF                                                         
232400           MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-TILPSP-ATTR                  
232500         END-IF                                                           
232600       END-IF                                                             
232700       MOVE JA TO SW-UPPDAT-CLAG                                          
232800     .                                                                    
232900     EJECT                                                                
233000 CCB-AVSLAG-FORSLAG  SECTION.                                             
233100     MOVE 'CCB-AVSLAG-FORSLAG '  TO CURRENT-SECTION                       
233200                                                                          
233300*    FÖRSLAG TILL AVROP (KDAVROP = 1) DELETAS                    *        
233400                                                                          
233500     MOVE 1 TO W-KDAVROP                                                  
233600     PERFORM IMS-GHNP-AVROP-FIRST                                         
233700     PERFORM UNTIL SEGMENT-SAKNAS                                         
233800       PERFORM IMS-DELETE-WDD9                                            
233900       PERFORM IMS-GHNP-AVROP-NEXT                                        
234000     END-PERFORM                                                          
234100     .                                                                    
234200     EJECT                                                                
234300 CCC-LAES-RED-GALLANDE-PLAN SECTION.                                      
234400     MOVE 'CCC-LAES-RED-GALLANDE-PLAN '  TO CURRENT-SECTION               
234500                                                                          
234600     MOVE ZERO TO IX-GAM                                                  
234700     PERFORM IMS-GNP-AVROP-F                                              
234800                                                                          
234900     PERFORM UNTIL SEGMENT-SAKNAS                                         
235000       IF AVROP-KDAVROP = 2                                               
235100         MOVE AVROP-DAAVROP-AVS   TO W-DAAVROP-AVS                        
235200         MOVE W-DAAVROP-AVS-AAVV  TO TMP1-YYWW                            
235300         MOVE W-DATUM-AKTUELLT    TO TMP2-YYWW                            
235400         PERFORM WY2000P3                                                 
235500         IF TMP1-YYWW < TMP2-YYWW                                         
235600           PERFORM S12-GAMMALT-AVROP-TILL-TAB                             
235700         ELSE                                                             
235800           PERFORM S08-NYA-AVROP-TILL-TAB                                 
235900         END-IF                                                           
236000       END-IF                                                             
236100       PERFORM IMS-GHNP-AVROP-OKVAL-NEXT                                  
236200     END-PERFORM                                                          
236300     .                                                                    
236400     EJECT                                                                
236500 CCD-GODKANN-FORSLAG SECTION.                                             
236600     MOVE 'CCD-GODKANN-FORSLAG '  TO CURRENT-SECTION                      
236700                                                                          
236800*    FÖRSLAG GODKÄNNES (KDAVROP=1 ÄNDRAS TILL 2) OCH             *        
236900*    GÄLLANDE FÖRSLAG FÖRKASTAS                                  *        
237000*    FÖR SATSARTIKEL KONTROLLERAS OM FÖRSLAG HAMNAT INOM LT,     *        
237100*    DE GODKÄNNS OCH BEORDRAS OMEDELBART. GAMLA, GODKÄNDA        *        
237200*    AVROP LIGGER KVAR.                                          *        
237300                                                                          
237400     MOVE W-OMSPEC-TISPECST TO W-DATUM-AAVV                               
237500*    IF  W-DATUM-VV = 1                                                   
237600*      IF W-DATUM-AA = 00                                                 
237700*        MOVE 99 TO W-DATUM-AA                                            
237800*      ELSE                                                               
237900*        SUBTRACT 1 FROM W-DATUM-AA                                       
238000*      END-IF                                                             
238100*      IF W-DATUM-AA = 9                                                  
238200*        MOVE 53 TO W-DATUM-VV                                            
238300*      ELSE                                                               
238400*        MOVE 52 TO W-DATUM-VV                                            
238500*      END-IF                                                             
238600*    ELSE                                                                 
238700       SUBTRACT 1 FROM W-DATUM-VV                                         
238800*    END-IF                                                               
238900     MOVE W-DATUM-AAVV TO W-DAAVROP-AAVV                                  
239000     PERFORM S25-SEKELJUSTERA                                             
239100                                                                          
239200     PERFORM IMS-GHNP-AVROP-OKVAL-GR-DAT                                  
239300     PERFORM UNTIL SEGMENT-SAKNAS                                         
239400       IF  W-IDLEVNR = '1002'                                             
239500*******AND AVROP-TIAVROP-AVS <= W-DAGENS-DAT-PLUS-LT                      
239600         PERFORM CCDA-BEH-SATSAVROP-INOM-LT                               
239700       ELSE                                                               
239800         IF AVROP-KDAVROP = 1                                             
239900           MOVE 2 TO AVROP-KDAVROP                                        
240000           PERFORM IMS-REPLACE-WDD9                                       
240100         ELSE                                                             
240200           IF AVROP-KDAVROP = 2                                           
240300             PERFORM IMS-DELETE-WDD9                                      
240400           END-IF                                                         
240500         END-IF                                                           
240600       END-IF                                                             
240700       PERFORM IMS-GHNP-AVROP-OKVAL-NEXT                                  
240800     END-PERFORM                                                          
240900     .                                                                    
241000     EJECT                                                                
241100 CCDA-BEH-SATSAVROP-INOM-LT SECTION.                                      
241200     MOVE 'CCDA-BEH-SATSAVROP-INOM-LT '  TO CURRENT-SECTION               
241300                                                                          
241400     IF AVROP-KDAVROP = 1                                                 
241500       MOVE AVROP-DAAVROP-AVS TO W-DAAVROP                                
241600       MOVE AVROP-TILEVDAG    TO W-TILEVDAG                               
241700       MOVE 2 TO W-KDAVROP                                                
241800       PERFORM IMS-GHNP-AVROP-KVAL-KEY-FIRST                              
241900       IF SEGMENT-FINNS                                                   
242000                                                                          
242100         MOVE AVROP-TILEVDAG TO W-TILEVDAG                                
242200         PERFORM IMS-GNP-SATSBEORDR                                       
242300         IF SEGMENT-SAKNAS                                                
242400            PERFORM CCDAA-BEH-SATS-ORDERNR-NOLL                           
242500         ELSE                                                             
242600***  FINNS REDAN ETT GÄLLANDE AVROP DEN VECKAN KASTAS                     
242700***  FÖRSLAGET                                                            
242800                                                                          
242900            MOVE 1 TO W-KDAVROP                                           
243000            PERFORM IMS-GHNP-AVROP-KVAL-KEY-FIRST                         
243100            IF SEGMENT-FINNS                                              
243200              PERFORM IMS-DELETE-WDD9                                     
243300            END-IF                                                        
243400         END-IF                                                           
243500       ELSE                                                               
243600         MOVE 1 TO W-KDAVROP                                              
243700         PERFORM IMS-GHNP-AVROP-KVAL-KEY-FIRST                            
243800         IF SEGMENT-FINNS                                                 
243900           MOVE 2 TO AVROP-KDAVROP                                        
244000           PERFORM IMS-REPLACE-WDD9                                       
244100         END-IF                                                           
244200       END-IF                                                             
244300     ELSE                                                                 
244400       IF AVROP-KDAVROP = 2                                               
244500         MOVE AVROP-DAAVROP-AVS TO W-DAAVROP                              
244600         MOVE AVROP-TILEVDAG    TO W-TILEVDAG                             
244700         MOVE 2 TO W-KDAVROP                                              
244800         PERFORM IMS-GNP-SATSBEORDR                                       
244900         IF SEGMENT-SAKNAS                                                
245000***         TAG BORT AVROP                                                
245100            PERFORM IMS-GHNP-AVROP-KVAL-KEY-FIRST                         
245200            IF SEGMENT-FINNS                                              
245300              PERFORM IMS-DELETE-WDD9                                     
245400            END-IF                                                        
245500         END-IF                                                           
245600       END-IF                                                             
245700     END-IF                                                               
245800     .                                                                    
245900     EJECT                                                                
246000 CCDAA-BEH-SATS-ORDERNR-NOLL SECTION.                                     
246100     MOVE 'CCDAA-BEH-SATS-ORDERNR-NOLL '  TO CURRENT-SECTION              
246200                                                                          
246210     MOVE 2 TO W-KDAVROP                                                  
246220     PERFORM IMS-GHNP-AVROP-KVAL-KEY-FIRST                                
246230     IF SEGMENT-FINNS                                                     
246240        PERFORM IMS-DELETE-WDD9                                           
246250     END-IF                                                               
246800                                                                          
246900***         LÄGG UPP NY 2:A                                               
247000                                                                          
247100     MOVE 1 TO W-KDAVROP                                                  
247200     PERFORM IMS-GHNP-AVROP-KVAL-KEY-FIRST                                
247300     IF SEGMENT-FINNS                                                     
247400        MOVE 2 TO AVROP-KDAVROP                                           
247500        PERFORM IMS-REPLACE-WDD9                                          
247600     END-IF                                                               
247700     .                                                                    
247800     EJECT                                                                
247900 CCE-TAG-BORT-FRAN-FORSLAGSKOE  Section.                                  
248000     MOVE 'CCE-TAG-BORT-FRAN-FORSLAGSKOE ' TO CURRENT-SECTION             
248100                                                                          
248200     Perform IMS-GHU-WDD601                                               
248300     If SEGMENT-FINNS                                                     
248400       Perform IMS-DELETE-WDD6                                            
248500                                                                          
248600       Move ALL '+'           To MSGI-WMSGINIT                            
248700       Move '001'             To MSGI-KDCALL                              
248800       Move MSG-SIGNON-USERID To MSGI-IDUSER                              
248900       Move MSG-LTERM-NAME    To MSGI-IDLTERM-USER                        
249000       Move '2103'            To MSGI-IDTRANS                             
249100       Call W005INIT Using MSGI-WMSGINIT WDP7-PCB                         
249200       Move MSGI-SPAR-AREA    To SPAR-AREA                                
249300                                                                          
249400       Move Zero              To SPAR-KEY-IDARTNR-NEXT                    
249500                                                                          
249600       Move '002'             To MSGI-KDCALL                              
249700       Move SPAR-AREA         To MSGI-SPAR-AREA                           
249800       Call W005INIT Using MSGI-WMSGINIT WDP7-PCB                         
249900     End-If                                                               
250000     .                                                                    
250100     EJECT                                                                
250200 CD-AVROP-AENDRAT SECTION.                                                
250300     MOVE 'CD-AVROP-AENDRAT '  TO CURRENT-SECTION                         
250400                                                                          
250500     IF W-DAAVROP-AAVV = 9999                                             
250600       SUBTRACT W-KVAVROP FROM W-KVAVROP-GAM (1)                          
250700       IF W-KVAVROP-GAM (1) > ZERO                                        
250800         MOVE JA TO W-AVROP-GAM-AENDRAT (1)                               
250900       ELSE                                                               
251000         MOVE 1 TO IX-GAM                                                 
251100         PERFORM CDA-FLYTTA-GAMLA-AVROP                                   
251200       END-IF                                                             
251300       MOVE 2 TO W-KDAVROP                                                
251400       PERFORM IMS-GHNP-AVROP-FIRST                                       
251500                                                                          
251600       PERFORM UNTIL SEGMENT-SAKNAS OR W-KVAVROP = ZERO                   
251700         IF W-KVAVROP NOT < AVROP-KVAVROP                                 
251800           SUBTRACT AVROP-KVAVROP FROM W-KVAVROP                          
251900           MOVE ZERO TO AVROP-KVAVROP                                     
252000         ELSE                                                             
252100           SUBTRACT W-KVAVROP FROM AVROP-KVAVROP                          
252200           MOVE ZERO TO W-KVAVROP                                         
252300         END-IF                                                           
252400         IF AVROP-KVAVROP > ZERO                                          
252500           PERFORM IMS-REPLACE-WDD9                                       
252600         ELSE                                                             
252700           PERFORM IMS-DELETE-WDD9                                        
252800         END-IF                                                           
252900         PERFORM IMS-GHNP-AVROP-NEXT                                      
253000       END-PERFORM                                                        
253100     ELSE                                                                 
253200       MOVE W-DAAVROP-AAVV     TO TMP1-YYWW                               
253300       MOVE W-DATUM-AKTUELLT   TO TMP2-YYWW                               
253400       PERFORM WY2000P3                                                   
253500       IF TMP1-YYWW < TMP2-YYWW                                           
253600         MOVE ZERO TO IX-GAM                                              
253700         EVALUATE TRUE                                                    
253800           WHEN W-DAAVROP-AAVV = W-TIAVROP-AVS-GAM (1)                    
253900             MOVE +1 TO IX-GAM                                            
254000           WHEN W-DAAVROP-AAVV = W-TIAVROP-AVS-GAM (2)                    
254100             MOVE +2 TO IX-GAM                                            
254200           WHEN W-DAAVROP-AAVV = W-TIAVROP-AVS-GAM (3)                    
254300             MOVE +3 TO IX-GAM                                            
254400           WHEN W-DAAVROP-AAVV = W-TIAVROP-AVS-GAM (4)                    
254500             MOVE +4 TO IX-GAM                                            
254600           WHEN W-DAAVROP-AAVV = W-TIAVROP-AVS-GAM (5)                    
254700             MOVE +5 TO IX-GAM                                            
254800         END-EVALUATE                                                     
254900         IF IX-GAM = ZERO                                                 
255000           PERFORM CDB-TILLAEGG-GAMMALT-AVROP                             
255100         ELSE                                                             
255200           MOVE W-KVAVROP TO W-KVAVROP-GAM (IX-GAM)                       
255300           IF W-KVAVROP-GAM (IX-GAM) = ZERO                               
255400             PERFORM CDA-FLYTTA-GAMLA-AVROP                               
255500           ELSE                                                           
255600             MOVE JA TO W-AVROP-GAM-AENDRAT (IX-GAM)                      
255700           END-IF                                                         
255800         END-IF                                                           
255900       ELSE                                                               
256000         PERFORM S18-TILLAGG-TILL-AVROPSTAB                               
256100       END-IF                                                             
256200       MOVE W-DAAVROP-AAVV      TO TMP1-YYWW                              
256300       MOVE W-OMSPEC-TISPECST   TO TMP2-YYWW                              
256400       PERFORM WY2000P3                                                   
256500       IF  TMP1-YYWW < TMP2-YYWW                                          
256600         MOVE 2 TO W-KDAVROP                                              
256700       ELSE                                                               
256800         IF  W-KDBEHX-PLAN = FORSLAG                                      
256900           MOVE 1 TO W-KDAVROP                                            
257000         ELSE                                                             
257100           MOVE 2 TO W-KDAVROP                                            
257200         END-IF                                                           
257300       END-IF                                                             
257400       MOVE IDARTNR-WS TO W-IDARTNR-D9                                    
257500       MOVE WC-CDC-SE      TO W-IDDC-D9                                   
257600       PERFORM IMS-GU-LEVART                                              
257700       PERFORM IMS-GHNP-AVROP-KVAL-FIRST                                  
257800*      OBS TILEVDAG EJ MED                                                
257900       IF  SEGMENT-SAKNAS                                                 
258000         IF  W-KVAVROP > ZERO                                             
258100           PERFORM CDC-TILLAGG-NYTT-AVROP                                 
258200                                                                          
258300           MOVE AVROP-TIAVRDAT-DISP         TO TMP1-YYMMDD                
258400           MOVE TIDISPIN-AAMMDD             TO TMP2-YYMMDD                
258500           PERFORM WY2000P1                                               
258600           IF TMP1-YYMMDD <= TMP2-YYMMDD                                  
258700             MOVE JA TO FLUPPD-2228                                       
258800           END-IF                                                         
258900                                                                          
259000         END-IF                                                           
259100       ELSE                                                               
259200         MOVE W-KVAVROP TO AVROP-KVAVROP                                  
259300         IF  AVROP-KVAVROP = ZERO                                         
259400           MOVE AVROP-TIAVRDAT-DISP         TO TMP1-YYMMDD                
259500           MOVE TIDISPIN-AAMMDD             TO TMP2-YYMMDD                
259600           PERFORM WY2000P1                                               
259700           IF TMP1-YYMMDD <= TMP2-YYMMDD                                  
259800             MOVE JA TO FLUPPD-2228                                       
259900           END-IF                                                         
260000           PERFORM IMS-DELETE-WDD9                                        
260100                                                                          
260200           MOVE W-IDLEVNR         TO WS-IDLEVNR-EMIL                      
260300           IF (NOT EJ-GODK-EMIL-LEVNR) AND                                
260400              (CLAG-KVQ > ZERO)        AND                                
260500              ((W-ARSBEH / CLAG-KVQ) > 35)                                
260600              MOVE IDARTNR-WS TO W-IDARTNR-D9                             
260700              MOVE WC-CDC-SE  TO W-IDDC-D9                                
260800              PERFORM IMS-GU-LEVART                                       
260900              PERFORM IMS-GHNP-AVROP-KVAL-FIRST                           
261000              PERFORM UNTIL SEGMENT-SAKNAS                                
261100                 PERFORM IMS-DELETE-WDD9                                  
261200                 PERFORM IMS-GHNP-AVROP-KVAL-FIRST                        
261300              END-PERFORM                                                 
261400           END-IF                                                         
261500         ELSE                                                             
261600           MOVE AVROP-TIAVRDAT-DISP         TO TMP1-YYMMDD                
261700           MOVE TIDISPIN-AAMMDD             TO TMP2-YYMMDD                
261800           PERFORM WY2000P1                                               
261900           IF TMP1-YYMMDD <= TMP2-YYMMDD                                  
262000             MOVE JA TO FLUPPD-2228                                       
262100           END-IF                                                         
262200           PERFORM CDD-AENDRA-DAGL-AVROP                                  
262300                                                                          
262400*          MOVE W-IDLEVNR         TO WS-IDLEVNR-EMIL                      
262500*          IF (NOT EJ-GODK-EMIL-LEVNR)                                    
262600*          IF (NOT EJ-GODK-EMIL-LEVNR) AND                                
262700*             (CLAG-KVQ > ZERO)        AND                                
262800*             ((W-ARSBEH / CLAG-KVQ) > 35)                                
262900*             PERFORM CDD-AENDRA-DAGL-AVROP                               
263000*          ELSE                                                           
263100*             PERFORM IMS-REPLACE-WDD9                                    
263200*             DELETE UTSMETADE ?                                          
263300*          END-IF                                                         
263400                                                                          
263500         END-IF                                                           
263600       END-IF                                                             
263700     END-IF                                                               
263800     MOVE JA TO SW-AVROP-AENDRAT                                          
263900     .                                                                    
264000     EJECT                                                                
264100 CDA-FLYTTA-GAMLA-AVROP SECTION.                                          
264200     MOVE 'CDA-FLYTTA-GAMLA-AVROP '  TO CURRENT-SECTION                   
264300                                                                          
264400     MOVE IX-GAM TO IX                                                    
264500                    IX-PLUS-1                                             
264600     ADD 1 TO IX-PLUS-1                                                   
264700                                                                          
264800     PERFORM UNTIL IX-PLUS-1 > MAX-ANT-GAMLA-AVROP                        
264900       MOVE W-KVAVROP-GAM (IX-PLUS-1) TO W-KVAVROP-GAM (IX)               
265000       MOVE W-TIAVROP-AVS-GAM (IX-PLUS-1)                                 
265100                                  TO W-TIAVROP-AVS-GAM (IX)               
265200       ADD 1 TO IX                                                        
265300                IX-PLUS-1                                                 
265400     END-PERFORM                                                          
265500     MOVE ZERO TO W-TIAVROP-AVS-GAM (5)                                   
265600                  W-KVAVROP-GAM (5)                                       
265700     MOVE NEJ TO W-AVROP-GAM-AENDRAT (5)                                  
265800     .                                                                    
265900     EJECT                                                                
266000 CDB-TILLAEGG-GAMMALT-AVROP SECTION.                                      
266100     MOVE 'CDB-TILLAEGG-GAMMALT-AVROP '  TO CURRENT-SECTION               
266200                                                                          
266300     MOVE +5 TO IX-GAM                                                    
266400     MOVE W-DAAVROP-AAVV        TO TMP1-YYWW                              
266500     MOVE W-TIAVROP-AVS-GAM (5) TO TMP2-YYWW                              
266600     PERFORM WY2000P3                                                     
266700     IF TMP1-YYWW < TMP2-YYWW                                             
266800     OR W-TIAVROP-AVS-GAM (5) = ZERO                                      
266900       SUBTRACT 1 FROM IX-GAM                                             
267000       MOVE W-DAAVROP-AAVV        TO TMP1-YYWW                            
267100       MOVE W-TIAVROP-AVS-GAM (4) TO TMP2-YYWW                            
267200       PERFORM WY2000P3                                                   
267300       IF TMP1-YYWW < TMP2-YYWW                                           
267400       OR W-TIAVROP-AVS-GAM (4) = ZERO                                    
267500         SUBTRACT 1 FROM IX-GAM                                           
267600         MOVE W-DAAVROP-AAVV        TO TMP1-YYWW                          
267700         MOVE W-TIAVROP-AVS-GAM (3) TO TMP2-YYWW                          
267800         PERFORM WY2000P3                                                 
267900         IF TMP1-YYWW < TMP2-YYWW                                         
268000         OR W-TIAVROP-AVS-GAM (3) = ZERO                                  
268100           SUBTRACT 1 FROM IX-GAM                                         
268200           MOVE W-DAAVROP-AAVV        TO TMP1-YYWW                        
268300           MOVE W-TIAVROP-AVS-GAM (2) TO TMP2-YYWW                        
268400           PERFORM WY2000P3                                               
268500           IF TMP1-YYWW < TMP2-YYWW                                       
268600           OR W-TIAVROP-AVS-GAM (2) = ZERO                                
268700             SUBTRACT 1 FROM IX-GAM                                       
268800             MOVE W-DAAVROP-AAVV        TO TMP1-YYWW                      
268900             MOVE W-TIAVROP-AVS-GAM (1) TO TMP2-YYWW                      
269000             PERFORM WY2000P3                                             
269100             IF  TMP1-YYWW < TMP2-YYWW                                    
269200             AND W-TIAVROP-AVS-GAM (1) NOT = 9999                         
269300             OR  W-TIAVROP-AVS-GAM (1) = ZERO                             
269400               SUBTRACT 1 FROM IX-GAM                                     
269500             END-IF                                                       
269600           END-IF                                                         
269700         END-IF                                                           
269800       END-IF                                                             
269900     END-IF                                                               
270000                                                                          
270100     IF W-TIAVROP-AVS-GAM (5) = ZERO                                      
270200       MOVE 4 TO IX                                                       
270300       MOVE 5 TO IX-PLUS-1                                                
270400                                                                          
270500       PERFORM UNTIL IX NOT > IX-GAM                                      
270600         MOVE W-KVAVROP-GAM (IX) TO W-KVAVROP-GAM (IX-PLUS-1)             
270700         MOVE W-TIAVROP-AVS-GAM (IX)                                      
270800                             TO W-TIAVROP-AVS-GAM (IX-PLUS-1)             
270900         SUBTRACT 1 FROM IX IX-PLUS-1                                     
271000       END-PERFORM                                                        
271100       MOVE W-DAAVROP-AAVV TO W-TIAVROP-AVS-GAM (IX-PLUS-1)               
271200       MOVE W-KVAVROP TO W-KVAVROP-GAM (IX-PLUS-1)                        
271300       MOVE JA TO W-AVROP-GAM-AENDRAT (IX-PLUS-1)                         
271400     ELSE                                                                 
271500       MOVE 9999 TO W-TIAVROP-AVS-GAM (1)                                 
271600       MOVE W-DAAVROP-AAVV        TO TMP1-YYWW                            
271700       MOVE W-TIAVROP-AVS-GAM (2) TO TMP2-YYWW                            
271800       PERFORM WY2000P3                                                   
271900       IF TMP1-YYWW < TMP2-YYWW                                           
272000         ADD W-KVAVROP TO W-KVAVROP-GAM (1)                               
272100         MOVE JA TO W-AVROP-GAM-AENDRAT (1)                               
272200       ELSE                                                               
272300         ADD W-KVAVROP-GAM (2) TO W-KVAVROP-GAM (1)                       
272400         MOVE 2 TO IX                                                     
272500         MOVE 3 TO IX-PLUS-1                                              
272600                                                                          
272700         PERFORM UNTIL IX NOT < IX-GAM                                    
272800           MOVE W-KVAVROP-GAM(IX-PLUS-1) TO W-KVAVROP-GAM(IX)             
272900           MOVE W-TIAVROP-AVS-GAM(IX-PLUS-1) TO                           
273000                                        W-TIAVROP-AVS-GAM(IX)             
273100           ADD 1 TO IX                                                    
273200                    IX-PLUS-1                                             
273300         END-PERFORM                                                      
273400         MOVE W-DAAVROP-AAVV TO W-TIAVROP-AVS-GAM (IX)                    
273500         MOVE W-KVAVROP TO W-KVAVROP-GAM (IX)                             
273600         MOVE JA TO W-AVROP-GAM-AENDRAT (IX)                              
273700       END-IF                                                             
273800     END-IF                                                               
273900     .                                                                    
274000     EJECT                                                                
274100 CDC-TILLAGG-NYTT-AVROP SECTION.                                          
274200     MOVE 'CDC-TILLAGG-NYTT-AVROP '  TO CURRENT-SECTION                   
274300                                                                          
274400     MOVE W-KDAVROP TO AVROP-KDAVROP                                      
274500     MOVE W-DAAVROP TO AVROP-DAAVROP-AVS                                  
274600     MOVE W-KVAVROP TO AVROP-KVAVROP                                      
274700                                                                          
274800     PERFORM CDCA-TILEVDAG                                                
274900                                                                          
275000*AVS-AAMMDD                                                               
276000                                                                          
276010     MOVE 'YYWWD'             TO DAYS-KDDATFMT1                           
276011     MOVE 'YYMMDD'            TO DAYS-KDDATFMT2                           
276012     COMPUTE WS-DAYS-TIDATE1-AAVVD = 10 * W-DAAVROP-AAVV +                
276013                                     AVROP-TILEVDAG                       
276014     MOVE WS-DAYS-TIDATE1-AAVVD   TO DAYS-TIDATE1                         
276015     MOVE 0                   TO DAYS-KVDAYS                              
276016     MOVE SPACE               TO DAYS-TIDATE2                             
276020                                 DAYS-IDCALEND                            
276030     CALL WZ20DAYS USING DAYS-WZ20DAYS                                    
276040*                                                                         
276050                                                                          
276051     IF DAYS-KDRC = 8                                                     
276052       MOVE 'FEL VID ANROP TILL WZ20DAYS 1'                               
276053                                TO FELTEXT                                
276054       CALL FELLOG                                                        
276055     ELSE                                                                 
276056        MOVE DAYS-TIDATE2(1:6)  TO W-TIAAMMDD-AVS                         
276057                                   W-DADATUM-HELG-AAMMDD                  
276058                                                                          
276059     END-IF                                                               
276060                                                                          
276070                                                                          
276100*INL + DISP                                                               
276200     PERFORM CDCBA-BERAEKNA-INL-DISP-AAMMDD                               
276300                                                                          
276400     MOVE W-IDLEVNR         TO WS-IDLEVNR-EMIL                            
276500     IF (NOT EJ-GODK-EMIL-LEVNR) AND                                      
276600        (CLAG-KVQ > ZERO)        AND                                      
276700        ((W-ARSBEH / CLAG-KVQ) > 35)                                      
276800                                                                          
276900        PERFORM CDCB-SKAPA-DAGL-AVROP                                     
277000                                                                          
277100     ELSE                                                                 
277200                                                                          
277300        MOVE IDARTNR-WS           TO W-IDARTNR-D9                         
277400        MOVE WC-CDC-SE            TO W-IDDC-D9                            
277500        PERFORM IMS-INSERT-AVROP-SEG                                      
277600                                                                          
277700        PERFORM S20-KOLL-LEV-HELGDAG                                      
277800                                                                          
277900        PERFORM S21-KOLL-LEV-BLOCKAD                                      
278000     END-IF                                                               
278100     .                                                                    
278200     EJECT                                                                
278300 CDCA-TILEVDAG            SECTION.                                        
278400     MOVE 'CDCA-TILEVDAG   '  TO CURRENT-SECTION                          
278500                                                                          
278600     MOVE ZERO                     TO AVROP-TILEVDAG                      
278700                                                                          
278800     MOVE +1                       TO IX-DAG                              
278900     PERFORM UNTIL IX-DAG > 5                                             
279000        IF CLAG-TILEVDAG (IX-DAG) > ZERO                                  
279100           MOVE CLAG-TILEVDAG (IX-DAG) TO AVROP-TILEVDAG                  
279200           MOVE +5                 TO IX-DAG                              
279300        END-IF                                                            
279400        ADD +1                     TO IX-DAG                              
279500     END-PERFORM                                                          
279600                                                                          
279700     IF AVROP-TILEVDAG = ZERO                                             
279800        PERFORM IMS-GU-LEVA01-WDF101-SHIP                                 
279900        IF SEGMENT-FINNS                                                  
280000          MOVE +1                  TO IX-DAG                              
280100          PERFORM UNTIL IX-DAG > 5                                        
280200             IF F1-LEV-TILEVDAG (IX-DAG) > ZERO                           
280300                MOVE F1-LEV-TILEVDAG (IX-DAG) TO AVROP-TILEVDAG           
280400                MOVE +5            TO IX-DAG                              
280500             END-IF                                                       
280600             ADD +1                TO IX-DAG                              
280700          END-PERFORM                                                     
280800                                                                          
280900          IF AVROP-TILEVDAG = ZERO                                        
281000             MOVE +1               TO AVROP-TILEVDAG                      
281100          END-IF                                                          
281200        ELSE                                                              
281300          MOVE +1                  TO AVROP-TILEVDAG                      
281400        END-IF                                                            
281500     END-IF                                                               
281600     .                                                                    
281700     EJECT                                                                
281800 CDCB-SKAPA-DAGL-AVROP         SECTION.                                   
281900     MOVE 'CDCB-SKAPA-DAGL-AVROP '  TO CURRENT-SECTION                    
282000                                                                          
282100     MOVE ZERO                TO WS-TILEVDAG (1)                          
282200                                 WS-TILEVDAG (2)                          
282300                                 WS-TILEVDAG (3)                          
282400                                 WS-TILEVDAG (4)                          
282500                                 WS-TILEVDAG (5)                          
282600     MOVE ZERO                TO ANT-LEVDAG                               
282700     MOVE ZERO                TO PASS-TILEVDAG                            
282800                                                                          
282900     MOVE +1                  TO IX-DAG                                   
283000     PERFORM UNTIL IX-DAG > 5                                             
283100        IF CLAG-TILEVDAG (IX-DAG) > ZERO                                  
283200         IF W-DAAVROP-AAVV = W-DATUM-AKTUELLT                             
283300          IF CLAG-TILEVDAG (IX-DAG) >= W-TID-AKTUELLT                     
283400           MOVE CLAG-TILEVDAG (IX-DAG) TO WS-TILEVDAG(IX-DAG)             
283500           ADD +1             TO ANT-LEVDAG                               
283600          ELSE                                                            
283700           IF PASS-TILEVDAG = ZERO                                        
283800             MOVE CLAG-TILEVDAG (IX-DAG) TO PASS-TILEVDAG                 
283900           END-IF                                                         
284000          END-IF                                                          
284100         ELSE                                                             
284200           MOVE CLAG-TILEVDAG (IX-DAG) TO WS-TILEVDAG(IX-DAG)             
284300           ADD +1             TO ANT-LEVDAG                               
284400         END-IF                                                           
284500        END-IF                                                            
284600        ADD +1                TO IX-DAG                                   
284700     END-PERFORM                                                          
284800                                                                          
284900     IF ANT-LEVDAG = ZERO                                                 
285000        PERFORM IMS-GU-LEVA01-WDF101-SHIP                                 
285100        IF SEGMENT-FINNS                                                  
285200          MOVE +1             TO IX-DAG                                   
285300          PERFORM UNTIL IX-DAG > 5                                        
285400             IF F1-LEV-TILEVDAG (IX-DAG) > ZERO                           
285500              IF W-DAAVROP-AAVV = W-DATUM-AKTUELLT                        
285600               IF F1-LEV-TILEVDAG (IX-DAG) >= W-TID-AKTUELLT              
285700                MOVE F1-LEV-TILEVDAG (IX-DAG)                             
285800                              TO WS-TILEVDAG (IX-DAG)                     
285900                ADD +1        TO ANT-LEVDAG                               
286000               ELSE                                                       
286100                IF PASS-TILEVDAG = ZERO                                   
286200                  MOVE F1-LEV-TILEVDAG (IX-DAG) TO PASS-TILEVDAG          
286300                END-IF                                                    
286400               END-IF                                                     
286500              ELSE                                                        
286600                MOVE F1-LEV-TILEVDAG (IX-DAG)                             
286700                              TO WS-TILEVDAG (IX-DAG)                     
286800                ADD +1        TO ANT-LEVDAG                               
286900              END-IF                                                      
287000             END-IF                                                       
287100             ADD +1           TO IX-DAG                                   
287200          END-PERFORM                                                     
287300                                                                          
287400          IF ANT-LEVDAG = ZERO                                            
287500            IF PASS-TILEVDAG > ZERO AND PASS-TILEVDAG < 6                 
287600             MOVE PASS-TILEVDAG TO WS-TILEVDAG (PASS-TILEVDAG)            
287700             MOVE +1            TO ANT-LEVDAG                             
287800            ELSE                                                          
287900             MOVE +1          TO WS-TILEVDAG (1)                          
288000                                 ANT-LEVDAG                               
288100            END-IF                                                        
288200          END-IF                                                          
288300        ELSE                                                              
288400          MOVE +1             TO WS-TILEVDAG (1)                          
288500                                 ANT-LEVDAG                               
288600        END-IF                                                            
288700     END-IF                                                               
288800                                                                          
288900*KVAVROP/DAG                                                              
289000     MOVE ZERO                  TO WS-KVAVROP (1)                         
289100                                   WS-KVAVROP (2)                         
289200                                   WS-KVAVROP (3)                         
289300                                   WS-KVAVROP (4)                         
289400                                   WS-KVAVROP (5)                         
289500                                                                          
289600     IF CLAG-KVPALL < +1                                                  
289700        MOVE +1                 TO WS-KVPALL                              
289800     ELSE                                                                 
289900        MOVE CLAG-KVPALL        TO WS-KVPALL                              
290000     END-IF                                                               
290100                                                                          
290200     IF W-KVAVROP > ZERO                                                  
290300*       PERFORM UNTIL (WS-KVAVROP(1) + WS-KVAVROP(2) +                    
290400*               WS-KVAVROP(3) + WS-KVAVROP(4) + WS-KVAVROP(5))            
290500*               NOT < W-KVAVROP                                           
290600          MOVE NEJ TO SW-TRAFF-DAG                                        
290700          MOVE +1               TO IX-DAG                                 
290800          PERFORM UNTIL    IX-DAG  > 5                                    
290900            IF WS-TILEVDAG (IX-DAG) > ZERO                                
291000            AND SW-TRAFF-DAG = NEJ                                        
291100              MOVE W-KVAVROP TO WS-KVAVROP (IX-DAG)                       
291200              MOVE JA TO SW-TRAFF-DAG                                     
291300*              IF (WS-KVAVROP(1) + WS-KVAVROP(2) + WS-KVAVROP(3) +        
291400*                  WS-KVAVROP(4) + WS-KVAVROP(5)) < W-KVAVROP             
291500*                  ADD WS-KVPALL TO WS-KVAVROP (IX-DAG)                   
291600*                                                                         
291700*                IF (WS-KVAVROP(1) + WS-KVAVROP(2) + WS-KVAVROP(3)        
291800*                  + WS-KVAVROP(4) + WS-KVAVROP(5)) > W-KVAVROP           
291900*                  COMPUTE WS-KVAVROP-DIFF =                              
292000*                   (WS-KVAVROP(1) + WS-KVAVROP(2) + WS-KVAVROP(3)        
292100*                  + WS-KVAVROP(4) + WS-KVAVROP(5)) - W-KVAVROP           
292200*                  SUBTRACT WS-KVAVROP-DIFF FROM                          
292300*                           WS-KVAVROP (IX-DAG)                           
292400*                END-IF                                                   
292500*                                                                         
292600*              END-IF                                                     
292700            END-IF                                                        
292800            ADD +1              TO IX-DAG                                 
292900          END-PERFORM                                                     
293000*       END-PERFORM                                                       
293100     END-IF                                                               
293200*    COMPUTE W-KVAVROP = WS-KVAVROP (1)                                   
293300*                      + WS-KVAVROP (2)                                   
293400*                      + WS-KVAVROP (3)                                   
293500*                      + WS-KVAVROP (4)                                   
293600*                      + WS-KVAVROP (5)                                   
293700                                                                          
293800*X* ?  ATT FÅ RÄTT KVAVROP UT PÅ BILDEN                                   
293900     MOVE AVROP-DAAVROP-AVS  TO W-DAAVROP-AVS                             
294000     MOVE W-DAAVROP-AVS-AAVV TO W-DATUM-AAVV                              
294100     PERFORM S09-BERAKNA-INDEX-I-AVROPSTAB                                
294200     IF IY-PERIOD > ZERO AND                                              
294300        IY-PERIOD NOT > MAX-ANT-PERIODER-I-TAB                            
294400       MOVE W-KVAVROP TO W-KVAVROP-TAB (IY-PERIOD, IX-VECKA)              
294500     END-IF                                                               
294600*X* ?                                                                     
294700                                                                          
294800*AVS-AAVV                                                                 
294900     MOVE +1                    TO IX-DAG                                 
295000     PERFORM UNTIL    (IX-DAG) > 5                                        
295100       IF WS-TILEVDAG (IX-DAG) > ZERO AND                                 
295200          WS-KVAVROP  (IX-DAG) > ZERO                                     
295300*AVS-AAMMDD                                                               
296202                                                                          
296210          MOVE 'YYWWD'             TO DAYS-KDDATFMT1                      
296220          MOVE 'YYMMDD'            TO DAYS-KDDATFMT2                      
296230          COMPUTE WS-DAYS-TIDATE1-AAVVD = 10 * W-DAAVROP-AAVV +           
296240                                      WS-TILEVDAG (IX-DAG)                
296250          MOVE WS-DAYS-TIDATE1-AAVVD  TO DAYS-TIDATE1                     
296260          MOVE 0                   TO DAYS-KVDAYS                         
296270          MOVE SPACE               TO DAYS-TIDATE2                        
296280                                      DAYS-IDCALEND                       
296290          CALL WZ20DAYS USING DAYS-WZ20DAYS                               
296291*                                                                         
296292                                                                          
296293          IF DAYS-KDRC = 8                                                
296294            MOVE 'FEL VID ANROP TILL WZ20DAYS 2'                          
296295                                     TO FELTEXT                           
296296            CALL FELLOG                                                   
296297          ELSE                                                            
296298             MOVE DAYS-TIDATE2(1:6)  TO W-TIAAMMDD-AVS                    
296299                                        W-DADATUM-HELG-AAMMDD             
296300                                                                          
296301          END-IF                                                          
296302                                                                          
296310                                                                          
296400*INL + DISP                                                               
296500          PERFORM CDCBA-BERAEKNA-INL-DISP-AAMMDD                          
296600                                                                          
296700                                                                          
296800          MOVE WS-TILEVDAG (IX-DAG) TO AVROP-TILEVDAG                     
296900          MOVE WS-KVAVROP (IX-DAG)  TO AVROP-KVAVROP                      
297000                                                                          
297100          MOVE IDARTNR-WS         TO W-IDARTNR-D9                         
297200          MOVE WC-CDC-SE          TO W-IDDC-D9                            
297300          PERFORM IMS-INSERT-AVROP-SEG                                    
297400                                                                          
297500          PERFORM S20-KOLL-LEV-HELGDAG                                    
297600                                                                          
297700          PERFORM S21-KOLL-LEV-BLOCKAD                                    
297800       END-IF                                                             
297900                                                                          
298000       ADD +1 TO IX-DAG                                                   
298100     END-PERFORM                                                          
298200     .                                                                    
298300     EJECT                                                                
298400 CDCBA-BERAEKNA-INL-DISP-AAMMDD SECTION.                                  
298500     MOVE 'CDCBA-BERAEKNA-INL-DISP-AAMMDD'  TO CURRENT-SECTION            
298600                                                                          
298700*INL                                                                      
298800          MOVE 2                   TO WORK-KDCALL                         
298900          MOVE '11'                TO WORK-IDDC                           
299000          MOVE W-TIAAMMDD-AVS      TO WORK-TIAAMMDD-FOM                   
299100          MOVE W-MATINFO-KVDAGAR-TT   TO WORK-KVWORKD                     
299200          ADD +1                   TO WORK-KVWORKD                        
299300          CALL WORKDAY USING  WORK-KDCALL                                 
299400               WORK-DATE-AREA WORK-KDSVAR                                 
299500          MOVE WORK-TIAAMMDD-TOM   TO AVROP-TIAVRDAT-INL                  
299600*DISP                                                                     
299700          MOVE 2                   TO WORK-KDCALL                         
299800          MOVE '11'                TO WORK-IDDC                           
299900          MOVE AVROP-TIAVRDAT-INL  TO WORK-TIAAMMDD-FOM                   
300000          MOVE W-MATINFO-KVDAGAR-INLEV  TO WORK-KVWORKD                   
300100          ADD +1                   TO WORK-KVWORKD                        
300200          CALL WORKDAY USING  WORK-KDCALL                                 
300300               WORK-DATE-AREA WORK-KDSVAR                                 
300400          MOVE WORK-TIAAMMDD-TOM   TO AVROP-TIAVRDAT-DISP                 
300500     .                                                                    
300600     EJECT                                                                
300700 CDD-AENDRA-DAGL-AVROP         SECTION.                                   
300800     MOVE 'CDD-AENDRA-DAGL-AVROP '  TO CURRENT-SECTION                    
300900                                                                          
301000     MOVE ZERO                TO WS-TILEVDAG (1)                          
301100                                 WS-TILEVDAG (2)                          
301200                                 WS-TILEVDAG (3)                          
301300                                 WS-TILEVDAG (4)                          
301400                                 WS-TILEVDAG (5)                          
301500     MOVE ZERO                TO ANT-LEVDAG                               
301600     MOVE ZERO                TO PASS-TILEVDAG                            
301700                                                                          
301800     MOVE +1                  TO IX-DAG                                   
301900     PERFORM UNTIL IX-DAG > 5                                             
302000        IF CLAG-TILEVDAG (IX-DAG) > ZERO                                  
302100         IF W-DAAVROP-AAVV = W-DATUM-AKTUELLT                             
302200          IF CLAG-TILEVDAG (IX-DAG) >= W-TID-AKTUELLT                     
302300           MOVE CLAG-TILEVDAG (IX-DAG) TO WS-TILEVDAG(IX-DAG)             
302400           ADD +1             TO ANT-LEVDAG                               
302500          ELSE                                                            
302600           IF PASS-TILEVDAG = ZERO                                        
302700             MOVE CLAG-TILEVDAG (IX-DAG) TO PASS-TILEVDAG                 
302800           END-IF                                                         
302900          END-IF                                                          
303000         ELSE                                                             
303100           MOVE CLAG-TILEVDAG (IX-DAG) TO WS-TILEVDAG(IX-DAG)             
303200           ADD +1             TO ANT-LEVDAG                               
303300         END-IF                                                           
303400        END-IF                                                            
303500        ADD +1                TO IX-DAG                                   
303600     END-PERFORM                                                          
303700                                                                          
303800     IF ANT-LEVDAG = ZERO                                                 
303900        PERFORM IMS-GU-LEVA01-WDF101-SHIP                                 
304000        IF SEGMENT-FINNS                                                  
304100          MOVE +1             TO IX-DAG                                   
304200          PERFORM UNTIL IX-DAG > 5                                        
304300             IF F1-LEV-TILEVDAG (IX-DAG) > ZERO                           
304400              IF W-DAAVROP-AAVV = W-DATUM-AKTUELLT                        
304500               IF F1-LEV-TILEVDAG (IX-DAG) >= W-TID-AKTUELLT              
304600                MOVE F1-LEV-TILEVDAG (IX-DAG)                             
304700                              TO WS-TILEVDAG (IX-DAG)                     
304800                ADD +1        TO ANT-LEVDAG                               
304900               ELSE                                                       
305000                IF PASS-TILEVDAG = ZERO                                   
305100                  MOVE F1-LEV-TILEVDAG (IX-DAG) TO PASS-TILEVDAG          
305200                END-IF                                                    
305300               END-IF                                                     
305400              ELSE                                                        
305500                MOVE F1-LEV-TILEVDAG (IX-DAG)                             
305600                              TO WS-TILEVDAG (IX-DAG)                     
305700                ADD +1        TO ANT-LEVDAG                               
305800              END-IF                                                      
305900             END-IF                                                       
306000             ADD +1           TO IX-DAG                                   
306100          END-PERFORM                                                     
306200                                                                          
306300          IF ANT-LEVDAG = ZERO                                            
306400            IF PASS-TILEVDAG > ZERO AND PASS-TILEVDAG < 6                 
306500             MOVE PASS-TILEVDAG TO WS-TILEVDAG (PASS-TILEVDAG)            
306600             MOVE +1            TO ANT-LEVDAG                             
306700            ELSE                                                          
306800             MOVE +1          TO WS-TILEVDAG (1)                          
306900                                 ANT-LEVDAG                               
307000            END-IF                                                        
307100          END-IF                                                          
307200        ELSE                                                              
307300          MOVE +1             TO WS-TILEVDAG (1)                          
307400                                 ANT-LEVDAG                               
307500        END-IF                                                            
307600     END-IF                                                               
307700                                                                          
307800*KVAVROP/DAG                                                              
307900     MOVE ZERO                  TO WS-KVAVROP (1)                         
308000                                   WS-KVAVROP (2)                         
308100                                   WS-KVAVROP (3)                         
308200                                   WS-KVAVROP (4)                         
308300                                   WS-KVAVROP (5)                         
308400                                                                          
308500     IF CLAG-KVPALL < +1                                                  
308600        MOVE +1                 TO WS-KVPALL                              
308700     ELSE                                                                 
308800        MOVE CLAG-KVPALL        TO WS-KVPALL                              
308900     END-IF                                                               
309000                                                                          
309100     IF W-KVAVROP > ZERO                                                  
309200*       PERFORM UNTIL (WS-KVAVROP(1) + WS-KVAVROP(2) +                    
309300*               WS-KVAVROP(3) + WS-KVAVROP(4) + WS-KVAVROP(5))            
309400*               NOT < W-KVAVROP                                           
309500          MOVE NEJ TO SW-TRAFF-DAG                                        
309600          MOVE +1               TO IX-DAG                                 
309700          PERFORM UNTIL    IX-DAG  > 5                                    
309800            IF WS-TILEVDAG (IX-DAG) > ZERO                                
309900            AND SW-TRAFF-DAG = NEJ                                        
310000              MOVE W-KVAVROP TO WS-KVAVROP (IX-DAG)                       
310100              MOVE JA TO SW-TRAFF-DAG                                     
310200*              IF (WS-KVAVROP(1) + WS-KVAVROP(2) + WS-KVAVROP(3) +        
310300*                  WS-KVAVROP(4) + WS-KVAVROP(5)) < W-KVAVROP             
310400*                  ADD WS-KVPALL TO WS-KVAVROP (IX-DAG)                   
310500*                                                                         
310600*                IF (WS-KVAVROP(1) + WS-KVAVROP(2) + WS-KVAVROP(3)        
310700*                  + WS-KVAVROP(4) + WS-KVAVROP(5)) > W-KVAVROP           
310800*                  COMPUTE WS-KVAVROP-DIFF =                              
310900*                   (WS-KVAVROP(1) + WS-KVAVROP(2) + WS-KVAVROP(3)        
311000*                  + WS-KVAVROP(4) + WS-KVAVROP(5)) - W-KVAVROP           
311100*                  SUBTRACT WS-KVAVROP-DIFF FROM                          
311200*                           WS-KVAVROP (IX-DAG)                           
311300*                END-IF                                                   
311400*                                                                         
311500*              END-IF                                                     
311600            END-IF                                                        
311700            ADD +1              TO IX-DAG                                 
311800          END-PERFORM                                                     
311900*       END-PERFORM                                                       
312000     END-IF                                                               
312100*    IF W-KVAVROP > ZERO                                                  
312200*       PERFORM UNTIL (WS-KVAVROP(1) + WS-KVAVROP(2) +                    
312300*               WS-KVAVROP(3) + WS-KVAVROP(4) + WS-KVAVROP(5))            
312400*               NOT < W-KVAVROP                                           
312500*         MOVE +1               TO IX-DAG                                 
312600*         PERFORM UNTIL    IX-DAG  > 5                                    
312700*           IF WS-TILEVDAG (IX-DAG) > ZERO                                
312800*              IF (WS-KVAVROP(1) + WS-KVAVROP(2) + WS-KVAVROP(3) +        
312900*                  WS-KVAVROP(4) + WS-KVAVROP(5)) < W-KVAVROP             
313000*                  ADD WS-KVPALL TO WS-KVAVROP (IX-DAG)                   
313100*                                                                         
313200*                IF (WS-KVAVROP(1) + WS-KVAVROP(2) + WS-KVAVROP(3)        
313300*                  + WS-KVAVROP(4) + WS-KVAVROP(5)) > W-KVAVROP           
313400*                  COMPUTE WS-KVAVROP-DIFF =                              
313500*                   (WS-KVAVROP(1) + WS-KVAVROP(2) + WS-KVAVROP(3)        
313600*                  + WS-KVAVROP(4) + WS-KVAVROP(5)) - W-KVAVROP           
313700*                  SUBTRACT WS-KVAVROP-DIFF FROM                          
313800*                           WS-KVAVROP (IX-DAG)                           
313900*                END-IF                                                   
314000*                                                                         
314100*              END-IF                                                     
314200*           END-IF                                                        
314300*           ADD +1              TO IX-DAG                                 
314400*         END-PERFORM                                                     
314500*       END-PERFORM                                                       
314600*    END-IF                                                               
314700*    COMPUTE W-KVAVROP = WS-KVAVROP (1)                                   
314800*                      + WS-KVAVROP (2)                                   
314900*                      + WS-KVAVROP (3)                                   
315000*                      + WS-KVAVROP (4)                                   
315100*                      + WS-KVAVROP (5)                                   
315200                                                                          
315300*X* ?  ATT FÅ RÄTT KVAVROP UT PÅ BILDEN                                   
315400     MOVE AVROP-DAAVROP-AVS  TO W-DAAVROP-AVS                             
315500     MOVE W-DAAVROP-AVS-AAVV TO W-DATUM-AAVV                              
315600     PERFORM S09-BERAKNA-INDEX-I-AVROPSTAB                                
315700     IF IY-PERIOD > ZERO AND                                              
315800        IY-PERIOD NOT > MAX-ANT-PERIODER-I-TAB                            
315900       MOVE W-KVAVROP TO W-KVAVROP-TAB (IY-PERIOD, IX-VECKA)              
316000     END-IF                                                               
316100*X* ?                                                                     
316200                                                                          
316300                                                                          
316400*AVS-AAVV                                                                 
316500     MOVE +1                    TO IX-DAG                                 
316600     PERFORM UNTIL    (IX-DAG) > 5                                        
316700       IF WS-TILEVDAG (IX-DAG) > ZERO AND                                 
316800          WS-KVAVROP  (IX-DAG) > ZERO                                     
316900*AVS-AAMMDD                                                               
317810                                                                          
317812          MOVE 'YYWWD'             TO DAYS-KDDATFMT1                      
317813          MOVE 'YYMMDD'            TO DAYS-KDDATFMT2                      
317814          COMPUTE WS-DAYS-TIDATE1-AAVVD = 10 * W-DAAVROP-AAVV +           
317815                                          WS-TILEVDAG (IX-DAG)            
317816          MOVE WS-DAYS-TIDATE1-AAVVD   TO DAYS-TIDATE1                    
317817          MOVE 0                   TO DAYS-KVDAYS                         
317818          MOVE SPACE               TO DAYS-TIDATE2                        
317819                                      DAYS-IDCALEND                       
317820          CALL WZ20DAYS USING DAYS-WZ20DAYS                               
317821                                                                          
317822          IF DAYS-KDRC = 8                                                
317823            MOVE 'FEL VID ANROP TILL WZ20DAYS 3'                          
317824                                     TO FELTEXT                           
317825            CALL FELLOG                                                   
317826          ELSE                                                            
317827             MOVE DAYS-TIDATE2(1:6)  TO W-TIAAMMDD-AVS                    
317828                                        W-DADATUM-HELG-AAMMDD             
317829                                                                          
317830          END-IF                                                          
317831                                                                          
317840                                                                          
317900*INL                                                                      
318000          MOVE 2                   TO WORK-KDCALL                         
318100          MOVE '11'                TO WORK-IDDC                           
318200          MOVE W-TIAAMMDD-AVS      TO WORK-TIAAMMDD-FOM                   
318300          MOVE W-MATINFO-KVDAGAR-TT   TO WORK-KVWORKD                     
318400          ADD +1                   TO WORK-KVWORKD                        
318500          CALL WORKDAY USING  WORK-KDCALL                                 
318600               WORK-DATE-AREA WORK-KDSVAR                                 
318700          MOVE WORK-TIAAMMDD-TOM   TO AVROP-TIAVRDAT-INL                  
318710                                                                          
318800*DISP                                                                     
318900          MOVE 2                   TO WORK-KDCALL                         
319000          MOVE '11'                TO WORK-IDDC                           
319100          MOVE AVROP-TIAVRDAT-INL  TO WORK-TIAAMMDD-FOM                   
319200          MOVE W-MATINFO-KVDAGAR-INLEV  TO WORK-KVWORKD                   
319300          ADD +1                   TO WORK-KVWORKD                        
319400          CALL WORKDAY USING  WORK-KDCALL                                 
319500               WORK-DATE-AREA WORK-KDSVAR                                 
319600          MOVE WORK-TIAAMMDD-TOM   TO AVROP-TIAVRDAT-DISP                 
319700                                                                          
319800          MOVE WS-KVAVROP  (IX-DAG) TO AVROP-KVAVROP                      
319900          MOVE WS-TILEVDAG (IX-DAG) TO AVROP-TILEVDAG                     
320000                                       W-TILEVDAG                         
320100*                                                                         
320200          MOVE IDARTNR-WS           TO W-IDARTNR-D9                       
320300          MOVE WC-CDC-SE            TO W-IDDC-D9                          
320400          PERFORM IMS-GHU-AVROP-KVAL-KEY                                  
320500*                 LÄSNING AV DAAVROP-AVS+TILEVDAG TILL ANNAN AREA         
320600          IF SEGMENT-FINNS                                                
320700             PERFORM IMS-REPLACE-WDD9                                     
320800             PERFORM S20-KOLL-LEV-HELGDAG                                 
320900             PERFORM S21-KOLL-LEV-BLOCKAD                                 
321000          ELSE                                                            
321100             PERFORM IMS-INSERT-AVROP-SEG                                 
321200             PERFORM S20-KOLL-LEV-HELGDAG                                 
321300             PERFORM S21-KOLL-LEV-BLOCKAD                                 
321400          END-IF                                                          
321500       ELSE                                                               
321600**010115**MOVE WS-TILEVDAG (IX-DAG) TO W-TILEVDAG                         
321700          MOVE IX-DAG               TO W-TILEVDAG                         
321800          MOVE IDARTNR-WS           TO W-IDARTNR-D9                       
321900          MOVE WC-CDC-SE            TO W-IDDC-D9                          
322000          PERFORM IMS-GHU-AVROP-KVAL-KEY2                                 
322100          IF SEGMENT-FINNS                                                
322200             PERFORM IMS-DELETE-AVROP                                     
322300          END-IF                                                          
322400       END-IF                                                             
322500                                                                          
322600       ADD +1 TO IX-DAG                                                   
322700     END-PERFORM                                                          
322800     .                                                                    
322900     EJECT                                                                
323000                                                                          
323100 CE-UPDATE-TEARTNOT SECTION.                                              
323200     MOVE 'CE-UPDATE-TEARTNOT     '  TO CURRENT-SECTION                   
323300                                                                          
323400     IF MID-TEARTNOT1 = ALL '+'  AND                                      
323500        MID-TEARTNOT2 = ALL '+'                                           
323600        CONTINUE                                                          
323700     ELSE                                                                 
323800        IF MID-TEARTNOT1 NOT = ALL '+'                                    
323900           MOVE 1                      TO W-KDNOTTYP                      
324000           PERFORM IMS-GHNP-WDK625                                        
324100           IF MID-TEARTNOT1  = SPACE                                      
324200              IF SEGMENT-FINNS                                            
324300                 PERFORM IMS-DELETE-WDK625                                
324400                 MOVE SPACE            TO MOD-TEARTNOT1-IN                
324500              ELSE                                                        
324600                 CONTINUE                                                 
324700              END-IF                                                      
324800           ELSE                                                           
324900              MOVE MID-TEARTNOT1       TO NOT-TEARTNOT                    
325000                                          MOD-TEARTNOT1-IN                
325100              MOVE 1                   TO NOT-KDNOTTYP                    
325200              IF SEGMENT-FINNS                                            
325300                 PERFORM IMS-REPL-WDK625                                  
325400              ELSE                                                        
325500                 PERFORM IMS-ISRT-WDK625                                  
325600              END-IF                                                      
325700           END-IF                                                         
325800        END-IF                                                            
325900                                                                          
326000        IF MID-TEARTNOT2 NOT = ALL '+'                                    
326100           MOVE 2                      TO W-KDNOTTYP                      
326200           PERFORM IMS-GHNP-WDK625                                        
326300           IF MID-TEARTNOT2  = SPACE                                      
326400              IF SEGMENT-FINNS                                            
326500                 PERFORM IMS-DELETE-WDK625                                
326600                 MOVE SPACE            TO MOD-TEARTNOT2-IN                
326700              ELSE                                                        
326800                 CONTINUE                                                 
326900              END-IF                                                      
327000           ELSE                                                           
327100              MOVE MID-TEARTNOT2       TO NOT-TEARTNOT                    
327200                                          MOD-TEARTNOT2-IN                
327300              MOVE 2                   TO NOT-KDNOTTYP                    
327310              IF SEGMENT-FINNS                                            
327320                 PERFORM IMS-REPL-WDK625                                  
327330              ELSE                                                        
327340                 PERFORM IMS-ISRT-WDK625                                  
327350              END-IF                                                      
327360           END-IF                                                         
327370        END-IF                                                            
327380     END-IF                                                               
327390     .                                                                    
327400     EJECT                                                                
328400 CF-KOEP-AENDRAT SECTION.                                                 
328500     MOVE 'CF-KOEP-AENDRAT '  TO CURRENT-SECTION                          
328600                                                                          
328700     MOVE IDARTNR-WS TO W-IDARTNR-D9                                      
328800     MOVE WC-CDC-SE  TO W-IDDC-D9                                         
328900     PERFORM IMS-GU-LEVART                                                
329000     MOVE JA TO SW-KOEP-AENDRAT                                           
329100     MOVE W-LEVNR-KVBR TO W-KVBR                                          
329200     ADD W-KVBEST-PL TO W-KVBR                                            
329300     MOVE W-KVBR TO MOD-KVBR                                              
329400     MOVE W-KVBEST-PL TO MOD-KVBEST-PL                                    
329500                         W-OMSPEC-KVBEST-PL                               
329600     MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-DATA-GRP2-ATTR                     
329700     PERFORM IMS-GHNP-OMSPEC-SEG-FIRST                                    
329800     IF  SEGMENT-SAKNAS                                                   
329900       PERFORM S03-NOLLSTALL-OMSPEC-SEG                                   
330000     END-IF                                                               
330100     IF  W-KDBEHX-PLAN = FORSLAG                                          
330200       IF  W-KVBEST-PL = ZERO AND MID-KOMKOD = '2'                        
330300         MOVE ZERO TO OMSPEC-KVBEST-PL                                    
330400                      OMSPEC-KDPLKOEP                                     
330500       END-IF                                                             
330600       IF  W-KVBEST-PL > ZERO                                             
330700         MOVE W-KVBEST-PL TO OMSPEC-KVBEST-PL                             
330800         MOVE 2 TO OMSPEC-KDPLKOEP                                        
330900       END-IF                                                             
331000     ELSE                                                                 
331100       MOVE W-KVBEST-PL TO OMSPEC-KVBEST-PL                               
331200       MOVE ORSAK-KOEP TO W-ORSAKS-KOD                                    
331300       IF  W-KVBEST-PL = ZERO                                             
331400         MOVE ZERO TO OMSPEC-KDPLKOEP                                     
331500         PERFORM S06-SUBTRACT-FRAN-ORSAKOMSPEC                            
331600         MOVE ZERO TO W-OMSPEC-KDPLKOEP                                   
331700       ELSE                                                               
331800         MOVE 2 TO OMSPEC-KDPLKOEP                                        
331900         PERFORM S05-ADD-TILL-ORSAKOMSPEC                                 
332000         MOVE 2 TO W-OMSPEC-KDPLKOEP                                      
332100       END-IF                                                             
332200     END-IF                                                               
332300     IF SEGMENT-FINNS                                                     
332400       PERFORM IMS-REPLACE-WDD9                                           
332500     ELSE                                                                 
332600       IF OMSPEC-KVBEST-PL > ZERO                                         
332700         PERFORM IMS-INSERT-OMSPEC-SEG                                    
332800       END-IF                                                             
332900     END-IF                                                               
333000     IF W-KVBEST-PL > ZERO                                                
333100       IF W-KDBEHX-PLAN = FORSLAG AND MID-KOMKOD = '1'                    
333220         PERFORM S01-GEN-UTSKRIFTSBEGAERAN                                
333300       END-IF                                                             
333400     ELSE                                                                 
333520       PERFORM S01-GEN-UTSKRIFTSBEGAERAN                                  
333600     END-IF                                                               
333700     .                                                                    
333800     EJECT                                                                
333900 CG-INITIERA-TABELL SECTION.                                              
334000     MOVE 'CG-INITIERA-TABELL '   TO CURRENT-SECTION                      
334100                                                                          
334200     MOVE W-START-PER-AA TO W-PERIOD-AA                                   
334300     MOVE W-START-PER-PP TO W-PERIOD-PP                                   
334400     MOVE W-PERIOD-AAPP  TO W-PERIOD-AAPP-START                           
334500     MOVE 1 TO IY                                                         
334600                                                                          
334700     PERFORM UNTIL IY > MAX-ANT-PERIODER-I-TAB                            
334800       MOVE W-PERIOD-AA TO MOD-PERIOD-AA  (IY)                            
334900       MOVE W-PERIOD-PP TO MOD-PERIOD-PP  (IY)                            
335000                           W-PERIOD-TAB-RAD (IY)                          
335100       MOVE PARENTES    TO MOD-PERIOD-PARENTES(IY)                        
335200                                                                          
335300       IF W-PERIOD-PP = 12                                                
335400         ADD 1  TO W-PERIOD-AA                                            
335500         MOVE 1 TO W-PERIOD-PP                                            
335600       ELSE                                                               
335700         ADD 1  TO W-PERIOD-PP                                            
335800       END-IF                                                             
335900       ADD 1 TO IY                                                        
336000     END-PERFORM                                                          
336100     .                                                                    
336200     EJECT                                                                
336210 CH-UPDATE-WDK6-WDD9 SECTION.                                             
336220     MOVE 'CH-UPDATE-WDK6-WDD9      '  TO CURRENT-SECTION                 
336230                                                                          
336240     MOVE W-IDARTNR                  TO W-IDARTNR-D9                      
336250     MOVE WC-CDC-SE                  TO W-IDDC-D9                         
336260                                                                          
336270     PERFORM IMS-GHU-WDD902                                               
336280     IF SEGMENT-FINNS                                                     
336290        ADD W-OMSPEC-KVBEST-PL       TO 902-LEVNR-KVBR                    
336291        MOVE WS-DAGENS-DATUM         TO 902-LEVNR-TILEVPL                 
336292        PERFORM IMS-REPL-WDD902                                           
336293     END-IF                                                               
336294                                                                          
336295     PERFORM IMS-GHU-WDD904                                               
336296     IF SEGMENT-FINNS                                                     
336297        IF CLAG-KDLPSP NOT = 5                                            
336298        OR W-IDLEVNR   NOT = ART-IDLEVNR                                  
336299           PERFORM IMS-DLET-WDD904                                        
336300        ELSE                                                              
336301           IF W-OMSPEC-KDPLKOEP = 2                                       
336302             MOVE W-OMSPEC-KDPLKOEP  TO 904-OMSPEC-KDPLKOEP               
336303             MOVE W-OMSPEC-KVBEST-PL TO 904-OMSPEC-KVBEST-PL              
336304             PERFORM IMS-REPL-WDD904                                      
336305           END-IF                                                         
336306        END-IF                                                            
336307     END-IF                                                               
336308                                                                          
336309     IF W-OMSPEC-KVBEST-PL > +0                                           
336310        IF CLAG-IDINK (1:3) NUMERIC                                       
336311           MOVE CLAG-IDINK(1:3)   TO W-BEST-IDINK                         
336312        ELSE                                                              
336313           IF CLAG-IDINK (2:3) NUMERIC                                    
336314              MOVE CLAG-IDINK(2:3) TO W-BEST-IDINK                        
336315           ELSE                                                           
336316              MOVE ZERO           TO W-BEST-IDINK                         
336317           END-IF                                                         
336318        END-IF                                                            
336319        MOVE W-IDBESTNR           TO BEST-IDBEST                          
336320        MOVE SPACE                TO BEST-IDLEVNR-BEST                    
336321        MOVE 2                    TO BEST-KDBEH-BEST                      
336322        MOVE W-OMSPEC-KVBEST-PL   TO BEST-KVBEST                          
336323        MOVE W-OMSPEC-KVBEST-PL   TO BEST-KVBEST-BEKR                     
336324        MOVE WS-DAGENS-DATUM      TO BEST-TIBEST                          
336325                                                                          
336326        PERFORM IMS-INSERT-WDK622                                         
336327                                                                          
336328        PERFORM IMS-GU-WDK611                                             
336329        MOVE +0                   TO WS-ANT-WDK622                        
336330        PERFORM IMS-GHNP-WDK622                                           
336331        PERFORM UNTIL SEGMENT-SAKNAS                                      
336332          ADD +1                  TO WS-ANT-WDK622                        
336333          IF WS-ANT-WDK622 > 7                                            
336334             PERFORM IMS-DLET-WDK622                                      
336335          END-IF                                                          
336336          PERFORM IMS-GHNP-WDK622                                         
336337        END-PERFORM                                                       
336338     END-IF                                                               
336339     .                                                                    
336340                                                                          
336341     EJECT                                                                
336350 E-STARTA-OMSPEC-2135-TRANS SECTION.                                      
336400     MOVE 'E-STARTA-OMSPEC-2135-TRANS '  TO CURRENT-SECTION               
336500                                                                          
336600     MOVE IDARTNR-WS           TO  MOD2135-MID-IDARTNR-IN                 
336700     COMPUTE P-TO-P-KVLL       = LNG-P-TO-P-PREFIX                        
336800                               + 23 + 9                                   
336900                                                                          
337000     MOVE LOW-VALUE            TO P-TO-P-KDZ1                             
337100     MOVE LOW-VALUE            TO P-TO-P-KDZ2                             
337200     MOVE 'W2T135X '           TO P-TO-P-KDTRANS                          
337300     MOVE '2135'               TO P-TO-P-IDTRANS                          
337400     MOVE MFS-KDMFSFOR         TO P-TO-P-KDMFSFOR                         
337500                                                                          
337600     MOVE MOD2135-MID-W2I13501 TO P-TO-P-DATA                             
337700     PERFORM IMS-ISRT-ALT1-MSG-2135                                       
337800                                                                          
337900     .                                                                    
338000     EJECT                                                                
338100 S01-GEN-UTSKRIFTSBEGAERAN SECTION.                                       
338110     MOVE 'S01-GEN-UTSKRIFTSBEGAERAN' TO CURRENT-SECTION                  
338200                                                                          
338300     IF  SW-UTSKRIFT-BEGAERD = NEJ                                        
340700       MOVE JA TO SW-UTSKRIFT-BEGAERD                                     
340800                                                                          
340811       MOVE JA TO SW-UPDATE-WDK6-WDD9                                     
340820                                                                          
340900       IF W-MATINFO-KDHF = 0                                              
341000         MOVE SPACE TO DLI-IO-AREA                                        
341100         MOVE '2215' TO W-WDGX-KEY                                        
341200         MOVE W-IDLEVNR TO X-IDLEVNR                                      
341300         PERFORM IMS-GU-2216-SEG                                          
341400         IF SEGMENT-FINNS AND 2216-KDEDI NOT = 'T'                        
341500           MOVE WDGX2216  TO W-WDGX2216                                   
341600           MOVE SPACE TO DLI-IO-AREA                                      
341700           MOVE '2217' TO W-WDGX-KEY                                      
341800           MOVE W-IDARTNR TO 2218-IDARTNR                                 
341900           MOVE W-IDLEVNR TO 2218-IDLEVNR                                 
342000           MOVE W-MATINFO-IDANSK TO 2218-IDANSK                           
342100           IF (W-2216-KDVECKOSL NOT = 'P')  AND                           
342200              (2218-IDLEVNR     NOT = 'BP8HB')                            
342400              PERFORM IMS-INSERT-2218-SEG                                 
342500           END-IF                                                         
342600***  FIX                                                                  
342700           IF W-2216-TISEND-PER NOT NUMERIC                               
342800              MOVE ZERO TO W-2216-TISEND-PER                              
342900           END-IF                                                         
343000***  FIX                                                                  
343100           MOVE W-2216-TISEND-PER   TO TMP1-YYMMDD                        
343200           MOVE W-2216-TISEND-SEN   TO TMP2-YYMMDD                        
343300           PERFORM WY2000P1                                               
343400           IF W-2216-FLLEVPLP = JA OR                                     
343500              W-2216-FLLEVVB  = JA OR                                     
343600             (W-2216-TISEND-PER > ZERO AND                                
343700              TMP1-YYMMDD > TMP2-YYMMDD)                                  
343800              PERFORM IMS-INSERT-2218-PERIOD-SEG                          
343900           END-IF                                                         
344000         END-IF                                                           
344100       END-IF                                                             
344200                                                                          
344300       MOVE W-IDLEVNR     TO OLIKA-LEV                                    
344400       IF LV-LEVNR                                                        
344500******** LEVERANTÖR LV                                                    
344600         MOVE LOW-VALUE TO XXCZ-2246-WDGX2246                             
344700         MOVE W-IDARTNR TO XXCZ-2246-IDARTNR                              
344800         MOVE W-IDLEVNR TO XXCZ-2246-IDLEVNR                              
344900         PERFORM IMS-INSERT-XXCZ-2246                                     
345000       END-IF                                                             
345100     END-IF                                                               
345200     .                                                                    
345300     EJECT                                                                
345419 S02-BEGAERAN-OMSPEC SECTION.                                             
345420     MOVE 'S02-BEGAERAN-OMSPEC        '  TO CURRENT-SECTION               
345500                                                                          
345600     MOVE SPACE TO DLI-IO-AREA                                            
345700     MOVE W-IDARTNR TO 2204-IDARTNR                                       
345800     IF  W-KDOMSPEC = 1                                                   
345900       MOVE ORSAK-BEGAERD TO 2204-KDLPORS                                 
346000     ELSE                                                                 
346100       MOVE ORSAK-BEG-OPT TO 2204-KDLPORS                                 
346200     END-IF                                                               
346300                                                                          
346400     PERFORM IMS-INSERT-2204-SEG                                          
346500     .                                                                    
346600     EJECT                                                                
346700 S03-NOLLSTALL-OMSPEC-SEG SECTION.                                        
346710     MOVE 'S03-NOLLSTALL-OMSPEC-SEG   '  TO CURRENT-SECTION               
346800                                                                          
346900     MOVE ZERO TO OMSPEC-DASPECST                                         
347000                  OMSPEC-KDLPORS-TAB (1)                                  
347100                  OMSPEC-KDLPORS-TAB (2)                                  
347200                  OMSPEC-KDLPORS-TAB (3)                                  
347300                  OMSPEC-KVBEST-PL                                        
347400                  OMSPEC-KDPLKOEP                                         
347500     .                                                                    
347600     EJECT                                                                
347700 S04-SATSART-AENDRAD SECTION.                                             
347710     MOVE 'S04-SATSART-AENDRAD        '  TO CURRENT-SECTION               
347800                                                                          
347900     MOVE SPACE TO DLI-IO-AREA                                            
348000     MOVE '2201' TO W-WDGX-KEY                                            
348100     MOVE W-IDARTNR TO 2201-IDARTNR-SATS                                  
348200     MOVE JA TO 2201-FLAGGA-LPKNTL-ING                                    
348300                                                                          
348400     PERFORM IMS-INSERT-HAENDEL-SEG                                       
348500     .                                                                    
348600     EJECT                                                                
348700 S05-ADD-TILL-ORSAKOMSPEC SECTION.                                        
348710     MOVE 'S05-ADD-TILL-ORSAKOMSPEC   '  TO CURRENT-SECTION               
348800                                                                          
348900     MOVE OMSPEC-KDLPORS-TAB(01) TO W-KDLPORS-TAB(01)                     
349000     MOVE OMSPEC-KDLPORS-TAB(02) TO W-KDLPORS-TAB(02)                     
349100     MOVE OMSPEC-KDLPORS-TAB(03) TO W-KDLPORS-TAB(03)                     
349200     MOVE W-ORSAKS-KOD           TO W-KDLPORS-TAB(04)                     
349300                                                                          
349400     CALL W221LPAD USING W-W221LP-CTX W-KDLPORS-GRP                       
349500                                                                          
349600     MOVE W-KDLPORS-TAB(01)    TO OMSPEC-KDLPORS-TAB(01)                  
349700     MOVE W-KDLPORS-TAB(02)    TO OMSPEC-KDLPORS-TAB(02)                  
349800     MOVE W-KDLPORS-TAB(03)    TO OMSPEC-KDLPORS-TAB(03)                  
349900     .                                                                    
350000     EJECT                                                                
350100 S06-SUBTRACT-FRAN-ORSAKOMSPEC SECTION.                                   
350110     MOVE 'S06-SUBTRACT-FRAN-ORSAKOMSPEC' TO CURRENT-SECTION              
350200                                                                          
350300     MOVE 1 TO IX                                                         
350400     PERFORM UNTIL IX > 3                                                 
350500       IF  W-ORSAKS-KOD = OMSPEC-KDLPORS-TAB (IX)                         
350600         MOVE IX TO IX-PLUS-1                                             
350700         ADD 1 TO IX-PLUS-1                                               
350800         PERFORM UNTIL IX NOT < 3                                         
350900           MOVE OMSPEC-KDLPORS-TAB (IX-PLUS-1)                            
351000                           TO OMSPEC-KDLPORS-TAB (IX)                     
351100           ADD 1 TO IX                                                    
351200                    IX-PLUS-1                                             
351300         END-PERFORM                                                      
351400         MOVE 4 TO IX                                                     
351500         MOVE ZERO TO OMSPEC-KDLPORS-TAB (3)                              
351600       END-IF                                                             
351700       ADD 1 TO IX                                                        
351800     END-PERFORM                                                          
351900     .                                                                    
352000     EJECT                                                                
352100 S07-KONTROLL-DATUM SECTION.                                              
352110     MOVE 'S07-KONTROLL-DATUM           ' TO CURRENT-SECTION              
352120                                                                          
352300*    ÅÅVV > 7001   OCH VV= 01-52, ELLER 99                       *        
352400                                                                          
352500     IF W-DAAVROP-AAVV NOT = 9999                                         
352600       MOVE W-DAAVROP-AAVV TO W-DATUM-AAVV                                
352700       IF W-DATUM-AA = 20                                                 
352800         IF (W-DAAVROP-AAVV < 7001 AND W-DAAVROP-AAVV > 5000)             
352900         OR W-DATUM-VV < 01 OR W-DATUM-VV > 53                            
353000           MOVE FEL TO FAELTTAB-FAELT (IX-PLUS-1)                         
353100         END-IF                                                           
353200       ELSE                                                               
353300         IF (W-DAAVROP-AAVV < 7001 AND W-DAAVROP-AAVV > 5000)             
353400         OR W-DATUM-VV < 01 OR W-DATUM-VV > 52                            
353500           MOVE FEL TO FAELTTAB-FAELT (IX-PLUS-1)                         
353600         END-IF                                                           
353700       END-IF                                                             
353800     END-IF                                                               
353900     .                                                                    
354000     EJECT                                                                
354100 S08-NYA-AVROP-TILL-TAB SECTION.                                          
354110     MOVE 'S08-NYA-AVROP-TILL-TAB       ' TO CURRENT-SECTION              
354200                                                                          
354300     MOVE AVROP-DAAVROP-AVS  TO W-DAAVROP-AVS                             
354400     MOVE W-DAAVROP-AVS-AAVV TO W-DATUM-AAVV                              
354500     PERFORM S09-BERAKNA-INDEX-I-AVROPSTAB                                
354600                                                                          
354700     IF IY-PERIOD > ZERO AND                                              
354800        IY-PERIOD NOT > MAX-ANT-PERIODER-I-TAB                            
354900       ADD  AVROP-KVAVROP TO W-KVAVROP-TAB (IY-PERIOD, IX-VECKA)          
355000       MOVE AVROP-DAAVROP-AVS  TO W-DAAVROP-AVS                           
355100       MOVE W-DAAVROP-AVS-AAVV TO W-DATUM-AAVV                            
355200       MOVE W-DATUM-VV TO W-TIAVROP-AVS-TAB (IY-PERIOD, IX-VECKA)         
355300     END-IF                                                               
355400     .                                                                    
355500     EJECT                                                                
355600 S09-BERAKNA-INDEX-I-AVROPSTAB SECTION.                                   
355610     MOVE 'S09-BERAKNA-INDEX-I-AVROPSTAB' TO CURRENT-SECTION              
355700                                                                          
355800*    BERÄKNING AV INDEX I TABELL FÖR PERIOD/VECKA (RADER/KOLUMN) *        
355900*                                                                *        
356000*    MED UTGÅNGSPUNKT FRÅN START-PERIOD (ÅÅPP) BERÄKNAS INDEX    *        
356100*    IY-PERIOD OCH IX-VECKA FÖR GIVEN VECKA (ÅÅVV)               *        
356200*    FÖRUTSÄTTNINGAR:                                            *        
356300*        - GIVEN VECKA INTE < AKTUELL VECKA                      *        
356400*        - IY-PERIOD KAN VARA > MAX-ANT-PERIODER-I-TAB           *        
356500*        - FÄLTEN W-START-PERIOD, W-START-PER-AA, W-START-PER-PP *        
356600*          SKALL VARA INITIERADE                                 *        
356700                                                                          
356800     IF  W-DATUM-AAVV = ZERO                                              
356900       MOVE W-DATUM-AKTUELLT TO W-DATUM-AAVV                              
357000     END-IF                                                               
357100                                                                          
357200     IF  W-DATUM-AA NOT = W-START-PER-AA                                  
357300       MOVE 12 TO IY                                                      
357400     ELSE                                                                 
357500       MOVE ZERO TO IY                                                    
357600     END-IF                                                               
357700                                                                          
357800     MOVE ZERO TO IX                                                      
357900     MOVE IY TO IX-TOT                                                    
358000     PERFORM UNTIL IX NOT < MAX-ANT-PERIODER                              
358100       ADD 1 TO IX                                                        
358200                IX-TOT                                                    
358300       IF  W-DATUM-VV NOT < PER-START-VV (IX-TOT)                         
358400       AND W-DATUM-VV NOT > PER-SLUT-VV  (IX-TOT)                         
358500         MOVE IX TO W-PER-PP                                              
358600       END-IF                                                             
358700     END-PERFORM                                                          
358800     MOVE W-PER-PP TO IX-TOT                                              
358900     ADD IY TO IX-TOT                                                     
359000                                                                          
359100     MOVE W-DATUM-AA  TO W-BER-PERIOD                                     
359200     MULTIPLY 100 BY W-BER-PERIOD                                         
359300     ADD W-PER-PP TO W-BER-PERIOD                                         
359400                                                                          
359500     MOVE ZERO       TO IY-PERIOD                                         
359600     MOVE W-DATUM-VV TO IX-VECKA                                          
359700     SUBTRACT PER-START-VV (IX-TOT) FROM IX-VECKA                         
359800     ADD 1 TO IX-VECKA                                                    
359900     IF  IX-VECKA > MAX-ANT-VECKOR-I-TAB                                  
360000         MOVE MAX-ANT-VECKOR-I-TAB TO IX-VECKA                            
360100     END-IF                                                               
360200                                                                          
360300     MOVE W-BER-PERIOD   TO W-PERIOD-AAPP                                 
360400                                                                          
360500     MOVE W-PERIOD-AA    TO TMP1-YY                                       
360600     MOVE W-START-PER-AA TO TMP2-YY                                       
360700     PERFORM WY2000P9                                                     
360800     COMPUTE IY-PERIOD = IY-PERIOD +                                      
360900             (TMP1-YY - TMP2-YY) * 12 +                                   
361000             W-PERIOD-PP - W-START-PER-PP + 1                             
361100                                                                          
361200     MOVE W-START-PER-AA TO W-PERIOD-AA                                   
361300     MOVE 5              TO W-PERIOD-PP                                   
361400*?   IF  W-START-PER-P > 5                                                
361500*?     ADD 1 TO W-PERIOD-AA                                               
361600*?   END-IF                                                               
361700*    MOVE W-START-PER-AAPP  TO TMP1-YYRP                                  
361800*    MOVE W-PERIOD-AAPP     TO TMP2-YYRP                                  
361900*    MOVE W-BER-PERIOD      TO TMP3-YYRP                                  
362000*    PERFORM WY2000Q7                                                     
362100*    IF (TMP1-YYRP < TMP2-YYRP AND                                        
362200*        TMP3-YYRP > TMP2-YYRP)                                           
362300*      ADD 1 TO IY-PERIOD                                                 
362400*    END-IF                                                               
362500     .                                                                    
362600     EJECT                                                                
362700 S10-ORSAKSTEXTER-TILL-MOD SECTION.                                       
362710     MOVE 'S10-ORSAKSTEXTER-TILL-MOD    ' TO CURRENT-SECTION              
362800                                                                          
362900*    ORSAKSTEXTER FRÅN TABELL W221W005 FLYTTAS MED HJÄLP         *        
363000*    AV TABELLEN FÖR ORSAKSKODER I WS                            *        
363100                                                                          
363200     IF W-KDBEHX-PLAN = FORSLAG                                           
363300     OR (W-KDBEHX-PLAN = GALLANDE AND                                     
363400         W-MATINFO-KDLPSP NOT = 5)                                        
363500                                                                          
363600       IF  W-ORSAK-TAB-KOD (1) > ZERO                                     
363700       AND W-ORSAK-TAB-KOD (1) NOT > MAX-ANT-ORSAKSKODER                  
363800         MOVE W-ORSAK-TAB-KOD (1) TO IX                                   
363900         MOVE TELPORS (IX) TO MOD-TEXT-ORSAK1                             
364000         IF W-ORSAK-AENDRAD (1) = JA                                      
364100           MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-TEXT-ORSAK1-ATTR             
364200         END-IF                                                           
364300       ELSE                                                               
364400         MOVE MFS-RENSA-FAELT TO MOD-TEXT-ORSAK1                          
364500       END-IF                                                             
364600       IF  W-ORSAK-TAB-KOD (2) > ZERO                                     
364700       AND W-ORSAK-TAB-KOD (2) NOT > MAX-ANT-ORSAKSKODER                  
364800         MOVE W-ORSAK-TAB-KOD (2) TO IX                                   
364900         MOVE TELPORS (IX) TO MOD-TEXT-ORSAK2                             
365000         IF W-ORSAK-AENDRAD (2) = JA                                      
365100           MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-TEXT-ORSAK2-ATTR             
365200         END-IF                                                           
365300       ELSE                                                               
365400         MOVE MFS-RENSA-FAELT TO MOD-TEXT-ORSAK2                          
365500       END-IF                                                             
365600     END-IF                                                               
365700     .                                                                    
365800     EJECT                                                                
365900 S11-GAM-AVROP-I-TAB-TILL-MOD SECTION.                                    
365910     MOVE 'S11-GAM-AVROP-I-TAB-TILL-MOD ' TO CURRENT-SECTION              
366000                                                                          
366100*    I BILDEN   ÅTSKILJS VÄRDE OCH VECKA MED  -  FÖR DE          *        
366200*    AVROP SOM RYMMS INOM  KÖP + BESTÄLLNINGSREST.               *        
366300*    FÖR ÖVRIGA AVROP ANVÄNDES  *                                *        
366400                                                                          
366500     MOVE W-LEVNR-KVBR TO W-KVBR                                          
366600     ADD W-OMSPEC-KVBEST-PL TO W-KVBR                                     
366700                                                                          
366800     MOVE STRECK TO W-SKILJETECKEN                                        
366900     MOVE 1 TO IX-GAM                                                     
367000     MOVE ZERO TO W-KVAVROP-ACC                                           
367100                                                                          
367200     PERFORM UNTIL IX-GAM > MAX-ANT-GAMLA-AVROP                           
367300       IF W-KVAVROP-GAM (IX-GAM) > ZERO                                   
367400         ADD W-KVAVROP-GAM (IX-GAM) TO W-KVAVROP-ACC                      
367500         MOVE W-KVAVROP-GAM (IX-GAM) TO MOD-KVAVROP-GAM (IX-GAM)          
367600         IF W-TIAVROP-AVS-GAM (IX-GAM) > ZERO                             
367700            MOVE W-TIAVROP-AVS-GAM (IX-GAM)                               
367800                                 TO MOD-TIAVROP-AVS-GAM (IX-GAM)          
367900         ELSE                                                             
368000            MOVE SPACE           TO MOD-TIAVROP-AVS-GAM (IX-GAM)          
368100         END-IF                                                           
368200         IF W-KVAVROP-ACC > W-KVBR                                        
368300           MOVE ASTERISK TO W-SKILJETECKEN                                
368400         END-IF                                                           
368500         IF W-AVROP-GAM-AENDRAT (IX-GAM) = JA                             
368600           MOVE MFS-ADD-LYS-UPP-FAELT                                     
368700                                  TO MOD-AVROP-GAM-ATTR (IX-GAM)          
368800         END-IF                                                           
368900         MOVE W-SKILJETECKEN TO MOD-SKILJETECKEN-GAM (IX-GAM)             
369000       ELSE                                                               
369100         MOVE MFS-RENSA-FAELT TO MOD-AVROP-GAM (IX-GAM)                   
369200       END-IF                                                             
369300       ADD 1 TO IX-GAM                                                    
369400     END-PERFORM                                                          
369500     .                                                                    
369600     EJECT                                                                
369700 S12-GAMMALT-AVROP-TILL-TAB SECTION.                                      
369710     MOVE 'S12-GAMMALT-AVROP-TILL-TAB   ' TO CURRENT-SECTION              
369800                                                                          
369900*    GAMLA AVROP LÄGGS I TABELL I WS. TABELLEN MOTSVARAR         *        
370000*    RADEN MED GAMLA AVROP I BILDEN                              *        
370100                                                                          
370200     MOVE +1                 TO IX-KOLL                                   
370300     MOVE AVROP-DAAVROP-AVS  TO W-DAAVROP-AVS                             
370400     PERFORM UNTIL IX-KOLL > 5                                            
370500        IF W-TIAVROP-AVS-GAM (IX-KOLL) = W-DAAVROP-AVS-AAVV               
370600           ADD AVROP-KVAVROP TO W-KVAVROP-GAM (IX-KOLL)                   
370700           MOVE +10          TO IX-KOLL                                   
370800        END-IF                                                            
370900        ADD +1               TO IX-KOLL                                   
371000     END-PERFORM                                                          
371100     IF IX-KOLL > 10                                                      
371200        CONTINUE                                                          
371300     ELSE                                                                 
371400        ADD 1 TO IX-GAM                                                   
371500        IF IX-GAM > 5                                                     
371600          ADD W-KVAVROP-GAM (2) TO W-KVAVROP-GAM (1)                      
371700          MOVE 9999 TO W-TIAVROP-AVS-GAM (1)                              
371800          MOVE 2 TO IX-GAM                                                
371900          MOVE 3 TO IX-PLUS-1                                             
372000          PERFORM UNTIL IX-GAM NOT < MAX-ANT-GAMLA-AVROP                  
372100            MOVE W-KVAVROP-GAM (IX-PLUS-1)                                
372200                                TO W-KVAVROP-GAM (IX-GAM)                 
372300            MOVE W-TIAVROP-AVS-GAM (IX-PLUS-1)                            
372400                                TO W-TIAVROP-AVS-GAM (IX-GAM)             
372500            ADD 1 TO IX-GAM                                               
372600                     IX-PLUS-1                                            
372700          END-PERFORM                                                     
372800          MOVE 5 TO IX-GAM                                                
372900        END-IF                                                            
373000        MOVE AVROP-KVAVROP      TO  W-KVAVROP-GAM (IX-GAM)                
373100        MOVE AVROP-DAAVROP-AVS  TO  W-DAAVROP-AVS                         
373200        MOVE W-DAAVROP-AVS-AAVV TO  W-TIAVROP-AVS-GAM (IX-GAM)            
373300     END-IF                                                               
373400     .                                                                    
373500     EJECT                                                                
373600 S13-NYA-AVROP-I-TAB-TILL-MOD SECTION.                                    
373610     MOVE 'S13-NYA-AVROP-I-TAB-TILL-MOD ' TO CURRENT-SECTION              
373700                                                                          
373800*    I BILDEN   ÅTSKILJS VÄRDE OCH VECKA MED  -  FÖR DE          *        
373900*    AVROP SOM RYMMS INOM  KÖP + BESTÄLLNINGSREST.               *        
374000*    FÖR ÖVRIGA AVROP ANVÄNDES *.                                *        
374100                                                                          
374200     MOVE W-LEVNR-KVBR TO W-KVBR                                          
374300     ADD W-OMSPEC-KVBEST-PL TO W-KVBR                                     
374400                                                                          
374500     MOVE 1 TO IY                                                         
374600     PERFORM UNTIL IY > MAX-ANT-PERIODER-I-TAB                            
374700       MOVE 1 TO IX                                                       
374800       PERFORM UNTIL IX > MAX-ANT-VECKOR-I-TAB                            
374900         IF W-KVAVROP-TAB (IY, IX) > ZERO                                 
375000           MOVE W-KVAVROP-TAB(IY, IX) TO MOD-KVAVROP-TAB(IY, IX)          
375100           MOVE W-TIAVROP-AVS-TAB (IY, IX)                                
375200                                  TO MOD-TIAVROP-AVS-TAB(IY, IX)          
375300                                                                          
375400           IF W-SKILJETECKEN = STRECK                                     
375500             ADD W-KVAVROP-TAB (IY, IX) TO W-KVAVROP-ACC                  
375600             IF  W-KVAVROP-ACC > W-KVBR                                   
375700               MOVE ASTERISK TO W-SKILJETECKEN                            
375800             END-IF                                                       
375900           END-IF                                                         
376000           MOVE W-SKILJETECKEN TO MOD-SKILJETECKEN-TAB (IY, IX)           
376100           IF W-AVROP-TAB-AENDRAT (IY, IX) = JA                           
376200             MOVE MFS-ADD-LYS-UPP-FAELT                                   
376300                                 TO MOD-AVROP-TAB-ATTR (IY, IX)           
376400           END-IF                                                         
376500         ELSE                                                             
376600           MOVE LOW-VALUE TO MOD-AVROP-TAB (IY, IX)                       
376700         END-IF                                                           
376800                                                                          
376900**********    W-PERIOD-TAB-RAD (IY) ÄR PERIODEN PÅ RAD IY                 
377000**********    PER-ANT-VV (PIX) ÄR ANTAL VECKOR I PERIODEN PIX             
377100**********    IX ÄR INDEXET FÖR VECKAN INOM PERIODEN                      
377200**********                             DVS KOLUMNEN I RADEN               
377300**********    ) SÄTTS BARA FÖR EXISTERANDE VECKOR INOM PERIODEN           
377400*********MOVE W-PERIOD-TAB-RAD (IY) TO PIX                                
377500         MOVE MOD-PERIOD-AA (IY)    TO W-PERIOD-AA                        
377600         MOVE MOD-PERIOD-PP (IY)    TO W-PERIOD-PP                        
377700         MOVE +1                    TO IX-AAPP PIX                        
377800         PERFORM UNTIL IX-AAPP > 24                                       
377900            IF W-PERIOD-AAPP = PER-AAPP (IX-AAPP)                         
378000               MOVE IX-AAPP         TO PIX                                
378100               MOVE 25              TO IX-AAPP                            
378200            END-IF                                                        
378300            ADD +1                  TO IX-AAPP                            
378400         END-PERFORM                                                      
378500*********                                                                 
378600         IF PER-ANT-VV (PIX) < IX                                         
378700            MOVE SPACE    TO MOD-PARENTES-TAB (IY, IX)                    
378800         ELSE                                                             
378900            MOVE PARENTES TO MOD-PARENTES-TAB (IY, IX)                    
379000         END-IF                                                           
379100                                                                          
379200         ADD 1 TO IX                                                      
379300       END-PERFORM                                                        
379400       ADD 1 TO IY                                                        
379500     END-PERFORM                                                          
379600     .                                                                    
379700     EJECT                                                                
379800 S14-NOLLSTAELL-AVROPSTABELLER SECTION.                                   
379810     MOVE 'S14-NOLLSTAELL-AVROPSTABELLER' TO CURRENT-SECTION              
379900                                                                          
380000     MOVE 1 TO IX                                                         
380100     PERFORM UNTIL IX > MAX-ANT-GAMLA-AVROP                               
380200       MOVE ZERO TO W-KVAVROP-GAM (IX)                                    
380300                    W-TIAVROP-AVS-GAM (IX)                                
380400       MOVE NEJ TO W-AVROP-GAM-AENDRAT (IX)                               
380500       ADD 1 TO IX                                                        
380600     END-PERFORM                                                          
380700                                                                          
380800     MOVE 1 TO IY                                                         
380900     PERFORM UNTIL IY > MAX-ANT-PERIODER-I-TAB                            
381000       MOVE 1 TO IX                                                       
381100       PERFORM UNTIL IX > MAX-ANT-VECKOR-I-TAB                            
381200         MOVE ZERO TO W-KVAVROP-TAB (IY, IX)                              
381300                      W-TIAVROP-AVS-TAB (IY, IX)                          
381400         MOVE NEJ TO W-AVROP-TAB-AENDRAT (IY, IX)                         
381500         ADD 1 TO IX                                                      
381600       END-PERFORM                                                        
381700       ADD 1 TO IY                                                        
381800     END-PERFORM                                                          
381900     .                                                                    
382000     EJECT                                                                
382100 S16-RADERA-I-AVROPSTAB SECTION.                                          
382110     MOVE 'S16-RADERA-I-AVROPSTAB       ' TO CURRENT-SECTION              
382200                                                                          
382300*    AVROPSTABELL RADERAS FR.O.M. DET DATUM SOM ANGES            *        
382400*    I W-DATUM-AAVV                                              *        
382500                                                                          
382600     PERFORM S09-BERAKNA-INDEX-I-AVROPSTAB                                
382700     PERFORM UNTIL IY-PERIOD > MAX-ANT-PERIODER-I-TAB                     
382800       PERFORM UNTIL IX-VECKA > MAX-ANT-VECKOR-I-TAB                      
382900         MOVE ZERO TO W-KVAVROP-TAB (IY-PERIOD, IX-VECKA)                 
383000                      W-TIAVROP-AVS-TAB (IY-PERIOD, IX-VECKA)             
383100         MOVE NEJ TO W-AVROP-TAB-AENDRAT (IY-PERIOD, IX-VECKA)            
383200         ADD 1 TO IX-VECKA                                                
383300       END-PERFORM                                                        
383400       ADD 1 TO IY-PERIOD                                                 
383500       MOVE 1 TO IX-VECKA                                                 
383600     END-PERFORM                                                          
383700     .                                                                    
383800     EJECT                                                                
383900 S18-TILLAGG-TILL-AVROPSTAB SECTION.                                      
383910     MOVE 'S18-TILLAGG-TILL-AVROPSTAB   ' TO CURRENT-SECTION              
384000                                                                          
384100     MOVE W-DAAVROP-AAVV TO W-DATUM-AAVV                                  
384200     PERFORM S09-BERAKNA-INDEX-I-AVROPSTAB                                
384300     IF IY-PERIOD > ZERO AND                                              
384400        IY-PERIOD NOT > MAX-ANT-PERIODER-I-TAB                            
384500       MOVE W-KVAVROP TO W-KVAVROP-TAB (IY-PERIOD, IX-VECKA)              
384600       MOVE W-DAAVROP-AAVV TO W-DATUM-AAVV                                
384700       MOVE W-DATUM-VV TO W-TIAVROP-AVS-TAB (IY-PERIOD, IX-VECKA)         
384800       MOVE JA TO W-AVROP-TAB-AENDRAT (IY-PERIOD, IX-VECKA)               
384900     END-IF                                                               
385000     EJECT                                                                
385100     .                                                                    
385200                                                                          
385300 S19-UPPDATERA-WDR5 SECTION.                                              
385310     MOVE 'S19-UPPDATERA-WDR5           ' TO CURRENT-SECTION              
385400                                                                          
385500     MOVE W-IDARTNR    TO  2228-IDARTNR                                   
385600     MOVE LOW-VALUE    TO  2228-LOW-VALUE                                 
385700     MOVE SPACE        TO  2228-FILLER                                    
385800     PERFORM IMS-ISRT-2228                                                
385900     .                                                                    
386000     EJECT                                                                
386100                                                                          
386200 S20-KOLL-LEV-HELGDAG SECTION.                                            
386300     MOVE 'S20-KOLL-LEV-HELGDAG '  TO CURRENT-SECTION                     
386400                                                                          
386500     MOVE SPACE              TO WS-IDLANDX2                               
386600     PERFORM IMS-GU-LEVA14-WDF106-SHIP                                    
386700     IF SEGMENT-FINNS                                                     
386800        MOVE ADR-IDLANDX2    TO WS-IDLANDX2                               
386900     END-IF                                                               
387000     MOVE WS-IDLANDX2       TO W-IDLANDX2                                 
387100     MOVE 20                TO W-DADATUM-HELG-SS                          
387200**** MOVE W-TIAAMMDD-AVS    TO W-DADATUM-HELG-AAMMDD  SE OVAN             
387210                                                                          
387300     PERFORM IMS-GU-WDF301                                                
387400     IF SEGMENT-FINNS                                                     
387500        IF ENGLISH-TEXT                                                   
387600           MOVE MED-13 TO MOD-MESSAGE-BOTTOM                              
387700        ELSE                                                              
387800           MOVE MED-3  TO MOD-MESSAGE-BOTTOM                              
387900        END-IF                                                            
388000*       MOVE MFS-ADD-LYS-UPP-FAELT                                        
388100*            TO MOD-TIAVROP-AVS-IN-ATTR (IX-AVROP)                        
388200     END-IF                                                               
388300     .                                                                    
388400     EJECT                                                                
388500 S21-KOLL-LEV-BLOCKAD SECTION.                                            
388600     MOVE 'S21-KOLL-LEV-BLOCKAD '  TO CURRENT-SECTION                     
388700*-------------------------------------------------------------            
388800*--- KOLLA OM AVROPET HAMNAR UNDER EN BLOCKAD PERIOD FÖR SHIP-            
388900*--- LEVERANTÖREN. SE TABELL BILD 2149.                                   
389000*-------------------------------------------------------------            
389100                                                                          
389200     IF SW-HUVUDLEVERANTOER = JA                                          
389300       PERFORM IMS-GU-WDGX2258                                            
389400       IF SEGMENT-FINNS                                                   
389500         MOVE AVROP-DAAVROP-AVS   TO W-DAAVROP-2260                       
389600         PERFORM IMS-GNP-WDGX2260                                         
389700         IF SEGMENT-FINNS                                                 
389800           MOVE INF-SUPPL-BLOCKED TO MED-IDMFSINF                         
389900           CALL WMEDKONV USING MED-WMEDAREA                               
390000           IF MOD-MESSAGE = SPACE                                         
390100             MOVE MED-MFSINF   TO MOD-MESSAGE                             
390200           ELSE                                                           
390300             MOVE MED-MFSINF   TO MOD-MESSAGE-BOTTOM                      
390400           END-IF                                                         
390500           MOVE MFS-ADD-LYS-UPP-FAELT                                     
390600                        TO MOD-TIAVROP-AVS-IN-ATTR (IX-AVROP)             
390700         END-IF                                                           
390800       END-IF                                                             
390900     END-IF                                                               
391000     .                                                                    
391100     EJECT                                                                
391200                                                                          
391300 S22-BERAKNA-VV-PLUS-LT SECTION.                                          
391310     MOVE 'S22-BERAKNA-VV-PLUS-LT       ' TO CURRENT-SECTION              
391400                                                                          
391500     MOVE W-DATUM-AKTUELLT TO W-DATUM-AAVV                                
391600     MOVE W-DATUM-VV TO W-ANTAL-VECKOR                                    
391700     ADD  W-MATINFO-KVVECKOR-LT TO W-ANTAL-VECKOR                         
391800                                                                          
391900     IF W-DATUM-AA = 20 AND W-ANTAL-VECKOR > 53                           
392000       SUBTRACT 53 FROM W-ANTAL-VECKOR                                    
392100       ADD 1 TO W-DATUM-AA                                                
392200     END-IF                                                               
392300     IF W-DATUM-AA = 20 AND W-ANTAL-VECKOR = 53                           
392400       CONTINUE                                                           
392500     ELSE                                                                 
392600       PERFORM UNTIL W-ANTAL-VECKOR <= 52                                 
392700         SUBTRACT 52 FROM W-ANTAL-VECKOR                                  
392800         ADD 1 TO W-DATUM-AA                                              
392900       END-PERFORM                                                        
393000     END-IF                                                               
393100     MOVE W-ANTAL-VECKOR TO W-DATUM-VV                                    
393200     MOVE W-DATUM-AAVV TO W-DAGENS-DAT-PLUS-LT                            
393300     .                                                                    
393400                                                                          
393500 S23-LYS-UPP-AVROP-I-TAB SECTION.                                         
393510     MOVE 'S23-LYS-UPP-AVROP-I-TAB      ' TO CURRENT-SECTION              
393600                                                                          
393700     PERFORM S09-BERAKNA-INDEX-I-AVROPSTAB                                
393800     PERFORM UNTIL IY-PERIOD > MAX-ANT-PERIODER-I-TAB                     
393900       PERFORM UNTIL IX-VECKA > MAX-ANT-VECKOR-I-TAB                      
394000         IF W-KVAVROP-TAB (IY-PERIOD, IX-VECKA) > 0                       
394100           MOVE JA TO W-AVROP-TAB-AENDRAT (IY-PERIOD, IX-VECKA)           
394200         END-IF                                                           
394300         ADD +1 TO IX-VECKA                                               
394400       END-PERFORM                                                        
394500       ADD +1 TO IY-PERIOD                                                
394600       MOVE +1 TO IX-VECKA                                                
394700     END-PERFORM                                                          
394800     .                                                                    
394900     EJECT                                                                
395000 S25-SEKELJUSTERA        SECTION.                                         
395010     MOVE 'S25-SEKELJUSTERA             ' TO CURRENT-SECTION              
395100                                                                          
395200*    NÄR NYCKEL DAAVROP SÄTTS ENDAST M.H.A. AAVV                          
395300*    SÅ LÄGGS SEKEL TILL HÄR,SÅ KORREKT AAAAVV BILDAS                     
395400                                                                          
395500     IF W-DAAVROP-AAVV = 9999                                             
395600        MOVE 99    TO W-DAAVROP-SS                                        
395700     ELSE                                                                 
395800        IF W-DAAVROP-AAVV > 5000                                          
395900           MOVE 19 TO W-DAAVROP-SS                                        
396000        ELSE                                                              
396100           MOVE 20 TO W-DAAVROP-SS                                        
396200        END-IF                                                            
396300     END-IF                                                               
396400     .                                                                    
396500     EJECT                                                                
396600 MFS-ROER-EJ-INDATA-FAELT SECTION.                                        
396700                                                                          
396800     MOVE 1 TO IX                                                         
396900     PERFORM UNTIL IX NOT < MAX-ANT-INDATA-FLT                            
397000       MOVE MFS-FORMATETS-ATTR TO MOD-ATTR (IX)                           
397100       MOVE MFS-RENSA-FAELT    TO MOD-FAELT (IX)                          
397200       ADD 1 TO IX                                                        
397300     END-PERFORM                                                          
397400     EJECT                                                                
397500     .                                                                    
397600 MFS-OEPPNA-INIT-UPPDAT-FAELT SECTION.                                    
397700                                                                          
397800     MOVE 1 TO IX                                                         
397900     PERFORM UNTIL IX NOT < MAX-ANT-INDATA-FLT                            
398000       MOVE MFS-OEPPNA-NUM-FAELT TO MOD-ATTR (IX)                         
398100       MOVE MFS-RENSA-FAELT      TO MOD-FAELT (IX)                        
398200       ADD 1 TO IX                                                        
398300     END-PERFORM                                                          
398400                                                                          
398500     IF ENGLISH-TEXT                                                      
398600       MOVE YYWW TO  MOD-TIAVROP-AVS-IN (1)                               
398700                     MOD-TIAVROP-AVS-IN (2)                               
398800                     MOD-TIAVROP-AVS-IN (3)                               
398900                     MOD-TIAVROP-AVS-IN (4)                               
399000                     MOD-TILPSP-IN                                        
399100     ELSE                                                                 
399200       MOVE AAVV TO  MOD-TIAVROP-AVS-IN (1)                               
399300                     MOD-TIAVROP-AVS-IN (2)                               
399400                     MOD-TIAVROP-AVS-IN (3)                               
399500                     MOD-TIAVROP-AVS-IN (4)                               
399600                     MOD-TILPSP-IN                                        
399700     END-IF                                                               
399800                                                                          
399900*    LÅT LIGGA KVAR VID UPPDATERING - KDLEVPLF OCH JIT                    
400200     MOVE MFS-ALFA-FAELT-RAETT  TO MOD-ATTR (13)                          
400300     MOVE MFS-ROER-EJ-FAELT     TO MOD-FAELT (13)                         
400310     MOVE MFS-ALFA-FAELT-RAETT  TO MOD-ATTR (14)                          
400320     MOVE MFS-ROER-EJ-FAELT     TO MOD-FAELT (14)                         
400800     MOVE MFS-ALFA-FAELT-RAETT  TO MOD-TEARTNOT1-IN-ATTR                  
400900     MOVE MFS-ROER-EJ-FAELT     TO MOD-TEARTNOT1-IN                       
401000     MOVE MFS-ALFA-FAELT-RAETT  TO MOD-TEARTNOT2-IN-ATTR                  
401100     MOVE MFS-ROER-EJ-FAELT     TO MOD-TEARTNOT2-IN                       
401200     .                                                                    
401300     EJECT                                                                
401400 MFS-SET-ATTR-FAELT-FEL SECTION.                                          
401500                                                                          
401600     MOVE +1 TO FAELTTAB-IX                                               
401700     PERFORM UNTIL FAELTTAB-IX > FAELTTAB-IX-MAX                          
401800       IF FAELTTAB-FAELT (FAELTTAB-IX) = RAETT OR EJ-IFYLLD               
401900         IF FAELTTAB-KDDATTYP (FAELTTAB-IX) = NUM                         
402000           MOVE MFS-NUM-FAELT-RAETT TO MOD-ATTR (FAELTTAB-IX)             
402100         ELSE                                                             
402200           MOVE MFS-ALFA-FAELT-RAETT TO MOD-ATTR (FAELTTAB-IX)            
402300         END-IF                                                           
402400       ELSE                                                               
402500         IF FAELTTAB-FAELT (FAELTTAB-IX) = FEL                            
402600           IF FAELTTAB-KDDATTYP (FAELTTAB-IX) = NUM                       
402700             MOVE MFS-NUM-FAELT-FEL TO MOD-ATTR (FAELTTAB-IX)             
402800           ELSE                                                           
402900             MOVE MFS-ALFA-FAELT-FEL TO MOD-ATTR (FAELTTAB-IX)            
403000           END-IF                                                         
403100         END-IF                                                           
403200       END-IF                                                             
403300       MOVE MFS-ROER-EJ-FAELT TO MOD-FAELT (FAELTTAB-IX)                  
403400       ADD +1 TO FAELTTAB-IX                                              
403500     END-PERFORM                                                          
403600     .                                                                    
403700     EJECT                                                                
403800 MFS-ROER-EJ-UTDATA-FAELT SECTION.                                        
403900                                                                          
404000     MOVE MFS-FORMATETS-ATTR TO MOD-TEXT-PLANTYP-ATTR                     
404100                                MOD-TEXT-ORSAK1-ATTR                      
404200                                MOD-TEXT-ORSAK2-ATTR                      
404300                                MOD-KDLPSP-ATTR                           
404400                                MOD-TILPSP-ATTR                           
404500                                MOD-DATA-GRP2-ATTR                        
404600                                MOD-TEXT-UPPLYSNING-ATTR (1)              
404700                                MOD-TEXT-UPPLYSNING-ATTR (2)              
404800                                MOD-TEXT-UPPLYSNING-ATTR (3)              
404900                                MOD-TEXT-UPPLYSNING-ATTR (4)              
405000                                MOD-KVPB-PLAN-ATTR                        
405100                                                                          
405200     MOVE MFS-ROER-EJ-FAELT TO MOD-TEXT-PLANTYP                           
405300                               MOD-TIOMSPEC                               
405400                               MOD-IDARTNR-IN                             
405500                               MOD-KDBEHX-PLAN-IN                         
405600                               MOD-IDLEVNR-IN                             
405700                               MOD-IDARTNR                                
405800                               MOD-BEART-SVE                              
405900                               MOD-TEXT-ORSAK1                            
406000                               MOD-TEXT-ORSAK2                            
406100                               MOD-DATA-GRP1                              
406200                               MOD-TIFINLV                                
406300                               MOD-TIURPROD                               
406400                               MOD-KDLPSP                                 
406500                               MOD-TILPSP                                 
406600                               MOD-FLSEASON                               
406700                               MOD-FLTREND                                
406800                               MOD-DATA-GRP2                              
406900                               MOD-KVPB-PLAN                              
407000                               MOD-KVPB-SATS                              
407100                               MOD-TEXT-UPPLYSNING (1)                    
407200                               MOD-TEXT-UPPLYSNING (2)                    
407300                               MOD-TEXT-UPPLYSNING (3)                    
407400                               MOD-TEXT-UPPLYSNING (4)                    
407500                               MOD-MESSAGE-BOTTOM                         
407600                               MOD-TEARTNOT1-IN                           
407700                               MOD-TEARTNOT2-IN                           
407800                               MOD-KDERS                                  
407900                               MOD-REPLACES                               
408000                               MOD-REPL-BY                                
408100                                                                          
408200     MOVE 1 TO IY                                                         
408300     PERFORM UNTIL IY > MAX-ANT-PERIODER-I-TAB                            
408400       MOVE 1 TO IX                                                       
408500       MOVE MFS-ROER-EJ-FAELT TO MOD-PERIOD (IY)                          
408600       PERFORM UNTIL IX > MAX-ANT-VECKOR-I-TAB                            
408700         MOVE MFS-FORMATETS-ATTR TO MOD-AVROP-TAB-ATTR (IY, IX)           
408800         MOVE MFS-ROER-EJ-FAELT TO MOD-AVROP-TAB (IY, IX)                 
408900         ADD 1 TO IX                                                      
409000       END-PERFORM                                                        
409100       MOVE MFS-ROER-EJ-FAELT TO MOD-PERIOD (IY)                          
409200       ADD 1 TO IY                                                        
409300     END-PERFORM                                                          
409400                                                                          
409500     MOVE 1 TO IX                                                         
409600     PERFORM UNTIL IX > MAX-ANT-GAMLA-AVROP                               
409700       MOVE MFS-FORMATETS-ATTR TO MOD-AVROP-GAM-ATTR (IX)                 
409800       MOVE MFS-ROER-EJ-FAELT  TO MOD-AVROP-GAM (IX)                      
409900       ADD 1 TO IX                                                        
410000     END-PERFORM                                                          
410100     .                                                                    
410200     EJECT                                                                
410300 IMS-GET-MSG SECTION.                                                     
410400                                                                          
410500     MOVE '  QC' TO GODK-STATUSKODER                                      
410600     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
410700     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
410800     PERFORM IMS-STATUSKONTROLL                                           
410900     .                                                                    
411000     SKIP2                                                                
411100 IMS-INSERT-MSG SECTION.                                                  
411200                                                                          
411300     IF ENGLISH-TEXT                                                      
411400        MOVE 'N' TO MFS-KDHUVOMR                                          
411500     END-IF                                                               
411600     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
411700     MOVE SPACE TO GODK-STATUSKODER                                       
411800     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
411900     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
412000     PERFORM IMS-STATUSKONTROLL                                           
412100     .                                                                    
412200     SKIP2                                                                
412300 IMS-GET-WMSGKOM SECTION.                                                 
412400     MOVE '  QD' TO GODK-STATUSKODER                                      
412500     CALL CBLTDLI USING GN MSG-PCB MSG-KOM-WMSGKOM                        
412600     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
412700     PERFORM IMS-STATUSKONTROLL                                           
412800     .                                                                    
412900     SKIP2                                                                
413000 IMS-INSERT-WMSGKOM SECTION.                                              
413100     MOVE '  ' TO GODK-STATUSKODER                                        
413200     CALL CBLTDLI USING ISRT MSGKOM-PCB MSG-KOM-WMSGKOM                   
413300     MOVE MSGKOM-STATUS-CODE TO STATUS-WS                                 
413400     PERFORM IMS-STATUSKONTROLL                                           
413500     .                                                                    
413600     EJECT                                                                
413700 IMS-ISRT-ALT1-MSG-2135 SECTION.                                          
413800     MOVE 'IMS-ISRT-ALT1-MSG-2135 '  TO DBS-SECTION                       
413900                                                                          
414000     MOVE SPACE TO GODK-STATUSKODER                                       
414100     CALL CBLTDLI USING ISRT ALT1-PCB P-TO-P-SW                           
414200     MOVE ALT1-STATUS-CODE TO STATUS-WS                                   
414300     PERFORM IMS-STATUSKONTROLL                                           
414400     .                                                                    
414500     SKIP2                                                                
414600 IMS-GU-WDK601          SECTION.                                          
414700     MOVE 'IMS-GU-WDK601 '       TO DBS-SECTION                           
414800                                                                          
414810     STRING 'WDK601  (IDARTNR  =' W-IDARTNR-X ')'                         
415000            DELIMITED BY SIZE INTO SSA1                                   
415100     MOVE '  GE' TO GODK-STATUSKODER                                      
415200     CALL CBLTDLI USING GU   WDK6-PCB DLI-IO-WDK601 SSA1                  
415300     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
415400     PERFORM IMS-STATUSKONTROLL                                           
415500     .                                                                    
415600     SKIP3                                                                
416814 IMS-GHNP-WDK611 SECTION.                                                 
416815     MOVE 'IMS-GHNP-WDK611 '    TO DBS-SECTION                            
416817                                                                          
416818     MOVE 'WDK611 ' TO SSA1                                               
416819     MOVE '  GE' TO GODK-STATUSKODER                                      
416820     CALL CBLTDLI USING GHNP WDK6-PCB DLI-IO-WDK611 SSA1                  
416821     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
416822     PERFORM IMS-STATUSKONTROLL                                           
416823     .                                                                    
416824     SKIP3                                                                
416825 IMS-GNP-WDK621 SECTION.                                                  
416826     MOVE 'IMS-GNP-WDK621 '   TO DBS-SECTION                              
416900                                                                          
417000     STRING 'WDK621  (DAPRLIST=>' W-DAPRLIST-X ')'                        
417100            DELIMITED BY SIZE INTO SSA1                                   
417200     MOVE '  GE' TO GODK-STATUSKODER                                      
417300     CALL CBLTDLI USING GNP WDK6-PCB DLI-IO-WDK621 SSA1                   
417400     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
417500     PERFORM IMS-STATUSKONTROLL                                           
417600     .                                                                    
417700     SKIP3                                                                
417800 IMS-GNP-WDK623 SECTION.                                                  
417900     MOVE 'IMS-GNP-WDK623 '  TO DBS-SECTION                               
418000                                                                          
418100     MOVE 'WDK611 ' TO SSA1                                               
418200     MOVE 'WDK623 ' TO SSA2                                               
418300     MOVE '  GE' TO GODK-STATUSKODER                                      
418400     CALL CBLTDLI USING GNP WDK6-PCB DLI-IO-WDK623 SSA1 SSA2              
418500     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
418600     PERFORM IMS-STATUSKONTROLL                                           
418700     .                                                                    
418800                                                                          
418900 IMS-GHNP-WDK625 SECTION.                                                 
419000     MOVE 'IMS-GHNP-WDK625 '  TO DBS-SECTION                              
419100                                                                          
419200     MOVE  'WDK611  *F(KDSEGKEY =1)'    TO SSA1                           
419300     STRING 'WDK625  (KDNOTTYP =' W-KDNOTTYP-X ')'                        
419400            DELIMITED BY SIZE       INTO SSA2                             
419500     MOVE '  GE'                      TO GODK-STATUSKODER                 
419600     CALL CBLTDLI USING GHNP WDK6-PCB DLI-IO-WDK625 SSA1 SSA2             
419700     MOVE WDK6-STATUS-CODE            TO STATUS-WS                        
419800     PERFORM IMS-STATUSKONTROLL                                           
419900     .                                                                    
420000     SKIP3                                                                
420100 IMS-REPL-WDK625 SECTION.                                                 
420200     MOVE 'IMS-REPL-WDK625' TO DBS-SECTION                                
420300                                                                          
420400     MOVE '  '   TO GODK-STATUSKODER                                      
420500     CALL CBLTDLI USING REPL WDK6-PCB DLI-IO-WDK625                       
420600     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
420700     PERFORM IMS-STATUSKONTROLL                                           
420800     .                                                                    
420900     SKIP3                                                                
421000 IMS-ISRT-WDK625 SECTION.                                                 
421100     MOVE 'IMS-ISRT-WDK625 ' TO DBS-SECTION                               
421200                                                                          
421300     STRING 'WDK601  (IDARTNR  =' W-IDARTNR-X ')'                         
421400             DELIMITED BY SIZE    INTO SSA1                               
421500     MOVE  'WDK611  (KDSEGKEY =1)'    TO SSA2                             
421600     MOVE  'WDK625 '                TO SSA3                               
421700     MOVE '  ' TO GODK-STATUSKODER                                        
421800     CALL CBLTDLI USING ISRT WDK6-PCB                                     
421900                               DLI-IO-WDK625                              
422000                               SSA1                                       
422100                               SSA2                                       
422200                               SSA3                                       
422300     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
422400     PERFORM IMS-STATUSKONTROLL                                           
422500     .                                                                    
422600     SKIP3                                                                
422700 IMS-DELETE-WDK625 SECTION.                                               
422800     MOVE 'IMS-DELETE-WDK625 ' TO DBS-SECTION                             
422900                                                                          
423000     MOVE '  '   TO GODK-STATUSKODER                                      
423100     CALL CBLTDLI USING DLET WDK6-PCB DLI-IO-WDK625                       
423200     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
423300     PERFORM IMS-STATUSKONTROLL                                           
423400     .                                                                    
423500     EJECT                                                                
423510 IMS-GU-WDK611 SECTION.                                                   
423520     MOVE 'IMS-GU-WDK611    '  TO DBS-SECTION                             
423540                                                                          
423550     STRING 'WDK601  (IDARTNR  =' W-IDARTNR-X ')'                         
423560         DELIMITED BY SIZE INTO SSA1                                      
423570     MOVE 'WDK611  ' TO SSA2                                              
423580     MOVE '    ' TO GODK-STATUSKODER                                      
423590     CALL CBLTDLI USING GU WDK6-PCB DLI-IO-WDK611 SSA1 SSA2               
423591     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
423592     PERFORM IMS-STATUSKONTROLL                                           
423593     SKIP3                                                                
423594     .                                                                    
423595 IMS-GHNP-WDK622 SECTION.                                                 
423596     MOVE 'IMS-GHNP-WDK622  '  TO DBS-SECTION                             
423598                                                                          
423599     MOVE 'WDK622  ' TO SSA1                                              
423600     MOVE '  GE' TO GODK-STATUSKODER                                      
423601     CALL CBLTDLI USING GHNP WDK6-PCB DLI-IO-WDK622 SSA1                  
423602     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
423603     PERFORM IMS-STATUSKONTROLL                                           
423604     .                                                                    
423605                                                                          
423606 IMS-INSERT-WDK622 SECTION.                                               
423607     MOVE 'IMS-INSERT-WDK622'  TO DBS-SECTION                             
423609                                                                          
423610     STRING 'WDK601  (IDARTNR  =' W-IDARTNR-X ')'                         
423611         DELIMITED BY SIZE INTO SSA1                                      
423612     MOVE 'WDK611  ' TO SSA2                                              
423613     MOVE 'WDK622  *F' TO SSA3                                            
423614     MOVE '  ' TO GODK-STATUSKODER                                        
423615     CALL CBLTDLI USING ISRT WDK6-PCB DLI-IO-WDK622 SSA1 SSA2 SSA3        
423616     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
423617     PERFORM IMS-STATUSKONTROLL                                           
423618     .                                                                    
423619                                                                          
423620 IMS-DLET-WDK622 SECTION.                                                 
423621     MOVE 'IMS-DLET-WDK622  '  TO DBS-SECTION                             
423623                                                                          
423624     MOVE '  '   TO GODK-STATUSKODER                                      
423625     CALL CBLTDLI USING DLET WDK6-PCB DLI-IO-WDK622                       
423626     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
423627     PERFORM IMS-STATUSKONTROLL                                           
423628     .                                                                    
423629     EJECT                                                                
423630 IMS-GU-LEVART SECTION.                                                   
423700     MOVE 'IMS-GU-LEVART '  TO DBS-SECTION                                
423710                                                                          
423800     STRING 'WDD901  (WDD901KY =' W-WDD901KY-X ')'                        
424000            DELIMITED BY SIZE INTO SSA1                                   
424100     MOVE '  GE' TO GODK-STATUSKODER                                      
424200     CALL CBLTDLI USING GU  INLB-PCB DLI-IO-AREA SSA1                     
424300     MOVE INLB-STATUS-CODE TO STATUS-WS                                   
424400     PERFORM IMS-STATUSKONTROLL                                           
424500     .                                                                    
424600     EJECT                                                                
424700 IMS-GNP-LEVERANTOER-SEG-KVAL SECTION.                                    
424800     MOVE 'IMS-GNP-LEVERANTOER-SEG-KVAL '  TO DBS-SECTION                 
424900                                                                          
424910     STRING 'WDD902  (IDLEVNR  =' W-IDLEVNR-X ')'                         
425100            DELIMITED BY SIZE INTO SSA1                                   
425200     MOVE '  GE' TO GODK-STATUSKODER                                      
425300     CALL CBLTDLI USING GNP  INLB-PCB DLI-IO-AREA SSA1                    
425400     MOVE INLB-STATUS-CODE TO STATUS-WS                                   
425500     PERFORM IMS-STATUSKONTROLL                                           
425600     .                                                                    
425700     SKIP3                                                                
425800 IMS-GHNP-OMSPEC-SEG SECTION.                                             
425900     MOVE 'IMS-GHNP-OMSPEC-SEG '  TO DBS-SECTION                          
426000                                                                          
426010     STRING 'WDD902  (IDLEVNR  =' W-IDLEVNR-X ')'                         
426200            DELIMITED BY SIZE INTO SSA1                                   
426300     MOVE 'WDD904 ' TO SSA2                                               
426400     MOVE '  GE' TO GODK-STATUSKODER                                      
426500     CALL CBLTDLI USING GHNP INLB-PCB DLI-IO-AREA SSA1 SSA2               
426600     MOVE INLB-STATUS-CODE TO STATUS-WS                                   
426700     PERFORM IMS-STATUSKONTROLL                                           
426800     .                                                                    
426900     SKIP3                                                                
427000 IMS-GHNP-OMSPEC-SEG-FIRST SECTION.                                       
427100     MOVE 'IMS-GHNP-OMSPEC-SEG-FIRST '  TO DBS-SECTION                    
427200                                                                          
427300     STRING 'WDD902  *F(IDLEVNR  =' W-IDLEVNR-X ')'                       
427400            DELIMITED BY SIZE INTO SSA1                                   
427500     MOVE 'WDD904 ' TO SSA2                                               
427600     MOVE '  GE' TO GODK-STATUSKODER                                      
427700     CALL CBLTDLI USING GHNP INLB-PCB DLI-IO-AREA SSA1 SSA2               
427800     MOVE INLB-STATUS-CODE TO STATUS-WS                                   
427900     PERFORM IMS-STATUSKONTROLL                                           
428000     .                                                                    
428100     EJECT                                                                
428200 IMS-GU-AVROP-KVAL SECTION.                                               
428300     MOVE 'IMS-GU-AVROP-KVAL '  TO DBS-SECTION                            
428400                                                                          
428410     STRING 'WDD901  (WDD901KY =' W-WDD901KY-X ')'                        
428600            DELIMITED BY SIZE INTO SSA1                                   
428610     STRING 'WDD902  (IDLEVNR  =' W-IDLEVNR-X ')'                         
428800            DELIMITED BY SIZE INTO SSA2                                   
428900     STRING 'WDD905  (DAAVROP  =' W-DAAVROP-X                             
429000                    '&KDAVROP  =' W-KDAVROP-X ')'                         
429100            DELIMITED BY SIZE INTO SSA3                                   
429200     MOVE '  GE' TO GODK-STATUSKODER                                      
429300     CALL CBLTDLI USING GU INLB-PCB DLI-IO-AREA SSA1 SSA2 SSA3            
429400     MOVE INLB-STATUS-CODE TO STATUS-WS                                   
429500     PERFORM IMS-STATUSKONTROLL                                           
429600     .                                                                    
429700     SKIP3                                                                
429800 IMS-GHNP-AVROP-KVAL-FIRST SECTION.                                       
429900     MOVE 'IMS-GHNP-AVROP-KVAL-FIRST '  TO DBS-SECTION                    
430000                                                                          
430010     STRING 'WDD902  (IDLEVNR  =' W-IDLEVNR-X ')'                         
430200            DELIMITED BY SIZE INTO SSA1                                   
430300     STRING 'WDD905  *F(DAAVROP  =' W-DAAVROP-X                           
430400                      '&KDAVROP  =' W-KDAVROP-X ')'                       
430500            DELIMITED BY SIZE INTO SSA2                                   
430600     MOVE '  GE' TO GODK-STATUSKODER                                      
430700     CALL CBLTDLI USING GHNP INLB-PCB DLI-IO-AREA SSA1 SSA2               
430800     MOVE INLB-STATUS-CODE TO STATUS-WS                                   
430900     PERFORM IMS-STATUSKONTROLL                                           
431000     .                                                                    
431100     SKIP3                                                                
431200 IMS-GHNP-AVROP-KVAL-KEY-FIRST SECTION.                                   
431300     MOVE 'IMS-GHNP-AVROP-KVAL-KEY-FIRST'  TO DBS-SECTION                 
431400                                                                          
431410     STRING 'WDD902  (IDLEVNR  =' W-IDLEVNR-X ')'                         
431600            DELIMITED BY SIZE INTO SSA1                                   
431700     STRING 'WDD905  *F(WDD905KY =' W-WDD905KY-X                          
431800                      '&KDAVROP  =' W-KDAVROP-X ')'                       
431900            DELIMITED BY SIZE INTO SSA2                                   
432000     MOVE '  GE' TO GODK-STATUSKODER                                      
432100     CALL CBLTDLI USING GHNP INLB-PCB DLI-IO-AREA SSA1 SSA2               
432200     MOVE INLB-STATUS-CODE TO STATUS-WS                                   
432300     PERFORM IMS-STATUSKONTROLL                                           
432400     .                                                                    
432500     SKIP3                                                                
432600 IMS-GHU-AVROP-KVAL-KEY SECTION.                                          
432700     MOVE 'IMS-GHU-AVROP-KVAL-KEY '  TO DBS-SECTION                       
432800*                                                                         
432801     STRING 'WDD901  (WDD901KY =' W-WDD901KY-X ')'                        
433000            DELIMITED BY SIZE INTO SSA1                                   
433010     STRING 'WDD902  (IDLEVNR  =' W-IDLEVNR-X ')'                         
433200            DELIMITED BY SIZE INTO SSA2                                   
433210     STRING 'WDD905  (WDD905KY =' W-WDD905KY-X                            
433400                    '&KDAVROP  =' W-KDAVROP-X ')'                         
433500            DELIMITED BY SIZE INTO SSA3                                   
433600     MOVE '  GE' TO GODK-STATUSKODER                                      
433700     CALL CBLTDLI USING GHU INLB-PCB DLI-IO-AREA9 SSA1 SSA2 SSA3          
433800     MOVE INLB-STATUS-CODE TO STATUS-WS                                   
433900     PERFORM IMS-STATUSKONTROLL                                           
434000     .                                                                    
434100     EJECT                                                                
434200 IMS-GHU-AVROP-KVAL-KEY2 SECTION.                                         
434300     MOVE 'IMS-GHU-AVROP-KVAL-KEY2 '  TO DBS-SECTION                      
434400*                                                                         
434410     STRING 'WDD901  (WDD901KY =' W-WDD901KY-X ')'                        
434600            DELIMITED BY SIZE INTO SSA1                                   
434610     STRING 'WDD902  (IDLEVNR  =' W-IDLEVNR-X ')'                         
434800            DELIMITED BY SIZE INTO SSA2                                   
434810     STRING 'WDD905  (WDD905KY =' W-WDD905KY-X                            
435000                    '&KDAVROP  =' W-KDAVROP-X ')'                         
435100            DELIMITED BY SIZE INTO SSA3                                   
435200     MOVE '  GE' TO GODK-STATUSKODER                                      
435300     CALL CBLTDLI USING GHU INLB-PCB DLI-IO-AREA SSA1 SSA2 SSA3           
435400     MOVE INLB-STATUS-CODE TO STATUS-WS                                   
435500     PERFORM IMS-STATUSKONTROLL                                           
435600     .                                                                    
435700     EJECT                                                                
435800 IMS-GHNP-AVROP-FIRST SECTION.                                            
435900     MOVE 'IMS-GHNP-AVROP-FIRST '  TO DBS-SECTION                         
436000                                                                          
436100     STRING 'WDD902  *F(IDLEVNR  =' W-IDLEVNR-X ')'                       
436200            DELIMITED BY SIZE INTO SSA1                                   
436300     STRING 'WDD905  (KDAVROP  =' W-KDAVROP-X ')'                         
436400            DELIMITED BY SIZE INTO SSA2                                   
436500     MOVE '  GE' TO GODK-STATUSKODER                                      
436600     CALL CBLTDLI USING GHNP INLB-PCB DLI-IO-AREA SSA1 SSA2               
436700     MOVE INLB-STATUS-CODE TO STATUS-WS                                   
436800     PERFORM IMS-STATUSKONTROLL                                           
436900     .                                                                    
437000     SKIP3                                                                
437100 IMS-GHNP-AVROP-GREATER-DAT SECTION.                                      
437200     MOVE 'IMS-GHNP-AVROP-GREATER-DAT '  TO DBS-SECTION                   
437300                                                                          
437400     STRING 'WDD902  *F(IDLEVNR  =' W-IDLEVNR-X ')'                       
437500            DELIMITED BY SIZE INTO SSA1                                   
437600     STRING 'WDD905  (DAAVROP  >' W-DAAVROP-X                             
437700                    '&KDAVROP  =' W-KDAVROP-X ')'                         
437800            DELIMITED BY SIZE INTO SSA2                                   
437900     MOVE '  GE' TO GODK-STATUSKODER                                      
438000     CALL CBLTDLI USING GHNP INLB-PCB DLI-IO-AREA SSA1 SSA2               
438100     MOVE INLB-STATUS-CODE TO STATUS-WS                                   
438200     PERFORM IMS-STATUSKONTROLL                                           
438300     .                                                                    
438400     SKIP3                                                                
438500 IMS-GHNP-AVROP-OKVAL-GR-DAT SECTION.                                     
438600     MOVE 'IMS-GHNP-AVROP-OKVAL-GR-DAT ' TO DBS-SECTION                   
438700                                                                          
438800     STRING 'WDD902  *F(IDLEVNR  =' W-IDLEVNR-X ')'                       
438900            DELIMITED BY SIZE INTO SSA1                                   
439000     STRING 'WDD905  (DAAVROP  >' W-DAAVROP-X ')'                         
439100            DELIMITED BY SIZE INTO SSA2                                   
439200     MOVE '  GE' TO GODK-STATUSKODER                                      
439300     CALL CBLTDLI USING GHNP INLB-PCB DLI-IO-AREA SSA1 SSA2               
439400     MOVE INLB-STATUS-CODE TO STATUS-WS                                   
439500     PERFORM IMS-STATUSKONTROLL                                           
439600     .                                                                    
439700     EJECT                                                                
439800 IMS-GHNP-AVROP-OKVAL-NEXT    SECTION.                                    
439900     MOVE 'IMS-GHNP-AVROP-OKVAL-NEXT '  TO DBS-SECTION                    
440000                                                                          
440010     STRING 'WDD902  (IDLEVNR  =' W-IDLEVNR-X ')'                         
440200            DELIMITED BY SIZE INTO SSA1                                   
440300     MOVE 'WDD905 ' TO SSA2                                               
440400     MOVE '  GE' TO GODK-STATUSKODER                                      
440500     CALL CBLTDLI USING GHNP INLB-PCB DLI-IO-AREA SSA1 SSA2               
440600     MOVE INLB-STATUS-CODE TO STATUS-WS                                   
440700     PERFORM IMS-STATUSKONTROLL                                           
440800     .                                                                    
440900     SKIP3                                                                
441000 IMS-GHNP-AVROP-NEXT SECTION.                                             
441100     MOVE 'IMS-GHNP-AVROP-NEXT '  TO DBS-SECTION                          
441200                                                                          
441210     STRING 'WDD902  (IDLEVNR  =' W-IDLEVNR-X ')'                         
441400            DELIMITED BY SIZE INTO SSA1                                   
441500     STRING 'WDD905  (KDAVROP  =' W-KDAVROP-X ')'                         
441600            DELIMITED BY SIZE INTO SSA2                                   
441700     MOVE '  GE' TO GODK-STATUSKODER                                      
441800     CALL CBLTDLI USING GHNP INLB-PCB DLI-IO-AREA SSA1 SSA2               
441900     MOVE INLB-STATUS-CODE TO STATUS-WS                                   
442000     PERFORM IMS-STATUSKONTROLL                                           
442100     .                                                                    
442200     EJECT                                                                
442300 IMS-GNP-AVROP-F     SECTION.                                             
442400     MOVE 'IMS-GNP-AVROP-F  '  TO DBS-SECTION                             
442500                                                                          
442600     STRING 'WDD902  *F(IDLEVNR  =' W-IDLEVNR-X ')'                       
442700            DELIMITED BY SIZE INTO SSA1                                   
442800     MOVE 'WDD905 ' TO SSA2                                               
442900     MOVE '  GE' TO GODK-STATUSKODER                                      
443000     CALL CBLTDLI USING GNP INLB-PCB DLI-IO-AREA SSA1 SSA2                
443100     MOVE INLB-STATUS-CODE TO STATUS-WS                                   
443200     PERFORM IMS-STATUSKONTROLL                                           
443300     .                                                                    
443400     SKIP3                                                                
443500                                                                          
443600 IMS-GNP-SATSBEORDR SECTION.                                              
443700     MOVE 'IMS-GNP-SATSBEORDR '  TO DBS-SECTION                           
443800                                                                          
443810     STRING 'WDD902  (IDLEVNR  =' W-IDLEVNR-X ')'                         
444000            DELIMITED BY SIZE INTO SSA1                                   
444100     STRING 'WDD905  *F(WDD905KY =' W-WDD905KY-X                          
444200                      '&KDAVROP  =' W-KDAVROP-X ')'                       
444300            DELIMITED BY SIZE INTO SSA2                                   
444400     MOVE 'WDD907 ' TO SSA3                                               
444500     MOVE '  GE' TO GODK-STATUSKODER                                      
444600     CALL CBLTDLI USING GNP   INLB-PCB DLI-IO-AREA SSA1 SSA2              
444700                                                   SSA3                   
444800     MOVE INLB-STATUS-CODE TO STATUS-WS                                   
444900     PERFORM IMS-STATUSKONTROLL                                           
445000     .                                                                    
445100     EJECT                                                                
445200                                                                          
445300 IMS-GU-WDD906 SECTION.                                                   
445400     MOVE 'IMS-GU-WDD906 '  TO DBS-SECTION                                
445500                                                                          
445600     STRING 'WDD901  (WDD901KY =' W-WDD901KY-X ')'                        
445700            DELIMITED BY SIZE INTO SSA1                                   
445800     STRING 'WDD902  (IDLEVNR  =' W-IDLEVNR-X ')'                         
445900            DELIMITED BY SIZE INTO SSA2                                   
446000     STRING 'WDD905  (WDD905KY =' W-WDD905KY-X                            
446100                    '&KDAVROP  =' W-KDAVROP-X ')'                         
446200            DELIMITED BY SIZE INTO SSA3                                   
446300     MOVE 'WDD906   ' TO SSA4                                             
446400     MOVE '  GE' TO GODK-STATUSKODER                                      
446500     CALL CBLTDLI USING GU    WDD9-PCB DLI-IO-AREA906 SSA1 SSA2           
446600                                                      SSA3 SSA4           
446700     MOVE WDD9-STATUS-CODE TO STATUS-WS                                   
446800     PERFORM IMS-STATUSKONTROLL                                           
446900     .                                                                    
447000     SKIP3                                                                
447100                                                                          
447200 IMS-GU-WDD907 SECTION.                                                   
447300     MOVE 'IMS-GU-WDD907 '  TO DBS-SECTION                                
447400                                                                          
447500     STRING 'WDD901  (WDD901KY =' W-WDD901KY-X ')'                        
447600            DELIMITED BY SIZE INTO SSA1                                   
447700     STRING 'WDD902  (IDLEVNR  =' W-IDLEVNR-X ')'                         
447800            DELIMITED BY SIZE INTO SSA2                                   
447900     STRING 'WDD905  (WDD905KY =' W-WDD905KY-X                            
448000                    '&KDAVROP  =' W-KDAVROP-X ')'                         
448100            DELIMITED BY SIZE INTO SSA3                                   
448200     MOVE 'WDD907   ' TO SSA4                                             
448300     MOVE '  GE' TO GODK-STATUSKODER                                      
448400     CALL CBLTDLI USING GU   WDD9-PCB DLI-IO-AREA907 SSA1 SSA2            
448500                                                     SSA3 SSA4            
448600     MOVE WDD9-STATUS-CODE TO STATUS-WS                                   
448700     PERFORM IMS-STATUSKONTROLL                                           
448800     .                                                                    
448900     SKIP3                                                                
449000                                                                          
449100 IMS-GHU-WDD907 SECTION.                                                  
449200     MOVE 'IMS-GHU-WDD907 '  TO DBS-SECTION                               
449300                                                                          
449400     STRING 'WDD901  (WDD901KY =' W-WDD901KY-X ')'                        
449500            DELIMITED BY SIZE INTO SSA1                                   
449600     STRING 'WDD902  (IDLEVNR  =' W-IDLEVNR-X ')'                         
449700            DELIMITED BY SIZE INTO SSA2                                   
449800     STRING 'WDD905  (WDD905KY =' W-WDD905KY-X                            
449900                      '&KDAVROP  =' W-KDAVROP-X ')'                       
450000            DELIMITED BY SIZE INTO SSA3                                   
450100     STRING 'WDD907  (IDORDNSB =' W-IDORDNSB-X ')'                        
450200            DELIMITED BY SIZE INTO SSA4                                   
450300     MOVE '  GE' TO GODK-STATUSKODER                                      
450400     CALL CBLTDLI USING GHU WDD9-PCB DLI-IO-AREA907 SSA1 SSA2             
450500                                                    SSA3 SSA4             
450600     MOVE WDD9-STATUS-CODE TO STATUS-WS                                   
450700     PERFORM IMS-STATUSKONTROLL                                           
450800     .                                                                    
450900     EJECT                                                                
451000                                                                          
451010 IMS-GHU-WDD902  SECTION.                                                 
451020     MOVE 'IMS-GHU-WDD902    '  TO DBS-SECTION                            
451040                                                                          
451050     STRING 'WDD901  (WDD901KY =' W-WDD901KY-X ')'                        
451060         DELIMITED BY SIZE INTO SSA1                                      
451070     STRING 'WDD902  *F(IDLEVNR  =' W-IDLEVNR-X ')'                       
451080         DELIMITED BY SIZE INTO SSA2                                      
451090     MOVE '  GE' TO GODK-STATUSKODER                                      
451091     CALL CBLTDLI USING GHU WDD9-PCB DLI-IO-WDD902 SSA1 SSA2              
451092     MOVE WDD9-STATUS-CODE TO STATUS-WS                                   
451093     PERFORM IMS-STATUSKONTROLL                                           
451094     SKIP3                                                                
451095     .                                                                    
451096 IMS-GHU-WDD904 SECTION.                                                  
451097     MOVE 'IMS-GHU-WDD904   '  TO DBS-SECTION                             
451099                                                                          
451100     STRING 'WDD901  (WDD901KY =' W-WDD901KY-X ')'                        
451101         DELIMITED BY SIZE INTO SSA1                                      
451102     STRING 'WDD902  *F(IDLEVNR  =' W-IDLEVNR-X ')'                       
451103         DELIMITED BY SIZE INTO SSA2                                      
451104     MOVE 'WDD904  ' TO SSA3                                              
451105     MOVE '  GE' TO GODK-STATUSKODER                                      
451106     CALL CBLTDLI USING GHU WDD9-PCB DLI-IO-WDD904 SSA1 SSA2 SSA3         
451107     MOVE WDD9-STATUS-CODE TO STATUS-WS                                   
451108     PERFORM IMS-STATUSKONTROLL                                           
451109     .                                                                    
451110     EJECT                                                                
451111 IMS-DLET-WDD904 SECTION.                                                 
451112     MOVE 'IMS-DLET-WDD904  '  TO DBS-SECTION                             
451114                                                                          
451115     MOVE '  '   TO GODK-STATUSKODER                                      
451116     CALL CBLTDLI USING DLET WDD9-PCB DLI-IO-WDD904                       
451117     MOVE WDD9-STATUS-CODE TO STATUS-WS                                   
451118     PERFORM IMS-STATUSKONTROLL                                           
451119     .                                                                    
451120 IMS-REPL-WDD904 SECTION.                                                 
451121     MOVE 'IMS-REPL-WDD904  '  TO DBS-SECTION                             
451123                                                                          
451124     MOVE '  '   TO GODK-STATUSKODER                                      
451125     CALL CBLTDLI USING REPL WDD9-PCB DLI-IO-WDD904                       
451126     MOVE WDD9-STATUS-CODE TO STATUS-WS                                   
451127     PERFORM IMS-STATUSKONTROLL                                           
451128     .                                                                    
451129 IMS-REPL-WDD902 SECTION.                                                 
451130     MOVE 'IMS-REPL-WDD902  '  TO DBS-SECTION                             
451132                                                                          
451133     MOVE '  '   TO GODK-STATUSKODER                                      
451134     CALL CBLTDLI USING REPL WDD9-PCB DLI-IO-WDD902                       
451135     MOVE WDD9-STATUS-CODE TO STATUS-WS                                   
451136     PERFORM IMS-STATUSKONTROLL                                           
451137     .                                                                    
451140 IMS-GU-2216-SEG SECTION.                                                 
451200     MOVE 'IMS-GU-2216-SEG '  TO DBS-SECTION                              
451300                                                                          
451400     STRING 'WLXXBK01(WDGXKEY  =' W-WDGXKEY-ROT ')'                       
451500            DELIMITED BY SIZE INTO SSA1                                   
451600     STRING 'WLXXBK11(IDLEVNR  =' W-WDGXKEY-IDLEVNR ')'                   
451700            DELIMITED BY SIZE INTO SSA2                                   
451800     MOVE '  GE' TO GODK-STATUSKODER                                      
451900     CALL CBLTDLI USING GU XXBK-PCB DLI-IO-AREA SSA1 SSA2                 
452000     MOVE XXBK-STATUS-CODE TO STATUS-WS                                   
452100     PERFORM IMS-STATUSKONTROLL                                           
452200     .                                                                    
452300     SKIP3                                                                
452400 IMS-INSERT-ART-SEG SECTION.                                              
452500     MOVE 'IMS-INSERT-ART-SEG '  TO DBS-SECTION                           
452600                                                                          
452700     MOVE   'WDD901 ' TO SSA1                                             
452800     MOVE '  II' TO GODK-STATUSKODER                                      
452900     CALL CBLTDLI USING ISRT INLB-PCB DLI-IO-AREA                         
453000                               SSA1                                       
453100     MOVE INLB-STATUS-CODE TO STATUS-WS                                   
453200     PERFORM IMS-STATUSKONTROLL                                           
453300     .                                                                    
453400     SKIP3                                                                
453500 IMS-INSERT-LEVERANTOER-SEG SECTION.                                      
453600     MOVE 'IMS-INSERT-LEVERANTOER-SEG '  TO DBS-SECTION                   
453700                                                                          
453710     STRING 'WDD901  (WDD901KY =' W-WDD901KY-X ')'                        
453900            DELIMITED BY SIZE INTO SSA1                                   
454000     MOVE   'WDD902 ' TO SSA2                                             
454100     MOVE '  II' TO GODK-STATUSKODER                                      
454200     CALL CBLTDLI USING ISRT INLB-PCB DLI-IO-AREA                         
454300                               SSA1 SSA2                                  
454400     MOVE INLB-STATUS-CODE TO STATUS-WS                                   
454500     PERFORM IMS-STATUSKONTROLL                                           
454600     .                                                                    
454700     EJECT                                                                
454800 IMS-INSERT-OMSPEC-SEG SECTION.                                           
454900     MOVE 'IMS-INSERT-OMSPEC-SEG '  TO DBS-SECTION                        
455000                                                                          
455010     STRING 'WDD901  (WDD901KY =' W-WDD901KY-X ')'                        
455200            DELIMITED BY SIZE INTO SSA1                                   
455210     STRING 'WDD902  (IDLEVNR  =' W-IDLEVNR-X ')'                         
455400            DELIMITED BY SIZE INTO SSA2                                   
455500     MOVE   'WDD904 ' TO SSA3                                             
455600     MOVE '  ' TO GODK-STATUSKODER                                        
455700     CALL CBLTDLI USING ISRT INLB-PCB DLI-IO-AREA                         
455800                               SSA1 SSA2 SSA3                             
455900     MOVE INLB-STATUS-CODE TO STATUS-WS                                   
456000     PERFORM IMS-STATUSKONTROLL                                           
456100     .                                                                    
456200     EJECT                                                                
456300 IMS-INSERT-AVROP-SEG SECTION.                                            
456400     MOVE 'IMS-INSERT-AVROP-SEG '  TO DBS-SECTION                         
456500                                                                          
456510     STRING 'WDD901  (WDD901KY =' W-WDD901KY-X ')'                        
456700            DELIMITED BY SIZE INTO SSA1                                   
456710     STRING 'WDD902  (IDLEVNR  =' W-IDLEVNR-X ')'                         
456900            DELIMITED BY SIZE INTO SSA2                                   
457000     MOVE 'WDD905 ' TO SSA3                                               
457100     MOVE '  ' TO GODK-STATUSKODER                                        
457200     CALL CBLTDLI USING ISRT INLB-PCB DLI-IO-AREA                         
457300                               SSA1 SSA2 SSA3                             
457400     MOVE INLB-STATUS-CODE TO STATUS-WS                                   
457500     PERFORM IMS-STATUSKONTROLL                                           
457600     .                                                                    
457700     SKIP3                                                                
457800 IMS-INSERT-2218-SEG SECTION.                                             
457900     MOVE 'IMS-INSERT-2218-SEG '  TO DBS-SECTION                          
458000                                                                          
458100     STRING 'WLXXBL01(WDGXKEY  =' W-WDGXKEY-ROT ')'                       
458200            DELIMITED BY SIZE INTO SSA1                                   
458300     MOVE 'WLXXBL11 ' TO SSA2                                             
458400     MOVE '  II' TO GODK-STATUSKODER                                      
458500     CALL CBLTDLI USING ISRT XXBL-PCB DLI-IO-AREA SSA1 SSA2               
458600     MOVE XXBL-STATUS-CODE TO STATUS-WS                                   
458700     PERFORM IMS-STATUSKONTROLL                                           
458800     .                                                                    
458900     EJECT                                                                
459000 IMS-INSERT-2218-PERIOD-SEG SECTION.                                      
459100     MOVE 'IMS-INSERT-2218-PERIOD-SEG '  TO DBS-SECTION                   
459200                                                                          
459300     STRING 'WLXXBL01(WDGXKEY  =' W-WDGXKEY-PERIOD-ROT ')'                
459400            DELIMITED BY SIZE INTO SSA1                                   
459500     MOVE 'WLXXBL11 ' TO SSA2                                             
459600     MOVE '  II' TO GODK-STATUSKODER                                      
459700     CALL CBLTDLI USING ISRT XXBL-PCB DLI-IO-AREA SSA1 SSA2               
459800     MOVE XXBL-STATUS-CODE TO STATUS-WS                                   
459900     PERFORM IMS-STATUSKONTROLL                                           
460000     .                                                                    
460100     SKIP3                                                                
460200 IMS-INSERT-2204-SEG SECTION.                                             
460300     MOVE 'IMS-INSERT-2204-SEG '  TO DBS-SECTION                          
460400                                                                          
460500     STRING 'WLXXBJ01(WDG3KEY  =' W-2203-KEY ')'                          
460600            DELIMITED BY SIZE INTO SSA1                                   
460700     MOVE 'WLXXBJ11   ' TO SSA2                                           
460800     MOVE '  ' TO GODK-STATUSKODER                                        
460900     CALL CBLTDLI USING ISRT XXBJ-PCB DLI-IO-AREA SSA1 SSA2               
461000     MOVE XXBJ-STATUS-CODE TO STATUS-WS                                   
461100     PERFORM IMS-STATUSKONTROLL                                           
461200     .                                                                    
461300     SKIP3                                                                
461400 IMS-INSERT-HAENDEL-SEG SECTION.                                          
461500     MOVE 'IMS-INSERT-HAENDEL-SEG '  TO DBS-SECTION                       
461600                                                                          
461700     STRING 'WLXXBM01(WDG3KEY  =' W-WDGXKEY-ROT ')'                       
461800            DELIMITED BY SIZE INTO SSA1                                   
461900     MOVE 'WLXXBM11   ' TO SSA2                                           
462000     MOVE '  ' TO GODK-STATUSKODER                                        
462100     CALL CBLTDLI USING ISRT XXBM-PCB DLI-IO-AREA SSA1 SSA2               
462200     MOVE XXBM-STATUS-CODE TO STATUS-WS                                   
462300     PERFORM IMS-STATUSKONTROLL                                           
462400     .                                                                    
463700     EJECT                                                                
463800 IMS-REPL-WDK611 SECTION.                                                 
463900     MOVE 'IMS-REPL-WDK611 '   TO DBS-SECTION                             
464000                                                                          
464100     MOVE '  ' TO GODK-STATUSKODER                                        
464200     CALL CBLTDLI USING REPL    WDK6-PCB DLI-IO-WDK611                    
464300     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
464400     PERFORM IMS-STATUSKONTROLL                                           
464500     .                                                                    
464600     SKIP3                                                                
464700 IMS-REPLACE-WDD9 SECTION.                                                
464800     MOVE 'IMS-REPLACE-WDD9 '  TO DBS-SECTION                             
464900                                                                          
465000     MOVE '  ' TO GODK-STATUSKODER                                        
465100     CALL CBLTDLI USING REPL    INLB-PCB DLI-IO-AREA                      
465200     MOVE INLB-STATUS-CODE TO STATUS-WS                                   
465300     PERFORM IMS-STATUSKONTROLL                                           
465400     .                                                                    
465500     SKIP3                                                                
465600 IMS-ISRT-2228 SECTION.                                                   
465601     MOVE 'IMS-ISRT-2228    '  TO DBS-SECTION                             
465620                                                                          
465700     STRING 'WLXXBW01(WDGXKEY  =' W-2227KEY-X ')'                         
465800            DELIMITED BY SIZE INTO SSA1                                   
465900     MOVE   'WLXXBW11 ' TO SSA2                                           
466000     MOVE '  II' TO GODK-STATUSKODER                                      
466100     CALL CBLTDLI USING ISRT XXBW-PCB DLI-IO-AREA2 SSA1 SSA2              
466200     MOVE XXBW-STATUS-CODE TO STATUS-WS                                   
466300     PERFORM IMS-STATUSKONTROLL                                           
466400     .                                                                    
466500     EJECT                                                                
466600 IMS-INSERT-XXCZ-2246 SECTION.                                            
466610     MOVE 'IMS-INSERT-XXCZ-2246' TO DBS-SECTION                           
466700                                                                          
466800     STRING 'WLXXCZ01(WDGXKEY  =' W-WDGXKEY-2245-X ')'                    
466900            DELIMITED BY SIZE INTO SSA1                                   
467000     MOVE   'WLXXCZ11 ' TO SSA2                                           
467100     MOVE '  II' TO GODK-STATUSKODER                                      
467200     CALL CBLTDLI USING ISRT XXCZ-PCB DLI-IO-AREA3 SSA1 SSA2              
467300     MOVE XXCZ-STATUS-CODE TO STATUS-WS                                   
467400     PERFORM IMS-STATUSKONTROLL                                           
467500     .                                                                    
467600     SKIP3                                                                
467700                                                                          
467800 IMS-DELETE-WDD9 SECTION.                                                 
467810     MOVE 'IMS-DELETE-WDD9     ' TO DBS-SECTION                           
467830                                                                          
467900     MOVE '  ' TO GODK-STATUSKODER                                        
468000     CALL CBLTDLI USING DLET INLB-PCB DLI-IO-AREA                         
468100     MOVE INLB-STATUS-CODE TO STATUS-WS                                   
468200     PERFORM IMS-STATUSKONTROLL                                           
468300     .                                                                    
468400     SKIP3                                                                
468500                                                                          
468600 IMS-DELETE-AVROP SECTION.                                                
468610     MOVE 'IMS-DELETE-AVROP    ' TO DBS-SECTION                           
468630                                                                          
468700     MOVE '  ' TO GODK-STATUSKODER                                        
468800     CALL CBLTDLI USING DLET INLB-PCB DLI-IO-AREA                         
468900     MOVE INLB-STATUS-CODE TO STATUS-WS                                   
469000     PERFORM IMS-STATUSKONTROLL                                           
469100     .                                                                    
469200     SKIP3                                                                
469300                                                                          
469400 IMS-DELETE-WDD907 SECTION.                                               
469410     MOVE 'IMS-DELETE-WDD907   ' TO DBS-SECTION                           
469430                                                                          
469500     MOVE '  ' TO GODK-STATUSKODER                                        
469600     CALL CBLTDLI USING DLET WDD9-PCB DLI-IO-AREA907                      
469700     MOVE WDD9-STATUS-CODE TO STATUS-WS                                   
469800     PERFORM IMS-STATUSKONTROLL                                           
469900     .                                                                    
470000     SKIP3                                                                
470100                                                                          
470200 IMS-GU-SATS-ROT       SECTION.                                           
470210     MOVE 'IMS-GU-SATS-ROT     ' TO DBS-SECTION                           
470230                                                                          
470300     STRING 'WLSATB01(IDARTNR  =' W-IDARTNR-X ')'                         
470400            DELIMITED BY SIZE INTO SSA1                                   
470500     MOVE '  GE' TO GODK-STATUSKODER                                      
470600     CALL CBLTDLI USING GU   SATB-PCB DLI-IO-AREA4 SSA1                   
470700     MOVE SATB-STATUS-CODE TO STATUS-WS                                   
470800     PERFORM IMS-STATUSKONTROLL                                           
470900     .                                                                    
471000     SKIP3                                                                
471100 IMS-GNP-ING-SATS-ART SECTION.                                            
471110     MOVE 'IMS-GNP-ING-SATS-ART' TO DBS-SECTION                           
471200                                                                          
471300     MOVE 'WLSATB11 ' TO SSA1                                             
471400     MOVE '  GE' TO GODK-STATUSKODER                                      
471500     CALL CBLTDLI USING GNP  SATB-PCB DLI-IO-AREA4 SSA1                   
471600     MOVE SATB-STATUS-CODE TO STATUS-WS                                   
471700     PERFORM IMS-STATUSKONTROLL                                           
471800     .                                                                    
471900     EJECT                                                                
472000 IMS-GU-LEVA01-WDF101-SHIP SECTION.                                       
472010     MOVE 'IMS-GU-LEVA01-WDF101-SHIP' TO DBS-SECTION                      
472100                                                                          
472200     STRING 'WDF101  (IDLEVNR  =' W-IDLEVNR-SHIP-X ')'                    
472300          DELIMITED BY SIZE INTO SSA1                                     
472400     MOVE '  GE' TO GODK-STATUSKODER                                      
472500     CALL CBLTDLI USING GU WDF1-PCB DLI-IO-AREA-F1 SSA1                   
472600     MOVE WDF1-STATUS-CODE TO STATUS-WS                                   
472700     PERFORM IMS-STATUSKONTROLL                                           
472800     .                                                                    
472900     SKIP3                                                                
473000 IMS-GU-LEVA14-WDF106-SHIP SECTION.                                       
473010     MOVE 'IMS-GU-LEVA14-WDF106-SHIP' TO DBS-SECTION                      
473100                                                                          
473200     STRING 'WDF101  (IDLEVNR  =' W-IDLEVNR-SHIP-X ')'                    
473300          DELIMITED BY SIZE INTO SSA1                                     
473400     STRING 'WDF106     '                                                 
473500          DELIMITED BY SIZE INTO SSA2                                     
473600     MOVE '  GE' TO GODK-STATUSKODER                                      
473700     CALL CBLTDLI USING GU WDF1-PCB DLI-IO-AREA-F106 SSA1 SSA2            
473800     MOVE WDF1-STATUS-CODE TO STATUS-WS                                   
473900     PERFORM IMS-STATUSKONTROLL                                           
474000     .                                                                    
474100     EJECT                                                                
474200 IMS-GU-ARTS01-WDK701 SECTION.                                            
474210     MOVE 'IMS-GU-ARTS01-WDK701     ' TO DBS-SECTION                      
474300                                                                          
474400     STRING 'WDK701  (IDARTNR  =' W-IDARTNR-X ')'                         
474500          DELIMITED BY SIZE INTO SSA1                                     
474600     MOVE '  GE' TO GODK-STATUSKODER                                      
474700     CALL CBLTDLI USING GU WDK7-PCB DLI-IO-AREA-K7 SSA1                   
474800     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
474900     PERFORM IMS-STATUSKONTROLL                                           
475000     .                                                                    
475100     EJECT                                                                
475200 IMS-GNP-ARTS11-WDK711 SECTION.                                           
475210     MOVE 'IMS-GNP-ARTS11-WDK711    ' TO DBS-SECTION                      
475300                                                                          
475400     STRING 'WDK711    '                                                  
475500          DELIMITED BY SIZE INTO SSA1                                     
475600     MOVE '  GE' TO GODK-STATUSKODER                                      
475700     CALL CBLTDLI USING GNP WDK7-PCB DLI-IO-AREA-K7 SSA1                  
475800     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
475900     PERFORM IMS-STATUSKONTROLL                                           
476000     .                                                                    
476100     EJECT                                                                
476200 IMS-GU-WDF301 SECTION.                                                   
476210     MOVE 'IMS-GU-WDF301            ' TO DBS-SECTION                      
476300                                                                          
476400     STRING 'WDF301  (WDF301KY =' W-WDF301KY-X ')'                        
476500          DELIMITED BY SIZE INTO SSA1                                     
476600     MOVE '  GE' TO GODK-STATUSKODER                                      
476700     CALL CBLTDLI USING GU WDF3-PCB DLI-IO-AREA-F301 SSA1                 
476800     MOVE WDF3-STATUS-CODE TO STATUS-WS                                   
476900     PERFORM IMS-STATUSKONTROLL                                           
477000     .                                                                    
477100*    SKIP3                                                                
477200 IMS-GHU-WDD601      Section.                                             
477210     MOVE 'IMS-GHU-WDD601           ' TO DBS-SECTION                      
477300                                                                          
477400     String 'WDD601  (WDD601KY>=' W-WDD601KY-MIN-X                        
477500                 OCH 'WDD601KY<=' W-WDD601KY-MAX-X ')'                    
477600     Delimited By Size Into SSA1                                          
477700     Move '  GE' To GODK-STATUSKODER                                      
477800     Call CBLTDLI Using GHU WDD6-PCB DLI-IO-WDD601 SSA1                   
477900     Move WDD6-STATUS-CODE To STATUS-WS                                   
478000     Perform IMS-STATUSKONTROLL                                           
478100     .                                                                    
478200     EJECT                                                                
478300 IMS-DELETE-WDD6     Section.                                             
478310     MOVE 'IMS-DELETE-WDD6          ' TO DBS-SECTION                      
478400                                                                          
478500     Move '  ' To GODK-STATUSKODER                                        
478600     Call CBLTDLI Using DLET WDD6-PCB DLI-IO-WDD601                       
478700     Move WDD6-STATUS-CODE To STATUS-WS                                   
478800     Perform IMS-STATUSKONTROLL                                           
478900     .                                                                    
480000     EJECT                                                                
480100 IMS-GU-WDGX2258 SECTION.                                                 
480200     MOVE 'IMS-GU-WDGX2258 '  TO DBS-SECTION                              
480300                                                                          
480400     MOVE SPACES              TO SSA1 SSA2                                
480500     STRING 'WDG301  (WDG3KEY  =' W-WDGXKEY-2257-X ')'                    
480600          DELIMITED BY SIZE INTO SSA1                                     
480700     STRING 'WDGX2258(IDLEVNRS =' W-WDGXKEY-2258-X ')'                    
480800          DELIMITED BY SIZE INTO SSA2                                     
480900     MOVE '  GE'              TO GODK-STATUSKODER                         
481000     CALL CBLTDLI USING GU 2257-PCB DLI-IO-WDGX2258 SSA1 SSA2             
481100     MOVE 2257-STATUS-CODE    TO STATUS-WS                                
481200     PERFORM IMS-STATUSKONTROLL                                           
481300     .                                                                    
481400     SKIP2                                                                
481500 IMS-GNP-WDGX2260 SECTION.                                                
481600     MOVE 'IMS-GNP-WDGX2260 '  TO DBS-SECTION                             
481700                                                                          
481800     MOVE SPACE               TO SSA1                                     
481900     STRING 'WDGX2260(DAAVROPF<=' W-DAAVROP-2260-X                        
482000                    '&DAAVROPT>=' W-DAAVROP-2260-X                        
482100                    '&IDANSKF <=' W-IDANSK-2260-X                         
482200                    '&IDANSKT >=' W-IDANSK-2260-X ')'                     
482300          DELIMITED BY SIZE INTO SSA1                                     
482400     MOVE '  GE'              TO GODK-STATUSKODER                         
482500     CALL CBLTDLI USING GNP 2257-PCB DLI-IO-WDGX2260 SSA1                 
482600     MOVE 2257-STATUS-CODE    TO STATUS-WS                                
482700     PERFORM IMS-STATUSKONTROLL                                           
482800     .                                                                    
482900     EJECT                                                                
483000                                                                          
483100 IMS-GU-WDB601-LEV SECTION.                                               
483200     MOVE 'IMS-GU-WDB601-LEV'  TO DBS-SECTION                             
483300                                                                          
483400     STRING 'WDB601  (IDLEVNDC =' W-IDLEVNR-DC-X ')'                      
483500          DELIMITED BY SIZE INTO SSA1                                     
483600     MOVE '  GE'           TO  GODK-STATUSKODER                           
483700     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-WDB601 SSA1                    
483800     MOVE WDB6-STATUS-CODE TO STATUS-WS                                   
483900     PERFORM IMS-STATUSKONTROLL                                           
484000     .                                                                    
484100     EJECT                                                                
484200 IMS-GU-WDJ201-OKVAL   SECTION.                                           
484300     MOVE 'IMS-GU-WDJ201-OKVAL'   TO DBS-SECTION                          
484400                                                                          
484500     STRING 'WDJ201  (WDJ2CSEQ>=' W-WDJ2CSEQ-MIN-X                        
484600                    '&WDJ2CSEQ<=' W-WDJ2CSEQ-MAX-X ')'                    
484700          DELIMITED BY SIZE INTO SSA1                                     
484800     MOVE '  GE' TO GODK-STATUSKODER                                      
484900     CALL CBLTDLI USING GU WDJ2-PCB DLI-IO-WDJ201 SSA1                    
485000     MOVE WDJ2-STATUS-CODE TO STATUS-WS                                   
485100     PERFORM IMS-STATUSKONTROLL                                           
485200     .                                                                    
485300     SKIP3                                                                
485400 IMS-GN-WDJ201-OKVAL   SECTION.                                           
485500     MOVE 'IMS-GN-WDJ201-OKVAL '  TO DBS-SECTION                          
485600                                                                          
485700     STRING 'WDJ201  (WDJ2CSEQ>=' W-WDJ2CSEQ-MIN-X                        
485800                    '&WDJ2CSEQ<=' W-WDJ2CSEQ-MAX-X ')'                    
485900          DELIMITED BY SIZE INTO SSA1                                     
486000     MOVE '  GE' TO GODK-STATUSKODER                                      
486100     CALL CBLTDLI USING GN WDJ2-PCB DLI-IO-WDJ201 SSA1                    
486200     MOVE WDJ2-STATUS-CODE TO STATUS-WS                                   
486300     PERFORM IMS-STATUSKONTROLL                                           
486400     .                                                                    
486500                                                                          
486600 IMS-GU-WLXXBK01 SECTION.                                                 
486610     MOVE 'IMS-GU-WLXXBK01  '  TO DBS-SECTION                             
486700                                                                          
486800     STRING 'WLXXBK01(WDGXKEY  =' W-WDGXKEY-ROT ')'                       
486900             DELIMITED BY SIZE INTO SSA1                                  
487000     MOVE '  ' TO GODK-STATUSKODER                                        
487100     CALL CBLTDLI USING GHU XXBK-PCB DLI-IO-AREA SSA1                     
487200     MOVE XXBK-STATUS-CODE TO STATUS-WS                                   
487300     PERFORM IMS-STATUSKONTROLL                                           
487500     .                                                                    
487600                                                                          
487700 IMS-GNP-WLXXBK11      SECTION.                                           
487710     MOVE 'IMS-GNP-WLXXBK11 '  TO DBS-SECTION                             
487730                                                                          
487900     STRING 'WLXXBK11(IDLEVNR  =' W-WDGXKEY-IDLEVNR ')'                   
488000             DELIMITED BY SIZE INTO SSA1                                  
488100     MOVE '  ' TO GODK-STATUSKODER                                        
488200     CALL CBLTDLI USING GNP XXBK-PCB DLI-IO-AREA SSA1                     
488300     MOVE XXBK-STATUS-CODE TO STATUS-WS                                   
488410     PERFORM IMS-STATUSKONTROLL                                           
488500     .                                                                    
488600                                                                          
488601 IMS-GU-WDR220 SECTION.                                                   
488602     MOVE 'IMS-GU-WDR220 '  TO DBS-SECTION                                
488604                                                                          
488605     STRING 'WDR201  (WDGXKEY  =' W-WDGX2231-X ')'                        
488606          DELIMITED BY SIZE INTO SSA1                                     
488607     STRING 'WDR220  (WDGXKEY  =' W-WDGX2232-X ')'                        
488608          DELIMITED BY SIZE INTO SSA2                                     
488609     MOVE '  GE' TO GODK-STATUSKODER                                      
488610     CALL CBLTDLI USING GU WDR2-PCB DLI-IO-WDGX2232 SSA1 SSA2             
488611     MOVE WDR2-STATUS-CODE TO STATUS-WS                                   
488612     PERFORM IMS-STATUSKONTROLL                                           
488613     .                                                                    
488614     SKIP2                                                                
488615 IMS-GU-WDGX2223 SECTION.                                                 
488620     MOVE 'IMS-GU-WDGX2223   '  TO DBS-SECTION                            
488630                                                                          
488640     STRING 'WDR501  (WDGXKEY  =' W-WDGXKEY-2223-X ')'                    
488650          DELIMITED BY SIZE INTO SSA1                                     
488660     MOVE '  GE'              TO GODK-STATUSKODER                         
488670     CALL CBLTDLI USING GU WDR5-PCB DLI-IO-WDGX2223 SSA1                  
488680     MOVE WDR5-STATUS-CODE    TO STATUS-WS                                
488681     PERFORM IMS-STATUSKONTROLL                                           
488691     .                                                                    
488692                                                                          
488693 IMS-GNP-WDGX2224 SECTION.                                                
488694     MOVE 'IMS-GHNP-WDGX2224  '  TO DBS-SECTION                           
488696                                                                          
488697     STRING 'WDR550  (IDARTNR  =' W-IDARTNR-X                             
488698                    '&IDDC     =' W-IDDC-X                                
488699                    '&KDLARM   =' W-KDLARM-X ')'                          
488700          DELIMITED BY SIZE INTO SSA1                                     
488701     MOVE '  GE'           TO GODK-STATUSKODER                            
488702     CALL CBLTDLI USING GNP WDR5-PCB DLI-IO-WDGX2224 SSA1                 
488703     MOVE WDR5-STATUS-CODE TO STATUS-WS                                   
488704     PERFORM IMS-STATUSKONTROLL                                           
488705     .                                                                    
488706                                                                          
488707 IMS-GHNP-WDGX2224 SECTION.                                               
488708     MOVE 'IMS-GHNP-WDGX2224  '  TO DBS-SECTION                           
488710                                                                          
488711     STRING 'WDR550  (IDARTNR  =' W-IDARTNR-X                             
488712                    '&IDDC     =' W-IDDC-X                                
488713                    '&KDLARM   =' W-KDLARM-X ')'                          
488714          DELIMITED BY SIZE INTO SSA1                                     
488715     MOVE '  GE'           TO GODK-STATUSKODER                            
488716     CALL CBLTDLI USING GHNP WDR5-PCB DLI-IO-WDGX2224 SSA1                
488717     MOVE WDR5-STATUS-CODE TO STATUS-WS                                   
488718     PERFORM IMS-STATUSKONTROLL                                           
488719     .                                                                    
488720                                                                          
488721 IMS-ISRT-WDGX2223 SECTION.                                               
488722     MOVE 'IMS-ISRT-WDGX2223  '  TO DBS-SECTION                           
488724                                                                          
488725     MOVE 'WDR501   '      TO SSA1                                        
488726     MOVE '  II'           TO GODK-STATUSKODER                            
488727     CALL CBLTDLI USING ISRT WDR5-PCB DLI-IO-WDGX2223 SSA1                
488728     MOVE WDR5-STATUS-CODE TO STATUS-WS                                   
488729     PERFORM IMS-STATUSKONTROLL                                           
488731     .                                                                    
488732                                                                          
488733 IMS-ISRT-WDGX2224 SECTION.                                               
488734     MOVE 'IMS-ISRT-WDGX2224  '  TO DBS-SECTION                           
488736                                                                          
488737     STRING 'WDR501  (WDGXKEY  =' W-WDGXKEY-2223-X ')'                    
488738          DELIMITED BY SIZE INTO SSA1                                     
488739     MOVE 'WDR550   '      TO SSA2                                        
488740     MOVE '  II'           TO GODK-STATUSKODER                            
488741     CALL CBLTDLI USING ISRT WDR5-PCB DLI-IO-WDGX2224 SSA1 SSA2           
488742     MOVE WDR5-STATUS-CODE TO STATUS-WS                                   
488743     PERFORM IMS-STATUSKONTROLL                                           
488745     .                                                                    
488746                                                                          
488747 IMS-REPL-WDGX2224 SECTION.                                               
488748     MOVE 'IMS-REPL-WDGX2224  '  TO DBS-SECTION                           
488750                                                                          
488751     MOVE '  '             TO GODK-STATUSKODER                            
488752     CALL CBLTDLI USING REPL WDR5-PCB DLI-IO-WDGX2224                     
488753     MOVE WDR5-STATUS-CODE TO STATUS-WS                                   
488754     PERFORM IMS-STATUSKONTROLL                                           
488756     .                                                                    
488760 IMS-STATUSKONTROLL SECTION.                                              
488800     SET STATUS-IX TO 1                                                   
488900     SEARCH GODK-STATUS AT END CALL FELLOG                                
489000        WHEN GODK-STATUS(STATUS-IX) = STATUS-WS                           
489100           CONTINUE                                                       
489200     END-SEARCH                                                           
489300     .                                                                    
489400 IMS-ROLLBACK    SECTION.                                                 
489500     SKIP2                                                                
489600     CALL CBLTDLI USING ROLB    MSG-PCB                                   
489700     .                                                                    
489800     EJECT                                                                
489900*    -COPY WY2000P9                                                       
490000     EJECT                                                                
490100*    -COPY WY2000P1                                                       
490200     EJECT                                                                
490300*    -COPY WY2000P3                                                       
490400     EJECT                                                                
490500*    -COPY WY2000Q1                                                       
490600     EJECT                                                                
490700*    -COPY WY2000Q3                                                       
